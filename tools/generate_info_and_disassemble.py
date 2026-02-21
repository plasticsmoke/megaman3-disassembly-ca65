#!/usr/bin/env python3
"""
Generate da65 info files from bank_regions.json and run da65 on each bank.
Produces ca65 .asm source files with proper segment directives.
"""
import json, os, subprocess, sys

BUILD = os.path.join(os.path.dirname(__file__), '..', 'build')
SRC = os.path.join(os.path.dirname(__file__), '..', 'src')

# Load region data
with open(os.path.join(BUILD, 'bank_regions.json')) as f:
    regions_data = json.load(f)

# Bank configurations
# (bank_id, bin_file, start_addr, size, segment_name, output_file)
BANK_CONFIGS = []

# Single 8KB banks $00-$19
SINGLE_BANK_MAP = {
    '00': ('bank00_enemy_data', 'BANK00'),
    '01': ('bank01_stage_magnet', 'BANK01'),
    '02': ('bank02_stage_gemini', 'BANK02'),
    '03': ('bank03_stage_hard', 'BANK03'),
    '04': ('bank04_doc_robot_a', 'BANK04'),
    '05': ('bank05_doc_robot_b', 'BANK05'),
    '06': ('bank06_robot_masters_a', 'BANK06'),
    '07': ('bank07_robot_masters_b', 'BANK07'),
    '08': ('bank08_stage_doc_needle', 'BANK08'),
    '09': ('bank09_per_frame', 'BANK09'),
    '0A': ('bank0A_damage_tables', 'BANK0A'),
    '0B': ('bank0B_intro', 'BANK0B'),
    '0C': ('bank0C_game_over', 'BANK0C'),
    '0D': ('bank0D_oam_sprites', 'BANK0D'),
    '0E': ('bank0E_anim_frames', 'BANK0E'),
    '0F': ('bank0F_entity_spawn', 'BANK0F'),
    '10': ('bank10_stage_setup', 'BANK10'),
    '11': ('bank11_ending_data', 'BANK11'),
    '12': ('bank12_fortress_bosses', 'BANK12'),
    '13': ('bank13_ending_data2', 'BANK13'),
    '14': ('bank14_sprite_offsets_alt', 'BANK14'),
    '15': ('bank15_weapon_anim', 'BANK15'),
    '16': ('bank16_sound_driver', 'BANK16'),
    '17': ('bank17_sound_data', 'BANK17'),
    '18': ('bank18_stage_select', 'BANK18'),
    '19': ('bank19_sprite_offsets', 'BANK19'),
}

# Map bank IDs to their base addresses from the xkas reference
BANK_BASE_ADDRS = {}


def get_base_addr(bank_id, regions):
    """Determine base address from region data."""
    if not regions:
        return 0x8000
    first_addr = int(regions[0][0], 16)
    return first_addr


def generate_info_file(bank_hex, bin_file, start_addr, size, info_path, data_regions):
    """Generate a da65 info file."""
    lines = []
    lines.append('GLOBAL {')
    lines.append(f'    STARTADDR ${start_addr:04X};')
    lines.append(f'    INPUTOFFS 0;')
    lines.append(f'    INPUTSIZE ${size:04X};')
    lines.append(f'    CPU "6502";')
    lines.append('};')
    lines.append('')

    # Add data ranges
    for start, end, rtype in data_regions:
        if rtype != 'data':
            continue
        s = int(start, 16) if isinstance(start, str) else start
        e = int(end, 16) if isinstance(end, str) else end

        # Verify range is within bank
        if s < start_addr or e >= start_addr + size:
            continue

        lines.append(f'RANGE {{ START ${s:04X}; END ${e:04X}; TYPE ByteTable; }};')

    with open(info_path, 'w') as f:
        f.write('\n'.join(lines) + '\n')


def add_segment_directive(asm_path, segment_name):
    """Add .segment directive at the top of a da65 output file."""
    with open(asm_path, 'r') as f:
        content = f.read()

    # Find the first non-comment, non-empty, non-setcpu, non-label-definition line
    lines = content.split('\n')
    insert_idx = 0
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith(';') or stripped == '' or stripped.startswith('.setcpu'):
            insert_idx = i + 1
            continue
        # Check if it's an external label definition (L0000 := $0000)
        if ':=' in stripped:
            insert_idx = i + 1
            continue
        break

    lines.insert(insert_idx, f'')
    lines.insert(insert_idx + 1, f'.segment "{segment_name}"')
    lines.insert(insert_idx + 2, f'')

    with open(asm_path, 'w') as f:
        f.write('\n'.join(lines))


def disassemble_single_bank(bank_hex, out_name, segment_name, regions_key=None):
    """Disassemble a single 8KB bank."""
    if regions_key is None:
        regions_key = bank_hex

    bin_file = os.path.join(BUILD, f'bank{bank_hex}.bin')
    info_file = os.path.join(BUILD, f'bank{bank_hex}.info')
    asm_file = os.path.join(SRC, f'{out_name}.asm')

    # Get regions and determine base address
    regions = regions_data.get(regions_key, [])
    if regions:
        base_addr = int(regions[0][0], 16)
        # Round down to bank boundary
        if base_addr >= 0xA000:
            base_addr = 0xA000
        elif base_addr >= 0x8000:
            base_addr = 0x8000
    else:
        base_addr = 0x8000

    # Generate info file
    generate_info_file(bank_hex, bin_file, base_addr, 0x2000, info_file, regions)

    # Run da65
    cmd = ['da65', '-i', info_file, '-o', asm_file, bin_file]
    print(f"  da65 bank {bank_hex} -> {out_name}.asm (base ${base_addr:04X})")
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    ERROR: {result.stderr}")
        return False

    # Add segment directive
    add_segment_directive(asm_file, segment_name)
    return True


def disassemble_bank_pair(bank_lo, bank_hi, out_name, segment_lo, segment_hi, regions_key):
    """Disassemble a 16KB bank pair."""
    bin_lo = os.path.join(BUILD, f'bank{bank_lo}.bin')
    bin_hi = os.path.join(BUILD, f'bank{bank_hi}.bin')
    combined = os.path.join(BUILD, f'bank{bank_lo}_{bank_hi}.bin')

    # Concatenate the two 8KB bins
    with open(bin_lo, 'rb') as f:
        data = f.read()
    with open(bin_hi, 'rb') as f:
        data += f.read()
    with open(combined, 'wb') as f:
        f.write(data)

    info_file = os.path.join(BUILD, f'bank{bank_lo}_{bank_hi}.info')
    asm_file = os.path.join(SRC, f'{out_name}.asm')

    regions = regions_data.get(regions_key, [])
    base_addr = 0x8000  # bank pairs start at $8000

    # Generate info file for 16KB
    generate_info_file(f'{bank_lo}_{bank_hi}', combined, base_addr, 0x4000, info_file, regions)

    # Run da65
    cmd = ['da65', '-i', info_file, '-o', asm_file, combined]
    print(f"  da65 banks {bank_lo}/{bank_hi} -> {out_name}.asm")
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    ERROR: {result.stderr}")
        return False

    # For bank pairs, we need to split at the $A000 boundary
    # Read the output and insert segment directive for the second segment
    with open(asm_file, 'r') as f:
        content = f.read()

    lines = content.split('\n')

    # Find where $A000 labels start and insert second segment directive
    # First, add the initial segment at the top
    insert_idx = 0
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith(';') or stripped == '' or stripped.startswith('.setcpu'):
            insert_idx = i + 1
            continue
        if ':=' in stripped:
            insert_idx = i + 1
            continue
        break

    lines.insert(insert_idx, '')
    lines.insert(insert_idx + 1, f'.segment "{segment_lo}"')
    lines.insert(insert_idx + 2, '')

    # Find the label at $A000 and insert second segment directive before it
    for i, line in enumerate(lines):
        if line.strip().startswith('LA000:') or line.strip() == 'LA000:':
            lines.insert(i, '')
            lines.insert(i + 1, f'.segment "{segment_hi}"')
            lines.insert(i + 2, '')
            break

    with open(asm_file, 'w') as f:
        f.write('\n'.join(lines))

    return True


def disassemble_fixed_bank():
    """Disassemble the fixed bank ($1E/$1F) as one 16KB unit, then split into 12 files."""
    bin_1e = os.path.join(BUILD, 'bank1E.bin')
    bin_1f = os.path.join(BUILD, 'bank1F.bin')
    combined = os.path.join(BUILD, 'bank1E_1F.bin')

    with open(bin_1e, 'rb') as f:
        data = f.read()
    with open(bin_1f, 'rb') as f:
        data += f.read()
    with open(combined, 'wb') as f:
        f.write(data)

    info_file = os.path.join(BUILD, 'bank1E_1F.info')
    asm_file = os.path.join(BUILD, 'bank1E_1F_full.asm')

    regions = regions_data.get('1E_1F', [])

    # Generate info file
    generate_info_file('1E_1F', combined, 0xC000, 0x4000, info_file, regions)

    # Run da65
    cmd = ['da65', '-i', info_file, '-o', asm_file, combined]
    print(f"  da65 fixed bank 1E/1F -> bank1E_1F_full.asm")
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    ERROR: {result.stderr}")
        return False

    # Now split into 12 logical files based on address boundaries
    FIXED_SEGMENTS = [
        ('nmi_irq',          'NMI_IRQ',          0xC000, 0xC4F7),
        ('ppu_util',         'PPU_UTIL',         0xC4F8, 0xC815),
        ('stage_load',       'STAGE_LOAD',       0xC816, 0xC8CF),
        ('game_loop',        'GAME_LOOP',        0xC8D0, 0xCD97),
        ('player_states',    'PLAYER_STATES',    0xCD98, 0xE169),
        ('camera',           'CAMERA',           0xE16A, 0xE466),
        ('render_col',       'RENDER_COL',       0xE467, 0xE8D5),
        ('tile_collision',   'TILE_COLLISION',   0xE8D6, 0xF033),
        ('sprite_anim',      'SPRITE_ANIM',      0xF034, 0xF57F),
        ('entity_move',      'ENTITY_MOVE',      0xF580, 0xF897),
        ('collision_util',   'COLLISION_UTIL',   0xF898, 0xFD51),
        ('reset_bankswitch', 'RESET_BANKSWITCH', 0xFD52, 0xFFFF),
    ]

    # Read the full disassembly
    with open(asm_file, 'r') as f:
        lines = f.readlines()

    # Parse line addresses: look for labels like LXXXX: to determine current address
    # Also track .byte/.addr lines to count bytes
    line_addrs = {}  # line_number -> address

    # First pass: find all label definitions to get addresses
    for i, line in enumerate(lines):
        import re
        m = re.match(r'^(L[0-9A-Fa-f]{4}):', line)
        if m:
            addr = int(m.group(1)[1:], 16)
            line_addrs[i] = addr

    # Find header lines (comments, .setcpu, external labels)
    header_end = 0
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith(';') or stripped == '' or stripped.startswith('.setcpu'):
            header_end = i + 1
            continue
        if ':=' in stripped:
            header_end = i + 1
            continue
        break

    # Collect all external label definitions (for import in split files)
    ext_labels = []
    for i in range(header_end):
        line = lines[i].strip()
        if ':=' in line:
            ext_labels.append(lines[i])

    # Collect the .setcpu line
    setcpu_line = ''
    for i in range(header_end):
        if lines[i].strip().startswith('.setcpu'):
            setcpu_line = lines[i]
            break

    # Split the assembly into segments based on label addresses
    # Build a map of line_number -> address for ALL lines
    current_addr = 0xC000
    all_line_addrs = []

    for i, line in enumerate(lines):
        if i in line_addrs:
            current_addr = line_addrs[i]
        all_line_addrs.append(current_addr)

    # For each segment, collect lines that fall within its address range
    fixed_dir = os.path.join(SRC, 'fixed')
    os.makedirs(fixed_dir, exist_ok=True)

    for filename, segment_name, seg_start, seg_end in FIXED_SEGMENTS:
        outpath = os.path.join(fixed_dir, f'{filename}.asm')

        seg_lines = []
        seg_lines.append(f'; {"=" * 77}\n')
        seg_lines.append(f'; {filename}.asm — ${seg_start:04X}-${seg_end:04X}\n')
        seg_lines.append(f'; {"=" * 77}\n')
        seg_lines.append(f'\n')
        seg_lines.append(setcpu_line)
        seg_lines.append(f'\n')

        # Add external label imports
        for label in ext_labels:
            seg_lines.append(label)
        if ext_labels:
            seg_lines.append('\n')

        seg_lines.append(f'.segment "{segment_name}"\n')
        seg_lines.append(f'\n')

        # Find lines in this address range
        in_range = False
        for i in range(header_end, len(lines)):
            addr = all_line_addrs[i]
            if i in line_addrs and line_addrs[i] >= seg_start and line_addrs[i] <= seg_end:
                in_range = True
            elif i in line_addrs and line_addrs[i] > seg_end:
                in_range = False

            if in_range and addr >= seg_start and addr <= seg_end:
                seg_lines.append(lines[i])

        with open(outpath, 'w') as f:
            f.writelines(seg_lines)

        print(f"    {filename}.asm (${seg_start:04X}-${seg_end:04X})")

    return True


def main():
    os.makedirs(os.path.join(SRC, 'fixed'), exist_ok=True)

    print("=== Disassembling single 8KB banks ===")
    for bank_hex, (out_name, segment_name) in sorted(SINGLE_BANK_MAP.items()):
        # Determine which region key to use
        # Bank 09 uses base $8000, others may vary
        # Check if bank uses $8000 or $A000 base
        regions = regions_data.get(bank_hex, [])
        if regions:
            first_addr = int(regions[0][0], 16)
            if first_addr >= 0xA000:
                # This bank is mapped at $A000
                pass
            elif first_addr >= 0x8000:
                # This bank is mapped at $8000
                pass

        disassemble_single_bank(bank_hex, out_name, segment_name)

    print("\n=== Disassembling 16KB bank pairs ===")
    disassemble_bank_pair('1A', '1B', 'bank1A_1B_oam_sequences', 'BANK1A', 'BANK1B', '1A_1B')
    disassemble_bank_pair('1C', '1D', 'bank1C_1D_entity_ai', 'BANK1C', 'BANK1D', '1C_1D')

    print("\n=== Disassembling fixed bank ($1E/$1F) ===")
    disassemble_fixed_bank()

    print("\nDone!")


if __name__ == '__main__':
    main()
