#!/usr/bin/env python3
"""
Generalized annotation transfer from xkas reference to ca65 bank files.

Transfers labels, comments, and section headers from the richly-annotated xkas
disassembly to the machine-generated ca65 output for any bank.

Uses ca65 listing output to get exact addresses for every line.

Usage:
    python3 tools/annotate_bank.py <bank_id>

Examples:
    python3 tools/annotate_bank.py 1C_1D
    python3 tools/annotate_bank.py 18
    python3 tools/annotate_bank.py 12
    python3 tools/annotate_bank.py 16
"""

import re
import subprocess
import sys
import os

XKAS_DIR = os.path.expanduser("~/megamanforever/mm3dasm")

# Bank configuration: bank_id → (xkas_file, ca65_file, [(bank_hex, base_addr), ...])
# bank_hex values are used to match the $XXyyyy address prefix in xkas comments
BANK_CONFIG = {
    '1C_1D': {
        'xkas': 'bank1C_1D.asm',
        'ca65': 'src/bank1C_1D_entity_ai.asm',
        'banks': [('1C', 0x8000), ('1D', 0xA000)],
    },
    '18': {
        'xkas': 'bank18.asm',
        'ca65': 'src/bank18_stage_select.asm',
        'banks': [('18', 0xA000)],
        # xkas uses org $9000 for second half; ca65 maps it to $B000
        'addr_fixup': lambda a: a + 0x2000 if a < 0xA000 else a,
    },
    '12': {
        'xkas': 'bank12.asm',
        'ca65': 'src/bank12_fortress_bosses.asm',
        'banks': [('12', 0xA000)],
    },
    '16': {
        'xkas': 'bank16.asm',
        'ca65': 'src/bank16_sound_driver.asm',
        'banks': [('16', 0x8000)],
    },
    '00': {
        'xkas': 'bank00.asm',
        'ca65': 'src/bank00_enemy_data.asm',
        'banks': [('00', 0xA000)],
    },
    '0A': {
        'xkas': 'bank0A.asm',
        'ca65': 'src/bank0A_damage_tables.asm',
        'banks': [('0A', 0xA000)],
    },
    '1A_1B': {
        'xkas': 'bank1A_1B.asm',
        'ca65': 'src/bank1A_1B_oam_sequences.asm',
        'banks': [('1A', 0x8000), ('1B', 0xA000)],
        # xkas uses org $8000 for both banks; bank $1B maps to ca65 $A000
        'addr_fixup_prefix': {'1B': lambda a: a + 0x2000},
    },
    '01': {
        'xkas': 'bank01.asm',
        'ca65': 'src/bank01_stage_magnet.asm',
        'banks': [('01', 0xA000)],
    },
    '03': {
        'xkas': 'bank03.asm',
        'ca65': 'src/bank03_stage_hard.asm',
        'banks': [('03', 0xA000)],
    },
    '04': {
        'xkas': 'bank04.asm',
        'ca65': 'src/bank04_doc_robot_a.asm',
        'banks': [('04', 0xA000)],
    },
    '05': {
        'xkas': 'bank05.asm',
        'ca65': 'src/bank05_doc_robot_b.asm',
        'banks': [('05', 0xA000)],
    },
    '06': {
        'xkas': 'bank06.asm',
        'ca65': 'src/bank06_robot_masters_a.asm',
        'banks': [('06', 0xA000)],
    },
    '07': {
        'xkas': 'bank07.asm',
        'ca65': 'src/bank07_robot_masters_b.asm',
        'banks': [('07', 0xA000)],
    },
    '02': {
        'xkas': 'bank02.asm',
        'ca65': 'src/bank02_stage_gemini.asm',
        'banks': [('02', 0xA000)],
    },
    '08': {
        'xkas': 'bank08.asm',
        'ca65': 'src/bank08_stage_doc_needle.asm',
        'banks': [('08', 0xA000)],
    },
    '09': {
        'xkas': 'bank09.asm',
        'ca65': 'src/bank09_per_frame.asm',
        'banks': [('09', 0x8000)],
    },
    '0B': {
        'xkas': 'bank0B.asm',
        'ca65': 'src/bank0B_intro.asm',
        'banks': [('0B', 0x8000)],
    },
    '0C': {
        'xkas': 'bank0C.asm',
        'ca65': 'src/bank0C_game_over.asm',
        'banks': [('0C', 0x8000)],
    },
    '0D': {
        'xkas': 'bank0D.asm',
        'ca65': 'src/bank0D_oam_sprites.asm',
        'banks': [('0D', 0xA000)],
    },
    '0E': {
        'xkas': 'bank0E.asm',
        'ca65': 'src/bank0E_anim_frames.asm',
        'banks': [('0E', 0xA000)],
    },
    '0F': {
        'xkas': 'bank0F.asm',
        'ca65': 'src/bank0F_entity_spawn.asm',
        'banks': [('0F', 0xA000)],
    },
    '10': {
        'xkas': 'bank10.asm',
        'ca65': 'src/bank10_stage_setup.asm',
        'banks': [('10', 0x8000)],
    },
    '11': {
        'xkas': 'bank11.asm',
        'ca65': 'src/bank11_ending_data.asm',
        'banks': [('11', 0xA000)],
    },
    '13': {
        'xkas': 'bank13.asm',
        'ca65': 'src/bank13_ending_data2.asm',
        'banks': [('13', 0xA000)],
    },
    '14': {
        'xkas': 'bank14.asm',
        'ca65': 'src/bank14_sprite_offsets_alt.asm',
        'banks': [('14', 0xA000)],
    },
    '15': {
        'xkas': 'bank15.asm',
        'ca65': 'src/bank15_weapon_anim.asm',
        'banks': [('15', 0x8000)],
    },
    '17': {
        'xkas': 'bank17.asm',
        'ca65': 'src/bank17_sound_data.asm',
        'banks': [('17', 0xA000)],
    },
    '19': {
        'xkas': 'bank19.asm',
        'ca65': 'src/bank19_sprite_offsets.asm',
        'banks': [('19', 0xA000)],
    },
}

# ─── xkas parsing ────────────────────────────────────────────────────────────

RE_XKAS_GLOBAL_LABEL = re.compile(r'^([a-zA-Z_][a-zA-Z0-9_]*):')
RE_XKAS_LOCAL_LABEL = re.compile(r'^(\.[a-zA-Z_][a-zA-Z0-9_]*):')


def build_addr_regex(bank_hexes):
    """Build regex to match address comments for the given bank hex prefixes.

    Captures group(1)=bank prefix, group(2)=4-digit addr, group(3)=comment.
    """
    prefix_pattern = '|'.join(re.escape(h) for h in bank_hexes)
    return re.compile(r';\s*\$(' + prefix_pattern + r')([0-9A-Fa-f]{4})\s*\|(.*)')


def parse_xkas(path, bank_hexes, addr_fixup=None, addr_fixup_prefix=None):
    """Parse xkas reference to extract address→annotation maps.

    addr_fixup: optional function(int) → int to remap all raw addresses.
    addr_fixup_prefix: optional dict of bank_hex → function(int) → int for per-prefix fixups.
    """
    re_addr = build_addr_regex(bank_hexes)

    def fix_addr(prefix, hex4):
        """Resolve a bank prefix + 4-digit hex to the ca65 CPU address string."""
        raw = int(hex4, 16)
        if addr_fixup is not None:
            raw = addr_fixup(raw)
        if addr_fixup_prefix and prefix.upper() in addr_fixup_prefix:
            raw = addr_fixup_prefix[prefix.upper()](raw)
        return f"{raw:04X}"

    addr_to_global = {}
    addr_to_local = {}
    addr_to_comment = {}
    addr_to_block = {}

    with open(path, 'r') as f:
        lines = f.readlines()

    in_header = True
    pending_block = []

    for i, raw in enumerate(lines):
        line = raw.rstrip()

        # Skip until first bank directive
        if in_header:
            if re.match(r'^bank\s+\$', line):
                in_header = False
            continue

        # Skip bank/org directives
        if re.match(r'^(bank|org)\s', line):
            continue

        # Collect comment-only lines as block comments
        if line.startswith(';'):
            pending_block.append(line)
            continue

        # Empty line
        if not line.strip():
            if pending_block:
                if _next_content_is_comment(lines, i + 1):
                    pending_block.append('')
                else:
                    pending_block = []
            continue

        # Global label
        m_global = RE_XKAS_GLOBAL_LABEL.match(line)
        if m_global:
            name = m_global.group(1)
            addr = _find_next_addr(lines, i, re_addr, fix_addr)
            if addr:
                # Strip bank prefix from auto-generated labels
                for bh in bank_hexes:
                    pat = re.compile(r'^(code|data)_' + re.escape(bh), re.IGNORECASE)
                    if pat.match(name):
                        name = name[:5] + name[5 + len(bh):]
                        break
                addr_to_global[addr] = name
                if pending_block:
                    addr_to_block[addr] = pending_block
                    pending_block = []
            else:
                pending_block = []
            continue

        # Local label
        m_local = RE_XKAS_LOCAL_LABEL.match(line)
        if m_local:
            addr = _find_next_addr(lines, i, re_addr, fix_addr)
            if addr and pending_block and addr not in addr_to_block:
                addr_to_block[addr] = pending_block
                pending_block = []
            elif not addr:
                pending_block = []
            continue

        # Instruction/data line with address comment
        m_addr = re_addr.search(line)
        if m_addr:
            addr = fix_addr(m_addr.group(1).upper(), m_addr.group(2).upper())
            comment = _clean_comment(m_addr.group(3))
            if comment:
                addr_to_comment[addr] = comment
            if pending_block:
                addr_to_block[addr] = pending_block
                pending_block = []
        else:
            pending_block = []

    return addr_to_global, addr_to_comment, addr_to_block


def _find_next_addr(lines, start_idx, re_addr, fix_addr=None):
    """Find the address from the nearest instruction comment."""
    for j in range(start_idx, min(start_idx + 20, len(lines))):
        m = re_addr.search(lines[j])
        if m:
            prefix = m.group(1).upper()
            raw = m.group(2).upper()
            return fix_addr(prefix, raw) if fix_addr else raw
    return None


def _next_content_is_comment(lines, start_idx):
    for j in range(start_idx, min(start_idx + 5, len(lines))):
        s = lines[j].strip()
        if s:
            return s.startswith(';')
    return False


def _clean_comment(raw):
    text = raw.strip()
    text = re.sub(r'^[|\\/ ]+', '', text).strip()
    text = re.sub(r'\s*[|\\/ ]+$', '', text).strip()
    return text


# ─── ca65 listing parsing ────────────────────────────────────────────────────

RE_LISTING_LINE = re.compile(r'^([0-9A-Fa-f]{6})r?\s+\d+\s')
RE_CA65_LABEL_DEF = re.compile(r'^(L[0-9A-F]{4}):')
RE_CA65_EXTERN = re.compile(r'^(L[0-9A-F]{4})\s+:=\s+\$([0-9A-F]{4})')
RE_CA65_SEGMENT = re.compile(r'^\.segment\s+"')


def get_line_addresses(ca65_path, segment_bases):
    """Use ca65 listing to get exact addresses for each source line.

    segment_bases: list of (base_addr,) for each segment in order of appearance.
    Returns: dict mapping 1-based line number → CPU address hex string.
    """
    result = subprocess.run(
        ['ca65', '--listing', '/dev/stdout', '-o', '/dev/null', ca65_path],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"ca65 listing failed: {result.stderr}", file=sys.stderr)
        sys.exit(1)

    listing_lines = result.stdout.splitlines()

    # Skip banner
    start = 0
    for i, ll in enumerate(listing_lines):
        if RE_LISTING_LINE.match(ll):
            start = i
            break

    # For single-segment banks, use the first base address.
    # For multi-segment banks (like 1C_1D with BANK1C + BANK1D),
    # we detect segment switches by offset resets.
    base = segment_bases[0]
    seg_idx = 0
    prev_offset = -1

    line_to_addr = {}
    source_line = 0

    for ll in listing_lines[start:]:
        m = RE_LISTING_LINE.match(ll)
        if not m:
            continue

        byte_area = ll[10:24] if len(ll) > 10 else ''
        has_bytes = any(c in '0123456789ABCDEFabcdef' for c in byte_area)
        has_source_text = len(ll) > 24 and ll[24:].strip() != ''

        if has_bytes and not has_source_text:
            continue

        source_line += 1
        offset = int(m.group(1), 16)

        # Detect segment switch: offset drops significantly
        if offset < prev_offset - 0x100 and seg_idx + 1 < len(segment_bases):
            seg_idx += 1
            base = segment_bases[seg_idx]

        prev_offset = offset
        addr = base + offset
        line_to_addr[source_line] = f"{addr:04X}"

    return line_to_addr


def parse_ca65_labels(path):
    extern_labels = set()
    code_labels = {}
    with open(path, 'r') as f:
        for i, line in enumerate(f, 1):
            line = line.rstrip()
            m = RE_CA65_EXTERN.match(line)
            if m:
                extern_labels.add(m.group(1))
                continue
            m = RE_CA65_LABEL_DEF.match(line)
            if m:
                label = m.group(1)
                if label not in extern_labels:
                    code_labels[label] = i
    return extern_labels, code_labels


# ─── annotation engine ───────────────────────────────────────────────────────

def build_rename_map(addr_to_global, code_labels, line_to_addr):
    rename = {}
    for label, line_num in code_labels.items():
        addr = line_to_addr.get(line_num)
        if addr and addr in addr_to_global:
            rename[label] = addr_to_global[addr]
    return rename


def annotate(ca65_path, rename_map, addr_to_global, addr_to_comment,
             addr_to_block, line_to_addr, code_labels):
    with open(ca65_path, 'r') as f:
        source_lines = [l.rstrip() for l in f.readlines()]

    # Find addresses that have xkas labels but no ca65 label
    renamed_addrs = set()
    for label, line_num in code_labels.items():
        addr = line_to_addr.get(line_num)
        if addr:
            renamed_addrs.add(addr)
    addr_to_global_remaining = {
        addr: name for addr, name in addr_to_global.items()
        if addr not in renamed_addrs
    }

    output = []

    for line_num_0 in range(len(source_lines)):
        line_num = line_num_0 + 1
        line = source_lines[line_num_0]
        addr = line_to_addr.get(line_num)

        # Insert block comments
        if addr and addr in addr_to_block:
            block = addr_to_block.pop(addr)
            if output and output[-1].strip():
                output.append('')
            for bc in block:
                output.append(bc)

        # Insert new global label if xkas has one and ca65 doesn't
        if addr and addr in addr_to_global_remaining:
            new_label = addr_to_global_remaining.pop(addr)
            if not RE_CA65_LABEL_DEF.match(line):
                output.append(f"{new_label}:")

        # Rename label definitions
        m_def = RE_CA65_LABEL_DEF.match(line)
        if m_def:
            old = m_def.group(1)
            if old in rename_map:
                line = rename_map[old] + line[len(old):]

        # Rename label references in operands
        if ':=' not in line:
            line = _rename_refs(line, rename_map, m_def)

        # Add inline comment
        if addr and addr in addr_to_comment:
            comment = addr_to_comment[addr]
            stripped = line.lstrip()
            if stripped and not stripped.startswith(';') and ';' not in line:
                line = f"{line:<39s} ; {comment}"

        output.append(line)

    return output


def _rename_refs(line, rename_map, label_def_match):
    if not rename_map:
        return line
    if label_def_match:
        prefix_len = len(label_def_match.group(0))
        prefix = line[:prefix_len]
        rest = line[prefix_len:]
        rest = re.sub(r'\bL[0-9A-F]{4}\b',
                       lambda m: rename_map.get(m.group(0), m.group(0)), rest)
        return prefix + rest
    return re.sub(r'\bL[0-9A-F]{4}\b',
                   lambda m: rename_map.get(m.group(0), m.group(0)), line)


def extract_xkas_header(path):
    """Extract documentation header (everything before first 'bank' directive)."""
    header = []
    with open(path, 'r') as f:
        for line in f:
            if re.match(r'^bank\s+\$', line.rstrip()):
                break
            header.append(line.rstrip())
    while header and not header[-1].strip():
        header.pop()
    return header


# ─── main ────────────────────────────────────────────────────────────────────

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 tools/annotate_bank.py <bank_id>")
        print("  bank_id: 1C_1D, 18, 12, 16")
        sys.exit(1)

    bank_id = sys.argv[1]
    if bank_id not in BANK_CONFIG:
        print(f"Unknown bank: {bank_id}")
        print(f"Available: {', '.join(BANK_CONFIG.keys())}")
        sys.exit(1)

    cfg = BANK_CONFIG[bank_id]
    xkas_path = os.path.join(XKAS_DIR, cfg['xkas'])
    ca65_path = cfg['ca65']
    bank_hexes = [b[0] for b in cfg['banks']]
    segment_bases = [b[1] for b in cfg['banks']]

    print(f"=== Annotating bank {bank_id} ===")

    addr_fixup = cfg.get('addr_fixup')
    addr_fixup_prefix = cfg.get('addr_fixup_prefix')

    print("1. Parsing xkas reference...")
    addr_to_global, addr_to_comment, addr_to_block = parse_xkas(
        xkas_path, bank_hexes, addr_fixup, addr_fixup_prefix)
    print(f"   Global labels: {len(addr_to_global)}")
    print(f"   Comments:      {len(addr_to_comment)}")
    print(f"   Block comments: {len(addr_to_block)}")

    print("2. Getting line→address map from ca65 listing...")
    line_to_addr = get_line_addresses(ca65_path, segment_bases)
    print(f"   Mapped lines:  {len(line_to_addr)}")

    print("3. Parsing ca65 labels...")
    extern_labels, code_labels = parse_ca65_labels(ca65_path)
    print(f"   Code labels:   {len(code_labels)}")
    print(f"   Extern labels: {len(extern_labels)}")

    print("4. Building rename map...")
    rename_map = build_rename_map(addr_to_global, code_labels, line_to_addr)
    print(f"   Renames:       {len(rename_map)}")

    # Show first few renames
    shown = 0
    for label in sorted(rename_map, key=lambda l: int(l[1:], 16)):
        if shown < 8:
            addr = line_to_addr.get(code_labels.get(label, 0), '????')
            print(f"   {label} (${addr}) -> {rename_map[label]}")
            shown += 1

    print("5. Extracting xkas header...")
    xkas_header = extract_xkas_header(xkas_path)
    print(f"   Header lines: {len(xkas_header)}")

    print("6. Applying annotations...")
    output = annotate(ca65_path, rename_map, addr_to_global, addr_to_comment,
                      addr_to_block, line_to_addr, code_labels)

    # Insert xkas header before .setcpu
    for j, line in enumerate(output):
        if line.strip().startswith('.setcpu'):
            output[j:j] = [''] + xkas_header + ['']
            break

    print(f"   Output lines: {len(output)}")

    print(f"7. Writing to {ca65_path}...")
    with open(ca65_path, 'w') as f:
        for line in output:
            f.write(line + '\n')

    total = len(code_labels)
    renamed = len(rename_map)
    inserted = len([a for a in addr_to_global if a not in
                    {line_to_addr.get(code_labels.get(l, 0))
                     for l in code_labels}])
    print(f"\nDone! {renamed} labels renamed, {len(addr_to_comment)} comments transferred.")
    print("Run 'make clean && make' to verify byte-perfect build.")


if __name__ == '__main__':
    main()
