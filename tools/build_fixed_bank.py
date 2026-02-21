#!/usr/bin/env python3
"""
Take the full da65 output for the fixed bank ($1E/$1F) and insert
.segment directives at the 12 logical section boundaries.
Produces a single .asm file that ca65 can assemble as one compilation unit.
"""
import re, os

BUILD = os.path.join(os.path.dirname(__file__), '..', 'build')
SRC = os.path.join(os.path.dirname(__file__), '..', 'src', 'fixed')

SEGMENTS = [
    (0xC000, 'NMI_IRQ'),
    (0xC4F8, 'PPU_UTIL'),
    (0xC816, 'STAGE_LOAD'),
    (0xC8D0, 'GAME_LOOP'),
    (0xCD98, 'PLAYER_STATES'),
    (0xE16A, 'CAMERA'),
    (0xE467, 'RENDER_COL'),
    (0xE8D6, 'TILE_COLLISION'),
    (0xF034, 'SPRITE_ANIM'),
    (0xF580, 'ENTITY_MOVE'),
    (0xF898, 'COLLISION_UTIL'),
    (0xFD52, 'RESET_BANKSWITCH'),
]

# Build a set of boundary addresses for quick lookup
BOUNDARY_ADDRS = {addr: name for addr, name in SEGMENTS}


def get_label_addr(line):
    """Extract address from a label line like 'LC000:' or 'LFFFA:'."""
    m = re.match(r'^(L[0-9A-Fa-f]{4})\s*:', line)
    if m:
        return int(m.group(1)[1:], 16)
    return None


def main():
    full_asm = os.path.join(BUILD, 'bank1E_1F_full.asm')
    out_asm = os.path.join(SRC, 'fixed_bank.asm')

    with open(full_asm, 'r') as f:
        lines = f.readlines()

    os.makedirs(SRC, exist_ok=True)

    output = []
    first_segment_inserted = False
    inserted_segments = set()

    for line in lines:
        addr = get_label_addr(line)

        if addr is not None and addr in BOUNDARY_ADDRS:
            seg_name = BOUNDARY_ADDRS[addr]
            if seg_name not in inserted_segments:
                output.append(f'\n; {"=" * 77}\n')
                output.append(f'; {seg_name} segment — ${addr:04X}\n')
                output.append(f'; {"=" * 77}\n')
                output.append(f'.segment "{seg_name}"\n\n')
                inserted_segments.add(seg_name)
                first_segment_inserted = True

        output.append(line)

    # If NMI_IRQ wasn't inserted (LC000 might be the very first code label),
    # we need it at the top
    if 'NMI_IRQ' not in inserted_segments:
        # Find insertion point (after header)
        for i, line in enumerate(output):
            if line.strip().startswith('LC000:'):
                output.insert(i, '.segment "NMI_IRQ"\n\n')
                break

    with open(out_asm, 'w') as f:
        f.writelines(output)

    print(f"Created {out_asm}")
    print(f"Segments inserted: {len(inserted_segments)}")
    for addr, name in SEGMENTS:
        status = "OK" if name in inserted_segments else "MISSING"
        print(f"  ${addr:04X} {name}: {status}")


if __name__ == '__main__':
    main()
