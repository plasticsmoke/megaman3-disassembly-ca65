#!/usr/bin/env python3
"""
Parse xkas-plus assembly files to identify code vs data regions.
Outputs a JSON file with byte ranges for each bank.

xkas format:
  - `org $XXXX` sets the current address
  - `db $XX, ...` or `db "string"` is byte data
  - `dw $XXXX, ...` is word data
  - 6502 mnemonics are code (lda, sta, jsr, jmp, etc.)
  - Lines starting with ; or empty are ignored
  - Labels end with : or start at column 0
"""
import re, sys, os, json

XKAS_DIR = os.path.expanduser('~/megamanforever/mm3dasm')

# 6502 mnemonics (all of them)
MNEMONICS = {
    'adc', 'and', 'asl', 'bcc', 'bcs', 'beq', 'bit', 'bmi', 'bne', 'bpl',
    'brk', 'bvc', 'bvs', 'clc', 'cld', 'cli', 'clv', 'cmp', 'cpx', 'cpy',
    'dec', 'dex', 'dey', 'eor', 'inc', 'inx', 'iny', 'jmp', 'jsr', 'lda',
    'ldx', 'ldy', 'lsr', 'nop', 'ora', 'pha', 'php', 'pla', 'plp', 'rol',
    'ror', 'rti', 'rts', 'sbc', 'sec', 'sed', 'sei', 'sta', 'stx', 'sty',
    'tax', 'tay', 'tsx', 'txa', 'txs', 'tya',
}

# Instruction sizes by addressing mode pattern
def instruction_size(mnemonic, operand):
    """Estimate instruction size from mnemonic and operand text."""
    m = mnemonic.lower()
    if m in ('brk', 'clc', 'cld', 'cli', 'clv', 'dex', 'dey', 'inx', 'iny',
             'nop', 'pha', 'php', 'pla', 'plp', 'rti', 'rts', 'sec', 'sed',
             'sei', 'tax', 'tay', 'tsx', 'txa', 'txs', 'tya', 'asl', 'lsr',
             'rol', 'ror'):
        if not operand or operand.strip() == '' or operand.strip().lower() == 'a':
            return 1  # implied or accumulator
    if not operand:
        return 1

    op = operand.strip()

    # Immediate: #$XX
    if op.startswith('#'):
        return 2

    # Relative (branches)
    if m in ('bcc', 'bcs', 'beq', 'bmi', 'bne', 'bpl', 'bvc', 'bvs'):
        return 2

    # Indirect: ($XXXX) for JMP
    if op.startswith('(') and op.endswith(')'):
        return 3

    # Indirect indexed: ($XX),y
    if op.startswith('(') and '),y' in op.lower():
        return 2

    # Indexed indirect: ($XX,x)
    if op.startswith('(') and ',x)' in op.lower():
        return 2

    # Parse the address value
    # Remove ,x or ,y suffix
    base = re.sub(r'\s*,\s*[xy]$', '', op, flags=re.I).strip()

    # Try to get numeric value
    val = None
    if base.startswith('$'):
        try:
            val = int(base[1:], 16)
        except ValueError:
            pass
    elif base.startswith('%'):
        try:
            val = int(base[1:], 2)
        except ValueError:
            pass
    elif base.isdigit():
        val = int(base)

    if val is not None:
        if val <= 0xFF:
            return 2  # zero page
        else:
            return 3  # absolute

    # Label reference - assume absolute (3 bytes) for most, 2 for zero page labels
    # Can't always tell, default to 3
    return 3


def count_db_bytes(operand):
    """Count bytes in a db directive operand."""
    if not operand:
        return 0
    count = 0
    # Handle string literals
    in_string = False
    i = 0
    parts = []
    current = ''

    # Simple split by comma, respecting quotes
    for ch in operand:
        if ch == '"' and not in_string:
            in_string = True
            current += ch
        elif ch == '"' and in_string:
            in_string = False
            current += ch
        elif ch == ',' and not in_string:
            parts.append(current.strip())
            current = ''
        else:
            current += ch
    if current.strip():
        parts.append(current.strip())

    for part in parts:
        part = part.strip()
        if not part:
            continue
        if part.startswith('"') and part.endswith('"'):
            count += len(part) - 2  # string literal
        else:
            count += 1  # single byte value
    return count


def count_dw_bytes(operand):
    """Count bytes in a dw directive operand."""
    if not operand:
        return 0
    parts = operand.split(',')
    return len(parts) * 2


def parse_xkas_file(filepath):
    """Parse an xkas file and return list of (start_addr, end_addr, type) regions."""
    regions = []  # [(start, end, 'code'|'data')]

    current_addr = None
    current_type = None
    region_start = None

    with open(filepath, 'r') as f:
        lines = f.readlines()

    for line in lines:
        # Strip comments (but not inside strings)
        stripped = line.split(';')[0].strip() if ';' in line and '"' not in line.split(';')[0] else line.strip()
        if not stripped or stripped.startswith(';'):
            continue

        # Check for org directive
        m = re.match(r'org\s+\$([0-9A-Fa-f]+)', stripped, re.I)
        if m:
            # Save current region
            if current_type and region_start is not None and current_addr is not None:
                regions.append((region_start, current_addr - 1, current_type))
            current_addr = int(m.group(1), 16)
            region_start = current_addr
            current_type = None
            continue

        # Skip labels (lines that are just a label)
        if re.match(r'^[a-zA-Z_]\w*\s*:', stripped) and not re.search(r':\s+\S', stripped):
            continue

        # Remove label prefix if present
        stripped = re.sub(r'^[a-zA-Z_]\w*\s*:\s*', '', stripped).strip()
        if not stripped:
            continue

        # Parse the instruction/directive
        parts = stripped.split(None, 1)
        mnemonic = parts[0].lower()
        operand = parts[1] if len(parts) > 1 else ''

        # Remove inline comment
        if ';' in operand and '"' not in operand:
            operand = operand.split(';')[0].strip()

        if current_addr is None:
            continue

        if mnemonic == 'db':
            line_type = 'data'
            byte_count = count_db_bytes(operand)
        elif mnemonic == 'dw':
            line_type = 'data'
            byte_count = count_dw_bytes(operand)
        elif mnemonic == 'fill' or mnemonic == 'fillbyte':
            # xkas fill directive
            continue  # skip for now
        elif mnemonic in MNEMONICS:
            line_type = 'code'
            byte_count = instruction_size(mnemonic, operand)
        else:
            # Unknown - could be a macro or label; skip
            continue

        if line_type != current_type:
            if current_type and region_start is not None:
                regions.append((region_start, current_addr - 1, current_type))
            region_start = current_addr
            current_type = line_type

        current_addr += byte_count

    # Save final region
    if current_type and region_start is not None and current_addr is not None:
        regions.append((region_start, current_addr - 1, current_type))

    return regions


def merge_small_regions(regions, min_size=4):
    """Merge very small code regions surrounded by data into data."""
    if len(regions) <= 2:
        return regions

    merged = list(regions)
    changed = True
    while changed:
        changed = False
        new = []
        i = 0
        while i < len(merged):
            if (i > 0 and i < len(merged) - 1 and
                merged[i][2] == 'code' and
                merged[i][1] - merged[i][0] + 1 < min_size and
                merged[i-1][2] == 'data' and merged[i+1][2] == 'data'):
                # Merge this small code region into surrounding data
                # Extend previous data region
                new[-1] = (new[-1][0], merged[i+1][1], 'data')
                i += 2  # skip next data region too (merged)
                changed = True
            else:
                new.append(merged[i])
                i += 1
        merged = new

    return merged


def consolidate_regions(regions):
    """Merge adjacent regions of the same type."""
    if not regions:
        return regions
    consolidated = [regions[0]]
    for r in regions[1:]:
        if r[2] == consolidated[-1][2] and r[0] == consolidated[-1][1] + 1:
            consolidated[-1] = (consolidated[-1][0], r[1], r[2])
        else:
            consolidated.append(r)
    return consolidated


def main():
    # Bank files and their mappings
    banks = {}

    # Single 8KB banks ($00-$19)
    for i in range(0x1A):
        fname = f'bank{i:02X}.asm'
        fpath = os.path.join(XKAS_DIR, fname)
        if os.path.exists(fpath):
            print(f"Parsing {fname}...")
            regions = parse_xkas_file(fpath)
            regions = consolidate_regions(regions)
            banks[f'{i:02X}'] = regions

    # 16KB pairs
    for pair in ['1A_1B', '1C_1D', '1E_1F']:
        fname = f'bank{pair}.asm'
        fpath = os.path.join(XKAS_DIR, fname)
        if os.path.exists(fpath):
            print(f"Parsing {fname}...")
            regions = parse_xkas_file(fpath)
            regions = consolidate_regions(regions)
            banks[pair] = regions

    # Output summary
    for bank_id, regions in sorted(banks.items()):
        data_bytes = sum(r[1] - r[0] + 1 for r in regions if r[2] == 'data')
        code_bytes = sum(r[1] - r[0] + 1 for r in regions if r[2] == 'code')
        total = data_bytes + code_bytes
        print(f"  Bank {bank_id}: {len(regions)} regions, {code_bytes} code + {data_bytes} data = {total} bytes")
        for start, end, rtype in regions:
            if rtype == 'data':
                print(f"    DATA ${start:04X}-${end:04X} ({end-start+1} bytes)")

    # Save to JSON
    output = {}
    for bank_id, regions in banks.items():
        output[bank_id] = [(hex(s), hex(e), t) for s, e, t in regions]

    outpath = os.path.join(os.path.dirname(__file__), '..', 'build', 'bank_regions.json')
    with open(outpath, 'w') as f:
        json.dump(output, f, indent=2)
    print(f"\nSaved to {outpath}")


if __name__ == '__main__':
    main()
