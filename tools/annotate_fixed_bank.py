#!/usr/bin/env python3
"""
Annotate the ca65 fixed bank assembly using the xkas reference disassembly.

Transfers labels, comments, and section headers from the richly-annotated xkas
disassembly (bank1E_1F.asm) to the machine-generated ca65 output (fixed_bank.asm).

Uses ca65 listing output to get exact addresses for every line, so there's no
need for fragile instruction-size computation.

Usage:
    python3 tools/annotate_fixed_bank.py
"""

import re
import subprocess
import sys
import os

XKAS_PATH = os.path.expanduser("~/megamanforever/mm3dasm/bank1E_1F.asm")
CA65_PATH = "src/fixed/fixed_bank.asm"
OUTPUT_PATH = "src/fixed/fixed_bank.asm"
SEGMENT_BASE = 0xC000

# ─── xkas parsing ────────────────────────────────────────────────────────────

RE_XKAS_ADDR_COMMENT = re.compile(r';\s*\$1[EF]([0-9A-Fa-f]{4})\s*\|(.*)')
RE_XKAS_GLOBAL_LABEL = re.compile(r'^([a-zA-Z_][a-zA-Z0-9_]*):')
RE_XKAS_LOCAL_LABEL = re.compile(r'^(\.[a-zA-Z_][a-zA-Z0-9_]*):')


def parse_xkas(path):
    """Parse xkas reference to extract address→annotation maps.

    Returns:
        addr_to_global: {addr_hex: label_name}
        addr_to_local: {addr_hex: '@local_name'}
        addr_to_comment: {addr_hex: comment_text}
        addr_to_block: {addr_hex: [comment_lines]}
    """
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

        # Skip the big header comment
        if in_header:
            if re.match(r'^bank\s+\$1[Ee]', line):
                in_header = False
            continue

        # Skip bank/org directives
        if re.match(r'^(bank|org)\s', line):
            continue

        # Collect comment-only lines as block comments
        if line.startswith(';'):
            pending_block.append(line)
            continue

        # Empty line — keep accumulating block if more comments follow
        if not line.strip():
            if pending_block:
                # Peek ahead for more comments
                if _next_content_is_comment(lines, i + 1):
                    pending_block.append('')
                else:
                    pending_block = []
            continue

        # ── Global label ──
        m_global = RE_XKAS_GLOBAL_LABEL.match(line)
        if m_global:
            name = m_global.group(1)
            addr = _find_next_addr(lines, i)
            if addr:
                # Strip bank prefix: code_1EC088 → code_C088
                if re.match(r'(code|data)_1[EeFf]', name):
                    name = name[:5] + name[7:]
                addr_to_global[addr] = name
                if pending_block:
                    addr_to_block[addr] = pending_block
                    pending_block = []
            else:
                pending_block = []
            continue

        # ── Local label ──
        m_local = RE_XKAS_LOCAL_LABEL.match(line)
        if m_local:
            name = m_local.group(1)  # .something
            addr = _find_next_addr(lines, i)
            if addr:
                addr_to_local[addr] = '@' + name[1:]  # .foo → @foo
                if pending_block and addr not in addr_to_block:
                    addr_to_block[addr] = pending_block
                    pending_block = []
            else:
                pending_block = []
            continue

        # ── Instruction/data line with address comment ──
        m_addr = RE_XKAS_ADDR_COMMENT.search(line)
        if m_addr:
            addr = m_addr.group(1).upper()
            comment_raw = m_addr.group(2)
            comment = _clean_comment(comment_raw)
            if comment:
                addr_to_comment[addr] = comment
            if pending_block:
                addr_to_block[addr] = pending_block
                pending_block = []
        else:
            # Comment-only line under instructions (e.g. multiline explanation)
            pending_block = []

    return addr_to_global, addr_to_local, addr_to_comment, addr_to_block


def _find_next_addr(lines, start_idx):
    """Find the address from the nearest instruction comment at or after start_idx."""
    for j in range(start_idx, min(start_idx + 20, len(lines))):
        m = RE_XKAS_ADDR_COMMENT.search(lines[j])
        if m:
            return m.group(1).upper()
    return None


def _next_content_is_comment(lines, start_idx):
    """Check if the next non-empty line is a comment."""
    for j in range(start_idx, min(start_idx + 5, len(lines))):
        s = lines[j].strip()
        if s:
            return s.startswith(';')
    return False


def _clean_comment(raw):
    """Clean the xkas comment text (everything after the | pipe)."""
    # Remove leading/trailing pipe art characters: |, \, /
    text = raw.strip()
    # Strip leading pipe chars
    text = re.sub(r'^[|\\/ ]+', '', text).strip()
    # Strip trailing pipe chars
    text = re.sub(r'\s*[|\\/ ]+$', '', text).strip()
    return text


# ─── ca65 listing parsing ────────────────────────────────────────────────────

# Listing format: "OOOOOOr D  BB BB BB BB  source..."
# where O=offset, r=relocatable, D=file depth, B=bytes
RE_LISTING_LINE = re.compile(r'^([0-9A-Fa-f]{6})r?\s+\d+\s')


def get_line_addresses(ca65_path):
    """Use ca65 --listing to get exact addresses for each source line.

    Returns: dict mapping 1-based line number → CPU address hex string (4 chars)
    """
    result = subprocess.run(
        ['ca65', '--listing', '/dev/stdout', '-o', '/dev/null', ca65_path],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"ca65 listing failed: {result.stderr}", file=sys.stderr)
        sys.exit(1)

    listing_lines = result.stdout.splitlines()

    # Skip the ca65 banner (lines before the first offset line)
    start = 0
    for i, ll in enumerate(listing_lines):
        if RE_LISTING_LINE.match(ll):
            start = i
            break

    # Listing lines after the banner correspond 1:1 with source lines,
    # EXCEPT continuation lines (extra bytes from long .byte directives).
    # Listing format columns:
    #   0-6:  offset + 'r'   (e.g., "000000r")
    #   7-9:  space, depth, space
    #   10-23: assembled bytes (e.g., "AD 02 20    ")
    #   24+:  source text
    # Continuation lines have bytes in col 10-23 but NO source text at col 24+.
    line_to_addr = {}
    source_line = 0  # will be incremented to 1-based

    for ll in listing_lines[start:]:
        m = RE_LISTING_LINE.match(ll)
        if not m:
            continue

        # Detect continuation lines: have hex bytes but no source text
        byte_area = ll[10:24] if len(ll) > 10 else ''
        has_bytes = any(c in '0123456789ABCDEFabcdef' for c in byte_area)
        has_source_text = len(ll) > 24 and ll[24:].strip() != ''

        if has_bytes and not has_source_text:
            continue  # continuation line, skip

        source_line += 1
        offset = int(m.group(1), 16)
        addr = SEGMENT_BASE + offset
        line_to_addr[source_line] = f"{addr:04X}"

    return line_to_addr


# ─── ca65 source parsing ─────────────────────────────────────────────────────

RE_CA65_LABEL_DEF = re.compile(r'^(L[0-9A-F]{4}):')
RE_CA65_EXTERN = re.compile(r'^(L[0-9A-F]{4})\s+:=\s+\$([0-9A-F]{4})')


def parse_ca65_labels(path):
    """Parse ca65 source for label definitions.

    Returns:
        extern_labels: set of label names that are := definitions
        code_labels: {label_name: line_number} for code labels
    """
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
    """Build old_label → new_label rename map."""
    rename = {}
    for label, line_num in code_labels.items():
        addr = line_to_addr.get(line_num)
        if addr and addr in addr_to_global:
            rename[label] = addr_to_global[addr]
    return rename


def annotate(ca65_path, rename_map, addr_to_global, addr_to_comment,
             addr_to_block, line_to_addr, extern_labels, xkas_header,
             code_labels):
    """Apply annotations to the ca65 source.

    Returns list of output lines.
    """
    with open(ca65_path, 'r') as f:
        source_lines = [l.rstrip() for l in f.readlines()]

    # Find xkas global labels that have no corresponding ca65 label
    # (need to be inserted as new labels)
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
    header_inserted = False

    for line_num_0 in range(len(source_lines)):
        line_num = line_num_0 + 1  # 1-based
        line = source_lines[line_num_0]

        # Insert xkas header before .setcpu
        if not header_inserted and line.strip().startswith('.setcpu'):
            output.append('')
            output.extend(xkas_header)
            output.append('')
            header_inserted = True

        addr = line_to_addr.get(line_num)

        # ── Insert block comments before this line ──
        if addr and addr in addr_to_block:
            block = addr_to_block.pop(addr)
            if output and output[-1].strip():
                output.append('')
            for bc in block:
                output.append(bc)

        # ── Insert new global label if xkas has one and ca65 doesn't ──
        if addr and addr in addr_to_global_remaining:
            new_label = addr_to_global_remaining.pop(addr)
            # Only insert if this line doesn't already have a label
            if not RE_CA65_LABEL_DEF.match(line):
                output.append(f"{new_label}:")

        # ── Rename label definitions ──
        m_def = RE_CA65_LABEL_DEF.match(line)
        if m_def:
            old = m_def.group(1)
            if old in rename_map:
                line = rename_map[old] + line[len(old):]

        # ── Rename label references in operands ──
        if ':=' not in line:
            line = _rename_refs(line, rename_map, m_def)

        # ── Add inline comment ──
        if addr and addr in addr_to_comment:
            comment = addr_to_comment[addr]
            # Only add if line has code/data (not just a comment or blank)
            stripped = line.lstrip()
            if stripped and not stripped.startswith(';') and ';' not in line:
                line = f"{line:<39s} ; {comment}"

        output.append(line)

    return output


def _rename_refs(line, rename_map, label_def_match):
    """Replace LXXXX references with their renamed equivalents."""
    if not rename_map:
        return line

    # If line starts with a label def, only rename in the operand part
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
    """Extract the documentation header from xkas (everything before 'bank $1E')."""
    header = []
    with open(path, 'r') as f:
        for line in f:
            if re.match(r'^bank\s+\$1[Ee]', line.rstrip()):
                break
            header.append(line.rstrip())
    # Trim trailing blanks
    while header and not header[-1].strip():
        header.pop()
    return header


# ─── main ────────────────────────────────────────────────────────────────────

def main():
    print("=== Annotating fixed bank ===")

    print("1. Parsing xkas reference...")
    addr_to_global, addr_to_local, addr_to_comment, addr_to_block = \
        parse_xkas(XKAS_PATH)
    print(f"   Global labels: {len(addr_to_global)}")
    print(f"   Local labels:  {len(addr_to_local)}")
    print(f"   Comments:      {len(addr_to_comment)}")
    print(f"   Block comments: {len(addr_to_block)}")

    print("2. Getting line→address map from ca65 listing...")
    line_to_addr = get_line_addresses(CA65_PATH)
    print(f"   Mapped lines:  {len(line_to_addr)}")

    print("3. Parsing ca65 labels...")
    extern_labels, code_labels = parse_ca65_labels(CA65_PATH)
    print(f"   Code labels:   {len(code_labels)}")
    print(f"   Extern labels: {len(extern_labels)}")

    print("4. Building rename map...")
    rename_map = build_rename_map(addr_to_global, code_labels, line_to_addr)
    print(f"   Renames:       {len(rename_map)}")

    # Spot-check key labels
    for label in ['LC000', 'LC143', 'LFE00', 'LC4F8', 'LFF6B', 'LFF41']:
        new = rename_map.get(label, '(no rename)')
        addr = line_to_addr.get(code_labels.get(label, 0), '????')
        print(f"   {label} (${addr}) -> {new}")

    print("5. Extracting xkas header...")
    xkas_header = extract_xkas_header(XKAS_PATH)
    print(f"   Header lines: {len(xkas_header)}")

    print("6. Applying annotations...")
    output = annotate(CA65_PATH, rename_map, addr_to_global, addr_to_comment,
                      addr_to_block, line_to_addr, extern_labels, xkas_header,
                      code_labels)
    print(f"   Output lines: {len(output)}")

    print(f"7. Writing to {OUTPUT_PATH}...")
    with open(OUTPUT_PATH, 'w') as f:
        for line in output:
            f.write(line + '\n')

    print(f"\nDone! {len(rename_map)} labels renamed, "
          f"{len(addr_to_comment)} comments transferred.")
    print("Run 'make clean && make' to verify byte-perfect build.")


if __name__ == '__main__':
    main()
