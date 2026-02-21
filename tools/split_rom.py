#!/usr/bin/env python3
"""Split mm3.nes into iNES header, 32x8KB PRG banks, and 128KB CHR blob."""
import sys, os

ROM = os.path.join(os.path.dirname(__file__), '..', 'mm3.nes')
OUTDIR = os.path.join(os.path.dirname(__file__), '..', 'build')

HEADER_SIZE = 16
PRG_BANK_SIZE = 0x2000  # 8KB
PRG_BANKS = 32
CHR_SIZE = 0x20000  # 128KB

EXPECTED_SIZE = HEADER_SIZE + PRG_BANKS * PRG_BANK_SIZE + CHR_SIZE  # 393232

def main():
    with open(ROM, 'rb') as f:
        data = f.read()

    assert len(data) == EXPECTED_SIZE, f"ROM size {len(data)} != expected {EXPECTED_SIZE}"

    os.makedirs(OUTDIR, exist_ok=True)

    # iNES header
    header = data[:HEADER_SIZE]
    with open(os.path.join(OUTDIR, 'header.bin'), 'wb') as f:
        f.write(header)
    print(f"header.bin: {len(header)} bytes")

    # PRG banks
    offset = HEADER_SIZE
    for i in range(PRG_BANKS):
        bank = data[offset:offset + PRG_BANK_SIZE]
        fname = f'bank{i:02X}.bin'
        with open(os.path.join(OUTDIR, fname), 'wb') as f:
            f.write(bank)
        offset += PRG_BANK_SIZE
    print(f"PRG: {PRG_BANKS} x {PRG_BANK_SIZE} byte banks")

    # CHR ROM
    chrdata = data[offset:offset + CHR_SIZE]
    chrpath = os.path.join(os.path.dirname(__file__), '..', 'chr', 'chr.bin')
    with open(chrpath, 'wb') as f:
        f.write(chrdata)
    print(f"chr.bin: {len(chrdata)} bytes")

    # Verify
    rebuilt = header
    for i in range(PRG_BANKS):
        with open(os.path.join(OUTDIR, f'bank{i:02X}.bin'), 'rb') as f:
            rebuilt += f.read()
    with open(chrpath, 'rb') as f:
        rebuilt += f.read()
    assert rebuilt == data, "Verification FAILED: rebuilt ROM != original"
    print("Verification PASSED: rebuilt ROM matches original")

if __name__ == '__main__':
    main()
