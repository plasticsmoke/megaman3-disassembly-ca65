#!/usr/bin/env python3
"""
Post-process da65 output files to add .segment directives.
"""
import os, re

SRC = os.path.join(os.path.dirname(__file__), '..', 'src')


def add_segment_to_file(filepath, segment_name):
    """Add .segment directive after header comments/labels in a da65 output file."""
    with open(filepath, 'r') as f:
        lines = f.readlines()

    # Find insertion point: after all comments, empty lines, .setcpu, and := definitions
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

    # Check if segment directive already exists
    for line in lines:
        if f'.segment "{segment_name}"' in line:
            return  # Already has segment directive

    lines.insert(insert_idx, f'\n.segment "{segment_name}"\n\n')

    with open(filepath, 'w') as f:
        f.writelines(lines)
    print(f"  Added .segment \"{segment_name}\" to {os.path.basename(filepath)}")


def add_pair_segments(filepath, seg_lo, seg_hi):
    """Add two .segment directives to a bank pair file, splitting at $A000."""
    with open(filepath, 'r') as f:
        lines = f.readlines()

    # Check if segments already exist
    content = ''.join(lines)
    if f'.segment "{seg_lo}"' in content:
        return

    # Find insertion point for first segment
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

    lines.insert(insert_idx, f'\n.segment "{seg_lo}"\n\n')

    # Find the LA000 label and insert second segment before it
    for i, line in enumerate(lines):
        if re.match(r'^LA000\s*:', line):
            lines.insert(i, f'\n.segment "{seg_hi}"\n\n')
            break

    with open(filepath, 'w') as f:
        f.writelines(lines)
    print(f"  Added .segment \"{seg_lo}\" / \"{seg_hi}\" to {os.path.basename(filepath)}")


# Single 8KB banks
SINGLE_BANKS = {
    'bank00_enemy_data': 'BANK00',
    'bank01_stage_magnet': 'BANK01',
    'bank02_stage_gemini': 'BANK02',
    'bank03_stage_hard': 'BANK03',
    'bank04_doc_robot_a': 'BANK04',
    'bank05_doc_robot_b': 'BANK05',
    'bank06_robot_masters_a': 'BANK06',
    'bank07_robot_masters_b': 'BANK07',
    'bank08_stage_doc_needle': 'BANK08',
    'bank09_per_frame': 'BANK09',
    'bank0A_damage_tables': 'BANK0A',
    'bank0B_intro': 'BANK0B',
    'bank0C_game_over': 'BANK0C',
    'bank0D_oam_sprites': 'BANK0D',
    'bank0E_anim_frames': 'BANK0E',
    'bank0F_entity_spawn': 'BANK0F',
    'bank10_stage_setup': 'BANK10',
    'bank11_ending_data': 'BANK11',
    'bank12_fortress_bosses': 'BANK12',
    'bank13_ending_data2': 'BANK13',
    'bank14_sprite_offsets_alt': 'BANK14',
    'bank15_weapon_anim': 'BANK15',
    'bank16_sound_driver': 'BANK16',
    'bank17_sound_data': 'BANK17',
    'bank18_stage_select': 'BANK18',
    'bank19_sprite_offsets': 'BANK19',
}

def main():
    print("=== Adding segment directives ===")

    for filename, segment in sorted(SINGLE_BANKS.items()):
        filepath = os.path.join(SRC, f'{filename}.asm')
        if os.path.exists(filepath):
            add_segment_to_file(filepath, segment)

    # Bank pairs
    pair_1a = os.path.join(SRC, 'bank1A_1B_oam_sequences.asm')
    if os.path.exists(pair_1a):
        add_pair_segments(pair_1a, 'BANK1A', 'BANK1B')

    pair_1c = os.path.join(SRC, 'bank1C_1D_entity_ai.asm')
    if os.path.exists(pair_1c):
        add_pair_segments(pair_1c, 'BANK1C', 'BANK1D')

    # Fixed bank files already have segments from the split script
    print("Done!")


if __name__ == '__main__':
    main()
