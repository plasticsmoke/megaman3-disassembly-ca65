#!/usr/bin/env python3
"""Add annotation status headers to all bank source files."""

import os
import re

SRC = 'src'

# Bank descriptions and annotation status
# Format: (filename, title, description, annotation_note)
BANKS = [
    ('bank00_enemy_data', 'BANK $00 — GLOBAL ENEMY DATA',
     'Global enemy data tables shared across all stages.\n'
     '; Contains enemy flags, routine IDs, spawn parameters, and hitbox data.',
     '0% — unannotated da65 output'),

    ('bank01_stage_magnet', 'BANK $01 — CHR/PALETTE INIT + MAGNET MAN STAGE DATA',
     'CHR bank initialization, palette loading, and Magnet Man stage layout data.',
     '0% — unannotated da65 output'),

    ('bank02_stage_gemini', 'BANK $02 — GEMINI MAN STAGE DATA + TITLE SCREEN',
     'Gemini Man stage layout data and title screen display logic.',
     '0% — unannotated da65 output'),

    ('bank03_stage_hard', 'BANK $03 — STAGE TRANSITION + HARD MAN STAGE DATA',
     'Stage transition effects and Hard Man stage layout data.',
     '0% — unannotated da65 output'),

    ('bank04_doc_robot_a', 'BANK $04 — DOC ROBOT AI (FLASH/WOOD/CRASH/METAL)',
     'Doc Robot AI routines mimicking Flash Man, Wood Man, Crash Man, and Metal Man.',
     '0% — unannotated da65 output'),

    ('bank05_doc_robot_b', 'BANK $05 — DOC ROBOT AI (BUBBLE/HEAT/QUICK/AIR)',
     'Doc Robot AI routines mimicking Bubble Man, Heat Man, Quick Man, and Air Man.',
     '0% — unannotated da65 output'),

    ('bank06_robot_masters_a', 'BANK $06 — ROBOT MASTER AI (NEEDLE/MAGNET/TOP/SHADOW)',
     'AI state machines for Needle Man, Magnet Man, Top Man, and Shadow Man.',
     '0% — unannotated da65 output'),

    ('bank07_robot_masters_b', 'BANK $07 — ROBOT MASTER AI (HARD/SPARK/SNAKE/GEMINI)',
     'AI state machines for Hard Man, Spark Man, Snake Man, and Gemini Man.',
     '0% — unannotated da65 output'),

    ('bank08_stage_doc_needle', 'BANK $08 — DOC ROBOT NEEDLE STAGE DATA',
     'Stage layout data for the Doc Robot revisit of Needle Man\'s stage.',
     '0% — unannotated da65 output (pure data)'),

    ('bank09_per_frame', 'BANK $09 — PER-FRAME ENTITY SUBSYSTEMS',
     'Per-frame entity processing subsystems called from the main game loop.',
     '0% — unannotated da65 output'),

    ('bank0A_damage_tables', 'BANK $0A — WEAPON DAMAGE TABLES + DOC ROBOT SPARK STAGE',
     'Weapon-vs-enemy damage tables (9 tables) and Doc Robot Spark Man stage data.',
     '0% — unannotated da65 output (pure data)'),

    ('bank0B_intro', 'BANK $0B — INTRO SEQUENCE + DOC ROBOT SHADOW STAGE',
     'Opening intro cinematic sequence and Doc Robot Shadow Man stage data.',
     '0% — unannotated da65 output'),

    ('bank0C_game_over', 'BANK $0C — GAME OVER / RESULTS SCREEN + WILY 1 STAGE',
     'Game over screen, stage results display, and Wily Castle stage 1 data.',
     '0% — unannotated da65 output'),

    ('bank0D_oam_sprites', 'BANK $0D — OAM/SPRITE ANIMATION + WILY 2/3/5 STAGE',
     'OAM sprite animation dispatch and Wily Castle stages 2, 3, 5 data.',
     '0% — unannotated da65 output'),

    ('bank0E_anim_frames', 'BANK $0E — ANIMATION FRAME MANAGEMENT',
     'Sprite animation frame sequencing and management routines.',
     '0% — unannotated da65 output'),

    ('bank0F_entity_spawn', 'BANK $0F — ENTITY SPAWNING / PROJECTILE DISPATCH + WILY 4',
     'Entity spawn routing, projectile dispatch tables, and Wily Castle stage 4 data.',
     '0% — unannotated da65 output'),

    ('bank10_stage_setup', 'BANK $10 — STAGE SETUP / BOSS POST-DEFEAT',
     'Stage initialization routines and boss post-defeat handling.',
     '0% — unannotated da65 output'),

    ('bank11_ending_data', 'BANK $11 — SPECIAL/ENDING STAGE DATA',
     'Stage data for special and ending sequences.',
     '0% — unannotated da65 output (pure data)'),

    ('bank12_fortress_bosses', 'BANK $12 — FORTRESS BOSSES + SPECIAL ENTITIES',
     'AI routines for Wily fortress bosses (Yellow Devil, Clone Mega Man, Wily\n'
     '; Machine, Gamma) and special entities (breakable blocks, Wily capsule).',
     '0% — unannotated da65 output'),

    ('bank13_ending_data2', 'BANK $13 — SPECIAL/ENDING STAGE DATA',
     'Additional stage data for ending sequences.',
     '0% — unannotated da65 output (pure data)'),

    ('bank14_sprite_offsets_alt', 'BANK $14 — SPRITE POSITION OFFSET DATA (ALTERNATE)',
     'Alternate sprite position offset lookup tables.',
     '0% — unannotated da65 output (pure data)'),

    ('bank15_weapon_anim', 'BANK $15 — WEAPON SPRITE ANIMATION SEQUENCES',
     'Weapon-specific sprite animation sequence data.',
     '0% — unannotated da65 output (pure data)'),

    ('bank16_sound_driver', 'BANK $16 — SOUND DRIVER CODE',
     'NES APU sound driver: channel management, envelope processing,\n'
     '; frequency tables, and music/SFX playback engine.',
     '0% — unannotated da65 output'),

    ('bank17_sound_data', 'BANK $17 — SOUND/MUSIC DATA',
     'Music scores and sound effect data tables for the sound driver.',
     '0% — unannotated da65 output (pure data)'),

    ('bank18_stage_select', 'BANK $18 — STAGE SELECT + PROTO MAN SCENES',
     'Stage select screen logic, cursor handling, stage-to-bank mapping,\n'
     '; Proto Man encounter cutscenes, and password system.',
     '0% — unannotated da65 output'),

    ('bank19_sprite_offsets', 'BANK $19 — SPRITE POSITION OFFSET DATA (DEFAULT)',
     'Default sprite position offset lookup tables.',
     '0% — unannotated da65 output (pure data)'),

    ('bank1A_1B_oam_sequences', 'BANKS $1A-$1B — OAM ANIMATION SEQUENCES',
     'OAM animation sequence tables and enemy spawn/check routines.\n'
     '; Bank $1A at $8000, Bank $1B at $A000.',
     '0% — unannotated da65 output'),

    ('bank1C_1D_entity_ai', 'BANKS $1C-$1D — SPRITE PROCESSING & ENTITY AI',
     'Entity AI dispatch, sprite processing loop, hit detection callbacks,\n'
     '; and AI routines for all standard enemies and mini-bosses.\n'
     '; Bank $1C at $8000, Bank $1D at $A000.',
     '0% — unannotated da65 output'),
]


def add_header(filepath, title, description, annotation):
    """Replace the da65 auto-header with a proper annotation header."""
    with open(filepath, 'r') as f:
        lines = f.readlines()

    # Find insertion point: skip da65 header comments and blank lines,
    # stop at .setcpu or first content
    insert_at = 0
    for i, line in enumerate(lines):
        s = line.strip()
        if s.startswith('.setcpu'):
            insert_at = i
            break
        if s and not s.startswith(';') and s != '':
            insert_at = i
            break
    else:
        insert_at = len(lines)

    # Build new header
    header = []
    header.append(f'; =============================================================================\n')
    header.append(f'; MEGA MAN 3 (U) — {title}\n')
    header.append(f'; =============================================================================\n')
    header.append(f'; {description}\n')
    header.append(f';\n')
    header.append(f'; Annotation: {annotation}\n')
    header.append(f'; =============================================================================\n')
    header.append(f'\n')

    # Replace everything before .setcpu with new header
    new_lines = header + lines[insert_at:]

    with open(filepath, 'w') as f:
        f.writelines(new_lines)


def main():
    for filename, title, desc, annot in BANKS:
        filepath = os.path.join(SRC, f'{filename}.asm')
        if os.path.exists(filepath):
            add_header(filepath, title, desc, annot)
            print(f'  {filename}: header added')
        else:
            print(f'  {filename}: NOT FOUND')


if __name__ == '__main__':
    main()
