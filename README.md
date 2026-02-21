# Mega Man 3 (U) — ca65 Disassembly

A byte-perfect disassembly of **Mega Man 3** (NES, US release) targeting the [ca65](https://cc65.github.io/doc/ca65.html) assembler. Builds an identical ROM from source.

## Building

**Requirements:** ca65/ld65 (from the [cc65](https://cc65.github.io/) suite) and GNU Make.

**You must supply your own ROM.** Place a Mega Man 3 (U) ROM as `mm3.nes` in the project root. The build compares the output against this file for verification.

```
make
```

Produces `build/mm3_built.nes` and verifies it byte-for-byte against the original:

```
BUILD VERIFIED: byte-perfect match!
```

## Project Structure

```
src/
  header.asm                  iNES header (Mapper 4 / MMC3)
  bank00_enemy_data.asm       Global enemy data tables
  bank01_stage_magnet.asm     CHR/palette init + Magnet Man stage data
  bank02_stage_gemini.asm     Gemini Man stage data + title screen
  bank03_stage_hard.asm       Stage transition + Hard Man stage data
  bank04_doc_robot_a.asm      Doc Robot AI (Flash/Wood/Crash/Metal)
  bank05_doc_robot_b.asm      Doc Robot AI (Bubble/Heat/Quick/Air)
  bank06_robot_masters_a.asm  Robot Master AI (Needle/Magnet/Top/Shadow)
  bank07_robot_masters_b.asm  Robot Master AI (Hard/Spark/Snake/Gemini)
  bank08_stage_doc_needle.asm Doc Robot Needle stage data
  bank09_per_frame.asm        Per-frame entity subsystems
  bank0A_damage_tables.asm    Weapon damage tables + Doc Robot Spark stage
  bank0B_intro.asm            Intro sequence + Doc Robot Shadow stage
  bank0C_game_over.asm        Game over/results + Wily 1 stage
  bank0D_oam_sprites.asm      OAM/sprite animation + Wily 2/3/5 stages
  bank0E_anim_frames.asm      Animation frame management
  bank0F_entity_spawn.asm     Entity spawning/projectile dispatch + Wily 4
  bank10_stage_setup.asm      Stage setup / boss post-defeat
  bank11_ending_data.asm      Special/ending stage data
  bank12_fortress_bosses.asm  Fortress bosses + special entities
  bank13_ending_data2.asm     Additional ending stage data
  bank14_sprite_offsets_alt.asm  Sprite offset data (alternate)
  bank15_weapon_anim.asm      Weapon sprite animation sequences
  bank16_sound_driver.asm     NES APU sound driver
  bank17_sound_data.asm       Music/SFX data
  bank18_stage_select.asm     Stage select + Proto Man scenes
  bank19_sprite_offsets.asm   Sprite offset data (default)
  bank1A_1B_oam_sequences.asm OAM animation sequences (banks $1A-$1B)
  bank1C_1D_entity_ai.asm    Sprite processing & entity AI (banks $1C-$1D)
  fixed/
    fixed_bank.asm            Fixed bank — main game logic (banks $1E-$1F)
  chr.asm                     CHR ROM (128KB pattern tables)
cfg/
  nes.cfg                     ld65 linker configuration
chr/
  chr.bin                     Raw CHR data (128KB)
tools/
  annotate_fixed_bank.py      Transfers annotations from xkas reference
  add_bank_headers.py         Adds description/status headers to bank files
```

## ROM Layout

| Banks | Address | Contents |
|-------|---------|----------|
| $00 | $A000 | Global enemy data |
| $01–$08 | $A000 | Stage data + per-stage code |
| $09 | $8000 | Per-frame entity subsystems |
| $0A | $A000 | Weapon damage tables |
| $0B–$0D | $A000 | Intro, game over, OAM sprites |
| $0E–$0F | $A000 | Animation frames, entity spawning |
| $10 | $8000 | Stage setup / boss post-defeat |
| $11–$15 | $A000 | Ending data, sprite offsets, weapon anim |
| $16 | $8000 | Sound driver code |
| $17 | $A000 | Music/SFX data |
| $18 | $A000 | Stage select + Proto Man scenes |
| $19 | $A000 | Sprite offset data |
| $1A/$1B | $8000/$A000 | OAM animation sequences |
| $1C/$1D | $8000/$A000 | Sprite processing & entity AI |
| $1E/$1F | $C000/$E000 | **Fixed bank** — always mapped |

Mapper 4 (MMC3). 256KB PRG + 128KB CHR. Fixed bank at $C000-$FFFF; swappable banks at $8000-$9FFF and $A000-$BFFF.

## Annotation Progress

The disassembly is byte-perfect but annotation is ongoing. Each source file has an `; Annotation:` header showing its status.

| File | Status |
|------|--------|
| **fixed_bank.asm** | **~52%** — 474 labels named, ~3970 inline comments |
| All other banks | 0% — unannotated da65 output |

**Overall: ~2% annotated** (1 of 30 code files). The fixed bank contains the core game engine and was annotated first since it's the most important — NMI/IRQ handlers, player state machine, physics, weapon logic, collision detection, bank switching.

The goal is 100% annotation across all banks.

## Technical Notes

- 32 entity slots with $20 stride (vs MM4's 24 slots / $18 stride)
- 22 player states dispatched via pointer table at the fixed bank
- Direct routine-index AI dispatch (vs MM4's page-based state machine)
- Entity arrays at $0300-$05FF, palette buffer at $0600-$061F
- Sound driver in bank $16, data in bank $17

## License

This is a disassembly of a copyrighted game for research and education purposes. You must own a copy of Mega Man 3 to use this project. The ROM file (`mm3.nes`) is not included.
