# Mega Man 3 (U) — ca65 Disassembly

A byte-perfect disassembly of **Mega Man 3** (NES, US release) targeting the [ca65](https://cc65.github.io/doc/ca65.html) assembler. Builds an identical ROM from source.

## Building

**Requirements:** ca65/ld65 (from the [cc65](https://cc65.github.io/) suite) and GNU Make.

```
make
```

Produces `build/mm3_built.nes` — a byte-perfect rebuild of the original ROM:

```
BUILD VERIFIED: byte-perfect match!
```

**Expected checksum** (SHA-256): `eddbe571cf0a201ba9d090fbace9843c9c9dd4053051649f2575b088752f4675`

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
include/
  zeropage.inc                Zero-page variable definitions
  constants.inc               Named constants (entity arrays, IDs, masks)
cfg/
  nes.cfg                     ld65 linker configuration
chr/
  chr.bin                     Raw CHR data (128KB)
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

## Stage Data Map

Most bank files are **dual-purpose**: executable code *plus* stage layout data packed into the same $A000-$BFFF region. The engine selects the stage data bank via `stage_to_bank[$22]` (table at `$C8B9` in the fixed bank). Stage layout data lives at `$AA00+`; enemy spawn tables at `$A000-$A4FF`.

| Stage | ID | Bank | Source File | Code in Same Bank |
|-------|----|------|-------------|-------------------|
| Needle Man | $00 | $00 | `bank00_enemy_data.asm` | Global enemy data tables |
| Magnet Man | $01 | $01 | `bank01_stage_magnet.asm` | CHR/palette init |
| Gemini Man | $02 | $02 | `bank02_stage_gemini.asm` | Title screen |
| Hard Man | $03 | $03 | `bank03_stage_hard.asm` | Stage transition, password system |
| Top Man | $04 | $04 | `bank04_doc_robot_a.asm` | Doc Robot AI (Flash/Wood/Crash/Metal) |
| Snake Man | $05 | $05 | `bank05_doc_robot_b.asm` | Doc Robot AI (Bubble/Heat/Quick/Air) |
| Spark Man | $06 | $06 | `bank06_robot_masters_a.asm` | Robot Master AI (Needle/Magnet/Top/Shadow) |
| Shadow Man | $07 | $07 | `bank07_robot_masters_b.asm` | Robot Master AI (Hard/Spark/Snake/Gemini) |
| Doc Robot Needle | $08 | $08 | `bank08_stage_doc_needle.asm` | *(pure data)* |
| Doc Robot Gemini | $09 | $09 | `bank09_per_frame.asm` | Per-frame entity subsystems |
| Doc Robot Spark | $0A | $0A | `bank0A_damage_tables.asm` | Weapon damage tables |
| Doc Robot Shadow | $0B | $0B | `bank0B_intro.asm` | Intro cinematic |
| Wily Fortress 1 | $0C | $0C | `bank0C_game_over.asm` | Game over / results screen |
| Wily Fortress 2 | $0D | $0D | `bank0D_oam_sprites.asm` | OAM sprite animation |
| Wily Fortress 3 | $0E | $0D | `bank0D_oam_sprites.asm` | *(shared with Wily 2)* |
| Wily Fortress 4 | $0F | $0F | `bank0F_entity_spawn.asm` | Entity spawning / projectile dispatch |
| Wily Fortress 5 | $10 | $0D | `bank0D_oam_sprites.asm` | *(shared with Wily 2)* |
| Wily Fortress 6 | $11 | $11 | `bank11_ending_data.asm` | *(pure data)* |
| Special $12 | $12 | $12 | `bank12_fortress_bosses.asm` | Fortress boss AI |
| Special $13 | $13 | $13 | `bank13_ending_data2.asm` | *(pure data)* |
| Special $14 | $14 | $10 | `bank10_stage_setup.asm` | Stage setup / boss post-defeat |
| Special $16 | $16 | $0E | `bank0E_anim_frames.asm` | Animation frame management |

Note: Wily Fortress stages 2, 3, and 5 all share bank $0D for stage data.

## Annotation Progress

The disassembly is byte-perfect but annotation is ongoing. Each source file has an `; Annotation:` header showing its status.

| File | Status |
|------|--------|
| **fixed_bank.asm** | **~52%** — 474 labels named, ~3970 inline comments |
| **bank1C_1D_entity_ai.asm** | **~77%** — 674 labels named, 1984 inline comments |
| **bank16_sound_driver.asm** | **~87%** — 148 labels named, 37 inline comments |
| **bank12_fortress_bosses.asm** | **~78%** — 309 labels named, 52 inline comments |
| **bank09_per_frame.asm** | **~90%** — 80 labels named, 100+ inline comments |
| **bank06_robot_masters_a.asm** | **~70%** — 93 labels named, 106 inline comments |
| **bank07_robot_masters_b.asm** | **~82%** — 120 labels named, 22 inline comments |
| **bank04_doc_robot_a.asm** | **~87%** — 104 labels named, 9 inline comments |
| **bank05_doc_robot_b.asm** | **~75%** — 111 labels named |
| **bank03_stage_hard.asm** | **~65%** — 69 labels named, 99 inline comments |
| **bank02_stage_gemini.asm** | **~82%** — 117 labels, 462 inline comments |
| **bank0B_intro.asm** | **~68%** — 94 labels, 543 inline comments |
| **bank0C_game_over.asm** | **~73%** — 85 labels, 601 inline comments |
| **bank0D_oam_sprites.asm** | **~35%** — 6 labels, 146 inline comments |
| **bank0E_anim_frames.asm** | **~40%** — 14 labels, 164 inline comments |
| **bank0F_entity_spawn.asm** | **~45%** — 31 labels, 222 inline comments |
| **bank10_stage_setup.asm** | **~50%** — 21 labels, 211 inline comments |
| **bank01_stage_magnet.asm** | **~45%** — 4 labels, 174 inline comments |
| **bank18_stage_select.asm** | **~41%** — 68 labels named, 331 inline comments |
| **bank00_enemy_data.asm** | **~55%** — 8 labels, 243 comments, 121 inline annotations |
| **bank0A_damage_tables.asm** | **~60%** — 9 labels, 217 comments, 322 inline annotations |
| **bank1A_1B_oam_sequences.asm** | **~28%** — 30 labels, 194 comments, 100 inline annotations |
| Pure data banks (08, 11, 13, 14, 15, 17, 19) | Section headers + structural annotations |

**Overall: all 30 bank files annotated.** Every bank has section headers and contextual comments. The fixed bank and bank1C_1D have the richest annotations; bank0A has full weapon damage tables with per-boss weakness notes. Pure data banks (08, 11, 13, 14, 15, 17, 19) have structural headers. 1,292 auto-labels renamed to descriptive function names across all bank files.

Next steps: Mesen-verified label renaming and detailed comments on remaining unknowns.

## Technical Notes

- 32 entity slots with $20 stride (vs MM4's 24 slots / $18 stride)
- 22 player states dispatched via pointer table at the fixed bank
- Direct routine-index AI dispatch (vs MM4's page-based state machine)
- Entity arrays at $0300-$05FF, palette buffer at $0600-$061F
- Sound driver in bank $16, data in bank $17

## License

MIT License. See [LICENSE](LICENSE) for details.

This is a disassembly — the original game is copyrighted by Capcom. This project provides only the annotated assembly source. No ROM data is included.
