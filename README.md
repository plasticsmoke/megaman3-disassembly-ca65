# Mega Man 3 (U) — ca65 Disassembly

An experiment in using [Claude Code](https://claude.com/claude-code) to disassemble and cleanly, accurately annotate a NES game. This is a byte-perfect disassembly of **Mega Man 3** (NES, US release) targeting the [ca65](https://cc65.github.io/doc/ca65.html) assembler.

Automated health checks verify comment accuracy against the actual instructions — catching branch condition mismatches, X/Y axis confusion, load/store direction errors, operand value contradictions, and more. The build system verifies byte-perfect ROM output on every build.

## Building

Requires ca65/ld65 (from [cc65](https://cc65.github.io/)) and GNU Make.

```
make
```

Produces `build/mm3_built.nes` and verifies it matches the original ROM byte-for-byte.

### Expected Checksums

| Algorithm | Value |
|-----------|-------|
| CRC32 | `452D8089` |
| MD5 | `75b924155cafee335c9ea7a01bfc8efb` |
| SHA-1 | `53197445e137e47a73fd4876b87e288ed0fed5c6` |
| SHA-256 | `eddbe571cf0a201ba9d090fbace9843c9c9dd4053051649f2575b088752f4675` |

## ROM Layout

Mapper 4 (MMC3). 256 KB PRG + 128 KB CHR. Fixed bank always mapped at $C000-$FFFF; two swappable windows at $8000-$9FFF and $A000-$BFFF.

| Banks | Address | Contents |
|-------|---------|----------|
| $00 | $A000 | Global enemy data |
| $01-$08 | $A000 | Stage data + per-stage code |
| $09 | $8000 | Per-frame entity subsystems |
| $0A | $A000 | Weapon damage tables |
| $0B-$0D | $A000 | Intro, game over, OAM sprites |
| $0E-$0F | $A000 | Animation frames, entity spawning |
| $10 | $8000 | Stage setup / boss post-defeat |
| $11-$15 | $A000 | Ending data, sprite offsets, weapon anim |
| $16 | $8000 | Sound driver code |
| $17 | $A000 | Music/SFX data |
| $18 | $A000 | Stage select + Proto Man scenes |
| $19 | $A000 | Sprite offset data |
| $1A/$1B | $8000/$A000 | OAM animation sequences |
| $1C/$1D | $8000/$A000 | Sprite processing + entity AI |
| $1E/$1F | $C000/$E000 | **Fixed bank** — always mapped |

## Project Structure

```
src/
  header.asm                    iNES header (Mapper 4 / MMC3)
  bank00_enemy_data.asm         Global enemy data tables + Needle Man stage
  bank01_stage_magnet.asm       CHR/palette init + Magnet Man stage
  bank02_stage_gemini.asm       Title screen + Gemini Man stage
  bank03_stage_hard.asm         Stage transition + Hard Man stage
  bank04_doc_robot_a.asm        Doc Robot AI (Flash/Wood/Crash/Metal) + Top Man stage
  bank05_doc_robot_b.asm        Doc Robot AI (Bubble/Heat/Quick/Air) + Snake Man stage
  bank06_robot_masters_a.asm    Robot Master AI (Needle/Magnet/Top/Shadow) + Spark Man stage
  bank07_robot_masters_b.asm    Robot Master AI (Hard/Spark/Snake/Gemini) + Shadow Man stage
  bank08_stage_doc_needle.asm   Doc Robot Needle stage data
  bank09_per_frame.asm          Per-frame entity subsystems + Doc Robot Gemini stage
  bank0A_damage_tables.asm      Weapon damage tables + Doc Robot Spark stage
  bank0B_intro.asm              Intro sequence + Doc Robot Shadow stage
  bank0C_game_over.asm          Game over / results + Wily 1 stage
  bank0D_oam_sprites.asm        OAM sprite animation + Wily 2/3/5 stages
  bank0E_anim_frames.asm        Animation frame management + Wily 6 stage
  bank0F_entity_spawn.asm       Entity spawning / projectile dispatch + Wily 4 stage
  bank10_stage_setup.asm        Stage setup / boss post-defeat
  bank11_ending_data.asm        Special / ending stage data
  bank12_fortress_bosses.asm    Fortress boss AI + special entities
  bank13_ending_data2.asm       Additional ending stage data
  bank14_sprite_offsets_alt.asm Sprite offset data (alternate)
  bank15_weapon_anim.asm        Weapon sprite animation sequences
  bank16_sound_driver.asm       NES APU sound driver
  bank17_sound_data.asm         Music / SFX data
  bank18_stage_select.asm       Stage select + Proto Man scenes
  bank19_sprite_offsets.asm     Sprite offset data (default)
  bank1A_1B_oam_sequences.asm   OAM animation sequences (dual bank)
  bank1C_1D_entity_ai.asm       Sprite processing + entity AI (dual bank)
  fixed/
    fixed_bank.asm              Fixed bank — main game loop, player states, movement,
                                collision, camera, rendering ($C000-$FFFF)
  chr.asm                       CHR ROM (128 KB pattern tables)
include/
  zeropage.inc                  Zero-page variable definitions (~80 vars)
  constants.inc                 Named constants (entity arrays, stage/weapon IDs,
                                button masks, tile types, music/SFX IDs)
cfg/
  nes.cfg                       ld65 linker configuration
chr/
  chr.bin                       Raw CHR data (128 KB)
```

## Annotation Progress

The disassembly is byte-perfect. Annotation is ongoing — all 30 bank files have section headers, inline comments, and named labels. All auto-labels (`L_XXXX`) and internal branch targets (`code_XXXX`) have been replaced with descriptive names (~3,310 total). Cross-bank imports have been named where possible (28 of 46 renamed, 17 orphaned artifacts removed). Named constants cover entity arrays, stage/weapon/player-state IDs, button masks, tile types, music/SFX IDs, and NES hardware registers. Zero-page variables cover ~80 addresses including general-purpose temps ($00-$0F) and sound driver workspace ($C0-$CF). 21 `.byte` code blocks have been converted back to proper instructions across 6 files. Pure data banks have section boundary markers, record indices, and enemy name annotations. An automated health check verifies comment accuracy and header correctness across 19 categories.

| Bank | File | Annotation |
|------|------|------------|
| $10 | bank10_stage_setup.asm | ~100% — 21 labels, 373 comments |
| $0E | bank0E_anim_frames.asm | ~100% — 14 labels, 298 comments |
| $03 | bank03_stage_hard.asm | ~94% — 107 labels, 731 comments |
| $0B | bank0B_intro.asm | ~93% — 94 labels, 866 comments |
| $16 | bank16_sound_driver.asm | ~92% — 170 labels, 1072 comments |
| $18 | bank18_stage_select.asm | ~91% — 175 labels, 1771 comments |
| $07 | bank07_robot_masters_b.asm | ~91% — 150 labels, 1041 comments |
| $06 | bank06_robot_masters_a.asm | ~91% — 141 labels, 956 comments |
| $09 | bank09_per_frame.asm | ~91% — 80 labels, 585 comments |
| $0F | bank0F_entity_spawn.asm | ~89% — 30 labels, 372 comments |
| $04 | bank04_doc_robot_a.asm | ~88% — 123 labels, 864 comments |
| $0C | bank0C_game_over.asm | ~88% — 85 labels, 848 comments |
| $12 | bank12_fortress_bosses.asm | ~85% — 408 labels, 2501 comments |
| $02 | bank02_stage_gemini.asm | ~84% — 117 labels, 785 comments |
| $05 | bank05_doc_robot_b.asm | ~81% — 152 labels, 906 comments |
| $1A/$1B | bank1A_1B_oam_sequences.asm | ~81% — 30 labels, 303 comments |
| Fixed | fixed_bank.asm | ~71% — 928 labels, 6901 comments |
| $1C/$1D | bank1C_1D_entity_ai.asm | ~71% — 946 labels, 5360 comments |
| $01 | bank01_stage_magnet.asm | ~73% — 4 labels, 221 comments |
| $0D | bank0D_oam_sprites.asm | ~73% — 6 labels, 282 comments |
| $00 | bank00_enemy_data.asm | Data — 8 labels, 364 comments |
| $08 | bank08_stage_doc_needle.asm | Data — section markers, enemy names, column indices |
| $0A | bank0A_damage_tables.asm | Data — 10 labels, 539 comments |
| $11 | bank11_ending_data.asm | Data — section markers, enemy names, column indices |
| $13 | bank13_ending_data2.asm | Data — section markers, enemy names, column indices |
| $14 | bank14_sprite_offsets_alt.asm | Data — 61 sprite record boundaries |
| $15 | bank15_weapon_anim.asm | Data — 13 animation sequence markers |
| $17 | bank17_sound_data.asm | Data — 13 music track boundaries |
| $19 | bank19_sprite_offsets.asm | Data — 91 sprite record boundaries |

## Technical Notes

- 32 entity slots with $20 stride, arrays at $0300-$05FF
- 22 player states dispatched via pointer table in the fixed bank
- Direct routine-index AI dispatch for entity behavior
- Palette buffer at $0600-$061F
- Sound driver in bank $16, music/SFX data in bank $17

## License

MIT License. See [LICENSE](LICENSE) for details.

This is a disassembly — the original game is copyrighted by Capcom. This project provides only the annotated assembly source. No ROM data is included.
