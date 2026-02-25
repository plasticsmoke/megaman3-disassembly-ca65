# Mega Man 3 (U) — ca65 Disassembly

A byte-perfect, fully annotated disassembly of **Mega Man 3** (NES, US release) targeting the [ca65](https://cc65.github.io/doc/ca65.html) assembler. Every instruction line across all 20 code banks has an inline comment. Data banks have section headers, record boundaries, and enemy name annotations. An automated 25-category health check verifies comment accuracy against the actual instructions.

Built with [Claude Code](https://claude.com/claude-code) — starting from raw da65 output through label renaming, constant extraction, and full annotation.

Anyone familiar with Mega Man 3's internals, NES development, or MMC3 mapper conventions is welcome to double-check the annotations and file corrections or improvements.

## Building

Requires ca65/ld65 (from [cc65](https://cc65.github.io/)) and GNU Make.

```
make
```

Produces:
- `build/mm3_built.nes` — byte-perfect ROM, verified against the original
- `build/mm3.nsfe` — NSFe soundtrack (built from source — see below)

### Expected Checksums

| Algorithm | Value |
|-----------|-------|
| CRC32 | `452D8089` |
| MD5 | `75b924155cafee335c9ea7a01bfc8efb` |
| SHA-1 | `53197445e137e47a73fd4876b87e288ed0fed5c6` |
| SHA-256 | `eddbe571cf0a201ba9d090fbace9843c9c9dd4053051649f2575b088752f4675` |

## NSFe Soundtrack

The NSFe file is built entirely from source — no ROM extraction, no external scripts. A two-pass ca65/ld65 pipeline assembles the sound engine banks ($16/$17/$18) with `-D NSF_BUILD`, which NOPs out a mapper bank-switch check that isn't needed in the linear NSF address space. The result is linked into a raw PRG binary, then wrapped by `src/nsfe.asm` into a complete NSFe container with chunk headers, track metadata, and the PRG payload via `.incbin`.

All metadata lives in `src/nsfe.asm` as assembly directives: track names, per-track durations, fade times, and composer credits. Chunk sizes auto-calculate via label math. To change a track title or timing, edit the file and rebuild.

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
    fixed_bank.asm              Fixed bank master file — header docs, address defs,
                                .include directives for the 12 sub-files below
    nmi.asm                     NMI handler (OAM DMA, palette, scroll, IRQ setup)
    irq.asm                     13 IRQ handlers (scanline splits) + vector tables
    ppu_utils.asm               PPU buffer drain, rendering, controllers, fade, stage load
    game_loop.asm               Main game entry, stage init, frame loop, pause, game over
    player_ground.asm           Player state $00 (ground) + airborne sub + weapon fire
    player_states.asm           Player states $02-$0D + weapon init routines
    player_warp.asm             Player states $10-$15 (scroll, warp, auto-walk, Break Man)
    camera.asm                  Scroll engine, room transitions, vertical scroll, metatile rendering
    collision.asm               Tile collision, breakable blocks, snap routines, metatile PPU writes
    sprites.asm                 Sprite animation engine, OAM assembly, energy bars
    movement.asm                Entity movement with collision, gravity, velocity, facing, helpers
    system.asm                  RESET, cooperative scheduler, bank switching, sound, vectors
  nsfe.asm                      NSFe container (metadata + .incbin PRG payload)
  chr.asm                       CHR ROM (128 KB pattern tables)
include/
  zeropage.inc                  Zero-page variable definitions (~80 vars)
  constants.inc                 Named constants (entity arrays, stage/weapon IDs,
                                button masks, tile types, music/SFX IDs)
  hardware.inc                  NES hardware registers (PPU, APU, controller, MMC3)
cfg/
  nes.cfg                       ld65 linker configuration (ROM)
  nsfe_prg.cfg                  ld65 linker configuration (NSFe sound banks)
  nsfe.cfg                      ld65 linker configuration (NSFe container)
chr/
  chr.bin                       Raw CHR data (128 KB)
```

## Annotation

The raw da65 disassembly produced ~2,340 `L_XXXX` address labels and ~970 `code_XXXX` branch targets. All have been replaced with descriptive names based on code analysis (~3,310 total). Named constants cover entity arrays, stage/weapon/player-state IDs, button masks, tile types, music/SFX IDs, and NES hardware registers. ~80 zero-page variables are named, including general-purpose temps ($00-$0F) and sound driver workspace ($C0-$CF). 21 `.byte` blocks where da65 emitted instructions as raw data have been converted back to proper mnemonics. All 46 cross-bank imports (`LXXXX` symbols) have been resolved to descriptive names or hardware registers.

Inline comments align to column 40. Automated health checks were run during this process for verification: branch condition contradictions, load/store direction, operand value mismatches, entity array cross-confusion, constant value verification, stale address references, unreachable code detection, and more.

### Label Renaming by Bank

- **Fixed bank ($1E/$1F)**: ~928 labels — player states, collision detection, camera/scroll engine, rendering, cooperative scheduler, bank switching
- **Entity AI ($1C/$1D)**: ~946 labels — all 100+ entity AI routines in the dispatch table, from Metalls to Wily Machine phases
- **Fortress bosses ($12)**: ~408 labels — Yellow Devil, Kamegoro Maker, Holograph, Wily Machine A/B/C, Gamma
- **Robot Masters ($04-$07)**: ~566 labels across 4 banks — AI state machines for all 8 Robot Masters and 8 Doc Robot variants
- **Stage banks ($00-$08)**: enemy names, stage event handlers, palette/CHR init routines
- **Cross-bank imports**: 46 symbols resolved — `banked_XXXX` for multi-bank trampolines, `stage_select_*` for bank $18 entry points, `music_driver_*` for bank $0E, `entity_ai_*` for bank $1C, `MMC3_MIRRORING` for hardware register writes

## Engine Notes

**Cooperative multitasking.** The game runs a stack-based coroutine scheduler in the fixed bank. Game logic calls `task_yield` to surrender control each frame. The NMI handler processes rendering, then patches the return address on the stack to route through the sound driver before resuming mainline code — avoiding bank-switch conflicts when NMI interrupts mid-swap.

**Entity system.** 32 entity slots using 18 parallel arrays at $0300-$05FF with $20 stride. Slot 0 is the player, slots $01-$0F are weapons/projectiles, $10-$1F are enemies/items/bosses. The `ent_hp` field doubles as a lifetime timer for projectiles. The `ent_routine` index encodes both the target bank and routine offset — values $00-$9F dispatch from bank $1D, $A0-$DF from the Robot Master banks ($04-$07), $E0-$FF from the fortress boss bank ($12). One pointer table, three bank groups.

**Mixed code+data banks.** Stage banks ($00-$08) pack both level layout data and stage-specific code in the same 8 KB window. Enemy spawn tables always sit at offset $0E00 ($AE00 CPU). The per-stage bank mapping table handles shared banks — bank $0D serves three different stages (Doc Shadow, Doc Snake, and Wily 1).

**IRQ scanline splits.** Multiple IRQ handler groups implement per-scanline screen effects: gameplay status bar, horizontal/vertical scroll splits, water wave distortion, screen wipes, stage select grid rendering, and mid-frame CHR bank swaps. Each screen mode chains its own custom split configuration through the MMC3 scanline counter.

**22 player states.** Beyond the standard ground/airborne/slide/ladder states, the engine has dedicated states for Top Spin recoil bounce, Hard Knuckle fire freeze, Doc Flash Time Stopper death, vertical scroll transitions, warp tube sequences, boss intro freeze, and two scripted auto-walk sequences for the ending.

**Cross-track music data sharing.** The sound data in bank $17 is not 13 isolated tracks — it's a continuous stream where track headers serve as entry points into a shared pool of channel sequences. The driver's loop (`$0E`) and jump (`$0F`) commands reference absolute addresses, so one track's channels can live inside another track's address range. Snake Man's entire stage theme is 62 bytes; its melody loops back into Top Man's data, and its percussion channel jumps into Spark Man's. Wily 3-4 is even smaller at 53 bytes — effectively a remix header that points 3 of its 4 channels into Wily 5-6's data. Shadow Man (75 bytes) and the Ending theme follow the same pattern. The only clean boundary between adjacent tracks is Gemini Man / Hard Man.

**Debug code in retail ROM.** Player 2 controller input activates debug features: P2 Right grants super jump + pit immunity, P2 Up enables slow-motion, P2 A freezes all entities, P2 Left latches permanent rightward movement.

## Acknowledgments

- Initial xkas disassembly and enemy/routine label naming by Raidenthequick ([original repo](https://github.com/refreshing-lemonade/megaman3-disassembly)).
- Enemy names sourced from official Capcom Rockman 3 documentation.

## License

MIT License. See [LICENSE](LICENSE) for details.

This is a disassembly — the original game is copyrighted by Capcom. This project provides only the annotated assembly source. No ROM data is included.
