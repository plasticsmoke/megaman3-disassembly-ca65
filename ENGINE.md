# Mega Man 3 Engine Reference

A detailed reference to how the Mega Man 3 NES game engine works, based on the fully annotated ca65 disassembly. Covers the frame loop, entity system, player state machine, collision, scrolling, PPU rendering pipeline, bank switching, sound driver, and IRQ-based screen effects.

For build instructions and project structure, see [README.md](README.md).

---

## Table of Contents

- [Architecture Overview](#architecture-overview)
- [Memory Map](#memory-map)
- [Frame Lifecycle](#frame-lifecycle)
- [Cooperative Scheduler](#cooperative-scheduler)
- [NMI Handler](#nmi-handler)
- [Gameplay Frame Loop](#gameplay-frame-loop)
- [Player State Machine](#player-state-machine)
- [Entity System](#entity-system)
- [Collision System](#collision-system)
- [Camera and Scrolling](#camera-and-scrolling)
- [PPU Buffer System](#ppu-buffer-system)
- [Sprite Animation Engine](#sprite-animation-engine)
- [Bank Switching](#bank-switching)
- [Sound Driver](#sound-driver)
- [IRQ Scanline Effects](#irq-scanline-effects)
- [Stage Data Format](#stage-data-format)
- [Debug Features](#debug-features)

---

## Architecture Overview

Mega Man 3 runs on Mapper 4 (MMC3) with 256 KB PRG ROM (32 x 8 KB banks) and 128 KB CHR ROM. The MMC3 provides two swappable PRG windows and a scanline-counting IRQ.

```
$8000-$9FFF   Swappable PRG window 1 (8 KB)
$A000-$BFFF   Swappable PRG window 2 (8 KB)
$C000-$FFFF   Fixed bank (16 KB) — always mapped, contains the engine
```

The fixed bank holds all core engine code: NMI/IRQ handlers, game loop, player logic, collision, camera, sprite engine, movement, bank switching, and the cooperative scheduler. Swappable banks hold stage data, entity AI, boss routines, sound driver, animation data, and sprites.

Key files:

| File | Contents |
|------|----------|
| `src/fixed/*.asm` | Fixed bank engine code (12 sub-files) |
| `src/bank1C_1D_entity_ai.asm` | Entity AI dispatch + all standard entity routines |
| `src/bank04-07_*.asm` | Robot Master and Doc Robot AI |
| `src/bank12_fortress_bosses.asm` | Fortress boss AI (Yellow Devil, Gamma, etc.) |
| `src/bank16_sound_driver.asm` | Sound driver code |
| `src/bank17_sound_data.asm` | Music and SFX data |
| `include/zeropage.inc` | ~80 named zero-page variables |
| `include/constants.inc` | Entity arrays, game IDs, button masks |
| `include/hardware.inc` | NES hardware register definitions |

---

## Memory Map

### Zero Page ($00-$FF)

The engine divides zero page into functional regions:

| Range | Purpose | Key Variables |
|-------|---------|---------------|
| `$00-$0F` | Scratch temps | `temp_00` through `temp_0F` — freely reused |
| `$14-$16` | Controller input | `joy1_press`, `joy1_press_alt`, `joy1_held` |
| `$18-$1A` | PPU dirty flags | `palette_dirty`, `nametable_dirty`, `nt_column_dirty` |
| `$22` | Stage | `stage_id` |
| `$30-$44` | Player state | `player_state`, `player_facing`, `walk_flag`, `invincibility_timer`, `jump_counter`, `hazard_pending`, `tile_at_feet_*` |
| `$50-$7B` | Scroll / screen | `scroll_lock`, `boss_active`, `bosses_beaten`, `screen_mode`, `scroll_x_fine`, `nt_select`, `irq_scanline` |
| `$80-$8F` | Scheduler / envelopes | 4 task slots (4 bytes each), also used as sound envelope timers |
| `$90-$9B` | NMI / timing | `nmi_occurred`, `frame_counter`, `oam_ptr`, `gravity`, `irq_enable` |
| `$A0-$AF` | Weapon / inventory | `current_weapon`, `player_hp`, `weapon_ammo`, `lives`, `etanks` |
| `$C0-$CF` | Sound driver workspace | `snd_flags`, `snd_ptr_lo/hi`, `snd_tempo_*`, `snd_pitch_offset`, `snd_fade_*` |
| `$EE-$FF` | System | `nmi_skip`, `mmc3_select`, `prg_bank`, `game_mode`, `camera_screen`, `camera_x_lo/hi`, `scroll_y`, `ppu_mask_shadow` |

### RAM Pages

| Range | Purpose |
|-------|---------|
| `$0100-$01FF` | Stack (grows down from `$01BF`) |
| `$0200-$02FF` | OAM shadow buffer (64 sprites, DMA'd to PPU each frame) |
| `$0300-$05FF` | Entity arrays (32 slots x 24 fields, stride `$20`) |
| `$0600-$061F` | Palette buffer (32 bytes, uploaded to PPU `$3F00` when dirty) |
| `$0780+` | PPU nametable update buffer |

---

## Frame Lifecycle

Each gameplay frame follows this sequence:

```
1. Mainline code runs (game loop, player, entities)
2. Mainline calls task_yield → saves SP, goes to sleep
3. VBlank fires → NMI handler runs:
   a. PPU updates (OAM DMA, nametable buffer, palette)
   b. Scroll + IRQ setup
   c. CHR bank refresh
   d. Increment frame counter, set nmi_occurred
   e. Decrement sound envelope timers
   f. Patch stack return address → sound driver trampoline
4. NMI returns → resumes at trampoline, not mainline
5. Trampoline calls play_sounds (bank $16/$17)
6. Trampoline returns → mainline code resumes
7. Goto 1
```

The stack-patching trick (step 3f) ensures the sound driver runs after every NMI without conflicting with whatever bank the mainline code was using when interrupted.

---

## Cooperative Scheduler

The engine does not use a traditional "call everything, return, wait for VBlank" loop. Instead, it uses cooperative multitasking with 4 task slots.

Each task slot (4 bytes at `$80-$8F`) holds:

| Byte | Purpose |
|------|---------|
| 0 | State: `$00`=free, `$01`=sleeping, `$02`=running, `$04`=ready, `$08`=fresh |
| 1 | Sleep countdown (decremented by NMI) |
| 2 | Saved stack pointer (state `$04`) or address low (state `$08`) |
| 3 | Address high (state `$08`) |

**task_yield**: Saves the current stack pointer into the task slot, marks the task as sleeping (`$01`), and jumps to the scheduler loop. The NMI handler wakes sleeping tasks by transitioning them to ready (`$04`).

**Scheduler loop**: Scans task slots for state >= `$04`. State `$08` = fresh task, JMP to stored address. State `$04` = resuming coroutine, restore SP and RTS. Task slot 0 gets `read_controllers` called on resume.

This allows game code, cutscenes, and other tasks to yield cleanly at any point for a frame boundary — no need to carefully return up the call stack.

---

## NMI Handler

**Source**: `src/fixed/nmi.asm` (entry at `$C000`)

Runs every VBlank. Must complete within ~2,200 CPU cycles (VBlank period) to avoid visible glitches.

### Sequence

1. **Save registers** — Push P, A, X, Y.
2. **Disable rendering** — Read `PPUSTATUS` (reset latch), clear NMI enable, set `PPUMASK = $00`.
3. **Skip check** — If `nmi_skip` ($EE) or NMI lock (`$9A`) is nonzero, skip all PPU writes and jump to scroll setup. This prevents partial updates when mainline code is mid-buffer-write.
4. **Latch scroll values** — Copy `camera_x_lo` -> `scroll_x_fine`, `camera_x_hi` -> `nt_select`, `game_mode` -> `screen_mode`.
5. **OAM DMA** — Write `$02` to `$4014`, transferring the 256-byte sprite buffer at `$0200` to PPU OAM.
6. **Primary PPU buffer** — If `nametable_dirty` is set, call `drain_ppu_buffer` (horizontal VRAM writes).
7. **Secondary PPU buffer** — If `nt_column_dirty` is set, set VRAM increment to +32 (vertical), drain buffer, restore +1.
8. **Palette upload** — If `palette_dirty` is set, copy 32 bytes from `$0600` to PPU `$3F00`.
9. **Scroll setup** — Write `PPUSCROLL` X/Y. Mode `$02` (auto-scroll) uses a separate X source (`$5F`). Re-enable rendering via `ppu_mask_shadow`. Write `PPUCTRL` with nametable select.
10. **CHR bank refresh** — Write 6 CHR bank registers (`$E8-$ED`) to MMC3.
11. **MMC3 IRQ setup** — Program scanline counter from `irq_scanline` ($7B). Load IRQ handler address from vector table indexed by `screen_mode`.
12. **Frame counter** — Increment `frame_counter` ($92), set `nmi_occurred` = `$FF`.
13. **Envelope timers** — Decrement sound envelope timers for 4 channels.
14. **Stack patch** — Overwrite the return address on the stack so RTI routes through the sound driver trampoline at `$C121` instead of returning directly to the interrupted code.

---

## Gameplay Frame Loop

**Source**: `src/fixed/game_loop.asm`

### Startup (RESET)

1. Standard NES init: disable interrupts, clear RAM, wait for PPU warm-up.
2. Initialize sound buffer (`$DC-$E3` = `$88` sentinel), set default PRG/CHR banks.
3. Clear both nametables, initialize OAM.
4. Register task 0 at `main_game_entry`, enable NMI, enter scheduler.

### main_game_entry

1. Reset stack to `$01BF`.
2. Enable IRQ, start title music, set gravity = `$40`.
3. Switch to bank `$18`, run title screen / stage select.
4. Set initial lives = 2 (displayed as 3), fill Rush Coil ammo.
5. Enter `stage_init`.

### stage_init

1. Fade to black, clear OAM and all 32 entity slots.
2. Set horizontal mirroring, zero all game state variables.
3. Start stage music (indexed by `stage_id`).
4. Render nametable columns (one per frame, yielding between each).
5. Parse room 0 config from `$AA40` (scroll flags, screen count, mirroring).
6. Load CHR/palettes, fade in with "READY" overlay (60 frames, flashing).
7. Initialize player entity: active, top of screen, teleport beam sprite, state = `PSTATE_REAPPEAR`.

### Per-Frame Gameplay Loop

Each frame of active gameplay:

```
 1. Check pause (Start pressed, state allows it)
 2. Player state dispatch → run current player state handler
 3. Apply pending hazard (damage or death)
 4. Update camera (horizontal scroll tracking)
 5. Process all entity AI (banks $1C/$1D)
 6. Check enemy spawning (bank $1A + stage bank)
 7. Per-frame subsystems via bank $09:
    - Checkpoint tracking
    - Screen scroll processing
    - HUD update
    - Sound processing
    - Background animation
    - Item pickup
 8. Palette fade tick
 9. Build OAM + yield to NMI
10. Debug input check (P2 Left)
11. Exit conditions: boss defeated ($59), player died ($3C), stage clear ($74)
```

---

## Player State Machine

**Source**: `src/fixed/player_ground.asm`, `player_states.asm`, `player_warp.asm`

22 states dispatched through `player_state_ptr_lo/hi` tables:

| ID | Constant | Handler | Description |
|----|----------|---------|-------------|
| `$00` | `PSTATE_GROUND` | `player_on_ground` | Idle, walking, jump initiation, slide, shoot |
| `$01` | `PSTATE_AIRBORNE` | `player_airborne` | Jumping or falling, variable-height jump |
| `$02` | `PSTATE_SLIDE` | `player_slide` | 20-frame slide at 2.5 px/f, uncancellable first 9 frames |
| `$03` | `PSTATE_LADDER` | `player_ladder` | Climbing, can fire or jump off |
| `$04` | `PSTATE_REAPPEAR` | `player_reappear` | Teleport beam drops from top of screen |
| `$05` | `PSTATE_RIDE` | `player_entity_ride` | Riding Mag Fly (magnetic pull) |
| `$06` | `PSTATE_DAMAGE` | `player_damage` | Knockback with gravity, hit flash |
| `$07` | `PSTATE_SPECIAL_DEATH` | `player_special_death` | Doc Flash Time Stopper kill |
| `$08` | `PSTATE_RUSH_MARINE` | `player_rush_marine` | Riding Rush Marine submarine |
| `$09` | `PSTATE_BOSS_WAIT` | `player_boss_wait` | Frozen during boss intro |
| `$0A` | `PSTATE_TOP_SPIN` | `player_top_spin` | Top Spin recoil bounce (8 frames) |
| `$0B` | `PSTATE_RECOIL` | `player_weapon_recoil` | Hard Knuckle fire freeze (16 frames) |
| `$0C` | `PSTATE_VICTORY` | `player_victory` | Boss defeated cutscene |
| `$0D` | `PSTATE_TELEPORT` | `player_teleport` | Teleport away (exit stage) |
| `$0E` | `PSTATE_DEATH` | `player_death` | Death explosion animation |
| `$0F` | `PSTATE_STUNNED` | `player_stunned` | Frozen by external force |
| `$10` | `PSTATE_SCROLL` | `player_screen_scroll` | Vertical scroll transition |
| `$11` | `PSTATE_WARP_INIT` | `player_warp_init` | Teleporter tube entry |
| `$12` | `PSTATE_WARP_ANIM` | `player_warp_anim` | Warp animation |
| `$13` | `PSTATE_BEAM` | `player_teleport_beam` | Proto Man exit beam |
| `$14` | `PSTATE_AUTO_WALK` | `player_auto_walk` | Scripted walk to X=$50 |
| `$15` | `PSTATE_AUTO_WALK2` | `player_auto_walk_2` | Scripted walk to X=$68 |

### Pause Permission

A per-state bitmask table controls which states allow pausing. Bit 7 set = cannot pause. States that block pause: slide, reappear, damage, special death, boss wait, and all states from weapon recoil ($0B) through auto-walk ($15).

### Player Physics

| Parameter | Value | Speed |
|-----------|-------|-------|
| Walk speed | `$01.4C` | 1.30 px/frame |
| Slide speed | `$02.80` | 2.50 px/frame |
| Normal jump | `$04.E5` | 4.89 px/frame initial |
| Rush Coil jump | `$06.EE` | 6.93 px/frame initial |
| Debug super jump | `$08.00` | 8.00 px/frame initial |
| Gravity (entities) | `$00.55` | 0.33 px/frame² |
| Gravity (player) | `$00.40` | 0.25 px/frame² |
| Terminal velocity | `$F9.00` | -7.00 px/frame (signed) |
| Resting velocity (player) | `$FF.C0` | -0.25 px/frame (gentle ground pin) |
| Resting velocity (enemies) | `$FF.AB` | -0.33 px/frame |

All velocities use 8.8 fixed-point (whole.fraction). Negative values are two's complement — `$F9.00` = -7.

Gravity has two values because the player state dispatch prelude writes `$40` to `gravity` each frame, then entity AI (`process_sprites` in bank `$1C`) overwrites it to `$55`. Since the player runs first, player physics uses `$40`; entities processed afterward use `$55`.

### Variable-Height Jump

The airborne state checks whether the A button is still held. If the player releases A while still rising, the velocity is immediately set to the resting value `$FF.C0` (-0.25 px/f) via `reset_gravity`, killing upward momentum and letting gravity take over. This creates the classic Mega Man short-hop vs. full-jump feel.

### Weapon Fire

`fire_weapon` in `player_ground.asm` handles all weapon types. It checks `current_weapon`, scans for a free projectile slot ($01-$0F), and dispatches through `weapon_init_ptr_lo/hi` tables to weapon-specific initialization routines. Each weapon has its own entity setup: velocity, sprite, hitbox, HP/lifetime.

---

## Entity System

**Source**: `src/fixed/sprites.asm`, `movement.asm`, `src/bank1C_1D_entity_ai.asm`

### Struct-of-Arrays Storage

32 entity slots stored as 24 parallel arrays with `$20` stride. Each array holds one field for all 32 slots:

| Array | Base | Purpose |
|-------|------|---------|
| `ent_status` | `$0300` | Active flag (bit 7 = alive) |
| `ent_routine` | `$0320` | AI routine index (encodes bank + routine) |
| `ent_x_sub` | `$0340` | X sub-pixel (fractional) |
| `ent_x_px` | `$0360` | X pixel position |
| `ent_x_scr` | `$0380` | X screen (coarse scroll position) |
| `ent_y_sub` | `$03A0` | Y sub-pixel |
| `ent_y_px` | `$03C0` | Y pixel position |
| `ent_y_scr` | `$03E0` | Y screen |
| `ent_xvel_sub` | `$0400` | X velocity sub-pixel |
| `ent_xvel` | `$0420` | X velocity whole |
| `ent_yvel_sub` | `$0440` | Y velocity sub-pixel |
| `ent_yvel` | `$0460` | Y velocity whole |
| `ent_hitbox` | `$0480` | Hitbox shape (bit 7 = contact damage to player) |
| `ent_facing` | `$04A0` | Facing direction (`$01`=right, `$02`=left) |
| `ent_spawn_id` | `$04C0` | Spawn table tracking ID |
| `ent_hp` | `$04E0` | Health / projectile lifetime |
| `ent_timer` | `$0500` | AI timer / countdown |
| `ent_var1` | `$0520` | General-purpose variable 1 |
| `ent_var2` | `$0540` | General-purpose variable 2 |
| `ent_var3` | `$0560` | General-purpose variable 3 |
| `ent_flags` | `$0580` | Bit 7=active, 6=H-flip, 5=on-ladder, 4=world coords, 3=wide margin, 2=invisible |
| `ent_anim_state` | `$05A0` | Animation state |
| `ent_anim_id` | `$05C0` | Animation/OAM ID (bit 7 = alternate bank) |
| `ent_anim_frame` | `$05E0` | Frame counter (bit 7 = damage flash) |

### Slot Allocation

| Slots | Purpose |
|-------|---------|
| `$00` | Mega Man (player) |
| `$01-$0F` | Weapons and projectiles (15 slots) |
| `$10-$1F` | Enemies, items, and bosses (16 slots) |

Only slots `$10-$1F` receive weapon-hit collision checks. The `ent_hp` field doubles as a lifetime timer for projectiles — when it counts down to zero, the projectile despawns.

### AI Dispatch

`process_sprites` in bank `$1C` iterates slots 1-31 (skipping the player). For each active entity:

1. Check Spark Shock freeze — slots stored in `$5B/$5C` skip AI when frozen.
2. Read `ent_routine,x` to determine the AI bank:

| Routine Range | Bank | Contents |
|--------------|------|----------|
| `$00-$9F` | `$1D` | Standard enemies and weapons |
| `$A0-$AF` | `$04` | Doc Robot group A (Flash, Wood, Crash, Metal) |
| `$B0-$BF` | `$05` | Doc Robot group B (Bubble, Heat, Quick, Air) |
| `$C0-$CF` | `$06` | Robot Masters A (Needle, Magnet, Top, Shadow) |
| `$D0-$DF` | `$07` | Robot Masters B (Hard, Spark, Snake, Gemini) |
| `$E0-$FF` | `$12` | Fortress bosses (Yellow Devil, Kamegoro, Gamma, etc.) |

3. Bank-switch if needed, then dispatch through `sprite_main_ptr_lo/hi` tables.

This single dispatch table with bank-encoded routine IDs lets the engine call into 6 different banks without any per-entity bank tracking.

### Enemy Spawn Data

Bank `$00` holds 6 global enemy property tables (256 entries each):

| Address | Table | Purpose |
|---------|-------|---------|
| `$A000` | `enemy_flags_g` | Initial `ent_flags` |
| `$A100` | `enemy_main_ID_g` | AI routine index for `ent_routine` |
| `$A200` | `enemy_shape_g` | Hitbox shape (bit 7 = contact damage) |
| `$A300` | `enemy_OAM_ID_g` | Animation ID |
| `$A400` | `enemy_health_g` | Starting HP (`$FF` = invincible) |
| `$A500` | `enemy_speed_ID_g` | Index into velocity lookup table |

Per-stage enemy placement lives at offset `$AE00` in each stage bank, with screen/X/Y coordinates at `$AB00/$AC00/$AD00`.

---

## Collision System

**Source**: `src/fixed/collision.asm`

### Tile Collision

Two scan routines handle orthogonal directions:

**`check_tile_horiz`** — Horizontal scan: checks multiple X positions at a fixed Y offset. Used for floor/ceiling detection (scanning across entity width). Config tables at `$EBE2` define: starting offset, check point count, Y offset, X offsets per check point.

**`check_tile_collision`** — Vertical scan: checks multiple Y positions at a fixed X offset. Used for wall detection (scanning down entity height). Config tables at `$ECE1`.

Both routines:
1. Save current PRG bank, switch to the stage bank.
2. Compute metatile grid coordinates from entity position.
3. Look up the metatile sub-tile, then read its collision attribute from the `$BF00` table.
4. Accumulate results across all check points.
5. For the player only: check for hazard tiles and set `hazard_pending`.
6. Restore the original PRG bank.

### Tile Collision Types

The upper nibble of each entry in the `$BF00` collision table determines the tile type:

| Value | Constant | Meaning |
|-------|----------|---------|
| `$00` | `TILE_AIR` | Passthrough |
| `$10` | `TILE_SOLID` | Solid ground/wall |
| `$20` | `TILE_LADDER` | Climbable |
| `$30` | `TILE_DAMAGE` | Damage on contact (lava, fire) |
| `$40` | `TILE_LADDER_TOP` | Ladder grab point (stand on top, press Down to climb) |
| `$50` | `TILE_SPIKES` | Instant kill |
| `$70` | `TILE_DISAPPEAR` | Breakable block (Gemini stages) |

### Special Overrides

**Breakable blocks**: On Gemini stages (`$02`/`$09`), a 64-byte bitfield at `$0110` tracks which blocks have been destroyed. The collision routine checks this bitfield and overrides destroyed blocks to `TILE_AIR`.

**Proto Man walls**: When `proto_man_flag` ($68) is set, per-stage wall position tables define passthrough overrides. Matching positions return `TILE_AIR`, letting the player walk through walls Proto Man has opened.

### Entity-vs-Entity Collision

Handled in `bank1C_1D_entity_ai.asm`:
- **`check_player_hit`**: Tests entities with `ent_hitbox` bit 7 (contact damage) against the player's position.
- **`check_weapon_hit`**: Tests weapon projectile slots against enemy slots, applies damage from weapon damage tables (bank `$0A`).

---

## Camera and Scrolling

**Source**: `src/fixed/camera.asm`

### Horizontal Scroll Tracking

`update_camera` runs every gameplay frame. Key variables:

| Address | Name | Purpose |
|---------|------|---------|
| `$2A` | scroll_flags | Bit 5 = horizontal scroll enabled, bits 6-7 = vertical connection type |
| `$2B` | room_index | Current room number |
| `$2C` | screen_count | Screens in current room |
| `$2D` | scroll_progress | Screens scrolled within room |
| `$F9` | camera_screen | Coarse camera position (screen number) |
| `$FC` | camera_x_lo | Fine X scroll (0-255 within current screen) |

Tracking algorithm:
1. Compute player's screen-relative X = `ent_x_px - camera_x_lo`.
2. If player is right of center (> `$80`): scroll camera right.
3. If player is left of center (< `$80`): scroll camera left.
4. Scroll speed = min(8, distance from center or movement delta).
5. On 256-pixel boundary crossing: increment/decrement `camera_screen`.
6. At room edges: clamp player X to `$10-$F0`.

### Room Transitions

When the player reaches a screen boundary on a non-scrolling screen:

**Horizontal**: Checks the room table at `$AA40` for adjacent rooms. Advances `room_index`, resets scroll progress, loads the new room, and fast-scrolls the camera.

**Vertical**: Detects player at Y >= `$E8` (bottom exit) or Y < `$09` (top exit, requires ladder state). Validates the link via `check_room_link`, changes mirroring (V-mirror for vertical layouts, H-mirror for horizontal), animates the vertical scroll, and loads the new room.

### Scroll Column Rendering

During horizontal scrolling, `render_scroll_column` queues nametable column updates into the PPU buffer at `$0780`. The camera update renders one column of metatiles per frame as the screen scrolls. In dual-nametable mode (game mode `$02`, used for Gemini Man's auto-scrolling stage), it renders columns for both nametables simultaneously.

---

## PPU Buffer System

**Source**: `src/fixed/ppu_utils.asm`

All PPU writes outside VBlank are illegal on the NES. The engine queues writes into a RAM buffer during gameplay, then the NMI handler drains the buffer during VBlank.

### Buffer Format

Buffer at `$0780`:

```
[addr_hi] [addr_lo] [count] [tile_0] [tile_1] ... [tile_N]
[addr_hi] [addr_lo] [count] [tile_0] ...
...
[$FF = terminator]
```

Each entry writes `count + 1` bytes to PPU address `addr_hi:addr_lo`. Multiple entries chain sequentially until the `$FF` terminator.

### Dirty Flags

Three flags in zero page control what the NMI handler uploads:

| Flag | Address | Triggers |
|------|---------|----------|
| `palette_dirty` | `$18` | Copy 32 bytes from `$0600` to PPU `$3F00` (all 8 palettes) |
| `nametable_dirty` | `$19` | Drain PPU buffer with +1 VRAM increment (horizontal writes) |
| `nt_column_dirty` | `$1A` | Drain PPU buffer with +32 VRAM increment (vertical column writes) |

### OAM Buffer

Sprite data lives at `$0200-$02FF` (64 hardware sprites). The NMI transfers this via OAM DMA (`$4014`) every frame.

OAM allocation is controlled by `oam_ptr` ($97):
- `$04` = include player sprites (normal gameplay)
- `$0C` = skip player sprites (boss intro sequences)
- `$30` = skip overlay sprites (reserved area `$0200-$022F`)

### Palette Buffer

`$0600-$061F` holds the 32-byte working palette (4 background + 4 sprite palettes). Gameplay code modifies this buffer and sets `palette_dirty`; the NMI uploads it to the PPU. See the Palette System section below for full details.

---

## Palette System

**Source**: `src/fixed/ppu_utils.asm` (fade routines, stage load), `src/fixed/nmi.asm` (upload), `src/bank01_stage_magnet.asm` (sprite palette table), `src/bank09_per_frame.asm` (palette animation)

### Buffer Layout

The 32-byte palette buffer at `$0600-$061F` maps directly to NES PPU palette RAM at `$3F00-$3F1F`:

```
$0600-$060F: BG palettes (4 sub-palettes x 4 colors)
  $0600-$0603: BG 0    $0604-$0607: BG 1
  $0608-$060B: BG 2    $060C-$060F: BG 3

$0610-$061F: Sprite palettes (4 sub-palettes x 4 colors)
  $0610-$0613: SP 0 — player body (default: $0F,$0F,$2C,$11 = blue)
  $0614-$0617: SP 1 — player weapon (changes per weapon selection)
  $0618-$061B: SP 2 — enemy palette 1 (per-room, from bank $01 $A030)
  $061C-$061F: SP 3 — enemy palette 2 (per-room, from bank $01 $A030)
```

A **working copy** at `$0620-$063F` holds the target palette that fade routines blend toward. Code generally writes to both buffers simultaneously.

### Palette Sources

**BG palettes** (`$0600-$060F`): Stored per-stage at offset `$AA82` within each stage's PRG bank (after the 2-byte CHR indices at `$AA80-$AA81`). Loaded during `load_room`. Also stored in bank `$18` for menus, bank `$0B` for intro, bank `$0C` for game over.

**Sprite palettes SP0/SP1** (`$0610-$0617`): Player default loaded from `load_stage_default_palette_table` (ppu_utils.asm line 693), a hardcoded 8-byte table in the fixed bank. SP1 changes when the player selects a weapon — the weapon palette table in bank `$02` (line 942) has 3 color bytes per weapon, copied to `$0611-$0613`.

**Sprite palettes SP2/SP3** (`$0618-$061F`): Set per-room by bank `$01`'s table at `$A030`. Each room's CHR/palette param (from `$AA60` in the stage bank) is multiplied by 8 to index into this table. This allows different rooms within a stage to have different enemy colors.

### Fade System

Two fade mechanisms exist:

**Blocking fade** (`fade_palette_out` / `fade_palette_in`): Used for stage transitions. Operates on all 32 bytes (BG + sprite). Works by copying the working palette (`$0620`) to the active buffer (`$0600`), then subtracting a brightness value from every color entry. NES palette values encode brightness in the upper nibble (`$0x`=darkest, `$3x`=brightest), so subtracting `$10` per step reduces brightness by one level. Values below `$0F` clamp to `$0F` (black). Each step waits 4 frames via `task_yield`. A full fade is 4 steps × 4 frames = 16 frames.

**Per-frame incremental fade** (`palette_fade_tick`): Used for smooth in-game transitions. Driven by three zero-page variables: `$1C` (active flag), `$1D` (current subtract amount), `$1E` (step delta added every 4th frame). Only affects BG palettes (`$0600-$060F`), not sprite palettes. Endpoint detection: `$1D` reaching `$F0` signals fade-out complete, `$50` signals fade-in complete.

### Palette Animation

Bank `$09` runs a 4-slot palette animation system (`palette_anim_update`, per_frame.asm line 625) that cycles 3 BG palette colors per slot based on animation definitions. This creates effects like water shimmer and lava glow. Skipped during pause or active fade.

---

## Sprite Animation Engine

**Source**: `src/fixed/sprites.asm`, `src/bank1A_1B_oam_sequences.asm`, `src/bank19_sprite_offsets.asm`, `src/bank14_sprite_offsets_alt.asm`

The sprite system transforms entity state into OAM entries through a 4-bank pipeline: animation sequences (banks `$1A`/`$1B`) define frame lists, sprite definitions within those banks specify tile arrangements, and position offset tables (banks `$19`/`$14`) provide per-tile XY offsets. The fixed bank orchestrates the pipeline, switching PRG banks twice per entity — once for animation data, once for position offsets.

### OAM Buffer

The `$0200-$02FF` buffer holds 64 hardware sprites (4 bytes each: Y position, tile index, attribute, X position). DMA-transferred to PPU OAM every NMI.

- `$0200-$022F` (sprites 0-11): Reserved for overlay sprites when `$72` is active (boss name display)
- `$0230+` (sprites 12+): Entity sprites, starting at the `oam_ptr` (`$97`) cursor

Before rendering, `prepare_oam_buffer` writes `$F8` (off-screen) to the Y byte of all unused OAM slots from `oam_ptr` to end of buffer.

### OAM Priority Fairness

The NES draws lower-numbered OAM entries on top. Since entities fill OAM sequentially from `oam_ptr`, entities processed first get lower indices (higher priority). The engine **alternates iteration direction each frame**: forward (slots 0→31) on even frames, backward (31→0) on odd frames. Energy bar drawing also alternates. This distributes sprite priority evenly over time, preventing permanent flickering bias toward any entity.

### Rendering Pipeline

For each active entity (`ent_status` bit 7 set):

**1. Screen culling** (`process_entity_display`): Converts world coordinates to screen coordinates. Entities with world positioning (`ent_flags` bit 4) compute screen X as `ent_x_px - camera_x_lo` with page comparison. Off-screen entities are deactivated (or kept alive within a 72-pixel margin if `ent_flags` bit 3 is set). Screen position stored in `$12` (Y) and `$13` (X).

**2. Bank selection** (`setup_sprite_render`): Reads `ent_flags` bit 6 for H-flip state → `$10` (EOR mask) and `$11` (0 or 1 offset). Selects PRG bank for animation data:
- `ent_anim_id` bit 7 clear: bank `$1A`
- `ent_anim_id` bit 7 set: bank `$1B` (ID masked to 7-bit)
- Boss slots ($10-$1F) when boss is active: bank `$15` (weapon sprites)

**3. Animation tick**: Compares tick counter (`ent_anim_frame & $7F`) against `tick_speed` (sequence byte 1). On match, tick resets to 0 (preserving bit 7) and frame index advances. When frame index reaches `frame_count`, it wraps to 0 — all animations loop. There is no "play once" mode; the only way to end an animation is a frame with sprite definition ID `$00`, which deactivates the entity.

**4. Damage flash check**: If `ent_anim_frame` bit 7 is set, the entity blinks: drawn for 4 frames, invisible for 4 frames (`frame_counter AND #$04`). The animation continues ticking underneath — only the OAM write is suppressed. This is pure visibility toggling, not a palette change.

**5. Sprite definition lookup** (`write_entity_oam`): Current frame index selects a sprite definition ID from the sequence. The definition pointer is resolved from tables at `$8100`/`$8200` within the banked window.

**6. Position offset resolution**: Sprite definition byte 0 bit 7 selects the offset PRG bank (`$19` default, `$14` alternate). Byte 1 (position offset table index) plus `$11` (0 or 1 for normal/flipped) indexes into the `$BE00`/`$BF00` pointer tables. The offset pointer is pre-decremented by 2 so that the Y index tracking tile/attribute pairs in the definition simultaneously indexes the correct Y/X offset pair.

**7. OAM assembly loop**: For each hardware sprite in the definition, reads tile ID and attribute from the definition, Y and X offsets from the offset table, computes final positions relative to `$12`/`$13`, applies H-flip EOR to the attribute, and writes 4 bytes to the OAM buffer. Sprites that overflow in Y or X are hidden at Y=`$F8`.

### Animation Sequence Format

Pointer tables at `$8000-$807F` (low) and `$8080-$80FF` (high) in banks `$1A`/`$1B`/`$15`. 128 entries per bank.

```
Byte 0: frame_count (number of frames minus 1; $00 = single static frame)
Byte 1: tick_speed  (game ticks per frame before advancing)
Byte 2+: sprite_def_IDs[frame_count+1]  (one per frame; $00 = deactivate entity)
```

### Sprite Definition Format

Pointer tables at `$8100-$81FF` (low) and `$8200-$82FF` (high). 256 entries per bank.

```
Byte 0: sprite_count | bank_flag
          Bits 0-6 = hardware sprite count (0-based loop counter, so $09 = 10 sprites)
          Bit 7    = offset bank select: 0 = bank $19 (default), 1 = bank $14 (alternate)
Byte 1: position_offset_table_index
          Indexes into $BE00/$BF00 pointer tables in the selected offset bank
Byte 2+: (tile_id, attribute) pairs, one per hardware sprite
          tile_id  = CHR tile index in the sprite pattern table
          attribute = bits 0-1: palette, bit 5: behind-BG priority, bit 6: H-flip, bit 7: V-flip
```

### Position Offset Format

Banks `$19` and `$14` share identical layout: offset data at `$A000-$BDFF`, pointer table low bytes at `$BE00-$BEFF`, high bytes at `$BF00-$BFFF`.

Each record is a sequence of signed byte pairs (Y offset, X offset), one pair per hardware sprite. Normal and H-flipped versions are stored as consecutive pointer table entries — the engine adds 0 or 1 to the index to select between them. Pre-computed flipped offsets avoid runtime negation.

### H-Flip Mechanism

Horizontal flipping uses two complementary mechanisms:

1. **Pre-mirrored position offsets**: The offset table index points to a pair of records (normal at index N, flipped at N+1). `ent_flags` bit 6 adds 0 or 1 to select the correct record. The flipped record has mirrored X offsets.

2. **Attribute EOR**: Each tile's OAM attribute is XORed with `$40` (the NES H-flip bit) when `ent_flags` bit 6 is set. This flips individual tiles while the mirrored offsets reposition them correctly.

### Energy Bars

`draw_energy_bars` renders up to 3 HUD energy meters (player HP, weapon ammo, boss HP). Each bar is 7 sprites tall (Y from `$48` down to `$10`, 8 pixels apart). Fill level selects from 5 tiles: `$6B` (empty), `$6A` (1/4), `$69` (2/4), `$68` (3/4), `$67` (full). Drawing direction alternates each frame for OAM priority fairness.

### Cross-Bank Data Flow

```
FIXED BANK ($C000)              BANKED $8000              BANKED $A000
sprites.asm                     ($1A/$1B/$15)             ($19/$14)
────────────────                ─────────────             ─────────────
update_entity_sprites
  ├─ screen cull → $12/$13
  ├─ bank-switch $8000 ────────→ anim seq ptr tables
  │   animation tick   ────────→ seq data ($8300+)
  │   sprite def ptr   ────────→ def ptr tables ($8100/$8200)
  │   read definition  ────────→ def data ($854E+)
  │
  └─ bank-switch $A000 ──────────────────────────→ offset ptr ($BE00/$BF00)
      OAM assembly loop ─────────────────────────→ Y/X offsets ($A000+)
      write $0200-$02FF
```

---

## Bank Switching

**Source**: `src/fixed/system.asm`

### PRG Bank Switching

`select_PRG_banks` writes both swappable windows in one call:

1. MMC3 command `$06`: map `mmc3_select` ($F4) to `$8000-$9FFF`.
2. MMC3 command `$07`: map `prg_bank` ($F5) to `$A000-$BFFF`.
3. Mirror values to `$F2/$F3` (shadow copies).

A race-condition flag (`$F6`) is set during bank switching and cleared after. The sound driver checks this flag — if set, it knows the mainline code was interrupted mid-swap and defers its own bank switch.

### CHR Bank Switching

Six CHR bank shadow registers at `$E8-$ED` map to MMC3 registers R0-R5. With CHR A12 inversion off and BG at PPU `$0000` / sprites at PPU `$1000`:

| Register | MMC3 Reg | PPU Range | Size | Purpose |
|----------|----------|-----------|------|---------|
| `$E8` | R0 | `$0000-$07FF` | 2 KB | BG tiles (left half) |
| `$E9` | R1 | `$0800-$0FFF` | 2 KB | BG tiles (right half) |
| `$EA` | R2 | `$1000-$13FF` | 1 KB | Sprite tiles `$00-$3F` (Mega Man body) |
| `$EB` | R3 | `$1400-$17FF` | 1 KB | Sprite tiles `$40-$7F` (Mega Man weapons) |
| `$EC` | R4 | `$1800-$1BFF` | 1 KB | Sprite tiles `$80-$BF` (per-room enemies) |
| `$ED` | R5 | `$1C00-$1FFF` | 1 KB | Sprite tiles `$C0-$FF` (per-room enemies) |

Game code writes to shadow registers, then calls `update_CHR_banks` which sets the dirty flag (`$1B = $FF`). The NMI handler calls `select_CHR_banks` — if `$1B` is nonzero, it clears the flag and writes all 6 values to MMC3 hardware via the R0-R5 bank select/data register pair.

**Stage initialization** (`load_stage`): Sets `$E8`/`$E9` from stage data at `$AA80`/`$AA81` (per-stage BG tileset). Resets `$EA`/`$EB` to pages 0/1 (shared Mega Man sprites). `$EC`/`$ED` are set per-room.

**Per-room switching**: Each room's CHR/palette param (from `$AA60`) indexes bank `$01`'s table at `$A200`, which provides the `$EC`/`$ED` values for that room's enemy sprites. This allows different enemies to appear in different rooms within the same stage.

**Mid-frame CHR swaps**: Two IRQ handlers (`irq_chr_split_swap` mode `$10`, `irq_chr_swap_only` mode `$11`) write to `$E8`/`$E9` and immediately apply via `select_CHR_banks` mid-scanline, then either restore originals or set up values for NMI to restore next frame. This enables split-screen effects with different BG tilesets above and below the split line.

---

## Sound Driver

**Source**: `src/bank16_sound_driver.asm` (code), `src/bank17_sound_data.asm` (data)

The sound driver occupies banks `$16` (code at `$8000`) and `$17` (data at `$A000`), called every frame via the NMI stack-patching trampoline. It runs 8 simultaneous channels — 4 music and 4 SFX — mapped to the 4 NES APU voices (Pulse 1, Pulse 2, Triangle, Noise). The driver uses two completely separate command languages for the global music stream vs per-channel sequences, has a full ADSR envelope system with 8-byte instrument definitions, and implements both pitch vibrato and volume tremolo — an unusual combination for an NES driver.

### Sound Submission

To play a sound, game code calls `submit_sound_ID` which writes to a circular buffer:

- 8 slots at `$DC-$E3`
- Write index at `$DA`, read index at `$DB`
- Sentinel value `$88` = empty slot
- If the write slot is occupied, the sound is dropped (buffer full)

### Sound Processing (per NMI)

`play_sounds` in the fixed bank:
1. Check race flag (`$F6`) — if mainline is mid-bank-switch, skip entirely.
2. Switch to banks `$16/$17`.
3. Drain circular buffer: read from `$DC + $DB`, compare to `$88` sentinel. For each non-sentinel entry, call the sound effect handler, clear the slot, advance the circular index (AND `#$07` for 8-slot wrap).
4. Call `sound_driver_tick` (main driver update).
5. Restore original PRG banks.

### Driver Tick (per frame)

`sound_driver_update` runs once per frame:

1. Check bit 0 of `snd_flags` (`$C0`). If set, driver is paused — exit immediately.
2. If the global data pointer (`$D0/$D1`) is non-null, parse the global music stream for channel enable/duration/tempo/transpose commands.
3. Update the fractional tempo accumulator: add `snd_tempo_hi` (`$CA`) to `snd_tempo_accum` (`$C8`); carry propagates into `snd_tempo_ticks` (`$C7`). This produces the number of logical ticks elapsed this frame.
4. Loop X from 3 down to 0 (4 channels). For each: if the music channel is active (per `snd_channel_mask`), call `process_music_channel`. Then, unless SFX are muted (bit 1 of `snd_flags`), call `process_sfx_channel`.
5. If `snd_fade_rate` (`$CC`) is nonzero, advance the global volume fade.

### Tempo System

The driver uses an **8.8 fixed-point tempo accumulator**. Each frame, `snd_tempo_hi` (fractional part) is added to an accumulator, and the carry feeds into `snd_tempo_ticks` (integer ticks). The default tempo `$01.$99` produces approximately 1.6 ticks per frame — sometimes 1 tick, sometimes 2 — but the long-term average is precise. This avoids the tempo quantization problem that simpler NES drivers have, where only integer tick rates are possible.

Note duration countdown subtracts `snd_tempo_ticks` each frame, so both music and SFX are tied to the same tempo clock.

### Dual Command Languages

The driver has two completely separate command encodings:

**Global music stream** (`parse_music_data`): Reads bitfield-encoded command bytes where each bit indicates a parameter follows:
- Bit 7: end-of-data (optionally chains to another sound via `$D7`)
- Bit 0: pointer redirect (reads a new 16-bit address)
- Bit 1: duration multiplier
- Bit 2: transpose offset
- After flag processing: note duration (multiplied by the duration multiplier) + 4-bit channel enable mask

**Per-channel SFX sequences** (`sfx_dispatch_command`): Bytes `$00-$18` are commands dispatched through a 25-entry jump table; bytes `$20+` are packed note+duration values. Key commands:

| Range | Function |
|-------|----------|
| `$04` | Set tempo (2-byte arg: lo + hi for the fractional accumulator) |
| `$05` | Set duration multiplier |
| `$06` | Set octave (low nibble of control flags) |
| `$07` | Set global pitch offset (`snd_pitch_offset` / `$CB`) |
| `$08` | Set per-channel detune |
| `$09-$0C` | Loop group start (4 independent nested counters) |
| `$12-$15` | Loop group end (decrement + conditional jump) |
| `$16` | Unconditional jump (16-bit address) |
| `$17` | End channel (silence + clear pointer) |
| `$18` | Set duty cycle bits |

The **4 nested loop groups** per channel allow complex repeated musical structures (e.g., "repeat phrase A 4 times, with phrase B repeated 3 times inside each A") without flattening the data.

### Packed Note+Duration Encoding

SFX note bytes encode both pitch and duration in a single byte. The top 3 bits select a duration scale (powers of 2: `$02, $04, $08, $10, $20, $40, $80`), and the bottom 5 bits encode the note index within the current octave. A dotted-note flag (set by command `$02`) multiplies the duration by 1.5. An alternate duration table provides the x1.5 variants directly: `$03, $06, $0C, $18, $30, $60, $C0`.

### Instrument Definitions (8 bytes)

Each instrument is an 8-byte record. The instrument pointer is computed as `base + (index - 1) * 8` from the sound data region.

| Byte | Purpose |
|------|---------|
| 0 | Attack rate (index into 32-entry non-linear volume curve) |
| 1 | Decay rate (same curve) |
| 2 | Sustain level |
| 3 | Release rate (same curve; 0 = hold indefinitely) |
| 4 | Vibrato speed (bit 7 = force phase reset on note start) |
| 5 | Vibrato depth (pitch) |
| 6 | Tremolo depth (volume) |
| 7 | Triangle linear counter value |

### ADSR Envelope

The volume envelope is a **5-phase state machine** per channel: attack, decay, sustain, release, and hold. Internal volume ranges from `$00` to `$F0`. Attack/decay/release rates are indices into a 32-entry non-linear curve table where gaps widen at higher values (`$00, $01, $02, ... $0C, $0E, $0F, $10, $12, ... $28, $30, $3C, $50, $7E, $7F, $FE, $FF`), giving exponential-feeling ramps. Rate 0 = no change, rate 31 = instant.

The final volume output path: internal envelope → compare with fade level (take minimum) → right-shift 4 to get 4-bit APU volume → apply tremolo attenuation → write to APU register.

### Pitch System

A 96-entry frequency period table covers the full chromatic range. Note indices are clamped to 0-95. Three levels of transpose stack:
- Global transpose (`$D2`, set by music stream)
- Master pitch offset (`snd_pitch_offset` / `$CB`, set by command `$07`)
- Per-channel detune (`$0734,x`, set by command `$08`)

**Vibrato**: A phase accumulator adds the instrument's vibrato speed each frame. On overflow, the direction bit toggles. Vibrato depth is multiplied by phase, shifted right 4, and added/subtracted from the base frequency.

**Portamento**: The slide rate (`$0718,x`) is added to the current frequency each frame. When the frequency reaches the target note, it clamps to the exact target value and stops.

### SFX Priority

Sound ID type byte determines behavior:
- `$00` = music track (initializes 4 channel pointers from the track header)
- `$01-$7F` = SFX priority level (higher overrides lower; rejected if current priority is higher)
- Bit 7 = **chain flag**: when the current SFX ends, automatically start the sound stored at `$D7`

SFX and music run in parallel on the same APU channels. When an SFX note triggers, it invalidates the frequency cache for the corresponding music channel (`$077C,y = $FF`), forcing a full register rewrite when music resumes.

### Transparent Cross-Bank Reads

Sound data spans three banks, unified by `read_ptr`:

- `$8000-$9FFF`: bank `$16` (driver code — also contains track data for sound IDs `$00-$04`)
- `$A000-$BFFF`: bank `$17` (track data for sound IDs `$05-$11`)
- `$C000+` (logical): bank `$18` (sound ID `$12` + all SFX/jingles)

When `read_ptr` receives an address >= `$C000`, it subtracts `$20` from the high byte (remapping `$C0xx` to `$A0xx`), temporarily swaps bank `$18` into the `$A000` window via MMC3, reads one byte, restores bank `$17`, and adds `$20` back. The caller never knows a bank switch occurred.

### MMC3 Register Overlay

The first 3 bytes of bank `$16` (`$8000-$8002`) serve a dual purpose. As code, they form a `JMP` instruction (opcode `$4C`) used as the driver's entry point. As addresses, `$8000` and `$8001` are the MMC3 bank select and bank data registers. The driver writes bank numbers directly to these code bytes for bank switching — a self-referential trick that avoids needing separate register address constants.

### NMI Envelope Timers

4 envelope timer channels at `$80-$8F` (shared with scheduler task slots):
- State `$01` = counting down (NMI decrements timer each frame)
- State `$04` = release (timer expired)

---

## IRQ Scanline Effects

**Source**: `src/fixed/irq.asm`

The MMC3 scanline counter fires an IRQ at a programmable scanline. The NMI programs the counter and handler address each frame based on `screen_mode` ($78).

### IRQ Handler Table

| Index | Handler | Effect |
|-------|---------|--------|
| `$00` | `irq_exit_disable` | No splits |
| `$01` | `irq_gameplay_status_bar` | HUD / gameplay scroll split |
| `$02` | `irq_gameplay_hscroll` | Horizontal scroll after HUD |
| `$03` | `irq_gameplay_ntswap` | Nametable swap after HUD |
| `$04` | `irq_gameplay_vscroll` | Vertical scroll after HUD |
| `$05-$06` | `irq_stagesel_*` | Stage select grid rendering |
| `$07-$08` | `irq_transition_*` | 3-strip stage transition wipe |
| `$09-$0A` | `irq_wave_*` | Water wave distortion (Gemini Man) |
| `$0B-$0C` | `irq_title_*` | Title screen / password splits |
| `$0D-$0E` | `irq_cutscene_*` | Cutscene scroll |
| `$0F-$10` | `irq_chr_split_*` | Mid-frame CHR bank swap |
| `$11` | `irq_chr_swap_only` | CHR swap only |

### Chained IRQs

Many handlers program the next scanline counter and chain to a different handler before exiting. This allows multi-split frames — for example, the stage transition uses 3 strips: NMI sets the top strip, the first IRQ at scanline 88 sets the middle, and the second IRQ at scanline 152 sets the bottom.

### Water Wave Effect

Gemini Man's stage uses IRQ modes `$09/$0A` to create water wave distortion. Three horizontal strips with alternating X scroll direction and 14-scanline gaps between strips produce the ripple effect.

---

## Stage Data Format

**Source**: `src/fixed/camera.asm` (metatile rendering, room transitions), `src/fixed/ppu_utils.asm` (`load_room`), `src/fixed/collision.asm` (tile collision)

Each stage occupies one 8 KB bank at `$A000-$BFFF`. The stage bank index equals the stage ID for Robot Master stages (`$00-$07`).

### Bank Layout

| Address | Contents |
|---------|----------|
| `$A000-$A5FF` | Global enemy property tables (bank `$00` only, shared across all stages) |
| `$A600-$A7FF` | Enemy velocity lookup tables (bank `$00` only) |
| `$A800-$A9FF` | Boss AI local data / stage-specific code |
| `$AA00-$AA3F` | Screen column ID table (1 byte per screen page) |
| `$AA40-$AA5F` | Room config table (1 byte per room) |
| `$AA60-$AA7F` | Room pointer table (2 bytes per room: CHR/palette param + layout index) |
| `$AA80-$AA81` | BG CHR bank indices for `$E8`/`$E9` |
| `$AA82-$AAFF` | Screen layout data (20 bytes per entry) |
| `$AB00-$ABFF` | Enemy spawn: screen numbers (`$FF` terminated) |
| `$AC00-$ACFF` | Enemy spawn: X positions |
| `$AD00-$ADFF` | Enemy spawn: Y positions |
| `$AE00-$AEFF` | Enemy spawn: global enemy IDs |
| `$AF00-$B6FF` | Metatile column definitions (64 bytes per column) |
| `$B700-$BAFF` | Metatile sub-tile indices (4 bytes per metatile) |
| `$BB00-$BEFF` | CHR tile lookup planes (4 × 256-byte tables) |
| `$BF00-$BFFF` | Collision/attribute table (1 byte per sub-tile index) |

### Room Table

Each room has a config byte at `$AA40 + room_index`:

```
Bit 7:     Vertical connection up
Bit 6:     Vertical connection down
Bit 5:     Horizontal scroll enabled
Bits 4-0:  Screen count (0 = single screen, N = N+1 screens)
```

Each room also has 2 bytes at `$AA60 + room_index * 2`:
- **Byte 0**: CHR/palette param — indexes bank `$01` tables at `$A200` (sprite CHR) and `$A030` (sprite palettes)
- **Byte 1**: Layout index — multiplied by 20 to offset into screen layout data at `$AA82`

Each layout entry is 20 bytes: 16 metatile column IDs (one per 32-pixel column of the screen) + 4 screen connection bytes (up/down/left/right, where bit 7 = scroll vs warp and bits 0-6 = target screen).

### Metatile Rendering Pipeline

The metatile system has **three levels of indirection** from screen to CHR tiles:

**Level 1 — Screen to column IDs**: `$AA00,y` (where Y = screen page) stores a metatile column ID. Different screens can share the same column ID to create repeated patterns.

**Level 2 — Column to metatile indices**: `$AF00 + (column_ID × 64)` gives a 64-byte block: an 8×8 grid of metatile indices (8 columns × 8 rows of 32×32 pixel metatiles). Grid position encoded as `$28 = (row × 8) + column`.

**Level 3 — Metatile to sub-tile indices**: `$B700 + (metatile_index × 4)` gives 4 sub-tile indices (top-left, top-right, bottom-left, bottom-right quadrants of the 32×32 metatile).

**Level 4 — Sub-tile to CHR tiles**: Each sub-tile index selects from four parallel 256-byte lookup tables that produce 4 CHR tile IDs (a 2×2 grid of 8×8 pixel tiles):

```
$BB00,y = top-left CHR tile
$BC00,y = top-right CHR tile
$BD00,y = bottom-left CHR tile
$BE00,y = bottom-right CHR tile
```

The result is a 4×4 grid of CHR tile IDs (16 total) stored in the `$06C0` buffer, representing one complete 32×32 pixel metatile.

```
Screen page ($F9)
    │
    ▼
$AA00,y ──→ column_ID ──→ $AF00 + (ID × 64) ──→ metatile_index
    │
    ▼
$B700 + (metatile × 4) ──→ 4 sub-tile indices
    │
    ▼
$BB00/$BC00/$BD00/$BE00 ──→ 16 CHR tile IDs ──→ $06C0 buffer ──→ PPU nametable
```

### Collision and Attributes

`$BF00` is a 256-byte table indexed by **sub-tile index** (the values from Level 3). Each byte serves dual purpose:

- **Upper nibble** (bits 7-4): collision type

| Value | Constant | Meaning |
|-------|----------|---------|
| `$00` | `TILE_AIR` | Passthrough |
| `$10` | `TILE_SOLID` | Solid ground |
| `$20` | `TILE_LADDER` | Climbable |
| `$30` | `TILE_DAMAGE` | Damage (lava/fire) |
| `$40` | `TILE_LADDER_TOP` | Ladder grab point |
| `$50` | `TILE_SPIKES` | Instant kill |
| `$70` | `TILE_DISAPPEAR` | Breakable block |

- **Lower 2 bits**: palette index for NES attribute table generation

During `metatile_to_chr_tiles`, the palette bits from all 4 quadrants are accumulated into a single NES attribute byte (2 bits per quadrant), then merged into the attribute cache at `$0640` using left/right or top/bottom masks depending on scroll direction.

### Scroll Column Rendering

When camera scrolling crosses an 8-pixel tile boundary, the engine renders a new metatile column:

1. Advance nametable column pointer `$24` (wraps 0-31 across the NES's 32-column nametable)
2. When `$24` wraps past 0 or 31, advance metatile column base `$29`
3. Iterate 8 metatile rows, calling `metatile_to_chr_tiles` for each
4. Build 30 tile IDs + attribute updates into the `$0780` PPU buffer
5. NMI transfers the buffer to VRAM during VBlank

### Room Transitions

**Horizontal**: Triggered when player X >= `$E5` on a scrolling room with the next room also scrolling-enabled. `fast_scroll_right` scrolls at 4 pixels/frame, rendering columns as they come into view, until `camera_x_lo` wraps. Then `load_room` initializes the new room's column IDs and CHR/palette.

**Vertical**: Triggered when player falls off bottom (Y >= `$E8`) or climbs off top (Y < `$09` on ladder). The engine scans `$AA40` entries for matching vertical connection bits, toggles NES mirroring (V-mirror during vertical transitions, H-mirror during horizontal), and scrolls `$FA` (fine Y) by 3 pixels/frame while rendering PPU row updates (32 tiles + 8 attribute bytes per row).

### Stage-to-Bank Mapping

| Stage | Bank | Notes |
|-------|------|-------|
| `$00-$07` | `$00-$07` | 1:1 mapping for Robot Master stages |
| `$08-$0F` | varies | Doc Robot remixes and Wily stages |
| Shared bank `$0D` | `$0D` | Doc Shadow (R), Doc Snake (R), and Wily 1 |

The mapping table `ensure_stage_bank_table` in the fixed bank handles all stage-to-bank translations.

---

## Debug Features

Four debug features are present in the retail ROM, activated by player 2 controller input:

| P2 Input | Effect |
|----------|--------|
| **Right** | Super jump (`$08.00` vs normal `$04.E5`) + immunity to pit death |
| **Up** | Slow-motion (animations tick every 8th frame) |
| **A** | Freeze all entity animation |
| **Left** | Latches permanent rightward movement flag |

P2 Left works as a latch because the NES d-pad physically cannot report Left+Right simultaneously — once latched, the Right flag stays set permanently.

---

## License

This document describes the engine of Mega Man 3, which is copyrighted by Capcom. This is a reverse-engineering reference for educational purposes. See [LICENSE](LICENSE) for project license details.
