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

`$0600-$061F` holds the 32-byte working palette (4 background + 4 sprite palettes). Gameplay code modifies this buffer and sets `palette_dirty`; the NMI uploads it to the PPU.

---

## Sprite Animation Engine

**Source**: `src/fixed/sprites.asm`

### OAM Build Loop

The sprite engine iterates all 32 entity slots each frame. To distribute OAM priority fairly (the NES draws lower-numbered sprites on top), it **alternates iteration direction**: forward on even frames, backward on odd frames.

For each active entity:
1. Check visibility (off-screen culling, invisible flag).
2. Look up animation sequence from `ent_anim_id` (bit 7 selects bank `$1A` vs `$1B`).
3. Advance animation timer. On timer expiry, advance to next frame. If frame ID = 0, deactivate entity.
4. Read sprite definition: sprite count, position offset table index, CHR tile + attribute pairs.
5. Apply facing (H-flip), damage flash (palette swap on `ent_anim_frame` bit 7).
6. Write OAM entries to the `$0200` buffer.

### Animation Sequence Format

Pointed to by the animation ID lookup tables in banks `$1A`/`$1B`:

```
Byte 0: total frames in sequence
Byte 1: ticks per frame (duration)
Byte 2+: sprite definition IDs per frame (0 = deactivate entity)
```

### Sprite Definition Format

```
Byte 0: sprite count (bit 7: use CHR bank $14 instead of $19)
Byte 1: position offset table index
Byte 2+: pairs of [CHR tile, OAM attribute]
```

### Energy Bars

`draw_energy_bars` renders up to 3 energy bar displays (player HP, boss HP, weapon ammo). Each bar is 7 sprites tall, 4 energy units per segment. Bar fill tiles: empty = `$6B`, 1/4 = `$6A`, 2/4 = `$69`, 3/4 = `$68`, full = `$67`.

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

Six CHR bank shadow registers at `$E8-$ED`:

| Register | MMC3 Reg | Window | Size |
|----------|----------|--------|------|
| `$E8` | R0 | BG tiles 1 | 2 KB |
| `$E9` | R1 | BG tiles 2 | 2 KB |
| `$EA` | R2 | Sprite tiles 1 | 1 KB |
| `$EB` | R3 | Sprite tiles 2 | 1 KB |
| `$EC` | R4 | Sprite tiles 3 | 1 KB |
| `$ED` | R5 | Sprite tiles 4 | 1 KB |

Mainline code writes to the shadow registers and sets a dirty flag (`$1B = $FF`). The NMI handler calls `select_CHR_banks` to write all 6 values to MMC3.

---

## Sound Driver

**Source**: `src/bank16_sound_driver.asm` (code), `src/bank17_sound_data.asm` (data)

The sound driver runs in banks `$16`/`$17`, called every frame via the NMI stack-patching trampoline.

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

### Envelope System

4 sound envelope channels at `$80-$8F` (shared with scheduler task slots):
- State `$01` = counting down (NMI decrements timer each frame)
- State `$04` = release (timer expired)

### Cross-Track Data Sharing

The music data in bank `$17` is not 13 isolated tracks. Track headers are entry points into a shared pool of channel sequences. The driver's loop and jump commands use absolute addresses, so one track's channels can reference data inside another track's address range. Snake Man's melody loops into Top Man's data. Wily 3-4 is a 53-byte remix header pointing 3 of its 4 channels into Wily 5-6's data.

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

Each stage occupies one 8 KB bank at `$A000-$BFFF`. The stage bank index equals the stage ID for Robot Master stages (`$00-$07`).

### Bank Layout

| Address | Contents |
|---------|----------|
| `$A000-$A5FF` | Global enemy property tables (bank `$00` only, shared across all stages) |
| `$A600-$A7FF` | Enemy velocity lookup tables (bank `$00` only) |
| `$A800-$A9FF` | Boss AI local data / stage-specific code |
| `$AA00-$AA3F` | Screen metatile column grid + room pointer table |
| `$AA40+` | Room table: 1 byte per room |
| `$AB00-$ABFF` | Enemy spawn: screen numbers (`$FF` terminated) |
| `$AC00-$ACFF` | Enemy spawn: X positions |
| `$AD00-$ADFF` | Enemy spawn: Y positions |
| `$AE00-$AEFF` | Enemy spawn: global enemy IDs |
| `$AF00-$B6FF` | Metatile column definitions (64 bytes per column) |
| `$B700-$BAFF` | Metatile CHR tile definitions (4 bytes per metatile: 2x2 pattern tiles) |
| `$BB00-$BEFF` | Metatile attribute data (palette assignments) |
| `$BF00-$BFFF` | Collision attribute table |

### Room Table Format

Each room is 1 byte at `$AA40 + room_index`:
- Bits 7-6: vertical connection type (00=none, 01=up, 10=down, 11=both)
- Bit 5: horizontal scroll enabled
- Bits 4-0: screen count

### Metatile System

Each screen is 16 columns wide (256 pixels). Each column is 15 metatiles tall (240 pixels). Each metatile is 16x16 pixels, composed of four 8x8 CHR tiles in a 2x2 grid. The collision attribute table at `$BF00` maps each metatile to a collision type (upper nibble).

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
