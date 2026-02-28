# Mega Man 3 — Data Reference

ROM hacking reference for Mega Man 3 (U). Every address in this document is a CPU address within its mapped bank window. For engine logic and code flow, see [ENGINE.md](ENGINE.md).

---

## 1. Quick Reference

| Want to change... | Location | Notes |
|-------------------|----------|-------|
| Weapon damage vs enemies | Bank $0A, $A000-$A9FF | 10 tables x 256 bytes, indexed by `ent_routine` |
| Enemy HP | Bank $00, $A400 | 256 bytes indexed by global enemy ID; $FF = invincible |
| Player starting HP | Fixed bank `game_loop.asm:135` | `lda #HEALTH_FULL` → `sta player_hp` |
| Player starting lives | Fixed bank `game_loop.asm:30` | `lda #$02` → `sta lives` (displays as 3) |
| Boss HP constant | `include/constants.inc` | `HEALTH_FULL = $9C` (28 bars x 4 sub-units = 156) |
| Stage-to-bank mapping | Fixed bank `ppu_utils.asm:723` | `ensure_stage_bank_table` (23 entries) |
| Stage music | Fixed bank `game_loop.asm:615` | `frame_loop_stage_music_table` (18 entries) |
| Enemy spawn positions | Per-stage bank, $AB00-$AEFF | 4 parallel arrays: screen/X/Y/enemy ID |
| Walk speed | Fixed bank `player_states.asm` | `$01.4C` (1.30 px/frame) |
| Gravity | ZP $99 | `$55` gameplay, `$40` stage select |
| Sprite palettes | Bank $01, $A030 | 8 bytes/entry (SP2+SP3), indexed by room param |
| BG CHR banks | Per-stage bank, $AA80 | 2 bytes → shadow registers $E8/$E9 |
| Music track data | Bank $17, $A000+ | Channel sequences; pointers at bank $16 $8A44 |

---

## 2. Weapon Damage Tables

**Source**: `src/bank0A_damage_tables.asm`
**Bank**: $0A (mapped at $A000-$BFFF)
**Range**: $A000-$A9FF (10 tables x 256 bytes)

Each table is 256 bytes indexed by the target's `ent_routine` value. The value at that index is the damage dealt. A value of $00 means immune.

| Address | Table | Weapon ID |
|---------|-------|-----------|
| $A000 | Contact damage | N/A (base collision) |
| $A100 | Mega Buster | $00 |
| $A200 | Needle Cannon | $02 |
| $A300 | Magnet Missile | $04 |
| $A400 | Gemini Laser | $01 |
| $A500 | Hard Knuckle | $03 |
| $A600 | Top Spin | $05 |
| $A700 | Search Snake | $06 |
| $A800 | Spark Shock | $08 |
| $A900 | Shadow Blade | $0A |

### Special Values

- **$58** in Spark Shock table ($A800) = **freeze effect** (stuns enemy, no HP damage). Used for regular enemies; bosses use normal numeric damage instead.

### Key Boss Routine Indices

These `ent_routine` values index into each damage table:

**Robot Masters:**

| Boss | `ent_routine` | Weakness (damage) |
|------|---------------|-------------------|
| Needle Man | $C0 | Gemini Laser (7) |
| Magnet Man | $C1 | Spark Shock (7) |
| Top Man | $C2 | Hard Knuckle (7) |
| Shadow Man | $C3 | Top Spin (7) |
| Hard Man | $D0 | Magnet Missile (4) |
| Spark Man | $D2 | Shadow Blade (4) |
| Snake Man | $D4 | Needle Cannon (4) |
| Gemini Man | $D6 | Search Snake (5) |

**Doc Robots:**

| Boss | `ent_routine` | Weakness (damage) |
|------|---------------|-------------------|
| Doc Flash | $A0 | Shadow Blade (2) |
| Doc Wood | $A1 | Search Snake (4) |
| Doc Crash | $A2 | Hard Knuckle (7) |
| Doc Metal | $A3 | Magnet Missile (4) |
| Doc Bubble | $B0 | Spark Shock (4) |
| Doc Heat | $B1 | Top Spin (7) |
| Doc Quick | $B2 | Gemini Laser (4), Search Snake (4) |
| Doc Air | $B3 | Spark Shock (4) |

**Fortress Bosses:**

| Boss | `ent_routine` |
|------|---------------|
| Yellow Devil | $E0 |
| Kamegoro Maker | $F0 |
| Wily Machine A | $E3 |
| Wily Machine B | $E5 |
| Wily Machine C | $ED |
| Gamma (body) | $E8 |
| Gamma (head) | $E9 |

### Loading Mechanism

The fixed bank reads damage via hardcoded bank switch: `LDA #$0A / STA prg_bank`, then `LDA $Axxx,y` where `x` selects the weapon table and `y` is `ent_routine`. Rush Coil/Marine/Jet share the Buster table.

---

## 3. Enemy Data

### Global Property Tables

**Source**: `src/bank00_enemy_data.asm`
**Bank**: $00 (mapped at $A000-$BFFF)
**Range**: $A000-$A5FF (6 tables x 256 bytes)

All tables are indexed by **global enemy ID** ($00-$8F valid). The engine switches to bank $00 during entity spawning to read these.

| Address | Label | Contents | On spawn → |
|---------|-------|----------|------------|
| $A000 | `enemy_flags_g` | Init flags | → `ent_flags` |
| $A100 | `enemy_main_ID_g` | AI routine index | → `ent_routine` |
| $A200 | `enemy_shape_g` | Hitbox shape (bit 7 = contact damage) | → `ent_hitbox` |
| $A300 | `enemy_OAM_ID_g` | Animation/OAM ID | → `ent_anim_id` |
| $A400 | `enemy_health_g` | Starting HP ($FF = invincible) | → `ent_hp` |
| $A500 | `enemy_speed_ID_g` | Velocity table index | → `ent_xvel`/`ent_xvel_sub` |

### Entity Flags Bitmask ($A000)

| Bit | Mask | Meaning |
|-----|------|---------|
| 7 | $80 | Entity active / drawn |
| 6 | $40 | Horizontal flip |
| 5 | $20 | Behind-background OAM priority |
| 4 | $10 | Uses world coordinates (subtract camera for screen pos) |
| 3 | $08 | Wide offscreen margin (keep alive within 72px) |
| 2 | $04 | Invisible (skip OAM rendering) |

### Velocity Lookup Tables ($A600-$A7FF)

Two parallel tables indexed by speed ID from `enemy_speed_ID_g`:

| Speed ID | Whole ($A700) | Sub ($A600) | Pixels/frame |
|----------|---------------|-------------|--------------|
| $00 | 0 | $00 | 0.00 |
| $01 | 1 | $00 | 1.00 |
| $02 | 0 | $66 | 0.40 |
| $03 | 1 | $80 | 1.50 |
| $04 | 2 | $00 | 2.00 |
| $05 | 4 | $00 | 4.00 |
| $06 | 2 | $19 | 2.10 |
| $07 | 1 | $33 | 1.20 |
| $08 | 0 | $80 | 0.50 |
| $09 | 3 | $00 | 3.00 |
| $0A | 2 | $80 | 2.50 |
| $0B | 1 | $4C | 1.30 |
| $0C | 1 | $B3 | 1.70 |
| $0D | 0 | $CC | 0.80 |

### Per-Stage Spawn Tables

**Location**: $AB00-$AEFF in each stage bank
**Format**: 4 parallel arrays, terminated by $FF in the screen array

| Offset | Contents |
|--------|----------|
| $AB00,y | Screen number ($FF = end of list) |
| $AC00,y | X pixel position (0-255) |
| $AD00,y | Y pixel position (0-239) |
| $AE00,y | Global enemy ID (indexes bank $00 tables) |

The engine iterates index `y` from 0 until it reads $FF from $AB00. Each entry spawns one enemy at the given screen/position using the global enemy ID to look up all properties from bank $00.

### Global Enemy ID Table

Complete mapping of global enemy IDs ($00-$8F) used in spawn tables ($AE00) and property tables ($A000-$A5FF). Source: `bank00_enemy_data.asm` header comments and inline annotations.

**Standard Enemies ($00-$3F):**

| ID | Enemy | HP | ID | Enemy | HP |
|----|-------|----|----|-------|-----|
| $00 | Dada | 1 | $20 | Bolton & Nutton | 1 |
| $01 | Potton | 1 | $21 | Wanaan | $FF |
| $02 | New Shotman | 3 | $22 | Needle Press | $FF |
| $03 | Hammer Joe | 8 | $23 | Needle Press (variant) | $FF |
| $04 | Bubukan | 4 | $24 | Elec'n | 1 |
| $05 | Jamacy | 1 | $25 | Magnet Pull | $FF |
| $06 | Bomb Flier | 3 | $26 | Mechakkero | 1 |
| $07 | (projectile) | 3 | $27 | Top Man Platform | $FF |
| $08 | Yambow | 3 | $28 | (no-op) | 1 |
| $09 | Metall DX | 1 | $29 | (no-op) | 1 |
| $0A | Cannon | 3 | $2A | Chibee | 1 |
| $0B | Cloud Platform | $FF | $2B | Block Breaker | 1 |
| $0C | Giant Metall Met | 1 | $2C | Penpen | 1 |
| $0D | Giant Metall Met | 1 | $2D | Electric Gabyoall | $FF |
| $0E | Gyoraibo | 2 | $2E | (no-op) | 1 |
| $0F | Mag Fly | 1 | $2F | Block Breaker | 6 |
| $10 | Block Breaker | 2 | $30 | Block Breaker | 1 |
| $11 | Junk Golem | 6 | $31 | (no-op) | $FF |
| $12 | Pickelman Bull | 3 | $32 | Pole | $FF |
| $13 | Bikky | 6 | $33 | Holograph | 28 |
| $14 | Giant Metall | $FF | $34 | Needle Press | $FF |
| $15 | Jamacy | 1 | $35 | (no-op) | $FF |
| $16 | Mag Force | $FF | $36 | Peterchy | 3 |
| $17 | Junk Block Thrown | 6 | $37 | Walking Bomb | 1 |
| $18 | Nitron | 1 | $38 | Parasyu | 3 |
| $19 | Pole | 1 | $39 | Hologran | 3 |
| $1A | Gyoraibo | 2 | $3A | Hologran | 3 |
| $1B | Hari Harry | 6 | $3B | Bomber Pepe | 6 |
| $1C | Penpen Maker | 10 | $3C | Metall DX (walk) | 1 |
| $1D | Returning Monking | 8 | $3D | Magnet Push | $FF |
| $1E | Block Breaker | 1 | $3E | Proto Man | 28 |
| $1F | Have 'Su' Bee | 3 | $3F | (no-op) | $FF |

HP = $FF means invincible (immune to all damage).

**Doc Robot Entries ($40-$4F):**

| ID | Entity | HP |
|----|--------|-----|
| $40-$46 | Doc Robot screen markers (no-op AI) | 3 |
| $47 | Needle Press (B) | 28 |
| $48 | Doc Robot (Flash Man) | 28 |
| $49 | Doc Robot (Bubble Man) | 28 |
| $4A | Doc Robot (Quick Man) | 28 |
| $4B | Doc Robot (Wood Man) | 28 |
| $4C | Doc Robot (Crash Man) | 28 |
| $4D | Doc Robot (Air Man) | 28 |
| $4E | Doc Robot (Metal Man) | 28 |
| $4F | (unused) | $FF |

**Robot Master Intros, Tama, Items ($50-$67):**

| ID | Entity | HP |
|----|--------|-----|
| $50-$56 | Robot Master intros (Needle through Spark) | $FF |
| $57 | Komasaburo (spinning top) | 10 |
| $58-$5B | Tama segments | $FF/0 |
| $5C | Giant Springer | 8 |
| $5D-$5E | Tama segments | 8/1 |
| $5F | (Tama-related) | 0 |
| $60-$61 | Item Pickup (small/large) | 2 |
| $62 | Komasaburo (variant) | 6 |
| $63 | Surprise Box | 8 |
| $64-$65 | Item Pickup | 1 |
| $66-$67 | (unused) | 0 |

**Robot Masters ($68-$6F):**

| ID | Boss | AI routine |
|----|------|-----------|
| $68 | Needle Man | $90 |
| $69 | Magnet Man | $91 |
| $6A | Gemini Man | $92 |
| $6B | Hard Man | $93 |
| $6C | Top Man | $94 |
| $6D | Snake Man | $95 |
| $6E | Spark Man | $96 |
| $6F | Shadow Man | $97 |

Robot Masters have $FF in the health table; actual HP (28) is set by their AI init routines.

**Boss Projectiles & Special ($70-$7F):**

| ID | Entity | HP |
|----|--------|-----|
| $70 | (boss projectile) | 0 |
| $71 | (boss projectile) | 10 |
| $72-$76 | (boss projectiles) | 0 |
| $77 | Gamma Fist | 0 |
| $78 | Proto Man (Gemini cutscene) | 0 |
| $79-$7D | (unused) | 0 |
| $7E-$7F | (fortress entity init) | 0 |

**Fortress Bosses ($80-$8F):**

| ID | Entity | HP | AI routine |
|----|--------|-----|-----------|
| $80 | Holograph / Kamegoro init | 0 | $EA |
| $81 | Yellow Devil | 28 | $E0 |
| $82 | Wily Machine | 28 | $E3 |
| $83 | Gamma | 28 | $E5 |
| $84 | Break Man | 0 | $ED |
| $85-$89 | (unused) | 0 | $00 |
| $8A | Kamegoro Maker | 0 | $EB |
| $8B-$8D | Kamegoro sub-entities | 0 | $EC |
| $8E-$8F | (unused) | 0 | $00 |

---

## 4. Player Values

### Zero-Page Addresses

| Address | Variable | Contents |
|---------|----------|----------|
| $A0 | `current_weapon` | Current weapon ID (WPN_BUSTER=$00 through WPN_RUSH_JET=$0B) |
| $A1 | `weapon_cursor` | Weapon menu cursor position |
| $A2 | `player_hp` | Player HP (HEALTH_FULL = $9C = 28 bars) |
| $A3-$AD | `weapon_ammo` | Per-weapon ammo (code indexes via `player_hp + weapon_id`, i.e. $A2 + wpn) |
| $AE | `lives` | Lives remaining (displays as value + 1) |
| $AF | `etanks` | E-Tank count |
| $30 | `player_state` | Player state machine index (22 states) |
| $31 | `player_facing` | 1=right, 2=left |
| $39 | `invincibility_timer` | I-frames countdown (nonzero = immune) |
| $99 | `gravity` | Gravity sub-pixel value ($55 gameplay, $40 stage select) |

### Initialization (game_loop.asm)

```
Line 30:  lda #$02 / sta lives          ; 2 extra lives (displays as 3)
Line 28:  lda #HEALTH_FULL / sta $A9    ; Rush Coil ammo = full
Line 135: lda #HEALTH_FULL / sta player_hp  ; full HP
```

### Physics Values

All velocities use 8.8 fixed-point format (whole.sub):

| Parameter | Value | Pixels/frame | Location |
|-----------|-------|--------------|----------|
| Walk speed | $01.4C | 1.30 | player_states.asm |
| Slide speed | $02.80 | 2.50 | player_states.asm |
| Normal jump | $04.E5 | 4.90 upward | player_ground.asm |
| Rush Coil bounce | $06.EE | 6.93 upward | player_ground.asm |
| Super jump (debug) | $08.00 | 8.00 upward | player_ground.asm |
| Gravity | $00.55 | 0.33/frame² | ZP $99 |
| Terminal velocity | $F9.00 | 7.00 downward | movement.asm |

### Weapon IDs

| ID | Constant | Weapon | Ammo at |
|----|----------|--------|---------|
| $00 | WPN_BUSTER | Mega Buster | $A2 (=player_hp, unlimited) |
| $01 | WPN_GEMINI | Gemini Laser | $A3 |
| $02 | WPN_NEEDLE | Needle Cannon | $A4 |
| $03 | WPN_HARD | Hard Knuckle | $A5 |
| $04 | WPN_MAGNET | Magnet Missile | $A6 |
| $05 | WPN_TOP | Top Spin | $A7 |
| $06 | WPN_SNAKE | Search Snake | $A8 |
| $07 | WPN_RUSH_COIL | Rush Coil | $A9 |
| $08 | WPN_SPARK | Spark Shock | $AA |
| $09 | WPN_RUSH_MARINE | Rush Marine | $AB |
| $0A | WPN_SHADOW | Shadow Blade | $AC |
| $0B | WPN_RUSH_JET | Rush Jet | $AD |

### Player States

| ID | Constant | State |
|----|----------|-------|
| $00 | PSTATE_GROUND | Idle/walking on ground |
| $01 | PSTATE_AIRBORNE | Jumping/falling |
| $02 | PSTATE_SLIDE | Sliding on ground |
| $03 | PSTATE_LADDER | On ladder |
| $04 | PSTATE_REAPPEAR | Respawn/reappear |
| $05 | PSTATE_ENTITY_RIDE | Riding Mag Fly |
| $06 | PSTATE_DAMAGE | Taking damage |
| $07 | PSTATE_SPECIAL_DEATH | Doc Flash Time Stopper kill |
| $08 | PSTATE_RUSH_MARINE | Riding Rush Marine |
| $09 | PSTATE_BOSS_WAIT | Frozen during boss intro |
| $0A | PSTATE_TOP_SPIN | Top Spin recoil bounce |
| $0B | PSTATE_WEAPON_RECOIL | Hard Knuckle fire freeze |
| $0C | PSTATE_VICTORY | Boss defeated cutscene |
| $0D | PSTATE_TELEPORT | Teleport away |
| $0E | PSTATE_DEATH | Dead, explosion |
| $0F | PSTATE_STUNNED | Frozen by external force |
| $10 | PSTATE_SCREEN_SCROLL | Vertical scroll transition |
| $11 | PSTATE_WARP_INIT | Teleporter initialization |
| $12 | PSTATE_WARP_ANIM | Warp animation |
| $13 | PSTATE_TELEPORT_BEAM | Proto Man exit beam |
| $14 | PSTATE_AUTO_WALK | Scripted walk (post-Wily) |
| $15 | PSTATE_AUTO_WALK2 | Scripted walk (ending) |

---

## 5. Stage Data Format

### Stage-to-Bank Mapping

**Location**: `ensure_stage_bank_table` at fixed bank `ppu_utils.asm:723`

| Stage ID | Constant | Stage Name | Bank |
|----------|----------|------------|------|
| $00 | STAGE_NEEDLE | Needle Man | $00 |
| $01 | STAGE_MAGNET | Magnet Man | $01 |
| $02 | STAGE_GEMINI | Gemini Man | $02 |
| $03 | STAGE_HARD | Hard Man | $03 |
| $04 | STAGE_TOP | Top Man | $04 |
| $05 | STAGE_SNAKE | Snake Man | $05 |
| $06 | STAGE_SPARK | Spark Man | $06 |
| $07 | STAGE_SHADOW | Shadow Man | $07 |
| $08 | STAGE_DOC_NEEDLE | Doc Robot (Needle stage) | $08 |
| $09 | STAGE_DOC_GEMINI | Doc Robot (Gemini stage) | $09 |
| $0A | STAGE_DOC_SPARK | Doc Robot (Spark stage) | $0A |
| $0B | STAGE_DOC_SHADOW | Doc Robot (Shadow stage) | $0B |
| $0C | STAGE_WILY1 | Wily Castle 1 | $0C |
| $0D | STAGE_WILY2 | Wily Castle 2 | $0D |
| $0E | STAGE_WILY3 | Wily Castle 3 | **$0D** (shared) |
| $0F | STAGE_WILY4 | Wily Castle 4 | $0F |
| $10 | STAGE_WILY5 | Wily Castle 5 | **$0D** (shared) |
| $11 | STAGE_WILY6 | Wily Castle 6 | $11 |
| $12 | (ending 1) | Special/ending | $12 |
| $13 | (ending 2) | Special/ending | $13 |
| $14 | (internal) | | $10 |
| $15 | (internal) | | $1E |
| $16 | (internal) | | $0E |

Wily 2, 3, and 5 all share bank $0D.

### Per-Bank Layout

Each stage bank occupies $A000-$BFFF (8 KB). Bank $00 is special: $A000-$A7FF holds global enemy tables shared across all stages.

| Offset | Size | Contents |
|--------|------|----------|
| $AA00 | variable | Screen metatile grid / room header data |
| $AA40,y | 1 byte/room | **Room config table** |
| $AA60,y*2 | 2 bytes/room | **Room pointer table** |
| $AA80 | 2 bytes | BG CHR bank indices → $E8/$E9 |
| $AA82+ | 20 bytes/screen | **Screen layout data** |
| $AB00,y | 1 byte/enemy | Enemy screen number |
| $AC00,y | 1 byte/enemy | Enemy X position |
| $AD00,y | 1 byte/enemy | Enemy Y position |
| $AE00,y | 1 byte/enemy | Enemy global ID |
| $AF00+ | variable | **Metatile column definitions** |
| $B700+ | 4 bytes/metatile | **Metatile sub-tile indices** (4 quadrant indices per metatile) |
| $BB00-$BEFF | 4 x 256 bytes | **Sub-tile CHR tables** ($BB=TL, $BC=TR, $BD=BL, $BE=BR) |
| $BF00 | 256 bytes | **Collision/palette table** (per sub-tile: low 2 bits=palette, upper nibble=collision) |

### Room Config Table ($AA40,y)

One byte per room:

| Bits | Meaning |
|------|---------|
| 7-6 | Vertical scroll: $40=down, $80=up |
| 5 | Horizontal scrolling enable |
| 4-0 | Screen count in this room |

### Room Pointer Table ($AA60,y*2)

Two bytes per room:
- **Byte 0**: CHR/palette param (indexes sprite palette table at bank $01 $A030 and sprite CHR table at $A200)
- **Byte 1**: Layout index (x20 offset into $AA82 screen layout data)

### Screen Layout Data ($AA82+, 20 bytes each)

- **Bytes 0-15**: 16 metatile column IDs (one per 16px column = 256px screen width)
- **Bytes 16-19**: Exit connections (up/down/left/right). Bit 7 = scroll type (0=warp, 1=scroll), bits 0-6 = target screen number.

### Collision Table ($BF00)

256 bytes, one per metatile index. Upper nibble = collision type:

| Value | Constant | Type |
|-------|----------|------|
| $00 | TILE_AIR | Passthrough |
| $10 | TILE_SOLID | Solid ground |
| $20 | TILE_LADDER | Climbable ladder |
| $30 | TILE_DAMAGE | Damage tile (lava/fire) |
| $40 | TILE_LADDER_TOP | Ladder top (grab point) |
| $50 | TILE_SPIKES | Instant kill spikes |
| $70 | TILE_DISAPPEAR | Disappearing block (Gemini stages) |

---

## 6. Palette Data

### RAM Palette Buffer ($0600-$063F)

| Address | Contents |
|---------|----------|
| $0600-$0603 | BG palette 0 |
| $0604-$0607 | BG palette 1 |
| $0608-$060B | BG palette 2 |
| $060C-$060F | BG palette 3 |
| $0610-$0613 | Sprite palette 0 (player colors) |
| $0614-$0617 | Sprite palette 1 |
| $0618-$061B | Sprite palette 2 (enemy sprites) |
| $061C-$061F | Sprite palette 3 (enemy sprites) |
| $0620-$063F | Mirror buffer (used during fade effects) |

When `palette_dirty` ($18) is set, the NMI handler copies $0600-$061F to PPU $3F00 (32 bytes).

### BG Palettes

Loaded per-stage from the stage bank. BG CHR bank indices at $AA80-$AA81 select which pattern table pages to use.

### Sprite Palettes (Bank $01)

**Table**: $A030 + (param x 8)
- 8 bytes per entry: SP2 palette (4 bytes) + SP3 palette (4 bytes)
- Param is byte 0 of the room pointer table entry ($AA60,y*2)
- Params $00-$11 = stage defaults (Needle through Wily 6)
- Params $12+ = per-room alternate palettes

### Sprite CHR Bank Table (Bank $01)

**Table**: $A200 + (param x 2)
- 2 bytes per entry: CHR bank for $EC (PPU $1800) and $ED (PPU $1C00)
- Same param index as sprite palettes

### Player Palette

Player colors live in SP0 at $0610-$0613. Initialized from the fixed bank `load_stage_default_palette_table` during stage load. Modified during damage flash and victory effects.

### Palette Loading Sequence

1. Switch to stage bank → load BG CHR indices from $AA80 → $E8/$E9
2. Load default SP0/SP1 palettes from fixed bank table → $0610-$0617
3. On room transition: read CHR/palette param from room pointer table
4. Switch to bank $01, index into $A030 (palettes) and $A200 (CHR banks)
5. Copy SP2/SP3 palettes → $0618-$061F, set CHR banks → $EC/$ED
6. Set `palette_dirty` — NMI uploads to PPU next VBlank

---

## 7. Sprite & Animation Data

### Bank Chain Overview

```
  Animation Sequences          Sprite Definitions         Position Offsets
  ┌──────────────────┐        ┌──────────────────┐       ┌──────────────────┐
  │ Bank $1A ($8000)  │───────→│ Bank $1A ($8000)  │──────→│ Bank $19 ($A000)  │
  │ IDs $00-$7F       │        │ Defs in same bank │       │ Default offsets   │
  ├──────────────────┤        ├──────────────────┤       ├──────────────────┤
  │ Bank $1B ($8000)  │───────→│ Bank $1B ($8000)  │──────→│ Bank $14 ($A000)  │
  │ IDs $80-$FF       │        │ Defs in same bank │       │ Alternate offsets │
  └──────────────────┘        └──────────────────┘       └──────────────────┘
         ▲                                                        ▲
         │  ent_anim_id bit 7                    sprite def byte 0 bit 7
         │  0 = bank $1A                         0 = bank $19
         │  1 = bank $1B                         1 = bank $14
         │
  ┌──────────────────┐
  │ Bank $15 ($8000)  │  Boss sprite override (when boss_active != 0)
  │ Slots $10-$1F     │  (enemy/boss slots)
  └──────────────────┘
```

### Animation Sequence Format (Banks $1A/$1B)

**Pointer tables**: $8000-$807F (low), $8080-$80FF (high) — 128 entries per bank

```
Byte 0:   frame_count    (0-based: N means N+1 frames)
Byte 1:   tick_speed     (game ticks per animation frame)
Byte 2+:  sprite_def_ID  (one per frame; $00 = deactivate entity)
```

The animation state machine advances each tick. When the tick counter reaches `tick_speed`, it advances to the next frame. After `frame_count + 1` frames, it loops to frame 0.

### Sprite Definition Format (Banks $1A/$1B)

**Pointer tables**: $8100-$81FF (low), $8200-$82FF (high) — 256 entries per bank

```
Byte 0:   count_flags    (bits 0-6: OAM sprite count, 0-based; bit 7: offset bank select)
Byte 1:   layout_id      (indexes position offset table in bank $19 or $14)
Byte 2+:  (tile_id, attribute) pairs — one per hardware OAM sprite
```

Attribute byte: bits 0-1 = palette, bit 5 = priority, bit 6 = H-flip, bit 7 = V-flip.

### Position Offset Format (Banks $19/$14)

**Pointer tables**: $BE00-$BEFF (low), $BF00-$BFFF (high) — 256 entries

```
Data: signed (Y_offset, X_offset) byte pairs, one per OAM sprite
```

Layout IDs come in pairs: even = normal, odd = horizontally flipped.

### OAM ID Bank Selection

`ent_anim_id` bit 7 determines which animation bank is loaded:
- Bit 7 clear ($00-$7F) → bank $1A mapped to $8000
- Bit 7 set ($80-$FF) → bank $1B mapped to $8000

The 7-bit index (ID AND $7F) is used for table lookup. During boss fights (`boss_active` != 0), enemy/boss slots ($10-$1F) override to bank $15, providing boss-specific sprite data independent of the stage's animation banks.

### Weapon Sprites (Bank $15)

Same format as banks $1A/$1B. Contains animation sequences and sprite definitions used during boss fights. Selected automatically for enemy/boss slots ($10-$1F) when `boss_active` is set, providing boss-specific sprite data independent of the stage's animation banks ($1A/$1B).

---

## 8. CHR Bank Layout

### MMC3 CHR Registers

Six shadow registers at ZP $E8-$ED, written to MMC3 by the NMI handler:

| Register | MMC3 Cmd | PPU Range | Size | Typical Use |
|----------|----------|-----------|------|-------------|
| $E8 | R0 | $0000-$07FF | 2 KB | BG tiles (left half) |
| $E9 | R1 | $0800-$0FFF | 2 KB | BG tiles (right half) |
| $EA | R2 | $1000-$13FF | 1 KB | Sprite tiles page 0 |
| $EB | R3 | $1400-$17FF | 1 KB | Sprite tiles page 1 |
| $EC | R4 | $1800-$1BFF | 1 KB | Sprite tiles page 2 |
| $ED | R5 | $1C00-$1FFF | 1 KB | Sprite tiles page 3 |

Mainline code writes to $E8-$ED and sets `$1B = $FF` (dirty flag). NMI calls `select_CHR_banks` to apply all 6 values.

### Per-Stage BG CHR

Each stage bank stores 2 bytes at $AA80:
- $AA80 → $E8 (BG left half)
- $AA81 → $E9 (BG right half)

### Per-Room Sprite CHR

Bank $01 at $A200 + (param x 2) stores 2 CHR bank numbers:
- Byte 0 → $EC (sprite page 2)
- Byte 1 → $ED (sprite page 3)

Pages 0-1 ($EA/$EB) are typically fixed at $00/$01 across all stages.

### Tracing a Tile

BG and sprite tiles use separate pattern tables selected by PPUCTRL bits 3-4.

**Background tiles** (PPU $0000-$0FFF):
- Tile IDs $00-$7F → $E8 (R0, PPU $0000-$07FF, 2 KB)
- Tile IDs $80-$FF → $E9 (R1, PPU $0800-$0FFF, 2 KB)

**Sprite tiles** (PPU $1000-$1FFF):
- Tile IDs $00-$3F → $EA (R2, PPU $1000-$13FF, 1 KB)
- Tile IDs $40-$7F → $EB (R3, PPU $1400-$17FF, 1 KB)
- Tile IDs $80-$BF → $EC (R4, PPU $1800-$1BFF, 1 KB)
- Tile IDs $C0-$FF → $ED (R5, PPU $1C00-$1FFF, 1 KB)

Each CHR bank number times 1 KB gives the offset into `chr/chr.bin` (128 KB total, 256 x 1 KB pages).

---

## 9. Music & Sound Effects

### Sound Engine

**Source**: `src/bank16_sound_driver.asm` (code at $8000), `src/bank17_sound_data.asm` (data at $A000)

**Pointer table**: Bank $16, $8A44 — contains pointers for all 57 sound IDs ($00-$38).

### Track Header Format

```
Byte 0:  type/priority
         $00        = music (followed by 4 channel pointers)
         $01-$7F    = SFX priority (higher = overrides lower-priority SFX)
         Bit 7 set  = chain flag (linked SFX follows)

Music: bytes 1-8 = 4 channel pointer pairs (hi/lo for Pulse1, Triangle, Pulse2, Noise)
SFX:   sequence data begins immediately after priority byte
```

### Music Track IDs

| ID | Constant | Track |
|----|----------|-------|
| $00 | MUSIC_TITLE | Title screen |
| $01 | MUSIC_NEEDLE | Needle Man stage |
| $02 | MUSIC_MAGNET | Magnet Man stage |
| $03 | MUSIC_GEMINI | Gemini Man stage |
| $04 | MUSIC_HARD | Hard Man stage |
| $05 | MUSIC_TOP | Top Man stage |
| $06 | MUSIC_SNAKE | Snake Man stage |
| $07 | MUSIC_SPARK | Spark Man stage |
| $08 | MUSIC_SHADOW | Shadow Man stage |
| $09 | MUSIC_WILY_1 | Wily Fortress 1-2 |
| $0A | MUSIC_WILY_2 | Wily Fortress 3-4 |
| $0B | MUSIC_WILY_3 | Wily Fortress 5-6 |
| $0C | MUSIC_WILY_MAP | Wily map / fortress gate |
| $0D | MUSIC_BOSS | Boss battle |
| $0E | MUSIC_PASSWORD | Password screen |
| $0F | MUSIC_CONTINUE | Continue screen |
| $10 | MUSIC_STAGE_SELECT | Stage select menu |
| $11 | MUSIC_PROTO_WHISTLE | Staff roll / Proto Man whistle |
| $12 | MUSIC_GAME_OVER | Ending / game over |

### SFX IDs

| ID | Constant | Sound |
|----|----------|-------|
| $13 | SFX_LAND | Player landing |
| $14 | SFX_1UP | 1-up / E-tank pickup |
| $15 | SFX_BUSTER | Mega Buster fire |
| $16 | SFX_PLAYER_HIT | Player takes damage |
| $17 | SFX_DEATH | Player / boss death |
| $18 | SFX_ENEMY_HIT | Weapon hits enemy |
| $19 | SFX_TINK | Weapon bounces off (immune) |
| $1A | SFX_PAUSE | Pause menu open |
| $1B | SFX_CURSOR | Menu cursor move |
| $1C | SFX_HP_FILL | HP bar refill tick |
| $1D | SFX_DOOR | Stage door / shutter |
| $1E | SFX_SHOT | Projectile fired |
| $1F | SFX_LAND_ALT | Landing (alternate) |
| $20 | SFX_STOMP | Heavy footstep |
| $22 | SFX_CLAMP | Clamp snap (Wanaan) |
| $23 | SFX_DEBRIS | Debris / object break |
| $24 | SFX_ATTACK | Enemy attack trigger |
| $25 | SFX_TURRET_FIRE | Turret projectile fire |
| $26 | SFX_APPROACH | Entity approach trigger |
| $27 | SFX_PLATFORM | Platform detach / item spawn |
| $28 | SFX_WIND | Wind effect (intro) |
| $2A | SFX_MAGNET_FIRE | Magnet Missile fire |
| $2B | SFX_GEMINI_FIRE | Gemini Laser fire |
| $2C | SFX_TOP_SPIN | Top Spin fire |
| $2D | SFX_SPARK_FIRE | Spark Shock fire |
| $2E | SFX_SHADOW_FIRE | Shadow Blade fire |
| $30 | SFX_HARD_STOMP | Hard Man stomp attack |
| $31 | SFX_WEAPON_GET | Weapon acquired jingle |
| $32 | SFX_EXPLOSION | Explosion burst |
| $34 | SFX_TELEPORT | Teleport beam |

Jingles that use the music range: $33 (MUSIC_STAGE_START), $35 (MUSIC_BOSS_INTRO), $36 (MUSIC_DOC_ROBOT), $37 (MUSIC_WILY_VICTORY), $38 (MUSIC_VICTORY).

IDs $21, $29, $2F are unused (no references in code).

### Sound Commands

| Value | Constant | Effect |
|-------|----------|--------|
| $F0 | SNDCMD_INIT | Silence / reset all channels |
| $F2 | SNDCMD_STOP | Stop current music |

### Stage-to-Music Mapping

**Table**: `frame_loop_stage_music_table` at `game_loop.asm:615`

| Stage | Music | Stage | Music |
|-------|-------|-------|-------|
| $00 Needle | $01 NEEDLE | $09 Doc Gemini | $03 GEMINI |
| $01 Magnet | $02 MAGNET | $0A Doc Spark | $07 SPARK |
| $02 Gemini | $03 GEMINI | $0B Doc Shadow | $08 SHADOW |
| $03 Hard | $04 HARD | $0C Wily 1 | $09 WILY_1 |
| $04 Top | $05 TOP | $0D Wily 2 | $09 WILY_1 |
| $05 Snake | $06 SNAKE | $0E Wily 3 | $0A WILY_2 |
| $06 Spark | $07 SPARK | $0F Wily 4 | $0A WILY_2 |
| $07 Shadow | $08 SHADOW | $10 Wily 5 | $0B WILY_3 |
| $08 Doc Needle | $01 NEEDLE | $11 Wily 6 | $0B WILY_3 |

Doc Robot stages reuse their original Robot Master music.

### Cross-Track Data Sharing

The sound data in bank $17 is a continuous stream where track headers serve as entry points into channel sequences. The driver uses absolute addresses for loop/jump commands, so cross-track sharing is architecturally possible. In practice, each of the 13 bank $17 tracks ($05-$11) is self-contained — all channel pointers and loop/jump targets stay within each track's own address range. Sound ID $12 (Ending) points to $C01D in bank $18 via `read_ptr`'s high-byte remapping.

---

## 10. Text & Screen Data

### No ASCII in ROM

All text in Mega Man 3 is rendered as CHR tile arrangements. There are no ASCII strings — every character is a tile ID referencing the pattern table in `chr/chr.bin`.

### Nametable String Data (Bank $0E)

**Source**: `src/bank0E_anim_frames.asm`
**Range**: $A0C1-$A32C (14 variable-length strings)

Format:
```
$FE xx yy  = set PPU write address to $xxyy (new row/column position)
$FF        = end of string
Other      = CHR tile ID (written sequentially to nametable)
```

Strings 0-7 are Robot Master bio screens:

| Index | Content |
|-------|---------|
| 0 | Needle Man bio |
| 1 | Magnet Man bio |
| 2 | Gemini Man bio |
| 3 | Hard Man bio |
| 4 | Top Man bio |
| 5 | Snake Man bio |
| 6 | Spark Man bio |
| 7 | Shadow Man bio |
| 8-13 | Password screen / score text |

### "READY" Overlay

The "READY" text is not nametable data — it's rendered as OAM sprites. The data table `frame_loop_ready_overlay_data` at `game_loop.asm:609` contains 5 OAM entries (Y, tile, attribute, X). During stage start, the overlay flashes by toggling visibility every 16 frames for 60 frames total.

### Modifying Text

1. Edit the CHR tile graphics in `chr/chr.bin` (tile art)
2. Edit the nametable string data in bank $0E (tile placement)
3. For OAM text like "READY", edit the sprite data table in `game_loop.asm`

---

## 11. Named Constants

Complete dump from `include/constants.inc`.

### Entity Arrays ($0300-$05E0)

```
$0300  ent_status       entity active flag (bit 7 = active)
$0320  ent_routine      AI routine index
$0340  ent_x_sub        X position sub-pixel
$0360  ent_x_px         X position pixel
$0380  ent_x_scr        X position screen/page
$03A0  ent_y_sub        Y position sub-pixel
$03C0  ent_y_px         Y position pixel
$03E0  ent_y_scr        Y position screen/page
$0400  ent_xvel_sub     X velocity sub-pixel
$0420  ent_xvel         X velocity pixel (signed)
$0440  ent_yvel_sub     Y velocity sub-pixel
$0460  ent_yvel         Y velocity pixel (signed)
$0480  ent_hitbox       shape/hitbox (bit 7 = contact damage)
$04A0  ent_facing       facing direction
$04C0  ent_spawn_id     stage enemy ID (spawn tracking)
$04E0  ent_hp           health / hit points
$0500  ent_timer        AI timer / general counter
$0520  ent_var1         general purpose field 1
$0540  ent_var2         general purpose field 2
$0560  ent_var3         general purpose field 3
$0580  ent_flags        bit 7=active, bit 6=H-flip, bit 4=world coords
$05A0  ent_anim_state   animation state (0=reset)
$05C0  ent_anim_id      current animation / OAM ID
$05E0  ent_anim_frame   animation frame counter
```

32 slots with $20 stride: slot 0 = player, $01-$0F = weapons/projectiles, $10-$1F = enemies.

### Stage IDs

```
$00  STAGE_NEEDLE      $08  STAGE_DOC_NEEDLE
$01  STAGE_MAGNET      $09  STAGE_DOC_GEMINI
$02  STAGE_GEMINI      $0A  STAGE_DOC_SPARK
$03  STAGE_HARD        $0B  STAGE_DOC_SHADOW
$04  STAGE_TOP         $0C  STAGE_WILY1
$05  STAGE_SNAKE       $0D  STAGE_WILY2
$06  STAGE_SPARK       $0E  STAGE_WILY3
$07  STAGE_SHADOW      $0F  STAGE_WILY4
                       $10  STAGE_WILY5
                       $11  STAGE_WILY6
```

### Weapon IDs

```
$00  WPN_BUSTER        $06  WPN_SNAKE
$01  WPN_GEMINI        $07  WPN_RUSH_COIL
$02  WPN_NEEDLE        $08  WPN_SPARK
$03  WPN_HARD          $09  WPN_RUSH_MARINE
$04  WPN_MAGNET        $0A  WPN_SHADOW
$05  WPN_TOP           $0B  WPN_RUSH_JET
```

### Button Masks

```
$80  BTN_A             $08  BTN_UP
$40  BTN_B             $04  BTN_DOWN
$20  BTN_SELECT        $02  BTN_LEFT
$10  BTN_START         $01  BTN_RIGHT
```

### Tile Collision Types

```
$00  TILE_AIR           passthrough
$10  TILE_SOLID         solid ground
$20  TILE_LADDER        climbable ladder
$30  TILE_DAMAGE        damage tile (lava/fire)
$40  TILE_LADDER_TOP    ladder top (grab point)
$50  TILE_SPIKES        instant kill spikes
$70  TILE_DISAPPEAR     disappearing block
```

### Facing / Flags

```
$01  FACING_RIGHT       $40  ENT_FLAG_HFLIP
$02  FACING_LEFT        $9C  HEALTH_FULL (28 bars)
```

### Sound Commands

```
$F0  SNDCMD_INIT        silence / reset all channels
$F2  SNDCMD_STOP        stop current music
```
