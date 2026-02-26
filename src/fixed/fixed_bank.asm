; da65 V2.18 - Ubuntu 2.19-1
; Created:    2026-02-21 06:01:26
; Input file: build/bank1E_1F.bin
; Page:       1



; =============================================================================
; MEGA MAN 3 (U) — BANKS $1E-$1F — MAIN GAME LOGIC (FIXED BANK)
; =============================================================================
; This is the fixed code bank for Mega Man 3. Always mapped to $C000-$FFFF.
; Contains:
;   - NMI / interrupt handling
;   - Player state machine (22 states via player_state_ptr_lo/hi)
;   - Player movement physics (walk, jump, gravity, slide, ladder)
;   - Weapon firing and initialization
;   - Entity movement routines (shared by player and enemies)
;   - Sprite animation control
;   - Collision detection (player-entity, weapon-entity)
;   - Sound submission
;   - PRG/CHR bank switching
;
; NOTE: MM3 shares its engine with Mega Man 4 (MM4). Many routines here
; are near-identical to MM4's bank38_3F.asm (the MM4 fixed bank).
; MM4 cross-references (from megaman4-disassembly):
;   process_sprites    ($1C800C) → code_3A8014 — entity processing loop
;   check_player_hit   ($1C8097) → code_3A81CC — contact damage check
;   check_weapon_hit   ($1C8102) → code_3FF95D — weapon-entity collision
;   find_enemy_freeslot_x ($1FFC43) → code_3FFB5C — find empty entity slot (X)
;   find_enemy_freeslot_y ($1FFC53) → code_3FFB6C — find empty entity slot (Y)
;   move_sprite_right  ($1FF71D) → code_3FF22D — entity X += velocity
;   move_sprite_left   ($1FF73B) → code_3FF24B — entity X -= velocity
;   move_sprite_down   ($1FF759) → code_3FF269 — entity Y += velocity
;   move_sprite_up     ($1FF779) → code_3FF289 — entity Y -= velocity
;   reset_sprite_anim  ($1FF835) → (equivalent in bank38_3F)
;   reset_gravity      ($1FF81B) → (equivalent in bank38_3F)
;   face_player        ($1FF869) → (equivalent in bank38_3F)
;   select_PRG_banks   ($1FFF6B) → code_3FFF37 — MMC3 bank switch
;   NMI                ($1EC000) → NMI ($3EC000) — vblank interrupt
;   RESET              ($1FFE00) → RESET ($3FFE00) — power-on entry
;
; Key differences from MM4: 32 entity slots (vs 24), $20 stride (vs $18),
; direct routine-index dispatch (vs page-based AI state machine),
; 16 weapon slots (vs 8), Top Spin mechanic, no 16-bit X coordinates.
;
; Annotation: all labels named, ~3970 inline comments.
;
; =============================================================================
; KEY MEMORY MAP
; =============================================================================
;
; Zero Page (see include/zeropage.inc for full list):
;   joy1_press ($14)       = new button presses this frame
;   joy1_held ($16)        = buttons held / continuous
;   palette_dirty ($18)    = palette update request flag
;   nametable_dirty ($19)  = nametable update request flag
;   nt_column_dirty ($1A)  = nametable column update flag
;   stage_id ($22)         = current stage index (see STAGE MAPPING below)
;   player_state ($30)     = player state (index into player_state_ptr, 22 states)
;   player_facing ($31)    = facing direction (FACING_RIGHT, FACING_LEFT)
;   walk_flag ($32)        = walking / sub-state (nonzero = active)
;   entity_ride_slot ($34) = entity slot being ridden (state $05)
;   facing_sub ($35)       = facing sub-state / Mag Fly direction inherit
;   invincibility_timer ($39) = i-frames timer (nonzero = immune)
;   jump_counter ($3A)     = jump/rush counter
;   hazard_pending ($3D)   = pending hazard ($06=damage, $0E=death)
;   tile_at_feet_max ($41) = max tile type at feet (highest priority hazard)
;   scroll_lock ($50)      = scroll lock flag
;   boss_active ($5A)      = boss active (bit 7 = fight in progress)
;   stage_select_page ($60) = select page (0=RM, nonzero=Doc Robot/Wily)
;   bosses_beaten ($61)    = boss-defeated bitmask (bit N = stage N beaten)
;   proto_man_flag ($68)   = cutscene-complete flag (Proto Man encounter)
;   warp_dest ($6C)        = warp destination index (teleporter target)
;   screen_mode ($78)      = screen mode
;   gravity ($99)          = sub-pixel increment ($55 gameplay, $40 stage select)
;
; Weapon / Inventory Block ($A0-$AF):
;   current_weapon ($A0)   = weapon ID (see constants.inc WPN_*)
;   weapon_cursor ($A1)    = pause screen cursor position
;   player_hp ($A2)        = player HP (HEALTH_FULL=full, $80=empty, 28 max)
;   player_hp+1..+$0B      = weapon ammo ($A3-$AD, indexed by weapon ID)
;   lives ($AE)            = extra lives (BCD)
;   etanks ($AF)           = E-Tank count
;
;   boss_hp_display ($B0)  = boss HP bar position (starts $80, fills to HEALTH_FULL)
;   nmi_skip ($EE)         = NMI skip flag
;   sprite_slot ($EF)      = current sprite slot (process_sprites loop var)
;   mmc3_shadow ($F0)      = MMC3 bank select register shadow
;   prg_bank ($F5)         = current $A000-$BFFF PRG bank number
;   game_mode ($F8)        = game mode / screen state
;   camera_screen ($F9)    = camera/scroll page
;   scroll_y ($FA)         = vertical scroll
;   camera_x_lo/hi ($FC-$FD) = camera X position
;   ppu_mask_shadow ($FE)  = PPUMASK ($2001) shadow
;   ppu_ctrl_shadow ($FF)  = PPUCTRL ($2000) shadow
;
; WEAPON IDS (see constants.inc WPN_*):
;   $00 = Mega Buster    $04 = Magnet Missile  $08 = Spark Shock
;   $01 = Gemini Laser   $05 = Top Spin        $09 = Rush Marine
;   $02 = Needle Cannon  $06 = Search Snake    $0A = Shadow Blade
;   $03 = Hard Knuckle   $07 = Rush Coil       $0B = Rush Jet
;
; STAGE MAPPING ($22 values):
;   Robot Masters (stage select grid, $12=col 0-2, $13=row 0/3/6):
;     $00 = Needle Man   (col 2, row 0 = top-right)
;     $01 = Magnet Man   (col 1, row 2 = bottom-middle)
;     $02 = Gemini Man   (col 0, row 2 = bottom-left)
;     $03 = Hard Man     (col 0, row 1 = middle-left)
;     $04 = Top Man      (col 2, row 1 = middle-right)
;     $05 = Snake Man    (col 1, row 0 = top-middle)
;     $06 = Spark Man    (col 0, row 0 = top-left)
;     $07 = Shadow Man   (col 2, row 2 = bottom-right)
;   Doc Robot stages: $08-$0B (4 revisited stages)
;   Wily Castle: $0C+ (fortress stages)
;   Stage select grid lookup table at CPU $9CE1 (bank18.asm)
;
; PLAYER PHYSICS:
;   Normal jump velocity:    $04.E5 (~4.9 px/frame upward)
;   Super jump (Rush Coil):  $08.00 (8.0 px/frame upward)
;   Slide jump velocity:     $06.EE (~6.9 px/frame upward)
;   Resting Y velocity:      $FF.C0 (-0.25, gentle ground pin)
;   Terminal fall velocity:   $F9.00 (-7.0, clamped in $99 code)
;   Gravity per frame:        $00.55 (0.332 px/frame², set in process_sprites)
;   Velocity format: ent_yvel.ent_yvel_sub = whole.sub, higher = upward
;
; TILE COLLISION TYPES (upper nibble of $BF00,y tile attribute table):
;   $00 = air (passthrough, no collision)
;   $10 = solid ground (bit 4 = solid flag)
;   $20 = ladder (climbable when pressing Up, passthrough)
;   $30 = damage tile (lava/fire — triggers player_damage, solid)
;   $40 = ladder top (climbable, grab point from above)
;   $50 = spikes (instant kill, solid)
;   $70 = disappearing block (Gemini Man stages $02/$09 only, solid when visible)
;   Solid test: accumulate tile types in $10, then AND #$10 — nonzero = on solid ground
;   Hazard priority: $41 tracks max type, $30→damage, $50→death
;   $BF00 table is per-stage, loaded from the stage's PRG bank
;
; LEVEL DATA FORMAT (per-stage PRG bank, mapped to $A000-$BFFF):
;   Stage bank = ensure_stage_bank_table[$22] at $C8B9 (usually bank = stage index)
;   $A000-$A4FF: enemy data tables (flags, main routine IDs, spawn data)
;   $AA00+:      screen metatile grid (column IDs, read during tile collision)
;   $AA60:       screen pointer table (2 bytes/screen: init param, layout index)
;   $AA80-$AA81: palette indices for stage
;   $AA82+:      screen layout data, 20 bytes per screen:
;                  bytes 0-15: 16 metatile column IDs (one per 16px column)
;                  bytes 16-19: screen connection data (bit 7=flag, bits 0-6=target)
;   $AB00+:      enemy placement tables (4 tables, indexed by stage enemy ID):
;                  $AB00,y = screen number, $AC00,y = X pixel, $AD00,y = Y pixel,
;                  $AE00,y = global enemy ID (→ bank $00 for AI/shape/HP)
;   $AF00+:      metatile column definitions, 64 bytes per column ID
;                  (vertical strip of metatile indices for one 16px-wide column)
;   $B700+:      metatile CHR definitions, 4 bytes per metatile index
;                  (2x2 CHR tile pattern: TL, TR, BL, BR)
;   $BF00-$BFFF: collision attribute table, 1 byte per metatile index (256 entries)
;                  upper nibble = collision type (see TILE COLLISION TYPES above)
;   Screen = 16 columns x N rows of 16x16 metatiles = 256px wide
;   load_stage routine at $1EC816 copies screen data to RAM ($0600+)
;
; Entity Arrays (indexed by X, stride $20, 32 slots $00-$1F):
;   ent_status,x = entity active flag (bit 7 = active)
;   ent_routine,x = main routine index (AI type, dispatched in process_sprites)
;   ent_x_sub,x = X position sub-pixel
;   ent_x_px,x = X position pixel
;   ent_x_scr,x = X position screen/page
;   ent_y_sub,x = Y position sub-pixel
;   ent_y_px,x = Y position pixel
;   ent_y_scr,x = Y position screen/page
;   ent_xvel_sub,x = X velocity sub-pixel
;   ent_xvel,x = X velocity pixel (signed: positive=right, negative=left)
;   ent_yvel_sub,x = Y velocity sub-pixel
;   ent_yvel,x = Y velocity pixel (signed: negative=up/$99, positive=down)
;   ent_hitbox,x = entity shape/hitbox (bit 7 = causes player contact damage)
;   ent_facing,x = facing direction (FACING_RIGHT, FACING_LEFT)
;   ent_spawn_id,x = stage enemy ID (spawn tracking, prevents duplicate spawns)
;   ent_hp,x = health / hit points
;   ent_timer,x = AI timer / general purpose counter
;   ent_var1,x = general purpose / wildcard 1
;   ent_var2,x = general purpose / wildcard 2
;   ent_var3,x = general purpose / wildcard 3
;   ent_flags,x = entity flags (bit 7=active/collidable, bit 6=H-flip, bit 4=world coords)
;   ent_anim_state,x = animation state (0=reset, set by reset_sprite_anim)
;   ent_anim_id,x = current animation / OAM ID
;   ent_anim_frame,x = animation frame counter (bit 7 preserved on anim reset)
;
; Entity Slot Ranges:
;   Slot $00       = Mega Man (player)
;   Slots $01-$0F  = weapons / projectiles (15 reserved slots)
;   Slots $10-$1F  = enemies / items / bosses (searched by find_enemy_freeslot)
;
; MM3→MM4 Entity Address Cross-Reference:
;   MM3 ent_status,x → MM4 ent_status,x  (entity active flag)
;   MM3 ent_routine,x → MM4 n/a      (routine index; MM4 uses page-based dispatch)
;   MM3 ent_x_sub,x → MM4 $0318,x  (X sub-pixel)
;   MM3 ent_x_px,x → MM4 $0330,x  (X pixel)
;   MM3 ent_x_scr,x → MM4 $0348,x  (X screen/page)
;   MM3 ent_y_sub,x → MM4 ent_x_px,x  (Y sub-pixel)
;   MM3 ent_y_px,x → MM4 $0378,x  (Y pixel)
;   MM3 ent_y_scr,x → MM4 $0390,x  (Y screen/page)
;   MM3 ent_xvel_sub,x → MM4 $03A8,x  (X velocity sub-pixel)
;   MM3 ent_xvel,x → MM4 ent_y_px,x  (X velocity pixel)
;   MM3 ent_yvel_sub,x → MM4 $03D8,x  (Y velocity sub-pixel)
;   MM3 ent_yvel,x → MM4 $03F0,x  (Y velocity pixel)
;   MM3 ent_hitbox,x → MM4 $0408,x  (shape/hitbox)
;   MM3 ent_facing,x → MM4 ent_xvel,x  (facing/direction)
;   MM3 ent_hp,x → MM4 (TBD)    (health)
;   MM3 ent_timer,x → MM4 $0468,x  (AI timer)
;   MM3 ent_flags,x → MM4 $0528,x  (entity flags)
;   MM3 ent_anim_id,x → MM4 $0558,x  (animation/OAM ID)
;   MM3 ent_anim_frame,x → MM4 ent_var2,x  (animation frame counter)
;
; Sound System:
;   submit_sound_ID ($1FFA98) — submit a sound effect to the queue
;
; Palette RAM:
;   $0600-$061F = palette buffer (32 bytes, uploaded to PPU $3F00 during NMI)
;
; Controller Button Masks (for joy1_press / joy1_held):
;   BTN_A      = A button (Jump)
;   BTN_B      = B button (Fire)
;   BTN_SELECT = Select
;   BTN_START  = Start
;   BTN_UP     = Up
;   BTN_DOWN   = Down
;   BTN_LEFT   = Left
;   BTN_RIGHT  = Right
;
; Controller 2 Debug Features (shipped in retail cart):
;   $17 = P2 held buttons — read directly by debug checks in gameplay loop.
;   P2 Right ($01): super jump ($08.00 vs $04.E5) + pit death immunity
;   P2 Up    ($08): slow-motion (animations tick every 8th frame)
;   P2 A     ($80): full freeze (all animation stops)
;   P2 Left  ($02): latches Right flag permanently (d-pad can't do L+R)
;   This debug code was left in by Capcom and is present in all retail copies.
;
; Fixed-Point Format: 8.8 (high byte = pixels, low byte = sub-pixel / 256)
;   Example: $01.4C = 1 + 76/256 = 1.297 pixels/frame
;
; PHYSICS CONSTANTS (from reset_gravity and player movement code):
;   Gravity (player):  $FFC0 as velocity = -0.25 px/frame² (upward bias)
;   Gravity (enemies): $FFAB as velocity = -0.332 px/frame²
;   Terminal velocity: $07.00 = 7.0 px/frame downward (clamped at ent_yvel >= $07)
;   Walk speed:        $01.4C = 1.297 px/frame (same as MM4)
;   Slide speed:       $02.80 = 2.5 px/frame (20 frames max, cancellable after 8)
;   Jump velocity:     $04.E5 = ~4.898 px/frame upward (confirmed, see .jump at $ECEB3)
;
; PLAYER STATES (22 states, index in $30, dispatched via player_state_ptr tables):
; Verified via Mesen 0.9.9 write breakpoint on $0030 during full playthrough.
;   $00 = player_on_ground    ($CE36) — idle/walking on ground [confirmed]
;   $01 = player_airborne     ($D007) — jumping/falling, variable jump [confirmed]
;   $02 = player_slide        ($D3FD) — sliding on ground [confirmed]
;   $03 = player_ladder       ($D4EB) — on ladder (climb, fire, jump off) [confirmed]
;   $04 = player_reappear     ($D5BA) — respawn/reappear (death, unpause, stage start) [confirmed]
;   $05 = player_entity_ride  ($D613) — riding Mag Fly (magnetic pull, Magnet Man stage) [confirmed]
;   $06 = player_damage       ($D6AB) — taking damage (contact or projectile) [confirmed]
;   $07 = player_special_death($D831) — Doc Flash Time Stopper kill [confirmed]
;   $08 = player_rush_marine  ($D858) — riding Rush Marine submarine [confirmed]
;   $09 = player_boss_wait    ($D929) — frozen during boss intro (shutters/HP bar) [confirmed]
;   $0A = player_top_spin     ($D991) — Top Spin recoil bounce (8 frames) [confirmed]
;   $0B = player_weapon_recoil($D9BE) — Hard Knuckle fire freeze (16 frames) [confirmed]
;   $0C = player_victory      ($D9D3) — boss defeated cutscene (jump, get weapon) [confirmed]
;   $0D = player_teleport     ($DBE1) — teleport away, persists through menus [confirmed]
;   $0E = player_death        ($D779) — dead, explosion on all sprites [confirmed]
;   $0F = player_stunned      ($CDCC) — frozen by external force (slam/grab/cutscene) [confirmed]
;   $10 = player_screen_scroll($DD14) — vertical scroll transition between sections [confirmed]
;   $11 = player_warp_init    ($DDAA) — teleporter tube area initialization [confirmed]
;   $12 = player_warp_anim    ($DE52) — wait for warp animation, return to $00 [confirmed]
;   $13 = player_teleport_beam($DF33) — Proto Man exit beam (Wily gate scene) [confirmed]
;   $14 = player_auto_walk    ($DF8A) — scripted walk to X=$50 (post-Wily) [confirmed]
;   $15 = player_auto_walk_2  ($E08C) — scripted walk to X=$68 (ending cutscene) [confirmed]
;
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"


.segment "FIXED"

task_ptr           := $0093
irq_handler_ptr           := $009C
banked_8000     := $8000
banked_8003     := $8003
banked_8006     := $8006
banked_8009     := $8009
banked_800C     := $800C
banked_800F     := $800F
banked_8012     := $8012
stage_select_rm_intro := $9000
stage_select_password := $9003
stage_select_wily_gate := $9006
stage_select_title := $9009
check_new_enemies := $9C00
banked_A000     := $A000
banked_A003     := $A003
banked_A006     := $A006

; =============================================================================
; SUB-FILE INCLUDES
; =============================================================================
; The fixed bank code is split into 12 logical sub-files for navigability.
; All labels remain in one compilation unit — no exports/imports needed.
; =============================================================================

.include "src/fixed/nmi.asm"
.include "src/fixed/irq.asm"
.include "src/fixed/ppu_utils.asm"
.include "src/fixed/game_loop.asm"
.include "src/fixed/player_ground.asm"
.include "src/fixed/player_states.asm"
.include "src/fixed/player_warp.asm"
.include "src/fixed/camera.asm"
.include "src/fixed/collision.asm"
.include "src/fixed/sprites.asm"
.include "src/fixed/movement.asm"
.include "src/fixed/system.asm"
