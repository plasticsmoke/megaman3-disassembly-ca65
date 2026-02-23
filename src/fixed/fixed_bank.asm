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
;   Stage bank = stage_to_bank[$22] table at $C8B9 (usually bank = stage index)
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
NMI:  php                               ; push processor status (NMI entry = $C000)
nmi_preserve_regs:  pha                 ; push A (this addr = $C001 = MMC3 IRQ reload)
        txa                             ; preserve X, Y, and
        pha                             ; processor flags
        tya                             ; preserve Y
        pha                             ; push Y

; --- NMI: disable rendering for safe PPU access during VBlank ---
        lda     PPUSTATUS               ; reset PPU address latch
        lda     ppu_ctrl_shadow         ; PPUCTRL: clear NMI enable (bit 7)
        and     #$7F                    ; to prevent re-entrant NMI
        sta     PPUCTRL                 ; write PPUCTRL with NMI disabled
        lda     #$00                    ; PPUMASK = 0: rendering off
        sta     PPUMASK                 ; (safe to write PPU during VBlank)

; --- check if PPU updates are suppressed ---
        lda     nmi_skip                ; rendering disabled?
        ora     $9A                     ; $9A = NMI lock flag
        bne     nmi_scroll_setup        ; either set → skip PPU writes, go to scroll

; --- latch scroll/mode/scanline values for this frame ---
        lda     camera_x_lo             ; latch scroll X fine
        sta     scroll_x_fine           ; latch to scroll_x_fine
        lda     camera_x_hi             ; latch nametable select
        sta     nt_select               ; latch nametable select
        lda     game_mode               ; latch game mode
        sta     screen_mode             ; latch screen mode
        lda     scroll_lock             ; if secondary split active,
        bne     nmi_oam_dma_setup       ; keep current irq_scanline (set by split code)
        lda     $5E                     ; else use default scanline count from $5E
        sta     irq_scanline            ; store default scanline count

; --- OAM DMA: transfer sprite data from $0200-$02FF to PPU ---
nmi_oam_dma_setup:  lda     #$00        ; OAMADDR = $00 (start of OAM)
        sta     OAMADDR                 ; write OAMADDR = 0
        lda     #$02                    ; OAMDMA: copy page $02 ($0200-$02FF)
        sta     OAMDMA                  ; to PPU OAM (256 bytes, 64 sprites)

; --- drain primary PPU buffer ($19 flag) ---
        lda     nametable_dirty         ; nametable_dirty = PPU buffer pending flag
        beq     nmi_drain_secondary_buffer ; no data → skip
        jsr     drain_ppu_buffer        ; write buffered tile data to PPU

; --- drain secondary PPU buffer with VRAM increment ($1A flag) ---
nmi_drain_secondary_buffer:  lda     nt_column_dirty ; nt_column_dirty = secondary buffer flag (vertical writes)
        beq     nmi_palette_update      ; no data → skip
        lda     ppu_ctrl_shadow         ; PPUCTRL: set bit 2 (VRAM addr +32 per write)
        and     #$7F                    ; for vertical column writes to nametable
        ora     #$04                    ; set VRAM +32 increment bit
        sta     PPUCTRL                 ; write PPUCTRL with +32 VRAM increment
        ldx     #$00                    ; clear $1A flag
        stx     nt_column_dirty         ; clear secondary buffer flag
        jsr     drain_ppu_buffer_continue ; write column data to PPU
        lda     ppu_ctrl_shadow         ; PPUCTRL: restore normal increment (+1)
        and     #$7F                    ; clear NMI enable bit
        sta     PPUCTRL                 ; write PPUCTRL with +1 VRAM increment

; --- write palette data ($18 flag) ---
nmi_palette_update:  lda     palette_dirty ; palette update pending?
        beq     nmi_scroll_setup        ; no update → skip to scroll
        ldx     #$00                    ; clear $18 flag
        stx     palette_dirty           ; clear palette dirty flag
        lda     PPUSTATUS               ; reset PPU latch
        lda     #$3F                    ; PPU addr = $3F00 (palette RAM start)
        sta     PPUADDR                 ; set PPU addr high = $3F
        stx     PPUADDR                 ; set PPU addr low = $00
        ldy     #$20                    ; 32 bytes = all 8 palettes (4 BG + 4 sprite)
nmi_palette_copy_loop:  lda     $0600,x ; copy palette from $0600-$061F to PPU
        sta     PPUDATA                 ; write palette byte to PPU
        inx                             ; next byte
        dey                             ; decrement count
        bne     nmi_palette_copy_loop   ; loop all 32 palette bytes
        lda     #$3F                    ; reset PPU address to $3F00 then $0000
        sta     PPUADDR                 ; (two dummy writes to clear PPU latch
        sty     PPUADDR                 ; and prevent palette corruption during
        sty     PPUADDR                 ; scroll register writes below)
        sty     PPUADDR                 ; second dummy write

; --- set scroll position for this frame ---
nmi_scroll_setup:  lda     screen_mode  ; game mode $02 = stage select screen
        cmp     #$02                    ; (uses horizontal-only scroll from $5F)
        bne     nmi_scroll_mode_check   ; not stage select → use gameplay scroll
        lda     PPUSTATUS               ; reset PPU latch
        lda     $5F                     ; PPUSCROLL X = $5F (stage select scroll)
        sta     PPUSCROLL               ; write X scroll for stage select
        lda     #$00                    ; PPUSCROLL Y = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        beq     nmi_restore_rendering   ; → restore rendering
nmi_scroll_mode_check:  lda     PPUSTATUS ; reset PPU latch
        lda     scroll_x_fine           ; PPUSCROLL X = $79 (gameplay scroll X)
        sta     PPUSCROLL               ; write gameplay X scroll
        lda     scroll_y                ; PPUSCROLL Y = $FA (vertical scroll offset)
        sta     PPUSCROLL               ; write gameplay Y scroll

; --- restore rendering and CHR banks ---
nmi_restore_rendering:  lda     ppu_mask_shadow ; PPUMASK = $FE (re-enable rendering)
        sta     PPUMASK                 ; re-enable rendering
        lda     nt_select               ; PPUCTRL = $FF | (nt_select & $03)
        and     #$03                    ; bits 0-1 from $7A = nametable select
        ora     ppu_ctrl_shadow         ; rest from ppu_ctrl_shadow (NMI enable, sprite table, etc.)
        sta     PPUCTRL                 ; write PPUCTRL with nametable
        jsr     select_CHR_banks        ; set MMC3 CHR bank registers
        lda     mmc3_shadow             ; $8000 = MMC3 bank select register
        sta     MMC3_BANK_SELECT        ; (restore R6/R7 select state from $F0)

; --- NMI: set up MMC3 scanline IRQ for this frame ---
; irq_scanline = scanline count for first IRQ. irq_enable = flag (0=off, 1=on).
; screen_mode = game mode, used to index irq_vector_table for handler address.
; scroll_lock/$51 = secondary split: if scroll_lock != 0, use gameplay handler.
        lda     irq_scanline            ; scanline count for first split
        sta     NMI                     ; set MMC3 IRQ counter value
        sta     nmi_preserve_regs       ; latch counter (reload)
        ldx     irq_enable              ; IRQ enable flag
        sta     auto_walk_spawn_done,x  ; $9B=0 → $E000 (disable), $9B=1 → $E001 (enable)
        beq     nmi_frame_counter_tick  ; if disabled, skip vector setup
        ldx     screen_mode             ; X = game mode (index into vector table)
        lda     scroll_lock             ; secondary split flag
        beq     nmi_irq_vector_setup    ; if no secondary split, use mode index
        lda     irq_scanline            ; if irq_scanline < $51, use mode index
        cmp     $51                     ; (first split happens before secondary)
        bcc     nmi_irq_vector_setup    ; first split before secondary
        ldx     #$01                    ; else override: use index 1 (gameplay)
nmi_irq_vector_setup:  lda     irq_vector_lo,x ; IRQ vector low byte from table
        sta     irq_handler_ptr         ; store handler address low byte
        lda     irq_vector_hi,x         ; IRQ vector high byte from table
        sta     $9D                     ; → $9C/$9D = handler address for JMP ($009C)

; --- NMI: frame counter and sound envelope timers ---
nmi_frame_counter_tick:  inc     frame_counter ; frame_counter = frame counter (increments every NMI)
        ldx     #$FF                    ; $90 = $FF (signal: NMI occurred this frame)
        stx     nmi_occurred            ; signal NMI occurred
        inx                             ; X = 0
        ldy     #$04                    ; 4 sound channels ($80-$8F, 4 bytes each)
nmi_sound_channel_loop:  lda     $80,x  ; channel state: $01 = envelope counting down
        cmp     #$01                    ; check if envelope is counting down
        bne     nmi_channel_next        ; not counting down, skip
        dec     $81,x                   ; decrement envelope timer
        bne     nmi_channel_next        ; not zero → still counting
        lda     #$04                    ; timer expired → set state to $04 (release)
        sta     $80,x                   ; set channel state to $04 (release)
nmi_channel_next:  inx                  ; advance to next channel (+4 bytes)
        inx                             ; advance channel index +1
        inx                             ; advance channel index +2
        inx                             ; advance channel index +3
        dey                             ; loop 4 channels
        bne     nmi_sound_channel_loop  ; loop all 4 channels
        tsx                             ; get current stack pointer into X
        lda     $0107,x                 ; read return address high byte from stack
        sta     $7D                     ; preserve original address
        lda     $0106,x                 ; read return address low byte from stack
        sta     $7C                     ; save to $7C ($7C/$7D = interrupted PC)
        lda     #$C1                    ; load $C1 (high byte of fake return)
        sta     $0107,x                 ; overwrite stack return addr high byte
        lda     #$21                    ; load $21 (low byte → RTI resumes at $C122)
        sta     $0106,x                 ; overwrite stack return addr low byte
        pla                             ; begin register restore
        tay                             ; restore Y
        pla                             ; restore X, Y, and P flags
        tax                             ; and clear interrupt flag
        pla                             ; restore A
        plp                             ; restore processor flags
        rti                             ; FAKE RETURN

; does not actually return, stack is hardcoded to go right here
; another preserve & restore just to call play_sounds
; this is done in case NMI happened in the middle of selecting PRG banks
; because play_sounds also selects PRG banks - possible race condition
; is handled in play_sounds routine

        php                             ; leave a stack slot for word sized
        php                             ; return address for the RTS
        php                             ; 3rd push for return address frame
        pha                             ; push A
        txa                             ; once again, preserve X, Y & flags
        pha                             ; push X
        tya                             ; preserve Y
        pha                             ; push Y
        tsx                             ; patch return address on stack:
        sec                             ; $7C/$7D = original PC saved by NMI.
        lda     $7C                     ; Subtract 1 because RTS adds 1.
        sbc     #$01                    ; Overwrites the stacked return address
        sta     $0105,x                 ; at SP+5/6 (behind P,A,X,Y,P).
        lda     $7D                     ; This makes the upcoming RTS
        sbc     #$00                    ; resume at the interrupted location.
        sta     $0106,x                 ; write return addr high byte
        jsr     play_sounds             ; call sound engine (safe from bank conflicts)
        pla                             ; begin register restore
        tay                             ; restore Y
        pla                             ; once again, restore X, Y & flags
        tax                             ; restore X
        pla                             ; restore A
        plp                             ; restore processor flags
        rts                             ; this is the "true" RTI

; ===========================================================================
; IRQ — MMC3 scanline IRQ entry point
; ===========================================================================
; Triggered by the MMC3 mapper when the scanline counter ($7B) reaches zero.
; Saves registers, acknowledges the IRQ, re-enables it, then dispatches
; to the handler whose address is stored at $9C/$9D (set by NMI each frame
; from the irq_vector_table indexed by game mode $78).
;
; The IRQ handlers implement mid-frame scroll splits. For example, during
; stage transitions ($78=$07), a two-IRQ chain creates a 3-strip effect:
;   1. NMI sets top strip scroll + first counter ($7B = scanline 88)
;   2. First IRQ ($C297) sets middle strip scroll, chains to second handler
;   3. Second IRQ ($C2D2) restores bottom strip scroll
; ---------------------------------------------------------------------------

IRQ:  php                               ; push processor status (IRQ entry)
        pha                             ; save registers
        txa                             ; preserve X
        pha                             ; push X
        tya                             ; preserve Y
        pha                             ; push Y
        sta     auto_walk_spawn_done    ; acknowledge MMC3 IRQ (disable)
        sta     weapon_hurt_timer_done  ; re-enable MMC3 IRQ
        jmp     (irq_handler_ptr)       ; dispatch to current handler

; ===========================================================================
; irq_gameplay_status_bar — split after HUD for gameplay area ($C152)
; ===========================================================================
; Fires after the status bar scanlines. Sets PPU scroll/nametable for the
; gameplay area below. $52 encodes the PPU coarse Y and nametable for the
; gameplay viewport. When mode $0B is active, falls through to a simpler
; reset-to-origin variant.
; ---------------------------------------------------------------------------

irq_gameplay_status_bar:  lda     screen_mode ; check game mode
        cmp     #$0B                    ; mode $0B (title)?
        beq     irq_gameplay_status_bar_branch ; → simplified scroll
        lda     PPUSTATUS               ; reset PPU latch
        lda     $52                     ; $2006 = $52:$C0
        sta     PPUADDR                 ; (sets coarse Y, nametable, coarse X=0)
        lda     #$C0                    ; PPU addr low = $C0
        sta     PPUADDR                 ; write PPU addr low byte
        lda     $52                     ; PPUCTRL: nametable from ($52 >> 2) & 3
        lsr     a                       ; merged with base $98 (NMI on, BG $1000)
        lsr     a                       ; shift right for nametable bits
        and     #$03                    ; isolate nametable bits
        ora     #$98                    ; merge with NMI-on + BG pattern $1000
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; fine X = 0, fine Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        jmp     irq_exit                ; done, exit IRQ handler

irq_gameplay_status_bar_branch:  lda     PPUSTATUS ; reset PPU latch
        lda     #$20                    ; $2006 = $20:$00
        sta     PPUADDR                 ; (nametable 0, origin)
        lda     #$00                    ; PPU addr low = $00
        sta     PPUADDR                 ; write PPU addr low byte
        lda     #$98                    ; PPUCTRL = $98 (NT 0, NMI, BG $1000)
        sta     PPUCTRL                 ; write PPUCTRL (NT 0, NMI, BG)
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_gameplay_hscroll — horizontal scroll for gameplay area ($C198)
; ===========================================================================
; Sets X scroll to $79 (the gameplay horizontal scroll position).
; If secondary split is enabled ($50 != 0), chains to status bar handler
; for an additional split at scanline $51.
; ---------------------------------------------------------------------------
irq_gameplay_hscroll:

        lda     PPUSTATUS               ; reset PPU latch
        lda     scroll_x_fine           ; X scroll = $79
        sta     PPUSCROLL               ; write gameplay X scroll
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     scroll_lock             ; secondary split enabled?
        beq     irq_jmp_exit_disable    ; no → last split
        lda     $51                     ; counter = $51 - $9F
        sec                             ; (scanlines until secondary split)
        sbc     #$9F                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for next split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_jmp_exit_disable:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_gameplay_ntswap — nametable swap after HUD ($C1C1)
; ===========================================================================
; Switches PPU to nametable 2 ($2800) at scroll origin (0,0).
; Used for vertical level layouts where the gameplay area uses a different
; nametable than the HUD. Chains to the next handler indexed by $78,
; unless secondary split overrides to mode $00 (no-op).
; ---------------------------------------------------------------------------
irq_gameplay_ntswap:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$28                    ; $2006 = $28:$00
        sta     PPUADDR                 ; (nametable 2 origin)
        lda     #$00                    ; PPU addr low = $00
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: set nametable bit 1
        ora     #$02                    ; → nametable 2
        sta     PPUCTRL                 ; write PPUCTRL with NT 2
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$B0                    ; counter = $B0 - $7B
        sec                             ; (scanlines to next split)
        sbc     irq_scanline            ; subtract base scanline count
        sta     NMI                     ; set MMC3 IRQ counter for next split
        ldx     screen_mode             ; chain index = current game mode
        lda     scroll_lock             ; secondary split?
        beq     irq_gameplay_ntswap_chain ; no → use mode index
        lda     $51                     ; if $51 == $B0, override to mode $00
        cmp     #$B0                    ; (disable further splits)
        bne     irq_gameplay_ntswap_chain ; not $B0, use mode index
        ldx     #$00                    ; X = 0 → irq_exit_disable handler
irq_gameplay_ntswap_chain:  lda     irq_vector_lo_gameplay_status,x ; chain to handler for mode X
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status,x ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_gameplay_vscroll — vertical scroll for gameplay area ($C200)
; ===========================================================================
; Sets PPU to $22C0 (nametable 0, bottom portion) with Y scroll = $B0 (176).
; Used for stages with vertical scrolling — positions the viewport to show
; the bottom part of the nametable. Optionally chains to status bar handler.
; ---------------------------------------------------------------------------
irq_gameplay_vscroll:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$22                    ; $2006 = $22:$C0
        sta     PPUADDR                 ; (nametable 0, coarse Y ≈ 22)
        lda     #$C0                    ; PPU addr low = $C0
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL from base value
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; X scroll = 0
        sta     PPUSCROLL               ; write X scroll = 0
        lda     #$B0                    ; Y scroll = $B0 (176)
        sta     PPUSCROLL               ; write Y scroll = $B0
        lda     scroll_lock             ; secondary split?
        beq     irq_jmp_exit_disable    ; no → exit disabled
        lda     $51                     ; counter = $51 - $B0
        sec                             ; set carry for subtraction
        sbc     #$B0                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for next split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_stagesel_first — stage select X scroll (mode $05)
; ===========================================================================
; Sets scroll for the upper portion of the stage select screen using the
; standard NES mid-frame $2006/$2005 trick. X scroll comes from $79.
; Chains to irq_stagesel_second after $C0 - $7B scanlines.
; ---------------------------------------------------------------------------
irq_stagesel_first:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$20                    ; $2006 = $20:coarseX
        sta     PPUADDR                 ; (nametable 0, coarse Y=0)
        lda     scroll_x_fine           ; load X scroll position
        lsr     a                       ; coarse X = $79 >> 3
        lsr     a                       ; shift right for coarse X
        lsr     a                       ; continue shift
        and     #$1F                    ; mask to 0-31
        ora     #$00                    ; coarse Y=0 (top)
        sta     PPUADDR                 ; write PPU address low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; clear nametable bits
        sta     PPUCTRL                 ; write PPUCTRL
        lda     scroll_x_fine           ; fine X scroll = $79
        sta     PPUSCROLL               ; write fine X scroll
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$C0                    ; counter = $C0 - $7B
        sec                             ; (scanlines to bottom split)
        sbc     irq_scanline            ; subtract base scanline count
        sta     NMI                     ; set MMC3 IRQ counter for next split
        lda     irq_vector_lo_stagesel_second ; chain to irq_stagesel_second ($C26F)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_stagesel_second ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_stagesel_second — stage select bottom scroll (mode $06)
; ===========================================================================
; Sets scroll for the bottom portion of the stage select screen.
; Uses nametable 0 at high coarse Y ($23xx). Last split — disables IRQ.
; ---------------------------------------------------------------------------
irq_stagesel_second:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$23                    ; $2006 = $23:coarseX
        sta     PPUADDR                 ; (nametable 0, high coarse Y)
        lda     scroll_x_fine           ; load X scroll position
        lsr     a                       ; coarse X = $79 >> 3
        lsr     a                       ; continue shift
        lsr     a                       ; continue shift
        and     #$1F                    ; mask to 0-31 tile columns
        ora     #$00                    ; coarse Y = 0 (no-op)
        sta     PPUADDR                 ; write PPU address low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; (select nametable 0)
        sta     PPUCTRL                 ; write PPUCTRL
        lda     scroll_x_fine           ; fine X scroll = $79
        sta     PPUSCROLL               ; write fine X scroll
        lda     #$C0                    ; Y scroll = $C0 (192)
        sta     PPUSCROLL               ; write Y scroll = $C0
        jmp     irq_exit_disable        ; last split

; ===========================================================================
; irq_transition_first_split — middle strip scroll (mode $07, scanline 88)
; ===========================================================================
; First IRQ handler during stage transitions. Fires at scanline $7B (88).
; Sets scroll for the MIDDLE strip (blue boss intro band, y=88-151).
;
; The top strip scrolls left via $FC/$FD (main scroll). The middle strip
; must appear FIXED, so this handler NEGATES the scroll values ($79/$7A),
; canceling the horizontal movement for the middle band.
;
; After setting the middle strip scroll, it programs the next IRQ to fire
; 64 scanlines later (at scanline 152) and chains to the second split
; handler (irq_transition_second_split at $C2D2).
; ---------------------------------------------------------------------------
irq_transition_first_split:

        lda     PPUSTATUS               ; reset PPU address latch
        lda     scroll_x_fine           ; negate scroll_x_fine/$7A (two's complement)
        eor     #$FF                    ; inverted_X = -$79
        clc                             ; clear carry for addition
        adc     #$01                    ; complete two's complement
        sta     irq_handler_ptr         ; $9C = inverted X scroll (temp)
        lda     nt_select               ; negate high byte with carry
        eor     #$FF                    ; negate high byte
        adc     #$00                    ; carry propagates the negation
        and     #$01                    ; keep only nametable bit
        sta     $9D                     ; $9D = inverted nametable select
        lda     ppu_ctrl_shadow         ; PPUCTRL: clear NT bits, set inverted NT
        and     #$FC                    ; clear nametable bits from base
        ora     $9D                     ; merge inverted nametable select
        sta     PPUCTRL                 ; → middle strip uses opposite nametable
        lda     irq_handler_ptr         ; set X scroll = negated value
        sta     PPUSCROLL               ; (cancels horizontal movement)
        lda     #$58                    ; set Y scroll = $58 (88)
        sta     PPUSCROLL               ; (top of middle band)
        lda     #$40                    ; set next IRQ at 64 scanlines later
        sta     NMI                     ; (scanline 88+64 = 152)
        lda     irq_vector_lo_transition_second ; chain to second split handler
        sta     irq_handler_ptr         ; $9C/$9D → $C2D2
        lda     irq_vector_hi_transition_second ; (irq_transition_second_split)
        sta     $9D                     ; store chain handler high byte
        jmp     irq_exit                ; exit without disabling IRQ

; ===========================================================================
; irq_transition_second_split — bottom strip scroll (mode $07, scanline 152)
; ===========================================================================
; Second IRQ handler during stage transitions. Fires at scanline 152.
; Restores scroll for the BOTTOM strip (y=152-239), which scrolls the same
; direction as the top strip (the stage select background scrolling away).
;
; Uses the $2006/$2006/$2000/$2005/$2005 sequence — the standard NES
; mid-frame scroll trick that sets both coarse and fine scroll via PPU
; address register manipulation.
; ---------------------------------------------------------------------------
irq_transition_second_split:

        lda     PPUSTATUS               ; reset PPU address latch
        lda     nt_select               ; compute PPU $2006 high byte:
        and     #$01                    ; nametable bit → bits 2-3
        asl     a                       ; ($7A & 1) << 2 | $22
        asl     a                       ; → $22 (NT 0) or $26 (NT 1)
        ora     #$22                    ; merge with nametable 0 base
        sta     PPUADDR                 ; write PPU address high byte
        lda     scroll_x_fine           ; compute PPU $2006 low byte:
        lsr     a                       ; ($79 >> 3) = coarse X scroll
        lsr     a                       ; $60 = coarse Y=12 (scanline 96)
        lsr     a                       ; continue shift for coarse X
        and     #$1F                    ; mask to 5-bit coarse X
        ora     #$60                    ; merge with coarse Y = 12
        sta     PPUADDR                 ; write PPU address low byte
        lda     nt_select               ; PPUCTRL: set nametable bits from nt_select
        and     #$03                    ; isolate nametable bits from $7A
        ora     ppu_ctrl_shadow         ; merge with base PPUCTRL shadow
        sta     PPUCTRL                 ; write PPUCTRL
        lda     scroll_x_fine           ; fine X scroll = $79 (low 3 bits used)
        sta     PPUSCROLL               ; write PPUSCROLL X
        lda     #$98                    ; Y scroll = $98 (152) — bottom strip
        sta     PPUSCROLL               ; write PPUSCROLL Y = 152
        jmp     irq_exit_disable        ; last split — disable IRQ

; ===========================================================================
; irq_wave_set_strip — water wave scroll strip (mode $09)
; ===========================================================================
; Creates a water wave effect (Gemini Man stage) by alternating X scroll
; direction across strips. $73 indexes the current strip (0-2).
;   Strip 0: X = -$69 + 1,  Y = $30 (48)
;   Strip 1: X =  $69,      Y = $60 (96)
;   Strip 2: X = -$69 + 1,  Y = $90 (144)
; Chains to irq_wave_advance after 14 scanlines.
; ---------------------------------------------------------------------------
irq_wave_set_strip:

        lda     PPUSTATUS               ; reset PPU latch
        ldy     $73                     ; Y = strip index (0-2)
        lda     $69                     ; X scroll = $69 EOR mask + offset
        eor     wave_eor_masks,y        ; masks: $FF/$00/$FF (negate strips 0,2)
        clc                             ; offsets: $01/$00/$01
        adc     wave_adc_offsets,y      ; → alternating wave scroll
        sta     PPUSCROLL               ; write wave X scroll
        lda     wave_y_scroll_set,y     ; Y scroll from table: $30/$60/$90
        sta     PPUSCROLL               ; write wave Y scroll
        lda     #$0E                    ; next IRQ in 14 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (14 scanlines)
        lda     irq_vector_lo_wave_advance ; chain to irq_wave_advance ($C32B)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_wave_advance ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_wave_advance — advance wave strip counter (mode $0A)
; ===========================================================================
; Resets X scroll to 0 between wave strips, advances $73 counter.
; If all 3 strips done, optionally chains to secondary split.
; Otherwise loops back to irq_wave_set_strip after 32 scanlines.
; ---------------------------------------------------------------------------
irq_wave_advance:

        lda     PPUSTATUS               ; reset PPU latch
        ldy     $73                     ; Y = strip index
        lda     #$00                    ; X scroll = 0 (reset between strips)
        sta     PPUSCROLL               ; write X scroll = 0
        lda     wave_y_scroll_advance,y ; Y scroll from table: $40/$70/$A0
        sta     PPUSCROLL               ; write advance Y scroll
        inc     $73                     ; advance to next strip
        lda     $73                     ; check strip count
        cmp     #$03                    ; all 3 strips done?
        beq     irq_wave_all_strips_done ; yes → finish
        lda     #$20                    ; next IRQ in 32 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (32 scanlines)
        lda     irq_vector_lo_wave_set_strip ; chain back to irq_wave_set_strip ($C302)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_wave_set_strip ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_wave_all_strips_done:  lda     #$00 ; reset strip counter
        sta     $73                     ; reset strip index to 0
        lda     scroll_lock             ; secondary split?
        beq     irq_wave_last_split     ; no → last split
        lda     $51                     ; counter = $51 - $A0
        sec                             ; set carry for subtraction
        sbc     #$A0                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for secondary split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_wave_last_split:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_title_first — title/password screen first split (mode $0B)
; ===========================================================================
; Sets scroll to $2140 (nametable 0, tile row 10) with X=0, Y=0.
; This positions the main content area of the title/password screen.
; Chains to irq_title_second after $4C (76) scanlines.
; ---------------------------------------------------------------------------
irq_title_first:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$21                    ; $2006 = $21:$40
        sta     PPUADDR                 ; (nametable 0, tile row 10)
        lda     #$40                    ; PPU addr low = $40
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; (select nametable 0)
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$4C                    ; next IRQ in 76 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (76 scanlines)
        lda     irq_vector_lo_title_cutscene ; chain to irq_title_second ($C3A3)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_title_cutscene ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_title_second — title/password screen X scroll (mode $0C)
; ===========================================================================
; Sets X scroll from $6A for the bottom portion of the title screen.
; Optionally chains to secondary split for HUD overlay.
; ---------------------------------------------------------------------------
irq_title_second:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $6A                     ; X scroll = $6A
        sta     PPUSCROLL               ; write X scroll from $6A
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     scroll_lock             ; secondary split?
        beq     irq_title_second_last_split ; no → last split
        lda     $51                     ; counter = $51 - $A0
        sec                             ; set carry for subtraction
        sbc     #$A0                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for secondary split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_title_second_last_split:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_cutscene_scroll — cutscene full scroll setup (mode $0D)
; ===========================================================================
; Full $2006/$2005 mid-frame scroll using $6A (X), $6B (nametable), $7B (Y).
; Used for cutscene/intro sequences with arbitrary scroll positioning.
; Chains to irq_cutscene_secondary after $AE - $7B scanlines.
; ---------------------------------------------------------------------------
irq_cutscene_scroll:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $6B                     ; $2006 high = ($6B << 2) | $20
        asl     a                       ; → $20 (NT 0) or $24 (NT 1)
        asl     a                       ; shift left for nametable bits
        ora     #$20                    ; merge with nametable 0 base
        sta     PPUADDR                 ; write PPU address high byte
        lda     $6A                     ; $2006 low = ($6A >> 3) | $E0
        lsr     a                       ; coarse X from $6A, high coarse Y
        lsr     a                       ; shift right for coarse X
        lsr     a                       ; continue shift
        ora     #$E0                    ; merge with high coarse Y
        sta     PPUADDR                 ; write PPU address low byte
        lda     $6A                     ; fine X scroll = $6A
        sta     PPUSCROLL               ; write PPUSCROLL X
        lda     irq_scanline            ; fine Y scroll = $7B
        sta     PPUSCROLL               ; write PPUSCROLL Y
        lda     ppu_ctrl_shadow         ; PPUCTRL: base | nametable bits from $6B
        ora     $6B                     ; merge nametable from $6B
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$AE                    ; counter = $AE - $7B
        sec                             ; (scanlines to secondary split)
        sbc     irq_scanline            ; subtract base scanline count
        sta     NMI                     ; set MMC3 IRQ counter
        lda     irq_vector_lo_chr_handlers ; chain to irq_cutscene_secondary ($C408)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_chr_handlers ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_cutscene_secondary — cutscene secondary split (mode $0E)
; ===========================================================================
; Handles the bottom portion of cutscene screens. If $50 != 0 and
; $51 - $B0 == 0, chains directly to status bar. Otherwise resets scroll
; to $22C0 (nametable 0 bottom) and optionally chains for another split.
; ---------------------------------------------------------------------------
irq_cutscene_secondary:

        lda     scroll_lock             ; secondary split enabled?
        beq     irq_cutscene_reset_scroll ; no → reset to origin
        lda     $51                     ; X = $51 - $B0
        sec                             ; (remaining scanlines)
        sbc     #$B0                    ; subtract scanline offset
        tax                             ; save remainder in X for IRQ counter
        bne     irq_cutscene_reset_scroll ; non-zero → need scroll reset
        jmp     irq_gameplay_status_bar ; zero → chain directly to status bar

irq_cutscene_reset_scroll:  lda     PPUSTATUS ; reset PPU latch
        lda     #$22                    ; $2006 = $22:$C0
        sta     PPUADDR                 ; (nametable 0, bottom portion)
        lda     #$C0                    ; PPU addr low = $C0
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; clear nametable bits (select NT 0)
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     scroll_lock             ; secondary split?
        beq     irq_cutscene_last_split ; no → last split
        stx     NMI                     ; counter = X (from $51 - $B0 above)
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_cutscene_last_split:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_chr_split_first — scroll + chain to CHR swap (mode $0F)
; ===========================================================================
; Sets X scroll from $69, then chains to irq_chr_split_swap after 48
; scanlines. First half of a two-part mid-frame CHR bank swap effect.
; ---------------------------------------------------------------------------
irq_chr_split_first:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $69                     ; X scroll = $69
        sta     PPUSCROLL               ; write X scroll from $69
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$30                    ; next IRQ in 48 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (48 scanlines)
        lda     irq_vector_lo_chr_swap  ; chain to irq_chr_split_swap ($C469)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_chr_swap  ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_chr_split_swap — scroll + mid-frame CHR bank swap (mode $10)
; ===========================================================================
; Sets X scroll from $6A with nametable from $6B, then performs a mid-frame
; CHR bank swap: swaps BG CHR to banks $66/$72, then sets up $78/$7A for
; the main loop to restore during NMI. $1B flag signals the swap occurred.
; ---------------------------------------------------------------------------
irq_chr_split_swap:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $6A                     ; X scroll = $6A
        sta     PPUSCROLL               ; write X scroll from $6A
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable from $6B
        and     #$FC                    ; clear nametable bits
        ora     $6B                     ; merge nametable from $6B
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$66                    ; swap BG CHR bank 0 → $66
        sta     $E8                     ; store to CHR bank 0 shadow
        lda     #$72                    ; swap BG CHR bank 1 → $72
        sta     $E9                     ; store to CHR bank 1 shadow
        jsr     task_yield_clear_flag   ; apply CHR bank swap via MMC3
        lda     mmc3_shadow             ; trigger MMC3 bank latch
        sta     MMC3_BANK_SELECT        ; write MMC3 bank select register
        lda     #$78                    ; set up next banks for NMI restore
        sta     $E8                     ; BG bank 0 → $78
        lda     #$7A                    ; swap BG CHR bank 1 to $7A
        sta     $E9                     ; BG bank 1 → $7A
        inc     $1B                     ; signal main loop: CHR swap occurred
        jmp     irq_exit_disable        ; last split

; ===========================================================================
; irq_chr_swap_only — CHR bank swap without scroll change (mode $11)
; ===========================================================================
; Performs a mid-frame CHR bank swap to $66/$72 without changing scroll.
; Saves and restores $E8/$E9 so the main loop's bank setup is preserved.
; Falls through to irq_exit_disable.
; ---------------------------------------------------------------------------
irq_chr_swap_only:

        lda     $E8                     ; save current CHR bank addresses
        pha                             ; save CHR bank 0 to stack
        lda     $E9                     ; load CHR bank 1 shadow
        pha                             ; save CHR bank 1 to stack
        lda     #$66                    ; temporarily swap BG CHR bank 0 → $66
        sta     $E8                     ; store to CHR bank 0 shadow
        lda     #$72                    ; temporarily swap BG CHR bank 1 → $72
        sta     $E9                     ; store to CHR bank 1 shadow
        jsr     task_yield_clear_flag   ; apply CHR bank swap via MMC3
        lda     mmc3_shadow             ; trigger MMC3 bank latch
        sta     MMC3_BANK_SELECT        ; write MMC3 bank select register
        pla                             ; restore $E9
        sta     $E9                     ; restore CHR bank 1 shadow
        pla                             ; restore $E8
        sta     $E8                     ; restore CHR bank 0 shadow
        inc     $1B                     ; signal main loop: CHR swap occurred

; --- IRQ exit routines ---
; Handlers jump here when done. Two entry points:
;   irq_exit_disable ($C4BA): disables IRQ (last split of frame)
;   irq_exit ($C4BD): keeps IRQ enabled (more splits coming)
irq_exit_disable:  sta     auto_walk_spawn_done ; disable MMC3 IRQ (no more splits)
irq_exit:  pla                          ; restore Y
        tay                             ; restore Y
        pla                             ; restore registers
        tax                             ; restore X
        pla                             ; restore A
        plp                             ; restore processor flags
        rti                             ; return from interrupt

; ===========================================================================
; irq_vector_table — handler addresses indexed by game mode ($78)
; ===========================================================================
; NMI loads $9C/$9D from these tables: low bytes at $C4C8, high at $C4DA.
; The IRQ entry point dispatches via JMP ($009C).
;
; Index | Handler  | Purpose
; ------+----------+---------------------------------------------------
;  $00  | $C4BA    | irq_exit_disable (no-op, no splits needed)
;  $01  | $C152    | irq_gameplay_status_bar — HUD/gameplay split
;  $02  | $C198    | irq_gameplay_hscroll — horizontal scroll after HUD
;  $03  | $C1C1    | irq_gameplay_ntswap — nametable swap after HUD
;  $04  | $C200    | irq_gameplay_vscroll — vertical scroll after HUD
;  $05  | $C235    | irq_stagesel_first — stage select X scroll
;  $06  | $C26F    | irq_stagesel_second — stage select bottom (chain)
;  $07  | $C297    | irq_transition_first_split — 3-strip middle band
;  $08  | $C2D2    | irq_transition_second_split — 3-strip bottom (chain)
;  $09  | $C302    | irq_wave_set_strip — water wave strip (Gemini Man)
;  $0A  | $C32B    | irq_wave_advance — wave strip loop (chain)
;  $0B  | $C375    | irq_title_first — title/password first split
;  $0C  | $C3A3    | irq_title_second — title/password X scroll (chain)
;  $0D  | $C3CC    | irq_cutscene_scroll — full $2006/$2005 scroll
;  $0E  | $C408    | irq_cutscene_secondary — secondary split (chain)
;  $0F  | $C44A    | irq_chr_split_first — scroll + chain to CHR swap
;  $10  | $C469    | irq_chr_split_swap — scroll + mid-frame CHR swap
;  $11  | $C49C    | irq_chr_swap_only — CHR bank swap (no scroll)
; ---------------------------------------------------------------------------
; 4 unused bytes (padding/alignment)

        .byte   $00,$00,$00,$00

; low bytes of handler addresses ($C4C8, 18 entries)
irq_vector_lo:  .byte   $BA             ; modes $00-$03
irq_vector_lo_gameplay_status:  .byte   $52,$98,$C1,$00,$35
irq_vector_lo_stagesel_second:  .byte   $6F,$97
irq_vector_lo_transition_second:  .byte   $D2
irq_vector_lo_wave_set_strip:  .byte   $02
irq_vector_lo_wave_advance:  .byte   $2B,$75
irq_vector_lo_title_cutscene:  .byte   $A3,$CC ; modes $0C-$11
irq_vector_lo_chr_handlers:  .byte   $08,$4A ; modes $0C-$11
irq_vector_lo_chr_swap:  .byte   $69,$9C

; high bytes of handler addresses ($C4DA, 18 entries)
irq_vector_hi:  .byte   $C4             ; modes $00-$01
irq_vector_hi_gameplay_status:  .byte   $C1,$C1,$C1,$C2,$C2 ; modes $00-$01
irq_vector_hi_stagesel_second:  .byte   $C2,$C2
irq_vector_hi_transition_second:  .byte   $C2
irq_vector_hi_wave_set_strip:  .byte   $C3
irq_vector_hi_wave_advance:  .byte   $C3,$C3 ; modes $0A-$11
irq_vector_hi_title_cutscene:  .byte   $C3,$C3
irq_vector_hi_chr_handlers:  .byte   $C4,$C4 ; modes $0A-$11
irq_vector_hi_chr_swap:  .byte   $C4,$C4

; --- water wave scroll tables (used by irq_wave_set_strip/advance) ---
; $73 indexes strip 0-2. Strips 0,2 invert X scroll; strip 1 keeps it.
wave_eor_masks:  .byte   $FF,$00,$FF    ; EOR: negate/keep/negate X scroll
wave_adc_offsets:  .byte   $01,$00,$01  ; ADC: +1/0/+1 (two's complement fixup)
wave_y_scroll_set:  .byte   $30,$60,$90 ; Y scroll for set_strip: 48/96/144
wave_y_scroll_advance:  .byte   $40,$70,$A0 ; Y scroll for advance: 64/112/160

; ===========================================================================
; PPU UTILITY ROUTINES — $C4F8-$C815
; ===========================================================================
; Contains:
;   drain_ppu_buffer      — flush PPU write buffer at $0780 to nametable
;   disable_nmi / enable_nmi — PPUCTRL NMI flag management
;   rendering_off / rendering_on — PPUMASK rendering control + nmi_skip
;   read_controllers      — read both joypads with DPCM-glitch mitigation
;   fill_nametable        — fill a PPU nametable with a single tile
;   prepare_oam_buffer    — clear unused OAM + draw overlay/scroll sprites
;   clear_entity_table    — deactivate all non-player entity slots
;   load_overlay_sprites  — copy ROM sprite data for stage transition overlays
;   scroll_overlay_sprites — slide overlay sprites leftward each frame
;   draw_scroll_sprites   — draw 8 camera-tracking sprites
;   fade_palette_out/in   — blocking palette fade effects
;   palette_fade_tick     — per-frame incremental palette fade
;   indirect_dispatch     — jump through word table using A as index
;   shift_register_tick   — 32-bit LFSR on $E4-$E7
; ===========================================================================

; ===========================================================================
; drain_ppu_buffer — write queued PPU updates from $0780 buffer
; ===========================================================================
; Called by NMI to flush pending nametable/attribute writes.
; Buffer format: [addr_hi][addr_lo][count][tile × (count+1)]...[FF=end]
; $19 is cleared (scroll dirty flag reset after PPU writes).
; ---------------------------------------------------------------------------
drain_ppu_buffer:  ldx     #$00         ; start at buffer offset 0
        stx     nametable_dirty         ; clear scroll-dirty flag
drain_ppu_buffer_continue:  lda     $0780,x ; read PPU addr high byte
        bmi     drain_ppu_exit          ; $FF terminator → exit
        sta     PPUADDR                 ; PPUADDR high
        lda     $0781,x                 ; read PPU addr low byte
        sta     PPUADDR                 ; PPUADDR low
        ldy     $0782,x                 ; Y = byte count
drain_ppu_write_tile_loop:  lda     $0783,x ; write tile data
        sta     PPUDATA                 ; to PPUDATA
        inx                             ; advance buffer index
        dey                             ; decrement byte count
        bpl     drain_ppu_write_tile_loop ; loop count+1 times
        inx                             ; skip past addr_hi
        inx                             ; addr_lo
        inx                             ; count fields
        bne     drain_ppu_buffer_continue ; next buffer entry
drain_ppu_exit:  rts

; ---------------------------------------------------------------------------
; disable_nmi — clear NMI enable + sprite bits in PPUCTRL
; ---------------------------------------------------------------------------
; Masks $FF (PPUCTRL shadow) to $11 (keep sprite table select + increment),
; clears NMI enable (bit 7) and sprite size (bit 5).
; Referenced from bank16 dispatch table at $168A68.
; ---------------------------------------------------------------------------
disable_nmi:

        lda     ppu_ctrl_shadow         ; load PPUCTRL shadow
        and     #$11                    ; keep bits 4,0 only
        sta     ppu_ctrl_shadow         ; update shadow
        sta     PPUCTRL                 ; write PPUCTRL
        rts                             ; return

; ---------------------------------------------------------------------------
; enable_nmi — set NMI enable bit in PPUCTRL
; ---------------------------------------------------------------------------
enable_nmi:

        lda     ppu_ctrl_shadow         ; load PPUCTRL shadow
        ora     #$80                    ; set bit 7 (NMI enable)
        sta     ppu_ctrl_shadow         ; update shadow
        sta     PPUCTRL                 ; write PPUCTRL
        rts                             ; return

; ---------------------------------------------------------------------------
; rendering_off — blank screen and increment frame-lock counter
; ---------------------------------------------------------------------------
; Sets PPUMASK ($2001) to $00 (all rendering off).
; $EE++ prevents task_yield from resuming entity processing.
; ppu_mask_shadow = PPUMASK shadow.
; ---------------------------------------------------------------------------
rendering_off:

        inc     nmi_skip                ; frame-lock counter++
        lda     #$00                    ; rendering disabled
        sta     ppu_mask_shadow         ; PPUMASK shadow = $00
        sta     PPUMASK                 ; all rendering off
        rts                             ; return

; ---------------------------------------------------------------------------
; rendering_on — restore screen rendering and decrement frame-lock
; ---------------------------------------------------------------------------
; Sets PPUMASK ($2001) to $18 (show background + show sprites).
; $EE-- re-enables entity processing in task_yield.
; ---------------------------------------------------------------------------
rendering_on:

        dec     nmi_skip                ; frame-lock counter--
        lda     #$18                    ; BG + sprites enabled
        sta     ppu_mask_shadow         ; PPUMASK shadow = $18
        sta     PPUMASK                 ; BG + sprites on
        rts                             ; return

; ===========================================================================
; read_controllers — read both joypads with DPCM-glitch mitigation
; ===========================================================================
; Standard NES controller read using the double-read technique: reads each
; controller via both bit 0 and bit 1 of $4016/$4017, then ORs results to
; compensate for DPCM channel interference on bit 0.
;
; After return:
;   joy1_press ($14) = player 1 new presses (edges only, not held)
;   joy1_press_alt ($15) = player 2 new presses
;   joy1_held ($16) = player 1 held (raw state this frame)
;   $17 = player 2 held
;
; Simultaneous Up+Down or Left+Right are cancelled (D-pad bits cleared).
; Called by scheduler on task slot 0 resume (main game task gets input).
; ---------------------------------------------------------------------------

read_controllers:  ldx     #$01         ; strobe controllers
        stx     JOY1                    ; (write 1 then 0 to latch)
        dex                             ; X = 0
        stx     JOY1                    ; (write 1 then 0 to latch)
        ldx     #$08                    ; 8 bits per controller
read_ctrl_bit_loop:  lda     JOY1       ; player 1: bit 0 → $14
        lsr     a                       ; bit 1 → $00 (DPCM-safe)
        rol     joy1_press              ; shift bit into joy1_press
        lsr     a                       ; shift out bit 1
        rol     temp_00                 ; shift bit into DPCM verify byte
        lda     JOY2                    ; player 2: bit 0 → $15
        lsr     a                       ; shift out bit 0
        rol     joy1_press_alt          ; shift bit into joy1_press_alt
        lsr     a                       ; shift out bit 1
        rol     $01                     ; shift bit into DPCM verify byte
        dex                             ; next controller bit
        bne     read_ctrl_bit_loop      ; loop all 8 bits
        lda     temp_00                 ; OR both reads together
        ora     joy1_press              ; to compensate for DPCM
        sta     joy1_press              ; bit-0 corruption
        lda     $01                     ; to compensate for DPCM
        ora     joy1_press_alt          ; bit-0 corruption
        sta     joy1_press_alt          ; store merged player 2 buttons

; --- edge detection: new presses = (current XOR previous) AND current ---
        ldx     #$01                    ; X=1 (P2), then X=0 (P1)
read_ctrl_edge_detect_loop:  lda     joy1_press,x ; load raw buttons this frame
        tay                             ; save raw state in Y
        eor     joy1_held,x             ; bits that changed from last frame
        and     joy1_press,x            ; AND current = newly pressed only
        sta     joy1_press,x            ; joy1_press/$15 = new presses
        sty     joy1_held,x             ; joy1_held/$17 = held (raw) for next frame
        dex                             ; next player (1 then 0)
        bpl     read_ctrl_edge_detect_loop ; loop for player 1

; --- cancel simultaneous opposite directions ---
        ldx     #$03                    ; check $14,$15,$16,$17
read_ctrl_cancel_opposite_loop:  lda     joy1_press,x ; load button state
        and     #$0C                    ; isolate Up+Down bits
        cmp     #$0C                    ; Up+Down both pressed? ($0C)
        beq     read_ctrl_clear_dpad    ; clear D-pad if both pressed
        lda     joy1_press,x            ; load button state again
        and     #$03                    ; isolate Left+Right bits
        cmp     #$03                    ; Left+Right both pressed? ($03)
        bne     read_ctrl_next_direction ; not both pressed, skip clear
read_ctrl_clear_dpad:  lda     joy1_press,x ; load button state
        and     #$F0                    ; keep A/B/Select/Start
        sta     joy1_press,x            ; store cleaned button state
read_ctrl_next_direction:  dex
        bpl     read_ctrl_cancel_opposite_loop ; loop all 4 button registers
        rts                             ; return

; ===========================================================================
; fill_nametable — fill a PPU nametable (or CHR range) with a single byte
; ===========================================================================
; Fills PPU memory starting at address A:$00 with value X.
; If A >= $20 (nametable range): writes 1024 bytes (full nametable), then
; fills 64-byte attribute table at (A+3):$C0 with value Y.
; If A < $20 (pattern table): writes Y × 256 bytes (Y pages).
;
; Input:  A = PPU address high byte ($20/$24 for nametables, $00-$1F for CHR)
;         X = fill byte (tile index for nametable, pattern data for CHR)
;         Y = attribute fill byte (nametable mode) or page count (CHR mode)
; Called from: RESET (clear both nametables), level transitions
; ---------------------------------------------------------------------------

fill_nametable:  sta     temp_00        ; save parameters
        stx     $01                     ; $00=addr_hi, $01=fill, $02=attr
        sty     $02                     ; $02 = attribute/page count
        lda     PPUSTATUS               ; reset PPU latch
        lda     ppu_ctrl_shadow         ; load PPUCTRL shadow
        and     #$FE                    ; (horizontal increment mode)
        sta     PPUCTRL                 ; write to PPUCTRL
        lda     temp_00                 ; PPUADDR = addr_hi : $00
        sta     PPUADDR                 ; set PPUADDR high byte
        ldy     #$00                    ; Y = 0 for PPUADDR low byte
        sty     PPUADDR                 ; set PPUADDR low byte
        ldx     #$04                    ; nametable? 4 pages (1024 bytes)
        cmp     #$20                    ; addr >= $20 means nametable
        bcs     fill_nametable_page_setup ; branch if nametable range
        ldx     $02                     ; CHR? Y pages (from parameter)
fill_nametable_page_setup:  ldy     #$00 ; 256 iterations per page
        lda     $01                     ; A = fill byte
fill_nametable_write_loop:  sta     PPUDATA ; write fill byte
        dey                             ; inner: 256 bytes
        bne     fill_nametable_write_loop ; loop 256 bytes per page
        dex                             ; outer: X pages
        bne     fill_nametable_write_loop ; loop X pages
        ldy     $02                     ; Y = attribute byte
        lda     temp_00                 ; if addr < $20, skip attributes
        cmp     #$20                    ; addr < $20 means pattern table
        bcc     fill_nametable_restore_x ; if addr < $20, skip attributes
        adc     #$02                    ; PPUADDR = (addr_hi+3):$C0
        sta     PPUADDR                 ; ($20→$23C0, $24→$27C0)
        lda     #$C0                    ; (carry set from CMP above)
        sta     PPUADDR                 ; set PPUADDR low byte
        ldx     #$40                    ; 64 attribute bytes
fill_nametable_attr_loop:  sty     PPUDATA ; write attribute byte
        dex                             ; next attribute byte
        bne     fill_nametable_attr_loop ; write attribute byte
fill_nametable_restore_x:  ldx     $01  ; restore X = fill byte
        rts                             ; return

; ===========================================================================
; prepare_oam_buffer — clear unused OAM sprites and draw overlays
; ===========================================================================
; Called each frame before update_entity_sprites. Hides all OAM entries from
; the current write position ($97) to end of buffer by setting Y=$F8
; (off-screen). Then dispatches to overlay sprite routines based on flags:
;   $71 != 0 → load_overlay_sprites: copy sprite data from ROM, start scroll
;   $72 != 0 → scroll_overlay_sprites: slide overlay sprites leftward
;   game_mode == 2 → draw_scroll_sprites: draw camera-tracking sprites
;
; OAM layout:
;   $0200-$022F (sprites 0-11) — reserved for overlays when $72 is active
;   $0230+ (sprites 12+) — entity sprites, starting at oam_ptr
;
; oam_ptr = OAM write index: $04 = with player, $0C = skip player, $30 = skip overlays
; scroll_lock = pause flag (nonzero = skip overlay dispatch)
; $71 = overlay init trigger (set by palette_fade_tick when fade-in completes)
; $72 = overlay scroll active (nonzero = overlays visible, preserve OAM 0-11)
; 22 callers across banks $02, $0B, $0C, $18, $1E, $1F.
; ---------------------------------------------------------------------------

prepare_oam_buffer:  lda     player_state ; state $07 = special_death:
        cmp     #PSTATE_SPECIAL_DEATH   ; force $97=$6C (keep 27 sprites,
        bne     prepare_oam_hide_sprite0 ; branch if not special death
        ldx     #$6C                    ; force $97=$6C (keep 27 sprites,
        stx     oam_ptr                 ; clear rest)
        bne     prepare_oam_start_hide  ; always branches (X nonzero)
prepare_oam_hide_sprite0:  ldx     oam_ptr ; if oam_ptr==$04 (player slot only):
        cpx     #$04                    ; hide sprite 0 (NES sprite 0 hit
        bne     prepare_oam_check_player_slot ; branch if oam_ptr != $04
        lda     #$F8                    ; hide sprite 0 (NES sprite 0 hit
        sta     $0200                   ; hide sprite 0 Y position
prepare_oam_check_player_slot:  lda     scroll_lock ; if paused (scroll_lock): clear from $97
        bne     prepare_oam_start_hide  ; skip overlay check if paused
        lda     $72                     ; if overlay active ($72): start
        beq     prepare_oam_start_hide  ; branch if no overlay active
        ldx     #$30                    ; start clearing at OAM $30
prepare_oam_start_hide:  lda     #$F8   ; Y=$F8 = off-screen (hide sprite)
prepare_oam_hide_loop:  sta     $0200,x ; write $F8 to Y byte of each
        inx                             ; advance OAM index +1
        inx                             ; advance OAM index +2
        inx                             ; advance OAM index +3
        inx                             ; advance OAM index +4
        bne     prepare_oam_hide_loop   ; loop until all 64 sprites done
        lda     scroll_lock             ; if paused: skip overlay dispatch
        bne     prepare_oam_exit        ; skip overlay dispatch if paused
        lda     $71                     ; $71: load overlay sprites from ROM
        bne     load_overlay_sprites    ; init overlay sprites
        lda     $72                     ; $72: scroll overlay sprites
        bne     scroll_overlay_sprites  ; scroll active overlays
        lda     game_mode               ; game_mode==2: draw camera-tracking sprites
        cmp     #$02                    ; game mode 2 = scroll mode
        beq     draw_scroll_sprites     ; $F8==2: draw camera-tracking sprites
prepare_oam_exit:  rts

; ===========================================================================
; clear_entity_table — reset all non-player entity slots
; ===========================================================================
; Clears entity active/type byte (ent_status,x) and palette-anim timer (ent_spawn_id,x)
; for slots 1-31 (skips slot 0 = player). Also clears $71/$72 (overlay
; sprite flags used by prepare_oam_buffer).
; Called during stage transitions, scene loads, and init sequences.
; 14 callers across banks $0B, $0C, $18, $1E.
; ---------------------------------------------------------------------------

clear_entity_table:  ldx     #$1F       ; start at slot 31
clear_entity_loop:  lda     #$00        ; clear entity type (deactivate slot)
        sta     ent_status,x            ; deactivate entity slot
        lda     #$FF                    ; $FF = palette-anim inactive
        sta     ent_spawn_id,x          ; reset spawn ID
        dex                             ; loop slots 31 down to 1
        bne     clear_entity_loop       ; (BNE: skips slot 0 = player)
        lda     #$00                    ; clear overlay sprite flags
        sta     $71                     ; $71 = load overlay trigger
        sta     $72                     ; $72 = overlay scroll active
        rts                             ; return

; --- load_overlay_sprites ---
; Copies 12 OAM entries from overlay_sprite_data ($C6D8) to OAM $0200-$022F.
; Tiles $F1/$F2, palette 2. Clears $71, sets $72 (activates scroll), $97=$30.

load_overlay_sprites:  lda     #$00     ; clear trigger flag (one-shot)
        sta     $71                     ; disable trigger flag
        ldy     #$2C                    ; Y = $2C: 12 entries (0-11)
load_overlay_copy_loop:  lda     overlay_sprite_data,y ; copy 4 OAM bytes per sprite:
        sta     $0200,y                 ; Y position
        lda     overlay_sprite_tile_table,y ; tile index
        sta     $0201,y                 ; store tile index
        lda     overlay_sprite_attr_table,y ; load attribute byte
        sta     $0202,y                 ; attribute
        lda     overlay_sprite_pos_table,y ; X position
        sta     $0203,y                 ; store X position
        dey                             ; next entry (Y -= 4)
        dey                             ; next sprite entry -1
        dey                             ; next entry (Y -= 4)
        dey                             ; next sprite entry -3
        bpl     load_overlay_copy_loop  ; loop while Y >= 0
        sty     $72                     ; Y=$FC (nonzero) → overlay scroll active
        lda     #$30                    ; $97=$30: entity sprites start at $0230
        sta     oam_ptr                 ; (12 overlay sprites reserved)
        rts                             ; return

; --- scroll_overlay_sprites ---
; Slides overlay sprites leftward (X -= 1 per frame). Sprites 0-5 scroll
; every frame; sprites 6-11 scroll every other frame ($95 bit 0 = frame parity).
; This creates a parallax-like spread effect as the sprites fly across screen.

scroll_overlay_sprites:  ldy     #$14   ; sprites 0-5: X byte offsets $03-$17
scroll_overlay_fast_loop:  lda     $0203,y ; X position -= 1
        sec                             ; set carry for subtraction
        sbc     #$01                    ; X position -= 1
        sta     $0203,y                 ; store updated X position
        dey                             ; next sprite (Y -= 4)
        dey                             ; next sprite entry -1
        dey                             ; next sprite (Y -= 4)
        dey                             ; next sprite entry -3
        bpl     scroll_overlay_fast_loop ; loop sprites 5 → 0
        lda     $95                     ; load global frame counter
        and     #$01                    ; ($95 = global frame counter)
        bne     scroll_overlay_set_oam_ptr ; skip slow set on odd frames
        ldy     #$14                    ; sprites 6-11: X byte offsets $1B-$2F
scroll_overlay_slow_loop:  lda     $021B,y ; X position -= 1 (half speed)
        sec                             ; set carry for subtraction
        sbc     #$01                    ; X position -= 1 (half speed)
        sta     $021B,y                 ; store updated X position
        dey                             ; next sprite (Y -= 4)
        dey                             ; (part of Y -= 4)
        dey                             ; next sprite (Y -= 4)
        dey                             ; (part of Y -= 4)
        bpl     scroll_overlay_slow_loop ; loop sprites 11 → 6
scroll_overlay_set_oam_ptr:  lda     #$30 ; $97=$30: entity sprites at $0230
        sta     oam_ptr                 ; set entity OAM start
        rts                             ; return to caller

; --- draw_scroll_sprites ---
; Draws 8 camera-tracking sprites when $F8==2 (screen scroll mode).
; X positions are adjusted by camera scroll offset ($F9:$FC >> 2).
; Alternates between two 8-sprite sets based on frame parity ($95 bit 0):
;   even frames: scroll_sprite_data+$00 (Y offset 0)
;   odd frames:  scroll_sprite_data+$20 (Y offset 32)
; Tile $E4 (solid fill), palette 3. Sprites written to OAM $0200-$021F.

draw_scroll_sprites:  lda     camera_x_lo ; compute camera X offset >> 2:
        sta     temp_00                 ; $00 = ($F9:$FC) >> 2
        lda     camera_screen           ; load camera screen page
        lsr     a                       ; $00 = ($F9:$FC) >> 2
        ror     temp_00                 ; rotate into result
        lsr     a                       ; shift right again
        ror     temp_00                 ; final rotate into result
        lda     $95                     ; Y = ($95 AND 1) << 5
        and     #$01                    ; even frames: Y=0, odd: Y=$20
        asl     a                       ; multiply by 32 (5 shifts)
        asl     a                       ; (part of multiply by 32)
        asl     a                       ; (part of multiply by 32)
        asl     a                       ; (part of multiply by 32)
        asl     a                       ; (part of multiply by 32)
        tay                             ; Y = sprite set offset (0 or $20)
        ldx     #$1C                    ; 8 sprites (OAM $00-$1C, 4 bytes each)
draw_scroll_sprites_loop:  lda     scroll_sprite_data,y ; Y position (from ROM table)
        sta     $0200,x                 ; store Y position in OAM
        lda     scroll_sprite_tile_table,y ; tile index
        sta     $0201,x                 ; store tile index in OAM
        lda     scroll_sprite_attr_table,y ; attribute byte
        sta     $0202,x                 ; store attribute in OAM
        lda     scroll_sprite_pos_table,y ; X position = ROM value - scroll offset
        sec                             ; prepare for subtraction
        sbc     temp_00                 ; X position = ROM value - scroll offset
        sta     $0203,x                 ; store X position in OAM
        iny                             ; advance source (Y += 4)
        iny                             ; (part of Y += 4)
        iny                             ; advance source (Y += 4)
        iny                             ; (part of Y += 4)
        dex                             ; advance dest (X -= 4)
        dex                             ; (part of X -= 4)
        dex                             ; advance dest (X -= 4)
        dex                             ; (part of X -= 4)
        bpl     draw_scroll_sprites_loop ; loop 8 sprites
        lda     #$20                    ; $97=$20: entity sprites at $0220
        sta     oam_ptr                 ; (8 scroll sprites reserved)
        rts                             ; return to caller

; overlay_sprite_data: 12 OAM entries for load_overlay_sprites
; Format: Y, tile, attr, X (4 bytes per sprite)
; Sprites 0-5: tiles $F1, palette 2 — scroll fast (every frame)
; Sprites 6-11: tiles $F2, palette 2 — scroll slow (every other frame)

overlay_sprite_data:  .byte   $58       ; sprites 0-1
overlay_sprite_tile_table:  .byte   $F1
overlay_sprite_attr_table:  .byte   $02 ; sprites 0-1
overlay_sprite_pos_table:  .byte   $28,$E0,$F1,$02,$28,$B8,$F1,$02
        .byte   $70,$20,$F1,$02,$A0,$68,$F1,$02
        .byte   $D0,$D8,$F1,$02,$D0,$90,$F2,$02
        .byte   $10,$40,$F2,$02,$58,$D0,$F2,$02
        .byte   $58,$78,$F2,$02,$80,$28,$F2,$02
        .byte   $D8,$A8,$F2,$02,$D8

; scroll_sprite_data: 16 OAM entries for draw_scroll_sprites (two 8-sprite sets)
; Format: Y, tile, attr, X (4 bytes per sprite), tile $E4, palette 3
; Set 0 (even frames): entries 0-7, set 1 (odd frames): entries 8-15
scroll_sprite_data:  .byte   $90        ; set 0, sprites 0-1
scroll_sprite_tile_table:  .byte   $E4
scroll_sprite_attr_table:  .byte   $03
scroll_sprite_pos_table:  .byte   $18,$28,$E4,$03,$20,$68,$E4,$03
        .byte   $30,$58,$E4,$03,$60,$80,$E4,$03
        .byte   $70,$10,$E4,$03,$98,$58,$E4,$03
        .byte   $C0,$80,$E4,$03,$D0,$18,$E4,$03
        .byte   $10,$A0,$E4,$03,$48,$28,$E4,$03
        .byte   $58,$40,$E4,$03,$90,$98,$E4,$03 ; set 0, sprites 0-1
        .byte   $A0,$78,$E4,$03,$D8,$30,$E4,$03
        .byte   $E0,$A0,$E4,$03,$E8,$00,$00,$00
        .byte   $00

; ===========================================================================
; fade_palette_out / fade_palette_in — blocking palette fade
; ===========================================================================
; fade_palette_out: starts at subtract=$30, step=$F0 (decreasing by $10/pass)
; fade_palette_in:  starts at subtract=$10, step=$10 (increasing by $10/pass)
; Both copy $0620 (target palette) → $0600 (active palette), subtract $0F
; per color channel, clamp to $0F (NES black). Yields 4 frames per step.
; Runs until subtract reaches $50 or wraps negative. $18 = palette-dirty flag.
; ---------------------------------------------------------------------------
fade_palette_out:  lda     #$30         ; start dark (subtract $30)
        ldx     #$F0                    ; step = -$10 (brighten each pass)
        bne     fade_start              ; always taken (X != 0)
fade_palette_in:  lda     #$10          ; start bright (subtract $10)
        tax                             ; step = +$10 (darken each pass)
fade_start:  sta     $0F                ; $0F = current subtract amount
        stx     $0D                     ; $0D = step delta per pass
        ldy     #$04                    ; 4 frames per fade step
        sty     $0E                     ; $0E = frames to yield per step
fade_copy_palette_loop:  ldy     #$1F   ; Y = 31 (all palette entries)
fade_copy_target_loop:  lda     $0620,y ; copy target → active palette
        sta     $0600,y                 ; store to active palette
        dey                             ; loop through all 32 palette entries
        bpl     fade_copy_target_loop   ; loop all 32 entries
        ldy     #$1F                    ; reset Y for subtract pass
fade_subtract_loop:  lda     $0600,y    ; subtract from each color
        sec                             ; prepare for subtraction
        sbc     $0F                     ; subtract fade amount from color
        bpl     fade_subtract_clamp     ; branch if result >= 0
        lda     #$0F                    ; clamp to $0F (NES black)
fade_subtract_clamp:  sta     $0600,y   ; store clamped color back
        dey                             ; next palette entry
        bpl     fade_subtract_loop      ; loop through all 32 entries
        sty     palette_dirty           ; Y=$FF, mark palette dirty
        lda     $0E                     ; frames to wait this step
fade_wait_frame_loop:  pha              ; wait $0E frames between fade steps
        jsr     task_yield              ; (lets NMI upload palette to PPU)
        pla                             ; restore frame counter
        sec                             ; prepare for subtraction
        sbc     #$01                    ; decrement frame counter
        bne     fade_wait_frame_loop    ; loop until all frames elapsed
        lda     $0F                     ; current subtract amount
        clc                             ; prepare for addition
        adc     $0D                     ; add step delta
        sta     $0F                     ; store new subtract amount
        cmp     #$50                    ; done when subtract reaches $50
        beq     fade_exit               ; fade complete, exit
        lda     $0F                     ; reload subtract amount
        bpl     fade_copy_palette_loop  ; continue if not wrapped negative
fade_exit:  rts                         ; return to caller

; ---------------------------------------------------------------------------
; palette_fade_tick — per-frame incremental palette fade
; ---------------------------------------------------------------------------
; Called each frame from the game loop. When $1C (fade-active flag) is set,
; copies $0620 → $0600 and subtracts $1D from BG palette entries ($0600-$060F)
; every 4th frame. $1E = step delta added to $1D each tick.
; When $1D reaches $F0: sets $72=0 (fade-out complete, rendering halted).
; When $1D reaches $50: sets $71++ (fade-in complete, gameplay resumes).
; $1C cleared when done.
; ---------------------------------------------------------------------------

palette_fade_tick:  lda     $1C         ; fade active?
        beq     fade_tick_exit          ; no → skip
        lda     $95                     ; frame counter
        and     #$03                    ; every 4th frame
        bne     fade_tick_exit          ; not 4th frame, skip
        ldy     #$1F                    ; Y = 31 (all palette entries)
fade_tick_copy_loop:  lda     $0620,y   ; copy target → active
        sta     $0600,y                 ; store to active palette
        dey                             ; loop through all 32 palette entries
        bpl     fade_tick_copy_loop     ; loop all 32 entries
        ldy     #$0F                    ; BG palette only ($0600-$060F)
fade_tick_bg_subtract_loop:  lda     $0600,y ; load current color
        sec                             ; prepare for subtraction
        sbc     $1D                     ; subtract current fade amount
        bpl     fade_tick_clamp         ; branch if result >= 0
        lda     #$0F                    ; clamp to $0F
fade_tick_clamp:  sta     $0600,y       ; store clamped color back
        dey                             ; next palette entry
        bpl     fade_tick_bg_subtract_loop ; loop through BG palette entries
        sty     palette_dirty           ; palette dirty
        lda     $1D                     ; advance fade amount
        clc                             ; prepare for addition
        adc     $1E                     ; $1E = step delta
        sta     $1D                     ; store new fade amount
        cmp     #$F0                    ; check for fully black
        beq     fade_tick_fully_black   ; $F0 = fully black
        cmp     #$50                    ; $50 = fully restored
        bne     fade_tick_exit          ; not done, continue fading
        inc     $71                     ; signal fade-in complete
        bne     fade_tick_restore_complete ; always taken (inc from nonzero)
fade_tick_fully_black:  lda     #$00    ; A = 0 for clear
        sta     $72                     ; signal rendering halted
fade_tick_restore_complete:  lda     #$00 ; A = 0 for clear
        sta     $1C                     ; clear fade-active flag
fade_tick_exit:  rts                    ; return to caller

; ---------------------------------------------------------------------------
; indirect_dispatch — jump through word table using A as index
; ---------------------------------------------------------------------------
; A = dispatch index. Reads return address from stack to find the table
; base (word table immediately follows the JSR to this routine).
; Computes table[A*2] and JMPs to that address. Preserves X, Y.
; ---------------------------------------------------------------------------
indirect_dispatch:

        stx     $0E                     ; save X, Y
        sty     $0F                     ; save Y register
        asl     a                       ; A * 2 (word index)
        tay                             ; skip past low byte
        iny                             ; +1 (return addr is 1 before table)
        pla                             ; pop return address low
        sta     temp_0C                 ; store return addr low ($0C)
        pla                             ; pop return address high
        sta     $0D                     ; store return addr high ($0D)
        lda     (temp_0C),y             ; read target addr low
        tax                             ; X = target addr low
        iny                             ; advance to high byte
        lda     (temp_0C),y             ; read target addr high
        sta     $0D                     ; store target addr high
        stx     temp_0C                 ; store target addr low ($0C)
        ldx     $0E                     ; restore X, Y
        ldy     $0F                     ; restore Y
        jmp     (temp_0C)               ; jump to target

; --- shift_register_tick ---
; 32-bit LFSR (linear feedback shift register) on $E4-$E7.
; Feedback: XOR of bit 1 of $E4 and bit 1 of $E5 → carry → ROR through
; all 4 bytes ($E4→$E5→$E6→$E7). Called once per frame from game loop.
; $E4 initialized to $88 in main_game_entry. Used for animation cycling.

shift_register_tick:  ldx     #$00      ; X = byte index (0)
        ldy     #$04                    ; 4 bytes to rotate
        lda     $E4,x                   ; feedback = ($E4 bit 1) XOR ($E5 bit 1)
        and     #$02                    ; isolate bit 1 of $E4
        sta     temp_00                 ; store in temp
        lda     $E5,x                   ; load $E5
        and     #$02                    ; isolate bit 1 of $E5
        eor     temp_00                 ; XOR → nonzero if bits differ
        clc                             ; default carry = 0 (bits match)
        beq     shift_register_rotate_loop ; skip sec if bits match
        sec                             ; carry = 1 (bits differ)
shift_register_rotate_loop:  ror     $E4,x ; rotate carry into high bit
        inx                             ; next byte
        dey                             ; loop through 4 bytes $E4-$E7
        bne     shift_register_rotate_loop ; loop until 4 bytes done
        rts                             ; return to caller

; -----------------------------------------------
; ===========================================================================
; load_stage — load room data from stage bank, set CHR banks and palettes
; ===========================================================================
; Reads room layout, metatile columns, screen connections, then calls
; bank $01's $A000 init routine to set sprite CHR banks and palettes.
;
; STAGE BANK DATA LAYOUT ($A000-$BFFF per stage):
;   $AA40,y:     room config table (1 byte/room):
;                  bits 7-6 = vertical connection ($40=down, $80=up)
;                  bit 5    = horizontal scrolling enabled
;                  bits 4-0 = screen count for this room
;   $AA60,y*2:   room pointer table (2 bytes/room):
;                  byte 0 = CHR/palette param (indexes bank $01 $A200/$A030)
;                  byte 1 = layout index (into $AA82, *20 for offset)
;   $AA80-$AA81: BG CHR bank indices ($E8/$E9)
;   $AA82+:      screen layout data (20 bytes/entry: 16 column IDs + 4 connection)
;   $AB00,y:     enemy screen number table
;   $AC00,y:     enemy X pixel position
;   $AD00,y:     enemy Y pixel position
;   $AE00,y:     enemy global entity ID
;   $AF00+:      metatile column definitions (64 bytes per column ID)
;   $B700+:      metatile CHR tile definitions (4 bytes per metatile: 2x2 CHR tiles)
;   $BF00-$BFFF: tile collision attribute table (256 bytes, 1 per metatile)
;
; CHR/PALETTE PARAM MECHANISM:
;   Each room has a param byte ($AA60 byte 0) that indexes two tables
;   in bank $01:
;     $A200 + param*2:     sprite CHR banks ($EC, $ED) for tiles $80-$FF
;     $A030 + param*8:     sprite palettes SP2-SP3 (8 bytes)
;   Params $00-$11 match stage defaults, params $12+ are alternate configs
;   used within stages (different rooms load different enemy tilesets).
;   This allows each room to have its own set of enemy graphics and palettes.
; ---------------------------------------------------------------------------

load_stage:  lda     stage_id           ; switch to stage's PRG bank
        sta     prg_bank                ; ($A000-$BFFF = stage data)
        jsr     select_PRG_banks        ; switch to stage data bank
        lda     $AA80                   ; load palette indices
        sta     $E8                     ; from stage bank
        lda     $AA81                   ; load BG CHR bank index 2
        sta     $E9                     ; store to CHR bank slot $E9
        ldx     #$07                    ; copy default palette
load_stage_copy_palette_loop:  lda     load_stage_default_palette_table,x ; to sprite palette RAM
        sta     $0610,x                 ; store to sprite palette RAM
        sta     $0630,x                 ; store to target palette mirror
        dex                             ; next palette byte
        bpl     load_stage_copy_palette_loop ; loop all 8 bytes
        lda     #$00                    ; $EA = page 0 (sprite tiles $00-$3F)
        sta     $EA                     ; shared across all stages
        lda     #$01                    ; $EB = page 1 (sprite tiles $40-$7F)
        sta     $EB                     ; shared across all stages

; --- load_room: read room data from $AA60[$2B] ---
; Called at stage start and on room transitions.
; $2B = current room/section index.
load_room:  lda     $2B                 ; X = room index * 2
        asl     a                       ; (2 bytes per room in $AA60)
        tax                             ; X = room index * 2
        lda     $AA60,x                 ; CHR/palette param from room table
        pha                             ; (saved for bank $01 $A000 call below)
        lda     $AA61,x                 ; layout index → offset into $AA82
        asl     a                       ; multiply by 20:
        asl     a                       ; *4 → $00
        sta     temp_00                 ; *16
        asl     a                       ; *16 + *4 = *20
        asl     a                       ; (20 bytes per layout entry:
        adc     temp_00                 ; 16 column IDs + 4 connection)
        tay                             ; Y = layout data offset
        ldx     #$00                    ; X = 0 (column copy index)
load_room_copy_column_ids:  lda     $AA82,y ; copy 16 metatile column IDs
        sta     $0600,x                 ; to $0600-$060F (current screen)
        sta     $0620,x                 ; and $0620-$062F (mirror)
        iny                             ; next byte in layout data
        inx                             ; next column ID
        cpx     #$10                    ; 16 column IDs per screen
        bne     load_room_copy_column_ids ; loop until all 16 copied
        ldx     #$00                    ; reset X for connection parsing
load_room_parse_connections:  lda     $AA82,y ; parse 4 screen connection bytes
        pha                             ; (up/down/left/right exits):
        and     #$80                    ; bit 7 → $0100,x (scroll direction)
        sta     $0100,x                 ; ($80=scroll, $00=warp)
        pla                             ; restore full connection byte
        and     #$7F                    ; bits 0-6 → $0108,x (target screen#)
        sta     $0108,x                 ; store target screen number
        lda     #$00                    ; clear $0104,x and $010C,x
        sta     $0104,x                 ; (unused padding/high bytes)
        sta     $010C,x                 ; clear unused high byte
        iny                             ; next connection byte
        inx                             ; next connection entry
        cpx     #$04                    ; 4 connections (U/D/L/R)
        bne     load_room_parse_connections ; loop all 4 connections
        lda     $0600                   ; first column ID from room layout
        sta     $0610                   ; placeholder for sprite palette slot
        sta     $0630                   ; placeholder for sprite palette mirror
        lda     #$01                    ; switch to bank $01 (CHR/palette tables)
        sta     prg_bank                ; bank $01 has CHR/palette lookup tables
        jsr     select_PRG_banks        ; switch to bank $01
        pla                             ; A = CHR/palette param from $AA60
        jsr     banked_A000             ; → sets $EC/$ED from $A200[param*2]
        jmp     update_CHR_banks        ; → and SP2-SP3 from $A030[param*8]

; default_sprite_palette: sprite palette 0-1 defaults (8 bytes)
; SP0: $0F(black), $0F(black), $2C(sky blue), $11(blue)
; SP1: $0F(black), $0F(black), $30(white), $37(orange)

load_stage_default_palette_table:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37

; ---------------------------------------------------------------------------
; ensure_stage_bank — switch $F5 to current stage's PRG bank if needed
; ---------------------------------------------------------------------------
; Looks up stage_to_bank[$22] and switches $F5. No-op if $F5 is already $13
; (bank $13 = fixed/shared). Preserves X and Y.
; ---------------------------------------------------------------------------
ensure_stage_bank:  txa                 ; save X
        pha                             ; on stack
        tya                             ; save Y
        pha                             ; push Y on stack
        lda     prg_bank                ; already on bank $13?
        cmp     #$13                    ; compare with bank $13
        beq     ensure_stage_bank_skip  ; yes → skip
        ldy     stage_id                ; stage index
        lda     ensure_stage_bank_table,y ; look up stage → bank
        sta     prg_bank                ; store looked-up bank
        jsr     select_PRG_banks        ; switch bank
ensure_stage_bank_skip:  pla            ; restore Y from stack
        tay                             ; transfer to Y
        pla                             ; restore X from stack
        tax                             ; transfer to X
        rts                             ; return to caller

; stage_to_bank: maps stage index ($22) → PRG bank ($F5)
; $00=Needle $01=Magnet $02=Gemini $03=Hard $04=Top $05=Snake $06=Spark $07=Shadow
; $08-$0B=Doc Robot (Needle/Gemini/Spark/Shadow stages)
; $0C-$0F=Wily fortress, $10+=special/ending

ensure_stage_bank_table:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0D,$0D,$0F
        .byte   $0D,$11,$12,$13,$10,$1E,$0E

; ===========================================================================
; main_game_entry — top-level game loop (registered as task 0 at $C8D0)
; ===========================================================================
; Called once from RESET via coroutine scheduler. Initializes system state,
; runs title/stage select (bank $18 $9009), then enters the stage init path.
; On game over or stage completion, control returns here for the next cycle.
; Registered at $1FFE99: $93/$94 = $C8D0 (coroutine entry address).
; ---------------------------------------------------------------------------
main_game_entry:
        ldx     #$BF                    ; reset stack pointer to $01BF
        txs                             ; set stack pointer
        lda     ppu_ctrl_shadow         ; sync PPUCTRL from shadow
        sta     PPUCTRL                 ; write to PPU control register
        jsr     task_yield              ; yield 1 frame (let NMI run)
        lda     #$88                    ; CHR bank config byte
        sta     $E4                     ; init shift register seed
        cli                             ; enable IRQ
        lda     #$01                    ; $9B = 1 (game active flag)
        sta     irq_enable              ; enable IRQ processing
        lda     #MUSIC_TITLE            ; play title screen music
        jsr     submit_sound_ID_D9      ; start title music
        lda     #$40                    ; $99 = $40 (initial $99,
        sta     gravity                 ; set to $55 once gameplay starts)
        lda     #$18                    ; map bank $18 to $8000-$9FFF
        sta     mmc3_select             ; set $8000 bank select
        jsr     select_PRG_banks        ; apply bank switch
        jsr     stage_select_title ; title screen / stage select (bank $18)
        lda     #HEALTH_FULL            ; full Rush Coil ammo
        sta     $A9                     ; store to ammo slot
        lda     #$02                    ; $AE = 2 $AE (display as 3)
        sta     lives                   ; set initial lives

; --- stage_reinit: re-entry point after death/boss dispatch ---
; Clears $0150-$016F (entity scratch buffer), then falls through to stage_init.
game_entry_stage_reinit:  lda     #$00  ; clear $0150-$016F (32 bytes)
        ldy     #$1F                    ; Y = $1F (32 bytes to clear)
game_entry_clear_scratch_buffer:  sta     $0150,y ; clear scratch buffer byte
        dey                             ; next byte
        bpl     game_entry_clear_scratch_buffer ; loop until all cleared

; --- stage_init: set up a new stage ---
stage_init:  lda     #$00               ; $EE = 0: allow NMI rendering
        sta     nmi_skip                ; allow NMI rendering during setup

; --- initialize display and entity state ---
        jsr     fade_palette_in         ; fade screen to black
        lda     #$04                    ; $97 = 4 (OAM scan start offset)
        sta     oam_ptr                 ; OAM scan start offset = 4
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; zero all 32 entity slots
        lda     #$01                    ; $A000 = 1 (H-mirroring)
        sta     MMC3_MIRRORING          ; set horizontal mirroring

; --- zero-init game state variables ---
        lda     #$00                    ; A = 0 for bulk clear
        sta     game_mode               ; scroll mode = 0 (normal)
        sta     ent_status              ; player entity inactive
        sta     camera_x_hi             ; nametable select = 0
        sta     $71                     ; overlay init trigger = 0
        sta     $72                     ; overlay scroll active = 0
        sta     camera_x_lo             ; camera X low = 0
        sta     scroll_y                ; scroll Y offset = 0
        sta     camera_screen           ; starting screen = 0
        sta     $25                     ; camera X coarse = 0
        sta     ent_x_scr               ; player X screen = 0
        sta     ent_y_scr               ; player Y screen = 0
        sta     $B1                     ; HP bar state = 0
        sta     $B2                     ; boss bar state = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     $9E                     ; scroll X fine = 0
        sta     $9F                     ; scroll X fine copy = 0
        sta     boss_active             ; boss active flag = 0
        sta     current_weapon          ; weapon = $00 (Mega Buster)
        sta     $B4                     ; ammo slot index = 0
        sta     weapon_cursor           ; weapon menu cursor = 0
        sta     $6F                     ; current screen = 0

; --- start stage music ---
        ldy     stage_id                ; Y = stage number
        lda     frame_loop_stage_music_table,y ; load stage music ID
        jsr     submit_sound_ID_D9      ; play music

; --- set initial scroll/render state ---
        lda     #FACING_RIGHT           ; initial facing = right
        sta     player_facing           ; player facing = right (1=R, 2=L)
        sta     $23                     ; scroll column state = 1
        sta     $2E                     ; scroll direction = right
        lda     #$FF                    ; $29 = $FF (render-done sentinel;
        sta     $29                     ; BEQ loops while 0, skips on $FF)
        lda     #$1F                    ; $24 = 31 (column counter for
        sta     $24                     ; initial nametable fill)

; --- fill nametable: render columns until $29 becomes nonzero ---
game_entry_render_columns_loop:  lda     #$01 ; $10 = 1 (render direction: right)
        sta     $10                     ; render direction = right
        jsr     do_render_column        ; render one BG column
        jsr     task_yield              ; yield to NMI (display frame)
        lda     $29                     ; check if rendering complete
        beq     game_entry_render_columns_loop ; still zero — keep rendering

; --- parse room 0 config from $AA40 ---
        lda     stage_id                ; switch to stage data bank
        sta     prg_bank                ; store stage bank number
        jsr     select_PRG_banks        ; apply bank switch
        lda     $AA40                   ; room 0 config byte
        pha                             ; save for screen count
        and     #$E0                    ; bits 7-5: scroll/connection flags
        sta     $2A                     ; $2A = room scroll/connection flags
        ldx     #$01                    ; default: X=1 (H-mirror), Y=$2A
        ldy     #$2A                    ; default viewport height (h-mirror)
        and     #$C0                    ; bits 7-6: vertical connection
        beq     game_entry_set_mirroring ; if no vertical connection: H-mirror
        dex                             ; else: X=0 (V-mirror), Y=$26
        ldy     #$26                    ; reduced viewport height (v-mirror)
game_entry_set_mirroring:  stx     MMC3_MIRRORING ; set mirroring mode
        sty     $52                     ; $52 = nametable height ($2A or $26)
        pla                             ; bits 4-0 of room config
        and     #$1F                    ; = screen count
        sta     $2C                     ; $2C = screen count for room 0
        lda     #$00                    ; A = 0 for room/screen init
        sta     $2B                     ; $2B = current room = 0
        sta     $2D                     ; $2D = room base screen = 0

; --- load stage data (palettes, CHR, nametable attributes) ---
        jsr     load_stage              ; load stage palettes + CHR banks
        lda     #$00                    ; $18 = 0 (rendering disabled during setup)
        sta     palette_dirty           ; suppress palette upload during setup
        jsr     task_yield              ; yield to NMI
        jsr     fade_palette_out        ; fade from black → stage palette
        lda     #$80                    ; player X = $80 (128, center of screen)
        sta     ent_x_px                ; player X = 128 (center of screen)

; --- set HP and scroll mode for stage type ---
game_entry_set_hp_scroll:  lda     #HEALTH_FULL ; (full HP: 28 bars)
        sta     player_hp               ; full HP = 28 bars
        lda     #$E8                    ; default: $51/$5E = $E8 (normal scroll)
        sta     $51                     ; scroll end marker (normal)
        sta     $5E                     ; scroll end marker copy
        lda     camera_screen           ; if not starting at screen 0: skip
        bne     game_entry_set_intro_chr ; not screen 0, skip Gemini
        lda     stage_id                ; stages $02 (Gemini) and $09 (DR-Gemini)
        cmp     #STAGE_GEMINI           ; use horizontal scroll mode
        beq     game_entry_gemini_scroll ; (start scrolling right from screen 0)
        cmp     #STAGE_DOC_GEMINI       ; check Doc Robot Gemini
        bne     game_entry_set_intro_chr ; not Gemini → skip scroll override
game_entry_gemini_scroll:  lda     #$9F ; $5E = $9F (Gemini scroll end marker)
        sta     $5E                     ; store Gemini scroll end
        lda     #$02                    ; $F8 = 2 (screen scroll mode)
        sta     game_mode               ; game mode = auto-scroll

; --- set intro CHR banks and begin fade-in ---
game_entry_set_intro_chr:  lda     #$74 ; $EA/$EB = $74/$75 (intro/ready CHR pages)
        sta     $EA                     ; store intro CHR page $EA
        lda     #$75                    ; intro CHR page $75
        sta     $EB                     ; store intro CHR page $EB
        jsr     update_CHR_banks        ; apply CHR bank settings
        lda     #$30                    ; $0611 = $30 (BG palette color for intro)
        sta     $0611                   ; store BG palette entry
        inc     palette_dirty           ; enable rendering (palette_dirty = 1)
        lda     #$3C                    ; A = 60 (fade-in frame count)

; --- fade-in loop: 60 frames with flashing "READY" overlay ---
game_entry_fadein_loop:  pha            ; save frame countdown
        lda     $95                     ; frame counter bit 4:
        and     #$10                    ; toggles every 16 frames
        beq     game_entry_hide_overlay ; if clear → hide overlay

; show "READY" overlay: copy 5 OAM entries from $CCF8 table
        ldx     #$10                    ; X = $10 (5 sprites × 4 bytes - 4)
game_entry_show_ready_overlay:  lda     frame_loop_ready_overlay_data,x ; copy OAM entry: Y, tile, attr, X
        sta     $0200,x                 ; store OAM Y position
        lda     rush_coil_dispatch_table,x ; OAM tile index
        sta     $0201,x                 ; store OAM tile index
        lda     rush_marine_dispatch_table,x ; load OAM attribute byte
        sta     $0202,x                 ; store OAM attribute byte
        lda     rush_jet_dispatch_table,x ; load OAM X position
        sta     $0203,x                 ; OAM X position
        dex                             ; prev OAM entry (X -= 4)
        dex                             ; (part of X -= 4)
        dex                             ; (part of X -= 4)
        dex                             ; next OAM entry
        bpl     game_entry_show_ready_overlay ; loop all 5 sprites
        lda     #$14                    ; A = 20 (start hiding at sprite 5)

; hide remaining OAM entries: set Y = $F8 (off-screen) from offset A onward
game_entry_hide_overlay:  sta     oam_ptr ; oam_ptr = OAM scan start offset
        tax                             ; X = OAM offset
        lda     #$F8                    ; Y = $F8 = off-screen
game_entry_hide_oam_loop:  sta     $0200,x ; hide sprite: Y = $F8
        inx                             ; advance to next OAM entry
        inx                             ; (part of X += 4)
        inx                             ; (part of X += 4)
        inx                             ; next OAM entry (4 bytes)
        bne     game_entry_hide_oam_loop ; until X wraps to 0
        lda     #$00                    ; $EE = 0 (allow NMI rendering)
        sta     nmi_skip                ; allow NMI rendering
        jsr     task_yield              ; yield to NMI (display frame)
        inc     nmi_skip                ; nmi_skip = 1 (suppress next NMI rendering)
        inc     $95                     ; advance frame counter
        pla                             ; decrement fade-in countdown
        sec                             ; prepare for subtraction
        sbc     #$01                    ; decrement countdown
        bne     game_entry_fadein_loop  ; loop until 60 frames complete

; --- fade-in complete: set up gameplay CHR and spawn player ---
        lda     #$0F                    ; $0611 = $0F (BG color → black)
        sta     $0611                   ; store BG palette entry
        inc     palette_dirty           ; mark palette dirty for upload
        lda     #$00                    ; $EA/$EB = pages 0/1 (shared sprite CHR)
        sta     $EA                     ; R2: tiles $00-$3F at $1000
        lda     #$01                    ; R3: tiles $40-$7F at $1400
        sta     $EB                     ; store CHR page $EB
        jsr     update_CHR_banks        ; switch to gameplay sprite CHR

; --- initialize player entity (slot 0) for teleport-in ---
        lda     #$80                    ; ent_status = $80 (player entity active)
        sta     ent_status              ; player entity = active
        lda     #$00                    ; ent_y_px = 0 (player Y = top of screen)
        sta     ent_y_px                ; start at top of screen
        lda     #$D0                    ; ent_flags = $D0 (bit7=drawn, bit6=face right, bit4=set)
        sta     ent_flags               ; store player flags
        lda     #$4C                    ; player X speed = $01.4C (walk speed, pre-loaded
        sta     ent_xvel_sub            ; for when reappear transitions to on_ground)
        lda     #$01                    ; X velocity whole = $01
        sta     ent_xvel                ; store X velocity whole
        lda     #$00                    ; player Y speed = $F9.00 (terminal fall velocity)
        sta     ent_yvel_sub            ; player drops from top during reappear state
        lda     #$F9                    ; Y velocity = $F9 (fast downward)
        sta     ent_yvel                ; terminal fall speed for teleport-in
        ldx     #$00                    ; slot 0 (player): set OAM $13 = teleport beam
        lda     #$13                    ; OAM ID $13 = teleport beam sprite
        jsr     reset_sprite_anim       ; set teleport beam animation
        lda     #PSTATE_REAPPEAR        ; $30 = $04 (player state = reappear)
        sta     player_state            ; set to reappear state
        lda     #$80                    ; $B2 = $80 (stage initialization complete)
        sta     $B2                     ; mark stage init complete

; ===========================================================================
; gameplay_frame_loop — main per-frame game loop
; ===========================================================================
; Runs once per frame during active gameplay. Each iteration:
;   1. Check Start button for pause/weapon menu
;   2. Run player state machine (dispatch by $30)
;   3. Update camera scroll
;   4. Process all entity AI (banks $1C/$1D)
;   5. Spawn new enemies (bank $1A + stage bank)
;   6. Run per-frame subsystems (bank $09: HUD, scroll, sound, etc.)
;   7. Fade palette, build OAM, yield to NMI
;   8. Check exit conditions ($59=boss done, $3C=death, $74=stage clear)
; ---------------------------------------------------------------------------
gameplay_frame_loop:  lda     joy1_press ; Start button pressed?
        and     #BTN_START              ; ($14 = new button presses this frame)
        beq     gameplay_no_pause       ; no Start press → skip pause logic
        lda     player_state            ; skip pause if player state is:
        cmp     #PSTATE_REAPPEAR        ; $04 = reappear
        beq     gameplay_no_pause       ; reappearing → can't pause
        cmp     #PSTATE_SPECIAL_DEATH   ; $09+ = boss_wait and above
        beq     gameplay_no_pause       ; special death, skip pause
        cmp     #PSTATE_BOSS_WAIT       ; check boss wait state
        bcs     gameplay_no_pause       ; state >= boss_wait, skip

; --- pause check: despawn Rush (slot 1) when switching from Rush weapon ---
        lda     current_weapon          ; weapon ID - 6: Rush weapons are $06-$0B
        sec                             ; carry clear = weapon < $06 (not Rush)
        sbc     #$06                    ; subtract 6 (Rush range)
        bcc     frame_loop_check_active_weapons ; weapon < 6, not Rush
        and     #$01                    ; odd result = Rush entity active ($07,$09,$0B)
        beq     frame_loop_check_active_weapons ; even = Rush item select, not active
        lda     #$00                    ; despawn slot 1 (Rush entity)
        sta     $0301                   ; before opening pause menu
        beq     game_entry_check_pause  ; always branches (A=0)
frame_loop_check_active_weapons:  lda     $0301 ; if ANY weapon slot (1-3) is active,
        ora     $0302                   ; don't allow pause
        ora     $0303                   ; (prevents pausing mid-shot)
        bmi     gameplay_no_pause       ; active projectile → can't pause
game_entry_check_pause:  ldy     player_state ; check per-state pause permission table
        lda     frame_loop_pause_permission,y ; bit 7 set = pause not allowed
        bmi     gameplay_no_pause       ; pause not allowed, skip
        lda     #$02                    ; switch to bank $02/$03 (pause menu code)
        sta     prg_bank                ; store bank number
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_A003             ; call pause menu handler
gameplay_no_pause:  lda     stage_id    ; switch to stage bank
        sta     prg_bank                ; stage bank for state dispatch
        jsr     select_PRG_banks        ; apply bank switch
        jsr     prelude_check_slide_speed ; player state dispatch + physics

; --- apply pending hazard state transition (set by tile collision) ---
        lda     hazard_pending          ; hazard_pending = pending state from hazard
        beq     frame_loop_update_camera ; 0 = no pending state
        sta     player_state            ; apply: set player state
        cmp     #$0E                    ; state $0E = spike/pit death?
        bne     frame_loop_clear_hazard_pending ; don't allow pause
        lda     #SNDCMD_STOP            ; play death jingle ($F2 = stop music)
        jsr     submit_sound_ID         ; stop current music
        lda     #SFX_DEATH              ; play death sound $17
        jsr     submit_sound_ID         ; play death sound effect
frame_loop_clear_hazard_pending:  lda     #$00 ; clear pending state
        sta     hazard_pending          ; hazard consumed
frame_loop_update_camera:  jsr     update_camera ; scroll/camera update
        lda     camera_x_lo             ; $25 = camera X (coarse)
        sta     $25                     ; sync coarse scroll from camera
        sta     temp_00                 ; also store in temp $00
        lda     game_mode               ; if scroll mode (game_mode==2):
        cmp     #$02                    ; compute camera offset for
        bne     frame_loop_compute_scroll_offset ; not auto-scroll mode → skip
        lda     camera_screen           ; load camera screen page
        lsr     a                       ; shift right (divide by 2)
        ror     temp_00                 ; rotate screen:scroll into $00
        lsr     a                       ; shift right again
        ror     temp_00                 ; shift right again
        lda     temp_00                 ; result = camera offset / 4
        sta     $5F                     ; $5F = camera offset / 4
frame_loop_compute_scroll_offset:  lda     ent_x_scr ; track max screen progress
        cmp     $6F                     ; ($6F = farthest screen reached)
        bcc     frame_loop_track_screen_progress ; haven't passed $6F yet → skip store
        sta     $6F                     ; update farthest screen
frame_loop_track_screen_progress:  lda     ent_x_px ; player X pixel position
        sta     $27                     ; store to $27 copy
        ldx     #$1C                    ; select banks $1C/$1D
        stx     mmc3_select             ; (entity processing code)
        inx                             ; X = $1D (bank pair high)
        stx     prg_bank                ; store PRG bank number
        jsr     select_PRG_banks        ; switch to entity AI banks
        jsr     banked_8000             ; process all entity AI
        lda     #$1A                    ; bank $1A = enemy spawner
        sta     mmc3_select             ; set $8000 bank to $1A
        lda     stage_id                ; load stage bank number
        sta     prg_bank                ; store stage PRG bank
        jsr     select_PRG_banks        ; apply bank switch
        jsr     check_new_enemies       ; spawn enemies for current screen
        lda     #$09                    ; select bank $09
        sta     mmc3_select             ; (per-frame subsystems)
        jsr     select_PRG_banks        ; switch to per-frame subsystem bank
        jsr     banked_8003             ; bank $09 subsystems:
        jsr     banked_8006             ; screen scroll, HUD update,
        jsr     banked_800F             ; sound processing,
        jsr     banked_8009             ; background animation,
        jsr     banked_800C             ; item pickup,
        jsr     banked_8000             ; checkpoint tracking,
        jsr     banked_8012             ; etc.
        jsr     palette_fade_tick       ; update palette fade animation
        jsr     process_frame_yield_with_player ; build OAM + yield 1 frame
; --- DEBUG (shipped in retail) — controller 2 left-press latches right-held ---
; Pressing Left on controller 2 ($98 bit 1) permanently ORs the Right flag
; ($17 bit 0) into the P2 held state. This is necessary because the NES d-pad
; physically cannot report Left+Right simultaneously. Once latched, the debug
; effects (super jump, pit death immunity) persist for the rest of the stage.
        lda     $98                     ; P2 newly-pressed buttons
        and     #$02                    ; bit 1 = Left pressed?
        beq     frame_loop_debug_flag_check ; no → skip
        lda     #$01                    ; latch Right-held flag
        ora     $17                     ; into P2 held state
        sta     $17                     ; store updated P2 state
frame_loop_debug_flag_check:  jsr     shift_register_tick ; LFSR animation tick
        lda     $59                     ; $59 != 0: boss defeated
        bne     frame_loop_boss_defeated ; boss defeated → handle post-boss
        lda     $3C                     ; $3C != 0: player died
        bne     death_handler           ; player died, handle death
        lda     $74                     ; $74 != 0: stage clear
        bne     stage_clear_handler     ; stage clear → handle completion
        jmp     gameplay_frame_loop     ; loop back for next frame

; --- boss_defeated_handler ($59 != 0) ---

frame_loop_boss_defeated:  lda     #$00 ; clear rendering/overlay/scroll state
        sta     nmi_skip                ; allow NMI rendering
        sta     $71                     ; clear overlay init trigger
        sta     $72                     ; clear overlay scroll
        sta     game_mode               ; reset game mode
        sta     boss_active             ; clear boss-active flag
        lda     #$18                    ; select banks $18/$10
        sta     mmc3_select             ; bank $18 for post-defeat dispatch
        lda     #$10                    ; bank $10 for post-defeat
        sta     prg_bank                ; bank $10 = boss post-defeat code
        jsr     select_PRG_banks        ; switch banks
        jsr     stage_select_rm_intro ; boss post-defeat (bank $10)
        jmp     game_entry_stage_reinit ; → stage_reinit

; --- death_handler ($3C != 0) ---

death_handler:  lda     #$00            ; clear death flag + boss state
        sta     $3C                     ; clear death flag
        sta     boss_active             ; clear boss active flag
        lda     lives                   ; decrement lives (BCD format)
        sec                             ; subtract 1 life (BCD)
        sbc     #$01                    ; subtract one life
        bcc     game_over               ; underflow → game over
        sta     lives                   ; store decremented lives
        and     #$0F                    ; wraps to $0F, subtract 6
        cmp     #$0F                    ; ($10 - $01 = $0F → $09)
        bne     frame_loop_bcd_lives_correction ; low nibble valid → no BCD fix needed
        lda     lives                   ; reload lives for BCD correction
        sec                             ; prepare for BCD fix
        sbc     #$06                    ; subtract 6 for BCD adjust
        sta     lives                   ; store corrected lives
frame_loop_bcd_lives_correction:  lda     #$00 ; clear rendering/overlay state
        sta     nmi_skip                ; allow NMI rendering
        sta     $71                     ; clear overlay trigger
        sta     $72                     ; clear overlay scroll
        sta     game_mode               ; clear game mode
        lda     stage_select_page       ; if stage_select_page == $12 (Wily stage):
        cmp     #$12                    ; special respawn path
        beq     frame_loop_wily_respawn ; Wily stage, special respawn
        jmp     handle_checkpoint       ; normal respawn at checkpoint

; Wily stage death → bank $18 $9006 (Wily-specific respawn)

frame_loop_wily_respawn:  lda     #$18  ; select bank $18
        sta     mmc3_select             ; bank $18 for Wily respawn
        jsr     select_PRG_banks        ; switch banks
        jmp     stage_select_wily_gate ; Wily respawn (bank $18)

; --- game_over ($AE underflowed) ---

game_over:  lda     #$00                ; clear all state
        sta     nmi_skip                ; clear NMI skip flag
        sta     boss_active             ; clear boss active flag
        sta     $71                     ; clear overlay trigger
        sta     $72                     ; clear overlay scroll
        sta     game_mode               ; clear game mode
        lda     #$18                    ; select banks $18/$13
        sta     mmc3_select             ; (game over screen)
        lda     #$13                    ; bank $13 for game over
        sta     prg_bank                ; bank $13 = game over handler
        jsr     select_PRG_banks        ; apply bank switch
        jsr     stage_select_password ; game over sequence (bank $18)
        jmp     game_entry_stage_reinit ; → stage_reinit (continue/retry)

; --- stage_clear_handler ($74 != 0: stage completion triggered) ---
; $74 bit 7 distinguishes: 0 = normal stage clear, 1 = special/Wily clear.

stage_clear_handler:  pha               ; save $74 (stage clear type)
        lda     #$00                    ; clear game state for transition
        sta     boss_active             ; boss active = 0
        sta     $74                     ; stage clear trigger = 0
        sta     $B1                     ; HP bar = 0
        sta     $B2                     ; boss bar = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     boss_active             ; boss active = 0 (redundant)
        sta     camera_screen           ; starting screen = 0
        sta     game_mode               ; scroll mode = 0
        lda     #$0B                    ; switch to banks $0B/$0E
        sta     mmc3_select             ; (bank $0B → $8000)
        lda     #$0E                    ; bank $0E for clear handler
        sta     prg_bank                ; store PRG bank number
        jsr     select_PRG_banks        ; switch banks
        pla                             ; $74 AND $7F: 0 = normal stage clear
        and     #$7F                    ; mask off high bit
        bne     frame_loop_special_clear_check ; nonzero = special clear (Wily/Doc Robot)
        jsr     banked_8000             ; normal stage clear sequence (bank $0E)
        jmp     game_entry_stage_reinit ; → stage_reinit

; --- special stage clear (Wily/Doc Robot stages) ---

frame_loop_special_clear_check:  lda     $75 ; $75 = stage clear sub-type
        cmp     #$06                    ; $06 = final boss / ending
        beq     frame_loop_ending_sequence ; $06 = ending → special path
        jsr     banked_8003             ; Wily/Doc Robot clear (bank $0E)
        jmp     game_entry_stage_reinit ; → stage_reinit

; --- game ending sequence ---

frame_loop_ending_sequence:  lda     #$0C ; switch to banks $0C/$0E
        sta     mmc3_select             ; (bank $0C → $8000)
        lda     #$0E                    ; bank $0E for ending
        sta     prg_bank                ; store PRG bank number
        jsr     select_PRG_banks        ; switch banks
        lda     #$11                    ; $F8 = $11 (ending game mode)
        sta     game_mode               ; set ending game mode
        jsr     banked_8000             ; run ending sequence
        jmp     game_entry_stage_reinit ; → stage_reinit (title screen)

handle_checkpoint:  lda     stage_id    ; store current level
        sta     prg_bank                ; as $A000-$BFFF bank
        jsr     select_PRG_banks        ; and swap banks to it
        ldy     #$00                    ; loop through checkpoint data
frame_loop_find_checkpoint:  lda     $6F ; if current checkpoint >
        cmp     $AAF8,y                 ; current screen ID
        bcc     frame_loop_checkpoint_boundary ; we're done & 1 too far
        iny                             ; next checkpoint entry
        bne     frame_loop_find_checkpoint ; next checkpoint
frame_loop_checkpoint_boundary:  dey    ; we're one checkpoint too far so
        bpl     frame_loop_checkpoint_restore ; go back one, if it's negative
        jmp     stage_init              ; we're just at the beginning

; --- checkpoint_restore: reset game state and reload from checkpoint ---

frame_loop_checkpoint_restore:  tya     ; save checkpoint index
        pha                             ; save checkpoint index
        jsr     fade_palette_in         ; fade screen to black
        lda     #$04                    ; $97 = 4 (OAM scan start)
        sta     oam_ptr                 ; store OAM scan start
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; zero all entity slots

; --- zero game state (same as stage_init, minus music/facing) ---
        lda     #$00                    ; A = 0 for bulk clear
        sta     game_mode               ; scroll mode = 0
        sta     camera_x_hi             ; nametable select = 0
        sta     $71                     ; overlay init trigger = 0
        sta     $72                     ; overlay scroll active = 0
        sta     camera_x_lo             ; camera X low = 0
        sta     scroll_y                ; scroll Y offset = 0
        sta     $25                     ; camera X coarse = 0
        sta     ent_y_scr               ; player Y screen = 0
        sta     $B1                     ; HP bar state = 0
        sta     $B2                     ; boss bar state = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     boss_active             ; boss active flag = 0
        sta     current_weapon          ; weapon = $00 (Mega Buster)
        sta     $B4                     ; ammo slot index = 0
        sta     weapon_cursor           ; weapon menu cursor = 0

; --- restore checkpoint position data ---
        pla                             ; restore checkpoint index
        tay                             ; Y = checkpoint index
        lda     $AAF8,y                 ; set screen from checkpoint table
        sta     $29                     ; $29 = render sentinel/screen
        sta     camera_screen           ; camera_screen = starting screen
        sta     ent_x_scr               ; player X screen
        lda     $ABC0,y                 ; set scroll fine position from checkpoint
        sta     $9E                     ; scroll fine X position
        sta     $9F                     ; scroll fine X copy
        ldx     $AAF0,y                 ; $2B = checkpoint room number
        stx     $2B                     ; store room number

; --- parse room config bits (same logic as stage_init) ---
        lda     $AA40,x                 ; load room config byte
        pha                             ; save for screen count later
        sta     temp_00                 ; save config to temp
        and     #$20                    ; bit 5 = horizontal scroll?
        sta     $2A                     ; if set, $2A = $20 (h-scroll flags)
        bne     frame_loop_checkpoint_mirroring ; h-scroll set, skip v-scroll
        lda     temp_00                 ; else $2A = bits 7-6 (vertical connection)
        and     #$C0                    ; bits 7-6 = vertical connection
        sta     $2A                     ; store v-scroll connection flags
frame_loop_checkpoint_mirroring:  ldx     #$01 ; default: H-mirror, viewport height $2A
        ldy     #$2A                    ; default viewport height (h-mirror)
        and     #$20                    ; if h-scroll (bit 5 set): keep defaults
        bne     frame_loop_checkpoint_set_height ; h-scroll → keep H-mirror defaults
        dex                             ; else: V-mirror (X=0), viewport $26
        ldy     #$26                    ; V-mirror viewport height $26
frame_loop_checkpoint_set_height:  stx     MMC3_MIRRORING ; set nametable mirroring (0=V, 1=H)
        sty     $52                     ; $52 = viewport height ($2A or $26)
        pla                             ; $2C = screen count (lower 5 bits)
        and     #$1F                    ; mask to 5-bit count
        sta     $2C                     ; store screen count
        lda     #$00                    ; $2D = 0 (scroll progress within room)
        sta     $2D                     ; $2D = 0 (room base screen)
        ldy     stage_id                ; load stage music ID from $CD0C table
        lda     frame_loop_stage_music_table,y ; and start playing it
        jsr     submit_sound_ID_D9      ; start stage music
        lda     #$01                    ; A = 1 for facing/flags
        sta     player_facing           ; player_facing = face right
        sta     $23                     ; $23 = column render flag
        sta     $2E                     ; $2E = render dirty flag
        dec     $29                     ; $29-- (back up screen sentinel for render)
        lda     #$1F                    ; $24 = $1F (screen attribute pointer)
        sta     $24                     ; 31 columns per nametable fill

; --- render 33 columns of room tiles (one per frame, yielding to NMI) ---
        lda     #$21                    ; 33 columns ($21)
frame_loop_render_columns_loop:  pha    ; save column counter
        lda     #$01                    ; render direction = right
        sta     $10                     ; $10 = 1 (render one column)
        jsr     do_render_column        ; render current column
        jsr     task_yield              ; yield (NMI uploads to PPU)
        pla                             ; restore column counter
        sec                             ; decrement counter
        sbc     #$01                    ; decrement column count
        bne     frame_loop_render_columns_loop ; loop until all 33 done
        jsr     load_stage              ; apply CHR/palette for this room
        lda     #$00                    ; suppress palette upload this frame
        sta     palette_dirty           ; suppress palette upload this frame
        jsr     task_yield              ; yield to let NMI run
        jsr     fade_palette_out        ; fade from black → stage palette
        lda     #$80                    ; spawn player at Y=$80 (mid-screen)
        sta     ent_x_px                ; player X = 128 (center of screen)
        jmp     game_entry_set_hp_scroll ; → continue stage_init (Gemini scroll, fade-in)

; --- refill_all_ammo: fill all weapon ammo to HEALTH_FULL ---
; Called when $98 mode indicates refill (e.g. E-Tank usage).

        lda     $98                     ; load P2 button state
        and     #$03                    ; check if mode == 1 (refill)
        cmp     #$01                    ; mode 1 = refill
        bne     frame_loop_ammo_refill_exit ; if not, skip
        ldy     #$0B                    ; fill $A2-$AD (12 ammo slots)
frame_loop_ready_overlay_oam:  lda     #HEALTH_FULL ; all to full (28 units)
frame_loop_ammo_refill_loop:  .byte   $99
frame_loop_ready_sprite_table:  ldx     #$00 ; (encoded: part of sta $A2,y)
        dey                             ; next ammo slot
        bpl     frame_loop_ammo_refill_loop ; loop until all 12 filled
frame_loop_ammo_refill_exit:  .byte   $60
frame_loop_ready_overlay_data:  .byte   $80
rush_coil_dispatch_table:  .byte   $40
rush_marine_dispatch_table:  .byte   $00
rush_jet_dispatch_table:  .byte   $6C,$80,$41,$00,$74,$80,$42,$00
        .byte   $7C,$80,$43,$00,$84,$80,$44,$00
        .byte   $8C
frame_loop_stage_music_table:           ; stage music IDs (indexed by stage_id)
        .byte   MUSIC_NEEDLE,MUSIC_MAGNET,MUSIC_GEMINI,MUSIC_HARD
        .byte   MUSIC_TOP,MUSIC_SNAKE,MUSIC_SPARK,MUSIC_SHADOW
        .byte   MUSIC_NEEDLE,MUSIC_GEMINI,MUSIC_SPARK,MUSIC_SHADOW
        .byte   MUSIC_WILY_1,MUSIC_WILY_1,MUSIC_WILY_2,MUSIC_WILY_2
        .byte   MUSIC_WILY_3,MUSIC_WILY_3
frame_loop_pause_permission:  .byte   $00,$00,$FF,$00,$FF,$00,$FF,$FF ; per-state pause permission (bit 7 = no pause)
        .byte   $00,$FF,$00,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF
        .byte   $FF

; ===========================================================================
; player_state_dispatch_prelude — pre-process before running state handler
; ===========================================================================
; Resets walk speed (unless sliding), handles platform push, invincibility
; timer, and charge shot (Needle auto-fire).
prelude_check_slide_speed:  lda     ent_xvel ; if X speed whole == $02 (slide speed):
        cmp     #$02                    ; slide speed?
        bne     prelude_reset_walk_speed ; no → reset to walk speed
        lda     player_state            ; load current player state
        cmp     #PSTATE_SLIDE           ; check if sliding
        beq     prelude_setup_per_frame ; sliding, keep slide speed
prelude_reset_walk_speed:  lda     #$4C ; reset walk speed to $01.4C
        sta     ent_xvel_sub            ; store walk sub-pixel speed
        lda     #$01                    ; walk speed whole = $01
        sta     ent_xvel                ; store walk whole speed

; --- per-frame setup ---
prelude_setup_per_frame:  lda     #$40  ; gravity = $40 (stage select value)
        sta     gravity                 ; store gravity value
        ldx     #$00                    ; X = 0 (player slot), clear collision entity
        stx     $5D                     ; clear collision entity slot
        lda     $36                     ; if platform pushing: apply push velocity
        beq     prelude_invincibility_timer ; no push pending, skip
        jsr     prelude_platform_push_apply ; (platform_push handler)

; --- invincibility timer ---
prelude_invincibility_timer:  lda     invincibility_timer ; load invincibility timer
        beq     prelude_charge_shot_check ; zero → skip
        dec     invincibility_timer     ; decrement timer
        bne     prelude_charge_shot_check ; not expired → skip
        lda     ent_anim_frame          ; clear animation lock (bit 7)
        and     #$7F                    ; when invincibility expires
        sta     ent_anim_frame          ; store cleared anim frame

; --- charge shot: Needle Cannon auto-fire ($1F counter) ---
prelude_charge_shot_check:  lda     joy1_held ; B button held?
        and     #BTN_B                  ; B button mask
        bne     prelude_charge_increment ; yes → increment charge
        lda     #$E0                    ; not held: reset charge to $E0
        sta     $1F                     ; (high value = "not charging")
        bne     prelude_dispatch_state  ; → dispatch state
prelude_charge_increment:  lda     $1F  ; increment charge counter by $20
        clc                             ; wraps to 0 after 8 increments
        adc     #$20                    ; add $20 to charge counter
        sta     $1F                     ; store updated charge counter
        bne     prelude_dispatch_state  ; not wrapped → dispatch state
        lda     current_weapon          ; if weapon == $02 (Needle Cannon):
        cmp     #WPN_NEEDLE             ; auto-fire when charge wraps
        bne     prelude_dispatch_state  ; not Needle → skip auto-fire
        lda     joy1_press              ; set B-button-pressed flag
        ora     #BTN_B                  ; (triggers weapon_fire in state handler)
        sta     joy1_press              ; force B press for auto-fire
prelude_dispatch_state:  ldy     player_state ; load player state index
        lda     player_state_ptr_lo,y   ; state handler ptr low byte
        sta     temp_00                 ; store at $00
        lda     player_state_ptr_hi,y   ; state handler ptr high byte
        sta     $01                     ; store at $01
        jmp     (temp_00)               ; jump to state handler

player_state_ptr_lo:  .byte   $36,$07,$FD,$EB,$BA,$13,$AB,$31
        .byte   $58,$29,$91,$BE,$D3,$E1,$79,$CC
        .byte   $14,$AA,$52,$33,$8A,$8C ; $12 player_warp_anim
player_state_ptr_hi:  .byte   $CE,$D0,$D3,$D4,$D5,$D6,$D6,$D8 ; $02
        .byte   $D8,$D9,$D9,$D9,$D9,$DB,$D7,$CD ; $0A
        .byte   $DD,$DD,$DE,$DF,$DF,$E0,$66,$61 ; $12
        .byte   $E0,$CF,$CE
        .byte   $CF

; player state $0F: frozen by external force (slam/grab/cutscene) [confirmed]
; Player cannot move. Gravity still applies (falls if airborne).
; Boss fight ($5A bit 7) freezes animation counter instead.
player_stunned:
        lda     boss_active             ; if boss fight active: freeze animation
        bmi     prelude_stunned_freeze_anim ; boss active, freeze anim
        ldy     #$00                    ; Y = 0 (standing hitbox offset)
        jsr     move_vertical_gravity   ; apply gravity + vertical collision
        bcc     prelude_stunned_done    ; not landed → done
        lda     #$01                    ; anim ID $01 = idle
        cmp     ent_anim_id             ; already idle?
        beq     prelude_stunned_done    ; yes → done
        jsr     reset_sprite_anim       ; reset to idle animation
        lda     #$00                    ; clear shooting flag
        sta     walk_flag               ; clear walk/shoot flag
prelude_stunned_done:  rts              ; return to caller

prelude_stunned_freeze_anim:  lda     #$00 ; freeze anim counter (boss cutscene)
        sta     ent_anim_frame          ; clear anim frame counter
        rts                             ; return to caller

; --- platform_push: apply external velocity from moving platform ---
; $36 = push direction (bit 0: 1=right, 0=left)
; $37/$38 = push speed (sub/whole), applied as player X speed temporarily.

prelude_platform_push_apply:  lda     ent_flags ; save player sprite flags
        pha                             ; push flags on stack
        lda     ent_xvel_sub            ; save player X speed (sub + whole)
        pha                             ; push X speed sub on stack
        lda     ent_xvel                ; load X velocity whole
        pha                             ; push X speed whole on stack
        lda     $37                     ; apply platform push speed
        sta     ent_xvel_sub            ; as player X speed temporarily
        lda     $38                     ; load push speed whole byte
        sta     ent_xvel                ; store as X velocity whole
        ldy     player_state            ; Y = collision offset:
        cpy     #PSTATE_SLIDE           ; if sliding (state $02), use slide hitbox
        beq     prelude_platform_push_dir ; sliding → skip Y reset
        ldy     #$00                    ; Y = 0 (standing hitbox)
prelude_platform_push_dir:  lda     $36 ; push direction: bit 0 = right
        and     #$01                    ; test bit 0 (direction)
        beq     prelude_platform_push_left ; bit 0 clear → push left
        jsr     move_right_collide      ; push right with collision
        jmp     prelude_platform_push_restore ; skip left push handler

prelude_platform_push_left:  iny        ; push left with collision
        jsr     move_left_collide       ; apply left push with collision

; --- restore player state after platform push ---
prelude_platform_push_restore:  pla     ; restore original X speed
        sta     ent_xvel                ; restore X velocity whole
        pla                             ; pull sub-pixel speed
        sta     ent_xvel_sub            ; restore X velocity sub
        pla                             ; restore original facing (bit 6 only)
        and     #ENT_FLAG_HFLIP         ; from saved sprite flags
        sta     temp_00                 ; store facing bit in temp
        lda     ent_flags               ; clear bit 6, then OR with saved
        and     #$BF                    ; mask off facing bit
        ora     temp_00                 ; merge saved facing bit
        sta     ent_flags               ; store merged flags
        lda     #$00                    ; clear platform push flag
        sta     $36                     ; clear push flag
prelude_exit:  rts                      ; return to caller

; ===========================================================================
; player state $00: on ground — walk, jump, slide, shoot, Rush interaction
; ===========================================================================
player_on_ground:

        jsr     player_airborne         ; run $99/collision first
        bcc     prelude_exit            ; carry clear = still airborne → RTS

; --- landed: check for Rush Coil interaction ---
        ldy     $5D                     ; $5D = last collision entity slot
        cpy     #$01                    ; slot 1 = Rush entity?
        bne     player_ground_normal_walk ; not Rush → skip
        lda     ent_anim_id             ; if player OAM == $11 (damage flash):
        cmp     #$11                    ; damage flash anim?
        bne     player_ground_rush_coil_check ; not damage flash, check Rush
        lda     #$01                    ; A = $01 (idle animation)
        jsr     reset_sprite_anim       ; reset to idle anim
player_ground_rush_coil_check:  ldy     $05C1 ; load slot 1 anim ID
        cpy     #$D7                    ; (Rush Coil/Marine/Jet active)
        bcc     player_ground_normal_walk ; no → normal ground

; --- Rush handler dispatch via pointer table ---
        lda     frame_loop_ready_overlay_oam,y ; build indirect jump pointer
        sta     temp_00                 ; from $CCEF/$CCF2 tables
        lda     frame_loop_ready_sprite_table,y ; indexed by Rush OAM ID
        sta     $01                     ; store handler ptr high byte
        jmp     (temp_00)               ; dispatch Rush handler via pointer table

; --- Rush Coil bounce: player lands on Rush Coil and bounces upward ---
; Reached via indirect jump from Rush handler dispatch above.

        lda     $05A1                   ; if Rush already bounced: skip
        bne     player_ground_normal_walk ; already bounced, normal walk
        inc     jump_counter            ; set forced-fall flag (no variable jump)
        lda     #$EE                    ; Y speed = $06.EE (Rush Coil bounce)
        sta     ent_yvel_sub            ; strong upward bounce
        lda     #$06                    ; Y velocity whole = $06 (upward)
        sta     ent_yvel                ; store Y velocity whole
        inc     $05A1                   ; mark bounce as triggered (one-shot)
        lda     #$3C                    ; slot 1 timer = 60 frames (Rush lingers)
        sta     $0501                   ; ent_timer slot 1 = 60 frames
        lda     $0581                   ; load slot 1 flags
        and     #$FE                    ; clear bit 0
        sta     $0581                   ; store updated flags
        jsr     decrease_ammo_tick      ; consume Rush Coil ammo
        jmp     player_airborne         ; → airborne state (bouncing up)

player_ground_normal_walk:  lda     ent_anim_id ; load current animation ID
        cmp     #$10                    ; anim $10 = slide anim?
        bne     player_ground_jump_or_slide ; not sliding → check jump/slide
        lda     joy1_press              ; check new button presses
        and     #BTN_A                  ; A button pressed?
        beq     prelude_exit            ; no → return
        lda     joy1_held               ; check held buttons
        and     #BTN_DOWN               ; holding down?
        beq     prelude_exit            ; return (no slide)
        jmp     slide_initiate          ; initiate slide

player_ground_jump_or_slide:  lda     joy1_press ; check new button presses
        and     #BTN_A                  ; A button pressed?
        beq     player_ground_anim_dispatch ; no → animate walk/idle
        lda     joy1_held               ; check held buttons
        and     #BTN_DOWN               ; holding down?
        beq     player_ground_normal_jump ; not holding down, jump
        jmp     slide_initiate          ; initiate slide

; --- DEBUG (shipped in retail) — controller 2 Right = super jump ---
; Holding Right on P2 (or latching it via Left) increases jump velocity
; from $04.E5 to $08.00 (~73% higher). Combined with pit death immunity,
; this lets the player skip large portions of stages.
player_ground_normal_jump:  lda     $17 ; P2 held buttons
        and     #$01                    ; bit 0 = Right held?
        bne     player_ground_super_jump ; yes → debug super jump
        lda     #$E5                    ; normal jump: Y vel = $04.E5
        sta     ent_yvel_sub,x          ; store Y velocity sub-pixel
        lda     #$04                    ; Y velocity whole = $04
        sta     ent_yvel,x              ; store Y velocity whole
        jmp     player_airborne         ; enter airborne state

player_ground_super_jump:  lda     #$00 ; debug jump: Y vel = $08.00
        sta     ent_yvel_sub            ; store Y velocity sub-pixel
        lda     #$08                    ; (~73% stronger than normal)
        sta     ent_yvel                ; store Y velocity whole
        jmp     player_airborne         ; enter airborne state

; --- animation state dispatch: determine walk/idle animation ---
; OAM IDs: $04/$05 = shoot-walk, $0D/$0E/$0F = special walk anims

player_ground_anim_dispatch:  lda     ent_anim_id ; A = current animation ID
        cmp     #$04                    ; $04/$05 = shoot-walk anims
        beq     player_ground_shoot_walk ; OAM $04 → shoot-walk handler
        cmp     #$05                    ; OAM $05 = shoot-walk variant
        beq     player_ground_shoot_walk ; shoot-walk variant match
        cmp     #$0D                    ; $0D/$0E/$0F = turning/special anims
        beq     player_ground_special_anim ; OAM $0D → check if anim complete
        cmp     #$0E                    ; OAM $0E = special anim variant
        beq     player_ground_special_anim ; OAM $0E → check if anim complete
        cmp     #$0F                    ; OAM $0F = special anim variant
        bne     player_ground_walk_handler ; other → normal walk handler
player_ground_special_anim:  lda     ent_anim_state ; check animation state
        bne     player_ground_walk_handler ; → normal walk handler
        beq     player_ground_slide_weapon ; anim done → slide/crouch check

; --- shoot-walk: continue walking in facing direction ---
player_ground_shoot_walk:  lda     joy1_held ; D-pad matches facing direction?
        and     player_facing           ; mask held direction with facing
        beq     player_ground_turning_anim ; no → turning animation
        pha                             ; save direction on stack
        jsr     player_ground_slide_weapon ; slide/crouch check
        pla                             ; restore direction from stack
        jmp     player_ground_move_horizontal ; move L/R based on direction

; --- turning: set turn animation, then move ---

player_ground_turning_anim:  lda     #$0D ; OAM $0D = turning animation
        jsr     reset_sprite_anim       ; set turning animation
        lda     walk_flag               ; check if shooting
        beq     player_ground_move_facing ; not shooting, move facing
        jsr     shoot_oam_advance_variant ; update shoot animation variant
player_ground_move_facing:  lda     player_facing ; move in facing direction
        jmp     player_ground_move_horizontal ; move in facing direction

; --- normal walk handler: D-pad L/R → walk, else idle ---

player_ground_walk_handler:  lda     joy1_held ; D-pad L/R held?
        and     #$03                    ; mask L/R bits
        beq     player_ground_idle      ; no → idle
        sta     player_facing           ; update facing direction
        jsr     player_ground_slide_weapon ; slide/crouch check
        lda     ent_anim_id             ; check current animation ID
        cmp     #$0D                    ; OAM $0D = turning anim?
        beq     player_ground_shoot_walk_anim ; yes → use shoot-walk anim
        cmp     #$0E                    ; OAM $0E = special anim?
        beq     player_ground_shoot_walk_anim ; special anim variant match
        cmp     #$0F                    ; check OAM $0F
        bne     player_ground_turning_anim ; else → turning animation

; --- shoot-walk animation: OAM $04 + check weapon-specific override ---
player_ground_shoot_walk_anim:  lda     #$04 ; OAM $04 = shoot-walk
        jsr     reset_sprite_anim       ; set animation to shoot-walk
        lda     player_facing           ; A = facing direction
        ldy     walk_flag               ; if not shooting: just move
        beq     player_ground_move_horizontal ; not shooting, move player
        jsr     shoot_oam_advance_variant ; update shoot animation
        ldy     current_weapon          ; Y = current weapon ID
        cmp     #$04                    ; A == $04? (Magnet Missile OAM)
        bne     player_ground_move_horizontal ; not Magnet OAM, skip
        ldy     #$AA                    ; OAM $AA = Magnet shoot-walk
        sta     ent_anim_id             ; set animation ID to Magnet variant

; --- move_horizontal: move player L/R based on direction in A ---
; A bit 0: 1=right, 0=left. Entry point used by walking, entity_ride, etc.
player_ground_move_horizontal:  and     #$01 ; bit 0: right?
        beq     player_ground_move_left ; left direction, branch
        ldy     #$00                    ; move right with collision
        jmp     move_right_collide      ; jump to right move handler

player_ground_move_left:  ldy     #$01  ; move left with collision
        jmp     move_left_collide       ; jump to left move handler

; --- idle: no D-pad input → check if need to reset animation ---

player_ground_idle:  lda     walk_flag  ; if shooting: skip idle anim reset
        bne     player_ground_slide_weapon ; shooting, skip idle check
        lda     #$01                    ; OAM $01 = idle standing
        cmp     ent_anim_id             ; compare with current anim
        beq     player_ground_slide_weapon ; already idle → skip reset
        jsr     reset_sprite_anim       ; set OAM to $01 (idle standing)

; --- common ground exit: check slide/crouch + B button ---
player_ground_slide_weapon:  jsr     shoot_timer_tick_dec ; decrement shoot timer
        lda     joy1_press              ; B button pressed → fire weapon
        and     #BTN_B                  ; B button mask
        beq     player_ground_exit      ; no B press, exit
        jsr     weapon_fire             ; fire current weapon
player_ground_exit:  rts                ; return to caller

; --- DEAD CODE: unreachable block at $1ECF66 ---
; No references to this address exist in any bank — unreachable dead code.
; Contains a Rush Marine ground-mode handler (slot 1 management, walk, climb,
; jump, shoot), superseded by the current player_rush_marine implementation.

        jsr     decrease_ammo_tick      ; decrease Rush ammo
        lda     #$82                    ; set slot 1 (Rush) main routine = $82
        cmp     $0321                   ; compare with slot 1 routine
        beq     dead_code_rush_marine_jump ; already $82 → skip store
        sta     $0321                   ; set slot 1 routine = $82
        lda     $05E1                   ; load slot 1 anim frame
        and     #$7F                    ; clear bit 7
        sta     $05E1                   ; store slot 1 anim frame
dead_code_rush_marine_jump:  lda     joy1_press ; A button → jump
        and     #BTN_A                  ; A button mask
        beq     dead_code_rush_marine_walk ; A not pressed, skip jump
        jmp     player_ground_normal_jump ; reuse normal jump routine

dead_code_rush_marine_walk:  lda     joy1_held ; D-pad L/R → walk
        and     #$03                    ; mask L/R bits
        beq     dead_code_rush_marine_climb ; no horizontal input
        sta     player_facing           ; update facing direction
        jsr     player_ground_move_horizontal ; move L/R with collision
        lda     #$01                    ; OAM $01 = walk animation
        jsr     reset_sprite_anim       ; set walk animation
        lda     walk_flag               ; check if shooting
        beq     dead_code_rush_marine_climb ; not shooting, skip variant
        jsr     shoot_oam_advance_variant ; update shoot animation variant
dead_code_rush_marine_climb:  lda     joy1_held ; D-pad U/D → climb vertically
        and     #$0C                    ; mask U/D bits
        beq     dead_code_rush_marine_shoot ; no vertical input → skip
        pha                             ; save D-pad state
        lda     #$80                    ; climb speed = $01.80
        sta     ent_yvel_sub,x          ; set Y velocity sub-pixel
        lda     #$01                    ; Y velocity pixel = $01
        sta     ent_yvel,x              ; set Y velocity pixel
        pla                             ; restore D-pad state
        and     #$08                    ; bit 3 = up?
        beq     dead_code_rush_marine_down ; no → move down
        ldy     #$01                    ; move up with collision
        jsr     move_up_collide         ; move up with collision check
        lda     #$0C                    ; top boundary Y = $0C
        cmp     ent_y_px                ; compare with player Y
        bcc     dead_code_rush_marine_sync ; below boundary, skip clamp
        sta     ent_y_px                ; clamp Y to top boundary
        bcs     dead_code_rush_marine_sync ; always branches
dead_code_rush_marine_down:  ldy     #$07 ; move down with collision
        jsr     move_down_collide       ; move down with collision check
dead_code_rush_marine_sync:  lda     ent_y_px ; slot 1 Y = player Y + $0E
        clc                             ; (Rush sits 14px below player)
        adc     #$0E                    ; add $0E offset for Rush
        sta     $03C1                   ; store to slot 1 Y pixel ($03C1)
        jsr     reset_gravity           ; clear gravity after manual climb
dead_code_rush_marine_shoot:  jsr     shoot_timer_tick_dec ; decrement shoot timer
        lda     joy1_press              ; B button → fire weapon
        and     #BTN_B                  ; B button mask
        beq     dead_code_rush_marine_exit ; not pressed → skip
        jsr     weapon_fire             ; fire current weapon
dead_code_rush_marine_exit:  rts        ; return to caller

; --- END DEAD CODE ---

; --- enter Rush Marine mode: copy Rush position to player, change state ---

        lda     $0361                   ; A = slot 1 X pixel (Rush)
        sta     ent_x_px                ; copy to player X pixel
        lda     $0381                   ; A = slot 1 X screen
        sta     ent_x_scr               ; copy to player X screen
        lda     $03C1                   ; A = slot 1 Y pixel (Rush)
        sta     ent_y_px                ; copy to player Y pixel
        lda     #$00                    ; deactivate slot 1 (Rush entity)
        sta     $0301                   ; store to slot 1 status (deactivate)
        lda     #$05                    ; $EB = CHR page 5 (Rush Marine tiles)
        sta     $EB                     ; store CHR page for Rush
        jsr     update_CHR_banks        ; apply CHR bank settings
        lda     #PSTATE_RUSH_MARINE     ; state = Rush Marine ($08)
        sta     player_state            ; set player state
        lda     #$DA                    ; OAM $DA = Rush Marine animation
        jmp     reset_sprite_anim       ; set animation and return

; player state $01: jumping/falling, variable jump

player_airborne:  lda     ent_yvel      ; check Y velocity sign
        bmi     player_airborne_variable_jump ; negative (rising) → skip to clear flag
        lda     jump_counter            ; check forced-fall flag
        bne     player_airborne_wall_check ; nonzero → skip variable jump
        lda     joy1_held               ; check if A button held
        and     #BTN_A                  ; A button mask
        bne     player_airborne_variable_jump ; held → keep rising (variable jump)
        jsr     reset_gravity           ; A released → clamp Y speed to 0 (variable jump)
player_airborne_variable_jump:  lda     #$00 ; clear forced-fall flag
        sta     jump_counter            ; clear jump counter

; --- horizontal wall check + vertical movement with $99 ---
player_airborne_wall_check:  ldy     #$06 ; Y=6: check at foot height
        jsr     check_tile_horiz        ; (Y=6: check at foot height)
        lda     $10                     ; A = tile collision result
        sta     $0F                     ; save in $0F
        ldy     #$00                    ; Y=0 for vertical movement
        jsr     move_vertical_gravity   ; apply gravity and move vertically
        php                             ; save carry (landing status)

; --- check for landing: spawn dust cloud on first ground contact ---
        lda     $0F                     ; check saved collision result
        cmp     #$80                    ; $80 = landed on solid ground
        bne     player_airborne_dust_clear ; not landed → clear dust state
        lda     $B9                     ; $B9 = dust cooldown timer
        bne     player_airborne_landing_check ; if active: skip dust spawn
        lda     #$03                    ; set dust cooldown = 3 frames
        sta     $B9                     ; store dust cooldown timer
        lda     $BA                     ; $BA = dust spawned flag (one-shot)
        bne     player_airborne_landing_check ; already spawned → skip
        inc     $BA                     ; mark dust as spawned
        lda     #SFX_LAND_ALT           ; play landing sound effect
        jsr     submit_sound_ID         ; play landing sound effect

; spawn landing dust cloud in slot 4 (visual effect slot)
        lda     #$80                    ; activate slot 4
        sta     $0304                   ; store to slot 4 status (activate)
        lda     ent_flags               ; A = player sprite flags
        sta     $0584                   ; copy to slot 4 flags
        ldx     #$04                    ; slot 4: OAM $68 = landing dust animation
        lda     #$68                    ; dust cloud animation tile $68
        jsr     reset_sprite_anim       ; set dust cloud animation
        lda     ent_x_px                ; A = player X pixel
        sta     $0364                   ; copy to slot 4 X pixel
        lda     ent_x_scr               ; A = player X screen
        sta     $0384                   ; copy to slot 4 X screen
        lda     ent_y_px                ; A = player Y pixel
        and     #$F0                    ; align to 16px tile grid
        sec                             ; prepare for subtraction
        sbc     #$08                    ; subtract 8px (snap to tile top)
        sta     $03C4                   ; store to slot 4 Y pixel
        lda     ent_y_scr               ; A = player Y screen
        sta     $03E4                   ; copy to slot 4 Y screen
        ldx     #$00                    ; restore X = 0 (player slot)
        stx     $0484                   ; clear slot 4 hitbox (no collision)
        stx     $0324                   ; clear slot 4 routine
        beq     player_airborne_landing_check ; always branches (X=0)

; --- not landed: reset dust state ---
player_airborne_dust_clear:  lda     #$00 ; clear dust spawn flag and cooldown
        sta     $BA                     ; clear dust spawned flag ($BA)
        sta     $B9                     ; clear dust cooldown ($B9)
player_airborne_landing_check:  plp     ; restore carry (landing status)
        jsr     ladder_check_entry_ground ; check if player should enter ladder
        bcc     player_airborne_state_dispatch ; carry clear = no ladder → continue airborne

; --- player landed on ground → transition to on_ground ---
        lda     player_state            ; check current player state
        beq     player_airborne_landing_anim ; already on_ground → skip transition
        lda     #SFX_LAND               ; play landing sound
        jsr     submit_sound_ID         ; play landing sound effect
        lda     #$00                    ; state = on_ground ($00)
        sta     player_state            ; set to on_ground state
        lda     #$0D                    ; OAM $0D = landing animation
        jsr     reset_sprite_anim       ; set landing animation
        inc     ent_anim_state          ; force anim state to 1 (playing)
        lda     walk_flag               ; if shooting: set shoot OAM variant
        beq     player_airborne_landing_anim ; not shooting → return
        jsr     shoot_oam_advance_variant ; update shoot animation variant
player_airborne_landing_anim:  rts      ; return to caller

; --- still airborne: dispatch by current state ---

player_airborne_state_dispatch:  lda     player_state ; check current player state
        cmp     #PSTATE_LADDER          ; state $03 = on ladder
        beq     player_airborne_exit    ; on ladder → just return
        cmp     #PSTATE_AIRBORNE        ; state $01 = already airborne
        beq     player_airborne_walk_shoot ; already airborne → walk+shoot

; --- first airborne frame: set jump animation ---
        lda     #$07                    ; OAM $07 = jump/fall animation
        jsr     reset_sprite_anim       ; set jump/fall animation
        lda     #PSTATE_AIRBORNE        ; set state to airborne ($01)
        sta     player_state            ; store player state
        lda     walk_flag               ; check if shooting
        beq     player_airborne_walk_shoot ; not shooting → walk+shoot handler
        jsr     shoot_oam_advance_variant ; update shoot animation variant

; --- horizontal_walk_and_shoot: D-pad L/R + B button + shoot timer ---
; Common subroutine for on_ground, airborne, and entity_ride states.
player_airborne_walk_shoot:  lda     joy1_held ; D-pad L/R held?
        and     #$03                    ; mask L/R bits
        beq     player_airborne_shoot_check ; no → skip walk
        sta     player_facing           ; update facing direction
        jsr     player_ground_move_horizontal ; move L/R with collision
player_airborne_shoot_check:  jsr     shoot_timer_tick_dec ; shoot timer tick
        lda     joy1_press              ; B button pressed → fire weapon
        and     #BTN_B                  ; B button mask
        beq     player_airborne_exit    ; no B press, skip fire
        jsr     weapon_fire             ; fire current weapon
player_airborne_exit:  clc              ; carry clear = still airborne (for caller)
        rts                             ; return to caller

; player intends to fire the active weapon
; check if enough ammo and if free slot available

weapon_fire:  ldy     current_weapon    ; Y = current weapon index
        lda     player_hp,y             ; read ammo for weapon Y
        and     #$1F                    ; mask to 5-bit ammo value
        beq     init_weapon_no_free_slots ; ammo = 0 → can't fire
        lda     weapon_max_shots,y      ; max simultaneous shots for this weapon
        tay                             ; Y = max slot index to search
weapon_fire_find_slot_loop:  lda     ent_status,y ; check slot Y status
        bpl     weapon_fire_decrease_ammo ; bit 7 clear = slot free
        dey                             ; try next lower slot
        bne     weapon_fire_find_slot_loop ; loop until slot found
        beq     init_weapon_no_free_slots ; all slots full → abort
weapon_fire_decrease_ammo:  ldy     current_weapon ; reload weapon index
        jsr     decrease_ammo           ; subtract one ammo unit
        lda     weapon_init_ptr_lo,y    ; load weapon init pointer low byte
        sta     temp_00                 ; store in indirect jump vector
        lda     weapon_init_ptr_hi,y    ; load weapon init pointer high byte
        sta     $01                     ; store high byte
        jmp     (temp_00)               ; indirect jump to weapon init routine

; common weapon init routine: spawns the shot

init_weapon:  lda     walk_flag         ; check if already in shoot animation
        bne     init_weapon_anim_check  ; nonzero = already shooting, skip OAM change
        jsr     shoot_oam_advance_variant ; advance OAM to shooting variant
init_weapon_anim_check:  lda     ent_anim_id ; check player animation ID
        cmp     #$05                    ; climbing ($05)?
        beq     init_weapon_set_shooting ; skip anim reset for climb/slide/ladder
        cmp     #$0E                    ; slide climb ($0E)?
        beq     init_weapon_set_shooting ; yes → skip anim reset
        cmp     #$0F                    ; ladder shoot ($0F)?
        beq     init_weapon_set_shooting ; yes → skip anim reset
        lda     #$00                    ; for other animations:
        sta     ent_anim_state          ; reset animation state to 0
        sta     ent_anim_frame          ; reset animation frame to 0
init_weapon_set_shooting:  lda     #$10 ; shoot timer = $10 (16 frames)
        sta     walk_flag               ; set shoot animation timer
        ldy     current_weapon          ; Y = current weapon index
        lda     weapon_max_shots,y      ; max shot slots for this weapon
        tay                             ; Y = max slot to search
init_weapon_find_slot_loop:  lda     ent_status,y ; check entity status for slot Y
        bpl     init_weapon_spawn_shot  ; bit 7 clear = free slot found
        dey                             ; try next lower slot
        bne     init_weapon_find_slot_loop ; keep searching
        clc                             ; no free slots, return carry off
init_weapon_no_free_slots:  rts         ; no free slot; return

init_weapon_spawn_shot:  ldx     current_weapon ; X = current weapon ID
        lda     weapon_fire_sound,x     ; play weapon sound effect
        jsr     submit_sound_ID         ; play weapon fire sound
        ldx     #$00                    ; X=0 (player slot)
        lda     #$80                    ; activate weapon slot
        sta     ent_status,y            ; mark weapon slot active
        lda     player_facing           ; read player facing direction
        ror     a                       ; to sprite flags bit 6 (H-flip)
        ror     a                       ; $01→ROR³→$00→AND=$00 (right)
        ror     a                       ; $02→ROR³→$40→AND=$40 (left=flip)
        and     #ENT_FLAG_HFLIP         ; isolate H-flip bit
        ora     #$90                    ; bits: $80=drawn, $10=child spawn
        sta     ent_flags,y             ; store weapon sprite flags
        lda     #$00                    ; clear sub-pixel velocity
        sta     ent_xvel_sub,y          ; X velocity sub = 0
        lda     #$04                    ; 4 pixels per frame
        sta     ent_xvel,y              ; X velocity = 4 px/frame
        lda     player_facing           ; copy player facing
        sta     ent_facing,y            ; to weapon facing
        and     #$02                    ; bit 1: 0=right, 2=left
        tax                             ; X = offset table index
        lda     ent_x_px                ; player X pixel position
        clc                             ; add directional offset
        adc     weapon_x_offset,x       ; weapon X = player X + offset
        sta     ent_x_px,y              ; store weapon X pixel
        lda     ent_x_scr               ; player X screen
        adc     weapon_x_offset_right_lo,x ; add screen carry offset
        sta     ent_x_scr,y             ; store weapon X screen
        lda     ent_y_px                ; player Y pixel position
        sta     ent_y_px,y              ; copy to weapon Y pixel
        lda     ent_y_scr               ; player Y screen
        sta     ent_y_scr,y             ; copy to weapon Y screen
        lda     #$00                    ; clear weapon fields to 0:
        sta     ent_hitbox,y            ; hitbox
        sta     ent_anim_state,y        ; animation state
        sta     ent_anim_frame,y        ; animation frame
        sta     ent_x_sub,y             ; X sub-pixel
        sta     ent_timer,y             ; timer
        ldx     current_weapon          ; X = current weapon ID
        lda     weapon_OAM_ID,x         ; look up OAM ID for weapon
        sta     ent_anim_id,y           ; set weapon animation ID
        lda     weapon_main_ID,x        ; look up AI routine for weapon
        sta     ent_routine,y           ; set weapon AI routine
        inc     $3B                     ; $3B++ (shot parity counter)
        ldx     #$00                    ; restore X=0 (player slot)
        sec                             ; carry set = weapon spawned OK
        rts                             ; return to caller

; ===========================================================================
; init_rush — spawn Rush entity (Coil/Marine/Jet) in slot 1
; ===========================================================================
; Checks if Rush can be summoned (player OAM < $D7, drawn, slot 1 free).
; If not, falls through to init_weapon for normal weapon fire.
init_rush:

        lda     ent_anim_id             ; if player OAM ID >= $D7 (Rush already active):
        cmp     #$D7                    ; can't spawn another Rush
        bcs     init_rush_fallback_weapon ; → try normal weapon instead
        lda     ent_flags               ; if player not drawn (bit 7 clear):
        bpl     init_rush_exit          ; abort (RTS)
        lda     $0301                   ; if slot 1 already active (bit 7):
        bpl     init_rush_spawn_entity  ; slot free → spawn Rush
init_rush_fallback_weapon:  jmp     init_weapon ; fallback: fire normal weapon

; --- spawn Rush entity in slot 1 ---

init_rush_spawn_entity:  ldy     #$01   ; spawn entity $13 (Rush) in slot 1
        lda     #$13                    ; Rush entity type = $13
        jsr     init_child_entity       ; spawn Rush in slot 1
        lda     #$00                    ; Rush Y position = 0 (will land via $99)
        sta     $03C1                   ; Rush Y pixel = 0
        sta     $03E1                   ; Rush Y screen = 0
        lda     #$11                    ; $0481 = $11 (Rush damage/collision flags)
        sta     $0481                   ; set Rush hitbox flags
        lda     player_facing           ; copy player facing to Rush ($04A1)
        sta     $04A1                   ; copy facing to Rush slot
        and     #FACING_LEFT            ; X = 0 if facing right, 2 if facing left
        tax                             ; (bit 1 of $31: 1=R has bit1=0, 2=L has bit1=1)
        lda     ent_x_px                ; Rush X = player X + directional offset
        clc                             ; offset from $D31F table (2 entries: R, L)
        adc     weapon_x_offset_right_hi,x ; add directional X offset
        sta     $0361                   ; store Rush X pixel
        lda     ent_x_scr               ; Rush X screen = player X screen + carry
        adc     weapon_x_offset_left_lo,x ; add screen carry for X
        sta     $0381                   ; store Rush X screen
        lda     #$80                    ; set Rush main routine = $80 (active)
        sta     ent_routine,y           ; set Rush routine to active
        lda     #$AB                    ; Rush Y speed = $FF.AB (slight downward drift)
        sta     $0441                   ; Rush Y velocity sub = $AB
        lda     #$FF                    ; Rush Y velocity = $FF (upward drift)
        sta     $0461                   ; store Rush Y velocity whole
        ldx     #$00                    ; restore X = 0 (player slot)
init_rush_exit:  rts                    ; return to caller

; --- init_needle_cannon: fire needle shot with vertical offset ---
; Alternates Y offset based on $3B bit 0 (shot parity).
init_needle_cannon:

        jsr     init_weapon             ; spawn basic weapon projectile
        bcc     init_needle_cannon_exit ; carry clear = spawn failed
        lda     $3B                     ; $3B bit 0 = shot parity (alternates 0/1)
        and     #$01                    ; isolate low bit (parity)
        tax                             ; X = parity index (0 or 1)
        lda     ent_y_px,y              ; read shot Y position
        clc                             ; add Y offset from table
        adc     weapon_max_shots_table,x ; even/odd needle offset
        sta     ent_y_px,y              ; store adjusted Y position
        ldx     #$00                    ; restore X = 0 (player slot)
init_needle_cannon_exit:  rts           ; return to caller
init_gemini_laser:

        jsr     init_weapon             ; spawn first laser shot
        bcc     init_gemini_laser_exit  ; spawn failed → return
        iny                             ; advance to slot 2
        jsr     init_weapon_spawn_shot  ; spawn shot in slot 2
        iny                             ; advance to slot 3
        jsr     init_weapon_spawn_shot  ; spawn shot in slot 3
        lda     #$B4                    ; gemini laser timer = $B4 (180 frames)
        sta     $0501                   ; ent_timer slot 1
        sta     $0502                   ; ent_timer slot 2
        sta     $0503                   ; ent_timer slot 3
        lda     #$00                    ; clear all Y velocities:
        sta     $0441                   ; Y vel sub slot 1
        sta     $0442                   ; Y vel sub slot 2
        sta     $0443                   ; Y vel sub slot 3
        sta     $0461                   ; Y vel whole slot 1
        sta     $0462                   ; Y vel whole slot 2
        sta     $0463                   ; Y vel whole slot 3
        lda     $0361                   ; read slot 1 X pixel
        and     #$FC                    ; align to 4-pixel grid
        sta     $0361                   ; store aligned X to slot 1
        sta     $0362                   ; copy to slot 2
        sta     $0363                   ; copy to slot 3
init_gemini_laser_exit:  rts            ; return to caller

; ===========================================================================
; init_hard_knuckle — weapon $03: Hard Knuckle projectile
; ===========================================================================
; Spawns in slot 1. Player state selects OAM from $D293 table:
;   $30=0(ground)→$AA, $30=1(air)→$AB, $30=2(slide)→$00(skip), $30=3(ladder)→$AD
; Hard Knuckle uniquely uses slot 1 (like Rush), overrides player OAM,
; and sets player state to $0B (hard_knuckle_ride) for steering.
init_hard_knuckle:

        lda     $0301                   ; slot 1 already active?
        bmi     init_hard_knuckle_exit  ; if so, can't fire
        ldy     player_state            ; player state >= $04 (reappear etc)?
        cpy     #PSTATE_REAPPEAR        ; state >= reappear?
        bcs     init_hard_knuckle_exit  ; can't fire in those states
        lda     init_hard_knuckle_oam_table,y ; OAM ID for this state ($00 = skip)
        beq     init_hard_knuckle_exit  ; state $02(slide): no Hard Knuckle
        jsr     init_weapon             ; spawn weapon entity (sets Y=slot)
        ldy     player_state            ; reload OAM for player state
        lda     init_hard_knuckle_oam_table,y ; and set player animation to it
        jsr     reset_sprite_anim       ; (punch/throw pose)
        cpy     #$03                    ; on ladder ($30=3)?
        bne     init_hard_knuckle_ladder ; not on ladder → skip
        lda     #$AE                    ; override slot 1 OAM to $AE
        sta     $05C1                   ; (ladder throw variant)
init_hard_knuckle_ladder:  lda     #$00 ; zero X/Y speeds for slot 1
        sta     $0401                   ; (Hard Knuckle starts stationary,
        sta     $0421                   ; player steers it with D-pad)
        sta     $0461                   ; Y vel whole = 0
        lda     #$80                    ; Y speed sub = $80
        sta     $0441                   ; (initial Y drift: $00.80 = 0.5 px/f)
        inc     $05E1                   ; bump anim frame (force sprite update)
        lda     player_state            ; save player state to ent_timer (slot 0)
        sta     ent_timer               ; (restore when Hard Knuckle ends)
        lda     #$10                    ; slot 1 AI routine = $10
        sta     ent_var1                ; (Hard Knuckle movement handler)
        lda     #PSTATE_WEAPON_RECOIL   ; set player state $0B = hard_knuckle_ride
        sta     player_state            ; (D-pad steers the projectile)
init_hard_knuckle_exit:  .byte   $60

; Hard Knuckle OAM IDs per player state ($30=0..3)
; $AA=ground, $AB=air, $00=skip(slide), $AD=ladder
init_hard_knuckle_oam_table:  .byte   $AA,$AB,$00,$AD
init_hard_knuckle_offset_table:  .byte   $01,$07,$00,$0A,$FC,$FF,$04,$00
init_top_spin:
        lda     player_state            ; check if player is airborne
        cmp     #PSTATE_AIRBORNE        ; must be airborne to use Top Spin
        bne     init_top_spin_exit      ; not airborne → exit
        lda     #$A3                    ; Top Spin OAM ID = $A3
        cmp     ent_anim_id             ; already spinning?
        beq     init_top_spin_exit      ; yes → exit
        jsr     reset_sprite_anim       ; else set animation to spin
        lda     #$2C                    ; and play the top spin
        jmp     submit_sound_ID         ; sound

; ===========================================================================
; init_search_snake — weapon $06: Search Snake
; ===========================================================================
; Spawns snake projectile. X speed = $01.00 (1 px/f), Y speed = $03.44
; (drops at ~3.27 px/f to find floor). AI routine $13 handles wall-crawling.
init_search_snake:

        jsr     init_weapon             ; spawn weapon; carry set = success
        bcc     init_top_spin_exit      ; no free slot → return
        lda     #$44                    ; Y speed sub = $44
        sta     ent_yvel_sub,y          ; set Y velocity sub
        lda     #$03                    ; Y speed whole = $03 (drop fast)
        sta     ent_yvel,y              ; set Y velocity whole
        lda     #$00                    ; X speed sub = $00
        sta     ent_xvel_sub,y          ; set X velocity sub-pixel
        lda     #$01                    ; X speed whole = $01 (1 px/f forward)
        sta     ent_xvel,y              ; set X velocity whole
        lda     #$13                    ; AI routine = $13 (snake crawl)
        sta     ent_timer,y             ; store AI routine in timer
init_top_spin_exit:  rts                ; return to caller

; ===========================================================================
; init_shadow_blade — weapon $0A: Shadow Blade
; ===========================================================================
; D-pad direction stored in ent_facing (bits: up=$08, down=FACING_LEFT, right=FACING_RIGHT).
; If no D-pad held, ent_facing=0 → blade fires horizontally (facing direction).
; X speed $04.00 (4 px/f). Copies player position to blade (spawns at player).
init_shadow_blade:

        jsr     init_weapon             ; spawn weapon; carry set = success
        bcc     shadow_blade_init_exit  ; no free slot → return
        lda     joy1_held               ; read D-pad held bits
        and     #$0B                    ; mask Up($08)+Down($02)+Right($01)
        beq     shadow_blade_init_dir   ; no direction → skip (fire horizontal)
        sta     ent_facing,y            ; store throw direction for AI
shadow_blade_init_dir:  lda     #$00    ; clear Y speed:
        sta     ent_yvel_sub,y          ; clear Y velocity sub-pixel
        lda     #$04                    ; X speed whole = $04 (4 px/f)
        sta     ent_yvel,y              ; Y speed whole = $04
        lda     #$14                    ; AI routine = $14 (shadow blade)
        sta     ent_timer,y             ; set AI timer for shadow blade
        lda     ent_x_px,x              ; copy player X position to blade
        sta     ent_x_px,y              ; store blade X pixel
        lda     ent_x_scr,x             ; copy player X screen to blade
        sta     ent_x_scr,y             ; store blade X screen
        lda     ent_y_px,x              ; copy player Y position to blade
        sta     ent_y_px,y              ; store blade Y pixel
shadow_blade_init_exit:  .byte   $60

; routine pointers for weapon init upon firing
weapon_init_ptr_lo:  .byte   $03,$12,$FB,$4D,$03,$9F,$B4,$A6 ; Mega Buster
        .byte   $03,$A6,$D3,$A6         ; Spark Shock
weapon_init_ptr_hi:  .byte   $D1,$D2,$D1,$D2,$D1,$D2,$D2,$D1 ; Mega Buster
        .byte   $D1,$D1,$D2,$D1         ; Spark Shock

; on weapon fire, weapon is placed at an offset from Mega Man
; based on left vs. right facing
; right X pixel, right X screen, left X pixel, left X screen
weapon_x_offset:  .byte   $0F
weapon_x_offset_right_lo:  .byte   $00,$F0,$FF
weapon_x_offset_right_hi:  .byte   $17
weapon_x_offset_left_lo:  .byte   $00,$E8,$FF
weapon_OAM_ID:  .byte   $18,$9F,$A2,$AC,$97,$18,$A5,$18 ; Mega Buster
        .byte   $9C,$18,$9E,$18         ; Spark Shock
weapon_main_ID:  .byte   $01,$84,$01,$85,$83,$01,$86,$01 ; Mega Buster
        .byte   $87,$01,$88,$01         ; Spark Shock
weapon_max_shots_table:  .byte   $FE,$02

; maximum # of shots on screen at once
weapon_max_shots:  .byte   $03,$01,$03,$01,$02,$00,$03,$03 ; Mega Buster
        .byte   $02,$03,$01,$03         ; Spark Shock

; sound ID played on weapon fire (indexed by current_weapon)
weapon_fire_sound:
        .byte   SFX_BUSTER,SFX_GEMINI_FIRE,SFX_BUSTER,SFX_BUSTER
        .byte   SFX_MAGNET_FIRE,SFX_TOP_SPIN,SFX_BUSTER,SFX_BUSTER
        .byte   SFX_SPARK_FIRE,SFX_BUSTER
        .byte   SFX_SHADOW_FIRE
        .byte   SFX_BUSTER

; --- shoot_timer_tick: decrement shoot timer, end shoot animation when done ---
; $32 = shoot animation timer. When it reaches 0, revert OAM to non-shoot variant.
; OAM IDs for shooting are base+1 (or +2 for Shadow Blade $0A).
shoot_timer_tick_dec:  lda     walk_flag ; read shoot timer
        beq     shoot_timer_tick_return ; zero = not shooting, return
        dec     walk_flag               ; decrement timer
        bne     shoot_timer_tick_return ; not done → return
        jsr     shoot_oam_revert_variant ; revert OAM from shoot variant
        ldy     ent_anim_id             ; if OAM == $04 (shoot-walk):
        cpy     #$04                    ; don't reset animation
        beq     shoot_timer_tick_return ; shoot-walk → skip reset
        lda     #$00                    ; reset animation state
        sta     ent_anim_frame          ; reset anim frame to 0
        sta     ent_anim_state          ; reset anim state to 0
shoot_timer_tick_return:  rts           ; return to caller

; --- set_shoot_oam: advance OAM to shooting variant ---
; OAM+1 for most weapons, OAM+2 for Shadow Blade ($0A).

shoot_oam_advance_variant:  pha         ; preserve A
        inc     ent_anim_id             ; OAM += 1 (shoot variant)
        lda     current_weapon          ; if weapon == $0A (Shadow Blade):
        cmp     #WPN_SHADOW             ; OAM += 2 (two-handed throw anim)
        bne     shoot_oam_advance_return ; not shadow blade → done
        inc     ent_anim_id             ; OAM += 2 for shadow blade
shoot_oam_advance_return:  pla          ; restore A
        rts                             ; return to caller

; --- revert_shoot_oam: reverse OAM from shooting variant ---

shoot_oam_revert_variant:  pha          ; save A on stack
        dec     ent_anim_id             ; OAM -= 1
        lda     current_weapon          ; if Shadow Blade: OAM -= 2
        cmp     #WPN_SHADOW             ; check for shadow blade
        bne     shoot_oam_revert_return ; not shadow blade → done
        dec     ent_anim_id             ; OAM -= 2 total for shadow blade
shoot_oam_revert_return:  pla           ; restore A from stack
        rts                             ; return to caller

; slide_initiate — check for wall ahead, then start slide
; Called from player_on_ground when Down+A pressed.
; Sets player state to $02 (player_slide), spawns dust cloud at slot 4.

slide_initiate:  lda     joy1_held      ; read D-pad state
        and     #$03                    ; left/right bits
        beq     slide_check_direction   ; no direction → keep current
        sta     player_facing           ; update facing from D-pad
slide_check_direction:  ldy     #$04    ; Y=4 (check right) or Y=5 (check left)
        lda     player_facing           ; based on facing direction
        and     #$01                    ; bit 0 = right
        bne     slide_wall_collision    ; facing right → skip INY
        iny                             ; facing left → Y=5
slide_wall_collision:  jsr     check_tile_horiz ; check for wall ahead at slide height
        lda     $10                     ; collision result
        and     #$10                    ; bit 4 = solid wall
        beq     slide_initiate_confirmed ; no wall → can slide
        rts                             ; wall ahead → abort

; slide confirmed — set up player state and dust cloud

slide_initiate_confirmed:  lda     #PSTATE_SLIDE ; state $02 = player_slide
        sta     player_state            ; set player state to slide
        lda     #$14                    ; slide timer = 20 frames
        sta     $33                     ; store slide timer
        lda     #$10                    ; OAM anim $10 = slide sprite
        jsr     reset_sprite_anim       ; set slide sprite animation
        lda     ent_y_px                ; player Y += 2 (crouch posture)
        clc                             ; prepare for addition
        adc     #$02                    ; lower Y by 2 for slide crouch
        sta     ent_y_px                ; store adjusted Y position
        lda     #$80                    ; X speed = $02.80 = 2.5 px/frame
        sta     ent_xvel_sub            ; X velocity sub-pixel
        lda     #$02                    ; (whole)
        sta     ent_xvel                ; set X velocity whole
        lda     #$80                    ; slot 4 active flag = $80
        sta     $0304                   ; (mark dust cloud entity active)
        lda     ent_flags               ; player sprite flags
        sta     $0584                   ; copy to dust cloud flags
        ldx     #$04                    ; slot 4: OAM anim $17 = slide dust
        lda     #$17                    ; dust cloud OAM = $17
        jsr     reset_sprite_anim       ; set dust cloud animation
        lda     ent_x_px                ; player X pixel
        sta     $0364                   ; copy to dust cloud X pixel
        lda     ent_x_scr               ; player X screen
        sta     $0384                   ; copy to dust cloud X screen
        lda     ent_y_px                ; player Y pixel
        sta     $03C4                   ; copy to dust cloud Y pixel
        lda     ent_y_scr               ; player Y screen
        sta     $03E4                   ; copy to dust cloud Y screen
        ldx     #$00                    ; restore X to player slot
        stx     $0484                   ; slot 4 damage flags = 0 (harmless)
        stx     $0324                   ; slot 4 main routine = 0 (no AI)
        beq     slide_move_check_facing ; always → skip to slide movement

; player state $02: sliding on ground
; Speed: $02.80 = 2.5 px/frame. Timer in $33 (starts at $14 = 20 frames).
; First 8 frames: uncancellable. Frames 9-20: cancellable with A (slide jump).
; Exits: timer expires, direction change, wall hit, ledge, or A button.
player_slide:
        ldy     #$02                    ; apply $99 (Y=2 = slide hitbox)
        jsr     move_vertical_gravity   ; apply gravity with slide hitbox
        bcc     slide_end_clear_state   ; C=0 → no ground, fell off ledge
        lda     joy1_held               ; read held D-pad
        and     #$03                    ; mask left/right bits
        beq     slide_move_check_facing ; no direction → keep sliding
        cmp     player_facing           ; same as current facing?
        beq     slide_move_check_facing ; yes → keep sliding
        sta     player_facing           ; changed direction → end slide
        bne     slide_end_ground_check  ; always taken → end slide
slide_move_check_facing:  lda     player_facing ; facing direction: bit 0 = right
        and     #$01                    ; isolate right-facing bit
        beq     slide_move_left_setup   ; bit 0 clear = facing left
        ldy     #$02                    ; slide right
        jsr     move_right_collide      ; check wall collision moving right
        jmp     slide_collision_wall_solid ; check wall collision result

slide_move_left_setup:  ldy     #$03    ; slide left
        jsr     move_left_collide       ; move left with collision
slide_collision_wall_solid:  lda     $10 ; hit a wall?
        and     #$10                    ; bit 4 = solid wall
        bne     slide_end_ground_check  ; yes → end slide
        lda     $33                     ; timer expired?
        beq     slide_end_ground_check  ; yes → end slide
        dec     $33                     ; decrement timer
        lda     $33                     ; timer >= $0C (first 8 frames)?
        cmp     #$0C                    ; first 8 frames threshold
        bcs     slide_continue_moving   ; yes → uncancellable, keep sliding
        lda     joy1_press              ; A button pressed? (cancellable phase)
        and     #BTN_A                  ; check A button
        bne     slide_end_ground_check  ; yes → cancel into slide jump
slide_continue_moving:  rts             ; keep sliding; return

; slide_end — transition out of slide state
; Checks if grounded: on ground → standing. Airborne + A → slide jump. Else → fall.

slide_end_ground_check:  ldy     #$01   ; check ground at standing height
        jsr     check_tile_horiz        ; check tile at standing height
        lda     $10                     ; solid ground under feet?
        and     #$10                    ; bit 4 = solid
        bne     slide_end_on_ground_return ; on ground → return to standing
        sta     $33                     ; clear timer and state
        sta     player_state            ; state = on_ground
        lda     joy1_press              ; A button pressed?
        and     #BTN_A                  ; check A button
        bne     slide_jump_walk_speed_init ; yes → slide jump
        beq     slide_end_fall_anim     ; no → fall from slide
slide_end_clear_state:  lda     #$00    ; clear timer
        sta     $33                     ; clear slide timer
        lda     #$01                    ; OAM anim $01 = airborne
        bne     slide_end_anim_set      ; always taken → set anim
slide_end_fall_anim:  lda     #$04      ; OAM anim $04 = jump/fall
slide_end_anim_set:  jsr     reset_sprite_anim ; set fall/airborne animation
        lda     #$4C                    ; walk speed X = $01.4C (decelerate
        sta     ent_xvel_sub            ; from slide 2.5 to walk 1.297)
        lda     #$01                    ; X velocity whole = 1
        sta     ent_xvel                ; X velocity = 1 (walk speed)
        lda     #$00                    ; state $00 = on_ground (will fall)
        sta     player_state            ; revert to on_ground state
slide_end_on_ground_return:  rts        ; return to caller

slide_jump_walk_speed_init:  lda     #$4C ; walk speed X = $01.4C
        sta     ent_xvel_sub            ; X velocity sub = $4C
        lda     #$01                    ; X velocity whole = 1
        sta     ent_xvel                ; X velocity = 1
        jmp     player_ground_normal_jump ; jump from slide (uses slide_jump velocity)

; --- check ladder/down-entry from ground or slide ---

ladder_check_entry_ground:  lda     ent_y_scr ; if Y screen != 0 (off main screen),
        bne     ladder_check_return     ; skip ladder check
        lda     joy1_held               ; pressing Up?
        and     #BTN_UP                 ; (Up = bit 3)
        beq     ladder_down_entry_check ; no → check Down instead
        php                             ; save flags (carry = came from slide?)
check_ladder_entry:  ldy     #$04       ; check tile at Y=4 offset
        jsr     check_tile_collision    ; check tile type at offset
        lda     tile_at_feet_hi         ; tile type at feet (high)
        cmp     #TILE_LADDER            ; ladder tile ($20)?
        beq     ladder_enter_from_up    ; ladder top ($40)
        cmp     #TILE_LADDER_TOP        ; ladder top tile ($40)?
        beq     ladder_enter_from_up    ; yes → enter ladder
        lda     tile_at_feet_lo         ; check other foot tile
        cmp     #TILE_LADDER            ; ladder?
        beq     ladder_enter_from_up    ; yes → enter ladder
        cmp     #TILE_LADDER_TOP        ; ladder top tile?
        bne     ladder_not_found_restore ; no ladder found

; --- enter ladder (from Up press) ---
ladder_enter_from_up:  plp              ; restore flags
        lda     $12                     ; center player X on ladder tile
        and     #$F0                    ; snap to 16-pixel grid
        ora     #$08                    ; then offset +8 (center of tile)
        sta     ent_x_px                ; set player X to ladder center
        lda     #$0A                    ; OAM $0A = ladder climb anim

; --- enter_ladder_common: set state $03 and reset Y speed ---
ladder_enter_common:  jsr     reset_sprite_anim ; set OAM animation (A = OAM ID)
        lda     #PSTATE_LADDER          ; player state = $03 (on ladder)
        sta     player_state            ; set state to on ladder
        lda     #$4C                    ; Y speed = $01.4C (climb speed)
        sta     ent_yvel_sub            ; (same as walk speed)
        lda     #$01                    ; Y velocity whole = 1
        sta     ent_yvel                ; set Y velocity whole
        lda     #$00                    ; $32 = 0 (clear walk/shoot sub-state)
        sta     walk_flag               ; clear walk/shoot flag
        clc                             ; carry clear = entered ladder
        rts                             ; return to caller

ladder_not_found_restore:  plp          ; no ladder found — restore flags
ladder_check_return:  rts               ; return to caller

; --- check Down+ladder entry (drop through ladder top) ---

ladder_down_entry_check:  lda     joy1_held ; pressing Down?
        and     #BTN_DOWN               ; (Down = bit 2)
        beq     ladder_check_return     ; no → return
        php                             ; save flags
        lda     tile_at_feet_lo         ; foot tile = $40 (ladder top)?
        cmp     #TILE_LADDER_TOP        ; if not, fall through to normal
        bne     check_ladder_entry      ; ladder check (check both feet)
        plp                             ; ladder top confirmed
        lda     ent_x_px                ; center X on ladder (AND $F0 ORA $08)
        and     #$F0                    ; snap to 16-pixel grid
        ora     #$08                    ; center on tile (+8)
        sta     ent_x_px                ; set player X to ladder center
        lda     $11                     ; snap Y to tile boundary
        and     #$F0                    ; (top of ladder tile)
        sta     ent_y_px                ; player Y = aligned
        lda     #$14                    ; OAM $14 = ladder dismount (going down)
        bne     ladder_enter_common     ; (always taken)

; ===========================================================================
; player state $03: on ladder — climb up/down, fire, jump off
; ===========================================================================
player_ladder:
        jsr     shoot_timer_tick_dec    ; handle shoot timer countdown

; --- B button: fire weapon on ladder ---
        lda     joy1_press              ; B button pressed?
        and     #BTN_B                  ; mask B button bit
        beq     ladder_check_climb_input ; no → skip shooting
        lda     joy1_held               ; D-pad L/R while shooting?
        and     #$03                    ; (aim direction on ladder)
        beq     ladder_fire_weapon      ; no → fire in current direction
        cmp     player_facing           ; same as current facing?
        beq     ladder_fire_weapon      ; yes → fire normally
        sta     player_facing           ; update facing direction
        lda     ent_flags               ; toggle H-flip (bit 6)
        eor     #ENT_FLAG_HFLIP         ; flip horizontal sprite flag
        sta     ent_flags               ; update sprite flags
ladder_fire_weapon:  jsr     weapon_fire ; fire weapon

; --- check climbing input ---
ladder_check_climb_input:  lda     walk_flag ; if shooting: return to shoot-on-ladder
        bne     ladder_check_return     ; (OAM animation handler above)
        lda     joy1_held               ; D-pad U/D held?
        and     #$0C                    ; mask Up/Down bits
        bne     ladder_climb_save_dpad  ; yes → climb
        lda     joy1_press              ; A button → jump off ladder
        and     #BTN_A                  ; mask A button bit
        bne     ladder_jump_off         ; A pressed → jump off
        jmp     ladder_idle_freeze_anim ; no input → idle on ladder (freeze anim)

ladder_jump_off:  jmp     ladder_detach_to_ground ; detach from ladder → ground state

; --- climb up/down: center X on ladder, move vertically ---

ladder_climb_save_dpad:  pha            ; save D-pad bits
        lda     ent_x_px                ; center player X on ladder tile:
        and     #$F0                    ; snap to 16px grid + 8
        ora     #$08                    ; center on tile (+8)
        sta     ent_x_px                ; set player X to ladder center
        pla                             ; bit 2 = down pressed?
        and     #$04                    ; isolate down bit
        beq     ladder_climb_up_start   ; no → climb up

; --- climb down ---
        lda     #$0A                    ; OAM $0A = climb-down animation
        cmp     ent_anim_id             ; (skip reset if already playing)
        beq     ladder_climb_down_start ; already playing → skip reset
        jsr     reset_sprite_anim       ; set climb-down animation
ladder_climb_down_start:  ldy     #$00  ; move down with collision
        jsr     move_down_collide       ; move player downward
        bcs     ladder_detach_to_ground ; hit ground → detach
        lda     ent_y_scr               ; if Y screen > 0: wrap Y to 0
        beq     ladder_down_tile_check  ; (scrolled past screen boundary)
        lda     #$00                    ; Y = 0 (top of screen)
        sta     ent_y_px                ; wrap Y to 0 (top of screen)
        rts                             ; return to caller

; --- check if still on ladder tile (going down) ---

ladder_down_tile_check:  ldy     #$04   ; check tile collision at feet
        jsr     check_tile_collision    ; check tile type at feet
        lda     tile_at_feet_hi         ; check high foot tile
        cmp     #TILE_LADDER            ; ladder body ($20)?
        beq     ladder_idle_return      ; yes → stay on ladder
        cmp     #TILE_LADDER_TOP        ; ladder top ($40)?
        beq     ladder_idle_return      ; yes → stay on ladder
        lda     tile_at_feet_lo         ; check low foot tile
        cmp     #TILE_LADDER            ; ladder body?
        beq     ladder_idle_return      ; yes → stay on ladder
        cmp     #TILE_LADDER_TOP        ; ladder top tile?
        beq     ladder_idle_return      ; yes → stay on ladder
        bne     ladder_detach_to_ground ; no ladder → detach

; --- climb up ---
ladder_climb_up_start:  ldy     #$01    ; move up with collision
        jsr     move_up_collide         ; move up with collision check
        bcs     ladder_idle_freeze_anim ; hit ceiling → freeze anim
        lda     ent_y_scr               ; if Y screen > 0: wrap Y to $EF
        beq     ladder_up_tile_check    ; (scrolled past screen boundary)
        lda     #$EF                    ; wrap Y to bottom of screen
        sta     ent_y_px                ; set Y to $EF (screen bottom)
        rts                             ; return to caller

; --- check if still on ladder tile (going up) ---

ladder_up_tile_check:  lda     ent_y_px ; if Y < $10: stay on ladder
        cmp     #$10                    ; (near top of screen)
        bcc     ladder_idle_return      ; near top → stay on ladder
        ldy     #$04                    ; check tile collision
        jsr     check_tile_collision    ; check tiles at player feet
        lda     tile_at_feet_hi         ; check high foot tile
        cmp     #TILE_LADDER            ; ladder body ($20)?
        beq     ladder_idle_return      ; yes → stay on ladder
        cmp     #TILE_LADDER_TOP        ; ladder top tile?
        beq     ladder_idle_return      ; yes → stay on ladder
        lda     tile_at_feet_lo         ; check low foot tile
        cmp     #TILE_LADDER            ; ladder body?
        beq     ladder_idle_return      ; yes → stay on ladder
        cmp     #TILE_LADDER_TOP        ; ladder top ($40)?
        beq     ladder_idle_return      ; yes → stay on ladder

; --- reached top of ladder: dismount ---
        lda     #$14                    ; OAM $14 = ladder-top dismount animation
        cmp     ent_anim_id             ; already playing dismount anim?
        beq     ladder_top_dismount     ; already dismounting → skip
        jsr     reset_sprite_anim       ; set dismount animation
ladder_top_dismount:  lda     ent_y_px  ; check Y position sub-tile:
        and     #$0F                    ; if Y mod 16 >= 12, still climbing
        cmp     #$0C                    ; Y mod 16 >= 12?
        bcs     ladder_idle_return      ; → stay on ladder

; --- detach from ladder: return to state $00 (on_ground) ---
ladder_detach_to_ground:  lda     #$00  ; $30 = 0 (state = on_ground)
        sta     player_state            ; set state to on_ground
        jmp     reset_gravity           ; clear Y speed/gravity

; --- freeze ladder animation (idle on ladder) ---

ladder_idle_freeze_anim:  lda     #$00  ; freeze animation frame to 0
        sta     ent_anim_frame          ; freeze animation at frame 0
ladder_idle_return:  rts                ; return to caller

; ===========================================================================
; player state $04: respawn/reappear (death, unpause, stage start) [confirmed]
; ===========================================================================
; Teleport beam drops from top of screen. Two phases:
;   Phase 1: apply_y_speed (constant fall) until Y reaches landing threshold
;   Phase 2: move_vertical_gravity (decelerate + land on ground)
; Shadow Man stage uses lower threshold ($30 vs $68) due to different spawn.
player_reappear:

        lda     ent_anim_state          ; ent_anim_state = anim frame counter
        bne     shadow_man_landing_check ; if >0: skip movement (anim playing)

; --- phase 1: constant-speed fall until reaching Y threshold ---
        lda     stage_id                ; Shadow Man stage ($07)?
        cmp     #STAGE_SHADOW           ; compare to Shadow Man stage
        beq     shadow_man_drop_vertical ; yes → use lower landing Y
        lda     ent_y_px                ; normal stages: Y >= $68 → switch to $99
        cmp     #$68                    ; Y pixel >= $68 threshold?
        bcs     shadow_man_landing_gravity ; past threshold → use gravity
        jsr     apply_y_speed           ; fall at constant speed (no $99)
        jmp     shadow_man_landing_clear ; skip to clear anim counter

shadow_man_drop_vertical:  lda     ent_y_px ; Shadow Man: Y >= $30 → switch to $99
        cmp     #$30                    ; Shadow Man Y threshold = $30
        bcs     shadow_man_landing_gravity ; → stay on ladder
        jsr     apply_y_speed           ; fall at constant speed
        jmp     shadow_man_landing_clear ; skip to clear anim counter

; --- phase 2: $99 + ground collision → land ---

shadow_man_landing_gravity:  ldy     #$00 ; apply $99 and check ground
        jsr     move_vertical_gravity   ; carry set = landed
        bcs     shadow_man_landing_check ; landed → proceed to anim check
shadow_man_landing_clear:  lda     #$00 ; clear anim counter (still falling)
        sta     ent_anim_frame          ; reset anim frame to 0

; --- check landing animation progress ---
shadow_man_landing_check:  lda     ent_anim_frame ; if anim counter == 0: not landed yet
        bne     shadow_man_reappear_check ; nonzero → check reappear state
        lda     ent_anim_state          ; if anim frame == 1: play landing sound $34
        cmp     #$01                    ; check for first landing frame
        bne     shadow_man_reappear_check ; not frame 1 → skip sound
        lda     #SFX_TELEPORT           ; teleport landing sound
        jsr     submit_sound_ID         ; play landing sound effect

; --- check reappear animation complete ---
shadow_man_reappear_check:  lda     ent_anim_state ; anim frame == 4 → reappear done
        cmp     #$04                    ; if >0: skip movement (anim playing)
        bne     shadow_man_reappear_return ; not done yet
        lda     #$00                    ; transition to state $00 (on_ground)
        sta     player_state            ; Shadow Man stage ($07)?
        sta     walk_flag               ; clear shooting flag
        sta     invincibility_timer     ; clear invincibility timer
shadow_man_reappear_return:  rts        ; return to caller

shadow_man_reappear_complete:  lda     #$00 ; clear player state
        sta     player_state            ; set state $00 (ground)
        jmp     reset_gravity           ; clear vertical velocity

; ===========================================================================
; player state $05: riding Mag Fly — magnetic pull, Magnet Man stage [confirmed]
; ===========================================================================
; Player is attached to a Mag Fly entity (slot $34). The fly pulls player
; upward magnetically. Player can D-pad up/down to adjust, A to jump off.
; If the Mag Fly dies or changes OAM, player detaches (→ state $00 + $99).
player_entity_ride:

        ldy     entity_ride_slot        ; Y = entity slot being ridden
        lda     ent_status,y            ; check if Mag Fly still active
        bpl     shadow_man_reappear_complete ; inactive → detach (state $00)
        lda     ent_anim_id,y           ; check Mag Fly OAM == $62
        cmp     #$62                    ; (normal Mag Fly animation)
        bne     shadow_man_reappear_complete ; changed → detach

; --- D-pad up/down: manual vertical adjustment ---
        lda     joy1_held               ; D-pad up/down held? (bits 2-3)
        and     #$0C                    ; mask d-pad up ($08) and down ($04)
        beq     mag_fly_ride_check_jump ; no vertical input → skip
        sta     temp_00                 ; save D-pad state
        lda     ent_yvel_sub            ; push current Y velocity (sub)
        pha                             ; save Y velocity sub on stack
        lda     ent_yvel                ; load Y velocity whole
        pha                             ; save Y velocity whole on stack
        lda     #$40                    ; set temp Y speed = $00.40
        sta     ent_yvel_sub            ; set temp Y speed sub = $40
        lda     #$00                    ; temp Y speed whole = 0
        sta     ent_yvel                ; set temp Y velocity whole
        lda     temp_00                 ; reload d-pad bits
        and     #$08                    ; test up bit
        beq     mag_fly_ride_move_down  ; no → move down
        ldy     #$01                    ; move up with collision
        jsr     move_up_collide         ; move up with collision check
        jmp     mag_fly_ride_restore_vel ; restore original velocity

mag_fly_ride_move_down:  ldy     #$00   ; move down with collision
        jsr     move_down_collide       ; move down with collision check
mag_fly_ride_restore_vel:  pla          ; restore original Y velocity
        sta     ent_yvel                ; restore Y velocity whole
        pla                             ; pull Y velocity sub from stack
        sta     ent_yvel_sub            ; restore Y velocity sub

; --- check jump off / horizontal movement when stationary ---
mag_fly_ride_check_jump:  lda     ent_yvel_sub ; check if Y velocity is zero
        ora     ent_yvel                ; combine sub + whole
        bne     mag_fly_ride_magnetic_pull ; nonzero → being pulled by Mag Fly
        lda     joy1_press              ; check A button for jump
        and     #BTN_A                  ; mask A button bit
        beq     mag_fly_ride_horizontal_idle ; no A → stay on Mag Fly
        jmp     player_ground_normal_jump ; jump off Mag Fly

; --- idle on Mag Fly: walk horizontally using ride direction ---

mag_fly_ride_horizontal_idle:  lda     #$00 ; set walk speed to $01.4C (but use
        sta     ent_xvel_sub            ; x_speed_sub = $00
        lda     #$01                    ; x_speed = $01 (walk speed $01.00)
        sta     ent_xvel                ; x_speed = $01
        lda     ent_flags               ; save current sprite flags
        pha                             ; save sprite flags on stack
        lda     facing_sub              ; load ride direction from facing_sub
        jsr     player_ground_move_horizontal ; move horizontally in ride direction
        pla                             ; restore sprite flags (preserve facing)
        sta     ent_flags               ; restore sprite flags

; --- magnetic pull: constant upward force with $99 cap ---
mag_fly_ride_magnetic_pull:  ldy     #$01 ; move upward with collision check
        jsr     move_up_collide         ; move upward with collision
        lda     ent_yvel_sub            ; add gravity sub-pixel to Y velocity
        clc                             ; counteracts magnetic pull
        adc     #$40                    ; add gravity increment ($40)
        sta     ent_yvel_sub            ; store updated Y velocity sub
        lda     ent_yvel                ; load Y velocity whole
        adc     #$00                    ; add carry to Y velocity whole
        sta     ent_yvel                ; store updated Y velocity whole
        cmp     #$07                    ; cap Y speed at $07 (terminal velocity)
        bcc     mag_fly_ride_walk_speed ; below cap → skip clamp
        lda     #$00                    ; clamp Y speed to cap
        sta     ent_yvel_sub            ; clear sub-pixel when capping
mag_fly_ride_walk_speed:  lda     #$4C  ; x_speed_sub = $4C
        sta     ent_xvel_sub            ; set X velocity sub = $4C
        lda     #$01                    ; x_speed = $01 (walk speed $01.4C)
        sta     ent_xvel                ; set X velocity whole = $01
        jsr     player_airborne_walk_shoot ; horizontal movement + animation
        rts                             ; return to caller

; ===========================================================================
; player state $06: taking damage — knockback with $99
; ===========================================================================
; On first call: saves current OAM, sets damage animation, spawns hit flash
; (slot 4), applies $99 + knockback movement. Knockback direction is
; opposite to facing (ent_flags bit 6). On subsequent frames, continues knockback
; physics until player touches ground or ent_anim_state signals landing ($09).
; $3E = saved OAM ID (to restore after damage animation ends).
; ---------------------------------------------------------------------------
player_damage:                          ; save player sprite flags

        lda     ent_anim_id             ; check current OAM ID
        cmp     #$11                    ; OAM $11 = normal damage pose
        beq     player_knockback_physics ; yes → skip init, continue knockback
        cmp     #$B1                    ; OAM $B1 = Rush Marine damage pose
        beq     player_knockback_physics ; yes → skip init

; --- first frame: initialize damage state ---
        lda     ent_anim_id             ; save current OAM to $3E
        sta     $3E                     ; (restored when damage ends)
        cmp     #$D9                    ; OAM >= $D9 = Rush Marine variants
        bcs     player_damage_rush_marine ; → use $B1 damage anim
        lda     #$11                    ; normal damage OAM = $11
        bne     player_damage_anim_set  ; always taken (A != 0)
player_damage_rush_marine:  lda     #$B1 ; Rush Marine damage OAM = $B1
player_damage_anim_set:  jsr     reset_sprite_anim ; set player damage animation
        lda     #$00                    ; clear walk/shoot flag
        sta     walk_flag               ; clear walk/shoot flag
        jsr     reset_gravity           ; reset vertical velocity

; --- spawn hit flash effect in slot 4 ---
        lda     #$80                    ; activate hit flash in slot 4
        sta     $0304                   ; activate slot 4 entity
        lda     ent_flags               ; copy player facing to slot 4
        sta     $0584                   ; ent_flags for slot 4
        ldx     #$04                    ; X = slot 4
        lda     #$12                    ; hit flash OAM = $12
        jsr     reset_sprite_anim       ; set hit flash animation
        lda     ent_x_px                ; slot 4 position = player position
        sta     $0364                   ; copy player X to slot 4
        lda     ent_x_scr               ; load player X screen
        sta     $0384                   ; copy X screen to slot 4
        lda     ent_y_px                ; load player Y pixel
        sta     $03C4                   ; copy Y pixel to slot 4
        lda     ent_y_scr               ; load player Y screen
        sta     $03E4                   ; copy Y screen to slot 4
        ldx     #$00                    ; restore X = player slot 0
        stx     $0484                   ; clear hitbox for slot 4
        stx     $0324                   ; clear routine for slot 4

; --- knockback physics: $99 + horizontal recoil ---
player_knockback_physics:  lda     $3E  ; check saved OAM for special cases:
        cmp     #$10                    ; $10 = climbing (no knockback physics)
        beq     player_knockback_landing ; climbing → skip knockback
        cmp     #$79                    ; >= $79 = special state (skip physics)
        bcs     player_knockback_landing ; special OAM → skip knockback physics
        ldy     #$00                    ; Y = 0 (player slot)
        jsr     move_vertical_gravity   ; apply gravity, carry = landed
        lda     #$80                    ; knockback speed = $00.80 (half pixel/frame)
        sta     ent_xvel_sub            ; set knockback X speed sub
        lda     #$00                    ; knockback X speed whole = 0
        sta     ent_xvel                ; x_speed whole = 0
        lda     ent_flags               ; save facing before move
        pha                             ; save facing on stack
        lda     ent_flags               ; check facing direction bit
        and     #ENT_FLAG_HFLIP         ; knockback goes OPPOSITE to facing
        bne     player_knockback_move_left ; facing right → knock left
        ldy     #$00                    ; facing left → knock right
        jsr     move_right_collide      ; move right with collision
        jmp     player_knockback_slot4_sync ; sync slot 4 with player

player_knockback_move_left:  ldy     #$01 ; facing right → knock left
        jsr     move_left_collide       ; move left with collision
player_knockback_slot4_sync:  lda     ent_x_px ; sync hit flash X with player
        sta     $0364                   ; copy player X to hit flash
        lda     ent_x_scr               ; sync hit flash screen with player
        sta     $0384                   ; copy X screen to hit flash
        pla                             ; restore original facing direction
        sta     ent_flags               ; restore facing flags

; --- check for landing: ent_anim_state = $09 means landed on ground ---
player_knockback_landing:  lda     ent_anim_state ; ent_anim_state = animation/collision state
        cmp     #$09                    ; $09 = landed after knockback
        bne     player_knockback_done   ; not landed → continue knockback

; --- landed: restore OAM and start invincibility frames ---
        lda     $3E                     ; saved OAM determines recovery anim
        pha                             ; save OAM on stack
        cmp     #$10                    ; $10 = climbing → reset anim
        beq     player_knockback_restore_oam ; climbing → restore anim
        cmp     #$D9                    ; >= $D9 = Rush Marine → reset anim
        bcc     player_knockback_state_check ; else keep current anim
player_knockback_restore_oam:  jsr     reset_sprite_anim ; reset to saved OAM
player_knockback_state_check:  pla      ; determine return state:
        cmp     #$10                    ; $10 (climbing) → state $02
        beq     player_knockback_return_climb ; climbing → return to ladder state
        cmp     #$D9                    ; >= $D9 (Rush Marine) → state $08
        bcc     player_knockback_return_normal ; else → state $00 (normal)
        lda     #$08                    ; return state = $08 (Rush Marine)
        bne     player_knockback_set_invincible ; branch always taken (A != 0)
player_knockback_return_climb:  lda     #$02 ; return state = $02 (climbing)
        bne     player_knockback_set_invincible ; branch always taken (A != 0)
player_knockback_return_normal:  lda     #$00 ; return state = $00 (normal)
player_knockback_set_invincible:  sta     player_state ; set return player state
        lda     #$3C                    ; $39 = $3C (60 frames invincibility)
        sta     invincibility_timer     ; 60 frames of invincibility
        lda     ent_anim_frame          ; set invincible blink flag (bit 7)
        ora     #$80                    ; set blink flag (bit 7)
        sta     ent_anim_frame          ; store blink-flagged anim frame
player_knockback_done:  rts             ; return to caller

; ===========================================================================
; player state $0E: dead — explosion burst on all sprite slots
; ===========================================================================
; First frame: copies player position to slots 0-15, assigns each slot
; a unique velocity vector from $D7F1 tables (radial burst pattern).
; All slots get OAM $7A (explosion sprite), routine $10 (generic mover).
; Slot 0 (player) gets special upward velocity ($03.00) for the "body
; flies up" effect. Subsequent frames: move slot 0 upward, increment
; death timer ($3F/$3C = 16-bit counter).
; ---------------------------------------------------------------------------
player_death:

        lda     #$00                    ; clear hazard pending flag
        sta     hazard_pending          ; clear hazard flag
        lda     ent_anim_id             ; already set to explosion OAM?
        cmp     #$7A                    ; $7A = explosion sprite
        beq     death_explosion_move_upward ; yes → skip init, continue upward motion
        lda     ent_y_scr               ; y_screen != 0 → off-screen, skip to timer
        bne     death_explosion_inc_timer ; off-screen → skip to timer

; --- init 16 explosion fragments (slots 0-15) ---
        ldy     #$0F                    ; Y = slot index (15 down to 0)
death_explosion_init_frags:  lda     #$7A ; OAM = $7A (explosion sprite)
        sta     ent_anim_id,y           ; set explosion sprite for this slot
        lda     #$00                    ; clear misc fields
        sta     ent_anim_frame,y        ; ent_anim_frame = 0
        sta     ent_anim_state,y        ; ent_anim_state = 0
        sta     ent_hitbox,y            ; hitbox = 0 (no collision)
        lda     ent_x_px                ; copy player position to this slot
        sta     ent_x_px,y              ; x_pixel
        lda     ent_x_scr               ; x_screen
        sta     ent_x_scr,y             ; copy X screen to slot
        lda     ent_y_px                ; y_pixel
        sta     ent_y_px,y              ; copy Y pixel to slot
        lda     ent_y_scr               ; y_screen
        sta     ent_y_scr,y             ; copy Y screen to slot
        lda     #$10                    ; routine = $10 (generic mover — applies velocity)
        sta     ent_routine,y           ; set AI routine for slot
        lda     #$80                    ; type = $80 (active entity)
        sta     ent_status,y            ; mark slot as active
        lda     #$90                    ; flags = $90 (active + bit 4)
        sta     ent_flags,y             ; set sprite flags for slot
        lda     death_burst_x_speed_sub,y ; x_speed_sub from radial velocity table
        sta     ent_xvel_sub,y          ; set X velocity sub for slot
        lda     death_burst_x_speed,y   ; x_speed from radial velocity table
        sta     ent_xvel,y              ; set X velocity whole for slot
        lda     death_burst_y_speed_sub,y ; y_speed_sub from radial velocity table
        sta     ent_yvel_sub,y          ; set Y velocity sub for slot
        lda     death_burst_y_speed,y   ; y_speed from radial velocity table
        sta     ent_yvel,y              ; set Y velocity whole for slot
        dey                             ; loop for all 16 slots
        bpl     death_explosion_init_frags ; loop all 16 slots

; --- override slot 0 (player body): flies straight up ---
        lda     #$00                    ; y_speed_sub = 0
        sta     ent_yvel_sub            ; clear Y velocity sub
        lda     #$03                    ; y_speed = $03 (3.0 px/frame upward,
        sta     ent_yvel                ; stored as positive = up for move_sprite_up)

; --- ongoing: move player body upward ---
death_explosion_move_upward:  jsr     move_sprite_up ; move slot 0 up at $03.00/frame

; --- increment death timer ($3F low byte, $3C high byte) ---
death_explosion_inc_timer:  inc     $3F ; $3F++, if overflow then $3C++
        bne     death_burst_table_start ; no overflow → return (data follows)
        inc     $3C                     ; high byte of death timer
death_burst_table_start:  .byte   $60   ; rts encoded as data byte

; radial velocity tables for death explosion (16 fragments, index 0-15)
; each fragment gets a unique direction vector for the burst pattern
; format: x_speed_sub, x_speed, y_speed_sub, y_speed per slot
death_burst_x_speed_sub:  .byte   $00,$1F,$00,$1F,$00,$E1,$00,$E1
        .byte   $00,$0F,$80,$0F,$00,$F1,$80,$F1
death_burst_x_speed:  .byte   $00,$02,$03,$02,$00,$FD,$FD,$FD
        .byte   $00,$01,$01,$01,$00,$FE,$FE,$FE
death_burst_y_speed_sub:  .byte   $00,$E1,$00,$1F,$00,$1F,$00,$E1
        .byte   $80,$F1,$00,$0F,$80,$0F,$00,$F1
death_burst_y_speed:  .byte   $FD,$FD,$00,$02,$03,$02,$00,$FD
        .byte   $FE,$FE,$00,$01,$01,$01,$00,$FE
player_special_death:  lda     #$00  ; state $07: Doc Flash Time Stopper kill
        sta     ent_anim_frame          ; reset animation frame
        dec     ent_timer               ; decrement explosion animation timer
        bne     death_explosion_loop_return ; not zero → wait
        lda     #$1E                    ; reset timer = $1E (30 frames per cycle)
        sta     ent_timer               ; reset timer to 30 frames
        ldy     #$68                    ; Y = last OAM offset (27 entries, 4 bytes each)
death_explosion_oam_inc_tile:  lda     $0201,y ; tile ID = $0201 + Y (byte 1 of OAM entry)
        clc                             ; prepare for addition
        adc     #$01                    ; increment tile ID
        cmp     #$FA                    ; if reached $FA, wrap to $F7
        bne     death_explosion_oam_write ; not at wrap point → write tile
        lda     #$F7                    ; wrap back to tile $F7
death_explosion_oam_write:  sta     $0201,y ; write back modified tile
        dey                             ; next OAM entry (Y -= 4)
        dey                             ; skip OAM byte 2 (attribute)
        dey                             ; skip OAM byte 3 (X pos)
        dey                             ; skip OAM byte 0 (Y pos)
        bpl     death_explosion_oam_inc_tile ; loop until Y < 0
death_explosion_loop_return:  rts       ; return to caller

; ===========================================================================
; player state $08: riding Rush Marine submarine [confirmed]
; ===========================================================================
; Handles two modes based on environment:
;   - On land (tile != water $80): OAM $DB, normal $99 + d-pad L/R movement
;   - In water (tile == $80): OAM $DC, slow sink ($01.80/frame), d-pad U/D movement
;     A button = jump/thrust upward ($04.E5). When thrusting up in water,
;     checks if surfacing (tile above != water) → apply jump velocity instead.
; Drains weapon ammo periodically via decrease_ammo.check_frames.
; Weapon firing (B button) and horizontal movement ($16 bits 0-1) handled
; in both modes.
; ---------------------------------------------------------------------------
player_rush_marine:

        jsr     decrease_ammo_tick      ; drain weapon ammo over time
        lda     ent_anim_id             ; intro anim check: $DA = mounting Rush
        cmp     #$DA                    ; compare to mount anim ($DA)
        bne     rush_marine_check_env   ; not mounting → skip
        lda     ent_anim_state          ; wait for mount anim to complete
        cmp     #$03                    ; anim state $03 = done
        bne     death_explosion_loop_return ; not done → wait (returns via earlier RTS)
        lda     #$DB                    ; transition to land-mode OAM
        jsr     reset_sprite_anim       ; set land-mode animation

; --- check environment: water or land ---
rush_marine_check_env:  ldy     #$06    ; check tile at player's feet
        jsr     check_tile_horiz        ; Y=$06 = below center
        lda     $10                     ; $10 = tile type result
        cmp     #$80                    ; $80 = water tile
        beq     rush_marine_water_check ; water → underwater mode

; --- land mode: $99 + horizontal movement ---
        lda     #$DB                    ; ensure land-mode OAM = $DB
        cmp     ent_anim_id             ; compare to current OAM
        beq     rush_marine_land_physics ; already set → skip anim reset
        jsr     reset_sprite_anim       ; switch to $DB anim
        jmp     rush_marine_land_physics ; skip to land physics

; --- water mode: buoyancy + vertical d-pad ---

rush_marine_water_check:  lda     #$DC  ; ensure water-mode OAM = $DC
        cmp     ent_anim_id             ; already water OAM?
        beq     rush_marine_water_physics ; already set → skip
        jsr     reset_sprite_anim       ; switch to $DC anim
        jmp     rush_marine_water_physics ; skip to water physics

; --- land physics: $99 + A=jump ---

rush_marine_land_physics:  ldy     #$00 ; apply $99
        jsr     move_vertical_gravity   ; apply gravity
        bcc     rush_marine_check_dpad  ; airborne → check horizontal input
        lda     joy1_press              ; A button pressed? (bit 7)
        and     #BTN_A                  ; mask A button bit
        beq     rush_marine_weapon_fire ; no → skip to weapon check
rush_marine_jump_vel_set:  lda     #$E5 ; y_speed = $04.E5 (jump velocity, same as normal)
        sta     ent_yvel_sub            ; set Y velocity sub
        lda     #$04                    ; Y velocity whole = $04
        sta     ent_yvel                ; set jump velocity whole
        ldy     #$00                    ; apply $99 this frame too (for smooth arc)
        jsr     move_vertical_gravity   ; apply gravity again for smooth arc
rush_marine_check_dpad:  lda     joy1_held ; check d-pad left/right held
        and     #$03                    ; mask left/right bits
        beq     rush_marine_weapon_fire ; no L/R → skip to weapon check
        sta     player_facing           ; set facing direction from input
        jsr     player_ground_move_horizontal ; horizontal movement handler
        jmp     rush_marine_weapon_fire ; skip to weapon check

; --- water physics: slow sink + A=thrust, d-pad U/D ---

rush_marine_water_physics:  lda     joy1_press ; A button pressed?
        and     #BTN_A                  ; mask A button bit
        beq     rush_marine_slow_sink   ; no → slow sink
        ldy     #$01                    ; check tile above (Y=$01)
        jsr     check_tile_horiz        ; check tile type above player
        lda     $10                     ; still water above?
        cmp     #$80                    ; water tile above?
        bne     rush_marine_jump_vel_set ; surfacing → apply jump velocity

; --- underwater: no thrust, slow sink ---
rush_marine_slow_sink:  lda     joy1_held ; d-pad L/R?
        and     #$03                    ; mask left/right bits
        beq     rush_marine_sink_vel_set ; no → skip horizontal
        sta     player_facing           ; set facing from d-pad input
        jsr     player_ground_move_horizontal ; move horizontally
rush_marine_sink_vel_set:  lda     #$80 ; y_speed = $01.80 (slow sink in water)
        sta     ent_yvel_sub            ; y_speed_sub = $80
        lda     #$01                    ; Y velocity whole = $01
        sta     ent_yvel                ; y_speed = $01 (sink at $01.80/frame)
        lda     joy1_held               ; check d-pad up/down held
        and     #$0C                    ; mask up/down bits
        beq     rush_marine_weapon_fire ; no → skip to weapon
        and     #$04                    ; bit 2 = down
        bne     rush_marine_move_down   ; down → move down

; --- d-pad up: move up, snap to surface if surfacing ---
        ldy     #$01                    ; move up with collision
        jsr     move_up_collide         ; move up with collision check
        ldy     #$06                    ; re-check water tile at feet
        jsr     check_tile_horiz        ; re-check tile type at feet
        lda     $10                     ; still in water?
        cmp     #$80                    ; still in water?
        beq     rush_marine_surface_snap ; yes → done
        lda     ent_y_px                ; surfaced: snap Y to next tile boundary
        and     #$F0                    ; (align to water surface)
        clc                             ; prepare for addition
        adc     #$10                    ; align to next 16px tile boundary
        sta     ent_y_px                ; snap Y to surface boundary
rush_marine_surface_snap:  jmp     rush_marine_weapon_fire ; no L/R → skip to weapon check

; --- d-pad down ---

rush_marine_move_down:  ldy     #$00    ; move down with collision
        jmp     move_down_collide       ; move down and return

; --- common: handle facing direction + weapon fire ---

rush_marine_weapon_fire:  lda     ent_anim_id ; save current OAM (Rush Marine sprite)
        pha                             ; save OAM (restore after shoot pose)
        jsr     shoot_timer_tick_dec    ; handle facing direction change
        lda     joy1_press              ; check B button for weapon fire
        and     #BTN_B                  ; mask B button bit
        beq     rush_marine_clear_shoot ; no → skip
        jsr     weapon_fire             ; fire weapon (changes OAM to shoot pose)
rush_marine_clear_shoot:  lda     #$00  ; clear shoot timer
        sta     walk_flag               ; clear walk/shoot flag
        pla                             ; restore Rush Marine OAM (undo shoot pose change)
        sta     ent_anim_id             ; restore Rush Marine OAM
        rts                             ; horizontal movement

; ===========================================================================
; player state $09: frozen during boss intro (shutters/HP bar) [confirmed]
; ===========================================================================
; Player stands still while boss intro plays (shutters close, HP bars fill).
; Applies $99 until grounded, then sets standing OAM. Handles two phases:
;   1. Wily4 ($22=$10): progressively draws boss arena nametable (column $1A)
;   2. HP bar fill: boss_hp_display increments from $80 to HEALTH_FULL (28 ticks),
;      playing SFX_HP_FILL each tick. When it reaches HEALTH_FULL, returns to state $00.
; ---------------------------------------------------------------------------
player_boss_wait:

        ldy     #$00                    ; apply $99
        jsr     move_vertical_gravity   ; apply gravity
        bcc     boss_intro_return       ; airborne → wait for landing

; --- grounded: set to standing pose ---
        lda     #$01                    ; OAM $01 = standing
        cmp     ent_anim_id             ; already standing?
        beq     boss_intro_stage_check  ; yes → skip anim reset
        sta     ent_anim_id             ; set standing OAM
        lda     #$00                    ; clear animation fields
        sta     ent_anim_frame          ; clear anim state
        sta     ent_anim_state          ; clear animation state

; --- Wily4 special: draw boss arena nametable progressively ---
boss_intro_stage_check:  lda     stage_id ; check current stage
        cmp     #STAGE_WILY5            ; check for Wily5 stage
        bne     boss_intro_hp_bar_fill  ; not Wily4 → skip to HP fill
        ldy     #$26                    ; $52 = $26 (set viewport for Wily4 arena)
        cpy     $52                     ; already set?
        beq     boss_intro_nt_render_check ; yes → check render progress
        sty     $52                     ; first time: initialize nametable render
        ldy     #$00                    ; $A000 bank target = 0
        sty     MMC3_MIRRORING          ; clear MMC3 mirroring
        sty     $70                     ; render progress counter = 0
        sty     $28                     ; column counter = 0
        beq     boss_intro_bank_switch  ; always taken → bank switch
boss_intro_nt_render_check:  ldy     $70 ; $70 = 0 means render complete
        beq     boss_intro_hp_bar_fill  ; done → skip to HP fill
boss_intro_bank_switch:  sta     prg_bank ; switch to stage bank
        jsr     select_PRG_banks        ; switch PRG banks
        lda     #$1A                    ; metatile column $1A (boss arena column)
        jsr     metatile_column_ptr_by_id ; load metatile column pointer
        lda     #$04                    ; nametable select = $04
        sta     $10                     ; set nametable select value
        jsr     fill_nametable_progressive ; draw one column per frame
        ldx     #$00                    ; restore X = player slot
        rts                             ; return to caller

; --- HP bar fill sequence ---

boss_intro_hp_bar_fill:  lda     boss_hp_display ; boss_hp_display = boss HP bar value
        cmp     #HEALTH_FULL            ; full (28 units)
        beq     boss_intro_return_normal ; full → end boss intro
        lda     $95                     ; $95 = frame counter, tick every 4th frame
        and     #$03                    ; check every 4th frame
        bne     boss_intro_return       ; not tick frame → wait
        inc     boss_hp_display         ; increment HP bar
        lda     boss_hp_display         ; re-read for sound check
        cmp     #$81                    ; skip sound on first tick ($81)
        beq     boss_intro_return       ; skip sound on first tick
        lda     #$1C                    ; sound $1C = HP fill tick
        jmp     submit_sound_ID         ; play HP fill tick sound

; --- boss intro complete: return to normal state ---

boss_intro_return_normal:  lda     #$00 ; $30 = state $00 (normal)
        sta     player_state            ; set state to on_ground
boss_intro_return:  rts                 ; return to caller

; ===========================================================================
; player state $0A: Top Spin recoil bounce [confirmed]
; ===========================================================================
; Applies $99 and horizontal movement in facing direction for ent_timer
; frames. When timer expires or player lands on ground, returns to state $00.
; Facing direction is preserved across move_collide calls.
; ---------------------------------------------------------------------------
player_top_spin:                        ; $A000 bank target = 0

        ldy     #$00                    ; apply $99 to player
        jsr     move_vertical_gravity   ; column counter = 0
        bcs     spark_freeze_clear_timer ; landed → end Top Spin recoil
        lda     ent_flags               ; save facing (move_collide may change it)
        pha                             ; save flags on stack
        and     #ENT_FLAG_HFLIP         ; bit 6: 0=right, 1=left
        bne     spark_freeze_move_left  ; H-flip set → move left
        ldy     #$00                    ; facing right → move right
        jsr     move_right_collide      ; move right with collision
        jmp     spark_freeze_restore_facing ; restore facing direction

spark_freeze_move_left:  ldy     #$01   ; facing left → move left
        jsr     move_left_collide       ; move left with collision
spark_freeze_restore_facing:  pla       ; restore original facing
        sta     ent_flags               ; restore facing flags
        dec     ent_timer               ; decrement Top Spin recoil timer
        bne     spark_freeze_return     ; not expired → continue
spark_freeze_clear_timer:  lda     #$00 ; timer expired or landed:
        sta     ent_timer               ; clear timer
        sta     player_state            ; return to state $00 (ground)
spark_freeze_return:  rts               ; return to caller

; ===========================================================================
; player state $0B: Hard Knuckle fire freeze [confirmed]
; ===========================================================================
; Freezes player for ent_var1 frames after firing Hard Knuckle. When timer
; expires, restores pre-freeze state from ent_timer and matching OAM from
; $D297 table, clears shoot timer $32.
; ---------------------------------------------------------------------------
player_weapon_recoil:

        dec     ent_var1                ; decrement freeze timer
        bne     gemini_dup_return       ; not zero → stay frozen
        ldy     ent_timer               ; ent_timer = saved pre-freeze state
        sty     player_state            ; player_state = return to that state
        lda     init_hard_knuckle_offset_table,y ; look up OAM ID for that state
        jsr     reset_sprite_anim       ; set player animation
        lda     #$00                    ; clear shoot timer
        sta     walk_flag               ; clear walk/shoot flag
gemini_dup_return:  rts                 ; return to caller

; ===========================================================================
; player state $0C: boss defeated cutscene [confirmed]
; ===========================================================================
; Multi-phase victory sequence after defeating a boss:
;   Phase 0 (ent_timer=0): initial setup, wait for landing on Y=$68
;   Phases 1-3: spawn explosion effects in entity slots 16-31, one batch
;     per phase, wait for all explosions to finish before next batch
;   Phase 4: play victory jingle, award weapon, transition to state $0D
; Also handles: walking to screen center (X=$80), victory music,
;   palette flash for Wily stages ($22 >= $10), Doc Robot ($08-$0B) exits.
; ---------------------------------------------------------------------------
player_victory:                         ; facing right → move right

        lda     ent_timer               ; get victory phase counter
        and     #$0F                    ; mask to low nibble
        beq     victory_walk_left       ; phase 0 → skip to $99/walk

; --- phases 1-4: wait for landing, then process explosions ---
        lda     ent_yvel                ; y_speed: negative = still rising from jump
        bpl     victory_walk_left       ; positive/zero = falling/grounded → continue
        lda     #$68                    ; clamp Y to landing position $68
        cmp     ent_y_px                ; compare to landing Y ($68)
        beq     victory_explosion_check ; at $68 → proceed
        bcs     victory_walk_left       ; above $68 → still falling, wait
        sta     ent_y_px                ; below $68 → snap to $68

; --- check if all explosion entities (slots $10-$1F) are inactive ---
victory_explosion_check:  ldy     #$0F  ; check slots $10-$1F (indices relative)
victory_entity_type_check:  lda     $0310,y ; $0310+Y = entity type for slots $10-$1F
        bmi     victory_phase_check     ; bit 7 set = still active → wait
        dey                             ; next slot down
        bpl     victory_entity_type_check ; loop through all 16 slots

; --- all explosions finished: advance phase ---
        lda     ent_timer               ; phase 4 = all explosion batches done
        cmp     #$04                    ; phase 4 = final batch done
        beq     victory_phase_continue  ; → award weapon
        jsr     explosion_init_table    ; spawn next batch of victory explosions
        inc     ent_timer               ; advance phase
victory_phase_check:  rts               ; return; wait for explosions

; --- phase 4 complete: award weapon and transition ---

victory_phase_continue:  lda     #SFX_WEAPON_GET ; weapon acquired jingle
        jsr     submit_sound_ID         ; play weapon get sound
        ldy     stage_id                ; load stage index
        lda     victory_weapon_id_table,y ; $DD04 table = weapon ID per stage
        sta     weapon_cursor           ; set weapon cursor to awarded weapon
        lda     warp_position_table,y   ; ammo slot offset per stage
        sta     $B4                     ; $B4 = ammo slot index
        clc                             ; Y = ammo slot + weapon ID = absolute ammo address
        adc     weapon_cursor           ; add weapon ID to offset
        tay                             ; Y = absolute ammo index
        lda     #$80                    ; set ammo to $80 (empty, will be filled by HUD)
        sta     player_hp,y             ; write to weapon ammo array
        lda     #$02                    ; bank $02 = HUD/menu update code
        sta     prg_bank                ; set bank for HUD update
        jsr     select_PRG_banks        ; switch PRG banks
        jsr     banked_A000             ; update HUD for new weapon
        lda     #PSTATE_TELEPORT        ; $30 = state $0D (teleport away)
        sta     player_state            ; set state $0D (teleport away)
        lda     #$80                    ; player type = $80 (active)
        sta     ent_status              ; set player entity active
        rts                             ; return to caller

; --- victory phase 0: $99 + palette flash + walk to center ---

victory_walk_left:  lda     stage_id    ; Wily stages (stage_id >= $10): palette flash effect
        cmp     #STAGE_WILY5            ; Wily5+ stages have palette flash
        bcc     victory_ground_check    ; not Wily → skip flash
        lda     $95                     ; flash every 8 frames
        and     #$07                    ; check every 8th frame
        bne     victory_ground_check    ; not flash frame → skip
        lda     ent_var1                ; ent_var1 = music countdown timer
        beq     victory_walk_right      ; if 0 → toggle
        lda     $0610                   ; SP0 color 0: if $0F → stop flashing
        cmp     #$0F                    ; (palette restored to normal)
        beq     victory_ground_check    ; palette normal → skip flash
victory_walk_right:  lda     $0610      ; toggle SP0 palette entry: XOR with $2F
        eor     #$2F                    ; (flashes between dark and light)
        sta     $0610                   ; store toggled palette value
        lda     #$FF                    ; trigger palette update
        sta     palette_dirty           ; request palette update

; --- apply $99 ---
victory_ground_check:  ldy     #$00     ; apply $99 to player
        jsr     move_vertical_gravity   ; apply gravity
        bcc     victory_jump_init       ; airborne → skip standing check
        lda     ent_var1                ; music timer active?
        beq     victory_jump_init       ; no → skip
        lda     #$01                    ; set OAM to standing ($01) if not already
        cmp     ent_anim_id             ; already standing OAM?
        beq     victory_jump_init       ; already standing → skip
        jsr     reset_sprite_anim       ; set standing animation
        sec                             ; set carry = grounded flag
victory_jump_init:  rol     $0F         ; $0F bit 0 = grounded this frame

; --- wait for boss explosion entities (slots $10-$1F) to finish ---
        ldy     #$0F                    ; check slots $10-$1F
victory_super_jump_start:  lda     stage_id ; Wily5 (stage_id=$11): skip checking slot $10
        cmp     #STAGE_WILY6            ; Wily6: skip slot $10
        bne     victory_check_explosions_loop ; not Wily6 → check all slots
        cpy     #$00                    ; at Y=0 (slot $10) → skip to next phase
        beq     victory_all_done_check  ; slot $10 → skip check
victory_check_explosions_loop:  lda     $0310,y ; check if slot still active
        bmi     victory_phase_check     ; active → wait (return)
        dey                             ; next slot
        bpl     victory_super_jump_start ; loop through all slots

; --- all explosions done: determine stage-specific exit behavior ---
victory_all_done_check:  lda     stage_id ; Doc Robot stages ($08-$0B)?
        cmp     #STAGE_DOC_NEEDLE       ; compare to first Doc stage
        bcc     victory_weapon_award    ; stages $00-$07 → robot master exit
        cmp     #STAGE_WILY1            ; compare to first Wily stage
        bcs     victory_weapon_award    ; stages $0C+ → fortress exit
        lda     camera_screen           ; camera screen < $18? → skip music
        cmp     #$18                    ; (Doc Robot in early part of stage)
        bcc     victory_return          ; early stage → skip music

; --- play victory music ---
victory_weapon_award:  lda     stage_id ; Wily5 ($11) → music $37 (special victory)
        cmp     #STAGE_WILY6            ; Wily6 → special victory music
        bne     victory_play_music      ; not Wily6 → normal music
        lda     #MUSIC_WILY_VICTORY     ; already playing?
        cmp     $D9                     ; already playing this music?
        beq     victory_exit_type       ; yes → skip
        lda     #$00                    ; reset nametable select (scroll to 0)
        sta     camera_x_hi             ; reset camera X high byte
        lda     #MUSIC_WILY_VICTORY     ; Wily victory music ID
        bne     victory_music_timer     ; always taken → start music
victory_play_music:  lda     #MUSIC_VICTORY ; normal boss victory music
        cmp     $D9                     ; already playing?
        beq     victory_exit_type       ; yes → skip
victory_music_timer:  jsr     submit_sound_ID_D9 ; start music
        lda     #$FF                    ; ent_var1 = $FF (255 frame countdown timer)
        sta     ent_var1                ; 255 frame music countdown

; --- countdown music timer, then transition ---
victory_exit_type:  lda     ent_var1    ; timer done?
        beq     victory_return          ; yes → proceed to exit
        dec     ent_var1                ; decrement music timer
        bne     victory_done_end        ; not zero → keep counting
        lda     stage_id                ; Wily stages (stage_id >= $10): reset nametable
        cmp     #STAGE_WILY5            ; check for Wily5+ stage
        bcc     victory_return          ; not Wily → skip scroll reset
        lda     #$00                    ; $FD = 0 (reset scroll)
        sta     camera_x_hi             ; reset camera X high byte

; --- determine exit type based on stage ---
victory_return:  lda     stage_id       ; stages $00-$07 (robot master): walk to center
        cmp     #STAGE_DOC_NEEDLE       ; stages $08+ → non-robot-master exit
        bcc     victory_exit_robot_master ; → walk to X=$80
        jmp     teleport_start_phase_check ; stages $08+ → alternate exit

; --- walk to screen center (X=$80) for robot master stages ---

victory_exit_robot_master:  sty     temp_00 ; save Y (slot check result)
        lda     ent_x_px                ; get player X position
        cmp     #$80                    ; compare X to screen center
        beq     victory_exit_wily       ; at center → done walking
        bcs     victory_exit_fortress   ; X > $80 → walk left

; --- walk right toward center ---
        ldy     #$00                    ; move right with collision
        jsr     move_right_collide      ; move right with collision
        rol     temp_00                 ; save collision result
        lda     #$80                    ; clamp: don't overshoot $80
        cmp     ent_x_px                ; check if past center
        bcs     victory_exit_wily       ; X <= $80 → ok
        sta     ent_x_px                ; clamp X to $80
        bcc     victory_exit_wily       ; always taken → check anim

; --- walk left toward center ---
victory_exit_fortress:  ldy     #$01    ; move left with collision
        jsr     move_left_collide       ; move left with collision
        rol     temp_00                 ; save collision result
        lda     #$80                    ; clamp: don't undershoot $80
        cmp     ent_x_px                ; compare X to center ($80)
        bcc     victory_exit_wily       ; X >= $80 → ok
        sta     ent_x_px                ; clamp X to $80

; --- set walking/standing animation based on ground state ---
victory_exit_wily:  ldy     #$00        ; $0F bit 0 = grounded (from ROL earlier)
        lsr     $0F                     ; carry = grounded
        bcs     victory_restore_palette ; grounded → Y=0 (walk anim)
        iny                             ; airborne → Y=1 (jump anim)
victory_restore_palette:  lda     victory_walk_oam_table,y ; $DC7F = walk/jump OAM lookup table
        cmp     ent_anim_id             ; already correct OAM?
        beq     victory_next_stage_setup ; yes → skip anim reset
        jsr     reset_sprite_anim       ; set new animation
victory_next_stage_setup:  cpy     #$01 ; airborne (Y=1)? → done for this frame
        beq     explosion_return        ; airborne → done this frame
        lsr     temp_00                 ; check collision flag from walk
        bcc     explosion_return        ; no collision → done

; --- grounded at center: do victory jump ---
        lda     ent_x_px                ; at center X=$80?
        cmp     #$80                    ; at screen center?
        beq     victory_exit_completion ; yes → super jump!
        lda     #$E5                    ; not at center: normal jump velocity
        sta     ent_yvel_sub            ; y_speed = $04.E5
        lda     #$04                    ; y_speed = $04 (normal jump)
        sta     ent_yvel                ; set Y velocity for jump
victory_done_end:  rts                  ; return to caller

; --- victory super jump at screen center ---

victory_exit_completion:  lda     #$00  ; y_speed = $08.00 (super jump!)
        sta     ent_yvel_sub            ; y_speed_sub = $00
        lda     #$08                    ; super jump velocity = $08
        sta     ent_yvel                ; y_speed = $08 (double height jump)
        inc     ent_timer               ; advance to next phase
        rts                             ; return to caller

; ---------------------------------------------------------------------------
; spawn_victory_explosions — create one batch of celebratory explosions
; ---------------------------------------------------------------------------
; Fills entity slots $10-$1F with explosion effects. Each batch uses a
; different velocity table offset from $DD00 (indexed by ent_timer = batch #).
; Entity type $5B = explosion effect, routine $11 = velocity mover.
; Positions from $DCE1 (X) and $DCD1 (Y) tables, velocities from $DC71+.
; ---------------------------------------------------------------------------

explosion_init_table:  ldy     ent_timer ; velocity table offset from batch number
        ldx     warp_boss_init_table,y  ; velocity table start index for this batch
        ldy     #$1F                    ; Y = slot $1F (fill backwards to $10)
explosion_type_set:  lda     #$5B       ; entity type $5B = victory explosion
        jsr     init_child_entity       ; spawn explosion entity in slot Y
        lda     #$00                    ; no hitbox (visual only)
        sta     ent_hitbox,y            ; no collision for visual effect
        lda     #$11                    ; routine $11 = constant velocity mover
        sta     ent_routine,y           ; set to constant velocity mover routine
        lda     victory_y_positions,y   ; x_pixel from position table
        sta     ent_x_px,y              ; set explosion X position
        lda     ent_x_scr               ; copy player screen to explosion
        sta     ent_x_scr,y             ; set explosion X screen
        lda     victory_burst_y_speed,y ; y_pixel from position table
        sta     ent_y_px,y              ; set explosion Y position
        lda     rush_jet_energy_set,x   ; x_speed_sub from velocity table
        sta     ent_xvel_sub,y          ; set explosion X sub-velocity
        lda     victory_burst_x_speed_sub,x ; x_speed from velocity table
        sta     ent_xvel,y              ; set explosion X velocity
        lda     victory_burst_x_speed,x ; y_speed_sub from velocity table
        sta     ent_yvel_sub,y          ; set explosion Y velocity sub
        lda     victory_burst_y_speed_sub,x ; y_speed from velocity table
        sta     ent_yvel,y              ; set explosion Y velocity
        dex                             ; next slot + velocity entry
        dey                             ; next entity slot
        cpy     #$0F                    ; loop until Y = $0F (slot $10 done)
        bne     explosion_type_set      ; loop for all explosion slots
        lda     #SFX_EXPLOSION          ; play explosion sound effect
        jsr     submit_sound_ID         ; queue sound effect
        ldx     #$00                    ; restore X = player slot
explosion_return:  rts

; --- alternate exit paths for non-RM stages ---

teleport_start_phase_check:  lda     stage_id ; check if Wily fortress stage
        cmp     #STAGE_WILY5            ; stage >= Wily 5?
        bcs     teleport_restore_palette ; → Wily exit
        cmp     #STAGE_WILY1            ; Wily1-4 ($0C-$0F)?
        bcs     teleport_set_state      ; → teleport exit
        lda     camera_screen           ; Doc Robot: check camera position
        cmp     #$18                    ; (deep in stage = Doc Robot area)
        bcs     teleport_set_state      ; → teleport exit

; --- normal exit: return to stage select ---
        lda     #$00                    ; $30 = state $00 (triggers stage clear)
        sta     player_state            ; return to normal state
        ldy     stage_id                ; load stage index for music
        lda     frame_loop_stage_music_table,y ; stage music ID from table
        jsr     submit_sound_ID_D9      ; play stage music (avoids restart)
        rts                             ; return to caller

; --- fortress exit: teleport away ---

teleport_set_state:  lda     #$81       ; player type = $81 (teleport state)
        sta     ent_status              ; mark player as teleporting
        lda     #$00                    ; clear phase counter
        sta     ent_timer               ; reset phase counter
        lda     #PSTATE_TELEPORT        ; set state $0D (teleport)
        sta     player_state            ; set teleport player state
        rts                             ; return to caller

; --- Wily exit: restore palette, set up next Wily stage ---

teleport_restore_palette:  lda     #$0F ; restore SP0 palette to black ($0F)
        sta     $0610                   ; (undo victory flash)
        sta     palette_dirty           ; trigger palette update
        lda     #$00                    ; clear various state:
        sta     game_mode               ; clear game mode
        sta     $6A                     ; $6A = cutscene flag
        sta     $6B                     ; $6B = cutscene flag
        sta     camera_x_hi             ; clear camera high byte
        lda     stage_id                ; compute next Wily stage state
        clc                             ; stages $10→$14, $11→$15, etc.
        adc     #$04                    ; (maps to player states $14/$15 = auto_walk)
        sta     player_state            ; set auto-walk state for Wily transition
        lda     #$80                    ; player type = $80
        sta     ent_status              ; set player entity status
        lda     #$00                    ; clear all timer/counter fields
        sta     ent_timer               ; clear timer
        sta     ent_var1                ; clear var1
        sta     ent_var2                ; clear var2
        sta     ent_var3                ; clear var3
        rts                             ; return to caller

; ===========================================================================
; player state $0D: teleport away [confirmed]
; ===========================================================================
; Two-phase teleport departure after boss defeat:
;   Phase 1 (ent_status & $0F == 0): fall with $99, land, wait $3C (60) frames
;   Phase 2 (ent_status & $0F != 0): start teleport beam, accelerate upward,
;     track boss defeat when player leaves screen (y_screen != 0)
; ---------------------------------------------------------------------------
player_teleport:

        lda     ent_status              ; check entity status low nibble
        and     #$0F                    ; 0 = falling/landing, !0 = teleporting
        bne     teleport_phase_1_wait   ; nonzero → teleport beam phase

; --- phase 1: fall and land ---
        ldy     #$00                    ; apply $99
        jsr     move_vertical_gravity   ; apply gravity, carry = landed
        bcs     teleport_phase_advance  ; landed → start wait timer
        lda     ent_anim_state          ; anim state $04 = landing frame
        cmp     #$04                    ; check for landing anim state
        bne     teleport_return         ; not at landing frame → wait
        lda     #$07                    ; set OAM $07 = landing pose
        jmp     reset_sprite_anim       ; apply landing animation

; --- landed: stop movement, start wait timer ---

teleport_phase_advance:  inc     ent_status ; advance phase ($80 → $81)
        sta     ent_yvel_sub            ; clear vertical speed (A=0 from BCS)
        sta     ent_yvel                ; clear Y velocity
        lda     #$3C                    ; ent_timer = $3C (60 frame wait before beam)
        sta     ent_timer               ; wait 60 frames before teleport beam
        lda     #$01                    ; OAM $01 = standing
        jsr     reset_sprite_anim       ; set standing pose
teleport_return:  rts

; --- phase 2: teleport beam ---

teleport_phase_1_wait:  lda     ent_timer ; wait timer still counting?
        beq     teleport_beam_anim_set  ; done → start beam
        dec     ent_timer               ; decrement timer
        rts                             ; wait for next frame

; --- start teleport beam animation ---

teleport_beam_anim_set:  lda     #$13   ; OAM $13 = teleport beam
        cmp     ent_anim_id             ; already set?
        beq     teleport_beam_check_formed ; yes → skip init
        jsr     reset_sprite_anim       ; set beam animation
        lda     #SFX_TELEPORT           ; play teleport beam sound
        jsr     submit_sound_ID         ; queue teleport sound
        lda     #$04                    ; ent_anim_state = $04 (beam startup frame)
        sta     ent_anim_state          ; set beam startup state

; --- beam active: accelerate upward ---
teleport_beam_check_formed:  lda     ent_anim_state ; anim state $02 = beam fully formed
        cmp     #$02                    ; $02 = beam fully formed
        bne     teleport_timer_done     ; not ready → wait
        lda     #$00                    ; clear animation field
        sta     ent_anim_frame          ; reset animation frame
        lda     ent_yvel_sub            ; y_speed_sub += $99 (acceleration)
        clc                             ; $99 = teleport acceleration constant
        adc     gravity                 ; add gravity to Y sub-velocity
        sta     ent_yvel_sub            ; store updated sub-velocity
        lda     ent_yvel                ; add carry to Y velocity whole
        adc     #$00                    ; propagate carry to whole byte
        sta     ent_yvel                ; store updated velocity
        jsr     move_sprite_up          ; move player upward

; --- boss defeat tracking ---
; called during teleport-away after boss explodes
; sets bit in $61 for defeated stage, awards special weapons
        lda     ent_y_scr               ; if boss not defeated,
        beq     teleport_timer_done     ; skip defeat tracking
        ldy     stage_id                ; load current stage
        cpy     #$0C                    ; if stage >= $0C (Wily fortress),
        bcs     wily_stage_clear        ; handle separately
        lda     bosses_beaten           ; load boss-defeated bitmask
        ora     entity_damage_table,y   ; set bit for this stage's boss
        sta     bosses_beaten           ; store updated bitmask
        inc     $59                     ; advance stage progression
        lda     stage_id                ; check if Needle Man stage
        cmp     #$00                    ; awards Rush Jet
        beq     rush_jet_energy_full_table ; Needle Man → award Rush Jet
        cmp     #STAGE_SHADOW           ; check if Shadow Man stage
        bne     teleport_timer_done     ; other stages → no Rush award
        lda     #HEALTH_FULL            ; fill Rush Marine energy
        sta     $AB                     ; store Rush Marine ammo
        rts                             ; return to caller

rush_jet_energy_full_table:  lda     #HEALTH_FULL ; fill Rush Jet energy
rush_jet_energy_set:  sta     $AD
teleport_timer_done:  rts

wily_stage_clear:  lda     #$FF         ; mark fortress stage cleared
        sta     $74                     ; store stage cleared flag
        inc     $75                     ; advance fortress progression
        lda     #HEALTH_FULL            ; refill player health
        sta     player_hp               ; refill HP
        rts                             ; return to caller

; victory animation lookup tables
; $DC7F: walk/jump OAM IDs (Y=0 → walk $04, Y=1 → jump $07)
victory_walk_oam_table:  .byte   $04,$07,$00,$C2,$00,$C2,$00,$3E
        .byte   $00,$3E
victory_burst_x_speed_sub:  .byte   $00,$E1,$00,$E1,$00,$1F,$00,$1F
        .byte   $00,$F1,$80,$F1,$00,$0F,$80,$0F
victory_x_speed:
        .byte   $00,$FB,$FA,$FB,$00,$04,$06,$04
victory_burst_x_speed:  .byte   $00,$FD,$FD,$FD,$00,$02,$03,$02
        .byte   $00,$FE,$FE,$FE,$00,$01,$01,$01
victory_y_speed_sub:
        .byte   $00,$3E,$00,$C2,$00,$C2,$00,$3E
victory_burst_y_speed_sub:  .byte   $00,$1F,$00,$E1,$00,$E1,$00,$1F
        .byte   $80,$0F,$00,$F1,$80,$F1,$00,$0F
victory_y_speed:
        .byte   $06,$04,$00,$FB,$FA,$FB,$00,$04
victory_burst_y_speed:  .byte   $03,$02,$00,$FD,$FD,$FD,$00,$02
        .byte   $01,$01,$00,$FE,$FE,$FE,$00,$01

; explosion starting Y positions (slots $10-$1F)
victory_y_positions:  .byte   $00,$23,$78,$C0,$F0,$CC,$78,$23
        .byte   $00,$23,$78,$C0,$F0,$CC,$78,$23

; explosion starting X positions (slots $10-$1F)
victory_x_positions:
        .byte   $80,$D4,$F8,$D4,$80,$2B,$08,$2B
        .byte   $80,$D4,$F8,$D4,$80,$2B,$08
warp_boss_init_table:  .byte   $2B,$1F,$1F,$27

; weapon ID per stage table (indexed by $22)
victory_weapon_id_table:  .byte   $02,$04,$01,$03,$05,$00,$02,$04 ; Ndl→$04(Mag) Mag→$01(Gem) Gem→$03(Hrd) Hrd→$05(Top)
warp_position_table:  .byte   $00,$00,$00,$00,$00,$06,$06,$06
player_screen_scroll:  ldy     #$00  ; state $10: vertical scroll transition
        jsr     move_vertical_gravity   ; fill Rush Jet energy ($AD)
        bcc     warp_boss_refight_init  ; airborne → skip standing check
        lda     #$01                    ; set standing OAM when grounded
        cmp     ent_anim_id             ; already standing anim?
        beq     warp_boss_refight_init  ; already set → skip
        jsr     reset_sprite_anim       ; set standing animation

; --- render shutter nametable columns ---
warp_boss_refight_init:  ldy     #$26   ; set viewport ($52 = $26)
        cpy     $52                     ; viewport already $26?
        beq     warp_arena_nt_render    ; already set → check progress
        sty     $52                     ; first time: init progressive render
        ldy     #$00                    ; clear init values
        sty     MMC3_MIRRORING          ; set V-mirroring
        sty     $70                     ; progress counter = 0
        sty     $28                     ; column counter = 0
        beq     warp_arena_finalize     ; always branch (Y=0)
warp_arena_nt_render:  ldy     $70      ; render complete?
        beq     warp_destination_check  ; yes → wait for entities
warp_arena_finalize:  lda     #$11      ; switch to bank $11 (shutter graphics)
        sta     prg_bank                ; select bank $11
        jsr     select_PRG_banks        ; apply bank switch
        lda     #$04                    ; metatile column $04 (shutter/door column)
        jsr     metatile_column_ptr_by_id ; set metatile column pointer
        lda     #$04                    ; nametable select = $04
        sta     $10                     ; store nametable select
        jsr     fill_nametable_progressive ; draw one column per frame
        ldx     #$00                    ; restore X = player slot
        rts                             ; return to caller

; --- wait for shutter entities (slots $08-$17) to clear ---

warp_destination_check:  ldy     #$0F   ; check 16 entity slots
warp_negative_screen:  lda     $0308,y  ; slot still active?
        bmi     proto_man_encounter_init ; yes → wait
        dey                             ; next slot index
        bpl     warp_negative_screen    ; next slot

; --- all entities done: begin Y scroll reveal ---
        lda     #$0B                    ; set game mode $0B (split-screen mode)
        cmp     game_mode               ; already in split-screen mode?
        beq     warp_boss_slot_setup    ; yes → skip init
        sta     game_mode               ; set game mode to split-screen
        lda     camera_x_hi             ; set nametable select bit
        ora     #$01                    ; set nametable select bit
        sta     camera_x_hi             ; store nametable toggle
        lda     #$50                    ; $FA = $50 (starting Y scroll offset)
        sta     scroll_y                ; set starting Y scroll
        lda     #$52                    ; $5E = $52 (IRQ scanline count)
        sta     $5E                     ; set IRQ scanline split

; --- scroll Y down by 3 per frame ---
warp_boss_slot_setup:  lda     scroll_y ; scroll_y -= 3
        sec                             ; prepare subtraction
        sbc     #$03                    ; subtract 3 pixels
        sta     scroll_y                ; store updated scroll Y
        bcs     warp_position_load      ; no underflow → continue
        lda     #$00                    ; underflow: clamp to 0
        sta     scroll_y                ; scroll_y = 0 (scroll complete)
        sta     player_state            ; player_state = state $00 (return to normal)

; --- adjust shutter entity Y positions during scroll ---
warp_position_load:  ldy     #$03       ; 4 shutter entities (slots $1C-$1F)
warp_arena_render_done:  lda     shutter_base_y_table,y ; base Y position from table
        sec                             ; subtract current scroll offset
        sbc     scroll_y                ; adjust for current scroll position
        bcc     warp_teleport_complete  ; off screen (negative) → skip
        sta     $03DC,y                 ; y_pixel for slot $1C+Y
        lda     $059C,y                 ; clear bit 2 of sprite flags
        and     #$FB                    ; (make entity visible)
        sta     $059C,y                 ; store updated flags
warp_teleport_complete:  dey            ; next entity
        bpl     warp_arena_render_done  ; loop all 4 shutter slots
        lda     player_state            ; scroll done (player_state = 0)?
        beq     proto_man_encounter_init ; yes → return
        lda     $059E                   ; set bit 2 on slot $1E (keep one shutter visible
        ora     #$04                    ; during transition for visual continuity)
        sta     $059E                   ; store updated slot $1E flags
proto_man_encounter_init:  .byte   $60  ; rts encoded as data byte

; shutter entity base Y positions (for slots $1C-$1F during scroll)
shutter_base_y_table:  .byte   $48
        .byte   $3F
        .byte   $3F
        .byte   $3F

; ===========================================================================
; player state $11: warp tube initialization (boss refight arena) [confirmed]
; ===========================================================================
; Sets up the Wily4 boss refight arena. $6C = boss/warp index (0-11).
; Waits for teleport-in animation to finish (ent_anim_state = $04), then:
;   1. If screen number from $DE5E is negative ($FF): Wily stage clear
;   2. Else: fade in, clear entities, load boss position/CHR from tables
;      ($DE5E-$DEAE indexed by $6C), render arena nametable, start warp_anim
;
; Boss refight tables (all indexed by $6C):
;   $DE5E: screen number for boss arena
;   $DE72: boss X pixel position
;   $DE86: boss Y pixel position
;   $DE9A: boss animation/state value
;   $DEAE: CHR bank param (passed to bank $01 set_room_chr_and_palette)
; ---------------------------------------------------------------------------
player_warp_init:
        lda     ent_anim_state          ; check teleport-in animation state
        cmp     #$04                    ; $04 = animation complete
        bne     proto_man_encounter_init ; not done → wait
        ldy     warp_dest               ; Y = warp destination index
        lda     warp_screen_table,y     ; check screen number
        bpl     proto_man_phase_2_check ; positive → valid boss arena

; --- negative screen = Wily stage clear (no more bosses) ---
        sta     $74                     ; mark fortress stage cleared ($FF)
        inc     $75                     ; advance fortress progression
        lda     #HEALTH_FULL            ; refill player health
        sta     player_hp               ; restore full HP
        rts                             ; return to caller

; --- set up boss refight arena ---

proto_man_phase_2_check:  lda     #$00  ; clear NMI skip flag
        sta     nmi_skip                ; allow rendering
        jsr     fade_palette_in         ; fade screen in
        lda     #$04                    ; $97 = $04 (OAM overlay mode for boss arena)
        sta     oam_ptr                 ; set OAM overlay mode
        jsr     prepare_oam_buffer      ; refresh OAM
        jsr     task_yield              ; yield to NMI (update screen)
        jsr     clear_entity_table      ; clear all entity slots
        lda     #$01                    ; switch to bank $01 (CHR/palette data)
        sta     prg_bank                ; select bank $01
        jsr     select_PRG_banks        ; switch PRG bank
        ldy     warp_dest               ; load warp destination index
        bne     proto_man_walk_init     ; nonzero → skip bitmask clear
        sty     $6E                     ; $6E = 0

; --- load boss position from tables ---
proto_man_walk_init:  lda     warp_screen_table,y ; camera screen = boss screen number
        sta     camera_screen           ; set camera to boss screen
        sta     ent_x_scr               ; player X screen = boss screen
        sta     $2B                     ; room index = boss screen
        sta     $29                     ; column render position
        lda     #$20                    ; $2A = $20 (scroll flags: h-scroll enabled)
        sta     $2A                     ; store scroll flags
        lda     weapon_cost_table,y     ; player X = boss X position
        sta     ent_x_px                ; set player X pixel
        lda     weapon_use_rate_table,y ; player Y = boss Y position
        sta     ent_y_px                ; set player Y pixel
        lda     weapon_ammo_drain_table,y ; boss anim/state value
        sta     $9E                     ; (used by boss spawn code)
        sta     $9F                     ; store duplicate state value
        lda     warp_chr_param_table,y  ; CHR bank param → bank $01 $A000
        jsr     banked_A000             ; (set_room_chr_and_palette in bank $01)
        jsr     update_CHR_banks        ; apply CHR bank changes

; --- render arena nametable (33 columns, right-to-left) ---
        lda     #$01                    ; $31 = 1 (direction flag)
        sta     player_facing           ; face right
        sta     $23                     ; $23 = 1 (scroll state)
        sta     $2E                     ; $2E = 1 (scroll direction)
        dec     $29                     ; $29 -= 1 (start one screen left)
        lda     #$1F                    ; $24 = $1F (starting row)
        sta     $24                     ; set starting column
        lda     #$21                    ; 33 columns to render
proto_man_walk_x_pos:  pha              ; save column counter
        lda     #$01                    ; $10 = nametable select (right)
        sta     $10                     ; store nametable select
        jsr     do_render_column        ; render one nametable column
        jsr     task_yield              ; yield to NMI
        pla                             ; decrement column counter
        sec                             ; decrement column counter
        sbc     #$01                    ; subtract 1
        bne     proto_man_walk_x_pos    ; more columns → loop

; --- arena rendered: finalize ---
        sta     $2C                     ; clear scroll progress fields
        sta     $2D                     ; clear scroll progress
        sta     boss_active             ; clear boss active flag
        lda     camera_screen           ; screen $08 = special arena
        cmp     #$08                    ; play music $0A (boss intro)
        bne     proto_man_rise_init     ; not screen $08 → skip music
        lda     #MUSIC_WILY_2           ; play Wily 3-4 music
        jsr     submit_sound_ID_D9      ; start music track
proto_man_rise_init:  jsr     task_yield ; yield for one more frame
        jsr     fade_palette_out        ; fade screen out (prepare for warp-in)
        ldx     #$00                    ; X = player slot 0
        lda     #$13                    ; set teleport beam OAM
        jsr     reset_sprite_anim       ; set player animation
        lda     #PSTATE_WARP_ANIM       ; set state $12 (warp animation)
        sta     player_state            ; enter warp animation state
        rts                             ; return to caller

; ===========================================================================
; player state $12: wait for warp animation, return to $00 [confirmed]
; ===========================================================================
; Simple state: waits for teleport-in animation to complete (ent_anim_state = $04),
; then sets $30 = $00 (return to normal gameplay state).
; ---------------------------------------------------------------------------
player_warp_anim:

        lda     ent_anim_state          ; check animation state
        cmp     #$04                    ; $04 = teleport-in finished
        bne     proto_man_rise_check    ; not done → wait
        lda     #$00                    ; $30 = state $00 (normal gameplay)
        sta     player_state            ; return to normal gameplay
proto_man_rise_check:  rts

; ---------------------------------------------------------------------------
; Boss refight tables — all indexed by $6C (boss/warp index 0-11)
; Used by player_warp_init ($DDAA) for Wily4 boss refight arena setup.
; ---------------------------------------------------------------------------
; $6C values: 0-1 = special warps, 2 = end marker, 3-10 = robot masters
;   3=Needle, 4=Magnet, 5=Gemini, 6=Hard, 7=Top, 8=Snake, 9=Spark, 10=Shadow
; Screen number where boss arena is located
warp_screen_table:  .byte   $07,$00,$FF,$0A,$0C,$0E,$10,$12
        .byte   $14,$16,$18,$00,$08,$08,$08,$08
        .byte   $08,$08,$08,$08
weapon_cost_table:  .byte   $30,$00,$00,$20,$20,$20,$20,$20
        .byte   $20,$20,$20,$00,$30,$30,$30,$70
        .byte   $90,$D0,$D0,$D0
weapon_use_rate_table:  .byte   $B0,$00,$00,$C0,$B0,$B0,$B0,$B0
        .byte   $B0,$A0,$B0,$00,$2C,$6C,$AC,$AC
        .byte   $AC,$2C,$6C,$AC
weapon_ammo_drain_table:  .byte   $10,$10,$00,$1B,$1C,$1D,$1E,$1F
        .byte   $20,$21,$22,$00,$11,$11,$11,$11
        .byte   $11,$11,$11,$11

; CHR bank param (indexes bank $01 $A200/$A030 for sprite tiles + palettes)
warp_chr_param_table:  .byte   $02,$02,$00,$25,$23,$27,$24,$2A
        .byte   $26,$28,$29,$00,$02,$02,$02,$02
        .byte   $02
entity_ai_routine_table:  .byte   $02,$02,$02

; boss_defeated_bitmask: indexed by stage ($22), sets bit in $61
; $00=Needle $01=Magnet $02=Gemini $03=Hard $04=Top $05=Snake $06=Spark $07=Shadow
entity_damage_table:  .byte   $01,$02,$04,$08,$10,$20,$40,$80

; Doc Robot stage bitmask (4 stages: Needle=$08, Gemini=$09, Spark=$0A, Shadow=$0B)
        .byte   $01
        .byte   $04
        rti

        .byte   $80
decrease_ammo:  lda     current_weapon
        cmp     #WPN_SNAKE              ; weapons $06+ are Rush items (odd IDs)
        bcc     decrease_ammo_tick      ; weapons $00-$05 always drain
        and     #$01                    ; odd = Rush weapon, skip drain
        bne     weapon_hurt_freeze_done ; Rush weapon → skip drain
decrease_ammo_tick:  ldy     current_weapon ; Y = current weapon index
        inc     $B5                     ; increment usage frame counter
        lda     $B5                     ; load current count
        cmp     weapon_framerate,y      ; compare to drain rate for this weapon
        bne     weapon_hurt_freeze_done ; not at threshold → return
        lda     #$00                    ; reset frame counter to 0
        sta     $B5                     ; store reset counter
        lda     player_hp,y             ; load weapon ammo
        and     #$1F                    ; mask to ammo value (low 5 bits)
        sec                             ; subtract ammo cost
        sbc     weapon_cost,y           ; subtract ammo cost
        bcs     entity_spawn_x_offset_table ; no underflow → store result
        lda     #$00                    ; clamp to zero if underflow
entity_spawn_x_offset_table:  ora     #$80 ; set bit 7 (weapon-owned flag)
        sta     player_hp,y             ; store updated ammo
        cmp     #$80                    ; check if ammo is now zero ($80)
        bne     weapon_hurt_freeze_done ; still has ammo → return

; ammo has been depleted, clean up rush
        cpy     #$0B                    ; Rush Jet? → deactivate
        beq     weapon_hurt_freeze_init ; yes → deactivate Rush
        cpy     #$09                    ; Rush Marine?
        bne     weapon_hurt_freeze_done ; neither → return
        lda     #$02                    ; set CHR reload flag
        sta     $EB                     ; set CHR reload flag
        jsr     update_CHR_banks        ; reload Mega Man CHR tiles
        lda     #$01                    ; restore standing OAM ($01)
        jsr     reset_sprite_anim       ; set standing animation
        lda     #$00                    ; state $00 = normal
        sta     player_state            ; return to normal ground state
weapon_hurt_freeze_init:  lda     #$00  ; A = 0 for sta below
        sta     ent_status+1            ; deactivate Rush entity slot 1
weapon_hurt_freeze_done:  rts

; how much ammo each weapon costs upon use
weapon_cost:  .byte   $00,$02,$01,$02,$02,$00,$01,$03
        .byte   $01,$01,$01,$01

; the rate or number of frames of use before decreasing
; most weapons are 1 shot == 1 frame
; jet & marine count while using them
weapon_framerate:  .byte   $00,$01,$04,$01,$01,$00,$02
        ora     ($01,x)
        asl     $1E02,x

; player state $13: Proto Man exit beam — Wily gate scene after all Doc Robots [confirmed]
; Triggered only by Proto Man routine $53 (bank18 scripted spawn, $60=$12).
; Player beams in with $99, lands, then rises off-screen.
; Two phases:
;   Phase 1 (OAM != $13): Fall with $99 until landing. On landing, switch
;   to teleport OAM $13, increment $05A0, clear Y speed.
;   Phase 2 (OAM == $13): Rise upward (move_sprite_up). Accelerate by $99
;   each frame. When Y screen wraps (ent_y_scr != 0), player has left — trigger
;   stage clear ($74=$80), mark all Doc Robots beaten ($60=$FF), refill ammo.
player_teleport_beam:
        lda     ent_anim_id             ; check current OAM ID
        cmp     #$13                    ; teleport beam OAM?
        beq     break_man_encounter_init ; yes → phase 2 (rising)

; --- phase 1: falling with $99 ---
        ldy     #$00                    ; move down with $99 (slot 0)
        jsr     move_vertical_gravity   ; carry set = landed
        bcc     break_man_spawn_explosions ; still airborne → return
        lda     #$13                    ; landed: set teleport beam OAM
        jsr     reset_sprite_anim       ; set teleport beam anim
        inc     ent_anim_state          ; signal landing (anim counts down)
        lda     #$00                    ; clear Y speed (stopped on ground)
        sta     ent_yvel_sub            ; clear Y sub-velocity
        sta     ent_yvel                ; clear Y velocity

; --- phase 2: rising off-screen ---
break_man_encounter_init:  lda     ent_anim_state ; nonzero = still in landing anim
        bne     break_man_spawn_explosions ; waiting for anim to finish → return
        sta     ent_anim_frame          ; reset anim frame counter
        jsr     move_sprite_up          ; move player upward by Y speed
        lda     ent_y_scr               ; check if off-screen (Y screen != 0)
        bne     break_man_walk_init     ; → trigger ending
        lda     ent_yvel_sub            ; accelerate Y speed by gravity
        clc                             ; (gradual speed increase each frame)
        adc     gravity                 ; add gravity sub-pixel
        sta     ent_yvel_sub            ; store updated sub-velocity
        lda     ent_yvel                ; load Y velocity whole
        adc     #$00                    ; add carry to whole velocity
        sta     ent_yvel                ; store updated velocity
        rts                             ; return to caller

; --- player has risen off-screen: trigger Wily gate ending ---

break_man_walk_init:  lda     #$00      ; reset player state
        sta     player_state            ; return to ground state
        lda     #$80                    ; $74 = $80 → trigger stage clear path
        sta     $74                     ; store stage clear flag
        lda     #$FF                    ; $60 = $FF → all Doc Robots beaten
        sta     stage_select_page       ; mark all Doc Robots beaten
        ldy     #$0B                    ; fill all 12 ammo slots to $9C (full)
        lda     #HEALTH_FULL            ; full ammo
break_man_check_defeated:  sta     player_hp,y ; fill ammo for each weapon slot
        dey                             ; next weapon slot
        bpl     break_man_check_defeated ; loop all 12 slots
break_man_spawn_explosions:  rts        ; return to caller

; ===========================================================================
; player_auto_walk — state $14: post-Wily cutscene walk (Break Man encounter)
; ===========================================================================
; Phase 0: Walk player to X=$50 with $99, set jump/walk animation.
;   On reaching X=$50: spawn Break Man entity in slot $1F, wait for timer.
; Phase 1: Wait for timer countdown, then walk right to X=$A0.
;   On reaching X=$A0: set jump velocity, spawn Break Man teleport OAM.
; Phase 2: After landing, check if Break Man defeated (ent_var1).
;   If defeated: transition to teleport state ($30=$0D, type=$81).
; Uses ent_status lower nibble as sub-phase (0→1→2).
; $0F bit 7 = "on ground" flag (rotated from carry after $99).
; ---------------------------------------------------------------------------
player_auto_walk:

        ldy     #$00                    ; Y=0 (no horizontal component)
        jsr     move_vertical_gravity   ; apply gravity + vertical collision
        php                             ; save carry (1=landed)
        ror     $0F                     ; rotate carry into $0F bit 7
        plp                             ; restore flags
        bcs     weapon_hurt_walk_oam    ; landed → set walk animation
        lda     #$07                    ; airborne: set jump OAM ($07)
        cmp     ent_anim_id             ; already jump OAM?
        beq     weapon_hurt_phase_check ; yes → check phase
        jsr     reset_sprite_anim       ; set jump animation
        jmp     weapon_hurt_phase_check ; skip to phase check

weapon_hurt_walk_oam:  lda     #$04     ; on ground: set walk OAM ($04)
        cmp     ent_anim_id             ; already walk OAM?
        beq     weapon_hurt_phase_check ; yes → check phase
        jsr     reset_sprite_anim       ; set walk animation
weapon_hurt_phase_check:  lda     ent_status ; check sub-phase from status low nibble
        and     #$0F                    ; phase 0 = walk to X=$50
        bne     aw_wait_timer           ; phase 1+ → wait/continue
        lda     ent_x_px                ; get player X position
        cmp     #$50                    ; reached target?
        beq     aw_wait_timer           ; yes → stop and wait
        bcs     weapon_hurt_walk_left   ; X > $50 → walk left
        inc     ent_x_px                ; X < $50 → walk right (inc X)
        jmp     weapon_hurt_walk_check_x ; check if arrived at target

weapon_hurt_walk_left:  dec     ent_x_px ; walk left (dec X)
weapon_hurt_walk_check_x:  lda     ent_x_px ; check if at X=$50
        cmp     #$50                    ; at target position?
        bne     aw_ret                  ; not yet → return
        lda     $031F                   ; slot $1F already active?
        bmi     aw_wait_timer           ; yes (bit 7 set) → just wait
        lda     #$80                    ; spawn Break Man in slot $1F
        sta     $031F                   ; set slot $1F active
        lda     #$90                    ; set flags: active + child
        sta     $059F                   ; store to ent_flags for slot $1F
        lda     #$6D                    ; Break Man sprite OAM
        sta     $05DF                   ; store to ent_anim_id for slot $1F
        lda     #$00                    ; clear slot $1F fields:
        sta     $05FF                   ; clear ent_anim_frame for slot $1F
        sta     $05BF                   ; clear ent_anim_state for slot $1F
        sta     $03FF                   ; Y screen = 0 (on-screen)
        sta     $03DF                   ; Y pixel = 0 (top of screen)
        sta     ent_var1                ; clear player var1 (phase flag)
        lda     camera_screen           ; Break Man X screen = camera screen
        sta     $039F                   ; set slot $1F X screen
        lda     #$C0                    ; Break Man X pixel = $C0
        sta     $037F                   ; set slot $1F X pixel
        lda     #$EE                    ; Break Man routine = $EE (AI state)
        sta     $033F                   ; store to ent_routine for slot $1F
auto_walk_spawn_done:  .byte   $A9      ; lda #$5A (hand-assembled)
weapon_hurt_timer_done:  .byte   $5A    ; initial wait = 90 frames
        sta     ent_timer               ; set player timer
        lda     ent_flags               ; load player flags
        ora     #ENT_FLAG_HFLIP         ; set horizontal flip
        sta     ent_flags               ; store updated facing
        lda     #$78                    ; override: wait = $78 frames (120)
        sta     ent_timer               ; override timer to 120 frames
        inc     ent_status              ; advance sub-phase (0 → 1)
aw_wait_timer:  lda     ent_timer       ; timer active?
        beq     aw_timer_done           ; 0 → done waiting
        dec     ent_timer               ; decrement timer
        lda     #$01                    ; set stand OAM ($01)
        jsr     reset_sprite_anim       ; set standing animation
aw_ret:  rts                            ; return to caller

aw_timer_done:  lda     ent_status      ; sub-phase (lower nibble)
        and     #$0F                    ; isolate sub-phase
        cmp     #$02                    ; phase 2 = post-jump (landed)
        beq     weapon_hurt_ground_check ; yes → check ground
        lda     ent_x_px                ; get player X position
        cmp     #$A0                    ; reached X=$A0?
        beq     weapon_hurt_jump_velocity ; yes → initiate jump
        lda     #$04                    ; set walk OAM ($04)
        cmp     ent_anim_id             ; already walk OAM?
        beq     weapon_hurt_walk_right  ; yes → skip anim reset
        jsr     reset_sprite_anim       ; set walk animation
weapon_hurt_walk_right:  inc     ent_x_px ; walk right 1 pixel per frame
        lda     ent_x_px                ; check if reached X=$A0
        cmp     #$A0                    ; at jump trigger point?
        bne     weapon_hurt_return      ; not there yet → return
        lda     #$6E                    ; Break Man teleport OAM ($6E)
        sta     $05DF                   ; set slot $1F OAM
        lda     #$00                    ; clear slot $1F anim fields
        sta     $05FF                   ; clear ent_anim_frame
        sta     $05BF                   ; clear ent_anim_state
        lda     #$3C                    ; wait $3C frames (60) before jump
        sta     ent_timer               ; set wait timer
        rts                             ; return to caller

weapon_hurt_jump_velocity:  lda     #$E5 ; y_speed_sub = $E5
        sta     ent_yvel_sub            ; set Y sub-velocity
        lda     #$04                    ; y_speed = $04 (jump vel = $04.E5 upward)
        sta     ent_yvel                ; y_speed = $04 (jump velocity $04.E5)
        inc     ent_status              ; advance to phase 2
        rts                             ; return to caller

weapon_hurt_ground_check:  lda     $0F  ; bit 7 = on ground? (from ROR carry)
        bpl     weapon_hurt_drift_left  ; airborne → drift left
        lda     ent_var1                ; check Break Man defeated flag
        bne     weapon_hurt_set_inactive ; defeated → teleport out
        inc     ent_var1                ; first landing: set flag, wait $78 frames
        lda     #$78                    ; wait 120 frames after landing
        sta     ent_timer               ; set countdown timer
weapon_hurt_drift_left:  dec     ent_x_px ; drift left 1 pixel per frame
        rts                             ; return to caller

weapon_hurt_set_inactive:  lda     #$81 ; set teleport status
        sta     ent_status              ; mark player as teleporting
        lda     #$00                    ; clear timer
        sta     ent_timer               ; clear phase timer
        lda     #PSTATE_TELEPORT        ; set state $0D (teleport)
        sta     player_state            ; store player state
weapon_hurt_return:  rts

; ===========================================================================
; player_auto_walk_2 — state $15: boss-defeated ending cutscene
; ===========================================================================
; Walk player to X=$68, stand still, spawn 4 explosion entities at
; positions from auto_walk_2_x_table, then spawn weapon orb pickup.
; ent_timer = frame counter (counts up, wraps at $FF→$00 to trigger spawns)
; ent_var1 = explosion count (0-3, then 4 = done spawning)
; ---------------------------------------------------------------------------
player_auto_walk_2:

        ldy     #$00                    ; no horizontal movement
        jsr     move_vertical_gravity   ; apply gravity
        bcs     explosion_award_walk_oam ; landed?
        lda     #$07                    ; airborne: jump OAM ($07)
        bne     explosion_award_oam_check ; (always taken)
explosion_award_walk_oam:  lda     #$04 ; on ground: walk OAM ($04)
explosion_award_oam_check:  cmp     ent_anim_id ; already set?
        beq     explosion_award_walk_x_check ; yes → skip
        jsr     reset_sprite_anim       ; reset animation state
explosion_award_walk_x_check:  lda     ent_x_px ; reached X=$68?
        cmp     #$68                    ; target X position
        beq     explosion_award_stand_oam ; yes → stop and spawn explosions
        bcs     explosion_award_walk_left ; past target → walk left
        inc     ent_x_px                ; walk right
        rts                             ; return to caller

explosion_award_walk_left:  dec     ent_x_px ; walk left
        rts                             ; return to caller

explosion_award_stand_oam:  lda     #$01 ; set stand OAM ($01)
        cmp     ent_anim_id             ; already set?
        beq     explosion_award_timer_inc ; yes → skip
        jsr     reset_sprite_anim       ; reset animation state
explosion_award_timer_inc:  inc     ent_timer ; increment frame counter
        bne     explosion_award_return  ; not wrapped → wait
        lda     #$C0                    ; reset counter to $C0 (192 frames between spawns)
        sta     ent_timer               ; set timer to $C0
        lda     ent_var1                ; all 4 explosions spawned?
        cmp     #$04                    ; all 4 explosions spawned?
        beq     explosion_award_return  ; yes → done
        jsr     find_enemy_freeslot_y   ; find free entity slot
        bcs     explosion_award_return  ; none free → skip
        lda     #$80                    ; type = $80 (generic cutscene entity)
        sta     ent_status,y            ; set entity active
        lda     #$90                    ; flags = active + bit 4
        sta     ent_flags,y             ; set entity flags
        lda     #$00                    ; clear position/velocity fields:
        sta     ent_y_px,y              ; y_pixel = 0
        sta     ent_y_scr,y             ; y_screen = 0
        sta     ent_anim_frame,y        ; field ent_anim_frame = 0
        sta     ent_anim_state,y        ; field ent_anim_state = 0
        sta     ent_hitbox,y            ; shape = 0 (no hitbox)
        sta     ent_yvel_sub,y          ; y_speed_sub = 0
        sta     ent_yvel,y              ; y_speed = 0
        lda     #$7C                    ; OAM = $7C (boss explosion anim)
        sta     ent_anim_id,y           ; set explosion OAM
        lda     #$F9                    ; routine = $F9 (explosion AI)
        sta     ent_routine,y           ; set explosion AI routine
        lda     #$C4                    ; ai_timer = $C4 (countdown)
        sta     ent_var1,y              ; set explosion ai_timer
        lda     ent_var1                ; player ent_var1 = explosion index
        sta     ent_timer,y             ; store explosion index as timer
        tax                             ; X = explosion index
        inc     ent_var1                ; advance to next explosion
        lda     auto_walk_2_x_table,x   ; x_pixel = auto_walk_2_x_table[index]
        sta     ent_x_px,y              ; set explosion X position
        lda     camera_screen           ; x_screen = camera screen
        sta     ent_x_scr,y             ; set explosion X screen
        ldx     #$00                    ; restore X = player slot 0
explosion_award_return:  rts

; -----------------------------------------------
; spawn_weapon_orb — create post-boss weapon pickup in entity slot $0E
; -----------------------------------------------
; Called from bank1C_1D after boss defeat to spawn the weapon orb that
; gives Mega Man the defeated boss's special weapon.
; Slot $0E (index $0E): hardcoded, not searched via find_freeslot.
; $6C = boss/weapon index, used to look up position and CHR from tables.
; ---------------------------------------------------------------------------
spawn_weapon_orb:

        lda     #$80                    ; $030E = type $80 (slot $0E)
        sta     $030E                   ; set slot $0E active
        lda     #$90                    ; $058E = flags: active + bit 4
        sta     $058E                   ; set slot $0E flags
        lda     #$EB                    ; $032E = routine $EB (weapon orb AI)
        sta     $032E                   ; set slot $0E routine
        lda     #$67                    ; $05CE = OAM $67 (weapon orb sprite)
        sta     $05CE                   ; set slot $0E OAM
        lda     #$00                    ; clear fields:
        sta     $05EE                   ; field ent_anim_frame+$0E = 0
        sta     $05AE                   ; field ent_anim_state+$0E = 0
        sta     $03EE                   ; y_screen = 0
        sta     $B3                     ; $B3 = 0 (weapon orb energy bar inactive)
        lda     camera_screen           ; $038E = x_screen = camera screen
        sta     $038E                   ; set x_screen for slot $0E
        ldy     warp_dest               ; Y = boss/weapon index
        lda     weapon_cost_table,y     ; x_pixel from weapon_cost_table
        sta     $036E                   ; set x_pixel for slot $0E
        lda     weapon_use_rate_table,y ; y_pixel from weapon_use_rate_table
        and     #$F0                    ; align to 16-pixel boundary
        sta     $03CE                   ; set y_pixel for slot $0E
        lda     warp_dest               ; $04CE = stage_enemy_id = boss index + $18
        clc                             ; (prevents respawn tracking conflict)
        adc     #$18                    ; offset spawn ID by $18
        sta     $04CE                   ; set spawn_id for slot $0E
        lda     entity_ai_routine_table,y ; load boss-defeated bitmask
        ora     $6E                     ; set defeated bit in $6E
        sta     $6E                     ; store updated weapon bitmask
        lda     #$0C                    ; $EC = $0C (trigger CHR bank update)
        sta     $EC                     ; trigger CHR bank update
        jmp     update_CHR_banks        ; update sprite CHR for weapon orb

; explosion X positions for auto_walk_2 (4 boss explosions)

auto_walk_2_x_table:  .byte   $E0,$30,$B0,$68

; ===========================================================================
; update_camera — horizontal scroll engine + room transition detection
; ===========================================================================
; Called every frame from gameplay_frame_loop (step 3). Tracks the player's
; X position and smoothly scrolls the camera to keep the player centered.
;
; Scroll variables (all Mesen-verified on Snake Man stage):
;   $2A = scroll flags (from upper 3 bits of $AA40,y per room)
;         bit 5 = horizontal scrolling enabled
;         bits 6-7 = vertical connection direction ($40=down, $80=up)
;   $2B = room/section index (changes at transitions: ladders, doors)
;   $2C = screen count for current room (lower 5 bits of $AA40,y)
;   $2D = scroll progress within room (counts 0→$2C as camera advances)
;   $2E = scroll direction this frame (1=right, 2=left)
;   camera_screen ($F9) = screen position (increments each 256-pixel boundary)
;   camera_x_lo ($FC) = fine X (sub-screen pixel offset, 0-255)
;   $25 = previous camera_x_lo (for column rendering delta)
;   $27 = previous player X (for direction detection)
;   ent_x_px = player X pixel position (slot 0)
;
; Camera tracking:
;   $00 = player screen X (= ent_x_px - camera_x_lo)
;   $01 = scroll speed (= |player screen X - $80|, clamped to max 8)
;   $02 = player movement delta (= |ent_x_px - $27|)

;   When player is right of center ($00 > $80): scroll right
;   When player is left of center ($00 < $80): scroll left
;   Scroll speed adapts — faster when player is far from center,
;   uses movement delta as alternative speed when it's smaller
; ---------------------------------------------------------------------------
update_camera:  lda     $2A             ; check scroll flags
        and     #$20                    ; bit 5 = horizontal scroll enabled
        bne     camera_scroll_dir_right ; enabled → compute scroll
        jmp     camera_mid_scroll_check ; disabled → boundary clamp only

; --- horizontal scroll: compute player screen position and scroll speed ---

camera_scroll_dir_right:  lda     #$01  ; default: scroll direction = right
        sta     $10                     ; $10 = nametable select (1=right)
        sta     $2E                     ; $2E = scroll direction flag
        lda     ent_x_px                ; $00 = player screen X
        sec                             ; = player pixel X - camera fine X
        sbc     camera_x_lo             ; subtract camera fine X
        sta     temp_00                 ; store player screen X in $00
        sec                             ; prepare subtraction
        sbc     #$80                    ; = distance from screen center
        bcs     camera_compute_distance ; positive → player right of center
        eor     #$FF                    ; negate if negative
        adc     #$01                    ; two's complement negate
camera_compute_distance:  sta     $01   ; $01 = abs distance from center
        lda     ent_x_px                ; $02 = player X - previous X ($27)
        sec                             ; = movement delta this frame
        sbc     $27                     ; subtract previous player X ($27)
        sta     $02                     ; store movement delta
        bpl     camera_scroll_right_center ; positive → player moved right

; --- player moving left: negate delta, compute leftward scroll speed ---
        eor     #$FF                    ; negate to get abs delta
        clc                             ; prepare addition
        adc     #$01                    ; two's complement negate
        sta     $02                     ; store abs movement delta
        lda     $01                     ; if center distance >= 9,
        cmp     #$09                    ; use movement delta instead
        bcc     camera_scroll_left_clamp ; (prevents jerky scroll when
        lda     $02                     ; player is far off-center but
        sta     $01                     ; moving slowly)
camera_scroll_left_clamp:  lda     #$08 ; clamp scroll speed to max 8
        cmp     $01                     ; max 8 >= speed?
        bcs     camera_scroll_left_dir  ; yes → no clamp needed
        sta     $01                     ; $01 = min(8, $01)

; --- scroll camera left ---
camera_scroll_left_dir:  lda     #$02   ; direction = left
        sta     $10                     ; $10 = 2 (nametable select)
        sta     $2E                     ; $2E = 2 (scroll direction)
        lda     temp_00                 ; if player screen X >= $80 (right of center)
        cmp     #$80                    ; don't scroll left — go to boundary check
        bcs     camera_mid_scroll_check  ; right of center → don't scroll left
        lda     camera_x_lo             ; camera_x_lo -= scroll speed
        sec                             ; (move camera left)
        sbc     $01                     ; subtract scroll speed
        sta     camera_x_lo             ; store updated fine scroll
        bcs     camera_scroll_render_column ; no underflow → just render
        lda     $2D                     ; $FC underflowed: crossed screen boundary
        dec     $2D                     ; decrement room scroll progress
        bpl     camera_screen_decrement ; still in room? advance screen
        sta     $2D                     ; $2D went negative: at left boundary
        lda     #$00                    ; clamp $FC = 0 (can't scroll further)
        sta     camera_x_lo             ; clamp fine scroll to 0
        lda     #$10                    ; clamp player X to minimum $10
        cmp     ent_x_px                ; (16 pixels from left edge)
        bcc     camera_mid_scroll_check ; player X > $10 → ok
        sta     ent_x_px                ; enforce left boundary
        bcs     camera_mid_scroll_check ; always branch
camera_screen_decrement:  dec     camera_screen ; camera screen position--
        jmp     camera_scroll_render_column ; render new column

; --- scroll camera right ---

camera_scroll_right_center:  lda     temp_00 ; if player screen X < $81 (at/left of center)
        cmp     #$81                    ; don't scroll right
        bcc     camera_mid_scroll_check  ; left of center → don't scroll right
        lda     $2D                     ; if scroll progress == screen count
        cmp     $2C                     ; already at right boundary
        beq     camera_mid_scroll_check ; → don't scroll
        lda     $01                     ; if center distance >= 9,
        cmp     #$09                    ; use movement delta as speed
        bcc     camera_scroll_right_clamp ; distance < 9 → skip delta swap
        lda     $02                     ; use movement delta as speed
        sta     $01                     ; use delta as speed
camera_scroll_right_clamp:  lda     #$08 ; clamp scroll speed to max 8
        cmp     $01                     ; max 8 >= speed?
        bcs     camera_scroll_speed_zero ; yes → no clamp needed
        sta     $01                     ; $01 = min(8, $01)
camera_scroll_speed_zero:  lda     $01  ; speed = 0? nothing to do
        beq     camera_mid_scroll_check ; nothing to scroll
        lda     camera_x_lo             ; camera_x_lo += scroll speed
        clc                             ; (move camera right)
        adc     $01                     ; add scroll speed
        sta     camera_x_lo             ; store updated fine scroll
        bcc     camera_scroll_render_column ; no overflow → just render
        inc     $2D                     ; $FC overflowed: crossed screen boundary
        lda     $2D                     ; increment room scroll progress
        cmp     $2C                     ; reached room right boundary?
        bne     camera_screen_increment ; not at boundary? advance screen
        lda     #$00                    ; at right boundary: clamp $FC = 0
        sta     camera_x_lo             ; clamp fine scroll to 0
        lda     #$F0                    ; clamp player X to maximum $F0
        cmp     ent_x_px                ; (240 pixels from left edge)
        bcs     camera_screen_increment ; player X <= $F0 → ok
        sta     ent_x_px                ; enforce right boundary
camera_screen_increment:  inc     camera_screen ; camera screen position++
camera_scroll_render_column:  jmp     render_scroll_column ; → column rendering

camera_h_scroll_return:  rts

; ===========================================================================
; No-scroll: boundary clamping + room transition detection
; ===========================================================================
; When horizontal scrolling is disabled or camera is between screens,
; clamp player X to screen bounds and check if player has walked to
; the edge of the screen (triggering a room transition via ladder/door).
; ---------------------------------------------------------------------------

camera_mid_scroll_check:  lda     camera_x_lo ; if camera is mid-scroll (camera_x_lo != 0)
        bne     camera_h_scroll_return  ; nothing more to do
        lda     ent_x_px                ; clamp player X >= $10 (left edge)
        cmp     #$10                    ; at left edge?
        bcs     camera_right_edge_check ; X >= $10 → check right edge
        lda     #$10                    ; clamp player X to $10
        sta     ent_x_px                ; enforce left boundary
        beq     camera_right_edge_check ; (always branches, A=$10)
camera_check_vert_transition:  jmp     check_vertical_transition ; check vertical transition

camera_right_edge_check:  cmp     #$E5  ; player X < $E5? not at right edge
        bcc     camera_check_vert_transition ; → check vertical instead
        cmp     #$F0                    ; clamp player X <= $F0
        bcc     camera_room_transition_right ; in range → check transition
        lda     #$F0                    ; clamp player X to $F0
        sta     ent_x_px                ; enforce right boundary

; --- room transition: player walked to right edge ($E5+) of non-scrolling screen ---
camera_room_transition_right:  ldy     $2B ; current room entry
        lda     $AA40,y                 ; check if current room allows
        and     #$20                    ; horizontal scroll (bit 5)
        beq     camera_check_vert_transition ; no scroll attr → vertical check
        lda     $AA41,y                 ; next room entry: check vertical
        and     #$C0                    ; connection bits (6-7)
        bne     camera_check_vert_transition ; has vertical link → not a horiz transition
        lda     $AA41,y                 ; next room: must also have horiz scroll
        and     #$20                    ; (bit 5 set) to allow transition
        beq     camera_check_vert_transition ; no scroll → vertical check
        sta     temp_00                 ; store next room scroll flags
        lda     stage_id                ; stage-specific transition blocks:
        cmp     #STAGE_DOC_NEEDLE       ; stage $08 (Doc Robot Needle)
        bne     camera_screen_offset_check ; skip if not stage $08
        lda     camera_screen           ; Rush Marine water boundary screens
        cmp     #$15                    ; $15 and $1A have special gate
        beq     camera_entity_slot_check ; screen $15 → check gate
        cmp     #$1A                    ; screen $1A → check gate
        bne     camera_screen_offset_check ; other screen → skip gate check
camera_entity_slot_check:  lda     $033F ; entity slot $1F type == $FC?
        cmp     #$FC                    ; (gate/shutter entity present
        beq     camera_check_vert_transition ; → block transition until opened)
camera_screen_offset_check:  lda     camera_screen ; load camera screen position
        sec                             ; subtract room base screen
        sbc     $AA30                   ; compute screen offset
        cmp     #$02                    ; if exactly 2 screens in:
        bne     camera_wily5_stage_check ; not 2 screens → skip boss shutter check
        lda     $031F                   ; check entity slot $1F status
        bmi     camera_check_vert_transition ; active → block room transition
        lda     player_state            ; player state >= $0C (victory)?
        cmp     #PSTATE_VICTORY         ; block if in cutscene state
        bcs     camera_check_vert_transition ; cutscene → block transition
camera_wily5_stage_check:  lda     stage_id ; stage $0F (Wily Fortress 4)
        cmp     #STAGE_WILY4            ; special check
        bne     camera_next_room_scroll ; not Wily 4 → advance to next room
        lda     camera_screen           ; only on screen $08
        cmp     #$08                    ; boss refight screen?
        bne     camera_next_room_scroll ; no → advance normally
        ldx     #$0F                    ; scan enemy slots $1F-$10
camera_entity_status_check:  lda     $0310,x ; ent_status[$10+x]
        bmi     camera_check_vert_transition ; bit 7 = active → block transition
        dex                             ; next enemy slot
        bpl     camera_entity_status_check ; check all 16 slots

; --- advance to next room: set up new room variables ---
camera_next_room_scroll:  lda     temp_00 ; next room's scroll flags
        sta     $2A                     ; store scroll flags
        lda     $AA41,y                 ; $2C = next room's screen count
        and     #$1F                    ; (lower 5 bits)
        sta     $2C                     ; store screen count
        lda     #$00                    ; $2D = 0 (start of new room)
        sta     $2D                     ; reset scroll progress
        inc     $2B                     ; room index++
        ldy     game_mode               ; save game_mode before clearing
        lda     #$00                    ; then clear transition state:
        sta     game_mode               ; game_mode = 0 (normal rendering)
        sta     $76                     ; $76 = 0 (enemy spawn flag)
        sta     $B3                     ; $B3 = 0 (?)
        sta     boss_active             ; clear boss active flag
        lda     #$E8                    ; $5E = $E8 (despawn boundary Y)
        sta     $5E                     ; set despawn boundary Y
        cpy     #$02                    ; was $F8 == 2? (camera scroll mode)
        bne     camera_nt_ptr_setup     ; no → skip
        lda     #$42                    ; special scroll init:
        sta     $E9                     ; $E9 = $42 (CHR bank?)
        lda     #$09                    ; $29 = $09 (metatile column)
        sta     $29                     ; set metatile column base
        jsr     load_room               ; load room layout + CHR/palette
camera_nt_ptr_setup:  lda     camera_screen ; compute screen offset from room base
        sec                             ; prepare subtraction
        sbc     $AA30                   ; subtract room base screen
        bcc     camera_stage_specific_check ; negative → no event trigger
        cmp     #$03                    ; < 3 screens: use table 1 ($AA31)
        bcs     camera_screen_count_3   ; >= 3 → use second table
        tax                             ; X = screen offset
        ldy     $AA31,x                 ; Y = event ID from table
        jmp     camera_bank10_event_call ; call event handler

camera_screen_count_3:  lda     camera_screen ; >= 3 screens: use second table
        sec                             ; compute offset from second base
        sbc     $AA38                   ; subtract second base screen
        bcc     camera_stage_specific_check ; negative → no event
        tax                             ; X = screen offset
        ldy     $AA39,x                 ; Y = event ID from table
camera_bank10_event_call:  jsr     call_bank10_8000 ; call bank $10 event handler
camera_stage_specific_check:  lda     stage_id ; stage-specific: Needle Man ($00)
        bne     camera_enter_from_left  ; skip if not Needle Man stage
        ldx     #$03                    ; copy 4 bytes from $AAA2 → $060C
camera_stage_spawn_table:  lda     $AAA2,x ; copy stage spawn data
        sta     $060C,x                 ; to $060C buffer
        dex                             ; next byte index
        bpl     camera_stage_spawn_table ; loop all 4 bytes
        stx     palette_dirty           ; palette_dirty = $FF (force update)
camera_enter_from_left:  lda     #$E4   ; player X = $E4 (entering from left)
        sta     ent_x_px                ; set player X pixel
        jsr     fast_scroll_right       ; clear entities + fast-scroll camera
        lda     stage_id                ; re-select stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        lda     camera_screen           ; second event dispatch (post-scroll)
        sec                             ; prepare subtraction
        sbc     $AA30                   ; subtract room base screen
        bcc     scroll_engine_rts       ; negative → done
        cmp     #$05                    ; < 5: use $AA30 table
        bcs     camera_screen_count_5   ; >= 5 → use second table
        tax                             ; X = screen offset
        ldy     $AA30,x                 ; Y = event ID from table
        jmp     camera_bank10_post_scroll ; call post-scroll handler

camera_screen_count_5:  lda     camera_screen ; >= 5: use $AA38 table
        sec                             ; prepare subtraction
        sbc     $AA38                   ; subtract second base screen
        bcc     scroll_engine_rts       ; negative → done
        beq     scroll_engine_rts       ; zero → done
        tax                             ; X = screen offset
        ldy     $AA38,x                 ; Y = event ID from table
camera_bank10_post_scroll:  jsr     call_bank10_8003 ; call bank $10 post-scroll handler
scroll_engine_rts:  rts

; ===========================================================================
; check_vertical_transition — detect player at top/bottom of screen
; ===========================================================================
; When horizontal scrolling is not active ($FC==0, not scrolling), checks
; if the player has reached the top or bottom of the visible screen.
;   ent_y_px = player Y pixel, ent_y_scr = player Y screen
;   $10 = direction flag: $80=up, $40=down
;   $23 = vertical scroll direction: $08=up, $04=down
; ---------------------------------------------------------------------------

check_vertical_transition:  lda     ent_y_px ; player Y pixel
        cmp     #$E8                    ; >= $E8? → fell off bottom
        bcs     camera_vert_check_y_screen ; yes → check Y screen page
        cmp     #$09                    ; < $09? → at top of screen
        bcs     scroll_engine_rts       ; $09-$E7 = normal range, RTS
        lda     player_state            ; must be climbing (state $03)
        cmp     #PSTATE_LADDER          ; to transition upward
        bne     scroll_engine_rts       ; not on ladder → skip
        lda     #$80                    ; $10 = $80 (upward direction)
        sta     $10                     ; set direction = up
        jsr     check_room_link         ; validate room connection
        bcc     scroll_engine_rts       ; C=0 → no valid link
        lda     #$80                    ; confirmed: transition up
        sta     $10                     ; set direction = up
        lda     #$08                    ; $23 = $08 (vertical scroll up)
        bne     camera_scroll_dir_set   ; always branch (set scroll dir)
camera_vert_check_y_screen:  lda     ent_y_scr ; player Y screen: bit 7 set?
        bmi     camera_check_vert_end   ; negative = death pit → RTS
        lda     #$40                    ; $10 = $40 (downward direction)
        sta     $10                     ; set direction = down
        jsr     check_room_link         ; validate room connection
        bcc     camera_check_vert_end   ; C=0 → no valid link (death)
        lda     #$40                    ; confirmed: transition down
        sta     $10                     ; set direction = down
        lda     #$04                    ; $23 = $04 (vertical scroll down)

; --- begin vertical room transition ---
camera_scroll_dir_set:  sta     $23     ; $23 = scroll direction ($04/$08)
        lda     #$00                    ; reset Y screen to 0
        sta     ent_y_scr               ; clear Y screen
        ldx     #$01                    ; $12 = direction-dependent flag
        ldy     $2B                     ; check if current room's vertical
        lda     $AA40,y                 ; connection matches $10
        and     #$C0                    ; isolate vertical link bits (6-7)
        cmp     $10                     ; compare with requested direction
        beq     camera_stx_load         ; match → use default $12
        ldx     #$FF                    ; mismatch → $12 = $FF (reverse)
camera_stx_load:  stx     $12
        lda     #$00                    ; clear Y sub-pixel
        sta     ent_y_sub               ; clear Y sub-pixel
        sta     game_mode               ; game_mode = 0 (normal render mode)
        lda     #$E8                    ; $5E = $E8 (despawn boundary Y)
        sta     $5E                     ; $5E = $E8 (despawn boundary)
        jsr     clear_destroyed_blocks  ; reset breakable blocks
        jsr     vertical_scroll_animate ; animate vertical scroll
        lda     stage_id                ; re-select stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldy     $2B                     ; update $2A from new room's scroll flags
        lda     $AA40,y                 ; load new room flags
        and     #$20                    ; (bit 5 = horizontal scroll)
        beq     camera_mmc3_protect_enable ; no h-scroll → skip
        sta     $2A                     ; store scroll flags
camera_mmc3_protect_enable:  lda     #$01 ; set H-mirroring
        sta     MMC3_MIRRORING          ; apply H-mirroring
        lda     #$2A                    ; $52 = $2A (viewport height, h-mirror)
        sta     $52                     ; set viewport height
        jsr     load_room               ; load room layout + CHR/palette
        ldx     #$00                    ; check player tile collision at (0,4)
        ldy     #$04                    ; to snap player into new room
        jsr     check_tile_collision    ; check tile at player feet
        lda     $10                     ; if tile collision result = 0 (air)
        bne     camera_check_vert_end   ; nonzero → solid tile, skip state change
        sta     player_state            ; (landing after vertical transition)
camera_check_vert_end:  rts

; ===========================================================================
; check_room_link — validate that a vertical room connection exists
; ===========================================================================
; Checks if the player can transition to an adjacent room vertically.
; Only allows transitions when at the start ($2D==0) or end ($2D==$2C)
; of the current room's scroll range.
;
; Input:  $10 = direction ($80=up, $40=down)
; Output: C=1 if valid link found, C=0 if no connection
;         Sets up $2B, $2A, $2C, $2D, $29, $F9, ent_x_scr on success
; ---------------------------------------------------------------------------

check_room_link:  lda     $2D           ; scroll progress at room boundary?
        beq     camera_vert_target_screen ; $2D == 0 (at room start) → check link
        cmp     $2C                     ; $2D == $2C (end) → check link
        beq     camera_vert_target_screen ; $2D == $2C (at room end) → check link
        clc                             ; midway through room → no transition
        rts                             ; C=0 → no link

; --- navigate $AA40 room table to find adjacent room in direction $10 ---

camera_vert_target_screen:  lda     camera_screen ; $00 = target screen position
        sta     temp_00                 ; $00 = current screen position
        ldy     $2B                     ; Y = current room index
        lda     $2A                     ; check current room's vertical bits
        and     #$C0                    ; bits 6-7 of $2A
        beq     camera_multi_screen_check ; no vertical → check neighbors
        lda     $2A                     ; current room has vertical link:
        cmp     $10                     ; does direction match?
        beq     room_link_next_entry    ; same direction → scan forward
        bne     room_link_prev_entry    ; opposite → scan backward
camera_multi_screen_check:  lda     $2C ; multi-screen room ($2C > 0)?
        bne     camera_vert_backward_check ; yes → check scroll position
        lda     $AA41,y                 ; next entry's vertical bits
        and     #$C0                    ; match direction?
        cmp     $10                     ; match direction?
        beq     room_link_next_entry    ; yes → look forward
        lda     $AA3F,y                 ; previous entry's vertical bits
        and     #$C0                    ; (inverted: $C0 XOR = flip up/down)
        eor     #$C0                    ; flip up/down bits
        cmp     $10                     ; match requested direction?
        beq     room_link_prev_entry    ; yes → scan backward
        bne     room_link_no_valid      ; no match → fail
camera_vert_backward_check:  lda     $2D ; $2D == 0 (at start) → look backward
        beq     room_link_prev_entry    ; else (at end) → look forward
room_link_next_entry:  iny              ; check next room entry
        lda     $AA40,y                 ; vertical bits must match $10
        and     #$C0                    ; isolate vertical bits
        cmp     $10                     ; match requested direction?
        bne     room_link_no_valid      ; no match → fail
        inc     temp_00                 ; target screen = $F9 + 1
        bne     room_link_setup_vars    ; always branch (screen > 0)
room_link_prev_entry:  lda     $AA40,y  ; current entry must have vert bits
        and     #$C0                    ; isolate vertical link bits
        beq     room_link_no_valid      ; no vertical → fail
        dey                             ; move to previous room entry
        bmi     room_link_no_valid      ; Y < 0 → no previous room
        lda     $AA40,y                 ; check prev room's vertical bits
        and     #$C0                    ; isolate vertical link bits
        bne     room_link_vert_check    ; nonzero → has vertical link
        lda     $AA41,y                 ; no vert bits → check next entry
room_link_vert_check:  eor     #$C0     ; invert and compare with direction
        cmp     $10                     ; (loop safety: BEQ loops if match,
        beq     room_link_vert_check    ; but this shouldn't infinite-loop)
        dec     temp_00                 ; target screen = $F9 - 1
        lda     $AA40,y                 ; load new room's flags

; --- link found: set up new room variables ---
room_link_setup_vars:  sta     $2A      ; $2A = new room's scroll flags
        lda     #$01                    ; $2E = 1 (forward direction)
        sta     $2E                     ; scroll forward
        cpy     $2B                     ; if new room index < current
        sty     $2B                     ; (going backward in table)
        bcs     room_link_set_screen_count ; forward → $2D stays 0
        lda     #$02                    ; $2E = 2 (backward direction)
        sta     $2E                     ; scroll backward
room_link_set_screen_count:  lda     $AA40,y ; $2C = new room's screen count
        and     #$1F                    ; mask to screen count
        sta     $2C                     ; store screen count
        ldx     temp_00                 ; if target screen >= current $F9:
        cpx     camera_screen           ; target screen >= camera screen?
        bcc     room_link_set_position  ; target < camera → set $2D = $2C (end)
        lda     #$00                    ; $2D = 0 (room start)
room_link_set_position:  sta     $2D    ; set scroll progress
        lda     temp_00                 ; update camera and player X screen
        sta     $29                     ; $29 = metatile column base
        sta     camera_screen           ; set camera screen
        sta     ent_x_scr               ; set player X screen
        lda     #$00                    ; set V-mirroring
        sta     MMC3_MIRRORING          ; apply V-mirroring
        lda     #$26                    ; $52 = $26 (viewport height for V-mirror)
        sta     $52                     ; set viewport height
        sec                             ; C=1 → link found
        rts                             ; return with carry set

room_link_no_valid:  clc                ; C=0 → no valid link
        rts                             ; return with carry clear

; ===========================================================================
; render_scroll_column — queue nametable column updates during scrolling
; ===========================================================================
; Called after horizontal scroll speed is applied to $FC. Determines how
; many columns have scrolled into view and queues PPU nametable updates
; via the $0780 NMI buffer.
;
; When $F8 == 2 (dual-nametable mode): renders columns for BOTH nametables
; (first call renders primary, then mirrors to secondary nametable offset).
; Otherwise: renders single column only.
;
; $25 = previous camera_x_lo, camera_x_lo = current fine scroll position
; nt_column_dirty = flag to trigger NMI buffer drain
; ---------------------------------------------------------------------------

render_scroll_column:  lda     game_mode ; if game_mode == 2: dual nametable mode
        cmp     #$02                    ; dual nametable mode?
        bne     scroll_column_single    ; no → single column render
        jsr     scroll_column_single    ; render first nametable column
        bcs     scroll_no_column        ; C=1 → no column needed, skip mirror
        lda     $0780                   ; mirror to second nametable:
        ora     #$22                    ; set bit 1 of high byte ($20→$22)
        sta     $0780                   ; store mirrored PPU addr high byte
        lda     $0781                   ; mirror column to second nametable
        ora     #$80                    ; set bit 7 of low byte
        sta     $0781                   ; store mirrored PPU addr low byte
        lda     #$09                    ; count = 9 (copy 10 tile rows)
        sta     $0782                   ; 10 tile rows to copy
        ldy     #$00                    ; copy attribute data to second
camera_attr_copy_loop:  lda     $0797,y ; read tile data from primary buffer
        sta     $0783,y                 ; copy to secondary buffer
        iny                             ; next byte
        cpy     #$0A                    ; 10 bytes total?
        bne     camera_attr_copy_loop   ; loop until done
        lda     $07A1                   ; check if attribute entry exists
        bpl     camera_attr_finalize    ; bit 7 clear → has attribute data
        sta     $078D                   ; no second attr: terminate here
        sta     nt_column_dirty         ; flag NMI to drain buffer
        rts                             ; return to caller

camera_attr_finalize:  ldy     #$00     ; copy second attribute section
scroll_column_attr_copy:  lda     $07B5,y ; copy secondary attribute section
        sta     $078D,y                 ; to mirror buffer
        iny                             ; next byte
        cpy     #$0D                    ; 13 bytes total?
        bne     scroll_column_attr_copy ; loop until done
        sta     nt_column_dirty         ; flag NMI to drain buffer
        rts                             ; return to caller

; --- single_column: compute if a column crossed an 8-pixel boundary ---
; $03 = |camera_x_lo - $25| = absolute scroll delta since last frame
; If delta + fine position crosses an 8-pixel tile boundary, we need
; to render a new column. $24 = nametable column pointer, $29 = metatile
; column. Direction tables at $E5C3/$E5CD/$E5CF configure left vs right.

scroll_column_single:  lda     camera_x_lo ; $03 = |camera_x_lo - $25
        sec                             ; absolute scroll delta
        sbc     $25                     ; delta = $FC - $25 (previous fine pos)
        bpl     scroll_compute_delta    ; positive → no negate needed
        eor     #$FF                    ; negate if negative
        clc                             ; prepare addition
        adc     #$01                    ; two's complement negate
scroll_compute_delta:  sta     $03      ; $03 = scroll delta (pixels)
        beq     scroll_no_column        ; zero delta → no column to render
        lda     $23                     ; if $23 has vertical scroll bits
        and     #$0C                    ; ($04 or $08), this is first frame
        beq     scroll_direction_setup  ; no vert bits → normal direction check
        lda     $10                     ; overwrite $23 with current direction
        sta     $23                     ; store current direction to $23
        and     #$01                    ; X = direction index (0=left, 1=right)
        tax                             ; X = direction index
        lda     scroll_init_fine_pos_table,x ; init $25 from direction table
        sta     $25                     ; store initial fine scroll position
        lda     scroll_init_column_table,x ; init $24 from direction table
        sta     $24                     ; store initial nametable column
scroll_direction_setup:  lda     $10    ; get fine position for boundary check:
        and     #$01                    ; direction 1 (right): use $25 as-is
        beq     scroll_left_direction   ; direction 0 (left) → invert fine pos
        lda     $25                     ; rightward: use fine pos directly
        jmp     scroll_boundary_check   ; skip inversion

scroll_left_direction:  lda     $25     ; invert for leftward scroll
        eor     #$FF                    ; bitwise complement for left direction
scroll_boundary_check:  and     #$07    ; (fine pos & 7) + delta
        clc                             ; if result / 8 > 0: crossed boundary
        adc     $03                     ; → need to render new column
        lsr     a                       ; divide by 8:
        lsr     a                       ;   result > 0 means crossed tile boundary
        lsr     a                       ;   (8 pixels per tile column)
        bne     do_render_column        ; nonzero → render new column
scroll_no_column:  sec                  ; C=1 → no column to render
        rts                             ; return with carry set

; --- render column: advance nametable pointer and build PPU update ---

do_render_column:  lda     $10          ; X = direction (0 or 1)
        pha                             ; save direction on stack
        and     #$01                    ; isolate direction bit
        tax                             ; X = direction index (0 or 1)
        pla                             ; if direction changed since last frame
        cmp     $23                     ; ($10 != $23): skip column advance,
        sta     $23                     ; just update metatile base
        beq     scroll_column_done      ; same direction → advance column
        jmp     scroll_advance_metatile ; direction changed → skip column advance

scroll_column_done:  lda     $24        ; advance nametable column pointer
        clc                             ; $24 += direction step ($E5C3,x)
        adc     scroll_direction_step_table,x ; add +1 or -1 per direction
        cmp     #$20                    ; wrap at 32 columns (NES nametable)
        and     #$1F                    ; mask to 0-31 range
        sta     $24                     ; store updated column pointer
        bcc     scroll_build_column     ; no column wrap → skip metatile advance
scroll_advance_metatile:  lda     $29   ; $29 += direction step
        clc                             ; (metatile column in level data)
        adc     scroll_direction_step_table,x ; add +1 or -1 per direction
        sta     $29                     ; store updated metatile column

; --- build nametable column from metatile data ---
scroll_build_column:  lda     stage_id  ; select stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        lda     $24                     ; $28 = attribute table row
        lsr     a                       ; = nametable column / 4
        lsr     a                       ; divide column by 4
        sta     $28                     ; store attribute row index
        ldy     $29                     ; set up metatile data pointer
        jsr     metatile_column_ptr     ; for column $29
        lda     #$00                    ; $03 = buffer write offset
        sta     $03                     ; (increments by 4 per row)
scroll_metatile_loop:  jsr     metatile_to_chr_tiles ; decode metatile → $06C0 tile buffer
        ldy     $28                     ; $11 = current attribute byte
        lda     $0640,y                 ; from attribute cache
        sta     $11                     ; save attribute byte
        lda     $24                     ; Y = column within metatile (0-3)
        and     #$03                    ; mask to sub-column (0-3)
        tay                             ; Y = column sub-position
        ldx     $03                     ; write 2 or 4 CHR tiles to buffer
        lda     $06C0,y                 ; top-left tile
        sta     $0783,x                 ; write to PPU column buffer
        lda     $06C4,y                 ; bottom-left tile
        sta     $0784,x                 ; store to PPU column buffer
        lda     $28                     ; if row >= $38 (bottom 2 rows):
        cmp     #$38                    ; skip second pair (status bar area)
        bcs     scroll_attribute_check  ; yes → skip remaining tiles
        lda     $06C8,y                 ; top-right tile
        sta     $0785,x                 ; store to PPU column buffer
        lda     $06CC,y                 ; bottom-right tile
        sta     $0786,x                 ; store to PPU column buffer
scroll_attribute_check:  lda     $24    ; if column is odd: update attribute byte
        and     #$01                    ; (attributes cover 2×2 metatile groups)
        beq     scroll_advance_buffer   ; even column → skip attribute update
        lda     $10                     ; merge new attribute bits:
        and     scroll_attr_mask_left_table,y ; mask with direction table
        sta     $10                     ; save masked new attribute
        lda     $11                     ; combine with old attribute
        and     scroll_attr_mask_right_table,y ; mask old attribute bits
        ora     $10                     ; merge old and new attributes
        sta     $07A4,x                 ; store in attribute buffer
        ldy     $28                     ; update attribute cache
        sta     $0640,y                 ; update attribute cache
        lda     #$23                    ; attribute table PPU address:
        sta     $07A1,x                 ; $23C0 + row offset
        tya                             ; A = attribute row index
        ora     #$C0                    ; form low byte ($C0 + row)
        sta     $07A2,x                 ; store attr PPU addr low byte
        lda     #$00                    ; count = 0 (single byte)
        sta     $07A3,x                 ; 1 attribute byte per entry
scroll_advance_buffer:  inc     $03     ; advance buffer offset by 4
        inc     $03                     ; (4 bytes per metatile row entry)
        inc     $03                     ; (2 tiles + 2 spare)
        inc     $03                     ; total +4
        lda     $28                     ; advance attribute row by 8
        clc                             ; (each metatile = 8 pixel rows)
        adc     #$08                    ; next metatile row block
        sta     $28                     ; store updated row index
        cmp     #$40                    ; < $40 (8 rows)? loop
        bcc     scroll_metatile_loop    ; loop until all 8 metatile rows done
        lda     #$20                    ; finalize PPU buffer header:
        sta     $0780                   ; $0780 = $20 (nametable $2000 base)
        lda     $24                     ; $0781 = nametable column
        sta     $0781                   ; store nametable column
        lda     #$1D                    ; $0782 = $1D (30 tiles = full column)
        sta     $0782                   ; 30 tiles per column
        ldy     #$00                    ; select attribute buffer terminator pos:
        lda     $24                     ; odd column → Y=$20 (secondary buffer)
        and     #$01                    ; even column → Y=$00 (primary buffer)
        beq     scroll_write_end_marker ; even → use primary attr buffer
        ldy     #$20                    ; Y = $20 (secondary attr buffer)
scroll_write_end_marker:  lda     #$FF  ; write $FF end marker
        sta     $07A1,y                 ; write end marker to attr buffer
        ldy     game_mode               ; if game_mode == 2 (dual nametable):
        cpy     #$02                    ; don't flag NMI yet (caller handles it)
        beq     scroll_success_exit     ; dual mode → caller flags NMI
        sta     nt_column_dirty         ; else: flag NMI to drain buffer
scroll_success_exit:  clc               ; C=0 → column was rendered
        rts                             ; return with carry clear

; Direction/column rendering tables:
; $E5C3: direction step (+1/-1 for right/left)
; $E5C5: attribute mask tables (4 bytes each)
; $E5CD: initial $25 values per direction
; $E5CF: initial $24 values per direction

scroll_direction_step_table:  .byte   $FF,$01 ; left = -1, right = +1
scroll_attr_mask_left_table:  .byte   $33,$33,$CC,$CC
scroll_attr_mask_right_table:  .byte   $CC,$CC,$33,$33
scroll_init_fine_pos_table:  .byte   $00,$FF
scroll_init_column_table:  .byte   $01,$1F

; ===========================================================================
; fast_scroll_right — rapid camera scroll during room transition
; ===========================================================================
; Called during horizontal room advance. Scrolls camera right at 4 pixels
; per frame while simultaneously advancing player X by ~$D0/$100 per frame
; (net rightward movement). Renders columns each frame and yields.
; Loops until $FC wraps back to 0 (full screen scrolled).
; ---------------------------------------------------------------------------
fast_scroll_right:  jsr     clear_entity_table ; clear all enemies
fast_scroll_right_loop:  lda     camera_x_lo ; camera_x_lo += 4 (scroll 4 pixels/frame)
        clc                             ; prepare addition
        adc     #$04                    ; scroll 4 pixels per frame
        sta     camera_x_lo             ; store updated camera X
        bcc     fast_scroll_set_direction ; no carry → same screen
        inc     camera_screen           ; crossed screen boundary
fast_scroll_set_direction:  lda     #$01 ; $10 = 1 (scroll right)
        sta     $10                     ; direction = right
        jsr     render_scroll_column    ; render column for new position
        lda     camera_x_lo             ; update $25 = previous fine scroll
        sta     $25                     ; save as previous fine scroll
        lda     ent_x_sub               ; player X sub += $D0
        clc                             ; (24-bit add: sub + pixel + screen)
        adc     #$D0                    ; net effect: player slides right
        sta     ent_x_sub               ; update player X sub-pixel
        lda     ent_x_px                ; player X pixel += carry
        adc     #$00                    ; propagate carry
        sta     ent_x_px                ; store updated player X pixel
        lda     ent_x_scr               ; player X screen += carry
        adc     #$00                    ; propagate carry
        sta     ent_x_scr               ; store updated player X screen
        jsr     process_frame_yield_with_player ; render frame + yield to NMI
        lda     camera_x_lo             ; loop until camera_x_lo wraps to 0
        bne     fast_scroll_right_loop  ; loop until full screen scrolled
        lda     stage_id                ; re-select stage bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        jmp     load_room               ; load room layout + CHR/palette

; ===========================================================================
; vertical_scroll_animate — animate vertical room transition
; ===========================================================================
; Scrolls the screen vertically at 3 pixels per frame while moving the
; player Y position at ~2.75 pixels per frame. Uses $FA as vertical
; fine scroll position (0-$EF, wraps at $F0 like ent_y_px).
; $23 bit 2 selects direction: set=down ($04), clear=up ($08).
; Loops until $FA reaches 0 (full screen scrolled).
; ---------------------------------------------------------------------------

vertical_scroll_animate:  jsr     clear_entity_table ; clear all enemies
        lda     $23                     ; X = direction index:
        and     #$04                    ; $04 → X=1 (scrolling down)
        lsr     a                       ; $08 → X=0 (scrolling up)
        lsr     a                       ; convert to 0 or 1
        tax                             ; X = direction index
        lda     metatile_row_render_table,x ; $24 = initial row from table
        sta     $24                     ; set initial row pointer
vert_scroll_dir_check:  lda     $23     ; bit 2 set = scrolling down
        and     #$04                    ; zero → scrolling up
        beq     vert_scroll_up_player_sub ; branch to scroll-up handler

; --- scroll down: camera moves down, player moves up ---
        lda     scroll_y                ; scroll_y += 3 (fine Y scroll advances)
        clc                             ; prepare addition
        adc     #$03                    ; add 3 pixels
        sta     scroll_y                ; store updated fine Y
        cmp     #$F0                    ; wrap at $F0 (NES screen height)
        bcc     vert_scroll_down_player_sub ; no wrap → move player
        adc     #$0F                    ; skip $F0-$FF range (wrap)
        sta     scroll_y                ; store wrapped fine Y
vert_scroll_down_player_sub:  lda     ent_y_sub ; player Y sub -= $C0
        sec                             ; (player moves up ~2.75 px/frame)
        sbc     #$C0                    ; subtract $C0 sub-pixels
        sta     ent_y_sub               ; store updated Y sub-pixel
        lda     ent_y_px                ; player Y pixel -= 2 + borrow
        sbc     #$02                    ; subtract 2 pixels
        sta     ent_y_px                ; store updated Y pixel
        bcs     vert_scroll_render_row  ; no underflow → render
        sbc     #$0F                    ; wrap at screen boundary
        sta     ent_y_px                ; store wrapped Y pixel
        jmp     vert_scroll_render_row  ; render row

; --- scroll up: camera moves up, player moves down ---

vert_scroll_up_player_sub:  lda     scroll_y ; scroll_y -= 3 (fine Y scroll retreats)
        sec                             ; subtract 3 pixels
        sbc     #$03                    ; subtract 3 pixels
        sta     scroll_y                ; store updated fine Y
        bcs     vert_scroll_up_player_add ; no underflow → move player
        sbc     #$0F                    ; wrap below $00
        sta     scroll_y                ; store wrapped fine Y
vert_scroll_up_player_add:  lda     ent_y_sub ; player Y sub += $C0
        clc                             ; (player moves down ~2.75 px/frame)
        adc     #$C0                    ; add $C0 sub-pixels
        sta     ent_y_sub               ; update player Y sub-pixel
        lda     ent_y_px                ; player Y pixel += 2 + carry
        adc     #$02                    ; add 2 pixels + carry
        sta     ent_y_px                ; update player Y pixel
        cmp     #$F0                    ; wrap at $F0
        bcc     vert_scroll_render_row  ; within screen → render row
        adc     #$0F                    ; skip $F0-$FF range (NES wrap)
        sta     ent_y_px                ; store wrapped Y pixel
vert_scroll_render_row:  jsr     render_vert_row ; render row for new vertical position
        lda     scroll_y                ; $26 = previous scroll_y (for delta)
        sta     $26                     ; save previous scroll Y
        lda     $12                     ; preserve $12 across frame yield
        pha                             ; push $12 to stack
        jsr     process_frame_yield_with_player ; render frame + yield to NMI
        pla                             ; restore $12
        sta     $12                     ; direction-dependent flag
        lda     scroll_y                ; loop until scroll_y == 0 (full screen)
        beq     vert_scroll_reselect_bank ; done → restore bank
        jmp     vert_scroll_dir_check   ; continue scrolling

vert_scroll_reselect_bank:  lda     stage_id ; re-select stage bank
        sta     prg_bank                ; set PRG bank to stage
        jmp     select_PRG_banks        ; apply bank switch

; ===========================================================================
; render_vert_row — queue nametable row update during vertical scroll
; ===========================================================================
; Vertical equivalent of render_scroll_column. Computes whether an 8-pixel
; row boundary was crossed (|$FA - $26| + fine position), and if so,
; builds the PPU update buffer for the new row of metatiles.
;
; scroll_y = current vertical fine scroll, $26 = previous scroll_y
; $23 bit 2: direction (set=down, clear=up)
; $24 = nametable row pointer
; ---------------------------------------------------------------------------

render_vert_row:  lda     scroll_y      ; $03 = |scroll_y - $26
        sec                             ; = absolute vertical scroll delta
        sbc     $26                     ; subtract previous fine scroll
        bpl     vert_row_compute_delta  ; positive → no negate needed
        eor     #$FF                    ; negate if negative
        clc                             ; two's complement:
        adc     #$01                    ; A = |$FA - $26|
vert_row_compute_delta:  sta     $03    ; $03 = scroll delta (pixels)
        beq     vert_row_no_row_exit    ; zero → nothing to render
        lda     $23                     ; get fine position for boundary check
        and     #$04                    ; down: use $26 as-is
        beq     vert_row_up_direction   ; up: invert $26
        lda     $26                     ; use $26 for downward scroll
        jmp     vert_row_boundary_check ; skip inversion

vert_row_up_direction:  lda     $26     ; invert for upward scroll
        eor     #$FF                    ; bitwise complement for up direction
vert_row_boundary_check:  and     #$07  ; (fine pos & 7) + delta
        clc                             ; if result / 8 > 0: crossed boundary
        adc     $03                     ; → need to render new row
        lsr     a                       ; divide by 8:
        lsr     a                       ;   result > 0 means crossed tile boundary
        lsr     a                       ;   (8 pixels per tile row)
        bne     vert_row_advance_ptr    ; nonzero → render new row
vert_row_no_row_exit:  rts

; --- render row: advance row pointer and build PPU update buffer ---
; $24 = nametable row index (0-$1D, wraps), $23 bit 2 = direction (1=down, 0=up)
; PPU buffer: $0780 = header, $0783 = top half tiles, $07AF = bottom half tiles
; $07A3-$07A5 = attribute table PPU address + byte count
; $07A6-$07AD = 8 attribute bytes for this row
; $0640 = attribute cache (8 bytes per row block, updated incrementally)

vert_row_advance_ptr:  lda     $23      ; Y = direction flag (0=up, 1=down)
        and     #$04                    ; bit 2 → shift to bit 0
        lsr     a                       ; shift right twice to get 0 or 1
        lsr     a                       ; A = direction index
        tay                             ; Y = direction index
        lda     $24                     ; advance row pointer:
        clc                             ; down: $24 += $01 (from vert_row_advance_tbl)
        adc     vert_row_advance_tbl,y  ; up: $24 += $FF (i.e. $24 -= 1)
        sta     $24                     ; store advanced row pointer
        cmp     #$1E                    ; row < 30? (NES nametable = 30 rows)
        bcc     vert_row_nt_check       ; yes → skip wrap
        lda     vert_row_wrap_tbl,y     ; wrap: down wraps to $00, up wraps to $1D
        sta     $24                     ; store wrapped row pointer
vert_row_nt_check:  lda     $24         ; check if row is on correct nametable half
        and     #$01                    ; (even/odd parity vs direction)
        cmp     vert_row_parity_tbl,y   ; down expects $01, up expects $00
        beq     vert_row_build_fresh    ; match → need to render fresh row
        jmp     vert_row_shift_existing ; mismatch → just shift existing buffer

vert_row_build_fresh:  lda     stage_id ; switch to stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldy     $29                     ; set up metatile column pointer for screen $29
        jsr     metatile_column_ptr     ; set metatile column pointer
        lda     $24                     ; $28 = row-within-block offset × 2
        and     #$1C                    ; (rows come in groups of 4 within metatile)
        asl     a                       ; multiply by 2 for column stride
        sta     $28                     ; store as attribute cache index
        ora     #$C0                    ; PPU addr for attribute table row = $23C0 + offset
        sta     $07A4                   ; store low byte of attr PPU addr
        lda     #$23                    ; high byte = $23 (nametable 0 attr table)
        sta     $07A3                   ; store high byte of attr PPU addr
        lda     #$07                    ; 8 attribute bytes to write
        sta     $07A5                   ; store attribute byte count
        lda     #$00                    ; $03 = PPU buffer write offset (starts at 0)
        sta     $03                     ; initialize buffer offset
vert_row_tile_copy_loop:  ldy     $28   ; $11 = previous attribute byte from cache
        lda     $0640,y                 ; read cached attribute for this column
        sta     $11                     ; save previous attribute byte
        jsr     metatile_to_chr_tiles   ; decode metatile → CHR tiles + attr in $10
        ldy     $03                     ; Y = buffer write offset
        lda     $24                     ; X = row sub-position (0-3 within metatile)
        and     #$03                    ; mask to sub-row (0-3)
        tax                             ; X = sub-row index
        lda     vert_chr_top_offsets,x  ; $04 = CHR buffer offset for top row
        sta     $04                     ; store top-half CHR offset
        lda     vert_chr_bot_offsets,x  ; $05 = CHR buffer offset for bottom row
        sta     $05                     ; store bottom-half CHR offset
        lda     #$03                    ; $06 = 4 tiles per metatile column (count-1)
        sta     $06                     ; initialize tile copy counter
vert_row_chr_buffer_copy:  ldx     $04  ; top half: $06C0[top offset] → $0783 buffer
        lda     $06C0,x                 ; read top-half CHR tile
        sta     $0783,y                 ; store to top-half PPU buffer
        ldx     $05                     ; bottom half: $06C0[bot offset] → $07AF buffer
        lda     $06C0,x                 ; read bottom-half CHR tile
        sta     $07AF,y                 ; store to bottom-half PPU buffer
        inc     $04                     ; advance offsets
        inc     $05                     ; advance bottom offset
        iny                             ; advance buffer write position
        dec     $06                     ; 4 tiles done?
        bpl     vert_row_chr_buffer_copy ; loop until 4 tiles copied
        sty     $03                     ; save buffer position
        lda     $24                     ; X = row sub-position (0-3)
        and     #$03                    ; mask to sub-row (0-3)
        tax                             ; X = sub-row index
        lda     $10                     ; keep new attr bits (mask from vert_attr_keep_tbl)
        and     vert_attr_keep_tbl,x    ; mask new attribute per sub-row
        sta     $10                     ; store masked new attribute
        lda     $11                     ; merge old attr bits (mask from vert_attr_old_tbl)
        and     vert_attr_old_tbl,x     ; mask old attribute per sub-row
        ora     $10                     ; combine old and new attribute bits
        sta     $10                     ; $10 = merged attribute byte
        ldx     $28                     ; update attribute cache
        sta     $0640,x                 ; write merged attr to cache
        txa                             ; X = column index (0-7)
        and     #$07                    ; isolate column within attr row
        tax                             ; X = attribute byte index
        lda     $10                     ; store attribute to PPU buffer
        sta     $07A6,x                 ; write to attr PPU buffer
        inc     $28                     ; next column
        cpx     #$07                    ; done all 8?
        bne     vert_row_tile_copy_loop ; loop until all 8 columns done
        lda     $24                     ; compute nametable PPU address for this row
        pha                             ; save $24 for sub-row calc
        and     #$03                    ; Y = sub-row (0-3)
        tay                             ; Y = sub-row (0-3)
        pla                             ; restore $24
        lsr     a                       ; X = row/4 (metatile row index)
        lsr     a                       ; divide row by 4
        tax                             ; X = metatile row index
        lda     metatile_nt_offset_table,x ; $0780 = PPU addr high byte (from row table)
        sta     $0780                   ; set PPU addr high byte
        lda     metatile_burst_table,x  ; $0781 = PPU addr low byte | nametable offset
        ora     vert_row_nt_offsets,y   ; (sub-row adds $00/$20/$40/$60)
        sta     $0781                   ; store PPU addr low byte
        lda     #$1F                    ; $0782 = tile count ($1F = 32 tiles per row)
        sta     $0782                   ; store tile count
        lda     $23                     ; Y = direction (0=up, 1=down)
        and     #$04                    ; convert direction to index
        lsr     a                       ; shift bit 2 to bit 0
        lsr     a                       ; A = 0 (up) or 1 (down)
        tay                             ; Y = direction index
        ldx     vert_term_offset_tbl,y  ; terminator offset from vert_term_offset_tbl
        lda     #$FF                    ; $FF = end-of-buffer marker
        sta     $0780,x                 ; place at appropriate end
        sta     nametable_dirty         ; flag PPU update pending
        rts                             ; return to caller

vert_row_shift_existing:  ldy     #$1F  ; copy 32 bytes: $07AF → $0783
vert_row_copy_bottom:  lda     $07AF,y ; read bottom-half tile
        sta     $0783,y                 ; copy bottom-half → top-half buffer
        dey                             ; next byte (descending)
        bpl     vert_row_copy_bottom    ; loop all 32 bytes
        lda     $24                     ; update PPU addr low byte
        and     #$03                    ; with new sub-row nametable offset
        tax                             ; X = sub-row index
        lda     $0781                   ; keep high bit (nametable select)
        and     #$80                    ; preserve nametable select bit
        ora     vert_row_nt_offsets,x   ; merge sub-row offset ($00/$20/$40/$60)
        sta     $0781                   ; store updated PPU addr low byte
        lda     #$23                    ; attribute table high byte
        sta     $07A3                   ; store attr PPU addr high byte
        lda     $23                     ; set terminator based on direction
        and     #$04                    ; isolate direction bit
        lsr     a                       ; shift bit 2 to bit 0
        lsr     a                       ; A = 0 (up) or 1 (down)
        tay                             ; Y = direction index
        ldx     vert_term_shift_tbl,y   ; terminator offset (shift variant)
        lda     #$FF                    ; $FF = end-of-buffer marker
        sta     $0780,x                 ; write end marker
        sta     nametable_dirty         ; PPU update pending
        rts                             ; return to caller

; --- vertical row rendering lookup tables ---
; attribute mask tables: which bits to keep from new vs old attribute

vert_attr_keep_tbl:  .byte   $0F,$0F,$F0,$F0 ; new attr mask per sub-row (0-3)
vert_attr_old_tbl:  .byte   $F0,$F0,$0F,$0F ; old attr mask per sub-row (0-3)

; CHR buffer offsets: top-half and bottom-half starting positions
vert_chr_top_offsets:  .byte   $00,$04,$08,$0C ; top-half CHR offset per sub-row
vert_chr_bot_offsets:  .byte   $04,$00,$0C,$08 ; bottom-half CHR offset per sub-row

; nametable row offsets: $00, $20, $40, $60 per sub-row
vert_row_nt_offsets:  .byte   $00,$20,$40,$60

; row parity check: down expects $01, up expects $00
vert_row_parity_tbl:  .byte   $01,$00

; row wrap values: down wraps to $00, up wraps to $1D
vert_row_wrap_tbl:  .byte   $1D,$00

; row advance: down +1 ($01), up -1 ($FF)
vert_row_advance_tbl:  .byte   $FF,$01

; unknown small tables (used by boundary check / buffer termination)
metatile_row_render_table:  .byte   $00,$1D
vert_term_offset_tbl:  .byte   $23,$2E  ; terminator offset: up=$23, down=$2E
vert_term_shift_tbl:  .byte   $2E,$23

; ===========================================================================
; metatile_to_chr_tiles — convert 4 metatile quadrants to CHR tile IDs
; ===========================================================================
; Reads the current metatile definition via pointer at $00/$01 (set by
; metatile_chr_ptr). Each metatile has 4 quadrants (indexed 0-3).
; For each quadrant:
;   - Reads metatile sub-index from ($00),y
;   - Looks up 4 CHR tile IDs from tables $BB00-$BE00
;   - Stores them to $06C0 buffer in 2×2 layout
;   - Reads palette/attribute bits from $BF00
;   - Accumulates attribute byte in $10 (2 bits per quadrant)
;
; $E89D offset table: {$00,$02,$08,$0A} maps quadrants to $06C0 offsets.
; Output: $06C0 = 4×4 = 16 CHR tiles, $10 = attribute byte.
; ---------------------------------------------------------------------------
metatile_to_chr_tiles:  jsr     metatile_chr_ptr ; set $00/$01 pointer to metatile CHR data
metatile_to_chr_tiles_continue:  ldy     #$03 ; start with quadrant 3
        sty     $02                     ; $02 = quadrant counter (3→0)
        lda     #$00                    ; clear attribute accumulator
        sta     $10                     ; $10 = accumulated attribute bits
metatile_quadrant_check:  ldy     $02   ; Y = current quadrant
        ldx     metatile_attr_offset_table,y ; X = buffer offset for this quadrant
        lda     (temp_00),y             ; read metatile sub-index
        tay                             ; Y = sub-index for CHR lookup
        lda     $BB00,y                 ; top-left tile
        sta     $06C0,x                 ; store to tile buffer
        lda     $BC00,y                 ; top-right tile
        sta     $06C1,x                 ; store to tile buffer
        lda     $BD00,y                 ; bottom-left tile
        sta     $06C4,x                 ; store to buffer (+4 = next row)
        lda     $BE00,y                 ; bottom-right tile
        sta     $06C5,x                 ; store to buffer (+5)
        jsr     breakable_block_override ; Gemini Man: zero destroyed blocks
        lda     $BF00,y                 ; attribute: low 2 bits = palette
        and     #$03                    ; merge into $10
        ora     $10                     ; combine with existing attribute bits
        sta     $10                     ; accumulate attribute bits
        dec     $02                     ; next quadrant
        bmi     metatile_chr_exit       ; if all 4 done, return
        asl     $10                     ; shift attribute bits left 2
        asl     $10                     ; (make room for next quadrant)
        jmp     metatile_quadrant_check ; process next quadrant

metatile_chr_exit:  rts                 ; return with tiles in $06C0, attr in $10

; ===========================================================================
; breakable_block_override — zero CHR tiles for destroyed blocks (Gemini Man)
; ===========================================================================
; Called from metatile_to_chr_tiles for Gemini Man stages only ($22 = $02 or
; $09). Checks if the current metatile is a breakable block type (attribute
; upper nibble = $7x). If so, looks up the destroyed-block bitfield at
; $0110 — if the bit is set, the block has been destroyed and its 4 CHR
; tiles are zeroed out (invisible).
;
; Y = metatile sub-index, X = $06C0 buffer offset (preserved on exit)
; Uses bitmask_table ($EB82) for bit testing: $80,$40,$20,...,$01
; ---------------------------------------------------------------------------

breakable_block_override:  lda     stage_id ; current stage number
        cmp     #STAGE_GEMINI           ; Gemini Man?
        beq     metatile_attr_upper_nibble ; yes → check
        cmp     #STAGE_DOC_GEMINI       ; Doc Robot (Gemini Man stage)?
        bne     metatile_chr_exit       ; no → skip (shared RTS above)
metatile_attr_upper_nibble:  lda     $BF00,y ; attribute byte upper nibble
        and     #$F0                    ; isolate upper nibble
        cmp     #$70                    ; breakable block type ($7x)?
        bne     metatile_chr_exit       ; no → skip
        sty     $0F                     ; save Y (metatile sub-index)
        stx     $0E                     ; save X (buffer offset)
        lda     $29                     ; compute destroyed-block array index:
        and     #$01                    ; Y = ($29 & 1) << 5 | ($28 >> 1)
        asl     a                       ; ($29 bit 0 = screen page,
        asl     a                       ; $28 = metatile column position)
        asl     a                       ; shift left 5 total
        asl     a                       ; to form high bits of byte index
        asl     a                       ; A = ($29 & 1) << 5
        sta     $0D                     ; store partial byte index
        lda     $28                     ; load metatile column
        pha                             ; save $28
        lsr     a                       ; Y = byte index into $0110 array
        ora     $0D                     ; merge screen page and column bits
        tay                             ; Y = byte index in destroyed array
        pla                             ; X = bit index within byte:
        asl     a                       ; ($28 << 2) & $04 | $02 (quadrant)
        asl     a                       ; combines column parity with quadrant
        and     #$04                    ; isolate column parity bit
        ora     $02                     ; merge with quadrant counter
        tax                             ; X = bit select index
        lda     $0110,y                 ; test destroyed bit
        and     bitmask_table,x         ; via bitmask_table
        beq     metatile_restore_y_reg  ; not destroyed → skip
        ldx     $0E                     ; restore buffer offset
        lda     #$00                    ; zero all 4 CHR tiles (invisible)
        sta     $06C0,x                 ; clear top-left tile
        sta     $06C1,x                 ; clear top-right tile
        sta     $06C4,x                 ; clear bottom-left tile
        sta     $06C5,x                 ; clear bottom-right tile
metatile_restore_y_reg:  ldy     $0F    ; restore Y
        ldx     $0E                     ; restore X
        rts                             ; return to metatile_to_chr_tiles

; metatile_chr_ptr: sets $00/$01 pointer to 4-byte metatile CHR definition
; reads metatile index from column data at ($20),y
; each metatile = 4 CHR tile indices (2x2 pattern: TL, TR, BL, BR)
; metatile definitions at $B700 + (metatile_index * 4) in stage bank

metatile_chr_ptr:  jsr     ensure_stage_bank ; ensure stage bank selected
        lda     #$00                    ; initialize pointer high byte
        sta     $01                     ; $01 = 0 (high byte of pointer)
        ldy     $28                     ; Y = metatile row
        lda     ($20),y                 ; metatile index from column data
calc_chr_offset:  asl     a             ; multiply by 4
        rol     $01                     ; (4 CHR tiles per metatile)
        asl     a                       ; shift 2
        rol     $01                     ; rotate high byte
        sta     temp_00                 ; $00/$01 = $B700 + (index * 4)
        lda     $01                     ; pointer to CHR tile definition
        clc                             ; add base address $B700
        adc     #$B7                    ; high byte of CHR definition table
        sta     $01                     ; store pointer high byte
        rts                             ; $00/$01 → metatile CHR definition

metatile_attr_offset_table:  .byte   $00,$02,$08,$0A
metatile_burst_table:  .byte   $00,$80,$00,$80,$00,$80,$00,$80
metatile_nt_offset_table:  .byte   $20,$20,$21,$21,$22,$22,$23,$23

; metatile_column_ptr: sets $20/$21 pointer to metatile column data
; Y = screen page → reads column ID from $AA00,y in stage bank
; column data is at $AF00 + (column_ID * 64) — 64 bytes per column
metatile_column_ptr:  lda     $AA00,y   ; column ID from screen data
metatile_column_ptr_by_id:  pha         ; multiply column ID by 64:
        lda     #$00                    ; A << 6 → $00:A (16-bit result)
        sta     temp_00                 ; 6 shifts with 16-bit rotate
        pla                             ; restore column ID
        asl     a                       ; shift 1
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 2
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 3
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 4
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 5
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 6
        rol     temp_00                 ; 16-bit rotate
        sta     $20                     ; $20/$21 = $AF00 + (column_ID × 64)
        lda     temp_00                 ; pointer to metatile column data
        clc                             ; in stage bank at $AF00+
        adc     #$AF                    ; add base address high byte
        sta     $21                     ; store pointer high byte
        rts                             ; $20/$21 → metatile column data

; -----------------------------------------------
; check_tile_horiz — horizontal-scan tile collision
; -----------------------------------------------
; Checks multiple X positions at a fixed Y offset for collisions.
; Used for floor/ceiling detection (scanning across the entity's
; width at a specific vertical position).
;
; parameters:
;   Y = hitbox config index into $EBE2 offset table
;   X = entity slot
; hitbox offset tables:
;   $EBE2,y = starting offset index (into $EC12)
;   $EC10,y = number of check points - 1
;   $EC11,y = Y pixel offset (signed, relative to entity Y)
;   $EC12,y = X pixel offsets (signed, relative to entity X)
; results:
;   $42-$44 = tile type at each check point
;   tile_at_feet_max = max (highest priority) tile type encountered
;   $10     = OR of all tile types (AND #$10 tests solid)
; -----------------------------------------------

check_tile_horiz:  lda     metatile_gemini_scr1_starts,y ; $40 = starting offset index
        sta     $40                     ; store starting offset
        jsr     tile_check_init         ; clear accumulators, switch to stage bank
        tay                             ; Y = hitbox config index
        lda     metatile_gemini_scr2_count,y ; $02 = number of check points - 1
        sta     $02                     ; store check point count
        lda     ent_y_scr,x             ; $03 = entity Y screen
        sta     $03                     ; store entity Y screen
        lda     metatile_gemini_scr2_offset,y ; $11 = entity_Y + Y_offset
        pha                             ; (compute check Y position)
        clc                             ; add Y offset to entity Y pixel
        adc     ent_y_px,x              ; compute check Y position
        sta     $11                     ; store check Y position
        pla                             ; A = original Y offset (for sign check)
        bmi     metatile_negative_offset ; negative offset?
        bcs     metatile_clear_results  ; positive + carry = past screen bottom
        lda     $11                     ; check if Y wrapped past $F0
        cmp     #$F0                    ; (screen height)
        bcs     metatile_clear_results  ; wrapped past screen → out of bounds
        bcc     metatile_y_screen_check ; in bounds → continue check
metatile_negative_offset:  bcs     metatile_y_screen_check ; neg + carry = no underflow → ok
metatile_clear_results:  lda     #$00   ; Y out of bounds: zero all results
        ldy     $02                     ; Y = check point count
metatile_clear_loop:  sta     $42,y     ; clear $42..$42+count
        dey                             ; next result slot
        bpl     metatile_clear_loop     ; loop until all cleared
        jmp     tile_check_cleanup      ; restore bank and return

metatile_y_screen_check:  lda     $03   ; Y screen != 0? treat as offscreen
        bne     metatile_clear_results  ; (only check screen 0)
        lda     $11                     ; Y >> 2 = 4-pixel rows
        lsr     a                       ; divide Y by 4
        lsr     a                       ; to get 4-pixel row index
        pha                             ; $28 = metatile row (bits 5-3 of Y>>2)
        and     #$38                    ; = column offset in metatile grid
        sta     $28                     ; store metatile row
        pla                             ; $03 bit1 = sub-tile Y (bit 2 of Y>>2)
        lsr     a                       ; selects top/bottom half of metatile
        and     #$02                    ; isolate sub-tile Y bit
        sta     $03                     ; store sub-tile Y select
        lda     #$00                    ; $04 = sign extension for X offset
        sta     $04                     ; clear sign extension
        lda     metatile_gemini_scr2_widths,y ; first X offset (signed)
        bpl     metatile_compute_x_pos  ; positive → skip sign extension
        dec     $04                     ; negative: $04 = $FF
metatile_compute_x_pos:  clc            ; $12/$13 = entity_X + X_offset
        adc     ent_x_px,x              ; $12 = X pixel
        sta     $12                     ; store X pixel position
        lda     ent_x_scr,x             ; entity X screen
        adc     $04                     ; add sign extension
        sta     $13                     ; store X screen
        lda     $12                     ; X >> 4 = tile column
        lsr     a                       ; divide X by 16
        lsr     a                       ; to get tile column index
        lsr     a                       ; (16 pixels per tile)
        lsr     a                       ; A = X / 16
        pha                             ; $03 bit0 = sub-tile X (bit 0 of X>>4)
        and     #$01                    ; selects left/right half of metatile
        ora     $03                     ; (combined with Y sub-tile in bits 0-1)
        sta     $03                     ; store combined sub-tile index
        pla                             ; $28 |= metatile column (X>>5)
        lsr     a                       ; merged with row from Y
        ora     $28                     ; combine column with row
        sta     $28                     ; store metatile grid index
metatile_save_slot:  stx     $04        ; save entity slot
        lda     stage_id                ; switch to stage PRG bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $04                     ; restore entity slot
        ldy     $13                     ; get metatile column pointer for X screen
        jsr     metatile_column_ptr     ; set $20/$21 to column data
metatile_get_chr_ptr:  jsr     metatile_chr_ptr ; get CHR data pointer for column+row ($28)
metatile_read_index:  ldy     $03       ; read metatile index at sub-tile ($03)
        lda     (temp_00),y             ; $03 = {0,1,2,3} for TL/TR/BL/BR
        tay                             ; Y = sub-index for attribute lookup
        lda     $BF00,y                 ; get collision attribute for this tile
        and     #$F0                    ; upper nibble = collision type
        jsr     breakable_block_collision ; override if destroyed breakable block
        jsr     proto_man_wall_override ; Proto Man wall override
        ldy     $02                     ; store result in $42+count
        sta     $42,y                   ; store tile type for this check point
        cmp     tile_at_feet_max        ; update max tile type
        bcc     metatile_accumulate_tiles ; less than max → skip update
        sta     tile_at_feet_max        ; update max tile type
metatile_accumulate_tiles:  ora     $10 ; accumulate all tile types
        sta     $10                     ; store combined tile type flags
        dec     $02                     ; all check points done?
        bmi     metatile_player_damage_check ; all done → check for hazard damage
        inc     $40                     ; next offset index
        ldy     $40                     ; Y = next offset table index
        lda     $12                     ; save old bit4 of X (16px tile boundary)
        pha                             ; save old X position
        and     #$10                    ; isolate bit4 (16px tile boundary)
        sta     $04                     ; save as comparison reference
        pla                             ; $12 += next X offset
        clc                             ; add next X offset from table
        adc     metatile_gemini_scr2_widths,y ; advance X to next check point
        sta     $12                     ; update X position
        and     #$10                    ; did bit4 change? (crossed 16px tile?)
        cmp     $04                     ; compare with previous bit4
        beq     metatile_read_index     ; same tile → re-read collision
        lda     $03                     ; toggle sub-tile X (bit 0)
        eor     #$01                    ; flip left/right within metatile
        sta     $03                     ; store updated sub-tile index
        and     #$01                    ; if now odd → just toggled into right half
        bne     metatile_read_index     ; same metatile, re-read sub-tile
        inc     $28                     ; advance metatile column index
        lda     $28                     ; check if crossed column group boundary
        and     #$07                    ; crossed column group? (8 columns)
        bne     metatile_get_chr_ptr    ; no → same screen, new column
        inc     $13                     ; next X screen
        dec     $28                     ; wrap column index back
        lda     $28                     ; keep row bits, clear column
        and     #$38                    ; preserve metatile row
        sta     $28                     ; store row-only grid index
        jmp     metatile_save_slot      ; re-load metatile data for new screen

metatile_player_damage_check:  cpx     #$00 ; only check damage for player (slot 0)
        bne     metatile_cleanup_return ; not player → skip hazard check
        lda     invincibility_timer     ; skip if invincibility active
        bne     metatile_cleanup_return ; immune → skip hazard check
        lda     hazard_pending          ; skip if damage already pending
        bne     metatile_cleanup_return ; damage pending → skip
        lda     player_state            ; skip if player in damage state ($06)
        cmp     #PSTATE_DAMAGE          ; already in damage state?
        beq     metatile_cleanup_return ; yes → skip
        cmp     #PSTATE_DEATH           ; skip if player in death state ($0E)
        beq     metatile_cleanup_return ; yes → skip
        ldy     #$06                    ; Y = damage state
        lda     tile_at_feet_max        ; $30 = damage tile?
        cmp     #TILE_DAMAGE            ; check for damage tile type
        beq     metatile_set_pending_damage ; damage tile → set hazard pending
        ldy     #$0E                    ; Y = death state
        cmp     #TILE_SPIKES            ; $50 = spike tile?
        bne     metatile_cleanup_return ; not spikes → no hazard
metatile_set_pending_damage:  sty     hazard_pending ; set pending damage/death transition
metatile_cleanup_return:  jmp     tile_check_cleanup ; restore bank and return

; -----------------------------------------------
; check_tile_collision — vertical-scan tile collision
; -----------------------------------------------
; Checks multiple Y positions at a fixed X offset for collisions.
; Used for wall detection (scanning down the entity's height at a
; specific horizontal position).
;
; parameters:
;   Y = hitbox config index into $ECE1 offset table
;   X = entity slot
; hitbox offset tables:
;   $ECE1,y = starting offset index (into $ED09)
;   $ED07,y = number of check points - 1
;   $ED08,y = X pixel offset (signed, relative to entity X)
;   $ED09,y = Y pixel offsets (signed, relative to entity Y)
; results:
;   $42-$44 = tile type at each check point
;   tile_at_feet_max = max (highest priority) tile type encountered
;   $10     = OR of all tile types (AND #$10 tests solid)
; -----------------------------------------------

check_tile_collision:  lda     metatile_gemini_scr3_starts,y ; $40 = starting offset index
        sta     $40                     ; store starting offset
        jsr     tile_check_init         ; clear accumulators, switch to stage bank
        tay                             ; Y = hitbox config index
        lda     metatile_gemini_scr4_count,y ; $02 = number of check points - 1
        sta     $02                     ; store check point count
        lda     #$00                    ; $04 = sign extension for X offset
        sta     $04                     ; $04 = 0 (sign extension)
        lda     metatile_gemini_scr4_offset,y ; X offset (signed)
        bpl     metatile_offset_compute ; positive → skip sign extension
        dec     $04                     ; negative: $04 = $FF
metatile_offset_compute:  clc           ; $12/$13 = entity_X + X_offset
        adc     ent_x_px,x              ; $12 = X pixel
        sta     $12                     ; $13 = X screen
        lda     ent_x_scr,x             ; entity X screen
        adc     $04                     ; add sign extension
        sta     $13                     ; store X screen
        lda     $12                     ; X >> 4 = tile column
        lsr     a                       ; divide X by 16
        lsr     a                       ; (continued)
        lsr     a                       ; (continued)
        lsr     a                       ; A = X / 16
        pha                             ; $03 bit0 = sub-tile X (left/right)
        and     #$01                    ; isolate sub-tile X bit
        sta     $03                     ; store sub-tile X select
        pla                             ; $28 = metatile column (X>>5)
        lsr     a                       ; divide by 2 for column
        sta     $28                     ; store metatile column
        lda     ent_y_scr,x             ; Y screen
        bmi     metatile_clamp_top_bound ; negative Y screen → clamp to 0
        bne     metatile_clamp_bottom_bound ; Y screen > 0 → clamp to $EF
        lda     ent_y_px,x              ; $11 = entity_Y + first Y offset
        clc                             ; prepare for addition
        adc     metatile_gemini_scr4_widths,y ; add first Y offset
        sta     $11                     ; store check Y position
        lda     metatile_gemini_scr4_widths,y ; check offset sign
        bpl     metatile_offset_overflow ; positive offset → check overflow
        bcc     metatile_clamp_top_bound ; negative + no carry = underflow
        bcs     metatile_screen_bounds  ; negative + carry = ok, check bounds
metatile_offset_overflow:  bcs     metatile_clamp_bottom_bound ; positive + carry = overflow
metatile_screen_bounds:  lda     $11    ; within screen ($00-$EF)?
        cmp     #$F0                    ; check screen boundary
        bcc     metatile_row_shift      ; < $F0 → within screen bounds
        bcs     metatile_offset_overflow ; wrapped past $F0 → below screen
metatile_clamp_bottom_bound:  lda     #$EF ; clamp to bottom of screen
        sta     $11                     ; store clamped Y
        bne     metatile_row_shift      ; always branches (nonzero)
metatile_clamp_top_bound:  lda     #$00 ; clamp to top of screen
        sta     $11                     ; store clamped Y
        beq     metatile_row_shift      ; always branches (zero)
metatile_advance_y_offset:  lda     $11 ; $11 += next Y offset
        clc                             ; add next Y offset from table
        adc     metatile_gemini_scr4_widths,y ; add next Y offset from table
        sta     $11                     ; store updated Y position
        cmp     #$F0                    ; crossed screen boundary?
        bcc     metatile_offset_index_adv ; within screen → continue check
        adc     #$10                    ; wrap into next screen
        sta     $11                     ; store wrapped Y pixel
        inc     $04                     ; Y screen++
        beq     metatile_row_shift      ; reached screen 0 → start checking
metatile_offset_index_adv:  iny         ; advance offset index
        sty     $40                     ; save offset index
        ldy     $02                     ; zero this check point's result
        lda     #$00                    ; clear check point result
        sta     $42,y                   ; zero this check point
        dec     $02                     ; more points? continue skipping
        bpl     metatile_advance_y_offset ; loop through offscreen points
        jmp     tile_check_cleanup      ; all offscreen → done

metatile_row_shift:  lda     $11        ; Y >> 2 = 4-pixel rows
        lsr     a                       ; divide Y by 4
        lsr     a                       ; (continued)
        pha                             ; $28 |= metatile row (bits 5-3)
        and     #$38                    ; merged with column from X
        ora     $28                     ; combine row and column
        sta     $28                     ; store metatile grid index
        pla                             ; $03 bit1 = sub-tile Y (top/bottom)
        lsr     a                       ; isolate sub-tile Y
        and     #$02                    ; isolate sub-tile Y bit
        ora     $03                     ; combine sub-tile X and Y
        sta     $03                     ; store combined sub-tile index
        stx     $04                     ; switch to stage PRG bank
        lda     stage_id                ; switch to stage PRG bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $04                     ; restore entity slot
        ldy     $13                     ; get metatile column pointer for X screen
        jsr     metatile_column_ptr     ; set column data pointer
metatile_get_row_chr_ptr:  jsr     metatile_chr_ptr ; get CHR data pointer for column+row ($28)
metatile_read_row_index:  ldy     $03   ; read metatile index at sub-tile ($03)
        lda     (temp_00),y             ; read metatile sub-index
        tay                             ; Y = sub-index for attribute lookup
        lda     $BF00,y                 ; get collision attribute
        and     #$F0                    ; upper nibble = collision type
        jsr     breakable_block_collision ; override if destroyed breakable block
        jsr     proto_man_wall_override ; Proto Man wall override
        ldy     $02                     ; store result in $42+count
        sta     $42,y                   ; store tile type for point
        cmp     tile_at_feet_max        ; update max tile type
        bcc     metatile_accum_row_tiles ; less than max → skip update
        sta     tile_at_feet_max        ; $03 bit0 = sub-tile X (left/right)
metatile_accum_row_tiles:  ora     $10  ; accumulate all tile types
        sta     $10                     ; store combined tile flags
        dec     $02                     ; all check points done?
        bmi     metatile_player_dmg_row ; all done → check hazard damage
        inc     $40                     ; next offset index
        ldy     $40                     ; Y = next offset index
        lda     $11                     ; save old bit4 of Y (16px tile boundary)
        pha                             ; save old Y position
        and     #$10                    ; isolate bit4 (16px tile boundary)
        sta     $04                     ; save as comparison reference
        pla                             ; $11 += next Y offset
        clc                             ; prepare for addition
        adc     metatile_gemini_scr4_widths,y ; check offset sign
        sta     $11                     ; store updated Y position
        and     #$10                    ; did bit4 change? (crossed 16px tile?)
        cmp     $04                     ; same 16px tile?
        beq     metatile_read_row_index ; yes → same tile, re-read collision
        lda     $03                     ; toggle sub-tile Y (bit 1)
        eor     #$02                    ; flip top/bottom within metatile
        sta     $03                     ; store updated sub-tile index
        and     #$02                    ; if now set → toggled into bottom half
        bne     metatile_read_row_index ; same metatile, re-read sub-tile
        lda     $28                     ; advance metatile row ($28 += $08)
        pha                             ; save old metatile row
        clc                             ; advance to next metatile row
        adc     #$08                    ; next metatile row (+8)
        sta     $28                     ; store advanced row
        cmp     #$40                    ; past screen bottom? ($40 = 8 rows)
        pla                             ; restore old row
        bcc     metatile_get_row_chr_ptr ; within screen → get new CHR pointer
        sta     $28                     ; restore row, check last sub-tile
        jmp     metatile_read_row_index ; (clamp — don't scroll to next screen)

metatile_player_dmg_row:  cpx     #$00  ; only check damage for player (slot 0)
        bne     metatile_row_cleanup_return ; not player → skip hazard check
        lda     invincibility_timer     ; skip if invincibility active
        bne     metatile_row_cleanup_return ; immune → skip hazard check
        lda     hazard_pending          ; skip if damage already pending
        bne     metatile_row_cleanup_return ; damage pending → skip
        lda     player_state            ; skip if player in damage state ($06)
        cmp     #PSTATE_DAMAGE          ; already in damage state?
        beq     metatile_row_cleanup_return ; yes → skip
        cmp     #PSTATE_DEATH           ; skip if player in death state ($0E)
        beq     metatile_row_cleanup_return ; already in death state → skip
        lda     tile_at_feet_max        ; $50 = spike tile → instant kill
        cmp     #TILE_SPIKES            ; check for spike tile
        bne     metatile_row_cleanup_return ; not spikes → no hazard
        lda     #PSTATE_DEATH           ; set pending death transition
        sta     hazard_pending          ; store pending death transition
metatile_row_cleanup_return:  jmp     tile_check_cleanup ; restore bank and return

; -----------------------------------------------
; tile_check_init — initialize tile collision check
; -----------------------------------------------
; Clears result accumulators ($10, $41), saves the current PRG bank
; to $2F, and switches to the stage PRG bank ($22) so the tile
; collision code can read metatile/attribute data from $A000-$BFFF.
; Preserves A and X across the bank switch.
; -----------------------------------------------

tile_check_init:  pha                   ; save A (table index)
        txa                             ; save X (entity slot)
        pha                             ; save X (entity slot)
        lda     #$00                    ; clear collision accumulators
        sta     $10                     ; $10 = OR of all tile types
        sta     tile_at_feet_max        ; max tile type = 0
        lda     prg_bank                ; save current PRG bank to $2F
        sta     $2F                     ; save current PRG bank
        lda     stage_id                ; switch to stage PRG bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        pla                             ; restore X and A
        tax                             ; restore X
        pla                             ; restore A
        rts                             ; return (A = table index)

; -----------------------------------------------
; tile_check_cleanup — restore PRG bank after tile collision
; -----------------------------------------------
; Restores the PRG bank saved in $2F by tile_check_init.
; Preserves X.
; -----------------------------------------------

tile_check_cleanup:  txa                ; save X
        pha                             ; save X on stack
        lda     $2F                     ; restore saved PRG bank
        sta     prg_bank                ; write back saved bank
        jsr     select_PRG_banks        ; apply bank switch
        pla                             ; restore X
        tax                             ; restore X
        rts                             ; return

; ===========================================================================
; breakable_block_collision — override tile type for destroyed blocks
; ===========================================================================
; Called during collision detection. If tile type is $70 (breakable block)
; and we're on Gemini Man stages ($22 = $02 or $09), checks the $0110
; destroyed-block bitfield. If the block is destroyed, overrides tile type
; to $00 (passthrough) so the player can walk through it.
;
; A = tile type on entry, X = caller context (preserved)
; Returns: A = tile type ($70 if intact, $00 if destroyed), X preserved
; ---------------------------------------------------------------------------

breakable_block_collision:  sta     $06 ; save tile type
        stx     $05                     ; save X
        cmp     #$70                    ; only handle $70 (breakable block)
        bne     metatile_return_tile_type ; not breakable → return as-is
        lda     stage_id                ; only on Gemini Man stages
        cmp     #STAGE_GEMINI           ; ($02 = Robot Master,
        beq     metatile_compute_array_index ; yes → check destroyed state
        cmp     #STAGE_DOC_GEMINI       ; Doc Robot Gemini stage?
        bne     metatile_return_tile_type ; not Gemini → return as-is
metatile_compute_array_index:  lda     $13 ; compute array index (same as
        and     #$01                    ; breakable_block_override):
        asl     a                       ; Y = ($13 & 1) << 5 | ($28 >> 1)
        asl     a                       ; ($13 = screen page for collision,
        asl     a                       ; $28 = metatile column position)
        asl     a                       ; shift left 5 total
        asl     a                       ; A = ($13 & 1) << 5
        sta     $07                     ; store partial index
        lda     $28                     ; load metatile column
        pha                             ; save $28
        lsr     a                       ; Y = byte index into $0110 array
        ora     $07                     ; merge screen page and column bits
        tay                             ; Y = byte index in destroyed array
        pla                             ; X = bit index within byte:
        asl     a                       ; ($28 << 2) & $04 | $03 (quadrant)
        asl     a                       ; shift column left 2
        and     #$04                    ; isolate column parity bit
        ora     $03                     ; merge with sub-tile quadrant
        tax                             ; X = bit select index
        lda     $0110,y                 ; test destroyed bit
        and     bitmask_table,x         ; mask with bitmask for this block
        beq     metatile_return_tile_type ; not destroyed → keep original type
        lda     #$00                    ; destroyed → override to passthrough
        sta     $06                     ; store $00 as tile type
metatile_return_tile_type:  lda     $06 ; return tile type ($00 if block destroyed)
        ldx     $05                     ; restore X
        rts                             ; return tile type in A

; ---------------------------------------------------------------------------
; clear_destroyed_blocks — reset the $0110 destroyed-block bitfield
; ---------------------------------------------------------------------------
; Called on stage init. Zeros all 64 bytes ($0110-$014F) on Gemini Man
; stages ($22 = $02 or $09), making all breakable blocks solid again.
; ---------------------------------------------------------------------------

clear_destroyed_blocks:  lda     stage_id ; only on Gemini Man stages
        cmp     #STAGE_GEMINI           ; Gemini Man stage?
        beq     metatile_zero_destroyed ; yes → clear bitfield
        cmp     #STAGE_DOC_GEMINI       ; Doc Robot Gemini stage?
        bne     metatile_zero_done      ; no → skip
metatile_zero_destroyed:  lda     #$00  ; zero $0110..$014F (64 bytes)
        ldy     #$3F                    ; = 64 block slots
        sta     $0110,y                 ; clear destroyed-block byte
        .byte   $88,$10,$FA
metatile_zero_done:  .byte   $60
bitmask_table:  .byte   $80,$40,$20,$10
        php                             ; data: bitmask $08
        .byte   $04
        .byte   $02
        .byte   $01

; ---------------------------------------------------------------------------
; proto_man_wall_override — open breakable walls after Proto Man cutscene
; ---------------------------------------------------------------------------
; When $68 (cutscene-complete flag) is set, checks if current tile position
; matches a Proto Man breakable wall for this stage. If so, returns A=$00
; (passthrough) instead of the original tile type, opening the path.
; Table at $EBC6: per-stage index into wall position data at $EBCE+.
;   $EBC6,y = data offset for stage $22 ($FF = no wall on this stage)
;   $EBCE,x = scroll position ($F9) to match
;   $EBCF,x = number of wall tile entries
;   $EBD0+  = tile column positions to match against $28
; ---------------------------------------------------------------------------
proto_man_wall_override:  sta     $05   ; save original tile type
        stx     $06                     ; save X
        sty     $07                     ; save Y
        lda     proto_man_flag          ; cutscene-complete flag
        beq     metatile_gemini_original ; no cutscene → return original
        ldy     stage_id                ; stage index
        ldx     metatile_gemini_table_1,y ; look up wall data offset
        bmi     metatile_gemini_wrong_stage ; $FF = no wall → clear $68
        lda     metatile_gemini_table_2,x ; expected scroll position
        cmp     camera_screen           ; match current scroll?
        bne     metatile_gemini_wrong_stage ; no → clear $68 (wrong screen)
        lda     metatile_gemini_col_offsets,x ; number of wall tile entries
        sta     $08                     ; store wall entry count
        lda     $28                     ; current tile column
metatile_gemini_wall_match:  cmp     metatile_gemini_wall_cols,x ; match wall column?
        beq     metatile_gemini_passthrough ; yes → return $00 (passthrough)
        inx                             ; next wall entry
        dec     $08                     ; more entries to check?
        bpl     metatile_gemini_wall_match ; loop through all wall entries
        bmi     metatile_gemini_original ; no match → return original tile type
metatile_gemini_passthrough:  lda     #$00 ; A = $00 (air/passthrough)
        beq     metatile_gemini_restore_xy ; always branches
metatile_gemini_original:  lda     $05  ; A = original tile type
metatile_gemini_restore_xy:  ldx     $06 ; restore X, Y
        ldy     $07                     ; restore Y
        rts                             ; return tile type in A

metatile_gemini_wrong_stage:  lda     #$00 ; wrong stage/screen
        sta     proto_man_flag          ; clear cutscene-complete flag
        beq     metatile_gemini_original ; always branches (A=0)
metatile_gemini_table_1:  .byte   $FF,$00,$11,$04,$FF,$FF,$FF,$0E ; ($28 << 2) & $04 | $03 (quadrant)
metatile_gemini_table_2:  .byte   $05
metatile_gemini_col_offsets:  .byte   $01
metatile_gemini_wall_cols:  .byte   $31,$39,$13,$07,$23,$24,$2B,$2C
        .byte   $33,$34,$3B,$3C,$05,$00,$31,$09
        .byte   $00,$00
metatile_gemini_scr1_starts:  .byte   $00,$05,$09,$0E,$12,$16,$1A,$1F
        .byte   $24,$28,$2C,$31,$36,$3B,$40,$45
        .byte   $49,$4E,$53,$57,$5B,$5F,$63,$68
        .byte   $6C,$70,$75,$79,$7D,$81,$85,$8A
        .byte   $8F,$94,$99,$9E,$A3,$A8,$AD,$B2
        .byte   $B7,$BC,$C1,$C6,$CB,$CE
metatile_gemini_scr2_count:  .byte   $02
metatile_gemini_scr2_offset:  .byte   $0C
metatile_gemini_scr2_widths:  .byte   $F9,$07,$07,$01,$F4,$F9,$0E,$02
        .byte   $0A,$F1,$0F,$0F,$01,$00,$F9,$0E
        .byte   $01,$00,$08,$0E,$01,$00,$EA,$0E
        .byte   $00,$00,$00,$00,$00,$02,$16,$F9
        .byte   $07,$07,$01,$08,$F9,$0E,$01,$F8
        .byte   $F9,$0E,$02,$10,$F5,$0B,$0B,$02
        .byte   $F0,$F5,$0B,$0B,$02,$08,$FC,$04
        .byte   $04,$00,$00,$00,$00,$00,$02,$0C
        .byte   $F5,$0B,$0B,$01,$0C,$F5,$16,$02
        .byte   $18,$F1,$0F,$0F,$02,$E8,$F1,$0F
        .byte   $0F,$01,$04,$FD,$06,$01,$FC,$FD
        .byte   $06,$01,$1C,$F9,$0E,$01,$10,$F9
        .byte   $07,$02,$E8,$F9,$07,$07,$01,$06
        .byte   $F9,$0E,$01,$08,$F5,$16,$02,$F8
        .byte   $F5,$0B,$0B,$01,$0C,$F9,$0E,$01
        .byte   $F4,$F9,$0E,$01,$04,$F9,$0E,$01
        .byte   $FC,$F9,$0E,$02,$10,$F1,$0F,$0F
        .byte   $02,$F0,$F1,$0F,$0F,$02,$0D,$F5
        .byte   $0B,$0B,$02,$F3,$F5,$0B,$0B,$02
        .byte   $10,$F1,$0F,$0F,$02,$E6,$F1,$0F
        .byte   $0F,$02,$14,$F1,$0F,$0F,$02,$00
        .byte   $00,$00,$00,$02,$14,$F0,$10,$10
        .byte   $02,$EC,$F0,$10,$10,$02,$18,$F1
        .byte   $0F,$0F,$02,$E8,$F1,$0F,$0F,$02
        .byte   $1C,$F5,$0B,$0B,$02,$DC,$F5,$0B
        .byte   $0B,$00,$04,$00,$00,$08,$00
metatile_gemini_scr3_starts:  .byte   $00,$05,$0A,$0E,$12,$17,$1C,$21
        .byte   $26,$2A,$2E,$33,$38,$3D,$42,$49
        .byte   $50,$57,$5E,$65,$6C,$71,$76,$7B
        .byte   $80,$85,$8A,$8F,$94,$99,$9E,$A2
        .byte   $A6,$AB,$B0,$B5,$BA,$BF
metatile_gemini_scr4_count:  .byte   $02
metatile_gemini_scr4_offset:  .byte   $08
metatile_gemini_scr4_widths:  .byte   $F5,$0B,$0B,$02,$F8,$F5,$0B,$0B
        .byte   $01,$10,$FB,$0C,$01,$F0,$FB,$0C
        .byte   $02,$00,$F5,$0B,$0B,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$08,$F9,$0E
        .byte   $01,$F8,$F9,$0E,$01,$0C,$F5,$0B
        .byte   $0B,$02,$F4,$F5,$0B,$0B,$02,$0C
        .byte   $F5,$0B,$0B,$02,$F4,$F5,$0B,$0B
        .byte   $04,$10,$E9,$10,$07,$10,$07,$04
        .byte   $F0,$E9,$10,$07,$10,$07,$04,$0C
        .byte   $E5,$10,$0B,$10,$0B,$04,$F4,$E5
        .byte   $10,$0B,$10,$0B,$04,$08,$E5,$10
        .byte   $0B,$10,$0B,$01,$F8,$E5,$10,$0B
        .byte   $10,$0B,$02,$10,$F1,$0F,$0F,$02
        .byte   $F0,$F1,$0F,$0F,$02,$14,$F5,$0B
        .byte   $0B,$02,$EC,$F5,$0B,$0B,$02,$0C
        .byte   $F9,$07,$07,$02,$F4,$F9,$07,$07
        .byte   $02,$0C,$F9,$07,$07,$02,$F4,$F9
        .byte   $07,$07,$02,$0C,$F9,$07,$07,$02
        .byte   $F4,$F9,$07,$07,$01,$04,$FD,$06
        .byte   $01,$FC,$FD,$06,$02,$10,$F1,$0F
metatile_gemini_scr4_hi_widths:  .byte   $0F,$02,$F0,$F1,$0F,$0F,$02,$14
        .byte   $EC,$14,$14,$02,$EC,$EC,$14,$14
        .byte   $02,$10,$E8,$18,$18,$02
        beq     metatile_gemini_scr4_hi_widths ; data (hitbox table continuation)
        clc                             ; data (hitbox table continuation)
        clc                             ; data (hitbox table continuation)

; -----------------------------------------------
; snap_x_to_wall_right — push entity left after hitting a wall on its right
; -----------------------------------------------
; After a rightward collision, aligns entity X to the left edge of the
; solid tile. $12 = collision check X; lower 4 bits = how far into the
; 16-pixel tile. Subtracts that offset from entity X position.
; -----------------------------------------------
snap_x_to_wall_right:  lda     $12      ; offset = $12 & $0F
        and     #$0F                    ; (sub-tile penetration depth)
        sta     $12                     ; store sub-tile penetration
        lda     ent_x_px,x              ; X pixel -= offset
        sec                             ; (push entity left)
        sbc     $12                     ; subtract penetration depth
        sta     ent_x_px,x              ; store corrected X pixel
        lda     ent_x_scr,x             ; X screen -= borrow
        sbc     #$00                    ; propagate borrow to screen
        sta     ent_x_scr,x             ; store corrected X screen
        rts                             ; return

; -----------------------------------------------
; snap_x_to_wall_left — push entity right after hitting a wall on its left
; -----------------------------------------------
; After a leftward collision, aligns entity X to the right edge of the
; solid tile + 1. $12 = collision check X; computes (16 - offset) and
; adds to entity X. EOR #$0F + SEC/ADC = add (16 - low_nibble).
; -----------------------------------------------

snap_x_to_wall_left:  lda     $12       ; offset = ($12 & $0F) ^ $0F
        and     #$0F                    ; = 15 - sub-tile position
        eor     #$0F                    ; invert to get complement
        sec                             ; X pixel += (16 - sub_tile_pos)
        adc     ent_x_px,x              ; SEC + ADC = add offset + 1
        sta     ent_x_px,x              ; (push entity right)
        lda     ent_x_scr,x             ; X screen += carry
        adc     #$00                    ; propagate carry to screen
        sta     ent_x_scr,x             ; store corrected X screen
        rts                             ; return

; -----------------------------------------------
; snap_y_to_ceil — push entity down after hitting a ceiling above
; -----------------------------------------------
; After an upward collision, aligns entity Y to the bottom of the
; solid tile + 1. Same math as snap_x_to_wall_left but for Y axis.
; Handles downward screen wrap at Y=$F0.
; -----------------------------------------------

snap_y_to_ceil:  lda     $11            ; offset = ($11 & $0F) ^ $0F
        and     #$0F                    ; isolate sub-tile position
        eor     #$0F                    ; invert to get complement
        sec                             ; Y pixel += (16 - sub_tile_pos)
        adc     ent_y_px,x              ; (push entity down)
        sta     ent_y_px,x              ; store corrected Y pixel
        cmp     #$F0                    ; screen height = $F0
        bcc     metatile_gemini_done    ; within screen? done
        adc     #$0F                    ; wrap to next screen down
        sta     ent_y_px,x              ; store wrapped Y pixel
        inc     ent_y_scr,x             ; Y screen++
metatile_gemini_done:  rts              ; return

; -----------------------------------------------
; snap_y_to_floor — push entity up after landing on a floor below
; -----------------------------------------------
; After a downward collision, aligns entity Y to the top of the
; solid tile. $11 = collision check Y; subtracts low nibble from
; entity Y. Preserves $11 (restored from stack after adjustment).
; Handles upward screen wrap (underflow).
; -----------------------------------------------

snap_y_to_floor:  lda     $11           ; save original $11
        pha                             ; save original $11
        and     #$0F                    ; offset = $11 & $0F
        sta     $11                     ; (sub-tile penetration depth)
        lda     ent_y_px,x              ; Y pixel -= offset
        sec                             ; (push entity up)
        sbc     $11                     ; subtract penetration depth
        sta     ent_y_px,x              ; store corrected Y pixel
        bcs     metatile_restore_result ; no underflow? done
        sbc     #$0F                    ; wrap to previous screen
        sta     ent_y_px,x              ; store wrapped Y pixel
        dec     ent_y_scr,x             ; Y screen--
metatile_restore_result:  pla           ; restore original $11
        sta     $11                     ; restore original $11
        rts                             ; return

; ===========================================================================
; call_bank10_8000 / call_bank10_8003 — bank $10 trampolines
; ===========================================================================
; Save current $8000-$9FFF bank, switch to bank $10, call entry point
; ($8000 or $8003), then restore original bank. Called from player cutscene
; state machine (code_1FE2F4, code_1FE338). Bank $10 contains cutscene/
; scripted sequence routines.
; ---------------------------------------------------------------------------

call_bank10_8000:  lda     mmc3_select  ; save current $8000 bank
        pha                             ; push current $8000 bank
        lda     #$10                    ; switch $8000-$9FFF to bank $10
        sta     mmc3_select             ; set $8000 bank to $10
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_8000             ; call bank $10 entry point 0
        pla                             ; restore original $8000 bank
        sta     mmc3_select             ; restore $8000 bank
        jmp     select_PRG_banks        ; apply bank switch and return

call_bank10_8003:  lda     mmc3_select  ; save current $8000 bank
        pha                             ; push current $8000 bank
        lda     #$10                    ; switch $8000-$9FFF to bank $10
        sta     mmc3_select             ; set $8000 bank to $10
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_8003             ; call bank $10 entry point 1
        pla                             ; restore original $8000 bank
        sta     mmc3_select             ; restore $8000 bank
        jmp     select_PRG_banks        ; apply bank switch and return

; ===========================================================================
; queue_metatile_clear — queue a 2×2 blank metatile write to PPU
; ===========================================================================
; Converts entity X's screen position (ent_x_px,x / ent_y_px,x) to a PPU nametable
; address and fills the NMI update buffer ($0780+) with two 2-tile rows of
; blank tiles ($00). NMI's drain_ppu_buffer drains this buffer during VBlank.
;
; Buffer format: [addr_hi][addr_lo][count][tile × (count+1)]...[FF=end]
; This builds two entries (top row + bottom row of metatile) + terminator.
;
; Input:  X = entity slot index
; Output: C=0 on success (buffer queued), C=1 if PPU update already pending
; Clobbers: $0780-$078A, $19
; Called from: bank1C_1D (tile destruction / block breaking)
; ---------------------------------------------------------------------------
queue_metatile_clear:

        sec                             ; preset carry = fail
        lda     nt_column_dirty         ; if nametable column or row
        ora     nametable_dirty         ; update already pending,
        bne     metatile_final_return   ; return C=1 (busy)
        lda     #$08                    ; seed high byte = $08
        sta     $0780                   ; (becomes $20-$23 after shifts)
        lda     ent_y_px,x              ; entity Y, upper nibble = metatile row
        and     #$F0                    ; ASL×2 with ROL into $0780:
        asl     a                       ; row * 64 → PPU row offset
        rol     $0780                   ; (each metatile = 2 tile rows × 32)
        asl     a                       ; row * 2 (continued)
        rol     $0780                   ; shift into high byte
        sta     $0781                   ; low byte = row component
        lda     ent_x_px,x              ; entity X, upper nibble
        and     #$F0                    ; LSR×3 = metatile column × 2
        lsr     a                       ; (each metatile = 2 tiles wide)
        lsr     a                       ; divide by 2 (continued)
        lsr     a                       ; divide by 2 (continued)
        ora     $0781                   ; merge column into low byte
        sta     $0781                   ; combine row + column
        ora     #$20                    ; addr + 32 = next tile row
        sta     $0786                   ; (second entry low byte)
        lda     #$01                    ; count = 1 → write 2 tiles per row
        sta     $0782                   ; (push entity left)
        sta     $0787                   ; count = 1 for second row too
        lda     $0780                   ; copy high byte for second entry
        sta     $0785                   ; X screen -= borrow
        lda     #$00                    ; tile data = $00 (blank) for all 4
        sta     $0783                   ; blank tile for row 0 left
        sta     $0784                   ; blank tile for row 0 right
        sta     $0788                   ; blank tile for row 1 left
        sta     $0789                   ; blank tile for row 1 right
        lda     #$FF                    ; terminator
        sta     $078A                   ; end-of-buffer marker
        sta     nametable_dirty         ; flag NMI to process buffer
        clc                             ; success
metatile_final_return:  rts             ; return

; ===========================================================================
; queue_metatile_update — build PPU update buffer for a 4×4 tile metatile
; ===========================================================================
; Converts metatile position ($28) to PPU nametable addresses and builds
; a 5-entry NMI update buffer at $0780: four 4-tile rows + one attribute
; byte. Tile data is sourced from $06C0-$06CF (filled by metatile_to_chr_tiles).
;
; $28 = metatile index (low 3 bits = column, upper bits = row)
; $10 = nametable select (bit 2: 0=$2000, 4=$2400)
; proto_man_flag = cutscene-complete flag (selects alternate tile lookup path)
; Y  = metatile ID (when $68=0, passed to metatile_column_ptr_by_id)
;
; Buffer layout: 4 row entries (7 bytes each) + 1 attribute entry + $FF end
;   Entry: [addr_hi][addr_lo][count=3][tile0][tile1][tile2][tile3]
;   Attr:  [attr_addr_hi][attr_addr_lo][count=0][attr_byte][$FF]
;
; Edge case: if metatile row >= $38 (bottom of nametable), the attribute
; entry overwrites the 3rd row entry to avoid writing past nametable bounds.
;
; Called from: bank09 (enemy spawning), bank0C (level loading), bank18
; ---------------------------------------------------------------------------
queue_metatile_update:

        lda     $28                     ; save metatile index
        pha                             ; push for later row extraction
        and     #$07                    ; column bits × 4 = PPU column offset
        asl     a                       ; (4 tiles wide per metatile)
        asl     a                       ; column × 4 tiles
        sta     $0781                   ; PPU addr low byte (column part)
        lda     #$02                    ; seed high byte = $02
        sta     $0780                   ; (becomes $20+ after shifts)
        pla                             ; row bits (upper 5 bits of $28)
        and     #$F8                    ; ASL×4 with ROL = row × 128
        asl     a                       ; (each metatile row = 4 tile rows
        rol     $0780                   ; × 32 bytes = 128)
        asl     a                       ; shift row bits left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row bits left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row bits left (continued)
        rol     $0780                   ; rotate into high byte
        ora     $0781                   ; merge row and column
        sta     $0781                   ; row 0 addr low byte
        clc                             ; prepare for addition
        adc     #$20                    ; row 1 addr low = row 0 + 32
        sta     $0788                   ; row 1 addr low byte
        adc     #$20                    ; row 2 addr low = row 1 + 32
        sta     $078F                   ; row 2 addr low byte
        adc     #$20                    ; row 3 addr low = row 2 + 32
        sta     $0796                   ; row 3 addr low byte
        lda     $28                     ; attribute addr low = $28 OR $C0
        ora     #$C0                    ; (attribute table offset)
        sta     $079D                   ; attribute table addr low byte
        lda     $0780                   ; merge nametable select bit
        ora     $10                     ; into all row high bytes
        sta     $0780                   ; (rows 0-3 share same high byte)
        sta     $0787                   ; row 1 high byte
        sta     $078E                   ; row 2 high byte
        sta     $0795                   ; row 3 high byte
        ora     #$03                    ; attribute high byte = $23 or $27
        sta     $079C                   ; attribute high = $23 or $27
        lda     #$03                    ; count = 3 → 4 tiles per row
        sta     $0782                   ; row 0 tile count
        sta     $0789                   ; row 1 tile count
        sta     $0790                   ; row 2 tile count
        sta     $0797                   ; row 3 tile count
        lda     #$00                    ; attribute entry count = 0 (1 byte)
        sta     $079E                   ; attribute entry count byte
        lda     proto_man_flag          ; cutscene-complete flag?
        beq     metatile_oam_id_load    ; no → normal metatile lookup
        lda     #$00                    ; clear high byte
        sta     $01                     ; clear high byte
        lda     $11                     ; load scroll position
        jsr     calc_chr_offset         ; calculate CHR tile offset
        jsr     metatile_to_chr_tiles_continue ; convert metatile to CHR tiles
        jmp     metatile_oam_copy_loop  ; skip normal lookup path

metatile_oam_id_load:  tya              ; Y = metatile ID
        jsr     metatile_column_ptr_by_id ; look up metatile definition
        jsr     metatile_to_chr_tiles   ; convert to CHR tile indices
metatile_oam_copy_loop:  ldx     #$03   ; copy 4×4 tile data from $06C0-$06CF
metatile_oam_row_copy:  lda     $06C0,x ; row 0: $06C0-$06C3 → $0783-$0786
        sta     $0783,x                 ; row 0 tiles → PPU entry 0
        lda     $06C4,x                 ; row 1: $06C4-$06C7 → $078A-$078D
        sta     $078A,x                 ; row 1 tiles
        lda     $06C8,x                 ; row 2: $06C8-$06CB → $0791-$0794
        sta     $0791,x                 ; row 2 tiles
        lda     $06CC,x                 ; row 3: $06CC-$06CF → $0798-$079B
        sta     $0798,x                 ; row 3 tiles
        dex                             ; next tile index
        bpl     metatile_oam_row_copy   ; loop all 4 tiles
        lda     $10                     ; attribute byte = nametable select
        sta     $079F                   ; store attribute byte
        stx     $07A0                   ; terminator ($FF from DEX past 0)
        lda     $28                     ; if metatile row >= $38 (bottom edge),
        and     #$3F                    ; row 3+4 would overflow nametable.
        cmp     #$38                    ; Move attribute entry up to replace
        bcc     metatile_oam_dirty_flag ; row 3 entry to avoid overflow.
        ldx     #$04                    ; copy attribute entry ($079C-$07A0)
metatile_oam_attr_copy:  lda     $079C,x ; source: attribute entry at $079C
        sta     $078E,x                 ; dest: overwrite row 2 entry at $078E
        dex                             ; next byte
        bpl     metatile_oam_attr_copy  ; loop all 5 bytes
metatile_oam_dirty_flag:  stx     nametable_dirty ; flag NMI to process buffer
        rts                             ; return

; ===========================================================================
; write_attribute_table — queue attribute table write to PPU
; ===========================================================================
; Called after fill_nametable_progressive finishes all 64 metatile columns.
; Copies the 64-byte attribute buffer ($0640-$067F) to the PPU write queue,
; targeting PPU $23C0 (attribute table of nametable 0 or 1 based on $10).
; Resets $70 to 0 so the nametable fill can restart if needed.
; ---------------------------------------------------------------------------

write_attribute_table:  lda     #$00    ; reset progress counter
        sta     $70                     ; progress counter = 0
        lda     #$23                    ; PPU address high byte: $23 or $27
        ora     $10                     ; ($10 bit 2 = nametable select)
        sta     $0780                   ; PPU addr high byte
        lda     #$C0                    ; PPU address low byte: $C0
        sta     $0781                   ; PPU addr low = $C0 (attr table start)
        ldy     #$3F                    ; count = 64 bytes ($3F+1)
        sty     $0782                   ; byte count = 64
metatile_attr_buffer_copy:  lda     $0640,y ; copy attribute buffer to PPU queue
        sta     $0783,y                 ; $0783+ = attribute data for NMI
        dey                             ; previous byte
        bpl     metatile_attr_buffer_copy ; loop all 64 bytes
        sty     $07C3                   ; $FF terminator after data
        sty     nametable_dirty         ; signal NMI to process queue
        rts                             ; return

; ===========================================================================
; fill_nametable_progressive — fill nametable 4 metatile columns at a time
; ===========================================================================
; Called once per frame during level loading. Progress counter $70 tracks
; the current metatile column (0-63). Each call processes 4 columns,
; converting metatiles to CHR tiles and queuing them for PPU writes.
;
; Each metatile column = 2 CHR tiles wide. 4 columns = 8 tiles = one row
; group. The PPU write queue gets 4 entries (one per tile row), each
; containing 16 tiles ($0F + 1). After all 64 columns ($70=$40), branches
; to write_attribute_table.
;
; PPU address layout: $70 maps to nametable position:
;   - Low 3 bits × 4 = tile column offset within row
;   - High 5 bits × 16 = tile row offset (each row = 32 bytes)
;
; $10 = nametable select (bit 2: 0=NT0, 4=NT1).
; $28 = working column index within the 4-column group.
; $0640 = attribute buffer (one byte per metatile column).
; ---------------------------------------------------------------------------

fill_nametable_progressive:  lda     $70 ; if $70 == $40 (64), all done
        cmp     #$40                    ; → write attribute table and reset
        beq     write_attribute_table   ; all 64 columns done → attributes
        sta     $28                     ; $28 = working column index

; --- Compute PPU base address from $70 ---
; Column ($70 & $07) × 4 → low byte offset.
; Row ($70 & $F8) << 4 → high byte bits.
        pha                             ; save $70 for row extraction
        and     #$07                    ; column within row group × 4
        asl     a                       ; column × 2
        asl     a                       ; column × 4
        sta     $0781                   ; → PPU addr low byte (partial)
        lda     #$02                    ; seed high byte = $02
        sta     $0780                   ; (nametable $2000 base >> 8)
        pla                             ; restore $70
        and     #$F8                    ; row index × 128 via shift+rotate:
        asl     a                       ; ($70 & $F8) << 4 into high/low
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row left (continued)
        rol     $0780                   ; rotate into high byte
        ora     $0781                   ; merge low byte parts
        sta     $0781                   ; store merged low byte

; --- Set up 4 PPU write entries (4 tile rows) ---
; Each entry is 19 bytes apart ($13). Low bytes advance by $20 (one row).
        clc                             ; prepare for addition
        adc     #$20                    ; row 1 low byte
        sta     $0794                   ; row 1 PPU addr low byte
        adc     #$20                    ; row 2 low byte
        sta     $07A7                   ; row 2 PPU addr low byte
        adc     #$20                    ; row 3 low byte
        sta     $07BA                   ; row 3 PPU addr low byte
        lda     $0780                   ; high byte + nametable select ($10)
        ora     $10                     ; same for all 4 entries
        sta     $0780                   ; row 0 high byte
        sta     $0793                   ; row 1 high byte
        sta     $07A6                   ; row 2 high byte
        sta     $07B9                   ; row 3 high byte
        lda     #$0F                    ; tile count = 16 ($0F+1) per entry
        sta     $0782                   ; count = 16 tiles per entry ($0F+1)
        sta     $0795                   ; row 1 tile count
        sta     $07A8                   ; row 2 tile count
        sta     $07BB                   ; row 3 tile count

; --- Process 4 metatile columns ---
metatile_column_convert:  jsr     metatile_to_chr_tiles ; convert metatile → 16 tiles in $06C0
        lda     $28                     ; which column within group? (0-3)
        and     #$03                    ; determines position within entries
        tax                             ; X = column within group
        ldy     ppu_column_offsets,x    ; Y = starting offset {$03,$07,$0B,$0F}
        ldx     #$00                    ; X = source offset into $06C0
metatile_column_tile_copy:  lda     $06C0,x ; copy 4 tiles from $06C0
        sta     $0780,y                 ; to current position in PPU entry
        iny                             ; advance dest offset
        inx                             ; advance source offset
        txa                             ; check if 4 tiles copied
        and     #$03                    ; 4 tiles per metatile row?
        bne     metatile_column_tile_copy ; more tiles → continue copy
        tya                             ; advance Y past remaining entry bytes
        clc                             ; prepare for addition
        adc     #$0F                    ; skip to next entry start
        pha                             ; save dest offset
        ldy     $28                     ; load current column index
        lda     $10                     ; attribute byte from metatile lookup
        sta     $0640,y                 ; store in attribute buffer
        pla                             ; restore dest offset
        tay                             ; restore Y dest offset
        cpy     #$4C                    ; all 4 tile rows filled?
        bcc     metatile_column_tile_copy ; no → copy next row
        inc     $70                     ; advance progress counter
        inc     $28                     ; advance working column
        lda     $28                     ; repeat for 4 columns total
        and     #$03                    ; check if 4 columns done
        bne     metatile_column_convert ; more columns → continue
        lda     #$FF                    ; $FF terminator after 4th entry
        sta     $07CC                   ; terminator after PPU entries
        ldy     $28                     ; check if past bottom of nametable
        cpy     #$39                    ; column >= $39?
        bcc     metatile_column_signal_nmi ; no → signal NMI
        sta     $07A6                   ; $FF = terminate 3rd entry early
metatile_column_signal_nmi:  sta     nametable_dirty ; signal NMI to process PPU queue
        rts                             ; return

; PPU entry column offsets — maps column-within-group (0-3) to starting
; byte offset within each PPU write entry's data area.
ppu_column_offsets:  .byte   $03
        .byte   $07
        .byte   $0B
        .byte   $0F

; ===========================================================================
; SPRITE ANIMATION ENGINE — $F034-$F57F
; ===========================================================================
; Per-frame entity rendering system:
;   update_entity_sprites  — main loop, iterates 32 entity slots
;   process_entity_display — screen culling, death/damage handling
;   setup_sprite_render    — bank selection, animation state machine
;   write_entity_oam       — assembles OAM entries from sprite definitions
;   draw_energy_bars       — draws up to 3 HUD energy meters
;
; Entity slots 0-31 are processed in alternating order each frame
; (forward on even frames, reverse on odd) for OAM priority fairness.
;
; Animation sequence format at ($00):
;   byte 0 = total frames in sequence
;   byte 1 = ticks per frame (duration)
;   byte 2+ = sprite definition IDs per frame (0 = deactivate entity)
;
; Sprite definition format:
;   byte 0 = sprite count (bit 7: use CHR bank $14 instead of $19)
;   byte 1 = position offset table index (for Y/X offsets)
;   byte 2+ = pairs of (CHR tile, OAM attribute) per sprite
; ===========================================================================

; --- update_entity_sprites ---
; main per-frame entity rendering loop
; iterates all 32 entity slots (0-31), alternating direction each frame
; on even frames: draws HUD first, then slots 0→31
; on odd frames: slots 31→0, then draws HUD
; $95 = global frame counter, $96 = current slot index
; oam_ptr = OAM buffer write index (used by sprite assembly routines)
update_entity_sprites:  inc     $95     ; advance global frame counter
        inc     $4F                     ; advance secondary counter
        lda     $95                     ; load frame counter
        lsr     a                       ; even frame? forward iteration
        bcs     sprite_backward_loop    ; odd frame? backward iteration
        jsr     draw_energy_bars        ; draw HUD bars first on even frames
        ldx     #$00                    ; X = first entity slot
        stx     $96                     ; start at slot 0
sprite_entity_active_check:  lda     ent_status,x ; bit 7 = entity active
        bpl     sprite_entity_count_inc ; skip inactive
        jsr     process_entity_display  ; render this entity
sprite_entity_count_inc:  inc     $96     ; advance slot index
        ldx     $96                     ; next slot
        cpx     #$20                    ; 32 slots total
        bne     sprite_entity_active_check ; loop until all 32 done
        rts                             ; return after forward pass

sprite_backward_loop:  ldx     #$1F     ; start at slot 31
        stx     $96                     ; start at slot 31
sprite_entity_check:  lda     ent_status,x ; bit 7 = entity active
        bpl     sprite_count_dec        ; skip inactive
        jsr     process_entity_display  ; render this entity
sprite_count_dec:  dec     $96             ; decrement slot index
        ldx     $96                     ; previous slot
        bpl     sprite_entity_check     ; loop until slot 0 done
        jsr     draw_energy_bars        ; draw HUD bars last on odd frames
        rts                             ; return after backward pass

; --- process_entity_display ---
; per-entity display handler: screen culling, death/damage, OAM assembly
; X = entity slot index (0-31)
; $FC/$F9 = camera scroll X pixel/screen
; $12/$13 = screen Y/X position for OAM drawing

process_entity_display:  lda     ent_flags,x ; load entity flags
        and     #$10                    ; bit 4 = uses world coordinates
        beq     sprite_screen_fixed_x   ; 0 = screen-fixed position
        lda     ent_x_px,x              ; load entity X pixel position
        sec                             ; screen X = entity X - camera X
        sbc     camera_x_lo             ; subtract camera X low byte
        sta     $13                     ; store screen X position
        lda     ent_x_scr,x             ; entity X screen - camera screen
        sbc     camera_screen           ; if different screen, offscreen
        bne     process_entity_offscreen ; X screens differ → offscreen
        lda     ent_y_scr,x             ; Y screen = 0? → on same screen
        beq     sprite_screen_fixed_y   ; on-screen → proceed to render
        cpx     #$00                    ; player? keep visible
        beq     sprite_deactivate_offscreen ; slot 0 = player, special handling
        lda     ent_y_scr,x             ; Y screen negative? keep visible
        bmi     sprite_deactivate_offscreen ; Y screen < 0? keep visible
        bpl     process_entity_deactivate ; Y screen > 0? offscreen, deactivate
process_entity_offscreen:  lda     ent_flags,x ; check wide offscreen margin flag
        and     #$08                    ; (keep alive within 72px of edge)
        beq     process_entity_deactivate ; no margin flag → deactivate
        lda     $13                     ; take absolute value of screen X
        bcs     process_entity_margin   ; carry set = positive
        eor     #$FF                    ; negate if negative
        adc     #$01                    ; complete two's complement
process_entity_margin:  cmp     #$48    ; within 72px margin?
        bcc     sprite_deactivate_offscreen ; within margin → keep alive, skip draw
process_entity_deactivate:  lda     #$00 ; deactivate entity
        sta     ent_status,x            ; clear entity status
        lda     #$FF                    ; mark as despawned
        sta     ent_spawn_id,x          ; prevent respawn
        rts                             ; return after deactivation

sprite_deactivate_offscreen:  lda     ent_flags,x ; load entity flags
        and     #$7F                    ; clear bit 7 (drawn flag)
        sta     ent_flags,x             ; update flags
        cpx     #$00                    ; not player? skip to sprite draw
        bne     sprite_landed_setup     ; not player → skip death check
; --- DEBUG (shipped in retail) — P2 Right = pit death immunity ---
        lda     $17                     ; P2 held buttons
        and     #$01                    ; bit 0 = Right held?
        bne     sprite_landed_check     ; yes → skip death trigger
        lda     ent_y_scr               ; player Y screen negative? draw
        bmi     sprite_landed_setup     ; Y screen < 0 → above screen, draw
        lda     #$0E                    ; set player state = $0E (death)
        cmp     player_state            ; if already dead, skip
        beq     sprite_display_return   ; already dead? skip
        sta     player_state            ; store death state
        lda     #SNDCMD_STOP            ; stop current music
        jsr     submit_sound_ID         ; queue stop command
        lda     #SFX_DEATH              ; play death music
        jsr     submit_sound_ID         ; queue death sound
sprite_display_return:  rts             ; return after triggering death

sprite_landed_check:  lda     ent_y_scr ; Y screen = 1? (landed from above)
        cmp     #$01                    ; Y screen = 1? (one screen below)
        bne     sprite_landed_setup     ; no → skip landing override
        lda     #PSTATE_AIRBORNE        ; set player state = $01 (airborne)
        sta     player_state            ; player state = airborne
        lda     #$10                    ; set Y subpixel speed = $10
        sta     ent_yvel_sub            ; set Y sub velocity
        lda     #$0D                    ; Y vel = $0D (fast fall)
        sta     ent_yvel                ; set Y velocity = fast fall
sprite_landed_setup:  jmp     setup_sprite_render ; proceed to sprite rendering

sprite_screen_fixed_x:  lda     ent_x_px,x ; for screen-fixed entities,
        sta     $13                     ; X pos is already screen-relative
sprite_screen_fixed_y:  lda     ent_y_px,x ; Y pixel → draw Y position
        sta     $12                     ; store screen Y position
        lda     ent_flags,x             ; load entity flags
        ora     #$80                    ; set bit 7 (mark as drawn)
        sta     ent_flags,x             ; store updated flags
        and     #$04                    ; bit 2 = invisible (skip OAM)
        beq     setup_sprite_render     ; visible → render sprite
sprite_render_ret:  rts                 ; invisible → skip rendering

; --- setup_sprite_render ---
; prepares sprite bank selection and animation state for OAM assembly
; X = entity slot, $10 = h-flip flag, $11 = flip offset
; $00/$01 = pointer to animation sequence data

setup_sprite_render:  ldy     #$00      ; Y = 0 (no flip)
        lda     ent_flags,x             ; bit 6 = horizontal flip
        and     #ENT_FLAG_HFLIP         ; isolate H-flip bit
        sta     $10                     ; $10 = flip flag for OAM attr
        beq     sprite_flip_offset_setup ; not flipped → Y stays 0
        iny                             ; Y=1 if flipped
sprite_flip_offset_setup:  sty     $11  ; $11 = flip table offset
        cpx     #$10                    ; slot < $10? enemy/boss slot
        bcc     sprite_oam_bank_select  ; enemy slot → normal bank select
        lda     boss_active             ; boss active? override sprite bank
        beq     sprite_oam_bank_select  ; 0 = no override
        ldy     #$15                    ; weapon sprites in PRG bank $15
        cpy     mmc3_select             ; already selected?
        beq     sprite_oam_id_check     ; already bank $15 → skip switch
        bne     sprite_bank_select_write ; (always branches)
sprite_oam_bank_select:  ldy     #$1A   ; OAM ID bit 7 selects bank:
        lda     ent_anim_id,x           ; $00-$7F → bank $1A
        bpl     sprite_bank_select_check ; bit 7 clear → bank $1A
        iny                             ; Y=$1B → bank $1B for bit 7 set
sprite_bank_select_check:  cpy     mmc3_select ; bank select already correct?
        beq     sprite_oam_id_check     ; already correct → skip switch
sprite_bank_select_write:  sty     mmc3_select ; set $8000 bank
        stx     temp_00                 ; save entity slot index
        jsr     select_PRG_banks        ; apply bank switch
        ldx     temp_00                 ; restore entity slot index
sprite_oam_id_check:  lda     ent_anim_id,x ; OAM ID = 0? no sprite
        beq     sprite_render_ret       ; return
        and     #$7F                    ; strip bank select bit
        tay                             ; Y = OAM ID (7-bit index)
        lda     banked_8000,y           ; low byte of anim sequence ptr
        sta     temp_00                 ; store ptr low
        lda     $8080,y                 ; high byte of anim sequence ptr
        sta     $01                     ; store ptr high

; --- animation tick / frame advancement ---
; Animation sequence format at ($00):
;   byte 0 = total frames in sequence
;   byte 1 = ticks per frame (duration)
;   byte 2+ = sprite definition IDs per frame (0 = deactivate entity)
;
; --- DEBUG (shipped in retail) — P2 Up = slow-mo, P2 A = freeze ---
; Holding Up on controller 2 reduces animation speed to 1/8th.
; Holding A on controller 2 freezes all animation entirely.
; If both are held, A (freeze) takes priority over Up (slow-mo).
        lda     $17                     ; P2 held buttons
        and     #$08                    ; bit 3 = Up held? (slow-mo)
        beq     sprite_anim_freeze_check ; no → check global freeze
        lda     $95                     ; slow-mo: tick every 8th frame
        and     #$07                    ; frame counter mod 8
        bne     sprite_drawn_flag_check ; not 8th frame → skip tick
        lda     $17                     ; also check P2 held buttons
        and     #$80                    ; bit 7 = A held? (full freeze)
        bne     sprite_drawn_flag_check ; yes → skip tick entirely
sprite_anim_freeze_check:  lda     $58  ; $58 = global animation freeze
        bne     sprite_drawn_flag_check ; nonzero = skip ticking
        lda     ent_anim_frame,x        ; ent_anim_frame = frame tick counter
        and     #$7F                    ; (bit 7 = damage flash flag)
        inc     ent_anim_frame,x        ; increment tick
        ldy     #$01                    ; Y=1 → offset to duration byte
        cmp     (temp_00),y             ; compare tick vs duration at seq[1]
        bne     sprite_drawn_flag_check ; not reached? keep waiting
        lda     ent_anim_frame,x        ; tick reached duration:
        and     #$80                    ; reset tick to 0, preserve bit 7
        sta     ent_anim_frame,x        ; (damage flash flag)
        lda     ent_anim_state,x        ; ent_anim_state = current frame index
        and     #$7F                    ; (bit 7 preserved)
        inc     ent_anim_state,x        ; advance to next frame
        dey                             ; Y=0
        cmp     (temp_00),y             ; compare frame vs total at seq[0]
        bne     sprite_drawn_flag_check ; more frames? continue
        lda     #$00                    ; reached end: loop back to frame 0
        sta     ent_anim_state,x        ; reset to frame 0

; --- damage flash and sprite definition lookup ---
sprite_drawn_flag_check:  lda     ent_flags,x ; bit 7 = drawn flag (set by process_entity_display)
        bpl     sprite_render_skip_draw ; not marked for drawing? skip
        lda     ent_anim_frame,x        ; bit 7 of tick counter = damage flash active
        bpl     sprite_weapon_slot_check ; not flashing? draw normally
        lda     frame_counter           ; frame counter for blink timing
        and     #$04                    ; blink every 4 frames
        bne     sprite_render_skip_draw ; odd phase = skip draw (invisible)
sprite_weapon_slot_check:  cpx     #$10 ; slot < 16? not a weapon/projectile
        bcc     sprite_anim_frame_strip ; not a weapon? skip lifetime check
        lda     ent_hp,x                ; ent_hp = projectile lifetime/timer
        and     #$E0                    ; upper 3 bits = active countdown
        beq     sprite_anim_frame_strip ; not counting down? normal
        lda     ent_status,x            ; ent_status bit 6 = slow decay flag
        and     #$40                    ; isolate slow-decay bit
        beq     sprite_hp_timer_dec     ; not set? always decay
        lda     $4F                     ; slow decay: only every 4th frame
        and     #$03                    ; every 4th frame only
        bne     sprite_damage_flash_check ; skip decay this frame
sprite_hp_timer_dec:  lda     ent_hp,x  ; subtract $20 from timer
        sec                             ; (decrement upper 3-bit counter)
        sbc     #$20                    ; decrement upper 3-bit counter
        sta     ent_hp,x                ; store decremented timer
sprite_damage_flash_check:  lda     ent_status,x ; bit 6 = explode when timer expires?
        and     #$40                    ; isolate explode-on-expire bit
        beq     sprite_render_skip_draw ; no explode flag? skip draw
        lda     ent_hp,x                ; check if timer reached $20 threshold
        and     #$20                    ; check if timer at $20 threshold
        beq     sprite_anim_frame_strip ; not yet? keep going
        lda     #$AF                    ; use explosion sprite def $AF
        bne     write_entity_oam        ; (always branches)
sprite_anim_frame_strip:  lda     ent_anim_state,x ; current frame index (strip bit 7)
        and     #$7F                    ; strip bit 7
        clc                             ; +2 to skip header bytes (count, duration)
        adc     #$02                    ; skip count + duration bytes
        tay                             ; Y = frame index + 2
        lda     (temp_00),y             ; load sprite def ID from sequence
        bne     write_entity_oam        ; nonzero? go draw it
        sta     ent_status,x            ; def ID = 0: deactivate entity
        lda     #$FF                    ; mark as despawned
        sta     ent_spawn_id,x          ; mark spawn slot as used
sprite_render_skip_draw:  rts            ; return without drawing

; -----------------------------------------------
; write_entity_oam — assembles OAM entries from sprite definition
; -----------------------------------------------
; Entry: A = sprite definition ID
;   $10 = H-flip mask (ENT_FLAG_HFLIP or $00)
;   $11 = flip table offset (0=normal, 1=flipped)
;   $12 = entity screen Y position
;   $13 = entity screen X position
;   oam_ptr = OAM buffer write pointer
; Sprite definition format:
;   byte 0 = sprite count (bit 7: use CHR bank $14 instead of $19)
;   byte 1 = position offset table index (for Y/X offsets)
;   byte 2+ = pairs of (CHR tile, OAM attribute) per sprite
; Position offset table at ($05/$06): Y offset, X offset per sprite

write_entity_oam:  tay                  ; sprite def ID → index
        lda     $8100,y                 ; $02/$03 = pointer to sprite definition
        sta     $02                     ; (low bytes at $8100+ID,
        lda     $8200,y                 ; high bytes at $8200+ID)
        sta     $03                     ; store ptr high byte
        ldy     #$00                    ; start at byte 0 of definition
        lda     ($02),y                 ; byte 0 = sprite count + bank flag
        pha                             ; save sprite count + bank flag
        ldy     #$19                    ; default CHR bank = $19
        pla                             ; restore count + bank flag
        bpl     write_oam_set_chr_bank  ; bit 7 clear → use bank $19
        and     #$7F                    ; strip bank flag from count
        ldy     #$14                    ; use CHR bank $14 instead
write_oam_set_chr_bank:  sta     $04    ; $04 = sprite count (0-based loop counter)
        cpy     prg_bank                ; current PRG bank already correct?
        beq     write_oam_position_offsets ; yes → skip bank switch
        sty     prg_bank                ; set new PRG bank
        stx     $05                     ; save entity slot
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $05                     ; restore entity slot
write_oam_position_offsets:  ldy     #$01 ; byte 1 = position offset table index
        lda     ($02),y                 ; add flip offset ($11=0 or 1)
        clc                             ; to select normal/flipped offsets
        adc     $11                     ; add flip offset for mirrored positions
        pha                             ; save offset table index
        lda     ent_flags,x             ; bit 5 = on-ladder flag
        and     #$20                    ; stored to $11 for OAM attr overlay
        sta     $11                     ; (behind-background priority)
        pla                             ; offset table index → X
        tax                             ; X = offset table index
        lda     $BE00,x                 ; $BE00/$BF00 = pointer table for
        sec                             ; sprite position offsets
        sbc     #$02                    ; subtract 2 to align with def bytes
        sta     $05                     ; $05/$06 = offset data pointer
        lda     $BF00,x                 ; offset table ptr high
        sbc     #$00                    ; borrow from high byte
        sta     $06                     ; store offset ptr high byte
        ldx     oam_ptr                 ; X = OAM write position
        beq     write_oam_buffer_full   ; 0 = buffer wrapped, full
write_oam_sprite_loop:  lda     #$F0    ; default Y clip boundary = $F0
        sta     temp_00                 ; $00 = Y clip boundary
        lda     stage_id                ; check stage ID
        cmp     #STAGE_DOC_NEEDLE       ; Doc Robot Needle stage?
        bne     write_oam_read_tile     ; no → use default clip
        lda     camera_screen           ; camera screen position
        cmp     #$15                    ; screen $15? underwater area
        beq     write_oam_y_clip        ; yes → reduce Y clip
        cmp     #$1A                    ; screen $1A? underwater area
        bne     write_oam_read_tile     ; not underwater → default clip
write_oam_y_clip:  lda     #$B0         ; Y clip = $B0 for these screens
        sta     temp_00                 ; store reduced Y clip boundary
write_oam_read_tile:  iny               ; read CHR tile from def
        lda     ($02),y                 ; read CHR tile number
        sta     $0201,x                 ; OAM byte 1 = tile index
        lda     $12                     ; screen Y + offset Y
        clc                             ; add Y position offset
        adc     ($05),y                 ; Y pos = screen Y + offset
        sta     $0200,x                 ; OAM byte 0 = Y position
        lda     ($05),y                 ; overflow detection:
        bmi     write_oam_y_underflow   ; offset negative?
        bcc     write_oam_y_range       ; no underflow → check Y range
        bcs     write_oam_skip_attr     ; positive offset + carry = overflow
write_oam_y_underflow:  bcc     write_oam_skip_attr ; neg offset + no borrow = underflow
write_oam_y_range:  lda     $0200,x     ; check Y against clip boundary
        cmp     temp_00                 ; compare Y to clip boundary
        bcs     write_oam_skip_attr     ; Y >= clip? hide sprite
        iny                             ; read OAM attribute from def
        lda     ($02),y                 ; EOR with flip flag ($10)
        eor     $10                     ; ORA with ladder priority ($11)
        ora     $11                     ; apply behind-background priority
        sta     $0202,x                 ; OAM byte 2 = attribute
        lda     $13                     ; screen X + offset X
        clc                             ; add X position offset
        adc     ($05),y                 ; add X position offset
        sta     $0203,x                 ; OAM byte 3 = X position
        lda     ($05),y                 ; overflow detection for X
        bmi     write_oam_x_overflow    ; X offset negative?
        bcc     energy_bar_draw_loop    ; no overflow → write next sprite
        bcs     write_oam_hide_sprite   ; positive offset + carry = offscreen
write_oam_x_overflow:  bcc     write_oam_hide_sprite ; neg offset + no borrow = offscreen
energy_bar_draw_loop:  inx              ; advance OAM pointer by 4
        inx                             ;  (4 bytes per OAM entry:
        inx                             ;   Y, tile, attr, X)
        inx                             ;  X now points to next entry
        stx     oam_ptr                 ; update write position
        beq     write_oam_buffer_full   ; wrapped to 0? buffer full
write_oam_sprite_next:  dec     $04     ; decrement sprite count
        bpl     write_oam_sprite_loop   ; more sprites? continue
write_oam_buffer_full:  rts             ; OAM buffer full or all sprites done

write_oam_skip_attr:  iny               ; skip attribute byte
write_oam_hide_sprite:  lda     #$F8    ; hide sprite (Y=$F8 = below screen)
        sta     $0200,x                 ; Y = $F8 hides sprite offscreen
        bne     write_oam_sprite_next   ; always taken → next sprite

; -----------------------------------------------
; draw_energy_bars — draws up to 3 HUD energy meters
; -----------------------------------------------
; $B1-$B3 = bar slot IDs (bit 7 = active, bits 0-6 = energy index)
; $A2+Y = energy value ($80=empty, HEALTH_FULL=full, 28 units)
; Alternates iteration direction each frame for OAM priority fairness.
; Each bar draws 7 sprites vertically: tile selected from $F313 table,
; position from $F318 (OAM attr) and $F31B (X position).
draw_energy_bars:  lda     $95          ; alternate direction each frame
        lsr     a                       ; even/odd frame check
        bcs     energy_bar_reverse      ; odd = reverse
        ldx     #$00                    ; forward: bar 0, 1, 2
        stx     $10                     ; $10 = bar index
energy_bar_forward_loop:  jsr     draw_one_energy_bar ; draw bar[$10]
        inc     $10                     ; next bar index
        ldx     $10                     ; load next bar index
        cpx     #$03                    ; all 3 bars drawn?
        bne     energy_bar_forward_loop ; loop 3 bars
        rts                             ; return

energy_bar_reverse:  ldx     #$02       ; reverse: bar 2, 1, 0
        stx     $10                     ; start at bar 2
energy_bar_reverse_loop:  jsr     draw_one_energy_bar ; +2 to skip header bytes (count, duration)
        dec     $10                     ; next bar (decrement)
        ldx     $10                     ; load bar index
        bpl     energy_bar_reverse_loop ; bar index >= 0? continue
energy_bar_exit:  rts                   ; return

; draw_one_bar — draws a single energy meter
; X = bar index (0-2), $B1+X = slot ID

draw_one_energy_bar:  lda     $B1,x     ; bit 7 = bar active?
        bpl     energy_bar_exit         ; not active? skip
        and     #$7F                    ; energy index (Y into $A2 table)
        tay                             ; Y = energy index
        lda     player_hp,y             ; read energy value
        and     #$7F                    ; strip bit 7 (display flag)
        sta     temp_00                 ; $00 = energy remaining (0-28)
        lda     bar_attributes,x        ; $01 = OAM attribute (palette)
        sta     $01                     ; bar 0=$00, 1=$01, 2=$02
        lda     bar_x_positions,x       ; $02 = X position
        sta     $02                     ; bar 0=$10, 1=$18, 2=$28
        ldx     oam_ptr                 ; X = OAM write pointer
        beq     energy_bar_buffer_full  ; 0 = full, skip
        lda     #$48                    ; $03 = starting Y position ($48)
        sta     $03                     ; draws upward 7 segments
energy_bar_segment_loop:  lda     $01  ; write OAM attribute
        sta     $0202,x                 ; OAM byte 2 = attribute/palette
        lda     $02                     ; write X position
        sta     $0203,x                 ; OAM byte 3 = X position
        lda     $03                     ; write Y position
        sta     $0200,x                 ; OAM byte 0 = Y position
        ldy     #$04                    ; 4 energy units per segment
        lda     temp_00                 ; load remaining energy
        sec                             ; subtract 4 from remaining
        sbc     #$04                    ; subtract 4 units
        bcs     energy_bar_fill_segment ; still >= 0? full segment tile
        ldy     temp_00                 ; partial: Y = remaining 0-3
        lda     #$00                    ; energy = 0
energy_bar_fill_segment:  sta     temp_00 ; update remaining energy
        lda     bar_fill_tiles,y        ; tile from fill table: 4=$6B 3=$6A 2=$69 1=$68 0=$67
        sta     $0201,x                 ; OAM tile
        inx                             ; advance OAM by 4
        inx                             ; advance OAM pointer
        inx                             ; (4 bytes per entry)
        inx                             ; (continued)
        beq     energy_bar_buffer_full  ; OAM buffer full?
        lda     $03                     ; Y -= 8 (move up one tile)
        sec                             ; prepare for subtraction
        sbc     #$08                    ; move up 8 pixels (one tile height)
        sta     $03                     ; store updated Y position
        cmp     #$10                    ; stop at Y=$10 (7 segments: $48→$10)
        bne     energy_bar_segment_loop ; loop if Y > $10
energy_bar_buffer_full:  stx     oam_ptr ; update OAM pointer
        rts                             ; return

; energy bar tile table: index=fill level (0=empty, 4=full)
bar_fill_tiles:  .byte   $6B,$6A,$69,$68,$67

; energy bar OAM attributes (palette): bar 0, 1, 2
bar_attributes:  .byte   $00,$01,$02

; energy bar X positions: bar 0=$10, 1=$18, 2=$28
bar_x_positions:  .byte   $10,$18,$28,$A0,$FB,$2A,$EC,$88
        .byte   $DF,$B8,$FE,$08,$50,$0A,$EB,$0A
        .byte   $6D,$8A,$6F,$2A,$D0,$A0,$B7,$8E
        .byte   $CD,$0A,$EC,$28,$C8,$28,$13,$A0
        .byte   $F7,$68,$83,$80,$CC,$A8,$BE,$AA
        .byte   $FB,$AA,$ED,$2A,$F9,$08,$F4,$A8
        .byte   $F7,$AA,$FB,$20,$CF,$2A,$9F,$28
        .byte   $BF,$CA,$FC,$E0,$78,$CA,$FF,$A2
        .byte   $BF,$20,$FB,$6A,$FF,$28,$FE,$AA
        .byte   $66,$A8,$B7,$8A,$59,$A2,$B2,$2B
        .byte   $AD,$0A,$96,$22,$3A,$82,$CA,$2A
        .byte   $DB,$A8,$92,$9A,$27,$88,$7E,$2A
        .byte   $E7,$46,$EE,$A2,$E9,$26,$C6,$C8
        .byte   $8C,$02,$DE,$B6,$AF,$E2,$5F,$EA
        .byte   $7D,$8A,$BB,$BA,$EC,$02,$FE,$90
        .byte   $E7,$88,$F7,$AE,$FE,$EA,$EE,$2A
        .byte   $B9,$2A,$BB,$A8,$77,$40,$E3,$AA
        .byte   $3D,$0A,$B2,$B8,$4F,$8A,$B8,$CC
        .byte   $FB,$A2,$FB,$81,$6E,$B2,$AD,$82
        .byte   $EF,$93,$76,$AC,$5F,$2E,$CF,$04
        .byte   $E1,$B0,$FF,$82,$9C,$2A,$FF,$6A
        .byte   $83,$A8,$D8,$AA,$4F,$2B,$F3,$28
        .byte   $5C,$2E,$D4,$8A,$52,$82,$67,$0A
        .byte   $D3,$EA,$5E,$0A,$9C,$8A,$36,$82
        .byte   $AB,$2A,$7E,$AD,$F8,$A0,$CE,$2A
        .byte   $2D,$A0,$ED,$88,$9F,$88,$D7,$2A
        .byte   $BF,$AA,$78,$A0,$ED,$A8,$DC,$0E
        .byte   $9B,$28,$FE,$EA,$75,$A2,$57,$A8
        .byte   $99,$A2,$7F,$A2,$B9,$7A,$04,$FC
        .byte   $11,$DC,$05,$3B,$05,$9B,$40,$4B
        .byte   $55,$3C,$51,$8A,$00,$67,$54,$72
        .byte   $15,$1A,$11,$59,$15,$AA,$45,$16
        .byte   $41,$47,$05,$A0,$51,$F1,$45,$76
        .byte   $10,$84,$11,$FF,$14,$6A,$54,$B8
        .byte   $54,$DC,$55,$FD,$40,$92,$51,$1D
        .byte   $14,$32,$40,$C3,$44,$40,$11,$84
        .byte   $45,$96,$44,$D9,$11,$0F,$01,$DB
        .byte   $40,$96,$01,$24,$11,$62,$19,$83
        .byte   $55,$4F,$15,$E7,$14,$3F,$04,$A0
        .byte   $45,$EE,$70,$8A,$54,$59,$44,$7D
        .byte   $11,$39,$44,$6F,$41,$1B,$15,$42
        .byte   $05,$71,$50,$E8,$11,$FD,$44,$2E
        .byte   $00,$BC,$45,$F1,$14,$7B,$51,$E1
        .byte   $45,$49,$10,$2C,$44,$46,$45,$F3
        .byte   $45,$46,$04,$E7,$40,$41,$00,$E8
        .byte   $41,$7A,$01,$E7,$11,$92,$11,$9F
        .byte   $00,$E7,$45,$4E,$51,$D0,$11,$C1
        .byte   $45,$26,$41,$48,$40,$0C,$50,$CD
        .byte   $51,$31,$54,$4D,$50,$6B,$00,$98
        .byte   $51,$78,$14,$CF,$55,$4C,$05,$8E
        .byte   $14,$26,$15,$EB,$04,$CB,$14,$68
        .byte   $50,$06,$00,$F9,$15,$9B,$20,$0C
        .byte   $15,$DE,$01,$C1,$55,$B5,$51,$20
        .byte   $11,$6A,$40,$C1,$00,$19,$54,$58
        .byte   $05,$2C,$41,$0B,$40,$11,$00,$E6
        .byte   $50,$AF,$14,$94,$00,$03,$51,$E6
        .byte   $04,$8F,$15,$EE,$51,$6E,$11,$D7
        .byte   $55,$F6,$05,$6E,$55,$C5,$D5,$CB
        .byte   $51,$FF,$40,$7A,$41,$FA,$50,$57
        .byte   $55,$59,$10,$40,$10,$29,$C4,$01
        .byte   $00,$20,$54,$2D,$45,$60,$04,$89
        .byte   $14,$2F,$65,$84,$51,$A1,$41,$BA
        .byte   $1C,$07,$01,$6C,$41,$79,$54,$88
        .byte   $51,$21,$40,$F9,$14,$3C,$11,$24
        .byte   $15,$A0,$54,$86,$1A,$78,$10,$41
        .byte   $05,$94,$54,$6D,$11,$85,$40,$A1
        .byte   $45,$25,$45,$23,$45,$53,$14,$E3
        .byte   $51,$67,$4C,$F7,$01,$8A,$14,$62
        .byte   $44,$C6,$45,$CA,$44,$E0,$45,$E7
        .byte   $01,$05,$14,$04,$41,$F6,$05,$C3
        .byte   $10,$03,$40,$AA,$40,$CA,$01,$66
        .byte   $05,$DB,$05,$0C,$14,$3D,$44,$80
        .byte   $50,$0A,$51,$AC,$71,$EE,$40,$53
        .byte   $D4,$F5,$05,$56,$00,$EC,$14,$9A
        .byte   $10,$B0,$50,$BE,$55,$F6,$14,$7C
        .byte   $01,$E0,$54,$2A,$45,$4A,$51,$6D
        ora     $38,x                   ; data (disassembler artifact)
        .byte   $14                     ; password table (continued)
        .byte   $E2
        .byte   $50

; ===========================================================================
; ENTITY MOVEMENT ROUTINES — $F580-$F897
; ===========================================================================
; Directional movement with collision, raw position updates,
; gravity/velocity system, animation reset, and facing logic.
; Shared by player (slot 0) and enemies.
;
; Velocity convention:
;   Y velocity: positive = rising, negative = falling
;   Position -= velocity, so negative vel subtracts a negative = adds to Y
;   Terminal velocity = $F9.00 (-7 px/frame)
; ===========================================================================

; -----------------------------------------------
; move_right_collide — move right + set facing + tile collision
; -----------------------------------------------
; Sets H-flip flag (facing right), saves pre-move X for player,
; moves right, checks screen boundary (player only), checks tile collision.
; Returns: C=1 if hit solid tile, C=0 if clear
move_right_collide:  lda     ent_flags,x ; set bit 6 = facing right (H-flip)
        ora     #ENT_FLAG_HFLIP         ; set H-flip bit (facing right)
        sta     ent_flags,x     ; store updated flags
move_right_collide_no_face:             ; 0 = full, skip
        cpx     #$00                    ; player (slot 0)?
        bne     move_right_collide_cont ; non-player → skip save
        lda     ent_x_px                ; save player X before move
        sta     $02                     ; save pre-move X pixel
        lda     ent_x_scr               ; write X position
        sta     $03                     ; save pre-move X screen
move_right_collide_cont:  jsr     move_sprite_right ; apply rightward movement
        cpx     #$00                    ; player?
        bne     mr_tile_check           ; non-player: just check tile
        jsr     check_platform_horizontal ; player overlapping horiz platform?
        bcc     mr_tile_check           ; no → normal tile check
        jsr     platform_push_x_left    ; push player left (away from platform)
        jsr     move_right_platform     ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_right_platform:  beq     mr_tile_check ; equal → normal tile check
        bcc     mr_tile_check           ; carry clear → normal tile check
        iny                             ; crossed screen right → check left tiles
        jmp     ml_tile_check           ; use left-side tile check

mr_tile_check:  jsr     check_tile_collision ; check tiles at entity position
        jsr     update_collision_flags  ; OAM buffer full?
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_right_collision_exit ; not solid → return clear
        jsr     snap_x_to_wall_right    ; snap to tile boundary (right)
        sec                             ; set carry = hit solid
move_right_collision_exit:  rts         ; return collision result in C

; -----------------------------------------------
; move_left_collide — move left + set facing + tile collision
; -----------------------------------------------

move_left_collide:  lda     ent_flags,x ; clear bit 6 = facing left (no H-flip)
        and     #$BF                    ; clear H-flip bit (facing left)
        sta     ent_flags,x             ; store updated flags
move_left_collide_no_face:
        cpx     #$00                    ; player (slot 0)?
        bne     move_left_collide_cont  ; non-player → skip save
        lda     ent_x_px                ; save player X before move
        sta     $02                     ; save pre-move X pixel
        lda     ent_x_scr               ; player X screen
        sta     $03                     ; save pre-move X screen
move_left_collide_cont:  jsr     move_sprite_left ; apply leftward movement
        cpx     #$00                    ; player?
        bne     ml_tile_check           ; non-player: just check tile
        jsr     check_platform_horizontal ; player overlapping horiz platform?
        bcc     ml_tile_check           ; no → normal tile check
        jsr     platform_push_x_right   ; push player right (away from platform)
        jsr     move_left_platform      ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_left_platform:  bcs     ml_tile_check ; carry set → normal tile check
        dey                             ; crossed screen left → check right tiles
        jmp     mr_tile_check           ; use right-side tile check

ml_tile_check:  jsr     check_tile_collision ; check tiles at entity position
        jsr     update_collision_flags  ; update ladder/collision flags
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_left_collision_exit ; not solid → return clear
        jsr     snap_x_to_wall_left     ; snap to tile boundary (left)
        sec                             ; set carry = hit solid
move_left_collision_exit:  rts          ; return collision result in C

; -----------------------------------------------
; move_down_collide — move down + tile collision
; -----------------------------------------------

move_down_collide:  cpx     #$00        ; player?
        bne     move_down_collide_cont  ; non-player → skip save
        lda     ent_y_px                ; save player Y before move
        sta     $02                     ; save pre-move Y pixel
        lda     ent_x_scr               ; save pre-move X screen
        sta     $03                     ; save pre-move X screen
move_down_collide_cont:  jsr     move_sprite_down ; apply downward movement
        cpx     #$00                    ; player?
        bne     md_tile_check           ; non-player: just check tile
        jsr     check_platform_vertical ; player standing on vert platform?
        bcc     md_tile_check           ; no → normal tile check
        jsr     platform_push_y_up      ; push player up (onto platform)
        jsr     move_down_platform      ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_down_platform:  beq     md_tile_check ; equal → normal tile check
        bcc     md_tile_check           ; carry clear → normal tile check
        iny                             ; crossed screen down → check up tiles
        jmp     mu_tile_check           ; use upward tile check

md_tile_check:  jsr     check_tile_horiz ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_down_collision_exit ; not solid → return clear
        jsr     snap_y_to_floor         ; snap to tile boundary (down)
        sec                             ; set carry = hit solid
move_down_collision_exit:  rts          ; return collision result in C

; -----------------------------------------------
; move_up_collide — move up + tile collision
; -----------------------------------------------

move_up_collide:  cpx     #$00          ; player?
        bne     move_up_collide_cont    ; non-player → skip save
        lda     ent_y_px                ; save pre-move Y pixel
        sta     $02                     ; store Y pixel to $02
        lda     ent_x_scr               ; save pre-move X screen
        sta     $03                     ; store X screen to $03
move_up_collide_cont:  jsr     move_sprite_up ; apply upward movement
        cpx     #$00                    ; player?
        bne     mu_tile_check           ; non-player: just check tile
        jsr     check_platform_vertical ; player touching vert platform?
        bcc     mu_tile_check           ; no → normal tile check
        jsr     platform_push_y_down    ; push player down (below platform)
        jsr     move_up_platform        ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_up_platform:  bcs     mu_tile_check ; carry set → normal tile check
        dey                             ; crossed screen up → check down tiles
        jmp     md_tile_check           ; use downward tile check

mu_tile_check:  jsr     check_tile_horiz ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_up_collision_exit  ; not solid → return clear
        jsr     snap_y_to_ceil          ; snap to tile boundary (up)
        sec                             ; set carry = hit solid
move_up_collision_exit:  rts            ; return collision result in C

; -----------------------------------------------
; move_vertical_gravity — $99-based vertical movement with collision
; -----------------------------------------------
; Combines velocity application, $99, screen boundary handling,
; and tile collision for entities affected by $99.
; Y velocity convention: positive = rising, negative = falling.
; Returns: C=1 if landed on solid ground, C=0 if airborne.

move_vertical_gravity:  cpx     #$00    ; player?
        bne     gravity_save_y          ; non-player → skip save
        lda     ent_y_px                ; save player Y before move
        sta     $02                     ; store Y pixel to $02
        lda     ent_x_scr               ; save pre-move X screen
        sta     $03                     ; store X screen to $03
gravity_save_y:  lda     ent_yvel,x     ; Y velocity whole byte
        bpl     gravity_rising          ; positive = rising
        jsr     apply_y_velocity_fall   ; pos -= vel (moves down)
        cpx     #$00                    ; player?
        bne     gravity_apply_physics   ; non-player → skip platform check
        jsr     check_platform_vertical ; player landing on platform?
        bcc     gravity_apply_physics   ; no → normal tile collision
        jsr     platform_push_y_up      ; push player up (onto platform)
        jsr     gravity_platform_fall   ; check screen boundary crossing
        jmp     gravity_reset_vel       ; landed on platform → reset velocity

gravity_platform_fall:  beq     gravity_apply_physics ; no boundary cross → apply physics
        bcc     gravity_apply_physics   ; carry clear → apply physics
        iny                             ; crossed screen → adjust tile offset
        jmp     mu_tile_check           ; → upward tile check on new screen

gravity_apply_physics:  jsr     apply_gravity ; increase downward velocity
        jsr     check_tile_horiz        ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        cpx     #$00                    ; player?
        bne     gravity_tile_check      ; non-player → skip ladder check
        lda     tile_at_feet_max        ; tile type = ladder top ($40)?
        cmp     #TILE_LADDER_TOP        ; player lands on ladder top
        beq     gravity_snap_floor      ; ladder top → land on it
gravity_tile_check:  lda     $10        ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     gravity_airborne_exit   ; no solid → still falling
gravity_snap_floor:  jsr     snap_y_to_floor ; snap to tile boundary (down)
gravity_reset_vel:  jsr     reset_gravity ; reset velocity to rest value
        sec                             ; C=1: landed
        rts                             ; return C=1 (on ground)

gravity_rising:  iny                    ; Y=1: upward collision check offset
        jsr     apply_y_velocity_rise   ; pos -= vel (moves up)
        cpx     #$00                    ; player?
        bne     gravity_apply_rise      ; non-player → skip platform check
        jsr     check_platform_vertical ; player hitting platform from below?
        bcc     gravity_apply_rise      ; no collision → normal rise
        jsr     platform_push_y_down    ; push player down (below platform)
        jsr     gravity_platform_rise   ; check screen boundary crossing
        jmp     gravity_snap_ceil       ; hit platform ceiling → reset velocity

gravity_platform_rise:  bcs     gravity_apply_rise ; no boundary cross → normal rise
        dey                             ; may need opposite tile check
        jmp     md_tile_check           ; → downward tile check on new screen

gravity_apply_rise:  jsr     apply_gravity ; apply gravity (slows rise)
        jsr     check_tile_horiz        ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        lda     $10                     ; bit 4 = solid tile?
        and     #$10                    ; isolate solid tile bit
        beq     gravity_airborne_exit   ; no solid → continue rising
        jsr     snap_y_to_ceil          ; snap to tile boundary (up)
gravity_snap_ceil:  jsr     reset_gravity ; reset velocity
gravity_airborne_exit:  clc             ; C=0: airborne
        rts                             ; return C=0 (in air)

; -----------------------------------------------
; update_collision_flags — update entity collision flags from tile check
; -----------------------------------------------
; $10 = tile collision result from check_tile_collision
; tile_at_feet_max = tile type from last collision (upper nibble of $BF00)
; Updates ent_flags bit 5 (on-ladder) based on tile type $60 (ladder).

update_collision_flags:  lda     $10    ; bit 4 = solid tile hit?
        and     #$10                    ; isolate solid tile bit
        bne     collision_flags_done    ; solid tile? keep flags unchanged
        lda     ent_flags,x             ; clear on-ladder flag (bit 5)
        and     #$DF                    ; mask off bit 5 (on-ladder)
        sta     ent_flags,x             ; store updated flags
        lda     tile_at_feet_max        ; check tile type at feet
        cmp     #$60                    ; $60 = ladder body tile type
        bne     collision_flags_done    ; not ladder → done
        lda     ent_flags,x             ; set on-ladder flag (bit 5)
        ora     #$20                    ; (used by OAM: behind-background priority)
        sta     ent_flags,x             ; store on-ladder flag
collision_flags_done:  rts              ; return

; moves sprite right by its X speeds
; parameters:
; X: sprite slot

move_sprite_right:  lda     ent_x_sub,x ; load X subpixel position
        clc                             ; X subpixel += X subpixel speed
        adc     ent_xvel_sub,x          ; add X subpixel velocity
        sta     ent_x_sub,x             ; store new X subpixel
        lda     ent_x_px,x              ; load X pixel position
        adc     ent_xvel,x              ; add X pixel velocity + carry
        sta     ent_x_px,x              ; store new X pixel
        bcc     move_sprite_right_no_wrap ; no page cross? done
        lda     ent_x_scr,x             ; load X screen
        adc     #$00                    ; increment X screen on overflow
        sta     ent_x_scr,x             ; store new X screen
move_sprite_right_no_wrap:  rts         ; return

; moves sprite left by its X speeds
; parameters:
; X: sprite slot

move_sprite_left:  lda     ent_x_sub,x ; load X subpixel position
        sec                             ; X subpixel -= X subpixel speed
        sbc     ent_xvel_sub,x          ; subtract X subpixel velocity
        sta     ent_x_sub,x             ; store new X subpixel
        lda     ent_x_px,x              ; load X pixel position
        sbc     ent_xvel,x              ; subtract X pixel velocity + borrow
        sta     ent_x_px,x              ; store new X pixel
        bcs     move_sprite_left_no_wrap ; no page cross? done
        lda     ent_x_scr,x             ; load X screen
        sbc     #$00                    ; decrement X screen on underflow
        sta     ent_x_scr,x             ; store new X screen
move_sprite_left_no_wrap:  rts          ; return

; moves sprite down by its Y speeds
; parameters:
; X: sprite slot

move_sprite_down:  lda     ent_y_sub,x ; load Y subpixel position
        clc                             ; Y subpixel += Y subpixel speed
        adc     ent_yvel_sub,x          ; add Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; load Y pixel position
        adc     ent_yvel,x              ; add Y pixel velocity + carry
        sta     ent_y_px,x              ; store new Y pixel
        cmp     #$F0                    ; past screen bottom ($F0)?
        bcc     move_sprite_down_screen ; Y < $F0? no screen wrap
        adc     #$0F                    ; wrap: add $10 to adjust Y
        sta     ent_y_px,x              ; store wrapped Y pixel
        inc     ent_y_scr,x             ; increment Y screen
move_sprite_down_screen:  rts           ; return

; moves sprite up by its Y speeds
; parameters:
; X: sprite slot

move_sprite_up:  lda     ent_y_sub,x ; load Y subpixel position
        sec                             ; Y subpixel += Y subpixel speed
        sbc     ent_yvel_sub,x          ; subtract Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; load Y pixel position
        sbc     ent_yvel,x              ; subtract Y pixel velocity + borrow
        sta     ent_y_px,x              ; store new Y pixel
        bcs     move_sprite_up_screen   ; no underflow? done
        sbc     #$0F                    ; wrap: subtract $10 to adjust Y
        sta     ent_y_px,x              ; store wrapped Y pixel
        dec     ent_y_scr,x             ; decrement Y screen
move_sprite_up_screen:  rts             ; return

; -----------------------------------------------
; apply_y_speed — apply Y velocity + $99 (no collision)
; -----------------------------------------------
; Dispatches to fall/rise velocity application based on sign,
; then applies $99. Used by entities without tile collision.

apply_y_speed:  lda     ent_yvel,x      ; Y velocity sign check
        bpl     apply_y_speed_rising    ; positive = rising
        jsr     apply_y_velocity_fall   ; falling: apply downward velocity
        jmp     apply_gravity           ; then apply gravity

apply_y_speed_rising:  jsr     apply_y_velocity_rise ; rising: apply upward velocity
        jmp     apply_gravity           ; then apply gravity

; -----------------------------------------------
; apply_y_velocity_fall — apply Y velocity when falling
; -----------------------------------------------
; Position -= velocity. When velocity is negative (falling),
; subtracting negative = adding to Y = moving down on screen.
; Handles downward screen wrap at Y=$F0.

apply_y_velocity_fall:  lda     ent_y_sub,x ; Y subpixel -= Y sub velocity
        sec                             ; prepare for subtraction
        sbc     ent_yvel_sub,x          ; subtract Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; Y pixel -= Y velocity
        sbc     ent_yvel,x              ; negative vel → subtracting neg = add
        sta     ent_y_px,x              ; store new Y pixel
        cmp     #$F0                    ; screen height = $F0
        bcc     apply_y_vel_fall_done   ; within screen? done
        adc     #$0F                    ; wrap: add $10 to next screen
        sta     ent_y_px,x              ; store wrapped Y pixel
        inc     ent_y_scr,x             ; increment Y screen
apply_y_vel_fall_done:  rts             ; return

; -----------------------------------------------
; apply_y_velocity_rise — apply Y velocity when rising
; -----------------------------------------------
; Position -= velocity. When velocity is positive (rising),
; subtracting positive = decreasing Y = moving up on screen.
; Handles upward screen wrap (underflow).

apply_y_velocity_rise:  lda     ent_y_sub,x ; Y subpixel -= Y sub velocity
        sec                             ; prepare for subtraction
        sbc     ent_yvel_sub,x          ; subtract Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; Y pixel -= Y velocity
        sbc     ent_yvel,x              ; positive vel → Y decreases (up)
        sta     ent_y_px,x              ; store new Y pixel
        bcs     apply_y_vel_rise_done   ; no underflow? done
        sbc     #$0F                    ; wrap: subtract $10 to prev screen
        sta     ent_y_px,x              ; store wrapped Y pixel
        dec     ent_y_scr,x             ; decrement Y screen
apply_y_vel_rise_done:  rts             ; return

; -----------------------------------------------
; apply_gravity — subtract $99 from Y velocity
; -----------------------------------------------
; Gravity ($99) subtracted from velocity each frame, making it
; more negative over time (falling faster). Clamps at terminal
; velocity $F9.00 (-7 px/frame). Player float timer ($B9) delays
; $99 while active.

apply_gravity:  cpx     #$00            ; player?
        bne     apply_gravity_player_check ; non-player → skip float check
        lda     $B9                     ; $B9 = player float timer
        beq     apply_gravity_player_check ; 0 = not floating, apply gravity
        dec     $B9                     ; decrement float timer
        bne     apply_gravity_exit      ; still floating? skip gravity
apply_gravity_player_check:  lda     ent_yvel_sub,x ; Y sub velocity -= gravity
        sec                             ; prepare for subtraction
        sbc     gravity                 ; subtract gravity value
        sta     ent_yvel_sub,x          ; store new Y sub velocity
        lda     ent_yvel,x              ; Y velocity -= borrow
        sbc     #$00                    ; propagate borrow to whole byte
        sta     ent_yvel,x              ; store new Y velocity
        bpl     apply_gravity_exit      ; still positive (rising)? done
        cmp     #$F9                    ; check against terminal velocity $F9
        bcs     apply_gravity_exit      ; within limit (-7 to -1)? done
        lda     ent_anim_id,x           ; check entity animation ID
        cmp     #$13                    ; $13 = exempt from terminal vel clamp
        beq     apply_gravity_exit      ; special entity → no clamp
        lda     #$F9                    ; clamp to terminal velocity
        sta     ent_yvel,x              ; clamp Y velocity to $F9.00
        lda     #$00                    ; clear sub velocity
        sta     ent_yvel_sub,x          ; clear Y sub velocity
apply_gravity_exit:  rts                ; return

; resets a sprite's $99/downward Y velocity
; parameters:
; X: sprite slot

reset_gravity:  cpx     #$00            ; $00 means player
        beq     reset_gravity_player    ; player → different rest velocity
        lda     #$AB                    ; enemy rest vel sub = $AB
        sta     ent_yvel_sub,x          ; store Y sub velocity
        lda     #$FF                    ; enemy rest vel = $FF ($FFAB = -0.33)
        sta     ent_yvel,x              ; store Y velocity whole byte
        rts                             ; entity rest vel = $FFAB

reset_gravity_player:  lda     #$C0     ; player rest vel sub = $C0
        sta     ent_yvel_sub,x          ; store Y sub velocity
        lda     #$FF                    ; player rest vel = $FF ($FFC0 = -0.25)
        sta     ent_yvel,x              ; store Y velocity whole byte
        rts                             ; player rest vel = $FFC0

; resets sprite's animation & sets ID
; parameters:
; A: OAM ID
; X: sprite slot

reset_sprite_anim:  sta     ent_anim_id,x ; store parameter -> OAM ID
        lda     #$00                    ; clear animation state
        sta     ent_anim_state,x        ; store cleared anim state
        lda     ent_anim_frame,x        ; load current anim frame
        and     #$80                    ; preserve damage flash bit (bit 7)
        sta     ent_anim_frame,x        ; reset tick counter, keep flash flag
        rts                             ; return

; -----------------------------------------------
; init_child_entity — initialize a new child entity in slot Y
; -----------------------------------------------
; Sets up a child entity spawned by parent entity in slot X.
; Inherits the parent's horizontal flip for facing direction.
;
; parameters:
;   X = parent entity slot
;   Y = child entity slot (from find_enemy_freeslot_y)
;   A = child entity shape/type value (stored to ent_anim_id,y)
; modifies:
;   ent_status,y = $80 (active)
;   ent_flags,y = parent's hflip | $90 (active + flags)
;   ent_y_scr,y = 0 (Y screen)
;   ent_anim_state,y = 0 (timer/counter cleared)
;   ent_anim_frame,y = bit 7 only preserved
; -----------------------------------------------

init_child_entity:  sta     ent_anim_id,y ; child shape/type
        lda     #$00                    ; clear anim state
        sta     ent_anim_state,y        ; clear child anim state
        sta     ent_y_scr,y             ; clear child Y screen
        lda     ent_flags,x             ; read parent flags
        and     #ENT_FLAG_HFLIP         ; isolate H-flip (bit 6)
        ora     #$90                    ; combine with active + child flags
        sta     ent_flags,y             ; store child flags
        lda     #$80                    ; status $80 = active
        sta     ent_status,y            ; activate child entity
        lda     ent_anim_frame,y        ; preserve only bit 7 of ent_anim_frame
        and     #$80                    ; isolate damage flash bit
        sta     ent_anim_frame,y        ; store cleared anim frame
        rts                             ; return

; faces a sprite toward the player
; parameters:
; X: sprite slot
face_player:

        lda     #FACING_RIGHT           ; start facing right
        sta     ent_facing,x            ; default = facing right
        lda     ent_x_px,x              ; entity X pixel
        sec                             ; subtract player X pixel
        sbc     ent_x_px                ; of player
        lda     ent_x_scr,x             ; entity X screen
        sbc     ent_x_scr               ; subtract player X screen
        bcc     face_player_exit        ; entity left of player → keep facing right
        lda     #FACING_LEFT            ; entity right of player → face left
        sta     ent_facing,x            ; store facing left
face_player_exit:  rts                  ; return

; -----------------------------------------------
; set_sprite_hflip — set horizontal flip from facing direction
; -----------------------------------------------
; Converts ent_facing,x bit 0 (facing right) into ent_flags,x bit 6
; (NES horizontal flip). Three ROR shifts move bit 0 → bit 6.
; Sprites face left by default; bit 6 set = flip to face right.
;
; parameters:
;   X = entity slot
; modifies:
;   ent_flags,x bit 6 = set if facing right (ent_facing,x bit 0)
; -----------------------------------------------
set_sprite_hflip:

        lda     ent_facing,x            ; load facing direction
        ror     a                       ; 3x ROR: bit 0 → bit 6
        ror     a                       ; (continued)
        ror     a                       ; (continued)
        and     #ENT_FLAG_HFLIP         ; isolate bit 6 (was bit 0)
        sta     temp_00                 ; save new hflip bit
        lda     ent_flags,x             ; clear old hflip, set new
        and     #$BF                    ; clear bit 6 (old hflip)
        ora     temp_00                 ; merge new hflip bit
        sta     ent_flags,x             ; store updated flags
        rts                             ; return

; submit a sound ID to global buffer for playing
; if full, do nothing
; parameters:
; A: sound ID

submit_sound_ID_D9:  sta     $D9        ; also store ID in $D9

; this version doesn't store in $D9
submit_sound_ID:  stx     temp_00       ; preserve X
        ldx     $DA                     ; X = current circular buffer index
        sta     $01                     ; sound ID -> $01 temp
        lda     $DC,x                   ; if current slot != $88
        cmp     #$88                    ; buffer FULL, return
        bne     submit_sound_id_exit    ; slot in use → buffer full
        lda     $01                     ; add sound ID to current
        sta     $DC,x                   ; buffer slot
        inx                             ; advance buffer index
        txa                             ; increment circular buffer index
        and     #$07                    ; wrap index at 8 slots
        sta     $DA                     ; store updated buffer index
submit_sound_id_exit:  ldx     temp_00  ; restore X register
        rts                             ; return

; -----------------------------------------------
; entity_y_dist_to_player — |player_Y - entity_Y|
; -----------------------------------------------
; parameters:
;   X = entity slot
; results:
;   A = abs(player_Y - entity[X]_Y) (pixel portion, single screen)
;   carry = set if player is below entity, clear if above
; -----------------------------------------------

entity_y_dist_to_player:  lda     ent_y_px ; player_Y - entity_Y
        sec                             ; prepare for subtraction
        sbc     ent_y_px,x              ; subtract entity Y pixel
        bcs     entity_distance_calc_return ; positive → player is below
        eor     #$FF                    ; negate: player is above entity
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is above
entity_distance_calc_return:  rts       ; A = abs(Y distance)

; -----------------------------------------------
; entity_x_dist_to_player — |player_X - entity_X|
; -----------------------------------------------
; 16-bit subtraction (pixel + screen), returns pixel portion only.
; parameters:
;   X = entity slot
; results:
;   A = abs(player_X - entity[X]_X) (pixel portion)
;   carry = set if player is to the right, clear if left
; -----------------------------------------------

entity_x_dist_to_player:  lda     ent_x_px ; player_X_pixel - entity_X_pixel
        sec                             ; prepare for subtraction
        sbc     ent_x_px,x              ; subtract entity X pixel
        pha                             ; save low byte
        lda     ent_x_scr               ; player_X_screen - entity_X_screen
        sbc     ent_x_scr,x             ; (borrow propagates from pixel sub)
        pla                             ; recover pixel difference
        bcs     entity_distance_x_setup ; positive → player is to the right
        eor     #$FF                    ; negate: player is to the left
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is left
entity_distance_x_setup:  rts           ; A = abs(X distance)

; -----------------------------------------------
; calc_direction_to_player — 16-direction angle from entity to player
; -----------------------------------------------
; Computes the direction from entity X to the player as a 16-step
; compass index (0-15). Used by entity AI for homing/tracking.
;
; Direction values (clockwise from north):
;   $00=N  $01=NNE $02=NE  $03=ENE $04=E  $05=ESE $06=SE  $07=SSE
;   $08=S  $09=SSW $0A=SW  $0B=WSW $0C=W  $0D=WNW $0E=NW  $0F=NNW
;
; Algorithm:
;   1. Compute abs Y and X distances, track quadrant in Y register:
;      bit 2 = player above entity, bit 1 = player left of entity
;   2. Sort distances: $01 = larger, $00 = smaller (bit 0 = swapped)
;   3. Compute sub-angle from slope ratio:
;      larger/4 >= smaller → 0 (nearly axial)
;      larger/2 >= smaller → 1 (moderate)
;      else               → 2 (nearly diagonal)
;   4. Look up final direction from table: [quadrant*4 + sub-angle]
;
; parameters:
;   X = entity slot
; results:
;   A = direction index (0-15)
; -----------------------------------------------
calc_direction_to_player:

        ldy     #$00                    ; Y = quadrant bits
        lda     ent_y_px                ; player_Y - entity_Y
        sec                             ; prepare for subtraction
        sbc     ent_y_px,x              ; subtract entity Y pixel
        ldy     #$00                    ; (clear Y again — assembler artifact)
        bcs     entity_distance_abs_y   ; player below or same → skip
        eor     #$FF                    ; negate: player is above
        adc     #$01                    ; two's complement = abs value
        ldy     #$04                    ; Y bit 2 = player above
entity_distance_abs_y:  sta     temp_00 ; $00 = abs(Y distance)
        lda     ent_x_px                ; player_X - entity_X (16-bit)
        sec                             ; prepare for subtraction
        sbc     ent_x_px,x              ; subtract entity X pixel
        pha                             ; save pixel difference
        lda     ent_x_scr               ; screen portion
        sbc     ent_x_scr,x             ; subtract entity X screen
        pla                             ; recover pixel difference
        bcs     entity_distance_abs_x   ; player right or same → skip
        eor     #$FF                    ; negate: player is left
        adc     #$01                    ; two's complement = abs value
        iny                             ; Y bit 1 = player left
        iny                             ; (INY x2 = add 2 to quadrant)
entity_distance_abs_x:  sta     $01     ; $01 = abs(X distance)
        cmp     temp_00                 ; if X_dist >= Y_dist, no swap needed
        bcs     entity_distance_sub_angle ; X dominant → no swap
        pha                             ; swap so $01=larger, $00=smaller
        lda     temp_00                 ; load Y distance (smaller)
        sta     $01                     ; $01 = Y distance (now larger)
        pla                             ; recover X distance
        sta     temp_00                 ; $00 = X distance (now smaller)
        iny                             ; Y bit 0 = axes swapped (Y dominant)
entity_distance_sub_angle:  lda     #$00 ; $02 = sub-angle (0-2)
        sta     $02                     ; init sub-angle = 0
        lda     $01                     ; larger_dist / 4
        lsr     a                       ; divide by 2
        lsr     a                       ; divide by 2 again (total /4)
        cmp     temp_00                 ; if larger/4 >= smaller → nearly axial
        bcs     entity_distance_table_index ; nearly axial → sub-angle 0
        inc     $02                     ; sub-angle = 1 (moderate)
        asl     a                       ; larger/4 * 2 = larger/2
        cmp     temp_00                 ; if larger/2 >= smaller → moderate
        bcs     entity_distance_table_index ; moderate → sub-angle 1
        inc     $02                     ; sub-angle = 2 (nearly diagonal)
entity_distance_table_index:  tya       ; table index = quadrant * 4 + sub-angle
        asl     a                       ; quadrant * 2
        asl     a                       ; quadrant * 4
        clc                             ; prepare for addition
        adc     $02                     ; + sub-angle
        tay                             ; index → Y
        lda     direction_to_player_table,y ; look up 16-dir facing value
        rts                             ; return direction in A
; ---------------------------------------------------------------------------
; direction_to_player_table — 16-direction lookup by quadrant and sub-angle
; ---------------------------------------------------------------------------
; Index = quadrant * 4 + sub_angle. 8 quadrants × 4 entries (sub_angle 0-2
; used, entry 3 unused). Returns direction 0-15 (0=right, 4=up, 8=left, 12=down).
direction_to_player_table:
        .byte   $04,$05,$06,$04         ; quadrant 0
        .byte   $08,$07,$06,$04         ; quadrant 1
        .byte   $0C,$0B,$0A,$04         ; quadrant 2
        .byte   $08,$09,$0A,$04         ; quadrant 3
        .byte   $04,$03,$02,$04         ; quadrant 4
        .byte   $00,$01,$02,$04         ; quadrant 5
        .byte   $0C,$0D,$0E,$04         ; quadrant 6
        .byte   $00,$0F,$0E,$04         ; quadrant 7
; ---------------------------------------------------------------------------
; track_direction_to_player — rotate ent_facing 1 step toward player
; ---------------------------------------------------------------------------
; Called by entity AI (e.g. Chibee). Gets the 16-direction target toward the
; player, then adjusts ent_facing,x by +1 or -1 each call.
; Output: ent_facing,x updated, temp_00 = target direction.
track_direction_to_player:
        jsr     calc_direction_to_player ; get 16-dir target → A
        sta     temp_00                 ; save target direction
        lda     ent_facing,x            ; signed circular difference:
        clc                             ; (current + 8 - target) & $0F - 8
        adc     #$08                    ; offset by 8 to center range
        and     #$0F                    ; wrap to 0-15
        sec                             ; prepare for subtraction
        sbc     temp_00                 ; subtract target
        and     #$0F                    ; wrap to 0-15
        sec                             ; prepare for subtraction
        sbc     #$08                    ; remove offset → signed diff (-7..+8)
        beq     entity_direction_done   ; diff=0: already facing target
        bcs     entity_direction_dec    ; diff>0: target is CCW → turn CW (DEC)
        inc     ent_facing,x            ; diff<0: target is CW → turn CCW (INC)
        bne     entity_direction_wrap   ; (always branches)
entity_direction_dec:  dec     ent_facing,x ; turn CW (decrement)
entity_direction_wrap:  lda     ent_facing,x ; wrap direction to 0-15
        and     #$0F                    ; mask to 0-15 range
        sta     ent_facing,x            ; store wrapped facing
entity_direction_done:  rts             ; return

; -----------------------------------------------
; check_platform_vertical — check if player stands on a platform entity
; -----------------------------------------------
; Scans all entity slots ($1F→$01) for platform entities that
; the player (slot 0) is standing on. Uses hitbox tables at
; $FB3B (half-heights) and $FB5B (half-widths) indexed by
; the entity's shape (ent_hitbox & $1F).
;
; ent_flags flag bits for platform entities:
;   bit 7 = active/collidable
;   bit 2 = disabled (skip if set)
;   bit 1 = horizontal platform flag
;   bit 0 = vertical platform flag (can stand on)
;
; parameters:
;   X = entity slot (must be player slot 0)
; results:
;   carry = set if standing on platform, clear if not
;   $11 = vertical overlap (clamped to max 8), used as push distance
;   $5D = platform entity slot
;   $36/$37/$38 = platform velocity data (if entity type $14)
; -----------------------------------------------

check_platform_vertical:  lda     ent_flags,x ; entity not active? bail
        bpl     entity_plat_not_found   ; bit 7 clear = inactive, bail
        sty     temp_00                 ; save caller's Y
        ldy     #$1F                    ; start scanning from slot $1F
        sty     $01                     ; store current scan slot
entity_plat_empty_check:  lda     ent_status,y ; entity type empty? skip
        bpl     entity_plat_dec         ; bit 7 clear = inactive, skip
        lda     ent_flags,y             ; entity not active? skip
        bpl     entity_plat_dec         ; bit 7 clear = inactive, skip
        lda     ent_flags,y             ; bit 2 set = disabled? skip
        and     #$04                    ; check disabled bit
        bne     entity_plat_dec         ; disabled → skip
        lda     ent_flags,y             ; bits 0-1 = platform flags
        and     #$03                    ; neither set? skip
        beq     entity_plat_dec         ; neither set? skip
        and     #$01                    ; bit 0 = vertical platform?
        beq     entity_plat_distance    ; not set → check as horiz-only
        lda     ent_yvel,x              ; player Y velocity (falling = negative)
        bpl     entity_plat_dec         ; positive = rising, can't land → skip
entity_plat_distance:  jsr     player_x_distance ; $10 = |player_X - entity_X
        jsr     player_y_distance       ; $11 = |player_Y - entity_Y
        bcc     entity_plat_hitbox      ; player above entity? check hitbox
        lda     ent_flags,y             ; player below + bit0 (vert platform)?
        and     #$01                    ; can't land from below
        bne     entity_plat_dec         ; vert platform → skip (below)
entity_plat_hitbox:  lda     ent_hitbox,y ; shape index = ent_hitbox & $1F
        and     #$1F                    ; mask to hitbox ID
        tay                             ; Y = hitbox table index
        lda     $10                     ; X distance >= half-width? no overlap
        cmp     hitbox_mega_man_widths,y ; compare against hitbox width
        bcs     entity_plat_dec         ; outside width → skip
        sec                             ; Y overlap = half-height - Y distance
        lda     hitbox_mega_man_heights,y ; load hitbox half-height
        sbc     $11                     ; subtract Y distance
        bcc     entity_plat_dec         ; negative? no Y overlap
        sta     $11                     ; $11 = overlap amount
        cmp     #$08                    ; clamp to max 8 pixels
        bcc     entity_plat_slot_save   ; overlap < 8? no clamp needed
        lda     #$08                    ; clamp overlap to max 8
        sta     $11                     ; store clamped overlap
entity_plat_slot_save:  ldy     $01     ; Y = platform entity slot
        lda     ent_routine,y           ; entity routine == $14 (moving platform)?
        cmp     #$14                    ; check for moving platform type
        bne     entity_plat_slot_store  ; not $14? skip velocity copy
        lda     ent_facing,y            ; copy platform velocity to player vars
        sta     $36                     ; $36 = platform direction/speed
        lda     ent_xvel_sub,y          ; $37 = platform X velocity
        sta     $37                     ; store platform X sub velocity
        lda     ent_xvel,y              ; platform X velocity whole byte
        sta     $38                     ; $38 = platform X velocity
entity_plat_slot_store:  sty     $5D    ; $5D = platform entity slot
        ldy     temp_00                 ; restore caller's Y
        sec                             ; C=1: standing on platform
        rts                             ; return C=1

entity_plat_dec:  dec     $01           ; next slot (decrement $1F→$01)
        ldy     $01                     ; load next slot index
        bne     entity_plat_empty_check ; slot 0 = player, stop there
        ldy     temp_00                 ; restore caller's Y
entity_plat_not_found:  clc             ; C=0: not on any platform
        rts                             ; return C=0

; -----------------------------------------------
; check_platform_horizontal — check if player is pushed by a platform entity
; -----------------------------------------------
; Scans all entity slots ($1F→$01) for horizontal platform entities
; that overlap the player. Uses hitbox tables at $FB3B (half-heights)
; and $FB5B (half-widths). Unlike vertical check, this only requires
; bit 1 (horizontal flag) and does not check Y velocity direction.
;
; parameters:
;   X = entity slot (must be player slot 0)
; results:
;   carry = set if overlapping platform, clear if not
;   $10 = horizontal overlap (clamped to max 8), used as push distance
;   $5D = platform entity slot (shape index, not original slot)
; -----------------------------------------------

check_platform_horizontal:  lda     ent_flags,x ; entity not active? bail
        bpl     entity_plat_search_2_not_found ; bit 7 clear = not active, bail
        sty     temp_00                 ; save caller's Y
        ldy     #$1F                    ; start scanning from slot $1F
        sty     $01                     ; save current scan slot
entity_plat_search_2_check:  lda     ent_status,y ; entity type empty? skip
        bpl     entity_plat_search_2_dec ; bit 7 clear = inactive, skip
        lda     ent_flags,y             ; bit 2 set = disabled? skip
        and     #$04                    ; check bit 2 (disabled)
        bne     entity_plat_search_2_dec ; bit 2 set = disabled, skip
        lda     ent_flags,y             ; bit 1 = horizontal platform?
        and     #$02                    ; not set? skip
        beq     entity_plat_search_2_dec ; not horizontal platform → skip
        jsr     player_x_distance       ; $10 = |player_X - entity_X|
        jsr     player_y_distance       ; $11 = |player_Y - entity_Y|
        lda     ent_hitbox,y            ; shape index = ent_hitbox & $1F
        and     #$1F                    ; mask to hitbox ID
        tay                             ; Y = hitbox table index
        lda     $11                     ; Y distance >= half-height? no overlap
        cmp     hitbox_mega_man_heights,y ; compare against hitbox height
        bcs     entity_plat_search_2_dec ; outside height → skip
        sec                             ; X overlap = half-width - X distance
        lda     hitbox_mega_man_widths,y ; load hitbox half-width
        sbc     $10                     ; subtract X distance
        bcc     entity_plat_search_2_dec ; negative? no X overlap
        sta     $10                     ; $10 = overlap amount
        cmp     #$08                    ; clamp to max 8 pixels
        bcc     entity_plat_shape_save  ; overlap < 8? no clamp needed
        lda     #$08                    ; clamp overlap to max 8
        sta     $10                     ; store clamped overlap
entity_plat_shape_save:  sty     $5D    ; $5D = platform shape index
        ldy     temp_00                 ; restore caller's Y
        sec                             ; C=1: overlapping platform
        rts                             ; return C=1

entity_plat_search_2_dec:  dec     $01  ; next slot (decrement $1F→$01)
        ldy     $01                     ; load next slot index
        bne     entity_plat_search_2_check ; continue scanning
        ldy     temp_00                 ; restore caller's Y
entity_plat_search_2_not_found:  clc    ; C=0: not overlapping any platform
        rts                             ; return C=0

; -----------------------------------------------
; player_x_distance — |player_X - entity_X|
; -----------------------------------------------
; Computes absolute X distance between player (slot 0) and entity Y.
; parameters:
;   Y = entity slot
; results:
;   $10 = abs(player_X - entity[Y]_X) (pixel portion)
;   carry = set if player is to the right, clear if left
; -----------------------------------------------

player_x_distance:  lda     ent_x_px    ; player_X_pixel - entity_X_pixel
        sec                             ; prepare for subtraction
        sbc     ent_x_px,y              ; subtract entity X pixel
        pha                             ; save pixel difference
        lda     ent_x_scr               ; player_X_screen - entity_X_screen
        sbc     ent_x_scr,y             ; borrow from pixel subtract
        pla                             ; recover pixel difference
        bcs     entity_dist_x_setup_2   ; player >= entity? already positive
        eor     #$FF                    ; negate: entity is right of player
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is to the left
entity_dist_x_setup_2:  sta     $10     ; $10 = abs X distance
        rts                             ; return

; -----------------------------------------------
; player_y_distance — |player_Y - entity_Y|
; -----------------------------------------------
; Computes absolute Y distance between player (slot 0) and entity Y.
; parameters:
;   Y = entity slot
; results:
;   $11 = abs(player_Y - entity[Y]_Y) (pixel portion)
;   carry = set if player is below entity, clear if above
; -----------------------------------------------

player_y_distance:  lda     ent_y_px    ; player_Y - entity_Y
        sec                             ; prepare for subtraction
        sbc     ent_y_px,y              ; subtract entity Y pixel
        bcs     entity_dist_y_setup_2   ; player >= entity? positive
        eor     #$FF                    ; negate: player is above entity
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is above
entity_dist_y_setup_2:  sta     $11     ; $11 = abs Y distance
        rts                             ; return

; -----------------------------------------------
; platform_push_x_left — push entity X left by $10
; -----------------------------------------------
; Subtracts $10 from entity[X] X position, then compares
; against reference point $02/$03 to determine side.
; results: carry = set if entity is left of $02/$03
; -----------------------------------------------

platform_push_x_left:  sec              ; entity_X -= $10 (push left)
        lda     ent_x_px,x              ; load entity X pixel
        sbc     $10                     ; subtract push distance
        sta     ent_x_px,x              ; store updated X pixel
        lda     ent_x_scr,x             ; (16-bit: X screen)
        sbc     #$00                    ; propagate borrow to screen byte
        sta     ent_x_scr,x             ; store updated X screen
        jmp     compare_x               ; compare against reference point

; -----------------------------------------------
; platform_push_x_right — push entity X right by $10
; -----------------------------------------------

platform_push_x_right:  clc             ; entity_X += $10 (push right)
        lda     ent_x_px,x              ; load entity X pixel
        adc     $10                     ; add push distance
        sta     ent_x_px,x              ; store updated X pixel
        lda     ent_x_scr,x             ; (16-bit: X screen)
        adc     #$00                    ; propagate carry to screen byte
        sta     ent_x_scr,x             ; store updated X screen
compare_x:  sec                         ; compare: $02/$03 - entity_X
        lda     $02                     ; load pre-move X pixel
        sbc     ent_x_px,x              ; C=1: entity left of ref point
        lda     $03                     ; load pre-move X screen
        sbc     ent_x_scr,x             ; propagate borrow for 16-bit compare
        rts                             ; return with C = comparison result

; -----------------------------------------------
; platform_push_y_up — push entity Y up by $11
; -----------------------------------------------
; Subtracts $11 from entity[X] Y position, with screen
; underflow handling. Then compares against $02/$03.
; results: carry = set if entity is above $02/$03
; -----------------------------------------------

platform_push_y_up:  sec                ; entity_Y -= $11 (push up)
        lda     ent_y_px,x              ; load entity Y pixel
        sbc     $11                     ; subtract push distance
        sta     ent_y_px,x              ; store updated Y pixel
        bcs     compare_y               ; no underflow? skip
        dec     ent_y_scr,x             ; underflow: decrement Y screen
        jmp     compare_y               ; compare against reference point

; -----------------------------------------------
; platform_push_y_down — push entity Y down by $11
; -----------------------------------------------
; Adds $11 to entity[X] Y position, with $F0 screen
; overflow handling. Then compares against $02/$03.
; results: carry = set if entity is above $02/$03
; -----------------------------------------------

platform_push_y_down:  clc              ; entity_Y += $11 (push down)
        lda     ent_y_px,x              ; load entity Y pixel
        adc     $11                     ; add push distance
        sta     ent_y_px,x              ; store updated Y pixel
        bcs     entity_y_screen_inc     ; carry? wrapped past $FF
        cmp     #$F0                    ; below screen bottom ($F0)?
        bcc     compare_y               ; no → done
        adc     #$0F                    ; wrap Y: $F0+$0F+C = next screen
        sta     ent_y_px,x              ; store wrapped Y pixel
entity_y_screen_inc:  inc     ent_y_scr,x ; increment Y screen
compare_y:  sec                         ; compare: $02/$03 - entity_Y
        lda     $02                     ; load pre-move Y pixel
        sbc     ent_y_px,x              ; C=1: entity above ref point
        lda     $03                     ; load pre-move screen
        sbc     ent_y_scr,x             ; propagate borrow for 16-bit compare
        rts                             ; return with C = comparison result

; tests if a sprite is colliding
; with the player (Mega Man)
; parameters:
; X: sprite slot to check player collision for
; returns:
; Carry flag off = collision, on = no collision
check_player_collision:

        lda     player_state            ; load current player state
        cmp     #PSTATE_DEATH           ; is player dead
        beq     entity_hitbox_done      ; dead? skip collision
        cmp     #PSTATE_REAPPEAR        ; or reappearing?
        beq     entity_hitbox_done      ; reappearing? skip collision
        sec                             ; set carry (will return C=1 if no collision)
        lda     ent_flags,x             ; load entity flags
        bpl     entity_hitbox_done      ; if not active (bit 7 clear)
        and     #$04                    ; or disabled (bit 2 set)
        bne     entity_hitbox_done      ; disabled? skip collision
check_player_collision_hitbox:
        lda     ent_hitbox,x            ; load entity hitbox ID
        and     #$1F                    ; y = hitbox ID
        tay                             ; Y = hitbox table index
        lda     hitbox_mega_man_heights,y ; Mega Man hitbox height
        sta     temp_00                 ; -> $00
        lda     ent_anim_id             ; player anim ID
        cmp     #$10                    ; $10 = sliding?
        bne     entity_hitbox_x_load    ; not sliding? skip height adjust
        lda     temp_00                 ; adjust hitbox height
        sec                             ; by subtracting 8
        sbc     #$08                    ; reduce hitbox by 8 for slide
        sta     temp_00                 ; store reduced height
entity_hitbox_x_load:  lda     ent_x_px ; player X pixel
        sec                             ; player X - entity X
        sbc     ent_x_px,x              ; pixel difference
        pha                             ; save pixel result
        lda     ent_x_scr               ; taking screen into account
        sbc     ent_x_scr,x             ; via carry
        pla                             ; then take absolute value
        bcs     entity_hitbox_width_cmp ; positive → player is right
        eor     #$FF                    ; negate if player is left
        adc     #$01                    ; two's complement
entity_hitbox_width_cmp:  cmp     hitbox_mega_man_widths,y ; if abs(X delta) > hitbox X delta
        bcs     entity_hitbox_done      ; outside width? no collision
        lda     ent_y_px                ; player Y pixel
        sec                             ; get Y delta between
        sbc     ent_y_px,x              ; the two sprites
        bcs     entity_hitbox_return    ; player below? already positive
        eor     #$FF                    ; negate if player is above
        adc     #$01                    ; two's complement
entity_hitbox_return:  .byte   $C5,$00,$90,$00 ; CMP $00 / BCC +0 (compare Y dist vs height)
entity_hitbox_done:  .byte   $60        ; RTS

; sprite hitbox heights for Mega Man collision
; the actual height is double this, cause it compares delta
hitbox_mega_man_heights:  .byte   $13,$1C,$18,$14,$1C,$28,$16,$1C
        .byte   $18,$18,$1C,$10,$24,$24,$34,$14
        .byte   $20,$0E,$1C,$1C,$3C,$1C,$2C,$14
        .byte   $2C,$2C,$14,$34,$0C,$0C,$0C,$0C

; sprite hitbox widths for Mega Man collision
; the actual width is double this, cause it compares delta
hitbox_mega_man_widths:  .byte   $0F,$14,$14,$14,$10,$20,$18,$14
        .byte   $10,$18,$18,$0C,$14,$20,$10,$18
        .byte   $1C,$14,$40,$0C,$0C,$0F,$0C,$10
        .byte   $28,$18,$28
        bit     $0808                   ; data (hitbox table padding)
        php                             ; data (hitbox table padding)
        php                             ; data (hitbox table padding)

; loops through all 3 weapon slots
; to check collision against each one
; for a passed in sprite slot
; parameters:
; X: sprite slot to check weapon collision for
; returns:
; Carry flag off = sprite is colliding with player's weapons, on = not
; $10: sprite slot of weapon collided with (if carry off)
check_sprite_weapon_collision:
        lda     player_state            ; check player state
        cmp     #PSTATE_DEATH           ; is player dead
        beq     entity_anim_collision_detected ; dead? skip collision
        cmp     #PSTATE_REAPPEAR        ; or reappearing?
        beq     entity_anim_collision_detected ; reappearing? skip collision
        lda     #$03                    ; start loop through
        sta     $10                     ; all 3 weapon slots
entity_anim_advance_loop:  ldy     $10  ; Y = current weapon slot
        lda     ent_status,y            ; weapon inactive?
        bpl     entity_anim_loop_counter ; not active (bit 7 clear)? skip
        lda     ent_flags,y             ; check weapon flags
        bpl     entity_anim_loop_counter ; not active? skip
        lda     ent_routine,y           ; check weapon routine
        cmp     #$0F                    ; or routine $0F (item pickup dying)
        beq     entity_anim_loop_counter ; dying item? skip
        lda     ent_anim_id,y           ; load weapon animation ID
        cmp     #$13                    ; or OAM ID $13
        beq     entity_anim_loop_counter ; excluded OAM ID? skip
        cmp     #$D7                    ; or $D7
        beq     entity_anim_loop_counter ; excluded OAM ID $D7? skip
        cmp     #$D8                    ; or $D8
        beq     entity_anim_loop_counter ; excluded OAM ID $D8? skip
        cmp     #$D9                    ; or $D9
        beq     entity_anim_loop_counter ; excluded OAM ID? skip
        lda     ent_flags,x             ; check target entity flags
        bpl     entity_anim_loop_counter ; not active? skip
        and     #$04                    ; then don't check collision
        bne     entity_anim_loop_counter ; disabled? skip collision
        lda     ent_x_px,y              ; weapon X pixel
        sta     temp_00                 ; $00 = weapon X pixel
        lda     ent_x_scr,y             ; weapon X screen
        sta     $01                     ; $01 = weapon X screen
        lda     ent_y_px,y              ; weapon Y pixel
        sta     $02                     ; $02 = weapon Y pixel
        jsr     check_weapon_collision  ; carry cleared == collision
        bcc     entity_anim_collision_return ; collision detected? return
entity_anim_loop_counter:  dec     $10  ; continue loop,
        bne     entity_anim_advance_loop ; stop at $00 (Mega Man)
entity_anim_collision_detected:  sec    ; no collision: set carry
entity_anim_collision_return:  rts      ; return

; parameters:
; X: sprite slot to check collision for
; $00: comparison sprite's X position
; $01: comparison sprite's X screen
; $02: comparison sprite's Y position
; returns:
; Carry flag off = collision, on = no collision

check_weapon_collision:  sec            ; set carry (no collision default)
        lda     ent_hitbox,x            ; get hitbox shape index
        and     #$1F                    ; mask to 5 bits
        tay                             ; Y = hitbox table index
        lda     temp_00                 ; weapon X pixel
        sec                             ; weapon X - entity X
        sbc     ent_x_px,x              ; pixel difference
        pha                             ; save result
        lda     $01                     ; weapon X screen
        sbc     ent_x_scr,x             ; subtract entity X screen
        pla                             ; recover pixel difference
        bcs     entity_anim_weapon_width ; positive? already absolute
        eor     #$FF                    ; negate for absolute value
        adc     #$01                    ; two's complement
entity_anim_weapon_width:  cmp     hitbox_weapon_widths,y ; if abs(X delta) > hitbox X delta
        bcs     entity_anim_collision_done ; outside width? no collision
        lda     $02                     ; weapon Y pixel
        sec                             ; weapon Y - entity Y
        sbc     ent_y_px,x              ; pixel difference
        bcs     entity_anim_weapon_height ; positive? already absolute
        eor     #$FF                    ; negate for absolute value
        adc     #$01                    ; two's complement
entity_anim_weapon_height:  cmp     hitbox_weapon_heights,y ; compare abs(Y) vs hitbox height
        .byte   $90,$00                 ; bcc = collision (carry clear)
entity_anim_collision_done:  .byte   $60 ; rts

; sprite hitbox heights for weapon collision
; the actual height is double this, cause it compares delta
hitbox_weapon_heights:  .byte   $0A,$12,$0E,$0A,$12,$1E,$0C,$16
        .byte   $0E,$0E,$12,$04,$1A,$1A,$2A,$0A
        .byte   $16,$04,$12,$2E,$32,$12,$22,$0A
        .byte   $22,$06,$02,$2A,$02,$02,$02,$02

; sprite hitbox widths for weapon collision
; the actual width is double this, cause it compares delta
hitbox_weapon_widths:  .byte   $0B,$0F,$0D,$0F,$0B,$13,$13,$13
        .byte   $0B,$13,$13,$05,$0F,$1B,$0B,$13
        .byte   $17,$0F,$13,$07,$07,$0A,$07,$0B
        .byte   $23,$0F,$03
        .byte   $27
        .byte   $03
        .byte   $03
        .byte   $03
        .byte   $03

; find free sprite slot routine, return in X register
; searches sprite state table ent_status (enemy slots)
; for free slots (inactive)
; returns:
; Carry flag off = slot found, on = not found
; X: next free slot # (if carry off)
find_enemy_freeslot_x:
        ldx     #$1F                    ; start looping from slot $1F
entity_freeslot_x_check:  lda     ent_status,x ; check sprite state sign bit
        bpl     entity_freeslot_x_found ; off means inactive, return
        dex                             ; next slot (down)
        cpx     #$0F                    ; $00-$0F slots not for enemies
        bne     entity_freeslot_x_check ; so stop there
        sec                             ; return C=1
        rts                             ; for no slots found

entity_freeslot_x_found:  clc           ; return C=0 slot found
        rts                             ; return C=0 (slot found)

; find free sprite slot routine, return in Y register
; searches sprite state table ent_status (enemy slots)
; for free slots (inactive)
; returns:
; Carry flag off = slot found, on = not found
; Y: next free slot # (if carry off)

find_enemy_freeslot_y:  ldy     #$1F    ; start looping from slot $1F
entity_freeslot_y_check:  lda     ent_status,y ; check sprite state sign bit
        bpl     entity_freeslot_y_found ; off means inactive, return
        dey                             ; next slot (down)
        cpy     #$0F                    ; $00-$0F slots not for enemies
        bne     entity_freeslot_y_check ; so stop there
        sec                             ; return C=1
        rts                             ; for no slots found

entity_freeslot_y_found:  clc           ; return C=0 slot found
        rts                             ; return C=0 (slot found)

; -----------------------------------------------
; calc_homing_velocity — proportional X/Y velocity toward player
; -----------------------------------------------
; Computes entity velocity that points at the player with the
; given speed magnitude. The dominant axis (larger distance) gets
; full speed; the minor axis gets proportional speed:
;   minor_vel = speed * minor_dist / major_dist
;
; Uses two successive divide_16bit calls to compute this ratio.
;
; parameters:
;   X = entity slot
;   $03.$02 = speed magnitude (8.8 fixed-point, whole.sub)
; results:
;   ent_xvel,x.ent_xvel_sub,x = X velocity (8.8 fixed-point)
;   ent_yvel,x.ent_yvel_sub,x = Y velocity (8.8 fixed-point)
;   $0C = direction bitmask:
;     bit 0 (FACING_RIGHT) = player is right
;     bit 1 (FACING_LEFT)  = player is left
;     bit 2 ($04) = player is below
;     bit 3 ($08) = player is above
;   Callers typically store $0C into ent_facing,x and use it
;   to select move_sprite_right/left/up/down.
; -----------------------------------------------
calc_homing_velocity:

        jsr     entity_x_dist_to_player ; A = |x_dist|, C = player right
        sta     $0A                     ; $0A = x_distance
        lda     #$01                    ; $01 = right
        bcs     entity_dir_x_store      ; player is right? use $01
        lda     #$02                    ; $02 = left
entity_dir_x_store:  sta     temp_0C    ; $0C = X direction bit
        jsr     entity_y_dist_to_player ; A = |y_dist|, C = player below
        sta     $0B                     ; $0B = y_distance
        lda     #$04                    ; $04 = below
        bcs     entity_dir_combine      ; player is below? use $04
        lda     #$08                    ; $08 = above
entity_dir_combine:  ora     temp_0C    ; combine X and Y direction bits
        sta     temp_0C                 ; store combined direction bits
        lda     $0B                     ; compare y_dist vs x_dist
        cmp     $0A                     ; compare Y dist vs X dist
        bcs     entity_divide_y_vel     ; y_dist >= x_dist? Y dominant
        lda     $02                     ; X gets full speed
        sta     ent_xvel_sub,x          ; X vel sub = speed sub
        lda     $03                     ; X vel whole = speed whole
        sta     ent_xvel,x              ; X vel whole = speed whole
        lda     $0A                     ; divisor = x_distance
        sta     $01                     ; $01 = x_distance
        lda     #$00                    ; clear low byte
        sta     temp_00                 ; clear low byte of dividend
        jsr     divide_16bit            ; scale = (x_dist << 16) / speed
        lda     $04                     ; use scale factor as new divisor
        sta     $02                     ; use quotient as new divisor lo
        lda     $05                     ; load quotient high
        sta     $03                     ; use quotient as new divisor hi
        lda     $0B                     ; dividend = y_distance
        sta     $01                     ; $01 = y_distance
        lda     #$00                    ; clear low byte
        sta     temp_00                 ; clear low byte of dividend
        jsr     divide_16bit            ; result = speed * y_dist / x_dist
        lda     $04                     ; Y gets proportional speed
        sta     ent_yvel_sub,x          ; Y vel sub
        lda     $05                     ; load quotient high byte
        sta     ent_yvel,x              ; Y vel whole
        rts                             ; return

entity_divide_y_vel:  lda     $02       ; Y gets full speed
        sta     ent_yvel_sub,x          ; Y vel sub = speed sub
        lda     $03                     ; Y vel whole = speed whole
        sta     ent_yvel,x              ; Y vel whole = speed whole
        lda     $0B                     ; divisor = y_distance
        sta     $01                     ; $01 = y_distance
        lda     #$00                    ; clear low byte of dividend
        sta     temp_00                 ; store cleared low byte
        jsr     divide_16bit            ; compute scale factor
        lda     $04                     ; use scale factor as new divisor
        sta     $02                     ; use quotient as new divisor lo
        lda     $05                     ; load quotient high
        sta     $03                     ; use quotient as new divisor hi
        lda     $0A                     ; dividend = x_distance
        sta     $01                     ; $01 = x_distance
        lda     #$00                    ; clear low byte of dividend
        sta     temp_00                 ; store cleared low byte
        jsr     divide_16bit            ; compute proportional velocity
        lda     $04                     ; X gets proportional speed
        sta     ent_xvel_sub,x          ; X vel sub
        lda     $05                     ; load quotient high byte
        sta     ent_xvel,x              ; X vel whole
        rts                             ; return

; -----------------------------------------------
; divide_8bit — 8-bit restoring division
; -----------------------------------------------
; Computes ($00 * 256) / $01 using shift-and-subtract.
; Effectively returns the ratio $00/$01 as a 0.8 fixed-point
; fraction, useful when $00 < $01.
;
; parameters:
;   $00 = dividend (8-bit)
;   $01 = divisor (8-bit)
; results:
;   $02 = quotient (= $00 * 256 / $01, 8-bit)
;   $03 = remainder
; -----------------------------------------------
divide_8bit:

        lda     #$00                    ; clear quotient
        sta     $02                     ; and remainder
        sta     $03                     ; clear remainder
        lda     temp_00                 ; if both inputs zero,
        ora     $01                     ; return 0
        bne     entity_divide_loop_setup ; nonzero? proceed to division
        sta     $02                     ; store zero quotient
        rts                             ; return (all zero)

entity_divide_loop_setup:  ldy     #$08 ; 8 iterations (8-bit quotient)
entity_divide_shift_left:  asl     $02  ; shift $03:$00:$02 left
        rol     temp_00                 ; shift dividend left into remainder
        rol     $03                     ; shift remainder left
        sec                             ; try remainder - divisor
        lda     $03                     ; load remainder
        sbc     $01                     ; try subtract divisor
        bcc     entity_divide_loop_counter ; divisor doesn't fit, skip
        sta     $03                     ; update remainder
        inc     $02                     ; set quotient bit
entity_divide_loop_counter:  dey        ; next iteration
        bne     entity_divide_shift_left ; loop 8 times
        rts                             ; return

; -----------------------------------------------
; divide_16bit — 16-bit restoring division
; -----------------------------------------------
; Computes ($01:$00 << 16) / ($03:$02) using shift-and-subtract.
; The dividend is placed in byte position 2 of the 32-bit shift
; chain ($07:$01:$00:$06), so the result is effectively:
;   ($01 * 65536) / ($03:$02)  [when $00 = 0, as typical]
;
; Used by calc_homing_velocity: two successive calls compute
; speed * minor_dist / major_dist for proportional velocity.
;
; parameters:
;   $01:$00 = dividend (16-bit, usually $01=value, $00=0)
;   $03:$02 = divisor (16-bit, 8.8 fixed-point speed)
; results:
;   $05:$04 = quotient (16-bit), $05=high, $04=low
; preserves:
;   X register (saved/restored via $09)
; -----------------------------------------------

divide_16bit:  lda     #$00             ; clear quotient accumulator
        sta     $06                     ; and remainder high byte
        sta     $07                     ; clear remainder high
        lda     temp_00                 ; if all inputs zero,
        ora     $01                     ; return 0
        ora     $02                     ; check if divisor nonzero
        ora     $03                     ; (continued)
        bne     entity_divide_temp_save ; nonzero? proceed to division
        sta     $04                     ; quotient low = 0
        sta     $05                     ; quotient high = 0
        rts                             ; return (all zero)

entity_divide_temp_save:  stx     $09   ; save X (used as temp)
        ldy     #$10                    ; 16 iterations (16-bit quotient)
entity_divide_shift_chain:  asl     $06 ; shift 32-bit chain left:
        rol     temp_00                 ; $07:$01:$00:$06
        rol     $01                     ; (remainder ← dividend ← quotient)
        rol     $07                     ; shift remainder high byte
        sec                             ; try $07:$01 - $03:$02
        lda     $01                     ; (remainder - divisor)
        sbc     $02                     ; low byte - divisor low
        tax                             ; X = low byte of difference
        lda     $07                     ; high byte - divisor high
        sbc     $03                     ; high byte - divisor high
        bcc     entity_divide_loop_2    ; divisor doesn't fit, skip
        stx     $01                     ; update remainder low
        sta     $07                     ; update remainder high
        inc     $06                     ; set quotient bit
entity_divide_loop_2:  dey              ; next iteration
        bne     entity_divide_shift_chain ; loop 16 times
        lda     $06                     ; $04 = quotient low byte
        sta     $04                     ; store quotient low byte
        lda     temp_00                 ; $05 = quotient high byte
        sta     $05                     ; store quotient high byte
        ldx     $09                     ; restore X
        rts                             ; return

; ===========================================================================
; RESET, BANK SWITCHING & COOPERATIVE SCHEDULER — $FD52-$FFFF
; ===========================================================================
; Contains:
;   boss_frame_yield          — boss AI frame yield ($5A=active, skip player)
;   process_frame_yield_full  — saves both banks, $97=$04 (includes player)
;   process_frame_yield       — saves both banks, uses caller's $97
;   call_bank0E_A006          — bank $0E trampoline, calls $A006 with X=$B8
;   call_bank0E_A003          — bank $0E trampoline, calls $A003
;   RESET                     — power-on initialization
;   task_scheduler            — cooperative multitasking (4 task slots)
;   task_register/kill/yield  — coroutine primitives
;   update_CHR_banks          — flag CHR bank refresh for NMI
;   select_CHR_banks          — write CHR bank registers to MMC3
;   process_frame_and_yield   — run one frame of game logic, yield to NMI
;   select_PRG_banks          — MMC3 PRG bank switch with race handling
;   play_sounds               — drain circular sound buffer via bank $16/$17
; ===========================================================================

; ===========================================================================
; Frame-yield trampolines — process one frame, yield to NMI, restore banks
; ===========================================================================
; These wrappers let banked code (boss AI, cutscenes, level transitions)
; run one frame of entity processing + sprite update, then yield to the
; cooperative scheduler until NMI completes. They save/restore PRG bank
; state across the yield so the caller doesn't need to worry about it.
;
; $97 controls OAM start offset for update_entity_sprites:
;   $04 = include player sprites (default, set by process_frame_yield_full)
;   $0C = skip player sprites (set by boss_frame_yield)
;
; Variants:
;   boss_frame_yield      — sets $5A (boss active), $97=$0C, saves $F5
;   process_frame_yield_full — saves both banks, $97=$04 (includes player)
;   process_frame_yield   — saves both banks, uses caller's $97
;   call_bank0E_A006      — bank $0E trampoline, calls $A006 with X=$B8
;   call_bank0E_A003      — bank $0E trampoline, calls $A003
; ---------------------------------------------------------------------------

; --- boss_frame_yield ---
; Called from bank03 (boss intro sequences). Sets boss-active flag, processes
; entities (skipping player sprites), yields, then restores banks.
boss_frame_yield:

        lda     #$80                    ; set boss-active flag
        sta     boss_active             ; (bit 7 = boss fight in progress)
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        lda     #$0C                    ; $97 = $0C: OAM offset past player
        sta     oam_ptr                 ; (skip first 3 sprite entries)
        jsr     process_frame_and_yield ; process entities + yield one frame
        lda     #$00                    ; clear boss-active flag
        sta     boss_active             ; boss fight no longer in progress
        lda     #$18                    ; restore $8000 bank to $18
        sta     mmc3_select             ; (bank1C_1D fixed pair)
        pla                             ; restore $A000 bank
        sta     prg_bank                ; restore $A000 bank from stack
        jmp     select_PRG_banks        ; re-select banks and return

; --- process_frame_yield_full ---
; Saves both PRG banks, sets $97=$04, processes frame, restores banks.
; Called from bank0C/0B (level loading, cutscenes), bank18 (stage select).
process_frame_yield_full:

        lda     mmc3_select             ; save both PRG banks
        pha                             ; push $8000 bank to stack
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        jsr     process_frame_yield_with_player ; $97=$04, process + yield
restore_banks:  pla                     ; restore $A000 bank
        sta     prg_bank                ; write back $A000 bank
        pla                             ; restore $8000 bank
        sta     mmc3_select             ; write back $8000 bank
        jmp     select_PRG_banks        ; re-select and return

; --- process_frame_yield ---
; Same as above but uses caller's $97 value (doesn't set $04).
; Called from bank0B/0F (level transitions with custom OAM offset).
process_frame_yield:

        lda     mmc3_select             ; save both PRG banks
        pha                             ; push $8000 bank to stack
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        jsr     process_frame_and_yield ; process + yield (caller's $97)
        jmp     restore_banks           ; restore banks and return

; --- call_bank0E_A006 ---
; Switches $A000-$BFFF to bank $0E, calls entry point $A006 with X=$B8.
; Called from bank12 (entity AI).
call_bank0E_A006:

        stx     $0F                     ; save X
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        lda     #$0E                    ; switch $A000 to bank $0E
        sta     prg_bank                ; set $A000 bank = $0E
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $B8                     ; X = parameter from $B8
        jsr     banked_A006             ; call bank $0E entry point
restore_A000:
        pla                             ; restore $A000 bank
        sta     prg_bank                ; write back $A000 bank
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $0F                     ; restore X
        rts                             ; return to caller

; --- call_bank0E_A003 ---
; Switches $A000-$BFFF to bank $0E, calls entry point $A003.
; Called from bank12 (entity AI).
call_bank0E_A003:

        stx     $0F                     ; save X
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        lda     #$0E                    ; switch $A000 to bank $0E
        sta     prg_bank                ; set $A000 bank = $0E
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_A003             ; call bank $0E entry point
        jmp     restore_A000            ; restore bank and return

; unused data / padding before RESET vector
        .byte   $8A,$40,$A3,$00,$0F
        .byte   $04,$4B,$50,$80,$04,$18,$10,$E0
        .byte   $00,$64,$04,$C5,$45,$67,$50,$CA
        .byte   $11,$1B,$51,$BA,$00,$44,$00,$3E
        .byte   $00,$A2,$04,$99,$00,$DD,$04,$81
        .byte   $10,$2B,$11,$80,$01,$4A,$40,$9C
        .byte   $01,$47,$15,$F7,$11,$47,$44,$17
        .byte   $40,$C2,$40,$28,$11,$CB,$44,$6E
        .byte   $50,$8A,$54,$AE,$10,$2B
        .byte   $00,$53,$05,$5D,$15
RESET:  sei                             ; disable interrupts
        cld                             ; clear decimal mode
        lda     #$08                    ; PPUCTRL: sprite table = $1000
        sta     PPUCTRL                 ; (NMI not yet enabled)
        lda     #$40                    ; APU frame counter: disable IRQ
        sta     APU_FRAME               ; write $40 to APU frame counter
        ldx     #$00                    ; X = 0 for clearing registers
        stx     PPUMASK                 ; PPUMASK: rendering off
        stx     DMC_FREQ                ; DMC: disable
        stx     SND_CHN                 ; APU status: silence all channels
        dex                             ; stack pointer = $FF
        txs                             ; set stack pointer to $01FF

; --- wait for PPU warm-up (4 VBlank cycles) ---
        ldx     #$04                    ; 4 iterations
ppu_vblank_wait_set:  lda     PPUSTATUS ; wait for VBlank flag set
        bpl     ppu_vblank_wait_set     ; loop until bit 7 set
ppu_vblank_wait_clear:  lda     PPUSTATUS ; wait for VBlank flag clear
        bmi     ppu_vblank_wait_clear   ; loop until bit 7 clear
        dex                             ; repeat 4 times
        bne     ppu_vblank_wait_set     ; next VBlank cycle

; --- exercise PPU address bus ---
        lda     PPUSTATUS               ; reset PPU latch
        lda     #$10                    ; toggle PPUADDR between $1010
        tay                             ; and $0000, 16 times
ppu_address_write:  sta     PPUADDR     ; write high byte
        sta     PPUADDR                 ; write low byte
        eor     #$10                    ; toggle $10 ↔ $00
        dey                             ; decrement iteration counter
        bne     ppu_address_write       ; loop 16 times

; --- clear zero page ($00-$FF) ---
        tya                             ; A = 0 (Y wrapped to 0)
zeropage_clear_loop:  sta     temp_00,y ; clear byte at Y
        dey                             ; Y: 0, $FF, $FE, ..., $01
        bne     zeropage_clear_loop     ; loop all 256 bytes

; --- clear RAM pages $01-$06 ($0100-$06FF) ---
ram_clear_page_advance:  inc     $01    ; advance page pointer ($01→$06)
ram_clear_loop:  sta     (temp_00),y    ; clear byte via ($00/$01)+Y
        iny                             ; next byte in page
        bne     ram_clear_loop          ; loop 256 bytes per page
        ldx     $01                     ; check page counter
        cpx     #$07                    ; stop after page $06
        bne     ram_clear_page_advance  ; more pages → continue

; --- initialize sound buffer ($DC-$E3) to $88 (no sound) ---
        ldy     #$07                    ; 8 sound buffer bytes
        lda     #$88                    ; $88 = "no sound" sentinel
sound_buffer_clear_loop:  sta     $DC,x ; X=7..0 → $E3..$DC
        dex                             ; next buffer byte
        bpl     sound_buffer_clear_loop ; loop all 8 bytes

; --- hardware/bank initialization ---
        lda     #$18                    ; PPUMASK shadow: show sprites + BG
        sta     ppu_mask_shadow         ; store PPU mask shadow
        lda     #$00                    ; MMC3 mirroring: vertical
        sta     MMC3_MIRRORING          ; set vertical mirroring
        ldx     #$1C                    ; $F4/$F5 = initial PRG banks
        stx     mmc3_select             ; bank $1C at $8000-$9FFF
        inx                             ; bank $1D at $A000-$BFFF
        stx     prg_bank                ; bank $1D at $A000-$BFFF
        jsr     select_PRG_banks        ; apply initial bank switch
        lda     #$40                    ; CHR bank setup:
        sta     $E8                     ; $E8=$40 (2KB bank 0)
        lda     #$42                    ; $E9=$42 (2KB bank 1)
        sta     $E9                     ; $EA=$00 (1KB bank 2)
        lda     #$00                    ; $EA=$00 (1KB bank 2)
        sta     $EA                     ; store 1KB CHR bank 2
        lda     #$01                    ; $EB=$01 (1KB bank 3)
        sta     $EB                     ; store 1KB CHR bank 3
        lda     #$0A                    ; $EC=$0A (1KB bank 4)
        sta     $EC                     ; store 1KB CHR bank 4
        lda     #$0B                    ; $ED=$0B (1KB bank 5)
        sta     $ED                     ; store 1KB CHR bank 5
        jsr     update_CHR_banks        ; apply CHR bank configuration
        jsr     prepare_oam_buffer      ; init OAM / sprite state
        lda     #$20                    ; clear nametable 0 ($2000)
        ldx     #$00                    ; A=addr hi, X=fill, Y=attr fill
        ldy     #$00                    ; X = fill byte ($00)
        jsr     fill_nametable          ; clear nametable 0
        lda     #$24                    ; clear nametable 1 ($2400)
        ldx     #$00                    ; X = fill byte ($00)
        ldy     #$00                    ; Y = attribute fill ($00)
        jsr     fill_nametable          ; clear nametable 1

; --- register main game task (slot 0, address $C8D0) ---
        lda     #$C8                    ; $93/$94 = $C8D0 (main game entry)
        sta     $94                     ; (in bank $1C, always-mapped range)
        lda     #$D0                    ; low byte of $C8D0
        sta     task_ptr                ; low byte of entry address
        lda     #$00                    ; A = slot 0
        jsr     task_register           ; register task with address
        lda     #$88                    ; PPUCTRL: NMI enable + sprite $1000
        sta     ppu_ctrl_shadow         ; store in PPUCTRL shadow

; fall through to scheduler
; ===========================================================================
; task_scheduler — cooperative multitasking scheduler
; ===========================================================================
; MM3 uses a simple cooperative scheduler with 4 task slots at $80-$8F.
; Each slot is 4 bytes:
;   byte 0 ($80,x): state — $00=free, $01=sleeping, $02=running,
;                            $04=ready (woken by NMI), $08=fresh (has address)
;   byte 1 ($81,x): sleep countdown (decremented by NMI when state=$01)
;   byte 2 ($82,x): saved stack pointer (state $04) or address low (state $08)
;   byte 3 ($83,x): address high (state $08 only)
;
; The scheduler spins until a task with state >= $04 is found, then either:
;   state $08: JMP to address stored in bytes 2-3 (fresh task launch)
;   state $04: restore stack pointer from byte 2 and RTS (resume coroutine)
;
; NMI decrements sleeping tasks' countdowns and sets state $04 when done.
; Task slot 0 gets controller input read on resume (read_controllers).
;
; nmi_occurred = NMI flag (set $FF by NMI, forces rescan)
; $91 = current task slot index (0-3)
; ---------------------------------------------------------------------------
task_scheduler:  ldx     #$FF           ; reset stack to top
        txs                             ; (discard all coroutine frames)
task_scheduler_state_ready:  ldx     #$00 ; clear NMI flag
        stx     nmi_occurred            ; clear NMI flag
        ldy     #$04                    ; 4 slots to check
task_scheduler_state_check:  lda     $80,x ; state >= $04? (ready or fresh)
        cmp     #$04                    ; state >= $04? (ready/fresh)
        bcs     task_scheduler_nmi_check ; yes → found runnable task
        inx                             ; advance to next slot (+4 bytes)
        inx                             ; advance 4 bytes per slot
        inx                             ; (continued)
        inx                             ; (continued)
        dey                             ; more slots?
        bne     task_scheduler_state_check ; more slots → keep scanning
        jmp     task_scheduler_state_ready ; no runnable tasks, spin until NMI

task_scheduler_nmi_check:  lda     nmi_occurred ; if NMI fired during scan,
        bne     task_scheduler_state_ready ; rescan (states may have changed)
        dey                             ; convert Y countdown → slot index
        tya                             ; Y=3→0, Y=2→1, Y=1→2, Y=0→3
        eor     #$03                    ; invert to get slot index
        sta     $91                     ; $91 = current slot index
        ldy     $80,x                   ; Y = task state
        lda     #$02                    ; mark slot as running
        sta     $80,x                   ; mark task as running
        cpy     #$08                    ; state $08? → fresh task (JMP)
        bne     task_scheduler_restore_sp ; state $08? → fresh task
        lda     $82,x                   ; load address from slot
        sta     task_ptr                ; store address low byte
        lda     $83,x                   ; load address high byte
        sta     $94                     ; store address high byte
        jmp     (task_ptr)              ; launch task at stored address

task_scheduler_restore_sp:  lda     $82,x ; restore saved stack pointer
        tax                             ; stack pointer → X
        txs                             ; restore stack pointer
        lda     $91                     ; slot 0? read controllers first
        bne     task_scheduler_restore_regs ; (only main game task gets input)
        jsr     read_controllers        ; read_controllers
task_scheduler_restore_regs:  pla       ; restore Y and X from stack
        tay                             ; (saved by task_yield)
        pla                             ; restore X from stack
        tax                             ; restore X from stack
        rts                             ; resume coroutine after yield

; ===========================================================================
; Coroutine primitives — task registration, yielding, and destruction
; ===========================================================================
; These routines manage the cooperative scheduler's task slots.
;
; task_register:   register a new task with an address (state $08)
; task_kill_by_id: free a task slot by ID (A = slot index)
; task_kill_self:  free current task and return to scheduler
; slot_offset_self/slot_offset: convert slot index → byte offset (×4)
; task_yield_x:    yield for X frames (calls task_yield in a loop)
; task_yield:      yield for 1 frame (save state, sleep, return to scheduler)
; ---------------------------------------------------------------------------

; --- task_register — A = slot index, $93/$94 = entry address ---

task_register:  jsr     slot_offset     ; X = A × 4
        lda     task_ptr                ; store address in slot bytes 2-3
        sta     $82,x                   ; store address low in slot
        lda     $94                     ; load address high byte
        sta     $83,x                   ; store address high in slot
        lda     #$08                    ; state = $08 (fresh, has address)
        sta     $80,x                   ; state = fresh (has address)
        rts                             ; return

; --- task_kill_by_id — A = slot index to kill ---
task_kill_by_id:

        jsr     slot_offset             ; X = A × 4
        lda     #$00                    ; state = $00 (free)
        sta     $80,x                   ; state = free
        rts                             ; return

; --- task_kill_self — kill current task ($91) and return to scheduler ---
task_kill_self:

        jsr     slot_offset_self        ; X = $91 × 4
        lda     #$00                    ; state = $00 (free)
        sta     $80,x                   ; state = free
        jmp     task_scheduler          ; back to scheduler (never returns)

; --- slot_offset_self — X = $91 × 4 (current task's byte offset) ---

slot_offset_self:  lda     $91          ; load current task index

; --- slot_offset — X = A × 4 (task slot byte offset) ---
slot_offset:  asl     a                 ; A × 4
        asl     a                       ; A × 2
        tax                             ; A × 4 → X
        rts                             ; return

; --- task_yield_x — yield for X frames ---
; Called extensively from cutscene/level-transition code for timed waits.

task_yield_x:  jsr     task_yield       ; yield one frame
        dex                             ; loop X times
        bne     task_yield_x            ; loop X times
        rts                             ; return

; --- task_yield — yield current task for 1 frame ---
; Saves X/Y on stack, stores sleep countdown and stack pointer in the
; task slot, sets state to $01 (sleeping), then jumps to scheduler.
; NMI will decrement the countdown; when it reaches 0, state → $04.
; Scheduler then restores SP and does RTS to resume here.

task_yield:  lda     #$01               ; $93 = sleep frames (1)
        sta     task_ptr                ; sleep for 1 frame
        txa                             ; save X and Y on stack
        pha                             ; (scheduler's .restore_regs will
        tya                             ; PLA these back on resume)
        pha                             ; save Y on stack
        jsr     slot_offset_self        ; X = $91 × 4
        lda     task_ptr                ; store sleep countdown in byte 1
        sta     $81,x                   ; store sleep countdown
        lda     #$01                    ; state = $01 (sleeping)
        sta     $80,x                   ; state = sleeping
        txa                             ; Y = slot byte offset
        tay                             ; slot offset → Y
        tsx                             ; save current stack pointer
        stx     $82,y                   ; in slot byte 2
        jmp     task_scheduler          ; hand off to scheduler

update_CHR_banks:  lda     #$FF         ; turns on the flag for
        sta     $1B                     ; refreshing CHR banks
        rts                             ; during NMI

; selects all 6 swappable CHR banks
; based on what's in $E8~$ED

select_CHR_banks:  lda     $1B          ; test select CHR flag
        beq     task_yield_return       ; return if not on
task_yield_clear_flag:  ldx     #$00    ; reset select CHR flag
        stx     $1B                     ; immediately, one-off usage
task_yield_select_chr:  stx     MMC3_BANK_SELECT ; MMC3 bank select register
        lda     $E8,x                   ; load CHR bank number from $E8+X
        sta     MMC3_BANK_DATA          ; write to MMC3 bank data register
        inx                             ; next CHR bank slot
        cpx     #$06                    ; all 6 CHR banks done?
        bne     task_yield_select_chr   ; loop until done
task_yield_return:  rts                 ; return

; ===========================================================================
; process_frame_and_yield — run one frame of game logic, then yield to NMI
; ===========================================================================
; Core game loop primitive. Processes all entities, builds OAM sprite data,
; clears $EE (signals "ready for NMI to render"), yields one frame, then
; sets $EE (signals "NMI done, safe to modify state").
;
; process_frame_yield_with_player: sets oam_ptr=$04 (include player sprites)
; process_frame_and_yield: uses caller's oam_ptr value
;
; oam_ptr = OAM write index start (controls which sprite slots to fill):
;   $04 = start after sprite 0 (include player), $0C = skip player sprites
; nmi_skip = rendering phase flag (0=NMI pending, nonzero=NMI completed)
; ---------------------------------------------------------------------------

process_frame_yield_with_player:  lda     #$04 ; $97 = $04: start OAM after sprite 0
        sta     oam_ptr                 ; (include player in sprite update)
process_frame_and_yield:  jsr     prepare_oam_buffer ; process entities (sprite state)
        jsr     update_entity_sprites   ; build OAM buffer from entity data
        lda     #$00                    ; $EE = 0: "waiting for NMI"
        sta     nmi_skip                ; signal waiting for NMI
        jsr     task_yield              ; sleep 1 frame (NMI will render)
        inc     nmi_skip                ; = 1: "NMI done, frame complete"
        rts                             ; return

; selects both swappable PRG banks
; based on $F4 and $F5

select_PRG_banks:  inc     $F6          ; flag on "selecting PRG bank"
        lda     #$06                    ; MMC3 cmd $06: $8000 bank
        sta     mmc3_shadow             ; MMC3 cmd $06: select $8000 bank
        sta     MMC3_BANK_SELECT        ; select the bank in $F4
        lda     mmc3_select             ; as $8000-$9FFF
        sta     $F2                     ; also mirror in $F2
        sta     MMC3_BANK_DATA          ; write to MMC3 bank data register
        lda     #$07                    ; MMC3 cmd $07: $A000 bank
        sta     mmc3_shadow             ; MMC3 cmd $07: select $A000 bank
        sta     MMC3_BANK_SELECT        ; select the bank in $F5
        lda     prg_bank                ; as $A000-$BFFF
        sta     $F3                     ; also mirror in $F3
        sta     MMC3_BANK_DATA          ; write to MMC3 bank data register
        dec     $F6                     ; flag selecting back off (done)
        lda     $F7                     ; if NMI and non-NMI race condition
        bne     play_sounds             ; we still need to play sounds
        rts                             ; else just return

; go through circular sound buffer and pop for play
; handle NMI simultaneous bank changes as well

play_sounds:  lda     $F6               ; this means both NMI and non
        bne     process_frame_race_condition ; yes: flag race and return
        lda     #$06                    ; MMC3 cmd $06: select $8000 bank
        sta     MMC3_BANK_SELECT        ; select $8000 bank register
        lda     #$16                    ; select bank 16 for $8000-$9FFF
        sta     MMC3_BANK_DATA          ; and 17 for $A000-$BFFF
        lda     #$07                    ; MMC3 cmd $07: $A000 bank
        sta     MMC3_BANK_SELECT        ; select $A000 bank register
        lda     #$17                    ; bank $17 for sound data
        sta     MMC3_BANK_DATA          ; write $A000 bank
process_frame_sound_check:  ldx     $DB ; is current sound slot in buffer
        lda     $DC,x                   ; == $88? this means
        cmp     #$88                    ; no sound, skip processing
        beq     process_frame_sound_call ; no sound → call driver tick
        pha                             ; push sound ID
        lda     #$88                    ; clear sound ID immediately
        sta     $DC,x                   ; in circular buffer
        inx                             ; advance buffer index
        txa                             ; increment circular buffer index
        and     #$07                    ; with wraparound $07 -> $00
        sta     $DB                     ; store wrapped buffer index
        pla                             ; play sound ID
        jsr     banked_8003             ; bank $16: play_sound_effect
        jmp     process_frame_sound_check ; check next slot

process_frame_sound_call:  jsr     banked_8000 ; bank $16: sound_driver_tick
        lda     #$00                    ; clear race condition flag
        sta     $F7                     ; clear race condition flag
        jmp     select_PRG_banks        ; apply bank switch and return
process_frame_race_condition:  inc     $F7 ; set race condition flag
        rts                             ; return
        .byte   $05,$10,$11,$04,$41
        .byte   $33,$5C,$D4,$45,$82,$00,$EF,$51
        .byte   $68,$50,$67,$10,$1C,$00,$07,$04
        .byte   $CD,$50,$00,$50,$04,$15,$96,$00
        .byte   $71,$14,$94,$15,$DD,$0E,$97,$C3
        .byte   $43,$04,$00,$00,$08,$57

; interrupt vectors
        .word   NMI                     ; $FFFA: NMI handler
        .word   RESET                   ; $FFFC: RESET handler
        .word   IRQ                     ; $FFFE: IRQ handler
