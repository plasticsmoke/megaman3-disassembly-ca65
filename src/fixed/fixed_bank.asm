; da65 V2.18 - Ubuntu 2.19-1
; Created:    2026-02-21 06:01:26
; Input file: /home/kn/megamanforever/megaman3-disassembly-ca65/tools/../build/bank1E_1F.bin
; Page:       1



; =============================================================================
; MEGA MAN 3 (U) — BANKS $1E-$1F — MAIN GAME LOGIC (FIXED BANK)
; =============================================================================
; This is the fixed code bank for Mega Man 3. Always mapped to $C000-$FFFF.
; Contains:
;   - NMI / interrupt handling
;   - Player state machine (22 states via player_state_ptr_lo/hi)
;   - Player movement physics (walk, jump, $99, slide, ladder)
;   - Weapon firing and initialization
;   - Entity movement routines (shared by player and enemies)
;   - Sprite animation control
;   - Collision detection (player-entity, weapon-entity)
;   - Sound submission
;   - PRG/CHR bank switching
;
; NOTE: MM3 shares its engine with Mega Man 4 (MM4). Many routines here
; are near-identical to MM4's bank38_3F.asm (the MM4 fixed bank).
; MM4 cross-references (from plasticsmoke/megaman4-disassembly):
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
; Annotation: ~52% — 474/919 labels named, ~3970 inline comments transferred
;   from xkas reference. 445 auto-labels remain (internal branch targets).
;
; =============================================================================
; KEY MEMORY MAP
; =============================================================================
;
; Zero Page:
;   $12     = stage select cursor column (0-2) / player tile X (in-game)
;   $13     = stage select cursor row offset (0/3/6)
;   $14     = controller 1 buttons (new presses this frame)
;   $15     = controller 1 buttons (new presses, alternate read)
;   $16     = controller 1 buttons (held / continuous)
;             Button bits: $80=A $40=B $20=Select $10=Start
;                          $08=Up $04=Down $02=Left $01=Right
;   $18     = palette update request flag
;   $19     = nametable update request flag
;   $1A     = nametable column update flag
;   $22     = current stage index (see STAGE MAPPING below)
;   $30     = player state (index into player_state_ptr tables, 22 states)
;   $31     = player facing direction (1=right, 2=left)
;   $32     = walking flag / sub-state (nonzero = walking or sub-state active)
;   $34     = entity_ride slot (entity slot index being ridden, state $05)
;   $35     = facing sub-state / Mag Fly direction inherit
;   $39     = invincibility/i-frames timer (nonzero = immune to damage)
;   $3A     = jump/rush counter
;   $3D     = pending hazard state ($06=damage, $0E=death from tile collision)
;   $41     = max tile type at feet (highest priority hazard)
;   $43-$44 = tile types at player feet (for ground/ladder detection)
;   $50     = scroll lock flag
;   $5A     = boss active flag (bit 7 = boss fight in progress)
;   $5B-$5C = spark freeze slots (entity X indices frozen by Spark Shock)
;   $5D     = sub-state / item interaction flag
;   $5E-$5F = scroll position values
;   $60     = stage select page offset (0=Robot Master, nonzero=Doc Robot/Wily)
;   $61     = boss-defeated bitmask (bit N = stage $0N boss beaten, $FF=all 8)
;   $68     = cutscene-complete flag (Proto Man encounter)
;   $69-$6A = scroll position (used during Gamma screen_scroll)
;   $6C     = warp destination index (teleporter tube target)
;   $78     = screen mode
;   $99     = $99 sub-pixel increment ($55 during gameplay = 0.332/frame,
;             set at start of process_sprites each frame; $40 at stage select)
;   $9E-$9F = enemy spawn tracking counters (left/right)
;
; Weapon / Inventory Block ($A0-$AF):
;   $A0     = current weapon ID (see WEAPON IDS below)
;   $A1     = weapon menu cursor position (remembers last pause screen selection)
;   $A2     = player HP        (full=$9C, empty=$80, AND #$1F for value, 28 max)
;   $A3     = Gemini Laser ammo (full=$9C, empty=$80, address = $A2 + weapon ID)
;   $A4     = Needle Cannon ammo
;   $A5     = Hard Knuckle ammo
;   $A6     = Magnet Missile ammo
;   $A7     = Top Spin ammo
;   $A8     = Search Snake ammo
;   $A9     = Rush Coil ammo
;   $AA     = Spark Shock ammo
;   $AB     = Rush Marine ammo
;   $AC     = Shadow Blade ammo
;   $AD     = Rush Jet ammo
;   $AE     = $AE
;   $AF     = E-Tanks
;
;   $B0     = boss HP display position (starts $80, fills to $9C = 28 HP)
;   $B3     = boss HP fill target ($8E = 28 HP)
;   $EE     = NMI skip flag
;   $EF     = current sprite slot counter (loop variable in process_sprites)
;   $F0     = MMC3 bank select register shadow
;   $F5     = current $A000-$BFFF PRG bank number
;   $F8     = game mode / screen state
;   $F9     = camera/scroll page
;   $FA     = vertical scroll
;   $FC-$FD = camera X position (low, high)
;   $FE     = PPU mask ($2001) shadow
;   $FF     = PPU control ($2000) shadow
;
; WEAPON IDS ($A0 values):
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
;   ent_facing,x = facing direction (1=right, 2=left)
;   ent_spawn_id,x = stage enemy ID (spawn tracking, prevents duplicate spawns)
;   ent_hp,x = health / hit points
;   ent_timer,x = AI timer / general purpose counter
;   ent_var1,x = general purpose / wildcard 1
;   ent_var2,x = general purpose / wildcard 2
;   ent_var3,x = general purpose / wildcard 3
;   ent_flags,x = entity flags (bit 7=active/collidable, bit 6=H-flip, bit 4=?)
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
; Controller Button Masks (for $14 / $16):
;   #$80 = A button (Jump)
;   #$40 = B button (Fire)
;   #$20 = Select
;   #$10 = Start
;   #$08 = Up
;   #$04 = Down
;   #$02 = Left
;   #$01 = Right
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


.segment "FIXED"

L0000           := $0000
L000C           := $000C
L0093           := $0093
L009C           := $009C
L8000           := $8000
L8003           := $8003
L8006           := $8006
L8009           := $8009
L800C           := $800C
L800F           := $800F
L8012           := $8012
L9000           := $9000
L9003           := $9003
L9006           := $9006
L9009           := $9009
L9C00           := $9C00
LA000           := $A000
LA003           := $A003
LA006           := $A006
NMI:  php
LC001:  pha
        txa                             ; preserve X, Y, and
        pha                             ; processor flags
        tya
        pha

; --- NMI: disable rendering for safe PPU access during VBlank ---
        lda     $2002                   ; reset PPU address latch
        lda     ppu_ctrl_shadow                     ; PPUCTRL: clear NMI enable (bit 7)
        and     #$7F                    ; to prevent re-entrant NMI
        sta     $2000
        lda     #$00                    ; PPUMASK = 0: rendering off
        sta     $2001                   ; (safe to write PPU during VBlank)

; --- check if PPU updates are suppressed ---
        lda     nmi_skip                     ; $EE = rendering disabled flag
        ora     $9A                     ; $9A = NMI lock flag
        bne     code_C088               ; either set → skip PPU writes, go to scroll

; --- latch scroll/mode/scanline values for this frame ---
        lda     camera_x_lo                     ; $79 = scroll X fine (latched from $FC)
        sta     scroll_x_fine
        lda     camera_x_hi                     ; $7A = PPUCTRL nametable select (from $FD)
        sta     nt_select
        lda     game_mode                     ; $78 = game mode (latched from $F8)
        sta     screen_mode
        lda     scroll_lock                     ; if secondary split active ($50 != 0),
        bne     code_C02F               ; keep current $7B (set by split code)
        lda     $5E                     ; else $7B = default scanline count from $5E
        sta     irq_scanline

; --- OAM DMA: transfer sprite data from $0200-$02FF to PPU ---
code_C02F:  lda     #$00                ; OAMADDR = $00 (start of OAM)
        sta     $2003
        lda     #$02                    ; OAMDMA: copy page $02 ($0200-$02FF)
        sta     $4014                   ; to PPU OAM (256 bytes, 64 sprites)

; --- drain primary PPU buffer ($19 flag) ---
        lda     nametable_dirty                     ; $19 = PPU buffer pending flag
        beq     code_C040               ; no data → skip
        jsr     drain_ppu_buffer        ; write buffered tile data to PPU

; --- drain secondary PPU buffer with VRAM increment ($1A flag) ---
code_C040:  lda     nt_column_dirty                 ; $1A = secondary buffer flag (vertical writes)
        beq     code_C05B               ; no data → skip
        lda     ppu_ctrl_shadow                     ; PPUCTRL: set bit 2 (VRAM addr +32 per write)
        and     #$7F                    ; for vertical column writes to nametable
        ora     #$04
        sta     $2000
        ldx     #$00                    ; clear $1A flag
        stx     nt_column_dirty
        jsr     drain_ppu_buffer_continue ; write column data to PPU
        lda     ppu_ctrl_shadow                     ; PPUCTRL: restore normal increment (+1)
        and     #$7F
        sta     $2000

; --- write palette data ($18 flag) ---
code_C05B:  lda     palette_dirty                 ; $18 = palette update flag
        beq     code_C088               ; no update → skip to scroll
        ldx     #$00                    ; clear $18 flag
        stx     palette_dirty
        lda     $2002                   ; reset PPU latch
        lda     #$3F                    ; PPU addr = $3F00 (palette RAM start)
        sta     $2006
        stx     $2006
        ldy     #$20                    ; 32 bytes = all 8 palettes (4 BG + 4 sprite)
code_C070:  lda     $0600,x             ; copy palette from $0600-$061F to PPU
        sta     $2007
        inx
        dey
        bne     code_C070
        lda     #$3F                    ; reset PPU address to $3F00 then $0000
        sta     $2006                   ; (two dummy writes to clear PPU latch
        sty     $2006                   ; and prevent palette corruption during
        sty     $2006                   ; scroll register writes below)
        sty     $2006

; --- set scroll position for this frame ---
code_C088:  lda     screen_mode                 ; game mode $02 = stage select screen
        cmp     #$02                    ; (uses horizontal-only scroll from $5F)
        bne     code_C09D
        lda     $2002                   ; reset PPU latch
        lda     $5F                     ; PPUSCROLL X = $5F (stage select scroll)
        sta     $2005
        lda     #$00                    ; PPUSCROLL Y = 0
        sta     $2005
        beq     code_C0AA               ; → restore rendering
code_C09D:  lda     $2002               ; reset PPU latch
        lda     scroll_x_fine                     ; PPUSCROLL X = $79 (gameplay scroll X)
        sta     $2005
        lda     scroll_y                     ; PPUSCROLL Y = $FA (vertical scroll offset)
        sta     $2005

; --- restore rendering and CHR banks ---
code_C0AA:  lda     ppu_mask_shadow                 ; PPUMASK = $FE (re-enable rendering)
        sta     $2001
        lda     nt_select                     ; PPUCTRL = $FF | ($7A & $03)
        and     #$03                    ; bits 0-1 from $7A = nametable select
        ora     ppu_ctrl_shadow                     ; rest from $FF (NMI enable, sprite table, etc.)
        sta     $2000
        jsr     select_CHR_banks        ; set MMC3 CHR bank registers
        lda     mmc3_shadow                     ; $8000 = MMC3 bank select register
        sta     L8000                   ; (restore R6/R7 select state from $F0)

; --- NMI: set up MMC3 scanline IRQ for this frame ---
; $7B = scanline count for first IRQ. $9B = enable flag (0=off, 1=on).
; $78 = game mode, used to index irq_vector_table for the handler address.
; $50/$51 = secondary split: if $50 != 0 and $7B >= $51, use gameplay handler.
        lda     irq_scanline                     ; scanline count for first split
        sta     NMI                     ; set MMC3 IRQ counter value
        sta     LC001                   ; latch counter (reload)
        ldx     irq_enable                     ; IRQ enable flag
        sta     auto_walk_spawn_done,x  ; $9B=0 → $E000 (disable), $9B=1 → $E001 (enable)
        beq     code_C0E7               ; if disabled, skip vector setup
        ldx     screen_mode                     ; X = game mode (index into vector table)
        lda     scroll_lock                     ; secondary split flag
        beq     code_C0DD               ; if no secondary split, use mode index
        lda     irq_scanline                     ; if $7B < $51, use mode index
        cmp     $51                     ; (first split happens before secondary)
        bcc     code_C0DD
        ldx     #$01                    ; else override: use index 1 (gameplay)
code_C0DD:  lda     irq_vector_lo,x     ; IRQ vector low byte from table
        sta     L009C
        lda     irq_vector_hi,x         ; IRQ vector high byte from table
        sta     $9D                     ; → $9C/$9D = handler address for JMP ($009C)

; --- NMI: frame counter and sound envelope timers ---
code_C0E7:  inc     frame_counter                 ; $92 = frame counter (increments every NMI)
        ldx     #$FF                    ; $90 = $FF (signal: NMI occurred this frame)
        stx     nmi_occurred
        inx                             ; X = 0
        ldy     #$04                    ; 4 sound channels ($80-$8F, 4 bytes each)
LC0F0:  lda     $80,x                   ; channel state: $01 = envelope counting down
        cmp     #$01
        bne     LC0FE
        dec     $81,x                   ; decrement envelope timer
        bne     LC0FE                   ; not zero → still counting
        lda     #$04                    ; timer expired → set state to $04 (release)
        sta     $80,x
LC0FE:  inx                             ; advance to next channel (+4 bytes)
        inx
        inx
        inx
        dey                             ; loop 4 channels
        bne     LC0F0
        tsx
        lda     $0107,x                 ; manual stack hackery!
        sta     $7D                     ; preserve original address
        lda     $0106,x                 ; from interrupt push
        sta     $7C                     ; (word address 6th & 7th down)
        lda     #$C1                    ; -> $7C & $7D
        sta     $0107,x                 ; then change that slot to be
        lda     #$21                    ; $C121, the address right after RTI
        sta     $0106,x
        pla
        tay
        pla                             ; restore X, Y, and P flags
        tax                             ; and clear interrupt flag
        pla
        plp
        rti                             ; FAKE RETURN

; does not actually return, stack is hardcoded to go right here
; another preserve & restore just to call play_sounds
; this is done in case NMI happened in the middle of selecting PRG banks
; because play_sounds also selects PRG banks - possible race condition
; is handled in play_sounds routine

        php                             ; leave a stack slot for word sized
        php                             ; return address for the RTS
        php
        pha
        txa                             ; once again, preserve X, Y & flags
        pha
        tya
        pha
        tsx                             ; patch return address on stack:
        sec                             ; $7C/$7D = original PC saved by NMI.
        lda     $7C                     ; Subtract 1 because RTS adds 1.
        sbc     #$01                    ; Overwrites the stacked return address
        sta     $0105,x                 ; at SP+5/6 (behind P,A,X,Y,P).
        lda     $7D                     ; This makes the upcoming RTS
        sbc     #$00                    ; resume at the interrupted location.
        sta     $0106,x
        jsr     play_sounds
        pla
        tay
        pla                             ; once again, restore X, Y & flags
        tax
        pla
        plp
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

IRQ:  php
        pha                             ; save registers
        txa
        pha
        tya
        pha
        sta     auto_walk_spawn_done    ; acknowledge MMC3 IRQ (disable)
        sta     LE001                   ; re-enable MMC3 IRQ
        jmp     (L009C)                 ; dispatch to current handler

; ===========================================================================
; irq_gameplay_status_bar — split after HUD for gameplay area ($C152)
; ===========================================================================
; Fires after the status bar scanlines. Sets PPU scroll/nametable for the
; gameplay area below. $52 encodes the PPU coarse Y and nametable for the
; gameplay viewport. When mode $0B is active, falls through to a simpler
; reset-to-origin variant.
; ---------------------------------------------------------------------------

irq_gameplay_status_bar:  lda     screen_mode   ; check game mode
        cmp     #$0B                    ; mode $0B (title)?
        beq     LC17B                   ; → simplified scroll
        lda     $2002                   ; reset PPU latch
        lda     $52                     ; $2006 = $52:$C0
        sta     $2006                   ; (sets coarse Y, nametable, coarse X=0)
        lda     #$C0
        sta     $2006
        lda     $52                     ; PPUCTRL: nametable from ($52 >> 2) & 3
        lsr     a                       ; merged with base $98 (NMI on, BG $1000)
        lsr     a
        and     #$03
        ora     #$98
        sta     $2000
        lda     #$00                    ; fine X = 0, fine Y = 0
        sta     $2005
        sta     $2005
        jmp     irq_exit

LC17B:  lda     $2002                   ; reset PPU latch
        lda     #$20                    ; $2006 = $20:$00
        sta     $2006                   ; (nametable 0, origin)
        lda     #$00
        sta     $2006
        lda     #$98                    ; PPUCTRL = $98 (NT 0, NMI, BG $1000)
        sta     $2000
        lda     #$00                    ; X = 0, Y = 0
        sta     $2005
        sta     $2005
        jmp     irq_exit

; ===========================================================================
; irq_gameplay_hscroll — horizontal scroll for gameplay area ($C198)
; ===========================================================================
; Sets X scroll to $79 (the gameplay horizontal scroll position).
; If secondary split is enabled ($50 != 0), chains to status bar handler
; for an additional split at scanline $51.
; ---------------------------------------------------------------------------
irq_gameplay_hscroll:

        lda     $2002                   ; reset PPU latch
        lda     scroll_x_fine                     ; X scroll = $79
        sta     $2005
        lda     #$00                    ; Y scroll = 0
        sta     $2005
        lda     scroll_lock                     ; secondary split enabled?
        beq     irq_jmp_exit_disable    ; no → last split
        lda     $51                     ; counter = $51 - $9F
        sec                             ; (scanlines until secondary split)
        sbc     #$9F
        sta     NMI
        lda     LC4C9                   ; chain to irq_gameplay_status_bar
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     irq_exit

irq_jmp_exit_disable:  jmp     irq_exit_disable

; ===========================================================================
; irq_gameplay_ntswap — nametable swap after HUD ($C1C1)
; ===========================================================================
; Switches PPU to nametable 2 ($2800) at scroll origin (0,0).
; Used for vertical level layouts where the gameplay area uses a different
; nametable than the HUD. Chains to the next handler indexed by $78,
; unless secondary split overrides to mode $00 (no-op).
; ---------------------------------------------------------------------------
irq_gameplay_ntswap:

        lda     $2002                   ; reset PPU latch
        lda     #$28                    ; $2006 = $28:$00
        sta     $2006                   ; (nametable 2 origin)
        lda     #$00
        sta     $2006
        lda     ppu_ctrl_shadow                     ; PPUCTRL: set nametable bit 1
        ora     #$02                    ; → nametable 2
        sta     $2000
        lda     #$00                    ; X = 0, Y = 0
        sta     $2005
        sta     $2005
        lda     #$B0                    ; counter = $B0 - $7B
        sec                             ; (scanlines to next split)
        sbc     irq_scanline
        sta     NMI
        ldx     screen_mode                     ; chain index = current game mode
        lda     scroll_lock                     ; secondary split?
        beq     LC1F3                   ; no → use mode index
        lda     $51                     ; if $51 == $B0, override to mode $00
        cmp     #$B0                    ; (disable further splits)
        bne     LC1F3
        ldx     #$00                    ; X = 0 → irq_exit_disable handler
LC1F3:  lda     LC4C9,x                 ; chain to handler for mode X
        sta     L009C
        lda     LC4DB,x
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_gameplay_vscroll — vertical scroll for gameplay area ($C200)
; ===========================================================================
; Sets PPU to $22C0 (nametable 0, bottom portion) with Y scroll = $B0 (176).
; Used for stages with vertical scrolling — positions the viewport to show
; the bottom part of the nametable. Optionally chains to status bar handler.
; ---------------------------------------------------------------------------
irq_gameplay_vscroll:

        lda     $2002                   ; reset PPU latch
        lda     #$22                    ; $2006 = $22:$C0
        sta     $2006                   ; (nametable 0, coarse Y ≈ 22)
        lda     #$C0
        sta     $2006
        lda     ppu_ctrl_shadow                     ; PPUCTRL from base value
        sta     $2000
        lda     #$00                    ; X scroll = 0
        sta     $2005
        lda     #$B0                    ; Y scroll = $B0 (176)
        sta     $2005
        lda     scroll_lock                     ; secondary split?
        beq     irq_jmp_exit_disable    ; no → exit disabled
        lda     $51                     ; counter = $51 - $B0
        sec
        sbc     #$B0
        sta     NMI
        lda     LC4C9                   ; chain to irq_gameplay_status_bar
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_stagesel_first — stage select X scroll (mode $05)
; ===========================================================================
; Sets scroll for the upper portion of the stage select screen using the
; standard NES mid-frame $2006/$2005 trick. X scroll comes from $79.
; Chains to irq_stagesel_second after $C0 - $7B scanlines.
; ---------------------------------------------------------------------------
irq_stagesel_first:

        lda     $2002                   ; reset PPU latch
        lda     #$20                    ; $2006 = $20:coarseX
        sta     $2006                   ; (nametable 0, coarse Y=0)
        lda     scroll_x_fine
        lsr     a                       ; coarse X = $79 >> 3
        lsr     a
        lsr     a
        and     #$1F                    ; mask to 0-31
        ora     #$00                    ; coarse Y=0 (top)
        sta     $2006
        lda     ppu_ctrl_shadow                     ; PPUCTRL: nametable 0
        and     #$FC
        sta     $2000
        lda     scroll_x_fine                     ; fine X scroll = $79
        sta     $2005
        lda     #$00                    ; Y scroll = 0
        sta     $2005
        lda     #$C0                    ; counter = $C0 - $7B
        sec                             ; (scanlines to bottom split)
        sbc     irq_scanline
        sta     NMI
        lda     LC4CE                   ; chain to irq_stagesel_second ($C26F)
        sta     L009C
        lda     LC4E0
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_stagesel_second — stage select bottom scroll (mode $06)
; ===========================================================================
; Sets scroll for the bottom portion of the stage select screen.
; Uses nametable 0 at high coarse Y ($23xx). Last split — disables IRQ.
; ---------------------------------------------------------------------------
irq_stagesel_second:

        lda     $2002                   ; reset PPU latch
        lda     #$23                    ; $2006 = $23:coarseX
        sta     $2006                   ; (nametable 0, high coarse Y)
        lda     scroll_x_fine
        lsr     a                       ; coarse X = $79 >> 3
        lsr     a
        lsr     a
        and     #$1F
        ora     #$00
        sta     $2006
        lda     ppu_ctrl_shadow                     ; PPUCTRL: nametable 0
        and     #$FC
        sta     $2000
        lda     scroll_x_fine                     ; fine X scroll = $79
        sta     $2005
        lda     #$C0                    ; Y scroll = $C0 (192)
        sta     $2005
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

        lda     $2002                   ; reset PPU address latch
        lda     scroll_x_fine                     ; negate $79/$7A (two's complement)
        eor     #$FF                    ; inverted_X = -$79
        clc
        adc     #$01
        sta     L009C                   ; $9C = inverted X scroll (temp)
        lda     nt_select                     ; negate high byte with carry
        eor     #$FF
        adc     #$00
        and     #$01                    ; keep only nametable bit
        sta     $9D                     ; $9D = inverted nametable select
        lda     ppu_ctrl_shadow                     ; PPUCTRL: clear NT bits, set inverted NT
        and     #$FC
        ora     $9D
        sta     $2000                   ; → middle strip uses opposite nametable
        lda     L009C                   ; set X scroll = negated value
        sta     $2005                   ; (cancels horizontal movement)
        lda     #$58                    ; set Y scroll = $58 (88)
        sta     $2005                   ; (top of middle band)
        lda     #$40                    ; set next IRQ at 64 scanlines later
        sta     NMI                     ; (scanline 88+64 = 152)
        lda     LC4D0                   ; chain to second split handler
        sta     L009C                   ; $9C/$9D → $C2D2
        lda     LC4E2                   ; (irq_transition_second_split)
        sta     $9D
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

        lda     $2002                   ; reset PPU address latch
        lda     nt_select                     ; compute PPU $2006 high byte:
        and     #$01                    ; nametable bit → bits 2-3
        asl     a                       ; ($7A & 1) << 2 | $22
        asl     a                       ; → $22 (NT 0) or $26 (NT 1)
        ora     #$22
        sta     $2006
        lda     scroll_x_fine                     ; compute PPU $2006 low byte:
        lsr     a                       ; ($79 >> 3) = coarse X scroll
        lsr     a                       ; $60 = coarse Y=12 (scanline 96)
        lsr     a
        and     #$1F
        ora     #$60
        sta     $2006
        lda     nt_select                     ; PPUCTRL: set nametable bits from $7A
        and     #$03                    ; merge with base value $FF
        ora     ppu_ctrl_shadow
        sta     $2000
        lda     scroll_x_fine                     ; fine X scroll = $79 (low 3 bits used)
        sta     $2005
        lda     #$98                    ; Y scroll = $98 (152) — bottom strip
        sta     $2005
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

        lda     $2002                   ; reset PPU latch
        ldy     $73                     ; Y = strip index (0-2)
        lda     $69                     ; X scroll = $69 EOR mask + offset
        eor     wave_eor_masks,y        ; masks: $FF/$00/$FF (negate strips 0,2)
        clc                             ; offsets: $01/$00/$01
        adc     wave_adc_offsets,y      ; → alternating wave scroll
        sta     $2005
        lda     wave_y_scroll_set,y     ; Y scroll from table: $30/$60/$90
        sta     $2005
        lda     #$0E                    ; next IRQ in 14 scanlines
        sta     NMI
        lda     LC4D2                   ; chain to irq_wave_advance ($C32B)
        sta     L009C
        lda     LC4E4
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_wave_advance — advance wave strip counter (mode $0A)
; ===========================================================================
; Resets X scroll to 0 between wave strips, advances $73 counter.
; If all 3 strips done, optionally chains to secondary split.
; Otherwise loops back to irq_wave_set_strip after 32 scanlines.
; ---------------------------------------------------------------------------
irq_wave_advance:

        lda     $2002                   ; reset PPU latch
        ldy     $73                     ; Y = strip index
        lda     #$00                    ; X scroll = 0 (reset between strips)
        sta     $2005
        lda     wave_y_scroll_advance,y ; Y scroll from table: $40/$70/$A0
        sta     $2005
        inc     $73                     ; advance to next strip
        lda     $73
        cmp     #$03                    ; all 3 strips done?
        beq     LC355                   ; yes → finish
        lda     #$20                    ; next IRQ in 32 scanlines
        sta     NMI
        lda     LC4D1                   ; chain back to irq_wave_set_strip ($C302)
        sta     L009C
        lda     LC4E3
        sta     $9D
        jmp     irq_exit

LC355:  lda     #$00                    ; reset strip counter
        sta     $73
        lda     scroll_lock                     ; secondary split?
        beq     LC372                   ; no → last split
        lda     $51                     ; counter = $51 - $A0
        sec
        sbc     #$A0
        sta     NMI
        lda     LC4C9                   ; chain to irq_gameplay_status_bar
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     irq_exit

LC372:  jmp     irq_exit_disable

; ===========================================================================
; irq_title_first — title/password screen first split (mode $0B)
; ===========================================================================
; Sets scroll to $2140 (nametable 0, tile row 10) with X=0, Y=0.
; This positions the main content area of the title/password screen.
; Chains to irq_title_second after $4C (76) scanlines.
; ---------------------------------------------------------------------------
irq_title_first:

        lda     $2002                   ; reset PPU latch
        lda     #$21                    ; $2006 = $21:$40
        sta     $2006                   ; (nametable 0, tile row 10)
        lda     #$40
        sta     $2006
        lda     ppu_ctrl_shadow                     ; PPUCTRL: nametable 0
        and     #$FC
        sta     $2000
        lda     #$00                    ; X = 0, Y = 0
        sta     $2005
        sta     $2005
        lda     #$4C                    ; next IRQ in 76 scanlines
        sta     NMI
        lda     LC4D4                   ; chain to irq_title_second ($C3A3)
        sta     L009C
        lda     LC4E6
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_title_second — title/password screen X scroll (mode $0C)
; ===========================================================================
; Sets X scroll from $6A for the bottom portion of the title screen.
; Optionally chains to secondary split for HUD overlay.
; ---------------------------------------------------------------------------
irq_title_second:

        lda     $2002                   ; reset PPU latch
        lda     $6A                     ; X scroll = $6A
        sta     $2005
        lda     #$00                    ; Y scroll = 0
        sta     $2005
        lda     scroll_lock                     ; secondary split?
        beq     LC3C9                   ; no → last split
        lda     $51                     ; counter = $51 - $A0
        sec
        sbc     #$A0
        sta     NMI
        lda     LC4C9                   ; chain to irq_gameplay_status_bar
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     irq_exit

LC3C9:  jmp     irq_exit_disable

; ===========================================================================
; irq_cutscene_scroll — cutscene full scroll setup (mode $0D)
; ===========================================================================
; Full $2006/$2005 mid-frame scroll using $6A (X), $6B (nametable), $7B (Y).
; Used for cutscene/intro sequences with arbitrary scroll positioning.
; Chains to irq_cutscene_secondary after $AE - $7B scanlines.
; ---------------------------------------------------------------------------
irq_cutscene_scroll:

        lda     $2002                   ; reset PPU latch
        lda     $6B                     ; $2006 high = ($6B << 2) | $20
        asl     a                       ; → $20 (NT 0) or $24 (NT 1)
        asl     a
        ora     #$20
        sta     $2006
        lda     $6A                     ; $2006 low = ($6A >> 3) | $E0
        lsr     a                       ; coarse X from $6A, high coarse Y
        lsr     a
        lsr     a
        ora     #$E0
        sta     $2006
        lda     $6A                     ; fine X scroll = $6A
        sta     $2005
        lda     irq_scanline                     ; fine Y scroll = $7B
        sta     $2005
        lda     ppu_ctrl_shadow                     ; PPUCTRL: base | nametable bits from $6B
        ora     $6B
        sta     $2000
        lda     #$AE                    ; counter = $AE - $7B
        sec                             ; (scanlines to secondary split)
        sbc     irq_scanline
        sta     NMI
        lda     LC4D6                   ; chain to irq_cutscene_secondary ($C408)
        sta     L009C
        lda     LC4E8
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_cutscene_secondary — cutscene secondary split (mode $0E)
; ===========================================================================
; Handles the bottom portion of cutscene screens. If $50 != 0 and
; $51 - $B0 == 0, chains directly to status bar. Otherwise resets scroll
; to $22C0 (nametable 0 bottom) and optionally chains for another split.
; ---------------------------------------------------------------------------
irq_cutscene_secondary:

        lda     scroll_lock                     ; secondary split enabled?
        beq     LC417                   ; no → reset to origin
        lda     $51                     ; X = $51 - $B0
        sec                             ; (remaining scanlines)
        sbc     #$B0
        tax
        bne     LC417                   ; non-zero → need scroll reset
        jmp     irq_gameplay_status_bar ; zero → chain directly to status bar

LC417:  lda     $2002                   ; reset PPU latch
        lda     #$22                    ; $2006 = $22:$C0
        sta     $2006                   ; (nametable 0, bottom portion)
        lda     #$C0
        sta     $2006
        lda     ppu_ctrl_shadow                     ; PPUCTRL: nametable 0
        and     #$FC
        sta     $2000
        lda     #$00                    ; X = 0, Y = 0
        sta     $2005
        sta     $2005
        lda     scroll_lock                     ; secondary split?
        beq     LC447                   ; no → last split
        stx     NMI                     ; counter = X (from $51 - $B0 above)
        lda     LC4C9                   ; chain to irq_gameplay_status_bar
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     irq_exit

LC447:  jmp     irq_exit_disable

; ===========================================================================
; irq_chr_split_first — scroll + chain to CHR swap (mode $0F)
; ===========================================================================
; Sets X scroll from $69, then chains to irq_chr_split_swap after 48
; scanlines. First half of a two-part mid-frame CHR bank swap effect.
; ---------------------------------------------------------------------------
irq_chr_split_first:

        lda     $2002                   ; reset PPU latch
        lda     $69                     ; X scroll = $69
        sta     $2005
        lda     #$00                    ; Y scroll = 0
        sta     $2005
        lda     #$30                    ; next IRQ in 48 scanlines
        sta     NMI
        lda     LC4D8                   ; chain to irq_chr_split_swap ($C469)
        sta     L009C
        lda     LC4EA
        sta     $9D
        jmp     irq_exit

; ===========================================================================
; irq_chr_split_swap — scroll + mid-frame CHR bank swap (mode $10)
; ===========================================================================
; Sets X scroll from $6A with nametable from $6B, then performs a mid-frame
; CHR bank swap: swaps BG CHR to banks $66/$72, then sets up $78/$7A for
; the main loop to restore during NMI. $1B flag signals the swap occurred.
; ---------------------------------------------------------------------------
irq_chr_split_swap:

        lda     $2002                   ; reset PPU latch
        lda     $6A                     ; X scroll = $6A
        sta     $2005
        lda     #$00                    ; Y scroll = 0
        sta     $2005
        lda     ppu_ctrl_shadow                     ; PPUCTRL: nametable from $6B
        and     #$FC
        ora     $6B
        sta     $2000
        lda     #$66                    ; swap BG CHR bank 0 → $66
        sta     $E8
        lda     #$72                    ; swap BG CHR bank 1 → $72
        sta     $E9
        jsr     LFF45                   ; apply CHR bank swap
        lda     mmc3_shadow                     ; trigger MMC3 bank latch
        sta     L8000
        lda     #$78                    ; set up next banks for NMI restore
        sta     $E8                     ; BG bank 0 → $78
        lda     #$7A
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
        pha
        lda     $E9
        pha
        lda     #$66                    ; temporarily swap BG CHR bank 0 → $66
        sta     $E8
        lda     #$72                    ; temporarily swap BG CHR bank 1 → $72
        sta     $E9
        jsr     LFF45                   ; apply CHR bank swap
        lda     mmc3_shadow                     ; trigger MMC3 bank latch
        sta     L8000
        pla                             ; restore $E9
        sta     $E9
        pla                             ; restore $E8
        sta     $E8
        inc     $1B                     ; signal main loop: CHR swap occurred

; --- IRQ exit routines ---
; Handlers jump here when done. Two entry points:
;   irq_exit_disable ($C4BA): disables IRQ (last split of frame)
;   irq_exit ($C4BD): keeps IRQ enabled (more splits coming)
irq_exit_disable:  sta     auto_walk_spawn_done ; disable MMC3 IRQ (no more splits)
irq_exit:  pla
        tay
        pla                             ; restore registers
        tax
        pla
        plp
        rti

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
LC4C9:  .byte   $52,$98,$C1,$00,$35
LC4CE:  .byte   $6F,$97
LC4D0:  .byte   $D2
LC4D1:  .byte   $02
LC4D2:  .byte   $2B,$75
LC4D4:  .byte   $A3,$CC                 ; modes $0C-$11
LC4D6:  .byte   $08,$4A
LC4D8:  .byte   $69,$9C

; high bytes of handler addresses ($C4DA, 18 entries)
irq_vector_hi:  .byte   $C4             ; modes $00-$01
LC4DB:  .byte   $C1,$C1,$C1,$C2,$C2
LC4E0:  .byte   $C2,$C2
LC4E2:  .byte   $C2
LC4E3:  .byte   $C3
LC4E4:  .byte   $C3,$C3                 ; modes $0A-$11
LC4E6:  .byte   $C3,$C3
LC4E8:  .byte   $C4,$C4
LC4EA:  .byte   $C4,$C4

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
drain_ppu_buffer:  ldx     #$00
        stx     nametable_dirty                     ; clear scroll-dirty flag
drain_ppu_buffer_continue:  lda     $0780,x ; read PPU addr high byte
        bmi     LC51C                   ; $FF terminator → exit
        sta     $2006                   ; PPUADDR high
        lda     $0781,x
        sta     $2006                   ; PPUADDR low
        ldy     $0782,x                 ; Y = byte count
LC50D:  lda     $0783,x                 ; write tile data
        sta     $2007                   ; to PPUDATA
        inx
        dey
        bpl     LC50D                   ; loop count+1 times
        inx                             ; skip past addr_hi
        inx                             ; addr_lo
        inx                             ; count fields
        bne     drain_ppu_buffer_continue ; next buffer entry
LC51C:  rts

; ---------------------------------------------------------------------------
; disable_nmi — clear NMI enable + sprite bits in PPUCTRL
; ---------------------------------------------------------------------------
; Masks $FF (PPUCTRL shadow) to $11 (keep sprite table select + increment),
; clears NMI enable (bit 7) and sprite size (bit 5).
; Referenced from bank16 dispatch table at $168A68.
; ---------------------------------------------------------------------------
disable_nmi:

        lda     ppu_ctrl_shadow
        and     #$11                    ; keep bits 4,0 only
        sta     ppu_ctrl_shadow                     ; update shadow
        sta     $2000                   ; write PPUCTRL
        rts

; ---------------------------------------------------------------------------
; enable_nmi — set NMI enable bit in PPUCTRL
; ---------------------------------------------------------------------------
enable_nmi:

        lda     ppu_ctrl_shadow
        ora     #$80                    ; set bit 7 (NMI enable)
        sta     ppu_ctrl_shadow                     ; update shadow
        sta     $2000                   ; write PPUCTRL
        rts

; ---------------------------------------------------------------------------
; rendering_off — blank screen and increment frame-lock counter
; ---------------------------------------------------------------------------
; Sets PPUMASK ($2001) to $00 (all rendering off).
; $EE++ prevents task_yield from resuming entity processing.
; $FE = PPUMASK shadow.
; ---------------------------------------------------------------------------
rendering_off:

        inc     nmi_skip                     ; frame-lock counter++
        lda     #$00
        sta     ppu_mask_shadow                     ; PPUMASK shadow = $00
        sta     $2001                   ; all rendering off
        rts

; ---------------------------------------------------------------------------
; rendering_on — restore screen rendering and decrement frame-lock
; ---------------------------------------------------------------------------
; Sets PPUMASK ($2001) to $18 (show background + show sprites).
; $EE-- re-enables entity processing in task_yield.
; ---------------------------------------------------------------------------
rendering_on:

        dec     nmi_skip                     ; frame-lock counter--
        lda     #$18
        sta     ppu_mask_shadow                     ; PPUMASK shadow = $18
        sta     $2001                   ; BG + sprites on
        rts

; ===========================================================================
; read_controllers — read both joypads with DPCM-glitch mitigation
; ===========================================================================
; Standard NES controller read using the double-read technique: reads each
; controller via both bit 0 and bit 1 of $4016/$4017, then ORs results to
; compensate for DPCM channel interference on bit 0.
;
; After return:
;   $14 = player 1 new presses (edges only, not held)
;   $15 = player 2 new presses
;   $16 = player 1 held (raw state this frame)
;   $17 = player 2 held
;
; Simultaneous Up+Down or Left+Right are cancelled (D-pad bits cleared).
; Called by scheduler on task slot 0 resume (main game task gets input).
; ---------------------------------------------------------------------------

read_controllers:  ldx     #$01         ; strobe controllers
        stx     $4016                   ; (write 1 then 0 to latch)
        dex
        stx     $4016
        ldx     #$08                    ; 8 bits per controller
LC550:  lda     $4016                   ; player 1: bit 0 → $14
        lsr     a                       ; bit 1 → $00 (DPCM-safe)
        rol     joy1_press
        lsr     a
        rol     L0000
        lda     $4017                   ; player 2: bit 0 → $15
        lsr     a                       ; bit 1 → $01 (DPCM-safe)
        rol     joy1_press_alt
        lsr     a
        rol     $01
        dex
        bne     LC550
        lda     L0000                   ; OR both reads together
        ora     joy1_press                     ; to compensate for DPCM
        sta     joy1_press                     ; bit-0 corruption
        lda     $01
        ora     joy1_press_alt
        sta     joy1_press_alt

; --- edge detection: new presses = (current XOR previous) AND current ---
        ldx     #$01                    ; X=1 (P2), then X=0 (P1)
LC573:  lda     joy1_press,x                   ; Y = raw buttons this frame
        tay
        eor     joy1_held,x                   ; bits that changed from last frame
        and     joy1_press,x                   ; AND current = newly pressed only
        sta     joy1_press,x                   ; $14/$15 = new presses
        sty     joy1_held,x                   ; $16/$17 = held (raw) for next frame
        dex
        bpl     LC573

; --- cancel simultaneous opposite directions ---
        ldx     #$03                    ; check $14,$15,$16,$17
LC583:  lda     joy1_press,x                   ; Up+Down both pressed? ($0C)
        and     #$0C
        cmp     #$0C
        beq     LC593
        lda     joy1_press,x                   ; Left+Right both pressed? ($03)
        and     #$03
        cmp     #$03
        bne     LC599
LC593:  lda     joy1_press,x                   ; clear all D-pad bits,
        and     #$F0                    ; keep A/B/Select/Start
        sta     joy1_press,x
LC599:  dex
        bpl     LC583
        rts

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

fill_nametable:  sta     L0000          ; save parameters
        stx     $01                     ; $00=addr_hi, $01=fill, $02=attr
        sty     $02
        lda     $2002                   ; reset PPU latch
        lda     ppu_ctrl_shadow                     ; PPUCTRL: clear bit 0
        and     #$FE                    ; (horizontal increment mode)
        sta     $2000
        lda     L0000                   ; PPUADDR = addr_hi : $00
        sta     $2006
        ldy     #$00
        sty     $2006
        ldx     #$04                    ; nametable? 4 pages (1024 bytes)
        cmp     #$20                    ; CHR? Y pages (from parameter)
        bcs     LC5BF
        ldx     $02
LC5BF:  ldy     #$00                    ; 256 iterations per page
        lda     $01                     ; A = fill byte
LC5C3:  sta     $2007                   ; write fill byte
        dey                             ; inner: 256 bytes
        bne     LC5C3
        dex                             ; outer: X pages
        bne     LC5C3
        ldy     $02                     ; Y = attribute byte
        lda     L0000                   ; if addr < $20, skip attributes
        cmp     #$20
        bcc     LC5E6
        adc     #$02                    ; PPUADDR = (addr_hi+3):$C0
        sta     $2006                   ; ($20→$23C0, $24→$27C0)
        lda     #$C0                    ; (carry set from CMP above)
        sta     $2006
        ldx     #$40                    ; 64 attribute bytes
LC5E0:  sty     $2007                   ; write attribute byte
        dex
        bne     LC5E0
LC5E6:  ldx     $01                     ; restore X = fill byte
        rts

; ===========================================================================
; prepare_oam_buffer — clear unused OAM sprites and draw overlays
; ===========================================================================
; Called each frame before update_entity_sprites. Hides all OAM entries from
; the current write position ($97) to end of buffer by setting Y=$F8
; (off-screen). Then dispatches to overlay sprite routines based on flags:
;   $71 != 0 → load_overlay_sprites: copy sprite data from ROM, start scroll
;   $72 != 0 → scroll_overlay_sprites: slide overlay sprites leftward
;   $F8 == 2 → draw_scroll_sprites: draw camera-tracking sprites
;
; OAM layout:
;   $0200-$022F (sprites 0-11) — reserved for overlays when $72 is active
;   $0230+ (sprites 12+) — entity sprites, starting at $97
;
; $97 = OAM write index: $04 = with player, $0C = skip player, $30 = skip overlays
; $50 = pause flag (nonzero = skip overlay dispatch)
; $71 = overlay init trigger (set by palette_fade_tick when fade-in completes)
; $72 = overlay scroll active (nonzero = overlays visible, preserve OAM 0-11)
; 22 callers across banks $02, $0B, $0C, $18, $1E, $1F.
; ---------------------------------------------------------------------------

prepare_oam_buffer:  lda     player_state        ; state $07 = special_death:
        cmp     #PSTATE_SPECIAL_DEATH                    ; force $97=$6C (keep 27 sprites,
        bne     LC5F5                   ; clear rest)
        ldx     #$6C
        stx     oam_ptr
        bne     LC60A
LC5F5:  ldx     oam_ptr                     ; if $97==$04 (player slot only):
        cpx     #$04                    ; hide sprite 0 (NES sprite 0 hit
        bne     LC600                   ; detection — no longer needed)
        lda     #$F8
        sta     $0200
LC600:  lda     scroll_lock                     ; if paused ($50): clear from $97
        bne     LC60A
        lda     $72                     ; if overlay active ($72): start
        beq     LC60A                   ; clearing at $30 (preserve
        ldx     #$30                    ; sprites 0-11 = overlay area)
LC60A:  lda     #$F8                    ; Y=$F8 = off-screen (hide sprite)
LC60C:  sta     $0200,x                 ; write $F8 to Y byte of each
        inx                             ; OAM entry (4-byte stride)
        inx                             ; from X to end of buffer
        inx
        inx
        bne     LC60C
        lda     scroll_lock                     ; if paused: skip overlay dispatch
        bne     LC627
        lda     $71                     ; $71: load overlay sprites from ROM
        bne     load_overlay_sprites
        lda     $72                     ; $72: scroll overlay sprites
        bne     scroll_overlay_sprites
        lda     game_mode                     ; $F8==2: draw camera-tracking sprites
        cmp     #$02
        beq     draw_scroll_sprites
LC627:  rts

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
LC62A:  lda     #$00                    ; clear entity type (deactivate slot)
        sta     ent_status,x
        lda     #$FF                    ; $FF = palette-anim inactive
        sta     ent_spawn_id,x
        dex                             ; loop slots 31 down to 1
        bne     LC62A                   ; (BNE: skips slot 0 = player)
        lda     #$00                    ; clear overlay sprite flags
        sta     $71                     ; $71 = load overlay trigger
        sta     $72                     ; $72 = overlay scroll active
        rts

; --- load_overlay_sprites ---
; Copies 12 OAM entries from overlay_sprite_data ($C6D8) to OAM $0200-$022F.
; Tiles $F1/$F2, palette 2. Clears $71, sets $72 (activates scroll), $97=$30.

load_overlay_sprites:  lda     #$00     ; clear trigger flag (one-shot)
        sta     $71
        ldy     #$2C                    ; Y = $2C: 12 entries (0-11)
LC644:  lda     overlay_sprite_data,y   ; copy 4 OAM bytes per sprite:
        sta     $0200,y                 ; Y position
        lda     LC6D9,y                 ; tile index
        sta     $0201,y                 ; attribute
        lda     LC6DA,y                 ; X position
        sta     $0202,y
        lda     LC6DB,y
        sta     $0203,y
        dey                             ; next entry (Y -= 4)
        dey
        dey
        dey
        bpl     LC644                   ; loop while Y >= 0
        sty     $72                     ; Y=$FC (nonzero) → overlay scroll active
        lda     #$30                    ; $97=$30: entity sprites start at $0230
        sta     oam_ptr                     ; (12 overlay sprites reserved)
        rts

; --- scroll_overlay_sprites ---
; Slides overlay sprites leftward (X -= 1 per frame). Sprites 0-5 scroll
; every frame; sprites 6-11 scroll every other frame ($95 bit 0 = frame parity).
; This creates a parallax-like spread effect as the sprites fly across screen.

scroll_overlay_sprites:  ldy     #$14   ; sprites 0-5: X byte offsets $03-$17
LC66B:  lda     $0203,y                 ; X position -= 1
        sec
        sbc     #$01
        sta     $0203,y
        dey                             ; next sprite (Y -= 4)
        dey
        dey
        dey
        bpl     LC66B                   ; loop sprites 5 → 0
        lda     $95                     ; skip slow set on odd frames
        and     #$01                    ; ($95 = global frame counter)
        bne     LC691
        ldy     #$14                    ; sprites 6-11: X byte offsets $1B-$2F
LC682:  lda     $021B,y                 ; X position -= 1 (half speed)
        sec
        sbc     #$01
        sta     $021B,y
        dey                             ; next sprite (Y -= 4)
        dey
        dey
        dey
        bpl     LC682                   ; loop sprites 11 → 6
LC691:  lda     #$30                    ; $97=$30: entity sprites at $0230
        sta     oam_ptr
        rts

; --- draw_scroll_sprites ---
; Draws 8 camera-tracking sprites when $F8==2 (screen scroll mode).
; X positions are adjusted by camera scroll offset ($F9:$FC >> 2).
; Alternates between two 8-sprite sets based on frame parity ($95 bit 0):
;   even frames: scroll_sprite_data+$00 (Y offset 0)
;   odd frames:  scroll_sprite_data+$20 (Y offset 32)
; Tile $E4 (solid fill), palette 3. Sprites written to OAM $0200-$021F.

draw_scroll_sprites:  lda     camera_x_lo       ; compute camera X offset >> 2:
        sta     L0000                   ; $00 = ($F9:$FC) >> 2
        lda     camera_screen                     ; (coarse scroll position / 4)
        lsr     a
        ror     L0000
        lsr     a
        ror     L0000
        lda     $95                     ; Y = ($95 AND 1) << 5
        and     #$01                    ; even frames: Y=0, odd: Y=$20
        asl     a                       ; selects between two sprite sets
        asl     a
        asl     a
        asl     a
        asl     a
        tay
        ldx     #$1C                    ; 8 sprites (OAM $00-$1C, 4 bytes each)
LC6AE:  lda     scroll_sprite_data,y    ; Y position (from ROM table)
        sta     $0200,x
        lda     LC709,y                 ; tile index
        sta     $0201,x
        lda     LC70A,y                 ; attribute byte
        sta     $0202,x
        lda     LC70B,y                 ; X position = ROM value - scroll offset
        sec                             ; (sprites track camera movement)
        sbc     L0000
        sta     $0203,x
        iny                             ; advance source (Y += 4)
        iny
        iny
        iny
        dex                             ; advance dest (X -= 4)
        dex
        dex
        dex
        bpl     LC6AE                   ; loop 8 sprites
        lda     #$20                    ; $97=$20: entity sprites at $0220
        sta     oam_ptr                     ; (8 scroll sprites reserved)
        rts

; overlay_sprite_data: 12 OAM entries for load_overlay_sprites
; Format: Y, tile, attr, X (4 bytes per sprite)
; Sprites 0-5: tiles $F1, palette 2 — scroll fast (every frame)
; Sprites 6-11: tiles $F2, palette 2 — scroll slow (every other frame)

overlay_sprite_data:  .byte   $58       ; sprites 0-1
LC6D9:  .byte   $F1
LC6DA:  .byte   $02
LC6DB:  .byte   $28,$E0,$F1,$02,$28,$B8,$F1,$02
        .byte   $70,$20,$F1,$02,$A0,$68,$F1,$02
        .byte   $D0,$D8,$F1,$02,$D0,$90,$F2,$02
        .byte   $10,$40,$F2,$02,$58,$D0,$F2,$02
        .byte   $58,$78,$F2,$02,$80,$28,$F2,$02
        .byte   $D8,$A8,$F2,$02,$D8

; scroll_sprite_data: 16 OAM entries for draw_scroll_sprites (two 8-sprite sets)
; Format: Y, tile, attr, X (4 bytes per sprite), tile $E4, palette 3
; Set 0 (even frames): entries 0-7, set 1 (odd frames): entries 8-15
scroll_sprite_data:  .byte   $90        ; set 0, sprites 0-1
LC709:  .byte   $E4
LC70A:  .byte   $03
LC70B:  .byte   $18,$28,$E4,$03,$20,$68,$E4,$03
        .byte   $30,$58,$E4,$03,$60,$80,$E4,$03
        .byte   $70,$10,$E4,$03,$98,$58,$E4,$03
        .byte   $C0,$80,$E4,$03,$D0,$18,$E4,$03
        .byte   $10,$A0,$E4,$03,$48,$28,$E4,$03
        .byte   $58,$40,$E4,$03,$90,$98,$E4,$03
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
        bne     fade_start
fade_palette_in:  lda     #$10          ; start bright (subtract $10)
        tax                             ; step = +$10 (darken each pass)
fade_start:  sta     $0F                ; $0F = current subtract amount
        stx     $0D                     ; $0D = step delta per pass
        ldy     #$04
        sty     $0E                     ; $0E = frames to yield per step
LC75D:  ldy     #$1F
LC75F:  lda     $0620,y                 ; copy target → active palette
        sta     $0600,y
        dey
        bpl     LC75F
        ldy     #$1F
LC76A:  lda     $0600,y                 ; subtract from each color
        sec
        sbc     $0F
        bpl     LC774
        lda     #$0F                    ; clamp to $0F (NES black)
LC774:  sta     $0600,y
        dey
        bpl     LC76A
        sty     palette_dirty                     ; $18 = palette dirty ($FF)
        lda     $0E                     ; A = $0E (frames to wait per fade step)
LC77E:  pha                             ; wait $0E frames between fade steps
        jsr     task_yield              ; (lets NMI upload palette to PPU)
        pla
        sec
        sbc     #$01
        bne     LC77E
        lda     $0F                     ; advance subtract by step
        clc
        adc     $0D
        sta     $0F
        cmp     #$50                    ; done when subtract reaches $50
        beq     LC797
        lda     $0F
        bpl     LC75D                   ; or wraps negative
LC797:  rts

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
        beq     LC7DC                   ; no → skip
        lda     $95                     ; frame counter
        and     #$03                    ; every 4th frame
        bne     LC7DC
        ldy     #$1F
LC7A4:  lda     $0620,y                 ; copy target → active
        sta     $0600,y
        dey
        bpl     LC7A4
        ldy     #$0F                    ; BG palette only ($0600-$060F)
LC7AF:  lda     $0600,y
        sec
        sbc     $1D                     ; subtract current fade amount
        bpl     LC7B9
        lda     #$0F                    ; clamp to $0F
LC7B9:  sta     $0600,y
        dey
        bpl     LC7AF
        sty     palette_dirty                     ; palette dirty
        lda     $1D                     ; advance fade amount
        clc
        adc     $1E                     ; $1E = step delta
        sta     $1D
        cmp     #$F0
        beq     LC7D4                   ; $F0 = fully black
        cmp     #$50
        bne     LC7DC
        inc     $71                     ; $50 = fully restored
        bne     LC7D8
LC7D4:  lda     #$00
        sta     $72                     ; $72=0 signals halted
LC7D8:  lda     #$00
        sta     $1C                     ; deactivate fade
LC7DC:  rts

; ---------------------------------------------------------------------------
; indirect_dispatch — jump through word table using A as index
; ---------------------------------------------------------------------------
; A = dispatch index. Reads return address from stack to find the table
; base (word table immediately follows the JSR to this routine).
; Computes table[A*2] and JMPs to that address. Preserves X, Y.
; ---------------------------------------------------------------------------
indirect_dispatch:

        stx     $0E                     ; save X, Y
        sty     $0F
        asl     a                       ; A * 2 (word index)
        tay
        iny                             ; +1 (return addr is 1 before table)
        pla                             ; pop return address low
        sta     L000C
        pla                             ; pop return address high
        sta     $0D
        lda     (L000C),y               ; read target addr low
        tax
        iny
        lda     (L000C),y               ; read target addr high
        sta     $0D
        stx     L000C
        ldx     $0E                     ; restore X, Y
        ldy     $0F
        jmp     (L000C)                 ; jump to target

; --- shift_register_tick ---
; 32-bit LFSR (linear feedback shift register) on $E4-$E7.
; Feedback: XOR of bit 1 of $E4 and bit 1 of $E5 → carry → ROR through
; all 4 bytes ($E4→$E5→$E6→$E7). Called once per frame from game loop.
; $E4 initialized to $88 in main_game_entry. Used for animation cycling.

shift_register_tick:  ldx     #$00
        ldy     #$04                    ; 4 bytes to rotate
        lda     $E4,x                   ; feedback = ($E4 bit 1) XOR ($E5 bit 1)
        and     #$02                    ; isolate bit 1 of $E4
        sta     L0000
        lda     $E5,x                   ; isolate bit 1 of $E5
        and     #$02
        eor     L0000                   ; XOR → nonzero if bits differ
        clc                             ; carry = 0 if bits match,
        beq     LC80F                   ; carry = 1 if bits differ
        sec
LC80F:  ror     $E4,x                   ; rotate carry → bit 7,
        inx                             ; old bit 0 → carry,
        dey                             ; cascading through $E4-$E7
        bne     LC80F
        rts

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

load_stage:  lda     stage_id                ; switch to stage's PRG bank
        sta     prg_bank                     ; ($A000-$BFFF = stage data)
        jsr     select_PRG_banks
        lda     $AA80                   ; load palette indices
        sta     $E8                     ; from stage bank
        lda     $AA81
        sta     $E9
        ldx     #$07                    ; copy default palette
code_C829:  lda     LC898,x             ; to sprite palette RAM
        sta     $0610,x
        sta     $0630,x
        dex
        bpl     code_C829
        lda     #$00                    ; $EA = page 0 (sprite tiles $00-$3F)
        sta     $EA                     ; shared across all stages
        lda     #$01                    ; $EB = page 1 (sprite tiles $40-$7F)
        sta     $EB                     ; shared across all stages

; --- load_room: read room data from $AA60[$2B] ---
; Called at stage start and on room transitions.
; $2B = current room/section index.
load_room:  lda     $2B                 ; X = room index * 2
        asl     a                       ; (2 bytes per room in $AA60)
        tax
        lda     $AA60,x                 ; CHR/palette param from room table
        pha                             ; (saved for bank $01 $A000 call below)
        lda     $AA61,x                 ; layout index → offset into $AA82
        asl     a                       ; multiply by 20:
        asl     a                       ; *4 → $00
        sta     L0000                   ; *16
        asl     a                       ; *16 + *4 = *20
        asl     a                       ; (20 bytes per layout entry:
        adc     L0000                   ; 16 column IDs + 4 connection)
        tay
        ldx     #$00
LC853:  lda     $AA82,y                 ; copy 16 metatile column IDs
        sta     $0600,x                 ; to $0600-$060F (current screen)
        sta     $0620,x                 ; and $0620-$062F (mirror)
        iny
        inx
        cpx     #$10
        bne     LC853
        ldx     #$00
LC864:  lda     $AA82,y                 ; parse 4 screen connection bytes
        pha                             ; (up/down/left/right exits):
        and     #$80                    ; bit 7 → $0100,x (scroll direction)
        sta     $0100,x                 ; ($80=scroll, $00=warp)
        pla
        and     #$7F                    ; bits 0-6 → $0108,x (target screen#)
        sta     $0108,x
        lda     #$00                    ; clear $0104,x and $010C,x
        sta     $0104,x                 ; (unused padding/high bytes)
        sta     $010C,x
        iny
        inx
        cpx     #$04                    ; 4 connections (U/D/L/R)
        bne     LC864
        lda     $0600                   ; copy first column ID to palette slots
        sta     $0610                   ; (overwritten by $A000 below)
        sta     $0630
        lda     #$01                    ; switch to bank $01 (CHR/palette tables)
        sta     prg_bank
        jsr     select_PRG_banks
        pla                             ; A = CHR/palette param from $AA60
        jsr     LA000                   ; → sets $EC/$ED from $A200[param*2]
        jmp     update_CHR_banks        ; → and SP2-SP3 from $A030[param*8]

; default_sprite_palette: sprite palette 0-1 defaults (8 bytes)
; SP0: $0F(black), $0F(black), $2C(sky blue), $11(blue)
; SP1: $0F(black), $0F(black), $30(white), $37(orange)

LC898:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37

; ---------------------------------------------------------------------------
; ensure_stage_bank — switch $F5 to current stage's PRG bank if needed
; ---------------------------------------------------------------------------
; Looks up stage_to_bank[$22] and switches $F5. No-op if $F5 is already $13
; (bank $13 = fixed/shared). Preserves X and Y.
; ---------------------------------------------------------------------------
ensure_stage_bank:  txa
        pha
        tya
        pha
        lda     prg_bank                     ; already on bank $13?
        cmp     #$13
        beq     LC8B4                   ; yes → skip
        ldy     stage_id                     ; stage index
        lda     LC8B9,y                 ; look up stage → bank
        sta     prg_bank
        jsr     select_PRG_banks        ; switch bank
LC8B4:  pla
        tay
        pla
        tax
        rts

; stage_to_bank: maps stage index ($22) → PRG bank ($F5)
; $00=Needle $01=Magnet $02=Gemini $03=Hard $04=Top $05=Snake $06=Spark $07=Shadow
; $08-$0B=Doc Robot (Needle/Gemini/Spark/Shadow stages)
; $0C-$0F=Wily fortress, $10+=special/ending

LC8B9:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
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
        txs
        lda     ppu_ctrl_shadow                     ; sync PPUCTRL from shadow
        sta     $2000
        jsr     task_yield              ; yield 1 frame (let NMI run)
        lda     #$88                    ; $E4 = $88 (CHR bank base?)
        sta     $E4
        cli                             ; enable IRQ
        lda     #$01                    ; $9B = 1 (game active flag)
        sta     irq_enable
        lda     #$00                    ; silence all sound channels
        jsr     submit_sound_ID_D9
        lda     #$40                    ; $99 = $40 (initial $99,
        sta     gravity                     ; set to $55 once gameplay starts)
        lda     #$18                    ; map bank $18 to $8000-$9FFF
        sta     mmc3_select
        jsr     select_PRG_banks
        jsr     L9009                   ; title screen / stage select (bank $18)
        lda     #$9C                    ; $A9 = $9C = full Rush Coil ammo
        sta     $A9
        lda     #$02                    ; $AE = 2 $AE (display as 3)
        sta     lives

; --- stage_reinit: re-entry point after death/boss dispatch ---
; Clears $0150-$016F (entity scratch buffer), then falls through to stage_init.
code_C8FF:  lda     #$00                ; clear $0150-$016F (32 bytes)
        ldy     #$1F
LC903:  sta     $0150,y
        dey
        bpl     LC903

; --- stage_init: set up a new stage ---
stage_init:  lda     #$00               ; $EE = 0: allow NMI rendering
        sta     nmi_skip

; --- initialize display and entity state ---
        jsr     fade_palette_in         ; fade screen to black
        lda     #$04                    ; $97 = 4 (OAM scan start offset)
        sta     oam_ptr
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; zero all 32 entity slots
        lda     #$01                    ; $A000 = 1 (H-mirroring)
        sta     LA000

; --- zero-init game state variables ---
        lda     #$00
        sta     game_mode                     ; scroll mode = 0 (normal)
        sta     ent_status                   ; player entity inactive
        sta     camera_x_hi                     ; nametable select = 0
        sta     $71                     ; overlay init trigger = 0
        sta     $72                     ; overlay scroll active = 0
        sta     camera_x_lo                     ; camera X low = 0
        sta     scroll_y                     ; scroll Y offset = 0
        sta     camera_screen                     ; starting screen = 0
        sta     $25                     ; camera X coarse = 0
        sta     ent_x_scr                   ; player X screen = 0
        sta     ent_y_scr                   ; player Y screen = 0
        sta     $B1                     ; HP bar state = 0
        sta     $B2                     ; boss bar state = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     $9E                     ; scroll X fine = 0
        sta     $9F                     ; scroll X fine copy = 0
        sta     boss_active                     ; boss active flag = 0
        sta     current_weapon                     ; weapon = $00 (Mega Buster)
        sta     $B4                     ; ammo slot index = 0
        sta     weapon_cursor                     ; weapon menu cursor = 0
        sta     $6F                     ; current screen = 0

; --- start stage music ---
        ldy     stage_id                     ; Y = stage number
        lda     LCD0C,y                 ; load stage music ID
        jsr     submit_sound_ID_D9      ; play music

; --- set initial scroll/render state ---
        lda     #$01
        sta     player_facing                     ; player facing = right (1=R, 2=L)
        sta     $23                     ; scroll column state = 1
        sta     $2E                     ; scroll direction = right
        lda     #$FF                    ; $29 = $FF (render-done sentinel;
        sta     $29                     ; BEQ loops while 0, skips on $FF)
        lda     #$1F                    ; $24 = 31 (column counter for
        sta     $24                     ; initial nametable fill)

; --- fill nametable: render columns until $29 becomes nonzero ---
code_C969:  lda     #$01                ; $10 = 1 (render direction: right)
        sta     $10
        jsr     do_render_column        ; render one BG column
        jsr     task_yield              ; yield to NMI (display frame)
        lda     $29                     ; loop until rendering complete
        beq     code_C969               ; ($29 set to nonzero by renderer)

; --- parse room 0 config from $AA40 ---
        lda     stage_id                     ; switch to stage data bank
        sta     prg_bank
        jsr     select_PRG_banks
        lda     $AA40                   ; room 0 config byte
        pha                             ; save for screen count
        and     #$E0                    ; bits 7-5: scroll/connection flags
        sta     $2A                     ; $2A = room scroll/connection flags
        ldx     #$01                    ; default: X=1 (H-mirror), Y=$2A
        ldy     #$2A
        and     #$C0                    ; bits 7-6: vertical connection
        beq     code_C991               ; if no vertical connection: H-mirror
        dex                             ; else: X=0 (V-mirror), Y=$26
        ldy     #$26
code_C991:  stx     LA000               ; set mirroring mode
        sty     $52                     ; $52 = nametable height ($2A or $26)
        pla                             ; bits 4-0 of room config
        and     #$1F                    ; = screen count
        sta     $2C                     ; $2C = screen count for room 0
        lda     #$00
        sta     $2B                     ; $2B = current room = 0
        sta     $2D                     ; $2D = room base screen = 0

; --- load stage data (palettes, CHR, nametable attributes) ---
        jsr     load_stage              ; load stage palettes + CHR banks
        lda     #$00                    ; $18 = 0 (rendering disabled during setup)
        sta     palette_dirty
        jsr     task_yield
        jsr     fade_palette_out        ; fade from black → stage palette
        lda     #$80                    ; player X = $80 (128, center of screen)
        sta     ent_x_px

; --- set HP and scroll mode for stage type ---
code_C9B3:  lda     #$9C                ; $A2 = $9C (full HP: 28 bars)
        sta     player_hp
        lda     #$E8                    ; default: $51/$5E = $E8 (normal scroll)
        sta     $51
        sta     $5E
        lda     camera_screen                     ; if not starting at screen 0: skip
        bne     code_C9D3
        lda     stage_id                     ; stages $02 (Gemini) and $09 (DR-Gemini)
        cmp     #STAGE_GEMINI                    ; use horizontal scroll mode
        beq     code_C9CB               ; (start scrolling right from screen 0)
        cmp     #STAGE_DOC_GEMINI
        bne     code_C9D3
code_C9CB:  lda     #$9F                ; $5E = $9F (Gemini scroll end marker)
        sta     $5E
        lda     #$02                    ; $F8 = 2 (screen scroll mode)
        sta     game_mode

; --- set intro CHR banks and begin fade-in ---
code_C9D3:  lda     #$74                ; $EA/$EB = $74/$75 (intro/ready CHR pages)
        sta     $EA
        lda     #$75
        sta     $EB
        jsr     update_CHR_banks
        lda     #$30                    ; $0611 = $30 (BG palette color for intro)
        sta     $0611
        inc     palette_dirty                     ; enable rendering ($18 = 1)
        lda     #$3C                    ; A = 60 (fade-in frame count)

; --- fade-in loop: 60 frames with flashing "READY" overlay ---
code_C9E7:  pha                         ; save frame countdown
        lda     $95                     ; frame counter bit 4:
        and     #$10                    ; toggles every 16 frames
        beq     code_CA10               ; if clear → hide overlay

; show "READY" overlay: copy 5 OAM entries from $CCF8 table
        ldx     #$10                    ; X = $10 (5 sprites × 4 bytes - 4)
code_C9F0:  lda     LCCF8,x             ; copy OAM entry: Y, tile, attr, X
        sta     $0200,x
        lda     LCCF9,x
        sta     $0201,x
        lda     LCCFA,x
        sta     $0202,x
        lda     LCCFB,x
        sta     $0203,x
        dex
        dex
        dex
        dex                             ; next OAM entry
        bpl     code_C9F0
        lda     #$14                    ; A = 20 (start hiding at sprite 5)

; hide remaining OAM entries: set Y = $F8 (off-screen) from offset A onward
code_CA10:  sta     oam_ptr                 ; $97 = OAM scan start offset
        tax
        lda     #$F8                    ; Y = $F8 = off-screen
code_CA15:  sta     $0200,x             ; hide sprite: Y = $F8
        inx
        inx
        inx
        inx                             ; next OAM entry (4 bytes)
        bne     code_CA15               ; until X wraps to 0
        lda     #$00                    ; $EE = 0 (allow NMI rendering)
        sta     nmi_skip
        jsr     task_yield              ; yield to NMI (display frame)
        inc     nmi_skip                     ; $EE = 1 (suppress next NMI rendering)
        inc     $95                     ; advance frame counter
        pla                             ; decrement fade-in countdown
        sec
        sbc     #$01
        bne     code_C9E7               ; loop until 60 frames complete

; --- fade-in complete: set up gameplay CHR and spawn player ---
        lda     #$0F                    ; $0611 = $0F (BG color → black)
        sta     $0611
        inc     palette_dirty                     ; advance rendering state
        lda     #$00                    ; $EA/$EB = pages 0/1 (shared sprite CHR)
        sta     $EA                     ; R2: tiles $00-$3F at $1000
        lda     #$01                    ; R3: tiles $40-$7F at $1400
        sta     $EB
        jsr     update_CHR_banks        ; switch to gameplay sprite CHR

; --- initialize player entity (slot 0) for teleport-in ---
        lda     #$80                    ; ent_status = $80 (player entity active)
        sta     ent_status
        lda     #$00                    ; ent_y_px = 0 (player Y = top of screen)
        sta     ent_y_px
        lda     #$D0                    ; ent_flags = $D0 (bit7=drawn, bit6=face right, bit4=set)
        sta     ent_flags
        lda     #$4C                    ; player X speed = $01.4C (walk speed, pre-loaded
        sta     ent_xvel_sub                   ; for when reappear transitions to on_ground)
        lda     #$01
        sta     ent_xvel
        lda     #$00                    ; player Y speed = $F9.00 (terminal fall velocity)
        sta     ent_yvel_sub                   ; player drops from top during reappear state
        lda     #$F9
        sta     ent_yvel
        ldx     #$00                    ; slot 0 (player): set OAM $13 = teleport beam
        lda     #$13
        jsr     reset_sprite_anim
        lda     #PSTATE_REAPPEAR                    ; $30 = $04 (player state = reappear)
        sta     player_state
        lda     #$80                    ; $B2 = $80 (stage initialization complete)
        sta     $B2

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
gameplay_frame_loop:  lda     joy1_press       ; Start button pressed?
        and     #BTN_START                    ; ($14 = new button presses this frame)
        beq     gameplay_no_pause
        lda     player_state                     ; skip pause if player state is:
        cmp     #PSTATE_REAPPEAR                    ; $04 = reappear
        beq     gameplay_no_pause       ; $07 = special_death
        cmp     #PSTATE_SPECIAL_DEATH                    ; $09+ = boss_wait and above
        beq     gameplay_no_pause
        cmp     #PSTATE_BOSS_WAIT
        bcs     gameplay_no_pause

; --- pause check: despawn Rush (slot 1) when switching from Rush weapon ---
        lda     current_weapon                     ; weapon ID - 6: Rush weapons are $06-$0B
        sec                             ; carry clear = weapon < $06 (not Rush)
        sbc     #$06
        bcc     code_CA99
        and     #$01                    ; odd result = Rush entity active ($07,$09,$0B)
        beq     code_CA99               ; even = Rush item ($06,$08,$0A) — skip
        lda     #$00                    ; despawn slot 1 (Rush entity)
        sta     $0301                   ; before opening pause menu
        beq     code_CAA4
code_CA99:  lda     $0301               ; if ANY weapon slot (1-3) is active,
        ora     $0302                   ; don't allow pause
        ora     $0303                   ; (prevents pausing mid-shot)
        bmi     gameplay_no_pause
code_CAA4:  ldy     player_state                 ; check per-state pause permission table
        lda     LCD1E,y                 ; bit 7 set = pause not allowed
        bmi     gameplay_no_pause
        lda     #$02                    ; switch to bank $02/$03 (pause menu code)
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     LA003                   ; call pause menu handler
gameplay_no_pause:  lda     stage_id         ; switch to stage bank
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     code_CD34               ; player state dispatch + physics

; --- apply pending hazard state transition (set by tile collision) ---
        lda     hazard_pending                     ; $3D = pending state from hazard
        beq     code_CAD7               ; 0 = no pending state
        sta     player_state                     ; apply: set player state
        cmp     #$0E                    ; state $0E = spike/pit death?
        bne     code_CAD3
        lda     #$F2                    ; play death jingle ($F2 = stop music)
        jsr     submit_sound_ID
        lda     #$17                    ; play death sound $17
        jsr     submit_sound_ID
code_CAD3:  lda     #$00                ; clear pending state
        sta     hazard_pending
code_CAD7:  jsr     update_camera       ; scroll/camera update
        lda     camera_x_lo                     ; $25 = camera X (coarse)
        sta     $25
        sta     L0000
        lda     game_mode                     ; if scroll mode ($F8==2):
        cmp     #$02                    ; compute camera offset for
        bne     LCAF2                   ; scroll sprites
        lda     camera_screen
        lsr     a
        ror     L0000
        lsr     a
        ror     L0000
        lda     L0000
        sta     $5F                     ; $5F = camera offset / 4
LCAF2:  lda     ent_x_scr                   ; track max screen progress
        cmp     $6F                     ; ($6F = farthest screen reached)
        bcc     LCAFB
        sta     $6F
LCAFB:  lda     ent_x_px                   ; $27 = player Y position
        sta     $27
        ldx     #$1C                    ; select banks $1C/$1D
        stx     mmc3_select                     ; (entity processing code)
        inx
        stx     prg_bank
        jsr     select_PRG_banks
        jsr     L8000                   ; process all entity AI
        lda     #$1A
        sta     mmc3_select
        lda     stage_id
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     L9C00                   ; spawn enemies for current screen
        lda     #$09                    ; select bank $09
        sta     mmc3_select                     ; (per-frame subsystems)
        jsr     select_PRG_banks
        jsr     L8003                   ; bank $09 subsystems:
        jsr     L8006                   ; screen scroll, HUD update,
        jsr     L800F                   ; sound processing,
        jsr     L8009                   ; background animation,
        jsr     L800C                   ; item pickup,
        jsr     L8000                   ; checkpoint tracking,
        jsr     L8012                   ; etc.
        jsr     palette_fade_tick       ; update palette fade animation
        jsr     process_frame_yield_with_player ; build OAM + yield 1 frame
        lda     $98                     ; if controller 2 bit 1 set:
        and     #$02                    ; set $17 bit 0 (debug flag?)
        beq     LCB49
        lda     #$01
        ora     $17
        sta     $17
LCB49:  jsr     shift_register_tick     ; LFSR animation tick
        lda     $59                     ; $59 != 0: boss defeated
        bne     code_CB5B
        lda     $3C                     ; $3C != 0: player died
        bne     death_handler
        lda     $74                     ; $74 != 0: stage clear
        bne     stage_clear_handler
        jmp     gameplay_frame_loop     ; loop back for next frame

; --- boss_defeated_handler ($59 != 0) ---

code_CB5B:  lda     #$00                ; clear rendering/overlay/scroll state
        sta     nmi_skip
        sta     $71
        sta     $72
        sta     game_mode
        sta     boss_active                     ; clear boss-active flag
        lda     #$18                    ; select banks $18/$10
        sta     mmc3_select                     ; ($10 = boss post-defeat routine)
        lda     #$10
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     L9000                   ; boss post-defeat (bank $10)
        jmp     code_C8FF               ; → stage_reinit

; --- death_handler ($3C != 0) ---

death_handler:  lda     #$00            ; clear death flag + boss state
        sta     $3C
        sta     boss_active
        lda     lives                     ; decrement $AE (BCD format)
        sec
        sbc     #$01
        bcc     game_over               ; underflow → game over
        sta     lives                     ; BCD correction: if low nibble
        and     #$0F                    ; wraps to $0F, subtract 6
        cmp     #$0F                    ; ($10 - $01 = $0F → $09)
        bne     LCB94
        lda     lives
        sec
        sbc     #$06
        sta     lives
LCB94:  lda     #$00                    ; clear rendering/overlay state
        sta     nmi_skip
        sta     $71
        sta     $72
        sta     game_mode
        lda     stage_select_page                     ; if $60 == $12 (Wily stage):
        cmp     #$12                    ; special respawn path
        beq     code_CBA7
        jmp     handle_checkpoint       ; normal respawn at checkpoint

; Wily stage death → bank $18 $9006 (Wily-specific respawn)

code_CBA7:  lda     #$18                ; select bank $18
        sta     mmc3_select
        jsr     select_PRG_banks
        jmp     L9006                   ; Wily respawn (bank $18)

; --- game_over ($AE underflowed) ---

game_over:  lda     #$00                ; clear all state
        sta     nmi_skip
        sta     boss_active
        sta     $71
        sta     $72
        sta     game_mode
        lda     #$18                    ; select banks $18/$13
        sta     mmc3_select                     ; (game over screen)
        lda     #$13
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     L9003                   ; game over sequence (bank $18)
        jmp     code_C8FF               ; → stage_reinit (continue/retry)

; --- stage_clear_handler ($74 != 0: stage completion triggered) ---
; $74 bit 7 distinguishes: 0 = normal stage clear, 1 = special/Wily clear.

stage_clear_handler:  pha               ; save $74 (stage clear type)
        lda     #$00                    ; clear game state for transition
        sta     boss_active                     ; boss active = 0
        sta     $74                     ; stage clear trigger = 0
        sta     $B1                     ; HP bar = 0
        sta     $B2                     ; boss bar = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     boss_active                     ; boss active = 0 (redundant)
        sta     camera_screen                     ; starting screen = 0
        sta     game_mode                     ; scroll mode = 0
        lda     #$0B                    ; switch to banks $0B/$0E
        sta     mmc3_select                     ; (bank $0E = stage clear/results handler)
        lda     #$0E
        sta     prg_bank
        jsr     select_PRG_banks
        pla                             ; $74 AND $7F: 0 = normal stage clear
        and     #$7F
        bne     code_CBF7               ; nonzero = special clear (Wily/Doc Robot)
        jsr     L8000                   ; normal stage clear sequence (bank $0E)
        jmp     code_C8FF               ; → stage_reinit

; --- special stage clear (Wily/Doc Robot stages) ---

code_CBF7:  lda     $75                 ; $75 = stage clear sub-type
        cmp     #$06                    ; $06 = final boss / ending
        beq     code_CC03
        jsr     L8003                   ; Wily/Doc Robot clear (bank $0E)
        jmp     code_C8FF               ; → stage_reinit

; --- game ending sequence ---

code_CC03:  lda     #$0C                ; switch to banks $0C/$0E
        sta     mmc3_select                     ; (bank $0E = ending handler)
        lda     #$0E
        sta     prg_bank
        jsr     select_PRG_banks
        lda     #$11                    ; $F8 = $11 (ending game mode)
        sta     game_mode
        jsr     L8000                   ; run ending sequence
        jmp     code_C8FF               ; → stage_reinit (title screen)

handle_checkpoint:  lda     stage_id         ; store current level
        sta     prg_bank                     ; as $A000-$BFFF bank
        jsr     select_PRG_banks        ; and swap banks to it
        ldy     #$00                    ; loop through checkpoint data
LCC21:  lda     $6F                     ; if current checkpoint >
        cmp     $AAF8,y                 ; current screen ID
        bcc     LCC2B                   ; we're done & 1 too far
        iny
        bne     LCC21                   ; next checkpoint
LCC2B:  dey                             ; we're one checkpoint too far so
        bpl     LCC31                   ; go back one, if it's negative
        jmp     stage_init              ; we're just at the beginning

; --- checkpoint_restore: reset game state and reload from checkpoint ---

LCC31:  tya                             ; save checkpoint index
        pha
        jsr     fade_palette_in         ; fade screen to black
        lda     #$04                    ; $97 = 4 (OAM scan start)
        sta     oam_ptr
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; zero all entity slots

; --- zero game state (same as stage_init, minus music/facing) ---
        lda     #$00
        sta     game_mode                     ; scroll mode = 0
        sta     camera_x_hi                     ; nametable select = 0
        sta     $71                     ; overlay init trigger = 0
        sta     $72                     ; overlay scroll active = 0
        sta     camera_x_lo                     ; camera X low = 0
        sta     scroll_y                     ; scroll Y offset = 0
        sta     $25                     ; camera X coarse = 0
        sta     ent_y_scr                   ; player Y screen = 0
        sta     $B1                     ; HP bar state = 0
        sta     $B2                     ; boss bar state = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     boss_active                     ; boss active flag = 0
        sta     current_weapon                     ; weapon = $00 (Mega Buster)
        sta     $B4                     ; ammo slot index = 0
        sta     weapon_cursor                     ; weapon menu cursor = 0

; --- restore checkpoint position data ---
        pla                             ; restore checkpoint index
        tay
        lda     $AAF8,y                 ; set screen from checkpoint table
        sta     $29                     ; $29 = render sentinel/screen
        sta     camera_screen                     ; $F9 = starting screen
        sta     ent_x_scr                   ; player X screen
        lda     $ABC0,y                 ; set scroll fine position from checkpoint
        sta     $9E
        sta     $9F
        ldx     $AAF0,y                 ; $2B = checkpoint room number
        stx     $2B

; --- parse room config bits (same logic as stage_init) ---
        lda     $AA40,x                 ; load room config byte
        pha                             ; save for screen count later
        sta     L0000
        and     #$20                    ; bit 5 = horizontal scroll?
        sta     $2A                     ; if set, $2A = $20 (h-scroll flags)
        bne     code_CC8E
        lda     L0000                   ; else $2A = bits 7-6 (vertical connection)
        and     #$C0
        sta     $2A
code_CC8E:  ldx     #$01                ; default: H-mirror, viewport height $2A
        ldy     #$2A
        and     #$20                    ; if h-scroll (bit 5 set): keep defaults
        bne     code_CC99
        dex                             ; else: V-mirror (X=0), viewport $26
        ldy     #$26
code_CC99:  stx     LA000               ; set nametable mirroring (0=V, 1=H)
        sty     $52                     ; $52 = viewport height ($2A or $26)
        pla                             ; $2C = screen count (lower 5 bits)
        and     #$1F
        sta     $2C
        lda     #$00                    ; $2D = 0 (scroll progress within room)
        sta     $2D
        ldy     stage_id                     ; load stage music ID from $CD0C table
        lda     LCD0C,y                 ; and start playing it
        jsr     submit_sound_ID_D9
        lda     #$01
        sta     player_facing                     ; $31 = face right
        sta     $23                     ; $23 = column render flag
        sta     $2E                     ; $2E = render dirty flag
        dec     $29                     ; $29-- (back up screen sentinel for render)
        lda     #$1F                    ; $24 = $1F (screen attribute pointer)
        sta     $24

; --- render 33 columns of room tiles (one per frame, yielding to NMI) ---
        lda     #$21                    ; 33 columns ($21)
code_CCBF:  pha                         ; save column counter
        lda     #$01
        sta     $10                     ; $10 = 1 (render one column)
        jsr     do_render_column        ; render current column
        jsr     task_yield              ; yield (NMI uploads to PPU)
        pla
        sec                             ; decrement counter
        sbc     #$01
        bne     code_CCBF               ; loop until all 33 done
        jsr     load_stage              ; apply CHR/palette for this room
        lda     #$00                    ; suppress palette upload this frame
        sta     palette_dirty
        jsr     task_yield              ; yield to let NMI run
        jsr     fade_palette_out        ; fade screen to black
        lda     #$80                    ; spawn player at Y=$80 (mid-screen)
        sta     ent_x_px
        jmp     code_C9B3               ; → continue stage_init (Gemini scroll, fade-in)

; --- refill_all_ammo: fill all weapon ammo to $9C (full) ---
; Called when $98 mode indicates refill (e.g. E-Tank usage).

        lda     $98
        and     #$03                    ; check if mode == 1 (refill)
        cmp     #$01
        bne     code_CCF7               ; if not, skip
        ldy     #$0B                    ; fill $A2-$AD (12 ammo slots)
LCCEF:  lda     #$9C                    ; all to $9C = full (28 units)
code_CCF1:  .byte   $99
LCCF2:  ldx     #$00
        dey
        bpl     code_CCF1
code_CCF7:  .byte   $60
LCCF8:  .byte   $80
LCCF9:  .byte   $40
LCCFA:  .byte   $00
LCCFB:  .byte   $6C,$80,$41,$00,$74,$80,$42,$00
        .byte   $7C,$80,$43,$00,$84,$80,$44,$00
        .byte   $8C
LCD0C:  .byte   $01,$02,$03,$04,$05,$06,$07,$08
        .byte   $01,$03,$07,$08,$09,$09,$0A,$0A
        .byte   $0B,$0B
LCD1E:  .byte   $00,$00,$FF,$00,$FF,$00,$FF,$FF
        .byte   $00,$FF,$00,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF
        .byte   $FF

; ===========================================================================
; player_state_dispatch_prelude — pre-process before running state handler
; ===========================================================================
; Resets walk speed (unless sliding), handles platform push, invincibility
; timer, and charge shot (Needle auto-fire).
code_CD34:  lda     ent_xvel               ; if X speed whole == $02 (slide speed):
        cmp     #$02                    ; and state == $02 (sliding):
        bne     code_CD41               ; preserve slide speed
        lda     player_state
        cmp     #PSTATE_SLIDE
        beq     code_CD4B
code_CD41:  lda     #$4C                ; reset walk speed to $01.4C
        sta     ent_xvel_sub
        lda     #$01
        sta     ent_xvel

; --- per-frame setup ---
code_CD4B:  lda     #$40                ; $99 = $40 (player hitbox size?)
        sta     gravity
        ldx     #$00                    ; X = 0 (player slot), clear collision entity
        stx     $5D
        lda     $36                     ; if platform pushing: apply push velocity
        beq     code_CD5A
        jsr     code_CDEC               ; (platform_push handler)

; --- invincibility timer ---
code_CD5A:  lda     invincibility_timer                 ; $39 = invincibility timer
        beq     code_CD6A               ; zero → skip
        dec     invincibility_timer                     ; decrement timer
        bne     code_CD6A               ; not expired → skip
        lda     ent_anim_frame                   ; clear animation lock (bit 7)
        and     #$7F                    ; when invincibility expires
        sta     ent_anim_frame

; --- charge shot: Needle Cannon auto-fire ($1F counter) ---
code_CD6A:  lda     joy1_held                 ; B button held?
        and     #BTN_B
        bne     code_CD76               ; yes → increment charge
        lda     #$E0                    ; not held: reset charge to $E0
        sta     $1F                     ; (high value = "not charging")
        bne     code_CD8B               ; → dispatch state
code_CD76:  lda     $1F                 ; increment charge counter by $20
        clc                             ; wraps to 0 after 8 increments
        adc     #$20
        sta     $1F
        bne     code_CD8B               ; not wrapped → dispatch state
        lda     current_weapon                     ; if weapon == $02 (Needle Cannon):
        cmp     #WPN_NEEDLE                    ; auto-fire when charge wraps
        bne     code_CD8B
        lda     joy1_press                     ; set B-button-pressed flag
        ora     #BTN_B                    ; (triggers weapon_fire in state handler)
        sta     joy1_press
code_CD8B:  ldy     player_state
        lda     player_state_ptr_lo,y                 ; grab player state
        sta     L0000                   ; index into state ptr tables
        lda     player_state_ptr_hi,y                 ; jump to address
        sta     $01
        jmp     (L0000)

player_state_ptr_lo:  .byte   $36,$07,$FD,$EB,$BA,$13,$AB,$31 ; $02 player_slide
        .byte   $58,$29,$91,$BE,$D3,$E1,$79,$CC ; $0A player_top_spin
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
        lda     boss_active                     ; if boss fight active: freeze animation
        bmi     code_CDE6
        ldy     #$00                    ; apply $99 + vertical collision
        jsr     move_vertical_gravity
        bcc     code_CDE5               ; not landed → done
        lda     #$01                    ; if landed and OAM != $01 (idle):
        cmp     ent_anim_id                   ; reset to idle animation
        beq     code_CDE5
        jsr     reset_sprite_anim
        lda     #$00                    ; clear shooting flag
        sta     walk_flag
code_CDE5:  rts

code_CDE6:  lda     #$00                ; freeze anim counter (boss cutscene)
        sta     ent_anim_frame
        rts

; --- platform_push: apply external velocity from moving platform ---
; $36 = push direction (bit 0: 1=right, 0=left)
; $37/$38 = push speed (sub/whole), applied as player X speed temporarily.

code_CDEC:  lda     ent_flags               ; save player sprite flags
        pha
        lda     ent_xvel_sub                   ; save player X speed (sub + whole)
        pha
        lda     ent_xvel
        pha
        lda     $37                     ; apply platform push speed
        sta     ent_xvel_sub                   ; as player X speed temporarily
        lda     $38
        sta     ent_xvel
        ldy     player_state                     ; Y = collision offset:
        cpy     #PSTATE_SLIDE                    ; if sliding (state $02), use slide hitbox
        beq     code_CE0A               ; otherwise Y = 0 (standing hitbox)
        ldy     #$00
code_CE0A:  lda     $36                 ; push direction: bit 0 = right
        and     #$01
        beq     code_CE16
        jsr     move_right_collide      ; push right with collision
        jmp     code_CE1A

code_CE16:  iny                         ; push left with collision
        jsr     move_left_collide

; --- restore player state after platform push ---
code_CE1A:  pla                         ; restore original X speed
        sta     ent_xvel
        pla
        sta     ent_xvel_sub
        pla                             ; restore original facing (bit 6 only)
        and     #$40                    ; from saved sprite flags
        sta     L0000
        lda     ent_flags                   ; clear bit 6, then OR with saved
        and     #$BF
        ora     L0000
        sta     ent_flags
        lda     #$00                    ; clear platform push flag
        sta     $36
code_CE35:  rts

; ===========================================================================
; player state $00: on ground — walk, jump, slide, shoot, Rush interaction
; ===========================================================================
player_on_ground:

        jsr     player_airborne         ; run $99/collision first
        bcc     code_CE35               ; carry clear = still airborne → RTS

; --- landed: check for Rush Coil interaction ---
        ldy     $5D                     ; $5D = last collision entity slot
        cpy     #$01                    ; slot 1 = Rush entity?
        bne     LCE88                   ; not Rush → skip
        lda     ent_anim_id                   ; if player OAM == $11 (damage flash):
        cmp     #$11                    ; reset to idle animation
        bne     LCE4D
        lda     #$01
        jsr     reset_sprite_anim
LCE4D:  ldy     $05C1                   ; slot 1 OAM ID >= $D7?
        cpy     #$D7                    ; (Rush Coil/Marine/Jet active)
        bcc     LCE88                   ; no → normal ground

; --- Rush handler dispatch via pointer table ---
        lda     LCCEF,y                 ; build indirect jump pointer
        sta     L0000                   ; from $CCEF/$CCF2 tables
        lda     LCCF2,y                 ; indexed by Rush OAM ID
        sta     $01
        jmp     (L0000)                 ; dispatch Rush handler via pointer table

; --- Rush Coil bounce: player lands on Rush Coil and bounces upward ---
; Reached via indirect jump from Rush handler dispatch above.

        lda     $05A1                   ; if Rush already bounced: skip
        bne     LCE88
        inc     jump_counter                     ; set forced-fall flag (no variable jump)
        lda     #$EE                    ; Y speed = $06.EE (slide jump velocity)
        sta     ent_yvel_sub                   ; strong upward bounce
        lda     #$06
        sta     ent_yvel
        inc     $05A1                   ; mark bounce as triggered (one-shot)
        lda     #$3C                    ; slot 1 timer = 60 frames (Rush lingers)
        sta     $0501
        lda     $0581                   ; clear slot 1 flag bit 0
        and     #$FE
        sta     $0581
        jsr     decrease_ammo_tick                   ; consume Rush Coil ammo
        jmp     player_airborne         ; → airborne state (bouncing up)

LCE88:  lda     ent_anim_id
        cmp     #$10                    ; player sliding?
        bne     LCE9E
        lda     joy1_press
        and     #BTN_A                    ; if so,
        beq     code_CE35               ; newpressing A
        lda     joy1_held                     ; and not holding down?
        and     #BTN_DOWN                    ; return, else go on
        beq     code_CE35               ; return (no slide)
        jmp     slide_initiate          ; initiate slide

LCE9E:  lda     joy1_press
        and     #BTN_A                    ; newpressing A
        beq     code_CECD               ; and not holding down
        lda     joy1_held                     ; means jumping
        and     #BTN_DOWN                    ; holding down → slide instead
        beq     LCEAD
        jmp     slide_initiate          ; initiate slide

LCEAD:  lda     $17                     ; check controller 2
        and     #$01                    ; holding Right for
        bne     LCEC0                   ; super jump
        lda     #$E5
        sta     ent_yvel_sub,x                 ; jump speed
        lda     #$04                    ; $04E5
        sta     ent_yvel,x
        jmp     player_airborne

LCEC0:  lda     #$00
        sta     ent_yvel_sub                   ; super jump speed
        lda     #$08                    ; $0800
        sta     ent_yvel
        jmp     player_airborne

; --- animation state dispatch: determine walk/idle animation ---
; OAM IDs: $04/$05 = shoot-walk, $0D/$0E/$0F = special walk anims

code_CECD:  lda     ent_anim_id               ; check current player OAM
        cmp     #$04                    ; $04/$05 = shoot-walk anims
        beq     code_CEEB
        cmp     #$05
        beq     code_CEEB
        cmp     #$0D                    ; $0D/$0E/$0F = turning/special anims
        beq     code_CEE4               ; → check if anim complete
        cmp     #$0E
        beq     code_CEE4
        cmp     #$0F
        bne     code_CF0A               ; other → normal walk handler
code_CEE4:  lda     ent_anim_state               ; if special anim still playing:
        bne     code_CF0A               ; → normal walk handler
        beq     code_CF59               ; anim done → slide/crouch check

; --- shoot-walk: continue walking in facing direction ---
code_CEEB:  lda     joy1_held                 ; D-pad matches facing direction?
        and     player_facing
        beq     code_CEF9               ; no → turning animation
        pha
        jsr     code_CF59               ; slide/crouch check
        pla
        jmp     code_CF3D               ; move L/R based on direction

; --- turning: set turn animation, then move ---

code_CEF9:  lda     #$0D                ; OAM $0D = turning animation
        jsr     reset_sprite_anim
        lda     walk_flag                     ; if shooting: update shoot-walk anim
        beq     code_CF05
        jsr     code_D370
code_CF05:  lda     player_facing                 ; move in facing direction
        jmp     code_CF3D

; --- normal walk handler: D-pad L/R → walk, else idle ---

code_CF0A:  lda     joy1_held                 ; D-pad L/R held?
        and     #$03
        beq     code_CF4B               ; no → idle
        sta     player_facing                     ; update facing direction
        jsr     code_CF59               ; slide/crouch check
        lda     ent_anim_id                   ; if OAM is $0D/$0E/$0F:
        cmp     #$0D                    ; use shoot-walk animation
        beq     code_CF24
        cmp     #$0E
        beq     code_CF24
        cmp     #$0F
        bne     code_CEF9               ; else → turning animation

; --- shoot-walk animation: OAM $04 + check weapon-specific override ---
code_CF24:  lda     #$04                ; OAM $04 = shoot-walk
        jsr     reset_sprite_anim
        lda     player_facing                     ; A = facing direction
        ldy     walk_flag                     ; if not shooting: just move
        beq     code_CF3D
        jsr     code_D370               ; update shoot animation
        ldy     current_weapon                     ; if weapon == $04 (Magnet Missile):
        cmp     #$04                    ; use special OAM $AA
        bne     code_CF3D
        ldy     #$AA
        sta     ent_anim_id

; --- move_horizontal: move player L/R based on direction in A ---
; A bit 0: 1=right, 0=left. Entry point used by walking, entity_ride, etc.
code_CF3D:  and     #$01                ; bit 0: right?
        beq     code_CF46
        ldy     #$00                    ; move right with collision
        jmp     move_right_collide

code_CF46:  ldy     #$01                ; move left with collision
        jmp     move_left_collide

; --- idle: no D-pad input → check if need to reset animation ---

code_CF4B:  lda     walk_flag                 ; if shooting: skip idle anim reset
        bne     code_CF59
        lda     #$01                    ; if already OAM $01 (idle): skip
        cmp     ent_anim_id
        beq     code_CF59
        jsr     reset_sprite_anim       ; set OAM to $01 (idle standing)

; --- common ground exit: check slide/crouch + B button ---
code_CF59:  jsr     code_D355           ; slide/crouch/ladder input check
        lda     joy1_press                     ; B button pressed → fire weapon
        and     #BTN_B
        beq     code_CF65
        jsr     weapon_fire
code_CF65:  rts

; --- DEAD CODE: unreachable block at $1ECF66 ---
; No references to this address exist in any bank — unreachable dead code.
; Contains a Rush Marine ground-mode handler (slot 1 management, walk, climb,
; jump, shoot), superseded by the current player_rush_marine implementation.

        jsr     decrease_ammo_tick                   ; decrease Rush ammo
        lda     #$82                    ; set slot 1 (Rush) main routine = $82
        cmp     $0321                   ; (skip if already $82)
        beq     code_CF7B
        sta     $0321
        lda     $05E1                   ; reset slot 1 animation
        and     #$7F
        sta     $05E1
code_CF7B:  lda     joy1_press                 ; A button → jump
        and     #BTN_A
        beq     code_CF84
        jmp     LCEAD

code_CF84:  lda     joy1_held                 ; D-pad L/R → walk
        and     #$03
        beq     code_CF9B               ; no horizontal input
        sta     player_facing                     ; update facing direction
        jsr     code_CF3D               ; move L/R with collision
        lda     #$01                    ; reset to walk animation
        jsr     reset_sprite_anim
        lda     walk_flag                     ; if shooting: update shoot-walk anim
        beq     code_CF9B
        jsr     code_D370
code_CF9B:  lda     joy1_held                 ; D-pad U/D → climb vertically
        and     #$0C
        beq     code_CFD3               ; no vertical input → skip
        pha                             ; save D-pad state
        lda     #$80                    ; climb speed = $01.80
        sta     ent_yvel_sub,x
        lda     #$01
        sta     ent_yvel,x
        pla                             ; bit 3 = up?
        and     #$08
        beq     code_CFC2               ; no → move down
        ldy     #$01                    ; move up with collision
        jsr     move_up_collide
        lda     #$0C                    ; clamp player Y >= $0C (top boundary)
        cmp     ent_y_px
        bcc     code_CFC7
        sta     ent_y_px
        bcs     code_CFC7               ; always branches
code_CFC2:  ldy     #$07                ; move down with collision
        jsr     move_down_collide
code_CFC7:  lda     ent_y_px               ; slot 1 Y = player Y + $0E
        clc                             ; (Rush sits 14px below player)
        adc     #$0E
        sta     $03C1
        jsr     reset_gravity           ; clear $99 after manual climb
code_CFD3:  jsr     code_D355           ; process slide/crouch input
        lda     joy1_press                     ; B button → fire weapon
        and     #BTN_B
        beq     code_CFDF
        jsr     weapon_fire
code_CFDF:  rts

; --- END DEAD CODE ---

; --- enter Rush Marine mode: copy Rush position to player, change state ---

        lda     $0361                   ; player X = Rush X (slot 1)
        sta     ent_x_px
        lda     $0381
        sta     ent_x_scr
        lda     $03C1                   ; player Y = Rush Y
        sta     ent_y_px
        lda     #$00                    ; deactivate slot 1 (Rush entity)
        sta     $0301
        lda     #$05                    ; $EB = CHR page 5 (Rush Marine tiles)
        sta     $EB
        jsr     update_CHR_banks
        lda     #PSTATE_RUSH_MARINE                    ; $30 = $08 (player state = Rush Marine)
        sta     player_state
        lda     #$DA
        jmp     reset_sprite_anim

; player state $01: jumping/falling, variable jump

player_airborne:  lda     ent_yvel         ; if Y speed negative (rising): skip
        bmi     code_D019               ; variable jump check
        lda     jump_counter                     ; if $3A nonzero (forced fall, e.g. Rush bounce):
        bne     code_D01D               ; skip variable jump
        lda     joy1_held                     ; if A button held: keep rising
        and     #BTN_A
        bne     code_D019
        jsr     reset_gravity           ; A released → clamp Y speed to 0 (variable jump)
code_D019:  lda     #$00                ; clear forced-fall flag
        sta     jump_counter

; --- horizontal wall check + vertical movement with $99 ---
code_D01D:  ldy     #$06                ; check horizontal tile collision
        jsr     check_tile_horiz        ; (Y=6: check at foot height)
        lda     $10                     ; save collision result in $0F
        sta     $0F
        ldy     #$00                    ; apply $99 and move vertically
        jsr     move_vertical_gravity   ; carry = landed on ground
        php                             ; save carry (landing status)

; --- check for landing: spawn dust cloud on first ground contact ---
        lda     $0F                     ; $0F == $80 → landed on solid ground
        cmp     #$80
        bne     code_D07E               ; not landed → clear dust state
        lda     $B9                     ; $B9 = dust cooldown timer
        bne     code_D084               ; if active: skip dust spawn
        lda     #$03                    ; set dust cooldown = 3 frames
        sta     $B9
        lda     $BA                     ; $BA = dust spawned flag (one-shot)
        bne     code_D084               ; already spawned → skip
        inc     $BA                     ; mark dust as spawned
        lda     #$1F                    ; play landing sound effect
        jsr     submit_sound_ID

; spawn landing dust cloud in slot 4 (visual effect slot)
        lda     #$80                    ; activate slot 4
        sta     $0304
        lda     ent_flags                   ; copy player sprite flags to dust
        sta     $0584                   ; (inherits facing direction)
        ldx     #$04                    ; slot 4: OAM $68 = landing dust animation
        lda     #$68
        jsr     reset_sprite_anim
        lda     ent_x_px                   ; copy player X position to dust
        sta     $0364
        lda     ent_x_scr
        sta     $0384
        lda     ent_y_px                   ; dust Y = player Y snapped to tile top - 8
        and     #$F0                    ; (align to 16px grid, then 8px up)
        sec
        sbc     #$08
        sta     $03C4
        lda     ent_y_scr                   ; copy player Y screen to dust
        sta     $03E4
        ldx     #$00                    ; restore X = 0 (player slot)
        stx     $0484                   ; dust damage flags = 0 (no collision)
        stx     $0324                   ; dust main routine = 0
        beq     code_D084               ; always branches (X=0)

; --- not landed: reset dust state ---
code_D07E:  lda     #$00                ; clear dust spawn flag and cooldown
        sta     $BA
        sta     $B9
code_D084:  plp                         ; restore carry (landing status)
        jsr     code_D47E               ; check if player should enter ladder
        bcc     code_D0A7               ; carry clear = no ladder → continue airborne

; --- player landed on ground → transition to on_ground ---
        lda     player_state                     ; if already state $00: skip transition
        beq     code_D0A6
        lda     #$13                    ; play landing sound
        jsr     submit_sound_ID
        lda     #$00                    ; $30 = 0 (state = on_ground)
        sta     player_state
        lda     #$0D                    ; OAM $0D = landing animation
        jsr     reset_sprite_anim
        inc     ent_anim_state                   ; advance animation frame
        lda     walk_flag                     ; if shooting: set shoot OAM variant
        beq     code_D0A6
        jsr     code_D370
code_D0A6:  rts

; --- still airborne: dispatch by current state ---

code_D0A7:  lda     player_state                 ; if on ladder (state $03): just return
        cmp     #PSTATE_LADDER
        beq     code_D0D8
        cmp     #PSTATE_AIRBORNE                    ; if already airborne: walk+shoot
        beq     code_D0C1

; --- first airborne frame: set jump animation ---
        lda     #$07                    ; OAM $07 = jump/fall animation
        jsr     reset_sprite_anim
        lda     #PSTATE_AIRBORNE                    ; $30 = 1 (state = airborne)
        sta     player_state
        lda     walk_flag                     ; if shooting: set shoot OAM variant
        beq     code_D0C1
        jsr     code_D370

; --- horizontal_walk_and_shoot: D-pad L/R + B button + shoot timer ---
; Common subroutine for on_ground, airborne, and entity_ride states.
code_D0C1:  lda     joy1_held                 ; D-pad L/R held?
        and     #$03
        beq     code_D0CC               ; no → skip walk
        sta     player_facing                     ; update facing direction
        jsr     code_CF3D               ; move L/R with collision
code_D0CC:  jsr     code_D355           ; shoot timer tick
        lda     joy1_press                     ; B button pressed → fire weapon
        and     #BTN_B
        beq     code_D0D8
        jsr     weapon_fire
code_D0D8:  clc                         ; carry clear = still airborne (for caller)
        rts

; player intends to fire the active weapon
; check if enough ammo and if free slot available

weapon_fire:  ldy     current_weapon
        lda     player_hp,y                   ; ammo run out?
        and     #$1F                    ; return
        beq     LD134
        lda     weapon_max_shots,y      ; Y = starting loop index
        tay                             ; to check for weapons
LD0E7:  lda     ent_status,y
        bpl     LD0F1                   ; check free slot for weapons
        dey                             ; by ent_status,x active table
        bne     LD0E7
        beq     LD134                   ; no free slots, return
LD0F1:  ldy     current_weapon                     ; decrease ammo
        jsr     decrease_ammo
        lda     weapon_init_ptr_lo,y
        sta     L0000                   ; jump to weapon init
        lda     weapon_init_ptr_hi,y    ; routine to spawn
        sta     $01                     ; the shot sprite
        jmp     (L0000)

; common weapon init routine: spawns the shot

init_weapon:  lda     walk_flag
        bne     LD10A
        jsr     code_D370
LD10A:  lda     ent_anim_id
        cmp     #$05                    ; if Mega Man OAM ID
        beq     LD121                   ; is $05, $0E, or $0F
        cmp     #$0E
        beq     LD121
        cmp     #$0F
        beq     LD121
        lda     #$00                    ; any other ID
        sta     ent_anim_state                   ; reset Mega Man animation
        sta     ent_anim_frame                   ; to $00 frame & timer
LD121:  lda     #$10
        sta     walk_flag
        ldy     current_weapon                     ; use max shot value as
        lda     weapon_max_shots,y      ; max slot to start loop
        tay
LD12B:  lda     ent_status,y
        bpl     LD135                   ; check free slot for weapons
        dey                             ; by ent_status,x active table
        bne     LD12B
        clc                             ; no free slots, return carry off
LD134:  rts

LD135:  ldx     current_weapon
        lda     weapon_fire_sound,x     ; play weapon sound effect
        jsr     submit_sound_ID
        ldx     #$00                    ; X=0 (player slot)
        lda     #$80                    ; activate weapon slot
        sta     ent_status,y
        lda     player_facing                     ; convert facing ($31: 1=R, 2=L)
        ror     a                       ; to sprite flags bit 6 (H-flip)
        ror     a                       ; $01→ROR³→$00→AND=$00 (right)
        ror     a                       ; $02→ROR³→$40→AND=$40 (left=flip)
        and     #$40
        ora     #$90                    ; bits: $80=drawn, $10=child spawn
        sta     ent_flags,y
        lda     #$00
        sta     ent_xvel_sub,y                 ; default weapon X speed:
        lda     #$04                    ; 4 pixels per frame
        sta     ent_xvel,y
        lda     player_facing                     ; set L/R facing of shot
        sta     ent_facing,y                 ; = player facing
        and     #$02
        tax
        lda     ent_x_px                   ; fetch X pixel & screen offsets
        clc                             ; based on player facing
        adc     weapon_x_offset,x       ; weapon X = player X + offset
        sta     ent_x_px,y
        lda     ent_x_scr
        adc     LD31C,x
        sta     ent_x_scr,y
        lda     ent_y_px
        sta     ent_y_px,y                 ; weapon Y = player Y
        lda     ent_y_scr
        sta     ent_y_scr,y
        lda     #$00
        sta     ent_hitbox,y                 ; clear animation, hitbox,
        sta     ent_anim_state,y                 ; wildcard & X subpixel
        sta     ent_anim_frame,y
        sta     ent_x_sub,y
        sta     ent_timer,y
        ldx     current_weapon
        lda     weapon_OAM_ID,x         ; set OAM & main
        sta     ent_anim_id,y                 ; ID's for weapon
        lda     weapon_main_ID,x
        sta     ent_routine,y
        inc     $3B                     ; $3B++ (shot parity counter)
        ldx     #$00                    ; restore X=0 (player slot)
        sec                             ; carry set = weapon spawned OK
        rts

; ===========================================================================
; init_rush — spawn Rush entity (Coil/Marine/Jet) in slot 1
; ===========================================================================
; Checks if Rush can be summoned (player OAM < $D7, drawn, slot 1 free).
; If not, falls through to init_weapon for normal weapon fire.
init_rush:

        lda     ent_anim_id                   ; if player OAM ID >= $D7 (Rush already active):
        cmp     #$D7                    ; can't spawn another Rush
        bcs     code_D1B7               ; → try normal weapon instead
        lda     ent_flags                   ; if player not drawn (bit 7 clear):
        bpl     code_D1FA               ; abort (RTS)
        lda     $0301                   ; if slot 1 already active (bit 7):
        bpl     code_D1BA               ; slot free → spawn Rush
code_D1B7:  jmp     init_weapon         ; fallback: fire normal weapon

; --- spawn Rush entity in slot 1 ---

code_D1BA:  ldy     #$01                ; spawn entity $13 (Rush) in slot 1
        lda     #$13
        jsr     init_child_entity
        lda     #$00                    ; Rush Y position = 0 (will land via $99)
        sta     $03C1
        sta     $03E1                   ; Rush Y screen = 0
        lda     #$11                    ; $0481 = $11 (Rush damage/collision flags)
        sta     $0481
        lda     player_facing                     ; copy player facing to Rush ($04A1)
        sta     $04A1
        and     #$02                    ; X = 0 if facing right, 2 if facing left
        tax                             ; (bit 1 of $31: 1=R has bit1=0, 2=L has bit1=1)
        lda     ent_x_px                   ; Rush X = player X + directional offset
        clc                             ; offset from $D31F table (2 entries: R, L)
        adc     LD31F,x
        sta     $0361
        lda     ent_x_scr                   ; Rush X screen = player X screen + carry
        adc     LD320,x
        sta     $0381
        lda     #$80                    ; set Rush main routine = $80 (active)
        sta     ent_routine,y
        lda     #$AB                    ; Rush Y speed = $FF.AB (slight downward drift)
        sta     $0441
        lda     #$FF
        sta     $0461
        ldx     #$00                    ; restore X = 0 (player slot)
code_D1FA:  rts

; --- init_needle_cannon: fire needle shot with vertical offset ---
; Alternates Y offset based on $3B bit 0 (shot parity).
init_needle_cannon:

        jsr     init_weapon             ; spawn basic weapon projectile
        bcc     code_D211               ; carry clear = spawn failed
        lda     $3B                     ; $3B bit 0 = shot parity (alternates 0/1)
        and     #$01
        tax
        lda     ent_y_px,y                 ; offset shot Y by table value at $D33B
        clc                             ; (two entries: even/odd shot offset)
        adc     LD33B,x
        sta     ent_y_px,y
        ldx     #$00                    ; restore X = 0 (player slot)
code_D211:  rts
init_gemini_laser:

        jsr     init_weapon
        bcc     LD24C
        iny
        jsr     LD135                   ; spawn 2 more shots
        iny                             ; uses all 3 slots
        jsr     LD135
        lda     #$B4
        sta     $0501                   ; store $B4
        sta     $0502                   ; wildcard 1
        sta     $0503                   ; all 3 slots
        lda     #$00
        sta     $0441
        sta     $0442                   ; clear Y speeds
        sta     $0443                   ; all 3 slots
        sta     $0461
        sta     $0462
        sta     $0463
        lda     $0361
        and     #$FC                    ; take first slot's
        sta     $0361                   ; X, chop off low two bits
        sta     $0362                   ; store into all three X slots
        sta     $0363
LD24C:  rts

; ===========================================================================
; init_hard_knuckle — weapon $03: Hard Knuckle projectile
; ===========================================================================
; Spawns in slot 1. Player state selects OAM from $D293 table:
;   $30=0(ground)→$AA, $30=1(air)→$AB, $30=2(slide)→$00(skip), $30=3(ladder)→$AD
; Hard Knuckle uniquely uses slot 1 (like Rush), overrides player OAM,
; and sets player state to $0B (hard_knuckle_ride) for steering.
init_hard_knuckle:

        lda     $0301                   ; slot 1 already active?
        bmi     code_D292               ; if so, can't fire
        ldy     player_state                     ; player state >= $04 (reappear etc)?
        cpy     #PSTATE_REAPPEAR
        bcs     code_D292               ; can't fire in those states
        lda     LD293,y                 ; OAM ID for this state ($00 = skip)
        beq     code_D292               ; state $02(slide): no Hard Knuckle
        jsr     init_weapon             ; spawn weapon entity (sets Y=slot)
        ldy     player_state                     ; reload OAM for player state
        lda     LD293,y                 ; and set player animation to it
        jsr     reset_sprite_anim       ; (punch/throw pose)
        cpy     #$03                    ; on ladder ($30=3)?
        bne     code_D271
        lda     #$AE                    ; override slot 1 OAM to $AE
        sta     $05C1                   ; (ladder throw variant)
code_D271:  lda     #$00                ; zero X/Y speeds for slot 1
        sta     $0401                   ; (Hard Knuckle starts stationary,
        sta     $0421                   ; player steers it with D-pad)
        sta     $0461
        lda     #$80                    ; Y speed sub = $80
        sta     $0441                   ; (initial Y drift: $00.80 = 0.5 px/f)
        inc     $05E1                   ; bump anim frame (force sprite update)
        lda     player_state                     ; save player state to ent_timer (slot 0)
        sta     ent_timer                   ; (restore when Hard Knuckle ends)
        lda     #$10                    ; slot 1 AI routine = $10
        sta     ent_var1                   ; (Hard Knuckle movement handler)
        lda     #PSTATE_WEAPON_RECOIL                    ; set player state $0B = hard_knuckle_ride
        sta     player_state                     ; (D-pad steers the projectile)
code_D292:  .byte   $60

; Hard Knuckle OAM IDs per player state ($30=0..3)
; $AA=ground, $AB=air, $00=skip(slide), $AD=ladder
LD293:  .byte   $AA,$AB,$00,$AD
LD297:  .byte   $01,$07,$00,$0A,$FC,$FF
        .byte   $04
        brk
init_top_spin:
        lda     player_state
        cmp     #PSTATE_AIRBORNE                    ; if player not in air,
        bne     LD2D2                   ; return
        lda     #$A3
        cmp     ent_anim_id                   ; if Mega Man already spinning,
        beq     LD2D2                   ; return
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
        bcc     LD2D2                   ; no free slot → return
        lda     #$44                    ; Y speed sub = $44
        sta     ent_yvel_sub,y
        lda     #$03                    ; Y speed whole = $03 (drop fast)
        sta     ent_yvel,y
        lda     #$00                    ; X speed sub = $00
        sta     ent_xvel_sub,y
        lda     #$01                    ; X speed whole = $01 (1 px/f forward)
        sta     ent_xvel,y
        lda     #$13                    ; AI routine = $13 (snake crawl)
        sta     ent_timer,y
LD2D2:  rts

; ===========================================================================
; init_shadow_blade — weapon $0A: Shadow Blade
; ===========================================================================
; D-pad direction stored in ent_facing (bits: up=$08, down=$02, right=$01).
; If no D-pad held, ent_facing=0 → blade fires horizontally (facing direction).
; X speed $04.00 (4 px/f). Copies player position to blade (spawns at player).
init_shadow_blade:

        jsr     init_weapon             ; spawn weapon; carry set = success
        bcc     code_D302               ; no free slot → return
        lda     joy1_held                     ; read D-pad held bits
        and     #$0B                    ; mask Up($08)+Down($02)+Right($01)
        beq     code_D2E1               ; no direction → skip (fire horizontal)
        sta     ent_facing,y                 ; store throw direction for AI
code_D2E1:  lda     #$00                ; Y speed sub = $00
        sta     ent_yvel_sub,y
        lda     #$04                    ; X speed whole = $04 (4 px/f)
        sta     ent_yvel,y
        lda     #$14                    ; AI routine = $14 (shadow blade)
        sta     ent_timer,y
        lda     ent_x_px,x                 ; copy player X position to blade
        sta     ent_x_px,y
        lda     ent_x_scr,x                 ; copy player X screen to blade
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; copy player Y position to blade
        sta     ent_y_px,y
code_D302:  .byte   $60

; routine pointers for weapon init upon firing
weapon_init_ptr_lo:  .byte   $03,$12,$FB,$4D,$03,$9F,$B4,$A6 ; Mega Buster
        .byte   $03,$A6,$D3,$A6         ; Spark Shock
weapon_init_ptr_hi:  .byte   $D1,$D2,$D1,$D2,$D1,$D2,$D2,$D1 ; Mega Buster
        .byte   $D1,$D1,$D2,$D1         ; Spark Shock

; on weapon fire, weapon is placed at an offset from Mega Man
; based on left vs. right facing
; right X pixel, right X screen, left X pixel, left X screen
weapon_x_offset:  .byte   $0F
LD31C:  .byte   $00,$F0,$FF
LD31F:  .byte   $17
LD320:  .byte   $00,$E8,$FF
weapon_OAM_ID:  .byte   $18,$9F,$A2,$AC,$97,$18,$A5,$18 ; Mega Buster
        .byte   $9C,$18,$9E,$18         ; Spark Shock
weapon_main_ID:  .byte   $01,$84,$01,$85,$83,$01,$86,$01 ; Mega Buster
        .byte   $87,$01,$88,$01         ; Spark Shock
LD33B:  .byte   $FE,$02

; maximum # of shots on screen at once
weapon_max_shots:  .byte   $03,$01,$03,$01,$02,$00,$03,$03 ; Mega Buster
        .byte   $02,$03,$01,$03         ; Spark Shock

; sound ID played on weapon fire
weapon_fire_sound:  .byte   $15,$2B,$15,$15,$2A,$2C,$15,$15 ; Mega Buster
        .byte   $2D,$15                 ; Spark Shock
        .byte   $2E                     ; Shadow Blade
        .byte   $15                     ; Rush Jet

; --- shoot_timer_tick: decrement shoot timer, end shoot animation when done ---
; $32 = shoot animation timer. When it reaches 0, revert OAM to non-shoot variant.
; OAM IDs for shooting are base+1 (or +2 for Shadow Blade $0A).
code_D355:  lda     walk_flag                 ; if not shooting: return
        beq     code_D36F
        dec     walk_flag                     ; decrement timer
        bne     code_D36F               ; not done → return
        jsr     code_D37F               ; revert OAM from shoot variant
        ldy     ent_anim_id                   ; if OAM == $04 (shoot-walk):
        cpy     #$04                    ; don't reset animation
        beq     code_D36F
        lda     #$00                    ; reset animation state
        sta     ent_anim_frame
        sta     ent_anim_state
code_D36F:  rts

; --- set_shoot_oam: advance OAM to shooting variant ---
; OAM+1 for most weapons, OAM+2 for Shadow Blade ($0A).

code_D370:  pha                         ; preserve A
        inc     ent_anim_id                   ; OAM += 1 (shoot variant)
        lda     current_weapon                     ; if weapon == $0A (Shadow Blade):
        cmp     #WPN_SHADOW                    ; OAM += 2 (two-handed throw anim)
        bne     code_D37D
        inc     ent_anim_id
code_D37D:  pla                         ; restore A
        rts

; --- revert_shoot_oam: reverse OAM from shooting variant ---

code_D37F:  pha
        dec     ent_anim_id                   ; OAM -= 1
        lda     current_weapon                     ; if Shadow Blade: OAM -= 2
        cmp     #WPN_SHADOW
        bne     code_D38C
        dec     ent_anim_id
code_D38C:  pla
        rts

; slide_initiate — check for wall ahead, then start slide
; Called from player_on_ground when Down+A pressed.
; Sets player state to $02 (player_slide), spawns dust cloud at slot 4.

slide_initiate:  lda     joy1_held            ; read D-pad state
        and     #$03                    ; left/right bits
        beq     LD396                   ; no direction → keep current
        sta     player_facing                     ; update facing from D-pad
LD396:  ldy     #$04                    ; Y=4 (check right) or Y=5 (check left)
        lda     player_facing                     ; based on facing direction
        and     #$01                    ; bit 0 = right
        bne     LD39F
        iny                             ; facing left → Y=5
LD39F:  jsr     check_tile_horiz        ; check for wall ahead at slide height
        lda     $10                     ; collision result
        and     #$10                    ; bit 4 = solid wall
        beq     LD3A9                   ; no wall → can slide
        rts                             ; wall ahead → abort

; slide confirmed — set up player state and dust cloud

LD3A9:  lda     #PSTATE_SLIDE                    ; state $02 = player_slide
        sta     player_state
        lda     #$14                    ; slide timer = 20 frames
        sta     $33
        lda     #$10                    ; OAM anim $10 = slide sprite
        jsr     reset_sprite_anim
        lda     ent_y_px                   ; player Y += 2 (crouch posture)
        clc
        adc     #$02
        sta     ent_y_px
        lda     #$80                    ; X speed = $02.80 = 2.5 px/frame
        sta     ent_xvel_sub                   ; (sub-pixel)
        lda     #$02                    ; (whole)
        sta     ent_xvel
        lda     #$80                    ; slot 4 active flag = $80
        sta     $0304                   ; (mark dust cloud entity active)
        lda     ent_flags                   ; copy player facing to slot 4
        sta     $0584
        ldx     #$04                    ; slot 4: OAM anim $17 = slide dust
        lda     #$17
        jsr     reset_sprite_anim
        lda     ent_x_px                   ; copy player position to slot 4
        sta     $0364                   ; X pixel
        lda     ent_x_scr                   ; X screen
        sta     $0384
        lda     ent_y_px                   ; Y pixel
        sta     $03C4
        lda     ent_y_scr                   ; Y screen
        sta     $03E4
        ldx     #$00                    ; restore X to player slot
        stx     $0484                   ; slot 4 damage flags = 0 (harmless)
        stx     $0324                   ; slot 4 main routine = 0 (no AI)
        beq     code_D412               ; always → skip to slide movement

; player state $02: sliding on ground
; Speed: $02.80 = 2.5 px/frame. Timer in $33 (starts at $14 = 20 frames).
; First 8 frames: uncancellable. Frames 9-20: cancellable with A (slide jump).
; Exits: timer expires, direction change, wall hit, ledge, or A button.
player_slide:
        ldy     #$02                    ; apply $99 (Y=2 = slide hitbox)
        jsr     move_vertical_gravity
        bcc     code_D455               ; C=0 → no ground, fell off ledge
        lda     joy1_held                     ; D-pad left/right
        and     #$03
        beq     code_D412               ; no direction → keep sliding
        cmp     player_facing                     ; same as current facing?
        beq     code_D412               ; yes → keep sliding
        sta     player_facing                     ; changed direction → end slide
        bne     code_D43E
code_D412:  lda     player_facing                 ; facing direction: bit 0 = right
        and     #$01
        beq     code_D420
        ldy     #$02                    ; slide right
        jsr     move_right_collide
        jmp     code_D425

code_D420:  ldy     #$03                ; slide left
        jsr     move_left_collide
code_D425:  lda     $10                 ; hit a wall?
        and     #$10
        bne     code_D43E               ; yes → end slide
        lda     $33                     ; timer expired?
        beq     code_D43E               ; yes → end slide
        dec     $33                     ; decrement timer
        lda     $33                     ; timer >= $0C (first 8 frames)?
        cmp     #$0C
        bcs     code_D43D               ; yes → uncancellable, keep sliding
        lda     joy1_press                     ; A button pressed? (cancellable phase)
        and     #BTN_A
        bne     code_D43E               ; yes → cancel into slide jump
code_D43D:  rts

; slide_end — transition out of slide state
; Checks if grounded: on ground → standing. Airborne + A → slide jump. Else → fall.

code_D43E:  ldy     #$01                ; check ground at standing height
        jsr     check_tile_horiz
        lda     $10                     ; solid ground under feet?
        and     #$10
        bne     code_D470               ; on ground → return to standing
        sta     $33                     ; clear timer and state
        sta     player_state
        lda     joy1_press                     ; A button held?
        and     #BTN_A
        bne     code_D471               ; yes → slide jump
        beq     code_D45D               ; no → fall from slide
code_D455:  lda     #$00                ; clear timer
        sta     $33
        lda     #$01                    ; OAM anim $01 = airborne
        bne     code_D45F
code_D45D:  lda     #$04                ; OAM anim $04 = jump/fall
code_D45F:  jsr     reset_sprite_anim
        lda     #$4C                    ; walk speed X = $01.4C (decelerate
        sta     ent_xvel_sub                   ; from slide 2.5 to walk 1.297)
        lda     #$01
        sta     ent_xvel
        lda     #$00                    ; state $00 = on_ground (will fall)
        sta     player_state
code_D470:  rts

code_D471:  lda     #$4C                ; walk speed X = $01.4C
        sta     ent_xvel_sub
        lda     #$01
        sta     ent_xvel
        jmp     LCEAD                   ; → jump (uses slide_jump vel $06.EE)

; --- check ladder/down-entry from ground or slide ---

code_D47E:  lda     ent_y_scr               ; if Y screen != 0 (off main screen),
        bne     code_D4C7               ; skip ladder check
        lda     joy1_held                     ; pressing Up?
        and     #BTN_UP                    ; (Up = bit 3)
        beq     code_D4C8               ; no → check Down instead
        php                             ; save flags (carry = came from slide?)
check_ladder_entry:  ldy     #$04
        jsr     check_tile_collision
        lda     tile_at_feet_hi                     ; check both foot tiles
        cmp     #TILE_LADDER                    ; for ladder ($20) or
        beq     code_D4A3               ; ladder top ($40)
        cmp     #TILE_LADDER_TOP
        beq     code_D4A3
        lda     tile_at_feet_lo                     ; same check on other foot
        cmp     #TILE_LADDER
        beq     code_D4A3
        cmp     #TILE_LADDER_TOP
        bne     code_D4C6

; --- enter ladder (from Up press) ---
code_D4A3:  plp                         ; restore flags
        lda     $12                     ; center player X on ladder tile
        and     #$F0                    ; snap to 16-pixel grid
        ora     #$08                    ; then offset +8 (center of tile)
        sta     ent_x_px
        lda     #$0A                    ; OAM $0A = ladder climb anim

; --- enter_ladder_common: set state $03 and reset Y speed ---
code_D4AF:  jsr     reset_sprite_anim   ; set OAM animation (A = OAM ID)
        lda     #PSTATE_LADDER                    ; player state = $03 (on ladder)
        sta     player_state
        lda     #$4C                    ; Y speed = $01.4C (climb speed)
        sta     ent_yvel_sub                   ; (same as walk speed)
        lda     #$01
        sta     ent_yvel
        lda     #$00                    ; $32 = 0 (clear walk/shoot sub-state)
        sta     walk_flag
        clc                             ; carry clear = entered ladder
        rts

code_D4C6:  plp                         ; no ladder found — restore flags
code_D4C7:  rts

; --- check Down+ladder entry (drop through ladder top) ---

code_D4C8:  lda     joy1_held                 ; pressing Down?
        and     #BTN_DOWN                    ; (Down = bit 2)
        beq     code_D4C7               ; no → return
        php                             ; save flags
        lda     tile_at_feet_lo                     ; foot tile = $40 (ladder top)?
        cmp     #TILE_LADDER_TOP                    ; if not, fall through to normal
        bne     check_ladder_entry      ; ladder check (check both feet)
        plp                             ; ladder top confirmed
        lda     ent_x_px                   ; center X on ladder (AND $F0 ORA $08)
        and     #$F0
        ora     #$08
        sta     ent_x_px
        lda     $11                     ; snap Y to tile boundary
        and     #$F0                    ; (top of ladder tile)
        sta     ent_y_px                   ; player Y = aligned
        lda     #$14                    ; OAM $14 = ladder dismount (going down)
        bne     code_D4AF               ; → enter_ladder_common

; ===========================================================================
; player state $03: on ladder — climb up/down, fire, jump off
; ===========================================================================
player_ladder:
        jsr     code_D355               ; slide/crouch input check

; --- B button: fire weapon on ladder ---
        lda     joy1_press                     ; B button pressed?
        and     #BTN_B
        beq     code_D50B               ; no → skip shooting
        lda     joy1_held                     ; D-pad L/R while shooting?
        and     #$03                    ; (aim direction on ladder)
        beq     code_D508               ; no → fire in current direction
        cmp     player_facing                     ; same as current facing?
        beq     code_D508               ; yes → fire normally
        sta     player_facing                     ; update facing direction
        lda     ent_flags                   ; toggle H-flip (bit 6)
        eor     #$40
        sta     ent_flags
code_D508:  jsr     weapon_fire         ; fire weapon

; --- check climbing input ---
code_D50B:  lda     walk_flag                 ; if shooting: return to shoot-on-ladder
        bne     code_D4C7               ; (OAM animation handler above)
        lda     joy1_held                     ; D-pad U/D held?
        and     #$0C
        bne     code_D521               ; yes → climb
        lda     joy1_press                     ; A button → jump off ladder
        and     #BTN_A
        bne     code_D51E
        jmp     code_D5B4               ; no input → idle on ladder (freeze anim)

code_D51E:  jmp     code_D5AD           ; → detach from ladder (state $00 + $99)

; --- climb up/down: center X on ladder, move vertically ---

code_D521:  pha                         ; save D-pad bits
        lda     ent_x_px                   ; center player X on ladder tile:
        and     #$F0                    ; snap to 16px grid + 8
        ora     #$08
        sta     ent_x_px
        pla                             ; bit 2 = down pressed?
        and     #$04
        beq     code_D568               ; no → climb up

; --- climb down ---
        lda     #$0A                    ; OAM $0A = climb-down animation
        cmp     ent_anim_id                   ; (skip reset if already playing)
        beq     code_D53B
        jsr     reset_sprite_anim
code_D53B:  ldy     #$00                ; move down with collision
        jsr     move_down_collide
        bcs     code_D5AD               ; hit ground → detach
        lda     ent_y_scr                   ; if Y screen > 0: wrap Y to 0
        beq     code_D54D               ; (scrolled past screen boundary)
        lda     #$00
        sta     ent_y_px
        rts

; --- check if still on ladder tile (going down) ---

code_D54D:  ldy     #$04                ; check tile collision at feet
        jsr     check_tile_collision
        lda     tile_at_feet_hi                     ; $44/$43 = tile type at check point
        cmp     #TILE_LADDER                    ; $20 = ladder top
        beq     code_D5B9               ; $40 = ladder body
        cmp     #TILE_LADDER_TOP                    ; if ladder found: stay on ladder
        beq     code_D5B9
        lda     tile_at_feet_lo                     ; check other collision point too
        cmp     #TILE_LADDER
        beq     code_D5B9
        cmp     #TILE_LADDER_TOP
        beq     code_D5B9
        bne     code_D5AD               ; no ladder → detach

; --- climb up ---
code_D568:  ldy     #$01                ; move up with collision
        jsr     move_up_collide
        bcs     code_D5B4               ; hit ceiling → freeze anim
        lda     ent_y_scr                   ; if Y screen > 0: wrap Y to $EF
        beq     code_D57A               ; (scrolled past screen boundary)
        lda     #$EF
        sta     ent_y_px
        rts

; --- check if still on ladder tile (going up) ---

code_D57A:  lda     ent_y_px               ; if Y < $10: stay on ladder
        cmp     #$10                    ; (near top of screen)
        bcc     code_D5B9
        ldy     #$04                    ; check tile collision
        jsr     check_tile_collision
        lda     tile_at_feet_hi                     ; $20 = ladder top, $40 = ladder body
        cmp     #TILE_LADDER                    ; if ladder tile found: stay
        beq     code_D5B9
        cmp     #TILE_LADDER_TOP
        beq     code_D5B9
        lda     tile_at_feet_lo
        cmp     #TILE_LADDER
        beq     code_D5B9
        cmp     #TILE_LADDER_TOP
        beq     code_D5B9

; --- reached top of ladder: dismount ---
        lda     #$14                    ; OAM $14 = ladder-top dismount animation
        cmp     ent_anim_id
        beq     code_D5A4
        jsr     reset_sprite_anim
code_D5A4:  lda     ent_y_px               ; check Y position sub-tile:
        and     #$0F                    ; if Y mod 16 >= 12, still climbing
        cmp     #$0C
        bcs     code_D5B9               ; → stay on ladder

; --- detach from ladder: return to state $00 (on_ground) ---
code_D5AD:  lda     #$00                ; $30 = 0 (state = on_ground)
        sta     player_state
        jmp     reset_gravity           ; clear Y speed/$99

; --- freeze ladder animation (idle on ladder) ---

code_D5B4:  lda     #$00
        sta     ent_anim_frame
code_D5B9:  rts

; ===========================================================================
; player state $04: respawn/reappear (death, unpause, stage start) [confirmed]
; ===========================================================================
; Teleport beam drops from top of screen. Two phases:
;   Phase 1: apply_y_speed (constant fall) until Y reaches landing threshold
;   Phase 2: move_vertical_gravity (decelerate + land on ground)
; Shadow Man stage uses lower threshold ($30 vs $68) due to different spawn.
player_reappear:

        lda     ent_anim_state                   ; ent_anim_state = anim frame counter
        bne     code_D5EB               ; if >0: skip movement (anim playing)

; --- phase 1: constant-speed fall until reaching Y threshold ---
        lda     stage_id                     ; Shadow Man stage ($07)?
        cmp     #STAGE_SHADOW
        beq     code_D5D2               ; yes → use lower landing Y
        lda     ent_y_px                   ; normal stages: Y >= $68 → switch to $99
        cmp     #$68
        bcs     code_D5DF
        jsr     apply_y_speed           ; fall at constant speed (no $99)
        jmp     code_D5E6

code_D5D2:  lda     ent_y_px               ; Shadow Man: Y >= $30 → switch to $99
        cmp     #$30
        bcs     code_D5DF
        jsr     apply_y_speed           ; fall at constant speed
        jmp     code_D5E6

; --- phase 2: $99 + ground collision → land ---

code_D5DF:  ldy     #$00                ; apply $99 and check ground
        jsr     move_vertical_gravity   ; carry set = landed
        bcs     code_D5EB               ; landed → proceed to anim check
code_D5E6:  lda     #$00                ; clear anim counter (still falling)
        sta     ent_anim_frame

; --- check landing animation progress ---
code_D5EB:  lda     ent_anim_frame               ; if anim counter == 0: not landed yet
        bne     code_D5FC
        lda     ent_anim_state                   ; if anim frame == 1: play landing sound $34
        cmp     #$01
        bne     code_D5FC
        lda     #$34
        jsr     submit_sound_ID

; --- check reappear animation complete ---
code_D5FC:  lda     ent_anim_state               ; anim frame == 4 → reappear done
        cmp     #$04
        bne     code_D60B               ; not done yet
        lda     #$00                    ; transition to state $00 (on_ground)
        sta     player_state
        sta     walk_flag                     ; clear shooting flag
        sta     invincibility_timer
code_D60B:  rts

code_D60C:  lda     #$00
        sta     player_state
        jmp     reset_gravity

; ===========================================================================
; player state $05: riding Mag Fly — magnetic pull, Magnet Man stage [confirmed]
; ===========================================================================
; Player is attached to a Mag Fly entity (slot $34). The fly pulls player
; upward magnetically. Player can D-pad up/down to adjust, A to jump off.
; If the Mag Fly dies or changes OAM, player detaches (→ state $00 + $99).
player_entity_ride:

        ldy     entity_ride_slot                     ; $34 = entity slot of Mag Fly
        lda     ent_status,y                 ; check if Mag Fly still active
        bpl     code_D60C               ; inactive → detach (state $00)
        lda     ent_anim_id,y                 ; check Mag Fly OAM == $62
        cmp     #$62                    ; (normal Mag Fly animation)
        bne     code_D60C               ; changed → detach

; --- D-pad up/down: manual vertical adjustment ---
        lda     joy1_held                     ; D-pad up/down held? (bits 2-3)
        and     #$0C
        beq     code_D656               ; no vertical input → skip
        sta     L0000                   ; save D-pad state
        lda     ent_yvel_sub                   ; save current Y speed
        pha
        lda     ent_yvel
        pha
        lda     #$40                    ; temp Y speed = $00.40 (slow adjust)
        sta     ent_yvel_sub
        lda     #$00
        sta     ent_yvel
        lda     L0000                   ; bit 3 = up pressed?
        and     #$08
        beq     code_D649               ; no → move down
        ldy     #$01                    ; move up with collision
        jsr     move_up_collide
        jmp     code_D64E

code_D649:  ldy     #$00                ; move down with collision
        jsr     move_down_collide
code_D64E:  pla                         ; restore original Y speed
        sta     ent_yvel
        pla
        sta     ent_yvel_sub

; --- check jump off / horizontal movement when stationary ---
code_D656:  lda     ent_yvel_sub               ; if Y speed nonzero (being pulled):
        ora     ent_yvel                   ; skip jump/walk checks
        bne     code_D67E
        lda     joy1_press                     ; A button → jump off Mag Fly
        and     #BTN_A
        beq     code_D667
        jmp     LCEAD

; --- idle on Mag Fly: walk horizontally using ride direction ---

code_D667:  lda     #$00                ; set walk speed to $01.4C (but use
        sta     ent_xvel_sub                   ; for horizontal movement only)
        lda     #$01
        sta     ent_xvel
        lda     ent_flags                   ; save player sprite flags
        pha
        lda     facing_sub                     ; $35 = ride direction
        jsr     code_CF3D               ; move L/R based on ride direction
        pla                             ; restore sprite flags (preserve facing)
        sta     ent_flags

; --- magnetic pull: constant upward force with $99 cap ---
code_D67E:  ldy     #$01                ; move upward with collision check
        jsr     move_up_collide
        lda     ent_yvel_sub                   ; Y speed += $00.40 ($99 pull-down)
        clc                             ; counteracts magnetic pull
        adc     #$40
        sta     ent_yvel_sub
        lda     ent_yvel
        adc     #$00
        sta     ent_yvel
        cmp     #$07                    ; cap Y speed at $07 (terminal velocity)
        bcc     code_D69D
        lda     #$00
        sta     ent_yvel_sub                   ; clear sub when capping
code_D69D:  lda     #$4C                ; reset walk speed = $01.4C
        sta     ent_xvel_sub
        lda     #$01
        sta     ent_xvel
        jsr     code_D0C1               ; horizontal movement + animation
        rts

; ===========================================================================
; player state $06: taking damage — knockback with $99
; ===========================================================================
; On first call: saves current OAM, sets damage animation, spawns hit flash
; (slot 4), applies $99 + knockback movement. Knockback direction is
; opposite to facing (ent_flags bit 6). On subsequent frames, continues knockback
; physics until player touches ground or ent_anim_state signals landing ($09).
; $3E = saved OAM ID (to restore after damage animation ends).
; ---------------------------------------------------------------------------
player_damage:

        lda     ent_anim_id                   ; already in damage anim?
        cmp     #$11                    ; OAM $11 = normal damage pose
        beq     code_D701               ; yes → skip init, continue knockback
        cmp     #$B1                    ; OAM $B1 = Rush Marine damage pose
        beq     code_D701               ; yes → skip init

; --- first frame: initialize damage state ---
        lda     ent_anim_id                   ; $3E = save current OAM ID
        sta     $3E                     ; (restored when damage ends)
        cmp     #$D9                    ; OAM >= $D9 = Rush Marine variants
        bcs     code_D6C3               ; → use $B1 damage anim
        lda     #$11                    ; normal damage OAM = $11
        bne     code_D6C5
code_D6C3:  lda     #$B1                ; Rush Marine damage OAM = $B1
code_D6C5:  jsr     reset_sprite_anim   ; set player damage animation
        lda     #$00                    ; $32 = 0 (clear shoot timer)
        sta     walk_flag
        jsr     reset_gravity           ; reset vertical velocity

; --- spawn hit flash effect in slot 4 ---
        lda     #$80                    ; slot 4 type = $80 (active)
        sta     $0304
        lda     ent_flags                   ; slot 4 facing = copy player facing
        sta     $0584
        ldx     #$04                    ; slot 4 OAM = $12 (hit flash sprite)
        lda     #$12
        jsr     reset_sprite_anim
        lda     ent_x_px                   ; slot 4 position = player position
        sta     $0364
        lda     ent_x_scr
        sta     $0384
        lda     ent_y_px
        sta     $03C4
        lda     ent_y_scr
        sta     $03E4
        ldx     #$00                    ; slot 4: no hitbox, no AI routine
        stx     $0484                   ; (visual-only, like slide dust)
        stx     $0324

; --- knockback physics: $99 + horizontal recoil ---
code_D701:  lda     $3E                 ; check saved OAM for special cases:
        cmp     #$10                    ; $10 = climbing (no knockback physics)
        beq     code_D742
        cmp     #$79                    ; >= $79 = special state (skip physics)
        bcs     code_D742
        ldy     #$00                    ; apply $99 (Y=0 = slot 0 = player)
        jsr     move_vertical_gravity
        lda     #$80                    ; x_speed = $00.80 (0.5 px/frame knockback)
        sta     ent_xvel_sub
        lda     #$00
        sta     ent_xvel
        lda     ent_flags                   ; save facing (move_collide can change it)
        pha
        lda     ent_flags                   ; bit 6 = facing direction
        and     #$40                    ; knockback goes OPPOSITE to facing
        bne     code_D72D               ; facing right → knock left
        ldy     #$00                    ; facing left → knock right
        jsr     move_right_collide
        jmp     code_D732

code_D72D:  ldy     #$01                ; facing right → knock left
        jsr     move_left_collide
code_D732:  lda     ent_x_px               ; sync slot 4 (hit flash) position with player
        sta     $0364
        lda     ent_x_scr
        sta     $0384
        pla                             ; restore original facing direction
        sta     ent_flags

; --- check for landing: ent_anim_state = $09 means landed on ground ---
code_D742:  lda     ent_anim_state               ; ent_anim_state = animation/collision state
        cmp     #$09                    ; $09 = landed after knockback
        bne     code_D778               ; not landed → continue knockback

; --- landed: restore OAM and start invincibility frames ---
        lda     $3E                     ; saved OAM determines recovery anim
        pha
        cmp     #$10                    ; $10 = climbing → reset anim
        beq     code_D754
        cmp     #$D9                    ; >= $D9 = Rush Marine → reset anim
        bcc     code_D757               ; else keep current anim
code_D754:  jsr     reset_sprite_anim   ; reset to saved OAM
code_D757:  pla                         ; determine return state:
        cmp     #$10                    ; $10 (climbing) → state $02
        beq     code_D764
        cmp     #$D9                    ; >= $D9 (Rush Marine) → state $08
        bcc     code_D768               ; else → state $00 (normal)
        lda     #$08                    ; return state = $08 (Rush Marine)
        bne     code_D76A
code_D764:  lda     #$02                ; return state = $02 (climbing)
        bne     code_D76A
code_D768:  lda     #$00                ; return state = $00 (normal)
code_D76A:  sta     player_state                 ; $30 = next player state
        lda     #$3C                    ; $39 = $3C (60 frames invincibility)
        sta     invincibility_timer
        lda     ent_anim_frame                   ; set ent_anim_frame bit 7 = invincible flag
        ora     #$80
        sta     ent_anim_frame
code_D778:  rts

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

        lda     #$00                    ; $3D = 0 (clear shooting flag)
        sta     hazard_pending
        lda     ent_anim_id                   ; already set to explosion OAM?
        cmp     #$7A                    ; $7A = explosion sprite
        beq     code_D7E7               ; yes → skip init, continue upward motion
        lda     ent_y_scr                   ; y_screen != 0 → off-screen, skip to timer
        bne     code_D7EA

; --- init 16 explosion fragments (slots 0-15) ---
        ldy     #$0F                    ; Y = slot index (15 down to 0)
code_D78B:  lda     #$7A                ; OAM = $7A (explosion sprite)
        sta     ent_anim_id,y
        lda     #$00                    ; clear misc fields
        sta     ent_anim_frame,y                 ; ent_anim_frame = 0
        sta     ent_anim_state,y                 ; ent_anim_state = 0
        sta     ent_hitbox,y                 ; hitbox = 0 (no collision)
        lda     ent_x_px                   ; copy player position to this slot
        sta     ent_x_px,y                 ; x_pixel
        lda     ent_x_scr                   ; x_screen
        sta     ent_x_scr,y
        lda     ent_y_px                   ; y_pixel
        sta     ent_y_px,y
        lda     ent_y_scr                   ; y_screen
        sta     ent_y_scr,y
        lda     #$10                    ; routine = $10 (generic mover — applies velocity)
        sta     ent_routine,y
        lda     #$80                    ; type = $80 (active entity)
        sta     ent_status,y
        lda     #$90                    ; flags = $90 (active + bit 4)
        sta     ent_flags,y
        lda     death_burst_x_speed_sub,y ; x_speed_sub from radial velocity table
        sta     ent_xvel_sub,y
        lda     death_burst_x_speed,y   ; x_speed from radial velocity table
        sta     ent_xvel,y
        lda     death_burst_y_speed_sub,y ; y_speed_sub from radial velocity table
        sta     ent_yvel_sub,y
        lda     death_burst_y_speed,y   ; y_speed from radial velocity table
        sta     ent_yvel,y
        dey                             ; loop for all 16 slots
        bpl     code_D78B

; --- override slot 0 (player body): flies straight up ---
        lda     #$00                    ; y_speed_sub = 0
        sta     ent_yvel_sub
        lda     #$03                    ; y_speed = $03 (3.0 px/frame upward,
        sta     ent_yvel                   ; stored as positive = up for move_sprite_up)

; --- ongoing: move player body upward ---
code_D7E7:  jsr     move_sprite_up      ; move slot 0 up at $03.00/frame

; --- increment death timer ($3F low byte, $3C high byte) ---
code_D7EA:  inc     $3F                 ; $3F++, if overflow then $3C++
        bne     code_D7F0               ; (16-bit frame counter for death sequence)
        inc     $3C
code_D7F0:  .byte   $60

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
        .byte   $FE,$FE,$00,$01,$01,$01
        brk
        inc     a:$A9,x
        sta     ent_anim_frame
        dec     ent_timer                   ; decrement timer
        bne     code_D857               ; not zero → wait
        lda     #$1E                    ; reset timer = $1E (30 frames per cycle)
        sta     ent_timer
        ldy     #$68                    ; Y = OAM offset ($68 = last of 27 entries × 4)
code_D842:  lda     $0201,y             ; tile ID = $0201 + Y (byte 1 of OAM entry)
        clc
        adc     #$01                    ; increment tile ID
        cmp     #$FA                    ; if reached $FA, wrap to $F7
        bne     code_D84E
        lda     #$F7
code_D84E:  sta     $0201,y             ; write back modified tile
        dey                             ; next OAM entry (Y -= 4)
        dey
        dey
        dey
        bpl     code_D842               ; loop until Y < 0
code_D857:  rts

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

        jsr     decrease_ammo_tick                   ; drain ammo over time
        lda     ent_anim_id                   ; intro anim check: $DA = mounting Rush
        cmp     #$DA
        bne     code_D86E               ; not mounting → skip
        lda     ent_anim_state                   ; wait for mount anim to complete
        cmp     #$03                    ; anim state $03 = done
        bne     code_D857               ; not done → wait (returns via earlier RTS)
        lda     #$DB                    ; transition to land-mode OAM
        jsr     reset_sprite_anim

; --- check environment: water or land ---
code_D86E:  ldy     #$06                ; check tile at player's feet
        jsr     check_tile_horiz        ; Y=$06 = below center
        lda     $10                     ; $10 = tile type result
        cmp     #$80                    ; $80 = water tile
        beq     code_D886               ; water → underwater mode

; --- land mode: $99 + horizontal movement ---
        lda     #$DB                    ; ensure land-mode OAM = $DB
        cmp     ent_anim_id
        beq     code_D893               ; already set → skip anim reset
        jsr     reset_sprite_anim       ; switch to $DB anim
        jmp     code_D893

; --- water mode: buoyancy + vertical d-pad ---

code_D886:  lda     #$DC                ; ensure water-mode OAM = $DC
        cmp     ent_anim_id
        beq     code_D8BD               ; already set → skip
        jsr     reset_sprite_anim       ; switch to $DC anim
        jmp     code_D8BD

; --- land physics: $99 + A=jump ---

code_D893:  ldy     #$00                ; apply $99
        jsr     move_vertical_gravity
        bcc     code_D8AF               ; airborne → check horizontal input
        lda     joy1_press                     ; A button pressed? (bit 7)
        and     #BTN_A
        beq     code_D910               ; no → skip to weapon check
code_D8A0:  lda     #$E5                ; y_speed = $04.E5 (jump velocity, same as normal)
        sta     ent_yvel_sub
        lda     #$04
        sta     ent_yvel
        ldy     #$00                    ; apply $99 this frame too (for smooth arc)
        jsr     move_vertical_gravity
code_D8AF:  lda     joy1_held                 ; $16 bits 0-1 = d-pad left/right
        and     #$03
        beq     code_D910               ; no L/R → skip to weapon check
        sta     player_facing                     ; $31 = direction (1=R, 2=L)
        jsr     code_CF3D               ; horizontal movement handler
        jmp     code_D910

; --- water physics: slow sink + A=thrust, d-pad U/D ---

code_D8BD:  lda     joy1_press                 ; A button pressed?
        and     #BTN_A
        beq     code_D8CE               ; no → slow sink
        ldy     #$01                    ; check tile above (Y=$01)
        jsr     check_tile_horiz
        lda     $10                     ; still water above?
        cmp     #$80
        bne     code_D8A0               ; surfacing → apply jump velocity

; --- underwater: no thrust, slow sink ---
code_D8CE:  lda     joy1_held                 ; d-pad L/R?
        and     #$03
        beq     code_D8D9               ; no → skip horizontal
        sta     player_facing                     ; horizontal movement
        jsr     code_CF3D
code_D8D9:  lda     #$80                ; y_speed = $01.80 (slow sink in water)
        sta     ent_yvel_sub
        lda     #$01
        sta     ent_yvel
        lda     joy1_held                     ; d-pad U/D? (bits 2-3)
        and     #$0C
        beq     code_D910               ; no → skip to weapon
        and     #$04                    ; bit 2 = down
        bne     code_D90B               ; down → move down

; --- d-pad up: move up, snap to surface if surfacing ---
        ldy     #$01                    ; move up with collision
        jsr     move_up_collide
        ldy     #$06                    ; re-check water tile at feet
        jsr     check_tile_horiz
        lda     $10                     ; still in water?
        cmp     #$80
        beq     code_D908               ; yes → done
        lda     ent_y_px                   ; surfaced: snap Y to next tile boundary
        and     #$F0                    ; (align to water surface)
        clc
        adc     #$10
        sta     ent_y_px
code_D908:  jmp     code_D910

; --- d-pad down ---

code_D90B:  ldy     #$00                ; move down with collision
        jmp     move_down_collide

; --- common: handle facing direction + weapon fire ---

code_D910:  lda     ent_anim_id               ; save current OAM (Rush Marine sprite)
        pha
        jsr     code_D355               ; handle facing direction change
        lda     joy1_press                     ; B button pressed? (bit 6)
        and     #BTN_B
        beq     code_D920               ; no → skip
        jsr     weapon_fire             ; fire weapon (changes OAM to shoot pose)
code_D920:  lda     #$00                ; clear shoot timer
        sta     walk_flag
        pla                             ; restore Rush Marine OAM (undo shoot pose change)
        sta     ent_anim_id
        rts

; ===========================================================================
; player state $09: frozen during boss intro (shutters/HP bar) [confirmed]
; ===========================================================================
; Player stands still while boss intro plays (shutters close, HP bars fill).
; Applies $99 until grounded, then sets standing OAM. Handles two phases:
;   1. Wily4 ($22=$10): progressively draws boss arena nametable (column $1A)
;   2. HP bar fill: $B0 increments from $80 to $9C (28 ticks = full HP bar),
;      playing sound $1C each tick. When $B0 reaches $9C, returns to state $00.
; ---------------------------------------------------------------------------
player_boss_wait:

        ldy     #$00                    ; apply $99
        jsr     move_vertical_gravity
        bcc     code_D990               ; airborne → wait for landing

; --- grounded: set to standing pose ---
        lda     #$01                    ; OAM $01 = standing
        cmp     ent_anim_id                   ; already standing?
        beq     code_D942               ; yes → skip anim reset
        sta     ent_anim_id                   ; set standing OAM
        lda     #$00
        sta     ent_anim_frame                   ; clear anim state
        sta     ent_anim_state

; --- Wily4 special: draw boss arena nametable progressively ---
code_D942:  lda     stage_id                 ; stage $10 = Wily4 (boss refight arena)
        cmp     #STAGE_WILY5
        bne     code_D973               ; not Wily4 → skip to HP fill
        ldy     #$26                    ; $52 = $26 (set viewport for Wily4 arena)
        cpy     $52                     ; already set?
        beq     code_D95B               ; yes → check render progress
        sty     $52                     ; first time: initialize nametable render
        ldy     #$00                    ; $A000 bank target = 0
        sty     LA000
        sty     $70                     ; render progress counter = 0
        sty     $28                     ; column counter = 0
        beq     code_D95F
code_D95B:  ldy     $70                 ; $70 = 0 means render complete
        beq     code_D973               ; done → skip to HP fill
code_D95F:  sta     prg_bank                 ; switch to stage bank
        jsr     select_PRG_banks
        lda     #$1A                    ; metatile column $1A (boss arena column)
        jsr     metatile_column_ptr_by_id
        lda     #$04                    ; nametable select = $04
        sta     $10
        jsr     fill_nametable_progressive ; draw one column per frame
        ldx     #$00                    ; restore X = player slot
        rts

; --- HP bar fill sequence ---

code_D973:  lda     boss_hp_display                 ; $B0 = boss HP bar value
        cmp     #$9C                    ; $9C = full (28 units)
        beq     code_D98C               ; full → end boss intro
        lda     $95                     ; $95 = frame counter, tick every 4th frame
        and     #$03
        bne     code_D990               ; not tick frame → wait
        inc     boss_hp_display                     ; increment HP bar
        lda     boss_hp_display
        cmp     #$81                    ; skip sound on first tick ($81)
        beq     code_D990
        lda     #$1C                    ; sound $1C = HP fill tick
        jmp     submit_sound_ID

; --- boss intro complete: return to normal state ---

code_D98C:  lda     #$00                ; $30 = state $00 (normal)
        sta     player_state
code_D990:  rts

; ===========================================================================
; player state $0A: Top Spin recoil bounce [confirmed]
; ===========================================================================
; Applies $99 and horizontal movement in facing direction for ent_timer
; frames. When timer expires or player lands on ground, returns to state $00.
; Facing direction is preserved across move_collide calls.
; ---------------------------------------------------------------------------
player_top_spin:

        ldy     #$00                    ; apply $99 to player
        jsr     move_vertical_gravity
        bcs     code_D9B6               ; landed on ground → end recoil
        lda     ent_flags                   ; save facing (move_collide may change it)
        pha
        and     #$40                    ; bit 6: 0=right, 1=left
        bne     code_D9A8
        ldy     #$00                    ; facing right → move right
        jsr     move_right_collide
        jmp     code_D9AD

code_D9A8:  ldy     #$01                ; facing left → move left
        jsr     move_left_collide
code_D9AD:  pla                         ; restore original facing
        sta     ent_flags
        dec     ent_timer                   ; decrement recoil timer
        bne     code_D9BD               ; not expired → continue
code_D9B6:  lda     #$00                ; timer expired or landed:
        sta     ent_timer                   ; clear timer
        sta     player_state                     ; $30 = state $00 (normal)
code_D9BD:  rts

; ===========================================================================
; player state $0B: Hard Knuckle fire freeze [confirmed]
; ===========================================================================
; Freezes player for ent_var1 frames after firing Hard Knuckle. When timer
; expires, restores pre-freeze state from ent_timer and matching OAM from
; $D297 table, clears shoot timer $32.
; ---------------------------------------------------------------------------
player_weapon_recoil:

        dec     ent_var1                   ; decrement freeze timer
        bne     code_D9D2               ; not zero → stay frozen
        ldy     ent_timer                   ; ent_timer = saved pre-freeze state
        sty     player_state                     ; $30 = return to that state
        lda     LD297,y                 ; look up OAM ID for that state
        jsr     reset_sprite_anim       ; set player animation
        lda     #$00                    ; clear shoot timer
        sta     walk_flag
code_D9D2:  rts

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
player_victory:

        lda     ent_timer                   ; ent_timer low nibble = phase counter
        and     #$0F
        beq     code_DA31               ; phase 0 → skip to $99/walk

; --- phases 1-4: wait for landing, then process explosions ---
        lda     ent_yvel                   ; y_speed: negative = still rising from jump
        bpl     code_DA31               ; positive/zero = falling/grounded → continue
        lda     #$68                    ; clamp Y to landing position $68
        cmp     ent_y_px
        beq     code_D9EB               ; at $68 → proceed
        bcs     code_DA31               ; above $68 → still falling, wait
        sta     ent_y_px                   ; below $68 → snap to $68

; --- check if all explosion entities (slots $10-$1F) are inactive ---
code_D9EB:  ldy     #$0F                ; check slots $10-$1F (indices relative)
code_D9ED:  lda     $0310,y             ; $0310+Y = entity type for slots $10-$1F
        bmi     code_DA02               ; bit 7 set = still active → wait
        dey
        bpl     code_D9ED               ; loop through all 16 slots

; --- all explosions finished: advance phase ---
        lda     ent_timer                   ; phase 4 = all explosion batches done
        cmp     #$04
        beq     code_DA03               ; → award weapon
        jsr     code_DB3A               ; spawn next batch of victory explosions
        inc     ent_timer                   ; advance phase
code_DA02:  rts

; --- phase 4 complete: award weapon and transition ---

code_DA03:  lda     #$31                ; sound $31 = weapon get jingle
        jsr     submit_sound_ID
        ldy     stage_id                     ; Y = stage index
        lda     victory_weapon_id_table,y ; $DD04 table = weapon ID per stage
        sta     weapon_cursor                     ; $A1 = weapon menu cursor position
        lda     LDD0C,y                 ; $DD0C table = weapon ammo slot offset
        sta     $B4                     ; $B4 = ammo slot index
        clc                             ; Y = ammo slot + weapon ID = absolute ammo address
        adc     weapon_cursor
        tay
        lda     #$80                    ; set ammo to $80 (empty, will be filled by HUD)
        sta     player_hp,y                   ; ($A2+offset = weapon ammo array)
        lda     #$02                    ; bank $02 = HUD/menu update code
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     LA000                   ; update HUD for new weapon
        lda     #PSTATE_TELEPORT                    ; $30 = state $0D (teleport away)
        sta     player_state
        lda     #$80                    ; player type = $80 (active)
        sta     ent_status
        rts

; --- victory phase 0: $99 + palette flash + walk to center ---

code_DA31:  lda     stage_id                 ; Wily stages ($22 >= $10): palette flash effect
        cmp     #STAGE_WILY5
        bcc     code_DA55               ; not Wily → skip flash
        lda     $95                     ; flash every 8 frames
        and     #$07
        bne     code_DA55
        lda     ent_var1                   ; ent_var1 = music countdown timer
        beq     code_DA49               ; if 0 → toggle
        lda     $0610                   ; SP0 color 0: if $0F → stop flashing
        cmp     #$0F                    ; (palette restored to normal)
        beq     code_DA55
code_DA49:  lda     $0610               ; toggle SP0 palette entry: XOR with $2F
        eor     #$2F                    ; (flashes between dark and light)
        sta     $0610
        lda     #$FF                    ; trigger palette update
        sta     palette_dirty

; --- apply $99 ---
code_DA55:  ldy     #$00                ; apply $99 to player
        jsr     move_vertical_gravity
        bcc     code_DA6C               ; airborne → skip standing check
        lda     ent_var1                   ; music timer active?
        beq     code_DA6C               ; no → skip
        lda     #$01                    ; set OAM to standing ($01) if not already
        cmp     ent_anim_id
        beq     code_DA6C               ; already standing → skip
        jsr     reset_sprite_anim       ; set standing animation
        sec                             ; set carry = grounded flag
code_DA6C:  rol     $0F                 ; $0F bit 0 = grounded this frame

; --- wait for boss explosion entities (slots $10-$1F) to finish ---
        ldy     #$0F
code_DA70:  lda     stage_id                 ; Wily5 ($22=$11): skip checking slot $10
        cmp     #STAGE_WILY6                    ; Wily 6: skip slot $10 check
        bne     code_DA7A
        cpy     #$00                    ; at Y=0 (slot $10) → skip to next phase
        beq     code_DA82
code_DA7A:  lda     $0310,y             ; check if slot still active
        bmi     code_DA02               ; active → wait (return)
        dey                             ; next slot
        bpl     code_DA70

; --- all explosions done: determine stage-specific exit behavior ---
code_DA82:  lda     stage_id                 ; Doc Robot stages ($08-$0B)?
        cmp     #STAGE_DOC_NEEDLE
        bcc     code_DA92
        cmp     #STAGE_WILY1
        bcs     code_DA92
        lda     camera_screen                     ; camera screen < $18? → skip music
        cmp     #$18                    ; (Doc Robot in early part of stage)
        bcc     code_DAC8

; --- play victory music ---
code_DA92:  lda     stage_id                 ; Wily5 ($11) → music $37 (special victory)
        cmp     #STAGE_WILY6
        bne     code_DAA6
        lda     #$37                    ; already playing?
        cmp     $D9
        beq     code_DAB4               ; yes → skip
        lda     #$00                    ; reset nametable select (scroll to 0)
        sta     camera_x_hi
        lda     #$37                    ; music $37
        bne     code_DAAC
code_DAA6:  lda     #$38                ; normal victory music = $38
        cmp     $D9                     ; already playing?
        beq     code_DAB4               ; yes → skip
code_DAAC:  jsr     submit_sound_ID_D9  ; start music
        lda     #$FF                    ; ent_var1 = $FF (255 frame countdown timer)
        sta     ent_var1

; --- countdown music timer, then transition ---
code_DAB4:  lda     ent_var1               ; timer done?
        beq     code_DAC8               ; yes → proceed to exit
        dec     ent_var1                   ; decrement music timer
        bne     code_DB2B               ; not zero → wait (walk to center)
        lda     stage_id                     ; Wily stages ($22 >= $10): reset nametable
        cmp     #STAGE_WILY5
        bcc     code_DAC8
        lda     #$00                    ; $FD = 0 (reset scroll)
        sta     camera_x_hi

; --- determine exit type based on stage ---
code_DAC8:  lda     stage_id                 ; stages $00-$07 (robot master): walk to center
        cmp     #STAGE_DOC_NEEDLE
        bcc     code_DAD1               ; → walk to X=$80
        jmp     code_DB89               ; stages $08+ → alternate exit

; --- walk to screen center (X=$80) for robot master stages ---

code_DAD1:  sty     L0000               ; save Y (slot check result)
        lda     ent_x_px                   ; compare player X to center ($80)
        cmp     #$80
        beq     code_DB00               ; at center → done walking
        bcs     code_DAEF               ; X > $80 → walk left

; --- walk right toward center ---
        ldy     #$00                    ; move right with collision
        jsr     move_right_collide
        rol     L0000                   ; save collision result
        lda     #$80                    ; clamp: don't overshoot $80
        cmp     ent_x_px
        bcs     code_DB00               ; X <= $80 → ok
        sta     ent_x_px                   ; snap to $80
        bcc     code_DB00

; --- walk left toward center ---
code_DAEF:  ldy     #$01                ; move left with collision
        jsr     move_left_collide
        rol     L0000                   ; save collision result
        lda     #$80                    ; clamp: don't undershoot $80
        cmp     ent_x_px
        bcc     code_DB00               ; X >= $80 → ok
        sta     ent_x_px                   ; snap to $80

; --- set walking/standing animation based on ground state ---
code_DB00:  ldy     #$00                ; $0F bit 0 = grounded (from ROL earlier)
        lsr     $0F                     ; carry = grounded
        bcs     code_DB07               ; grounded → Y=0 (walk anim)
        iny                             ; airborne → Y=1 (jump anim)
code_DB07:  lda     victory_walk_oam_table,y ; $DC7F = walk/jump OAM lookup table
        cmp     ent_anim_id                   ; already correct OAM?
        beq     code_DB12               ; yes → skip anim reset
        jsr     reset_sprite_anim       ; set new animation
code_DB12:  cpy     #$01                ; airborne (Y=1)? → done for this frame
        beq     code_DB88
        lsr     L0000                   ; check collision flag from walk
        bcc     code_DB88               ; no collision → done

; --- grounded at center: do victory jump ---
        lda     ent_x_px                   ; at center X=$80?
        cmp     #$80
        beq     code_DB2C               ; yes → super jump!
        lda     #$E5                    ; not at center: normal jump velocity
        sta     ent_yvel_sub                   ; y_speed = $04.E5
        lda     #$04
        sta     ent_yvel
code_DB2B:  rts

; --- victory super jump at screen center ---

code_DB2C:  lda     #$00                ; y_speed = $08.00 (super jump!)
        sta     ent_yvel_sub
        lda     #$08
        sta     ent_yvel
        inc     ent_timer                   ; advance to next phase
        rts

; ---------------------------------------------------------------------------
; spawn_victory_explosions — create one batch of celebratory explosions
; ---------------------------------------------------------------------------
; Fills entity slots $10-$1F with explosion effects. Each batch uses a
; different velocity table offset from $DD00 (indexed by ent_timer = batch #).
; Entity type $5B = explosion effect, routine $11 = velocity mover.
; Positions from $DCE1 (X) and $DCD1 (Y) tables, velocities from $DC71+.
; ---------------------------------------------------------------------------

code_DB3A:  ldy     ent_timer               ; velocity table offset from batch number
        ldx     LDD00,y
        ldy     #$1F                    ; Y = slot $1F (fill backwards to $10)
code_DB42:  lda     #$5B                ; entity type $5B = victory explosion
        jsr     init_child_entity
        lda     #$00                    ; no hitbox (visual only)
        sta     ent_hitbox,y
        lda     #$11                    ; routine $11 = constant velocity mover
        sta     ent_routine,y
        lda     victory_y_positions,y   ; x_pixel from position table
        sta     ent_x_px,y
        lda     ent_x_scr                   ; x_screen = player's screen
        sta     ent_x_scr,y
        lda     LDCD1,y                 ; y_pixel from position table
        sta     ent_y_px,y
        lda     LDC71,x                 ; x_speed_sub from velocity table
        sta     ent_xvel_sub,y
        lda     LDC89,x                 ; x_speed from velocity table
        sta     ent_xvel,y
        lda     LDCA1,x                 ; y_speed_sub from velocity table
        sta     ent_yvel_sub,y
        lda     LDCB9,x                 ; y_speed from velocity table
        sta     ent_yvel,y
        dex                             ; next slot + velocity entry
        dey
        cpy     #$0F                    ; loop until Y = $0F (slot $10 done)
        bne     code_DB42
        lda     #$32                    ; sound $32 = explosion burst
        jsr     submit_sound_ID
        ldx     #$00                    ; restore X = player slot
code_DB88:  rts

; --- alternate exit paths for non-RM stages ---

code_DB89:  lda     stage_id                 ; Wily stages ($22 >= $10)?
        cmp     #STAGE_WILY5
        bcs     code_DBB5               ; → Wily exit
        cmp     #STAGE_WILY1                    ; Wily1-3 ($0C-$0F)?
        bcs     code_DBA6               ; → teleport exit
        lda     camera_screen                     ; Doc Robot: camera screen >= $18?
        cmp     #$18                    ; (deep in stage = Doc Robot area)
        bcs     code_DBA6               ; → teleport exit

; --- normal exit: return to stage select ---
        lda     #$00                    ; $30 = state $00 (triggers stage clear)
        sta     player_state
        ldy     stage_id                     ; play stage-specific ending music
        lda     LCD0C,y                 ; from $CD0C table
        jsr     submit_sound_ID_D9
        rts

; --- fortress exit: teleport away ---

code_DBA6:  lda     #$81                ; player type = $81 (teleport state)
        sta     ent_status
        lda     #$00                    ; clear phase counter
        sta     ent_timer
        lda     #PSTATE_TELEPORT                    ; $30 = state $0D (teleport)
        sta     player_state
        rts

; --- Wily exit: restore palette, set up next Wily stage ---

code_DBB5:  lda     #$0F                ; restore SP0 palette to black ($0F)
        sta     $0610                   ; (undo victory flash)
        sta     palette_dirty                     ; trigger palette update
        lda     #$00                    ; clear various state:
        sta     game_mode                     ; $F8 = game mode
        sta     $6A                     ; $6A = cutscene flag
        sta     $6B                     ; $6B = cutscene flag
        sta     camera_x_hi                     ; $FD = nametable select
        lda     stage_id                     ; $30 = $22 + 4 (advance to next Wily stage)
        clc                             ; stages $10→$14, $11→$15, etc.
        adc     #$04                    ; (maps to player states $14/$15 = auto_walk)
        sta     player_state
        lda     #$80                    ; player type = $80
        sta     ent_status
        lda     #$00                    ; clear all timer/counter fields
        sta     ent_timer
        sta     ent_var1
        sta     ent_var2
        sta     ent_var3
        rts

; ===========================================================================
; player state $0D: teleport away [confirmed]
; ===========================================================================
; Two-phase teleport departure after boss defeat:
;   Phase 1 (ent_status & $0F == 0): fall with $99, land, wait $3C (60) frames
;   Phase 2 (ent_status & $0F != 0): start teleport beam, accelerate upward,
;     track boss defeat when player leaves screen (y_screen != 0)
; ---------------------------------------------------------------------------
player_teleport:

        lda     ent_status                   ; phase check: low nibble of entity type
        and     #$0F                    ; 0 = falling/landing, !0 = teleporting
        bne     code_DC0F

; --- phase 1: fall and land ---
        ldy     #$00                    ; apply $99
        jsr     move_vertical_gravity
        bcs     code_DBFB               ; landed → start wait timer
        lda     ent_anim_state                   ; anim state $04 = landing frame
        cmp     #$04
        bne     code_DC0E               ; not at landing frame → wait
        lda     #$07                    ; set OAM $07 = landing pose
        jmp     reset_sprite_anim

; --- landed: stop movement, start wait timer ---

code_DBFB:  inc     ent_status               ; advance phase ($80 → $81)
        sta     ent_yvel_sub                   ; clear vertical speed (A=0 from BCS)
        sta     ent_yvel
        lda     #$3C                    ; ent_timer = $3C (60 frame wait before beam)
        sta     ent_timer
        lda     #$01                    ; OAM $01 = standing
        jsr     reset_sprite_anim
code_DC0E:  rts

; --- phase 2: teleport beam ---

code_DC0F:  lda     ent_timer               ; wait timer still counting?
        beq     code_DC18               ; done → start beam
        dec     ent_timer                   ; decrement timer
        rts

; --- start teleport beam animation ---

code_DC18:  lda     #$13                ; OAM $13 = teleport beam
        cmp     ent_anim_id                   ; already set?
        beq     code_DC2C               ; yes → skip init
        jsr     reset_sprite_anim       ; set beam animation
        lda     #$34                    ; sound $34 = teleport beam
        jsr     submit_sound_ID
        lda     #$04                    ; ent_anim_state = $04 (beam startup frame)
        sta     ent_anim_state

; --- beam active: accelerate upward ---
code_DC2C:  lda     ent_anim_state               ; anim state $02 = beam fully formed
        cmp     #$02
        bne     code_DC73               ; not ready → wait
        lda     #$00                    ; clear animation field
        sta     ent_anim_frame
        lda     ent_yvel_sub                   ; y_speed_sub += $99 (acceleration)
        clc                             ; $99 = teleport acceleration constant
        adc     gravity
        sta     ent_yvel_sub
        lda     ent_yvel                   ; y_speed += carry (16-bit add)
        adc     #$00
        sta     ent_yvel
        jsr     move_sprite_up          ; move player upward

; --- boss defeat tracking ---
; called during teleport-away after boss explodes
; sets bit in $61 for defeated stage, awards special weapons
        lda     ent_y_scr                   ; if boss not defeated,
        beq     code_DC73               ; skip defeat tracking
        ldy     stage_id                     ; stage index
        cpy     #$0C                    ; if stage >= $0C (Wily fortress),
        bcs     wily_stage_clear        ; handle separately
        lda     bosses_beaten                     ; $61 |= (1 << stage_index)
        ora     LDEC2,y                 ; mark this stage's boss as defeated
        sta     bosses_beaten                     ; ($FF = all 8 Robot Masters beaten)
        inc     $59                     ; advance stage progression
        lda     stage_id                     ; stage $00 (Needle Man):
        cmp     #$00                    ; awards Rush Jet
        beq     LDC6F
        cmp     #STAGE_SHADOW                    ; stage $07 (Shadow Man):
        bne     code_DC73               ; awards Rush Marine
        lda     #$9C                    ; fill Rush Marine energy ($AB)
        sta     $AB                     ; $9C = full
        rts

LDC6F:  lda     #$9C                    ; fill Rush Jet energy ($AD)
LDC71:  sta     $AD                     ; $9C = full
code_DC73:  rts

wily_stage_clear:  lda     #$FF         ; mark fortress stage cleared
        sta     $74
        inc     $75                     ; advance fortress progression
        lda     #$9C                    ; refill player health
        .byte   $85,$A2,$60             ; $9C = full

; victory animation lookup tables
; $DC7F: walk/jump OAM IDs (Y=0 → walk $04, Y=1 → jump $07)
victory_walk_oam_table:  .byte   $04,$07,$00,$C2,$00,$C2,$00,$3E
        .byte   $00,$3E
LDC89:  .byte   $00,$E1,$00,$E1,$00,$1F,$00,$1F
        .byte   $00,$F1,$80,$F1,$00,$0F,$80,$0F
victory_x_speed:
        .byte   $00,$FB,$FA,$FB,$00,$04,$06,$04
LDCA1:  .byte   $00,$FD,$FD,$FD,$00,$02,$03,$02
        .byte   $00,$FE,$FE,$FE,$00,$01,$01,$01
victory_y_speed_sub:
        .byte   $00,$3E,$00,$C2,$00,$C2,$00,$3E
LDCB9:  .byte   $00,$1F,$00,$E1,$00,$E1,$00,$1F
        .byte   $80,$0F,$00,$F1,$80,$F1,$00,$0F
victory_y_speed:
        .byte   $06,$04,$00,$FB,$FA,$FB,$00,$04
LDCD1:  .byte   $03,$02,$00,$FD,$FD,$FD,$00,$02
        .byte   $01,$01,$00,$FE,$FE,$FE,$00,$01

; explosion starting Y positions (slots $10-$1F)
victory_y_positions:  .byte   $00,$23,$78,$C0,$F0,$CC,$78,$23
        .byte   $00,$23,$78,$C0,$F0,$CC,$78,$23

; explosion starting X positions (slots $10-$1F)
victory_x_positions:
        .byte   $80,$D4,$F8,$D4,$80,$2B,$08,$2B
        .byte   $80,$D4,$F8,$D4,$80,$2B,$08
LDD00:  .byte   $2B,$1F,$1F,$27

; weapon ID per stage table (indexed by $22)
victory_weapon_id_table:  .byte   $02,$04,$01,$03,$05,$00,$02,$04 ; Ndl→$04(Mag) Mag→$01(Gem) Gem→$03(Hrd) Hrd→$05(Top)
LDD0C:  .byte   $00,$00,$00,$00,$00
        asl     $06
        asl     current_weapon
        brk
        jsr     move_vertical_gravity
        bcc     code_DD25               ; airborne → skip standing check
        lda     #$01                    ; set standing OAM ($01) when grounded
        cmp     ent_anim_id
        beq     code_DD25               ; already set → skip
        jsr     reset_sprite_anim

; --- render shutter nametable columns ---
code_DD25:  ldy     #$26                ; set viewport ($52 = $26)
        cpy     $52
        beq     code_DD38               ; already set → check progress
        sty     $52                     ; first time: init progressive render
        ldy     #$00
        sty     LA000
        sty     $70                     ; progress counter = 0
        sty     $28                     ; column counter = 0
        beq     code_DD3C
code_DD38:  ldy     $70                 ; render complete?
        beq     code_DD52               ; yes → wait for entities
code_DD3C:  lda     #$11                ; switch to bank $11 (shutter graphics)
        sta     prg_bank
        jsr     select_PRG_banks
        lda     #$04                    ; metatile column $04 (shutter/door column)
        jsr     metatile_column_ptr_by_id
        lda     #$04                    ; nametable select = $04
        sta     $10
        jsr     fill_nametable_progressive ; draw one column per frame
        ldx     #$00                    ; restore X = player slot
        rts

; --- wait for shutter entities (slots $08-$17) to clear ---

code_DD52:  ldy     #$0F                ; check 16 entity slots
code_DD54:  lda     $0308,y             ; slot still active?
        bmi     code_DDA5               ; yes → wait
        dey
        bpl     code_DD54               ; next slot

; --- all entities done: begin Y scroll reveal ---
        lda     #$0B                    ; set game mode $0B (split-screen mode)
        cmp     game_mode                     ; already set?
        beq     code_DD72               ; yes → skip init
        sta     game_mode                     ; $F8 = game mode $0B
        lda     camera_x_hi                     ; set nametable bit 0
        ora     #$01
        sta     camera_x_hi
        lda     #$50                    ; $FA = $50 (starting Y scroll offset)
        sta     scroll_y
        lda     #$52                    ; $5E = $52 (IRQ scanline count)
        sta     $5E

; --- scroll Y down by 3 per frame ---
code_DD72:  lda     scroll_y                 ; $FA -= 3
        sec
        sbc     #$03
        sta     scroll_y
        bcs     code_DD81               ; no underflow → continue
        lda     #$00                    ; underflow: clamp to 0
        sta     scroll_y                     ; $FA = 0 (scroll complete)
        sta     player_state                     ; $30 = state $00 (return to normal)

; --- adjust shutter entity Y positions during scroll ---
code_DD81:  ldy     #$03                ; 4 shutter entities (slots $1C-$1F)
code_DD83:  lda     shutter_base_y_table,y ; base Y position from table
        sec                             ; subtract current scroll offset
        sbc     scroll_y
        bcc     code_DD96               ; off screen (negative) → skip
        sta     $03DC,y                 ; y_pixel for slot $1C+Y
        lda     $059C,y                 ; clear bit 2 of sprite flags
        and     #$FB                    ; (make entity visible)
        sta     $059C,y
code_DD96:  dey                         ; next entity
        bpl     code_DD83
        lda     player_state                     ; scroll done ($30 = 0)?
        beq     code_DDA5               ; yes → return
        lda     $059E                   ; set bit 2 on slot $1E (keep one shutter visible
        ora     #$04                    ; during transition for visual continuity)
        sta     $059E
code_DDA5:  .byte   $60

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
        lda     ent_anim_state                   ; wait for teleport-in anim to finish
        cmp     #$04                    ; $04 = animation complete
        bne     code_DDA5               ; not done → wait (returns via earlier RTS)
        ldy     warp_dest                     ; Y = boss/warp index
        lda     warp_screen_table,y     ; check screen number
        bpl     code_DDC1               ; positive → valid boss arena

; --- negative screen = Wily stage clear (no more bosses) ---
        sta     $74                     ; mark fortress stage cleared ($FF)
        inc     $75                     ; advance fortress progression
        lda     #$9C                    ; refill player health
        sta     player_hp
        rts

; --- set up boss refight arena ---

code_DDC1:  lda     #$00                ; clear rendering disabled flag
        sta     nmi_skip
        jsr     fade_palette_in         ; fade screen in
        lda     #$04                    ; $97 = $04 (OAM overlay mode for boss arena)
        sta     oam_ptr
        jsr     prepare_oam_buffer      ; refresh OAM
        jsr     task_yield              ; yield to NMI (update screen)
        jsr     clear_entity_table      ; clear all entity slots
        lda     #$01                    ; switch to bank $01 (CHR/palette data)
        sta     prg_bank
        jsr     select_PRG_banks
        ldy     warp_dest                     ; if $6C = 0 (first warp): clear weapon bitmask
        bne     code_DDE2
        sty     $6E                     ; $6E = 0

; --- load boss position from tables ---
code_DDE2:  lda     warp_screen_table,y ; camera screen = boss screen number
        sta     camera_screen
        sta     ent_x_scr                   ; player x_screen = boss screen
        sta     $2B                     ; room index = boss screen
        sta     $29                     ; column render position
        lda     #$20                    ; $2A = $20 (scroll flags: h-scroll enabled)
        sta     $2A
        lda     LDE72,y                 ; player x_pixel = boss X position
        sta     ent_x_px
        lda     LDE86,y                 ; player y_pixel = boss Y position
        sta     ent_y_px
        lda     LDE9A,y                 ; $9E/$9F = boss animation/state value
        sta     $9E                     ; (used by boss spawn code)
        sta     $9F
        lda     warp_chr_param_table,y  ; CHR bank param → bank $01 $A000
        jsr     LA000                   ; (set_room_chr_and_palette in bank $01)
        jsr     update_CHR_banks        ; apply CHR bank changes

; --- render arena nametable (33 columns, right-to-left) ---
        lda     #$01                    ; $31 = 1 (direction flag)
        sta     player_facing
        sta     $23                     ; $23 = 1 (scroll state)
        sta     $2E                     ; $2E = 1 (scroll direction)
        dec     $29                     ; $29 -= 1 (start one screen left)
        lda     #$1F                    ; $24 = $1F (starting row)
        sta     $24
        lda     #$21                    ; 33 columns to render
code_DE1E:  pha                         ; save column counter
        lda     #$01                    ; $10 = nametable select (right)
        sta     $10
        jsr     do_render_column        ; render one nametable column
        jsr     task_yield              ; yield to NMI
        pla                             ; decrement column counter
        sec
        sbc     #$01
        bne     code_DE1E               ; more columns → loop

; --- arena rendered: finalize ---
        sta     $2C                     ; $2C/$2D/$5A = 0 (clear scroll progress)
        sta     $2D
        sta     boss_active
        lda     camera_screen                     ; screen $08 = special arena
        cmp     #$08                    ; play music $0A (boss intro)
        bne     code_DE40
        lda     #$0A                    ; music $0A = boss intro
        jsr     submit_sound_ID_D9
code_DE40:  jsr     task_yield          ; yield for one more frame
        jsr     fade_palette_out        ; fade screen out (prepare for warp-in)
        ldx     #$00                    ; set player OAM = $13 (teleport beam)
        lda     #$13
        jsr     reset_sprite_anim
        lda     #PSTATE_WARP_ANIM                    ; $30 = state $12 (warp_anim)
        sta     player_state
        rts

; ===========================================================================
; player state $12: wait for warp animation, return to $00 [confirmed]
; ===========================================================================
; Simple state: waits for teleport-in animation to complete (ent_anim_state = $04),
; then sets $30 = $00 (return to normal gameplay state).
; ---------------------------------------------------------------------------
player_warp_anim:

        lda     ent_anim_state                   ; animation complete?
        cmp     #$04                    ; $04 = teleport-in finished
        bne     code_DE5D               ; not done → wait
        lda     #$00                    ; $30 = state $00 (normal gameplay)
        .byte   $85,$30
code_DE5D:  .byte   $60

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
LDE72:  .byte   $30,$00,$00,$20,$20,$20,$20,$20
        .byte   $20,$20,$20,$00,$30,$30,$30,$70
        .byte   $90,$D0,$D0,$D0
LDE86:  .byte   $B0,$00,$00,$C0,$B0,$B0,$B0,$B0
        .byte   $B0,$A0,$B0,$00,$2C,$6C,$AC,$AC
        .byte   $AC,$2C,$6C,$AC
LDE9A:  .byte   $10,$10,$00,$1B,$1C,$1D,$1E,$1F
        .byte   $20,$21,$22,$00,$11,$11,$11,$11
        .byte   $11,$11,$11,$11

; CHR bank param (indexes bank $01 $A200/$A030 for sprite tiles + palettes)
warp_chr_param_table:  .byte   $02,$02,$00,$25,$23,$27,$24,$2A
        .byte   $26,$28,$29,$00,$02,$02,$02,$02
        .byte   $02
LDEBF:  .byte   $02,$02,$02

; boss_defeated_bitmask: indexed by stage ($22), sets bit in $61
; $00=Needle $01=Magnet $02=Gemini $03=Hard $04=Top $05=Snake $06=Spark $07=Shadow
LDEC2:  .byte   $01,$02,$04,$08,$10,$20,$40,$80

; Doc Robot stage bitmask (4 stages: Needle=$08, Gemini=$09, Spark=$0A, Shadow=$0B)
        .byte   $01
        .byte   $04
        rti

        .byte   $80
decrease_ammo:  lda     current_weapon
        cmp     #WPN_SNAKE                    ; if current weapon < 6
        bcc     decrease_ammo_tick                   ; or is even
        and     #$01                    ; excludes Rush weapons
        bne     LDF1A
decrease_ammo_tick:  ldy     current_weapon                     ; increment number of frames/shots
        inc     $B5                     ; for current weapon, if it has
        lda     $B5                     ; not yet reached the ammo decrease
        cmp     weapon_framerate,y      ; threshold, return
        bne     LDF1A
        lda     #$00                    ; upon reaching threshold, reset
        sta     $B5                     ; frame/shot counter
        lda     player_hp,y
        and     #$1F                    ; fetch ammo value to decrease by
        sec                             ; and decrease ammo
        sbc     weapon_cost,y
        bcs     LDEF4                   ; clamp to minimum zero
        lda     #$00                    ; (no negatives)
LDEF4:  ora     #$80                    ; flag "own weapon" back on
        sta     player_hp,y                   ; store new ammo value
        cmp     #$80                    ; if not zero, return
        bne     LDF1A

; ammo has been depleted, clean up rush
        cpy     #$0B                    ; are we rush jet? disable him
        beq     LDF15
        cpy     #$09                    ; if we're rush marine
        bne     LDF1A                   ; there's more to do:
        lda     #$02
        sta     $EB
        jsr     update_CHR_banks        ; reset mega man graphics,
        lda     #$01                    ; animation, ID & state
        jsr     reset_sprite_anim
        lda     #$00
        sta     player_state
LDF15:  lda     #$00                    ; set rush sprite to inactive
        .byte   $8D,$01,$03
LDF1A:  .byte   $60

; how much ammo each weapon costs upon use
weapon_cost:  .byte   $00,$02,$01,$02,$02,$00,$01,$03 ; Mega Buster
        .byte   $01,$01,$01,$01         ; Spark Shock

; the rate or number of frames of use before decreasing
; most weapons are 1 shot == 1 frame
; jet & marine count while using them
weapon_framerate:  .byte   $00,$01,$04,$01,$01,$00,$02 ; Mega Buster
        ora     ($01,x)                 ; Rush Coil
        asl     $1E02,x                 ; Rush Marine

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
        lda     ent_anim_id                   ; already in teleport OAM $13?
        cmp     #$13
        beq     code_DF51               ; yes → phase 2 (rising)

; --- phase 1: falling with $99 ---
        ldy     #$00                    ; move down with $99 (slot 0)
        jsr     move_vertical_gravity   ; carry set = landed
        bcc     code_DF89               ; still falling → return
        lda     #$13                    ; landed: set teleport beam OAM
        jsr     reset_sprite_anim
        inc     ent_anim_state                   ; ent_anim_state = 1 (landing timer, counts down in anim)
        lda     #$00                    ; clear Y speed (stopped on ground)
        sta     ent_yvel_sub
        sta     ent_yvel

; --- phase 2: rising off-screen ---
code_DF51:  lda     ent_anim_state               ; ent_anim_state nonzero = still in landing delay
        bne     code_DF89               ; wait for anim to clear it → return
        sta     ent_anim_frame                   ; reset anim frame counter
        jsr     move_sprite_up          ; move player upward by Y speed
        lda     ent_y_scr                   ; Y screen != 0 → off-screen
        bne     code_DF73               ; → trigger ending
        lda     ent_yvel_sub                   ; accelerate: Y speed += $99 (sub)
        clc                             ; (gradual speed increase each frame)
        adc     gravity
        sta     ent_yvel_sub
        lda     ent_yvel
        adc     #$00
        sta     ent_yvel
        rts

; --- player has risen off-screen: trigger Wily gate ending ---

code_DF73:  lda     #$00                ; reset player state
        sta     player_state
        lda     #$80                    ; $74 = $80 → trigger stage clear path
        sta     $74
        lda     #$FF                    ; $60 = $FF → all Doc Robots beaten
        sta     stage_select_page
        ldy     #$0B                    ; fill all 12 ammo slots to $9C (full)
        lda     #$9C                    ; (reward for completing Doc Robot stages)
code_DF83:  sta     player_hp,y
        dey
        bpl     code_DF83
code_DF89:  rts

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
        jsr     move_vertical_gravity   ; apply $99 + vertical collision
        php                             ; save carry (1=landed)
        ror     $0F                     ; rotate carry into $0F bit 7
        plp                             ; restore flags
        bcs     LDFA2                   ; landed → walk anim
        lda     #$07                    ; airborne: set jump OAM ($07)
        cmp     ent_anim_id                   ; already set?
        beq     LDFAC                   ; yes → skip
        jsr     reset_sprite_anim
        jmp     LDFAC

LDFA2:  lda     #$04                    ; on ground: set walk OAM ($04)
        cmp     ent_anim_id                   ; already set?
        beq     LDFAC                   ; yes → skip
        jsr     reset_sprite_anim
LDFAC:  lda     ent_status                   ; check sub-phase (lower nibble of type)
        and     #$0F                    ; phase 0 = walk to X=$50
        bne     aw_wait_timer           ; phase 1+ → wait/continue
        lda     ent_x_px                   ; player X pixel position
        cmp     #$50                    ; reached target?
        beq     aw_wait_timer           ; yes → stop and wait
        bcs     LDFC2                   ; past target → walk left
        inc     ent_x_px                   ; walk right toward X=$50
        jmp     LDFC5

LDFC2:  dec     ent_x_px                   ; walk left toward X=$50
LDFC5:  lda     ent_x_px                   ; just arrived at X=$50?
        cmp     #$50
        bne     aw_ret                  ; not yet → return
        lda     $031F                   ; slot $1F already active?
        bmi     aw_wait_timer           ; yes (bit 7 set) → just wait
        lda     #$80                    ; $031F = type $80 (Break Man entity)
        sta     $031F
        lda     #$90                    ; $059F = flags: active + bit 4
        sta     $059F
        lda     #$6D                    ; $05DF = OAM ID $6D (Break Man sprite)
        sta     $05DF
        lda     #$00                    ; clear slot $1F fields:
        sta     $05FF                   ; $05FF = 0 (field ent_anim_frame+$1F)
        sta     $05BF                   ; $05BF = 0 (field ent_anim_state+$1F)
        sta     $03FF                   ; $03FF = y_screen = 0
        sta     $03DF                   ; $03DF = y_pixel = 0
        sta     ent_var1                   ; ent_var1 = player ai_timer = 0 (reused as phase flag)
        lda     camera_screen                     ; $039F = x_screen = camera screen
        sta     $039F
        lda     #$C0                    ; $037F = x_pixel = $C0 (right side)
        sta     $037F
        lda     #$EE                    ; $033F = routine = $EE (Break Man AI state)
        sta     $033F
auto_walk_spawn_done:  .byte   $A9      ; initial wait = $5A frames (90)
LE001:  .byte   $5A
        sta     ent_timer                   ; player ai_timer = 90
        lda     ent_flags                   ; face right (set bit 6)
        ora     #$40
        sta     ent_flags
        lda     #$78                    ; override: wait = $78 frames (120)
        sta     ent_timer                   ; (overwrites the $5A above)
        inc     ent_status                   ; advance to phase 1 (type $80→$81? no, lower nibble 0→1)
aw_wait_timer:  lda     ent_timer           ; timer active?
        beq     aw_timer_done           ; 0 → done waiting
        dec     ent_timer                   ; decrement timer
        lda     #$01                    ; set stand OAM ($01)
        jsr     reset_sprite_anim
aw_ret:  rts

aw_timer_done:  lda     ent_status           ; sub-phase (lower nibble)
        and     #$0F
        cmp     #$02                    ; phase 2 = post-jump (landed)
        beq     LE068
        lda     ent_x_px                   ; reached X=$A0?
        cmp     #$A0
        beq     LE05A                   ; yes → initiate jump
        lda     #$04                    ; set walk OAM ($04)
        cmp     ent_anim_id                   ; already set?
        beq     LE03D                   ; yes → skip
        jsr     reset_sprite_anim
LE03D:  inc     ent_x_px                   ; walk right 1px/frame
        lda     ent_x_px                   ; just arrived at X=$A0?
        cmp     #$A0
        bne     LE08B                   ; no → return
        lda     #$6E                    ; $05DF = slot $1F OAM $6E (Break Man teleport)
        sta     $05DF
        lda     #$00                    ; clear slot $1F fields
        sta     $05FF
        sta     $05BF
        lda     #$3C                    ; wait $3C frames (60) before jump
        sta     ent_timer
        rts

LE05A:  lda     #$E5                    ; y_speed_sub = $E5
        sta     ent_yvel_sub
        lda     #$04                    ; y_speed = $04 (jump vel = $04.E5 upward)
        sta     ent_yvel
        inc     ent_status                   ; advance to phase 2
        rts

LE068:  lda     $0F                     ; bit 7 = on ground? (from ROR carry)
        bpl     LE079                   ; not landed → keep falling
        lda     ent_var1                   ; ent_var1 = Break Man defeated flag
        bne     LE07D                   ; nonzero → done, teleport out
        inc     ent_var1                   ; first landing: set flag, wait $78 frames
        lda     #$78
        sta     ent_timer
LE079:  dec     ent_x_px                   ; drift left 1px/frame while airborne
        rts

LE07D:  lda     #$81                    ; player type = $81 (inactive marker)
        sta     ent_status
        lda     #$00                    ; clear timer
        sta     ent_timer
        lda     #PSTATE_TELEPORT                    ; player state = $0D (teleport)
        sta     player_state
LE08B:  rts

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
        jsr     move_vertical_gravity   ; apply $99
        bcs     LE097                   ; landed?
        lda     #$07                    ; airborne: jump OAM ($07)
        bne     LE099                   ; (always taken)
LE097:  lda     #$04                    ; on ground: walk OAM ($04)
LE099:  cmp     ent_anim_id                   ; already set?
        beq     LE0A1                   ; yes → skip
        jsr     reset_sprite_anim
LE0A1:  lda     ent_x_px                   ; reached X=$68?
        cmp     #$68
        beq     LE0B2                   ; yes → stop and spawn explosions
        bcs     LE0AE                   ; past target → walk left
        inc     ent_x_px                   ; walk right
        rts

LE0AE:  dec     ent_x_px                   ; walk left
        rts

LE0B2:  lda     #$01                    ; set stand OAM ($01)
        cmp     ent_anim_id                   ; already set?
        beq     LE0BC                   ; yes → skip
        jsr     reset_sprite_anim
LE0BC:  inc     ent_timer                   ; increment frame counter
        bne     LE119                   ; not wrapped → wait
        lda     #$C0                    ; reset counter to $C0 (192 frames between spawns)
        sta     ent_timer
        lda     ent_var1                   ; all 4 explosions spawned?
        cmp     #$04
        beq     LE119                   ; yes → done
        jsr     find_enemy_freeslot_y   ; find free entity slot
        bcs     LE119                   ; none free → skip
        lda     #$80                    ; type = $80 (generic cutscene entity)
        sta     ent_status,y
        lda     #$90                    ; flags = active + bit 4
        sta     ent_flags,y
        lda     #$00                    ; clear position/velocity fields:
        sta     ent_y_px,y                 ; y_pixel = 0
        sta     ent_y_scr,y                 ; y_screen = 0
        sta     ent_anim_frame,y                 ; field ent_anim_frame = 0
        sta     ent_anim_state,y                 ; field ent_anim_state = 0
        sta     ent_hitbox,y                 ; shape = 0 (no hitbox)
        sta     ent_yvel_sub,y                 ; y_speed_sub = 0
        sta     ent_yvel,y                 ; y_speed = 0
        lda     #$7C                    ; OAM = $7C (boss explosion anim)
        sta     ent_anim_id,y
        lda     #$F9                    ; routine = $F9 (explosion AI)
        sta     ent_routine,y
        lda     #$C4                    ; ai_timer = $C4 (countdown)
        sta     ent_var1,y
        lda     ent_var1                   ; player ent_var1 = explosion index
        sta     ent_timer,y                 ; copy to explosion's timer field
        tax
        inc     ent_var1                   ; advance to next explosion
        lda     auto_walk_2_x_table,x   ; x_pixel = auto_walk_2_x_table[index]
        sta     ent_x_px,y
        lda     camera_screen                     ; x_screen = camera screen
        sta     ent_x_scr,y
        ldx     #$00                    ; restore X = player slot 0
LE119:  rts

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
        sta     $030E
        lda     #$90                    ; $058E = flags: active + bit 4
        sta     $058E
        lda     #$EB                    ; $032E = routine $EB (weapon orb AI)
        sta     $032E
        lda     #$67                    ; $05CE = OAM $67 (weapon orb sprite)
        sta     $05CE
        lda     #$00                    ; clear fields:
        sta     $05EE                   ; field ent_anim_frame+$0E = 0
        sta     $05AE                   ; field ent_anim_state+$0E = 0
        sta     $03EE                   ; y_screen = 0
        sta     $B3                     ; $B3 = 0 (weapon orb energy bar inactive)
        lda     camera_screen                     ; $038E = x_screen = camera screen
        sta     $038E
        ldy     warp_dest                     ; Y = boss/weapon index
        lda     LDE72,y                 ; $036E = x_pixel from table (per-boss X)
        sta     $036E
        lda     LDE86,y                 ; $03CE = y_pixel from table (upper nibble only)
        and     #$F0
        sta     $03CE
        lda     warp_dest                     ; $04CE = stage_enemy_id = boss index + $18
        clc                             ; (prevents respawn tracking conflict)
        adc     #$18
        sta     $04CE
        lda     LDEBF,y                 ; set boss-defeated bit in weapon bitmask $6E
        ora     $6E
        sta     $6E
        lda     #$0C                    ; $EC = $0C (trigger CHR bank update)
        sta     $EC
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
;   $F9 = camera screen position (absolute, increments each 256-pixel boundary)
;   $FC = camera fine X (sub-screen pixel offset, 0-255)
;   $25 = previous $FC (for column rendering delta)
;   $27 = previous player X (for direction detection)
;   ent_x_px = player X pixel position (slot 0)
;
; Camera tracking:
;   $00 = player screen X (= ent_x_px - $FC)
;   $01 = scroll speed (= |player screen X - $80|, clamped to max 8)
;   $02 = player movement delta (= |ent_x_px - $27|)
;   When player is right of center ($00 > $80): scroll right
;   When player is left of center ($00 < $80): scroll left
;   Scroll speed adapts — faster when player is far from center,
;   uses movement delta as alternative speed when it's smaller
; ---------------------------------------------------------------------------
update_camera:  lda     $2A             ; check scroll flags
        and     #$20                    ; bit 5 = horizontal scroll enabled
        bne     LE173                   ; enabled → compute scroll
        jmp     LE228                   ; disabled → boundary clamp only

; --- horizontal scroll: compute player screen position and scroll speed ---

LE173:  lda     #$01                    ; default: scroll direction = right
        sta     $10                     ; $10 = nametable select (1=right)
        sta     $2E                     ; $2E = scroll direction flag
        lda     ent_x_px                   ; $00 = player screen X
        sec                             ; = player pixel X - camera fine X
        sbc     camera_x_lo
        sta     L0000
        sec                             ; $01 = |player screen X - $80
        sbc     #$80                    ; = distance from screen center
        bcs     LE18A                   ; (used as scroll speed)
        eor     #$FF                    ; negate if negative
        adc     #$01
LE18A:  sta     $01                     ; $01 = abs distance from center
        lda     ent_x_px                   ; $02 = player X - previous X ($27)
        sec                             ; = movement delta this frame
        sbc     $27
        sta     $02
        bpl     LE1E1                   ; positive → player moved right

; --- player moving left: negate delta, compute leftward scroll speed ---
        eor     #$FF                    ; negate to get abs delta
        clc
        adc     #$01
        sta     $02
        lda     $01                     ; if center distance >= 9,
        cmp     #$09                    ; use movement delta instead
        bcc     LE1A7                   ; (prevents jerky scroll when
        lda     $02                     ; player is far off-center but
        sta     $01                     ; moving slowly)
LE1A7:  lda     #$08                    ; clamp scroll speed to max 8
        cmp     $01
        bcs     LE1AF
        sta     $01                     ; $01 = min(8, $01)

; --- scroll camera left ---
LE1AF:  lda     #$02                    ; direction = left
        sta     $10                     ; $10 = 2 (nametable select)
        sta     $2E                     ; $2E = 2 (scroll direction)
        lda     L0000                   ; if player screen X >= $80 (right of center)
        cmp     #$80                    ; don't scroll left — go to boundary check
        bcs     LE228
        lda     camera_x_lo                     ; $FC -= scroll speed
        sec                             ; (move camera left)
        sbc     $01
        sta     camera_x_lo
        bcs     LE224                   ; no underflow → just render
        lda     $2D                     ; $FC underflowed: crossed screen boundary
        dec     $2D                     ; decrement room scroll progress
        bpl     LE1DC                   ; still in room? advance screen
        sta     $2D                     ; $2D went negative: at left boundary
        lda     #$00                    ; clamp $FC = 0 (can't scroll further)
        sta     camera_x_lo
        lda     #$10                    ; clamp player X to minimum $10
        cmp     ent_x_px                   ; (16 pixels from left edge)
        bcc     LE228
        sta     ent_x_px
        bcs     LE228
LE1DC:  dec     camera_screen                     ; camera screen position--
        jmp     LE224

; --- scroll camera right ---

LE1E1:  lda     L0000                   ; if player screen X < $81 (at/left of center)
        cmp     #$81                    ; don't scroll right
        bcc     LE228
        lda     $2D                     ; if scroll progress == screen count
        cmp     $2C                     ; already at right boundary
        beq     LE228                   ; → don't scroll
        lda     $01                     ; if center distance >= 9,
        cmp     #$09                    ; use movement delta as speed
        bcc     LE1F7
        lda     $02
        sta     $01
LE1F7:  lda     #$08                    ; clamp scroll speed to max 8
        cmp     $01
        bcs     LE1FF
        sta     $01                     ; $01 = min(8, $01)
LE1FF:  lda     $01                     ; speed = 0? nothing to do
        beq     LE228
        lda     camera_x_lo                     ; $FC += scroll speed
        clc                             ; (move camera right)
        adc     $01
        sta     camera_x_lo
        bcc     LE224                   ; no overflow → just render
        inc     $2D                     ; $FC overflowed: crossed screen boundary
        lda     $2D                     ; increment room scroll progress
        cmp     $2C
        bne     LE222                   ; not at boundary? advance screen
        lda     #$00                    ; at right boundary: clamp $FC = 0
        sta     camera_x_lo
        lda     #$F0                    ; clamp player X to maximum $F0
        cmp     ent_x_px                   ; (240 pixels from left edge)
        bcs     LE222
        sta     ent_x_px
LE222:  inc     camera_screen                     ; camera screen position++
LE224:  jmp     render_scroll_column    ; → column rendering

LE227:  rts

; ===========================================================================
; No-scroll: boundary clamping + room transition detection
; ===========================================================================
; When horizontal scrolling is disabled or camera is between screens,
; clamp player X to screen bounds and check if player has walked to
; the edge of the screen (triggering a room transition via ladder/door).
; ---------------------------------------------------------------------------

LE228:  lda     camera_x_lo                     ; if camera is mid-scroll ($FC != 0)
        bne     LE227                   ; nothing more to do
        lda     ent_x_px                   ; clamp player X >= $10 (left edge)
        cmp     #$10
        bcs     LE23D
        lda     #$10
        sta     ent_x_px
        beq     LE23D
LE23A:  jmp     check_vertical_transition

LE23D:  cmp     #$E5                    ; player X < $E5? not at right edge
        bcc     LE23A                   ; → check vertical instead
        cmp     #$F0                    ; clamp player X <= $F0
        bcc     LE24A
        lda     #$F0
        sta     ent_x_px

; --- room transition: player walked to right edge ($E5+) of non-scrolling screen ---
LE24A:  ldy     $2B                     ; current room entry
        lda     $AA40,y                 ; check if current room allows
        and     #$20                    ; horizontal scroll (bit 5)
        beq     LE23A                   ; no scroll attr → vertical check
        lda     $AA41,y                 ; next room entry: check vertical
        and     #$C0                    ; connection bits (6-7)
        bne     LE23A                   ; has vertical link → not a horiz transition
        lda     $AA41,y                 ; next room: must also have horiz scroll
        and     #$20                    ; (bit 5 set) to allow transition
        beq     LE23A
        sta     L0000                   ; $00 = $20 (next room scroll flags)
        lda     stage_id                     ; stage-specific transition blocks:
        cmp     #STAGE_DOC_NEEDLE                    ; stage $08 (Doc Robot Needle)
        bne     LE27A                   ; skip if not stage $08
        lda     camera_screen                     ; Rush Marine water boundary screens
        cmp     #$15                    ; $15 and $1A have special gate
        beq     LE273
        cmp     #$1A
        bne     LE27A
LE273:  lda     $033F                   ; entity slot $1F type == $FC?
        cmp     #$FC                    ; (gate/shutter entity present
        beq     LE23A                   ; → block transition until opened)
LE27A:  lda     camera_screen                     ; screens from room start:
        sec                             ; $F9 - $AA30 (room base screen)
        sbc     $AA30
        cmp     #$02                    ; if exactly 2 screens in:
        bne     LE28F                   ; (boss shutter position)
        lda     $031F                   ; entity slot $1F active (bit 7 clear)?
        bmi     LE23A                   ; active → boss shutter blocks transition
        lda     player_state                     ; player state >= $0C (victory)?
        cmp     #PSTATE_VICTORY                    ; block if in cutscene state
        bcs     LE23A
LE28F:  lda     stage_id                     ; stage $0F (Wily Fortress 4)
        cmp     #STAGE_WILY4                    ; special check
        bne     LE2A5
        lda     camera_screen                     ; only on screen $08
        cmp     #$08
        bne     LE2A5
        ldx     #$0F                    ; scan entity slots $10-$01
LE29D:  lda     $0310,x                 ; if any slot $0310+x is active
        bmi     LE23A                   ; (bit 7 = ???), block transition
        dex
        bpl     LE29D

; --- advance to next room: set up new room variables ---
LE2A5:  lda     L0000                   ; $2A = next room's scroll flags
        sta     $2A
        lda     $AA41,y                 ; $2C = next room's screen count
        and     #$1F                    ; (lower 5 bits)
        sta     $2C
        lda     #$00                    ; $2D = 0 (start of new room)
        sta     $2D
        inc     $2B                     ; room index++
        ldy     game_mode                     ; save $F8 (scroll render mode)
        lda     #$00                    ; then clear transition state:
        sta     game_mode                     ; $F8 = 0 (normal rendering)
        sta     $76                     ; $76 = 0 (enemy spawn flag)
        sta     $B3                     ; $B3 = 0 (?)
        sta     boss_active                     ; $5A = 0 (?)
        lda     #$E8                    ; $5E = $E8 (despawn boundary Y)
        sta     $5E
        cpy     #$02                    ; was $F8 == 2? (camera scroll mode)
        bne     LE2D5                   ; no → skip
        lda     #$42                    ; special scroll init:
        sta     $E9                     ; $E9 = $42 (CHR bank?)
        lda     #$09                    ; $29 = $09 (metatile column)
        sta     $29
        jsr     load_room               ; load room layout + CHR/palette
LE2D5:  lda     camera_screen                     ; screen offset from room base
        sec                             ; = $F9 - $AA30
        sbc     $AA30
        bcc     LE2F7                   ; negative → no event
        cmp     #$03                    ; < 3 screens: use table 1 ($AA31)
        bcs     LE2E8
        tax                             ; Y = event ID from $AA31+offset
        ldy     $AA31,x
        jmp     LE2F4

LE2E8:  lda     camera_screen                     ; >= 3 screens: use table 2 ($AA39)
        sec                             ; offset from $AA38
        sbc     $AA38
        bcc     LE2F7
        tax
        ldy     $AA39,x
LE2F4:  jsr     call_bank10_8000        ; call bank $10 event handler
LE2F7:  lda     stage_id                     ; stage-specific: Needle Man ($00)
        bne     LE308                   ; skip if not Needle Man
        ldx     #$03                    ; copy 4 bytes from $AAA2 → $060C
LE2FD:  lda     $AAA2,x
        sta     $060C,x
        dex
        bpl     LE2FD
        stx     palette_dirty                     ; $18 = $FF
LE308:  lda     #$E4                    ; player X = $E4 (entering from left)
        sta     ent_x_px                   ; — wait, $E4 is right side
        jsr     fast_scroll_right       ; clear entities + fast-scroll camera
        lda     stage_id                     ; re-select stage data bank
        sta     prg_bank
        jsr     select_PRG_banks
        lda     camera_screen                     ; second event dispatch (post-scroll)
        sec                             ; same table lookup pattern
        sbc     $AA30
        bcc     scroll_engine_rts
        cmp     #$05                    ; < 5: use $AA30 table
        bcs     LE32A
        tax
        ldy     $AA30,x
        jmp     LE338

LE32A:  lda     camera_screen                     ; >= 5: use $AA38 table
        sec
        sbc     $AA38
        bcc     scroll_engine_rts
        beq     scroll_engine_rts
        tax
        ldy     $AA38,x
LE338:  jsr     call_bank10_8003        ; call bank $10 post-scroll handler
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
        bcs     LE35E
        cmp     #$09                    ; < $09? → at top of screen
        bcs     scroll_engine_rts       ; $09-$E7 = normal range, RTS
        lda     player_state                     ; must be climbing (state $03)
        cmp     #PSTATE_LADDER                    ; to transition upward
        bne     scroll_engine_rts
        lda     #$80                    ; $10 = $80 (upward direction)
        sta     $10
        jsr     check_room_link         ; validate room connection
        bcc     scroll_engine_rts       ; C=0 → no valid link
        lda     #$80                    ; confirmed: transition up
        sta     $10
        lda     #$08                    ; $23 = $08 (vertical scroll up)
        bne     LE372
LE35E:  lda     ent_y_scr                   ; player Y screen: bit 7 set?
        bmi     LE3C6                   ; negative = death pit → RTS
        lda     #$40                    ; $10 = $40 (downward direction)
        sta     $10
        jsr     check_room_link         ; validate room connection
        bcc     LE3C6                   ; C=0 → no valid link (death)
        lda     #$40                    ; confirmed: transition down
        sta     $10
        lda     #$04                    ; $23 = $04 (vertical scroll down)

; --- begin vertical room transition ---
LE372:  sta     $23                     ; $23 = scroll direction ($04/$08)
        lda     #$00                    ; reset Y screen to 0
        sta     ent_y_scr
        ldx     #$01                    ; $12 = direction-dependent flag
        ldy     $2B                     ; check if current room's vertical
        lda     $AA40,y                 ; connection matches $10
        and     #$C0
        cmp     $10
        beq     LE388
        ldx     #$FF                    ; mismatch → $12 = $FF (reverse)
LE388:  stx     $12
        lda     #$00                    ; clear Y sub-pixel
        sta     ent_y_sub
        sta     game_mode                     ; $F8 = 0 (normal render mode)
        lda     #$E8
        sta     $5E                     ; $5E = $E8 (despawn boundary)
        jsr     clear_destroyed_blocks  ; reset breakable blocks
        jsr     vertical_scroll_animate ; animate vertical scroll
        lda     stage_id                     ; re-select stage data bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldy     $2B                     ; update $2A from new room's scroll flags
        lda     $AA40,y
        and     #$20                    ; (bit 5 = horizontal scroll)
        beq     LE3AD
        sta     $2A
LE3AD:  lda     #$01                    ; enable MMC3 RAM protect
        sta     LA000
        lda     #$2A                    ; $52 = $2A (timer/delay value?)
        sta     $52
        jsr     load_room               ; load room layout + CHR/palette
        ldx     #$00                    ; check player tile collision at (0,4)
        ldy     #$04                    ; to snap player into new room
        jsr     check_tile_collision
        lda     $10                     ; if tile collision result = 0 (air)
        bne     LE3C6                   ; set player state to $00 (on_ground)
        sta     player_state                     ; (landing after vertical transition)
LE3C6:  rts

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
        beq     LE3D1                   ; $2D == 0 (start) → check link
        cmp     $2C                     ; $2D == $2C (end) → check link
        beq     LE3D1
        clc                             ; midway through room → no transition
        rts

; --- navigate $AA40 room table to find adjacent room in direction $10 ---

LE3D1:  lda     camera_screen                     ; $00 = target screen position
        sta     L0000                   ; $00 = current screen position
        ldy     $2B                     ; Y = current room index
        lda     $2A                     ; check current room's vertical bits
        and     #$C0                    ; bits 6-7 of $2A
        beq     LE3E5                   ; no vertical → check neighbors
        lda     $2A                     ; current room has vertical link:
        cmp     $10                     ; does direction match?
        beq     LE403                   ; same direction → look forward
        bne     LE411                   ; different → look backward
LE3E5:  lda     $2C                     ; multi-screen room ($2C > 0)?
        bne     LE3FF                   ; → check scroll position
        lda     $AA41,y                 ; next entry's vertical bits
        and     #$C0                    ; match direction?
        cmp     $10
        beq     LE403                   ; yes → look forward
        lda     $AA3F,y                 ; previous entry's vertical bits
        and     #$C0                    ; (inverted: $C0 XOR = flip up/down)
        eor     #$C0
        cmp     $10
        beq     LE411
        bne     LE465                   ; no match → fail
LE3FF:  lda     $2D                     ; $2D == 0 (at start) → look backward
        beq     LE411                   ; else (at end) → look forward
LE403:  iny                             ; check next room entry
        lda     $AA40,y                 ; vertical bits must match $10
        and     #$C0
        cmp     $10
        bne     LE465                   ; no match → fail
        inc     L0000                   ; target screen = $F9 + 1
        bne     LE430
LE411:  lda     $AA40,y                 ; current entry must have vert bits
        and     #$C0
        beq     LE465                   ; no vertical → fail
        dey                             ; move to previous room entry
        bmi     LE465                   ; Y < 0 → no previous room
        lda     $AA40,y                 ; check prev room's vertical bits
        and     #$C0
        bne     LE425                   ; has vert → use it directly
        lda     $AA41,y                 ; no vert bits → check next entry
LE425:  eor     #$C0                    ; invert and compare with direction
        cmp     $10                     ; (loop safety: BEQ loops if match,
        beq     LE425                   ; but this shouldn't infinite-loop)
        dec     L0000                   ; target screen = $F9 - 1
        lda     $AA40,y

; --- link found: set up new room variables ---
LE430:  sta     $2A                     ; $2A = new room's scroll flags
        lda     #$01                    ; $2E = 1 (forward direction)
        sta     $2E
        cpy     $2B                     ; if new room index < current
        sty     $2B                     ; (going backward in table)
        bcs     LE440
        lda     #$02                    ; $2E = 2 (backward direction)
        sta     $2E
LE440:  lda     $AA40,y                 ; $2C = new room's screen count
        and     #$1F
        sta     $2C
        ldx     L0000                   ; if target screen >= current $F9:
        cpx     camera_screen                     ; $2D = 0 (at start of new room)
        bcc     LE44F                   ; else: $2D = $2C (at end)
        lda     #$00
LE44F:  sta     $2D
        lda     L0000                   ; update camera and player X screen
        sta     $29                     ; $29 = metatile column base
        sta     camera_screen                     ; $F9 = camera screen
        sta     ent_x_scr                   ; ent_x_scr = player X screen
        lda     #$00                    ; $A000 = 0 (MMC3 bank config)
        sta     LA000
        lda     #$26                    ; $52 = $26 (viewport height for V-mirror)
        sta     $52
        sec                             ; C=1 → link found
        rts

LE465:  clc                             ; C=0 → no valid link
        rts

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
; $25 = previous $FC value, $FC = current fine scroll position
; $1A = flag to trigger NMI buffer drain
; ---------------------------------------------------------------------------

render_scroll_column:  lda     game_mode      ; if $F8 == 2: dual nametable mode
        cmp     #$02
        bne     LE4AF                   ; not dual → single column
        jsr     LE4AF                   ; render first nametable column
        bcs     LE4EF                   ; C=1 → no column to render
        lda     $0780                   ; mirror to second nametable:
        ora     #$22                    ; set bit 1 of high byte ($20→$22)
        sta     $0780                   ; and bit 7 of low byte
        lda     $0781
        ora     #$80
        sta     $0781
        lda     #$09                    ; count = 9 (copy 10 tile rows)
        sta     $0782
        ldy     #$00                    ; copy attribute data to second
LE489:  lda     $0797,y
        sta     $0783,y
        iny
        cpy     #$0A
        bne     LE489
        lda     $07A1                   ; check if attribute entry exists
        bpl     LE49F                   ; bit 7 set = end marker
        sta     $078D                   ; no second attr: terminate here
        sta     nt_column_dirty                     ; flag NMI to drain buffer
        rts

LE49F:  ldy     #$00                    ; copy second attribute section
LE4A1:  lda     $07B5,y
        sta     $078D,y
        iny
        cpy     #$0D
        bne     LE4A1
        sta     nt_column_dirty                     ; flag NMI to drain buffer
        rts

; --- single_column: compute if a column crossed an 8-pixel boundary ---
; $03 = |$FC - $25| = absolute scroll delta since last frame
; If delta + fine position crosses an 8-pixel tile boundary, we need
; to render a new column. $24 = nametable column pointer, $29 = metatile
; column. Direction tables at $E5C3/$E5CD/$E5CF configure left vs right.

LE4AF:  lda     camera_x_lo                     ; $03 = |$FC - $25
        sec                             ; absolute scroll delta
        sbc     $25
        bpl     LE4BB
        eor     #$FF                    ; negate if negative
        clc
        adc     #$01
LE4BB:  sta     $03                     ; $03 = scroll delta (pixels)
        beq     LE4EF                   ; zero → nothing to render
        lda     $23                     ; if $23 has vertical scroll bits
        and     #$0C                    ; ($04 or $08), this is first frame
        beq     LE4D6                   ; after vertical transition → init
        lda     $10                     ; overwrite $23 with current direction
        sta     $23
        and     #$01                    ; X = direction index (0=left, 1=right)
        tax
        lda     LE5CD,x                 ; init $25 from direction table
        sta     $25
        lda     LE5CF,x                 ; init $24 from direction table
        sta     $24
LE4D6:  lda     $10                     ; get fine position for boundary check:
        and     #$01                    ; direction 1 (right): use $25 as-is
        beq     LE4E1                   ; direction 2 (left): invert $25
        lda     $25
        jmp     LE4E5

LE4E1:  lda     $25                     ; invert for leftward scroll
        eor     #$FF
LE4E5:  and     #$07                    ; (fine pos & 7) + delta
        clc                             ; if result / 8 > 0: crossed boundary
        adc     $03                     ; → need to render new column
        lsr     a
        lsr     a
        lsr     a
        bne     do_render_column
LE4EF:  sec                             ; C=1 → no column to render
        rts

; --- render column: advance nametable pointer and build PPU update ---

do_render_column:  lda     $10          ; X = direction (0 or 1)
        pha
        and     #$01
        tax
        pla                             ; if direction changed since last frame
        cmp     $23                     ; ($10 != $23): skip column advance,
        sta     $23                     ; just update metatile base
        beq     LE501
        jmp     LE50F

LE501:  lda     $24                     ; advance nametable column pointer
        clc                             ; $24 += direction step ($E5C3,x)
        adc     LE5C3,x
        cmp     #$20                    ; wrap at 32 columns (NES nametable)
        and     #$1F
        sta     $24
        bcc     LE517                   ; no wrap → skip metatile base update
LE50F:  lda     $29                     ; $29 += direction step
        clc                             ; (metatile column in level data)
        adc     LE5C3,x
        sta     $29

; --- build nametable column from metatile data ---
LE517:  lda     stage_id                     ; select stage data bank
        sta     prg_bank
        jsr     select_PRG_banks
        lda     $24                     ; $28 = attribute table row
        lsr     a                       ; = nametable column / 4
        lsr     a
        sta     $28
        ldy     $29                     ; set up metatile data pointer
        jsr     metatile_column_ptr     ; for column $29
        lda     #$00                    ; $03 = buffer write offset
        sta     $03                     ; (increments by 4 per row)
LE52D:  jsr     metatile_to_chr_tiles   ; decode metatile → $06C0 tile buffer
        ldy     $28                     ; $11 = current attribute byte
        lda     $0640,y                 ; from attribute cache
        sta     $11
        lda     $24                     ; Y = column within metatile (0-3)
        and     #$03
        tay
        ldx     $03                     ; write 2 or 4 CHR tiles to buffer
        lda     $06C0,y                 ; top-left tile
        sta     $0783,x
        lda     $06C4,y                 ; bottom-left tile
        sta     $0784,x
        lda     $28                     ; if row >= $38 (bottom 2 rows):
        cmp     #$38                    ; skip second pair (status bar area)
        bcs     LE55C
        lda     $06C8,y                 ; top-right tile
        sta     $0785,x
        lda     $06CC,y                 ; bottom-right tile
        sta     $0786,x
LE55C:  lda     $24                     ; if column is odd: update attribute byte
        and     #$01                    ; (attributes cover 2×2 metatile groups)
        beq     LE588
        lda     $10                     ; merge new attribute bits:
        and     LE5C5,y                 ; mask with direction table
        sta     $10
        lda     $11                     ; combine with old attribute
        and     LE5C9,y
        ora     $10
        sta     $07A4,x                 ; store in attribute buffer
        ldy     $28                     ; update attribute cache
        sta     $0640,y
        lda     #$23                    ; attribute table PPU address:
        sta     $07A1,x                 ; $23C0 + row offset
        tya
        ora     #$C0
        sta     $07A2,x
        lda     #$00                    ; count = 0 (single byte)
        sta     $07A3,x
LE588:  inc     $03                     ; advance buffer offset by 4
        inc     $03
        inc     $03
        inc     $03
        lda     $28                     ; advance attribute row by 8
        clc                             ; (each metatile = 8 pixel rows)
        adc     #$08
        sta     $28
        cmp     #$40                    ; < $40 (8 rows)? loop
        bcc     LE52D
        lda     #$20                    ; finalize PPU buffer header:
        sta     $0780                   ; $0780 = $20 (nametable $2000 base)
        lda     $24                     ; $0781 = nametable column
        sta     $0781
        lda     #$1D                    ; $0782 = $1D (30 tiles = full column)
        sta     $0782
        ldy     #$00                    ; select attribute buffer terminator pos:
        lda     $24                     ; odd column → Y=$20 (secondary buffer)
        and     #$01                    ; even column → Y=$00 (primary buffer)
        beq     LE5B4
        ldy     #$20
LE5B4:  lda     #$FF                    ; write $FF end marker
        sta     $07A1,y
        ldy     game_mode                     ; if $F8 == 2 (dual nametable):
        cpy     #$02                    ; don't flag NMI yet (caller handles it)
        beq     LE5C1
        sta     nt_column_dirty                     ; else: flag NMI to drain buffer
LE5C1:  clc                             ; C=0 → column was rendered
        rts

; Direction/column rendering tables:
; $E5C3: direction step (+1/-1 for right/left)
; $E5C5: attribute mask tables (4 bytes each)
; $E5CD: initial $25 values per direction
; $E5CF: initial $24 values per direction

LE5C3:  .byte   $FF,$01
LE5C5:  .byte   $33,$33,$CC,$CC
LE5C9:  .byte   $CC,$CC,$33,$33
LE5CD:  .byte   $00,$FF
LE5CF:  .byte   $01,$1F

; ===========================================================================
; fast_scroll_right — rapid camera scroll during room transition
; ===========================================================================
; Called during horizontal room advance. Scrolls camera right at 4 pixels
; per frame while simultaneously advancing player X by ~$D0/$100 per frame
; (net rightward movement). Renders columns each frame and yields.
; Loops until $FC wraps back to 0 (full screen scrolled).
; ---------------------------------------------------------------------------
fast_scroll_right:  jsr     clear_entity_table ; clear all enemies
LE5D4:  lda     camera_x_lo                     ; $FC += 4 (scroll 4 pixels/frame)
        clc
        adc     #$04
        sta     camera_x_lo
        bcc     LE5DF                   ; carry → screen boundary crossed
        inc     camera_screen                     ; $F9++ (camera screen)
LE5DF:  lda     #$01                    ; $10 = 1 (scroll right)
        sta     $10
        jsr     render_scroll_column    ; render column for new position
        lda     camera_x_lo                     ; update $25 = previous $FC
        sta     $25
        lda     ent_x_sub                   ; player X sub += $D0
        clc                             ; (24-bit add: sub + pixel + screen)
        adc     #$D0                    ; net effect: player slides right
        sta     ent_x_sub                   ; slightly faster than camera
        lda     ent_x_px
        adc     #$00
        sta     ent_x_px
        lda     ent_x_scr
        adc     #$00
        sta     ent_x_scr
        jsr     process_frame_yield_with_player ; render frame + yield to NMI
        lda     camera_x_lo                     ; loop until $FC wraps to 0
        bne     LE5D4                   ; (full 256-pixel screen scrolled)
        lda     stage_id                     ; re-select stage bank
        sta     prg_bank
        jsr     select_PRG_banks
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
        lsr     a
        tax
        lda     LE7EB,x                 ; $24 = initial row from table
        sta     $24
LE623:  lda     $23                     ; bit 2 set = scrolling down
        and     #$04
        beq     LE653

; --- scroll down: camera moves down, player moves up ---
        lda     scroll_y                     ; $FA += 3 (fine Y scroll advances)
        clc
        adc     #$03
        sta     scroll_y
        cmp     #$F0                    ; wrap at $F0 (NES screen height)
        bcc     LE638
        adc     #$0F                    ; skip $F0-$FF range
        sta     scroll_y
LE638:  lda     ent_y_sub                   ; player Y sub -= $C0
        sec                             ; (player moves up ~2.75 px/frame)
        sbc     #$C0
        sta     ent_y_sub
        lda     ent_y_px                   ; player Y pixel -= 2 + borrow
        sbc     #$02
        sta     ent_y_px
        bcs     LE67A
        sbc     #$0F                    ; wrap at screen boundary
        sta     ent_y_px
        jmp     LE67A

; --- scroll up: camera moves up, player moves down ---

LE653:  lda     scroll_y                     ; $FA -= 3 (fine Y scroll retreats)
        sec
        sbc     #$03
        sta     scroll_y
        bcs     LE660
        sbc     #$0F                    ; wrap below $00
        sta     scroll_y
LE660:  lda     ent_y_sub                   ; player Y sub += $C0
        clc                             ; (player moves down ~2.75 px/frame)
        adc     #$C0
        sta     ent_y_sub
        lda     ent_y_px                   ; player Y pixel += 2 + carry
        adc     #$02
        sta     ent_y_px
        cmp     #$F0                    ; wrap at $F0
        bcc     LE67A
        adc     #$0F
        sta     ent_y_px
LE67A:  jsr     render_vert_row         ; render row for new vertical position
        lda     scroll_y                     ; $26 = previous $FA (for delta)
        sta     $26
        lda     $12                     ; preserve $12 across frame yield
        pha
        jsr     process_frame_yield_with_player ; render frame + yield to NMI
        pla
        sta     $12
        lda     scroll_y                     ; loop until $FA == 0 (full screen)
        beq     LE691
        jmp     LE623

LE691:  lda     stage_id                     ; re-select stage bank
        sta     prg_bank
        jmp     select_PRG_banks

; ===========================================================================
; render_vert_row — queue nametable row update during vertical scroll
; ===========================================================================
; Vertical equivalent of render_scroll_column. Computes whether an 8-pixel
; row boundary was crossed (|$FA - $26| + fine position), and if so,
; builds the PPU update buffer for the new row of metatiles.
;
; $FA = current vertical fine scroll, $26 = previous $FA
; $23 bit 2: direction (set=down, clear=up)
; $24 = nametable row pointer
; ---------------------------------------------------------------------------

render_vert_row:  lda     scroll_y           ; $03 = |$FA - $26
        sec                             ; = absolute vertical scroll delta
        sbc     $26
        bpl     LE6A4
        eor     #$FF                    ; negate if negative
        clc
        adc     #$01
LE6A4:  sta     $03                     ; $03 = scroll delta (pixels)
        beq     LE6C1                   ; zero → nothing to render
        lda     $23                     ; get fine position for boundary check
        and     #$04                    ; down: use $26 as-is
        beq     LE6B3                   ; up: invert $26
        lda     $26
        jmp     LE6B7

LE6B3:  lda     $26                     ; invert for upward scroll
        eor     #$FF
LE6B7:  and     #$07                    ; (fine pos & 7) + delta
        clc                             ; if result / 8 > 0: crossed boundary
        adc     $03                     ; → need to render new row
        lsr     a
        lsr     a
        lsr     a
        bne     LE6C2
LE6C1:  rts

; --- render row: advance row pointer and build PPU update buffer ---
; $24 = nametable row index (0-$1D, wraps), $23 bit 2 = direction (1=down, 0=up)
; PPU buffer: $0780 = header, $0783 = top half tiles, $07AF = bottom half tiles
; $07A3-$07A5 = attribute table PPU address + byte count
; $07A6-$07AD = 8 attribute bytes for this row
; $0640 = attribute cache (8 bytes per row block, updated incrementally)

LE6C2:  lda     $23                     ; Y = direction flag (0=up, 1=down)
        and     #$04                    ; bit 2 → shift to bit 0
        lsr     a
        lsr     a
        tay
        lda     $24                     ; advance row pointer:
        clc                             ; down: $24 += $01 (from vert_row_advance_tbl)
        adc     vert_row_advance_tbl,y  ; up: $24 += $FF (i.e. $24 -= 1)
        sta     $24
        cmp     #$1E                    ; row < 30? (NES nametable = 30 rows)
        bcc     LE6DA                   ; yes → skip wrap
        lda     vert_row_wrap_tbl,y     ; wrap: down wraps to $00, up wraps to $1D
        sta     $24
LE6DA:  lda     $24                     ; check if row is on correct nametable half
        and     #$01                    ; (even/odd parity vs direction)
        cmp     vert_row_parity_tbl,y   ; down expects $01, up expects $00
        beq     LE6E6                   ; match → need to render fresh row
        jmp     LE79F                   ; mismatch → just shift existing buffer

LE6E6:  lda     stage_id                     ; switch to stage data bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldy     $29                     ; set up metatile column pointer for screen $29
        jsr     metatile_column_ptr
        lda     $24                     ; $28 = row-within-block offset × 2
        and     #$1C                    ; (rows come in groups of 4 within metatile)
        asl     a
        sta     $28
        ora     #$C0                    ; PPU addr for attribute table row = $23C0 + offset
        sta     $07A4                   ; store low byte of attr PPU addr
        lda     #$23                    ; high byte = $23 (nametable 0 attr table)
        sta     $07A3
        lda     #$07                    ; 8 attribute bytes to write
        sta     $07A5
        lda     #$00                    ; $03 = PPU buffer write offset (starts at 0)
        sta     $03
LE70C:  ldy     $28                     ; $11 = previous attribute byte from cache
        lda     $0640,y
        sta     $11
        jsr     metatile_to_chr_tiles   ; decode metatile → CHR tiles + attr in $10
        ldy     $03                     ; Y = buffer write offset
        lda     $24                     ; X = row sub-position (0-3 within metatile)
        and     #$03
        tax
        lda     vert_chr_top_offsets,x  ; $04 = CHR buffer offset for top row
        sta     $04
        lda     vert_chr_bot_offsets,x  ; $05 = CHR buffer offset for bottom row
        sta     $05
        lda     #$03                    ; $06 = 4 tiles per metatile column (count-1)
        sta     $06
LE72B:  ldx     $04                     ; top half: $06C0[top offset] → $0783 buffer
        lda     $06C0,x
        sta     $0783,y
        ldx     $05                     ; bottom half: $06C0[bot offset] → $07AF buffer
        lda     $06C0,x
        sta     $07AF,y
        inc     $04                     ; advance offsets
        inc     $05
        iny                             ; advance buffer write position
        dec     $06                     ; 4 tiles done?
        bpl     LE72B
        sty     $03                     ; save buffer position
        lda     $24                     ; X = row sub-position (0-3)
        and     #$03
        tax
        lda     $10                     ; keep new attr bits (mask from vert_attr_keep_tbl)
        and     vert_attr_keep_tbl,x
        sta     $10
        lda     $11                     ; merge old attr bits (mask from vert_attr_old_tbl)
        and     vert_attr_old_tbl,x
        ora     $10
        sta     $10                     ; $10 = merged attribute byte
        ldx     $28                     ; update attribute cache
        sta     $0640,x
        txa                             ; X = column index (0-7)
        and     #$07
        tax
        lda     $10                     ; store attribute to PPU buffer
        sta     $07A6,x
        inc     $28                     ; next column
        cpx     #$07                    ; done all 8?
        bne     LE70C
        lda     $24                     ; compute nametable PPU address for this row
        pha
        and     #$03                    ; Y = sub-row (0-3)
        tay
        pla
        lsr     a                       ; X = row/4 (metatile row index)
        lsr     a
        tax
        lda     LE8A9,x                 ; $0780 = PPU addr high byte (from row table)
        sta     $0780
        lda     LE8A1,x                 ; $0781 = PPU addr low byte | nametable offset
        ora     vert_row_nt_offsets,y   ; (sub-row adds $00/$20/$40/$60)
        sta     $0781
        lda     #$1F                    ; $0782 = tile count ($1F = 32 tiles per row)
        sta     $0782
        lda     $23                     ; Y = direction (0=up, 1=down)
        and     #$04
        lsr     a
        lsr     a
        tay
        ldx     vert_term_offset_tbl,y  ; terminator offset from vert_term_offset_tbl
        lda     #$FF                    ; $FF = end-of-buffer marker
        sta     $0780,x                 ; place at appropriate end
        sta     nametable_dirty                     ; $19 = flag: PPU update pending
        rts

LE79F:  ldy     #$1F                    ; copy 32 bytes: $07AF → $0783
LE7A1:  lda     $07AF,y
        sta     $0783,y
        dey
        bpl     LE7A1
        lda     $24                     ; update PPU addr low byte
        and     #$03                    ; with new sub-row nametable offset
        tax
        lda     $0781                   ; keep high bit (nametable select)
        and     #$80
        ora     vert_row_nt_offsets,x
        sta     $0781
        lda     #$23                    ; attribute table high byte
        sta     $07A3
        lda     $23                     ; set terminator based on direction
        and     #$04
        lsr     a
        lsr     a
        tay
        ldx     vert_term_shift_tbl,y   ; terminator offset (shift variant)
        lda     #$FF
        sta     $0780,x
        sta     nametable_dirty                     ; PPU update pending
        rts

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
LE7EB:  .byte   $00,$1D
vert_term_offset_tbl:  .byte   $23,$2E  ; terminator offset: up=$23, down=$2E
vert_term_shift_tbl:  .byte   $2E,$23   ; shift terminator: up=$2E, down=$23

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
metatile_to_chr_tiles:  jsr     metatile_chr_ptr
metatile_to_chr_tiles_continue:  ldy     #$03 ; start with quadrant 3
        sty     $02                     ; $02 = quadrant counter (3→0)
        lda     #$00
        sta     $10                     ; $10 = accumulated attribute bits
LE7FC:  ldy     $02                     ; Y = current quadrant
        ldx     LE89D,y                 ; X = buffer offset for this quadrant
        lda     (L0000),y               ; read metatile sub-index
        tay                             ; Y = sub-index for CHR lookup
        lda     $BB00,y                 ; top-left tile
        sta     $06C0,x
        lda     $BC00,y                 ; top-right tile
        sta     $06C1,x
        lda     $BD00,y                 ; bottom-left tile
        sta     $06C4,x
        lda     $BE00,y                 ; bottom-right tile
        sta     $06C5,x
        jsr     breakable_block_override ; Gemini Man: zero destroyed blocks
        lda     $BF00,y                 ; attribute: low 2 bits = palette
        and     #$03                    ; merge into $10
        ora     $10
        sta     $10
        dec     $02                     ; next quadrant
        bmi     metatile_chr_exit       ; if all 4 done, return
        asl     $10                     ; shift attribute bits left 2
        asl     $10                     ; (make room for next quadrant)
        jmp     LE7FC

metatile_chr_exit:  rts

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

breakable_block_override:  lda     stage_id  ; current stage number
        cmp     #STAGE_GEMINI                    ; Gemini Man?
        beq     LE83E                   ; yes → check
        cmp     #STAGE_DOC_GEMINI                    ; Doc Robot (Gemini Man stage)?
        bne     metatile_chr_exit       ; no → skip (shared RTS above)
LE83E:  lda     $BF00,y                 ; attribute byte upper nibble
        and     #$F0
        cmp     #$70                    ; breakable block type ($7x)?
        bne     metatile_chr_exit       ; no → skip
        sty     $0F                     ; save Y (metatile sub-index)
        stx     $0E                     ; save X (buffer offset)
        lda     $29                     ; compute destroyed-block array index:
        and     #$01                    ; Y = ($29 & 1) << 5 | ($28 >> 1)
        asl     a                       ; ($29 bit 0 = screen page,
        asl     a                       ; $28 = metatile column position)
        asl     a
        asl     a
        asl     a
        sta     $0D
        lda     $28
        pha                             ; save $28
        lsr     a                       ; Y = byte index into $0110 array
        ora     $0D
        tay
        pla                             ; X = bit index within byte:
        asl     a                       ; ($28 << 2) & $04 | $02 (quadrant)
        asl     a                       ; combines column parity with quadrant
        and     #$04
        ora     $02
        tax
        lda     $0110,y                 ; test destroyed bit
        and     bitmask_table,x         ; via bitmask_table
        beq     LE87D                   ; not destroyed → skip
        ldx     $0E                     ; restore buffer offset
        lda     #$00                    ; zero all 4 CHR tiles (invisible)
        sta     $06C0,x
        sta     $06C1,x
        sta     $06C4,x
        sta     $06C5,x
LE87D:  ldy     $0F                     ; restore Y
        ldx     $0E                     ; restore X
        rts

; metatile_chr_ptr: sets $00/$01 pointer to 4-byte metatile CHR definition
; reads metatile index from column data at ($20),y
; each metatile = 4 CHR tile indices (2x2 pattern: TL, TR, BL, BR)
; metatile definitions at $B700 + (metatile_index * 4) in stage bank

metatile_chr_ptr:  jsr     ensure_stage_bank ; ensure stage bank selected
        lda     #$00
        sta     $01
        ldy     $28
        lda     ($20),y                 ; metatile index from column data
calc_chr_offset:  asl     a             ; multiply by 4
        rol     $01                     ; (4 CHR tiles per metatile)
        asl     a
        rol     $01
        sta     L0000                   ; $00/$01 = $B700 + (index * 4)
        lda     $01                     ; pointer to CHR tile definition
        clc
        adc     #$B7
        sta     $01
        rts

LE89D:  .byte   $00,$02,$08,$0A
LE8A1:  .byte   $00,$80,$00,$80,$00,$80,$00,$80
LE8A9:  .byte   $20,$20,$21,$21,$22,$22,$23,$23

; metatile_column_ptr: sets $20/$21 pointer to metatile column data
; Y = screen page → reads column ID from $AA00,y in stage bank
; column data is at $AF00 + (column_ID * 64) — 64 bytes per column
metatile_column_ptr:  lda     $AA00,y   ; column ID from screen data
metatile_column_ptr_by_id:  pha         ; multiply column ID by 64:
        lda     #$00                    ; A << 6 → $00:A (16-bit result)
        sta     L0000                   ; 6 shifts with 16-bit rotate
        pla
        asl     a                       ; shift 1
        rol     L0000
        asl     a                       ; shift 2
        rol     L0000
        asl     a                       ; shift 3
        rol     L0000
        asl     a                       ; shift 4
        rol     L0000
        asl     a                       ; shift 5
        rol     L0000
        asl     a                       ; shift 6
        rol     L0000
        sta     $20                     ; $20/$21 = $AF00 + (column_ID × 64)
        lda     L0000                   ; pointer to metatile column data
        clc                             ; in stage bank at $AF00+
        adc     #$AF
        sta     $21
        rts

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
;   $41     = max (highest priority) tile type encountered
;   $10     = OR of all tile types (AND #$10 tests solid)
; -----------------------------------------------

check_tile_horiz:  lda     LEBE2,y      ; $40 = starting offset index
        sta     $40
        jsr     tile_check_init         ; clear accumulators, switch to stage bank
        tay                             ; Y = hitbox config index
        lda     LEC10,y                 ; $02 = number of check points - 1
        sta     $02
        lda     ent_y_scr,x                 ; $03 = entity Y screen
        sta     $03
        lda     LEC11,y                 ; $11 = entity_Y + Y_offset
        pha                             ; (compute check Y position)
        clc
        adc     ent_y_px,x
        sta     $11
        pla                             ; A = original Y offset (for sign check)
        bmi     LE900                   ; negative offset? handle differently
        bcs     LE902                   ; positive offset + carry = past screen
        lda     $11                     ; check if Y wrapped past $F0
        cmp     #$F0                    ; (screen height)
        bcs     LE902
        bcc     LE90F                   ; within bounds → proceed
LE900:  bcs     LE90F                   ; neg offset + carry = no underflow → ok
LE902:  lda     #$00                    ; Y out of bounds: zero all results
        ldy     $02
LE906:  sta     $42,y                   ; clear $42..$42+count
        dey
        bpl     LE906
        jmp     tile_check_cleanup      ; restore bank and return

LE90F:  lda     $03                     ; Y screen != 0? treat as offscreen
        bne     LE902                   ; (only check screen 0)
        lda     $11                     ; Y >> 2 = 4-pixel rows
        lsr     a
        lsr     a
        pha                             ; $28 = metatile row (bits 5-3 of Y>>2)
        and     #$38                    ; = column offset in metatile grid
        sta     $28
        pla                             ; $03 bit1 = sub-tile Y (bit 2 of Y>>2)
        lsr     a                       ; selects top/bottom half of metatile
        and     #$02
        sta     $03
        lda     #$00                    ; $04 = sign extension for X offset
        sta     $04
        lda     LEC12,y                 ; first X offset (signed)
        bpl     LE92D                   ; positive? skip sign extend
        dec     $04                     ; negative: $04 = $FF
LE92D:  clc                             ; $12/$13 = entity_X + X_offset
        adc     ent_x_px,x                 ; $12 = X pixel
        sta     $12                     ; $13 = X screen
        lda     ent_x_scr,x
        adc     $04
        sta     $13
        lda     $12                     ; X >> 4 = tile column
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        pha                             ; $03 bit0 = sub-tile X (bit 0 of X>>4)
        and     #$01                    ; selects left/right half of metatile
        ora     $03                     ; (combined with Y sub-tile in bits 0-1)
        sta     $03
        pla                             ; $28 |= metatile column (X>>5)
        lsr     a                       ; merged with row from Y
        ora     $28
        sta     $28
LE94D:  stx     $04                     ; save entity slot
        lda     stage_id                     ; switch to stage PRG bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $04                     ; restore entity slot
        ldy     $13                     ; get metatile column pointer for X screen
        jsr     metatile_column_ptr
LE95D:  jsr     metatile_chr_ptr        ; get CHR data pointer for column+row ($28)
LE960:  ldy     $03                     ; read metatile index at sub-tile ($03)
        lda     (L0000),y               ; $03 = {0,1,2,3} for TL/TR/BL/BR
        tay
        lda     $BF00,y                 ; get collision attribute for this tile
        and     #$F0                    ; upper nibble = collision type
        jsr     breakable_block_collision ; override if destroyed breakable block
        jsr     proto_man_wall_override ; Proto Man wall override
        ldy     $02                     ; store result in $42+count
        sta     $42,y
        cmp     tile_at_feet_max                     ; update max tile type
        bcc     LE97B
        sta     tile_at_feet_max
LE97B:  ora     $10                     ; accumulate all tile types
        sta     $10
        dec     $02                     ; all check points done?
        bmi     LE9BA
        inc     $40                     ; next offset index
        ldy     $40
        lda     $12                     ; save old bit4 of X (16px tile boundary)
        pha
        and     #$10
        sta     $04
        pla                             ; $12 += next X offset
        clc
        adc     LEC12,y
        sta     $12
        and     #$10                    ; did bit4 change? (crossed 16px tile?)
        cmp     $04
        beq     LE960                   ; no → same tile, just re-read
        lda     $03                     ; toggle sub-tile X (bit 0)
        eor     #$01
        sta     $03
        and     #$01                    ; if now odd → just toggled into right half
        bne     LE960                   ; same metatile, re-read sub-tile
        inc     $28                     ; advance metatile column index
        lda     $28
        and     #$07                    ; crossed column group? (8 columns)
        bne     LE95D                   ; no → same screen, new column
        inc     $13                     ; next X screen
        dec     $28                     ; wrap column index back
        lda     $28                     ; keep row bits, clear column
        and     #$38
        sta     $28
        jmp     LE94D                   ; re-load metatile data for new screen

LE9BA:  cpx     #$00                    ; only check damage for player (slot 0)
        bne     LE9E0
        lda     invincibility_timer                     ; skip if invincibility active
        bne     LE9E0
        lda     hazard_pending                     ; skip if damage already pending
        bne     LE9E0
        lda     player_state                     ; skip if player in damage state ($06)
        cmp     #PSTATE_DAMAGE
        beq     LE9E0
        cmp     #PSTATE_DEATH                    ; skip if player in death state ($0E)
        beq     LE9E0
        ldy     #$06                    ; Y = damage state
        lda     tile_at_feet_max                     ; $30 = damage tile?
        cmp     #TILE_DAMAGE
        beq     LE9DE                   ; → set $3D = $06
        ldy     #$0E                    ; Y = death state
        cmp     #TILE_SPIKES                    ; $50 = spike tile?
        bne     LE9E0                   ; no hazard → skip
LE9DE:  sty     hazard_pending                     ; set pending damage/death transition
LE9E0:  jmp     tile_check_cleanup      ; restore bank and return

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
;   $41     = max (highest priority) tile type encountered
;   $10     = OR of all tile types (AND #$10 tests solid)
; -----------------------------------------------

check_tile_collision:  lda     LECE1,y  ; $40 = starting offset index
        sta     $40
        jsr     tile_check_init         ; clear accumulators, switch to stage bank
        tay                             ; Y = hitbox config index
        lda     LED07,y                 ; $02 = number of check points - 1
        sta     $02
        lda     #$00                    ; $04 = sign extension for X offset
        sta     $04
        lda     LED08,y                 ; X offset (signed)
        bpl     LE9FC
        dec     $04                     ; negative: $04 = $FF
LE9FC:  clc                             ; $12/$13 = entity_X + X_offset
        adc     ent_x_px,x                 ; $12 = X pixel
        sta     $12                     ; $13 = X screen
        lda     ent_x_scr,x
        adc     $04
        sta     $13
        lda     $12                     ; X >> 4 = tile column
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        pha                             ; $03 bit0 = sub-tile X (left/right)
        and     #$01
        sta     $03
        pla                             ; $28 = metatile column (X>>5)
        lsr     a
        sta     $28
        lda     ent_y_scr,x                 ; Y screen
        bmi     LEA41                   ; negative screen → clamp to 0
        bne     LEA3B                   ; screen > 0 → clamp to $EF
        lda     ent_y_px,x                 ; $11 = entity_Y + first Y offset
        clc
        adc     LED09,y
        sta     $11
        lda     LED09,y                 ; check offset sign
        bpl     LEA31                   ; positive offset
        bcc     LEA41                   ; neg offset + no carry = underflow
        bcs     LEA33                   ; neg offset + carry = ok, check $F0
LEA31:  bcs     LEA3B                   ; pos offset + carry = overflow
LEA33:  lda     $11                     ; within screen ($00-$EF)?
        cmp     #$F0
        bcc     LEA6C                   ; yes → proceed to tile lookup
        bcs     LEA31                   ; wrapped past $F0 → below screen
LEA3B:  lda     #$EF                    ; clamp to bottom of screen
        sta     $11
        bne     LEA6C
LEA41:  lda     #$00                    ; clamp to top of screen
        sta     $11
        beq     LEA6C
LEA47:  lda     $11                     ; $11 += next Y offset
        clc
        adc     LED09,y
        sta     $11
        cmp     #$F0                    ; crossed screen boundary?
        bcc     LEA5B                   ; no → skip this point
        adc     #$10                    ; wrap into next screen
        sta     $11
        inc     $04                     ; Y screen++
        beq     LEA6C                   ; reached screen 0 → start checking
LEA5B:  iny                             ; advance offset index
        sty     $40
        ldy     $02                     ; zero this check point's result
        lda     #$00
        sta     $42,y
        dec     $02                     ; more points? continue skipping
        bpl     LEA47
        jmp     tile_check_cleanup      ; all offscreen → done

LEA6C:  lda     $11                     ; Y >> 2 = 4-pixel rows
        lsr     a
        lsr     a
        pha                             ; $28 |= metatile row (bits 5-3)
        and     #$38                    ; merged with column from X
        ora     $28
        sta     $28
        pla                             ; $03 bit1 = sub-tile Y (top/bottom)
        lsr     a
        and     #$02
        ora     $03
        sta     $03
        stx     $04                     ; switch to stage PRG bank
        lda     stage_id
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $04                     ; restore entity slot
        ldy     $13                     ; get metatile column pointer for X screen
        jsr     metatile_column_ptr
LEA8F:  jsr     metatile_chr_ptr        ; get CHR data pointer for column+row ($28)
LEA92:  ldy     $03                     ; read metatile index at sub-tile ($03)
        lda     (L0000),y
        tay
        lda     $BF00,y                 ; get collision attribute
        and     #$F0                    ; upper nibble = collision type
        jsr     breakable_block_collision ; override if destroyed breakable block
        jsr     proto_man_wall_override ; Proto Man wall override
        ldy     $02                     ; store result in $42+count
        sta     $42,y
        cmp     tile_at_feet_max                     ; update max tile type
        bcc     LEAAD
        sta     tile_at_feet_max
LEAAD:  ora     $10                     ; accumulate all tile types
        sta     $10
        dec     $02                     ; all check points done?
        bmi     LEAE9
        inc     $40                     ; next offset index
        ldy     $40
        lda     $11                     ; save old bit4 of Y (16px tile boundary)
        pha
        and     #$10
        sta     $04
        pla                             ; $11 += next Y offset
        clc
        adc     LED09,y
        sta     $11
        and     #$10                    ; did bit4 change? (crossed 16px tile?)
        cmp     $04
        beq     LEA92                   ; no → same tile, just re-read
        lda     $03                     ; toggle sub-tile Y (bit 1)
        eor     #$02
        sta     $03
        and     #$02                    ; if now set → toggled into bottom half
        bne     LEA92                   ; same metatile, re-read sub-tile
        lda     $28                     ; advance metatile row ($28 += $08)
        pha
        clc
        adc     #$08
        sta     $28
        cmp     #$40                    ; past screen bottom? ($40 = 8 rows)
        pla
        bcc     LEA8F                   ; no → same screen, new row
        sta     $28                     ; restore row, check last sub-tile
        jmp     LEA92                   ; (clamp — don't scroll to next screen)

LEAE9:  cpx     #$00                    ; only check damage for player (slot 0)
        bne     LEB09
        lda     invincibility_timer                     ; skip if invincibility active
        bne     LEB09
        lda     hazard_pending                     ; skip if damage already pending
        bne     LEB09
        lda     player_state                     ; skip if player in damage state ($06)
        cmp     #PSTATE_DAMAGE
        beq     LEB09
        cmp     #PSTATE_DEATH                    ; skip if player in death state ($0E)
        beq     LEB09
        lda     tile_at_feet_max                     ; $50 = spike tile → instant kill
        cmp     #TILE_SPIKES
        bne     LEB09
        lda     #PSTATE_DEATH                    ; set pending death transition
        sta     hazard_pending
LEB09:  jmp     tile_check_cleanup      ; restore bank and return

; -----------------------------------------------
; tile_check_init — initialize tile collision check
; -----------------------------------------------
; Clears result accumulators ($10, $41), saves the current PRG bank
; to $2F, and switches to the stage PRG bank ($22) so the tile
; collision code can read metatile/attribute data from $A000-$BFFF.
; Preserves A and X across the bank switch.
; -----------------------------------------------

tile_check_init:  pha                   ; save A (table index)
        txa
        pha                             ; save X (entity slot)
        lda     #$00                    ; clear collision accumulators
        sta     $10                     ; $10 = OR of all tile types
        sta     tile_at_feet_max                     ; $41 = max tile type
        lda     prg_bank                     ; save current PRG bank to $2F
        sta     $2F
        lda     stage_id                     ; switch to stage PRG bank
        sta     prg_bank
        jsr     select_PRG_banks
        pla                             ; restore X and A
        tax
        pla
        rts

; -----------------------------------------------
; tile_check_cleanup — restore PRG bank after tile collision
; -----------------------------------------------
; Restores the PRG bank saved in $2F by tile_check_init.
; Preserves X.
; -----------------------------------------------

tile_check_cleanup:  txa                ; save X
        pha
        lda     $2F                     ; restore saved PRG bank
        sta     prg_bank
        jsr     select_PRG_banks
        pla                             ; restore X
        tax
        rts

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
        bne     LEB68
        lda     stage_id                     ; only on Gemini Man stages
        cmp     #STAGE_GEMINI                    ; ($02 = Robot Master,
        beq     LEB42                   ; $09 = Doc Robot)
        cmp     #STAGE_DOC_GEMINI
        bne     LEB68
LEB42:  lda     $13                     ; compute array index (same as
        and     #$01                    ; breakable_block_override):
        asl     a                       ; Y = ($13 & 1) << 5 | ($28 >> 1)
        asl     a                       ; ($13 = screen page for collision,
        asl     a                       ; $28 = metatile column position)
        asl     a
        asl     a
        sta     $07
        lda     $28
        pha                             ; save $28
        lsr     a                       ; Y = byte index into $0110 array
        ora     $07
        tay
        pla                             ; X = bit index within byte:
        asl     a                       ; ($28 << 2) & $04 | $03 (quadrant)
        asl     a
        and     #$04
        ora     $03
        tax
        lda     $0110,y                 ; test destroyed bit
        and     bitmask_table,x
        beq     LEB68                   ; not destroyed → keep tile type
        lda     #$00                    ; destroyed → override to passthrough
        sta     $06
LEB68:  lda     $06                     ; return tile type ($00 if block destroyed)
        ldx     $05                     ; restore X
        rts

; ---------------------------------------------------------------------------
; clear_destroyed_blocks — reset the $0110 destroyed-block bitfield
; ---------------------------------------------------------------------------
; Called on stage init. Zeros all 64 bytes ($0110-$014F) on Gemini Man
; stages ($22 = $02 or $09), making all breakable blocks solid again.
; ---------------------------------------------------------------------------

clear_destroyed_blocks:  lda     stage_id    ; only on Gemini Man stages
        cmp     #STAGE_GEMINI
        beq     LEB77
        cmp     #STAGE_DOC_GEMINI
        bne     LEB81
LEB77:  lda     #$00                    ; zero $0110..$014F (64 bytes)
        ldy     #$3F                    ; = 64 block slots
        sta     $0110,y
        .byte   $88,$10,$FA
LEB81:  .byte   $60
bitmask_table:  .byte   $80,$40,$20,$10
        php
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
        stx     $06
        sty     $07
        lda     proto_man_flag                     ; $68 = cutscene-complete flag
        beq     LEBB9                   ; no cutscene → return original
        ldy     stage_id                     ; stage index
        ldx     LEBC6,y                 ; look up wall data offset
        bmi     LEBC0                   ; $FF = no wall → clear $68
        lda     LEBCE,x                 ; expected scroll position
        cmp     camera_screen                     ; match current scroll?
        bne     LEBC0                   ; no → clear $68 (wrong screen)
        lda     LEBCF,x                 ; number of wall tile entries
        sta     $08
        lda     $28                     ; current tile column
LEBA9:  cmp     LEBD0,x                 ; match wall column?
        beq     LEBB5                   ; yes → override to passthrough
        inx
        dec     $08
        bpl     LEBA9                   ; try next entry
        bmi     LEBB9                   ; no match → return original
LEBB5:  lda     #$00                    ; A = $00 (air/passthrough)
        beq     LEBBB
LEBB9:  lda     $05                     ; A = original tile type
LEBBB:  ldx     $06                     ; restore X, Y
        ldy     $07
        rts

LEBC0:  lda     #$00                    ; wrong stage/screen
        .byte   $85,$68,$F0,$F3         ; clear cutscene-complete flag
LEBC6:  .byte   $FF,$00,$11,$04,$FF,$FF,$FF,$0E
LEBCE:  .byte   $05
LEBCF:  .byte   $01
LEBD0:  .byte   $31,$39,$13,$07,$23,$24,$2B,$2C
        .byte   $33,$34,$3B,$3C,$05,$00,$31,$09
        .byte   $00,$00
LEBE2:  .byte   $00,$05,$09,$0E,$12,$16,$1A,$1F
        .byte   $24,$28,$2C,$31,$36,$3B,$40,$45
        .byte   $49,$4E,$53,$57,$5B,$5F,$63,$68
        .byte   $6C,$70,$75,$79,$7D,$81,$85,$8A
        .byte   $8F,$94,$99,$9E,$A3,$A8,$AD,$B2
        .byte   $B7,$BC,$C1,$C6,$CB,$CE
LEC10:  .byte   $02
LEC11:  .byte   $0C
LEC12:  .byte   $F9,$07,$07,$01,$F4,$F9,$0E,$02
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
LECE1:  .byte   $00,$05,$0A,$0E,$12,$17,$1C,$21
        .byte   $26,$2A,$2E,$33,$38,$3D,$42,$49
        .byte   $50,$57,$5E,$65,$6C,$71,$76,$7B
        .byte   $80,$85,$8A,$8F,$94,$99,$9E,$A2
        .byte   $A6,$AB,$B0,$B5,$BA,$BF
LED07:  .byte   $02
LED08:  .byte   $08
LED09:  .byte   $F5,$0B,$0B,$02,$F8,$F5,$0B,$0B
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
LEDB1:  .byte   $0F,$02,$F0,$F1,$0F,$0F,$02,$14
        .byte   $EC,$14,$14,$02,$EC,$EC,$14,$14
        .byte   $02,$10,$E8,$18,$18,$02
        beq     LEDB1
        clc
        clc

; -----------------------------------------------
; snap_x_to_wall_right — push entity left after hitting a wall on its right
; -----------------------------------------------
; After a rightward collision, aligns entity X to the left edge of the
; solid tile. $12 = collision check X; lower 4 bits = how far into the
; 16-pixel tile. Subtracts that offset from entity X position.
; -----------------------------------------------
snap_x_to_wall_right:  lda     $12      ; offset = $12 & $0F
        and     #$0F                    ; (sub-tile penetration depth)
        sta     $12
        lda     ent_x_px,x                 ; X pixel -= offset
        sec                             ; (push entity left)
        sbc     $12
        sta     ent_x_px,x
        lda     ent_x_scr,x                 ; X screen -= borrow
        sbc     #$00
        sta     ent_x_scr,x
        rts

; -----------------------------------------------
; snap_x_to_wall_left — push entity right after hitting a wall on its left
; -----------------------------------------------
; After a leftward collision, aligns entity X to the right edge of the
; solid tile + 1. $12 = collision check X; computes (16 - offset) and
; adds to entity X. EOR #$0F + SEC/ADC = add (16 - low_nibble).
; -----------------------------------------------

snap_x_to_wall_left:  lda     $12       ; offset = ($12 & $0F) ^ $0F
        and     #$0F                    ; = 15 - sub-tile position
        eor     #$0F
        sec                             ; X pixel += (16 - sub_tile_pos)
        adc     ent_x_px,x                 ; SEC + ADC = add offset + 1
        sta     ent_x_px,x                 ; (push entity right)
        lda     ent_x_scr,x                 ; X screen += carry
        adc     #$00
        sta     ent_x_scr,x
        rts

; -----------------------------------------------
; snap_y_to_ceil — push entity down after hitting a ceiling above
; -----------------------------------------------
; After an upward collision, aligns entity Y to the bottom of the
; solid tile + 1. Same math as snap_x_to_wall_left but for Y axis.
; Handles downward screen wrap at Y=$F0.
; -----------------------------------------------

snap_y_to_ceil:  lda     $11            ; offset = ($11 & $0F) ^ $0F
        and     #$0F
        eor     #$0F
        sec                             ; Y pixel += (16 - sub_tile_pos)
        adc     ent_y_px,x                 ; (push entity down)
        sta     ent_y_px,x
        cmp     #$F0                    ; screen height = $F0
        bcc     LEE12                   ; within screen? done
        adc     #$0F                    ; wrap to next screen down
        sta     ent_y_px,x
        inc     ent_y_scr,x                 ; Y screen++
LEE12:  rts

; -----------------------------------------------
; snap_y_to_floor — push entity up after landing on a floor below
; -----------------------------------------------
; After a downward collision, aligns entity Y to the top of the
; solid tile. $11 = collision check Y; subtracts low nibble from
; entity Y. Preserves $11 (restored from stack after adjustment).
; Handles upward screen wrap (underflow).
; -----------------------------------------------

snap_y_to_floor:  lda     $11           ; save original $11
        pha
        and     #$0F                    ; offset = $11 & $0F
        sta     $11                     ; (sub-tile penetration depth)
        lda     ent_y_px,x                 ; Y pixel -= offset
        sec                             ; (push entity up)
        sbc     $11
        sta     ent_y_px,x
        bcs     LEE2D                   ; no underflow? done
        sbc     #$0F                    ; wrap to previous screen
        sta     ent_y_px,x
        dec     ent_y_scr,x                 ; Y screen--
LEE2D:  pla                             ; restore original $11
        sta     $11
        rts

; ===========================================================================
; call_bank10_8000 / call_bank10_8003 — bank $10 trampolines
; ===========================================================================
; Save current $8000-$9FFF bank, switch to bank $10, call entry point
; ($8000 or $8003), then restore original bank. Called from player cutscene
; state machine (code_1FE2F4, code_1FE338). Bank $10 contains cutscene/
; scripted sequence routines.
; ---------------------------------------------------------------------------

call_bank10_8000:  lda     mmc3_select          ; save current $8000 bank
        pha
        lda     #$10                    ; switch $8000-$9FFF to bank $10
        sta     mmc3_select
        jsr     select_PRG_banks
        jsr     L8000                   ; call bank $10 entry point 0
        pla                             ; restore original $8000 bank
        sta     mmc3_select
        jmp     select_PRG_banks

call_bank10_8003:  lda     mmc3_select          ; save current $8000 bank
        pha
        lda     #$10                    ; switch $8000-$9FFF to bank $10
        sta     mmc3_select
        jsr     select_PRG_banks
        jsr     L8003                   ; call bank $10 entry point 1
        pla                             ; restore original $8000 bank
        sta     mmc3_select
        jmp     select_PRG_banks

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
        lda     nt_column_dirty                     ; if nametable column or row
        ora     nametable_dirty                     ; update already pending,
        bne     LEEAA                   ; return C=1 (busy)
        lda     #$08                    ; seed high byte = $08
        sta     $0780                   ; (becomes $20-$23 after shifts)
        lda     ent_y_px,x                 ; entity Y, upper nibble = metatile row
        and     #$F0                    ; ASL×2 with ROL into $0780:
        asl     a                       ; row * 64 → PPU row offset
        rol     $0780                   ; (each metatile = 2 tile rows × 32)
        asl     a
        rol     $0780
        sta     $0781                   ; low byte = row component
        lda     ent_x_px,x                 ; entity X, upper nibble
        and     #$F0                    ; LSR×3 = metatile column × 2
        lsr     a                       ; (each metatile = 2 tiles wide)
        lsr     a
        lsr     a
        ora     $0781
        sta     $0781                   ; combine row + column
        ora     #$20                    ; addr + 32 = next tile row
        sta     $0786                   ; (second entry low byte)
        lda     #$01                    ; count = 1 → write 2 tiles per row
        sta     $0782
        sta     $0787
        lda     $0780                   ; copy high byte for second entry
        sta     $0785
        lda     #$00                    ; tile data = $00 (blank) for all 4
        sta     $0783
        sta     $0784
        sta     $0788
        sta     $0789
        lda     #$FF                    ; terminator
        sta     $078A
        sta     nametable_dirty                     ; flag NMI to process buffer
        clc                             ; success
LEEAA:  rts

; ===========================================================================
; queue_metatile_update — build PPU update buffer for a 4×4 tile metatile
; ===========================================================================
; Converts metatile position ($28) to PPU nametable addresses and builds
; a 5-entry NMI update buffer at $0780: four 4-tile rows + one attribute
; byte. Tile data is sourced from $06C0-$06CF (filled by metatile_to_chr_tiles).
;
; $28 = metatile index (low 3 bits = column, upper bits = row)
; $10 = nametable select (bit 2: 0=$2000, 4=$2400)
; $68 = cutscene-complete flag (selects alternate tile lookup path)
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
        pha
        and     #$07                    ; column bits × 4 = PPU column offset
        asl     a                       ; (4 tiles wide per metatile)
        asl     a
        sta     $0781
        lda     #$02                    ; seed high byte = $02
        sta     $0780                   ; (becomes $20+ after shifts)
        pla                             ; row bits (upper 5 bits of $28)
        and     #$F8                    ; ASL×4 with ROL = row × 128
        asl     a                       ; (each metatile row = 4 tile rows
        rol     $0780                   ; × 32 bytes = 128)
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        ora     $0781
        sta     $0781                   ; row 0 addr low byte
        clc
        adc     #$20                    ; row 1 addr low = row 0 + 32
        sta     $0788
        adc     #$20                    ; row 2 addr low = row 1 + 32
        sta     $078F
        adc     #$20                    ; row 3 addr low = row 2 + 32
        sta     $0796
        lda     $28                     ; attribute addr low = $28 OR $C0
        ora     #$C0                    ; (attribute table offset)
        sta     $079D
        lda     $0780                   ; merge nametable select bit
        ora     $10                     ; into all row high bytes
        sta     $0780                   ; (rows 0-3 share same high byte)
        sta     $0787
        sta     $078E
        sta     $0795
        ora     #$03                    ; attribute high byte = $23 or $27
        sta     $079C
        lda     #$03                    ; count = 3 → 4 tiles per row
        sta     $0782
        sta     $0789
        sta     $0790
        sta     $0797
        lda     #$00                    ; attribute entry count = 0 (1 byte)
        sta     $079E
        lda     proto_man_flag                     ; cutscene-complete flag?
        beq     LEF26                   ; no → normal metatile lookup
        lda     #$00                    ; alternate path: use $11 (scroll pos)
        sta     $01                     ; for CHR tile offset calculation
        lda     $11
        jsr     calc_chr_offset
        jsr     metatile_to_chr_tiles_continue
        jmp     LEF2D

LEF26:  tya                             ; Y = metatile ID
        jsr     metatile_column_ptr_by_id ; look up metatile definition
        jsr     metatile_to_chr_tiles   ; convert to CHR tile indices
LEF2D:  ldx     #$03                    ; copy 4×4 tile data from $06C0-$06CF
LEF2F:  lda     $06C0,x                 ; row 0: $06C0-$06C3 → $0783-$0786
        sta     $0783,x
        lda     $06C4,x                 ; row 1: $06C4-$06C7 → $078A-$078D
        sta     $078A,x
        lda     $06C8,x                 ; row 2: $06C8-$06CB → $0791-$0794
        sta     $0791,x
        lda     $06CC,x                 ; row 3: $06CC-$06CF → $0798-$079B
        sta     $0798,x
        dex
        bpl     LEF2F
        lda     $10                     ; attribute byte = nametable select
        sta     $079F
        stx     $07A0                   ; terminator ($FF from DEX past 0)
        lda     $28                     ; if metatile row >= $38 (bottom edge),
        and     #$3F                    ; row 3+4 would overflow nametable.
        cmp     #$38                    ; Move attribute entry up to replace
        bcc     LEF65                   ; row 3 entry to avoid overflow.
        ldx     #$04                    ; copy attribute entry ($079C-$07A0)
LEF5C:  lda     $079C,x
        sta     $078E,x
        dex
        bpl     LEF5C
LEF65:  stx     nametable_dirty                     ; flag NMI to process buffer
        rts

; ===========================================================================
; write_attribute_table — queue attribute table write to PPU
; ===========================================================================
; Called after fill_nametable_progressive finishes all 64 metatile columns.
; Copies the 64-byte attribute buffer ($0640-$067F) to the PPU write queue,
; targeting PPU $23C0 (attribute table of nametable 0 or 1 based on $10).
; Resets $70 to 0 so the nametable fill can restart if needed.
; ---------------------------------------------------------------------------

write_attribute_table:  lda     #$00    ; reset progress counter
        sta     $70
        lda     #$23                    ; PPU address high byte: $23 or $27
        ora     $10                     ; ($10 bit 2 = nametable select)
        sta     $0780
        lda     #$C0                    ; PPU address low byte: $C0
        sta     $0781                   ; (attribute table start)
        ldy     #$3F                    ; count = 64 bytes ($3F+1)
        sty     $0782
LEF7D:  lda     $0640,y                 ; copy attribute buffer to PPU queue
        sta     $0783,y
        dey
        bpl     LEF7D
        sty     $07C3                   ; $FF terminator after data
        sty     nametable_dirty                     ; signal NMI to process queue
        rts

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
        beq     write_attribute_table
        sta     $28                     ; $28 = working column index

; --- Compute PPU base address from $70 ---
; Column ($70 & $07) × 4 → low byte offset.
; Row ($70 & $F8) << 4 → high byte bits.
        pha
        and     #$07                    ; column within row group × 4
        asl     a
        asl     a
        sta     $0781                   ; → PPU addr low byte (partial)
        lda     #$02                    ; seed high byte = $02
        sta     $0780                   ; (nametable $2000 base >> 8)
        pla
        and     #$F8                    ; row index × 128 via shift+rotate:
        asl     a                       ; ($70 & $F8) << 4 into high/low
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        ora     $0781                   ; merge low byte parts
        sta     $0781

; --- Set up 4 PPU write entries (4 tile rows) ---
; Each entry is 19 bytes apart ($13). Low bytes advance by $20 (one row).
        clc
        adc     #$20                    ; row 1 low byte
        sta     $0794
        adc     #$20                    ; row 2 low byte
        sta     $07A7
        adc     #$20                    ; row 3 low byte
        sta     $07BA
        lda     $0780                   ; high byte + nametable select ($10)
        ora     $10                     ; same for all 4 entries
        sta     $0780
        sta     $0793
        sta     $07A6
        sta     $07B9
        lda     #$0F                    ; tile count = 16 ($0F+1) per entry
        sta     $0782
        sta     $0795
        sta     $07A8
        sta     $07BB

; --- Process 4 metatile columns ---
LEFE9:  jsr     metatile_to_chr_tiles   ; convert metatile → 16 tiles in $06C0
        lda     $28                     ; which column within group? (0-3)
        and     #$03                    ; determines position within entries
        tax
        ldy     ppu_column_offsets,x    ; Y = starting offset {$03,$07,$0B,$0F}
        ldx     #$00                    ; X = source offset into $06C0
LEFF6:  lda     $06C0,x                 ; copy 4 tiles from $06C0
        sta     $0780,y                 ; to current position in PPU entry
        iny
        inx
        txa
        and     #$03                    ; (4 tiles per row)
        bne     LEFF6
        tya                             ; skip to next entry (+$0F)
        clc
        adc     #$0F
        pha
        ldy     $28                     ; store attribute byte for this column
        lda     $10
        sta     $0640,y
        pla
        tay
        cpy     #$4C                    ; loop until all 4 rows filled
        bcc     LEFF6                   ; (Y < $4C → more rows)
        inc     $70                     ; advance progress counter
        inc     $28                     ; advance working column
        lda     $28                     ; repeat for 4 columns total
        and     #$03
        bne     LEFE9
        lda     #$FF                    ; $FF terminator after 4th entry
        sta     $07CC
        ldy     $28                     ; if past row 28 ($39 columns),
        cpy     #$39                    ; disable 3rd PPU entry (rows 29-30
        bcc     LF02D                   ; would overflow nametable)
        sta     $07A6                   ; terminate 3rd entry early
LF02D:  .byte   $85,$19,$60             ; signal NMI to process PPU queue

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
; $97 = OAM buffer write index (used by sprite assembly routines)
update_entity_sprites:  inc     $95     ; advance global frame counter
        inc     $4F                     ; advance secondary counter
        lda     $95
        lsr     a                       ; even frame? forward iteration
        bcs     LF055                   ; odd? reverse
        jsr     draw_energy_bars        ; draw HUD bars first on even frames
        ldx     #$00
        stx     $96                     ; start at slot 0
LF044:  lda     ent_status,x                 ; bit 7 = entity active
        bpl     LF04C                   ; skip inactive
        jsr     process_entity_display  ; render this entity
LF04C:  inc     $96
        ldx     $96                     ; next slot
        cpx     #$20                    ; 32 slots total
        bne     LF044
        rts

LF055:  ldx     #$1F
        stx     $96                     ; start at slot 31
LF059:  lda     ent_status,x                 ; bit 7 = entity active
        bpl     LF061                   ; skip inactive
        jsr     process_entity_display  ; render this entity
LF061:  dec     $96
        ldx     $96                     ; previous slot
        bpl     LF059
        jsr     draw_energy_bars        ; draw HUD bars last on odd frames
        rts

; --- process_entity_display ---
; per-entity display handler: screen culling, death/damage, OAM assembly
; X = entity slot index (0-31)
; $FC/$F9 = camera scroll X pixel/screen
; $12/$13 = screen Y/X position for OAM drawing

process_entity_display:  lda     ent_flags,x
        and     #$10                    ; bit 4 = uses world coordinates
        beq     LF0F1                   ; (needs camera subtraction)
        lda     ent_x_px,x
        sec                             ; screen X = entity X - camera X
        sbc     camera_x_lo
        sta     $13
        lda     ent_x_scr,x                 ; entity X screen - camera screen
        sbc     camera_screen                     ; if different screen, offscreen
        bne     LF091
        lda     ent_y_scr,x                 ; Y screen = 0? → on same screen
        beq     LF0F6
        cpx     #$00                    ; player? keep visible
        beq     LF0AF
        lda     ent_y_scr,x                 ; Y screen negative? keep visible
        bmi     LF0AF                   ; Y screen positive? deactivate
        bpl     LF0A4
LF091:  lda     ent_flags,x                 ; bit 3 = wide offscreen margin
        and     #$08                    ; (keep alive within 72px of edge)
        beq     LF0A4
        lda     $13                     ; take absolute value of screen X
        bcs     LF0A0
        eor     #$FF                    ; negate if negative
        adc     #$01
LF0A0:  cmp     #$48                    ; within 72px margin?
        bcc     LF0AF                   ; yes → keep alive
LF0A4:  lda     #$00                    ; deactivate entity
        sta     ent_status,x
        lda     #$FF                    ; mark as despawned
        sta     ent_spawn_id,x
        rts

LF0AF:  lda     ent_flags,x
        and     #$7F                    ; clear bit 7 (drawn flag)
        sta     ent_flags,x
        cpx     #$00                    ; not player? skip to sprite draw
        bne     LF0EE
        lda     $17                     ; controller 2 Right held?
        and     #$01                    ; (debug: prevents off-screen death)
        bne     LF0D9
        lda     ent_y_scr                   ; player Y screen negative? draw
        bmi     LF0EE
        lda     #$0E                    ; set player state = $0E (death)
        cmp     player_state                     ; if already dead, skip
        beq     LF0D8
        sta     player_state                     ; store death state
        lda     #$F2                    ; play death sound
        jsr     submit_sound_ID
        lda     #$17                    ; play death music
        jsr     submit_sound_ID
LF0D8:  rts

LF0D9:  lda     ent_y_scr                   ; Y screen = 1? (landed from above)
        cmp     #$01
        bne     LF0EE
        lda     #PSTATE_AIRBORNE                    ; set player state = $01 (airborne)
        sta     player_state
        lda     #$10                    ; set Y subpixel speed = $10
        sta     ent_yvel_sub
        lda     #$0D                    ; set Y velocity = $0D (falling fast)
        sta     ent_yvel
LF0EE:  jmp     setup_sprite_render

LF0F1:  lda     ent_x_px,x                 ; for screen-fixed entities,
        sta     $13                     ; X pos is already screen-relative
LF0F6:  lda     ent_y_px,x                 ; Y pixel → draw Y position
        sta     $12
        lda     ent_flags,x
        ora     #$80                    ; set bit 7 (mark as drawn)
        sta     ent_flags,x
        and     #$04                    ; bit 2 = invisible (skip OAM)
        beq     setup_sprite_render
sprite_render_ret:  rts

; --- setup_sprite_render ---
; prepares sprite bank selection and animation state for OAM assembly
; X = entity slot, $10 = h-flip flag, $11 = flip offset
; $00/$01 = pointer to animation sequence data

setup_sprite_render:  ldy     #$00
        lda     ent_flags,x                 ; bit 6 = horizontal flip
        and     #$40
        sta     $10                     ; $10 = flip flag for OAM attr
        beq     LF114
        iny                             ; Y=1 if flipped
LF114:  sty     $11                     ; $11 = flip table offset
        cpx     #$10                    ; slot >= 16? (weapon/projectile)
        bcc     LF126
        lda     boss_active                     ; $5A = weapon sprite bank override
        beq     LF126                   ; 0 = use normal entity bank
        ldy     #$15                    ; weapon sprites in PRG bank $15
        cpy     mmc3_select                     ; already selected?
        beq     LF13B
        bne     LF132
LF126:  ldy     #$1A                    ; OAM ID bit 7 selects bank:
        lda     ent_anim_id,x                 ; $00-$7F → bank $1A
        bpl     LF12E                   ; $80-$FF → bank $1B
        iny
LF12E:  cpy     mmc3_select                     ; already selected?
        beq     LF13B
LF132:  sty     mmc3_select
        stx     L0000                   ; select $8000-$9FFF PRG bank
        jsr     select_PRG_banks
        ldx     L0000                   ; restore entity slot
LF13B:  lda     ent_anim_id,x                 ; OAM ID = 0? no sprite
        beq     sprite_render_ret       ; return
        and     #$7F                    ; strip bank select bit
        tay
        lda     L8000,y                 ; $00/$01 = animation sequence ptr
        sta     L0000                   ; (low bytes at $8000+ID,
        lda     $8080,y                 ; high bytes at $8080+ID)
        sta     $01

; --- animation tick / frame advancement ---
; Animation sequence format at ($00):
;   byte 0 = total frames in sequence
;   byte 1 = ticks per frame (duration)
;   byte 2+ = sprite definition IDs per frame (0 = deactivate entity)
        lda     $17                     ; $17 bit 3 = slow-motion mode
        and     #$08
        beq     LF15F                   ; not set? check freeze
        lda     $95                     ; in slow-motion: only tick every 8th frame
        and     #$07
        bne     LF18B                   ; skip 7 of 8 frames
        lda     $17                     ; $17 bit 7 = full freeze
        and     #$80
        bne     LF18B                   ; completely frozen
LF15F:  lda     $58                     ; $58 = global animation freeze
        bne     LF18B                   ; nonzero = skip ticking
        lda     ent_anim_frame,x                 ; ent_anim_frame = frame tick counter
        and     #$7F                    ; (bit 7 = damage flash flag)
        inc     ent_anim_frame,x                 ; increment tick
        ldy     #$01
        cmp     (L0000),y               ; compare tick vs duration at seq[1]
        bne     LF18B                   ; not reached? keep waiting
        lda     ent_anim_frame,x                 ; tick reached duration:
        and     #$80                    ; reset tick to 0, preserve bit 7
        sta     ent_anim_frame,x                 ; (damage flash flag)
        lda     ent_anim_state,x                 ; ent_anim_state = current frame index
        and     #$7F                    ; (bit 7 preserved)
        inc     ent_anim_state,x                 ; advance to next frame
        dey                             ; Y=0
        cmp     (L0000),y               ; compare frame vs total at seq[0]
        bne     LF18B                   ; more frames? continue
        lda     #$00                    ; reached end: loop back to frame 0
        sta     ent_anim_state,x

; --- damage flash and sprite definition lookup ---
LF18B:  lda     ent_flags,x                 ; bit 7 = drawn flag (set by process_entity_display)
        bpl     LF1E3                   ; not marked for drawing? skip
        lda     ent_anim_frame,x                 ; bit 7 of tick counter = damage flash active
        bpl     LF19B                   ; not flashing? draw normally
        lda     frame_counter                     ; $92 = invincibility blink timer
        and     #$04                    ; blink every 4 frames
        bne     LF1E3                   ; odd phase = skip draw (invisible)
LF19B:  cpx     #$10                    ; slot < 16? not a weapon/projectile
        bcc     LF1CE
        lda     ent_hp,x                 ; ent_hp = projectile lifetime/timer
        and     #$E0                    ; upper 3 bits = active countdown
        beq     LF1CE                   ; not counting down? normal
        lda     ent_status,x                 ; ent_status bit 6 = slow decay flag
        and     #$40
        beq     LF1B3                   ; not set? always decay
        lda     $4F                     ; slow decay: only every 4th frame
        and     #$03
        bne     LF1BC                   ; skip 3 of 4 frames
LF1B3:  lda     ent_hp,x                 ; subtract $20 from timer
        sec                             ; (decrement upper 3-bit counter)
        sbc     #$20
        sta     ent_hp,x
LF1BC:  lda     ent_status,x                 ; bit 6 = explode when timer expires?
        and     #$40
        beq     LF1E3                   ; no flag? just disappear
        lda     ent_hp,x                 ; check if timer reached $20 threshold
        and     #$20
        beq     LF1CE                   ; not yet? keep going
        lda     #$AF                    ; use explosion sprite def $AF
        bne     write_entity_oam        ; (always branches)
LF1CE:  lda     ent_anim_state,x                 ; current frame index (strip bit 7)
        and     #$7F
        clc                             ; +2 to skip header bytes (count, duration)
        adc     #$02
        tay
        lda     (L0000),y               ; load sprite def ID from sequence
        bne     write_entity_oam        ; nonzero? go draw it
        sta     ent_status,x                 ; def ID = 0: deactivate entity
        lda     #$FF                    ; mark as despawned
        sta     ent_spawn_id,x
LF1E3:  rts

; -----------------------------------------------
; write_entity_oam — assembles OAM entries from sprite definition
; -----------------------------------------------
; Entry: A = sprite definition ID
;   $10 = H-flip mask ($40 or $00)
;   $11 = flip table offset (0=normal, 1=flipped)
;   $12 = entity screen Y position
;   $13 = entity screen X position
;   $97 = OAM buffer write pointer
; Sprite definition format:
;   byte 0 = sprite count (bit 7: use CHR bank $14 instead of $19)
;   byte 1 = position offset table index (for Y/X offsets)
;   byte 2+ = pairs of (CHR tile, OAM attribute) per sprite
; Position offset table at ($05/$06): Y offset, X offset per sprite

write_entity_oam:  tay                  ; sprite def ID → index
        lda     $8100,y                 ; $02/$03 = pointer to sprite definition
        sta     $02                     ; (low bytes at $8100+ID,
        lda     $8200,y                 ; high bytes at $8200+ID)
        sta     $03
        ldy     #$00
        lda     ($02),y                 ; byte 0 = sprite count + bank flag
        pha
        ldy     #$19                    ; default CHR bank = $19
        pla
        bpl     LF1FD                   ; bit 7 set? use bank $14
        and     #$7F
        ldy     #$14
LF1FD:  sta     $04                     ; $04 = sprite count (0-based loop counter)
        cpy     prg_bank                     ; $F5 = currently selected CHR bank
        beq     LF20C                   ; already selected? skip
        sty     prg_bank                     ; select CHR bank via $A000/$A001
        stx     $05
        jsr     select_PRG_banks
        ldx     $05                     ; restore temp
LF20C:  ldy     #$01                    ; byte 1 = position offset table index
        lda     ($02),y                 ; add flip offset ($11=0 or 1)
        clc                             ; to select normal/flipped offsets
        adc     $11
        pha
        lda     ent_flags,x                 ; bit 5 = on-ladder flag
        and     #$20                    ; stored to $11 for OAM attr overlay
        sta     $11                     ; (behind-background priority)
        pla                             ; offset table index → X
        tax
        lda     $BE00,x                 ; $BE00/$BF00 = pointer table for
        sec                             ; sprite position offsets
        sbc     #$02                    ; subtract 2 to align with def bytes
        sta     $05                     ; $05/$06 = offset data pointer
        lda     $BF00,x
        sbc     #$00
        sta     $06
        ldx     oam_ptr                     ; X = OAM write position
        beq     LF28F                   ; 0 = buffer wrapped, full
LF230:  lda     #$F0                    ; default Y clip boundary = $F0
        sta     L0000
        lda     stage_id                     ; stage $08 = Rush Marine underwater?
        cmp     #STAGE_DOC_NEEDLE
        bne     LF248
        lda     camera_screen                     ; check specific screens ($15 or $1A)
        cmp     #$15                    ; for reduced Y clip boundary
        beq     LF244
        cmp     #$1A
        bne     LF248
LF244:  lda     #$B0                    ; Y clip = $B0 for these screens
        sta     L0000
LF248:  iny                             ; read CHR tile from def
        lda     ($02),y
        sta     $0201,x                 ; OAM byte 1 = tile index
        lda     $12                     ; screen Y + offset Y
        clc
        adc     ($05),y
        sta     $0200,x                 ; OAM byte 0 = Y position
        lda     ($05),y                 ; overflow detection:
        bmi     LF25E                   ; if offset negative and result didn't
        bcc     LF260                   ; underflow, it's OK
        bcs     LF290                   ; positive offset + carry = overflow
LF25E:  bcc     LF290                   ; negative offset without borrow = underflow
LF260:  lda     $0200,x                 ; check Y against clip boundary
        cmp     L0000
        bcs     LF290                   ; Y >= clip? hide sprite
        iny                             ; read OAM attribute from def
        lda     ($02),y                 ; EOR with flip flag ($10)
        eor     $10                     ; ORA with ladder priority ($11)
        ora     $11
        sta     $0202,x                 ; OAM byte 2 = attribute
        lda     $13                     ; screen X + offset X
        clc
        adc     ($05),y
        sta     $0203,x                 ; OAM byte 3 = X position
        lda     ($05),y                 ; overflow detection for X
        bmi     LF281
        bcc     LF283
        bcs     LF291
LF281:  bcc     LF291
LF283:  inx                             ; advance OAM pointer by 4 bytes
        inx
        inx
        inx
        stx     oam_ptr                     ; update write position
        beq     LF28F                   ; wrapped to 0? buffer full
LF28B:  dec     $04                     ; decrement sprite count
        bpl     LF230                   ; more sprites? continue
LF28F:  rts

LF290:  iny                             ; skip attribute byte
LF291:  lda     #$F8                    ; hide sprite (Y=$F8 = below screen)
        sta     $0200,x
        bne     LF28B                   ; (always branches)

; -----------------------------------------------
; draw_energy_bars — draws up to 3 HUD energy meters
; -----------------------------------------------
; $B1-$B3 = bar slot IDs (bit 7 = active, bits 0-6 = energy index)
; $A2+Y = energy value ($80=empty, $9C=full, 28 units)
; Alternates iteration direction each frame for OAM priority fairness.
; Each bar draws 7 sprites vertically: tile selected from $F313 table,
; position from $F318 (OAM attr) and $F31B (X position).
draw_energy_bars:  lda     $95          ; alternate direction each frame
        lsr     a
        bcs     LF2AD                   ; odd = reverse
        ldx     #$00                    ; forward: bar 0, 1, 2
        stx     $10
LF2A1:  jsr     LF2BB                   ; draw bar[$10]
        inc     $10                     ; next bar index
        ldx     $10
        cpx     #$03                    ; all 3 bars drawn?
        bne     LF2A1                   ; no → loop
        rts

LF2AD:  ldx     #$02                    ; reverse: bar 2, 1, 0
        stx     $10
LF2B1:  jsr     LF2BB
        dec     $10
        ldx     $10
        bpl     LF2B1
LF2BA:  rts

; draw_one_bar — draws a single energy meter
; X = bar index (0-2), $B1+X = slot ID

LF2BB:  lda     $B1,x                   ; bit 7 = bar active?
        bpl     LF2BA                   ; not active? skip
        and     #$7F                    ; energy index (Y into $A2 table)
        tay
        lda     player_hp,y                   ; read energy value
        and     #$7F                    ; strip bit 7 (display flag)
        sta     L0000                   ; $00 = energy remaining (0-28)
        lda     bar_attributes,x        ; $01 = OAM attribute (palette)
        sta     $01                     ; bar 0=$00, 1=$01, 2=$02
        lda     bar_x_positions,x       ; $02 = X position
        sta     $02                     ; bar 0=$10, 1=$18, 2=$28
        ldx     oam_ptr                     ; X = OAM write pointer
        beq     LF310                   ; 0 = full, skip
        lda     #$48                    ; $03 = starting Y position ($48)
        sta     $03                     ; draws upward 7 segments
        lda     $01                     ; write OAM attribute
        sta     $0202,x
        lda     $02                     ; write X position
        sta     $0203,x
        lda     $03                     ; write Y position
        sta     $0200,x
        ldy     #$04                    ; each segment = 4 units of energy
        lda     L0000
        sec                             ; subtract 4 from remaining
        sbc     #$04
        bcs     LF2F7                   ; still >= 0? full segment
        ldy     L0000                   ; partial: Y = remaining 0-3
        lda     #$00                    ; energy = 0
LF2F7:  sta     L0000                   ; update remaining energy
        lda     bar_fill_tiles,y        ; tile from fill table: 4=$6B 3=$6A 2=$69 1=$68 0=$67
        sta     $0201,x                 ; OAM tile
        inx                             ; advance OAM by 4
        inx
        inx
        inx
        beq     LF310                   ; OAM buffer full?
        lda     $03                     ; Y -= 8 (move up one tile)
        sec
        sbc     #$08
        sta     $03
        cmp     #$10                    ; stop at Y=$10 (7 segments: $48→$10)
        .byte   $D0,$CB
LF310:  .byte   $86,$97,$60             ; update OAM pointer

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
        ora     $38,x
        .byte   $14
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
move_right_collide:  lda     ent_flags,x    ; set bit 6 = facing right (H-flip)
        ora     #$40
        sta     ent_flags,x
move_right_collide_no_face:
        cpx     #$00                    ; player (slot 0)?
        bne     LF596                   ; non-player: skip pre-move save
        lda     ent_x_px                   ; save player X before move
        sta     $02                     ; (for screen boundary detection)
        lda     ent_x_scr
        sta     $03
LF596:  jsr     move_sprite_right
        cpx     #$00                    ; player?
        bne     mr_tile_check           ; non-player: just check tile
        jsr     check_platform_horizontal ; player overlapping horiz platform?
        bcc     mr_tile_check           ; no → normal tile check
        jsr     platform_push_x_left    ; push player left (away from platform)
        jsr     LF5AA
        sec
        rts

LF5AA:  beq     mr_tile_check
        bcc     mr_tile_check
        iny
        jmp     ml_tile_check

mr_tile_check:  jsr     check_tile_collision
        jsr     update_collision_flags
        clc
        lda     $10                     ; bit 4 = solid tile?
        and     #$10
        beq     LF5C3                   ; no? clear carry, return
        jsr     snap_x_to_wall_right    ; snap to tile boundary (right)
        sec                             ; set carry = hit solid
LF5C3:  rts

; -----------------------------------------------
; move_left_collide — move left + set facing + tile collision
; -----------------------------------------------

move_left_collide:  lda     ent_flags,x     ; clear bit 6 = facing left (no H-flip)
        and     #$BF
        sta     ent_flags,x
move_left_collide_no_face:
        cpx     #$00                    ; player (slot 0)?
        bne     LF5DA
        lda     ent_x_px                   ; save player X before move
        sta     $02
        lda     ent_x_scr
        sta     $03
LF5DA:  jsr     move_sprite_left
        cpx     #$00                    ; player?
        bne     ml_tile_check
        jsr     check_platform_horizontal ; player overlapping horiz platform?
        bcc     ml_tile_check           ; no → normal tile check
        jsr     platform_push_x_right   ; push player right (away from platform)
        jsr     LF5EE
        sec
        rts

LF5EE:  bcs     ml_tile_check
        dey
        jmp     mr_tile_check

ml_tile_check:  jsr     check_tile_collision
        jsr     update_collision_flags
        clc
        lda     $10                     ; bit 4 = solid?
        and     #$10
        beq     LF605
        jsr     snap_x_to_wall_left     ; snap to tile boundary (left)
        sec
LF605:  rts

; -----------------------------------------------
; move_down_collide — move down + tile collision
; -----------------------------------------------

move_down_collide:  cpx     #$00        ; player?
        bne     LF614
        lda     ent_y_px                   ; save player Y before move
        sta     $02
        lda     ent_x_scr
        sta     $03
LF614:  jsr     move_sprite_down
        cpx     #$00                    ; player?
        bne     md_tile_check
        jsr     check_platform_vertical ; player standing on vert platform?
        bcc     md_tile_check           ; no → normal tile check
        jsr     platform_push_y_up      ; push player up (onto platform)
        jsr     LF628
        sec
        rts

LF628:  beq     md_tile_check
        bcc     md_tile_check
        iny
        jmp     mu_tile_check

md_tile_check:  jsr     check_tile_horiz ; vertical tile collision check
        jsr     update_collision_flags
        clc
        lda     $10                     ; bit 4 = solid?
        and     #$10
        beq     LF641
        jsr     snap_y_to_floor         ; snap to tile boundary (down)
        sec
LF641:  rts

; -----------------------------------------------
; move_up_collide — move up + tile collision
; -----------------------------------------------

move_up_collide:  cpx     #$00          ; player?
        bne     LF650
        lda     ent_y_px                   ; save player Y
        sta     $02
        lda     ent_x_scr
        sta     $03
LF650:  jsr     move_sprite_up
        cpx     #$00                    ; player?
        bne     mu_tile_check
        jsr     check_platform_vertical ; player touching vert platform?
        bcc     mu_tile_check           ; no → normal tile check
        jsr     platform_push_y_down    ; push player down (below platform)
        jsr     LF664
        sec
        rts

LF664:  bcs     mu_tile_check
        dey
        jmp     md_tile_check

mu_tile_check:  jsr     check_tile_horiz ; vertical tile collision check
        jsr     update_collision_flags
        clc
        lda     $10                     ; bit 4 = solid?
        and     #$10
        beq     LF67B
        jsr     snap_y_to_ceil          ; snap to tile boundary (up)
        sec
LF67B:  rts

; -----------------------------------------------
; move_vertical_gravity — $99-based vertical movement with collision
; -----------------------------------------------
; Combines velocity application, $99, screen boundary handling,
; and tile collision for entities affected by $99.
; Y velocity convention: positive = rising, negative = falling.
; Returns: C=1 if landed on solid ground, C=0 if airborne.

move_vertical_gravity:  cpx     #$00    ; player?
        bne     LF68A
        lda     ent_y_px                   ; save player Y before move
        sta     $02
        lda     ent_x_scr
        sta     $03
LF68A:  lda     ent_yvel,x                 ; Y velocity whole byte
        bpl     LF6CD                   ; positive = rising
        jsr     apply_y_velocity_fall   ; pos -= vel (moves down)
        cpx     #$00                    ; player?
        bne     LF6AC
        jsr     check_platform_vertical ; player landing on platform?
        bcc     LF6AC                   ; no → normal tile collision
        jsr     platform_push_y_up      ; push player up (onto platform)
        jsr     LF6A4
        jmp     LF6C8

LF6A4:  beq     LF6AC                   ; boundary adj: if crossed screen
        bcc     LF6AC                   ; may need opposite tile check
        iny
        jmp     mu_tile_check           ; → upward tile check on new screen

LF6AC:  jsr     apply_gravity           ; increase downward velocity
        jsr     check_tile_horiz        ; vertical tile collision check
        jsr     update_collision_flags
        cpx     #$00                    ; player?
        bne     LF6BF
        lda     tile_at_feet_max                     ; tile type = ladder top ($40)?
        cmp     #TILE_LADDER_TOP                    ; player lands on ladder top
        beq     LF6C5
LF6BF:  lda     $10                     ; bit 4 = solid tile?
        and     #$10
        beq     LF6FE                   ; no solid? still falling
LF6C5:  jsr     snap_y_to_floor         ; snap to tile boundary (down)
LF6C8:  jsr     reset_gravity           ; reset velocity to rest value
        sec                             ; C=1: landed
        rts

LF6CD:  iny                             ; adjust collision check direction
        jsr     apply_y_velocity_rise   ; pos -= vel (moves up)
        cpx     #$00                    ; player?
        bne     LF6E9
        jsr     check_platform_vertical ; player hitting platform from below?
        bcc     LF6E9                   ; no → normal tile collision
        jsr     platform_push_y_down    ; push player down (below platform)
        jsr     LF6E3
        jmp     LF6FB

LF6E3:  bcs     LF6E9                   ; boundary adj: if crossed screen
        dey                             ; may need opposite tile check
        jmp     md_tile_check           ; → downward tile check on new screen

LF6E9:  jsr     apply_gravity           ; apply $99 (slows rise)
        jsr     check_tile_horiz        ; vertical tile collision check
        jsr     update_collision_flags
        lda     $10                     ; bit 4 = solid tile?
        and     #$10
        beq     LF6FE                   ; no solid? continue rising
        jsr     snap_y_to_ceil          ; snap to tile boundary (up)
LF6FB:  jsr     reset_gravity           ; reset velocity
LF6FE:  clc                             ; C=0: airborne
        rts

; -----------------------------------------------
; update_collision_flags — update entity collision flags from tile check
; -----------------------------------------------
; $10 = tile collision result from check_tile_collision
; $41 = tile type from last collision (upper nibble of $BF00)
; Updates ent_flags bit 5 (on-ladder) based on tile type $60 (ladder).

update_collision_flags:  lda     $10    ; bit 4 = solid tile hit?
        and     #$10
        bne     LF71C                   ; solid? keep existing flags
        lda     ent_flags,x                 ; clear on-ladder flag (bit 5)
        and     #$DF
        sta     ent_flags,x
        lda     tile_at_feet_max                     ; $41 = tile type
        cmp     #$60                    ; $60 = ladder tile? (was $40 top + $20 body)
        bne     LF71C                   ; not ladder? done
        lda     ent_flags,x                 ; set on-ladder flag (bit 5)
        ora     #$20                    ; (used by OAM: behind-background priority)
        sta     ent_flags,x
LF71C:  rts

; moves sprite right by its X speeds
; parameters:
; X: sprite slot

move_sprite_right:  lda     ent_x_sub,x
        clc                             ; X subpixel += X subpixel speed
        adc     ent_xvel_sub,x
        sta     ent_x_sub,x
        lda     ent_x_px,x
        adc     ent_xvel,x                 ; X pixel += X velocity
        sta     ent_x_px,x                 ; (with carry from sub)
        bcc     LF73A                   ; no carry? no screen change
        lda     ent_x_scr,x
        adc     #$00                    ; X screen += 1
        sta     ent_x_scr,x
LF73A:  rts

; moves sprite left by its X speeds
; parameters:
; X: sprite slot

move_sprite_left:  lda     ent_x_sub,x
        sec                             ; X subpixel -= X subpixel speed
        sbc     ent_xvel_sub,x
        sta     ent_x_sub,x
        lda     ent_x_px,x
        sbc     ent_xvel,x                 ; X pixel -= X speed
        sta     ent_x_px,x                 ; (with carry from sub)
        bcs     LF758                   ; carry on? no screen change
        lda     ent_x_scr,x
        sbc     #$00                    ; X screen -= 1
        sta     ent_x_scr,x
LF758:  rts

; moves sprite down by its Y speeds
; parameters:
; X: sprite slot

move_sprite_down:  lda     ent_y_sub,x
        clc                             ; Y subpixel += Y subpixel speed
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     ent_yvel,x                 ; Y pixel += Y speed
        sta     ent_y_px,x                 ; (with carry from sub)
        cmp     #$F0
        bcc     LF778                   ; Y screens are only $F0 tall
        adc     #$0F                    ; if Y < $F0, return
        sta     ent_y_px,x                 ; else push down $F more and
        inc     ent_y_scr,x                 ; increment Y screen
LF778:  rts

; moves sprite up by its Y speeds
; parameters:
; X: sprite slot

move_sprite_up:  lda     ent_y_sub,x
        sec                             ; Y subpixel += Y subpixel speed
        sbc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x
        sbc     ent_yvel,x                 ; Y pixel += Y speed
        sta     ent_y_px,x                 ; (with carry from sub)
        bcs     LF796                   ; Y screens are only $F0 tall
        sbc     #$0F                    ; if result >= $00, return
        sta     ent_y_px,x                 ; else push up $F more and
        dec     ent_y_scr,x                 ; decrement Y screen
LF796:  rts

; -----------------------------------------------
; apply_y_speed — apply Y velocity + $99 (no collision)
; -----------------------------------------------
; Dispatches to fall/rise velocity application based on sign,
; then applies $99. Used by entities without tile collision.

apply_y_speed:  lda     ent_yvel,x         ; Y velocity sign check
        bpl     LF7A2
        jsr     apply_y_velocity_fall   ; falling: apply downward velocity
        jmp     apply_gravity           ; then apply $99

LF7A2:  jsr     apply_y_velocity_rise   ; rising: apply upward velocity
        jmp     apply_gravity           ; then apply $99

; -----------------------------------------------
; apply_y_velocity_fall — apply Y velocity when falling
; -----------------------------------------------
; Position -= velocity. When velocity is negative (falling),
; subtracting negative = adding to Y = moving down on screen.
; Handles downward screen wrap at Y=$F0.

apply_y_velocity_fall:  lda     ent_y_sub,x ; Y subpixel -= Y sub velocity
        sec
        sbc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x                 ; Y pixel -= Y velocity
        sbc     ent_yvel,x                 ; (negative vel → Y increases)
        sta     ent_y_px,x
        cmp     #$F0                    ; screen height = $F0
        bcc     LF7C7                   ; within screen? done
        adc     #$0F                    ; wrap to next screen
        sta     ent_y_px,x
        inc     ent_y_scr,x                 ; increment Y screen
LF7C7:  rts

; -----------------------------------------------
; apply_y_velocity_rise — apply Y velocity when rising
; -----------------------------------------------
; Position -= velocity. When velocity is positive (rising),
; subtracting positive = decreasing Y = moving up on screen.
; Handles upward screen wrap (underflow).

apply_y_velocity_rise:  lda     ent_y_sub,x ; Y subpixel -= Y sub velocity
        sec
        sbc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x                 ; Y pixel -= Y velocity
        sbc     ent_yvel,x                 ; (positive vel → Y decreases)
        sta     ent_y_px,x
        bcs     LF7E5                   ; no underflow? done
        sbc     #$0F                    ; wrap to previous screen
        sta     ent_y_px,x
        dec     ent_y_scr,x                 ; decrement Y screen
LF7E5:  rts

; -----------------------------------------------
; apply_gravity — subtract $99 from Y velocity
; -----------------------------------------------
; Gravity ($99) subtracted from velocity each frame, making it
; more negative over time (falling faster). Clamps at terminal
; velocity $F9.00 (-7 px/frame). Player float timer ($B9) delays
; $99 while active.

apply_gravity:  cpx     #$00            ; player?
        bne     LF7F2
        lda     $B9                     ; $B9 = player float timer
        beq     LF7F2                   ; 0 = not floating
        dec     $B9                     ; decrement float timer
        bne     LF81A                   ; still floating? skip $99
LF7F2:  lda     ent_yvel_sub,x                 ; Y sub velocity -= $99 ($99)
        sec
        sbc     gravity
        sta     ent_yvel_sub,x
        lda     ent_yvel,x                 ; Y whole velocity -= borrow
        sbc     #$00
        sta     ent_yvel,x
        bpl     LF81A                   ; still positive (rising)? done
        cmp     #$F9                    ; terminal velocity = $F9 = -7
        bcs     LF81A                   ; >= $F9 (-7 to -1)? within limit
        lda     ent_anim_id,x                 ; OAM ID check
        cmp     #$13                    ; $13 = exempt from terminal vel
        beq     LF81A                   ; (special entity, unlimited fall)
        lda     #$F9                    ; clamp to terminal velocity
        sta     ent_yvel,x                 ; $F9.00 = -7.0 px/frame
        lda     #$00
        sta     ent_yvel_sub,x
LF81A:  rts

; resets a sprite's $99/downward Y velocity
; parameters:
; X: sprite slot

reset_gravity:  cpx     #$00            ; $00 means player
        beq     LF82A
        lda     #$AB
        sta     ent_yvel_sub,x                 ; Y velocity
        lda     #$FF                    ; = $FFAB, or -0.332
        sta     ent_yvel,x
        rts

LF82A:  lda     #$C0
        sta     ent_yvel_sub,x                 ; player Y velocity
        lda     #$FF                    ; = $FFC0, or -0.25
        sta     ent_yvel,x
        rts

; resets sprite's animation & sets ID
; parameters:
; A: OAM ID
; X: sprite slot

reset_sprite_anim:  sta     ent_anim_id,x     ; store parameter -> OAM ID
        lda     #$00                    ; reset animation
        sta     ent_anim_state,x
        lda     ent_anim_frame,x
        and     #$80                    ; reset anim frame counter
        sta     ent_anim_frame,x
        rts

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

init_child_entity:  sta     ent_anim_id,y     ; child shape/type
        lda     #$00
        sta     ent_anim_state,y                 ; clear timer and Y screen
        sta     ent_y_scr,y
        lda     ent_flags,x                 ; inherit parent's horizontal flip
        and     #$40                    ; (bit 6) and combine with $90
        ora     #$90
        sta     ent_flags,y
        lda     #$80                    ; mark entity as active
        sta     ent_status,y
        lda     ent_anim_frame,y                 ; preserve only bit 7 of ent_anim_frame
        and     #$80
        sta     ent_anim_frame,y
        rts

; faces a sprite toward the player
; parameters:
; X: sprite slot
face_player:

        lda     #$01                    ; start facing right
        sta     ent_facing,x
        lda     ent_x_px,x
        sec                             ; if sprite is to the left
        sbc     ent_x_px                   ; of player
        lda     ent_x_scr,x                 ; (X screen priority, then
        sbc     ent_x_scr                   ; X pixel), return
        bcc     LF882
        lda     #$02                    ; else set facing to left
        sta     ent_facing,x
LF882:  rts

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

        lda     ent_facing,x                 ; load facing direction
        ror     a                       ; 3x ROR: bit 0 → bit 6
        ror     a
        ror     a
        and     #$40                    ; isolate bit 6 (was bit 0)
        sta     L0000
        lda     ent_flags,x                 ; clear old hflip, set new
        and     #$BF
        ora     L0000
        sta     ent_flags,x
        rts

; submit a sound ID to global buffer for playing
; if full, do nothing
; parameters:
; A: sound ID

submit_sound_ID_D9:  sta     $D9        ; also store ID in $D9

; this version doesn't store in $D9
submit_sound_ID:  stx     L0000         ; preserve X
        ldx     $DA                     ; X = current circular buffer index
        sta     $01                     ; sound ID -> $01 temp
        lda     $DC,x                   ; if current slot != $88
        cmp     #$88                    ; buffer FULL, return
        bne     LF8B0
        lda     $01                     ; add sound ID to current
        sta     $DC,x                   ; buffer slot
        inx
        txa                             ; increment circular buffer index
        and     #$07                    ; with wraparound $07 -> $00
        sta     $DA
LF8B0:  ldx     L0000                   ; restore X
        rts

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
        sec
        sbc     ent_y_px,x
        bcs     LF8C1                   ; positive → player is below
        eor     #$FF                    ; negate: player is above entity
        adc     #$01
        clc                             ; C=0 means player is above
LF8C1:  rts

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
        sec
        sbc     ent_x_px,x
        pha                             ; save low byte
        lda     ent_x_scr                   ; player_X_screen - entity_X_screen
        sbc     ent_x_scr,x                 ; (borrow propagates from pixel sub)
        pla                             ; recover pixel difference
        bcs     LF8D8                   ; positive → player is to the right
        eor     #$FF                    ; negate: player is to the left
        adc     #$01
        clc                             ; C=0 means player is left
LF8D8:  rts

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
        lda     ent_y_px                   ; player_Y - entity_Y
        sec
        sbc     ent_y_px,x
        ldy     #$00                    ; (clear Y again — assembler artifact)
        bcs     LF8EC                   ; player below or same → skip
        eor     #$FF                    ; negate: player is above
        adc     #$01
        ldy     #$04                    ; Y bit 2 = player above
LF8EC:  sta     L0000                   ; $00 = abs(Y distance)
        lda     ent_x_px                   ; player_X - entity_X (16-bit)
        sec
        sbc     ent_x_px,x
        pha
        lda     ent_x_scr                   ; screen portion
        sbc     ent_x_scr,x
        pla
        bcs     LF905                   ; player right or same → skip
        eor     #$FF                    ; negate: player is left
        adc     #$01
        iny                             ; Y bit 1 = player left
        iny
LF905:  sta     $01                     ; $01 = abs(X distance)
        cmp     L0000                   ; if X_dist >= Y_dist, no swap needed
        bcs     LF914
        pha                             ; swap so $01=larger, $00=smaller
        lda     L0000
        sta     $01
        pla
        sta     L0000
        iny                             ; Y bit 0 = axes swapped (Y dominant)
LF914:  lda     #$00                    ; $02 = sub-angle (0-2)
        sta     $02
        lda     $01                     ; larger_dist / 4
        lsr     a
        lsr     a
        cmp     L0000                   ; if larger/4 >= smaller → nearly axial
        bcs     LF929
        inc     $02                     ; sub-angle = 1 (moderate)
        asl     a                       ; larger/4 * 2 = larger/2
        cmp     L0000                   ; if larger/2 >= smaller → moderate
        bcs     LF929
        inc     $02                     ; sub-angle = 2 (nearly diagonal)
LF929:  tya                             ; table index = quadrant * 4 + sub-angle
        asl     a
        asl     a
        clc
        adc     $02
        .byte   $A8,$B9,$34,$F9,$60,$04,$05,$06
        .byte   $04,$08,$07,$06,$04,$0C,$0B,$0A
        .byte   $04,$08,$09,$0A,$04,$04,$03,$02
        .byte   $04,$00,$01,$02,$04,$0C,$0D,$0E
        .byte   $04
        brk                             ; above+left,  Y-dom: N, NNW, NW, (pad)
        .byte   $0F
        asl     $2004
        cmp     $85F8,y
        brk
        lda     ent_facing,x                 ; signed circular difference:
        clc                             ; (current + 8 - target) & $0F - 8
        adc     #$08                    ; offset by 8 to center range
        and     #$0F                    ; wrap to 0-15
        sec
        sbc     L0000                   ; subtract target
        and     #$0F                    ; wrap to 0-15
        sec
        sbc     #$08                    ; remove offset → signed diff (-7..+8)
        beq     LF97D                   ; diff=0: already facing target
        bcs     LF972                   ; diff>0: target is CCW → turn CW (DEC)
        inc     ent_facing,x                 ; diff<0: target is CW → turn CCW (INC)
        bne     LF975                   ; (always branches)
LF972:  dec     ent_facing,x
LF975:  lda     ent_facing,x                 ; wrap direction to 0-15
        and     #$0F
        sta     ent_facing,x
LF97D:  rts

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
        bpl     LF9FE
        sty     L0000                   ; save caller's Y
        ldy     #$1F                    ; start scanning from slot $1F
        sty     $01
LF989:  lda     ent_status,y                 ; entity type empty? skip
        bpl     LF9F6
        lda     ent_flags,y                 ; entity not active? skip
        bpl     LF9F6
        lda     ent_flags,y                 ; bit 2 set = disabled? skip
        and     #$04
        bne     LF9F6
        lda     ent_flags,y                 ; bits 0-1 = platform flags
        and     #$03                    ; neither set? skip
        beq     LF9F6
        and     #$01                    ; bit 0 = vertical platform?
        beq     LF9AA                   ; not set → check as horiz-only
        lda     ent_yvel,x                 ; player Y velocity positive (rising)?
        bpl     LF9F6                   ; can't land while rising → skip
LF9AA:  jsr     player_x_distance       ; $10 = |player_X - entity_X
        jsr     player_y_distance       ; $11 = |player_Y - entity_Y
        bcc     LF9B9                   ; player above entity? check hitbox
        lda     ent_flags,y                 ; player below + bit0 (vert platform)?
        and     #$01                    ; can't land from below
        bne     LF9F6
LF9B9:  lda     ent_hitbox,y                 ; shape index = ent_hitbox & $1F
        and     #$1F
        tay
        lda     $10                     ; X distance >= half-width? no overlap
        cmp     hitbox_mega_man_widths,y
        bcs     LF9F6
        sec                             ; Y overlap = half-height - Y distance
        lda     hitbox_mega_man_heights,y
        sbc     $11
        bcc     LF9F6                   ; negative? no Y overlap
        sta     $11                     ; $11 = overlap amount
        cmp     #$08                    ; clamp to max 8 pixels
        bcc     LF9D8
        lda     #$08
        sta     $11
LF9D8:  ldy     $01                     ; Y = platform entity slot
        lda     ent_routine,y                 ; entity type == $14 (moving platform)?
        cmp     #$14
        bne     LF9F0
        lda     ent_facing,y                 ; copy platform velocity to player vars
        sta     $36                     ; $36 = platform direction/speed
        lda     ent_xvel_sub,y                 ; $37 = platform X velocity
        sta     $37                     ; $38 = platform Y velocity
        lda     ent_xvel,y
        sta     $38
LF9F0:  sty     $5D                     ; $5D = platform entity slot
        ldy     L0000                   ; restore caller's Y
        sec                             ; C=1: standing on platform
        rts

LF9F6:  dec     $01                     ; next slot (decrement $1F→$01)
        ldy     $01
        bne     LF989                   ; slot 0 = player, stop there
        ldy     L0000                   ; restore caller's Y
LF9FE:  clc                             ; C=0: not on any platform
        rts

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
        bpl     LFA51
        sty     L0000                   ; save caller's Y
        ldy     #$1F                    ; start scanning from slot $1F
        sty     $01
LFA0B:  lda     ent_status,y                 ; entity type empty? skip
        bpl     LFA49
        lda     ent_flags,y                 ; bit 2 set = disabled? skip
        and     #$04
        bne     LFA49
        lda     ent_flags,y                 ; bit 1 = horizontal platform?
        and     #$02                    ; not set? skip
        beq     LFA49
        jsr     player_x_distance       ; $10 = |player_X - entity_X
        jsr     player_y_distance       ; $11 = |player_Y - entity_Y
        lda     ent_hitbox,y                 ; shape index = ent_hitbox & $1F
        and     #$1F
        tay
        lda     $11                     ; Y distance >= half-height? no overlap
        cmp     hitbox_mega_man_heights,y
        bcs     LFA49
        sec                             ; X overlap = half-width - X distance
        lda     hitbox_mega_man_widths,y
        sbc     $10
        bcc     LFA49                   ; negative? no X overlap
        sta     $10                     ; $10 = overlap amount
        cmp     #$08                    ; clamp to max 8 pixels
        bcc     LFA43
        lda     #$08
        sta     $10
LFA43:  sty     $5D                     ; $5D = platform shape index
        ldy     L0000                   ; restore caller's Y
        sec                             ; C=1: overlapping platform
        rts

LFA49:  dec     $01                     ; next slot (decrement $1F→$01)
        ldy     $01
        bne     LFA0B
        ldy     L0000                   ; restore caller's Y
LFA51:  clc                             ; C=0: not overlapping any platform
        rts

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

player_x_distance:  lda     ent_x_px       ; player_X_pixel - entity_X_pixel
        sec
        sbc     ent_x_px,y
        pha                             ; (16-bit subtract, high byte for sign)
        lda     ent_x_scr                   ; player_X_screen - entity_X_screen
        sbc     ent_x_scr,y
        pla
        bcs     LFA69                   ; player >= entity? already positive
        eor     #$FF                    ; negate: entity is right of player
        adc     #$01
        clc                             ; C=0 means player is to the left
LFA69:  sta     $10                     ; $10 = abs X distance
        rts

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

player_y_distance:  lda     ent_y_px       ; player_Y - entity_Y
        sec
        sbc     ent_y_px,y
        bcs     LFA7A                   ; player >= entity? positive
        eor     #$FF                    ; negate: player is above entity
        adc     #$01
        clc                             ; C=0 means player is above
LFA7A:  sta     $11                     ; $11 = abs Y distance
        rts

; -----------------------------------------------
; platform_push_x_left — push entity X left by $10
; -----------------------------------------------
; Subtracts $10 from entity[X] X position, then compares
; against reference point $02/$03 to determine side.
; results: carry = set if entity is left of $02/$03
; -----------------------------------------------

platform_push_x_left:  sec              ; entity_X -= $10 (push left)
        lda     ent_x_px,x
        sbc     $10
        sta     ent_x_px,x
        lda     ent_x_scr,x                 ; (16-bit: X screen)
        sbc     #$00
        sta     ent_x_scr,x
        jmp     compare_x

; -----------------------------------------------
; platform_push_x_right — push entity X right by $10
; -----------------------------------------------

platform_push_x_right:  clc             ; entity_X += $10 (push right)
        lda     ent_x_px,x
        adc     $10
        sta     ent_x_px,x
        lda     ent_x_scr,x                 ; (16-bit: X screen)
        adc     #$00
        sta     ent_x_scr,x
compare_x:  sec                         ; compare: $02/$03 - entity_X
        lda     $02                     ; result in carry flag
        sbc     ent_x_px,x                 ; C=1: entity left of ref point
        lda     $03                     ; C=0: entity right of ref point
        sbc     ent_x_scr,x
        rts

; -----------------------------------------------
; platform_push_y_up — push entity Y up by $11
; -----------------------------------------------
; Subtracts $11 from entity[X] Y position, with screen
; underflow handling. Then compares against $02/$03.
; results: carry = set if entity is above $02/$03
; -----------------------------------------------

platform_push_y_up:  sec                ; entity_Y -= $11 (push up)
        lda     ent_y_px,x
        sbc     $11
        sta     ent_y_px,x
        bcs     compare_y               ; no underflow? skip
        dec     ent_y_scr,x                 ; underflow: decrement Y screen
        jmp     compare_y

; -----------------------------------------------
; platform_push_y_down — push entity Y down by $11
; -----------------------------------------------
; Adds $11 to entity[X] Y position, with $F0 screen
; overflow handling. Then compares against $02/$03.
; results: carry = set if entity is above $02/$03
; -----------------------------------------------

platform_push_y_down:  clc              ; entity_Y += $11 (push down)
        lda     ent_y_px,x
        adc     $11
        sta     ent_y_px,x
        bcs     LFAD3                   ; carry? wrapped past $FF
        cmp     #$F0                    ; below screen bottom ($F0)?
        bcc     compare_y               ; no → done
        adc     #$0F                    ; wrap Y: $F0+$0F+C = next screen
        sta     ent_y_px,x
LFAD3:  inc     ent_y_scr,x                 ; increment Y screen
compare_y:  sec                         ; compare: $02/$03 - entity_Y
        lda     $02                     ; result in carry flag
        sbc     ent_y_px,x                 ; C=1: entity above ref point
        lda     $03                     ; C=0: entity below ref point
        sbc     ent_y_scr,x
        rts

; tests if a sprite is colliding
; with the player (Mega Man)
; parameters:
; X: sprite slot to check player collision for
; returns:
; Carry flag off = collision, on = no collision
check_player_collision:

        lda     player_state
        cmp     #PSTATE_DEATH                    ; is player dead
        beq     LFB3A                   ; or teleporting in?
        cmp     #PSTATE_REAPPEAR                    ; return
        beq     LFB3A
        sec
        lda     ent_flags,x
        bpl     LFB3A                   ; if not active (bit 7 clear)
        and     #$04                    ; or disabled (bit 2 set)
        bne     LFB3A                   ; return
        lda     ent_hitbox,x
        and     #$1F                    ; y = hitbox ID
        tay
        lda     hitbox_mega_man_heights,y ; Mega Man hitbox height
        sta     L0000                   ; -> $00
        lda     ent_anim_id
        cmp     #$10
        bne     LFB0F                   ; if Mega Man is sliding,
        lda     L0000                   ; adjust hitbox height
        sec                             ; by subtracting 8
        sbc     #$08
        sta     L0000
LFB0F:  lda     ent_x_px
        sec
        sbc     ent_x_px,x                 ; grab X delta
        pha                             ; player X - sprite X
        lda     ent_x_scr                   ; taking screen into account
        sbc     ent_x_scr,x                 ; via carry
        pla                             ; then take absolute value
        bcs     LFB24
        eor     #$FF
        adc     #$01
LFB24:  cmp     hitbox_mega_man_widths,y ; if abs(X delta) > hitbox X delta
        bcs     LFB3A                   ; return
        lda     ent_y_px
        sec                             ; get Y delta between
        sbc     ent_y_px,x                 ; the two sprites
        bcs     LFB36                   ; A = player sprite Y
        eor     #$FF                    ; - current sprite Y
        adc     #$01                    ; take absolute value
LFB36:  .byte   $C5,$00,$90,$00         ; return
LFB3A:  .byte   $60

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
        bit     $0808
        php
        php

; loops through all 3 weapon slots
; to check collision against each one
; for a passed in sprite slot
; parameters:
; X: sprite slot to check weapon collision for
; returns:
; Carry flag off = sprite is colliding with player's weapons, on = not
; $10: sprite slot of weapon collided with (if carry off)
check_sprite_weapon_collision:
        lda     player_state
        cmp     #PSTATE_DEATH                    ; is player dead
        beq     LFBD0                   ; or teleporting in?
        cmp     #PSTATE_REAPPEAR                    ; return carry on
        beq     LFBD0
        lda     #$03                    ; start loop through
        sta     $10                     ; all 3 weapon slots
LFB89:  ldy     $10
        lda     ent_status,y
        bpl     LFBCC                   ; if weapon
        lda     ent_flags,y                 ; is inactive
        bpl     LFBCC                   ; or not active (bit 7 clear)
        lda     ent_routine,y
        cmp     #$0F                    ; or routine $0F (item pickup dying)
        beq     LFBCC
        lda     ent_anim_id,y
        cmp     #$13                    ; or OAM ID $13
        beq     LFBCC
        cmp     #$D7                    ; or $D7
        beq     LFBCC
        cmp     #$D8                    ; or $D8
        beq     LFBCC
        cmp     #$D9                    ; or $D9
        beq     LFBCC                   ; or if current sprite (passed in)
        lda     ent_flags,x                 ; not active (bit 7 clear)
        bpl     LFBCC                   ; or disabled (bit 2 set)
        and     #$04                    ; then don't check collision
        bne     LFBCC
        lda     ent_x_px,y
        sta     L0000                   ; store X, screen, and Y
        lda     ent_x_scr,y                 ; information as parameters
        sta     $01                     ; for collision routine
        lda     ent_y_px,y
        sta     $02
        jsr     check_weapon_collision  ; carry cleared == collision
        bcc     LFBD1                   ; return carry off
LFBCC:  dec     $10                     ; continue loop,
        bne     LFB89                   ; stop at $00 (Mega Man)
LFBD0:  sec
LFBD1:  rts

; parameters:
; X: sprite slot to check collision for
; $00: comparison sprite's X position
; $01: comparison sprite's X screen
; $02: comparison sprite's Y position
; returns:
; Carry flag off = collision, on = no collision

check_weapon_collision:  sec
        lda     ent_hitbox,x                 ; Y = shape index
        and     #$1F                    ; (mod $1F)
        tay
        lda     L0000
        sec                             ; get an X delta between
        sbc     ent_x_px,x                 ; the two sprites
        pha                             ; A = comparison sprite X
        lda     $01                     ; - current sprite X
        sbc     ent_x_scr,x                 ; take screen into account
        pla                             ; as well via carry
        bcs     LFBEC                   ; take absolute value
        eor     #$FF                    ; so it's always positive
        adc     #$01                    ; whichever side it's on
LFBEC:  cmp     hitbox_weapon_widths,y  ; if abs(X delta) > hitbox X delta
        bcs     LFC02                   ; return
        lda     $02
        sec                             ; get Y delta between
        sbc     ent_y_px,x                 ; the two sprites
        bcs     LFBFD                   ; A = comparison sprite Y
        eor     #$FF                    ; - current sprite Y
        adc     #$01                    ; take absolute value
LFBFD:  cmp     hitbox_weapon_heights,y ; return
        .byte   $90,$00                 ; abs(Y delta) < hitbox delta
LFC02:  .byte   $60                     ; carry off means collision

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
LFC45:  lda     ent_status,x                 ; check sprite state sign bit
        bpl     LFC51                   ; off means inactive, return
        dex                             ; next slot (down)
        cpx     #$0F                    ; $00-$0F slots not for enemies
        bne     LFC45                   ; so stop there
        sec                             ; return C=1
        rts                             ; for no slots found

LFC51:  clc                             ; return C=0 slot found
        rts

; find free sprite slot routine, return in Y register
; searches sprite state table ent_status (enemy slots)
; for free slots (inactive)
; returns:
; Carry flag off = slot found, on = not found
; Y: next free slot # (if carry off)

find_enemy_freeslot_y:  ldy     #$1F    ; start looping from slot $1F
LFC55:  lda     ent_status,y                 ; check sprite state sign bit
        bpl     LFC61                   ; off means inactive, return
        dey                             ; next slot (down)
        cpy     #$0F                    ; $00-$0F slots not for enemies
        bne     LFC55                   ; so stop there
        sec                             ; return C=1
        rts                             ; for no slots found

LFC61:  clc                             ; return C=0 slot found
        rts

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
;     bit 0 ($01) = player is right
;     bit 1 ($02) = player is left
;     bit 2 ($04) = player is below
;     bit 3 ($08) = player is above
;   Callers typically store $0C into ent_facing,x and use it
;   to select move_sprite_right/left/up/down.
; -----------------------------------------------
calc_homing_velocity:

        jsr     entity_x_dist_to_player ; A = |x_dist|, C = player right
        sta     $0A                     ; $0A = x_distance
        lda     #$01                    ; $01 = right
        bcs     LFC6E
        lda     #$02                    ; $02 = left
LFC6E:  sta     L000C                   ; $0C = X direction bit
        jsr     entity_y_dist_to_player ; A = |y_dist|, C = player below
        sta     $0B                     ; $0B = y_distance
        lda     #$04                    ; $04 = below
        bcs     LFC7B
        lda     #$08                    ; $08 = above
LFC7B:  ora     L000C                   ; combine X and Y direction bits
        sta     L000C
        lda     $0B                     ; compare y_dist vs x_dist
        cmp     $0A
        bcs     LFCB8                   ; y_dist >= x_dist → Y dominant
        lda     $02                     ; X gets full speed
        sta     ent_xvel_sub,x                 ; X vel sub = speed sub
        lda     $03
        sta     ent_xvel,x                 ; X vel whole = speed whole
        lda     $0A                     ; divide_16bit: (x_dist << 16) / speed
        sta     $01                     ; → intermediate scale factor
        lda     #$00
        sta     L0000
        jsr     divide_16bit
        lda     $04                     ; use scale factor as new divisor
        sta     $02
        lda     $05
        sta     $03
        lda     $0B                     ; divide_16bit: (y_dist << 16) / scale
        sta     $01                     ; = speed * y_dist / x_dist
        lda     #$00
        sta     L0000
        jsr     divide_16bit
        lda     $04                     ; Y gets proportional speed
        sta     ent_yvel_sub,x                 ; Y vel sub
        lda     $05
        sta     ent_yvel,x                 ; Y vel whole
        rts

LFCB8:  lda     $02                     ; Y gets full speed
        sta     ent_yvel_sub,x                 ; Y vel sub = speed sub
        lda     $03
        sta     ent_yvel,x                 ; Y vel whole = speed whole
        lda     $0B                     ; divide_16bit: (y_dist << 16) / speed
        sta     $01                     ; → intermediate scale factor
        lda     #$00
        sta     L0000
        jsr     divide_16bit
        lda     $04                     ; use scale factor as new divisor
        sta     $02
        lda     $05
        sta     $03
        lda     $0A                     ; divide_16bit: (x_dist << 16) / scale
        sta     $01                     ; = speed * x_dist / y_dist
        lda     #$00
        sta     L0000
        jsr     divide_16bit
        lda     $04                     ; X gets proportional speed
        sta     ent_xvel_sub,x                 ; X vel sub
        lda     $05
        sta     ent_xvel,x                 ; X vel whole
        rts

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
        sta     $03
        lda     L0000                   ; if both inputs zero,
        ora     $01                     ; return 0
        bne     LFCFA
        sta     $02
        rts

LFCFA:  ldy     #$08                    ; 8 iterations (8-bit quotient)
LFCFC:  asl     $02                     ; shift $03:$00:$02 left
        rol     L0000                   ; (quotient ← dividend ← remainder)
        rol     $03
        sec                             ; try remainder - divisor
        lda     $03
        sbc     $01
        bcc     LFD0D                   ; divisor doesn't fit, skip
        sta     $03                     ; update remainder
        inc     $02                     ; set quotient bit
LFD0D:  dey
        bne     LFCFC
        rts

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
        sta     $07
        lda     L0000                   ; if all inputs zero,
        ora     $01                     ; return 0
        ora     $02
        ora     $03
        bne     LFD26
        sta     $04
        sta     $05
        rts

LFD26:  stx     $09                     ; save X (used as temp)
        ldy     #$10                    ; 16 iterations (16-bit quotient)
LFD2A:  asl     $06                     ; shift 32-bit chain left:
        rol     L0000                   ; $07:$01:$00:$06
        rol     $01                     ; (remainder ← dividend ← quotient)
        rol     $07
        sec                             ; try $07:$01 - $03:$02
        lda     $01                     ; (remainder - divisor)
        sbc     $02
        tax                             ; X = low byte of difference
        lda     $07
        sbc     $03
        bcc     LFD44                   ; divisor doesn't fit, skip
        stx     $01                     ; update remainder low
        sta     $07                     ; update remainder high
        inc     $06                     ; set quotient bit
LFD44:  dey
        bne     LFD2A
        lda     $06                     ; $04 = quotient low byte
        sta     $04
        lda     L0000                   ; $05 = quotient high byte
        sta     $05
        ldx     $09                     ; restore X
        rts

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
        sta     boss_active                     ; (bit 7 = boss fight in progress)
        lda     prg_bank                     ; save $A000 bank
        pha
        lda     #$0C                    ; $97 = $0C: OAM offset past player
        sta     oam_ptr                     ; (skip first 3 sprite entries)
        jsr     process_frame_and_yield ; process entities + yield one frame
        lda     #$00                    ; clear boss-active flag
        sta     boss_active
        lda     #$18                    ; restore $8000 bank to $18
        sta     mmc3_select                     ; (bank1C_1D fixed pair)
        pla                             ; restore $A000 bank
        sta     prg_bank
        jmp     select_PRG_banks        ; re-select banks and return

; --- process_frame_yield_full ---
; Saves both PRG banks, sets $97=$04, processes frame, restores banks.
; Called from bank0C/0B (level loading, cutscenes), bank18 (stage select).
process_frame_yield_full:

        lda     mmc3_select                     ; save both PRG banks
        pha
        lda     prg_bank
        pha
        jsr     process_frame_yield_with_player ; $97=$04, process + yield
restore_banks:  pla                     ; restore $A000 bank
        sta     prg_bank
        pla                             ; restore $8000 bank
        sta     mmc3_select
        jmp     select_PRG_banks        ; re-select and return

; --- process_frame_yield ---
; Same as above but uses caller's $97 value (doesn't set $04).
; Called from bank0B/0F (level transitions with custom OAM offset).
process_frame_yield:

        lda     mmc3_select                     ; save both PRG banks
        pha
        lda     prg_bank
        pha
        jsr     process_frame_and_yield ; process + yield (caller's $97)
        jmp     restore_banks           ; restore banks and return

; --- call_bank0E_A006 ---
; Switches $A000-$BFFF to bank $0E, calls entry point $A006 with X=$B8.
; Called from bank12 (entity AI).
call_bank0E_A006:

        stx     $0F                     ; save X
        lda     prg_bank                     ; save $A000 bank
        pha
        lda     #$0E                    ; switch $A000 to bank $0E
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $B8                     ; X = parameter from $B8
        jsr     LA006                   ; call bank $0E entry point
restore_A000:
        pla                             ; restore $A000 bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $0F                     ; restore X
        rts

; --- call_bank0E_A003 ---
; Switches $A000-$BFFF to bank $0E, calls entry point $A003.
; Called from bank12 (entity AI).
call_bank0E_A003:

        stx     $0F                     ; save X
        lda     prg_bank                     ; save $A000 bank
        pha
        lda     #$0E                    ; switch $A000 to bank $0E
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     LA003                   ; call bank $0E entry point
        .byte   $4C,$9D,$FD,$8A,$40,$A3,$00,$0F ; restore and return
        .byte   $04,$4B,$50,$80,$04,$18,$10,$E0
        .byte   $00,$64,$04,$C5,$45,$67,$50,$CA
        .byte   $11,$1B,$51,$BA,$00,$44,$00,$3E
        .byte   $00,$A2,$04,$99,$00,$DD,$04,$81
        .byte   $10,$2B,$11,$80,$01,$4A,$40,$9C
        .byte   $01,$47,$15,$F7,$11,$47,$44,$17
        .byte   $40,$C2,$40,$28,$11,$CB,$44,$6E
        .byte   $50,$8A,$54,$AE,$10,$2B
        brk
        .byte   $53
        ora     $5D
        ora     screen_mode,x
        cld                             ; clear decimal mode
        lda     #$08                    ; PPUCTRL: sprite table = $1000
        sta     $2000                   ; (NMI not yet enabled)
        lda     #$40                    ; APU frame counter: disable IRQ
        sta     $4017
        ldx     #$00
        stx     $2001                   ; PPUMASK: rendering off
        stx     $4010                   ; DMC: disable
        stx     $4015                   ; APU status: silence all channels
        dex                             ; stack pointer = $FF
        txs

; --- wait for PPU warm-up (4 VBlank cycles) ---
        ldx     #$04                    ; 4 iterations
LFE1B:  lda     $2002                   ; wait for VBlank flag set
        bpl     LFE1B
LFE20:  lda     $2002                   ; wait for VBlank flag clear
        bmi     LFE20
        dex                             ; repeat 4 times
        bne     LFE1B

; --- exercise PPU address bus ---
        lda     $2002                   ; reset PPU latch
        lda     #$10                    ; toggle PPUADDR between $1010
        tay                             ; and $0000, 16 times
LFE2E:  sta     $2006                   ; write high byte
        sta     $2006                   ; write low byte
        eor     #$10                    ; toggle $10 ↔ $00
        dey
        bne     LFE2E

; --- clear zero page ($00-$FF) ---
        tya                             ; A = 0 (Y wrapped to 0)
LFE3A:  sta     L0000,y                 ; clear byte at Y
        dey                             ; Y: 0, $FF, $FE, ..., $01
        bne     LFE3A

; --- clear RAM pages $01-$06 ($0100-$06FF) ---
LFE40:  inc     $01                     ; advance page pointer ($01→$06)
LFE42:  sta     (L0000),y               ; clear byte via ($00/$01)+Y
        iny
        bne     LFE42                   ; 256 bytes per page
        ldx     $01
        cpx     #$07                    ; stop after page $06
        bne     LFE40                   ; (leaves stack area $0700+ alone)

; --- initialize sound buffer ($DC-$E3) to $88 (no sound) ---
        ldy     #$07
        lda     #$88                    ; $88 = "no sound" sentinel
LFE51:  sta     $DC,x                   ; X=7..0 → $E3..$DC
        dex
        bpl     LFE51

; --- hardware/bank initialization ---
        lda     #$18                    ; $FE = PPUMASK value
        sta     ppu_mask_shadow                     ; ($18 = show sprites + background)
        lda     #$00                    ; MMC3 mirroring: vertical
        sta     LA000
        ldx     #$1C                    ; $F4/$F5 = initial PRG banks
        stx     mmc3_select                     ; bank $1C at $8000-$9FFF
        inx                             ; bank $1D at $A000-$BFFF
        stx     prg_bank
        jsr     select_PRG_banks
        lda     #$40                    ; CHR bank setup:
        sta     $E8                     ; $E8=$40 (2KB bank 0)
        lda     #$42                    ; $E9=$42 (2KB bank 1)
        sta     $E9                     ; $EA=$00 (1KB bank 2)
        lda     #$00                    ; $EB=$01 (1KB bank 3)
        sta     $EA                     ; $EC=$0A (1KB bank 4)
        lda     #$01                    ; $ED=$0B (1KB bank 5)
        sta     $EB
        lda     #$0A
        sta     $EC
        lda     #$0B
        sta     $ED
        jsr     update_CHR_banks
        jsr     prepare_oam_buffer      ; init OAM / sprite state
        lda     #$20                    ; clear nametable 0 ($2000)
        ldx     #$00                    ; A=addr hi, X=fill, Y=attr fill
        ldy     #$00
        jsr     fill_nametable
        lda     #$24                    ; clear nametable 1 ($2400)
        ldx     #$00
        ldy     #$00
        jsr     fill_nametable

; --- register main game task (slot 0, address $C8D0) ---
        lda     #$C8                    ; $93/$94 = $C8D0 (main game entry)
        sta     $94                     ; (in bank $1C, always-mapped range)
        lda     #$D0
        sta     L0093
        lda     #$00                    ; A = slot 0
        jsr     task_register           ; register task with address
        lda     #$88                    ; $FF = PPUCTRL: NMI enable + sprite $1000
        sta     ppu_ctrl_shadow

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
; $90 = NMI-occurred flag (set $FF by NMI, forces rescan)
; $91 = current task slot index (0-3)
; ---------------------------------------------------------------------------
task_scheduler:  ldx     #$FF           ; reset stack to top
        txs                             ; (discard all coroutine frames)
LFEAD:  ldx     #$00                    ; clear NMI flag
        stx     nmi_occurred
        ldy     #$04                    ; 4 slots to check
LFEB3:  lda     $80,x                   ; state >= $04? (ready or fresh)
        cmp     #$04
        bcs     LFEC3
        inx                             ; advance to next slot (+4 bytes)
        inx
        inx
        inx
        dey                             ; more slots?
        bne     LFEB3
        jmp     LFEAD                   ; no runnable tasks, spin until NMI

LFEC3:  lda     nmi_occurred                     ; if NMI fired during scan,
        bne     LFEAD                   ; rescan (states may have changed)
        dey                             ; convert Y countdown → slot index
        tya                             ; Y=3→0, Y=2→1, Y=1→2, Y=0→3
        eor     #$03
        sta     $91                     ; $91 = current slot index
        ldy     $80,x                   ; Y = task state
        lda     #$02                    ; mark slot as running
        sta     $80,x
        cpy     #$08                    ; state $08? → fresh task (JMP)
        bne     LFEE2
        lda     $82,x                   ; load address from slot
        sta     L0093
        lda     $83,x
        sta     $94
        jmp     (L0093)                 ; launch task at stored address

LFEE2:  lda     $82,x                   ; restore saved stack pointer
        tax
        txs
        lda     $91                     ; slot 0? read controllers first
        bne     LFEED                   ; (only main game task gets input)
        jsr     read_controllers        ; read_controllers
LFEED:  pla                             ; restore Y and X from stack
        tay                             ; (saved by task_yield)
        pla
        tax
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
        lda     L0093                   ; store address in slot bytes 2-3
        sta     $82,x
        lda     $94
        sta     $83,x
        lda     #$08                    ; state = $08 (fresh, has address)
        sta     $80,x
        rts

; --- task_kill_by_id — A = slot index to kill ---
task_kill_by_id:

        jsr     slot_offset             ; X = A × 4
        lda     #$00                    ; state = $00 (free)
        sta     $80,x
        rts

; --- task_kill_self — kill current task ($91) and return to scheduler ---
task_kill_self:

        jsr     slot_offset_self        ; X = $91 × 4
        lda     #$00                    ; state = $00 (free)
        sta     $80,x
        jmp     task_scheduler          ; back to scheduler (never returns)

; --- slot_offset_self — X = $91 × 4 (current task's byte offset) ---

slot_offset_self:  lda     $91          ; load current task index

; --- slot_offset — X = A × 4 (task slot byte offset) ---
slot_offset:  asl     a                 ; A × 4
        asl     a
        tax
        rts

; --- task_yield_x — yield for X frames ---
; Called extensively from cutscene/level-transition code for timed waits.

task_yield_x:  jsr     task_yield       ; yield one frame
        dex                             ; loop X times
        bne     task_yield_x
        rts

; --- task_yield — yield current task for 1 frame ---
; Saves X/Y on stack, stores sleep countdown and stack pointer in the
; task slot, sets state to $01 (sleeping), then jumps to scheduler.
; NMI will decrement the countdown; when it reaches 0, state → $04.
; Scheduler then restores SP and does RTS to resume here.

task_yield:  lda     #$01               ; $93 = sleep frames (1)
        sta     L0093
        txa                             ; save X and Y on stack
        pha                             ; (scheduler's .restore_regs will
        tya                             ; PLA these back on resume)
        pha
        jsr     slot_offset_self        ; X = $91 × 4
        lda     L0093                   ; store sleep countdown in byte 1
        sta     $81,x
        lda     #$01                    ; state = $01 (sleeping)
        sta     $80,x
        txa                             ; Y = slot byte offset
        tay
        tsx                             ; save current stack pointer
        stx     $82,y                   ; in slot byte 2
        jmp     task_scheduler          ; hand off to scheduler

update_CHR_banks:  lda     #$FF         ; turns on the flag for
        sta     $1B                     ; refreshing CHR banks
        rts                             ; during NMI

; selects all 6 swappable CHR banks
; based on what's in $E8~$ED

select_CHR_banks:  lda     $1B          ; test select CHR flag
        beq     LFF56                   ; return if not on
LFF45:  ldx     #$00                    ; reset select CHR flag
        stx     $1B                     ; immediately, one-off usage
LFF49:  stx     L8000
        lda     $E8,x                   ; loop index $00 to $05
        sta     $8001                   ; -> bank select register
        inx                             ; fetch CHR bank from RAM
        cpx     #$06                    ; -> bank data register
        bne     LFF49
LFF56:  rts

; ===========================================================================
; process_frame_and_yield — run one frame of game logic, then yield to NMI
; ===========================================================================
; Core game loop primitive. Processes all entities, builds OAM sprite data,
; clears $EE (signals "ready for NMI to render"), yields one frame, then
; sets $EE (signals "NMI done, safe to modify state").
;
; process_frame_yield_with_player: sets $97=$04 (include player sprites)
; process_frame_and_yield: uses caller's $97 value
;
; $97 = OAM write index start (controls which sprite slots to fill):
;   $04 = start after sprite 0 (include player), $0C = skip player sprites
; $EE = rendering phase flag (0=NMI pending, nonzero=NMI completed)
; ---------------------------------------------------------------------------

process_frame_yield_with_player:  lda     #$04 ; $97 = $04: start OAM after sprite 0
        sta     oam_ptr                     ; (include player in sprite update)
process_frame_and_yield:  jsr     prepare_oam_buffer ; process entities (sprite state)
        jsr     update_entity_sprites   ; build OAM buffer from entity data
        lda     #$00                    ; $EE = 0: "waiting for NMI"
        sta     nmi_skip
        jsr     task_yield              ; sleep 1 frame (NMI will render)
        inc     nmi_skip                     ; $EE = 1: "NMI done, frame complete"
        rts

; selects both swappable PRG banks
; based on $F4 and $F5

select_PRG_banks:  inc     $F6          ; flag on "selecting PRG bank"
        lda     #$06
        sta     mmc3_shadow
        sta     L8000                   ; select the bank in $F4
        lda     mmc3_select                     ; as $8000-$9FFF
        sta     $F2                     ; also mirror in $F2
        sta     $8001
        lda     #$07
        sta     mmc3_shadow
        sta     L8000                   ; select the bank in $F5
        lda     prg_bank                     ; as $A000-$BFFF
        sta     $F3                     ; also mirror in $F3
        sta     $8001
        dec     $F6                     ; flag selecting back off (done)
        lda     $F7                     ; if NMI and non-NMI race condition
        bne     play_sounds             ; we still need to play sounds
        rts                             ; else just return

; go through circular sound buffer and pop for play
; handle NMI simultaneous bank changes as well

play_sounds:  lda     $F6               ; this means both NMI and non
        bne     LFFCC                   ; are trying to select banks
        lda     #$06
        sta     L8000
        lda     #$16                    ; select bank 16 for $8000-$9FFF
        sta     $8001                   ; and 17 for $A000-$BFFF
        lda     #$07
        sta     L8000
        lda     #$17
        sta     $8001
LFFA8:  ldx     $DB                     ; is current sound slot in buffer
        lda     $DC,x                   ; == $88? this means
        cmp     #$88                    ; no sound, skip processing
        beq     LFFC2
        pha                             ; push sound ID
        lda     #$88                    ; clear sound ID immediately
        sta     $DC,x                   ; in circular buffer
        inx
        txa                             ; increment circular buffer index
        and     #$07                    ; with wraparound $07 -> $00
        sta     $DB
        pla                             ; play sound ID
        jsr     L8003                   ; bank $16: play_sound_effect
        jmp     LFFA8                   ; check next slot

LFFC2:  jsr     L8000                   ; bank $16: sound_driver_tick
        lda     #$00                    ; clear race condition flag
        sta     $F7
        .byte   $4C,$6B,$FF
LFFCC:  .byte   $E6,$F7,$60,$05,$10,$11,$04,$41 ; flag the race condition
        .byte   $33,$5C,$D4,$45,$82,$00,$EF,$51
        .byte   $68,$50,$67,$10,$1C,$00,$07,$04
        .byte   $CD,$50,$00,$50,$04,$15,$96,$00
        .byte   $71,$14,$94,$15,$DD,$0E,$97,$C3
        .byte   $43,$04,$00,$00,$08,$57

; interrupt vectors
        brk
        cpy     #$00
        inc     IRQ,x
