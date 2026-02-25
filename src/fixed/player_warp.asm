; ===========================================================================
; PLAYER WARP STATES — $DD14-$E0FF
; ===========================================================================
; States $10-$15: vertical scroll transitions, teleporter tube sequences,
; Break Man encounter, and scripted auto-walk cutscenes.
; Also contains weapon/boss lookup tables shared with victory state.
; ===========================================================================

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
proto_man_encounter_init:  rts           ; return from encounter init

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
; --- overlap trick: $A9,$5A = lda #$5A (load 90 into A for timer) ---
; Fall-through loads A=$5A (90 frames); entry at weapon_hurt_timer_done
; skips the load (A already set by caller).
auto_walk_spawn_done:  .byte   $A9      ; opcode: lda immediate
weapon_hurt_timer_done:  .byte   $5A    ; operand: $5A (90 frames)
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

