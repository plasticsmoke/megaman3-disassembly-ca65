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
init_hard_knuckle_exit:  rts

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
shadow_blade_init_exit:  rts

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
        lda     $33                     ; check slide timer phase
        cmp     #$0C                    ; >= $0C? (uncancellable phase)
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
death_burst_table_start:  rts           ; return from death timer increment

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
