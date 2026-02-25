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

