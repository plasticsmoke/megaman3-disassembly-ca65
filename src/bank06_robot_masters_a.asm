; =============================================================================
; MEGA MAN 3 (U) — BANK $06 — ROBOT MASTER AI (NEEDLE/MAGNET/TOP/SHADOW)
; =============================================================================
; Mapped to $A000-$BFFF. Contains boss AI routines dispatched from bank1C_1D
; for routine indices $C0-$CF. Entry points: main_needle_man, main_magnet_man_j,
; main_top_man_j, main_shadow_man_j. Each boss has a multi-state AI with
; attack patterns, movement, and vulnerability windows.
; Also doubles as Spark Man stage data ($22=$06).
;
; Routine index mapping (from bank1C_1D dispatch):
;   $C0 -> main_needle_man_j  (Needle Man boss AI)
;   $C1 -> main_magnet_man_j  (Magnet Man boss AI)
;   $C2 -> main_top_man_j     (Top Man boss AI)
;   $C3 -> main_shadow_man_j  (Shadow Man boss AI)
;   $C4 -> code_A472           (Magnet Missile projectile)
;   $C5 -> code_A62B           (Top Spin projectile)
;   $C6 -> code_A8E0           (Shadow Blade projectile)
;   $C7 -> main_needle_man     (Needle Man — duplicate entry)
;   $C8 -> code_A698           (Shadow Man — duplicate entry)
;   $C9 -> RTS                 (stub)
; =============================================================================
main_needle_man_j:

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

L0000           := $0000
move_right_collide           := $F580
move_left_collide           := $F5C4
move_down_collide           := $F606
move_vertical_gravity           := $F67C
move_sprite_right           := $F71D
move_sprite_left           := $F73B
move_sprite_down           := $F759
move_sprite_up           := $F779
reset_gravity           := $F81B
reset_sprite_anim           := $F835
init_child_entity           := $F846
face_player           := $F869
submit_sound_ID           := $F89A
entity_x_dist_to_player           := $F8C2
find_enemy_freeslot_y           := $FC53
calc_homing_velocity           := $FC63

.segment "BANK06"

; --- Entry trampolines (dispatched from bank1C_1D entity AI) ----------------
        jmp     main_needle_man ; $C0 -- Needle Man boss AI
main_magnet_man_j:
        jmp     code_A24E       ; $C1 -- Magnet Man boss AI
main_top_man_j:
        jmp     code_A4B3       ; $C2 -- Top Man boss AI
main_shadow_man_j:
        jmp     code_A698       ; $C3 -- Shadow Man boss AI
        jmp     code_A472       ; $C4 -- Magnet Missile projectile
        jmp     code_A62B       ; $C5 -- Top Spin projectile
        jmp     code_A8E0       ; $C6 -- Shadow Blade projectile
        jmp     main_needle_man ; $C7 -- Needle Man (duplicate)
        jmp     code_A698       ; $C8 -- Shadow Man (duplicate)
        rts                     ; $C9 -- stub

; =============================================================================
; NEEDLE MAN AI ($C0)
; =============================================================================
; 5-state AI: init, wait for B press, jump + throw needles, jump toward
; player, headbutt. Randomly varies jump height and X velocity each cycle.
; Throws two needles per jump, then either repeats or switches to headbutt.
; =============================================================================

main_needle_man:  lda     ent_status,x
        and     #$0F
        tay                             ; load Needle Man's
        lda     needle_man_state_ptr_lo,y ; AI pointer based on state
        sta     L0000                   ; jump to it
        lda     needle_man_state_ptr_hi,y
        sta     $01
        jmp     (L0000)

; Needle Man state pointer table (lo/hi bytes)
; state $00: init  state $01: wait for B  state $02: throw needles
; state $03: jump toward player           state $04: headbutt
needle_man_state_ptr_lo:  .byte   $39,$4D,$68,$F4,$50
needle_man_state_ptr_hi:  .byte   $A0,$A0,$A0,$A0,$A1

; state $00: one-frame state for init
needle_man_init:
        lda     #$78                    ; timer: 120 frames
        sta     ent_timer,x                 ; to wait before acting
        jsr     needle_man_setup_throw
        lda     ent_status,x
        ora     #$40                    ; set boss flag
        sta     ent_status,x
        inc     ent_status,x                 ; next state
        rts

; state $01: waiting for B press at beginning of battle
needle_man_wait_B:

        lda     #$00
        sta     ent_anim_frame,x                 ; clear animation
        sta     ent_anim_state,x
        dec     ent_timer,x
        bne     LA05E                   ; if timer expires,
        inc     ent_status,x                 ; go to next state
        rts

LA05E:  lda     joy1_press
        and     #BTN_B                    ; if player presses B,
        beq     LA067                   ; go to next state
        inc     ent_status,x
LA067:  rts

; state $02: jumping & throwing needles
needle_man_throw:

        lda     ent_anim_id,x
        cmp     #$28                    ; if his animation is throwing
        beq     LA0AD                   ; needle, skip some stuff
        ldy     #$1E
        jsr     move_vertical_gravity                   ; I believe this checks for his
        bcc     LA093                   ; Y value to be back on ground?
        lda     #$29                    ; start animation sequence
        jsr     reset_sprite_anim                   ; for jumping before needle throw
        lda     #$02
        sta     ent_anim_state,x                 ; set up animation frame
        lda     #$00                    ; for ???
        sta     ent_anim_frame,x
        lda     #$08                    ; 8 frame timer for a brief pause
        sta     ent_var2,x
        inc     ent_status,x                 ; next state
        jsr     face_player                   ; face toward player
        jmp     needle_man_setup_jump   ; setup jump values

LA093:  lda     ent_yvel,x                 ; if he is moving down
        bmi     LA0A3
        lda     #$01
        sta     ent_anim_state,x                 ; if not, set up animation
        lda     #$00                    ; frame for ???
        sta     ent_anim_frame,x
        rts

LA0A3:  lda     ent_var2,x                 ; if timer hasn't expired
        bne     LA0EE                   ; for moving down
        lda     #$28                    ; if it has, set up animation
        jsr     reset_sprite_anim                   ; for throwing needle
LA0AD:  jsr     test_facing_change      ; handle facing change
        lda     ent_timer,x                 ; if first needle has
        bne     LA0C2                   ; been thrown, skip
        lda     ent_anim_state,x
        cmp     #$01                    ; if animation frame is not ???
        bne     LA0D4
        jsr     spawn_needle            ; else spawn a needle and set
        inc     ent_timer,x                 ; "first needle thrown" flag
LA0C2:  lda     ent_var1,x                 ; if second needle has
        bne     LA0D4                   ; been thrown, skip
        lda     ent_anim_state,x
        cmp     #$03                    ; if animation frame is not ???
        bne     LA0D4
        jsr     spawn_needle            ; else spawn a needle and set
        inc     ent_var1,x                 ; "second needle thrown" flag
LA0D4:  lda     ent_anim_state,x
        cmp     #$03                    ; if animation frame is not ???
        bne     LA0F3
        lda     #$00
        sta     ent_timer,x                 ; clear needle thrown flags
        sta     ent_var1,x
        lda     #$29                    ; start animation sequence
        jsr     reset_sprite_anim                   ; for jumping between needle throws
        lda     #$10                    ; give 16 frames between throws
        sta     ent_var2,x                 ; timer
        rts

LA0EE:  dec     ent_var2,x                 ; tick timer down
        bne     LA0F3                   ; useless branch
LA0F3:  rts

; state $03: jump toward player (or pause)
needle_man_jump_player:

        lda     ent_anim_state,x                 ; if animation frame is ???
        cmp     #$02                    ; this is to skip movement
        beq     LA132                   ; if this is just a pause state
        lda     #$01
        sta     ent_anim_state,x                 ; set up animation frame
        lda     #$00                    ; for ???
        sta     ent_anim_frame,x
        lda     ent_facing,x
        and     #$01                    ; if he's not facing right
        beq     LA114
        ldy     #$20                    ; move right
        jsr     move_right_collide
        jmp     LA119

LA114:  ldy     #$21                    ; move left
        jsr     move_left_collide
LA119:  ldy     #$1E
        jsr     move_vertical_gravity                   ; I believe this checks for his
        bcc     LA14F                   ; Y value to be back on ground?
        lda     #$02
        sta     ent_anim_state,x                 ; set up animation frame
        lda     #$00                    ; for ???
        sta     ent_anim_frame,x
        jsr     test_facing_change
        lda     #$08                    ; 8-frame timer
        sta     ent_var2,x                 ; for extra pause when ground reached
LA132:  dec     ent_var2,x                 ; tick timer down
        bne     LA14F                   ; check if done
        lda     ent_var3,x                 ; test if state value from table
        bne     LA145                   ; was $FF
        lda     #$C2                    ; if it was $00 instead
        sta     ent_status,x                 ; go to throw needles state
        jsr     needle_man_setup_throw  ; and setup values for it
        rts

LA145:  lda     #$2A                    ; set up headbutt
        jsr     reset_sprite_anim                   ; animation
        lda     #$C4                    ; go to headbutt state
        sta     ent_status,x
LA14F:  rts

; state $04: headbutt — Needle Man extends head spike
needle_man_headbutt:
        lda     ent_anim_state,x
        cmp     #$04                    ; animation done?
        bne     code_A176
        lda     ent_anim_frame,x
        and     #$08
        beq     code_A176
        lda     #$C0                    ; reset routine to main entry
        sta     ent_routine,x
        lda     #$29                    ; jumping anim
        jsr     reset_sprite_anim
        lda     #$C3                    ; go to jump-toward-player state
        sta     ent_status,x
        jsr     needle_man_setup_jump
code_A170:  lda     #$CA                ; normal hitbox
        sta     ent_hitbox,x
        rts

code_A176:  lda     ent_anim_state,x
        cmp     #$02                    ; headbutt extended?
        bne     code_A170
        lda     #$D2                    ; headbutt hitbox (larger)
        sta     ent_hitbox,x
        lda     #$C7                    ; switch routine to headbutt collision
        sta     ent_routine,x
        rts

needle_man_setup_throw:  lda     $E4
        adc     $E5                     ; grab RNG value
        sta     $E5                     ; and update it as well
        and     #$01                    ; y = random index from 0 to 1
        tay                             ; 50/50
        lda     needle_man_throw_vel_y_sub,y
        sta     ent_yvel_sub,x                 ; Y velocity = either
        lda     needle_man_throw_vel_y,y ; $093C or $0688
        sta     ent_yvel,x                 ; 50/50 chance
        rts

; values for needle man's throwing of needles
; indexed randomly 0 through 1 (50/50 chance each one)
; contains Y speed (both subpixel and pixel)
; so, jump heights

needle_man_throw_vel_y_sub:  .byte   $88,$3C
needle_man_throw_vel_y:  .byte   $06,$09
needle_man_setup_jump:  lda     #$88
        sta     ent_yvel_sub,x                 ; Y velocity = $0688
        lda     #$06
        sta     ent_yvel,x
        lda     $E4
        adc     $E5                     ; grab RNG value
        sta     $E5                     ; and update it as well
        and     #$07                    ; y = random index from 0 to 7
        tay
        lda     needle_man_jump_vel_x_sub,y
        sta     ent_xvel_sub,x                 ; one of 8 random X velocity
        lda     needle_man_jump_vel_x,y ; values
        sta     ent_xvel,x
        lda     needle_man_jump_states,y ; one of 8 random state values
        sta     ent_var3,x                 ; to go onto after jump
        rts

; values for needle man's jump toward player
; indexed randomly 0 through 7 (8 possible values)
; contains X speed (both subpixel and pixel) and states
; to move onto after the jump is completed

needle_man_jump_vel_x_sub:  .byte   $00,$80,$80,$00,$00,$80,$80,$80
needle_man_jump_vel_x:  .byte   $00,$01,$02,$00,$00,$01,$03,$02
needle_man_jump_states:  .byte   $FF,$00,$FF,$FF,$00,$FF,$00,$FF
; --- spawn_needle: create a needle projectile from Needle Man ----------------
spawn_needle:  jsr     find_enemy_freeslot_y           ; find free enemy slot
        bcs     code_A245               ; bail if none available
        sty     L0000
        lda     ent_facing,x           ; copy facing to projectile
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_anim_state,x
        cmp     #$01                    ; first throw pose?
        bne     code_A211
        lda     ent_x_px,x             ; position needle (pose 1 offset)
        clc
        adc     LA246,y
        pha
        lda     ent_x_scr,x
        adc     LA247,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        jmp     code_A228

code_A211:  lda     ent_x_px,x     ; position needle (pose 2 offset)
        clc
        adc     LA24A,y
        pha
        lda     ent_x_scr,x
        adc     LA24B,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
code_A228:  lda     ent_y_px,x     ; Y offset = +7 pixels down
        clc
        adc     #$07
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$27                    ; needle sprite ID
        jsr     init_child_entity                   ; init child entity
        lda     #$8B
        sta     ent_hitbox,y            ; needle hitbox
        lda     #$AF                    ; needle AI routine
        sta     ent_routine,y
code_A245:  rts

; Needle spawn X offsets (per facing direction, two poses)
LA246:  .byte   $15
LA247:  .byte   $00,$EB,$FF
LA24A:  .byte   $02
LA24B:  .byte   $00,$FE,$FF
; =============================================================================
; MAGNET MAN AI ($C1)
; =============================================================================
; 5-state AI: init (3 bounces), random branch (50/50), fire Magnet Missiles
; while hovering, fall/land, and magnetic pull attack. Alternates direction
; each landing cycle. Magnet Missiles home toward the player.
; =============================================================================

code_A24E:  lda     ent_status,x    ; --- state dispatch ---
        and     #$0F
        tay
        lda     LA261,y
        sta     L0000
        lda     LA266,y
        sta     $01
        jmp     (L0000)

; Magnet Man state pointer table (lo/hi bytes)
; state $00: init (3 bounces)  state $01: random branch
; state $02: fire missiles     state $03: fall/land
; state $04: magnetic pull
LA261:  .byte   $6B,$EB,$0C,$76,$A5
LA266:  .byte   $A2,$A2,$A3,$A3,$A3
; state $00: init — 3 bounces with alternating direction
        lda     ent_var1,x              ; landing pause active?
        bne     code_A2E7
        lda     ent_status,x
        ora     #$40                    ; set boss invincibility flag
        sta     ent_status,x
        lda     ent_var3,x              ; direction toggle
        and     #$01
        beq     code_A28D
        lda     ent_flags,x
        ora     #$40                    ; face right
        sta     ent_flags,x
        jsr     move_sprite_right                   ; move sprite right
        jmp     code_A298

code_A28D:  lda     ent_flags,x
        and     #$BF                    ; face left
        sta     ent_flags,x
        jsr     move_sprite_left                   ; move sprite left
code_A298:  ldy     #$22
        jsr     move_vertical_gravity                   ; apply gravity, check floor
        bcc     code_A2E2               ; still airborne
        lda     ent_timer,x             ; bounce index (0-2)
        tay
        lda     LA419,y                 ; load Y velocity sub for this bounce
        sta     ent_yvel_sub,x
        lda     LA41C,y                 ; load Y velocity for this bounce
        sta     ent_yvel,x
        lda     LA41F,y                 ; load X velocity sub for this bounce
        sta     ent_xvel_sub,x
        lda     LA422,y                 ; load X velocity for this bounce
        sta     ent_xvel,x
        lda     #$1F                    ; bounce animation
        jsr     reset_sprite_anim
        lda     #$04                    ; 4-frame landing pause
        sta     ent_var1,x
        inc     ent_timer,x             ; next bounce
        lda     ent_timer,x
        cmp     #$03                    ; done with 3 bounces?
        bcc     code_A2E1
        inc     ent_status,x            ; advance to next state
        lda     #$00
        sta     ent_timer,x
        lda     #$3C                    ; high jump Y velocity
        sta     ent_yvel_sub,x
        lda     #$09
        sta     ent_yvel,x
code_A2E1:  rts

code_A2E2:  lda     #$20                ; airborne animation
        jmp     reset_sprite_anim

code_A2E7:  dec     ent_var1,x          ; tick landing pause
        rts

; state $01: random branch — 50/50 missiles vs magnetic pull
        lda     $E4
        adc     $E5                     ; RNG
        sta     $E4
        and     #$01
        bne     code_A2F9
        inc     ent_status,x            ; -> state $02: fire missiles
        rts

code_A2F9:  lda     #$C4                ; -> state $04: magnetic pull
        sta     ent_status,x
        lda     #$F0                    ; pull duration = 240 frames
        sta     ent_var2,x
        lda     #$1E                    ; pull animation
        jsr     reset_sprite_anim
        jsr     code_A3FC               ; face player + set sprite flip
        rts

; state $02: fire Magnet Missiles while airborne
        lda     ent_anim_id,x
        cmp     #$21                    ; hovering anim started?
        beq     code_A333
        lda     #$20                    ; airborne anim
        jsr     reset_sprite_anim
        ldy     #$23
        jsr     move_vertical_gravity                   ; apply gravity (slow fall)
        lda     $10
        and     #$10                    ; landed on ceiling?
        beq     code_A332
        lda     #$21                    ; switch to hover animation
        jsr     reset_sprite_anim
        lda     #$00
        sta     ent_var1,x              ; missile count = 0
        lda     #$06
        sta     ent_timer,x             ; fire interval = 6 frames
code_A332:  rts

code_A333:  jsr     code_A3FC           ; face player
        dec     ent_timer,x             ; fire interval countdown
        bne     code_A375
        lda     #$06
        sta     ent_timer,x             ; reset fire interval
        lda     ent_anim_state,x
        cmp     #$01                    ; correct anim frame to fire?
        bne     code_A375
        jsr     code_A425               ; spawn Magnet Missile
        lda     #SFX_MAGNET_FIRE                    ; missile fire SFX
        jsr     submit_sound_ID
        inc     ent_var1,x              ; missile count++
        lda     ent_var1,x
        cmp     #$03                    ; fired 3 missiles?
        bcc     code_A375
        inc     ent_status,x            ; -> state $03: fall/land
        lda     #$00
        sta     ent_timer,x
        lda     #$1E                    ; post-fire delay
        sta     ent_var2,x
        lda     #$20                    ; falling anim
        jsr     reset_sprite_anim
        lda     #$80                    ; fall velocity
        sta     ent_yvel_sub,x
        lda     #$06
        sta     ent_yvel,x
code_A375:  rts

; state $03: fall after missiles, land, restart cycle
        lda     ent_timer,x
        bne     code_A383
        dec     ent_var2,x              ; post-fire delay
        bne     code_A375
        inc     ent_timer,x             ; begin falling
code_A383:  ldy     #$1E
        jsr     move_down_collide                   ; move down, check floor
        bcc     code_A3A4               ; still falling
        lda     #$1F                    ; landing animation
        jsr     reset_sprite_anim
        jsr     reset_gravity                   ; reset gravity
        lda     ent_var3,x
        eor     #$01                    ; toggle direction for next cycle
        sta     ent_var3,x
        lda     #$C0                    ; restart from state $00
        sta     ent_status,x
        lda     #$00
        sta     ent_timer,x
code_A3A4:  rts

; state $04: magnetic pull — drags player toward Magnet Man
        jsr     entity_x_dist_to_player                   ; get X distance to player
        cmp     #$18                    ; close enough? (< 24 px)
        bcc     code_A3E8               ; yes -> end pull, go to state $03
        lda     ent_flags,x
        and     #$40                    ; check facing direction
        bne     code_A3B7
        lda     #$01                    ; pull direction = right
        bne     code_A3B9
code_A3B7:  lda     #$02                ; pull direction = left
code_A3B9:  sta     $36                 ; set scroll pull direction
        lda     #$00
        sta     $37                     ; pull sub-speed
        lda     #$01
        sta     $38                     ; pull speed = 1 px/frame
        lda     ent_anim_state,x
        cmp     #$04                    ; advance animation cycle
        bne     code_A3D2
        lda     ent_anim_frame,x
        beq     code_A3D2
        inc     ent_anim_state,x
code_A3D2:  lda     ent_anim_state,x
        cmp     #$06                    ; loop animation back
        bne     code_A3E3
        lda     ent_anim_frame,x
        beq     code_A3E3
        lda     #$05
        sta     ent_anim_state,x
code_A3E3:  dec     ent_var2,x          ; pull duration countdown
        bne     code_A3F6
code_A3E8:  lda     #$C3                ; end pull -> state $03 (fall/land)
        sta     ent_status,x
        lda     #$CA                    ; restore normal hitbox
        sta     ent_hitbox,x
        sta     ent_timer,x
        rts

code_A3F6:  lda     #$AA                ; magnet pull hitbox (contact damage)
        sta     ent_hitbox,x
        rts

; --- face player and update sprite flip flag --------------------------------
code_A3FC:  jsr     face_player           ; face toward player
        lda     ent_facing,x
        and     #$01
        beq     code_A410
        lda     ent_flags,x
        ora     #$40                    ; set H-flip flag (facing right)
        sta     ent_flags,x
        bne     code_A418
code_A410:  lda     ent_flags,x
        and     #$BF                    ; clear H-flip flag (facing left)
        sta     ent_flags,x
code_A418:  rts

; Magnet Man bounce velocity tables (indexed 0-2 for 3 bounces)
; Y velocity sub, Y velocity, X velocity sub, X velocity
LA419:  .byte   $9E,$88,$88
LA41C:  .byte   $04,$06,$06
LA41F:  .byte   $B3,$00,$00
LA422:  .byte   $01,$02,$02
; --- spawn Magnet Missile projectile -----------------------------------------
code_A425:  jsr     find_enemy_freeslot_y           ; find free enemy slot
        bcs     code_A46F               ; bail if none
        sty     L0000
        lda     ent_facing,x           ; copy facing to missile
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x             ; position missile with X offset
        clc
        adc     LA470,y                 ; facing-dependent offset
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_y_scr,y
        sta     ent_xvel_sub,y
        sta     ent_yvel_sub,x
        lda     #$04                    ; missile speed = 4 px/frame
        sta     ent_xvel,y
        sta     ent_yvel,y
        lda     #$4D                    ; Magnet Missile sprite ID
        jsr     init_child_entity                   ; init child entity
        lda     #$80                    ; missile hitbox
        sta     ent_hitbox,y
        lda     #$C4                    ; missile AI routine (homing)
        sta     ent_routine,y
code_A46F:  rts

; Magnet Missile spawn X offset per facing direction
LA470:  .byte   $EC,$14
; --- Magnet Missile projectile AI ($C4) — homing toward player --------------
code_A472:  lda     ent_anim_id,x
        cmp     #$51                    ; exploding anim?
        beq     code_A49C
        lda     ent_status,x
        and     #$0F
        bne     code_A49C
        lda     ent_facing,x           ; move in facing direction
        and     #$01
        beq     code_A48D
        jsr     move_sprite_right                   ; move right
        jmp     code_A490

code_A48D:  jsr     move_sprite_left               ; move left
code_A490:  jsr     entity_x_dist_to_player               ; X distance to player
        cmp     #$06                    ; within 6 px?
        bcs     code_A4B2               ; no -> keep moving
        lda     #$51                    ; start explode animation
        jsr     reset_sprite_anim
code_A49C:  lda     ent_anim_id,x
        cmp     #$59                    ; impact anim done?
        beq     code_A4B2
        ldy     #$12
        jsr     move_down_collide                   ; move down, check floor collision
        bcc     code_A4B2
        lda     #$59                    ; play impact animation
        jsr     reset_sprite_anim
        inc     ent_status,x            ; deactivate after impact
code_A4B2:  rts

; =============================================================================
; TOP MAN AI ($C2)
; =============================================================================
; 4-state AI: init, spin attack (spawn Top Spin projectiles, invulnerable),
; walk across room (vulnerable), change direction + repeat.
; Top Spin projectiles orbit outward then fall.
; =============================================================================

code_A4B3:  lda     ent_status,x    ; --- state dispatch ---
        and     #$0F
        tay
        lda     LA4C6,y
        sta     L0000
        lda     LA4CA,y
        sta     $01
        jmp     (L0000)

; Top Man state pointer table (lo/hi bytes)
; state $00: init  state $01: spin attack
; state $02: walk  state $03: turn around
LA4C6:  .byte   $CE,$E2,$3A,$75
LA4CA:  .byte   $A4,$A4,$A5,$A5
; state $00: init
        lda     #$3C                    ; timer = 60 frames
        sta     ent_timer,x
        sta     ent_var1,x              ; walk timer = 60 frames
        lda     ent_status,x
        ora     #$40                    ; set boss invincibility flag
        sta     ent_status,x
        inc     ent_status,x            ; advance to spin attack
        rts

; state $01: spin attack — invulnerable, spawns Top Spin projectiles
        lda     #$CA                    ; normal hitbox
        sta     ent_hitbox,x
        lda     ent_anim_state,x
        cmp     #$02                    ; at spawn frame?
        bne     code_A4FD
        lda     ent_var2,x              ; already spawned tops?
        bne     code_A4FD
        lda     #$00                    ; spawn 3 Top Spin projectiles
        sta     $01
        jsr     code_A5BC               ; spawn top projectile loop
        inc     ent_var2,x              ; mark as spawned
code_A4FD:  lda     ent_anim_state,x
        cmp     #$03                    ; at recall frame?
        bne     code_A525
        lda     ent_anim_frame,x
        and     #$08
        beq     code_A525
        ldy     #$1F                    ; scan enemy slots for active tops
        lda     #$10                    ; top spawn ID marker
code_A50F:  cmp     ent_spawn_id,y      ; find matching top
        beq     code_A51B
        dey
        cpy     #$0F
        bne     code_A50F
        beq     code_A525               ; no more tops found
code_A51B:  lda     #$C5                ; set top to recall routine
        sta     ent_routine,y
        dey
        lda     #$10                    ; continue scanning for more
        bne     code_A50F
code_A525:  lda     ent_anim_state,x
        cmp     #$05                    ; spin complete?
        bne     code_A539
        lda     ent_anim_frame,x
        beq     code_A539
        inc     ent_status,x            ; -> state $02: walk
        lda     #$44                    ; walking animation
        jsr     reset_sprite_anim
code_A539:  rts

; state $02: walk across room (vulnerable to attacks)
        lda     ent_anim_id,x
        cmp     #$47                    ; spinning walk anim?
        beq     code_A555
        lda     ent_var2,x              ; top spawned previously?
        beq     code_A564
        dec     ent_timer,x             ; wait before spinning walk
        bne     code_A539
        lda     #$47                    ; start spinning walk anim
        jsr     reset_sprite_anim
        lda     #$AA                    ; spinning hitbox (contact damage)
        sta     ent_hitbox,x
code_A555:  lda     ent_anim_state,x
        cmp     #$02                    ; spin cycle complete?
        bne     code_A564
        lda     #$48                    ; next spin cycle anim
        jsr     reset_sprite_anim
        dec     ent_var2,x              ; count spin cycles
code_A564:  dec     ent_var1,x          ; walk duration countdown
        bne     code_A539
        inc     ent_status,x            ; -> state $03: turn around
        lda     #$78                    ; timer = 120 frames
        sta     ent_timer,x
        sta     ent_var1,x
        rts

; state $03: walk in direction, reverse at screen edge, restart spin
        lda     ent_var3,x              ; direction flag
        and     #$01
        beq     code_A589
        jsr     move_sprite_right                   ; move right
        lda     ent_x_px,x
        cmp     #$D0                    ; past right boundary?
        bcs     code_A593               ; yes -> reverse
        jmp     code_A5BB

code_A589:  jsr     move_sprite_left               ; move left
        lda     ent_x_px,x
        cmp     #$30                    ; past left boundary?
        bcs     code_A5BB               ; no -> keep going
code_A593:  lda     ent_var3,x          ; toggle walk direction
        eor     #$01
        sta     ent_var3,x
        lda     ent_facing,x           ; flip facing
        eor     #$03
        sta     ent_facing,x
        lda     ent_flags,x            ; flip sprite
        eor     #$40
        sta     ent_flags,x
        lda     #$49                    ; turn-around animation
        jsr     reset_sprite_anim
        dec     ent_status,x           ; back to state $01 (spin attack)
        dec     ent_status,x
        lda     #$00
        sta     ent_var2,x              ; reset top spawn flag
code_A5BB:  rts

; --- spawn Top Spin projectiles (called in loop, spawns 3) ------------------
code_A5BC:  jsr     find_enemy_freeslot_y           ; find free enemy slot
        bcs     code_A624               ; bail if none
        sty     L0000
        lda     ent_facing,x           ; copy facing
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x             ; same X as Top Man
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sec
        sbc     #$0E                    ; Y = Top Man Y - 14 px (above head)
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        sta     ent_routine,y
        sta     ent_xvel_sub,y
        sta     ent_yvel_sub,y
        lda     #$04                    ; speed = 4 px/frame
        sta     ent_xvel,y
        sta     ent_yvel,y
        lda     #$46                    ; Top Spin projectile sprite
        jsr     init_child_entity                   ; init child entity
        lda     #$8B                    ; projectile hitbox
        sta     ent_hitbox,y
        lda     #$10                    ; spawn ID marker for recall
        sta     ent_spawn_id,y
        ldy     $01                     ; projectile index (0-2)
        lda     LA625,y                 ; staggered delay per projectile
        ldy     L0000
        sta     ent_timer,y
        ldy     $01
        lda     LA628,y                 ; orbit duration per projectile
        ldy     L0000
        sta     ent_var1,y
        inc     $01                     ; next projectile
        lda     $01
        cmp     #$03                    ; spawned all 3?
        bcc     code_A5BC               ; loop if not
code_A624:  rts

; Top Spin projectile delay and orbit duration tables (3 projectiles)
LA625:  .byte   $08,$10,$18             ; staggered launch delays
LA628:  .byte   $32,$2A,$1E             ; orbit durations
; --- Top Spin projectile AI ($C5) — orbit then home toward player -----------
; Phase 0: move outward horizontally + upward (staggered delay)
; Phase 1: orbit/float (timed duration)
; Phase 2: home toward player
code_A62B:  lda     ent_status,x
        and     #$0F
        bne     code_A653
        lda     ent_facing,x           ; phase 0: move outward
        and     #$01
        beq     code_A63F
        jsr     move_sprite_right                   ; move right
        jmp     code_A642

code_A63F:  jsr     move_sprite_left               ; move left
code_A642:  jsr     move_sprite_up               ; move up
        dec     ent_timer,x             ; launch delay countdown
        bne     code_A652
        lda     #$00
        sta     ent_timer,x
        inc     ent_status,x            ; -> phase 1: orbit
code_A652:  rts

code_A653:  lda     ent_status,x        ; phase 1: orbit duration
        and     #$02
        bne     code_A663
        dec     ent_var1,x              ; orbit timer countdown
        bne     code_A652
        inc     ent_status,x            ; -> phase 2: home toward player
        rts

code_A663:  lda     ent_timer,x         ; phase 2: homing
        bne     code_A67B
        lda     #$33                    ; calculate homing velocity
        sta     $02                     ; max speed
        lda     #$05
        sta     $03                     ; acceleration
        jsr     calc_homing_velocity                   ; calc homing direction
        lda     $0C                     ; resulting facing bits
        sta     ent_facing,x
        inc     ent_timer,x             ; mark as initialized
code_A67B:  lda     ent_facing,x        ; apply homing movement
        and     #$08
        beq     code_A688
        jsr     move_sprite_up                   ; move up
        jmp     code_A68B

code_A688:  jsr     move_sprite_down               ; move down
code_A68B:  lda     ent_facing,x
        and     #$01
        beq     code_A695
        jmp     move_sprite_right                   ; move right

code_A695:  jmp     move_sprite_left               ; move left

; =============================================================================
; SHADOW MAN AI ($C3)
; =============================================================================
; 5-state AI: init, jump + throw Shadow Blades, decision branch (slide vs
; ninja jump), throw blades during jump, slide attack. Shadow Man moves
; quickly and alternates between ranged blade attacks and close-range slides.
; =============================================================================

code_A698:  lda     ent_status,x    ; --- state dispatch ---
        and     #$0F
        tay
        lda     LA6AB,y
        sta     L0000
        lda     LA6B0,y
        sta     $01
        jmp     (L0000)

; Shadow Man state pointer table (lo/hi bytes)
; state $00: init  state $01: jump + throw blades
; state $02: decision (slide vs ninja jump)
; state $03: throw blades during jump  state $04: slide attack
LA6AB:  .byte   $B5,$D6,$30,$93,$EE
LA6B0:  .byte   $A6,$A6,$A7,$A7,$A7
; state $00: init
        lda     #$00
        sta     ent_timer,x             ; clear timer
        sta     ent_var1,x              ; clear landing pause
        sta     ent_var3,x              ; clear misc counter
        lda     #$80
        sta     ent_xvel_sub,x          ; slow X drift
        lda     #$00
        sta     ent_xvel,x
        lda     ent_status,x
        ora     #$40                    ; set boss invincibility flag
        sta     ent_status,x
        inc     ent_status,x            ; -> state $01
        rts

; state $01: jump toward player + throw Shadow Blades on landing
        lda     ent_var1,x              ; landing pause active?
        bne     code_A722
        lda     ent_facing,x           ; move in facing direction
        and     #$01
        beq     code_A6EA
        ldy     #$24                    ; move right with collision
        jsr     move_right_collide
        jmp     code_A6EF

code_A6EA:  ldy     #$25                ; move left with collision
        jsr     move_left_collide
code_A6EF:  ldy     #$22
        jsr     move_vertical_gravity                   ; apply gravity, check floor
        bcc     code_A714               ; still airborne
        lda     #$40                    ; landing animation
        jsr     reset_sprite_anim
        lda     #$04                    ; 4-frame landing pause
        sta     ent_var1,x
        jsr     code_A84C               ; random jump velocity setup
        jsr     test_facing_change      ; face player on landing
        inc     ent_timer,x             ; count landings
        lda     ent_timer,x
        cmp     #$03                    ; done with 3 jumps?
        bcc     code_A72F
        inc     ent_status,x            ; -> state $02: decision
        rts

code_A714:  lda     #$40                ; airborne: reset anim
        jsr     reset_sprite_anim
        lda     #$00
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        rts

code_A722:  lda     #$01                ; landing pause: show standing frame
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        dec     ent_var1,x              ; tick landing pause
code_A72F:  rts

; state $02: decision — slide (var2==0) vs ninja jump (var2!=0)
        lda     ent_var2,x              ; jump height flag from table
        bne     code_A743
        inc     ent_status,x            ; -> state $03: throw blades
        lda     #$41                    ; blade throw animation
        jsr     reset_sprite_anim
        lda     #$00
        sta     ent_var1,x
        rts

code_A743:  inc     ent_status,x        ; -> state $04: slide attack
        inc     ent_status,x
        lda     #$08                    ; slide timer = 8
        sta     ent_timer,x
        lda     #$00
        sta     ent_var1,x
        lda     #$3E                    ; slide animation
        jsr     reset_sprite_anim
        lda     #$C8                    ; slide collision routine
        sta     ent_routine,x
        lda     ent_x_px,x             ; spawn shadow decoy at current pos
        sta     $0370
        lda     ent_x_scr,x
        sta     $0390
        lda     ent_y_px,x
        sta     $03D0
        lda     #$00
        sta     $04F0
        sta     $0490
        sta     $0330
        ldy     #$10
        lda     #$5A                    ; shadow decoy sprite
        jsr     init_child_entity                   ; init child entity
        lda     #$C3                    ; slide hitbox
        sta     ent_hitbox,x
        lda     #$00
        sta     ent_xvel_sub,x
        lda     #$04                    ; slide speed = 4 px/frame
        sta     ent_xvel,x
        jmp     face_player                   ; face player

; state $03: throw Shadow Blades (spawns 2 blades, then plays throw anim)
        lda     ent_var1,x              ; already spawned?
        bne     code_A7B4
        lda     ent_anim_state,x
        cmp     #$00                    ; at spawn frame?
        bne     code_A7ED
        jsr     test_facing_change      ; face player
        lda     #$00
        sta     $01
        jsr     code_A874               ; spawn 2 Shadow Blades
        lda     #$14                    ; 20-frame delay before throw anim
        sta     ent_timer,x
        lda     #$FF
        sta     ent_var1,x              ; mark as spawned
        rts

code_A7B4:  lda     ent_anim_state,x   ; throw animation sequence
        bne     code_A7D7
        dec     ent_timer,x             ; pre-throw delay
        bne     code_A7CE
        lda     #SFX_SHADOW_FIRE                    ; blade throw SFX
        jsr     submit_sound_ID
        lda     #$01                    ; advance to throw frame
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        rts

code_A7CE:  lda     #$00                ; hold pre-throw pose
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        rts

code_A7D7:  lda     ent_anim_frame,x   ; advance throw animation
        and     #$08
        beq     code_A7E1
        inc     ent_anim_state,x
code_A7E1:  lda     ent_anim_state,x
        cmp     #$06                    ; throw anim complete?
        bne     code_A7ED
        lda     #$C0                    ; restart from state $00
        sta     ent_status,x
code_A7ED:  rts

; state $04: slide attack — move toward player, stop on contact/wall/timer
        lda     ent_var1,x              ; passed through player?
        beq     code_A7F8
        dec     ent_timer,x             ; post-pass timer
        beq     code_A81E               ; timer expired -> stop
code_A7F8:  lda     ent_facing,x       ; slide in facing direction
        and     #$01
        beq     code_A807
        ldy     #$24                    ; move right with collision
        jsr     move_right_collide
        jmp     code_A80C

code_A807:  ldy     #$25                ; move left with collision
        jsr     move_left_collide
code_A80C:  bcs     code_A81E           ; hit wall -> stop
        lda     ent_var1,x
        bne     code_A835
        jsr     entity_x_dist_to_player                   ; X distance to player
        cmp     #$08                    ; within 8 px?
        bcs     code_A835               ; no -> keep sliding
        inc     ent_var1,x              ; mark as passed player
        rts

code_A81E:  lda     #$3D                ; slide end animation
        jsr     reset_sprite_anim
        lda     #$C0                    ; restart from state $00
        sta     ent_status,x
        lda     #$CA                    ; restore normal hitbox
        sta     ent_hitbox,x
        jsr     test_facing_change      ; face player
        lda     #$C3                    ; restore normal routine
        sta     ent_routine,x
code_A835:  rts

test_facing_change:  lda     ent_facing,x
        pha                             ; faces toward player then
        jsr     face_player                   ; tests if the facing
        pla                             ; has changed (left to right
        cmp     ent_facing,x                 ; or right to left)
        beq     LA84B
        lda     ent_flags,x                 ; if so,
        eor     #$40                    ; flip facing-lock flag
        sta     ent_flags,x                 ; to simulate proper facing
LA84B:  rts

; --- random jump velocity setup for Shadow Man (4 options) ------------------
code_A84C:  lda     $E4
        adc     $E5                     ; RNG
        sta     $E5
        and     #$03                    ; index 0-3
        tay
        lda     LA868,y                 ; Y velocity sub
        sta     ent_yvel_sub,x
        lda     LA86C,y                 ; Y velocity (jump height)
        sta     ent_yvel,x
        lda     LA870,y                 ; var2: decides slide vs blade throw
        sta     ent_var2,x
        rts

; Shadow Man jump velocity table (4 entries)
; Y vel sub, Y vel (jump height), var2 (0=blade throw, 1=slide)
LA868:  .byte   $9E,$88,$00,$88
LA86C:  .byte   $04,$06,$08,$06
LA870:  .byte   $00,$01,$00,$01
; --- spawn Shadow Blades (called in loop, spawns 2) -------------------------
code_A874:  jsr     find_enemy_freeslot_y           ; find free enemy slot
        bcs     code_A8D7               ; bail if none
        sty     L0000
        lda     ent_facing,x           ; copy facing to blade
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x             ; same X position
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sec
        sbc     #$18                    ; Y = Shadow Man Y - 24 px
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$42                    ; Shadow Blade sprite
        jsr     init_child_entity                   ; init child entity
        lda     #$80                    ; blade hitbox
        sta     ent_hitbox,y
        lda     #$C6                    ; blade AI routine
        sta     ent_routine,y
        stx     $03                     ; save X (boss slot)
        lda     $01                     ; blade index (0 or 1)
        asl     a
        tax
        lda     LA8D8,x                 ; blade Y velocity sub
        sta     ent_yvel_sub,y
        lda     LA8D9,x                 ; blade Y velocity
        sta     ent_yvel,y
        lda     LA8DC,x                 ; blade X velocity sub
        sta     ent_xvel_sub,y
        lda     LA8DD,x                 ; blade X velocity
        sta     ent_xvel,y
        ldx     $03                     ; restore boss slot
        inc     $01                     ; next blade
        lda     $01
        cmp     #$02                    ; spawned both?
        bcc     code_A874               ; loop if not
code_A8D7:  rts

; Shadow Blade velocity table (2 blades: one fast-up, one fast-forward)
; Y vel sub, Y vel, X vel sub, X vel
LA8D8:  .byte   $D2
LA8D9:  .byte   $FC,$00,$00
LA8DC:  .byte   $2E
LA8DD:  .byte   $03,$80,$04
; --- Shadow Blade projectile AI ($C6) — fly in arc, despawn offscreen -------
code_A8E0:  lda     ent_anim_id,x
        cmp     #$42                    ; initial blade anim?
        bne     code_A8FC
        lda     ent_anim_state,x
        cmp     #$06                    ; anim sequence done?
        bne     code_A929
        lda     #$43                    ; switch to spinning blade anim
        jsr     reset_sprite_anim
        lda     ent_y_px,x             ; adjust Y for new sprite frame
        clc
        adc     #$0E
        sta     ent_y_px,x
code_A8FC:  lda     #$00                ; apply Y velocity (with sign extension)
        sta     $02
        lda     ent_yvel,x
        bpl     code_A907
        dec     $02                     ; negative Y vel -> sign extend
code_A907:  lda     ent_y_sub,x
        clc
        adc     ent_yvel_sub,x          ; add Y sub-velocity
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     ent_yvel,x              ; add Y velocity
        sta     ent_y_px,x
        lda     ent_y_scr,x
        adc     $02
        bne     code_A924               ; went offscreen? despawn
        jmp     code_A68B               ; apply horizontal movement

code_A924:  lda     #$00                ; despawn blade
        sta     ent_status,x
code_A929:  rts

; =============================================================================
; SPARK MAN STAGE DATA (stage_id $22 = bank $06)
; =============================================================================
; Compressed tile/nametable data, palettes, enemy spawn tables, and
; metatile definitions for Spark Man's stage. Occupies the remainder
; of the bank from $A92A to $BFFF.
; =============================================================================

        .byte   $AA,$81,$0A,$B6,$03,$D7,$A9,$F9
        .byte   $A8,$3C,$E2,$71,$20,$F7,$22,$9B
        .byte   $A0,$ED,$AA,$32,$8A,$BD,$0A,$4F
        .byte   $AA,$55,$EA,$6B,$A0,$B2,$BA,$C7
        .byte   $90,$7F,$A8,$47,$88,$F2,$A0,$C2
        .byte   $8A,$7F,$A2,$E3,$A2,$A7,$22,$BF
        .byte   $8A,$F1,$AA,$EF,$A2,$FA,$80,$E6
        .byte   $AA,$F7,$88,$E7,$2A,$FD,$AE,$C8
        .byte   $22,$71,$80,$F9,$A2,$B6,$88,$99
        .byte   $28,$36,$2A,$98,$A2,$6E,$A2,$76
        .byte   $A9,$FF,$8A,$D3,$22,$3C,$8A,$73
        .byte   $2A,$D6,$28,$15,$09,$B0,$2E,$7F
        .byte   $A2,$E4,$EA,$7D,$0A,$51,$8A,$C5
        .byte   $80,$57,$A8,$C4,$82,$54,$A2,$EF
        .byte   $AA,$C1,$AA,$DE,$2A,$F1,$82,$66
        .byte   $8A,$10,$23,$1F,$AA,$7C,$3A,$7C
        .byte   $8A,$E7,$88,$FA,$20,$0D,$A2,$1D
        .byte   $20,$63,$A8,$FC,$88,$7D,$A8,$4B
        .byte   $80,$FD,$AA,$E5,$22,$E9,$A0,$FC
        .byte   $A2,$39,$80,$1C,$AA,$54,$A8,$AB
        .byte   $8E,$2D,$82,$7F,$8A,$33,$8A,$F7
        .byte   $AA,$DF,$02,$3F,$A0,$FC,$A3,$74
        .byte   $AA,$B7,$AA,$EF,$2A,$FF,$A8,$6A
        .byte   $A0,$BF,$A2,$BF,$08,$DE,$80,$F2
        .byte   $A2,$FF,$28,$73,$82,$38,$18,$FF
        .byte   $88,$DD,$A8,$FF,$8A,$FC,$8A,$F6
        .byte   $AA,$FF,$A2,$F3,$AA,$EC,$00,$01
        .byte   $02,$03,$04,$05,$06,$07,$08,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
        .byte   $12,$13,$14,$15,$16,$17,$20,$EF
        .byte   $28,$BF,$A2,$D5,$AA,$EF,$88,$DF
        .byte   $A0,$EE,$28,$F6,$08,$6F,$AA,$5E
        .byte   $4A,$B8,$B2,$4F,$02,$6C,$15,$24
        .byte   $30,$FE,$A8,$F1,$A8,$BE,$82,$77
        .byte   $A2,$9B,$22,$EB,$0A,$BF,$80,$A3
        .byte   $80,$A2,$80,$A1,$40,$40,$40,$62
        .byte   $63,$20,$20,$00,$88,$63,$88,$EF
        .byte   $82,$FB,$22,$7C,$AA,$E6,$8A,$6D
        .byte   $A8,$08,$A8,$FB,$0A,$FC,$08,$00
        .byte   $1A,$00,$17,$00,$03,$00,$2D,$00
        .byte   $08,$00,$00,$00,$00,$00,$00,$01
        .byte   $32,$01,$02,$00,$00,$00,$28,$00
        .byte   $00,$38,$A0,$BE,$A2,$EF,$54,$56
        .byte   $0F,$20,$32,$02,$0F,$20,$32,$26
        .byte   $0F,$20,$32,$0F,$0F,$1B,$1B,$0F
        .byte   $87,$88,$89,$00,$0F,$20,$32,$02
        .byte   $0F,$20,$32,$26,$0F,$20,$32,$0F
        .byte   $0F,$20,$10,$16,$87,$88,$00,$00
        .byte   $8A,$EE,$0A,$FE,$A8,$D3,$8A,$FD
        .byte   $A8,$B9,$AA,$91,$24,$CD,$83,$7F
        .byte   $AA,$E7,$A0,$EF,$2A,$FE,$0A,$35
        .byte   $28,$F7,$A8,$FF,$AA,$5D,$A6,$FD
        .byte   $AA,$36,$0A,$D6,$28,$7D,$AA,$EA
        .byte   $A2,$36,$8A,$F4,$23,$56,$A8,$EA
        .byte   $E8,$FC,$8A,$EE,$28,$DF,$2A,$18
        .byte   $08,$D9,$0A,$F4,$99,$D7,$A2,$DE
        .byte   $20,$FA,$82,$73,$22,$A9,$09,$0B
        .byte   $AA,$B7,$A8,$ED,$28,$4C,$0F,$16
        .byte   $FF,$4F,$AA,$DF,$AA,$BD,$00,$01
        .byte   $02,$03,$03,$03,$03,$04,$05,$06
        .byte   $07,$07,$07,$08,$08,$09,$09,$09
        .byte   $0A,$0B,$0B,$0B,$10,$10,$11,$12
        .byte   $12,$12,$12,$13,$13,$14,$14,$14
        .byte   $14,$14,$14,$15,$15,$15,$17,$FF
        .byte   $08,$5F,$88,$E1,$2A,$FD,$AA,$7F
        .byte   $02,$D0,$AA,$B7,$22,$5F,$A0,$EF
        .byte   $AA,$E8,$8A,$9F,$A8,$BC,$AA,$4B
        .byte   $2A,$5E,$08,$CC,$AA,$BB,$82,$E9
        .byte   $A0,$CE,$20,$DA,$A2,$BF,$82,$BB
        .byte   $8A,$86,$3A,$5F,$8A,$FA,$AA,$F4
        .byte   $00,$AD,$AA,$BE,$00,$6F,$88,$B9
        .byte   $00,$92,$2A,$C4,$AA,$3C,$42,$FC
        .byte   $2A,$BB,$A8,$AA,$A2,$5D,$0A,$FC
        .byte   $AA,$D7,$A8,$6C,$A8,$CF,$EA,$47
        .byte   $82,$3B,$20,$DB,$A8,$FF,$82,$9B
        .byte   $28,$CE,$2A,$F9,$02,$87,$2A,$E5
        .byte   $2A,$5D,$A8,$AF,$B8,$8E,$2A,$39
        .byte   $A8,$D3,$22,$79,$AA,$34,$AA,$F5
        .byte   $DA,$7E,$AA,$E3,$A2,$FD,$2A,$21
        .byte   $8A,$B8,$80,$DF,$AA,$FE,$28,$FD
        .byte   $AA,$F7,$AA,$5D,$2A,$7D,$80,$AF
        .byte   $0A,$F3,$02,$A9,$83,$BA,$2A,$A5
        .byte   $A8,$F6,$A8,$B0,$2A,$BF,$16,$28
        .byte   $8A,$D1,$2A,$59,$AA,$4E,$AA,$5E
        .byte   $02,$E5,$02,$2D,$88,$EB,$8E,$96
        .byte   $2A,$F7,$22,$92,$2C,$BB,$AA,$95
        .byte   $AA,$DF,$88,$59,$AE,$DF,$A2,$11
        .byte   $AA,$72,$82,$70,$AF,$89,$8A,$9F
        .byte   $8A,$BE,$A0,$95,$AE,$EF,$AA,$8A
        .byte   $A2,$37,$8A,$BE,$AA,$FF,$28,$FC
        .byte   $2A,$C3,$2A,$31,$8A,$CD,$48,$A8
        .byte   $90,$10,$68,$90,$E8,$10,$70,$F8
        .byte   $38,$68,$98,$78,$D8,$38,$68,$98
        .byte   $D0,$10,$48,$76,$50,$F0,$90,$C8
        .byte   $D8,$E8,$F8,$C8,$D8,$08,$18,$68
        .byte   $98,$B8,$F8,$08,$28,$68,$D8,$FF
        .byte   $B2,$10,$10,$00,$12,$04,$24,$40
        .byte   $84,$04,$C5,$00,$14,$00,$8B,$40
        .byte   $18,$10,$30,$11,$40,$11,$62,$10
        .byte   $00,$20,$08,$40,$0C,$01,$0E,$00
        .byte   $16,$80,$00,$40,$C8,$20,$40,$00
        .byte   $C0,$40,$02,$00,$06,$00,$84,$01
        .byte   $65,$00,$20,$00,$80,$50,$20,$40
        .byte   $2C,$10,$F0,$40,$E4,$10,$88,$11
        .byte   $08,$40,$60,$C0,$64,$11,$91,$55
        .byte   $21,$00,$20,$44,$A0,$00,$03,$05
        .byte   $69,$00,$00,$51,$C8,$51,$21,$04
        .byte   $00,$10,$00,$00,$00,$40,$00,$00
        .byte   $20,$00,$40,$00,$14,$00,$20,$05
        .byte   $08,$00,$40,$40,$00,$00,$02,$00
        .byte   $04,$00,$3B,$24,$87,$01,$84,$41
        .byte   $04,$44,$21,$14,$11,$14,$44,$14
        .byte   $00,$00,$0A,$14,$02,$41,$00,$10
        .byte   $39,$40,$45,$20,$C8,$00,$8A,$05
        .byte   $20,$00,$50,$81,$4D,$0C,$12,$00
        .byte   $60,$41,$25,$00,$B0,$00,$5C,$10
        .byte   $40,$51,$00,$10,$04,$20,$84,$00
        .byte   $07,$00,$04,$20,$11,$10,$57,$00
        .byte   $50,$00,$3C,$41,$08,$01,$0A,$41
        .byte   $29,$06,$0C,$54,$08,$04,$E2,$54
        .byte   $80,$00,$48,$00,$4C,$01,$0E,$00
        .byte   $F0,$50,$F0,$44,$87,$51,$89,$50
        .byte   $E1,$41,$C1,$41,$0F,$55,$58,$70
        .byte   $54,$54,$80,$54,$80,$54,$50,$C8
        .byte   $98,$D8,$88,$70,$70,$A4,$58,$58
        .byte   $98,$98,$98,$98,$40,$40,$40,$BC
        .byte   $BC,$BC,$BC,$B8,$48,$90,$38,$58
        .byte   $98,$38,$68,$C8,$48,$68,$00,$FF
        .byte   $A0,$40,$40,$00,$20,$00,$E1,$40
        .byte   $95,$01,$B0,$01,$C0,$40,$89,$44
        .byte   $A1,$04,$04,$04,$FE,$07,$70,$00
        .byte   $30,$10,$A0,$10,$20,$00,$01,$00
        .byte   $20,$00,$06,$01,$A0,$01,$8A,$00
        .byte   $A5,$11,$A0,$04,$80,$10,$C4,$10
        .byte   $89,$00,$C2,$00,$9C,$45,$00,$41
        .byte   $A4,$00,$45,$84,$69,$14,$08,$00
        .byte   $05,$11,$48,$14,$83,$01,$28,$05
        .byte   $60,$15,$E2,$10,$1A,$21,$81,$55
        .byte   $08,$00,$A3,$21,$CD,$44,$09,$10
        .byte   $01,$04,$22,$00,$00,$00,$00,$00
        .byte   $00,$00,$A1,$04,$25,$01,$00,$00
        .byte   $0D,$04,$81,$00,$54,$01,$83,$00
        .byte   $00,$00,$3E,$00,$CA,$40,$50,$14
        .byte   $00,$00,$0C,$15,$88,$04,$21,$40
        .byte   $18,$00,$09,$44,$66,$40,$C0,$10
        .byte   $80,$40,$22,$02,$30,$91,$12,$00
        .byte   $A4,$20,$82,$50,$84,$40,$00,$20
        .byte   $04,$03,$01,$54,$00,$10,$20,$01
        .byte   $20,$00,$28,$10,$05,$00,$10,$00
        .byte   $68,$18,$02,$00,$45,$00,$51,$00
        .byte   $20,$02,$B0,$00,$48,$01,$DA,$10
        .byte   $96,$55,$A9,$55,$4C,$10,$B5,$05
        .byte   $E9,$00,$C5,$15,$32,$14,$D0,$00
        .byte   $59,$40,$9D,$04,$FD,$04,$87,$00
        .byte   $7A,$44,$11,$04,$28,$00,$36,$64
        .byte   $24,$24,$65,$24,$65,$24,$03,$2D
        .byte   $2D,$2D,$2D,$64,$64,$12,$50,$52
        .byte   $36,$36,$36,$36,$63,$63,$63,$53
        .byte   $53,$53,$53,$2D,$20,$2D,$20,$20
        .byte   $2D,$20,$20,$2D,$20,$20,$4D,$FF
        .byte   $40,$04,$01,$41,$48,$04,$0C,$40
        .byte   $00,$40,$43,$04,$11,$44,$A0,$05
        .byte   $00,$04,$01,$04,$69,$40,$C0,$01
        .byte   $04,$00,$10,$41,$20,$11,$08,$00
        .byte   $48,$00,$00,$05,$80,$00,$02,$00
        .byte   $40,$00,$10,$41,$20,$00,$44,$40
        .byte   $11,$00,$18,$00,$C4,$41,$80,$80
        .byte   $22,$15,$08,$04,$2A,$10,$64,$15
        .byte   $00,$00,$60,$00,$88,$60,$14,$00
        .byte   $80,$50,$5D,$00,$A0,$44,$26,$40
        .byte   $13,$01,$4A,$05,$78,$C8,$00,$00
        .byte   $19,$41,$40,$81,$00,$14,$10,$05
        .byte   $E0,$01,$92,$01,$04,$00,$4C,$00
        .byte   $88,$40,$40,$10,$85,$40,$68,$00
        .byte   $82,$00,$12,$50,$0F,$00,$80,$42
        .byte   $01,$00,$55,$50,$08,$01,$80,$50
        .byte   $80,$05,$20,$10,$81,$20,$10,$51
        .byte   $22,$04,$8F,$00,$4D,$50,$A2,$01
        .byte   $04,$40,$80,$80,$65,$80,$C0,$20
        .byte   $00,$00,$82,$00,$00,$04,$11,$00
        .byte   $24,$10,$15,$00,$80,$40,$09,$44
        .byte   $02,$04,$00,$10,$80,$11,$CD,$44
        .byte   $00,$00,$80,$00,$65,$40,$22,$19
        .byte   $70,$41,$61,$01,$E2,$69,$A2,$00
        .byte   $49,$50,$EA,$10,$74,$45,$84,$44
        .byte   $95,$14,$5D,$00,$B1,$40,$0A,$50
        .byte   $20,$05,$A4,$16,$5F,$00,$00,$01
        .byte   $02,$03,$03,$03,$03,$03,$00,$04
        .byte   $05,$06,$07,$07,$07,$08,$00,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
        .byte   $11,$11,$11,$12,$13,$0F,$14,$15
        .byte   $16,$0E,$0E,$17,$18,$0F,$19,$1A
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$1B,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1C,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$1D,$07
        .byte   $07,$1E,$07,$07,$07,$07,$00,$0E
        .byte   $0E,$1F,$14,$20,$14,$15,$00,$21
        .byte   $22,$23,$24,$25,$24,$26,$00,$27
        .byte   $28,$29,$19,$2A,$19,$1A,$00,$0E
        .byte   $2B,$1B,$1B,$1B,$1B,$2C,$00,$2D
        .byte   $02,$03,$03,$03,$03,$2E,$00,$01
        .byte   $02,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$2F,$03,$03,$03,$07,$07
        .byte   $07,$07,$2F,$07,$07,$07,$0E,$30
        .byte   $30,$30,$31,$32,$32,$32,$0E,$33
        .byte   $33,$34,$35,$36,$37,$38,$0E,$0E
        .byte   $39,$3A,$09,$0A,$3B,$3C,$3D,$0C
        .byte   $0D,$33,$0E,$3E,$38,$0E,$1B,$1B
        .byte   $1B,$1B,$1B,$3F,$2C,$0E,$03,$03
        .byte   $03,$03,$03,$03,$00,$0E,$2F,$03
        .byte   $03,$03,$2F,$03,$03,$03,$2F,$07
        .byte   $07,$07,$2F,$07,$07,$07,$31,$32
        .byte   $32,$32,$31,$32,$32,$32,$0E,$40
        .byte   $36,$37,$0E,$38,$38,$37,$0E,$41
        .byte   $42,$43,$3C,$17,$0E,$44,$0E,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$38,$45,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1B,$0F,$03
        .byte   $03,$03,$03,$03,$03,$03,$2F,$03
        .byte   $03,$03,$03,$03,$46,$0F,$2F,$07
        .byte   $07,$07,$07,$07,$47,$0F,$31,$30
        .byte   $30,$32,$32,$0E,$01,$0F,$0E,$34
        .byte   $48,$2C,$37,$0E,$0E,$0F,$49,$3A
        .byte   $4A,$00,$3B,$4B,$4C,$1C,$0E,$33
        .byte   $0F,$2E,$1B,$1C,$03,$03,$1B,$1B
        .byte   $1C,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$4D,$4E
        .byte   $4F,$4F,$4F,$4F,$4F,$50,$4D,$51
        .byte   $20,$14,$14,$20,$14,$52,$4D,$53
        .byte   $2A,$19,$19,$2A,$19,$52,$4D,$24
        .byte   $25,$54,$24,$25,$24,$52,$4D,$19
        .byte   $55,$56,$57,$2A,$19,$52,$58,$59
        .byte   $5A,$5B,$58,$57,$24,$52,$5B,$5B
        .byte   $5B,$5B,$5B,$58,$5C,$52,$5B,$5B
        .byte   $5B,$5B,$5B,$5B,$5D,$52,$5E,$5F
        .byte   $5E,$5F,$5E,$1D,$07,$07,$60,$61
        .byte   $60,$61,$60,$00,$62,$62,$63,$64
        .byte   $63,$64,$63,$00,$0E,$65,$1D,$07
        .byte   $66,$07,$07,$67,$28,$68,$00,$15
        .byte   $14,$15,$14,$15,$0E,$17,$00,$26
        .byte   $24,$26,$24,$26,$0C,$0D,$00,$69
        .byte   $1B,$1B,$1B,$1B,$2C,$0E,$00,$6A
        .byte   $03,$03,$03,$03,$00,$0E,$07,$07
        .byte   $07,$07,$07,$07,$07,$6B,$62,$62
        .byte   $62,$62,$62,$62,$62,$6C,$30,$0E
        .byte   $32,$6D,$0E,$65,$30,$6E,$6F,$0E
        .byte   $70,$71,$72,$73,$74,$75,$33,$0E
        .byte   $76,$77,$0E,$78,$79,$7A,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$45,$1B,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0F,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0F,$03,$03,$03
        .byte   $03,$03,$03,$03,$7B,$0F,$07,$07
        .byte   $07,$07,$07,$07,$47,$0F,$6E,$0E
        .byte   $0E,$41,$42,$0E,$01,$0F,$79,$65
        .byte   $0E,$7C,$7D,$0E,$0E,$0F,$28,$7E
        .byte   $3D,$0C,$0D,$0E,$0E,$0F,$2C,$0E
        .byte   $45,$2C,$0E,$45,$1B,$1C,$00,$0E
        .byte   $0F,$00,$0E,$0F,$03,$03,$00,$0E
        .byte   $0F,$00,$0E,$0F,$03,$03,$4D,$7F
        .byte   $80,$4F,$4F,$4F,$4F,$50,$4D,$7F
        .byte   $4D,$15,$14,$14,$1F,$52,$4D,$7F
        .byte   $4D,$1A,$19,$19,$29,$52,$4D,$4E
        .byte   $81,$82,$82,$83,$23,$52,$4D,$84
        .byte   $85,$86,$87,$17,$23,$52,$4D,$1A
        .byte   $0A,$0B,$0C,$0D,$29,$52,$58,$59
        .byte   $59,$59,$59,$59,$5C,$52,$5B,$5B
        .byte   $5B,$5B,$5B,$5B,$5D,$52,$5E,$5F
        .byte   $5E,$5F,$5E,$5F,$5E,$5F,$63,$64
        .byte   $63,$64,$63,$64,$63,$64,$1D,$07
        .byte   $88,$07,$07,$07,$07,$07,$00,$16
        .byte   $32,$32,$89,$32,$32,$0E,$00,$42
        .byte   $43,$8A,$8A,$8B,$76,$0E,$00,$0A
        .byte   $0B,$8C,$0B,$8C,$45,$1B,$00,$69
        .byte   $1B,$1B,$1B,$1B,$1C,$03,$00,$6A
        .byte   $03,$03,$03,$03,$03,$03,$5E,$5F
        .byte   $5E,$5F,$5E,$5F,$5E,$5F,$63,$64
        .byte   $63,$64,$63,$64,$63,$64,$07,$07
        .byte   $07,$07,$07,$08,$03,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0F,$03,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0F,$03,$03,$1B,$1B
        .byte   $1B,$2C,$0E,$0F,$03,$03,$03,$03
        .byte   $03,$00,$0E,$0F,$03,$03,$03,$03
        .byte   $8D,$8E,$0E,$0F,$03,$03,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$19,$19,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$8F,$8F,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$03,$03
        .byte   $00,$19,$19,$0F,$03,$03,$03,$03
        .byte   $00,$8F,$8F,$0F,$03,$03,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$03,$03
        .byte   $00,$19,$19,$0F,$03,$03,$03,$03
        .byte   $00,$8F,$8F,$0F,$03,$03,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$19,$19,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$24,$24,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$19,$19,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$8F,$8F,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$5F,$1D,$07
        .byte   $67,$19,$19,$6C,$90,$91,$00,$14
        .byte   $1F,$24,$24,$14,$1F,$14,$00,$92
        .byte   $93,$92,$92,$92,$93,$92,$00,$24
        .byte   $23,$24,$24,$24,$23,$24,$00,$24
        .byte   $23,$24,$24,$24,$23,$24,$2E,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1B,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$5E,$03
        .byte   $2F,$03,$5F,$5E,$03,$2F,$94,$07
        .byte   $2F,$07,$91,$94,$07,$2F,$14,$95
        .byte   $96,$97,$14,$1F,$95,$96,$92,$98
        .byte   $0E,$99,$92,$93,$98,$0E,$24,$9A
        .byte   $0E,$9B,$24,$23,$9A,$0E,$45,$2C
        .byte   $9C,$45,$1B,$1B,$2C,$9C,$1C,$00
        .byte   $9C,$0F,$03,$03,$00,$9D,$03,$00
        .byte   $9C,$0F,$03,$03,$00,$9C,$03,$5F
        .byte   $5E,$03,$2F,$03,$5F,$5E,$07,$91
        .byte   $94,$07,$2F,$07,$91,$9E,$97,$14
        .byte   $1F,$95,$96,$97,$14,$0F,$99,$92
        .byte   $93,$98,$0E,$99,$92,$0F,$9B,$24
        .byte   $23,$9A,$0E,$9B,$24,$0F,$45,$1B
        .byte   $1B,$2C,$9C,$45,$9F,$0F,$0F,$03
        .byte   $03,$00,$9C,$0F,$7B,$0F,$0F,$03
        .byte   $03,$00,$9D,$0F,$A0,$0F,$1D,$07
        .byte   $07,$10,$11,$A1,$A2,$A3,$00,$15
        .byte   $14,$14,$15,$0E,$0E,$0F,$00,$A4
        .byte   $92,$92,$A4,$0E,$45,$1C,$00,$26
        .byte   $24,$45,$1B,$1B,$1C,$03,$00,$A4
        .byte   $92,$6C,$07,$07,$08,$1D,$00,$26
        .byte   $24,$14,$15,$0E,$A5,$8E,$A6,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1B,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$8D,$A7,$A7,$A7,$A7,$03,$03
        .byte   $1D,$67,$95,$89,$0E,$0E,$03,$1D
        .byte   $67,$A8,$A9,$AA,$AB,$22,$1D,$67
        .byte   $14,$24,$AC,$3B,$AD,$28,$67,$AE
        .byte   $92,$92,$3E,$38,$0E,$0E,$0E,$9B
        .byte   $24,$45,$AF,$0E,$0E,$0E,$1B,$1B
        .byte   $1B,$1C,$00,$0E,$0E,$0E,$03,$03
        .byte   $03,$03,$00,$0E,$0E,$0E,$A7,$A7
        .byte   $A7,$A7,$A7,$A7,$A7,$A7,$0E,$6E
        .byte   $32,$89,$32,$89,$0E,$79,$B0,$B1
        .byte   $AA,$B2,$76,$B3,$AB,$22,$68,$B4
        .byte   $76,$0E,$0E,$44,$AD,$28,$0E,$B5
        .byte   $0E,$32,$0E,$38,$0E,$0E,$0E,$B6
        .byte   $0E,$B7,$0E,$0E,$B8,$0E,$0E,$B6
        .byte   $0E,$B9,$0E,$0E,$BA,$0E,$0E,$B6
        .byte   $0E,$B9,$0E,$0E,$BA,$0E,$A7,$A7
        .byte   $A7,$A7,$A7,$BB,$03,$03,$79,$6E
        .byte   $97,$14,$14,$6C,$08,$03,$B0,$BC
        .byte   $BD,$19,$19,$BE,$6C,$08,$68,$B4
        .byte   $9B,$24,$24,$26,$14,$6C,$0E,$33
        .byte   $BD,$19,$19,$1A,$19,$BF,$0E,$0E
        .byte   $45,$2C,$24,$26,$24,$C0,$0E,$0E
        .byte   $0F,$2E,$1B,$1B,$1B,$1B,$0E,$0E
        .byte   $0F,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$07,$07
        .byte   $07,$07,$07,$C1,$C2,$C3,$C4,$15
        .byte   $14,$14,$14,$15,$19,$C5,$C6,$26
        .byte   $24,$24,$24,$26,$C7,$C8,$1B,$1B
        .byte   $1B,$1B,$1B,$1B,$1C,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$C9,$07
        .byte   $07,$07,$07,$07,$07,$CA,$CB,$15
        .byte   $0E,$0E,$32,$32,$1F,$CC,$CD,$1A
        .byte   $41,$42,$CE,$CF,$29,$D0,$D1,$26
        .byte   $D2,$D3,$3B,$D4,$23,$D5,$D6,$1A
        .byte   $0E,$2B,$D7,$38,$29,$D0,$D8,$D9
        .byte   $DA,$DB,$DC,$DD,$23,$D5,$03,$03
        .byte   $03,$03,$03,$DC,$1B,$DB,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$1F,$2E
        .byte   $17,$2A,$00,$07,$00,$07,$00,$2C
        .byte   $00,$28,$1F,$1E,$17,$16,$00,$07
        .byte   $A6,$07,$00,$2C,$00,$30,$08,$09
        .byte   $35,$31,$1F,$1E,$35,$31,$1F,$1E
        .byte   $3D,$16,$A2,$A3,$AA,$00,$A3,$A4
        .byte   $00,$AC,$A5,$B6,$00,$BE,$B6,$B7
        .byte   $BF,$BE,$A8,$A9,$00,$00,$00,$00
        .byte   $00,$00,$28,$1E,$2C,$16,$1F,$3E
        .byte   $35,$31,$21,$25,$35,$31,$21,$26
        .byte   $35,$32,$06,$00,$07,$00,$74,$74
        .byte   $6C,$6C,$74,$75,$6C,$6D,$00,$00
        .byte   $00,$A7,$00,$00,$A0,$A1,$07,$00
        .byte   $07,$00,$64,$64,$6C,$6C,$64,$65
        .byte   $6C,$6D,$21,$25,$17,$16,$3B,$1E
        .byte   $17,$16,$1F,$1E,$17,$38,$0A,$0B
        .byte   $35,$31,$73,$74,$6B,$6C,$75,$74
        .byte   $6D,$6C,$00,$A6,$A5,$A2,$00,$00
        .byte   $A3,$A3,$6B,$6C,$6B,$6C,$6C,$6C
        .byte   $6C,$6C,$6D,$6C,$6D,$6C,$6C,$6D
        .byte   $6C,$6D,$A5,$A2,$00,$AA,$A3,$A3
        .byte   $00,$00,$63,$64,$6B,$6C,$65,$64
        .byte   $6D,$6C,$00,$24,$00,$28,$21,$26
        .byte   $17,$2A,$00,$06,$00,$07,$1F,$3E
        .byte   $17,$16,$14,$15,$14,$15,$B0,$00
        .byte   $B8,$00,$1A,$1B,$00,$00,$00,$B0
        .byte   $00,$B8,$B1,$00,$00,$00,$B8,$00
        .byte   $B8,$00,$00,$00,$A6,$00,$00,$B1
        .byte   $00,$A7,$00,$B8,$00,$B8,$00,$B1
        .byte   $00,$00,$00,$BC,$A0,$A1,$B7,$BA
        .byte   $B8,$00,$B9,$B7,$00,$B8,$BD,$00
        .byte   $00,$00,$B6,$B6,$BE,$BF,$20,$26
        .byte   $2C,$2A,$3B,$3E,$17,$16,$00,$B1
        .byte   $A6,$00,$A2,$A3,$A2,$A3,$A3,$A4
        .byte   $A3,$A4,$B9,$B7,$00,$00,$BC,$B7
        .byte   $00,$B8,$20,$25,$2C,$16,$2A,$07
        .byte   $2A,$07,$2A,$07,$36,$07,$B1,$24
        .byte   $00,$28,$BA,$B9,$00,$00,$A5,$2C
        .byte   $24,$3B,$BD,$00,$24,$21,$20,$25
        .byte   $3F,$16,$17,$2A,$1F,$2E,$07,$28
        .byte   $07,$34,$17,$16,$31,$35,$17,$16
        .byte   $39,$1E,$07,$74,$07,$6C,$2C,$16
        .byte   $28,$1E,$07,$64,$07,$6C,$24,$22
        .byte   $28,$2A,$24,$21,$28,$1E,$3F,$3A
        .byte   $1F,$1E,$25,$22,$1F,$2A,$17,$3A
        .byte   $1F,$1E,$25,$21,$1F,$1E,$3F,$16
        .byte   $1F,$1E,$17,$16,$1F,$1E,$26,$06
        .byte   $2A,$07,$2E,$07,$2A,$07,$1F,$1E
        .byte   $41,$16,$1F,$1E,$17,$40,$49,$1E
        .byte   $41,$16,$1F,$48,$17,$40,$66,$6E
        .byte   $00,$00,$49,$1E,$17,$16,$1F,$48
        .byte   $17,$16,$00,$00,$A7,$00,$0C,$0B
        .byte   $35,$31,$1F,$2E,$35,$32,$A4,$B9
        .byte   $AC,$00,$06,$24,$07,$28,$07,$2C
        .byte   $07,$28,$1F,$1E,$39,$16,$28,$1E
        .byte   $34,$31,$00,$00,$00,$A6,$00,$00
        .byte   $B0,$00,$B7,$00,$B8,$00,$BC,$B7
        .byte   $BC,$B7,$BA,$A2,$BA,$A2,$A3,$A3
        .byte   $A3,$A3,$A4,$B9,$A4,$B9,$B7,$BD
        .byte   $B7,$BD,$B8,$00,$B1,$A6,$00,$B8
        .byte   $00,$B1,$00,$AA,$00,$00,$AC,$00
        .byte   $00,$00,$B8,$00,$B1,$00,$00,$A2
        .byte   $00,$AA,$2A,$07,$2E,$07,$AA,$00
        .byte   $00,$00,$00,$AC,$A0,$A1,$A4,$A5
        .byte   $AC,$00,$07,$28,$07,$2C,$17,$16
        .byte   $1F,$3C,$17,$3A,$31,$35,$25,$21
        .byte   $31,$35,$25,$22,$31,$36,$6C,$75
        .byte   $6C,$6D,$00,$A7,$A3,$A4,$B1,$B8
        .byte   $00,$B1,$B1,$B1,$00,$00,$0D,$0B
        .byte   $35,$31,$00,$00,$00,$B0,$BB,$B7
        .byte   $00,$00,$BD,$B8,$00,$B1,$B6,$B7
        .byte   $BF,$BF,$1F,$3C,$17,$2A,$31,$36
        .byte   $00,$00,$6C,$6C,$67,$67,$08,$0E
        .byte   $35,$31,$1F,$48,$35,$31,$6C,$6C
        .byte   $64,$64,$6B,$6C,$63,$64,$49,$1E
        .byte   $35,$31,$75,$00,$6D,$00,$1C,$1D
        .byte   $00,$00,$00,$73,$00,$6B,$6D,$00
        .byte   $65,$00,$00,$6B,$00,$63,$6D,$00
        .byte   $6D,$00,$00,$6B,$00,$6B,$12,$13
        .byte   $10,$11,$12,$12,$10,$11,$49,$1E
        .byte   $3D,$16,$22,$06,$2E,$07,$2E,$07
        .byte   $2E,$07,$3B,$1E,$35,$31,$2A,$00
        .byte   $36,$00,$28,$1E,$28,$16,$6C,$6D
        .byte   $64,$65,$30,$35,$00,$00,$1F,$3A
        .byte   $17,$16,$31,$35,$6E,$66,$74,$74
        .byte   $64,$64,$6D,$A7,$65,$A4,$00,$B8
        .byte   $B9,$B7,$00,$A6,$BA,$A2,$6D,$A4
        .byte   $6D,$AC,$BA,$A2,$00,$AA,$00,$73
        .byte   $00,$63,$3B,$2E,$17,$2A,$A7,$00
        .byte   $A4,$B9,$B8,$00,$B7,$BA,$00,$B8
        .byte   $BD,$B1,$00,$B8,$BC,$B7,$B7,$BD
        .byte   $B8,$00,$B1,$00,$00,$90,$00,$98
        .byte   $00,$98,$90,$B8,$98,$B1,$90,$90
        .byte   $98,$98,$98,$00,$98,$00,$98,$98
        .byte   $98,$98,$39,$1E,$2C,$16,$B8,$00
        .byte   $B7,$BD,$00,$63,$00,$6B,$74,$74
        .byte   $6C,$6D,$74,$0F,$6C,$0F,$6C,$0F
        .byte   $6C,$0F,$1F,$3C,$35,$32,$31,$35
        .byte   $74,$74,$31,$35,$74,$0F,$02,$74
        .byte   $03,$6C,$64,$0F,$6C,$0F,$03,$6C
        .byte   $03,$6C,$6C,$6C,$24,$21,$6C,$0F
        .byte   $25,$21,$1F,$1E,$3C,$31,$1F,$1E
        .byte   $35,$39,$2A,$74,$2E,$6C,$74,$2C
        .byte   $6C,$28,$2A,$64,$2E,$6C,$B9,$B7
        .byte   $B9,$B7,$BB,$B7,$BB,$B7,$64,$2C
        .byte   $6C,$28,$32,$6C,$02,$6C,$A2,$A3
        .byte   $00,$00,$A3,$A4,$00,$00,$BB,$B7
        .byte   $00,$B8,$6C,$2C,$6C,$28,$01,$64
        .byte   $03,$6C,$22,$B1,$2E,$00,$03,$6C
        .byte   $25,$21,$6C,$6D,$25,$21,$00,$24
        .byte   $25,$3B,$21,$3F,$17,$16,$3A,$25
        .byte   $17,$16,$22,$00,$2E,$00,$00,$91
        .byte   $00,$00,$00,$00,$91,$00,$C0,$85
        .byte   $85,$80,$86,$87,$81,$88,$8D,$8E
        .byte   $82,$80,$8F,$C7,$81,$84,$85,$86
        .byte   $80,$81,$82,$88,$88,$82,$89,$82
        .byte   $80,$81,$80,$81,$88,$89,$82,$89
        .byte   $82,$80,$12,$13,$12,$12,$12,$13
        .byte   $13,$12,$12,$12,$13,$12,$90,$00
        .byte   $98,$00,$00,$90,$00,$98,$00,$00
        .byte   $00,$90,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$23
        .byte   $00,$33,$04,$0E,$0E,$0E,$27,$2A
        .byte   $27,$29,$27,$27,$2D,$1E,$06,$08
        .byte   $84,$86,$0A,$0C,$40,$42,$30,$20
        .byte   $1A,$1C,$0A,$0C,$60,$62,$44,$4D
        .byte   $46,$44,$44,$4D,$46,$44,$48,$00
        .byte   $4A,$48,$48,$00,$4A,$48,$64,$4E
        .byte   $66,$00,$64,$4E,$66,$00,$68,$4E
        .byte   $4A,$7A,$68,$4E,$4A,$7A,$80,$82
        .byte   $88,$8A,$11,$13,$8C,$8E,$A0,$A2
        .byte   $A8,$AA,$10,$10,$AC,$AE,$C1,$82
        .byte   $06,$16,$08,$09,$00,$00,$35,$00
        .byte   $06,$06,$C5,$00,$9D,$00,$44,$4D
        .byte   $46,$22,$23,$23,$6C,$33,$48,$00
        .byte   $4A,$32,$33,$33,$6C,$08,$64,$4E
        .byte   $66,$00,$00,$00,$33,$9D,$68,$4E
        .byte   $4A,$7A,$8C,$00,$8C,$00,$CC,$CE
        .byte   $CA,$00,$00,$00,$00,$00,$EC,$EE
        .byte   $00,$00,$EB,$00,$00,$00,$11,$11
        .byte   $01,$01,$01,$01,$01,$01,$C5,$01
        .byte   $01,$01,$01,$01,$01,$01,$00,$00
        .byte   $AC,$C8,$C7,$C3,$00,$00,$F1,$E3
        .byte   $00,$01,$F7,$01,$01,$01,$E5,$4C
        .byte   $01,$01,$01,$01,$20,$8E,$E0,$C3
        .byte   $AD,$AD,$00,$AD,$AE,$B4,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$23
        .byte   $00,$33,$05,$0F,$0F,$0F,$28,$2F
        .byte   $2A,$2F,$2B,$2C,$2E,$1F,$07,$09
        .byte   $85,$87,$0B,$0D,$41,$43,$21,$31
        .byte   $1B,$1D,$0B,$0D,$61,$63,$45,$4D
        .byte   $47,$47,$45,$4D,$47,$47,$49,$00
        .byte   $4B,$4B,$49,$00,$4B,$4B,$65,$4F
        .byte   $67,$00,$65,$4F,$67,$00,$4F,$6B
        .byte   $79,$49,$4F,$6B,$79,$49,$81,$83
        .byte   $89,$8B,$11,$10,$8D,$8F,$A1,$A3
        .byte   $A9,$AB,$10,$14,$AD,$AF,$81,$C0
        .byte   $07,$17,$08,$09,$00,$00,$00,$36
        .byte   $06,$06,$C6,$00,$9D,$00,$45,$4D
        .byte   $47,$23,$23,$24,$6D,$33,$49,$00
        .byte   $4B,$33,$33,$34,$6D,$08,$65,$4F
        .byte   $67,$00,$00,$00,$9D,$33,$4F,$6B
        .byte   $79,$49,$00,$8D,$00,$8D,$CD,$CF
        .byte   $CB,$00,$00,$00,$00,$00,$ED,$EF
        .byte   $00,$EA,$00,$00,$00,$00,$12,$12
        .byte   $01,$01,$01,$01,$01,$01,$C6,$01
        .byte   $01,$01,$01,$01,$01,$01,$00,$00
        .byte   $C7,$C9,$AD,$C3,$00,$00,$E2,$E4
        .byte   $F7,$01,$00,$01,$01,$01,$E6,$5F
        .byte   $01,$01,$01,$01,$21,$8F,$C2,$AC
        .byte   $C3,$AC,$AC,$00,$AF,$B5,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$33
        .byte   $33,$33,$0E,$25,$0E,$0E,$37,$3A
        .byte   $37,$39,$37,$37,$3D,$1E,$16,$18
        .byte   $94,$96,$0A,$0C,$50,$52,$30,$20
        .byte   $00,$00,$1A,$1C,$70,$72,$54,$5C
        .byte   $56,$54,$54,$5C,$56,$54,$48,$00
        .byte   $5A,$48,$48,$00,$5A,$48,$74,$5E
        .byte   $76,$00,$74,$5E,$76,$00,$5A,$6A
        .byte   $78,$5C,$5A,$6A,$78,$5C,$90,$92
        .byte   $98,$9A,$10,$13,$9C,$9E,$B0,$B2
        .byte   $B8,$BA,$12,$10,$BC,$BE,$D1,$92
        .byte   $06,$16,$18,$19,$00,$00,$35,$00
        .byte   $06,$06,$D5,$00,$00,$9C,$01,$01
        .byte   $01,$32,$33,$33,$7C,$15,$01,$00
        .byte   $01,$32,$33,$33,$7C,$18,$01,$01
        .byte   $01,$32,$33,$33,$8C,$00,$01,$01
        .byte   $01,$01,$8C,$00,$33,$9C,$DC,$DE
        .byte   $DA,$00,$FB,$00,$E8,$F8,$FC,$FE
        .byte   $00,$00,$FB,$EA,$E9,$F9,$D5,$13
        .byte   $01,$01,$01,$01,$01,$01,$D5,$01
        .byte   $01,$01,$01,$01,$01,$01,$E1,$D3
        .byte   $BC,$D8,$D7,$C3,$00,$E7,$F0,$F3
        .byte   $00,$01,$00,$01,$01,$01,$58,$F5
        .byte   $01,$01,$01,$01,$30,$9E,$E0,$C3
        .byte   $BD,$BD,$00,$BD,$BE,$B6,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$33
        .byte   $33,$33,$0F,$26,$0F,$0F,$38,$3F
        .byte   $3A,$3F,$3B,$3C,$3E,$1F,$17,$19
        .byte   $95,$97,$0B,$0D,$51,$53,$21,$31
        .byte   $00,$00,$1B,$1D,$71,$73,$55,$5D
        .byte   $57,$57,$55,$5D,$57,$57,$59,$00
        .byte   $4B,$4B,$59,$00,$4B,$4B,$75,$5E
        .byte   $77,$00,$75,$5E,$77,$00,$69,$59
        .byte   $5D,$7B,$69,$59,$5D,$7B,$91,$93
        .byte   $99,$9B,$10,$10,$9D,$9F,$B1,$B3
        .byte   $B9,$BB,$12,$14,$BD,$BF,$91,$D0
        .byte   $07,$17,$18,$19,$00,$00,$00,$36
        .byte   $06,$7F,$D6,$00,$00,$9C,$55,$5D
        .byte   $57,$33,$33,$34,$7D,$15,$59,$00
        .byte   $4B,$33,$33,$34,$7D,$18,$75,$5E
        .byte   $77,$33,$33,$34,$00,$8D,$69,$59
        .byte   $5D,$7B,$00,$8D,$9C,$33,$DD,$DF
        .byte   $DB,$FA,$00,$EA,$E9,$F9,$FD,$FF
        .byte   $00,$FA,$00,$E8,$F8,$00,$D6,$14
        .byte   $01,$01,$01,$01,$01,$01,$D6,$01
        .byte   $01,$01,$01,$01,$01,$01,$D2,$D4
        .byte   $D7,$D9,$BD,$C3,$E7,$00,$F2,$F4
        .byte   $00,$01,$00,$01,$01,$01,$5B,$F6
        .byte   $01,$01,$01,$01,$31,$9F,$C2,$BC
        .byte   $C3,$BC,$BC,$00,$BF,$B7,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$10
        .byte   $10,$10,$40,$20,$40,$20,$11,$12
        .byte   $11,$12,$11,$11,$12,$10,$03,$03
        .byte   $13,$13,$10,$10,$10,$10,$03,$03
        .byte   $10,$10,$10,$10,$10,$10,$11,$11
        .byte   $11,$11,$12,$12,$12,$12,$11,$11
        .byte   $11,$11,$12,$12,$12,$12,$11,$11
        .byte   $11,$00,$12,$12,$12,$00,$11,$11
        .byte   $11,$11,$12,$12,$12,$12,$11,$12
        .byte   $50,$50,$10,$10,$00,$00,$10,$10
        .byte   $51,$51,$10,$10,$00,$00,$11,$12
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$00,$13,$13
        .byte   $13,$00,$00,$00,$51,$00,$13,$13
        .byte   $13,$00,$00,$00,$52,$00,$13,$13
        .byte   $13,$00,$00,$00,$00,$00,$13,$13
        .byte   $13,$13,$00,$00,$00,$00,$63,$63
        .byte   $63,$63,$63,$63,$63,$63,$63,$63
        .byte   $00,$63,$63,$63,$63,$63,$10,$10
        .byte   $00,$00,$00,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$00,$03,$00,$00,$00,$03,$03
        .byte   $00,$00,$00,$00,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00
