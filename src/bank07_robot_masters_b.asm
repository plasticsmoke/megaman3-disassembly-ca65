; =============================================================================
; MEGA MAN 3 (U) — BANK $07 — ROBOT MASTER AI (HARD/SPARK/SNAKE/GEMINI)
; =============================================================================
; Mapped to $A000-$BFFF. Contains boss AI routines dispatched from bank1C_1D
; for routine indices $D0-$D9. Entry points: main_hard_man_j, main_spark_man_j,
; main_snake_man_j, main_gemini_man_j. Also doubles as Shadow Man stage
; data ($22=$07).
;
; Routine index mapping (from bank1C_1D dispatch):
;   $D0 → main_hard_man_j    (Hard Man boss AI)
;   $D1 → hard_man_fist_main           (Hard Man fist projectile)
;   $D2 → main_spark_man_j   (Spark Man boss AI)
;   $D3 → (unused — 3x NOP)
;   $D4 → main_snake_man_j   (Snake Man boss AI)
;   $D5 → (unused — 3x NOP)
;   $D6 → main_gemini_man_j  (Gemini Man boss AI)
;   $D7 → gemini_man_clone_main           (Gemini Man clone AI)
;   $D8 → stub RTS
; =============================================================================
main_hard_man_j:

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"

entity_ai_dispatch           := $8003
move_right_collide           := $F580
move_left_collide           := $F5C4
move_down_collide           := $F606
move_vertical_gravity           := $F67C
move_sprite_right           := $F71D
move_sprite_left           := $F73B
move_sprite_down           := $F759
move_sprite_up           := $F779
reset_sprite_anim           := $F835
init_child_entity           := $F846
face_player           := $F869
set_sprite_hflip           := $F883
submit_sound_ID           := $F89A
entity_x_dist_to_player           := $F8C2
find_enemy_freeslot_y           := $FC53
calc_homing_velocity           := $FC63

.segment "BANK07"

; --- Entry trampolines (dispatched from bank1C_1D entity AI) ----------------
        jmp     hard_man_state_dispatch               ; $D0 — Hard Man boss AI
                                        ; $D1 — Hard Man fist projectile
        jmp     hard_man_fist_main
main_spark_man_j:
                                        ; $D2 — Spark Man boss AI
        jmp     spark_man_state_dispatch
                                        ; $D3 — unused
        nop
        nop
        nop
main_snake_man_j:
        jmp     snake_man_state_dispatch               ; $D4 — Snake Man boss AI
                                        ; $D5 — unused
        nop
        nop
        nop
main_gemini_man_j:
        jmp     main_gemini_man         ; $D6 — Gemini Man boss AI
                                        ; $D7 — Gemini Man clone AI
        jmp     gemini_man_clone_main
                                        ; $D8 — stub RTS
        rts

; =============================================================================
; HARD MAN AI ($D0)
; =============================================================================
; State machine with 5 phases:
;   Phase 0 ($A036): init — clear vars, set invincibility flag
;   Phase 1 ($A052): walk toward player, fire fist projectiles at anim frames
;   Phase 2 ($A0B2): jumping / descending toward player
;   Phase 3 ($A166): screen-shaking landing with stun
;   Phase 4 ($A1CF): cooldown, then reset to phase 1
; =============================================================================

hard_man_state_dispatch:  lda     ent_status,x        ; --- state dispatch ---
        and     #$0F                    ; isolate phase number (low nibble)
        tay                             ; use as table index
        lda     hard_man_state_ptrs_low_table,y ; load state handler address low byte
        sta     temp_00                 ; store jump target low byte
        lda     hard_man_state_ptrs_high_table,y ; load state handler address high byte
        sta     $01                     ; store jump target high byte
        jmp     (temp_00)               ; indirect jump to current phase

; Hard Man state pointer table (low/high bytes)
hard_man_state_ptrs_low_table:  .byte   $36,$52,$B2,$66,$CF ; low bytes for phases 0-4
hard_man_state_ptrs_high_table:  .byte   $A0,$A0,$A0,$A1,$A1 ; high bytes for phases 0-4
; --- phase 0: init ---
        lda     #$00                    ; zero
        sta     ent_timer,x             ; clear timer
        sta     ent_var1,x              ; clear shot counter
        sta     ent_var2,x              ; clear var2
        lda     #$3D                    ; 61 frames
        sta     ent_var3,x              ; screen shake counter = 61
        lda     ent_status,x            ; read current status
        ora     #$40                    ; set invincibility flag
        sta     ent_status,x            ; write back with invincibility
        inc     ent_status,x            ; advance to phase 1
        rts

; --- phase 1: walk toward player, fire fist projectiles ---
        lda     ent_facing,x            ; save old facing direction
        pha                             ; push old facing to stack
        jsr     face_player             ; face player
        pla                             ; recover old facing
        cmp     ent_facing,x            ; did facing direction change?
        beq     hard_man_walk_check_timer               ; same direction → skip flip
        lda     ent_flags,x             ; load sprite flags
        eor     #$40                    ; flip sprite horizontally
        sta     ent_flags,x             ; store updated flags
hard_man_walk_check_timer:  lda     ent_timer,x
        bne     hard_man_walk_check_second_shot               ; timer nonzero → already fired once
        lda     ent_anim_state,x        ; check current anim state
        cmp     #$05                    ; at anim frame 5?
        bne     hard_man_walk_check_anim_done               ; not at frame 5 → skip
        jsr     hard_man_spawn_fist               ; spawn fist projectile (first shot)
        inc     ent_timer,x             ; mark first shot fired
hard_man_walk_check_second_shot:  lda     ent_var1,x
        bne     hard_man_walk_check_anim_done               ; var1 nonzero → already fired
        lda     ent_anim_state,x        ; check current anim state
        cmp     #$08                    ; at anim frame 8?
        bne     hard_man_walk_check_anim_done               ; not at frame 8 → skip
        jsr     hard_man_spawn_fist               ; spawn fist projectile (second shot)
        inc     ent_var1,x              ; mark second shot fired
hard_man_walk_check_anim_done:  lda     ent_anim_state,x
        cmp     #$0B                    ; walk anim finished?
        bne     hard_man_walk_done               ; anim not done → keep walking
        lda     #$2F                    ; jump anim ID
        jsr     reset_sprite_anim       ; set jump anim
        lda     #$00                    ; zero
        sta     ent_timer,x             ; clear timer for next phase
        jsr     hard_man_set_jump_xvel               ; set X velocity based on distance to player
        lda     #$68                    ; sub-pixel
        sta     ent_yvel_sub,x          ; Y velocity = $08.68 (jump upward)
        lda     #$08                    ; whole pixels upward
        sta     ent_yvel,x              ; set upward velocity
        inc     ent_status,x            ; advance to phase 2
        lda     #$1E                    ; 30 frames
        sta     ent_var1,x              ; invulnerable timer = 30 frames
hard_man_walk_done:  rts

; --- phase 2: ascending / descending toward player ---
        lda     ent_var1,x              ; invulnerable countdown?
        beq     hard_man_airborne_check_timer               ; zero → invulnerability ended
        dec     ent_var1,x              ; count down invulnerable timer
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; hold jump anim frame 1
        lda     #$01                    ; anim state 1 = jump pose
        sta     ent_anim_state,x        ; hold jump anim state
        rts                             ; return while invulnerable

hard_man_airborne_check_timer:  lda     ent_timer,x         ; timer == 0 → first descent
        bne     hard_man_headbutt_descent               ; timer != 0 → head-butt descent
        lda     ent_facing,x            ; get facing direction
        and     #$01                    ; check bit 0 (right)
        beq     hard_man_move_left               ; facing left → branch
        ldy     #$22                    ; collision mask index
        jsr     move_right_collide      ; move right with collision
        jmp     hard_man_apply_gravity               ; skip left movement

hard_man_move_left:  ldy     #$23
        jsr     move_left_collide       ; move left with collision
hard_man_apply_gravity:  ldy     #$26
        jsr     move_vertical_gravity   ; apply gravity
        bcc     hard_man_check_dive_distance               ; not on ground yet
        lda     #$30                    ; landing anim ID
        jsr     reset_sprite_anim       ; set landing anim
        lda     #$04                    ; anim state 4 = landed
        sta     ent_anim_state,x        ; set landed anim state
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        inc     ent_status,x            ; advance to phase 3 — ground slam

; Hard Man ground slam — stun the player
        lda     player_state            ; check player state
        cmp     #PSTATE_DEATH           ; if dead, don't stun
        beq     hard_man_stun_done
        cmp     #PSTATE_STUNNED         ; if already stunned, skip
        beq     hard_man_stun_done
        lda     #PSTATE_STUNNED         ; state → $0F (stunned)
        sta     player_state            ; player frozen in midair
        rts

hard_man_check_dive_distance:  lda     #$00                ; still in air — check if close to player
        sta     ent_anim_frame,x        ; reset anim frame
        sta     ent_anim_state,x        ; reset anim state
        jsr     entity_x_dist_to_player ; get X distance to player
        cmp     #$08                    ; close enough to dive?
        bcs     hard_man_airborne_rts               ; no — keep falling
        lda     #$30                    ; head-butt anim ID
        jsr     reset_sprite_anim       ; set head-butt anim
        inc     ent_timer,x             ; mark: switch to head-butt descent
        lda     #$00                    ; zero sub-pixel
        sta     ent_yvel_sub,x          ; Y velocity = $05.00 (fall fast)
        lda     #$05                    ; fast fall speed
        sta     ent_yvel,x              ; set fast downward velocity
hard_man_airborne_rts:  rts

; --- head-butt descent: fast drop straight down ---
hard_man_headbutt_descent:  lda     ent_anim_state,x
        cmp     #$03                    ; wait for anim to reach frame 3
        bne     hard_man_airborne_rts               ; not at frame 3 → wait
        lda     #$03                    ; hold at anim state 3
        sta     ent_anim_state,x        ; hold on anim state 3
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        lda     #SFX_HARD_STOMP
        jsr     submit_sound_ID         ; play stomp sound
        ldy     #$26                    ; collision mask index
        jsr     move_down_collide       ; move down with collision check
        bcc     hard_man_airborne_rts               ; not on ground → keep falling
        lda     #$30                    ; landing anim ID
        jsr     reset_sprite_anim       ; set landing anim
        lda     #$04                    ; anim state 4 = landed
        sta     ent_anim_state,x        ; set landed anim state
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        inc     ent_status,x            ; advance to phase 3

; Hard Man body slam — stun the player (second attack variant)
        lda     player_state            ; check player state
        cmp     #PSTATE_DEATH           ; if dead, don't stun
        beq     hard_man_stun_done               ; dead → skip stun
        cmp     #PSTATE_STUNNED         ; if already stunned, skip
        beq     hard_man_stun_done               ; already stunned → skip
        lda     #PSTATE_STUNNED         ; state → $0F (stunned)
        sta     player_state            ; player frozen in midair
hard_man_stun_done:  rts

; --- phase 3: screen shake after ground slam ---
        lda     ent_var3,x              ; shake counter
        beq     hard_man_shake_finished               ; done shaking → land
        dec     ent_var3,x              ; count down shake counter
        beq     hard_man_shake_finished               ; reached zero → stop shaking
        lda     #$CA                    ; wide hitbox ID
        sta     ent_hitbox,x            ; wide contact hitbox during shake
        lda     #$04                    ; anim state 4 = grounded
        sta     ent_anim_state,x        ; hold grounded anim
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        lda     #$9E                    ; bounce sub-pixel
        sta     ent_yvel_sub,x          ; keep bouncing on ground
        lda     #$04                    ; bounce whole pixels
        sta     ent_yvel,x              ; set bounce velocity
        lda     ent_var3,x              ; read shake counter
        and     #$01                    ; alternate screen shake direction
        bne     hard_man_shake_screen_up               ; odd → shift screen up
        lda     scroll_y                ; even frame: shift screen down
        clc
        adc     #$02                    ; shift down by 2 pixels
        sta     scroll_y                ; update scroll Y
        rts                             ; done this frame

hard_man_shake_screen_up:  lda     scroll_y            ; odd frame: shift screen up
        sec
        sbc     #$02                    ; shift up by 2 pixels
        sta     scroll_y                ; update scroll Y
        rts                             ; done this frame

; Hard Man landing — launches player into the air on impact

hard_man_shake_finished:  ldy     #$26                ; collision check
        jsr     move_vertical_gravity   ; is Hard Man near ground?
        bcc     hard_man_still_falling               ; no → still falling
        lda     player_state            ; if player dead ($0E),
        cmp     #PSTATE_DEATH           ; don't launch
        beq     hard_man_enter_cooldown               ; if player dead ($0E),
        lda     #PSTATE_AIRBORNE        ; set airborne state
        sta     player_state            ; player bounced into the air
hard_man_enter_cooldown:  inc     ent_status,x
        lda     #$10                    ; 16 frames cooldown
        sta     ent_var2,x              ; set cooldown timer
        rts                             ; done — enter phase 4

hard_man_still_falling:  lda     #$D0                ; still falling — set normal hitbox
        sta     ent_hitbox,x            ; restore normal hitbox
        lda     ent_anim_state,x        ; check current anim state
        cmp     #$04                    ; still at state 4?
        bne     hard_man_falling_rts               ; no → already advanced
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        inc     ent_anim_state,x        ; advance anim to state 5
hard_man_falling_rts:  rts

; --- phase 4: cooldown after landing, then loop back to phase 1 ---
        dec     ent_var2,x              ; count down cooldown timer
        bne     hard_man_cooldown_idle               ; still counting down → idle
        lda     #$C1                    ; reset status to phase 1 + invincibility
        sta     ent_status,x            ; status = phase 1 + invincible
        lda     #$00                    ; zero
        sta     ent_timer,x             ; reset all vars for next cycle
        sta     ent_var1,x              ; clear shot counter
        sta     ent_var2,x              ; clear var2
        lda     #$3D                    ; 61 frames
        sta     ent_var3,x              ; reset shake counter = 61
        lda     #$2C                    ; walk anim ID
        jsr     reset_sprite_anim       ; set walk anim
        rts

hard_man_cooldown_idle:  lda     #$2F                ; still cooling down — set jump pose
        jsr     reset_sprite_anim       ; set jump anim
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        lda     #$01                    ; anim state 1 = jump pose
        sta     ent_anim_state,x        ; hold jump pose
        rts                             ; done cooling down

; --- set Hard Man X velocity based on distance to player ---
hard_man_set_jump_xvel:  jsr     entity_x_dist_to_player ; get X distance to player
        ldy     #$06                    ; start at bracket 6 (closest)
hard_man_xvel_bracket_loop:  cmp     hard_man_jump_xvel_distance_thresholds_table,y ; find matching distance bracket
        bcc     hard_man_apply_xvel               ; below threshold → use this
        dey                             ; try next bracket
        bne     hard_man_xvel_bracket_loop               ; loop until bracket 0
hard_man_apply_xvel:  lda     hard_man_jump_xvel_sub_table,y ; set X velocity sub-pixel
        sta     ent_xvel_sub,x          ; store sub-pixel velocity
        lda     hard_man_jump_xvel_whole_table,y ; set X velocity whole pixel
        sta     ent_xvel,x              ; store whole pixel velocity
        rts                             ; velocity set for jump

; Hard Man jump X velocity lookup tables (7 distance brackets)
hard_man_jump_xvel_distance_thresholds_table:  .byte   $79,$6A,$5B,$4C,$3D,$2E,$1F ; distance thresholds
hard_man_jump_xvel_sub_table:  .byte   $80,$00,$80,$00,$80,$00,$80 ; X velocity sub-pixel
hard_man_jump_xvel_whole_table:  .byte   $03,$03,$02,$02,$01,$01,$00 ; X velocity whole pixel
; --- spawn Hard Man fist projectile ---
hard_man_spawn_fist:  jsr     find_enemy_freeslot_y ; find free enemy slot → Y
        bcs     hard_man_spawn_fist_done               ; no slot → bail
        sty     temp_00                 ; save free slot index
        lda     ent_facing,x            ; copy facing direction
        sta     ent_facing,y            ; projectile inherits facing
        and     #$02                    ; isolate left/right bit
        tay                             ; use as table index (0 or 2)
        lda     ent_x_px,x              ; position projectile relative to Hard Man
        clc                             ; add carry into position
        adc     hard_man_fist_xoffset_table,y ; X offset based on facing
        pha
        lda     ent_x_scr,x             ; load Hard Man screen X
        adc     hard_man_fist_screen_offset_table,y ; add screen offset for facing
        ldy     temp_00                 ; restore free slot index
        sta     ent_x_scr,y             ; set projectile screen X
        pla                             ; pull pixel X from stack
        sta     ent_x_px,y              ; set projectile pixel X
        lda     ent_y_px,x              ; get Hard Man Y position
        clc
        adc     #$06                    ; Y offset: slightly below center
        sta     ent_y_px,y              ; set projectile Y position
        lda     #$00                    ; zero HP
        sta     ent_hp,y                ; projectile has no HP
        lda     #$2D                    ; fist flying anim ID
        jsr     init_child_entity       ; init child entity with anim $2D
        lda     #$8B                    ; fist hitbox ID
        sta     ent_hitbox,y            ; set projectile hitbox
        lda     #$D1                    ; routine $D1 = fist AI
        sta     ent_routine,y           ; routine $D1 → Hard Man fist AI
hard_man_spawn_fist_done:  rts

; Hard Man fist spawn X offsets (facing right / facing left)
hard_man_fist_xoffset_table:  .byte   $04
hard_man_fist_screen_offset_table:  .byte   $00,$FC,$FF
; =============================================================================
; HARD MAN FIST PROJECTILE ($D1)
; =============================================================================
; Homing fist that tracks player position. Two phases:
;   Phase 1: fly toward player, homing on position
;   Phase 2: return to Hard Man after reaching player
; =============================================================================

hard_man_fist_main:  lda     ent_status,x        ; --- init ---
        and     #$0F                    ; isolate phase number
        bne     hard_man_fist_phase_dispatch               ; nonzero → already inited
        sta     ent_timer,x             ; clear timer
        sta     ent_var1,x              ; clear return flag
        lda     #$0C                    ; 12 frames between re-homes
        sta     ent_var2,x              ; frames between homing recalculations
        jsr     hard_man_fist_calc_homing               ; calculate homing velocity toward player
        inc     ent_status,x            ; advance to phase 1
hard_man_fist_phase_dispatch:  lda     ent_status,x        ; --- phase dispatch ---
        and     #$02                    ; check bit 1 (return phase)
        bne     hard_man_fist_return_phase               ; phase 2: returning
        lda     ent_facing,x            ; --- phase 1: fly toward player ---
        and     #$08                    ; check vertical direction
        beq     hard_man_fist_move_down               ; bit 3 clear → move down
        jsr     move_sprite_up          ; move up
        lda     ent_y_scr,x             ; check if off-screen
        bne     hard_man_fist_despawn               ; off-screen → despawn
        jmp     hard_man_fist_move_horiz               ; skip to horizontal move

hard_man_fist_move_down:  jsr     move_sprite_down    ; move down
        lda     ent_y_scr,x             ; check if off-screen
        bne     hard_man_fist_despawn               ; off-screen → despawn
hard_man_fist_move_horiz:  jsr     set_sprite_hflip    ; update sprite flip
        lda     ent_facing,x            ; get facing direction
        and     #$01                    ; check bit 0 (right)
        beq     hard_man_fist_move_left               ; facing left → branch
        jsr     move_sprite_right       ; move right
        jmp     hard_man_fist_check_reached               ; skip to distance check

hard_man_fist_move_left:  jsr     move_sprite_left    ; move left
hard_man_fist_check_reached:  lda     ent_timer,x         ; already reached player?
        bne     hard_man_fist_check_return               ; already reached → start return
        jsr     entity_x_dist_to_player ; get X distance to player
        cmp     #$0C                    ; close enough?
        bcs     hard_man_fist_rts               ; no → keep flying
        inc     ent_timer,x             ; mark: reached player
hard_man_fist_rts:  rts

hard_man_fist_check_return:  lda     ent_var1,x          ; already started return?
        bne     hard_man_fist_rts               ; already returning → wait
        dec     ent_var2,x              ; count down return delay
        bne     hard_man_fist_rts               ; delay not done → wait
        lda     #$0C                    ; reset return delay to 12
        sta     ent_var2,x              ; store return delay
        inc     ent_var1,x              ; mark return started
        lda     #$2E                    ; return anim ID
        jsr     reset_sprite_anim       ; set return anim
        inc     ent_status,x            ; advance to phase 2
        rts

; --- phase 2: return to Hard Man ---
hard_man_fist_return_phase:  lda     ent_anim_frame,x
        ora     ent_anim_state,x        ; check if anim finished
        bne     hard_man_fist_return_wait               ; still playing → wait
        lda     #$2D                    ; flying anim ID
        jsr     reset_sprite_anim       ; reset to flying anim
        jsr     hard_man_fist_calc_homing               ; recalculate homing velocity toward Hard Man
        dec     ent_status,x            ; back to phase 1 (re-home)
hard_man_fist_return_wait:  rts

hard_man_fist_despawn:  lda     #$00                ; off-screen → despawn
        sta     ent_status,x            ; despawn fist
        rts                             ; done

; --- calculate homing velocity toward player (or back to owner) ---
hard_man_fist_calc_homing:  lda     #$4C                ; speed parameter low
        sta     $02                     ; store speed low byte
        lda     #$03                    ; speed parameter high
        sta     $03                     ; store speed high byte
        jsr     calc_homing_velocity    ; calculate homing velocity vector
        lda     $0C                     ; store resulting facing direction
        sta     ent_facing,x            ; apply calculated facing
        rts                             ; done

; =============================================================================
; SPARK MAN AI ($D2)
; =============================================================================
; Two main phases:
;   Phase 0: jump between fixed X positions, fire spark shots on landing.
;     Bounces 1-4 times before attacking. After final bounce, advances to
;     phase 1 for the attack anim.
;   Phase 1: attack — plays Spark Shock anim, spawns projectiles, then
;     returns to phase 0 with a cooldown timer.
; Uses 8 fixed X-position waypoints (spark_man_waypoint_xpos_table) for landing spots.
; =============================================================================

spark_man_state_dispatch:  lda     ent_status,x        ; --- main dispatch ---
        and     #$0F                    ; isolate phase number
        beq     spark_man_cooldown_check               ; phase 0: jumping/bouncing
        jmp     spark_man_attack_phase               ; phase 1: attack

; --- phase 0: jumping between platforms ---
spark_man_cooldown_check:  lda     ent_timer,x         ; cooldown timer active?
        beq     spark_man_check_ground_state               ; no → check bounce state
        dec     ent_timer,x             ; count down cooldown
        bne     spark_man_face_player_and_reset               ; not zero → still cooling
        dec     ent_anim_state,x        ; timer expired → reset anim
spark_man_face_player_and_reset:  jsr     face_player_preserve_facing           ; face player (preserve facing)
spark_man_reset_anim_frame:  lda     #$00                ; reset anim frame
        sta     ent_anim_frame,x        ; reset anim frame
        rts                             ; done cooling down

spark_man_check_ground_state:  lda     ent_anim_state,x    ; on the ground?
        beq     spark_man_init_jump               ; yes → start new jump
        lda     ent_anim_frame,x        ; check landing anim frame
        cmp     #$08                    ; landing anim done?
        bne     hard_man_fist_return_wait               ; not done → wait
        dec     ent_var2,x              ; decrement bounce counter
        beq     spark_man_start_attack               ; all bounces done → attack
spark_man_set_jump_velocity:  lda     #$88                ; set jump velocity $06.88
        sta     ent_yvel_sub,x          ; set Y velocity sub-pixel
        lda     #$06                    ; whole pixels upward
        sta     ent_yvel,x              ; set Y velocity whole
        rts                             ; done setting jump velocity

spark_man_start_attack:  lda     #$38                ; set Spark Shock attack anim
        jsr     reset_sprite_anim       ; set jump anim
        inc     ent_status,x            ; advance to phase 1

; --- face player utility (preserves original facing for movement) ---
face_player_preserve_facing:  lda     ent_facing,x
        pha                             ; save old facing
        jsr     face_player             ; face player
        jsr     set_sprite_hflip        ; update sprite flip
        pla                             ; recover old facing
        sta     ent_facing,x            ; restore original facing
        rts                             ; done

; --- start new jump from ground ---
spark_man_init_jump:  lda     ent_var2,x          ; bounce counter initialized?
        bne     spark_man_jump_apply_gravity               ; already initialized → skip
        lda     #$37                    ; jump anim ID
        jsr     reset_sprite_anim       ; set jump anim
        jsr     set_sprite_hflip        ; update sprite flip
        jsr     spark_man_set_jump_velocity               ; set jump velocity
        lda     $E4                     ; RNG: random bounce count 1-4
        adc     $E6                     ; mix with second RNG byte
        sta     $E7                     ; store mixed RNG result
        and     #$03                    ; mask to 0-3 range
        clc
        adc     #$01                    ; add 1 → range 1-4
        sta     ent_var2,x              ; store bounce count
spark_man_jump_apply_gravity:  ldy     #$1E
        jsr     move_vertical_gravity   ; apply gravity
        bcs     spark_man_snap_to_waypoint               ; landed → snap to waypoint
        jsr     spark_man_reset_anim_frame               ; in air: reset anim frame
        lda     ent_var1,x              ; get current waypoint index
        and     #$03                    ; isolate waypoint sub-index
        tay
        lda     spark_man_waypoint_xvel_sub_table,y ; look up X velocity sub-pixel
        sta     ent_xvel_sub,x          ; set X velocity sub-pixel
        lda     spark_man_waypoint_xvel_whole_table,y ; look up X velocity whole pixel
        sta     ent_xvel,x              ; set X velocity whole
        lda     ent_facing,x            ; get facing direction
        and     #$02                    ; check horizontal direction
        beq     spark_man_move_right               ; facing right → branch
        ldy     #$21                    ; left collision mask index
        jmp     move_left_collide       ; move left with collision

spark_man_move_right:  ldy     #$20
        jmp     move_right_collide      ; move right with collision

; --- landed on ground: snap to waypoint X position ---
spark_man_snap_to_waypoint:  lda     ent_var1,x
        tay
        lda     spark_man_waypoint_xpos_table,y ; get fixed X position for this waypoint
        sta     ent_x_px,x              ; snap X to waypoint
        inc     ent_anim_state,x        ; signal: on ground
        jsr     spark_man_reset_anim_frame               ; reset anim frame on landing
        inc     ent_var1,x              ; advance to next waypoint
        lda     ent_var1,x              ; read new waypoint index
        and     #$07                    ; wrap waypoint index 0-7
        sta     ent_var1,x              ; store wrapped index
        and     #$03                    ; check if multiple of 4
        bne     flip_direction_done               ; every 4 waypoints → reverse direction
flip_direction:  lda     ent_facing,x        ; flip horizontal direction
        eor     #$03                    ; toggle right/left bits
        sta     ent_facing,x            ; store flipped facing
        lda     ent_flags,x             ; load sprite flags
        eor     #$40                    ; flip sprite
        sta     ent_flags,x             ; store flipped sprite
flip_direction_done:  rts

; --- phase 1: Spark Shock attack ---
spark_man_attack_phase:  jsr     face_player_preserve_facing           ; face player
        lda     ent_anim_state,x        ; check anim state
        ora     ent_anim_frame,x        ; combine with anim frame
        bne     spark_man_attack_check_frame               ; anim still playing
        lda     #$39                    ; check if large spark anim
        cmp     ent_anim_id,x           ; is it large spark anim?
        beq     spark_man_attack_finished               ; yes → handle large spark
        jmp     reset_sprite_anim       ; set large spark anim $39

spark_man_attack_finished:  lda     #$37                ; large spark done → set idle anim
        jsr     reset_sprite_anim       ; advance anim state
        inc     ent_anim_state,x        ; signal: spark anim started
        lda     #$64                    ; 100 frames
        sta     ent_timer,x             ; cooldown = 100 frames
        dec     ent_status,x            ; return to phase 0
        rts

spark_man_attack_check_frame:  lda     ent_anim_frame,x    ; check attack anim progress
        bne     flip_direction_done               ; not at frame 0 → skip
        lda     #$39                    ; large spark anim ID
        cmp     ent_anim_id,x           ; large spark anim?
        beq     spark_man_large_spark_check               ; yes → handle large spark
        lda     ent_anim_state,x        ; small spark: wait for anim state 3
        cmp     #$03                    ; wait for anim state 3
        bne     flip_direction_done               ; not yet → wait
        jmp     spark_man_spawn_8way_sparks               ; spawn 8-way spark projectiles

spark_man_large_spark_check:  lda     ent_anim_state,x    ; large spark: wait for anim state $0A
        cmp     #$0A                    ; wait for anim state $0A
        bne     flip_direction_done               ; not yet → wait
        jmp     spark_man_spawn_homing_ball               ; spawn homing spark ball

; --- spawn 8-directional spark projectiles (small spark attack) ---
spark_man_spawn_8way_sparks:  stx     temp_00 ; save Spark Man slot
        lda     #$07                    ; spawn 8 projectiles (indices 7→0)
        sta     $01                     ; store projectile counter
spark_man_spawn_spark_loop:  jsr     find_enemy_freeslot_y ; find free enemy slot → Y
        bcs     spark_man_spawn_sparks_done               ; no slot → done
        ldx     $01                     ; load projectile index
        lda     spark_man_8way_xvel_sub_table,x ; set X velocity sub from table
        sta     ent_xvel_sub,y          ; set projectile X vel sub
        lda     spark_man_8way_xvel_whole_table,x ; set X velocity whole from table
        sta     ent_xvel,y              ; set projectile X vel whole
        lda     spark_man_8way_yvel_sub_table,x ; set Y velocity sub from table
        sta     ent_yvel_sub,y          ; set projectile Y vel sub
        lda     spark_man_8way_yvel_whole_table,x ; set Y velocity whole from table
        sta     ent_yvel,y              ; set projectile Y vel whole
        lda     spark_man_8way_facing_table,x ; set facing direction from table
        sta     ent_facing,y            ; set projectile facing
        ldx     temp_00                 ; restore Spark Man slot
        lda     #$3A                    ; spark projectile anim ID
        jsr     init_child_entity       ; init child entity with spark anim
        lda     #$8B                    ; small spark hitbox ID
        sta     ent_hitbox,y            ; projectile hitbox
        lda     #$43                    ; generic projectile routine
        sta     ent_routine,y           ; generic projectile routine
        lda     ent_x_px,x              ; spawn at Spark Man's position
        sta     ent_x_px,y              ; copy Spark Man pixel X
        lda     ent_x_scr,x             ; get Spark Man screen X
        sta     ent_x_scr,y             ; copy screen X to projectile
        lda     ent_y_px,x              ; get Spark Man Y position
        sec
        sbc     #$08                    ; offset slightly above center
        sta     ent_y_px,y              ; set projectile Y position
        dec     $01                     ; next projectile
        bpl     spark_man_spawn_spark_loop               ; more to spawn → loop
spark_man_spawn_sparks_done:  ldx     temp_00
        rts                             ; done spawning projectiles

; --- spawn homing spark ball (large spark attack) ---
spark_man_spawn_homing_ball:  stx     $0E                 ; save Spark Man slot
        jsr     find_enemy_freeslot_y   ; find free enemy slot → Y
        bcs     spark_man_spawn_ball_done               ; no slot → bail
        lda     ent_y_px,x              ; get Spark Man Y position
        clc
        adc     #$05                    ; spawn slightly below center
        sta     ent_y_px,y              ; set child Y position
        lda     ent_x_scr,x             ; copy screen X to child
        sta     ent_x_scr,y             ; set child screen X
        lda     ent_x_px,x              ; copy pixel X to child
        sta     ent_x_px,y              ; set child pixel X
        lda     #$3C                    ; homing spark ball anim ID
        jsr     init_child_entity       ; init child entity with spark ball anim
        lda     #$8A
        sta     ent_hitbox,y            ; spark ball hitbox
        lda     #$B8
        sta     ent_routine,y           ; homing spark ball routine
        sty     $0F                     ; save child slot index
        lda     #$00                    ; calculate homing velocity toward player
        sta     $02                     ; speed parameter low
        lda     #$02
        sta     $03                     ; speed parameter high
        tya                             ; child slot to A
        tax                             ; set X to child slot
        jsr     calc_homing_velocity    ; calculate homing velocity vector
        ldy     $0F                     ; restore child slot
        lda     $0C                     ; apply homing facing/velocity
        sta     ent_facing,y            ; set child facing
        and     #$02                    ; isolate left-facing bit
        tax                             ; use as table index
        lda     ent_x_px,y              ; offset X position based on facing
        clc
        adc     spark_man_homing_ball_xoffset_right,x ; add X offset for facing
        sta     ent_x_px,y              ; store adjusted X pixel
        lda     ent_x_scr,y             ; get child screen X
        adc     spark_man_homing_ball_xoffset_table,x ; add screen carry offset
        sta     ent_x_scr,y             ; store adjusted screen X
spark_man_spawn_ball_done:  ldx     $0E                 ; restore Spark Man slot
        rts

; --- Spark Man data tables ---
; Waypoint X positions for Spark Man's 8 fixed landing spots
spark_man_waypoint_xpos_table:  .byte   $A8,$80,$58,$20,$58,$80,$A8,$E0
; X velocity for each waypoint (sub-pixel / whole pixel)
spark_man_waypoint_xvel_sub_table:  .byte   $6D,$05,$05,$6D ; X velocity sub-pixel per waypoint
spark_man_waypoint_xvel_whole_table:  .byte   $01,$01,$01,$01 ; X velocity whole pixel per waypoint
; Homing spark ball spawn X offsets (facing right / facing left)
spark_man_homing_ball_xoffset_right:  .byte   $20
spark_man_homing_ball_xoffset_table:  .byte   $00,$E0,$FF
; 8-directional spark projectile velocity tables (8 directions)
spark_man_8way_xvel_sub_table:  .byte   $00,$6A,$00,$6A,$00,$6A,$00,$6A ; X velocity sub
spark_man_8way_xvel_whole_table:  .byte   $00,$01,$02,$01,$00,$01,$02,$01 ; X velocity whole
spark_man_8way_yvel_sub_table:  .byte   $00,$96,$00,$6A,$00,$6A,$00,$96 ; Y velocity sub
spark_man_8way_yvel_whole_table:  .byte   $FE,$FE,$00,$01,$02,$01,$00,$FE ; Y velocity whole
spark_man_8way_facing_table:  .byte   $02,$02,$02,$02,$01,$01,$01,$01 ; facing direction per spark
; =============================================================================
; SNAKE MAN AI ($D4)
; =============================================================================
; Two phases:
;   Phase 0: walk on the ground, move between 4 waypoints (snake_man_waypoint_xpos_table).
;     At each waypoint, decide to jump or fire Search Snake.
;   Phase 1: in the air — apply gravity, move horizontally. On landing,
;     return to phase 0.
; Search Snake ($BA) is spawned as a homing projectile.
; =============================================================================

snake_man_state_dispatch:  lda     ent_status,x        ; --- main dispatch ---
        and     #$0F                    ; isolate phase number
        beq     snake_man_ground_phase               ; phase 0
        jmp     snake_man_airborne_phase               ; phase 1: airborne

; --- phase 0: on ground ---
snake_man_ground_phase:  ldy     #$00                ; gravity type = normal
        jsr     move_vertical_gravity   ; apply gravity
        bcs     snake_man_on_ground               ; on ground → walk logic
snake_man_set_fall_anim:  lda     #$23                ; in air → set fall anim
        jsr     reset_sprite_anim
        inc     ent_anim_state,x        ; advance anim state
        rts                             ; return

snake_man_on_ground:  lda     #$23                ; check if walk anim is active
        cmp     ent_anim_id,x           ; is current anim = fall?
        bne     snake_man_walk               ; different anim → start walking
        lda     ent_anim_state,x        ; check anim state
        cmp     #$01                    ; walk anim at state 1?
        bne     snake_man_pause_check               ; not state 1 yet
        lda     #$02                    ; advance to walk state 2
        sta     ent_anim_state,x        ; set walk state 2
        jmp     snake_man_reset_anim_frame               ; reset anim frame

snake_man_pause_check:  lda     ent_var2,x          ; pause timer active?
        beq     snake_man_wait_anim_frame               ; no pause → continue
        dec     ent_var2,x              ; decrement pause timer
        lda     #$00                    ; freeze animation
        sta     ent_anim_frame,x        ; hold current frame during pause
        rts                             ; wait for timer

snake_man_wait_anim_frame:  lda     ent_anim_frame,x    ; wait for anim frame 4
        cmp     #$04                    ; reached frame 4?
        beq     snake_man_walk               ; yes → start walking
        rts                             ; wait for frame 4

; --- walking: check waypoint and decide action ---
snake_man_walk:  jsr     set_sprite_hflip    ; update sprite flip
        lda     #$25                    ; set walk anim
        cmp     ent_anim_id,x           ; already walking?
        beq     snake_man_walk_move               ; yes → skip anim reset
        jsr     reset_sprite_anim       ; set walk anim
snake_man_walk_move:  jsr     move_horizontal_facing           ; move horizontally
        bcc     snake_man_check_waypoint               ; no wall hit → check waypoint
        ldy     #$00                    ; hit wall → jump (Search Snake type 0)
        beq     snake_man_start_jump               ; always branch to jump
snake_man_check_waypoint:  lda     ent_timer,x         ; current waypoint index
        tay                             ; waypoint index to Y
        lda     ent_facing,x            ; get facing direction
        and     #$02                    ; check facing direction
        beq     snake_man_check_waypoint_right               ; facing right → branch
        lda     snake_man_waypoint_xpos_table,y ; facing left: check if passed waypoint
        cmp     ent_x_px,x              ; compare with X position
        bcs     snake_man_waypoint_reached               ; reached waypoint → jump/fire
        rts                             ; not at waypoint yet

snake_man_check_waypoint_right:  lda     snake_man_waypoint_xpos_table,y ; facing right: check if passed waypoint
        cmp     ent_x_px,x              ; compare with X position
        bcc     snake_man_waypoint_reached               ; reached waypoint → jump/fire
        rts                             ; not at waypoint yet

; --- waypoint reached: advance and decide action ---
snake_man_waypoint_reached:  inc     ent_timer,x         ; next waypoint
        lda     ent_timer,x             ; get updated waypoint index
        and     #$03                    ; wrap 0-3
        sta     ent_timer,x
        and     #$01                    ; check odd/even waypoint
        bne     snake_man_scan_search_snake               ; odd → skip direction flip
        jsr     flip_direction               ; every 2 waypoints → reverse direction
snake_man_scan_search_snake:  ldy     #$0F                ; scan enemy slots for existing Search Snake
snake_man_scan_slot_loop:  lda     $0310,y             ; check entity status high
        bpl     snake_man_scan_next_slot               ; slot inactive → skip
        lda     $0330,y                 ; check routine ID
        cmp     #$BA                    ; is it a Search Snake?
        bne     snake_man_scan_next_slot               ; not Search Snake → skip
        ldy     #$00                    ; Search Snake exists → small jump (type 0)
        beq     snake_man_start_jump               ; always branch to jump
snake_man_scan_next_slot:  dey                         ; next slot down
        bne     snake_man_scan_slot_loop               ; continue scanning
        lda     $E4                     ; no Search Snake → RNG pick jump type
        adc     $E6                     ; add second RNG byte
        and     #$01                    ; 0 = small jump, 1 = high jump + fire
        tay                             ; jump type index to Y
snake_man_start_jump:  lda     snake_man_jump_anim_table,y ; set jump anim based on type
        jsr     reset_sprite_anim       ; apply jump anim
        lda     snake_man_jump_yvel_sub_table,y ; set Y velocity sub based on type
        sta     ent_yvel_sub,x          ; set Y velocity sub
        lda     snake_man_jump_yvel_whole_table,y ; set Y velocity whole based on type
        sta     ent_yvel,x              ; set Y velocity whole
        inc     ent_status,x            ; advance to phase 1 (airborne)
        rts                             ; return

; --- phase 1: airborne ---
snake_man_airborne_phase:  ldy     #$00                ; gravity type = normal
        jsr     move_vertical_gravity   ; apply gravity
        bcs     snake_man_landed               ; landed → return to phase 0
        lda     ent_anim_id,x           ; get current anim ID
        cmp     #$24                    ; firing anim active?
        beq     snake_man_check_falling               ; yes → skip horizontal move
        jsr     move_horizontal_facing               ; move horizontally
snake_man_check_falling:  lda     ent_yvel,x          ; check Y velocity sign
        bpl     snake_man_reset_anim_frame               ; still rising → reset frame
        lda     #$24                    ; falling → check if firing anim
        cmp     ent_anim_id,x
        beq     snake_man_fire_check               ; yes → fire Search Snake logic
        jmp     snake_man_set_fall_anim               ; set fall anim

; --- landed on ground ---
snake_man_landed:  lda     ent_anim_id,x       ; get current anim ID
        cmp     #$24                    ; was firing anim active?
        bne     snake_man_reset_vars               ; not firing → skip pause
        lda     #$1A                    ; pause = 26 frames
        sta     ent_var2,x              ; set pause timer after landing from firing
snake_man_reset_vars:  lda     #$00                ; clear fire cooldown
        sta     ent_var1,x              ; reset var1
        dec     ent_status,x            ; return to phase 0
        jsr     snake_man_set_fall_anim               ; set fall anim
        inc     ent_anim_state,x        ; advance anim state
snake_man_reset_anim_frame:  lda     #$00                ; clear frame counter
        sta     ent_anim_frame,x        ; reset anim frame
snake_man_rts:  rts                         ; return

; --- airborne firing: Search Snake spawn logic ---
snake_man_fire_check:  lda     ent_var1,x          ; fire cooldown active?
        beq     snake_man_spawn_search_snake               ; no → spawn Search Snake
        dec     ent_var1,x              ; decrement cooldown
        lda     ent_anim_state,x        ; check anim state
        cmp     #$02                    ; at fire pose state?
        bne     snake_man_reset_anim_frame               ; no → reset anim frame
        lda     ent_anim_frame,x        ; check anim frame
        cmp     #$08                    ; at frame 8?
        bne     snake_man_rts               ; no → return
        dec     ent_anim_state,x        ; advance firing anim
        bne     snake_man_reset_anim_frame               ; always → reset anim frame

; --- spawn Search Snake projectile ---
snake_man_spawn_search_snake:  lda     #$02                ; set firing anim state
        sta     ent_anim_state,x        ; apply to anim state
        jsr     snake_man_reset_anim_frame               ; reset anim frame
        jsr     face_player_preserve_facing               ; face player (preserve facing)
        lda     #$14
        sta     ent_var1,x              ; fire cooldown = 20 frames
        stx     temp_00                 ; save Snake Man slot
        jsr     find_enemy_freeslot_y   ; find free enemy slot → Y
        bcs     snake_man_spawn_done               ; no slot → bail
        lda     #$52                    ; Search Snake anim ID
        jsr     init_child_entity       ; init child entity with Search Snake anim
        lda     #$CB                    ; Search Snake hitbox ID
        sta     ent_hitbox,y            ; Search Snake hitbox
        lda     #$BA                    ; Search Snake routine ID
        sta     ent_routine,y           ; Search Snake AI routine
        lda     #$44                    ; Y velocity sub = $44
        sta     ent_yvel_sub,y          ; initial Y velocity = $03.44 (fall)
        lda     #$03                    ; Y velocity whole = $03
        sta     ent_yvel,y              ; set initial fall speed
        lda     ent_y_px,x              ; spawn at Snake Man position
        sec                             ; prepare subtraction
        sbc     #$04                    ; offset slightly above
        sta     ent_y_px,y              ; set child Y position
        lda     ent_x_scr,x             ; copy screen X to child
        sta     ent_x_scr,y             ; set child screen X
        lda     ent_x_px,x              ; copy pixel X to child
        sta     ent_x_px,y              ; set child pixel X
        lda     ent_facing,x            ; face toward player for projectile
        pha                             ; save Snake Man facing
        jsr     face_player             ; face player
        lda     ent_facing,x            ; get player-facing result
        sta     ent_facing,y            ; set child facing
        and     #$02                    ; offset X based on facing
        tax                             ; facing as table index
        lda     ent_x_px,y              ; get child X pixel
        clc
        adc     snake_man_search_snake_xoffset_right,x ; add spawn X offset
        sta     ent_x_px,y              ; store adjusted X pixel
        lda     ent_x_scr,y             ; get child screen X
        adc     snake_man_search_snake_xoffset_table,x ; add screen carry offset
        sta     ent_x_scr,y             ; store adjusted screen X
        ldx     temp_00                 ; restore Snake Man slot
        pla                             ; recover saved facing
        sta     ent_facing,x            ; restore Snake Man's original facing
snake_man_spawn_done:  ldx     temp_00  ; restore Snake Man slot
        rts                             ; return

; --- Snake Man data tables ---
; Waypoint X positions (4 waypoints, cycled with wrap)
snake_man_waypoint_xpos_table:  .byte   $80,$28,$80,$D8
; Jump type tables: [0]=small jump, [1]=high jump+fire
snake_man_jump_anim_table:  .byte   $23,$24 ; anim ID per jump type
snake_man_jump_yvel_sub_table:  .byte   $A8,$00 ; Y velocity sub per jump type
snake_man_jump_yvel_whole_table:  .byte   $05,$08 ; Y velocity whole per jump type
; Search Snake spawn X offsets (facing right / facing left)
snake_man_search_snake_xoffset_right:  .byte   $1E
snake_man_search_snake_xoffset_table:  .byte   $00,$E2,$FF
; =============================================================================
; GEMINI MAN AI ($D6)
; =============================================================================
; Three phases:
;   Phase 0: init — set fall velocity, spawn clone via gemini_man_spawn_clone, set anim.
;     The clone is a second Gemini Man entity linked via ent_var1.
;   Phase 1: dual Gemini Man — two copies run around. Syncs HP between
;     original and clone. When HP drops below 15, the clone gets destroyed
;     and phase transitions to phase 2 (solo).
;   Phase 2: solo Gemini Man — runs back and forth, fires Gemini Laser.
;     Jumps when B pressed (simulating player-like input).
; =============================================================================

main_gemini_man:  lda     ent_status,x  ; get entity status
        and     #$0F                    ; isolate phase number
        cmp     #$02                    ; phase 2?
        bne     gemini_man_check_phase1               ; not phase 2 → check 0/1
        jmp     gemini_man_solo_phase               ; solo phase

gemini_man_check_phase1:  cmp     #$01                ; phase 1?
        beq     gemini_man_dual_sync_hp               ; already initialized → skip init

; --- phase 0: init ---
        lda     #$3D                    ; fall velocity sub = $3D
        sta     ent_yvel_sub,x          ; Y velocity = $09.3D (fall)
        lda     #$09                    ; fall velocity whole = $09
        sta     ent_yvel,x              ; set initial Y velocity
        inc     ent_status,x            ; advance to phase 1
        lda     #$33                    ; Gemini Man run anim
        jsr     reset_sprite_anim       ; set Gemini Man run anim
        lda     ent_timer,x             ; clone already spawned?
        bne     gemini_man_dual_sync_hp               ; skip a little bit
        jsr     gemini_man_spawn_clone               ; spawn clone entity
        lda     #$01                    ; set timer = 1
        sta     ent_timer,x             ; mark: clone spawned

; --- phase 1: dual mode — sync HP between original and clone ---
gemini_man_dual_sync_hp:  lda     ent_timer,x         ; check clone status flag
        bmi     gemini_man_movement_dispatch               ; timer negative → clone destroyed
        ldy     ent_var1,x              ; Y = clone entity slot
        lda     ent_status,y            ; check clone active bit
        bpl     gemini_man_movement_dispatch               ; clone inactive → skip sync
        jsr     entity_ai_dispatch      ; check collision with player
        bcs     gemini_man_movement_dispatch
        ldy     ent_var1,x              ; reload clone slot
        lda     ent_hp,x                ; sync HP: copy to clone
        and     #$1F                    ; mask HP to 5 bits
        sta     ent_hp,y                ; sync HP to clone
        cmp     #$0F                    ; HP < 15?
        bcs     gemini_man_movement_dispatch               ; no → keep dual mode
        lda     ent_hitbox,y            ; yes → destroy clone
        ora     #$40                    ; set clone invincible (dying)
        sta     ent_hitbox,y            ; apply invincible flag
        lda     #$00                    ; clear status to re-init
        sta     ent_status,x            ; reset this entity (will re-init as solo)
        lda     #$80                    ; negative timer value
        sta     ent_timer,y             ; mark clone for destruction
        rts                             ; return

; --- phase 1 continued: movement and attack dispatch ---
gemini_man_movement_dispatch:  lda     ent_anim_id,x       ; check current anim
        cmp     #$35                    ; running anim?
        beq     gemini_man_run_check_shoot               ; yes → run logic
        cmp     #$34                    ; shooting anim?
        bne     gemini_man_check_clone_shooting               ; not shooting → check clone
        jmp     gemini_man_shoot_anim               ; yes → shooting logic

; --- check if clone is shooting (wait for clone to finish) ---
gemini_man_check_clone_shooting:  lda     ent_timer,x         ; check clone alive flag
        bmi     gemini_man_apply_gravity               ; clone destroyed → skip
        lda     ent_var1,x              ; get clone slot index
        tay                             ; clone slot to Y
        lda     ent_anim_id,y           ; is clone shooting?
        cmp     #$34                    ; shooting anim ID?
        bne     gemini_man_apply_gravity               ; clone not shooting → go
        lda     #$00                    ; yes → hold position
        sta     ent_anim_frame,x        ; freeze anim frame
gemini_man_rts:  rts                         ; return

; --- gravity and horizontal movement ---
gemini_man_apply_gravity:  ldy     #$00                ; gravity type = normal
        jsr     move_vertical_gravity   ; apply gravity
        bcs     gemini_man_ground_check_reverse               ; on ground
        lda     #$00                    ; in air → reset anim frame
        sta     ent_anim_frame,x        ; clear anim frame

; --- horizontal movement utility (shared with Snake Man) ---
move_horizontal_facing:  lda     ent_facing,x        ; get facing direction
        and     #$02                    ; isolate left-facing bit
        beq     move_horizontal_right               ; facing right → branch
        ldy     #$01                    ; collision check slot 1
        jmp     move_left_collide       ; move left with collision

move_horizontal_right:  ldy     #$00                ; collision check slot 0
        jmp     move_right_collide      ; move right with collision

; --- on ground: check if should reverse direction ---
gemini_man_ground_check_reverse:  lda     ent_anim_state,x    ; check anim state
        beq     gemini_man_rts               ; anim not ready
        lda     ent_anim_frame,x        ; check anim frame
        cmp     #$04                    ; at frame 4?
        bne     gemini_man_rts               ; no → wait
        lda     #$35                    ; running anim ID
        jsr     reset_sprite_anim       ; set running anim
        jsr     flip_direction               ; reverse direction
        lda     #$28                    ; left boundary X = $28
        sta     ent_x_px,x              ; reset X position to left side
        lda     ent_timer,x             ; check clone status
        bpl     gemini_man_run_check_shoot               ; clone alive → run mode

; --- solo mode: set running velocity, advance to phase 2 ---
gemini_man_set_solo_speed:  lda     #$4C                ; X velocity sub = $4C
        sta     ent_xvel_sub,x          ; X velocity = $01.4C
        lda     #$01                    ; X velocity whole = $01
        sta     ent_xvel,x              ; set run speed
        inc     ent_status,x            ; advance to phase 2
        jsr     face_player             ; face player
        jsr     set_sprite_hflip        ; update sprite flip

; --- set random timer for next action ---
gemini_man_set_random_timer:  lda     $E6                 ; RNG
        adc     $E7                     ; add second RNG byte
        and     #$01                    ; mask to 0 or 1
        tay                             ; timer index to Y
        lda     gemini_man_random_timer_table,y ; pick timer value ($B4 or $FF)
        sta     ent_timer,x             ; set action timer
        rts

; --- running mode: check for B press to shoot ---
gemini_man_run_check_shoot:  lda     joy1_press          ; read button presses
        and     #BTN_B                  ; B button pressed?
        beq     gemini_man_run_move               ; no B press → keep running
        jsr     face_player             ; face player
        jsr     set_sprite_hflip
        lda     #$34                    ; set shooting anim
        jmp     reset_sprite_anim       ; apply shooting anim

; --- running: move horizontally, check right wall ---
gemini_man_run_move:  jsr     move_horizontal_facing           ; move horizontally
        lda     #$D8                    ; right boundary
        cmp     ent_x_px,x              ; past right boundary?
        bcs     gemini_man_run_rts               ; not at boundary yet
        sta     ent_x_px,x              ; clamp to boundary
        lda     ent_timer,x             ; check clone status
        bpl     gemini_man_reverse_dual               ; clone alive → reverse
        jmp     gemini_man_set_solo_speed               ; solo → set new velocity

; --- reverse direction at wall (dual mode) ---
gemini_man_reverse_dual:  dec     ent_status,x        ; back to phase 1 (dual)
        jsr     flip_direction               ; flip direction
        lda     #$33                    ; set run anim
        jmp     reset_sprite_anim       ; apply anim and return

gemini_man_run_rts:  rts                         ; return

; --- shooting anim logic: spawn Gemini Laser ---
gemini_man_shoot_anim:  lda     ent_anim_frame,x    ; check anim frame
        bne     gemini_man_shoot_check_end               ; not frame 0 → continue
        lda     ent_anim_state,x        ; check anim state
        cmp     #$01                    ; at shoot frame 1?
        bne     gemini_man_shoot_check_end               ; not state 1 → continue
        jmp     gemini_man_spawn_laser               ; spawn Gemini Laser projectile

gemini_man_shoot_check_end:  lda     ent_anim_frame,x    ; check anim frame
        cmp     #$04                    ; frame 4 = last frame?
        bne     gemini_man_run_rts               ; not done yet → return
        lda     ent_anim_state,x        ; check anim state
        cmp     #$02                    ; shoot anim done?
        bne     gemini_man_run_rts
        lda     #$01                    ; facing = right
        sta     ent_facing,x            ; face right
        jsr     set_sprite_hflip        ; update sprite flip
        lda     #$35                    ; resume running anim
        jmp     reset_sprite_anim

; --- phase 2: solo Gemini Man ---
gemini_man_solo_phase:  ldy     #$00                ; no floor check
        jsr     move_vertical_gravity   ; apply gravity
        bcs     gemini_man_solo_ground               ; on ground
        lda     #$00                    ; in air → reset frame, move horizontally
        sta     ent_anim_frame,x
        jmp     move_horizontal_facing

; --- on ground: solo behavior ---
gemini_man_solo_ground:  lda     ent_anim_id,x
        cmp     #$34                    ; shooting anim?
        beq     gemini_man_solo_shoot_anim               ; yes → shooting logic
        lda     ent_timer,x             ; shoot cooldown?
        bne     gemini_man_solo_dec_timer               ; timer active → decrement
        lda     $0310                   ; check if entity slot 0 active
        bmi     gemini_man_solo_check_anim               ; active → skip shooting
        jsr     face_player             ; face player
        jsr     set_sprite_hflip        ; update sprite flip
        lda     #$34                    ; start shooting anim
        jmp     reset_sprite_anim

gemini_man_solo_dec_timer:  dec     ent_timer,x         ; count down shoot cooldown
gemini_man_solo_check_anim:  lda     ent_anim_id,x
        cmp     #$35                    ; running anim?
        beq     gemini_man_solo_run               ; yes → run
        lda     ent_anim_state,x        ; transition to running anim
        beq     gemini_man_solo_rts               ; anim not started → rts
        lda     ent_anim_frame,x        ; check anim frame
        cmp     #$04                    ; frame 4 = last frame?
        bne     gemini_man_solo_rts               ; not done → rts
        lda     #$35                    ; running anim ID
        jsr     reset_sprite_anim       ; set running anim
gemini_man_solo_run:  jsr     move_horizontal_facing           ; move horizontally
        bcc     gemini_man_solo_check_jump               ; no wall hit → check jump
        jsr     flip_direction               ; hit wall → reverse direction
gemini_man_solo_check_jump:  lda     joy1_press
        and     #BTN_B                  ; B pressed → jump
        beq     gemini_man_solo_rts               ; no B press → rts
        lda     #$AB                    ; jump velocity sub = $AB
        sta     ent_yvel_sub,x          ; Y velocity = $05.AB (jump)
        lda     #$05
        sta     ent_yvel,x
        lda     #$33                    ; set jump anim
        jmp     reset_sprite_anim

; --- solo shooting anim: spawn Gemini Laser or scatter shot ---
gemini_man_solo_shoot_anim:  lda     ent_anim_frame,x    ; check anim frame
        bne     gemini_man_solo_shoot_end               ; not frame 0 → check end
        lda     ent_anim_state,x        ; check anim state
        cmp     #$01                    ; state 1 = shoot trigger?
        bne     gemini_man_solo_shoot_end               ; no → check anim end
        jmp     gemini_man_spawn_scatter_shot               ; spawn scatter shot (3 projectiles)

gemini_man_solo_shoot_end:  lda     ent_anim_frame,x    ; check anim frame
        cmp     #$04                    ; frame 4 = last frame?
        bne     gemini_man_solo_rts               ; not done → rts
        lda     ent_anim_state,x        ; check anim state
        cmp     #$02                    ; shoot anim done?
        bne     gemini_man_solo_rts
        jsr     face_player             ; face player
        jsr     set_sprite_hflip        ; update sprite flip
        jsr     gemini_man_set_random_timer               ; set random timer for next action
        lda     #$35                    ; resume running anim
        jmp     reset_sprite_anim

gemini_man_solo_rts:  rts

; --- spawn Gemini Man clone ---
gemini_man_spawn_clone:  stx     temp_00 ; save caller X
        jsr     find_enemy_freeslot_y   ; find free enemy slot → Y
        bcs     gemini_man_spawn_clone_done               ; no slot → bail
        tya                             ; A = clone slot
        sta     ent_var1,x              ; link: original → clone
        txa                             ; A = original slot
        sta     ent_var1,y              ; link: clone → original
        lda     #$00
        sta     ent_timer,y             ; clone timer = 0
        lda     #$33
        jsr     init_child_entity       ; init child with Gemini Man anim
        lda     #$01                    ; anim state = 1 (playing)
        sta     ent_anim_state,y
        lda     #$1C                    ; HP = 28
        sta     ent_hp,y                ; clone HP = 28
        lda     #$C0                    ; active + invincible
        sta     ent_status,y            ; clone status = active + invincible
        lda     #$8A                    ; Gemini Man hitbox
        sta     ent_hitbox,y            ; clone hitbox
        lda     ent_x_px,x              ; copy position from original
        sta     ent_x_px,y              ; copy X pixel
        lda     ent_x_scr,x             ; copy X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy Y pixel
        sta     ent_y_px,y
        lda     ent_y_scr,x             ; copy Y screen
        sta     ent_y_scr,y
        lda     #$D7                    ; clone AI routine
        sta     ent_routine,y           ; clone AI routine ($D7)
        lda     ent_facing,x            ; copy facing direction
        sta     ent_facing,y
gemini_man_spawn_clone_done:  ldx     temp_00 ; restore caller X
        rts

; --- spawn Gemini Laser projectile ---
gemini_man_spawn_laser:  stx     temp_00 ; save caller X
        jsr     find_enemy_freeslot_y   ; find free enemy slot → Y
        bcs     gemini_man_spawn_laser_done               ; no slot → bail
        lda     #$40
        sta     ent_routine,y           ; generic projectile routine
        lda     #$00
        sta     ent_xvel_sub,y          ; X velocity = $04.00
        lda     #$04
        sta     ent_xvel,y
        lda     #$50                    ; Gemini Laser anim ID
        jsr     init_child_entity       ; init child with Gemini Laser anim

; --- shared: copy position from owner to child entity, offset X by facing ---
gemini_man_copy_pos_to_child:  lda     #$8B
        sta     ent_hitbox,y            ; projectile hitbox
        lda     ent_x_px,x              ; copy full position
        sta     ent_x_px,y              ; copy X pixel
        lda     ent_x_scr,x             ; copy X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy Y pixel
        sta     ent_y_px,y
        lda     ent_y_scr,x             ; copy Y screen
        sta     ent_y_scr,y
        lda     ent_facing,x            ; offset X by facing direction
        sta     ent_facing,y            ; copy facing to child
        and     #$02                    ; bit 1 = facing left?
        tax                             ; use as table index (0/2)
        lda     ent_x_px,y              ; get child X position
        clc                             ; prepare add
        adc     gemini_man_laser_xoffset_right,x ; X offset (facing right/left)
        sta     ent_x_px,y              ; apply X offset
        lda     ent_x_scr,y             ; add screen carry
        adc     gemini_man_laser_xoffset_table,x
        sta     ent_x_scr,y             ; store adjusted screen
gemini_man_spawn_laser_done:  ldx     temp_00 ; restore caller X
        rts

; --- spawn scatter shot (3 bouncing projectiles, slots $10-$12) ---
gemini_man_spawn_scatter_shot:  stx     temp_00 ; save caller X
        ldy     #$10                    ; start at entity slot $10
gemini_man_scatter_slot_loop:  lda     ent_status,y
        bmi     gemini_man_scatter_done               ; slot in use → skip remaining
        lda     #$B9
        sta     ent_routine,y           ; bouncing projectile routine
        lda     #$00
        sta     ent_xvel_sub,y          ; X velocity = $03.00
        sta     ent_yvel_sub,y          ; Y velocity = $03.00
        lda     #$03
        sta     ent_xvel,y
        sta     ent_yvel,y
        lda     #$B4
        sta     ent_timer,y             ; lifetime timer = 180 frames
        lda     #$96
        sta     ent_var1,y              ; bounce parameter = 150
        ldx     temp_00                 ; restore owner X
        lda     #$4A                    ; scatter shot anim ID
        jsr     init_child_entity       ; init child with scatter anim
        jsr     gemini_man_copy_pos_to_child               ; copy position + offset X
        lda     ent_x_px,y              ; get spawned X position
        and     #$FC                    ; align X to 4-pixel grid
        sta     ent_x_px,y              ; store aligned X
        iny                             ; next slot
        cpy     #$13                    ; spawned 3? (slots $10-$12)
        bcc     gemini_man_scatter_slot_loop               ; loop until 3 spawned
gemini_man_scatter_done:  ldx     temp_00 ; restore caller X
        rts

; --- Gemini Man data tables ---
; Random timer values for next action (phase 1 dual mode)
gemini_man_random_timer_table:  .byte   $B4,$FF
; Gemini Laser spawn X offsets (facing right / facing left)
gemini_man_laser_xoffset_right:  .byte   $0D
gemini_man_laser_xoffset_table:  .byte   $00,$F3,$FF
; =============================================================================
; GEMINI MAN CLONE AI ($D7)
; =============================================================================
; Runs the same AI as main_gemini_man but with special init logic.
; Waits until the original Gemini Man reaches X > $30 before moving,
; then sets its own X velocity and delegates to main_gemini_man.
; =============================================================================

gemini_man_clone_main:  lda     ent_status,x        ; get clone status
        and     #$0F                    ; check phase bits
        bne     gemini_man_clone_run_ai               ; already initialized → run main AI
        lda     ent_timer,x             ; check init flag
        bne     gemini_man_clone_set_velocity               ; timer set → start moving
        lda     ent_var1,x              ; Y = original Gemini Man slot
        tay                             ; Y = linked slot index
        lda     ent_x_px,y              ; get original's X pos
        cmp     #$30                    ; original past X=$30?
        bcc     gemini_man_clone_set_velocity               ; yes → start moving
        lda     #$00                    ; no → hold position
        sta     ent_anim_frame,x
        rts                             ; wait for original

gemini_man_clone_set_velocity:  lda     #$2D                ; set X velocity = $03.2D
        sta     ent_xvel_sub,x          ; set X velocity sub
        lda     #$03                    ; X velocity = 3 px/frame
        sta     ent_xvel,x              ; store X velocity
        sta     ent_timer,x             ; mark: initialized
gemini_man_clone_run_ai:  jmp     main_gemini_man     ; delegate to main Gemini Man AI

; =============================================================================
; SHADOW MAN STAGE DATA ($A99F-$BFFF)
; =============================================================================
; Compressed stage layout data for Shadow Man's stage. Bank $07 doubles as
; stage data bank ($22=$07). Includes tile maps, enemy placements, screen
; data, palette data, and tile collision attributes.
; =============================================================================

        .byte   $2E,$2A,$AA,$EA,$5F,$A0,$11,$2A
        .byte   $88,$A0,$1F,$AA,$F7,$A2,$57,$A6
        .byte   $DA,$8A,$EB,$AA,$7B,$88,$D3,$2A
        .byte   $EF,$AC,$7C,$2A,$60,$AA,$3F,$2A
        .byte   $ED,$A0,$76,$20,$E8,$28,$B7,$AA
        .byte   $9A,$AA,$F6,$AA,$36,$20,$F3,$A8
        .byte   $DB,$0C,$7F,$A2,$B7,$AA,$37,$EA
        .byte   $FF,$AA,$DB,$A8,$D7,$A8,$E6,$2A
        .byte   $2B,$A8,$A6,$22,$B7,$2A,$F5,$A0
        .byte   $FF,$AA,$F9,$28,$57,$20,$DB,$08
        .byte   $3F,$00,$FF,$AA,$DA,$AA,$6D,$AA
        .byte   $E8,$2A,$05,$A0,$FF,$AA,$F8,$AA
        .byte   $F2,$00,$01,$02,$03,$04,$05,$06
        .byte   $07,$08,$09,$0A,$0B,$0C,$0D,$0E
        .byte   $0F,$10,$11,$12,$13,$14,$15,$16
        .byte   $BD,$AA,$F7,$AA,$F9,$20,$9A,$2A
        .byte   $DB,$28,$45,$A6,$FA,$A2,$8D,$1A
        .byte   $6F,$0A,$D7,$A0,$DE,$AA,$BF,$A8
        .byte   $AB,$14,$0C,$0C,$EF,$02,$BC,$A8
        .byte   $F2,$A2,$D7,$A8,$E9,$28,$5F,$08
        .byte   $E1,$40,$40,$40,$61,$40,$66,$67
        .byte   $20,$20,$00,$A0,$7F,$88,$EC,$A2
        .byte   $8E,$8A,$BA,$A0,$DE,$A8,$A7,$28
        .byte   $5C,$A2,$BE,$20,$7D,$AA,$FD,$8A
        .byte   $EB,$06,$00,$06,$00,$06,$00,$2D
        .byte   $00,$0E,$00,$08,$00,$30,$00,$00
        .byte   $00,$29,$00,$00,$7E,$28,$EB,$2A
        .byte   $FA,$2A,$EC,$8A,$B7,$2A,$FE,$0A
        .byte   $B5,$5C,$5E,$0F,$27,$26,$06,$0F
        .byte   $26,$16,$06,$0F,$0C,$09,$01,$0F
        .byte   $30,$28,$07,$8A,$00,$00,$00,$2A
        .byte   $7E,$A0,$B3,$20,$FF,$A0,$F7,$AA
        .byte   $F7,$20,$FE,$88,$60,$88,$DB,$22
        .byte   $A6,$0A,$F6,$2A,$EC,$A2,$FF,$82
        .byte   $3D,$A0,$8D,$AA,$2C,$AA,$DD,$02
        .byte   $EF,$88,$FF,$88,$EE,$A2,$E3,$AA
        .byte   $65,$2A,$8A,$2B,$94,$A2,$F3,$A8
        .byte   $7B,$AA,$75,$22,$C3,$88,$FE,$E8
        .byte   $D6,$A8,$F9,$AA,$6E,$A1,$F7,$A8
        .byte   $A4,$28,$DF,$02,$FF,$20,$F3,$A2
        .byte   $FD,$2A,$8E,$A8,$B7,$22,$97,$A8
        .byte   $7D,$0A,$EF,$AC,$A7,$2A,$7E,$A8
        .byte   $FE,$06,$07,$22,$9D,$22,$31,$A2
        .byte   $FF,$0D,$15,$FF,$9C,$AA,$77,$88
        .byte   $F9,$01,$03,$03,$04,$05,$06,$07
        .byte   $07,$07,$07,$08,$08,$08,$08,$09
        .byte   $09,$09,$0A,$0A,$0A,$0B,$0B,$0B
        .byte   $0C,$0C,$0C,$0D,$0D,$0D,$0D,$0D
        .byte   $0E,$0E,$0F,$0F,$10,$10,$11,$11
        .byte   $11,$12,$12,$12,$13,$14,$16,$FF
        .byte   $A0,$88,$FF,$88,$BE,$AA,$7A,$2A
        .byte   $F7,$A8,$D7,$A2,$FB,$20,$18,$28
        .byte   $F0,$A8,$AF,$2A,$D5,$2A,$B9,$0A
        .byte   $FD,$82,$B7,$28,$2F,$28,$E7,$AB
        .byte   $B3,$AA,$67,$22,$EF,$8A,$A7,$AA
        .byte   $BA,$AA,$DF,$AA,$5D,$AA,$F7,$AA
        .byte   $E3,$AA,$E9,$A2,$6E,$08,$EE,$A8
        .byte   $AC,$A0,$0F,$22,$CF,$82,$50,$EA
        .byte   $BE,$80,$CB,$28,$FB,$80,$47,$AA
        .byte   $F1,$2A,$76,$A0,$FF,$A8,$7F,$A8
        .byte   $2F,$22,$ED,$A0,$25,$2A,$5A,$A2
        .byte   $3F,$0A,$F7,$A0,$EE,$A8,$B1,$00
        .byte   $FF,$8A,$ED,$62,$ED,$AA,$5F,$A8
        .byte   $A7,$92,$F3,$A2,$FD,$AA,$6F,$A0
        .byte   $F7,$2A,$EA,$A2,$E1,$88,$10,$28
        .byte   $FA,$AA,$B7,$80,$75,$8A,$CF,$2A
        .byte   $89,$80,$37,$8A,$A2,$88,$7B,$AA
        .byte   $E2,$0A,$D7,$A8,$E6,$A2,$A9,$8A
        .byte   $5F,$1A,$2D,$88,$2E,$A8,$D9,$22
        .byte   $32,$8A,$5F,$2A,$97,$88,$7E,$AB
        .byte   $FE,$8A,$DD,$28,$77,$80,$4F,$AA
        .byte   $B0,$AA,$EF,$00,$3D,$2A,$EE,$BA
        .byte   $FF,$2A,$D6,$08,$44,$00,$3D,$A8
        .byte   $6E,$AA,$DD,$82,$06,$AA,$FB,$AA
        .byte   $E7,$28,$FD,$A2,$F1,$A0,$9D,$28
        .byte   $29,$AA,$38,$AA,$EF,$AA,$71,$A2
        .byte   $61,$80,$94,$F4,$A8,$30,$D0,$10
        .byte   $90,$B0,$D0,$10,$28,$A8,$D0,$48
        .byte   $68,$F0,$68,$88,$A8,$70,$C8,$F0
        .byte   $10,$B0,$C8,$50,$70,$90,$B0,$D0
        .byte   $78,$A8,$28,$E8,$B8,$E8,$48,$88
        .byte   $E8,$38,$88,$F8,$E8,$88,$C0,$FF
        .byte   $10,$00,$14,$0C,$00,$40,$40,$00
        .byte   $44,$8A,$04,$44,$30,$81,$53,$88
        .byte   $40,$08,$44,$00,$10,$84,$01,$00
        .byte   $00,$04,$04,$20,$00,$85,$11,$91
        .byte   $10,$34,$00,$04,$04,$50,$00,$E0
        .byte   $20,$52,$40,$80,$00,$CA,$41,$10
        .byte   $00,$84,$40,$09,$42,$1A,$14,$00
        .byte   $00,$11,$14,$85,$24,$01,$01,$22
        .byte   $40,$91,$44,$01,$50,$24,$10,$AC
        .byte   $00,$0C,$02,$00,$44,$06,$13,$32
        .byte   $40,$83,$00,$44,$44,$10,$00,$04
        .byte   $00,$20,$40,$41,$00,$24,$01,$C5
        .byte   $05,$04,$04,$20,$10,$94,$40,$02
        .byte   $00,$09,$11,$45,$10,$28,$00,$99
        .byte   $14,$29,$40,$00,$70,$C1,$04,$BC
        .byte   $04,$C1,$15,$20,$40,$06,$00,$21
        .byte   $41,$1B,$44,$32,$00,$41,$94,$80
        .byte   $34,$12,$55,$20,$54,$60,$02,$02
        .byte   $41,$41,$04,$A0,$04,$C0,$05,$13
        .byte   $00,$00,$00,$20,$10,$88,$40,$24
        .byte   $40,$94,$42,$08,$01,$01,$00,$80
        .byte   $40,$84,$04,$00,$10,$02,$10,$83
        .byte   $00,$14,$64,$D0,$51,$4A,$15,$66
        .byte   $55,$22,$51,$18,$05,$8A,$01,$51
        .byte   $44,$00,$10,$98,$44,$A6,$91,$8B
        .byte   $01,$89,$50,$CB,$40,$00,$10,$D0
        .byte   $4C,$84,$98,$B8,$84,$00,$78,$78
        .byte   $B8,$98,$68,$68,$20,$74,$78,$20
        .byte   $74,$58,$54,$20,$74,$78,$20,$74
        .byte   $58,$78,$20,$5C,$5C,$5C,$5C,$B8
        .byte   $B8,$98,$78,$58,$04,$04,$04,$04
        .byte   $04,$04,$04,$98,$18,$18,$00,$FF
        .byte   $40,$82,$00,$0A,$40,$A0,$10,$01
        .byte   $44,$21,$08,$41,$51,$86,$00,$40
        .byte   $74,$52,$50,$0A,$00,$A0,$00,$88
        .byte   $01,$00,$01,$02,$00,$72,$00,$00
        .byte   $05,$21,$00,$10,$11,$00,$50,$2A
        .byte   $04,$01,$10,$C2,$00,$21,$01,$80
        .byte   $41,$81,$41,$04,$00,$80,$04,$65
        .byte   $10,$A6,$04,$AC,$01,$1C,$00,$33
        .byte   $40,$F0,$05,$30,$44,$81,$02,$89
        .byte   $01,$A4,$00,$DC,$04,$BB,$04,$24
        .byte   $01,$88,$10,$01,$00,$40,$04,$00
        .byte   $00,$00,$00,$40,$00,$C8,$00,$23
        .byte   $01,$01,$10,$1B,$00,$60,$00,$80
        .byte   $35,$29,$00,$C0,$00,$02,$04,$C0
        .byte   $04,$18,$10,$51,$01,$04,$41,$F5
        .byte   $00,$90,$54,$A0,$00,$02,$01,$9C
        .byte   $00,$84,$00,$66,$10,$62,$44,$00
        .byte   $10,$8C,$11,$09,$00,$41,$50,$88
        .byte   $05,$08,$00,$5A,$04,$0A,$00,$80
        .byte   $10,$02,$00,$10,$00,$80,$00,$38
        .byte   $00,$21,$12,$14,$00,$68,$04,$21
        .byte   $00,$32,$00,$E9,$01,$12,$11,$54
        .byte   $04,$1F,$00,$2E,$11,$4B,$00,$C0
        .byte   $41,$2B,$00,$85,$11,$05,$1F,$28
        .byte   $00,$6D,$10,$B3,$10,$56,$14,$C2
        .byte   $45,$8E,$40,$31,$14,$23,$01,$4E
        .byte   $09,$02,$26,$26,$12,$3E,$36,$36
        .byte   $36,$36,$36,$36,$3A,$37,$36,$3A
        .byte   $37,$36,$37,$3A,$37,$36,$3A,$37
        .byte   $36,$36,$3A,$53,$53,$53,$53,$26
        .byte   $26,$26,$26,$26,$38,$38,$38,$38
        .byte   $38,$38,$38,$26,$08,$08,$4E,$FF
        .byte   $04,$08,$00,$91,$14,$31,$50,$2B
        .byte   $00,$2A,$41,$70,$10,$10,$00,$48
        .byte   $11,$28,$40,$20,$40,$D3,$00,$18
        .byte   $00,$00,$10,$00,$40,$21,$01,$44
        .byte   $00,$00,$00,$62,$10,$10,$04,$21
        .byte   $00,$30,$10,$A1,$02,$90,$00,$68
        .byte   $50,$98,$00,$8F,$45,$0A,$14,$22
        .byte   $40,$C6,$C0,$03,$51,$D8,$40,$E4
        .byte   $70,$42,$10,$28,$04,$54,$10,$83
        .byte   $00,$C2,$94,$84,$45,$BA,$80,$16
        .byte   $C0,$20,$01,$40,$00,$80,$00,$02
        .byte   $00,$64,$40,$18,$51,$2A,$00,$62
        .byte   $11,$35,$00,$80,$04,$08,$04,$40
        .byte   $04,$00,$41,$65,$10,$02,$00,$01
        .byte   $D9,$84,$10,$00,$00,$59,$10,$22
        .byte   $64,$82,$10,$C0,$00,$02,$01,$08
        .byte   $40,$C2,$00,$0A,$00,$0E,$45,$98
        .byte   $10,$1C,$00,$10,$20,$35,$01,$CA
        .byte   $10,$00,$00,$00,$40,$10,$00,$62
        .byte   $00,$E0,$10,$20,$01,$8A,$05,$40
        .byte   $00,$42,$90,$01,$18,$00,$04,$29
        .byte   $05,$50,$01,$00,$00,$30,$50,$54
        .byte   $48,$C5,$14,$26,$10,$ED,$45,$9A
        .byte   $00,$88,$85,$EA,$01,$98,$34,$52
        .byte   $15,$06,$14,$34,$45,$50,$14,$32
        .byte   $45,$00,$44,$73,$40,$38,$44,$89
        .byte   $40,$00,$01,$02,$03,$01,$02,$03
        .byte   $00,$04,$05,$05,$05,$05,$05,$05
        .byte   $04,$06,$07,$07,$07,$07,$07,$07
        .byte   $06,$08,$09,$09,$09,$09,$09,$09
        .byte   $08,$0A,$09,$09,$09,$09,$09,$09
        .byte   $0A,$00,$09,$09,$0B,$0B,$09,$09
        .byte   $00,$04,$09,$09,$0C,$0C,$09,$09
        .byte   $04,$06,$09,$09,$0D,$0D,$09,$09
        .byte   $06,$08,$0E,$0E,$0D,$0D,$0E,$0E
        .byte   $08,$0A,$0E,$0E,$0F,$0F,$0E,$0E
        .byte   $0A,$00,$0E,$0E,$10,$10,$0E,$0E
        .byte   $00,$04,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $04,$06,$0E,$0E,$11,$12,$0E,$0E
        .byte   $06,$13,$14,$14,$15,$16,$14,$14
        .byte   $13,$17,$18,$19,$0E,$0E,$18,$19
        .byte   $17,$1A,$1B,$1C,$0E,$0E,$1B,$1C
        .byte   $1A,$04,$1D,$1E,$09,$09,$1D,$1E
        .byte   $04,$06,$1F,$20,$09,$09,$1F,$20
        .byte   $06,$13,$01,$03,$09,$09,$01,$03
        .byte   $13,$17,$21,$21,$09,$09,$18,$19
        .byte   $17,$22,$23,$23,$0B,$0B,$1B,$1C
        .byte   $22,$24,$0E,$0E,$18,$19,$25,$26
        .byte   $24,$08,$0E,$0E,$1B,$1C,$1F,$20
        .byte   $08,$0A,$0E,$0E,$27,$28,$27,$28
        .byte   $0A,$17,$09,$09,$1F,$20,$25,$26
        .byte   $1A,$22,$09,$09,$01,$03,$01,$03
        .byte   $1A,$24,$09,$09,$29,$2A,$2B,$2C
        .byte   $2D,$08,$09,$09,$2E,$2F,$30,$31
        .byte   $32,$0A,$09,$09,$33,$34,$35,$36
        .byte   $37,$00,$09,$09,$38,$0C,$39,$3A
        .byte   $3B,$04,$3C,$3C,$3D,$3E,$18,$3F
        .byte   $19,$06,$40,$40,$41,$41,$42,$43
        .byte   $44,$45,$46,$47,$20,$25,$48,$49
        .byte   $17,$25,$47,$46,$49,$45,$4A,$4B
        .byte   $22,$01,$02,$02,$03,$01,$02,$03
        .byte   $24,$4C,$4D,$4E,$32,$4C,$4D,$32
        .byte   $08,$4F,$35,$50,$37,$4F,$35,$51
        .byte   $0A,$0C,$0C,$0C,$0C,$0C,$0C,$52
        .byte   $00,$24,$24,$24,$24,$24,$24,$3B
        .byte   $04,$08,$08,$0D,$0A,$0A,$0A,$3B
        .byte   $06,$24,$53,$0F,$54,$54,$54,$3B
        .byte   $08,$08,$0A,$32,$32,$4D,$21,$3B
        .byte   $0A,$0A,$54,$55,$34,$56,$3B,$3B
        .byte   $00,$00,$21,$57,$58,$59,$3A,$3B
        .byte   $04,$04,$3B,$5A,$5B,$5C,$5D,$3B
        .byte   $06,$06,$3B,$5A,$5E,$5F,$5D,$3B
        .byte   $13,$13,$60,$61,$18,$3F,$3F,$19
        .byte   $17,$17,$3B,$62,$42,$43,$43,$44
        .byte   $22,$00,$3B,$0A,$63,$64,$65,$66
        .byte   $67,$04,$3B,$54,$01,$02,$02,$03
        .byte   $0F,$06,$3B,$21,$21,$68,$69,$4D
        .byte   $21,$13,$3B,$6A,$50,$6B,$34,$35
        .byte   $50,$17,$18,$3F,$6C,$6D,$6E,$3F
        .byte   $19,$22,$1B,$6F,$6F,$70,$43,$43
        .byte   $44,$24,$25,$47,$46,$70,$47,$46
        .byte   $20,$08,$01,$02,$71,$72,$73,$02
        .byte   $03,$67,$09,$09,$09,$09,$74,$34
        .byte   $35,$0F,$09,$09,$09,$09,$2E,$31
        .byte   $39,$75,$09,$09,$09,$09,$76,$50
        .byte   $77,$78,$09,$09,$09,$09,$79,$7A
        .byte   $7B,$0C,$09,$09,$09,$09,$2E,$67
        .byte   $7C,$08,$09,$09,$09,$09,$7D,$7E
        .byte   $1D,$06,$3C,$3C,$3C,$3C,$7F,$80
        .byte   $81,$13,$40,$40,$40,$40,$41,$41
        .byte   $1F,$82,$09,$09,$83,$84,$34,$35
        .byte   $50,$82,$09,$09,$83,$30,$31,$39
        .byte   $3A,$82,$09,$09,$85,$35,$36,$77
        .byte   $4F,$86,$09,$09,$83,$39,$3A,$87
        .byte   $31,$88,$09,$09,$83,$89,$18,$19
        .byte   $3B,$1E,$09,$09,$18,$19,$1B,$1C
        .byte   $0C,$49,$3C,$3C,$1B,$1C,$1F,$49
        .byte   $08,$20,$40,$40,$1F,$49,$81,$26
        .byte   $06,$6B,$34,$35,$50,$78,$09,$09
        .byte   $09,$30,$31,$39,$3A,$82,$09,$09
        .byte   $09,$35,$36,$77,$4F,$8A,$09,$09
        .byte   $09,$39,$3A,$87,$31,$8B,$09,$09
        .byte   $0C,$89,$5D,$87,$0C,$8B,$09,$0C
        .byte   $08,$89,$4F,$0C,$08,$18,$19,$08
        .byte   $06,$18,$19,$08,$06,$1B,$1C,$06
        .byte   $13,$1B,$1C,$06,$13,$1F,$49,$13
        .byte   $17,$83,$84,$34,$35,$50,$6B,$34
        .byte   $35,$83,$30,$31,$39,$3A,$30,$31
        .byte   $39,$85,$35,$36,$77,$4F,$35,$8C
        .byte   $77,$83,$39,$3A,$0C,$31,$39,$67
        .byte   $87,$83,$89,$0C,$08,$18,$19,$53
        .byte   $87,$0C,$89,$08,$06,$1B,$1C,$0A
        .byte   $0C,$08,$8D,$06,$13,$1F,$20,$8E
        .byte   $8F,$0D,$6F,$13,$17,$25,$49,$7F
        .byte   $7F,$50,$82,$09,$09,$09,$09,$83
        .byte   $6B,$3A,$82,$09,$09,$09,$09,$83
        .byte   $30,$4F,$8A,$09,$09,$09,$09,$85
        .byte   $35,$3B,$8B,$09,$09,$09,$09,$83
        .byte   $39,$31,$8B,$09,$18,$19,$09,$83
        .byte   $0C,$3B,$8B,$0C,$1B,$1C,$18,$19
        .byte   $08,$60,$90,$08,$1F,$20,$1B,$1C
        .byte   $06,$6F,$6F,$06,$25,$49,$1F,$20
        .byte   $13,$34,$35,$50,$6B,$91,$92,$31
        .byte   $13,$31,$39,$3A,$30,$31,$93,$3B
        .byte   $17,$36,$77,$4F,$35,$36,$94,$3B
        .byte   $22,$0C,$87,$31,$39,$3A,$95,$3B
        .byte   $24,$08,$0C,$96,$89,$18,$19,$3B
        .byte   $08,$06,$08,$18,$19,$1B,$1C,$3B
        .byte   $0A,$13,$06,$1B,$1C,$25,$49,$3B
        .byte   $00,$17,$13,$27,$97,$27,$97,$3B
        .byte   $04,$00,$54,$01,$03,$01,$03,$3B
        .byte   $24,$67,$32,$98,$99,$21,$9A,$3B
        .byte   $08,$24,$9B,$9B,$39,$50,$9C,$3B
        .byte   $0A,$08,$9B,$9D,$9E,$9E,$9E,$9E
        .byte   $54,$0A,$55,$2C,$99,$21,$68,$9F
        .byte   $99,$00,$3B,$3B,$39,$3A,$87,$31
        .byte   $39,$67,$A0,$A0,$A1,$18,$3F,$3F
        .byte   $19,$24,$6F,$6F,$0D,$1B,$6F,$6F
        .byte   $44,$18,$19,$24,$25,$A2,$49,$08
        .byte   $1F,$1B,$1C,$08,$1F,$A2,$26,$0A
        .byte   $01,$1F,$26,$0A,$01,$02,$03,$54
        .byte   $4D,$01,$03,$54,$4D,$21,$A3,$9F
        .byte   $35,$21,$68,$9F,$35,$50,$77,$3B
        .byte   $39,$3A,$87,$31,$39,$38,$18,$3F
        .byte   $19,$18,$3F,$3F,$19,$67,$1B,$6F
        .byte   $1C,$1B,$6F,$6F,$1C,$24,$01,$02
        .byte   $03,$A2,$26,$0A,$01,$02,$03,$54
        .byte   $3B,$02,$03,$54,$4D,$21,$A3,$4C
        .byte   $3B,$21,$A3,$9F,$35,$50,$77,$4F
        .byte   $A4,$50,$77,$31,$39,$38,$18,$19
        .byte   $38,$38,$18,$3F,$19,$67,$1B,$1C
        .byte   $67,$67,$1B,$6F,$1C,$24,$01,$03
        .byte   $24,$24,$01,$02,$03,$08,$18,$19
        .byte   $08,$A5,$18,$3F,$19,$0A,$1B,$1C
        .byte   $0A,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$A7,$A6,$A7,$A6,$09,$3B
        .byte   $A8,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$A9,$AA,$A9,$AA,$09,$A9
        .byte   $AA,$AB,$AC,$AD,$AE,$AF,$AB,$AC
        .byte   $AD,$B0,$B1,$B0,$B1,$B0,$B0,$B1
        .byte   $B0,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$3B,$A8,$09,$A7,$A6,$A7,$A6
        .byte   $09,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$A9,$AA,$09,$A9,$AA,$A9,$AA
        .byte   $09,$AE,$AF,$AB,$AC,$B2,$AE,$AF
        .byte   $AB,$B1,$B0,$B0,$B1,$64,$B1,$B0
        .byte   $B0,$3B,$A6,$3B,$A6,$09,$5D,$84
        .byte   $34,$3B,$A6,$3B,$A6,$09,$5D,$30
        .byte   $31,$3B,$A8,$3B,$A8,$09,$4F,$35
        .byte   $36,$3B,$A6,$3B,$A6,$09,$3B,$39
        .byte   $3A,$3B,$A6,$3B,$A6,$09,$3B,$89
        .byte   $5D,$A9,$AA,$A9,$AA,$09,$3B,$89
        .byte   $38,$AC,$B2,$AE,$AF,$AB,$18,$19
        .byte   $67,$B1,$B0,$B1,$B0,$B0,$1B,$1C
        .byte   $24,$B3,$3B,$84,$34,$B3,$3B,$84
        .byte   $34,$39,$3A,$30,$31,$39,$3A,$30
        .byte   $31,$77,$4F,$35,$36,$77,$4F,$35
        .byte   $36,$87,$3B,$39,$3A,$87,$3B,$39
        .byte   $3A,$87,$3B,$89,$18,$3F,$19,$18
        .byte   $3F,$18,$3F,$19,$1B,$6F,$1C,$1B
        .byte   $6F,$1B,$6F,$1C,$01,$02,$03,$01
        .byte   $02,$25,$B4,$26,$18,$3F,$19,$18
        .byte   $3F,$B3,$3B,$84,$34,$B3,$3B,$3B
        .byte   $54,$39,$3A,$30,$31,$39,$3A,$3B
        .byte   $B5,$77,$4F,$35,$36,$77,$5D,$3B
        .byte   $B6,$87,$3B,$39,$3A,$38,$18,$3F
        .byte   $19,$19,$18,$3F,$19,$67,$1B,$6F
        .byte   $1C,$1C,$1B,$6F,$1C,$24,$01,$02
        .byte   $03,$03,$01,$02,$03,$08,$18,$3F
        .byte   $19,$19,$18,$3F,$19,$0A,$1B,$6F
        .byte   $1C,$01,$02,$02,$03,$01,$02,$71
        .byte   $B7,$B8,$21,$21,$21,$21,$21,$21
        .byte   $B5,$B9,$A7,$3B,$3B,$3B,$A7,$3B
        .byte   $B6,$18,$6C,$6D,$6E,$19,$18,$6C
        .byte   $BA,$1B,$6F,$70,$6F,$1C,$1B,$6F
        .byte   $BB,$01,$71,$72,$73,$03,$01,$71
        .byte   $BC,$18,$6C,$6D,$6E,$19,$18,$6C
        .byte   $BD,$1B,$6F,$70,$6F,$1C,$1B,$6F
        .byte   $BE,$BF,$73,$02,$03,$01,$02,$71
        .byte   $C0,$C1,$C2,$32,$C3,$C4,$C2,$21
        .byte   $C5,$C6,$C7,$C8,$C9,$96,$C7,$CA
        .byte   $CB,$CC,$CD,$CE,$CF,$5D,$CD,$CE
        .byte   $D0,$D1,$C9,$96,$87,$D2,$C9,$D3
        .byte   $D4,$D5,$89,$5D,$87,$D6,$89,$95
        .byte   $D7,$D8,$6E,$3F,$19,$18,$3F,$6C
        .byte   $BD,$D9,$6F,$6F,$1C,$1B,$6F,$6F
        .byte   $BE,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$1D,$1E,$25,$26,$7C,$7D,$1B
        .byte   $05,$6E,$6E,$05,$05,$7E,$7F,$05
        .byte   $1C,$09,$0A,$8E,$8F,$34,$34,$61
        .byte   $61,$86,$87,$11,$12,$69,$69,$14
        .byte   $15,$11,$12,$8E,$8F,$16,$17,$17
        .byte   $16,$19,$1A,$25,$26,$16,$17,$24
        .byte   $23,$01,$02,$09,$0A,$11,$12,$11
        .byte   $12,$17,$16,$16,$17,$19,$1A,$2A
        .byte   $2B,$36,$35,$16,$17,$17,$23,$16
        .byte   $70,$24,$16,$71,$17,$8E,$8F,$19
        .byte   $1A,$17,$16,$23,$24,$17,$35,$16
        .byte   $17,$36,$16,$16,$17,$25,$26,$1D
        .byte   $1E,$0C,$04,$6C,$65,$04,$0D,$66
        .byte   $6F,$25,$26,$25,$26,$74,$07,$74
        .byte   $0F,$06,$77,$0E,$77,$74,$0F,$74
        .byte   $00,$0E,$77,$10,$77,$74,$00,$74
        .byte   $08,$00,$77,$00,$77,$34,$34,$33
        .byte   $33,$25,$26,$09,$0A,$61,$61,$15
        .byte   $14,$8E,$8F,$86,$87,$74,$00,$74
        .byte   $00,$10,$77,$00,$77,$74,$00,$7C
        .byte   $7D,$10,$77,$7E,$7F,$38,$34,$2F
        .byte   $33,$55,$34,$43,$33,$34,$57,$4C
        .byte   $50,$34,$56,$47,$40,$25,$26,$2A
        .byte   $2B,$2F,$33,$2F,$33,$54,$52,$32
        .byte   $48,$43,$4F,$43,$4F,$33,$33,$32
        .byte   $33,$34,$56,$33,$48,$2F,$33,$2F
        .byte   $4C,$33,$48,$47,$40,$44,$45,$41
        .byte   $42,$46,$46,$33,$33,$46,$53,$33
        .byte   $33,$01,$02,$1D,$1E,$49,$4A,$4E
        .byte   $4F,$4B,$52,$48,$48,$33,$33,$33
        .byte   $33,$04,$04,$3B,$3C,$80,$81,$8C
        .byte   $8D,$82,$83,$8C,$8D,$04,$04,$6D
        .byte   $6D,$30,$31,$0E,$0F,$8A,$8B,$8A
        .byte   $8B,$74,$06,$74,$0E,$07,$06,$0F
        .byte   $0E,$07,$77,$0F,$77,$74,$08,$74
        .byte   $10,$00,$08,$00,$00,$00,$08,$00
        .byte   $08,$10,$00,$00,$08,$08,$77,$10
        .byte   $77,$00,$00,$00,$00,$08,$77,$00
        .byte   $77,$56,$56,$48,$48,$55,$57,$43
        .byte   $4F,$34,$34,$32,$33,$48,$48,$58
        .byte   $40,$46,$46,$33,$32,$46,$53,$33
        .byte   $32,$4B,$51,$58,$47,$8E,$8F,$11
        .byte   $12,$1D,$1E,$2A,$2B,$33,$48,$33
        .byte   $58,$43,$4F,$41,$42,$33,$33,$33
        .byte   $59,$33,$33,$46,$46,$49,$4A,$4D
        .byte   $4F,$33,$43,$33,$43,$59,$46,$43
        .byte   $33,$53,$4F,$33,$4F,$48,$48,$48
        .byte   $48,$43,$33,$43,$33,$33,$4F,$33
        .byte   $4F,$04,$04,$34,$34,$0B,$02,$1D
        .byte   $1E,$09,$0A,$19,$1A,$74,$0E,$74
        .byte   $00,$0F,$0E,$00,$08,$0F,$0E,$00
        .byte   $00,$0F,$77,$10,$77,$09,$0A,$11
        .byte   $12,$56,$57,$48,$4F,$34,$56,$32
        .byte   $48,$33,$59,$33,$43,$53,$4F,$4C
        .byte   $50,$04,$04,$6D,$66,$0D,$0C,$6F
        .byte   $6C,$04,$04,$65,$6D,$06,$07,$0E
        .byte   $0F,$77,$74,$77,$74,$6E,$7E,$05
        .byte   $05,$7F,$7C,$1C,$1B,$7D,$6E,$05
        .byte   $05,$2F,$48,$2F,$58,$56,$38,$48
        .byte   $2F,$2F,$59,$2F,$43,$4D,$4F,$48
        .byte   $4F,$53,$2F,$33,$2F,$2F,$43,$2F
        .byte   $54,$33,$33,$01,$02,$48,$4F,$0C
        .byte   $04,$6C,$65,$74,$07,$01,$02,$80
        .byte   $81,$8E,$8F,$82,$83,$2D,$2E,$8A
        .byte   $8B,$8A,$8B,$8C,$8D,$74,$08,$74
        .byte   $00,$33,$2F,$33,$2F,$2F,$48,$2F
        .byte   $48,$33,$4F,$4C,$50,$2F,$48,$2F
        .byte   $5C,$33,$2F,$04,$0D,$48,$4F,$48
        .byte   $4F,$66,$6F,$06,$77,$4E,$4F,$4E
        .byte   $4F,$33,$2F,$5A,$2F,$5B,$2F,$5B
        .byte   $2F,$46,$46,$01,$02,$04,$04,$5E
        .byte   $57,$1D,$1E,$80,$81,$11,$12,$82
        .byte   $83,$04,$04,$34,$38,$33,$33,$47
        .byte   $47,$4E,$33,$5F,$33,$4E,$33,$4E
        .byte   $33,$4D,$33,$48,$33,$48,$33,$48
        .byte   $33,$33,$33,$4B,$52,$08,$77,$7E
        .byte   $7F,$34,$56,$33,$5C,$56,$57,$41
        .byte   $42,$56,$34,$48,$33,$33,$48,$33
        .byte   $48,$53,$33,$33,$33,$0C,$04,$1B
        .byte   $05,$04,$04,$05,$05,$56,$56,$58
        .byte   $40,$04,$04,$00,$00,$0B,$02,$09
        .byte   $0A,$08,$00,$10,$00,$5E,$57,$4E
        .byte   $4F,$33,$33,$5A,$33,$11,$12,$19
        .byte   $1A,$33,$17,$33,$16,$32,$32,$33
        .byte   $33,$32,$17,$33,$16,$03,$03,$00
        .byte   $00,$03,$17,$00,$16,$23,$24,$30
        .byte   $31,$70,$71,$06,$07,$3D,$24,$06
        .byte   $31,$3E,$70,$06,$07,$71,$24,$06
        .byte   $31,$0E,$0F,$08,$00,$0F,$0F,$00
        .byte   $00,$3D,$24,$06,$30,$4E,$4F,$41
        .byte   $42,$10,$00,$00,$00,$34,$27,$33
        .byte   $27,$33,$27,$33,$27,$7F,$1D,$1C
        .byte   $2A,$72,$34,$7A,$33,$7A,$33,$7A
        .byte   $33,$0D,$01,$6F,$1D,$77,$09,$77
        .byte   $11,$7F,$8E,$1C,$86,$0D,$11,$6F
        .byte   $8E,$77,$19,$77,$25,$1E,$7C,$2B
        .byte   $1B,$7F,$86,$1C,$86,$72,$56,$7A
        .byte   $48,$34,$57,$33,$4F,$55,$57,$44
        .byte   $45,$34,$56,$46,$53,$34,$11,$33
        .byte   $8E,$7A,$48,$7A,$48,$4C,$50,$43
        .byte   $4F,$47,$40,$33,$33,$41,$42,$49
        .byte   $4A,$47,$47,$33,$33,$47,$19,$33
        .byte   $25,$02,$48,$1E,$48,$43,$4F,$44
        .byte   $45,$32,$33,$46,$46,$4E,$4F,$4D
        .byte   $4F,$33,$1D,$46,$25,$0A,$40,$12
        .byte   $33,$58,$40,$33,$33,$33,$33,$4B
        .byte   $51,$33,$09,$51,$11,$8F,$33,$87
        .byte   $33,$32,$33,$33,$33,$33,$8E,$33
        .byte   $86,$12,$0C,$8F,$6C,$1A,$74,$26
        .byte   $74,$08,$00,$00,$00,$44,$45,$01
        .byte   $02,$11,$12,$86,$87,$8E,$8F,$00
        .byte   $00,$1B,$05,$00,$00,$05,$05,$00
        .byte   $00,$1C,$1B,$00,$00,$05,$1C,$00
        .byte   $00,$19,$1A,$00,$00,$0E,$0F,$00
        .byte   $00,$8A,$8B,$00,$00,$74,$08,$00
        .byte   $00,$00,$77,$00,$00,$10,$77,$00
        .byte   $00,$74,$00,$00,$00,$77,$74,$00
        .byte   $00,$10,$08,$00,$00,$11,$12,$00
        .byte   $00,$3D,$24,$30,$31,$3E,$70,$30
        .byte   $07,$3D,$24,$31,$30,$3E,$70,$31
        .byte   $07,$77,$19,$77,$25,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$8C,$8E,$6B,$05,$06,$08
        .byte   $0A,$00,$A8,$AA,$23,$AC,$05,$28
        .byte   $2A,$00,$34,$47,$8E,$0C,$0E,$2C
        .byte   $2E,$00,$A4,$A6,$AE,$06,$9C,$7C
        .byte   $00,$34,$47,$3A,$2C,$2E,$9C,$9E
        .byte   $4E,$B4,$B6,$9C,$9E,$00,$99,$9E
        .byte   $45,$4A,$4C,$6E,$6B,$00,$01,$03
        .byte   $5E,$00,$36,$38,$01,$03,$00,$00
        .byte   $5E,$6B,$30,$32,$6B,$6B,$5B,$55
        .byte   $6B,$6B,$50,$52,$49,$6B,$55,$6B
        .byte   $5B,$5B,$5D,$5D,$55,$6B,$00,$00
        .byte   $00,$6B,$6B,$6B,$5A,$6B,$69,$00
        .byte   $6B,$6B,$79,$7A,$00,$00,$74,$76
        .byte   $00,$6B,$89,$9A,$00,$72,$76,$00
        .byte   $70,$C0,$05,$00,$74,$82,$00,$00
        .byte   $26,$C2,$06,$6B,$00,$82,$00,$00
        .byte   $80,$C4,$C6,$C8,$CA,$00,$00,$99
        .byte   $47,$00,$00,$99,$9E,$99,$9E,$99
        .byte   $DB,$70,$72,$74,$76,$78,$00,$00
        .byte   $00,$70,$72,$74,$76,$78,$00,$00
        .byte   $00,$98,$9A,$9C,$9C,$9E,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$E8,$EA,$EC,$EC,$EE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$8D,$8F,$6B,$05,$06,$09
        .byte   $0B,$10,$A9,$AB,$8D,$05,$AD,$29
        .byte   $2B,$10,$35,$48,$24,$0D,$0F,$2D
        .byte   $2F,$00,$A5,$A7,$06,$AF,$7B,$9F
        .byte   $00,$35,$48,$3B,$2D,$2F,$9D,$9F
        .byte   $4F,$B5,$B7,$9D,$9F,$00,$9D,$CB
        .byte   $46,$4B,$4D,$6F,$6B,$00,$02,$04
        .byte   $5F,$00,$37,$39,$02,$04,$00,$00
        .byte   $5F,$54,$31,$33,$5C,$56,$55,$55
        .byte   $6B,$54,$51,$53,$59,$6B,$58,$5A
        .byte   $6B,$6B,$5D,$65,$66,$67,$00,$00
        .byte   $00,$54,$64,$6B,$6B,$54,$6B,$00
        .byte   $5A,$78,$79,$6B,$00,$00,$75,$77
        .byte   $00,$98,$89,$6B,$00,$73,$76,$00
        .byte   $71,$05,$C1,$00,$22,$27,$00,$00
        .byte   $81,$06,$C3,$6B,$00,$83,$00,$00
        .byte   $81,$C5,$C7,$C9,$CB,$00,$00,$35
        .byte   $48,$00,$00,$9D,$CB,$9D,$CB,$DA
        .byte   $48,$71,$73,$75,$77,$79,$00,$00
        .byte   $00,$71,$73,$75,$77,$79,$00,$00
        .byte   $00,$99,$9B,$9D,$9D,$9F,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$E9,$EB,$ED,$ED,$EF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$9C,$9E,$44,$15,$16,$18
        .byte   $1A,$20,$B8,$BA,$9C,$BC,$15,$00
        .byte   $00,$00,$34,$47,$9E,$1C,$1E,$1E
        .byte   $1C,$00,$B4,$B6,$BE,$16,$9C,$9E
        .byte   $00,$A4,$A6,$3A,$3C,$3E,$9C,$9E
        .byte   $4E,$9C,$9E,$11,$13,$00,$99,$7C
        .byte   $45,$18,$1A,$7E,$6B,$6B,$1E,$1C
        .byte   $5E,$45,$36,$38,$3C,$3E,$00,$6D
        .byte   $5E,$55,$40,$42,$6B,$6B,$5B,$6B
        .byte   $55,$6B,$60,$62,$6B,$6B,$6B,$6B
        .byte   $5B,$5B,$6B,$6B,$6B,$6B,$6B,$6B
        .byte   $5B,$6B,$6B,$69,$5A,$6B,$5A,$6B
        .byte   $55,$6B,$89,$8A,$00,$00,$84,$00
        .byte   $00,$6B,$89,$6B,$00,$82,$00,$85
        .byte   $80,$D0,$15,$6B,$84,$82,$00,$00
        .byte   $80,$D2,$16,$6B,$86,$92,$86,$85
        .byte   $90,$D4,$D6,$D8,$9E,$00,$00,$B0
        .byte   $B2,$00,$00,$99,$9E,$A0,$A2,$99
        .byte   $47,$80,$82,$84,$86,$88,$00,$00
        .byte   $00,$80,$82,$84,$86,$88,$00,$00
        .byte   $00,$A8,$AA,$AC,$AC,$AE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$F8,$FA,$FC,$FC,$FE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$9D,$9F,$44,$15,$16,$19
        .byte   $1B,$00,$B9,$BB,$9D,$15,$BD,$21
        .byte   $00,$20,$35,$48,$9F,$1D,$1F,$1F
        .byte   $1D,$00,$B5,$B7,$16,$BF,$9D,$9F
        .byte   $00,$A5,$A7,$3B,$3D,$3F,$9D,$9F
        .byte   $4F,$9D,$9F,$12,$14,$00,$7B,$CB
        .byte   $46,$19,$1B,$7F,$6B,$6B,$1F,$1D
        .byte   $5F,$46,$37,$39,$3D,$3F,$6C,$00
        .byte   $5F,$57,$41,$43,$5C,$5A,$6B,$6B
        .byte   $55,$54,$61,$63,$54,$64,$54,$5A
        .byte   $6B,$55,$6B,$54,$6B,$6B,$5C,$54
        .byte   $6B,$68,$5C,$6B,$6B,$6A,$6B,$5A
        .byte   $5A,$88,$89,$6B,$00,$00,$00,$00
        .byte   $00,$6B,$89,$6B,$00,$83,$00,$85
        .byte   $81,$15,$D1,$6B,$00,$83,$00,$00
        .byte   $81,$16,$D3,$6B,$87,$93,$85,$87
        .byte   $91,$D5,$D7,$D9,$CB,$00,$00,$B1
        .byte   $B3,$00,$00,$9D,$CB,$A1,$A3,$35
        .byte   $48,$81,$83,$85,$87,$89,$00,$00
        .byte   $00,$81,$83,$85,$87,$89,$00,$00
        .byte   $00,$A9,$AB,$AD,$AD,$AF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$F9,$FB,$FD,$FD,$FF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$02,$13,$13,$01
        .byte   $01,$01,$13,$13,$13,$13,$13,$01
        .byte   $01,$01,$13,$13,$13,$01,$01,$01
        .byte   $01,$00,$13,$13,$13,$13,$13,$13
        .byte   $00,$13,$13,$02,$01,$01,$13,$13
        .byte   $13,$13,$13,$13,$13,$00,$13,$13
        .byte   $02,$01,$01,$02,$02,$02,$01,$01
        .byte   $43,$02,$12,$12,$01,$01,$01,$01
        .byte   $23,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$10,$10,$13,$13
        .byte   $10,$02,$02,$02,$10,$13,$13,$13
        .byte   $13,$13,$13,$12,$13,$13,$10,$10
        .byte   $13,$13,$13,$12,$13,$13,$13,$13
        .byte   $13,$13,$13,$13,$13,$00,$00,$13
        .byte   $13,$00,$00,$13,$13,$13,$13,$13
        .byte   $13,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00
