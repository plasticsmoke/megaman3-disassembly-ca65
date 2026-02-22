; =============================================================================
; MEGA MAN 3 (U) — BANK $05 — DOC ROBOT AI (BUBBLE/HEAT/QUICK/AIR)
; =============================================================================
; Mapped to $A000-$BFFF. Contains AI routines for Doc Robot encounters
; that mimic MM2 bosses. Dispatched from bank1C_1D for routine indices $B0-$BF.
; Entry points: main_doc_bubble_j, main_doc_heat_j, main_doc_quick_j,
; main_doc_air_j. Also doubles as Snake Man stage data ($22=$05).
;
; Routine index mapping (from bank1C_1D dispatch):
;   $B0 → main_doc_bubble_j  (Doc Bubble Man boss AI)
;   $B1 → main_doc_heat_j    (Doc Heat Man boss AI)
;   $B2 → main_doc_quick_j   (Doc Quick Man boss AI)
;   $B3 → main_doc_air_j     (Doc Air Man boss AI)
;   $B4 → code_A1B4          (Doc Heat projectile — moving fireball)
;   $B5 → code_A360          (Doc Bubble projectile — bouncing bubble)
;   $B6 → code_A50E          (Doc Quick boomerang)
;   $B7 → code_A68E          (Doc Air tornado — rising phase)
;   $B8 → code_A796          (Doc Air tornado — vertical movement)
;   $B9 → code_A7BE          (Doc Air tornado — diagonal bouncing)
;   $BA → code_A87D          (unused)
;   $BB → code_A021          (stub RTS)
; =============================================================================
main_doc_bubble_j:

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

L0000           := $0000
L8003           := $8003
move_right_collide           := $F580
move_right_collide_no_face           := $F588
move_left_collide           := $F5C4
move_left_collide_no_face           := $F5CC
move_down_collide           := $F606
move_up_collide           := $F642
move_vertical_gravity           := $F67C
move_sprite_right           := $F71D
move_sprite_left           := $F73B
move_sprite_down           := $F759
move_sprite_up           := $F779
reset_sprite_anim           := $F835
init_child_entity           := $F846
face_player           := $F869
set_sprite_hflip           := $F883
entity_y_dist_to_player           := $F8B3
entity_x_dist_to_player           := $F8C2
find_enemy_freeslot_y           := $FC53
calc_homing_velocity           := $FC63
divide_8bit           := $FCEB
divide_16bit           := $FD11

.segment "BANK05"

; --- Entry trampolines (dispatched from bank1C_1D entity AI) ----------------
        jmp     code_A1DD               ; $B0 — Doc Bubble Man boss AI
main_doc_heat_j:
        jmp     code_A022               ; $B1 — Doc Heat Man boss AI
main_doc_quick_j:
        jmp     code_A393               ; $B2 — Doc Quick Man boss AI
main_doc_air_j:
        jmp     code_A561               ; $B3 — Doc Air Man boss AI
        jmp     code_A1B4               ; $B4 — Heat projectile (moving fireball)
        jmp     code_A360               ; $B5 — Bubble projectile (bouncing)
        jmp     code_A50E               ; $B6 — Quick boomerang
        jmp     code_A68E               ; $B7 — Air tornado (rising phase)
        jmp     code_A796               ; $B8 — Air tornado (vertical movement)
        jmp     code_A7BE               ; $B9 — Air tornado (diagonal bouncing)
        jmp     code_A87D               ; $BA — unused
        rts                             ; $BB — stub RTS

; =============================================================================
; DOC HEAT MAN AI ($B1)
; =============================================================================
; Mimics Heat Man from MM2. Two-phase state machine:
;   Phase 0 (status low nibble == 0): init — face player, set hitbox, randomize
;     jump delay timer from table, advance to phase 1
;   Phase 1 (status low nibble == 1): jump/charge attack cycle — wait for timer,
;     then dash toward player's stored X position, stop on arrival, fire 3
;     spread projectiles (code_A12C), loop back to phase 0
; =============================================================================

code_A022:  lda     ent_status,x        ; dispatch on status low nibble
        and     #$0F
        tay
        lda     doc_heat_phase_ptr_lo_table,y ; phase handler pointer (low)
        sta     L0000
        lda     doc_heat_phase_ptr_hi_table,y ; phase handler pointer (high)
        sta     $01
        jmp     (L0000)

doc_heat_phase_ptr_lo_table:  .byte   $39,$AF ; phase handler pointers (low bytes)
doc_heat_phase_ptr_hi_table:  .byte   $A0,$A0 ; phase handler pointers (high bytes)
; --- phase 0: init / idle ---
        jsr     face_player             ; face player
        jsr     set_sprite_hflip        ; set sprite H-flip from facing
        jsr     L8003                   ; check if intro done (bank $8000 call)
        bcs     code_A06E               ; intro done — go to attack logic
        lda     ent_hp,x                ; if HP is zero, return
        beq     code_A090
        inc     ent_status,x            ; advance to phase 1
        lda     #$07
        jsr     reset_sprite_anim       ; set anim: charge-up
        lda     #$AA
        sta     ent_hitbox,x            ; set hitbox for charge state
        lda     $E5                     ; PRNG: randomize jump delay
        adc     $E4
        sta     $E4
        sta     L0000
        lda     #$03
        sta     $01
        jsr     divide_8bit             ; divide_8bit: $E4 mod 3
        ldy     $03
        lda     doc_heat_jump_delay_table,y ; lookup delay from table
        sta     ent_timer,x             ; store as jump timer
        rts

; --- attack sequence after intro ---
code_A06E:  lda     ent_anim_id,x       ; are we in the landing anim (#$02)?
        cmp     #$02
        bne     code_A091
        lda     ent_anim_frame,x        ; wait for frame 0 (loop point)
        bne     code_A0AE
        lda     ent_anim_state,x
        cmp     #$01                    ; anim state 1 → fire spread shot
        bne     code_A084
        jsr     code_A12C               ; spawn 3 spread projectiles
code_A084:  lda     ent_anim_state,x
        cmp     #$03                    ; anim state 3 → done, return to idle
        bne     code_A0AE
        lda     #$01
        jsr     reset_sprite_anim       ; set anim: idle
code_A090:  rts

code_A091:  lda     #$00                ; check if fireball entity exists
        sta     ent_anim_frame,x
        ldy     #$1F                    ; scan enemy slots $10-$1F
code_A098:  lda     ent_status,y
        bpl     code_A0A4               ; skip inactive slots
        lda     ent_anim_id,y
        cmp     #$0A                    ; is it a fireball (#$0A)?
        beq     code_A0AE               ; yes — wait for it to finish
code_A0A4:  dey
        cpy     #$0F
        bne     code_A098
        lda     #$02                    ; no fireballs — start landing anim
        jsr     reset_sprite_anim
code_A0AE:  rts

; --- phase 1: charge-up and dash toward player ---
        lda     ent_timer,x             ; wait for charge timer
        beq     code_A0C8
        dec     ent_timer,x
        bne     code_A12B               ; still counting down
        lda     #$80                    ; timer expired — begin dash
        sta     ent_hitbox,x            ; set contact-damage hitbox
        lda     #$08
        jsr     reset_sprite_anim       ; set anim: dash
        lda     #$01
        sta     ent_var2,x              ; var2 = dash sub-state (1=dashing)
code_A0C8:  lda     ent_anim_id,x
        cmp     #$09                    ; already in slide anim?
        beq     code_A0FF               ; yes — continue moving
        lda     ent_anim_frame,x
        cmp     #$06                    ; wait for frame 6 (transition point)
        bne     code_A12B
        lda     ent_anim_state,x
        cmp     ent_var2,x              ; match sub-state
        bne     code_A12B
        lda     ent_var2,x
        tay
        lda     doc_heat_anim_id_table,y ; lookup next anim from sub-state
        jsr     reset_sprite_anim
        lda     ent_var2,x
        bne     code_A0F6               ; if dashing, store target X
        dec     ent_status,x            ; done — back to phase 0
        lda     #$CA
        sta     ent_hitbox,x            ; restore idle hitbox
        rts

code_A0F6:  lda     ent_x_px            ; store player X as dash target
        sta     ent_var1,x
        jsr     face_player             ; face player
code_A0FF:  lda     ent_facing,x        ; --- move toward target X ---
        and     #$02
        beq     code_A113
        jsr     move_sprite_left        ; move left
        lda     ent_x_px,x
        cmp     ent_var1,x              ; reached target?
        bcs     code_A12B
        bcc     code_A11E               ; yes — stop
code_A113:  jsr     move_sprite_right   ; move right
        lda     ent_x_px,x
        cmp     ent_var1,x              ; reached target?
        bcc     code_A12B
code_A11E:  lda     #$08                ; arrived at target
        jsr     reset_sprite_anim       ; set anim: stop
        inc     ent_anim_state,x
        lda     #$00
        sta     ent_var2,x              ; clear sub-state
code_A12B:  rts

; --- spawn 3 spread projectiles ---
code_A12C:  jsr     entity_x_dist_to_player ; get X distance to player
        clc
        adc     #$40                    ; offset for spread angle
        sta     $0C                     ; store as base angle
        stx     $0E                     ; save parent entity index
        lda     #$02
        sta     $0F                     ; spawn 3 projectiles (2,1,0)
code_A13A:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     code_A1A3               ; no slot — done
        ldx     $0F                     ; projectile index
        lda     doc_heat_projectile_yvel_sub_table,x ; Y velocity sub-pixel
        sta     ent_yvel_sub,y
        lda     doc_heat_projectile_yvel_table,x ; Y velocity whole
        sta     ent_yvel,y
        lda     doc_heat_projectile_xspeed_div_table,x ; X speed divisor
        sta     $03
        lda     #$00
        sta     $02
        sta     L0000
        lda     $0C                     ; get spread offset for this projectile
        sec
        sbc     #$20                    ; narrow spread per projectile
        bcs     code_A161
        lda     #$00                    ; clamp to 0
code_A161:  sta     $01
        sta     $0C
        sty     $0D                     ; save child slot index
        jsr     divide_16bit            ; divide_16bit: compute X velocity
        ldy     $0D
        lda     $04
        sta     ent_xvel_sub,y          ; set X velocity sub-pixel
        lda     $05
        sta     ent_xvel,y              ; set X velocity whole
        ldx     $0E                     ; restore parent index
        lda     #$0A                    ; anim ID for fireball
        jsr     init_child_entity       ; init child entity
        lda     #$80
        sta     ent_hitbox,y            ; set projectile hitbox
        lda     #$B4
        sta     ent_routine,y           ; routine $B4 = Heat projectile AI
        lda     ent_x_px,x              ; copy position from parent
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_facing,x            ; copy facing from parent
        sta     ent_facing,y
        dec     $0F                     ; next projectile
        bpl     code_A13A
code_A1A3:  ldx     $0E                 ; restore parent index
        rts

doc_heat_jump_delay_table:  .byte   $1E,$3C,$5A ; jump delay timers (30, 60, 90 frames)
doc_heat_anim_id_table:  .byte   $01,$09 ; anim IDs per sub-state (idle, slide)
doc_heat_projectile_yvel_sub_table:  .byte   $00,$88,$54 ; Y velocity sub-pixels (3 projectiles)
doc_heat_projectile_yvel_table:  .byte   $04,$06,$08 ; Y velocity whole (3 projectiles)
doc_heat_projectile_xspeed_div_table:  .byte   $1C,$2A,$34 ; X speed divisors (3 projectiles)
; =============================================================================
; DOC HEAT PROJECTILE ($B4) — Moving Fireball
; =============================================================================
; Fireball projectile spawned by Doc Heat Man's spread shot.
; Applies gravity and moves horizontally based on facing direction.
; Despawns on wall collision or when off-screen.
; =============================================================================

code_A1B4:  ldy     #$12
        jsr     move_vertical_gravity   ; apply gravity
        bcs     code_A1DC               ; off-screen — despawn
        lda     ent_facing,x
        and     #$02
        beq     code_A1CA
        ldy     #$1F
        jsr     move_left_collide       ; move left with collision
        jmp     code_A1CF

code_A1CA:  ldy     #$1E
        jsr     move_right_collide      ; move right with collision
code_A1CF:  bcs     code_A1D7           ; wall hit — despawn
        lda     #$00
        sta     ent_anim_frame,x        ; reset anim frame
        rts

code_A1D7:  lda     #$00
        sta     ent_status,x            ; kill entity
code_A1DC:  rts

; =============================================================================
; DOC BUBBLE MAN AI ($B0)
; =============================================================================
; Mimics Bubble Man from MM2. Two-phase state machine:
;   Phase 0 (status low nibble == 0): idle — face player, wait for landing
;     anim to complete, then enter attack phase. Randomizes bubble count
;     and spawns projectiles via code_A2DD.
;   Phase 1 (status low nibble == 1): jumping/falling — move upward, drift
;     horizontally, apply gravity, land and cycle anim, loop back to phase 0
; =============================================================================

code_A1DD:  lda     ent_status,x        ; dispatch on status low nibble
        and     #$0F
        bne     code_A1F8               ; phase 1: jumping/falling
        lda     #$01                    ; --- phase 0: idle ---
        cmp     ent_anim_id,x           ; already in idle anim?
        beq     code_A252               ; yes — go to shared face-player logic
        ldy     ent_anim_state,x
        cpy     #$02                    ; landing anim done?
        bne     code_A1DC               ; no — keep waiting
        jsr     reset_sprite_anim       ; set anim: idle (#$01)
        jmp     code_A252               ; face player and check attack

; --- phase 1: jumping/falling ---
code_A1F8:  jsr     code_A252           ; face player + attack check
        lda     ent_var3,x
        and     #$F0                    ; already set downward velocity?
        bne     code_A21B
        lda     ent_y_px,x
        cmp     #$50                    ; above Y=$50? (near ceiling)
        bcs     code_A23C               ; no — move up and drift
        lda     #$00                    ; yes — start falling
        sta     ent_yvel_sub,x
        lda     #$01
        sta     ent_yvel,x              ; set downward velocity
        lda     ent_var3,x
        ora     #$10                    ; flag: falling started
        sta     ent_var3,x
code_A21B:  ldy     #$1E
        jsr     move_down_collide       ; move down with collision
        bcc     code_A1DC               ; no floor hit — keep falling
        dec     ent_status,x            ; landed — back to phase 0
        lda     #$00
        sta     ent_var2,x              ; reset bubble count
        sta     ent_var3,x              ; reset jump flags
        lda     ent_anim_state,x
        and     #$01                    ; alternate landing anim
        clc
        adc     #$01
        jsr     reset_sprite_anim       ; set landing anim
        inc     ent_anim_state,x
        rts

code_A23C:  ldy     #$1F                ; --- rising: move up ---
        jsr     move_up_collide         ; move up with collision
        lda     ent_var3,x
        and     #$01                    ; drift direction flag
        bne     code_A24D
        ldy     #$21
        jmp     move_left_collide_no_face ; move left (no facing change)

code_A24D:  ldy     #$20
        jmp     move_right_collide_no_face ; move right (no facing change)

; --- shared: face player, manage attack timer and bubble spawning ---
code_A252:  jsr     face_player         ; face player
        jsr     set_sprite_hflip        ; set sprite H-flip
        lda     ent_anim_state,x
        bne     code_A262
        lda     #$00
        sta     ent_anim_frame,x        ; reset frame if anim state 0
code_A262:  lda     ent_var1,x          ; attack cooldown timer
        beq     code_A26B
        dec     ent_var1,x              ; still cooling down
        rts

code_A26B:  lda     ent_status,x        ; --- attack decision ---
        and     #$0F
        tay                             ; Y = phase (0 or 1)
        lda     ent_var2,x              ; remaining bubbles to spawn
        beq     code_A2B6               ; none — check if we should start
        dec     ent_var2,x
        beq     code_A28D               ; last bubble spawned — initiate jump
        lda     doc_bubble_inter_projectile_delay_table,y ; set inter-bubble delay per phase
        sta     ent_var1,x
        lda     #$00
        sta     ent_anim_frame,x
        lda     #$01
        sta     ent_anim_state,x        ; set shoot anim state
        bne     code_A2DD               ; spawn a bubble
code_A28D:  tya                         ; all bubbles spawned
        bne     code_A2DC               ; if in phase 1, just return
        inc     ent_status,x            ; phase 0 → phase 1 (start jump)
        lda     ent_y_px                ; save player Y
        pha
        lda     #$50                    ; fake target Y at $50 (near ceiling)
        sta     ent_y_px
        lda     #$60
        sta     $02                     ; speed parameter
        lda     #$01
        sta     $03
        jsr     calc_homing_velocity    ; calc homing velocity toward target
        lda     ent_facing,x
        sta     ent_var3,x              ; store facing as drift direction
        pla
        sta     ent_y_px                ; restore player Y
        lda     #$1C                    ; set jumping anim
        jmp     reset_sprite_anim

; --- check Y distance to player, start attack if close ---
code_A2B6:  jsr     entity_y_dist_to_player ; get Y distance to player
        cmp     doc_bubble_distance_threshold_table,y ; close enough to attack?
        bcs     code_A2DC               ; no — too far
        tya
        beq     code_A2C5               ; phase 0: randomize bubble count
        lda     #$05                    ; phase 1: fixed 5 bubbles
        bne     code_A2D9
code_A2C5:  lda     $E6                 ; PRNG: randomize bubble count
        adc     $E7
        sta     $E6
        sta     L0000
        lda     #$03
        sta     $01
        jsr     divide_8bit             ; divide_8bit: mod 3
        lda     $03
        clc
        adc     #$02                    ; bubble count = 2..4
code_A2D9:  sta     ent_var2,x          ; store bubble count
code_A2DC:  rts

; --- spawn bubble projectile ---
code_A2DD:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     code_A2DC               ; no slot — abort
        stx     L0000                   ; save parent index
        lda     ent_status,x
        and     #$0F
        tax                             ; X = phase (0 or 1)
        stx     $01
        lda     doc_bubble_phase_yvel_sub_table,x ; Y velocity sub-pixel (per phase)
        sta     ent_yvel_sub,y
        lda     doc_bubble_phase_yvel_table,x ; Y velocity whole
        sta     ent_yvel,y
        lda     doc_bubble_phase_xvel_sub_table,x ; X velocity sub-pixel
        sta     ent_xvel_sub,y
        lda     doc_bubble_phase_xvel_table,x ; X velocity whole
        sta     ent_xvel,y
        lda     doc_bubble_projectile_routine_table,x ; routine index (per phase)
        sta     ent_routine,y
        lda     doc_bubble_projectile_anim_table,x ; anim ID (per phase)
        ldx     L0000
        jsr     init_child_entity       ; init child entity
        lda     #$80
        sta     ent_hitbox,y            ; set projectile hitbox
        lda     ent_x_px,x              ; copy position from parent
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_facing,x            ; copy facing
        sta     ent_facing,y
        ldx     $01
        beq     code_A349               ; phase 0: no X offset
        and     #$02                    ; phase 1: offset X per facing
        tax
        lda     ent_x_px,y
        clc
        adc     doc_bubble_x_offset_right,x
        sta     ent_x_px,y
        lda     ent_x_scr,y
        adc     doc_bubble_x_offset_left_table,x
        sta     ent_x_scr,y
code_A349:  ldx     L0000               ; restore parent index
        rts

; --- Bubble Man data tables (indexed by phase: 0=idle, 1=jumping) ---
doc_bubble_phase_yvel_sub_table:  .byte   $AB,$00 ; Y velocity sub-pixel
doc_bubble_phase_yvel_table:  .byte   $05,$00 ; Y velocity whole
doc_bubble_phase_xvel_sub_table:  .byte   $A8,$00 ; X velocity sub-pixel
doc_bubble_phase_xvel_table:  .byte   $01,$04 ; X velocity whole
doc_bubble_inter_projectile_delay_table:  .byte   $1E,$14 ; inter-bubble delay (30, 20 frames)
doc_bubble_projectile_routine_table:  .byte   $B5,$41 ; projectile routine index
doc_bubble_projectile_anim_table:  .byte   $11,$58 ; projectile anim ID
doc_bubble_distance_threshold_table:  .byte   $05,$07 ; Y distance threshold to attack
doc_bubble_x_offset_right:  .byte   $1A ; X offset for phase 1 (facing right)
doc_bubble_x_offset_left_table:  .byte   $00,$E6,$FF ; X offset + screen (facing left: -26)
; =============================================================================
; DOC BUBBLE PROJECTILE ($B5) — Bouncing Bubble
; =============================================================================
; Bubble projectile from Doc Bubble Man. Applies gravity (bounces off floor),
; moves horizontally based on facing. Despawns on wall collision.
; Only updates on odd frames ($95 AND #$01) for slow movement feel.
; =============================================================================

code_A360:  lda     $95                 ; frame parity check
        and     #$01
        bne     code_A392               ; skip on even frames
        ldy     #$08
        jsr     move_vertical_gravity   ; apply gravity
        bcc     code_A377               ; no floor hit
        lda     #$A8                    ; floor hit — bounce upward
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
code_A377:  lda     ent_facing,x        ; move horizontally
        and     #$02
        beq     code_A386
        ldy     #$09
        jsr     move_left_collide       ; move left with collision
        jmp     code_A38B

code_A386:  ldy     #$08
        jsr     move_right_collide      ; move right with collision
code_A38B:  bcc     code_A392           ; no wall hit
        lda     #$00
        sta     ent_status,x            ; wall hit — despawn
code_A392:  rts

; =============================================================================
; DOC QUICK MAN AI ($B2)
; =============================================================================
; Mimics Quick Man from MM2. Three-phase state machine:
;   Phase 0: init — randomize jump parameters (height, boomerang spawn timing),
;     advance to phase 1
;   Phase 1: jumping — apply gravity with random velocity variations, move
;     horizontally toward player, spawn 3 boomerangs at apex. On landing,
;     advance to phase 2.
;   Phase 2: running — move horizontally for 60 frames, face player, then
;     loop back to phase 0 for next jump
; =============================================================================

code_A393:  lda     ent_status,x        ; dispatch on status low nibble
        and     #$0F
        tay
        lda     doc_quick_phase_ptr_lo_table,y ; phase handler pointer (low)
        sta     L0000
        lda     doc_quick_phase_ptr_hi_table,y ; phase handler pointer (high)
        sta     $01
        jmp     (L0000)

; --- phase 0: init jump parameters ---
        lda     $E4                     ; PRNG: randomize jump params
        adc     $E5
        sta     $E6
        and     #$03
        tay                             ; Y = random 0..3
        lda     doc_quick_bounce_count_table,y ; jump duration (bounce count)
        sta     ent_timer,x
        lda     doc_quick_boomerang_spawn_timing_table,y ; boomerang spawn timing
        sta     ent_var2,x
        inc     ent_status,x            ; advance to phase 1
; --- phase 1: jumping with gravity ---
        lda     ent_yvel,x              ; save current Y velocity
        sta     $0F
        ldy     #$1E
        jsr     move_vertical_gravity   ; apply gravity
        bcc     code_A43D               ; in air — drift horizontally
        jsr     face_player             ; landed — face player
        jsr     set_sprite_hflip        ; set sprite H-flip
        dec     ent_timer,x             ; count down bounces
        bpl     code_A3D8               ; more bounces — set new velocity
        jmp     code_A422               ; done bouncing — go to phase 2

code_A3D8:  lda     $E4                 ; randomize jump velocity
        adc     $E6
        sta     $E7
        and     #$03
        tay
        lda     doc_quick_random_yvel_sub_table,y ; Y velocity sub-pixel (random)
        sta     ent_yvel_sub,x
        lda     doc_quick_random_yvel_table,y ; Y velocity whole (random)
        sta     ent_yvel,x
        lda     doc_quick_random_xspeed_div_table,y ; X speed divisor (random)
        sta     $03
        lda     #$00
        sta     L0000
        sta     $02
        jsr     entity_x_dist_to_player ; get X distance to player
        sta     $01
        clc
        adc     doc_quick_random_x_offset_table,y ; add random X offset
        sta     $04
        lda     doc_quick_random_x_offset_table,y ; clamp: don't overshoot
        bpl     code_A40E
        bcc     code_A414
        lda     $04
        sta     $01
code_A40E:  bcs     code_A414
        lda     $04
        sta     $01
code_A414:  jsr     divide_16bit        ; divide_16bit: compute X velocity
        lda     $04
        sta     ent_xvel_sub,x          ; set X velocity
        lda     $05
        sta     ent_xvel,x
        rts

; --- phase 2: running on ground ---
code_A422:  inc     ent_status,x        ; advance to phase 2
        lda     #$00
        sta     ent_xvel_sub,x
        lda     #$02
        sta     ent_xvel,x              ; set run speed
        lda     #$3C
        sta     ent_var1,x              ; run timer = 60 frames
        lda     #$01
        jsr     reset_sprite_anim       ; set anim: running
        inc     ent_anim_state,x
code_A43C:  rts

; --- in-air: drift horizontally, spawn boomerangs at apex ---
code_A43D:  lda     ent_timer,x         ; check if at boomerang spawn point
        cmp     ent_var2,x
        bne     code_A451               ; not yet
        lda     $0F                     ; was previously moving down?
        bmi     code_A451               ; yes — skip
        lda     ent_yvel,x              ; now moving up? (at apex)
        bpl     code_A451               ; no — skip
        jsr     code_A498               ; spawn 3 boomerangs
code_A451:  lda     ent_facing,x        ; drift horizontally
        and     #$02
        beq     code_A45D
        ldy     #$21
        jmp     move_left_collide       ; move left with collision

code_A45D:  ldy     #$20
        jmp     move_right_collide      ; move right with collision

; --- phase 2 continued: run timer countdown ---
        lda     ent_var1,x              ; run timer
        beq     code_A485               ; timer done — loop back
        dec     ent_var1,x
        lda     ent_anim_id,x
        cmp     #$05                    ; already in run anim?
        beq     code_A451               ; yes — keep moving
        lda     ent_anim_state,x
        bne     code_A43C               ; anim not ready
        lda     ent_anim_frame,x
        cmp     #$03                    ; wait for frame 3
        bcc     code_A43C
        lda     #$05
        jsr     reset_sprite_anim       ; set anim: running fast
        jmp     code_A451               ; move horizontally

; --- run timer expired: reset to phase 0 ---
code_A485:  jsr     face_player         ; face player
        jsr     set_sprite_hflip        ; set sprite H-flip
        lda     ent_status,x
        and     #$F0                    ; clear low nibble (back to phase 0)
        sta     ent_status,x
        lda     #$03
        jmp     reset_sprite_anim       ; set anim: idle

; --- spawn 3 boomerangs ---
code_A498:  stx     $0E                 ; save parent index
        lda     #$02
        sta     $0F                     ; spawn 3 (2,1,0)
code_A49E:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     code_A4EA               ; no slot — done
        ldx     $0E
        lda     ent_x_px,x              ; copy position from parent
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$25
        sta     ent_var1,y              ; travel distance = 37
        lda     #$00
        sta     ent_timer,y
        lda     #$0F                    ; anim ID for boomerang
        jsr     init_child_entity       ; init child entity
        lda     #$80
        sta     ent_hitbox,y            ; set projectile hitbox
        lda     #$B6
        sta     ent_routine,y           ; routine $B6 = Quick boomerang AI
        ldx     $0F
        lda     ent_y_px                ; offset target Y per boomerang
        pha
        clc
        adc     doc_quick_boomerang_y_offset_table,x ; Y offset: -24, 0, +24
        sta     ent_y_px
        tya
        tax
        jsr     code_A518               ; calc homing velocity for this boomerang
        pla
        sta     ent_y_px                ; restore player Y
        dec     $0F                     ; next boomerang
        bpl     code_A49E
code_A4EA:  ldx     $0E                 ; restore parent index
        rts

; --- Quick Man data tables ---
doc_quick_phase_ptr_lo_table:  .byte   $A6,$BE,$62 ; phase handler pointers (low)
doc_quick_phase_ptr_hi_table:  .byte   $A3,$A3,$A4 ; phase handler pointers (high)
doc_quick_random_x_offset_table:  .byte   $C0,$00,$40,$00 ; X offset for velocity calc (4 random sets)
doc_quick_random_yvel_sub_table:  .byte   $88,$15,$3D,$15 ; Y velocity sub-pixel (4 random sets)
doc_quick_random_yvel_table:  .byte   $06,$08,$09,$08 ; Y velocity whole (4 random sets)
doc_quick_random_xspeed_div_table:  .byte   $27,$30,$37,$30 ; X speed divisor (4 random sets)
doc_quick_bounce_count_table:  .byte   $03,$02,$02,$01 ; bounce count (4 random sets)
doc_quick_boomerang_spawn_timing_table:  .byte   $01,$01,$00,$00 ; boomerang spawn timing (4 random sets)
doc_quick_boomerang_y_offset_table:  .byte   $E8,$00,$18 ; Y offsets for 3 boomerangs (-24, 0, +24)
; =============================================================================
; DOC QUICK BOOMERANG ($B6)
; =============================================================================
; Boomerang projectile from Doc Quick Man. Homes toward player on spawn,
; travels for var1 frames, then returns (reversal timer in ent_timer).
; Moves vertically (up/down based on facing bit 3) and horizontally
; (left/right based on facing bit 1). Despawns when off-screen.
; =============================================================================

code_A50E:  lda     ent_timer,x         ; return timer active?
        beq     code_A529
        dec     ent_timer,x
        bne     code_A553               ; still returning — wait
; --- calc homing velocity (also called during spawn) ---
code_A518:  lda     #$00
        sta     $02                     ; speed parameter
        lda     #$04
        sta     $03
        jsr     calc_homing_velocity    ; calc homing velocity toward player
        lda     $0C
        sta     ent_facing,x            ; set facing from homing result
        rts

code_A529:  lda     ent_var1,x          ; travel distance countdown
        beq     code_A539               ; expired — just move
        dec     ent_var1,x
        bne     code_A539
        lda     #$1F                    ; travel done — start return (31 frames)
        sta     ent_timer,x
        rts

code_A539:  lda     ent_facing,x        ; move vertically
        and     #$08
        beq     code_A546
        jsr     move_sprite_up          ; move up
        jmp     code_A549

code_A546:  jsr     move_sprite_down    ; move down
code_A549:  lda     ent_flags,x         ; still on-screen?
        bmi     code_A554               ; yes — move horizontally too
        lda     #$00
        sta     ent_status,x            ; off-screen — despawn
code_A553:  rts

code_A554:  lda     ent_facing,x        ; move horizontally
        and     #$02
        beq     code_A55E
        jmp     move_sprite_left        ; move left

code_A55E:  jmp     move_sprite_right   ; move right

; =============================================================================
; DOC AIR MAN AI ($B3)
; =============================================================================
; Mimics Air Man from MM2. Two-phase state machine:
;   Phase 0 (status low nibble == 0): fan attack — spawn 6 tornado entities
;     in a randomized pattern, wait 150 frames ($96) for tornadoes to clear,
;     then after 3 attack cycles, advance to phase 1 (jumping).
;     The $36 variable controls wind push effect on the player.
;   Phase 1 (status low nibble == 1): jumping — apply gravity, move
;     horizontally (facing-dependent). On landing, flip facing and X position,
;     loop back to phase 0.
; =============================================================================

code_A561:  lda     ent_status,x        ; dispatch on status low nibble
        and     #$0F
        beq     code_A56B               ; phase 0: fan attack
        jmp     code_A632               ; phase 1: jumping

; --- phase 0: fan attack sequence ---
code_A56B:  lda     ent_anim_state,x    ; check if blowing anim playing
        beq     code_A57C
        lda     ent_anim_frame,x
        cmp     #$08                    ; wait for frame 8 (anim done)
        bcc     code_A553
        lda     #$00
        sta     ent_anim_state,x        ; reset anim state
code_A57C:  sta     ent_anim_frame,x
        lda     ent_var2,x              ; tornado wait timer
        beq     code_A593
        dec     ent_var2,x
        lda     ent_var2,x
        cmp     #$1E                    ; at 30 frames remaining
        bne     code_A553
        lda     #$00                    ; disable wind push on player
        sta     $36
        rts

code_A593:  lda     ent_var1,x          ; attack cycle counter
        beq     code_A5A6               ; no cycles left — spawn tornadoes
        dec     ent_var1,x
        bne     code_A5AB               ; still counting down
        lda     #$03                    ; all cycles done — start jump
        jsr     reset_sprite_anim       ; set anim: jump prep
        inc     ent_status,x            ; advance to phase 1
        rts

code_A5A6:  lda     #$03                ; set 3 attack cycles
        sta     ent_var1,x
code_A5AB:  lda     $E4                 ; PRNG: randomize tornado pattern
        adc     $E6
        sta     $E4
        sta     L0000
        lda     #$05
        sta     $01
        jsr     divide_8bit             ; divide_8bit: mod 5
        lda     $03                     ; result * 6 = table index
        asl     a
        sta     L0000
        asl     a
        clc
        adc     L0000
        sta     L0000                   ; L0000 = base index into tornado data
        lda     #$96
        sta     ent_var2,x              ; wait 150 frames for tornadoes
        inc     ent_anim_state,x        ; set blowing anim
        lda     #$05                    ; spawn 6 tornadoes (5,4,3,2,1,0)
        sta     $01
        stx     $02                     ; save parent index
; --- spawn 6 tornado entities ---
code_A5D3:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     code_A62F               ; no slot — done
        ldx     $03                     ; tornado index (0..5)
        lda     doc_air_tornado_spawn_delay_table,x ; spawn delay for this tornado
        sta     ent_var1,y
        ldx     L0000                   ; data table index (pattern * 6 + tornado#)
        lda     doc_air_tornado_yvel_sub_data,x ; Y velocity sub-pixel
        sta     ent_yvel_sub,y
        lda     doc_air_tornado_yvel_data,x ; Y velocity whole
        sta     ent_yvel,y
        lda     doc_air_tornado_xvel_sub_data,x ; X velocity sub-pixel
        sta     ent_xvel_sub,y
        lda     doc_air_tornado_xvel_data,x ; X velocity whole
        sta     ent_xvel,y
        lda     doc_air_tornado_lifetime_data,x ; lifetime timer
        sta     ent_timer,y
        ldx     $02                     ; restore parent index
        lda     ent_x_px,x              ; copy position from parent
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_facing,x            ; copy facing
        sta     ent_facing,y
        lda     #$10                    ; anim ID for tornado
        jsr     init_child_entity       ; init child entity
        lda     #$A0
        sta     ent_hitbox,y            ; set tornado hitbox
        lda     #$B7
        sta     ent_routine,y           ; routine $B7 = tornado rising phase
        inc     L0000                   ; next data table entry
        dec     $01                     ; next tornado
        bpl     code_A5D3
code_A62F:  ldx     $02                 ; restore parent index
        rts

; --- phase 1: jumping/landing ---
code_A632:  ldy     #$1E
        jsr     move_vertical_gravity   ; apply gravity
        bcs     code_A63C               ; landed — set jump velocity
        jmp     code_A554               ; in air — move horizontally

code_A63C:  lda     ent_var1,x          ; jump step counter
        cmp     #$02
        beq     code_A660               ; step 2 → landing — reposition
        tay                             ; step 0 or 1 — set velocity from table
        lda     doc_air_jump_yvel_sub_table,y ; Y velocity sub-pixel
        sta     ent_yvel_sub,x
        lda     doc_air_jump_yvel_table,y ; Y velocity whole
        sta     ent_yvel,x
        lda     doc_air_jump_xvel_sub_table,y ; X velocity sub-pixel
        sta     ent_xvel_sub,x
        lda     doc_air_jump_xvel_table,y ; X velocity whole
        sta     ent_xvel,x
        inc     ent_var1,x              ; next step
        rts

; --- landing: flip side and restart fan attack ---
code_A660:  dec     ent_status,x        ; back to phase 0
        lda     #$00
        sta     ent_var1,x              ; reset step counter
        lda     ent_flags,x             ; toggle H-flip flag
        eor     #$40
        sta     ent_flags,x
        lda     ent_facing,x            ; flip facing (left <-> right)
        eor     #$03
        sta     ent_facing,x
        and     #$02
        beq     code_A680
        lda     #$C8                    ; facing left: X = $C8 (right side)
        bne     code_A682
code_A680:  lda     #$38                ; facing right: X = $38 (left side)
code_A682:  sta     ent_x_px,x          ; snap to new position
        lda     #$01
        jsr     reset_sprite_anim       ; set anim: idle
        inc     ent_anim_state,x
        rts

; =============================================================================
; DOC AIR TORNADO — RISING PHASE ($B7)
; =============================================================================
; Tornado entity spawned by Doc Air Man. Two sub-phases:
;   1. Rising: moves upward for ent_timer frames while also moving horizontally.
;      Uses pre-set Y and X velocities from spawn data.
;   2. Falling/pushing: after rising, decelerates to stop, then accelerates
;      horizontally (pushes player via $36/$37/$38 wind variables).
;      Despawns when off-screen (checked by code_A554).
; =============================================================================

code_A68E:  lda     ent_timer,x         ; rising timer
        beq     code_A6AF               ; timer done — start push phase
        dec     ent_timer,x
        dec     ent_var1,x              ; also count down spawn delay
        lda     ent_y_sub,x             ; move upward (subtract Y velocity)
        sec
        sbc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x
        sbc     ent_yvel,x
        sta     ent_y_px,x
        jmp     code_A554               ; move horizontally

; --- push phase: accelerate horizontally ---
code_A6AF:  lda     ent_var1,x          ; deceleration delay
        beq     code_A6C2               ; delay done — accelerate
        dec     ent_var1,x
        beq     code_A6BA               ; just expired — zero out velocity
        rts

code_A6BA:  lda     #$00                ; zero X velocity for fresh start
        sta     ent_xvel_sub,x
        sta     ent_xvel,x
code_A6C2:  lda     ent_xvel_sub,x      ; accelerate X velocity
        clc
        adc     #$10
        sta     ent_xvel_sub,x
        lda     ent_xvel,x
        adc     #$00
        sta     ent_xvel,x
        cmp     #$04                    ; cap at speed 4
        bcc     code_A6E1
        lda     #$04
        sta     ent_xvel,x
        lda     #$00
        sta     ent_xvel_sub,x
code_A6E1:  lda     ent_facing,x        ; set wind push variables
        sta     $36                     ; $36 = wind direction
        lda     ent_xvel_sub,x
        sta     $37                     ; $37 = wind speed (sub)
        lda     ent_xvel,x
        sta     $38                     ; $38 = wind speed (whole)
        jmp     code_A554               ; move horizontally

; --- Air Man jump velocity tables (2 steps) ---
doc_air_jump_yvel_sub_table:  .byte   $A8,$A4 ; Y velocity sub-pixel (step 0, 1)
doc_air_jump_yvel_table:  .byte   $05,$08 ; Y velocity whole (step 0, 1)
doc_air_jump_xvel_sub_table:  .byte   $6A,$DA ; X velocity sub-pixel (step 0, 1)
doc_air_jump_xvel_table:  .byte   $01,$01 ; X velocity whole (step 0, 1)
; --- tornado spawn data ---
doc_air_tornado_spawn_delay_table:  .byte   $44,$4A,$42,$43,$43 ; spawn delay per tornado (6 entries, index 0..5)
; Tornado data tables: 30 entries (5 patterns x 6 tornadoes)
; Each pattern defines velocity and lifetime for 6 tornado projectiles.
doc_air_tornado_yvel_sub_data:  .byte   $00,$F0,$50,$3C,$00,$00,$D3,$CD ; Y velocity sub-pixel
        .byte   $68,$0F,$1A,$00,$A7,$68,$00,$7F
        .byte   $B1,$A7,$88,$50,$D4,$D0,$D0,$B9
        .byte   $98,$50,$3C,$1A,$7C,$35
doc_air_tornado_yvel_data:  .byte   $04,$03,$03,$02,$02,$00,$03,$03 ; Y velocity whole
        .byte   $02,$02,$01,$00,$03,$02,$02,$01
        .byte   $00,$FF,$03,$03,$02,$01,$01,$FF
        .byte   $03,$03,$02,$01,$00,$00
doc_air_tornado_xvel_sub_data:  .byte   $00,$B1,$3C,$50,$76,$00,$2B,$3C ; X velocity sub-pixel
        .byte   $31,$6B,$DB,$00,$A0,$31,$76,$B5
        .byte   $F0,$FC,$E0,$3C,$D4,$90,$90,$FD
        .byte   $C0,$3C,$50,$DB,$F8,$FE
doc_air_tornado_xvel_data:  .byte   $00,$00,$02,$03,$03,$04,$01,$01 ; X velocity whole
        .byte   $03,$03,$03,$04,$01,$03,$03,$03
        .byte   $03,$03,$01,$02,$02,$03,$03,$03
        .byte   $01,$02,$03,$03,$03,$03
doc_air_tornado_lifetime_data:  .byte   $0C,$16,$24,$0E,$24,$18,$1B,$0E ; lifetime timer (frames)
        .byte   $1E,$2A,$1D,$0C,$0D,$0A,$20,$15
        .byte   $22,$18,$21,$15,$05,$0D,$23,$1C
        .byte   $1A,$0E,$1C,$1D,$10,$24
; =============================================================================
; DOC AIR TORNADO — VERTICAL MOVEMENT ($B8)
; =============================================================================
; Tornado moves vertically (up or down based on facing bit 3) and
; horizontally (left or right based on facing bit 1).
; Despawns when off-screen (flags bit 7 clear).
; =============================================================================

code_A796:  lda     ent_facing,x        ; vertical direction
        and     #$08
        beq     code_A7A3
        jsr     move_sprite_up          ; move up
        jmp     code_A7A6

code_A7A3:  jsr     move_sprite_down    ; move down
code_A7A6:  lda     ent_flags,x         ; still on-screen?
        bmi     code_A7B1               ; yes — move horizontally
        lda     #$00
        sta     ent_status,x            ; off-screen — despawn
        rts

code_A7B1:  lda     ent_facing,x        ; horizontal direction
        and     #$02
        beq     code_A7BB
        jmp     move_sprite_left        ; move left

code_A7BB:  jmp     move_sprite_right   ; move right

; =============================================================================
; DOC AIR TORNADO — DIAGONAL BOUNCING ($B9)
; =============================================================================
; Tornado entity that bounces diagonally off walls, floor, and ceiling.
; Uses facing bits to track direction: bit 0 = horizontal flip,
; bits 2-3 = vertical direction. Reverses direction on wall/floor/ceiling
; collision. Anim IDs $4B/$4C are used for downward/upward visual.
; =============================================================================

code_A7BE:  lda     ent_timer,x         ; initial delay timer
        beq     code_A7CF
        dec     ent_timer,x
        lda     ent_timer,x
        cmp     doc_air_tornado_diagonal_anim_threshold,x ; threshold check (data overlap)
        bcc     code_A7CF
        rts                             ; still waiting

code_A7CF:  lda     ent_facing,x        ; --- horizontal movement ---
        and     #$01
        beq     code_A7F1
        lda     ent_timer,x             ; facing bit 0 set: move right
        beq     code_A7E3
        ldy     #$1E
        jsr     move_right_collide      ; move right with collision
        jmp     code_A80C

code_A7E3:  lda     ent_flags,x         ; no timer — move right (no collision)
        ora     #$40                    ; set H-flip flag
        sta     ent_flags,x
        jsr     move_sprite_right       ; move right (sprite only)
        jmp     code_A833

code_A7F1:  lda     ent_timer,x         ; facing bit 0 clear: move left
        beq     code_A7FE
        ldy     #$1F
        jsr     move_left_collide       ; move left with collision
        jmp     code_A80C

code_A7FE:  lda     ent_flags,x         ; no timer — move left (no collision)
        and     #$BF                    ; clear H-flip flag
        sta     ent_flags,x
        jsr     move_sprite_left        ; move left (sprite only)
        jmp     code_A833

code_A80C:  bcc     code_A833           ; no wall collision
        lda     #$00                    ; wall hit — bounce: reverse direction
        sta     ent_yvel_sub,x
        sta     ent_xvel_sub,x
        lda     #$03                    ; reset velocity to 3
        sta     ent_yvel,x
        sta     ent_xvel,x
        lda     ent_facing,x            ; flip horizontal direction
        eor     #$03
        sta     ent_facing,x
        and     #$0C                    ; check vertical bits
        bne     code_A879               ; has vertical direction — done
        lda     ent_facing,x            ; no vertical — set upward
        ora     #$08
        sta     ent_facing,x
        rts

; --- vertical movement ---
code_A833:  lda     ent_facing,x
        and     #$0C                    ; vertical direction bits
        beq     code_A879               ; no vertical — skip
        and     #$04
        beq     code_A858               ; bit 2 clear → move up
        lda     ent_timer,x             ; bit 2 set → move down
        bne     code_A84B
        lda     #$4B                    ; no timer — move down (no collision)
        sta     ent_anim_id,x
        jmp     move_sprite_down

code_A84B:  ldy     #$12                ; move down with collision
        jsr     move_down_collide
        lda     #$4B
        sta     ent_anim_id,x           ; anim: downward tornado
        jmp     code_A86F

code_A858:  lda     ent_timer,x         ; --- move up ---
        bne     code_A865
        lda     #$4C                    ; no timer — move up (no collision)
        sta     ent_anim_id,x
        jmp     move_sprite_up

code_A865:  ldy     #$13                ; move up with collision
        jsr     move_up_collide
doc_air_tornado_diagonal_anim_threshold:  lda     #$4C ; anim: upward tornado
        sta     ent_anim_id,x
code_A86F:  bcc     code_A879           ; no floor/ceiling collision
        lda     ent_facing,x            ; collision — flip vertical direction
        eor     #$0C
        sta     ent_facing,x
code_A879:  rts

        .byte   $B4,$B2,$B0             ; data (routine index fallthrough refs)
; =============================================================================
; UNUSED ENTITY ROUTINE ($BA)
; =============================================================================
; Diagonal-bouncing entity (unused in final game).
; Phase 0: apply gravity, on landing set velocity and facing, advance.
; Phase 1: move diagonally, bounce off walls/floor/ceiling.
; =============================================================================

code_A87D:  lda     ent_status,x        ; dispatch on status low nibble
        and     #$0F
        bne     code_A8A9               ; phase 1: bouncing
        ldy     #$12                    ; --- phase 0: init with gravity ---
        jsr     move_vertical_gravity   ; apply gravity
        bcc     code_A8F5               ; in air — wait for landing
        lda     #$00                    ; landed — set diagonal velocity
        sta     ent_xvel_sub,x
        sta     ent_yvel_sub,x
        lda     #$02
        sta     ent_xvel,x
        sta     ent_yvel,x
        jsr     face_player             ; face player
        lda     ent_facing,x
        ora     #$04                    ; set downward vertical flag
        sta     ent_facing,x
        inc     ent_status,x            ; advance to phase 1
; --- phase 1: diagonal bouncing ---
code_A8A9:  lda     ent_facing,x        ; vertical direction
        and     #$08
        bne     code_A8BD
        ldy     #$12                    ; bit 3 clear: move down
        jsr     move_down_collide       ; move down with collision
        lda     #$53
        sta     ent_anim_id,x           ; anim: downward
        jmp     code_A8C7

code_A8BD:  ldy     #$13                ; bit 3 set: move up
        jsr     move_up_collide         ; move up with collision
        lda     #$54
        sta     ent_anim_id,x           ; anim: upward
code_A8C7:  bcs     code_A8F6           ; floor/ceiling hit
        lda     ent_facing,x            ; no vertical collision — try horizontal
        and     #$0C
        tay
        lda     ent_facing,x
        pha                             ; save facing
        cpy     #$08
        beq     code_A8DC               ; if moving up, don't flip H
        eor     #$03                    ; flip horizontal for wall check
        sta     ent_facing,x
code_A8DC:  lda     ent_anim_id,x
        pha                             ; save anim
        jsr     code_A908               ; try horizontal movement
        pla
        sta     ent_anim_id,x           ; restore anim
        pla
        sta     ent_facing,x            ; restore facing
        bcs     code_A92B               ; wall hit — done
        lda     ent_facing,x            ; no wall — flip vertical
        eor     #$0C
        sta     ent_facing,x
code_A8F5:  rts

code_A8F6:  lda     ent_facing,x        ; floor/ceiling hit while moving up?
        and     #$08
        beq     code_A908               ; no — try horizontal movement
        lda     #$59                    ; yes — destroy (hit ceiling)
        jsr     reset_sprite_anim       ; set destruction anim
        lda     #$00
        sta     ent_routine,x           ; clear routine (despawn)
        rts

; --- horizontal movement sub-routine ---
code_A908:  lda     #$52
        sta     ent_anim_id,x           ; anim: horizontal
        lda     ent_facing,x
        and     #$01
        beq     code_A91C
        ldy     #$1E
        jsr     move_right_collide      ; move right with collision
        jmp     code_A921

code_A91C:  ldy     #$1F
        jsr     move_left_collide       ; move left with collision
code_A921:  bcc     code_A92B           ; no wall — done
        lda     ent_facing,x            ; wall hit — flip vertical
        eor     #$0C
        sta     ent_facing,x
code_A92B:  rts

; =============================================================================
; SNAKE MAN STAGE DATA (stage $05)
; =============================================================================
; Compressed stage layout data for Snake Man's stage. This bank doubles as
; stage data storage when stage_id ($22) == $05. Contains tile maps, object
; placement, scroll data, enemy spawns, and palette data for the stage.
; =============================================================================

        .byte   $AA,$6F,$0A,$6F,$80,$DD,$AA,$B0
        .byte   $A2,$5F,$0A,$DC,$A8,$D3,$2A,$EF
        .byte   $AA,$7F,$AA,$E6,$A8,$BF,$2A,$BC
        .byte   $8A,$DF,$2A,$4F,$22,$95,$00,$77
        .byte   $A2,$1D,$AB,$70,$AA,$63,$8A,$ED
        .byte   $82,$CF,$A2,$4F,$2A,$ED,$AA,$EA
        .byte   $A8,$CE,$EA,$F7,$28,$26,$A3,$BC
        .byte   $2A,$5B,$82,$57,$AA,$7F,$AA,$57
        .byte   $8A,$8F,$28,$26,$AB,$D0,$A6,$FF
        .byte   $A2,$FF,$02,$D8,$AA,$FA,$A0,$73
        .byte   $AA,$D2,$AA,$FE,$A8,$B5,$0A,$19
        .byte   $AA,$4F,$A8,$79,$82,$52,$A8,$9E
        .byte   $AA,$EF,$A8,$C1,$8A,$EA,$A0,$7F
        .byte   $A2,$FA,$88,$FF,$22,$FE,$A8,$FB
        .byte   $AA,$FE,$C8,$DF,$28,$D5,$20,$FC
        .byte   $0A,$82,$A2,$E4,$AA,$FF,$80,$B2
        .byte   $AA,$CF,$E8,$82,$82,$9F,$AA,$91
        .byte   $28,$5C,$8A,$BB,$8A,$FE,$28,$D9
        .byte   $AA,$DD,$A0,$7B,$80,$66,$A2,$68
        .byte   $82,$ED,$28,$8E,$A8,$0C,$22,$4F
        .byte   $22,$EF,$A8,$52,$20,$FC,$A2,$47
        .byte   $A8,$63,$A0,$19,$8A,$D7,$A8,$D1
        .byte   $2A,$A8,$AA,$7A,$8A,$F5,$2A,$CC
        .byte   $02,$6E,$2A,$CF,$8A,$F6,$A0,$78
        .byte   $AA,$DF,$88,$EF,$88,$FB,$82,$AE
        .byte   $A8,$AF,$A0,$DF,$AA,$F4,$AA,$FF
        .byte   $AA,$56,$AA,$FF,$00,$01,$02,$03
        .byte   $04,$1A,$06,$07,$08,$09,$0A,$0B
        .byte   $0C,$0D,$1A,$0F,$10,$11,$12,$13
        .byte   $14,$15,$16,$17,$18,$19,$A8,$F9
        .byte   $88,$FF,$AA,$FE,$20,$CF,$2A,$33
        .byte   $AA,$5D,$42,$2E,$20,$62,$02,$77
        .byte   $A6,$F3,$0A,$05,$17,$1E,$1E,$26
        .byte   $A2,$2F,$20,$EA,$AA,$3A,$22,$D1
        .byte   $AA,$DE,$28,$F7,$23,$80,$A0,$21
        .byte   $62,$80,$80,$80,$A0,$22,$80,$A4
        .byte   $20,$20,$00,$C4,$88,$71,$0A,$5D
        .byte   $A2,$F1,$AA,$D5,$20,$7F,$28,$7B
        .byte   $A8,$C7,$80,$CF,$15,$00,$15,$00
        .byte   $2C,$00,$15,$00,$01,$00,$17,$00
        .byte   $17,$00,$17,$00,$2C,$00,$0C,$00
        .byte   $06,$00,$06,$00,$06,$00,$26,$00
        .byte   $00,$6F,$2A,$37,$40,$42,$0F,$39
        .byte   $19,$09,$0F,$37,$10,$00,$0F,$19
        .byte   $09,$21,$0F,$20,$31,$21,$80,$00
        .byte   $00,$00,$88,$DE,$02,$B3,$08,$FD
        .byte   $08,$BA,$22,$5F,$28,$5F,$02,$4C
        .byte   $22,$9D,$20,$2F,$2A,$FD,$AA,$D5
        .byte   $AA,$48,$80,$3C,$AA,$A2,$A2,$C3
        .byte   $AA,$EF,$AA,$4B,$6A,$F0,$A0,$6E
        .byte   $82,$E3,$8A,$A9,$2A,$DF,$A2,$6D
        .byte   $0A,$89,$08,$4C,$28,$16,$2A,$44
        .byte   $AA,$FB,$8A,$6F,$00,$F7,$0A,$9F
        .byte   $22,$4A,$AA,$33,$2A,$C3,$AA,$78
        .byte   $88,$5F,$AA,$1A,$84,$9A,$A2,$5E
        .byte   $82,$59,$20,$F9,$A0,$DD,$22,$4E
        .byte   $A8,$D3,$2A,$77,$09,$0C,$A2,$FE
        .byte   $AA,$7F,$80,$BF,$0F,$18,$FF,$D9
        .byte   $8A,$F8,$AA,$57,$01,$01,$01,$02
        .byte   $02,$02,$03,$04,$04,$04,$05,$05
        .byte   $05,$05,$05,$05,$05,$05,$05,$05
        .byte   $05,$06,$06,$06,$07,$07,$07,$07
        .byte   $07,$09,$09,$09,$0A,$0A,$0A,$0B
        .byte   $0C,$0D,$0D,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0F,$10
        .byte   $11,$11,$11,$12,$14,$14,$14,$15
        .byte   $15,$15,$15,$15,$15,$16,$16,$16
        .byte   $16,$17,$17,$17,$17,$17,$19,$FF
        .byte   $AA,$FA,$A2,$D1,$8A,$42,$A2,$F3
        .byte   $82,$C3,$A0,$B3,$AA,$FF,$CA,$E7
        .byte   $20,$D7,$A8,$BF,$A8,$3C,$22,$4E
        .byte   $08,$F2,$08,$AD,$2A,$43,$A8,$7D
        .byte   $A2,$FF,$82,$EF,$A0,$DF,$02,$7F
        .byte   $AA,$09,$A0,$DD,$8E,$76,$A2,$32
        .byte   $2A,$33,$8A,$FE,$20,$1E,$82,$47
        .byte   $80,$87,$8A,$F6,$2A,$4C,$A6,$77
        .byte   $A0,$F5,$0A,$4B,$AA,$F5,$2A,$7B
        .byte   $8A,$7E,$A8,$C3,$A2,$FE,$82,$7B
        .byte   $A0,$57,$E8,$FF,$08,$47,$82,$67
        .byte   $2A,$25,$88,$8A,$C8,$C7,$8E,$AC
        .byte   $8A,$61,$24,$87,$80,$B6,$2A,$BA
        .byte   $2A,$F9,$22,$CF,$AA,$11,$A2,$E8
        .byte   $82,$94,$8A,$5D,$32,$4A,$EA,$97
        .byte   $2A,$4F,$88,$B3,$AA,$BF,$8D,$B7
        .byte   $AA,$61,$8A,$EF,$AA,$FA,$2A,$3C
        .byte   $E2,$EB,$2A,$5E,$C8,$E5,$AA,$73
        .byte   $0A,$1F,$AA,$CE,$AA,$6E,$22,$36
        .byte   $A8,$EE,$29,$4E,$AA,$7D,$A2,$5F
        .byte   $A8,$B5,$A2,$F8,$AA,$7A,$A8,$FD
        .byte   $A8,$F9,$AA,$F6,$82,$7D,$AA,$79
        .byte   $A8,$1F,$AA,$FF,$28,$48,$E8,$18
        .byte   $A8,$F8,$B8,$58,$78,$98,$68,$78
        .byte   $88,$98,$A8,$B8,$C8,$D8,$E4,$D0
        .byte   $D8,$A8,$C8,$E8,$48,$78,$88,$A8
        .byte   $D8,$90,$B0,$E0,$30,$50,$A0,$A8
        .byte   $6C,$98,$B8,$68,$78,$88,$98,$A8
        .byte   $B8,$C8,$D8,$E4,$D0,$D8,$E8,$38
        .byte   $28,$48,$D8,$78,$18,$78,$D8,$38
        .byte   $58,$98,$B8,$D8,$F8,$18,$58,$88
        .byte   $B8,$38,$58,$78,$98,$C8,$D0,$FF
        .byte   $80,$00,$00,$00,$00,$04,$80,$00
        .byte   $95,$14,$60,$10,$4B,$10,$94,$40
        .byte   $10,$01,$10,$40,$0A,$11,$1D,$01
        .byte   $98,$51,$CA,$40,$A9,$01,$06,$00
        .byte   $18,$55,$A0,$11,$08,$44,$30,$10
        .byte   $06,$44,$30,$30,$11,$00,$2C,$04
        .byte   $40,$45,$A1,$00,$40,$00,$54,$04
        .byte   $00,$00,$00,$00,$20,$08,$00,$00
        .byte   $00,$C1,$82,$10,$0B,$00,$01,$04
        .byte   $00,$44,$00,$40,$81,$00,$00,$15
        .byte   $20,$00,$45,$41,$CA,$00,$58,$11
        .byte   $16,$00,$00,$01,$A2,$41,$C0,$10
        .byte   $94,$40,$92,$54,$00,$00,$04,$91
        .byte   $A1,$40,$66,$41,$0C,$01,$98,$00
        .byte   $04,$11,$1D,$10,$08,$00,$60,$00
        .byte   $28,$00,$19,$11,$0C,$00,$01,$40
        .byte   $8E,$15,$05,$00,$04,$10,$40,$14
        .byte   $24,$00,$01,$40,$4C,$50,$88,$04
        .byte   $46,$14,$23,$80,$EE,$04,$C0,$00
        .byte   $3C,$51,$44,$10,$CE,$15,$83,$50
        .byte   $69,$50,$B8,$44,$40,$40,$41,$54
        .byte   $65,$44,$82,$40,$00,$11,$81,$81
        .byte   $38,$80,$AC,$44,$70,$54,$B0,$94
        .byte   $74,$50,$B0,$54,$74,$94,$90,$98
        .byte   $90,$88,$90,$98,$90,$88,$50,$27
        .byte   $3F,$28,$68,$88,$38,$98,$38,$B4
        .byte   $88,$38,$B4,$48,$68,$B4,$48,$80
        .byte   $60,$B8,$B8,$90,$98,$90,$88,$90
        .byte   $98,$90,$88,$50,$27,$3F,$74,$74
        .byte   $28,$B4,$B4,$50,$B8,$98,$58,$B8
        .byte   $48,$38,$98,$68,$78,$28,$48,$68
        .byte   $B8,$A8,$48,$68,$68,$88,$00,$FF
        .byte   $94,$00,$26,$00,$00,$28,$30,$00
        .byte   $9A,$00,$03,$00,$A0,$00,$39,$00
        .byte   $01,$00,$0E,$00,$68,$10,$25,$04
        .byte   $22,$44,$01,$01,$32,$15,$49,$14
        .byte   $02,$50,$62,$02,$80,$00,$C8,$52
        .byte   $C9,$21,$11,$00,$0D,$00,$86,$44
        .byte   $95,$00,$91,$30,$12,$90,$00,$00
        .byte   $09,$10,$45,$00,$82,$01,$00,$04
        .byte   $1A,$01,$00,$04,$08,$40,$08,$40
        .byte   $A0,$40,$20,$00,$02,$00,$02,$00
        .byte   $15,$15,$7E,$40,$62,$04,$08,$01
        .byte   $E4,$00,$10,$01,$0A,$10,$09,$50
        .byte   $87,$14,$0A,$00,$80,$10,$88,$00
        .byte   $02,$01,$AE,$40,$A0,$00,$6C,$40
        .byte   $00,$15,$56,$10,$01,$00,$84,$00
        .byte   $4D,$00,$02,$50,$08,$05,$0C,$10
        .byte   $00,$00,$A2,$00,$04,$00,$C1,$50
        .byte   $04,$01,$89,$01,$50,$0C,$B0,$00
        .byte   $02,$10,$00,$4A,$00,$00,$E1,$00
        .byte   $8C,$00,$19,$00,$B3,$55,$60,$45
        .byte   $0C,$41,$C0,$50,$6A,$40,$81,$00
        .byte   $17,$04,$2D,$50,$0E,$40,$B0,$08
        .byte   $43,$01,$88,$41,$00,$60,$00,$60
        .byte   $60,$00,$00,$60,$60,$60,$66,$66
        .byte   $66,$66,$66,$66,$66,$66,$67,$70
        .byte   $71,$01,$61,$61,$50,$01,$50,$60
        .byte   $01,$01,$04,$01,$01,$04,$01,$03
        .byte   $03,$56,$56,$66,$66,$66,$66,$66
        .byte   $66,$66,$66,$67,$70,$71,$04,$04
        .byte   $50,$04,$04,$05,$0B,$0B,$0B,$0B
        .byte   $06,$0B,$06,$06,$0B,$06,$06,$06
        .byte   $0B,$06,$06,$06,$06,$06,$4C,$FF
        .byte   $66,$20,$06,$11,$12,$11,$52,$01
        .byte   $D3,$00,$F0,$00,$08,$41,$00,$00
        .byte   $C2,$00,$08,$00,$44,$40,$10,$40
        .byte   $A1,$01,$20,$11,$C8,$10,$48,$C0
        .byte   $18,$01,$20,$01,$0A,$05,$72,$10
        .byte   $CC,$41,$40,$14,$91,$08,$6C,$04
        .byte   $8C,$43,$23,$41,$00,$00,$40,$00
        .byte   $43,$40,$1B,$40,$00,$04,$40,$44
        .byte   $14,$04,$0C,$04,$00,$11,$C5,$01
        .byte   $32,$40,$10,$01,$20,$40,$04,$10
        .byte   $09,$40,$82,$01,$22,$00,$18,$01
        .byte   $02,$00,$D9,$41,$D1,$94,$42,$C1
        .byte   $88,$44,$04,$40,$44,$50,$8C,$52
        .byte   $00,$10,$D8,$40,$03,$01,$20,$50
        .byte   $20,$44,$13,$90,$A9,$50,$0D,$35
        .byte   $01,$00,$58,$50,$00,$44,$00,$04
        .byte   $04,$04,$04,$40,$40,$02,$10,$01
        .byte   $02,$10,$20,$51,$40,$00,$46,$90
        .byte   $49,$00,$68,$10,$00,$40,$42,$15
        .byte   $18,$15,$32,$04,$09,$00,$80,$10
        .byte   $49,$15,$24,$44,$A2,$55,$E8,$05
        .byte   $40,$40,$95,$11,$84,$51,$D3,$05
        .byte   $CA,$40,$D2,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$02,$02,$02
        .byte   $02,$02,$02,$02,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$04,$05
        .byte   $00,$00,$00,$00,$00,$01,$06,$03
        .byte   $00,$00,$00,$00,$07,$03,$08,$03
        .byte   $00,$00,$00,$00,$09,$0A,$00,$0B
        .byte   $02,$02,$05,$01,$0C,$00,$00,$00
        .byte   $00,$00,$03,$03,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$07,$0D,$00,$00,$00,$00
        .byte   $00,$04,$0E,$0F,$10,$00,$00,$00
        .byte   $01,$11,$12,$13,$14,$02,$02,$02
        .byte   $0A,$00,$00,$15,$15,$0D,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$16,$03,$00,$00,$00,$00
        .byte   $00,$00,$16,$03,$00,$00,$00,$00
        .byte   $00,$00,$16,$03,$00,$00,$17,$18
        .byte   $00,$00,$16,$03,$00,$00,$19,$1A
        .byte   $18,$00,$00,$03,$02,$02,$0A,$1B
        .byte   $1C,$00,$00,$03,$1D,$1D,$1D,$1D
        .byte   $03,$1D,$1D,$1E,$00,$00,$00,$00
        .byte   $03,$00,$00,$03,$03,$1F,$0B,$02
        .byte   $02,$02,$02,$05,$03,$0C,$00,$00
        .byte   $00,$00,$00,$03,$1E,$20,$00,$00
        .byte   $00,$00,$00,$03,$03,$21,$22,$00
        .byte   $00,$00,$00,$03,$03,$23,$21,$22
        .byte   $00,$00,$00,$03,$24,$25,$23,$21
        .byte   $22,$00,$00,$03,$0A,$26,$26,$27
        .byte   $28,$1D,$29,$03,$26,$26,$26,$26
        .byte   $26,$26,$2A,$03,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$31,$03,$00,$00,$00
        .byte   $00,$00,$00,$32,$1E,$29,$01,$02
        .byte   $02,$02,$02,$0A,$03,$1F,$03,$00
        .byte   $17,$33,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$00,$00,$00,$00
        .byte   $00,$00,$37,$38,$00,$00,$00,$00
        .byte   $00,$00,$39,$3A,$00,$00,$00,$00
        .byte   $00,$00,$3B,$3C,$00,$00,$01,$05
        .byte   $00,$00,$00,$0C,$01,$02,$0A,$03
        .byte   $00,$00,$00,$00,$3D,$3E,$3F,$03
        .byte   $00,$00,$00,$00,$40,$41,$42,$03
        .byte   $43,$01,$02,$02,$36,$44,$45,$03
        .byte   $46,$03,$00,$07,$00,$00,$00,$00
        .byte   $00,$00,$03,$00,$00,$00,$00,$00
        .byte   $00,$00,$03,$00,$26,$47,$48,$26
        .byte   $49,$00,$0B,$05,$00,$00,$16,$00
        .byte   $00,$00,$00,$03,$00,$00,$4A,$00
        .byte   $00,$00,$00,$03,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$05,$3E,$4B,$3E
        .byte   $25,$4C,$4D,$03,$1E,$4E,$4F,$50
        .byte   $1D,$0D,$16,$03,$24,$27,$23,$51
        .byte   $25,$38,$00,$03,$03,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$00,$00,$01
        .byte   $02,$02,$02,$0A,$03,$00,$01,$03
        .byte   $02,$05,$00,$01,$03,$00,$0B,$0A
        .byte   $00,$0B,$02,$0A,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$0B,$02,$02,$02
        .byte   $05,$52,$53,$54,$00,$00,$07,$1D
        .byte   $03,$55,$15,$1D,$19,$36,$36,$36
        .byte   $36,$56,$1B,$36,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$0A,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$1D,$57,$58,$59
        .byte   $02,$02,$02,$02,$0D,$00,$03,$10
        .byte   $00,$00,$00,$00,$36,$36,$36,$36
        .byte   $56,$5A,$03,$00,$00,$00,$00,$00
        .byte   $00,$5A,$03,$00,$00,$00,$00,$00
        .byte   $00,$5A,$0B,$05,$00,$00,$00,$00
        .byte   $00,$5B,$25,$24,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$02,$02,$02,$02
        .byte   $05,$01,$02,$0A,$00,$00,$00,$00
        .byte   $03,$03,$00,$00,$5C,$3D,$1D,$5D
        .byte   $5E,$1D,$5F,$03,$60,$61,$25,$62
        .byte   $25,$25,$63,$03,$03,$00,$00,$00
        .byte   $00,$00,$16,$03,$03,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$00,$25,$25
        .byte   $25,$4B,$00,$03,$03,$00,$00,$00
        .byte   $00,$10,$00,$03,$0B,$02,$02,$02
        .byte   $05,$64,$01,$0A,$02,$02,$02,$02
        .byte   $0A,$5A,$03,$00,$24,$63,$0B,$02
        .byte   $02,$0A,$5B,$3F,$03,$16,$00,$00
        .byte   $00,$00,$65,$0C,$03,$00,$00,$00
        .byte   $00,$00,$00,$51,$03,$00,$00,$25
        .byte   $25,$00,$00,$01,$0A,$00,$00,$00
        .byte   $00,$00,$00,$03,$02,$05,$00,$00
        .byte   $00,$00,$66,$03,$26,$1E,$67,$1D
        .byte   $68,$26,$2A,$03,$00,$03,$15,$1D
        .byte   $08,$00,$1F,$03,$03,$1F,$0B,$05
        .byte   $00,$00,$00,$00,$03,$1F,$01,$03
        .byte   $02,$02,$02,$05,$03,$1F,$03,$03
        .byte   $00,$00,$00,$03,$0A,$1F,$03,$03
        .byte   $00,$00,$00,$03,$00,$1F,$03,$03
        .byte   $00,$00,$00,$03,$05,$1F,$03,$03
        .byte   $00,$00,$00,$0B,$03,$1F,$03,$0B
        .byte   $02,$05,$69,$1D,$03,$1F,$03,$00
        .byte   $00,$03,$5A,$00,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$31,$03,$00,$00,$00
        .byte   $00,$00,$00,$32,$1E,$29,$01,$02
        .byte   $02,$02,$02,$0A,$03,$1F,$03,$00
        .byte   $17,$33,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$02,$02,$02
        .byte   $02,$02,$02,$02,$03,$33,$18,$00
        .byte   $00,$01,$02,$05,$6A,$35,$1C,$18
        .byte   $01,$0A,$00,$03,$6B,$36,$03,$1C
        .byte   $03,$00,$00,$03,$00,$00,$6C,$6D
        .byte   $6D,$6D,$6D,$6D,$00,$00,$6C,$6E
        .byte   $6F,$70,$6D,$6E,$00,$00,$6C,$71
        .byte   $72,$72,$73,$71,$00,$00,$74,$75
        .byte   $75,$75,$75,$75,$02,$02,$05,$00
        .byte   $00,$00,$00,$00,$00,$17,$3D,$00
        .byte   $00,$00,$00,$00,$00,$19,$76,$43
        .byte   $43,$01,$02,$02,$00,$03,$46,$46
        .byte   $46,$03,$46,$46,$6D,$77,$78,$79
        .byte   $7A,$7B,$7C,$6D,$7D,$7E,$7F,$79
        .byte   $7A,$7B,$7C,$80,$7B,$81,$72,$79
        .byte   $82,$7B,$83,$84,$7B,$85,$75,$79
        .byte   $86,$7B,$87,$72,$7B,$88,$00,$79
        .byte   $89,$7B,$8A,$72,$7B,$88,$00,$79
        .byte   $89,$7B,$8A,$72,$02,$02,$02,$02
        .byte   $02,$02,$05,$72,$79,$8B,$46,$79
        .byte   $8B,$79,$03,$72,$6D,$8C,$84,$7B
        .byte   $81,$72,$72,$72,$8D,$72,$72,$7B
        .byte   $81,$72,$72,$72,$72,$72,$72,$7B
        .byte   $81,$72,$72,$72,$72,$72,$72,$7B
        .byte   $81,$72,$72,$72,$72,$72,$72,$7B
        .byte   $81,$8E,$8F,$90,$72,$72,$72,$7B
        .byte   $91,$7B,$92,$6D,$93,$93,$94,$95
        .byte   $96,$7B,$7C,$6D,$6D,$6D,$6D,$79
        .byte   $7A,$7B,$7C,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$97,$6D,$6D,$6D,$8E
        .byte   $98,$6D,$99,$80,$6D,$6D,$6D,$7B
        .byte   $7C,$70,$9A,$71,$6D,$6D,$6D,$7B
        .byte   $9B,$72,$72,$72,$6D,$6D,$8D,$7B
        .byte   $81,$72,$72,$72,$6D,$6D,$80,$9C
        .byte   $84,$72,$72,$72,$6D,$6E,$71,$72
        .byte   $72,$72,$72,$72,$8C,$84,$72,$72
        .byte   $72,$72,$72,$72,$72,$72,$72,$72
        .byte   $72,$72,$9D,$9E,$9F,$72,$72,$72
        .byte   $72,$72,$A0,$A1,$72,$72,$72,$9D
        .byte   $9E,$72,$A0,$A1,$9D,$9E,$72,$A0
        .byte   $A1,$72,$A0,$A1,$A0,$A1,$72,$A0
        .byte   $A1,$72,$A0,$A1,$72,$72,$72,$72
        .byte   $72,$72,$72,$A2,$72,$72,$72,$72
        .byte   $72,$72,$72,$72,$72,$72,$72,$72
        .byte   $9D,$9E,$72,$72,$72,$72,$72,$72
        .byte   $A0,$A1,$72,$72,$72,$72,$72,$72
        .byte   $A0,$A1,$72,$9D,$72,$72,$72,$72
        .byte   $A0,$A1,$72,$A0,$72,$9D,$9E,$72
        .byte   $A0,$A1,$72,$A0,$72,$A0,$A1,$72
        .byte   $A0,$A1,$72,$A0,$78,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$7F,$A3,$A4,$6D
        .byte   $6D,$80,$8D,$A5,$72,$A6,$A7,$A8
        .byte   $A9,$84,$72,$72,$72,$72,$72,$72
        .byte   $72,$72,$72,$72,$9E,$72,$72,$72
        .byte   $72,$72,$72,$AA,$A1,$72,$72,$A6
        .byte   $72,$72,$72,$AB,$A1,$72,$72,$72
        .byte   $AC,$9D,$AD,$6D,$A1,$72,$72,$AB
        .byte   $AE,$A0,$AF,$6D,$6D,$6E,$B0,$72
        .byte   $72,$72,$72,$03,$A8,$84,$72,$72
        .byte   $72,$72,$72,$03,$72,$72,$72,$72
        .byte   $72,$72,$72,$03,$72,$72,$AB,$B1
        .byte   $72,$72,$72,$0B,$72,$B2,$B3,$6D
        .byte   $B4,$B5,$72,$B6,$B7,$6D,$6D,$6D
        .byte   $6D,$B8,$B5,$B6,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$B9,$01,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$03,$BA,$BB,$33,$18
        .byte   $00,$03,$10,$BC,$1B,$BD,$BE,$1C
        .byte   $5E,$1E,$BF,$C0,$00,$00,$00,$0B
        .byte   $02,$0A,$00,$C0,$02,$02,$02,$02
        .byte   $02,$02,$02,$C1,$C2,$00,$00,$00
        .byte   $00,$00,$00,$C3,$C2,$00,$00,$00
        .byte   $00,$00,$00,$C3,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$C4,$0A,$0B,$02
        .byte   $02,$02,$02,$0E,$C5,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C5,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C6,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C2,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C2,$00,$00,$C7
        .byte   $C8,$00,$00,$C9,$02,$0E,$CA,$CB
        .byte   $CC,$CD,$CE,$CE,$00,$C0,$CF,$D0
        .byte   $D1,$D2,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$00
        .byte   $D4,$00,$D4,$D5,$1E,$29,$01,$D6
        .byte   $D7,$D6,$D7,$0A,$03,$1F,$03,$D8
        .byte   $17,$D9,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$DA
        .byte   $00,$DA,$00,$D5,$1E,$29,$01,$DB
        .byte   $DC,$DB,$DC,$0A,$03,$1F,$03,$00
        .byte   $DD,$33,$DE,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$D4
        .byte   $00,$D4,$00,$D5,$1E,$29,$01,$D7
        .byte   $D6,$D7,$D6,$0A,$03,$1F,$03,$00
        .byte   $DF,$33,$D9,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$00
        .byte   $DA,$00,$DA,$D5,$1E,$29,$01,$DC
        .byte   $DB,$DC,$DB,$0A,$03,$1F,$03,$E0
        .byte   $17,$DE,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$1E,$29,$01,$02
        .byte   $02,$02,$02,$0A,$03,$1F,$03,$00
        .byte   $17,$33,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$58,$58,$58,$58
        .byte   $20,$21,$28,$29,$32,$32,$3A,$3A
        .byte   $30,$31,$30,$31,$3C,$21,$3C,$29
        .byte   $22,$23,$2A,$2B,$3C,$27,$3C,$2F
        .byte   $34,$3D,$3C,$58,$37,$58,$58,$58
        .byte   $3C,$32,$3C,$3A,$26,$27,$2E,$2F
        .byte   $24,$25,$2C,$2D,$3C,$58,$3C,$58
        .byte   $3D,$35,$58,$3C,$32,$22,$3A,$2A
        .byte   $23,$3C,$2B,$3C,$58,$3C,$58,$3C
        .byte   $36,$3D,$2E,$2F,$37,$24,$58,$2C
        .byte   $25,$3C,$2D,$3C,$32,$3C,$3A,$3C
        .byte   $58,$36,$58,$58,$58,$13,$58,$13
        .byte   $58,$58,$20,$21,$58,$58,$22,$23
        .byte   $28,$29,$30,$31,$2A,$2B,$24,$25
        .byte   $2C,$2D,$58,$58,$2A,$2B,$30,$31
        .byte   $3D,$3D,$58,$58,$3D,$3D,$30,$31
        .byte   $3C,$13,$3C,$13,$3C,$35,$3C,$3C
        .byte   $3C,$36,$3C,$58,$35,$3C,$3C,$3C
        .byte   $3C,$58,$37,$58,$30,$31,$3D,$3D
        .byte   $58,$58,$3D,$3D,$3D,$3D,$3D,$3D
        .byte   $37,$58,$3D,$3D,$3C,$36,$37,$58
        .byte   $35,$12,$3C,$13,$37,$13,$35,$13
        .byte   $58,$58,$41,$42,$58,$58,$43,$58
        .byte   $58,$48,$58,$50,$49,$4A,$51,$52
        .byte   $4B,$4C,$53,$54,$58,$5A,$58,$58
        .byte   $5B,$5C,$73,$74,$73,$74,$73,$74
        .byte   $58,$58,$32,$32,$28,$29,$24,$25
        .byte   $3A,$3A,$32,$32,$3A,$3A,$58,$58
        .byte   $3C,$58,$3C,$34,$58,$3C,$3D,$37
        .byte   $3C,$3C,$3C,$3C,$34,$35,$3C,$3C
        .byte   $36,$3C,$34,$37,$3D,$3C,$3C,$36
        .byte   $30,$31,$26,$27,$58,$58,$34,$3D
        .byte   $58,$58,$35,$58,$2E,$2F,$32,$32
        .byte   $3C,$58,$3C,$22,$36,$35,$23,$3C
        .byte   $58,$58,$10,$11,$3C,$2A,$3C,$30
        .byte   $2B,$3C,$31,$3C,$00,$00,$00,$00
        .byte   $35,$34,$36,$3D,$3D,$12,$3D,$13
        .byte   $3D,$35,$3D,$37,$58,$13,$58,$58
        .byte   $58,$58,$3D,$35,$36,$35,$3D,$37
        .byte   $58,$12,$58,$13,$3C,$3D,$3C,$58
        .byte   $35,$36,$3C,$58,$3D,$3D,$3C,$58
        .byte   $3C,$58,$36,$3D,$34,$3D,$3C,$34
        .byte   $3D,$35,$3D,$3D,$34,$3D,$37,$58
        .byte   $3C,$37,$3C,$58,$2E,$2F,$58,$58
        .byte   $3D,$35,$58,$36,$20,$21,$3D,$3D
        .byte   $32,$32,$3D,$35,$13,$3C,$13,$3C
        .byte   $13,$3C,$13,$36,$3D,$3D,$20,$21
        .byte   $3D,$35,$34,$3C,$58,$34,$3D,$37
        .byte   $37,$13,$3C,$13,$28,$29,$3D,$3D
        .byte   $2E,$2F,$3D,$3D,$3C,$3C,$37,$36
        .byte   $3C,$13,$37,$13,$12,$3C,$13,$3C
        .byte   $13,$58,$13,$58,$34,$12,$3C,$13
        .byte   $35,$34,$37,$3C,$3D,$3D,$34,$3D
        .byte   $12,$34,$13,$3C,$30,$31,$32,$32
        .byte   $3A,$3A,$30,$31,$58,$59,$58,$59
        .byte   $60,$60,$60,$60,$60,$60,$60,$75
        .byte   $60,$60,$79,$7A,$60,$60,$7B,$7A
        .byte   $75,$01,$01,$01,$01,$01,$01,$01
        .byte   $79,$7A,$01,$01,$58,$44,$58,$58
        .byte   $4D,$4D,$58,$58,$2E,$2F,$11,$10
        .byte   $60,$60,$75,$79,$60,$60,$7A,$60
        .byte   $15,$02,$15,$02,$1F,$60,$1F,$60
        .byte   $15,$16,$15,$16,$17,$60,$17,$60
        .byte   $7B,$7C,$0D,$0E,$01,$01,$0F,$01
        .byte   $01,$70,$01,$01,$60,$60,$60,$7B
        .byte   $17,$01,$17,$01,$1F,$7A,$1F,$01
        .byte   $17,$7B,$17,$01,$7C,$01,$01,$01
        .byte   $17,$4D,$17,$58,$1F,$4D,$1F,$58
        .byte   $17,$45,$17,$59,$17,$58,$17,$58
        .byte   $1F,$58,$1F,$58,$17,$59,$17,$59
        .byte   $1F,$00,$1F,$00,$60,$7B,$75,$01
        .byte   $60,$75,$7C,$01,$0D,$0E,$15,$16
        .byte   $0F,$01,$17,$01,$01,$01,$61,$64
        .byte   $17,$01,$17,$64,$17,$62,$17,$60
        .byte   $01,$01,$64,$61,$01,$61,$62,$60
        .byte   $15,$0B,$15,$02,$0C,$60,$1F,$60
        .byte   $7B,$7C,$63,$64,$0F,$60,$17,$60
        .byte   $60,$60,$08,$60,$60,$60,$7B,$7C
        .byte   $17,$75,$17,$01,$60,$7B,$7C,$01
        .byte   $18,$19,$1B,$1C,$1A,$01,$1D,$01
        .byte   $6D,$01,$7C,$01,$1B,$1C,$1B,$1C
        .byte   $1D,$01,$1D,$01,$70,$60,$01,$79
        .byte   $60,$60,$79,$7C,$60,$60,$70,$60
        .byte   $70,$7B,$01,$01,$01,$08,$01,$01
        .byte   $01,$79,$01,$01,$7A,$7B,$01,$01
        .byte   $7C,$79,$01,$01,$01,$01,$01,$08
        .byte   $01,$61,$68,$60,$01,$01,$01,$61
        .byte   $1A,$62,$1D,$60,$62,$60,$60,$60
        .byte   $1D,$60,$1D,$60,$7B,$7C,$01,$01
        .byte   $64,$01,$60,$6D,$01,$68,$08,$60
        .byte   $60,$60,$60,$08,$63,$64,$08,$60
        .byte   $01,$01,$6D,$01,$5D,$07,$5D,$07
        .byte   $64,$68,$60,$60,$60,$6D,$60,$60
        .byte   $08,$08,$60,$60,$30,$31,$24,$25
        .byte   $36,$35,$32,$3C,$58,$20,$58,$28
        .byte   $3A,$3C,$58,$36,$3A,$3A,$3D,$3D
        .byte   $3D,$37,$58,$58,$58,$30,$58,$30
        .byte   $32,$26,$3A,$2E,$05,$58,$05,$58
        .byte   $58,$07,$58,$07,$21,$32,$29,$3A
        .byte   $31,$58,$31,$58,$27,$58,$2F,$58
        .byte   $58,$34,$58,$3C,$35,$58,$3C,$58
        .byte   $58,$24,$58,$2C,$23,$58,$2B,$20
        .byte   $58,$3C,$21,$3C,$3C,$58,$32,$32
        .byte   $58,$34,$32,$3C,$3D,$3D,$32,$32
        .byte   $31,$28,$31,$30,$29,$3C,$31,$3C
        .byte   $3A,$3A,$3C,$58,$3A,$3C,$58,$3C
        .byte   $5B,$5C,$76,$77,$58,$58,$58,$38
        .byte   $76,$77,$76,$77,$7E,$38,$7F,$39
        .byte   $7E,$39,$7F,$3B,$58,$3B,$58,$58
        .byte   $58,$3B,$32,$32,$58,$58,$38,$58
        .byte   $39,$7E,$3B,$7F,$38,$7E,$39,$7F
        .byte   $3B,$58,$20,$21,$3B,$58,$32,$32
        .byte   $58,$3B,$20,$21,$3B,$58,$58,$58
        .byte   $55,$4D,$5D,$58,$5D,$58,$5D,$58
        .byte   $2E,$2F,$58,$59,$60,$60,$3F,$60
        .byte   $01,$3F,$01,$01,$01,$01,$01,$3F
        .byte   $01,$68,$3F,$60,$60,$60,$60,$3F
        .byte   $63,$64,$3F,$60,$3F,$6D,$60,$60
        .byte   $23,$58,$2B,$3D,$58,$20,$35,$28
        .byte   $25,$32,$2D,$3A,$3C,$30,$3C,$30
        .byte   $5B,$5C,$30,$31,$3C,$58,$32,$22
        .byte   $58,$58,$23,$58,$3A,$2A,$3C,$30
        .byte   $2B,$58,$31,$58,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$3F,$00,$02,$C4,$12,$12,$28
        .byte   $19,$0A,$0C,$0E,$3F,$3F,$3C,$3C
        .byte   $3A,$2A,$2C,$2E,$8D,$8E,$8E,$04
        .byte   $06,$08,$3A,$3E,$40,$42,$AE,$46
        .byte   $C0,$4A,$4C,$C2,$60,$62,$64,$66
        .byte   $68,$6A,$CE,$6E,$C0,$C2,$AE,$12
        .byte   $20,$22,$24,$26,$3F,$BE,$CE,$DE
        .byte   $C6,$CA,$12,$CE,$12,$3F,$3F,$3F
        .byte   $3F,$14,$E8,$EA,$3F,$91,$93,$95
        .byte   $97,$14,$EC,$00,$3F,$B1,$B3,$B5
        .byte   $B7,$07,$E0,$E2,$3F,$3F,$3F,$99
        .byte   $9B,$07,$E4,$E6,$01,$11,$54,$38
        .byte   $11,$12,$C8,$12,$11,$00,$00,$00
        .byte   $00,$38,$00,$00,$70,$00,$00,$C0
        .byte   $C2,$01,$C0,$C2,$00,$71,$01,$01
        .byte   $77,$00,$AE,$CE,$4F,$AE,$CE,$4F
        .byte   $BE,$DE,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$4F,$00,$03,$C5,$12,$12,$29
        .byte   $2F,$0B,$0D,$0F,$4F,$4F,$3D,$3D
        .byte   $3B,$10,$2D,$2F,$8E,$8E,$8F,$05
        .byte   $06,$09,$3B,$2F,$41,$AF,$45,$47
        .byte   $C1,$4B,$4D,$C3,$61,$63,$65,$67
        .byte   $69,$CF,$6D,$6F,$C1,$C3,$AF,$12
        .byte   $21,$23,$25,$27,$4F,$BF,$CF,$DF
        .byte   $C7,$CA,$12,$CF,$12,$4F,$4F,$4F
        .byte   $07,$07,$E9,$EB,$4F,$92,$94,$96
        .byte   $4F,$13,$00,$EF,$B0,$B2,$B4,$B6
        .byte   $B8,$13,$E1,$E3,$4F,$07,$98,$9A
        .byte   $9C,$4F,$E5,$E7,$01,$11,$44,$48
        .byte   $11,$12,$C9,$C9,$44,$00,$00,$00
        .byte   $00,$11,$00,$00,$01,$00,$00,$C1
        .byte   $C3,$77,$C1,$C3,$00,$70,$01,$01
        .byte   $76,$00,$AF,$CF,$3F,$AF,$CF,$3F
        .byte   $BF,$DF,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$4F,$3C,$02,$D4,$12,$12,$10
        .byte   $3E,$1A,$1C,$1E,$4F,$00,$3C,$3C
        .byte   $3A,$2A,$2C,$2E,$9D,$9E,$9E,$04
        .byte   $16,$18,$3A,$3E,$50,$52,$BE,$56
        .byte   $58,$5A,$5C,$5E,$D0,$72,$74,$D2
        .byte   $78,$7A,$DE,$7E,$D0,$D2,$BE,$12
        .byte   $30,$32,$34,$36,$AE,$CE,$DE,$4F
        .byte   $C6,$DA,$12,$DE,$12,$81,$83,$85
        .byte   $4F,$4F,$F8,$00,$4F,$A1,$A3,$A5
        .byte   $A7,$4F,$FC,$FE,$4F,$4F,$4F,$89
        .byte   $8B,$17,$F0,$F2,$4F,$4F,$4F,$A9
        .byte   $AB,$17,$F4,$F6,$01,$54,$01,$01
        .byte   $49,$12,$D8,$D8,$53,$00,$00,$00
        .byte   $00,$01,$00,$00,$11,$00,$00,$D0
        .byte   $D2,$39,$D0,$D2,$00,$11,$71,$39
        .byte   $11,$00,$BE,$DE,$3F,$BE,$DE,$AE
        .byte   $CE,$3F,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$3F,$3D,$03,$D5,$12,$12,$10
        .byte   $2F,$1B,$1D,$1F,$87,$87,$3D,$3D
        .byte   $3B,$10,$2D,$2F,$9E,$9E,$9F,$15
        .byte   $16,$09,$3B,$2F,$51,$BF,$55,$57
        .byte   $59,$5B,$5D,$5F,$D1,$73,$75,$D3
        .byte   $79,$DF,$7D,$7F,$D1,$D3,$BF,$12
        .byte   $31,$33,$35,$37,$AF,$CF,$DF,$3F
        .byte   $C7,$DA,$12,$DF,$12,$82,$84,$86
        .byte   $3F,$17,$F9,$FB,$A0,$A2,$A4,$A6
        .byte   $3F,$3F,$FD,$FF,$3F,$3F,$00,$8A
        .byte   $8C,$3F,$F1,$F3,$3F,$17,$3F,$AA
        .byte   $AC,$3F,$F5,$F7,$01,$53,$01,$01
        .byte   $48,$12,$D9,$D9,$01,$00,$00,$00
        .byte   $00,$49,$00,$00,$43,$00,$00,$D1
        .byte   $D3,$11,$D1,$D3,$00,$11,$43,$76
        .byte   $11,$00,$BF,$DF,$4F,$BF,$DF,$AF
        .byte   $CF,$4F,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$03,$01,$00
        .byte   $00,$10,$00,$10,$10,$00,$00,$01
        .byte   $01,$11,$41,$11,$00,$00,$43,$23
        .byte   $43,$01,$21,$01,$11,$11,$11,$11
        .byte   $11,$11,$23,$01,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$00,$00,$10,$00
        .byte   $10,$10,$60,$10,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$00,$00,$00,$00
        .byte   $00,$00,$11,$11,$00,$00,$00,$00
        .byte   $00,$00,$11,$11,$03,$03,$03,$03
        .byte   $03,$00,$13,$10,$03,$00,$00,$00
        .byte   $00,$03,$00,$00,$03,$00,$00,$00
        .byte   $00,$03,$00,$00,$00,$03,$03,$03
        .byte   $03,$00,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00
