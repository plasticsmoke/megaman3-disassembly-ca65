; =============================================================================
; MEGA MAN 3 (U) — BANK $04 — DOC ROBOT AI (FLASH/WOOD/CRASH/METAL)
; =============================================================================
; Mapped to $A000-$BFFF. Contains AI routines for Doc Robot encounters
; that mimic MM2 bosses. Dispatched from bank1C_1D for routine indices $A0-$AF.
; Entry points: main_doc_flash_j, main_doc_wood_j, main_doc_crash_j,
; main_doc_metal_j. Also doubles as Top Man stage data ($22=$04).
;
; Routine index mapping (from bank1C_1D dispatch):
;   $A0 → main_doc_flash_j    (Doc Flash Man boss AI)
;   $A1 → main_doc_wood_j     (Doc Wood Man boss AI)
;   $A2 → main_doc_crash_j    (Doc Crash Man boss AI)
;   $A3 → main_doc_metal_j    (Doc Metal Man boss AI)
;   $A4 → code_A13C           (Doc Flash projectile — homing magnet)
;   $A5 → code_A44D           (Doc Wood leaf — falling)
;   $A6 → code_A465           (Doc Wood leaf — bouncing)
;   $A7 → code_A633           (Doc Crash bomb projectile)
;   $A8 → code_A887           (unused)
;   $A9 → code_A01E           (stub RTS)
; =============================================================================
main_doc_flash_j:

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

L0000           := $0000
move_right_collide           := $F580
move_left_collide           := $F5C4
move_down_collide           := $F606
move_up_collide           := $F642
move_vertical_gravity           := $F67C
move_sprite_right           := $F71D
move_sprite_left           := $F73B
move_sprite_down           := $F759
move_sprite_up           := $F779
reset_gravity           := $F81B
reset_sprite_anim           := $F835
init_child_entity           := $F846
face_player           := $F869
set_sprite_hflip           := $F883
entity_x_dist_to_player           := $F8C2
find_enemy_freeslot_y           := $FC53
calc_homing_velocity           := $FC63

.segment "BANK04"

; --- Entry trampolines (dispatched from bank1C_1D entity AI) ----------------
        jmp     code_A030               ; $A0 — Doc Flash Man boss AI
main_doc_wood_j:
        jmp     code_A250               ; $A1 — Doc Wood Man boss AI
main_doc_crash_j:
        jmp     code_A49A               ; $A2 — Doc Crash Man boss AI
main_doc_metal_j:
        jmp     code_A6E5               ; $A3 — Doc Metal Man boss AI
        jmp     code_A13C               ; $A4 — Flash projectile (homing magnet)
        jmp     code_A44D               ; $A5 — Wood leaf (falling)
        jmp     code_A465               ; $A6 — Wood leaf (bouncing)
        jmp     code_A633               ; $A7 — Crash bomb projectile
        jmp     code_A887               ; $A8 — unused
        jmp     code_A01E               ; $A9 — stub RTS

code_A01E:  rts

        .byte   $F3,$AA,$B5,$89,$5D,$BA,$F8,$AA
        .byte   $37,$28,$DF,$8A,$3D,$A2
        jmp     code_A887

; =============================================================================
; DOC FLASH MAN AI ($A0)
; =============================================================================
; Mimics Flash Man from MM2. Two phases:
;   Phase 1 (status & $01): walk toward player, apply gravity
;   Phase 2 (status & $02): Time Stopper — spawn projectiles every 8 frames,
;     after 6 projectiles revert to phase 1
; The Time Stopper attack triggers the special_death state ($07) on the player.
; =============================================================================

code_A030:  lda     ent_status,x            ; --- init (status low nibble == 0) ---
        and     #$0F
        bne     code_A04C
        lda     ent_status,x
        ora     #$40                    ; set invincibility flag
        sta     ent_status,x
        inc     ent_status,x            ; advance to phase 1
        lda     #$60
        sta     ent_timer,x             ; walk timer = 96 frames
        lda     #$08
        sta     ent_var1,x              ; projectile spawn interval
code_A04C:  lda     ent_status,x            ; --- phase dispatch ---
        and     #$02
        bne     code_A0A7               ; phase 2: Time Stopper attack
        ldy     #$1E
        jsr     move_vertical_gravity
        rol     $0F
        lda     ent_facing,x
        and     #$01
        beq     code_A069
        ldy     #$20
        jsr     move_right_collide
        jmp     code_A06E

code_A069:  ldy     #$21
        jsr     move_left_collide
code_A06E:  lda     $0F
        and     #$01
        beq     code_A0A6
        dec     ent_timer,x
        bne     code_A082
        lda     #$06
        jsr     reset_sprite_anim
        inc     ent_status,x
        rts

code_A082:  lda     $10
        and     #$10
        beq     code_A09A
        lda     #$03
        jsr     reset_sprite_anim
        lda     #$9E
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        jmp     face_player

code_A09A:  lda     ent_anim_id,x
        cmp     #$05
        beq     code_A0A6
        lda     #$05
        jsr     reset_sprite_anim
code_A0A6:  rts

; --- phase 2: Time Stopper attack ---
code_A0A7:  lda     ent_anim_id,x
        cmp     #$06                    ; check if Time Stopper anim playing
        bne     code_A0C5
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_A0F4
        jsr     code_A1B4
        lda     #$02
        jsr     reset_sprite_anim
        jsr     face_player
        jsr     set_sprite_hflip
        rts

code_A0C5:  dec     ent_var1,x              ; count down spawn interval
        bne     code_A0F4
        lda     #$08
        sta     ent_var1,x              ; reset interval
        jsr     code_A0F5               ; spawn projectile
        inc     ent_timer,x             ; count projectiles spawned
        lda     ent_timer,x
        cmp     #$06                    ; spawned all 6?
        bcs     code_A0DD
        rts

code_A0DD:  lda     #$05                    ; done — revert to walk anim
        jsr     reset_sprite_anim
        lda     #$60
        sta     ent_timer,x             ; reset walk timer
        dec     ent_status,x            ; back to phase 1
        lda     player_state                     ; if player already dead ($0E),
        cmp     #PSTATE_DEATH                    ; don't reset state
        beq     code_A0F4
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; release player from Doc Robot
code_A0F4:  rts

; --- spawn Doc Flash projectile ---
code_A0F5:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     code_A139               ; no slot available
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x
        clc
        adc     doc_flash_projectile_x_offset_table,y
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        sta     ent_xvel_sub,y
        lda     #$08
        sta     ent_xvel,y
        lda     #$58
        jsr     init_child_entity
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$A4
        sta     ent_routine,y
code_A139:  rts

doc_flash_projectile_x_offset_table:  .byte   $E9,$17                 ; X offset per facing: left=-23, right=+23

; =============================================================================
; DOC FLASH HOMING PROJECTILE ($A4)
; =============================================================================
; Homing projectile used by Doc Flash Man's Time Stopper attack.
; Calculates homing velocity toward player on init, then moves in that
; direction. Uses random Y-offset table to vary target position.
; =============================================================================

code_A13C:  lda     ent_status,x
        and     #$0F
        bne     code_A182
        sta     ent_var2,x
        inc     ent_status,x
        jsr     entity_x_dist_to_player
        cmp     #$18
        bcc     code_A182
        lda     ent_y_px
        sta     ent_timer,x
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$0F
        tay
        lda     ent_y_px,x
        clc
        adc     doc_flash_homing_random_y_offset_table,y
        sta     ent_y_px
        lda     #$00
        sta     $02
        lda     #$08
        sta     $03
        jsr     calc_homing_velocity
        lda     $0C
        sta     ent_facing,x
        lda     ent_timer,x
        sta     ent_y_px
        inc     ent_var2,x
code_A182:  lda     ent_var2,x
        beq     code_A197
        lda     ent_facing,x
        and     #$08
        beq     code_A194
        jsr     move_sprite_up
        jmp     code_A197

code_A194:  jsr     move_sprite_down
code_A197:  lda     ent_facing,x
        and     #$01
        beq     code_A1A1
        jmp     move_sprite_right

code_A1A1:  jmp     move_sprite_left

doc_flash_homing_random_y_offset_table:  .byte   $24,$0C,$10,$00,$E0,$F4,$10,$F8     ; random Y-offset table (16 entries)
        .byte   $18,$F0,$08,$10,$00,$F0,$00,$E8

; --- copy explosion OAM data to sprite page (special death effect) ---
code_A1B4:  ldy     #$68
code_A1B6:  lda     doc_flash_explosion_oam_data_y,y
        sta     $0200,y
        lda     doc_flash_explosion_oam_data_x,y
        sta     $0201,y
        lda     doc_flash_explosion_oam_data_flags,y
        sta     $0202,y
        lda     doc_flash_explosion_oam_data_palette,y
        sta     $0203,y
        dey
        dey
        dey
        dey
        bpl     code_A1B6

; This is the ONLY trigger for state $07 (special_death) in the entire game.
; Copies explosion OAM data to sprite page, then sets palette-cycling kill.
; Triggered by Doc Flash Man's Time Stopper attack (Gemini Man Doc Robot stage).
; [confirmed via Mesen]
        lda     player_state                     ; if player already dead ($0E),
        cmp     #PSTATE_DEATH                    ; don't overwrite with special_death
        beq     code_A1E3
        lda     #PSTATE_SPECIAL_DEATH                    ; state → $07 (special_death)
        sta     player_state                     ; palette cycling kill effect
        lda     #$1E                    ; timer = 30 frames
        sta     ent_timer
code_A1E3:  rts

doc_flash_explosion_oam_data_y:  .byte   $20
doc_flash_explosion_oam_data_x:  .byte   $F7
doc_flash_explosion_oam_data_flags:  .byte   $03
doc_flash_explosion_oam_data_palette:  .byte   $20,$20,$F7,$03,$88,$30,$F7,$03
        .byte   $E0,$40,$F7,$03,$58,$70,$F7,$03
        .byte   $C0,$80,$F7,$03,$50,$B0,$F7,$03
        .byte   $A0,$C0,$F7,$03,$E8,$D0,$F7,$03
        .byte   $18,$10,$F8,$03,$58,$40,$F8,$03
        .byte   $D0,$50,$F8,$03,$20,$60,$F8,$03
        .byte   $80,$70,$F8,$03,$E8,$88,$F8,$03
        .byte   $B0,$98,$F8,$03,$28,$D0,$F8,$03
        .byte   $C0,$D8,$F8,$03,$60,$18,$F9,$03
        .byte   $C8,$30,$F9,$03,$40,$38,$F9,$03
        .byte   $A0,$60,$F9,$03,$40,$78,$F9,$03
        .byte   $80,$88,$F9,$03,$E8,$B8,$F9,$03
        .byte   $30,$C0,$F9,$03,$C8,$D8,$F9,$03
        .byte   $D8
; =============================================================================
; DOC WOOD MAN AI ($A1)
; =============================================================================
; Mimics Wood Man from MM2. State machine with 6 states:
;   0: init — set upward velocity, spawn leaf shield
;   1: throw leaves every 18 frames, after 4 throws wait 46 frames
;   2: wait for var1 countdown, then spawn 4 falling crash blocks
;   3: wait for var2 countdown, search for leaf shield entity, make it attack
;   4: fall — walk and apply gravity, transition on landing
;   5: land — wait for landing anim, then decrement back to state 1
; =============================================================================

code_A250:  lda     ent_status,x            ; dispatch to state handler
        and     #$0F
        tay
        lda     doc_wood_state_handler_low_table,y
        sta     L0000
        lda     doc_wood_state_handler_high_table,y
        sta     $01
        jmp     (L0000)

doc_wood_state_handler_low_table:  .byte   $6F,$92,$B5,$CA,$FD,$46     ; state handler pointers (low)
doc_wood_state_handler_high_table:  .byte   $A2,$A2,$A2,$A2,$A2,$A3     ; state handler pointers (high)
        lda     #$9E
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        inc     ent_status,x
        lda     #$12
        sta     ent_timer,x
        lda     #$60
        sta     ent_var3,x
        jsr     code_A382
        rts

        dec     ent_timer,x
        bne     code_A2B1
        lda     #$12
        sta     ent_timer,x
        jsr     code_A3C1
        inc     ent_var1,x
        lda     ent_var1,x
        cmp     #$04
        bcc     code_A2B1
        lda     #$2E
        sta     ent_var1,x
        inc     ent_status,x
code_A2B1:  jsr     code_A377
        rts

        dec     ent_var1,x
        bne     code_A2B1
        lda     #$00
        sta     L0000
        jsr     code_A3FB
        inc     ent_status,x
        lda     #$24
        sta     ent_var2,x
        rts

        jsr     code_A377
        dec     ent_var2,x
        bne     code_A2FC
        lda     #$0F
        sta     ent_var2,x
        inc     ent_status,x
        lda     #$80
        sta     L0000
        ldy     #$1F
code_A2E0:  lda     ent_status,y
        bmi     code_A2EB
code_A2E5:  dey
        cpy     #$0F
        bne     code_A2E0
        rts

code_A2EB:  lda     L0000
        cmp     ent_spawn_id,y
        bne     code_A2E5
        lda     #$3C
        sta     ent_routine,y
        lda     #$8D
        sta     ent_hitbox,y
code_A2FC:  rts

        lda     ent_var2,x
        beq     code_A306
        dec     ent_var2,x
        rts

code_A306:  lda     ent_facing,x
        and     #$01
        beq     code_A315
        ldy     #$20
        jsr     move_right_collide
        jmp     code_A31A

code_A315:  ldy     #$21
        jsr     move_left_collide
code_A31A:  ldy     #$1E
        jsr     move_vertical_gravity
        bcc     code_A332
        lda     #$9E
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        inc     ent_status,x
        lda     #$1D
        bne     code_A343
code_A332:  lda     ent_anim_id,x
        cmp     #$1D
        bne     code_A2FC
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_A2FC
        lda     #$03
code_A343:  jmp     reset_sprite_anim

        lda     ent_anim_state,x
        cmp     #$01
        bne     code_A357
        lda     #$01
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
code_A357:  dec     ent_var3,x
        bne     code_A376
        dec     ent_status,x
        dec     ent_status,x
        dec     ent_status,x
        dec     ent_status,x
        lda     #$60
        sta     ent_var3,x
        jsr     face_player
        jsr     set_sprite_hflip
        jsr     code_A382
code_A376:  rts

; --- utility: force animation to advance one frame ---
code_A377:  lda     #$01
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        rts

; --- spawn leaf shield entity (orbits Doc Wood Man) ---
code_A382:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     code_A3FA
        lda     ent_facing,x
        sta     ent_facing,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        sta     ent_xvel_sub,y
        lda     #$A9
        sta     ent_routine,y
        lda     #$1B
        jsr     init_child_entity
        lda     #$AD
        sta     ent_hitbox,y
        lda     #$80
        sta     ent_spawn_id,y
        lda     #$04
        sta     ent_xvel,y
        rts

; --- spawn leaf projectile (thrown at player) ---
code_A3C1:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     code_A3FA
        lda     ent_facing,x
        sta     ent_facing,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        sta     ent_yvel_sub,y
        lda     #$12
        jsr     init_child_entity
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$A5
        sta     ent_routine,y
code_A3FA:  rts

; --- spawn 4 falling crash blocks (called recursively) ---
code_A3FB:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     code_A3FA
        lda     #$02
        sta     ent_facing,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     #$20
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$12
        jsr     init_child_entity
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$62
        sta     ent_xvel_sub,y
        sta     ent_yvel_sub,y
        lda     #$01
        sta     ent_xvel,y
        sta     ent_yvel,y
        lda     #$A6
        sta     ent_routine,y
        stx     $01
        ldx     L0000
        lda     doc_wood_crash_block_x_positions_table,x
        sta     ent_x_px,y
        ldx     $01
        inc     L0000
        lda     L0000
        cmp     #$04
        bcc     code_A3FB
        rts

doc_wood_crash_block_x_positions_table:  .byte   $40,$70,$A0,$D0         ; X positions for 4 crash blocks

; =============================================================================
; DOC WOOD LEAF — FALLING ($A5)
; =============================================================================
; Leaf entity that falls upward. Despawns when Y < 4.
; =============================================================================

code_A44D:  lda     #$00
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        jsr     move_sprite_up
        lda     ent_y_px,x
        cmp     #$04
        bcs     code_A464
        lda     #$00
        sta     ent_status,x
code_A464:  rts

; =============================================================================
; DOC WOOD LEAF — BOUNCING ($A6)
; =============================================================================
; Leaf entity that bounces diagonally, reversing direction every 15 frames.
; =============================================================================

code_A465:  lda     ent_status,x
        and     #$0F
        bne     code_A474
        lda     #$0F
        sta     ent_timer,x             ; bounce direction timer
        inc     ent_status,x
code_A474:  lda     ent_facing,x            ; move horizontally
        and     #$01
        beq     code_A481
        jsr     move_sprite_right
        jmp     code_A484

code_A481:  jsr     move_sprite_left
code_A484:  jsr     move_sprite_down
        dec     ent_timer,x
        bne     code_A499
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$0F
        sta     ent_timer,x
code_A499:  rts

; =============================================================================
; DOC CRASH MAN AI ($A2)
; =============================================================================
; Mimics Crash Man from MM2. Two phases:
;   Phase 1 (status & $01): walk left/right, bounce off walls at X=$CC/$34.
;     Jumps on B-press or after 150-frame timer. Random X velocity on jump.
;   Phase 2 (status & $02): airborne — face player while ascending,
;     spawn Crash Bomb at apex, land and revert to phase 1.
; =============================================================================

code_A49A:  lda     ent_status,x            ; --- init ---
        and     #$0F
        bne     code_A4B9
        jsr     reset_gravity                   ; reset gravity
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        inc     ent_status,x
        lda     #$05
        jsr     reset_sprite_anim
        lda     #$96
        sta     ent_var1,x
code_A4B9:  lda     ent_status,x
        and     #$02
        bne     code_A522
        lda     ent_facing,x
        and     #$01
        beq     code_A4D6
        ldy     #$20
        jsr     move_right_collide
        lda     ent_x_px,x
        cmp     #$CC
        bcs     code_A4E2
        jmp     code_A4EA

code_A4D6:  ldy     #$21
        jsr     move_left_collide
        lda     ent_x_px,x
        cmp     #$34
        bcs     code_A4EA
code_A4E2:  lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_A4EA:  ldy     #$1E
        jsr     move_vertical_gravity
        lda     ent_timer,x
        bne     code_A504
        lda     joy1_press
        and     #BTN_B
        beq     code_A503
        inc     ent_status,x
        jsr     code_A599
        inc     ent_timer,x
code_A503:  rts

code_A504:  dec     ent_var1,x
        bne     code_A515
        inc     ent_status,x
        jsr     code_A599
        lda     #$96
        sta     ent_var1,x
        rts

code_A515:  lda     joy1_press
        and     #BTN_B
        beq     code_A503
        inc     ent_status,x
        jsr     code_A599
        rts

code_A522:  lda     ent_facing,x
        and     #$01
        beq     code_A534
        ldy     #$20
        jsr     move_right_collide
        jsr     code_A5C1
        jmp     code_A53C

code_A534:  ldy     #$21
        jsr     move_left_collide
        jsr     code_A5C1
code_A53C:  ldy     #$1E
        jsr     move_vertical_gravity
        bcc     code_A55D
        dec     ent_status,x
        jsr     reset_gravity
        lda     #$4C
        sta     ent_xvel_sub,x
        lda     #$01
        sta     ent_xvel,x
        lda     #$00
        sta     ent_var2,x
        lda     #$05
        jmp     reset_sprite_anim

code_A55D:  lda     ent_anim_id,x
        cmp     #$04
        beq     code_A598
        lda     ent_yvel,x
        bpl     code_A593
        lda     ent_facing,x
        sta     ent_var2,x
        lda     #$04
        jsr     reset_sprite_anim
        lda     ent_facing,x
        pha
        jsr     face_player
        pla
        cmp     ent_facing,x
        beq     code_A589
        lda     ent_flags,x
        eor     #$40
        sta     ent_flags,x
code_A589:  jsr     code_A5E3
        lda     ent_var2,x
        sta     ent_facing,x
        rts

code_A593:  lda     #$03
        jsr     reset_sprite_anim
code_A598:  rts

; --- set random jump velocity ---
code_A599:  lda     #$88                    ; Y velocity = $07.88 (upward)
        sta     ent_yvel_sub,x
        lda     #$07
        sta     ent_yvel,x
        lda     $E4                     ; RNG: advance LFSR
        adc     $E5
        sta     $E5
        and     #$03                    ; pick 1 of 4 X velocities
        tay
        lda     doc_crash_jump_x_velocity_sub_table,y
        sta     ent_xvel_sub,x
        lda     doc_crash_jump_x_velocity_table,y
        sta     ent_xvel,x
        rts

doc_crash_jump_x_velocity_sub_table:  .byte   $00,$80,$00,$00         ; X velocity sub table
doc_crash_jump_x_velocity_table:  .byte   $01,$01,$01,$02         ; X velocity table
; --- face player and flip sprite without changing movement direction ---
code_A5C1:  lda     ent_facing,x
        sta     ent_var2,x              ; save current facing
        lda     ent_facing,x
        pha
        jsr     face_player                   ; face player
        pla
        cmp     ent_facing,x
        beq     code_A5DC
        lda     ent_flags,x             ; facing changed — flip sprite H
        eor     #$40
        sta     ent_flags,x
code_A5DC:  lda     ent_var2,x              ; restore original facing
        sta     ent_facing,x
        rts

; --- spawn Crash Bomb projectile (checks for existing one first) ---
code_A5E3:  ldy     #$1F
        lda     #$80
code_A5E7:  cmp     ent_spawn_id,y
        beq     code_A632
        dey
        cpy     #$0F
        bne     code_A5E7
        jsr     find_enemy_freeslot_y
        bcs     code_A632
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x
        clc
        adc     doc_flash_projectile_x_offset_table,y
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$80
        sta     ent_spawn_id,y
        lda     #$0B
        jsr     init_child_entity
        lda     #$80
        sta     ent_hitbox,y
        lda     #$A7
        sta     ent_routine,y
code_A632:  rts

; =============================================================================
; DOC CRASH BOMB PROJECTILE ($A7)
; =============================================================================
; Crash Bomb projectile AI. Moves vertically/horizontally toward target using
; homing velocity calculation. Two phases:
;   Phase 1: move toward target, set explosion anim on wall contact
;   Phase 2: play explosion anim, then despawn via routine $48
; =============================================================================

code_A633:  lda     ent_status,x            ; --- init ---
        and     #$0F
        bne     code_A66E
        lda     #$1E
        sta     ent_timer,x             ; explosion countdown = 30 frames
        lda     #$00
        sta     $02
        lda     #$04
        sta     $03
        lda     ent_x_px,x
        sta     ent_var1,x
        lda     ent_facing,x
        and     #$01
        tay
        lda     ent_x_px,x
        clc
        adc     doc_crash_bomb_x_offset_table,y
        sta     ent_x_px,x
        jsr     calc_homing_velocity
        lda     ent_var1,x
        sta     ent_x_px,x
        lda     $0C
        sta     ent_facing,x
        inc     ent_status,x
code_A66E:  lda     ent_status,x
        and     #$02
        bne     code_A6BA
        lda     ent_facing,x
        and     #$08
        beq     code_A684
        ldy     #$13
        jsr     move_up_collide
        jmp     code_A689

code_A684:  ldy     #$12
        jsr     move_down_collide
code_A689:  bcs     code_A6B1
        lda     ent_facing,x
        and     #$01
        beq     code_A6A2
        ldy     #$1E
        jsr     move_right_collide
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        jmp     code_A6AF

code_A6A2:  ldy     #$1F
        jsr     move_left_collide
        lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
code_A6AF:  bcc     code_A6E2
code_A6B1:  lda     #$0C
        jsr     reset_sprite_anim
        inc     ent_status,x
        rts

code_A6BA:  lda     ent_anim_id,x
        cmp     #$0C
        bne     code_A6CE
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_A6CE
        lda     #$0D
        jsr     reset_sprite_anim
code_A6CE:  dec     ent_timer,x
        bne     code_A6E2
        lda     #$59
        jsr     reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$48
        sta     ent_routine,x
code_A6E2:  rts

doc_crash_bomb_x_offset_table:  .byte   $18,$E8                 ; X offset: right=+24, left=-24

; =============================================================================
; DOC METAL MAN AI ($A3)
; =============================================================================
; Mimics Metal Man from MM2. State machine with 3 main states:
;   State 1: patrol — face player, wait for B-press or 180-frame timer
;     If player close (<$28 px), jump high immediately
;   State 2: jumping — apply gravity, throw Metal Blades while ascending
;     (var1 counts down throw interval, var2 reloads it)
;   State 3: wall bounce — reverse direction on landing, re-jump
; Uses random velocity tables for jump height/speed variation.
; =============================================================================

code_A6E5:  lda     ent_status,x            ; --- init ---
        and     #$0F
        bne     code_A6FC
        lda     #$B4
        sta     ent_timer,x             ; patrol timer = 180 frames
        lda     #$07
        sta     ent_var3,x              ; throw delay after anim
        jsr     code_A81F               ; set random jump velocity
        inc     ent_status,x
code_A6FC:  lda     ent_status,x            ; --- state dispatch ---
        and     #$0F
        cmp     #$02
        beq     code_A73C               ; state 2: jumping
        cmp     #$03
        beq     code_A77E               ; state 3: wall bounce
        jsr     code_A377
        jsr     entity_x_dist_to_player
        cmp     #$28
        bcs     code_A724
        lda     #$2A
        sta     ent_yvel_sub,x
        lda     #$08
        sta     ent_yvel,x
        inc     ent_status,x
        inc     ent_status,x
        rts

code_A724:  dec     ent_timer,x
        bne     code_A732
        lda     #$B4
        sta     ent_timer,x
        inc     ent_status,x
        rts

code_A732:  lda     joy1_press
        and     #BTN_B
        beq     code_A73B
        inc     ent_status,x
code_A73B:  rts

code_A73C:  lda     ent_anim_id,x
        cmp     #$04
        bne     code_A748
        dec     ent_var3,x
        bne     code_A73B
code_A748:  ldy     #$1E
        jsr     move_vertical_gravity
        bcc     code_A75B
        jsr     code_A81F
        lda     #$1D
        jsr     reset_sprite_anim
        dec     ent_status,x
        rts

code_A75B:  lda     #$03
        jsr     reset_sprite_anim
        lda     ent_yvel,x
        bpl     code_A77D
        dec     ent_var1,x
        bne     code_A77D
        lda     ent_var2,x
        sta     ent_var1,x
        jsr     code_A84A
        lda     #$04
        jsr     reset_sprite_anim
        lda     #$07
        sta     ent_var3,x
code_A77D:  rts

code_A77E:  lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_A78B
        lda     #$03
        jsr     reset_sprite_anim
code_A78B:  lda     ent_facing,x
        and     #$01
        beq     code_A798
        jsr     move_sprite_right
        jmp     code_A79B

code_A798:  jsr     move_sprite_left
code_A79B:  ldy     #$1E
        jsr     move_vertical_gravity
        bcc     code_A7E1
        lda     #$2A
        sta     ent_yvel_sub,x
        lda     #$08
        sta     ent_yvel,x
        lda     #$1D
        jsr     reset_sprite_anim
        lda     ent_facing,x
        and     #$01
        beq     code_A7C2
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        bne     code_A7CA
code_A7C2:  lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
code_A7CA:  lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        dec     ent_status,x
        dec     ent_status,x
        jsr     code_A81F
        lda     #$07
        sta     ent_var3,x
        rts

code_A7E1:  lda     ent_yvel,x
        bpl     code_A81E
        lda     ent_var2,x
        beq     code_A81E
        lda     ent_facing,x
        sta     $0F
        lda     ent_facing,x
        and     #$01
        beq     code_A801
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        bne     code_A809
code_A801:  lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
code_A809:  jsr     face_player
        jsr     code_A84A
        lda     #$04
        jsr     reset_sprite_anim
        lda     #$00
        sta     ent_var2,x
        lda     $0F
        sta     ent_facing,x
code_A81E:  rts

; --- set random jump velocity from lookup tables ---
code_A81F:  lda     $E4                     ; RNG: advance LFSR
        adc     $E5
        sta     $E5
        and     #$03                    ; pick 1 of 4 velocity sets
        tay
        lda     doc_metal_jump_y_velocity_sub_table,y
        sta     ent_yvel_sub,x
        lda     doc_metal_jump_y_velocity_table,y
        sta     ent_yvel,x
        lda     doc_metal_jump_throw_interval_table,y
        sta     ent_var1,x              ; throw interval
        sta     ent_var2,x              ; throw interval reload
        rts

doc_metal_jump_y_velocity_sub_table:  .byte   $88,$00,$9E,$88         ; Y velocity sub table
doc_metal_jump_y_velocity_table:  .byte   $06,$08,$04,$06         ; Y velocity table
doc_metal_jump_throw_interval_table:  .byte   $0A,$08,$0D,$0A         ; throw interval table
; --- spawn Metal Blade projectile ---
code_A84A:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     code_A886
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x
        clc
        adc     doc_flash_projectile_x_offset_table,y
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$0E
        jsr     init_child_entity
        lda     #$80
        sta     ent_hitbox,y
        lda     #$A8
        sta     ent_routine,y
code_A886:  rts

; =============================================================================
; METAL BLADE PROJECTILE AI ($A8)
; =============================================================================
; Metal Blade projectile. Calculates homing direction on init, then moves
; vertically and horizontally toward target.
; =============================================================================

code_A887:  lda     ent_status,x
        and     #$0F
        bne     code_A8A1
        inc     ent_status,x
        lda     #$00
        sta     $02
        lda     #$04
        sta     $03
        jsr     calc_homing_velocity
        lda     $0C
        sta     ent_facing,x
code_A8A1:  lda     ent_facing,x
        and     #$08
        beq     code_A8AE
        jsr     move_sprite_up
        jmp     code_A8B1

code_A8AE:  jsr     move_sprite_down
code_A8B1:  jmp     code_A197

; =============================================================================
; TOP MAN STAGE DATA
; =============================================================================
; Bank $04 doubles as Top Man stage data ($22=$04).
; Raw stage tilemap/level layout data follows.
; =============================================================================

        .byte   $08,$FC,$88,$BF,$08,$B6,$88,$50
        .byte   $A8,$C5,$A2,$BF,$8A,$ED,$28,$A0
        .byte   $A2,$7E,$82,$F8,$28,$7F,$AA,$EA
        .byte   $A8,$73,$8A,$B2,$28,$F3,$82,$D3
        .byte   $A8,$FC,$AB,$FA,$8A,$41,$2A,$FF
        .byte   $A8,$D4,$AA,$9F,$AA,$7A,$AA,$BE
        .byte   $20,$85,$AA,$4D,$8A,$DB,$68,$E7
        .byte   $A0,$F9,$88,$E7,$02,$88,$AA,$FF
        .byte   $82,$F5,$AA,$D8,$A8,$FE,$82,$5C
        .byte   $28,$DB,$02,$8E,$2A,$5D,$A2,$FF
        .byte   $0A,$27,$A8,$9D,$88,$7D,$A0,$86
        .byte   $0A,$F4,$80,$DC,$06,$C4,$A0,$DF
        .byte   $AA,$49,$08,$FD,$00,$97,$A0,$DD
        .byte   $AA,$BE,$A8,$CF,$00,$CE,$A2,$47
        .byte   $03,$F2,$8A,$96,$9A,$DF,$A2,$FE
        .byte   $2A,$6E,$82,$29,$A2,$7F,$AA,$8D
        .byte   $A8,$7B,$AA,$77,$82,$75,$AA,$D4
        .byte   $A8,$EB,$AA,$5A,$02,$F5,$AA,$DA
        .byte   $02,$19,$AA,$EA,$88,$DB,$A8,$9C
        .byte   $2A,$97,$82,$F7,$32,$ED,$80,$67
        .byte   $A2,$DF,$2A,$3B,$AA,$BB,$AA,$F7
        .byte   $2A,$FF,$88,$FF,$88,$5B,$A8,$65
        .byte   $02,$67,$AA,$5D,$A8,$C3,$80,$F1
        .byte   $AB,$F6,$A2,$CF,$2A,$43,$AA,$CF
        .byte   $AA,$7D,$20,$9F,$AA,$EE,$8A,$6E
        .byte   $A8,$FF,$AA,$FA,$22,$8F,$2A,$DB
        .byte   $AA,$53,$02,$DE,$A8,$81,$A0,$7F
        .byte   $0A,$E5,$90,$51,$A2,$88,$82,$CC
        .byte   $08,$B4,$22,$5B,$AA,$F9,$A2,$EE
        .byte   $0A,$DF,$A8,$DE,$88,$35,$22,$C3
        .byte   $08,$FE,$28,$7A,$A2,$47,$8A,$EB
        .byte   $80,$59,$8A,$DE,$B8,$7C,$A2,$6E
        .byte   $0A,$9C,$22,$4F,$28,$BD,$A8,$F9
        .byte   $AA,$AF,$AA,$97,$2A,$EF,$AA,$DA
        .byte   $3A,$22,$2A,$3C,$AA,$D3,$A8,$0E
        .byte   $C8,$B7,$AA,$8D,$82,$EE,$02,$B7
        .byte   $AA,$F5,$2A,$D8,$AA,$5C,$AB,$7C
        .byte   $22,$E7,$AA,$7F,$A8,$6A,$A8,$6D
        .byte   $A8,$D2,$28,$0B,$AA,$B7,$A3,$52
        .byte   $88,$8B,$0C,$AD,$8A,$D7,$AA,$52
        .byte   $82,$71,$22,$F7,$0B,$1F,$0A,$BE
        .byte   $A2,$F7,$8A,$F9,$00,$01,$02,$03
        .byte   $04,$05,$06,$07,$08,$0A,$0B,$0C
        .byte   $0D,$0E,$0F,$10,$11,$12,$13,$14
        .byte   $15,$16,$2A,$7A,$A2,$7E,$82,$9E
        .byte   $98,$FF,$A0,$DD,$2A,$78,$22,$BC
        .byte   $BA,$1C,$28,$FE,$A8,$E9,$AA,$FB
        .byte   $8A,$E9,$28,$8E,$13,$18,$18,$59
        .byte   $88,$69,$AA,$63,$2A,$71,$A8,$68
        .byte   $AA,$5F,$02,$CF,$23,$40,$40,$40
        .byte   $62,$40,$40,$61,$20,$80,$A3,$20
        .byte   $20,$00,$82,$EE,$20,$0F,$82,$9F
        .byte   $82,$7D,$2A,$DF,$A8,$DA,$A8,$5B
        .byte   $AA,$7A,$AA,$DF,$02,$01,$31,$00
        .byte   $0A,$00,$02,$00,$2D,$01,$00,$02
        .byte   $2B,$02,$31,$02,$2B,$02,$00,$02
        .byte   $31,$01,$31,$01,$2A,$01,$00,$7F
        .byte   $0A,$CE,$A8,$EC,$58,$5A,$0F,$20
        .byte   $1A,$21,$0F,$20,$1A,$2A,$0F,$20
        .byte   $10,$00,$0F,$27,$17,$00,$00,$00
        .byte   $00,$00,$0F,$20,$1A,$21,$0F,$20
        .byte   $1A,$2A,$0F,$20,$10,$00,$0F,$31
        .byte   $21,$21,$8B,$00,$00,$00,$0F,$20
        .byte   $1A,$21,$0F,$20,$1A,$2A,$0F,$20
        .byte   $10,$00,$0F,$0F,$0F,$0F,$00,$00
        .byte   $00,$00,$A8,$38,$A8,$A1,$02,$77
        .byte   $80,$77,$28,$89,$08,$A7,$82,$EF
        .byte   $AA,$C6,$A2,$F2,$AA,$50,$2A,$3F
        .byte   $0A,$E3,$28,$D3,$08,$FB,$A8,$BF
        .byte   $0A,$FE,$28,$AF,$88,$F6,$A6,$BE
        .byte   $AA,$BF,$A8,$7E,$A0,$EE,$A8,$6E
        .byte   $2A,$7E,$8A,$DF,$07,$0B,$A8,$FD
        .byte   $20,$ED,$AA,$DB,$0C,$14,$FF,$5A
        .byte   $8A,$5F,$A8,$2C,$01,$01,$01,$01
        .byte   $02,$02,$02,$03,$03,$03,$03,$03
        .byte   $04,$05,$05,$05,$05,$06,$08,$09
        .byte   $09,$09,$09,$0A,$0A,$0B,$0B,$0B
        .byte   $0B,$0B,$0D,$0E,$0E,$0E,$0E,$0E
        .byte   $0F,$0F,$10,$11,$12,$12,$12,$12
        .byte   $13,$15,$FF,$9D,$AA,$69,$8A,$FC
        .byte   $88,$DD,$28,$9D,$A2,$FC,$28,$36
        .byte   $A8,$9E,$2B,$DF,$82,$F7,$08,$93
        .byte   $A8,$55,$AA,$F4,$AA,$FE,$08,$F7
        .byte   $98,$7F,$A0,$47,$AA,$CA,$20,$2E
        .byte   $8A,$7F,$80,$7F,$8A,$6E,$A0,$5B
        .byte   $02,$E3,$8A,$47,$80,$6F,$A8,$BE
        .byte   $8A,$D9,$22,$D5,$A2,$FA,$8A,$39
        .byte   $A3,$FB,$88,$EF,$80,$BC,$09,$2F
        .byte   $AA,$FB,$A8,$DA,$0A,$FF,$A0,$A7
        .byte   $A8,$3E,$AA,$3F,$A8,$85,$A2,$58
        .byte   $0A,$D3,$AA,$74,$A0,$96,$88,$DF
        .byte   $2A,$D3,$2A,$E7,$BA,$B1,$A2,$ED
        .byte   $A0,$5A,$0A,$ED,$AA,$58,$82,$1E
        .byte   $AA,$EF,$8A,$FD,$A8,$95,$80,$7B
        .byte   $28,$FB,$88,$83,$28,$AF,$CA,$2F
        .byte   $88,$FB,$8E,$26,$A2,$80,$22,$4E
        .byte   $A2,$F7,$AA,$D3,$AA,$AA,$8E,$77
        .byte   $A8,$3F,$AE,$DF,$1E,$2D,$AA,$AF
        .byte   $2A,$4B,$80,$BC,$08,$FF,$2A,$B9
        .byte   $0A,$BA,$2A,$E9,$A8,$65,$8A,$9F
        .byte   $00,$BA,$08,$7A,$1A,$F3,$0A,$3D
        .byte   $A2,$FD,$AA,$79,$88,$EE,$A2,$5E
        .byte   $82,$4D,$28,$0C,$AA,$63,$A8,$57
        .byte   $AA,$AE,$A8,$82,$A2,$7F,$A8,$76
        .byte   $22,$A2,$AA,$C8,$A8,$FD,$0A,$FB
        .byte   $AA,$EF,$2A,$57,$18,$48,$B8,$F8
        .byte   $58,$B8,$D8,$28,$48,$58,$88,$B8
        .byte   $3E,$97,$98,$A8,$B8,$98,$48,$28
        .byte   $68,$98,$A8,$38,$68,$A0,$89,$8C
        .byte   $C7,$AC,$80,$A0,$89,$8C,$C7,$AC
        .byte   $98,$C8,$B8,$D0,$18,$58,$98,$E8
        .byte   $30,$C0,$FF,$10,$2E,$00,$44,$00
        .byte   $08,$04,$CA,$00,$39,$04,$92,$10
        .byte   $D0,$05,$0D,$00,$91,$14,$2A,$04
        .byte   $02,$00,$00,$41,$10,$01,$29,$14
        .byte   $10,$40,$80,$00,$92,$04,$68,$05
        .byte   $03,$04,$46,$44,$84,$00,$96,$05
        .byte   $80,$80,$00,$00,$A4,$50,$38,$00
        .byte   $10,$00,$16,$04,$80,$44,$16,$01
        .byte   $61,$44,$71,$04,$12,$01,$81,$10
        .byte   $A2,$54,$47,$00,$13,$48,$50,$00
        .byte   $44,$00,$00,$04,$10,$00,$40,$01
        .byte   $06,$10,$20,$04,$40,$00,$80,$10
        .byte   $80,$50,$00,$01,$20,$00,$9A,$08
        .byte   $01,$40,$04,$00,$1C,$00,$C4,$40
        .byte   $20,$41,$C0,$00,$62,$14,$C0,$01
        .byte   $80,$10,$00,$00,$40,$01,$60,$00
        .byte   $BB,$04,$00,$40,$80,$10,$C9,$D0
        .byte   $02,$50,$30,$01,$09,$40,$32,$01
        .byte   $40,$80,$02,$A0,$08,$10,$A0,$40
        .byte   $00,$00,$A0,$04,$08,$00,$40,$50
        .byte   $84,$90,$10,$00,$05,$00,$8A,$12
        .byte   $60,$02,$00,$00,$18,$40,$08,$00
        .byte   $54,$94,$09,$44,$22,$40,$00,$07
        .byte   $04,$50,$6B,$40,$90,$04,$70,$50
        .byte   $40,$10,$84,$51,$B0,$00,$82,$50
        .byte   $07,$21,$29,$10,$0E,$48,$43,$40
        .byte   $1A,$00,$E0,$51,$88,$78,$88,$B8
        .byte   $B8,$48,$88,$B8,$28,$88,$48,$48
        .byte   $30,$5A,$98,$5A,$5A,$68,$A4,$44
        .byte   $58,$58,$A4,$54,$74,$00,$43,$53
        .byte   $3F,$2F,$50,$00,$73,$83,$6F,$5F
        .byte   $64,$48,$30,$F0,$04,$F0,$04,$F0
        .byte   $04,$00,$FF,$51,$02,$21,$08,$01
        .byte   $0A,$41,$08,$00,$40,$50,$06,$04
        .byte   $0E,$44,$F2,$84,$00,$01,$44,$00
        .byte   $00,$51,$10,$00,$04,$00,$30,$00
        .byte   $04,$30,$00,$00,$CC,$00,$05,$10
        .byte   $80,$40,$35,$00,$26,$00,$21,$01
        .byte   $61,$01,$8C,$10,$00,$00,$06,$00
        .byte   $CC,$00,$67,$04,$0C,$40,$42,$51
        .byte   $07,$00,$92,$11,$95,$44,$36,$41
        .byte   $75,$10,$90,$04,$E2,$01,$6D,$00
        .byte   $9C,$14,$91,$05,$00,$00,$09,$00
        .byte   $00,$01,$80,$01,$05,$04,$A0,$04
        .byte   $81,$15,$00,$40,$0E,$00,$04,$40
        .byte   $00,$00,$18,$00,$0A,$01,$00,$01
        .byte   $04,$00,$59,$10,$04,$04,$56,$14
        .byte   $2A,$14,$41,$00,$C2,$40,$33,$11
        .byte   $A0,$00,$80,$11,$9C,$40,$48,$05
        .byte   $10,$60,$50,$01,$57,$11,$80,$01
        .byte   $00,$11,$30,$50,$62,$00,$34,$04
        .byte   $20,$00,$60,$00,$02,$11,$0D,$00
        .byte   $00,$00,$14,$40,$08,$41,$21,$00
        .byte   $00,$00,$91,$05,$0E,$00,$20,$01
        .byte   $C2,$20,$6C,$45,$F4,$40,$88,$05
        .byte   $46,$80,$61,$10,$09,$41,$4E,$00
        .byte   $C0,$20,$92,$04,$10,$15,$A1,$01
        .byte   $71,$18,$C1,$05,$C5,$44,$AC,$11
        .byte   $46,$07,$B3,$01,$20,$20,$20,$26
        .byte   $26,$20,$26,$26,$20,$20,$26,$50
        .byte   $62,$51,$52,$51,$51,$20,$12,$12
        .byte   $55,$50,$12,$09,$09,$77,$72,$73
        .byte   $75,$76,$62,$77,$72,$73,$75,$76
        .byte   $09,$50,$62,$27,$27,$27,$27,$27
        .byte   $27,$4B,$FF,$00,$02,$00,$98,$00
        .byte   $40,$00,$E4,$40,$41,$04,$9D,$00
        .byte   $91,$00,$82,$01,$20,$04,$14,$00
        .byte   $00,$00,$00,$00,$84,$11,$00,$00
        .byte   $08,$01,$24,$40,$28,$14,$E8,$59
        .byte   $00,$00,$21,$40,$40,$00,$00,$00
        .byte   $40,$40,$A1,$C0,$04,$01,$04,$40
        .byte   $40,$4C,$90,$40,$8C,$14,$01,$CC
        .byte   $09,$04,$74,$00,$02,$11,$10,$04
        .byte   $C8,$40,$98,$31,$98,$05,$45,$05
        .byte   $78,$04,$1C,$04,$01,$10,$10,$10
        .byte   $09,$00,$11,$11,$68,$01,$20,$10
        .byte   $21,$01,$30,$00,$00,$00,$20,$00
        .byte   $01,$10,$20,$10,$40,$40,$02,$00
        .byte   $3E,$00,$18,$40,$C8,$04,$A4,$10
        .byte   $40,$00,$31,$10,$46,$10,$07,$00
        .byte   $62,$C1,$00,$46,$B0,$C4,$90,$C0
        .byte   $20,$50,$48,$41,$C8,$42,$C0,$18
        .byte   $63,$50,$40,$50,$00,$10,$D2,$44
        .byte   $28,$00,$00,$41,$10,$01,$21,$40
        .byte   $4B,$05,$06,$00,$00,$C4,$08,$41
        .byte   $34,$10,$20,$01,$C8,$44,$04,$00
        .byte   $60,$14,$00,$10,$0E,$40,$14,$02
        .byte   $9E,$41,$A1,$51,$19,$45,$82,$44
        .byte   $29,$14,$B1,$15,$92,$42,$67,$40
        .byte   $28,$00,$E0,$10,$16,$00,$04,$84
        .byte   $D2,$40,$50,$01,$00,$00,$00,$01
        .byte   $02,$00,$00,$00,$03,$04,$05,$06
        .byte   $07,$08,$09,$08,$0A,$0B,$0C,$0D
        .byte   $0E,$03,$0F,$05,$10,$11,$12,$13
        .byte   $14,$15,$0B,$16,$0A,$0B,$0C,$17
        .byte   $18,$19,$1A,$1B,$19,$1C,$1B,$1D
        .byte   $1E,$08,$1F,$08,$20,$21,$20,$20
        .byte   $20,$20,$21,$20,$22,$23,$22,$22
        .byte   $22,$22,$23,$22,$01,$02,$00,$00
        .byte   $00,$01,$02,$00,$06,$07,$08,$09
        .byte   $08,$06,$07,$03,$0D,$0E,$03,$0F
        .byte   $05,$0D,$0E,$0A,$13,$14,$15,$0B
        .byte   $16,$13,$14,$10,$17,$18,$19,$24
        .byte   $1B,$17,$18,$0A,$1D,$1E,$25,$26
        .byte   $27,$1D,$1E,$19,$20,$20,$28,$29
        .byte   $2A,$20,$20,$20,$22,$22,$22,$23
        .byte   $22,$22,$22,$22,$00,$00,$01,$02
        .byte   $00,$00,$00,$00,$04,$05,$06,$07
        .byte   $03,$04,$05,$08,$0B,$0C,$0D,$0E
        .byte   $0A,$0B,$0C,$08,$11,$12,$13,$14
        .byte   $10,$11,$12,$08,$0B,$0C,$17,$18
        .byte   $0A,$0B,$2B,$2C,$1C,$1B,$1D,$1E
        .byte   $19,$1C,$2D,$2E,$21,$20,$20,$20
        .byte   $20,$21,$2F,$30,$23,$22,$22,$22
        .byte   $22,$23,$2F,$30,$00,$00,$00,$00
        .byte   $00,$00,$00,$31,$08,$08,$08,$08
        .byte   $08,$08,$08,$32,$08,$08,$08,$08
        .byte   $33,$2C,$08,$32,$08,$34,$35,$08
        .byte   $2D,$2E,$08,$32,$08,$36,$37,$08
        .byte   $38,$39,$08,$32,$08,$08,$08,$08
        .byte   $3A,$3B,$3C,$32,$20,$20,$21,$20
        .byte   $20,$20,$3D,$32,$22,$22,$23,$22
        .byte   $22,$22,$3E,$32,$3F,$00,$00,$00
        .byte   $00,$00,$40,$32,$41,$42,$08,$08
        .byte   $08,$08,$36,$32,$41,$43,$44,$08
        .byte   $08,$08,$08,$32,$41,$45,$2A,$44
        .byte   $08,$08,$08,$32,$41,$46,$22,$2A
        .byte   $44,$08,$08,$32,$41,$46,$47,$47
        .byte   $2A,$44,$08,$32,$41,$46,$47,$47
        .byte   $22,$2A,$20,$28,$41,$46,$22,$22
        .byte   $22,$22,$48,$22,$41,$49,$22,$22
        .byte   $4A,$00,$31,$22,$41,$4B,$00,$4C
        .byte   $4D,$08,$4E,$4F,$41,$37,$08,$50
        .byte   $51,$08,$08,$52,$41,$08,$08,$53
        .byte   $54,$55,$56,$52,$41,$08,$08,$57
        .byte   $08,$50,$51,$52,$2A,$20,$20,$20
        .byte   $20,$20,$58,$52,$22,$22,$22,$22
        .byte   $22,$22,$59,$5A,$22,$22,$22,$22
        .byte   $22,$22,$5B,$5C,$22,$4A,$00,$4F
        .byte   $22,$22,$02,$52,$4A,$5D,$08,$5E
        .byte   $00,$00,$07,$52,$4D,$08,$08,$08
        .byte   $08,$08,$37,$52,$4D,$08,$08,$08
        .byte   $08,$08,$42,$5F,$4D,$3C,$08,$08
        .byte   $42,$3C,$60,$61,$62,$63,$08,$08
        .byte   $60,$64,$08,$52,$65,$66,$20,$67
        .byte   $68,$68,$68,$69,$65,$6A,$22,$22
        .byte   $22,$22,$22,$22,$4D,$6B,$6C,$6D
        .byte   $6E,$6F,$6F,$70,$4D,$06,$71,$72
        .byte   $08,$08,$08,$08,$4D,$36,$08,$08
        .byte   $08,$08,$3C,$08,$4D,$08,$08,$08
        .byte   $08,$08,$73,$74,$4D,$08,$08,$42
        .byte   $08,$08,$36,$08,$75,$20,$76,$77
        .byte   $08,$08,$08,$08,$23,$22,$78,$79
        .byte   $20,$20,$20,$20,$22,$22,$22,$30
        .byte   $22,$22,$22,$22,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$7A,$7B,$7C,$7D
        .byte   $7E,$7A,$7B,$7C,$7F,$80,$81,$82
        .byte   $83,$7F,$80,$84,$74,$74,$74,$74
        .byte   $85,$86,$87,$73,$7A,$7B,$7C,$7D
        .byte   $88,$7A,$7B,$89,$7F,$80,$81,$82
        .byte   $8A,$7F,$80,$81,$20,$20,$20,$21
        .byte   $20,$20,$20,$21,$22,$22,$22,$23
        .byte   $22,$22,$22,$23,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$00,$00,$8C,$00
        .byte   $00,$8C,$00,$31,$7D,$8D,$8E,$08
        .byte   $08,$8E,$08,$32,$82,$8F,$90,$08
        .byte   $08,$91,$08,$32,$74,$74,$74,$74
        .byte   $74,$85,$08,$32,$7D,$7E,$7A,$7B
        .byte   $08,$37,$08,$32,$82,$8A,$7F,$80
        .byte   $08,$08,$3C,$32,$20,$20,$20,$21
        .byte   $20,$20,$3D,$32,$22,$22,$22,$23
        .byte   $22,$22,$3E,$32,$3F,$00,$00,$00
        .byte   $00,$00,$92,$32,$41,$08,$08,$08
        .byte   $08,$08,$36,$32,$41,$42,$08,$08
        .byte   $08,$08,$08,$32,$41,$93,$27,$08
        .byte   $08,$08,$08,$32,$41,$94,$2A,$27
        .byte   $08,$08,$08,$32,$41,$46,$47,$2A
        .byte   $27,$08,$08,$32,$41,$46,$47,$47
        .byte   $2A,$20,$20,$28,$41,$95,$22,$22
        .byte   $22,$22,$48,$22,$41,$96,$00,$00
        .byte   $00,$00,$00,$31,$41,$97,$97,$98
        .byte   $99,$9A,$98,$32,$41,$8B,$8B,$9B
        .byte   $9C,$9D,$9B,$32,$41,$9E,$9E,$9F
        .byte   $A0,$20,$A1,$32,$A2,$A3,$A3,$A4
        .byte   $A5,$22,$A6,$32,$22,$47,$47,$47
        .byte   $2F,$22,$A7,$32,$22,$22,$22,$22
        .byte   $2F,$22,$A7,$32,$22,$22,$22,$22
        .byte   $2F,$22,$A8,$32,$3F,$00,$00,$00
        .byte   $00,$00,$92,$32,$41,$08,$08,$08
        .byte   $08,$08,$36,$A9,$41,$08,$3C,$08
        .byte   $08,$08,$42,$AA,$41,$08,$73,$74
        .byte   $74,$74,$85,$AB,$41,$08,$36,$08
        .byte   $08,$08,$37,$08,$41,$08,$08,$08
        .byte   $08,$08,$08,$08,$2A,$20,$21,$20
        .byte   $20,$20,$21,$20,$22,$22,$23,$22
        .byte   $22,$22,$23,$22,$3F,$00,$00,$00
        .byte   $00,$00,$00,$00,$AC,$08,$08,$08
        .byte   $08,$08,$08,$AD,$08,$08,$08,$08
        .byte   $08,$08,$08,$AE,$08,$08,$08,$25
        .byte   $27,$08,$08,$AF,$08,$08,$25,$28
        .byte   $2A,$27,$08,$B0,$08,$25,$28,$47
        .byte   $47,$2A,$27,$AF,$20,$28,$29,$29
        .byte   $29,$29,$2A,$20,$22,$22,$23,$23
        .byte   $23,$23,$22,$22,$00,$00,$00,$00
        .byte   $2F,$22,$A7,$32,$B1,$B1,$B2,$B1
        .byte   $38,$22,$A8,$32,$97,$97,$B3,$97
        .byte   $B4,$B5,$B6,$32,$8B,$8B,$9B,$8B
        .byte   $B7,$B8,$B9,$32,$BA,$BA,$9F,$BA
        .byte   $BB,$BC,$BD,$BE,$BF,$BF,$9B,$BF
        .byte   $2D,$22,$22,$22,$20,$20,$21,$20
        .byte   $2F,$22,$22,$22,$22,$22,$23,$22
        .byte   $2F,$22,$22,$22,$41,$C0,$00,$00
        .byte   $00,$00,$00,$31,$41,$C1,$08,$08
        .byte   $08,$08,$08,$32,$41,$08,$08,$08
        .byte   $08,$08,$C2,$32,$41,$08,$C3,$08
        .byte   $C3,$08,$08,$32,$41,$08,$08,$08
        .byte   $08,$08,$C2,$32,$41,$08,$08,$08
        .byte   $08,$08,$3C,$32,$2A,$68,$68,$68
        .byte   $C4,$20,$A1,$32,$22,$22,$22,$22
        .byte   $22,$22,$A6,$32,$47,$30,$00,$00
        .byte   $00,$00,$00,$00,$47,$30,$08,$08
        .byte   $3C,$08,$08,$3C,$47,$30,$08,$3C
        .byte   $73,$74,$74,$C5,$47,$02,$08,$73
        .byte   $85,$08,$08,$25,$3F,$07,$C6,$C7
        .byte   $C8,$C9,$08,$32,$41,$CA,$CB,$CC
        .byte   $CC,$CD,$08,$32,$41,$93,$20,$20
        .byte   $20,$20,$20,$28,$41,$CE,$22,$22
        .byte   $22,$22,$22,$22,$00,$00,$00,$00
        .byte   $00,$07,$08,$08,$08,$08,$08,$08
        .byte   $08,$37,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$20,$20,$44,$08
        .byte   $08,$08,$08,$08,$47,$47,$2A,$44
        .byte   $08,$08,$08,$08,$47,$47,$22,$2A
        .byte   $44,$42,$08,$08,$47,$47,$22,$22
        .byte   $2A,$CF,$08,$08,$47,$47,$22,$22
        .byte   $22,$D0,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$08,$08
        .byte   $08,$08,$08,$08,$08,$08,$8C,$22
        .byte   $22,$22,$22,$22,$08,$08,$D1,$00
        .byte   $00,$00,$00,$00,$08,$08,$D2,$08
        .byte   $08,$08,$08,$D3,$08,$08,$D4,$08
        .byte   $08,$08,$3C,$D3,$08,$08,$D5,$D6
        .byte   $D6,$D6,$A0,$20,$08,$08,$2D,$22
        .byte   $22,$22,$A5,$22,$08,$08,$2F,$22
        .byte   $22,$22,$2F,$22,$08,$08,$2F,$22
        .byte   $22,$22,$D7,$22,$47,$22,$47,$22
        .byte   $47,$22,$47,$22,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$D8,$08,$08,$08
        .byte   $08,$08,$08,$D3,$D8,$08,$08,$08
        .byte   $08,$08,$08,$D3,$20,$20,$20,$20
        .byte   $20,$20,$20,$20,$47,$22,$47,$22
        .byte   $47,$22,$47,$22,$22,$22,$22,$22
        .byte   $22,$22,$22,$22,$22,$22,$22,$22
        .byte   $22,$22,$22,$22,$4A,$00,$00,$00
        .byte   $00,$00,$00,$4C,$5D,$D9,$DA,$08
        .byte   $08,$D9,$DA,$52,$D8,$13,$14,$08
        .byte   $08,$13,$14,$52,$D8,$DB,$DC,$08
        .byte   $08,$DB,$DC,$52,$DD,$13,$14,$08
        .byte   $08,$13,$14,$52,$4D,$DE,$DF,$08
        .byte   $08,$DE,$DF,$52,$E0,$21,$21,$20
        .byte   $20,$21,$21,$E1,$22,$23,$23,$22
        .byte   $22,$23,$23,$22,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$8B,$8B,$8B,$8B
        .byte   $8B,$8B,$8B,$8B,$16,$15,$AE,$AD
        .byte   $B8,$15,$98,$16,$16,$BB,$15,$9B
        .byte   $04,$04,$04,$C0,$D4,$D5,$C1,$C2
        .byte   $04,$04,$C3,$04,$A4,$15,$AC,$AD
        .byte   $16,$A7,$AE,$AF,$04,$04,$04,$04
        .byte   $D9,$DA,$D4,$D5,$04,$C8,$04,$C8
        .byte   $C9,$CA,$C9,$CA,$CB,$04,$CB,$04
        .byte   $09,$D4,$C0,$C1,$D5,$09,$C2,$C3
        .byte   $D9,$DA,$C1,$C2,$04,$D0,$04,$C0
        .byte   $D1,$D2,$C1,$C2,$D3,$04,$C3,$04
        .byte   $C8,$C9,$C8,$C9,$CA,$CB,$CA,$CB
        .byte   $C4,$C8,$C4,$C8,$CB,$C4,$CB,$C4
        .byte   $D0,$D1,$04,$D9,$D2,$D3,$DA,$04
        .byte   $04,$D0,$04,$04,$D1,$D2,$D9,$DA
        .byte   $D3,$04,$04,$04,$D1,$D2,$DC,$DD
        .byte   $04,$DC,$04,$D9,$DD,$04,$DA,$04
        .byte   $DC,$DD,$D9,$DA,$A2,$A1,$15,$16
        .byte   $A2,$A1,$70,$71,$16,$15,$15,$16
        .byte   $78,$79,$15,$16,$BF,$B7,$BC,$BD
        .byte   $BF,$A1,$BC,$16,$A6,$A5,$15,$16
        .byte   $A2,$B7,$15,$BD,$A6,$15,$15,$16
        .byte   $16,$15,$70,$71,$16,$A5,$15,$16
        .byte   $CB,$04,$A0,$A1,$04,$08,$A2,$A3
        .byte   $A8,$15,$B0,$16,$16,$AB,$15,$B3
        .byte   $B8,$15,$B8,$16,$16,$BB,$15,$BB
        .byte   $16,$15,$AA,$16,$B4,$15,$BC,$16
        .byte   $08,$04,$A0,$A1,$08,$04,$10,$11
        .byte   $04,$08,$11,$12,$09,$04,$04,$04
        .byte   $04,$09,$04,$04,$98,$15,$A4,$16
        .byte   $16,$9B,$15,$A7,$AC,$AE,$09,$04
        .byte   $AD,$AF,$04,$09,$04,$04,$08,$04
        .byte   $A3,$06,$AB,$07,$9A,$07,$A7,$07
        .byte   $16,$15,$15,$A9,$A7,$07,$AF,$07
        .byte   $16,$B5,$15,$BD,$04,$04,$04,$08
        .byte   $04,$A0,$04,$A8,$B7,$04,$A5,$B7
        .byte   $06,$B0,$07,$B8,$07,$B8,$07,$B8
        .byte   $70,$71,$78,$79,$16,$16,$15,$16
        .byte   $04,$B8,$04,$98,$16,$15,$A9,$AD
        .byte   $04,$A4,$04,$AC,$16,$15,$AE,$BE
        .byte   $BD,$04,$B5,$04,$B4,$15,$9E,$AD
        .byte   $16,$15,$AE,$AA,$04,$9E,$04,$04
        .byte   $9F,$04,$04,$04,$04,$BC,$04,$B4
        .byte   $BF,$A1,$BC,$A9,$A2,$A1,$AE,$AD
        .byte   $A2,$A1,$AE,$AA,$B7,$04,$BD,$04
        .byte   $9E,$9F,$04,$04,$B7,$04,$B5,$08
        .byte   $A5,$A0,$15,$A8,$06,$BC,$07,$B4
        .byte   $16,$B0,$15,$B8,$07,$BC,$07,$B4
        .byte   $BD,$04,$9F,$04,$04,$BC,$04,$9E
        .byte   $04,$BC,$08,$B4,$04,$10,$04,$09
        .byte   $12,$BC,$09,$B4,$BD,$06,$B5,$07
        .byte   $A3,$04,$AB,$04,$12,$04,$09,$04
        .byte   $BD,$07,$B5,$07,$B3,$A1,$BB,$16
        .byte   $D6,$C6,$15,$CE,$C6,$C7,$CE,$CF
        .byte   $C6,$B6,$CE,$16,$BB,$15,$BB,$16
        .byte   $BB,$15,$98,$16,$16,$15,$15,$70
        .byte   $16,$15,$71,$A9,$16,$A9,$AE,$9F
        .byte   $AD,$AE,$04,$04,$BE,$15,$9E,$AD
        .byte   $16,$78,$AE,$AD,$79,$B5,$AE,$9F
        .byte   $A0,$B1,$AC,$B9,$B2,$B1,$BA,$B9
        .byte   $A5,$A1,$70,$71,$A2,$A1,$15,$70
        .byte   $A2,$A3,$71,$AB,$16,$78,$15,$16
        .byte   $79,$B3,$15,$BB,$C0,$C1,$C8,$C9
        .byte   $C2,$C3,$CA,$CB,$04,$C0,$C4,$C8
        .byte   $C1,$C2,$C9,$CA,$C3,$04,$CB,$C4
        .byte   $C8,$C9,$D0,$D1,$CA,$CB,$D2,$D3
        .byte   $C4,$C8,$04,$D0,$C9,$CA,$D1,$D2
        .byte   $CB,$C4,$D3,$08,$C4,$C8,$08,$D0
        .byte   $B2,$A3,$BA,$AF,$04,$D9,$04,$DC
        .byte   $DA,$04,$DD,$04,$C3,$09,$CB,$C4
        .byte   $09,$C0,$C4,$C8,$CB,$C4,$D3,$04
        .byte   $00,$00,$00,$00,$B8,$BB,$B8,$BB
        .byte   $C3,$04,$CB,$04,$98,$9B,$A4,$A7
        .byte   $CB,$04,$D3,$04,$AC,$AF,$09,$09
        .byte   $AC,$AF,$09,$0B,$A7,$04,$AF,$04
        .byte   $06,$A0,$07,$A8,$07,$B0,$07,$B8
        .byte   $07,$98,$07,$A4,$00,$A4,$00,$AC
        .byte   $81,$81,$00,$00,$82,$81,$83,$00
        .byte   $81,$81,$84,$85,$81,$81,$86,$87
        .byte   $83,$00,$83,$00,$8C,$8D,$94,$95
        .byte   $8E,$8F,$96,$97,$91,$92,$81,$81
        .byte   $83,$00,$83,$81,$A0,$A1,$A8,$16
        .byte   $A3,$0D,$AB,$0E,$16,$B5,$15,$A5
        .byte   $00,$00,$A1,$A2,$83,$00,$A1,$A2
        .byte   $B0,$15,$B8,$16,$B3,$0E,$BB,$0E
        .byte   $BB,$0E,$BB,$0E,$9B,$0E,$A7,$0E
        .byte   $B4,$15,$BC,$A9,$B4,$B5,$BC,$BD
        .byte   $A6,$B5,$AD,$9F,$16,$B5,$AE,$9F
        .byte   $80,$00,$88,$00,$88,$81,$88,$00
        .byte   $88,$00,$88,$00,$88,$00,$88,$81
        .byte   $00,$00,$91,$92,$82,$00,$83,$00
        .byte   $83,$81,$83,$00,$AC,$AE,$00,$00
        .byte   $AD,$AE,$00,$00,$AF,$0E,$82,$0E
        .byte   $84,$85,$8C,$8D,$86,$87,$8E,$8F
        .byte   $83,$0E,$83,$0E,$00,$00,$81,$81
        .byte   $94,$95,$A0,$A1,$96,$97,$A2,$A1
        .byte   $83,$00,$A2,$A1,$B4,$15,$B6,$16
        .byte   $00,$00,$89,$8A,$07,$A4,$07,$AC
        .byte   $07,$09,$07,$04,$04,$04,$13,$04
        .byte   $04,$04,$04,$13,$D7,$A1,$15,$16
        .byte   $A3,$04,$AF,$04,$08,$04,$A0,$B1
        .byte   $09,$04,$B2,$B1,$04,$09,$B2,$B1
        .byte   $04,$08,$B2,$A3,$04,$09,$04,$08
        .byte   $AC,$B9,$09,$04,$BA,$B9,$04,$04
        .byte   $BA,$AF,$04,$09,$07,$99,$07,$A4
        .byte   $A2,$A3,$15,$AB,$16,$B3,$15,$BB
        .byte   $B8,$BB,$98,$9B,$A4,$A7,$AC,$AF
        .byte   $04,$0F,$04,$0F,$09,$09,$04,$04
        .byte   $08,$04,$A0,$A2,$04,$04,$A1,$A2
        .byte   $B8,$15,$BB,$16,$17,$04,$17,$04
        .byte   $04,$D4,$C0,$C1,$D5,$04,$C2,$C3
        .byte   $D0,$D1,$C0,$C1,$D2,$D3,$C2,$C3
        .byte   $B7,$04,$B5,$04,$D0,$D1,$04,$DC
        .byte   $D2,$D3,$DD,$04,$A5,$A1,$15,$16
        .byte   $A2,$B6,$15,$16,$16,$AB,$15,$BB
        .byte   $09,$07,$04,$07,$A2,$B0,$15,$B8
        .byte   $16,$B8,$15,$B8,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$10,$0C
        .byte   $11,$00,$0E,$0E,$11,$13,$11,$13
        .byte   $11,$8C,$8C,$1E,$28,$26,$2A,$6E
        .byte   $FF,$22,$20,$11,$02,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$3B,$4C,$4C,$5C
        .byte   $5B,$67,$BC,$67,$4B,$4C,$4C,$4C
        .byte   $4C,$5C,$4B,$4C,$11,$11,$FF,$04
        .byte   $FF,$FF,$FF,$FF,$B0,$10,$A0,$79
        .byte   $FF,$FF,$FF,$FF,$11,$11,$C1,$C3
        .byte   $C5,$00,$FE,$00,$11,$CD,$10,$10
        .byte   $10,$CF,$FE,$00,$D6,$10,$10,$8E
        .byte   $10,$D8,$11,$11,$DE,$10,$AC,$AE
        .byte   $10,$E0,$FD,$FD,$E6,$10,$10,$10
        .byte   $10,$E8,$FE,$11,$11,$EE,$F0,$10
        .byte   $F2,$FB,$00,$00,$88,$8A,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$A8,$AA,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$00,$00,$00,$60
        .byte   $00,$62,$00,$00,$50,$40,$42,$B8
        .byte   $80,$82,$84,$86,$FF,$00,$00,$FF
        .byte   $00,$A2,$A4,$A6,$7C,$68,$6A,$7C
        .byte   $FF,$FF,$4F,$44,$28,$24,$26,$2A
        .byte   $28,$22,$0B,$6D,$48,$20,$09,$6D
        .byte   $48,$44,$46,$4A,$68,$06,$06,$6A
        .byte   $2C,$22,$0B,$26,$7C,$20,$22,$7C
        .byte   $2C,$20,$09,$2F,$11,$E4,$11,$11
        .byte   $D1,$00,$11,$11,$D0,$C5,$C4,$C6
        .byte   $00,$00,$AC,$AE,$F0,$F2,$F1,$F1
        .byte   $E4,$11,$26,$CE,$00,$E4,$11,$00
        .byte   $F4,$F5,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$10,$0D
        .byte   $11,$00,$0F,$0F,$11,$13,$11,$13
        .byte   $11,$8F,$8F,$1F,$29,$24,$28,$6F
        .byte   $FF,$23,$21,$11,$02,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$4C,$4C,$2B,$5C
        .byte   $5C,$67,$67,$BF,$4C,$4C,$4D,$4C
        .byte   $4C,$5D,$4C,$4D,$11,$11,$FF,$05
        .byte   $FF,$FF,$FF,$FF,$10,$10,$B0,$7A
        .byte   $FF,$FF,$FF,$FF,$11,$C0,$C2,$C4
        .byte   $11,$00,$11,$00,$CC,$10,$10,$10
        .byte   $CE,$11,$FD,$00,$D7,$10,$8D,$10
        .byte   $10,$D9,$11,$FE,$DF,$10,$AD,$AF
        .byte   $10,$E1,$FD,$FE,$E7,$10,$10,$10
        .byte   $10,$E9,$11,$FE,$FA,$EF,$10,$F1
        .byte   $F3,$11,$00,$00,$89,$8B,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$A9,$AB,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$00,$00,$00,$61
        .byte   $00,$63,$00,$00,$51,$41,$00,$BA
        .byte   $81,$83,$85,$87,$FF,$00,$00,$FF
        .byte   $A1,$A3,$A5,$A7,$7D,$69,$6B,$7D
        .byte   $FF,$FF,$47,$4E,$29,$25,$27,$28
        .byte   $6C,$0A,$21,$28,$6C,$08,$23,$48
        .byte   $49,$45,$47,$48,$69,$07,$07,$6B
        .byte   $21,$2D,$23,$2E,$7D,$21,$23,$7D
        .byte   $23,$2D,$21,$27,$11,$11,$E4,$11
        .byte   $D2,$00,$11,$11,$C4,$C6,$C5,$D3
        .byte   $00,$00,$AD,$AF,$F1,$F1,$F2,$F3
        .byte   $11,$E4,$CF,$27,$00,$11,$E4,$00
        .byte   $F5,$F6,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$10,$1C
        .byte   $11,$00,$0E,$0E,$03,$11,$03,$03
        .byte   $12,$8C,$8C,$1E,$58,$54,$5A,$7E
        .byte   $FF,$32,$30,$11,$4C,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$4B,$4C,$4C,$11
        .byte   $11,$4C,$4B,$4C,$5B,$5C,$5C,$3B
        .byte   $4C,$11,$4B,$4C,$11,$B0,$FF,$14
        .byte   $FF,$FF,$FF,$FF,$10,$10,$10,$79
        .byte   $FF,$FF,$FF,$FF,$11,$C6,$C8,$10
        .byte   $CA,$11,$FE,$FE,$D0,$D2,$10,$10
        .byte   $10,$D4,$FE,$11,$DA,$10,$9C,$9E
        .byte   $10,$DC,$FC,$FC,$E2,$10,$10,$BE
        .byte   $10,$E4,$11,$11,$11,$EB,$10,$10
        .byte   $10,$ED,$FE,$11,$11,$11,$F5,$F7
        .byte   $F9,$FB,$11,$FB,$98,$9A,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$B8,$BA,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$50,$52,$60,$60
        .byte   $70,$72,$74,$76,$50,$00,$00,$B8
        .byte   $90,$92,$94,$96,$FF,$40,$42,$FF
        .byte   $00,$B2,$B4,$B6,$78,$78,$7C,$7C
        .byte   $FF,$FF,$5F,$55,$38,$34,$36,$3A
        .byte   $38,$32,$1B,$6D,$58,$30,$19,$6D
        .byte   $58,$54,$56,$5A,$7C,$32,$30,$7C
        .byte   $3C,$32,$1B,$36,$7C,$16,$16,$7C
        .byte   $3C,$30,$19,$3F,$C0,$C2,$C1,$C1
        .byte   $E1,$00,$9C,$9E,$E0,$D5,$D4,$D6
        .byte   $00,$00,$BC,$BE,$11,$E4,$11,$11
        .byte   $F4,$F5,$36,$DE,$00,$E4,$11,$00
        .byte   $E4,$11,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$10,$1D
        .byte   $11,$00,$0F,$0F,$03,$11,$03,$03
        .byte   $12,$8F,$8F,$1F,$59,$56,$58,$7F
        .byte   $FF,$33,$31,$11,$4C,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$4C,$4C,$4D,$11
        .byte   $11,$4C,$4C,$4D,$5C,$5C,$5D,$4C
        .byte   $2B,$11,$4C,$4D,$A0,$A0,$FF,$15
        .byte   $FF,$FF,$FF,$FF,$10,$10,$10,$7A
        .byte   $FF,$FF,$FF,$FF,$11,$C7,$10,$C9
        .byte   $CB,$11,$FC,$11,$D1,$10,$10,$10
        .byte   $D3,$D5,$11,$FE,$DB,$10,$9D,$9F
        .byte   $10,$DD,$FC,$FE,$E3,$10,$BD,$10
        .byte   $10,$E5,$11,$FE,$EA,$10,$10,$10
        .byte   $EC,$11,$11,$FE,$FA,$F4,$F6,$F8
        .byte   $11,$11,$FA,$11,$99,$9B,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$B9,$BB,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$51,$52,$61,$61
        .byte   $71,$73,$75,$77,$51,$00,$00,$BA
        .byte   $91,$93,$95,$97,$FF,$41,$00,$FF
        .byte   $B1,$B3,$B5,$B7,$7D,$7D,$7B,$7B
        .byte   $FF,$FF,$57,$5E,$39,$35,$37,$38
        .byte   $6C,$1A,$31,$38,$6C,$18,$33,$58
        .byte   $59,$55,$57,$58,$7D,$33,$31,$7D
        .byte   $31,$3D,$33,$3E,$7D,$17,$17,$7D
        .byte   $33,$3D,$31,$37,$C1,$C1,$C2,$C3
        .byte   $E2,$00,$9D,$9F,$D4,$D6,$D5,$E3
        .byte   $00,$00,$BD,$BF,$11,$11,$E4,$11
        .byte   $F5,$F6,$DF,$37,$00,$11,$E4,$00
        .byte   $11,$E4,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$03,$52
        .byte   $00,$00,$40,$20,$00,$00,$02,$00
        .byte   $00,$40,$20,$12,$12,$10,$12,$10
        .byte   $10,$11,$11,$10,$13,$00,$00,$00
        .byte   $10,$00,$10,$00,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$00,$03
        .byte   $00,$00,$00,$00,$03,$03,$03,$03
        .byte   $00,$00,$00,$00,$02,$03,$03,$03
        .byte   $03,$02,$02,$02,$03,$03,$03,$03
        .byte   $03,$03,$02,$02,$03,$03,$03,$03
        .byte   $03,$03,$02,$02,$03,$03,$03,$03
        .byte   $03,$03,$02,$02,$03,$03,$03,$03
        .byte   $03,$03,$02,$02,$02,$03,$03,$03
        .byte   $03,$02,$02,$02,$12,$12,$00,$00
        .byte   $00,$00,$00,$00,$12,$11,$00,$00
        .byte   $00,$00,$00,$00,$02,$02,$02,$02
        .byte   $03,$03,$03,$03,$02,$02,$02,$03
        .byte   $03,$03,$03,$03,$00,$02,$02,$10
        .byte   $03,$03,$03,$03,$12,$12,$12,$12
        .byte   $00,$00,$10,$10,$12,$10,$10,$12
        .byte   $12,$11,$11,$12,$12,$11,$11,$12
        .byte   $12,$10,$10,$12,$12,$11,$11,$12
        .byte   $11,$11,$11,$10,$12,$11,$11,$12
        .byte   $11,$11,$11,$10,$03,$03,$03,$03
        .byte   $03,$00,$00,$00,$03,$03,$03,$03
        .byte   $00,$00,$50,$50,$03,$03,$03,$03
        .byte   $03,$03,$10,$10,$00,$03,$03,$00
        .byte   $03,$03,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00
