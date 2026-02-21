main_yellow_devil:
; =============================================================================
; MEGA MAN 3 (U) — BANK $12 — FORTRESS BOSSES + SPECIAL ENTITIES
; =============================================================================
; AI routines for Wily fortress bosses (Yellow Devil, Clone Mega Man, Wily
; Machine, Gamma) and special entities (breakable blocks, Wily capsule).
;
; Annotation: ~78% — 309 labels named, 52 inline comments
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $12 — FORTRESS BOSSES + SPECIAL ENTITIES
; =============================================================================
; Mapped to $A000-$BFFF. Contains AI routines for Wily fortress bosses
; and special entities. Dispatched from bank1C_1D for routine indices $E0-$FF.
; Known bosses: main_yellow_devil, main_wily_machine_A, main_wily_machine_B,
; main_gamma_B, main_gamma_F.
; Also serves as stage data for stage $12 (special/ending) via stage_to_bank.
;
; Annotation: partial — all 11 boss/entity entry points named, 309 auto labels remain
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

; --- External references (fixed bank + $8000 entry points, bank $1C) ---
L0000           := $0000
L8003           := $8003                ; entity AI dispatch
L8006           := $8006                ; entity AI dispatch (alt)
L8009           := $8009                ; check player hit
move_right_collide           := $F580
move_left_collide           := $F5C4
move_down_collide           := $F606
move_up_collide           := $F642
move_vertical_gravity           := $F67C
move_sprite_right           := $F71D
move_sprite_left           := $F73B
move_sprite_down           := $F759
move_sprite_up           := $F779
apply_y_speed           := $F797
reset_sprite_anim           := $F835
init_child_entity           := $F846
face_player           := $F869
submit_sound_ID_D9           := $F898
submit_sound_ID           := $F89A
entity_y_dist_to_player           := $F8B3
entity_x_dist_to_player           := $F8C2
check_player_collision           := $FAE2
check_sprite_weapon_collision           := $FB7B
find_enemy_freeslot_y           := $FC53
calc_homing_velocity           := $FC63
divide_16bit           := $FD11
call_bank0E_A006           := $FD8C
call_bank0E_A003           := $FDA6
update_CHR_banks           := $FF3C

.segment "BANK12"

        jmp     yellow_devil_dispatch

        jmp     yellow_devil_piece_left_update

        jmp     yellow_devil_piece_falling
main_wily_machine_A:

        jmp     wily_machine_a_dispatch

        jmp     wily_machine_b_move_dir
main_wily_machine_B:

        jmp     wily_machine_b_dispatch

        jmp     wily_machine_b_attack_vert
main_gamma_B:

        jmp     gamma_b_init

        jmp     gamma_b_main_update

        jmp     gamma_f_main
main_gamma_F:

        jmp     gamma_f_collision_check
main_teleporter:

        jmp     teleporter_collision

        jmp     teleporter_pos_check_y
main_wily_machine_C:

        nop
        nop
        rts

        jmp     teleporter_activate_check

        jmp     teleporter_fall
main_kamegoro_maker:

        jmp     kamegoro_current_init

        jmp     kamegoro_current_phase_init

        jmp     holograph_block_init
main_kamegoro_current:

        jmp     holograph_main_init

        jmp     holograph_current_dir_init
main_holograph:

        jmp     holograph_boss_init

        jmp     yellow_devil_noop

        jmp     yellow_devil_noop

        jmp     yellow_devil_noop

        jmp     teleporter_fall_rts

        jmp     wily_machine_c_block_fall

        jmp     wily_machine_c_block_y_update
main_giant_met:

        jmp     kamegoro_maker_init

yellow_devil_noop:  rts

yellow_devil_dispatch:  lda     ent_status,x
        and     #$0F
        tay
        lda     yellow_devil_routine_table,y
        sta     L0000
        lda     yellow_devil_addr_table,y
        sta     $01
        jmp     (L0000)

; Yellow Devil init — freeze player for boss intro

        lda     #$09                    ; state → $09 (boss_wait)
        cmp     player_state                     ; already in boss_wait?
        beq     yellow_devil_hp_bar_filled                   ; skip to HP fill
        sta     player_state                     ; freeze player
        lda     #$80                    ; init boss HP display
        sta     boss_hp_display
        sta     boss_active                     ; boss active flag
        lda     #$8E                    ; HP fill target (28 HP)
        sta     $B3
        lda     #MUSIC_BOSS                    ; SFX $0D = boss intro music
        jsr     submit_sound_ID_D9
yellow_devil_hp_bar_filled:  lda     boss_hp_display                     ; has HP bar filled to $9C?
        cmp     #$9C
        bne     yellow_devil_return
        inc     ent_status,x
        lda     #$02
        sta     ent_facing,x
        lda     #$FF
        sta     ent_timer,x
        lda     #$01
        sta     $10
yellow_devil_wait_timer:  lda     ent_timer,x
        beq     yellow_devil_spawn_piece
        dec     ent_timer,x
yellow_devil_return:  rts

yellow_devil_spawn_piece:  jsr     find_enemy_freeslot_y
        stx     L0000
        lda     ent_var1,x
        sta     ent_timer,y
        tax
        lda     yellow_devil_y_pos_table,x
        sta     ent_y_px,y
        lda     $10
        and     #$01
        bne     yellow_devil_spawn_left
        lda     yellow_devil_x_pos_right_table,x
        sta     ent_x_px,y
        lda     #$71
        sta     $01
        bne     yellow_devil_spawn_common
yellow_devil_spawn_left:  lda     #$04
        sta     ent_x_px,y
        lda     #$7E
        sta     $01
yellow_devil_spawn_common:  lda     #$80
        sta     ent_status,y
        sta     ent_hitbox,y
        lda     #$90
        sta     ent_flags,y
        lda     $01
        sta     ent_anim_id,y
        lda     $10
        sta     ent_facing,y
        lda     #$00
        sta     ent_y_scr,y
        sta     ent_anim_frame,y
        sta     ent_anim_state,y
        sta     ent_xvel_sub,y
        lda     #$04
        sta     ent_xvel,y
        lda     camera_screen
        sta     ent_x_scr,y
        lda     #$E1
        sta     ent_routine,y
        lda     $10
        and     #$01
        bne     yellow_devil_load_timer
        txa
        clc
        adc     #$18
        tax
yellow_devil_load_timer:  lda     yellow_devil_timer_table,x
        ldx     L0000
        sta     ent_timer,x
        inc     ent_var1,x
        lda     ent_timer,x
        bne     yellow_devil_spawn_complete
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$82
        sta     ent_status,x
        lda     #$78
        sta     ent_timer,x
        lda     #$03
        sta     ent_var1,x
        lda     #$00
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
yellow_devil_spawn_complete:  rts

        lda     ent_hitbox,x
        pha
        lda     #$18
        sta     ent_hitbox,x
        lda     ent_y_px,x
        pha
        clc
        adc     #$20
        sta     ent_y_px,x
        lda     ent_flags,x
        pha
        and     #$FB
        sta     ent_flags,x
        jsr     L8009
        pla
        sta     ent_flags,x
        pla
        sta     ent_y_px,x
        pla
        sta     ent_hitbox,x
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        lda     ent_timer,x
        bne     yellow_devil_piece_update
        lda     ent_anim_state,x
        cmp     #$02
        bne     yellow_devil_piece_collision
        jsr     find_enemy_freeslot_y
        bcs     yellow_devil_piece_collision
        lda     #$58
        jsr     init_child_entity
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$80
        sta     ent_hitbox,y
        lda     #$8F
        sta     ent_routine,y
        lda     #$00
        sta     ent_xvel_sub,y
        sta     $02
        lda     #$04
        sta     ent_xvel,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     ent_facing,y
        lda     #$15
        sta     ent_timer,x
        dec     ent_var1,x
        bne     yellow_devil_piece_update
        lda     ent_facing,x
        and     #$02
        tay
        lda     yellow_devil_status_table,y
        sta     ent_status,x
        lda     #$1E
        sta     ent_timer,x
        lda     #$00
        sta     ent_var1,x
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        rts

yellow_devil_piece_update:  dec     ent_timer,x
        lda     ent_anim_state,x
        bne     yellow_devil_piece_collision
        sta     ent_anim_frame,x
yellow_devil_piece_collision:  jsr     L8003
        lda     ent_hp,x
        bne     yellow_devil_death_end
        lda     #$0F
        ldy     #$03
yellow_devil_death_palette_loop:  sta     $0608,y
        sta     $0628,y
        dey
        bpl     yellow_devil_death_palette_loop
        sty     palette_dirty
yellow_devil_death_end:  rts

        lda     ent_timer,x
        bne     yellow_devil_piece_wait
        lda     ent_flags,x
        ora     #$04
        sta     ent_flags,x
        lda     #$38
        sta     ent_x_px,x
        lda     nametable_dirty
        beq     yellow_devil_piece_reassemble
        inc     ent_timer,x
        bne     yellow_devil_piece_wait
yellow_devil_piece_reassemble:  ldy     ent_var1,x
        lda     yellow_devil_chr_even2_table,y
        sta     $0780
        sta     $0785
        lda     yellow_devil_chr_comp_table,y
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        lda     #$00
        sta     $0783
        sta     $0784
        sta     $0788
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     nametable_dirty
yellow_devil_piece_wait:  lda     ent_anim_state,x
        bne     yellow_devil_piece_continue
        sta     ent_anim_frame,x
yellow_devil_piece_continue:  lda     #$02
        sta     $10
        jmp     yellow_devil_wait_timer

        lda     ent_timer,x
        beq     yellow_devil_piece_flatten
        dec     ent_timer,x
        lda     ent_anim_state,x
        bne     yellow_devil_piece_timer_dec
        sta     ent_anim_frame,x
yellow_devil_piece_timer_dec:  rts

yellow_devil_piece_flatten:  lda     ent_flags,x
        ora     #$04
        sta     ent_flags,x
        stx     L0000
        ldy     ent_var1,x
        lda     yellow_devil_chr_seq_table,y
        sta     $0780
        sta     $078D
        lda     yellow_devil_chr_offset_table,y
        sta     $0781
        ora     #$01
        sta     $078E
        ldx     #$09
        stx     $0782
        stx     $078F
        lda     #$00
yellow_devil_palette_clear_loop:  sta     $0783,x
        sta     $0790,x
        dex
        bpl     yellow_devil_palette_clear_loop
        stx     $079A
        stx     nt_column_dirty
        lda     #$98
        sta     $01
        lda     yellow_devil_y_step_table,y
        sta     $02
        lda     #$00
        sta     $03
        sty     $04
yellow_devil_body_spawn_loop:  jsr     find_enemy_freeslot_y
        bcs     yellow_devil_body_spawn_done
        lda     #$71
        sta     ent_anim_id,y
        lda     #$00
        sta     ent_anim_frame,y
        sta     ent_anim_state,y
        sta     ent_y_scr,y
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$80
        sta     ent_status,y
        sta     ent_hitbox,y
        lda     #$90
        sta     ent_flags,y
        lda     #$E2
        sta     ent_routine,y
        lda     $03
        sta     ent_timer,y
        lda     camera_screen
        sta     ent_x_scr,y
        lda     $02
        sta     ent_x_px,y
        lda     $01
        sta     ent_y_px,y
        sec
        sbc     #$10
        sta     $01
        cmp     #$88
        bne     yellow_devil_body_pos_next
        ldx     $04
        lda     yellow_devil_x_pos_table,x
        sta     ent_var2,y
        lda     yellow_devil_var1_pos_table,x
        sta     ent_var1,y
yellow_devil_body_pos_next:  inc     $03
        cmp     #$48
        bne     yellow_devil_body_spawn_loop
yellow_devil_body_spawn_done:  ldx     L0000
        lda     #$14
        sta     ent_timer,x
        inc     ent_var1,x
        lda     ent_var1,x
        cmp     #$05
        bne     yellow_devil_body_spawn_end
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$82
        sta     ent_status,x
        lda     #$C8
        sta     ent_x_px,x
        lda     #$F0
        sta     ent_timer,x
        lda     #$03
        sta     ent_var1,x
        lda     #$00
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
yellow_devil_body_spawn_end:  rts

yellow_devil_routine_table:  .byte   $6B,$95,$3E,$0D,$6B
yellow_devil_addr_table:  .byte   $A0,$A0,$A1,$A2,$A2
yellow_devil_piece_left_update:  lda     ent_anim_id,x
        cmp     #$71
        bne     yellow_devil_piece_right_update
        lda     ent_anim_state,x
        cmp     #$02
        bne     yellow_devil_piece_left_return
        lda     #$7E
        jsr     reset_sprite_anim
yellow_devil_piece_left_return:  rts

yellow_devil_piece_right_update:  lda     ent_facing,x
        and     #$02
        tay
        lda     yellow_devil_facing_offset,y
        clc
        adc     ent_timer,x
        tay
        lda     ent_status,x
        and     #$0F
        bne     yellow_devil_piece_right_anim
        lda     ent_facing,x
        and     #$01
        beq     yellow_devil_piece_right_left
        jsr     move_sprite_right
        jmp     yellow_devil_piece_right_cont

yellow_devil_piece_right_left:  jsr     move_sprite_left
yellow_devil_piece_right_cont:  lda     yellow_devil_x_pos_left_table,y
        cmp     ent_x_px,x
        bne     yellow_devil_piece_right_end
        inc     ent_status,x
        lda     #$7F
        jsr     reset_sprite_anim
yellow_devil_piece_right_anim:  lda     ent_anim_state,x
        cmp     #$03
        bne     yellow_devil_piece_right_end
        lda     nametable_dirty
        bne     yellow_devil_piece_right_nt
        lda     #$00
        sta     ent_status,x
        lda     yellow_devil_chr_even_table,y
        sta     $0780
        sta     $0785
        lda     yellow_devil_chr_odd_table,y
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        lda     yellow_devil_chr_index_table,y
        asl     a
        asl     a
        tay
        lda     yellow_devil_chr_attr_80,y
        sta     $0783
        lda     yellow_devil_chr_attr_81,y
        sta     $0784
        lda     yellow_devil_chr_attr_80_dup,y
        sta     $0788
        lda     yellow_devil_chr_attr_large,y
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     nametable_dirty
        lda     ent_timer,x
        cmp     #$17
        bne     yellow_devil_piece_right_nt
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $051F
        lda     $04BF
        and     #$01
        tay
        lda     yellow_devil_palette_indices,y
        sta     $05DF
yellow_devil_piece_right_nt:  lda     #$00
        sta     ent_anim_frame,x
yellow_devil_piece_right_end:  rts

yellow_devil_piece_falling:  lda     ent_anim_id,x
        cmp     #$7E
        beq     yellow_devil_piece_fall_move
        cmp     #$7F
        beq     yellow_devil_piece_fall_delay
        lda     ent_anim_state,x
        cmp     #$02
        bne     yellow_devil_piece_right_end
        lda     #$7E
        jmp     reset_sprite_anim

yellow_devil_piece_fall_move:  jmp     yellow_devil_piece_fall_descent

yellow_devil_piece_fall_delay:  lda     #$3C
        sta     $051F
        lda     ent_anim_state,x
        cmp     #$03
        bne     yellow_devil_piece_right_end
        lda     nametable_dirty
        bne     yellow_devil_piece_fall_anim
        ldy     ent_timer,x
        lda     yellow_devil_timer_seq_table,y
        bmi     yellow_devil_piece_fall_death
        pha
        lda     yellow_devil_nt_even_table,y
        sta     $0780
        sta     $0785
        lda     yellow_devil_nt_odd_table,y
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        pla
        asl     a
        asl     a
        tay
        lda     yellow_devil_chr_attr_80,y
        sta     $0783
        lda     yellow_devil_chr_attr_81,y
        sta     $0784
        lda     yellow_devil_chr_attr_80_dup,y
        sta     $0788
        lda     yellow_devil_chr_attr_large,y
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     nametable_dirty
        lda     ent_timer,x
        cmp     #$14
        bne     yellow_devil_piece_fall_death
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $051F
        lda     #$70
        sta     $05DF
yellow_devil_piece_fall_death:  lda     #$00
        sta     ent_status,x
        rts

yellow_devil_piece_fall_anim:  lda     #$00
        sta     ent_anim_frame,x
        rts

yellow_devil_piece_fall_descent:  lda     ent_timer,x
        beq     yellow_devil_piece_fall_var1_zero
        jsr     move_sprite_down
        lda     ent_y_px,x
        cmp     #$98
        bne     yellow_devil_piece_fall_check
        lda     #$00
        sta     ent_status,x
yellow_devil_piece_fall_check:  rts

yellow_devil_piece_fall_var1_zero:  lda     ent_var1,x
        beq     yellow_devil_piece_fall_phase2
        dec     ent_var1,x
        bne     yellow_devil_piece_fall_check
        lda     #$02
        sta     ent_var3,x
yellow_devil_piece_fall_split:  dec     ent_var3,x
        bmi     yellow_devil_piece_fall_hi_vel
        lda     #$A3
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        lda     #$BD
        sta     ent_xvel_sub,x
        lda     #$01
        sta     ent_xvel,x
        rts

yellow_devil_piece_fall_hi_vel:  lda     #$87
        sta     ent_yvel_sub,x
        lda     #$06
        sta     ent_yvel,x
        lda     #$72
        sta     ent_xvel_sub,x
        lda     #$02
        sta     ent_xvel,x
        rts

yellow_devil_piece_fall_phase2:  lda     ent_status,x
        and     #$0F
        bne     yellow_devil_piece_fall_pixel
        jsr     apply_y_speed
        lda     #$98
        cmp     ent_y_px,x
        bcs     yellow_devil_piece_fall_right
        sta     ent_y_px,x
        bcc     yellow_devil_piece_fall_split
yellow_devil_piece_fall_right:  jsr     move_sprite_right
        lda     ent_var2,x
        cmp     ent_x_px,x
        bcs     yellow_devil_piece_fall_check
        sta     ent_x_px,x
        sbc     #$9F
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     yellow_devil_var3_timer_table,y
        sta     ent_var3,x
        lda     #$58
        sta     ent_y_px,x
        lda     #$00
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        inc     ent_status,x
yellow_devil_piece_fall_pixel:  lda     ent_y_px,x
        and     #$0F
        cmp     #$08
        bne     yellow_devil_piece_fall_down
        jsr     find_enemy_freeslot_y
        bcs     yellow_devil_piece_fall_loop_end
        lda     ent_var3,x
        sta     ent_timer,y
        inc     ent_var3,x
        lda     #$7F
        jsr     init_child_entity
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_hitbox,x
        sta     ent_hitbox,y
        lda     ent_routine,x
        sta     ent_routine,y
        lda     ent_y_px,x
        cmp     #$98
        bne     yellow_devil_piece_fall_down
        lda     #$00
        sta     ent_status,x
yellow_devil_piece_fall_loop_end:  rts

yellow_devil_piece_fall_down:  jmp     move_sprite_down

yellow_devil_palette_indices:  .byte   $6F,$70
yellow_devil_y_pos_table:  .byte   $98,$58,$88,$98,$68,$78,$88,$58
        .byte   $68,$88,$78,$68,$78,$98,$58,$78
        .byte   $68,$58,$98,$78,$88,$88,$68,$58
yellow_devil_x_pos_left_table:  .byte   $E8,$E8,$E8,$D8,$E8,$E8,$D8,$D8
        .byte   $D8,$C8,$D8,$C8,$C8,$B8,$C8,$B8
        .byte   $B8,$B8,$A8,$A8,$B8,$A8,$A8,$A8
        .byte   $18,$18,$18,$28,$18,$18,$28,$28
        .byte   $28,$38,$28,$38,$38,$48,$38,$48
        .byte   $48,$48,$58,$58,$48,$58,$58,$58
yellow_devil_x_pos_right_table:  .byte   $A8,$A8,$A8,$B8,$A8,$A8,$B8,$B8
        .byte   $B8,$C8,$B8,$C8,$C8,$D8,$C8,$D8
        .byte   $D8,$D8,$E8,$E8,$D8,$E8,$E8,$E8
yellow_devil_facing_offset:  .byte   $00
yellow_devil_status_table:  .byte   $83,$18,$84
yellow_devil_timer_table:  .byte   $26,$2C,$26,$28,$28,$2C,$28,$26
        .byte   $28,$2C,$26,$2C,$26,$2C,$2C,$26
        .byte   $26,$26,$2C,$24,$26,$28,$26,$00
        .byte   $26,$2C,$22,$2C,$28,$28,$28,$26
        .byte   $24,$30,$22,$2C,$22,$30,$28,$26
        .byte   $26,$22,$2C,$30,$26,$28,$26,$00
yellow_devil_chr_index_table:  .byte   $17,$04,$13,$16,$09,$0E,$12,$03
        .byte   $08,$11,$0D,$07,$0C,$15,$02,$0B
        .byte   $06,$01,$14,$0A,$10,$0F,$05,$00
        .byte   $14,$00,$0F,$15,$05,$0A,$10,$01
        .byte   $06,$11,$0B,$07,$0C,$16,$02,$0D
        .byte   $08,$03,$17,$0E,$12,$13,$09,$04
yellow_devil_chr_odd_table:  .byte   $5C,$5C,$1C,$5A,$9C,$DC,$1A,$5A
        .byte   $9A,$18,$DA,$98,$D8,$56,$58,$D6
        .byte   $96,$56,$54,$D4,$16,$14,$94,$54
        .byte   $42,$42,$02,$44,$82,$C2,$04,$44
        .byte   $84,$06,$C4,$86,$C6,$48,$46,$C8
        .byte   $88,$48,$4A,$CA,$08,$0A,$8A,$4A
yellow_devil_chr_even_table:  .byte   $22,$21,$22,$22,$21,$21,$22,$21
        .byte   $21,$22,$21,$21,$21,$22,$21,$21
        .byte   $21,$21,$22,$21,$22,$22,$21,$21
        .byte   $22,$21,$22,$22,$21,$21,$22,$21
        .byte   $21,$22,$21,$21,$21,$22,$21,$21
        .byte   $21,$21,$22,$21,$22,$22,$21,$21
yellow_devil_chr_comp_table:  .byte   $54,$54,$14,$56,$94,$D4,$16,$56
        .byte   $96,$18,$D6,$98,$D8,$5A,$58,$DA
        .byte   $9A,$5A,$5C,$DC,$1A,$1C,$9C,$5C
yellow_devil_chr_even2_table:  .byte   $22,$21,$22,$22,$21,$21,$22,$21
        .byte   $21,$22,$21,$21,$21,$22,$21,$21
        .byte   $21,$21,$22,$21,$22,$22,$21,$21
yellow_devil_chr_seq_table:  .byte   $21,$21,$21,$21,$21
yellow_devil_chr_offset_table:  .byte   $4A,$48,$46,$44,$42
yellow_devil_y_step_table:  .byte   $58,$48,$38,$28,$18
yellow_devil_x_pos_table:  .byte   $E8,$D8,$C8,$B8,$A8
yellow_devil_var1_pos_table:  .byte   $10,$1A,$10,$24,$1A
yellow_devil_var3_timer_table:  .byte   $14,$0F,$0A,$05,$00
yellow_devil_timer_seq_table:  .byte   $04,$09,$0E,$13,$17,$03,$08,$0D
        .byte   $12,$16,$02,$07,$0C,$11,$FF,$01
        .byte   $06,$0B,$10,$15,$00,$05,$0A,$0F
        .byte   $14
yellow_devil_nt_even_table:  .byte   $21,$21,$21,$22,$22,$21,$21,$21
        .byte   $22,$22,$21,$21,$21,$22,$22,$21
        .byte   $21,$21,$22,$22,$21,$21,$21,$22
        .byte   $22
yellow_devil_nt_odd_table:  .byte   $5C,$9C,$DC,$1C,$5C,$5A,$9A,$DA
        .byte   $1A,$5A,$58,$98,$D8,$18,$58,$56
        .byte   $96,$D6,$16,$56,$54,$94,$D4,$14
        .byte   $54
yellow_devil_chr_attr_80:  .byte   $80
yellow_devil_chr_attr_81:  .byte   $81
yellow_devil_chr_attr_80_dup:  .byte   $80
yellow_devil_chr_attr_large:  .byte   $89,$82,$83,$8A,$8B,$84,$85,$8C
        .byte   $8D,$86,$87,$8E,$8F,$88,$80,$90
        .byte   $80,$91,$92,$9A,$9B,$93,$94,$9C
        .byte   $9D,$95,$96,$9E,$9F,$97,$98,$A0
        .byte   $A1,$99,$80,$A2,$A3,$A4,$A5,$80
        .byte   $AD,$A6,$A7,$AE,$AF,$A8,$A8,$B0
        .byte   $B1,$A9,$AA,$B2,$B3,$AB,$AC,$B4
        .byte   $B5,$80,$80,$80,$BD,$B6,$B7,$BE
        .byte   $BF,$B8,$A8,$C0,$C1,$B9,$BA,$C2
        .byte   $C3,$BB,$BC,$C4,$80,$C5,$C6,$CD
        .byte   $CE,$C7,$C8,$CF,$D0,$C9,$CA,$D1
        .byte   $D2,$CB,$CC,$D3,$D4
wily_machine_a_dispatch:  lda     #$AB
        pha
        lda     #$03
        pha
        lda     ent_status,x
        and     #$0F
        tay
        lda     wily_machine_b_addr_a,y
        sta     L0000
        lda     wily_machine_b_addr_b,y
        sta     $01
        jmp     (L0000)

        lda     #$09
        cmp     player_state
        beq     wily_machine_a_hp_check
        sta     player_state
        lda     #$80
        sta     boss_hp_display
        sta     boss_active
        lda     #$8E
        sta     $B3
        lda     #MUSIC_BOSS
        jsr     submit_sound_ID_D9
        lda     #$00
        sta     $69
        sta     $6B
        sta     $6A
        ldy     #$08
wily_machine_a_oam_loop:  lda     $0377,y
        sta     $0577,y
        dey
        bpl     wily_machine_a_oam_loop
wily_machine_a_hp_check:  lda     boss_hp_display
        cmp     #$9C
        bne     wily_machine_a_return
        inc     ent_status,x
        lda     #$0D
        sta     game_mode
        lda     #$3A
        sta     $5E
        lda     #$40
        sta     ent_var1,x
        lda     #$32
        sta     ent_var2,x
        lda     #$40
        sta     $6A
wily_machine_a_return:  rts

        ldy     ent_timer,x
        lda     wily_machine_b_routine_ptr,y
        sta     L0000
        lda     wily_machine_b_addr_c,y
        sta     $01
        jmp     (L0000)

        lda     $5E
        cmp     #$5A
        beq     wily_machine_a_move_up_end
        lda     $5E
        clc
        adc     #$02
        sta     $5E
        cmp     #$5A
        bne     wily_machine_a_sprite_move
        lda     #$1E
        sta     ent_var1,x
wily_machine_a_sprite_move:  lda     $03DA
        pha
        lda     $03DB
        pha
        ldy     #$08
wily_machine_a_move_up_loop:  lda     $03D7,y
        clc
        adc     #$02
        sta     $03D7,y
        dey
        bpl     wily_machine_a_move_up_loop
        pla
        sta     $03DB
        pla
        sta     $03DA
wily_machine_a_move_up_end:  dec     ent_var1,x
        bne     wily_machine_a_move_continue
        inc     ent_timer,x
wily_machine_a_move_continue:  jmp     wily_machine_a_bullet_fire

        lda     $03DA
        pha
        lda     $03DB
        pha
        ldy     #$08
wily_machine_a_move_down_loop:  lda     $03D7,y
        sec
        sbc     #$02
        sta     $03D7,y
        dey
        bpl     wily_machine_a_move_down_loop
        pla
        sta     $03DB
        pla
        sta     $03DA
        lda     $5E
        sec
        sbc     #$02
        sta     $5E
        cmp     #$3A
        bne     wily_machine_a_move_down_end
        lda     #$40
        sta     ent_var1,x
        lda     $053E
        sta     ent_timer,x
wily_machine_a_move_down_end:  jmp     wily_machine_a_bullet_fire

        lda     ent_var1,x
        and     #$01
        bne     wily_machine_a_dec_y
        dec     $057D
        dec     $0579
        dec     $057B
wily_machine_a_dec_y:  dec     $03DB
        lda     ent_var1,x
        cmp     #$20
        bne     wily_machine_a_move_return
        inc     $05BD
        inc     $05B9
wily_machine_a_move_return:  jmp     wily_machine_a_move_timer_dec

        lda     ent_var1,x
        and     #$01
        bne     wily_machine_a_inc_y
        lda     $6A
        clc
        adc     #$01
        sta     $6A
        lda     $6B
        adc     #$00
        sta     $6B
        inc     $057C
        inc     $0578
        inc     $057A
        inc     $057D
        inc     $0579
        inc     $057B
wily_machine_a_inc_y:  inc     $03DB
        lda     ent_var1,x
        cmp     #$20
        bne     wily_machine_a_inc_return
        dec     $05BD
        dec     $05B9
        inc     $05BC
        inc     $05B8
wily_machine_a_inc_return:  jmp     wily_machine_a_move_timer_dec

        lda     ent_var1,x
        and     #$01
        bne     wily_machine_a_dec_x
        dec     $057C
        dec     $0578
        dec     $057A
wily_machine_a_dec_x:  dec     $03DA
        lda     ent_var1,x
        cmp     #$20
        bne     wily_machine_a_dec_x_return
        dec     $05BC
        dec     $05B8
wily_machine_a_dec_x_return:  jmp     wily_machine_a_move_timer_dec

        inc     $03DA
        jmp     wily_machine_a_move_timer_dec

        lda     ent_var1,x
        and     #$01
        bne     wily_machine_a_dec_x_alt
        inc     $057C
        inc     $0578
        inc     $057A
wily_machine_a_dec_x_alt:  dec     $03DA
        lda     ent_var1,x
        cmp     #$20
        bne     wily_machine_a_move_timer_dec
        inc     $05BC
        inc     $05B8
        jmp     wily_machine_a_move_timer_dec

        lda     ent_var1,x
        and     #$01
        bne     wily_machine_a_inc_x
        lda     $6A
        sec
        sbc     #$01
        sta     $6A
        lda     $6B
        sbc     #$00
        sta     $6B
        dec     $057D
        dec     $0579
        dec     $057B
        dec     $057C
        dec     $0578
        dec     $057A
wily_machine_a_inc_x:  inc     $03DA
        lda     ent_var1,x
        cmp     #$20
        bne     wily_machine_a_move_timer_dec
        dec     $05BC
        dec     $05B8
        inc     $05BD
        inc     $05B9
        bne     wily_machine_a_move_timer_dec
        lda     ent_var1,x
        and     #$01
        bne     wily_machine_a_inc_y_alt
        inc     $057D
        inc     $0579
        inc     $057B
wily_machine_a_inc_y_alt:  dec     $03DB
        lda     ent_var1,x
        cmp     #$20
        bne     wily_machine_a_move_timer_dec
        dec     $05BD
        dec     $05B9
        jmp     wily_machine_a_move_timer_dec

        inc     $03DB
wily_machine_a_move_timer_dec:  dec     ent_var1,x
        bne     wily_machine_a_bullet_fire
        lda     #$40
        sta     ent_var1,x
        inc     ent_timer,x
        lda     ent_timer,x
        and     #$07
        sta     ent_timer,x
        beq     wily_machine_a_check_dir
        cmp     #$04
        bne     wily_machine_a_bullet_fire
        lda     $6A
        sec
        sbc     #$40
        sta     L0000
        lda     $6B
        sbc     #$01
        ora     L0000
        beq     wily_machine_a_input_check
        lda     #$00
        sta     ent_timer,x
        beq     wily_machine_a_input_check
wily_machine_a_check_dir:  lda     $6A
        sec
        sbc     #$C0
        sta     L0000
        lda     $6B
        sbc     #$00
        ora     L0000
        beq     wily_machine_a_input_check
        lda     #$04
        sta     ent_timer,x
wily_machine_a_input_check:  lda     $031E
        bmi     wily_machine_a_bullet_fire
        lda     ent_timer,x
        sta     $053E
        lda     #$08
        sta     ent_timer,x
wily_machine_a_bullet_fire:  lda     $031E
        bpl     wily_machine_a_bullet_spawn
        lda     $059E
        and     #$04
        bne     wily_machine_a_fire_return
        lda     ent_var2,x
        bne     wily_machine_a_dec_var2
        jsr     entity_x_dist_to_player
        cmp     #$48
        bcc     wily_machine_a_fire_return
        jsr     find_enemy_freeslot_y
        bcs     wily_machine_a_fire_return
        lda     #$B4
        sta     ent_var2,x
        lda     #$61
        jsr     init_child_entity
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     camera_screen
        sta     ent_x_scr,y
        lda     #$A8
        sta     ent_y_px,y
        lda     #$90
        sta     ent_flags,y
        lda     #$E4
        sta     ent_routine,y
        lda     #$80
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_timer,y
        sta     ent_xvel_sub,y
        sta     $02
        lda     #$01
        sta     ent_xvel,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     ent_facing,y
        lda     #$02
        sta     ent_var1,y
        sta     ent_var2,y
wily_machine_a_dec_var2:  dec     ent_var2,x
wily_machine_a_fire_return:  rts

wily_machine_a_bullet_spawn:  lda     ent_var2,x
        bne     wily_machine_a_dec_var2
        ldy     #$16
wily_machine_a_find_slot_loop:  lda     ent_status,y
        bpl     wily_machine_a_bullet_init
        dey
        cpy     #$0F
        bne     wily_machine_a_find_slot_loop
        rts

wily_machine_a_bullet_init:  lda     #$58
        jsr     init_child_entity
        lda     #$00
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$E6
        sta     ent_routine,y
        lda     #$80
        sta     ent_hitbox,y
        lda     ent_y_px,x
        clc
        adc     #$0C
        sta     ent_y_px,y
        lda     camera_screen
        sta     ent_x_scr,y
        sty     $0F
        ldy     $051E
        lda     wily_machine_b_param_table,y
        sta     ent_var2,x
        lda     wily_machine_b_bullet_param,y
        clc
        adc     ent_x_px,x
        ldy     $0F
        sta     ent_x_px,y
        ldx     $0F
        jsr     face_player
        jsr     entity_x_dist_to_player
        sta     $01
        lda     #$00
        sta     L0000
        sta     $02
        lda     #$24
        sta     $03
        jsr     divide_16bit
        ldy     $0F
        lda     $04
        sta     ent_xvel_sub,x
        lda     $05
        sta     ent_xvel,x
        ldx     #$1F
        inc     $051E
        lda     $051E
        cmp     #$06
        bne     wily_machine_a_death_return
        lda     #$00
        sta     $051E
wily_machine_a_death_return:  rts

        ldy     #$08
wily_machine_a_flip_loop:  lda     $0597,y
        ora     #$04
        sta     $0597,y
        lda     $0577,y
        sec
        sbc     $6A
        sta     L0000
        lda     #$01
        sbc     $6B
        bne     wily_machine_a_dey_loop
        lda     L0000
        sta     $0377,y
        lda     $0597,y
        and     #$FB
        sta     $0597,y
wily_machine_a_dey_loop:  dey
        bpl     wily_machine_a_flip_loop
        lda     $95
        and     #$01
        tay
        lda     $059D
        and     #$04
        bne     wily_machine_a_palette_update
        lda     wily_machine_b_flip_table,y
        sta     $059D
        lda     wily_machine_b_flip_alt,y
        sta     $0599
wily_machine_a_palette_update:  lda     $059C
        and     #$04
        bne     wily_machine_a_palette_check
        lda     wily_machine_b_flip_table,y
        ora     #$40
        sta     $059C
        lda     wily_machine_b_flip_alt,y
        ora     #$40
        sta     $0598
wily_machine_a_palette_check:  lda     $031E
        bpl     wily_machine_a_palette_end
        lda     $059F
        ora     #$04
        sta     $059F
wily_machine_a_palette_end:  lda     #$00
        sta     $05FD
        sta     $05FC
        sta     $05F9
        sta     $05F8
        lda     $057F
        sec
        sbc     $6A
        lda     #$01
        sbc     $6B
        bne     wily_machine_a_death_palette
        lda     $03DF
        pha
        clc
        adc     #$18
        sta     $03DF
        lda     #$18
        sta     $049F
        lda     $059F
        pha
        and     #$F0
        sta     $059F
        jsr     L8009
        pla
        sta     $059F
        pla
        sta     $03DF
wily_machine_a_death_palette:  lda     $059F
        and     #$04
        bne     wily_machine_a_death_end
        lda     #$02
        sta     $049F
        jsr     L8003
        lda     ent_hp,x
        bne     wily_machine_a_death_end
        lda     #$6D
        sta     $05D0
        lda     #$00
        sta     $05B0
        sta     $05F0
        ldy     #$0B
        lda     #$0F
wily_machine_a_death_pal_loop:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     wily_machine_a_death_pal_loop
        sty     palette_dirty
wily_machine_a_death_end:  rts

wily_machine_b_dispatch:  lda     $B3
        bpl     wily_machine_a_death_end
        lda     player_state
        cmp     #PSTATE_BOSS_WAIT
        beq     wily_machine_a_death_end
        lda     ent_hp,x
        beq     wily_machine_b_fire_delay
        jsr     L8003
        lda     ent_hp,x
        ora     #$80
        sta     boss_hp_display
        and     #$1F
        bne     wily_machine_b_fire_return
        lda     #$E5
        sta     ent_routine,x
        lda     #$94
        sta     ent_flags,x
        lda     #$00
        sta     ent_timer,x
        sta     ent_anim_id,x
        ldy     #$00
wily_machine_b_nt_loop:  lda     wily_machine_b_nt_data,y
        sta     $0780,y
        cmp     #$FF
        beq     wily_machine_b_nt_end
        iny
        bne     wily_machine_b_nt_loop
wily_machine_b_nt_end:  sta     nametable_dirty
        jmp     wily_machine_b_spawn_pellet

wily_machine_b_fire_delay:  lda     ent_timer,x
        and     #$03
        bne     wily_machine_b_fire_timer_inc
        lda     #SFX_HP_FILL
        jsr     submit_sound_ID
        inc     boss_hp_display
        lda     boss_hp_display
        cmp     #$9C
        bne     wily_machine_b_fire_timer_inc
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $031E
        sta     $051E
        sta     $053E
        sta     $055E
        lda     $031F
        ora     #$40
        sta     $031F
wily_machine_b_fire_timer_inc:  inc     ent_timer,x
wily_machine_b_fire_return:  rts

wily_machine_b_spawn_pellet:  lda     #$02
        sta     $01
        lda     ent_x_px,x
        sta     $02
        lda     ent_y_px,x
        sta     $03
wily_machine_b_spawn_loop:  jsr     find_enemy_freeslot_y
        bcs     wily_machine_b_spawn_end
        lda     #$59
        jsr     init_child_entity
        lda     #$00
        sta     ent_timer,y
        sta     ent_hitbox,y
        lda     #$19
        sta     ent_routine,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        stx     $0F
        ldx     $01
        lda     $02
        clc
        adc     wily_machine_b_param2,x
        sta     ent_x_px,y
        lda     $03
        clc
        adc     wily_machine_b_param3,x
        sta     ent_y_px,y
        ldx     $0F
        dec     $01
        bpl     wily_machine_b_spawn_loop
wily_machine_b_spawn_end:  rts

wily_machine_b_move_dir:  lda     ent_facing,x
        and     #$01
        beq     wily_machine_b_move_left
        jsr     move_sprite_right
        jmp     wily_machine_b_move_vert

wily_machine_b_move_left:  jsr     move_sprite_left
wily_machine_b_move_vert:  lda     ent_facing,x
        and     #$08
        beq     wily_machine_b_move_down
        jsr     move_sprite_up
        jmp     wily_machine_b_calc_vel

wily_machine_b_move_down:  jsr     move_sprite_down
wily_machine_b_calc_vel:  ldy     ent_timer,x
        lda     #$00
        sta     L0000
        sta     $01
        lda     wily_machine_b_xvel_sub_table,y
        bpl     wily_machine_b_vel_apply
        dec     L0000
wily_machine_b_vel_apply:  lda     ent_y_sub,x
        clc
        adc     wily_machine_b_yvel_table,y
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     wily_machine_b_xvel_sub_table,y
        sta     ent_y_px,x
        lda     ent_y_scr,x
        adc     L0000
        beq     wily_machine_b_x_vel_apply
        lda     #$00
        sta     ent_status,x
        rts

wily_machine_b_x_vel_apply:  lda     wily_machine_b_xvel_sign_table,y
        bpl     wily_machine_b_x_sub_vel
        dec     $01
wily_machine_b_x_sub_vel:  lda     ent_x_sub,x
        clc
        adc     wily_machine_b_xvel_table,y
        sta     ent_x_sub,x
        lda     ent_x_px,x
        adc     wily_machine_b_xvel_sign_table,y
        sta     ent_x_px,x
        lda     ent_x_scr,x
        adc     $01
        sta     ent_x_scr,x
        dec     ent_var1,x
        bne     wily_machine_b_move_return
        inc     ent_timer,x
        lda     ent_timer,x
        and     #$0F
        sta     ent_timer,x
        bne     wily_machine_b_collision
        inc     ent_var2,x
wily_machine_b_collision:  lda     ent_var2,x
        sta     ent_var1,x
wily_machine_b_move_return:  rts

wily_machine_b_attack_vert:  jsr     apply_y_speed
        lda     ent_facing,x
        and     #$01
        beq     wily_machine_b_move_left_atk
        jmp     move_sprite_right

wily_machine_b_move_left_atk:  jmp     move_sprite_left

wily_machine_b_addr_a:  .byte   $D3,$1D
wily_machine_b_addr_b:  .byte   $A7,$A8
wily_machine_b_routine_ptr:  .byte   $A3,$C6,$05,$28,$2E,$51,$8F,$B2
        .byte   $2D,$6C
wily_machine_b_addr_c:  .byte   $A8,$A8,$A9,$A9,$A9,$A9,$A9,$A9
        .byte   $A8,$A8
wily_machine_b_nt_data:  .byte   $25,$0A,$0B,$80,$80,$80,$80,$E6
        .byte   $E7,$E8,$E9,$80,$80,$80,$80,$25
        .byte   $2A,$0B,$80,$80,$80,$EA,$EB,$80
        .byte   $80,$80,$EC,$80,$80,$80,$25,$4A
        .byte   $0B,$80,$80,$80,$ED,$EE,$80,$80
        .byte   $EF,$F0,$80,$80,$80,$25,$6A,$0B
        .byte   $80,$F1,$F2,$9A,$F3,$F4,$F5,$F6
        .byte   $9F,$F7,$F8,$80,$FF
wily_machine_b_flip_table:  .byte   $90,$94
wily_machine_b_flip_alt:  .byte   $94,$90
wily_machine_b_yvel_table:  .byte   $00,$3B,$E1,$DB,$00,$25,$1F,$C5
        .byte   $00,$C5,$1F,$25,$00,$DB,$E1,$3B
wily_machine_b_xvel_sub_table:  .byte   $FD,$FD,$FD,$FE,$00,$01,$02,$02
        .byte   $03,$02,$02,$01,$00,$FE,$FD,$FD
wily_machine_b_xvel_table:  .byte   $00,$25,$1F,$C5,$00,$C5,$1F,$25
        .byte   $00,$DB,$E1,$3B,$00,$3B,$E1,$DB
wily_machine_b_xvel_sign_table:  .byte   $00,$01,$02,$02,$03,$02,$02,$01
        .byte   $00,$FE,$FD,$FD,$FD,$FD,$FD,$FE
wily_machine_b_param_table:  .byte   $32,$3C,$32,$3C,$32,$5A
wily_machine_b_bullet_param:  .byte   $E0,$20,$E0,$20,$E0,$20
wily_machine_b_param2:  .byte   $00,$F0,$10
wily_machine_b_param3:  .byte   $C0,$D0,$D0

; Gamma init phase — same boss_wait pattern as all other bosses
gamma_b_init:  lda     ent_status,x             ; AI phase
        and     #$0F
        bne     gamma_b_phase_handler               ; skip init if not phase 0
        sta     $95
        inc     ent_status,x                 ; advance to phase 1
        lda     #PSTATE_BOSS_WAIT                    ; state → $09 (boss_wait)
        sta     player_state                     ; freeze player for HP fill
        lda     #$80                    ; init boss HP display
        sta     boss_hp_display
        sta     boss_active                     ; boss active flag
        lda     #$8E                    ; HP fill target (28 HP)
        sta     $B3
        lda     #MUSIC_BOSS                    ; SFX $0D = boss intro music
        jsr     submit_sound_ID_D9
        lda     #$30
        sta     ent_timer,x
        lda     #$6C
        sta     $E8
        lda     #$6E
        sta     $E9
        jsr     update_CHR_banks
        lda     #$00
        sta     $69
        sta     $6A
        sta     $6B
gamma_b_phase_handler:  lda     ent_timer,x
        bmi     gamma_b_hp_bar_check
        lda     #$00
        lda     $95
        and     #$0F
        bne     gamma_b_setup_hp
        ldy     #$0B
gamma_b_pal_fill_hi_loop:  lda     gamma_b_pal_hi_table,y
        sec
        sbc     ent_timer,x
        bcs     gamma_b_pal_hi_store
        lda     #$0F
gamma_b_pal_hi_store:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     gamma_b_pal_fill_hi_loop
        ldy     #$07
gamma_b_pal_fill_lo_loop:  lda     gamma_b_pal_lo_table,y
        sec
        sbc     ent_timer,x
        bcs     gamma_b_pal_lo_store
        lda     #$0F
gamma_b_pal_lo_store:  sta     $0618,y
        sta     $0638,y
        dey
        bpl     gamma_b_pal_fill_lo_loop
        sty     palette_dirty
        lda     ent_timer,x
        sec
        sbc     #$10
        sta     ent_timer,x
gamma_b_setup_hp:  lda     #$80
        sta     boss_hp_display

; Wait for boss HP bar to fill, then release player
gamma_b_hp_bar_check:  lda     boss_hp_display                 ; HP bar position
        cmp     #$9C                    ; filled to max?
        bne     gamma_b_init_end               ; no → keep filling
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; release player, fight begins
        sta     ent_timer,x
        lda     #$C0
        sta     ent_status,x
        lda     #$E8
        sta     ent_routine,x
        lda     #$1C
        sta     ent_hp,x
gamma_b_init_end:  lda     #$00
        sta     ent_anim_frame,x
        rts

gamma_b_pal_hi_table:  .byte   $0F,$30,$16,$04,$0F,$30,$11,$01
        .byte   $0F,$30,$36,$26
gamma_b_pal_lo_table:  .byte   $0F,$01,$30,$11,$0F,$0F,$30,$10
gamma_b_main_update:  lda     ent_timer,x
        bne     gamma_b_timer_dec
        jsr     entity_x_dist_to_player
        cmp     #$50
        bcs     gamma_b_var2_check
        jsr     face_player
        jsr     gamma_b_spawn_bullet
        lda     #$1F
        sta     ent_timer,x
gamma_b_timer_dec:  dec     ent_timer,x
gamma_b_var2_check:  lda     ent_var2,x
        bne     gamma_b_var1_loop
        jsr     entity_y_dist_to_player
        cmp     #$30
        bcs     gamma_b_hp_check
gamma_b_var1_loop:  lda     ent_var1,x
        bne     gamma_b_var1_dec
        lda     #$02
        sta     $01
        jsr     gamma_b_spawn_homing_loop
        lda     #$1F
        sta     ent_var1,x
        inc     ent_var2,x
        lda     ent_var2,x
        cmp     #$03
        bcc     gamma_b_var1_dec
        lda     #$79
        sta     ent_var1,x
        lda     #$00
        sta     ent_var2,x
gamma_b_var1_dec:  dec     ent_var1,x
gamma_b_hp_check:  lda     ent_hp,x
        cmp     #$0F
        bcc     gamma_b_spawn_mine
        rts

gamma_b_spawn_mine:  ldy     #$17
gamma_b_spawn_loop:  cpy     #$10
        bcs     gamma_b_spawn_type2
        lda     #$7A
        bne     gamma_b_spawn_init
gamma_b_spawn_type2:  lda     #$5B
gamma_b_spawn_init:  jsr     init_child_entity
        lda     #$90
        sta     ent_flags,y
        lda     #$00
        sta     ent_hitbox,y
        lda     #$10
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     $D7E9,y
        sta     ent_xvel_sub,y
        lda     $D7F9,y
        sta     ent_xvel,y
        lda     $D809,y
        sta     ent_yvel_sub,y
        lda     $D819,y
        sta     ent_yvel,y
        dey
        cpy     #$07
        bne     gamma_b_spawn_loop

; Gamma phase transition — scroll screen vertically
        lda     #PSTATE_SCREEN_SCROLL                    ; state → $10 (screen_scroll)
        sta     player_state                     ; player frozen during scroll
        lda     #$C0
        sta     ent_status,x
        lda     #$00
        sta     $69
        sta     $6A
        sta     ent_xvel_sub,x
        sta     ent_xvel,x
        lda     #$B4
        sta     ent_timer,x
        lda     #$F0
        sta     ent_var1,x
        lda     #$02
        sta     ent_var2,x
        lda     #$E9
        sta     ent_routine,x
        lda     ent_flags,x
        ora     #$04
        sta     ent_flags,x
        lda     ent_hitbox,x
        and     #$BF
        sta     ent_hitbox,x
        lda     #$6B
        jmp     reset_sprite_anim

gamma_b_spawn_homing_loop:  jsr     find_enemy_freeslot_y
        bcs     gamma_b_spawn_end
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     gamma_b_param_table,y
        pha
        lda     ent_x_scr,x
        adc     gamma_b_sprite_id_table,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$80
        sta     ent_xvel_sub,y
        lda     #$01
        sta     ent_xvel,y
        lda     #$58
        jsr     init_child_entity
        lda     #$51
        sta     ent_routine,y
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_hp,y
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        dec     $01
        bne     gamma_b_spawn_homing_loop
gamma_b_spawn_end:  rts

gamma_b_param_table:  .byte   $0F
gamma_b_sprite_id_table:  .byte   $00,$F1,$FF
gamma_b_spawn_bullet:  jsr     find_enemy_freeslot_y
        bcs     gamma_b_bullet_return
        lda     #$00
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$58
        jsr     init_child_entity
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$0C
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sec
        sbc     #$10
        sta     ent_y_px,y
        lda     ent_y_scr,x
        sta     ent_y_scr,y
        lda     ent_facing,x
        sta     ent_facing,y
        jsr     entity_x_dist_to_player
        stx     L0000
        ldx     #$03
gamma_b_vel_select_loop:  cmp     gamma_b_vel_threshold_table,x
        bcc     gamma_b_vel_apply
        dex
        bne     gamma_b_vel_select_loop
gamma_b_vel_apply:  lda     gamma_b_xvel_sub_table,x
        sta     ent_xvel_sub,y
        lda     gamma_b_xvel_table,x
        sta     ent_xvel,y
        ldx     L0000
gamma_b_bullet_return:  rts

gamma_b_vel_threshold_table:  .byte   $4C,$3D,$2E,$1F
gamma_b_xvel_sub_table:  .byte   $00,$80,$00,$80
gamma_b_xvel_table:  .byte   $02,$01,$01,$00
gamma_f_main:  lda     ent_timer,x
        bne     gamma_f_timer_dec
        jsr     find_enemy_freeslot_y
        bcs     gamma_f_var1_check
        lda     #$77
        jsr     init_child_entity
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        clc
        adc     #$38
        sta     ent_y_px,y
        lda     #$80
        sta     ent_hitbox,y
        lda     #$50
        sta     ent_routine,y
        lda     #$00
        sta     ent_xvel_sub,y
        sta     $02
        lda     #$04
        sta     ent_xvel,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     ent_facing,y
        lda     #$B5
        sta     ent_timer,x
gamma_f_timer_dec:  dec     ent_timer,x
gamma_f_var1_check:  lda     ent_var1,x
        bne     gamma_f_var1_dec
        lda     ent_var2,x
        and     #$01
        bne     gamma_f_scroll_alt
        lda     $69
        clc
        adc     ent_xvel_sub,x
        sta     $69
        lda     $6A
        adc     ent_xvel,x
        sta     $6A
        cmp     #$A0
        bcs     gamma_f_set_scroll
        lda     ent_xvel_sub,x
        clc
        adc     #$10
        sta     ent_xvel_sub,x
        lda     ent_xvel,x
        adc     #$00
        sta     ent_xvel,x
        cmp     #$03
        bne     gamma_f_collision_update
        lda     #$00
        sta     ent_xvel_sub,x
        beq     gamma_f_collision_update
gamma_f_set_scroll:  lda     #$A0
        sta     $6A
        lda     #$80
        sta     ent_xvel_sub,x
        lda     #$01
        sta     ent_xvel,x
        lda     #$01
        sta     ent_var2,x
        bne     gamma_f_collision_update
gamma_f_scroll_alt:  lda     $69
        sec
        sbc     ent_xvel_sub,x
        sta     $69
        lda     $6A
        sbc     ent_xvel,x
        sta     $6A
        bcs     gamma_f_collision_update
        lda     #$00
        sta     $69
        sta     $6A
        sta     ent_xvel_sub,x
        sta     ent_xvel,x
        lda     #$F1
        sta     ent_var1,x
        lda     #$02
        sta     ent_var2,x
gamma_f_var1_dec:  dec     ent_var1,x
gamma_f_collision_update:  jsr     L8003
        lda     ent_hp,x
        bne     gamma_f_return
        sta     $6A
        sta     $6B
        ldy     #$0B
        lda     #$0F
gamma_f_death_pal_loop:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     gamma_f_death_pal_loop
        sty     palette_dirty
        ldy     #$00
gamma_f_death_nt_loop:  lda     gamma_f_nt_data,y
        sta     $0780,y
        cmp     #$FF
        beq     gamma_f_death_end
        iny
        bne     gamma_f_death_nt_loop
gamma_f_death_end:  sta     nametable_dirty
        lda     #$80
        sta     $0310
        lda     #$90
        sta     $0590
        lda     #$6D
        sta     $05D0
        lda     #$00
        sta     $05F0
        sta     $05B0
        sta     $0490
        lda     #$EF
        sta     $0330
        lda     #$A3
        sta     $0450
        lda     #$04
        sta     $0470
        lda     #$00
        sta     $6A
        sta     $6B
gamma_f_return:  rts

gamma_f_nt_data:  .byte   $23,$C0,$0F,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$FF
gamma_f_collision_check:  lda     ent_flags,x
        and     #$04
        bne     gamma_f_collision_flag
        lda     player_state
        cmp     #PSTATE_DAMAGE
        beq     gamma_f_collision_flag
        cmp     #PSTATE_DEATH
        beq     gamma_f_collision_flag
        lda     ent_y_px
        pha
        inc     ent_y_px
        jsr     check_player_collision
        bcs     gamma_f_player_collision
        lda     $041F
        sta     $37
        lda     $043F
        sta     $38
        lda     $055F
        sta     $36
gamma_f_player_collision:  pla
        sta     ent_y_px

; Gamma/fortress hazard — instant kill on contact
        lda     ent_spawn_id,x                 ; entity sub-type
        cmp     #$0D                    ; sub-type $0D = Gamma fist (skip instant kill)
        beq     gamma_f_collision_flag
        lda     invincibility_timer                     ; i-frames active?
        bne     gamma_f_collision_flag               ; skip
        jsr     check_player_collision                   ; AABB collision test
        bcs     gamma_f_collision_flag               ; no collision → skip
        lda     #PSTATE_DEATH                    ; state → $0E (death)
        sta     player_state                     ; instant kill, no damage calc
gamma_f_collision_flag:  lda     ent_flags,x
        .byte   $09
gamma_f_collision_byte:  .byte   $04
        sta     ent_flags,x
        lda     gamma_f_collision_byte,x
        sec
        sbc     $6A
        bcs     gamma_f_collision_end
        sta     ent_x_px,x
        lda     ent_flags,x
        and     #$FB
        sta     ent_flags,x
gamma_f_collision_end:  rts

        .byte   $80,$40,$10

; ---------------------------------------------------------------------------
; Teleporter tube — player steps in, warps to boss refight room
; Sets state $11 (warp_init), which transitions to $12 (warp_anim),
; then back to $00 (on_ground) in the destination room.
; $6C = destination index (from entity sub-type).
; ---------------------------------------------------------------------------
teleporter_collision:  jsr     check_player_collision               ; is player touching teleporter?
        bcs     teleporter_return               ; no → return
        jsr     entity_x_dist_to_player                   ; detailed alignment check
        cmp     #$02                    ; close enough to center?
        bcs     teleporter_return               ; no → return
        lda     ent_x_px,x                 ; snap player X to teleporter X
        sta     ent_x_px
        lda     ent_spawn_id,x                 ; entity sub-type - $0E =
        sbc     #$0E                    ; destination index
        cmp     #$01
        beq     teleporter_return               ; destination 1 = invalid? skip
        sta     warp_dest                     ; $6C = warp destination
        lda     #PSTATE_WARP_INIT                    ; state → $11 (warp_init)
        sta     player_state                     ; begin teleporter sequence
        lda     #$13                    ; player OAM $13 = teleport beam
        sta     ent_anim_id
        lda     #$00
        sta     ent_anim_frame                   ; reset animation counter
        sta     ent_anim_state                   ; reset animation frame
        sta     ent_status,x                 ; despawn teleporter entity
teleporter_return:  rts

teleporter_pos_check_y:  lda     ent_y_px,x
        cmp     #$68
        beq     teleporter_pos_check_x
        inc     ent_x_px,x
        lda     #$01
        jmp     teleporter_set_facing

teleporter_pos_check_x:  dec     ent_x_px,x
        lda     #$02
teleporter_set_facing:  sta     ent_facing,x
        lda     ent_y_px,x
        pha
        dec     ent_y_px,x
        jsr     check_player_collision
        pla
        sta     ent_y_px,x
        bcs     teleporter_door_return
        lda     ent_facing,x
        sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
teleporter_door_return:  rts

teleporter_activate_check:  lda     ent_y_px,x
        cmp     #$A8
        beq     teleporter_anim_check
        clc
        adc     #$04
        sta     ent_y_px,x
        cmp     #$A8
        bne     teleporter_anim_end
        lda     #$6C
        cmp     ent_anim_id,x
        beq     teleporter_anim_check
        jsr     reset_sprite_anim
        lda     #$10
        sta     ent_timer,x
teleporter_anim_check:  lda     ent_anim_id,x
        cmp     #$6E
        bne     teleporter_anim_end
        lda     ent_timer,x
        beq     teleporter_anim_end
        dec     ent_timer,x
        lda     #$00
        sta     ent_anim_frame,x
teleporter_anim_end:  rts

teleporter_fall:  ldy     #$08
        jsr     move_vertical_gravity
        bcs     teleporter_fall_check
        inc     ent_x_px,x
        rts

teleporter_fall_check:  lda     #$6C
        cmp     ent_anim_id,x
        beq     teleporter_fall_end
        jsr     reset_sprite_anim
teleporter_fall_end:  rts

teleporter_fall_rts:  jsr     apply_y_speed
        lda     #$B0
        cmp     ent_y_px,x
        bcs     teleporter_fall_end
        sta     ent_y_px,x
        lda     ent_timer,x
        cmp     #$02
        beq     wily_machine_c_clear_reg
        bcs     wily_machine_c_set_flags
        lda     #$00
        sta     ent_status,x
wily_machine_c_var1_init:  lda     #$03
        sta     L0000
wily_machine_c_spawn_loop:  jsr     find_enemy_freeslot_y
        bcs     teleporter_fall_end
        lda     #$78
        jsr     init_child_entity
        lda     #$FA
        sta     ent_routine,y
        lda     #$00
        sta     ent_hitbox,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        sta     ent_xvel_sub,y
        lda     #$44
        sta     ent_yvel_sub,y
        lda     #$03
        sta     ent_yvel,y
        stx     $01
        lda     ent_x_px,x
        sta     $02
        ldx     L0000
        lda     wily_machine_c_y_pos_table,x
        sta     ent_y_px,y
        lda     $02
        clc
        adc     wily_machine_c_xvel_table,x
        sta     ent_x_px,y
        lda     wily_machine_c_facing_table,x
        sta     ent_facing,y
        lda     wily_machine_c_flags_table,x
        sta     ent_xvel,y
        ldx     $01
        dec     L0000
        bpl     wily_machine_c_spawn_loop
        rts

wily_machine_c_clear_reg:  lda     #$00
        sta     $0310
        rts

wily_machine_c_set_flags:  lda     ent_flags
        ora     #$04
        sta     ent_flags
        lda     ent_var1,x
        cmp     #$3C
        bne     wily_machine_c_main
        lda     #$79
        jsr     reset_sprite_anim
        stx     ent_var3
        lda     #$00
        sta     ent_routine,x
        lda     #$B4
        sta     ent_y_px,x
        jmp     wily_machine_c_var1_init

wily_machine_c_main:  inc     ent_var1,x
        bne     wily_machine_c_return
        jsr     find_enemy_freeslot_y
        bcs     wily_machine_c_return
        lda     #$7D
        jsr     init_child_entity
        lda     #$00
        sta     ent_y_px,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        sta     ent_timer,y
        lda     #$00
        sta     ent_hitbox,y
        sta     ent_yvel_sub,y
        sta     ent_yvel,y
        sta     ent_xvel_sub,y
        sta     ent_xvel,y
        lda     #$FB
        sta     ent_routine,y
        lda     #$01
        sta     ent_facing,y
        ldy     #$07
wily_machine_c_pal_loop:  lda     wily_machine_c_pal_table,y
        sta     $0610,y
        sta     $0630,y
        dey
        bpl     wily_machine_c_pal_loop
        sty     palette_dirty
wily_machine_c_return:  rts

wily_machine_c_y_pos_table:  .byte   $A8,$A8,$B8,$B8
wily_machine_c_xvel_table:  .byte   $F8,$08,$F8,$08
wily_machine_c_facing_table:  .byte   $02,$01,$02,$01
wily_machine_c_flags_table:  .byte   $02,$02,$01,$01
wily_machine_c_block_fall:  jsr     apply_y_speed
        lda     ent_y_px,x
        cmp     #$B8
        bcc     wily_machine_c_block_move
        lda     #$00
        sta     ent_status,x
        rts

wily_machine_c_block_move:  lda     ent_facing,x
        and     #$01
        beq     wily_machine_c_block_left
        jmp     move_sprite_right

wily_machine_c_block_left:  jmp     move_sprite_left

wily_machine_c_block_y_update:  jsr     apply_y_speed
        lda     ent_facing,x
        and     #$02
        beq     wily_machine_c_block_y_check
        lda     #$B4
        cmp     ent_y_px,x
        bcs     wily_machine_c_block_facing
        lda     #$00
        sta     ent_status,x
        lda     #$81
        sta     ent_status
        lda     #$00
        ldy     ent_var3
        sta     ent_status,y
        sta     ent_timer                   ; clear player timer
        lda     #PSTATE_TELEPORT                    ; state → $0D (teleport)
        sta     player_state                     ; player teleports away (end stage)
        lda     ent_flags
        and     #$FB
        sta     ent_flags
        rts

wily_machine_c_block_y_check:  lda     #$94
        cmp     ent_y_px,x
        bcs     wily_machine_c_block_anim_end
        sta     ent_y_px,x
        lda     ent_anim_id,x
        cmp     #$7A
        beq     wily_machine_c_block_timer
        cmp     #$7B
        bne     wily_machine_c_block_anim
        lda     #$7A
        jsr     reset_sprite_anim
        lda     #$00
        sta     $B8
wily_machine_c_block_anim:  lda     ent_anim_state,x
        cmp     #$04
        bne     wily_machine_c_block_end
        lda     #$7B
        jsr     reset_sprite_anim
        lda     #$A3
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        lda     #$9B
        sta     ent_xvel_sub,x
        lda     #$02
        sta     ent_xvel,x
wily_machine_c_block_anim_end:  lda     #$00
        sta     ent_anim_frame,x
wily_machine_c_block_facing:  lda     ent_facing,x
        and     #$01
        beq     wily_machine_c_block_left_move
        jmp     move_sprite_right

wily_machine_c_block_left_move:  jmp     move_sprite_left

wily_machine_c_block_timer:  inc     ent_timer,x
        lda     ent_timer,x
        cmp     #$3C
        bne     wily_machine_c_block_end
        lda     $B8
        bne     wily_machine_c_block_pal
        dec     ent_timer,x
        lda     #$11
        sta     game_mode
        lda     #$D0
        sta     $5E
        lda     #$0A
        sta     $B8
        jmp     call_bank0E_A006

wily_machine_c_block_pal:  jsr     call_bank0E_A003
        lda     $B8
        cmp     #$FF
        beq     wily_machine_c_block_jump
        dec     ent_timer,x
        rts

wily_machine_c_block_jump:  lda     #$A3
        sta     ent_yvel_sub,x
        lda     #$04
        sta     ent_yvel,x
        lda     #$7B
        jsr     reset_sprite_anim
        lda     #$02
        sta     ent_facing,x
wily_machine_c_block_end:  rts

wily_machine_c_pal_table:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
kamegoro_maker_init:  lda     ent_hitbox,x
        pha
        lda     #$00
        sta     ent_hitbox,x
        jsr     L8003
        pla
        sta     ent_hitbox,x
        bcs     kamegoro_maker_mode_check
        lda     ent_hp,x
        bne     kamegoro_maker_mode_check
        sta     game_mode
        sta     ent_hitbox,x
        lda     #$02
        sta     $01
        lda     ent_x_px,x
        sta     $02
        lda     ent_y_px,x
        sta     $03
kamegoro_maker_spawn_loop:  jsr     find_enemy_freeslot_y
        bcs     kamegoro_maker_spawn_end
        lda     #$71
        jsr     init_child_entity
        lda     #$00
        sta     ent_timer,y
        sta     ent_hitbox,y
        lda     #$19
        sta     ent_routine,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        stx     $0F
        ldx     $01
        lda     $02
        clc
        adc     kamegoro_maker_param_e,x
        sta     ent_x_px,y
        lda     $03
        clc
        adc     kamegoro_maker_param_f,x
        sta     ent_y_px,y
        ldx     $0F
        dec     $01
        bpl     kamegoro_maker_spawn_loop
        ldy     #$0F
kamegoro_maker_pal_loop:  lda     kamegoro_maker_pal_table,y
        sta     $0600,y
        dey
        bpl     kamegoro_maker_pal_loop
        sty     palette_dirty
kamegoro_maker_spawn_end:  rts

kamegoro_maker_mode_check:  lda     game_mode
        cmp     #$03
        bcc     kamegoro_maker_spawn_end
        lda     ent_var1,x
        beq     kamegoro_maker_phase_check
        dec     ent_var1,x
        bne     kamegoro_maker_jump
        lda     ent_var2,x
        beq     kamegoro_maker_jump
        dec     ent_var2,x
        beq     kamegoro_maker_jump
        lda     #$1E
        sta     ent_var1,x
        jmp     kamegoro_maker_spawn_main

kamegoro_maker_jump:  jmp     kamegoro_maker_gravity

kamegoro_maker_phase_check:  lda     ent_status,x
        and     #$0F
        bne     kamegoro_maker_timer_start
        inc     ent_status,x
        sta     ent_yvel,x
        lda     #$80
        sta     ent_yvel_sub,x
        lda     #$88
        sta     ent_facing,x
        lda     #$F0
        sta     ent_timer,x
kamegoro_maker_timer_start:  lda     ent_timer,x
        bne     kamegoro_maker_movement
        lda     ent_facing,x
        eor     #$0C
        and     #$0C
        sta     ent_facing,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     kamegoro_maker_timer_table,y
        sta     ent_timer,x
        lda     #$00
        sta     camera_x_lo
kamegoro_maker_movement:  lda     ent_facing,x
        and     #$04
        bne     kamegoro_maker_move_down
        jsr     move_sprite_up
        lda     #$48
        cmp     ent_y_px,x
        bcc     kamegoro_maker_timer_dec
        sta     ent_y_px,x
        bcs     kamegoro_maker_facing
kamegoro_maker_move_down:  jsr     move_sprite_down
        lda     #$80
        cmp     ent_y_px,x
        bcs     kamegoro_maker_timer_dec
        sta     ent_y_px,x
kamegoro_maker_facing:  lda     ent_facing,x
        eor     #$0C
        sta     ent_facing,x
kamegoro_maker_timer_dec:  dec     ent_timer,x
        bne     kamegoro_maker_gravity
        lda     #$1E
        sta     ent_var1,x
        lda     $E4
        adc     $E6
        sta     $E4
        and     #$01
        beq     kamegoro_maker_var2_init
        jsr     kamegoro_maker_spawn_pellet
        jmp     kamegoro_maker_gravity

kamegoro_maker_var2_init:  lda     #$03
        sta     ent_var2,x
        jsr     kamegoro_maker_spawn_main
kamegoro_maker_gravity:  lda     ent_y_px,x
        sec
        sbc     #$D0
        clc
        adc     #$AF
        sta     $5E
        rts

kamegoro_maker_spawn_pellet:  lda     #$02
        sta     $01
kamegoro_maker_pellet_loop:  jsr     find_enemy_freeslot_y
        bcs     kamegoro_maker_pellet_end
        lda     #$6F
        jsr     init_child_entity
        lda     #$80
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_hp,y
        lda     #$0F
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        clc
        adc     #$30
        sta     ent_y_px,y
        lda     #$02
        sta     ent_facing,y
        stx     L0000
        ldx     $01
        lda     kamegoro_maker_param_a,x
        sta     ent_xvel_sub,y
        lda     kamegoro_maker_param_b,x
        sta     ent_xvel,y
        lda     kamegoro_maker_param_c,x
        sta     ent_yvel_sub,y
        lda     kamegoro_maker_param_d,x
        sta     ent_yvel,y
        ldx     L0000
        dec     $01
        bpl     kamegoro_maker_pellet_loop
kamegoro_maker_pellet_end:  rts

kamegoro_maker_spawn_main:  jsr     find_enemy_freeslot_y
        bcs     kamegoro_maker_pellet_end
        lda     #$1D
        jsr     init_child_entity
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$6D
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        clc
        adc     #$30
        sta     ent_y_px,y
        lda     #$AB
        sta     ent_yvel_sub,y
        lda     #$FF
        sta     ent_yvel,y
        lda     #$00
        sta     ent_xvel_sub,y
        lda     #$02
        sta     ent_xvel,y
        lda     #$01
        sta     ent_hp,y
        rts

kamegoro_maker_timer_table:  .byte   $1E,$3C,$3C,$5A
kamegoro_maker_param_a:  .byte   $B5,$00,$B5
kamegoro_maker_param_b:  .byte   $00,$01,$00
kamegoro_maker_param_c:  .byte   $4B,$00,$B5
kamegoro_maker_param_d:  .byte   $FF,$00,$00
kamegoro_maker_param_e:  .byte   $00,$F0,$10
kamegoro_maker_param_f:  .byte   $00,$10,$10
kamegoro_maker_pal_table:  .byte   $0F,$20,$27,$17,$0F,$03,$12,$0F
        .byte   $0F,$2B,$1B,$0B,$0F,$22,$12,$02
kamegoro_current_init:  lda     ent_status,x
        and     #$0F
        bne     kamegoro_current_phase_check
        lda     #$09
        cmp     player_state
        beq     kamegoro_current_hp_check
        sta     player_state
        lda     #$80
        sta     boss_hp_display
        sta     boss_active
        lda     #$8E
        sta     $B3
        lda     #MUSIC_BOSS
        jsr     submit_sound_ID_D9
kamegoro_current_hp_check:  lda     boss_hp_display
        cmp     #$9C
        bne     kamegoro_current_return
        jsr     kamegoro_current_death_counter
        inc     ent_status,x
kamegoro_current_phase_check:  lda     ent_status,x
        and     #$02
        bne     kamegoro_current_return
        lda     ent_anim_id,x
        cmp     #$4F
        beq     kamegoro_current_anim_check
        lda     ent_var1,x
        bne     kamegoro_current_death_init
        lda     ent_var2,x
        cmp     #$05
        bcs     kamegoro_current_status_inc
        lda     ent_timer,x
        bne     kamegoro_current_movement
        lda     #$4F
        jsr     reset_sprite_anim
kamegoro_current_anim_check:  lda     ent_anim_state,x
        cmp     #$02
        bne     kamegoro_current_return
        jsr     kamegoro_current_spawn_entity
        inc     ent_var2,x
        lda     #$31
        jsr     reset_sprite_anim
        inc     ent_var1,x
kamegoro_current_movement:  lda     ent_facing,x
        and     #$01
        beq     kamegoro_current_move_left_col
        ldy     #$20
        jsr     move_right_collide
        jmp     kamegoro_current_move_check

kamegoro_current_move_left_col:  ldy     #$21
        jsr     move_left_collide
kamegoro_current_move_check:  bcc     kamegoro_current_timer_dec
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
kamegoro_current_timer_dec:  dec     ent_timer,x
        rts

kamegoro_current_status_inc:  inc     ent_status,x
kamegoro_current_return:  rts

kamegoro_current_death_init:  lda     #$00
        sta     L0000
        lda     #$80
        sta     $01
        ldy     #$1F
kamegoro_current_death_loop:  lda     ent_status,y
        bmi     kamegoro_current_death_filter
kamegoro_current_death_next:  dey
        cpy     #$0F
        bne     kamegoro_current_death_loop
        lda     L0000
        bne     kamegoro_current_death_anim
        lda     #$00
        sta     ent_var1,x
        lda     ent_var2,x
        tay
        lda     boss_hp_display
        sec
        sbc     kamegoro_current_timer_table,y
        sta     boss_hp_display
        and     #$1F
        bne     kamegoro_current_death_anim
        jmp     L8006

kamegoro_current_death_anim:  lda     #$31
        jsr     reset_sprite_anim
        jsr     kamegoro_current_death_counter
        rts

kamegoro_current_death_filter:  lda     $01
        cmp     ent_spawn_id,y
        bne     kamegoro_current_death_next
        inc     L0000
        jmp     kamegoro_current_death_next

kamegoro_current_death_counter:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     kamegoro_current_pos_table,y
        sta     ent_timer,x
        lda     kamegoro_current_spawn_table,y
        sta     ent_facing,x
        rts

kamegoro_current_pos_table:  .byte   $40,$A0,$70,$D0
kamegoro_current_spawn_table:  .byte   $01,$02,$01,$02
kamegoro_current_timer_table:  .byte   $01,$02,$03,$05,$08,$0A
kamegoro_current_spawn_entity:  jsr     find_enemy_freeslot_y
        bcs     kamegoro_current_spawn_return
        sty     L0000
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        clc
        adc     #$18
        sta     ent_y_px,y
        lda     #$C2
        sta     ent_hitbox,y
        lda     #$F1
        sta     ent_routine,y
        lda     #$03
        sta     ent_hp,y
        lda     #$80
        sta     ent_spawn_id,y
        lda     ent_var2,x
        sta     ent_var2,y
        sta     $02
        tay
        lda     kamegoro_current_spawn_timer_a,y
        ldy     L0000
        sta     ent_facing,y
        ldy     $02
        lda     kamegoro_current_spawn_facing,y
        ldy     L0000
        sta     ent_xvel_sub,y
        sta     ent_yvel_sub,y
        ldy     $02
        lda     kamegoro_current_spawn_timer_b,y
        ldy     L0000
        sta     ent_xvel,y
        sta     ent_yvel,y
        lda     #$5E
        jsr     init_child_entity
        lda     ent_facing,y
        and     #$01
        bne     kamegoro_current_spawn_return
        lda     ent_flags,y
        and     #$BF
        sta     ent_flags,y
kamegoro_current_spawn_return:  rts

kamegoro_current_spawn_timer_a:  .byte   $06,$05,$05,$06,$05,$06
kamegoro_current_spawn_facing:  .byte   $80,$00,$80,$00,$00,$00
kamegoro_current_spawn_timer_b:  .byte   $00,$01,$01,$02,$03,$04
kamegoro_current_phase_init:  lda     ent_status,x
        and     #$0F
        bne     kamegoro_current_phase_status
        sta     ent_var1,x
        lda     #$78
        sta     ent_timer,x
        inc     ent_status,x
kamegoro_current_phase_status:  lda     ent_status,x
        and     #$02
        beq     kamegoro_current_move_routine
        jmp     kamegoro_current_launched

kamegoro_current_move_routine:  jsr     kamegoro_current_var1_spawn
        lda     ent_timer,x
        bne     kamegoro_current_timer_cmp
        lda     ent_y_px,x
        cmp     #$68
        bcs     kamegoro_current_spawn_effect
        lda     #$78
        sta     ent_timer,x
        bne     kamegoro_current_timer_dec_2
kamegoro_current_spawn_effect:  jsr     kamegoro_current_effect_spawn_2
        jsr     kamegoro_current_face_dir
        lda     #$FF
        sta     ent_timer,x
kamegoro_current_timer_cmp:  cmp     #$FF
        beq     kamegoro_current_move_dir
kamegoro_current_timer_dec_2:  dec     ent_timer,x
kamegoro_current_move_dir:  lda     ent_facing,x
        and     #$01
        beq     kamegoro_current_move_left
        ldy     #$0C
        jsr     move_right_collide
        jmp     kamegoro_current_collision

kamegoro_current_move_left:  ldy     #$0D
        jsr     move_left_collide
kamegoro_current_collision:  bcc     kamegoro_current_vert_check
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        and     #$0C
        bne     kamegoro_current_return_end
        lda     ent_facing,x
        ora     #$08
        sta     ent_facing,x
        rts

kamegoro_current_vert_check:  lda     ent_facing,x
        and     #$0C
        beq     kamegoro_current_return_end
        and     #$04
        beq     kamegoro_current_facing_check
        lda     ent_timer,x
        cmp     #$FF
        beq     kamegoro_current_anim_type2
        lda     #$5E
        bne     kamegoro_current_anim_set
kamegoro_current_anim_type2:  lda     #$62
kamegoro_current_anim_set:  sta     ent_anim_id,x
        ldy     #$0E
        jsr     move_down_collide
        jmp     kamegoro_current_col_return

kamegoro_current_facing_check:  lda     ent_facing,x
        and     #$01
        beq     kamegoro_current_flag_clear
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        bne     kamegoro_current_flag_check
kamegoro_current_flag_clear:  lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
kamegoro_current_flag_check:  lda     ent_timer,x
        cmp     #$FF
        beq     kamegoro_current_anim_up
        lda     #$60
        bne     kamegoro_current_anim_down
kamegoro_current_anim_up:  lda     #$64
kamegoro_current_anim_down:  sta     ent_anim_id,x
        ldy     #$0F
        jsr     move_up_collide
kamegoro_current_col_return:  bcc     kamegoro_current_return_end
        lda     ent_facing,x
        eor     #$0C
        sta     ent_facing,x
kamegoro_current_return_end:  rts

kamegoro_current_launched:  lda     ent_var3,x
        bne     kamegoro_current_gravity
        lda     ent_var2,x
        tay
        lda     kamegoro_current_spawn_id_table,y
        sta     ent_yvel_sub,x
        lda     kamegoro_current_spawn_type,y
        sta     ent_yvel,x
        lda     kamegoro_current_facing_table,y
        sta     ent_xvel_sub,x
        lda     kamegoro_current_speed_table,y
        sta     ent_xvel,x
        inc     ent_var3,x
kamegoro_current_gravity:  ldy     #$0F
        jsr     move_vertical_gravity
        lda     $10
        and     #$10
        beq     kamegoro_current_move_dir_2
        jmp     kamegoro_current_status_dec

kamegoro_current_move_dir_2:  lda     ent_facing,x
        and     #$01
        beq     kamegoro_current_move_left_2
        ldy     #$0C
        jsr     move_right_collide
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        jmp     kamegoro_current_col_end

kamegoro_current_move_left_2:  ldy     #$0D
        jsr     move_left_collide
        lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
kamegoro_current_col_end:  bcc     kamegoro_current_vel_check
kamegoro_current_status_dec:  dec     ent_status,x
        lda     #$00
        sta     ent_var1,x
        sta     ent_var3,x
        lda     ent_facing,x
        eor     #$0C
        sta     ent_facing,x
        lda     ent_var2,x
        tay
        lda     kamegoro_current_spawn_facing,y
        sta     ent_yvel_sub,x
        sta     ent_xvel_sub,x
        lda     kamegoro_current_spawn_timer_b,y
        sta     ent_yvel,x
        sta     ent_xvel,x
        rts

kamegoro_current_vel_check:  lda     ent_yvel,x
        bpl     kamegoro_current_return_final
        lda     ent_y_px,x
        cmp     #$20
        bcs     kamegoro_current_status_dec
kamegoro_current_return_final:  rts

kamegoro_current_spawn_id_table:  .byte   $A2,$4F,$B4,$44,$00,$9E
kamegoro_current_spawn_type:  .byte   $01,$02,$02,$03,$04,$04
kamegoro_current_facing_table:  .byte   $00,$80,$00,$80,$00,$80
kamegoro_current_speed_table:  .byte   $01,$01,$02,$02,$03,$03
kamegoro_current_var1_spawn:  lda     ent_var1,x
        bne     kamegoro_current_y_check
        lda     ent_y_px,x
        cmp     #$45
        bcc     kamegoro_current_spawn_ret_a
        jsr     kamegoro_current_effect_spawn
        lda     ent_var2,x
        tay
        lda     kamegoro_current_spawn_facing,y
        sta     ent_yvel_sub,x
        sta     ent_xvel_sub,x
        lda     kamegoro_current_spawn_timer_b,y
        sta     ent_yvel,x
        sta     ent_xvel,x
        inc     ent_var1,x
kamegoro_current_spawn_ret_a:  rts

kamegoro_current_y_check:  lda     ent_y_px,x
        cmp     #$45
        bcs     kamegoro_current_spawn_ret_a
        jsr     kamegoro_current_effect_spawn
        lda     #$00
        sta     ent_var1,x
        sta     ent_var3,x
        inc     ent_status,x
        rts

kamegoro_current_face_dir:  lda     ent_facing,x
        and     #$0C
        beq     kamegoro_current_horiz_check
        lda     #$62
        sta     ent_anim_id,x
        rts

kamegoro_current_horiz_check:  lda     ent_facing,x
        and     #$01
        beq     kamegoro_current_facing_right
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        bne     kamegoro_current_anim_id_set
kamegoro_current_facing_right:  lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
kamegoro_current_anim_id_set:  lda     #$64
        sta     ent_anim_id,x
        rts

kamegoro_current_effect_spawn:  jsr     find_enemy_freeslot_y
        bcs     kamegoro_current_effect_return
        sty     L0000
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_y_px,x
        sec
        sbc     #$0C
        sta     ent_y_px,y
        lda     #$00
        sta     ent_routine,y
        sta     ent_hitbox,y
        sta     ent_hp,y
        lda     #$67
        jsr     init_child_entity
kamegoro_current_effect_return:  rts

        .byte   $00,$00,$00,$00
kamegoro_current_effect_spawn_2:  jsr     find_enemy_freeslot_y
        bcs     kamegoro_current_effect_ret_2
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$F2
        sta     ent_routine,y
        lda     #$CA
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
        lda     #$66
        jsr     init_child_entity
        lda     ent_facing,y
        and     #$01
        bne     kamegoro_current_effect_ret_2
        lda     ent_flags,y
        and     #$BF
        sta     ent_flags,y
kamegoro_current_effect_ret_2:  rts

holograph_block_init:  lda     ent_status,x
        and     #$0F
        bne     holograph_block_collision
        sta     ent_yvel,x
        sta     ent_var2,x
        lda     #$80
        sta     ent_yvel_sub,x
        lda     #$32
        sta     ent_timer,x
        lda     #$F0
        sta     ent_var1,x
        inc     ent_status,x
holograph_block_collision:  jsr     check_sprite_weapon_collision
        bcs     holograph_block_damage
        lda     #SFX_ENEMY_HIT
        jsr     submit_sound_ID
        ldy     $10
        lda     #$00
        sta     ent_status,y
        jmp     holograph_block_death

holograph_block_damage:  lda     ent_status,x
        and     #$02
        bne     holograph_block_facing
        jsr     move_sprite_up
        dec     ent_timer,x
        bne     holograph_block_return
        lda     #$02
        sta     ent_timer,x
        inc     ent_status,x
holograph_block_return:  rts

holograph_block_facing:  lda     ent_facing,x
        and     #$01
        bne     holograph_block_move_down
        jsr     move_sprite_up
        jmp     holograph_block_timer_dec

holograph_block_move_down:  jsr     move_sprite_down
holograph_block_timer_dec:  dec     ent_timer,x
        bne     holograph_block_var1_dec
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$04
        sta     ent_timer,x
holograph_block_var1_dec:  dec     ent_var1,x
        bne     holograph_block_var2_check
        lda     #$90
        sta     ent_flags,x
holograph_block_death:  lda     #$59
        jsr     reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$19
        sta     ent_routine,x
        rts

holograph_block_var2_check:  lda     ent_var2,x
        bne     holograph_block_var1_check
        lda     ent_var1,x
        cmp     #$02
        bcs     holograph_block_repeat_return
        lda     #$F2
        sta     ent_var1,x
        inc     ent_var2,x
holograph_block_repeat_return:  rts

holograph_block_var1_check:  lda     ent_var1,x
        cmp     #$78
        bcs     holograph_block_repeat_return
        lda     ent_flags,x
        eor     #$04
        sta     ent_flags,x
        rts

holograph_main_init:  lda     ent_status,x
        and     #$0F
        bne     holograph_phase_check
        jsr     holograph_timer_init
        lda     #$3C
        sta     ent_var3,x
        inc     ent_status,x
holograph_phase_check:  lda     ent_status,x
        and     #$02
        bne     holograph_timer_dec
        dec     ent_var3,x
        bne     holograph_return
        inc     ent_status,x
holograph_timer_dec:  dec     ent_timer,x
        bne     holograph_var1_check
        jsr     holograph_spawn_entity
        jsr     holograph_timer_init
        lda     #$94
        sta     ent_flags,x
        dec     ent_var1,x
        rts

holograph_var1_check:  lda     ent_var1,x
        bne     holograph_return
        lda     ent_timer,x
        cmp     #$3C
        bcs     holograph_return
        lda     #$90
        sta     ent_flags,x
        inc     ent_var1,x
holograph_return:  rts

holograph_timer_init:  lda     #$78
        sta     ent_timer,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        cmp     #$02
        bcs     holograph_death_y_reset
        tay
        lda     holograph_spawn_param,y
        sta     ent_x_px,x
        lda     holograph_spawn_type,y
        sta     ent_facing,x
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$0F
        tay
        lda     holograph_y_pos_table,y
        sta     ent_y_px,x
        rts

holograph_death_y_reset:  lda     #$CC
        sta     ent_y_px,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$0F
        tay
        lda     holograph_y_pos_alt_table,y
        sta     ent_x_px,x
        lda     #$08
        sta     ent_facing,x
        rts

holograph_spawn_param:  .byte   $14,$EC
holograph_spawn_type:  .byte   $01,$02
holograph_y_pos_table:  .byte   $48,$58,$68,$78,$88,$98,$A8,$B8
        .byte   $88,$B8,$A8,$98,$88,$78,$68,$58
holograph_y_pos_alt_table:  .byte   $28,$38,$48,$58,$68,$78,$88,$98
        .byte   $A8,$B8,$C8,$D8,$B8,$A8,$98,$88
holograph_spawn_entity:  jsr     find_enemy_freeslot_y
        bcs     holograph_spawn_return
        sty     L0000
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     #$00
        sta     ent_hp,y
        sta     ent_xvel_sub,y
        sta     ent_yvel_sub,y
        lda     #$20
        sta     ent_hitbox,y
        lda     ent_facing,x
        and     #$08
        bne     holograph_spawn_facing
        lda     #$5D
        jsr     init_child_entity
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x
        clc
        adc     holograph_spawn_param_2,y
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        jmp     holograph_spawn_routine

holograph_spawn_facing:  lda     ent_facing,x
        sta     ent_facing,y
        lda     #$5C
        jsr     init_child_entity
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_y_px,x
        sec
        sbc     #$18
        sta     ent_y_px,y
holograph_spawn_routine:  lda     #$F4
        sta     ent_routine,y
        lda     ent_flags,y
        ora     #$02
        sta     ent_flags,y
        lda     #$02
        sta     ent_xvel,y
        sta     ent_yvel,y
holograph_spawn_return:  rts

holograph_spawn_param_2:  .byte   $E8,$18
holograph_current_dir_init:  lda     ent_facing,x
        and     #$08
        beq     holograph_current_horiz
        ldy     #$09
        jsr     move_up_collide
        lda     ent_y_px,x
        cmp     #$48
        bcs     holograph_current_dist_check
        jmp     holograph_current_hit_check

holograph_current_horiz:  lda     ent_facing,x
        and     #$01
        beq     holograph_current_move_left
        ldy     #$08
        jsr     move_right_collide
        jmp     holograph_current_collision

holograph_current_move_left:  ldy     #$09
        jsr     move_left_collide
holograph_current_collision:  bcc     holograph_current_dist_check
holograph_current_hit_check:  lda     ent_facing,x
        and     #$08
        beq     holograph_current_status_clear
holograph_current_status_clear:  lda     #$00
        sta     ent_status,x
        lda     #$FF
        sta     ent_spawn_id,x
        rts

holograph_current_dist_check:  jsr     entity_x_dist_to_player
        cmp     #$18
        bcs     holograph_current_dir_check
        jsr     entity_y_dist_to_player
        cmp     #$14
        bcs     holograph_current_dir_check
        lda     ent_facing,x
        and     #$08
        beq     holograph_current_facing_dir
        rts

holograph_current_facing_dir:  lda     ent_facing,x
        and     #$02
        bne     holograph_current_facing_right
        lda     #$01
        bne     holograph_current_dir_set
holograph_current_facing_right:  lda     #$02
holograph_current_dir_set:  sta     $36
        lda     #$00
        sta     $37
        lda     #$02
        sta     $38
holograph_current_dir_check:  lda     ent_facing,x
        and     #$08
        beq     holograph_current_return
holograph_current_return:  rts

holograph_boss_init:  lda     ent_status,x
        and     #$0F
        bne     holograph_boss_phase_check
        lda     #$09
        cmp     player_state
        beq     holograph_boss_hp_check
        sta     player_state
        lda     #$80
        sta     boss_hp_display
        lda     #$8E
        sta     $B3
        lda     #MUSIC_BOSS
        jsr     submit_sound_ID_D9
holograph_boss_hp_check:  lda     boss_hp_display
        cmp     #$9C
        bne     holograph_boss_return
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        lda     #$00
        sta     $01
        sta     $02
        jsr     holograph_tentacle_spawn
        lda     #$3C
        sta     ent_timer,x
        lda     #$36
        sta     ent_var1,x
        lda     #$01
        sta     ent_facing,x
        inc     ent_status,x
holograph_boss_phase_check:  lda     ent_status,x
        and     #$0F
        cmp     #$02
        beq     holograph_boss_facing_check
        cmp     #$03
        bne     holograph_boss_flag_check
        jmp     holograph_boss_attack_pattern

holograph_boss_flag_check:  lda     ent_flags,x
        and     #$04
        beq     holograph_boss_anim_check
        dec     ent_timer,x
        bne     holograph_boss_return
        lda     ent_flags,x
        eor     #$04
        sta     ent_flags,x
holograph_boss_anim_check:  lda     ent_anim_state,x
        cmp     #$04
        bne     holograph_boss_return
        lda     #$04
        jsr     reset_sprite_anim
        lda     #$3C
        sta     ent_timer,x
        inc     ent_status,x
holograph_boss_return:  rts

holograph_boss_facing_check:  lda     ent_facing,x
        and     #$01
        beq     holograph_boss_flag_left
        lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
        jsr     move_sprite_right
        jmp     holograph_boss_var1_dec

holograph_boss_flag_left:  lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        jsr     move_sprite_left
holograph_boss_var1_dec:  dec     ent_var1,x
        bne     holograph_boss_var2_check
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$6C
        sta     ent_var1,x
        inc     ent_var2,x
holograph_boss_var2_check:  lda     ent_var2,x
        cmp     #$02
        bcc     holograph_boss_timer_dec
        lda     ent_var1,x
        cmp     #$36
        bcs     holograph_boss_timer_dec
        inc     ent_status,x
        lda     #$1E
        sta     ent_var3,x
        lda     #$A1
        sta     ent_hitbox,x
        lda     #$01
        jmp     reset_sprite_anim

holograph_boss_timer_dec:  dec     ent_timer,x
        bne     holograph_boss_anim_alt
        lda     #$05
        jsr     reset_sprite_anim
        jsr     holograph_tentacle_entity
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$03
        tay
        lda     holograph_boss_timer_table,y
        sta     ent_timer,x
holograph_boss_anim_alt:  lda     ent_anim_id,x
        cmp     #$04
        beq     holograph_boss_return_a
        lda     ent_anim_state,x
        beq     holograph_boss_return_a
        cmp     #$03
        bne     holograph_boss_return_a
        lda     #$04
        jsr     reset_sprite_anim
holograph_boss_return_a:  rts

holograph_boss_attack_pattern:  lda     ent_anim_id,x
        cmp     #$13
        beq     holograph_boss_flag_xor
        dec     ent_var3,x
        bne     holograph_boss_pattern_return
        lda     #$13
        jsr     reset_sprite_anim
        lda     #$3C
        sta     ent_var3,x
holograph_boss_pattern_return:  rts

holograph_boss_flag_xor:  lda     ent_flags,x
        and     #$04
        bne     holograph_boss_var3_dec
        lda     ent_anim_state,x
        cmp     #$04
        bne     holograph_boss_pattern_return
        jsr     holograph_tentacle_id_check
        lda     ent_flags,x
        eor     #$04
        sta     ent_flags,x
holograph_boss_var3_dec:  dec     ent_var3,x
        bne     holograph_boss_pattern_return
        lda     #$06
        sta     ent_timer,x
        lda     #$36
        sta     ent_var1,x
        lda     #$00
        sta     ent_var2,x
        sta     ent_var3,x
        lda     #$13
        jsr     reset_sprite_anim
        lda     #$94
        sta     ent_flags,x
        lda     #$80
        sta     ent_x_px,x
        lda     #$C1
        sta     ent_status,x
        rts

holograph_boss_timer_table:  .byte   $1E,$3C,$1E,$3C
holograph_tentacle_spawn:  jsr     find_enemy_freeslot_y
        bcs     holograph_tentacle_return
        sty     L0000
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        ldy     $01
        lda     holograph_tentacle_facing,y
        ldy     L0000
        sta     ent_facing,y
        lda     #$80
        sta     ent_x_px,y
        ldy     $01
        lda     holograph_tentacle_chr_table,y
        ldy     L0000
        sta     ent_y_px,y
        lda     #$01
        sta     ent_xvel,y
        lda     #$13
        jsr     init_child_entity
        lda     #$00
        sta     ent_var2,y
        sta     ent_var3,y
        sta     ent_xvel_sub,y
        lda     #$F5
        sta     ent_routine,y
        sta     ent_hp,y
        sta     ent_spawn_id,y
        lda     #$81
        sta     ent_status,y
        lda     #$3C
        sta     ent_timer,y
        lda     #$36
        sta     ent_var1,y
        lda     #$94
        sta     ent_flags,y
        lda     #$81
        sta     ent_hitbox,y
        inc     $01
        inc     $02
        lda     $02
        cmp     #$02
        bcc     holograph_tentacle_spawn
holograph_tentacle_return:  rts

holograph_tentacle_chr_table:  .byte   $74,$C4,$24,$74,$C4,$24,$C4,$24
        .byte   $C4,$74,$24,$74,$24,$C4,$24,$C4
holograph_tentacle_facing:  .byte   $02,$01,$01,$02,$01,$01,$01,$01
        .byte   $01,$02,$01,$02,$01,$01,$01,$01
holograph_tentacle_id_check:  lda     ent_spawn_id,x
        cmp     #$2C
        bne     holograph_tentacle_clear
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$07
        tay
        lda     holograph_tentacle_chr_alt,y
        sta     ent_y_px,x
        lda     holograph_tentacle_facing_alt,y
        sta     ent_facing,x
        lda     #$C1
        sta     ent_hitbox,x
        lda     holograph_tentacle_pos_offset,y
        sta     $01
        lda     #$00
        sta     $02
        jsr     holograph_tentacle_spawn
        rts

holograph_tentacle_clear:  lda     #$00
        sta     ent_status,x
        rts

holograph_tentacle_chr_alt:  .byte   $24,$C4,$74,$74,$24,$C4,$74,$74
holograph_tentacle_facing_alt:  .byte   $01,$01,$02,$02,$01,$01,$02,$02
holograph_tentacle_pos_offset:  .byte   $00,$02,$04,$06,$08,$0A,$0C,$0E
holograph_tentacle_entity:  jsr     find_enemy_freeslot_y
        bcs     holograph_tentacle_spawn_end
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$01
        tay
        lda     ent_x_px,x
        clc
        adc     holograph_tentacle_init_table,y
        ldy     L0000
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$19
        sta     ent_xvel_sub,y
        sta     $02
        lda     #$04
        sta     ent_xvel,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     ent_facing,y
        lda     #$73
        jsr     init_child_entity
        lda     #$8F
        sta     ent_routine,y
        lda     #$8B
        sta     ent_hitbox,y
holograph_tentacle_spawn_end:  rts

holograph_tentacle_init_table:  .byte   $EE,$12,$00,$00,$00,$20,$00,$00
        .byte   $10,$08,$10,$43,$04,$40,$00,$00
        .byte   $00,$84,$00,$82,$00,$40,$50,$00
        .byte   $10,$00,$00,$20,$00,$02,$00,$01
        .byte   $00,$80,$40,$02,$40,$00,$00,$04
        .byte   $00,$01,$00,$00,$00,$04,$00,$01
        .byte   $00,$40,$04,$80,$00,$80,$00,$02
        .byte   $00,$02,$00,$90,$00,$00,$00,$40
        .byte   $00,$11,$00,$02,$00,$88,$00,$90
        .byte   $00,$00,$00,$01,$40,$02,$00,$80
        .byte   $00,$13,$10,$40,$00,$04,$00,$26
        .byte   $00,$0C,$00,$01,$00,$14,$00,$02
        .byte   $01,$48,$04,$4D,$44,$30,$00,$90
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $00,$01,$00,$00,$00,$81,$00,$40
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$00,$04,$00,$00,$10,$00,$00
        .byte   $00,$01,$00,$03,$01,$02,$00,$21
        .byte   $00,$20,$40,$04,$00,$A0,$00,$01
        .byte   $05,$14,$00,$00,$40,$02,$11,$30
        .byte   $00,$00,$10,$00,$00,$00,$00,$04
        .byte   $01,$01,$00,$08,$04,$00,$40,$01
        .byte   $00,$42,$00,$10,$00,$84,$00,$40
        .byte   $00,$00,$10,$00,$00,$40,$04,$81
        .byte   $00,$00,$00,$00,$10,$00,$01,$00
        .byte   $00,$00,$10,$08,$00,$80,$01,$00
        .byte   $00,$00,$40,$40,$10,$80,$54,$10
        .byte   $00,$01,$04,$00,$00,$18,$00,$20
        .byte   $00,$08,$00,$00,$00,$88,$00,$00
        .byte   $04,$00,$00
