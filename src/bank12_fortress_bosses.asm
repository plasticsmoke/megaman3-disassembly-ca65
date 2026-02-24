main_yellow_devil:
; =============================================================================
; MEGA MAN 3 (U) — BANK $12 — FORTRESS BOSSES + SPECIAL ENTITIES
; =============================================================================
; Mapped to $A000-$BFFF. AI routines for Wily fortress bosses (Yellow Devil,
; Clone Mega Man, Wily Machine, Gamma) and special entities (breakable blocks,
; Wily capsule). Dispatched from bank1C_1D for routine indices $E0-$FF.
; Known bosses: main_yellow_devil, main_wily_machine_A, main_wily_machine_B,
; main_gamma_B, main_gamma_F.
; Also serves as stage data for stage $12 (special/ending) via stage_to_bank.
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"

; --- External references (fixed bank + $8000 entry points, bank $1C) ---
entity_ai_dispatch := $8003             ; entity AI dispatch (bank $1C)
entity_ai_defeat := $8006               ; entity AI defeat handler (bank $1C)
entity_check_player_hit := $8009        ; check player hit (bank $1C)
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

        jmp     yellow_devil_dispatch   ; $E0 cmd 0: Yellow Devil AI
                                        ; $E0 cmd 1:
        jmp     yellow_devil_piece_left_update ; piece horizontal update
                                        ; $E1 cmd 0:
        jmp     yellow_devil_piece_falling ; falling piece update
main_wily_machine_A:
                                        ; $E2 cmd 0:
        jmp     wily_machine_a_dispatch ; Wily Machine phase A AI
                                        ; $E2 cmd 1:
        jmp     wily_machine_b_move_dir ; Wily Machine B movement
main_wily_machine_B:
                                        ; $E3 cmd 0:
        jmp     wily_machine_b_dispatch ; Wily Machine phase B AI
                                        ; $E3 cmd 1:
        jmp     wily_machine_b_attack_vert ; vertical attack movement
main_gamma_B:
                                        ; $E4 cmd 0:
        jmp     gamma_b_init            ; Gamma phase B init
                                        ; $E4 cmd 1:
        jmp     gamma_b_main_update     ; Gamma phase B main loop
                                        ; $E4 cmd 2:
        jmp     gamma_f_main            ; Gamma fist main AI
main_gamma_F:
                                        ; $E5 cmd 0:
        jmp     gamma_f_collision_check ; Gamma fist collision
main_teleporter:
                                        ; $E6 cmd 0:
        jmp     teleporter_collision    ; teleporter collision check
                                        ; $E6 cmd 1:
        jmp     teleporter_pos_check_y  ; teleporter Y position check
main_wily_machine_C:
                                        ; $E7 cmd 0:
        nop                             ; Wily Machine C stub (unused)
        nop                             ; padding
        rts                             ; return immediately
                                        ; $E7 cmd 1:
        jmp     teleporter_activate_check ; teleporter activation
                                        ; $E7 cmd 2:
        jmp     teleporter_fall         ; teleporter fall handler
main_kamegoro_maker:
                                        ; $E8 cmd 0:
        jmp     kamegoro_current_init   ; Kamegoro Maker init
                                        ; $E8 cmd 1:
        jmp     kamegoro_current_phase_init ; Kamegoro current phase init
                                        ; $E8 cmd 2:
        jmp     holograph_block_init    ; holograph block init
main_kamegoro_current:
                                        ; $E9 cmd 0:
        jmp     holograph_main_init     ; holograph main init
                                        ; $E9 cmd 1:
        jmp     holograph_current_dir_init ; holograph direction init
main_holograph:
                                        ; $EA cmd 0:
        jmp     holograph_boss_init     ; holograph boss init
                                        ; $EA cmd 1:
        jmp     yellow_devil_noop       ; unused (noop)
                                        ; $EA cmd 2:
        jmp     yellow_devil_noop       ; unused (noop)
                                        ; $EA cmd 3:
        jmp     yellow_devil_noop       ; unused (noop)
                                        ; $EA cmd 4:
        jmp     teleporter_fall_rts     ; teleporter fall return
                                        ; $EA cmd 5:
        jmp     wily_machine_c_block_fall ; falling block AI
                                        ; $EA cmd 6:
        jmp     wily_machine_c_block_y_update ; block Y position update
main_giant_met:
                                        ; $EB cmd 0:
        jmp     kamegoro_maker_init     ; Giant Met / Kamegoro init

yellow_devil_noop:  rts

yellow_devil_dispatch:  lda     ent_status,x ; current status
        and     #$0F                    ; routine index (low nibble)
        tay                             ; use as table index
        lda     yellow_devil_routine_table,y ; routine addr low byte
        sta     temp_00                 ; store in indirect ptr low
        lda     yellow_devil_addr_table,y ; routine addr high byte
        sta     $01                     ; store in indirect ptr high
        jmp     (temp_00)               ; jump to AI routine

; Yellow Devil init — freeze player for boss intro

        lda     #$09                    ; state → $09 (boss_wait)
        cmp     player_state            ; already in boss_wait?
        beq     yellow_devil_hp_bar_filled ; skip to HP fill
        sta     player_state            ; freeze player
        lda     #$80                    ; init boss HP display
        sta     boss_hp_display         ; set boss HP bar start
        sta     boss_active             ; boss active flag
        lda     #$8E                    ; HP fill target (28 HP)
        sta     $B3                     ; store HP fill target
        lda     #MUSIC_BOSS             ; SFX $0D = boss intro music
        jsr     submit_sound_ID_D9      ; play boss music
yellow_devil_hp_bar_filled:  lda     boss_hp_display ; has HP bar filled to $9C?
        cmp     #HEALTH_FULL            ; HP bar fully filled?
        bne     yellow_devil_return     ; wait for HP bar to fill
        inc     ent_status,x            ; advance to spawn state
        lda     #FACING_LEFT            ; face left
        sta     ent_facing,x            ; set facing
        lda     #$FF                    ; long delay (255 frames)
        sta     ent_timer,x             ; set timer
        lda     #$01                    ; direction flag = right
        sta     $10                     ; store direction state
yellow_devil_wait_timer:  lda     ent_timer,x ; check timer
        beq     yellow_devil_spawn_piece ; timer expired → spawn
        dec     ent_timer,x             ; count down timer
yellow_devil_return:  rts

yellow_devil_spawn_piece:  jsr     find_enemy_freeslot_y ; get free entity slot
        stx     temp_00                 ; save parent slot index
        lda     ent_var1,x              ; piece index
        sta     ent_timer,y             ; pass to child as timer
        tax                             ; use piece index as X
        lda     yellow_devil_y_pos_table,x ; Y position from table
        sta     ent_y_px,y              ; set piece Y pos
        lda     $10                     ; direction state
        and     #$01                    ; test direction bit
        bne     yellow_devil_spawn_left ; odd = spawn from left
        lda     yellow_devil_x_pos_right_table,x ; X pos from right table
        sta     ent_x_px,y              ; set piece X pos
        lda     #$71                    ; OAM ID: piece flying left
        sta     $01                     ; store anim ID
        bne     yellow_devil_spawn_common ; always branches
yellow_devil_spawn_left:  lda     #$04  ; left edge X position
        sta     ent_x_px,y              ; set piece X pos
        lda     #$7E                    ; OAM ID: piece flying right
        sta     $01                     ; store anim ID
yellow_devil_spawn_common:  lda     #$80 ; active entity
        sta     ent_status,y            ; activate piece
        sta     ent_hitbox,y            ; contact damage enabled
        lda     #$90                    ; active + child flag
        sta     ent_flags,y             ; set flags
        lda     $01                     ; anim ID from $01
        sta     ent_anim_id,y           ; set OAM animation
        lda     $10                     ; direction state
        sta     ent_facing,y            ; copy facing to piece
        lda     #$00                    ; zero
        sta     ent_y_scr,y             ; clear Y screen
        sta     ent_anim_frame,y        ; reset anim frame
        sta     ent_anim_state,y        ; reset anim state
        sta     ent_xvel_sub,y          ; clear X sub-velocity
        lda     #$04                    ; 4 pixels/frame
        sta     ent_xvel,y              ; set X speed
        lda     camera_screen           ; current screen page
        sta     ent_x_scr,y             ; set piece screen
        lda     #$E1                    ; routine $E1 = piece update
        sta     ent_routine,y           ; set AI routine
        lda     $10                     ; direction state
        and     #$01                    ; test direction bit
        bne     yellow_devil_load_timer ; odd = spawning from left
        txa                             ; piece index to A
        clc                             ; prepare for addition
        adc     #$18                    ; offset +24 for left table
        tax                             ; index into left timers
yellow_devil_load_timer:  lda     yellow_devil_timer_table,x ; delay before next piece
        ldx     temp_00                 ; restore parent slot
        sta     ent_timer,x             ; set spawn delay
        inc     ent_var1,x              ; advance piece counter
        lda     ent_timer,x             ; check spawn delay
        bne     yellow_devil_spawn_complete ; nonzero = more pieces
        lda     ent_facing,x            ; current facing
        eor     #$03                    ; flip direction (1↔2)
        sta     ent_facing,x            ; update facing
        lda     #$82                    ; status → assembled state
        sta     ent_status,x            ; set status
        lda     #$78                    ; 120 frame delay
        sta     ent_timer,x             ; set timer
        lda     #$03                    ; 3 eye shots before break
        sta     ent_var1,x              ; set shot counter
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        sta     ent_anim_state,x        ; reset anim state
yellow_devil_spawn_complete:  rts

; Yellow Devil assembled form — body collision check + eye projectile firing
yellow_devil_body_attack:  lda     ent_hitbox,x ; save current hitbox
        pha                             ; push to stack
        lda     #$18                    ; temp hitbox for body
        sta     ent_hitbox,x            ; set body hitbox
        lda     ent_y_px,x              ; save Y position
        pha                             ; push to stack
        clc                             ; prepare for addition
        adc     #$20                    ; offset to body center
        sta     ent_y_px,x              ; set adjusted Y pos
        lda     ent_flags,x             ; save flags
        pha                             ; push to stack
        and     #$FB                    ; clear bit 2 (invisible)
        sta     ent_flags,x             ; make body visible
        jsr     entity_check_player_hit ; check player collision
        pla                             ; restore flags
        sta     ent_flags,x             ; restore flags
        pla                             ; restore Y position
        sta     ent_y_px,x              ; restore Y position
        pla                             ; restore hitbox
        sta     ent_hitbox,x            ; restore hitbox
        lda     ent_status,x            ; current status
        ora     #$40                    ; set invincible bit
        sta     ent_status,x            ; body takes no damage
        lda     ent_timer,x             ; check eye open timer
        bne     yellow_devil_piece_update ; nonzero = still waiting
        lda     ent_anim_state,x        ; check eye anim state
        cmp     #$02                    ; state 2 = eye fully open
        bne     yellow_devil_piece_collision ; not open yet
        jsr     find_enemy_freeslot_y   ; find slot for eye shot
        bcs     yellow_devil_piece_collision ; no free slots
        lda     #$58                    ; child entity ID $58
        jsr     init_child_entity       ; spawn eye projectile
        lda     ent_x_px,x              ; copy parent X to shot
        sta     ent_x_px,y              ; set shot X position
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; set shot X screen
        lda     ent_y_px,x              ; copy parent Y
        sta     ent_y_px,y              ; set shot Y position
        lda     #$80                    ; contact damage
        sta     ent_hitbox,y            ; set shot hitbox
        lda     #$8F                    ; routine $8F = projectile
        sta     ent_routine,y           ; set shot AI routine
        lda     #$00                    ; zero
        sta     ent_xvel_sub,y          ; clear shot X sub-vel
        sta     $02                     ; homing min X sub-vel = 0
        lda     #$04                    ; 4 pixels/frame speed
        sta     ent_xvel,y              ; set shot X velocity
        sta     $03                     ; homing max speed = 4
        sty     $0F                     ; save child slot
        stx     $0E                     ; save parent slot
        ldx     $0F                     ; child as current entity
        jsr     calc_homing_velocity    ; aim shot at player
        ldy     $0F                     ; restore child slot
        ldx     $0E                     ; restore parent slot
        lda     $0C                     ; homing facing result
        sta     ent_facing,y            ; set shot facing
        lda     #$15                    ; 21 frames between shots
        sta     ent_timer,x             ; set eye cooldown timer
        dec     ent_var1,x              ; decrement shots left
        bne     yellow_devil_piece_update ; more shots remaining
        lda     ent_facing,x            ; current facing
        and     #FACING_LEFT            ; isolate left bit
        tay                             ; 0 or 2 = table index
        lda     yellow_devil_status_table,y ; next state from table
        sta     ent_status,x            ; transition to reassemble
        lda     #$1E                    ; 30 frame delay
        sta     ent_timer,x             ; set delay timer
        lda     #$00                    ; zero
        sta     ent_var1,x              ; reset piece counter
        sta     ent_anim_frame,x        ; reset anim frame
        sta     ent_anim_state,x        ; reset anim state
        rts                             ; done, begin reassembly next frame

yellow_devil_piece_update:  dec     ent_timer,x ; count down timer
        lda     ent_anim_state,x        ; check anim state
        bne     yellow_devil_piece_collision ; nonzero = run collision
        sta     ent_anim_frame,x        ; keep frame at 0
yellow_devil_piece_collision:  jsr     entity_ai_dispatch ; process collision + anim
        lda     ent_hp,x                ; check if boss HP = 0
        bne     yellow_devil_death_end  ; HP > 0 → still alive
        lda     #$0F                    ; white palette value
        ldy     #$03                    ; 4 palette entries
yellow_devil_death_palette_loop:  sta     $0608,y ; flash BG palette row 2
        sta     $0628,y                 ; flash BG palette row 3
        dey                             ; next palette entry
        bpl     yellow_devil_death_palette_loop ; loop all 4 entries
        sty     palette_dirty           ; flag palette for update
yellow_devil_death_end:  rts

        lda     ent_timer,x             ; check reassembly timer
        bne     yellow_devil_piece_wait ; nonzero = still waiting
        lda     ent_flags,x             ; get flags
        ora     #$04                    ; set bit 2 (invisible)
        sta     ent_flags,x             ; hide sprite (body is BG)
        lda     #$38                    ; body center X = $38
        sta     ent_x_px,x              ; set X position
        lda     nametable_dirty         ; NT update pending?
        beq     yellow_devil_piece_reassemble ; free → write tiles
        inc     ent_timer,x             ; wait 1 more frame
        bne     yellow_devil_piece_wait ; always branches
yellow_devil_piece_reassemble:  ldy     ent_var1,x ; body column index
        lda     yellow_devil_chr_even2_table,y ; CHR tile ID (even row)
        sta     $0780                   ; NT update cmd 1 tile hi
        sta     $0785                   ; NT update cmd 2 tile hi
        lda     yellow_devil_chr_comp_table,y ; CHR tile ID (compressed)
        sta     $0781                   ; NT cmd 1 tile lo
        ora     #$20                    ; set high NT bit
        sta     $0786                   ; NT cmd 2 tile lo
        lda     #$01                    ; 1 byte per write
        sta     $0782                   ; NT cmd 1 length
        sta     $0787                   ; NT cmd 2 length
        lda     #$00                    ; zero
        sta     $0783                   ; NT cmd 1 attr byte 1
        sta     $0784                   ; NT cmd 1 attr byte 2
        sta     $0788                   ; NT cmd 2 attr byte 1
        sta     $0789                   ; NT cmd 2 attr byte 2
        lda     #$FF                    ; end-of-list sentinel
        sta     $078A                   ; terminate NT update
        sta     nametable_dirty         ; flag NT for update
yellow_devil_piece_wait:  lda     ent_anim_state,x ; check anim state
        bne     yellow_devil_piece_continue ; nonzero = keep animating
        sta     ent_anim_frame,x        ; hold at frame 0
yellow_devil_piece_continue:  lda     #$02 ; direction = both sides
        sta     $10                     ; store direction state
        jmp     yellow_devil_wait_timer ; reuse spawn timer loop

        lda     ent_timer,x             ; check flatten timer
        beq     yellow_devil_piece_flatten ; zero → start flatten
        dec     ent_timer,x             ; count down timer
        lda     ent_anim_state,x        ; check anim state
        bne     yellow_devil_piece_timer_dec ; nonzero = keep animating
        sta     ent_anim_frame,x        ; hold at frame 0
yellow_devil_piece_timer_dec:  rts

yellow_devil_piece_flatten:  lda     ent_flags,x ; get flags
        ora     #$04                    ; set bit 2 (invisible)
        sta     ent_flags,x             ; hide sprite
        stx     temp_00                 ; save parent slot
        ldy     ent_var1,x              ; body column index
        lda     yellow_devil_chr_seq_table,y ; CHR tile for column
        sta     $0780                   ; NT cmd 1 tile hi
        sta     $078D                   ; NT cmd 2 tile hi
        lda     yellow_devil_chr_offset_table,y ; CHR offset for column
        sta     $0781                   ; NT cmd 1 tile lo
        ora     #$01                    ; OR #$01 for second tile
        sta     $078E                   ; NT cmd 2 tile lo
        ldx     #$09                    ; 9 bytes to clear
        stx     $0782                   ; NT cmd 1 length
        stx     $078F                   ; NT cmd 2 length
        lda     #$00                    ; zero
yellow_devil_palette_clear_loop:  sta     $0783,x ; clear NT cmd 1 attrs
        sta     $0790,x                 ; clear NT cmd 2 attrs
        dex                             ; next attr byte index
        bpl     yellow_devil_palette_clear_loop ; loop all attr bytes
        stx     $079A                   ; end sentinel ($FF)
        stx     nt_column_dirty         ; flag column for update
        lda     #$98                    ; Y start = $98 (bottom)
        sta     $01                     ; body column Y position
        lda     yellow_devil_y_step_table,y ; X pos for this column
        sta     $02                     ; body column X position
        lda     #$00                    ; zero
        sta     $03                     ; spawn order counter
        sty     $04                     ; save column index
yellow_devil_body_spawn_loop:  jsr     find_enemy_freeslot_y ; find free entity slot
        bcs     yellow_devil_body_spawn_done ; no slots → done
        lda     #$71                    ; OAM ID: body piece
        sta     ent_anim_id,y           ; set animation ID
        lda     #$00                    ; zero
        sta     ent_anim_frame,y        ; reset anim frame
        sta     ent_anim_state,y        ; reset anim state
        sta     ent_y_scr,y             ; clear Y screen
        sta     ent_yvel_sub,y          ; clear Y sub-velocity
        lda     #$04                    ; fall speed = 4 px/frame
        sta     ent_yvel,y              ; set Y velocity
        lda     #$80                    ; active entity
        sta     ent_status,y            ; activate piece
        sta     ent_hitbox,y            ; contact damage
        lda     #$90                    ; active + child flag
        sta     ent_flags,y             ; set flags
        lda     #$E2                    ; routine $E2 = falling piece
        sta     ent_routine,y           ; set AI routine
        lda     $03                     ; spawn order index
        sta     ent_timer,y             ; stagger fall timing
        lda     camera_screen           ; current screen page
        sta     ent_x_scr,y             ; set piece X screen
        lda     $02                     ; column X position
        sta     ent_x_px,y              ; set piece X pos
        lda     $01                     ; column Y position
        sta     ent_y_px,y              ; set piece Y pos
        sec                             ; prepare for subtraction
        sbc     #$10                    ; move up 16 px (1 tile)
        sta     $01                     ; update Y cursor
        cmp     #$88                    ; reached eye row ($88)?
        bne     yellow_devil_body_pos_next ; not eye row → continue
        ldx     $04                     ; column index
        lda     yellow_devil_x_pos_table,x ; eye X pos for column
        sta     ent_var2,y              ; store target X in var2
        lda     yellow_devil_var1_pos_table,x ; body part ID for column
        sta     ent_var1,y              ; store in var1
yellow_devil_body_pos_next:  inc     $03 ; next spawn order
        cmp     #$48                    ; reached top ($48)?
        bne     yellow_devil_body_spawn_loop ; not done → spawn more
yellow_devil_body_spawn_done:  ldx     temp_00 ; restore parent slot
        lda     #$14                    ; 20 frame delay
        sta     ent_timer,x             ; set inter-column timer
        inc     ent_var1,x              ; next column
        lda     ent_var1,x              ; current column index
        cmp     #$05                    ; all 5 columns done?
        bne     yellow_devil_body_spawn_end ; not yet → keep spawning
        lda     ent_facing,x            ; current facing
        eor     #$03                    ; flip direction (1↔2)
        sta     ent_facing,x            ; swap facing
        lda     #$82                    ; status → eye attack state
        sta     ent_status,x            ; set status
        lda     #$C8                    ; body X position ($C8)
        sta     ent_x_px,x              ; set X position
        lda     #$F0                    ; 240 frame delay
        sta     ent_timer,x             ; set timer
        lda     #$03                    ; 3 eye shots
        sta     ent_var1,x              ; set shot counter
        lda     #$00                    ; zero
        sta     ent_anim_frame,x        ; reset anim frame
        sta     ent_anim_state,x        ; reset anim state
yellow_devil_body_spawn_end:  rts

yellow_devil_routine_table:  .byte   $6B,$95,$3E,$0D,$6B
yellow_devil_addr_table:  .byte   $A0,$A0,$A1,$A2,$A2
yellow_devil_piece_left_update:  lda     ent_anim_id,x ; current piece animation
        cmp     #$71                    ; assembling anim?
        bne     yellow_devil_piece_right_update ; no: go to right/general update
        lda     ent_anim_state,x        ; check animation progress
        cmp     #$02                    ; anim state 2 = finished
        bne     yellow_devil_piece_left_return ; not done yet
        lda     #$7E                    ; switch to visible piece anim
        jsr     reset_sprite_anim       ; set new OAM animation
yellow_devil_piece_left_return:  rts

yellow_devil_piece_right_update:  lda     ent_facing,x ; facing direction
        and     #FACING_LEFT            ; isolate left-facing bit
        tay                             ; Y = 0 or 2 (facing offset)
        lda     yellow_devil_facing_offset,y ; base table offset for direction
        clc                             ; prepare for addition
        adc     ent_timer,x             ; add piece index to offset
        tay                             ; Y = table index for this piece
        lda     ent_status,x            ; entity status
        and     #$0F                    ; isolate low nibble (sub-phase)
        bne     yellow_devil_piece_right_anim ; nonzero: skip to anim check
        lda     ent_facing,x            ; facing direction
        and     #FACING_RIGHT           ; check right-facing bit
        beq     yellow_devil_piece_right_left ; not right-facing: move left
        jsr     move_sprite_right       ; move piece rightward
        jmp     yellow_devil_piece_right_cont ; skip left branch

yellow_devil_piece_right_left:  jsr     move_sprite_left ; move piece leftward
yellow_devil_piece_right_cont:  lda     yellow_devil_x_pos_left_table,y ; target X for this piece
        cmp     ent_x_px,x              ; reached target X?
        bne     yellow_devil_piece_right_end ; no: done for this frame
        inc     ent_status,x            ; advance to NT update phase
        lda     #$7F                    ; piece-with-delay anim
        jsr     reset_sprite_anim       ; set new OAM animation
yellow_devil_piece_right_anim:  lda     ent_anim_state,x ; check animation progress
        cmp     #$03                    ; anim state 3 = complete
        bne     yellow_devil_piece_right_end ; not done: exit
        lda     nametable_dirty         ; PPU update pending?
        bne     yellow_devil_piece_right_nt ; yes: skip NT write
        lda     #$00                    ; deactivate piece sprite
        sta     ent_status,x            ; clear entity status
        lda     yellow_devil_chr_even_table,y ; PPU addr high byte (even row)
        sta     $0780                   ; NT buffer: addr high byte
        sta     $0785                   ; 2nd row: same high byte
        lda     yellow_devil_chr_odd_table,y ; PPU addr low byte (odd row)
        sta     $0781                   ; NT buffer: addr low byte
        ora     #$20                    ; offset +$20 for next NT row
        sta     $0786                   ; 2nd row: addr low byte
        lda     #$01                    ; 1 tile per row
        sta     $0782                   ; NT buffer: tile count
        sta     $0787                   ; 2nd row: tile count
        lda     yellow_devil_chr_index_table,y ; CHR tile index for this piece
        asl     a                       ; multiply by 4 (attr stride)
        asl     a                       ; *4 complete (4 bytes per entry)
        tay                             ; Y = attr table offset
        lda     yellow_devil_chr_attr_80,y ; top-left tile
        sta     $0783                   ; NT buffer: tile data
        lda     yellow_devil_chr_attr_81,y ; top-right tile
        sta     $0784                   ; NT buffer: tile data
        lda     yellow_devil_chr_attr_80_dup,y ; bottom-left tile
        sta     $0788                   ; NT buffer: tile data
        lda     yellow_devil_chr_attr_large,y ; bottom-right tile
        sta     $0789                   ; NT buffer: tile data
        lda     #$FF                    ; end-of-buffer sentinel
        sta     $078A                   ; NT buffer: terminator
        sta     nametable_dirty         ; signal PPU update needed
        lda     ent_timer,x             ; piece index
        cmp     #$17                    ; last piece (#$17 = 23)?
        bne     yellow_devil_piece_right_nt ; not last: skip final cleanup
        lda     $059F                   ; slot $1F flags
        and     #$FB                    ; clear bit 2 (piece active)
        sta     $059F                   ; update slot $1F flags
        lda     #$00                    ; clear slot $1F timer
        sta     $051F                   ; stop piece spawning
        lda     $04BF                   ; slot $1F spawn ID (facing)
        and     #FACING_RIGHT           ; isolate right-facing bit
        tay                             ; Y = 0 (right) or 1 (left)
        lda     yellow_devil_palette_indices,y ; palette for assembled form
        sta     $05DF                   ; set slot $1F OAM anim ID
yellow_devil_piece_right_nt:  lda     #$00 ; reset anim frame counter
        sta     ent_anim_frame,x        ; prevent anim auto-advance
yellow_devil_piece_right_end:  rts

yellow_devil_piece_falling:  lda     ent_anim_id,x ; current piece animation
        cmp     #$7E                    ; visible piece anim ($7E)?
        beq     yellow_devil_piece_fall_move ; yes: do falling movement
        cmp     #$7F                    ; delay anim ($7F)?
        beq     yellow_devil_piece_fall_delay ; yes: do NT update phase
        lda     ent_anim_state,x        ; still assembling — check anim
        cmp     #$02                    ; anim state 2 = complete
        bne     yellow_devil_piece_right_end ; not done yet: exit
        lda     #$7E                    ; switch to visible piece anim
        jmp     reset_sprite_anim       ; set OAM anim and return

yellow_devil_piece_fall_move:  jmp     yellow_devil_piece_fall_descent

yellow_devil_piece_fall_delay:  lda     #$3C ; 60-frame delay for parent
        sta     $051F                   ; set slot $1F timer
        lda     ent_anim_state,x        ; check animation progress
        cmp     #$03                    ; anim state 3 = complete
        bne     yellow_devil_piece_right_end ; not done: exit
        lda     nametable_dirty         ; PPU update pending?
        bne     yellow_devil_piece_fall_anim ; yes: hold anim frame
        ldy     ent_timer,x             ; piece index
        lda     yellow_devil_timer_seq_table,y ; NT tile sequence entry
        bmi     yellow_devil_piece_fall_death ; $FF = last piece: done
        pha                             ; save tile index for later
        lda     yellow_devil_nt_even_table,y ; PPU addr high byte (even)
        sta     $0780                   ; NT buffer: addr high
        sta     $0785                   ; 2nd row: same high byte
        lda     yellow_devil_nt_odd_table,y ; PPU addr low byte (odd)
        sta     $0781                   ; NT buffer: addr low
        ora     #$20                    ; offset +$20 for next NT row
        sta     $0786                   ; 2nd row: addr low byte
        lda     #$01                    ; 1 tile per row
        sta     $0782                   ; NT buffer: tile count
        sta     $0787                   ; 2nd row: tile count
        pla                             ; restore NT tile index
        asl     a                       ; multiply by 4 (attr stride)
        asl     a                       ; *4 complete (4 bytes per entry)
        tay                             ; Y = attr table offset
        lda     yellow_devil_chr_attr_80,y ; top-left tile
        sta     $0783                   ; NT buffer: tile data
        lda     yellow_devil_chr_attr_81,y ; top-right tile
        sta     $0784                   ; NT buffer: tile data
        lda     yellow_devil_chr_attr_80_dup,y ; bottom-left tile
        sta     $0788                   ; NT buffer: tile data
        lda     yellow_devil_chr_attr_large,y ; bottom-right tile
        sta     $0789                   ; NT buffer: tile data
        lda     #$FF                    ; end-of-buffer sentinel
        sta     $078A                   ; NT buffer: terminator
        sta     nametable_dirty         ; signal PPU update needed
        lda     ent_timer,x             ; piece index
        cmp     #$14                    ; piece #$14 = 20th piece?
        bne     yellow_devil_piece_fall_death ; no: deactivate and exit
        lda     $059F                   ; slot $1F flags
        and     #$FB                    ; clear bit 2 (piece active)
        sta     $059F                   ; update slot $1F flags
        lda     #$00                    ; clear slot $1F timer
        sta     $051F                   ; stop piece spawning
        lda     #$70                    ; assembled devil anim ID
        sta     $05DF                   ; set slot $1F OAM anim
yellow_devil_piece_fall_death:  lda     #$00 ; deactivate piece entity
        sta     ent_status,x            ; clear status
        rts                             ; piece removed

yellow_devil_piece_fall_anim:  lda     #$00 ; hold current anim frame
        sta     ent_anim_frame,x        ; prevent anim auto-advance
        rts                             ; wait for NT update to finish

yellow_devil_piece_fall_descent:  lda     ent_timer,x ; piece timer (movement phase)
        beq     yellow_devil_piece_fall_var1_zero ; zero: use var1 path
        jsr     move_sprite_down        ; move piece downward
        lda     ent_y_px,x              ; current Y position
        cmp     #$98                    ; reached floor ($98)?
        bne     yellow_devil_piece_fall_check ; no: continue falling
        lda     #$00                    ; deactivate piece
        sta     ent_status,x            ; clear status on floor hit
yellow_devil_piece_fall_check:  rts

yellow_devil_piece_fall_var1_zero:  lda     ent_var1,x ; delay counter before bounce
        beq     yellow_devil_piece_fall_phase2 ; zero: start bounce phase
        dec     ent_var1,x              ; count down delay
        bne     yellow_devil_piece_fall_check ; not zero yet: exit
        lda     #$02                    ; init bounce sub-phase
        sta     ent_var3,x              ; set var3 = 2 (bounce count)
yellow_devil_piece_fall_split:  dec     ent_var3,x ; decrement bounce counter
        bmi     yellow_devil_piece_fall_hi_vel ; negative: use high velocity
        lda     #$A3                    ; Y vel sub = $A3
        sta     ent_yvel_sub,x          ; set Y velocity sub-pixel
        lda     #$04                    ; Y velocity = 4 pixels/frame
        sta     ent_yvel,x              ; set Y velocity
        lda     #$BD                    ; X vel sub = $BD
        sta     ent_xvel_sub,x          ; set X velocity sub-pixel
        lda     #$01                    ; X velocity = 1 pixel/frame
        sta     ent_xvel,x              ; set X velocity
        rts                             ; low-speed bounce initialized

yellow_devil_piece_fall_hi_vel:  lda     #$87 ; Y vel sub = $87 (faster)
        sta     ent_yvel_sub,x          ; set Y velocity sub-pixel
        lda     #$06                    ; Y velocity = 6 pixels/frame
        sta     ent_yvel,x              ; set Y velocity
        lda     #$72                    ; X vel sub = $72 (faster)
        sta     ent_xvel_sub,x          ; set X velocity sub-pixel
        lda     #$02                    ; X velocity = 2 pixels/frame
        sta     ent_xvel,x              ; set X velocity
        rts                             ; high-speed bounce initialized

yellow_devil_piece_fall_phase2:  lda     ent_status,x ; entity status
        and     #$0F                    ; isolate low nibble (sub-phase)
        bne     yellow_devil_piece_fall_pixel ; nonzero: landed, fall column
        jsr     apply_y_speed           ; apply gravity to Y position
        lda     #$98                    ; floor Y position
        cmp     ent_y_px,x              ; compare floor to current Y
        bcs     yellow_devil_piece_fall_right ; above floor: move right
        sta     ent_y_px,x              ; clamp Y to floor
        bcc     yellow_devil_piece_fall_split ; hit floor: re-init bounce
yellow_devil_piece_fall_right:  jsr     move_sprite_right ; move piece rightward
        lda     ent_var2,x              ; target X for this piece
        cmp     ent_x_px,x              ; compare to current X
        bcs     yellow_devil_piece_fall_check ; not past target: keep going
        sta     ent_x_px,x              ; clamp X to target
        sbc     #$9F                    ; subtract $A0 (left edge)
        lsr     a                       ; divide by 16
        lsr     a                       ; /4
        lsr     a                       ; /8
        lsr     a                       ; /16 = column index
        tay                             ; Y = column index (0-4)
        lda     yellow_devil_var3_timer_table,y ; delay before fall for column
        sta     ent_var3,x              ; set as var3 timer
        lda     #$58                    ; reset Y to top of screen
        sta     ent_y_px,x              ; set Y position
        lda     #$00                    ; clear Y velocity sub-pixel
        sta     ent_yvel_sub,x          ; reset Y vel fraction
        lda     #$04                    ; fall speed = 4 px/frame
        sta     ent_yvel,x              ; set Y velocity
        inc     ent_status,x            ; advance to fall-column phase
yellow_devil_piece_fall_pixel:  lda     ent_y_px,x ; current Y pixel position
        and     #$0F                    ; isolate low nibble
        cmp     #$08                    ; at 8-pixel boundary?
        bne     yellow_devil_piece_fall_down ; no: just move down
        jsr     find_enemy_freeslot_y   ; find free entity slot
        bcs     yellow_devil_piece_fall_loop_end ; none free: skip spawn
        lda     ent_var3,x              ; parent's spawn counter
        sta     ent_timer,y             ; pass to child as timer
        inc     ent_var3,x              ; advance spawn counter
        lda     #$7F                    ; child piece anim ID ($7F)
        jsr     init_child_entity       ; spawn child piece entity
        lda     ent_x_px,x              ; copy X position to child
        sta     ent_x_px,y              ; set child X pixel
        lda     ent_x_scr,x             ; copy X screen to child
        sta     ent_x_scr,y             ; set child X screen
        lda     ent_y_px,x              ; copy Y position to child
        sta     ent_y_px,y              ; set child Y pixel
        lda     ent_hitbox,x            ; copy hitbox to child
        sta     ent_hitbox,y            ; set child hitbox
        lda     ent_routine,x           ; copy AI routine to child
        sta     ent_routine,y           ; set child AI routine
        lda     ent_y_px,x              ; check parent Y position
        cmp     #$98                    ; reached floor ($98)?
        bne     yellow_devil_piece_fall_down ; no: continue downward
        lda     #$00                    ; deactivate parent piece
        sta     ent_status,x            ; clear status
yellow_devil_piece_fall_loop_end:  rts

yellow_devil_piece_fall_down:  jmp     move_sprite_down ; fall one pixel downward

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
wily_machine_a_dispatch:  lda     #$AB  ; push return addr hi ($AB)
        pha                             ; onto stack for RTS dispatch
        lda     #$03                    ; push return addr lo ($03)
        pha                             ; return will jump to $03AC
        lda     ent_status,x            ; get AI phase
        and     #$0F                    ; isolate low nibble
        tay                             ; use as table index
        lda     wily_machine_b_addr_a,y ; load jump target lo
        sta     temp_00                 ; store in indirect ptr low
        lda     wily_machine_b_addr_b,y ; load jump target hi
        sta     $01                     ; store in indirect ptr high
        jmp     (temp_00)               ; dispatch to phase handler

        lda     #$09                    ; PSTATE_BOSS_WAIT
        cmp     player_state            ; already in boss wait?
        beq     wily_machine_a_hp_check ; yes — skip to HP fill check
        sta     player_state            ; freeze player for intro
        lda     #$80                    ; init value for HP bar fill
        sta     boss_hp_display         ; start HP bar display
        sta     boss_active             ; set boss active flag
        lda     #$8E                    ; HP fill target = 28 HP
        sta     $B3                     ; store boss HP fill target
        lda     #MUSIC_BOSS             ; boss intro music ID
        jsr     submit_sound_ID_D9      ; play boss music
        lda     #$00                    ; clear machine state vars
        sta     $69                     ; clear scroll sub-pixel
        sta     $6B                     ; clear scroll page
        sta     $6A                     ; clear scroll pixel
        ldy     #$08                    ; 9 sprite slots ($17-$1F)
wily_machine_a_oam_loop:  lda     $0377,y ; copy X positions to var3
        sta     $0577,y                 ; save initial X for flipping
        dey                             ; next sprite slot
        bpl     wily_machine_a_oam_loop ; loop all 9 sprites
wily_machine_a_hp_check:  lda     boss_hp_display ; check HP bar fill progress
        cmp     #HEALTH_FULL            ; 28 HP filled?
        bne     wily_machine_a_return   ; no — wait for fill
        inc     ent_status,x            ; advance to next AI phase
        lda     #$0D                    ; game mode = boss fight
        sta     game_mode               ; set boss fight mode
        lda     #$3A                    ; initial Y platform pos
        sta     $5E                     ; set machine Y position
        lda     #$40                    ; movement duration = 64 frames
        sta     ent_var1,x              ; set movement timer
        lda     #$32                    ; fire cooldown = 50 frames
        sta     ent_var2,x              ; set bullet cooldown
        lda     #$40                    ; initial X offset = $40
        sta     $6A                     ; set machine X offset lo
wily_machine_a_return:  rts

        ldy     ent_timer,x             ; get movement subroutine idx
        lda     wily_machine_b_routine_ptr,y ; load sub-handler addr lo
        sta     temp_00                 ; store in indirect ptr low
        lda     wily_machine_b_addr_c,y ; load sub-handler addr hi
        sta     $01                     ; store in indirect ptr high
        jmp     (temp_00)               ; dispatch to move routine

        lda     $5E                     ; current machine Y pos
        cmp     #$5A                    ; reached top position?
        beq     wily_machine_a_move_up_end ; yes — skip sprite movement
        lda     $5E                     ; reload Y position
        clc                             ; prepare for addition
        adc     #$02                    ; move up by 2 pixels
        sta     $5E                     ; store new Y position
        cmp     #$5A                    ; reached top ($5A)?
        bne     wily_machine_a_sprite_move ; no — continue sprite move
        lda     #$1E                    ; pause at top = 30 frames
        sta     ent_var1,x              ; set pause timer
wily_machine_a_sprite_move:  lda     $03DA ; save slot $1A Y pos
        pha                             ; preserve for restore after loop
        lda     $03DB                   ; save slot $1B Y pos
        pha                             ; preserve for restore after loop
        ldy     #$08                    ; 9 sprite slots
wily_machine_a_move_up_loop:  lda     $03D7,y ; get sprite Y position
        clc                             ; prepare for addition
        adc     #$02                    ; move sprite down 2px (screen)
        sta     $03D7,y                 ; update sprite Y
        dey                             ; next sprite
        bpl     wily_machine_a_move_up_loop ; loop all 9
        pla                             ; restore slot $1B Y pos
        sta     $03DB                   ; keep $1B stationary
        pla                             ; restore slot $1A Y pos
        sta     $03DA                   ; keep $1A stationary
wily_machine_a_move_up_end:  dec     ent_var1,x ; decrement move timer
        bne     wily_machine_a_move_continue ; timer expired?
        inc     ent_timer,x             ; advance to next move phase
wily_machine_a_move_continue:  jmp     wily_machine_a_bullet_fire ; check bullet firing

        lda     $03DA                   ; save slot $1A Y pos
        pha                             ; preserve for restore after loop
        lda     $03DB                   ; save slot $1B Y pos
        pha                             ; preserve for restore after loop
        ldy     #$08                    ; 9 sprite slots
wily_machine_a_move_down_loop:  lda     $03D7,y ; get sprite Y position
        sec                             ; prepare for subtraction
        sbc     #$02                    ; move sprite up 2px (screen)
        sta     $03D7,y                 ; update sprite Y
        dey                             ; next sprite
        bpl     wily_machine_a_move_down_loop ; loop all 9
        pla                             ; restore slot $1B Y pos
        sta     $03DB                   ; keep $1B stationary
        pla                             ; restore slot $1A Y pos
        sta     $03DA                   ; keep $1A stationary
        lda     $5E                     ; current machine Y pos
        sec                             ; prepare for subtraction
        sbc     #$02                    ; move platform down 2px
        sta     $5E                     ; store new Y position
        cmp     #$3A                    ; reached bottom ($3A)?
        bne     wily_machine_a_move_down_end ; no — continue
        lda     #$40                    ; movement duration = 64
        sta     ent_var1,x              ; reset move timer
        lda     $053E                   ; load saved timer index
        sta     ent_timer,x             ; restore movement direction
wily_machine_a_move_down_end:  jmp     wily_machine_a_bullet_fire ; check bullet firing

        lda     ent_var1,x              ; check frame parity
        and     #$01                    ; odd frame?
        bne     wily_machine_a_dec_y    ; yes — skip sub-sprites
        dec     $057D                   ; var3 slot $1D -= 1
        dec     $0579                   ; var3 slot $19 -= 1
        dec     $057B                   ; var3 slot $1B -= 1
wily_machine_a_dec_y:  dec     $03DB    ; Y pos slot $1B -= 1
        lda     ent_var1,x              ; check timer midpoint
        cmp     #$20                    ; at frame 32?
        bne     wily_machine_a_move_return ; no — skip anim update
        inc     $05BD                   ; anim_state slot $1D += 1
        inc     $05B9                   ; anim_state slot $19 += 1
wily_machine_a_move_return:  jmp     wily_machine_a_move_timer_dec

        lda     ent_var1,x              ; check frame parity
        and     #$01                    ; odd frame?
        bne     wily_machine_a_inc_y    ; yes — skip sub-sprites
        lda     $6A                     ; X offset lo
        clc                             ; prepare for 16-bit add
        adc     #$01                    ; increment X offset
        sta     $6A                     ; store X offset lo
        lda     $6B                     ; X offset hi
        adc     #$00                    ; propagate carry
        sta     $6B                     ; store X offset hi
        inc     $057C                   ; var3 slot $1C += 1
        inc     $0578                   ; var3 slot $18 += 1
        inc     $057A                   ; var3 slot $1A += 1
        inc     $057D                   ; var3 slot $1D += 1
        inc     $0579                   ; var3 slot $19 += 1
        inc     $057B                   ; var3 slot $1B += 1
wily_machine_a_inc_y:  inc     $03DB    ; Y pos slot $1B += 1
        lda     ent_var1,x              ; check timer midpoint
        cmp     #$20                    ; at frame 32?
        bne     wily_machine_a_inc_return ; no — skip anim update
        dec     $05BD                   ; anim_state slot $1D -= 1
        dec     $05B9                   ; anim_state slot $19 -= 1
        inc     $05BC                   ; anim_state slot $1C += 1
        inc     $05B8                   ; anim_state slot $18 += 1
wily_machine_a_inc_return:  jmp     wily_machine_a_move_timer_dec

        lda     ent_var1,x              ; check frame parity
        and     #$01                    ; odd frame?
        bne     wily_machine_a_dec_x    ; yes — skip sub-sprites
        dec     $057C                   ; var3 slot $1C -= 1
        dec     $0578                   ; var3 slot $18 -= 1
        dec     $057A                   ; var3 slot $1A -= 1
wily_machine_a_dec_x:  dec     $03DA    ; X pos slot $1A -= 1
        lda     ent_var1,x              ; check timer midpoint
        cmp     #$20                    ; at frame 32?
        bne     wily_machine_a_dec_x_return ; no — skip anim update
        dec     $05BC                   ; anim_state slot $1C -= 1
        dec     $05B8                   ; anim_state slot $18 -= 1
wily_machine_a_dec_x_return:  jmp     wily_machine_a_move_timer_dec

        inc     $03DA                   ; X pos slot $1A += 1
        jmp     wily_machine_a_move_timer_dec ; decrement timer + fire

        lda     ent_var1,x              ; check frame parity
        and     #$01                    ; odd frame?
        bne     wily_machine_a_dec_x_alt ; yes — skip sub-sprites
        inc     $057C                   ; var3 slot $1C += 1
        inc     $0578                   ; var3 slot $18 += 1
        inc     $057A                   ; var3 slot $1A += 1
wily_machine_a_dec_x_alt:  dec     $03DA ; X pos slot $1A -= 1
        lda     ent_var1,x              ; check timer midpoint
        cmp     #$20                    ; at frame 32?
        bne     wily_machine_a_move_timer_dec ; no — skip to timer dec
        inc     $05BC                   ; anim_state slot $1C += 1
        inc     $05B8                   ; anim_state slot $18 += 1
        jmp     wily_machine_a_move_timer_dec ; fall through to timer dec

        lda     ent_var1,x              ; check frame parity
        and     #$01                    ; odd frame?
        bne     wily_machine_a_inc_x    ; yes — skip sub-sprites
        lda     $6A                     ; X offset lo
        sec                             ; prepare for 16-bit subtract
        sbc     #$01                    ; decrement X offset
        sta     $6A                     ; store X offset lo
        lda     $6B                     ; X offset hi
        sbc     #$00                    ; borrow from hi byte
        sta     $6B                     ; store X offset hi
        dec     $057D                   ; var3 slot $1D -= 1
        dec     $0579                   ; var3 slot $19 -= 1
        dec     $057B                   ; var3 slot $1B -= 1
        dec     $057C                   ; var3 slot $1C -= 1
        dec     $0578                   ; var3 slot $18 -= 1
        dec     $057A                   ; var3 slot $1A -= 1
wily_machine_a_inc_x:  inc     $03DA    ; X pos slot $1A += 1
        lda     ent_var1,x              ; check timer midpoint
        cmp     #$20                    ; at frame 32?
        bne     wily_machine_a_move_timer_dec ; no — skip anim update
        dec     $05BC                   ; anim_state slot $1C -= 1
        dec     $05B8                   ; anim_state slot $18 -= 1
        inc     $05BD                   ; anim_state slot $1D += 1
        inc     $05B9                   ; anim_state slot $19 += 1
        bne     wily_machine_a_move_timer_dec ; always branches (nonzero)
        lda     ent_var1,x              ; check frame parity
        and     #$01                    ; odd frame?
        bne     wily_machine_a_inc_y_alt ; yes — skip sub-sprites
        inc     $057D                   ; var3 slot $1D += 1
        inc     $0579                   ; var3 slot $19 += 1
        inc     $057B                   ; var3 slot $1B += 1
wily_machine_a_inc_y_alt:  dec     $03DB ; Y pos slot $1B -= 1
        lda     ent_var1,x              ; check timer midpoint
        cmp     #$20                    ; at frame 32?
        bne     wily_machine_a_move_timer_dec ; no — skip anim update
        dec     $05BD                   ; anim_state slot $1D -= 1
        dec     $05B9                   ; anim_state slot $19 -= 1
        jmp     wily_machine_a_move_timer_dec ; dec timer + check bullets

        inc     $03DB                   ; Y pos slot $1B += 1
wily_machine_a_move_timer_dec:  dec     ent_var1,x ; decrement movement timer
        bne     wily_machine_a_bullet_fire ; timer active — fire bullets
        lda     #$40                    ; reset to 64 frames
        sta     ent_var1,x              ; reload movement timer
        inc     ent_timer,x             ; next direction index
        lda     ent_timer,x             ; read new direction
        and     #$07                    ; wrap to 0-7 range
        sta     ent_timer,x             ; store wrapped index
        beq     wily_machine_a_check_dir ; dir 0 — check X boundary
        cmp     #$04                    ; dir 4?
        bne     wily_machine_a_bullet_fire ; no — go fire bullets
        lda     $6A                     ; X offset lo
        sec                             ; prepare for 16-bit subtract
        sbc     #$40                    ; subtract lo byte of $0140
        sta     temp_00                 ; store difference lo
        lda     $6B                     ; X offset hi
        sbc     #$01                    ; subtract hi byte
        ora     temp_00                 ; check if exactly $140
        beq     wily_machine_a_input_check ; at right boundary — pause
        lda     #$00                    ; not at boundary
        sta     ent_timer,x             ; reset to direction 0
        beq     wily_machine_a_input_check ; always branches
wily_machine_a_check_dir:  lda     $6A  ; X offset lo
        sec                             ; prepare for 16-bit subtract
        sbc     #$C0                    ; subtract lo byte of $00C0
        sta     temp_00                 ; store difference lo
        lda     $6B                     ; X offset hi
        sbc     #$00                    ; subtract hi byte
        ora     temp_00                 ; check if exactly $C0
        beq     wily_machine_a_input_check ; at left boundary — pause
        lda     #$04                    ; force direction 4 (right)
        sta     ent_timer,x             ; override timer index
wily_machine_a_input_check:  lda     $031E ; check slot $1E status
        bmi     wily_machine_a_bullet_fire ; bit 7 set — still firing
        lda     ent_timer,x             ; save current direction
        sta     $053E                   ; backup to var1 slot $1E
        lda     #$08                    ; timer idx 8 = special mode
        sta     ent_timer,x             ; enter pause/transition
wily_machine_a_bullet_fire:  lda     $031E ; check slot $1E status
        bpl     wily_machine_a_bullet_spawn ; bit 7 clear — spawn bullets
        lda     $059E                   ; flags slot $1E
        and     #$04                    ; bit 2 = offscreen?
        bne     wily_machine_a_fire_return ; yes — skip firing
        lda     ent_var2,x              ; fire cooldown timer
        bne     wily_machine_a_dec_var2 ; still cooling down
        jsr     entity_x_dist_to_player ; get X distance to player
        cmp     #$48                    ; within 72px?
        bcc     wily_machine_a_fire_return ; too close — don't fire
        jsr     find_enemy_freeslot_y   ; find free enemy slot
        bcs     wily_machine_a_fire_return ; no slot — skip
        lda     #$B4                    ; cooldown = 180 frames
        sta     ent_var2,x              ; set fire cooldown
        lda     #$61                    ; entity ID $61 = homing shot
        jsr     init_child_entity       ; spawn homing projectile
        lda     ent_x_px,x              ; copy parent X position
        sta     ent_x_px,y              ; to child X position
        lda     camera_screen           ; copy screen page
        sta     ent_x_scr,y             ; to child screen page
        lda     #$A8                    ; Y = $A8 (near bottom)
        sta     ent_y_px,y              ; set child Y position
        lda     #$90                    ; flags = active + child
        sta     ent_flags,y             ; set child flags
        lda     #$E4                    ; routine = $E4 (homing AI)
        sta     ent_routine,y           ; set child AI routine
        lda     #$80                    ; contact damage enabled
        sta     ent_hitbox,y            ; set hitbox
        lda     #$00                    ; clear child timer
        sta     ent_timer,y             ; set child timer = 0
        sta     ent_xvel_sub,y          ; clear X velocity sub
        sta     $02                     ; clear velocity calc lo
        lda     #$01                    ; initial speed = 1 px/frame
        sta     ent_xvel,y              ; set child X velocity
        sta     $03                     ; velocity calc hi = 1
        sty     $0F                     ; save child slot
        stx     $0E                     ; save parent slot
        ldx     $0F                     ; X = child for homing calc
        jsr     calc_homing_velocity    ; calc velocity toward player
        ldy     $0F                     ; restore child slot
        ldx     $0E                     ; restore parent slot
        lda     $0C                     ; facing result from calc
        sta     ent_facing,y            ; set child facing
        lda     #$02                    ; child var1 = 2
        sta     ent_var1,y              ; set projectile param
        sta     ent_var2,y              ; set projectile param 2
wily_machine_a_dec_var2:  dec     ent_var2,x ; count down fire cooldown
wily_machine_a_fire_return:  rts

wily_machine_a_bullet_spawn:  lda     ent_var2,x ; fire cooldown active?
        bne     wily_machine_a_dec_var2 ; yes — just decrement
        ldy     #$16                    ; start at slot $16
wily_machine_a_find_slot_loop:  lda     ent_status,y ; slot active?
        bpl     wily_machine_a_bullet_init ; no — use this slot
        dey                             ; try previous slot
        cpy     #$0F                    ; below slot $10?
        bne     wily_machine_a_find_slot_loop ; no — keep searching
        rts                             ; no free slots

wily_machine_a_bullet_init:  lda     #$58 ; entity ID $58 = falling bullet
        jsr     init_child_entity       ; spawn bullet entity
        lda     #$00                    ; clear Y velocity sub
        sta     ent_yvel_sub,y          ; set Y vel sub = 0
        lda     #$04                    ; fall speed = 4 px/frame
        sta     ent_yvel,y              ; set Y velocity
        lda     #$E6                    ; routine = $E6 (bullet AI)
        sta     ent_routine,y           ; set bullet AI routine
        lda     #$80                    ; contact damage enabled
        sta     ent_hitbox,y            ; set hitbox
        lda     ent_y_px,x              ; parent Y position
        clc                             ; prepare for addition
        adc     #$0C                    ; offset down 12px
        sta     ent_y_px,y              ; set bullet Y position
        lda     camera_screen           ; copy screen page
        sta     ent_x_scr,y             ; set bullet screen page
        sty     $0F                     ; save bullet slot
        ldy     $051E                   ; bullet pattern index
        lda     wily_machine_b_param_table,y ; get fire delay for pattern
        sta     ent_var2,x              ; set next fire cooldown
        lda     wily_machine_b_bullet_param,y ; get X offset for pattern
        clc                             ; prepare for addition
        adc     ent_x_px,x              ; add to parent X pos
        ldy     $0F                     ; restore bullet slot
        sta     ent_x_px,y              ; set bullet X position
        ldx     $0F                     ; X = bullet for face_player
        jsr     face_player             ; face bullet toward player
        jsr     entity_x_dist_to_player ; get distance to player
        sta     $01                     ; dividend hi = distance
        lda     #$00                    ; clear dividend lo
        sta     temp_00                 ; set dividend lo = 0
        sta     $02                     ; clear divisor lo
        lda     #$24                    ; frames to reach = 36
        sta     $03                     ; divisor hi = 36
        jsr     divide_16bit            ; speed = dist / 36
        ldy     $0F                     ; restore bullet slot
        lda     $04                     ; quotient sub-pixel
        sta     ent_xvel_sub,x          ; set bullet X vel sub
        lda     $05                     ; quotient pixel
        sta     ent_xvel,x              ; set bullet X velocity
        ldx     #$1F                    ; X = slot $1F (machine body)
        inc     $051E                   ; advance pattern index
        lda     $051E                   ; read new index
        cmp     #$06                    ; 6 patterns total?
        bne     wily_machine_a_death_return ; not wrapped — done
        lda     #$00                    ; reset to pattern 0
        sta     $051E                   ; wrap pattern index
wily_machine_a_death_return:  rts

        ldy     #$08                    ; 9 sprite slots
wily_machine_a_flip_loop:  lda     $0597,y ; get sprite flags
        ora     #$04                    ; set offscreen bit
        sta     $0597,y                 ; assume offscreen
        lda     $0577,y                 ; saved initial X pos
        sec                             ; prepare for 16-bit subtract
        sbc     $6A                     ; subtract X offset lo
        sta     temp_00                 ; store screen X result lo
        lda     #$01                    ; X base = $100
        sbc     $6B                     ; subtract X offset hi
        bne     wily_machine_a_dey_loop ; hi != 0 — offscreen
        lda     temp_00                 ; onscreen X position
        sta     $0377,y                 ; update sprite X pos
        lda     $0597,y                 ; get sprite flags back
        and     #$FB                    ; clear offscreen bit
        sta     $0597,y                 ; mark as onscreen
wily_machine_a_dey_loop:  dey           ; next sprite slot
        bpl     wily_machine_a_flip_loop ; loop all 9 sprites
        lda     $95                     ; animation counter
        and     #$01                    ; toggle every other frame
        tay                             ; Y = 0 or 1
        lda     $059D                   ; flags slot $1D
        and     #$04                    ; offscreen?
        bne     wily_machine_a_palette_update ; yes — skip leg anim
        lda     wily_machine_b_flip_table,y ; get walk frame A
        sta     $059D                   ; set slot $1D flags
        lda     wily_machine_b_flip_alt,y ; get walk frame B
        sta     $0599                   ; set slot $19 flags
wily_machine_a_palette_update:  lda     $059C ; flags slot $1C
        and     #$04                    ; offscreen?
        bne     wily_machine_a_palette_check ; yes — skip leg anim
        lda     wily_machine_b_flip_table,y ; get walk frame A
        ora     #ENT_FLAG_HFLIP         ; add H-flip bit
        sta     $059C                   ; set slot $1C flags
        lda     wily_machine_b_flip_alt,y ; get walk frame B
        ora     #ENT_FLAG_HFLIP         ; add H-flip bit
        sta     $0598                   ; set slot $18 flags
wily_machine_a_palette_check:  lda     $031E ; check slot $1E status
        bpl     wily_machine_a_palette_end ; bit 7 clear — skip
        lda     $059F                   ; flags slot $1F
        ora     #$04                    ; set offscreen bit
        sta     $059F                   ; hide turret sprite
wily_machine_a_palette_end:  lda     #$00 ; clear anim frames
        sta     $05FD                   ; anim_frame slot $1D = 0
        sta     $05FC                   ; anim_frame slot $1C = 0
        sta     $05F9                   ; anim_frame slot $19 = 0
        sta     $05F8                   ; anim_frame slot $18 = 0
        lda     $057F                   ; var3 slot $1F
        sec                             ; prepare for 16-bit subtract
        sbc     $6A                     ; subtract X offset
        lda     #$01                    ; X base = $100
        sbc     $6B                     ; subtract X offset hi
        bne     wily_machine_a_death_palette ; turret offscreen — skip
        lda     $03DF                   ; turret Y pos (slot $1F)
        pha                             ; save original Y
        clc                             ; prepare for addition
        adc     #$18                    ; offset down 24px
        sta     $03DF                   ; temp hitbox Y position
        lda     #$18                    ; hitbox height = 24
        sta     $049F                   ; set hitbox for slot $1F
        lda     $059F                   ; save flags slot $1F
        pha                             ; preserve for restore after hit check
        and     #$F0                    ; keep upper nibble only
        sta     $059F                   ; clear flip/offscreen bits
        jsr     entity_check_player_hit ; check player hit on turret
        pla                             ; restore flags slot $1F
        sta     $059F                   ; put back flags
        pla                             ; restore turret Y pos
        sta     $03DF                   ; put back original Y
wily_machine_a_death_palette:  lda     $059F ; flags slot $1F
        and     #$04                    ; offscreen?
        bne     wily_machine_a_death_end ; yes — skip collision
        lda     #$02                    ; hitbox = 2 (small)
        sta     $049F                   ; set body hitbox
        jsr     entity_ai_dispatch      ; run AI dispatch + collision
        lda     ent_hp,x                ; check HP remaining
        bne     wily_machine_a_death_end ; still alive — done
        lda     #$6D                    ; death explosion anim ID
        sta     $05D0                   ; set anim_id slot $10
        lda     #$00                    ; clear state/frame
        sta     $05B0                   ; anim_state slot $10 = 0
        sta     $05F0                   ; anim_frame slot $10 = 0
        ldy     #$0B                    ; 12 palette entries
        lda     #$0F                    ; color = $0F (black)
wily_machine_a_death_pal_loop:  sta     $0604,y ; fill sprite palette 0
        sta     $0624,y                 ; fill sprite palette 1
        dey                             ; next entry
        bpl     wily_machine_a_death_pal_loop ; loop all 12
        sty     palette_dirty           ; flag palette for update
wily_machine_a_death_end:  rts

wily_machine_b_dispatch:  lda     $B3   ; boss active/HP flag
        bpl     wily_machine_a_death_end ; not active yet, return
        lda     player_state            ; check player state
        cmp     #PSTATE_BOSS_WAIT       ; still in boss intro?
        beq     wily_machine_a_death_end ; yes, wait for intro
        lda     ent_hp,x                ; check boss HP
        beq     wily_machine_b_fire_delay ; HP=0, do HP bar fill
        jsr     entity_ai_dispatch      ; run collision + animation AI
        lda     ent_hp,x                ; re-check HP after collision
        ora     #$80                    ; set display flag (bit 7)
        sta     boss_hp_display         ; update HP bar display
        and     #$1F                    ; mask to HP value (0-28)
        bne     wily_machine_b_fire_return ; still alive, return
        lda     #$E5                    ; death routine ID
        sta     ent_routine,x           ; switch to death handler
        lda     #$94                    ; active + child flags
        sta     ent_flags,x             ; set entity flags
        lda     #$00                    ; clear timer and anim
        sta     ent_timer,x             ; reset AI timer
        sta     ent_anim_id,x           ; clear animation ID
        ldy     #$00                    ; NT data index
wily_machine_b_nt_loop:  lda     wily_machine_b_nt_data,y ; load NT tile data byte
        sta     $0780,y                 ; store to NT update buffer
        cmp     #$FF                    ; check for $FF terminator
        beq     wily_machine_b_nt_end   ; end of NT data
        iny                             ; next data byte index
        bne     wily_machine_b_nt_loop  ; loop (always, Y won't wrap to 0)
wily_machine_b_nt_end:  sta     nametable_dirty ; request nametable update
        jmp     wily_machine_b_spawn_pellet ; spawn debris pellets

wily_machine_b_fire_delay:  lda     ent_timer,x ; HP bar fill timer
        and     #$03                    ; every 4th frame
        bne     wily_machine_b_fire_timer_inc ; not a tick frame, skip
        lda     #SFX_HP_FILL            ; HP fill tick sound
        jsr     submit_sound_ID         ; play HP fill SFX
        inc     boss_hp_display         ; advance HP bar 1 tick
        lda     boss_hp_display         ; current bar position
        cmp     #HEALTH_FULL            ; 28 HP (full)
        bne     wily_machine_b_fire_timer_inc ; not full, keep filling
        lda     $059F                   ; slot $1F flags
        and     #$FB                    ; clear bit 2 (palette)
        sta     $059F                   ; update slot $1F flags
        lda     #$00                    ; deactivate old Wily Machine
        sta     $031E                   ; clear slot $1E status
        sta     $051E                   ; clear slot $1E timer
        sta     $053E                   ; clear slot $1E var1
        sta     $055E                   ; clear slot $1E var2
        lda     $031F                   ; slot $1F status
        ora     #ENT_FLAG_HFLIP         ; set H-flip bit
        sta     $031F                   ; update slot $1F status
wily_machine_b_fire_timer_inc:  inc     ent_timer,x ; advance fill timer
wily_machine_b_fire_return:  rts

wily_machine_b_spawn_pellet:  lda     #$02 ; spawn 3 pellets (0,1,2)
        sta     $01                     ; pellet spawn counter
        lda     ent_x_px,x              ; parent X position
        sta     $02                     ; save for offset calc
        lda     ent_y_px,x              ; parent Y position
        sta     $03                     ; save for offset calc
wily_machine_b_spawn_loop:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     wily_machine_b_spawn_end ; no free slot, done
        lda     #$59                    ; pellet sprite ID
        jsr     init_child_entity       ; spawn pellet child entity
        lda     #$00                    ; clear child fields
        sta     ent_timer,y             ; reset pellet timer
        sta     ent_hitbox,y            ; no hitbox yet
        lda     #$19                    ; pellet AI routine
        sta     ent_routine,y           ; set routine index
        lda     ent_x_scr,x             ; inherit parent screen
        sta     ent_x_scr,y             ; copy screen to child
        stx     $0F                     ; save parent slot index
        ldx     $01                     ; use counter as table index
        lda     $02                     ; parent X position
        clc                             ; prepare for addition
        adc     wily_machine_b_param2,x ; add X offset for pellet
        sta     ent_x_px,y              ; set pellet X position
        lda     $03                     ; parent Y position
        clc                             ; prepare for addition
        adc     wily_machine_b_param3,x ; add Y offset for pellet
        sta     ent_y_px,y              ; set pellet Y position
        ldx     $0F                     ; restore parent slot
        dec     $01                     ; decrement spawn counter
        bpl     wily_machine_b_spawn_loop ; more pellets to spawn
wily_machine_b_spawn_end:  rts

wily_machine_b_move_dir:  lda     ent_facing,x ; check H-move direction
        and     #FACING_RIGHT           ; bit 0 = right
        beq     wily_machine_b_move_left ; not right, move left
        jsr     move_sprite_right       ; move entity right 1px
        jmp     wily_machine_b_move_vert ; skip left branch

wily_machine_b_move_left:  jsr     move_sprite_left
wily_machine_b_move_vert:  lda     ent_facing,x ; check V-move direction
        and     #$08                    ; bit 3 = up
        beq     wily_machine_b_move_down ; not up, move down
        jsr     move_sprite_up          ; move entity up 1px
        jmp     wily_machine_b_calc_vel ; skip down branch

wily_machine_b_move_down:  jsr     move_sprite_down
wily_machine_b_calc_vel:  ldy     ent_timer,x ; velocity table index
        lda     #$00                    ; clear sign extension
        sta     temp_00                 ; init high byte = 0
        sta     $01                     ; init X sign extend = 0
        lda     wily_machine_b_xvel_sub_table,y ; Y velocity (signed)
        bpl     wily_machine_b_vel_apply ; positive, no sign extend
        dec     temp_00                 ; negative, sign extend $FF
wily_machine_b_vel_apply:  lda     ent_y_sub,x ; add Y sub-pixel velocity
        clc                             ; prepare for 16-bit add
        adc     wily_machine_b_yvel_table,y ; add Y sub-pixel component
        sta     ent_y_sub,x             ; update Y sub-pixel
        lda     ent_y_px,x              ; add Y pixel velocity
        adc     wily_machine_b_xvel_sub_table,y ; add Y pixel component
        sta     ent_y_px,x              ; update Y pixel
        lda     ent_y_scr,x             ; add sign extension to screen
        adc     temp_00                 ; carry into screen byte
        beq     wily_machine_b_x_vel_apply ; still on screen 0, OK
        lda     #$00                    ; off-screen, despawn
        sta     ent_status,x            ; deactivate entity
        rts                             ; entity left screen

wily_machine_b_x_vel_apply:  lda     wily_machine_b_xvel_sign_table,y ; X velocity sign byte
        bpl     wily_machine_b_x_sub_vel ; positive, no sign extend
        dec     $01                     ; negative, sign extend $FF
wily_machine_b_x_sub_vel:  lda     ent_x_sub,x ; add X sub-pixel velocity
        clc                             ; prepare for 16-bit add
        adc     wily_machine_b_xvel_table,y ; add X sub-pixel component
        sta     ent_x_sub,x             ; update X sub-pixel
        lda     ent_x_px,x              ; add X pixel velocity
        adc     wily_machine_b_xvel_sign_table,y ; add X pixel component
        sta     ent_x_px,x              ; update X pixel
        lda     ent_x_scr,x             ; add sign to screen page
        adc     $01                     ; carry into screen byte
        sta     ent_x_scr,x             ; update X screen
        dec     ent_var1,x              ; decrement step counter
        bne     wily_machine_b_move_return ; steps remain, keep moving
        inc     ent_timer,x             ; advance to next vel index
        lda     ent_timer,x             ; read new timer value
        and     #$0F                    ; wrap at 16 entries
        sta     ent_timer,x             ; store wrapped index
        bne     wily_machine_b_collision ; not at start, skip speed bump
        inc     ent_var2,x              ; full cycle: increase speed
wily_machine_b_collision:  lda     ent_var2,x ; reload step count from var2
        sta     ent_var1,x              ; set steps for this segment
wily_machine_b_move_return:  rts

wily_machine_b_attack_vert:  jsr     apply_y_speed ; apply gravity + Y velocity
        lda     ent_facing,x            ; check facing direction
        and     #FACING_RIGHT           ; bit 0 = right
        beq     wily_machine_b_move_left_atk ; not right, move left
        jmp     move_sprite_right       ; move right and return

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
gamma_b_init:  lda     ent_status,x     ; AI phase
        and     #$0F                    ; check phase (low nibble)
        bne     gamma_b_phase_handler   ; skip init if not phase 0
        sta     $95                     ; clear palette phase flag
        inc     ent_status,x            ; advance to phase 1
        lda     #PSTATE_BOSS_WAIT       ; state → $09 (boss_wait)
        sta     player_state            ; freeze player for HP fill
        lda     #$80                    ; init boss HP display
        sta     boss_hp_display         ; init HP display
        sta     boss_active             ; boss active flag
        lda     #$8E                    ; HP fill target (28 HP)
        sta     $B3                     ; store HP target
        lda     #MUSIC_BOSS             ; SFX $0D = boss intro music
        jsr     submit_sound_ID_D9      ; play boss music
        lda     #$30                    ; palette fade timer
        sta     ent_timer,x             ; set fade frame count
        lda     #$6C                    ; CHR bank for Gamma sprites
        sta     $E8                     ; set $2000 CHR bank
        lda     #$6E                    ; CHR bank for Gamma BG
        sta     $E9                     ; set $2800 CHR bank
        jsr     update_CHR_banks        ; apply CHR bank changes
        lda     #$00                    ; clear scroll offsets
        sta     $69                     ; clear scroll sub-pixel
        sta     $6A                     ; clear scroll pixel
        sta     $6B                     ; clear scroll page
gamma_b_phase_handler:  lda     ent_timer,x ; check fade timer
        bmi     gamma_b_hp_bar_check    ; negative = fade done
        lda     #$00                    ; unused dead load
        lda     $95                     ; palette phase flag
        and     #$0F                    ; check low nibble
        bne     gamma_b_setup_hp        ; nonzero = skip palette fade
        ldy     #$0B                    ; 12 palette entries (0-11)
gamma_b_pal_fill_hi_loop:  lda     gamma_b_pal_hi_table,y ; target palette value
        sec                             ; prepare for subtraction
        sbc     ent_timer,x             ; subtract fade progress
        bcs     gamma_b_pal_hi_store    ; no underflow, use result
        lda     #$0F                    ; underflow = black ($0F)
gamma_b_pal_hi_store:  sta     $0604,y  ; write to sprite palette buf
        sta     $0624,y                 ; write to BG palette buf
        dey                             ; next palette entry
        bpl     gamma_b_pal_fill_hi_loop ; loop all 12 entries
        ldy     #$07                    ; 8 palette entries (0-7)
gamma_b_pal_fill_lo_loop:  lda     gamma_b_pal_lo_table,y ; target palette value
        sec                             ; prepare for subtraction
        sbc     ent_timer,x             ; subtract fade progress
        bcs     gamma_b_pal_lo_store    ; no underflow, use result
        lda     #$0F                    ; underflow = black ($0F)
gamma_b_pal_lo_store:  sta     $0618,y  ; write to sprite palette buf
        sta     $0638,y                 ; write to BG palette buf
        dey                             ; next palette entry
        bpl     gamma_b_pal_fill_lo_loop ; loop all 8 entries
        sty     palette_dirty           ; Y=$FF, flag palette dirty
        lda     ent_timer,x             ; current fade timer
        sec                             ; prepare for subtraction
        sbc     #$10                    ; step fade by $10
        sta     ent_timer,x             ; update fade timer
gamma_b_setup_hp:  lda     #$80         ; reset HP display flag
        sta     boss_hp_display         ; keep bar at position 0

; Wait for boss HP bar to fill, then release player
gamma_b_hp_bar_check:  lda     boss_hp_display ; HP bar position
        cmp     #HEALTH_FULL            ; filled to max?
        bne     gamma_b_init_end        ; no → keep filling
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state            ; release player, fight begins
        sta     ent_timer,x             ; clear AI timer
        lda     #$C0                    ; status = active phase 0
        sta     ent_status,x            ; set active status
        lda     #$E8                    ; Gamma main AI routine
        sta     ent_routine,x           ; set routine index
        lda     #$1C                    ; 28 HP
        sta     ent_hp,x                ; set boss HP
gamma_b_init_end:  lda     #$00         ; clear anim frame counter
        sta     ent_anim_frame,x        ; reset animation
        rts                             ; init/fill frame complete

gamma_b_pal_hi_table:  .byte   $0F,$30,$16,$04,$0F,$30,$11,$01
        .byte   $0F,$30,$36,$26
gamma_b_pal_lo_table:  .byte   $0F,$01,$30,$11,$0F,$0F,$30,$10
gamma_b_main_update:  lda     ent_timer,x ; bullet cooldown timer
        bne     gamma_b_timer_dec       ; nonzero = still cooling down
        jsr     entity_x_dist_to_player ; X distance to player
        cmp     #$50                    ; within 80px?
        bcs     gamma_b_var2_check      ; too far, skip bullet
        jsr     face_player             ; turn toward player
        jsr     gamma_b_spawn_bullet    ; fire bullet at player
        lda     #$1F                    ; 31 frame cooldown
        sta     ent_timer,x             ; set bullet cooldown
gamma_b_timer_dec:  dec     ent_timer,x ; tick cooldown timer
gamma_b_var2_check:  lda     ent_var2,x ; homing spawn burst count
        bne     gamma_b_var1_loop       ; nonzero = in burst, spawn
        jsr     entity_y_dist_to_player ; Y distance to player
        cmp     #$30                    ; within 48px vertically?
        bcs     gamma_b_hp_check        ; too far, check HP
gamma_b_var1_loop:  lda     ent_var1,x  ; homing spawn delay timer
        bne     gamma_b_var1_dec        ; nonzero = still waiting
        lda     #$02                    ; spawn 2 homing projectiles
        sta     $01                     ; set spawn pair count
        jsr     gamma_b_spawn_homing_loop ; spawn homing pair
        lda     #$1F                    ; 31 frame delay between pairs
        sta     ent_var1,x              ; set inter-pair delay
        inc     ent_var2,x              ; count pairs spawned
        lda     ent_var2,x              ; read pair count
        cmp     #$03                    ; spawned 3 pairs yet?
        bcc     gamma_b_var1_dec        ; not yet, keep spawning
        lda     #$79                    ; long cooldown (121 frames)
        sta     ent_var1,x              ; set long cooldown timer
        lda     #$00                    ; reset burst counter
        sta     ent_var2,x              ; clear pairs spawned
gamma_b_var1_dec:  dec     ent_var1,x   ; tick spawn delay
gamma_b_hp_check:  lda     ent_hp,x     ; check boss HP
        cmp     #$0F                    ; below 15 HP?
        bcc     gamma_b_spawn_mine      ; yes, trigger mine phase
        rts                             ; HP >= 15, continue normal AI

gamma_b_spawn_mine:  ldy     #$17       ; start at slot $17
gamma_b_spawn_loop:  cpy     #$10       ; slot $10+?
        bcs     gamma_b_spawn_type2     ; yes, use type 2 sprite
        lda     #$7A                    ; mine sprite ID
        bne     gamma_b_spawn_init      ; always branch (A!=0)
gamma_b_spawn_type2:  lda     #$5B      ; debris sprite ID
gamma_b_spawn_init:  jsr     init_child_entity ; spawn mine/debris child
        lda     #$90                    ; active + no H-flip
        sta     ent_flags,y             ; set entity flags
        lda     #$00                    ; no collision initially
        sta     ent_hitbox,y            ; clear hitbox
        lda     #$10                    ; mine/debris AI routine
        sta     ent_routine,y           ; set routine index
        lda     ent_x_px,x              ; inherit parent X position
        sta     ent_x_px,y              ; copy X pixel to child
        lda     ent_x_scr,x             ; inherit parent X screen
        sta     ent_x_scr,y             ; copy X screen to child
        lda     ent_y_px,x              ; inherit parent Y position
        sta     ent_y_px,y              ; copy Y pixel to child
        lda     $D7E9,y                 ; X velocity sub from table
        sta     ent_xvel_sub,y          ; set child X vel sub
        lda     $D7F9,y                 ; X velocity from table
        sta     ent_xvel,y              ; set child X velocity
        lda     $D809,y                 ; Y velocity sub from table
        sta     ent_yvel_sub,y          ; set child Y vel sub
        lda     $D819,y                 ; Y velocity from table
        sta     ent_yvel,y              ; set child Y velocity
        dey                             ; next slot down
        cpy     #$07                    ; slots $08-$17
        bne     gamma_b_spawn_loop      ; loop until all spawned

; Gamma phase transition — scroll screen vertically
        lda     #PSTATE_SCREEN_SCROLL   ; state → $10 (screen_scroll)
        sta     player_state            ; player frozen during scroll
        lda     #$C0                    ; active entity status
        sta     ent_status,x            ; set status
        lda     #$00                    ; clear scroll/movement vars
        sta     $69                     ; clear scroll sub-pixel
        sta     $6A                     ; clear scroll pixel
        sta     ent_xvel_sub,x          ; stop X movement
        sta     ent_xvel,x              ; zero X velocity
        lda     #$B4                    ; 180 frame timer
        sta     ent_timer,x             ; set transition timer
        lda     #$F0                    ; scroll speed = $F0
        sta     ent_var1,x              ; set scroll var1
        lda     #$02                    ; scroll direction = up
        sta     ent_var2,x              ; set scroll var2
        lda     #$E9                    ; Gamma head AI routine
        sta     ent_routine,x           ; switch to head phase
        lda     ent_flags,x             ; current entity flags
        ora     #$04                    ; set bit 2 (immune)
        sta     ent_flags,x             ; update flags
        lda     ent_hitbox,x            ; current hitbox
        and     #$BF                    ; clear bit 6 (no contact dmg)
        sta     ent_hitbox,x            ; update hitbox
        lda     #$6B                    ; Gamma head sprite ID
        jmp     reset_sprite_anim       ; set animation to head

gamma_b_spawn_homing_loop:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     gamma_b_spawn_end       ; no free slot, return
        sty     temp_00                 ; save child slot index
        lda     ent_facing,x            ; parent facing direction
        sta     ent_facing,y            ; child inherits facing
        and     #FACING_LEFT            ; bit 1 = facing left
        tay                             ; use as table index (0/2)
        lda     ent_x_px,x              ; parent X pixel
        clc                             ; prepare for addition
        adc     gamma_b_param_table,y   ; add facing-based X offset
        pha                             ; save low byte
        lda     ent_x_scr,x             ; parent X screen
        adc     gamma_b_sprite_id_table,y ; add screen carry/offset
        ldy     temp_00                 ; restore child slot
        sta     ent_x_scr,y             ; set child X screen
        pla                             ; restore X pixel low
        sta     ent_x_px,y              ; set child X pixel
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y              ; set child Y pixel
        lda     #$80                    ; initial X speed $01.80
        sta     ent_xvel_sub,y          ; set X vel sub-pixel
        lda     #$01                    ; 1 pixel/frame base speed
        sta     ent_xvel,y              ; set X velocity
        lda     #$58                    ; homing projectile sprite
        jsr     init_child_entity       ; spawn homing child
        lda     #$51                    ; homing AI routine
        sta     ent_routine,y           ; set routine index
        lda     #$8B                    ; contact damage + hitbox
        sta     ent_hitbox,y            ; set hitbox
        lda     #$00                    ; 0 HP (1-hit kill)
        sta     ent_hp,y                ; set child HP
        lda     ent_facing,x            ; current facing
        eor     #$03                    ; flip left/right bits
        sta     ent_facing,x            ; alternate facing each spawn
        dec     $01                     ; decrement pair counter
        bne     gamma_b_spawn_homing_loop ; spawn next of pair
gamma_b_spawn_end:  rts

gamma_b_param_table:  .byte   $0F
gamma_b_sprite_id_table:  .byte   $00,$F1,$FF
gamma_b_spawn_bullet:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     gamma_b_bullet_return   ; no free slot, return
        lda     #$00                    ; no Y sub-pixel velocity
        sta     ent_yvel_sub,y          ; clear Y vel sub
        lda     #$04                    ; 4 px/frame downward
        sta     ent_yvel,y              ; set Y velocity
        lda     #$58                    ; bullet projectile sprite
        jsr     init_child_entity       ; spawn bullet child
        lda     #$8B                    ; contact damage + hitbox
        sta     ent_hitbox,y            ; set hitbox
        lda     #$0C                    ; bullet AI routine
        sta     ent_routine,y           ; set routine index
        lda     ent_x_px,x              ; copy parent X to bullet
        sta     ent_x_px,y              ; set bullet X pixel
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; set bullet X screen
        lda     ent_y_px,x              ; parent Y position
        sec                             ; prepare for subtraction
        sbc     #$10                    ; offset 16px up (mouth)
        sta     ent_y_px,y              ; set bullet Y pixel
        lda     ent_y_scr,x             ; copy parent Y screen
        sta     ent_y_scr,y             ; set bullet Y screen
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y            ; bullet inherits facing
        jsr     entity_x_dist_to_player ; get X dist to player
        stx     temp_00                 ; save parent slot
        ldx     #$03                    ; start at highest speed
gamma_b_vel_select_loop:  cmp     gamma_b_vel_threshold_table,x ; dist < threshold?
        bcc     gamma_b_vel_apply       ; yes, use this speed
        dex                             ; try slower speed index
        bne     gamma_b_vel_select_loop ; try next threshold
gamma_b_vel_apply:  lda     gamma_b_xvel_sub_table,x ; X vel sub for distance
        sta     ent_xvel_sub,y          ; set bullet X vel sub
        lda     gamma_b_xvel_table,x    ; X vel pixel for distance
        sta     ent_xvel,y              ; set bullet X velocity
        ldx     temp_00                 ; restore parent slot
gamma_b_bullet_return:  rts

gamma_b_vel_threshold_table:  .byte   $4C,$3D,$2E,$1F
gamma_b_xvel_sub_table:  .byte   $00,$80,$00,$80
gamma_b_xvel_table:  .byte   $02,$01,$01,$00
gamma_f_main:  lda     ent_timer,x      ; spawn cooldown active?
        bne     gamma_f_timer_dec       ; yes → decrement and skip spawn
        jsr     find_enemy_freeslot_y   ; find free enemy slot
        bcs     gamma_f_var1_check      ; none free → skip spawn
        lda     #$77                    ; child entity ID $77 = Gamma fist
        jsr     init_child_entity       ; spawn fist projectile
        lda     ent_x_px,x              ; copy parent X position
        sta     ent_x_px,y              ; to child entity
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to child entity
        lda     ent_y_px,x              ; parent Y position
        clc                             ; prepare for addition
        adc     #$38                    ; offset +$38 px downward
        sta     ent_y_px,y              ; set child Y position
        lda     #$80                    ; hitbox $80 = contact damage
        sta     ent_hitbox,y            ; set fist hitbox
        lda     #$50                    ; AI routine $50
        sta     ent_routine,y           ; set fist AI routine
        lda     #$00                    ; zero X velocity sub-pixel
        sta     ent_xvel_sub,y          ; clear fist X vel sub
        sta     $02                     ; store sub-pixel for homing calc
        lda     #$04                    ; base speed = 4 px/frame
        sta     ent_xvel,y              ; set fist X velocity
        sta     $03                     ; store pixel vel for homing calc
        sty     $0F                     ; save child slot in $0F
        stx     $0E                     ; save parent slot in $0E
        ldx     $0F                     ; X = child slot for homing calc
        jsr     calc_homing_velocity    ; aim fist toward player
        ldy     $0F                     ; restore child slot
        ldx     $0E                     ; restore parent slot
        lda     $0C                     ; homing result = facing dir
        sta     ent_facing,y            ; set child facing direction
        lda     #$B5                    ; spawn cooldown = $B5 frames
        sta     ent_timer,x             ; set fist respawn timer
gamma_f_timer_dec:  dec     ent_timer,x ; count down spawn timer
gamma_f_var1_check:  lda     ent_var1,x ; pause timer (scroll done)
        bne     gamma_f_var1_dec        ; nonzero → count down
        lda     ent_var2,x              ; scroll direction flag
        and     #$01                    ; bit 0: 0=scroll right, 1=left
        bne     gamma_f_scroll_alt      ; odd → scroll left
        lda     $69                     ; scroll position sub-pixel
        clc                             ; prepare for addition
        adc     ent_xvel_sub,x          ; add velocity to scroll pos
        sta     $69                     ; update scroll sub-pixel
        lda     $6A                     ; scroll position pixel
        adc     ent_xvel,x              ; add velocity to scroll pos
        sta     $6A                     ; update scroll pixel position
        cmp     #$A0                    ; reached right limit $A0?
        bcs     gamma_f_set_scroll      ; yes → clamp and reverse
        lda     ent_xvel_sub,x          ; accelerate scroll speed
        clc                             ; prepare for acceleration add
        adc     #$10                    ; add $10 to sub-pixel velocity
        sta     ent_xvel_sub,x          ; store updated sub-pixel vel
        lda     ent_xvel,x              ; carry into pixel velocity
        adc     #$00                    ; carry into pixel velocity
        sta     ent_xvel,x              ; store updated pixel velocity
        cmp     #$03                    ; speed capped at 3 px/frame?
        bne     gamma_f_collision_update ; not yet → continue
        lda     #$00                    ; clamp sub-pixel to 0
        sta     ent_xvel_sub,x          ; cap sub-pixel at zero
        beq     gamma_f_collision_update ; always branch (A=0)
gamma_f_set_scroll:  lda     #$A0       ; clamp scroll to $A0
        sta     $6A                     ; set scroll pixel to limit
        lda     #$80                    ; deceleration sub-pixel
        sta     ent_xvel_sub,x          ; set deceleration sub-pixel
        lda     #$01                    ; deceleration = 1 px/frame
        sta     ent_xvel,x              ; set deceleration pixel velocity
        lda     #$01                    ; switch to scroll-left mode
        sta     ent_var2,x              ; switch scroll to leftward
        bne     gamma_f_collision_update ; always branch
gamma_f_scroll_alt:  lda     $69        ; scroll sub-pixel
        sec                             ; prepare for subtraction
        sbc     ent_xvel_sub,x          ; subtract velocity from scroll
        sta     $69                     ; update scroll sub-pixel
        lda     $6A                     ; scroll pixel
        sbc     ent_xvel,x              ; subtract velocity from scroll
        sta     $6A                     ; update scroll pixel position
        bcs     gamma_f_collision_update ; no underflow → continue
        lda     #$00                    ; clamp scroll to 0
        sta     $69                     ; clamp scroll sub to zero
        sta     $6A                     ; clamp scroll pixel to zero
        sta     ent_xvel_sub,x          ; stop scroll sub-pixel
        sta     ent_xvel,x              ; stop scroll pixel velocity
        lda     #$F1                    ; pause timer = $F1 frames
        sta     ent_var1,x              ; set pause duration
        lda     #$02                    ; var2 = 2 → scroll phase done
        sta     ent_var2,x              ; store completed scroll phase
gamma_f_var1_dec:  dec     ent_var1,x   ; count down pause timer
gamma_f_collision_update:  jsr     entity_ai_dispatch ; run collision + animation
        lda     ent_hp,x                ; check if Gamma F defeated
        bne     gamma_f_return          ; HP > 0 → still alive
        sta     $6A                     ; clear scroll pixel
        sta     $6B                     ; clear scroll high byte
        ldy     #$0B                    ; 12 palette entries
        lda     #$0F                    ; white ($0F) for death flash
gamma_f_death_pal_loop:  sta     $0604,y ; write BG palette 1
        sta     $0624,y                 ; write BG palette 2
        dey                             ; next palette entry
        bpl     gamma_f_death_pal_loop  ; loop all 12 entries
        sty     palette_dirty           ; Y=$FF → trigger palette DMA
        ldy     #$00                    ; index for NT data copy
gamma_f_death_nt_loop:  lda     gamma_f_nt_data,y ; load nametable patch data
        sta     $0780,y                 ; write to NT update buffer
        cmp     #$FF                    ; $FF = end-of-data sentinel
        beq     gamma_f_death_end       ; done → exit loop
        iny                             ; advance NT data index
        bne     gamma_f_death_nt_loop   ; loop until 256 or sentinel
gamma_f_death_end:  sta     nametable_dirty ; trigger nametable update
        lda     #$80                    ; set up death explosion entity
        sta     $0310                   ; ent_status slot $10 = active
        lda     #$90                    ; ent_flags slot $10
        sta     $0590                   ; set explosion entity flags
        lda     #$6D                    ; anim ID $6D = explosion
        sta     $05D0                   ; set explosion anim ID
        lda     #$00                    ; clear anim frame
        sta     $05F0                   ; clear explosion anim frame
        sta     $05B0                   ; clear anim state
        sta     $0490                   ; clear hitbox (no damage)
        lda     #$EF                    ; AI routine $EF
        sta     $0330                   ; set explosion AI routine
        lda     #$A3                    ; Y velocity sub = $A3
        sta     $0450                   ; set explosion Y vel sub
        lda     #$04                    ; Y velocity = 4 (falling)
        sta     $0470                   ; set explosion Y velocity
        lda     #$00                    ; clear scroll position
        sta     $6A                     ; zero scroll pixel
        sta     $6B                     ; zero scroll high byte
gamma_f_return:  rts

gamma_f_nt_data:  .byte   $23,$C0,$0F,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$FF
gamma_f_collision_check:  lda     ent_flags,x ; check bit 2 of entity flags
        and     #$04                    ; bit 2 = offscreen flag
        bne     gamma_f_collision_flag  ; offscreen → skip collision
        lda     player_state            ; current player state
        cmp     #PSTATE_DAMAGE          ; player taking damage?
        beq     gamma_f_collision_flag  ; taking damage → skip
        cmp     #PSTATE_DEATH           ; player dead?
        beq     gamma_f_collision_flag  ; dead → skip
        lda     ent_y_px                ; save player Y pixel
        pha                             ; save original Y on stack
        inc     ent_y_px                ; nudge player Y down 1 px
        jsr     check_player_collision  ; test overlap with nudged Y
        bcs     gamma_f_player_collision ; no overlap → check platform
        lda     $041F                   ; copy weapon X velocity sub
        sta     $37                     ; store as push sub-pixel
        lda     $043F                   ; copy weapon X velocity
        sta     $38                     ; store as push velocity
        lda     $055F                   ; copy weapon facing
        sta     $36                     ; store as push direction
gamma_f_player_collision:  pla          ; restore player Y pixel
        sta     ent_y_px                ; write back player Y pixel

; Gamma/fortress hazard — instant kill on contact
        lda     ent_spawn_id,x          ; entity sub-type
        cmp     #$0D                    ; sub-type $0D = Gamma fist (skip instant kill)
        beq     gamma_f_collision_flag  ; fist uses normal damage instead
        lda     invincibility_timer     ; i-frames active?
        bne     gamma_f_collision_flag  ; skip
        jsr     check_player_collision  ; AABB collision test
        bcs     gamma_f_collision_flag  ; no collision → skip
        lda     #PSTATE_DEATH           ; state → $0E (death)
        sta     player_state            ; instant kill, no damage calc
; --- overlap trick: $09,$04 = ora #$04 (set bit 2 in flags) ---
; Fall-through executes ora #$04; gamma_f_collision_byte also read by index.
gamma_f_collision_flag:  lda     ent_flags,x ; read current flags
        .byte   $09                     ; opcode: ora immediate
gamma_f_collision_byte:  .byte   $04     ; code: #$04 | data: indexed table
        sta     ent_flags,x             ; store updated flags
        lda     gamma_f_collision_byte,x ; indexed read from $04 table
        sec                             ; prepare for subtraction
        sbc     $6A                     ; subtract scroll offset
        bcs     gamma_f_collision_end   ; no underflow → done
        sta     ent_x_px,x              ; update X pixel from scroll
        lda     ent_flags,x             ; clear bit 2 (now onscreen)
        and     #$FB                    ; clear bit 2 (now onscreen)
        sta     ent_flags,x             ; update entity flags
gamma_f_collision_end:  rts

        .byte   $80,$40,$10

; ---------------------------------------------------------------------------
; Teleporter tube — player steps in, warps to boss refight room
; Sets state $11 (warp_init), which transitions to $12 (warp_anim),
; then back to $00 (on_ground) in the destination room.
; warp_dest = destination index (from entity sub-type).
; ---------------------------------------------------------------------------
teleporter_collision:  jsr     check_player_collision ; is player touching teleporter?
        bcs     teleporter_return       ; no → return
        jsr     entity_x_dist_to_player ; detailed alignment check
        cmp     #$02                    ; close enough to center?
        bcs     teleporter_return       ; no → return
        lda     ent_x_px,x              ; snap player X to teleporter X
        sta     ent_x_px                ; align player X to teleporter
        lda     ent_spawn_id,x          ; entity sub-type - $0E =
        sbc     #$0E                    ; destination index
        cmp     #$01                    ; check for invalid destination
        beq     teleporter_return       ; destination 1 = invalid? skip
        sta     warp_dest               ; set warp destination
        lda     #PSTATE_WARP_INIT       ; state → $11 (warp_init)
        sta     player_state            ; begin teleporter sequence
        lda     #$13                    ; player OAM $13 = teleport beam
        sta     ent_anim_id             ; set teleport beam animation
        lda     #$00                    ; zero for clearing anim state
        sta     ent_anim_frame          ; reset animation counter
        sta     ent_anim_state          ; reset animation frame
        sta     ent_status,x            ; despawn teleporter entity
teleporter_return:  rts

teleporter_pos_check_y:  lda     ent_y_px,x ; check Y position
        cmp     #$68                    ; target Y = $68
        beq     teleporter_pos_check_x  ; at target → check X
        inc     ent_x_px,x              ; not at target → move right
        lda     #FACING_RIGHT           ; facing right
        jmp     teleporter_set_facing   ; set facing and return

teleporter_pos_check_x:  dec     ent_x_px,x ; move left toward center
        lda     #FACING_LEFT            ; facing left
teleporter_set_facing:  sta     ent_facing,x ; store facing direction
        lda     ent_y_px,x              ; save Y position
        pha                             ; save Y on stack
        dec     ent_y_px,x              ; nudge Y up 1 px for overlap
        jsr     check_player_collision  ; test player collision
        pla                             ; restore original Y
        sta     ent_y_px,x              ; restore original Y position
        bcs     teleporter_door_return  ; no collision → return
        lda     ent_facing,x            ; push direction to player
        sta     $36                     ; store facing as push direction
        lda     #$00                    ; clear push sub-pixel
        sta     $37                     ; clear push sub-pixel
        lda     #$01                    ; push speed = 1 px
        sta     $38                     ; set push pixel velocity
teleporter_door_return:  rts

teleporter_activate_check:  lda     ent_y_px,x ; current Y position
        cmp     #$A8                    ; target Y = $A8 (ground)
        beq     teleporter_anim_check   ; at ground → check anim
        clc                             ; prepare for addition
        adc     #$04                    ; move down 4 px/frame
        sta     ent_y_px,x              ; update Y position
        cmp     #$A8                    ; reached ground level?
        bne     teleporter_anim_end     ; not yet → done
        lda     #$6C                    ; anim $6C = closed teleporter
        cmp     ent_anim_id,x           ; already closed?
        beq     teleporter_anim_check   ; yes → skip to timer check
        jsr     reset_sprite_anim       ; set closed teleporter anim
        lda     #$10                    ; delay timer = 16 frames
        sta     ent_timer,x             ; set open delay timer
teleporter_anim_check:  lda     ent_anim_id,x ; current animation ID
        cmp     #$6E                    ; anim $6E = fully open
        bne     teleporter_anim_end     ; not fully open → done
        lda     ent_timer,x             ; check delay timer
        beq     teleporter_anim_end     ; expired → done
        dec     ent_timer,x             ; count down delay
        lda     #$00                    ; hold animation at frame 0
        sta     ent_anim_frame,x        ; freeze animation at frame 0
teleporter_anim_end:  rts

teleporter_fall:  ldy     #$08          ; gravity strength = 8
        jsr     move_vertical_gravity   ; apply gravity + floor check
        bcs     teleporter_fall_check   ; landed → check anim state
        inc     ent_x_px,x              ; still falling → nudge right
        rts                             ; return while still airborne

teleporter_fall_check:  lda     #$6C    ; anim $6C = closed teleporter
        cmp     ent_anim_id,x           ; already set?
        beq     teleporter_fall_end     ; yes → done
        jsr     reset_sprite_anim       ; set closed anim on landing
teleporter_fall_end:  rts

teleporter_fall_rts:  jsr     apply_y_speed ; apply vertical velocity
        lda     #$B0                    ; floor limit Y = $B0
        cmp     ent_y_px,x              ; below floor?
        bcs     teleporter_fall_end     ; above floor → done
        sta     ent_y_px,x              ; clamp Y to floor
        lda     ent_timer,x             ; check block phase timer
        cmp     #$02                    ; phase 2 = clear register
        beq     wily_machine_c_clear_reg ; timer 2 → clear $0310
        bcs     wily_machine_c_set_flags ; timer > 2 → set flags
        lda     #$00                    ; timer < 2 → despawn block
        sta     ent_status,x            ; despawn expired block
wily_machine_c_var1_init:  lda     #$03 ; loop counter = 3 (4 blocks)
        sta     temp_00                 ; store in temp $00
wily_machine_c_spawn_loop:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     teleporter_fall_end     ; none free → abort
        lda     #$78                    ; child ID $78 = falling block
        jsr     init_child_entity       ; spawn block entity
        lda     #$FA                    ; AI routine $FA
        sta     ent_routine,y           ; set block AI routine
        lda     #$00                    ; no hitbox
        sta     ent_hitbox,y            ; blocks have no collision
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; set child X screen
        sta     ent_xvel_sub,y          ; X vel sub = screen (0)
        lda     #$44                    ; Y velocity sub = $44
        sta     ent_yvel_sub,y          ; set block Y vel sub-pixel
        lda     #$03                    ; Y velocity = 3 (falling)
        sta     ent_yvel,y              ; set block fall speed
        stx     $01                     ; save parent slot
        lda     ent_x_px,x              ; parent X position
        sta     $02                     ; save in temp $02
        ldx     temp_00                 ; X = loop counter
        lda     wily_machine_c_y_pos_table,x ; Y offset for this block
        sta     ent_y_px,y              ; set block Y position
        lda     $02                     ; parent X position
        clc                             ; prepare for X offset add
        adc     wily_machine_c_xvel_table,x ; add X offset for this block
        sta     ent_x_px,y              ; set block X position
        lda     wily_machine_c_facing_table,x ; block facing direction
        sta     ent_facing,y            ; set block scatter direction
        lda     wily_machine_c_flags_table,x ; block movement flags
        sta     ent_xvel,y              ; set block lateral speed
        ldx     $01                     ; restore parent slot
        dec     temp_00                 ; next block index
        bpl     wily_machine_c_spawn_loop ; loop until all 4 spawned
        rts                             ; all blocks spawned

wily_machine_c_clear_reg:  lda     #$00 ; clear entity status $10
        sta     $0310                   ; deactivate slot $10 entity
        rts                             ; return after clearing

wily_machine_c_set_flags:  lda     ent_flags ; player entity flags
        ora     #$04                    ; set bit 2 (freeze player)
        sta     ent_flags               ; freeze player movement
        lda     ent_var1,x              ; frame counter
        cmp     #$3C                    ; reached 60 frames ($3C)?
        bne     wily_machine_c_main     ; not yet → increment
        lda     #$79                    ; anim $79 = Wily Machine reveal
        jsr     reset_sprite_anim       ; set reveal animation
        stx     ent_var3                ; save parent slot for cleanup
        lda     #$00                    ; deactivate AI routine
        sta     ent_routine,x           ; disable Wily Machine AI
        lda     #$B4                    ; Y = $B4 (floor level)
        sta     ent_y_px,x              ; place at floor level
        jmp     wily_machine_c_var1_init ; spawn 4 new blocks

wily_machine_c_main:  inc     ent_var1,x ; increment frame counter
        bne     wily_machine_c_return   ; nonzero → wait
        jsr     find_enemy_freeslot_y   ; find free enemy slot
        bcs     wily_machine_c_return   ; none free → skip
        lda     #$7D                    ; child ID $7D = falling block
        jsr     init_child_entity       ; spawn falling block
        lda     #$00                    ; block starts at Y = 0
        sta     ent_y_px,y              ; start at top of screen
        lda     ent_x_px,x              ; match parent X position
        sta     ent_x_px,y              ; set block X pixel
        lda     ent_x_scr,x             ; match parent X screen
        sta     ent_x_scr,y             ; set block X screen
        sta     ent_timer,y             ; timer = X screen (0)
        lda     #$00                    ; clear hitbox
        sta     ent_hitbox,y            ; no collision for block
        sta     ent_yvel_sub,y          ; clear Y vel sub-pixel
        sta     ent_yvel,y              ; clear Y velocity
        sta     ent_xvel_sub,y          ; clear X vel sub-pixel
        sta     ent_xvel,y              ; clear X velocity
        lda     #$FB                    ; AI routine $FB
        sta     ent_routine,y           ; set block Y-update AI
        lda     #FACING_RIGHT           ; facing right
        sta     ent_facing,y            ; block scatters rightward
        ldy     #$07                    ; 8 palette entries
wily_machine_c_pal_loop:  lda     wily_machine_c_pal_table,y ; load palette data
        sta     $0610,y                 ; write sprite palette 1
        sta     $0630,y                 ; write sprite palette 2
        dey                             ; next palette entry
        bpl     wily_machine_c_pal_loop ; loop all 8 entries
        sty     palette_dirty           ; Y=$FF → trigger palette DMA
wily_machine_c_return:  rts

wily_machine_c_y_pos_table:  .byte   $A8,$A8,$B8,$B8
wily_machine_c_xvel_table:  .byte   $F8,$08,$F8,$08
wily_machine_c_facing_table:  .byte   $02,$01,$02,$01
wily_machine_c_flags_table:  .byte   $02,$02,$01,$01
wily_machine_c_block_fall:  jsr     apply_y_speed ; apply vertical velocity
        lda     ent_y_px,x              ; current Y position
        cmp     #$B8                    ; below floor ($B8)?
        bcc     wily_machine_c_block_move ; above → continue moving
        lda     #$00                    ; despawn block
        sta     ent_status,x            ; remove block entity
        rts                             ; return after despawn

wily_machine_c_block_move:  lda     ent_facing,x ; check facing direction
        and     #FACING_RIGHT           ; bit 0: 1=right, 0=left
        beq     wily_machine_c_block_left ; even → move left
        jmp     move_sprite_right       ; move block rightward

wily_machine_c_block_left:  jmp     move_sprite_left ; move block leftward

wily_machine_c_block_y_update:  jsr     apply_y_speed ; apply vertical velocity
        lda     ent_facing,x            ; check facing direction
        and     #$02                    ; bit 1: phase flag
        beq     wily_machine_c_block_y_check ; phase 0 → check Y limit
        lda     #$B4                    ; Y limit = $B4
        cmp     ent_y_px,x              ; past limit?
        bcs     wily_machine_c_block_facing ; above → continue lateral move
        lda     #$00                    ; despawn this block
        sta     ent_status,x            ; despawn this scatter block
        lda     #$81                    ; reactivate player entity
        sta     ent_status              ; reactivate player entity
        lda     #$00                    ; despawn Wily Machine parent
        ldy     ent_var3                ; var3 = saved parent slot
        sta     ent_status,y            ; despawn parent entity
        sta     ent_timer               ; clear player timer
        lda     #PSTATE_TELEPORT        ; state → $0D (teleport)
        sta     player_state            ; player teleports away (end stage)
        lda     ent_flags               ; player flags
        and     #$FB                    ; clear bit 2 (unfreeze)
        sta     ent_flags               ; unfreeze player movement
        rts                             ; return after stage end

wily_machine_c_block_y_check:  lda     #$94 ; Y limit = $94
        cmp     ent_y_px,x              ; past limit?
        bcs     wily_machine_c_block_anim_end ; above → done for this frame
        sta     ent_y_px,x              ; clamp Y to $94
        lda     ent_anim_id,x           ; current anim ID
        cmp     #$7A                    ; $7A = block idle
        beq     wily_machine_c_block_timer ; idle → run timer
        cmp     #$7B                    ; $7B = block breaking
        bne     wily_machine_c_block_anim ; other → set idle anim
        lda     #$7A                    ; set to idle anim $7A
        jsr     reset_sprite_anim       ; reset animation
        lda     #$00                    ; clear flash state
        sta     $B8                     ; clear palette flash flag
wily_machine_c_block_anim:  lda     ent_anim_state,x ; check anim state
        cmp     #$04                    ; state 4 = last frame?
        bne     wily_machine_c_block_end ; not last → continue
        lda     #$7B                    ; set breaking anim $7B
        jsr     reset_sprite_anim       ; set breaking animation
        lda     #$A3                    ; Y velocity sub = $A3
        sta     ent_yvel_sub,x          ; set launch Y vel sub
        lda     #$04                    ; Y velocity = 4 (launch up)
        sta     ent_yvel,x              ; set upward launch speed
        lda     #$9B                    ; X velocity sub = $9B
        sta     ent_xvel_sub,x          ; set lateral X vel sub
        lda     #$02                    ; X velocity = 2
        sta     ent_xvel,x              ; set lateral scatter speed
wily_machine_c_block_anim_end:  lda     #$00 ; freeze at frame 0
        sta     ent_anim_frame,x        ; hold animation at first frame
wily_machine_c_block_facing:  lda     ent_facing,x ; check lateral direction
        and     #FACING_RIGHT           ; bit 0: 1=right, 0=left
        beq     wily_machine_c_block_left_move ; even → move left
        jmp     move_sprite_right       ; move block rightward

wily_machine_c_block_left_move:  jmp     move_sprite_left ; move block leftward

wily_machine_c_block_timer:  inc     ent_timer,x ; increment idle timer
        lda     ent_timer,x             ; read current timer
        cmp     #$3C                    ; 60 frames = 1 second
        bne     wily_machine_c_block_end ; not yet → wait
        lda     $B8                     ; palette flash flag
        bne     wily_machine_c_block_pal ; nonzero → run pal effect
        dec     ent_timer,x             ; hold timer at $3B
        lda     #$11                    ; game mode $11 = ending
        sta     game_mode               ; trigger ending sequence
        lda     #$D0                    ; set ending timer = $D0
        sta     $5E                     ; set ending countdown timer
        lda     #$0A                    ; palette flash count = $0A
        sta     $B8                     ; init palette flash counter
        jmp     call_bank0E_A006        ; start palette flash effect

wily_machine_c_block_pal:  jsr     call_bank0E_A003 ; advance palette flash
        lda     $B8                     ; check flash counter
        cmp     #$FF                    ; $FF = flash complete
        beq     wily_machine_c_block_jump ; done → launch blocks
        dec     ent_timer,x             ; hold timer (keep flashing)
        rts                             ; return while flash continues

wily_machine_c_block_jump:  lda     #$A3 ; Y velocity sub = $A3
        sta     ent_yvel_sub,x          ; set launch Y vel sub
        lda     #$04                    ; Y velocity = 4 (launch up)
        sta     ent_yvel,x              ; launch block upward
        lda     #$7B                    ; breaking anim $7B
        jsr     reset_sprite_anim       ; set breaking animation
        lda     #FACING_LEFT            ; facing left (scatter dir)
        sta     ent_facing,x            ; set scatter direction
wily_machine_c_block_end:  rts

wily_machine_c_pal_table:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
kamegoro_maker_init:  lda     ent_hitbox,x ; save hitbox
        pha                             ; preserve on stack
        lda     #$00                    ; disable hitbox temporarily
        sta     ent_hitbox,x            ; clear contact damage
        jsr     entity_ai_dispatch      ; run collision + animation
        pla                             ; restore original hitbox
        sta     ent_hitbox,x            ; put hitbox back
        bcs     kamegoro_maker_mode_check ; if still alive, skip init
        lda     ent_hp,x                ; check if HP depleted
        bne     kamegoro_maker_mode_check ; if HP > 0, skip death
        sta     game_mode               ; reset game mode (death)
        sta     ent_hitbox,x            ; disable hitbox on death
        lda     #$02                    ; spawn 3 explosion effects
        sta     $01                     ; loop counter (0-2)
        lda     ent_x_px,x              ; base X position for spawns
        sta     $02                     ; store in temp
        lda     ent_y_px,x              ; base Y position for spawns
        sta     $03                     ; store in temp
kamegoro_maker_spawn_loop:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     kamegoro_maker_spawn_end ; no free slot, exit
        lda     #$71                    ; explosion entity type $71
        jsr     init_child_entity       ; spawn explosion child
        lda     #$00                    ; clear child timer
        sta     ent_timer,y             ; timer = 0
        sta     ent_hitbox,y            ; no hitbox on explosion
        lda     #$19                    ; AI routine $19
        sta     ent_routine,y           ; set child AI routine
        lda     ent_x_scr,x             ; copy screen from parent
        sta     ent_x_scr,y             ; to child X screen
        stx     $0F                     ; save parent slot
        ldx     $01                     ; use counter as index
        lda     $02                     ; base X position
        clc                             ; prepare for X offset add
        adc     kamegoro_maker_param_e,x ; add X offset from table
        sta     ent_x_px,y              ; set child X position
        lda     $03                     ; base Y position
        clc                             ; prepare for Y offset add
        adc     kamegoro_maker_param_f,x ; add Y offset from table
        sta     ent_y_px,y              ; set child Y position
        ldx     $0F                     ; restore parent slot
        dec     $01                     ; decrement spawn counter
        bpl     kamegoro_maker_spawn_loop ; loop until all 3 spawned
        ldy     #$0F                    ; 16 palette bytes
kamegoro_maker_pal_loop:  lda     kamegoro_maker_pal_table,y ; load boss palette color
        sta     $0600,y                 ; write to palette buffer
        dey                             ; next palette byte
        bpl     kamegoro_maker_pal_loop ; loop all 16 colors
        sty     palette_dirty           ; flag palette for upload
kamegoro_maker_spawn_end:  rts

kamegoro_maker_mode_check:  lda     game_mode ; check current game mode
        cmp     #$03                    ; mode < 3?
        bcc     kamegoro_maker_spawn_end ; not ready yet, return
        lda     ent_var1,x              ; check spawn delay timer
        beq     kamegoro_maker_phase_check ; 0 = ready for new phase
        dec     ent_var1,x              ; count down spawn delay
        bne     kamegoro_maker_jump     ; still counting, apply gravity
        lda     ent_var2,x              ; check burst spawn counter
        beq     kamegoro_maker_jump     ; no more bursts, gravity
        dec     ent_var2,x              ; decrement burst counter
        beq     kamegoro_maker_jump     ; done bursting, gravity
        lda     #$1E                    ; 30-frame delay between spawns
        sta     ent_var1,x              ; reset spawn delay
        jmp     kamegoro_maker_spawn_main ; spawn Kamegoro entity

kamegoro_maker_jump:  jmp     kamegoro_maker_gravity

kamegoro_maker_phase_check:  lda     ent_status,x ; get sub-phase
        and     #$0F                    ; check low nibble
        bne     kamegoro_maker_timer_start ; already initialized
        inc     ent_status,x            ; advance to phase 1
        sta     ent_yvel,x              ; clear Y velocity (A=0)
        lda     #$80                    ; Y velocity sub = $80
        sta     ent_yvel_sub,x          ; slow upward movement
        lda     #$88                    ; facing = $88 (up + flags)
        sta     ent_facing,x            ; set initial direction
        lda     #$F0                    ; timer = 240 frames
        sta     ent_timer,x             ; set movement duration
kamegoro_maker_timer_start:  lda     ent_timer,x ; check movement timer
        bne     kamegoro_maker_movement ; timer active, keep moving
        lda     ent_facing,x            ; get direction flags
        eor     #$0C                    ; toggle up/down bits
        and     #$0C                    ; isolate direction bits
        sta     ent_facing,x            ; update facing direction
        lda     $E4                     ; RNG seed
        adc     $E5                     ; advance RNG state
        sta     $E5                     ; store new RNG value
        and     #$03                    ; random 0-3
        tay                             ; use random value as index
        lda     kamegoro_maker_timer_table,y ; look up timer duration
        sta     ent_timer,x             ; set new movement timer
        lda     #$00                    ; reset camera X
        sta     camera_x_lo             ; lock scroll position
kamegoro_maker_movement:  lda     ent_facing,x ; get direction flags
        and     #$04                    ; test bit 2 (down flag)
        bne     kamegoro_maker_move_down ; if set, move down
        jsr     move_sprite_up          ; move maker upward
        lda     #$48                    ; upper Y limit = $48
        cmp     ent_y_px,x              ; reached top boundary?
        bcc     kamegoro_maker_timer_dec ; no, keep moving
        sta     ent_y_px,x              ; clamp to upper limit
        bcs     kamegoro_maker_facing   ; reverse direction
kamegoro_maker_move_down:  jsr     move_sprite_down ; move maker downward
        lda     #$80                    ; lower Y limit = $80
        cmp     ent_y_px,x              ; reached bottom boundary?
        bcs     kamegoro_maker_timer_dec ; no, keep moving
        sta     ent_y_px,x              ; clamp to lower limit
kamegoro_maker_facing:  lda     ent_facing,x ; get current direction
        eor     #$0C                    ; toggle up/down bits
        sta     ent_facing,x            ; reverse vertical dir
kamegoro_maker_timer_dec:  dec     ent_timer,x ; count down timer
        bne     kamegoro_maker_gravity  ; timer not zero, continue
        lda     #$1E                    ; 30-frame spawn delay
        sta     ent_var1,x              ; set delay before spawn
        lda     $E4                     ; RNG seed
        adc     $E6                     ; advance RNG state
        sta     $E4                     ; store updated RNG
        and     #$01                    ; random 0 or 1
        beq     kamegoro_maker_var2_init ; 0 = spawn Kamegoro turtle
        jsr     kamegoro_maker_spawn_pellet ; 1 = spawn pellet spread
        jmp     kamegoro_maker_gravity  ; skip turtle spawn path

kamegoro_maker_var2_init:  lda     #$03 ; 3 burst spawns
        sta     ent_var2,x              ; set burst counter
        jsr     kamegoro_maker_spawn_main ; spawn first Kamegoro
kamegoro_maker_gravity:  lda     ent_y_px,x ; get Y position
        sec                             ; subtract $D0 offset
        sbc     #$D0                    ; subtract screen base offset
        clc                             ; adjust for screen bounds
        adc     #$AF                    ; map to offscreen threshold
        sta     $5E                     ; store offscreen check result
        rts                             ; return after gravity update

kamegoro_maker_spawn_pellet:  lda     #$02 ; spawn 3 pellets
        sta     $01                     ; loop counter (0-2)
kamegoro_maker_pellet_loop:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     kamegoro_maker_pellet_end ; no free slot, exit
        lda     #$6F                    ; pellet entity type $6F
        jsr     init_child_entity       ; spawn pellet child
        lda     #$80                    ; contact damage, no HP box
        sta     ent_hitbox,y            ; set pellet hitbox
        lda     #$00                    ; pellets have 0 HP
        sta     ent_hp,y                ; indestructible projectile
        lda     #$0F                    ; AI routine $0F
        sta     ent_routine,y           ; set pellet AI routine
        lda     ent_x_px,x              ; copy parent X pixel
        sta     ent_x_px,y              ; to pellet X pixel
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to pellet X screen
        lda     ent_y_px,x              ; parent Y position
        clc                             ; prepare for Y offset add
        adc     #$30                    ; offset down by 48 px
        sta     ent_y_px,y              ; set pellet Y position
        lda     #FACING_LEFT            ; face left
        sta     ent_facing,y            ; pellets fire leftward
        stx     temp_00                 ; save parent slot
        ldx     $01                     ; use counter as param index
        lda     kamegoro_maker_param_a,x ; X velocity sub-pixel
        sta     ent_xvel_sub,y          ; set pellet X vel sub
        lda     kamegoro_maker_param_b,x ; X velocity whole pixel
        sta     ent_xvel,y              ; set pellet X velocity
        lda     kamegoro_maker_param_c,x ; Y velocity sub-pixel
        sta     ent_yvel_sub,y          ; set pellet Y vel sub
        lda     kamegoro_maker_param_d,x ; Y velocity whole pixel
        sta     ent_yvel,y              ; set pellet Y velocity
        ldx     temp_00                 ; restore parent slot
        dec     $01                     ; decrement pellet counter
        bpl     kamegoro_maker_pellet_loop ; loop until all 3 spawned
kamegoro_maker_pellet_end:  rts

kamegoro_maker_spawn_main:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     kamegoro_maker_pellet_end ; no slot available, return
        lda     #$1D                    ; Kamegoro entity type $1D
        jsr     init_child_entity       ; spawn Kamegoro turtle
        lda     #$C0                    ; contact dmg + HP bar visible
        sta     ent_hitbox,y            ; set hitbox
        lda     #$6D                    ; AI routine $6D
        sta     ent_routine,y           ; set turtle AI routine
        lda     ent_x_px,x              ; copy parent X position
        sta     ent_x_px,y              ; to turtle X position
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to turtle X screen
        lda     ent_y_px,x              ; parent Y position
        clc                             ; prepare for Y offset add
        adc     #$30                    ; offset 48 px below maker
        sta     ent_y_px,y              ; set turtle Y position
        lda     #$AB                    ; Y vel sub = $AB (upward)
        sta     ent_yvel_sub,y          ; launch upward fast
        lda     #$FF                    ; Y vel = -1 (upward)
        sta     ent_yvel,y              ; strong upward velocity
        lda     #$00                    ; clear X vel sub-pixel
        sta     ent_xvel_sub,y          ; no horizontal sub-vel
        lda     #$02                    ; X vel = 2 px/frame right
        sta     ent_xvel,y              ; launched rightward
        lda     #$01                    ; 1 HP
        sta     ent_hp,y                ; turtle takes 1 hit
        rts                             ; return after Kamegoro spawn

kamegoro_maker_timer_table:  .byte   $1E,$3C,$3C,$5A
kamegoro_maker_param_a:  .byte   $B5,$00,$B5
kamegoro_maker_param_b:  .byte   $00,$01,$00
kamegoro_maker_param_c:  .byte   $4B,$00,$B5
kamegoro_maker_param_d:  .byte   $FF,$00,$00
kamegoro_maker_param_e:  .byte   $00,$F0,$10
kamegoro_maker_param_f:  .byte   $00,$10,$10
kamegoro_maker_pal_table:  .byte   $0F,$20,$27,$17,$0F,$03,$12,$0F
        .byte   $0F,$2B,$1B,$0B,$0F,$22,$12,$02
kamegoro_current_init:  lda     ent_status,x ; get sub-phase counter
        and     #$0F                    ; isolate low nibble
        bne     kamegoro_current_phase_check ; already initialized
        lda     #$09                    ; PSTATE_BOSS_WAIT
        cmp     player_state            ; player already waiting?
        beq     kamegoro_current_hp_check ; skip if already in boss wait
        sta     player_state            ; freeze player for boss intro
        lda     #$80                    ; enable boss HP bar display
        sta     boss_hp_display         ; show boss HP bar
        sta     boss_active             ; activate boss fight flag
        lda     #$8E                    ; boss HP display target
        sta     $B3                     ; set max boss HP ($0E = 14)
        lda     #MUSIC_BOSS             ; play boss music
        jsr     submit_sound_ID_D9      ; if not already playing
kamegoro_current_hp_check:  lda     boss_hp_display ; check HP bar fill status
        cmp     #HEALTH_FULL            ; fully filled?
        bne     kamegoro_current_return ; still filling, wait
        jsr     kamegoro_current_death_counter ; randomize movement params
        inc     ent_status,x            ; advance to active phase
kamegoro_current_phase_check:  lda     ent_status,x ; get current status
        and     #$02                    ; check bit 1 (death phase)
        bne     kamegoro_current_return ; not in death phase
        lda     ent_anim_id,x           ; get animation ID
        cmp     #$4F                    ; shell-open anim $4F?
        beq     kamegoro_current_anim_check ; check if anim finished
        lda     ent_var1,x              ; check death flag
        bne     kamegoro_current_death_init ; nonzero = children active
        lda     ent_var2,x              ; check spawn count
        cmp     #$05                    ; spawned 5 or more?
        bcs     kamegoro_current_status_inc ; max spawns reached
        lda     ent_timer,x             ; check movement timer
        bne     kamegoro_current_movement ; timer expired, open shell
        lda     #$4F                    ; shell-open animation $4F
        jsr     reset_sprite_anim       ; switch to opening anim
kamegoro_current_anim_check:  lda     ent_anim_state,x ; check anim completion
        cmp     #$02                    ; state 2 = anim done
        bne     kamegoro_current_return ; still animating, wait
        jsr     kamegoro_current_spawn_entity ; spawn child from shell
        inc     ent_var2,x              ; increment spawn counter
        lda     #$31                    ; closed-shell animation $31
        jsr     reset_sprite_anim       ; switch to closed anim
        inc     ent_var1,x              ; flag children alive
kamegoro_current_movement:  lda     ent_facing,x ; check horizontal direction
        and     #FACING_RIGHT           ; test right-facing bit
        beq     kamegoro_current_move_left_col ; 0 = facing left
        ldy     #$20                    ; right collision box $20
        jsr     move_right_collide      ; move right with collision
        jmp     kamegoro_current_move_check ; check for wall collision

kamegoro_current_move_left_col:  ldy     #$21 ; left collision box $21
        jsr     move_left_collide       ; move left with collision
kamegoro_current_move_check:  bcc     kamegoro_current_timer_dec ; no collision, count down
        lda     ent_facing,x            ; hit wall, flip direction
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x            ; reverse horizontal facing
kamegoro_current_timer_dec:  dec     ent_timer,x ; count down move timer
        rts                             ; return after timer tick

kamegoro_current_status_inc:  inc     ent_status,x ; max spawns, advance phase
kamegoro_current_return:  rts

kamegoro_current_death_init:  lda     #$00 ; start child scan
        sta     temp_00                 ; clear live child count
        lda     #$80                    ; Kamegoro maker spawn ID $80
        sta     $01                     ; store for comparison
        ldy     #$1F                    ; scan slots $10-$1F
kamegoro_current_death_loop:  lda     ent_status,y ; check if slot active
        bmi     kamegoro_current_death_filter ; active, check spawn ID
kamegoro_current_death_next:  dey
        cpy     #$0F                    ; check slot $10
        bne     kamegoro_current_death_loop ; loop all enemy slots
        lda     temp_00                 ; get live child count
        bne     kamegoro_current_death_anim ; children still alive
        lda     #$00                    ; all children defeated
        sta     ent_var1,x              ; clear death flag
        lda     ent_var2,x              ; get total spawn count
        tay                             ; use spawn count as index
        lda     boss_hp_display         ; current HP bar value
        sec                             ; prepare for HP subtraction
        sbc     kamegoro_current_timer_table,y ; subtract HP for this wave
        sta     boss_hp_display         ; update HP display
        and     #$1F                    ; isolate HP value bits
        bne     kamegoro_current_death_anim ; HP = 0 means boss dead
        jmp     entity_ai_defeat        ; trigger boss defeat

kamegoro_current_death_anim:  lda     #$31 ; closed-shell animation
        jsr     reset_sprite_anim       ; return to closed pose
        jsr     kamegoro_current_death_counter ; re-randomize next timer
        rts                             ; return after shell reset

kamegoro_current_death_filter:  lda     $01 ; get spawn ID to match
        cmp     ent_spawn_id,y          ; is this a Kamegoro child?
        bne     kamegoro_current_death_next ; no match, skip slot
        inc     temp_00                 ; count this live child
        jmp     kamegoro_current_death_next ; continue scanning slots

kamegoro_current_death_counter:  lda     $E4 ; RNG seed
        adc     $E5                     ; advance RNG
        sta     $E5                     ; store updated RNG
        and     #$03                    ; random 0-3
        tay                             ; use random value as index
        lda     kamegoro_current_pos_table,y ; look up move timer value
        sta     ent_timer,x             ; set movement timer
        lda     kamegoro_current_spawn_table,y ; look up facing direction
        sta     ent_facing,x            ; set move direction
        rts                             ; return after timer/facing set

kamegoro_current_pos_table:  .byte   $40,$A0,$70,$D0
kamegoro_current_spawn_table:  .byte   $01,$02,$01,$02
kamegoro_current_timer_table:  .byte   $01,$02,$03,$05,$08,$0A
kamegoro_current_spawn_entity:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     kamegoro_current_spawn_return ; no slot, return
        sty     temp_00                 ; save child slot index
        lda     ent_x_px,x              ; copy parent X pixel
        sta     ent_x_px,y              ; to child X pixel
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to child X screen
        lda     ent_y_px,x              ; parent Y position
        clc                             ; prepare for Y offset add
        adc     #$18                    ; offset 24 px below shell
        sta     ent_y_px,y              ; set child Y position
        lda     #$C2                    ; hitbox $C2 (dmg + shape)
        sta     ent_hitbox,y            ; set child hitbox
        lda     #$F1                    ; AI routine $F1
        sta     ent_routine,y           ; set child AI routine
        lda     #$03                    ; 3 HP per child
        sta     ent_hp,y                ; set child HP
        lda     #$80                    ; spawn ID = $80 (Kamegoro)
        sta     ent_spawn_id,y          ; tag as Kamegoro child
        lda     ent_var2,x              ; copy wave count to child
        sta     ent_var2,y              ; child knows its wave index
        sta     $02                     ; save wave index to temp
        tay                             ; use as table index
        lda     kamegoro_current_spawn_timer_a,y ; look up facing for wave
        ldy     temp_00                 ; restore child slot
        sta     ent_facing,y            ; set child facing
        ldy     $02                     ; reload wave index
        lda     kamegoro_current_spawn_facing,y ; look up X vel sub for wave
        ldy     temp_00                 ; restore child slot
        sta     ent_xvel_sub,y          ; set child X vel sub
        sta     ent_yvel_sub,y          ; same value for Y vel sub
        ldy     $02                     ; reload wave index
        lda     kamegoro_current_spawn_timer_b,y ; look up velocity for wave
        ldy     temp_00                 ; restore child slot
        sta     ent_xvel,y              ; set child X velocity
        sta     ent_yvel,y              ; same value for Y velocity
        lda     #$5E                    ; entity OAM type $5E
        jsr     init_child_entity       ; init child sprite/status
        lda     ent_facing,y            ; check child facing
        and     #FACING_RIGHT           ; test right-facing bit
        bne     kamegoro_current_spawn_return ; facing right, keep H-flip
        lda     ent_flags,y             ; facing left, clear H-flip
        and     #$BF                    ; clear bit 6 (H-flip)
        sta     ent_flags,y             ; update child flags
kamegoro_current_spawn_return:  rts

kamegoro_current_spawn_timer_a:  .byte   $06,$05,$05,$06,$05,$06
kamegoro_current_spawn_facing:  .byte   $80,$00,$80,$00,$00,$00
kamegoro_current_spawn_timer_b:  .byte   $00,$01,$01,$02,$03,$04
kamegoro_current_phase_init:  lda     ent_status,x ; get sub-phase
        and     #$0F                    ; isolate low nibble
        bne     kamegoro_current_phase_status ; already initialized
        sta     ent_var1,x              ; clear var1
        lda     #$78                    ; timer = 120 frames
        sta     ent_timer,x             ; set initial move timer
        inc     ent_status,x            ; advance to phase 1
kamegoro_current_phase_status:  lda     ent_status,x ; get current status
        and     #$02                    ; check bit 1 (launched)
        beq     kamegoro_current_move_routine ; not launched, walk
        jmp     kamegoro_current_launched ; handle launched/airborne

kamegoro_current_move_routine:  jsr     kamegoro_current_var1_spawn ; check if at spawn Y level
        lda     ent_timer,x             ; check move timer
        bne     kamegoro_current_timer_cmp ; timer active, keep moving
        lda     ent_y_px,x              ; get current Y position
        cmp     #$68                    ; above floor level $68?
        bcs     kamegoro_current_spawn_effect ; on floor, spawn effect
        lda     #$78                    ; new timer = 120 frames
        sta     ent_timer,x             ; reset movement timer
        bne     kamegoro_current_timer_dec_2 ; skip to timer decrement
kamegoro_current_spawn_effect:  jsr     kamegoro_current_effect_spawn_2 ; spawn landing/turn effect
        jsr     kamegoro_current_face_dir ; set facing toward player
        lda     #$FF                    ; timer = $FF (continuous)
        sta     ent_timer,x             ; set continuous movement
kamegoro_current_timer_cmp:  cmp     #$FF ; $FF = continuous mode?
        beq     kamegoro_current_move_dir ; skip decrement if $FF
kamegoro_current_timer_dec_2:  dec     ent_timer,x ; count down move timer
kamegoro_current_move_dir:  lda     ent_facing,x ; check facing direction
        and     #FACING_RIGHT           ; test right-facing bit
        beq     kamegoro_current_move_left ; 0 = facing left
        ldy     #$0C                    ; collision box $0C
        jsr     move_right_collide      ; move right with collision
        jmp     kamegoro_current_collision ; check wall collision

kamegoro_current_move_left:  ldy     #$0D ; collision box $0D
        jsr     move_left_collide       ; move left with collision
kamegoro_current_collision:  bcc     kamegoro_current_vert_check ; no wall hit, check vert
        lda     ent_facing,x            ; hit wall, flip direction
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x            ; reverse horizontal facing
        and     #$0C                    ; check vertical bits
        bne     kamegoro_current_return_end ; has vert direction, done
        lda     ent_facing,x            ; no vert direction yet
        ora     #$08                    ; set upward bit
        sta     ent_facing,x            ; add climb after wall hit
        rts                             ; done after wall reversal

kamegoro_current_vert_check:  lda     ent_facing,x ; check vertical direction
        and     #$0C                    ; isolate vert bits (up/down)
        beq     kamegoro_current_return_end ; no vert movement, done
        and     #$04                    ; test bit 2 (down flag)
        beq     kamegoro_current_facing_check ; 0 = moving up
        lda     ent_timer,x             ; check if continuous mode
        cmp     #$FF                    ; timer $FF = continuous?
        beq     kamegoro_current_anim_type2 ; use alt anim in continuous
        lda     #$5E                    ; normal crawl-down anim
        bne     kamegoro_current_anim_set ; always branch (A != 0)
kamegoro_current_anim_type2:  lda     #$62 ; continuous crawl-down anim
kamegoro_current_anim_set:  sta     ent_anim_id,x ; set animation ID
        ldy     #$0E                    ; collision box $0E (down)
        jsr     move_down_collide       ; move down with collision
        jmp     kamegoro_current_col_return ; check floor collision

kamegoro_current_facing_check:  lda     ent_facing,x ; check facing for sprite flip
        and     #FACING_RIGHT           ; test right-facing bit
        beq     kamegoro_current_flag_clear ; 0 = facing left
        lda     ent_flags,x             ; facing right, clear H-flip
        and     #$BF                    ; clear bit 6
        sta     ent_flags,x             ; update flags (no H-flip)
        bne     kamegoro_current_flag_check ; always branch (flags != 0)
kamegoro_current_flag_clear:  lda     ent_flags,x ; facing left, set H-flip
        ora     #ENT_FLAG_HFLIP         ; set bit 6 (H-flip)
        sta     ent_flags,x             ; update flags (H-flipped)
kamegoro_current_flag_check:  lda     ent_timer,x ; check if continuous mode
        cmp     #$FF                    ; timer $FF = continuous?
        beq     kamegoro_current_anim_up ; use alt anim in continuous
        lda     #$60                    ; normal crawl-up anim
        bne     kamegoro_current_anim_down ; always branch (A != 0)
kamegoro_current_anim_up:  lda     #$64 ; continuous crawl-up anim
kamegoro_current_anim_down:  sta     ent_anim_id,x ; set animation ID
        ldy     #$0F                    ; collision box $0F (up)
        jsr     move_up_collide         ; move up with collision
kamegoro_current_col_return:  bcc     kamegoro_current_return_end ; no collision, done
        lda     ent_facing,x            ; hit ceiling/floor
        eor     #$0C                    ; toggle up/down bits
        sta     ent_facing,x            ; reverse vertical direction
kamegoro_current_return_end:  rts

kamegoro_current_launched:  lda     ent_var3,x ; check if velocity set
        bne     kamegoro_current_gravity ; already initialized
        lda     ent_var2,x              ; get wave index
        tay                             ; use as table index
        lda     kamegoro_current_spawn_id_table,y ; look up Y vel sub
        sta     ent_yvel_sub,x          ; set launch Y vel sub
        lda     kamegoro_current_spawn_type,y ; look up Y velocity
        sta     ent_yvel,x              ; set launch Y velocity
        lda     kamegoro_current_facing_table,y ; look up X vel sub
        sta     ent_xvel_sub,x          ; set launch X vel sub
        lda     kamegoro_current_speed_table,y ; look up X velocity
        sta     ent_xvel,x              ; set launch X velocity
        inc     ent_var3,x              ; mark velocity initialized
kamegoro_current_gravity:  ldy     #$0F ; collision box $0F
        jsr     move_vertical_gravity   ; apply gravity + move vert
        lda     $10                     ; get collision result
        and     #$10                    ; test TILE_SOLID bit
        beq     kamegoro_current_move_dir_2 ; no solid tile, move horiz
        jmp     kamegoro_current_status_dec ; landed, return to walking

kamegoro_current_move_dir_2:  lda     ent_facing,x ; check facing direction
        and     #FACING_RIGHT           ; test right-facing bit
        beq     kamegoro_current_move_left_2 ; 0 = facing left
        ldy     #$0C                    ; collision box $0C
        jsr     move_right_collide      ; move right with collision
        lda     ent_flags,x             ; clear H-flip for right
        and     #$BF                    ; clear bit 6
        sta     ent_flags,x             ; update flags
        jmp     kamegoro_current_col_end ; check for wall collision

kamegoro_current_move_left_2:  ldy     #$0D ; collision box $0D
        jsr     move_left_collide       ; move left with collision
        lda     ent_flags,x             ; set H-flip for left
        ora     #ENT_FLAG_HFLIP         ; set bit 6
        sta     ent_flags,x             ; update flags
kamegoro_current_col_end:  bcc     kamegoro_current_vel_check ; no wall hit, check Y vel
kamegoro_current_status_dec:  dec     ent_status,x ; wall/floor hit, end launch
        lda     #$00                    ; clear var1
        sta     ent_var1,x              ; reset death flag
        sta     ent_var3,x              ; clear launch init flag
        lda     ent_facing,x            ; get facing
        eor     #$0C                    ; toggle vertical bits
        sta     ent_facing,x            ; reverse vert direction
        lda     ent_var2,x              ; get wave index
        tay                             ; use as table index
        lda     kamegoro_current_spawn_facing,y ; look up walk vel sub
        sta     ent_yvel_sub,x          ; restore Y vel sub
        sta     ent_xvel_sub,x          ; restore X vel sub
        lda     kamegoro_current_spawn_timer_b,y ; look up walk velocity
        sta     ent_yvel,x              ; restore Y velocity
        sta     ent_xvel,x              ; restore X velocity
        rts                             ; return to walking mode

kamegoro_current_vel_check:  lda     ent_yvel,x ; check Y velocity sign
        bpl     kamegoro_current_return_final ; moving down, done
        lda     ent_y_px,x              ; get Y position
        cmp     #$20                    ; above screen top ($20)?
        bcs     kamegoro_current_status_dec ; too high, end launch
kamegoro_current_return_final:  rts

kamegoro_current_spawn_id_table:  .byte   $A2,$4F,$B4,$44,$00,$9E
kamegoro_current_spawn_type:  .byte   $01,$02,$02,$03,$04,$04
kamegoro_current_facing_table:  .byte   $00,$80,$00,$80,$00,$80
kamegoro_current_speed_table:  .byte   $01,$01,$02,$02,$03,$03
kamegoro_current_var1_spawn:  lda     ent_var1,x ; check spawn trigger flag
        bne     kamegoro_current_y_check ; already past spawn Y
        lda     ent_y_px,x              ; get Y position
        cmp     #$45                    ; above spawn threshold $45?
        bcc     kamegoro_current_spawn_ret_a ; not deep enough yet
        jsr     kamegoro_current_effect_spawn ; spawn water splash effect
        lda     ent_var2,x              ; get wave index
        tay                             ; use as table index
        lda     kamegoro_current_spawn_facing,y ; look up vel sub for wave
        sta     ent_yvel_sub,x          ; set Y vel sub (slow down)
        sta     ent_xvel_sub,x          ; same for X vel sub
        lda     kamegoro_current_spawn_timer_b,y ; look up velocity for wave
        sta     ent_yvel,x              ; set Y velocity
        sta     ent_xvel,x              ; set X velocity
        inc     ent_var1,x              ; mark spawn Y reached
kamegoro_current_spawn_ret_a:  rts

kamegoro_current_y_check:  lda     ent_y_px,x ; get Y position
        cmp     #$45                    ; still below threshold $45?
        bcs     kamegoro_current_spawn_ret_a ; yes, keep moving
        jsr     kamegoro_current_effect_spawn ; spawn surface effect
        lda     #$00                    ; clear var1
        sta     ent_var1,x              ; reset spawn trigger
        sta     ent_var3,x              ; clear launch init flag
        inc     ent_status,x            ; advance to launched phase
        rts                             ; done, will launch next frame

kamegoro_current_face_dir:  lda     ent_facing,x ; get facing direction
        and     #$0C                    ; isolate vert bits
        beq     kamegoro_current_horiz_check ; no vert, check horizontal
        lda     #$62                    ; vertical crawl-down anim
        sta     ent_anim_id,x           ; set animation ID
        rts                             ; return with vert anim set

kamegoro_current_horiz_check:  lda     ent_facing,x ; get facing direction
        and     #FACING_RIGHT           ; test right-facing bit
        beq     kamegoro_current_facing_right ; 0 = facing left
        lda     ent_flags,x             ; facing right, clear H-flip
        and     #$BF                    ; clear bit 6
        sta     ent_flags,x             ; update flags
        bne     kamegoro_current_anim_id_set ; always branch (flags != 0)
kamegoro_current_facing_right:  lda     ent_flags,x ; facing left, set H-flip
        ora     #ENT_FLAG_HFLIP         ; set bit 6 (H-flip)
        sta     ent_flags,x             ; update flags
kamegoro_current_anim_id_set:  lda     #$64 ; horizontal crawl-up anim
        sta     ent_anim_id,x           ; set animation ID
        rts                             ; return with horiz anim set

kamegoro_current_effect_spawn:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     kamegoro_current_effect_return ; no slot, return
        sty     temp_00                 ; save child slot index
        lda     ent_x_px,x              ; copy parent X pixel
        sta     ent_x_px,y              ; to child X pixel
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to child X screen
        lda     ent_y_px,x              ; parent Y position
        sta     ent_y_px,y              ; copy to child Y
        lda     ent_y_px,x              ; parent Y position again
        sec                             ; prepare for subtraction
        sbc     #$0C                    ; offset 12 px upward
        sta     ent_y_px,y              ; effect above turtle
        lda     #$00                    ; no AI routine
        sta     ent_routine,y           ; clear child routine
        sta     ent_hitbox,y            ; no hitbox
        sta     ent_hp,y                ; no HP (visual only)
        lda     #$67                    ; splash effect type $67
        jsr     init_child_entity       ; spawn splash effect
kamegoro_current_effect_return:  rts

        .byte   $00,$00,$00,$00
kamegoro_current_effect_spawn_2:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     kamegoro_current_effect_ret_2 ; no slot, return
        sty     temp_00                 ; save child slot index
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y            ; to child facing
        lda     ent_x_px,x              ; copy parent X pixel
        sta     ent_x_px,y              ; to child X pixel
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to child X screen
        lda     ent_y_px,x              ; copy parent Y pixel
        sta     ent_y_px,y              ; to child Y pixel
        lda     #$F2                    ; AI routine $F2
        sta     ent_routine,y           ; set child AI routine
        lda     #$CA                    ; hitbox $CA (dmg + shape)
        sta     ent_hitbox,y            ; set child hitbox
        lda     #$01                    ; 1 HP
        sta     ent_hp,y                ; set child HP
        lda     #$66                    ; entity OAM type $66
        jsr     init_child_entity       ; init child sprite/status
        lda     ent_facing,y            ; check child facing
        and     #FACING_RIGHT           ; test right-facing bit
        bne     kamegoro_current_effect_ret_2 ; facing right, keep H-flip
        lda     ent_flags,y             ; facing left, clear H-flip
        and     #$BF                    ; clear bit 6
        sta     ent_flags,y             ; update child flags
kamegoro_current_effect_ret_2:  rts

holograph_block_init:  lda     ent_status,x ; check sub-state
        and     #$0F                    ; mask low nibble
        bne     holograph_block_collision ; already initialized?
        sta     ent_yvel,x              ; clear Y velocity
        sta     ent_var2,x              ; clear var2 (phase flag)
        lda     #$80                    ; half-pixel Y sub-velocity
        sta     ent_yvel_sub,x          ; set Y sub-velocity
        lda     #$32                    ; 50 frames initial rise time
        sta     ent_timer,x             ; set initial rise timer
        lda     #$F0                    ; 240 frame lifetime
        sta     ent_var1,x              ; set block lifetime
        inc     ent_status,x            ; advance to active state
holograph_block_collision:  jsr     check_sprite_weapon_collision ; check if weapon hit block
        bcs     holograph_block_damage  ; no hit — take damage path
        lda     #SFX_ENEMY_HIT          ; play enemy hit sound
        jsr     submit_sound_ID         ; queue the sound effect
        ldy     $10                     ; Y = weapon slot that hit
        lda     #$00                    ; deactivate the weapon
        sta     ent_status,y            ; destroy the weapon entity
        jmp     holograph_block_death   ; block destroyed by weapon

holograph_block_damage:  lda     ent_status,x ; check sub-state bits
        and     #$02                    ; bit 1 = oscillation phase
        bne     holograph_block_facing  ; oscillation started?
        jsr     move_sprite_up          ; move block upward
        dec     ent_timer,x             ; count down rise timer
        bne     holograph_block_return  ; still rising?
        lda     #$02                    ; 2 frames per oscillation
        sta     ent_timer,x             ; set oscillation timer
        inc     ent_status,x            ; enter oscillation phase
holograph_block_return:  rts

holograph_block_facing:  lda     ent_facing,x ; check oscillation direction
        and     #$01                    ; bit 0 = down direction
        bne     holograph_block_move_down ; moving down?
        jsr     move_sprite_up          ; still moving up
        jmp     holograph_block_timer_dec ; skip to timer decrement

holograph_block_move_down:  jsr     move_sprite_down ; move block downward
holograph_block_timer_dec:  dec     ent_timer,x ; count down oscillation
        bne     holograph_block_var1_dec ; timer not expired?
        lda     ent_facing,x            ; get current direction
        eor     #$03                    ; toggle bits 0+1 (reverse)
        sta     ent_facing,x            ; flip oscillation direction
        lda     #$04                    ; 4 frames per direction
        sta     ent_timer,x             ; reset oscillation timer
holograph_block_var1_dec:  dec     ent_var1,x ; count down lifetime
        bne     holograph_block_var2_check ; still alive?
        lda     #$90                    ; set active + child flags
        sta     ent_flags,x             ; prepare for death anim
holograph_block_death:  lda     #$59    ; explosion anim ID
        jsr     reset_sprite_anim       ; play explosion animation
        lda     #$00                    ; clear timer
        sta     ent_timer,x             ; reset timer for death
        lda     #$19                    ; death/explosion routine
        sta     ent_routine,x           ; switch to death handler
        rts                             ; done, block is destroyed

holograph_block_var2_check:  lda     ent_var2,x ; check phase flag
        bne     holograph_block_var1_check ; not in flicker phase yet?
        lda     ent_var1,x              ; check remaining lifetime
        cmp     #$02                    ; less than 2 frames left?
        bcs     holograph_block_repeat_return ; still has time remaining
        lda     #$F2                    ; reset lifetime to 242
        sta     ent_var1,x              ; extend lifetime for flicker
        inc     ent_var2,x              ; enter flicker phase
holograph_block_repeat_return:  rts

holograph_block_var1_check:  lda     ent_var1,x ; check remaining lifetime
        cmp     #$78                    ; past flicker threshold?
        bcs     holograph_block_repeat_return ; not flickering yet
        lda     ent_flags,x             ; get entity flags
        eor     #$04                    ; toggle visibility bit
        sta     ent_flags,x             ; update visibility state
        rts                             ; done, flicker applied

holograph_main_init:  lda     ent_status,x ; check sub-state
        and     #$0F                    ; mask low nibble
        bne     holograph_phase_check   ; already initialized?
        jsr     holograph_timer_init    ; set spawn timer + position
        lda     #$3C                    ; 60 frame initial delay
        sta     ent_var3,x              ; set initial wait timer
        inc     ent_status,x            ; advance to waiting state
holograph_phase_check:  lda     ent_status,x ; check sub-state
        and     #$02                    ; bit 1 = spawning active
        bne     holograph_timer_dec     ; spawning started?
        dec     ent_var3,x              ; count down initial delay
        bne     holograph_return        ; still waiting?
        inc     ent_status,x            ; delay done, start spawning
holograph_timer_dec:  dec     ent_timer,x ; count down spawn timer
        bne     holograph_var1_check    ; time to spawn?
        jsr     holograph_spawn_entity  ; spawn a block/current
        jsr     holograph_timer_init    ; reset timer + position
        lda     #$94                    ; active + child + visible
        sta     ent_flags,x             ; update spawner visibility
        dec     ent_var1,x              ; one fewer to spawn
        rts                             ; done, spawned one entity

holograph_var1_check:  lda     ent_var1,x ; check spawn count
        bne     holograph_return        ; all spawned?
        lda     ent_timer,x             ; get spawn timer
        cmp     #$3C                    ; less than 60 frames left?
        bcs     holograph_return        ; still plenty of time
        lda     #$90                    ; active + child flags
        sta     ent_flags,x             ; hide spawner (done spawning)
        inc     ent_var1,x              ; prevent re-entry
holograph_return:  rts

holograph_timer_init:  lda     #$78     ; 120 frame spawn interval
        sta     ent_timer,x             ; set spawn interval timer
        lda     $E4                     ; PRNG seed low
        adc     $E5                     ; add to high byte
        sta     $E5                     ; store updated PRNG high
        and     #$03                    ; mask to 0-3
        cmp     #$02                    ; value >= 2?
        bcs     holograph_death_y_reset ; use vertical spawn path
        tay                             ; use as table index
        lda     holograph_spawn_param,y ; look up X offset
        sta     ent_x_px,x              ; set spawn X position
        lda     holograph_spawn_type,y  ; look up spawn direction
        sta     ent_facing,x            ; 1=right, 2=left
        lda     $E4                     ; advance PRNG
        adc     $E5                     ; add PRNG high byte
        sta     $E4                     ; store updated PRNG low
        and     #$0F                    ; mask to 0-15
        tay                             ; use as Y position index
        lda     holograph_y_pos_table,y ; look up Y position
        sta     ent_y_px,x              ; set spawn Y position
        rts                             ; done, horizontal spawn set

holograph_death_y_reset:  lda     #$CC  ; Y = $CC (bottom of screen)
        sta     ent_y_px,x              ; set Y to bottom of screen
        lda     $E4                     ; advance PRNG
        adc     $E5                     ; add PRNG high byte
        sta     $E5                     ; store updated PRNG high
        and     #$0F                    ; mask to 0-15
        tay                             ; use as X position index
        lda     holograph_y_pos_alt_table,y ; look up X position
        sta     ent_x_px,x              ; set spawn X position
        lda     #$08                    ; facing = $08 (upward)
        sta     ent_facing,x            ; set upward movement
        rts                             ; done, vertical spawn set

holograph_spawn_param:  .byte   $14,$EC
holograph_spawn_type:  .byte   $01,$02
holograph_y_pos_table:  .byte   $48,$58,$68,$78,$88,$98,$A8,$B8
        .byte   $88,$B8,$A8,$98,$88,$78,$68,$58
holograph_y_pos_alt_table:  .byte   $28,$38,$48,$58,$68,$78,$88,$98
        .byte   $A8,$B8,$C8,$D8,$B8,$A8,$98,$88
holograph_spawn_entity:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     holograph_spawn_return  ; no free slot?
        sty     temp_00                 ; save child slot index
        lda     ent_x_scr,x             ; copy parent screen
        sta     ent_x_scr,y             ; set child screen position
        lda     #$00                    ; clear HP (invincible)
        sta     ent_hp,y                ; child has no HP
        sta     ent_xvel_sub,y          ; clear X sub-velocity
        sta     ent_yvel_sub,y          ; clear Y sub-velocity
        lda     #$20                    ; small hitbox
        sta     ent_hitbox,y            ; set child hitbox size
        lda     ent_facing,x            ; check facing for type
        and     #$08                    ; bit 3 = vertical spawn
        bne     holograph_spawn_facing  ; vertical current?
        lda     #$5D                    ; horiz block entity ID $5D
        jsr     init_child_entity       ; init as child entity
        lda     ent_facing,x            ; inherit parent facing
        sta     ent_facing,y            ; set child facing direction
        and     #FACING_RIGHT           ; bit 0 = right side
        tay                             ; use as offset index (0 or 1)
        lda     ent_x_px,x              ; parent X position
        clc                             ; prepare for addition
        adc     holograph_spawn_param_2,y ; add directional offset
        ldy     temp_00                 ; restore child slot
        sta     ent_x_px,y              ; set child X position
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y              ; set child Y position
        jmp     holograph_spawn_routine ; finish spawn setup

holograph_spawn_facing:  lda     ent_facing,x ; copy facing to child
        sta     ent_facing,y            ; set child facing direction
        lda     #$5C                    ; vert current entity ID $5C
        jsr     init_child_entity       ; init as child entity
        lda     ent_x_px,x              ; copy parent X to child
        sta     ent_x_px,y              ; set child X position
        lda     ent_y_px,x              ; parent Y position
        sec                             ; offset 24px upward
        sbc     #$18                    ; subtract 24px offset
        sta     ent_y_px,y              ; set child Y position
holograph_spawn_routine:  lda     #$F4  ; current AI routine
        sta     ent_routine,y           ; set child AI routine
        lda     ent_flags,y             ; get child flags
        ora     #$02                    ; set invincibility bit
        sta     ent_flags,y             ; make child invincible
        lda     #$02                    ; 2px/frame X speed
        sta     ent_xvel,y              ; set child X velocity
        sta     ent_yvel,y              ; 2px/frame Y speed
holograph_spawn_return:  rts

holograph_spawn_param_2:  .byte   $E8,$18
holograph_current_dir_init:  lda     ent_facing,x ; check direction bits
        and     #$08                    ; bit 3 = vertical current
        beq     holograph_current_horiz ; horizontal current?
        ldy     #$09                    ; Y = 9 (speed param)
        jsr     move_up_collide         ; move current upward
        lda     ent_y_px,x              ; check Y position
        cmp     #$48                    ; above screen top?
        bcs     holograph_current_dist_check ; still on screen
        jmp     holograph_current_hit_check ; off screen, despawn

holograph_current_horiz:  lda     ent_facing,x ; check horizontal dir
        and     #FACING_RIGHT           ; bit 0 = rightward
        beq     holograph_current_move_left ; moving left?
        ldy     #$08                    ; Y = 8 (speed param)
        jsr     move_right_collide      ; move current right
        jmp     holograph_current_collision ; check for wall collision

holograph_current_move_left:  ldy     #$09 ; Y = 9 (speed param)
        jsr     move_left_collide       ; move current left
holograph_current_collision:  bcc     holograph_current_dist_check ; hit solid tile?
holograph_current_hit_check:  lda     ent_facing,x ; check direction type
        and     #$08                    ; bit 3 = vertical
        beq     holograph_current_status_clear ; horizontal — despawn
holograph_current_status_clear:  lda     #$00 ; deactivate entity
        sta     ent_status,x            ; clear entity status
        lda     #$FF                    ; mark spawn slot free
        sta     ent_spawn_id,x          ; free spawn slot
        rts                             ; done, entity removed

holograph_current_dist_check:  jsr     entity_x_dist_to_player ; get X distance to player
        cmp     #$18                    ; within 24px X?
        bcs     holograph_current_dir_check ; too far away
        jsr     entity_y_dist_to_player ; get Y distance to player
        cmp     #$14                    ; within 20px Y?
        bcs     holograph_current_dir_check ; too far away
        lda     ent_facing,x            ; check direction type
        and     #$08                    ; bit 3 = vertical
        beq     holograph_current_facing_dir ; horizontal — apply damage
        rts                             ; vertical current, no push

holograph_current_facing_dir:  lda     ent_facing,x ; check horizontal facing
        and     #FACING_LEFT            ; bit 1 = left facing
        bne     holograph_current_facing_right ; facing left?
        lda     #$01                    ; push player right
        bne     holograph_current_dir_set ; always branch
holograph_current_facing_right:  lda     #FACING_LEFT ; push player left
holograph_current_dir_set:  sta     $36 ; set push direction
        lda     #$00                    ; no Y push
        sta     $37                     ; store Y push component
        lda     #$02                    ; 2px damage push speed
        sta     $38                     ; store push speed
holograph_current_dir_check:  lda     ent_facing,x ; check direction type
        and     #$08                    ; bit 3 = vertical
        beq     holograph_current_return ; horizontal — done
holograph_current_return:  rts

holograph_boss_init:  lda     ent_status,x ; check sub-state
        and     #$0F                    ; mask low nibble
        bne     holograph_boss_phase_check ; already initialized?
        lda     #$09                    ; PSTATE_BOSS_WAIT
        cmp     player_state            ; player already waiting?
        beq     holograph_boss_hp_check ; skip if already set
        sta     player_state            ; freeze player for intro
        lda     #$80                    ; start HP bar fill
        sta     boss_hp_display         ; init HP bar display
        lda     #$8E                    ; boss HP bar palette
        sta     $B3                     ; set palette for HP bar
        lda     #MUSIC_BOSS             ; play boss music
        jsr     submit_sound_ID_D9      ; start boss music
holograph_boss_hp_check:  lda     boss_hp_display ; check HP bar fill
        cmp     #HEALTH_FULL            ; bar fully filled
        bne     holograph_boss_return   ; still filling?
        lda     ent_status,x            ; set invincibility bit
        ora     #$40                    ; set invincible flag
        sta     ent_status,x            ; update boss status
        lda     #$00                    ; clear temp counters
        sta     $01                     ; clear tentacle index
        sta     $02                     ; clear loop counter
        jsr     holograph_tentacle_spawn ; spawn initial tentacles
        lda     #$3C                    ; 60 frame initial delay
        sta     ent_timer,x             ; set initial move timer
        lda     #$36                    ; 54px movement distance
        sta     ent_var1,x              ; set initial move distance
        lda     #FACING_RIGHT           ; start facing right
        sta     ent_facing,x            ; start moving right
        inc     ent_status,x            ; advance to movement phase
holograph_boss_phase_check:  lda     ent_status,x ; check sub-state
        and     #$0F                    ; mask low nibble
        cmp     #$02                    ; state 2 = movement
        beq     holograph_boss_facing_check ; in movement phase?
        cmp     #$03                    ; state 3 = attack
        bne     holograph_boss_flag_check ; not in known state?
        jmp     holograph_boss_attack_pattern ; handle attack phase

holograph_boss_flag_check:  lda     ent_flags,x ; check flags
        and     #$04                    ; bit 2 = wait flag
        beq     holograph_boss_anim_check ; not waiting?
        dec     ent_timer,x             ; count down wait timer
        bne     holograph_boss_return   ; still waiting?
        lda     ent_flags,x             ; clear wait flag
        eor     #$04                    ; toggle bit 2
        sta     ent_flags,x             ; update flag state
holograph_boss_anim_check:  lda     ent_anim_state,x ; check animation progress
        cmp     #$04                    ; state 4 = anim complete
        bne     holograph_boss_return   ; not done yet?
        lda     #$04                    ; set idle animation
        jsr     reset_sprite_anim       ; switch to idle anim
        lda     #$3C                    ; 60 frame pre-move timer
        sta     ent_timer,x             ; set pre-movement timer
        inc     ent_status,x            ; advance to movement phase
holograph_boss_return:  rts

holograph_boss_facing_check:  lda     ent_facing,x ; check facing direction
        and     #FACING_RIGHT           ; bit 0 = right
        beq     holograph_boss_flag_left ; facing right?
        lda     ent_flags,x             ; set H-flip flag
        ora     #ENT_FLAG_HFLIP         ; bit 6 = horizontal flip
        sta     ent_flags,x             ; update H-flip for right
        jsr     move_sprite_right       ; move boss rightward
        jmp     holograph_boss_var1_dec ; continue to distance check

holograph_boss_flag_left:  lda     ent_flags,x ; clear H-flip flag
        and     #$BF                    ; mask off bit 6
        sta     ent_flags,x             ; update H-flip for left
        jsr     move_sprite_left        ; move boss leftward
holograph_boss_var1_dec:  dec     ent_var1,x ; count down move distance
        bne     holograph_boss_var2_check ; reached edge?
        lda     ent_facing,x            ; get current facing
        eor     #$03                    ; reverse direction
        sta     ent_facing,x            ; update facing direction
        lda     #$6C                    ; 108px full sweep distance
        sta     ent_var1,x              ; set full sweep distance
        inc     ent_var2,x              ; count direction changes
holograph_boss_var2_check:  lda     ent_var2,x ; check reversal count
        cmp     #$02                    ; done 2 sweeps?
        bcc     holograph_boss_timer_dec ; not yet — keep moving
        lda     ent_var1,x              ; check move distance left
        cmp     #$36                    ; past center point?
        bcs     holograph_boss_timer_dec ; not at center yet
        inc     ent_status,x            ; advance to attack phase
        lda     #$1E                    ; 30 frame attack delay
        sta     ent_var3,x              ; set attack delay timer
        lda     #$A1                    ; contact damage + hitbox
        sta     ent_hitbox,x            ; enable boss contact damage
        lda     #$01                    ; set attack animation
        jmp     reset_sprite_anim       ; set anim and return

holograph_boss_timer_dec:  dec     ent_timer,x ; count down tentacle timer
        bne     holograph_boss_anim_alt ; time to spawn?
        lda     #$05                    ; set tentacle anim
        jsr     reset_sprite_anim       ; play tentacle launch anim
        jsr     holograph_tentacle_entity ; spawn tentacle projectile
        lda     $E4                     ; PRNG advance
        adc     $E5                     ; add PRNG high byte
        sta     $E4                     ; store updated PRNG low
        and     #$03                    ; mask to 0-3
        tay                             ; use as timer table index
        lda     holograph_boss_timer_table,y ; look up random timer
        sta     ent_timer,x             ; set next spawn delay
holograph_boss_anim_alt:  lda     ent_anim_id,x ; check current anim
        cmp     #$04                    ; anim 4 = idle?
        beq     holograph_boss_return_a ; already idle — done
        lda     ent_anim_state,x        ; check anim state
        beq     holograph_boss_return_a ; not started?
        cmp     #$03                    ; state 3 = anim ending
        bne     holograph_boss_return_a ; not at end frame?
        lda     #$04                    ; return to idle anim
        jsr     reset_sprite_anim       ; switch back to idle anim
holograph_boss_return_a:  rts

holograph_boss_attack_pattern:  lda     ent_anim_id,x ; check current anim ID
        cmp     #$13                    ; anim $13 = attack pose
        beq     holograph_boss_flag_xor ; in attack pose?
        dec     ent_var3,x              ; count down attack timer
        bne     holograph_boss_pattern_return ; timer expired?
        lda     #$13                    ; set attack animation
        jsr     reset_sprite_anim       ; play attack animation
        lda     #$3C                    ; 60 frame attack duration
        sta     ent_var3,x              ; set attack duration timer
holograph_boss_pattern_return:  rts

holograph_boss_flag_xor:  lda     ent_flags,x ; check flags
        and     #$04                    ; bit 2 = attack fired
        bne     holograph_boss_var3_dec ; already fired?
        lda     ent_anim_state,x        ; check anim progress
        cmp     #$04                    ; state 4 = anim done
        bne     holograph_boss_pattern_return ; not done yet?
        jsr     holograph_tentacle_id_check ; spawn new tentacles
        lda     ent_flags,x             ; get current flags
        eor     #$04                    ; toggle attack-fired bit
        sta     ent_flags,x             ; mark attack as fired
holograph_boss_var3_dec:  dec     ent_var3,x ; count down post-attack
        bne     holograph_boss_pattern_return ; timer expired?
        lda     #$06                    ; 6 frame wait timer
        sta     ent_timer,x             ; set post-attack wait timer
        lda     #$36                    ; 54px movement distance
        sta     ent_var1,x              ; reset movement distance
        lda     #$00                    ; reset sweep counter
        sta     ent_var2,x              ; clear reversal counter
        sta     ent_var3,x              ; clear attack timer
        lda     #$13                    ; set attack pose anim
        jsr     reset_sprite_anim       ; play return animation
        lda     #$94                    ; active + child + visible
        sta     ent_flags,x             ; update boss flags
        lda     #$80                    ; X = $80 (center screen)
        sta     ent_x_px,x              ; recenter boss horizontally
        lda     #$C1                    ; active + invincible + state 1
        sta     ent_status,x            ; reset to intro/wait state
        rts                             ; done, boss returns to intro

holograph_boss_timer_table:  .byte   $1E,$3C,$1E,$3C
holograph_tentacle_spawn:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     holograph_tentacle_return ; no free slot?
        sty     temp_00                 ; save child slot index
        lda     ent_x_scr,x             ; copy parent screen
        sta     ent_x_scr,y             ; set child screen position
        ldy     $01                     ; get tentacle index
        lda     holograph_tentacle_facing,y ; look up facing direction
        ldy     temp_00                 ; restore child slot
        sta     ent_facing,y            ; set tentacle facing
        lda     #$80                    ; X = center screen
        sta     ent_x_px,y              ; set tentacle X position
        ldy     $01                     ; get tentacle index
        lda     holograph_tentacle_chr_table,y ; look up Y position
        ldy     temp_00                 ; restore child slot
        sta     ent_y_px,y              ; set tentacle Y position
        lda     #$01                    ; 1px/frame move speed
        sta     ent_xvel,y              ; set tentacle X velocity
        lda     #$13                    ; tentacle OAM anim ID
        jsr     init_child_entity       ; init as child entity
        lda     #$00                    ; clear var2/var3
        sta     ent_var2,y              ; clear tentacle phase flag
        sta     ent_var3,y              ; clear tentacle launch flag
        sta     ent_xvel_sub,y          ; clear X sub-velocity
        lda     #$F5                    ; tentacle AI routine $F5
        sta     ent_routine,y           ; set tentacle AI handler
        sta     ent_hp,y                ; HP = $F5 (very high)
        sta     ent_spawn_id,y          ; mark spawn slot used
        lda     #$81                    ; active state 1
        sta     ent_status,y            ; activate tentacle entity
        lda     #$3C                    ; 60 frame initial timer
        sta     ent_timer,y             ; set tentacle move timer
        lda     #$36                    ; 54px move distance
        sta     ent_var1,y              ; set tentacle move distance
        lda     #$94                    ; active + child + visible
        sta     ent_flags,y             ; set tentacle display flags
        lda     #$81                    ; contact damage + small box
        sta     ent_hitbox,y            ; set tentacle hitbox
        inc     $01                     ; next tentacle index
        inc     $02                     ; increment spawn loop counter
        lda     $02                     ; check spawn count
        cmp     #$02                    ; spawned 2 tentacles?
        bcc     holograph_tentacle_spawn ; spawn next tentacle
holograph_tentacle_return:  rts

holograph_tentacle_chr_table:  .byte   $74,$C4,$24,$74,$C4,$24,$C4,$24
        .byte   $C4,$74,$24,$74,$24,$C4,$24,$C4
holograph_tentacle_facing:  .byte   $02,$01,$01,$02,$01,$01,$01,$01
        .byte   $01,$02,$01,$02,$01,$01,$01,$01
holograph_tentacle_id_check:  lda     ent_spawn_id,x ; check entity spawn ID
        cmp     #$2C                    ; $2C = holograph boss ID
        bne     holograph_tentacle_clear ; not holograph boss?
        lda     $E4                     ; PRNG advance
        adc     $E5                     ; add PRNG high byte
        sta     $E4                     ; store updated PRNG low
        and     #$07                    ; mask to 0-7
        tay                             ; use as pattern index
        lda     holograph_tentacle_chr_alt,y ; look up Y position
        sta     ent_y_px,x              ; set new Y position
        lda     holograph_tentacle_facing_alt,y ; look up facing
        sta     ent_facing,x            ; set new facing
        lda     #$C1                    ; contact + large hitbox
        sta     ent_hitbox,x            ; update boss hitbox
        lda     holograph_tentacle_pos_offset,y ; look up table offset
        sta     $01                     ; save as spawn index
        lda     #$00                    ; reset loop counter
        sta     $02                     ; reset loop counter
        jsr     holograph_tentacle_spawn ; spawn new tentacle pair
        rts                             ; done, new tentacles spawned

holograph_tentacle_clear:  lda     #$00 ; deactivate entity
        sta     ent_status,x            ; clear entity status
        rts                             ; done, entity removed

holograph_tentacle_chr_alt:  .byte   $24,$C4,$74,$74,$24,$C4,$74,$74
holograph_tentacle_facing_alt:  .byte   $01,$01,$02,$02,$01,$01,$02,$02
holograph_tentacle_pos_offset:  .byte   $00,$02,$04,$06,$08,$0A,$0C,$0E
holograph_tentacle_entity:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     holograph_tentacle_spawn_end ; no free slot?
        sty     temp_00                 ; save child slot index
        lda     ent_facing,x            ; inherit parent facing
        sta     ent_facing,y            ; set child facing direction
        and     #FACING_RIGHT           ; bit 0 = right direction
        tay                             ; use as offset index (0 or 1)
        lda     ent_x_px,x              ; parent X position
        clc                             ; prepare for addition
        adc     holograph_tentacle_init_table,y ; add directional offset
        ldy     temp_00                 ; restore child slot
        sta     ent_x_px,y              ; set child X position
        lda     ent_x_scr,x             ; copy parent screen
        sta     ent_x_scr,y             ; set child screen position
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y              ; set child Y position
        lda     #$00                    ; clear HP (projectile)
        sta     ent_hp,y                ; projectile has no HP
        lda     #$19                    ; X sub-velocity $19
        sta     ent_xvel_sub,y          ; set X sub-velocity
        sta     $02                     ; save for homing calc
        lda     #$04                    ; 4px/frame speed
        sta     ent_xvel,y              ; set projectile X velocity
        sta     $03                     ; save for homing calc
        sty     $0F                     ; save child slot
        stx     $0E                     ; save parent slot
        ldx     $0F                     ; X = child for homing
        jsr     calc_homing_velocity    ; calc velocity to player
        ldy     $0F                     ; restore child slot
        ldx     $0E                     ; restore parent slot
        lda     $0C                     ; get homing direction
        sta     ent_facing,y            ; set projectile facing
        lda     #$73                    ; tentacle proj anim ID
        jsr     init_child_entity       ; init as child entity
        lda     #$8F                    ; projectile AI routine
        sta     ent_routine,y           ; set projectile AI handler
        lda     #$8B                    ; contact damage + hitbox
        sta     ent_hitbox,y            ; set projectile hitbox
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
