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
;   $A4 → flash_homing_init           (Doc Flash projectile — homing magnet)
;   $A5 → wood_leaf_fall_update           (Doc Wood leaf — falling)
;   $A6 → wood_leaf_bounce_init           (Doc Wood leaf — bouncing)
;   $A7 → crash_bomb_init           (Doc Crash bomb projectile)
;   $A8 → metal_blade_init           (unused)
;   $A9 → stub_rts           (stub RTS)
; =============================================================================
main_doc_flash_j:

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"

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
        jmp     flash_init_and_dispatch               ; $A0 — Doc Flash Man boss AI
main_doc_wood_j:
        jmp     wood_state_dispatch               ; $A1 — Doc Wood Man boss AI
main_doc_crash_j:
        jmp     crash_init_and_dispatch               ; $A2 — Doc Crash Man boss AI
main_doc_metal_j:
        jmp     metal_init_and_dispatch               ; $A3 — Doc Metal Man boss AI
        jmp     flash_homing_init               ; $A4 — Flash projectile (homing magnet)
        jmp     wood_leaf_fall_update               ; $A5 — Wood leaf (falling)
        jmp     wood_leaf_bounce_init               ; $A6 — Wood leaf (bouncing)
        jmp     crash_bomb_init               ; $A7 — Crash bomb projectile
        jmp     metal_blade_init               ; $A8 — unused
        jmp     stub_rts               ; $A9 — stub RTS

stub_rts:  rts

        .byte   $F3,$AA,$B5,$89,$5D,$BA,$F8,$AA
        .byte   $37,$28,$DF,$8A,$3D,$A2
        jmp     metal_blade_init

; =============================================================================
; DOC FLASH MAN AI ($A0)
; =============================================================================
; Mimics Flash Man from MM2. Two phases:
;   Phase 1 (status & $01): walk toward player, apply gravity
;   Phase 2 (status & $02): Time Stopper — spawn projectiles every 8 frames,
;     after 6 projectiles revert to phase 1
; The Time Stopper attack triggers the special_death state ($07) on the player.
; =============================================================================

flash_init_and_dispatch:  lda     ent_status,x        ; --- init (status low nibble == 0) ---
        and     #$0F                    ; isolate low nibble (phase)
        bne     flash_phase_dispatch               ; skip init if already running
        lda     ent_status,x            ; reload for modification
        ora     #$40                    ; set invincibility flag
        sta     ent_status,x            ; apply invincibility + phase bits
        inc     ent_status,x            ; advance to phase 1
        lda     #$60                    ; 96-frame walk duration
        sta     ent_timer,x             ; walk timer = 96 frames
        lda     #$08                    ; 8-frame spawn interval
        sta     ent_var1,x              ; projectile spawn interval
flash_phase_dispatch:  lda     ent_status,x        ; --- phase dispatch ---
        and     #$02                    ; test phase 2 (Time Stopper) bit
        bne     flash_time_stopper               ; phase 2: Time Stopper attack
        ldy     #$1E                    ; gravity speed parameter
        jsr     move_vertical_gravity   ; apply gravity to Doc Flash
        rol     $0F                     ; save carry (ground flag)
        lda     ent_facing,x            ; check current facing
        and     #$01                    ; test right-facing bit
        beq     flash_walk_left               ; branch if facing left
        ldy     #$20                    ; walk speed right
        jsr     move_right_collide      ; walk right with collision
        jmp     flash_check_grounded               ; skip left movement

flash_walk_left:  ldy     #$21                ; walk speed left
        jsr     move_left_collide       ; walk left with collision
flash_check_grounded:  lda     $0F                 ; load collision result
        and     #$01                    ; test on-ground bit
        beq     flash_walk_done               ; skip if airborne
        dec     ent_timer,x             ; count down walk timer
        bne     flash_check_ceiling_hit               ; still walking
        lda     #$06                    ; Time Stopper anim ID
        jsr     reset_sprite_anim       ; start Time Stopper anim
        inc     ent_status,x            ; advance to phase 2
        rts

flash_check_ceiling_hit:  lda     $10                 ; load vertical collision flags
        and     #$10                    ; test hit-ceiling bit
        beq     flash_set_walk_anim               ; no ceiling hit, stay grounded
        lda     #$03                    ; jump anim ID
        jsr     reset_sprite_anim       ; set jump animation
        lda     #$9E                    ; Y velocity sub-pixel
        sta     ent_yvel_sub,x          ; set Y sub-velocity for jump
        lda     #$04                    ; upward jump speed = 4
        sta     ent_yvel,x              ; set Y velocity (jump)
        jmp     face_player             ; face player before jumping

flash_set_walk_anim:  lda     ent_anim_id,x       ; check current anim
        cmp     #$05                    ; already walking anim?
        beq     flash_walk_done               ; skip if already set
        lda     #$05                    ; walking anim ID
        jsr     reset_sprite_anim       ; set walking animation
flash_walk_done:  rts

; --- phase 2: Time Stopper attack ---
flash_time_stopper:  lda     ent_anim_id,x
        cmp     #$06                    ; check if Time Stopper anim playing
        bne     flash_spawn_interval_tick               ; not Time Stopper intro
        lda     ent_anim_frame,x        ; check anim frame counter
        ora     ent_anim_state,x        ; combine with anim state
        bne     flash_phase2_done               ; wait for anim to finish
        jsr     flash_copy_explosion_oam               ; trigger explosion + kill player
        lda     #$02                    ; standing anim ID
        jsr     reset_sprite_anim       ; set standing anim
        jsr     face_player             ; turn toward player
        jsr     set_sprite_hflip        ; flip sprite to match facing
        rts

flash_spawn_interval_tick:  dec     ent_var1,x          ; count down spawn interval
        bne     flash_phase2_done               ; interval not elapsed yet
        lda     #$08                    ; 8-frame spawn interval
        sta     ent_var1,x              ; reset interval
        jsr     flash_spawn_projectile               ; spawn projectile
        inc     ent_timer,x             ; count projectiles spawned
        lda     ent_timer,x             ; check projectile count
        cmp     #$06                    ; spawned all 6?
        bcs     flash_end_attack               ; all 6 spawned, end attack
        rts

flash_end_attack:  lda     #$05                ; done — revert to walk anim
        jsr     reset_sprite_anim       ; set walking anim
        lda     #$60                    ; 96-frame walk timer
        sta     ent_timer,x             ; reset walk timer
        dec     ent_status,x            ; back to phase 1
        lda     player_state            ; if player already dead ($0E),
        cmp     #PSTATE_DEATH           ; don't reset state
        beq     flash_phase2_done               ; if player already dead ($0E),
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state            ; release player from Doc Robot
flash_phase2_done:  rts                         ; state → $00 (on_ground)

; --- spawn Doc Flash projectile ---
flash_spawn_projectile:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     flash_spawn_projectile_done               ; no slot available
        sty     temp_00                 ; save child slot index
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y            ; to child entity
        and     #$01                    ; extract right bit as index
        tay                             ; use as X-offset table index
        lda     ent_x_px,x              ; parent X position
        clc                             ; prepare offset addition
        adc     doc_flash_projectile_x_offset_table,y ; add facing-based X offset
        ldy     temp_00                 ; restore child slot index
        sta     ent_x_px,y              ; set child X position
        lda     ent_x_scr,x             ; parent screen page
        sta     ent_x_scr,y             ; copy screen to child
        lda     ent_y_px,x              ; parent Y position
        sta     ent_y_px,y              ; copy Y to child
        lda     #$00                    ; zero
        sta     ent_hp,y                ; projectile has 0 HP
        sta     ent_xvel_sub,y          ; no sub-pixel X velocity
        lda     #$08                    ; X speed = 8 px/frame
        sta     ent_xvel,y              ; set projectile X velocity
        lda     #$58                    ; child sprite/OAM ID
        jsr     init_child_entity       ; initialize child entity
        lda     #$8B                    ; hitbox + contact damage
        sta     ent_hitbox,y            ; set projectile hitbox
        lda     #$A4                    ; homing projectile AI ($A4)
        sta     ent_routine,y           ; set child AI routine
flash_spawn_projectile_done:  rts

doc_flash_projectile_x_offset_table:  .byte   $E9,$17 ; X offset per facing: left=-23, right=+23

; =============================================================================
; DOC FLASH HOMING PROJECTILE ($A4)
; =============================================================================
; Homing projectile used by Doc Flash Man's Time Stopper attack.
; Calculates homing velocity toward player on init, then moves in that
; direction. Uses random Y-offset table to vary target position.
; =============================================================================

flash_homing_init:  lda     ent_status,x        ; --- init (first frame) ---
        and     #$0F                    ; isolate low nibble
        bne     flash_homing_move               ; skip init if already active
        sta     ent_var2,x              ; clear homing-active flag
        inc     ent_status,x            ; advance to active state
        jsr     entity_x_dist_to_player ; get X dist to player
        cmp     #$18                    ; at least 24px away?
        bcc     flash_homing_move               ; too close, skip homing calc
        lda     ent_y_px                ; save player Y position
        sta     ent_timer,x             ; stash in timer temporarily
        lda     $E4                     ; pseudo-random seed
        adc     $E5                     ; advance PRNG
        sta     $E4                     ; store new seed
        and     #$0F                    ; mask to 0-15 table index
        tay                             ; use as table index
        lda     ent_y_px,x              ; projectile Y position
        clc                             ; prepare offset addition
        adc     doc_flash_homing_random_y_offset_table,y ; add random Y offset
        sta     ent_y_px                ; set as fake target Y
        lda     #$00                    ; zero sub-pixel velocity
        sta     $02                     ; velocity sub-pixel = 0
        lda     #$08                    ; homing speed = 8
        sta     $03                     ; velocity magnitude
        jsr     calc_homing_velocity    ; compute XY homing velocity
        lda     $0C                     ; facing result from calc
        sta     ent_facing,x            ; set projectile facing
        lda     ent_timer,x             ; retrieve saved player Y
        sta     ent_y_px                ; restore real player Y
        inc     ent_var2,x              ; mark homing as active
flash_homing_move:  lda     ent_var2,x          ; --- movement phase ---
        beq     flash_homing_move_horiz               ; skip Y move if not homing
        lda     ent_facing,x            ; check direction bits
        and     #$08                    ; test up-direction bit
        beq     flash_homing_move_down               ; branch if moving down
        jsr     move_sprite_up          ; move projectile up
        jmp     flash_homing_move_horiz               ; skip down movement

flash_homing_move_down:  jsr     move_sprite_down    ; move projectile down
flash_homing_move_horiz:  lda     ent_facing,x        ; check horizontal direction
        and     #$01                    ; test right-facing bit
        beq     flash_homing_move_left               ; branch if facing left
        jmp     move_sprite_right       ; move projectile right

flash_homing_move_left:  jmp     move_sprite_left    ; move projectile left

doc_flash_homing_random_y_offset_table:  .byte   $24,$0C,$10,$00,$E0,$F4,$10,$F8 ; random Y-offset table (16 entries)
        .byte   $18,$F0,$08,$10,$00,$F0,$00,$E8

; --- copy explosion OAM data to sprite page (special death effect) ---
flash_copy_explosion_oam:  ldy     #$68                ; 27 sprites * 4 bytes = $6C
flash_explosion_oam_loop:  lda     doc_flash_explosion_oam_data_y,y ; load sprite Y coordinate
        sta     $0200,y                 ; write to OAM buffer Y
        lda     doc_flash_explosion_oam_data_x,y ; load sprite tile index
        sta     $0201,y                 ; write to OAM buffer tile
        lda     doc_flash_explosion_oam_data_flags,y ; load sprite attributes
        sta     $0202,y                 ; write to OAM buffer attr
        lda     doc_flash_explosion_oam_data_palette,y ; load sprite X coordinate
        sta     $0203,y                 ; write to OAM buffer X
        dey
        dey
        dey
        dey
        bpl     flash_explosion_oam_loop               ; loop until all sprites copied

; This is the ONLY trigger for state $07 (special_death) in the entire game.
; Copies explosion OAM data to sprite page, then sets palette-cycling kill.
; Triggered by Doc Flash Man's Time Stopper attack (Gemini Man Doc Robot stage).
; [confirmed via Mesen]
        lda     player_state            ; if player already dead ($0E),
        cmp     #PSTATE_DEATH           ; don't overwrite with special_death
        beq     flash_special_death_done
        lda     #PSTATE_SPECIAL_DEATH   ; state → $07 (special_death)
        sta     player_state            ; palette cycling kill effect
        lda     #$1E                    ; timer = 30 frames
        sta     ent_timer
flash_special_death_done:  rts

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

wood_state_dispatch:  lda     ent_status,x        ; dispatch to state handler
        and     #$0F                    ; isolate state index (low nibble)
        tay                             ; use as table index
        lda     doc_wood_state_handler_low_table,y ; load handler address low byte
        sta     temp_00                 ; store in jump pointer
        lda     doc_wood_state_handler_high_table,y ; load handler address high byte
        sta     $01
        jmp     (temp_00)               ; jump to state handler

doc_wood_state_handler_low_table:  .byte   $6F,$92,$B5,$CA,$FD,$46 ; state handler pointers (low)
doc_wood_state_handler_high_table:  .byte   $A2,$A2,$A2,$A2,$A2,$A3 ; state handler pointers (high)
        lda     #$9E                    ; --- state 0: init ---
        sta     ent_yvel_sub,x          ; set upward Y velocity (sub)
        lda     #$04                    ; Y velocity = 4 px/frame (jump)
        sta     ent_yvel,x
        lda     ent_status,x            ; set bit 6 = shielded/invuln
        ora     #$40
        sta     ent_status,x
        inc     ent_status,x            ; advance to state 1
        lda     #$12                    ; leaf throw interval = 18 frames
        sta     ent_timer,x
        lda     #$60                    ; overall cycle timer = 96 frames
        sta     ent_var3,x
        jsr     wood_spawn_leaf_shield               ; spawn leaf shield entity
        rts

        dec     ent_timer,x             ; --- state 1: throw leaves ---
        bne     wood_tick_anim               ; timer not expired, keep waiting
        lda     #$12                    ; reload 18-frame interval
        sta     ent_timer,x
        jsr     wood_spawn_thrown_leaf               ; spawn a thrown leaf
        inc     ent_var1,x              ; count throws
        lda     ent_var1,x
        cmp     #$04                    ; thrown 4 leaves yet?
        bcc     wood_tick_anim               ; no, keep throwing
        lda     #$2E                    ; set wait timer = 46 frames
        sta     ent_var1,x
        inc     ent_status,x            ; advance to state 2
wood_tick_anim:  jsr     wood_force_anim_tick           ; force anim to tick one frame
        rts

        dec     ent_var1,x              ; --- state 2: countdown + crash ---
        bne     wood_tick_anim               ; still counting down
        lda     #$00                    ; crash block spawn index = 0
        sta     temp_00
        jsr     wood_spawn_crash_blocks               ; spawn 4 falling crash blocks
        inc     ent_status,x            ; advance to state 3
        lda     #$24                    ; shield attack delay = 36 frames
        sta     ent_var2,x
        rts

        jsr     wood_force_anim_tick               ; --- state 3: release shield ---
        dec     ent_var2,x              ; count down shield delay
        bne     wood_state_done               ; not zero, wait
        lda     #$0F                    ; pre-fall delay = 15 frames
        sta     ent_var2,x
        inc     ent_status,x            ; advance to state 4 (fall)
        lda     #$80                    ; leaf shield spawn ID = $80
        sta     temp_00
        ldy     #$1F                    ; search slots $1F down to $10
wood_search_shield_loop:  lda     ent_status,y        ; is slot active?
        bmi     wood_check_shield_match               ; yes, check if it's the shield
wood_search_shield_next:  dey                         ; not active, try next slot
        cpy     #$0F                    ; searched all enemy slots?
        bne     wood_search_shield_loop               ; no, keep searching
        rts

wood_check_shield_match:  lda     temp_00 ; check spawn ID
        cmp     ent_spawn_id,y          ; match spawn ID $80 = shield
        bne     wood_search_shield_next               ; not the shield, try next
        lda     #$3C                    ; set shield to attack routine
        sta     ent_routine,y
        lda     #$8D                    ; enable contact damage on shield
        sta     ent_hitbox,y
wood_state_done:  rts

        lda     ent_var2,x              ; --- state 4: falling ---
        beq     wood_fall_walk               ; pre-walk delay active?
        dec     ent_var2,x              ; decrement pre-walk delay
        rts

wood_fall_walk:  lda     ent_facing,x        ; check facing direction
        and     #$01                    ; bit 0 set = facing right
        beq     wood_fall_walk_left               ; facing left, branch
        ldy     #$20                    ; walk speed parameter
        jsr     move_right_collide      ; move right with collision
        jmp     wood_fall_gravity

wood_fall_walk_left:  ldy     #$21                ; walk speed parameter
        jsr     move_left_collide       ; move left with collision
wood_fall_gravity:  ldy     #$1E                ; gravity strength parameter
        jsr     move_vertical_gravity   ; apply gravity, check landing
        bcc     wood_fall_check_anim               ; not landed → skip jump setup
        lda     #$9E                    ; set upward Y velocity (sub)
        sta     ent_yvel_sub,x
        lda     #$04                    ; Y velocity = 4 (next jump)
        sta     ent_yvel,x
        inc     ent_status,x            ; advance to state 5 (land)
        lda     #$1D                    ; landing anim ID = $1D
        bne     wood_set_anim_and_return               ; always branches (A != 0)
wood_fall_check_anim:  lda     ent_anim_id,x       ; still airborne: check anim
        cmp     #$1D                    ; anim $1D = falling
        bne     wood_state_done               ; not in falling anim yet
        lda     ent_anim_frame,x        ; check if jump anim finished
        ora     ent_anim_state,x        ; combine frame + state
        bne     wood_state_done               ; still animating, wait
        lda     #$03                    ; switch to jumping anim $03
wood_set_anim_and_return:  jmp     reset_sprite_anim   ; set anim and return

        lda     ent_anim_state,x        ; --- state 5: landed ---
        cmp     #$01                    ; check if anim tick complete
        bne     wood_land_cycle_tick               ; not ready yet
        lda     #$01                    ; reset anim state to 1
        sta     ent_anim_state,x
        lda     #$00                    ; reset anim frame to 0
        sta     ent_anim_frame,x
wood_land_cycle_tick:  dec     ent_var3,x          ; decrement cycle timer
        bne     wood_land_done               ; cycle not done, keep going
        dec     ent_status,x            ; dec status 4x: state 5 -> 1
        dec     ent_status,x
        dec     ent_status,x
        dec     ent_status,x
        lda     #$60                    ; reset cycle timer = 96
        sta     ent_var3,x
        jsr     face_player             ; turn toward player
        jsr     set_sprite_hflip        ; update sprite H-flip
        jsr     wood_spawn_leaf_shield               ; spawn new leaf shield
wood_land_done:  rts

; --- utility: force animation to advance one frame ---
wood_force_anim_tick:  lda     #$01                ; trigger anim advance
        sta     ent_anim_state,x
        lda     #$00                    ; reset frame counter
        sta     ent_anim_frame,x
        rts

; --- spawn leaf shield entity (orbits Doc Wood Man) ---
wood_spawn_leaf_shield:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     wood_spawn_done               ; no free slot, abort
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        lda     ent_x_px,x              ; copy parent X position
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y position
        sta     ent_y_px,y
        lda     #$00                    ; shield has 0 HP (invincible)
        sta     ent_hp,y
        sta     ent_xvel_sub,y          ; clear X velocity sub
        lda     #$A9                    ; shield AI routine = $A9
        sta     ent_routine,y
        lda     #$1B                    ; OAM ID $1B = leaf shield
        jsr     init_child_entity       ; init as child entity
        lda     #$AD                    ; hitbox $AD = shield shape
        sta     ent_hitbox,y
        lda     #$80                    ; spawn ID $80 = shield marker
        sta     ent_spawn_id,y
        lda     #$04                    ; orbit speed = 4 px/frame
        sta     ent_xvel,y
        rts

; --- spawn leaf projectile (thrown at player) ---
wood_spawn_thrown_leaf:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     wood_spawn_done               ; no free slot, abort
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        lda     ent_x_px,x              ; copy parent X position
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y position
        sta     ent_y_px,y
        lda     #$00                    ; leaf has 0 HP
        sta     ent_hp,y
        sta     ent_yvel_sub,y          ; clear Y velocity sub
        lda     #$12                    ; OAM ID $12 = thrown leaf
        jsr     init_child_entity       ; init as child entity
        lda     #$8B                    ; hitbox $8B = small + damage
        sta     ent_hitbox,y
        lda     #$04                    ; fall speed = 4 px/frame
        sta     ent_yvel,y
        lda     #$A5                    ; leaf AI routine = $A5
        sta     ent_routine,y
wood_spawn_done:  rts

; --- spawn 4 falling crash blocks (called recursively) ---
wood_spawn_crash_blocks:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     wood_spawn_done               ; no free slot, abort
        lda     #$02                    ; face left
        sta     ent_facing,y
        lda     ent_x_scr,x             ; same screen as parent
        sta     ent_x_scr,y
        lda     #$20                    ; spawn near top of screen
        sta     ent_y_px,y
        lda     #$00                    ; block has 0 HP
        sta     ent_hp,y
        lda     #$12                    ; OAM ID $12 = crash block
        jsr     init_child_entity       ; init as child entity
        lda     #$8B                    ; hitbox $8B = small + damage
        sta     ent_hitbox,y
        lda     #$62                    ; diagonal velocity (sub)
        sta     ent_xvel_sub,y          ; same sub for Y axis
        sta     ent_yvel_sub,y
        lda     #$01                    ; X + Y speed = 1 px/frame
        sta     ent_xvel,y              ; diagonal fall
        sta     ent_yvel,y
        lda     #$A6                    ; crash block AI routine = $A6
        sta     ent_routine,y
        stx     $01                     ; save parent entity index
        ldx     temp_00                 ; load spawn counter
        lda     doc_wood_crash_block_x_positions_table,x ; get X position from table
        sta     ent_x_px,y
        ldx     $01                     ; restore parent entity index
        inc     temp_00                 ; next crash block index
        lda     temp_00
        cmp     #$04                    ; spawned all 4 blocks?
        bcc     wood_spawn_crash_blocks               ; no, spawn next block
        rts

doc_wood_crash_block_x_positions_table:  .byte   $40,$70,$A0,$D0 ; X positions for 4 crash blocks

; =============================================================================
; DOC WOOD LEAF — FALLING ($A5)
; =============================================================================
; Leaf entity that falls upward. Despawns when Y < 4.
; =============================================================================

wood_leaf_fall_update:  lda     #$00                ; reset animation
        sta     ent_anim_frame,x        ; clear frame counter
        sta     ent_anim_state,x        ; clear anim state
        jsr     move_sprite_up          ; move leaf upward
        lda     ent_y_px,x              ; check Y position
        cmp     #$04                    ; below top of screen?
        bcs     wood_leaf_fall_done               ; yes — keep alive
        lda     #$00                    ; reached top — despawn
        sta     ent_status,x            ; deactivate entity
wood_leaf_fall_done:  rts

; =============================================================================
; DOC WOOD LEAF — BOUNCING ($A6)
; =============================================================================
; Leaf entity that bounces diagonally, reversing direction every 15 frames.
; =============================================================================

wood_leaf_bounce_init:  lda     ent_status,x        ; check if initialized
        and     #$0F                    ; low nibble = sub-state
        bne     wood_leaf_bounce_move               ; already initialized
        lda     #$0F                    ; 15 frames per direction
        sta     ent_timer,x             ; bounce direction timer
        inc     ent_status,x            ; advance to active state
wood_leaf_bounce_move:  lda     ent_facing,x        ; move horizontally
        and     #$01                    ; facing right?
        beq     wood_leaf_bounce_left               ; no — move left
        jsr     move_sprite_right       ; move leaf right
        jmp     wood_leaf_bounce_down               ; skip left movement

wood_leaf_bounce_left:  jsr     move_sprite_left    ; move leaf left
wood_leaf_bounce_down:  jsr     move_sprite_down    ; move leaf down
        dec     ent_timer,x             ; count down direction timer
        bne     wood_leaf_bounce_done               ; timer not expired
        lda     ent_facing,x            ; timer expired — reverse
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x            ; store new direction
        lda     #$0F                    ; reset 15-frame timer
        sta     ent_timer,x             ; store timer
wood_leaf_bounce_done:  rts

; =============================================================================
; DOC CRASH MAN AI ($A2)
; =============================================================================
; Mimics Crash Man from MM2. Two phases:
;   Phase 1 (status & $01): walk left/right, bounce off walls at X=$CC/$34.
;     Jumps on B-press or after 150-frame timer. Random X velocity on jump.
;   Phase 2 (status & $02): airborne — face player while ascending,
;     spawn Crash Bomb at apex, land and revert to phase 1.
; =============================================================================

crash_init_and_dispatch:  lda     ent_status,x        ; --- init ---
        and     #$0F                    ; low nibble = sub-state
        bne     crash_phase_dispatch               ; skip init if nonzero
        jsr     reset_gravity           ; reset gravity
        lda     ent_status,x            ; set bit 6 (contact damage)
        ora     #$40                    ; enable contact damage
        sta     ent_status,x            ; store updated status
        inc     ent_status,x            ; sub-state = 1 (walking)
        lda     #$05                    ; anim 5 = walking
        jsr     reset_sprite_anim       ; set walk animation
        lda     #$96                    ; 150 frames until auto-jump
        sta     ent_var1,x              ; store jump countdown
crash_phase_dispatch:  lda     ent_status,x        ; --- phase dispatch ---
        and     #$02                    ; check bit 1 (airborne)
        bne     crash_airborne_move               ; airborne — jump to phase 2
        lda     ent_facing,x            ; --- phase 1: walking ---
        and     #$01                    ; facing right?
        beq     crash_walk_left               ; no — move left
        ldy     #$20                    ; hitbox Y=$20
        jsr     move_right_collide      ; walk right with collision
        lda     ent_x_px,x              ; check X position
        cmp     #$CC                    ; right wall boundary
        bcs     crash_reverse_direction               ; at right wall — reverse
        jmp     crash_apply_gravity               ; within bounds — continue

crash_walk_left:  ldy     #$21                ; hitbox Y=$21
        jsr     move_left_collide       ; walk left with collision
        lda     ent_x_px,x              ; check X position
        cmp     #$34                    ; left wall boundary
        bcs     crash_apply_gravity               ; within bounds — skip reverse
crash_reverse_direction:  lda     ent_facing,x        ; --- reverse direction ---
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x            ; store reversed facing
crash_apply_gravity:  ldy     #$1E                ; hitbox Y=$1E
        jsr     move_vertical_gravity   ; apply gravity + floor check
        lda     ent_timer,x             ; check if timer active
        bne     crash_countdown_tick               ; timer active — use countdown
        lda     joy1_press              ; no timer — check B button
        and     #BTN_B                  ; B pressed?
        beq     crash_walk_done               ; no — stay walking
        inc     ent_status,x            ; advance to airborne phase
        jsr     crash_set_random_jump_vel               ; set random jump velocity
        inc     ent_timer,x             ; mark timer as active
crash_walk_done:  rts

crash_countdown_tick:  dec     ent_var1,x          ; decrement jump countdown
        bne     crash_check_b_press               ; not expired — check B
        inc     ent_status,x            ; timer expired — force jump
        jsr     crash_set_random_jump_vel               ; set random jump velocity
        lda     #$96                    ; reset to 150 frames
        sta     ent_var1,x              ; store jump countdown
        rts

crash_check_b_press:  lda     joy1_press          ; check B button mid-walk
        and     #BTN_B                  ; B pressed?
        beq     crash_walk_done               ; no — stay walking
        inc     ent_status,x            ; advance to airborne phase
        jsr     crash_set_random_jump_vel               ; set random jump velocity
        rts

crash_airborne_move:  lda     ent_facing,x        ; --- phase 2: airborne ---
        and     #$01                    ; facing right?
        beq     crash_airborne_left               ; no — move left
        ldy     #$20                    ; hitbox Y=$20
        jsr     move_right_collide      ; move right with collision
        jsr     crash_face_player_sprite               ; face player (sprite only)
        jmp     crash_airborne_gravity               ; skip left movement

crash_airborne_left:  ldy     #$21                ; hitbox Y=$21
        jsr     move_left_collide       ; move left with collision
        jsr     crash_face_player_sprite               ; face player (sprite only)
crash_airborne_gravity:  ldy     #$1E                ; hitbox Y=$1E
        jsr     move_vertical_gravity   ; apply gravity + floor check
        bcc     crash_airborne_anim               ; no landing — stay airborne
        dec     ent_status,x            ; --- landed ---
        jsr     reset_gravity           ; clear gravity accumulator
        lda     #$4C                    ; X vel = $01.4C (walking)
        sta     ent_xvel_sub,x          ; set walk speed sub-pixel
        lda     #$01                    ; X vel pixel = 1
        sta     ent_xvel,x              ; set walk speed pixel
        lda     #$00                    ; clear saved facing
        sta     ent_var2,x              ; reset var2
        lda     #$05                    ; anim 5 = walking
        jmp     reset_sprite_anim       ; set walk animation

crash_airborne_anim:  lda     ent_anim_id,x       ; --- still airborne ---
        cmp     #$04                    ; already in throw anim?
        beq     crash_airborne_done               ; yes — skip anim changes
        lda     ent_yvel,x              ; check Y velocity sign
        bpl     crash_set_fall_anim               ; positive = descending
        lda     ent_facing,x            ; --- ascending: aim + throw ---
        sta     ent_var2,x              ; save movement direction
        lda     #$04                    ; anim 4 = throwing
        jsr     reset_sprite_anim       ; set throw animation
        lda     ent_facing,x            ; save pre-face direction
        pha                             ; push onto stack
        jsr     face_player             ; turn sprite toward player
        pla                             ; restore old facing
        cmp     ent_facing,x            ; did facing change?
        beq     crash_spawn_bomb_at_apex               ; no — skip H-flip
        lda     ent_flags,x             ; toggle sprite H-flip
        eor     #$40                    ; flip bit 6
        sta     ent_flags,x             ; store updated flags
crash_spawn_bomb_at_apex:  jsr     crash_spawn_bomb           ; spawn crash bomb
        lda     ent_var2,x              ; restore movement direction
        sta     ent_facing,x            ; keep original walk dir
        rts

crash_set_fall_anim:  lda     #$03                ; --- descending ---
        jsr     reset_sprite_anim       ; anim 3 = falling
crash_airborne_done:  rts

; --- set random jump velocity ---
crash_set_random_jump_vel:  lda     #$88                ; Y velocity = $07.88 (upward)
        sta     ent_yvel_sub,x          ; set Y velocity sub-pixel
        lda     #$07                    ; Y vel pixel = 7 (upward)
        sta     ent_yvel,x              ; set Y velocity pixel
        lda     $E4                     ; RNG: advance LFSR
        adc     $E5                     ; add to advance RNG state
        sta     $E5                     ; store new RNG value
        and     #$03                    ; pick 1 of 4 X velocities
        tay                             ; use as table index
        lda     doc_crash_jump_x_velocity_sub_table,y ; load random X vel sub
        sta     ent_xvel_sub,x          ; set X velocity sub-pixel
        lda     doc_crash_jump_x_velocity_table,y ; load random X vel pixel
        sta     ent_xvel,x              ; set X velocity pixel
        rts

doc_crash_jump_x_velocity_sub_table:  .byte   $00,$80,$00,$00 ; X velocity sub table
doc_crash_jump_x_velocity_table:  .byte   $01,$01,$01,$02 ; X velocity table
; --- face player and flip sprite without changing movement direction ---
crash_face_player_sprite:  lda     ent_facing,x        ; save movement direction
        sta     ent_var2,x              ; save current facing
        lda     ent_facing,x            ; save facing for compare
        pha                             ; push onto stack
        jsr     face_player             ; face player
        pla                             ; restore old facing
        cmp     ent_facing,x            ; did facing change?
        beq     crash_restore_facing               ; no — skip H-flip
        lda     ent_flags,x             ; facing changed — flip sprite H
        eor     #$40                    ; flip bit 6
        sta     ent_flags,x             ; store updated flags
crash_restore_facing:  lda     ent_var2,x          ; restore original facing
        sta     ent_facing,x            ; keep original walk dir
        rts

; --- spawn Crash Bomb projectile (checks for existing one first) ---
crash_spawn_bomb:  ldy     #$1F                ; scan enemy slots $10-$1F
        lda     #$80                    ; crash bomb spawn ID = $80
crash_scan_bomb_loop:  cmp     ent_spawn_id,y      ; bomb already exists?
        beq     crash_spawn_bomb_done               ; yes — abort spawn
        dey                             ; next slot
        cpy     #$0F                    ; scanned all enemy slots?
        bne     crash_scan_bomb_loop               ; no — keep scanning
        jsr     find_enemy_freeslot_y   ; find free enemy slot
        bcs     crash_spawn_bomb_done               ; no free slot — abort
        sty     temp_00                 ; save child slot index
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y            ; to child entity
        and     #$01                    ; isolate right/left bit
        tay                             ; use as X offset index
        lda     ent_x_px,x              ; parent X position
        clc                             ; add facing-based offset
        adc     doc_flash_projectile_x_offset_table,y ; offset by facing direction
        ldy     temp_00                 ; restore child slot index
        sta     ent_x_px,y              ; set child X position
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to child entity
        lda     ent_y_px,x              ; copy parent Y position
        sta     ent_y_px,y              ; to child entity
        lda     #$00                    ; bomb has 0 HP
        sta     ent_hp,y                ; set child HP
        lda     #$80                    ; crash bomb spawn ID
        sta     ent_spawn_id,y          ; mark spawn tracking
        lda     #$0B                    ; child OAM ID = $0B
        jsr     init_child_entity       ; initialize child entity
        lda     #$80                    ; enable contact damage
        sta     ent_hitbox,y            ; set child hitbox
        lda     #$A7                    ; routine $A7 = crash bomb AI
        sta     ent_routine,y           ; set child AI routine
crash_spawn_bomb_done:  rts

; =============================================================================
; DOC CRASH BOMB PROJECTILE ($A7)
; =============================================================================
; Crash Bomb projectile AI. Moves vertically/horizontally toward target using
; homing velocity calculation. Two phases:
;   Phase 1: move toward target, set explosion anim on wall contact
;   Phase 2: play explosion anim, then despawn via routine $48
; =============================================================================

crash_bomb_init:  lda     ent_status,x        ; --- init ---
        and     #$0F                    ; low nibble = sub-state
        bne     crash_bomb_phase_dispatch               ; skip init if nonzero
        lda     #$1E                    ; 30-frame explosion timer
        sta     ent_timer,x             ; explosion countdown = 30 frames
        lda     #$00                    ; homing speed sub = $00
        sta     $02                     ; set temp $02
        lda     #$04                    ; homing speed pixel = $04
        sta     $03                     ; set temp $03
        lda     ent_x_px,x              ; save original X position
        sta     ent_var1,x              ; store in var1
        lda     ent_facing,x            ; get facing direction
        and     #$01                    ; isolate right/left bit
        tay                             ; use as offset index
        lda     ent_x_px,x              ; current X position
        clc                             ; offset for homing calc
        adc     doc_crash_bomb_x_offset_table,y ; add facing-based X offset
        sta     ent_x_px,x              ; set offset X for targeting
        jsr     calc_homing_velocity    ; compute homing velocities
        lda     ent_var1,x              ; restore original X
        sta     ent_x_px,x              ; revert X position
        lda     $0C                     ; homing direction result
        sta     ent_facing,x            ; set as bomb facing
        inc     ent_status,x            ; advance to flight phase
crash_bomb_phase_dispatch:  lda     ent_status,x        ; --- phase dispatch ---
        and     #$02                    ; check bit 1 (exploding)
        bne     crash_bomb_explode_anim               ; exploding — play anim
        lda     ent_facing,x            ; --- flight phase ---
        and     #$08                    ; bit 3 = move up
        beq     crash_bomb_move_down               ; not set — move down
        ldy     #$13                    ; hitbox Y=$13
        jsr     move_up_collide         ; move up with collision
        jmp     crash_bomb_check_wall_v               ; skip down movement

crash_bomb_move_down:  ldy     #$12                ; hitbox Y=$12
        jsr     move_down_collide       ; move down with collision
crash_bomb_check_wall_v:  bcs     crash_bomb_explode_start           ; hit wall — start explosion
        lda     ent_facing,x            ; check horizontal dir
        and     #$01                    ; facing right?
        beq     crash_bomb_move_left               ; no — move left
        ldy     #$1E                    ; hitbox Y=$1E
        jsr     move_right_collide      ; move right with collision
        lda     ent_flags,x             ; clear H-flip (face right)
        and     #$BF                    ; clear bit 6
        sta     ent_flags,x             ; store updated flags
        jmp     crash_bomb_check_wall_h               ; check for wall hit

crash_bomb_move_left:  ldy     #$1F                ; hitbox Y=$1F
        jsr     move_left_collide       ; move left with collision
        lda     ent_flags,x             ; set H-flip (face left)
        ora     #$40                    ; set bit 6
        sta     ent_flags,x             ; store updated flags
crash_bomb_check_wall_h:  bcc     crash_bomb_done           ; no wall hit — continue
crash_bomb_explode_start:  lda     #$0C                ; --- wall hit: explode ---
        jsr     reset_sprite_anim       ; anim $0C = explosion start
        inc     ent_status,x            ; advance to explode phase
        rts

crash_bomb_explode_anim:  lda     ent_anim_id,x       ; --- explosion phase ---
        cmp     #$0C                    ; still in first explosion?
        bne     crash_bomb_explode_tick               ; no — skip anim advance
        lda     ent_anim_frame,x        ; check if anim finished
        ora     ent_anim_state,x        ; frame or state nonzero?
        bne     crash_bomb_explode_tick               ; still playing — wait
        lda     #$0D                    ; anim $0D = explosion loop
        jsr     reset_sprite_anim       ; advance explosion anim
crash_bomb_explode_tick:  dec     ent_timer,x         ; count down explosion timer
        bne     crash_bomb_done               ; not done — keep exploding
        lda     #$59                    ; anim $59 = despawn effect
        jsr     reset_sprite_anim       ; set despawn animation
        lda     #$00                    ; clear timer
        sta     ent_timer,x             ; reset timer to 0
        lda     #$48                    ; routine $48 = generic despawn
        sta     ent_routine,x           ; hand off to despawn routine
crash_bomb_done:  rts

doc_crash_bomb_x_offset_table:  .byte   $18,$E8 ; X offset: right=+24, left=-24

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

metal_init_and_dispatch:  lda     ent_status,x        ; --- init ---
        and     #$0F                    ; extract state from low nibble
        bne     metal_state_dispatch               ; skip init if already running
        lda     #$B4
        sta     ent_timer,x             ; patrol timer = 180 frames
        lda     #$07
        sta     ent_var3,x              ; throw delay after anim
        jsr     metal_set_random_jump_vel               ; set random jump velocity
        inc     ent_status,x            ; advance to state 1 (ground)
metal_state_dispatch:  lda     ent_status,x        ; --- state dispatch ---
        and     #$0F                    ; extract state from low nibble
        cmp     #$02                    ; state 2?
        beq     metal_jump_update               ; state 2: jumping
        cmp     #$03                    ; state 3?
        beq     metal_bounce_update               ; state 3: wall bounce
        jsr     wood_force_anim_tick               ; reset animation state
        jsr     entity_x_dist_to_player ; get horizontal distance to player
        cmp     #$28                    ; within 40px?
        bcs     metal_patrol_countdown               ; too far — keep patrolling
        lda     #$2A                    ; set Y velocity sub for bounce
        sta     ent_yvel_sub,x          ; high upward bounce
        lda     #$08                    ; set Y velocity for bounce
        sta     ent_yvel,x              ; high upward bounce
        inc     ent_status,x            ; advance to state 3 (bounce)
        inc     ent_status,x            ; two increments = +2
        rts                             ; done — now in wall bounce

metal_patrol_countdown:  dec     ent_timer,x         ; count down patrol timer
        bne     metal_check_b_press               ; not expired yet
        lda     #$B4                    ; reload timer = 180 frames
        sta     ent_timer,x             ; restart patrol timer
        inc     ent_status,x            ; advance to state 2 (jump)
        rts

metal_check_b_press:  lda     joy1_press          ; check player input
        and     #BTN_B                  ; player pressed B (shoot)?
        beq     metal_patrol_done               ; no — keep waiting
        inc     ent_status,x            ; react: advance to jump state
metal_patrol_done:  rts

metal_jump_update:  lda     ent_anim_id,x       ; --- state 2: jumping ---
        cmp     #$04                    ; throwing animation?
        bne     metal_jump_gravity               ; no — skip throw delay
        dec     ent_var3,x              ; count down throw anim delay
        bne     metal_patrol_done               ; still in throw pose — wait
metal_jump_gravity:  ldy     #$1E                ; gravity collision box
        jsr     move_vertical_gravity   ; apply gravity + collision
        bcc     metal_jump_anim_throw               ; no ground hit — still airborne
        jsr     metal_set_random_jump_vel               ; pick new random jump velocity
        lda     #$1D                    ; standing anim ID
        jsr     reset_sprite_anim       ; set standing animation
        dec     ent_status,x            ; back to state 1 (ground)
        rts

metal_jump_anim_throw:  lda     #$03                ; jumping anim ID
        jsr     reset_sprite_anim       ; set jumping animation
        lda     ent_yvel,x              ; check Y velocity sign
        bpl     metal_jump_done               ; falling — don't throw
        dec     ent_var1,x              ; count down throw interval
        bne     metal_jump_done               ; not time to throw yet
        lda     ent_var2,x              ; reload throw interval
        sta     ent_var1,x              ; reset throw countdown
        jsr     metal_spawn_blade               ; spawn Metal Blade projectile
        lda     #$04                    ; throw anim ID
        jsr     reset_sprite_anim       ; set throw animation
        lda     #$07                    ; throw anim hold = 7 frames
        sta     ent_var3,x              ; set throw anim delay
metal_jump_done:  rts

metal_bounce_update:  lda     ent_anim_frame,x    ; --- state 3: wall bounce ---
        ora     ent_anim_state,x        ; check if anim active
        bne     metal_bounce_move_horiz               ; anim already playing?
        lda     #$03                    ; jumping anim ID
        jsr     reset_sprite_anim       ; start jump animation
metal_bounce_move_horiz:  lda     ent_facing,x        ; check facing direction
        and     #$01                    ; bit 0: 1=right
        beq     metal_bounce_move_left               ; facing left — branch
        jsr     move_sprite_right       ; move right (no collision)
        jmp     metal_bounce_gravity               ; skip left move

metal_bounce_move_left:  jsr     move_sprite_left    ; move left (no collision)
metal_bounce_gravity:  ldy     #$1E                ; gravity collision box
        jsr     move_vertical_gravity   ; apply gravity + collision
        bcc     metal_bounce_midair_throw               ; no ground hit — still airborne
        lda     #$2A                    ; set Y velocity sub for bounce
        sta     ent_yvel_sub,x          ; high upward rebound
        lda     #$08                    ; set Y velocity for bounce
        sta     ent_yvel,x              ; high upward rebound
        lda     #$1D                    ; standing anim ID
        jsr     reset_sprite_anim       ; set standing animation
        lda     ent_facing,x            ; check facing for H-flip
        and     #$01                    ; bit 0: 1=right
        beq     metal_bounce_set_hflip               ; facing left — set H-flip
        lda     ent_flags,x             ; facing right — clear H-flip
        and     #$BF                    ; clear bit 6 (H-flip)
        sta     ent_flags,x             ; store cleared flags
        bne     metal_bounce_reverse_dir               ; always taken (nonzero)
metal_bounce_set_hflip:  lda     ent_flags,x         ; facing left — set H-flip
        ora     #$40                    ; set bit 6 (H-flip)
        sta     ent_flags,x             ; store flipped flags
metal_bounce_reverse_dir:  lda     ent_facing,x        ; reverse facing direction
        eor     #$03                    ; toggle bits 0-1 (L<->R)
        sta     ent_facing,x            ; save reversed facing
        dec     ent_status,x            ; back to state 1 (ground)
        dec     ent_status,x            ; two decrements = -2
        jsr     metal_set_random_jump_vel               ; pick new random jump velocity
        lda     #$07                    ; throw anim hold = 7 frames
        sta     ent_var3,x              ; set throw anim delay
        rts

metal_bounce_midair_throw:  lda     ent_yvel,x          ; --- midair throw (bounce) ---
        bpl     metal_bounce_throw_done               ; falling — skip throw
        lda     ent_var2,x              ; throw interval reload value
        beq     metal_bounce_throw_done               ; zero = already threw — skip
        lda     ent_facing,x            ; save current facing
        sta     $0F                     ; preserve in temp
        lda     ent_facing,x            ; check facing for H-flip
        and     #$01                    ; bit 0: 1=right
        beq     metal_bounce_set_hflip_left               ; facing left — set H-flip
        lda     ent_flags,x             ; facing right — clear H-flip
        and     #$BF                    ; clear bit 6 (H-flip)
        sta     ent_flags,x             ; store cleared flags
        bne     metal_bounce_face_and_throw               ; always taken (nonzero)
metal_bounce_set_hflip_left:  lda     ent_flags,x         ; facing left — set H-flip
        ora     #$40                    ; set bit 6 (H-flip)
        sta     ent_flags,x             ; store flipped flags
metal_bounce_face_and_throw:  jsr     face_player         ; turn toward player to throw
        jsr     metal_spawn_blade               ; spawn Metal Blade projectile
        lda     #$04                    ; throw anim ID
        jsr     reset_sprite_anim       ; set throw animation
        lda     #$00                    ; clear throw reload value
        sta     ent_var2,x              ; one throw per bounce only
        lda     $0F                     ; restore original facing
        sta     ent_facing,x            ; resume bounce direction
metal_bounce_throw_done:  rts

; --- set random jump velocity from lookup tables ---
metal_set_random_jump_vel:  lda     $E4                 ; RNG: advance LFSR
        adc     $E5                     ; advance RNG state
        sta     $E5                     ; store new RNG value
        and     #$03                    ; pick 1 of 4 velocity sets
        tay                             ; index into velocity tables
        lda     doc_metal_jump_y_velocity_sub_table,y ; load Y velocity sub-pixel
        sta     ent_yvel_sub,x          ; set jump Y velocity sub
        lda     doc_metal_jump_y_velocity_table,y ; load Y velocity whole pixel
        sta     ent_yvel,x              ; set jump Y velocity
        lda     doc_metal_jump_throw_interval_table,y ; load throw interval
        sta     ent_var1,x              ; throw interval
        sta     ent_var2,x              ; throw interval reload
        rts

doc_metal_jump_y_velocity_sub_table:  .byte   $88,$00,$9E,$88 ; Y velocity sub table
doc_metal_jump_y_velocity_table:  .byte   $06,$08,$04,$06 ; Y velocity table
doc_metal_jump_throw_interval_table:  .byte   $0A,$08,$0D,$0A ; throw interval table
; --- spawn Metal Blade projectile ---
metal_spawn_blade:  jsr     find_enemy_freeslot_y ; find free enemy slot
        bcs     metal_spawn_blade_done               ; no free slot — abort
        sty     temp_00                 ; save child slot index
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y            ; to child entity
        and     #$01                    ; extract direction bit
        tay                             ; index for X offset table
        lda     ent_x_px,x              ; parent X position
        clc                             ; add facing-based offset
        adc     doc_flash_projectile_x_offset_table,y ; left=-23, right=+23
        ldy     temp_00                 ; restore child slot index
        sta     ent_x_px,y              ; set child X position
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y             ; to child entity
        lda     ent_y_px,x              ; copy parent Y position
        sta     ent_y_px,y              ; to child entity
        lda     #$00                    ; blade has 0 HP
        sta     ent_hp,y                ; set child HP
        lda     #$0E                    ; blade sprite ID
        jsr     init_child_entity       ; init child with sprite $0E
        lda     #$80                    ; contact damage flag
        sta     ent_hitbox,y            ; enable contact damage
        lda     #$A8                    ; Metal Blade AI routine ID
        sta     ent_routine,y           ; set blade AI to $A8
metal_spawn_blade_done:  rts

; =============================================================================
; METAL BLADE PROJECTILE AI ($A8)
; =============================================================================
; Metal Blade projectile. Calculates homing direction on init, then moves
; vertically and horizontally toward target.
; =============================================================================

metal_blade_init:  lda     ent_status,x        ; --- init ---
        and     #$0F                    ; extract state from low nibble
        bne     metal_blade_move               ; skip init if already active
        inc     ent_status,x            ; advance to moving state
        lda     #$00                    ; min homing speed = 0
        sta     $02                     ; set speed floor
        lda     #$04                    ; max homing speed = 4
        sta     $03                     ; set speed ceiling
        jsr     calc_homing_velocity    ; calculate aim at player
        lda     $0C                     ; homing direction result
        sta     ent_facing,x            ; set blade facing/direction
metal_blade_move:  lda     ent_facing,x        ; --- movement ---
        and     #$08                    ; bit 3 = moving up?
        beq     metal_blade_move_down               ; not set — move down
        jsr     move_sprite_up          ; move blade upward
        jmp     metal_blade_move_horiz               ; skip down movement

metal_blade_move_down:  jsr     move_sprite_down    ; move blade downward
metal_blade_move_horiz:  jmp     flash_homing_move_horiz           ; move blade horizontally

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
