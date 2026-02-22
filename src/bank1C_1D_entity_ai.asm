process_sprites_j:
; =============================================================================
; MEGA MAN 3 (U) — BANKS $1C-$1D — SPRITE PROCESSING & ENTITY AI
; =============================================================================
; Entity AI dispatch, sprite processing loop, hit detection callbacks,
; and AI routines for all standard enemies and mini-bosses.
; Bank $1C at $8000, Bank $1D at $A000.
;
; Annotation: ~77% — 674 labels named, 1984 inline comments
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANKS $1C-$1D — SPRITE PROCESSING & ENTITY AI
; =============================================================================
; Swappable bank pair mapped to $8000-$BFFF. Contains:
;   - process_sprites: main entity processing loop (iterates all 32 slots)
;   - check_player_hit: contact damage from entity to player
;   - check_weapon_hit: weapon-entity collision and damage
;   - 75+ named enemy/weapon main routines (main_*)
;   - sprite_main_ptr_lo/hi: dispatch tables for routine indices
;
; Entity Dispatch Mechanism (differs from MM4's page-based system):
;   Each entity has a "main routine index" at ent_routine,x.
;   process_sprites reads ent_routine,x, selects the appropriate PRG bank:
;     Indices $00-$9F → bank $1D (this bank pair)
;     Indices $A0-$AF → bank $04 (Doc Robot: Flash/Wood/Crash/Metal)
;     Indices $B0-$BF → bank $05 (Doc Robot: Bubble/Heat/Quick/Air)
;     Indices $C0-$CF → bank $06 (Robot Masters: Needle/Magnet/Top/Shadow)
;     Indices $D0-$DF → bank $07 (Robot Masters: Hard/Spark/Snake/Gemini)
;     Indices $E0-$FF → bank $12 (Fortress bosses + special entities)
;   Then jumps through sprite_main_ptr_lo/hi tables to the routine.
;
; Entity Slot Layout:
;   Slot $00       = Mega Man (player, skipped in loop)
;   Slots $01-$0F  = weapons / projectiles
;   Slots $10-$1F  = enemies / items / bosses
;   Only slots >= $10 get weapon-hit checks.
;   Slot bit 7 of ent_hitbox,x = causes player contact damage.
;
; Spark Freeze: $5B/$5C hold frozen entity slot indices.
;   Frozen entities skip AI, get constant weapon-collision re-checks.
;   Animation counter ent_anim_frame,x held at 0 to freeze sprite frame.
;
; See bank1E_1F.asm header for full entity memory map.
;
; MM4 cross-reference:
;   process_sprites ($1C800C) → code_3A8014 (24 slots, $18 stride in MM4)
;   check_player_hit ($1C8097) → code_3A81CC
;   check_weapon_hit ($1C8102) → code_3FF95D
;
; Annotation: partial — all 75+ entity entry points named, 662 auto labels remain
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

L0000           := $0000
LE11A           := $E11A
LE8D6           := $E8D6
LEE13           := $EE13
LEE57           := $EE57
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
apply_y_velocity_fall           := $F7A8
apply_y_velocity_rise           := $F7C8
reset_gravity           := $F81B
reset_sprite_anim           := $F835
init_child_entity           := $F846
face_player           := $F869
set_sprite_hflip           := $F883
submit_sound_ID_D9           := $F898
submit_sound_ID           := $F89A
entity_y_dist_to_player           := $F8B3
entity_x_dist_to_player           := $F8C2
calc_direction_to_player           := $F8D9
LF954           := $F954
check_player_collision           := $FAE2
LFAF6           := $FAF6
check_sprite_weapon_collision           := $FB7B
find_enemy_freeslot_y           := $FC53
calc_homing_velocity           := $FC63
divide_8bit           := $FCEB
process_frame_yield_full           := $FD6E
update_CHR_banks           := $FF3C
select_PRG_banks           := $FF6B

.segment "BANK1C"

        jmp     process_sprites

process_sprites_top_spin_check:  jmp     check_weapon_hit_top_spin_entry

        jmp     check_weapon_hit_boss_death_sound

        jmp     check_player_hit

process_sprites:  lda     #$55          ; $99 = $00.55 (0.332 px/frame²)
        sta     gravity                     ; set each frame for gameplay physics
        ldx     #$01                    ; start at weapons
        stx     sprite_slot                     ; (skip mega man)
process_sprites_loop_entry:  ldy     #$01
        cpx     spark_freeze_a
        beq     process_sprites_spark_freeze_loop                   ; if this sprite slot
        iny                             ; is spark frozen,
        cpx     spark_freeze_b                     ; skip regular processing
        beq     process_sprites_spark_freeze_loop
        lda     ent_status,x                 ; if sprite inactive,
        bpl     process_sprites_next_slot                   ; continue loop
        ldy     #$1D                    ; select $A000~$BFFF bank
        lda     ent_routine,x                 ; bank $1D for main routine
        cmp     #$E0                    ; indices $00~$9F
        bcc     process_sprites_bank_calc_shift
        ldy     #$12                    ; bank $12 for $E0~$FF
        bne     process_sprites_bank_compare
process_sprites_bank_calc_shift:  lsr     a
        lsr     a                       ; in between $9F and $E0,
        lsr     a                       ; index >> 4 - $06
        lsr     a                       ; meaning bank $04 for $A0~$AF,
        cmp     #$0A                    ; $05 for $B0~$BF,
        bcc     process_sprites_bank_compare                   ; $06 for $C0~$CF,
        sec                             ; $07 for $D0~$DF
        sbc     #$06
        tay
process_sprites_bank_compare:  cpy     prg_bank
        beq     process_sprites_load_routine_ptr                   ; if not already selected,
        sty     prg_bank
        txa                             ; preserve X
        pha                             ; select new $A000~$BFFF bank
        jsr     select_PRG_banks                   ; restore X
        pla
        tax
process_sprites_load_routine_ptr:  ldy     ent_routine,x
        lda     sprite_main_ptr_lo,y
        sta     L0000                   ; load sprite main routine
        lda     sprite_main_ptr_hi,y    ; pointer (low then high)
        sta     $01
        lda     #$80
        pha                             ; return address
        lda     #$76                    ; (skips some code below)
        pha
        jmp     (L0000)                 ; jump to sprite main

; if spark freeze effect active

process_sprites_spark_freeze_loop:  lda     #$00                    ; clear spark freeze slot
        sta     boss_active,y                   ; recheck weapon collision
        jsr     check_sprite_weapon_collision                   ; if none, spark cleared
        bcs     process_sprites_spark_freeze_wpn
        txa                             ; if so, reapply
        ldy     $10                     ; spark freeze slot
        sta     boss_active,y
        lda     #$00                    ; constantly reset
        sta     ent_anim_frame,x                 ; animation counter
        beq     process_sprites_spark_freeze_wpn                   ; to keep sprite from animating
        cpx     #$10
        bcc     process_sprites_next_slot                   ; check being hit by weapon
        lda     ent_routine,x                 ; only for enemies with
        beq     process_sprites_next_slot                   ; nonzero main indices
        jsr     check_weapon_hit
process_sprites_spark_freeze_wpn:  lda     ent_hitbox,x                 ; if this sprite can
        bpl     process_sprites_next_slot                   ; cause player damage,
        jsr     check_player_hit        ; check for that
process_sprites_next_slot:  inc     sprite_slot
        ldx     sprite_slot                     ; go to next sprite
        cpx     #$20                    ; in X as well as $EF
        beq     process_sprites_done                   ; stop at $20
        jmp     process_sprites_loop_entry

process_sprites_done:  rts

; checks if a sprite is damaging Mega Man
; applies damage if so
; parameters:
; X: sprite slot
; ---------------------------------------------------------------------------
; check_player_hit — contact damage from entity to player
; Called from process_sprites for entities with bit 7 of ent_hitbox,x set.
; Checks AABB collision between entity X and player, applies damage.
; State transitions: → $06 (damage) on hit, → $0E (death) if HP depleted.
; Immune during: $06 (already damaged), $0E (dead), $0C (victory).
; Also immune if $39 != 0 (i-frames / invincibility timer).
; $A2 = player HP (low 5 bits = current HP, bit 7 = death flag)
; Damage amount comes from table at $A000 indexed by entity routine ent_routine,x.
; ---------------------------------------------------------------------------

check_player_hit:  lda     ent_anim_id        ; check player animation
        cmp     #$A4                    ; $A4 = death/invincible anim (skip)
        beq     process_sprites_done
        stx     $0F                     ; save entity slot
        lda     prg_bank                     ; save current bank
        pha
        lda     #$0A                    ; switch to bank $0A
        sta     prg_bank                     ; (damage tables at $A000)
        jsr     select_PRG_banks
        ldx     $0F                     ; restore entity slot
        lda     invincibility_timer                     ; i-frames timer
        bne     check_player_hit_done               ; skip if invincible
        lda     player_state                     ; check player state
        cmp     #PSTATE_DAMAGE                    ; already taking damage?
        beq     check_player_hit_done               ; skip
        cmp     #PSTATE_DEATH                    ; already dead?
        beq     check_player_hit_done               ; skip
        cmp     #PSTATE_VICTORY                    ; victory cutscene?
        beq     check_player_hit_done               ; skip
        jsr     check_player_collision                   ; AABB overlap test
        bcs     check_player_hit_done               ; no collision → skip
        lda     #PSTATE_DAMAGE                    ; --- CONTACT HIT ---
        sta     player_state                     ; state → $06 (damage)
        lda     #SFX_PLAYER_HIT                    ; SFX $16 = damage sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     player_hp                     ; player HP
        and     #$1F                    ; isolate HP value (0-28)
        beq     check_player_hit_done               ; already 0 → skip damage calc
        ldy     ent_routine,x                 ; entity routine index
        lda     player_hp                     ; current HP
        and     #$1F
        sec
        sbc     proto_man_opcode_data,y                 ; subtract damage from table
        php                             ; save carry (underflow = dead)
        ora     #$80                    ; set bit 7 (HP dirty flag)
        sta     player_hp
        plp
        beq     check_player_hit_killed                   ; HP == 0 → dead
        bcs     check_player_hit_done               ; HP > 0 → survived, done
check_player_hit_killed:  lda     #$80                    ; --- PLAYER KILLED ---
        sta     player_hp                     ; HP = 0 with dirty flag
        lda     #PSTATE_DEATH                    ; state → $0E (death)
        sta     player_state
        lda     #SNDCMD_STOP                    ; SFX $F2 = stop music
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     #SFX_DEATH                    ; SFX $17 = death sound
        jsr     submit_sound_ID                   ; submit_sound_ID
check_player_hit_done:  pla                         ; restore bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $0F                     ; restore entity slot
        rts

; checks for and applies effects/damage
; from a sprite being hit by a weapon
; parameters:
; X: sprite slot

check_weapon_hit:  lda     ent_hitbox,x      ; if vulnerable & shot tink
        and     #$60                    ; flags BOTH off,
        beq     check_weapon_hit_carry_return                   ; return
check_weapon_hit_top_spin_entry:  lda     ent_anim_id
        cmp     #$A3                    ; if Mega Man OAM ID == $A3
        bne     check_weapon_hit_collision_check                   ; he is top spinning
        jmp     check_weapon_hit_top_spin_contact

check_weapon_hit_collision_check:  jsr     check_sprite_weapon_collision                   ; if no weapon collision
        bcs     check_weapon_hit_carry_return                   ; return
        lda     ent_hitbox,x
        and     #$20                    ; if shot tink flag on,
        beq     check_weapon_hit_damage_calc                   ; bounce diagonally up
check_weapon_hit_tink_sound:  lda     #$19                    ; play tink sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     $10                     ; y = tinked weapon slot
        lda     ent_facing,y
        eor     #$03                    ; flip horizontal facing
        sta     ent_facing,y
        lda     #$00
        sta     ent_yvel_sub,y                 ; 4 pixel per frame
        lda     #$FC                    ; upward speed
        sta     ent_yvel,y
        lda     #$00                    ; clear hitbox flags
        sta     ent_hitbox,y
        lda     #$0F                    ; tinked shot routine
        sta     ent_routine,y
check_weapon_hit_carry_return:  sec                             ; return carry on
        rts

check_weapon_hit_damage_calc:  lda     #$18                    ; play damage sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     prg_bank
        pha                             ; preserve and select
        stx     $0F                     ; $0A as $A000~$BFFF bank
        lda     #$0A
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $0F                     ; restore X (sprite slot)
        ldy     current_weapon
        lda     weapon_damage_ptr_lo,y  ; grab damage table pointer for
        sta     L0000                   ; currently equipped weapon
        lda     weapon_damage_ptr_hi,y
        sta     $01
        ldy     ent_routine,x                 ; if damage for main routine index
        lda     (L0000),y               ; is nonzero, do stuff
        bne     check_weapon_hit_spark_check
        jsr     check_weapon_hit_tink_sound                   ; else tink shot
        jmp     check_weapon_hit_restore_bank

check_weapon_hit_spark_check:  lda     current_weapon                     ; if weapon is
        cmp     #WPN_SPARK                    ; anything but spark
        bne     check_weapon_hit_ack_check                   ; apply normal damage
        lda     (L0000),y               ; if damage value is zero,
        beq     check_weapon_hit_spark_freeze_done                   ; don't do anything
        cmp     #$58                    ; if damage isn't magic number $58,
        bne     check_weapon_hit_ack_check                   ; apply normal damage

; apply spark freeze effect
        txa                             ; Y = spark slot
        ldy     $10                     ; set shot sprite slot
        sta     boss_active,y                   ; for this weapon slot
        lda     ent_status,y
        ora     #$01                    ; turn on freeze state for
        sta     ent_status,y                 ; spark shot
        lda     #$9D                    ; if OAM ID is already
        cmp     ent_anim_id,y                 ; electric shocking, don't
        beq     check_weapon_hit_spark_freeze_done                   ; bother resetting it up

; initialize spark freeze animation & position
        sta     ent_anim_id,y
        lda     #$00                    ; set OAM ID to $9D (shocking),
        sta     ent_anim_state,y                 ; reset animation frame & counter
        sta     ent_anim_frame,y                 ; & reset shock timer
        sta     ent_timer,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x                 ; set shock position same
        sta     ent_x_scr,y                 ; as shot sprite's position
        lda     ent_y_px,x
        sta     ent_y_px,y
check_weapon_hit_spark_freeze_done:  jmp     check_weapon_hit_restore_bank

check_weapon_hit_ack_check:  lda     ent_hp,x                 ; if any hit-ack flags set
        and     #$E0                    ; (bits 7/6/5 = already hit)
        beq     check_weapon_hit_subtract_hp                   ; skip — already took damage
        jmp     check_weapon_hit_weapon_despawn                   ; else don't

check_weapon_hit_subtract_hp:  ldy     ent_routine,x
        lda     ent_hp,x                 ; subtract health by
        sec                             ; damage table value
        sbc     (L0000),y               ; indexed by main routine index
        bcs     check_weapon_hit_hp_stored                   ; minimum health is 00
        lda     #$00                    ; (no negatives)
check_weapon_hit_hp_stored:  sta     ent_hp,x                 ; store new health value
        bne     check_weapon_hit_hp_zero_check                   ; if nonzero, we're still alive

; dead
        lda     ent_routine,x
        cmp     #$52                    ; if Proto Man ($52 or $53)
        beq     check_weapon_hit_hp_zero_check                   ; don't do normal death —
        cmp     #$53                    ; Proto Man has his own
        beq     check_weapon_hit_hp_zero_check                   ; fly-away exit in main_proto_man
        lda     boss_active
        bpl     check_weapon_hit_death_oam_boss                   ; $5A<0: boss active → OAM $71
        lda     #$59                    ; $5A>=0: normal → OAM $59
        bne     check_weapon_hit_death_oam_normal
check_weapon_hit_death_oam_boss:  lda     #$71
check_weapon_hit_death_oam_normal:  jsr     reset_sprite_anim                   ; animate enemy's death
        lda     #$00                    ; turn off collision
        sta     ent_hitbox,x
        lda     ent_routine,x
        cmp     #$30                    ; if not bolton and nutton
        bne     check_weapon_hit_death_routine_other
        lda     #$00                    ; use $00 death routine index
        sta     ent_routine,x                 ; for bolton and nutton
        beq     check_weapon_hit_death_routine_done
check_weapon_hit_death_routine_other:  lda     #$7A                    ; for everything else,
        sta     ent_routine,x                 ; use $7A routine
check_weapon_hit_death_routine_done:  lda     #$90
        sta     ent_flags,x
check_weapon_hit_hp_zero_check:  lda     ent_hp,x                 ; health zero?
        beq     check_weapon_hit_weapon_despawn                   ; don't set boss flags

; not dead
        lda     ent_status,x
        and     #$40                    ; if sprite is a boss
        bne     check_weapon_hit_boss_stage_check
        lda     ent_hp,x
        ora     #$20                    ; set bit 5 = hit-ack flag
        sta     ent_hp,x                 ; (prevents double-damage this frame)
        bne     check_weapon_hit_weapon_despawn
check_weapon_hit_boss_stage_check:  lda     stage_id                     ; if stage == Wily 4
        cmp     #STAGE_WILY4                    ; or < Wily 1
        beq     check_weapon_hit_boss_ack_all                   ; this means robot master
        cmp     #STAGE_WILY1                    ; or doc robot bosses
        bcs     check_weapon_hit_weapon_despawn
check_weapon_hit_boss_ack_all:  lda     ent_hp,x
        ora     #$E0                    ; for doc/robot masters,
        sta     ent_hp,x                 ; set all hit-ack flags (bits 7/6/5)
check_weapon_hit_weapon_despawn:  lda     current_weapon
        cmp     #WPN_TOP                    ; if weapon is top spin
        beq     check_weapon_hit_restore_bank                   ; no shot to despawn
        ldy     $10
        lda     #$00                    ; despawn the shot
        sta     ent_status,y
        lda     current_weapon
        cmp     #WPN_GEMINI
        bne     check_weapon_hit_restore_bank                   ; if weapon is gemini laser,
        lda     #$00                    ; despawn all three shots
        sta     $0301
        sta     $0302
        sta     $0303
check_weapon_hit_restore_bank:  pla
        sta     prg_bank                     ; restore bank
        jsr     select_PRG_banks                   ; and sprite slot
        ldx     $0F
        clc
        lda     ent_status,x
        and     #$40                    ; is sprite a boss?
        bne     check_weapon_hit_boss_hp_display
check_weapon_hit_return:  rts                             ; if not, return

check_weapon_hit_top_spin_contact:  lda     ent_hitbox,x                 ; shot tink flag also
        and     #$20                    ; implies invulnerable
        bne     check_weapon_hit_return                   ; to top spin, return
        jsr     check_player_collision                   ; check if enemy collidiog with
        bcs     check_weapon_hit_return                   ; player, if not return
        stx     $0F                     ; preserve X
        lda     prg_bank
        pha                             ; preserve $A000-$BFFF bank
        lda     #$0A                    ; select $0A as new bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $0F                     ; restore X
        ldy     ent_routine,x                 ; y = main ID
        lda     weapon_damage_ptr_lo
        sta     L0000
        lda     weapon_damage_ptr_hi
        sta     $01
        lda     $A7                     ; Top Spin ammo
        and     #$1F                    ; isolate ammo count
        sec
        sbc     (L0000),y               ; subtract cost from damage table
        bcs     check_weapon_hit_top_spin_ammo
        lda     #$00                    ; clamp to 0
check_weapon_hit_top_spin_ammo:  ora     #$80                    ; set dirty flag
        sta     $A7
        lda     #PSTATE_TOP_SPIN                    ; state → $0A (Top Spin recoil)
        sta     player_state                     ; player bounces back from contact
        lda     #$08                    ; recoil timer = 8 frames
        sta     ent_timer
        lda     weapon_damage_ptr_lo_special
        sta     L0000
        lda     weapon_damage_ptr_hi_special
        sta     $01
        jmp     check_weapon_hit_ack_check

check_weapon_hit_boss_hp_display:  lda     ent_hp,x                 ; boss health bits
        and     #$1F                    ; mask to HP (low 5 bits)
        ora     #$80                    ; set bit 7 = "boss was hit" flag
        sta     boss_hp_display                     ; store to boss HP mirror
        and     #$7F                    ; did boss die?
        beq     check_weapon_hit_boss_death_sound
        rts                             ; if not, return

check_weapon_hit_boss_death_sound:  lda     #$F2                    ; SFX $F2 = stop music
        jsr     submit_sound_ID_D9                   ; submit_sound_ID_D9
        lda     #SFX_DEATH                    ; SFX $17 = boss death sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     #$1F
boss_death_loop_entry:  lda     boss_active
        bmi     boss_death_spark_routine
        lda     #$7A
        bne     boss_death_spawn_child
boss_death_spark_routine:  lda     #$5B
boss_death_spawn_child:  jsr     init_child_entity               ; init_child_entity
        lda     #$80
        sta     ent_status,y
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
        lda     $D7E1,y
        sta     ent_xvel_sub,y
        lda     $D7F1,y
        sta     ent_xvel,y
        lda     $D801,y
        sta     ent_yvel_sub,y
        lda     $D811,y
        sta     ent_yvel,y
        dey
        cpy     #$0F
        bne     boss_death_loop_entry
        lda     stage_id
        cmp     #STAGE_HARD
        bne     boss_death_weapon_despawn
        lda     #$00
        sta     scroll_y
boss_death_weapon_despawn:  lda     #$00
        sta     $0301
        sta     $0302
        sta     $0303
        sta     ent_var1
        lda     stage_id                     ; current stage
        cmp     #STAGE_WILY4                    ; stage $0F = Wily 4 (refights)
        beq     boss_death_wily4_handler                   ; special handling for refights
        lda     player_state                     ; check player state
        cmp     #PSTATE_DEATH                    ; if player is dead ($0E),
        beq     boss_death_illegal_bytes               ; don't start victory cutscene
        lda     #PSTATE_VICTORY                    ; state → $0C (victory)
        sta     player_state                     ; begin boss defeated cutscene
        lda     #$00
        sta     walk_flag                     ; clear sub-state
        sta     ent_timer                   ; clear player timer
        sta     $0301                   ; despawn weapon slots 1-3
        sta     $0302
        sta     $0303
        lda     #$01                    ; set player OAM to $01 (standing)
        cmp     ent_anim_id                   ; already standing?
        beq     boss_death_normal_return                   ; skip animation reset
        sta     ent_anim_id
        lda     #$00
        sta     ent_anim_state                   ; reset animation frame
        sta     ent_anim_frame                   ; reset animation counter
boss_death_normal_return:  clc
        rts

; Wily 4 (stage $0F) boss refight — no victory cutscene, just unstun and
; spawn the "boss defeated" entity at the boss's position

boss_death_wily4_handler:  lda     player_state
        cmp     #PSTATE_STUNNED                    ; if player was stunned ($0F),
        bne     boss_death_wily4_unstun
        lda     #$00                    ; release to on_ground ($00)
        sta     player_state
boss_death_wily4_unstun:  lda     #$80
        sta     $030F
        lda     #$90
        sta     $058F
        lda     ent_x_px,x
        sta     $036F
        lda     ent_x_scr,x
        sta     $038F
        lda     ent_y_px,x
        sta     $03CF
        lda     #$00
        sta     $03EF
        sta     $05EF
        sta     $05AF
        sta     $048F
        sta     $04EF
        sta     $044F
        sta     $046F
        sta     $050F
        lda     #$F9
        sta     $05CF
        lda     #$64
        sta     $032F
        jsr     LE11A
boss_death_illegal_bytes:  .byte   $18,$60

; weapon damage table pointers, low then high
weapon_damage_ptr_lo:  .byte   $00,$00,$00,$00,$00 ; Buster, Gemini, Needle, Hard, Magnet
weapon_damage_ptr_lo_special:  .byte   $00,$00,$00,$00,$00,$00,$00 ; TopSpin, Snake, RushCoil, Spark, RushMarine, Shadow, RushJet
weapon_damage_ptr_hi:  .byte   $A1,$A4,$A2,$A5,$A3 ; Buster, Gemini, Needle, Hard, Magnet
weapon_damage_ptr_hi_special:  .byte   $A6,$A7,$A1,$A8,$A1,$A9,$A1 ; TopSpin, Snake, RushCoil, Spark, RushMarine, Shadow, RushJet

; low bytes of sprite main routine pointers
; Indexed by entity type. Each byte is the low byte of the AI routine address.
; $00=ret_A $02=dada $03=potton $05=new_shotman $06=hammer_joe $07=peterchy
; $08=bubukan $0A=bomb_flier $0D=yambow $0E=met $12=cannon $14=cloud_platform
; $15/$16=jamacy $17/$18=unknown_0C $1A=mag_fly $1E=gyoraibo $1F=junk_golem
; $20=pickelman_bull $21=giant_springer $24=unknown_14 $25=magnet_force
; $28=gyoraibo $2A=hari_harry $2B=penpen_maker $2C=returning_monking
; $2D=unknown_1B $2E=have_su_bee $2F=beehive $30=bolton_nutton $32=wanaan
; $33=needle_press $34=walking_bomb $35=elecn $37=mechakkero $38=top_man_plat
; $3B=chibee $3D=bomb_flier $3E=spark_falling_plat $3F=ret_B $42=pole
; $47=komasaburo $49=parasyu $4A/$4B=hologran $4C=bomber_pepe $4D=metall_dx
; $4E=petit_snakey $4F=init_tama $52=proto_man
; $55-$5C=robot_master_intro(x8)  $60-$67=item pickups / surprise box
; $68-$6C=junk_block, spinning_wheel, trap/plat, giant_springer, breakable_wall
; $6F/$70=electric_gabyoall
; $78-$7F=magnet_missile, gemini, hard_knuckle, snake, spark, shadow, big_snakey, tama
; $80-$97=boss jump tables (needle/magnet/top/shadow/hard/spark/snake/gemini man)
; $98-$A7=fortress boss jump tables (yellow_devil, wily_machine, gamma, etc.)
sprite_main_ptr_lo:  .byte   $C7,$C9,$FB,$58,$DE,$B4,$FD,$7C
        .byte   $D3,$C8,$14,$49,$12,$C5,$83,$85
        .byte   $0E,$09,$B3,$9B,$8A,$CB,$CB,$E2
        .byte   $E2,$60,$BB,$C9,$C8,$E0,$6B,$44
        .byte   $56,$31,$96,$84,$19,$98,$FE,$3A
        .byte   $6B,$35,$EC,$CB,$C3,$C9,$09,$28
        .byte   $5B,$82,$D7,$FE,$C4,$52,$85,$BD
        .byte   $3E,$60,$60,$B6,$C9,$14,$E5,$C8
        .byte   $C9,$C9,$56,$85,$F7,$35,$3F,$7F
        .byte   $60,$CC,$40,$40,$0A,$C3,$E4,$55
        .byte   $34,$C9,$F7,$F7,$C8,$C8,$60,$C8
        .byte   $53,$53,$53,$53,$53,$53,$53,$53 ; $55-$5C: robot_master_intro (x8)
        .byte   $C8,$C9,$93,$3F,$FD,$F9,$FD,$F9 ; $60-$67: item pickups, surprise box
        .byte   $FD,$FD,$D2,$C8,$2F,$65,$F8,$C8 ; $68-$6F: junk_block, spinning_wheel, etc.
        .byte   $31,$E2,$C8,$94,$AC,$B6,$C8,$C8 ; $70-$77: elec_gabyoall, breakable_wall, etc.
        .byte   $5F,$5F,$51,$C8,$C8,$C8,$C8,$C8 ; (unused padding)
        .byte   $A4,$1E,$96,$DD,$88,$47,$C2,$79 ; $78-$7F: magnet_missile thru shadow_blade
        .byte   $98,$6C,$BE,$A7,$A8,$65,$95,$34 ; $80-$87: big_snakey, tama, doc_robot bosses
        .byte   $E8,$E8,$E8,$E8,$E8,$E8,$E8,$E8 ; $88-$8F: doc_robot_intro (x8)
        .byte   $C8,$C8,$C8,$C8,$C8,$C8,$C8,$C8
        .byte   $00,$03,$06,$09,$0C,$0F,$12,$15
        .byte   $18,$1B,$1E,$21,$24,$27,$2A,$2D
        .byte   $00,$03,$06,$09,$0C,$0F,$12,$15
        .byte   $18,$1B,$1E,$21,$24,$27,$2A,$2D
        .byte   $00,$03,$06,$09,$0C,$0F,$12,$15
        .byte   $18,$1B,$1E,$21,$24,$27,$2A,$2D
        .byte   $00,$03,$06,$09,$0C,$0F,$12,$15
        .byte   $18,$1B,$1E,$21,$24,$27,$2A,$2D
        .byte   $00,$03,$06,$09,$0C,$0F,$12,$15
        .byte   $18,$1B,$1E,$21,$24,$27,$2A,$2D
        .byte   $30,$33,$36,$39,$3C,$3F,$42,$45
        .byte   $48,$4B,$4E,$51,$54,$57,$5A,$5D

; high bytes of sprite main routine pointers
sprite_main_ptr_hi:  .byte   $85,$85,$8A,$8B,$8B,$9D,$8B,$B4
        .byte   $8C,$8D,$8E,$9F,$9F,$96,$97,$98
        .byte   $94,$94,$98,$99,$8F,$8D,$8D,$90
        .byte   $90,$A9,$9A,$85,$85,$AA,$A6,$9B
        .byte   $9C,$9D,$90,$9D,$9C,$A4,$A5,$A6
        .byte   $A6,$A7,$A2,$A7,$AE,$85,$AD,$AE
        .byte   $AB,$AC,$AF,$B2,$B4,$B3,$98,$B1
        .byte   $B2,$A9,$A9,$92,$85,$8E,$B9,$85
        .byte   $85,$85,$B1,$98,$B8,$A7,$A9,$B0
        .byte   $A9,$B5,$B5,$B5,$AA,$99,$95,$BB
        .byte   $BB,$85,$9F,$9F,$85,$85,$A9,$85
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $85,$B7,$95,$A8,$BD,$BD,$BD,$BD
        .byte   $BD,$BD,$BE,$85,$95,$91,$B8,$85
        .byte   $B9,$A1,$85,$91,$B9,$92,$85,$85
        .byte   $94,$94,$BF,$85,$85,$85,$85,$85
        .byte   $86,$87,$87,$87,$88,$89,$89,$8A
        .byte   $8A,$BA,$BB,$BC,$BC,$BD,$BD,$BB
        .byte   $B6,$B6,$B6,$B6,$B6,$B6,$B6,$B6
        .byte   $85,$85,$85,$85,$85,$85,$85,$85
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
        .byte   $A0,$A0,$A0,$A0,$A0
        ldy     #$A0
        ldy     #$60
main_ret_B:
        rts

; ===========================================================================
; main_unknown_1B — Breakable block projectile walker
;\
; | Moves horizontally (left/right per ent_facing direction flags).
; | For weapon slots (X < $10): checks for breakable tiles ($70 type)
; | ahead. If found, queues a metatile clear to erase the block from the
; | nametable, spawns a debris child entity (OAM $71, routine $27), and
; | marks the block as destroyed in the $0110 bitfield so collision
; | checks treat it as passthrough. Max 3 debris entities at once.
; | Used on Gemini Man stages for breakable block destruction.
; /
; ===========================================================================
main_unknown_1B:

        lda     ent_facing,x                 ; check direction flags
        and     #$01                    ; bit 0 = moving right
        beq     unknown_1B_move_left
        jsr     move_sprite_right                   ; move right (unchecked)
        jmp     unknown_1B_check_is_enemy

unknown_1B_move_left:  jsr     move_sprite_left               ; move left (unchecked)
unknown_1B_check_is_enemy:  cpx     #$10                ; only weapon/player slots break blocks
        bcs     unknown_1B_done               ; enemy slots ($10+) → return
        ldy     #$06                    ; check tile at foot height ahead
        jsr     LE8D6
        lda     tile_at_feet_max                     ; tile type = breakable block ($70)?
        cmp     #TILE_DISAPPEAR
        bne     unknown_1B_done               ; no → return
        lda     ent_x_px,x                 ; entity X - camera X
        sec                             ; = screen-relative position
        sbc     camera_x_lo
        cmp     #$10                    ; < $10 → off left edge
        bcc     unknown_1B_done
        cmp     #$F0                    ; >= $F0 → off right edge
        bcs     unknown_1B_done               ; must be visible to break
        jsr     LEE57                   ; erase 2x2 metatile from nametable
        bcs     unknown_1B_done               ; carry set = buffer full, abort
        jsr     find_enemy_freeslot_y                   ; find free slot for debris entity
        bcc     unknown_1B_spawn_debris_init               ; found → spawn debris

; --- no free slot or max debris: become explosion in place ---
unknown_1B_explode_in_place:  lda     #$71                ; OAM $71 = small explosion sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; routine $00 = idle dispatch
        sta     ent_routine,x
        lda     ent_x_px,x                 ; snap X to metatile grid center
        and     #$F0                    ; (16px boundary + 8)
        ora     #$08
        sta     ent_x_px,x
        lda     ent_x_scr,x                 ; preserve X screen
        sta     ent_x_scr,x                 ; (no-op write)
        lda     ent_y_px,x                 ; snap Y to metatile grid center
        and     #$F0
        ora     #$08
        sta     ent_y_px,x
        jmp     unknown_1B_mark_block_destroyed               ; mark block destroyed in bitfield

unknown_1B_done:  rts

; --- free slot found: count existing debris, then spawn child ---

unknown_1B_spawn_debris_init:  sty     $01                 ; save free slot index
        lda     #$00                    ; debris counter = 0
        sta     L0000
        ldy     #$1F                    ; scan enemy slots $10-$1F
unknown_1B_count_debris_loop:  lda     ent_status,y             ; skip inactive slots
        bpl     unknown_1B_debris_loop_next
        lda     ent_routine,y                 ; is this a debris entity (routine $27)?
        cmp     #$27
        bne     unknown_1B_debris_loop_next               ; no → skip
        inc     L0000                   ; count++
unknown_1B_debris_loop_next:  dey                         ; loop $1F down to $10
        cpy     #$0F
        bne     unknown_1B_count_debris_loop
        ldy     $01                     ; restore free slot
        lda     L0000                   ; already 3 debris on screen?
        cmp     #$03
        beq     unknown_1B_explode_in_place               ; yes → just explode, no child
        lda     #$71                    ; spawn child with OAM $71 (explosion)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$27                    ; child AI routine = $27
        sta     ent_routine,y                 ; (falling debris handler)
        lda     ent_x_px,x                 ; snap child X to metatile grid center
        and     #$F0                    ; bit 0 = moving right
        ora     #$08
        sta     ent_x_px,y              ; move right (unchecked)
        lda     ent_x_scr,x                 ; copy X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; snap child Y to metatile grid center
        and     #$F0                    ; only weapon/player slots break blocks
        ora     #$08                    ; enemy slots ($10+) → return
        sta     ent_y_px,y              ; check tile at foot height ahead
        lda     #$00                    ; child has no damage flags (harmless)
        sta     ent_hitbox,y            ; tile type = breakable block ($70)?
        sta     ent_status,x                 ; deactivate parent (breaker entity)
        lda     #$FF                    ; ent_spawn_id = $FF (cleanup marker)
        sta     ent_spawn_id,x          ; entity X - camera X

; --- mark block destroyed in $0110 bitfield ---
unknown_1B_mark_block_destroyed:  stx     L0000               ; save entity slot
        lda     $13                     ; nametable page (bit 0) << 5
        and     #$01                    ; → $00 or $20 (page offset)
        asl     a                       ; must be visible to break
        asl     a                       ; erase 2x2 metatile from nametable
        asl     a                       ; carry set = buffer full, abort
        asl     a                       ; find free slot for debris entity
        asl     a                       ; found → spawn debris
        sta     $01
        lda     $28                     ; $28 = metatile column index
        pha                             ; column >> 1 | page_offset
        lsr     a                       ; → Y = byte index into $0110
        ora     $01                     ; routine $00 = idle dispatch
        tay
        pla                             ; column << 2 AND $04
        asl     a                       ; → bit pair selector
        asl     a                       ; ORA $03 (metatile sub-pos)
        and     #$04                    ; → X = bit index for bitmask_table
        ora     $03                     ; preserve X screen
        tax                             ; (no-op write)
        lda     $0110,y                 ; set destroyed bit via
        ora     $EB82,x                 ; bitmask_table ($80,$40,...,$01)
        sta     $0110,y
        ldx     L0000                   ; restore entity slot
        rts                             ; mark block destroyed in bitfield

; ===========================================================================
; Routine $80 — Item drop / weapon capsule falling
;\
; | State 0: apply initial Y speed (thrown upward), wait until Y+$10
; |   passes player Y → advance to state 1.
; | State 1: fall with $99. On landing (anim frame 4), switch to
; |   routine $81 (item waiting). Check tile ahead: if solid wall,
; |   advance state and clear Y speed. Otherwise, select OAM based on
; |   current weapon ($A0): Snake=$D8, Spark=$D9, Shadow=$D7,
; |   RushCoil=$81, RushMarine=$82, RushJet=$83.
; |   Special case: Rush Marine ($A0=$09) in water ($41=$80) → skip.
; /
; ===========================================================================

        lda     ent_status,x                 ; check state
        and     #$0F
        bne     unknown_1B_state_1_gravity               ; state 1+ → $99 fall

; --- state 0: initial upward toss ---
        jsr     apply_y_speed                   ; apply initial Y velocity
        lda     ent_y_px,x                 ; entity Y + $10
        clc                             ; spawn child with OAM $71 (explosion)
        adc     #$10                    ; if below player Y →
        cmp     ent_y_px                   ; still rising, freeze anim
        bcc     unknown_1B_freeze_anim_timer ; (falling debris handler)
        inc     ent_status,x                 ; → state 1 ($99 fall)

; --- state 1: fall with $99 ---
unknown_1B_state_1_gravity:  ldy     #$00                ; apply $99 + move down
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     unknown_1B_freeze_anim_timer               ; no landing → freeze anim
        lda     ent_anim_state,x                 ; landed: check anim frame
        cmp     #$04                    ; must be frame 4 (final bounce)
        bne     unknown_1B_freeze_anim_return               ; not ready → return
        lda     #$81                    ; switch to routine $81
        sta     ent_routine,x                 ; (item waiting on ground)
        lda     #$80                    ; active, state 0
        sta     ent_status,x            ; deactivate parent (breaker entity)
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        ldy     #$03                    ; check tile at mid-height ahead
        jsr     LE8D6
        lda     $10                     ; solid wall? (bit 4)
        and     #$10                    ; nametable page (bit 0) << 5
        beq     unknown_1B_check_weapon_type               ; no wall → check weapon type
unknown_1B_state_blocked_advance:  inc     ent_status,x             ; advance state (wall blocked)
        lda     #$00                    ; clear Y speed
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
        rts

unknown_1B_check_weapon_type:  lda     current_weapon                 ; current weapon = Rush Marine ($09)?
        cmp     #WPN_RUSH_MARINE        ; column >> 1 | page_offset
        bne     unknown_1B_fetch_weapon_oam               ; no → set weapon OAM
        lda     tile_at_feet_max                     ; tile type = water ($80)?
        cmp     #$80
        bne     unknown_1B_state_blocked_advance               ; not water → wall-blocked path
unknown_1B_fetch_weapon_oam:  lda     ent_flags,x             ; set sprite flag bit 0
        ora     #$01                    ; (direction/visibility)
        sta     ent_flags,x             ; → X = bit index for bitmask_table
        lda     current_weapon                     ; weapon_id - 6, >> 1 = table index
        sec                             ; $06→0, $08→1, $0A→2,
        sbc     #$06                    ; $07→0, $09→1, $0B→2
        lsr     a                       ; bitmask_table ($80,$40,...,$01)
        tay
        lda     unknown_1B_weapon_oam_table,y                 ; OAM from weapon_oam_table
        jsr     reset_sprite_anim                   ; set sprite animation
        rts

unknown_1B_freeze_anim_timer:  lda     #$00                ; freeze animation timer
        sta     ent_anim_frame,x                 ; (keep current frame)
unknown_1B_freeze_anim_return:  .byte   $60

; weapon_oam_table: OAM IDs indexed by (weapon_id - 6) >> 1
; $D8=Search Snake, $D9=Spark Shock, $D7=Shadow Blade
; $81=Rush Coil, $82=Rush Marine, $83=Rush Jet
unknown_1B_weapon_oam_table:  .byte   $D8,$D9,$D7
        sta     ($82,x)
        .byte   $83

; ===========================================================================
; Routine $81 — Item sitting on ground / teleport rise
;\
; | State 0: count down ent_timer timer. While timer > 0, item sits on
; |   ground. If OAM $D8 (Search Snake), freeze anim until frame 0.
; |   When timer < $88, set anim bit 7 (flicker/flash effect).
; |   When timer expires → state 1: become teleport beam.
; | State 1: OAM $13 (teleport beam), accelerate upward using $99
; |   constant $99, move up each frame. When Y screen != 0 → deactivate.
; /
; ===========================================================================
        lda     ent_status,x                 ; check state
        and     #$0F
        bne     unknown_1B_wait_anim_frame_2               ; state 1 → rising

; --- state 0: item sitting on ground, timer countdown ---
        dec     ent_timer,x                 ; decrement wait timer
        beq     unknown_1B_start_teleport_rise               ; timer done → become beam
        lda     ent_anim_id,x                 ; if OAM = $D8 (Search Snake item)
        cmp     #$D8                    ; landed: check anim frame
        bne     unknown_1B_check_early_timer               ; other → skip freeze
        lda     #$00                    ; freeze anim timer
        sta     ent_anim_frame,x        ; switch to routine $81
        lda     ent_anim_state,x                 ; if anim frame != 0 → return
        bne     unknown_1B_check_offscreen               ; (wait for idle frame)
unknown_1B_check_early_timer:  lda     ent_timer,x             ; timer >= $88?
        cmp     #$88                    ; (still early in wait)
        bcs     unknown_1B_check_offscreen               ; → normal display, return
        lda     ent_anim_frame,x                 ; set bit 7 of anim timer
        ora     #$80                    ; (flicker/flash effect
        sta     ent_anim_frame,x                 ; when about to expire)
        rts

; --- timer expired: become teleport beam rising ---

unknown_1B_start_teleport_rise:  inc     ent_status,x             ; advance to state 1
        lda     #$00                    ; clear Y speed
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
        sta     ent_hitbox,x                 ; clear damage flags
        lda     ent_flags,x                 ; clear direction bits 0-1
        and     #$FC                    ; no → set weapon OAM
        sta     ent_flags,x             ; tile type = water ($80)?
        lda     #$13                    ; OAM $13 = teleport beam
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$04                    ; set anim frame to 4
        sta     ent_anim_state,x                 ; (beam animation start)

; --- state 1: accelerate upward and rise off screen ---
unknown_1B_wait_anim_frame_2:  lda     ent_anim_state,x             ; wait for anim frame 2
        cmp     #$02                    ; (beam fully formed)
        bne     unknown_1B_check_offscreen               ; not yet → return
        lda     #$00                    ; freeze anim at frame 2
        sta     ent_anim_frame,x        ; OAM from weapon_oam_table
        lda     ent_yvel_sub,x                 ; Y speed += $99 ($99)
        clc                             ; accelerate upward
        adc     gravity
        sta     ent_yvel_sub,x          ; freeze animation timer
        lda     ent_yvel,x              ; (keep current frame)
        adc     #$00
        sta     ent_yvel,x
        jsr     move_sprite_up                   ; move up (unchecked)
        lda     ent_y_scr,x                 ; if Y screen != 0
        beq     unknown_1B_check_offscreen               ; (off-screen) → deactivate
        lda     #$00                    ; deactivate entity
        sta     ent_status,x
unknown_1B_check_offscreen:  rts

; ===========================================================================
; Routine $82 — Horizontal player chaser
;\
; | Computes X distance from entity to player (16-bit).
; | Moves toward player at min(distance, 3) px/frame with collision.
; | After moving, copies player facing direction to slot 1 ($0581).
; /
; ===========================================================================

        lda     ent_x_px                   ; player_X - entity_X (16-bit)
        sec
        sbc     ent_x_px,x              ; check state
        pha                             ; save low byte
        lda     ent_x_scr                   ; screen page subtraction
        sbc     ent_x_scr,x
        pla
        bcs     unknown_1B_save_direction               ; player is to the right
        eor     #$FF                    ; negate: absolute distance
        adc     #$01                    ; if OAM = $D8 (Search Snake item)
        clc                             ; carry clear = player left
unknown_1B_save_direction:  php                         ; save direction (carry)
        cmp     #$03                    ; clamp speed to max 3 px/frame
        bcc     unknown_1B_restore_dir_set_xvel
        lda     #$03                    ; if anim frame != 0 → return
unknown_1B_restore_dir_set_xvel:  plp                         ; restore direction
        sta     ent_xvel,x                 ; set X speed = clamped distance
        lda     #$00                    ; sub-pixel = 0
        sta     ent_xvel_sub,x          ; → normal display, return
        bcc     unknown_1B_move_left_chase               ; player left → move left
        ldy     #$08                    ; move right with collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     unknown_1B_copy_player_facing

unknown_1B_move_left_chase:  ldy     #$09                ; move left with collision
        jsr     move_left_collide                   ; move_left_collide
unknown_1B_copy_player_facing:  lda     ent_flags               ; copy player facing (bit 6)
        and     #$40                    ; to slot 1 entity
        sta     L0000
        lda     $0581                   ; clear old bit 6
        and     #$BF                    ; set to player's facing
        ora     L0000                   ; clear direction bits 0-1
        sta     $0581
        rts

; ===========================================================================
; main_magnet_missile — Magnet Missile weapon AI
;\
; | Fires horizontally at 4.0 px/frame (set by init_weapon).
; | While traveling, scans enemy slots $10-$1F each frame.
; | If an active hittable enemy is within 8 pixels X-distance,
; | the missile turns 90 degrees toward that enemy (up or down),
; | transferring its horizontal speed to vertical speed.
; | After turning, accelerates vertically by $00.20/frame
; | up to 6.0 px/frame terminal velocity. Despawns off-screen.
; |
; | ent_facing direction flags:
; |   bit 0 ($01) = moving right  (horizontal phase)
; |   bit 1 ($02) = moving left   (horizontal phase)
; |   bit 2 ($04) = moving down   (vertical phase)
; |   bit 3 ($08) = moving up     (vertical phase)
; |   $00 = entered vertical phase (bits 0-1 cleared)
; |
; | OAM IDs: $97=horizontal, $9A=vertical up, $9B=vertical down
; /
main_magnet_missile:                    ; deactivate entity

        lda     ent_facing,x
        and     #$03                    ; check horizontal direction bits
        beq     magnet_missile_vertical_phase               ; if neither L/R set, vertical phase
        lda     #$97
        cmp     ent_anim_id,x                 ; set horizontal OAM sprite ($97)
        beq     magnet_missile_check_facing               ; (skip write if already set)
        sta     ent_anim_id,x
magnet_missile_check_facing:  lda     ent_facing,x
        and     #$01                    ; bit 0 = moving right
        beq     magnet_missile_move_left
        jsr     move_sprite_right                   ; move missile right
        jmp     magnet_missile_scan_init               ; skip left branch

magnet_missile_move_left:  jsr     move_sprite_left               ; move missile left
magnet_missile_scan_init:  ldy     #$1F                ; Y = slot 31 (start of enemy scan)
magnet_missile_scan_loop:  lda     ent_status,y             ; scan enemy slots $1F down to $10
        bpl     magnet_missile_scan_next               ; skip if slot inactive (bit 7 clear)
        lda     ent_hitbox,y                 ; check if entity is hittable
        and     #$40                    ; ($40 = can be hit by weapons)
        beq     magnet_missile_scan_next               ; skip if not hittable
        lda     ent_x_px,x
        sec                             ; 16-bit X distance:
        sbc     ent_x_px,y                 ; missile.X - enemy.X
        pha                             ; save low byte of difference
        lda     ent_x_scr,x                 ; subtract screen bytes
        sbc     ent_x_scr,y                 ; (carry propagates from pixel sub)
        pla                             ; restore low byte; carry = sign
        bcs     magnet_missile_check_x_dist               ; positive (missile >= enemy)? skip
        eor     #$FF                    ; negative: negate to get |distance
        adc     #$01                    ; (carry was clear from BCS fall-thru)
magnet_missile_check_x_dist:  cmp     #$08                ; X distance| < 8 pixels?
        bcs     magnet_missile_scan_next               ; no: skip this enemy
        lda     ent_xvel_sub,x                 ; enemy found within range!
        sta     ent_yvel_sub,x                 ; copy X speed (sub) → Y speed (sub)
        lda     ent_xvel,x                 ; copy X speed (whole) → Y speed
        sta     ent_yvel,x                 ; (vertical speed = 4.0 px/frame)
        lda     #$08                    ; default: direction = up ($08)
        sta     ent_facing,x                 ; (clears horizontal bits)
        lda     ent_y_px,x                 ; compare Y positions:
        sec                             ; missile.Y - enemy.Y
        sbc     ent_y_px,y              ; clear old bit 6
        bcs     magnet_missile_scan_done               ; missile below enemy? up is correct, done
        lda     #$04                    ; missile above enemy:
        sta     ent_facing,x                 ; change direction to down ($04)
        rts

magnet_missile_scan_next:  dey                         ; next enemy slot
        cpy     #$0F                    ; loop until Y < $10 (slot 15)
        bne     magnet_missile_scan_loop               ; (scans slots $1F down to $10)
magnet_missile_scan_done:  rts                         ; no enemy in range, keep flying

magnet_missile_vertical_phase:  lda     ent_yvel,x             ; --- vertical phase ---
        cmp     #$06                    ; check if at terminal velocity
        beq     magnet_missile_check_vert_dir               ; (6.0 px/frame); skip accel if so
        lda     ent_yvel_sub,x
        clc                             ; accelerate: Y speed += $00.20
        adc     #$20                    ; ($0.125 px/frame per frame)
        sta     ent_yvel_sub,x
        lda     ent_yvel,x                 ; add carry to whole byte
        adc     #$00
        sta     ent_yvel,x
magnet_missile_check_vert_dir:  lda     ent_facing,x             ; check vertical direction
        and     #$08                    ; bit 3 ($08) = moving up
        beq     magnet_missile_move_down               ; not set? moving down
        lda     #$9A                    ; OAM $9A = vertical up sprite
        sta     ent_anim_id,x
        jsr     move_sprite_up                   ; move missile upward
        jmp     magnet_missile_check_despawn               ; skip down branch

magnet_missile_move_down:  jsr     move_sprite_down               ; move missile downward
        lda     #$9B                    ; OAM $9B = vertical down sprite
        sta     ent_anim_id,x
magnet_missile_check_despawn:  lda     ent_y_scr,x             ; check Y screen position
        beq     magnet_missile_despawn_rts               ; if Y screen == 0, still on-screen
        lda     #$00                    ; otherwise despawn missile
        sta     ent_status,x                 ; (clear active flag)
magnet_missile_despawn_rts:  rts        ; bit 0 = moving right

; ===========================================================================
; main_gemini_laser — weapon $01: Gemini Laser bouncing projectile
; ===========================================================================
; 3 shots spawned (slots 1-3), each bounces off walls/floors/ceilings.
; ent_timer,x = bounce timer (init $B4, counts down). While >0: collision-checked
; movement with wall/floor bouncing. At 0: free movement, no collision.
; ent_facing,x direction flags: bit0=right, bit1=left, bit2=down, bit3=up.
; Speed set to 3.0 px/frame on both axes after first wall bounce.
main_gemini_laser:                      ; ($40 = can be hit by weapons)

        lda     ent_timer,x                 ; bounce timer
        beq     gemini_laser_direction               ; 0 = free movement phase
        dec     ent_timer,x                 ; decrement bounce timer
        lda     ent_timer,x                 ; compare against stagger threshold
        cmp     gemini_laser_bounce_rts,x             ; ($B4/$B2/$B0 for slots 1/2/3)
        bcc     gemini_laser_direction               ; below threshold → start moving
        rts                             ; still waiting (staggered spawn delay)

gemini_laser_direction:  lda     ent_facing,x             ; check horizontal direction
        and     #$01                    ; bit 0 = moving right?
        beq     gemini_laser_left_check               ; no → moving left
        lda     ent_timer,x                 ; bounce timer active?
        beq     gemini_laser_free_right               ; 0 → free movement (no collision)
        ldy     #$1E                    ; collision point: right edge
        jsr     move_right_collide                   ; move right with wall detection
        jmp     gemini_laser_wall_bounce               ; → check if wall was hit

gemini_laser_free_right:  lda     ent_flags,x             ; set facing right (bit 6)
        ora     #$40                    ; (free phase: set manually since
        sta     ent_flags,x                 ; move_sprite doesn't set facing)
        jsr     move_sprite_right                   ; move right, no collision
        jmp     gemini_laser_vert_dispatch               ; → vertical movement

gemini_laser_left_check:  lda     ent_timer,x             ; bounce timer active?
        beq     gemini_laser_free_left               ; 0 → free movement
        ldy     #$1F                    ; collision point: left edge
        jsr     move_left_collide                   ; move left with wall detection
        jmp     gemini_laser_wall_bounce               ; → check if wall was hit

gemini_laser_free_left:  lda     ent_flags,x             ; clear facing (bit 6 = 0 → left)
        and     #$BF
        sta     ent_flags,x             ; --- vertical phase ---
        jsr     move_sprite_left                   ; move left, no collision
        jmp     gemini_laser_vert_dispatch               ; → vertical movement

; --- wall bounce handler (after horizontal collision-checked move) ---

gemini_laser_wall_bounce:  bcc     gemini_laser_vert_dispatch           ; C=0: no wall hit → vertical movement
        lda     #$00                    ; zero sub-pixel speeds
        sta     ent_yvel_sub,x                 ; Y speed sub = 0
        sta     ent_xvel_sub,x                 ; X speed sub = 0
        lda     #$03                    ; set both speeds to 3.0 px/frame
        sta     ent_yvel,x                 ; Y speed whole = 3
        sta     ent_xvel,x                 ; X speed whole = 3
        lda     ent_facing,x                 ; flip horizontal direction
        eor     #$03                    ; (swap bits 0↔1: right↔left)
        sta     ent_facing,x            ; move missile upward
        and     #$0C                    ; already has vertical component?
        bne     gemini_laser_bounce_rts               ; yes → done (keep existing V dir)
        lda     ent_facing,x                 ; first bounce: add upward direction
        ora     #$08                    ; (bit 3 = up)
        sta     ent_facing,x
        rts                             ; check Y screen position

; --- vertical movement (after horizontal move, no wall hit) ---

gemini_laser_vert_dispatch:  lda     ent_facing,x             ; check vertical direction bits
        and     #$0C                    ; (bits 2-3)
        beq     gemini_laser_bounce_rts               ; no vertical component → done
        .byte   $29                     ; bit 2 = moving down?
gemini_laser_down_entry:  .byte   $04
        beq     gemini_laser_up_check               ; no → moving up
        lda     ent_timer,x                 ; bounce timer active?
        beq     gemini_laser_free_down               ; 0 → free movement
        ldy     #$12                    ; collision point: bottom edge
        jsr     move_down_collide                   ; move down with floor detection
        lda     #$A0                    ; OAM = $A0 (angled down)
        sta     ent_anim_id,x
        jmp     gemini_laser_floor_bounce               ; → check if floor was hit

gemini_laser_free_down:  lda     #$A0                ; OAM = $A0 (angled down)
        sta     ent_anim_id,x           ; compare against stagger threshold
        jmp     move_sprite_down                   ; move down, no collision

gemini_laser_up_check:  lda     ent_timer,x             ; bounce timer active?
        bne     gemini_laser_up_move_collision               ; nonzero → collision-checked move
        lda     #$A1                    ; OAM = $A1 (angled up)
        sta     ent_anim_id,x           ; bit 0 = moving right?
        jmp     move_sprite_up                   ; move up, no collision

gemini_laser_up_move_collision:  ldy     #$13                ; collision point: top edge
        jsr     move_up_collide                   ; move up with ceiling detection
        lda     #$A1                    ; OAM = $A1 (angled up)
        sta     ent_anim_id,x           ; → check if wall was hit

; --- floor/ceiling bounce handler ---
gemini_laser_floor_bounce:  bcc     gemini_laser_bounce_rts           ; C=0: no hit → done
        lda     ent_facing,x                 ; flip vertical direction
        eor     #$0C                    ; (swap bits 2↔3: down↔up)
        sta     ent_facing,x            ; → vertical movement
gemini_laser_bounce_rts:  .byte   $60
        ldy     $B2,x                   ; stagger thresholds per slot (1/2/3)
        bcs     gemini_laser_down_entry ; 0 → free movement
        cpy     #$05                    ; collision point: left edge
        cmp     #$71                    ; $71 = fist opening?
        beq     gemini_laser_open_check               ; → check if opening done
        cmp     #$AC                    ; $AC = launch anim frame 1?
        beq     gemini_laser_launch_wait               ; → wait for anim timer
        cmp     #$AE                    ; $AE = launch anim frame 2?
        bne     gemini_laser_fly_accel               ; neither → already flying, skip to movement
gemini_laser_launch_wait:  lda     ent_anim_frame,x             ; launch anim: wait for timer to expire
        bne     gemini_laser_exit               ; nonzero = still animating
        lda     #$71                    ; timer done → switch to fist opening
        sta     ent_anim_id,x
        rts

gemini_laser_open_check:  lda     ent_anim_state,x             ; fist opening ($71): wait for frame 4
        cmp     #$04                    ; (fist fully open)
        bne     gemini_laser_exit               ; not yet → return
        lda     #$AF                    ; done → switch to flying fist anim
        jsr     reset_sprite_anim                   ; reset_sprite_anim
gemini_laser_fly_accel:  lda     ent_xvel,x             ; flying phase: accelerate X speed
        cmp     #$03                    ; already at max $03.00?
        beq     gemini_laser_fly_maxed               ; yes → skip acceleration
        lda     ent_xvel_sub,x                 ; X speed sub += $20
        clc                             ; ($00.20 = 0.125 px/frame per frame)
        adc     #$20                    ; yes → done (keep existing V dir)
        sta     ent_xvel_sub,x          ; first bounce: add upward direction
        lda     ent_xvel,x                 ; carry into whole byte
        adc     #$00
        sta     ent_xvel,x
gemini_laser_fly_maxed:  lda     ent_facing,x             ; move horizontally based on facing
        and     #$01                    ; bit 0: 1=right, 0=left
        beq     gemini_laser_fly_left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     gemini_laser_wobble     ; (bits 2-3)

gemini_laser_fly_left:  jsr     move_sprite_left               ; move_sprite_left
gemini_laser_wobble:  lda     $95                 ; Y wobble via frame parity
        and     #$01                    ; $95 = global frame counter
        beq     gemini_laser_wobble_even               ; even frame → Y-1
        inc     ent_y_px,x                 ; odd frame: Y += 1 (nudge down)
        jmp     gemini_laser_steer      ; collision point: bottom edge

gemini_laser_wobble_even:  dec     ent_y_px,x             ; even frame: Y -= 1 (nudge up)
gemini_laser_steer:  lda     joy1_held                 ; D-pad steering: check Up/Down held
        and     #$0C                    ; ($08=Up, $04=Down)
        beq     gemini_laser_exit               ; neither → return
        and     #$08                    ; Up held?
        beq     gemini_laser_steer_down               ; no → Down
        jsr     move_sprite_up                   ; steer upward
        jmp     gemini_laser_screen_check

gemini_laser_steer_down:  jsr     move_sprite_down               ; steer downward
gemini_laser_screen_check:  lda     ent_y_scr,x             ; offscreen check after vertical steer
        beq     gemini_laser_exit               ; Y screen 0 = on-screen → return
        lda     #$00                    ; offscreen: despawn
        sta     ent_status,x
gemini_laser_exit:  rts                 ; collision point: top edge

; ===========================================================================
; main_search_snake — Search Snake weapon AI ($06)
; ===========================================================================
; State 0: falling with $99 to find a surface (horizontal timer ent_timer).
; State 1: crawling along surfaces (floor→wall→ceiling), wraps around corners.
; ent_facing direction: bit0=right, bit1=left, bit2=down, bit3=up.
; Crawl speed: 3.0 px/frame. OAM: $A5=horiz, $A6=descend, $A7=ascend.
main_search_snake:

        lda     ent_status,x                 ; entity state bits 0-3
        and     #$0F
        bne     search_snake_check_vert_dir               ; nonzero → state 1: crawling

; --- state 0: falling (searching for floor) ---
        ldy     #$12                    ; hitbox index for $99
        jsr     move_vertical_gravity                   ; apply $99; C=1 if landed
        bcs     search_snake_set_crawl_speed               ; landed → begin crawling
        lda     ent_timer,x                 ; horizontal move timer
        beq     search_snake_corner_return               ; expired → stop moving, RTS
        dec     ent_timer,x                 ; decrement timer
        jmp     spark_shock_check_facing                   ; move horizontally (shared code)

; --- landed: set crawl speed and transition to state 1 ---

search_snake_set_crawl_speed:  lda     #$00                ; zero sub-pixel speeds
        sta     ent_xvel_sub,x          ; (fist fully open)
        sta     ent_yvel_sub,x          ; not yet → return
        lda     #$03                    ; crawl speed = 3.0 px/frame both axes
        sta     ent_xvel,x
        sta     ent_yvel,x              ; flying phase: accelerate X speed
        lda     ent_facing,x                 ; add "down" to direction (pressing floor)
        ora     #$04                    ; yes → skip acceleration
        sta     ent_facing,x            ; X speed sub += $20
        inc     ent_status,x                 ; state 0 → 1

; --- state 1: crawl on surface ---
search_snake_check_vert_dir:  lda     ent_facing,x             ; bit 3 = moving up?
        and     #$08
        bne     search_snake_climb_upward               ; yes → climb upward
        ldy     #$12                    ; move downward with collision
        jsr     move_down_collide                   ; C=1 if hit solid below
        lda     #$A6                    ; OAM $A6 = descending wall
        sta     ent_anim_id,x
        jmp     search_snake_check_offscreen

search_snake_climb_upward:  ldy     #$13                ; move upward with collision
        jsr     move_up_collide                   ; C=1 if hit solid above
        lda     #$A7                    ; OAM $A7 = ascending wall
        sta     ent_anim_id,x           ; even frame → Y-1
search_snake_check_offscreen:  lda     ent_y_scr,x             ; Y screen nonzero = offscreen
        bne     spark_shock_clear_status                   ; → despawn
        bcs     search_snake_hit_vert_solid               ; C=1: hit solid vertically

; --- no vertical contact: at surface edge, try corner wrap ---
        lda     ent_facing,x                 ; isolate vertical direction
        and     #$0C                    ; neither → return
        tay                             ; Y = $04(down) or $08(up)
        lda     ent_facing,x                 ; save original direction
        pha                             ; steer upward
        cpy     #$08                    ; moving up? don't flip horizontal
        beq     search_snake_check_no_flip
        eor     #$03                    ; moving down: flip horizontal
        sta     ent_facing,x                 ; (reverse to probe around floor edge)
search_snake_check_no_flip:  lda     ent_anim_id,x             ; save OAM
        pha                             ; offscreen: despawn
        jsr     search_snake_move_horizontal               ; probe horizontal move
        pla                             ; restore OAM
        sta     ent_anim_id,x
        pla                             ; restore original direction
        sta     ent_facing,x
        bcs     search_snake_surface_return               ; C=1: hit wall → corner wrap done
        lda     ent_facing,x                 ; no wall: flip vertical (wrap edge)
        eor     #$0C                    ; down↔up
        sta     ent_facing,x
search_snake_corner_return:  rts

; --- hit solid vertically ---

search_snake_hit_vert_solid:  lda     ent_facing,x             ; moving up and hit ceiling?
        and     #$08                    ; nonzero → state 1: crawling
        beq     search_snake_move_horizontal               ; no → hit floor, move horizontal
        lda     #$00                    ; up + ceiling = dead end: despawn
        sta     ent_status,x            ; hitbox index for gravity
        rts                             ; apply gravity; C=1 if landed

; --- move horizontally along surface ---

search_snake_move_horizontal:  lda     #$A5                ; OAM $A5 = horizontal
        sta     ent_anim_id,x           ; move horizontally (shared code)
        lda     ent_facing,x                 ; bit 0 = moving right?
        and     #$01
        beq     search_snake_move_left_wall               ; no → move left
        ldy     #$1E                    ; move right with wall detection
        jsr     move_right_collide                   ; move_right_collide
        jmp     search_snake_check_wall

search_snake_move_left_wall:  ldy     #$1F                ; move left with wall detection
        jsr     move_left_collide                   ; move_left_collide
search_snake_check_wall:  bcc     search_snake_surface_return           ; no wall → done
        lda     ent_facing,x                 ; hit wall: flip vertical direction
        eor     #$0C                    ; (transition to wall climbing)
        sta     ent_facing,x            ; state 0 → 1
search_snake_surface_return:  rts

; ===========================================================================
; main_spark_shock — Spark Shock weapon projectile
; Travels horizontally in the direction fired. If it hits an enemy
; (ent_status nonzero), freezes in place for ent_timer frames, then despawns.
; ===========================================================================
main_spark_shock:                       ; OAM $A6 = descending wall

        lda     ent_status,x                 ; sprite state nonzero?
        and     #$0F                    ; shocking an enemy
        bne     spark_shock_dec_timer   ; move upward with collision
spark_shock_check_facing:  lda     ent_facing,x                 ; facing left?
        and     #$01                    ; move left
        beq     spark_shock_move_left
        jmp     move_sprite_right                   ; else move right

spark_shock_move_left:  jmp     move_sprite_left                   ; move_sprite_left

spark_shock_dec_timer:  dec     ent_timer,x                 ; decrease shock timer
        bne     spark_shock_return                   ; return if not expired
spark_shock_clear_status:  lda     #$00                    ; on timer expiration,
        sta     ent_status,x                 ; despawn (set inactive)
spark_shock_return:  rts                ; save original direction

; ===========================================================================
; main_shadow_blade — Shadow Blade weapon projectile
; Moves in up to 8 directions using ent_facing bits 0-3 (R/L/D/U).
; Horizontal and vertical movement handled independently. Returns to
; Mega Man after ent_timer expires (boomerang behavior).
; ===========================================================================
main_shadow_blade:                      ; probe horizontal move

        lda     ent_facing,x                 ; facing neither right nor left?
        and     #$03                    ; skip horizontal movement
        beq     shadow_blade_check_vertical
        and     #$01                    ; facing left? move left
        beq     shadow_blade_move_left  ; no wall: flip vertical (wrap edge)
        jsr     move_sprite_right                   ; else move right
        jmp     shadow_blade_check_vertical

shadow_blade_move_left:  jsr     move_sprite_left                   ; move_sprite_left
shadow_blade_check_vertical:  lda     ent_facing,x                 ; facing neither up nor down?
        and     #$0C                    ; skip vertical movement
        beq     shadow_blade_despawn_check ; moving up and hit ceiling?
        and     #$08                    ; facing down? move down
        beq     shadow_blade_move_down  ; no → hit floor, move horizontal
        jsr     move_sprite_up                   ; else move up
        jmp     shadow_blade_despawn_check

shadow_blade_move_down:  jsr     move_sprite_down                   ; move_sprite_down
shadow_blade_despawn_check:  lda     ent_y_scr,x                 ; offscreen vertically?
        bne     shadow_blade_offscreen_despawn                   ; despawn
        dec     ent_timer,x                 ; movement timer not expired?
        bne     shadow_blade_rts                   ; return
        lda     ent_status,x                 ; on timer expired, check if
        and     #$0F                    ; state == $00, or blade hasn't
        beq     shadow_blade_flip_direction                   ; flipped yet, if it has return
shadow_blade_offscreen_despawn:  lda     #$00                    ; set to inactive
        sta     ent_status,x
        rts

shadow_blade_flip_direction:  inc     ent_status,x                 ; indicate flipped state
        lda     #$14                    ; reset movement timer
        sta     ent_timer,x             ; no wall → done
        lda     ent_facing,x                 ; if not facing up or down,
        and     #$0C                    ; only flip horizontal
        beq     shadow_blade_flip_horizontal
        lda     ent_facing,x                 ; flip vertical
        eor     #$0C                    ; facing direction
        sta     ent_facing,x
        and     #$03
        beq     shadow_blade_rts
shadow_blade_flip_horizontal:  lda     ent_facing,x                 ; flip horizontal
        eor     #$03                    ; facing direction
        sta     ent_facing,x
shadow_blade_rts:  rts

; ===========================================================================
; main_dada — Dada (bouncing robot, Hard Man stage)
; ===========================================================================
; Walks horizontally with $99, bouncing in 3 progressively higher arcs.
; Re-faces player every 3 bounces. ent_timer=bounce index (0-2), ent_var1=face timer.
main_dada:                              ; else move right

        lda     ent_status,x                 ; state 0: init
        and     #$0F
        bne     dada_movement               ; already init'd → skip
        inc     ent_status,x                 ; state → 1
        lda     #$03                    ; face-player countdown = 3 bounces
        sta     ent_var1,x              ; despawn (set inactive)
dada_movement:  lda     ent_facing,x             ; walk horizontally with wall collision
        and     #$01
        beq     dada_move_left               ; bit 0 clear → move left
        ldy     #$0A
        jsr     move_right_collide                   ; move_right_collide
        jmp     dada_apply_gravity

dada_move_left:  ldy     #$0B
        jsr     move_left_collide                   ; move_left_collide
dada_apply_gravity:  ldy     #$0A                ; apply $99; C=1 if landed
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     dada_rts               ; still airborne → return
        lda     ent_timer,x                 ; on landing: load bounce Y speed
        tay                             ; from table indexed by bounce#
        lda     dada_yvel_sub,y                 ; Y speed sub (3 entries)
        sta     ent_yvel_sub,x          ; else move right
        lda     dada_yvel,y                 ; Y speed whole (3 entries)
        sta     ent_yvel,x
        dec     ent_var1,x                 ; decrement face-player counter
        bne     dada_advance_bounce               ; not zero → skip re-facing
        jsr     face_player                   ; re-face toward player
        lda     #$03                    ; reset counter to 3
        sta     ent_var1,x              ; facing down? move down
dada_advance_bounce:  inc     ent_timer,x             ; advance bounce index
        lda     ent_timer,x             ; else move up
        cmp     #$03                    ; wrap at 3 (cycle 0→1→2→0)
        bcc     dada_rts
        lda     #$00
        sta     ent_timer,x             ; offscreen vertically?
dada_rts:  .byte   $60                  ; despawn

; bounce Y speeds: sub={$44,$44,$EA}, whole={$03,$03,$07}
; bounce 0: $03.44, bounce 1: $03.44, bounce 2: $07.EA (big hop)
dada_yvel_sub:  .byte   $44,$44,$EA     ; state == $00, or blade hasn't
dada_yvel:  .byte   $03                 ; flipped yet, if it has return
        .byte   $03                     ; set to inactive
        .byte   $07

; ===========================================================================
; main_potton — Potton (helicopter dropper, Snake Man stage)
; ===========================================================================
; Flies horizontally, reverses on wall hit. When player is within 4 screens
; X-distance, stops and drops a bomb (Copipi child). OAM $23=flying, $24=bomb bay open.
main_potton:                            ; only flip horizontal
        lda     ent_anim_id,x                 ; OAM $23 = flying with propeller
        cmp     #$23                    ; flip vertical
        beq     potton_collision               ; already dropping → skip movement
        lda     ent_facing,x                 ; horizontal movement with wall collision
        and     #$01
        beq     potton_move_left
        ldy     #$08                    ; flip horizontal
        jsr     move_right_collide                   ; move_right_collide
        jmp     potton_collision

potton_move_left:  ldy     #$09
        jsr     move_left_collide                   ; move_left_collide
potton_collision:  bcc     potton_state_check           ; hit wall → reverse direction
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
potton_state_check:  lda     ent_status,x             ; state check
        and     #$0F
        bne     potton_check_drop               ; state 1+ → check bomb drop anim
        jsr     entity_x_dist_to_player                   ; state 0: check range to player
        cmp     #$04                    ; < 4 screens away?
        bcs     potton_rts               ; no → return
        inc     ent_status,x                 ; state → 1 (stop and drop)
        lda     #$23                    ; set OAM $23 (propeller stop anim)
        bne     potton_reset_anim               ; → reset_sprite_anim
potton_check_drop:  lda     ent_anim_id,x             ; already showing bomb bay ($24)?
        cmp     #$24                    ; bit 0 clear → move left
        beq     potton_rts               ; yes → done
        lda     ent_anim_state,x                 ; anim frame == 6? (propeller stop done)
        cmp     #$06
        bne     potton_rts               ; not yet → wait
        jsr     potton_spawn_bomb               ; spawn bomb child (Copipi)
        lda     #$24                    ; OAM $24 = bomb bay open
potton_reset_anim:  jsr     reset_sprite_anim               ; reset_sprite_anim
potton_rts:  rts

; --- spawn_copipi: drop bomb child below Potton ---

potton_spawn_bomb:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     potton_spawn_rts               ; none → return
        lda     ent_facing,x                 ; copy parent facing to child
        sta     ent_facing,y
        lda     ent_x_px,x                 ; copy X position
        sta     ent_x_px,y              ; not zero → skip re-facing
        lda     ent_x_scr,x             ; re-face toward player
        sta     ent_x_scr,y             ; reset counter to 3
        lda     ent_y_px,x                 ; Y = parent Y + $11 (below)
        clc                             ; advance bounce index
        adc     #$11
        sta     ent_y_px,y              ; wrap at 3 (cycle 0→1→2→0)
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$25                    ; OAM $25 = Copipi (bomb)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$04                    ; AI routine = $04 (falling bomb)
        sta     ent_routine,y
        lda     #$C0                    ; dmg flags: $C0 = hurts player + hittable
        sta     ent_hitbox,y
potton_spawn_rts:  rts

        lda     ent_status,x
        and     #$0F
        bne     potton_vertical_move
        jsr     reset_gravity                   ; reset_gravity
        inc     ent_status,x
potton_vertical_move:  ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     hammer_joe_init_rts
        lda     #$71                    ; OAM $23 = flying with propeller
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; already dropping → skip movement
        sta     ent_routine,x           ; horizontal movement with wall collision
hammer_joe_init_rts:  rts

; ===========================================================================
; main_hammer_joe — Hammer Joe (shielded enemy, throws hammers)
; ===========================================================================
; Cycle: shield up (invulnerable, timer ent_timer=$1E) → shield open (OAM $27,
; vulnerable) → throw hammer at anim frame $0A → shield close → repeat.
; Tracks player facing. ent_var1 = opened-once flag (toggles vulnerability).
main_hammer_joe:                        ; hit wall → reverse direction

        lda     ent_status,x                 ; state 0: init
        and     #$0F
        bne     hammer_joe_shield_check               ; already init'd → skip
        sta     ent_var1,x                 ; clear opened flag
        lda     #$1E                    ; shield timer = $1E (30 frames)
        sta     ent_timer,x             ; state 0: check range to player
        jsr     set_sprite_hflip                   ; face player
        inc     ent_status,x                 ; state → 1
hammer_joe_shield_check:  lda     ent_timer,x             ; shield timer active?
        bne     hammer_joe_dec_timer               ; yes → count down
        lda     ent_anim_frame,x                 ; anim still playing?
        ora     ent_anim_state,x                 ; (frame timer or seq index nonzero)
        bne     hammer_joe_face_player               ; yes → track player + continue
        lda     #$27                    ; OAM $27 = shield open (visor up)
        cmp     ent_anim_id,x                 ; already set?
        beq     hammer_joe_open_check               ; yes → process open state
        sta     ent_anim_id,x                 ; set it
        rts                             ; spawn bomb child (Copipi)

hammer_joe_dec_timer:  dec     ent_timer,x             ; decrement shield timer
hammer_joe_face_player:  lda     ent_facing,x             ; save old facing, re-face player
        pha
        jsr     face_player                   ; face_player
        pla                             ; if facing changed,
        cmp     ent_facing,x                 ; flip sprite horizontally
        beq     hammer_joe_open_check   ; none → return
        lda     ent_flags,x             ; copy parent facing to child
        eor     #$40
        sta     ent_flags,x             ; copy X position
hammer_joe_open_check:  lda     ent_anim_id,x             ; only act when shield is open ($27)
        cmp     #$27
        bne     hammer_joe_rts               ; closed → return
        lda     ent_var1,x                 ; first time opening this cycle?
        bne     hammer_joe_toggle_vuln               ; no → skip vulnerability toggle
        lda     ent_hitbox,x                 ; toggle vulnerability bits ($60)
        eor     #$60                    ; now hittable + hurts player
        sta     ent_hitbox,x            ; HP = 1
        inc     ent_var1,x                 ; mark as opened
hammer_joe_toggle_vuln:  lda     ent_anim_state,x             ; anim frame == $0A? (throw frame)
        cmp     #$0A
        bne     hammer_joe_anim_check               ; not yet → check for close
        lda     ent_anim_frame,x                 ; frame timer == 0? (exact moment)
        bne     hammer_joe_anim_check               ; no → wait
        jsr     hammer_joe_spawn_hammer               ; spawn hammer projectile
        rts

hammer_joe_anim_check:  lda     ent_anim_frame,x             ; anim fully complete?
        ora     ent_anim_state,x                 ; (both zero = anim done)
        bne     hammer_joe_rts               ; still playing → return
        dec     ent_anim_id,x                 ; close shield: OAM $27→$26
        dec     ent_status,x                 ; state back to 0 (re-init timer)
        lda     ent_hitbox,x                 ; toggle vulnerability off
        eor     #$60                    ; (shield closed = invulnerable)
        sta     ent_hitbox,x
hammer_joe_rts:  rts

; --- spawn_hammer: create thrown hammer projectile ---

hammer_joe_spawn_hammer:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     hammer_joe_spawn_rts               ; none → return
        sty     L0000                   ; save child slot
        lda     ent_facing,x                 ; copy facing to hammer
        sta     ent_facing,y
        and     #$02                    ; index: 0=facing right, 2=facing left
        tay
        lda     ent_x_px,x                 ; hammer X = Joe X + offset
        clc                             ; (+$13 if right, -$13 if left)
        adc     hammer_joe_hammer_x_off,y
        pha                             ; state 0: init
        lda     ent_x_scr,x                 ; with screen carry
        adc     hammer_joe_hammer_x_scr,y ; already init'd → skip
        ldy     L0000                   ; clear opened flag
        sta     ent_x_scr,y             ; shield timer = $1E (30 frames)
        pla
        sta     ent_x_px,y              ; face player
        lda     ent_y_px,x                 ; hammer Y = Joe Y - 6 (arm height)
        sec                             ; shield timer active?
        sbc     #$06                    ; yes → count down
        sta     ent_y_px,y              ; anim still playing?
        lda     #$33                    ; hammer X speed = $03.33 (3.2 px/f)
        sta     ent_xvel_sub,y          ; yes → track player + continue
        lda     #$03                    ; OAM $27 = shield open (visor up)
        sta     ent_xvel,y              ; already set?
        lda     #$28                    ; OAM $28 = hammer sprite
        jsr     init_child_entity                   ; init_child_entity
        lda     #$2D                    ; AI routine = $2D (arcing projectile)
        sta     ent_routine,y
        lda     #$C0                    ; dmg flags: hurts player + hittable
        sta     ent_hitbox,y            ; save old facing, re-face player
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
hammer_joe_spawn_rts:  .byte   $60      ; if facing changed,

; hammer X offset: right=$0013, left=$FFED (-19)
hammer_joe_hammer_x_off:  .byte   $13
hammer_joe_hammer_x_scr:  brk
        sbc     proto_man_x_check_byte
        brk                             ; only act when shield is open ($27)
        jsr     move_vertical_gravity                   ; carry=1 if landed
        rol     $0F                     ; save landed flag in $0F bit 0
        lda     ent_anim_id,x                 ; if OAM ID == $6A (crouch anim),
        cmp     #$6A                    ; skip horizontal movement
        beq     hammer_joe_wall_check               ; (crouching before jump)
        lda     ent_var1,x                 ; if ent_var1 == 0, skip walk timer
        beq     hammer_joe_check_walk_dir               ; (not in post-land walk phase)
        lda     ent_timer,x                 ; if walk timer > 0,
        beq     hammer_joe_check_walk_dir               ; decrement and wait
        dec     ent_timer,x                 ; (post-land walk delay)
        rts                             ; not yet → check for close

hammer_joe_check_walk_dir:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = facing right
        beq     hammer_joe_move_left
        ldy     #$00                    ; move right with wall collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     hammer_joe_wall_check   ; (both zero = anim done)

hammer_joe_move_left:  ldy     #$01                ; move left with wall collision
        jsr     move_left_collide                   ; move_left_collide
hammer_joe_wall_check:  bcc     hammer_joe_proximity_trigger           ; if no wall hit, skip
        lda     ent_facing,x                 ; hit wall: flip direction
        eor     #$03                    ; toggle bits 0+1 (left/right)
        sta     ent_facing,x
hammer_joe_proximity_trigger:  lda     ent_status,x             ; check state (bits 0-3)
        and     #$0F                    ; if already in jump state,
        bne     hammer_joe_crouch_anim_check               ; skip proximity trigger
        jsr     entity_x_dist_to_player                   ; get X distance to player
        cmp     #$40                    ; if distance >= $40 pixels,
        bcs     hammer_joe_crouch_anim_check               ; stay in walk state
        inc     ent_status,x                 ; state 0 -> 1 (enter jump)
        lda     #$6A                    ; switch to crouch anim ($6A)
        jsr     reset_sprite_anim                   ; (pre-jump windup)
hammer_joe_crouch_anim_check:  lda     ent_anim_id,x             ; if current OAM != $6A (crouch),
        cmp     #$6A                    ; not ready to jump yet
        bne     hammer_joe_jump_landing ; (+$13 if right, -$13 if left)
        lda     ent_anim_state,x                 ; wait until anim reaches frame 2
        cmp     #$02                    ; (crouch anim finished)
        bne     hammer_joe_jump_landing ; with screen carry
        lda     #$6B                    ; switch to jump anim ($6B)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_hitbox,x                 ; toggle damage flags bits 5-6
        eor     #$60                    ; (change vulnerability during jump)
        sta     ent_hitbox,x
        lda     ent_y_px,x                 ; nudge Y position up by $10
        sec                             ; (offset sprite above ground
        sbc     #$10                    ; before applying upward velocity)
        sta     ent_y_px,x
        lda     #$4D                    ; set Y speed = $07.4D upward
        sta     ent_yvel_sub,x                 ; (strong jump)
        lda     #$07
        sta     ent_yvel,x
        jsr     find_enemy_freeslot_y                   ; find free slot for child projectile
        bcs     hammer_joe_jump_landing               ; no free slot, skip spawn
        sty     L0000                   ; save child slot in $00
        lda     ent_facing,x                 ; use facing to index X offset table
        and     #$02                    ; y=0 if right, y=2 if left
        tay
        lda     ent_x_px,x                 ; child X = parent X + offset
        clc                             ; (16-bit add from table at $8DC4)
        adc     hammer_joe_xoffset,y
        pha
        lda     ent_x_scr,x
        adc     hammer_joe_xscreen_offset,y
        ldy     L0000                   ; restore child slot
        sta     ent_x_scr,y                 ; child X.screen
        pla
        sta     ent_x_px,y                 ; child X.pixel
        lda     ent_y_px,x                 ; child Y = parent Y
        sta     ent_y_px,y              ; if OAM ID == $6A (crouch anim),
        lda     #$01                    ; child HP = 1
        sta     ent_hp,y                ; (crouching before jump)
        lda     #$6C                    ; init child entity with OAM $6C
        jsr     init_child_entity                   ; (projectile sprite)
        lda     #$C2                    ; child damage flags = $C2
        sta     ent_hitbox,y                 ; (hurts player + hittable + invincible)
        lda     #$44                    ; child Y speed = $03.44
        sta     ent_yvel_sub,y                 ; (falling arc projectile)
        lda     #$03
        sta     ent_yvel,y              ; check facing direction
        lda     #$09                    ; child AI routine = $09
        sta     ent_routine,y                 ; ($99 projectile handler)
        jmp     hammer_joe_done               ; done

hammer_joe_jump_landing:  lda     ent_anim_id,x             ; if OAM != $6B (jump anim),
        cmp     #$6B                    ; skip landing logic
        bne     hammer_joe_done         ; move left with wall collision
        lda     $0F                     ; check landed flag (saved from
        and     #$01                    ; move_vertical_gravity earlier)
        beq     hammer_joe_done               ; not landed yet, keep falling
        lda     #$6D                    ; switch to walk-toward anim ($6D)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        jsr     face_player                   ; face player after landing
        lda     #$00                    ; set X speed = $02.00
        sta     ent_xvel_sub,x                 ; (walk toward player)
        lda     #$02                    ; get X distance to player
        sta     ent_xvel,x              ; if distance >= $40 pixels,
        lda     #$10                    ; set walk timer = $10 frames
        sta     ent_timer,x                 ; (walk toward player briefly)
        inc     ent_var1,x                 ; set post-land walk flag (ent_var1=1)
hammer_joe_done:  rts                   ; (pre-jump windup)

; bubukan child projectile X offset table (read as data at $8DC4)
; also serves as auto_walk_spawn_done trampoline for child entity

hammer_joe_xoffset:  .byte   $20        ; (crouch anim finished)
hammer_joe_xscreen_offset:  brk
        cpx     #$FF                    ; switch to jump anim ($6B)

; child projectile AI: just apply Y speed ($99 projectile)
        jmp     apply_y_speed                   ; apply_y_speed

; ===========================================================================
; main_jamacy — Jamacy (chain/spike ball, Spark Man stage)
; Oscillates vertically at speed $00.C0/frame. Initial half-period
; is read from table at $8E12 based on AI routine index ($15->$60,
; $16->$70 frames). When the period counter expires, reverses Y
; direction and doubles the period (each swing longer than the last).
; ent_timer = current countdown, ent_var1 = base period (doubled each reversal)
; ===========================================================================
main_jamacy:                            ; find free slot for child projectile

        lda     ent_status,x                 ; check state (bits 0-3)
        and     #$0F                    ; if already initialized,
        bne     jamacy_check_direction               ; skip init
jamacy_set_yvel:  sta     ent_yvel,x                 ; set Y speed = $00.C0
        lda     #$C0                    ; (slow vertical drift; A=0 from AND)
        sta     ent_yvel_sub,x          ; (16-bit add from table at $8DC4)
        lda     ent_routine,x                 ; AI routine index - $15 = table offset
        sec                             ; (routine $15 -> y=0, $16 -> y=1)
        sbc     #$15
        tay
        lda     jamacy_done,y                 ; load initial half-period from table
        sta     ent_timer,x                 ; set as current countdown
        sta     ent_var1,x                 ; save as base period for doubling
        inc     ent_status,x                 ; state 0 -> 1 (oscillating)
jamacy_check_direction:  lda     ent_facing,x             ; check direction bit 0
        and     #$01                    ; bit 0 set = moving up
        beq     jamacy_move_down        ; child HP = 1
        jsr     move_sprite_up                   ; move up at current Y speed
        jmp     jamacy_apply_movement   ; init child entity with OAM $6C

jamacy_move_down:  jsr     move_sprite_down               ; move down at current Y speed
jamacy_apply_movement:  dec     ent_timer,x             ; decrement period counter
        bne     jamacy_exit               ; if not expired, done
        lda     ent_facing,x                 ; reverse Y direction
        eor     #$03                    ; toggle bits 0+1
        sta     ent_facing,x
        lda     ent_var1,x                 ; double the base period and reload
        asl     a                       ; (note: ent_var1 is NOT updated, so
        sta     ent_timer,x                 ; it always doubles from initial value)
jamacy_exit:  rts

; jamacy initial half-period table: routine $15=$60, $16=$70 frames

jamacy_done:  rts                       ; check landed flag (saved from

        bvs     jamacy_set_yvel         ; not landed yet, keep falling
        brk                             ; switch to walk-toward anim ($6D)
        .byte   $03
        and     #$0F                    ; if already initialized,
        bne     bombflier_check_state               ; skip init
        inc     ent_status,x                 ; state 0 -> 1 (flying)
        lda     #$00                    ; clear speed countdown and
        sta     ent_timer,x                 ; table index (start at entry 0,
        sta     ent_var1,x                 ; load speeds immediately)
        jsr     set_sprite_hflip                   ; set sprite flip from facing
bombflier_check_state:  lda     ent_status,x             ; if state bit 1 set (state >= 2),
        and     #$02                    ; jump to walking bomb / homing
        beq     bombflier_load_speed
        jmp     bombflier_walking_state               ; -> state 2+ handler

bombflier_load_speed:  lda     ent_timer,x             ; if speed countdown > 0,
        bne     bombflier_apply_movement               ; skip to movement (use current speeds)
        ldy     ent_var1,x                 ; load table index -> indirection table
        lda     bombflier_speed_index,y                 ; $8F3C[idx] = speed pair index
        asl     a                       ; * 2 for 16-bit entries
        tay
        lda     bombflier_yspeed_lo,y                 ; load Y speed (16-bit signed)
        sta     ent_yvel_sub,x                 ; from table at $8F4A
        lda     bombflier_yspeed_table,y
        sta     ent_yvel,x
        lda     bombflier_xspeed_lo,y                 ; load X speed (16-bit signed)
        sta     ent_xvel_sub,x                 ; from table at $8F6A
        lda     bombflier_xspeed_table,y
        sta     ent_xvel,x
        lda     ent_xvel,x                 ; if X speed is positive (moving right),
        bpl     bombflier_advance_table               ; skip negation
        lda     ent_xvel_sub,x                 ; negate X speed (16-bit two's complement)
        eor     #$FF                    ; for leftward-facing entities, table
        clc                             ; values are mirrored so both directions
        adc     #$01                    ; use the same sinusoidal curve
        sta     ent_xvel_sub,x          ; skip init
        lda     ent_xvel,x              ; set Y speed = $00.C0
        eor     #$FF                    ; (slow vertical drift; A=0 from AND)
        adc     #$00
        sta     ent_xvel,x              ; AI routine index - $15 = table offset
bombflier_advance_table:  inc     ent_var1,x             ; advance table index
        lda     ent_var1,x                 ; wrap at 14 entries (0-13)
        cmp     #$0E
        bne     bombflier_reset_countdown ; load initial half-period from table
        lda     #$00                    ; set as current countdown
        sta     ent_var1,x              ; save as base period for doubling
bombflier_reset_countdown:  lda     #$05                ; reset speed countdown = 5 frames
        sta     ent_timer,x                 ; (hold each speed for 5 frames)
bombflier_apply_movement:  dec     ent_timer,x             ; decrement speed countdown
        lda     ent_y_sub,x                 ; apply Y speed manually (16-bit)
        clc                             ; Y.sub += Yspd.sub
        adc     ent_yvel_sub,x                 ; Y.pixel += Yspd.whole + carry
        sta     ent_y_sub,x
        lda     ent_y_px,x              ; move down at current Y speed
        adc     ent_yvel,x              ; decrement period counter
        sta     ent_y_px,x              ; if not expired, done
        lda     ent_facing,x                 ; apply X movement based on facing
        and     #$02                    ; bit 1 = facing left
        bne     bombflier_move_left
        jsr     move_sprite_right                   ; move right at X speed
        bcs     bombflier_penpen_check               ; unconditional jump
        bcc     bombflier_penpen_check               ; (BCS+BCC = always)
bombflier_move_left:  jsr     move_sprite_left               ; move left at X speed
bombflier_penpen_check:  lda     ent_routine,x             ; if AI routine != $0A (not PenPen),
        cmp     #$0A                    ; skip to bomb flier proximity check
        bne     bombflier_distance_check
        jsr     check_sprite_weapon_collision                   ; check if player weapon hit PenPen
        bcs     bombflier_penpen_exit               ; no hit, return
        ldy     $10                     ; destroy the weapon that hit us
        lda     #$00                    ; ($10 = weapon slot from collision)
        sta     ent_status,y
        inc     ent_status,x                 ; state 1 -> 2 (walking bomb)
        lda     #$80                    ; set X speed = $02.80
        sta     ent_xvel_sub,x                 ; (walking bomb speed)
        lda     #$02                    ; clear speed countdown and
        sta     ent_xvel,x              ; table index (start at entry 0,
        lda     #$48                    ; switch to walking bomb anim ($48)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
bombflier_penpen_exit:  rts             ; if state bit 1 set (state >= 2),

bombflier_walking_state:  lda     ent_routine,x             ; if AI routine != $0A (not PenPen),
        cmp     #$0A                    ; skip to bomb flier homing movement
        bne     bombflier_horiz_move
        lda     ent_anim_id,x                 ; if already on walking anim ($49),
        cmp     #$49                    ; go straight to walking movement
        beq     bombflier_walk_direction ; load table index -> indirection table
        lda     ent_anim_frame,x                 ; wait for current anim to finish
        ora     ent_anim_state,x                 ; (timer=0 AND frame=0)
        bne     bombflier_anim_wait               ; still animating, return
        lda     #$49                    ; switch to walking bomb anim ($49)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_hitbox,x                 ; set damage flags: hurts player +
        ora     #$C3                    ; hittable + invincible ($C3)
        sta     ent_hitbox,x                 ; (walking bomb is dangerous)
bombflier_anim_wait:  rts               ; from table at $8F6A

bombflier_walk_direction:  lda     ent_facing,x             ; walk in facing direction
        and     #$01                    ; bit 0 = right
        beq     bombflier_walk_left     ; skip negation
        jmp     move_sprite_right                   ; walk right

bombflier_walk_left:  jmp     move_sprite_left               ; walk left

bombflier_distance_check:  jsr     entity_x_dist_to_player               ; get X distance to player
        cmp     #$30                    ; if distance >= $30 pixels,
        bcs     bombflier_anim_wait               ; too far, return (keep flying)
        lda     #$00                    ; set homing speed = $02.00
        sta     $02                     ; ($02/$03 = speed params for
        lda     #$02                    ; calc_homing_velocity)
        sta     $03                     ; wrap at 14 entries (0-13)
        jsr     calc_homing_velocity                   ; calculate homing direction + speed
        lda     $0C                     ; set direction flags from homing result
        sta     ent_facing,x
        inc     ent_status,x                 ; advance state (-> homing flight)
        rts                             ; reset speed countdown = 5 frames

bombflier_horiz_move:  lda     ent_facing,x             ; move horizontally based on direction
        and     #$01                    ; bit 0 = right
        beq     bombflier_homing_left   ; Y.sub += Yspd.sub
        jsr     move_sprite_right                   ; move right
        jmp     bombflier_vert_move

bombflier_homing_left:  jsr     move_sprite_left               ; move left
bombflier_vert_move:  lda     ent_facing,x             ; move vertically based on direction
        and     #$08                    ; bit 3 = up
        beq     bombflier_move_down     ; bit 1 = facing left
        jmp     move_sprite_up                   ; move up

bombflier_move_down:  .byte   $4C,$59,$F7         ; move down

; sinusoidal speed indirection table (14 entries, indexes into Y/X speed tables)
bombflier_speed_index:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$01 ; if AI routine != $0A (not PenPen),
        .byte   $02,$03,$04,$05,$06,$07 ; skip to bomb flier proximity check

; Y speed table (16 signed 16-bit values, indexed by indirection * 2)
bombflier_yspeed_lo:  .byte   $CD       ; no hit, return
bombflier_yspeed_table:  .byte   $FE,$E5,$FE,$27,$FF,$8B,$FF,$00 ; destroy the weapon that hit us
        .byte   $00,$75,$00,$D9,$00,$1B,$01,$33 ; ($10 = weapon slot from collision)
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE ; state 1 -> 2 (walking bomb)

; X speed table (16 signed 16-bit values, indexed by indirection * 2)
bombflier_xspeed_lo:  .byte   $00
bombflier_xspeed_table:  .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00 ; switch to walking bomb anim ($48)
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE,$CD
        .byte   $FE,$E5,$FE,$27
        .byte   $FF
        .byte   $8B                     ; if AI routine != $0A (not PenPen),
        .byte   $FF                     ; skip to bomb flier homing movement

; -----------------------------------------------
; main_cloud_platform — Cloud platform (Snake Man stage)
; -----------------------------------------------
; Rideable platform that rises upward while zigzagging horizontally.
; State 0: idle — wait for player to stand on it.
; State 1: active — fly upward, move left/right in a repeating pattern.
;   ent_timer = horizontal movement timer (frames per direction segment)
;   ent_var1 = direction table index (cycles 0-3, table at $9030)
;   ent_var2 = lifetime timer — when expired, spawns a clone and despawns
;   ent_var3 = saved X position (metatile-aligned) for child spawn
;   ent_flags bit 5 ($20) = tile collision check flag
; Direction table at $9030: $02,$01,$01,$02 = left, right, right, left
; -----------------------------------------------
main_cloud_platform:                    ; walk in facing direction
        lda     ent_status,x                 ; get entity state
        and     #$0F                    ; isolate state bits
        bne     cloud_platform_movement               ; state 1+: already active, skip to movement
        jsr     LFAF6                   ; state 0: check if player standing on platform
        bcc     cloud_platform_activate               ; player on top -> activate
        rts                             ; not standing on it -> wait

cloud_platform_activate:  inc     ent_status,x             ; advance to state 1 (active flying)
        lda     #$CC                    ; Y speed sub = $CC
        sta     ent_yvel_sub,x                 ; rise speed $00.CC (~0.8 px/frame upward)
        lda     #$00                    ; ($02/$03 = speed params for
        sta     ent_yvel,x                 ; Y speed whole = $00
        lda     #$02                    ; initial direction = left ($02)
        sta     ent_facing,x            ; calculate homing direction + speed
        lda     #$10                    ; movement timer = 16 frames (first segment)
        sta     ent_timer,x
        lda     #$B4                    ; lifetime timer = 180 frames ($B4)
        sta     ent_var2,x
        lda     #$E8                    ; Y position = $E8 (start near bottom of screen)
        sta     ent_y_px,x              ; move horizontally based on direction
        lda     ent_x_px,x                 ; save X aligned to 16px metatile boundary
        and     #$F0                    ; mask off low nibble
        ora     #$08                    ; center within metatile (+8)
        sta     ent_var3,x                 ; store saved X for child spawn position
        lda     #$00
        sta     ent_x_sub,x                 ; clear X sub-pixel
        lda     ent_flags,x                 ; clear sprite flag bit 2
        and     #$FB                    ; bit 3 = up
        sta     ent_flags,x
cloud_platform_movement:  jsr     move_sprite_up               ; rise upward (apply Y speed)
        lda     ent_flags,x                 ; check tile collision flag (bit 5)
        and     #$20                    ; move down
        beq     cloud_platform_horizontal               ; not set -> skip tile check
        ldy     #$06                    ; Y offset for tile check (center of platform)
        jsr     LE8D6                   ; check tile at current horizontal position
        lda     $10                     ; tile result flags
        and     #$10                    ; bit 4 = solid tile
        bne     cloud_platform_lifetime               ; solid -> skip horizontal movement
        lda     ent_flags,x                 ; no longer in solid: clear tile check flag
        and     #$DF
        sta     ent_flags,x
cloud_platform_horizontal:  lda     ent_facing,x             ; check direction
        and     #$01                    ; bit 0 = right
        beq     cloud_platform_move_left               ; not right -> move left
        jsr     move_sprite_right                   ; move right
        jmp     cloud_platform_timer_check

cloud_platform_move_left:  jsr     move_sprite_left               ; move left
cloud_platform_timer_check:  dec     ent_timer,x             ; decrement movement timer
        bne     cloud_platform_lifetime               ; not expired -> skip direction change
        inc     ent_var1,x                 ; advance direction table index
        lda     ent_var1,x
        and     #$03                    ; wrap index to 0-3
        sta     ent_var1,x
        tay
        lda     cloud_platform_dir_table,y                 ; load direction from table (L,R,R,L)
        sta     ent_facing,x                 ; set new direction
        lda     #$0E                    ; reset timer = 14 frames for next segment
        sta     ent_timer,x
cloud_platform_lifetime:  lda     ent_var2,x             ; check lifetime timer
        beq     cloud_platform_screen_check               ; already expired -> check offscreen
        dec     ent_var2,x                 ; decrement lifetime
        bne     cloud_platform_rts               ; not zero yet -> done for this frame
        jsr     cloud_platform_respawn               ; timer just hit 0: spawn replacement platform
cloud_platform_screen_check:  lda     ent_y_scr,x             ; check Y screen (high byte)
        beq     cloud_platform_rts               ; still on screen 0 -> keep alive
        lda     #$00                    ; scrolled offscreen: deactivate entity
        sta     ent_status,x
cloud_platform_rts:  .byte   $60

; Cloud platform direction table: left, right, right, left (zigzag)
cloud_platform_dir_table:  .byte   $02
        ora     ($01,x)
        .byte   $02

; -----------------------------------------------
; spawn_cloud_platform_clone
; -----------------------------------------------
; Spawns a replacement cloud platform at the saved X position (ent_var3)
; and bottom of screen (Y=$E8). The child starts in state 1 (already
; active) with the same rise speed, direction pattern, and lifetime.
; OAM $70, AI routine $14 (main_cloud_platform).
; Damage flags = $0F (invulnerable -- rideable platform).
; -----------------------------------------------
cloud_platform_respawn:  jsr     find_enemy_freeslot_y               ; find free enemy slot -> Y
        bcs     cloud_platform_done               ; no free slot -> abort
        lda     #$70                    ; OAM ID = $70 (cloud platform sprite)
        jsr     init_child_entity                   ; initialize child entity in slot Y
        lda     #$14                    ; AI routine = $14 (main_cloud_platform)
        sta     ent_routine,y           ; initial direction = left ($02)
        lda     #$81                    ; active ($80) + state 1 (already flying)
        sta     ent_status,y            ; movement timer = 16 frames (first segment)
        lda     ent_flags,y                 ; set sprite flags: bit 5 (tile check) + bit 0
        ora     #$21                    ; lifetime timer = 180 frames ($B4)
        sta     ent_flags,y
        lda     #$0F                    ; damage flags = $0F (invulnerable platform)
        sta     ent_hitbox,y
        lda     ent_var3,x                 ; copy saved X from parent
        sta     ent_x_px,y                 ; set child X position
        sta     ent_var3,y                 ; propagate saved X for future clones
        lda     ent_x_scr,x                 ; copy X screen from parent
        sta     ent_x_scr,y
        lda     #$00                    ; reset direction index to 0
        sta     ent_var1,y              ; clear sprite flag bit 2
        sta     ent_y_scr,y                 ; Y screen = 0
        sta     ent_yvel,y                 ; Y speed whole = 0
        sta     ent_xvel,y                 ; X speed whole = 0
        lda     #$CC                    ; Y speed sub = $CC (rise speed $00.CC)
        sta     ent_yvel_sub,y
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y          ; Y offset for tile check (center of platform)
        lda     #$E8                    ; Y position = $E8 (bottom of screen)
        sta     ent_y_px,y              ; tile result flags
        lda     #$02                    ; initial direction = left ($02)
        sta     ent_facing,y            ; solid -> skip horizontal movement
        lda     #$10                    ; movement timer = 16 frames
        sta     ent_timer,y
        lda     #$B4                    ; lifetime timer = 180 frames ($B4)
        sta     ent_var2,y              ; check direction
        lda     #$E8                    ; Y position = $E8 (redundant store)
        sta     ent_y_px,y              ; not right -> move left
cloud_platform_done:  rts               ; move right

; -----------------------------------------------
; main_unknown_14 -- Entity spawner
; -----------------------------------------------
; Periodically spawns a child entity every $F0 (240) frames.
; The child uses OAM $4E, AI routine $18, and has X speed $01.80.
; The spawner faces the player after each spawn cycle.
; Child HP = 1, damage flags = $C0 (bit 7 = hurts player, bit 6 set).
; -----------------------------------------------
main_unknown_14:

        lda     ent_timer,x                 ; check spawn cooldown timer
        bne     unknown_14_dec_cooldown               ; timer active -> decrement and wait
        jsr     find_enemy_freeslot_y                   ; find free enemy slot -> Y
        bcs     unknown_14_init_cooldown               ; no free slot -> reset timer, skip spawn
        lda     ent_facing,x                 ; copy parent direction to child
        sta     ent_facing,y            ; decrement lifetime
        lda     ent_x_px,x                 ; copy parent X position to child
        sta     ent_x_px,y              ; timer just hit 0: spawn replacement platform
        lda     ent_x_scr,x                 ; copy parent X screen to child
        sta     ent_x_scr,y             ; still on screen 0 -> keep alive
        lda     ent_y_px,x                 ; copy parent Y position to child
        sta     ent_y_px,y
        lda     #$01                    ; child HP = 1
        sta     ent_hp,y
        lda     #$18                    ; child AI routine = $18
        sta     ent_routine,y
        lda     #$4E                    ; child OAM ID = $4E
        jsr     init_child_entity                   ; init child entity
        lda     #$C0                    ; damage flags = $C0 (hurts player)
        sta     ent_hitbox,y
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y                 ; child X speed = $01.80 (1.5 px/frame)
        lda     #$01
        sta     ent_xvel,y                 ; X speed whole = $01
unknown_14_init_cooldown:  lda     #$F0                ; spawn cooldown = 240 frames ($F0)
        sta     ent_timer,x
        jmp     face_player                   ; turn toward player

unknown_14_dec_cooldown:  dec     ent_timer,x             ; decrement spawn cooldown
        rts                             ; no free slot -> abort

; -----------------------------------------------
; main_unknown_0C -- Falling entity with physics (also used as AI $0D)
; -----------------------------------------------
; A falling projectile/entity that interacts with tile properties.
; Two OAM modes: $4E (falling/grounded) and $4F (rising after tile trigger).
;
; OAM $4E (falling/grounded state):
;   Falls with $99. On landing (tile $40 below), checks adjacent
;   horizontal tiles. If the tile in the facing direction is also $40,
;   transitions to rising mode (OAM $4F, Y speed $01.80 upward).
;   Otherwise, walks horizontally and bounces off walls.
;
; OAM != $4E (initial drop state):
;   Moves downward with collision. On landing, resets to OAM $4E,
;   faces player, and enters the grounded/falling state.
;
; Tile ID $40 = special trigger tile (e.g. lava/spikes/water surface).
; $41 = tile below, $42 = tile to left, $43 = tile to right.
; -----------------------------------------------
main_unknown_0C:                        ; X speed whole = 0

        lda     ent_status,x                 ; get entity state
        and     #$0F                    ; isolate state bits
        bne     unknown_0C_check_anim_grounded               ; state 1+: skip init
        jsr     reset_gravity                   ; state 0: zero Y speed
        inc     ent_status,x                 ; advance to state 1
unknown_0C_check_anim_grounded:  lda     ent_anim_id,x             ; check current OAM ID
        cmp     #$4E                    ; OAM $4E = grounded/falling mode
        bne     unknown_0C_falling_init               ; different OAM -> initial drop state
        ldy     #$08                    ; $99 strength index
        jsr     move_vertical_gravity                   ; apply $99 + move (C=1 if landed)
        ror     L0000                   ; save carry (landed flag) into $00 bit 7
        lda     tile_at_feet_max                     ; tile ID at feet (below entity)
        cmp     #TILE_LADDER_TOP                    ; special trigger tile?
        beq     unknown_0C_check_horizontal_trigger               ; yes -> check horizontal tiles
        lda     L0000                   ; no special tile: check if landed
        bpl     unknown_0C_done               ; not landed (bit 7 clear) -> done
unknown_0C_check_horizontal_trigger:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = facing right
        beq     unknown_0C_check_left_tile               ; facing left -> check left tile
        lda     tile_at_feet_lo                     ; facing right: check tile to right
        cmp     #TILE_LADDER_TOP                    ; is it the trigger tile?
        bne     unknown_0C_horizontal_walk               ; no -> walk horizontally
        beq     unknown_0C_transition_rising               ; yes -> transition to rising
unknown_0C_check_left_tile:  lda     $42                 ; facing left: check tile to left
        cmp     #$40                    ; is it the trigger tile?
        bne     unknown_0C_horizontal_walk               ; no -> walk horizontally
unknown_0C_transition_rising:  lda     #$4F                ; switch to rising OAM sprite ($4F)
        jsr     reset_sprite_anim                   ; reset animation
        lda     #$80                    ; Y speed sub = $80
        sta     ent_yvel_sub,x                 ; rise speed = $01.80 (1.5 px/frame upward)
        lda     #$01
        sta     ent_yvel,x                 ; Y speed whole = $01
        rts

unknown_0C_falling_init:  ldy     #$0C                ; collision check offset
        jsr     move_down_collide                   ; move down with collision (C=1 if landed)
        bcc     unknown_0C_state_return               ; not landed -> keep falling
        dec     ent_status,x                 ; landed: go back to state 0
        jsr     face_player                   ; turn toward player
        jsr     reset_gravity                   ; zero Y speed
        lda     #$4E                    ; switch to grounded OAM ($4E)
        jsr     reset_sprite_anim                   ; reset animation
unknown_0C_state_return:  rts           ; init child entity

unknown_0C_horizontal_walk:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     unknown_0C_move_left               ; facing left -> move left
        ldy     #$08                    ; collision offset for right
        jsr     move_right_collide                   ; move right with wall check
        jmp     unknown_0C_check_wall_collision ; spawn cooldown = 240 frames ($F0)

unknown_0C_move_left:  ldy     #$09                ; collision offset for left
        jsr     move_left_collide                   ; move left with wall check
unknown_0C_check_wall_collision:  lda     $10                 ; tile collision result flags
        and     #$10                    ; bit 4 = hit solid wall
        beq     unknown_0C_done               ; no wall -> done
        lda     ent_facing,x                 ; hit wall: reverse direction
        eor     #$03                    ; flip both direction bits (left<->right)
        sta     ent_facing,x
unknown_0C_done:  rts

; --- Unreferenced code block at $9165 ---
; Gravity fall, on landing: state 0 sets X speed $03.44 and faces player,
; state 1 walks horizontally in facing direction.

        ldy     #$00                    ; $99 index 0
        jsr     move_vertical_gravity                   ; fall with $99
        bcc     unknown_0C_horizontal_return               ; not landed -> done
        lda     ent_status,x                 ; landed: check state
        and     #$0F
        bne     unknown_0C_facing_check               ; state 1+ -> walk horizontally
        inc     ent_status,x                 ; state 0: advance to state 1
        lda     #$44                    ; X speed sub = $44
        sta     ent_yvel_sub,x                 ; walk speed = $03.44 (~3.27 px/frame)
        lda     #$03
        sta     ent_yvel,x                 ; X speed whole = $03
        jsr     face_player                   ; face toward player
        jsr     set_sprite_hflip                   ; update sprite flip to match direction
unknown_0C_horizontal_return:  rts      ; isolate state bits

unknown_0C_facing_check:  lda     ent_facing,x             ; check direction
        and     #$01                    ; bit 0 = right
        beq     unknown_0C_move_left_2               ; facing left -> move left
        jmp     move_sprite_right                   ; move right

unknown_0C_move_left_2:  jmp     move_sprite_left               ; move left

; =============================================================================
; main_giant_springer — Large bouncing spring enemy (entity $BD parent)
; =============================================================================
; State 0: walks left/right, reverses on wall collision. When player is within
;   $30 pixels, transitions to bouncing state. After $1E walk frames, stops
;   and switches to stopped OAM ($BC). While stopped, counts active children
;   (spawn group $80) and spawns a small springer ($BD) launched upward.
; State 1: bouncing in place. Adjusts damage flags based on player Y position.
;   Counts down ent_var1 timer ($3C frames). When timer expires and player is
;   far enough away (>= $30 px), returns to walk state facing the player.
; OAM IDs: $BB=walking, $BC=stopped/launching, $C2=bouncing
; =============================================================================
main_giant_springer:                    ; yes -> transition to rising

        ldy     #$1E                    ; $99 speed index
        jsr     move_vertical_gravity                   ; apply $99
        lda     ent_anim_id,x                 ; current OAM ID
        cmp     #$BC                    ; is it stopped (launching) sprite?
        bne     giant_springer_dispatch               ; if not, go to walk/bounce state logic
        lda     ent_anim_frame,x                 ; anim frame timer
        ora     ent_anim_state,x                 ; OR with anim sequence frame
        bne     giant_springer_wait_rts               ; if animation still playing, wait
        jsr     giant_springer_count_kids               ; count active children (spawn group $80)
        lda     ent_var2,x                 ; $FF = children exist, $00 = none
        bne     giant_springer_dispatch               ; if children active, skip spawn → walk state
        dec     ent_anim_id,x                 ; OAM $BC → $BB (walk sprite, visual transition)
        jsr     find_enemy_freeslot_y                   ; find free enemy slot → Y
        bcs     giant_springer_no_slot               ; no free slot: skip spawn, reset timer
        lda     ent_x_px,x                 ; copy parent X pixel
        sta     ent_x_px,y              ; zero Y speed
        lda     ent_x_scr,x                 ; copy parent X screen
        sta     ent_x_scr,y             ; reset animation
        lda     ent_y_px,x                 ; parent Y pixel
        sbc     #$17                    ; spawn 23 pixels above parent
        sta     ent_y_px,y              ; check facing direction
        lda     #$BD                    ; child entity type: small springer
        jsr     init_child_entity                   ; initialize child entity in slot Y
        lda     #$75                    ; AI routine index
        sta     ent_routine,y           ; move right with wall check
        lda     #$C0                    ; damage flags: hurts player + hittable
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y                ; move left with wall check
        lda     #$80                    ; spawn group marker $80 (for child counting)
        sta     ent_spawn_id,y          ; bit 4 = hit solid wall
        lda     #$00                    ; zero out child's movement values
        sta     ent_facing,y                 ; direction = 0
        sta     ent_xvel_sub,y                 ; X speed sub = 0
        sta     ent_xvel,y                 ; X speed whole = 0
        sta     ent_yvel_sub,y                 ; Y speed sub = 0
        sta     ent_var2,y                 ; third timer = 0
        lda     #$FE                    ; Y speed whole = -2 (launched upward)
        sta     ent_yvel,y
        lda     #$08                    ; timer = 8 frames
        sta     ent_timer,y                 ; general timer
        sta     ent_var1,y                 ; secondary timer
giant_springer_no_slot:  lda     #$00                ; reset parent walk timer
        sta     ent_timer,x             ; not landed -> done
giant_springer_wait_rts:  rts           ; landed: check state

; -- giant springer: walk/bounce state dispatch --

giant_springer_dispatch:  lda     ent_status,x             ; active flag + state
        and     #$0F                    ; isolate state bits
        bne     giant_springer_bouncing               ; state 1 → bouncing
        lda     ent_facing,x                 ; direction flags
        and     #$01                    ; bit 0 = moving right?
        beq     giant_springer_move_left               ; if not, move left
        ldy     #$20                    ; speed index for move right
        jsr     move_right_collide                   ; move right with wall check
        jmp     giant_springer_after_move ; check direction

giant_springer_move_left:  ldy     #$21                ; speed index for move left
        jsr     move_left_collide                   ; move left with wall check
giant_springer_after_move:  bcc     giant_springer_dist_check           ; C=0: no wall hit, skip reversal
        lda     ent_facing,x                 ; hit a wall: flip direction
        eor     #$03                    ; toggle both left/right bits
        sta     ent_facing,x
giant_springer_dist_check:  jsr     entity_x_dist_to_player               ; A = horizontal distance to player
        cmp     #$30                    ; within 48 pixels?
        bcs     giant_springer_walk_cont               ; no → continue walking
        lda     #$3C                    ; bounce duration = 60 frames
        sta     ent_var1,x
        inc     ent_status,x                 ; state 0 → state 1
        lda     #$C2                    ; OAM ID $C2 = bouncing sprite
        jmp     reset_sprite_anim                   ; reset_sprite_anim

giant_springer_walk_cont:  lda     #$CA                ; damage flags: hittable + hurts player
        sta     ent_hitbox,x
        inc     ent_timer,x                 ; increment walk frame counter
        lda     ent_timer,x
        cmp     #$1E                    ; walked for 30 frames?
        bne     giant_springer_rts               ; not yet → continue
        lda     #$BC                    ; switch to stopped OAM (ready to launch)
        jmp     reset_sprite_anim                   ; reset_sprite_anim

; -- state 1: bouncing in place --

giant_springer_bouncing:  jsr     entity_y_dist_to_player               ; check vertical distance to player
        bcs     giant_springer_dmg_below               ; C=1: player is below → use $CA
        lda     #$DB                    ; player above: damage flags $DB
        bne     giant_springer_set_dmg               ; (always taken)
giant_springer_dmg_below:  lda     #$CA                ; player below: damage flags $CA
giant_springer_set_dmg:  sta     ent_hitbox,x             ; set damage flags
        lda     ent_var1,x                 ; bounce timer
        beq     giant_springer_bounce_end               ; if already 0, check distance
        dec     ent_var1,x                 ; decrement bounce timer
        bne     giant_springer_rts               ; still bouncing → done
giant_springer_bounce_end:  jsr     entity_x_dist_to_player               ; A = distance to player
        cmp     #$30                    ; player still within 48 px?
        bcc     giant_springer_rts               ; yes → keep bouncing
        dec     ent_status,x                 ; state 1 → state 0
        lda     #$BB                    ; OAM ID $BB = walking sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        jsr     face_player                   ; turn toward player
        lda     #$00                    ; reset walk frame counter
        sta     ent_timer,x             ; damage flags: hurts player + hittable
giant_springer_rts:  rts

; -----------------------------------------------------------------------------
; count_springer_children — counts active small springers (spawn group $80)
; -----------------------------------------------------------------------------
; Scans enemy slots $10-$1F for entities with ent_spawn_id == $80.
; Sets ent_var2,x = $00 if no children found (allow spawning),
;              = $FF if any children exist (block spawning).
; -----------------------------------------------------------------------------

giant_springer_count_kids:  lda     #$00                ; child count = 0
        sta     L0000                   ; Y speed whole = -2 (launched upward)
        lda     #$80                    ; target spawn group marker
        sta     $01                     ; timer = 8 frames
        ldy     #$1F                    ; start from slot 31 (last enemy slot)
giant_springer_scan_loop:  lda     ent_status,y             ; is slot active? (bit 7)
        bmi     giant_springer_check_kid               ; yes → check if it's a springer child
giant_springer_scan_next:  dey                         ; next slot
        cpy     #$0F                    ; scanned down to slot $10?
        bne     giant_springer_scan_loop               ; no → keep scanning
        lda     L0000                   ; child count
        bne     giant_springer_kids_exist               ; if > 0, block spawning
        lda     #$00                    ; no children: allow spawning
        sta     ent_var2,x              ; isolate state bits
        rts                             ; state 1 → bouncing

giant_springer_kids_exist:  lda     #$FF                ; children exist: block spawning
        sta     ent_var2,x              ; if not, move left
        rts                             ; speed index for move right

giant_springer_check_kid:  lda     $01                 ; $80 = spawn group marker
        cmp     ent_spawn_id,y                 ; does this entity's group match?
        bne     giant_springer_scan_next               ; no → skip
        inc     L0000                   ; yes → increment child count
        jmp     giant_springer_scan_next               ; continue scanning

; =============================================================================
; main_chibee — Small bee enemy with 16-direction player tracking
; =============================================================================
; Flies toward the player using table-driven velocity for 16 compass directions.
; Every ent_var1 frames (ent_timer countdown), recalculates direction to player via
; track_direction_to_player, then loads Y speed (sub/whole), X speed (sub/whole),
; OAM ID, and facing flag from lookup tables indexed by direction.
; Each frame: applies 24-bit X and Y position updates. If Y screen overflows
; (entity goes off-screen vertically), deactivates.
; =============================================================================
main_chibee:

        lda     ent_timer,x                 ; movement timer (frames until recalc)
        bne     chibee_apply_movement               ; if > 0, skip direction recalculation
        jsr     LF954                   ; sets ent_facing = base dir, ent_var2 = adjustment
        lda     ent_facing,x                 ; base direction index
        clc                             ; not yet → continue
        adc     ent_var2,x                 ; + fine adjustment → combined direction
        tay                             ; Y = table index (0-31)
        lda     chibee_yvel_sub_table,y                 ; Y speed sub from direction table
        sta     ent_yvel_sub,x
        lda     chibee_yvel_whole_table,y                 ; Y speed whole from direction table
        sta     ent_yvel,x              ; check vertical distance to player
        lda     chibee_xvel_sub_table,y                 ; X speed sub from direction table
        sta     ent_xvel_sub,x          ; player above: damage flags $DB
        lda     chibee_xvel_whole_table,y                 ; X speed whole from direction table
        sta     ent_xvel,x              ; player below: damage flags $CA
        lda     chibee_sprite_oam_table,y                 ; OAM ID from direction table
        sta     ent_anim_id,x           ; bounce timer
        lda     ent_flags,x                 ; sprite flags
        and     #$BF                    ; clear facing bit (bit 6)
        ora     chibee_facing_flag_table,y                 ; OR in facing flag from table ($00 or $40)
        sta     ent_flags,x             ; A = distance to player
        lda     ent_var1,x                 ; reload movement duration
        sta     ent_timer,x                 ; reset countdown timer
chibee_apply_movement:  dec     ent_timer,x             ; decrement movement timer
        lda     #$00                    ; sign extend X speed whole
        sta     L0000
        lda     ent_xvel,x                 ; X speed whole
        bpl     chibee_apply_x_movement               ; positive -> skip sign extension
        dec     L0000                   ; negative -> $00 = $FF (sign extend)
chibee_apply_x_movement:  lda     ent_x_sub,x             ; X sub-pixel
        clc
        adc     ent_xvel_sub,x                 ; + X speed sub
        sta     ent_x_sub,x
        lda     ent_x_px,x                 ; X pixel
        adc     ent_xvel,x                 ; + X speed whole + carry
        sta     ent_x_px,x
        lda     ent_x_scr,x                 ; X screen
        adc     L0000                   ; + sign extension + carry
        sta     ent_x_scr,x
        lda     #$00                    ; sign extend Y speed whole
        sta     L0000
        lda     ent_yvel,x                 ; Y speed whole
        bpl     chibee_apply_y_movement               ; positive -> skip
        dec     L0000                   ; negative -> $00 = $FF
chibee_apply_y_movement:  lda     ent_y_sub,x             ; Y sub-pixel
        clc                             ; yes → check if it's a springer child
        adc     ent_yvel_sub,x                 ; + Y speed sub
        sta     ent_y_sub,x             ; scanned down to slot $10?
        lda     ent_y_px,x                 ; Y pixel
        adc     ent_yvel,x                 ; + Y speed whole + carry
        sta     ent_y_px,x              ; if > 0, block spawning
        lda     ent_y_scr,x                 ; Y screen
        adc     L0000                   ; + sign extension + carry
        beq     chibee_movement_done               ; still on screen 0 -> done
        lda     #$00                    ; went off-screen vertically
        sta     ent_status,x                 ; deactivate entity
chibee_movement_done:  .byte   $60

; -- chibee direction lookup tables (16 directions x 2 sets = 32 entries each) --
; Indexed by combined direction from track_direction_to_player.
; Directions: 0=up, 2=up-right, 4=right, 6=down-right, 8=down, etc. (clockwise)
chibee_yvel_sub_table:  .byte   $00,$27,$4B,$3D,$00,$C3,$B5,$D9 ; Y speed sub (set 1)
        .byte   $00,$D0,$B5,$C3,$00,$3D,$4B,$27 ; yes → increment child count
        .byte   $CD,$E5,$27,$8B,$00,$75,$D9,$1B ; Y speed sub (set 2)
        .byte   $33,$1B,$D9,$75,$00,$8B,$27,$E5
chibee_yvel_whole_table:  .byte   $FE,$FE,$FF,$FF,$00,$00,$00,$01 ; Y speed whole (set 1)
        .byte   $02,$01,$00,$00,$00,$FF,$FF,$FE
        .byte   $FE,$FE,$FF,$FF,$00,$00,$00,$01 ; Y speed whole (set 2)
        .byte   $01,$01,$00,$00,$00,$FF,$FF,$FE
chibee_xvel_sub_table:  .byte   $00,$C3,$B5,$D9,$00,$D9,$B5,$C3 ; X speed sub (set 1)
        .byte   $00,$3D,$4B,$27,$00,$27,$4B,$3D
        .byte   $00,$75,$D9,$1B,$33,$1B,$D9,$75 ; X speed sub (set 2)
        .byte   $00,$8B,$27,$E5,$CD,$E5,$27,$8B
chibee_xvel_whole_table:  .byte   $00,$00,$00,$01,$02,$01,$00,$00 ; X speed whole (set 1)
        .byte   $00,$FF,$FF,$FE,$FE,$FE,$FF,$FF
        .byte   $00,$00,$00,$01,$01,$01,$00,$00 ; X speed whole (set 2)
        .byte   $00,$FF,$FF,$FE,$FE,$FE,$FF,$FF
chibee_sprite_oam_table:  .byte   $BD,$BD,$BE,$BE,$BF,$BF,$C0,$C0 ; OAM ID per direction (set 1)
        .byte   $C1,$C1,$C0,$C0,$BF,$BF,$BE,$BE ; if > 0, skip direction recalculation
        .byte   $41,$41,$41,$41,$41,$41,$41,$41 ; OAM ID per direction (set 2)
        .byte   $41,$41,$41,$41,$41,$41,$41,$41 ; base direction index
chibee_facing_flag_table:  .byte   $00,$00,$40,$40,$40,$40,$40,$40 ; facing flag ($00=right, $40=left) (set 1)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; + fine adjustment → combined direction
        .byte   $00,$00,$40,$40,$40,$40,$40,$40 ; facing flag (set 2)
        .byte   $00,$00,$00,$00,$00     ; Y speed sub from direction table
        brk
        brk                             ; Y speed whole from direction table
        brk
        jsr     check_player_collision                   ; check_player_collision
        bcc     chibee_deactivate
        lda     #$00                    ; X speed whole from direction table
        sta     L0000
        lda     ent_xvel,x              ; OAM ID from direction table
        bpl     chibee_xvel_sign_extend
        dec     L0000                   ; sprite flags
chibee_xvel_sign_extend:  lda     ent_x_sub,x ; clear facing bit (bit 6)
        clc                             ; OR in facing flag from table ($00 or $40)
        adc     ent_xvel_sub,x
        sta     ent_x_sub,x             ; reload movement duration
        lda     ent_x_px,x              ; reset countdown timer
        adc     ent_xvel,x              ; decrement movement timer
        sta     ent_x_px,x              ; sign extend X speed whole
        lda     ent_x_scr,x
        adc     L0000                   ; X speed whole
        sta     ent_x_scr,x             ; positive -> skip sign extension
        lda     #$00                    ; negative -> $00 = $FF (sign extend)
        sta     L0000                   ; X sub-pixel
        lda     ent_yvel,x
        bpl     chibee_yvel_sign_extend ; + X speed sub
        dec     L0000
chibee_yvel_sign_extend:  lda     ent_y_sub,x ; X pixel
        clc                             ; + X speed whole + carry
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x             ; X screen
        lda     ent_y_px,x              ; + sign extension + carry
        adc     ent_yvel,x
        sta     ent_y_px,x              ; sign extend Y speed whole
        lda     ent_y_scr,x
        adc     L0000                   ; Y speed whole
        beq     chibee_return           ; positive -> skip
chibee_deactivate:  lda     #$00        ; negative -> $00 = $FF
        sta     ent_status,x            ; Y sub-pixel
chibee_return:  rts

; ===========================================================================
; main_electric_gabyoall — Electric Gabyoall (rolling electrified ball)
;\
; | Rolls along surfaces with collision-based direction reversal.
; | Alternates between normal and electrified states on a $3C-frame
; | (60 frame) timer. In electrified state, uses OAM-2 sprite and
; | enables contact damage. Checks player hit at two Y offsets per
; | frame for reliable collision. Uses ent_routine as index into per-variant
; | offset tables ($94AF-$94B5).
; /
; ===========================================================================
main_electric_gabyoall:

        lda     ent_status,x                 ; state check
        and     #$0F                    ; Y speed sub (set 1)
        bne     electric_gabyoall_main_loop               ; state 1+ → main logic

; --- state 0: init ---
        inc     ent_status,x                 ; advance to state 1
        lda     #$3C                    ; electric toggle timer = 60 frames
        sta     ent_timer,x

; --- main loop: horizontal movement + player hit checks ---
electric_gabyoall_main_loop:  lda     ent_hitbox,x             ; clear low 5 damage bits
        and     #$E0                    ; (keep flags, reset damage)
        sta     ent_hitbox,x
        ldy     ent_routine,x                 ; variant index for tables
        lda     ent_y_px,x                 ; save original Y position
        pha
        clc                             ; Y += upper hitbox offset
        adc     electric_gabyoall_offset_table,y             ; (table-based per variant)
        sta     ent_y_px,x
        jsr     check_player_hit        ; check contact with player
        lda     ent_facing,x                 ; direction bit 0 = right?
        and     #$01                    ; X speed sub (set 2)
        beq     electric_gabyoall_move_left
        ldy     #$08                    ; move right with collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     electric_gabyoall_clear_flip ; X speed whole (set 1)

electric_gabyoall_move_left:  ldy     #$09                ; move left with collision
        jsr     move_left_collide                   ; move_left_collide
electric_gabyoall_clear_flip:  lda     ent_flags,x             ; clear H-flip bit
        and     #$BF                    ; (ball has no facing)
        sta     ent_flags,x
        bcc     electric_gabyoall_offset_table               ; no wall hit → vertical check
        lda     ent_facing,x                 ; wall hit: reverse direction
        eor     #$03                    ; toggle bits 0-1
        sta     ent_facing,x
        bne     electric_gabyoall_toggle_electric               ; → skip to electric toggle
electric_gabyoall_offset_table:  .byte   $BC ; OAM ID per direction (set 2)
        .byte   $20
electric_gabyoall_variant_index:  .byte   $03
        .byte   $BD
electric_gabyoall_anim_id_table:  cpy     #$03 ; facing flag ($00=right, $40=left) (set 1)
electric_gabyoall_hitbox_table:  clc
        adc     electric_gabyoall_variant_index,y
        sta     ent_y_px,x
        jsr     check_player_hit        ; facing flag (set 2)
        ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        ldy     ent_facing,x
        lda     tile_at_feet_max,y
        bne     electric_gabyoall_toggle_electric
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
electric_gabyoall_toggle_electric:  lda     ent_timer,x
        bne     electric_gabyoall_timer_decrement
        lda     ent_anim_state,x
        ora     ent_anim_frame,x
        bne     electric_gabyoall_restore_y
        ldy     ent_routine,x
        lda     ent_anim_id,x
        cmp     electric_gabyoall_anim_id_table,y
        bne     electric_gabyoall_inc_anim_id
        sec
        sbc     #$02
        sta     ent_anim_id,x
        lda     #$3C
        sta     ent_timer,x
        bne     electric_gabyoall_reset_anim_frame
electric_gabyoall_timer_decrement:  dec     ent_timer,x
        bne     electric_gabyoall_restore_y
electric_gabyoall_inc_anim_id:  inc     ent_anim_id,x
electric_gabyoall_reset_anim_frame:  lda     #$00
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
electric_gabyoall_restore_y:  pla
        sta     ent_y_px,x
        ldy     ent_routine,x
        lda     ent_anim_id,x
        cmp     electric_gabyoall_anim_id_table,y
        bne     electric_gabyoall_apply_hitbox
        lda     ent_hitbox,x
        and     #$E0
        ora     electric_gabyoall_hitbox_table,y
        sta     ent_hitbox,x
        .byte   $20,$97,$80
electric_gabyoall_apply_hitbox:  .byte   $60,$D8,$C8,$50,$70
        cmp     $E1
        asl     joy1_press,x
; ---------------------------------------------------------------------------
; main_junk_block -- Junk Block (entity type $64)
; ---------------------------------------------------------------------------
; State 0: wait for player within $3C px, then activate.
; Active: spawns child block (type $94) launched upward (Y speed $FF.AB)
;   at same position. Child gets routine $62, flags $B2, HP=8.
;   ent_var1 cooldown=$5B before next spawn.
;   junk_block_scan_slots scans enemy slots for matching X position at target Y.
; ---------------------------------------------------------------------------
main_junk_block:
        lda     ent_status,x
        and     #$0F
        bne     junk_block_spawn_check                   ; already initialized
        jsr     entity_x_dist_to_player                       ; X distance to player
        cmp     #$3C                        ; within 60 px?
        bcs     junk_block_rts                   ; no -> rts
        inc     ent_status,x                ; activate
junk_block_spawn_check:  lda     ent_var1,x              ; spawn cooldown active?
        bne     junk_block_dec_cooldown                   ; yes -> decrement and return
        lda     #$70
        sta     ent_timer,x                 ; target Y position for slot scan
        jsr     junk_block_scan_slots                   ; find entity at same X, target Y
        bcs     junk_block_rts                   ; none found -> rts
        jsr     find_enemy_freeslot_y                       ; find free enemy slot -> Y
        bcs     junk_block_rts                   ; no free slot -> rts
        lda     #$94                        ; child entity type $94
        jsr     init_child_entity                       ; init child entity
        lda     ent_x_px,x                  ; copy parent X to child
        sta     ent_x_px,y              ; Y += upper hitbox offset
        lda     ent_x_scr,x             ; (table-based per variant)
        sta     ent_x_scr,y
        lda     ent_y_px,x                  ; copy parent Y to child
        sta     ent_y_px,y              ; direction bit 0 = right?
        lda     #$62
        sta     ent_routine,y               ; child AI routine $62
        lda     ent_hitbox,x            ; move right with collision
        sta     ent_hitbox,y                ; copy parent hitbox
        lda     #$B2
        sta     ent_flags,y                 ; child sprite flags
        lda     #$5B                    ; move left with collision
        sta     ent_var1,x                  ; parent spawn cooldown = $5B
        lda     #$AB                    ; clear H-flip bit
        sta     ent_yvel_sub,y              ; child Y speed = $FF.AB (upward)
        lda     #$FF
        sta     ent_yvel,y              ; no wall hit → vertical check
        lda     #$08                    ; wall hit: reverse direction
        sta     ent_hp,y                    ; child HP = 8
junk_block_dec_cooldown:  dec     ent_var1,x              ; decrement spawn cooldown
junk_block_rts:  rts                    ; → skip to electric toggle

        ldy     #$1E
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcs     junk_block_sprite_flags
        lda     ent_y_px,x
        cmp     #$70
        bcc     junk_block_sprite_flags
        lda     #$90
        sta     ent_timer,x
        jsr     junk_block_scan_slots
        bcc     junk_block_sprite_flags
        lda     #$70
        sta     ent_y_px,x
junk_block_sprite_flags:  lda     ent_flags,x
        ora     #$20
        sta     ent_flags,x
        rts

; --- scan enemy slots for entity at same X, matching target Y ---
; Returns C=1 if found, C=0 if none.
junk_block_scan_slots:  stx     L0000
        ldy     #$1F                        ; start from slot $1F
junk_block_scan_loop:  cpy     L0000
        beq     junk_block_next_slot
        lda     ent_status,y
        bpl     junk_block_next_slot
        lda     ent_flags,y
        and     #$04
        bne     junk_block_next_slot
        lda     ent_x_px,x
        cmp     ent_x_px,y
        bne     junk_block_next_slot
        lda     ent_y_px,y
        cmp     ent_timer,x
        beq     junk_block_scan_success
junk_block_next_slot:  dey
        cpy     #$0F
        bne     junk_block_scan_loop
        clc
junk_block_scan_success:  rts
; ---------------------------------------------------------------------------
; main_petit_snakey -- Petit Snakey (small snake, entity type $4E)
; ---------------------------------------------------------------------------
; State 0: init -- set sprite hflip, face player, timer=$24.
; Active: waits for timer, checks direction to player. If in firing arc
;   (within 7 direction units), switches anim and fires homing projectile
;   via petit_snakey_spawn_proj. ent_var1=$10 attack cooldown, idle timer=$78.
; petit_snakey_spawn_proj: spawns bullet with speed $03.66 using calc_homing_velocity.
; ---------------------------------------------------------------------------
main_petit_snakey:

        lda     ent_status,x
        and     #$0F
        bne     petit_snakey_init_check                   ; skip init if already active
        jsr     set_sprite_hflip                       ; set_sprite_hflip
        jsr     face_player                       ; face_player
        inc     ent_status,x
        lda     #$24
        sta     ent_timer,x                 ; idle timer = 36 frames
petit_snakey_init_check:  lda     ent_var1,x              ; attack cooldown active?
        bne     petit_snakey_dec_cooldown                   ; yes -> decrement cooldown
        lda     ent_timer,x                 ; idle timer active?
        bne     petit_snakey_dec_idle                   ; yes -> decrement idle
        lda     ent_facing,x               ; check facing direction
        and     #$02
        bne     petit_snakey_left_arc                   ; facing left
        jsr     calc_direction_to_player                       ; calc_direction_to_player (right)
        sec
        sbc     #$01                        ; adjust for right-facing arc
        cmp     #$07                        ; in firing arc?
        bcs     petit_snakey_reset_idle                   ; no -> reset idle timer
        jmp     petit_snakey_fire                   ; yes -> fire

petit_snakey_left_arc:  jsr     calc_direction_to_player                   ; calc_direction_to_player (left)
        sec
        sbc     #$09                        ; adjust for left-facing arc
        cmp     #$07                        ; in firing arc?
        bcs     petit_snakey_reset_idle                   ; no -> reset idle timer
petit_snakey_fire:  lda     ent_anim_id,x           ; choose attack anim based on current
        cmp     #$D1
        bne     petit_snakey_anim_b
        lda     #$D2                        ; attack anim variant A
        bne     petit_snakey_reset_anim
petit_snakey_anim_b:  lda     #$D5                    ; attack anim variant B
petit_snakey_reset_anim:  jsr     reset_sprite_anim                   ; reset_sprite_anim
        jsr     petit_snakey_spawn_proj                   ; fire homing projectile
        lda     #$10
        sta     ent_var1,x                  ; attack cooldown = 16 frames
petit_snakey_reset_idle:  lda     #$78                    ; idle timer = 120 frames
        .byte   $9D
petit_snakey_data:  brk
        ora     stage_select_page
petit_snakey_dec_idle:  dec     ent_timer,x             ; decrement idle timer
        rts

petit_snakey_dec_cooldown:  dec     ent_var1,x              ; decrement attack cooldown
        bne     petit_snakey_cooldown_rts
        lda     ent_anim_id,x              ; cooldown done -> revert anim
        cmp     #$D2
        bne     petit_snakey_revert_anim
        lda     #$D1
        bne     petit_snakey_revert_done
petit_snakey_revert_anim:  lda     #$D4
petit_snakey_revert_done:  jsr     reset_sprite_anim               ; reset_sprite_anim
petit_snakey_cooldown_rts:  rts

; --- petit snakey: spawn homing projectile (speed $03.66, type $73) ---
petit_snakey_spawn_proj:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     petit_snakey_spawn_fail
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     petit_snakey_proj_x_off,y
        pha
        lda     ent_x_scr,x
        adc     petit_snakey_proj_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     #$00
        sta     ent_hp,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_y_scr,x
        sta     ent_y_scr,y
        lda     #$66
        sta     ent_xvel_sub,y
        sta     $02
        lda     #$03
        sta     ent_xvel,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     calc_homing_velocity                   ; calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     ent_facing,y
        lda     #$73
        jsr     init_child_entity                   ; init_child_entity
        lda     #$8F
        sta     ent_routine,y
        lda     #$8B
        .byte   $99,$80,$04
petit_snakey_spawn_fail:  .byte   $60
petit_snakey_proj_x_off:  .byte   $04
petit_snakey_proj_x_scr:  brk
        .byte   $FC
        .byte   $FF
; ---------------------------------------------------------------------------
; main_yambow -- Yambow (dragonfly enemy, entity type $0D)
; ---------------------------------------------------------------------------
; 4-state flight: init -> flap upward -> swoop horizontal -> fly forward.
; State 0: activate if player within $51 px, face player.
; State 1: flap with gravity (accel $10/frame, Y cap +2). On timer=0,
;   check Y distance; if close, advance to swooping.
; State 2: swoop toward player. If within $29 px, advance.
; State 3: re-check Y distance. If close, advance.
; State 4: fly in facing direction indefinitely.
; ---------------------------------------------------------------------------
main_yambow:
        lda     ent_status,x
        and     #$0F
        bne     yambow_timer_flap                   ; skip init if active
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        cmp     #$51                        ; within 81 px?
        bcs     yambow_rts                   ; no -> rts
        jsr     face_player                       ; face_player
        jsr     set_sprite_hflip                       ; set_sprite_hflip
        lda     ent_flags,x
        and     #$FB                        ; clear bit 2 (enable collision)
        sta     ent_flags,x
        inc     ent_status,x                ; advance to state 1
        rts

        ; --- timer active: flap with gravity ---
yambow_timer_flap:  lda     ent_timer,x
        beq     yambow_state_dispatch                   ; timer expired -> state dispatch
        dec     ent_timer,x
        lda     ent_status,x
        and     #$01                        ; odd states skip flapping
        bne     yambow_rts
        lda     ent_yvel,x                  ; Y speed cap check
        bmi     yambow_gravity_accel                   ; rising -> keep accelerating
        cmp     #$02
        bcs     yambow_rts                   ; >= +2 -> stop
yambow_gravity_accel:  lda     ent_yvel_sub,x          ; gravity accel: +$10/frame
        clc
        adc     #$10
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        bpl     yambow_apply_yvel                   ; positive -> falling
        jmp     apply_y_velocity_fall                       ; apply_y_velocity (rising)

yambow_apply_yvel:  jmp     apply_y_velocity_rise                   ; apply_y_velocity (falling)

yambow_rts:  rts

        ; --- timer expired: state dispatch ---
yambow_state_dispatch:  lda     ent_status,x
        and     #$0F
        cmp     #$04
        beq     yambow_fly_forward                   ; state 4: fly forward
        cmp     #$03
        beq     yambow_recheck_y                   ; state 3: check Y distance
        cmp     #$02
        beq     yambow_swoop                   ; state 2: swoop horizontal
        ; state 1: check Y distance to player
        jsr     entity_y_dist_to_player                       ; entity_y_dist_to_player
        bcc     yambow_advance_state                   ; player above -> advance
        cmp     #$4D                        ; > 77 px below?
        bcc     yambow_advance_state                   ; close enough -> advance
        jmp     apply_y_speed                       ; apply_y_speed (keep descending)

yambow_advance_state:  lda     #$14                    ; timer = 20 frames
        sta     ent_timer,x
        inc     ent_status,x                ; advance state
        jsr     reset_gravity                       ; reset_gravity
        jsr     face_player                       ; face_player
        jmp     set_sprite_hflip                       ; set_sprite_hflip

        ; --- state 2: swoop toward player ---
yambow_swoop:  lda     ent_facing,x
        and     #$02
        beq     yambow_swoop_right_check                   ; facing right
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        bcc     yambow_swoop_left
        cmp     #$29                        ; within 41 px?
        bcs     yambow_next_state                   ; yes -> advance state
yambow_swoop_left:  jmp     move_sprite_left                   ; move_sprite_left

yambow_swoop_right_check:  jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        bcs     yambow_swoop_right
        cmp     #$29                        ; within 41 px?
        bcs     yambow_next_state                   ; yes -> advance state
yambow_swoop_right:  jmp     move_sprite_right                   ; move_sprite_right

yambow_next_state:  jmp     yambow_advance_state               ; advance to next state

        ; --- state 3: re-check Y distance ---
yambow_recheck_y:  jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        bcc     yambow_advance_state                   ; close -> advance
        cmp     #$09                        ; within 9 px?
        bcc     yambow_to_state4                   ; close -> advance
        jmp     apply_y_speed                       ; apply_y_speed

yambow_to_state4:  jmp     yambow_advance_state               ; advance to state 4

        ; --- state 4: fly forward in facing direction ---
yambow_fly_forward:  lda     ent_facing,x
        and     #$02
        beq     yambow_fly_right
        jmp     move_sprite_left                       ; move_sprite_left

yambow_fly_right:  jmp     move_sprite_right                   ; move_sprite_right

; ===========================================================================
; main_met — Met (hard hat enemy, classic hide/peek/shoot)
; ===========================================================================
; State 0: hiding under helmet (invulnerable, ent_hitbox=$A3). Timer ent_timer counts
; down before peeking. When timer done + anim at frame 0 with counter 1 +
; player within $41 X-distance → become vulnerable ($C3), open helmet anim.
; At anim frame 2 → fire 3 bullets, enter state 1 (walking).
; State 1: walk with $99, ent_var1 = walk frames. When walk done → close
; helmet, return to state 0 with random delay.
main_met:

        lda     ent_timer,x                 ; hide timer active?
        beq     met_state_check               ; zero → check state
        dec     ent_timer,x                 ; decrement; still ticking?
        bne     met_freeze_anim               ; → freeze anim + return
met_state_check:  lda     ent_status,x             ; state check
        and     #$0F
        bne     met_walking               ; state 1+ → walking

; --- state 0: hiding / peeking ---
        jsr     face_player                   ; track player direction
        jsr     set_sprite_hflip                   ; flip sprite to face player
        lda     ent_anim_state,x                 ; anim seq index == 0?
        bne     met_check_fire_frame               ; not yet → check fire frame
        lda     ent_anim_frame,x                 ; anim frame timer == 1?
        cmp     #$01                    ; (last frame before loop)
        bne     met_check_fire_frame               ; no → continue anim
        jsr     entity_x_dist_to_player                   ; player within $41 (~4 tiles)?
        cmp     #$41
        bcs     met_freeze_anim               ; too far → stay hiding
        lda     #$C3                    ; close enough: become vulnerable
        sta     ent_hitbox,x                 ; $C3 = hittable + contact damage
        rts

met_freeze_anim:  lda     #$00                ; freeze animation (stay in helmet)
        sta     ent_anim_frame,x
        rts

met_check_fire_frame:  lda     ent_anim_state,x             ; anim frame == 2? (helmet fully open)
        cmp     #$02
        bne     met_return               ; not yet → return
        jsr     met_fire_bullets               ; fire 3 bullets
        inc     ent_status,x                 ; state → 1 (walking)
        lda     #$13                    ; walk duration = $13 (19 frames)
        sta     ent_var1,x
        lda     #$3C                    ; post-walk hide timer = $3C (60 frames)
        sta     ent_timer,x
met_return:  rts

; --- state 1: walking after shooting ---

met_walking:  lda     #$1D                ; set walking OAM $1D (if not already)
        cmp     ent_anim_id,x
        beq     met_apply_gravity
        jsr     reset_sprite_anim                   ; reset_sprite_anim
met_apply_gravity:  ldy     #$00                ; apply $99; C=1 if on ground
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     met_return               ; airborne → return
        lda     ent_var1,x                 ; walk frames remaining?
        beq     met_close_helmet               ; zero → done walking
        dec     ent_var1,x                 ; decrement walk counter
        lda     ent_facing,x                 ; walk in facing direction
        and     #$01
        beq     met_walk_left
        ldy     #$00
        jmp     move_right_collide

met_walk_left:  ldy     #$01
        jmp     move_left_collide

; --- walk done: close helmet, return to hiding ---

met_close_helmet:  lda     #$1C                ; OAM $1C = helmet closing anim
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_status,x                 ; state → 0 (clear low nibble)
        and     #$F0
        sta     ent_status,x
        lda     #$A3                    ; become invulnerable again
        sta     ent_hitbox,x                 ; $A3 = no contact, no weapon hit
        lda     $E5                     ; pseudo-random hide delay
        adc     $E6                     ; LFSR seed mix
        sta     $E6
        and     #$03                    ; 4 possible delays from table
        tay
        lda     met_hide_delay,y                 ; load random delay
        sta     ent_timer,x                 ; set hide timer
        rts

; --- met_fire_3_bullets: spawn 3 projectiles ---

met_fire_bullets:  stx     L0000               ; save Met slot
        lda     #$02                    ; bullet counter = 3 (indexes 2,1,0)
        sta     $01
met_bullet_loop:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     met_bullet_done               ; none → done
        ldx     $01                     ; set bullet speeds from table
        lda     met_bullet_xvel_sub,x                 ; X speed sub (3 entries)
        sta     ent_xvel_sub,y
        lda     met_bullet_xvel,x                 ; X speed whole
        sta     ent_xvel,y
        lda     met_bullet_yvel_sub,x                 ; Y speed sub
        sta     ent_yvel_sub,y
        lda     met_bullet_yvel,x                 ; Y speed whole
        sta     ent_yvel,y
        lda     #$73                    ; OAM $73 = Met bullet
        jsr     init_child_entity                   ; init_child_entity
        lda     #$8B                    ; dmg = $8B (hurts player only)
        sta     ent_hitbox,y
        ldx     L0000                   ; restore Met slot to X
        lda     #$0F                    ; AI routine = $0F (simple projectile)
        sta     ent_routine,y
        lda     ent_x_px,x                 ; copy Met position to bullet
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; hide timer active?
        sta     ent_x_scr,y             ; zero → check state
        lda     ent_y_px,x                 ; bullet Y = Met Y + 4
        clc                             ; → freeze anim + return
        adc     #$04                    ; state check
        sta     ent_y_px,y
        lda     ent_facing,x                 ; copy Met facing to bullet
        sta     ent_facing,y
        dec     $01                     ; loop for all 3 bullets
        bpl     met_bullet_loop         ; track player direction
met_bullet_done:  .byte   $A6,$00,$60         ; restore X

; Met bullet speeds: X.sub={$FB,$33,$FB}, X.whole={$00,$01,$00},
; Y.sub={$50,$00,$B0}, Y.whole={$00,$00,$FF}
; bullet 0: slow right+down, bullet 1: fast right, bullet 2: slow right+up
met_bullet_xvel_sub:  .byte   $FB,$33,$FB ; no → continue anim
met_bullet_xvel:  .byte   $00,$01,$00   ; player within $41 (~4 tiles)?
met_bullet_yvel_sub:  .byte   $50,$00,$B0
met_bullet_yvel:  .byte   $FF,$00,$00   ; too far → stay hiding
met_hide_delay:  asl     petit_snakey_data,x ; close enough: become vulnerable
        .byte   $3C                     ; $C3 = hittable + contact damage

; ===========================================================================
; main_pole — Pole (climbing pole enemy, Spark Man stage)
; ===========================================================================
; Moves vertically at set speed, walks horizontally (via code_1C9776).
; Despawns when Y screen changes (goes offscreen).
main_pole:                              ; anim frame == 2? (helmet fully open)
        lda     #$00                    ; sign extend Y speed for 24-bit add
        sta     L0000                   ; not yet → return
        lda     ent_yvel,x                 ; if Y speed negative, sign = $FF
        bpl     pole_apply_yvel_sub     ; state → 1 (walking)
        dec     L0000                   ; walk duration = $13 (19 frames)
pole_apply_yvel_sub:  lda     ent_y_sub,x             ; apply Y speed: Y.sub += Yspd.sub
        clc                             ; post-walk hide timer = $3C (60 frames)
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x                 ; Y += Yspd.whole
        adc     ent_yvel,x
        sta     ent_y_px,x
        lda     ent_y_scr,x                 ; Y.screen += sign
        adc     L0000
        bne     pole_despawn               ; offscreen → despawn
        jmp     yambow_fly_forward               ; on-screen → walk horizontally

pole_despawn:  lda     #$00                ; despawn
        sta     ent_status,x            ; airborne → return
        rts                             ; walk frames remaining?

; ===========================================================================
; main_cannon — Cannon (stationary turret, fires at player)
; ===========================================================================
; Cycle: idle (invulnerable, timer ent_timer) → detect player within $51 distance
; → open ($C9 = vulnerable) → fire 2 shells (at anim frames $09 and $12)
; → close ($A9 = invulnerable) → random idle delay. Fires distance-scaled shots.
main_cannon:

        lda     ent_timer,x                 ; idle timer active?
        beq     cannon_check_anim               ; zero → check for player
        dec     ent_timer,x                 ; decrement; still idling?
        bne     cannon_freeze_anim               ; → freeze anim + return
cannon_check_anim:  lda     ent_anim_state,x             ; anim seq frame == 0?
        bne     cannon_face_player               ; no → animating, continue
        lda     ent_anim_frame,x                 ; frame timer == 1? (ready to peek)
        cmp     #$01
        bne     cannon_face_player               ; no → continue
        jsr     entity_x_dist_to_player                   ; player within $51 distance?
        cmp     #$51                    ; $A3 = no contact, no weapon hit
        bcc     cannon_open               ; yes → open up
cannon_freeze_anim:  lda     #$00                ; freeze anim (stay closed)
        sta     ent_anim_frame,x
        rts                             ; 4 possible delays from table

cannon_open:  lda     #$C9                ; become vulnerable ($C9)
        sta     ent_hitbox,x            ; set hide timer
cannon_face_player:  jsr     face_player               ; track player direction
        jsr     set_sprite_hflip                   ; flip sprite
        lda     ent_anim_state,x                 ; anim fully done? (both zero)
        ora     ent_anim_frame,x
        bne     cannon_check_fire               ; still animating → check fire

; --- anim complete: close cannon, set idle delay ---
        lda     $E4                     ; pseudo-random idle delay
        adc     $E7                     ; LFSR seed mix
        sta     $E7                     ; set bullet speeds from table
        and     #$01                    ; 2 possible delays
        tay
        lda     cannon_idle_delay,y     ; X speed whole
        sta     ent_timer,x                 ; set idle timer
        lda     #$A9                    ; close: become invulnerable ($A9)
        sta     ent_hitbox,x
cannon_rts:  rts                        ; Y speed whole

; --- during open anim: fire at specific frames ---

cannon_check_fire:  lda     ent_anim_frame,x             ; frame timer must be 0 (exact moment)
        bne     cannon_rts               ; not zero → return
        lda     ent_anim_state,x                 ; fire at anim frame $09 or $12
        cmp     #$09                    ; (two shots per open cycle)
        beq     cannon_spawn_shell
        cmp     #$12                    ; copy Met position to bullet
        beq     cannon_spawn_shell
        rts

; --- spawn_cannon_shell: fires distance-scaled projectile ---

cannon_spawn_shell:  jsr     find_enemy_freeslot_y               ; find free slot
        bcs     cannon_rts               ; none → return
        lda     #$00                    ; shell Y speed = $04.00 (4.0 px/f up)
        sta     ent_yvel_sub,y
        lda     #$04                    ; loop for all 3 bullets
        sta     ent_yvel,y
        lda     #$6F                    ; OAM $6F = cannon shell
        jsr     init_child_entity                   ; init_child_entity
        lda     #SFX_SHOT                    ; play shot sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     #$C0                    ; dmg: hurts player + hittable
        sta     ent_hitbox,y
        lda     #$13                    ; AI routine $13 = arcing shell
        sta     ent_routine,y
        lda     ent_x_px,x                 ; copy cannon position to shell
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; shell Y = cannon Y - $0C (barrel)
        sec
        sbc     #$0C
        sta     ent_y_px,y
        lda     ent_y_scr,x                 ; copy Y screen
        sta     ent_y_scr,y
        lda     ent_facing,x                 ; copy facing + save for X offset
        sta     ent_facing,y
        pha                             ; if Y speed negative, sign = $FF
        jsr     entity_x_dist_to_player                   ; get distance for speed scaling
        stx     L0000                   ; save cannon slot
        ldx     #$03                    ; find speed bracket from distance table
cannon_speed_scan:  cmp     cannon_dist_table,x             ; (farther = slower X speed)
        bcc     cannon_speed_found
        dex
        bne     cannon_speed_scan       ; Y += Yspd.whole
cannon_speed_found:  lda     cannon_xvel_sub,x             ; set shell X speed from bracket
        sta     ent_xvel_sub,y                 ; X speed sub
        lda     cannon_xvel,x                 ; X speed whole
        sta     ent_xvel,y
        pla                             ; offset shell X based on facing
        and     #$02                    ; (facing left: bit 1 set → index 2)
        tax
        lda     ent_x_px,y                 ; shell X += offset
        clc                             ; right: +$0C, left: -$0C
        adc     cannon_shell_x_off,x
        sta     ent_x_px,y
        lda     ent_x_scr,y                 ; with screen carry
        adc     cannon_shell_x_scr,x
        sta     ent_x_scr,y
        .byte   $A6,$00,$60             ; restore cannon slot

; distance thresholds: $4C, $3D, $2E, $1F (close→far)
; X speed sub: $00, $80, $00, $80 | X speed whole: $02, $01, $01, $00
; idle delays: $3C, $78 | X offsets: right=$0C/$00, left=$F4/$FF
cannon_dist_table:  .byte   $4C,$3D,$2E,$1F ; idle timer active?
cannon_xvel_sub:  .byte   $00,$80,$00,$80 ; zero → check for player
cannon_xvel:  .byte   $02,$01,$01,$00   ; decrement; still idling?
cannon_idle_delay:  .byte   $3C,$78     ; → freeze anim + return
cannon_shell_x_off:  .byte   $0C        ; anim seq frame == 0?
cannon_shell_x_scr:  brk                ; no → animating, continue
        .byte   $F4                     ; frame timer == 1? (ready to peek)
        .byte   $FF

; --- cannon shell AI: $99 + walk, explode on landing/wall hit ---
        ldy     #$08                    ; apply $99; C=1 if landed
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcs     cannon_shell_explode               ; landed → explode
        lda     ent_facing,x                 ; walk horizontally with collision
        and     #$02
        beq     cannon_shell_move_right
        ldy     #$07                    ; become vulnerable ($C9)
        jsr     move_left_collide                   ; move_left_collide
        jmp     cannon_shell_collision  ; track player direction

cannon_shell_move_right:  ldy     #$08  ; anim fully done? (both zero)
        jsr     move_right_collide                   ; move_right_collide
cannon_shell_collision:  bcs     cannon_shell_explode           ; hit wall → explode
        rts

cannon_shell_explode:  lda     #$00                ; become generic explosion
        sta     ent_routine,x                 ; (routine $00)
        lda     #$71                    ; OAM $71 = small explosion
        jmp     reset_sprite_anim                   ; reset_sprite_anim

; ===========================================================================
; main_metall_dx — Metall DX (walking Met variant)
; ===========================================================================
; State 0: hiding, opens when player within $61 distance. Anim frame 5 →
;   launch up (Y speed $02.00). Flies up until within $49 Y of player.
; State 1: fly past player, fire 3 bullets when crossing. State 2: descend.
; State 3: walk horizontally (shared code_1C9776).
main_metall_dx:

        lda     ent_status,x                 ; state machine dispatch
        and     #$0F                    ; fire at anim frame $09 or $12
        cmp     #$01                    ; state 1: fly past player
        beq     metall_dx_fly_past
        cmp     #$02                    ; state 2: descend
        beq     metall_dx_descend
        cmp     #$03                    ; state 3: walk horizontally
        bne     metall_dx_hiding
        jmp     yambow_fly_forward               ; → walk in facing direction

; --- state 0: hiding / opening / ascending ---

metall_dx_hiding:  jsr     face_player               ; track player
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        lda     ent_anim_id,x                 ; OAM $1F = ascending (propeller)?
        cmp     #$1F
        beq     metall_dx_fly_up               ; yes → fly up logic
        lda     ent_anim_state,x                 ; anim in progress?
        bne     metall_dx_check_open               ; yes → check frame
        jsr     entity_x_dist_to_player                   ; player within $61?
        cmp     #$61                    ; dmg: hurts player + hittable
        bcs     metall_dx_freeze_anim               ; too far → stay hidden
        lda     #$C3                    ; become vulnerable
        sta     ent_hitbox,x
        inc     ent_anim_state,x                 ; advance anim frame
        lda     ent_anim_state,x
metall_dx_check_open:  cmp     #$05                ; anim frame 5? (fully opened)
        bne     metall_dx_state0_rts               ; not yet → return
        lda     #$00                    ; set upward velocity $02.00
        sta     ent_yvel_sub,x
        lda     #$02
        sta     ent_yvel,x
        lda     #$1F                    ; OAM $1F = ascending
        jmp     reset_sprite_anim                   ; reset_sprite_anim

metall_dx_fly_up:  jsr     entity_y_dist_to_player               ; within $49 Y of player?
        cmp     #$49
        bcs     metall_dx_advance_state               ; yes → next state
        jmp     move_sprite_up                   ; keep flying up

metall_dx_freeze_anim:  lda     #$00                ; freeze anim (stay hidden)
        sta     ent_anim_frame,x
metall_dx_state0_rts:  rts

; --- state 1: fly past player, fire when crossing ---

metall_dx_fly_past:  jsr     entity_x_dist_to_player               ; get X distance (sets carry)
        lda     ent_facing,x                 ; check if passed player
        and     #$02                    ; facing left + player behind → fire
        beq     metall_dx_check_fire    ; (facing left: bit 1 set → index 2)
        bcs     metall_dx_fire_3               ; C=1: player left of us → fire
        jmp     yambow_fly_forward               ; keep flying

metall_dx_check_fire:  bcc     metall_dx_fire_3           ; C=0: player right → fire
        jmp     yambow_fly_forward               ; keep flying

; --- state 2: descend to player altitude ---

metall_dx_descend:  jsr     face_player               ; track player
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        lda     ent_timer,x                 ; post-fire delay timer
        beq     metall_dx_check_descent               ; zero → descend
        dec     ent_timer,x                 ; wait
        rts

metall_dx_check_descent:  jsr     entity_y_dist_to_player               ; within 4 Y of player?
        cmp     #$04
        bcc     metall_dx_advance_state               ; yes → next state
        jmp     move_sprite_down                   ; keep descending

metall_dx_advance_state:  inc     ent_status,x             ; advance to next state
        rts

metall_dx_fire_3:  stx     L0000        ; apply gravity; C=1 if landed
        lda     #$02
        sta     $01                     ; landed → explode
metall_dx_proj_loop:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     metall_dx_after_fire
        ldx     $01
        lda     metall_dx_proj_xvel_sub,x
        sta     ent_xvel_sub,y
        lda     metall_dx_proj_xvel,x
        sta     ent_xvel,y
        lda     metall_dx_proj_yvel_sub,x
        sta     ent_yvel_sub,y
        lda     metall_dx_proj_yvel,x   ; hit wall → explode
        sta     ent_yvel,y
        lda     metall_dx_proj_facing,x
        sta     ent_facing,y            ; become generic explosion
        lda     #$73                    ; (routine $00)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$8B
        sta     ent_hitbox,y
        ldx     L0000
        lda     #$0F
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        dec     $01                     ; state machine dispatch
        bpl     metall_dx_proj_loop
metall_dx_after_fire:  ldx     L0000    ; state 1: fly past player
        inc     ent_status,x
        lda     #$3C                    ; state 2: descend
        .byte   $9D,$00,$05,$60
metall_dx_proj_xvel_sub:  .byte   $DB,$00,$DB ; state 3: walk horizontally
metall_dx_proj_xvel:  .byte   $00,$00,$00
metall_dx_proj_yvel_sub:  .byte   $DB,$33,$DB ; → walk in facing direction
metall_dx_proj_yvel:  .byte   $00,$01
        brk
metall_dx_proj_facing:  .byte   $02
        ora     ($01,x)                 ; track player

; ---------------------------------------------------------------------------
; main_mag_fly — Mag Fly (flying horseshoe magnet, Magnet Man stage)
; Flying magnet enemy that magnetically pulls Mega Man upward.
; This is the ONLY entity that triggers player state $05 (entity_ride).
; Player mounts when within X distance < $10 and Y overlap, state $00/$01.
; $34 = slot index of entity being ridden (player tracks this entity).
; Confirmed via Mesen breakpoint — triggered by magnetic pull while
; standing or jumping near a Mag Fly in Magnet Man's stage.
; ---------------------------------------------------------------------------
main_mag_fly:
        lda     ent_facing,x                 ; direction flag
        and     #$01
        beq     mag_fly_move_left       ; anim frame 5? (fully opened)
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     mag_fly_check_distance  ; set upward velocity $02.00

mag_fly_move_left:  jsr     move_sprite_left                   ; move_sprite_left
mag_fly_check_distance:  jsr     entity_y_dist_to_player                   ; check player proximity
        bcc     mag_fly_final_dismount                   ; no overlap → check dismount
        jsr     entity_x_dist_to_player                   ; detailed collision check
        cmp     #$10                    ; too far away?
        bcs     mag_fly_final_dismount  ; within $49 Y of player?
        lda     player_state                     ; only mount if state < $02
        cmp     #PSTATE_SLIDE                    ; (on_ground or airborne)
        bcs     mag_fly_check_dismount  ; keep flying up
        lda     #PSTATE_ENTITY_RIDE                    ; state → $05 (entity_ride)
        sta     player_state            ; freeze anim (stay hidden)
        stx     entity_ride_slot                     ; $34 = ridden entity slot
        lda     #$07                    ; player OAM $07 (riding anim)
        sta     ent_anim_id
        lda     #$00
        sta     ent_anim_frame                   ; reset animation counter
        sta     ent_anim_state                   ; reset animation frame
        sta     walk_flag                     ; clear sub-state
        lda     ent_y_px                   ; save player Y as reference
        sta     ent_timer,x
        lda     ent_yvel                   ; player Y velocity (high byte)
        bpl     mag_fly_return               ; if falling down, done
        lda     #$55                    ; if moving up, zero out velocity
        sta     ent_yvel_sub                   ; ent_yvel_sub/ent_yvel = $55/$00
        lda     #$00                    ; ($99 baseline, not moving)
        sta     ent_yvel
        rts

mag_fly_check_dismount:  lda     player_state ; track player
        cmp     #PSTATE_ENTITY_RIDE                    ; if not in entity_ride, skip
        bne     mag_fly_return          ; post-fire delay timer
        cpx     entity_ride_slot                     ; if riding different entity, skip
        bne     mag_fly_return          ; wait
        lda     ent_timer,x                 ; check Y distance from mount point
        sec
        sbc     ent_y_px                ; within 4 Y of player?
        cmp     #$20                    ; if player drifted > 32px away,
        bcc     mag_fly_return               ; stay mounted
        lda     #$00                    ; dismount: clear player velocity
        sta     ent_yvel_sub
        sta     ent_yvel                ; advance to next state
        lda     ent_facing,x                 ; $35 = Mag Fly's direction
        sta     facing_sub                     ; (player inherits movement dir)
        rts

mag_fly_final_dismount:  lda     player_state                     ; if player is riding ($05)
        cmp     #PSTATE_ENTITY_RIDE
        bne     mag_fly_return               ; and it's THIS entity
        cpx     entity_ride_slot
        bne     mag_fly_return
        lda     #$AB                    ; set fall velocity
        sta     ent_yvel_sub
        lda     #$FF
        sta     ent_yvel
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; dismount complete
mag_fly_return:  rts

; ===========================================================================
; main_junk_golem — Junk Golem mini-boss (Hard Man stage)
; State 0: idle, waits for player within $76 pixels horizontally.
; State 1: falling with $99 (hitbox Y offset $24).
; State 2: grounded — tracks player facing, periodically throws junk blocks
;   (entity $94, routine $24) that fall then home toward player.
; ent_timer = child slot (thrown block), ent_var1 = throw cooldown timer ($78 frames),
; ent_var2 = throw-anim started flag. OAM $38 = idle, $39 = throwing.
; ===========================================================================
main_junk_golem:

        lda     ent_status,x                 ; state 0: activation check
        and     #$0F
        bne     junk_golem_clear_vflip               ; skip if already active
        jsr     entity_x_dist_to_player                   ; check horizontal distance to player
        cmp     #$76                    ; if >= $76 px away,
        bcs     mag_fly_return               ; stay idle (returns via RTS above)
        inc     ent_status,x                 ; activate: advance to state 1 (falling)
        jsr     set_sprite_hflip                   ; face toward player
junk_golem_clear_vflip:  lda     ent_flags,x             ; clear sprite flags bit 2 if set
        and     #$04                    ; (prevents unwanted vertical flip
        beq     junk_golem_check_grounded               ; from collision or child spawn)
        lda     ent_flags,x
        eor     #$04
        sta     ent_flags,x
junk_golem_check_grounded:  lda     ent_status,x             ; check if state >= 2 (grounded)
        and     #$02
        bne     junk_golem_face_player               ; if grounded, skip $99
        ldy     #$24                    ; state 1: apply $99, Y hitbox offset $24
        jsr     move_vertical_gravity                   ; move down with collision
        bcc     junk_golem_return               ; C=0: still airborne, done
        inc     ent_status,x                 ; C=1: landed, advance to state 2
junk_golem_face_player:  lda     ent_facing,x             ; save old direction
        pha
        jsr     face_player                   ; update facing toward player
        pla                             ; compare old vs new direction
        cmp     ent_facing,x
        beq     junk_golem_toggle_flip               ; no change, skip flip
        lda     ent_flags,x                 ; direction changed: toggle sprite
        eor     #$40                    ; horizontal flip (bit 6)
        sta     ent_flags,x
junk_golem_toggle_flip:  lda     ent_var1,x             ; throw cooldown timer
        bne     junk_golem_check_throw_anim               ; non-zero: skip spawning
        jsr     junk_golem_spawn_block               ; spawn junk block child entity
        sty     L0000                   ; save child slot index
        lda     L0000                   ; check player proximity
        sta     ent_timer,x                 ; ent_timer = child slot (to track Y)
        lda     #$78                    ; reset throw cooldown = $78 (120 frames)
        sta     ent_var1,x              ; too far away?
        lda     #$00                    ; clear throw-anim flag
        sta     ent_var2,x              ; only mount if state < $02
junk_golem_check_throw_anim:  lda     ent_var2,x             ; if throw anim already started,
        bne     junk_golem_throw_anim_done               ; skip to countdown
        lda     ent_timer,x                 ; Y = child slot index
        tay
        lda     ent_y_px,y                 ; child Y position
        sec                             ; player OAM $07 (riding anim)
        sbc     ent_y_px,x                 ; minus golem Y position
        bcs     junk_golem_check_block_dist               ; if negative,
        eor     #$FF                    ; take absolute value
        adc     #$01                    ; reset animation frame
        clc                             ; clear sub-state
junk_golem_check_block_dist:  cmp     #$30                ; if |Y dist| >= $30, block still far
        bcs     junk_golem_return               ; done (block still falling from top)
        lda     ent_anim_id,x                 ; if already using throw OAM ($39),
        cmp     #$39                    ; skip animation reset
        beq     junk_golem_throw_anim_done ; if moving up, zero out velocity
        lda     #$39                    ; switch to throwing animation (OAM $39)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_var2,x                 ; set throw-anim flag = 1
        rts

junk_golem_throw_anim_done:  dec     ent_var1,x             ; decrement throw cooldown timer
        lda     ent_anim_frame,x                 ; check if throw animation finished
        ora     ent_anim_state,x                 ; (anim timer=0 AND frame=0)
        bne     junk_golem_return               ; not done yet, skip
        lda     #$38                    ; revert to idle animation (OAM $38)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
junk_golem_return:  rts

; --- spawn_junk_block: create thrown junk block child entity ---

junk_golem_spawn_block:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     junk_golem_spawn_return               ; none available, return
        lda     ent_facing,x                 ; copy parent direction to child
        sta     ent_facing,y            ; $35 = Mag Fly's direction
        lda     ent_x_px,x                 ; copy parent X position to child
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y             ; if player is riding ($05)
        lda     #$04                    ; child Y = $04 (near top of screen)
        sta     ent_y_px,y                 ; block spawns high and falls down
        lda     ent_y_px,x                 ; ent_timer,y = golem's Y position
        sta     ent_timer,y                 ; (target Y for homing transition)
        lda     #$94                    ; entity type $94 (junk block)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$CA                    ; damage flags $CA: hurts player + takes damage
        sta     ent_hitbox,y
        lda     #$24                    ; AI routine = $24 (main_unknown_24)
        sta     ent_routine,y           ; dismount complete
        lda     #$08                    ; HP = 8
        sta     ent_hp,y
junk_golem_spawn_return:  rts

; ===========================================================================
; main_unknown_24 — Junk Golem's thrown block (entity $94)
; State 0: init, set downward speed $04.00 (4 px/frame).
; State 1: fall straight down. When within $20 pixels of target Y (ent_timer,x
;   = golem's Y at spawn time), compute homing velocity toward player at
;   speed $04.80 and switch to routine $0B (generic homing projectile).
; ===========================================================================
main_unknown_24:

        lda     ent_status,x                 ; state 0: init
        and     #$0F
        bne     unknown_24_move_down               ; skip if already initialized
        sta     ent_yvel_sub,x                 ; Y speed = $04.00 (4 px/frame down)
        lda     #$04                    ; (A=0 from AND above, sub=0)
        sta     ent_yvel,x              ; stay idle (returns via RTS above)
        inc     ent_status,x                 ; advance to state 1
unknown_24_move_down:  jsr     move_sprite_down               ; move downward (no collision)
        lda     ent_y_px,x                 ; current Y position
        sec                             ; (prevents unwanted vertical flip
        sbc     ent_timer,x                 ; minus target Y (golem's Y)
        bcs     unknown_24_distance_check               ; if negative,
        eor     #$FF                    ; take absolute value
        adc     #$01
        clc                             ; check if state >= 2 (grounded)
unknown_24_distance_check:  cmp     #$20                ; if |Y dist to target| >= $20,
        bcs     unknown_24_done               ; still falling, done
        lda     #$80                    ; homing speed = $04.80 (4.5 px/frame)
        sta     $02                     ; move down with collision
        lda     #$04                    ; C=0: still airborne, done
        sta     $03                     ; C=1: landed, advance to state 2
        jsr     calc_homing_velocity                   ; compute X/Y velocity toward player
        lda     $0C                     ; set direction from homing result
        sta     ent_facing,x            ; update facing toward player
        lda     #$0B                    ; switch to routine $0B (generic homing)
        sta     ent_routine,x                 ; block now flies toward player
unknown_24_done:  rts                   ; no change, skip flip

; ===========================================================================
; main_pickelman_bull — Pickelman Bull (bulldozer enemy with rider)
; State 0: init — fall speed $04.00, random drive count, $1E frame timer.
; State 1: driving — moves horizontally with collision, $99. Rider
;   hitbox checked at Y-$17 (separate weapon collision). On drive count
;   expired, advance to state 2 (stopped).
; State 2: stopped — rider oscillates left/right (1px per 2 frames).
;   After $1E-frame timer expires, returns to state 1 with new drive count.
; ent_timer = drive step counter (random: $10/$20/$30),
; ent_var1 = stop timer ($1E = 30 frames), ent_var2/ent_var3 = oscillation counters.
; ===========================================================================
main_pickelman_bull:

        lda     ent_status,x                 ; state 0: init
        and     #$0F                    ; Y = child slot index
        bne     pickelman_bull_rider_hit_check               ; skip if already initialized
        sta     ent_yvel_sub,x                 ; Y speed = $04.00 ($99 fall)
        lda     #$04
        sta     ent_yvel,x              ; minus golem Y position
        jsr     pickelman_bull_random_drive               ; get random drive count ($10/$20/$30)
        sta     ent_timer,x                 ; store as drive step counter
        lda     #$1E                    ; stop timer = $1E (30 frames)
        sta     ent_var1,x
        inc     ent_status,x                 ; advance to state 1
        ; --- rider weapon collision check (Y-$17 offset for rider hitbox) ---
pickelman_bull_rider_hit_check:  lda     ent_y_px,x             ; save real Y position
        pha                             ; skip animation reset
        lda     ent_y_px,x
        sec                             ; switch to throwing animation (OAM $39)
        sbc     #$17                        ; offset Y up by 23 px for rider
        sta     ent_y_px,x              ; set throw-anim flag = 1
        lda     #$C3                        ; rider hitbox (vulnerable)
        sta     ent_hitbox,x
        jsr     process_sprites_top_spin_check                   ; check_weapon_hit
        pla                             ; check if throw animation finished
        sta     ent_y_px,x                  ; restore real Y
        lda     ent_hp,x                    ; rider killed?
        beq     pickelman_bull_rts                   ; yes -> rts
        lda     #$AC                        ; bull hitbox (body)
        sta     ent_hitbox,x
        lda     ent_status,x
        and     #$02                        ; state 2 = stopped?
        bne     pickelman_bull_stopped_state                   ; yes -> oscillation logic
        dec     ent_timer,x                 ; decrement drive counter
        bne     pickelman_bull_driving_gravity                   ; still driving
        inc     ent_status,x               ; drive count 0 -> stop state
pickelman_bull_rts:  rts

        ; --- state 1: driving with gravity + wall check ---
pickelman_bull_driving_gravity:  ldy     #$2A
        jsr     move_down_collide                       ; move_down_collide (gravity)
        lda     ent_facing,x            ; child Y = $04 (near top of screen)
        and     #$01                        ; facing right?
        beq     pickelman_bull_check_left_wall                   ; no -> check left
        lda     $42                         ; tile to left
        and     #$10                        ; solid?
        beq     pickelman_bull_reverse_direction                   ; no solid -> reverse
        ldy     #$10                    ; damage flags $CA: hurts player + takes damage
        jsr     move_right_collide                       ; move_right_collide
        jmp     pickelman_bull_wall_done ; AI routine = $24 (main_unknown_24)

pickelman_bull_check_left_wall:  lda     tile_at_feet_hi ; HP = 8
        and     #$10
        beq     pickelman_bull_reverse_direction
        ldy     #$11
        jsr     move_left_collide                   ; move_left_collide
pickelman_bull_wall_done:  bcc     pickelman_bull_rts_2
pickelman_bull_reverse_direction:  lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
pickelman_bull_rts_2:  rts

        ; --- state 2: stopped, rider oscillates left/right ---
pickelman_bull_stopped_state:  dec     ent_var1,x              ; decrement stop timer
        bne     pickelman_bull_oscillate_rider                   ; still stopped
        sta     ent_var2,x                  ; reset oscillation delay
        sta     ent_var3,x                  ; reset oscillation counter
        lda     #$1E                    ; Y speed = $04.00 (4 px/frame down)
        sta     ent_var1,x                  ; new stop timer = 30 frames
        jsr     pickelman_bull_random_drive                   ; random drive count
        sta     ent_timer,x                 ; set new drive step counter
        dec     ent_status,x               ; -> back to driving state
        rts                             ; current Y position

        ; --- oscillation: move rider 1px left/right alternating ---
pickelman_bull_oscillate_rider:  lda     ent_var2,x              ; oscillation delay
        bne     pickelman_bull_delay_decrement                   ; delay active -> decrement
        lda     ent_var3,x                  ; oscillation counter
        and     #$01                        ; even=right, odd=left
        asl     a                       ; if |Y dist to target| >= $20,
        tay                             ; still falling, done
        lda     ent_x_px,x              ; homing speed = $04.80 (4.5 px/frame)
        clc
        adc     pickelman_bull_oscillation_x,y                     ; +1 or -1 pixel
        sta     ent_x_px,x
        lda     ent_x_scr,x             ; compute X/Y velocity toward player
        adc     pickelman_bull_oscillation_scr,y ; set direction from homing result
        sta     ent_x_scr,x
        lda     #$02                    ; switch to routine $0B (generic homing)
        sta     ent_var2,x                  ; 2-frame delay between oscillations
        inc     ent_var3,x
        rts

pickelman_bull_delay_decrement:  .byte   $DE,$40,$05,$60         ; dec ent_var2,x; rts
pickelman_bull_oscillation_x:  .byte   $01                         ; oscillation X offsets: +1, 0, -1, -1
pickelman_bull_oscillation_scr:  brk
        .byte   $FF
        .byte   $FF
; --- random drive count from table ($10/$20/$30/$10) ---
pickelman_bull_random_drive:  lda     $E4                     ; pseudo-random: add two RNG bytes
        adc     $E5
        sta     $E4
        and     #$03
        tay
        .byte   $B9,$2D,$9D,$60
        bpl     bikky_jump_encode       ; state 0: init
        bmi     bikky_facing_decode
; ---------------------------------------------------------------------------
; main_bikky -- Bikky (stomping enemy, entity type $21)
; ---------------------------------------------------------------------------
; Applies gravity, walks horizontally with collision.
; On landing (anim_state=$08, frame=0): launch upward (Y speed $05.A8),
;   face player, hitbox $C5 (dangerous). Sound $20 on subsequent landing.
; While airborne: hitbox $A5 (safe).
; ---------------------------------------------------------------------------
main_bikky:                             ; advance to state 1
        jsr     set_sprite_hflip                       ; set_sprite_hflip
        ldy     #$10                    ; save real Y position
        jsr     move_vertical_gravity                       ; move_vertical_gravity
        bcs     bikky_check_anim_state                   ; landed -> check anim state
        lda     #$00
        sta     ent_anim_frame,x            ; reset anim frame while airborne
        .byte   $BD
bikky_facing_decode:  ldy     #$04                        ; (encoded lda ent_facing,x)
        and     #$01
        beq     bikky_move_left                   ; facing left
        ldy     #$0E
        jmp     move_right_collide                       ; move_right_collide

bikky_move_left:  ldy     #$0F
        .byte   $4C
bikky_jump_encode:  cpy     prg_bank                    ; (encoded jmp move_left_collide)
        ; --- landed: check animation state ---
bikky_check_anim_state:  lda     ent_anim_state,x
        cmp     #$08                        ; stomp anim finished?
        bne     bikky_set_airborne_hitbox                   ; no -> safe hitbox
        lda     ent_anim_frame,x
        beq     bikky_launch_upward                   ; frame 0 -> launch upward
        lda     #$00                        ; frame > 0 -> reset anim, play sound
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        lda     #SFX_STOMP
        jsr     submit_sound_ID                       ; submit_sound_ID (stomp)
        rts

bikky_launch_upward:  lda     #$A8                    ; launch upward: Y speed = $05.A8
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        jsr     face_player                       ; face_player
        lda     #$C5
        sta     ent_hitbox,x                ; dangerous hitbox (landing)
        rts

bikky_set_airborne_hitbox:  lda     #$A5                    ; safe hitbox (airborne)
        sta     ent_hitbox,x
        rts
; ---------------------------------------------------------------------------
; main_magnet_force -- Magnet Force (horizontal pull/push, type $25)
; ---------------------------------------------------------------------------
; Applies horizontal force on player when within range (Y<$1C, X<$68).
; Uses entity flag bit 6 for direction. Sets $36/$37/$38 for pull effect.
; ---------------------------------------------------------------------------
main_magnet_force:

        jsr     entity_y_dist_to_player                       ; entity_y_dist_to_player
        cmp     #$1C                        ; within 28 px vertically?
        bcs     magnet_force_done                   ; no -> rts
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        ror     L0000                       ; carry = direction -> $00 bit 7
        cmp     #$68                        ; within 104 px horizontally?
        bcs     magnet_force_done                   ; no -> rts
        lda     ent_flags,x
        and     #$40                        ; entity facing (bit 6)
        bne     magnet_force_player_check                   ; facing left -> branch
        lda     L0000                       ; facing right: player to right?
        bmi     magnet_force_done                   ; no -> no force
        lda     #$01                        ; pull direction = right
        bne     magnet_force_direction_set
magnet_force_player_check:  lda     L0000                   ; facing left: player to left?
        bpl     magnet_force_done                   ; no -> no force
        lda     #$02                        ; pull direction = left
magnet_force_direction_set:  sta     $36                     ; set magnet pull direction
        lda     #$00
        sta     $37                         ; pull sub-speed = 0
        lda     #$01
        sta     $38                         ; pull whole-speed = 1
magnet_force_done:  rts
; ---------------------------------------------------------------------------
; main_new_shotman -- New Shotman (shooting enemy, entity type $05)
; ---------------------------------------------------------------------------
; State 0: init timer=$1E.
; State 1: if player within $50 px, shoot anim $5A, spawn falling projectile.
;   After 2 shots, walk state (ent_var2=$78 timer).
; State 2: walk timer, return to shoot. Fires horizontal bullets every $1E
;   frames, 3 per burst, then $5A frame cooldown.
; shotman_spawn_falling_proj: spawn falling projectile (type $73, Y speed $04.00, routine $0C).
; shotman_spawn_bullet_pair: spawn horizontal bullet pair (speed $01.80, type $73, routine $1B).
; ---------------------------------------------------------------------------
main_new_shotman:

        lda     ent_status,x
        and     #$0F
        bne     shotman_check_state                   ; skip init
        lda     #$1E
        sta     ent_timer,x                 ; fire timer = 30 frames
        inc     ent_status,x                ; -> state 1
shotman_check_state:  lda     ent_status,x
        and     #$02                        ; bit 1 = walking state?
        bne     shotman_walk_timer                   ; yes -> walk countdown
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        cmp     #$50                        ; within 80 px?
        bcs     shotman_fire_loop                   ; no -> fire timer only
        lda     ent_var2,x                  ; shot cooldown active?
        bne     shotman_shot_cooldown                   ; yes -> count down
        lda     #$5A                        ; shooting animation
        jsr     reset_sprite_anim                       ; reset_sprite_anim
        jsr     face_player                       ; face_player
        jsr     shotman_spawn_falling_proj                   ; spawn falling projectile
        lda     #$1E
        sta     ent_var2,x                  ; shot cooldown = 30 frames
        rts

shotman_shot_cooldown:  dec     ent_var2,x              ; decrement shot cooldown
        bne     shotman_fire_loop
        inc     ent_var3,x                  ; shot count++
        lda     ent_var3,x
        cmp     #$02                        ; fired 2 shots?
        bcc     shotman_fire_loop
        lda     #$00
        sta     ent_var3,x
        lda     #$78
        sta     ent_var2,x
        inc     ent_status,x
        rts

        ; --- walk state ---
shotman_walk_timer:  dec     ent_var2,x              ; walk timer--
        bne     shotman_fire_loop
        dec     ent_status,x               ; -> back to shoot state
        ; --- horizontal bullet fire timer ---
shotman_fire_loop:  dec     ent_timer,x
        bne     shotman_check_anim_done                   ; timer not expired
        lda     #$00
        sta     $01                         ; bullet pair counter
        jsr     shotman_spawn_bullet_pair                   ; spawn horizontal bullet pair
        lda     #$1E
        sta     ent_timer,x                 ; reset fire timer
        inc     ent_var1,x                  ; burst count++
        lda     ent_var1,x
        cmp     #$03                        ; 3 bursts in a row?
        bcc     shotman_check_anim_done                   ; no -> keep firing
        lda     #$5A
        sta     ent_timer,x                 ; long cooldown = 90 frames
        lda     #$00
        sta     ent_var1,x                  ; reset burst count
shotman_check_anim_done:  lda     ent_anim_id,x           ; if still in shoot anim ($5A)
        cmp     #$5A
        bne     shotman_return
        lda     ent_anim_frame,x            ; and anim done...
        ora     ent_anim_state,x
        bne     shotman_return
        lda     #$59                        ; ...return to idle anim
        jsr     reset_sprite_anim                       ; reset_sprite_anim
shotman_return:  rts

; --- new shotman: spawn horizontal bullet pair (speed $01.80, type $73) ---
; Fires two bullets by flipping facing between iterations ($01 counter).
shotman_spawn_bullet_pair:  jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     shotman_bullet_no_slot                   ; no slot -> rts
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     shotman_bullet_x_offset,y
        pha
        lda     ent_x_scr,x
        adc     shotman_bullet_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sec
        sbc     #$04
        sta     ent_y_px,y
        lda     #$80
        sta     ent_xvel_sub,y
        lda     #$01
        sta     ent_xvel,y
        lda     #$73
        jsr     init_child_entity                   ; init_child_entity
        lda     #$1B
        sta     ent_routine,y
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_hp,y
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        inc     $01
        lda     $01
        cmp     #$02
        .byte   $90,$A2
shotman_bullet_no_slot:  .byte   $60
shotman_bullet_x_offset:  .byte   $0F
shotman_bullet_x_scr:  brk
        sbc     (ppu_ctrl_shadow),y
; --- new shotman: spawn falling projectile (type $73, Y speed $04.00) ---
shotman_spawn_falling_proj:  jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     shotman_bullet_no_slot                   ; no slot -> rts
        lda     #$00                        ; Y speed = $04.00 (falling)
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$73                        ; entity type $73 (projectile)
        jsr     init_child_entity                       ; init_child_entity
        lda     #$8B
        sta     ent_hitbox,y                ; projectile hitbox
        lda     #$0C
        sta     ent_routine,y               ; routine $0C (falling projectile)
        lda     ent_x_px,x                  ; copy parent position
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sec
        sbc     #$10                        ; spawn 16 px above parent
        sta     ent_y_px,y
        lda     ent_y_scr,x
        sta     ent_y_scr,y
        lda     ent_facing,x               ; copy facing
        sta     ent_facing,y
        ; --- set X speed based on distance to player ---
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        stx     L0000
        ldx     #$03                        ; scan distance brackets
shotman_distance_scan:  cmp     shotman_distance_table,x                 ; distance < threshold?
        bcc     shotman_set_proj_speed                   ; yes -> use this speed
        dex
        bne     shotman_distance_scan
shotman_set_proj_speed:  lda     shotman_xvel_sub_table,x                 ; X speed sub from table
        sta     ent_xvel_sub,y
        lda     shotman_xvel_table,x                    ; X speed whole from table
        sta     ent_xvel,y
        .byte   $A6,$00,$60
; X speed lookup by distance bracket: $4C/$3D/$2E/$1F thresholds
shotman_distance_table:  .byte   $4C,$3D,$2E,$1F             ; distance thresholds
shotman_xvel_sub_table:  .byte   $00,$80,$00,$80             ; X speed sub values
shotman_xvel_table:  .byte   $02                         ; X speed whole: $02, $01, $01, $00
        ora     ($01,x)
        brk
; --- generic projectile AI: gravity + walk, used by new_shotman bullets ---
        ldy     #$12
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcs     shotman_proj_collision
        lda     ent_facing,x
        and     #$01
        beq     shotman_proj_move_left
        ldy     #$1E
        jsr     move_right_collide                   ; move_right_collide
        jmp     shotman_proj_after_move

shotman_proj_move_left:  ldy     #$1F
        jsr     move_left_collide                   ; move_left_collide
shotman_proj_after_move:  bcs     shotman_proj_collision
        rts

shotman_proj_collision:  lda     ent_routine,x
        cmp     #$0C
        bne     shotman_debris_spawn
        lda     #$00
        sta     ent_routine,x
        lda     $B3
        bpl     shotman_proj_death_upper
        lda     #$59
        bne     shotman_proj_death_anim
shotman_proj_death_upper:  lda     #$71
shotman_proj_death_anim:  jmp     reset_sprite_anim               ; reset_sprite_anim

        lda     ent_facing,x
        and     #$01
        beq     shotman_debris_move_left
        ldy     #$0C
        jsr     move_right_collide                   ; move_right_collide
        jmp     shotman_debris_after_move

shotman_debris_move_left:  ldy     #$0D
        jsr     move_left_collide                   ; move_left_collide
shotman_debris_after_move:  bcs     shotman_debris_spawn
        lda     ent_facing,x
        and     #$08
        beq     shotman_debris_move_down
        jmp     move_sprite_up                   ; move_sprite_up

shotman_debris_move_down:  jmp     move_sprite_down               ; move_sprite_down

shotman_debris_spawn:  lda     #$00
        sta     ent_status,x
        sta     $01
        lda     #$FF
        sta     ent_spawn_id,x
shotman_debris_spawn_loop:
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     shotman_debris_rts
        sty     L0000
        lda     ent_facing,x
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     gyoraibo_child_x_off,y
        pha
        lda     ent_x_scr,x
        adc     gyoraibo_child_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$5B
        jsr     init_child_entity                   ; init_child_entity
        lda     #$0C
        sta     ent_routine,y
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_hp,y
        stx     $02
        ldx     $01
        lda     shotman_debris_facing,x
        sta     ent_facing,y
        lda     shotman_debris_yvel_sub,x
        sta     ent_yvel_sub,y
        lda     shotman_debris_yvel,x
        sta     ent_yvel,y
        lda     shotman_debris_xvel_sub,x
        sta     ent_xvel_sub,y
        lda     shotman_debris_xvel,x
        sta     ent_xvel,y
        ldx     $02
        inc     $01
        lda     $01
        cmp     #$04
        .byte   $90,$96
shotman_debris_rts:  .byte   $60
shotman_debris_facing:  .byte   $01,$01,$02,$02
shotman_debris_yvel_sub:  .byte   $9E,$44,$9E,$44
shotman_debris_yvel:  .byte   $04,$03,$04,$03
shotman_debris_xvel_sub:  .byte   $CC,$80,$CC,$80
shotman_debris_xvel:  brk
        brk
        brk
        brk

; ---------------------------------------------------------------------------
; main_proto_man — Proto Man (Break Man) fighting encounter
; Used by both routine $52 (normal) and $53 (Hard Man scripted).
; Routine $52: Magnet Man stage (global enemy ID $3E). On defeat → $68=$80.
; Routine $53: Hard Man stage (hardcoded spawn in bank18, slot $1F).
;   On defeat → player state $13 (teleport_beam). Only $53 triggers this.
; ent_routine - $52 indexes into data tables at $A176+ for per-variant animations.
; ---------------------------------------------------------------------------
main_proto_man:
        jsr     cutscene_init
        lda     ent_var3,x
        beq     shotman_debris_rts
        .byte   $BD

; ===========================================================================
; BANK $1D — Entity AI routines (continued)
; ===========================================================================
; This bank continues main_proto_man from bank $1C. The .byte $BD at end of
; bank $1C + .byte $E0,$04 here form a cross-bank LDA $04E0,x instruction.
; ===========================================================================

.segment "BANK1D"

proto_man_opcode_data:  .byte   $E0,$04                 ; cross-bank: LDA ent_hp,x (opcode $BD in bank $1C)
        bne     proto_man_alive               ; HP > 0 → alive, continue AI
        jmp     proto_man_defeated_start               ; HP = 0 → defeated sequence

; --- Proto Man alive: state machine ---
proto_man_alive:  lda     ent_status,x            ; extract sub-state
        and     #$0F
        bne     proto_man_state_dispatch               ; nonzero → already initialized
        sta     ent_anim_frame,x        ; state 0: reset anim frame
        lda     ent_y_px,x
        cmp     #$90                    ; above Y=$90?
        bcs     proto_man_init_landing               ; no → apply gravity+collision
        jmp     apply_y_speed                   ; yes → apply Y speed (falling)

proto_man_init_landing:  ldy     #$00
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     proto_man_walk_phase               ; no floor hit → walking logic
        lda     ent_routine,x           ; routine $52 or $53
        sec
        sbc     #$52                    ; index 0 or 1 into tables
        tay
        lda     proto_man_timer_table,y                 ; timer variant from table
        sta     ent_timer,x
        inc     ent_status,x            ; advance to state 1
proto_man_state_dispatch:  lda     ent_anim_id,x
        cmp     #$99                    ; teleport beam sprite?
        bne     proto_man_check_attack
        lda     ent_anim_state,x
        cmp     #$04                    ; anim complete?
        bne     proto_man_walk_phase
        lda     ent_timer,x             ; load variant index
        tya
        lda     proto_man_walk_anim_table,y                 ; walking anim from table
        jsr     reset_sprite_anim                   ; reset_sprite_anim
proto_man_check_attack:  lda     ent_status,x
        and     #$02                    ; state >= 2?
        beq     proto_man_distance_check
        jmp     proto_man_attack_phase               ; → attacking phase

proto_man_distance_check:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$60                    ; within 96 px?
        bcs     proto_man_walk_phase               ; no → keep walking
        inc     ent_status,x            ; yes → advance to attack
        rts

; --- walking phase: gravity + horizontal movement ---
proto_man_walk_phase:  ldy     #$00
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        rol     $0F                     ; save carry (landed) into $0F bit 0
        lda     ent_facing,x
        and     #$01
        beq     proto_man_walk_right
        ldy     #$00
        jsr     move_right_collide                   ; move_right_collide
        jmp     proto_man_walk_collision

proto_man_walk_right:  ldy     #$01
        jsr     move_left_collide                   ; move_left_collide
proto_man_walk_collision:  lda     $0F
        and     #$01                    ; check if landed on floor
        beq     proto_man_walk_done               ; airborne → skip wall checks
        lda     ent_facing,x
        and     #$01
        beq     proto_man_walk_left_edge
        lda     ent_x_px,x
        cmp     #$D6                    ; near right edge?
        bcc     proto_man_walk_jump_check
        jmp     proto_man_reverse_facing               ; → reverse direction

proto_man_walk_left_edge:  lda     ent_x_px,x
        cmp     #$2A                    ; near left edge?
        bcs     proto_man_walk_jump_check
proto_man_reverse_facing:  lda     ent_facing,x          ; reverse facing direction
        eor     #$03
        sta     ent_facing,x
        rts

proto_man_walk_jump_check:  lda     $10
        and     #$10                    ; floor collision flag?
        beq     proto_man_walk_anim               ; no floor → check walking anim
        lda     ent_timer,x             ; variant index
        tya
        lda     proto_man_jump_anim_table,y                 ; jump anim from table
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$A8                    ; set Y velocity for jump
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        rts

proto_man_walk_anim:  lda     ent_timer,x          ; variant index
        tay
        lda     proto_man_walk2_anim_table,y                 ; walk anim from table
        cmp     ent_anim_id,x           ; already set?
        beq     proto_man_walk_done               ; yes → skip
        lda     ent_timer,x
        tay
        lda     proto_man_walk2_anim_table,y
        jsr     reset_sprite_anim                   ; reset_sprite_anim
proto_man_walk_done:  rts

; --- attacking phase (state 2) ---
proto_man_attack_phase:  lda     ent_var2,x          ; attack cooldown
        bne     proto_man_attack_cooldown               ; still cooling down → decrement
        lda     ent_facing,x
        and     #$01
        beq     proto_man_attack_left
        ldy     #$00
        jsr     move_right_collide                   ; move_right_collide
        jmp     proto_man_attack_edges

proto_man_attack_left:  ldy     #$01
        jsr     move_left_collide                   ; move_left_collide
proto_man_attack_edges:  lda     ent_facing,x          ; edge/wall checks same as walking
        and     #$01
        beq     proto_man_attack_x_check
        lda     ent_x_px,x
        cmp     #$D6
        bcc     proto_man_attack_landed
        jmp     proto_man_attack_reverse

proto_man_attack_x_check:  .byte   $BD                 ; cross-bank LDA ent_x_px,x
        rts                             ; (rts = $60, part of the address)

proto_man_x_check_byte:  .byte   $03                     ; ($0360 = ent_x_px)
        cmp     #$2A                    ; near left edge?
        bcs     proto_man_attack_landed
proto_man_attack_reverse:  lda     ent_facing,x          ; reverse direction at edges
        eor     #$03
        sta     ent_facing,x
proto_man_attack_landed:  ldy     #$00
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     proto_man_attack_airborne               ; airborne → check anim
        lda     #$04                    ; landed: set attack cooldown
        sta     ent_var2,x
        lda     ent_timer,x
        tay
        lda     proto_man_walk2_anim_table,y                 ; walk anim from table
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$A8                    ; set jump velocity
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$60                    ; player far away (> 96 px)?
        bcc     proto_man_done               ; no → stay in attack
        dec     ent_status,x            ; yes → revert to walking
        jsr     reset_gravity                   ; reset_gravity
proto_man_done:  rts

proto_man_attack_cooldown:  dec     ent_var2,x          ; decrement attack cooldown
        rts

proto_man_attack_airborne:  lda     ent_yvel,x          ; airborne: check Y direction
        bpl     proto_man_attack_falling               ; rising → jump anim
        lda     ent_timer,x             ; falling → use different anim
        tay
        lda     proto_man_jump_anim_table,y                 ; jump anim from table
        jmp     reset_sprite_anim                   ; reset_sprite_anim

proto_man_attack_falling:  lda     ent_timer,x          ; falling: shooting anim
        tya
        lda     proto_man_shoot_anim_table,y                 ; shoot anim from table
        cmp     ent_anim_id,x
        beq     proto_man_fire_projectile               ; already set → skip
        lda     ent_timer,x
        tya
        lda     proto_man_shoot_anim_table,y
        jsr     reset_sprite_anim                   ; reset_sprite_anim
proto_man_fire_projectile:  lda     ent_anim_state,x
        cmp     #$01                    ; at frame 1?
        bne     proto_man_done               ; no → return
        jsr     proto_man_cutscene_spawn_projectile               ; spawn projectile
        lda     #$00                    ; reset anim after firing
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        rts

; Proto Man variant data tables (indexed by routine - $52):
;   index 0 = routine $52 (normal), index 1 = routine $53 (Hard Man)
proto_man_timer_table:  .byte   $00,$01                 ; timer variant
proto_man_walk_anim_table:  .byte   $88,$8A                 ; walking anim IDs
proto_man_jump_anim_table:  .byte   $86,$8F                 ; jump anim IDs
proto_man_walk2_anim_table:  .byte   $83,$8C                 ; walk anim IDs
proto_man_shoot_anim_table:  .byte   $85,$8E                 ; shoot anim IDs
; --- Proto Man defeated: fly upward off screen ---
proto_man_defeated_start:  lda     #$99                ; teleport beam sprite
        cmp     ent_anim_id,x
        beq     proto_man_defeated_rise               ; already set → skip to accelerate
        lda     #$00
        sta     ent_hitbox,x            ; remove hitbox
        tay
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     proto_man_cutscene_not_started               ; airborne → return
        lda     #$99                    ; set teleport beam sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_anim_state,x
        lda     #$00                    ; clear Y velocity
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
proto_man_defeated_rise:  lda     ent_anim_state,x    ; wait for anim to complete
        bne     proto_man_cutscene_not_started
        sta     ent_anim_frame,x        ; hold on frame 0
        lda     ent_yvel_sub,x          ; accelerate upward
        clc
        adc     gravity
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        cmp     #$0C                    ; cap speed at $0C
        bne     proto_man_defeated_cap_speed
        lda     #$00
        sta     ent_yvel_sub,x
proto_man_defeated_cap_speed:  jsr     move_sprite_up               ; move_sprite_up
        lda     ent_y_scr,x
        beq     proto_man_cutscene_not_started
        lda     #$00
        sta     ent_status,x
        lda     ent_routine,x                 ; entity routine index
        cmp     #$53                    ; $53 = Hard Man stage Proto Man
        bne     proto_man_defeated_set_flag               ; $52 = normal → skip, set $68
        lda     #PSTATE_TELEPORT_BEAM                    ; state → $13 (teleport_beam)
        sta     player_state                     ; Proto Man defeated → player beams out
        rts

proto_man_defeated_set_flag:  lda     #$80                ; $68 = cutscene-complete flag
        sta     proto_man_flag
proto_man_cutscene_not_started:  rts

; ---------------------------------------------------------------------------
; main_proto_man_gemini_cutscene — Proto Man / Gemini Man intro cutscene
; Freezes player in state $0F (stunned) while cutscene plays.
; Proto Man whistles, drops down, then flies away.
; When done, releases player back to state $00 (on_ground).
; Entity routine $53 = Proto Man triggers state $13 (teleport_beam) on exit.
; ---------------------------------------------------------------------------
main_proto_man_gemini_cutscene:

        jsr     cutscene_init           ; cutscene init/whistle
        lda     ent_var3,x                 ; phase flag
        beq     proto_man_cutscene_not_started               ; not started yet → return
        lda     #PSTATE_STUNNED                    ; state → $0F (stunned)
        sta     player_state                     ; freeze player during cutscene
        lda     ent_status,x
        and     #$0F
        bne     proto_man_cutscene_main_state
        jsr     apply_y_speed                   ; apply_y_speed
        lda     #$9C
        cmp     ent_y_px,x
        bcs     proto_man_cutscene_clear_anim_frame
        sta     ent_y_px,x
        lda     ent_anim_state,x
        cmp     #$04
        bne     proto_man_cutscene_return
        lda     #$88
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_status,x
        lda     #$FF
        sta     ent_timer,x
proto_man_cutscene_main_state:  lda     ent_timer,x
        beq     proto_man_cutscene_defeat_rise
        dec     ent_timer,x
        bne     proto_man_cutscene_clear_anim_frame
        lda     #$99
        sta     ent_anim_id,x
        inc     ent_anim_state,x
        lda     #$00
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
proto_man_cutscene_defeat_rise:  jsr     proto_man_defeated_rise
        lda     proto_man_flag                     ; cutscene-complete flag
        beq     proto_man_cutscene_return               ; not done yet → return
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; release player from stun
        ldy     #$0F                    ; clear all weapon slots
proto_man_cutscene_clear_weapons_loop:  sta     $0310,y
        dey
        bpl     proto_man_cutscene_clear_weapons_loop
proto_man_cutscene_clear_anim_frame:  lda     #$00
        sta     ent_anim_frame,x
proto_man_cutscene_return:  rts

; cutscene init — freeze player and play Proto Man's whistle

cutscene_init:  lda     ent_var3,x         ; if phase already started,
        bne     proto_man_cutscene_init_return               ; skip init
        lda     player_state                     ; if player already stunned,
        bne     proto_man_cutscene_play_whistle               ; skip to whistle
        lda     ent_anim_id                   ; player OAM ID
        cmp     #$13                    ; $13 = teleporting? skip
        beq     proto_man_cutscene_init_return
        lda     #PSTATE_STUNNED                    ; state → $0F (stunned)
        sta     player_state                     ; freeze player for cutscene
proto_man_cutscene_play_whistle:  lda     #$11
        cmp     $D9
        beq     proto_man_cutscene_whistle_timer
        jsr     submit_sound_ID_D9                   ; submit_sound_ID_D9
        lda     #$B4
        sta     ent_timer,x
proto_man_cutscene_whistle_timer:  dec     ent_timer,x
        bne     proto_man_cutscene_init_return
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; whistle done, release player
        inc     ent_var3,x                 ; advance to next cutscene phase
        lda     ent_flags,x
        and     #$FB                    ; clear bit 2 (disabled flag)
        sta     ent_flags,x
        lda     ent_routine,x                 ; entity routine index
        cmp     #$53                    ; $53 = Proto Man
        bne     proto_man_cutscene_select_music
        lda     #$0C                    ; Proto Man stage music ($0C)
        bne     proto_man_cutscene_submit_music
proto_man_cutscene_select_music:  lda     stage_id                     ; else play stage music
        clc                             ; (stage index + 1)
        adc     #$01
proto_man_cutscene_submit_music:  jsr     submit_sound_ID_D9                   ; submit_sound_ID_D9
proto_man_cutscene_init_return:  rts

; --- spawn Proto Man projectile ---
proto_man_cutscene_spawn_projectile:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     proto_man_cutscene_spawn_abort               ; no slot → abort
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; copy parent facing to child
        sta     ent_facing,y            ; state → $13 (teleport_beam)
        and     #$02                    ; direction offset for X spawn table
        tay
        lda     ent_x_px,x              ; child X = parent X + offset
        clc                             ; $68 = cutscene-complete flag
        adc     proto_man_proj_x_offset,y
        pha
        lda     ent_x_scr,x
        adc     proto_man_proj_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x              ; child Y = parent Y
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y                ; HP = 0 (indestructible)
        sta     ent_xvel_sub,y          ; cutscene init/whistle
        lda     #$04                    ; phase flag
        sta     ent_xvel,y              ; X speed = 4
        lda     ent_routine,x           ; state → $0F (stunned)
        and     #$01                    ; routine $52 vs $53
        bne     proto_man_cutscene_routine_53_oam
        lda     #$18                    ; routine $52: OAM $18
        bne     proto_man_cutscene_init_projectile
proto_man_cutscene_routine_53_oam:  lda     #$73                ; routine $53: OAM $73
proto_man_cutscene_init_projectile:  jsr     init_child_entity               ; init_child_entity
        lda     #$8B                    ; hitbox = projectile
        sta     ent_hitbox,y
        lda     #$1B                    ; AI routine = $1B
        sta     ent_routine,y
proto_man_cutscene_spawn_abort:  rts

; Proto Man projectile X spawn offsets (indexed by facing direction):
proto_man_proj_x_offset:  .byte   $0D
proto_man_proj_x_scr:  .byte   $00,$F3,$FF

; ===========================================================================
; main_hari_harry — Hard Hat Machine (wall-mounted turret enemy)
; Hides inside helmet, periodically opens to fire 5 bullets in a spread,
; then deploys as a walking enemy that bounces off walls. After walk timer
; expires, retracts back into helmet and repeats.
; States: 0=init, 1=hidden/wait, 2=firing, 3=walking/deployed
; ===========================================================================
main_hari_harry:
        lda     ent_status,x                 ; state 0: init
        and     #$0F                    ; extract sub-state
        bne     hari_harry_state_dispatch
        jsr     set_sprite_hflip                   ; face toward player
        lda     #$3C                    ; hide timer = 60 frames
        sta     ent_var3,x              ; not done yet → return
        inc     ent_status,x                 ; advance to state 1 (hidden)
hari_harry_state_dispatch:  lda     ent_status,x             ; dispatch by sub-state
        and     #$0F                    ; clear all weapon slots
        cmp     #$02                    ; state 2 = firing
        beq     hari_harry_firing
        cmp     #$03                    ; state 3 = walking
        beq     hari_harry_walking
        dec     ent_var3,x                 ; state 1: count down hide timer
        bne     hari_harry_freeze_anim
        lda     #$00                    ; timer expired: clear it
        sta     ent_var3,x
        inc     ent_status,x                 ; advance to state 2 (firing)
hari_harry_freeze_anim:  lda     #$00                ; freeze animation while hidden
        sta     ent_anim_frame,x                 ; (reset anim timer and frame)
        sta     ent_anim_state,x        ; if player already stunned,
        rts                             ; skip to whistle

; --- state 2: firing sequence (turret open) ---

hari_harry_firing:  lda     ent_timer,x             ; if firing-done flag set,
        bne     hari_harry_post_fire_pause               ; skip to post-fire pause
        lda     ent_anim_state,x                 ; check anim frame 1, tick 1:
        cmp     #$01                    ; fire first bullet spread
        bne     hari_harry_fire_spread_2
        lda     ent_anim_frame,x
        cmp     #$01
        bne     hari_harry_fire_spread_2
        lda     #$04                    ; spawn 5 bullets (index 0-4)
        sta     $01
        stx     L0000                   ; state → $00 (on_ground)
        jsr     hari_harry_spawn_bullets ; whistle done, release player
hari_harry_fire_spread_2:  lda     ent_anim_state,x             ; check anim frame 3, tick 1:
        cmp     #$03                    ; fire second bullet spread
        bne     hari_harry_fire_complete ; clear bit 2 (disabled flag)
        lda     ent_anim_frame,x
        cmp     #$01                    ; entity routine index
        bne     hari_harry_fire_complete ; $53 = Proto Man
        lda     #$04                    ; spawn 5 bullets (index 0-4)
        sta     $01                     ; Proto Man stage music ($0C)
        stx     L0000
        jsr     hari_harry_spawn_bullets ; else play stage music
hari_harry_fire_complete:  lda     ent_anim_state,x             ; check anim frame 4, tick 2:
        cmp     #$04                    ; firing animation complete
        bne     hari_harry_fire_rts
        lda     ent_anim_frame,x
        cmp     #$02
        bne     hari_harry_fire_rts
        inc     ent_timer,x                 ; set firing-done flag
        lda     #$10                    ; post-fire pause = 16 frames
        sta     ent_var1,x
hari_harry_fire_rts:  rts

; --- post-fire pause, then transition to walking ---

hari_harry_post_fire_pause:  lda     #$00                ; freeze animation during pause
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        dec     ent_var1,x                 ; count down post-fire timer
        bne     hari_harry_pause_rts
        inc     ent_status,x                 ; advance to state 3 (walking)
        lda     #$77                    ; set walking sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        jsr     face_player                   ; face player for walk direction
        lda     #$5A                    ; walk timer = 90 frames
        sta     ent_var2,x
hari_harry_pause_rts:  rts

; --- state 3: walking/deployed ---

hari_harry_walking:  lda     ent_anim_id,x             ; check if using helmet sprite (OAM $76)
        cmp     #$76                    ; if so, do retract-and-fire sequence
        beq     hari_harry_retract_fire
        ldy     #$0E                    ; apply $99, speed index $0E
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     hari_harry_walk_timer               ; no floor hit: skip horizontal move
        lda     #$AA                    ; on floor: set damage flags (hurts player, takes damage)
        sta     ent_hitbox,x
        lda     ent_facing,x                 ; check facing direction
        and     #$01
        beq     hari_harry_walk_left               ; branch if facing left
        ldy     #$1C                    ; move right with collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     hari_harry_wall_check

hari_harry_walk_left:  ldy     #$1D                ; move left with collision
        jsr     move_left_collide                   ; move_left_collide
hari_harry_wall_check:  bcc     hari_harry_walk_timer           ; no wall hit: continue
        lda     ent_facing,x                 ; wall hit: reverse direction
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x
hari_harry_walk_timer:  lda     ent_var3,x             ; check retract phase flag
        bne     hari_harry_retract               ; if set, count down retract timer
        dec     ent_var2,x                 ; decrement walk timer
        bne     hari_harry_walk_rts               ; not zero: keep walking
        lda     #$C6                    ; walk timer expired: switch to helmet
        sta     ent_hitbox,x                 ; damage flags = invincible in helmet
        lda     #$76                    ; set helmet closing sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$FF                    ; retract timer = 255 frames
        sta     ent_var2,x              ; face toward player
        inc     ent_var3,x                 ; set retract phase flag
hari_harry_walk_rts:  rts

; --- retract phase: wait in helmet, then reset to state 2 ---

hari_harry_retract:  dec     ent_var2,x             ; count down retract timer
        bne     hari_harry_walk_rts               ; not done yet: wait
        lda     #$00                    ; reset all entity-specific counters
        sta     ent_timer,x                 ; firing-done flag
        sta     ent_var1,x                 ; post-fire timer
        sta     ent_var2,x                 ; walk/retract timer
        sta     ent_var3,x                 ; retract phase flag
        lda     #$C6                    ; damage flags = shielded helmet
        sta     ent_hitbox,x            ; advance to state 2 (firing)
        lda     #$76                    ; set helmet sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$82                    ; reset to state 2 (active + firing)
        sta     ent_status,x                 ; skips init, goes straight to fire
        rts

; --- helmet retract-and-fire: single shot while closing ---

hari_harry_retract_fire:  lda     ent_anim_state,x             ; wait for anim frame 1, tick 2
        cmp     #$01                    ; (helmet opening frame)
        bne     hari_harry_retract_rts  ; fire first bullet spread
        lda     ent_anim_frame,x
        cmp     #$02
        bne     hari_harry_retract_rts
        lda     #$04                    ; fire 5 bullets
        sta     $01                     ; spawn 5 bullets (index 0-4)
        stx     L0000
        jsr     hari_harry_spawn_bullets
        lda     #$10                    ; set post-fire pause = 16 frames
        sta     ent_var1,x              ; check anim frame 3, tick 1:
        dec     ent_status,x                 ; go back to state 2 (wait for pause)
hari_harry_retract_rts:  rts

; --- spawn_hari_harry_bullets: loop spawns up to 5 projectiles ---
; $01 = bullet index (counts down from 4 to 0), $00 = parent slot

hari_harry_spawn_bullets:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     hari_harry_spawn_done               ; no slot: abort
        ldx     $01                     ; X = bullet index for table lookup
        lda     hari_harry_bullet_xvel_sub,x                 ; set X speed sub from table
        sta     ent_xvel_sub,y          ; firing animation complete
        lda     hari_harry_bullet_xvel,x                 ; set X speed whole from table
        sta     ent_xvel,y
        lda     hari_harry_bullet_yvel_sub,x                 ; set Y speed sub from table
        sta     ent_yvel_sub,y
        lda     hari_harry_bullet_yvel,x                 ; set Y speed whole from table
        sta     ent_yvel,y              ; post-fire pause = 16 frames
        lda     hari_harry_bullet_facing,x                 ; set direction flags from table
        sta     ent_facing,y
        lda     hari_harry_bullet_oam_id,x                 ; init child with OAM ID from table
        jsr     init_child_entity                   ; init_child_entity
        lda     hari_harry_bullet_flags,x                 ; set sprite flags from table
        sta     ent_flags,y             ; freeze animation during pause
        ldx     L0000                   ; restore parent slot to X
        lda     #$CB                    ; bullet damage flags (projectile)
        sta     ent_hitbox,y            ; count down post-fire timer
        lda     #$36                    ; AI routine = $36 (bullet movement)
        sta     ent_routine,y           ; advance to state 3 (walking)
        lda     ent_x_px,x                 ; copy parent X position to bullet
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; face player for walk direction
        sta     ent_x_scr,y             ; walk timer = 90 frames
        lda     ent_y_px,x                 ; copy parent Y position to bullet
        sta     ent_y_px,y
        lda     #$01                    ; set bullet HP = 1
        sta     ent_hp,y
        dec     $01                     ; loop: next bullet index
        bpl     hari_harry_spawn_bullets               ; continue until all 5 spawned
hari_harry_spawn_done:  ldx     L0000               ; restore parent slot to X
        rts

; bullet speed/dir/OAM/sprite tables (5 entries each, indexed 0-4):
; $A475: X speed sub    $A47A: X speed whole   $A47F: Y speed sub
; $A484: Y speed whole  $A489: direction flags  $A48E: OAM ID
; $A493: sprite flags

hari_harry_bullet_xvel_sub:  .byte   $00,$1F,$00,$00,$1F
hari_harry_bullet_xvel:  .byte   $00,$02,$03,$03,$02 ; branch if facing left
hari_harry_bullet_yvel_sub:  .byte   $00,$E1,$00,$00,$E1 ; move right with collision
hari_harry_bullet_yvel:  .byte   $FD,$FD,$00,$00,$FD
hari_harry_bullet_facing:  .byte   $02,$02,$02,$01,$01
hari_harry_bullet_oam_id:  .byte   $40,$79,$78,$78,$79
hari_harry_bullet_flags:  .byte   $90,$90,$90,$D0,$D0 ; move left with collision

; ===========================================================================
; main_nitron — Nitron (rocket/missile enemy, Gemini Man stage)
; Approaches player horizontally, then flies in a sinusoidal wave pattern
; while dropping bombs. After 2 bomb drops, climbs away upward.
; States: 0=approach, 1=sine wave flight + bomb, 2=climb away
; ent_timer=frame delay, ent_var1=sine index, ent_var2=bomb count, ent_var3=bomb cooldown
; ===========================================================================
main_nitron:                            ; not zero: keep walking
        lda     ent_status,x                 ; state 0: approach
        and     #$0F                    ; damage flags = invincible in helmet
        bne     nitron_state_dispatch   ; set helmet closing sprite
        lda     ent_facing,x                 ; check direction
        and     #$01                    ; retract timer = 255 frames
        beq     nitron_approach_left               ; branch if moving left
        jsr     move_sprite_right                   ; move right toward player
        jmp     nitron_check_distance

nitron_approach_left:  jsr     move_sprite_left               ; move left toward player
nitron_check_distance:  jsr     entity_x_dist_to_player               ; check X distance to player
        cmp     #$40                    ; if < 64 pixels away,
        bcs     nitron_approach_rts               ; advance to sine wave state
        inc     ent_status,x            ; reset all entity-specific counters
nitron_approach_rts:  rts               ; firing-done flag

; --- state 1+: dispatch flying vs climbing ---

nitron_state_dispatch:  lda     ent_status,x             ; check if state >= 2
        and     #$02
        beq     nitron_sine_init               ; state 1: sine wave flight
        jmp     nitron_climb_away               ; state 2: climb away

; --- state 1: sinusoidal wave flight ---

nitron_sine_init:  lda     ent_timer,x             ; if frame delay active,
        bne     nitron_apply_movement               ; skip to movement
        ldy     ent_var1,x                 ; load sine table step index
        lda     nitron_phase_table,y                 ; lookup phase entry
        asl     a                       ; *2 for 16-bit table offset
        tay
        lda     nitron_yvel_lo,y                 ; set Y speed from sine table (sub)
        sta     ent_yvel_sub,x
        lda     nitron_yvel_table,y                 ; set Y speed from sine table (whole)
        sta     ent_yvel,x              ; fire 5 bullets
        lda     nitron_xvel_lo,y                 ; set X speed from sine table (sub)
        sta     ent_xvel_sub,x
        lda     nitron_xvel_table,y                 ; set X speed from sine table (whole)
        sta     ent_xvel,x              ; set post-fire pause = 16 frames
        lda     ent_xvel,x                 ; if X speed is negative,
        bpl     nitron_negate_xvel               ; negate it (always move in facing dir)
        lda     ent_xvel_sub,x                 ; negate 16-bit X speed:
        eor     #$FF                    ; two's complement low byte
        clc
        adc     #$01
        sta     ent_xvel_sub,x
        lda     ent_xvel,x                 ; two's complement high byte
        eor     #$FF                    ; no slot: abort
        adc     #$00                    ; X = bullet index for table lookup
        sta     ent_xvel,x              ; set X speed sub from table
nitron_negate_xvel:  inc     ent_var1,x             ; advance sine step index
        lda     ent_var1,x              ; set X speed whole from table
        cmp     #$06                    ; after 6 steps: flight phase done
        bne     nitron_set_delay        ; set Y speed sub from table
        inc     ent_status,x                 ; advance to state 2 (climb away)
        lda     #$1A                    ; bomb cooldown = 26 frames
        sta     ent_var3,x
nitron_set_delay:  lda     #$0D                ; frame delay = 13 (hold each sine
        sta     ent_timer,x                 ; step for 13 frames)

; --- apply current speed each frame ---
nitron_apply_movement:  dec     ent_timer,x             ; decrement frame delay
        lda     ent_y_sub,x                 ; apply Y speed to Y position
        clc                             ; (16-bit add: sub + whole)
        adc     ent_yvel_sub,x          ; bullet damage flags (projectile)
        sta     ent_y_sub,x
        lda     ent_y_px,x              ; AI routine = $36 (bullet movement)
        adc     ent_yvel,x
        sta     ent_y_px,x              ; copy parent X position to bullet
        lda     ent_facing,x                 ; move horizontally in facing dir
        and     #$02
        bne     nitron_move_left               ; branch if facing left
        jsr     move_sprite_right                   ; unconditional branch past
        bcs     nitron_after_move
        bcc     nitron_after_move       ; set bullet HP = 1
nitron_move_left:  jsr     move_sprite_left               ; move left
nitron_after_move:  lda     ent_var3,x             ; if bomb cooldown active,
        bne     nitron_cooldown_rts               ; skip to decrement
        jsr     nitron_spawn_bomb               ; drop a bomb (spawn child)
        inc     ent_var2,x                 ; increment bomb count
        lda     ent_var2,x
        cmp     #$02                    ; after 2nd bomb: shorter cooldown
        bne     nitron_bomb_cooldown
        lda     #$10                    ; cooldown = 16 frames (after 2nd)
        bne     nitron_cooldown_set
nitron_bomb_cooldown:  lda     #$1A                ; cooldown = 26 frames (normal)
nitron_cooldown_set:  sta     ent_var3,x             ; store bomb cooldown timer
        rts

nitron_cooldown_rts:  dec     ent_var3,x             ; decrement bomb cooldown
        rts

; --- state 2: climb away (flee upward) ---

nitron_climb_away:  lda     #$00                ; Y speed = $00.03 (3.0 px/frame up)
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
        jmp     move_sprite_up                   ; fly upward off screen

; Nitron sine wave tables:
; $A56E: phase-to-index mapping (16 entries, one full sine cycle)
; $A57C: Y speed table (16-bit signed pairs for vertical oscillation)
; $A59C: X speed table (16-bit signed pairs for horizontal component)

nitron_phase_table:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$01
        .byte   $02,$03,$04,$05,$06,$07
nitron_yvel_lo:  .byte   $00
nitron_yvel_table:  .byte   $FE,$27,$FE,$96,$FE,$3D,$FF,$00
        .byte   $00,$C3,$00,$6A,$01,$D9,$01,$00 ; state 0: approach
        .byte   $02,$D9,$01,$6A,$01,$C3,$00,$00
        .byte   $00,$3D,$FF,$96,$FE,$27,$FE
nitron_xvel_lo:  .byte   $00            ; check direction
nitron_xvel_table:  .byte   $00,$C3,$00,$6A,$01,$D9,$01,$00
        .byte   $02,$D9,$01,$6A,$01,$C3,$00,$00 ; branch if moving left
        .byte   $00,$3D,$FF,$96,$FE,$27,$FE,$00 ; move right toward player
        .byte   $FE,$27,$FE,$96,$FE,$3D,$FF

; --- spawn_nitron_bomb: drop a bomb projectile below Nitron ---
nitron_spawn_bomb:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     nitron_spawn_rts               ; no slot: abort
        sty     L0000                   ; save child slot
        lda     ent_facing,x                 ; dir offset (0 or 2) for X table
        and     #$02
        tay
        lda     ent_x_px,x                 ; child X = parent X + dir offset
        clc
        adc     gyoraibo_child_x_off,y  ; check if state >= 2
        pha
        lda     ent_x_scr,x             ; state 1: sine wave flight
        adc     gyoraibo_child_x_scr,y  ; state 2: climb away
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y              ; if frame delay active,
        lda     ent_y_px,x                 ; child Y = parent Y + 16 (below)
        clc                             ; load sine table step index
        adc     #$10                    ; lookup phase entry
        sta     ent_y_px,y              ; *2 for 16-bit table offset
        lda     #$00                    ; HP = 0 (indestructible)
        sta     ent_hp,y                ; set Y speed from sine table (sub)
        lda     #$81                    ; init child with OAM $81
        jsr     init_child_entity                   ; init_child_entity
        lda     #$26                    ; AI routine = $26 (falling bomb)
        sta     ent_routine,y           ; set X speed from sine table (sub)
        lda     #$93                    ; damage flags = projectile
        sta     ent_hitbox,y            ; set X speed from sine table (whole)
nitron_spawn_rts:  rts

; --- Nitron bomb AI (routine $26): fall with gravity, explode on impact ---
        lda     ent_status,x            ; negate 16-bit X speed:
        and     #$0F                    ; two's complement low byte
        bne     nitron_status_check
        jsr     reset_gravity                   ; reset_gravity
        inc     ent_status,x
nitron_status_check:  lda     ent_status,x ; two's complement high byte
        and     #$02
        bne     nitron_animation_init
        ldy     #$12
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     nitron_climb_return
        lda     #$71                    ; after 6 steps: flight phase done
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #SFX_ATTACK             ; advance to state 2 (climb away)
        jsr     submit_sound_ID                   ; submit_sound_ID
        inc     ent_status,x
nitron_climb_return:  rts               ; frame delay = 13 (hold each sine

nitron_animation_init:  lda     ent_anim_id,x
        cmp     #$71
        bne     nitron_climb_return     ; decrement frame delay
        lda     ent_anim_state,x        ; apply Y speed to Y position
        cmp     #$04                    ; (16-bit add: sub + whole)
        bne     nitron_climb_return
        lda     #$80
        jmp     reset_sprite_anim                   ; reset_sprite_anim

; ===========================================================================
; main_unknown_27 — Debris / explosion child entity
; Spawned by breakable objects. Checks if break animation ($71) has
; completed (anim_state $04), then switches to a secondary OAM ($92).
; ===========================================================================
main_unknown_27:
        lda     ent_anim_id,x
        cmp     #$71                    ; move left
        bne     nitron_homing_jump      ; if bomb cooldown active,
        lda     ent_anim_state,x        ; skip to decrement
        cmp     #$04                    ; drop a bomb (spawn child)
        bne     nitron_done             ; increment bomb count
        lda     #$92
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$40
        sta     ent_xvel_sub,x          ; cooldown = 16 frames (after 2nd)
        sta     ent_yvel_sub,x
        lda     #$00                    ; cooldown = 26 frames (normal)
        sta     ent_xvel,x              ; store bomb cooldown timer
        sta     ent_yvel,x
        lda     #$C0
        sta     ent_hitbox,x            ; decrement bomb cooldown
        lda     #$01
        sta     ent_hp,x
nitron_done:  rts

nitron_homing_jump:  jmp     nutton_homing_entry               ; → Nutton homing AI

; ===========================================================================
; main_gyoraibo — Gyoraibo (torpedo fish, Gemini Man stage)
; Swims horizontally, opens mouth to fire upward projectile when aligned
; with player. If wall is hit, begins descending. ent_var2=wall-hit flag,
; ent_var1=fired flag, ent_timer=variant index.
; ===========================================================================
main_gyoraibo:
        lda     ent_status,x
        and     #$0F
        bne     gyoraibo_state_dispatch
        sta     ent_yvel,x
        lda     #$80
        sta     ent_yvel_sub,x
        inc     ent_status,x
gyoraibo_state_dispatch:  lda     ent_status,x
        and     #$02
        bne     gyoraibo_fire_phase
        lda     ent_facing,x
        and     #$01
        beq     gyoraibo_move_left
        ldy     #$14
        jsr     move_right_collide                   ; move_right_collide
        jmp     gyoraibo_move_check

gyoraibo_move_left:  ldy     #$15
        jsr     move_left_collide                   ; move_left_collide
gyoraibo_move_check:  bcc     gyoraibo_collision
        inc     ent_var2,x
gyoraibo_move_down:  jmp     move_sprite_down               ; move_sprite_down

gyoraibo_collision:  lda     ent_var2,x
        bne     gyoraibo_move_down
        lda     ent_var1,x
        bne     gyoraibo_rts            ; find free enemy slot
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$08                    ; save child slot
        bcs     gyoraibo_rts            ; dir offset (0 or 2) for X table
        inc     ent_status,x
        inc     ent_var1,x
        lda     #$33                    ; child X = parent X + dir offset
        jsr     reset_sprite_anim                   ; reset_sprite_anim
gyoraibo_rts:  rts

gyoraibo_fire_phase:  lda     ent_anim_id,x
        cmp     #$33
        bne     gyoraibo_rts
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     gyoraibo_rts
        lda     #$32                    ; child Y = parent Y + 16 (below)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_xvel_sub,x
        lda     #$04                    ; HP = 0 (indestructible)
        sta     ent_xvel,x
        dec     ent_status,x            ; init child with OAM $81
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     gyoraibo_no_slot        ; AI routine = $26 (falling bomb)
        sty     L0000
        lda     ent_facing,x            ; damage flags = projectile
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     gyoraibo_child_x_off,y
        pha
        lda     ent_x_scr,x
        adc     gyoraibo_child_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sec
        sbc     #$10
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y
        lda     #$34
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_routine,x
        cmp     #$28
        beq     gyoraibo_set_routine
        lda     #$45
        sta     ent_routine,y
        bne     gyoraibo_set_hitbox
gyoraibo_set_routine:  lda     #$29
        sta     ent_routine,y
gyoraibo_set_hitbox:  lda     #$C0
        sta     ent_hitbox,y
gyoraibo_no_slot:  rts

gyoraibo_child_x_off:  .byte   $00
gyoraibo_child_x_scr:  .byte   $00,$00,$00
        lda     ent_status,x
        and     #$0F
        bne     gyoraibo_status_check
        sta     ent_timer,x
        sta     ent_yvel_sub,x
        lda     #$02
        sta     ent_yvel,x
        inc     ent_status,x
gyoraibo_status_check:  lda     ent_status,x
        and     #$02
        bne     gyoraibo_timer_check
        ldy     #$17
        jsr     move_up_collide
        bcc     gyoraibo_timer_check
        inc     ent_status,x
        lda     #$71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$56
        sta     ent_routine,x
        rts

gyoraibo_timer_check:  lda     ent_timer,x
        bne     penpen_maker_rts
        lda     ent_routine,x
        cmp     #$29
        beq     gyoraibo_spawn_check
        lda     ent_y_px,x
        cmp     #$62
        bcs     penpen_maker_rts
        bcc     gyoraibo_increment_timer
gyoraibo_spawn_check:  lda     ent_y_px,x
        cmp     #$B4
        bcs     penpen_maker_rts
gyoraibo_increment_timer:  inc     ent_timer,x
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     penpen_maker_rts
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     gyoraibo_child_x_off,y
        pha
        lda     ent_x_scr,x
        adc     gyoraibo_child_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sec
        sbc     #$04
        sta     ent_y_px,y
        lda     #$00
        sta     ent_routine,y
        sta     ent_hitbox,y
        sta     ent_hp,y
        lda     #$68
        jsr     init_child_entity                   ; init_child_entity
penpen_maker_rts:  rts

; ===========================================================================
; main_penpen_maker — Penpen Maker (Gemini Man stage pipe spawner)
; Periodically spawns Penpen enemies. Changes palette colors on spawn.
; Uses a pseudo-random timer between spawns.
; ===========================================================================
main_penpen_maker:
        lda     ent_status,x            ; state 0: init
        and     #$0F
        bne     penpen_maker_spawn_loop
        ldy     #$02
penpen_maker_palette_init:  lda     penpen_maker_palette_data,y
        sta     $060D,y
        sta     $062D,y
        dey
        bpl     penpen_maker_palette_init
        sty     palette_dirty
        inc     ent_status,x
        jsr     penpen_maker_random_timer
        sta     ent_timer,x
penpen_maker_spawn_loop:  lda     ent_y_px,x
        pha
        sec
        sbc     #$10
        sta     ent_y_px,x
        lda     #$00
        sta     ent_hitbox,x
        lda     ent_anim_id,x
        pha
        jsr     process_sprites_top_spin_check
        pla
        sta     ent_anim_id,x
        pla
        sta     ent_y_px,x
        lda     ent_hp,x
        bne     penpen_maker_alive
        sta     ent_var1,x
        sta     ent_var2,x
        lda     #$63
        sta     ent_routine,x
        rts

penpen_maker_alive:  lda     #$98
        sta     ent_hitbox,x
        dec     ent_timer,x
        bne     penpen_maker_timer_rts
        jsr     penpen_maker_spawn_penpen
        jsr     penpen_maker_random_timer
        sta     ent_timer,x
penpen_maker_timer_rts:  rts

penpen_maker_random_timer:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     penpen_maker_timer_table,y
        rts

penpen_maker_timer_table:  .byte   $3C,$1E,$78,$3C
        lda     ent_var1,x
        bne     penpen_maker_dec_var1
        lda     #$0A
        sta     ent_var1,x
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     penpen_maker_spawn_rts
        lda     #SFX_PLATFORM
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     #$71
        jsr     init_child_entity                   ; init_child_entity
        lda     #$19
        sta     ent_routine,y
        lda     #$00
        sta     ent_hitbox,y
        sta     ent_timer,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        stx     L0000
        lda     ent_y_px,x
        sta     $01
        lda     ent_x_px,x
        sta     $02
        lda     ent_var2,x
        pha
        tax
        lda     $01
        clc
        adc     penpen_maker_spawn_y,x
        sta     ent_y_px,y
        lda     $02
        clc
        adc     penpen_maker_spawn_x,x
        sta     ent_x_px,y
        pla
        asl     a
        asl     a
        tax
        ldy     #$00
penpen_maker_palette_swap:  lda     penpen_maker_palette_byte,x
        sta     $060C,y
        sta     $062C,y
        inx
        iny
        cpy     #$04
        bne     penpen_maker_palette_swap
        lda     #$FF
        sta     palette_dirty
        ldx     L0000
        inc     ent_var2,x
        lda     ent_var2,x
        and     #$03
        bne     penpen_maker_spawn_rts
        lda     #$00
        sta     ent_status,x
penpen_maker_dec_var1:  dec     ent_var1,x
penpen_maker_spawn_rts:  rts

penpen_maker_palette_byte:  .byte   $0F
penpen_maker_palette_data:  .byte   $20,$10,$17,$0F,$10,$00,$17,$0F
        .byte   $00,$0F,$07,$0F,$0F,$0F,$0F,$0F
        .byte   $20,$37,$17,$0F,$10,$27,$07,$0F
        .byte   $0F,$17,$0F,$0F,$0F,$0F,$0F
penpen_maker_spawn_y:  .byte   $F0,$10,$10,$D0,$F0,$10,$10,$D0
penpen_maker_spawn_x:  .byte   $F0,$20,$E8,$10,$F0,$20,$E8,$10
penpen_maker_spawn_penpen:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     penpen_maker_spawn_done
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     penpen_maker_penpen_x_adj,y
        pha
        lda     ent_x_scr,x
        adc     penpen_maker_penpen_x_scr,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        clc
        adc     #$1C
        sta     ent_y_px,y
        lda     #$80
        sta     ent_xvel_sub,y
        lda     #$02
        sta     ent_xvel,y
        lda     #$93
        jsr     init_child_entity                   ; init_child_entity
        lda     #$46
        sta     ent_routine,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
penpen_maker_spawn_done:  rts

penpen_maker_penpen_x_adj:  .byte   $F8
penpen_maker_penpen_x_scr:  .byte   $FF,$F8,$FF
        lda     ent_facing,x
        and     #$01
        beq     penpen_maker_check_facing
        lda     #$08
        jsr     move_right_collide                   ; move_right_collide
        jmp     penpen_maker_after_move

penpen_maker_check_facing:  ldy     #$09
        jsr     move_left_collide                   ; move_left_collide
penpen_maker_after_move:  bcc     penpen_maker_move_rts
        lda     #$71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_routine,x
penpen_maker_move_rts:  rts

        lda     ent_anim_frame,x
        cmp     #$02
        bne     penpen_maker_move_rts
        lda     ent_timer,x
        cmp     #$08
        beq     penpen_maker_move_rts
        stx     L0000
        ldy     ent_x_px,x
        sty     $02
        ldy     ent_x_scr,x
        sty     $03
        ldy     ent_y_px,x
        sty     $04
        ldy     ent_anim_id,x
        sty     $10
        lda     ent_routine,x
        sta     $11
        lda     ent_timer,x
        tax
penpen_maker_spawn_parts:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     penpen_maker_parts_rts
        lda     $10
        jsr     init_child_entity                   ; init_child_entity
        lda     $11
        cmp     #$56
        beq     penpen_maker_part_type_a
        cmp     #$48
        beq     penpen_maker_part_type_a
        cmp     #$39
        beq     penpen_maker_part_type_b
        cmp     #$48
        beq     penpen_maker_part_type_b
        lda     #$00
        sta     ent_hitbox,y
        sta     ent_routine,y
        beq     penpen_maker_part_pos
penpen_maker_part_type_a:  lda     #$80
        sta     ent_hitbox,y
        lda     #$54
        sta     ent_routine,y
        bne     penpen_maker_part_pos
penpen_maker_part_type_b:  lda     #$80
        sta     ent_hitbox,y
        lda     #$55
        sta     ent_routine,y
penpen_maker_part_pos:  lda     #$00
        sta     $01
        lda     penpen_maker_part_x_off,x
        bpl     penpen_maker_part_offset
        dec     $01
penpen_maker_part_offset:  clc
        adc     $02
        sta     ent_x_px,y
        lda     $03
        adc     $01
        sta     ent_x_scr,y
        lda     $04
        clc
        adc     penpen_maker_part_y_off,x
        sta     ent_y_px,y
        ldx     L0000
        inc     ent_timer,x
        lda     ent_timer,x
        tax
        and     #$01
        bne     penpen_maker_spawn_parts
penpen_maker_parts_rts:  ldx     L0000
        rts

penpen_maker_part_x_off:  .byte   $0C,$F4,$F0,$10,$F4,$0C,$00,$00
penpen_maker_part_y_off:  .byte   $F4,$0C,$00,$00,$F4,$0C,$F0,$10

; ===========================================================================
; main_bomber_pepe — Bomber Pepe (penguin bomber, Gemini Man stage)
; Flies horizontally dropping bombs, with gravity and floor collision.
; Uses pseudo-random timer between bomb drops.
; ===========================================================================
main_bomber_pepe:
        lda     ent_status,x
        and     #$0F
        bne     bomber_pepe_bomb_timer
        lda     #$44
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
        lda     #$1E
        sta     ent_timer,x
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        jsr     penpen_maker_random_timer
        sta     ent_var1,x
        inc     ent_status,x
bomber_pepe_bomb_timer:  dec     ent_var1,x
        bne     bomber_pepe_move_phase
        jsr     bomber_pepe_spawn_bomb
        jsr     penpen_maker_random_timer
        sta     ent_var1,x
bomber_pepe_move_phase:  lda     ent_status,x
        and     #$02
        bne     bomber_pepe_bounce
        lda     ent_facing,x
        and     #$01
        beq     bomber_pepe_move_left
        ldy     #$0A
        jsr     move_right_collide                   ; move_right_collide
        jmp     bomber_pepe_gravity

bomber_pepe_move_left:  ldy     #$0B
        jsr     move_left_collide                   ; move_left_collide
bomber_pepe_gravity:  ldy     #$20
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     bomber_pepe_no_floor
        lda     #$44
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
        inc     ent_status,x
        lda     #$3D
        bne     bomber_pepe_set_anim
bomber_pepe_no_floor:  lda     #$3C
bomber_pepe_set_anim:  jmp     reset_sprite_anim               ; reset_sprite_anim

bomber_pepe_bounce:  lda     ent_anim_id,x
        cmp     #$3B
        beq     bomber_pepe_bounce_timer
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     bomber_pepe_rts
        lda     #$3B
        jsr     reset_sprite_anim                   ; reset_sprite_anim
bomber_pepe_bounce_timer:  dec     ent_timer,x
        bne     bomber_pepe_rts
        jsr     face_player                   ; face_player
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        dec     ent_status,x
        lda     #$3C
        sta     ent_timer,x
bomber_pepe_rts:  rts

bomber_pepe_spawn_bomb:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     bomber_pepe_spawn_rts
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     bomber_pepe_bomb_x_off,y
        pha
        lda     ent_x_scr,x
        adc     bomber_pepe_bomb_x_facing,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$01
        sta     ent_hp,y
        lda     #$3E
        jsr     init_child_entity                   ; init_child_entity
        lda     #$1D
        sta     ent_routine,y
        lda     #$C0
        sta     ent_hitbox,y
bomber_pepe_spawn_rts:  rts

bomber_pepe_bomb_x_off:  .byte   $08
bomber_pepe_bomb_x_facing:  .byte   $00,$F8,$FF
        lda     ent_status,x
        and     #$0F
        bne     bomber_pepe_bomb_fall_init
        sta     ent_timer,x
        lda     #$1E
        sta     ent_var1,x
        inc     ent_status,x
bomber_pepe_bomb_fall_init:  lda     ent_status,x
        and     #$02
        bne     bomber_pepe_bounce_countdown
        lda     ent_facing,x
        and     #$01
        beq     bomber_pepe_bomb_move_left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     bomber_pepe_bomb_gravity

bomber_pepe_bomb_move_left:  jsr     move_sprite_left               ; move_sprite_left
bomber_pepe_bomb_gravity:  ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     bomber_pepe_bomb_floor_hit
        lda     ent_timer,x
        tay
        lda     bomber_pepe_bomb_yvel_sub,y
        sta     ent_yvel_sub,x
        lda     bomber_pepe_bomb_yvel,y
        sta     ent_yvel,x
        lda     bomber_pepe_bomb_xvel_sub,y
        sta     ent_xvel_sub,x
        lda     bomber_pepe_bomb_xvel,y
        sta     ent_xvel,x
        inc     ent_timer,x
        lda     ent_timer,x
        cmp     #$03
        bcc     bomber_pepe_bomb_floor_hit
        inc     ent_status,x
bomber_pepe_bomb_floor_hit:  rts

bomber_pepe_bounce_countdown:  dec     ent_var1,x
        bne     bomber_pepe_bounce_countdown
        lda     #$71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$39
        sta     ent_routine,x
        rts

bomber_pepe_bomb_yvel_sub:  .byte   $9E,$44,$4F
bomber_pepe_bomb_yvel:  .byte   $04,$03,$02
bomber_pepe_bomb_xvel_sub:  .byte   $00,$00,$80
bomber_pepe_bomb_xvel:  .byte   $01,$01,$00

; ===========================================================================
; main_bolton_and_nutton — Two-part bolt enemy (Hard Man stage)
; Bolton (nut body, OAM $2E) sits on the wall and launches the Nutton
; (bolt projectile, OAM $2F) when player is within range. Nutton homes
; toward the player's position once launched. Code at $1DAC82 (unlabeled,
; AI routine $31) handles Nutton's return flight back to Bolton.
; Entity memory (Nutton child):
;   ent_timer/ent_var2 = target X position (Bolton's X - 8)
;   ent_var1 = parent Bolton slot index
;   ent_var3 = sound-played flag
; ===========================================================================
main_bolton_and_nutton:
        lda     ent_anim_id,x                 ; check OAM sprite ID
        cmp     #$2F                    ; is this the Nutton (bolt)?
        bne     bolton_idle_logic               ; no -> Bolton (nut body) logic
        jmp     nutton_homing_entry               ; yes -> Nutton homing toward player

; -- Bolton (nut body) logic --

bolton_idle_logic:  lda     ent_status,x             ; check state
        and     #$0F                    ; state 0 = idle, waiting
        bne     bolton_state_check               ; nonzero = bolt already launched
        jsr     bolton_check_range               ; check player range + find free slot
        bcs     bolton_return               ; carry set = can't fire, return
        lda     ent_flags,x                 ; clear bit 2 (open-mouth indicator)
        and     #$FB
        sta     ent_flags,x
        inc     ent_status,x                 ; advance to state 1 (bolt launched)
        lda     #$66                    ; Y speed = $00.66/frame
        sta     ent_yvel_sub,x                 ; (slow upward drift for Bolton body)
        lda     #$00
        sta     ent_yvel,x
        lda     #$2E                    ; spawn child entity type $2E
        jsr     init_child_entity                   ; (Nutton bolt projectile)
        lda     #$31                    ; child AI routine = $31
        sta     ent_routine,y                 ; (Nutton return-flight handler)
        lda     ent_y_px,x                 ; copy Bolton Y to child
        sta     ent_y_px,y
        lda     camera_x_lo                     ; child X = camera left edge + 4
        clc                             ; (spawn at left side of screen)
        adc     #$04
        sta     ent_x_px,y
        lda     camera_screen                     ; child X screen
        adc     #$00
        sta     ent_x_scr,y
        lda     #$00                    ; child X speed = $04.00 px/frame
        sta     ent_xvel_sub,y                 ; (flies rightward toward Bolton)
        lda     #$04
        sta     ent_xvel,y
        lda     ent_x_px,x                 ; child target X = Bolton X - 8
        sec                             ; (stored in child ent_timer/ent_var2)
        sbc     #$08
        sta     ent_timer,y
        lda     ent_x_scr,x                 ; target X screen
        sbc     #$00
        sta     ent_var2,y
        txa                             ; store parent Bolton slot in child
        sta     ent_var1,y
        lda     #$00                    ; child damage = $00 (harmless during return)
        sta     ent_hitbox,y
bolton_state_check:  lda     ent_flags,x
        and     #$FB
        sta     ent_flags,x
        lda     ent_timer,x
        bne     bolton_return
        lda     #$00
        sta     ent_anim_frame,x
        lda     frame_counter
        and     #$04
        beq     bolton_return
        lda     ent_flags,x
        ora     #$04
        sta     ent_flags,x
bolton_return:  rts

nutton_homing_entry:  lda     ent_x_px
        sec
        sbc     ent_x_px,x
        pha
        lda     ent_x_scr
        sbc     ent_x_scr,x
        pla
        beq     nutton_move_y
        bcc     nutton_move_left
        jsr     move_sprite_right                   ; move_sprite_right
        lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
        jmp     nutton_move_y

nutton_move_left:  jsr     move_sprite_left               ; move_sprite_left
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
nutton_move_y:  lda     ent_y_px
        sec
        sbc     ent_y_px,x
        beq     bolton_return
        bcs     nutton_move_down
        jmp     move_sprite_up                   ; move_sprite_up

nutton_move_down:  jmp     move_sprite_down               ; move_sprite_down

bolton_check_range:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$44
        bcs     bolton_range_fail
        lda     ent_x_px,x
        sec
        sbc     camera_x_lo
        sta     L0000
        lda     ent_x_scr,x
        sbc     camera_screen
        bcc     bolton_too_many
        lda     L0000
        cmp     #$80
        bcc     bolton_too_many
        lda     #$00
        sta     $01
        stx     L0000
        ldy     #$1F
bolton_count_nuttons:  cpy     L0000
        beq     bolton_count_next
        lda     ent_status,y
        bpl     bolton_count_next
        lda     ent_flags,y
        bpl     bolton_count_next
        lda     ent_routine,y
        cmp     #$30
        bne     bolton_count_next
        inc     $01
bolton_count_next:  dey
        cpy     #$0F
        bne     bolton_count_nuttons
        lda     $01
        cmp     #$03
        beq     bolton_too_many
        ldy     L0000
bolton_find_slot:  lda     ent_status,y
        bpl     bolton_range_ok
        dey                             ; check OAM sprite ID
        cpy     #$0F                    ; is this the Nutton (bolt)?
        bne     bolton_find_slot        ; no -> Bolton (nut body) logic
bolton_too_many:  lda     #$00          ; yes -> Nutton homing toward player
        sta     ent_status,x
bolton_range_fail:  sec
        rts

bolton_range_ok:  clc                   ; state 0 = idle, waiting
        rts                             ; nonzero = bolt already launched

        lda     ent_status,x            ; carry set = can't fire, return
        and     #$0F                    ; clear bit 2 (open-mouth indicator)
        bne     nutton_return_state
        sta     ent_var3,x
        jsr     move_sprite_right                   ; move_sprite_right
        lda     ent_timer,x             ; Y speed = $00.66/frame
        sec                             ; (slow upward drift for Bolton body)
        sbc     ent_x_px,x
        lda     ent_var2,x
        sbc     ent_x_scr,x             ; spawn child entity type $2E
        bcs     nutton_reached_target   ; (Nutton bolt projectile)
        lda     ent_timer,x             ; child AI routine = $31
        sta     ent_x_px,x              ; (Nutton return-flight handler)
        lda     ent_var2,x              ; copy Bolton Y to child
        sta     ent_x_scr,x
        inc     ent_status,x            ; child X = camera left edge + 4
        ldy     ent_var1,x              ; (spawn at left side of screen)
        lda     #$08
        sta     ent_timer,y
        sta     ent_timer,x             ; child X screen
nutton_reached_target:  lda     #$00
        sta     ent_anim_frame,x
        rts                             ; child X speed = $04.00 px/frame

nutton_return_state:  lda     ent_var3,x
        bne     nutton_sound_and_fly
        lda     #SFX_TURRET_FIRE        ; child target X = Bolton X - 8
        jsr     submit_sound_ID                   ; submit_sound_ID
        inc     ent_var3,x
nutton_sound_and_fly:  lda     #$01
        sta     $95                     ; target X screen
        lda     ent_timer,x
        beq     nutton_dock_ready
        lda     ent_anim_frame,x        ; store parent Bolton slot in child
        bne     nutton_return_end
        dec     ent_timer,x             ; child damage = $00 (harmless during return)
        inc     ent_x_px,x
        rts

nutton_dock_ready:  lda     ent_anim_state,x
        bne     nutton_return_end
        sta     ent_anim_frame,x
        ldy     ent_var1,x
        lda     ent_anim_state,y
        bne     nutton_return_end
        sta     ent_anim_frame,y
        sta     ent_timer,y
        sta     ent_status,x
        lda     #$2F
        sta     ent_anim_id,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
nutton_return_end:  rts

; ===========================================================================
; main_have_su_bee — Have "Su" Bee (bee carrier, Snake Man stage)
; Flies toward player, hovers while carrying a bee, then releases it.
; After release, reverses direction and flies away. ent_timer=pre-launch
; delay, ent_var2=hover timer, ent_var1=release range check flag.
; ===========================================================================
main_have_su_bee:

        lda     ent_status,x
        and     #$0F
        bne     have_su_bee_active
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$28
        bcs     have_su_bee_init_rts
        inc     ent_status,x
        lda     #$1E
        sta     ent_timer,x
        lda     #$30
        sta     ent_var2,x
        lda     ent_flags,x
        eor     #$04
        sta     ent_flags,x
        lda     ent_facing,x
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     have_su_bee_x_off,y
        pha
        lda     ent_x_scr,x
        adc     have_su_bee_x_scr,y
        sta     ent_x_scr,x
        pla
        sta     ent_x_px,x
        jsr     face_player                   ; face_player
have_su_bee_init_rts:  rts

have_su_bee_active:  lda     ent_status,x
        and     #$02
        bne     have_su_bee_hover
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        lda     ent_facing,x
        and     #$01
        beq     have_su_bee_move_left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     have_su_bee_dist_check

have_su_bee_move_left:  jsr     move_sprite_left               ; move_sprite_left
have_su_bee_dist_check:  lda     ent_var1,x
        bne     have_su_bee_move_rts
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$50
        bcc     have_su_bee_move_rts
        inc     ent_status,x
        inc     ent_var1,x
        lda     #$80
        sta     ent_yvel_sub,x
        lda     #$00
        sta     ent_yvel,x
        lda     ent_facing,x
        and     #$01
        beq     have_su_bee_flip_left
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        rts

have_su_bee_flip_left:  lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
have_su_bee_move_rts:  rts

have_su_bee_hover:  dec     ent_var2,x
        bne     have_su_bee_vert_move
        dec     ent_status,x
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$3A
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        jmp     have_su_bee_spawn_bee

have_su_bee_vert_move:  lda     ent_facing,x
        and     #$01
        beq     have_su_bee_move_down
        jsr     move_sprite_up                   ; move_sprite_up
        jmp     have_su_bee_hover_timer

have_su_bee_move_down:  jsr     move_sprite_down               ; move_sprite_down
have_su_bee_hover_timer:  dec     ent_timer,x
        bne     have_su_bee_move_rts
        lda     #$3C
        sta     ent_timer,x
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        rts

have_su_bee_x_off:  .byte   $50
have_su_bee_x_scr:  .byte   $00,$B0,$FF
have_su_bee_spawn_bee:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     have_su_bee_spawn_rts
        sty     $01
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     gyoraibo_child_x_off,y
        pha
        lda     ent_x_scr,x
        adc     gyoraibo_child_x_scr,y
        ldy     $01
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        clc
        adc     #$18
        sta     ent_y_px,y
        lda     #$35
        jsr     init_child_entity                   ; init_child_entity
        lda     #$2F
        sta     ent_routine,y
        lda     #$CF
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
        lda     #$50
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
have_su_bee_spawn_rts:  rts

; ===========================================================================
; main_beehive — Beehive (Snake Man stage)
; Falls until hitting floor, then explodes and spawns 5 Chibee enemies
; at offset positions around the hive. Changes AI routine to $3A (dead).
; ===========================================================================
main_beehive:
        ldy     #$08
        jsr     move_down_collide                   ; move_down_collide
        bcc     have_su_bee_spawn_rts
        lda     #$71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$3A
        sta     ent_routine,x
        lda     #$00
        sta     $01
beehive_find_freeslot:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     beehive_done
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        lda     $01
        asl     a
        tay
        lda     ent_x_px,x
        clc
        adc     bee_spawn_x_offset,y
        pha
        lda     ent_x_scr,x
        adc     beehive_spawn_offsets,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        ldy     $01
        lda     ent_y_px,x
        clc
        adc     bee_spawn_y_offset,y
        ldy     L0000
        sta     ent_y_px,y
        lda     #$41
        jsr     init_child_entity                   ; init_child_entity
        lda     #$00
        sta     ent_xvel_sub,y
        sta     ent_xvel,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
        lda     #$3B
        sta     ent_routine,y
        lda     #$08
        sta     ent_timer,y
        sta     ent_var1,y
        lda     #$10
        sta     ent_var2,y
        lda     #$33
        sta     ent_yvel_sub,y
        lda     #$01
        sta     ent_yvel,y
        inc     $01
        lda     $01
        cmp     #$05
        bcc     beehive_find_freeslot
beehive_done:  rts

; spawn offsets indexed by bee #
; screen and pixel (not sub)

bee_spawn_x_offset:  .byte   $E8
beehive_spawn_offsets:  .byte   $FF,$E8,$FF,$01,$00,$18,$00,$18
        .byte   $00

; pixel
bee_spawn_y_offset:  .byte   $E8,$18,$01,$E8,$18

; ===========================================================================
; main_returning_monking — Returning Monking (monkey enemy, Snake Man stage)
; Jumps between platforms, pauses on landing, then leaps again toward player.
; States: 0=init, 1=jumping/patrolling, 2=grounded/attacking, 3=retreating
; ===========================================================================
main_returning_monking:
        lda     ent_status,x
        and     #$0F
        bne     monking_state_router
        inc     ent_status,x
        lda     #$3C
        sta     ent_yvel_sub,x
        lda     #$09
        sta     ent_yvel,x
        lda     #$1E
        sta     ent_timer,x
        jsr     set_sprite_hflip                   ; set_sprite_hflip
monking_state_router:  lda     ent_status,x
        and     #$0F
        cmp     #$02
        beq     monking_grounded
        cmp     #$03
        bne     monking_jump_state
        jmp     monking_retreat

monking_jump_state:  lda     ent_timer,x
        bne     monking_timer_dec
        lda     ent_anim_id,x
        cmp     #$44
        beq     monking_land_setup
        lda     ent_var1,x
        bne     monking_land_setup
        lda     #$45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        ldy     #$15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     $10
        and     #$10
        beq     monking_jump_rts
        lda     #$44
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        rts

monking_timer_dec:  dec     ent_timer,x
monking_jump_rts:  rts

monking_land_setup:  lda     ent_anim_id,x
        cmp     #$45
        beq     monking_apply_gravity
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$28
        bcs     monking_jump_rts
        lda     #$45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_var1,x
monking_apply_gravity:  ldy     #$15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     monking_jump_rts
        lda     #$43
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_status,x
monking_grounded:  lda     ent_var2,x
        bne     monking_walk_dir
        lda     ent_hp,x
        cmp     #$04
        bne     monking_walk_dir
        inc     ent_status,x
        lda     #$3C
        sta     ent_yvel_sub,x
        lda     #$09
        sta     ent_yvel,x
        inc     ent_var2,x
        rts

monking_walk_dir:  lda     ent_facing,x
        and     #$01
        beq     monking_move_left
        ldy     #$16
        jsr     move_right_collide                   ; move_right_collide
        jmp     monking_gravity_check

monking_move_left:  ldy     #$17
        jsr     move_left_collide                   ; move_left_collide
monking_gravity_check:  lda     ent_anim_id,x
        cmp     #$43
        beq     monking_attack_anim
        ldy     #$15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     monking_attack_rts
        lda     #$BB
        sta     ent_yvel_sub,x
        lda     #$06
        sta     ent_yvel,x
        lda     #$43
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$01
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        jsr     face_player                   ; face_player
monking_attack_rts:  rts

monking_attack_anim:  lda     ent_anim_state,x
        bne     monking_attack_finish
monking_attack_finish:  lda     #$46
        jmp     reset_sprite_anim                   ; reset_sprite_anim

monking_retreat:  lda     ent_anim_id,x
        cmp     #$44
        beq     monking_retreat_timer
        lda     #$45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        ldy     #$15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     $10
        and     #$10
        beq     monking_attack_rts
        lda     #$44
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$5A
        sta     ent_var3,x
monking_retreat_timer:  dec     ent_var3,x
        bne     monking_retreat_rts
        dec     ent_status,x
        jsr     reset_gravity                   ; reset_gravity
        lda     #$45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
monking_retreat_rts:  rts

; ===========================================================================
; main_wanaan — Wanaan (pipe snake, Snake Man stage)
; Hides in pipe, snaps out to bite when player is within $18 px both X and Y.
; Snap sequence: presnap delay → upward snap (6 frames) → sound → downward
; snap (16 frames) → retract to original position and re-hide.
; ===========================================================================
main_wanaan:
        lda     ent_status,x                 ; test state:
        and     #$0F                    ; any of these bits
        bne     wanaan_active                   ; means at least presnap
        sta     ent_yvel_sub,x
        lda     #$02                    ; if not, $0200
        sta     ent_yvel,x                 ; -> Y speed
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$18
        bcs     wanaan_idle_rts                   ; if player is within $18
        jsr     entity_x_dist_to_player                   ; pixel distance both X & Y
        cmp     #$18                    ; (if not return)
        bcs     wanaan_idle_rts
        inc     ent_status,x                 ; start snapping!
        lda     #$21                    ; $21 frames of delay timer
        sta     ent_timer,x                 ; for presnap
        lda     #$06                    ; 6 frames to snap upward
        sta     ent_var1,x                 ; snapping timer
        lda     ent_y_px,x                 ; preserve original Y position
        sta     ent_var2,x                 ; pre-snap
        lda     #$10                    ; 16 frames downward snap
        sta     ent_var3,x
wanaan_idle_rts:  rts

wanaan_active:  lda     ent_flags,x                 ; this sprite flag
        and     #$04                    ; indicates past presnap
        beq     wanaan_snap_phase
        dec     ent_timer,x                 ; presnap timer
        bne     wanaan_idle_rts                   ; not expired yet? return
        lda     ent_flags,x                 ; on expiration,
        eor     #$04                    ; turn $04 sprite flag on
        sta     ent_flags,x
        lda     #$A3                    ; set shape $A3 (extended hitbox)
        sta     ent_hitbox,x
wanaan_snap_phase:  lda     ent_status,x                 ; this bitflag on
        and     #$02                    ; means past snapping
        bne     wanaan_downsnap
        lda     #$00
        sta     ent_anim_frame,x                 ; show open mouth frame
        sta     ent_anim_state,x
        dec     ent_y_px,x
        dec     ent_y_px,x                 ; move up 3 pixels
        dec     ent_y_px,x
        dec     ent_var1,x                 ; upward snap timer
        bne     wanaan_idle_rts                   ; not expired yet? return
        inc     ent_status,x
        lda     #SFX_CLAMP                    ; on expiration,
        jsr     submit_sound_ID                   ; $02 -> state
        rts

wanaan_downsnap:  lda     #$01
        sta     ent_anim_state,x                 ; show closed mouth frame
        lda     #$00
        sta     ent_anim_frame,x
        jsr     move_sprite_down                   ; move_sprite_down
        dec     ent_var3,x                 ; downward snap timer
        bne     wanaan_idle_rts                   ; not expired yet? return
        lda     #$00                    ; on expiration,
        sta     ent_anim_frame,x                 ; reset animation frame
        sta     ent_anim_state,x
        lda     ent_var2,x                 ; restore original presnap
        sta     ent_y_px,x                 ; Y position
        lda     ent_flags,x
        ora     #$94                    ; flags: active+bit4+disabled ($94)
        sta     ent_flags,x
        lda     #$80                    ; reset entity to active/idle
        sta     ent_status,x
        lda     #$83                    ; reset shape
        sta     ent_hitbox,x
        rts

; ===========================================================================
; main_komasaburo — Komasaburo (spinning top enemy, Top Man stage)
; Spins in place, periodically fires child projectiles (OAM $E2).
; After 3 firings, enters a different behavioral pattern.
; ent_timer=fire delay, ent_var1=anim timer, ent_var2=fire count.
; ===========================================================================
main_komasaburo:
        lda     ent_status,x
        and     #$0F
        bne     komasaburo_state_check
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        inc     ent_status,x
        lda     #$36
        sta     ent_timer,x
        lda     #$10
        sta     ent_var1,x
komasaburo_state_check:  lda     ent_status,x
        and     #$02
        bne     komasaburo_fire_phase
        lda     ent_anim_id,x
        cmp     #$D6
        beq     komasaburo_anim_reset
        lda     ent_var2,x
        cmp     #$03
        bcs     komasaburo_count_kids
        dec     ent_timer,x
        bne     komasaburo_anim_idle
        lda     #$D6
        jsr     reset_sprite_anim                   ; reset_sprite_anim
komasaburo_anim_reset:  lda     #$00
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        jsr     komasaburo_spawn_proj
        inc     ent_var2,x
        lda     #$36
        sta     ent_timer,x
        inc     ent_status,x
komasaburo_fire_phase:  lda     #$00
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        dec     ent_var1,x
        bne     komasaburo_fire_rts
        lda     #$10
        sta     ent_var1,x
        dec     ent_status,x
komasaburo_anim_idle:  lda     ent_anim_id,x
        cmp     #$C6
        beq     komasaburo_fire_rts
        lda     #$C6
        jsr     reset_sprite_anim                   ; reset_sprite_anim
komasaburo_fire_rts:  rts

komasaburo_count_kids:  lda     #$00
        sta     L0000
        lda     #$E2
        sta     $01
        ldy     #$1F
komasaburo_scan_start:  lda     ent_status,y
        bmi     komasaburo_check_child
komasaburo_scan_next:  dey              ; test state:
        cpy     #$0F                    ; any of these bits
        bne     komasaburo_scan_start   ; means at least presnap
        lda     L0000
        cmp     #$03                    ; if not, $0200
        beq     komasaburo_scan_done    ; -> Y speed
        dec     ent_var2,x
komasaburo_scan_done:  lda     #$38
        sta     ent_timer,x             ; if player is within $18
        rts                             ; pixel distance both X & Y

komasaburo_check_child:  lda     $01
        cmp     ent_anim_id,y           ; start snapping!
        bne     komasaburo_scan_next    ; $21 frames of delay timer
        inc     L0000                   ; for presnap
        jmp     komasaburo_scan_next    ; 6 frames to snap upward

komasaburo_spawn_proj:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     komasaburo_spawn_fail   ; pre-snap
        lda     ent_facing,x            ; 16 frames downward snap
        sta     ent_facing,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; this sprite flag
        sta     ent_x_scr,y             ; indicates past presnap
        lda     ent_y_px,x
        clc                             ; presnap timer
        adc     #$04                    ; not expired yet? return
        sta     ent_y_px,y              ; on expiration,
        lda     #$E2                    ; turn $04 sprite flag on
        jsr     init_child_entity                   ; init_child_entity
        lda     #$98                    ; set shape $A3 (extended hitbox)
        sta     ent_flags,y
        lda     #$C0                    ; this bitflag on
        sta     ent_hitbox,y            ; means past snapping
        lda     #$42
        sta     ent_routine,y
        lda     #$01                    ; show open mouth frame
        sta     ent_hp,y
komasaburo_spawn_fail:  rts

        lda     ent_status,x
        and     #$0F                    ; upward snap timer
        bne     komasaburo_child_fall   ; not expired yet? return
        sta     ent_var1,x
        sta     ent_xvel_sub,x          ; on expiration,
        lda     #$02                    ; $02 -> state
        sta     ent_xvel,x
        inc     ent_status,x
        lda     #$F0
        sta     ent_timer,x             ; show closed mouth frame
        jsr     reset_gravity                   ; reset_gravity
komasaburo_child_fall:  ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     komasaburo_child_timer  ; downward snap timer
        lda     ent_facing,x            ; not expired yet? return
        and     #$01                    ; on expiration,
        beq     komasaburo_child_left   ; reset animation frame
        ldy     #$08
        jsr     move_right_collide                   ; move_right_collide
        jmp     komasaburo_child_move   ; Y position

komasaburo_child_left:  ldy     #$09    ; flags: active+bit4+disabled ($94)
        jsr     move_left_collide                   ; move_left_collide
komasaburo_child_move:  bcc     komasaburo_child_timer ; reset entity to active/idle
        lda     ent_facing,x
        eor     #$03                    ; reset shape
        sta     ent_facing,x
komasaburo_child_timer:  dec     ent_timer,x
        bne     komasaburo_child_air
        lda     #$71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_routine,x
komasaburo_child_idle:  rts

komasaburo_child_air:  lda     ent_var1,x
        bne     komasaburo_child_idle
        lda     ent_timer,x
        cmp     #$B4
        bne     komasaburo_child_idle
        lda     #$F0
        sta     ent_timer,x
        inc     ent_var1,x
        rts

; ===========================================================================
; main_mechakkero — Mechakkero (frog robot, Shadow Man stage)
; Hops toward player with gravity. On floor hit, sets random jump height
; and waits on a timer before hopping again. State 0=airborne, 1=grounded.
; ===========================================================================
main_mechakkero:

        lda     ent_status,x                 ; state nonzero
        and     #$0F                    ; means on ground
        bne     mechakkero_in_air_state
        lda     ent_facing,x
        and     #$01
        beq     mechakkero_move_left
        ldy     #$18
        jsr     move_right_collide                   ; move_right_collide
        jmp     mechakkero_apply_gravity

mechakkero_move_left:  ldy     #$19
        jsr     move_left_collide                   ; move_left_collide
mechakkero_apply_gravity:  ldy     #$18
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     mechakkero_landing_setup
        jsr     mechakkero_random_jump_vel
        inc     ent_status,x
        lda     #$CB
        sta     ent_hitbox,x
        lda     #$2B
        bne     mechakkero_reset_anim_exit
mechakkero_landing_setup:  lda     #$C3
        sta     ent_hitbox,x
        lda     #$2A
mechakkero_reset_anim_exit:  jmp     reset_sprite_anim               ; reset_sprite_anim

mechakkero_in_air_state:  lda     ent_anim_id,x
        cmp     #$2B
        bne     mechakkero_anim_check_alt
        lda     ent_anim_state,x
        cmp     #$02
        bne     mechakkero_air_done
        lda     #$29
        bne     mechakkero_reset_anim_air
mechakkero_anim_check_alt:  lda     #$29
mechakkero_reset_anim_air:  jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_timer,x                 ; timer not expired yet?
        bne     mechakkero_timer_dec
        lda     #$3C                    ; on timer expiration,
        sta     ent_timer,x                 ; set hopping state
        dec     ent_status,x                 ; with timer 60 frames
        jsr     face_player                   ; and hop toward player
mechakkero_air_done:  rts

mechakkero_timer_dec:  dec     ent_timer,x
        rts

mechakkero_random_jump_vel:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$01
        tay
        lda     mechakkero_jump_vel_sub,y
        sta     ent_yvel_sub,x
        lda     mechakkero_jump_vel,y
        sta     ent_yvel,x
        rts

mechakkero_jump_vel_sub:  .byte   $52,$A8
mechakkero_jump_vel:  .byte   $04,$05

; ===========================================================================
; main_top_man_platform — Top Man stage moving platform (conveyor belt)
; Moves up or down, carries the player when standing on it. Reverses
; direction on a timer. Checks player collision to apply conveyor push.
; ===========================================================================
main_top_man_platform:
        lda     ent_status,x
        and     #$0F
        bne     top_man_platform_check_proximity
        sta     ent_yvel_sub,x
        lda     #$01
        sta     ent_yvel,x
        inc     ent_status,x
        lda     ent_y_px,x
        cmp     #$88
        bcs     top_man_platform_set_timer
        inc     ent_var1,x
top_man_platform_set_timer:  lda     #$10
        sta     ent_timer,x
        lda     #$01
        sta     ent_facing,x
        rts

top_man_platform_check_proximity:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$16
        bcs     top_man_platform_movement_check
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$15
        bcs     top_man_platform_movement_check
        lda     ent_facing,x
        and     #$02
        bne     top_man_platform_facing_right
        lda     #$01
        bne     top_man_platform_move
top_man_platform_facing_right:  lda     #$02
top_man_platform_move:  sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
        dec     ent_timer,x
        bne     top_man_platform_movement_check
        lda     #$10
        sta     ent_timer,x
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
top_man_platform_movement_check:  lda     ent_var1,x
        bne     top_man_platform_move_down_check
        jsr     move_sprite_up                   ; move_sprite_up
        lda     ent_y_scr,x
        beq     top_man_platform_up_rts
        lda     #$00
        sta     ent_y_scr,x
top_man_platform_up_rts:  rts

top_man_platform_move_down_check:  lda     ent_y_px,x
        pha
        dec     ent_y_px,x
        jsr     check_player_collision                   ; check_player_collision
        pla
        sta     ent_y_px,x
        bcs     top_man_platform_down_movement
        lda     ent_y_px
        sec
        sbc     ent_y_px,x
        bcs     top_man_platform_render_update
        lda     ent_y_px
        adc     #$02                    ; state nonzero
        sta     ent_y_px                ; means on ground
        cmp     #$F0
        bcc     top_man_platform_render_update
        adc     #$0F
        sta     ent_y_px
        inc     ent_y_scr
top_man_platform_render_update:  stx     $0F
        ldx     #$00
        ldy     #$00
        jsr     LE8D6
        lda     $10
        and     #$10
        beq     top_man_platform_render_check
        jsr     LEE13
top_man_platform_render_check:  ldx     $0F
top_man_platform_down_movement:  jsr     move_sprite_down               ; move_sprite_down
        lda     ent_y_scr,x
        beq     top_man_platform_up_rts
        lda     #$00
        sta     ent_y_scr,x
        rts

; =============================================================================
; main_needle_press -- Needle Man stage ceiling crusher
; =============================================================================
; Animation-driven press that hangs from ceiling. Uses ent_flags bit 2 as
; "retracted" flag. When retracted, waits for player within $61 pixels
; horizontally before descending. Animation frames drive the crush cycle:
;   frame 0: fully retracted (invincible, $80). Sets retracted flag + delay.
;   frame 2: midway down (hittable+hurts, $AE). Sets delay timer.
;   other frames: actively crushing (hittable+hurts, $A4).
; ent_timer = delay timer ($1E = 30 frames pause at key positions)
; =============================================================================
main_needle_press:

        lda     ent_timer,x                 ; delay timer
        beq     needle_press_check_state               ; if zero, process state
        dec     ent_timer,x                 ; decrement delay timer
        bne     needle_press_freeze_anim               ; still waiting -> freeze anim + return
needle_press_check_state:  lda     ent_flags,x             ; sprite flags
        and     #$04                    ; check bit 2 (retracted flag)
        beq     needle_press_animate_crush               ; not retracted -> animate crush cycle
        jsr     entity_x_dist_to_player                   ; A = horizontal distance to player
        cmp     #$61                    ; within 97 pixels?
        bcs     needle_press_freeze_anim               ; too far -> freeze anim + return
        lda     ent_flags,x                 ; clear retracted flag (bit 2)
        and     #$FB
        sta     ent_flags,x
        rts

needle_press_animate_crush:  lda     ent_anim_frame,x             ; anim frame timer
        bne     elecn_rts               ; animation still ticking -> wait
        lda     ent_anim_state,x                 ; anim sequence frame index
        bne     needle_press_check_frame_two               ; not frame 0 -> check frame 2
        lda     #$80                    ; damage = invincible only (no hurt)
        sta     ent_hitbox,x
        lda     ent_flags,x                 ; set retracted flag (bit 2)
        ora     #$04
        sta     ent_flags,x
        bne     needle_press_set_delay               ; (always taken) -> set delay timer
needle_press_check_frame_two:  cmp     #$02                ; anim frame 2? (midway)
        bne     needle_press_set_active_damage               ; other frames -> set active damage
        lda     #$AE                    ; damage = hittable + hurts player
        sta     ent_hitbox,x
needle_press_set_delay:  lda     #$1E                ; delay timer = 30 frames
        sta     ent_timer,x
needle_press_freeze_anim:  lda     #$00                ; clear anim frame timer
        sta     ent_anim_frame,x                 ; (forces instant frame advance)
elecn_rts:  rts

needle_press_set_active_damage:  lda     #$A4                ; damage = hittable + hurts player
        sta     ent_hitbox,x
        rts

; ===========================================================================
; main_elecn — Elec'n (electric jellyfish, Spark Man stage)
; Descends from ceiling, fires electric sparks at timed intervals.
; Waits for player proximity before activating. ent_timer=fire interval,
; ent_var1=spark count, ent_var2=spark spawn index.
; ===========================================================================
main_elecn:
        lda     ent_status,x
        and     #$0F
        bne     elecn_anim_check
        lda     ent_flags,x
        and     #$04
        beq     elecn_apply_yspeed
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$61
        bcs     elecn_rts
        lda     ent_flags,x
        and     #$FB
        sta     ent_flags,x
        jmp     set_sprite_hflip

elecn_apply_yspeed:  jsr     apply_y_speed               ; apply_y_speed
        lda     ent_y_px,x
        cmp     #$78
        bcc     elecn_rts
        lda     #$11
        sta     ent_timer,x
        lda     #$03
        sta     ent_var1,x
        inc     ent_status,x
elecn_anim_check:  lda     ent_anim_id,x
        cmp     #$56
        bne     elecn_dec_timer
        lda     ent_anim_frame,x
        bne     elecn_rts
        lda     #$55
        jsr     reset_sprite_anim                   ; reset_sprite_anim
elecn_dec_timer:  dec     ent_timer,x
        lda     ent_timer,x
        bne     elecn_oscillation
        lda     #$11
        sta     ent_timer,x
        inc     ent_var1,x
elecn_oscillation:  lda     ent_var1,x
        and     #$03
        sta     ent_var1,x
        tay
        lda     ent_y_sub,x
        clc
        adc     elecn_y_move_sub,y
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     elecn_y_move_whole,y
        sta     ent_y_px,x
        lda     ent_facing,x
        and     #$02
        beq     elecn_dist_right
        lda     ent_var2,x
        bne     elecn_move_left
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        bcs     elecn_activate
elecn_move_left:  jmp     move_sprite_left               ; move_sprite_left

elecn_dist_right:  lda     ent_var2,x
        bne     elecn_move_right
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        bcc     elecn_activate
elecn_move_right:  jmp     move_sprite_right               ; move_sprite_right

elecn_activate:  lda     #SFX_APPROACH
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     #$56
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        stx     L0000
        lda     #$07
        sta     $01
elecn_spawn_loop:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     elecn_spawn_done
        ldx     $01
        lda     elecn_proj_xvel_sub,x
        sta     ent_xvel_sub,y
        lda     elecn_proj_xvel,x
        sta     ent_xvel,y
        lda     elecn_proj_yvel_sub,x
        sta     ent_yvel_sub,y
        lda     elecn_proj_yvel,x
        sta     ent_yvel,y
        lda     elecn_proj_facing,x
        sta     ent_facing,y
        ldx     L0000
        lda     #$57                    ; delay timer
        jsr     init_child_entity                   ; init_child_entity
        lda     #$80                    ; decrement delay timer
        sta     ent_hitbox,y            ; still waiting -> freeze anim + return
        lda     #$0F                    ; sprite flags
        sta     ent_routine,y           ; check bit 2 (retracted flag)
        lda     ent_x_px,x              ; not retracted -> animate crush cycle
        sta     ent_x_px,y              ; A = horizontal distance to player
        lda     ent_x_scr,x             ; within 97 pixels?
        sta     ent_x_scr,y             ; too far -> freeze anim + return
        lda     ent_y_px,x              ; clear retracted flag (bit 2)
        sec
        sbc     #$0C
        sta     ent_y_px,y
        dec     $01
        bpl     elecn_spawn_loop        ; anim frame timer
elecn_spawn_done:  ldx     L0000        ; animation still ticking -> wait
        inc     ent_var2,x              ; anim sequence frame index
        rts                             ; not frame 0 -> check frame 2

elecn_y_move_sub:  .byte   $80,$80,$80,$80
elecn_y_move_whole:  .byte   $FF,$00,$00,$FF ; set retracted flag (bit 2)
elecn_proj_xvel_sub:  .byte   $00,$6A,$00,$6A,$00,$6A,$00,$6A
elecn_proj_xvel:  .byte   $00,$01,$02,$01,$00,$01,$02,$01
elecn_proj_yvel_sub:  .byte   $00,$96,$00,$6A,$00,$6A,$00,$96 ; (always taken) -> set delay timer
elecn_proj_yvel:  .byte   $FE,$FE,$00,$01,$02,$01,$00,$FE ; anim frame 2? (midway)
elecn_proj_facing:  .byte   $02,$02,$02,$02,$01,$01,$01,$01 ; other frames -> set active damage

; ===========================================================================
; main_peterchy — Peterchy (walking snake, Snake Man stage)
; Walks horizontally with gravity, reverses at walls. Charges when player
; is close (< $10 px), backs off when > $30 px away.
; ===========================================================================
main_peterchy:
        ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     ent_facing,x
        and     #$01
        beq     peterchy_move_right
        ldy     #$1A
        jsr     move_right_collide                   ; move_right_collide
        jmp     peterchy_collision_check

peterchy_move_right:  ldy     #$1B
        jsr     move_left_collide                   ; move_left_collide
peterchy_collision_check:  bcc     peterchy_reverse_dir
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
peterchy_reverse_dir:  lda     ent_status,x
        and     #$0F
        bne     peterchy_aggression_check
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$10
        bcs     peterchy_return
        inc     ent_status,x
peterchy_return:  rts

peterchy_aggression_check:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$30
        bcc     peterchy_return
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        dec     ent_status,x
        rts

; ===========================================================================
; main_walking_bomb — Walking Bomb (Gemini Man / Snake Man stages)
; ===========================================================================
; Walks toward player and explodes when hit by a weapon. Bounces off walls
; up to 3 times before reversing direction. Hitbox $1A ($99), $1C/$1D
; (walk). ent_timer=wall-bounce counter. OAM $51=walk, $52=bounce, $71=explode.
main_walking_bomb:

        ldy     #$1A                    ; apply $99 with hitbox $1A
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        rol     $0F                     ; save carry (landed flag) into $0F bit 0
        jsr     check_sprite_weapon_collision                   ; check if weapon hit this enemy
        bcs     walking_bomb_check_facing               ; survived → continue walking

; --- weapon killed this enemy: explode ---
        lda     #SFX_ENEMY_HIT                    ; play explosion sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     $10                     ; deactivate the weapon that hit us
        lda     #$00
        sta     ent_status,y
        lda     #$71                    ; switch to explosion sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        lda     #$39                    ; change main routine to explosion handler
        sta     ent_routine,x
        rts

; --- survived weapon hit: walk horizontally ---

walking_bomb_check_facing:  lda     ent_facing,x             ; check facing direction
        and     #$01
        beq     walking_bomb_move_left               ; bit 0 clear → move left
        ldy     #$1C                    ; move right with wall collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     walking_bomb_check_ground

walking_bomb_move_left:  ldy     #$1D                ; move left with wall collision
        jsr     move_left_collide                   ; move_left_collide
walking_bomb_check_ground:  lda     $0F                 ; check landed flag (saved from $99)
        and     #$01
        beq     walking_bomb_return               ; not on ground → done
        lda     $10                     ; check collision result: bit 4 = hit wall
        and     #$10
        beq     walking_bomb_restore_walk               ; no wall hit → restore walk anim

; --- hit a wall: bounce ---
        lda     #$52                    ; switch to bounce sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_timer,x                 ; increment bounce counter
        lda     ent_timer,x
        cmp     #$04                    ; 4 bounces → reverse direction
        beq     walking_bomb_reverse_dir
        lda     #$29                    ; set upward bounce velocity
        sta     ent_yvel_sub,x                 ; Y speed = $05.29 upward
        lda     #$05                    ; ($99 will arc it back down)
        sta     ent_yvel,x
        rts

; --- bounced 4 times: reverse walk direction ---

walking_bomb_reverse_dir:  lda     ent_facing,x             ; flip horizontal direction
        eor     #$03                    ; (toggle bits 0 and 1)
        sta     ent_facing,x

; --- restore walking animation ---
walking_bomb_restore_walk:  lda     ent_anim_id,x             ; if already using walk OAM $51, skip
        cmp     #$51
        beq     walking_bomb_reset_bounce
        lda     #$51                    ; reset to walking sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
walking_bomb_reset_bounce:  lda     #$00                ; reset bounce counter
        sta     ent_timer,x
walking_bomb_return:  rts

; ===========================================================================
; main_hologran — Hologran (Gemini Man stage)
; ===========================================================================
; Holographic enemy that drifts horizontally while invisible, then appears
; briefly to shoot when player is within range. ent_flags bit 7=active/on-screen.
; ent_timer=visibility timer (counts down while visible). ent_anim_state=anim frame.
; State 0: invisible, drifting. State 1: visible, counting down timer.
; $8003 = swappable-bank entity update (handles damage, animation).
main_hologran:

        lda     ent_flags,x                 ; check sprite flags bit 7 (on-screen)
        bmi     hologran_active_process               ; on-screen → active processing

; --- off-screen or destroyed: deactivate ---
        lda     #$00                    ; deactivate entity
        sta     ent_status,x
hologran_deactivate_effects:  sta     $95                 ; clear palette cycle / flash vars
        sta     $72
        lda     #$30                    ; set screen effect parameters
        sta     $1D                     ; $1D=$30 (palette base)
        lda     #$F0                    ; $1E=$F0 (palette range)
        sta     $1E
        lda     #$FF                    ; $1C=$FF (effect enable flag)
        sta     $1C
        rts

; --- active: process entity ---

hologran_active_process:  ora     #$08                ; set sprite flag bit 3 (invulnerable frame)
        sta     ent_flags,x
        jsr     process_sprites_top_spin_check               ; call swappable-bank entity update
        lda     ent_hp,x                 ; check HP
        beq     hologran_deactivate_effects               ; HP=0 → killed, deactivate

; --- hold on first animation frame (invisible pose) ---
        lda     ent_anim_state,x                 ; if anim frame > 0, skip hold
        bne     hologran_visibility_timer
        lda     ent_anim_frame,x                 ; if anim counter about to advance (=1),
        cmp     #$01                    ; reset to 0 to hold on frame 0
        bne     hologran_visibility_timer               ; (keeps Hologran invisible until triggered)
        lda     #$00                    ; freeze animation on invisible frame
        sta     ent_anim_frame,x

; --- timer-based phase control ---
hologran_visibility_timer:  lda     ent_timer,x             ; if visibility timer active,
        bne     hologran_countdown_timer               ; skip to countdown

; --- state 0: invisible, drifting horizontally ---
        lda     ent_facing,x                 ; move in facing direction
        and     #$01
        beq     hologran_move_left               ; bit 0 clear → move left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     hologran_check_range

hologran_move_left:  jsr     move_sprite_left               ; move_sprite_left
hologran_check_range:  lda     ent_status,x             ; check entity state
        and     #$0F
        bne     hologran_return               ; already appeared → done
        jsr     entity_x_dist_to_player                   ; get X distance to player
        cmp     #$61                    ; if farther than $61 px,
        bcs     hologran_return               ; too far → keep drifting

; --- player in range: appear and shoot ---
        inc     ent_status,x                 ; advance state (0 → 1: visible)
        inc     ent_anim_state,x                 ; advance anim frame (show visible pose)
        lda     #$3C                    ; visibility timer = 60 frames (1 second)
        sta     ent_timer,x
        lda     ent_routine,x                 ; odd main routine index:
        and     #$01                    ; set slow X drift speed $00.40
        beq     hologran_set_appearance               ; even → skip speed change
        lda     #$40                    ; X speed sub = $40
        sta     ent_xvel_sub,x          ; survived → continue walking
        lda     #$00                    ; X speed whole = $00
        sta     ent_xvel,x                 ; (slow drift: 0.25 px/frame)
hologran_set_appearance:  lda     #$FF                ; set palette flash / screen effect
        sta     $1C                     ; $1C=$FF, $1D=$10, $1E=$10
        lda     #$10                    ; (visual flash on appearance)
        sta     $1D
        sta     $1E
        lda     #$00                    ; clear palette cycle var
        sta     $95

; --- countdown visibility timer ---
hologran_countdown_timer:  dec     ent_timer,x             ; decrement timer; when 0
        bne     hologran_return               ; entity returns to invisible drift
hologran_return:  rts

; ===========================================================================
; main_parasyu — Parasyu (parachute bomb, Gemini Man stage)
; Falls at 3.0 px/frame. When player is within 100 px X-distance,
; drops the parachute and falls faster. Explodes on contact with ground.
; ===========================================================================
main_parasyu:                           ; move right with wall collision

        lda     ent_status,x                 ; check entity state
        and     #$0F
        bne     parasyu_state_activated               ; nonzero -> activated, skip init
        sta     ent_yvel_sub,x                 ; Y speed sub = 0
        lda     #$03                    ; Y speed whole = 3 (fall at 3.0 px/frame
        sta     ent_yvel,x                 ; with parachute)
        jsr     entity_x_dist_to_player                   ; X distance to player
        cmp     #$64                    ; if > 100 px away,
        bcs     parasyu_init_done               ; too far -> keep falling

; --- player close: detach parachute ---
        lda     $E4                     ; pseudo-random: add shift register bytes
        adc     $E5                     ; to get random index 0-3
        sta     $E5
        and     #$03                    ; increment bounce counter
        tay
        lda     parasyu_initial_delay_table,y                 ; load random delay timer from table
        sta     ent_var2,x                 ; ($22, $2A, $26, or $2E frames)
        inc     ent_status,x                 ; advance state -> 1 (activated)
        lda     ent_flags,x                 ; toggle sprite flag bit 2
        eor     #$04                    ; (visual change: chute detaching)
        sta     ent_flags,x
parasyu_init_done:  rts

; --- state 1+: activated (parachute detaching or detached) ---

parasyu_state_activated:  lda     ent_var3,x             ; check phase flag
        bne     parasyu_phase_detached               ; nonzero -> chute detached, swoop phase

; --- phase 1: chute still attached, slow fall with delay ---
        lda     ent_var2,x                 ; if delay timer > 0,
        beq     parasyu_detach_parachute               ; expired -> detach chute
        dec     ent_var2,x                 ; count down delay
        jmp     move_sprite_down                   ; fall slowly while waiting

; --- delay expired: detach parachute ---

parasyu_detach_parachute:  lda     #$4D                ; switch to detached sprite (OAM $4D)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_var3,x                 ; set phase flag -> 1 (detached)
        lda     #$10                    ; set fall timer = 16 frames
        sta     ent_var2,x                 ; (before first swoop)
        rts

; --- phase 2: chute detached, swoop pattern ---

parasyu_phase_detached:  lda     ent_anim_state,x             ; clamp anim frame: if >= 2,
        cmp     #$02                    ; force frame 3 and reset counter
        bcc     parasyu_clamp_anim_frame               ; (lock to falling sprite pose)
        lda     #$03                    ; set anim frame = 3
        sta     ent_anim_state,x        ; check sprite flags bit 7 (on-screen)
        lda     #$00                    ; reset anim counter
        sta     ent_anim_frame,x
parasyu_clamp_anim_frame:  lda     ent_status,x             ; check state bit 1
        and     #$02                    ; (0=swooping, 1=gentle fall between swoops)
        beq     parasyu_swoop_phase               ; bit 1 clear -> swoop phase
        jmp     parasyu_gentle_fall_phase               ; bit 1 set -> gentle fall phase

; --- swoop sub-phase: sine-like arc movement ---

parasyu_swoop_phase:  lda     ent_timer,x             ; speed hold timer: if > 0,
        bne     parasyu_apply_speed               ; hold current speed -> skip to apply

; --- load next speed step from tables ---
        ldy     ent_var1,x                 ; swoop step index -> $B6D6 lookup
        lda     parasyu_swoop_index_table,y                 ; get speed table sub-index
        asl     a                       ; *2 for 16-bit table offset
        tay
        lda     bombflier_yspeed_lo,y                 ; load Y speed from table (16-bit)
        sta     ent_yvel_sub,x
        lda     bombflier_yspeed_table,y ; call swappable-bank entity update
        sta     ent_yvel,x              ; check HP
        lda     bombflier_xspeed_lo,y                 ; load X speed from table (16-bit)
        sta     ent_xvel_sub,x
        lda     bombflier_xspeed_table,y
        sta     ent_xvel,x              ; if anim frame > 0, skip hold
        lda     ent_xvel,x                 ; if X speed whole is negative,
        bpl     parasyu_advance_swoop_step               ; positive -> skip negate

; --- negate X speed (make absolute value for directional move) ---
        lda     ent_xvel_sub,x                 ; two's complement negate 16-bit
        eor     #$FF                    ; X speed sub
        clc
        adc     #$01
        sta     ent_xvel_sub,x          ; if visibility timer active,
        lda     ent_xvel,x                 ; X speed whole
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x              ; move in facing direction
parasyu_advance_swoop_step:  inc     ent_var1,x             ; advance swoop step
        lda     ent_var1,x              ; bit 0 clear → move left
        cmp     #$07                    ; after 7 steps: swoop complete
        bne     parasyu_load_speed_timer
        lda     ent_facing,x                 ; reverse horizontal direction
        eor     #$03
        sta     ent_facing,x            ; check entity state
        inc     ent_status,x                 ; advance state (set bit 1 -> fall phase)
        lda     #$00                    ; reset swoop step for next cycle
        sta     ent_var1,x              ; get X distance to player
parasyu_load_speed_timer:  lda     #$05                ; hold each speed step for 5 frames
        sta     ent_timer,x             ; too far → keep drifting

; --- apply current speed to position ---
parasyu_apply_speed:  dec     ent_timer,x             ; decrement speed hold timer
        lda     ent_y_sub,x                 ; add Y speed to Y position (16-bit)
        clc                             ; visibility timer = 60 frames (1 second)
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x             ; odd main routine index:
        lda     ent_y_px,x              ; set slow X drift speed $00.40
        adc     ent_yvel,x              ; even → skip speed change
        sta     ent_y_px,x              ; X speed sub = $40
        lda     ent_facing,x                 ; move horizontally by X speed
        and     #$02                    ; bit 1: 0=right, 1=left
        bne     parasyu_move_left       ; (slow drift: 0.25 px/frame)
        jsr     move_sprite_right                   ; move_sprite_right
        bcs     parasyu_horizontal_done               ; (unconditional branch pair)
        bcc     parasyu_horizontal_done ; (visual flash on appearance)
parasyu_move_left:  jsr     move_sprite_left               ; move_sprite_left
parasyu_horizontal_done:  rts

; --- gentle fall phase (between swoops) ---

parasyu_gentle_fall_phase:  dec     ent_var2,x             ; count down fall timer
        bne     parasyu_fall_timer_expired               ; not expired -> keep falling
        dec     ent_status,x                 ; clear state bit 1 -> back to swoop
        lda     #$10                    ; reset fall timer = 16 frames
        sta     ent_var2,x
parasyu_fall_timer_expired:  lda     #$80                ; Y speed = $00.80 (0.5 px/frame)
        sta     ent_yvel_sub,x                 ; gentle downward drift
        lda     #$00
        sta     ent_yvel,x
        jmp     move_sprite_down                   ; apply downward movement

; parasyu data tables
; $B6D6: swoop speed sub-indices (7 steps per swoop, 2 swoops = 14 entries)

parasyu_swoop_index_table:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$09 ; nonzero -> activated, skip init
        .byte   $0A,$0B,$0C,$0D,$0E,$0F ; Y speed sub = 0
parasyu_initial_delay_table:  .byte   $22,$2A,$26,$2E ; Y speed whole = 3 (fall at 3.0 px/frame

; ===========================================================================
; main_doc_robot_intro — Doc Robot introduction sequence (all 8 stages)
; ===========================================================================
; Same routine handles all 8 Doc Robot intros. State 0: calls init_boss_wait,
; spawns a shutter entity, sets up CHR banks and palette for the specific
; Doc Robot master. State 1: waits for boss HP bar to fill ($B0 >= $9C),
; then morphs this entity into the actual Doc Robot AI (via
; doc_robot_master_main_indices table).
main_doc_robot_intro:
        lda     #$00
        sta     ent_anim_frame,x        ; load random delay timer from table
        lda     ent_status,x            ; ($22, $2A, $26, or $2E frames)
        and     #$0F                    ; advance state -> 1 (activated)
        beq     doc_robot_intro_shutter_init ; toggle sprite flag bit 2
        lda     boss_hp_display         ; (visual change: chute detaching)
        cmp     #$9C
        bne     doc_robot_intro_morph_done
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        stx     L0000                   ; check phase flag
        lda     ent_routine,x           ; nonzero -> chute detached, swoop phase
        and     #$07                    ; fetch doc robot master's
        tay                             ; main routine index
        lda     doc_robot_master_main_indices,y ; morph this sprite into it
        sta     ent_routine,x           ; expired -> detach chute
        lda     #$CA                    ; doc robot hitbox
        sta     ent_hitbox,x            ; fall slowly while waiting
        lda     #$1C                    ; 28 HP
        sta     ent_hp,x
        lda     doc_robot_intro_xvel_sub,y                 ; master-specific X velocity (sub)
        sta     ent_xvel_sub,x          ; switch to detached sprite (OAM $4D)
        lda     doc_robot_intro_xvel,y                 ; master-specific X velocity (whole)
        sta     ent_xvel,x              ; set phase flag -> 1 (detached)
        jsr     reset_gravity                   ; reset_gravity
        lda     doc_robot_intro_chr_lookup,y                 ; CHR bank set for this master
        sta     $ED
        jsr     update_CHR_banks                   ; update_CHR_banks
        lda     #$C0                    ; set status: active + invincible
        sta     ent_status,x
        tya                             ; clamp anim frame: if >= 2,
        asl     a                       ; force frame 3 and reset counter
        asl     a                       ; (lock to falling sprite pose)
        tay                             ; set anim frame = 3
        ldx     #$00
doc_robot_intro_palette_loop:  lda     doc_robot_intro_palette_data,y ; reset anim counter
        sta     $061C,x
        sta     $063C,x                 ; check state bit 1
        iny                             ; (0=swooping, 1=gentle fall between swoops)
        inx                             ; bit 1 clear -> swoop phase
        cpx     #$04                    ; bit 1 set -> gentle fall phase
        bne     doc_robot_intro_palette_loop
        lda     #$FF
        sta     palette_dirty
        ldx     L0000                   ; speed hold timer: if > 0,
doc_robot_intro_morph_done:  rts        ; hold current speed -> skip to apply

; --- state 0: spawn shutter and set up doc robot ---
doc_robot_intro_shutter_init:  jsr     init_boss_wait          ; freeze player, start HP bar fill
        inc     ent_status,x            ; get speed table sub-index
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        lda     #$61
        sta     ent_routine,y           ; load Y speed from table (16-bit)
        lda     ent_y_px,x
        sta     ent_var1,y
        lda     ent_x_px,x
        sta     ent_x_px,y              ; load X speed from table (16-bit)
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     #$10
        sta     ent_y_px,y              ; if X speed whole is negative,
        lda     #$80                    ; positive -> skip negate
        sta     ent_yvel_sub,y
        sta     ent_hitbox,y
        lda     #$00                    ; two's complement negate 16-bit
        sta     ent_yvel,y              ; X speed sub
        stx     L0000
        lda     ent_routine,x
        and     #$07
        tax                             ; X speed whole
        lda     doc_robot_intro_sprite_anim,x
        ldx     L0000
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_routine,x           ; advance swoop step
        and     #$07
        tay                             ; after 7 steps: swoop complete
        lda     doc_robot_intro_chr_bank,y
        sta     $ED                     ; reverse horizontal direction
        tya
        asl     a
        tay                             ; advance state (set bit 1 -> fall phase)
        lda     #$0F                    ; reset swoop step for next cycle
        sta     $061C
        sta     $061D                   ; hold each speed step for 5 frames
        sta     $063C
        sta     $063D
        lda     doc_robot_intro_palette_color,y
        sta     $061E                   ; decrement speed hold timer
        sta     $063E                   ; add Y speed to Y position (16-bit)
        lda     doc_robot_intro_palette_setup,y
        sta     $061F
        sta     $063F
        lda     #$FF
        sta     palette_dirty
        jmp     update_CHR_banks                   ; update_CHR_banks

        jsr     move_sprite_down                   ; move_sprite_down
        lda     ent_y_px,x
        cmp     ent_var1,x
        beq     doc_robot_intro_shutter_landed ; (unconditional branch pair)
        lda     #$80
        sta     boss_hp_display
        rts

doc_robot_intro_shutter_landed:  lda     #$00
        sta     ent_status,x
        rts                             ; count down fall timer

; ---------------------------------------------------------------------------
; init_boss_wait — freeze player for boss intro sequence
; Called when boss shutter closes. Sets state $09 (boss_wait):
; player frozen while boss HP bar fills, then released to $00.
; Same pattern used by all Robot Masters, Doc Robots, and fortress bosses.
; $B0 = boss HP meter position, $B3 = HP fill target ($8E = 28 HP).
; ---------------------------------------------------------------------------

init_boss_wait:  lda     #PSTATE_BOSS_WAIT           ; state → $09 (boss_wait)
        sta     player_state                     ; freeze player
        lda     #$80                    ; init boss HP display
        sta     boss_hp_display                     ; $B0 = HP bar position
        sta     boss_active                     ; $5A = boss active flag
        lda     #$8E                    ; $B3 = HP fill target
        sta     $B3                     ; ($8E = $80 + 14 ticks = 28 HP)
        lda     #MUSIC_WILY_MAP                    ; SFX $0C = boss intro music
        jsr     submit_sound_ID_D9                   ; submit_sound_ID_D9
        rts

doc_robot_intro_sprite_anim:  .byte   $16,$1A,$14,$18,$15,$13,$19,$17

; doc robot AI indices, as opposed to the intro docs
; Flash, Bubble, Quick, Wood, Crash, Air, Metal, Heat
doc_robot_master_main_indices:  .byte   $A0,$B0,$B2,$A1,$A2,$B3,$A3,$B1
doc_robot_intro_chr_bank:  .byte   $23,$10,$23,$23,$23,$23,$10,$23
doc_robot_intro_palette_color:  .byte   $30
doc_robot_intro_palette_setup:  .byte   $11,$30,$19,$27,$15,$37,$17,$30
        .byte   $26,$27,$11,$27,$15,$27,$15
doc_robot_intro_chr_lookup:  .byte   $21,$22,$21,$22,$21,$22,$21,$22
doc_robot_intro_palette_data:  .byte   $0F,$30,$15,$27,$0F,$0F,$30,$19
        .byte   $0F,$0F,$27,$15,$0F,$0F,$30,$19
        .byte   $0F,$0F,$30,$26,$0F,$0F,$2C,$11
        .byte   $0F,$0F,$27,$15,$0F,$0F,$27,$15
doc_robot_intro_xvel_sub:  .byte   $00,$00,$00,$B3,$4C,$00,$80,$00
doc_robot_intro_xvel:  .byte   $01,$00,$00,$01,$01,$00,$02,$04

; ===========================================================================
; main_robot_master_intro — Robot Master introduction sequence (all 8)
; ===========================================================================
; Shared intro for all 8 Robot Masters. State 0: calls init_boss_wait,
; advances state. Then falls with gravity until Y >= $80. Once landed and
; boss HP bar filled ($B0 >= $9C), morphs entity into the actual Robot Master
; AI (via robot_master_main_indices table), sets HP to $1C (28), and loads
; the master-specific initial animation frame.
main_robot_master_intro:
        lda     ent_status,x
        and     #$0F
        bne     robot_master_intro_falling
        inc     ent_status,x
        jsr     init_boss_wait                  ; freeze player, start HP bar fill
; --- falling / waiting for HP bar to fill ---
robot_master_intro_falling:  lda     ent_y_px,x          ; still above landing Y ($80)?
        cmp     #$80
        bcs     robot_master_intro_gravity               ; no → apply gravity with floor
        jsr     apply_y_speed                   ; apply_y_speed (still falling)
        jmp     robot_master_intro_clear_frame

robot_master_intro_gravity:  lda     ent_routine,x       ; look up floor tile for this master
        and     #$07
        tay
        lda     robot_master_intro_gravity_floor,y                 ; gravity floor offset per master
        tay
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     robot_master_intro_clear_frame               ; not landed yet
        lda     ent_routine,x           ; check if landing animation done
        and     #$07
        tay
        lda     robot_master_intro_target_anim,y                 ; target anim_state for this master
        cmp     ent_anim_state,x
        bne     robot_master_intro_landing_wait               ; not yet → keep boss HP bar filling
        lda     #$00
        sta     ent_anim_frame,x
        lda     boss_hp_display
        cmp     #$9C                    ; HP bar fully filled?
        bne     robot_master_intro_rts               ; no → wait
; --- morph into actual Robot Master AI ---
        lda     #$C0                    ; active + invincible
        sta     ent_status,x
        lda     #$1C                    ; 28 HP
        sta     ent_hp,x
        lda     ent_routine,x
        and     #$07                    ; fetch robot master's
        tay                             ; main routine index
        lda     robot_master_main_indices,y ; morph this sprite into it
        sta     ent_routine,x
        lda     robot_master_intro_xvel_sub,y
        sta     ent_xvel_sub,x
        lda     robot_master_intro_xvel,y
        sta     ent_xvel,x
        lda     robot_master_intro_init_anim,y
        jmp     reset_sprite_anim                   ; reset_sprite_anim

robot_master_intro_clear_frame:  lda     #$00
        sta     ent_anim_frame,x
robot_master_intro_landing_wait:  lda     #$80
        sta     boss_hp_display
robot_master_intro_rts:  rts

; Needle, Magnet, Gemini, Hard, Top, Snake, Spark, Shadow
robot_master_main_indices:  .byte   $C0,$C1,$D6,$D0,$C2,$D4,$D2,$C3
robot_master_intro_xvel_sub:  .byte   $B3,$00,$2D,$00,$00,$4C,$6D,$00 ; X velocity (sub) per master
robot_master_intro_xvel:  .byte   $01,$00,$03,$01,$04,$01,$01,$04 ; X velocity (whole) per master
robot_master_intro_init_anim:  .byte   $29,$1F,$33,$2C,$49,$22,$36,$3F ; initial anim frame per master
robot_master_intro_target_anim:  .byte   $04,$03,$05,$06,$02,$02,$08,$03 ; target anim_state per master
robot_master_intro_gravity_floor:  .byte   $1E,$1E,$00,$26,$1E,$00,$1E,$1E ; gravity floor offset per master
        .byte   $60

; ===========================================================================
; main_spinning_wheel — Spinning Wheel (Shadow Man stage conveyor)
; Checks player collision, then scrolls camera horizontally based on
; the OAM ID's low bit (left/right wheel direction).
; ===========================================================================
main_spinning_wheel:
        lda     ent_y_px,x
        pha
        dec     ent_y_px,x
        jsr     check_player_collision                   ; check_player_collision
        pla
        sta     ent_y_px,x
        bcs     spinning_wheel_done
        lda     ent_anim_id,x
        and     #$01
        tay
        lda     ent_x_sub
        clc
        adc     spinning_wheel_x_offsets,y
        sta     ent_x_sub
        lda     ent_x_px
        adc     spinning_wheel_y_sign,y
        sta     ent_x_px
        lda     ent_x_scr
        adc     spinning_wheel_y_data,y
        sta     ent_x_scr
spinning_wheel_done:  rts

spinning_wheel_x_offsets:  .byte   $80,$80
spinning_wheel_y_sign:  .byte   $00,$FF
spinning_wheel_y_data:  .byte   $00,$FF

; ===========================================================================
; main_trap_platform — Trap Platform (Shadow Man stage)
; Triggers when player is close (< $15 Y, < $18 X). Plays open animation,
; then closes after a delay. Toggles sprite flag bit 0 on completion.
; ===========================================================================
main_trap_platform:                     ; state → $09 (boss_wait)
        lda     ent_status,x            ; freeze player
        and     #$0F                    ; init boss HP display
        bne     trap_platform_var1_check ; $B0 = HP bar position
        sta     ent_anim_state,x        ; $5A = boss active flag
        sta     ent_anim_frame,x        ; $B3 = HP fill target
        sta     ent_var1,x              ; ($8E = $80 + 14 ticks = 28 HP)
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$15
        bcs     trap_platform_timer_return
        lda     player_state
        bne     trap_platform_timer_return
        lda     ent_y_px,x
        cmp     ent_y_px
        bcc     trap_platform_timer_return
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$18
        bcs     trap_platform_timer_return
        lda     #$0C
        sta     ent_timer,x
        inc     ent_status,x
trap_platform_timer_return:  rts

trap_platform_var1_check:  lda     ent_var1,x
        bne     trap_platform_status_check
        dec     ent_timer,x
        bne     trap_platform_timer_return
        inc     ent_var1,x
        lda     ent_flags,x
        and     #$90
        sta     ent_flags,x
trap_platform_status_check:  lda     ent_status,x
        and     #$02
        bne     trap_platform_animation_set
        lda     ent_anim_state,x
        cmp     #$04
        bne     trap_platform_timer_return
        inc     ent_status,x
        lda     #$14
        sta     ent_timer,x
trap_platform_animation_set:  lda     #$04
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        dec     ent_timer,x
        bne     trap_platform_timer_return
        lda     ent_flags,x
        eor     #$01
        sta     ent_flags,x
        lda     #$80
        sta     ent_status,x
        rts

; ===========================================================================
; main_breakable_wall — Breakable Wall (Hard Knuckle / Shadow Blade target)
; Checks weapon slots $01-$02 for Hard Knuckle ($AC) or Shadow Blade ($AF).
; If hit, plays explosion and changes AI routine to $19 (debris).
; ===========================================================================
main_breakable_wall:

        ldy     #$01
breakable_wall_child_status:  lda     ent_status,y
        bpl     breakable_wall_loop_next
        lda     ent_anim_id,y
        cmp     #$AC
        beq     breakable_wall_collision_check
        cmp     #$AF
        bne     breakable_wall_loop_next
breakable_wall_collision_check:  jsr     check_sprite_weapon_collision               ; check_sprite_weapon_collision
        bcs     breakable_wall_loop_next
        lda     #SFX_ENEMY_HIT
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     $10
        lda     #$00
        sta     ent_status,y
        lda     #$71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$19
        sta     ent_routine,x
        rts

breakable_wall_loop_next:  iny
        cpy     #$03
        bcc     breakable_wall_child_status ; fetch robot master's
        rts                             ; main routine index

; ===========================================================================
; main_spark_falling_platform — Spark Man stage falling platform
; ===========================================================================
; Platform that falls when the player stands on it. State 0: checks if player
; is within range (Y < $15, X < $0A), saves initial Y to ent_var3 for
; return trip. State 1: falls upward (accelerates from speed $01.00 to max
; $03.00), stops when Y < $3A (offscreen top). State 2: returns to original
; Y position (ent_var3), then re-checks player proximity to cycle again.
main_spark_falling_platform:

        lda     ent_status,x
        and     #$0F
        bne     spark_platform_rising_state
; --- state 0: idle, waiting for player proximity ---
        sta     ent_yvel_sub,x          ; init Y speed = $01.00
        lda     #$01
        sta     ent_yvel,x
        lda     ent_y_px,x
        sta     ent_var3,x              ; save home Y position
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$15                    ; within $15 pixels Y?
        bcs     spark_platform_return
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$0A                    ; within $0A pixels X?
        bcs     spark_platform_return
        inc     ent_status,x            ; player close enough → start rising
; --- state 1: rising upward ---
spark_platform_rising_state:  lda     ent_timer,x
        bne     spark_platform_return_home               ; timer set → returning phase
        jsr     move_sprite_up                   ; move_sprite_up
        lda     ent_yvel_sub,x          ; accelerate upward (+$10 sub each frame)
        clc
        adc     #$10
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        cmp     #$03                    ; cap speed at $03.00
        bne     spark_platform_speed_cap
        lda     #$00
        sta     ent_yvel_sub,x
spark_platform_speed_cap:  lda     ent_y_px,x
        cmp     #$3A                    ; reached top of screen?
        bcs     spark_platform_return
        inc     ent_timer,x             ; mark: time to return
        lda     #$00                    ; reset speed to $01.00 for descent
        sta     ent_yvel_sub,x
        lda     #$01
        sta     ent_yvel,x
spark_platform_return:  rts

; --- state 1+: returning to home position ---
spark_platform_return_home:  lda     ent_status,x
        and     #$02
        bne     spark_platform_reaggression               ; state 2+ → check player again
        jsr     top_man_platform_move_down_check               ; move downward (return to home)
        lda     ent_y_px,x
        cmp     ent_var3,x              ; reached home Y?
        bcc     spark_platform_return               ; not yet
        inc     ent_status,x            ; advance to state 2
spark_platform_reaggression:  jsr     entity_y_dist_to_player               ; entity_y_dist_to_player
        cmp     #$16
        bcs     spark_platform_return
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$09
        bcs     spark_platform_return
        dec     ent_status,x
        dec     ent_timer,x
        rts

; ===========================================================================
; main_big_snakey — Big Snakey (Snake Man stage mini-boss)
; ===========================================================================
; Large snake entity. State 0: picks random shot count (2-4) from table,
; sets $78 frame timer. State 1: when timer expires, opens mouth (animation
; state |= $01), spawns homing bullet child (routine $8F) using
; calc_homing_velocity (calc_homing_velocity). After all shots fired, returns to state 0.
; On death (HP=0): despawns all child projectiles below Y < $80, sets $55
; to $80 (screen shake / boss defeated flag).
main_big_snakey:

; --- state 0: init, pick random shot count ---
        lda     ent_status,x
        and     #$0F
        bne     big_snakey_fire_state
        inc     ent_status,x
        lda     $E4                     ; RNG: $E4 += $E6
        adc     $E6
        sta     $E4
        and     #$03                    ; 0-3 index
        tay
        lda     big_snakey_shot_count,y                 ; shot count: 2, 3, 4, or 2
        sta     ent_var1,x
        lda     #$78                    ; 120-frame delay before firing
        sta     ent_timer,x
        bne     big_snakey_collision
; --- state 1: firing phase ---
big_snakey_fire_state:  lda     ent_timer,x
        bne     big_snakey_pre_fire              ; timer not expired → wait
        lda     ent_anim_state,x       ; open mouth (anim |= $01)
        ora     #$01
        sta     ent_anim_state,x
        lda     ent_var2,x             ; inter-shot delay active?
        bne     big_snakey_inter_shot              ; yes → count down
; --- spawn homing bullet child ---
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        lda     #$BA                    ; child anim ID
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_x_px,x             ; copy position to child
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$80
        sta     ent_hitbox,y            ; child hitbox
        lda     #$8F
        sta     ent_routine,y           ; child AI routine
        lda     #$00
        sta     ent_xvel_sub,y
        sta     $02                     ; target speed (sub) for homing calc
        lda     #$04
        sta     ent_xvel,y
        sta     $03                     ; target speed (whole) for homing calc
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     calc_homing_velocity                   ; calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C                     ; homing result → child facing
        sta     ent_facing,y
        dec     ent_var1,x              ; shots remaining--
        beq     big_snakey_shots_done              ; all shots fired → close mouth
        lda     #$12                    ; 18-frame delay between shots
        sta     ent_var2,x
        bne     big_snakey_collision
big_snakey_shots_done:  lda     #$00               ; close mouth (clear anim bit)
        sta     ent_anim_state,x
        dec     ent_status,x            ; return to state 0
        bne     big_snakey_collision
big_snakey_inter_shot:  dec     ent_var2,x          ; inter-shot cooldown
        jmp     big_snakey_collision

big_snakey_pre_fire:  dec     ent_timer,x         ; pre-fire delay countdown
; --- common: run bank $1C collision + check death ---
big_snakey_collision:  lda     ent_anim_id,x       ; preserve anim_id across $8003 call
        pha
        jsr     process_sprites_top_spin_check               ; bank $1C collision handler
        pla
        sta     ent_anim_id,x
        lda     ent_hp,x
        bne     big_snakey_post_collision              ; still alive → skip
; --- death: despawn all child projectiles ---
        sta     ent_status,x            ; despawn self
        ldy     #$0F
big_snakey_despawn_loop:  lda     $0310,y             ; check enemy slot $10+Y
        bpl     big_snakey_loop_check              ; not active → skip
        lda     $03D0,y                 ; child Y position
        cmp     #$80                    ; below midscreen?
        bcs     big_snakey_loop_check              ; yes → don't despawn (offscreen)
        lda     #$00
        sta     $0310,y                 ; despawn child
big_snakey_loop_check:  dey
        bpl     big_snakey_despawn_loop
        lda     #$80
        sta     $55                     ; boss defeated flag / screen shake
big_snakey_post_collision:  lda     #$00
        sta     ent_anim_frame,x
        rts

        lda     ent_facing,x
        and     #$01
        beq     big_snakey_move_left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     big_snakey_vert_check

big_snakey_move_left:  jsr     move_sprite_left               ; move_sprite_left
big_snakey_vert_check:  lda     ent_facing,x
        and     #$08
        beq     big_snakey_move_down
        jmp     move_sprite_up                   ; move_sprite_up

big_snakey_move_down:  jmp     move_sprite_down               ; move_sprite_down

big_snakey_shot_count:  .byte   $03,$03,$04,$02         ; shot count table (3, 3, 4, 2)

; --- init_tama — shared Tama initialization / floor clamp ---
init_tama:
        lda     ent_anim_id,x
        beq     tama_init_return
        jsr     apply_y_speed                   ; apply_y_speed
        ldy     #$00
        sty     $54
        lda     ent_x_scr,x
        cmp     #$0B
        beq     tama_init_load_floor
        iny
tama_init_load_floor:  lda     tama_init_floor_table,y
        cmp     ent_y_px,x
        bcs     tama_init_clamp_floor
        sta     ent_y_px,x
        lda     ent_anim_frame,x
        cmp     #$02
        bne     tama_init_return
        lda     ent_anim_state,x
        cmp     #$02
        bne     tama_init_return
        lda     #$20
        sta     $060D
        sta     $062D
        lda     #$37
        sta     $060E
        sta     $062E
        lda     #$17
        sta     $060F
        sta     $062F
        sta     palette_dirty
        lda     #$00
        sta     ent_anim_id,x
        ldy     #$1F
tama_init_loop_entities:  lda     ent_status,y
        bpl     tama_init_entity_continue
        lda     ent_flags,y
        and     #$FB
        sta     ent_flags,y
tama_init_entity_continue:  dey
        cpy     #$0F
        bne     tama_init_loop_entities
        rts

tama_init_clamp_floor:  lda     #$00
        sta     ent_anim_frame,x
tama_init_return:  rts

tama_init_floor_table:  .byte   $48,$78

; ===========================================================================
; main_tama_A — Tama (giant cat, Snake Man stage boss support, variant A)
; ===========================================================================
; Tama variant A. Checks if disabled (flags bit 2). Preserves anim_id across
; bank $1C collision check ($8003). On death (HP=0): clears all enemy slots,
; switches to item-drop routine $63. When alive: state 0 waits for all $CF/$D0
; child projectiles to despawn (checks $54 flag). State 1: spawns kitten
; projectiles ($CF) with homing velocity, then returns to state 0 after 2
; volleys. Includes floor-level clamping from tama_init_floor_table table ($48/$78).
main_tama_A:
        lda     ent_flags,x
        and     #$04
        bne     tama_init_return
        lda     ent_anim_id,x
        pha
        jsr     process_sprites_top_spin_check
        pla
        sta     ent_anim_id,x
        lda     ent_hp,x
        bne     tama_a_scan_proj_state
        sta     ent_var1,x
        sta     ent_hitbox,x
        lda     #$04
        sta     ent_var2,x
        lda     #$63
        sta     ent_routine,x
        ldy     #$0F
        lda     #$00
tama_a_clear_oam_loop:  sta     $0310,y
        dey
        bpl     tama_a_clear_oam_loop
        lda     #$80
        sta     ent_status,x
        rts

tama_a_scan_proj_state:  lda     ent_status,x
        and     #$0F
        bne     tama_a_check_anim_fire
        sta     ent_anim_frame,x
        ldy     #$1F
tama_a_scan_slots_loop:  lda     ent_status,y
        bpl     tama_a_loop_next_slot
        lda     ent_anim_id,y
        cmp     #$CF
        beq     tama_a_scan_return
        cmp     #$D0
        beq     tama_a_scan_return
tama_a_loop_next_slot:  dey
        cpy     #$0F
        bne     tama_a_scan_slots_loop
        lda     $54
        bne     tama_a_scan_return
        inc     ent_status,x
tama_a_scan_return:  rts

tama_a_check_anim_fire:  lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     tama_a_check_fire_cooldown
        inc     ent_var1,x
        lda     ent_var1,x
        cmp     #$02
        bne     tama_a_set_fire_cooldown
        dec     ent_status,x
        lda     #$00
        sta     ent_timer,x
        sta     ent_var1,x
        inc     $54
        rts

tama_a_set_fire_cooldown:  lda     #$3C
        sta     ent_timer,x
        rts

tama_a_check_fire_cooldown:  lda     ent_timer,x
        beq     tama_a_spawn_proj_check
        dec     ent_timer,x
        lda     #$00
        sta     ent_anim_frame,x
        rts

tama_a_spawn_proj_check:  lda     ent_anim_frame,x
        bne     tama_a_spawn_return
        lda     ent_anim_state,x
        cmp     #$02
        bne     tama_a_spawn_return
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     tama_a_spawn_return
        lda     #$CF
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$08
        sta     ent_hp,y
        lda     #$8D
        sta     ent_routine,y
        lda     #$00
        sta     ent_xvel_sub,y
        lda     #$01
        sta     ent_xvel,y
        lda     #$00
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        jsr     face_player                   ; face_player
        lda     ent_facing,x
        sta     ent_facing,y
tama_a_spawn_return:  rts

tama_a_dead_rts:  rts

; ===========================================================================
; main_tama_B — Tama (giant cat, Snake Man stage boss support, variant B)
; ===========================================================================
; Tama variant B. Similar to variant A but spawns different projectiles ($D0)
; and uses the $54 flag in reverse: waits for $54 to be set before advancing.
; State 0: scans enemy slots for active $CF/$D0 projectiles. State 1: fires
; a spread of 3 bouncing kitten bullets ($D0) using velocity tables at $BD59,
; each with 30-frame timers. Clears $54 when all shots spawned.
main_tama_B:

        lda     ent_flags,x
        and     #$04
        bne     tama_a_dead_rts
        lda     ent_status,x
        and     #$0F
        bne     tama_b_state_active
        sta     ent_anim_frame,x
        ldy     #$1F
tama_b_scan_active_projectiles:  lda     ent_status,y
        bpl     tama_b_next_slot
        lda     ent_anim_id,y
        cmp     #$CF
        beq     tama_b_state_idle_done
        cmp     #$D0
        beq     tama_b_state_idle_done
tama_b_next_slot:  dey
        cpy     #$0F
        bne     tama_b_scan_active_projectiles
        lda     $54
        beq     tama_b_state_idle_done
        inc     ent_status,x
tama_b_state_idle_done:  rts

tama_b_state_active:  lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     tama_b_check_anim_frame
        dec     ent_status,x
tama_b_check_anim_frame:  lda     ent_anim_state,x
        cmp     #$02
        bne     tama_b_state_idle_done
        lda     ent_anim_frame,x
        bne     tama_b_state_idle_done
        lda     #$02
        sta     $10
        jsr     face_player                   ; face_player
tama_b_launch_projectile_loop:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     tama_b_launch_done
        lda     #$D0
        jsr     init_child_entity                   ; init_child_entity
        lda     #$01
        sta     ent_anim_state,y
        sta     ent_hp,y
        lda     #$CB
        sta     ent_hitbox,y
        lda     #$8E
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     ent_facing,x
        sta     ent_facing,y
        stx     L0000
        ldx     $10
        lda     tama_b_proj_yspeed_sub,x
        sta     ent_yvel_sub,y
        lda     tama_b_proj_yspeed,x
        sta     ent_yvel,y
        lda     tama_b_proj_xspeed_sub,x
        sta     ent_xvel_sub,y
        lda     tama_b_proj_xspeed,x
        sta     ent_xvel,y
        lda     #$1E
        sta     ent_timer,y
        sta     ent_var1,y
        ldx     L0000
        dec     $10
        bpl     tama_b_launch_projectile_loop
        lda     #$00
        sta     $54
tama_b_launch_done:  rts

tama_b_proj_yspeed_sub:  .byte   $44,$00,$2A
tama_b_proj_yspeed:  .byte   $03,$04,$05
tama_b_proj_xspeed_sub:  .byte   $39,$55,$8C
tama_b_proj_xspeed:  .byte   $01,$01,$01
        ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     tama_b_gravity_applied
        lda     #$44
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
tama_b_gravity_applied:  lda     ent_facing,x
        and     #$01
        beq     tama_b_move_left
        ldy     #$08
        jsr     move_right_collide                   ; move_right_collide
        jmp     tama_b_wall_bounce_check

tama_b_move_left:  ldy     #$09
        jsr     move_left_collide                   ; move_left_collide
tama_b_wall_bounce_check:  bcc     tama_b_projectile_done
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
tama_b_projectile_done:  rts

        lda     ent_timer,x
        beq     tama_b_projectile_fall_phase
        dec     ent_timer,x
        jsr     apply_y_speed                   ; apply_y_speed
        lda     ent_facing,x
        and     #$01
        beq     tama_b_projectile_move_left
        jmp     move_sprite_right                   ; move_sprite_right

tama_b_projectile_move_left:  jmp     move_sprite_left               ; move_sprite_left

tama_b_projectile_fall_phase:  ldy     #$12
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     #$01
        sta     ent_anim_state,x
        bcc     tama_b_fall_loop
        lda     #$00
        sta     ent_anim_state,x
        dec     ent_var1,x
        bne     tama_b_clear_anim_frame
        lda     #$3C
        sta     ent_var1,x
        lda     #$A8
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        lda     #$F1
        sta     ent_xvel_sub,x
        lda     #$00
        sta     ent_xvel,x
        jsr     face_player                   ; face_player
tama_b_fall_loop:  lda     ent_facing,x
        and     #$01
        beq     tama_b_move_left_collision
tama_b_move_right_collision_jmp:  ldy     #$1E
        jsr     move_right_collide                   ; move_right_collide
        .byte   $4C
tama_b_move_left_collision_jmp:  .byte   $F3
        .byte   $BD
tama_b_move_left_collision:  ldy     #$1F
        jsr     move_left_collide                   ; move_left_collide
tama_b_clear_anim_frame:  lda     #$00
        sta     ent_anim_frame,x
        rts

; ===========================================================================
; main_item_pickup — item pickup entity (small/large HP, ammo, 1-up, E-tank)
; ===========================================================================
; Two entry points: main_item_pickup (small, hitbox $2C) and .large ($2D).
; Falls with $99, checks player collision. On pickup: marks item
; collected in respawn table ($0150), despawns, and jumps to type-specific
; pickup handler via table at $BDE6/$BDEC. If ent_timer set, item has a
; despawn timer (e.g. from enemy drops).
main_item_pickup:

        ldy     #$2C                    ; small pickup hitbox
        bne     item_pickup_apply_gravity
        ldy     #$2D                    ; large pickup hitbox
item_pickup_apply_gravity:  jsr     move_vertical_gravity               ; apply $99
        jsr     check_player_collision                   ; check if player touches item
        bcs     item_pickup_despawn_timer               ; no collision → timer logic

; --- player picked up item ---
        lda     ent_timer,x                 ; if despawn timer set, skip
        bne     item_pickup_despawn               ; respawn-table marking
        lda     ent_spawn_id,x                 ; mark item collected in respawn table
        pha                             ; ent_spawn_id = spawn index (bit packed)
        and     #$07                    ; low 3 bits = bit position
        tay
        lda     $DEC2,y                 ; bit mask from table
        sta     L0000
        pla                             ; high 5 bits = byte index
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     $0150,y                 ; set collected bit
        ora     L0000
        sta     $0150,y
item_pickup_despawn:  lda     #$00                ; despawn pickup entity
        sta     ent_status,x
        ldy     ent_routine,x                 ; dispatch to item type handler
        lda     tama_b_move_right_collision_jmp,y                 ; via pointer table indexed by
        sta     L0000                   ; AI routine (ent_routine)
        lda     tama_b_move_left_collision_jmp,y                 ; loads pickup-effect routine address
        sta     $01
        jmp     (L0000)                 ; indirect jump to effect handler

; --- no collision: handle despawn timer ---

item_pickup_despawn_timer:  lda     ent_timer,x             ; despawn timer active?
        beq     item_pickup_timer_return               ; no → return
        dec     ent_timer,x                 ; decrement; expired?
        bne     item_pickup_timer_return               ; no → return
        lda     #$00                    ; timer expired: despawn uncollected item
        sta     ent_status,x
item_pickup_timer_return:  rts

; pickup handler pointer table (lo bytes, indexed by ent_routine)

        .byte   $56,$5C,$62,$66,$9D,$AB,$BE,$BE
        .byte   $BE,$BE,$BE,$BE

; --- pickup_hp_large: restore 10 HP to Mega Man's weapon (buster=slot 0) ---
        lda     #$0A                    ; amount = 10 energy ticks
        ldy     #$00                    ; weapon index = 0 (HP)
        beq     item_pickup_apply_energy

; --- pickup_hp_small: restore 2 HP ---
        lda     #$02                    ; amount = 2
        ldy     #$00                    ; weapon = 0 (HP)
        beq     item_pickup_apply_energy

; --- pickup_ammo_large: restore 10 ammo to current weapon ---
        lda     #$0A                    ; amount = 10
        bne     item_pickup_set_weapon_ammo

; --- pickup_ammo_small: restore 2 ammo ---
        lda     #$02
item_pickup_set_weapon_ammo:  ldy     current_weapon                 ; Y = current weapon ID ($A0)
        beq     item_pickup_clear_refill               ; weapon 0 (buster) has no ammo → skip

; --- apply_energy_refill: A=amount, Y=weapon slot index ---
item_pickup_apply_energy:  inc     $58                 ; flag: energy refill active
        sta     $0F                     ; remaining ticks to add
        sty     $0E                     ; target weapon/HP slot
item_pickup_energy_loop:  ldy     $0E                 ; check current energy level
        lda     player_hp,y                   ; ($A2+Y: $A2=HP, $A3+=weapon ammo)
        cmp     #$9C                    ; $9C = max energy (28 units)
        beq     item_pickup_clear_refill               ; already full → done
        lda     player_hp,y                   ; add 1 tick of energy
        clc
        adc     #$01
        sta     player_hp,y
        lda     #SFX_HP_FILL                    ; play refill tick sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        dec     $0F                     ; all ticks applied?
        beq     item_pickup_clear_refill               ; yes → done
item_pickup_wait_frame:  jsr     process_frame_yield_full               ; wait 4 frames between ticks
        lda     $95                     ; (frame counter & 3 == 0)
        and     #$03
        bne     item_pickup_wait_frame
        beq     item_pickup_energy_loop               ; → next tick
item_pickup_clear_refill:  lda     #$00                ; clear refill-active flag
        sta     $58
        rts

; --- pickup_etank: add 1 E-tank (max 9) ---

        lda     #SFX_1UP                    ; play 1-up/E-tank sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     etanks                     ; current E-tanks ($AF)
        cmp     #$09                    ; max 9?
        beq     item_pickup_etank_return               ; yes → don't add more
        inc     etanks                     ; E-tanks += 1
item_pickup_etank_return:  rts

; --- pickup_1up: add 1 extra life (BCD, max 99) ---

        lda     #SFX_1UP                    ; play 1-up sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     lives                     ; $AE ($AE, BCD format)
        cmp     #$99                    ; max 99?
        beq     item_pickup_1up_return               ; yes → done
        inc     lives                     ; $AE += 1
        lda     lives                     ; BCD fixup: if low nibble >= $A
        and     #$0F                    ; carry into high nibble
        cmp     #$0A
        bne     item_pickup_1up_return
        lda     lives                     ; add $10 (next tens digit)
        and     #$F0                    ; clear low nibble
        clc
        adc     #$10
        sta     lives
        cmp     #$A0                    ; overflow past 99? clamp to 99
        bne     item_pickup_1up_return
        lda     #$99
        sta     lives
item_pickup_1up_return:  rts

; ===========================================================================
; main_surprise_box — ? Box (random item container)
; ===========================================================================
; Destructible box that reveals a random pickup when hit. Checks weapon
; collision; on hit, marks collected in respawn table ($0150), despawns the
; weapon, and plays break animation ($71). When break animation completes
; (anim_state == $04): uses RNG ($E5) divided by 100 to select a random item
; from weighted probability table at $BF3F. Spawns the selected item with
; routine ID from $BF4B table and 240-frame despawn timer.
main_surprise_box:

        lda     ent_anim_id,x           ; already broken?
        cmp     #$71                    ; $71 = break animation
        beq     surprise_box_item_spawn               ; yes → handle item spawn
        jsr     check_sprite_weapon_collision                   ; check_sprite_weapon_collision
        bcs     item_pickup_1up_return               ; no hit → return
; --- weapon hit: mark collected, despawn weapon, play break anim ---
        lda     ent_spawn_id,x          ; mark in respawn table ($0150)
        pha
        and     #$07                    ; bit position
        tay
        lda     $DEC2,y                 ; bit mask
        sta     L0000
        pla
        lsr     a                       ; byte index
        lsr     a
        lsr     a
        tay
        lda     $0150,y
        ora     L0000                   ; small pickup hitbox
        sta     $0150,y
        ldy     $10                     ; despawn the weapon that hit
        lda     #$00                    ; apply gravity
        sta     ent_status,y            ; check if player touches item
        lda     #$71                    ; play break animation
        jmp     reset_sprite_anim                   ; reset_sprite_anim

; --- break animation done: spawn random item ---
surprise_box_item_spawn:  lda     ent_anim_state,x ; respawn-table marking
        cmp     #$04                    ; break anim finished?
        bne     surprise_box_return               ; no → wait
        lda     $E5                     ; RNG: $E5 += $E6
        adc     $E6
        sta     $E5                     ; bit mask from table
        sta     L0000
        lda     #$64                    ; divide by 100
        sta     $01
        jsr     divide_8bit                   ; divide_8bit (remainder in $03)
        ldy     #$05                    ; scan probability thresholds
        lda     $03
surprise_box_probability_scan:  cmp     surprise_box_prob_thresholds,y             ; weighted probability table
        bcc     surprise_box_item_selected
        dey
        bne     surprise_box_probability_scan ; despawn pickup entity
surprise_box_item_selected:  lda     surprise_box_item_anim_ids,y             ; item anim ID for selected item
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     surprise_box_item_routine_ids,y                 ; item AI routine
        sta     ent_routine,x           ; AI routine ($0320)
        lda     ent_flags,x             ; clear low 2 flag bits
        and     #$FC
        sta     ent_flags,x             ; indirect jump to effect handler
        lda     #$F0                    ; 240-frame despawn timer
        sta     ent_timer,x
surprise_box_return:  .byte   $60
; surprise box data tables
surprise_box_prob_thresholds:  .byte   $63,$41,$23,$19,$0F,$05 ; probability thresholds (99,65,35,25,15,5)
surprise_box_item_anim_ids:  .byte   $FB,$F9,$FA,$FC,$FE,$FD ; item anim IDs
surprise_box_item_routine_ids:  .byte   $66,$64,$65             ; item AI routine IDs
        .byte   $67                     ; timer expired: despawn uncollected item
        adc     #$68
        lda     ent_anim_state,x
        cmp     #$04
        bne     surprise_box_alt_return
        lda     boss_active
        bmi     surprise_box_boss_deactivate
        lda     $E6
        adc     $E7
        sta     $E7
        sta     L0000
        lda     #$64                    ; amount = 10 energy ticks
        sta     $01                     ; weapon index = 0 (HP)
        jsr     divide_8bit                   ; divide_8bit
        ldy     #$04
        lda     $03
surprise_box_alt_prob_scan:  cmp     surprise_box_alt_prob_thresholds,y ; amount = 2
        bcc     surprise_box_alt_item_selected ; weapon = 0 (HP)
        dey
        bpl     surprise_box_alt_prob_scan
surprise_box_boss_deactivate:  lda     #$00
        sta     ent_status,x            ; amount = 10
        rts

surprise_box_alt_item_selected:  lda     surprise_box_alt_anim_ids,y
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     surprise_box_alt_routine_ids,y ; Y = current weapon ID ($A0)
        sta     ent_routine,x           ; weapon 0 (buster) has no ammo → skip
        lda     #$F0
        sta     ent_timer,x
        lda     #$00                    ; flag: energy refill active
        sta     ent_yvel_sub,x          ; remaining ticks to add
        sta     ent_yvel,x              ; target weapon/HP slot
surprise_box_alt_return:  .byte   $60   ; check current energy level
surprise_box_alt_prob_thresholds:  .byte   $1D,$1B,$0C,$0A,$01 ; ($A2+Y: $A2=HP, $A3+=weapon ammo)
surprise_box_alt_anim_ids:  .byte   $FB,$FC,$F9,$FA,$FE ; $9C = max energy (28 units)
surprise_box_alt_routine_ids:  .byte   $66,$67,$64,$65,$69 ; already full → done

; freespace (unused bytes, fills remainder of bank $1D to $BFFF)
        .byte   $ED,$40,$40
        .byte   $01,$C6,$15,$ED,$00,$A6,$41,$97
        .byte   $11,$59,$54,$93,$44,$CD,$84,$66 ; play refill tick sound
        .byte   $04,$08,$41,$75,$51,$9B,$15,$0B
        .byte   $01,$88,$40,$C0,$01,$18,$00,$57 ; all ticks applied?
        .byte   $80,$88,$40,$02,$00,$02,$10,$0E ; yes → done
        .byte   $01,$C7,$10,$30,$00,$EF,$00,$0F ; wait 4 frames between ticks
        .byte   $50,$AB,$15,$C0,$05,$07,$55,$6F ; (frame counter & 3 == 0)
        .byte   $10,$97,$44,$08,$40,$2B,$01,$23
        .byte   $00,$92,$71,$ED,$15,$67,$54,$7A
        .byte   $54,$A1,$00,$AD,$01,$3A,$00,$DE ; → next tick
        .byte   $04,$82,$51,$90         ; clear refill-active flag
        brk
        asl     $05,x
