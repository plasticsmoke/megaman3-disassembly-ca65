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

        jmp     process_sprites         ; entry point: main entity processing

process_sprites_top_spin_check:  jmp     check_weapon_hit_top_spin_entry  ; entry point: top spin hit check

        jmp     check_weapon_hit_boss_death_sound  ; entry point: boss death handler

        jmp     check_player_hit        ; entry point: player contact damage

process_sprites:  lda     #$55          ; $99 = $00.55 (0.332 px/frame²)
        sta     gravity                     ; set each frame for gameplay physics
        ldx     #$01                    ; start at weapons
        stx     sprite_slot                     ; (skip mega man)
process_sprites_loop_entry:  ldy     #$01  ; Y = spark freeze slot index (1 or 2)
        cpx     spark_freeze_a          ; slot matches spark freeze A?
        beq     process_sprites_spark_freeze_loop                   ; if this sprite slot
        iny                             ; is spark frozen,
        cpx     spark_freeze_b                     ; skip regular processing
        beq     process_sprites_spark_freeze_loop
        lda     ent_status,x                 ; if sprite inactive,
        bpl     process_sprites_next_slot                   ; continue loop
        ldy     #$1D                    ; select $A000~$BFFF bank
        lda     ent_routine,x           ; get main routine index
        cmp     #$E0                    ; routine >= $E0?
        bcc     process_sprites_bank_calc_shift
        ldy     #$12                    ; bank $12 for $E0~$FF
        bne     process_sprites_bank_compare  ; branch always taken (Y != 0)
process_sprites_bank_calc_shift:  lsr     a
        lsr     a                       ; in between $9F and $E0,
        lsr     a                       ; index >> 4 - $06
        lsr     a                       ; meaning bank $04 for $A0~$AF,
        cmp     #$0A                    ; index < $0A means bank $1D (default)
        bcc     process_sprites_bank_compare                   ; $06 for $C0~$CF,
        sec                             ; $07 for $D0~$DF
        sbc     #$06                    ; map $0A-$0D → bank $04-$07
        tay
process_sprites_bank_compare:  cpy     prg_bank
        beq     process_sprites_load_routine_ptr  ; bank already selected? skip switch
        sty     prg_bank                ; update PRG bank shadow
        txa                             ; preserve X
        pha                             ; save entity slot on stack
        jsr     select_PRG_banks        ; switch $A000-$BFFF bank
        pla                             ; restore entity slot
        tax
process_sprites_load_routine_ptr:  ldy     ent_routine,x  ; index into dispatch table
        lda     sprite_main_ptr_lo,y
        sta     L0000                   ; store routine ptr low byte
        lda     sprite_main_ptr_hi,y    ; pointer (low then high)
        sta     $01                     ; store routine ptr high byte
        lda     #$80                    ; push fake return address $8077
        pha                             ; return address
        lda     #$76                    ; (skips some code below)
        pha                             ; entity main rts returns here
        jmp     (L0000)                 ; jump to sprite main

; if spark freeze effect active

process_sprites_spark_freeze_loop:  lda     #$00  ; A = 0
        sta     boss_active,y           ; temporarily clear spark freeze slot
        jsr     check_sprite_weapon_collision  ; recheck weapon collision
        bcs     process_sprites_spark_freeze_wpn  ; collision found → skip to player hit
        txa                             ; A = entity slot X
        ldy     $10                     ; Y = colliding weapon slot
        sta     boss_active,y           ; reapply spark freeze to weapon slot
        lda     #$00                    ; constantly reset
        sta     ent_anim_frame,x                 ; animation counter
        beq     process_sprites_spark_freeze_wpn  ; always branches (A == 0)
        cpx     #$10
        bcc     process_sprites_next_slot                   ; check being hit by weapon
        lda     ent_routine,x                 ; only for enemies with
        beq     process_sprites_next_slot                   ; nonzero main indices
        jsr     check_weapon_hit
process_sprites_spark_freeze_wpn:  lda     ent_hitbox,x                 ; if this sprite can
        bpl     process_sprites_next_slot                   ; cause player damage,
        jsr     check_player_hit        ; check for that
process_sprites_next_slot:  inc     sprite_slot  ; advance loop counter
        ldx     sprite_slot                     ; go to next sprite
        cpx     #$20                    ; all 32 slots processed?
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
        lda     ent_hitbox,x            ; reload hitbox flags
        and     #$20                    ; if shot tink flag on,
        beq     check_weapon_hit_damage_calc  ; tink flag off → apply damage
check_weapon_hit_tink_sound:  lda     #$19                    ; play tink sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     $10                     ; y = tinked weapon slot
        lda     ent_facing,y            ; get weapon facing direction
        eor     #$03                    ; flip horizontal facing
        sta     ent_facing,y
        lda     #$00                    ; set tink Y velocity = -4.0 px/frame
        sta     ent_yvel_sub,y          ; Y velocity sub = 0
        lda     #$FC                    ; Y velocity = -4 (upward)
        sta     ent_yvel,y              ; store upward tink velocity
        lda     #$00                    ; disable collision on tinked shot
        sta     ent_hitbox,y
        lda     #$0F                    ; set tinked shot routine ($0F)
        sta     ent_routine,y
check_weapon_hit_carry_return:  sec                             ; return carry on
        rts

check_weapon_hit_damage_calc:  lda     #$18                    ; play damage sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     prg_bank                ; save current PRG bank
        pha                             ; preserve and select
        stx     $0F                     ; save entity slot to temp
        lda     #$0A                    ; select bank $0A (damage tables)
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $0F                     ; restore X (sprite slot)
        ldy     current_weapon          ; weapon ID as table index
        lda     weapon_damage_ptr_lo,y  ; grab damage table pointer for
        sta     L0000                   ; currently equipped weapon
        lda     weapon_damage_ptr_hi,y  ; high byte of damage ptr
        sta     $01                     ; store to pointer at $00-$01
        ldy     ent_routine,x                 ; if damage for main routine index
        lda     (L0000),y               ; is nonzero, do stuff
        bne     check_weapon_hit_spark_check  ; nonzero damage → check weapon type
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
        txa                             ; A = entity slot (frozen target)
        ldy     $10                     ; Y = weapon slot that collided
        sta     boss_active,y           ; store target slot in spark freeze
        lda     ent_status,y            ; get weapon status
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
        lda     ent_x_px,x              ; copy entity X position to spark
        sta     ent_x_px,y
        lda     ent_x_scr,x                 ; set shock position same
        sta     ent_x_scr,y                 ; as shot sprite's position
        lda     ent_y_px,x              ; copy entity Y position to spark
        sta     ent_y_px,y
check_weapon_hit_spark_freeze_done:  jmp     check_weapon_hit_restore_bank

check_weapon_hit_ack_check:  lda     ent_hp,x                 ; if any hit-ack flags set
        and     #$E0                    ; (bits 7/6/5 = already hit)
        beq     check_weapon_hit_subtract_hp  ; no ack flags → apply damage
        jmp     check_weapon_hit_weapon_despawn  ; already hit this frame, skip damage

check_weapon_hit_subtract_hp:  ldy     ent_routine,x  ; Y = entity routine (damage table index)
        lda     ent_hp,x                 ; subtract health by
        sec                             ; damage table value
        sbc     (L0000),y               ; indexed by main routine index
        bcs     check_weapon_hit_hp_stored  ; no underflow → HP >= 0
        lda     #$00                    ; (no negatives)
check_weapon_hit_hp_stored:  sta     ent_hp,x                 ; store new health value
        bne     check_weapon_hit_hp_zero_check                   ; if nonzero, we're still alive

; dead
        lda     ent_routine,x           ; check entity type before death
        cmp     #$52                    ; if Proto Man ($52 or $53)
        beq     check_weapon_hit_hp_zero_check                   ; don't do normal death —
        cmp     #$53                    ; Proto Man has his own
        beq     check_weapon_hit_hp_zero_check                   ; fly-away exit in main_proto_man
        lda     boss_active             ; check boss active flag
        bpl     check_weapon_hit_death_oam_boss  ; bit 7 clear → use OAM $71
        lda     #$59                    ; bit 7 set → use OAM $59 (normal death)
        bne     check_weapon_hit_death_oam_normal
check_weapon_hit_death_oam_boss:  lda     #$71  ; large death explosion OAM
check_weapon_hit_death_oam_normal:  jsr     reset_sprite_anim                   ; animate enemy's death
        lda     #$00                    ; turn off collision
        sta     ent_hitbox,x            ; disable collision on dead entity
        lda     ent_routine,x           ; check entity routine type
        cmp     #$30                    ; if not bolton and nutton
        bne     check_weapon_hit_death_routine_other  ; not bolton/nutton → use $7A
        lda     #$00                    ; use $00 death routine index
        sta     ent_routine,x                 ; for bolton and nutton
        beq     check_weapon_hit_death_routine_done  ; always branches (A == 0)
check_weapon_hit_death_routine_other:  lda     #$7A                    ; for everything else,
        sta     ent_routine,x                 ; use $7A routine
check_weapon_hit_death_routine_done:  lda     #$90  ; set entity flags to $90 (active)
        sta     ent_flags,x
check_weapon_hit_hp_zero_check:  lda     ent_hp,x                 ; health zero?
        beq     check_weapon_hit_weapon_despawn                   ; don't set boss flags

; not dead
        lda     ent_status,x            ; check entity status
        and     #$40                    ; if sprite is a boss
        bne     check_weapon_hit_boss_stage_check  ; boss entity → special ack handling
        lda     ent_hp,x                ; reload HP for ack flag
        ora     #$20                    ; set bit 5 = hit-ack flag
        sta     ent_hp,x                 ; (prevents double-damage this frame)
        bne     check_weapon_hit_weapon_despawn  ; always branches (HP > 0 here)
check_weapon_hit_boss_stage_check:  lda     stage_id                     ; if stage == Wily 4
        cmp     #STAGE_WILY4                    ; or < Wily 1
        beq     check_weapon_hit_boss_ack_all                   ; this means robot master
        cmp     #STAGE_WILY1                    ; or doc robot bosses
        bcs     check_weapon_hit_weapon_despawn  ; wily stage → single hit-ack only
check_weapon_hit_boss_ack_all:  lda     ent_hp,x  ; reload HP for boss ack
        ora     #$E0                    ; for doc/robot masters,
        sta     ent_hp,x                 ; set all hit-ack flags (bits 7/6/5)
check_weapon_hit_weapon_despawn:  lda     current_weapon  ; check weapon type for despawn
        cmp     #WPN_TOP                    ; if weapon is top spin
        beq     check_weapon_hit_restore_bank                   ; no shot to despawn
        ldy     $10                     ; Y = weapon slot that hit
        lda     #$00                    ; clear weapon status (despawn shot)
        sta     ent_status,y
        lda     current_weapon          ; check for gemini laser
        cmp     #WPN_GEMINI
        bne     check_weapon_hit_restore_bank                   ; if weapon is gemini laser,
        lda     #$00                    ; despawn all three shots
        sta     $0301
        sta     $0302
        sta     $0303
check_weapon_hit_restore_bank:  pla     ; restore saved PRG bank
        sta     prg_bank                     ; restore bank
        jsr     select_PRG_banks        ; reselect PRG bank
        ldx     $0F                     ; restore entity slot from temp
        clc                             ; clear carry (hit confirmed)
        lda     ent_status,x            ; check if entity is a boss
        and     #$40                    ; is sprite a boss?
        bne     check_weapon_hit_boss_hp_display  ; boss → update HP bar display
check_weapon_hit_return:  rts                             ; if not, return

check_weapon_hit_top_spin_contact:  lda     ent_hitbox,x                 ; shot tink flag also
        and     #$20                    ; implies invulnerable
        bne     check_weapon_hit_return                   ; to top spin, return
        jsr     check_player_collision  ; check if player overlaps entity
        bcs     check_weapon_hit_return                   ; player, if not return
        stx     $0F                     ; preserve X
        lda     prg_bank                ; save current PRG bank
        pha                             ; preserve $A000-$BFFF bank
        lda     #$0A                    ; select $0A as new bank
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     $0F                     ; restore X
        ldy     ent_routine,x                 ; y = main ID
        lda     weapon_damage_ptr_lo    ; load buster damage ptr (base weapon)
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
        lda     weapon_damage_ptr_lo_special  ; load Top Spin special damage ptr
        sta     L0000
        lda     weapon_damage_ptr_hi_special
        sta     $01
        jmp     check_weapon_hit_ack_check  ; proceed to damage calculation

check_weapon_hit_boss_hp_display:  lda     ent_hp,x                 ; boss health bits
        and     #$1F                    ; mask to HP (low 5 bits)
        ora     #$80                    ; set bit 7 = "boss was hit" flag
        sta     boss_hp_display                     ; store to boss HP mirror
        and     #$7F                    ; clear dirty flag, check if HP == 0
        beq     check_weapon_hit_boss_death_sound  ; HP == 0 → boss killed
        rts                             ; if not, return

check_weapon_hit_boss_death_sound:  lda     #$F2                    ; SFX $F2 = stop music
        jsr     submit_sound_ID_D9                   ; submit_sound_ID_D9
        lda     #SFX_DEATH                    ; SFX $17 = boss death sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     #$1F                    ; Y = slot $1F (start of enemy range end)
boss_death_loop_entry:  lda     boss_active  ; check boss active flag
        bmi     boss_death_spark_routine  ; bit 7 set → use routine $5B
        lda     #$7A                    ; bit 7 clear → use routine $7A
        bne     boss_death_spawn_child  ; always branches (A != 0)
boss_death_spark_routine:  lda     #$5B
boss_death_spawn_child:  jsr     init_child_entity  ; spawn explosion child entity
        lda     #$80                    ; mark child as active
        sta     ent_status,y
        lda     #$90                    ; set flags (active + render)
        sta     ent_flags,y
        lda     #$00                    ; no collision for explosion
        sta     ent_hitbox,y
        lda     #$10                    ; set death anim routine ($10)
        sta     ent_routine,y
        lda     ent_x_px,x              ; copy boss X to explosion
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy boss Y to explosion
        sta     ent_y_px,y
        lda     $D7E1,y                 ; set X velocity sub from table
        sta     ent_xvel_sub,y
        lda     $D7F1,y                 ; set X velocity from table
        sta     ent_xvel,y
        lda     $D801,y                 ; set Y velocity sub from table
        sta     ent_yvel_sub,y
        lda     $D811,y                 ; set Y velocity from table
        sta     ent_yvel,y
        dey
        cpy     #$0F                    ; loop slots $1F down to $10
        bne     boss_death_loop_entry
        lda     stage_id                ; check for Hard Man stage
        cmp     #STAGE_HARD
        bne     boss_death_weapon_despawn  ; not Hard Man → skip scroll fix
        lda     #$00
        sta     scroll_y                ; reset vertical scroll for Hard Man
boss_death_weapon_despawn:  lda     #$00  ; despawn all weapon slots
        sta     $0301
        sta     $0302
        sta     $0303
        sta     ent_var1                ; clear player var1
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
        sta     ent_anim_id             ; set standing OAM ID
        lda     #$00
        sta     ent_anim_state                   ; reset animation frame
        sta     ent_anim_frame                   ; reset animation counter
boss_death_normal_return:  clc
        rts

; Wily 4 (stage $0F) boss refight — no victory cutscene, just unstun and
; spawn the "boss defeated" entity at the boss's position

boss_death_wily4_handler:  lda     player_state
        cmp     #PSTATE_STUNNED                    ; if player was stunned ($0F),
        bne     boss_death_wily4_unstun ; not stunned → skip state change
        lda     #$00                    ; release to on_ground ($00)
        sta     player_state
boss_death_wily4_unstun:  lda     #$80  ; ent_status[$0F] = active
        sta     $030F                   ; slot $0F status
        lda     #$90                    ; ent_flags[$0F] = $90
        sta     $058F                   ; slot $0F flags
        lda     ent_x_px,x              ; copy boss X pixel to slot $0F
        sta     $036F                   ; ent_x_px[$0F]
        lda     ent_x_scr,x             ; copy boss X screen to slot $0F
        sta     $038F                   ; ent_x_scr[$0F]
        lda     ent_y_px,x              ; copy boss Y pixel to slot $0F
        sta     $03CF                   ; ent_y_px[$0F]
        lda     #$00                    ; clear remaining slot $0F fields
        sta     $03EF                   ; ent_y_scr[$0F] = 0
        sta     $05EF                   ; ent_anim_frame[$0F] = 0
        sta     $05AF                   ; ent_anim_state[$0F] = 0
        sta     $048F                   ; ent_hitbox[$0F] = 0
        sta     $04EF                   ; ent_hp[$0F] = 0
        sta     $044F                   ; ent_yvel_sub[$0F] = 0
        sta     $046F                   ; ent_yvel[$0F] = 0
        sta     $050F                   ; ent_timer[$0F] = 0
        lda     #$F9                    ; set defeated boss anim OAM
        sta     $05CF                   ; ent_anim_id[$0F] = $F9
        lda     #$64                    ; set defeated entity routine
        sta     $032F                   ; ent_routine[$0F] = $64
        jsr     LE11A
boss_death_illegal_bytes:  .byte   $18,$60  ; hidden clc + rts ($18 $60)

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
        and     #$F0                    ; align to 16px grid
        ora     #$08
        sta     ent_x_px,y              ; set child X pixel
        lda     ent_x_scr,x                 ; copy X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; snap child Y to metatile grid center
        and     #$F0                    ; align to 16px grid
        ora     #$08                    ; offset to grid center
        sta     ent_y_px,y              ; set child Y pixel
        lda     #$00                    ; child has no damage flags (harmless)
        sta     ent_hitbox,y            ; clear child hitbox
        sta     ent_status,x                 ; deactivate parent (breaker entity)
        lda     #$FF                    ; ent_spawn_id = $FF (cleanup marker)
        sta     ent_spawn_id,x          ; prevent respawn

; --- mark block destroyed in $0110 bitfield ---
unknown_1B_mark_block_destroyed:  stx     L0000               ; save entity slot
        lda     $13                     ; nametable page (bit 0) << 5
        and     #$01                    ; isolate bit 0 (nametable page)
        asl     a                       ; shift left 5 times total
        asl     a
        asl     a
        asl     a
        asl     a
        sta     $01
        lda     $28                     ; $28 = metatile column index
        pha                             ; save column index
        lsr     a                       ; → Y = byte index into $0110
        ora     $01                     ; combine with page offset
        tay
        pla                             ; restore column; shift left 2
        asl     a
        asl     a                       ; combine with metatile sub-position
        and     #$04                    ; → X = bit index for bitmask_table
        ora     $03                     ; combine with sub-position from $03
        tax                             ; X = bit index into bitmask table
        lda     $0110,y                 ; load current destroyed bits
        ora     $EB82,x                 ; bitmask_table ($80,$40,...,$01)
        sta     $0110,y
        ldx     L0000                   ; restore entity slot
        rts

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
        clc
        adc     #$10                    ; offset 16 pixels below
        cmp     ent_y_px                ; compare with player Y
        bcc     unknown_1B_freeze_anim_timer  ; still above player → freeze anim
        inc     ent_status,x                 ; → state 1 ($99 fall)

; --- state 1: fall with $99 ---
unknown_1B_state_1_gravity:  ldy     #$00  ; hitbox index 0
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     unknown_1B_freeze_anim_timer               ; no landing → freeze anim
        lda     ent_anim_state,x                 ; landed: check anim frame
        cmp     #$04                    ; must be frame 4 (final bounce)
        bne     unknown_1B_freeze_anim_return               ; not ready → return
        lda     #$81                    ; switch to routine $81
        sta     ent_routine,x                 ; (item waiting on ground)
        lda     #$80                    ; active, state 0
        sta     ent_status,x            ; reset status to active, state 0
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        ldy     #$03                    ; check tile at mid-height ahead
        jsr     LE8D6
        lda     $10                     ; solid wall? (bit 4)
        and     #$10                    ; check solid bit
        beq     unknown_1B_check_weapon_type               ; no wall → check weapon type
unknown_1B_state_blocked_advance:  inc     ent_status,x             ; advance state (wall blocked)
        lda     #$00                    ; clear Y speed
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
        rts

unknown_1B_check_weapon_type:  lda     current_weapon                 ; current weapon = Rush Marine ($09)?
        cmp     #WPN_RUSH_MARINE
        bne     unknown_1B_fetch_weapon_oam               ; no → set weapon OAM
        lda     tile_at_feet_max                     ; tile type = water ($80)?
        cmp     #$80
        bne     unknown_1B_state_blocked_advance               ; not water → wall-blocked path
unknown_1B_fetch_weapon_oam:  lda     ent_flags,x             ; set sprite flag bit 0
        ora     #$01                    ; (direction/visibility)
        sta     ent_flags,x
        lda     current_weapon                     ; weapon_id - 6, >> 1 = table index
        sec                             ; subtract Snake weapon ID ($06)
        sbc     #$06                    ; $07→0, $09→1, $0B→2
        lsr     a                       ; divide by 2 for table index
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
        cmp     #$D8                    ; is it Search Snake item?
        bne     unknown_1B_check_early_timer               ; other → skip freeze
        lda     #$00                    ; freeze anim timer
        sta     ent_anim_frame,x        ; hold on frame 0
        lda     ent_anim_state,x        ; check anim state
        bne     unknown_1B_check_offscreen  ; not at idle frame → return
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
        and     #$FC                    ; mask off direction bits 0-1
        sta     ent_flags,x             ; store updated flags
        lda     #$13                    ; OAM $13 = teleport beam
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$04                    ; set anim frame to 4
        sta     ent_anim_state,x                 ; (beam animation start)

; --- state 1: accelerate upward and rise off screen ---
unknown_1B_wait_anim_frame_2:  lda     ent_anim_state,x             ; wait for anim frame 2
        cmp     #$02                    ; (beam fully formed)
        bne     unknown_1B_check_offscreen               ; not yet → return
        lda     #$00                    ; freeze anim at frame 2
        sta     ent_anim_frame,x        ; hold at frame 2
        lda     ent_yvel_sub,x          ; Y speed sub += gravity
        clc                             ; accelerate upward
        adc     gravity
        sta     ent_yvel_sub,x          ; store sub-pixel
        lda     ent_yvel,x              ; propagate carry to whole byte
        adc     #$00
        sta     ent_yvel,x
        jsr     move_sprite_up                   ; move up (unchecked)
        lda     ent_y_scr,x                 ; if Y screen != 0
        beq     unknown_1B_check_offscreen  ; on-screen → return
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
        sbc     ent_x_px,x              ; player X - entity X (low byte)
        pha                             ; save low byte
        lda     ent_x_scr                   ; screen page subtraction
        sbc     ent_x_scr,x
        pla
        bcs     unknown_1B_save_direction               ; player is to the right
        eor     #$FF                    ; negate: absolute distance
        adc     #$01                    ; abs(distance) + 1
        clc                             ; carry clear = player left
unknown_1B_save_direction:  php                         ; save direction (carry)
        cmp     #$03                    ; clamp speed to max 3 px/frame
        bcc     unknown_1B_restore_dir_set_xvel
        lda     #$03                    ; cap at 3 px/frame
unknown_1B_restore_dir_set_xvel:  plp                         ; restore direction
        sta     ent_xvel,x                 ; set X speed = clamped distance
        lda     #$00                    ; sub-pixel = 0
        sta     ent_xvel_sub,x          ; clear X speed sub-pixel
        bcc     unknown_1B_move_left_chase               ; player left → move left
        ldy     #$08                    ; move right with collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     unknown_1B_copy_player_facing

unknown_1B_move_left_chase:  ldy     #$09                ; move left with collision
        jsr     move_left_collide                   ; move_left_collide
unknown_1B_copy_player_facing:  lda     ent_flags               ; copy player facing (bit 6)
        and     #$40                    ; isolate H-flip bit
        sta     L0000
        lda     $0581                   ; slot 1 ent_flags
        and     #$BF                    ; clear existing bit 6
        ora     L0000                   ; merge player facing
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
main_magnet_missile:

        lda     ent_facing,x
        and     #$03                    ; check horizontal direction bits
        beq     magnet_missile_vertical_phase               ; if neither L/R set, vertical phase
        lda     #$97
        cmp     ent_anim_id,x           ; already using horizontal OAM?
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
        bcs     magnet_missile_check_x_dist  ; positive distance → no negate
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
        sbc     ent_y_px,y              ; missile Y - enemy Y
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
magnet_missile_despawn_rts:  rts

; ===========================================================================
; main_gemini_laser — weapon $01: Gemini Laser bouncing projectile
; ===========================================================================
; 3 shots spawned (slots 1-3), each bounces off walls/floors/ceilings.
; ent_timer,x = bounce timer (init $B4, counts down). While >0: collision-checked
; movement with wall/floor bouncing. At 0: free movement, no collision.
; ent_facing,x direction flags: bit0=right, bit1=left, bit2=down, bit3=up.
; Speed set to 3.0 px/frame on both axes after first wall bounce.
main_gemini_laser:

        lda     ent_timer,x                 ; bounce timer
        beq     gemini_laser_direction               ; 0 = free movement phase
        dec     ent_timer,x                 ; decrement bounce timer
        lda     ent_timer,x             ; reload decremented timer
        cmp     gemini_laser_bounce_rts,x  ; compare with per-slot threshold
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
        sta     ent_flags,x             ; store updated flags
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
        sta     ent_facing,x            ; store reversed direction
        and     #$0C                    ; already has vertical component?
        bne     gemini_laser_bounce_rts               ; yes → done (keep existing V dir)
        lda     ent_facing,x                 ; first bounce: add upward direction
        ora     #$08                    ; (bit 3 = up)
        sta     ent_facing,x
        rts

; --- vertical movement (after horizontal move, no wall hit) ---

gemini_laser_vert_dispatch:  lda     ent_facing,x             ; check vertical direction bits
        and     #$0C                    ; (bits 2-3)
        beq     gemini_laser_bounce_rts               ; no vertical component → done
        .byte   $29                     ; AND #$04 (inline byte trick)
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
        sta     ent_anim_id,x
        jmp     move_sprite_down                   ; move down, no collision

gemini_laser_up_check:  lda     ent_timer,x             ; bounce timer active?
        bne     gemini_laser_up_move_collision               ; nonzero → collision-checked move
        lda     #$A1                    ; OAM = $A1 (angled up)
        sta     ent_anim_id,x
        jmp     move_sprite_up                   ; move up, no collision

gemini_laser_up_move_collision:  ldy     #$13                ; collision point: top edge
        jsr     move_up_collide                   ; move up with ceiling detection
        lda     #$A1                    ; OAM = $A1 (angled up)
        sta     ent_anim_id,x

; --- floor/ceiling bounce handler ---
gemini_laser_floor_bounce:  bcc     gemini_laser_bounce_rts           ; C=0: no hit → done
        lda     ent_facing,x                 ; flip vertical direction
        eor     #$0C                    ; (swap bits 2↔3: down↔up)
        sta     ent_facing,x            ; store flipped direction
gemini_laser_bounce_rts:  .byte   $60
        ldy     $B2,x
        bcs     gemini_laser_down_entry
        cpy     #$05
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
        adc     #$20                    ; add $20 sub-pixel increment
        sta     ent_xvel_sub,x          ; store X speed sub-pixel
        lda     ent_xvel,x                 ; carry into whole byte
        adc     #$00
        sta     ent_xvel,x
gemini_laser_fly_maxed:  lda     ent_facing,x             ; move horizontally based on facing
        and     #$01                    ; bit 0: 1=right, 0=left
        beq     gemini_laser_fly_left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     gemini_laser_wobble     ; → Y wobble

gemini_laser_fly_left:  jsr     move_sprite_left               ; move_sprite_left
gemini_laser_wobble:  lda     $95                 ; Y wobble via frame parity
        and     #$01                    ; bit 0 = odd/even frame
        beq     gemini_laser_wobble_even  ; even frame → nudge up
        inc     ent_y_px,x                 ; odd frame: Y += 1 (nudge down)
        jmp     gemini_laser_steer      ; → D-pad steering

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
gemini_laser_exit:  rts

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
        ldy     #$12                    ; collision point: bottom edge
        jsr     move_vertical_gravity   ; apply gravity; C=1 if landed
        bcs     search_snake_set_crawl_speed               ; landed → begin crawling
        lda     ent_timer,x                 ; horizontal move timer
        beq     search_snake_corner_return               ; expired → stop moving, RTS
        dec     ent_timer,x                 ; decrement timer
        jmp     spark_shock_check_facing                   ; move horizontally (shared code)

; --- landed: set crawl speed and transition to state 1 ---

search_snake_set_crawl_speed:  lda     #$00                ; zero sub-pixel speeds
        sta     ent_xvel_sub,x          ; clear X sub-pixel
        sta     ent_yvel_sub,x          ; clear Y sub-pixel
        lda     #$03                    ; crawl speed = 3.0 px/frame both axes
        sta     ent_xvel,x
        sta     ent_yvel,x              ; set Y crawl speed = 3
        lda     ent_facing,x                 ; add "down" to direction (pressing floor)
        ora     #$04                    ; bit 2 = moving down
        sta     ent_facing,x            ; store updated facing
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
        sta     ent_anim_id,x
search_snake_check_offscreen:  lda     ent_y_scr,x             ; Y screen nonzero = offscreen
        bne     spark_shock_clear_status                   ; → despawn
        bcs     search_snake_hit_vert_solid               ; C=1: hit solid vertically

; --- no vertical contact: at surface edge, try corner wrap ---
        lda     ent_facing,x                 ; isolate vertical direction
        and     #$0C                    ; isolate vertical bits (2-3)
        tay                             ; Y = $04(down) or $08(up)
        lda     ent_facing,x                 ; save original direction
        pha
        cpy     #$08                    ; moving up? don't flip horizontal
        beq     search_snake_check_no_flip
        eor     #$03                    ; moving down: flip horizontal
        sta     ent_facing,x                 ; (reverse to probe around floor edge)
search_snake_check_no_flip:  lda     ent_anim_id,x             ; save OAM
        pha
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
        and     #$08                    ; bit 3 = was moving up?
        beq     search_snake_move_horizontal               ; no → hit floor, move horizontal
        lda     #$00                    ; up + ceiling = dead end: despawn
        sta     ent_status,x            ; deactivate entity
        rts

; --- move horizontally along surface ---

search_snake_move_horizontal:  lda     #$A5                ; OAM $A5 = horizontal
        sta     ent_anim_id,x
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
        sta     ent_facing,x            ; store flipped direction
search_snake_surface_return:  rts

; ===========================================================================
; main_spark_shock — Spark Shock weapon projectile
; Travels horizontally in the direction fired. If it hits an enemy
; (ent_status nonzero), freezes in place for ent_timer frames, then despawns.
; ===========================================================================
main_spark_shock:

        lda     ent_status,x                 ; sprite state nonzero?
        and     #$0F                    ; check state low nibble
        bne     spark_shock_dec_timer   ; nonzero = shocking enemy
spark_shock_check_facing:  lda     ent_facing,x                 ; facing left?
        and     #$01                    ; bit 0 = moving right?
        beq     spark_shock_move_left
        jmp     move_sprite_right                   ; else move right

spark_shock_move_left:  jmp     move_sprite_left                   ; move_sprite_left

spark_shock_dec_timer:  dec     ent_timer,x                 ; decrease shock timer
        bne     spark_shock_return                   ; return if not expired
spark_shock_clear_status:  lda     #$00                    ; on timer expiration,
        sta     ent_status,x                 ; despawn (set inactive)
spark_shock_return:  rts

; ===========================================================================
; main_shadow_blade — Shadow Blade weapon projectile
; Moves in up to 8 directions using ent_facing bits 0-3 (R/L/D/U).
; Horizontal and vertical movement handled independently. Returns to
; Mega Man after ent_timer expires (boomerang behavior).
; ===========================================================================
main_shadow_blade:

        lda     ent_facing,x            ; check horizontal direction bits
        and     #$03                    ; bits 0-1 = left/right
        beq     shadow_blade_check_vertical
        and     #$01                    ; bit 0 = moving right?
        beq     shadow_blade_move_left  ; no → move left
        jsr     move_sprite_right                   ; else move right
        jmp     shadow_blade_check_vertical

shadow_blade_move_left:  jsr     move_sprite_left                   ; move_sprite_left
shadow_blade_check_vertical:  lda     ent_facing,x  ; check vertical direction bits
        and     #$0C                    ; bits 2-3 = up/down
        beq     shadow_blade_despawn_check  ; no vertical → despawn check
        and     #$08                    ; bit 3 = moving up?
        beq     shadow_blade_move_down  ; no → move down
        jsr     move_sprite_up                   ; else move up
        jmp     shadow_blade_despawn_check

shadow_blade_move_down:  jsr     move_sprite_down                   ; move_sprite_down
shadow_blade_despawn_check:  lda     ent_y_scr,x                 ; offscreen vertically?
        bne     shadow_blade_offscreen_despawn                   ; despawn
        dec     ent_timer,x                 ; movement timer not expired?
        bne     shadow_blade_rts                   ; return
        lda     ent_status,x            ; check state low nibble
        and     #$0F                    ; already in return phase?
        beq     shadow_blade_flip_direction  ; state 0 → flip direction
shadow_blade_offscreen_despawn:  lda     #$00                    ; set to inactive
        sta     ent_status,x
        rts

shadow_blade_flip_direction:  inc     ent_status,x                 ; indicate flipped state
        lda     #$14                    ; reset movement timer
        sta     ent_timer,x             ; return timer = 20 frames
        lda     ent_facing,x                 ; if not facing up or down,
        and     #$0C                    ; only flip horizontal
        beq     shadow_blade_flip_horizontal
        lda     ent_facing,x                 ; flip vertical
        eor     #$0C                    ; swap up↔down
        sta     ent_facing,x
        and     #$03                    ; has horizontal component?
        beq     shadow_blade_rts
shadow_blade_flip_horizontal:  lda     ent_facing,x                 ; flip horizontal
        eor     #$03                    ; swap right↔left
        sta     ent_facing,x
shadow_blade_rts:  rts

; ===========================================================================
; main_dada — Dada (bouncing robot, Hard Man stage)
; ===========================================================================
; Walks horizontally with $99, bouncing in 3 progressively higher arcs.
; Re-faces player every 3 bounces. ent_timer=bounce index (0-2), ent_var1=face timer.
main_dada:

        lda     ent_status,x                 ; state 0: init
        and     #$0F                    ; isolate low nibble (sub-state)
        bne     dada_movement               ; already init'd → skip
        inc     ent_status,x                 ; state → 1
        lda     #$03                    ; face-player countdown = 3 bounces
        sta     ent_var1,x              ; store face-player countdown
dada_movement:  lda     ent_facing,x    ; check facing direction
        and     #$01
        beq     dada_move_left               ; bit 0 clear → move left
        ldy     #$0A                    ; speed index for rightward walk
        jsr     move_right_collide                   ; move_right_collide
        jmp     dada_apply_gravity

dada_move_left:  ldy     #$0B           ; speed index for leftward walk
        jsr     move_left_collide                   ; move_left_collide
dada_apply_gravity:  ldy     #$0A       ; gravity speed index
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     dada_rts               ; still airborne → return
        lda     ent_timer,x                 ; on landing: load bounce Y speed
        tay                             ; from table indexed by bounce#
        lda     dada_yvel_sub,y                 ; Y speed sub (3 entries)
        sta     ent_yvel_sub,x          ; set bounce Y speed sub
        lda     dada_yvel,y                 ; Y speed whole (3 entries)
        sta     ent_yvel,x              ; set bounce Y speed whole
        dec     ent_var1,x              ; decrement face-player counter
        bne     dada_advance_bounce               ; not zero → skip re-facing
        jsr     face_player                   ; re-face toward player
        lda     #$03                    ; reset counter to 3
        sta     ent_var1,x              ; reset face-player countdown
dada_advance_bounce:  inc     ent_timer,x             ; advance bounce index
        lda     ent_timer,x             ; check if bounce index >= 3
        cmp     #$03                    ; wrap at 3 (cycle 0→1→2→0)
        bcc     dada_rts                ; still in range → return
        lda     #$00
        sta     ent_timer,x             ; reset bounce index to 0
dada_rts:  .byte   $60                  ; rts (encoded as .byte $60)

; bounce Y speeds: sub={$44,$44,$EA}, whole={$03,$03,$07}
; bounce 0: $03.44, bounce 1: $03.44, bounce 2: $07.EA (big hop)
dada_yvel_sub:  .byte   $44,$44,$EA     ; sub-pixel: bounce 0, 1, 2
dada_yvel:  .byte   $03                 ; whole: bounce 0
        .byte   $03                     ; whole: bounce 1
        .byte   $07

; ===========================================================================
; main_potton — Potton (helicopter dropper, Snake Man stage)
; ===========================================================================
; Flies horizontally, reverses on wall hit. When player is within 4 screens
; X-distance, stops and drops a bomb (Copipi child). OAM $23=flying, $24=bomb bay open.
main_potton:
        lda     ent_anim_id,x           ; check current anim
        cmp     #$23                    ; OAM $23 = flying
        beq     potton_collision               ; already dropping → skip movement
        lda     ent_facing,x            ; check facing for movement
        and     #$01                    ; bit 0 = facing right
        beq     potton_move_left
        ldy     #$08                    ; speed index for rightward fly
        jsr     move_right_collide                   ; move_right_collide
        jmp     potton_collision

potton_move_left:  ldy     #$09
        jsr     move_left_collide                   ; move_left_collide
potton_collision:  bcc     potton_state_check  ; no wall hit → skip reversal
        lda     ent_facing,x            ; hit wall: flip facing
        eor     #$03                    ; toggle bits 0+1
        sta     ent_facing,x
potton_state_check:  lda     ent_status,x             ; state check
        and     #$0F                    ; isolate low nibble (sub-state)
        bne     potton_check_drop               ; state 1+ → check bomb drop anim
        jsr     entity_x_dist_to_player                   ; state 0: check range to player
        cmp     #$04                    ; < 4 screens away?
        bcs     potton_rts               ; no → return
        inc     ent_status,x                 ; state → 1 (stop and drop)
        lda     #$23                    ; set OAM $23 (propeller stop anim)
        bne     potton_reset_anim               ; → reset_sprite_anim
potton_check_drop:  lda     ent_anim_id,x             ; already showing bomb bay ($24)?
        cmp     #$24                    ; OAM $24 = bomb bay open
        beq     potton_rts               ; yes → done
        lda     ent_anim_state,x        ; check anim sequence position
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
        sta     ent_x_px,y              ; store child X pixel
        lda     ent_x_scr,x             ; copy X screen
        sta     ent_x_scr,y             ; store child X screen
        lda     ent_y_px,x              ; load parent Y pixel
        clc                             ; offset +$11 below parent
        adc     #$11
        sta     ent_y_px,y              ; store child Y pixel
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$25                    ; OAM $25 = Copipi (bomb)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$04                    ; AI routine = $04 (falling bomb)
        sta     ent_routine,y
        lda     #$C0                    ; dmg flags: $C0 = hurts player + hittable
        sta     ent_hitbox,y
potton_spawn_rts:  rts

        lda     ent_status,x            ; Copipi fall AI: check state
        and     #$0F                    ; isolate low nibble
        bne     potton_vertical_move    ; already init'd → skip
        jsr     reset_gravity                   ; reset_gravity
        inc     ent_status,x            ; state → 1
potton_vertical_move:  ldy     #$08     ; gravity speed index
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     hammer_joe_init_rts     ; not landed → return
        lda     #$71                    ; OAM $71 = explosion
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear AI routine
        sta     ent_routine,x           ; set to routine 0 (despawn)
hammer_joe_init_rts:  rts

; ===========================================================================
; main_hammer_joe — Hammer Joe (shielded enemy, throws hammers)
; ===========================================================================
; Cycle: shield up (invulnerable, timer ent_timer=$1E) → shield open (OAM $27,
; vulnerable) → throw hammer at anim frame $0A → shield close → repeat.
; Tracks player facing. ent_var1 = opened-once flag (toggles vulnerability).
main_hammer_joe:

        lda     ent_status,x                 ; state 0: init
        and     #$0F
        bne     hammer_joe_shield_check               ; already init'd → skip
        sta     ent_var1,x                 ; clear opened flag
        lda     #$1E                    ; shield timer = $1E (30 frames)
        sta     ent_timer,x             ; set shield timer
        jsr     set_sprite_hflip        ; set sprite H-flip from facing
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
        rts                             ; return (let anim play one frame)

hammer_joe_dec_timer:  dec     ent_timer,x             ; decrement shield timer
hammer_joe_face_player:  lda     ent_facing,x             ; save old facing, re-face player
        pha
        jsr     face_player                   ; face_player
        pla                             ; restore old facing
        cmp     ent_facing,x            ; compare with new facing
        beq     hammer_joe_open_check   ; same → skip flip
        lda     ent_flags,x             ; facing changed: toggle H-flip
        eor     #$40
        sta     ent_flags,x             ; store updated flags
hammer_joe_open_check:  lda     ent_anim_id,x             ; only act when shield is open ($27)
        cmp     #$27
        bne     hammer_joe_rts               ; closed → return
        lda     ent_var1,x                 ; first time opening this cycle?
        bne     hammer_joe_toggle_vuln               ; no → skip vulnerability toggle
        lda     ent_hitbox,x                 ; toggle vulnerability bits ($60)
        eor     #$60                    ; now hittable + hurts player
        sta     ent_hitbox,x            ; store updated hitbox
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
        pha                             ; save X pixel on stack
        lda     ent_x_scr,x             ; add screen offset with carry
        adc     hammer_joe_hammer_x_scr,y
        ldy     L0000                   ; restore child slot
        sta     ent_x_scr,y             ; store child X screen
        pla
        sta     ent_x_px,y              ; store child X pixel
        lda     ent_y_px,x                 ; hammer Y = Joe Y - 6 (arm height)
        sec
        sbc     #$06                    ; subtract 6 (arm height)
        sta     ent_y_px,y              ; store child Y pixel
        lda     #$33                    ; hammer X speed = $03.33 (3.2 px/f)
        sta     ent_xvel_sub,y          ; store hammer X speed sub
        lda     #$03                    ; X speed whole = $03
        sta     ent_xvel,y              ; store hammer X speed whole
        lda     #$28                    ; OAM $28 = hammer sprite
        jsr     init_child_entity                   ; init_child_entity
        lda     #$2D                    ; AI routine = $2D (arcing projectile)
        sta     ent_routine,y
        lda     #$C0                    ; dmg flags: hurts player + hittable
        sta     ent_hitbox,y            ; store child hitbox
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
hammer_joe_spawn_rts:  .byte   $60      ; rts (encoded as .byte $60)

; hammer X offset: right=$0013, left=$FFED (-19)
hammer_joe_hammer_x_off:  .byte   $13
hammer_joe_hammer_x_scr:  brk
        sbc     proto_man_x_check_byte
        brk
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
        rts

hammer_joe_check_walk_dir:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = facing right
        beq     hammer_joe_move_left
        ldy     #$00                    ; move right with wall collision
        jsr     move_right_collide                   ; move_right_collide
        jmp     hammer_joe_wall_check   ; skip to wall check

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
        bne     hammer_joe_jump_landing ; not crouch → check landing
        lda     ent_anim_state,x                 ; wait until anim reaches frame 2
        cmp     #$02                    ; (crouch anim finished)
        bne     hammer_joe_jump_landing ; not at frame 2 → check landing
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
        sta     ent_y_px,y              ; store child Y pixel
        lda     #$01                    ; child HP = 1
        sta     ent_hp,y                ; store child HP
        lda     #$6C                    ; init child entity with OAM $6C
        jsr     init_child_entity                   ; (projectile sprite)
        lda     #$C2                    ; child damage flags = $C2
        sta     ent_hitbox,y                 ; (hurts player + hittable + invincible)
        lda     #$44                    ; child Y speed = $03.44
        sta     ent_yvel_sub,y                 ; (falling arc projectile)
        lda     #$03
        sta     ent_yvel,y              ; store child Y speed whole
        lda     #$09                    ; child AI routine = $09
        sta     ent_routine,y           ; store child AI routine
        jmp     hammer_joe_done               ; done

hammer_joe_jump_landing:  lda     ent_anim_id,x             ; if OAM != $6B (jump anim),
        cmp     #$6B                    ; skip landing logic
        bne     hammer_joe_done         ; not jump anim → skip landing
        lda     $0F                     ; check landed flag (saved from
        and     #$01                    ; move_vertical_gravity earlier)
        beq     hammer_joe_done               ; not landed yet, keep falling
        lda     #$6D                    ; switch to walk-toward anim ($6D)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        jsr     face_player                   ; face player after landing
        lda     #$00                    ; set X speed = $02.00
        sta     ent_xvel_sub,x                 ; (walk toward player)
        lda     #$02                    ; X speed whole = $02
        sta     ent_xvel,x              ; store walk X speed
        lda     #$10                    ; set walk timer = $10 frames
        sta     ent_timer,x                 ; (walk toward player briefly)
        inc     ent_var1,x                 ; set post-land walk flag (ent_var1=1)
hammer_joe_done:  rts

; bubukan child projectile X offset table (read as data at $8DC4)
; also serves as auto_walk_spawn_done trampoline for child entity

hammer_joe_xoffset:  .byte   $20        ; right offset = $20 pixels
hammer_joe_xscreen_offset:  brk
        cpx     #$FF                    ; left offset = $FFE0 (-32)

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
main_jamacy:

        lda     ent_status,x                 ; check state (bits 0-3)
        and     #$0F
        bne     jamacy_check_direction               ; skip init
jamacy_set_yvel:  sta     ent_yvel,x    ; Y speed whole = 0
        lda     #$C0                    ; Y speed sub = $C0
        sta     ent_yvel_sub,x          ; store Y speed sub
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
        beq     jamacy_move_down        ; bit 0 clear → move down
        jsr     move_sprite_up                   ; move up at current Y speed
        jmp     jamacy_apply_movement   ; skip to timer decrement

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

jamacy_done:  rts                       ; table: $60 (routine $15)

        bvs     jamacy_set_yvel         ; table: $70 (routine $16)
        brk
        .byte   $03
        and     #$0F
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
        sta     ent_xvel_sub,x          ; store negated X speed sub
        lda     ent_xvel,x              ; negate X speed whole (high byte)
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x              ; store negated X speed whole
bombflier_advance_table:  inc     ent_var1,x             ; advance table index
        lda     ent_var1,x                 ; wrap at 14 entries (0-13)
        cmp     #$0E
        bne     bombflier_reset_countdown  ; not at limit → set countdown
        lda     #$00                    ; reset table index to 0
        sta     ent_var1,x              ; store reset table index
bombflier_reset_countdown:  lda     #$05                ; reset speed countdown = 5 frames
        sta     ent_timer,x                 ; (hold each speed for 5 frames)
bombflier_apply_movement:  dec     ent_timer,x             ; decrement speed countdown
        lda     ent_y_sub,x                 ; apply Y speed manually (16-bit)
        clc
        adc     ent_yvel_sub,x          ; add Y speed sub to Y sub
        sta     ent_y_sub,x
        lda     ent_y_px,x              ; add Y speed whole to Y pixel
        adc     ent_yvel,x
        sta     ent_y_px,x              ; store updated Y pixel
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
        lda     #$02                    ; X speed whole = $02
        sta     ent_xvel,x              ; store walking bomb X speed
        lda     #$48                    ; switch to walking bomb anim ($48)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
bombflier_penpen_exit:  rts

bombflier_walking_state:  lda     ent_routine,x             ; if AI routine != $0A (not PenPen),
        cmp     #$0A                    ; skip to bomb flier homing movement
        bne     bombflier_horiz_move
        lda     ent_anim_id,x                 ; if already on walking anim ($49),
        cmp     #$49                    ; go straight to walking movement
        beq     bombflier_walk_direction  ; yes → walk in direction
        lda     ent_anim_frame,x                 ; wait for current anim to finish
        ora     ent_anim_state,x                 ; (timer=0 AND frame=0)
        bne     bombflier_anim_wait               ; still animating, return
        lda     #$49                    ; switch to walking bomb anim ($49)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_hitbox,x                 ; set damage flags: hurts player +
        ora     #$C3                    ; hittable + invincible ($C3)
        sta     ent_hitbox,x                 ; (walking bomb is dangerous)
bombflier_anim_wait:  rts

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
        sta     $03                     ; speed high byte = $02
        jsr     calc_homing_velocity                   ; calculate homing direction + speed
        lda     $0C                     ; set direction flags from homing result
        sta     ent_facing,x
        inc     ent_status,x                 ; advance state (-> homing flight)
        rts

bombflier_horiz_move:  lda     ent_facing,x             ; move horizontally based on direction
        and     #$01                    ; bit 0 = right
        beq     bombflier_homing_left   ; facing left -> move left
        jsr     move_sprite_right                   ; move right
        jmp     bombflier_vert_move

bombflier_homing_left:  jsr     move_sprite_left               ; move left
bombflier_vert_move:  lda     ent_facing,x             ; move vertically based on direction
        and     #$08                    ; bit 3 = up
        beq     bombflier_move_down     ; bit 3 clear -> move down
        jmp     move_sprite_up                   ; move up

bombflier_move_down:  .byte   $4C,$59,$F7         ; move down

; sinusoidal speed indirection table (14 entries, indexes into Y/X speed tables)
bombflier_speed_index:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$01
        .byte   $02,$03,$04,$05,$06,$07

; Y speed table (16 signed 16-bit values, indexed by indirection * 2)
bombflier_yspeed_lo:  .byte   $CD
bombflier_yspeed_table:  .byte   $FE,$E5,$FE,$27,$FF,$8B,$FF,$00
        .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE

; X speed table (16 signed 16-bit values, indexed by indirection * 2)
bombflier_xspeed_lo:  .byte   $00
bombflier_xspeed_table:  .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE,$CD
        .byte   $FE,$E5,$FE,$27
        .byte   $FF
        .byte   $8B
        .byte   $FF

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
main_cloud_platform:
        lda     ent_status,x                 ; get entity state
        and     #$0F                    ; isolate state bits
        bne     cloud_platform_movement               ; state 1+: already active, skip to movement
        jsr     LFAF6                   ; state 0: check if player standing on platform
        bcc     cloud_platform_activate               ; player on top -> activate
        rts                             ; not standing on it -> wait

cloud_platform_activate:  inc     ent_status,x             ; advance to state 1 (active flying)
        lda     #$CC                    ; Y speed sub = $CC
        sta     ent_yvel_sub,x                 ; rise speed $00.CC (~0.8 px/frame upward)
        lda     #$00                    ; Y speed whole = $00 (no whole-pixel rise)
        sta     ent_yvel,x                 ; Y speed whole = $00
        lda     #$02                    ; initial direction = left ($02)
        sta     ent_facing,x            ; set facing direction
        lda     #$10                    ; movement timer = 16 frames (first segment)
        sta     ent_timer,x
        lda     #$B4                    ; lifetime timer = 180 frames ($B4)
        sta     ent_var2,x
        lda     #$E8                    ; Y position = $E8 (start near bottom of screen)
        sta     ent_y_px,x              ; set Y pixel position
        lda     ent_x_px,x                 ; save X aligned to 16px metatile boundary
        and     #$F0                    ; mask off low nibble
        ora     #$08                    ; center within metatile (+8)
        sta     ent_var3,x                 ; store saved X for child spawn position
        lda     #$00                    ; zero value for clearing sub-pixel
        sta     ent_x_sub,x                 ; clear X sub-pixel
        lda     ent_flags,x                 ; clear sprite flag bit 2
        and     #$FB                    ; clear bit 2
        sta     ent_flags,x
cloud_platform_movement:  jsr     move_sprite_up               ; rise upward (apply Y speed)
        lda     ent_flags,x                 ; check tile collision flag (bit 5)
        and     #$20                    ; bit 5 = tile collision flag
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
        sta     ent_routine,y           ; set AI routine
        lda     #$81                    ; active ($80) + state 1 (already flying)
        sta     ent_status,y            ; set status: active + state 1
        lda     ent_flags,y                 ; set sprite flags: bit 5 (tile check) + bit 0
        ora     #$21                    ; set bit 5 (tile check) + bit 0
        sta     ent_flags,y
        lda     #$0F                    ; damage flags = $0F (invulnerable platform)
        sta     ent_hitbox,y
        lda     ent_var3,x                 ; copy saved X from parent
        sta     ent_x_px,y                 ; set child X position
        sta     ent_var3,y                 ; propagate saved X for future clones
        lda     ent_x_scr,x                 ; copy X screen from parent
        sta     ent_x_scr,y
        lda     #$00                    ; reset direction index to 0
        sta     ent_var1,y              ; direction index = 0
        sta     ent_y_scr,y                 ; Y screen = 0
        sta     ent_yvel,y                 ; Y speed whole = 0
        sta     ent_xvel,y                 ; X speed whole = 0
        lda     #$CC                    ; Y speed sub = $CC (rise speed $00.CC)
        sta     ent_yvel_sub,y
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y          ; set X speed sub-pixel
        lda     #$E8                    ; Y position = $E8 (bottom of screen)
        sta     ent_y_px,y              ; set child Y position
        lda     #$02                    ; initial direction = left ($02)
        sta     ent_facing,y            ; set child facing direction
        lda     #$10                    ; movement timer = 16 frames
        sta     ent_timer,y
        lda     #$B4                    ; lifetime timer = 180 frames ($B4)
        sta     ent_var2,y              ; set child lifetime timer
        lda     #$E8                    ; Y position = $E8 (redundant store)
        sta     ent_y_px,y              ; set child Y position (redundant)
cloud_platform_done:  rts

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
        sta     ent_facing,y            ; set child facing direction
        lda     ent_x_px,x                 ; copy parent X position to child
        sta     ent_x_px,y              ; set child X position
        lda     ent_x_scr,x                 ; copy parent X screen to child
        sta     ent_x_scr,y             ; set child X screen
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
        rts

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
main_unknown_0C:

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
unknown_0C_state_return:  rts

unknown_0C_horizontal_walk:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     unknown_0C_move_left               ; facing left -> move left
        ldy     #$08                    ; collision offset for right
        jsr     move_right_collide                   ; move right with wall check
        jmp     unknown_0C_check_wall_collision  ; check wall collision result

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
unknown_0C_horizontal_return:  rts

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
main_giant_springer:

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
        sta     ent_x_px,y              ; set child X position
        lda     ent_x_scr,x                 ; copy parent X screen
        sta     ent_x_scr,y             ; set child X screen
        lda     ent_y_px,x                 ; parent Y pixel
        sbc     #$17                    ; spawn 23 pixels above parent
        sta     ent_y_px,y              ; set child Y position
        lda     #$BD                    ; child entity type: small springer
        jsr     init_child_entity                   ; initialize child entity in slot Y
        lda     #$75                    ; AI routine index
        sta     ent_routine,y           ; set child AI routine
        lda     #$C0                    ; damage flags: hurts player + hittable
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y                ; set child HP
        lda     #$80                    ; spawn group marker $80 (for child counting)
        sta     ent_spawn_id,y          ; set child spawn group
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
        sta     ent_timer,x             ; clear walk timer
giant_springer_wait_rts:  rts

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
        sta     ent_timer,x             ; clear walk frame counter
giant_springer_rts:  rts

; -----------------------------------------------------------------------------
; count_springer_children — counts active small springers (spawn group $80)
; -----------------------------------------------------------------------------
; Scans enemy slots $10-$1F for entities with ent_spawn_id == $80.
; Sets ent_var2,x = $00 if no children found (allow spawning),
;              = $FF if any children exist (block spawning).
; -----------------------------------------------------------------------------

giant_springer_count_kids:  lda     #$00                ; child count = 0
        sta     L0000                   ; store to temp (child count)
        lda     #$80                    ; target spawn group marker
        sta     $01                     ; store spawn group marker $80
        ldy     #$1F                    ; start from slot 31 (last enemy slot)
giant_springer_scan_loop:  lda     ent_status,y             ; is slot active? (bit 7)
        bmi     giant_springer_check_kid               ; yes → check if it's a springer child
giant_springer_scan_next:  dey                         ; next slot
        cpy     #$0F                    ; scanned down to slot $10?
        bne     giant_springer_scan_loop               ; no → keep scanning
        lda     L0000                   ; child count
        bne     giant_springer_kids_exist               ; if > 0, block spawning
        lda     #$00                    ; no children: allow spawning
        sta     ent_var2,x              ; var2 = 0 (allow spawning)
        rts

giant_springer_kids_exist:  lda     #$FF                ; children exist: block spawning
        sta     ent_var2,x              ; var2 = $FF (block spawning)
        rts

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
        clc
        adc     ent_var2,x                 ; + fine adjustment → combined direction
        tay                             ; Y = table index (0-31)
        lda     chibee_yvel_sub_table,y                 ; Y speed sub from direction table
        sta     ent_yvel_sub,x          ; store Y velocity sub
        lda     chibee_yvel_whole_table,y                 ; Y speed whole from direction table
        sta     ent_yvel,x              ; store Y velocity whole
        lda     chibee_xvel_sub_table,y                 ; X speed sub from direction table
        sta     ent_xvel_sub,x          ; store X velocity sub
        lda     chibee_xvel_whole_table,y                 ; X speed whole from direction table
        sta     ent_xvel,x              ; store X velocity whole
        lda     chibee_sprite_oam_table,y                 ; OAM ID from direction table
        sta     ent_anim_id,x           ; store animation/OAM ID
        lda     ent_flags,x                 ; sprite flags
        and     #$BF                    ; clear facing bit (bit 6)
        ora     chibee_facing_flag_table,y                 ; OR in facing flag from table ($00 or $40)
        sta     ent_flags,x             ; store updated flags
        lda     ent_var1,x                 ; reload movement duration
        sta     ent_timer,x                 ; reset countdown timer
chibee_apply_movement:  dec     ent_timer,x             ; decrement movement timer
        lda     #$00                    ; sign-extend init = 0
        sta     L0000                   ; store to temp
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
        lda     #$00                    ; sign-extend init = 0
        sta     L0000                   ; store to temp
        lda     ent_yvel,x                 ; Y speed whole
        bpl     chibee_apply_y_movement               ; positive -> skip
        dec     L0000                   ; negative -> $00 = $FF
chibee_apply_y_movement:  lda     ent_y_sub,x             ; Y sub-pixel
        clc
        adc     ent_yvel_sub,x                 ; + Y speed sub
        sta     ent_y_sub,x             ; store Y sub-pixel
        lda     ent_y_px,x                 ; Y pixel
        adc     ent_yvel,x                 ; + Y speed whole + carry
        sta     ent_y_px,x              ; store Y pixel
        lda     ent_y_scr,x                 ; Y screen
        adc     L0000                   ; + sign extension + carry
        beq     chibee_movement_done               ; still on screen 0 -> done
        lda     #$00                    ; off-screen vertically
        sta     ent_status,x                 ; deactivate entity
chibee_movement_done:  .byte   $60

; -- chibee direction lookup tables (16 directions x 2 sets = 32 entries each) --
; Indexed by combined direction from track_direction_to_player.
; Directions: 0=up, 2=up-right, 4=right, 6=down-right, 8=down, etc. (clockwise)
chibee_yvel_sub_table:  .byte   $00,$27,$4B,$3D,$00,$C3,$B5,$D9 ; Y speed sub (set 1)
        .byte   $00,$D0,$B5,$C3,$00,$3D,$4B,$27
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
        .byte   $C1,$C1,$C0,$C0,$BF,$BF,$BE,$BE
        .byte   $41,$41,$41,$41,$41,$41,$41,$41 ; OAM ID per direction (set 2)
        .byte   $41,$41,$41,$41,$41,$41,$41,$41
chibee_facing_flag_table:  .byte   $00,$00,$40,$40,$40,$40,$40,$40 ; facing flag ($00=right, $40=left) (set 1)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$40,$40,$40,$40,$40 ; facing flag (set 2)
        .byte   $00,$00,$00,$00,$00
        brk
        brk
        brk
        jsr     check_player_collision  ; check player collision
        bcc     chibee_deactivate       ; no collision: deactivate
        lda     #$00                    ; sign-extend init = 0
        sta     L0000                   ; store to temp
        lda     ent_xvel,x              ; X velocity whole
        bpl     chibee_xvel_sign_extend ; positive: skip sign extend
        dec     L0000                   ; negative: sign extend to $FF
chibee_xvel_sign_extend:  lda     ent_x_sub,x  ; X sub-pixel
        clc
        adc     ent_xvel_sub,x          ; + X velocity sub
        sta     ent_x_sub,x             ; store X sub-pixel
        lda     ent_x_px,x              ; X pixel
        adc     ent_xvel,x              ; + X velocity whole + carry
        sta     ent_x_px,x              ; store X pixel
        lda     ent_x_scr,x             ; X screen
        adc     L0000                   ; + sign extension + carry
        sta     ent_x_scr,x             ; store X screen
        lda     #$00                    ; sign-extend init = 0
        sta     L0000                   ; store to temp
        lda     ent_yvel,x              ; Y velocity whole
        bpl     chibee_yvel_sign_extend ; positive: skip sign extend
        dec     L0000                   ; negative: sign extend to $FF
chibee_yvel_sign_extend:  lda     ent_y_sub,x  ; Y sub-pixel
        clc
        adc     ent_yvel_sub,x          ; + Y velocity sub
        sta     ent_y_sub,x             ; store Y sub-pixel
        lda     ent_y_px,x              ; Y pixel
        adc     ent_yvel,x              ; + Y velocity whole + carry
        sta     ent_y_px,x              ; store Y pixel
        lda     ent_y_scr,x             ; Y screen
        adc     L0000                   ; + sign extension + carry
        beq     chibee_return           ; still on screen 0: done
chibee_deactivate:  lda     #$00        ; off-screen vertically
        sta     ent_status,x            ; deactivate entity
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

        lda     ent_status,x            ; entity status
        and     #$0F                    ; isolate state nibble
        bne     electric_gabyoall_main_loop               ; state 1+ → main logic

; --- state 0: init ---
        inc     ent_status,x                 ; advance to state 1
        lda     #$3C                    ; electric toggle timer = 60 frames
        sta     ent_timer,x

; --- main loop: horizontal movement + player hit checks ---
electric_gabyoall_main_loop:  lda     ent_hitbox,x  ; load hitbox
        and     #$E0                    ; (keep flags, reset damage)
        sta     ent_hitbox,x
        ldy     ent_routine,x           ; variant index
        lda     ent_y_px,x              ; save Y pixel for later restore
        pha
        clc
        adc     electric_gabyoall_offset_table,y  ; + upper Y offset per variant
        sta     ent_y_px,x
        jsr     check_player_hit        ; check contact with player
        lda     ent_facing,x            ; facing direction
        and     #$01                    ; isolate direction bit
        beq     electric_gabyoall_move_left  ; bit 0 clear: moving left
        ldy     #$08                    ; speed index for move right
        jsr     move_right_collide                   ; move_right_collide
        jmp     electric_gabyoall_clear_flip

electric_gabyoall_move_left:  ldy     #$09  ; speed index for move left
        jsr     move_left_collide                   ; move_left_collide
electric_gabyoall_clear_flip:  lda     ent_flags,x  ; load flags
        and     #$BF                    ; clear H-flip (ball symmetric)
        sta     ent_flags,x
        bcc     electric_gabyoall_offset_table  ; no wall hit: vertical check
        lda     ent_facing,x            ; wall hit: reverse direction
        eor     #$03                    ; toggle bits 0-1
        sta     ent_facing,x
        bne     electric_gabyoall_toggle_electric  ; always taken: to electric toggle
electric_gabyoall_offset_table:  .byte   $BC
        .byte   $20
electric_gabyoall_variant_index:  .byte   $03
        .byte   $BD
electric_gabyoall_anim_id_table:  cpy     #$03
electric_gabyoall_hitbox_table:  clc
        adc     electric_gabyoall_variant_index,y  ; + lower Y offset per variant
        sta     ent_y_px,x              ; store adjusted Y pixel
        jsr     check_player_hit        ; check lower hitbox contact
        ldy     #$08                    ; gravity speed index
        jsr     move_vertical_gravity   ; apply gravity + floor check
        ldy     ent_facing,x            ; facing direction index
        lda     tile_at_feet_max,y      ; tile at feet in that direction
        bne     electric_gabyoall_toggle_electric  ; solid floor: keep direction
        lda     ent_facing,x            ; no floor: reverse at ledge
        eor     #$03                    ; toggle direction bits
        sta     ent_facing,x
electric_gabyoall_toggle_electric:  lda     ent_timer,x  ; check electric toggle timer
        bne     electric_gabyoall_timer_decrement  ; timer active: decrement
        lda     ent_anim_state,x        ; animation state
        ora     ent_anim_frame,x        ; OR with animation frame
        bne     electric_gabyoall_restore_y  ; animation busy: restore Y
        ldy     ent_routine,x           ; variant index
        lda     ent_anim_id,x           ; current animation ID
        cmp     electric_gabyoall_anim_id_table,y  ; at electric anim ID?
        bne     electric_gabyoall_inc_anim_id  ; no: increment anim
        sec
        sbc     #$02                    ; revert to non-electric anim
        sta     ent_anim_id,x           ; store normal anim ID
        lda     #$3C                    ; reset timer = 60 frames
        sta     ent_timer,x
        bne     electric_gabyoall_reset_anim_frame  ; always taken: reset anim frame
electric_gabyoall_timer_decrement:  dec     ent_timer,x  ; decrement electric timer
        bne     electric_gabyoall_restore_y  ; timer not zero: restore Y
electric_gabyoall_inc_anim_id:  inc     ent_anim_id,x  ; advance to next anim ID
electric_gabyoall_reset_anim_frame:  lda     #$00  ; reset animation frame to 0
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
electric_gabyoall_restore_y:  pla       ; restore original Y pixel
        sta     ent_y_px,x
        ldy     ent_routine,x           ; variant index
        lda     ent_anim_id,x           ; current animation ID
        cmp     electric_gabyoall_anim_id_table,y  ; at electric anim ID?
        bne     electric_gabyoall_apply_hitbox  ; no: skip hitbox update
        lda     ent_hitbox,x            ; load current hitbox
        and     #$E0                    ; keep upper 3 flag bits
        ora     electric_gabyoall_hitbox_table,y  ; OR in electric damage bits
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
        lda     ent_status,x            ; entity status
        and     #$0F                    ; isolate state nibble
        bne     junk_block_spawn_check                   ; already initialized
        jsr     entity_x_dist_to_player ; X distance to player
        cmp     #$3C                        ; within 60 px?
        bcs     junk_block_rts                   ; no -> rts
        inc     ent_status,x            ; advance to state 1
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
        sta     ent_x_px,y              ; copy X pixel to child
        lda     ent_x_scr,x             ; copy X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x                  ; copy parent Y to child
        sta     ent_y_px,y              ; copy Y pixel to child
        lda     #$62
        sta     ent_routine,y           ; child routine = $62
        lda     ent_hitbox,x            ; copy parent hitbox
        sta     ent_hitbox,y                ; copy parent hitbox
        lda     #$B2
        sta     ent_flags,y                 ; child sprite flags
        lda     #$5B                    ; parent spawn cooldown = $5B
        sta     ent_var1,x                  ; parent spawn cooldown = $5B
        lda     #$AB                    ; child Y velocity sub = $AB
        sta     ent_yvel_sub,y              ; child Y speed = $FF.AB (upward)
        lda     #$FF
        sta     ent_yvel,y              ; child Y velocity = $FF (upward)
        lda     #$08                    ; child HP = 8
        sta     ent_hp,y                    ; child HP = 8
junk_block_dec_cooldown:  dec     ent_var1,x              ; decrement spawn cooldown
junk_block_rts:  rts

        ldy     #$1E                    ; gravity speed index
        jsr     move_vertical_gravity   ; apply gravity + floor check
        bcs     junk_block_sprite_flags ; landed on ground: skip
        lda     ent_y_px,x              ; current Y pixel
        cmp     #$70                    ; reached target Y $70?
        bcc     junk_block_sprite_flags ; above target: skip
        lda     #$90                    ; scan target Y = $90
        sta     ent_timer,x
        jsr     junk_block_scan_slots
        bcc     junk_block_sprite_flags
        lda     #$70                    ; clamp Y to $70
        sta     ent_y_px,x
junk_block_sprite_flags:  lda     ent_flags,x  ; load flags
        ora     #$20                    ; set behind-sprite bit
        sta     ent_flags,x
        rts

; --- scan enemy slots for entity at same X, matching target Y ---
; Returns C=1 if found, C=0 if none.
junk_block_scan_slots:  stx     L0000   ; save current slot index
        ldy     #$1F                        ; start from slot $1F
junk_block_scan_loop:  cpy     L0000    ; skip self
        beq     junk_block_next_slot
        lda     ent_status,y            ; is slot active? (bit 7)
        bpl     junk_block_next_slot    ; inactive: skip
        lda     ent_flags,y
        and     #$04                    ; check bit 2 (child flag?)
        bne     junk_block_next_slot    ; set: skip
        lda     ent_x_px,x              ; compare parent X pixel
        cmp     ent_x_px,y              ; with scanned slot X pixel
        bne     junk_block_next_slot    ; X mismatch: skip
        lda     ent_y_px,y              ; scanned slot Y pixel
        cmp     ent_timer,x             ; matches target Y?
        beq     junk_block_scan_success ; yes: found (C=1)
junk_block_next_slot:  dey
        cpy     #$0F                    ; reached slot $0F?
        bne     junk_block_scan_loop    ; no: keep scanning
        clc                             ; not found: C=0
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

        lda     ent_status,x            ; entity status
        and     #$0F                    ; isolate state nibble
        bne     petit_snakey_init_check                   ; skip init if already active
        jsr     set_sprite_hflip        ; set H-flip from facing
        jsr     face_player             ; face toward player
        inc     ent_status,x            ; advance to state 1
        lda     #$24
        sta     ent_timer,x                 ; idle timer = 36 frames
petit_snakey_init_check:  lda     ent_var1,x              ; attack cooldown active?
        bne     petit_snakey_dec_cooldown                   ; yes -> decrement cooldown
        lda     ent_timer,x                 ; idle timer active?
        bne     petit_snakey_dec_idle                   ; yes -> decrement idle
        lda     ent_facing,x               ; check facing direction
        and     #$02                    ; bit 1 = facing left
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
        cmp     #$D1                    ; current anim == $D1?
        bne     petit_snakey_anim_b     ; no -> use variant B
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
        bne     petit_snakey_cooldown_rts  ; still counting down -> rts
        lda     ent_anim_id,x              ; cooldown done -> revert anim
        cmp     #$D2                    ; was attack anim variant A?
        bne     petit_snakey_revert_anim  ; no -> revert to idle B
        lda     #$D1                    ; revert to idle anim A ($D1)
        bne     petit_snakey_revert_done  ; always branches
petit_snakey_revert_anim:  lda     #$D4 ; revert to idle anim B ($D4)
petit_snakey_revert_done:  jsr     reset_sprite_anim               ; reset_sprite_anim
petit_snakey_cooldown_rts:  rts

; --- petit snakey: spawn homing projectile (speed $03.66, type $73) ---
petit_snakey_spawn_proj:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     petit_snakey_spawn_fail ; no free slot -> abort
        sty     L0000                   ; save projectile slot index
        lda     ent_facing,x            ; copy parent facing to proj
        sta     ent_facing,y
        and     #$02                    ; facing as table index (0 or 2)
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     petit_snakey_proj_x_off,y  ; add proj X pixel offset
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     petit_snakey_proj_x_scr,y  ; add proj X screen offset
        ldy     L0000                   ; restore projectile slot
        sta     ent_x_scr,y             ; set proj X screen
        pla
        sta     ent_x_px,y              ; set proj X pixel
        lda     #$00
        sta     ent_hp,y                ; proj HP = 0
        lda     ent_y_px,x              ; copy parent Y to proj
        sta     ent_y_px,y
        lda     ent_y_scr,x
        sta     ent_y_scr,y
        lda     #$66                    ; base speed sub = $66
        sta     ent_xvel_sub,y          ; set proj X vel sub
        sta     $02                     ; speed sub param for homing calc
        lda     #$03                    ; base speed whole = $03
        sta     ent_xvel,y              ; set proj X vel whole
        sta     $03                     ; speed whole param for homing calc
        sty     $0F                     ; save proj slot
        stx     $0E                     ; save parent slot
        ldx     $0F                     ; proj slot -> X for homing calc
        jsr     calc_homing_velocity                   ; calc_homing_velocity
        ldy     $0F                     ; restore proj slot to Y
        ldx     $0E                     ; restore parent slot to X
        lda     $0C                     ; homing direction result
        sta     ent_facing,y            ; set proj facing from homing
        lda     #$73                    ; entity type $73
        jsr     init_child_entity                   ; init_child_entity
        lda     #$8F                    ; AI routine = $8F (homing proj)
        sta     ent_routine,y
        lda     #$8B                    ; hitbox = $8B (contact damage)
        .byte   $99,$80,$04             ; sta ent_hitbox,y (hand-encoded)
petit_snakey_spawn_fail:  .byte   $60   ; rts (hand-encoded)
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
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; low nibble = state number
        bne     yambow_timer_flap                   ; skip init if active
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        cmp     #$51                        ; within 81 px?
        bcs     yambow_rts                   ; no -> rts
        jsr     face_player                       ; face_player
        jsr     set_sprite_hflip                       ; set_sprite_hflip
        lda     ent_flags,x             ; load entity flags
        and     #$FB                        ; clear bit 2 (enable collision)
        sta     ent_flags,x
        inc     ent_status,x                ; advance to state 1
        rts

        ; --- timer active: flap with gravity ---
yambow_timer_flap:  lda     ent_timer,x
        beq     yambow_state_dispatch                   ; timer expired -> state dispatch
        dec     ent_timer,x             ; decrement timer
        lda     ent_status,x            ; check state parity
        and     #$01                        ; odd states skip flapping
        bne     yambow_rts
        lda     ent_yvel,x                  ; Y speed cap check
        bmi     yambow_gravity_accel                   ; rising -> keep accelerating
        cmp     #$02                    ; Y speed >= +2 -> cap reached
        bcs     yambow_rts                   ; >= +2 -> stop
yambow_gravity_accel:  lda     ent_yvel_sub,x          ; gravity accel: +$10/frame
        clc
        adc     #$10
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00                    ; carry into whole byte
        sta     ent_yvel,x              ; store updated Y velocity
        bpl     yambow_apply_yvel                   ; positive -> falling
        jmp     apply_y_velocity_fall                       ; apply_y_velocity (rising)

yambow_apply_yvel:  jmp     apply_y_velocity_rise                   ; apply_y_velocity (falling)

yambow_rts:  rts

        ; --- timer expired: state dispatch ---
yambow_state_dispatch:  lda     ent_status,x
        and     #$0F                    ; get state number
        cmp     #$04                    ; state 4?
        beq     yambow_fly_forward                   ; state 4: fly forward
        cmp     #$03                    ; state 3?
        beq     yambow_recheck_y                   ; state 3: check Y distance
        cmp     #$02                    ; state 2?
        beq     yambow_swoop                   ; state 2: swoop horizontal
        ; state 1: check Y distance to player
        jsr     entity_y_dist_to_player                       ; entity_y_dist_to_player
        bcc     yambow_advance_state                   ; player above -> advance
        cmp     #$4D                        ; > 77 px below?
        bcc     yambow_advance_state                   ; close enough -> advance
        jmp     apply_y_speed                       ; apply_y_speed (keep descending)

yambow_advance_state:  lda     #$14                    ; timer = 20 frames
        sta     ent_timer,x             ; set state timer
        inc     ent_status,x                ; advance state
        jsr     reset_gravity                       ; reset_gravity
        jsr     face_player                       ; face_player
        jmp     set_sprite_hflip                       ; set_sprite_hflip

        ; --- state 2: swoop toward player ---
yambow_swoop:  lda     ent_facing,x     ; check facing direction
        and     #$02                    ; bit 1 = facing left
        beq     yambow_swoop_right_check                   ; facing right
        jsr     entity_x_dist_to_player                       ; entity_x_dist_to_player
        bcc     yambow_swoop_left       ; player is to the right
        cmp     #$29                        ; within 41 px?
        bcs     yambow_next_state                   ; yes -> advance state
yambow_swoop_left:  jmp     move_sprite_left                   ; move_sprite_left

yambow_swoop_right_check:  jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        bcs     yambow_swoop_right      ; player is to the left
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
        and     #$02                    ; check facing direction
        beq     yambow_fly_right        ; facing right -> fly right
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
        and     #$0F                    ; low nibble = state
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
        cmp     ent_anim_id,x           ; already using walk anim?
        beq     met_apply_gravity       ; yes -> skip anim reset
        jsr     reset_sprite_anim                   ; reset_sprite_anim
met_apply_gravity:  ldy     #$00                ; apply $99; C=1 if on ground
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     met_return               ; airborne → return
        lda     ent_var1,x                 ; walk frames remaining?
        beq     met_close_helmet               ; zero → done walking
        dec     ent_var1,x                 ; decrement walk counter
        lda     ent_facing,x                 ; walk in facing direction
        and     #$01                    ; bit 0 = facing right
        beq     met_walk_left           ; facing left -> walk left
        ldy     #$00                    ; Y = 0 (no slope offset)
        jmp     move_right_collide

met_walk_left:  ldy     #$01            ; Y = 1 (no slope offset)
        jmp     move_left_collide

; --- walk done: close helmet, return to hiding ---

met_close_helmet:  lda     #$1C                ; OAM $1C = helmet closing anim
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_status,x                 ; state → 0 (clear low nibble)
        and     #$F0                    ; clear low nibble
        sta     ent_status,x
        lda     #$A3                    ; become invulnerable again
        sta     ent_hitbox,x                 ; $A3 = no contact, no weapon hit
        lda     $E5                     ; pseudo-random hide delay
        adc     $E6                     ; LFSR seed mix
        sta     $E6                     ; store back to RNG state
        and     #$03                    ; 4 possible delays from table
        tay
        lda     met_hide_delay,y                 ; load random delay
        sta     ent_timer,x                 ; set hide timer
        rts

; --- met_fire_3_bullets: spawn 3 projectiles ---

met_fire_bullets:  stx     L0000               ; save Met slot
        lda     #$02                    ; bullet counter = 3 (indexes 2,1,0)
        sta     $01                     ; store to temp $01
met_bullet_loop:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     met_bullet_done               ; none → done
        ldx     $01                     ; set bullet speeds from table
        lda     met_bullet_xvel_sub,x                 ; X speed sub (3 entries)
        sta     ent_xvel_sub,y          ; set bullet X vel sub
        lda     met_bullet_xvel,x                 ; X speed whole
        sta     ent_xvel,y              ; set bullet X vel whole
        lda     met_bullet_yvel_sub,x                 ; Y speed sub
        sta     ent_yvel_sub,y          ; set bullet Y vel sub
        lda     met_bullet_yvel,x                 ; Y speed whole
        sta     ent_yvel,y              ; set bullet Y vel whole
        lda     #$73                    ; OAM $73 = Met bullet
        jsr     init_child_entity                   ; init_child_entity
        lda     #$8B                    ; dmg = $8B (hurts player only)
        sta     ent_hitbox,y            ; set bullet hitbox
        ldx     L0000                   ; restore Met slot to X
        lda     #$0F                    ; AI routine = $0F (simple projectile)
        sta     ent_routine,y           ; set bullet AI routine
        lda     ent_x_px,x                 ; copy Met position to bullet
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy Met X screen to bullet
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; Met Y pixel
        clc
        adc     #$04                    ; bullet Y = Met Y + 4
        sta     ent_y_px,y              ; set bullet Y pixel
        lda     ent_facing,x            ; copy facing to bullet
        sta     ent_facing,y
        dec     $01                     ; loop for all 3 bullets
        bpl     met_bullet_loop         ; loop until all 3 spawned
met_bullet_done:  .byte   $A6,$00,$60         ; restore X

; Met bullet speeds: X.sub={$FB,$33,$FB}, X.whole={$00,$01,$00},
; Y.sub={$50,$00,$B0}, Y.whole={$00,$00,$FF}
; bullet 0: slow right+down, bullet 1: fast right, bullet 2: slow right+up
met_bullet_xvel_sub:  .byte   $FB,$33,$FB
met_bullet_xvel:  .byte   $00,$01,$00
met_bullet_yvel_sub:  .byte   $50,$00,$B0
met_bullet_yvel:  .byte   $FF,$00,$00
met_hide_delay:  asl     petit_snakey_data,x
        .byte   $3C

; ===========================================================================
; main_pole — Pole (climbing pole enemy, Spark Man stage)
; ===========================================================================
; Moves vertically at set speed, walks horizontally (via code_1C9776).
; Despawns when Y screen changes (goes offscreen).
main_pole:
        lda     #$00                    ; sign extend = 0 (positive)
        sta     L0000                   ; temp $00 = sign extension byte
        lda     ent_yvel,x              ; check Y velocity sign
        bpl     pole_apply_yvel_sub     ; positive → skip sign extend
        dec     L0000                   ; negative → sign = $FF
pole_apply_yvel_sub:  lda     ent_y_sub,x             ; apply Y speed: Y.sub += Yspd.sub
        clc
        adc     ent_yvel_sub,x          ; add Y vel sub
        sta     ent_y_sub,x             ; store updated Y sub
        lda     ent_y_px,x                 ; Y += Yspd.whole
        adc     ent_yvel,x              ; add Y vel whole
        sta     ent_y_px,x              ; store updated Y pixel
        lda     ent_y_scr,x                 ; Y.screen += sign
        adc     L0000                   ; add sign extend to Y screen
        bne     pole_despawn               ; offscreen → despawn
        jmp     yambow_fly_forward               ; on-screen → walk horizontally

pole_despawn:  lda     #$00                ; despawn
        sta     ent_status,x            ; clear entity status
        rts

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
        cmp     #$51                    ; within $51 px?
        bcc     cannon_open               ; yes → open up
cannon_freeze_anim:  lda     #$00                ; freeze anim (stay closed)
        sta     ent_anim_frame,x
        rts

cannon_open:  lda     #$C9                ; become vulnerable ($C9)
        sta     ent_hitbox,x            ; set hitbox to vulnerable
cannon_face_player:  jsr     face_player               ; track player direction
        jsr     set_sprite_hflip                   ; flip sprite
        lda     ent_anim_state,x                 ; anim fully done? (both zero)
        ora     ent_anim_frame,x
        bne     cannon_check_fire               ; still animating → check fire

; --- anim complete: close cannon, set idle delay ---
        lda     $E4                     ; pseudo-random idle delay
        adc     $E7                     ; LFSR seed mix
        sta     $E7                     ; store mixed seed
        and     #$01                    ; 2 possible delays
        tay
        lda     cannon_idle_delay,y     ; load idle delay from table
        sta     ent_timer,x                 ; set idle timer
        lda     #$A9                    ; close: become invulnerable ($A9)
        sta     ent_hitbox,x
cannon_rts:  rts

; --- during open anim: fire at specific frames ---

cannon_check_fire:  lda     ent_anim_frame,x             ; frame timer must be 0 (exact moment)
        bne     cannon_rts               ; not zero → return
        lda     ent_anim_state,x                 ; fire at anim frame $09 or $12
        cmp     #$09                    ; (two shots per open cycle)
        beq     cannon_spawn_shell
        cmp     #$12                    ; second shot frame?
        beq     cannon_spawn_shell
        rts

; --- spawn_cannon_shell: fires distance-scaled projectile ---

cannon_spawn_shell:  jsr     find_enemy_freeslot_y               ; find free slot
        bcs     cannon_rts               ; none → return
        lda     #$00                    ; shell Y speed sub = 0
        sta     ent_yvel_sub,y
        lda     #$04                    ; shell Y speed = $04 (upward arc)
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
        pha                             ; save facing for X offset later
        jsr     entity_x_dist_to_player                   ; get distance for speed scaling
        stx     L0000                   ; save cannon slot
        ldx     #$03                    ; find speed bracket from distance table
cannon_speed_scan:  cmp     cannon_dist_table,x             ; (farther = slower X speed)
        bcc     cannon_speed_found
        dex
        bne     cannon_speed_scan       ; loop until bracket found
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
        .byte   $A6,$00,$60             ; ldx $00; rts (restore cannon slot)

; distance thresholds: $4C, $3D, $2E, $1F (close→far)
; X speed sub: $00, $80, $00, $80 | X speed whole: $02, $01, $01, $00
; idle delays: $3C, $78 | X offsets: right=$0C/$00, left=$F4/$FF
cannon_dist_table:  .byte   $4C,$3D,$2E,$1F
cannon_xvel_sub:  .byte   $00,$80,$00,$80
cannon_xvel:  .byte   $02,$01,$01,$00
cannon_idle_delay:  .byte   $3C,$78
cannon_shell_x_off:  .byte   $0C
cannon_shell_x_scr:  brk
        .byte   $F4
        .byte   $FF

; --- cannon shell AI: $99 + walk, explode on landing/wall hit ---
        ldy     #$08                    ; gravity strength = 8
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcs     cannon_shell_explode               ; landed → explode
        lda     ent_facing,x                 ; walk horizontally with collision
        and     #$02
        beq     cannon_shell_move_right
        ldy     #$07                    ; move speed = 7
        jsr     move_left_collide                   ; move_left_collide
        jmp     cannon_shell_collision  ; check wall collision result

cannon_shell_move_right:  ldy     #$08  ; move speed = 8
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
        and     #$0F                    ; isolate state (lower nibble)
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
        cmp     #$61                    ; within $61 px?
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
        bcs     metall_dx_advance_state ; far enough → next state
        jmp     move_sprite_up                   ; keep flying up

metall_dx_freeze_anim:  lda     #$00                ; freeze anim (stay hidden)
        sta     ent_anim_frame,x
metall_dx_state0_rts:  rts

; --- state 1: fly past player, fire when crossing ---

metall_dx_fly_past:  jsr     entity_x_dist_to_player               ; get X distance (sets carry)
        lda     ent_facing,x                 ; check if passed player
        and     #$02                    ; facing left + player behind → fire
        beq     metall_dx_check_fire    ; facing right → check fire
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

metall_dx_fire_3:  stx     L0000        ; save entity slot
        lda     #$02                    ; 3 bullets (index 2,1,0)
        sta     $01                     ; bullet loop counter
metall_dx_proj_loop:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     metall_dx_after_fire
        ldx     $01                     ; load bullet index
        lda     metall_dx_proj_xvel_sub,x  ; set bullet X speed sub
        sta     ent_xvel_sub,y
        lda     metall_dx_proj_xvel,x   ; set bullet X speed whole
        sta     ent_xvel,y
        lda     metall_dx_proj_yvel_sub,x  ; set bullet Y speed sub
        sta     ent_yvel_sub,y
        lda     metall_dx_proj_yvel,x   ; set bullet Y speed whole
        sta     ent_yvel,y
        lda     metall_dx_proj_facing,x ; set bullet facing from table
        sta     ent_facing,y
        lda     #$73                    ; OAM $73 = metall DX bullet
        jsr     init_child_entity                   ; init_child_entity
        lda     #$8B                    ; hitbox $8B = small projectile
        sta     ent_hitbox,y
        ldx     L0000                   ; restore parent entity slot
        lda     #$0F                    ; AI routine $0F = linear projectile
        sta     ent_routine,y
        lda     ent_x_px,x              ; copy parent X position
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y position
        sta     ent_y_px,y
        dec     $01                     ; next bullet index
        bpl     metall_dx_proj_loop
metall_dx_after_fire:  ldx     L0000    ; restore parent entity slot
        inc     ent_status,x            ; advance to state 2 (descend)
        lda     #$3C                    ; post-fire delay = $3C (60 frames)
        .byte   $9D,$00,$05,$60
metall_dx_proj_xvel_sub:  .byte   $DB,$00,$DB
metall_dx_proj_xvel:  .byte   $00,$00,$00
metall_dx_proj_yvel_sub:  .byte   $DB,$33,$DB
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
        beq     mag_fly_move_left       ; bit 0 clear → move left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     mag_fly_check_distance  ; then check player distance

mag_fly_move_left:  jsr     move_sprite_left                   ; move_sprite_left
mag_fly_check_distance:  jsr     entity_y_dist_to_player                   ; check player proximity
        bcc     mag_fly_final_dismount  ; player above → check dismount
        jsr     entity_x_dist_to_player                   ; detailed collision check
        cmp     #$10                    ; too far away?
        bcs     mag_fly_final_dismount  ; too far → full dismount
        lda     player_state                     ; only mount if state < $02
        cmp     #PSTATE_SLIDE                    ; (on_ground or airborne)
        bcs     mag_fly_check_dismount  ; state >= 2 → check dismount only
        lda     #PSTATE_ENTITY_RIDE                    ; state → $05 (entity_ride)
        sta     player_state            ; set player to entity_ride
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

mag_fly_check_dismount:  lda     player_state  ; check player state
        cmp     #PSTATE_ENTITY_RIDE                    ; if not in entity_ride, skip
        bne     mag_fly_return          ; not riding → skip
        cpx     entity_ride_slot                     ; if riding different entity, skip
        bne     mag_fly_return          ; different entity → skip
        lda     ent_timer,x                 ; check Y distance from mount point
        sec
        sbc     ent_y_px                ; subtract current player Y
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
        lda     L0000                   ; reload child slot index
        sta     ent_timer,x                 ; ent_timer = child slot (to track Y)
        lda     #$78                    ; reset throw cooldown = $78 (120 frames)
        sta     ent_var1,x              ; store throw cooldown
        lda     #$00                    ; clear throw-anim flag
        sta     ent_var2,x              ; clear throw-anim flag
junk_golem_check_throw_anim:  lda     ent_var2,x             ; if throw anim already started,
        bne     junk_golem_throw_anim_done               ; skip to countdown
        lda     ent_timer,x             ; load child slot index
        tay
        lda     ent_y_px,y                 ; child Y position
        sec
        sbc     ent_y_px,x                 ; minus golem Y position
        bcs     junk_golem_check_block_dist  ; if positive, skip negate
        eor     #$FF                    ; take absolute value
        adc     #$01
        clc
junk_golem_check_block_dist:  cmp     #$30                ; if |Y dist| >= $30, block still far
        bcs     junk_golem_return               ; done (block still falling from top)
        lda     ent_anim_id,x                 ; if already using throw OAM ($39),
        cmp     #$39                    ; skip animation reset
        beq     junk_golem_throw_anim_done  ; already throwing, skip reset
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
        sta     ent_facing,y            ; child inherits golem facing
        lda     ent_x_px,x                 ; copy parent X position to child
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y             ; copy parent X screen to child
        lda     #$04                    ; child Y = $04 (near top of screen)
        sta     ent_y_px,y                 ; block spawns high and falls down
        lda     ent_y_px,x                 ; ent_timer,y = golem's Y position
        sta     ent_timer,y                 ; (target Y for homing transition)
        lda     #$94                    ; entity type $94 (junk block)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$CA                    ; damage flags $CA: hurts player + takes damage
        sta     ent_hitbox,y
        lda     #$24                    ; AI routine = $24 (main_unknown_24)
        sta     ent_routine,y           ; set child AI routine
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

        lda     ent_status,x            ; check entity sub-state
        and     #$0F
        bne     unknown_24_move_down               ; skip if already initialized
        sta     ent_yvel_sub,x          ; A=0: clear Y velocity sub
        lda     #$04                    ; Y velocity = $04 (4 px/frame)
        sta     ent_yvel,x              ; set Y velocity whole
        inc     ent_status,x                 ; advance to state 1
unknown_24_move_down:  jsr     move_sprite_down               ; move downward (no collision)
        lda     ent_y_px,x                 ; current Y position
        sec
        sbc     ent_timer,x                 ; minus target Y (golem's Y)
        bcs     unknown_24_distance_check  ; if positive, skip negate
        eor     #$FF                    ; take absolute value
        adc     #$01
        clc
unknown_24_distance_check:  cmp     #$20                ; if |Y dist to target| >= $20,
        bcs     unknown_24_done               ; still falling, done
        lda     #$80                    ; homing speed = $04.80 (4.5 px/frame)
        sta     $02                     ; speed sub-pixel = $80
        lda     #$04                    ; speed whole = $04
        sta     $03                     ; store speed whole
        jsr     calc_homing_velocity                   ; compute X/Y velocity toward player
        lda     $0C                     ; homing result direction
        sta     ent_facing,x            ; set entity facing
        lda     #$0B                    ; switch to routine $0B (generic homing)
        sta     ent_routine,x                 ; block now flies toward player
unknown_24_done:  rts

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
        and     #$0F                    ; isolate sub-state
        bne     pickelman_bull_rider_hit_check               ; skip if already initialized
        sta     ent_yvel_sub,x          ; A=0: clear Y velocity sub
        lda     #$04                    ; Y velocity = $04 (4 px/frame)
        sta     ent_yvel,x              ; set Y velocity whole
        jsr     pickelman_bull_random_drive               ; get random drive count ($10/$20/$30)
        sta     ent_timer,x                 ; store as drive step counter
        lda     #$1E                    ; stop timer = $1E (30 frames)
        sta     ent_var1,x
        inc     ent_status,x                 ; advance to state 1
        ; --- rider weapon collision check (Y-$17 offset for rider hitbox) ---
pickelman_bull_rider_hit_check:  lda     ent_y_px,x             ; save real Y position
        pha                             ; push real Y to stack
        lda     ent_y_px,x
        sec
        sbc     #$17                        ; offset Y up by 23 px for rider
        sta     ent_y_px,x              ; temp set Y to rider position
        lda     #$C3                        ; rider hitbox (vulnerable)
        sta     ent_hitbox,x
        jsr     process_sprites_top_spin_check                   ; check_weapon_hit
        pla                             ; pull saved Y from stack
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
        lda     ent_facing,x            ; check entity facing
        and     #$01                        ; facing right?
        beq     pickelman_bull_check_left_wall                   ; no -> check left
        lda     $42                     ; tile at feet (low check point)
        and     #$10                        ; solid?
        beq     pickelman_bull_reverse_direction                   ; no solid -> reverse
        ldy     #$10                    ; Y = speed parameter
        jsr     move_right_collide                       ; move_right_collide
        jmp     pickelman_bull_wall_done  ; skip left wall check

pickelman_bull_check_left_wall:  lda     tile_at_feet_hi  ; tile at feet (high check point)
        and     #$10
        beq     pickelman_bull_reverse_direction
        ldy     #$11
        jsr     move_left_collide                   ; move_left_collide
pickelman_bull_wall_done:  bcc     pickelman_bull_rts_2  ; C=0: no wall hit, done
pickelman_bull_reverse_direction:  lda     ent_facing,x
        eor     #$03                    ; toggle facing (1<->2)
        sta     ent_facing,x
pickelman_bull_rts_2:  rts

        ; --- state 2: stopped, rider oscillates left/right ---
pickelman_bull_stopped_state:  dec     ent_var1,x              ; decrement stop timer
        bne     pickelman_bull_oscillate_rider  ; timer > 0: keep oscillating
        sta     ent_var2,x                  ; reset oscillation delay
        sta     ent_var3,x                  ; reset oscillation counter
        lda     #$1E                    ; reload stop timer = $1E
        sta     ent_var1,x                  ; new stop timer = 30 frames
        jsr     pickelman_bull_random_drive                   ; random drive count
        sta     ent_timer,x                 ; set new drive step counter
        dec     ent_status,x               ; -> back to driving state
        rts

        ; --- oscillation: move rider 1px left/right alternating ---
pickelman_bull_oscillate_rider:  lda     ent_var2,x              ; oscillation delay
        bne     pickelman_bull_delay_decrement                   ; delay active -> decrement
        lda     ent_var3,x                  ; oscillation counter
        and     #$01                        ; even=right, odd=left
        asl     a                       ; index into 2-byte table entries
        tay                             ; Y = table index
        lda     ent_x_px,x              ; current X position
        clc
        adc     pickelman_bull_oscillation_x,y                     ; +1 or -1 pixel
        sta     ent_x_px,x
        lda     ent_x_scr,x             ; current X screen
        adc     pickelman_bull_oscillation_scr,y  ; add screen carry from offset
        sta     ent_x_scr,x
        lda     #$02                    ; oscillation delay = 2 frames
        sta     ent_var2,x                  ; 2-frame delay between oscillations
        inc     ent_var3,x              ; next oscillation direction
        rts

pickelman_bull_delay_decrement:  .byte   $DE,$40,$05,$60         ; dec ent_var2,x; rts
pickelman_bull_oscillation_x:  .byte   $01                         ; oscillation X offsets: +1, 0, -1, -1
pickelman_bull_oscillation_scr:  brk    ; screen offset: +0
        .byte   $FF
        .byte   $FF
; --- random drive count from table ($10/$20/$30/$10) ---
pickelman_bull_random_drive:  lda     $E4                     ; pseudo-random: add two RNG bytes
        adc     $E5                     ; add second RNG byte
        sta     $E4                     ; update RNG state
        and     #$03                    ; mask to 0-3 for table index
        tay
        .byte   $B9,$2D,$9D,$60
        bpl     bikky_jump_encode       ; positive result: branch forward
        bmi     bikky_facing_decode     ; negative result: branch forward
; ---------------------------------------------------------------------------
; main_bikky -- Bikky (stomping enemy, entity type $21)
; ---------------------------------------------------------------------------
; Applies gravity, walks horizontally with collision.
; On landing (anim_state=$08, frame=0): launch upward (Y speed $05.A8),
;   face player, hitbox $C5 (dangerous). Sound $20 on subsequent landing.
; While airborne: hitbox $A5 (safe).
; ---------------------------------------------------------------------------
main_bikky:
        jsr     set_sprite_hflip                       ; set_sprite_hflip
        ldy     #$10                    ; Y = hitbox offset $10
        jsr     move_vertical_gravity                       ; move_vertical_gravity
        bcs     bikky_check_anim_state                   ; landed -> check anim state
        lda     #$00
        sta     ent_anim_frame,x            ; reset anim frame while airborne
        .byte   $BD                     ; encoded: lda ent_facing,x
bikky_facing_decode:  ldy     #$04                        ; (encoded lda ent_facing,x)
        and     #$01
        beq     bikky_move_left                   ; facing left
        ldy     #$0E
        jmp     move_right_collide                       ; move_right_collide

bikky_move_left:  ldy     #$0F
        .byte   $4C
bikky_jump_encode:  cpy     prg_bank    ; encoded: jmp move_left_collide
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
        jsr     entity_x_dist_to_player ; get X distance to player
        cmp     #$50                        ; within 80 px?
        bcs     shotman_fire_loop                   ; no -> fire timer only
        lda     ent_var2,x                  ; shot cooldown active?
        bne     shotman_shot_cooldown                   ; yes -> count down
        lda     #$5A                        ; shooting animation
        jsr     reset_sprite_anim       ; reset_sprite_anim
        jsr     face_player             ; face_player
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
        lda     #$00                    ; clear shot count
        sta     ent_var3,x
        lda     #$78                    ; post-burst cooldown = 120 frames
        sta     ent_var2,x              ; store cooldown timer
        inc     ent_status,x            ; advance to walk state
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
shotman_spawn_bullet_pair:  jsr     find_enemy_freeslot_y  ; find_enemy_freeslot_y
        bcs     shotman_bullet_no_slot  ; no slot -> rts
        sty     L0000                   ; save child slot index
        lda     ent_facing,x            ; copy parent facing to child
        sta     ent_facing,y
        and     #$02                    ; facing -> offset index (0 or 2)
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     shotman_bullet_x_offset,y  ; add X offset for facing dir
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     shotman_bullet_x_scr,y  ; add screen carry
        ldy     L0000
        sta     ent_x_scr,y             ; set child X screen
        pla
        sta     ent_x_px,y              ; set child X pixel
        lda     ent_y_px,x              ; parent Y pixel
        sec
        sbc     #$04                    ; spawn 4 px above parent
        sta     ent_y_px,y              ; set child Y pixel
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y
        lda     #$01                    ; X speed whole = 1
        sta     ent_xvel,y
        lda     #$73                    ; entity type $73 (projectile)
        jsr     init_child_entity       ; init_child_entity
        lda     #$1B                    ; AI routine = $1B
        sta     ent_routine,y
        lda     #$8B                    ; projectile hitbox
        sta     ent_hitbox,y
        lda     #$00                    ; HP = 0 (indestructible)
        sta     ent_hp,y
        lda     ent_facing,x            ; flip facing for 2nd bullet
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x
        inc     $01                     ; increment bullet pair counter
        lda     $01
        cmp     #$02                    ; spawned both bullets?
        .byte   $90,$A2
shotman_bullet_no_slot:  .byte   $60
shotman_bullet_x_offset:  .byte   $0F
shotman_bullet_x_scr:  brk
        sbc     (ppu_ctrl_shadow),y
; --- new shotman: spawn falling projectile (type $73, Y speed $04.00) ---
shotman_spawn_falling_proj:  jsr     find_enemy_freeslot_y  ; find_enemy_freeslot_y
        bcs     shotman_bullet_no_slot  ; no slot -> rts
        lda     #$00                        ; Y speed = $04.00 (falling)
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$73                        ; entity type $73 (projectile)
        jsr     init_child_entity       ; init_child_entity
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
        jsr     entity_x_dist_to_player ; entity_x_dist_to_player
        stx     L0000                   ; save parent X index
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
        ldy     #$12                    ; gravity speed index $12
        jsr     move_vertical_gravity   ; move_vertical_gravity
        bcs     shotman_proj_collision  ; floor/wall hit -> destroy
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     shotman_proj_move_left  ; 0 = left
        ldy     #$1E                    ; speed index $1E
        jsr     move_right_collide      ; move_right_collide
        jmp     shotman_proj_after_move

shotman_proj_move_left:  ldy     #$1F   ; speed index $1F
        jsr     move_left_collide       ; move_left_collide
shotman_proj_after_move:  bcs     shotman_proj_collision  ; wall hit -> destroy
        rts

shotman_proj_collision:  lda     ent_routine,x  ; check current routine
        cmp     #$0C                    ; routine $0C = falling proj
        bne     shotman_debris_spawn    ; not $0C -> debris
        lda     #$00                    ; routine = 0 (dead)
        sta     ent_routine,x
        lda     $B3                     ; collision direction flag
        bpl     shotman_proj_death_upper  ; bit 7 set = hit from below
        lda     #$59                    ; anim $59 (ground impact)
        bne     shotman_proj_death_anim
shotman_proj_death_upper:  lda     #$71 ; anim $71 (explosion)
shotman_proj_death_anim:  jmp     reset_sprite_anim  ; set death animation

        lda     ent_facing,x            ; check facing direction
        and     #$01
        beq     shotman_debris_move_left  ; 0 = facing left
        ldy     #$0C                    ; speed index $0C
        jsr     move_right_collide      ; move_right_collide
        jmp     shotman_debris_after_move

shotman_debris_move_left:  ldy     #$0D ; speed index $0D
        jsr     move_left_collide       ; move_left_collide
shotman_debris_after_move:  bcs     shotman_debris_spawn  ; wall hit -> spawn debris
        lda     ent_facing,x            ; check vertical direction flag
        and     #$08                    ; bit 3 = moving up
        beq     shotman_debris_move_down  ; 0 = moving down
        jmp     move_sprite_up          ; move_sprite_up

shotman_debris_move_down:  jmp     move_sprite_down  ; move_sprite_down

shotman_debris_spawn:  lda     #$00     ; deactivate parent entity
        sta     ent_status,x
        sta     $01
        lda     #$FF                    ; mark spawn slot as used ($FF)
        sta     ent_spawn_id,x
shotman_debris_spawn_loop:
        jsr     find_enemy_freeslot_y   ; find_enemy_freeslot_y
        bcs     shotman_debris_rts      ; no slot -> done
        sty     L0000                   ; save child slot index
        lda     ent_facing,x            ; get parent facing
        and     #$02                    ; facing -> offset index
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     gyoraibo_child_x_off,y  ; add child X offset
        pha
        lda     ent_x_scr,x
        adc     gyoraibo_child_x_scr,y  ; add screen carry
        ldy     L0000
        sta     ent_x_scr,y             ; set child X screen
        pla
        sta     ent_x_px,y              ; set child X pixel
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y
        lda     #$5B                    ; entity type $5B (debris)
        jsr     init_child_entity       ; init_child_entity
        lda     #$0C                    ; routine $0C (falling)
        sta     ent_routine,y
        lda     #$8B                    ; projectile hitbox
        sta     ent_hitbox,y
        lda     #$00                    ; HP = 0 (indestructible)
        sta     ent_hp,y
        stx     $02                     ; save parent slot
        ldx     $01                     ; X = debris index
        lda     shotman_debris_facing,x ; facing from table
        sta     ent_facing,y
        lda     shotman_debris_yvel_sub,x  ; Y speed sub from table
        sta     ent_yvel_sub,y
        lda     shotman_debris_yvel,x   ; Y speed whole from table
        sta     ent_yvel,y
        lda     shotman_debris_xvel_sub,x  ; X speed sub from table
        sta     ent_xvel_sub,y
        lda     shotman_debris_xvel,x   ; X speed whole from table
        sta     ent_xvel,y
        ldx     $02                     ; restore parent slot
        inc     $01                     ; next debris index
        lda     $01
        cmp     #$04                    ; spawned all 4 debris?
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
        jsr     cutscene_init           ; cutscene init/whistle
        lda     ent_var3,x              ; phase flag
        beq     shotman_debris_rts      ; not started yet -> return
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
        tya                             ; variant index
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
        tya                             ; variant index
        lda     proto_man_jump_anim_table,y                 ; jump anim from table
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$A8                    ; set Y velocity for jump
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        rts

proto_man_walk_anim:  lda     ent_timer,x          ; variant index
        tay                             ; variant index
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
        tay                             ; variant index
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
        tya                             ; variant index
        lda     proto_man_shoot_anim_table,y                 ; shoot anim from table
        cmp     ent_anim_id,x
        beq     proto_man_fire_projectile               ; already set → skip
        lda     ent_timer,x             ; variant index
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
        lda     ent_routine,x           ; check routine type
        cmp     #$53                    ; $53 = Hard Man stage Proto Man
        bne     proto_man_defeated_set_flag               ; $52 = normal → skip, set $68
        lda     #PSTATE_TELEPORT_BEAM   ; player state = teleport beam
        sta     player_state            ; Proto Man exit -> player beams out
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
        lda     #PSTATE_STUNNED         ; player state = stunned
        sta     player_state            ; freeze player during cutscene
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
        lda     ent_routine,x           ; check routine type
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
        sta     ent_facing,y            ; set child facing
        and     #$02                    ; direction offset for X spawn table
        tay
        lda     ent_x_px,x              ; child X = parent X + offset
        clc
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
        sta     ent_xvel_sub,y          ; clear X speed sub
        lda     #$04                    ; X speed whole = 4
        sta     ent_xvel,y              ; X speed = 4
        lda     ent_routine,x           ; check routine odd/even
        and     #$01                    ; bit 0: $52 vs $53
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
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     hari_harry_state_dispatch
        jsr     set_sprite_hflip                   ; face toward player
        lda     #$3C                    ; hide timer = 60 frames
        sta     ent_var3,x              ; store as hide timer
        inc     ent_status,x            ; advance to state 1
hari_harry_state_dispatch:  lda     ent_status,x  ; check sub-state
        and     #$0F                    ; extract sub-state
        cmp     #$02                    ; state 2 = firing
        beq     hari_harry_firing
        cmp     #$03                    ; state 3 = walking
        beq     hari_harry_walking
        dec     ent_var3,x              ; decrement hide timer
        bne     hari_harry_freeze_anim
        lda     #$00                    ; timer expired: clear it
        sta     ent_var3,x
        inc     ent_status,x            ; advance to state 2
hari_harry_freeze_anim:  lda     #$00                ; freeze animation while hidden
        sta     ent_anim_frame,x                 ; (reset anim timer and frame)
        sta     ent_anim_state,x        ; reset anim state
        rts

; --- state 2: firing sequence (turret open) ---

hari_harry_firing:  lda     ent_timer,x             ; if firing-done flag set,
        bne     hari_harry_post_fire_pause               ; skip to post-fire pause
        lda     ent_anim_state,x        ; check anim state
        cmp     #$01                    ; state 1 = first fire frame
        bne     hari_harry_fire_spread_2
        lda     ent_anim_frame,x
        cmp     #$01
        bne     hari_harry_fire_spread_2
        lda     #$04                    ; bullet index = 4 (5 bullets)
        sta     $01
        stx     L0000                   ; save parent slot
        jsr     hari_harry_spawn_bullets  ; spawn 5-bullet spread
hari_harry_fire_spread_2:  lda     ent_anim_state,x  ; check anim state
        cmp     #$03                    ; state 3 = second fire frame
        bne     hari_harry_fire_complete
        lda     ent_anim_frame,x
        cmp     #$01                    ; tick 1?
        bne     hari_harry_fire_complete
        lda     #$04                    ; bullet index = 4 (5 bullets)
        sta     $01
        stx     L0000
        jsr     hari_harry_spawn_bullets  ; spawn 5-bullet spread
hari_harry_fire_complete:  lda     ent_anim_state,x  ; check anim state
        cmp     #$04                    ; firing animation complete
        bne     hari_harry_fire_rts
        lda     ent_anim_frame,x
        cmp     #$02
        bne     hari_harry_fire_rts
        inc     ent_timer,x             ; mark firing done
        lda     #$10                    ; post-fire pause = 16 frames
        sta     ent_var1,x
hari_harry_fire_rts:  rts

; --- post-fire pause, then transition to walking ---

hari_harry_post_fire_pause:  lda     #$00                ; freeze animation during pause
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        dec     ent_var1,x              ; decrement post-fire timer
        bne     hari_harry_pause_rts
        inc     ent_status,x            ; advance to state 3
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
        bcc     hari_harry_walk_timer   ; no floor hit -> check walk timer
        lda     #$AA                    ; on floor: set damage flags (hurts player, takes damage)
        sta     ent_hitbox,x
        lda     ent_facing,x            ; check facing direction
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
        sta     ent_hitbox,x            ; set invincible hitbox
        lda     #$76                    ; set helmet closing sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$FF                    ; retract timer = 255 frames
        sta     ent_var2,x              ; store retract timer
        inc     ent_var3,x              ; set retract phase flag
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
        sta     ent_hitbox,x            ; set shielded hitbox
        lda     #$76                    ; set helmet sprite
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$82                    ; reset to state 2 (active + firing)
        sta     ent_status,x            ; set active+firing state
        rts

; --- helmet retract-and-fire: single shot while closing ---

hari_harry_retract_fire:  lda     ent_anim_state,x  ; check anim state
        cmp     #$01                    ; (helmet opening frame)
        bne     hari_harry_retract_rts  ; not frame 1 -> wait
        lda     ent_anim_frame,x
        cmp     #$02
        bne     hari_harry_retract_rts
        lda     #$04                    ; fire 5 bullets
        sta     $01                     ; bullet index = 4 (5 bullets)
        stx     L0000
        jsr     hari_harry_spawn_bullets
        lda     #$10                    ; set post-fire pause = 16 frames
        sta     ent_var1,x              ; store post-fire timer
        dec     ent_status,x            ; back to state 2
hari_harry_retract_rts:  rts

; --- spawn_hari_harry_bullets: loop spawns up to 5 projectiles ---
; $01 = bullet index (counts down from 4 to 0), $00 = parent slot

hari_harry_spawn_bullets:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     hari_harry_spawn_done               ; no slot: abort
        ldx     $01                     ; X = bullet index for table lookup
        lda     hari_harry_bullet_xvel_sub,x                 ; set X speed sub from table
        sta     ent_xvel_sub,y
        lda     hari_harry_bullet_xvel,x                 ; set X speed whole from table
        sta     ent_xvel,y
        lda     hari_harry_bullet_yvel_sub,x                 ; set Y speed sub from table
        sta     ent_yvel_sub,y
        lda     hari_harry_bullet_yvel,x                 ; set Y speed whole from table
        sta     ent_yvel,y
        lda     hari_harry_bullet_facing,x                 ; set direction flags from table
        sta     ent_facing,y
        lda     hari_harry_bullet_oam_id,x                 ; init child with OAM ID from table
        jsr     init_child_entity                   ; init_child_entity
        lda     hari_harry_bullet_flags,x                 ; set sprite flags from table
        sta     ent_flags,y
        ldx     L0000                   ; restore parent slot to X
        lda     #$CB                    ; bullet damage flags (projectile)
        sta     ent_hitbox,y
        lda     #$36                    ; AI routine = $36 (bullet movement)
        sta     ent_routine,y
        lda     ent_x_px,x              ; copy parent X to bullet
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y to bullet
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
hari_harry_bullet_xvel:  .byte   $00,$02,$03,$03,$02
hari_harry_bullet_yvel_sub:  .byte   $00,$E1,$00,$00,$E1
hari_harry_bullet_yvel:  .byte   $FD,$FD,$00,$00,$FD
hari_harry_bullet_facing:  .byte   $02,$02,$02,$01,$01
hari_harry_bullet_oam_id:  .byte   $40,$79,$78,$78,$79
hari_harry_bullet_flags:  .byte   $90,$90,$90,$D0,$D0

; ===========================================================================
; main_nitron — Nitron (rocket/missile enemy, Gemini Man stage)
; Approaches player horizontally, then flies in a sinusoidal wave pattern
; while dropping bombs. After 2 bomb drops, climbs away upward.
; States: 0=approach, 1=sine wave flight + bomb, 2=climb away
; ent_timer=frame delay, ent_var1=sine index, ent_var2=bomb count, ent_var3=bomb cooldown
; ===========================================================================
main_nitron:
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     nitron_state_dispatch   ; nonzero -> already moving
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     nitron_approach_left    ; 0 = left
        jsr     move_sprite_right                   ; move right toward player
        jmp     nitron_check_distance

nitron_approach_left:  jsr     move_sprite_left               ; move left toward player
nitron_check_distance:  jsr     entity_x_dist_to_player  ; get X distance to player
        cmp     #$40                    ; within 64 px?
        bcs     nitron_approach_rts     ; no -> keep approaching
        inc     ent_status,x            ; advance to sine wave state
nitron_approach_rts:  rts

; --- state 1+: dispatch flying vs climbing ---

nitron_state_dispatch:  lda     ent_status,x             ; check if state >= 2
        and     #$02
        beq     nitron_sine_init               ; state 1: sine wave flight
        jmp     nitron_climb_away               ; state 2: climb away

; --- state 1: sinusoidal wave flight ---

nitron_sine_init:  lda     ent_timer,x             ; if frame delay active,
        bne     nitron_apply_movement               ; skip to movement
        ldy     ent_var1,x              ; load sine step index
        lda     nitron_phase_table,y                 ; lookup phase entry
        asl     a                       ; *2 for 16-bit table offset
        tay
        lda     nitron_yvel_lo,y                 ; set Y speed from sine table (sub)
        sta     ent_yvel_sub,x
        lda     nitron_yvel_table,y                 ; set Y speed from sine table (whole)
        sta     ent_yvel,x
        lda     nitron_xvel_lo,y                 ; set X speed from sine table (sub)
        sta     ent_xvel_sub,x
        lda     nitron_xvel_table,y                 ; set X speed from sine table (whole)
        sta     ent_xvel,x
        lda     ent_xvel,x              ; check if X speed negative
        bpl     nitron_negate_xvel      ; positive -> skip negate
        lda     ent_xvel_sub,x          ; negate 16-bit X speed
        eor     #$FF                    ; invert low byte
        clc
        adc     #$01
        sta     ent_xvel_sub,x
        lda     ent_xvel,x              ; invert high byte
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x              ; store negated X speed
nitron_negate_xvel:  inc     ent_var1,x ; next sine step
        lda     ent_var1,x
        cmp     #$06                    ; 6 steps = flight done
        bne     nitron_set_delay
        inc     ent_status,x            ; advance to climb-away state
        lda     #$1A                    ; bomb cooldown = 26 frames
        sta     ent_var3,x
nitron_set_delay:  lda     #$0D                ; frame delay = 13 (hold each sine
        sta     ent_timer,x             ; store frame delay

; --- apply current speed each frame ---
nitron_apply_movement:  dec     ent_timer,x             ; decrement frame delay
        lda     ent_y_sub,x             ; Y sub-pixel position
        clc                             ; (16-bit add: sub + whole)
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x              ; Y pixel position
        adc     ent_yvel,x
        sta     ent_y_px,x              ; store updated Y pixel
        lda     ent_facing,x            ; check facing direction
        and     #$02
        bne     nitron_move_left        ; bit 1 set = facing left
        jsr     move_sprite_right       ; move_sprite_right
        bcs     nitron_after_move
        bcc     nitron_after_move
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
        .byte   $00,$C3,$00,$6A,$01,$D9,$01,$00
        .byte   $02,$D9,$01,$6A,$01,$C3,$00,$00
        .byte   $00,$3D,$FF,$96,$FE,$27,$FE
nitron_xvel_lo:  .byte   $00            ; check direction
nitron_xvel_table:  .byte   $00,$C3,$00,$6A,$01,$D9,$01,$00
        .byte   $02,$D9,$01,$6A,$01,$C3,$00,$00
        .byte   $00,$3D,$FF,$96,$FE,$27,$FE,$00
        .byte   $FE,$27,$FE,$96,$FE,$3D,$FF

; --- spawn_nitron_bomb: drop a bomb projectile below Nitron ---
nitron_spawn_bomb:  jsr     find_enemy_freeslot_y               ; find free enemy slot
        bcs     nitron_spawn_rts               ; no slot: abort
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; facing -> offset (0 or 2)
        and     #$02
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     gyoraibo_child_x_off,y  ; add X offset for facing dir
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     gyoraibo_child_x_scr,y  ; add screen carry
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x              ; parent Y pixel
        clc
        adc     #$10                    ; bomb spawns 16 px below
        sta     ent_y_px,y
        lda     #$00                    ; HP = 0 (indestructible)
        sta     ent_hp,y
        lda     #$81                    ; init child with OAM $81
        jsr     init_child_entity       ; init child entity (OAM $81)
        lda     #$26                    ; AI routine = $26 (falling bomb)
        sta     ent_routine,y
        lda     #$93                    ; damage flags = projectile
        sta     ent_hitbox,y
nitron_spawn_rts:  rts

; --- Nitron bomb AI (routine $26): fall with gravity, explode on impact ---
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     nitron_status_check     ; nonzero -> skip init
        jsr     reset_gravity                   ; reset_gravity
        inc     ent_status,x            ; advance to state 1
nitron_status_check:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = explosion anim
        bne     nitron_animation_init   ; -> check explosion anim
        ldy     #$12                    ; gravity speed index $12
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     nitron_climb_return
        lda     #$71                    ; explosion anim $71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #SFX_ATTACK             ; play attack sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        inc     ent_status,x            ; advance to explosion state
nitron_climb_return:  rts

nitron_animation_init:  lda     ent_anim_id,x
        cmp     #$71
        bne     nitron_climb_return     ; not explosion anim -> return
        lda     ent_anim_state,x        ; check anim state
        cmp     #$04                    ; state 4 = anim complete
        bne     nitron_climb_return     ; not done -> return
        lda     #$80                    ; switch to OAM $80
        jmp     reset_sprite_anim                   ; reset_sprite_anim

; ===========================================================================
; main_unknown_27 — Debris / explosion child entity
; Spawned by breakable objects. Checks if break animation ($71) has
; completed (anim_state $04), then switches to a secondary OAM ($92).
; ===========================================================================
main_unknown_27:
        lda     ent_anim_id,x           ; check current OAM sprite
        cmp     #$71                    ; break anim $71?
        bne     nitron_homing_jump      ; no -> nutton homing AI
        lda     ent_anim_state,x        ; check anim state
        cmp     #$04                    ; state 4 = anim complete
        bne     nitron_done             ; not done -> return
        lda     #$92                    ; switch to OAM $92
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$40                    ; X/Y speed sub = $40
        sta     ent_xvel_sub,x          ; set X speed sub
        sta     ent_yvel_sub,x          ; set Y speed sub
        lda     #$00                    ; X/Y speed whole = 0
        sta     ent_xvel,x              ; set X speed whole
        sta     ent_yvel,x              ; set Y speed whole
        lda     #$C0                    ; hitbox $C0
        sta     ent_hitbox,x            ; set hitbox
        lda     #$01                    ; HP = 1
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
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     gyoraibo_state_dispatch ; nonzero -> skip init
        sta     ent_yvel,x              ; Y speed whole = 0
        lda     #$80                    ; Y speed sub = $80
        sta     ent_yvel_sub,x
        inc     ent_status,x            ; advance to state 1
gyoraibo_state_dispatch:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = fire phase
        bne     gyoraibo_fire_phase     ; -> firing logic
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     gyoraibo_move_left      ; 0 = left
        ldy     #$14                    ; speed index $14
        jsr     move_right_collide                   ; move_right_collide
        jmp     gyoraibo_move_check

gyoraibo_move_left:  ldy     #$15
        jsr     move_left_collide                   ; move_left_collide
gyoraibo_move_check:  bcc     gyoraibo_collision  ; no wall hit -> check fire
        inc     ent_var2,x              ; set wall-hit flag
gyoraibo_move_down:  jmp     move_sprite_down               ; move_sprite_down

gyoraibo_collision:  lda     ent_var2,x ; check wall-hit flag
        bne     gyoraibo_move_down      ; hit wall -> descend
        lda     ent_var1,x              ; check fired flag
        bne     gyoraibo_rts            ; already fired -> return
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$08                    ; within 8 px of player?
        bcs     gyoraibo_rts            ; no -> return
        inc     ent_status,x            ; advance to fire state
        inc     ent_var1,x              ; set fired flag
        lda     #$33                    ; open mouth anim $33
        jsr     reset_sprite_anim                   ; reset_sprite_anim
gyoraibo_rts:  rts

gyoraibo_fire_phase:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$33                    ; still in mouth-open anim?
        bne     gyoraibo_rts            ; no -> return
        lda     ent_anim_frame,x        ; check anim frame counter
        ora     ent_anim_state,x        ; or anim state
        bne     gyoraibo_rts            ; not done -> return
        lda     #$32                    ; swimming anim $32
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear X speed sub
        sta     ent_xvel_sub,x
        lda     #$04                    ; X speed = 4 (speed boost)
        sta     ent_xvel,x
        dec     ent_status,x            ; back to swim state
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     gyoraibo_no_slot        ; no slot -> skip
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     gyoraibo_child_x_off,y  ; add child X offset
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     gyoraibo_child_x_scr,y  ; add screen carry
        ldy     L0000
        sta     ent_x_scr,y             ; set child X screen
        pla
        sta     ent_x_px,y              ; set child X pixel
        lda     ent_y_px,x              ; parent Y pixel
        sec
        sbc     #$10                    ; spawn 16 px above parent
        sta     ent_y_px,y              ; set child Y pixel
        lda     #$00                    ; HP = 0
        sta     ent_hp,y
        lda     #$34                    ; entity type $34
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_routine,x           ; check parent routine
        cmp     #$28                    ; routine $28?
        beq     gyoraibo_set_routine    ; yes -> set child routine $29
        lda     #$45                    ; else child routine = $45
        sta     ent_routine,y
        bne     gyoraibo_set_hitbox
gyoraibo_set_routine:  lda     #$29
        sta     ent_routine,y
gyoraibo_set_hitbox:  lda     #$C0
        sta     ent_hitbox,y
gyoraibo_no_slot:  rts

gyoraibo_child_x_off:  .byte   $00
gyoraibo_child_x_scr:  .byte   $00,$00,$00
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     gyoraibo_status_check   ; nonzero -> skip init
        sta     ent_timer,x             ; clear timer
        sta     ent_yvel_sub,x          ; Y speed sub = 0
        lda     #$02                    ; Y speed whole = 2 (rising)
        sta     ent_yvel,x
        inc     ent_status,x            ; advance to state 1
gyoraibo_status_check:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = ceiling hit state
        bne     gyoraibo_timer_check
        ldy     #$17                    ; speed index $17
        jsr     move_up_collide         ; move upward with collision
        bcc     gyoraibo_timer_check    ; no ceiling hit -> check timer
        inc     ent_status,x            ; advance to ceiling-hit state
        lda     #$71                    ; explosion anim $71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        lda     #$56                    ; set routine $56
        sta     ent_routine,x
        rts

gyoraibo_timer_check:  lda     ent_timer,x  ; check timer
        bne     penpen_maker_rts        ; nonzero -> return
        lda     ent_routine,x           ; check entity routine
        cmp     #$29                    ; routine $29?
        beq     gyoraibo_spawn_check
        lda     ent_y_px,x              ; check Y position
        cmp     #$62                    ; above Y=$62?
        bcs     penpen_maker_rts        ; no -> return
        bcc     gyoraibo_increment_timer
gyoraibo_spawn_check:  lda     ent_y_px,x  ; check Y position
        cmp     #$B4                    ; above Y=$B4?
        bcs     penpen_maker_rts        ; no -> return
gyoraibo_increment_timer:  inc     ent_timer,x  ; set timer (stop spawning)
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
        lda     #$68                    ; entity type $68
        jsr     init_child_entity                   ; init_child_entity
penpen_maker_rts:  rts

; ===========================================================================
; main_penpen_maker — Penpen Maker (Gemini Man stage pipe spawner)
; Periodically spawns Penpen enemies. Changes palette colors on spawn.
; Uses a pseudo-random timer between spawns.
; ===========================================================================
main_penpen_maker:
        lda     ent_status,x            ; state 0: init
        and     #$0F                    ; extract sub-state
        bne     penpen_maker_spawn_loop ; nonzero -> skip init
        ldy     #$02                    ; Y = 2 (3 palette bytes)
penpen_maker_palette_init:  lda     penpen_maker_palette_data,y
        sta     $060D,y
        sta     $062D,y
        dey
        bpl     penpen_maker_palette_init
        sty     palette_dirty           ; flag palette update needed
        inc     ent_status,x            ; advance to state 1
        jsr     penpen_maker_random_timer  ; get random spawn timer
        sta     ent_timer,x             ; store as entity timer
penpen_maker_spawn_loop:  lda     ent_y_px,x  ; save Y position
        pha
        sec
        sbc     #$10                    ; offset 16 px up for collision
        sta     ent_y_px,x
        lda     #$00                    ; clear hitbox during check
        sta     ent_hitbox,x
        lda     ent_anim_id,x           ; save current OAM ID
        pha
        jsr     process_sprites_top_spin_check  ; run collision/damage check
        pla
        sta     ent_anim_id,x           ; restore OAM ID
        pla
        sta     ent_y_px,x              ; restore Y position
        lda     ent_hp,x                ; check HP
        bne     penpen_maker_alive      ; alive -> continue
        sta     ent_var1,x              ; clear var1
        sta     ent_var2,x              ; clear var2
        lda     #$63                    ; set death routine $63
        sta     ent_routine,x
        rts

penpen_maker_alive:  lda     #$98       ; set normal hitbox $98
        sta     ent_hitbox,x
        dec     ent_timer,x             ; decrement spawn timer
        bne     penpen_maker_timer_rts  ; not zero -> wait
        jsr     penpen_maker_spawn_penpen  ; spawn a penpen enemy
        jsr     penpen_maker_random_timer  ; get next random timer
        sta     ent_timer,x             ; store as spawn timer
penpen_maker_timer_rts:  rts

penpen_maker_random_timer:  lda     $E4 ; pseudo-random shift register
        adc     $E5
        sta     $E5
        and     #$03                    ; mask to 0-3
        tay
        lda     penpen_maker_timer_table,y
        rts

penpen_maker_timer_table:  .byte   $3C,$1E,$78,$3C
        lda     ent_var1,x              ; check spawn phase counter
        bne     penpen_maker_dec_var1   ; nonzero -> decrement
        lda     #$0A                    ; spawn delay = 10 frames
        sta     ent_var1,x
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     penpen_maker_spawn_rts
        lda     #SFX_PLATFORM           ; play platform spawn sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     #$71                    ; explosion anim $71
        jsr     init_child_entity                   ; init_child_entity
        lda     #$19                    ; set child routine $19
        sta     ent_routine,y
        lda     #$00                    ; no hitbox
        sta     ent_hitbox,y            ; no hitbox for child
        sta     ent_timer,y             ; clear child timer
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y
        stx     L0000                   ; save parent slot
        lda     ent_y_px,x              ; parent Y pixel
        sta     $01
        lda     ent_x_px,x              ; parent X pixel
        sta     $02
        lda     ent_var2,x              ; spawn pattern index
        pha
        tax
        lda     $01
        clc
        adc     penpen_maker_spawn_y,x  ; Y offset from table
        sta     ent_y_px,y
        lda     $02
        clc
        adc     penpen_maker_spawn_x,x  ; X offset from table
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
        lda     #$FF                    ; flag palette update
        sta     palette_dirty
        ldx     L0000
        inc     ent_var2,x              ; next spawn pattern
        lda     ent_var2,x
        and     #$03                    ; cycle through 4 patterns
        bne     penpen_maker_spawn_rts
        lda     #$00
        sta     ent_status,x            ; reset entity to re-init
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
        bcs     penpen_maker_spawn_done ; no slot -> return
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     penpen_maker_penpen_x_adj,y  ; add X offset for facing
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     penpen_maker_penpen_x_scr,y  ; add screen carry
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x              ; parent Y pixel
        clc
        adc     #$1C                    ; penpen spawns 28 px below
        sta     ent_y_px,y
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y
        lda     #$02                    ; X speed whole = 2
        sta     ent_xvel,y
        lda     #$93                    ; entity type $93
        jsr     init_child_entity                   ; init_child_entity
        lda     #$46
        sta     ent_routine,y           ; set child routine $46
        lda     #$C0                    ; hitbox $C0
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
penpen_maker_spawn_done:  rts

penpen_maker_penpen_x_adj:  .byte   $F8
penpen_maker_penpen_x_scr:  .byte   $FF,$F8,$FF
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     penpen_maker_check_facing  ; 0 = left
        lda     #$08                    ; speed index $08
        jsr     move_right_collide                   ; move_right_collide
        jmp     penpen_maker_after_move

penpen_maker_check_facing:  ldy     #$09  ; speed index $09
        jsr     move_left_collide                   ; move_left_collide
penpen_maker_after_move:  bcc     penpen_maker_move_rts  ; no wall hit -> continue
        lda     #$71                    ; explosion anim $71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear routine (dead)
        sta     ent_routine,x           ; deactivate entity
penpen_maker_move_rts:  rts

        lda     ent_anim_frame,x        ; check anim frame counter
        cmp     #$02                    ; frame 2?
        bne     penpen_maker_move_rts   ; not yet -> return
        lda     ent_timer,x             ; check spawn count
        cmp     #$08                    ; max 8 parts spawned?
        beq     penpen_maker_move_rts   ; yes -> return
        stx     L0000                   ; save parent slot
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
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     bomber_pepe_bomb_timer  ; nonzero -> skip init
        lda     #$44                    ; Y speed sub = $44
        sta     ent_yvel_sub,x
        lda     #$03                    ; Y speed whole = 3
        sta     ent_yvel,x
        lda     #$1E                    ; bounce timer = 30 frames
        sta     ent_timer,x
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        jsr     penpen_maker_random_timer  ; get random bomb timer
        sta     ent_var1,x              ; store as bomb drop timer
        inc     ent_status,x            ; advance to state 1
bomber_pepe_bomb_timer:  dec     ent_var1,x  ; decrement bomb drop timer
        bne     bomber_pepe_move_phase  ; not zero -> skip bomb
        jsr     bomber_pepe_spawn_bomb  ; spawn bomb child entity
        jsr     penpen_maker_random_timer  ; get next random timer
        sta     ent_var1,x              ; reset bomb drop timer
bomber_pepe_move_phase:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = bouncing
        bne     bomber_pepe_bounce      ; -> bounce logic
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     bomber_pepe_move_left   ; 0 = left
        ldy     #$0A                    ; speed index $0A
        jsr     move_right_collide                   ; move_right_collide
        jmp     bomber_pepe_gravity

bomber_pepe_move_left:  ldy     #$0B    ; speed index $0B
        jsr     move_left_collide                   ; move_left_collide
bomber_pepe_gravity:  ldy     #$20      ; gravity speed index $20
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     bomber_pepe_no_floor    ; no floor hit -> airborne
        lda     #$44                    ; reset Y speed sub = $44
        sta     ent_yvel_sub,x
        lda     #$03                    ; reset Y speed whole = 3
        sta     ent_yvel,x
        inc     ent_status,x            ; advance to bounce state
        lda     #$3D                    ; landing anim $3D
        bne     bomber_pepe_set_anim
bomber_pepe_no_floor:  lda     #$3C     ; flying anim $3C
bomber_pepe_set_anim:  jmp     reset_sprite_anim               ; reset_sprite_anim

bomber_pepe_bounce:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$3B                    ; resting anim $3B?
        beq     bomber_pepe_bounce_timer  ; not yet -> wait for anim
        lda     ent_anim_frame,x        ; check anim frame + state
        ora     ent_anim_state,x        ; combine to check completion
        bne     bomber_pepe_rts         ; not done -> return
        lda     #$3B                    ; set resting anim $3B
        jsr     reset_sprite_anim                   ; reset_sprite_anim
bomber_pepe_bounce_timer:  dec     ent_timer,x  ; decrement rest timer
        bne     bomber_pepe_rts         ; not zero -> wait
        jsr     face_player                   ; face_player
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        dec     ent_status,x            ; back to flying state
        lda     #$3C                    ; new bounce timer = 60 frames
        sta     ent_timer,x
bomber_pepe_rts:  rts

bomber_pepe_spawn_bomb:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     bomber_pepe_spawn_rts
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     bomber_pepe_bomb_x_off,y  ; add X offset for facing
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     bomber_pepe_bomb_x_facing,y  ; add screen carry
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x              ; copy parent Y to bomb
        sta     ent_y_px,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$3E                    ; entity type $3E (bomb)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$1D                    ; child routine $1D
        sta     ent_routine,y
        lda     #$C0                    ; hitbox $C0
        sta     ent_hitbox,y
bomber_pepe_spawn_rts:  rts

bomber_pepe_bomb_x_off:  .byte   $08
bomber_pepe_bomb_x_facing:  .byte   $00,$F8,$FF
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     bomber_pepe_bomb_fall_init  ; nonzero -> skip init
        sta     ent_timer,x             ; clear bounce counter
        lda     #$1E                    ; fuse timer = 30 frames
        sta     ent_var1,x
        inc     ent_status,x            ; advance to state 1
bomber_pepe_bomb_fall_init:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = fuse expired
        bne     bomber_pepe_bounce_countdown  ; -> countdown to explode
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     bomber_pepe_bomb_move_left  ; 0 = left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     bomber_pepe_bomb_gravity

bomber_pepe_bomb_move_left:  jsr     move_sprite_left               ; move_sprite_left
bomber_pepe_bomb_gravity:  ldy     #$08 ; gravity speed index $08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     bomber_pepe_bomb_floor_hit  ; floor hit -> bounce
        lda     ent_timer,x             ; get bounce index
        tay
        lda     bomber_pepe_bomb_yvel_sub,y  ; Y speed sub from table
        sta     ent_yvel_sub,x
        lda     bomber_pepe_bomb_yvel,y ; Y speed whole from table
        sta     ent_yvel,x
        lda     bomber_pepe_bomb_xvel_sub,y  ; X speed sub from table
        sta     ent_xvel_sub,x
        lda     bomber_pepe_bomb_xvel,y ; X speed whole from table
        sta     ent_xvel,x
        inc     ent_timer,x             ; next bounce index
        lda     ent_timer,x
        cmp     #$03                    ; 3 bounces done?
        bcc     bomber_pepe_bomb_floor_hit  ; no -> keep bouncing
        inc     ent_status,x            ; advance to explode state
bomber_pepe_bomb_floor_hit:  rts

bomber_pepe_bounce_countdown:  dec     ent_var1,x  ; decrement fuse timer
        bne     bomber_pepe_bounce_countdown  ; not zero -> wait
        lda     #$71                    ; explosion anim $71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x             ; clear timer
        lda     #$39                    ; set explode routine $39
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
        lda     ent_anim_id,x           ; check OAM sprite ID
        cmp     #$2F                    ; is this the Nutton (bolt)?
        bne     bolton_idle_logic               ; no -> Bolton (nut body) logic
        jmp     nutton_homing_entry               ; yes -> Nutton homing toward player

; -- Bolton (nut body) logic --

bolton_idle_logic:  lda     ent_status,x  ; check entity state
        and     #$0F                    ; state 0 = idle, waiting
        bne     bolton_state_check               ; nonzero = bolt already launched
        jsr     bolton_check_range               ; check player range + find free slot
        bcs     bolton_return               ; carry set = can't fire, return
        lda     ent_flags,x             ; clear bit 2 (mouth indicator)
        and     #$FB
        sta     ent_flags,x
        inc     ent_status,x            ; advance to state 1
        lda     #$66                    ; Y speed = $00.66/frame
        sta     ent_yvel_sub,x          ; set Y speed sub
        lda     #$00
        sta     ent_yvel,x
        lda     #$2E                    ; spawn child entity type $2E
        jsr     init_child_entity       ; init Nutton child entity
        lda     #$31                    ; child AI routine = $31
        sta     ent_routine,y           ; set child routine
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y
        lda     camera_x_lo             ; child X = camera left + 4
        clc                             ; (spawn at left side of screen)
        adc     #$04
        sta     ent_x_px,y
        lda     camera_screen           ; child X screen = camera screen
        adc     #$00
        sta     ent_x_scr,y
        lda     #$00                    ; child X speed = $04.00 px/frame
        sta     ent_xvel_sub,y          ; child X speed sub = 0
        lda     #$04
        sta     ent_xvel,y
        lda     ent_x_px,x                 ; child target X = Bolton X - 8
        sec                             ; (stored in child ent_timer/ent_var2)
        sbc     #$08
        sta     ent_timer,y
        lda     ent_x_scr,x             ; target X screen
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

nutton_homing_entry:  lda     ent_x_px  ; player X pixel
        sec
        sbc     ent_x_px,x              ; subtract entity X pixel
        pha
        lda     ent_x_scr               ; player X screen
        sbc     ent_x_scr,x             ; subtract entity X screen
        pla
        beq     nutton_move_y           ; same X -> move vertically
        bcc     nutton_move_left        ; player is left -> move left
        jsr     move_sprite_right                   ; move_sprite_right
        lda     ent_flags,x             ; set H-flip flag (facing right)
        ora     #$40                    ; set bit 6
        sta     ent_flags,x
        jmp     nutton_move_y

nutton_move_left:  jsr     move_sprite_left               ; move_sprite_left
        lda     ent_flags,x             ; clear H-flip (facing left)
        and     #$BF                    ; clear bit 6
        sta     ent_flags,x
nutton_move_y:  lda     ent_y_px        ; player Y pixel
        sec
        sbc     ent_y_px,x              ; subtract entity Y pixel
        beq     bolton_return           ; same Y -> done
        bcs     nutton_move_down        ; player below -> move down
        jmp     move_sprite_up                   ; move_sprite_up

nutton_move_down:  jmp     move_sprite_down               ; move_sprite_down

bolton_check_range:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$44                    ; within 68 px?
        bcs     bolton_range_fail       ; no -> can't fire
        lda     ent_x_px,x              ; entity X pixel
        sec
        sbc     camera_x_lo             ; subtract camera X
        sta     L0000
        lda     ent_x_scr,x             ; entity X screen
        sbc     camera_screen           ; subtract camera screen
        bcc     bolton_too_many         ; off screen left -> too many
        lda     L0000
        cmp     #$80                    ; screen-relative X < $80?
        bcc     bolton_too_many         ; left half -> too many
        lda     #$00                    ; clear nutton counter
        sta     $01
        stx     L0000
        ldy     #$1F                    ; scan enemy slots $1F-$10
bolton_count_nuttons:  cpy     L0000
        beq     bolton_count_next
        lda     ent_status,y            ; check if slot active
        bpl     bolton_count_next
        lda     ent_flags,y             ; check if child entity
        bpl     bolton_count_next
        lda     ent_routine,y           ; check if routine $30
        cmp     #$30                    ; bolton/nutton routine?
        bne     bolton_count_next
        inc     $01                     ; count active nuttons
bolton_count_next:  dey
        cpy     #$0F
        bne     bolton_count_nuttons
        lda     $01
        cmp     #$03                    ; max 3 nuttons allowed
        beq     bolton_too_many
        ldy     L0000
bolton_find_slot:  lda     ent_status,y
        bpl     bolton_range_ok
        dey
        cpy     #$0F
        bne     bolton_find_slot
bolton_too_many:  lda     #$00
        sta     ent_status,x
bolton_range_fail:  sec
        rts

bolton_range_ok:  clc
        rts

        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     nutton_return_state
        sta     ent_var3,x              ; clear sound-played flag
        jsr     move_sprite_right                   ; move_sprite_right
        lda     ent_timer,x             ; target X pixel
        sec
        sbc     ent_x_px,x              ; subtract current X
        lda     ent_var2,x              ; target X screen
        sbc     ent_x_scr,x             ; subtract current X screen
        bcs     nutton_reached_target   ; past target -> snap to target
        lda     ent_timer,x             ; snap X to target pixel
        sta     ent_x_px,x
        lda     ent_var2,x              ; snap X screen to target
        sta     ent_x_scr,x
        inc     ent_status,x            ; advance to state 1
        ldy     ent_var1,x              ; get parent Bolton slot
        lda     #$08                    ; set Bolton flash timer = 8
        sta     ent_timer,y
        sta     ent_timer,x             ; child X screen
nutton_reached_target:  lda     #$00    ; clear anim frame
        sta     ent_anim_frame,x
        rts

nutton_return_state:  lda     ent_var3,x  ; check sound-played flag
        bne     nutton_sound_and_fly
        lda     #SFX_TURRET_FIRE        ; play turret fire sound
        jsr     submit_sound_ID                   ; submit_sound_ID
        inc     ent_var3,x
nutton_sound_and_fly:  lda     #$01     ; set invincibility frame
        sta     $95                     ; target X screen
        lda     ent_timer,x             ; check flash timer
        beq     nutton_dock_ready       ; zero -> ready to dock
        lda     ent_anim_frame,x        ; check anim frame
        bne     nutton_return_end       ; nonzero -> skip
        dec     ent_timer,x             ; decrement flash timer
        inc     ent_x_px,x              ; shift X right 1 px
        rts

nutton_dock_ready:  lda     ent_anim_state,x  ; check parent anim state
        bne     nutton_return_end       ; nonzero -> wait
        sta     ent_anim_frame,x        ; clear own anim frame
        ldy     ent_var1,x              ; get parent Bolton slot
        lda     ent_anim_state,y        ; check parent anim state
        bne     nutton_return_end       ; nonzero -> wait
        sta     ent_anim_frame,y        ; clear parent anim frame
        sta     ent_timer,y             ; clear parent timer
        sta     ent_status,x            ; deactivate this nutton
        lda     #$2F                    ; restore Bolton OAM to $2F
        sta     ent_anim_id,y
        lda     #$C0                    ; set Bolton hitbox $C0
        sta     ent_hitbox,y
        lda     #$01                    ; set Bolton HP = 1
        sta     ent_hp,y
nutton_return_end:  rts

; ===========================================================================
; main_have_su_bee — Have "Su" Bee (bee carrier, Snake Man stage)
; Flies toward player, hovers while carrying a bee, then releases it.
; After release, reverses direction and flies away. ent_timer=pre-launch
; delay, ent_var2=hover timer, ent_var1=release range check flag.
; ===========================================================================
main_have_su_bee:

        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     have_su_bee_active      ; nonzero -> active
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$28                    ; within 40 px?
        bcs     have_su_bee_init_rts    ; no -> return
        inc     ent_status,x            ; advance to active state
        lda     #$1E                    ; pre-launch delay = 30 frames
        sta     ent_timer,x
        lda     #$30                    ; hover timer = 48 frames
        sta     ent_var2,x
        lda     ent_flags,x             ; toggle sprite flag bit 2
        eor     #$04
        sta     ent_flags,x             ; store updated flags
        lda     ent_facing,x            ; get facing direction
        and     #$02                    ; facing -> offset (0 or 2)
        tay
        lda     ent_x_px,x              ; current X pixel
        clc
        adc     have_su_bee_x_off,y     ; add X offset for facing
        pha
        lda     ent_x_scr,x             ; current X screen
        adc     have_su_bee_x_scr,y     ; add screen carry
        sta     ent_x_scr,x             ; update X screen
        pla
        sta     ent_x_px,x              ; update X pixel
        jsr     face_player                   ; face_player
have_su_bee_init_rts:  rts

have_su_bee_active:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = hovering
        bne     have_su_bee_hover       ; -> hover logic
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     have_su_bee_move_left   ; 0 = left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     have_su_bee_dist_check

have_su_bee_move_left:  jsr     move_sprite_left               ; move_sprite_left
have_su_bee_dist_check:  lda     ent_var1,x  ; check range flag
        bne     have_su_bee_move_rts    ; already past range -> return
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$50                    ; within 80 px?
        bcc     have_su_bee_move_rts    ; yes -> keep flying
        inc     ent_status,x            ; advance to hover state
        inc     ent_var1,x              ; set range check flag
        lda     #$80                    ; Y speed sub = $80
        sta     ent_yvel_sub,x
        lda     #$00                    ; Y speed whole = 0
        sta     ent_yvel,x
        lda     ent_facing,x            ; check facing direction
        and     #$01
        beq     have_su_bee_flip_left   ; facing left -> flip left
        lda     ent_flags,x             ; clear H-flip (bit 6)
        and     #$BF
        sta     ent_flags,x
        rts

have_su_bee_flip_left:  lda     ent_flags,x  ; set H-flip (bit 6)
        ora     #$40
        sta     ent_flags,x
have_su_bee_move_rts:  rts

have_su_bee_hover:  dec     ent_var2,x  ; decrement hover timer
        bne     have_su_bee_vert_move   ; not zero -> keep hovering
        dec     ent_status,x            ; back to fly-away state
        lda     ent_facing,x            ; reverse facing direction
        eor     #$03
        sta     ent_facing,x
        lda     #$3A                    ; release anim $3A
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        jmp     have_su_bee_spawn_bee

have_su_bee_vert_move:  lda     ent_facing,x  ; check facing direction
        and     #$01                    ; bit 0 = up
        beq     have_su_bee_move_down   ; 0 = down
        jsr     move_sprite_up                   ; move_sprite_up
        jmp     have_su_bee_hover_timer

have_su_bee_move_down:  jsr     move_sprite_down               ; move_sprite_down
have_su_bee_hover_timer:  dec     ent_timer,x  ; decrement vert move timer
        bne     have_su_bee_move_rts    ; not zero -> continue
        lda     #$3C                    ; reset timer = 60 frames
        sta     ent_timer,x
        lda     ent_facing,x            ; reverse vert direction
        eor     #$03
        sta     ent_facing,x
        rts

have_su_bee_x_off:  .byte   $50
have_su_bee_x_scr:  .byte   $00,$B0,$FF
have_su_bee_spawn_bee:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     have_su_bee_spawn_rts
        sty     $01                     ; save child slot
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     gyoraibo_child_x_off,y  ; add X offset for facing
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     gyoraibo_child_x_scr,y  ; add screen carry
        ldy     $01
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x              ; parent Y pixel
        clc
        adc     #$18                    ; bee spawns 24 px below
        sta     ent_y_px,y
        lda     #$35                    ; entity type $35 (bee)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$2F                    ; child routine $2F
        sta     ent_routine,y
        lda     #$CF                    ; hitbox $CF
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$50                    ; Y speed sub = $50
        sta     ent_yvel_sub,y
        lda     #$04                    ; Y speed whole = 4 (fast fall)
        sta     ent_yvel,y
have_su_bee_spawn_rts:  rts

; ===========================================================================
; main_beehive — Beehive (Snake Man stage)
; Falls until hitting floor, then explodes and spawns 5 Chibee enemies
; at offset positions around the hive. Changes AI routine to $3A (dead).
; ===========================================================================
main_beehive:
        ldy     #$08                    ; speed index $08
        jsr     move_down_collide                   ; move_down_collide
        bcc     have_su_bee_spawn_rts
        lda     #$71                    ; explosion anim $71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        lda     #$3A                    ; set dead routine $3A
        sta     ent_routine,x
        lda     #$00                    ; clear spawn counter
        sta     $01
beehive_find_freeslot:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     beehive_done            ; no slot -> done
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        lda     $01                     ; spawn counter * 2 for offset
        asl     a
        tay
        lda     ent_x_px,x              ; parent X pixel
        clc
        adc     bee_spawn_x_offset,y    ; add X offset from table
        pha
        lda     ent_x_scr,x             ; parent X screen
        adc     beehive_spawn_offsets,y ; add screen carry from table
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        ldy     $01
        lda     ent_y_px,x              ; parent Y pixel
        clc
        adc     bee_spawn_y_offset,y    ; add Y offset from table
        ldy     L0000
        sta     ent_y_px,y
        lda     #$41                    ; entity type $41 (chibee)
        jsr     init_child_entity                   ; init_child_entity
        lda     #$00                    ; clear X speed
        sta     ent_xvel_sub,y
        sta     ent_xvel,y
        lda     #$C0                    ; hitbox $C0
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$3B                    ; child routine $3B
        sta     ent_routine,y
        lda     #$08                    ; timer = 8
        sta     ent_timer,y             ; var1 = 8
        sta     ent_var1,y
        lda     #$10                    ; var2 = 16
        sta     ent_var2,y
        lda     #$33                    ; Y speed sub = $33
        sta     ent_yvel_sub,y
        lda     #$01                    ; Y speed whole = 1
        sta     ent_yvel,y
        inc     $01                     ; next spawn index
        lda     $01
        cmp     #$05                    ; spawned all 5 chibees?
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
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     monking_state_router    ; nonzero -> active
        inc     ent_status,x            ; advance to state 1
        lda     #$3C                    ; Y speed sub = $3C
        sta     ent_yvel_sub,x
        lda     #$09                    ; Y speed whole = 9 (big jump)
        sta     ent_yvel,x
        lda     #$1E                    ; land timer = 30 frames
        sta     ent_timer,x
        jsr     set_sprite_hflip                   ; set_sprite_hflip
monking_state_router:  lda     ent_status,x  ; check state
        and     #$0F                    ; extract sub-state
        cmp     #$02                    ; state 2 = grounded
        beq     monking_grounded
        cmp     #$03                    ; state 3 = retreat
        bne     monking_jump_state
        jmp     monking_retreat

monking_jump_state:  lda     ent_timer,x  ; check land timer
        bne     monking_timer_dec       ; nonzero -> count down
        lda     ent_anim_id,x           ; check current OAM
        cmp     #$44                    ; landing anim $44?
        beq     monking_land_setup      ; yes -> start landing setup
        lda     ent_var1,x              ; check attack flag
        bne     monking_land_setup      ; nonzero -> start landing
        lda     #$45                    ; jumping anim $45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        ldy     #$15                    ; gravity speed index $15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     $10                     ; collision result
        and     #$10                    ; bit 4 = floor hit
        beq     monking_jump_rts        ; no floor -> keep jumping
        lda     #$44                    ; landing anim $44
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        rts

monking_timer_dec:  dec     ent_timer,x
monking_jump_rts:  rts

monking_land_setup:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$45                    ; jumping anim $45?
        beq     monking_apply_gravity   ; yes -> apply gravity
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$28                    ; within 40 px?
        bcs     monking_jump_rts        ; no -> return
        lda     #$45                    ; set jumping anim $45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_var1,x              ; set attack flag
monking_apply_gravity:  ldy     #$15    ; gravity speed index $15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     monking_jump_rts        ; no floor -> keep falling
        lda     #$43                    ; grounded anim $43
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        inc     ent_status,x            ; advance to grounded state
monking_grounded:  lda     ent_var2,x   ; check retreat flag
        bne     monking_walk_dir        ; nonzero -> walk direction
        lda     ent_hp,x                ; check HP
        cmp     #$04                    ; HP = 4 (full)?
        bne     monking_walk_dir        ; damaged -> walk direction
        inc     ent_status,x            ; advance to retreat state
        lda     #$3C                    ; Y speed sub = $3C
        sta     ent_yvel_sub,x
        lda     #$09                    ; Y speed whole = 9
        sta     ent_yvel,x
        inc     ent_var2,x              ; set retreat flag
        rts

monking_walk_dir:  lda     ent_facing,x ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     monking_move_left       ; 0 = left
        ldy     #$16                    ; speed index $16
        jsr     move_right_collide                   ; move_right_collide
        jmp     monking_gravity_check

monking_move_left:  ldy     #$17        ; speed index $17
        jsr     move_left_collide                   ; move_left_collide
monking_gravity_check:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$43                    ; grounded anim $43?
        beq     monking_attack_anim     ; yes -> attack sequence
        ldy     #$15                    ; gravity speed index $15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     monking_attack_rts      ; no floor -> keep falling
        lda     #$BB                    ; Y speed sub = $BB
        sta     ent_yvel_sub,x
        lda     #$06                    ; Y speed whole = 6
        sta     ent_yvel,x              ; grounded anim $43
        lda     #$43
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$01                    ; start at anim state 1
        sta     ent_anim_state,x
        lda     #$00                    ; clear anim frame
        sta     ent_anim_frame,x
        jsr     face_player                   ; face_player
monking_attack_rts:  rts

monking_attack_anim:  lda     ent_anim_state,x  ; check anim state
        bne     monking_attack_finish   ; nonzero -> attack finished
monking_attack_finish:  lda     #$46    ; attack throw anim $46
        jmp     reset_sprite_anim                   ; reset_sprite_anim

monking_retreat:  lda     ent_anim_id,x ; check current OAM
        cmp     #$44                    ; landing anim $44?
        beq     monking_retreat_timer   ; yes -> count down retreat
        lda     #$45                    ; jumping anim $45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        ldy     #$15                    ; gravity speed index $15
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     $10                     ; collision result
        and     #$10                    ; bit 4 = floor hit
        beq     monking_attack_rts      ; no floor -> keep jumping
        lda     #$44                    ; landing anim $44
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$5A                    ; retreat timer = 90 frames
        sta     ent_var3,x
monking_retreat_timer:  dec     ent_var3,x  ; decrement retreat timer
        bne     monking_retreat_rts     ; not zero -> wait
        dec     ent_status,x            ; back to jump state
        jsr     reset_gravity                   ; reset_gravity
        lda     #$45                    ; set jumping anim $45
        jsr     reset_sprite_anim                   ; reset_sprite_anim
monking_retreat_rts:  rts

; ===========================================================================
; main_wanaan — Wanaan (pipe snake, Snake Man stage)
; Hides in pipe, snaps out to bite when player is within $18 px both X and Y.
; Snap sequence: presnap delay → upward snap (6 frames) → sound → downward
; snap (16 frames) → retract to original position and re-hide.
; ===========================================================================
main_wanaan:
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     wanaan_active           ; nonzero -> active
        sta     ent_yvel_sub,x          ; clear Y speed sub
        lda     #$02                    ; Y speed whole = 2
        sta     ent_yvel,x              ; set Y speed
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$18                    ; within 24 px Y?
        bcs     wanaan_idle_rts         ; no -> too far, return
        jsr     entity_x_dist_to_player ; check X distance
        cmp     #$18                    ; within 24 px X?
        bcs     wanaan_idle_rts         ; no -> too far, return
        inc     ent_status,x            ; advance to snap state
        lda     #$21                    ; pre-snap delay = 33 frames
        sta     ent_timer,x             ; store delay timer
        lda     #$06                    ; upward snap = 6 frames
        sta     ent_var1,x              ; store snap timer
        lda     ent_y_px,x              ; save current Y position
        sta     ent_var2,x              ; store as original Y
        lda     #$10                    ; downward retract = 16 frames
        sta     ent_var3,x
wanaan_idle_rts:  rts

wanaan_active:  lda     ent_flags,x     ; check sprite flags
        and     #$04                    ; bit 2 = pre-snap phase
        beq     wanaan_snap_phase       ; clear -> snap phase
        dec     ent_timer,x             ; decrement pre-snap timer
        bne     wanaan_idle_rts         ; not done -> return
        lda     ent_flags,x             ; timer expired
        eor     #$04                    ; toggle bit 2 (enter snap)
        sta     ent_flags,x
        lda     #$A3                    ; set shape $A3 (extended hitbox)
        sta     ent_hitbox,x
wanaan_snap_phase:  lda     ent_status,x  ; check entity state
        and     #$02                    ; bit 1 = past snap
        bne     wanaan_downsnap         ; set -> downward retract
        lda     #$00
        sta     ent_anim_frame,x        ; freeze on open mouth frame
        sta     ent_anim_state,x
        dec     ent_y_px,x
        dec     ent_y_px,x                 ; move up 3 pixels
        dec     ent_y_px,x
        dec     ent_var1,x                 ; upward snap timer
        bne     wanaan_idle_rts                   ; not expired yet? return
        inc     ent_status,x
        lda     #SFX_CLAMP              ; play clamp snap sound
        jsr     submit_sound_ID         ; submit_sound_ID
        rts

wanaan_downsnap:  lda     #$01
        sta     ent_anim_state,x        ; set closed mouth frame
        lda     #$00
        sta     ent_anim_frame,x
        jsr     move_sprite_down                   ; move_sprite_down
        dec     ent_var3,x                 ; downward snap timer
        bne     wanaan_idle_rts                   ; not expired yet? return
        lda     #$00                    ; retract complete
        sta     ent_anim_frame,x        ; reset anim frame
        sta     ent_anim_state,x
        lda     ent_var2,x              ; restore original Y position
        sta     ent_y_px,x              ; set Y pixel
        lda     ent_flags,x
        ora     #$94                    ; set active+child+disabled
        sta     ent_flags,x
        lda     #$80                    ; reset to active/idle
        sta     ent_status,x
        lda     #$83                    ; reset hitbox to $83
        sta     ent_hitbox,x
        rts

; ===========================================================================
; main_komasaburo — Komasaburo (spinning top enemy, Top Man stage)
; Spins in place, periodically fires child projectiles (OAM $E2).
; After 3 firings, enters a different behavioral pattern.
; ent_timer=fire delay, ent_var1=anim timer, ent_var2=fire count.
; ===========================================================================
main_komasaburo:
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     komasaburo_state_check  ; nonzero -> skip init
        jsr     set_sprite_hflip                   ; set_sprite_hflip
        inc     ent_status,x
        lda     #$36                    ; fire delay = 54 frames
        sta     ent_timer,x
        lda     #$10                    ; anim timer = 16
        sta     ent_var1,x
komasaburo_state_check:  lda     ent_status,x  ; check state
        and     #$02                    ; bit 1 = fire phase
        bne     komasaburo_fire_phase   ; -> fire phase
        lda     ent_anim_id,x           ; check current OAM
        cmp     #$D6                    ; fire anim $D6?
        beq     komasaburo_anim_reset   ; yes -> reset anim
        lda     ent_var2,x              ; check fire count
        cmp     #$03                    ; fired 3 times?
        bcs     komasaburo_count_kids   ; yes -> count children
        dec     ent_timer,x             ; decrement fire delay
        bne     komasaburo_anim_idle    ; not zero -> idle anim
        lda     #$D6                    ; fire anim $D6
        jsr     reset_sprite_anim                   ; reset_sprite_anim
komasaburo_anim_reset:  lda     #$00    ; reset anim state
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        jsr     komasaburo_spawn_proj   ; spawn projectile child
        inc     ent_var2,x              ; increment fire count
        lda     #$36                    ; reset fire delay = 54
        sta     ent_timer,x
        inc     ent_status,x            ; advance to fire phase
komasaburo_fire_phase:  lda     #$00    ; freeze anim during fire phase
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        dec     ent_var1,x              ; decrement anim timer
        bne     komasaburo_fire_rts     ; not zero -> wait
        lda     #$10                    ; reset anim timer = 16
        sta     ent_var1,x
        dec     ent_status,x            ; back to idle state
komasaburo_anim_idle:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$C6                    ; idle anim $C6?
        beq     komasaburo_fire_rts     ; already set -> return
        lda     #$C6                    ; set idle anim $C6
        jsr     reset_sprite_anim                   ; reset_sprite_anim
komasaburo_fire_rts:  rts

komasaburo_count_kids:  lda     #$00    ; clear child counter
        sta     L0000
        lda     #$E2                    ; OAM $E2 = child projectile
        sta     $01
        ldy     #$1F                    ; scan slots $1F-$10
komasaburo_scan_start:  lda     ent_status,y  ; check if slot active
        bmi     komasaburo_check_child  ; active -> check OAM
komasaburo_scan_next:  dey
        cpy     #$0F
        bne     komasaburo_scan_start
        lda     L0000                   ; get child count
        cmp     #$03
        beq     komasaburo_scan_done
        dec     ent_var2,x
komasaburo_scan_done:  lda     #$38     ; reset fire delay = 56
        sta     ent_timer,x
        rts

komasaburo_check_child:  lda     $01    ; target OAM to match
        cmp     ent_anim_id,y
        bne     komasaburo_scan_next
        inc     L0000
        jmp     komasaburo_scan_next

komasaburo_spawn_proj:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     komasaburo_spawn_fail   ; no slot -> abort
        lda     ent_facing,x            ; copy parent facing
        sta     ent_facing,y
        lda     ent_x_px,x              ; copy parent X pixel
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; parent Y pixel
        clc
        adc     #$04                    ; child Y = parent Y + 4
        sta     ent_y_px,y
        lda     #$E2                    ; entity type $E2
        jsr     init_child_entity                   ; init_child_entity
        lda     #$98                    ; sprite flags = $98
        sta     ent_flags,y
        lda     #$C0                    ; hitbox $C0
        sta     ent_hitbox,y
        lda     #$42                    ; child routine $42
        sta     ent_routine,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
komasaburo_spawn_fail:  rts

        lda     ent_status,x            ; check entity state
        and     #$0F
        bne     komasaburo_child_fall
        sta     ent_var1,x              ; clear var1
        sta     ent_xvel_sub,x
        lda     #$02                    ; X speed whole = 2
        sta     ent_xvel,x
        inc     ent_status,x            ; advance to state 1
        lda     #$F0                    ; lifetime timer = 240
        sta     ent_timer,x
        jsr     reset_gravity                   ; reset_gravity
komasaburo_child_fall:  ldy     #$08    ; gravity speed index $08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     komasaburo_child_timer
        lda     ent_facing,x
        and     #$01
        beq     komasaburo_child_left
        ldy     #$08                    ; speed index $08
        jsr     move_right_collide                   ; move_right_collide
        jmp     komasaburo_child_move

komasaburo_child_left:  ldy     #$09
        jsr     move_left_collide                   ; move_left_collide
komasaburo_child_move:  bcc     komasaburo_child_timer
        lda     ent_facing,x
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x
komasaburo_child_timer:  dec     ent_timer,x  ; decrement lifetime timer
        bne     komasaburo_child_air    ; not zero -> keep moving
        lda     #$71                    ; explosion anim $71
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00                    ; clear routine (dead)
        sta     ent_routine,x
komasaburo_child_idle:  rts

komasaburo_child_air:  lda     ent_var1,x  ; check var1 (bounce flag)
        bne     komasaburo_child_idle   ; nonzero -> return
        lda     ent_timer,x             ; check timer
        cmp     #$B4                    ; timer = $B4 (180)?
        bne     komasaburo_child_idle   ; no -> return
        lda     #$F0                    ; reset timer to $F0
        sta     ent_timer,x
        inc     ent_var1,x              ; set bounce flag
        rts

; ===========================================================================
; main_mechakkero — Mechakkero (frog robot, Shadow Man stage)
; Hops toward player with gravity. On floor hit, sets random jump height
; and waits on a timer before hopping again. State 0=airborne, 1=grounded.
; ===========================================================================
main_mechakkero:

        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     mechakkero_in_air_state ; nonzero -> grounded state
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     mechakkero_move_left    ; 0 = left
        ldy     #$18                    ; speed index $18
        jsr     move_right_collide                   ; move_right_collide
        jmp     mechakkero_apply_gravity

mechakkero_move_left:  ldy     #$19     ; speed index $19
        jsr     move_left_collide                   ; move_left_collide
mechakkero_apply_gravity:  ldy     #$18 ; gravity speed index $18
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     mechakkero_landing_setup  ; no floor -> airborne
        jsr     mechakkero_random_jump_vel  ; pick random jump velocity
        inc     ent_status,x            ; advance to grounded state
        lda     #$CB                    ; grounded hitbox $CB
        sta     ent_hitbox,x
        lda     #$2B                    ; grounded anim $2B
        bne     mechakkero_reset_anim_exit
mechakkero_landing_setup:  lda     #$C3 ; airborne hitbox $C3
        sta     ent_hitbox,x
        lda     #$2A                    ; airborne anim $2A
mechakkero_reset_anim_exit:  jmp     reset_sprite_anim               ; reset_sprite_anim

mechakkero_in_air_state:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$2B                    ; grounded anim $2B?
        bne     mechakkero_anim_check_alt  ; no -> check alt anim
        lda     ent_anim_state,x        ; check anim state
        cmp     #$02                    ; state 2 = anim done
        bne     mechakkero_air_done     ; not done -> return
        lda     #$29                    ; idle anim $29
        bne     mechakkero_reset_anim_air
mechakkero_anim_check_alt:  lda     #$29  ; idle anim $29
mechakkero_reset_anim_air:  jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     ent_timer,x             ; check ground timer
        bne     mechakkero_timer_dec    ; nonzero -> count down
        lda     #$3C                    ; set hop timer = 60 frames
        sta     ent_timer,x             ; store timer
        dec     ent_status,x            ; back to airborne state
        jsr     face_player             ; face_player
mechakkero_air_done:  rts

mechakkero_timer_dec:  dec     ent_timer,x
        rts

mechakkero_random_jump_vel:  lda     $E4  ; pseudo-random shift register
        adc     $E5
        sta     $E5
        and     #$01                    ; pick index 0 or 1
        tay
        lda     mechakkero_jump_vel_sub,y  ; Y speed sub from table
        sta     ent_yvel_sub,x
        lda     mechakkero_jump_vel,y   ; Y speed whole from table
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
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     top_man_platform_check_proximity  ; nonzero -> skip init
        sta     ent_yvel_sub,x          ; clear Y speed sub
        lda     #$01                    ; Y speed whole = 1
        sta     ent_yvel,x
        inc     ent_status,x            ; advance to state 1
        lda     ent_y_px,x              ; check Y position
        cmp     #$88                    ; below $88?
        bcs     top_man_platform_set_timer  ; yes -> set direction flag
        inc     ent_var1,x              ; set moving-down flag
top_man_platform_set_timer:  lda     #$10  ; conveyor timer = 16 frames
        sta     ent_timer,x
        lda     #$01                    ; face right initially
        sta     ent_facing,x
        rts

top_man_platform_check_proximity:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$16                    ; within 22 px X?
        bcs     top_man_platform_movement_check  ; no -> skip conveyor push
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$15                    ; within 21 px Y?
        bcs     top_man_platform_movement_check  ; no -> skip conveyor push
        lda     ent_facing,x            ; check facing direction
        and     #$02                    ; bit 1 = direction
        bne     top_man_platform_facing_right  ; bit 1 set -> push right
        lda     #$01                    ; push left direction
        bne     top_man_platform_move
top_man_platform_facing_right:  lda     #$02  ; push right direction
top_man_platform_move:  sta     $36     ; store conveyor push dir
        lda     #$00
        sta     $37
        lda     #$01                    ; conveyor push active
        sta     $38
        dec     ent_timer,x             ; decrement conveyor timer
        bne     top_man_platform_movement_check  ; not zero -> skip reverse
        lda     #$10                    ; reset timer = 16
        sta     ent_timer,x
        lda     ent_facing,x            ; reverse conveyor direction
        eor     #$03
        sta     ent_facing,x
top_man_platform_movement_check:  lda     ent_var1,x  ; check direction flag
        bne     top_man_platform_move_down_check  ; 0 = moving up
        jsr     move_sprite_up                   ; move_sprite_up
        lda     ent_y_scr,x             ; check Y screen overflow
        beq     top_man_platform_up_rts ; no overflow -> return
        lda     #$00                    ; clamp Y screen to 0
        sta     ent_y_scr,x
top_man_platform_up_rts:  rts

top_man_platform_move_down_check:  lda     ent_y_px,x  ; get current Y pixel
        pha
        dec     ent_y_px,x              ; move up 1 px for check
        jsr     check_player_collision                   ; check_player_collision
        pla
        sta     ent_y_px,x              ; restore Y pixel
        bcs     top_man_platform_down_movement  ; player on platform -> move
        lda     ent_y_px                ; player Y pixel
        sec
        sbc     ent_y_px,x              ; subtract platform Y
        bcs     top_man_platform_render_update
        lda     ent_y_px
        adc     #$02
        sta     ent_y_px
        cmp     #$F0
        bcc     top_man_platform_render_update
        adc     #$0F                    ; push player below $F0
        sta     ent_y_px
        inc     ent_y_scr               ; wrap to next screen
top_man_platform_render_update:  stx     $0F  ; save entity slot
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

        lda     ent_timer,x             ; check delay timer
        beq     needle_press_check_state  ; zero -> process state
        dec     ent_timer,x             ; count down delay timer
        bne     needle_press_freeze_anim  ; still waiting -> freeze anim
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
        lda     ent_flags,x             ; check sprite flags
        and     #$04                    ; bit 2 = waiting for player
        beq     elecn_apply_yspeed      ; clear -> apply Y speed
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$61                    ; within 97 px?
        bcs     elecn_rts               ; no -> return
        lda     ent_flags,x             ; clear waiting flag (bit 2)
        and     #$FB
        sta     ent_flags,x
        jmp     set_sprite_hflip

elecn_apply_yspeed:  jsr     apply_y_speed               ; apply_y_speed
        lda     ent_y_px,x              ; check Y position
        cmp     #$78                    ; below Y=$78?
        bcc     elecn_rts               ; not yet -> return
        lda     #$11                    ; fire interval = 17 frames
        sta     ent_timer,x
        lda     #$03                    ; spark count = 3
        sta     ent_var1,x
        inc     ent_status,x            ; advance to active state
elecn_anim_check:  lda     ent_anim_id,x  ; check current OAM
        cmp     #$56                    ; fire anim $56?
        bne     elecn_dec_timer         ; no -> decrement timer
        lda     ent_anim_frame,x        ; check anim frame counter
        bne     elecn_rts               ; not done -> return
        lda     #$55                    ; idle anim $55
        jsr     reset_sprite_anim                   ; reset_sprite_anim
elecn_dec_timer:  dec     ent_timer,x   ; decrement fire interval
        lda     ent_timer,x
        bne     elecn_oscillation       ; not zero -> oscillate
        lda     #$11                    ; reset fire interval = 17
        sta     ent_timer,x
        inc     ent_var1,x              ; next spark phase
elecn_oscillation:  lda     ent_var1,x  ; get spark phase
        and     #$03                    ; wrap to 0-3
        sta     ent_var1,x
        tay
        lda     ent_y_sub,x             ; Y sub-pixel position
        clc
        adc     elecn_y_move_sub,y      ; add Y movement sub
        sta     ent_y_sub,x
        lda     ent_y_px,x              ; Y pixel position
        adc     elecn_y_move_whole,y    ; add Y movement whole
        sta     ent_y_px,x
        lda     ent_facing,x            ; check facing direction
        and     #$02                    ; bit 1 = left
        beq     elecn_dist_right        ; right -> check dist right
        lda     ent_var2,x              ; check fire flag
        bne     elecn_move_left         ; already fired -> move left
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        bcs     elecn_activate          ; player behind -> activate
elecn_move_left:  jmp     move_sprite_left               ; move_sprite_left

elecn_dist_right:  lda     ent_var2,x   ; check fire flag
        bne     elecn_move_right        ; already fired -> move right
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        bcc     elecn_activate          ; player behind -> activate
elecn_move_right:  jmp     move_sprite_right               ; move_sprite_right

elecn_activate:  lda     #SFX_APPROACH
        jsr     submit_sound_ID                   ; submit_sound_ID
        lda     #$56
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        stx     L0000                   ; save parent slot
        lda     #$07                    ; 8 sparks (index 7..0)
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
        lda     #$57                    ; init child entity (OAM $57)
        jsr     init_child_entity
        lda     #$80                    ; hitbox $80 (invulnerable)
        sta     ent_hitbox,y
        lda     #$0F                    ; child routine $0F
        sta     ent_routine,y
        lda     ent_x_px,x              ; copy parent X pixel
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; parent Y pixel
        sec
        sbc     #$0C                    ; child Y = parent Y - 12
        sta     ent_y_px,y
        dec     $01                     ; next spark index
        bpl     elecn_spawn_loop        ; loop until all 8 spawned
elecn_spawn_done:  ldx     L0000        ; restore parent slot
        inc     ent_var2,x              ; set fired flag
        rts

elecn_y_move_sub:  .byte   $80,$80,$80,$80
elecn_y_move_whole:  .byte   $FF,$00,$00,$FF
elecn_proj_xvel_sub:  .byte   $00,$6A,$00,$6A,$00,$6A,$00,$6A
elecn_proj_xvel:  .byte   $00,$01,$02,$01,$00,$01,$02,$01
elecn_proj_yvel_sub:  .byte   $00,$96,$00,$6A,$00,$6A,$00,$96
elecn_proj_yvel:  .byte   $FE,$FE,$00,$01,$02,$01,$00,$FE
elecn_proj_facing:  .byte   $02,$02,$02,$02,$01,$01,$01,$01

; ===========================================================================
; main_peterchy — Peterchy (walking snake, Snake Man stage)
; Walks horizontally with gravity, reverses at walls. Charges when player
; is close (< $10 px), backs off when > $30 px away.
; ===========================================================================
main_peterchy:
        ldy     #$08
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     peterchy_move_right     ; 0 = left
        ldy     #$1A                    ; speed index $1A
        jsr     move_right_collide                   ; move_right_collide
        jmp     peterchy_collision_check

peterchy_move_right:  ldy     #$1B      ; speed index $1B
        jsr     move_left_collide                   ; move_left_collide
peterchy_collision_check:  bcc     peterchy_reverse_dir  ; no wall -> check aggression
        lda     ent_facing,x            ; wall hit: reverse facing
        eor     #$03
        sta     ent_facing,x
peterchy_reverse_dir:  lda     ent_status,x  ; check entity state
        and     #$0F                    ; extract sub-state
        bne     peterchy_aggression_check  ; nonzero -> aggression check
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$10                    ; within 16 px?
        bcs     peterchy_return         ; no -> return
        inc     ent_status,x            ; advance to aggressive state
peterchy_return:  rts

peterchy_aggression_check:  jsr     entity_x_dist_to_player               ; entity_x_dist_to_player
        cmp     #$30                    ; within 48 px?
        bcc     peterchy_return         ; yes -> return (stay aggro)
        lda     ent_facing,x            ; reverse facing direction
        eor     #$03
        sta     ent_facing,x
        dec     ent_status,x            ; back to passive state
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

        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        bne     parasyu_state_activated               ; nonzero -> activated, skip init
        sta     ent_yvel_sub,x          ; clear Y speed sub
        lda     #$03                    ; Y speed = 3 (parachute fall)
        sta     ent_yvel,x              ; set Y speed
        jsr     entity_x_dist_to_player                   ; X distance to player
        cmp     #$64                    ; if > 100 px away,
        bcs     parasyu_init_done               ; too far -> keep falling

; --- player close: detach parachute ---
        lda     $E4                     ; pseudo-random: add shift register bytes
        adc     $E5                     ; to get random index 0-3
        sta     $E5
        and     #$03                    ; mask to 0-3
        tay
        lda     parasyu_initial_delay_table,y                 ; load random delay timer from table
        sta     ent_var2,x              ; store as delay timer
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

parasyu_phase_detached:  lda     ent_anim_state,x  ; check anim state
        cmp     #$02                    ; force frame 3 and reset counter
        bcc     parasyu_clamp_anim_frame               ; (lock to falling sprite pose)
        lda     #$03                    ; set anim frame = 3
        sta     ent_anim_state,x
        lda     #$00                    ; reset anim counter
        sta     ent_anim_frame,x
parasyu_clamp_anim_frame:  lda     ent_status,x  ; check entity state
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
        lda     bombflier_yspeed_lo,y   ; Y speed sub from table
        sta     ent_yvel_sub,x
        lda     bombflier_yspeed_table,y  ; Y speed whole from table
        sta     ent_yvel,x
        lda     bombflier_xspeed_lo,y   ; X speed sub from table
        sta     ent_xvel_sub,x
        lda     bombflier_xspeed_table,y  ; X speed whole from table
        sta     ent_xvel,x
        lda     ent_xvel,x              ; check if X speed negative
        bpl     parasyu_advance_swoop_step               ; positive -> skip negate

; --- negate X speed (make absolute value for directional move) ---
        lda     ent_xvel_sub,x          ; negate 16-bit X speed
        eor     #$FF                    ; X speed sub
        clc
        adc     #$01
        sta     ent_xvel_sub,x
        lda     ent_xvel,x              ; invert high byte
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x              ; store negated X speed
parasyu_advance_swoop_step:  inc     ent_var1,x             ; advance swoop step
        lda     ent_var1,x              ; get step count
        cmp     #$07                    ; after 7 steps: swoop complete
        bne     parasyu_load_speed_timer  ; not done -> set timer
        lda     ent_facing,x            ; reverse facing direction
        eor     #$03
        sta     ent_facing,x
        inc     ent_status,x            ; advance to fall phase
        lda     #$00                    ; reset swoop step for next cycle
        sta     ent_var1,x
parasyu_load_speed_timer:  lda     #$05                ; hold each speed step for 5 frames
        sta     ent_timer,x

; --- apply current speed to position ---
parasyu_apply_speed:  dec     ent_timer,x             ; decrement speed hold timer
        lda     ent_y_sub,x             ; Y sub-pixel position
        clc
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x              ; Y pixel position
        adc     ent_yvel,x
        sta     ent_y_px,x              ; store updated Y pixel
        lda     ent_facing,x            ; check facing direction
        and     #$02                    ; bit 1: 0=right, 1=left
        bne     parasyu_move_left
        jsr     move_sprite_right                   ; move_sprite_right
        bcs     parasyu_horizontal_done ; unconditional branch pair
        bcc     parasyu_horizontal_done
parasyu_move_left:  jsr     move_sprite_left               ; move_sprite_left
parasyu_horizontal_done:  rts

; --- gentle fall phase (between swoops) ---

parasyu_gentle_fall_phase:  dec     ent_var2,x             ; count down fall timer
        bne     parasyu_fall_timer_expired  ; not zero -> keep falling
        dec     ent_status,x            ; back to swoop phase
        lda     #$10                    ; reset fall timer = 16 frames
        sta     ent_var2,x
parasyu_fall_timer_expired:  lda     #$80                ; Y speed = $00.80 (0.5 px/frame)
        sta     ent_yvel_sub,x          ; set gentle Y speed sub
        lda     #$00
        sta     ent_yvel,x
        jmp     move_sprite_down                   ; apply downward movement

; parasyu data tables
; $B6D6: swoop speed sub-indices (7 steps per swoop, 2 swoops = 14 entries)

parasyu_swoop_index_table:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F
parasyu_initial_delay_table:  .byte   $22,$2A,$26,$2E

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
        sta     ent_anim_frame,x
        lda     ent_status,x            ; check entity state
        and     #$0F                    ; extract sub-state
        beq     doc_robot_intro_shutter_init
        lda     boss_hp_display
        cmp     #$9C
        bne     doc_robot_intro_morph_done
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        stx     L0000
        lda     ent_routine,x           ; get doc robot routine
        and     #$07                    ; mask to 0-7 for table index
        tay
        lda     doc_robot_master_main_indices,y  ; get master's AI routine
        sta     ent_routine,x           ; set as new entity routine
        lda     #$CA                    ; doc robot hitbox
        sta     ent_hitbox,x
        lda     #$1C                    ; 28 HP
        sta     ent_hp,x
        lda     doc_robot_intro_xvel_sub,y
        sta     ent_xvel_sub,x
        lda     doc_robot_intro_xvel,y
        sta     ent_xvel,x
        jsr     reset_gravity                   ; reset_gravity
        lda     doc_robot_intro_chr_lookup,y                 ; CHR bank set for this master
        sta     $ED                     ; store CHR bank to shadow register
        jsr     update_CHR_banks                   ; update_CHR_banks
        lda     #$C0                    ; set status: active + invincible
        sta     ent_status,x
        tya
        asl     a
        asl     a
        tay
        ldx     #$00                    ; X = palette write index
doc_robot_intro_palette_loop:  lda     doc_robot_intro_palette_data,y
        sta     $061C,x                 ; write to palette buffer sprite 0
        sta     $063C,x
        iny
        inx
        cpx     #$04
        bne     doc_robot_intro_palette_loop
        lda     #$FF                    ; mark palette dirty
        sta     palette_dirty
        ldx     L0000
doc_robot_intro_morph_done:  rts

; --- state 0: spawn shutter and set up doc robot ---
doc_robot_intro_shutter_init:  jsr     init_boss_wait          ; freeze player, start HP bar fill
        inc     ent_status,x
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        lda     #$61                    ; child routine $61 (shutter)
        sta     ent_routine,y
        lda     ent_y_px,x              ; parent Y -> shutter var1
        sta     ent_var1,y              ; child var1 = landing target Y
        lda     ent_x_px,x              ; copy parent X to shutter
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen to child
        sta     ent_x_scr,y
        lda     #$10                    ; child Y = $10 (top of screen)
        sta     ent_y_px,y
        lda     #$80                    ; child Y velocity sub = $80
        sta     ent_yvel_sub,y          ; set child Y velocity sub
        sta     ent_hitbox,y            ; shutter hitbox $80
        lda     #$00
        sta     ent_yvel,y
        stx     L0000                   ; save entity slot to temp
        lda     ent_routine,x           ; get doc robot routine
        and     #$07                    ; mask to low 3 bits (0-7)
        tax
        lda     doc_robot_intro_sprite_anim,x  ; get sprite anim from table
        ldx     L0000                   ; restore entity slot
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_routine,x           ; get doc robot routine index
        and     #$07                    ; mask to master index 0-7
        tay                             ; Y = master index
        lda     doc_robot_intro_chr_bank,y  ; load CHR bank for this master
        sta     $ED                     ; store CHR bank to shadow register
        tya                             ; master index * 2 = palette pair offset
        asl     a
        tay                             ; Y = palette pair index
        lda     #$0F                    ; palette value $0F (black)
        sta     $061C                   ; set palette buffer $061C
        sta     $061D                   ; set palette buffer $061D
        sta     $063C                   ; set palette buffer $063C
        sta     $063D                   ; set palette buffer $063D
        lda     doc_robot_intro_palette_color,y  ; load master-specific palette color
        sta     $061E                   ; set palette buffer $061E
        sta     $063E                   ; set palette buffer $063E
        lda     doc_robot_intro_palette_setup,y  ; load master-specific palette setup
        sta     $061F                   ; set palette buffer $061F
        sta     $063F                   ; set palette buffer $063F
        lda     #$FF                    ; mark palette dirty
        sta     palette_dirty
        jmp     update_CHR_banks                   ; update_CHR_banks

        jsr     move_sprite_down                   ; move_sprite_down
        lda     ent_y_px,x              ; check if shutter reached target Y
        cmp     ent_var1,x              ; compare to landing target (var1)
        beq     doc_robot_intro_shutter_landed  ; reached target Y -> land
        lda     #$80                    ; keep boss HP display active
        sta     boss_hp_display         ; store to boss_hp_display
        rts

doc_robot_intro_shutter_landed:  lda     #$00  ; shutter landed
        sta     ent_status,x            ; deactivate shutter entity
        rts

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
        lda     ent_status,x            ; check status sub-state
        and     #$0F                    ; isolate low nibble
        bne     robot_master_intro_falling  ; nonzero = already initialized
        inc     ent_status,x            ; advance sub-state to 1
        jsr     init_boss_wait                  ; freeze player, start HP bar fill
; --- falling / waiting for HP bar to fill ---
robot_master_intro_falling:  lda     ent_y_px,x          ; still above landing Y ($80)?
        cmp     #$80                    ; compare to floor Y = $80
        bcs     robot_master_intro_gravity  ; at or below floor -> use gravity
        jsr     apply_y_speed                   ; apply_y_speed (still falling)
        jmp     robot_master_intro_clear_frame

robot_master_intro_gravity:  lda     ent_routine,x       ; look up floor tile for this master
        and     #$07                    ; mask to 0-7
        tay
        lda     robot_master_intro_gravity_floor,y                 ; gravity floor offset per master
        tay
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     robot_master_intro_clear_frame  ; carry clear = still airborne
        lda     ent_routine,x           ; check if landing animation done
        and     #$07                    ; mask to 0-7
        tay
        lda     robot_master_intro_target_anim,y                 ; target anim_state for this master
        cmp     ent_anim_state,x        ; compare to current anim_state
        bne     robot_master_intro_landing_wait  ; not at target anim -> keep waiting
        lda     #$00                    ; reset animation frame
        sta     ent_anim_frame,x
        lda     boss_hp_display         ; check boss HP bar fill progress
        cmp     #$9C                    ; HP bar fully filled?
        bne     robot_master_intro_rts  ; not full yet -> wait
; --- morph into actual Robot Master AI ---
        lda     #$C0                    ; active + invincible
        sta     ent_status,x
        lda     #$1C                    ; 28 HP
        sta     ent_hp,x
        lda     ent_routine,x           ; get master index from routine
        and     #$07                    ; fetch robot master's
        tay                             ; main routine index
        lda     robot_master_main_indices,y ; morph this sprite into it
        sta     ent_routine,x
        lda     robot_master_intro_xvel_sub,y  ; load master-specific X vel sub
        sta     ent_xvel_sub,x          ; store initial X velocity sub
        lda     robot_master_intro_xvel,y  ; load master-specific X vel whole
        sta     ent_xvel,x              ; store initial X velocity whole
        lda     robot_master_intro_init_anim,y  ; load master-specific init anim
        jmp     reset_sprite_anim                   ; reset_sprite_anim

robot_master_intro_clear_frame:  lda     #$00  ; clear animation frame
        sta     ent_anim_frame,x        ; store to ent_anim_frame
robot_master_intro_landing_wait:  lda     #$80  ; keep boss HP display active
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
        lda     ent_y_px,x              ; save original Y position
        pha
        dec     ent_y_px,x              ; nudge Y up 1px for collision check
        jsr     check_player_collision                   ; check_player_collision
        pla                             ; restore original Y position
        sta     ent_y_px,x              ; write back original Y pixel
        bcs     spinning_wheel_done     ; carry set = no collision -> done
        lda     ent_anim_id,x           ; get anim/OAM ID
        and     #$01                    ; bit 0 = direction (0=left, 1=right)
        tay
        lda     ent_x_sub               ; player X sub-pixel
        clc
        adc     spinning_wheel_x_offsets,y  ; add X scroll offset from table
        sta     ent_x_sub               ; store updated X sub
        lda     ent_x_px                ; player X pixel
        adc     spinning_wheel_y_sign,y ; add X scroll sign (+0 or -1)
        sta     ent_x_px                ; store updated X pixel
        lda     ent_x_scr               ; player X screen
        adc     spinning_wheel_y_data,y ; add screen carry (+0 or -1)
        sta     ent_x_scr               ; store updated X screen
spinning_wheel_done:  rts

spinning_wheel_x_offsets:  .byte   $80,$80
spinning_wheel_y_sign:  .byte   $00,$FF
spinning_wheel_y_data:  .byte   $00,$FF

; ===========================================================================
; main_trap_platform — Trap Platform (Shadow Man stage)
; Triggers when player is close (< $15 Y, < $18 X). Plays open animation,
; then closes after a delay. Toggles sprite flag bit 0 on completion.
; ===========================================================================
main_trap_platform:
        lda     ent_status,x            ; check status sub-state
        and     #$0F                    ; isolate low nibble
        bne     trap_platform_var1_check  ; nonzero = already triggered
        sta     ent_anim_state,x        ; clear anim_state
        sta     ent_anim_frame,x        ; clear anim_frame
        sta     ent_var1,x              ; clear var1 (open/close phase)
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$15                    ; Y distance < $15?
        bcs     trap_platform_timer_return  ; too far away vertically
        lda     player_state            ; check if player is active
        bne     trap_platform_timer_return  ; not ground state -> skip
        lda     ent_y_px,x              ; get entity Y pixel
        cmp     ent_y_px                ; compare to player Y pixel
        bcc     trap_platform_timer_return  ; entity above player -> skip
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$18                    ; X distance < $18?
        bcs     trap_platform_timer_return  ; too far away horizontally
        lda     #$0C                    ; 12-frame open delay
        sta     ent_timer,x             ; set timer
        inc     ent_status,x            ; advance to triggered state
trap_platform_timer_return:  rts

trap_platform_var1_check:  lda     ent_var1,x  ; check open/close phase flag
        bne     trap_platform_status_check  ; nonzero = in close phase
        dec     ent_timer,x             ; decrement open delay timer
        bne     trap_platform_timer_return  ; timer not expired -> wait
        inc     ent_var1,x              ; advance to open phase
        lda     ent_flags,x             ; clear collision bits from flags
        and     #$90                    ; keep bits 7 and 4 only
        sta     ent_flags,x             ; store updated flags
trap_platform_status_check:  lda     ent_status,x  ; check status bit 1
        and     #$02                    ; bit 1 = closing phase
        bne     trap_platform_animation_set  ; set -> skip to close anim
        lda     ent_anim_state,x        ; check if open anim complete
        cmp     #$04                    ; anim_state == 4 = fully open
        bne     trap_platform_timer_return  ; not open yet -> wait
        inc     ent_status,x            ; advance status (set bit 1)
        lda     #$14                    ; 20-frame hold-open timer
        sta     ent_timer,x             ; set close delay timer
trap_platform_animation_set:  lda     #$04  ; set anim_state = 4 (close anim)
        sta     ent_anim_state,x        ; store anim_state
        lda     #$00                    ; reset anim frame
        sta     ent_anim_frame,x        ; store anim_frame = 0
        dec     ent_timer,x             ; decrement close timer
        bne     trap_platform_timer_return  ; timer not expired -> wait
        lda     ent_flags,x             ; toggle sprite flag bit 0
        eor     #$01                    ; flip low bit
        sta     ent_flags,x             ; store updated flags
        lda     #$80                    ; reset to idle active state
        sta     ent_status,x            ; store status = $80
        rts

; ===========================================================================
; main_breakable_wall — Breakable Wall (Hard Knuckle / Shadow Blade target)
; Checks weapon slots $01-$02 for Hard Knuckle ($AC) or Shadow Blade ($AF).
; If hit, plays explosion and changes AI routine to $19 (debris).
; ===========================================================================
main_breakable_wall:

        ldy     #$01                    ; start at weapon slot 1
breakable_wall_child_status:  lda     ent_status,y  ; check if weapon slot active
        bpl     breakable_wall_loop_next  ; bit 7 clear = inactive -> skip
        lda     ent_anim_id,y           ; get weapon's anim ID
        cmp     #$AC                    ; is it Hard Knuckle ($AC)?
        beq     breakable_wall_collision_check  ; yes -> check collision
        cmp     #$AF                    ; is it Shadow Blade ($AF)?
        bne     breakable_wall_loop_next  ; neither -> skip this slot
breakable_wall_collision_check:  jsr     check_sprite_weapon_collision               ; check_sprite_weapon_collision
        bcs     breakable_wall_loop_next  ; carry set = no collision -> skip
        lda     #SFX_ENEMY_HIT
        jsr     submit_sound_ID                   ; submit_sound_ID
        ldy     $10                     ; get weapon slot that collided
        lda     #$00                    ; despawn the weapon projectile
        sta     ent_status,y            ; clear weapon status = inactive
        lda     #$71                    ; break animation ($71)
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x             ; clear timer
        lda     #$19                    ; switch to debris AI routine $19
        sta     ent_routine,x           ; store new routine
        rts

breakable_wall_loop_next:  iny          ; next weapon slot
        cpy     #$03                    ; checked slots 1 and 2?
        bcc     breakable_wall_child_status  ; no -> check next slot
        rts

; ===========================================================================
; main_spark_falling_platform — Spark Man stage falling platform
; ===========================================================================
; Platform that falls when the player stands on it. State 0: checks if player
; is within range (Y < $15, X < $0A), saves initial Y to ent_var3 for
; return trip. State 1: falls upward (accelerates from speed $01.00 to max
; $03.00), stops when Y < $3A (offscreen top). State 2: returns to original
; Y position (ent_var3), then re-checks player proximity to cycle again.
main_spark_falling_platform:

        lda     ent_status,x            ; check status sub-state
        and     #$0F                    ; isolate low nibble
        bne     spark_platform_rising_state  ; nonzero = already activated
; --- state 0: idle, waiting for player proximity ---
        sta     ent_yvel_sub,x          ; init Y velocity sub = 0
        lda     #$01                    ; init Y velocity whole = 1
        sta     ent_yvel,x              ; store Y velocity = $01.00
        lda     ent_y_px,x              ; get current Y pixel
        sta     ent_var3,x              ; save home Y position
        jsr     entity_y_dist_to_player                   ; entity_y_dist_to_player
        cmp     #$15                    ; within $15 pixels Y?
        bcs     spark_platform_return
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$0A                    ; within $0A pixels X?
        bcs     spark_platform_return
        inc     ent_status,x            ; player close enough → start rising
; --- state 1: rising upward ---
spark_platform_rising_state:  lda     ent_timer,x  ; check if return timer is set
        bne     spark_platform_return_home               ; timer set → returning phase
        jsr     move_sprite_up                   ; move_sprite_up
        lda     ent_yvel_sub,x          ; accelerate: add $10 to Y vel sub
        clc
        adc     #$10                    ; add $10 to sub-pixel speed
        sta     ent_yvel_sub,x          ; store updated Y velocity sub
        lda     ent_yvel,x              ; propagate carry to whole byte
        adc     #$00
        sta     ent_yvel,x              ; store updated Y velocity whole
        cmp     #$03                    ; cap speed at $03.00
        bne     spark_platform_speed_cap  ; reached max speed? -> clamp
        lda     #$00                    ; clear sub-pixel at max
        sta     ent_yvel_sub,x          ; clamp Y vel sub = 0
spark_platform_speed_cap:  lda     ent_y_px,x  ; check Y pixel position
        cmp     #$3A                    ; reached top of screen?
        bcs     spark_platform_return   ; not at top yet -> continue rising
        inc     ent_timer,x             ; mark: time to return
        lda     #$00                    ; reset speed to $01.00 for descent
        sta     ent_yvel_sub,x          ; Y vel sub = 0
        lda     #$01                    ; Y vel whole = 1
        sta     ent_yvel,x              ; store descent speed
spark_platform_return:  rts

; --- state 1+: returning to home position ---
spark_platform_return_home:  lda     ent_status,x  ; check status bit 1
        and     #$02                    ; bit 1 = reaggression phase
        bne     spark_platform_reaggression               ; state 2+ → check player again
        jsr     top_man_platform_move_down_check               ; move downward (return to home)
        lda     ent_y_px,x              ; check if back at home Y
        cmp     ent_var3,x              ; reached home Y?
        bcc     spark_platform_return               ; not yet
        inc     ent_status,x            ; advance to state 2
spark_platform_reaggression:  jsr     entity_y_dist_to_player               ; entity_y_dist_to_player
        cmp     #$16                    ; within $16 pixels Y?
        bcs     spark_platform_return   ; too far -> stay idle
        jsr     entity_x_dist_to_player                   ; entity_x_dist_to_player
        cmp     #$09                    ; within $09 pixels X?
        bcs     spark_platform_return   ; too far -> stay idle
        dec     ent_status,x            ; decrement status (re-enter rising)
        dec     ent_timer,x             ; clear return timer
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
        lda     ent_status,x            ; check status sub-state
        and     #$0F                    ; isolate low nibble
        bne     big_snakey_fire_state   ; nonzero = firing phase
        inc     ent_status,x            ; advance to firing state
        lda     $E4                     ; RNG: $E4 += $E6
        adc     $E6
        sta     $E4                     ; store updated RNG value
        and     #$03                    ; 0-3 index
        tay
        lda     big_snakey_shot_count,y                 ; shot count: 2, 3, 4, or 2
        sta     ent_var1,x              ; store in var1 = shots remaining
        lda     #$78                    ; 120-frame delay before firing
        sta     ent_timer,x             ; set pre-fire delay timer
        bne     big_snakey_collision    ; always taken (A nonzero)
; --- state 1: firing phase ---
big_snakey_fire_state:  lda     ent_timer,x  ; check pre-fire timer
        bne     big_snakey_pre_fire              ; timer not expired → wait
        lda     ent_anim_state,x       ; open mouth (anim |= $01)
        ora     #$01                    ; set bit 0 = mouth open
        sta     ent_anim_state,x        ; store updated anim_state
        lda     ent_var2,x             ; inter-shot delay active?
        bne     big_snakey_inter_shot              ; yes → count down
; --- spawn homing bullet child ---
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        lda     #$BA                    ; child anim ID
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_x_px,x             ; copy position to child
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y
        lda     #$80                    ; child hitbox = $80 (contact dmg)
        sta     ent_hitbox,y            ; child hitbox
        lda     #$8F                    ; child AI routine = $8F (homing)
        sta     ent_routine,y           ; child AI routine
        lda     #$00                    ; child X velocity sub = 0
        sta     ent_xvel_sub,y
        sta     $02                     ; target speed (sub) for homing calc
        lda     #$04                    ; child X velocity whole = 4
        sta     ent_xvel,y
        sta     $03                     ; target speed (whole) for homing calc
        sty     $0F                     ; save child slot index
        stx     $0E                     ; save parent slot index
        ldx     $0F                     ; X = child (for homing calc)
        jsr     calc_homing_velocity                   ; calc_homing_velocity
        ldy     $0F                     ; restore child slot index
        ldx     $0E                     ; restore parent slot index
        lda     $0C                     ; homing result → child facing
        sta     ent_facing,y
        dec     ent_var1,x              ; shots remaining--
        beq     big_snakey_shots_done              ; all shots fired → close mouth
        lda     #$12                    ; 18-frame delay between shots
        sta     ent_var2,x              ; store to var2
        bne     big_snakey_collision    ; always taken -> collision check
big_snakey_shots_done:  lda     #$00               ; close mouth (clear anim bit)
        sta     ent_anim_state,x
        dec     ent_status,x            ; return to state 0
        bne     big_snakey_collision    ; always taken -> collision check
big_snakey_inter_shot:  dec     ent_var2,x          ; inter-shot cooldown
        jmp     big_snakey_collision

big_snakey_pre_fire:  dec     ent_timer,x         ; pre-fire delay countdown
; --- common: run bank $1C collision + check death ---
big_snakey_collision:  lda     ent_anim_id,x       ; preserve anim_id across $8003 call
        pha
        jsr     process_sprites_top_spin_check               ; bank $1C collision handler
        pla
        sta     ent_anim_id,x           ; restore anim_id after call
        lda     ent_hp,x                ; check if HP reached zero
        bne     big_snakey_post_collision              ; still alive → skip
; --- death: despawn all child projectiles ---
        sta     ent_status,x            ; despawn self
        ldy     #$0F                    ; scan 16 enemy slots ($10-$1F)
big_snakey_despawn_loop:  lda     $0310,y             ; check enemy slot $10+Y
        bpl     big_snakey_loop_check              ; not active → skip
        lda     $03D0,y                 ; child Y position
        cmp     #$80                    ; below midscreen?
        bcs     big_snakey_loop_check              ; yes → don't despawn (offscreen)
        lda     #$00                    ; despawn child entity
        sta     $0310,y                 ; despawn child
big_snakey_loop_check:  dey
        bpl     big_snakey_despawn_loop
        lda     #$80                    ; set boss defeated flag
        sta     $55                     ; boss defeated flag / screen shake
big_snakey_post_collision:  lda     #$00  ; clear anim frame
        sta     ent_anim_frame,x
        rts

        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = horizontal dir
        beq     big_snakey_move_left    ; bit 0 clear -> move left
        jsr     move_sprite_right                   ; move_sprite_right
        jmp     big_snakey_vert_check

big_snakey_move_left:  jsr     move_sprite_left               ; move_sprite_left
big_snakey_vert_check:  lda     ent_facing,x  ; check vertical direction
        and     #$08                    ; bit 3 = vertical dir
        beq     big_snakey_move_down    ; bit 3 clear -> move down
        jmp     move_sprite_up                   ; move_sprite_up

big_snakey_move_down:  jmp     move_sprite_down               ; move_sprite_down

big_snakey_shot_count:  .byte   $03,$03,$04,$02         ; shot count table (3, 3, 4, 2)

; --- init_tama — shared Tama initialization / floor clamp ---
init_tama:
        lda     ent_anim_id,x           ; check if anim_id is zero
        beq     tama_init_return        ; zero = disabled -> return
        jsr     apply_y_speed                   ; apply_y_speed
        ldy     #$00
        sty     $54                     ; clear $54 flag
        lda     ent_x_scr,x             ; get entity X screen
        cmp     #$0B                    ; screen $0B?
        beq     tama_init_load_floor    ; yes -> use floor[0] = $48
        iny                             ; no -> use floor[1] = $78
tama_init_load_floor:  lda     tama_init_floor_table,y  ; load floor Y from table
        cmp     ent_y_px,x              ; compare to entity Y pixel
        bcs     tama_init_clamp_floor   ; below floor -> clamp
        sta     ent_y_px,x              ; set Y to floor level
        lda     ent_anim_frame,x        ; check anim frame
        cmp     #$02                    ; frame == 2?
        bne     tama_init_return        ; not frame 2 -> return
        lda     ent_anim_state,x        ; check anim state
        cmp     #$02                    ; anim_state == 2?
        bne     tama_init_return        ; not state 2 -> return
        lda     #$20                    ; palette color $20 (white)
        sta     $060D
        sta     $062D
        lda     #$37                    ; palette color $37 (orange)
        sta     $060E
        sta     $062E
        lda     #$17                    ; palette color $17 (dark orange)
        sta     $060F
        sta     $062F
        sta     palette_dirty           ; mark palette dirty
        lda     #$00
        sta     ent_anim_id,x           ; disable entity (anim_id = 0)
        ldy     #$1F                    ; scan enemy slots $10-$1F
tama_init_loop_entities:  lda     ent_status,y  ; check slot status
        bpl     tama_init_entity_continue  ; bit 7 clear -> skip
        lda     ent_flags,y             ; clear invincibility flag (bit 2)
        and     #$FB                    ; mask off bit 2
        sta     ent_flags,y             ; store updated flags
tama_init_entity_continue:  dey         ; next slot
        cpy     #$0F                    ; scanned all enemy slots?
        bne     tama_init_loop_entities
        rts

tama_init_clamp_floor:  lda     #$00    ; clamp Y velocity to 0
        sta     ent_anim_frame,x        ; reset anim frame
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
        lda     ent_flags,x             ; check entity flags
        and     #$04                    ; bit 2 = disabled flag
        bne     tama_init_return        ; set -> entity disabled, return
        lda     ent_anim_id,x           ; save anim_id before collision
        pha
        jsr     process_sprites_top_spin_check  ; bank $1C collision handler
        pla
        sta     ent_anim_id,x           ; restore anim_id after call
        lda     ent_hp,x                ; check HP
        bne     tama_a_scan_proj_state  ; HP > 0 -> still alive
        sta     ent_var1,x              ; clear var1 (HP is 0 = A)
        sta     ent_hitbox,x            ; clear hitbox
        lda     #$04                    ; set var2 = 4 (death counter)
        sta     ent_var2,x
        lda     #$63                    ; switch to item-drop routine $63
        sta     ent_routine,x           ; store new AI routine
        ldy     #$0F                    ; scan 16 enemy slots
        lda     #$00                    ; A = 0 for clearing
tama_a_clear_oam_loop:  sta     $0310,y ; clear enemy slot status
        dey
        bpl     tama_a_clear_oam_loop
        lda     #$80                    ; set status = $80 (active)
        sta     ent_status,x            ; store to entity status
        rts

tama_a_scan_proj_state:  lda     ent_status,x  ; check status sub-state
        and     #$0F                    ; isolate low nibble
        bne     tama_a_check_anim_fire  ; nonzero = firing phase
        sta     ent_anim_frame,x        ; clear anim frame
        ldy     #$1F                    ; scan slots $10-$1F
tama_a_scan_slots_loop:  lda     ent_status,y  ; check slot status
        bpl     tama_a_loop_next_slot   ; inactive -> check next
        lda     ent_anim_id,y           ; get slot's anim ID
        cmp     #$CF                    ; is it kitten projectile $CF?
        beq     tama_a_scan_return      ; yes -> projectiles still active
        cmp     #$D0                    ; is it kitten projectile $D0?
        beq     tama_a_scan_return      ; yes -> projectiles still active
tama_a_loop_next_slot:  dey
        cpy     #$0F                    ; checked all enemy slots?
        bne     tama_a_scan_slots_loop
        lda     $54                     ; check $54 flag
        bne     tama_a_scan_return      ; nonzero = partner still firing
        inc     ent_status,x            ; advance to firing state
tama_a_scan_return:  rts

tama_a_check_anim_fire:  lda     ent_anim_frame,x  ; check anim frame
        ora     ent_anim_state,x        ; OR with anim_state
        bne     tama_a_check_fire_cooldown  ; both zero = volley complete
        inc     ent_var1,x              ; increment volley count
        lda     ent_var1,x              ; get updated volley count
        cmp     #$02                    ; fired 2 volleys?
        bne     tama_a_set_fire_cooldown  ; no -> set cooldown for next
        dec     ent_status,x            ; back to state 0 (wait)
        lda     #$00                    ; clear timer
        sta     ent_timer,x             ; reset timer
        sta     ent_var1,x              ; clear volley counter
        inc     $54                     ; set $54 flag for partner
        rts

tama_a_set_fire_cooldown:  lda     #$3C ; 60-frame cooldown between volleys
        sta     ent_timer,x             ; store timer
        rts

tama_a_check_fire_cooldown:  lda     ent_timer,x  ; check cooldown timer
        beq     tama_a_spawn_proj_check ; zero = ready to fire
        dec     ent_timer,x             ; decrement cooldown
        lda     #$00                    ; clear anim frame during wait
        sta     ent_anim_frame,x        ; store anim_frame = 0
        rts

tama_a_spawn_proj_check:  lda     ent_anim_frame,x  ; check anim frame
        bne     tama_a_spawn_return     ; nonzero -> not ready
        lda     ent_anim_state,x        ; check anim state
        cmp     #$02                    ; anim_state == 2 = fire pose?
        bne     tama_a_spawn_return     ; no -> return
        jsr     find_enemy_freeslot_y                   ; find_enemy_freeslot_y
        bcs     tama_a_spawn_return     ; no free slot -> return
        lda     #$CF                    ; kitten projectile anim $CF
        jsr     init_child_entity                   ; init_child_entity
        lda     ent_x_px,x              ; copy parent X to child
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y
        lda     #$C0                    ; child hitbox = $C0
        sta     ent_hitbox,y
        lda     #$08                    ; child HP = 8
        sta     ent_hp,y
        lda     #$8D                    ; child AI routine = $8D
        sta     ent_routine,y
        lda     #$00                    ; child X velocity sub = 0
        sta     ent_xvel_sub,y
        lda     #$01                    ; child X velocity whole = 1
        sta     ent_xvel,y
        lda     #$00                    ; child Y velocity sub = 0
        sta     ent_yvel_sub,y
        lda     #$04                    ; child Y velocity whole = 4
        sta     ent_yvel,y
        jsr     face_player                   ; face_player
        lda     ent_facing,x            ; copy parent facing to child
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

        lda     ent_flags,x             ; check entity flags
        and     #$04                    ; bit 2 = disabled flag
        bne     tama_a_dead_rts         ; set -> entity disabled, return
        lda     ent_status,x            ; check status sub-state
        and     #$0F                    ; isolate low nibble
        bne     tama_b_state_active     ; nonzero = active/firing
        sta     ent_anim_frame,x        ; clear anim frame
        ldy     #$1F                    ; scan slots $10-$1F
tama_b_scan_active_projectiles:  lda     ent_status,y  ; check slot status
        bpl     tama_b_next_slot        ; inactive -> check next
        lda     ent_anim_id,y           ; get slot's anim ID
        cmp     #$CF                    ; is it projectile $CF?
        beq     tama_b_state_idle_done  ; yes -> still active
        cmp     #$D0                    ; is it projectile $D0?
        beq     tama_b_state_idle_done  ; yes -> still active
tama_b_next_slot:  dey
        cpy     #$0F                    ; checked all enemy slots?
        bne     tama_b_scan_active_projectiles
        lda     $54                     ; check $54 flag from partner
        beq     tama_b_state_idle_done  ; zero = partner not ready
        inc     ent_status,x            ; advance to active state
tama_b_state_idle_done:  rts

tama_b_state_active:  lda     ent_anim_frame,x  ; check anim frame
        ora     ent_anim_state,x        ; OR with anim state
        bne     tama_b_check_anim_frame ; nonzero = anim still playing
        dec     ent_status,x            ; back to idle state
tama_b_check_anim_frame:  lda     ent_anim_state,x  ; check anim state
        cmp     #$02                    ; anim_state == 2 = fire pose?
        bne     tama_b_state_idle_done  ; no -> return
        lda     ent_anim_frame,x        ; check anim frame
        bne     tama_b_state_idle_done  ; nonzero -> return
        lda     #$02                    ; launch counter = 2 (3 projectiles)
        sta     $10                     ; store to temp $10
        jsr     face_player                   ; face_player
tama_b_launch_projectile_loop:  jsr     find_enemy_freeslot_y               ; find_enemy_freeslot_y
        bcs     tama_b_launch_done      ; no free slot -> done launching
        lda     #$D0                    ; bouncing kitten anim $D0
        jsr     init_child_entity                   ; init_child_entity
        lda     #$01                    ; child anim_state = 1
        sta     ent_anim_state,y
        sta     ent_hp,y                ; child HP = 1
        lda     #$CB                    ; child hitbox = $CB
        sta     ent_hitbox,y
        lda     #$8E                    ; child AI routine = $8E
        sta     ent_routine,y
        lda     ent_x_px,x              ; copy parent X to child
        sta     ent_x_px,y
        lda     ent_x_scr,x             ; copy parent X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x              ; copy parent Y to child
        sta     ent_y_px,y
        lda     ent_facing,x            ; copy parent facing to child
        sta     ent_facing,y
        stx     L0000                   ; save parent slot to temp
        ldx     $10                     ; X = launch counter index
        lda     tama_b_proj_yspeed_sub,x  ; load Y speed sub from table
        sta     ent_yvel_sub,y          ; set child Y velocity sub
        lda     tama_b_proj_yspeed,x    ; load Y speed whole from table
        sta     ent_yvel,y              ; set child Y velocity whole
        lda     tama_b_proj_xspeed_sub,x  ; load X speed sub from table
        sta     ent_xvel_sub,y          ; set child X velocity sub
        lda     tama_b_proj_xspeed,x    ; load X speed whole from table
        sta     ent_xvel,y              ; set child X velocity whole
        lda     #$1E                    ; 30-frame timer
        sta     ent_timer,y             ; set child timer
        sta     ent_var1,y              ; set child var1 = 30
        ldx     L0000                   ; restore parent slot
        dec     $10                     ; decrement launch counter
        bpl     tama_b_launch_projectile_loop  ; more to launch -> loop
        lda     #$00                    ; clear $54 flag (all shots fired)
        sta     $54
tama_b_launch_done:  rts

tama_b_proj_yspeed_sub:  .byte   $44,$00,$2A
tama_b_proj_yspeed:  .byte   $03,$04,$05
tama_b_proj_xspeed_sub:  .byte   $39,$55,$8C
tama_b_proj_xspeed:  .byte   $01,$01,$01
        ldy     #$08                    ; gravity offset = 8
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        bcc     tama_b_gravity_applied  ; carry clear = still airborne
        lda     #$44                    ; bounce Y velocity sub = $44
        sta     ent_yvel_sub,x
        lda     #$03                    ; bounce Y velocity whole = 3
        sta     ent_yvel,x
tama_b_gravity_applied:  lda     ent_facing,x  ; check facing direction
        and     #$01                    ; bit 0 = horizontal dir
        beq     tama_b_move_left        ; bit 0 clear -> move left
        ldy     #$08                    ; collision offset = 8
        jsr     move_right_collide                   ; move_right_collide
        jmp     tama_b_wall_bounce_check

tama_b_move_left:  ldy     #$09         ; collision offset = 9
        jsr     move_left_collide                   ; move_left_collide
tama_b_wall_bounce_check:  bcc     tama_b_projectile_done  ; carry set = hit wall
        lda     ent_facing,x            ; reverse horizontal direction
        eor     #$03                    ; toggle facing bits 0 and 1
        sta     ent_facing,x            ; store reversed facing
tama_b_projectile_done:  rts

        lda     ent_timer,x             ; check initial flight timer
        beq     tama_b_projectile_fall_phase  ; zero = switch to fall phase
        dec     ent_timer,x             ; decrement flight timer
        jsr     apply_y_speed                   ; apply_y_speed
        lda     ent_facing,x            ; check facing direction
        and     #$01                    ; bit 0 = horizontal dir
        beq     tama_b_projectile_move_left  ; bit 0 clear -> move left
        jmp     move_sprite_right                   ; move_sprite_right

tama_b_projectile_move_left:  jmp     move_sprite_left               ; move_sprite_left

tama_b_projectile_fall_phase:  ldy     #$12  ; gravity offset = $12
        jsr     move_vertical_gravity                   ; move_vertical_gravity
        lda     #$01                    ; set anim_state = 1 (falling)
        sta     ent_anim_state,x        ; store falling anim state
        bcc     tama_b_fall_loop        ; carry clear = no floor hit
        lda     #$00                    ; floor hit: anim_state = 0
        sta     ent_anim_state,x        ; store grounded anim state
        dec     ent_var1,x              ; decrement bounce counter
        bne     tama_b_clear_anim_frame ; more bounces left
        lda     #$3C                    ; 60-frame final bounce timer
        sta     ent_var1,x              ; store to var1
        lda     #$A8                    ; large bounce Y vel sub = $A8
        sta     ent_yvel_sub,x
        lda     #$05                    ; large bounce Y vel whole = 5
        sta     ent_yvel,x
        lda     #$F1                    ; X velocity sub = $F1
        sta     ent_xvel_sub,x
        lda     #$00                    ; X velocity whole = 0 (slow)
        sta     ent_xvel,x
        jsr     face_player                   ; face_player
tama_b_fall_loop:  lda     ent_facing,x ; check facing direction
        and     #$01                    ; bit 0 = horizontal dir
        beq     tama_b_move_left_collision  ; bit 0 clear -> move left
tama_b_move_right_collision_jmp:  ldy     #$1E  ; collision offset = $1E
        jsr     move_right_collide                   ; move_right_collide
        .byte   $4C
tama_b_move_left_collision_jmp:  .byte   $F3
        .byte   $BD
tama_b_move_left_collision:  ldy     #$1F  ; collision offset = $1F
        jsr     move_left_collide                   ; move_left_collide
tama_b_clear_anim_frame:  lda     #$00  ; clear anim frame
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
        bne     item_pickup_apply_gravity  ; always taken (Y nonzero)
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
        sta     L0000                   ; store bit mask in temp
        pla                             ; high 5 bits = byte index
        lsr     a                       ; shift right 3 = byte index
        lsr     a
        lsr     a
        tay                             ; Y = byte index
        lda     $0150,y                 ; set collected bit
        ora     L0000                   ; set collected bit
        sta     $0150,y                 ; store back to respawn table
item_pickup_despawn:  lda     #$00                ; despawn pickup entity
        sta     ent_status,x            ; deactivate pickup entity
        ldy     ent_routine,x                 ; dispatch to item type handler
        lda     tama_b_move_right_collision_jmp,y                 ; via pointer table indexed by
        sta     L0000                   ; AI routine (ent_routine)
        lda     tama_b_move_left_collision_jmp,y                 ; loads pickup-effect routine address
        sta     $01                     ; store to indirect pointer high
        jmp     (L0000)                 ; indirect jump to effect handler

; --- no collision: handle despawn timer ---

item_pickup_despawn_timer:  lda     ent_timer,x             ; despawn timer active?
        beq     item_pickup_timer_return               ; no → return
        dec     ent_timer,x                 ; decrement; expired?
        bne     item_pickup_timer_return               ; no → return
        lda     #$00                    ; timer expired: despawn uncollected item
        sta     ent_status,x            ; deactivate entity
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
        adc     #$01                    ; add 1 energy tick
        sta     player_hp,y             ; store updated energy
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
        sta     L0000                   ; store bit mask in temp
        pla
        lsr     a                       ; byte index
        lsr     a
        lsr     a
        tay
        lda     $0150,y                 ; load respawn table byte
        ora     L0000                   ; set collected bit
        sta     $0150,y                 ; store back to respawn table
        ldy     $10                     ; despawn the weapon that hit
        lda     #$00                    ; despawn the weapon
        sta     ent_status,y            ; clear weapon status
        lda     #$71                    ; play break animation
        jmp     reset_sprite_anim                   ; reset_sprite_anim

; --- break animation done: spawn random item ---
surprise_box_item_spawn:  lda     ent_anim_state,x  ; check break anim state
        cmp     #$04                    ; break anim finished?
        bne     surprise_box_return               ; no → wait
        lda     $E5                     ; RNG: $E5 += $E6
        adc     $E6
        sta     $E5                     ; store updated RNG value
        sta     L0000                   ; store to temp
        lda     #$64                    ; divide by 100
        sta     $01
        jsr     divide_8bit                   ; divide_8bit (remainder in $03)
        ldy     #$05                    ; scan probability thresholds
        lda     $03                     ; load remainder from division
surprise_box_probability_scan:  cmp     surprise_box_prob_thresholds,y             ; weighted probability table
        bcc     surprise_box_item_selected  ; below threshold -> item selected
        dey
        bne     surprise_box_probability_scan  ; try next lower threshold
surprise_box_item_selected:  lda     surprise_box_item_anim_ids,y             ; item anim ID for selected item
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     surprise_box_item_routine_ids,y                 ; item AI routine
        sta     ent_routine,x           ; AI routine ($0320)
        lda     ent_flags,x             ; clear low 2 flag bits
        and     #$FC                    ; mask off bits 0-1
        sta     ent_flags,x             ; store updated flags
        lda     #$F0                    ; 240-frame despawn timer
        sta     ent_timer,x             ; set entity timer
surprise_box_return:  .byte   $60
; surprise box data tables
surprise_box_prob_thresholds:  .byte   $63,$41,$23,$19,$0F,$05 ; probability thresholds (99,65,35,25,15,5)
surprise_box_item_anim_ids:  .byte   $FB,$F9,$FA,$FC,$FE,$FD ; item anim IDs
surprise_box_item_routine_ids:  .byte   $66,$64,$65             ; item AI routine IDs
        .byte   $67
        adc     #$68
        lda     ent_anim_state,x        ; check break anim state
        cmp     #$04                    ; anim_state == 4 = break done?
        bne     surprise_box_alt_return ; not done -> return
        lda     boss_active             ; check boss active flag
        bmi     surprise_box_boss_deactivate  ; bit 7 set -> boss active, despawn
        lda     $E6                     ; RNG: add $E7 to $E6
        adc     $E7
        sta     $E7                     ; store updated RNG value
        sta     L0000                   ; store to temp
        lda     #$64                    ; divisor = 100
        sta     $01                     ; store divisor
        jsr     divide_8bit                   ; divide_8bit
        ldy     #$04                    ; scan 5 probability thresholds
        lda     $03                     ; load remainder from division
surprise_box_alt_prob_scan:  cmp     surprise_box_alt_prob_thresholds,y  ; compare to threshold
        bcc     surprise_box_alt_item_selected  ; below threshold -> item selected
        dey
        bpl     surprise_box_alt_prob_scan
surprise_box_boss_deactivate:  lda     #$00  ; despawn entity (no item)
        sta     ent_status,x            ; clear entity status
        rts

surprise_box_alt_item_selected:  lda     surprise_box_alt_anim_ids,y  ; load selected item's anim ID
        jsr     reset_sprite_anim                   ; reset_sprite_anim
        lda     surprise_box_alt_routine_ids,y  ; load selected item's AI routine
        sta     ent_routine,x           ; store to entity routine
        lda     #$F0                    ; 240-frame despawn timer
        sta     ent_timer,x             ; set entity timer
        lda     #$00                    ; clear Y velocity sub
        sta     ent_yvel_sub,x          ; store Y velocity sub = 0
        sta     ent_yvel,x              ; store Y velocity whole = 0
surprise_box_alt_return:  .byte   $60
surprise_box_alt_prob_thresholds:  .byte   $1D,$1B,$0C,$0A,$01
surprise_box_alt_anim_ids:  .byte   $FB,$FC,$F9,$FA,$FE
surprise_box_alt_routine_ids:  .byte   $66,$67,$64,$65,$69

; freespace (unused bytes, fills remainder of bank $1D to $BFFF)
        .byte   $ED,$40,$40
        .byte   $01,$C6,$15,$ED,$00,$A6,$41,$97
        .byte   $11,$59,$54,$93,$44,$CD,$84,$66
        .byte   $04,$08,$41,$75,$51,$9B,$15,$0B
        .byte   $01,$88,$40,$C0,$01,$18,$00,$57
        .byte   $80,$88,$40,$02,$00,$02,$10,$0E
        .byte   $01,$C7,$10,$30,$00,$EF,$00,$0F
        .byte   $50,$AB,$15,$C0,$05,$07,$55,$6F
        .byte   $10,$97,$44,$08,$40,$2B,$01,$23
        .byte   $00,$92,$71,$ED,$15,$67,$54,$7A
        .byte   $54,$A1,$00,$AD,$01,$3A,$00,$DE ; → next tick
        .byte   $04,$82,$51,$90
        brk
        asl     $05,x
