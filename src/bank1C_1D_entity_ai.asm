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
LF580           := $F580
LF5C4           := $F5C4
LF606           := $F606
LF642           := $F642
LF67C           := $F67C
LF71D           := $F71D
LF73B           := $F73B
LF759           := $F759
LF779           := $F779
LF797           := $F797
LF7A8           := $F7A8
LF7C8           := $F7C8
LF81B           := $F81B
LF835           := $F835
LF846           := $F846
LF869           := $F869
LF883           := $F883
LF898           := $F898
LF89A           := $F89A
LF8B3           := $F8B3
LF8C2           := $F8C2
LF8D9           := $F8D9
LF954           := $F954
LFAE2           := $FAE2
LFAF6           := $FAF6
LFB7B           := $FB7B
LFC53           := $FC53
LFC63           := $FC63
LFCEB           := $FCEB
LFD6E           := $FD6E
LFF3C           := $FF3C
LFF6B           := $FF6B

.segment "BANK1C"

        jmp     process_sprites

code_8003:  jmp     L8109

        jmp     L82B8

        jmp     check_player_hit

process_sprites:  lda     #$55          ; $99 = $00.55 (0.332 px/frame²)
        sta     gravity                     ; set each frame for gameplay physics
        ldx     #$01                    ; start at weapons
        stx     sprite_slot                     ; (skip mega man)
L8014:  ldy     #$01
        cpx     spark_freeze_a
        beq     L8060                   ; if this sprite slot
        iny                             ; is spark frozen,
        cpx     spark_freeze_b                     ; skip regular processing
        beq     L8060
        lda     ent_status,x                 ; if sprite inactive,
        bpl     L808B                   ; continue loop
        ldy     #$1D                    ; select $A000~$BFFF bank
        lda     ent_routine,x                 ; bank $1D for main routine
        cmp     #$E0                    ; indices $00~$9F
        bcc     L8031
        ldy     #$12                    ; bank $12 for $E0~$FF
        bne     L803D
L8031:  lsr     a
        lsr     a                       ; in between $9F and $E0,
        lsr     a                       ; index >> 4 - $06
        lsr     a                       ; meaning bank $04 for $A0~$AF,
        cmp     #$0A                    ; $05 for $B0~$BF,
        bcc     L803D                   ; $06 for $C0~$CF,
        sec                             ; $07 for $D0~$DF
        sbc     #$06
        tay
L803D:  cpy     prg_bank
        beq     L804A                   ; if not already selected,
        sty     prg_bank
        txa                             ; preserve X
        pha                             ; select new $A000~$BFFF bank
        jsr     LFF6B                   ; restore X
        pla
        tax
L804A:  ldy     ent_routine,x
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

L8060:  lda     #$00                    ; clear spark freeze slot
        sta     boss_active,y                   ; recheck weapon collision
        jsr     LFB7B                   ; if none, spark cleared
        bcs     L8083
        txa                             ; if so, reapply
        ldy     $10                     ; spark freeze slot
        sta     boss_active,y
        lda     #$00                    ; constantly reset
        sta     ent_anim_frame,x                 ; animation counter
        beq     L8083                   ; to keep sprite from animating
        cpx     #$10
        bcc     L808B                   ; check being hit by weapon
        lda     ent_routine,x                 ; only for enemies with
        beq     L808B                   ; nonzero main indices
        jsr     check_weapon_hit
L8083:  lda     ent_hitbox,x                 ; if this sprite can
        bpl     L808B                   ; cause player damage,
        jsr     check_player_hit        ; check for that
L808B:  inc     sprite_slot
        ldx     sprite_slot                     ; go to next sprite
        cpx     #$20                    ; in X as well as $EF
        beq     L8096                   ; stop at $20
        jmp     L8014

L8096:  rts

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
        beq     L8096
        stx     $0F                     ; save entity slot
        lda     prg_bank                     ; save current bank
        pha
        lda     #$0A                    ; switch to bank $0A
        sta     prg_bank                     ; (damage tables at $A000)
        jsr     LFF6B
        ldx     $0F                     ; restore entity slot
        lda     invincibility_timer                     ; i-frames timer
        bne     code_80F9               ; skip if invincible
        lda     player_state                     ; check player state
        cmp     #PSTATE_DAMAGE                    ; already taking damage?
        beq     code_80F9               ; skip
        cmp     #PSTATE_DEATH                    ; already dead?
        beq     code_80F9               ; skip
        cmp     #PSTATE_VICTORY                    ; victory cutscene?
        beq     code_80F9               ; skip
        jsr     LFAE2                   ; AABB overlap test
        bcs     code_80F9               ; no collision → skip
        lda     #PSTATE_DAMAGE                    ; --- CONTACT HIT ---
        sta     player_state                     ; state → $06 (damage)
        lda     #$16                    ; SFX $16 = damage sound
        jsr     LF89A                   ; submit_sound_ID
        lda     player_hp                     ; player HP
        and     #$1F                    ; isolate HP value (0-28)
        beq     code_80F9               ; already 0 → skip damage calc
        ldy     ent_routine,x                 ; entity routine index
        lda     player_hp                     ; current HP
        and     #$1F
        sec
        sbc     LA000,y                 ; subtract damage from table
        php                             ; save carry (underflow = dead)
        ora     #$80                    ; set bit 7 (HP dirty flag)
        sta     player_hp
        plp
        beq     L80E7                   ; HP == 0 → dead
        bcs     code_80F9               ; HP > 0 → survived, done
L80E7:  lda     #$80                    ; --- PLAYER KILLED ---
        sta     player_hp                     ; HP = 0 with dirty flag
        lda     #PSTATE_DEATH                    ; state → $0E (death)
        sta     player_state
        lda     #$F2                    ; SFX $F2 = stop music
        jsr     LF89A                   ; submit_sound_ID
        lda     #$17                    ; SFX $17 = death sound
        jsr     LF89A                   ; submit_sound_ID
code_80F9:  pla                         ; restore bank
        sta     prg_bank
        jsr     LFF6B
        ldx     $0F                     ; restore entity slot
        rts

; checks for and applies effects/damage
; from a sprite being hit by a weapon
; parameters:
; X: sprite slot

check_weapon_hit:  lda     ent_hitbox,x      ; if vulnerable & shot tink
        and     #$60                    ; flags BOTH off,
        beq     L8142                   ; return
L8109:  lda     ent_anim_id
        cmp     #$A3                    ; if Mega Man OAM ID == $A3
        bne     L8113                   ; he is top spinning
        jmp     L825E

L8113:  jsr     LFB7B                   ; if no weapon collision
        bcs     L8142                   ; return
        lda     ent_hitbox,x
        and     #$20                    ; if shot tink flag on,
        beq     L8144                   ; bounce diagonally up
L811F:  lda     #$19                    ; play tink sound
        jsr     LF89A                   ; submit_sound_ID
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
L8142:  sec                             ; return carry on
        rts

L8144:  lda     #$18                    ; play damage sound
        jsr     LF89A                   ; submit_sound_ID
        lda     prg_bank
        pha                             ; preserve and select
        stx     $0F                     ; $0A as $A000~$BFFF bank
        lda     #$0A
        sta     prg_bank
        jsr     LFF6B
        ldx     $0F                     ; restore X (sprite slot)
        ldy     current_weapon
        lda     weapon_damage_ptr_lo,y  ; grab damage table pointer for
        sta     L0000                   ; currently equipped weapon
        lda     weapon_damage_ptr_hi,y
        sta     $01
        ldy     ent_routine,x                 ; if damage for main routine index
        lda     (L0000),y               ; is nonzero, do stuff
        bne     L8170
        jsr     L811F                   ; else tink shot
        jmp     L824D

L8170:  lda     current_weapon                     ; if weapon is
        cmp     #WPN_SPARK                    ; anything but spark
        bne     L81B6                   ; apply normal damage
        lda     (L0000),y               ; if damage value is zero,
        beq     L81B3                   ; don't do anything
        cmp     #$58                    ; if damage isn't magic number $58,
        bne     L81B6                   ; apply normal damage

; apply spark freeze effect
        txa                             ; Y = spark slot
        ldy     $10                     ; set shot sprite slot
        sta     boss_active,y                   ; for this weapon slot
        lda     ent_status,y
        ora     #$01                    ; turn on freeze state for
        sta     ent_status,y                 ; spark shot
        lda     #$9D                    ; if OAM ID is already
        cmp     ent_anim_id,y                 ; electric shocking, don't
        beq     L81B3                   ; bother resetting it up

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
L81B3:  jmp     L824D

L81B6:  lda     ent_hp,x                 ; if any hit-ack flags set
        and     #$E0                    ; (bits 7/6/5 = already hit)
        beq     L81C0                   ; skip — already took damage
        jmp     L822F                   ; else don't

L81C0:  ldy     ent_routine,x
        lda     ent_hp,x                 ; subtract health by
        sec                             ; damage table value
        sbc     (L0000),y               ; indexed by main routine index
        bcs     L81CD                   ; minimum health is 00
        lda     #$00                    ; (no negatives)
L81CD:  sta     ent_hp,x                 ; store new health value
        bne     L8207                   ; if nonzero, we're still alive

; dead
        lda     ent_routine,x
        cmp     #$52                    ; if Proto Man ($52 or $53)
        beq     L8207                   ; don't do normal death —
        cmp     #$53                    ; Proto Man has his own
        beq     L8207                   ; fly-away exit in main_proto_man
        lda     boss_active
        bpl     L81E5                   ; $5A<0: boss active → OAM $71
        lda     #$59                    ; $5A>=0: normal → OAM $59
        bne     L81E7
L81E5:  lda     #$71
L81E7:  jsr     LF835                   ; animate enemy's death
        lda     #$00                    ; turn off collision
        sta     ent_hitbox,x
        lda     ent_routine,x
        cmp     #$30                    ; if not bolton and nutton
        bne     L81FD
        lda     #$00                    ; use $00 death routine index
        sta     ent_routine,x                 ; for bolton and nutton
        beq     L8202
L81FD:  lda     #$7A                    ; for everything else,
        sta     ent_routine,x                 ; use $7A routine
L8202:  lda     #$90
        sta     ent_flags,x
L8207:  lda     ent_hp,x                 ; health zero?
        beq     L822F                   ; don't set boss flags

; not dead
        lda     ent_status,x
        and     #$40                    ; if sprite is a boss
        bne     L821D
        lda     ent_hp,x
        ora     #$20                    ; set bit 5 = hit-ack flag
        sta     ent_hp,x                 ; (prevents double-damage this frame)
        bne     L822F
L821D:  lda     stage_id                     ; if stage == Wily 4
        cmp     #STAGE_WILY4                    ; or < Wily 1
        beq     L8227                   ; this means robot master
        cmp     #STAGE_WILY1                    ; or doc robot bosses
        bcs     L822F
L8227:  lda     ent_hp,x
        ora     #$E0                    ; for doc/robot masters,
        sta     ent_hp,x                 ; set all hit-ack flags (bits 7/6/5)
L822F:  lda     current_weapon
        cmp     #WPN_TOP                    ; if weapon is top spin
        beq     L824D                   ; no shot to despawn
        ldy     $10
        lda     #$00                    ; despawn the shot
        sta     ent_status,y
        lda     current_weapon
        cmp     #WPN_GEMINI
        bne     L824D                   ; if weapon is gemini laser,
        lda     #$00                    ; despawn all three shots
        sta     $0301
        sta     $0302
        sta     $0303
L824D:  pla
        sta     prg_bank                     ; restore bank
        jsr     LFF6B                   ; and sprite slot
        ldx     $0F
        clc
        lda     ent_status,x
        and     #$40                    ; is sprite a boss?
        bne     L82AA
L825D:  rts                             ; if not, return

L825E:  lda     ent_hitbox,x                 ; shot tink flag also
        and     #$20                    ; implies invulnerable
        bne     L825D                   ; to top spin, return
        jsr     LFAE2                   ; check if enemy collidiog with
        bcs     L825D                   ; player, if not return
        stx     $0F                     ; preserve X
        lda     prg_bank
        pha                             ; preserve $A000-$BFFF bank
        lda     #$0A                    ; select $0A as new bank
        sta     prg_bank
        jsr     LFF6B
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
        bcs     L8290
        lda     #$00                    ; clamp to 0
L8290:  ora     #$80                    ; set dirty flag
        sta     $A7
        lda     #PSTATE_TOP_SPIN                    ; state → $0A (Top Spin recoil)
        sta     player_state                     ; player bounces back from contact
        lda     #$08                    ; recoil timer = 8 frames
        sta     ent_timer
        lda     L83B4
        sta     L0000
        lda     L83C0
        sta     $01
        jmp     L81B6

L82AA:  lda     ent_hp,x                 ; boss health bits
        and     #$1F                    ; mask to HP (low 5 bits)
        ora     #$80                    ; set bit 7 = "boss was hit" flag
        sta     boss_hp_display                     ; store to boss HP mirror
        and     #$7F                    ; did boss die?
        beq     L82B8
        rts                             ; if not, return

L82B8:  lda     #$F2                    ; SFX $F2 = stop music
        jsr     LF898                   ; submit_sound_ID_D9
        lda     #$17                    ; SFX $17 = boss death sound
        jsr     LF89A                   ; submit_sound_ID
        ldy     #$1F
code_82C4:  lda     boss_active
        bmi     code_82CC
        lda     #$7A
        bne     code_82CE
code_82CC:  lda     #$5B
code_82CE:  jsr     LF846               ; init_child_entity
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
        bne     code_82C4
        lda     stage_id
        cmp     #STAGE_HARD
        bne     code_831E
        lda     #$00
        sta     scroll_y
code_831E:  lda     #$00
        sta     $0301
        sta     $0302
        sta     $0303
        sta     ent_var1
        lda     stage_id                     ; current stage
        cmp     #STAGE_WILY4                    ; stage $0F = Wily 4 (refights)
        beq     L8360                   ; special handling for refights
        lda     player_state                     ; check player state
        cmp     #PSTATE_DEATH                    ; if player is dead ($0E),
        beq     code_83AD               ; don't start victory cutscene
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
        beq     L835E                   ; skip animation reset
        sta     ent_anim_id
        lda     #$00
        sta     ent_anim_state                   ; reset animation frame
        sta     ent_anim_frame                   ; reset animation counter
L835E:  clc
        rts

; Wily 4 (stage $0F) boss refight — no victory cutscene, just unstun and
; spawn the "boss defeated" entity at the boss's position

L8360:  lda     player_state
        cmp     #PSTATE_STUNNED                    ; if player was stunned ($0F),
        bne     code_836A
        lda     #$00                    ; release to on_ground ($00)
        sta     player_state
code_836A:  lda     #$80
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
code_83AD:  .byte   $18,$60

; weapon damage table pointers, low then high
weapon_damage_ptr_lo:  .byte   $00,$00,$00,$00,$00 ; Buster, Gemini, Needle, Hard, Magnet
L83B4:  .byte   $00,$00,$00,$00,$00,$00,$00 ; TopSpin, Snake, RushCoil, Spark, RushMarine, Shadow, RushJet
weapon_damage_ptr_hi:  .byte   $A1,$A4,$A2,$A5,$A3 ; Buster, Gemini, Needle, Hard, Magnet
L83C0:  .byte   $A6,$A7,$A1,$A8,$A1,$A9,$A1 ; TopSpin, Snake, RushCoil, Spark, RushMarine, Shadow, RushJet

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
        beq     code_85D6
        jsr     LF71D                   ; move right (unchecked)
        jmp     code_85D9

code_85D6:  jsr     LF73B               ; move left (unchecked)
code_85D9:  cpx     #$10                ; only weapon/player slots break blocks
        bcs     code_8627               ; enemy slots ($10+) → return
        ldy     #$06                    ; check tile at foot height ahead
        jsr     LE8D6
        lda     tile_at_feet_max                     ; tile type = breakable block ($70)?
        cmp     #TILE_DISAPPEAR
        bne     code_8627               ; no → return
        lda     ent_x_px,x                 ; entity X - camera X
        sec                             ; = screen-relative position
        sbc     camera_x_lo
        cmp     #$10                    ; < $10 → off left edge
        bcc     code_8627
        cmp     #$F0                    ; >= $F0 → off right edge
        bcs     code_8627               ; must be visible to break
        jsr     LEE57                   ; erase 2x2 metatile from nametable
        bcs     code_8627               ; carry set = buffer full, abort
        jsr     LFC53                   ; find free slot for debris entity
        bcc     code_8628               ; found → spawn debris

; --- no free slot or max debris: become explosion in place ---
code_8600:  lda     #$71                ; OAM $71 = small explosion sprite
        jsr     LF835                   ; reset_sprite_anim
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
        jmp     code_867C               ; mark block destroyed in bitfield

code_8627:  rts

; --- free slot found: count existing debris, then spawn child ---

code_8628:  sty     $01                 ; save free slot index
        lda     #$00                    ; debris counter = 0
        sta     L0000
        ldy     #$1F                    ; scan enemy slots $10-$1F
code_8630:  lda     ent_status,y             ; skip inactive slots
        bpl     code_863E
        lda     ent_routine,y                 ; is this a debris entity (routine $27)?
        cmp     #$27
        bne     code_863E               ; no → skip
        inc     L0000                   ; count++
code_863E:  dey                         ; loop $1F down to $10
        cpy     #$0F
        bne     code_8630
        ldy     $01                     ; restore free slot
        lda     L0000                   ; already 3 debris on screen?
        cmp     #$03
        beq     code_8600               ; yes → just explode, no child
        lda     #$71                    ; spawn child with OAM $71 (explosion)
        jsr     LF846                   ; init_child_entity
        lda     #$27                    ; child AI routine = $27
        sta     ent_routine,y                 ; (falling debris handler)
        lda     ent_x_px,x                 ; snap child X to metatile grid center
        and     #$F0
        ora     #$08
        sta     ent_x_px,y
        lda     ent_x_scr,x                 ; copy X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; snap child Y to metatile grid center
        and     #$F0
        ora     #$08
        sta     ent_y_px,y
        lda     #$00                    ; child has no damage flags (harmless)
        sta     ent_hitbox,y
        sta     ent_status,x                 ; deactivate parent (breaker entity)
        lda     #$FF                    ; ent_spawn_id = $FF (cleanup marker)
        sta     ent_spawn_id,x

; --- mark block destroyed in $0110 bitfield ---
code_867C:  stx     L0000               ; save entity slot
        lda     $13                     ; nametable page (bit 0) << 5
        and     #$01                    ; → $00 or $20 (page offset)
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        sta     $01
        lda     $28                     ; $28 = metatile column index
        pha                             ; column >> 1 | page_offset
        lsr     a                       ; → Y = byte index into $0110
        ora     $01
        tay
        pla                             ; column << 2 AND $04
        asl     a                       ; → bit pair selector
        asl     a                       ; ORA $03 (metatile sub-pos)
        and     #$04                    ; → X = bit index for bitmask_table
        ora     $03
        tax
        lda     $0110,y                 ; set destroyed bit via
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
        bne     code_86BC               ; state 1+ → $99 fall

; --- state 0: initial upward toss ---
        jsr     LF797                   ; apply initial Y velocity
        lda     ent_y_px,x                 ; entity Y + $10
        clc
        adc     #$10                    ; if below player Y →
        cmp     ent_y_px                   ; still rising, freeze anim
        bcc     code_8712
        inc     ent_status,x                 ; → state 1 ($99 fall)

; --- state 1: fall with $99 ---
code_86BC:  ldy     #$00                ; apply $99 + move down
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_8712               ; no landing → freeze anim
        lda     ent_anim_state,x                 ; landed: check anim frame
        cmp     #$04                    ; must be frame 4 (final bounce)
        bne     code_8717               ; not ready → return
        lda     #$81                    ; switch to routine $81
        sta     ent_routine,x                 ; (item waiting on ground)
        lda     #$80                    ; active, state 0
        sta     ent_status,x
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        ldy     #$03                    ; check tile at mid-height ahead
        jsr     LE8D6
        lda     $10                     ; solid wall? (bit 4)
        and     #$10
        beq     code_86F0               ; no wall → check weapon type
code_86E4:  inc     ent_status,x             ; advance state (wall blocked)
        lda     #$00                    ; clear Y speed
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
        rts

code_86F0:  lda     current_weapon                 ; current weapon = Rush Marine ($09)?
        cmp     #WPN_RUSH_MARINE
        bne     code_86FC               ; no → set weapon OAM
        lda     tile_at_feet_max                     ; tile type = water ($80)?
        cmp     #$80
        bne     code_86E4               ; not water → wall-blocked path
code_86FC:  lda     ent_flags,x             ; set sprite flag bit 0
        ora     #$01                    ; (direction/visibility)
        sta     ent_flags,x
        lda     current_weapon                     ; weapon_id - 6, >> 1 = table index
        sec                             ; $06→0, $08→1, $0A→2,
        sbc     #$06                    ; $07→0, $09→1, $0B→2
        lsr     a
        tay
        lda     L8718,y                 ; OAM from weapon_oam_table
        jsr     LF835                   ; set sprite animation
        rts

code_8712:  lda     #$00                ; freeze animation timer
        sta     ent_anim_frame,x                 ; (keep current frame)
code_8717:  .byte   $60

; weapon_oam_table: OAM IDs indexed by (weapon_id - 6) >> 1
; $D8=Search Snake, $D9=Spark Shock, $D7=Shadow Blade
; $81=Rush Coil, $82=Rush Marine, $83=Rush Jet
L8718:  .byte   $D8,$D9,$D7
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
        bne     code_876B               ; state 1 → rising

; --- state 0: item sitting on ground, timer countdown ---
        dec     ent_timer,x                 ; decrement wait timer
        beq     code_874B               ; timer done → become beam
        lda     ent_anim_id,x                 ; if OAM = $D8 (Search Snake item)
        cmp     #$D8
        bne     code_873B               ; other → skip freeze
        lda     #$00                    ; freeze anim timer
        sta     ent_anim_frame,x
        lda     ent_anim_state,x                 ; if anim frame != 0 → return
        bne     code_8795               ; (wait for idle frame)
code_873B:  lda     ent_timer,x             ; timer >= $88?
        cmp     #$88                    ; (still early in wait)
        bcs     code_8795               ; → normal display, return
        lda     ent_anim_frame,x                 ; set bit 7 of anim timer
        ora     #$80                    ; (flicker/flash effect
        sta     ent_anim_frame,x                 ; when about to expire)
        rts

; --- timer expired: become teleport beam rising ---

code_874B:  inc     ent_status,x             ; advance to state 1
        lda     #$00                    ; clear Y speed
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
        sta     ent_hitbox,x                 ; clear damage flags
        lda     ent_flags,x                 ; clear direction bits 0-1
        and     #$FC
        sta     ent_flags,x
        lda     #$13                    ; OAM $13 = teleport beam
        jsr     LF835                   ; reset_sprite_anim
        lda     #$04                    ; set anim frame to 4
        sta     ent_anim_state,x                 ; (beam animation start)

; --- state 1: accelerate upward and rise off screen ---
code_876B:  lda     ent_anim_state,x             ; wait for anim frame 2
        cmp     #$02                    ; (beam fully formed)
        bne     code_8795               ; not yet → return
        lda     #$00                    ; freeze anim at frame 2
        sta     ent_anim_frame,x
        lda     ent_yvel_sub,x                 ; Y speed += $99 ($99)
        clc                             ; accelerate upward
        adc     gravity
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        jsr     LF779                   ; move up (unchecked)
        lda     ent_y_scr,x                 ; if Y screen != 0
        beq     code_8795               ; (off-screen) → deactivate
        lda     #$00                    ; deactivate entity
        sta     ent_status,x
code_8795:  rts

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
        sbc     ent_x_px,x
        pha                             ; save low byte
        lda     ent_x_scr                   ; screen page subtraction
        sbc     ent_x_scr,x
        pla
        bcs     code_87AC               ; player is to the right
        eor     #$FF                    ; negate: absolute distance
        adc     #$01
        clc                             ; carry clear = player left
code_87AC:  php                         ; save direction (carry)
        cmp     #$03                    ; clamp speed to max 3 px/frame
        bcc     code_87B3
        lda     #$03
code_87B3:  plp                         ; restore direction
        sta     ent_xvel,x                 ; set X speed = clamped distance
        lda     #$00                    ; sub-pixel = 0
        sta     ent_xvel_sub,x
        bcc     code_87C6               ; player left → move left
        ldy     #$08                    ; move right with collision
        jsr     LF580                   ; move_right_collide
        jmp     code_87CB

code_87C6:  ldy     #$09                ; move left with collision
        jsr     LF5C4                   ; move_left_collide
code_87CB:  lda     ent_flags               ; copy player facing (bit 6)
        and     #$40                    ; to slot 1 entity
        sta     L0000
        lda     $0581                   ; clear old bit 6
        and     #$BF                    ; set to player's facing
        ora     L0000
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
        beq     code_884B               ; if neither L/R set, vertical phase
        lda     #$97
        cmp     ent_anim_id,x                 ; set horizontal OAM sprite ($97)
        beq     code_87EE               ; (skip write if already set)
        sta     ent_anim_id,x
code_87EE:  lda     ent_facing,x
        and     #$01                    ; bit 0 = moving right
        beq     code_87FB
        jsr     LF71D                   ; move missile right
        jmp     code_87FE               ; skip left branch

code_87FB:  jsr     LF73B               ; move missile left
code_87FE:  ldy     #$1F                ; Y = slot 31 (start of enemy scan)
code_8800:  lda     ent_status,y             ; scan enemy slots $1F down to $10
        bpl     code_8845               ; skip if slot inactive (bit 7 clear)
        lda     ent_hitbox,y                 ; check if entity is hittable
        and     #$40                    ; ($40 = can be hit by weapons)
        beq     code_8845               ; skip if not hittable
        lda     ent_x_px,x
        sec                             ; 16-bit X distance:
        sbc     ent_x_px,y                 ; missile.X - enemy.X
        pha                             ; save low byte of difference
        lda     ent_x_scr,x                 ; subtract screen bytes
        sbc     ent_x_scr,y                 ; (carry propagates from pixel sub)
        pla                             ; restore low byte; carry = sign
        bcs     code_8821               ; positive (missile >= enemy)? skip
        eor     #$FF                    ; negative: negate to get |distance
        adc     #$01                    ; (carry was clear from BCS fall-thru)
code_8821:  cmp     #$08                ; X distance| < 8 pixels?
        bcs     code_8845               ; no: skip this enemy
        lda     ent_xvel_sub,x                 ; enemy found within range!
        sta     ent_yvel_sub,x                 ; copy X speed (sub) → Y speed (sub)
        lda     ent_xvel,x                 ; copy X speed (whole) → Y speed
        sta     ent_yvel,x                 ; (vertical speed = 4.0 px/frame)
        lda     #$08                    ; default: direction = up ($08)
        sta     ent_facing,x                 ; (clears horizontal bits)
        lda     ent_y_px,x                 ; compare Y positions:
        sec                             ; missile.Y - enemy.Y
        sbc     ent_y_px,y
        bcs     code_884A               ; missile below enemy? up is correct, done
        lda     #$04                    ; missile above enemy:
        sta     ent_facing,x                 ; change direction to down ($04)
        rts

code_8845:  dey                         ; next enemy slot
        cpy     #$0F                    ; loop until Y < $10 (slot 15)
        bne     code_8800               ; (scans slots $1F down to $10)
code_884A:  rts                         ; no enemy in range, keep flying

code_884B:  lda     ent_yvel,x             ; --- vertical phase ---
        cmp     #$06                    ; check if at terminal velocity
        beq     code_8863               ; (6.0 px/frame); skip accel if so
        lda     ent_yvel_sub,x
        clc                             ; accelerate: Y speed += $00.20
        adc     #$20                    ; ($0.125 px/frame per frame)
        sta     ent_yvel_sub,x
        lda     ent_yvel,x                 ; add carry to whole byte
        adc     #$00
        sta     ent_yvel,x
code_8863:  lda     ent_facing,x             ; check vertical direction
        and     #$08                    ; bit 3 ($08) = moving up
        beq     code_8875               ; not set? moving down
        lda     #$9A                    ; OAM $9A = vertical up sprite
        sta     ent_anim_id,x
        jsr     LF779                   ; move missile upward
        jmp     code_887D               ; skip down branch

code_8875:  jsr     LF759               ; move missile downward
        lda     #$9B                    ; OAM $9B = vertical down sprite
        sta     ent_anim_id,x
code_887D:  lda     ent_y_scr,x             ; check Y screen position
        beq     code_8887               ; if Y screen == 0, still on-screen
        lda     #$00                    ; otherwise despawn missile
        sta     ent_status,x                 ; (clear active flag)
code_8887:  rts

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
        beq     code_8899               ; 0 = free movement phase
        dec     ent_timer,x                 ; decrement bounce timer
        lda     ent_timer,x                 ; compare against stagger threshold
        cmp     code_8943,x             ; ($B4/$B2/$B0 for slots 1/2/3)
        bcc     code_8899               ; below threshold → start moving
        rts                             ; still waiting (staggered spawn delay)

code_8899:  lda     ent_facing,x             ; check horizontal direction
        and     #$01                    ; bit 0 = moving right?
        beq     code_88BB               ; no → moving left
        lda     ent_timer,x                 ; bounce timer active?
        beq     code_88AD               ; 0 → free movement (no collision)
        ldy     #$1E                    ; collision point: right edge
        jsr     LF580                   ; move right with wall detection
        jmp     code_88D6               ; → check if wall was hit

code_88AD:  lda     ent_flags,x             ; set facing right (bit 6)
        ora     #$40                    ; (free phase: set manually since
        sta     ent_flags,x                 ; move_sprite doesn't set facing)
        jsr     LF71D                   ; move right, no collision
        jmp     code_88FD               ; → vertical movement

code_88BB:  lda     ent_timer,x             ; bounce timer active?
        beq     code_88C8               ; 0 → free movement
        ldy     #$1F                    ; collision point: left edge
        jsr     LF5C4                   ; move left with wall detection
        jmp     code_88D6               ; → check if wall was hit

code_88C8:  lda     ent_flags,x             ; clear facing (bit 6 = 0 → left)
        and     #$BF
        sta     ent_flags,x
        jsr     LF73B                   ; move left, no collision
        jmp     code_88FD               ; → vertical movement

; --- wall bounce handler (after horizontal collision-checked move) ---

code_88D6:  bcc     code_88FD           ; C=0: no wall hit → vertical movement
        lda     #$00                    ; zero sub-pixel speeds
        sta     ent_yvel_sub,x                 ; Y speed sub = 0
        sta     ent_xvel_sub,x                 ; X speed sub = 0
        lda     #$03                    ; set both speeds to 3.0 px/frame
        sta     ent_yvel,x                 ; Y speed whole = 3
        sta     ent_xvel,x                 ; X speed whole = 3
        lda     ent_facing,x                 ; flip horizontal direction
        eor     #$03                    ; (swap bits 0↔1: right↔left)
        sta     ent_facing,x
        and     #$0C                    ; already has vertical component?
        bne     code_8943               ; yes → done (keep existing V dir)
        lda     ent_facing,x                 ; first bounce: add upward direction
        ora     #$08                    ; (bit 3 = up)
        sta     ent_facing,x
        rts

; --- vertical movement (after horizontal move, no wall hit) ---

code_88FD:  lda     ent_facing,x             ; check vertical direction bits
        and     #$0C                    ; (bits 2-3)
        beq     code_8943               ; no vertical component → done
        .byte   $29                     ; bit 2 = moving down?
L8905:  .byte   $04
        beq     code_8922               ; no → moving up
        lda     ent_timer,x                 ; bounce timer active?
        beq     code_891A               ; 0 → free movement
        ldy     #$12                    ; collision point: bottom edge
        jsr     LF606                   ; move down with floor detection
        lda     #$A0                    ; OAM = $A0 (angled down)
        sta     ent_anim_id,x
        jmp     code_8939               ; → check if floor was hit

code_891A:  lda     #$A0                ; OAM = $A0 (angled down)
        sta     ent_anim_id,x
        jmp     LF759                   ; move down, no collision

code_8922:  lda     ent_timer,x             ; bounce timer active?
        bne     code_892F               ; nonzero → collision-checked move
        lda     #$A1                    ; OAM = $A1 (angled up)
        sta     ent_anim_id,x
        jmp     LF779                   ; move up, no collision

code_892F:  ldy     #$13                ; collision point: top edge
        jsr     LF642                   ; move up with ceiling detection
        lda     #$A1                    ; OAM = $A1 (angled up)
        sta     ent_anim_id,x

; --- floor/ceiling bounce handler ---
code_8939:  bcc     code_8943           ; C=0: no hit → done
        lda     ent_facing,x                 ; flip vertical direction
        eor     #$0C                    ; (swap bits 2↔3: down↔up)
        sta     ent_facing,x
code_8943:  .byte   $60
        ldy     $B2,x                   ; stagger thresholds per slot (1/2/3)
        bcs     L8905
        cpy     #$05
        cmp     #$71                    ; $71 = fist opening?
        beq     code_8961               ; → check if opening done
        cmp     #$AC                    ; $AC = launch anim frame 1?
        beq     code_8956               ; → wait for anim timer
        cmp     #$AE                    ; $AE = launch anim frame 2?
        bne     code_896D               ; neither → already flying, skip to movement
code_8956:  lda     ent_anim_frame,x             ; launch anim: wait for timer to expire
        bne     code_89C1               ; nonzero = still animating
        lda     #$71                    ; timer done → switch to fist opening
        sta     ent_anim_id,x
        rts

code_8961:  lda     ent_anim_state,x             ; fist opening ($71): wait for frame 4
        cmp     #$04                    ; (fist fully open)
        bne     code_89C1               ; not yet → return
        lda     #$AF                    ; done → switch to flying fist anim
        jsr     LF835                   ; reset_sprite_anim
code_896D:  lda     ent_xvel,x             ; flying phase: accelerate X speed
        cmp     #$03                    ; already at max $03.00?
        beq     code_8985               ; yes → skip acceleration
        lda     ent_xvel_sub,x                 ; X speed sub += $20
        clc                             ; ($00.20 = 0.125 px/frame per frame)
        adc     #$20
        sta     ent_xvel_sub,x
        lda     ent_xvel,x                 ; carry into whole byte
        adc     #$00
        sta     ent_xvel,x
code_8985:  lda     ent_facing,x             ; move horizontally based on facing
        and     #$01                    ; bit 0: 1=right, 0=left
        beq     code_8992
        jsr     LF71D                   ; move_sprite_right
        jmp     code_8995

code_8992:  jsr     LF73B               ; move_sprite_left
code_8995:  lda     $95                 ; Y wobble via frame parity
        and     #$01                    ; $95 = global frame counter
        beq     code_89A1               ; even frame → Y-1
        inc     ent_y_px,x                 ; odd frame: Y += 1 (nudge down)
        jmp     code_89A4

code_89A1:  dec     ent_y_px,x             ; even frame: Y -= 1 (nudge up)
code_89A4:  lda     joy1_held                 ; D-pad steering: check Up/Down held
        and     #$0C                    ; ($08=Up, $04=Down)
        beq     code_89C1               ; neither → return
        and     #$08                    ; Up held?
        beq     code_89B4               ; no → Down
        jsr     LF779                   ; steer upward
        jmp     code_89B7

code_89B4:  jsr     LF759               ; steer downward
code_89B7:  lda     ent_y_scr,x             ; offscreen check after vertical steer
        beq     code_89C1               ; Y screen 0 = on-screen → return
        lda     #$00                    ; offscreen: despawn
        sta     ent_status,x
code_89C1:  rts

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
        bne     code_89F6               ; nonzero → state 1: crawling

; --- state 0: falling (searching for floor) ---
        ldy     #$12                    ; hitbox index for $99
        jsr     LF67C                   ; apply $99; C=1 if landed
        bcs     code_89DB               ; landed → begin crawling
        lda     ent_timer,x                 ; horizontal move timer
        beq     code_8A47               ; expired → stop moving, RTS
        dec     ent_timer,x                 ; decrement timer
        jmp     L8A80                   ; move horizontally (shared code)

; --- landed: set crawl speed and transition to state 1 ---

code_89DB:  lda     #$00                ; zero sub-pixel speeds
        sta     ent_xvel_sub,x
        sta     ent_yvel_sub,x
        lda     #$03                    ; crawl speed = 3.0 px/frame both axes
        sta     ent_xvel,x
        sta     ent_yvel,x
        lda     ent_facing,x                 ; add "down" to direction (pressing floor)
        ora     #$04
        sta     ent_facing,x
        inc     ent_status,x                 ; state 0 → 1

; --- state 1: crawl on surface ---
code_89F6:  lda     ent_facing,x             ; bit 3 = moving up?
        and     #$08
        bne     code_8A0A               ; yes → climb upward
        ldy     #$12                    ; move downward with collision
        jsr     LF606                   ; C=1 if hit solid below
        lda     #$A6                    ; OAM $A6 = descending wall
        sta     ent_anim_id,x
        jmp     code_8A14

code_8A0A:  ldy     #$13                ; move upward with collision
        jsr     LF642                   ; C=1 if hit solid above
        lda     #$A7                    ; OAM $A7 = ascending wall
        sta     ent_anim_id,x
code_8A14:  lda     ent_y_scr,x             ; Y screen nonzero = offscreen
        bne     L8A92                   ; → despawn
        bcs     code_8A48               ; C=1: hit solid vertically

; --- no vertical contact: at surface edge, try corner wrap ---
        lda     ent_facing,x                 ; isolate vertical direction
        and     #$0C
        tay                             ; Y = $04(down) or $08(up)
        lda     ent_facing,x                 ; save original direction
        pha
        cpy     #$08                    ; moving up? don't flip horizontal
        beq     code_8A2E
        eor     #$03                    ; moving down: flip horizontal
        sta     ent_facing,x                 ; (reverse to probe around floor edge)
code_8A2E:  lda     ent_anim_id,x             ; save OAM
        pha
        jsr     code_8A55               ; probe horizontal move
        pla                             ; restore OAM
        sta     ent_anim_id,x
        pla                             ; restore original direction
        sta     ent_facing,x
        bcs     code_8A78               ; C=1: hit wall → corner wrap done
        lda     ent_facing,x                 ; no wall: flip vertical (wrap edge)
        eor     #$0C                    ; down↔up
        sta     ent_facing,x
code_8A47:  rts

; --- hit solid vertically ---

code_8A48:  lda     ent_facing,x             ; moving up and hit ceiling?
        and     #$08
        beq     code_8A55               ; no → hit floor, move horizontal
        lda     #$00                    ; up + ceiling = dead end: despawn
        sta     ent_status,x
        rts

; --- move horizontally along surface ---

code_8A55:  lda     #$A5                ; OAM $A5 = horizontal
        sta     ent_anim_id,x
        lda     ent_facing,x                 ; bit 0 = moving right?
        and     #$01
        beq     code_8A69               ; no → move left
        ldy     #$1E                    ; move right with wall detection
        jsr     LF580                   ; move_right_collide
        jmp     code_8A6E

code_8A69:  ldy     #$1F                ; move left with wall detection
        jsr     LF5C4                   ; move_left_collide
code_8A6E:  bcc     code_8A78           ; no wall → done
        lda     ent_facing,x                 ; hit wall: flip vertical direction
        eor     #$0C                    ; (transition to wall climbing)
        sta     ent_facing,x
code_8A78:  rts
main_spark_shock:

        lda     ent_status,x                 ; sprite state nonzero?
        and     #$0F                    ; shocking an enemy
        bne     L8A8D
L8A80:  lda     ent_facing,x                 ; facing left?
        and     #$01                    ; move left
        beq     L8A8A
        jmp     LF71D                   ; else move right

L8A8A:  jmp     LF73B                   ; move_sprite_left

L8A8D:  dec     ent_timer,x                 ; decrease shock timer
        bne     L8A97                   ; return if not expired
L8A92:  lda     #$00                    ; on timer expiration,
        sta     ent_status,x                 ; despawn (set inactive)
L8A97:  rts
main_shadow_blade:

        lda     ent_facing,x                 ; facing neither right nor left?
        and     #$03                    ; skip horizontal movement
        beq     L8AAC
        and     #$01                    ; facing left? move left
        beq     L8AA9
        jsr     LF71D                   ; else move right
        jmp     L8AAC

L8AA9:  jsr     LF73B                   ; move_sprite_left
L8AAC:  lda     ent_facing,x                 ; facing neither up nor down?
        and     #$0C                    ; skip vertical movement
        beq     L8AC0
        and     #$08                    ; facing down? move down
        beq     L8ABD
        jsr     LF779                   ; else move up
        jmp     L8AC0

L8ABD:  jsr     LF759                   ; move_sprite_down
L8AC0:  lda     ent_y_scr,x                 ; offscreen vertically?
        bne     L8AD1                   ; despawn
        dec     ent_timer,x                 ; movement timer not expired?
        bne     L8AFA                   ; return
        lda     ent_status,x                 ; on timer expired, check if
        and     #$0F                    ; state == $00, or blade hasn't
        beq     L8AD7                   ; flipped yet, if it has return
L8AD1:  lda     #$00                    ; set to inactive
        sta     ent_status,x
        rts

L8AD7:  inc     ent_status,x                 ; indicate flipped state
        lda     #$14                    ; reset movement timer
        sta     ent_timer,x
        lda     ent_facing,x                 ; if not facing up or down,
        and     #$0C                    ; only flip horizontal
        beq     L8AF2
        lda     ent_facing,x                 ; flip vertical
        eor     #$0C                    ; facing direction
        sta     ent_facing,x
        and     #$03
        beq     L8AFA
L8AF2:  lda     ent_facing,x                 ; flip horizontal
        eor     #$03                    ; facing direction
        sta     ent_facing,x
L8AFA:  rts

; ===========================================================================
; main_dada — Dada (bouncing robot, Hard Man stage)
; ===========================================================================
; Walks horizontally with $99, bouncing in 3 progressively higher arcs.
; Re-faces player every 3 bounces. ent_timer=bounce index (0-2), ent_var1=face timer.
main_dada:

        lda     ent_status,x                 ; state 0: init
        and     #$0F
        bne     code_8B0A               ; already init'd → skip
        inc     ent_status,x                 ; state → 1
        lda     #$03                    ; face-player countdown = 3 bounces
        sta     ent_var1,x
code_8B0A:  lda     ent_facing,x             ; walk horizontally with wall collision
        and     #$01
        beq     code_8B19               ; bit 0 clear → move left
        ldy     #$0A
        jsr     LF580                   ; move_right_collide
        jmp     code_8B1E

code_8B19:  ldy     #$0B
        jsr     LF5C4                   ; move_left_collide
code_8B1E:  ldy     #$0A                ; apply $99; C=1 if landed
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_8B51               ; still airborne → return
        lda     ent_timer,x                 ; on landing: load bounce Y speed
        tay                             ; from table indexed by bounce#
        lda     L8B52,y                 ; Y speed sub (3 entries)
        sta     ent_yvel_sub,x
        lda     L8B55,y                 ; Y speed whole (3 entries)
        sta     ent_yvel,x
        dec     ent_var1,x                 ; decrement face-player counter
        bne     code_8B42               ; not zero → skip re-facing
        jsr     LF869                   ; re-face toward player
        lda     #$03                    ; reset counter to 3
        sta     ent_var1,x
code_8B42:  inc     ent_timer,x             ; advance bounce index
        lda     ent_timer,x
        cmp     #$03                    ; wrap at 3 (cycle 0→1→2→0)
        bcc     code_8B51
        lda     #$00
        sta     ent_timer,x
code_8B51:  .byte   $60

; bounce Y speeds: sub={$44,$44,$EA}, whole={$03,$03,$07}
; bounce 0: $03.44, bounce 1: $03.44, bounce 2: $07.EA (big hop)
L8B52:  .byte   $44,$44,$EA
L8B55:  .byte   $03
        .byte   $03
        .byte   $07

; ===========================================================================
; main_potton — Potton (helicopter dropper, Snake Man stage)
; ===========================================================================
; Flies horizontally, reverses on wall hit. When player is within 4 screens
; X-distance, stops and drops a bomb (Copipi child). OAM $23=flying, $24=bomb bay open.
main_potton:
        lda     ent_anim_id,x                 ; OAM $23 = flying with propeller
        cmp     #$23
        beq     code_8B73               ; already dropping → skip movement
        lda     ent_facing,x                 ; horizontal movement with wall collision
        and     #$01
        beq     code_8B6E
        ldy     #$08
        jsr     LF580                   ; move_right_collide
        jmp     code_8B73

code_8B6E:  ldy     #$09
        jsr     LF5C4                   ; move_left_collide
code_8B73:  bcc     code_8B7D           ; hit wall → reverse direction
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_8B7D:  lda     ent_status,x             ; state check
        and     #$0F
        bne     code_8B92               ; state 1+ → check bomb drop anim
        jsr     LF8C2                   ; state 0: check range to player
        cmp     #$04                    ; < 4 screens away?
        bcs     code_8BA8               ; no → return
        inc     ent_status,x                 ; state → 1 (stop and drop)
        lda     #$23                    ; set OAM $23 (propeller stop anim)
        bne     code_8BA5               ; → reset_sprite_anim
code_8B92:  lda     ent_anim_id,x             ; already showing bomb bay ($24)?
        cmp     #$24
        beq     code_8BA8               ; yes → done
        lda     ent_anim_state,x                 ; anim frame == 6? (propeller stop done)
        cmp     #$06
        bne     code_8BA8               ; not yet → wait
        jsr     code_8BA9               ; spawn bomb child (Copipi)
        lda     #$24                    ; OAM $24 = bomb bay open
code_8BA5:  jsr     LF835               ; reset_sprite_anim
code_8BA8:  rts

; --- spawn_copipi: drop bomb child below Potton ---

code_8BA9:  jsr     LFC53               ; find free enemy slot
        bcs     code_8BDD               ; none → return
        lda     ent_facing,x                 ; copy parent facing to child
        sta     ent_facing,y
        lda     ent_x_px,x                 ; copy X position
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; Y = parent Y + $11 (below)
        clc
        adc     #$11
        sta     ent_y_px,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$25                    ; OAM $25 = Copipi (bomb)
        jsr     LF846                   ; init_child_entity
        lda     #$04                    ; AI routine = $04 (falling bomb)
        sta     ent_routine,y
        lda     #$C0                    ; dmg flags: $C0 = hurts player + hittable
        sta     ent_hitbox,y
code_8BDD:  rts

        lda     ent_status,x
        and     #$0F
        bne     code_8BEB
        jsr     LF81B                   ; reset_gravity
        inc     ent_status,x
code_8BEB:  ldy     #$08
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_8BFC
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_routine,x
code_8BFC:  rts

; ===========================================================================
; main_hammer_joe — Hammer Joe (shielded enemy, throws hammers)
; ===========================================================================
; Cycle: shield up (invulnerable, timer ent_timer=$1E) → shield open (OAM $27,
; vulnerable) → throw hammer at anim frame $0A → shield close → repeat.
; Tracks player facing. ent_var1 = opened-once flag (toggles vulnerability).
main_hammer_joe:

        lda     ent_status,x                 ; state 0: init
        and     #$0F
        bne     code_8C12               ; already init'd → skip
        sta     ent_var1,x                 ; clear opened flag
        lda     #$1E                    ; shield timer = $1E (30 frames)
        sta     ent_timer,x
        jsr     LF883                   ; face player
        inc     ent_status,x                 ; state → 1
code_8C12:  lda     ent_timer,x             ; shield timer active?
        bne     code_8C2A               ; yes → count down
        lda     ent_anim_frame,x                 ; anim still playing?
        ora     ent_anim_state,x                 ; (frame timer or seq index nonzero)
        bne     code_8C2D               ; yes → track player + continue
        lda     #$27                    ; OAM $27 = shield open (visor up)
        cmp     ent_anim_id,x                 ; already set?
        beq     code_8C42               ; yes → process open state
        sta     ent_anim_id,x                 ; set it
        rts

code_8C2A:  dec     ent_timer,x             ; decrement shield timer
code_8C2D:  lda     ent_facing,x             ; save old facing, re-face player
        pha
        jsr     LF869                   ; face_player
        pla                             ; if facing changed,
        cmp     ent_facing,x                 ; flip sprite horizontally
        beq     code_8C42
        lda     ent_flags,x
        eor     #$40
        sta     ent_flags,x
code_8C42:  lda     ent_anim_id,x             ; only act when shield is open ($27)
        cmp     #$27
        bne     code_8C7F               ; closed → return
        lda     ent_var1,x                 ; first time opening this cycle?
        bne     code_8C59               ; no → skip vulnerability toggle
        lda     ent_hitbox,x                 ; toggle vulnerability bits ($60)
        eor     #$60                    ; now hittable + hurts player
        sta     ent_hitbox,x
        inc     ent_var1,x                 ; mark as opened
code_8C59:  lda     ent_anim_state,x             ; anim frame == $0A? (throw frame)
        cmp     #$0A
        bne     code_8C69               ; not yet → check for close
        lda     ent_anim_frame,x                 ; frame timer == 0? (exact moment)
        bne     code_8C69               ; no → wait
        jsr     code_8C80               ; spawn hammer projectile
        rts

code_8C69:  lda     ent_anim_frame,x             ; anim fully complete?
        ora     ent_anim_state,x                 ; (both zero = anim done)
        bne     code_8C7F               ; still playing → return
        dec     ent_anim_id,x                 ; close shield: OAM $27→$26
        dec     ent_status,x                 ; state back to 0 (re-init timer)
        lda     ent_hitbox,x                 ; toggle vulnerability off
        eor     #$60                    ; (shield closed = invulnerable)
        sta     ent_hitbox,x
code_8C7F:  rts

; --- spawn_hammer: create thrown hammer projectile ---

code_8C80:  jsr     LFC53               ; find free enemy slot
        bcs     code_8CCE               ; none → return
        sty     L0000                   ; save child slot
        lda     ent_facing,x                 ; copy facing to hammer
        sta     ent_facing,y
        and     #$02                    ; index: 0=facing right, 2=facing left
        tay
        lda     ent_x_px,x                 ; hammer X = Joe X + offset
        clc                             ; (+$13 if right, -$13 if left)
        adc     L8CCF,y
        pha
        lda     ent_x_scr,x                 ; with screen carry
        adc     L8CD0,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x                 ; hammer Y = Joe Y - 6 (arm height)
        sec
        sbc     #$06
        sta     ent_y_px,y
        lda     #$33                    ; hammer X speed = $03.33 (3.2 px/f)
        sta     ent_xvel_sub,y
        lda     #$03
        sta     ent_xvel,y
        lda     #$28                    ; OAM $28 = hammer sprite
        jsr     LF846                   ; init_child_entity
        lda     #$2D                    ; AI routine = $2D (arcing projectile)
        sta     ent_routine,y
        lda     #$C0                    ; dmg flags: hurts player + hittable
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
code_8CCE:  .byte   $60

; hammer X offset: right=$0013, left=$FFED (-19)
L8CCF:  .byte   $13
L8CD0:  brk
        sbc     LA0FF
        brk
        jsr     LF67C                   ; carry=1 if landed
        rol     $0F                     ; save landed flag in $0F bit 0
        lda     ent_anim_id,x                 ; if OAM ID == $6A (crouch anim),
        cmp     #$6A                    ; skip horizontal movement
        beq     code_8D03               ; (crouching before jump)
        lda     ent_var1,x                 ; if ent_var1 == 0, skip walk timer
        beq     code_8CEF               ; (not in post-land walk phase)
        lda     ent_timer,x                 ; if walk timer > 0,
        beq     code_8CEF               ; decrement and wait
        dec     ent_timer,x                 ; (post-land walk delay)
        rts

code_8CEF:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = facing right
        beq     code_8CFE
        ldy     #$00                    ; move right with wall collision
        jsr     LF580                   ; move_right_collide
        jmp     code_8D03

code_8CFE:  ldy     #$01                ; move left with wall collision
        jsr     LF5C4                   ; move_left_collide
code_8D03:  bcc     code_8D0D           ; if no wall hit, skip
        lda     ent_facing,x                 ; hit wall: flip direction
        eor     #$03                    ; toggle bits 0+1 (left/right)
        sta     ent_facing,x
code_8D0D:  lda     ent_status,x             ; check state (bits 0-3)
        and     #$0F                    ; if already in jump state,
        bne     code_8D23               ; skip proximity trigger
        jsr     LF8C2                   ; get X distance to player
        cmp     #$40                    ; if distance >= $40 pixels,
        bcs     code_8D23               ; stay in walk state
        inc     ent_status,x                 ; state 0 -> 1 (enter jump)
        lda     #$6A                    ; switch to crouch anim ($6A)
        jsr     LF835                   ; (pre-jump windup)
code_8D23:  lda     ent_anim_id,x             ; if current OAM != $6A (crouch),
        cmp     #$6A                    ; not ready to jump yet
        bne     code_8D9C
        lda     ent_anim_state,x                 ; wait until anim reaches frame 2
        cmp     #$02                    ; (crouch anim finished)
        bne     code_8D9C
        lda     #$6B                    ; switch to jump anim ($6B)
        jsr     LF835                   ; reset_sprite_anim
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
        jsr     LFC53                   ; find free slot for child projectile
        bcs     code_8D9C               ; no free slot, skip spawn
        sty     L0000                   ; save child slot in $00
        lda     ent_facing,x                 ; use facing to index X offset table
        and     #$02                    ; y=0 if right, y=2 if left
        tay
        lda     ent_x_px,x                 ; child X = parent X + offset
        clc                             ; (16-bit add from table at $8DC4)
        adc     L8DC4,y
        pha
        lda     ent_x_scr,x
        adc     L8DC5,y
        ldy     L0000                   ; restore child slot
        sta     ent_x_scr,y                 ; child X.screen
        pla
        sta     ent_x_px,y                 ; child X.pixel
        lda     ent_y_px,x                 ; child Y = parent Y
        sta     ent_y_px,y
        lda     #$01                    ; child HP = 1
        sta     ent_hp,y
        lda     #$6C                    ; init child entity with OAM $6C
        jsr     LF846                   ; (projectile sprite)
        lda     #$C2                    ; child damage flags = $C2
        sta     ent_hitbox,y                 ; (hurts player + hittable + invincible)
        lda     #$44                    ; child Y speed = $03.44
        sta     ent_yvel_sub,y                 ; (falling arc projectile)
        lda     #$03
        sta     ent_yvel,y
        lda     #$09                    ; child AI routine = $09
        sta     ent_routine,y                 ; ($99 projectile handler)
        jmp     code_8DC3               ; done

code_8D9C:  lda     ent_anim_id,x             ; if OAM != $6B (jump anim),
        cmp     #$6B                    ; skip landing logic
        bne     code_8DC3
        lda     $0F                     ; check landed flag (saved from
        and     #$01                    ; move_vertical_gravity earlier)
        beq     code_8DC3               ; not landed yet, keep falling
        lda     #$6D                    ; switch to walk-toward anim ($6D)
        jsr     LF835                   ; reset_sprite_anim
        jsr     LF869                   ; face player after landing
        lda     #$00                    ; set X speed = $02.00
        sta     ent_xvel_sub,x                 ; (walk toward player)
        lda     #$02
        sta     ent_xvel,x
        lda     #$10                    ; set walk timer = $10 frames
        sta     ent_timer,x                 ; (walk toward player briefly)
        inc     ent_var1,x                 ; set post-land walk flag (ent_var1=1)
code_8DC3:  rts

; bubukan child projectile X offset table (read as data at $8DC4)
; also serves as auto_walk_spawn_done trampoline for child entity

L8DC4:  .byte   $20
L8DC5:  brk
        cpx     #$FF

; child projectile AI: just apply Y speed ($99 projectile)
        jmp     LF797                   ; apply_y_speed

; =============================================
; Jamacy -- chain/spike ball enemy
; =============================================
; Oscillates vertically at speed $00.C0/frame. Initial half-period
; is read from table at $8E12 based on AI routine index ($15->$60,
; $16->$70 frames). When the period counter expires, reverses Y
; direction and doubles the period (each swing longer than the last).
; ent_timer = current countdown, ent_var1 = base period (doubled each reversal)
; =============================================
main_jamacy:

        lda     ent_status,x                 ; check state (bits 0-3)
        and     #$0F                    ; if already initialized,
        bne     code_8DED               ; skip init
L8DD2:  sta     ent_yvel,x                 ; set Y speed = $00.C0
        lda     #$C0                    ; (slow vertical drift; A=0 from AND)
        sta     ent_yvel_sub,x
        lda     ent_routine,x                 ; AI routine index - $15 = table offset
        sec                             ; (routine $15 -> y=0, $16 -> y=1)
        sbc     #$15
        tay
        lda     L8E12,y                 ; load initial half-period from table
        sta     ent_timer,x                 ; set as current countdown
        sta     ent_var1,x                 ; save as base period for doubling
        inc     ent_status,x                 ; state 0 -> 1 (oscillating)
code_8DED:  lda     ent_facing,x             ; check direction bit 0
        and     #$01                    ; bit 0 set = moving up
        beq     code_8DFA
        jsr     LF779                   ; move up at current Y speed
        jmp     code_8DFD

code_8DFA:  jsr     LF759               ; move down at current Y speed
code_8DFD:  dec     ent_timer,x             ; decrement period counter
        bne     code_8E11               ; if not expired, done
        lda     ent_facing,x                 ; reverse Y direction
        eor     #$03                    ; toggle bits 0+1
        sta     ent_facing,x
        lda     ent_var1,x                 ; double the base period and reload
        asl     a                       ; (note: ent_var1 is NOT updated, so
        sta     ent_timer,x                 ; it always doubles from initial value)
code_8E11:  rts

; jamacy initial half-period table: routine $15=$60, $16=$70 frames

L8E12:  rts

        bvs     L8DD2
        brk
        .byte   $03
        and     #$0F                    ; if already initialized,
        bne     code_8E29               ; skip init
        inc     ent_status,x                 ; state 0 -> 1 (flying)
        lda     #$00                    ; clear speed countdown and
        sta     ent_timer,x                 ; table index (start at entry 0,
        sta     ent_var1,x                 ; load speeds immediately)
        jsr     LF883                   ; set sprite flip from facing
code_8E29:  lda     ent_status,x             ; if state bit 1 set (state >= 2),
        and     #$02                    ; jump to walking bomb / homing
        beq     code_8E33
        jmp     code_8ED3               ; -> state 2+ handler

code_8E33:  lda     ent_timer,x             ; if speed countdown > 0,
        bne     code_8E86               ; skip to movement (use current speeds)
        ldy     ent_var1,x                 ; load table index -> indirection table
        lda     L8F3C,y                 ; $8F3C[idx] = speed pair index
        asl     a                       ; * 2 for 16-bit entries
        tay
        lda     L8F4A,y                 ; load Y speed (16-bit signed)
        sta     ent_yvel_sub,x                 ; from table at $8F4A
        lda     L8F4B,y
        sta     ent_yvel,x
        lda     L8F6A,y                 ; load X speed (16-bit signed)
        sta     ent_xvel_sub,x                 ; from table at $8F6A
        lda     L8F6B,y
        sta     ent_xvel,x
        lda     ent_xvel,x                 ; if X speed is positive (moving right),
        bpl     code_8E72               ; skip negation
        lda     ent_xvel_sub,x                 ; negate X speed (16-bit two's complement)
        eor     #$FF                    ; for leftward-facing entities, table
        clc                             ; values are mirrored so both directions
        adc     #$01                    ; use the same sinusoidal curve
        sta     ent_xvel_sub,x
        lda     ent_xvel,x
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x
code_8E72:  inc     ent_var1,x             ; advance table index
        lda     ent_var1,x                 ; wrap at 14 entries (0-13)
        cmp     #$0E
        bne     code_8E81
        lda     #$00
        sta     ent_var1,x
code_8E81:  lda     #$05                ; reset speed countdown = 5 frames
        sta     ent_timer,x                 ; (hold each speed for 5 frames)
code_8E86:  dec     ent_timer,x             ; decrement speed countdown
        lda     ent_y_sub,x                 ; apply Y speed manually (16-bit)
        clc                             ; Y.sub += Yspd.sub
        adc     ent_yvel_sub,x                 ; Y.pixel += Yspd.whole + carry
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     ent_yvel,x
        sta     ent_y_px,x
        lda     ent_facing,x                 ; apply X movement based on facing
        and     #$02                    ; bit 1 = facing left
        bne     code_8EAA
        jsr     LF71D                   ; move right at X speed
        bcs     code_8EAD               ; unconditional jump
        bcc     code_8EAD               ; (BCS+BCC = always)
code_8EAA:  jsr     LF73B               ; move left at X speed
code_8EAD:  lda     ent_routine,x             ; if AI routine != $0A (not PenPen),
        cmp     #$0A                    ; skip to bomb flier proximity check
        bne     code_8F04
        jsr     LFB7B                   ; check if player weapon hit PenPen
        bcs     code_8ED2               ; no hit, return
        ldy     $10                     ; destroy the weapon that hit us
        lda     #$00                    ; ($10 = weapon slot from collision)
        sta     ent_status,y
        inc     ent_status,x                 ; state 1 -> 2 (walking bomb)
        lda     #$80                    ; set X speed = $02.80
        sta     ent_xvel_sub,x                 ; (walking bomb speed)
        lda     #$02
        sta     ent_xvel,x
        lda     #$48                    ; switch to walking bomb anim ($48)
        jsr     LF835                   ; reset_sprite_anim
code_8ED2:  rts

code_8ED3:  lda     ent_routine,x             ; if AI routine != $0A (not PenPen),
        cmp     #$0A                    ; skip to bomb flier homing movement
        bne     code_8F1F
        lda     ent_anim_id,x                 ; if already on walking anim ($49),
        cmp     #$49                    ; go straight to walking movement
        beq     code_8EF7
        lda     ent_anim_frame,x                 ; wait for current anim to finish
        ora     ent_anim_state,x                 ; (timer=0 AND frame=0)
        bne     code_8EF6               ; still animating, return
        lda     #$49                    ; switch to walking bomb anim ($49)
        jsr     LF835                   ; reset_sprite_anim
        lda     ent_hitbox,x                 ; set damage flags: hurts player +
        ora     #$C3                    ; hittable + invincible ($C3)
        sta     ent_hitbox,x                 ; (walking bomb is dangerous)
code_8EF6:  rts

code_8EF7:  lda     ent_facing,x             ; walk in facing direction
        and     #$01                    ; bit 0 = right
        beq     code_8F01
        jmp     LF71D                   ; walk right

code_8F01:  jmp     LF73B               ; walk left

code_8F04:  jsr     LF8C2               ; get X distance to player
        cmp     #$30                    ; if distance >= $30 pixels,
        bcs     code_8EF6               ; too far, return (keep flying)
        lda     #$00                    ; set homing speed = $02.00
        sta     $02                     ; ($02/$03 = speed params for
        lda     #$02                    ; calc_homing_velocity)
        sta     $03
        jsr     LFC63                   ; calculate homing direction + speed
        lda     $0C                     ; set direction flags from homing result
        sta     ent_facing,x
        inc     ent_status,x                 ; advance state (-> homing flight)
        rts

code_8F1F:  lda     ent_facing,x             ; move horizontally based on direction
        and     #$01                    ; bit 0 = right
        beq     code_8F2C
        jsr     LF71D                   ; move right
        jmp     code_8F2F

code_8F2C:  jsr     LF73B               ; move left
code_8F2F:  lda     ent_facing,x             ; move vertically based on direction
        and     #$08                    ; bit 3 = up
        beq     code_8F39
        jmp     LF779                   ; move up

code_8F39:  .byte   $4C,$59,$F7         ; move down

; sinusoidal speed indirection table (14 entries, indexes into Y/X speed tables)
L8F3C:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$01
        .byte   $02,$03,$04,$05,$06,$07

; Y speed table (16 signed 16-bit values, indexed by indirection * 2)
L8F4A:  .byte   $CD
L8F4B:  .byte   $FE,$E5,$FE,$27,$FF,$8B,$FF,$00
        .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE

; X speed table (16 signed 16-bit values, indexed by indirection * 2)
L8F6A:  .byte   $00
L8F6B:  .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
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
        bne     code_8FCF               ; state 1+: already active, skip to movement
        jsr     LFAF6                   ; state 0: check if player standing on platform
        bcc     code_8F97               ; player on top -> activate
        rts                             ; not standing on it -> wait

code_8F97:  inc     ent_status,x             ; advance to state 1 (active flying)
        lda     #$CC                    ; Y speed sub = $CC
        sta     ent_yvel_sub,x                 ; rise speed $00.CC (~0.8 px/frame upward)
        lda     #$00
        sta     ent_yvel,x                 ; Y speed whole = $00
        lda     #$02                    ; initial direction = left ($02)
        sta     ent_facing,x
        lda     #$10                    ; movement timer = 16 frames (first segment)
        sta     ent_timer,x
        lda     #$B4                    ; lifetime timer = 180 frames ($B4)
        sta     ent_var2,x
        lda     #$E8                    ; Y position = $E8 (start near bottom of screen)
        sta     ent_y_px,x
        lda     ent_x_px,x                 ; save X aligned to 16px metatile boundary
        and     #$F0                    ; mask off low nibble
        ora     #$08                    ; center within metatile (+8)
        sta     ent_var3,x                 ; store saved X for child spawn position
        lda     #$00
        sta     ent_x_sub,x                 ; clear X sub-pixel
        lda     ent_flags,x                 ; clear sprite flag bit 2
        and     #$FB
        sta     ent_flags,x
code_8FCF:  jsr     LF779               ; rise upward (apply Y speed)
        lda     ent_flags,x                 ; check tile collision flag (bit 5)
        and     #$20
        beq     code_8FEC               ; not set -> skip tile check
        ldy     #$06                    ; Y offset for tile check (center of platform)
        jsr     LE8D6                   ; check tile at current horizontal position
        lda     $10                     ; tile result flags
        and     #$10                    ; bit 4 = solid tile
        bne     code_9018               ; solid -> skip horizontal movement
        lda     ent_flags,x                 ; no longer in solid: clear tile check flag
        and     #$DF
        sta     ent_flags,x
code_8FEC:  lda     ent_facing,x             ; check direction
        and     #$01                    ; bit 0 = right
        beq     code_8FF9               ; not right -> move left
        jsr     LF71D                   ; move right
        jmp     code_8FFC

code_8FF9:  jsr     LF73B               ; move left
code_8FFC:  dec     ent_timer,x             ; decrement movement timer
        bne     code_9018               ; not expired -> skip direction change
        inc     ent_var1,x                 ; advance direction table index
        lda     ent_var1,x
        and     #$03                    ; wrap index to 0-3
        sta     ent_var1,x
        tay
        lda     L9030,y                 ; load direction from table (L,R,R,L)
        sta     ent_facing,x                 ; set new direction
        lda     #$0E                    ; reset timer = 14 frames for next segment
        sta     ent_timer,x
code_9018:  lda     ent_var2,x             ; check lifetime timer
        beq     code_9025               ; already expired -> check offscreen
        dec     ent_var2,x                 ; decrement lifetime
        bne     code_902F               ; not zero yet -> done for this frame
        jsr     code_9034               ; timer just hit 0: spawn replacement platform
code_9025:  lda     ent_y_scr,x             ; check Y screen (high byte)
        beq     code_902F               ; still on screen 0 -> keep alive
        lda     #$00                    ; scrolled offscreen: deactivate entity
        sta     ent_status,x
code_902F:  .byte   $60

; Cloud platform direction table: left, right, right, left (zigzag)
L9030:  .byte   $02
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
code_9034:  jsr     LFC53               ; find free enemy slot -> Y
        bcs     code_9095               ; no free slot -> abort
        lda     #$70                    ; OAM ID = $70 (cloud platform sprite)
        jsr     LF846                   ; initialize child entity in slot Y
        lda     #$14                    ; AI routine = $14 (main_cloud_platform)
        sta     ent_routine,y
        lda     #$81                    ; active ($80) + state 1 (already flying)
        sta     ent_status,y
        lda     ent_flags,y                 ; set sprite flags: bit 5 (tile check) + bit 0
        ora     #$21
        sta     ent_flags,y
        lda     #$0F                    ; damage flags = $0F (invulnerable platform)
        sta     ent_hitbox,y
        lda     ent_var3,x                 ; copy saved X from parent
        sta     ent_x_px,y                 ; set child X position
        sta     ent_var3,y                 ; propagate saved X for future clones
        lda     ent_x_scr,x                 ; copy X screen from parent
        sta     ent_x_scr,y
        lda     #$00                    ; reset direction index to 0
        sta     ent_var1,y
        sta     ent_y_scr,y                 ; Y screen = 0
        sta     ent_yvel,y                 ; Y speed whole = 0
        sta     ent_xvel,y                 ; X speed whole = 0
        lda     #$CC                    ; Y speed sub = $CC (rise speed $00.CC)
        sta     ent_yvel_sub,y
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y
        lda     #$E8                    ; Y position = $E8 (bottom of screen)
        sta     ent_y_px,y
        lda     #$02                    ; initial direction = left ($02)
        sta     ent_facing,y
        lda     #$10                    ; movement timer = 16 frames
        sta     ent_timer,y
        lda     #$B4                    ; lifetime timer = 180 frames ($B4)
        sta     ent_var2,y
        lda     #$E8                    ; Y position = $E8 (redundant store)
        sta     ent_y_px,y
code_9095:  rts

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
        bne     code_90DE               ; timer active -> decrement and wait
        jsr     LFC53                   ; find free enemy slot -> Y
        bcs     code_90D6               ; no free slot -> reset timer, skip spawn
        lda     ent_facing,x                 ; copy parent direction to child
        sta     ent_facing,y
        lda     ent_x_px,x                 ; copy parent X position to child
        sta     ent_x_px,y
        lda     ent_x_scr,x                 ; copy parent X screen to child
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; copy parent Y position to child
        sta     ent_y_px,y
        lda     #$01                    ; child HP = 1
        sta     ent_hp,y
        lda     #$18                    ; child AI routine = $18
        sta     ent_routine,y
        lda     #$4E                    ; child OAM ID = $4E
        jsr     LF846                   ; init child entity
        lda     #$C0                    ; damage flags = $C0 (hurts player)
        sta     ent_hitbox,y
        lda     #$80                    ; X speed sub = $80
        sta     ent_xvel_sub,y                 ; child X speed = $01.80 (1.5 px/frame)
        lda     #$01
        sta     ent_xvel,y                 ; X speed whole = $01
code_90D6:  lda     #$F0                ; spawn cooldown = 240 frames ($F0)
        sta     ent_timer,x
        jmp     LF869                   ; turn toward player

code_90DE:  dec     ent_timer,x             ; decrement spawn cooldown
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
        bne     code_90EF               ; state 1+: skip init
        jsr     LF81B                   ; state 0: zero Y speed
        inc     ent_status,x                 ; advance to state 1
code_90EF:  lda     ent_anim_id,x             ; check current OAM ID
        cmp     #$4E                    ; OAM $4E = grounded/falling mode
        bne     code_912C               ; different OAM -> initial drop state
        ldy     #$08                    ; $99 strength index
        jsr     LF67C                   ; apply $99 + move (C=1 if landed)
        ror     L0000                   ; save carry (landed flag) into $00 bit 7
        lda     tile_at_feet_max                     ; tile ID at feet (below entity)
        cmp     #TILE_LADDER_TOP                    ; special trigger tile?
        beq     code_9107               ; yes -> check horizontal tiles
        lda     L0000                   ; no special tile: check if landed
        bpl     code_9164               ; not landed (bit 7 clear) -> done
code_9107:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = facing right
        beq     code_9116               ; facing left -> check left tile
        lda     tile_at_feet_lo                     ; facing right: check tile to right
        cmp     #TILE_LADDER_TOP                    ; is it the trigger tile?
        bne     code_9142               ; no -> walk horizontally
        beq     code_911C               ; yes -> transition to rising
code_9116:  lda     $42                 ; facing left: check tile to left
        cmp     #$40                    ; is it the trigger tile?
        bne     code_9142               ; no -> walk horizontally
code_911C:  lda     #$4F                ; switch to rising OAM sprite ($4F)
        jsr     LF835                   ; reset animation
        lda     #$80                    ; Y speed sub = $80
        sta     ent_yvel_sub,x                 ; rise speed = $01.80 (1.5 px/frame upward)
        lda     #$01
        sta     ent_yvel,x                 ; Y speed whole = $01
        rts

code_912C:  ldy     #$0C                ; collision check offset
        jsr     LF606                   ; move down with collision (C=1 if landed)
        bcc     code_9141               ; not landed -> keep falling
        dec     ent_status,x                 ; landed: go back to state 0
        jsr     LF869                   ; turn toward player
        jsr     LF81B                   ; zero Y speed
        lda     #$4E                    ; switch to grounded OAM ($4E)
        jsr     LF835                   ; reset animation
code_9141:  rts

code_9142:  lda     ent_facing,x             ; check facing direction
        and     #$01                    ; bit 0 = right
        beq     code_9151               ; facing left -> move left
        ldy     #$08                    ; collision offset for right
        jsr     LF580                   ; move right with wall check
        jmp     code_9156

code_9151:  ldy     #$09                ; collision offset for left
        jsr     LF5C4                   ; move left with wall check
code_9156:  lda     $10                 ; tile collision result flags
        and     #$10                    ; bit 4 = hit solid wall
        beq     code_9164               ; no wall -> done
        lda     ent_facing,x                 ; hit wall: reverse direction
        eor     #$03                    ; flip both direction bits (left<->right)
        sta     ent_facing,x
code_9164:  rts

; --- Unreferenced code block at $9165 ---
; Gravity fall, on landing: state 0 sets X speed $03.44 and faces player,
; state 1 walks horizontally in facing direction.

        ldy     #$00                    ; $99 index 0
        jsr     LF67C                   ; fall with $99
        bcc     code_9186               ; not landed -> done
        lda     ent_status,x                 ; landed: check state
        and     #$0F
        bne     code_9187               ; state 1+ -> walk horizontally
        inc     ent_status,x                 ; state 0: advance to state 1
        lda     #$44                    ; X speed sub = $44
        sta     ent_yvel_sub,x                 ; walk speed = $03.44 (~3.27 px/frame)
        lda     #$03
        sta     ent_yvel,x                 ; X speed whole = $03
        jsr     LF869                   ; face toward player
        jsr     LF883                   ; update sprite flip to match direction
code_9186:  rts

code_9187:  lda     ent_facing,x             ; check direction
        and     #$01                    ; bit 0 = right
        beq     code_9191               ; facing left -> move left
        jmp     LF71D                   ; move right

code_9191:  jmp     LF73B               ; move left

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
        jsr     LF67C                   ; apply $99
        lda     ent_anim_id,x                 ; current OAM ID
        cmp     #$BC                    ; is it stopped (launching) sprite?
        bne     code_9209               ; if not, go to walk/bounce state logic
        lda     ent_anim_frame,x                 ; anim frame timer
        ora     ent_anim_state,x                 ; OR with anim sequence frame
        bne     code_9208               ; if animation still playing, wait
        jsr     code_9286               ; count active children (spawn group $80)
        lda     ent_var2,x                 ; $FF = children exist, $00 = none
        bne     code_9209               ; if children active, skip spawn → walk state
        dec     ent_anim_id,x                 ; OAM $BC → $BB (walk sprite, visual transition)
        jsr     LFC53                   ; find free enemy slot → Y
        bcs     code_9203               ; no free slot: skip spawn, reset timer
        lda     ent_x_px,x                 ; copy parent X pixel
        sta     ent_x_px,y
        lda     ent_x_scr,x                 ; copy parent X screen
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; parent Y pixel
        sbc     #$17                    ; spawn 23 pixels above parent
        sta     ent_y_px,y
        lda     #$BD                    ; child entity type: small springer
        jsr     LF846                   ; initialize child entity in slot Y
        lda     #$75                    ; AI routine index
        sta     ent_routine,y
        lda     #$C0                    ; damage flags: hurts player + hittable
        sta     ent_hitbox,y
        lda     #$01                    ; HP = 1
        sta     ent_hp,y
        lda     #$80                    ; spawn group marker $80 (for child counting)
        sta     ent_spawn_id,y
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
code_9203:  lda     #$00                ; reset parent walk timer
        sta     ent_timer,x
code_9208:  rts

; -- giant springer: walk/bounce state dispatch --

code_9209:  lda     ent_status,x             ; active flag + state
        and     #$0F                    ; isolate state bits
        bne     code_9256               ; state 1 → bouncing
        lda     ent_facing,x                 ; direction flags
        and     #$01                    ; bit 0 = moving right?
        beq     code_921F               ; if not, move left
        ldy     #$20                    ; speed index for move right
        jsr     LF580                   ; move right with wall check
        jmp     code_9224

code_921F:  ldy     #$21                ; speed index for move left
        jsr     LF5C4                   ; move left with wall check
code_9224:  bcc     code_922E           ; C=0: no wall hit, skip reversal
        lda     ent_facing,x                 ; hit a wall: flip direction
        eor     #$03                    ; toggle both left/right bits
        sta     ent_facing,x
code_922E:  jsr     LF8C2               ; A = horizontal distance to player
        cmp     #$30                    ; within 48 pixels?
        bcs     code_9242               ; no → continue walking
        lda     #$3C                    ; bounce duration = 60 frames
        sta     ent_var1,x
        inc     ent_status,x                 ; state 0 → state 1
        lda     #$C2                    ; OAM ID $C2 = bouncing sprite
        jmp     LF835                   ; reset_sprite_anim

code_9242:  lda     #$CA                ; damage flags: hittable + hurts player
        sta     ent_hitbox,x
        inc     ent_timer,x                 ; increment walk frame counter
        lda     ent_timer,x
        cmp     #$1E                    ; walked for 30 frames?
        bne     code_9285               ; not yet → continue
        lda     #$BC                    ; switch to stopped OAM (ready to launch)
        jmp     LF835                   ; reset_sprite_anim

; -- state 1: bouncing in place --

code_9256:  jsr     LF8B3               ; check vertical distance to player
        bcs     code_925F               ; C=1: player is below → use $CA
        lda     #$DB                    ; player above: damage flags $DB
        bne     code_9261               ; (always taken)
code_925F:  lda     #$CA                ; player below: damage flags $CA
code_9261:  sta     ent_hitbox,x             ; set damage flags
        lda     ent_var1,x                 ; bounce timer
        beq     code_926E               ; if already 0, check distance
        dec     ent_var1,x                 ; decrement bounce timer
        bne     code_9285               ; still bouncing → done
code_926E:  jsr     LF8C2               ; A = distance to player
        cmp     #$30                    ; player still within 48 px?
        bcc     code_9285               ; yes → keep bouncing
        dec     ent_status,x                 ; state 1 → state 0
        lda     #$BB                    ; OAM ID $BB = walking sprite
        jsr     LF835                   ; reset_sprite_anim
        jsr     LF869                   ; turn toward player
        lda     #$00                    ; reset walk frame counter
        sta     ent_timer,x
code_9285:  rts

; -----------------------------------------------------------------------------
; count_springer_children — counts active small springers (spawn group $80)
; -----------------------------------------------------------------------------
; Scans enemy slots $10-$1F for entities with ent_spawn_id == $80.
; Sets ent_var2,x = $00 if no children found (allow spawning),
;              = $FF if any children exist (block spawning).
; -----------------------------------------------------------------------------

code_9286:  lda     #$00                ; child count = 0
        sta     L0000
        lda     #$80                    ; target spawn group marker
        sta     $01
        ldy     #$1F                    ; start from slot 31 (last enemy slot)
code_9290:  lda     ent_status,y             ; is slot active? (bit 7)
        bmi     code_92AA               ; yes → check if it's a springer child
code_9295:  dey                         ; next slot
        cpy     #$0F                    ; scanned down to slot $10?
        bne     code_9290               ; no → keep scanning
        lda     L0000                   ; child count
        bne     code_92A4               ; if > 0, block spawning
        lda     #$00                    ; no children: allow spawning
        sta     ent_var2,x
        rts

code_92A4:  lda     #$FF                ; children exist: block spawning
        sta     ent_var2,x
        rts

code_92AA:  lda     $01                 ; $80 = spawn group marker
        cmp     ent_spawn_id,y                 ; does this entity's group match?
        bne     code_9295               ; no → skip
        inc     L0000                   ; yes → increment child count
        jmp     code_9295               ; continue scanning

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
        bne     code_92F5               ; if > 0, skip direction recalculation
        jsr     LF954                   ; sets ent_facing = base dir, ent_var2 = adjustment
        lda     ent_facing,x                 ; base direction index
        clc
        adc     ent_var2,x                 ; + fine adjustment → combined direction
        tay                             ; Y = table index (0-31)
        lda     L9349,y                 ; Y speed sub from direction table
        sta     ent_yvel_sub,x
        lda     L9369,y                 ; Y speed whole from direction table
        sta     ent_yvel,x
        lda     L9389,y                 ; X speed sub from direction table
        sta     ent_xvel_sub,x
        lda     L93A9,y                 ; X speed whole from direction table
        sta     ent_xvel,x
        lda     L93C9,y                 ; OAM ID from direction table
        sta     ent_anim_id,x
        lda     ent_flags,x                 ; sprite flags
        and     #$BF                    ; clear facing bit (bit 6)
        ora     L93E9,y                 ; OR in facing flag from table ($00 or $40)
        sta     ent_flags,x
        lda     ent_var1,x                 ; reload movement duration
        sta     ent_timer,x                 ; reset countdown timer
code_92F5:  dec     ent_timer,x             ; decrement movement timer
        lda     #$00                    ; sign extend X speed whole
        sta     L0000
        lda     ent_xvel,x                 ; X speed whole
        bpl     code_9303               ; positive -> skip sign extension
        dec     L0000                   ; negative -> $00 = $FF (sign extend)
code_9303:  lda     ent_x_sub,x             ; X sub-pixel
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
        bpl     code_9329               ; positive -> skip
        dec     L0000                   ; negative -> $00 = $FF
code_9329:  lda     ent_y_sub,x             ; Y sub-pixel
        clc
        adc     ent_yvel_sub,x                 ; + Y speed sub
        sta     ent_y_sub,x
        lda     ent_y_px,x                 ; Y pixel
        adc     ent_yvel,x                 ; + Y speed whole + carry
        sta     ent_y_px,x
        lda     ent_y_scr,x                 ; Y screen
        adc     L0000                   ; + sign extension + carry
        beq     code_9348               ; still on screen 0 -> done
        lda     #$00                    ; went off-screen vertically
        sta     ent_status,x                 ; deactivate entity
code_9348:  .byte   $60

; -- chibee direction lookup tables (16 directions x 2 sets = 32 entries each) --
; Indexed by combined direction from track_direction_to_player.
; Directions: 0=up, 2=up-right, 4=right, 6=down-right, 8=down, etc. (clockwise)
L9349:  .byte   $00,$27,$4B,$3D,$00,$C3,$B5,$D9 ; Y speed sub (set 1)
        .byte   $00,$D0,$B5,$C3,$00,$3D,$4B,$27
        .byte   $CD,$E5,$27,$8B,$00,$75,$D9,$1B ; Y speed sub (set 2)
        .byte   $33,$1B,$D9,$75,$00,$8B,$27,$E5
L9369:  .byte   $FE,$FE,$FF,$FF,$00,$00,$00,$01 ; Y speed whole (set 1)
        .byte   $02,$01,$00,$00,$00,$FF,$FF,$FE
        .byte   $FE,$FE,$FF,$FF,$00,$00,$00,$01 ; Y speed whole (set 2)
        .byte   $01,$01,$00,$00,$00,$FF,$FF,$FE
L9389:  .byte   $00,$C3,$B5,$D9,$00,$D9,$B5,$C3 ; X speed sub (set 1)
        .byte   $00,$3D,$4B,$27,$00,$27,$4B,$3D
        .byte   $00,$75,$D9,$1B,$33,$1B,$D9,$75 ; X speed sub (set 2)
        .byte   $00,$8B,$27,$E5,$CD,$E5,$27,$8B
L93A9:  .byte   $00,$00,$00,$01,$02,$01,$00,$00 ; X speed whole (set 1)
        .byte   $00,$FF,$FF,$FE,$FE,$FE,$FF,$FF
        .byte   $00,$00,$00,$01,$01,$01,$00,$00 ; X speed whole (set 2)
        .byte   $00,$FF,$FF,$FE,$FE,$FE,$FF,$FF
L93C9:  .byte   $BD,$BD,$BE,$BE,$BF,$BF,$C0,$C0 ; OAM ID per direction (set 1)
        .byte   $C1,$C1,$C0,$C0,$BF,$BF,$BE,$BE
        .byte   $41,$41,$41,$41,$41,$41,$41,$41 ; OAM ID per direction (set 2)
        .byte   $41,$41,$41,$41,$41,$41,$41,$41
L93E9:  .byte   $00,$00,$40,$40,$40,$40,$40,$40 ; facing flag ($00=right, $40=left) (set 1)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$40,$40,$40,$40,$40 ; facing flag (set 2)
        .byte   $00,$00,$00,$00,$00
        brk
        brk
        brk
        jsr     LFAE2                   ; check_player_collision
        bcc     code_9459
        lda     #$00
        sta     L0000
        lda     ent_xvel,x
        bpl     code_9419
        dec     L0000
code_9419:  lda     ent_x_sub,x
        clc
        adc     ent_xvel_sub,x
        sta     ent_x_sub,x
        lda     ent_x_px,x
        adc     ent_xvel,x
        sta     ent_x_px,x
        lda     ent_x_scr,x
        adc     L0000
        sta     ent_x_scr,x
        lda     #$00
        sta     L0000
        lda     ent_yvel,x
        bpl     code_943F
        dec     L0000
code_943F:  lda     ent_y_sub,x
        clc
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     ent_yvel,x
        sta     ent_y_px,x
        lda     ent_y_scr,x
        adc     L0000
        beq     code_945E
code_9459:  lda     #$00
        sta     ent_status,x
code_945E:  rts

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
        and     #$0F
        bne     code_946E               ; state 1+ → main logic

; --- state 0: init ---
        inc     ent_status,x                 ; advance to state 1
        lda     #$3C                    ; electric toggle timer = 60 frames
        sta     ent_timer,x

; --- main loop: horizontal movement + player hit checks ---
code_946E:  lda     ent_hitbox,x             ; clear low 5 damage bits
        and     #$E0                    ; (keep flags, reset damage)
        sta     ent_hitbox,x
        ldy     ent_routine,x                 ; variant index for tables
        lda     ent_y_px,x                 ; save original Y position
        pha
        clc                             ; Y += upper hitbox offset
        adc     code_94AF,y             ; (table-based per variant)
        sta     ent_y_px,x
        jsr     check_player_hit        ; check contact with player
        lda     ent_facing,x                 ; direction bit 0 = right?
        and     #$01
        beq     code_9496
        ldy     #$08                    ; move right with collision
        jsr     LF580                   ; move_right_collide
        jmp     code_949B

code_9496:  ldy     #$09                ; move left with collision
        jsr     LF5C4                   ; move_left_collide
code_949B:  lda     ent_flags,x             ; clear H-flip bit
        and     #$BF                    ; (ball has no facing)
        sta     ent_flags,x
        bcc     code_94AF               ; no wall hit → vertical check
        lda     ent_facing,x                 ; wall hit: reverse direction
        eor     #$03                    ; toggle bits 0-1
        sta     ent_facing,x
        bne     code_94D4               ; → skip to electric toggle
code_94AF:  .byte   $BC
        .byte   $20
L94B1:  .byte   $03
        .byte   $BD
L94B3:  cpy     #$03
L94B5:  clc
        adc     L94B1,y
        sta     ent_y_px,x
        jsr     check_player_hit
        ldy     #$08
        jsr     LF67C                   ; move_vertical_gravity
        ldy     ent_facing,x
        lda     tile_at_feet_max,y
        bne     code_94D4
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_94D4:  lda     ent_timer,x
        bne     code_94F9
        lda     ent_anim_state,x
        ora     ent_anim_frame,x
        bne     code_9509
        ldy     ent_routine,x
        lda     ent_anim_id,x
        cmp     L94B3,y
        bne     code_94FE
        sec
        sbc     #$02
        sta     ent_anim_id,x
        lda     #$3C
        sta     ent_timer,x
        bne     code_9501
code_94F9:  dec     ent_timer,x
        bne     code_9509
code_94FE:  inc     ent_anim_id,x
code_9501:  lda     #$00
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
code_9509:  pla
        sta     ent_y_px,x
        ldy     ent_routine,x
        lda     ent_anim_id,x
        cmp     L94B3,y
        bne     code_9526
        lda     ent_hitbox,x
        and     #$E0
        ora     L94B5,y
        sta     ent_hitbox,x
        .byte   $20,$97,$80
code_9526:  .byte   $60,$D8,$C8,$50,$70
        cmp     $E1
        asl     joy1_press,x
; ---------------------------------------------------------------------------
; main_junk_block -- Junk Block (entity type $64)
; ---------------------------------------------------------------------------
; State 0: wait for player within $3C px, then activate.
; Active: spawns child block (type $94) launched upward (Y speed $FF.AB)
;   at same position. Child gets routine $62, flags $B2, HP=8.
;   ent_var1 cooldown=$5B before next spawn.
;   code_95B9 scans enemy slots for matching X position at target Y.
; ---------------------------------------------------------------------------
main_junk_block:
        lda     ent_status,x
        and     #$0F
        bne     code_9540                   ; already initialized
        jsr     LF8C2                       ; X distance to player
        cmp     #$3C                        ; within 60 px?
        bcs     code_9592                   ; no -> rts
        inc     ent_status,x                ; activate
code_9540:  lda     ent_var1,x              ; spawn cooldown active?
        bne     code_958F                   ; yes -> decrement and return
        lda     #$70
        sta     ent_timer,x                 ; target Y position for slot scan
        jsr     code_95B9                   ; find entity at same X, target Y
        bcs     code_9592                   ; none found -> rts
        jsr     LFC53                       ; find free enemy slot -> Y
        bcs     code_9592                   ; no free slot -> rts
        lda     #$94                        ; child entity type $94
        jsr     LF846                       ; init child entity
        lda     ent_x_px,x                  ; copy parent X to child
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x                  ; copy parent Y to child
        sta     ent_y_px,y
        lda     #$62
        sta     ent_routine,y               ; child AI routine $62
        lda     ent_hitbox,x
        sta     ent_hitbox,y                ; copy parent hitbox
        lda     #$B2
        sta     ent_flags,y                 ; child sprite flags
        lda     #$5B
        sta     ent_var1,x                  ; parent spawn cooldown = $5B
        lda     #$AB
        sta     ent_yvel_sub,y              ; child Y speed = $FF.AB (upward)
        lda     #$FF
        sta     ent_yvel,y
        lda     #$08
        sta     ent_hp,y                    ; child HP = 8
code_958F:  dec     ent_var1,x              ; decrement spawn cooldown
code_9592:  rts

        ldy     #$1E
        jsr     LF67C                   ; move_vertical_gravity
        bcs     code_95B0
        lda     ent_y_px,x
        cmp     #$70
        bcc     code_95B0
        lda     #$90
        sta     ent_timer,x
        jsr     code_95B9
        bcc     code_95B0
        lda     #$70
        sta     ent_y_px,x
code_95B0:  lda     ent_flags,x
        ora     #$20
        sta     ent_flags,x
        rts

; --- scan enemy slots for entity at same X, matching target Y ---
; Returns C=1 if found, C=0 if none.
code_95B9:  stx     L0000
        ldy     #$1F                        ; start from slot $1F
code_95BD:  cpy     L0000
        beq     code_95DD
        lda     ent_status,y
        bpl     code_95DD
        lda     ent_flags,y
        and     #$04
        bne     code_95DD
        lda     ent_x_px,x
        cmp     ent_x_px,y
        bne     code_95DD
        lda     ent_y_px,y
        cmp     ent_timer,x
        beq     code_95E3
code_95DD:  dey
        cpy     #$0F
        bne     code_95BD
        clc
code_95E3:  rts
; ---------------------------------------------------------------------------
; main_petit_snakey -- Petit Snakey (small snake, entity type $4E)
; ---------------------------------------------------------------------------
; State 0: init -- set sprite hflip, face player, timer=$24.
; Active: waits for timer, checks direction to player. If in firing arc
;   (within 7 direction units), switches anim and fires homing projectile
;   via code_9659. ent_var1=$10 attack cooldown, idle timer=$78.
; code_9659: spawns bullet with speed $03.66 using calc_homing_velocity.
; ---------------------------------------------------------------------------
main_petit_snakey:

        lda     ent_status,x
        and     #$0F
        bne     code_95F9                   ; skip init if already active
        jsr     LF883                       ; set_sprite_hflip
        jsr     LF869                       ; face_player
        inc     ent_status,x
        lda     #$24
        sta     ent_timer,x                 ; idle timer = 36 frames
code_95F9:  lda     ent_var1,x              ; attack cooldown active?
        bne     code_9643                   ; yes -> decrement cooldown
        lda     ent_timer,x                 ; idle timer active?
        bne     code_963F                   ; yes -> decrement idle
        lda     ent_facing,x               ; check facing direction
        and     #$02
        bne     code_9617                   ; facing left
        jsr     LF8D9                       ; calc_direction_to_player (right)
        sec
        sbc     #$01                        ; adjust for right-facing arc
        cmp     #$07                        ; in firing arc?
        bcs     code_9639                   ; no -> reset idle timer
        jmp     code_9621                   ; yes -> fire

code_9617:  jsr     LF8D9                   ; calc_direction_to_player (left)
        sec
        sbc     #$09                        ; adjust for left-facing arc
        cmp     #$07                        ; in firing arc?
        bcs     code_9639                   ; no -> reset idle timer
code_9621:  lda     ent_anim_id,x           ; choose attack anim based on current
        cmp     #$D1
        bne     code_962C
        lda     #$D2                        ; attack anim variant A
        bne     code_962E
code_962C:  lda     #$D5                    ; attack anim variant B
code_962E:  jsr     LF835                   ; reset_sprite_anim
        jsr     code_9659                   ; fire homing projectile
        lda     #$10
        sta     ent_var1,x                  ; attack cooldown = 16 frames
code_9639:  lda     #$78                    ; idle timer = 120 frames
        .byte   $9D
L963C:  brk
        ora     stage_select_page
code_963F:  dec     ent_timer,x             ; decrement idle timer
        rts

code_9643:  dec     ent_var1,x              ; decrement attack cooldown
        bne     code_9658
        lda     ent_anim_id,x              ; cooldown done -> revert anim
        cmp     #$D2
        bne     code_9653
        lda     #$D1
        bne     code_9655
code_9653:  lda     #$D4
code_9655:  jsr     LF835               ; reset_sprite_anim
code_9658:  rts

; --- petit snakey: spawn homing projectile (speed $03.66, type $73) ---
code_9659:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_96C0
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     L96C1,y
        pha
        lda     ent_x_scr,x
        adc     L96C2,y
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
        jsr     LFC63                   ; calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     ent_facing,y
        lda     #$73
        jsr     LF846                   ; init_child_entity
        lda     #$8F
        sta     ent_routine,y
        lda     #$8B
        .byte   $99,$80,$04
code_96C0:  .byte   $60
L96C1:  .byte   $04
L96C2:  brk
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
        bne     code_96E5                   ; skip init if active
        jsr     LF8C2                       ; entity_x_dist_to_player
        cmp     #$51                        ; within 81 px?
        bcs     code_9716                   ; no -> rts
        jsr     LF869                       ; face_player
        jsr     LF883                       ; set_sprite_hflip
        lda     ent_flags,x
        and     #$FB                        ; clear bit 2 (enable collision)
        sta     ent_flags,x
        inc     ent_status,x                ; advance to state 1
        rts

        ; --- timer active: flap with gravity ---
code_96E5:  lda     ent_timer,x
        beq     code_9717                   ; timer expired -> state dispatch
        dec     ent_timer,x
        lda     ent_status,x
        and     #$01                        ; odd states skip flapping
        bne     code_9716
        lda     ent_yvel,x                  ; Y speed cap check
        bmi     code_96FD                   ; rising -> keep accelerating
        cmp     #$02
        bcs     code_9716                   ; >= +2 -> stop
code_96FD:  lda     ent_yvel_sub,x          ; gravity accel: +$10/frame
        clc
        adc     #$10
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        bpl     code_9713                   ; positive -> falling
        jmp     LF7A8                       ; apply_y_velocity (rising)

code_9713:  jmp     LF7C8                   ; apply_y_velocity (falling)

code_9716:  rts

        ; --- timer expired: state dispatch ---
code_9717:  lda     ent_status,x
        and     #$0F
        cmp     #$04
        beq     code_9776                   ; state 4: fly forward
        cmp     #$03
        beq     code_9767                   ; state 3: check Y distance
        cmp     #$02
        beq     code_9745                   ; state 2: swoop horizontal
        ; state 1: check Y distance to player
        jsr     LF8B3                       ; entity_y_dist_to_player
        bcc     code_9734                   ; player above -> advance
        cmp     #$4D                        ; > 77 px below?
        bcc     code_9734                   ; close enough -> advance
        jmp     LF797                       ; apply_y_speed (keep descending)

code_9734:  lda     #$14                    ; timer = 20 frames
        sta     ent_timer,x
        inc     ent_status,x                ; advance state
        jsr     LF81B                       ; reset_gravity
        jsr     LF869                       ; face_player
        jmp     LF883                       ; set_sprite_hflip

        ; --- state 2: swoop toward player ---
code_9745:  lda     ent_facing,x
        and     #$02
        beq     code_9758                   ; facing right
        jsr     LF8C2                       ; entity_x_dist_to_player
        bcc     code_9755
        cmp     #$29                        ; within 41 px?
        bcs     code_9764                   ; yes -> advance state
code_9755:  jmp     LF73B                   ; move_sprite_left

code_9758:  jsr     LF8C2                   ; entity_x_dist_to_player
        bcs     code_9761
        cmp     #$29                        ; within 41 px?
        bcs     code_9764                   ; yes -> advance state
code_9761:  jmp     LF71D                   ; move_sprite_right

code_9764:  jmp     code_9734               ; advance to next state

        ; --- state 3: re-check Y distance ---
code_9767:  jsr     LF8B3                   ; entity_y_dist_to_player
        bcc     code_9734                   ; close -> advance
        cmp     #$09                        ; within 9 px?
        bcc     code_9773                   ; close -> advance
        jmp     LF797                       ; apply_y_speed

code_9773:  jmp     code_9734               ; advance to state 4

        ; --- state 4: fly forward in facing direction ---
code_9776:  lda     ent_facing,x
        and     #$02
        beq     code_9780
        jmp     LF73B                       ; move_sprite_left

code_9780:  jmp     LF71D                   ; move_sprite_right

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
        beq     code_978D               ; zero → check state
        dec     ent_timer,x                 ; decrement; still ticking?
        bne     code_97B3               ; → freeze anim + return
code_978D:  lda     ent_status,x             ; state check
        and     #$0F
        bne     code_97D1               ; state 1+ → walking

; --- state 0: hiding / peeking ---
        jsr     LF869                   ; track player direction
        jsr     LF883                   ; flip sprite to face player
        lda     ent_anim_state,x                 ; anim seq index == 0?
        bne     code_97B9               ; not yet → check fire frame
        lda     ent_anim_frame,x                 ; anim frame timer == 1?
        cmp     #$01                    ; (last frame before loop)
        bne     code_97B9               ; no → continue anim
        jsr     LF8C2                   ; player within $41 (~4 tiles)?
        cmp     #$41
        bcs     code_97B3               ; too far → stay hiding
        lda     #$C3                    ; close enough: become vulnerable
        sta     ent_hitbox,x                 ; $C3 = hittable + contact damage
        rts

code_97B3:  lda     #$00                ; freeze animation (stay in helmet)
        sta     ent_anim_frame,x
        rts

code_97B9:  lda     ent_anim_state,x             ; anim frame == 2? (helmet fully open)
        cmp     #$02
        bne     code_97D0               ; not yet → return
        jsr     code_981D               ; fire 3 bullets
        inc     ent_status,x                 ; state → 1 (walking)
        lda     #$13                    ; walk duration = $13 (19 frames)
        sta     ent_var1,x
        lda     #$3C                    ; post-walk hide timer = $3C (60 frames)
        sta     ent_timer,x
code_97D0:  rts

; --- state 1: walking after shooting ---

code_97D1:  lda     #$1D                ; set walking OAM $1D (if not already)
        cmp     ent_anim_id,x
        beq     code_97DB
        jsr     LF835                   ; reset_sprite_anim
code_97DB:  ldy     #$00                ; apply $99; C=1 if on ground
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_97D0               ; airborne → return
        lda     ent_var1,x                 ; walk frames remaining?
        beq     code_97FB               ; zero → done walking
        dec     ent_var1,x                 ; decrement walk counter
        lda     ent_facing,x                 ; walk in facing direction
        and     #$01
        beq     code_97F6
        ldy     #$00
        jmp     LF580

code_97F6:  ldy     #$01
        jmp     LF5C4

; --- walk done: close helmet, return to hiding ---

code_97FB:  lda     #$1C                ; OAM $1C = helmet closing anim
        jsr     LF835                   ; reset_sprite_anim
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
        lda     L9881,y                 ; load random delay
        sta     ent_timer,x                 ; set hide timer
        rts

; --- met_fire_3_bullets: spawn 3 projectiles ---

code_981D:  stx     L0000               ; save Met slot
        lda     #$02                    ; bullet counter = 3 (indexes 2,1,0)
        sta     $01
code_9823:  jsr     LFC53               ; find free enemy slot
        bcs     code_9872               ; none → done
        ldx     $01                     ; set bullet speeds from table
        lda     L9875,x                 ; X speed sub (3 entries)
        sta     ent_xvel_sub,y
        lda     L9878,x                 ; X speed whole
        sta     ent_xvel,y
        lda     L987B,x                 ; Y speed sub
        sta     ent_yvel_sub,y
        lda     L987E,x                 ; Y speed whole
        sta     ent_yvel,y
        lda     #$73                    ; OAM $73 = Met bullet
        jsr     LF846                   ; init_child_entity
        lda     #$8B                    ; dmg = $8B (hurts player only)
        sta     ent_hitbox,y
        ldx     L0000                   ; restore Met slot to X
        lda     #$0F                    ; AI routine = $0F (simple projectile)
        sta     ent_routine,y
        lda     ent_x_px,x                 ; copy Met position to bullet
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; bullet Y = Met Y + 4
        clc
        adc     #$04
        sta     ent_y_px,y
        lda     ent_facing,x                 ; copy Met facing to bullet
        sta     ent_facing,y
        dec     $01                     ; loop for all 3 bullets
        bpl     code_9823
code_9872:  .byte   $A6,$00,$60         ; restore X

; Met bullet speeds: X.sub={$FB,$33,$FB}, X.whole={$00,$01,$00},
; Y.sub={$50,$00,$B0}, Y.whole={$00,$00,$FF}
; bullet 0: slow right+down, bullet 1: fast right, bullet 2: slow right+up
L9875:  .byte   $FB,$33,$FB
L9878:  .byte   $00,$01,$00
L987B:  .byte   $50,$00,$B0
L987E:  .byte   $FF,$00,$00
L9881:  asl     L963C,x
        .byte   $3C

; ===========================================================================
; main_pole — Pole (climbing pole enemy, Spark Man stage)
; ===========================================================================
; Moves vertically at set speed, walks horizontally (via code_1C9776).
; Despawns when Y screen changes (goes offscreen).
main_pole:
        lda     #$00                    ; sign extend Y speed for 24-bit add
        sta     L0000
        lda     ent_yvel,x                 ; if Y speed negative, sign = $FF
        bpl     code_9890
        dec     L0000
code_9890:  lda     ent_y_sub,x             ; apply Y speed: Y.sub += Yspd.sub
        clc
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x                 ; Y += Yspd.whole
        adc     ent_yvel,x
        sta     ent_y_px,x
        lda     ent_y_scr,x                 ; Y.screen += sign
        adc     L0000
        bne     code_98AD               ; offscreen → despawn
        jmp     code_9776               ; on-screen → walk horizontally

code_98AD:  lda     #$00                ; despawn
        sta     ent_status,x
        rts

; ===========================================================================
; main_cannon — Cannon (stationary turret, fires at player)
; ===========================================================================
; Cycle: idle (invulnerable, timer ent_timer) → detect player within $51 distance
; → open ($C9 = vulnerable) → fire 2 shells (at anim frames $09 and $12)
; → close ($A9 = invulnerable) → random idle delay. Fires distance-scaled shots.
main_cannon:

        lda     ent_timer,x                 ; idle timer active?
        beq     code_98BD               ; zero → check for player
        dec     ent_timer,x                 ; decrement; still idling?
        bne     code_98D0               ; → freeze anim + return
code_98BD:  lda     ent_anim_state,x             ; anim seq frame == 0?
        bne     code_98DB               ; no → animating, continue
        lda     ent_anim_frame,x                 ; frame timer == 1? (ready to peek)
        cmp     #$01
        bne     code_98DB               ; no → continue
        jsr     LF8C2                   ; player within $51 distance?
        cmp     #$51
        bcc     code_98D6               ; yes → open up
code_98D0:  lda     #$00                ; freeze anim (stay closed)
        sta     ent_anim_frame,x
        rts

code_98D6:  lda     #$C9                ; become vulnerable ($C9)
        sta     ent_hitbox,x
code_98DB:  jsr     LF869               ; track player direction
        jsr     LF883                   ; flip sprite
        lda     ent_anim_state,x                 ; anim fully done? (both zero)
        ora     ent_anim_frame,x
        bne     code_98FE               ; still animating → check fire

; --- anim complete: close cannon, set idle delay ---
        lda     $E4                     ; pseudo-random idle delay
        adc     $E7                     ; LFSR seed mix
        sta     $E7
        and     #$01                    ; 2 possible delays
        tay
        lda     L9995,y
        sta     ent_timer,x                 ; set idle timer
        lda     #$A9                    ; close: become invulnerable ($A9)
        sta     ent_hitbox,x
code_98FD:  rts

; --- during open anim: fire at specific frames ---

code_98FE:  lda     ent_anim_frame,x             ; frame timer must be 0 (exact moment)
        bne     code_98FD               ; not zero → return
        lda     ent_anim_state,x                 ; fire at anim frame $09 or $12
        cmp     #$09                    ; (two shots per open cycle)
        beq     code_990F
        cmp     #$12
        beq     code_990F
        rts

; --- spawn_cannon_shell: fires distance-scaled projectile ---

code_990F:  jsr     LFC53               ; find free slot
        bcs     code_98FD               ; none → return
        lda     #$00                    ; shell Y speed = $04.00 (4.0 px/f up)
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$6F                    ; OAM $6F = cannon shell
        jsr     LF846                   ; init_child_entity
        lda     #$1E                    ; play shot sound
        jsr     LF89A                   ; submit_sound_ID
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
        pha
        jsr     LF8C2                   ; get distance for speed scaling
        stx     L0000                   ; save cannon slot
        ldx     #$03                    ; find speed bracket from distance table
code_995B:  cmp     L9989,x             ; (farther = slower X speed)
        bcc     code_9963
        dex
        bne     code_995B
code_9963:  lda     L998D,x             ; set shell X speed from bracket
        sta     ent_xvel_sub,y                 ; X speed sub
        lda     L9991,x                 ; X speed whole
        sta     ent_xvel,y
        pla                             ; offset shell X based on facing
        and     #$02                    ; (facing left: bit 1 set → index 2)
        tax
        lda     ent_x_px,y                 ; shell X += offset
        clc                             ; right: +$0C, left: -$0C
        adc     L9997,x
        sta     ent_x_px,y
        lda     ent_x_scr,y                 ; with screen carry
        adc     L9998,x
        sta     ent_x_scr,y
        .byte   $A6,$00,$60             ; restore cannon slot

; distance thresholds: $4C, $3D, $2E, $1F (close→far)
; X speed sub: $00, $80, $00, $80 | X speed whole: $02, $01, $01, $00
; idle delays: $3C, $78 | X offsets: right=$0C/$00, left=$F4/$FF
L9989:  .byte   $4C,$3D,$2E,$1F
L998D:  .byte   $00,$80,$00,$80
L9991:  .byte   $02,$01,$01,$00
L9995:  .byte   $3C,$78
L9997:  .byte   $0C
L9998:  brk
        .byte   $F4
        .byte   $FF

; --- cannon shell AI: $99 + walk, explode on landing/wall hit ---
        ldy     #$08                    ; apply $99; C=1 if landed
        jsr     LF67C                   ; move_vertical_gravity
        bcs     code_99B9               ; landed → explode
        lda     ent_facing,x                 ; walk horizontally with collision
        and     #$02
        beq     code_99B1
        ldy     #$07
        jsr     LF5C4                   ; move_left_collide
        jmp     code_99B6

code_99B1:  ldy     #$08
        jsr     LF580                   ; move_right_collide
code_99B6:  bcs     code_99B9           ; hit wall → explode
        rts

code_99B9:  lda     #$00                ; become generic explosion
        sta     ent_routine,x                 ; (routine $00)
        lda     #$71                    ; OAM $71 = small explosion
        jmp     LF835                   ; reset_sprite_anim

; ===========================================================================
; main_metall_dx — Metall DX (walking Met variant)
; ===========================================================================
; State 0: hiding, opens when player within $61 distance. Anim frame 5 →
;   launch up (Y speed $02.00). Flies up until within $49 Y of player.
; State 1: fly past player, fire 3 bullets when crossing. State 2: descend.
; State 3: walk horizontally (shared code_1C9776).
main_metall_dx:

        lda     ent_status,x                 ; state machine dispatch
        and     #$0F
        cmp     #$01                    ; state 1: fly past player
        beq     code_9A1E
        cmp     #$02                    ; state 2: descend
        beq     code_9A32
        cmp     #$03                    ; state 3: walk horizontally
        bne     code_99D7
        jmp     code_9776               ; → walk in facing direction

; --- state 0: hiding / opening / ascending ---

code_99D7:  jsr     LF869               ; track player
        jsr     LF883                   ; set_sprite_hflip
        lda     ent_anim_id,x                 ; OAM $1F = ascending (propeller)?
        cmp     #$1F
        beq     code_9A0E               ; yes → fly up logic
        lda     ent_anim_state,x                 ; anim in progress?
        bne     code_99FB               ; yes → check frame
        jsr     LF8C2                   ; player within $61?
        cmp     #$61
        bcs     code_9A18               ; too far → stay hidden
        lda     #$C3                    ; become vulnerable
        sta     ent_hitbox,x
        inc     ent_anim_state,x                 ; advance anim frame
        lda     ent_anim_state,x
code_99FB:  cmp     #$05                ; anim frame 5? (fully opened)
        bne     code_9A1D               ; not yet → return
        lda     #$00                    ; set upward velocity $02.00
        sta     ent_yvel_sub,x
        lda     #$02
        sta     ent_yvel,x
        lda     #$1F                    ; OAM $1F = ascending
        jmp     LF835                   ; reset_sprite_anim

code_9A0E:  jsr     LF8B3               ; within $49 Y of player?
        cmp     #$49
        bcs     code_9A4B               ; yes → next state
        jmp     LF779                   ; keep flying up

code_9A18:  lda     #$00                ; freeze anim (stay hidden)
        sta     ent_anim_frame,x
code_9A1D:  rts

; --- state 1: fly past player, fire when crossing ---

code_9A1E:  jsr     LF8C2               ; get X distance (sets carry)
        lda     ent_facing,x                 ; check if passed player
        and     #$02                    ; facing left + player behind → fire
        beq     code_9A2D
        bcs     code_9A4F               ; C=1: player left of us → fire
        jmp     code_9776               ; keep flying

code_9A2D:  bcc     code_9A4F           ; C=0: player right → fire
        jmp     code_9776               ; keep flying

; --- state 2: descend to player altitude ---

code_9A32:  jsr     LF869               ; track player
        jsr     LF883                   ; set_sprite_hflip
        lda     ent_timer,x                 ; post-fire delay timer
        beq     code_9A41               ; zero → descend
        dec     ent_timer,x                 ; wait
        rts

code_9A41:  jsr     LF8B3               ; within 4 Y of player?
        cmp     #$04
        bcc     code_9A4B               ; yes → next state
        jmp     LF759                   ; keep descending

code_9A4B:  inc     ent_status,x             ; advance to next state
        rts

code_9A4F:  stx     L0000
        lda     #$02
        sta     $01
code_9A55:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_9AA1
        ldx     $01
        lda     L9AAC,x
        sta     ent_xvel_sub,y
        lda     L9AAF,x
        sta     ent_xvel,y
        lda     L9AB2,x
        sta     ent_yvel_sub,y
        lda     L9AB5,x
        sta     ent_yvel,y
        lda     L9AB8,x
        sta     ent_facing,y
        lda     #$73
        jsr     LF846                   ; init_child_entity
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
        dec     $01
        bpl     code_9A55
code_9AA1:  ldx     L0000
        inc     ent_status,x
        lda     #$3C
        .byte   $9D,$00,$05,$60
L9AAC:  .byte   $DB,$00,$DB
L9AAF:  .byte   $00,$00,$00
L9AB2:  .byte   $DB,$33,$DB
L9AB5:  .byte   $00,$01
        brk
L9AB8:  .byte   $02
        ora     ($01,x)

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
        beq     L9AC8
        jsr     LF71D                   ; move_sprite_right
        jmp     L9ACB

L9AC8:  jsr     LF73B                   ; move_sprite_left
L9ACB:  jsr     LF8B3                   ; check player proximity
        bcc     L9B2B                   ; no overlap → check dismount
        jsr     LF8C2                   ; detailed collision check
        cmp     #$10                    ; too far away?
        bcs     L9B2B
        lda     player_state                     ; only mount if state < $02
        cmp     #PSTATE_SLIDE                    ; (on_ground or airborne)
        bcs     L9B08
        lda     #PSTATE_ENTITY_RIDE                    ; state → $05 (entity_ride)
        sta     player_state
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
        bpl     code_9B43               ; if falling down, done
        lda     #$55                    ; if moving up, zero out velocity
        sta     ent_yvel_sub                   ; ent_yvel_sub/ent_yvel = $55/$00
        lda     #$00                    ; ($99 baseline, not moving)
        sta     ent_yvel
        rts

L9B08:  lda     player_state
        cmp     #PSTATE_ENTITY_RIDE                    ; if not in entity_ride, skip
        bne     code_9B43
        cpx     entity_ride_slot                     ; if riding different entity, skip
        bne     code_9B43
        lda     ent_timer,x                 ; check Y distance from mount point
        sec
        sbc     ent_y_px
        cmp     #$20                    ; if player drifted > 32px away,
        bcc     code_9B43               ; stay mounted
        lda     #$00                    ; dismount: clear player velocity
        sta     ent_yvel_sub
        sta     ent_yvel
        lda     ent_facing,x                 ; $35 = Mag Fly's direction
        sta     facing_sub                     ; (player inherits movement dir)
        rts

L9B2B:  lda     player_state                     ; if player is riding ($05)
        cmp     #PSTATE_ENTITY_RIDE
        bne     code_9B43               ; and it's THIS entity
        cpx     entity_ride_slot
        bne     code_9B43
        lda     #$AB                    ; set fall velocity
        sta     ent_yvel_sub
        lda     #$FF
        sta     ent_yvel
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; dismount complete
code_9B43:  rts

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
        bne     code_9B58               ; skip if already active
        jsr     LF8C2                   ; check horizontal distance to player
        cmp     #$76                    ; if >= $76 px away,
        bcs     code_9B43               ; stay idle (returns via RTS above)
        inc     ent_status,x                 ; activate: advance to state 1 (falling)
        jsr     LF883                   ; face toward player
code_9B58:  lda     ent_flags,x             ; clear sprite flags bit 2 if set
        and     #$04                    ; (prevents unwanted vertical flip
        beq     code_9B67               ; from collision or child spawn)
        lda     ent_flags,x
        eor     #$04
        sta     ent_flags,x
code_9B67:  lda     ent_status,x             ; check if state >= 2 (grounded)
        and     #$02
        bne     code_9B78               ; if grounded, skip $99
        ldy     #$24                    ; state 1: apply $99, Y hitbox offset $24
        jsr     LF67C                   ; move down with collision
        bcc     code_9BE1               ; C=0: still airborne, done
        inc     ent_status,x                 ; C=1: landed, advance to state 2
code_9B78:  lda     ent_facing,x             ; save old direction
        pha
        jsr     LF869                   ; update facing toward player
        pla                             ; compare old vs new direction
        cmp     ent_facing,x
        beq     code_9B8D               ; no change, skip flip
        lda     ent_flags,x                 ; direction changed: toggle sprite
        eor     #$40                    ; horizontal flip (bit 6)
        sta     ent_flags,x
code_9B8D:  lda     ent_var1,x             ; throw cooldown timer
        bne     code_9BA6               ; non-zero: skip spawning
        jsr     code_9BE2               ; spawn junk block child entity
        sty     L0000                   ; save child slot index
        lda     L0000
        sta     ent_timer,x                 ; ent_timer = child slot (to track Y)
        lda     #$78                    ; reset throw cooldown = $78 (120 frames)
        sta     ent_var1,x
        lda     #$00                    ; clear throw-anim flag
        sta     ent_var2,x
code_9BA6:  lda     ent_var2,x             ; if throw anim already started,
        bne     code_9BD1               ; skip to countdown
        lda     ent_timer,x                 ; Y = child slot index
        tay
        lda     ent_y_px,y                 ; child Y position
        sec
        sbc     ent_y_px,x                 ; minus golem Y position
        bcs     code_9BBD               ; if negative,
        eor     #$FF                    ; take absolute value
        adc     #$01
        clc
code_9BBD:  cmp     #$30                ; if |Y dist| >= $30, block still far
        bcs     code_9BE1               ; done (block still falling from top)
        lda     ent_anim_id,x                 ; if already using throw OAM ($39),
        cmp     #$39                    ; skip animation reset
        beq     code_9BD1
        lda     #$39                    ; switch to throwing animation (OAM $39)
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_var2,x                 ; set throw-anim flag = 1
        rts

code_9BD1:  dec     ent_var1,x             ; decrement throw cooldown timer
        lda     ent_anim_frame,x                 ; check if throw animation finished
        ora     ent_anim_state,x                 ; (anim timer=0 AND frame=0)
        bne     code_9BE1               ; not done yet, skip
        lda     #$38                    ; revert to idle animation (OAM $38)
        jsr     LF835                   ; reset_sprite_anim
code_9BE1:  rts

; --- spawn_junk_block: create thrown junk block child entity ---

code_9BE2:  jsr     LFC53               ; find free enemy slot
        bcs     code_9C18               ; none available, return
        lda     ent_facing,x                 ; copy parent direction to child
        sta     ent_facing,y
        lda     ent_x_px,x                 ; copy parent X position to child
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     #$04                    ; child Y = $04 (near top of screen)
        sta     ent_y_px,y                 ; block spawns high and falls down
        lda     ent_y_px,x                 ; ent_timer,y = golem's Y position
        sta     ent_timer,y                 ; (target Y for homing transition)
        lda     #$94                    ; entity type $94 (junk block)
        jsr     LF846                   ; init_child_entity
        lda     #$CA                    ; damage flags $CA: hurts player + takes damage
        sta     ent_hitbox,y
        lda     #$24                    ; AI routine = $24 (main_unknown_24)
        sta     ent_routine,y
        lda     #$08                    ; HP = 8
        sta     ent_hp,y
code_9C18:  rts

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
        bne     code_9C2B               ; skip if already initialized
        sta     ent_yvel_sub,x                 ; Y speed = $04.00 (4 px/frame down)
        lda     #$04                    ; (A=0 from AND above, sub=0)
        sta     ent_yvel,x
        inc     ent_status,x                 ; advance to state 1
code_9C2B:  jsr     LF759               ; move downward (no collision)
        lda     ent_y_px,x                 ; current Y position
        sec
        sbc     ent_timer,x                 ; minus target Y (golem's Y)
        bcs     code_9C3C               ; if negative,
        eor     #$FF                    ; take absolute value
        adc     #$01
        clc
code_9C3C:  cmp     #$20                ; if |Y dist to target| >= $20,
        bcs     code_9C55               ; still falling, done
        lda     #$80                    ; homing speed = $04.80 (4.5 px/frame)
        sta     $02
        lda     #$04
        sta     $03
        jsr     LFC63                   ; compute X/Y velocity toward player
        lda     $0C                     ; set direction from homing result
        sta     ent_facing,x
        lda     #$0B                    ; switch to routine $0B (generic homing)
        sta     ent_routine,x                 ; block now flies toward player
code_9C55:  rts

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
        and     #$0F
        bne     code_9C73               ; skip if already initialized
        sta     ent_yvel_sub,x                 ; Y speed = $04.00 ($99 fall)
        lda     #$04
        sta     ent_yvel,x
        jsr     code_9D20               ; get random drive count ($10/$20/$30)
        sta     ent_timer,x                 ; store as drive step counter
        lda     #$1E                    ; stop timer = $1E (30 frames)
        sta     ent_var1,x
        inc     ent_status,x                 ; advance to state 1
        ; --- rider weapon collision check (Y-$17 offset for rider hitbox) ---
code_9C73:  lda     ent_y_px,x             ; save real Y position
        pha
        lda     ent_y_px,x
        sec
        sbc     #$17                        ; offset Y up by 23 px for rider
        sta     ent_y_px,x
        lda     #$C3                        ; rider hitbox (vulnerable)
        sta     ent_hitbox,x
        jsr     code_8003                   ; check_weapon_hit
        pla
        sta     ent_y_px,x                  ; restore real Y
        lda     ent_hp,x                    ; rider killed?
        beq     code_9CA5                   ; yes -> rts
        lda     #$AC                        ; bull hitbox (body)
        sta     ent_hitbox,x
        lda     ent_status,x
        and     #$02                        ; state 2 = stopped?
        bne     code_9CD6                   ; yes -> oscillation logic
        dec     ent_timer,x                 ; decrement drive counter
        bne     code_9CA6                   ; still driving
        inc     ent_status,x               ; drive count 0 -> stop state
code_9CA5:  rts

        ; --- state 1: driving with gravity + wall check ---
code_9CA6:  ldy     #$2A
        jsr     LF606                       ; move_down_collide (gravity)
        lda     ent_facing,x
        and     #$01                        ; facing right?
        beq     code_9CC0                   ; no -> check left
        lda     $42                         ; tile to left
        and     #$10                        ; solid?
        beq     code_9CCD                   ; no solid -> reverse
        ldy     #$10
        jsr     LF580                       ; move_right_collide
        jmp     code_9CCB

code_9CC0:  lda     tile_at_feet_hi
        and     #$10
        beq     code_9CCD
        ldy     #$11
        jsr     LF5C4                   ; move_left_collide
code_9CCB:  bcc     code_9CD5
code_9CCD:  lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_9CD5:  rts

        ; --- state 2: stopped, rider oscillates left/right ---
code_9CD6:  dec     ent_var1,x              ; decrement stop timer
        bne     code_9CF0                   ; still stopped
        sta     ent_var2,x                  ; reset oscillation delay
        sta     ent_var3,x                  ; reset oscillation counter
        lda     #$1E
        sta     ent_var1,x                  ; new stop timer = 30 frames
        jsr     code_9D20                   ; random drive count
        sta     ent_timer,x                 ; set new drive step counter
        dec     ent_status,x               ; -> back to driving state
        rts

        ; --- oscillation: move rider 1px left/right alternating ---
code_9CF0:  lda     ent_var2,x              ; oscillation delay
        bne     code_9D18                   ; delay active -> decrement
        lda     ent_var3,x                  ; oscillation counter
        and     #$01                        ; even=right, odd=left
        asl     a
        tay
        lda     ent_x_px,x
        clc
        adc     L9D1C,y                     ; +1 or -1 pixel
        sta     ent_x_px,x
        lda     ent_x_scr,x
        adc     L9D1D,y
        sta     ent_x_scr,x
        lda     #$02
        sta     ent_var2,x                  ; 2-frame delay between oscillations
        inc     ent_var3,x
        rts

code_9D18:  .byte   $DE,$40,$05,$60         ; dec ent_var2,x; rts
L9D1C:  .byte   $01                         ; oscillation X offsets: +1, 0, -1, -1
L9D1D:  brk
        .byte   $FF
        .byte   $FF
; --- random drive count from table ($10/$20/$30/$10) ---
code_9D20:  lda     $E4                     ; pseudo-random: add two RNG bytes
        adc     $E5
        sta     $E4
        and     #$03
        tay
        .byte   $B9,$2D,$9D,$60
        bpl     L9D4F
        bmi     L9D41
; ---------------------------------------------------------------------------
; main_bikky -- Bikky (stomping enemy, entity type $21)
; ---------------------------------------------------------------------------
; Applies gravity, walks horizontally with collision.
; On landing (anim_state=$08, frame=0): launch upward (Y speed $05.A8),
;   face player, hitbox $C5 (dangerous). Sound $20 on subsequent landing.
; While airborne: hitbox $A5 (safe).
; ---------------------------------------------------------------------------
main_bikky:
        jsr     LF883                       ; set_sprite_hflip
        ldy     #$10
        jsr     LF67C                       ; move_vertical_gravity
        bcs     code_9D51                   ; landed -> check anim state
        lda     #$00
        sta     ent_anim_frame,x            ; reset anim frame while airborne
        .byte   $BD
L9D41:  ldy     #$04                        ; (encoded lda ent_facing,x)
        and     #$01
        beq     code_9D4C                   ; facing left
        ldy     #$0E
        jmp     LF580                       ; move_right_collide

code_9D4C:  ldy     #$0F
        .byte   $4C
L9D4F:  cpy     prg_bank                    ; (encoded jmp move_left_collide)
        ; --- landed: check animation state ---
code_9D51:  lda     ent_anim_state,x
        cmp     #$08                        ; stomp anim finished?
        bne     code_9D7E                   ; no -> safe hitbox
        lda     ent_anim_frame,x
        beq     code_9D6B                   ; frame 0 -> launch upward
        lda     #$00                        ; frame > 0 -> reset anim, play sound
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        lda     #$20
        jsr     LF89A                       ; submit_sound_ID (stomp)
        rts

code_9D6B:  lda     #$A8                    ; launch upward: Y speed = $05.A8
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        jsr     LF869                       ; face_player
        lda     #$C5
        sta     ent_hitbox,x                ; dangerous hitbox (landing)
        rts

code_9D7E:  lda     #$A5                    ; safe hitbox (airborne)
        sta     ent_hitbox,x
        rts
; ---------------------------------------------------------------------------
; main_magnet_force -- Magnet Force (horizontal pull/push, type $25)
; ---------------------------------------------------------------------------
; Applies horizontal force on player when within range (Y<$1C, X<$68).
; Uses entity flag bit 6 for direction. Sets $36/$37/$38 for pull effect.
; ---------------------------------------------------------------------------
main_magnet_force:

        jsr     LF8B3                       ; entity_y_dist_to_player
        cmp     #$1C                        ; within 28 px vertically?
        bcs     code_9DB3                   ; no -> rts
        jsr     LF8C2                       ; entity_x_dist_to_player
        ror     L0000                       ; carry = direction -> $00 bit 7
        cmp     #$68                        ; within 104 px horizontally?
        bcs     code_9DB3                   ; no -> rts
        lda     ent_flags,x
        and     #$40                        ; entity facing (bit 6)
        bne     code_9DA3                   ; facing left -> branch
        lda     L0000                       ; facing right: player to right?
        bmi     code_9DB3                   ; no -> no force
        lda     #$01                        ; pull direction = right
        bne     code_9DA9
code_9DA3:  lda     L0000                   ; facing left: player to left?
        bpl     code_9DB3                   ; no -> no force
        lda     #$02                        ; pull direction = left
code_9DA9:  sta     $36                     ; set magnet pull direction
        lda     #$00
        sta     $37                         ; pull sub-speed = 0
        lda     #$01
        sta     $38                         ; pull whole-speed = 1
code_9DB3:  rts
; ---------------------------------------------------------------------------
; main_new_shotman -- New Shotman (shooting enemy, entity type $05)
; ---------------------------------------------------------------------------
; State 0: init timer=$1E.
; State 1: if player within $50 px, shoot anim $5A, spawn falling projectile.
;   After 2 shots, walk state (ent_var2=$78 timer).
; State 2: walk timer, return to shoot. Fires horizontal bullets every $1E
;   frames, 3 per burst, then $5A frame cooldown.
; code_9EA9: spawn falling projectile (type $73, Y speed $04.00, routine $0C).
; code_9E46: spawn horizontal bullet pair (speed $01.80, type $73, routine $1B).
; ---------------------------------------------------------------------------
main_new_shotman:

        lda     ent_status,x
        and     #$0F
        bne     code_9DC3                   ; skip init
        lda     #$1E
        sta     ent_timer,x                 ; fire timer = 30 frames
        inc     ent_status,x                ; -> state 1
code_9DC3:  lda     ent_status,x
        and     #$02                        ; bit 1 = walking state?
        bne     code_9E04                   ; yes -> walk countdown
        jsr     LF8C2                       ; entity_x_dist_to_player
        cmp     #$50                        ; within 80 px?
        bcs     code_9E0C                   ; no -> fire timer only
        lda     ent_var2,x                  ; shot cooldown active?
        bne     code_9DE7                   ; yes -> count down
        lda     #$5A                        ; shooting animation
        jsr     LF835                       ; reset_sprite_anim
        jsr     LF869                       ; face_player
        jsr     code_9EA9                   ; spawn falling projectile
        lda     #$1E
        sta     ent_var2,x                  ; shot cooldown = 30 frames
        rts

code_9DE7:  dec     ent_var2,x              ; decrement shot cooldown
        bne     code_9E0C
        inc     ent_var3,x                  ; shot count++
        lda     ent_var3,x
        cmp     #$02                        ; fired 2 shots?
        bcc     code_9E0C
        lda     #$00
        sta     ent_var3,x
        lda     #$78
        sta     ent_var2,x
        inc     ent_status,x
        rts

        ; --- walk state ---
code_9E04:  dec     ent_var2,x              ; walk timer--
        bne     code_9E0C
        dec     ent_status,x               ; -> back to shoot state
        ; --- horizontal bullet fire timer ---
code_9E0C:  dec     ent_timer,x
        bne     code_9E31                   ; timer not expired
        lda     #$00
        sta     $01                         ; bullet pair counter
        jsr     code_9E46                   ; spawn horizontal bullet pair
        lda     #$1E
        sta     ent_timer,x                 ; reset fire timer
        inc     ent_var1,x                  ; burst count++
        lda     ent_var1,x
        cmp     #$03                        ; 3 bursts in a row?
        bcc     code_9E31                   ; no -> keep firing
        lda     #$5A
        sta     ent_timer,x                 ; long cooldown = 90 frames
        lda     #$00
        sta     ent_var1,x                  ; reset burst count
code_9E31:  lda     ent_anim_id,x           ; if still in shoot anim ($5A)
        cmp     #$5A
        bne     code_9E45
        lda     ent_anim_frame,x            ; and anim done...
        ora     ent_anim_state,x
        bne     code_9E45
        lda     #$59                        ; ...return to idle anim
        jsr     LF835                       ; reset_sprite_anim
code_9E45:  rts

; --- new shotman: spawn horizontal bullet pair (speed $01.80, type $73) ---
; Fires two bullets by flipping facing between iterations ($01 counter).
code_9E46:  jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_9EA4                   ; no slot -> rts
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     L9EA5,y
        pha
        lda     ent_x_scr,x
        adc     L9EA6,y
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
        jsr     LF846                   ; init_child_entity
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
code_9EA4:  .byte   $60
L9EA5:  .byte   $0F
L9EA6:  brk
        sbc     (ppu_ctrl_shadow),y
; --- new shotman: spawn falling projectile (type $73, Y speed $04.00) ---
code_9EA9:  jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_9EA4                   ; no slot -> rts
        lda     #$00                        ; Y speed = $04.00 (falling)
        sta     ent_yvel_sub,y
        lda     #$04
        sta     ent_yvel,y
        lda     #$73                        ; entity type $73 (projectile)
        jsr     LF846                       ; init_child_entity
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
        jsr     LF8C2                       ; entity_x_dist_to_player
        stx     L0000
        ldx     #$03                        ; scan distance brackets
code_9EEF:  cmp     L9F06,x                 ; distance < threshold?
        bcc     code_9EF7                   ; yes -> use this speed
        dex
        bne     code_9EEF
code_9EF7:  lda     L9F0A,x                 ; X speed sub from table
        sta     ent_xvel_sub,y
        lda     L9F0E,x                    ; X speed whole from table
        sta     ent_xvel,y
        .byte   $A6,$00,$60
; X speed lookup by distance bracket: $4C/$3D/$2E/$1F thresholds
L9F06:  .byte   $4C,$3D,$2E,$1F             ; distance thresholds
L9F0A:  .byte   $00,$80,$00,$80             ; X speed sub values
L9F0E:  .byte   $02                         ; X speed whole: $02, $01, $01, $00
        ora     ($01,x)
        brk
; --- generic projectile AI: gravity + walk, used by new_shotman bullets ---
        ldy     #$12
        jsr     LF67C                   ; move_vertical_gravity
        bcs     code_9F30
        lda     ent_facing,x
        and     #$01
        beq     code_9F28
        ldy     #$1E
        jsr     LF580                   ; move_right_collide
        jmp     code_9F2D

code_9F28:  ldy     #$1F
        jsr     LF5C4                   ; move_left_collide
code_9F2D:  bcs     code_9F30
        rts

code_9F30:  lda     ent_routine,x
        cmp     #$0C
        bne     code_9F6C
        lda     #$00
        sta     ent_routine,x
        lda     $B3
        bpl     code_9F44
        lda     #$59
        bne     code_9F46
code_9F44:  lda     #$71
code_9F46:  jmp     LF835               ; reset_sprite_anim

        lda     ent_facing,x
        and     #$01
        beq     code_9F58
        ldy     #$0C
        jsr     LF580                   ; move_right_collide
        jmp     code_9F5D

code_9F58:  ldy     #$0D
        jsr     LF5C4                   ; move_left_collide
code_9F5D:  bcs     code_9F6C
        lda     ent_facing,x
        and     #$08
        beq     code_9F69
        jmp     LF779                   ; move_sprite_up

code_9F69:  jmp     LF759               ; move_sprite_down

code_9F6C:  lda     #$00
        sta     ent_status,x
        sta     $01
        lda     #$FF
        sta     ent_spawn_id,x
code_9F78:
        jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_9FE2
        sty     L0000
        lda     ent_facing,x
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     LA731,y
        pha
        lda     ent_x_scr,x
        adc     LA732,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$5B
        jsr     LF846                   ; init_child_entity
        lda     #$0C
        sta     ent_routine,y
        lda     #$8B
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_hp,y
        stx     $02
        ldx     $01
        lda     L9FE3,x
        sta     ent_facing,y
        lda     L9FE7,x
        sta     ent_yvel_sub,y
        lda     L9FEB,x
        sta     ent_yvel,y
        lda     L9FEF,x
        sta     ent_xvel_sub,y
        lda     L9FF3,x
        sta     ent_xvel,y
        ldx     $02
        inc     $01
        lda     $01
        cmp     #$04
        .byte   $90,$96
code_9FE2:  .byte   $60
L9FE3:  .byte   $01,$01,$02,$02
L9FE7:  .byte   $9E,$44,$9E,$44
L9FEB:  .byte   $04,$03,$04,$03
L9FEF:  .byte   $CC,$80,$CC,$80
L9FF3:  brk
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
        beq     code_9FE2
        .byte   $BD

; ===========================================================================
; BANK $1D — Entity AI routines (continued)
; ===========================================================================
; This bank continues main_proto_man from bank $1C. The .byte $BD at end of
; bank $1C + .byte $E0,$04 here form a cross-bank LDA $04E0,x instruction.
; ===========================================================================

.segment "BANK1D"

LA000:  .byte   $E0,$04                 ; cross-bank: LDA ent_hp,x (opcode $BD in bank $1C)
        bne     code_A007               ; HP > 0 → alive, continue AI
        jmp     code_A180               ; HP = 0 → defeated sequence

; --- Proto Man alive: state machine ---
code_A007:  lda     ent_status,x            ; extract sub-state
        and     #$0F
        bne     code_A032               ; nonzero → already initialized
        sta     ent_anim_frame,x        ; state 0: reset anim frame
        lda     ent_y_px,x
        cmp     #$90                    ; above Y=$90?
        bcs     code_A01B               ; no → apply gravity+collision
        jmp     LF797                   ; yes → apply Y speed (falling)

code_A01B:  ldy     #$00
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_A05F               ; no floor hit → walking logic
        lda     ent_routine,x           ; routine $52 or $53
        sec
        sbc     #$52                    ; index 0 or 1 into tables
        tay
        lda     LA176,y                 ; timer variant from table
        sta     ent_timer,x
        inc     ent_status,x            ; advance to state 1
code_A032:  lda     ent_anim_id,x
        cmp     #$99                    ; teleport beam sprite?
        bne     code_A04A
        lda     ent_anim_state,x
        cmp     #$04                    ; anim complete?
        bne     code_A05F
        lda     ent_timer,x             ; load variant index
        tya
        lda     LA178,y                 ; walking anim from table
        jsr     LF835                   ; reset_sprite_anim
code_A04A:  lda     ent_status,x
        and     #$02                    ; state >= 2?
        beq     code_A054
        jmp     code_A0D3               ; → attacking phase

code_A054:  jsr     LF8C2               ; entity_x_dist_to_player
        cmp     #$60                    ; within 96 px?
        bcs     code_A05F               ; no → keep walking
        inc     ent_status,x            ; yes → advance to attack
        rts

; --- walking phase: gravity + horizontal movement ---
code_A05F:  ldy     #$00
        jsr     LF67C                   ; move_vertical_gravity
        rol     $0F                     ; save carry (landed) into $0F bit 0
        lda     ent_facing,x
        and     #$01
        beq     code_A075
        ldy     #$00
        jsr     LF580                   ; move_right_collide
        jmp     code_A07A

code_A075:  ldy     #$01
        jsr     LF5C4                   ; move_left_collide
code_A07A:  lda     $0F
        and     #$01                    ; check if landed on floor
        beq     code_A0D2               ; airborne → skip wall checks
        lda     ent_facing,x
        and     #$01
        beq     code_A091
        lda     ent_x_px,x
        cmp     #$D6                    ; near right edge?
        bcc     code_A0A1
        jmp     code_A098               ; → reverse direction

code_A091:  lda     ent_x_px,x
        cmp     #$2A                    ; near left edge?
        bcs     code_A0A1
code_A098:  lda     ent_facing,x          ; reverse facing direction
        eor     #$03
        sta     ent_facing,x
        rts

code_A0A1:  lda     $10
        and     #$10                    ; floor collision flag?
        beq     code_A0BC               ; no floor → check walking anim
        lda     ent_timer,x             ; variant index
        tya
        lda     LA17A,y                 ; jump anim from table
        jsr     LF835                   ; reset_sprite_anim
        lda     #$A8                    ; set Y velocity for jump
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        rts

code_A0BC:  lda     ent_timer,x          ; variant index
        tay
        lda     LA17C,y                 ; walk anim from table
        cmp     ent_anim_id,x           ; already set?
        beq     code_A0D2               ; yes → skip
        lda     ent_timer,x
        tay
        lda     LA17C,y
        jsr     LF835                   ; reset_sprite_anim
code_A0D2:  rts

; --- attacking phase (state 2) ---
code_A0D3:  lda     ent_var2,x          ; attack cooldown
        bne     code_A13A               ; still cooling down → decrement
        lda     ent_facing,x
        and     #$01
        beq     code_A0E7
        ldy     #$00
        jsr     LF580                   ; move_right_collide
        jmp     code_A0EC

code_A0E7:  ldy     #$01
        jsr     LF5C4                   ; move_left_collide
code_A0EC:  lda     ent_facing,x          ; edge/wall checks same as walking
        and     #$01
        beq     code_A0FD
        lda     ent_x_px,x
        cmp     #$D6
        bcc     code_A10C
        jmp     code_A104

code_A0FD:  .byte   $BD                 ; cross-bank LDA ent_x_px,x
        rts                             ; (rts = $60, part of the address)

LA0FF:  .byte   $03                     ; ($0360 = ent_x_px)
        cmp     #$2A                    ; near left edge?
        bcs     code_A10C
code_A104:  lda     ent_facing,x          ; reverse direction at edges
        eor     #$03
        sta     ent_facing,x
code_A10C:  ldy     #$00
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_A13E               ; airborne → check anim
        lda     #$04                    ; landed: set attack cooldown
        sta     ent_var2,x
        lda     ent_timer,x
        tay
        lda     LA17C,y                 ; walk anim from table
        jsr     LF835                   ; reset_sprite_anim
        lda     #$A8                    ; set jump velocity
        sta     ent_yvel_sub,x
        lda     #$05
        sta     ent_yvel,x
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$60                    ; player far away (> 96 px)?
        bcc     code_A139               ; no → stay in attack
        dec     ent_status,x            ; yes → revert to walking
        jsr     LF81B                   ; reset_gravity
code_A139:  rts

code_A13A:  dec     ent_var2,x          ; decrement attack cooldown
        rts

code_A13E:  lda     ent_yvel,x          ; airborne: check Y direction
        bpl     code_A14D               ; rising → jump anim
        lda     ent_timer,x             ; falling → use different anim
        tay
        lda     LA17A,y                 ; jump anim from table
        jmp     LF835                   ; reset_sprite_anim

code_A14D:  lda     ent_timer,x          ; falling: shooting anim
        tya
        lda     LA17E,y                 ; shoot anim from table
        cmp     ent_anim_id,x
        beq     code_A163               ; already set → skip
        lda     ent_timer,x
        tya
        lda     LA17E,y
        jsr     LF835                   ; reset_sprite_anim
code_A163:  lda     ent_anim_state,x
        cmp     #$01                    ; at frame 1?
        bne     code_A139               ; no → return
        jsr     code_A293               ; spawn projectile
        lda     #$00                    ; reset anim after firing
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        rts

; Proto Man variant data tables (indexed by routine - $52):
;   index 0 = routine $52 (normal), index 1 = routine $53 (Hard Man)
LA176:  .byte   $00,$01                 ; timer variant
LA178:  .byte   $88,$8A                 ; walking anim IDs
LA17A:  .byte   $86,$8F                 ; jump anim IDs
LA17C:  .byte   $83,$8C                 ; walk anim IDs
LA17E:  .byte   $85,$8E                 ; shoot anim IDs
; --- Proto Man defeated: fly upward off screen ---
code_A180:  lda     #$99                ; teleport beam sprite
        cmp     ent_anim_id,x
        beq     code_A1A2               ; already set → skip to accelerate
        lda     #$00
        sta     ent_hitbox,x            ; remove hitbox
        tay
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_A1E1               ; airborne → return
        lda     #$99                    ; set teleport beam sprite
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_anim_state,x
        lda     #$00                    ; clear Y velocity
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
code_A1A2:  lda     ent_anim_state,x    ; wait for anim to complete
        bne     code_A1E1
        sta     ent_anim_frame,x        ; hold on frame 0
        lda     ent_yvel_sub,x          ; accelerate upward
        clc
        adc     gravity
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        cmp     #$0C                    ; cap speed at $0C
        bne     code_A1C4
        lda     #$00
        sta     ent_yvel_sub,x
code_A1C4:  jsr     LF779               ; move_sprite_up
        lda     ent_y_scr,x
        beq     code_A1E1
        lda     #$00
        sta     ent_status,x
        lda     ent_routine,x                 ; entity routine index
        cmp     #$53                    ; $53 = Hard Man stage Proto Man
        bne     code_A1DD               ; $52 = normal → skip, set $68
        lda     #PSTATE_TELEPORT_BEAM                    ; state → $13 (teleport_beam)
        sta     player_state                     ; Proto Man defeated → player beams out
        rts

code_A1DD:  lda     #$80                ; $68 = cutscene-complete flag
        sta     proto_man_flag
code_A1E1:  rts

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
        beq     code_A1E1               ; not started yet → return
        lda     #PSTATE_STUNNED                    ; state → $0F (stunned)
        sta     player_state                     ; freeze player during cutscene
        lda     ent_status,x
        and     #$0F
        bne     code_A216
        jsr     LF797                   ; apply_y_speed
        lda     #$9C
        cmp     ent_y_px,x
        bcs     code_A243
        sta     ent_y_px,x
        lda     ent_anim_state,x
        cmp     #$04
        bne     code_A248
        lda     #$88
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_status,x
        lda     #$FF
        sta     ent_timer,x
code_A216:  lda     ent_timer,x
        beq     code_A230
        dec     ent_timer,x
        bne     code_A243
        lda     #$99
        sta     ent_anim_id,x
        inc     ent_anim_state,x
        lda     #$00
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
code_A230:  jsr     code_A1A2
        lda     proto_man_flag                     ; cutscene-complete flag
        beq     code_A248               ; not done yet → return
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; release player from stun
        ldy     #$0F                    ; clear all weapon slots
code_A23D:  sta     $0310,y
        dey
        bpl     code_A23D
code_A243:  lda     #$00
        sta     ent_anim_frame,x
code_A248:  rts

; cutscene init — freeze player and play Proto Man's whistle

cutscene_init:  lda     ent_var3,x         ; if phase already started,
        bne     code_A292               ; skip init
        lda     player_state                     ; if player already stunned,
        bne     code_A25D               ; skip to whistle
        lda     ent_anim_id                   ; player OAM ID
        cmp     #$13                    ; $13 = teleporting? skip
        beq     code_A292
        lda     #PSTATE_STUNNED                    ; state → $0F (stunned)
        sta     player_state                     ; freeze player for cutscene
code_A25D:  lda     #$11
        cmp     $D9
        beq     code_A26B
        jsr     LF898                   ; submit_sound_ID_D9
        lda     #$B4
        sta     ent_timer,x
code_A26B:  dec     ent_timer,x
        bne     code_A292
        lda     #$00                    ; state → $00 (on_ground)
        sta     player_state                     ; whistle done, release player
        inc     ent_var3,x                 ; advance to next cutscene phase
        lda     ent_flags,x
        and     #$FB                    ; clear bit 2 (disabled flag)
        sta     ent_flags,x
        lda     ent_routine,x                 ; entity routine index
        cmp     #$53                    ; $53 = Proto Man
        bne     LA28A
        lda     #$0C                    ; Proto Man stage music ($0C)
        bne     LA28F
LA28A:  lda     stage_id                     ; else play stage music
        clc                             ; (stage index + 1)
        adc     #$01
LA28F:  jsr     LF898                   ; submit_sound_ID_D9
code_A292:  rts

; --- spawn Proto Man projectile ---
code_A293:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_A2E7               ; no slot → abort
        sty     L0000                   ; save child slot
        lda     ent_facing,x            ; copy parent facing to child
        sta     ent_facing,y
        and     #$02                    ; direction offset for X spawn table
        tay
        lda     ent_x_px,x              ; child X = parent X + offset
        clc
        adc     LA2E8,y
        pha
        lda     ent_x_scr,x
        adc     LA2E9,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x              ; child Y = parent Y
        sta     ent_y_px,y
        lda     #$00
        sta     ent_hp,y                ; HP = 0 (indestructible)
        sta     ent_xvel_sub,y
        lda     #$04
        sta     ent_xvel,y              ; X speed = 4
        lda     ent_routine,x
        and     #$01                    ; routine $52 vs $53
        bne     code_A2D8
        lda     #$18                    ; routine $52: OAM $18
        bne     code_A2DA
code_A2D8:  lda     #$73                ; routine $53: OAM $73
code_A2DA:  jsr     LF846               ; init_child_entity
        lda     #$8B                    ; hitbox = projectile
        sta     ent_hitbox,y
        lda     #$1B                    ; AI routine = $1B
        sta     ent_routine,y
code_A2E7:  rts

; Proto Man projectile X spawn offsets (indexed by facing direction):
LA2E8:  .byte   $0D
LA2E9:  .byte   $00,$F3,$FF

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
        bne     code_A2FE
        jsr     LF883                   ; face toward player
        lda     #$3C                    ; hide timer = 60 frames
        sta     ent_var3,x
        inc     ent_status,x                 ; advance to state 1 (hidden)
code_A2FE:  lda     ent_status,x             ; dispatch by sub-state
        and     #$0F
        cmp     #$02                    ; state 2 = firing
        beq     code_A321
        cmp     #$03                    ; state 3 = walking
        beq     code_A389
        dec     ent_var3,x                 ; state 1: count down hide timer
        bne     code_A318
        lda     #$00                    ; timer expired: clear it
        sta     ent_var3,x
        inc     ent_status,x                 ; advance to state 2 (firing)
code_A318:  lda     #$00                ; freeze animation while hidden
        sta     ent_anim_frame,x                 ; (reset anim timer and frame)
        sta     ent_anim_state,x
        rts

; --- state 2: firing sequence (turret open) ---

code_A321:  lda     ent_timer,x             ; if firing-done flag set,
        bne     code_A36B               ; skip to post-fire pause
        lda     ent_anim_state,x                 ; check anim frame 1, tick 1:
        cmp     #$01                    ; fire first bullet spread
        bne     code_A33D
        lda     ent_anim_frame,x
        cmp     #$01
        bne     code_A33D
        lda     #$04                    ; spawn 5 bullets (index 0-4)
        sta     $01
        stx     L0000
        jsr     code_A41A
code_A33D:  lda     ent_anim_state,x             ; check anim frame 3, tick 1:
        cmp     #$03                    ; fire second bullet spread
        bne     code_A354
        lda     ent_anim_frame,x
        cmp     #$01
        bne     code_A354
        lda     #$04                    ; spawn 5 bullets (index 0-4)
        sta     $01
        stx     L0000
        jsr     code_A41A
code_A354:  lda     ent_anim_state,x             ; check anim frame 4, tick 2:
        cmp     #$04                    ; firing animation complete
        bne     code_A36A
        lda     ent_anim_frame,x
        cmp     #$02
        bne     code_A36A
        inc     ent_timer,x                 ; set firing-done flag
        lda     #$10                    ; post-fire pause = 16 frames
        sta     ent_var1,x
code_A36A:  rts

; --- post-fire pause, then transition to walking ---

code_A36B:  lda     #$00                ; freeze animation during pause
        sta     ent_anim_frame,x
        sta     ent_anim_state,x
        dec     ent_var1,x                 ; count down post-fire timer
        bne     code_A388
        inc     ent_status,x                 ; advance to state 3 (walking)
        lda     #$77                    ; set walking sprite
        jsr     LF835                   ; reset_sprite_anim
        jsr     LF869                   ; face player for walk direction
        lda     #$5A                    ; walk timer = 90 frames
        sta     ent_var2,x
code_A388:  rts

; --- state 3: walking/deployed ---

code_A389:  lda     ent_anim_id,x             ; check if using helmet sprite (OAM $76)
        cmp     #$76                    ; if so, do retract-and-fire sequence
        beq     code_A3FA
        ldy     #$0E                    ; apply $99, speed index $0E
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_A3BA               ; no floor hit: skip horizontal move
        lda     #$AA                    ; on floor: set damage flags (hurts player, takes damage)
        sta     ent_hitbox,x
        lda     ent_facing,x                 ; check facing direction
        and     #$01
        beq     code_A3AB               ; branch if facing left
        ldy     #$1C                    ; move right with collision
        jsr     LF580                   ; move_right_collide
        jmp     code_A3B0

code_A3AB:  ldy     #$1D                ; move left with collision
        jsr     LF5C4                   ; move_left_collide
code_A3B0:  bcc     code_A3BA           ; no wall hit: continue
        lda     ent_facing,x                 ; wall hit: reverse direction
        eor     #$03                    ; toggle left/right bits
        sta     ent_facing,x
code_A3BA:  lda     ent_var3,x             ; check retract phase flag
        bne     code_A3D7               ; if set, count down retract timer
        dec     ent_var2,x                 ; decrement walk timer
        bne     code_A3D6               ; not zero: keep walking
        lda     #$C6                    ; walk timer expired: switch to helmet
        sta     ent_hitbox,x                 ; damage flags = invincible in helmet
        lda     #$76                    ; set helmet closing sprite
        jsr     LF835                   ; reset_sprite_anim
        lda     #$FF                    ; retract timer = 255 frames
        sta     ent_var2,x
        inc     ent_var3,x                 ; set retract phase flag
code_A3D6:  rts

; --- retract phase: wait in helmet, then reset to state 2 ---

code_A3D7:  dec     ent_var2,x             ; count down retract timer
        bne     code_A3D6               ; not done yet: wait
        lda     #$00                    ; reset all entity-specific counters
        sta     ent_timer,x                 ; firing-done flag
        sta     ent_var1,x                 ; post-fire timer
        sta     ent_var2,x                 ; walk/retract timer
        sta     ent_var3,x                 ; retract phase flag
        lda     #$C6                    ; damage flags = shielded helmet
        sta     ent_hitbox,x
        lda     #$76                    ; set helmet sprite
        jsr     LF835                   ; reset_sprite_anim
        lda     #$82                    ; reset to state 2 (active + firing)
        sta     ent_status,x                 ; skips init, goes straight to fire
        rts

; --- helmet retract-and-fire: single shot while closing ---

code_A3FA:  lda     ent_anim_state,x             ; wait for anim frame 1, tick 2
        cmp     #$01                    ; (helmet opening frame)
        bne     code_A419
        lda     ent_anim_frame,x
        cmp     #$02
        bne     code_A419
        lda     #$04                    ; fire 5 bullets
        sta     $01
        stx     L0000
        jsr     code_A41A
        lda     #$10                    ; set post-fire pause = 16 frames
        sta     ent_var1,x
        dec     ent_status,x                 ; go back to state 2 (wait for pause)
code_A419:  rts

; --- spawn_hari_harry_bullets: loop spawns up to 5 projectiles ---
; $01 = bullet index (counts down from 4 to 0), $00 = parent slot

code_A41A:  jsr     LFC53               ; find free enemy slot
        bcs     code_A472               ; no slot: abort
        ldx     $01                     ; X = bullet index for table lookup
        lda     LA475,x                 ; set X speed sub from table
        sta     ent_xvel_sub,y
        lda     LA47A,x                 ; set X speed whole from table
        sta     ent_xvel,y
        lda     LA47F,x                 ; set Y speed sub from table
        sta     ent_yvel_sub,y
        lda     LA484,x                 ; set Y speed whole from table
        sta     ent_yvel,y
        lda     LA489,x                 ; set direction flags from table
        sta     ent_facing,y
        lda     LA48E,x                 ; init child with OAM ID from table
        jsr     LF846                   ; init_child_entity
        lda     LA493,x                 ; set sprite flags from table
        sta     ent_flags,y
        ldx     L0000                   ; restore parent slot to X
        lda     #$CB                    ; bullet damage flags (projectile)
        sta     ent_hitbox,y
        lda     #$36                    ; AI routine = $36 (bullet movement)
        sta     ent_routine,y
        lda     ent_x_px,x                 ; copy parent X position to bullet
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x                 ; copy parent Y position to bullet
        sta     ent_y_px,y
        lda     #$01                    ; set bullet HP = 1
        sta     ent_hp,y
        dec     $01                     ; loop: next bullet index
        bpl     code_A41A               ; continue until all 5 spawned
code_A472:  ldx     L0000               ; restore parent slot to X
        rts

; bullet speed/dir/OAM/sprite tables (5 entries each, indexed 0-4):
; $A475: X speed sub    $A47A: X speed whole   $A47F: Y speed sub
; $A484: Y speed whole  $A489: direction flags  $A48E: OAM ID
; $A493: sprite flags

LA475:  .byte   $00,$1F,$00,$00,$1F
LA47A:  .byte   $00,$02,$03,$03,$02
LA47F:  .byte   $00,$E1,$00,$00,$E1
LA484:  .byte   $FD,$FD,$00,$00,$FD
LA489:  .byte   $02,$02,$02,$01,$01
LA48E:  .byte   $40,$79,$78,$78,$79
LA493:  .byte   $90,$90,$90,$D0,$D0

; ===========================================================================
; main_nitron — Nitron (rocket/missile enemy, Gemini Man stage)
; Approaches player horizontally, then flies in a sinusoidal wave pattern
; while dropping bombs. After 2 bomb drops, climbs away upward.
; States: 0=approach, 1=sine wave flight + bomb, 2=climb away
; ent_timer=frame delay, ent_var1=sine index, ent_var2=bomb count, ent_var3=bomb cooldown
; ===========================================================================
main_nitron:
        lda     ent_status,x                 ; state 0: approach
        and     #$0F
        bne     code_A4BA
        lda     ent_facing,x                 ; check direction
        and     #$01
        beq     code_A4AC               ; branch if moving left
        jsr     LF71D                   ; move right toward player
        jmp     code_A4AF

code_A4AC:  jsr     LF73B               ; move left toward player
code_A4AF:  jsr     LF8C2               ; check X distance to player
        cmp     #$40                    ; if < 64 pixels away,
        bcs     code_A4B9               ; advance to sine wave state
        inc     ent_status,x
code_A4B9:  rts

; --- state 1+: dispatch flying vs climbing ---

code_A4BA:  lda     ent_status,x             ; check if state >= 2
        and     #$02
        beq     code_A4C4               ; state 1: sine wave flight
        jmp     code_A561               ; state 2: climb away

; --- state 1: sinusoidal wave flight ---

code_A4C4:  lda     ent_timer,x             ; if frame delay active,
        bne     code_A51A               ; skip to movement
        ldy     ent_var1,x                 ; load sine table step index
        lda     LA56E,y                 ; lookup phase entry
        asl     a                       ; *2 for 16-bit table offset
        tay
        lda     LA57C,y                 ; set Y speed from sine table (sub)
        sta     ent_yvel_sub,x
        lda     LA57D,y                 ; set Y speed from sine table (whole)
        sta     ent_yvel,x
        lda     LA59C,y                 ; set X speed from sine table (sub)
        sta     ent_xvel_sub,x
        lda     LA59D,y                 ; set X speed from sine table (whole)
        sta     ent_xvel,x
        lda     ent_xvel,x                 ; if X speed is negative,
        bpl     code_A503               ; negate it (always move in facing dir)
        lda     ent_xvel_sub,x                 ; negate 16-bit X speed:
        eor     #$FF                    ; two's complement low byte
        clc
        adc     #$01
        sta     ent_xvel_sub,x
        lda     ent_xvel,x                 ; two's complement high byte
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x
code_A503:  inc     ent_var1,x             ; advance sine step index
        lda     ent_var1,x
        cmp     #$06                    ; after 6 steps: flight phase done
        bne     code_A515
        inc     ent_status,x                 ; advance to state 2 (climb away)
        lda     #$1A                    ; bomb cooldown = 26 frames
        sta     ent_var3,x
code_A515:  lda     #$0D                ; frame delay = 13 (hold each sine
        sta     ent_timer,x                 ; step for 13 frames)

; --- apply current speed each frame ---
code_A51A:  dec     ent_timer,x             ; decrement frame delay
        lda     ent_y_sub,x                 ; apply Y speed to Y position
        clc                             ; (16-bit add: sub + whole)
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     ent_yvel,x
        sta     ent_y_px,x
        lda     ent_facing,x                 ; move horizontally in facing dir
        and     #$02
        bne     code_A53E               ; branch if facing left
        jsr     LF71D                   ; unconditional branch past
        bcs     code_A541
        bcc     code_A541
code_A53E:  jsr     LF73B               ; move left
code_A541:  lda     ent_var3,x             ; if bomb cooldown active,
        bne     code_A55D               ; skip to decrement
        jsr     code_A5BC               ; drop a bomb (spawn child)
        inc     ent_var2,x                 ; increment bomb count
        lda     ent_var2,x
        cmp     #$02                    ; after 2nd bomb: shorter cooldown
        bne     code_A557
        lda     #$10                    ; cooldown = 16 frames (after 2nd)
        bne     code_A559
code_A557:  lda     #$1A                ; cooldown = 26 frames (normal)
code_A559:  sta     ent_var3,x             ; store bomb cooldown timer
        rts

code_A55D:  dec     ent_var3,x             ; decrement bomb cooldown
        rts

; --- state 2: climb away (flee upward) ---

code_A561:  lda     #$00                ; Y speed = $00.03 (3.0 px/frame up)
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
        jmp     LF779                   ; fly upward off screen

; Nitron sine wave tables:
; $A56E: phase-to-index mapping (16 entries, one full sine cycle)
; $A57C: Y speed table (16-bit signed pairs for vertical oscillation)
; $A59C: X speed table (16-bit signed pairs for horizontal component)

LA56E:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$01
        .byte   $02,$03,$04,$05,$06,$07
LA57C:  .byte   $00
LA57D:  .byte   $FE,$27,$FE,$96,$FE,$3D,$FF,$00
        .byte   $00,$C3,$00,$6A,$01,$D9,$01,$00
        .byte   $02,$D9,$01,$6A,$01,$C3,$00,$00
        .byte   $00,$3D,$FF,$96,$FE,$27,$FE
LA59C:  .byte   $00
LA59D:  .byte   $00,$C3,$00,$6A,$01,$D9,$01,$00
        .byte   $02,$D9,$01,$6A,$01,$C3,$00,$00
        .byte   $00,$3D,$FF,$96,$FE,$27,$FE,$00
        .byte   $FE,$27,$FE,$96,$FE,$3D,$FF

; --- spawn_nitron_bomb: drop a bomb projectile below Nitron ---
code_A5BC:  jsr     LFC53               ; find free enemy slot
        bcs     code_A5FD               ; no slot: abort
        sty     L0000                   ; save child slot
        lda     ent_facing,x                 ; dir offset (0 or 2) for X table
        and     #$02
        tay
        lda     ent_x_px,x                 ; child X = parent X + dir offset
        clc
        adc     LA731,y
        pha
        lda     ent_x_scr,x
        adc     LA732,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x                 ; child Y = parent Y + 16 (below)
        clc
        adc     #$10
        sta     ent_y_px,y
        lda     #$00                    ; HP = 0 (indestructible)
        sta     ent_hp,y
        lda     #$81                    ; init child with OAM $81
        jsr     LF846                   ; init_child_entity
        lda     #$26                    ; AI routine = $26 (falling bomb)
        sta     ent_routine,y
        lda     #$93                    ; damage flags = projectile
        sta     ent_hitbox,y
code_A5FD:  rts

; --- Nitron bomb AI (routine $26): fall with gravity, explode on impact ---
        lda     ent_status,x
        and     #$0F
        bne     code_A60B
        jsr     LF81B                   ; reset_gravity
        inc     ent_status,x
code_A60B:  lda     ent_status,x
        and     #$02
        bne     code_A627
        ldy     #$12
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_A626
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$24
        jsr     LF89A                   ; submit_sound_ID
        inc     ent_status,x
code_A626:  rts

code_A627:  lda     ent_anim_id,x
        cmp     #$71
        bne     code_A626
        lda     ent_anim_state,x
        cmp     #$04
        bne     code_A626
        lda     #$80
        jmp     LF835                   ; reset_sprite_anim

; --- main_unknown_27 — unknown entity AI routine $27 ---
main_unknown_27:
        lda     ent_anim_id,x
        cmp     #$71
        bne     code_A668
        lda     ent_anim_state,x
        cmp     #$04
        bne     code_A667
        lda     #$92
        jsr     LF835                   ; reset_sprite_anim
        lda     #$40
        sta     ent_xvel_sub,x
        sta     ent_yvel_sub,x
        lda     #$00
        sta     ent_xvel,x
        sta     ent_yvel,x
        lda     #$C0
        sta     ent_hitbox,x
        lda     #$01
        sta     ent_hp,x
code_A667:  rts

code_A668:  jmp     code_ABEA               ; → Nutton homing AI

; ===========================================================================
; main_gyoraibo — Gyoraibo (torpedo fish, Gemini Man stage)
; Swims horizontally, opens mouth to fire upward projectile when aligned
; with player. If wall is hit, begins descending. ent_var2=wall-hit flag,
; ent_var1=fired flag, ent_timer=variant index.
; ===========================================================================
main_gyoraibo:
        lda     ent_status,x
        and     #$0F
        bne     code_A67D
        sta     ent_yvel,x
        lda     #$80
        sta     ent_yvel_sub,x
        inc     ent_status,x
code_A67D:  lda     ent_status,x
        and     #$02
        bne     code_A6BD
        lda     ent_facing,x
        and     #$01
        beq     code_A693
        ldy     #$14
        jsr     LF580                   ; move_right_collide
        jmp     code_A698

code_A693:  ldy     #$15
        jsr     LF5C4                   ; move_left_collide
code_A698:  bcc     code_A6A0
        inc     ent_var2,x
code_A69D:  jmp     LF759               ; move_sprite_down

code_A6A0:  lda     ent_var2,x
        bne     code_A69D
        lda     ent_var1,x
        bne     code_A6BC
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$08
        bcs     code_A6BC
        inc     ent_status,x
        inc     ent_var1,x
        lda     #$33
        jsr     LF835                   ; reset_sprite_anim
code_A6BC:  rts

code_A6BD:  lda     ent_anim_id,x
        cmp     #$33
        bne     code_A6BC
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_A6BC
        lda     #$32
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_xvel_sub,x
        lda     #$04
        sta     ent_xvel,x
        dec     ent_status,x
        jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_A730
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     LA731,y
        pha
        lda     ent_x_scr,x
        adc     LA732,y
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
        jsr     LF846                   ; init_child_entity
        lda     ent_routine,x
        cmp     #$28
        beq     code_A726
        lda     #$45
        sta     ent_routine,y
        bne     code_A72B
code_A726:  lda     #$29
        sta     ent_routine,y
code_A72B:  lda     #$C0
        sta     ent_hitbox,y
code_A730:  rts

LA731:  .byte   $00
LA732:  .byte   $00,$00,$00
        lda     ent_status,x
        and     #$0F
        bne     code_A74A
        sta     ent_timer,x
        sta     ent_yvel_sub,x
        lda     #$02
        sta     ent_yvel,x
        inc     ent_status,x
code_A74A:  lda     ent_status,x
        and     #$02
        bne     code_A76B
        ldy     #$17
        jsr     LF642
        bcc     code_A76B
        inc     ent_status,x
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$56
        sta     ent_routine,x
        rts

code_A76B:  lda     ent_timer,x
        bne     code_A7CA
        lda     ent_routine,x
        cmp     #$29
        beq     code_A780
        lda     ent_y_px,x
        cmp     #$62
        bcs     code_A7CA
        bcc     code_A787
code_A780:  lda     ent_y_px,x
        cmp     #$B4
        bcs     code_A7CA
code_A787:  inc     ent_timer,x
        jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_A7CA
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     LA731,y
        pha
        lda     ent_x_scr,x
        adc     LA732,y
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
        jsr     LF846                   ; init_child_entity
code_A7CA:  rts

; ===========================================================================
; main_penpen_maker — Penpen Maker (Gemini Man stage pipe spawner)
; Periodically spawns Penpen enemies. Changes palette colors on spawn.
; Uses a pseudo-random timer between spawns.
; ===========================================================================
main_penpen_maker:
        lda     ent_status,x            ; state 0: init
        and     #$0F
        bne     code_A7EB
        ldy     #$02
code_A7D4:  lda     LA8BD,y
        sta     $060D,y
        sta     $062D,y
        dey
        bpl     code_A7D4
        sty     palette_dirty
        inc     ent_status,x
        jsr     code_A82E
        sta     ent_timer,x
code_A7EB:  lda     ent_y_px,x
        pha
        sec
        sbc     #$10
        sta     ent_y_px,x
        lda     #$00
        sta     ent_hitbox,x
        lda     ent_anim_id,x
        pha
        jsr     code_8003
        pla
        sta     ent_anim_id,x
        pla
        sta     ent_y_px,x
        lda     ent_hp,x
        bne     code_A81A
        sta     ent_var1,x
        sta     ent_var2,x
        lda     #$63
        sta     ent_routine,x
        rts

code_A81A:  lda     #$98
        sta     ent_hitbox,x
        dec     ent_timer,x
        bne     code_A82D
        jsr     code_A8EC
        jsr     code_A82E
        sta     ent_timer,x
code_A82D:  rts

code_A82E:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     LA83B,y
        rts

LA83B:  .byte   $3C,$1E,$78,$3C
        lda     ent_var1,x
        bne     code_A8B8
        lda     #$0A
        sta     ent_var1,x
        jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_A8BB
        lda     #$27
        jsr     LF89A                   ; submit_sound_ID
        lda     #$71
        jsr     LF846                   ; init_child_entity
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
        adc     LA8DC,x
        sta     ent_y_px,y
        lda     $02
        clc
        adc     LA8E4,x
        sta     ent_x_px,y
        pla
        asl     a
        asl     a
        tax
        ldy     #$00
code_A894:  lda     LA8BC,x
        sta     $060C,y
        sta     $062C,y
        inx
        iny
        cpy     #$04
        bne     code_A894
        lda     #$FF
        sta     palette_dirty
        ldx     L0000
        inc     ent_var2,x
        lda     ent_var2,x
        and     #$03
        bne     code_A8BB
        lda     #$00
        sta     ent_status,x
code_A8B8:  dec     ent_var1,x
code_A8BB:  rts

LA8BC:  .byte   $0F
LA8BD:  .byte   $20,$10,$17,$0F,$10,$00,$17,$0F
        .byte   $00,$0F,$07,$0F,$0F,$0F,$0F,$0F
        .byte   $20,$37,$17,$0F,$10,$27,$07,$0F
        .byte   $0F,$17,$0F,$0F,$0F,$0F,$0F
LA8DC:  .byte   $F0,$10,$10,$D0,$F0,$10,$10,$D0
LA8E4:  .byte   $F0,$20,$E8,$10,$F0,$20,$E8,$10
code_A8EC:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_A93A
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     LA93B,y
        pha
        lda     ent_x_scr,x
        adc     LA93C,y
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
        jsr     LF846                   ; init_child_entity
        lda     #$46
        sta     ent_routine,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
code_A93A:  rts

LA93B:  .byte   $F8
LA93C:  .byte   $FF,$F8,$FF
        lda     ent_facing,x
        and     #$01
        beq     code_A94E
        lda     #$08
        jsr     LF580                   ; move_right_collide
        jmp     code_A953

code_A94E:  ldy     #$09
        jsr     LF5C4                   ; move_left_collide
code_A953:  bcc     code_A95F
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_routine,x
code_A95F:  rts

        lda     ent_anim_frame,x
        cmp     #$02
        bne     code_A95F
        lda     ent_timer,x
        cmp     #$08
        beq     code_A95F
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
code_A98D:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_A9F7
        lda     $10
        jsr     LF846                   ; init_child_entity
        lda     $11
        cmp     #$56
        beq     code_A9B3
        cmp     #$48
        beq     code_A9B3
        cmp     #$39
        beq     code_A9BF
        cmp     #$48
        beq     code_A9BF
        lda     #$00
        sta     ent_hitbox,y
        sta     ent_routine,y
        beq     code_A9C9
code_A9B3:  lda     #$80
        sta     ent_hitbox,y
        lda     #$54
        sta     ent_routine,y
        bne     code_A9C9
code_A9BF:  lda     #$80
        sta     ent_hitbox,y
        lda     #$55
        sta     ent_routine,y
code_A9C9:  lda     #$00
        sta     $01
        lda     LA9FA,x
        bpl     code_A9D4
        dec     $01
code_A9D4:  clc
        adc     $02
        sta     ent_x_px,y
        lda     $03
        adc     $01
        sta     ent_x_scr,y
        lda     $04
        clc
        adc     LAA02,x
        sta     ent_y_px,y
        ldx     L0000
        inc     ent_timer,x
        lda     ent_timer,x
        tax
        and     #$01
        bne     code_A98D
code_A9F7:  ldx     L0000
        rts

LA9FA:  .byte   $0C,$F4,$F0,$10,$F4,$0C,$00,$00
LAA02:  .byte   $F4,$0C,$00,$00,$F4,$0C,$F0,$10

; ===========================================================================
; main_bomber_pepe — Bomber Pepe (penguin bomber, Gemini Man stage)
; Flies horizontally dropping bombs, with gravity and floor collision.
; Uses pseudo-random timer between bomb drops.
; ===========================================================================
main_bomber_pepe:
        lda     ent_status,x
        and     #$0F
        bne     code_AA2C
        lda     #$44
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
        lda     #$1E
        sta     ent_timer,x
        jsr     LF883                   ; set_sprite_hflip
        jsr     code_A82E
        sta     ent_var1,x
        inc     ent_status,x
code_AA2C:  dec     ent_var1,x
        bne     code_AA3A
        jsr     code_AA9A
        jsr     code_A82E
        sta     ent_var1,x
code_AA3A:  lda     ent_status,x
        and     #$02
        bne     code_AA72
        lda     ent_facing,x
        and     #$01
        beq     code_AA50
        ldy     #$0A
        jsr     LF580                   ; move_right_collide
        jmp     code_AA55

code_AA50:  ldy     #$0B
        jsr     LF5C4                   ; move_left_collide
code_AA55:  ldy     #$20
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_AA6D
        lda     #$44
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
        inc     ent_status,x
        lda     #$3D
        bne     code_AA6F
code_AA6D:  lda     #$3C
code_AA6F:  jmp     LF835               ; reset_sprite_anim

code_AA72:  lda     ent_anim_id,x
        cmp     #$3B
        beq     code_AA86
        lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_AA99
        lda     #$3B
        jsr     LF835                   ; reset_sprite_anim
code_AA86:  dec     ent_timer,x
        bne     code_AA99
        jsr     LF869                   ; face_player
        jsr     LF883                   ; set_sprite_hflip
        dec     ent_status,x
        lda     #$3C
        sta     ent_timer,x
code_AA99:  rts

code_AA9A:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_AADB
        sty     L0000
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     LAADC,y
        pha
        lda     ent_x_scr,x
        adc     LAADD,y
        ldy     L0000
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        sta     ent_y_px,y
        lda     #$01
        sta     ent_hp,y
        lda     #$3E
        jsr     LF846                   ; init_child_entity
        lda     #$1D
        sta     ent_routine,y
        lda     #$C0
        sta     ent_hitbox,y
code_AADB:  rts

LAADC:  .byte   $08
LAADD:  .byte   $00,$F8,$FF
        lda     ent_status,x
        and     #$0F
        bne     code_AAF2
        sta     ent_timer,x
        lda     #$1E
        sta     ent_var1,x
        inc     ent_status,x
code_AAF2:  lda     ent_status,x
        and     #$02
        bne     code_AB3A
        lda     ent_facing,x
        and     #$01
        beq     code_AB06
        jsr     LF71D                   ; move_sprite_right
        jmp     code_AB09

code_AB06:  jsr     LF73B               ; move_sprite_left
code_AB09:  ldy     #$08
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_AB39
        lda     ent_timer,x
        tay
        lda     LAB4F,y
        sta     ent_yvel_sub,x
        lda     LAB52,y
        sta     ent_yvel,x
        lda     LAB55,y
        sta     ent_xvel_sub,x
        lda     LAB58,y
        sta     ent_xvel,x
        inc     ent_timer,x
        lda     ent_timer,x
        cmp     #$03
        bcc     code_AB39
        inc     ent_status,x
code_AB39:  rts

code_AB3A:  dec     ent_var1,x
        bne     code_AB3A
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$39
        sta     ent_routine,x
        rts

LAB4F:  .byte   $9E,$44,$4F
LAB52:  .byte   $04,$03,$02
LAB55:  .byte   $00,$00,$80
LAB58:  .byte   $01,$01,$00

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
        bne     code_AB65               ; no -> Bolton (nut body) logic
        jmp     code_ABEA               ; yes -> Nutton homing toward player

; -- Bolton (nut body) logic --

code_AB65:  lda     ent_status,x             ; check state
        and     #$0F                    ; state 0 = idle, waiting
        bne     code_ABC9               ; nonzero = bolt already launched
        jsr     code_AC27               ; check player range + find free slot
        bcs     code_ABE9               ; carry set = can't fire, return
        lda     ent_flags,x                 ; clear bit 2 (open-mouth indicator)
        and     #$FB
        sta     ent_flags,x
        inc     ent_status,x                 ; advance to state 1 (bolt launched)
        lda     #$66                    ; Y speed = $00.66/frame
        sta     ent_yvel_sub,x                 ; (slow upward drift for Bolton body)
        lda     #$00
        sta     ent_yvel,x
        lda     #$2E                    ; spawn child entity type $2E
        jsr     LF846                   ; (Nutton bolt projectile)
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
code_ABC9:  lda     ent_flags,x
        and     #$FB
        sta     ent_flags,x
        lda     ent_timer,x
        bne     code_ABE9
        lda     #$00
        sta     ent_anim_frame,x
        lda     frame_counter
        and     #$04
        beq     code_ABE9
        lda     ent_flags,x
        ora     #$04
        sta     ent_flags,x
code_ABE9:  rts

code_ABEA:  lda     ent_x_px
        sec
        sbc     ent_x_px,x
        pha
        lda     ent_x_scr
        sbc     ent_x_scr,x
        pla
        beq     code_AC16
        bcc     code_AC0B
        jsr     LF71D                   ; move_sprite_right
        lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
        jmp     code_AC16

code_AC0B:  jsr     LF73B               ; move_sprite_left
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
code_AC16:  lda     ent_y_px
        sec
        sbc     ent_y_px,x
        beq     code_ABE9
        bcs     code_AC24
        jmp     LF779                   ; move_sprite_up

code_AC24:  jmp     LF759               ; move_sprite_down

code_AC27:  jsr     LF8C2               ; entity_x_dist_to_player
        cmp     #$44
        bcs     code_AC7E
        lda     ent_x_px,x
        sec
        sbc     camera_x_lo
        sta     L0000
        lda     ent_x_scr,x
        sbc     camera_screen
        bcc     code_AC79
        lda     L0000
        cmp     #$80
        bcc     code_AC79
        lda     #$00
        sta     $01
        stx     L0000
        ldy     #$1F
code_AC4B:  cpy     L0000
        beq     code_AC62
        lda     ent_status,y
        bpl     code_AC62
        lda     ent_flags,y
        bpl     code_AC62
        lda     ent_routine,y
        cmp     #$30
        bne     code_AC62
        inc     $01
code_AC62:  dey
        cpy     #$0F
        bne     code_AC4B
        lda     $01
        cmp     #$03
        beq     code_AC79
        ldy     L0000
code_AC6F:  lda     ent_status,y
        bpl     code_AC80
        dey
        cpy     #$0F
        bne     code_AC6F
code_AC79:  lda     #$00
        sta     ent_status,x
code_AC7E:  sec
        rts

code_AC80:  clc
        rts

        lda     ent_status,x
        and     #$0F
        bne     code_ACBE
        sta     ent_var3,x
        jsr     LF71D                   ; move_sprite_right
        lda     ent_timer,x
        sec
        sbc     ent_x_px,x
        lda     ent_var2,x
        sbc     ent_x_scr,x
        bcs     code_ACB8
        lda     ent_timer,x
        sta     ent_x_px,x
        lda     ent_var2,x
        sta     ent_x_scr,x
        inc     ent_status,x
        ldy     ent_var1,x
        lda     #$08
        sta     ent_timer,y
        sta     ent_timer,x
code_ACB8:  lda     #$00
        sta     ent_anim_frame,x
        rts

code_ACBE:  lda     ent_var3,x
        bne     code_ACCB
        lda     #$25
        jsr     LF89A                   ; submit_sound_ID
        inc     ent_var3,x
code_ACCB:  lda     #$01
        sta     $95
        lda     ent_timer,x
        beq     code_ACE0
        lda     ent_anim_frame,x
        bne     code_AD08
        dec     ent_timer,x
        inc     ent_x_px,x
        rts

code_ACE0:  lda     ent_anim_state,x
        bne     code_AD08
        sta     ent_anim_frame,x
        ldy     ent_var1,x
        lda     ent_anim_state,y
        bne     code_AD08
        sta     ent_anim_frame,y
        sta     ent_timer,y
        sta     ent_status,x
        lda     #$2F
        sta     ent_anim_id,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$01
        sta     ent_hp,y
code_AD08:  rts

; ===========================================================================
; main_have_su_bee — Have "Su" Bee (bee carrier, Snake Man stage)
; Flies toward player, hovers while carrying a bee, then releases it.
; After release, reverses direction and flies away. ent_timer=pre-launch
; delay, ent_var2=hover timer, ent_var1=release range check flag.
; ===========================================================================
main_have_su_bee:

        lda     ent_status,x
        and     #$0F
        bne     code_AD4B
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$28
        bcs     code_AD4A
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
        adc     LADD5,y
        pha
        lda     ent_x_scr,x
        adc     LADD6,y
        sta     ent_x_scr,x
        pla
        sta     ent_x_px,x
        jsr     LF869                   ; face_player
code_AD4A:  rts

code_AD4B:  lda     ent_status,x
        and     #$02
        bne     code_AD9A
        jsr     LF883                   ; set_sprite_hflip
        lda     ent_facing,x
        and     #$01
        beq     code_AD62
        jsr     LF71D                   ; move_sprite_right
        jmp     code_AD65

code_AD62:  jsr     LF73B               ; move_sprite_left
code_AD65:  lda     ent_var1,x
        bne     code_AD99
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$50
        bcc     code_AD99
        inc     ent_status,x
        inc     ent_var1,x
        lda     #$80
        sta     ent_yvel_sub,x
        lda     #$00
        sta     ent_yvel,x
        lda     ent_facing,x
        and     #$01
        beq     code_AD91
        lda     ent_flags,x
        and     #$BF
        sta     ent_flags,x
        rts

code_AD91:  lda     ent_flags,x
        ora     #$40
        sta     ent_flags,x
code_AD99:  rts

code_AD9A:  dec     ent_var2,x
        bne     code_ADB2
        dec     ent_status,x
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        lda     #$3A
        jsr     LF835                   ; reset_sprite_anim
        jmp     code_ADD9

code_ADB2:  lda     ent_facing,x
        and     #$01
        beq     code_ADBF
        jsr     LF779                   ; move_sprite_up
        jmp     code_ADC2

code_ADBF:  jsr     LF759               ; move_sprite_down
code_ADC2:  dec     ent_timer,x
        bne     code_AD99
        lda     #$3C
        sta     ent_timer,x
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
        rts

LADD5:  .byte   $50
LADD6:  .byte   $00,$B0,$FF
code_ADD9:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_AE27
        sty     $01
        lda     ent_facing,x
        sta     ent_facing,y
        and     #$02
        tay
        lda     ent_x_px,x
        clc
        adc     LA731,y
        pha
        lda     ent_x_scr,x
        adc     LA732,y
        ldy     $01
        sta     ent_x_scr,y
        pla
        sta     ent_x_px,y
        lda     ent_y_px,x
        clc
        adc     #$18
        sta     ent_y_px,y
        lda     #$35
        jsr     LF846                   ; init_child_entity
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
code_AE27:  rts

; ===========================================================================
; main_beehive — Beehive (Snake Man stage)
; Falls until hitting floor, then explodes and spawns 5 Chibee enemies
; at offset positions around the hive. Changes AI routine to $3A (dead).
; ===========================================================================
main_beehive:
        ldy     #$08
        jsr     LF606                   ; move_down_collide
        bcc     code_AE27
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$3A
        sta     ent_routine,x
        lda     #$00
        sta     $01
code_AE42:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_AEB3
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
        adc     LAEB5,y
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
        jsr     LF846                   ; init_child_entity
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
        bcc     code_AE42
code_AEB3:  rts

; spawn offsets indexed by bee #
; screen and pixel (not sub)

bee_spawn_x_offset:  .byte   $E8
LAEB5:  .byte   $FF,$E8,$FF,$01,$00,$18,$00,$18
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
        bne     code_AEDF
        inc     ent_status,x
        lda     #$3C
        sta     ent_yvel_sub,x
        lda     #$09
        sta     ent_yvel,x
        lda     #$1E
        sta     ent_timer,x
        jsr     LF883                   ; set_sprite_hflip
code_AEDF:  lda     ent_status,x
        and     #$0F
        cmp     #$02
        beq     code_AF3F
        cmp     #$03
        bne     code_AEEF
        jmp     code_AFA5

code_AEEF:  lda     ent_timer,x
        bne     code_AF16
        lda     ent_anim_id,x
        cmp     #$44
        beq     code_AF1A
        lda     ent_var1,x
        bne     code_AF1A
        lda     #$45
        jsr     LF835                   ; reset_sprite_anim
        ldy     #$15
        jsr     LF67C                   ; move_vertical_gravity
        lda     $10
        and     #$10
        beq     code_AF19
        lda     #$44
        jsr     LF835                   ; reset_sprite_anim
        rts

code_AF16:  dec     ent_timer,x
code_AF19:  rts

code_AF1A:  lda     ent_anim_id,x
        cmp     #$45
        beq     code_AF30
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$28
        bcs     code_AF19
        lda     #$45
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_var1,x
code_AF30:  ldy     #$15
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_AF19
        lda     #$43
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_status,x
code_AF3F:  lda     ent_var2,x
        bne     code_AF5C
        lda     ent_hp,x
        cmp     #$04
        bne     code_AF5C
        inc     ent_status,x
        lda     #$3C
        sta     ent_yvel_sub,x
        lda     #$09
        sta     ent_yvel,x
        inc     ent_var2,x
        rts

code_AF5C:  lda     ent_facing,x
        and     #$01
        beq     code_AF6B
        ldy     #$16
        jsr     LF580                   ; move_right_collide
        jmp     code_AF70

code_AF6B:  ldy     #$17
        jsr     LF5C4                   ; move_left_collide
code_AF70:  lda     ent_anim_id,x
        cmp     #$43
        beq     code_AF9B
        ldy     #$15
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_AF9A
        lda     #$BB
        sta     ent_yvel_sub,x
        lda     #$06
        sta     ent_yvel,x
        lda     #$43
        jsr     LF835                   ; reset_sprite_anim
        lda     #$01
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        jsr     LF869                   ; face_player
code_AF9A:  rts

code_AF9B:  lda     ent_anim_state,x
        bne     code_AFA0
code_AFA0:  lda     #$46
        jmp     LF835                   ; reset_sprite_anim

code_AFA5:  lda     ent_anim_id,x
        cmp     #$44
        beq     code_AFC6
        lda     #$45
        jsr     LF835                   ; reset_sprite_anim
        ldy     #$15
        jsr     LF67C                   ; move_vertical_gravity
        lda     $10
        and     #$10
        beq     code_AF9A
        lda     #$44
        jsr     LF835                   ; reset_sprite_anim
        lda     #$5A
        sta     ent_var3,x
code_AFC6:  dec     ent_var3,x
        bne     code_AFD6
        dec     ent_status,x
        jsr     LF81B                   ; reset_gravity
        lda     #$45
        jsr     LF835                   ; reset_sprite_anim
code_AFD6:  rts

; ===========================================================================
; main_wanaan — Wanaan (pipe snake, Snake Man stage)
; Hides in pipe, snaps out to bite when player is within $18 px both X and Y.
; Snap sequence: presnap delay → upward snap (6 frames) → sound → downward
; snap (16 frames) → retract to original position and re-hide.
; ===========================================================================
main_wanaan:
        lda     ent_status,x                 ; test state:
        and     #$0F                    ; any of these bits
        bne     LB00D                   ; means at least presnap
        sta     ent_yvel_sub,x
        lda     #$02                    ; if not, $0200
        sta     ent_yvel,x                 ; -> Y speed
        jsr     LF8B3                   ; entity_y_dist_to_player
        cmp     #$18
        bcs     LB00C                   ; if player is within $18
        jsr     LF8C2                   ; pixel distance both X & Y
        cmp     #$18                    ; (if not return)
        bcs     LB00C
        inc     ent_status,x                 ; start snapping!
        lda     #$21                    ; $21 frames of delay timer
        sta     ent_timer,x                 ; for presnap
        lda     #$06                    ; 6 frames to snap upward
        sta     ent_var1,x                 ; snapping timer
        lda     ent_y_px,x                 ; preserve original Y position
        sta     ent_var2,x                 ; pre-snap
        lda     #$10                    ; 16 frames downward snap
        sta     ent_var3,x
LB00C:  rts

LB00D:  lda     ent_flags,x                 ; this sprite flag
        and     #$04                    ; indicates past presnap
        beq     LB026
        dec     ent_timer,x                 ; presnap timer
        bne     LB00C                   ; not expired yet? return
        lda     ent_flags,x                 ; on expiration,
        eor     #$04                    ; turn $04 sprite flag on
        sta     ent_flags,x
        lda     #$A3                    ; set shape $A3 (extended hitbox)
        sta     ent_hitbox,x
LB026:  lda     ent_status,x                 ; this bitflag on
        and     #$02                    ; means past snapping
        bne     LB04C
        lda     #$00
        sta     ent_anim_frame,x                 ; show open mouth frame
        sta     ent_anim_state,x
        dec     ent_y_px,x
        dec     ent_y_px,x                 ; move up 3 pixels
        dec     ent_y_px,x
        dec     ent_var1,x                 ; upward snap timer
        bne     LB00C                   ; not expired yet? return
        inc     ent_status,x
        lda     #$22                    ; on expiration,
        jsr     LF89A                   ; $02 -> state
        rts

LB04C:  lda     #$01
        sta     ent_anim_state,x                 ; show closed mouth frame
        lda     #$00
        sta     ent_anim_frame,x
        jsr     LF759                   ; move_sprite_down
        dec     ent_var3,x                 ; downward snap timer
        bne     LB00C                   ; not expired yet? return
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
        bne     code_B096
        jsr     LF883                   ; set_sprite_hflip
        inc     ent_status,x
        lda     #$36
        sta     ent_timer,x
        lda     #$10
        sta     ent_var1,x
code_B096:  lda     ent_status,x
        and     #$02
        bne     code_B0CB
        lda     ent_anim_id,x
        cmp     #$D6
        beq     code_B0B5
        lda     ent_var2,x
        cmp     #$03
        bcs     code_B0ED
        dec     ent_timer,x
        bne     code_B0E0
        lda     #$D6
        jsr     LF835                   ; reset_sprite_anim
code_B0B5:  lda     #$00
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        jsr     code_B11C
        inc     ent_var2,x
        lda     #$36
        sta     ent_timer,x
        inc     ent_status,x
code_B0CB:  lda     #$00
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        dec     ent_var1,x
        bne     code_B0EC
        lda     #$10
        sta     ent_var1,x
        dec     ent_status,x
code_B0E0:  lda     ent_anim_id,x
        cmp     #$C6
        beq     code_B0EC
        lda     #$C6
        jsr     LF835                   ; reset_sprite_anim
code_B0EC:  rts

code_B0ED:  lda     #$00
        sta     L0000
        lda     #$E2
        sta     $01
        ldy     #$1F
code_B0F7:  lda     ent_status,y
        bmi     code_B110
code_B0FC:  dey
        cpy     #$0F
        bne     code_B0F7
        lda     L0000
        cmp     #$03
        beq     code_B10A
        dec     ent_var2,x
code_B10A:  lda     #$38
        sta     ent_timer,x
        rts

code_B110:  lda     $01
        cmp     ent_anim_id,y
        bne     code_B0FC
        inc     L0000
        jmp     code_B0FC

code_B11C:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_B155
        lda     ent_facing,x
        sta     ent_facing,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        clc
        adc     #$04
        sta     ent_y_px,y
        lda     #$E2
        jsr     LF846                   ; init_child_entity
        lda     #$98
        sta     ent_flags,y
        lda     #$C0
        sta     ent_hitbox,y
        lda     #$42
        sta     ent_routine,y
        lda     #$01
        sta     ent_hp,y
code_B155:  rts

        lda     ent_status,x
        and     #$0F
        bne     code_B173
        sta     ent_var1,x
        sta     ent_xvel_sub,x
        lda     #$02
        sta     ent_xvel,x
        inc     ent_status,x
        lda     #$F0
        sta     ent_timer,x
        jsr     LF81B                   ; reset_gravity
code_B173:  ldy     #$08
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_B198
        lda     ent_facing,x
        and     #$01
        beq     code_B189
        ldy     #$08
        jsr     LF580                   ; move_right_collide
        jmp     code_B18E

code_B189:  ldy     #$09
        jsr     LF5C4                   ; move_left_collide
code_B18E:  bcc     code_B198
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_B198:  dec     ent_timer,x
        bne     code_B1A8
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_routine,x
code_B1A7:  rts

code_B1A8:  lda     ent_var1,x
        bne     code_B1A7
        lda     ent_timer,x
        cmp     #$B4
        bne     code_B1A7
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
        bne     code_B1F8
        lda     ent_facing,x
        and     #$01
        beq     code_B1D3
        ldy     #$18
        jsr     LF580                   ; move_right_collide
        jmp     code_B1D8

code_B1D3:  ldy     #$19
        jsr     LF5C4                   ; move_left_collide
code_B1D8:  ldy     #$18
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_B1EE
        jsr     code_B224
        inc     ent_status,x
        lda     #$CB
        sta     ent_hitbox,x
        lda     #$2B
        bne     code_B1F5
code_B1EE:  lda     #$C3
        sta     ent_hitbox,x
        lda     #$2A
code_B1F5:  jmp     LF835               ; reset_sprite_anim

code_B1F8:  lda     ent_anim_id,x
        cmp     #$2B
        bne     LB20A
        lda     ent_anim_state,x
        cmp     #$02
        bne     LB21F
        lda     #$29
        bne     LB20C
LB20A:  lda     #$29
LB20C:  jsr     LF835                   ; reset_sprite_anim
        lda     ent_timer,x                 ; timer not expired yet?
        bne     LB220
        lda     #$3C                    ; on timer expiration,
        sta     ent_timer,x                 ; set hopping state
        dec     ent_status,x                 ; with timer 60 frames
        jsr     LF869                   ; and hop toward player
LB21F:  rts

LB220:  dec     ent_timer,x
        rts

code_B224:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$01
        tay
        lda     LB23A,y
        sta     ent_yvel_sub,x
        lda     LB23C,y
        sta     ent_yvel,x
        rts

LB23A:  .byte   $52,$A8
LB23C:  .byte   $04,$05

; ===========================================================================
; main_top_man_platform — Top Man stage moving platform (conveyor belt)
; Moves up or down, carries the player when standing on it. Reverses
; direction on a timer. Checks player collision to apply conveyor push.
; ===========================================================================
main_top_man_platform:
        lda     ent_status,x
        and     #$0F
        bne     code_B265
        sta     ent_yvel_sub,x
        lda     #$01
        sta     ent_yvel,x
        inc     ent_status,x
        lda     ent_y_px,x
        cmp     #$88
        bcs     code_B25A
        inc     ent_var1,x
code_B25A:  lda     #$10
        sta     ent_timer,x
        lda     #$01
        sta     ent_facing,x
        rts

code_B265:  jsr     LF8C2               ; entity_x_dist_to_player
        cmp     #$16
        bcs     code_B29C
        jsr     LF8B3                   ; entity_y_dist_to_player
        cmp     #$15
        bcs     code_B29C
        lda     ent_facing,x
        and     #$02
        bne     code_B27E
        lda     #$01
        bne     code_B280
code_B27E:  lda     #$02
code_B280:  sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
        dec     ent_timer,x
        bne     code_B29C
        lda     #$10
        sta     ent_timer,x
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_B29C:  lda     ent_var1,x
        bne     code_B2AF
        jsr     LF779                   ; move_sprite_up
        lda     ent_y_scr,x
        beq     code_B2AE
        lda     #$00
        sta     ent_y_scr,x
code_B2AE:  rts

code_B2AF:  lda     ent_y_px,x
        pha
        dec     ent_y_px,x
        jsr     LFAE2                   ; check_player_collision
        pla
        sta     ent_y_px,x
        bcs     code_B2F0
        lda     ent_y_px
        sec
        sbc     ent_y_px,x
        bcs     code_B2DC
        lda     ent_y_px
        adc     #$02
        sta     ent_y_px
        cmp     #$F0
        bcc     code_B2DC
        adc     #$0F
        sta     ent_y_px
        inc     ent_y_scr
code_B2DC:  stx     $0F
        ldx     #$00
        ldy     #$00
        jsr     LE8D6
        lda     $10
        and     #$10
        beq     code_B2EE
        jsr     LEE13
code_B2EE:  ldx     $0F
code_B2F0:  jsr     LF759               ; move_sprite_down
        lda     ent_y_scr,x
        beq     code_B2AE
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
        beq     code_B308               ; if zero, process state
        dec     ent_timer,x                 ; decrement delay timer
        bne     code_B346               ; still waiting -> freeze anim + return
code_B308:  lda     ent_flags,x             ; sprite flags
        and     #$04                    ; check bit 2 (retracted flag)
        beq     code_B31F               ; not retracted -> animate crush cycle
        jsr     LF8C2                   ; A = horizontal distance to player
        cmp     #$61                    ; within 97 pixels?
        bcs     code_B346               ; too far -> freeze anim + return
        lda     ent_flags,x                 ; clear retracted flag (bit 2)
        and     #$FB
        sta     ent_flags,x
        rts

code_B31F:  lda     ent_anim_frame,x             ; anim frame timer
        bne     code_B34B               ; animation still ticking -> wait
        lda     ent_anim_state,x                 ; anim sequence frame index
        bne     code_B338               ; not frame 0 -> check frame 2
        lda     #$80                    ; damage = invincible only (no hurt)
        sta     ent_hitbox,x
        lda     ent_flags,x                 ; set retracted flag (bit 2)
        ora     #$04
        sta     ent_flags,x
        bne     code_B341               ; (always taken) -> set delay timer
code_B338:  cmp     #$02                ; anim frame 2? (midway)
        bne     code_B34C               ; other frames -> set active damage
        lda     #$AE                    ; damage = hittable + hurts player
        sta     ent_hitbox,x
code_B341:  lda     #$1E                ; delay timer = 30 frames
        sta     ent_timer,x
code_B346:  lda     #$00                ; clear anim frame timer
        sta     ent_anim_frame,x                 ; (forces instant frame advance)
code_B34B:  rts

code_B34C:  lda     #$A4                ; damage = hittable + hurts player
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
        bne     code_B389
        lda     ent_flags,x
        and     #$04
        beq     code_B372
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$61
        bcs     code_B34B
        lda     ent_flags,x
        and     #$FB
        sta     ent_flags,x
        jmp     LF883

code_B372:  jsr     LF797               ; apply_y_speed
        lda     ent_y_px,x
        cmp     #$78
        bcc     code_B34B
        lda     #$11
        sta     ent_timer,x
        lda     #$03
        sta     ent_var1,x
        inc     ent_status,x
code_B389:  lda     ent_anim_id,x
        cmp     #$56
        bne     code_B39A
        lda     ent_anim_frame,x
        bne     code_B34B
        lda     #$55
        jsr     LF835                   ; reset_sprite_anim
code_B39A:  dec     ent_timer,x
        lda     ent_timer,x
        bne     code_B3AA
        lda     #$11
        sta     ent_timer,x
        inc     ent_var1,x
code_B3AA:  lda     ent_var1,x
        and     #$03
        sta     ent_var1,x
        tay
        lda     ent_y_sub,x
        clc
        adc     LB44C,y
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     LB450,y
        sta     ent_y_px,x
        lda     ent_facing,x
        and     #$02
        beq     code_B3DA
        lda     ent_var2,x
        bne     code_B3D7
        jsr     LF8C2                   ; entity_x_dist_to_player
        bcs     code_B3E7
code_B3D7:  jmp     LF73B               ; move_sprite_left

code_B3DA:  lda     ent_var2,x
        bne     code_B3E4
        jsr     LF8C2                   ; entity_x_dist_to_player
        bcc     code_B3E7
code_B3E4:  jmp     LF71D               ; move_sprite_right

code_B3E7:  lda     #$26
        jsr     LF89A                   ; submit_sound_ID
        lda     #$56
        jsr     LF835                   ; reset_sprite_anim
        stx     L0000
        lda     #$07
        sta     $01
code_B3F7:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_B446
        ldx     $01
        lda     LB454,x
        sta     ent_xvel_sub,y
        lda     LB45C,x
        sta     ent_xvel,y
        lda     LB464,x
        sta     ent_yvel_sub,y
        lda     LB46C,x
        sta     ent_yvel,y
        lda     LB474,x
        sta     ent_facing,y
        ldx     L0000
        lda     #$57
        jsr     LF846                   ; init_child_entity
        lda     #$80
        sta     ent_hitbox,y
        lda     #$0F
        sta     ent_routine,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     ent_y_px,x
        sec
        sbc     #$0C
        sta     ent_y_px,y
        dec     $01
        bpl     code_B3F7
code_B446:  ldx     L0000
        inc     ent_var2,x
        rts

LB44C:  .byte   $80,$80,$80,$80
LB450:  .byte   $FF,$00,$00,$FF
LB454:  .byte   $00,$6A,$00,$6A,$00,$6A,$00,$6A
LB45C:  .byte   $00,$01,$02,$01,$00,$01,$02,$01
LB464:  .byte   $00,$96,$00,$6A,$00,$6A,$00,$96
LB46C:  .byte   $FE,$FE,$00,$01,$02,$01,$00,$FE
LB474:  .byte   $02,$02,$02,$02,$01,$01,$01,$01

; ===========================================================================
; main_peterchy — Peterchy (walking snake, Snake Man stage)
; Walks horizontally with gravity, reverses at walls. Charges when player
; is close (< $10 px), backs off when > $30 px away.
; ===========================================================================
main_peterchy:
        ldy     #$08
        jsr     LF67C                   ; move_vertical_gravity
        lda     ent_facing,x
        and     #$01
        beq     code_B490
        ldy     #$1A
        jsr     LF580                   ; move_right_collide
        jmp     code_B495

code_B490:  ldy     #$1B
        jsr     LF5C4                   ; move_left_collide
code_B495:  bcc     code_B49F
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_B49F:  lda     ent_status,x
        and     #$0F
        bne     code_B4B1
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$10
        bcs     code_B4B0
        inc     ent_status,x
code_B4B0:  rts

code_B4B1:  jsr     LF8C2               ; entity_x_dist_to_player
        cmp     #$30
        bcc     code_B4B0
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
        jsr     LF67C                   ; move_vertical_gravity
        rol     $0F                     ; save carry (landed flag) into $0F bit 0
        jsr     LFB7B                   ; check if weapon hit this enemy
        bcs     code_B4EC               ; survived → continue walking

; --- weapon killed this enemy: explode ---
        lda     #$18                    ; play explosion sound
        jsr     LF89A                   ; submit_sound_ID
        ldy     $10                     ; deactivate the weapon that hit us
        lda     #$00
        sta     ent_status,y
        lda     #$71                    ; switch to explosion sprite
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00                    ; clear timer
        sta     ent_timer,x
        lda     #$39                    ; change main routine to explosion handler
        sta     ent_routine,x
        rts

; --- survived weapon hit: walk horizontally ---

code_B4EC:  lda     ent_facing,x             ; check facing direction
        and     #$01
        beq     code_B4FB               ; bit 0 clear → move left
        ldy     #$1C                    ; move right with wall collision
        jsr     LF580                   ; move_right_collide
        jmp     code_B500

code_B4FB:  ldy     #$1D                ; move left with wall collision
        jsr     LF5C4                   ; move_left_collide
code_B500:  lda     $0F                 ; check landed flag (saved from $99)
        and     #$01
        beq     code_B53F               ; not on ground → done
        lda     $10                     ; check collision result: bit 4 = hit wall
        and     #$10
        beq     code_B52E               ; no wall hit → restore walk anim

; --- hit a wall: bounce ---
        lda     #$52                    ; switch to bounce sprite
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_timer,x                 ; increment bounce counter
        lda     ent_timer,x
        cmp     #$04                    ; 4 bounces → reverse direction
        beq     code_B526
        lda     #$29                    ; set upward bounce velocity
        sta     ent_yvel_sub,x                 ; Y speed = $05.29 upward
        lda     #$05                    ; ($99 will arc it back down)
        sta     ent_yvel,x
        rts

; --- bounced 4 times: reverse walk direction ---

code_B526:  lda     ent_facing,x             ; flip horizontal direction
        eor     #$03                    ; (toggle bits 0 and 1)
        sta     ent_facing,x

; --- restore walking animation ---
code_B52E:  lda     ent_anim_id,x             ; if already using walk OAM $51, skip
        cmp     #$51
        beq     code_B53A
        lda     #$51                    ; reset to walking sprite
        jsr     LF835                   ; reset_sprite_anim
code_B53A:  lda     #$00                ; reset bounce counter
        sta     ent_timer,x
code_B53F:  rts

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
        bmi     code_B55B               ; on-screen → active processing

; --- off-screen or destroyed: deactivate ---
        lda     #$00                    ; deactivate entity
        sta     ent_status,x
code_B54A:  sta     $95                 ; clear palette cycle / flash vars
        sta     $72
        lda     #$30                    ; set screen effect parameters
        sta     $1D                     ; $1D=$30 (palette base)
        lda     #$F0                    ; $1E=$F0 (palette range)
        sta     $1E
        lda     #$FF                    ; $1C=$FF (effect enable flag)
        sta     $1C
        rts

; --- active: process entity ---

code_B55B:  ora     #$08                ; set sprite flag bit 3 (invulnerable frame)
        sta     ent_flags,x
        jsr     code_8003               ; call swappable-bank entity update
        lda     ent_hp,x                 ; check HP
        beq     code_B54A               ; HP=0 → killed, deactivate

; --- hold on first animation frame (invisible pose) ---
        lda     ent_anim_state,x                 ; if anim frame > 0, skip hold
        bne     code_B579
        lda     ent_anim_frame,x                 ; if anim counter about to advance (=1),
        cmp     #$01                    ; reset to 0 to hold on frame 0
        bne     code_B579               ; (keeps Hologran invisible until triggered)
        lda     #$00                    ; freeze animation on invisible frame
        sta     ent_anim_frame,x

; --- timer-based phase control ---
code_B579:  lda     ent_timer,x             ; if visibility timer active,
        bne     code_B5C6               ; skip to countdown

; --- state 0: invisible, drifting horizontally ---
        lda     ent_facing,x                 ; move in facing direction
        and     #$01
        beq     code_B58B               ; bit 0 clear → move left
        jsr     LF71D                   ; move_sprite_right
        jmp     code_B58E

code_B58B:  jsr     LF73B               ; move_sprite_left
code_B58E:  lda     ent_status,x             ; check entity state
        and     #$0F
        bne     code_B5CB               ; already appeared → done
        jsr     LF8C2                   ; get X distance to player
        cmp     #$61                    ; if farther than $61 px,
        bcs     code_B5CB               ; too far → keep drifting

; --- player in range: appear and shoot ---
        inc     ent_status,x                 ; advance state (0 → 1: visible)
        inc     ent_anim_state,x                 ; advance anim frame (show visible pose)
        lda     #$3C                    ; visibility timer = 60 frames (1 second)
        sta     ent_timer,x
        lda     ent_routine,x                 ; odd main routine index:
        and     #$01                    ; set slow X drift speed $00.40
        beq     code_B5B8               ; even → skip speed change
        lda     #$40                    ; X speed sub = $40
        sta     ent_xvel_sub,x
        lda     #$00                    ; X speed whole = $00
        sta     ent_xvel,x                 ; (slow drift: 0.25 px/frame)
code_B5B8:  lda     #$FF                ; set palette flash / screen effect
        sta     $1C                     ; $1C=$FF, $1D=$10, $1E=$10
        lda     #$10                    ; (visual flash on appearance)
        sta     $1D
        sta     $1E
        lda     #$00                    ; clear palette cycle var
        sta     $95

; --- countdown visibility timer ---
code_B5C6:  dec     ent_timer,x             ; decrement timer; when 0
        bne     code_B5CB               ; entity returns to invisible drift
code_B5CB:  rts

; --- state 0: parachute descent ---
main_parasyu:

        lda     ent_status,x                 ; check entity state
        and     #$0F
        bne     code_B5FD               ; nonzero -> activated, skip init
        sta     ent_yvel_sub,x                 ; Y speed sub = 0
        lda     #$03                    ; Y speed whole = 3 (fall at 3.0 px/frame
        sta     ent_yvel,x                 ; with parachute)
        jsr     LF8C2                   ; X distance to player
        cmp     #$64                    ; if > 100 px away,
        bcs     code_B5FC               ; too far -> keep falling

; --- player close: detach parachute ---
        lda     $E4                     ; pseudo-random: add shift register bytes
        adc     $E5                     ; to get random index 0-3
        sta     $E5
        and     #$03
        tay
        lda     LB6E4,y                 ; load random delay timer from table
        sta     ent_var2,x                 ; ($22, $2A, $26, or $2E frames)
        inc     ent_status,x                 ; advance state -> 1 (activated)
        lda     ent_flags,x                 ; toggle sprite flag bit 2
        eor     #$04                    ; (visual change: chute detaching)
        sta     ent_flags,x
code_B5FC:  rts

; --- state 1+: activated (parachute detaching or detached) ---

code_B5FD:  lda     ent_var3,x             ; check phase flag
        bne     code_B61B               ; nonzero -> chute detached, swoop phase

; --- phase 1: chute still attached, slow fall with delay ---
        lda     ent_var2,x                 ; if delay timer > 0,
        beq     code_B60D               ; expired -> detach chute
        dec     ent_var2,x                 ; count down delay
        jmp     LF759                   ; fall slowly while waiting

; --- delay expired: detach parachute ---

code_B60D:  lda     #$4D                ; switch to detached sprite (OAM $4D)
        jsr     LF835                   ; reset_sprite_anim
        inc     ent_var3,x                 ; set phase flag -> 1 (detached)
        lda     #$10                    ; set fall timer = 16 frames
        sta     ent_var2,x                 ; (before first swoop)
        rts

; --- phase 2: chute detached, swoop pattern ---

code_B61B:  lda     ent_anim_state,x             ; clamp anim frame: if >= 2,
        cmp     #$02                    ; force frame 3 and reset counter
        bcc     code_B62C               ; (lock to falling sprite pose)
        lda     #$03                    ; set anim frame = 3
        sta     ent_anim_state,x
        lda     #$00                    ; reset anim counter
        sta     ent_anim_frame,x
code_B62C:  lda     ent_status,x             ; check state bit 1
        and     #$02                    ; (0=swooping, 1=gentle fall between swoops)
        beq     code_B636               ; bit 1 clear -> swoop phase
        jmp     code_B6BC               ; bit 1 set -> gentle fall phase

; --- swoop sub-phase: sine-like arc movement ---

code_B636:  lda     ent_timer,x             ; speed hold timer: if > 0,
        bne     code_B694               ; hold current speed -> skip to apply

; --- load next speed step from tables ---
        ldy     ent_var1,x                 ; swoop step index -> $B6D6 lookup
        lda     LB6D6,y                 ; get speed table sub-index
        asl     a                       ; *2 for 16-bit table offset
        tay
        lda     L8F4A,y                 ; load Y speed from table (16-bit)
        sta     ent_yvel_sub,x
        lda     L8F4B,y
        sta     ent_yvel,x
        lda     L8F6A,y                 ; load X speed from table (16-bit)
        sta     ent_xvel_sub,x
        lda     L8F6B,y
        sta     ent_xvel,x
        lda     ent_xvel,x                 ; if X speed whole is negative,
        bpl     code_B675               ; positive -> skip negate

; --- negate X speed (make absolute value for directional move) ---
        lda     ent_xvel_sub,x                 ; two's complement negate 16-bit
        eor     #$FF                    ; X speed sub
        clc
        adc     #$01
        sta     ent_xvel_sub,x
        lda     ent_xvel,x                 ; X speed whole
        eor     #$FF
        adc     #$00
        sta     ent_xvel,x
code_B675:  inc     ent_var1,x             ; advance swoop step
        lda     ent_var1,x
        cmp     #$07                    ; after 7 steps: swoop complete
        bne     code_B68F
        lda     ent_facing,x                 ; reverse horizontal direction
        eor     #$03
        sta     ent_facing,x
        inc     ent_status,x                 ; advance state (set bit 1 -> fall phase)
        lda     #$00                    ; reset swoop step for next cycle
        sta     ent_var1,x
code_B68F:  lda     #$05                ; hold each speed step for 5 frames
        sta     ent_timer,x

; --- apply current speed to position ---
code_B694:  dec     ent_timer,x             ; decrement speed hold timer
        lda     ent_y_sub,x                 ; add Y speed to Y position (16-bit)
        clc
        adc     ent_yvel_sub,x
        sta     ent_y_sub,x
        lda     ent_y_px,x
        adc     ent_yvel,x
        sta     ent_y_px,x
        lda     ent_facing,x                 ; move horizontally by X speed
        and     #$02                    ; bit 1: 0=right, 1=left
        bne     code_B6B8
        jsr     LF71D                   ; move_sprite_right
        bcs     code_B6BB               ; (unconditional branch pair)
        bcc     code_B6BB
code_B6B8:  jsr     LF73B               ; move_sprite_left
code_B6BB:  rts

; --- gentle fall phase (between swoops) ---

code_B6BC:  dec     ent_var2,x             ; count down fall timer
        bne     code_B6C9               ; not expired -> keep falling
        dec     ent_status,x                 ; clear state bit 1 -> back to swoop
        lda     #$10                    ; reset fall timer = 16 frames
        sta     ent_var2,x
code_B6C9:  lda     #$80                ; Y speed = $00.80 (0.5 px/frame)
        sta     ent_yvel_sub,x                 ; gentle downward drift
        lda     #$00
        sta     ent_yvel,x
        jmp     LF759                   ; apply downward movement

; parasyu data tables
; $B6D6: swoop speed sub-indices (7 steps per swoop, 2 swoops = 14 entries)

LB6D6:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F
LB6E4:  .byte   $22,$2A,$26,$2E

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
        lda     ent_status,x
        and     #$0F
        beq     code_B752
        lda     boss_hp_display
        cmp     #$9C
        bne     code_B751
        lda     ent_status,x
        ora     #$40
        sta     ent_status,x
        stx     L0000
        lda     ent_routine,x
        and     #$07                    ; fetch doc robot master's
        tay                             ; main routine index
        lda     doc_robot_master_main_indices,y ; morph this sprite into it
        sta     ent_routine,x
        lda     #$CA                    ; doc robot hitbox
        sta     ent_hitbox,x
        lda     #$1C                    ; 28 HP
        sta     ent_hp,x
        lda     LB843,y                 ; master-specific X velocity (sub)
        sta     ent_xvel_sub,x
        lda     LB84B,y                 ; master-specific X velocity (whole)
        sta     ent_xvel,x
        jsr     LF81B                   ; reset_gravity
        lda     LB81B,y                 ; CHR bank set for this master
        sta     $ED
        jsr     LFF3C                   ; update_CHR_banks
        lda     #$C0                    ; set status: active + invincible
        sta     ent_status,x
        tya
        asl     a
        asl     a
        tay
        ldx     #$00
code_B73C:  lda     LB823,y
        sta     $061C,x
        sta     $063C,x
        iny
        inx
        cpx     #$04
        bne     code_B73C
        lda     #$FF
        sta     palette_dirty
        ldx     L0000
code_B751:  rts

; --- state 0: spawn shutter and set up doc robot ---
code_B752:  jsr     init_boss_wait          ; freeze player, start HP bar fill
        inc     ent_status,x
        jsr     LFC53                   ; find_enemy_freeslot_y
        lda     #$61
        sta     ent_routine,y
        lda     ent_y_px,x
        sta     ent_var1,y
        lda     ent_x_px,x
        sta     ent_x_px,y
        lda     ent_x_scr,x
        sta     ent_x_scr,y
        lda     #$10
        sta     ent_y_px,y
        lda     #$80
        sta     ent_yvel_sub,y
        sta     ent_hitbox,y
        lda     #$00
        sta     ent_yvel,y
        stx     L0000
        lda     ent_routine,x
        and     #$07
        tax
        lda     LB7F3,x
        ldx     L0000
        jsr     LF846                   ; init_child_entity
        lda     ent_routine,x
        and     #$07
        tay
        lda     LB803,y
        sta     $ED
        tya
        asl     a
        tay
        lda     #$0F
        sta     $061C
        sta     $061D
        sta     $063C
        sta     $063D
        lda     LB80B,y
        sta     $061E
        sta     $063E
        lda     LB80C,y
        sta     $061F
        sta     $063F
        lda     #$FF
        sta     palette_dirty
        jmp     LFF3C                   ; update_CHR_banks

        jsr     LF759                   ; move_sprite_down
        lda     ent_y_px,x
        cmp     ent_var1,x
        beq     code_B7D9
        lda     #$80
        sta     boss_hp_display
        rts

code_B7D9:  lda     #$00
        sta     ent_status,x
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
        lda     #$0C                    ; SFX $0C = boss intro music
        jsr     LF898                   ; submit_sound_ID_D9
        rts

LB7F3:  .byte   $16,$1A,$14,$18,$15,$13,$19,$17

; doc robot AI indices, as opposed to the intro docs
; Flash, Bubble, Quick, Wood, Crash, Air, Metal, Heat
doc_robot_master_main_indices:  .byte   $A0,$B0,$B2,$A1,$A2,$B3,$A3,$B1
LB803:  .byte   $23,$10,$23,$23,$23,$23,$10,$23
LB80B:  .byte   $30
LB80C:  .byte   $11,$30,$19,$27,$15,$37,$17,$30
        .byte   $26,$27,$11,$27,$15,$27,$15
LB81B:  .byte   $21,$22,$21,$22,$21,$22,$21,$22
LB823:  .byte   $0F,$30,$15,$27,$0F,$0F,$30,$19
        .byte   $0F,$0F,$27,$15,$0F,$0F,$30,$19
        .byte   $0F,$0F,$30,$26,$0F,$0F,$2C,$11
        .byte   $0F,$0F,$27,$15,$0F,$0F,$27,$15
LB843:  .byte   $00,$00,$00,$B3,$4C,$00,$80,$00
LB84B:  .byte   $01,$00,$00,$01,$01,$00,$02,$04

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
        bne     code_B860
        inc     ent_status,x
        jsr     init_boss_wait                  ; freeze player, start HP bar fill
; --- falling / waiting for HP bar to fill ---
code_B860:  lda     ent_y_px,x          ; still above landing Y ($80)?
        cmp     #$80
        bcs     code_B86D               ; no → apply gravity with floor
        jsr     LF797                   ; apply_y_speed (still falling)
        jmp     code_B8BD

code_B86D:  lda     ent_routine,x       ; look up floor tile for this master
        and     #$07
        tay
        lda     LB8EF,y                 ; gravity floor offset per master
        tay
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_B8BD               ; not landed yet
        lda     ent_routine,x           ; check if landing animation done
        and     #$07
        tay
        lda     LB8E7,y                 ; target anim_state for this master
        cmp     ent_anim_state,x
        bne     code_B8C2               ; not yet → keep boss HP bar filling
        lda     #$00
        sta     ent_anim_frame,x
        lda     boss_hp_display
        cmp     #$9C                    ; HP bar fully filled?
        bne     code_B8C6               ; no → wait
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
        lda     LB8CF,y
        sta     ent_xvel_sub,x
        lda     LB8D7,y
        sta     ent_xvel,x
        lda     LB8DF,y
        jmp     LF835                   ; reset_sprite_anim

code_B8BD:  lda     #$00
        sta     ent_anim_frame,x
code_B8C2:  lda     #$80
        sta     boss_hp_display
code_B8C6:  rts

; Needle, Magnet, Gemini, Hard, Top, Snake, Spark, Shadow
robot_master_main_indices:  .byte   $C0,$C1,$D6,$D0,$C2,$D4,$D2,$C3
LB8CF:  .byte   $B3,$00,$2D,$00,$00,$4C,$6D,$00 ; X velocity (sub) per master
LB8D7:  .byte   $01,$00,$03,$01,$04,$01,$01,$04 ; X velocity (whole) per master
LB8DF:  .byte   $29,$1F,$33,$2C,$49,$22,$36,$3F ; initial anim frame per master
LB8E7:  .byte   $04,$03,$05,$06,$02,$02,$08,$03 ; target anim_state per master
LB8EF:  .byte   $1E,$1E,$00,$26,$1E,$00,$1E,$1E ; gravity floor offset per master
        .byte   $60

; --- main_spinning_wheel — Shadow Man stage conveyor wheel ---
; Checks player collision, then scrolls camera horizontally based on
; the OAM ID's low bit (left/right wheel direction).
main_spinning_wheel:
        lda     ent_y_px,x
        pha
        dec     ent_y_px,x
        jsr     LFAE2                   ; check_player_collision
        pla
        sta     ent_y_px,x
        bcs     code_B92A
        lda     ent_anim_id,x
        and     #$01
        tay
        lda     ent_x_sub
        clc
        adc     LB92B,y
        sta     ent_x_sub
        lda     ent_x_px
        adc     LB92D,y
        sta     ent_x_px
        lda     ent_x_scr
        adc     LB92F,y
        sta     ent_x_scr
code_B92A:  rts

LB92B:  .byte   $80,$80
LB92D:  .byte   $00,$FF
LB92F:  .byte   $00,$FF

; --- main_trap_platform — Shadow Man stage trap platform ---
; Triggers when player is close (< $15 Y, < $18 X). Plays open animation,
; then closes after a delay. Toggles sprite flag bit 0 on completion.
main_trap_platform:
        lda     ent_status,x
        and     #$0F
        bne     code_B964
        sta     ent_anim_state,x
        sta     ent_anim_frame,x
        sta     ent_var1,x
        jsr     LF8B3                   ; entity_y_dist_to_player
        cmp     #$15
        bcs     code_B963
        lda     player_state
        bne     code_B963
        lda     ent_y_px,x
        cmp     ent_y_px
        bcc     code_B963
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$18
        bcs     code_B963
        lda     #$0C
        sta     ent_timer,x
        inc     ent_status,x
code_B963:  rts

code_B964:  lda     ent_var1,x
        bne     code_B979
        dec     ent_timer,x
        bne     code_B963
        inc     ent_var1,x
        lda     ent_flags,x
        and     #$90
        sta     ent_flags,x
code_B979:  lda     ent_status,x
        and     #$02
        bne     code_B98F
        lda     ent_anim_state,x
        cmp     #$04
        bne     code_B963
        inc     ent_status,x
        lda     #$14
        sta     ent_timer,x
code_B98F:  lda     #$04
        sta     ent_anim_state,x
        lda     #$00
        sta     ent_anim_frame,x
        dec     ent_timer,x
        bne     code_B963
        lda     ent_flags,x
        eor     #$01
        sta     ent_flags,x
        lda     #$80
        sta     ent_status,x
        rts

; --- main_breakable_wall — breakable wall segment (Hard Knuckle target) ---
; Checks weapon slots $01-$02 for Hard Knuckle ($AC) or Shadow Blade ($AF).
; If hit, plays explosion and changes AI routine to $19 (debris).
main_breakable_wall:

        ldy     #$01
code_B9AE:  lda     ent_status,y
        bpl     code_B9DF
        lda     ent_anim_id,y
        cmp     #$AC
        beq     code_B9BE
        cmp     #$AF
        bne     code_B9DF
code_B9BE:  jsr     LFB7B               ; check_sprite_weapon_collision
        bcs     code_B9DF
        lda     #$18
        jsr     LF89A                   ; submit_sound_ID
        ldy     $10
        lda     #$00
        sta     ent_status,y
        lda     #$71
        jsr     LF835                   ; reset_sprite_anim
        lda     #$00
        sta     ent_timer,x
        lda     #$19
        sta     ent_routine,x
        rts

code_B9DF:  iny
        cpy     #$03
        bcc     code_B9AE
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

        lda     ent_status,x
        and     #$0F
        bne     code_BA0B
; --- state 0: idle, waiting for player proximity ---
        sta     ent_yvel_sub,x          ; init Y speed = $01.00
        lda     #$01
        sta     ent_yvel,x
        lda     ent_y_px,x
        sta     ent_var3,x              ; save home Y position
        jsr     LF8B3                   ; entity_y_dist_to_player
        cmp     #$15                    ; within $15 pixels Y?
        bcs     code_BA41
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$0A                    ; within $0A pixels X?
        bcs     code_BA41
        inc     ent_status,x            ; player close enough → start rising
; --- state 1: rising upward ---
code_BA0B:  lda     ent_timer,x
        bne     code_BA42               ; timer set → returning phase
        jsr     LF779                   ; move_sprite_up
        lda     ent_yvel_sub,x          ; accelerate upward (+$10 sub each frame)
        clc
        adc     #$10
        sta     ent_yvel_sub,x
        lda     ent_yvel,x
        adc     #$00
        sta     ent_yvel,x
        cmp     #$03                    ; cap speed at $03.00
        bne     code_BA2D
        lda     #$00
        sta     ent_yvel_sub,x
code_BA2D:  lda     ent_y_px,x
        cmp     #$3A                    ; reached top of screen?
        bcs     code_BA41
        inc     ent_timer,x             ; mark: time to return
        lda     #$00                    ; reset speed to $01.00 for descent
        sta     ent_yvel_sub,x
        lda     #$01
        sta     ent_yvel,x
code_BA41:  rts

; --- state 1+: returning to home position ---
code_BA42:  lda     ent_status,x
        and     #$02
        bne     code_BA57               ; state 2+ → check player again
        jsr     code_B2AF               ; move downward (return to home)
        lda     ent_y_px,x
        cmp     ent_var3,x              ; reached home Y?
        bcc     code_BA41               ; not yet
        inc     ent_status,x            ; advance to state 2
code_BA57:  jsr     LF8B3               ; entity_y_dist_to_player
        cmp     #$16
        bcs     code_BA41
        jsr     LF8C2                   ; entity_x_dist_to_player
        cmp     #$09
        bcs     code_BA41
        dec     ent_status,x
        dec     ent_timer,x
        rts

; ===========================================================================
; main_big_snakey — Big Snakey (Snake Man stage mini-boss)
; ===========================================================================
; Large snake entity. State 0: picks random shot count (2-4) from table,
; sets $78 frame timer. State 1: when timer expires, opens mouth (animation
; state |= $01), spawns homing bullet child (routine $8F) using
; calc_homing_velocity (LFC63). After all shots fired, returns to state 0.
; On death (HP=0): despawns all child projectiles below Y < $80, sets $55
; to $80 (screen shake / boss defeated flag).
main_big_snakey:

; --- state 0: init, pick random shot count ---
        lda     ent_status,x
        and     #$0F
        bne     code_BA8C
        inc     ent_status,x
        lda     $E4                     ; RNG: $E4 += $E6
        adc     $E6
        sta     $E4
        and     #$03                    ; 0-3 index
        tay
        lda     LBB51,y                 ; shot count: 2, 3, 4, or 2
        sta     ent_var1,x
        lda     #$78                    ; 120-frame delay before firing
        sta     ent_timer,x
        bne     code_BB01
; --- state 1: firing phase ---
code_BA8C:  lda     ent_timer,x
        bne     code_BAFE              ; timer not expired → wait
        lda     ent_anim_state,x       ; open mouth (anim |= $01)
        ora     #$01
        sta     ent_anim_state,x
        lda     ent_var2,x             ; inter-shot delay active?
        bne     code_BAF8              ; yes → count down
; --- spawn homing bullet child ---
        jsr     LFC53                   ; find_enemy_freeslot_y
        lda     #$BA                    ; child anim ID
        jsr     LF846                   ; init_child_entity
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
        jsr     LFC63                   ; calc_homing_velocity
        ldy     $0F
        ldx     $0E
        lda     $0C                     ; homing result → child facing
        sta     ent_facing,y
        dec     ent_var1,x              ; shots remaining--
        beq     code_BAEE              ; all shots fired → close mouth
        lda     #$12                    ; 18-frame delay between shots
        sta     ent_var2,x
        bne     code_BB01
code_BAEE:  lda     #$00               ; close mouth (clear anim bit)
        sta     ent_anim_state,x
        dec     ent_status,x            ; return to state 0
        bne     code_BB01
code_BAF8:  dec     ent_var2,x          ; inter-shot cooldown
        jmp     code_BB01

code_BAFE:  dec     ent_timer,x         ; pre-fire delay countdown
; --- common: run bank $1C collision + check death ---
code_BB01:  lda     ent_anim_id,x       ; preserve anim_id across $8003 call
        pha
        jsr     code_8003               ; bank $1C collision handler
        pla
        sta     ent_anim_id,x
        lda     ent_hp,x
        bne     code_BB2E              ; still alive → skip
; --- death: despawn all child projectiles ---
        sta     ent_status,x            ; despawn self
        ldy     #$0F
code_BB16:  lda     $0310,y             ; check enemy slot $10+Y
        bpl     code_BB27              ; not active → skip
        lda     $03D0,y                 ; child Y position
        cmp     #$80                    ; below midscreen?
        bcs     code_BB27              ; yes → don't despawn (offscreen)
        lda     #$00
        sta     $0310,y                 ; despawn child
code_BB27:  dey
        bpl     code_BB16
        lda     #$80
        sta     $55                     ; boss defeated flag / screen shake
code_BB2E:  lda     #$00
        sta     ent_anim_frame,x
        rts

        lda     ent_facing,x
        and     #$01
        beq     code_BB41
        jsr     LF71D                   ; move_sprite_right
        jmp     code_BB44

code_BB41:  jsr     LF73B               ; move_sprite_left
code_BB44:  lda     ent_facing,x
        and     #$08
        beq     code_BB4E
        jmp     LF779                   ; move_sprite_up

code_BB4E:  jmp     LF759               ; move_sprite_down

LBB51:  .byte   $03,$03,$04,$02         ; shot count table (3, 3, 4, 2)

; --- init_tama — shared Tama initialization / floor clamp ---
init_tama:
        lda     ent_anim_id,x
        beq     code_BBBB
        jsr     LF797                   ; apply_y_speed
        ldy     #$00
        sty     $54
        lda     ent_x_scr,x
        cmp     #$0B
        beq     code_BB69
        iny
code_BB69:  lda     LBBBC,y
        cmp     ent_y_px,x
        bcs     code_BBB6
        sta     ent_y_px,x
        lda     ent_anim_frame,x
        cmp     #$02
        bne     code_BBBB
        lda     ent_anim_state,x
        cmp     #$02
        bne     code_BBBB
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
code_BBA3:  lda     ent_status,y
        bpl     code_BBB0
        lda     ent_flags,y
        and     #$FB
        sta     ent_flags,y
code_BBB0:  dey
        cpy     #$0F
        bne     code_BBA3
        rts

code_BBB6:  lda     #$00
        sta     ent_anim_frame,x
code_BBBB:  rts

LBBBC:  .byte   $48,$78

; ===========================================================================
; main_tama_A — Tama (giant cat, Snake Man stage boss support, variant A)
; ===========================================================================
; Tama variant A. Checks if disabled (flags bit 2). Preserves anim_id across
; bank $1C collision check ($8003). On death (HP=0): clears all enemy slots,
; switches to item-drop routine $63. When alive: state 0 waits for all $CF/$D0
; child projectiles to despawn (checks $54 flag). State 1: spawns kitten
; projectiles ($CF) with homing velocity, then returns to state 0 after 2
; volleys. Includes floor-level clamping from LBBBC table ($48/$78).
main_tama_A:
        lda     ent_flags,x
        and     #$04
        bne     code_BBBB
        lda     ent_anim_id,x
        pha
        jsr     code_8003
        pla
        sta     ent_anim_id,x
        lda     ent_hp,x
        bne     code_BBF5
        sta     ent_var1,x
        sta     ent_hitbox,x
        lda     #$04
        sta     ent_var2,x
        lda     #$63
        sta     ent_routine,x
        ldy     #$0F
        lda     #$00
code_BBE9:  sta     $0310,y
        dey
        bpl     code_BBE9
        lda     #$80
        sta     ent_status,x
        rts

code_BBF5:  lda     ent_status,x
        and     #$0F
        bne     code_BC1E
        sta     ent_anim_frame,x
        ldy     #$1F
code_BC01:  lda     ent_status,y
        bpl     code_BC11
        lda     ent_anim_id,y
        cmp     #$CF
        beq     code_BC1D
        cmp     #$D0
        beq     code_BC1D
code_BC11:  dey
        cpy     #$0F
        bne     code_BC01
        lda     $54
        bne     code_BC1D
        inc     ent_status,x
code_BC1D:  rts

code_BC1E:  lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_BC44
        inc     ent_var1,x
        lda     ent_var1,x
        cmp     #$02
        bne     code_BC3E
        dec     ent_status,x
        lda     #$00
        sta     ent_timer,x
        sta     ent_var1,x
        inc     $54
        rts

code_BC3E:  lda     #$3C
        sta     ent_timer,x
        rts

code_BC44:  lda     ent_timer,x
        beq     code_BC52
        dec     ent_timer,x
        lda     #$00
        sta     ent_anim_frame,x
        rts

code_BC52:  lda     ent_anim_frame,x
        bne     code_BCA6
        lda     ent_anim_state,x
        cmp     #$02
        bne     code_BCA6
        jsr     LFC53                   ; find_enemy_freeslot_y
        bcs     code_BCA6
        lda     #$CF
        jsr     LF846                   ; init_child_entity
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
        jsr     LF869                   ; face_player
        lda     ent_facing,x
        sta     ent_facing,y
code_BCA6:  rts

code_BCA7:  rts

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
        bne     code_BCA7
        lda     ent_status,x
        and     #$0F
        bne     code_BCD8
        sta     ent_anim_frame,x
        ldy     #$1F
code_BCBB:  lda     ent_status,y
        bpl     code_BCCB
        lda     ent_anim_id,y
        cmp     #$CF
        beq     code_BCD7
        cmp     #$D0
        beq     code_BCD7
code_BCCB:  dey
        cpy     #$0F
        bne     code_BCBB
        lda     $54
        beq     code_BCD7
        inc     ent_status,x
code_BCD7:  rts

code_BCD8:  lda     ent_anim_frame,x
        ora     ent_anim_state,x
        bne     code_BCE3
        dec     ent_status,x
code_BCE3:  lda     ent_anim_state,x
        cmp     #$02
        bne     code_BCD7
        lda     ent_anim_frame,x
        bne     code_BCD7
        lda     #$02
        sta     $10
        jsr     LF869                   ; face_player
code_BCF6:  jsr     LFC53               ; find_enemy_freeslot_y
        bcs     code_BD58
        lda     #$D0
        jsr     LF846                   ; init_child_entity
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
        lda     LBD59,x
        sta     ent_yvel_sub,y
        lda     LBD5C,x
        sta     ent_yvel,y
        lda     LBD5F,x
        sta     ent_xvel_sub,y
        lda     LBD62,x
        sta     ent_xvel,y
        lda     #$1E
        sta     ent_timer,y
        sta     ent_var1,y
        ldx     L0000
        dec     $10
        bpl     code_BCF6
        lda     #$00
        sta     $54
code_BD58:  rts

LBD59:  .byte   $44,$00,$2A
LBD5C:  .byte   $03,$04,$05
LBD5F:  .byte   $39,$55,$8C
LBD62:  .byte   $01,$01,$01
        ldy     #$08
        jsr     LF67C                   ; move_vertical_gravity
        bcc     code_BD76
        lda     #$44
        sta     ent_yvel_sub,x
        lda     #$03
        sta     ent_yvel,x
code_BD76:  lda     ent_facing,x
        and     #$01
        beq     code_BD85
        ldy     #$08
        jsr     LF580                   ; move_right_collide
        jmp     code_BD8A

code_BD85:  ldy     #$09
        jsr     LF5C4                   ; move_left_collide
code_BD8A:  bcc     code_BD94
        lda     ent_facing,x
        eor     #$03
        sta     ent_facing,x
code_BD94:  rts

        lda     ent_timer,x
        beq     code_BDAD
        dec     ent_timer,x
        jsr     LF797                   ; apply_y_speed
        lda     ent_facing,x
        and     #$01
        beq     code_BDAA
        jmp     LF71D                   ; move_sprite_right

code_BDAA:  jmp     LF73B               ; move_sprite_left

code_BDAD:  ldy     #$12
        jsr     LF67C                   ; move_vertical_gravity
        lda     #$01
        sta     ent_anim_state,x
        bcc     code_BDDF
        lda     #$00
        sta     ent_anim_state,x
        dec     ent_var1,x
        bne     code_BDF3
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
        jsr     LF869                   ; face_player
code_BDDF:  lda     ent_facing,x
        and     #$01
        beq     code_BDEE
LBDE6:  ldy     #$1E
        jsr     LF580                   ; move_right_collide
        .byte   $4C
LBDEC:  .byte   $F3
        .byte   $BD
code_BDEE:  ldy     #$1F
        jsr     LF5C4                   ; move_left_collide
code_BDF3:  lda     #$00
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
        bne     code_BDFF
        ldy     #$2D                    ; large pickup hitbox
code_BDFF:  jsr     LF67C               ; apply $99
        jsr     LFAE2                   ; check if player touches item
        bcs     code_BE3A               ; no collision → timer logic

; --- player picked up item ---
        lda     ent_timer,x                 ; if despawn timer set, skip
        bne     code_BE25               ; respawn-table marking
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
code_BE25:  lda     #$00                ; despawn pickup entity
        sta     ent_status,x
        ldy     ent_routine,x                 ; dispatch to item type handler
        lda     LBDE6,y                 ; via pointer table indexed by
        sta     L0000                   ; AI routine (ent_routine)
        lda     LBDEC,y                 ; loads pickup-effect routine address
        sta     $01
        jmp     (L0000)                 ; indirect jump to effect handler

; --- no collision: handle despawn timer ---

code_BE3A:  lda     ent_timer,x             ; despawn timer active?
        beq     code_BE49               ; no → return
        dec     ent_timer,x                 ; decrement; expired?
        bne     code_BE49               ; no → return
        lda     #$00                    ; timer expired: despawn uncollected item
        sta     ent_status,x
code_BE49:  rts

; pickup handler pointer table (lo bytes, indexed by ent_routine)

        .byte   $56,$5C,$62,$66,$9D,$AB,$BE,$BE
        .byte   $BE,$BE,$BE,$BE

; --- pickup_hp_large: restore 10 HP to Mega Man's weapon (buster=slot 0) ---
        lda     #$0A                    ; amount = 10 energy ticks
        ldy     #$00                    ; weapon index = 0 (HP)
        beq     code_BE6C

; --- pickup_hp_small: restore 2 HP ---
        lda     #$02                    ; amount = 2
        ldy     #$00                    ; weapon = 0 (HP)
        beq     code_BE6C

; --- pickup_ammo_large: restore 10 ammo to current weapon ---
        lda     #$0A                    ; amount = 10
        bne     code_BE68

; --- pickup_ammo_small: restore 2 ammo ---
        lda     #$02
code_BE68:  ldy     current_weapon                 ; Y = current weapon ID ($A0)
        beq     code_BE98               ; weapon 0 (buster) has no ammo → skip

; --- apply_energy_refill: A=amount, Y=weapon slot index ---
code_BE6C:  inc     $58                 ; flag: energy refill active
        sta     $0F                     ; remaining ticks to add
        sty     $0E                     ; target weapon/HP slot
code_BE72:  ldy     $0E                 ; check current energy level
        lda     player_hp,y                   ; ($A2+Y: $A2=HP, $A3+=weapon ammo)
        cmp     #$9C                    ; $9C = max energy (28 units)
        beq     code_BE98               ; already full → done
        lda     player_hp,y                   ; add 1 tick of energy
        clc
        adc     #$01
        sta     player_hp,y
        lda     #$1C                    ; play refill tick sound
        jsr     LF89A                   ; submit_sound_ID
        dec     $0F                     ; all ticks applied?
        beq     code_BE98               ; yes → done
code_BE8D:  jsr     LFD6E               ; wait 4 frames between ticks
        lda     $95                     ; (frame counter & 3 == 0)
        and     #$03
        bne     code_BE8D
        beq     code_BE72               ; → next tick
code_BE98:  lda     #$00                ; clear refill-active flag
        sta     $58
        rts

; --- pickup_etank: add 1 E-tank (max 9) ---

        lda     #$14                    ; play 1-up/E-tank sound
        jsr     LF89A                   ; submit_sound_ID
        lda     etanks                     ; current E-tanks ($AF)
        cmp     #$09                    ; max 9?
        beq     code_BEAA               ; yes → don't add more
        inc     etanks                     ; E-tanks += 1
code_BEAA:  rts

; --- pickup_1up: add 1 extra life (BCD, max 99) ---

        lda     #$14                    ; play 1-up sound
        jsr     LF89A                   ; submit_sound_ID
        lda     lives                     ; $AE ($AE, BCD format)
        cmp     #$99                    ; max 99?
        beq     code_BED1               ; yes → done
        inc     lives                     ; $AE += 1
        lda     lives                     ; BCD fixup: if low nibble >= $A
        and     #$0F                    ; carry into high nibble
        cmp     #$0A
        bne     code_BED1
        lda     lives                     ; add $10 (next tens digit)
        and     #$F0                    ; clear low nibble
        clc
        adc     #$10
        sta     lives
        cmp     #$A0                    ; overflow past 99? clamp to 99
        bne     code_BED1
        lda     #$99
        sta     lives
code_BED1:  rts

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
        beq     code_BF03               ; yes → handle item spawn
        jsr     LFB7B                   ; check_sprite_weapon_collision
        bcs     code_BED1               ; no hit → return
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
        ora     L0000
        sta     $0150,y
        ldy     $10                     ; despawn the weapon that hit
        lda     #$00
        sta     ent_status,y
        lda     #$71                    ; play break animation
        jmp     LF835                   ; reset_sprite_anim

; --- break animation done: spawn random item ---
code_BF03:  lda     ent_anim_state,x
        cmp     #$04                    ; break anim finished?
        bne     code_BF3E               ; no → wait
        lda     $E5                     ; RNG: $E5 += $E6
        adc     $E6
        sta     $E5
        sta     L0000
        lda     #$64                    ; divide by 100
        sta     $01
        jsr     LFCEB                   ; divide_8bit (remainder in $03)
        ldy     #$05                    ; scan probability thresholds
        lda     $03
code_BF1D:  cmp     LBF3F,y             ; weighted probability table
        bcc     code_BF25
        dey
        bne     code_BF1D
code_BF25:  lda     LBF45,y             ; item anim ID for selected item
        jsr     LF835                   ; reset_sprite_anim
        lda     LBF4B,y                 ; item AI routine
        sta     ent_routine,x
        lda     ent_flags,x             ; clear low 2 flag bits
        and     #$FC
        sta     ent_flags,x
        lda     #$F0                    ; 240-frame despawn timer
        sta     ent_timer,x
code_BF3E:  .byte   $60
; surprise box data tables
LBF3F:  .byte   $63,$41,$23,$19,$0F,$05 ; probability thresholds (99,65,35,25,15,5)
LBF45:  .byte   $FB,$F9,$FA,$FC,$FE,$FD ; item anim IDs
LBF4B:  .byte   $66,$64,$65             ; item AI routine IDs
        .byte   $67
        adc     #$68
        lda     ent_anim_state,x
        cmp     #$04
        bne     code_BF96
        lda     boss_active
        bmi     code_BF77
        lda     $E6
        adc     $E7
        sta     $E7
        sta     L0000
        lda     #$64
        sta     $01
        jsr     LFCEB                   ; divide_8bit
        ldy     #$04
        lda     $03
code_BF6F:  cmp     LBF97,y
        bcc     code_BF7D
        dey
        bpl     code_BF6F
code_BF77:  lda     #$00
        sta     ent_status,x
        rts

code_BF7D:  lda     LBF9C,y
        jsr     LF835                   ; reset_sprite_anim
        lda     LBFA1,y
        sta     ent_routine,x
        lda     #$F0
        sta     ent_timer,x
        lda     #$00
        sta     ent_yvel_sub,x
        sta     ent_yvel,x
code_BF96:  .byte   $60
LBF97:  .byte   $1D,$1B,$0C,$0A,$01
LBF9C:  .byte   $FB,$FC,$F9,$FA,$FE
LBFA1:  .byte   $66,$67,$64,$65,$69

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
        .byte   $54,$A1,$00,$AD,$01,$3A,$00,$DE
        .byte   $04,$82,$51,$90
        brk
        asl     $05,x
