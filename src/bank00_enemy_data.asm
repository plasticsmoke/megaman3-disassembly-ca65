; =============================================================================
; MEGA MAN 3 (U) — BANK $00 — GLOBAL ENEMY DATA / NEEDLE MAN STAGE
; =============================================================================
; Mapped to $A000-$BFFF. This bank serves a dual purpose:
;
; 1) GLOBAL ENEMY PROPERTY TABLES ($A000-$A70F)
;    Six 256-byte tables indexed by "global enemy ID" (the ID stored in
;    the enemy spawn tables at $AE00,y in each stage bank). Only the first
;    $90 entries are meaningful; bytes $90-$FF in each table are unused
;    padding that overlaps with Needle Man stage data via bitpacking.
;
;    $A000: enemy_flags_g       — entity init flags (written to ent_flags)
;    $A100: enemy_main_ID_g     — AI routine index for bank1C_1D dispatch
;    $A200: enemy_shape_g       — hitbox shape ID (bit 7 = deals contact damage)
;    $A300: enemy_OAM_ID_g      — OAM animation ID (written to ent_anim_id)
;    $A400: enemy_health_g      — starting HP (written to ent_hp; $FF = invincible)
;    $A500: enemy_speed_ID_g    — index into X velocity lookup tables below
;
;    Two small lookup tables indexed by speed ID ($00-$0D, max $18):
;    $A600: enemy_x_velocity_sub_g — X velocity subpixel component
;    $A700: enemy_x_velocity_g     — X velocity whole pixel component
;
; 2) NEEDLE MAN STAGE DATA ($A800-$BFFF)
;    Stage layout data for Needle Man (stage_id $22 = bank $00).
;    Standard MM3 stage bank format:
;    $A800-$A9FF: boss AI local data (read by bank $06 Needle Man routines)
;    $AA00-$AA12: screen metatile column grid (column IDs per screen page)
;    $AA13-$AA5F: room pointer table entries (CHR/palette param + layout index)
;    $AA60-$AA81: room table (2 bytes/room) + palette indices at $AA80-$AA81
;    $AA82-$AAFF: screen layout data (20 bytes/entry: 16 column IDs + 4 connections)
;    $AB00-$ABFF: enemy spawn table: screen number (terminated by $FF)
;    $AC00-$ACFF: enemy spawn table: X pixel position
;    $AD00-$ADFF: enemy spawn table: Y pixel position
;    $AE00-$AEFF: enemy spawn table: global enemy ID (indexes tables at $A000+)
;    $AF00-$B6FF: metatile column definitions (64 bytes per column ID)
;    $B700-$BAFF: metatile CHR tile definitions (4 bytes per metatile: 2x2 tiles)
;    $BB00-$BEFF: metatile nametable data (4 quadrants, each with padding)
;    $BF00-$BFFF: tile collision attribute table (1 byte per metatile index)
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"


.segment "BANK00"

; =============================================================================
; GLOBAL ENEMY PROPERTY TABLES ($A000-$A70F)
; =============================================================================
; Each table is 256 bytes. Indexed by global enemy ID ($00-$8F valid).
; The engine loads bank $00 explicitly during entity spawning (bank0F) to
; read these properties, then switches back to the stage bank.
; =============================================================================


; ===========================================================================
; enemy_flags_g ($A000) — entity initialization flags
; ===========================================================================
; Written to ent_flags on spawn. Bit meanings (from rendering engine):
;   bit 7 ($80): entity active / drawn
;   bit 6 (ENT_FLAG_HFLIP): horizontal flip
;   bit 5 ($20): on-ladder (behind-background OAM priority)
;   bit 4 ($10): uses world coordinates (subtract camera for screen pos)
;   bit 3 ($08): wide offscreen margin (keep alive within 72px of edge)
;   bit 2 ($04): invisible (skip OAM rendering)
;   bit 1 ($02): unknown (set by spawn table, no known consumer)
;   bit 0 ($01): unknown (set by spawn table, no known consumer)
;
; --- enemy ID index (confirmed via dispatch table + Mesen): ---
;   $00=Dada            $01=Potton          $02=New Shotman     $03=Hammer Joe
;   $04=Bubukan         $05=Jamacy          $06=Bomb Flier      $07=(projectile)
;   $08=Yambow          $09=Metall DX       $0A=Cannon          $0B=Cloud Platform
;   $0C=Giant Metall Met $0D=Giant Metall Met $0E=Gyoraibo       $0F=Mag Fly
;   $10=Block Breaker   $11=Junk Golem      $12=Pickelman Bull  $13=Bikky
;   $14=Giant Metall    $15=Jamacy          $16=Mag Force       $17=Junk Block Thrown
;   $18=Nitron          $19=Pole            $1A=Gyoraibo        $1B=Hari Harry
;   $1C=Penpen Maker    $1D=Returning Monking $1E=Block Breaker $1F=Have 'Su' Bee
;   $20=Bolton & Nutton $21=Wanaan          $22=Needle Press    $23=Needle Press
;   $24=Elec'n          $25=Magnet Pull     $26=Mechakkero      $27=Top Man Platform
;   $28=(no-op)         $29=(no-op)         $2A=Chibee          $2B=Block Breaker
;   $2C=Penpen          $2D=Electric Gabyoall $2E=(no-op)       $2F=Block Breaker
;   $30=Block Breaker   $31=(no-op)         $32=Pole            $33=Holograph
;   $34=Needle Press    $35=(no-op)         $36=Peterchy        $37=Walking Bomb
;   $38=Parasyu         $39=Hologran        $3A=Hologran        $3B=Bomber Pepe
;   $3C=Metall DX (walk) $3D=Magnet Push    $3E=Proto Man       $3F=(no-op)
;   $40-$4B=Doc Robot entries               $5C=Giant Springer
;   $50-$56=Robot Master intros             $62=Komasaburo
;   $58-$5E=Tama parts
;   $60-$66=Surprise Box / item pickups (except $62)
;   $68-$6F=Robot Masters (Needle/Magnet/Gemini/Hard/Top/Snake/Spark/Shadow)
;   $70-$77=Boss projectiles / special entities
;   $78=Proto Man (Gemini cutscene)
;   $80-$84=Wily fortress bosses / Break Man
;   $8A-$8B=Fortress boss parts
; ===========================================================================
enemy_flags_g:
        .byte   $90,$90,$90,$90,$98,$90,$90,$94 ; $00-$07
        .byte   $94,$90,$90,$B5,$90,$90,$90,$90 ; $08-$0F
        .byte   $90,$94,$98,$98,$94,$90,$90,$90 ; $10-$17
        .byte   $90,$90,$90,$90,$90,$98,$98,$9C ; $18-$1F
        .byte   $94,$B4,$94,$94,$94,$90,$90,$91 ; $20-$27
        .byte   $98,$98,$98,$98,$90,$91,$98,$98 ; $28-$2F
        .byte   $98,$D0,$90,$94,$B4,$90,$98,$90 ; $30-$37
        .byte   $94,$90,$98,$90,$90,$D0,$94,$98 ; $38-$3F
        .byte   $98,$98,$98,$98,$98,$98,$98,$90 ; $40-$47: Doc Robot / Needle Press(B)
        .byte   $90,$90,$90,$90,$90,$90,$90,$90 ; $48-$4F
        .byte   $90,$90,$90,$90,$90,$90,$92,$90 ; $50-$57: Robot Master intros / Komasaburo
        .byte   $91,$91,$91,$90,$90,$92,$90,$94 ; $58-$5F: Tama segments
        .byte   $90,$90,$90,$B6,$90,$90,$92,$92 ; $60-$67: item pickups / surprise box
        .byte   $90,$90,$90,$90,$90,$90,$90,$90 ; $68-$6F: Robot Masters
        .byte   $90,$90,$94,$94,$90,$94,$94,$92 ; $70-$77: boss projectiles
        .byte   $94,$90,$90,$96,$94,$94,$96,$96 ; $78-$7F: Proto Man (Gemini) / fortress
        .byte   $94,$94,$94,$94,$B4,$94,$D4,$94 ; $80-$87: Wily bosses
        .byte   $94,$D4,$90,$92,$92,$92,$90,$90 ; $88-$8F
        .byte   $AA,$FE,$2A,$FF,$A9,$FF,$EE,$FF ; $90-$FF: unused (overlaps stage data)
        .byte   $FE,$FF,$EA,$FF,$9A,$FF,$AC,$FD
        .byte   $A9,$FF,$AA,$FF,$FB,$BF,$AA,$F7
        .byte   $EA,$FE,$EA,$FD,$FE,$FD,$FE,$EF
        .byte   $E9,$FF,$FA,$FF,$EF,$F7,$EB,$FF
        .byte   $DB,$F7,$E9,$7F,$AA,$FF,$BE,$FF
        .byte   $6E,$7F,$FF,$DF,$AA,$FF,$BA,$7F
        .byte   $BA,$FF,$AA,$FF,$AE,$7E,$A7,$FF
        .byte   $EA,$FF,$AA,$FF,$AB,$FF,$AA,$FF
        .byte   $FB,$FF,$AA,$FF,$EA,$7F,$A8,$FF
        .byte   $BB,$FF,$FE,$FF,$EA,$FF,$EB,$DF
        .byte   $E7,$FF,$AA,$FF,$AA,$FF,$BF,$F7
        .byte   $AA,$FF,$AF,$FE,$AF,$7F,$AA,$EF
        .byte   $FB,$FD,$EA,$FF,$FA,$FD,$AA,$FF

; ===========================================================================
; enemy_main_ID_g ($A100) — AI routine index
; ===========================================================================
; Index into the sprite_main_ptr table in bank1C_1D ($83C7/$84C7).
; The engine jumps to the routine pointed to by this index each frame.
; $00 = no-op/return, $FF entries are unused.
; ===========================================================================
enemy_main_ID_g:
        .byte   $02,$03,$05,$06,$08,$15,$0A,$00 ; $00-$07: Dada/Potton/NewShotman/HammerJoe/Bubukan/Jamacy/BombFlier/(proj)
        .byte   $0D,$0E,$12,$14,$17,$18,$1E,$1A ; $08-$0F: Yambow/MetallDX/Cannon/CloudPlat/GiantMetallMet/GiantMetallMet/Gyoraibo/MagFly
        .byte   $1B,$1F,$20,$21,$22,$16,$23,$24 ; $10-$17: BlockBreaker/JunkGolem/PickelmanBull/Bikky/GiantMetall/Jamacy/MagForce/JunkBlockThrown
        .byte   $25,$27,$28,$2A,$2B,$2C,$2D,$2E ; $18-$1F: Nitron/Pole/Gyoraibo/HariHarry/PenpenMaker/RetMonking/BlockBreaker/HaveSuBee
        .byte   $30,$32,$33,$33,$35,$00,$37,$38 ; $20-$27: Bolton+Nutton/Wanaan/NeedlePress/NeedlePress/Elecn/MagnetPull/Mechakkero/TopManPlat
        .byte   $39,$3A,$3B,$3C,$3D,$3E,$3F,$40 ; $28-$2F: (noop)/(noop)/Chibee/BlockBreaker/Penpen/ElecGabyoall/(noop)/BlockBreaker
        .byte   $41,$00,$43,$F5,$33,$48,$07,$34 ; $30-$37: BlockBreaker/(noop)/Pole/Holograph/NeedlePress/(noop)/Peterchy/WalkingBomb
        .byte   $49,$4A,$4B,$4C,$4D,$23,$52,$00 ; $38-$3F: Parasyu/Hologran/Hologran/BomberPepe/MetallDX(walk)/MagnetPush/ProtoMan/(noop)
        .byte   $00,$00,$00,$00,$00,$00,$00,$58 ; $40-$47: Doc Robot screens (AI $00=noop) / NeedlePress(B)
        .byte   $59,$5A,$5B,$5C,$5D,$5E,$5F,$00 ; $48-$4F: Doc Robot AI entries
        .byte   $64,$65,$66,$67,$68,$69,$6A,$FC ; $50-$57: Robot Master intros ($50-$56) / $57=(unk)
        .byte   $6E,$6E,$70,$72,$73,$74,$F0,$F3 ; $58-$5F: Tama segments
        .byte   $4E,$4E,$47,$6C,$78,$79,$00,$00 ; $60-$67: item pickup ($60-$61) / $62=Komasaburo / surprise box
        .byte   $90,$91,$92,$93,$94,$95,$96,$97 ; $68-$6F: Robot Masters (bank06/07 dispatch)
        .byte   $00,$89,$00,$8A,$00,$00,$8C,$4F ; $70-$77: boss projectiles
        .byte   $71,$00,$E7,$00,$00,$00,$EA,$EA ; $78-$7F: Proto Man (Gemini) / fortress
        .byte   $EA,$E0,$E3,$E5,$ED,$00,$00,$00 ; $80-$87: Wily bosses
        .byte   $00,$00,$EB,$EC,$EC,$EC,$00,$00 ; $88-$8F
        .byte   $AA,$FF,$EB,$FE,$B9,$FE,$3A,$FE ; $90-$FF: unused (overlaps stage data)
        .byte   $FA,$FF,$AA,$FF,$EA,$FF,$AA,$FF
        .byte   $AA,$BF,$EA,$FF,$BE,$7B,$FA,$FF
        .byte   $AA,$F7,$EB,$9F,$BA,$FF,$BA,$FF
        .byte   $AA,$FF,$BA,$FF,$AB,$BF,$EF,$FF
        .byte   $BA,$FF,$FA,$DF,$BE,$FF,$BE,$FF
        .byte   $EA,$7F,$AE,$FF,$BA,$77,$BA,$FF
        .byte   $AE,$9F,$F9,$FA,$FA,$FF,$EE,$F7
        .byte   $EE,$6F,$BE,$FF,$BA,$FF,$AE,$FF
        .byte   $BB,$DF,$BE,$FF,$BF,$EF,$EA,$FF
        .byte   $AB,$F7,$AB,$FF,$A2,$FF,$CA,$F7
        .byte   $BA,$FF,$EA,$FF,$AA,$FF,$EE,$FF
        .byte   $EA,$FF,$BE,$FF,$BE,$FF,$A8,$FF
        .byte   $AA,$7F,$EE,$FD,$EA,$FF,$E2,$FF

; ===========================================================================
; enemy_shape_g ($A200) — hitbox shape ID
; ===========================================================================
; Determines the entity's collision box dimensions.
;   bit 7 ($80): entity deals contact damage to player on touch
;   bits 6-0: shape index into hitbox dimension tables
;   $00 = no hitbox (projectile or non-collidable)
;   $0F = special shape (boss projectile / large entity)
; ===========================================================================
enemy_shape_g:
        .byte   $C1,$C0,$C2,$A1,$A2,$C0,$83,$A1 ; $00-$07
        .byte   $C8,$A3,$A9,$0F,$C0,$C0,$CA,$C4 ; $08-$0F
        .byte   $A2,$CA,$CC,$A5,$00,$C0,$00,$C0 ; $10-$17
        .byte   $C2,$C0,$CA,$C6,$98,$C7,$C7,$CA ; $18-$1F
        .byte   $A3,$83,$80,$80,$CA,$00,$C0,$0F ; $20-$27
        .byte   $80,$80,$80,$80,$C0,$00,$80,$80 ; $28-$2F
        .byte   $A0,$00,$0F,$C1,$80,$00,$C2,$82 ; $30-$37
        .byte   $CA,$C2,$C2,$C1,$A3,$00,$C2,$C0 ; $38-$3F
        .byte   $C0,$C0,$C0,$C0,$C0,$C0,$C0,$CA ; $40-$47
        .byte   $CA,$8A,$D0,$CA,$CA,$CA,$CA,$00 ; $48-$4F
        .byte   $00,$00,$00,$00,$00,$00,$00,$98 ; $50-$57
        .byte   $00,$00,$00,$C0,$CA,$21,$AA,$00 ; $58-$5F
        .byte   $C1,$C1,$CA,$CA,$00,$00,$15,$14 ; $60-$67
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $68-$6F: Robot Masters (shape set by AI)
        .byte   $00,$8A,$00,$C3,$00,$00,$00,$98 ; $70-$77
        .byte   $00,$00,$CA,$0F,$00,$00,$19,$18 ; $78-$7F
        .byte   $18,$00,$00,$80,$94,$00,$00,$00 ; $80-$87
        .byte   $00,$00,$0B,$1A,$0F,$17,$00,$00 ; $88-$8F
        .byte   $AA,$FF,$A6,$F7,$EE,$FF,$AF,$BF ; $90-$FF: unused (overlaps stage data)
        .byte   $BB,$FF,$AA,$FF,$FE,$FF,$AB,$FF
        .byte   $CE,$7F,$AA,$FF,$8A,$EF,$21,$FF
        .byte   $BA,$FF,$AF,$FF,$BF,$F6,$BB,$FE
        .byte   $AA,$FF,$AB,$FD,$AA,$FF,$AE,$FF
        .byte   $AF,$FF,$AA,$FF,$AA,$FF,$BA,$7D
        .byte   $AA,$9F,$A8,$BF,$AA,$FB,$B6,$BF
        .byte   $AE,$FF,$AC,$6F,$BB,$FF,$BE,$DF
        .byte   $AA,$FF,$FA,$FF,$BE,$FD,$AA,$FF
        .byte   $AA,$FE,$2A,$FF,$AA,$FF,$EA,$FF
        .byte   $AA,$7F,$BA,$FF,$AA,$FF,$BA,$FB
        .byte   $AA,$FF,$EA,$F7,$AE,$FF,$BB,$FE
        .byte   $AA,$FF,$EE,$FF,$AA,$FF,$BA,$FE
        .byte   $EB,$FF,$BA,$FB,$AA,$FF,$BA,$FF

; ===========================================================================
; enemy_OAM_ID_g ($A300) — OAM animation sequence ID
; ===========================================================================
; Written to ent_anim_id on spawn. Indexes the OAM sequence tables in
; bank1A_1B to determine which sprite frames to display.
; $00 = no sprite (invisible / managed by AI code directly).
; ===========================================================================
enemy_OAM_ID_g:
        .byte   $21,$22,$59,$26,$69,$4F,$47,$13 ; $00-$07
        .byte   $74,$1C,$6E,$70,$4E,$4E,$32,$62 ; $08-$0F
        .byte   $65,$38,$31,$72,$00,$4F,$36,$5B ; $10-$17
        .byte   $53,$36,$32,$76,$95,$43,$44,$3F ; $18-$1F
        .byte   $30,$42,$82,$96,$55,$2D,$29,$91 ; $20-$27
        .byte   $20,$20,$20,$20,$93,$19,$20,$75 ; $28-$2F
        .byte   $20,$2D,$92,$13,$82,$B6,$50,$51 ; $30-$37
        .byte   $4C,$B2,$B2,$3B,$1E,$36,$99,$00 ; $38-$3F
        .byte   $00,$00,$00,$00,$00,$00,$00,$26 ; $40-$47
        .byte   $1F,$32,$2B,$45,$22,$36,$3F,$01 ; $48-$4F
        .byte   $F9,$FA,$FB,$FC,$FD,$FE,$FF,$C7 ; $50-$57: Robot Master intros use $F9-$FF
        .byte   $B3,$B4,$A4,$00,$BB,$4B,$31,$55 ; $58-$5F
        .byte   $D1,$D4,$C6,$94,$C3,$DF,$00,$00 ; $60-$67
        .byte   $01,$01,$01,$01,$01,$01,$01,$01 ; $68-$6F: Robot Masters (OAM set by AI)
        .byte   $B9,$B8,$CA,$CB,$A8,$CD,$CE,$B0 ; $70-$77
        .byte   $99,$73,$72,$00,$74,$75,$00,$00 ; $78-$7F
        .byte   $00,$70,$6B,$5F,$6A,$63,$63,$68 ; $80-$87
        .byte   $63,$63,$67,$00,$00,$00,$00,$00 ; $88-$8F
        .byte   $BA,$FF,$AA,$FF,$AA,$DF,$FA,$FF ; $90-$FF: unused (overlaps stage data)
        .byte   $AB,$F7,$EF,$FF,$BA,$FF,$AA,$FF
        .byte   $A0,$FF,$EE,$DF,$2B,$FF,$AE,$FF
        .byte   $C2,$7F,$DA,$FF,$9E,$FF,$BE,$FF
        .byte   $AA,$FE,$A2,$FF,$BA,$FE,$CB,$F7
        .byte   $FE,$F7,$AA,$FD,$BA,$FF,$FA,$FF
        .byte   $AA,$FF,$BA,$FF,$BE,$FF,$BA,$7F
        .byte   $FA,$FF,$8A,$FF,$AA,$FF,$AA,$E7
        .byte   $AE,$FF,$AA,$FF,$AA,$FF,$AA,$FF
        .byte   $DB,$FF,$FB,$FF,$EA,$FF,$E8,$7F
        .byte   $2A,$DF,$BA,$FE,$AA,$FF,$AA,$3D
        .byte   $A2,$F7,$AA,$DE,$AE,$FF,$AE,$FE
        .byte   $AF,$77,$AA,$FF,$BA,$BF,$8A,$FF
        .byte   $FA,$FF,$AA,$FD,$EA,$FF,$A2,$F7

; ===========================================================================
; enemy_health_g ($A400) — starting hit points
; ===========================================================================
; Written to ent_hp on spawn.
;   $FF = invincible (cannot be killed by any weapon)
;   $1C = 28 HP (standard boss health)
;   $00 = instant death / not a real entity
; ===========================================================================
enemy_health_g:
        .byte   $01,$01,$03,$08,$04,$01,$03,$03 ; $00-$07: Dada=1, Potton=1, NewShotman=3, HammerJoe=8, Bubukan=4
        .byte   $03,$01,$03,$FF,$01,$01,$02,$01 ; $08-$0F: Gabyoall=$FF(invincible)
        .byte   $02,$06,$03,$06,$FF,$01,$FF,$06 ; $10-$17: PetitSnakey=$FF, CloudPlatform=$FF
        .byte   $01,$01,$02,$06,$0A,$08,$01,$03 ; $18-$1F: HariHarry=6, PenpenMaker=10
        .byte   $01,$FF,$FF,$FF,$01,$FF,$01,$FF ; $20-$27: Wanaan=$FF, NeedlePress=$FF, TopManPlat=$FF
        .byte   $01,$01,$01,$01,$01,$FF,$01,$06 ; $28-$2F: buster=1, ProtoShield=$FF
        .byte   $01,$FF,$FF,$1C,$FF,$FF,$03,$01 ; $30-$37: Chibee=$FF, HaveSuBee=$1C(28)
        .byte   $03,$03,$03,$06,$01,$FF,$1C,$FF ; $38-$3F: MagnetPush=$FF, ProtoMan=$1C(28)
        .byte   $03,$03,$03,$03,$03,$03,$03,$1C ; $40-$47: Doc Robot screens, NeedlePress(B)=$1C
        .byte   $1C,$1C,$1C,$1C,$1C,$1C,$1C,$FF ; $48-$4F: Doc Robot=$1C(28)
        .byte   $FF,$FF,$1C,$1C,$FF,$FF,$FF,$0A ; $50-$57: Komasaburo=10
        .byte   $FF,$FF,$FF,$00,$08,$08,$01,$00 ; $58-$5F: Tama parts
        .byte   $02,$02,$06,$08,$01,$01,$00,$00 ; $60-$67: item pickups
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; $68-$6F: Robot Masters=$FF (HP set by AI)
        .byte   $00,$0A,$00,$0A,$00,$00,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$1C,$1C,$1C,$00,$00,$00,$00 ; $80-$87: Wily bosses=$1C(28)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $88-$8F
        .byte   $FD,$44,$EB,$14,$F7,$51,$5F,$54 ; $90-$FF: unused (overlaps stage data)
        .byte   $BF,$55,$FE,$44,$EF,$14,$DB,$51
        .byte   $CA,$44,$77,$55,$BA,$51,$5D,$57
        .byte   $E7,$35,$EE,$71,$33,$54,$FF,$0C
        .byte   $FC,$51,$8B,$45,$1F,$45,$1D,$05
        .byte   $A2,$51,$AF,$4C,$F5,$04,$32,$5D
        .byte   $67,$55,$1B,$15,$D5,$44,$FC,$57
        .byte   $3F,$54,$7F,$51,$CD,$15,$E7,$45
        .byte   $76,$57,$EF,$55,$F3,$45,$CE,$55
        .byte   $DF,$54,$EB,$75,$4C,$15,$7E,$62
        .byte   $ED,$51,$39,$51,$DA,$11,$CF,$C5
        .byte   $7B,$45,$FF,$10,$FB,$44,$D7,$41
        .byte   $CE,$44,$8F,$11,$FD,$5D,$93,$45
        .byte   $DC,$45,$8F,$45,$F5,$14,$5F,$55

; ===========================================================================
; enemy_speed_ID_g ($A500) — movement speed index
; ===========================================================================
; Index into enemy_x_velocity_sub_g and enemy_x_velocity_g tables.
; Values $00-$0D are standard speeds; $18 is used by some fortress entities.
; The actual pixel velocity is: enemy_x_velocity_g[id].enemy_x_velocity_sub_g[id]
; ===========================================================================
enemy_speed_ID_g:
        .byte   $03,$01,$00,$00,$04,$00,$01,$00 ; $00-$07
        .byte   $05,$06,$00,$08,$03,$03,$04,$01 ; $08-$0F
        .byte   $01,$00,$01,$07,$00,$00,$00,$03 ; $10-$17
        .byte   $0D,$01,$04,$0C,$00,$0C,$0C,$05 ; $18-$1F
        .byte   $02,$00,$00,$00,$01,$00,$04,$01 ; $20-$27
        .byte   $02,$02,$02,$02,$02,$00,$00,$02 ; $28-$2F
        .byte   $00,$00,$00,$01,$00,$00,$08,$01 ; $30-$37
        .byte   $04,$00,$08,$01,$04,$00,$01,$00 ; $38-$3F
        .byte   $00,$00,$00,$00,$00,$00,$00,$0C ; $40-$47
        .byte   $00,$00,$01,$09,$0B,$00,$05,$00 ; $48-$4F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$08,$00,$01,$00 ; $58-$5F
        .byte   $00,$00,$00,$00,$08,$08,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$00,$00,$18,$18,$18,$00,$00 ; $88-$8F: fortress boss parts use speed $18
        .byte   $A6,$54,$6F,$45,$FF,$16,$AA,$55 ; $90-$FF: unused (overlaps stage data)
        .byte   $87,$41,$BE,$15,$E3,$15,$4A,$66
        .byte   $FA,$54,$AE,$51,$7F,$55,$BF,$58
        .byte   $D0,$15,$FF,$55,$93,$53,$AF,$44
        .byte   $A9,$55,$47,$55,$BB,$D1,$FE,$11
        .byte   $BE,$51,$3F,$55,$7F,$D5,$76,$55
        .byte   $FE,$50,$BC,$59,$EB,$11,$B9,$41
        .byte   $1B,$55,$FD,$11,$E3,$55,$EF,$71
        .byte   $9B,$71,$2F,$54,$F7,$43,$93,$45
        .byte   $F7,$45,$EF,$44,$F7,$54,$7E,$55
        .byte   $BF,$05,$FD,$50,$39,$55,$EF,$44
        .byte   $B4,$05,$EF,$41,$6F,$51,$EA,$10
        .byte   $75,$54,$9A,$17,$FF,$55,$7F,$71
        .byte   $FF,$15,$3D,$54,$FF,$55,$FF,$40

; ===========================================================================
; enemy_x_velocity_sub_g ($A600) — X velocity subpixel table
; ===========================================================================
; Indexed by speed ID (from enemy_speed_ID_g). Only entries $00-$0D (14 bytes)
; are used as velocity data; the remainder shares address space with stage data.
;   $00=0.00  $01=0.00  $02=0.66  $03=0.80  $04=0.00  $05=0.00
;   $06=0.19  $07=0.33  $08=0.80  $09=0.00  $0A=0.80  $0B=0.4C
;   $0C=0.B3  $0D=0.CC
; ===========================================================================
enemy_x_velocity_sub_g:
        .byte   $00,$00,$66,$80,$00,$00,$19,$33 ; speed IDs $00-$07
        .byte   $80,$00,$80,$4C,$B3,$CC,$00,$00 ; speed IDs $08-$0F (last 2 unused)
        .byte   $AB,$45,$DC,$55,$FB,$11,$3E,$50 ; (stage data overlap from here)
        .byte   $F4,$55,$ED,$11,$FE,$50,$F7,$01
        .byte   $DB,$55,$CD,$85,$3F,$55,$5A,$15
        .byte   $7E,$55,$2B,$15,$5E,$44,$FF,$55
        .byte   $79,$45,$6F,$45,$B3,$15,$FF,$55
        .byte   $FE,$45,$6F,$55,$E3,$45,$9F,$41
        .byte   $3B,$45,$93,$41,$D3,$91,$BF,$55
        .byte   $CD,$55,$FF,$41,$7F,$14,$F6,$50
        .byte   $CE,$54,$EF,$45,$B3,$55,$E7,$55
        .byte   $6F,$46,$AD,$F1,$CF,$51,$DB,$51
        .byte   $FA,$54,$BD,$41,$F9,$55,$FC,$55
        .byte   $28,$45,$9E,$55,$FB,$01,$6F,$55
        .byte   $DB,$55,$F9,$45,$8F,$55,$F4,$55
        .byte   $59,$13,$AB,$54,$6D,$D7,$2E,$D4
        .byte   $6B,$51,$AA,$51,$EF,$45,$71,$51
        .byte   $63,$45,$FF,$57,$EC,$45,$C7,$17
        .byte   $DF,$15,$0F,$49,$F5,$55,$9E,$40
        .byte   $A6,$95,$E9,$45,$CC,$55,$8B,$14
        .byte   $EE,$1C,$EB,$10,$72,$51,$EB,$05
        .byte   $FF,$51,$7F,$54,$8E,$51,$CF,$04
        .byte   $3D,$15,$E6,$D0,$FB,$55,$DC,$55
        .byte   $DA,$54,$E6,$55,$F7,$14,$EB,$55
        .byte   $F4,$04,$EE,$45,$DF,$91,$FE,$55
        .byte   $B4,$54,$9E,$5C,$EA,$45,$AD,$10
        .byte   $BF,$15,$CB,$47,$D9,$54,$FB,$11
        .byte   $F7,$54,$6F,$55,$2E,$45,$FF,$15
        .byte   $B7,$55,$F5,$55,$03,$10,$FF,$55
        .byte   $BD,$D4,$77,$95,$8C,$16,$13,$15
        .byte   $EA,$15,$40,$55,$AF,$85,$F7,$45
        .byte   $F7,$45,$B9,$40,$FB,$55,$FA,$55

; ===========================================================================
; enemy_x_velocity_g ($A700) — X velocity whole pixel table
; ===========================================================================
; Indexed by speed ID. Only entries $00-$0D are velocity data:
;   $00=0px  $01=1px  $02=0px  $03=1px  $04=2px  $05=4px
;   $06=2px  $07=1px  $08=0px  $09=3px  $0A=2px  $0B=1px
;   $0C=1px  $0D=0px
; Combined with subpixel: e.g. speed $03 = 1.80 px/frame, speed $04 = 2.00
; ===========================================================================
enemy_x_velocity_g:
        .byte   $00,$01,$00,$01,$02,$04,$02,$01 ; speed IDs $00-$07
        .byte   $00,$03,$02,$01,$01,$00,$00,$00 ; speed IDs $08-$0F (last 2 unused)
        .byte   $76,$70,$DF,$51,$FB,$55,$9B,$51 ; (stage data overlap from here)
        .byte   $CC,$45,$7B,$03,$FF,$57,$DA,$90
        .byte   $7D,$61,$FA,$4D,$B2,$41,$1A,$55
        .byte   $3F,$55,$2D,$14,$DF,$55,$F7,$45
        .byte   $7D,$50,$F6,$55,$FE,$74,$EA,$55
        .byte   $6E,$05,$F4,$51,$7D,$51,$F7,$55
        .byte   $FD,$D5,$6F,$55,$3F,$14,$B3,$51
        .byte   $A3,$40,$B5,$25,$72,$51,$16,$15
        .byte   $D7,$55,$D7,$45,$3B,$91,$08,$01
        .byte   $C3,$4D,$FE,$55,$B7,$47,$A9,$14
        .byte   $5F,$54,$FE,$15,$F6,$55,$0A,$57
        .byte   $70,$51,$EA,$45,$F5,$19,$E7,$44
        .byte   $1D,$10,$F7,$17,$FA,$55,$CE,$41
        .byte   $FA,$55,$8E,$55,$CE,$75,$FE,$51
        .byte   $FE,$41,$C8,$51,$BF,$15,$9F,$46
        .byte   $87,$54,$BB,$1C,$75,$14,$F2,$60
        .byte   $7A,$55,$ED,$45,$65,$54,$FD,$06
        .byte   $E1,$55,$6D,$57,$EF,$D5,$6A,$14
        .byte   $92,$51,$EE,$55,$FD,$15,$F2,$55
        .byte   $F5,$54,$E2,$45,$FB,$95,$2B,$04
        .byte   $FE,$50,$FD,$50,$9E,$10,$67,$45
        .byte   $FE,$81,$EB,$14,$EC,$05,$9B,$57
        .byte   $CB,$45,$BF,$44,$57,$05,$EB,$55
        .byte   $DF,$55,$EF,$35,$73,$15,$B1,$05
        .byte   $FA,$45,$FB,$11,$DC,$D0,$32,$54
        .byte   $EB,$05,$FB,$50,$5A,$55,$D6,$75
        .byte   $FF,$74,$9A,$15,$9F,$55,$E7,$55
        .byte   $F7,$55,$8D,$55,$73,$01,$97,$45
        .byte   $73,$45,$ED,$05,$DE,$D1,$F3,$45
        .byte   $F7,$54,$B3,$51,$85,$55,$F9,$55


; =============================================================================
; NEEDLE MAN STAGE DATA ($A800-$BFFF)
; =============================================================================
; Stage layout and enemy placement data for Needle Man's stage.
; Stage ID $22 maps to PRG bank $00 via the ensure_stage_bank_table.
; =============================================================================


; ===========================================================================
; Boss AI local data ($A800-$A9FF)
; ===========================================================================
; Read by bank $06 Needle Man AI routines. Contains movement parameters,
; projectile patterns, and state machine data for the Needle Man boss fight.
; Referenced as $A868,y / $A86C,y / $A870,y / $A8D8,x etc. by bank $06.
; ===========================================================================
        .byte   $02,$00,$08,$04,$00,$50,$22,$08
        .byte   $00,$D4,$80,$81,$00,$11,$00,$20
        .byte   $0A,$80,$00,$00,$0A,$13,$0A,$04
        .byte   $80,$05,$00,$00,$20,$00,$00,$40
        .byte   $00,$00,$00,$01,$00,$02,$A0,$62
        .byte   $00,$01,$80,$04,$80,$30,$00,$00
        .byte   $00,$20,$08,$04,$00,$41,$80,$02
        .byte   $A0,$02,$80,$00,$00,$20,$00,$00
        .byte   $00,$00,$08,$00,$00,$71,$00,$C3
        .byte   $08,$80,$00,$80,$08,$70,$02,$12
        .byte   $00,$2A,$08,$C5,$00,$00,$20,$00
        .byte   $00,$22,$20,$20,$20,$0A,$08,$04
        .byte   $00,$00,$00,$01,$00,$B4,$80,$A6
        .byte   $00,$20,$08,$01,$08,$70,$80,$00
        .byte   $28,$00,$00,$00,$00,$44,$20,$04
        .byte   $00,$70,$00,$3B,$20,$2C,$22,$78
        .byte   $00,$10,$00,$02,$20,$00,$00,$50
        .byte   $02,$22,$00,$20,$00,$40,$00,$80
        .byte   $88,$04,$00,$28,$08,$28,$00,$0C
        .byte   $08,$0A,$00,$78,$00,$25,$00,$61
        .byte   $00,$54,$00,$C0,$00,$10,$00,$A4
        .byte   $08,$10,$02,$00,$00,$00,$00,$02
        .byte   $00,$12,$08,$30,$0A,$01,$20,$06
        .byte   $02,$13,$00,$40,$02,$00,$18,$04
        .byte   $00,$20,$00,$08,$20,$00,$00,$00
        .byte   $00,$84,$00,$10,$80,$00,$00,$21
        .byte   $08,$24,$00,$09,$20,$00,$80,$00
        .byte   $2A,$14,$00,$10,$00,$42,$00,$09
        .byte   $00,$00,$A2,$03,$00,$0D,$28,$10
        .byte   $02,$20,$00,$00,$00,$A4,$A0,$04
        .byte   $00,$00,$02,$25,$22,$84,$20,$C1
        .byte   $02,$00,$20,$E0,$A0,$04,$20,$20
        .byte   $00,$08,$08,$42,$02,$40,$08,$C0
        .byte   $00,$60,$00,$20,$00,$00,$08,$03
        .byte   $00,$10,$88,$20,$20,$18,$08,$06
        .byte   $02,$A1,$82,$B2,$08,$00,$08,$1C
        .byte   $00,$52,$00,$02,$0A,$08,$20,$50
        .byte   $01,$42,$00,$40,$00,$00,$00,$40
        .byte   $08,$00,$28,$01,$A0,$04,$80,$01
        .byte   $00,$40,$02,$88,$20,$54,$22,$20
        .byte   $00,$64,$28,$00,$00,$04,$20,$04
        .byte   $00,$20,$00,$01,$00,$04,$28,$90
        .byte   $20,$00,$02,$A8,$00,$00,$0A,$00
        .byte   $28,$12,$00,$92,$02,$14,$00,$18
        .byte   $00,$90,$28,$09,$00,$84,$20,$00
        .byte   $00,$80,$40,$24,$08,$A4,$00,$8F
        .byte   $00,$00,$00,$80,$00,$00,$8A,$01
        .byte   $00,$83,$00,$43,$03,$00,$20,$20
        .byte   $00,$40,$20,$04,$00,$49,$28,$10
        .byte   $00,$08,$00,$20,$28,$40,$20,$08
        .byte   $00,$69,$08,$6C,$00,$83,$08,$00
        .byte   $00,$4E,$22,$02,$00,$48,$20,$DD
        .byte   $00,$00,$00,$14,$00,$84,$00,$00
        .byte   $82,$03,$00,$C0,$08,$08,$80,$00
        .byte   $02,$08,$88,$34,$20,$20,$80,$10
        .byte   $08,$12,$02,$A4,$00,$10,$00,$00
        .byte   $00,$04,$00,$C0,$00,$00,$00,$4C
        .byte   $02,$01,$08,$00,$08,$52,$88,$00
        .byte   $00,$18,$00,$64,$2A,$00,$00,$80
        .byte   $28,$20,$02,$8E,$00,$00,$00,$0C
        .byte   $00,$10,$00,$20,$8A,$22,$82,$82
        .byte   $00,$A2,$00,$44,$00,$A1,$0A,$11
        .byte   $82,$35,$80,$00,$00,$06,$20,$20
        .byte   $80,$17,$00,$40,$2C,$5D,$08,$91

; ===========================================================================
; Screen metatile grid + room table ($AA00-$AAFF)
; ===========================================================================
; $AA00-$AA12: screen column IDs per page (19 entries for Needle Man)
; $AA13-$AA5F: room/screen pointer data
; $AA60,y*2:   room pointer table (2 bytes/room):
;                byte 0 = CHR/palette param (indexes bank $01 $A200/$A030)
;                byte 1 = layout index (into $AA82, *20 for offset)
; $AA80-$AA81: BG CHR bank indices for stage
; $AA82+:      screen layout data (20 bytes/entry):
;                bytes 0-15: 16 metatile column IDs (one per 16px column)
;                bytes 16-19: screen connection data (bit 7=scroll, bits 0-6=target)
; ===========================================================================
        .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte   $10,$11,$12,$A0,$20,$08,$02,$80
        .byte   $80,$0C,$80,$80,$00,$00,$28,$02
        .byte   $00,$08,$00,$0A,$00,$00,$00,$00
        .byte   $08,$40,$00,$80,$00,$00,$00,$01
        .byte   $10,$2A,$36,$0B,$80,$80,$20,$18
        .byte   $00,$13,$A0,$40,$28,$10,$20,$00
        .byte   $22,$66,$62,$80,$80,$A1,$20,$20
        .byte   $00,$22,$20,$00,$02,$00,$00,$22
        .byte   $00,$C9,$0A,$00,$20,$00,$02,$00
        .byte   $00,$18,$08,$81,$08,$98,$00,$48
        .byte   $2E,$00,$00,$00,$0A,$00,$2E,$00
        .byte   $17,$00,$0B,$00,$0B,$01,$25,$01
        .byte   $00,$19,$00,$C1,$00,$40,$80,$60
        .byte   $00,$00,$08,$02,$20,$64,$0A,$01

; --- stage palette data ($AA80) ---
        .byte   $48,$4A,$0F,$20,$27,$17,$0F,$1C
        .byte   $20,$21,$0F,$2B,$1B,$0B,$0F,$38
        .byte   $27,$21,$00,$00,$00,$00,$0F,$20
        .byte   $27,$17,$0F,$1C,$20,$21,$0F,$2B
        .byte   $1B,$0B,$0F,$27,$17,$06,$00,$00
        .byte   $00,$00,$20,$A0,$00,$04,$20,$06
        .byte   $00,$04,$00,$02,$00,$54,$00,$40
        .byte   $00,$00,$2A,$81,$08,$44,$20,$04
        .byte   $00,$00,$00,$00,$08,$40,$80,$09
        .byte   $00,$00,$00,$21,$20,$28,$0A,$0B
        .byte   $00,$02,$00,$0C,$20,$08,$80,$82
        .byte   $08,$20,$80,$14,$20,$04,$22,$08
        .byte   $00,$50,$80,$0C,$00,$01,$88,$60
        .byte   $00,$98,$08,$12,$00,$31,$2A,$42
        .byte   $02,$06,$00,$03,$20,$86,$8A,$08
        .byte   $0A,$11,$FF,$70,$00,$65,$00,$68

; ===========================================================================
; Enemy spawn tables ($AB00-$AEFF)
; ===========================================================================
; Four parallel 256-byte tables define enemy placement:
;   $AB00,y = screen number where enemy appears ($FF = end of list)
;   $AC00,y = X pixel position on screen
;   $AD00,y = Y pixel position on screen
;   $AE00,y = global enemy ID (indexes the $A000+ property tables)
; ===========================================================================

; --- $AB00: enemy screen numbers (terminated by $FF) ---
        .byte   $01,$01,$02,$02,$04,$04,$04,$05
        .byte   $05,$06,$06,$07,$07,$08,$08,$09
        .byte   $09,$0A,$0A,$0B,$0B,$0B,$0B,$0B
        .byte   $0C,$0D,$0D,$0E,$0E,$0F,$12,$FF

; --- $AC00: enemy X pixel positions ---
        .byte   $80,$00,$00,$40,$08,$C8,$00,$C4
        .byte   $0A,$00,$00,$00,$00,$00,$20,$00
        .byte   $08,$00,$02,$00,$28,$20,$08,$D8
        .byte   $08,$00,$A0,$0D,$A8,$08,$22,$20
        .byte   $00,$48,$A0,$20,$02,$84,$00,$04
        .byte   $80,$20,$00,$08,$88,$02,$20,$52
        .byte   $08,$41,$82,$01,$08,$40,$80,$C0
        .byte   $08,$30,$00,$00,$08,$19,$22,$33
        .byte   $00,$10,$20,$10,$02,$42,$00,$41
        .byte   $80,$00,$00,$80,$20,$05,$28,$10
        .byte   $00,$40,$12,$24,$00,$20,$02,$04
        .byte   $80,$30,$88,$20,$08,$10,$80,$08
        .byte   $00,$01,$00,$01,$2A,$40,$20,$C0
        .byte   $20,$00,$20,$0C,$02,$0C,$00,$00
        .byte   $02,$10,$00,$84,$A0,$10,$00,$50
        .byte   $00,$30,$08,$00,$00,$00,$08,$30
        .byte   $00,$00,$22,$48,$80,$2A,$00,$00
        .byte   $00,$20,$00,$80,$20,$00,$00,$01
        .byte   $08,$00,$8A,$04,$22,$89,$00,$80
        .byte   $00,$0A,$80,$00,$00,$00,$00,$00
        .byte   $11,$1E,$88,$18,$00,$41,$A0,$40
        .byte   $02,$20,$08,$02,$00,$10,$00,$28
        .byte   $08,$3D,$02,$00,$80,$80,$A0,$00
        .byte   $00,$09,$00,$85,$AA,$20,$28,$20
        .byte   $08,$01,$08,$00,$2A,$20,$00,$40
        .byte   $80,$00,$08,$83,$00,$05,$08,$0E
        .byte   $A0,$20,$08,$00,$A0,$16,$00,$BE
        .byte   $02,$28,$80,$B0,$22,$02,$22,$3B

; --- $AD00: enemy Y pixel positions ---
        .byte   $10,$80,$40,$A0,$48,$B8,$D0,$70
        .byte   $D0,$48,$C8,$68,$A8,$48,$A8,$68
        .byte   $C8,$70,$FE,$50,$70,$B0,$D0,$F0
        .byte   $90,$48,$D0,$68,$A8,$F8,$C0,$FF

; --- $AE00: global enemy IDs ---
        .byte   $28,$00,$00,$40,$01,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$00,$00,$40
        .byte   $80,$04,$00,$01,$00,$00,$08,$00
        .byte   $90,$01,$00,$00,$00,$10,$04,$00
        .byte   $20,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$10,$20,$00,$00,$10,$00,$00
        .byte   $00,$00,$04,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$40,$00,$00,$08,$00
        .byte   $00,$00,$C0,$00,$80,$00,$40,$00
        .byte   $00,$00,$06,$00,$12,$00,$02,$00
        .byte   $10,$00,$10,$00,$01,$10,$00,$00
        .byte   $04,$00,$00,$10,$00,$00,$00,$00
        .byte   $48,$00,$10,$00,$02,$40,$00,$00
        .byte   $00,$01,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$11,$00
        .byte   $02,$01,$00,$00,$00,$00,$30,$00
        .byte   $00,$04,$04,$40,$00,$00,$41,$00
        .byte   $20,$00,$04,$00,$00,$00,$00,$00
        .byte   $10,$00,$22,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$01,$00,$20,$00
        .byte   $01,$00,$00,$00,$00,$00,$00,$10
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$48,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $16,$01,$80,$00,$06,$00,$00,$10
        .byte   $08,$01,$00,$00,$14,$00,$00,$00
        .byte   $84,$00,$80,$40,$10,$04,$10,$00

; ===========================================================================
; Metatile column definitions ($AF00-$B6FF)
; ===========================================================================
; Each column ID has 64 bytes of metatile indices (8 rows x 8 entries).
; The engine reads these via metatile_column_ptr at $AF00 + (column_ID * 64).
; 32 column IDs = 2048 bytes total.
; ===========================================================================
        .byte   $B4,$84,$84,$64,$A4,$A8,$74,$74
        .byte   $54,$74,$18,$74,$18,$74,$64,$20
        .byte   $20,$38,$98,$98,$98,$68,$68,$68
        .byte   $28,$74,$28,$40,$90,$68,$00,$FF
        .byte   $00,$00,$00,$00,$08,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $03,$00,$01,$00,$00,$00,$20,$04
        .byte   $00,$00,$00,$00,$00,$00,$90,$00
        .byte   $40,$00,$20,$04,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$08,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$04,$00
        .byte   $04,$00,$10,$00,$80,$04,$02,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$00,$08,$00,$10,$00
        .byte   $00,$00,$00,$00,$40,$00,$04,$04
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$10,$00,$40,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$24,$10,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$09,$01
        .byte   $08,$40,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$00,$02,$00,$00,$00
        .byte   $00,$00,$00,$10,$80,$00,$00,$00
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$04,$10,$00,$00,$10
        .byte   $04,$00,$00,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$04,$20,$00,$00,$00,$00,$00
        .byte   $08,$10,$88,$00,$81,$00,$84,$10
        .byte   $0A,$00,$08,$10,$00,$04,$00,$00
        .byte   $02,$00,$08,$00,$00,$10,$00,$00
        .byte   $00,$00,$10,$01,$84,$40,$50,$00

; --- screen tile map data (column definitions continued) ---
        .byte   $1B,$1B,$1B,$1B,$09,$52,$0A,$0A
        .byte   $0A,$09,$08,$09,$08,$09,$09,$08
        .byte   $08,$23,$23,$23,$23,$23,$23,$23
        .byte   $23,$1B,$54,$03,$03,$13,$47,$FF
        .byte   $00,$00,$00,$40,$0A,$00,$02,$00
        .byte   $00,$00,$02,$00,$00,$10,$00,$00
        .byte   $80,$00,$11,$00,$00,$00,$00,$00
        .byte   $08,$00,$00,$40,$01,$00,$00,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$40
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$10,$00,$00,$00,$08,$00
        .byte   $00,$40,$00,$40,$00,$00,$00,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$00,$00,$04,$00,$00
        .byte   $00,$00,$00,$04,$00,$00,$88,$00
        .byte   $00,$10,$10,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $10,$40,$00,$04,$00,$00,$00,$04
        .byte   $08,$00,$10,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$04,$00,$00,$00,$00
        .byte   $A0,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$40
        .byte   $00,$00,$00,$04,$01,$00,$00,$00
        .byte   $80,$00,$04,$00,$01,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$10
        .byte   $00,$00,$C2,$00,$06,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$40,$00
        .byte   $00,$00,$00,$00,$00,$00,$80,$11
        .byte   $00,$00,$00,$00,$00,$00,$09,$00

; ===========================================================================
; Screen layout data ($AF00+ continued)
; ===========================================================================
; Metatile column definitions continue here. Each screen is built from
; 16 column references; each column is 64 bytes defining 8 rows of metatiles.
; ===========================================================================
        .byte   $00,$01,$02,$02,$03,$04,$05,$06
        .byte   $07,$08,$09,$0A,$04,$04,$0B,$0C
        .byte   $00,$01,$04,$04,$0D,$04,$06,$0E
        .byte   $0F,$10,$04,$04,$04,$04,$11,$12
        .byte   $08,$13,$04,$04,$04,$04,$04,$13
        .byte   $08,$14,$15,$16,$04,$04,$17,$14
        .byte   $18,$19,$18,$19,$18,$19,$18,$19
        .byte   $1A,$1B,$1A,$1B,$1A,$1B,$1A,$1B
        .byte   $0E,$00,$01,$04,$05,$02,$02,$06
        .byte   $1C,$1C,$08,$04,$04,$1D,$09,$1E
        .byte   $1F,$00,$01,$04,$04,$04,$04,$06
        .byte   $20,$0F,$10,$04,$04,$04,$04,$11
        .byte   $1E,$08,$04,$21,$21,$04,$04,$13
        .byte   $1E,$08,$15,$22,$23,$04,$17,$14
        .byte   $18,$19,$18,$22,$23,$19,$18,$19
        .byte   $1A,$1B,$1A,$1B,$1A,$1B,$1A,$1B
        .byte   $0E,$1F,$00,$01,$03,$04,$04,$24
        .byte   $1C,$1C,$08,$10,$04,$0D,$04,$25
        .byte   $0E,$00,$01,$04,$04,$04,$04,$25
        .byte   $20,$0F,$10,$04,$21,$21,$04,$25
        .byte   $1E,$26,$21,$27,$28,$29,$04,$25
        .byte   $1E,$22,$1A,$1B,$1A,$29,$16,$25
        .byte   $18,$22,$1A,$1B,$1A,$1B,$18,$2A
        .byte   $1A,$1B,$1A,$1B,$1A,$1B,$1A,$2B
        .byte   $2C,$2C,$2C,$2C,$2C,$2C,$2D,$25
        .byte   $2E,$02,$02,$03,$0D,$04,$04,$25
        .byte   $2E,$09,$0A,$04,$04,$2F,$30,$31
        .byte   $2E,$04,$04,$04,$04,$04,$32,$33
        .byte   $2E,$04,$0D,$34,$35,$36,$37,$36
        .byte   $2E,$2F,$38,$38,$39,$3A,$3B,$3A
        .byte   $2E,$04,$3C,$3D,$3E,$3F,$40,$3E
        .byte   $2E,$04,$41,$42,$43,$44,$42,$43
        .byte   $43,$2E,$02,$03,$45,$46,$47,$46
        .byte   $42,$48,$0A,$04,$45,$49,$4A,$49
        .byte   $4B,$4C,$4D,$04,$45,$47,$46,$47
        .byte   $4E,$33,$4F,$04,$45,$04,$50,$04
        .byte   $37,$36,$37,$36,$37,$51,$52,$04
        .byte   $3B,$3A,$3B,$3A,$3B,$53,$54,$38
        .byte   $3F,$3D,$3E,$3F,$3D,$3E,$3F,$3D
        .byte   $44,$55,$43,$44,$55,$43,$44,$55
        .byte   $4A,$56,$02,$02,$02,$03,$05,$03
        .byte   $47,$57,$1D,$09,$0A,$04,$04,$04
        .byte   $47,$57,$04,$04,$04,$04,$04,$04
        .byte   $49,$57,$04,$04,$04,$04,$52,$04
        .byte   $04,$57,$04,$52,$04,$58,$59,$5A
        .byte   $38,$5B,$38,$54,$38,$5C,$5D,$5E
        .byte   $3E,$3F,$3D,$3E,$3F,$3D,$3E,$3F
        .byte   $43,$44,$55,$43,$44,$55,$43,$44
        .byte   $45,$4A,$47,$46,$46,$56,$02,$02
        .byte   $45,$47,$46,$49,$4A,$57,$1D,$09
        .byte   $45,$4A,$49,$47,$50,$57,$04,$04
        .byte   $45,$49,$50,$5F,$60,$61,$62,$5A
        .byte   $45,$04,$63,$25,$64,$65,$66,$67
        .byte   $68,$38,$38,$69,$6A,$6B,$6C,$6D
        .byte   $40,$3E,$6E,$40,$3E,$6E,$40,$3E
        .byte   $42,$43,$44,$42,$43,$44,$42,$43
        .byte   $02,$03,$04,$04,$04,$0D,$04,$05
        .byte   $0A,$04,$0D,$04,$04,$04,$04,$04
        .byte   $04,$04,$04,$04,$04,$6F,$70,$71
        .byte   $04,$04,$04,$04,$04,$72,$73,$74
        .byte   $04,$04,$04,$75,$76,$77,$42,$78
        .byte   $38,$38,$38,$79,$55,$42,$42,$7A
        .byte   $6E,$40,$40,$7B,$43,$44,$42,$7C
        .byte   $44,$42,$43,$42,$55,$43,$43,$7D
        .byte   $7E,$4A,$47,$46,$46,$46,$56,$03
        .byte   $45,$47,$46,$4A,$49,$4A,$57,$04
        .byte   $45,$4A,$49,$50,$47,$50,$57,$04
        .byte   $45,$49,$50,$5F,$60,$62,$61,$5A
        .byte   $45,$04,$63,$25,$64,$65,$66,$67
        .byte   $68,$38,$38,$69,$6A,$6B,$6C,$6D
        .byte   $7F,$3F,$40,$40,$3E,$3F,$3E,$3F
        .byte   $43,$44,$55,$55,$43,$44,$43,$44
        .byte   $04,$04,$04,$04,$04,$04,$05,$80
        .byte   $04,$04,$0D,$04,$04,$04,$04,$25
        .byte   $04,$0D,$04,$04,$04,$6F,$71,$81
        .byte   $04,$04,$04,$04,$04,$72,$82,$83
        .byte   $04,$04,$04,$75,$76,$77,$2E,$83
        .byte   $38,$38,$38,$79,$55,$42,$78,$83
        .byte   $40,$3D,$40,$7B,$43,$44,$2E,$83
        .byte   $55,$55,$43,$42,$55,$43,$78,$83

; --- Needle Man boss room and fortress transition screens ---
        .byte   $84,$85,$86,$55,$43,$84,$87,$88
        .byte   $48,$89,$52,$8A,$8B,$8C,$89,$88
        .byte   $48,$8D,$8E,$8D,$8F,$90,$8D,$91
        .byte   $48,$92,$2C,$2C,$2C,$2C,$2C,$2C
        .byte   $48,$93,$89,$89,$89,$94,$95,$96
        .byte   $8C,$8D,$8D,$8D,$8D,$97,$98,$99
        .byte   $9A,$9A,$9A,$9A,$9A,$9A,$9A,$9A
        .byte   $4E,$9B,$4E,$9B,$9B,$9B,$4E,$9B
        .byte   $42,$42,$42,$43,$43,$43,$43,$48
        .byte   $43,$43,$42,$42,$42,$55,$55,$48
        .byte   $55,$55,$42,$84,$85,$85,$85,$87
        .byte   $9C,$85,$86,$48,$89,$89,$9D,$89
        .byte   $9E,$89,$9F,$90,$8D,$A0,$A1,$A2
        .byte   $8D,$8D,$A3,$8D,$8D,$A4,$3D,$3D
        .byte   $9A,$9A,$9A,$9A,$9A,$A5,$42,$42
        .byte   $4E,$9B,$4E,$9B,$9B,$43,$55,$43
        .byte   $85,$85,$85,$85,$85,$85,$A6,$A7
        .byte   $A8,$A9,$89,$A8,$A9,$89,$AA,$AB
        .byte   $AC,$AD,$8D,$AC,$AD,$8D,$8D,$AE
        .byte   $AC,$AD,$8D,$AC,$AF,$8D,$8D,$A7
        .byte   $B0,$AD,$B1,$AC,$AD,$8D,$B1,$A7
        .byte   $B2,$AD,$8D,$AC,$AD,$8D,$8D,$AB
        .byte   $2E,$B3,$B4,$B5,$B3,$B6,$B7,$AE
        .byte   $48,$B8,$B8,$B8,$B8,$B8,$B8,$A7
        .byte   $48,$B9,$04,$47,$4A,$4A,$47,$A7
        .byte   $48,$B9,$04,$50,$47,$49,$2F,$AB
        .byte   $48,$BA,$0D,$04,$50,$04,$04,$AE
        .byte   $BB,$04,$04,$04,$04,$04,$04,$A7
        .byte   $9A,$9A,$9A,$9A,$BC,$04,$04,$A7
        .byte   $BD,$BE,$BF,$BD,$C0,$30,$C1,$AB
        .byte   $42,$42,$43,$44,$C2,$BF,$C3,$AE
        .byte   $43,$44,$55,$43,$55,$42,$C3,$A7
        .byte   $48,$B9,$04,$47,$47,$46,$4A,$A7
        .byte   $48,$04,$04,$50,$04,$49,$50,$AB
        .byte   $48,$30,$30,$30,$30,$30,$C4,$AE
        .byte   $48,$47,$4A,$49,$04,$47,$C5,$A7
        .byte   $48,$04,$50,$49,$04,$04,$04,$A7
        .byte   $48,$C6,$C7,$C8,$C7,$C7,$C8,$AB
        .byte   $48,$B9,$4A,$47,$4A,$49,$47,$AE
        .byte   $48,$B9,$47,$4A,$47,$4A,$4A,$A7

; --- duplicate screen data (Doc Robot revisit of Needle Man stage) ---
        .byte   $48,$05,$02,$02,$02,$03,$04,$04
        .byte   $48,$04,$1D,$09,$0A,$04,$0D,$04
        .byte   $48,$04,$04,$0D,$04,$04,$04,$04
        .byte   $48,$04,$04,$04,$0D,$04,$04,$04
        .byte   $48,$04,$04,$04,$04,$04,$04,$C9
        .byte   $48,$C6,$C7,$C8,$C7,$C7,$C8,$CA
        .byte   $48,$B9,$4A,$47,$4A,$49,$4A,$A7
        .byte   $48,$B9,$47,$4A,$47,$4A,$47,$AB
        .byte   $04,$A7,$42,$43,$44,$42,$42,$43
        .byte   $04,$CB,$85,$85,$85,$CC,$42,$42
        .byte   $04,$04,$04,$0D,$04,$CB,$85,$85
        .byte   $04,$04,$04,$04,$04,$04,$04,$CD
        .byte   $9A,$9A,$9A,$BC,$04,$04,$04,$CD
        .byte   $BE,$9B,$BE,$CE,$CF,$9A,$9A,$9A
        .byte   $42,$55,$42,$42,$D0,$BE,$BF,$BD
        .byte   $44,$43,$44,$42,$42,$42,$42,$42
        .byte   $44,$42,$43,$44,$42,$43,$44,$42
        .byte   $43,$44,$42,$43,$44,$42,$43,$44
        .byte   $85,$85,$85,$85,$85,$85,$85,$85
        .byte   $D1,$D2,$D3,$D3,$D2,$D3,$D3,$D4
        .byte   $D5,$D6,$D7,$D7,$D6,$D7,$D7,$D8
        .byte   $9A,$9A,$9A,$9A,$9A,$9A,$9A,$9A
        .byte   $BD,$BE,$D9,$BE,$D9,$BD,$4E,$DA
        .byte   $42,$43,$44,$42,$42,$43,$44,$42
        .byte   $DB,$85,$85,$85,$85,$85,$85,$85
        .byte   $DC,$D3,$D2,$D3,$D3,$D2,$D3,$DD
        .byte   $DE,$DF,$D6,$DF,$DF,$D6,$D7,$E0
        .byte   $E1,$D7,$D6,$DF,$DF,$D6,$DF,$E2
        .byte   $D5,$D7,$D6,$D7,$D7,$D6,$DF,$E2
        .byte   $E3,$DF,$D6,$DF,$DF,$D6,$D7,$E0
        .byte   $E4,$DF,$D6,$D7,$D7,$D6,$DF,$E2
        .byte   $E5,$9A,$9A,$9A,$9A,$9A,$9A,$BC

; --- empty screen padding (filled with tile $B8) ---
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8

; --- Doc Robot revisit screen data (duplicate of earlier screens) ---
        .byte   $48,$05,$02,$02,$02,$03,$04,$04
        .byte   $48,$04,$1D,$09,$0A,$04,$0D,$04
        .byte   $48,$04,$04,$0D,$04,$04,$04,$04
        .byte   $48,$04,$04,$04,$0D,$04,$04,$04
        .byte   $48,$04,$04,$04,$04,$04,$04,$C9
        .byte   $48,$C6,$C7,$C8,$C7,$C7,$C8,$CA
        .byte   $48,$B9,$4A,$47,$4A,$49,$4A,$A7
        .byte   $48,$B9,$47,$4A,$47,$4A,$47,$AB
        .byte   $04,$A7,$42,$43,$44,$42,$42,$43
        .byte   $04,$CB,$85,$85,$85,$CC,$42,$42
        .byte   $04,$04,$04,$0D,$04,$CB,$85,$85
        .byte   $04,$04,$04,$04,$04,$04,$04,$CD
        .byte   $9A,$9A,$9A,$BC,$04,$04,$04,$CD
        .byte   $BE,$9B,$BE,$CE,$CF,$9A,$9A,$9A
        .byte   $42,$55,$42,$42,$D0,$BE,$BF,$BD
        .byte   $44,$43,$44,$42,$42,$42,$42,$42
        .byte   $44,$42,$43,$44,$42,$43,$44,$42
        .byte   $43,$44,$42,$43,$44,$42,$43,$44
        .byte   $85,$85,$85,$85,$85,$85,$85,$85
        .byte   $D1,$D2,$D3,$D3,$D2,$D3,$D3,$D4
        .byte   $D5,$D6,$D7,$D7,$D6,$D7,$D7,$D8
        .byte   $9A,$9A,$9A,$9A,$9A,$9A,$9A,$9A
        .byte   $BD,$BE,$D9,$BE,$D9,$BD,$4E,$DA
        .byte   $42,$43,$44,$42,$42,$43,$44,$42
        .byte   $DB,$85,$85,$85,$85,$85,$85,$85
        .byte   $DC,$D3,$D2,$D3,$D3,$D2,$D3,$DD
        .byte   $DE,$DF,$D6,$DF,$DF,$D6,$D7,$E0
        .byte   $E1,$D7,$D6,$DF,$DF,$D6,$DF,$E2
        .byte   $D5,$D7,$D6,$D7,$D7,$D6,$DF,$E2
        .byte   $E3,$DF,$D6,$DF,$DF,$D6,$D7,$E0
        .byte   $E4,$DF,$D6,$D7,$D7,$D6,$DF,$E2
        .byte   $E5,$9A,$9A,$9A,$9A,$9A,$9A,$BC

; --- empty screen padding (filled with tile $B8) ---
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8
        .byte   $B8,$B8,$B8,$B8,$B8,$B8,$B8,$B8

; ===========================================================================
; Metatile CHR tile definitions ($B700-$BAFF)
; ===========================================================================
; 4 bytes per metatile index, defining the 2x2 CHR tile pattern:
;   byte 0 = top-left CHR tile
;   byte 1 = top-right CHR tile
;   byte 2 = bottom-left CHR tile
;   byte 3 = bottom-right CHR tile
; Read by the engine at $B700 + (metatile_index * 4).
; The engine also reads palette bits from the collision table at $BF00.
; ===========================================================================
        .byte   $07,$28,$6A,$28,$20,$29,$20,$29
        .byte   $5C,$5C,$5C,$5C,$5C,$50,$50,$08
        .byte   $08,$08,$08,$08,$52,$5C,$08,$52
        .byte   $2A,$20,$2A,$20,$0F,$0A,$0D,$0A
        .byte   $0A,$0B,$0A,$0B,$5C,$58,$53,$08
        .byte   $59,$53,$08,$08,$08,$08,$09,$0A
        .byte   $09,$0A,$0A,$0A,$08,$08,$45,$4D
        .byte   $21,$06,$21,$69,$0A,$0A,$0A,$0B
        .byte   $0A,$0B,$08,$08,$09,$0A,$08,$08
        .byte   $0A,$0A,$08,$08,$1F,$16,$1D,$1E
        .byte   $22,$17,$17,$22,$1D,$16,$22,$1E
        .byte   $08,$08,$1D,$08,$16,$08,$1E,$1D
        .byte   $05,$04,$0E,$26,$05,$04,$27,$0C
        .byte   $27,$0C,$0E,$26,$0E,$26,$27,$0C
        .byte   $0A,$0A,$0A,$0A,$5A,$5B,$08,$08
        .byte   $09,$0A,$09,$0A,$0F,$0F,$0D,$0D
        .byte   $0A,$0A,$09,$0A,$08,$08,$05,$04
        .byte   $91,$26,$27,$0C,$27,$90,$0E,$26
        .byte   $52,$57,$45,$57,$08,$57,$08,$57
        .byte   $0A,$0B,$05,$04,$1F,$16,$05,$04
        .byte   $27,$0C,$91,$26,$0E,$26,$27,$90
        .byte   $56,$57,$3D,$57,$3D,$57,$3D,$57
        .byte   $4F,$4F,$4E,$4E,$32,$33,$47,$3B
        .byte   $01,$57,$11,$57,$08,$08,$18,$19
        .byte   $08,$08,$19,$19,$08,$38,$19,$19
        .byte   $70,$66,$78,$11,$6D,$6C,$01,$02
        .byte   $18,$19,$08,$08,$19,$19,$70,$66
        .byte   $19,$19,$66,$67,$19,$19,$6D,$6C
        .byte   $08,$08,$19,$1B,$78,$11,$19,$1B
        .byte   $61,$62,$19,$1B,$01,$02,$19,$1B
        .byte   $08,$1C,$08,$70,$63,$1C,$66,$67
        .byte   $63,$1C,$66,$6F,$63,$1C,$67,$66
        .byte   $63,$1C,$6D,$6C,$08,$78,$08,$78
        .byte   $01,$02,$11,$03,$11,$64,$01,$02
        .byte   $03,$03,$01,$02,$08,$23,$08,$2B
        .byte   $3C,$08,$39,$3A,$39,$3A,$3C,$08
        .byte   $03,$57,$02,$57,$3C,$08,$3C,$08
        .byte   $39,$3A,$39,$3A,$01,$02,$19,$19
        .byte   $01,$3B,$19,$19,$08,$08,$19,$1A
        .byte   $66,$67,$61,$62,$66,$71,$11,$79
        .byte   $3C,$08,$08,$08,$19,$1A,$66,$71
        .byte   $24,$25,$2C,$2D,$11,$79,$19,$1B
        .byte   $34,$35,$19,$1B,$61,$62,$11,$03
        .byte   $23,$5C,$2B,$52,$23,$08,$2B,$08
        .byte   $08,$08,$30,$31,$34,$35,$32,$31
        .byte   $08,$08,$32,$33,$23,$08,$19,$1B
        .byte   $38,$46,$19,$1B,$47,$46,$19,$1B
        .byte   $47,$3B,$19,$1B,$08,$08,$08,$30
        .byte   $08,$08,$31,$4F,$2B,$08,$4F,$4F
        .byte   $08,$08,$4F,$4F,$3F,$08,$08,$08
        .byte   $00,$00,$00,$40,$00,$00,$41,$42
        .byte   $00,$00,$43,$44,$00,$57,$D7,$57
        .byte   $08,$2B,$19,$1B,$08,$38,$19,$1B
        .byte   $00,$48,$19,$1B,$49,$4A,$19,$1B
        .byte   $4B,$4C,$19,$1B,$DF,$3B,$19,$1B
        .byte   $63,$1C,$67,$6C,$08,$08,$30,$37
        .byte   $08,$08,$37,$37,$08,$08,$37,$33
        .byte   $57,$66,$5F,$61,$6D,$6C,$62,$02
        .byte   $66,$57,$61,$57,$30,$37,$57,$6C
        .byte   $37,$37,$66,$67,$54,$03,$66,$01
        .byte   $64,$57,$02,$57,$5F,$02,$1A,$01
        .byte   $64,$5F,$02,$18,$6C,$11,$01,$02
        .byte   $03,$6D,$01,$02,$01,$03,$11,$02
        .byte   $5C,$23,$50,$2B,$6C,$1C,$01,$6F
        .byte   $5C,$57,$50,$57,$08,$57,$56,$57
        .byte   $67,$57,$62,$57,$15,$57,$15,$57
        .byte   $01,$02,$02,$2E,$01,$02,$37,$37
        .byte   $61,$62,$54,$03,$02,$5F,$37,$54
        .byte   $68,$57,$68,$57,$6B,$6B,$68,$68
        .byte   $37,$37,$6B,$6B,$54,$62,$57,$03
        .byte   $11,$57,$02,$5F,$68,$68,$68,$68
        .byte   $34,$35,$6B,$6B,$2E,$37,$6B,$6B
        .byte   $37,$54,$6B,$6B,$68,$57,$68,$2E
        .byte   $3E,$24,$15,$34,$15,$6B,$15,$68
        .byte   $2E,$37,$24,$4F,$37,$37,$4F,$4F
        .byte   $37,$37,$25,$6B,$34,$4E,$6B,$6B
        .byte   $4E,$4E,$6B,$6B,$35,$68,$6B,$68
        .byte   $19,$1B,$63,$1C,$66,$67,$01,$02
        .byte   $25,$62,$35,$37,$76,$77,$74,$75
        .byte   $54,$6B,$6B,$68,$2E,$37,$76,$77
        .byte   $68,$68,$5E,$18,$72,$73,$19,$1B
        .byte   $68,$68,$19,$1B,$72,$73,$74,$75
        .byte   $57,$1C,$5F,$67,$1A,$01,$67,$11
        .byte   $5F,$15,$54,$15,$57,$02,$57,$03
        .byte   $6B,$76,$68,$74,$77,$6B,$75,$68
        .byte   $6B,$15,$68,$15,$57,$11,$57,$01
        .byte   $68,$72,$68,$74,$73,$68,$75,$68
        .byte   $57,$61,$57,$11,$18,$1A,$77,$6B
        .byte   $68,$72,$19,$1A,$18,$1A,$6B,$6B
        .byte   $63,$57,$66,$57,$73,$7B,$7D,$00
        .byte   $68,$68,$6E,$6E,$60,$72,$00,$7C
        .byte   $60,$6E,$00,$00,$7B,$6E,$00,$00
        .byte   $00,$00,$00,$00,$3D,$08,$3D,$08
        .byte   $45,$4D,$08,$08,$03,$57,$02,$5F
        .byte   $19,$1A,$63,$57,$6D,$6C,$61,$62
        .byte   $66,$6F,$11,$64,$66,$67,$11,$03
        .byte   $66,$57,$11,$18,$08,$08,$1A,$3E
        .byte   $03,$6C,$61,$62,$57,$15,$57,$15
        .byte   $08,$08,$1A,$56,$39,$3D,$08,$3D
        .byte   $56,$18,$3D,$08,$19,$19,$39,$3A
        .byte   $19,$19,$3C,$08,$18,$19,$57,$63
        .byte   $57,$6F,$57,$02,$57,$02,$38,$37
        .byte   $01,$02,$54,$03,$08,$5D,$08,$5D
        .byte   $66,$18,$61,$66,$19,$1B,$67,$1C
        .byte   $03,$66,$01,$02,$7A,$85,$14,$80
        .byte   $7E,$7F,$81,$82,$85,$85,$80,$80
        .byte   $85,$5D,$80,$5D,$14,$80,$14,$80
        .byte   $89,$8A,$81,$82,$80,$80,$80,$80
        .byte   $80,$5D,$80,$5D,$67,$66,$03,$03
        .byte   $6D,$6C,$11,$64,$01,$02,$2E,$37
        .byte   $57,$85,$57,$80,$85,$57,$80,$57
        .byte   $57,$8C,$54,$85,$8C,$8C,$85,$85
        .byte   $80,$57,$80,$57,$7A,$80,$14,$80
        .byte   $8C,$57,$85,$57,$1A,$8C,$57,$85
        .byte   $57,$8C,$57,$85,$18,$1B,$6D,$1C
        .byte   $3E,$30,$15,$57,$37,$37,$6C,$66
        .byte   $54,$24,$57,$34,$15,$57,$15,$2E
        .byte   $01,$2E,$02,$57,$37,$54,$2C,$2D
        .byte   $76,$2E,$74,$77,$68,$6B,$68,$68
        .byte   $71,$71,$74,$74,$A7,$A7,$A7,$A7
        .byte   $19,$19,$66,$71,$19,$1A,$08,$08

; --- zero padding ($BAC8-$BAFF) ---
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00

; ===========================================================================
; Metatile nametable data ($BB00-$BEFF)
; ===========================================================================
; Four 256-byte blocks, one per nametable quadrant. Each block contains
; metatile-to-CHR mapping data used during nametable updates. The engine
; reads $BB00,x / $BC00,x / $BD00,x / $BE00,x for the 4 CHR tiles of
; each metatile (top-left, top-right, bottom-left, bottom-right).
; Each block has ~$92 bytes of data followed by zero padding.
; ===========================================================================

; --- nametable quadrant 0 ($BB00): top-left CHR tiles ---
        .byte   $00,$DD,$DE,$10,$EE,$6D,$8B,$7C
        .byte   $01,$69,$6A,$6A,$5C,$10,$21,$7C
        .byte   $60,$56,$CC,$EE,$02,$1C,$01,$BE
        .byte   $8C,$8D,$8D,$8D,$44,$AC,$AE,$01
        .byte   $11,$11,$11,$6E,$3D,$3E,$10,$ED
        .byte   $66,$68,$77,$6E,$2D,$4F,$3D,$3E
        .byte   $3D,$3E,$3E,$3E,$2D,$4F,$5E,$3E
        .byte   $2D,$48,$01,$2D,$49,$0C,$1C,$8C
        .byte   $00,$B4,$B2,$EC,$00,$62,$4E,$5E
        .byte   $E2,$11,$11,$11,$B7,$64,$5E,$3E
        .byte   $10,$10,$5F,$5F,$3E,$7E,$0C,$2D
        .byte   $10,$74,$72,$10,$10,$0E,$3D,$2D
        .byte   $02,$56,$43,$2C,$10,$10,$46,$01
        .byte   $02,$8B,$10,$00,$01,$46,$BF,$01
        .byte   $40,$01,$22,$24,$32,$01,$35,$00
        .byte   $50,$10,$00,$BF,$2E,$2F,$35,$00
        .byte   $02,$32,$24,$23,$03,$00,$6A,$00
        .byte   $12,$32,$01,$01,$02,$00,$32,$01
        .byte   $5C,$DD,$00,$00,$00,$00,$00,$00
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

; --- nametable quadrant 1 ($BC00): top-right CHR tiles ---
        .byte   $00,$DE,$DF,$57,$6C,$EE,$7C,$8B
        .byte   $01,$6A,$6A,$6B,$20,$10,$5D,$7C
        .byte   $CC,$10,$61,$EE,$02,$1D,$01,$11
        .byte   $8E,$8E,$8F,$8E,$45,$AD,$BD,$01
        .byte   $68,$76,$11,$6F,$3E,$3F,$EF,$10
        .byte   $68,$67,$78,$4C,$4E,$2D,$3E,$3E
        .byte   $3E,$3E,$3E,$3F,$4E,$2D,$5E,$3E
        .byte   $2D,$01,$4B,$2D,$01,$0D,$1D,$8F
        .byte   $00,$B5,$B8,$A6,$00,$63,$5E,$4F
        .byte   $C3,$11,$11,$C6,$E7,$65,$5E,$3E
        .byte   $7E,$10,$10,$7E,$3F,$5F,$0D,$2D
        .byte   $10,$75,$73,$10,$10,$0F,$3F,$2D
        .byte   $BF,$42,$57,$2C,$10,$10,$01,$47
        .byte   $02,$10,$8B,$00,$47,$01,$1E,$01
        .byte   $01,$41,$23,$22,$01,$32,$00,$35
        .byte   $10,$51,$00,$02,$1F,$32,$00,$35
        .byte   $02,$23,$32,$24,$EF,$00,$6A,$00
        .byte   $13,$01,$32,$01,$02,$00,$01,$32
        .byte   $DF,$5D,$00,$00,$00,$00,$00,$00
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

; --- nametable quadrant 2 ($BD00): bottom-left CHR tiles ---
        .byte   $00,$56,$10,$FE,$10,$ED,$8B,$10
        .byte   $01,$79,$7A,$7A,$10,$7D,$31,$10
        .byte   $70,$FD,$CD,$10,$02,$1C,$01,$CE
        .byte   $9C,$9D,$9D,$52,$54,$11,$11,$01
        .byte   $11,$11,$11,$6E,$2D,$4F,$5D,$FD
        .byte   $66,$68,$77,$6E,$2D,$4F,$3D,$4E
        .byte   $2D,$4E,$5E,$2D,$3D,$3E,$5E,$3E
        .byte   $3D,$58,$5A,$3E,$4A,$0C,$1C,$9C
        .byte   $00,$C4,$11,$FC,$A7,$72,$3E,$3E
        .byte   $F0,$11,$88,$11,$C7,$74,$3E,$5E
        .byte   $7E,$7E,$01,$01,$3E,$01,$0C,$2D
        .byte   $74,$01,$01,$72,$10,$0E,$2D,$3D
        .byte   $1E,$56,$43,$3C,$FE,$10,$56,$10
        .byte   $02,$8B,$7D,$02,$FE,$FD,$00,$10
        .byte   $50,$10,$32,$34,$32,$01,$32,$01
        .byte   $50,$10,$02,$00,$00,$00,$32,$01
        .byte   $02,$32,$34,$33,$03,$02,$7A,$01
        .byte   $13,$22,$01,$01,$12,$00,$32,$01
        .byte   $10,$ED,$00,$00,$00,$00,$00,$00
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

; --- nametable quadrant 3 ($BE00): bottom-right CHR tiles ---
        .byte   $00,$10,$57,$FF,$EF,$10,$10,$8B
        .byte   $01,$7A,$7A,$7B,$30,$7D,$10,$10
        .byte   $CD,$FE,$71,$10,$02,$1D,$AF,$11
        .byte   $9E,$9E,$9F,$53,$55,$BD,$11,$BC
        .byte   $68,$76,$11,$6F,$4E,$2D,$FF,$5C
        .byte   $68,$67,$78,$4C,$4E,$2D,$3E,$4F
        .byte   $2D,$5E,$4F,$2D,$3E,$3F,$5E,$3E
        .byte   $3E,$59,$5B,$3F,$01,$0D,$1D,$9F
        .byte   $B3,$11,$11,$B6,$00,$73,$3E,$3E
        .byte   $11,$87,$11,$11,$FB,$75,$3E,$5E
        .byte   $01,$7F,$5F,$01,$3F,$01,$0D,$2D
        .byte   $75,$01,$01,$73,$10,$0F,$2D,$3F
        .byte   $00,$42,$57,$3C,$FE,$10,$10,$57
        .byte   $02,$7D,$8B,$02,$FF,$FE,$00,$10
        .byte   $10,$51,$33,$32,$01,$32,$01,$32
        .byte   $10,$51,$02,$1E,$00,$2E,$01,$32
        .byte   $02,$33,$32,$34,$EF,$02,$7A,$01
        .byte   $12,$01,$22,$01,$13,$00,$01,$32
        .byte   $EF,$10,$00,$00,$00,$00,$00,$00
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

; ===========================================================================
; Tile collision attribute table ($BF00-$BFFF)
; ===========================================================================
; 1 byte per metatile index (256 entries). Read by the engine at $BF00,y
; during tile collision checks.
;
; Upper nibble = collision type:
;   $00 = air (passable)
;   $01 = background (passable, visual only)
;   $02 = background variant
;   $03 = hazard / spike (instant kill on contact)
;   $10 = solid (blocks movement)
;   $13 = solid + hazard
;   $23 = ladder
;   $43 = ladder top (player can stand on it)
;
; Lower 2 bits = palette index for attribute table generation.
; ===========================================================================
        .byte   $00,$02,$02,$02,$10,$10,$02,$02
        .byte   $01,$02,$02,$02,$10,$02,$10,$02
        .byte   $10,$02,$10,$10,$13,$23,$01,$01
        .byte   $10,$10,$10,$10,$10,$01,$01,$01
        .byte   $01,$01,$01,$02,$10,$10,$10,$10
        .byte   $01,$01,$01,$02,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$01,$01,$10,$01,$23,$43,$10
        .byte   $03,$03,$03,$03,$03,$01,$10,$10
        .byte   $03,$03,$03,$03,$03,$01,$10,$10
        .byte   $01,$01,$01,$01,$10,$01,$43,$10
        .byte   $01,$01,$01,$01,$01,$10,$10,$10
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$13,$02,$02,$02,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$00,$03,$03
        .byte   $10,$10,$00,$00,$00,$00,$00,$00 ; metatiles $90+ unused (zero)
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
