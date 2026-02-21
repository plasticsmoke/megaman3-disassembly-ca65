; =============================================================================
; MEGA MAN 3 (U) — BANK $18 — STAGE SELECT + PROTO MAN SCENES
; =============================================================================
; Stage select screen logic, cursor handling, stage-to-bank mapping,
; Proto Man encounter cutscenes, and password system.
;
; Annotation: ~41% — 68 labels named, 331 inline comments
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $18 — STAGE SELECT + PROTO MAN SCENES
; =============================================================================
; Mapped to $A000-$BFFF. Contains stage select screen logic (grid layout,
; cursor movement, portrait rendering, palette flash), robot master intro
; transitions, Proto Man encounter scenes, and Wily gate sequences.
;
; Annotation: heavy — stage select thoroughly covered, Proto Man scenes documented
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

L0000           := $0000
L0400           := $0400
L0515           := $0515
L0801           := $0801
L0F06           := $0F06
L0F10           := $0F10
L0F21           := $0F21
L1121           := $1121
L1137           := $1137
L1626           := $1626
L2020           := $2020
L2076           := $2076
L2110           := $2110
L2120           := $2120
L2937           := $2937
L6276           := $6276
L8004           := $8004
L8100           := $8100
L90B4           := $90B4
L9155           := $9155
L9212           := $9212
L9258           := $9258
L9300           := $9300
L939E           := $939E
L93E9           := $93E9
L93FE           := $93FE
L9410           := $9410
L94D6           := $94D6
L954A           := $954A
L9581           := $9581
L9681           := $9681
L968C           := $968C
L96FC           := $96FC
L970B           := $970B
L9762           := $9762
L97EC           := $97EC
L985D           := $985D
L98F2           := $98F2
L9936           := $9936
L995C           := $995C
L99DC           := $99DC
L99FA           := $99FA
L9A58           := $9A58
L9A87           := $9A87
L9ABC           := $9ABC
L9ABE           := $9ABE
LC4F8           := $C4F8
LC531           := $C531
LC53B           := $C53B
LC59D           := $C59D
LC5E9           := $C5E9
LC628           := $C628
LC74C           := $C74C
LC752           := $C752
LC9B3           := $C9B3
LE4F1           := $E4F1
LE8B4           := $E8B4
LEEAB           := $EEAB
LEF8C           := $EF8C
LF81B           := $F81B
LF835           := $F835
LF898           := $F898
LF89A           := $F89A
LFD6E           := $FD6E
LFF1A           := $FF1A
LFF21           := $FF21
LFF3C           := $FF3C
LFF45           := $FF45
LFF6B           := $FF6B

.segment "BANK18"

LA000:  .byte   $28,$30,$50,$70,$01,$90,$0D,$00
        .byte   $AF,$8D,$88,$12,$28,$C0,$16,$CA
        .byte   $8A,$8D,$0E,$01,$BF,$EB,$02,$00
        .byte   $CA,$17,$17,$17,$17,$00,$C0,$26
        .byte   $C2,$AD,$C4,$0A,$C5,$32,$0A,$FE
        .byte   $05,$01,$11,$06,$C8,$07,$0C,$08
        .byte   $26,$18,$80,$09,$03,$00,$03,$8A
        .byte   $8D,$04,$48,$2D,$0D,$28,$2F,$4F
        .byte   $02,$8F,$00,$01,$AF,$8D,$D1,$0D
        .byte   $00,$8A,$8D,$01,$2D,$0D,$28,$30
        .byte   $50,$70,$01,$90,$0D,$00,$AF,$8D
        .byte   $88,$12,$28,$C0,$64,$CA,$8A,$8D
        .byte   $0E,$01,$C0,$39,$05,$00,$CC,$AA
        .byte   $05,$00,$88,$AD,$05,$00,$66,$AF
        .byte   $04,$48,$06,$64,$08,$11,$05,$01
        .byte   $11,$2F,$0D,$28,$31,$51,$02,$01
        .byte   $91,$0D,$00,$6A,$6D,$71,$01,$2D
        .byte   $0D,$28,$2F,$4F,$01,$8F,$0D,$00
        .byte   $8D,$6C,$02,$8D,$12,$08,$C0,$AF
        .byte   $8A,$6A,$8C,$6C,$0D,$28,$01,$2B
        .byte   $2D,$4D,$02,$01,$8D,$0D,$00,$8C
        .byte   $6A,$02,$8C,$0E,$01,$C0,$70,$01
        .byte   $2F,$0D,$28,$31,$51,$01,$71,$0D
        .byte   $00,$71,$8F,$01,$6F,$0D,$28,$01
        .byte   $B1,$0D,$00,$09,$02,$08,$20,$6A
        .byte   $6D,$71,$6F,$6D,$6C,$08,$11,$04
        .byte   $48,$09,$03,$2F,$0D,$28,$31,$51
        .byte   $02,$01,$91,$0D,$00,$6A,$6D,$71
        .byte   $01,$2D,$0D,$28,$2F,$4F,$01,$8F
        .byte   $0D,$00,$8D,$6C,$02,$8D,$12,$08
        .byte   $C1,$09,$8A,$6A,$8C,$6C,$0D,$28
        .byte   $01,$2B,$2D,$4D,$02,$01,$8D,$0D
        .byte   $00,$8C,$6A,$02,$8C,$0E,$01,$C0
        .byte   $CF,$09,$02,$01,$34,$0D,$28,$36
        .byte   $56,$01,$76,$76,$98,$78,$09,$03
        .byte   $01,$CD,$01,$8D,$0D,$00,$09,$02
        .byte   $08,$27,$00,$26,$2A,$2D,$00,$01
        .byte   $51,$02,$01,$D1,$02,$80,$04,$00
        .byte   $08,$11,$18,$80,$09,$03,$05,$01
        .byte   $11,$04,$00,$A0,$60,$76,$79,$7B
        .byte   $00,$5B,$0D,$3C,$00,$9C,$02,$9B
        .byte   $00,$03,$88,$86,$12,$28,$C1,$83
        .byte   $0D,$00,$00,$A5,$60,$00,$86,$88
        .byte   $00,$85,$61,$02,$81,$00,$40,$00
        .byte   $61,$63,$0D,$3C,$A4,$60,$00,$86
        .byte   $88,$0D,$00,$00,$83,$03,$77,$02
        .byte   $97,$00,$40,$00,$74,$77,$B9,$60
        .byte   $79,$7B,$79,$98,$75,$B1,$60,$0E
        .byte   $01,$C1,$39,$00,$A5,$60,$00,$86
        .byte   $88,$0D,$00,$00,$8A,$66,$02,$83
        .byte   $00,$40,$00,$03,$76,$78,$B9,$60
        .byte   $79,$7B,$79,$98,$96,$94,$98,$02
        .byte   $D6,$80,$7D,$7E,$04,$40,$3E,$0D
        .byte   $28,$03,$28,$48,$01,$68,$0D,$00
        .byte   $68,$88,$88,$02,$88,$86,$85,$83
        .byte   $03,$98,$79,$98,$96,$B4,$02,$80
        .byte   $12,$00,$C1,$CA,$7D,$7E,$0E,$01
        .byte   $C1,$A4,$03,$68,$69,$01,$29,$0D
        .byte   $28,$2B,$4B,$01,$6B,$0D,$00,$6B
        .byte   $8B,$8B,$02,$8B,$89,$88,$86,$83
        .byte   $64,$83,$03,$97,$02,$99,$99,$9B
        .byte   $9C,$D7,$80,$01,$03,$29,$0D,$28
        .byte   $2B,$4B,$01,$6B,$0D,$00,$89,$88
        .byte   $A6,$60,$66,$68,$66,$C5,$A0,$60
        .byte   $03,$76,$79,$7B,$00,$5B,$0D,$3C
        .byte   $00,$9C,$02,$9B,$00,$03,$88,$86
        .byte   $0D,$00,$00,$A5,$60,$00,$86,$88
        .byte   $00,$8A,$66,$02,$83,$00,$40,$00
        .byte   $03,$76,$78,$B9,$60,$79,$7B,$79
        .byte   $98,$96,$94,$98,$D6,$09,$01,$08
        .byte   $1C
LA231:  .byte   $03
LA232:  .byte   $71
LA233:  .byte   $71
LA234:  .byte   $6F,$91,$09,$02,$6A,$72,$71,$04
        .byte   $08,$08,$20,$B2,$60,$08,$1C,$6A
        .byte   $72,$71,$08,$20,$B2,$60,$08,$1C
        .byte   $68,$72,$71,$94,$72,$92,$91,$6F
        .byte   $6F,$12,$08,$C2,$63,$6D,$6F,$91
        .byte   $68,$72,$71,$0E,$01,$C2,$3B,$71
        .byte   $6A,$88,$08,$1C,$66,$66,$65,$08
        .byte   $20,$A5,$60,$08,$1C,$66,$66,$65
        .byte   $08,$20,$A5,$60,$65,$6A,$6C,$AF
        .byte   $8D,$6C,$AD,$02,$80,$6D,$6F,$B1
        .byte   $60,$6D,$6D,$6F,$91,$8F,$91,$92
        .byte   $8F,$05,$01,$00,$8C,$05,$00,$F0
        .byte   $8C,$05,$00,$DF,$88,$05,$00,$CC
        .byte   $88,$05,$00,$BD,$85,$05,$00,$88
        .byte   $86,$05,$00,$44,$88,$16,$C1,$2E
        .byte   $17,$06,$64,$07,$0C,$08,$20,$18
        .byte   $80,$09,$01,$00,$A0,$00,$E0,$E0
        .byte   $E0,$E0,$04,$08,$AA,$60,$71,$6D
        .byte   $A8,$A0,$60,$12,$08,$C2,$D8,$86
        .byte   $66,$88,$68,$02,$AA,$66,$02,$88
        .byte   $0E,$01,$C2,$BE,$8D,$6D,$8C,$6C
        .byte   $01,$CD,$01,$8D,$04,$08,$AA,$60
        .byte   $71,$6D,$A8,$A0,$60,$12,$08,$C2
        .byte   $FA,$86,$66,$88,$68,$02,$AA,$66
        .byte   $02,$88,$0E,$01,$C2,$E0,$8D,$6D
        .byte   $8F,$6F,$01,$D1,$01,$91,$E0,$04
        .byte   $00,$18,$40,$04,$00,$96,$99,$9D
        .byte   $99,$95,$98,$9D,$98,$94,$97,$99
        .byte   $9D,$12,$00,$C3,$31,$92,$96,$99
        .byte   $9D,$92,$95,$99,$9C,$90,$94,$97
        .byte   $9B,$8F,$92,$96,$99,$91,$95,$98
        .byte   $95,$0E,$01,$C3,$07,$93,$96,$99
        .byte   $9D,$92,$96,$99,$9D,$94,$98,$9B
        .byte   $9D,$8A,$8E,$91,$96,$9A,$9D,$03
        .byte   $8A,$8E,$04,$00,$8F,$92,$96,$99
        .byte   $8F,$94,$98,$94,$8D,$91,$94,$91
        .byte   $8D,$91,$94,$91,$0E,$01,$C3,$46
        .byte   $92,$95,$99,$9C,$8B,$8F,$92,$95
        .byte   $90,$94,$97,$9B,$93,$96,$99,$9C
        .byte   $92,$95,$99,$9C,$92,$95,$99,$9C
        .byte   $99,$9E,$03,$88,$86,$81,$85,$88
        .byte   $85,$03,$96,$99,$9D,$99,$95,$98
        .byte   $9D,$98,$94,$97,$99,$9D,$93,$96
        .byte   $99,$9D,$92,$96,$99,$9D,$94,$98
        .byte   $9B,$9D,$8A,$8E,$91,$96,$08,$1C
        .byte   $03,$76,$76,$76,$01,$76,$01,$B6
        .byte   $18,$80,$04,$00,$08,$20,$6F,$72
        .byte   $76,$79,$7B,$7E,$03,$6A,$66,$03
        .byte   $74,$78,$7B,$7E,$03,$68,$6C,$6F
        .byte   $6C,$03,$6D,$71,$74,$78,$79,$7D
        .byte   $03,$68,$6D,$71,$6D,$68,$65,$61
        .byte   $03,$76,$72,$71,$0E,$01,$C3,$A6
        .byte   $04,$00,$71,$75,$78,$7B,$7E,$7B
        .byte   $78,$75,$0E,$01,$C3,$D4,$76,$79
        .byte   $7D,$79,$74,$78,$7B,$78,$72,$76
        .byte   $79,$7D,$7E,$03,$6A,$6D,$71,$18
        .byte   $40,$03,$8F,$92,$96,$99,$8F,$92
        .byte   $96,$99,$94,$98,$9B,$9E,$94,$98
        .byte   $9B,$9E,$16,$C3,$03,$17,$06,$B4
        .byte   $08,$00,$09,$03,$00,$A0,$00,$E0
        .byte   $E0,$E0,$E0,$01,$AA,$02,$01,$8A
        .byte   $01,$C5,$01,$65,$02,$86,$02,$88
        .byte   $02,$AA,$A8,$01,$A6,$02,$01,$86
        .byte   $01,$C5,$01,$65,$02,$86,$02,$88
        .byte   $AA,$8A,$91,$88,$01,$A6,$02,$01
        .byte   $86,$01,$C5,$01,$65,$02,$86,$02
        .byte   $88,$02,$AA,$A8,$01,$A6,$02,$01
        .byte   $86,$01,$C5,$01,$65,$02,$83,$02
        .byte   $85,$01,$C6,$01,$86,$C0,$80,$88
        .byte   $8D,$8C,$04,$40,$AA,$01,$6A,$6A
        .byte   $6D,$6A,$02,$A9,$89,$01,$A8,$01
        .byte   $68,$68,$6D,$68,$02,$A6,$86,$01
        .byte   $A6,$01,$66,$69,$68,$66,$02,$A4
        .byte   $84,$01,$A3,$01,$63,$63,$68,$66
        .byte   $02,$A5,$65,$68,$04,$40,$AA,$01
        .byte   $6A,$6A,$6D,$6A,$02,$A9,$89,$01
        .byte   $A8,$01,$68,$68,$6D,$68,$02,$A7
        .byte   $87,$01,$A6,$01,$66,$66,$6D,$66
        .byte   $A8,$8D,$8C,$01,$CA,$01,$8A,$6E
        .byte   $6F,$71,$70,$71,$68,$13,$00,$C4
        .byte   $F2,$04,$40,$A3,$01,$63,$63,$65
        .byte   $66,$02,$A8,$68,$6A,$01,$A1,$01
        .byte   $61,$81,$61,$C1,$0E,$01,$C4,$B5
        .byte   $01,$A6,$01,$66,$66,$68,$69,$02
        .byte   $AB,$6B,$69,$02,$A4,$64,$66,$02
        .byte   $A7,$6B,$69,$02,$A6,$6B,$6D,$8B
        .byte   $02,$A6,$01,$A8,$01,$68,$68,$6B
        .byte   $6C,$CD,$0F,$01,$C4,$88,$04,$40
        .byte   $A3,$01,$63,$63,$65,$66,$02,$A8
        .byte   $68,$6A,$01,$A1,$01,$61,$81,$61
        .byte   $02,$A1,$61,$63,$0E,$01,$C4,$F2
        .byte   $01,$A5,$01,$65,$65,$66,$68,$01
        .byte   $A5,$01,$65,$65,$6D,$6C,$AA,$A8
        .byte   $02,$A6,$66,$65,$02,$A3,$83,$01
        .byte   $A3,$01,$63,$63,$65,$66,$A8,$AA
        .byte   $AC,$AF,$16,$C4,$5E,$17,$17,$06
        .byte   $02,$6F,$03,$0A,$1F,$35,$40,$0F
        .byte   $90,$08,$41,$1D,$31,$0F,$2A,$7F
        .byte   $10,$02,$82,$02,$0A,$00,$41,$00
        .byte   $10,$FF,$14,$02,$FF,$03,$02,$07
        .byte   $28,$C0,$0F,$3E,$02,$FF,$03,$02
        .byte   $00,$40,$02,$FF,$03,$02,$00,$42
        .byte   $02,$FF,$03,$02,$00,$43,$02,$FF
        .byte   $03
LA56D:  .byte   $02,$00,$45,$02,$FF,$03,$02,$00
        .byte   $47,$02,$FF,$03,$02,$00,$49,$02
        .byte   $FF,$03,$02
LA580:  .byte   $00,$4A,$02,$FF,$03,$02,$04,$08
        .byte   $3E,$02,$FF,$03,$02,$00,$40,$02
        .byte   $FF,$03,$02
LA593:  .byte   $00,$42,$02,$FF,$03,$02,$00,$43
        .byte   $02,$FF,$03,$02,$00,$45,$02,$FF
        .byte   $03,$02,$00,$47,$02,$FF,$03,$02
        .byte   $00,$49,$02,$FF,$03,$02,$00,$4A
        .byte   $FF,$07,$02,$EB,$0A,$02,$0F,$36
        .byte   $40,$0F,$FF,$33,$FF,$10,$02,$6F
        .byte   $03,$0A,$0F,$2C,$C0,$0F,$40,$14
        .byte   $0D,$2D,$0F,$7F,$06,$02,$FF,$0B
        .byte   $0A,$0D,$2F,$08,$7F,$30,$09,$2E
        .byte   $FF,$07,$FF,$15,$02,$FF,$0E,$03
        .byte   $0F,$2A,$80,$0F,$7F,$3E,$0F,$2A
        .byte   $40,$0F,$7F,$3E,$02,$FF,$0E,$03
        .byte   $04,$0C,$3E,$04,$0C,$3E,$02,$FF
        .byte   $0E,$03,$04,$09,$3E,$04,$09,$3E
        .byte   $02,$FF,$0E,$03,$04,$06,$3E,$04
        .byte   $06,$3E,$02,$FF,$0E,$03,$04,$03
        .byte   $3E,$04,$03,$3E,$02,$FF,$0E,$03
        .byte   $04,$01,$3E,$04,$01,$3E,$FF,$09
        .byte   $02,$C8,$1C,$08,$0D,$32,$0F,$FF
        .byte   $0E,$FF,$09,$02,$FF,$04,$06,$07
        .byte   $28,$00,$0F,$52,$0D,$28,$AF,$00
        .byte   $5E,$FF,$15,$02,$FF,$04,$02,$07
        .byte   $28,$80,$0F,$2E,$02,$FF,$04,$02
        .byte   $00,$29,$02,$FF,$04,$02,$00,$26
        .byte   $02,$FF,$04,$02,$00,$3A,$FF,$02
        .byte   $02,$FF,$05,$02,$07,$28,$C0,$0F
        .byte   $39,$02,$FF,$08,$02,$02,$40,$32
        .byte   $FF,$09,$02,$FF,$04,$03,$07,$29
        .byte   $40,$0F,$3D,$07,$29,$40,$0F,$41
        .byte   $01,$02,$C6,$6D,$00,$00,$FF,$08
        .byte   $02,$FF,$02,$0A,$0F,$2C,$80,$0F
        .byte   $6E,$59,$0D,$3C,$0F,$4A,$09,$02
        .byte   $FF,$04,$00,$03,$02,$C6,$83,$FF
        .byte   $02,$0A,$0F,$2C,$80,$0F,$6E,$59
        .byte   $0D,$3C,$0F,$4A,$09,$FF,$08,$02
        .byte   $51,$05,$0E,$0F,$01,$80,$0F,$7F
        .byte   $33,$0D,$31,$FF,$7F,$3D,$0D,$28
        .byte   $0F,$FF,$04,$02,$51,$05,$04,$04
        .byte   $00,$34,$FF,$08,$02,$FF,$04,$08
        .byte   $0D,$28,$0F,$7F,$06,$02,$FF,$16
        .byte   $08,$08,$3C,$10,$FF,$08,$02,$51
        .byte   $08,$0C,$0D,$31,$FF,$7F,$38,$0D
        .byte   $28,$0F,$FF,$04,$02,$EB,$06,$06
        .byte   $0F,$2C,$80,$0F,$7F,$21,$0D,$2C
        .byte   $FF,$FF,$2D,$FF,$08,$02,$FF,$07
        .byte   $04,$05,$29,$9F,$5F,$02,$FF,$07
        .byte   $04,$04,$00,$5F,$02,$FF,$07,$04
        .byte   $04,$00,$5F,$FF,$08,$02,$FF,$07
        .byte   $0E,$0F,$2C,$C0,$0F,$7F,$28,$0D
        .byte   $2B,$8C,$7F,$5F,$0D,$32,$0F,$7F
        .byte   $10,$02,$FF,$03,$0E,$08,$FF,$4E
        .byte   $0C,$00,$FF,$5F,$08,$FF,$10,$FF
        .byte   $08,$02,$FF,$2F,$0E,$0F,$28,$80
        .byte   $0F,$87,$21,$0D,$28,$FF,$87,$2D
        .byte   $0D,$28,$0F,$88,$0C,$FF,$07,$02
        .byte   $FF,$1E,$0C,$05,$34,$FF,$2F,$05
        .byte   $28,$0F,$01,$FF,$08,$02,$FF,$0C
        .byte   $06,$1F,$32,$80,$0F,$FF,$FF,$53
        .byte   $05,$32,$74,$5F,$FF,$09,$02,$FF
        .byte   $06,$0A,$05,$32,$0F,$47,$05,$32
        .byte   $0C,$06,$FF,$11,$02,$96,$05,$08
        .byte   $05,$28,$0F,$03,$02,$28,$28,$08
        .byte   $09,$30,$AA,$03,$FF,$08,$02,$FF
        .byte   $0A,$06,$17,$3B,$00,$0F,$07,$58
        .byte   $05,$3B,$FF,$4C,$02,$FF,$0A,$06
        .byte   $00,$58,$04,$00,$4C,$FF,$11,$02
        .byte   $EB,$08,$08,$05,$2F,$0F,$03,$02
        .byte   $EB,$00,$08,$00,$03,$02,$EB,$08
        .byte   $08,$00,$03,$02,$EB,$08,$08,$00
        .byte   $03,$02,$EB,$08,$08,$00,$03,$02
        .byte   $EB,$08,$08,$00,$03,$02,$EB,$08
        .byte   $08,$00,$03,$02,$EB,$08,$08,$00
        .byte   $03,$02,$EB,$08,$08,$00,$03,$02
        .byte   $EB,$08,$08,$00,$03,$02,$EB,$08
        .byte   $08,$00,$03,$02,$EB,$08,$08,$00
        .byte   $03,$FF,$07,$02,$AB,$03,$0A,$05
        .byte   $33,$0F,$19,$05,$33,$0F,$08,$02
        .byte   $A9,$25,$08,$09,$38,$2C,$0D,$FF
        .byte   $07,$02,$FF,$17,$0A,$17,$29,$00
        .byte   $06,$02,$2D,$0D,$32,$0F,$01,$0D
        .byte   $FF,$07,$02,$BF,$04,$08,$05,$2E
        .byte   $0F,$0A,$03,$02,$C8,$15,$01,$01
        .byte   $00,$FF,$09,$02,$96,$04,$08,$05
        .byte   $32,$0F,$09,$02,$B4,$04,$08,$01
        .byte   $28,$09,$01,$01,$C8,$26,$00,$00
        .byte   $FF,$07,$02,$FF,$1B,$0A,$0D,$32
        .byte   $0F,$88,$40,$0D,$32,$0F,$98,$07
        .byte   $FF,$07,$02,$6E,$04,$0A,$0F,$2C
        .byte   $00,$0F,$A1,$24,$0D,$30,$0F,$93
        .byte   $07,$02,$28,$28,$08,$09,$37,$9C
        .byte   $02,$FF,$08,$02,$FF,$10,$0E,$0D
        .byte   $35,$0F,$63,$44,$0D,$28,$E6,$7F
        .byte   $38,$0D,$32,$0F,$2E,$03
LA879:  .byte   $02,$FF,$0A,$0E,$04,$05,$58,$04
        .byte   $A1,$31,$04,$05,$03,$FF,$08,$02
        .byte   $FF,$03,$02,$0F,$3A,$80,$0F,$FF
        .byte   $4F,$02,$05,$1D,$02,$01,$37,$54
        .byte   $FF,$08,$02,$E1,$32,$02,$0F,$39
        .byte   $80,$0F,$8F,$3E,$FF,$00,$C8,$AF
        .byte   $C8,$EE,$C9,$37,$C9,$7E,$05,$02
        .byte   $00,$06,$C8,$07,$0C,$08,$06,$09
        .byte   $01,$18,$40,$03,$AD,$60,$6D,$6C
        .byte   $6D,$8F,$91,$92,$8F,$B1,$B4,$B6
        .byte   $B3,$09,$01,$08,$00,$07,$0A,$65
        .byte   $65,$60,$68,$68,$60,$6A,$6A,$60
        .byte   $6C,$6C,$60,$6F
LA8DD:  .byte   $60,$70,$60,$71,$71,$74,$73,$6F
        .byte   $71,$60,$71,$74,$73,$6F,$02,$71
        .byte   $17,$06,$C8,$07,$0A,$08,$06,$09
        .byte   $02,$18,$40,$76,$71,$6F,$6A,$60
        .byte   $6A,$74,$76,$98,$99,$9B,$98,$79
        .byte   $79,$60,$79,$79,$79,$60,$79,$7B
        .byte   $7B,$60,$7B,$7B,$7B,$60,$7B,$0C
        .byte   $FE,$08,$00,$7D,$7D,$60,$03,$68
        .byte   $68,$60,$6A,$6A,$60,$6C,$6C,$60
        .byte   $6F,$60,$70,$60,$65,$65,$68,$67
        .byte   $63,$65,$60,$65,$68,$67,$63,$02
        .byte   $65,$17,$06,$C8,$08,$00,$09,$03
        .byte   $6A,$6A,$6A,$6A,$A0,$08,$0E,$06
        .byte   $E6,$79,$79,$96,$79,$79,$96,$08
        .byte   $00,$06,$C8,$6D,$60,$6D,$6D,$6D
        .byte   $60,$6D,$6D,$6F,$60,$6F,$6F,$6F
        .byte   $60,$6F,$6F,$65,$65,$60,$68,$68
        .byte   $60,$6A,$6A,$60,$6C,$6C,$60,$6F
        .byte   $60,$70,$60,$71,$71,$74,$73,$6F
        .byte   $71,$60,$71,$74,$73,$6F,$02,$71
        .byte   $17,$06,$C8,$07,$0A,$08,$0C,$6C
        .byte   $6C,$6C,$6C,$02,$C0,$04,$00,$65
        .byte   $6D,$6C,$6D,$6A,$6D,$6C,$6D,$0E
        .byte   $01,$C9,$8A,$04,$00,$6C,$6C,$60
        .byte   $0E,$03,$C9,$98,$6C,$60,$6C,$60
        .byte   $6C,$6C,$6C,$6C,$6C,$6C,$60,$6C
        .byte   $6C,$6C,$6C,$6C,$80,$17,$01,$02
        .byte   $FF,$06,$0A,$0F,$39,$C0,$0F,$FF
        .byte   $40,$0D,$39,$06,$7F,$10,$02,$FF
        .byte   $01,$02,$00,$48,$FF,$00,$C9,$D3
        .byte   $CA,$4B,$CA,$C7,$CB,$2B,$0A,$FD
        .byte   $05,$02,$00,$07,$0C,$18,$C0,$04
        .byte   $08,$04,$08,$09,$01,$08,$1D,$06
        .byte   $C8,$02,$B1,$6F,$6D,$02,$AF,$6D
        .byte   $6C,$02,$AD,$6C,$6A,$8C,$8A,$88
        .byte   $8A,$6A,$60,$71,$6F,$02,$01,$D1
        .byte   $08,$01,$06,$F5,$01,$F1,$0E,$01
        .byte   $C9,$DE,$04,$08,$06,$C8,$08,$1D
        .byte   $02,$AA,$6A,$71,$8F,$8D,$8C,$8D
        .byte   $AA,$02,$8C,$6D,$6C,$60,$02,$AA
        .byte   $12,$08,$CA,$35,$02,$AA,$6A,$71
        .byte   $94,$92,$91,$8F,$B1,$02,$92,$74
        .byte   $72,$60,$02,$B1,$0E,$01,$CA,$07
        .byte   $09,$02,$02,$AA,$6C,$6D,$8C,$8A
        .byte   $88,$85,$AA,$02,$8C,$6D,$6C,$60
        .byte   $02,$AA,$16,$C9,$DC,$17,$06,$C8
        .byte   $07,$0C,$08,$06,$09,$02,$04,$00
        .byte   $04,$00,$04,$00,$96,$99,$98,$99
        .byte   $0E,$05,$CA,$57,$76,$79,$78,$76
        .byte   $7E,$79,$76,$03,$68,$63,$03,$78
        .byte   $03,$6A,$65,$61,$6D,$6C,$68,$0F
        .byte   $01,$CA,$55,$04,$00,$0C,$00,$09
        .byte   $01,$9E,$9B,$9D,$9E,$03,$89,$85
        .byte   $87,$89,$0C,$FE,$AA,$02,$8C,$6D
        .byte   $6C,$60,$02,$AA,$12,$08,$CA,$AD
        .byte   $0C,$00,$86,$83,$85,$86,$8C,$89
        .byte   $8A,$8C,$B1,$0C,$FE,$02,$92,$74
        .byte   $72,$60,$02,$B1,$0E,$01,$CA,$78
        .byte   $0C,$00,$09,$02,$8A,$86,$88,$8A
        .byte   $8C,$88,$8A,$8C,$0C,$FE,$AA,$02
        .byte   $8C,$6D,$6C,$60,$02,$AA,$16,$CA
        .byte   $53,$17,$06,$C8,$08,$00,$09,$03
        .byte   $04,$00,$04,$00,$04,$00,$6A,$60
        .byte   $6A,$6A,$0E,$07,$CA,$D1,$04,$00
        .byte   $66,$60,$66,$66,$0E,$06,$CA,$DB
        .byte   $68,$60,$68,$68,$0F,$01,$CA,$CF
        .byte   $04,$00,$63,$60,$63,$63,$63,$60
        .byte   $63,$63,$65,$60,$65,$65,$65,$60
        .byte   $65,$65,$04,$00,$6A,$60,$6A,$6A
        .byte   $0E,$03,$CA,$FF,$0F,$02,$CA,$ED
        .byte   $66,$60,$66,$66,$66,$60,$66,$66
        .byte   $68,$60,$68,$68,$68,$60,$68,$68
        .byte   $04,$00,$6A,$60,$6A,$6A,$0E,$03
        .byte   $CB,$1D,$16,$CA,$CD,$17,$04,$00
        .byte   $04,$00,$08,$0C,$07,$08,$06,$C8
        .byte   $65,$6C,$08,$03,$06,$F0,$8D,$08
        .byte   $0C,$06,$C8,$6A,$6C,$08,$03,$06
        .byte   $F0,$8D,$0E,$0F,$CB,$2D,$04,$00
        .byte   $08,$0C,$6D,$6D,$6D,$6D,$68,$6D
        .byte   $6D,$6D,$0E,$0E,$CB,$4B,$65,$6C
        .byte   $68,$65,$6C,$68,$65,$6C,$16,$CB
        .byte   $2B,$17,$00,$CB,$70,$CB,$F6,$CC
        .byte   $7B,$CC,$CB,$0A,$03,$05,$02,$00
        .byte   $06,$D2,$07,$0A,$08,$16,$09,$01
        .byte   $18,$40,$83,$86,$89,$8C,$89,$86
        .byte   $8F,$92,$95,$98,$95,$92,$9B,$9E
        .byte   $03,$89,$8C,$89,$86,$8F,$92,$95
        .byte   $98,$95,$92,$04,$08,$06,$78,$07
        .byte   $0A,$6F,$70,$0E,$0B,$CB,$98,$08
        .byte   $06,$06,$96,$07,$0C,$09,$02,$03
        .byte   $96,$76,$79,$96,$03,$88,$86,$85
        .byte   $84,$83,$81,$83,$81,$03,$96,$96
        .byte   $76,$79,$96,$9C,$9B,$99,$9B,$99
        .byte   $96,$02,$B6,$03,$8A,$6A,$68,$8A
        .byte   $8D,$8C,$88,$88,$86,$85,$83,$85
        .byte   $81,$03,$96,$9C,$9B,$99,$98,$94
        .byte   $96,$99,$9D,$08,$22,$01,$03,$AA
        .byte   $07,$0A,$AA,$07,$08,$AA,$07,$06
        .byte   $AA,$07,$04,$AA,$07,$02,$01,$AA
        .byte   $17,$06,$D2,$07,$0A,$08,$16,$09
        .byte   $01,$0C,$01,$18,$40,$83,$86,$89
        .byte   $8C,$89,$86,$8F,$92,$95,$98,$95
        .byte   $92,$9B,$9E,$03,$89,$8C,$89,$86
        .byte   $8F,$92,$95,$98,$95,$92,$09,$02
        .byte   $08,$01,$0C,$00,$07,$08,$06,$FA
        .byte   $02,$03,$B6,$02,$B0,$02,$B1,$02
        .byte   $A9,$08,$06,$06,$96,$04,$00,$07
        .byte   $0C,$8A,$6D,$6F,$90,$0E,$02,$CC
        .byte   $32,$8F,$8D,$8A,$04,$00,$8A,$6D
        .byte   $6C,$8A,$0E,$02,$CC,$41,$8F,$8D
        .byte   $8C,$04,$00,$8A,$6D,$6F,$90,$0E
        .byte   $03,$CC,$4E,$0C,$02,$96,$9C,$9B
        .byte   $99,$98,$94,$96,$99,$9D,$08,$22
        .byte   $09,$01,$01,$03,$AA,$07,$0A,$AA
        .byte   $07,$08,$AA,$07,$06,$AA,$07,$04
        .byte   $AA,$07,$02,$01,$AA,$17,$06,$C8
        .byte   $08,$00,$09,$03,$04,$00,$63,$60
        .byte   $63,$63,$63,$60,$0E,$07,$CC,$81
        .byte   $09,$02,$08,$01,$06,$FA,$02,$B6
        .byte   $02,$B0,$02,$B1,$02,$A9,$09,$03
        .byte   $06,$C8,$08,$00,$8A,$8A,$8A,$8A
        .byte   $8A,$8A,$86,$86,$86,$86,$86,$86
        .byte   $83,$83,$83,$88,$88,$88,$04,$00
        .byte   $8A,$0E,$0B,$CC,$B3,$86,$86,$86
        .byte   $86,$86,$86,$8A,$90,$8F,$8D,$8C
        .byte   $88,$8A,$8D,$91,$96,$17,$06,$FF
        .byte   $07,$0C,$08,$09,$02,$CF,$02,$CC
        .byte   $02,$C8,$02,$C5,$E0,$C0,$04,$00
        .byte   $08,$0C,$06,$C8,$07,$0A,$6C,$60
        .byte   $6C,$6C,$6C,$60,$0E,$06,$CC,$DB
        .byte   $6C,$6C,$6C,$6C,$6C,$6C,$04,$00
        .byte   $6C,$60,$6C,$6C,$6C,$60,$0E,$05
        .byte   $CC,$F3,$6C,$6C,$6C,$6C,$6C,$6C
        .byte   $08,$09,$06,$FF,$07,$0C,$01,$A8
        .byte   $07,$0A,$A8,$07,$08,$A8,$07,$06
        .byte   $A8,$07,$04,$A8,$07,$02,$01,$A8
        .byte   $17,$00,$CD,$27,$CD,$5C,$CD,$90
        .byte   $CD,$BE,$05,$02,$00,$06,$C8,$07
        .byte   $0C,$08,$00,$09,$01,$18,$40,$7B
        .byte   $7F,$03,$6A,$6E,$60,$6C,$60,$6A
        .byte   $6C,$60,$6A,$60,$67,$02,$8A,$66
        .byte   $6A,$6D,$71,$60,$6F,$60,$6D,$6F
        .byte   $60,$6D,$60,$6C,$6D,$6C,$68,$70
        .byte   $70,$60,$72,$72,$60,$F4,$17,$06
        .byte   $C8,$07,$0C,$08,$00,$18,$40,$09
        .byte   $01,$73,$76,$7A,$7D,$60,$7B,$60
        .byte   $7A,$7B,$60,$7A,$60,$76,$02,$9A
        .byte   $76,$79,$7D,$03,$68,$60,$66,$60
        .byte   $65,$66,$60,$65,$60,$63,$65,$63
        .byte   $03,$78,$03,$6D,$6D,$60,$6F,$6F
        .byte   $60,$F1,$17,$06,$C8,$08,$00,$09
        .byte   $02,$7B,$7B,$7B,$7B,$60,$7B,$60
        .byte   $7B,$7B,$60,$7B,$60,$7B,$02,$9B
        .byte   $79,$79,$79,$79,$60,$79,$60,$79
        .byte   $03,$68,$60,$68,$60,$68,$68,$68
        .byte   $68,$69,$69,$60,$6B,$6B,$60,$ED
        .byte   $17,$17,$00,$CD,$C8,$CD,$E9,$CE
        .byte   $05,$CE,$3B,$0A,$06,$05,$02,$2E
        .byte   $06,$7D,$07,$0C,$08,$00,$18,$C0
        .byte   $C0,$03,$70,$70,$6F,$70,$C0,$06
        .byte   $96,$70,$72,$60,$01,$74,$D4,$08
        .byte   $01,$01,$D4,$17,$06,$7D,$07,$0C
        .byte   $08,$00,$18,$40,$C0,$03,$6D,$6D
        .byte   $6C,$6D,$C0,$6D,$6F,$60,$06,$96
        .byte   $01,$71,$D1,$08,$01,$01,$D1,$17
        .byte   $06,$C8,$08,$0E,$09,$02,$00,$78
        .byte   $76,$74,$00,$03,$88,$00,$03,$78
        .byte   $76,$74,$00,$03,$88,$08,$00,$69
        .byte   $69,$68,$69,$08,$0E,$06,$E6,$09
        .byte   $02,$68,$68,$82,$68,$68,$81,$08
        .byte   $00,$69,$6B,$60,$06,$96,$01,$6D
        .byte   $CD,$08,$01,$01,$CD,$17,$17,$00
        .byte   $00,$00,$00,$20,$00,$24,$40,$11
        .byte   $00,$30,$00,$00,$00,$00,$00,$01
        .byte   $00,$84,$00,$00,$40,$00,$00,$80
        .byte   $80,$20,$00,$12,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$04,$00,$22
        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $01,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$80
        .byte   $00,$00,$00,$48,$01,$00,$00,$10
        .byte   $00,$01,$00,$20,$40,$00,$00,$10
        .byte   $00,$02,$00,$00,$00,$00,$04,$00
        .byte   $00,$04,$40,$82,$00,$00,$00,$1A
        .byte   $04,$40,$00,$C2,$01,$08,$00,$00
        .byte   $00,$00,$10,$00,$00,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$00,$00,$14
        .byte   $00,$00,$00,$40,$04,$00,$40,$10
        .byte   $00,$80,$01,$20,$00,$00,$00,$44
        .byte   $00,$00,$00,$00,$00,$40,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$44,$40,$00
        .byte   $00,$08,$10,$50,$00,$02,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $04,$21,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$40,$00,$00,$08
        .byte   $00,$20,$00,$00,$00,$80,$00,$04
        .byte   $44,$50,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$80,$00,$00,$04,$00
        .byte   $00,$34,$00,$01,$10,$21,$00,$80
        .byte   $00,$00,$00,$00,$00,$00,$00,$80
        .byte   $00,$02,$00,$00,$00,$00,$00,$09
        .byte   $00,$80,$00,$12,$00,$08,$00,$40
        .byte   $10,$01,$01,$02,$00,$00,$00,$40
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$20,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$01,$14,$00
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$40,$00
        .byte   $00,$01,$00,$00,$00,$44,$00,$00
        .byte   $00,$02,$00,$50,$00,$00,$00,$00
        .byte   $00,$02,$00,$80,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $04,$00,$00,$00,$40,$01,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$40,$80
        .byte   $40,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10
        jmp     L9410

        jmp     L985D

        jmp     L9ABE

        jsr     LC752
        jsr     LFF21
        jsr     LC531
        ldy     #$05
code_9014:  lda     $9BF7,y
        sta     $E8,y
        dey
        bpl     code_9014
        jsr     LFF45
        lda     #$20
        ldx     #$24
        ldy     #$00
        jsr     LC59D
        jsr     LC53B
        ldy     #$1F
code_902E:  lda     $9C03,y
        sta     $0620,y
        dey
        bpl     code_902E
        lda     #$00
        sta     $10
        ldx     #$10
        jsr     L939E
        jsr     LFF21
        ldx     #$11
        jsr     L939E
        jsr     LFF21
        jsr     LC74C
        ldx     #$B4
        jsr     LFF1A
        jsr     LC752
        jsr     LFF21
        jsr     LC531
        ldy     #$05
code_905E:  lda     $9BF7,y
        sta     $E8,y
        dey
        bpl     code_905E
        jsr     LFF45
        lda     #$13
        sta     prg_bank
        jsr     LFF6B
        lda     #$00
        sta     $28
code_9075:  ldy     #$00
        sty     $10
        jsr     LEEAB
        jsr     LC4F8
        jsr     LFF21
        inc     $28
        lda     $28
        and     #$3F
        bne     code_9075
        ldy     #$1F
code_908C:  lda     $9C03,y
        sta     $0620,y
        dey
        bpl     code_908C
        jsr     LFF21
        jsr     LC53B
        ldx     #$03
code_909D:  lda     $9C69,x
        sta     $0200,x
        dex
        bpl     code_909D
        lda     #$00
        sta     nmi_skip
        lda     #$13
        sta     prg_bank
        jsr     LFF6B
        jsr     LC74C
code_90B4:
        lda     joy1_press
        and     #BTN_START
        bne     stage_select_init
        lda     joy1_press
        and     #$0C
        beq     code_90CA
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     $9BFF,y
        sta     $0200
code_90CA:  jsr     LFF21
        jmp     L90B4

; --- stage_select_init ($1890D0) ---
; Entry point for the stage select screen. Sets up CHR banks, loads palettes,
; and determines if this is a fresh start or return from a stage.
;
; Palette system:
;   $0600-$060F = BG palette buffer (4 sub-palettes × 4 colors)
;   $0610-$061F = Sprite palette buffer (4 sub-palettes × 4 colors)
;   $0620-$062F = BG palette working copy
;   $18 = palette update flag (nonzero → NMI uploads $0600-$061F to PPU $3F00-$3F1F)
;
; Fresh start ($0F=0): BG palettes from $9C33, sprite palettes from $9C23
;   BG 0: $0F $20 $21 $11 — frame borders: black, white, lt blue, dk blue
;   BG 1: $0F $20 $37 $29 — portrait face BG: black, white, tan, green
;   BG 2: $0F $20 $26 $16 — portrait face detail: black, white, salmon, dk red
;   BG 3: $0F $20 $10 $21 — text/misc: black, white, grey, lt blue
;   SP 0: $0F $30 $15 $11 — cursor bolts + center portrait: transparent, white, dk magenta, dk blue
;   SP 1: $0F $37 $21 $10 — face sprites 1: black, tan, lt blue, grey
;   SP 2: $0F $37 $26 $15 — face sprites 2: black, tan, salmon, dk magenta
;   SP 3: $0F $37 $26 $0F — face sprites 3: black, tan, salmon, black
;
; Return from stage ($0F=1): BG palettes from $9C43

stage_select_init:  lda     #$36        ; CHR bank $36 for sprite tiles
        sta     $EC
        lda     #$34                    ; CHR bank $34 for BG tiles
        sta     $ED
        jsr     LFF45                   ; apply CHR bank selection

; Load sprite palettes from $9C23 ($9C23)
        ldy     #$0F                    ; counter = 16 bytes
LB0DD:  lda     $9C23,y                 ; load sprite palette color
        sta     $0610,y                 ; store to sprite palette buffer
        dey
        bpl     LB0DD                   ; loop Y=$0F down to $00
        sty     palette_dirty                     ; Y=$FF, trigger palette update

; Determine fresh start vs return: check if OAM is initialized
        ldy     #$00                    ; assume fresh start (Y=0)
        lda     $0200                   ; check OAM sprite 0 Y position
        cmp     #$97                    ; $97 = sentinel for uninitialized
        beq     LB0F2                   ; fresh start → Y stays 0
        iny                             ; returning from stage → Y=1
LB0F2:  sty     $0F                     ; $0F = 0 (fresh) or 1 (return)
        lda     $9BFD,y                 ; select music track
        jsr     LF898                   ; play stage select music
        lda     #$00
        sta     $70                     ; clear NMI sync flag
        lda     $9C63,y                 ; load screen scroll/setup param
        jsr     LE8B4
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9                   ; clear unused OAM sprites
LB10B:  lda     #$04
        sta     $10
        jsr     LEF8C
        jsr     LFF21                   ; wait for NMI
        lda     $70
        bne     LB10B                   ; loop until NMI complete

; Load BG palettes — offset depends on fresh start vs return
        ldy     $0F                     ; Y = 0 (fresh) or 1 (return)
        beq     LB126                   ; skip portrait restore if fresh
        lda     #$04
        sta     $10
        ldx     #$00
        jsr     L939E                   ; restore defeated boss portraits
LB126:  lda     $9C65,y                 ; load scroll params
        sta     $10
        lda     $9C67,y
        sta     $11
        jsr     LC531                   ; enable PPU rendering
        ldx     $9C01,y                 ; X = offset into $9C03
        ldy     #$00
LB138:  lda     $9C03,x                 ; load BG palette color
        sta     $0600,y                 ; store to BG palette buffer
        sta     $0620,y                 ; store to working copy
        inx
        iny
        cpy     #$10                    ; 16 bytes = 4 palettes × 4 colors
        bne     LB138
        sty     palette_dirty                     ; Y=$10, trigger palette update
        lda     #$20
        ldx     #$24
        ldy     #$00
        jsr     LC59D
        jsr     LC53B
code_9155:
        lda     #$F8
        sta     $0200
        inc     nmi_skip
        lda     #$58
        sta     $5E
        lda     #$07
        sta     game_mode
code_9164:  lda     camera_x_lo
        clc
        adc     $10
        sta     camera_x_lo
        lda     camera_x_hi
        adc     $11
        and     #$01
        sta     camera_x_hi
        lda     #$00
        sta     nmi_skip
        jsr     LFF21
        inc     nmi_skip
        lda     camera_x_lo
        bne     code_9164
        ldy     #$10
        lda     $11
        bmi     code_91C8
        lda     camera_x_hi
        eor     #$01
        asl     a
        asl     a
        sta     $10
        lda     #$01
        jsr     LE8B4
        lda     #$00
        sta     $70
        sta     nmi_skip
code_9199:  lda     $10
        pha
        jsr     LEF8C
        pla
        sta     $10
        jsr     LFF21
        lda     $70
        bne     code_9199
        jsr     L995C
        ldx     #$03
        jsr     L939E
        jsr     LFF21
        ldx     #$04
        jsr     L939E
        lda     camera_x_hi
        eor     #$01
        sta     camera_x_hi
        ldy     #$00
        lda     #$7E
        sta     $E9
        jsr     LFF3C
code_91C8:  ldx     #$00
code_91CA:  lda     $9C33,y
        sta     $0600,x
        iny
        inx
        cpx     #$10
        bne     code_91CA
        lda     #$FF
        sta     palette_dirty
        lda     #$01
        sta     $12
        lda     #$03
        sta     $13
        lda     $11
        bmi     code_91EC
        jsr     L99FA
        jmp     L9258

code_91EC:  jsr     L93E9
code_91EF:  jsr     L93FE
        lda     #$00
        sta     nmi_skip
        jsr     LFF21
        inc     nmi_skip
        lda     joy1_press
        and     #BTN_START
        beq     code_91EF
        lda     $0200
        cmp     #$B7
        beq     code_9212
        lda     #$03
        sta     prg_bank
        jsr     LFF6B
        jmp     LA593

code_9212:  ldy     #$04
        lda     #$F8
code_9216:  sta     $0200,y
        iny
        iny
        iny
        iny
        bne     code_9216
        lda     #$13
        sta     prg_bank
        jsr     LFF6B
        lda     #$10
        jsr     LF898
        lda     #$00
        sta     $70
        sta     $28
        lda     #$04
        jsr     LE8B4
code_9236:  lda     #$00
        sta     nmi_skip
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_9236
        lda     #$04
        sta     $10
        lda     #$00
        sta     $11
        lda     #$7C
        sta     $E8
        jsr     LFF3C
        jmp     L9155
code_9258:

        lda     joy1_press
        and     #$90
        beq     code_9261
        jmp     L9300

; ==========================================================================
; STAGE SELECT SCREEN
; ==========================================================================
; The stage select screen displays a 3×3 grid of boss portraits with
; Mega Man's face in the center. The player moves a cursor between the
; 9 grid positions using the D-pad.
;
; Grid layout:
;   Spark Man ($06) | Snake Man ($05) | Needle Man ($00)
;   Hard Man ($03)  | Mega Man (ctr)  | Top Man ($04)
;   Gemini Man ($02)| Magnet Man ($01)| Shadow Man ($07)
;
; Visual composition:
;   Background tiles: Blue "MEGA MAN" repeating pattern, portrait frame
;     borders (8×6 tiles per frame), boss name text labels, "PUSH START"
;   Nametable writes: Portrait face tiles (4×4 per boss, CHR bank selects face),
;     center Mega Man face tiles ($CC-$FF)
;   OAM sprites: Cursor bolt connectors (4 per selected frame, tiles $E4/$E5,
;     sprite palette 0: $0F/$30/$15/$11 — attribute byte never written, defaults
;     to $00 from OAM clear. Tile $E4 = magenta fill ($15), tile $E5 = blue fill
;     ($11), both with white ($30) upper-left highlight. Alternates every 8 frames.
;     Mega Man eye sprites (6 per cursor position, attr $03 = sprite palette 3,
;     eyes track toward selected boss), boss portrait detail sprites
;
; Key data tables in this bank:
;   $9C75: megaman_eye_sprites — 9 × 12 bytes (6 sprites × tile+attr)
;   $9CE1: stage_select_lookup — grid index → stage $22 value
;   $9CF3: cursor_pos_y — bolt sprite Y base per grid position
;   $9CFC: cursor_pos_x — bolt sprite X base per grid position
;   $9D05: cursor_bolt_offset_y — Y offsets for 4 bolt corners
;   $9D09: cursor_bolt_offset_x — X offsets for 4 bolt corners
;   $9D0D: cursor_direction_offsets — D-pad bit → position delta
;   $9D4C: portrait_addr_lo — PPU nametable low bytes for frame positions
;   $9D55: portrait_addr_hi — PPU nametable high bytes for frame positions
;   $9D5E: portrait_frame_tiles — frame border tile layout (groups)
;   $9D99: face_tiles — 4×4 boss face tile IDs ($C8-$FB)
;   $9DA9: face_attr_offsets — attribute table offsets per portrait
;   $9DB2: center_portrait_data — Mega Man center face tiles ($CC-$FF)
;   $9DC9: face_addr_hi — PPU high bytes for face tile writes
;   $9DD2: face_addr_lo — PPU low bytes for face tile writes
;
; Portrait pixel positions (frame top-left corners):
;   Col 0: x=16   Col 1: x=96   Col 2: x=176
;   Row 0: y=24   Row 1: y=88   Row 2: y=152
;
; Cursor bolt pixel positions (base + offsets):
;   Base Y: row 0=$18(24), row 1=$58(88), row 2=$98(152)
;   Base X: col 0=$17(23), col 1=$67(103), col 2=$B7(183)
;   Offsets: TL(0,0) TR(42,0) BL(0,38) BR(42,38)
;   Bolt tile $E4 = magenta ($15) fill, $E5 = blue ($11) fill, alternates every 8 frames
; ==========================================================================

; --- Stage select d-pad handling ---
; $12 = cursor column (0-2), $13 = cursor row offset (0/3/6)
; Combined ($12+$13) = grid index 0-8 into stage lookup table at $9CE1

code_9261:  lda     joy1_press                 ; new button presses
        and     #$03                    ; $02=Left, $01=Right
        beq     code_9279               ; no horizontal input → skip
        tay
        lda     $12                     ; current column
        clc
        adc     $9D0D,y                 ; add direction offset from table
        cmp     #$03                    ; clamp to 0-2
        bcs     code_9279               ; out of range → ignore
        sta     $12                     ; update column
        lda     #$1B                    ; SFX $1B = cursor move
        jsr     LF89A
code_9279:  lda     joy1_press                 ; new button presses
        and     #$0C                    ; $08=Up, $04=Down
        beq     code_9291               ; no vertical input → skip
        tay
        lda     $13                     ; current row offset
        clc
        adc     $9D0D,y                 ; add direction offset
        cmp     #$07                    ; clamp to 0-6 (rows 0/3/6)
        bcs     code_9291               ; out of range → ignore
        sta     $13                     ; update row
        lda     #$1B                    ; SFX $1B = cursor move
        jsr     LF89A
code_9291:  lda     $12                 ; combine column + row
        clc
        adc     $13
        sta     L0000
        asl     a
        asl     a
        sta     $01
        asl     a

; --- Cursor sprite rendering ---
; Writes 6 cursor selector sprites + 4 cursor bolt sprites to OAM.
; Grid index = $12 + $13 = $00 (0-8).
        adc     $01                     ; ×12 = OAM data offset for grid pos
        tax
        ldy     #$00
LB2A2:  lda     $9C75,x                 ; cursor selector OAM data (12 bytes/pos)
        sta     $0299,y                 ; → OAM sprites at $0298+
        lda     $9C76,x
        sta     $029A,y
        inx
        inx
        iny
        iny
        iny
        iny                             ; Y += 4 (OAM entry size)
        cpy     #$18                    ; 6 sprites × 4 bytes = $18
        bne     LB2A2

; --- Cursor bolt sprites (4 corner bolts) ---
; Position from $9CF3/$9CFC tables, offsets from $9D05/$9D09.
; Bolt tile alternates $E4/$E5 every 8 frames for flash effect.
; NOTE: OAM attribute byte is never written → defaults to $00 = sprite palette 0.
; SP 0: $0F(transparent), $30(white), $15(dk_magenta), $11(dk_blue).
; Tile $E4 uses color 2 ($15 magenta fill), tile $E5 uses color 3 ($11 blue fill).
; Both tiles have color 1 ($30 white) as upper-left highlight.
; Solid filled shape (not outline) — same pattern as center portrait bolt sprites.
        ldy     L0000                   ; grid index
        lda     $9CF3,y                 ; cursor Y base (per grid position)
        sta     L0000
        lda     $9CFC,y                 ; cursor X base (per grid position)
        sta     $01
        lda     $95                     ; frame counter
        lsr     a
        lsr     a
        lsr     a                       ; ÷8
        and     #$01                    ; alternates 0/1
        clc
        adc     #$E4                    ; tile $E4 or $E5 (bolt flash)
        sta     $02
        ldx     #$03                    ; 4 bolts (3→0)
        ldy     #$C8                    ; OAM offset $C8 (sprite 50-53)
LB2D4:  lda     L0000                   ; Y base
        clc
        adc     $9D05,x                 ; + Y offset (0 or 38 for top/bottom)
        sta     $0200,y                 ; → OAM Y
        lda     $01                     ; X base
        clc
        adc     $9D09,x                 ; + X offset (0 or 42 for left/right)
        sta     $0203,y                 ; → OAM X
        lda     $02                     ; bolt tile ($E4/$E5)
        sta     $0201,y                 ; → OAM tile
        dey
        dey
        dey
        dey                             ; Y -= 4 (next OAM slot, backwards)
        dex
        bpl     LB2D4
code_92F2:  lda     #$00
        sta     nmi_skip
        jsr     LFF21
        inc     nmi_skip
        inc     $95
        jmp     L9258

; ===========================================================================
; Stage select confirmation (A or Start pressed)
; ===========================================================================
; Called when the player presses A/Start on the stage select screen.
; Validates the selection, then initiates the stage transition:
;   1. Fills the offscreen nametable with boss intro layout (blue band)
;   2. Loads transition palettes from $9C53
;   3. Jumps to bank03 $A000 for the horizontal scroll + boss name reveal
;
; The boss intro layout is a solid light-blue band (NES $21) spanning
; tile rows 11-18 (y=88-151), bordered by white ($20) lines at y=88-89
; and y=150-151. This band is written into the offscreen nametable by
; fill_nametable_progressive ($1FEF8C) using metatile data from bank $13.
; The NMI/IRQ scroll split then creates a 3-strip effect:
;   - Top strip    (y=0-87):    scrolls horizontally, sliding portraits away
;   - Middle band  (y=88-151):  stays fixed, shows blue intro band + boss sprite
;   - Bottom strip (y=152-239): scrolls horizontally, sliding portraits away
; ---------------------------------------------------------------------------
stage_select_confirm:

        jsr     LC628                   ; clear all entity slots
        lda     $12                     ; $12 = cursor column (0-2)
        clc
        adc     $13                     ; $13 = cursor row offset (0/3/6)
        tay                             ; Y = grid index (0-8)
        cpy     #$04                    ; index 4 = center position
        bne     LB316                   ; not center → try select
        lda     stage_select_page                     ; center only selectable when
        cmp     #$12                    ; $60 == $12 (all Doc Robots beaten)
        bne     LB316
        jmp     L9ABC                   ; → Wily fortress entrance

LB316:  lda     bosses_beaten                     ; $61 = boss-defeated bitmask
        and     $9DED,y                 ; check if this boss already beaten
        bne     code_92F2               ; already beaten → back to select loop
        lda     stage_select_page                     ; $60 = game progression page
        cmp     #$0A                    ; >= $0A = invalid state
        bcs     code_92F2               ; → back to select loop
        tya                             ; Y = grid index (0-8)
        clc
        adc     stage_select_page                     ; + page offset (0=Robot Masters, $0A=Doc Robot)
        tay                             ; Y = adjusted grid index for table lookup

; Stage select lookup table at $9CE1 (9 entries per page):
;   Index: 0     1     2     3     4     5     6     7     8
;   Value: $06   $05   $00   $03   $FF   $04   $02   $01   $07
;   Boss:  Spark Snake Needl Hard  (n/a) Top   Gemin Magnt Shadw
;   Grid:  TL    TM    TR    ML    CTR   MR    BL    BM    BR
        lda     $9CE1,y                 ; look up stage number
        bmi     code_92F2               ; $FF = center (not selectable)
        sta     stage_id                     ; $22 = current stage number
        sty     $0F                     ; $0F = save adjusted grid index

; --- Set up PRG/CHR banks for the boss intro layout ---
        lda     #$13                    ; select PRG bank $13
        sta     prg_bank                     ; (contains boss intro metatile data
        jsr     LFF6B                   ; at $AF00+, referenced by fill routine)
        lda     #$04                    ; set rendering mode
        sta     oam_ptr
        jsr     LC5E9                   ; clear unused OAM sprites
        lda     #$76                    ; set CHR bank $76
        sta     $E9                     ; (boss intro screen tileset)
        jsr     LFF3C

; --- Determine which nametable to fill (the offscreen one) ---
; $FD bit 0 = currently displayed nametable. Toggle it so the NEW
; nametable becomes visible, then compute $10 = old nametable's
; PPU address bit (0 or 4) to write the intro layout into it first.
        lda     camera_x_hi                     ; toggle displayed nametable
        pha
        eor     #$01
        sta     camera_x_hi
        pla                             ; $10 = OLD nametable select bit
        and     #$01                    ; bit 0 → shifted left 2 = 0 or 4
        asl     a                       ; 0 = nametable $2000, 4 = nametable $2400
        asl     a
        sta     $10

; --- Set up metatile pointer and fill nametable ---
; metatile_column_ptr_by_id computes: ($20/$21) = $AF00 + (A << 6)
; A=$03 → column 3 of bank $13 level data → pointer $AFC0
        lda     #$03                    ; set metatile column pointer
        jsr     LE8B4                   ; ($20/$21) → $AFC0 in bank $13
        lda     #$00                    ; $70 = nametable fill progress (0-63)
        sta     $70                     ; starts at 0
        sta     nmi_skip                     ; $EE = NMI skip flag (0=allow NMI)

; Fill the offscreen nametable progressively.
; fill_nametable_progressive writes 4 tile rows per call.
; 16 calls × 4 rows = 64 rows → full nametable.
; When $70 reaches $40, it writes the attribute table and resets $70 to 0.
LB35F:  lda     $10                     ; preserve nametable select
        pha
        jsr     LEF8C                   ; write 4 rows to PPU queue
        jsr     LFF21                   ; wait for NMI (PPU uploads queued data)
        pla                             ; restore nametable select
        sta     $10
        lda     $70                     ; $70 = 0 when fill complete
        bne     LB35F                   ; loop until done

; --- Load transition palette and jump to bank03 ---
; $9C53: 16-byte palette for the boss intro screen (BG palettes only).
; This replaces the stage select palettes with the intro band colors.
        ldy     #$0F
LB371:  lda     $9C53,y                 ; copy 16 bytes from $9C53
        sta     $0600,y                 ; to BG palette buffer
        sta     $0620,y                 ; and working copy
        dey
        bpl     LB371
        sty     palette_dirty                     ; $18 = $FF → flag palette upload

; Jump to bank03 entry point.
; Y = adjusted grid index (used by bank03 to index stage parameter tables).
; This is a JMP (not JSR) — bank03's RTS returns to whoever called the
; stage select, not back here.
        lda     #$03                    ; select PRG bank $03
        sta     prg_bank
        jsr     LFF6B
        ldy     $0F                     ; Y = adjusted grid index
        jmp     LA000                   ; → bank03 stage_transition_entry

        pha
        lda     #$01
        sta     prg_bank
        jsr     LFF6B
        pla
        jsr     LA000
        lda     #$03
        sta     prg_bank
        jmp     LFF6B

; ---------------------------------------------------------------------------
; write_ppu_data_from_bank03 — copy a PPU write command list from bank03
; ---------------------------------------------------------------------------
; Parameters: X = table index, $10 = nametable select (0 or 4)
; Loads a pointer from bank03 tables $A56D/$A580 and copies the data
; to the PPU write queue at $0780+. The data format is:
;   byte 0: PPU high address (ORed with $10 for nametable select)
;   byte 1: PPU low address
;   byte 2: tile count (N)
;   bytes 3..3+N: tile data
;   ... (repeats for more rows)
;   terminator: high byte with bit 7 set ($FF)
; Used to write nametable tile data for the boss intro screen.
; ---------------------------------------------------------------------------
write_ppu_data_from_bank03:

        sty     $04                     ; save Y
        stx     $05                     ; save table index
        lda     prg_bank                     ; save current PRG bank
        pha
        lda     #$03                    ; switch to bank 03
        sta     prg_bank                     ; (contains PPU data tables)
        jsr     LFF6B
        ldx     $05                     ; X = table index
        lda     LA56D,x                 ; ($02/$03) = pointer to PPU data
        sta     $02                     ; low byte from $A56D+X
        lda     LA580,x                 ; high byte from $A580+X
        sta     $03
        ldy     #$00
LB3BA:  lda     ($02),y                 ; PPU high address
        ora     $10                     ; OR nametable select bit
        sta     $0780,y                 ; store to PPU write queue
        bmi     LB3DE                   ; bit 7 set = terminator ($FF)
        iny
        lda     ($02),y                 ; PPU low address
        sta     $0780,y
        iny
        lda     ($02),y                 ; tile count
        sta     $0780,y
        sta     L0000                   ; save count for loop
        iny
LB3D2:  lda     ($02),y                 ; tile data byte
        sta     $0780,y
        iny
        dec     L0000
        bpl     LB3D2
        bmi     LB3BA                   ; next PPU entry (always branches)
LB3DE:  sta     nametable_dirty                     ; $19 = $FF → flag PPU write pending
        pla                             ; restore original PRG bank
        sta     prg_bank
        jsr     LFF6B
        ldy     $04                     ; restore Y
        rts
code_93E9:

        lda     #$B7
        sta     $0200
        lda     #$ED
        sta     $0201
        lda     #$00
        sta     $0202
        lda     #$40
        sta     $0203
        rts
code_93FE:

        lda     joy1_press
        and     #$0C
        beq     code_940F
        ldy     #$B7
        and     #$08
        bne     code_940C
        ldy     #$C7
code_940C:  sty     $0200
code_940F:  rts

; ===========================================================================
; Robot Master stage intro sequence
; ===========================================================================
; Called after bank03 returns from the horizontal scroll + boss name reveal.
; This routine handles:
;   1. Refill HP/weapon energy
;   2. Fill nametable with boss intro layout (blue band screen)
;   3. Write stage-specific nametable data (boss face tiles)
;   4. Boss sprite drops from top of screen to center of blue band
;   5. Boss does intro animation, then switches to idle pose
;   6. Wait, then Mega Man teleports in from below
;   7. Palette fade to black + boss face flash
;   8. Jump to stage loading
;
; Verified via Mesen save states:
;   - Blue band: y=88-151 (tile rows 11-18), white borders at y=88-89/150-151
;   - Boss sprite centered at ~x=124, y=112 (42×30 px bounding box)
;   - Boss name text at y=136-143 (tile row 17, col 11 = PPU $222B)
;   - Boss drops from y=$E8 (232) to y=$74 (116) at 4px/frame = 29 frames
;   - Mega Man rises from y=$80 (128) to y=$C0 (192) at 2px/frame = 32 frames
; ---------------------------------------------------------------------------
robot_master_intro:

        ldy     #$0B                    ; refill weapon energy
LB412:  lda     player_hp,y                   ; $A2-$AD = weapon ammo
        bpl     LB41C                   ; if negative (depleted), set to full
        lda     #$9C                    ; $9C = full energy (28 units)
        sta     player_hp,y
LB41C:  dey
        bpl     LB412
        lda     stage_id                     ; stage < $08 = Robot Master
        cmp     #STAGE_DOC_NEEDLE                    ; stage >= $08 = Doc Robot/Wily
        bcc     LB428
        jmp     L9581                   ; → skip to stage loading for Doc/Wily

; --- Robot Master intro animation ---

LB428:  jsr     LC752                   ; disable sprites/rendering
        lda     #$04                    ; set rendering mode
        sta     oam_ptr
        jsr     LC5E9                   ; configure PPU
        jsr     LFF21                   ; wait 1 frame
        lda     #$35                    ; play sound $35
        jsr     LF898                   ; (boss intro fanfare)
        jsr     L9936                   ; zero out game state

; Fill nametable 0 with boss intro layout from bank $13.
; Temporarily set $22 = $14 so metatile pointer references the correct
; level section in bank $13 (stage $14 = the boss intro screen layout).
; metatile_column_ptr_by_id with A=$07 → metatile column 7 → pointer $B0C0.
        lda     stage_id                     ; save real stage number
        pha
        lda     #$14                    ; $22 = $14 (boss intro layout ID)
        sta     stage_id
        lda     #$07                    ; metatile column 7
        jsr     LE8B4                   ; pointer → $B0C0 in bank $13
LB449:  lda     #$00                    ; $10 = 0 → write to nametable $2000
        sta     $10
        jsr     LEF8C                   ; write 4 tile rows
        jsr     LFF21                   ; wait for NMI
        lda     $70                     ; $70 = 0 when complete
        bne     LB449
        pla                             ; restore real stage number
        sta     stage_id

; Write stage-specific nametable data (boss face, decorations).
; Two calls to write_ppu_data_from_bank03:
;   X=$05: common intro data
;   X=$22+$06: per-boss face data
        ldx     #$05                    ; table index $05 = common data
        lda     #$00                    ; $10 = 0 → nametable $2000
        sta     $10
        jsr     L939E                   ; write common intro nametable data
        jsr     LFF21                   ; wait for NMI
        lda     stage_id                     ; X = stage + 6
        clc                             ; (per-boss face data table index)
        adc     #$06
        tax
        jsr     L939E                   ; write boss-specific face tiles

; Load CHR bank configuration and palettes for the intro screen.
; $9D46: 6 bytes of CHR bank IDs → $E8-$ED
; $9D16: 32 bytes of palette data (BG + sprite) → $0620 working copy
        ldy     #$05
LB471:  lda     $9D46,y
        sta     $E8,y
        dey
        bpl     LB471
        jsr     LFF3C
        ldy     #$1F
LB47F:  lda     $9D16,y                 ; 32 bytes: BG ($0620-$062F) + sprite ($0630-$063F)
        sta     $0620,y
        dey
        bpl     LB47F

; --- Set up boss entity and begin drop animation ---
; Entity slot 0: the boss sprite (reused as the intro display entity).
; ent_x_px = entity 0 X position (player X during gameplay)
; ent_y_px = entity 0 Y position (player Y during gameplay)
        lda     #$80                    ; entity 0 X = $80 (128 = centered)
        sta     ent_x_px
        lda     #$E8                    ; entity 0 Y = $E8 (232 = below screen)
        sta     ent_y_px
        ldx     #$00                    ; set animation to $B0
        lda     #$B0                    ; (boss intro drop animation)
        jsr     LF835
        lda     ent_flags                   ; clear bit 6 of entity flags
        and     #$BF                    ; (enable rendering?)
        sta     ent_flags
        jsr     LFF21                   ; wait 1 frame
        jsr     LC74C                   ; enable rendering

; Boss drop loop: decrement Y from $E8 to $74 at 4px/frame.
; ($E8 - $74) / 4 = 29 frames for the boss to slide down.
; When anim phase (ent_anim_state) reaches $02, switch to idle anim $1A.
LB4A7:  lda     ent_y_px                   ; if Y == $74, skip decrement
        cmp     #$74                    ; (target reached)
        beq     LB4B9
        sec                             ; Y -= 4
        sbc     #$04
        sta     ent_y_px
        lda     #$00                    ; reset anim frame counter
        sta     ent_anim_frame                   ; (keep animation progressing)
LB4B9:  lda     ent_anim_state                   ; check animation phase
        cmp     #$02                    ; phase 2 = intro anim done
        bne     LB4C7
        ldx     #$00                    ; switch to idle animation $1A
        lda     #$1A
        jsr     LF835
LB4C7:  jsr     LFD6E                   ; process sprites + wait for NMI
        lda     ent_anim_id                   ; check current OAM ID
        cmp     #$1A                    ; $1A = idle pose active
        bne     LB4A7                   ; loop until idle
        ldx     #$3C                    ; wait $3C (60) frames
        jsr     LFF1A                   ; (boss stands idle)

; --- Mega Man teleport-in animation ---
; Mega Man rises from Y=$80 to Y=$C0, 2px/frame = 32 frames.
; (Teleporting from below the blue band upward into view.)
        lda     ent_x_px                   ; if X == $C0, done
        cmp     #$C0
        beq     LB4E9
        clc                             ; X += 2
        adc     #$02                    ; (note: using ent_x_px which is
        sta     ent_x_px                   ; the X position for the entity)
        jsr     LFD6E                   ; process sprites + wait for NMI
        jmp     L94D6

; --- Palette fade to black ---
; Fade all 3 BG palette groups to black ($0F).
; Each call to fade_palette_to_black subtracts $10 per step
; from palette colors, 4 frames per step, until all reach $0F.

LB4E9:  ldx     #$3C                    ; wait $3C (60) frames
        jsr     LFF1A
        lda     #$00                    ; clear NMI skip flag
        sta     nmi_skip
        ldy     #$03                    ; fade BG palette 0 (bytes $00-$03)
        jsr     L954A
        ldy     #$07                    ; fade BG palette 1 (bytes $04-$07)
        jsr     L954A
        ldy     #$0B                    ; fade BG palette 2 (bytes $08-$0B)
        jsr     L954A
        ldx     #$B4                    ; wait $B4 (180) frames
        jsr     LFF1A                   ; (3 seconds on black screen)
        jmp     L9581                   ; → stage loading

; ---------------------------------------------------------------------------
; boss_face_palette_flash — alternates boss/default sprite palettes
; ---------------------------------------------------------------------------
; Called after the last palette fade (Y=$07 → code_189509 via BEQ).
; Flashes 17 times ($10 counts $10 down to $00), 2 frames per flash.
; Even frames: load boss-specific face colors from $9BB7 table.
; Odd frames: restore default palette from $0630 working copy.
; ---------------------------------------------------------------------------

boss_face_palette_flash:  lda     #$10  ; $10 = flash counter (17 iterations)
        sta     $10
LB50D:  ldy     #$03                    ; process 4 palette colors (Y=3..0)
        lda     $10                     ; even/odd toggle
        and     #$01
        bne     LB52F                   ; odd → restore defaults

; Even frames: load boss face colors from $9BB7 table.
; $9BB7 + stage*8 = 8-byte palette (bright + dark variant).
; Colors 0-3 from $9BB7 → SP 0 ($0610), colors 4-7 from $9BBB → SP 1 ($0618).
        lda     stage_id                     ; X = stage * 8 + 3
        asl     a
        asl     a
        asl     a
        ora     #$03
        tax
LB51D:  lda     $9BB7,x                 ; bright variant → SP 0
        sta     $0610,y
        lda     $9BBB,x                 ; dark variant → SP 1
        sta     $0618,y
        dex
        dey
        bne     LB51D
        beq     LB53E                   ; (always branches)

; Odd frames: copy default palettes back from $0630/$0638 working copy.
LB52F:  lda     $0630,y                 ; default SP 0
        sta     $0610,y
        lda     $0638,y                 ; default SP 1
        sta     $0618,y
        dey
        bne     LB52F
LB53E:  inc     palette_dirty                     ; flag palette upload
        ldx     #$02                    ; wait 2 frames per flash
        jsr     LFF1A
        dec     $10                     ; decrement flash counter
        bpl     LB50D
        rts

; ---------------------------------------------------------------------------
; fade_palette_to_black — progressively fade a BG palette group to black
; ---------------------------------------------------------------------------
; Parameters: Y = palette end index (3, 7, or $0B for BG palette 0/1/2)
; Subtracts $10 per step from each color in the palette group.
; Colors that would go below $0F are clamped to $0F (black).
; 4 steps × $10 = $40 total subtraction. 4 frames per step.
; Source: $0600,y (base palette). Destination: $0604,y (working palette).
; After the last step (Y=$07), chains into boss_face_palette_flash.
; ---------------------------------------------------------------------------
fade_palette_to_black:

        lda     #$30                    ; $10 = starting subtract value
        sta     $10                     ; (decreases $30→$20→$10→$00)
        sty     $11                     ; save palette end index
LB550:  ldy     $11                     ; restore palette index
        ldx     #$03                    ; 4 colors per palette group
LB554:  lda     $0600,y                 ; load base palette color
        sec
        sbc     $10                     ; subtract fade amount
        bcs     LB55E                   ; if no underflow, use result
        lda     #$0F                    ; clamp to black ($0F)
LB55E:  sta     $0604,y                 ; store to working palette
        dey
        dex
        bpl     LB554
        sty     palette_dirty                     ; flag palette upload
        ldx     #$04                    ; wait 4 frames per step
        jsr     LFF1A
        lda     $10                     ; $10 -= $10 (next darker step)
        sec
        sbc     #$10
        sta     $10
        bcs     LB550                   ; loop while $10 >= 0
        lda     $11                     ; if this was BG palette 1 (Y=$07),
        cmp     #$07                    ; chain into boss face flash
        beq     boss_face_palette_flash
        ldx     #$1E                    ; wait $1E (30) frames between groups
        jsr     LFF1A
        rts
code_9581:

        lda     bosses_beaten
        cmp     #$FF
        beq     code_958A
        jmp     L968C

code_958A:  jsr     LC752
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LFF21
        jsr     L9936
        lda     #$10
        jsr     LF898
        lda     #$13
        sta     prg_bank
        jsr     LFF6B
        lda     #$01
        jsr     LE8B4
        lda     #$00
        sta     $70
code_95AF:  lda     #$00
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_95AF
        lda     #$00
        sta     $10
        jsr     L995C
        lda     #$00
        sta     $10
        ldx     #$03
        jsr     L939E
        jsr     LFF21
        ldx     #$04
        jsr     L939E
        jsr     LFF21
        lda     #$04
        jsr     LE8B4
        lda     #$00
        sta     $70
code_95E1:  lda     #$04
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_95E1
        lda     #$7C
        sta     $E8
        lda     #$7E
        sta     $E9
        lda     #$36
        sta     $EC
        lda     #$34
        sta     $ED
        jsr     LFF3C
        ldy     #$0F
code_9604:  lda     $9C33,y
        sta     $0620,y
        dey
        bpl     code_9604
        ldy     #$0F
code_960F:  lda     $9C23,y
        sta     $0630,y
        dey
        bpl     code_960F
        jsr     LFF21
        jsr     L99FA
        lda     #$00
        sta     palette_dirty
        jsr     LFF21
        lda     #$58
        sta     $5E
        lda     #$07
        sta     game_mode
        jsr     LFF21
        jsr     LC74C
        lda     stage_select_page
        beq     code_964D
        lda     #$12
        sta     stage_select_page
        ldy     #$00
        sty     $10
        ldx     #$19
        jsr     L970B
        jsr     L97EC
        jsr     L9A87
        jmp     L9681

code_964D:  ldx     #$F0
        jsr     LFF1A
        lda     #$3A
        sta     bosses_beaten
        lda     #$09
        sta     stage_select_page
        lda     #$74
        sta     $E9
        jsr     LFF3C
        ldy     #$0F
code_9663:  lda     $9D36,y
        sta     $0600,y
        sta     $0620,y
        dey
        bpl     code_9663
        sty     palette_dirty
        lda     #$00
        sta     $10
        ldy     #$01
        ldx     #$19
        jsr     L970B
        ldx     #$19
        jsr     L9762
code_9681:
        lda     #$01
        sta     $12
        lda     #$03
        sta     $13
        jmp     L9258
code_968C:

        jsr     LC752
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LFF21
        jsr     L9936
        lda     #$13
        sta     prg_bank
        jsr     LFF6B
        lda     #$01
        sta     camera_x_hi
        lda     #$02
        jsr     LE8B4
code_96AC:  lda     #$04
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_96AC
        lda     #$7C
        sta     $E8
        lda     #$76
        sta     $E9
        lda     #$36
        sta     $EC
        lda     #$34
        sta     $ED
        jsr     LFF3C
        ldy     #$0F
code_96CF:  lda     $9C43,y
        sta     $0620,y
        dey
        bpl     code_96CF
        ldy     #$0F
code_96DA:  lda     $9C23,y
        sta     $0630,y
        dey
        bpl     code_96DA
        lda     #$04
        sta     $10
        ldx     #$12
        jsr     L939E
        jsr     LFF21
        lda     #$03
        sta     prg_bank
        jsr     LFF6B
        jsr     LA8DD
        jsr     LC74C
code_96FC:
        lda     joy1_press
        and     #$90
        bne     code_9708
        jsr     LFF21
        jmp     L96FC

code_9708:  jmp     L9212

; --- write_portrait_frames ---
; Writes portrait frame border tiles to the nametable for all 8 boss positions.
; Uses PPU update buffer at $0780+. Format per group:
;   $0780: PPU addr high, $0781: PPU addr low, $0782: tile count-1, $0783+: tile IDs
; Base addresses from $9D4C/$9D55 tables, tile layout from $9D5E data.
; Iterates y=0,2,4(skip center),6,8 for all 9 grid positions minus Mega Man center.
;
; Portrait frame pixel positions (decoded from nametable addresses):
;   Col 0: x=16   Col 1: x=96   Col 2: x=176
;   Row 0: y=24   Row 1: y=88   Row 2: y=152
; Frame dimensions: 8×6 tiles (64×48 px)
write_portrait_frames:

        stx     $0F
LB70D:  lda     $9D4C,y                 ; PPU addr low byte for portrait y
        sta     L0000
        lda     $9D55,y                 ; PPU addr high byte ($20/$21/$22)
        ora     $10
        sta     $01
        ldx     #$00
LB71B:  lda     $9D5E,x                 ; row Y-offset (bit 7 = end marker)
        bmi     LB749
        lda     $9D5F,x                 ; row X-offset within nametable
        clc
        adc     L0000                   ; + base low byte
        sta     $0781,x                 ; → PPU addr low
        lda     $9D5E,x                 ; row offset (carry into high byte)
        adc     $01                     ; + base high byte
        sta     $0780,x                 ; → PPU addr high
        inx
        inx
        lda     $9D5E,x                 ; tile count (N → writes N+1 tiles)
        sta     $0780,x
        sta     $02                     ; loop counter
        inx
LB73C:  lda     $9D5E,x                 ; tile ID
        sta     $0780,x
        inx
        dec     $02
        bpl     LB73C
        bmi     LB71B
LB749:  sta     $0780,x                 ; $FF end marker
        sta     nametable_dirty
        tya
        pha
        ldx     $0F
        jsr     LFF1A                   ; submit PPU update buffer
        pla
        tay
LB757:  iny
        iny                             ; y += 2
        cpy     #$04
        beq     LB757                   ; skip index 4 (center = Mega Man)
        cpy     #$09
        bcc     LB70D                   ; loop until all 9 done
        rts

; --- write_portrait_faces ---
; Writes 4×4 portrait face tiles into each boss portrait frame.
; Face tiles from $9D99-$9DA8 are arranged as 4 rows of 4 tiles each.
; PPU addresses from $9DC9 (high) / $9DD2 (low), rows spaced $20 (one nametable row).
; Also writes attribute table entry at $23C0+ for palette assignment.
;
; Face pixel positions (2 tiles inside the frame border):
;   Col 0: x=32   Col 1: x=112  Col 2: x=192
;   Row 0: y=32   Row 1: y=96   Row 2: y=160
; Face dimensions: 4×4 tiles (32×32 px)
;
; Face tile layout (same tiles for ALL portraits — actual portrait selected by CHR bank):
;   Row 0: $C8 $C9 $CA $CB
;   Row 1: $D8 $D9 $DA $DB
;   Row 2: $E8 $E9 $EA $EB
;   Row 3: $F8 $F9 $FA $FB
write_portrait_faces:

        stx     $0F
        ldx     #$03
LB766:  lda     $9D99,x                 ; face row 0 tiles ($C8-$CB)
        sta     $0783,x
        lda     $9D9D,x                 ; face row 1 tiles ($D8-$DB)
        sta     $078A,x
        lda     $9DA1,x                 ; face row 2 tiles ($E8-$EB)
        sta     $0791,x
        lda     $9DA5,x                 ; face row 3 tiles ($F8-$FB)
        sta     $0798,x
        dex
        bpl     LB766
        ldy     #$00
LB783:  lda     $9DC9,y                 ; PPU addr high ($20/$21/$22)
        ora     $10
        sta     $0780                   ; row 0 PPU high
        sta     $0787                   ; row 1 PPU high
        sta     $078E                   ; row 2 PPU high
        sta     $0795                   ; row 3 PPU high
        clc
        lda     $9DD2,y                 ; PPU addr low (base)
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; + 1 nametable row (32 bytes)
        sta     $0788                   ; row 1 PPU low
        adc     #$20
        sta     $078F                   ; row 2 PPU low
        adc     #$20
        sta     $0796                   ; row 3 PPU low
        lda     #$03                    ; 4 tiles per row (count-1)
        sta     $0782
        sta     $0789
        sta     $0790
        sta     $0797
        lda     #$23                    ; attribute table addr high ($23xx)
        ora     $10
        sta     $079C
        lda     #$C0                    ; attribute table base ($23C0)
        ora     $9DA9,y                 ; + portrait-specific attr offset
        sta     $079D
        lda     #$00                    ; 1 attribute byte (count-1=0)
        sta     $079E
        lda     #$55                    ; attr value $55 = palette 1 for all quadrants
        sta     $079F
        lda     #$FF                    ; end marker
        sta     $07A0
        sta     nametable_dirty
        tya
        pha
        ldx     $0F
        jsr     LFF1A                   ; submit PPU update buffer
        pla
        tay
LB7E1:  iny
        iny                             ; y += 2
        cpy     #$04
        beq     LB7E1                   ; skip center
        cpy     #$09
        bcc     LB783
        rts
write_center_portrait:

        lda     $9DB2
        ora     $10
        sta     $0780
        sta     $0787
        sta     $078E
        sta     $0795
        clc
        lda     $9DB3
        sta     $0781
        adc     #$20
        sta     $0788
        adc     #$20
        sta     $078F
        adc     #$20
        sta     $0796
        ldx     #$03
        stx     $0782
        stx     $0789
        stx     $0790
        stx     $0797
code_9821:  lda     $9DB4,x
        sta     $0783,x
        lda     $9DB8,x
        sta     $078A,x
        lda     $9DBC,x
        sta     $0791,x
        lda     $9DC0,x
        sta     $0798,x
        dex
        bpl     code_9821
        ldx     #$04
code_983E:  lda     $9DC4,x
        sta     $079C,x
        dex
        bpl     code_983E
        sta     $079C
        ora     $10
        sta     $079C
        stx     $07A1
        stx     nametable_dirty
        jsr     LFF21
        ldx     #$0E
        jsr     L939E
        rts
code_985D:

        jsr     LC752
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LFF21
        lda     #$0E
        jsr     LF898
        jsr     L9936
        lda     #$01
        sta     camera_x_hi
        lda     #$00
        sta     scroll_y
        lda     #$02
        jsr     LE8B4
code_987F:  lda     #$04
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_987F
        lda     #$7C
        sta     $E8
        lda     #$76
        sta     $E9
        lda     #$36
        sta     $EC
        lda     #$34
        sta     $ED
        jsr     LFF3C
        ldy     #$0F
code_98A2:  lda     $9C43,y
        sta     $0620,y
        dey
        bpl     code_98A2
        ldy     #$0F
code_98AD:  lda     $9C23,y
        sta     $0630,y
        dey
        bpl     code_98AD
        jsr     LFF21
        ldx     #$01
        lda     #$04
        sta     $10
        jsr     L939E
        lda     #$58
        sta     $5E
        lda     #$07
        sta     game_mode
        lda     #$03
        sta     prg_bank
        jsr     LFF6B
        jsr     LA8DD
        jsr     LFF21
        jsr     LC74C
        lda     #$78
code_98DC:  pha
        jsr     LFF21
        pla
        sec
        sbc     #$01
        bne     code_98DC
        lda     #$04
        sta     $10
        ldx     #$02
        jsr     L939E
        jsr     L93E9
code_98F2:
        jsr     L93FE
        lda     joy1_press
        and     #BTN_START
        bne     code_9901
        jsr     LFF21
        jmp     L98F2

code_9901:  ldx     #$0B
code_9903:  lda     player_hp,x
        bpl     code_990B
        lda     #$9C
        sta     player_hp,x
code_990B:  dex
        bpl     code_9903
        lda     $0200
        cmp     #$C7
        beq     code_991E
        lda     stage_id
        cmp     #STAGE_WILY1
        bcs     code_992C
        jsr     L9212
code_991E:  lda     #$02
        sta     lives
        lda     stage_select_page
        cmp     #$12
        bne     code_992B
        jmp     L9ABC

code_992B:  rts

code_992C:  lda     #$03
        sta     prg_bank
        jsr     LFF6B
        jmp     LA879

; ---------------------------------------------------------------------------
; reset_stage_state — zero out game state for stage transition
; ---------------------------------------------------------------------------
; Clears scroll position, weapon state, screen page, and various
; game state variables to prepare for the boss intro or stage load.
; ---------------------------------------------------------------------------
reset_stage_state:

        lda     #$00
        sta     LA000                   ; reset something in mapped bank
        sta     $59                     ; game sub-state
        sta     camera_screen                     ; camera/scroll page
        sta     ent_x_scr                   ; entity 0 screen page (Y high)
        sta     ent_y_scr                   ; entity 0 screen page (X high)
        sta     $B1                     ; scroll-related
        sta     $B2
        sta     $B3
        sta     camera_x_hi                     ; horizontal scroll (nametable select)
        sta     camera_x_lo                     ; horizontal scroll (sub-tile)
        sta     weapon_cursor                     ; menu cursor
        sta     $B4
        sta     current_weapon                     ; weapon ID (0 = Mega Buster)
        sta     $9E
        sta     $9F
        sta     $70                     ; nametable fill progress counter
        rts
code_995C:

        jsr     L99DC
        ldx     #$08
code_9961:  lda     $9DED,x
        beq     code_99CC
        and     bosses_beaten
        beq     code_99BA
code_996A:  lda     $9DC9,x
        ora     $10
        sta     $0780
        sta     $0787
        sta     $078E
        sta     $0795
        clc
        lda     $9DD2,x
        sta     $0781
        adc     #$20
        sta     $0788
        adc     #$20
        sta     $078F
        adc     #$20
        sta     $0796
        lda     #$03
        sta     $0782
        sta     $0789
        sta     $0790
        sta     $0797
        ldy     #$03
        lda     #$24
code_99A3:  sta     $0783,y
        sta     $078A,y
        sta     $0791,y
        sta     $0798,y
        dey
        bpl     code_99A3
        sty     $079C
        sty     nametable_dirty
        jsr     LFF21
code_99BA:  dex
        bpl     code_9961
        lda     stage_select_page
        beq     code_99CB
        cmp     #$12
        bne     code_99CB
        jsr     L97EC
        jsr     LFF21
code_99CB:  rts

code_99CC:  lda     stage_select_page
        beq     code_99BA
        cmp     #$12
        beq     code_996A
        lda     bosses_beaten
        cmp     #$FF
        bne     code_99BA
        beq     code_996A
code_99DC:
        lda     stage_select_page
        beq     code_99F9
        ldy     #$01
        ldx     #$01
        jsr     L970B
        lda     stage_select_page
        cmp     #$12
        bne     code_99F4
        ldy     #$00
        ldx     #$01
        jsr     L970B
code_99F4:  ldx     #$01
        jsr     L9762
code_99F9:  rts
code_99FA:

        ldx     #$00
        lda     stage_select_page
        beq     code_9A19
        lda     #$74
        sta     $E9
        jsr     LFF3C
        ldy     #$0F
code_9A09:  lda     $9D36,y
        sta     $0600,y
        sta     $0620,y
        dey
        bpl     code_9A09
        sty     palette_dirty
        ldx     #$98
code_9A19:  stx     L0000
        lda     prg_bank
        pha
        lda     #$03
        sta     prg_bank
        jsr     LFF6B
        ldx     L0000
code_9A27:  lda     LA231,x
        sta     $0200,x
        lda     LA232,x
        sta     $0201,x
        lda     LA233,x
        sta     $0202,x
        lda     LA234,x
        sta     $0203,x
        inx
        inx
        inx
        inx
        cpx     #$CC
        bne     code_9A27
        pla
        sta     prg_bank
        jsr     LFF6B
        ldx     #$08
code_9A4F:  lda     $9DED,x
        beq     code_9A71
        and     bosses_beaten
        beq     code_9A6D
code_9A58:  ldy     $9DDB,x
        lda     $9DE4,x
        sta     L0000
        lda     #$F8
code_9A62:  sta     $0200,y
        iny
        iny
        iny
        iny
        dec     L0000
        bpl     code_9A62
code_9A6D:  dex
        bpl     code_9A4F
        rts

code_9A71:  lda     stage_select_page
        beq     code_9A6D
        cmp     #$12
        beq     code_9A81
        lda     bosses_beaten
        cmp     #$FF
        bne     code_9A6D
        beq     code_9A58
code_9A81:  jsr     L9A87
        jmp     L9A58
code_9A87:

        sty     L0000
        ldy     #$20
code_9A8B:  lda     $9DF6,y
        sta     $02DC,y
        lda     $9DF7,y
        sta     $02DD,y
        lda     $9DF8,y
        sta     $02DE,y
        lda     $9DF9,y
        sta     $02DF,y
        dey
        dey
        dey
        dey
        bpl     code_9A8B
        ldy     #$0F
code_9AAB:  lda     $9E1A,y
        sta     $0610,y
        sta     $0630,y
        dey
        bpl     code_9AAB
        sty     palette_dirty
        ldy     L0000
        rts
code_9ABC:

        pla
        pla
code_9ABE:
        jsr     LC752
        jsr     L9936
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LFF21
        jsr     LC628
        lda     #$01
        sta     LA000
        lda     #$00
        sta     game_mode
        sta     $9E
        sta     $9F
        sta     nmi_skip
        sta     ent_y_scr
        sta     ent_y_px
        lda     #$17
        sta     $29
        lda     #$20
        sta     $2A
        lda     #$30
        sta     ent_x_px
        lda     #$01
        sta     player_facing
        sta     $23
        sta     $2E
        lda     #$0D
        sta     $2B
        lda     #STAGE_HARD
        sta     stage_id
        sta     prg_bank
        jsr     LFF6B
        lda     #$1F
        sta     $24
        lda     #$21
code_9B0E:  pha
        lda     #$01
        sta     $10
        jsr     LE4F1
        jsr     LFF21
        pla
        sec
        sbc     #$01
        bne     code_9B0E
        sta     $2C
        sta     $2D
        lda     #$50
        sta     $E8
        lda     #$52
        sta     $E9
        lda     #$1D
        sta     $EC
        lda     #$1E
        sta     $ED
        jsr     LFF3C
        ldy     #$1F
code_9B38:  lda     $9E2A,y
        sta     $0620,y
        dey
        bpl     code_9B38
        lda     #$2A
        sta     $52
        ldx     #$1F
        lda     #$80
        sta     $031F
        lda     #$94
        sta     $059F
        lda     #$53
        sta     $033F
        lda     #$18
        sta     $04FF
        lda     #$C2
        sta     $049F
        lda     #$01
        sta     $043F
        jsr     LF81B
        lda     #$99
        jsr     LF835
        lda     #$18
        sta     camera_screen
        sta     ent_x_scr
        sta     $039F
        lda     #$C0
        sta     $037F
        lda     #$02
        sta     $04BF
        lda     #$00
        sta     $041F
        sta     $03FF
        sta     $03DF
        sta     $051F
        sta     $053F
        sta     $055F
        sta     $057F
        sta     $0100
        sta     $0101
        sta     $0102
        sta     $0103
        lda     #$30
        sta     ent_x_px
        lda     #$0C
        jsr     LF898
        jsr     LFF21
        jsr     LC74C
        jmp     LC9B3

        .byte   $0F
        bmi     LBBEA
        .byte   $17
        .byte   $0F
        .byte   $07
        bmi     LBBD6
        .byte   $0F
        bmi     LBBD2
        asl     $0F,x
        asl     $10
        asl     $0F,x
        bmi     LBBFA
        and     ($0F,x)
        ora     (player_state),y
        and     ($0F,x)
        bmi     LBBE2
LBBD2:  ora     ($0F,x)
        .byte   $0C
        .byte   $10
LBBD6:  ora     ($0F,x)
        bmi     LBC01
        brk
        .byte   $0F
        brk
        plp
        brk
        .byte   $0F
        bmi     LBC12
LBBE2:  ora     $090F,y
        bmi     LBC00
        .byte   $0F
        bmi     LBC1A
LBBEA:  rol     $0F
        asl     player_state,x
        rol     $0F
        bmi     LBC26
        .byte   $14
        .byte   $0F
        .byte   $04
        .byte   $34
        .byte   $14
        .byte   $7C
        ror     $38,x
LBBFA:  and     $2536,y
        bpl     LBC0D
        .byte   $A7
LBC00:  .byte   $97
LBC01:  bmi     LBC43
        .byte   $0F
        jsr     L0F21
        .byte   $0F
        and     ($20,x)
        ora     #$0F
        .byte   $16
LBC0D:  jsr     L0F06
        .byte   $20
        .byte   $37
LBC12:  .byte   $0F
        .byte   $0F
        bmi     LBC52
        ora     ($0F),y
        asl     $26,x
LBC1A:  .byte   $27
        .byte   $0F
        ora     ($2C,x)
        ora     ($0F),y
        bmi     LBC59
        rol     $0F
        bmi     LBC3B
LBC26:  ora     ($0F),y
        .byte   $37
        and     ($10,x)
        .byte   $0F
        .byte   $37
        rol     joy1_press_alt
        .byte   $0F
        .byte   $37
        rol     $0F
        .byte   $0F
        jsr     L1121
        .byte   $0F
        jsr     L2937
LBC3B:  .byte   $0F
        jsr     L1626
        .byte   $0F
        jsr     L2110
LBC43:  .byte   $0F
        jsr     L1121
        .byte   $0F
        jsr     L1137
        .byte   $0F
        jsr     L0F10
        .byte   $0F
        .byte   $20
        .byte   $37
LBC52:  .byte   $0F
        .byte   $0F
        jsr     L1121
        .byte   $0F
        .byte   $27
LBC59:  .byte   $17
        asl     $0F
        .byte   $27
        .byte   $17
        and     ($0F,x)
        jsr     L2110
        .byte   $04
        .byte   $02
        .byte   $04
        .byte   $FC
        brk
        .byte   $FF
        .byte   $97
        sbc     $2801
        .byte   $47
        .byte   $67
        ora     ($C0,x)                 ; (preceding data)
        .byte   $47
        pla
        ora     ($C8,x)

; --- megaman_eye_sprites ($9C75) ---
; Mega Man center portrait eye/face detail sprites. 6 sprites × 2 bytes (tile, attr)
; per grid position. Attribute $03 = palette 3, $43 = palette 3 + H-flip.
; Updates which way Mega Man looks based on cursor position.
; Tiles $E6-$FF, $6F, $80-$81 are CHR sprite tiles for face details.
;
; Position 0 (Spark Man, top-left): eyes look upper-left
        beq     LBC7A
        sbc     ($03),y
        .byte   $F2
LBC7A:  .byte   $03
        .byte   $F3
        .byte   $03
        .byte   $F4
        .byte   $03
        sbc     $03,x

; Position 1 (Snake Man, top-center): eyes look up
        inc     $03,x
        .byte   $F7
        .byte   $03
        inc     tile_at_feet_lo,x
        sed
        .byte   $03
        sbc     $F803,y
        .byte   $43

; Position 2 (Needle Man, top-right): eyes look upper-right (pos 0 H-flipped)
        .byte   $F2
        .byte   $43
        sbc     (tile_at_feet_lo),y
        beq     LBCD6
        sbc     tile_at_feet_lo,x
        .byte   $F4
        .byte   $43
        .byte   $F3
        .byte   $43

; Position 3 (Hard Man, middle-left): eyes look left
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $FA
        .byte   $03
        .byte   $FB
        .byte   $03
        .byte   $FC
        .byte   $03

; Position 4 (center): eyes look forward (straight ahead)
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $E7
        .byte   $03
        inx
        .byte   $03
        .byte   $E7
        .byte   $43

; Position 5 (Top Man, middle-right): eyes look right (pos 3 H-flipped lower)
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $FC
        .byte   $43
        .byte   $FB
        .byte   $43
        .byte   $FA
        .byte   $43

; Position 6 (Gemini Man, bottom-left): eyes look lower-left
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        sbc     $FE03,x
        .byte   $03
        .byte   $FF
        .byte   $03

; Position 7 (Magnet Man, bottom-center): eyes look down
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $80
        .byte   $03
        sta     ($03,x)
        .byte   $80
        .byte   $43

; Position 8 (Shadow Man, bottom-right): eyes look lower-right (pos 6 H-flipped)
        .byte   $E6
LBCD6:  .byte   $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $FF                     ; (end: $189CDF = $FD, $43)
        .byte   $43
        inc     $FD43,x
        .byte   $43

; --- stage_select_lookup ($9CE1) ---
; Grid index (col + row*3) → stage $22 value. $FF = not selectable.
; Robot Masters (tier $60=0): indices 0-8
; Doc Robots (tier $60=9):   indices 9-17
;   Index: 0    1    2    3    4    5    6    7    8
;   RM:    $06  $05  $00  $03  $FF  $04  $02  $01  $07
;   Boss:  Sprk Snke Ndl  Hard ctr  Top  Gmni Mgnt Shdw
;   Doc:   $0A  $FF  $08  $FF  $FF  $FF  $09  $FF  $0B
        asl     $05                     ; RM: Spark/Snake/Needle/Hard/ctr/Top
        brk
        .byte   $03
        .byte   $FF
        .byte   $04
        .byte   $02                     ; RM: Gemini/Magnet/Shadow
        ora     ($07,x)
        asl     a                       ; Doc: Spark/$FF/Needle/$FF/$FF
        .byte   $FF
        php
        .byte   $FF
        .byte   $FF
        .byte   $FF                     ; Doc: $FF/Gemini/$FF/Shadow
        ora     #$FF
        .byte   $0B

; --- cursor_pos_y ($9CF3) ---
; Y pixel position for cursor base, per grid index (9 entries).
; Row 0=$18(24), Row 1=$58(88), Row 2=$98(152) — matches portrait frame Y positions.
        clc
        clc
        clc
        cli
        cli
        cli
        tya
        tya
        tya

; --- cursor_pos_x ($9CFC) ---
; X pixel position for cursor base, per grid index (9 entries).
; Col 0=$17(23), Col 1=$67(103), Col 2=$B7(183) — 7px into each frame.
        .byte   $17
        .byte   $67
        .byte   $B7
        .byte   $17
        .byte   $67
        .byte   $B7
        .byte   $17
        .byte   $67
        .byte   $B7

; --- cursor_bolt_offset_y ($9D05) ---
; Y offsets for the 4 corner bolts (indexed 3→0):
;   [0]=$00 top, [1]=$00 top, [2]=$26(38) bottom, [3]=$26(38) bottom
        brk
        brk
        rol     $26

; --- cursor_bolt_offset_x ($9D09) ---
; X offsets for the 4 corner bolts (indexed 3→0):
;   [0]=$00 left, [1]=$2A(42) right, [2]=$00 left, [3]=$2A(42) right
; Bolt corners: TL(0,0) TR(42,0) BL(0,38) BR(42,38)
        brk
        rol     a
        brk
        rol     a

; --- cursor_direction_offsets ($9D0D) ---
; Indexed by button bits: $01=Right, $02=Left, $04=Down, $08=Up
        brk
        ora     (ppu_ctrl_shadow,x)
        brk
        .byte   $03
        brk
        brk
        brk
        sbc     $200F,x
        rol     $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        bmi     LBD65
        ora     ($0F),y
        asl     $26,x
        .byte   $27
        .byte   $0F
        ora     ($2C,x)
        ora     ($0F),y
        bmi     LBD6C
        rol     $0F
        jsr     L1121
        .byte   $0F
        .byte   $37
        .byte   $17
        ora     $0F,x
        jsr     L0515
        .byte   $0F
        jsr     L2110
        .byte   $7C
        ror     $3938,x                 ; CHR bank/tile data
        rol     $25,x

; --- portrait_addr_lo ($9D4C) ---
; PPU nametable address low bytes for portrait frame positions (9 entries).
; Repeats per column: $62(col2)/$6C(col12)/$76(col22).
; Combined with $9D55 high bytes, pixel positions:
;   (16,24) (96,24) (176,24) — row 0
;   (16,88) (96,88) (176,88) — row 1
;   (16,152)(96,152)(176,152)— row 2
        .byte   $62                     ; row 0: Spark/Snake/Needle
        jmp     (L6276)

        jmp     (L6276)

        jmp     (L2076)

        jsr     L2120
        and     ($21,x)
        .byte   $22                     ; row 2 ($22xx = tile row 19)
        .byte   $22
        .byte   $22

; --- portrait_frame_tiles ($9D5E) ---
; Portrait frame tile layout. Groups of: [Y-offset, X-offset, count-1, tile IDs...]
; $FF = end marker. Y/X offsets are relative to portrait base nametable address.
; Frame is 8 tiles wide × 6 tiles tall (64×48 px).
;
; Group format in nametable offset terms:
;   Row 0 (+$00): top border    — 8 tiles: $45 $E0 $E1 $E2 $E2 $E3 $E4 $4B
;   Row 1 (+$21): frame body    — 6 tiles: $D5 $24 $24 $24 $24 $D6
;   Row 2 (+$41): frame body    — 6 tiles: $E5 $24 $24 $24 $24 $E6
;   Row 3 (+$61): frame body    — 6 tiles: $E5 $24 $24 $24 $24 $E6
;   Row 4 (+$81): frame body    — 6 tiles: $F5 $24 $24 $24 $24 $F6
;   Row 5 (+$A0): bottom border — 8 tiles: $55 $F0 $F1 $F2 $F2 $F3 $F4 $5B
;   $24 = empty interior tile (face tiles written separately by write_portrait_faces)
        brk                             ; row 0: top border
        brk
        .byte   $07
        eor     $E0
        sbc     ($E2,x)
LBD65:  .byte   $E2
        .byte   $E3
        cpx     $4B
        brk                             ; row 1: L border + interior + R border
        and     ($05,x)
LBD6C:  cmp     $24,x
        bit     $24
        bit     $D6
        brk                             ; row 2
        eor     ($05,x)
        sbc     $24
        bit     $24
        bit     $E6
        brk                             ; row 3
        adc     ($05,x)
        sbc     $24
        bit     $24
        bit     $E6
        brk                             ; row 4
        sta     ($05,x)
        sbc     $24,x
        bit     $24
        bit     $F6
        brk                             ; row 5: bottom border
        ldy     #$07
        eor     mmc3_shadow,x
        sbc     ($F2),y
        .byte   $F2
        .byte   $F3
        .byte   $F4
        .byte   $5B
        .byte   $FF                     ; end marker

; --- face_tiles_row0 ($9D99) ---
; Portrait face tile IDs — 4×4 grid written by write_portrait_faces.
; Same tile IDs for ALL portraits; CHR bank selects which boss face appears.
        iny                             ; face row 0
        cmp     #$CA
        .byte   $CB
        cld                             ; face row 1
        cmp     $DBDA,y
        inx                             ; face row 2
        sbc     #$EA
        .byte   $EB
        sed                             ; face row 3
        sbc     $FBFA,y

; --- face_attr_offsets ($9DA9) ---
; Attribute table offsets for portrait palette (OR'd with $23C0 base).
        ora     #$00
        asl     a:L0000
        brk
        and     #$00                    ; (attr offsets continued)
        rol     $8E21
        cpy     $CECD                   ; center face row 0
        .byte   $CF
        .byte   $DC                     ; center face row 1
        cmp     $DFDE,x
        cpx     $EEED                   ; center face row 2
        .byte   $EF
        .byte   $FC                     ; center face row 3
        sbc     $FFFE,x

; --- center_attr_data ($9DC4) ---
; Attribute table write for center portrait palette.
        .byte   $23                     ; $23DB: attr addr, 2 bytes: $88,$22
        .byte   $DB
        ora     ($88,x)
        .byte   $22

; --- face_addr_hi ($9DC9) ---
; PPU addr high bytes for face tile writes (9 entries). Same as $9D55.
        jsr     L2020                   ; rows 0/1/2
        and     ($21,x)
        and     (stage_id,x)
        .byte   $22
        .byte   $22

; --- face_addr_lo ($9DD2) ---
; PPU addr low bytes for face positions (9 entries).
; Col 0=$84(tile4), Col 1=$8E(tile14), Col 2=$98(tile24).
; Face pixel positions (inside frame, 2 tiles in from border):
;   (32,32) (112,32) (192,32) — row 0
;   (32,96) (112,96) (192,96) — row 1
;   (32,160)(112,160)(192,160)— row 2
        sty     $8E                     ; row 0: Spark/Snake/Needle
        tya
        sty     $8E                     ; row 1: Hard/center/Top
        tya
        sty     $8E                     ; row 2: Gemini/Magnet/Shadow
        tya

; --- portrait_beat_oam_offsets ($9DDB) ---
        sei
        .byte   $54
        bpl     LBE07
        tya
        rti

        clc
        .byte   $64
        brk
        .byte   $07
        .byte   $03
        ora     ($05,x)
        php
        .byte   $04
        .byte   $03
        .byte   $04
        .byte   $03
        rti

        jsr     L0801
        brk
        bpl     LBDF8
        .byte   $02
        .byte   $80
        .byte   $63
        .byte   $83
LBDF8:  .byte   $02
        ror     $826B,x
        .byte   $03
        ror     $6B,x
        sty     $02
        ror     $856B,x
        .byte   $02
        stx     $6B
LBE07:  stx     $01
        stx     $876B
        ora     ($96,x)
        .byte   $73
        dey
        .byte   $02
        stx     $73
        .byte   $89
        ora     ($8E,x)
        .byte   $73
        txa
        ora     ($96,x)
        .byte   $0F
        bmi     LBE32
        ora     ($0F),y
        .byte   $0F
        .byte   $27
        .byte   $17
        .byte   $0F
        bpl     LBE4C
        .byte   $17
        .byte   $0F
        .byte   $37
        .byte   $3C
        .byte   $0F
        .byte   $0F
LBE2B:  .byte   $37
        .byte   $27
        asl     $0F
        .byte   $07
        php
        .byte   $09
LBE32:  .byte   $0F
        and     $1929,y
        .byte   $0F
        and     ($08,x)
        bmi     LBE4A
        .byte   $0F
        bit     $0F11
        .byte   $0F
        bmi     LBE79
        .byte   $0F
        .byte   $0F
        bpl     LBE5B
        .byte   $0F
        .byte   $0F
        brk
        brk
LBE4A:  .byte   $20
        rti

LBE4C:  brk
        brk
        jsr     L8100
        brk
        brk
        bpl     LBE75
        bpl     LBE57
LBE57:  brk
        brk
        brk
        brk
LBE5B:  brk
        brk
        brk
        brk
        brk
        brk
        brk
        rti

        brk
        brk
        brk
        brk
        brk
        brk
        ora     (L0000,x)
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
LBE75:  brk
        brk
        brk
        brk
LBE79:  brk
        brk
        brk
        .byte   $02
        brk
        brk
        brk
        brk
        .byte   $04
        brk
        rti

        brk
        brk
        brk
        brk
        jsr     L8004
        brk
        brk
        brk
        brk
        brk
        .byte   $04
        brk
        .byte   $42
        ora     (L0000,x)
        brk
        rti

        brk
        .byte   $02
        brk
        brk
        brk
        brk
        brk
        .byte   $80
        brk
        .byte   $02
        ora     L0000
        brk
        brk
        brk
        rti

        brk
        brk
        bpl     LBE2B
        .byte   $04
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        bpl     LBEBB
LBEBB:  brk
        brk
        brk
        brk
        brk
        rti

        bpl     LBEC4
        pha
LBEC4:  rti

        .byte   $04
        brk
        brk
        rti

        bpl     LBECC
        brk
LBECC:  brk
        brk
        .byte   $04
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        ora     (L0000,x)
        brk
        bpl     LBEDF
LBEDF:  brk
        bmi     LBEE2
LBEE2:  jsr     L0400
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        ora     ($40,x)
        brk
        ora     (L0000,x)
        brk
        brk
        ora     (L0000,x)
        .byte   $80
        brk
        brk
        brk
        .byte   $02
        .byte   $04
        brk
        brk
        brk
        brk
        php
        brk
        ora     (L0000,x)
        brk
        ora     ($04,x)
        brk
        asl     a
        brk
        brk
        ora     ($20,x)
        brk
        jsr     L0000
        brk
        brk
        brk
        ora     (L0000,x)
        brk
        bpl     LBF2B
LBF2B:  brk
        brk
        brk
        brk
        .byte   $04
        bpl     LBF32
LBF32:  bpl     LBF34
LBF34:  brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        bpl     LBF7E
        sta     (L0000,x)
        bpl     LBF82
        brk
        brk
        brk
        bpl     LBF47
LBF47:  brk
        bpl     LBF4A
LBF4A:  brk
        brk
        brk
        brk
        .byte   $04
        brk
        ora     (L0000,x)
        brk
        brk
        php
        bpl     LBF57
LBF57:  brk
        brk
        .byte   $04
        bpl     LBF5C
LBF5C:  brk
        brk
        brk
        brk
        .byte   $0C
        brk
        jsr     L0000
        brk
        .byte   $02
        bpl     LBF69
LBF69:  brk
        .byte   $22
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        php
        brk
        ora     (L0000,x)
        brk
        brk
        brk
        brk
        brk
        brk
LBF7E:  bpl     LBF80
LBF80:  .byte   $20
        .byte   $01
LBF82:  brk
        brk
        bvc     LBF86
LBF86:  ora     (L0000,x)
        .byte   $80
        brk
        brk
        .byte   $04
        php
        brk
        php
        bpl     LBF91
LBF91:  brk
        php
        brk
        brk
        brk
        brk
        .byte   $04
        .byte   $02
        brk
        brk
        brk
        .byte   $80
        brk
        brk
        brk
        brk
        brk
        .byte   $1C
        brk
        brk
        ora     L0000
        brk
        brk
        brk
        cpy     L0000
        brk
        brk
        brk
        brk
        .byte   $02
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        bpl     LBFDF
        brk
        brk
        .byte   $04
        ora     L0000
        brk
        brk
        rti

        brk
        brk
        brk
        brk
        brk
        brk
        rti

        brk
        ora     (L0000,x)
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        jsr     L0000
LBFDF:  brk
        brk
        brk
        .byte   $02
        brk
        .byte   $04
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        rti

        brk
        brk
        brk
        rti

        brk
        brk
        brk
        brk
        jsr     L0000
        brk
        eor     #$F1
