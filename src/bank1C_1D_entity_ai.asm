; da65 V2.18 - Ubuntu 2.19-1
; Created:    2026-02-21 05:24:06
; Input file: /home/kn/megamanforever/megaman3-disassembly-ca65/tools/../build/bank1C_1D.bin
; Page:       1


        .setcpu "6502"

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

        jmp     L800C

L8003:  jmp     L8109

        jmp     L82B8

        jmp     L8097

L800C:  lda     #$55
        sta     $99
        ldx     #$01
        stx     $EF
L8014:  ldy     #$01
        cpx     $5B
        beq     L8060
        iny
        cpx     $5C
        beq     L8060
        lda     $0300,x
        bpl     L808B
        ldy     #$1D
        lda     $0320,x
        cmp     #$E0
        bcc     L8031
        ldy     #$12
        bne     L803D
L8031:  lsr     a
        lsr     a
        lsr     a
        lsr     a
        cmp     #$0A
        bcc     L803D
        sec
        sbc     #$06
        tay
L803D:  cpy     $F5
        beq     L804A
        sty     $F5
        txa
        pha
        jsr     LFF6B
        pla
        tax
L804A:  ldy     $0320,x
        lda     L83C7,y
        sta     L0000
        lda     L84C7,y
        sta     $01
        lda     #$80
        pha
        lda     #$76
        pha
        jmp     (L0000)

L8060:  lda     #$00
        sta     $5A,y
        jsr     LFB7B
        bcs     L8083
        txa
        ldy     $10
        sta     $5A,y
        lda     #$00
        sta     $05E0,x
        beq     L8083
        cpx     #$10
        bcc     L808B
        lda     $0320,x
        beq     L808B
        jsr     L8102
L8083:  lda     $0480,x
        bpl     L808B
        jsr     L8097
L808B:  inc     $EF
        ldx     $EF
        cpx     #$20
        beq     L8096
        jmp     L8014

L8096:  rts

L8097:  lda     $05C0
        cmp     #$A4
        beq     L8096
        stx     $0F
        lda     $F5
        pha
        lda     #$0A
        sta     $F5
        jsr     LFF6B
        ldx     $0F
        lda     $39
        bne     L80F9
        lda     $30
        cmp     #$06
        beq     L80F9
        cmp     #$0E
        beq     L80F9
        cmp     #$0C
        beq     L80F9
        jsr     LFAE2
        bcs     L80F9
        lda     #$06
        sta     $30
        lda     #$16
        jsr     LF89A
        lda     $A2
        and     #$1F
        beq     L80F9
        ldy     $0320,x
        lda     $A2
        and     #$1F
        sec
        sbc     LA000,y
        php
        ora     #$80
        sta     $A2
        plp
        beq     L80E7
        bcs     L80F9
L80E7:  lda     #$80
        sta     $A2
        lda     #$0E
        sta     $30
        lda     #$F2
        jsr     LF89A
        lda     #$17
        jsr     LF89A
L80F9:  pla
        sta     $F5
        jsr     LFF6B
        ldx     $0F
        rts

L8102:  lda     $0480,x
        and     #$60
        beq     L8142
L8109:  lda     $05C0
        cmp     #$A3
        bne     L8113
        jmp     L825E

L8113:  jsr     LFB7B
        bcs     L8142
        lda     $0480,x
        and     #$20
        beq     L8144
L811F:  lda     #$19
        jsr     LF89A
        ldy     $10
        lda     $04A0,y
        eor     #$03
        sta     $04A0,y
        lda     #$00
        sta     $0440,y
        lda     #$FC
        sta     $0460,y
        lda     #$00
        sta     $0480,y
        lda     #$0F
        sta     $0320,y
L8142:  sec
        rts

L8144:  lda     #$18
        jsr     LF89A
        lda     $F5
        pha
        stx     $0F
        lda     #$0A
        sta     $F5
        jsr     LFF6B
        ldx     $0F
        ldy     $A0
        lda     L83AF,y
        sta     L0000
        lda     L83BB,y
        sta     $01
        ldy     $0320,x
        lda     (L0000),y
        bne     L8170
        jsr     L811F
        jmp     L824D

L8170:  lda     $A0
        cmp     #$08
        bne     L81B6
        lda     (L0000),y
        beq     L81B3
        cmp     #$58
        bne     L81B6
        txa
        ldy     $10
        sta     $5A,y
        lda     $0300,y
        ora     #$01
        sta     $0300,y
        lda     #$9D
        cmp     $05C0,y
        beq     L81B3
        sta     $05C0,y
        lda     #$00
        sta     $05A0,y
        sta     $05E0,y
        sta     $0500,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
L81B3:  jmp     L824D

L81B6:  lda     $04E0,x
        and     #$E0
        beq     L81C0
        jmp     L822F

L81C0:  ldy     $0320,x
        lda     $04E0,x
        sec
        sbc     (L0000),y
        bcs     L81CD
        lda     #$00
L81CD:  sta     $04E0,x
        bne     L8207
        lda     $0320,x
        cmp     #$52
        beq     L8207
        cmp     #$53
        beq     L8207
        lda     $5A
        bpl     L81E5
        lda     #$59
        bne     L81E7
L81E5:  lda     #$71
L81E7:  jsr     LF835
        lda     #$00
        sta     $0480,x
        lda     $0320,x
        cmp     #$30
        bne     L81FD
        lda     #$00
        sta     $0320,x
        beq     L8202
L81FD:  lda     #$7A
        sta     $0320,x
L8202:  lda     #$90
        sta     $0580,x
L8207:  lda     $04E0,x
        beq     L822F
        lda     $0300,x
        and     #$40
        bne     L821D
        lda     $04E0,x
        ora     #$20
        sta     $04E0,x
        bne     L822F
L821D:  lda     $22
        cmp     #$0F
        beq     L8227
        cmp     #$0C
        bcs     L822F
L8227:  lda     $04E0,x
        ora     #$E0
        sta     $04E0,x
L822F:  lda     $A0
        cmp     #$05
        beq     L824D
        ldy     $10
        lda     #$00
        sta     $0300,y
        lda     $A0
        cmp     #$01
        bne     L824D
        lda     #$00
        sta     $0301
        sta     $0302
        sta     $0303
L824D:  pla
        sta     $F5
        jsr     LFF6B
        ldx     $0F
        clc
        lda     $0300,x
        and     #$40
        bne     L82AA
L825D:  rts

L825E:  lda     $0480,x
        and     #$20
        bne     L825D
        jsr     LFAE2
        bcs     L825D
        stx     $0F
        lda     $F5
        pha
        lda     #$0A
        sta     $F5
        jsr     LFF6B
        ldx     $0F
        ldy     $0320,x
        lda     L83AF
        sta     L0000
        lda     L83BB
        sta     $01
        lda     $A7
        and     #$1F
        sec
        sbc     (L0000),y
        bcs     L8290
        lda     #$00
L8290:  ora     #$80
        sta     $A7
        lda     #$0A
        sta     $30
        lda     #$08
        sta     $0500
        lda     L83B4
        sta     L0000
        lda     L83C0
        sta     $01
        jmp     L81B6

L82AA:  lda     $04E0,x
        and     #$1F
        ora     #$80
        sta     $B0
        and     #$7F
        beq     L82B8
        rts

L82B8:  lda     #$F2
        jsr     LF898
        lda     #$17
        jsr     LF89A
        ldy     #$1F
L82C4:  lda     $5A
        bmi     L82CC
        lda     #$7A
        bne     L82CE
L82CC:  lda     #$5B
L82CE:  jsr     LF846
        lda     #$80
        sta     $0300,y
        lda     #$90
        sta     $0580,y
        lda     #$00
        sta     $0480,y
        lda     #$10
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $D7E1,y
        sta     $0400,y
        lda     $D7F1,y
        sta     $0420,y
        lda     $D801,y
        sta     $0440,y
        lda     $D811,y
        sta     $0460,y
        dey
        cpy     #$0F
        bne     L82C4
        lda     $22
        cmp     #$03
        bne     L831E
        lda     #$00
        sta     $FA
L831E:  lda     #$00
        sta     $0301
        sta     $0302
        sta     $0303
        sta     $0520
        lda     $22
        cmp     #$0F
        beq     L8360
        lda     $30
        cmp     #$0E
        beq     L83AD
        lda     #$0C
        sta     $30
        lda     #$00
        sta     $32
        sta     $0500
        sta     $0301
        sta     $0302
        sta     $0303
        lda     #$01
        cmp     $05C0
        beq     L835E
        sta     $05C0
        lda     #$00
        sta     $05A0
        sta     $05E0
L835E:  clc
        rts

L8360:  lda     $30
        cmp     #$0F
        bne     L836A
        lda     #$00
        sta     $30
L836A:  lda     #$80
        sta     $030F
        lda     #$90
        sta     $058F
        lda     $0360,x
        sta     $036F
        lda     $0380,x
        sta     $038F
        lda     $03C0,x
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
L83AD:  .byte   $18,$60
L83AF:  .byte   $00,$00,$00,$00,$00
L83B4:  .byte   $00,$00,$00,$00,$00,$00,$00
L83BB:  .byte   $A1,$A4,$A2,$A5,$A3
L83C0:  .byte   $A6,$A7,$A1,$A8,$A1,$A9,$A1
L83C7:  .byte   $C7,$C9,$FB,$58,$DE,$B4,$FD,$7C
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
        .byte   $53,$53,$53,$53,$53,$53,$53,$53
        .byte   $C8,$C9,$93,$3F,$FD,$F9,$FD,$F9
        .byte   $FD,$FD,$D2,$C8,$2F,$65,$F8,$C8
        .byte   $31,$E2,$C8,$94,$AC,$B6,$C8,$C8
        .byte   $5F,$5F,$51,$C8,$C8,$C8,$C8,$C8
        .byte   $A4,$1E,$96,$DD,$88,$47,$C2,$79
        .byte   $98,$6C,$BE,$A7,$A8,$65,$95,$34
        .byte   $E8,$E8,$E8,$E8,$E8,$E8,$E8,$E8
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
L84C7:  .byte   $85,$85,$8A,$8B,$8B,$9D,$8B,$B4
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
        rts

        lda     $04A0,x
        and     #$01
        beq     L85D6
        jsr     LF71D
        jmp     L85D9

L85D6:  jsr     LF73B
L85D9:  cpx     #$10
        bcs     L8627
        ldy     #$06
        jsr     LE8D6
        lda     $41
        cmp     #$70
        bne     L8627
        lda     $0360,x
        sec
        sbc     $FC
        cmp     #$10
        bcc     L8627
        cmp     #$F0
        bcs     L8627
        jsr     LEE57
        bcs     L8627
        jsr     LFC53
        bcc     L8628
L8600:  lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0320,x
        lda     $0360,x
        and     #$F0
        ora     #$08
        sta     $0360,x
        lda     $0380,x
        sta     $0380,x
        lda     $03C0,x
        and     #$F0
        ora     #$08
        sta     $03C0,x
        jmp     L867C

L8627:  rts

L8628:  sty     $01
        lda     #$00
        sta     L0000
        ldy     #$1F
L8630:  lda     $0300,y
        bpl     L863E
        lda     $0320,y
        cmp     #$27
        bne     L863E
        inc     L0000
L863E:  dey
        cpy     #$0F
        bne     L8630
        ldy     $01
        lda     L0000
        cmp     #$03
        beq     L8600
        lda     #$71
        jsr     LF846
        lda     #$27
        sta     $0320,y
        lda     $0360,x
        and     #$F0
        ora     #$08
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        and     #$F0
        ora     #$08
        sta     $03C0,y
        lda     #$00
        sta     $0480,y
        sta     $0300,x
        lda     #$FF
        sta     $04C0,x
L867C:  stx     L0000
        lda     $13
        and     #$01
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        sta     $01
        lda     $28
        pha
        lsr     a
        ora     $01
        tay
        pla
        asl     a
        asl     a
        and     #$04
        ora     $03
        tax
        lda     $0110,y
        ora     $EB82,x
        sta     $0110,y
        ldx     L0000
        rts

        lda     $0300,x
        and     #$0F
        bne     L86BC
        jsr     LF797
        lda     $03C0,x
        clc
        adc     #$10
        cmp     $03C0
        bcc     L8712
        inc     $0300,x
L86BC:  ldy     #$00
        jsr     LF67C
        bcc     L8712
        lda     $05A0,x
        cmp     #$04
        bne     L8717
        lda     #$81
        sta     $0320,x
        lda     #$80
        sta     $0300,x
        lda     #$00
        sta     $0500,x
        ldy     #$03
        jsr     LE8D6
        lda     $10
        and     #$10
        beq     L86F0
L86E4:  inc     $0300,x
        lda     #$00
        sta     $0440,x
        sta     $0460,x
        rts

L86F0:  lda     $A0
        cmp     #$09
        bne     L86FC
        lda     $41
        cmp     #$80
        bne     L86E4
L86FC:  lda     $0580,x
        ora     #$01
        sta     $0580,x
        lda     $A0
        sec
        sbc     #$06
        lsr     a
        tay
        lda     L8718,y
        jsr     LF835
        rts

L8712:  lda     #$00
        sta     $05E0,x
L8717:  .byte   $60
L8718:  .byte   $D8,$D9,$D7
        sta     ($82,x)
        .byte   $83
        lda     $0300,x
        and     #$0F
        bne     L876B
        dec     $0500,x
        beq     L874B
        lda     $05C0,x
        cmp     #$D8
        bne     L873B
        lda     #$00
        sta     $05E0,x
        lda     $05A0,x
        bne     L8795
L873B:  lda     $0500,x
        cmp     #$88
        bcs     L8795
        lda     $05E0,x
        ora     #$80
        sta     $05E0,x
        rts

L874B:  inc     $0300,x
        lda     #$00
        sta     $0440,x
        sta     $0460,x
        sta     $0480,x
        lda     $0580,x
        and     #$FC
        sta     $0580,x
        lda     #$13
        jsr     LF835
        lda     #$04
        sta     $05A0,x
L876B:  lda     $05A0,x
        cmp     #$02
        bne     L8795
        lda     #$00
        sta     $05E0,x
        lda     $0440,x
        clc
        adc     $99
        sta     $0440,x
        lda     $0460,x
        adc     #$00
        sta     $0460,x
        jsr     LF779
        lda     $03E0,x
        beq     L8795
        lda     #$00
        sta     $0300,x
L8795:  rts

        lda     $0360
        sec
        sbc     $0360,x
        pha
        lda     $0380
        sbc     $0380,x
        pla
        bcs     L87AC
        eor     #$FF
        adc     #$01
        clc
L87AC:  php
        cmp     #$03
        bcc     L87B3
        lda     #$03
L87B3:  plp
        sta     $0420,x
        lda     #$00
        sta     $0400,x
        bcc     L87C6
        ldy     #$08
        jsr     LF580
        jmp     L87CB

L87C6:  ldy     #$09
        jsr     LF5C4
L87CB:  lda     $0580
        and     #$40
        sta     L0000
        lda     $0581
        and     #$BF
        ora     L0000
        sta     $0581
        rts

        lda     $04A0,x
        and     #$03
        beq     L884B
        lda     #$97
        cmp     $05C0,x
        beq     L87EE
        sta     $05C0,x
L87EE:  lda     $04A0,x
        and     #$01
        beq     L87FB
        jsr     LF71D
        jmp     L87FE

L87FB:  jsr     LF73B
L87FE:  ldy     #$1F
L8800:  lda     $0300,y
        bpl     L8845
        lda     $0480,y
        and     #$40
        beq     L8845
        lda     $0360,x
        sec
        sbc     $0360,y
        pha
        lda     $0380,x
        sbc     $0380,y
        pla
        bcs     L8821
        eor     #$FF
        adc     #$01
L8821:  cmp     #$08
        bcs     L8845
        lda     $0400,x
        sta     $0440,x
        lda     $0420,x
        sta     $0460,x
        lda     #$08
        sta     $04A0,x
        lda     $03C0,x
        sec
        sbc     $03C0,y
        bcs     L884A
        lda     #$04
        sta     $04A0,x
        rts

L8845:  dey
        cpy     #$0F
        bne     L8800
L884A:  rts

L884B:  lda     $0460,x
        cmp     #$06
        beq     L8863
        lda     $0440,x
        clc
        adc     #$20
        sta     $0440,x
        lda     $0460,x
        adc     #$00
        sta     $0460,x
L8863:  lda     $04A0,x
        and     #$08
        beq     L8875
        lda     #$9A
        sta     $05C0,x
        jsr     LF779
        jmp     L887D

L8875:  jsr     LF759
        lda     #$9B
        sta     $05C0,x
L887D:  lda     $03E0,x
        beq     L8887
        lda     #$00
        sta     $0300,x
L8887:  rts

        lda     $0500,x
        beq     L8899
        dec     $0500,x
        lda     $0500,x
        cmp     L8943,x
        bcc     L8899
        rts

L8899:  lda     $04A0,x
        and     #$01
        beq     L88BB
        lda     $0500,x
        beq     L88AD
        ldy     #$1E
        jsr     LF580
        jmp     L88D6

L88AD:  lda     $0580,x
        ora     #$40
        sta     $0580,x
        jsr     LF71D
        jmp     L88FD

L88BB:  lda     $0500,x
        beq     L88C8
        ldy     #$1F
        jsr     LF5C4
        jmp     L88D6

L88C8:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        jsr     LF73B
        jmp     L88FD

L88D6:  bcc     L88FD
        lda     #$00
        sta     $0440,x
        sta     $0400,x
        lda     #$03
        sta     $0460,x
        sta     $0420,x
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        and     #$0C
        bne     L8943
        lda     $04A0,x
        ora     #$08
        sta     $04A0,x
        rts

L88FD:  lda     $04A0,x
        and     #$0C
        beq     L8943
        .byte   $29
L8905:  .byte   $04
        beq     L8922
        lda     $0500,x
        beq     L891A
        ldy     #$12
        jsr     LF606
        lda     #$A0
        sta     $05C0,x
        jmp     L8939

L891A:  lda     #$A0
        sta     $05C0,x
        jmp     LF759

L8922:  lda     $0500,x
        bne     L892F
        lda     #$A1
        sta     $05C0,x
        jmp     LF779

L892F:  ldy     #$13
        jsr     LF642
        lda     #$A1
        sta     $05C0,x
L8939:  bcc     L8943
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
L8943:  .byte   $60
        ldy     $B2,x
        bcs     L8905
        cpy     #$05
        cmp     #$71
        beq     L8961
        cmp     #$AC
        beq     L8956
        cmp     #$AE
        bne     L896D
L8956:  lda     $05E0,x
        bne     L89C1
        lda     #$71
        sta     $05C0,x
        rts

L8961:  lda     $05A0,x
        cmp     #$04
        bne     L89C1
        lda     #$AF
        jsr     LF835
L896D:  lda     $0420,x
        cmp     #$03
        beq     L8985
        lda     $0400,x
        clc
        adc     #$20
        sta     $0400,x
        lda     $0420,x
        adc     #$00
        sta     $0420,x
L8985:  lda     $04A0,x
        and     #$01
        beq     L8992
        jsr     LF71D
        jmp     L8995

L8992:  jsr     LF73B
L8995:  lda     $95
        and     #$01
        beq     L89A1
        inc     $03C0,x
        jmp     L89A4

L89A1:  dec     $03C0,x
L89A4:  lda     $16
        and     #$0C
        beq     L89C1
        and     #$08
        beq     L89B4
        jsr     LF779
        jmp     L89B7

L89B4:  jsr     LF759
L89B7:  lda     $03E0,x
        beq     L89C1
        lda     #$00
        sta     $0300,x
L89C1:  rts

        lda     $0300,x
        and     #$0F
        bne     L89F6
        ldy     #$12
        jsr     LF67C
        bcs     L89DB
        lda     $0500,x
        beq     L8A47
        dec     $0500,x
        jmp     L8A80

L89DB:  lda     #$00
        sta     $0400,x
        sta     $0440,x
        lda     #$03
        sta     $0420,x
        sta     $0460,x
        lda     $04A0,x
        ora     #$04
        sta     $04A0,x
        inc     $0300,x
L89F6:  lda     $04A0,x
        and     #$08
        bne     L8A0A
        ldy     #$12
        jsr     LF606
        lda     #$A6
        sta     $05C0,x
        jmp     L8A14

L8A0A:  ldy     #$13
        jsr     LF642
        lda     #$A7
        sta     $05C0,x
L8A14:  lda     $03E0,x
        bne     L8A92
        bcs     L8A48
        lda     $04A0,x
        and     #$0C
        tay
        lda     $04A0,x
        pha
        cpy     #$08
        beq     L8A2E
        eor     #$03
        sta     $04A0,x
L8A2E:  lda     $05C0,x
        pha
        jsr     L8A55
        pla
        sta     $05C0,x
        pla
        sta     $04A0,x
        bcs     L8A78
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
L8A47:  rts

L8A48:  lda     $04A0,x
        and     #$08
        beq     L8A55
        lda     #$00
        sta     $0300,x
        rts

L8A55:  lda     #$A5
        sta     $05C0,x
        lda     $04A0,x
        and     #$01
        beq     L8A69
        ldy     #$1E
        jsr     LF580
        jmp     L8A6E

L8A69:  ldy     #$1F
        jsr     LF5C4
L8A6E:  bcc     L8A78
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
L8A78:  rts

        lda     $0300,x
        and     #$0F
        bne     L8A8D
L8A80:  lda     $04A0,x
        and     #$01
        beq     L8A8A
        jmp     LF71D

L8A8A:  jmp     LF73B

L8A8D:  dec     $0500,x
        bne     L8A97
L8A92:  lda     #$00
        sta     $0300,x
L8A97:  rts

        lda     $04A0,x
        and     #$03
        beq     L8AAC
        and     #$01
        beq     L8AA9
        jsr     LF71D
        jmp     L8AAC

L8AA9:  jsr     LF73B
L8AAC:  lda     $04A0,x
        and     #$0C
        beq     L8AC0
        and     #$08
        beq     L8ABD
        jsr     LF779
        jmp     L8AC0

L8ABD:  jsr     LF759
L8AC0:  lda     $03E0,x
        bne     L8AD1
        dec     $0500,x
        bne     L8AFA
        lda     $0300,x
        and     #$0F
        beq     L8AD7
L8AD1:  lda     #$00
        sta     $0300,x
        rts

L8AD7:  inc     $0300,x
        lda     #$14
        sta     $0500,x
        lda     $04A0,x
        and     #$0C
        beq     L8AF2
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
        and     #$03
        beq     L8AFA
L8AF2:  lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L8AFA:  rts

        lda     $0300,x
        and     #$0F
        bne     L8B0A
        inc     $0300,x
        lda     #$03
        sta     $0520,x
L8B0A:  lda     $04A0,x
        and     #$01
        beq     L8B19
        ldy     #$0A
        jsr     LF580
        jmp     L8B1E

L8B19:  ldy     #$0B
        jsr     LF5C4
L8B1E:  ldy     #$0A
        jsr     LF67C
        bcc     L8B51
        lda     $0500,x
        tay
        lda     L8B52,y
        sta     $0440,x
        lda     L8B55,y
        sta     $0460,x
        dec     $0520,x
        bne     L8B42
        jsr     LF869
        lda     #$03
        sta     $0520,x
L8B42:  inc     $0500,x
        lda     $0500,x
        cmp     #$03
        bcc     L8B51
        lda     #$00
        sta     $0500,x
L8B51:  .byte   $60
L8B52:  .byte   $44,$44,$EA
L8B55:  .byte   $03
        .byte   $03
        .byte   $07
        lda     $05C0,x
        cmp     #$23
        beq     L8B73
        lda     $04A0,x
        and     #$01
        beq     L8B6E
        ldy     #$08
        jsr     LF580
        jmp     L8B73

L8B6E:  ldy     #$09
        jsr     LF5C4
L8B73:  bcc     L8B7D
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L8B7D:  lda     $0300,x
        and     #$0F
        bne     L8B92
        jsr     LF8C2
        cmp     #$04
        bcs     L8BA8
        inc     $0300,x
        lda     #$23
        bne     L8BA5
L8B92:  lda     $05C0,x
        cmp     #$24
        beq     L8BA8
        lda     $05A0,x
        cmp     #$06
        bne     L8BA8
        jsr     L8BA9
        lda     #$24
L8BA5:  jsr     LF835
L8BA8:  rts

L8BA9:  jsr     LFC53
        bcs     L8BDD
        lda     $04A0,x
        sta     $04A0,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$11
        sta     $03C0,y
        lda     #$01
        sta     $04E0,y
        lda     #$25
        jsr     LF846
        lda     #$04
        sta     $0320,y
        lda     #$C0
        sta     $0480,y
L8BDD:  rts

        lda     $0300,x
        and     #$0F
        bne     L8BEB
        jsr     LF81B
        inc     $0300,x
L8BEB:  ldy     #$08
        jsr     LF67C
        bcc     L8BFC
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0320,x
L8BFC:  rts

        lda     $0300,x
        and     #$0F
        bne     L8C12
        sta     $0520,x
        lda     #$1E
        sta     $0500,x
        jsr     LF883
        inc     $0300,x
L8C12:  lda     $0500,x
        bne     L8C2A
        lda     $05E0,x
        ora     $05A0,x
        bne     L8C2D
        lda     #$27
        cmp     $05C0,x
        beq     L8C42
        sta     $05C0,x
        rts

L8C2A:  dec     $0500,x
L8C2D:  lda     $04A0,x
        pha
        jsr     LF869
        pla
        cmp     $04A0,x
        beq     L8C42
        lda     $0580,x
        eor     #$40
        sta     $0580,x
L8C42:  lda     $05C0,x
        cmp     #$27
        bne     L8C7F
        lda     $0520,x
        bne     L8C59
        lda     $0480,x
        eor     #$60
        sta     $0480,x
        inc     $0520,x
L8C59:  lda     $05A0,x
        cmp     #$0A
        bne     L8C69
        lda     $05E0,x
        bne     L8C69
        jsr     L8C80
        rts

L8C69:  lda     $05E0,x
        ora     $05A0,x
        bne     L8C7F
        dec     $05C0,x
        dec     $0300,x
        lda     $0480,x
        eor     #$60
        sta     $0480,x
L8C7F:  rts

L8C80:  jsr     LFC53
        bcs     L8CCE
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     L8CCF,y
        pha
        lda     $0380,x
        adc     L8CD0,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sec
        sbc     #$06
        sta     $03C0,y
        lda     #$33
        sta     $0400,y
        lda     #$03
        sta     $0420,y
        lda     #$28
        jsr     LF846
        lda     #$2D
        sta     $0320,y
        lda     #$C0
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
L8CCE:  .byte   $60
L8CCF:  .byte   $13
L8CD0:  brk
        sbc     LA0FF
        brk
        jsr     LF67C
        rol     $0F
        lda     $05C0,x
        cmp     #$6A
        beq     L8D03
        lda     $0520,x
        beq     L8CEF
        lda     $0500,x
        beq     L8CEF
        dec     $0500,x
        rts

L8CEF:  lda     $04A0,x
        and     #$01
        beq     L8CFE
        ldy     #$00
        jsr     LF580
        jmp     L8D03

L8CFE:  ldy     #$01
        jsr     LF5C4
L8D03:  bcc     L8D0D
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L8D0D:  lda     $0300,x
        and     #$0F
        bne     L8D23
        jsr     LF8C2
        cmp     #$40
        bcs     L8D23
        inc     $0300,x
        lda     #$6A
        jsr     LF835
L8D23:  lda     $05C0,x
        cmp     #$6A
        bne     L8D9C
        lda     $05A0,x
        cmp     #$02
        bne     L8D9C
        lda     #$6B
        jsr     LF835
        lda     $0480,x
        eor     #$60
        sta     $0480,x
        lda     $03C0,x
        sec
        sbc     #$10
        sta     $03C0,x
        lda     #$4D
        sta     $0440,x
        lda     #$07
        sta     $0460,x
        jsr     LFC53
        bcs     L8D9C
        sty     L0000
        lda     $04A0,x
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     L8DC4,y
        pha
        lda     $0380,x
        adc     L8DC5,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$01
        sta     $04E0,y
        lda     #$6C
        jsr     LF846
        lda     #$C2
        sta     $0480,y
        lda     #$44
        sta     $0440,y
        lda     #$03
        sta     $0460,y
        lda     #$09
        sta     $0320,y
        jmp     L8DC3

L8D9C:  lda     $05C0,x
        cmp     #$6B
        bne     L8DC3
        lda     $0F
        and     #$01
        beq     L8DC3
        lda     #$6D
        jsr     LF835
        jsr     LF869
        lda     #$00
        sta     $0400,x
        lda     #$02
        sta     $0420,x
        lda     #$10
        sta     $0500,x
        inc     $0520,x
L8DC3:  rts

L8DC4:  .byte   $20
L8DC5:  brk
        cpx     #$FF
        jmp     LF797

        lda     $0300,x
        and     #$0F
        bne     L8DED
L8DD2:  sta     $0460,x
        lda     #$C0
        sta     $0440,x
        lda     $0320,x
        sec
        sbc     #$15
        tay
        lda     L8E12,y
        sta     $0500,x
        sta     $0520,x
        inc     $0300,x
L8DED:  lda     $04A0,x
        and     #$01
        beq     L8DFA
        jsr     LF779
        jmp     L8DFD

L8DFA:  jsr     LF759
L8DFD:  dec     $0500,x
        bne     L8E11
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     $0520,x
        asl     a
        sta     $0500,x
L8E11:  rts

L8E12:  rts

        bvs     L8DD2
        brk
        .byte   $03
        and     #$0F
        bne     L8E29
        inc     $0300,x
        lda     #$00
        sta     $0500,x
        sta     $0520,x
        jsr     LF883
L8E29:  lda     $0300,x
        and     #$02
        beq     L8E33
        jmp     L8ED3

L8E33:  lda     $0500,x
        bne     L8E86
        ldy     $0520,x
        lda     L8F3C,y
        asl     a
        tay
        lda     L8F4A,y
        sta     $0440,x
        lda     L8F4B,y
        sta     $0460,x
        lda     L8F6A,y
        sta     $0400,x
        lda     L8F6B,y
        sta     $0420,x
        lda     $0420,x
        bpl     L8E72
        lda     $0400,x
        eor     #$FF
        clc
        adc     #$01
        sta     $0400,x
        lda     $0420,x
        eor     #$FF
        adc     #$00
        sta     $0420,x
L8E72:  inc     $0520,x
        lda     $0520,x
        cmp     #$0E
        bne     L8E81
        lda     #$00
        sta     $0520,x
L8E81:  lda     #$05
        sta     $0500,x
L8E86:  dec     $0500,x
        lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $04A0,x
        and     #$02
        bne     L8EAA
        jsr     LF71D
        bcs     L8EAD
        bcc     L8EAD
L8EAA:  jsr     LF73B
L8EAD:  lda     $0320,x
        cmp     #$0A
        bne     L8F04
        jsr     LFB7B
        bcs     L8ED2
        ldy     $10
        lda     #$00
        sta     $0300,y
        inc     $0300,x
        lda     #$80
        sta     $0400,x
        lda     #$02
        sta     $0420,x
        lda     #$48
        jsr     LF835
L8ED2:  rts

L8ED3:  lda     $0320,x
        cmp     #$0A
        bne     L8F1F
        lda     $05C0,x
        cmp     #$49
        beq     L8EF7
        lda     $05E0,x
        ora     $05A0,x
        bne     L8EF6
        lda     #$49
        jsr     LF835
        lda     $0480,x
        ora     #$C3
        sta     $0480,x
L8EF6:  rts

L8EF7:  lda     $04A0,x
        and     #$01
        beq     L8F01
        jmp     LF71D

L8F01:  jmp     LF73B

L8F04:  jsr     LF8C2
        cmp     #$30
        bcs     L8EF6
        lda     #$00
        sta     $02
        lda     #$02
        sta     $03
        jsr     LFC63
        lda     $0C
        sta     $04A0,x
        inc     $0300,x
        rts

L8F1F:  lda     $04A0,x
        and     #$01
        beq     L8F2C
        jsr     LF71D
        jmp     L8F2F

L8F2C:  jsr     LF73B
L8F2F:  lda     $04A0,x
        and     #$08
        beq     L8F39
        jmp     LF779

L8F39:  .byte   $4C,$59,$F7
L8F3C:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$01
        .byte   $02,$03,$04,$05,$06,$07
L8F4A:  .byte   $CD
L8F4B:  .byte   $FE,$E5,$FE,$27,$FF,$8B,$FF,$00
        .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE
L8F6A:  .byte   $00
L8F6B:  .byte   $00,$75,$00,$D9,$00,$1B,$01,$33
        .byte   $01,$1B,$01,$D9,$00,$75,$00,$00
        .byte   $00,$8B,$FF,$27,$FF,$E5,$FE,$CD
        .byte   $FE,$E5,$FE,$27
        .byte   $FF
        .byte   $8B
        .byte   $FF
        lda     $0300,x
        and     #$0F
        bne     L8FCF
        jsr     LFAF6
        bcc     L8F97
        rts

L8F97:  inc     $0300,x
        lda     #$CC
        sta     $0440,x
        lda     #$00
        sta     $0460,x
        lda     #$02
        sta     $04A0,x
        lda     #$10
        sta     $0500,x
        lda     #$B4
        sta     $0540,x
        lda     #$E8
        sta     $03C0,x
        lda     $0360,x
        and     #$F0
        ora     #$08
        sta     $0560,x
        lda     #$00
        sta     $0340,x
        lda     $0580,x
        and     #$FB
        sta     $0580,x
L8FCF:  jsr     LF779
        lda     $0580,x
        and     #$20
        beq     L8FEC
        ldy     #$06
        jsr     LE8D6
        lda     $10
        and     #$10
        bne     L9018
        lda     $0580,x
        and     #$DF
        sta     $0580,x
L8FEC:  lda     $04A0,x
        and     #$01
        beq     L8FF9
        jsr     LF71D
        jmp     L8FFC

L8FF9:  jsr     LF73B
L8FFC:  dec     $0500,x
        bne     L9018
        inc     $0520,x
        lda     $0520,x
        and     #$03
        sta     $0520,x
        tay
        lda     L9030,y
        sta     $04A0,x
        lda     #$0E
        sta     $0500,x
L9018:  lda     $0540,x
        beq     L9025
        dec     $0540,x
        bne     L902F
        jsr     L9034
L9025:  lda     $03E0,x
        beq     L902F
        lda     #$00
        sta     $0300,x
L902F:  .byte   $60
L9030:  .byte   $02
        ora     ($01,x)
        .byte   $02
L9034:  jsr     LFC53
        bcs     L9095
        lda     #$70
        jsr     LF846
        lda     #$14
        sta     $0320,y
        lda     #$81
        sta     $0300,y
        lda     $0580,y
        ora     #$21
        sta     $0580,y
        lda     #$0F
        sta     $0480,y
        lda     $0560,x
        sta     $0360,y
        sta     $0560,y
        lda     $0380,x
        sta     $0380,y
        lda     #$00
        sta     $0520,y
        sta     $03E0,y
        sta     $0460,y
        sta     $0420,y
        lda     #$CC
        sta     $0440,y
        lda     #$80
        sta     $0400,y
        lda     #$E8
        sta     $03C0,y
        lda     #$02
        sta     $04A0,y
        lda     #$10
        sta     $0500,y
        lda     #$B4
        sta     $0540,y
        lda     #$E8
        sta     $03C0,y
L9095:  rts

        lda     $0500,x
        bne     L90DE
        jsr     LFC53
        bcs     L90D6
        lda     $04A0,x
        sta     $04A0,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$01
        sta     $04E0,y
        lda     #$18
        sta     $0320,y
        lda     #$4E
        jsr     LF846
        lda     #$C0
        sta     $0480,y
        lda     #$80
        sta     $0400,y
        lda     #$01
        sta     $0420,y
L90D6:  lda     #$F0
        sta     $0500,x
        jmp     LF869

L90DE:  dec     $0500,x
        rts

        lda     $0300,x
        and     #$0F
        bne     L90EF
        jsr     LF81B
        inc     $0300,x
L90EF:  lda     $05C0,x
        cmp     #$4E
        bne     L912C
        ldy     #$08
        jsr     LF67C
        ror     L0000
        lda     $41
        cmp     #$40
        beq     L9107
        lda     L0000
        bpl     L9164
L9107:  lda     $04A0,x
        and     #$01
        beq     L9116
        lda     $43
        cmp     #$40
        bne     L9142
        beq     L911C
L9116:  lda     $42
        cmp     #$40
        bne     L9142
L911C:  lda     #$4F
        jsr     LF835
        lda     #$80
        sta     $0440,x
        lda     #$01
        sta     $0460,x
        rts

L912C:  ldy     #$0C
        jsr     LF606
        bcc     L9141
        dec     $0300,x
        jsr     LF869
        jsr     LF81B
        lda     #$4E
        jsr     LF835
L9141:  rts

L9142:  lda     $04A0,x
        and     #$01
        beq     L9151
        ldy     #$08
        jsr     LF580
        jmp     L9156

L9151:  ldy     #$09
        jsr     LF5C4
L9156:  lda     $10
        and     #$10
        beq     L9164
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L9164:  rts

        ldy     #$00
        jsr     LF67C
        bcc     L9186
        lda     $0300,x
        and     #$0F
        bne     L9187
        inc     $0300,x
        lda     #$44
        sta     $0440,x
        lda     #$03
        sta     $0460,x
        jsr     LF869
        jsr     LF883
L9186:  rts

L9187:  lda     $04A0,x
        and     #$01
        beq     L9191
        jmp     LF71D

L9191:  jmp     LF73B

        ldy     #$1E
        jsr     LF67C
        lda     $05C0,x
        cmp     #$BC
        bne     L9209
        lda     $05E0,x
        ora     $05A0,x
        bne     L9208
        jsr     L9286
        lda     $0540,x
        bne     L9209
        dec     $05C0,x
        jsr     LFC53
        bcs     L9203
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sbc     #$17
        sta     $03C0,y
        lda     #$BD
        jsr     LF846
        lda     #$75
        sta     $0320,y
        lda     #$C0
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
        lda     #$80
        sta     $04C0,y
        lda     #$00
        sta     $04A0,y
        sta     $0400,y
        sta     $0420,y
        sta     $0440,y
        sta     $0540,y
        lda     #$FE
        sta     $0460,y
        lda     #$08
        sta     $0500,y
        sta     $0520,y
L9203:  lda     #$00
        sta     $0500,x
L9208:  rts

L9209:  lda     $0300,x
        and     #$0F
        bne     L9256
        lda     $04A0,x
        and     #$01
        beq     L921F
        ldy     #$20
        jsr     LF580
        jmp     L9224

L921F:  ldy     #$21
        jsr     LF5C4
L9224:  bcc     L922E
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L922E:  jsr     LF8C2
        cmp     #$30
        bcs     L9242
        lda     #$3C
        sta     $0520,x
        inc     $0300,x
        lda     #$C2
        jmp     LF835

L9242:  lda     #$CA
        sta     $0480,x
        inc     $0500,x
        lda     $0500,x
        cmp     #$1E
        bne     L9285
        lda     #$BC
        jmp     LF835

L9256:  jsr     LF8B3
        bcs     L925F
        lda     #$DB
        bne     L9261
L925F:  lda     #$CA
L9261:  sta     $0480,x
        lda     $0520,x
        beq     L926E
        dec     $0520,x
        bne     L9285
L926E:  jsr     LF8C2
        cmp     #$30
        bcc     L9285
        dec     $0300,x
        lda     #$BB
        jsr     LF835
        jsr     LF869
        lda     #$00
        sta     $0500,x
L9285:  rts

L9286:  lda     #$00
        sta     L0000
        lda     #$80
        sta     $01
        ldy     #$1F
L9290:  lda     $0300,y
        bmi     L92AA
L9295:  dey
        cpy     #$0F
        bne     L9290
        lda     L0000
        bne     L92A4
        lda     #$00
        sta     $0540,x
        rts

L92A4:  lda     #$FF
        sta     $0540,x
        rts

L92AA:  lda     $01
        cmp     $04C0,y
        bne     L9295
        inc     L0000
        jmp     L9295

        lda     $0500,x
        bne     L92F5
        jsr     LF954
        lda     $04A0,x
        clc
        adc     $0540,x
        tay
        lda     L9349,y
        sta     $0440,x
        lda     L9369,y
        sta     $0460,x
        lda     L9389,y
        sta     $0400,x
        lda     L93A9,y
        sta     $0420,x
        lda     L93C9,y
        sta     $05C0,x
        lda     $0580,x
        and     #$BF
        ora     L93E9,y
        sta     $0580,x
        lda     $0520,x
        sta     $0500,x
L92F5:  dec     $0500,x
        lda     #$00
        sta     L0000
        lda     $0420,x
        bpl     L9303
        dec     L0000
L9303:  lda     $0340,x
        clc
        adc     $0400,x
        sta     $0340,x
        lda     $0360,x
        adc     $0420,x
        sta     $0360,x
        lda     $0380,x
        adc     L0000
        sta     $0380,x
        lda     #$00
        sta     L0000
        lda     $0460,x
        bpl     L9329
        dec     L0000
L9329:  lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $03E0,x
        adc     L0000
        beq     L9348
        lda     #$00
        sta     $0300,x
L9348:  .byte   $60
L9349:  .byte   $00,$27,$4B,$3D,$00,$C3,$B5,$D9
        .byte   $00,$D0,$B5,$C3,$00,$3D,$4B,$27
        .byte   $CD,$E5,$27,$8B,$00,$75,$D9,$1B
        .byte   $33,$1B,$D9,$75,$00,$8B,$27,$E5
L9369:  .byte   $FE,$FE,$FF,$FF,$00,$00,$00,$01
        .byte   $02,$01,$00,$00,$00,$FF,$FF,$FE
        .byte   $FE,$FE,$FF,$FF,$00,$00,$00,$01
        .byte   $01,$01,$00,$00,$00,$FF,$FF,$FE
L9389:  .byte   $00,$C3,$B5,$D9,$00,$D9,$B5,$C3
        .byte   $00,$3D,$4B,$27,$00,$27,$4B,$3D
        .byte   $00,$75,$D9,$1B,$33,$1B,$D9,$75
        .byte   $00,$8B,$27,$E5,$CD,$E5,$27,$8B
L93A9:  .byte   $00,$00,$00,$01,$02,$01,$00,$00
        .byte   $00,$FF,$FF,$FE,$FE,$FE,$FF,$FF
        .byte   $00,$00,$00,$01,$01,$01,$00,$00
        .byte   $00,$FF,$FF,$FE,$FE,$FE,$FF,$FF
L93C9:  .byte   $BD,$BD,$BE,$BE,$BF,$BF,$C0,$C0
        .byte   $C1,$C1,$C0,$C0,$BF,$BF,$BE,$BE
        .byte   $41,$41,$41,$41,$41,$41,$41,$41
        .byte   $41,$41,$41,$41,$41,$41,$41,$41
L93E9:  .byte   $00,$00,$40,$40,$40,$40,$40,$40
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$40,$40,$40,$40,$40
        .byte   $00,$00,$00,$00,$00
        brk
        brk
        brk
        jsr     LFAE2
        bcc     L9459
        lda     #$00
        sta     L0000
        lda     $0420,x
        bpl     L9419
        dec     L0000
L9419:  lda     $0340,x
        clc
        adc     $0400,x
        sta     $0340,x
        lda     $0360,x
        adc     $0420,x
        sta     $0360,x
        lda     $0380,x
        adc     L0000
        sta     $0380,x
        lda     #$00
        sta     L0000
        lda     $0460,x
        bpl     L943F
        dec     L0000
L943F:  lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $03E0,x
        adc     L0000
        beq     L945E
L9459:  lda     #$00
        sta     $0300,x
L945E:  rts

        lda     $0300,x
        and     #$0F
        bne     L946E
        inc     $0300,x
        lda     #$3C
        sta     $0500,x
L946E:  lda     $0480,x
        and     #$E0
        sta     $0480,x
        ldy     $0320,x
        lda     $03C0,x
        pha
        clc
        adc     L94AF,y
        sta     $03C0,x
        jsr     L8097
        lda     $04A0,x
        and     #$01
        beq     L9496
        ldy     #$08
        jsr     LF580
        jmp     L949B

L9496:  ldy     #$09
        jsr     LF5C4
L949B:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        bcc     L94AF
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        bne     L94D4
L94AF:  .byte   $BC
        .byte   $20
L94B1:  .byte   $03
        .byte   $BD
L94B3:  cpy     #$03
L94B5:  clc
        adc     L94B1,y
        sta     $03C0,x
        jsr     L8097
        ldy     #$08
        jsr     LF67C
        ldy     $04A0,x
        lda     $41,y
        bne     L94D4
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L94D4:  lda     $0500,x
        bne     L94F9
        lda     $05A0,x
        ora     $05E0,x
        bne     L9509
        ldy     $0320,x
        lda     $05C0,x
        cmp     L94B3,y
        bne     L94FE
        sec
        sbc     #$02
        sta     $05C0,x
        lda     #$3C
        sta     $0500,x
        bne     L9501
L94F9:  dec     $0500,x
        bne     L9509
L94FE:  inc     $05C0,x
L9501:  lda     #$00
        sta     $05E0,x
        sta     $05A0,x
L9509:  pla
        sta     $03C0,x
        ldy     $0320,x
        lda     $05C0,x
        cmp     L94B3,y
        bne     L9526
        lda     $0480,x
        and     #$E0
        ora     L94B5,y
        sta     $0480,x
        .byte   $20,$97,$80
L9526:  .byte   $60,$D8,$C8,$50,$70
        cmp     $E1
        asl     $14,x
        lda     $0300,x
        and     #$0F
        bne     L9540
        jsr     LF8C2
        cmp     #$3C
        bcs     L9592
        inc     $0300,x
L9540:  lda     $0520,x
        bne     L958F
        lda     #$70
        sta     $0500,x
        jsr     L95B9
        bcs     L9592
        jsr     LFC53
        bcs     L9592
        lda     #$94
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$62
        sta     $0320,y
        lda     $0480,x
        sta     $0480,y
        lda     #$B2
        sta     $0580,y
        lda     #$5B
        sta     $0520,x
        lda     #$AB
        sta     $0440,y
        lda     #$FF
        sta     $0460,y
        lda     #$08
        sta     $04E0,y
L958F:  dec     $0520,x
L9592:  rts

        ldy     #$1E
        jsr     LF67C
        bcs     L95B0
        lda     $03C0,x
        cmp     #$70
        bcc     L95B0
        lda     #$90
        sta     $0500,x
        jsr     L95B9
        bcc     L95B0
        lda     #$70
        sta     $03C0,x
L95B0:  lda     $0580,x
        ora     #$20
        sta     $0580,x
        rts

L95B9:  stx     L0000
        ldy     #$1F
L95BD:  cpy     L0000
        beq     L95DD
        lda     $0300,y
        bpl     L95DD
        lda     $0580,y
        and     #$04
        bne     L95DD
        lda     $0360,x
        cmp     $0360,y
        bne     L95DD
        lda     $03C0,y
        cmp     $0500,x
        beq     L95E3
L95DD:  dey
        cpy     #$0F
        bne     L95BD
        clc
L95E3:  rts

        lda     $0300,x
        and     #$0F
        bne     L95F9
        jsr     LF883
        jsr     LF869
        inc     $0300,x
        lda     #$24
        sta     $0500,x
L95F9:  lda     $0520,x
        bne     L9643
        lda     $0500,x
        bne     L963F
        lda     $04A0,x
        and     #$02
        bne     L9617
        jsr     LF8D9
        sec
        sbc     #$01
        cmp     #$07
        bcs     L9639
        jmp     L9621

L9617:  jsr     LF8D9
        sec
        sbc     #$09
        cmp     #$07
        bcs     L9639
L9621:  lda     $05C0,x
        cmp     #$D1
        bne     L962C
        lda     #$D2
        bne     L962E
L962C:  lda     #$D5
L962E:  jsr     LF835
        jsr     L9659
        lda     #$10
        sta     $0520,x
L9639:  lda     #$78
        .byte   $9D
L963C:  brk
        ora     $60
L963F:  dec     $0500,x
        rts

L9643:  dec     $0520,x
        bne     L9658
        lda     $05C0,x
        cmp     #$D2
        bne     L9653
        lda     #$D1
        bne     L9655
L9653:  lda     #$D4
L9655:  jsr     LF835
L9658:  rts

L9659:  jsr     LFC53
        bcs     L96C0
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     L96C1,y
        pha
        lda     $0380,x
        adc     L96C2,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     #$00
        sta     $04E0,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $03E0,x
        sta     $03E0,y
        lda     #$66
        sta     $0400,y
        sta     $02
        lda     #$03
        sta     $0420,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     LFC63
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     $04A0,y
        lda     #$73
        jsr     LF846
        lda     #$8F
        sta     $0320,y
        lda     #$8B
        .byte   $99,$80,$04
L96C0:  .byte   $60
L96C1:  .byte   $04
L96C2:  brk
        .byte   $FC
        .byte   $FF
        lda     $0300,x
        and     #$0F
        bne     L96E5
        jsr     LF8C2
        cmp     #$51
        bcs     L9716
        jsr     LF869
        jsr     LF883
        lda     $0580,x
        and     #$FB
        sta     $0580,x
        inc     $0300,x
        rts

L96E5:  lda     $0500,x
        beq     L9717
        dec     $0500,x
        lda     $0300,x
        and     #$01
        bne     L9716
        lda     $0460,x
        bmi     L96FD
        cmp     #$02
        bcs     L9716
L96FD:  lda     $0440,x
        clc
        adc     #$10
        sta     $0440,x
        lda     $0460,x
        adc     #$00
        sta     $0460,x
        bpl     L9713
        jmp     LF7A8

L9713:  jmp     LF7C8

L9716:  rts

L9717:  lda     $0300,x
        and     #$0F
        cmp     #$04
        beq     L9776
        cmp     #$03
        beq     L9767
        cmp     #$02
        beq     L9745
        jsr     LF8B3
        bcc     L9734
        cmp     #$4D
        bcc     L9734
        jmp     LF797

L9734:  lda     #$14
        sta     $0500,x
        inc     $0300,x
        jsr     LF81B
        jsr     LF869
        jmp     LF883

L9745:  lda     $04A0,x
        and     #$02
        beq     L9758
        jsr     LF8C2
        bcc     L9755
        cmp     #$29
        bcs     L9764
L9755:  jmp     LF73B

L9758:  jsr     LF8C2
        bcs     L9761
        cmp     #$29
        bcs     L9764
L9761:  jmp     LF71D

L9764:  jmp     L9734

L9767:  jsr     LF8B3
        bcc     L9734
        cmp     #$09
        bcc     L9773
        jmp     LF797

L9773:  jmp     L9734

L9776:  lda     $04A0,x
        and     #$02
        beq     L9780
        jmp     LF73B

L9780:  jmp     LF71D

        lda     $0500,x
        beq     L978D
        dec     $0500,x
        bne     L97B3
L978D:  lda     $0300,x
        and     #$0F
        bne     L97D1
        jsr     LF869
        jsr     LF883
        lda     $05A0,x
        bne     L97B9
        lda     $05E0,x
        cmp     #$01
        bne     L97B9
        jsr     LF8C2
        cmp     #$41
        bcs     L97B3
        lda     #$C3
        sta     $0480,x
        rts

L97B3:  lda     #$00
        sta     $05E0,x
        rts

L97B9:  lda     $05A0,x
        cmp     #$02
        bne     L97D0
        jsr     L981D
        inc     $0300,x
        lda     #$13
        sta     $0520,x
        lda     #$3C
        sta     $0500,x
L97D0:  rts

L97D1:  lda     #$1D
        cmp     $05C0,x
        beq     L97DB
        jsr     LF835
L97DB:  ldy     #$00
        jsr     LF67C
        bcc     L97D0
        lda     $0520,x
        beq     L97FB
        dec     $0520,x
        lda     $04A0,x
        and     #$01
        beq     L97F6
        ldy     #$00
        jmp     LF580

L97F6:  ldy     #$01
        jmp     LF5C4

L97FB:  lda     #$1C
        jsr     LF835
        lda     $0300,x
        and     #$F0
        sta     $0300,x
        lda     #$A3
        sta     $0480,x
        lda     $E5
        adc     $E6
        sta     $E6
        and     #$03
        tay
        lda     L9881,y
        sta     $0500,x
        rts

L981D:  stx     L0000
        lda     #$02
        sta     $01
L9823:  jsr     LFC53
        bcs     L9872
        ldx     $01
        lda     L9875,x
        sta     $0400,y
        lda     L9878,x
        sta     $0420,y
        lda     L987B,x
        sta     $0440,y
        lda     L987E,x
        sta     $0460,y
        lda     #$73
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        ldx     L0000
        lda     #$0F
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$04
        sta     $03C0,y
        lda     $04A0,x
        sta     $04A0,y
        dec     $01
        bpl     L9823
L9872:  .byte   $A6,$00,$60
L9875:  .byte   $FB,$33,$FB
L9878:  .byte   $00,$01,$00
L987B:  .byte   $50,$00,$B0
L987E:  .byte   $FF,$00,$00
L9881:  asl     L963C,x
        .byte   $3C
        lda     #$00
        sta     L0000
        lda     $0460,x
        bpl     L9890
        dec     L0000
L9890:  lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $03E0,x
        adc     L0000
        bne     L98AD
        jmp     L9776

L98AD:  lda     #$00
        sta     $0300,x
        rts

        lda     $0500,x
        beq     L98BD
        dec     $0500,x
        bne     L98D0
L98BD:  lda     $05A0,x
        bne     L98DB
        lda     $05E0,x
        cmp     #$01
        bne     L98DB
        jsr     LF8C2
        cmp     #$51
        bcc     L98D6
L98D0:  lda     #$00
        sta     $05E0,x
        rts

L98D6:  lda     #$C9
        sta     $0480,x
L98DB:  jsr     LF869
        jsr     LF883
        lda     $05A0,x
        ora     $05E0,x
        bne     L98FE
        lda     $E4
        adc     $E7
        sta     $E7
        and     #$01
        tay
        lda     L9995,y
        sta     $0500,x
        lda     #$A9
        sta     $0480,x
L98FD:  rts

L98FE:  lda     $05E0,x
        bne     L98FD
        lda     $05A0,x
        cmp     #$09
        beq     L990F
        cmp     #$12
        beq     L990F
        rts

L990F:  jsr     LFC53
        bcs     L98FD
        lda     #$00
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        lda     #$6F
        jsr     LF846
        lda     #$1E
        jsr     LF89A
        lda     #$C0
        sta     $0480,y
        lda     #$13
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sec
        sbc     #$0C
        sta     $03C0,y
        lda     $03E0,x
        sta     $03E0,y
        lda     $04A0,x
        sta     $04A0,y
        pha
        jsr     LF8C2
        stx     L0000
        ldx     #$03
L995B:  cmp     L9989,x
        bcc     L9963
        dex
        bne     L995B
L9963:  lda     L998D,x
        sta     $0400,y
        lda     L9991,x
        sta     $0420,y
        pla
        and     #$02
        tax
        lda     $0360,y
        clc
        adc     L9997,x
        sta     $0360,y
        lda     $0380,y
        adc     L9998,x
        sta     $0380,y
        .byte   $A6,$00,$60
L9989:  .byte   $4C,$3D,$2E,$1F
L998D:  .byte   $00,$80,$00,$80
L9991:  .byte   $02,$01,$01,$00
L9995:  .byte   $3C,$78
L9997:  .byte   $0C
L9998:  brk
        .byte   $F4
        .byte   $FF
        ldy     #$08
        jsr     LF67C
        bcs     L99B9
        lda     $04A0,x
        and     #$02
        beq     L99B1
        ldy     #$07
        jsr     LF5C4
        jmp     L99B6

L99B1:  ldy     #$08
        jsr     LF580
L99B6:  bcs     L99B9
        rts

L99B9:  lda     #$00
        sta     $0320,x
        lda     #$71
        jmp     LF835

        lda     $0300,x
        and     #$0F
        cmp     #$01
        beq     L9A1E
        cmp     #$02
        beq     L9A32
        cmp     #$03
        bne     L99D7
        jmp     L9776

L99D7:  jsr     LF869
        jsr     LF883
        lda     $05C0,x
        cmp     #$1F
        beq     L9A0E
        lda     $05A0,x
        bne     L99FB
        jsr     LF8C2
        cmp     #$61
        bcs     L9A18
        lda     #$C3
        sta     $0480,x
        inc     $05A0,x
        lda     $05A0,x
L99FB:  cmp     #$05
        bne     L9A1D
        lda     #$00
        sta     $0440,x
        lda     #$02
        sta     $0460,x
        lda     #$1F
        jmp     LF835

L9A0E:  jsr     LF8B3
        cmp     #$49
        bcs     L9A4B
        jmp     LF779

L9A18:  lda     #$00
        sta     $05E0,x
L9A1D:  rts

L9A1E:  jsr     LF8C2
        lda     $04A0,x
        and     #$02
        beq     L9A2D
        bcs     L9A4F
        jmp     L9776

L9A2D:  bcc     L9A4F
        jmp     L9776

L9A32:  jsr     LF869
        jsr     LF883
        lda     $0500,x
        beq     L9A41
        dec     $0500,x
        rts

L9A41:  jsr     LF8B3
        cmp     #$04
        bcc     L9A4B
        jmp     LF759

L9A4B:  inc     $0300,x
        rts

L9A4F:  stx     L0000
        lda     #$02
        sta     $01
L9A55:  jsr     LFC53
        bcs     L9AA1
        ldx     $01
        lda     L9AAC,x
        sta     $0400,y
        lda     L9AAF,x
        sta     $0420,y
        lda     L9AB2,x
        sta     $0440,y
        lda     L9AB5,x
        sta     $0460,y
        lda     L9AB8,x
        sta     $04A0,y
        lda     #$73
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        ldx     L0000
        lda     #$0F
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        dec     $01
        bpl     L9A55
L9AA1:  ldx     L0000
        inc     $0300,x
        lda     #$3C
        .byte   $9D,$00,$05,$60
L9AAC:  .byte   $DB,$00,$DB
L9AAF:  .byte   $00,$00,$00
L9AB2:  .byte   $DB,$33,$DB
L9AB5:  .byte   $00,$01
        brk
L9AB8:  .byte   $02
        ora     ($01,x)
        lda     $04A0,x
        and     #$01
        beq     L9AC8
        jsr     LF71D
        jmp     L9ACB

L9AC8:  jsr     LF73B
L9ACB:  jsr     LF8B3
        bcc     L9B2B
        jsr     LF8C2
        cmp     #$10
        bcs     L9B2B
        lda     $30
        cmp     #$02
        bcs     L9B08
        lda     #$05
        sta     $30
        stx     $34
        lda     #$07
        sta     $05C0
        lda     #$00
        sta     $05E0
        sta     $05A0
        sta     $32
        lda     $03C0
        sta     $0500,x
        lda     $0460
        bpl     L9B43
        lda     #$55
        sta     $0440
        lda     #$00
        sta     $0460
        rts

L9B08:  lda     $30
        cmp     #$05
        bne     L9B43
        cpx     $34
        bne     L9B43
        lda     $0500,x
        sec
        sbc     $03C0
        cmp     #$20
        bcc     L9B43
        lda     #$00
        sta     $0440
        sta     $0460
        lda     $04A0,x
        sta     $35
        rts

L9B2B:  lda     $30
        cmp     #$05
        bne     L9B43
        cpx     $34
        bne     L9B43
        lda     #$AB
        sta     $0440
        lda     #$FF
        sta     $0460
        lda     #$00
        sta     $30
L9B43:  rts

        lda     $0300,x
        and     #$0F
        bne     L9B58
        jsr     LF8C2
        cmp     #$76
        bcs     L9B43
        inc     $0300,x
        jsr     LF883
L9B58:  lda     $0580,x
        and     #$04
        beq     L9B67
        lda     $0580,x
        eor     #$04
        sta     $0580,x
L9B67:  lda     $0300,x
        and     #$02
        bne     L9B78
        ldy     #$24
        jsr     LF67C
        bcc     L9BE1
        inc     $0300,x
L9B78:  lda     $04A0,x
        pha
        jsr     LF869
        pla
        cmp     $04A0,x
        beq     L9B8D
        lda     $0580,x
        eor     #$40
        sta     $0580,x
L9B8D:  lda     $0520,x
        bne     L9BA6
        jsr     L9BE2
        sty     L0000
        lda     L0000
        sta     $0500,x
        lda     #$78
        sta     $0520,x
        lda     #$00
        sta     $0540,x
L9BA6:  lda     $0540,x
        bne     L9BD1
        lda     $0500,x
        tay
        lda     $03C0,y
        sec
        sbc     $03C0,x
        bcs     L9BBD
        eor     #$FF
        adc     #$01
        clc
L9BBD:  cmp     #$30
        bcs     L9BE1
        lda     $05C0,x
        cmp     #$39
        beq     L9BD1
        lda     #$39
        jsr     LF835
        inc     $0540,x
        rts

L9BD1:  dec     $0520,x
        lda     $05E0,x
        ora     $05A0,x
        bne     L9BE1
        lda     #$38
        jsr     LF835
L9BE1:  rts

L9BE2:  jsr     LFC53
        bcs     L9C18
        lda     $04A0,x
        sta     $04A0,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     #$04
        sta     $03C0,y
        lda     $03C0,x
        sta     $0500,y
        lda     #$94
        jsr     LF846
        lda     #$CA
        sta     $0480,y
        lda     #$24
        sta     $0320,y
        lda     #$08
        sta     $04E0,y
L9C18:  rts

        lda     $0300,x
        and     #$0F
        bne     L9C2B
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        inc     $0300,x
L9C2B:  jsr     LF759
        lda     $03C0,x
        sec
        sbc     $0500,x
        bcs     L9C3C
        eor     #$FF
        adc     #$01
        clc
L9C3C:  cmp     #$20
        bcs     L9C55
        lda     #$80
        sta     $02
        lda     #$04
        sta     $03
        jsr     LFC63
        lda     $0C
        sta     $04A0,x
        lda     #$0B
        sta     $0320,x
L9C55:  rts

        lda     $0300,x
        and     #$0F
        bne     L9C73
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        jsr     L9D20
        sta     $0500,x
        lda     #$1E
        sta     $0520,x
        inc     $0300,x
L9C73:  lda     $03C0,x
        pha
        lda     $03C0,x
        sec
        sbc     #$17
        sta     $03C0,x
        lda     #$C3
        sta     $0480,x
        jsr     L8003
        pla
        sta     $03C0,x
        lda     $04E0,x
        beq     L9CA5
        lda     #$AC
        sta     $0480,x
        lda     $0300,x
        and     #$02
        bne     L9CD6
        dec     $0500,x
        bne     L9CA6
        inc     $0300,x
L9CA5:  rts

L9CA6:  ldy     #$2A
        jsr     LF606
        lda     $04A0,x
        and     #$01
        beq     L9CC0
        lda     $42
        and     #$10
        beq     L9CCD
        ldy     #$10
        jsr     LF580
        jmp     L9CCB

L9CC0:  lda     $44
        and     #$10
        beq     L9CCD
        ldy     #$11
        jsr     LF5C4
L9CCB:  bcc     L9CD5
L9CCD:  lda     $04A0,x
        eor     #$03
        sta     $04A0,x
L9CD5:  rts

L9CD6:  dec     $0520,x
        bne     L9CF0
        sta     $0540,x
        sta     $0560,x
        lda     #$1E
        sta     $0520,x
        jsr     L9D20
        sta     $0500,x
        dec     $0300,x
        rts

L9CF0:  lda     $0540,x
        bne     L9D18
        lda     $0560,x
        and     #$01
        asl     a
        tay
        lda     $0360,x
        clc
        adc     L9D1C,y
        sta     $0360,x
        lda     $0380,x
        adc     L9D1D,y
        sta     $0380,x
        lda     #$02
        sta     $0540,x
        inc     $0560,x
        rts

L9D18:  .byte   $DE,$40,$05,$60
L9D1C:  .byte   $01
L9D1D:  brk
        .byte   $FF
        .byte   $FF
L9D20:  lda     $E4
        adc     $E5
        sta     $E4
        and     #$03
        tay
        .byte   $B9,$2D,$9D,$60
        bpl     L9D4F
        bmi     L9D41
        jsr     LF883
        ldy     #$10
        jsr     LF67C
        bcs     L9D51
        lda     #$00
        sta     $05E0,x
        .byte   $BD
L9D41:  ldy     #$04
        and     #$01
        beq     L9D4C
        ldy     #$0E
        jmp     LF580

L9D4C:  ldy     #$0F
        .byte   $4C
L9D4F:  cpy     $F5
L9D51:  lda     $05A0,x
        cmp     #$08
        bne     L9D7E
        lda     $05E0,x
        beq     L9D6B
        lda     #$00
        sta     $05A0,x
        sta     $05E0,x
        lda     #$20
        jsr     LF89A
        rts

L9D6B:  lda     #$A8
        sta     $0440,x
        lda     #$05
        sta     $0460,x
        jsr     LF869
        lda     #$C5
        sta     $0480,x
        rts

L9D7E:  lda     #$A5
        sta     $0480,x
        rts

        jsr     LF8B3
        cmp     #$1C
        bcs     L9DB3
        jsr     LF8C2
        ror     L0000
        cmp     #$68
        bcs     L9DB3
        lda     $0580,x
        and     #$40
        bne     L9DA3
        lda     L0000
        bmi     L9DB3
        lda     #$01
        bne     L9DA9
L9DA3:  lda     L0000
        bpl     L9DB3
        lda     #$02
L9DA9:  sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
L9DB3:  rts

        lda     $0300,x
        and     #$0F
        bne     L9DC3
        lda     #$1E
        sta     $0500,x
        inc     $0300,x
L9DC3:  lda     $0300,x
        and     #$02
        bne     L9E04
        jsr     LF8C2
        cmp     #$50
        bcs     L9E0C
        lda     $0540,x
        bne     L9DE7
        lda     #$5A
        jsr     LF835
        jsr     LF869
        jsr     L9EA9
        lda     #$1E
        sta     $0540,x
        rts

L9DE7:  dec     $0540,x
        bne     L9E0C
        inc     $0560,x
        lda     $0560,x
        cmp     #$02
        bcc     L9E0C
        lda     #$00
        sta     $0560,x
        lda     #$78
        sta     $0540,x
        inc     $0300,x
        rts

L9E04:  dec     $0540,x
        bne     L9E0C
        dec     $0300,x
L9E0C:  dec     $0500,x
        bne     L9E31
        lda     #$00
        sta     $01
        jsr     L9E46
        lda     #$1E
        sta     $0500,x
        inc     $0520,x
        lda     $0520,x
        cmp     #$03
        bcc     L9E31
        lda     #$5A
        sta     $0500,x
        lda     #$00
        sta     $0520,x
L9E31:  lda     $05C0,x
        cmp     #$5A
        bne     L9E45
        lda     $05E0,x
        ora     $05A0,x
        bne     L9E45
        lda     #$59
        jsr     LF835
L9E45:  rts

L9E46:  jsr     LFC53
        bcs     L9EA4
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     L9EA5,y
        pha
        lda     $0380,x
        adc     L9EA6,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sec
        sbc     #$04
        sta     $03C0,y
        lda     #$80
        sta     $0400,y
        lda     #$01
        sta     $0420,y
        lda     #$73
        jsr     LF846
        lda     #$1B
        sta     $0320,y
        lda     #$8B
        sta     $0480,y
        lda     #$00
        sta     $04E0,y
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        inc     $01
        lda     $01
        cmp     #$02
        .byte   $90,$A2
L9EA4:  .byte   $60
L9EA5:  .byte   $0F
L9EA6:  brk
        sbc     ($FF),y
L9EA9:  jsr     LFC53
        bcs     L9EA4
        lda     #$00
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        lda     #$73
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        lda     #$0C
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sec
        sbc     #$10
        sta     $03C0,y
        lda     $03E0,x
        sta     $03E0,y
        lda     $04A0,x
        sta     $04A0,y
        jsr     LF8C2
        stx     L0000
        ldx     #$03
L9EEF:  cmp     L9F06,x
        bcc     L9EF7
        dex
        bne     L9EEF
L9EF7:  lda     L9F0A,x
        sta     $0400,y
        lda     L9F0E,x
        sta     $0420,y
        .byte   $A6,$00,$60
L9F06:  .byte   $4C,$3D,$2E,$1F
L9F0A:  .byte   $00,$80,$00,$80
L9F0E:  .byte   $02
        ora     ($01,x)
        brk
        ldy     #$12
        jsr     LF67C
        bcs     L9F30
        lda     $04A0,x
        and     #$01
        beq     L9F28
        ldy     #$1E
        jsr     LF580
        jmp     L9F2D

L9F28:  ldy     #$1F
        jsr     LF5C4
L9F2D:  bcs     L9F30
        rts

L9F30:  lda     $0320,x
        cmp     #$0C
        bne     L9F6C
        lda     #$00
        sta     $0320,x
        lda     $B3
        bpl     L9F44
        lda     #$59
        bne     L9F46
L9F44:  lda     #$71
L9F46:  jmp     LF835

        lda     $04A0,x
        and     #$01
        beq     L9F58
        ldy     #$0C
        jsr     LF580
        jmp     L9F5D

L9F58:  ldy     #$0D
        jsr     LF5C4
L9F5D:  bcs     L9F6C
        lda     $04A0,x
        and     #$08
        beq     L9F69
        jmp     LF779

L9F69:  jmp     LF759

L9F6C:  lda     #$00
        sta     $0300,x
        sta     $01
        lda     #$FF
        sta     $04C0,x
        jsr     LFC53
        bcs     L9FE2
        sty     L0000
        lda     $04A0,x
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA731,y
        pha
        lda     $0380,x
        adc     LA732,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$5B
        jsr     LF846
        lda     #$0C
        sta     $0320,y
        lda     #$8B
        sta     $0480,y
        lda     #$00
        sta     $04E0,y
        stx     $02
        ldx     $01
        lda     L9FE3,x
        sta     $04A0,y
        lda     L9FE7,x
        sta     $0440,y
        lda     L9FEB,x
        sta     $0460,y
        lda     L9FEF,x
        sta     $0400,y
        lda     L9FF3,x
        sta     $0420,y
        ldx     $02
        inc     $01
        lda     $01
        cmp     #$04
        .byte   $90,$96
L9FE2:  .byte   $60
L9FE3:  .byte   $01,$01,$02,$02
L9FE7:  .byte   $9E,$44,$9E,$44
L9FEB:  .byte   $04,$03,$04,$03
L9FEF:  .byte   $CC,$80,$CC,$80
L9FF3:  brk
        brk
        brk
        brk
        jsr     LA249
        lda     $0560,x
        beq     L9FE2
        .byte   $BD

.segment "BANK1D"

LA000:  .byte   $E0,$04
        bne     LA007
        jmp     LA180

LA007:  lda     $0300,x
        and     #$0F
        bne     LA032
        sta     $05E0,x
        lda     $03C0,x
        cmp     #$90
        bcs     LA01B
        jmp     LF797

LA01B:  ldy     #$00
        jsr     LF67C
        bcc     LA05F
        lda     $0320,x
        sec
        sbc     #$52
        tay
        lda     LA176,y
        sta     $0500,x
        inc     $0300,x
LA032:  lda     $05C0,x
        cmp     #$99
        bne     LA04A
        lda     $05A0,x
        cmp     #$04
        bne     LA05F
        lda     $0500,x
        tya
        lda     LA178,y
        jsr     LF835
LA04A:  lda     $0300,x
        and     #$02
        beq     LA054
        jmp     LA0D3

LA054:  jsr     LF8C2
        cmp     #$60
        bcs     LA05F
        inc     $0300,x
        rts

LA05F:  ldy     #$00
        jsr     LF67C
        rol     $0F
        lda     $04A0,x
        and     #$01
        beq     LA075
        ldy     #$00
        jsr     LF580
        jmp     LA07A

LA075:  ldy     #$01
        jsr     LF5C4
LA07A:  lda     $0F
        and     #$01
        beq     LA0D2
        lda     $04A0,x
        and     #$01
        beq     LA091
        lda     $0360,x
        cmp     #$D6
        bcc     LA0A1
        jmp     LA098

LA091:  lda     $0360,x
        cmp     #$2A
        bcs     LA0A1
LA098:  lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        rts

LA0A1:  lda     $10
        and     #$10
        beq     LA0BC
        lda     $0500,x
        tya
        lda     LA17A,y
        jsr     LF835
        lda     #$A8
        sta     $0440,x
        lda     #$05
        sta     $0460,x
        rts

LA0BC:  lda     $0500,x
        tay
        lda     LA17C,y
        cmp     $05C0,x
        beq     LA0D2
        lda     $0500,x
        tay
        lda     LA17C,y
        jsr     LF835
LA0D2:  rts

LA0D3:  lda     $0540,x
        bne     LA13A
        lda     $04A0,x
        and     #$01
        beq     LA0E7
        ldy     #$00
        jsr     LF580
        jmp     LA0EC

LA0E7:  ldy     #$01
        jsr     LF5C4
LA0EC:  lda     $04A0,x
        and     #$01
        beq     LA0FD
        lda     $0360,x
        cmp     #$D6
        bcc     LA10C
        jmp     LA104

LA0FD:  .byte   $BD
        rts

LA0FF:  .byte   $03
        cmp     #$2A
        bcs     LA10C
LA104:  lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LA10C:  ldy     #$00
        jsr     LF67C
        bcc     LA13E
        lda     #$04
        sta     $0540,x
        lda     $0500,x
        tay
        lda     LA17C,y
        jsr     LF835
        lda     #$A8
        sta     $0440,x
        lda     #$05
        sta     $0460,x
        jsr     LF8C2
        cmp     #$60
        bcc     LA139
        dec     $0300,x
        jsr     LF81B
LA139:  rts

LA13A:  dec     $0540,x
        rts

LA13E:  lda     $0460,x
        bpl     LA14D
        lda     $0500,x
        tay
        lda     LA17A,y
        jmp     LF835

LA14D:  lda     $0500,x
        tya
        lda     LA17E,y
        cmp     $05C0,x
        beq     LA163
        lda     $0500,x
        tya
        lda     LA17E,y
        jsr     LF835
LA163:  lda     $05A0,x
        cmp     #$01
        bne     LA139
        jsr     LA293
        lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        rts

LA176:  .byte   $00,$01
LA178:  .byte   $88,$8A
LA17A:  .byte   $86,$8F
LA17C:  .byte   $83,$8C
LA17E:  .byte   $85,$8E
LA180:  lda     #$99
        cmp     $05C0,x
        beq     LA1A2
        lda     #$00
        sta     $0480,x
        tay
        jsr     LF67C
        bcc     LA1E1
        lda     #$99
        jsr     LF835
        inc     $05A0,x
        lda     #$00
        sta     $0440,x
        sta     $0460,x
LA1A2:  lda     $05A0,x
        bne     LA1E1
        sta     $05E0,x
        lda     $0440,x
        clc
        adc     $99
        sta     $0440,x
        lda     $0460,x
        adc     #$00
        sta     $0460,x
        cmp     #$0C
        bne     LA1C4
        lda     #$00
        sta     $0440,x
LA1C4:  jsr     LF779
        lda     $03E0,x
        beq     LA1E1
        lda     #$00
        sta     $0300,x
        lda     $0320,x
        cmp     #$53
        bne     LA1DD
        lda     #$13
        sta     $30
        rts

LA1DD:  lda     #$80
        sta     $68
LA1E1:  rts

        jsr     LA249
        lda     $0560,x
        beq     LA1E1
        lda     #$0F
        sta     $30
        lda     $0300,x
        and     #$0F
        bne     LA216
        jsr     LF797
        lda     #$9C
        cmp     $03C0,x
        bcs     LA243
        sta     $03C0,x
        lda     $05A0,x
        cmp     #$04
        bne     LA248
        lda     #$88
        jsr     LF835
        inc     $0300,x
        lda     #$FF
        sta     $0500,x
LA216:  lda     $0500,x
        beq     LA230
        dec     $0500,x
        bne     LA243
        lda     #$99
        sta     $05C0,x
        inc     $05A0,x
        lda     #$00
        sta     $0440,x
        sta     $0460,x
LA230:  jsr     LA1A2
        lda     $68
        beq     LA248
        lda     #$00
        sta     $30
        ldy     #$0F
LA23D:  sta     $0310,y
        dey
        bpl     LA23D
LA243:  lda     #$00
        sta     $05E0,x
LA248:  rts

LA249:  lda     $0560,x
        bne     LA292
        lda     $30
        bne     LA25D
        lda     $05C0
        cmp     #$13
        beq     LA292
        lda     #$0F
        sta     $30
LA25D:  lda     #$11
        cmp     $D9
        beq     LA26B
        jsr     LF898
        lda     #$B4
        sta     $0500,x
LA26B:  dec     $0500,x
        bne     LA292
        lda     #$00
        sta     $30
        inc     $0560,x
        lda     $0580,x
        and     #$FB
        sta     $0580,x
        lda     $0320,x
        cmp     #$53
        bne     LA28A
        lda     #$0C
        bne     LA28F
LA28A:  lda     $22
        clc
        adc     #$01
LA28F:  jsr     LF898
LA292:  rts

LA293:  jsr     LFC53
        bcs     LA2E7
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA2E8,y
        pha
        lda     $0380,x
        adc     LA2E9,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        sta     $0400,y
        lda     #$04
        sta     $0420,y
        lda     $0320,x
        and     #$01
        bne     LA2D8
        lda     #$18
        bne     LA2DA
LA2D8:  lda     #$73
LA2DA:  jsr     LF846
        lda     #$8B
        sta     $0480,y
        lda     #$1B
        sta     $0320,y
LA2E7:  rts

LA2E8:  .byte   $0D
LA2E9:  .byte   $00,$F3,$FF
        lda     $0300,x
        and     #$0F
        bne     LA2FE
        jsr     LF883
        lda     #$3C
        sta     $0560,x
        inc     $0300,x
LA2FE:  lda     $0300,x
        and     #$0F
        cmp     #$02
        beq     LA321
        cmp     #$03
        beq     LA389
        dec     $0560,x
        bne     LA318
        lda     #$00
        sta     $0560,x
        inc     $0300,x
LA318:  lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        rts

LA321:  lda     $0500,x
        bne     LA36B
        lda     $05A0,x
        cmp     #$01
        bne     LA33D
        lda     $05E0,x
        cmp     #$01
        bne     LA33D
        lda     #$04
        sta     $01
        stx     L0000
        jsr     LA41A
LA33D:  lda     $05A0,x
        cmp     #$03
        bne     LA354
        lda     $05E0,x
        cmp     #$01
        bne     LA354
        lda     #$04
        sta     $01
        stx     L0000
        jsr     LA41A
LA354:  lda     $05A0,x
        cmp     #$04
        bne     LA36A
        lda     $05E0,x
        cmp     #$02
        bne     LA36A
        inc     $0500,x
        lda     #$10
        sta     $0520,x
LA36A:  rts

LA36B:  lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        dec     $0520,x
        bne     LA388
        inc     $0300,x
        lda     #$77
        jsr     LF835
        jsr     LF869
        lda     #$5A
        sta     $0540,x
LA388:  rts

LA389:  lda     $05C0,x
        cmp     #$76
        beq     LA3FA
        ldy     #$0E
        jsr     LF67C
        bcc     LA3BA
        lda     #$AA
        sta     $0480,x
        lda     $04A0,x
        and     #$01
        beq     LA3AB
        ldy     #$1C
        jsr     LF580
        jmp     LA3B0

LA3AB:  ldy     #$1D
        jsr     LF5C4
LA3B0:  bcc     LA3BA
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LA3BA:  lda     $0560,x
        bne     LA3D7
        dec     $0540,x
        bne     LA3D6
        lda     #$C6
        sta     $0480,x
        lda     #$76
        jsr     LF835
        lda     #$FF
        sta     $0540,x
        inc     $0560,x
LA3D6:  rts

LA3D7:  dec     $0540,x
        bne     LA3D6
        lda     #$00
        sta     $0500,x
        sta     $0520,x
        sta     $0540,x
        sta     $0560,x
        lda     #$C6
        sta     $0480,x
        lda     #$76
        jsr     LF835
        lda     #$82
        sta     $0300,x
        rts

LA3FA:  lda     $05A0,x
        cmp     #$01
        bne     LA419
        lda     $05E0,x
        cmp     #$02
        bne     LA419
        lda     #$04
        sta     $01
        stx     L0000
        jsr     LA41A
        lda     #$10
        sta     $0520,x
        dec     $0300,x
LA419:  rts

LA41A:  jsr     LFC53
        bcs     LA472
        ldx     $01
        lda     LA475,x
        sta     $0400,y
        lda     LA47A,x
        sta     $0420,y
        lda     LA47F,x
        sta     $0440,y
        lda     LA484,x
        sta     $0460,y
        lda     LA489,x
        sta     $04A0,y
        lda     LA48E,x
        jsr     LF846
        lda     LA493,x
        sta     $0580,y
        ldx     L0000
        lda     #$CB
        sta     $0480,y
        lda     #$36
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$01
        sta     $04E0,y
        dec     $01
        bpl     LA41A
LA472:  ldx     L0000
        rts

LA475:  .byte   $00,$1F,$00,$00,$1F
LA47A:  .byte   $00,$02,$03,$03,$02
LA47F:  .byte   $00,$E1,$00,$00,$E1
LA484:  .byte   $FD,$FD,$00,$00,$FD
LA489:  .byte   $02,$02,$02,$01,$01
LA48E:  .byte   $40,$79,$78,$78,$79
LA493:  .byte   $90,$90,$90,$D0,$D0
        lda     $0300,x
        and     #$0F
        bne     LA4BA
        lda     $04A0,x
        and     #$01
        beq     LA4AC
        jsr     LF71D
        jmp     LA4AF

LA4AC:  jsr     LF73B
LA4AF:  jsr     LF8C2
        cmp     #$40
        bcs     LA4B9
        inc     $0300,x
LA4B9:  rts

LA4BA:  lda     $0300,x
        and     #$02
        beq     LA4C4
        jmp     LA561

LA4C4:  lda     $0500,x
        bne     LA51A
        ldy     $0520,x
        lda     LA56E,y
        asl     a
        tay
        lda     LA57C,y
        sta     $0440,x
        lda     LA57D,y
        sta     $0460,x
        lda     LA59C,y
        sta     $0400,x
        lda     LA59D,y
        sta     $0420,x
        lda     $0420,x
        bpl     LA503
        lda     $0400,x
        eor     #$FF
        clc
        adc     #$01
        sta     $0400,x
        lda     $0420,x
        eor     #$FF
        adc     #$00
        sta     $0420,x
LA503:  inc     $0520,x
        lda     $0520,x
        cmp     #$06
        bne     LA515
        inc     $0300,x
        lda     #$1A
        sta     $0560,x
LA515:  lda     #$0D
        sta     $0500,x
LA51A:  dec     $0500,x
        lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $04A0,x
        and     #$02
        bne     LA53E
        jsr     LF71D
        bcs     LA541
        bcc     LA541
LA53E:  jsr     LF73B
LA541:  lda     $0560,x
        bne     LA55D
        jsr     LA5BC
        inc     $0540,x
        lda     $0540,x
        cmp     #$02
        bne     LA557
        lda     #$10
        bne     LA559
LA557:  lda     #$1A
LA559:  sta     $0560,x
        rts

LA55D:  dec     $0560,x
        rts

LA561:  lda     #$00
        sta     $0440,x
        lda     #$03
        sta     $0460,x
        jmp     LF779

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
LA5BC:  jsr     LFC53
        bcs     LA5FD
        sty     L0000
        lda     $04A0,x
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA731,y
        pha
        lda     $0380,x
        adc     LA732,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        clc
        adc     #$10
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        lda     #$81
        jsr     LF846
        lda     #$26
        sta     $0320,y
        lda     #$93
        sta     $0480,y
LA5FD:  rts

        lda     $0300,x
        and     #$0F
        bne     LA60B
        jsr     LF81B
        inc     $0300,x
LA60B:  lda     $0300,x
        and     #$02
        bne     LA627
        ldy     #$12
        jsr     LF67C
        bcc     LA626
        lda     #$71
        jsr     LF835
        lda     #$24
        jsr     LF89A
        inc     $0300,x
LA626:  rts

LA627:  lda     $05C0,x
        cmp     #$71
        bne     LA626
        lda     $05A0,x
        cmp     #$04
        bne     LA626
        lda     #$80
        jmp     LF835

        lda     $05C0,x
        cmp     #$71
        bne     LA668
        lda     $05A0,x
        cmp     #$04
        bne     LA667
        lda     #$92
        jsr     LF835
        lda     #$40
        sta     $0400,x
        sta     $0440,x
        lda     #$00
        sta     $0420,x
        sta     $0460,x
        lda     #$C0
        sta     $0480,x
        lda     #$01
        sta     $04E0,x
LA667:  rts

LA668:  jmp     LABEA

        lda     $0300,x
        and     #$0F
        bne     LA67D
        sta     $0460,x
        lda     #$80
        sta     $0440,x
        inc     $0300,x
LA67D:  lda     $0300,x
        and     #$02
        bne     LA6BD
        lda     $04A0,x
        and     #$01
        beq     LA693
        ldy     #$14
        jsr     LF580
        jmp     LA698

LA693:  ldy     #$15
        jsr     LF5C4
LA698:  bcc     LA6A0
        inc     $0540,x
LA69D:  jmp     LF759

LA6A0:  lda     $0540,x
        bne     LA69D
        lda     $0520,x
        bne     LA6BC
        jsr     LF8C2
        cmp     #$08
        bcs     LA6BC
        inc     $0300,x
        inc     $0520,x
        lda     #$33
        jsr     LF835
LA6BC:  rts

LA6BD:  lda     $05C0,x
        cmp     #$33
        bne     LA6BC
        lda     $05E0,x
        ora     $05A0,x
        bne     LA6BC
        lda     #$32
        jsr     LF835
        lda     #$00
        sta     $0400,x
        lda     #$04
        sta     $0420,x
        dec     $0300,x
        jsr     LFC53
        bcs     LA730
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA731,y
        pha
        lda     $0380,x
        adc     LA732,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sec
        sbc     #$10
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        lda     #$34
        jsr     LF846
        lda     $0320,x
        cmp     #$28
        beq     LA726
        lda     #$45
        sta     $0320,y
        bne     LA72B
LA726:  lda     #$29
        sta     $0320,y
LA72B:  lda     #$C0
        sta     $0480,y
LA730:  rts

LA731:  .byte   $00
LA732:  .byte   $00,$00,$00
        lda     $0300,x
        and     #$0F
        bne     LA74A
        sta     $0500,x
        sta     $0440,x
        lda     #$02
        sta     $0460,x
        inc     $0300,x
LA74A:  lda     $0300,x
        and     #$02
        bne     LA76B
        ldy     #$17
        jsr     LF642
        bcc     LA76B
        inc     $0300,x
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$56
        sta     $0320,x
        rts

LA76B:  lda     $0500,x
        bne     LA7CA
        lda     $0320,x
        cmp     #$29
        beq     LA780
        lda     $03C0,x
        cmp     #$62
        bcs     LA7CA
        bcc     LA787
LA780:  lda     $03C0,x
        cmp     #$B4
        bcs     LA7CA
LA787:  inc     $0500,x
        jsr     LFC53
        bcs     LA7CA
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA731,y
        pha
        lda     $0380,x
        adc     LA732,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sec
        sbc     #$04
        sta     $03C0,y
        lda     #$00
        sta     $0320,y
        sta     $0480,y
        sta     $04E0,y
        lda     #$68
        jsr     LF846
LA7CA:  rts

        lda     $0300,x
        and     #$0F
        bne     LA7EB
        ldy     #$02
LA7D4:  lda     LA8BD,y
        sta     $060D,y
        sta     $062D,y
        dey
        bpl     LA7D4
        sty     $18
        inc     $0300,x
        jsr     LA82E
        sta     $0500,x
LA7EB:  lda     $03C0,x
        pha
        sec
        sbc     #$10
        sta     $03C0,x
        lda     #$00
        sta     $0480,x
        lda     $05C0,x
        pha
        jsr     L8003
        pla
        sta     $05C0,x
        pla
        sta     $03C0,x
        lda     $04E0,x
        bne     LA81A
        sta     $0520,x
        sta     $0540,x
        lda     #$63
        sta     $0320,x
        rts

LA81A:  lda     #$98
        sta     $0480,x
        dec     $0500,x
        bne     LA82D
        jsr     LA8EC
        jsr     LA82E
        sta     $0500,x
LA82D:  rts

LA82E:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     LA83B,y
        rts

LA83B:  .byte   $3C,$1E,$78,$3C
        lda     $0520,x
        bne     LA8B8
        lda     #$0A
        sta     $0520,x
        jsr     LFC53
        bcs     LA8BB
        lda     #$27
        jsr     LF89A
        lda     #$71
        jsr     LF846
        lda     #$19
        sta     $0320,y
        lda     #$00
        sta     $0480,y
        sta     $0500,y
        lda     $0380,x
        sta     $0380,y
        stx     L0000
        lda     $03C0,x
        sta     $01
        lda     $0360,x
        sta     $02
        lda     $0540,x
        pha
        tax
        lda     $01
        clc
        adc     LA8DC,x
        sta     $03C0,y
        lda     $02
        clc
        adc     LA8E4,x
        sta     $0360,y
        pla
        asl     a
        asl     a
        tax
        ldy     #$00
LA894:  lda     LA8BC,x
        sta     $060C,y
        sta     $062C,y
        inx
        iny
        cpy     #$04
        bne     LA894
        lda     #$FF
        sta     $18
        ldx     L0000
        inc     $0540,x
        lda     $0540,x
        and     #$03
        bne     LA8BB
        lda     #$00
        sta     $0300,x
LA8B8:  dec     $0520,x
LA8BB:  rts

LA8BC:  .byte   $0F
LA8BD:  .byte   $20,$10,$17,$0F,$10,$00,$17,$0F
        .byte   $00,$0F,$07,$0F,$0F,$0F,$0F,$0F
        .byte   $20,$37,$17,$0F,$10,$27,$07,$0F
        .byte   $0F,$17,$0F,$0F,$0F,$0F,$0F
LA8DC:  .byte   $F0,$10,$10,$D0,$F0,$10,$10,$D0
LA8E4:  .byte   $F0,$20,$E8,$10,$F0,$20,$E8,$10
LA8EC:  jsr     LFC53
        bcs     LA93A
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA93B,y
        pha
        lda     $0380,x
        adc     LA93C,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        clc
        adc     #$1C
        sta     $03C0,y
        lda     #$80
        sta     $0400,y
        lda     #$02
        sta     $0420,y
        lda     #$93
        jsr     LF846
        lda     #$46
        sta     $0320,y
        lda     #$C0
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
LA93A:  rts

LA93B:  .byte   $F8
LA93C:  .byte   $FF,$F8,$FF
        lda     $04A0,x
        and     #$01
        beq     LA94E
        lda     #$08
        jsr     LF580
        jmp     LA953

LA94E:  ldy     #$09
        jsr     LF5C4
LA953:  bcc     LA95F
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0320,x
LA95F:  rts

        lda     $05E0,x
        cmp     #$02
        bne     LA95F
        lda     $0500,x
        cmp     #$08
        beq     LA95F
        stx     L0000
        ldy     $0360,x
        sty     $02
        ldy     $0380,x
        sty     $03
        ldy     $03C0,x
        sty     $04
        ldy     $05C0,x
        sty     $10
        lda     $0320,x
        sta     $11
        lda     $0500,x
        tax
LA98D:  jsr     LFC53
        bcs     LA9F7
        lda     $10
        jsr     LF846
        lda     $11
        cmp     #$56
        beq     LA9B3
        cmp     #$48
        beq     LA9B3
        cmp     #$39
        beq     LA9BF
        cmp     #$48
        beq     LA9BF
        lda     #$00
        sta     $0480,y
        sta     $0320,y
        beq     LA9C9
LA9B3:  lda     #$80
        sta     $0480,y
        lda     #$54
        sta     $0320,y
        bne     LA9C9
LA9BF:  lda     #$80
        sta     $0480,y
        lda     #$55
        sta     $0320,y
LA9C9:  lda     #$00
        sta     $01
        lda     LA9FA,x
        bpl     LA9D4
        dec     $01
LA9D4:  clc
        adc     $02
        sta     $0360,y
        lda     $03
        adc     $01
        sta     $0380,y
        lda     $04
        clc
        adc     LAA02,x
        sta     $03C0,y
        ldx     L0000
        inc     $0500,x
        lda     $0500,x
        tax
        and     #$01
        bne     LA98D
LA9F7:  ldx     L0000
        rts

LA9FA:  .byte   $0C,$F4,$F0,$10,$F4,$0C,$00,$00
LAA02:  .byte   $F4,$0C,$00,$00,$F4,$0C,$F0,$10
        lda     $0300,x
        and     #$0F
        bne     LAA2C
        lda     #$44
        sta     $0440,x
        lda     #$03
        sta     $0460,x
        lda     #$1E
        sta     $0500,x
        jsr     LF883
        jsr     LA82E
        sta     $0520,x
        inc     $0300,x
LAA2C:  dec     $0520,x
        bne     LAA3A
        jsr     LAA9A
        jsr     LA82E
        sta     $0520,x
LAA3A:  lda     $0300,x
        and     #$02
        bne     LAA72
        lda     $04A0,x
        and     #$01
        beq     LAA50
        ldy     #$0A
        jsr     LF580
        jmp     LAA55

LAA50:  ldy     #$0B
        jsr     LF5C4
LAA55:  ldy     #$20
        jsr     LF67C
        bcc     LAA6D
        lda     #$44
        sta     $0440,x
        lda     #$03
        sta     $0460,x
        inc     $0300,x
        lda     #$3D
        bne     LAA6F
LAA6D:  lda     #$3C
LAA6F:  jmp     LF835

LAA72:  lda     $05C0,x
        cmp     #$3B
        beq     LAA86
        lda     $05E0,x
        ora     $05A0,x
        bne     LAA99
        lda     #$3B
        jsr     LF835
LAA86:  dec     $0500,x
        bne     LAA99
        jsr     LF869
        jsr     LF883
        dec     $0300,x
        lda     #$3C
        sta     $0500,x
LAA99:  rts

LAA9A:  jsr     LFC53
        bcs     LAADB
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LAADC,y
        pha
        lda     $0380,x
        adc     LAADD,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$01
        sta     $04E0,y
        lda     #$3E
        jsr     LF846
        lda     #$1D
        sta     $0320,y
        lda     #$C0
        sta     $0480,y
LAADB:  rts

LAADC:  .byte   $08
LAADD:  .byte   $00,$F8,$FF
        lda     $0300,x
        and     #$0F
        bne     LAAF2
        sta     $0500,x
        lda     #$1E
        sta     $0520,x
        inc     $0300,x
LAAF2:  lda     $0300,x
        and     #$02
        bne     LAB3A
        lda     $04A0,x
        and     #$01
        beq     LAB06
        jsr     LF71D
        jmp     LAB09

LAB06:  jsr     LF73B
LAB09:  ldy     #$08
        jsr     LF67C
        bcc     LAB39
        lda     $0500,x
        tay
        lda     LAB4F,y
        sta     $0440,x
        lda     LAB52,y
        sta     $0460,x
        lda     LAB55,y
        sta     $0400,x
        lda     LAB58,y
        sta     $0420,x
        inc     $0500,x
        lda     $0500,x
        cmp     #$03
        bcc     LAB39
        inc     $0300,x
LAB39:  rts

LAB3A:  dec     $0520,x
        bne     LAB3A
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$39
        sta     $0320,x
        rts

LAB4F:  .byte   $9E,$44,$4F
LAB52:  .byte   $04,$03,$02
LAB55:  .byte   $00,$00,$80
LAB58:  .byte   $01,$01,$00
        lda     $05C0,x
        cmp     #$2F
        bne     LAB65
        jmp     LABEA

LAB65:  lda     $0300,x
        and     #$0F
        bne     LABC9
        jsr     LAC27
        bcs     LABE9
        lda     $0580,x
        and     #$FB
        sta     $0580,x
        inc     $0300,x
        lda     #$66
        sta     $0440,x
        lda     #$00
        sta     $0460,x
        lda     #$2E
        jsr     LF846
        lda     #$31
        sta     $0320,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $FC
        clc
        adc     #$04
        sta     $0360,y
        lda     $F9
        adc     #$00
        sta     $0380,y
        lda     #$00
        sta     $0400,y
        lda     #$04
        sta     $0420,y
        lda     $0360,x
        sec
        sbc     #$08
        sta     $0500,y
        lda     $0380,x
        sbc     #$00
        sta     $0540,y
        txa
        sta     $0520,y
        lda     #$00
        sta     $0480,y
LABC9:  lda     $0580,x
        and     #$FB
        sta     $0580,x
        lda     $0500,x
        bne     LABE9
        lda     #$00
        sta     $05E0,x
        lda     $92
        and     #$04
        beq     LABE9
        lda     $0580,x
        ora     #$04
        sta     $0580,x
LABE9:  rts

LABEA:  lda     $0360
        sec
        sbc     $0360,x
        pha
        lda     $0380
        sbc     $0380,x
        pla
        beq     LAC16
        bcc     LAC0B
        jsr     LF71D
        lda     $0580,x
        ora     #$40
        sta     $0580,x
        jmp     LAC16

LAC0B:  jsr     LF73B
        lda     $0580,x
        and     #$BF
        sta     $0580,x
LAC16:  lda     $03C0
        sec
        sbc     $03C0,x
        beq     LABE9
        bcs     LAC24
        jmp     LF779

LAC24:  jmp     LF759

LAC27:  jsr     LF8C2
        cmp     #$44
        bcs     LAC7E
        lda     $0360,x
        sec
        sbc     $FC
        sta     L0000
        lda     $0380,x
        sbc     $F9
        bcc     LAC79
        lda     L0000
        cmp     #$80
        bcc     LAC79
        lda     #$00
        sta     $01
        stx     L0000
        ldy     #$1F
LAC4B:  cpy     L0000
        beq     LAC62
        lda     $0300,y
        bpl     LAC62
        lda     $0580,y
        bpl     LAC62
        lda     $0320,y
        cmp     #$30
        bne     LAC62
        inc     $01
LAC62:  dey
        cpy     #$0F
        bne     LAC4B
        lda     $01
        cmp     #$03
        beq     LAC79
        ldy     L0000
LAC6F:  lda     $0300,y
        bpl     LAC80
        dey
        cpy     #$0F
        bne     LAC6F
LAC79:  lda     #$00
        sta     $0300,x
LAC7E:  sec
        rts

LAC80:  clc
        rts

        lda     $0300,x
        and     #$0F
        bne     LACBE
        sta     $0560,x
        jsr     LF71D
        lda     $0500,x
        sec
        sbc     $0360,x
        lda     $0540,x
        sbc     $0380,x
        bcs     LACB8
        lda     $0500,x
        sta     $0360,x
        lda     $0540,x
        sta     $0380,x
        inc     $0300,x
        ldy     $0520,x
        lda     #$08
        sta     $0500,y
        sta     $0500,x
LACB8:  lda     #$00
        sta     $05E0,x
        rts

LACBE:  lda     $0560,x
        bne     LACCB
        lda     #$25
        jsr     LF89A
        inc     $0560,x
LACCB:  lda     #$01
        sta     $95
        lda     $0500,x
        beq     LACE0
        lda     $05E0,x
        bne     LAD08
        dec     $0500,x
        inc     $0360,x
        rts

LACE0:  lda     $05A0,x
        bne     LAD08
        sta     $05E0,x
        ldy     $0520,x
        lda     $05A0,y
        bne     LAD08
        sta     $05E0,y
        sta     $0500,y
        sta     $0300,x
        lda     #$2F
        sta     $05C0,y
        lda     #$C0
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
LAD08:  rts

        lda     $0300,x
        and     #$0F
        bne     LAD4B
        jsr     LF8C2
        cmp     #$28
        bcs     LAD4A
        inc     $0300,x
        lda     #$1E
        sta     $0500,x
        lda     #$30
        sta     $0540,x
        lda     $0580,x
        eor     #$04
        sta     $0580,x
        lda     $04A0,x
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LADD5,y
        pha
        lda     $0380,x
        adc     LADD6,y
        sta     $0380,x
        pla
        sta     $0360,x
        jsr     LF869
LAD4A:  rts

LAD4B:  lda     $0300,x
        and     #$02
        bne     LAD9A
        jsr     LF883
        lda     $04A0,x
        and     #$01
        beq     LAD62
        jsr     LF71D
        jmp     LAD65

LAD62:  jsr     LF73B
LAD65:  lda     $0520,x
        bne     LAD99
        jsr     LF8C2
        cmp     #$50
        bcc     LAD99
        inc     $0300,x
        inc     $0520,x
        lda     #$80
        sta     $0440,x
        lda     #$00
        sta     $0460,x
        lda     $04A0,x
        and     #$01
        beq     LAD91
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        rts

LAD91:  lda     $0580,x
        ora     #$40
        sta     $0580,x
LAD99:  rts

LAD9A:  dec     $0540,x
        bne     LADB2
        dec     $0300,x
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$3A
        jsr     LF835
        jmp     LADD9

LADB2:  lda     $04A0,x
        and     #$01
        beq     LADBF
        jsr     LF779
        jmp     LADC2

LADBF:  jsr     LF759
LADC2:  dec     $0500,x
        bne     LAD99
        lda     #$3C
        sta     $0500,x
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        rts

LADD5:  .byte   $50
LADD6:  .byte   $00,$B0,$FF
LADD9:  jsr     LFC53
        bcs     LAE27
        sty     $01
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA731,y
        pha
        lda     $0380,x
        adc     LA732,y
        ldy     $01
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        clc
        adc     #$18
        sta     $03C0,y
        lda     #$35
        jsr     LF846
        lda     #$2F
        sta     $0320,y
        lda     #$CF
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
        lda     #$50
        sta     $0440,y
        lda     #$04
        sta     $0460,y
LAE27:  rts

        ldy     #$08
        jsr     LF606
        bcc     LAE27
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$3A
        sta     $0320,x
        lda     #$00
        sta     $01
LAE42:  jsr     LFC53
        bcs     LAEB3
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        lda     $01
        asl     a
        tay
        lda     $0360,x
        clc
        adc     LAEB4,y
        pha
        lda     $0380,x
        adc     LAEB5,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        ldy     $01
        lda     $03C0,x
        clc
        adc     LAEBE,y
        ldy     L0000
        sta     $03C0,y
        lda     #$41
        jsr     LF846
        lda     #$00
        sta     $0400,y
        sta     $0420,y
        lda     #$C0
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
        lda     #$3B
        sta     $0320,y
        lda     #$08
        sta     $0500,y
        sta     $0520,y
        lda     #$10
        sta     $0540,y
        lda     #$33
        sta     $0440,y
        lda     #$01
        sta     $0460,y
        inc     $01
        lda     $01
        cmp     #$05
        bcc     LAE42
LAEB3:  rts

LAEB4:  .byte   $E8
LAEB5:  .byte   $FF,$E8,$FF,$01,$00,$18,$00,$18
        .byte   $00
LAEBE:  .byte   $E8,$18,$01,$E8,$18
        lda     $0300,x
        and     #$0F
        bne     LAEDF
        inc     $0300,x
        lda     #$3C
        sta     $0440,x
        lda     #$09
        sta     $0460,x
        lda     #$1E
        sta     $0500,x
        jsr     LF883
LAEDF:  lda     $0300,x
        and     #$0F
        cmp     #$02
        beq     LAF3F
        cmp     #$03
        bne     LAEEF
        jmp     LAFA5

LAEEF:  lda     $0500,x
        bne     LAF16
        lda     $05C0,x
        cmp     #$44
        beq     LAF1A
        lda     $0520,x
        bne     LAF1A
        lda     #$45
        jsr     LF835
        ldy     #$15
        jsr     LF67C
        lda     $10
        and     #$10
        beq     LAF19
        lda     #$44
        jsr     LF835
        rts

LAF16:  dec     $0500,x
LAF19:  rts

LAF1A:  lda     $05C0,x
        cmp     #$45
        beq     LAF30
        jsr     LF8C2
        cmp     #$28
        bcs     LAF19
        lda     #$45
        jsr     LF835
        inc     $0520,x
LAF30:  ldy     #$15
        jsr     LF67C
        bcc     LAF19
        lda     #$43
        jsr     LF835
        inc     $0300,x
LAF3F:  lda     $0540,x
        bne     LAF5C
        lda     $04E0,x
        cmp     #$04
        bne     LAF5C
        inc     $0300,x
        lda     #$3C
        sta     $0440,x
        lda     #$09
        sta     $0460,x
        inc     $0540,x
        rts

LAF5C:  lda     $04A0,x
        and     #$01
        beq     LAF6B
        ldy     #$16
        jsr     LF580
        jmp     LAF70

LAF6B:  ldy     #$17
        jsr     LF5C4
LAF70:  lda     $05C0,x
        cmp     #$43
        beq     LAF9B
        ldy     #$15
        jsr     LF67C
        bcc     LAF9A
        lda     #$BB
        sta     $0440,x
        lda     #$06
        sta     $0460,x
        lda     #$43
        jsr     LF835
        lda     #$01
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        jsr     LF869
LAF9A:  rts

LAF9B:  lda     $05A0,x
        bne     LAFA0
LAFA0:  lda     #$46
        jmp     LF835

LAFA5:  lda     $05C0,x
        cmp     #$44
        beq     LAFC6
        lda     #$45
        jsr     LF835
        ldy     #$15
        jsr     LF67C
        lda     $10
        and     #$10
        beq     LAF9A
        lda     #$44
        jsr     LF835
        lda     #$5A
        sta     $0560,x
LAFC6:  dec     $0560,x
        bne     LAFD6
        dec     $0300,x
        jsr     LF81B
        lda     #$45
        jsr     LF835
LAFD6:  rts

        lda     $0300,x
        and     #$0F
        bne     LB00D
        sta     $0440,x
        lda     #$02
        sta     $0460,x
        jsr     LF8B3
        cmp     #$18
        bcs     LB00C
        jsr     LF8C2
        cmp     #$18
        bcs     LB00C
        inc     $0300,x
        lda     #$21
        sta     $0500,x
        lda     #$06
        sta     $0520,x
        lda     $03C0,x
        sta     $0540,x
        lda     #$10
        sta     $0560,x
LB00C:  rts

LB00D:  lda     $0580,x
        and     #$04
        beq     LB026
        dec     $0500,x
        bne     LB00C
        lda     $0580,x
        eor     #$04
        sta     $0580,x
        lda     #$A3
        sta     $0480,x
LB026:  lda     $0300,x
        and     #$02
        bne     LB04C
        lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        dec     $03C0,x
        dec     $03C0,x
        dec     $03C0,x
        dec     $0520,x
        bne     LB00C
        inc     $0300,x
        lda     #$22
        jsr     LF89A
        rts

LB04C:  lda     #$01
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        jsr     LF759
        dec     $0560,x
        bne     LB00C
        lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        lda     $0540,x
        sta     $03C0,x
        lda     $0580,x
        ora     #$94
        sta     $0580,x
        lda     #$80
        sta     $0300,x
        lda     #$83
        sta     $0480,x
        rts

        lda     $0300,x
        and     #$0F
        bne     LB096
        jsr     LF883
        inc     $0300,x
        lda     #$36
        sta     $0500,x
        lda     #$10
        sta     $0520,x
LB096:  lda     $0300,x
        and     #$02
        bne     LB0CB
        lda     $05C0,x
        cmp     #$D6
        beq     LB0B5
        lda     $0540,x
        cmp     #$03
        bcs     LB0ED
        dec     $0500,x
        bne     LB0E0
        lda     #$D6
        jsr     LF835
LB0B5:  lda     #$00
        sta     $05A0,x
        sta     $05E0,x
        jsr     LB11C
        inc     $0540,x
        lda     #$36
        sta     $0500,x
        inc     $0300,x
LB0CB:  lda     #$00
        sta     $05A0,x
        sta     $05E0,x
        dec     $0520,x
        bne     LB0EC
        lda     #$10
        sta     $0520,x
        dec     $0300,x
LB0E0:  lda     $05C0,x
        cmp     #$C6
        beq     LB0EC
        lda     #$C6
        jsr     LF835
LB0EC:  rts

LB0ED:  lda     #$00
        sta     L0000
        lda     #$E2
        sta     $01
        ldy     #$1F
LB0F7:  lda     $0300,y
        bmi     LB110
LB0FC:  dey
        cpy     #$0F
        bne     LB0F7
        lda     L0000
        cmp     #$03
        beq     LB10A
        dec     $0540,x
LB10A:  lda     #$38
        sta     $0500,x
        rts

LB110:  lda     $01
        cmp     $05C0,y
        bne     LB0FC
        inc     L0000
        jmp     LB0FC

LB11C:  jsr     LFC53
        bcs     LB155
        lda     $04A0,x
        sta     $04A0,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$04
        sta     $03C0,y
        lda     #$E2
        jsr     LF846
        lda     #$98
        sta     $0580,y
        lda     #$C0
        sta     $0480,y
        lda     #$42
        sta     $0320,y
        lda     #$01
        sta     $04E0,y
LB155:  rts

        lda     $0300,x
        and     #$0F
        bne     LB173
        sta     $0520,x
        sta     $0400,x
        lda     #$02
        sta     $0420,x
        inc     $0300,x
        lda     #$F0
        sta     $0500,x
        jsr     LF81B
LB173:  ldy     #$08
        jsr     LF67C
        bcc     LB198
        lda     $04A0,x
        and     #$01
        beq     LB189
        ldy     #$08
        jsr     LF580
        jmp     LB18E

LB189:  ldy     #$09
        jsr     LF5C4
LB18E:  bcc     LB198
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LB198:  dec     $0500,x
        bne     LB1A8
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0320,x
LB1A7:  rts

LB1A8:  lda     $0520,x
        bne     LB1A7
        lda     $0500,x
        cmp     #$B4
        bne     LB1A7
        lda     #$F0
        sta     $0500,x
        inc     $0520,x
        rts

        lda     $0300,x
        and     #$0F
        bne     LB1F8
        lda     $04A0,x
        and     #$01
        beq     LB1D3
        ldy     #$18
        jsr     LF580
        jmp     LB1D8

LB1D3:  ldy     #$19
        jsr     LF5C4
LB1D8:  ldy     #$18
        jsr     LF67C
        bcc     LB1EE
        jsr     LB224
        inc     $0300,x
        lda     #$CB
        sta     $0480,x
        lda     #$2B
        bne     LB1F5
LB1EE:  lda     #$C3
        sta     $0480,x
        lda     #$2A
LB1F5:  jmp     LF835

LB1F8:  lda     $05C0,x
        cmp     #$2B
        bne     LB20A
        lda     $05A0,x
        cmp     #$02
        bne     LB21F
        lda     #$29
        bne     LB20C
LB20A:  lda     #$29
LB20C:  jsr     LF835
        lda     $0500,x
        bne     LB220
        lda     #$3C
        sta     $0500,x
        dec     $0300,x
        jsr     LF869
LB21F:  rts

LB220:  dec     $0500,x
        rts

LB224:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$01
        tay
        lda     LB23A,y
        sta     $0440,x
        lda     LB23C,y
        sta     $0460,x
        rts

LB23A:  .byte   $52,$A8
LB23C:  .byte   $04,$05
        lda     $0300,x
        and     #$0F
        bne     LB265
        sta     $0440,x
        lda     #$01
        sta     $0460,x
        inc     $0300,x
        lda     $03C0,x
        cmp     #$88
        bcs     LB25A
        inc     $0520,x
LB25A:  lda     #$10
        sta     $0500,x
        lda     #$01
        sta     $04A0,x
        rts

LB265:  jsr     LF8C2
        cmp     #$16
        bcs     LB29C
        jsr     LF8B3
        cmp     #$15
        bcs     LB29C
        lda     $04A0,x
        and     #$02
        bne     LB27E
        lda     #$01
        bne     LB280
LB27E:  lda     #$02
LB280:  sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
        dec     $0500,x
        bne     LB29C
        lda     #$10
        sta     $0500,x
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LB29C:  lda     $0520,x
        bne     LB2AF
        jsr     LF779
        lda     $03E0,x
        beq     LB2AE
        lda     #$00
        sta     $03E0,x
LB2AE:  rts

LB2AF:  lda     $03C0,x
        pha
        dec     $03C0,x
        jsr     LFAE2
        pla
        sta     $03C0,x
        bcs     LB2F0
        lda     $03C0
        sec
        sbc     $03C0,x
        bcs     LB2DC
        lda     $03C0
        adc     #$02
        sta     $03C0
        cmp     #$F0
        bcc     LB2DC
        adc     #$0F
        sta     $03C0
        inc     $03E0
LB2DC:  stx     $0F
        ldx     #$00
        ldy     #$00
        jsr     LE8D6
        lda     $10
        and     #$10
        beq     LB2EE
        jsr     LEE13
LB2EE:  ldx     $0F
LB2F0:  jsr     LF759
        lda     $03E0,x
        beq     LB2AE
        lda     #$00
        sta     $03E0,x
        rts

        lda     $0500,x
        beq     LB308
        dec     $0500,x
        bne     LB346
LB308:  lda     $0580,x
        and     #$04
        beq     LB31F
        jsr     LF8C2
        cmp     #$61
        bcs     LB346
        lda     $0580,x
        and     #$FB
        sta     $0580,x
        rts

LB31F:  lda     $05E0,x
        bne     LB34B
        lda     $05A0,x
        bne     LB338
        lda     #$80
        sta     $0480,x
        lda     $0580,x
        ora     #$04
        sta     $0580,x
        bne     LB341
LB338:  cmp     #$02
        bne     LB34C
        lda     #$AE
        sta     $0480,x
LB341:  lda     #$1E
        sta     $0500,x
LB346:  lda     #$00
        sta     $05E0,x
LB34B:  rts

LB34C:  lda     #$A4
        sta     $0480,x
        rts

        lda     $0300,x
        and     #$0F
        bne     LB389
        lda     $0580,x
        and     #$04
        beq     LB372
        jsr     LF8C2
        cmp     #$61
        bcs     LB34B
        lda     $0580,x
        and     #$FB
        sta     $0580,x
        jmp     LF883

LB372:  jsr     LF797
        lda     $03C0,x
        cmp     #$78
        bcc     LB34B
        lda     #$11
        sta     $0500,x
        lda     #$03
        sta     $0520,x
        inc     $0300,x
LB389:  lda     $05C0,x
        cmp     #$56
        bne     LB39A
        lda     $05E0,x
        bne     LB34B
        lda     #$55
        jsr     LF835
LB39A:  dec     $0500,x
        lda     $0500,x
        bne     LB3AA
        lda     #$11
        sta     $0500,x
        inc     $0520,x
LB3AA:  lda     $0520,x
        and     #$03
        sta     $0520,x
        tay
        lda     $03A0,x
        clc
        adc     LB44C,y
        sta     $03A0,x
        lda     $03C0,x
        adc     LB450,y
        sta     $03C0,x
        lda     $04A0,x
        and     #$02
        beq     LB3DA
        lda     $0540,x
        bne     LB3D7
        jsr     LF8C2
        bcs     LB3E7
LB3D7:  jmp     LF73B

LB3DA:  lda     $0540,x
        bne     LB3E4
        jsr     LF8C2
        bcc     LB3E7
LB3E4:  jmp     LF71D

LB3E7:  lda     #$26
        jsr     LF89A
        lda     #$56
        jsr     LF835
        stx     L0000
        lda     #$07
        sta     $01
LB3F7:  jsr     LFC53
        bcs     LB446
        ldx     $01
        lda     LB454,x
        sta     $0400,y
        lda     LB45C,x
        sta     $0420,y
        lda     LB464,x
        sta     $0440,y
        lda     LB46C,x
        sta     $0460,y
        lda     LB474,x
        sta     $04A0,y
        ldx     L0000
        lda     #$57
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     #$0F
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sec
        sbc     #$0C
        sta     $03C0,y
        dec     $01
        bpl     LB3F7
LB446:  ldx     L0000
        inc     $0540,x
        rts

LB44C:  .byte   $80,$80,$80,$80
LB450:  .byte   $FF,$00,$00,$FF
LB454:  .byte   $00,$6A,$00,$6A,$00,$6A,$00,$6A
LB45C:  .byte   $00,$01,$02,$01,$00,$01,$02,$01
LB464:  .byte   $00,$96,$00,$6A,$00,$6A,$00,$96
LB46C:  .byte   $FE,$FE,$00,$01,$02,$01,$00,$FE
LB474:  .byte   $02,$02,$02,$02,$01,$01,$01,$01
        ldy     #$08
        jsr     LF67C
        lda     $04A0,x
        and     #$01
        beq     LB490
        ldy     #$1A
        jsr     LF580
        jmp     LB495

LB490:  ldy     #$1B
        jsr     LF5C4
LB495:  bcc     LB49F
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LB49F:  lda     $0300,x
        and     #$0F
        bne     LB4B1
        jsr     LF8C2
        cmp     #$10
        bcs     LB4B0
        inc     $0300,x
LB4B0:  rts

LB4B1:  jsr     LF8C2
        cmp     #$30
        bcc     LB4B0
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        dec     $0300,x
        rts

        ldy     #$1A
        jsr     LF67C
        rol     $0F
        jsr     LFB7B
        bcs     LB4EC
        lda     #$18
        jsr     LF89A
        ldy     $10
        lda     #$00
        sta     $0300,y
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$39
        sta     $0320,x
        rts

LB4EC:  lda     $04A0,x
        and     #$01
        beq     LB4FB
        ldy     #$1C
        jsr     LF580
        jmp     LB500

LB4FB:  ldy     #$1D
        jsr     LF5C4
LB500:  lda     $0F
        and     #$01
        beq     LB53F
        lda     $10
        and     #$10
        beq     LB52E
        lda     #$52
        jsr     LF835
        inc     $0500,x
        lda     $0500,x
        cmp     #$04
        beq     LB526
        lda     #$29
        sta     $0440,x
        lda     #$05
        sta     $0460,x
        rts

LB526:  lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LB52E:  lda     $05C0,x
        cmp     #$51
        beq     LB53A
        lda     #$51
        jsr     LF835
LB53A:  lda     #$00
        sta     $0500,x
LB53F:  rts

        lda     $0580,x
        bmi     LB55B
        lda     #$00
        sta     $0300,x
LB54A:  sta     $95
        sta     $72
        lda     #$30
        sta     $1D
        lda     #$F0
        sta     $1E
        lda     #$FF
        sta     $1C
        rts

LB55B:  ora     #$08
        sta     $0580,x
        jsr     L8003
        lda     $04E0,x
        beq     LB54A
        lda     $05A0,x
        bne     LB579
        lda     $05E0,x
        cmp     #$01
        bne     LB579
        lda     #$00
        sta     $05E0,x
LB579:  lda     $0500,x
        bne     LB5C6
        lda     $04A0,x
        and     #$01
        beq     LB58B
        jsr     LF71D
        jmp     LB58E

LB58B:  jsr     LF73B
LB58E:  lda     $0300,x
        and     #$0F
        bne     LB5CB
        jsr     LF8C2
        cmp     #$61
        bcs     LB5CB
        inc     $0300,x
        inc     $05A0,x
        lda     #$3C
        sta     $0500,x
        lda     $0320,x
        and     #$01
        beq     LB5B8
        lda     #$40
        sta     $0400,x
        lda     #$00
        sta     $0420,x
LB5B8:  lda     #$FF
        sta     $1C
        lda     #$10
        sta     $1D
        sta     $1E
        lda     #$00
        sta     $95
LB5C6:  dec     $0500,x
        bne     LB5CB
LB5CB:  rts

        lda     $0300,x
        and     #$0F
        bne     LB5FD
        sta     $0440,x
        lda     #$03
        sta     $0460,x
        jsr     LF8C2
        cmp     #$64
        bcs     LB5FC
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     LB6E4,y
        sta     $0540,x
        inc     $0300,x
        lda     $0580,x
        eor     #$04
        sta     $0580,x
LB5FC:  rts

LB5FD:  lda     $0560,x
        bne     LB61B
        lda     $0540,x
        beq     LB60D
        dec     $0540,x
        jmp     LF759

LB60D:  lda     #$4D
        jsr     LF835
        inc     $0560,x
        lda     #$10
        sta     $0540,x
        rts

LB61B:  lda     $05A0,x
        cmp     #$02
        bcc     LB62C
        lda     #$03
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
LB62C:  lda     $0300,x
        and     #$02
        beq     LB636
        jmp     LB6BC

LB636:  lda     $0500,x
        bne     LB694
        ldy     $0520,x
        lda     LB6D6,y
        asl     a
        tay
        lda     L8F4A,y
        sta     $0440,x
        lda     L8F4B,y
        sta     $0460,x
        lda     L8F6A,y
        sta     $0400,x
        lda     L8F6B,y
        sta     $0420,x
        lda     $0420,x
        bpl     LB675
        lda     $0400,x
        eor     #$FF
        clc
        adc     #$01
        sta     $0400,x
        lda     $0420,x
        eor     #$FF
        adc     #$00
        sta     $0420,x
LB675:  inc     $0520,x
        lda     $0520,x
        cmp     #$07
        bne     LB68F
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        inc     $0300,x
        lda     #$00
        sta     $0520,x
LB68F:  lda     #$05
        sta     $0500,x
LB694:  dec     $0500,x
        lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $04A0,x
        and     #$02
        bne     LB6B8
        jsr     LF71D
        bcs     LB6BB
        bcc     LB6BB
LB6B8:  jsr     LF73B
LB6BB:  rts

LB6BC:  dec     $0540,x
        bne     LB6C9
        dec     $0300,x
        lda     #$10
        sta     $0540,x
LB6C9:  lda     #$80
        sta     $0440,x
        lda     #$00
        sta     $0460,x
        jmp     LF759

LB6D6:  .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F
LB6E4:  .byte   $22,$2A,$26,$2E
        lda     #$00
        sta     $05E0,x
        lda     $0300,x
        and     #$0F
        beq     LB752
        lda     $B0
        cmp     #$9C
        bne     LB751
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        stx     L0000
        lda     $0320,x
        and     #$07
        tay
        lda     LB7FB,y
        sta     $0320,x
        lda     #$CA
        sta     $0480,x
        lda     #$1C
        sta     $04E0,x
        lda     LB843,y
        sta     $0400,x
        lda     LB84B,y
        sta     $0420,x
        jsr     LF81B
        lda     LB81B,y
        sta     $ED
        jsr     LFF3C
        lda     #$C0
        sta     $0300,x
        tya
        asl     a
        asl     a
        tay
        ldx     #$00
LB73C:  lda     LB823,y
        sta     $061C,x
        sta     $063C,x
        iny
        inx
        cpx     #$04
        bne     LB73C
        lda     #$FF
        sta     $18
        ldx     L0000
LB751:  rts

LB752:  jsr     LB7DF
        inc     $0300,x
        jsr     LFC53
        lda     #$61
        sta     $0320,y
        lda     $03C0,x
        sta     $0520,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     #$10
        sta     $03C0,y
        lda     #$80
        sta     $0440,y
        sta     $0480,y
        lda     #$00
        sta     $0460,y
        stx     L0000
        lda     $0320,x
        and     #$07
        tax
        lda     LB7F3,x
        ldx     L0000
        jsr     LF846
        lda     $0320,x
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
        sta     $18
        jmp     LFF3C

        jsr     LF759
        lda     $03C0,x
        cmp     $0520,x
        beq     LB7D9
        lda     #$80
        sta     $B0
        rts

LB7D9:  lda     #$00
        sta     $0300,x
        rts

LB7DF:  lda     #$09
        sta     $30
        lda     #$80
        sta     $B0
        sta     $5A
        lda     #$8E
        sta     $B3
        lda     #$0C
        jsr     LF898
        rts

LB7F3:  .byte   $16,$1A,$14,$18,$15,$13,$19,$17
LB7FB:  .byte   $A0,$B0,$B2,$A1,$A2,$B3,$A3,$B1
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
        lda     $0300,x
        and     #$0F
        bne     LB860
        inc     $0300,x
        jsr     LB7DF
LB860:  lda     $03C0,x
        cmp     #$80
        bcs     LB86D
        jsr     LF797
        jmp     LB8BD

LB86D:  lda     $0320,x
        and     #$07
        tay
        lda     LB8EF,y
        tay
        jsr     LF67C
        bcc     LB8BD
        lda     $0320,x
        and     #$07
        tay
        lda     LB8E7,y
        cmp     $05A0,x
        bne     LB8C2
        lda     #$00
        sta     $05E0,x
        lda     $B0
        cmp     #$9C
        bne     LB8C6
        lda     #$C0
        sta     $0300,x
        lda     #$1C
        sta     $04E0,x
        lda     $0320,x
        and     #$07
        tay
        lda     LB8C7,y
        sta     $0320,x
        lda     LB8CF,y
        sta     $0400,x
        lda     LB8D7,y
        sta     $0420,x
        lda     LB8DF,y
        jmp     LF835

LB8BD:  lda     #$00
        sta     $05E0,x
LB8C2:  lda     #$80
        sta     $B0
LB8C6:  rts

LB8C7:  .byte   $C0,$C1,$D6,$D0,$C2,$D4,$D2,$C3
LB8CF:  .byte   $B3,$00,$2D,$00,$00,$4C,$6D,$00
LB8D7:  .byte   $01,$00,$03,$01,$04,$01,$01,$04
LB8DF:  .byte   $29,$1F,$33,$2C,$49,$22,$36,$3F
LB8E7:  .byte   $04,$03,$05,$06,$02,$02,$08,$03
LB8EF:  .byte   $1E,$1E,$00,$26,$1E,$00,$1E,$1E
        .byte   $60
        lda     $03C0,x
        pha
        dec     $03C0,x
        jsr     LFAE2
        pla
        sta     $03C0,x
        bcs     LB92A
        lda     $05C0,x
        and     #$01
        tay
        lda     $0340
        clc
        adc     LB92B,y
        sta     $0340
        lda     $0360
        adc     LB92D,y
        sta     $0360
        lda     $0380
        adc     LB92F,y
        sta     $0380
LB92A:  rts

LB92B:  .byte   $80,$80
LB92D:  .byte   $00,$FF
LB92F:  .byte   $00,$FF
        lda     $0300,x
        and     #$0F
        bne     LB964
        sta     $05A0,x
        sta     $05E0,x
        sta     $0520,x
        jsr     LF8B3
        cmp     #$15
        bcs     LB963
        lda     $30
        bne     LB963
        lda     $03C0,x
        cmp     $03C0
        bcc     LB963
        jsr     LF8C2
        cmp     #$18
        bcs     LB963
        lda     #$0C
        sta     $0500,x
        inc     $0300,x
LB963:  rts

LB964:  lda     $0520,x
        bne     LB979
        dec     $0500,x
        bne     LB963
        inc     $0520,x
        lda     $0580,x
        and     #$90
        sta     $0580,x
LB979:  lda     $0300,x
        and     #$02
        bne     LB98F
        lda     $05A0,x
        cmp     #$04
        bne     LB963
        inc     $0300,x
        lda     #$14
        sta     $0500,x
LB98F:  lda     #$04
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        dec     $0500,x
        bne     LB963
        lda     $0580,x
        eor     #$01
        sta     $0580,x
        lda     #$80
        sta     $0300,x
        rts

        ldy     #$01
LB9AE:  lda     $0300,y
        bpl     LB9DF
        lda     $05C0,y
        cmp     #$AC
        beq     LB9BE
        cmp     #$AF
        bne     LB9DF
LB9BE:  jsr     LFB7B
        bcs     LB9DF
        lda     #$18
        jsr     LF89A
        ldy     $10
        lda     #$00
        sta     $0300,y
        lda     #$71
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$19
        sta     $0320,x
        rts

LB9DF:  iny
        cpy     #$03
        bcc     LB9AE
        rts

        lda     $0300,x
        and     #$0F
        bne     LBA0B
        sta     $0440,x
        lda     #$01
        sta     $0460,x
        lda     $03C0,x
        sta     $0560,x
        jsr     LF8B3
        cmp     #$15
        bcs     LBA41
        jsr     LF8C2
        cmp     #$0A
        bcs     LBA41
        inc     $0300,x
LBA0B:  lda     $0500,x
        bne     LBA42
        jsr     LF779
        lda     $0440,x
        clc
        adc     #$10
        sta     $0440,x
        lda     $0460,x
        adc     #$00
        sta     $0460,x
        cmp     #$03
        bne     LBA2D
        lda     #$00
        sta     $0440,x
LBA2D:  lda     $03C0,x
        cmp     #$3A
        bcs     LBA41
        inc     $0500,x
        lda     #$00
        sta     $0440,x
        lda     #$01
        sta     $0460,x
LBA41:  rts

LBA42:  lda     $0300,x
        and     #$02
        bne     LBA57
        jsr     LB2AF
        lda     $03C0,x
        cmp     $0560,x
        bcc     LBA41
        inc     $0300,x
LBA57:  jsr     LF8B3
        cmp     #$16
        bcs     LBA41
        jsr     LF8C2
        cmp     #$09
        bcs     LBA41
        dec     $0300,x
        dec     $0500,x
        rts

        lda     $0300,x
        and     #$0F
        bne     LBA8C
        inc     $0300,x
        lda     $E4
        adc     $E6
        sta     $E4
        and     #$03
        tay
        lda     LBB51,y
        sta     $0520,x
        lda     #$78
        sta     $0500,x
        bne     LBB01
LBA8C:  lda     $0500,x
        bne     LBAFE
        lda     $05A0,x
        ora     #$01
        sta     $05A0,x
        lda     $0540,x
        bne     LBAF8
        jsr     LFC53
        lda     #$BA
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$80
        sta     $0480,y
        lda     #$8F
        sta     $0320,y
        lda     #$00
        sta     $0400,y
        sta     $02
        lda     #$04
        sta     $0420,y
        sta     $03
        sty     $0F
        stx     $0E
        ldx     $0F
        jsr     LFC63
        ldy     $0F
        ldx     $0E
        lda     $0C
        sta     $04A0,y
        dec     $0520,x
        beq     LBAEE
        lda     #$12
        sta     $0540,x
        bne     LBB01
LBAEE:  lda     #$00
        sta     $05A0,x
        dec     $0300,x
        bne     LBB01
LBAF8:  dec     $0540,x
        jmp     LBB01

LBAFE:  dec     $0500,x
LBB01:  lda     $05C0,x
        pha
        jsr     L8003
        pla
        sta     $05C0,x
        lda     $04E0,x
        bne     LBB2E
        sta     $0300,x
        ldy     #$0F
LBB16:  lda     $0310,y
        bpl     LBB27
        lda     $03D0,y
        cmp     #$80
        bcs     LBB27
        lda     #$00
        sta     $0310,y
LBB27:  dey
        bpl     LBB16
        lda     #$80
        sta     $55
LBB2E:  lda     #$00
        sta     $05E0,x
        rts

        lda     $04A0,x
        and     #$01
        beq     LBB41
        jsr     LF71D
        jmp     LBB44

LBB41:  jsr     LF73B
LBB44:  lda     $04A0,x
        and     #$08
        beq     LBB4E
        jmp     LF779

LBB4E:  jmp     LF759

LBB51:  .byte   $03,$03,$04,$02
        lda     $05C0,x
        beq     LBBBB
        jsr     LF797
        ldy     #$00
        sty     $54
        lda     $0380,x
        cmp     #$0B
        beq     LBB69
        iny
LBB69:  lda     LBBBC,y
        cmp     $03C0,x
        bcs     LBBB6
        sta     $03C0,x
        lda     $05E0,x
        cmp     #$02
        bne     LBBBB
        lda     $05A0,x
        cmp     #$02
        bne     LBBBB
        lda     #$20
        sta     $060D
        sta     $062D
        lda     #$37
        sta     $060E
        sta     $062E
        lda     #$17
        sta     $060F
        sta     $062F
        sta     $18
        lda     #$00
        sta     $05C0,x
        ldy     #$1F
LBBA3:  lda     $0300,y
        bpl     LBBB0
        lda     $0580,y
        and     #$FB
        sta     $0580,y
LBBB0:  dey
        cpy     #$0F
        bne     LBBA3
        rts

LBBB6:  lda     #$00
        sta     $05E0,x
LBBBB:  rts

LBBBC:  .byte   $48,$78
        lda     $0580,x
        and     #$04
        bne     LBBBB
        lda     $05C0,x
        pha
        jsr     L8003
        pla
        sta     $05C0,x
        lda     $04E0,x
        bne     LBBF5
        sta     $0520,x
        sta     $0480,x
        lda     #$04
        sta     $0540,x
        lda     #$63
        sta     $0320,x
        ldy     #$0F
        lda     #$00
LBBE9:  sta     $0310,y
        dey
        bpl     LBBE9
        lda     #$80
        sta     $0300,x
        rts

LBBF5:  lda     $0300,x
        and     #$0F
        bne     LBC1E
        sta     $05E0,x
        ldy     #$1F
LBC01:  lda     $0300,y
        bpl     LBC11
        lda     $05C0,y
        cmp     #$CF
        beq     LBC1D
        cmp     #$D0
        beq     LBC1D
LBC11:  dey
        cpy     #$0F
        bne     LBC01
        lda     $54
        bne     LBC1D
        inc     $0300,x
LBC1D:  rts

LBC1E:  lda     $05E0,x
        ora     $05A0,x
        bne     LBC44
        inc     $0520,x
        lda     $0520,x
        cmp     #$02
        bne     LBC3E
        dec     $0300,x
        lda     #$00
        sta     $0500,x
        sta     $0520,x
        inc     $54
        rts

LBC3E:  lda     #$3C
        sta     $0500,x
        rts

LBC44:  lda     $0500,x
        beq     LBC52
        dec     $0500,x
        lda     #$00
        sta     $05E0,x
        rts

LBC52:  lda     $05E0,x
        bne     LBCA6
        lda     $05A0,x
        cmp     #$02
        bne     LBCA6
        jsr     LFC53
        bcs     LBCA6
        lda     #$CF
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$C0
        sta     $0480,y
        lda     #$08
        sta     $04E0,y
        lda     #$8D
        sta     $0320,y
        lda     #$00
        sta     $0400,y
        lda     #$01
        sta     $0420,y
        lda     #$00
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        jsr     LF869
        lda     $04A0,x
        sta     $04A0,y
LBCA6:  rts

LBCA7:  rts

        lda     $0580,x
        and     #$04
        bne     LBCA7
        lda     $0300,x
        and     #$0F
        bne     LBCD8
        sta     $05E0,x
        ldy     #$1F
LBCBB:  lda     $0300,y
        bpl     LBCCB
        lda     $05C0,y
        cmp     #$CF
        beq     LBCD7
        cmp     #$D0
        beq     LBCD7
LBCCB:  dey
        cpy     #$0F
        bne     LBCBB
        lda     $54
        beq     LBCD7
        inc     $0300,x
LBCD7:  rts

LBCD8:  lda     $05E0,x
        ora     $05A0,x
        bne     LBCE3
        dec     $0300,x
LBCE3:  lda     $05A0,x
        cmp     #$02
        bne     LBCD7
        lda     $05E0,x
        bne     LBCD7
        lda     #$02
        sta     $10
        jsr     LF869
LBCF6:  jsr     LFC53
        bcs     LBD58
        lda     #$D0
        jsr     LF846
        lda     #$01
        sta     $05A0,y
        sta     $04E0,y
        lda     #$CB
        sta     $0480,y
        lda     #$8E
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $04A0,x
        sta     $04A0,y
        stx     L0000
        ldx     $10
        lda     LBD59,x
        sta     $0440,y
        lda     LBD5C,x
        sta     $0460,y
        lda     LBD5F,x
        sta     $0400,y
        lda     LBD62,x
        sta     $0420,y
        lda     #$1E
        sta     $0500,y
        sta     $0520,y
        ldx     L0000
        dec     $10
        bpl     LBCF6
        lda     #$00
        sta     $54
LBD58:  rts

LBD59:  .byte   $44,$00,$2A
LBD5C:  .byte   $03,$04,$05
LBD5F:  .byte   $39,$55,$8C
LBD62:  .byte   $01,$01,$01
        ldy     #$08
        jsr     LF67C
        bcc     LBD76
        lda     #$44
        sta     $0440,x
        lda     #$03
        sta     $0460,x
LBD76:  lda     $04A0,x
        and     #$01
        beq     LBD85
        ldy     #$08
        jsr     LF580
        jmp     LBD8A

LBD85:  ldy     #$09
        jsr     LF5C4
LBD8A:  bcc     LBD94
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LBD94:  rts

        lda     $0500,x
        beq     LBDAD
        dec     $0500,x
        jsr     LF797
        lda     $04A0,x
        and     #$01
        beq     LBDAA
        jmp     LF71D

LBDAA:  jmp     LF73B

LBDAD:  ldy     #$12
        jsr     LF67C
        lda     #$01
        sta     $05A0,x
        bcc     LBDDF
        lda     #$00
        sta     $05A0,x
        dec     $0520,x
        bne     LBDF3
        lda     #$3C
        sta     $0520,x
        lda     #$A8
        sta     $0440,x
        lda     #$05
        sta     $0460,x
        lda     #$F1
        sta     $0400,x
        lda     #$00
        sta     $0420,x
        jsr     LF869
LBDDF:  lda     $04A0,x
        and     #$01
        beq     LBDEE
LBDE6:  ldy     #$1E
        jsr     LF580
        .byte   $4C
LBDEC:  .byte   $F3
        .byte   $BD
LBDEE:  ldy     #$1F
        jsr     LF5C4
LBDF3:  lda     #$00
        sta     $05E0,x
        rts

        ldy     #$2C
        bne     LBDFF
        ldy     #$2D
LBDFF:  jsr     LF67C
        jsr     LFAE2
        bcs     LBE3A
        lda     $0500,x
        bne     LBE25
        lda     $04C0,x
        pha
        and     #$07
        tay
        lda     $DEC2,y
        sta     L0000
        pla
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     $0150,y
        ora     L0000
        sta     $0150,y
LBE25:  lda     #$00
        sta     $0300,x
        ldy     $0320,x
        lda     LBDE6,y
        sta     L0000
        lda     LBDEC,y
        sta     $01
        jmp     (L0000)

LBE3A:  lda     $0500,x
        beq     LBE49
        dec     $0500,x
        bne     LBE49
        lda     #$00
        sta     $0300,x
LBE49:  rts

        .byte   $56,$5C,$62,$66,$9D,$AB,$BE,$BE
        .byte   $BE,$BE,$BE,$BE
        lda     #$0A
        ldy     #$00
        beq     LBE6C
        lda     #$02
        ldy     #$00
        beq     LBE6C
        lda     #$0A
        bne     LBE68
        lda     #$02
LBE68:  ldy     $A0
        beq     LBE98
LBE6C:  inc     $58
        sta     $0F
        sty     $0E
LBE72:  ldy     $0E
        lda     $A2,y
        cmp     #$9C
        beq     LBE98
        lda     $A2,y
        clc
        adc     #$01
        sta     $A2,y
        lda     #$1C
        jsr     LF89A
        dec     $0F
        beq     LBE98
LBE8D:  jsr     LFD6E
        lda     $95
        and     #$03
        bne     LBE8D
        beq     LBE72
LBE98:  lda     #$00
        sta     $58
        rts

        lda     #$14
        jsr     LF89A
        lda     $AF
        cmp     #$09
        beq     LBEAA
        inc     $AF
LBEAA:  rts

        lda     #$14
        jsr     LF89A
        lda     $AE
        cmp     #$99
        beq     LBED1
        inc     $AE
        lda     $AE
        and     #$0F
        cmp     #$0A
        bne     LBED1
        lda     $AE
        and     #$F0
        clc
        adc     #$10
        sta     $AE
        cmp     #$A0
        bne     LBED1
        lda     #$99
        sta     $AE
LBED1:  rts

        lda     $05C0,x
        cmp     #$71
        beq     LBF03
        jsr     LFB7B
        bcs     LBED1
        lda     $04C0,x
        pha
        and     #$07
        tay
        lda     $DEC2,y
        sta     L0000
        pla
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     $0150,y
        ora     L0000
        sta     $0150,y
        ldy     $10
        lda     #$00
        sta     $0300,y
        lda     #$71
        jmp     LF835

LBF03:  lda     $05A0,x
        cmp     #$04
        bne     LBF3E
        lda     $E5
        adc     $E6
        sta     $E5
        sta     L0000
        lda     #$64
        sta     $01
        jsr     LFCEB
        ldy     #$05
        lda     $03
LBF1D:  cmp     LBF3F,y
        bcc     LBF25
        dey
        bne     LBF1D
LBF25:  lda     LBF45,y
        jsr     LF835
        lda     LBF4B,y
        sta     $0320,x
        lda     $0580,x
        and     #$FC
        sta     $0580,x
        lda     #$F0
        sta     $0500,x
LBF3E:  .byte   $60
LBF3F:  .byte   $63,$41,$23,$19,$0F,$05
LBF45:  .byte   $FB,$F9,$FA,$FC,$FE,$FD
LBF4B:  .byte   $66,$64,$65
        .byte   $67
        adc     #$68
        lda     $05A0,x
        cmp     #$04
        bne     LBF96
        lda     $5A
        bmi     LBF77
        lda     $E6
        adc     $E7
        sta     $E7
        sta     L0000
        lda     #$64
        sta     $01
        jsr     LFCEB
        ldy     #$04
        lda     $03
LBF6F:  cmp     LBF97,y
        bcc     LBF7D
        dey
        bpl     LBF6F
LBF77:  lda     #$00
        sta     $0300,x
        rts

LBF7D:  lda     LBF9C,y
        jsr     LF835
        lda     LBFA1,y
        sta     $0320,x
        lda     #$F0
        sta     $0500,x
        lda     #$00
        sta     $0440,x
        sta     $0460,x
LBF96:  .byte   $60
LBF97:  .byte   $1D,$1B,$0C,$0A,$01
LBF9C:  .byte   $FB,$FC,$F9,$FA,$FE
LBFA1:  .byte   $66,$67,$64,$65,$69,$ED,$40,$40
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
