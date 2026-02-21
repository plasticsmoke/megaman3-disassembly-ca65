; =============================================================================
; MEGA MAN 3 (U) — BANK $07 — ROBOT MASTER AI (HARD/SPARK/SNAKE/GEMINI)
; =============================================================================
; AI state machines for Hard Man, Spark Man, Snake Man, and Gemini Man.
;
; Annotation: 0% — unannotated da65 output
; =============================================================================

        .setcpu "6502"

L0000           := $0000
L8003           := $8003
LF580           := $F580
LF5C4           := $F5C4
LF606           := $F606
LF67C           := $F67C
LF71D           := $F71D
LF73B           := $F73B
LF759           := $F759
LF779           := $F779
LF835           := $F835
LF846           := $F846
LF869           := $F869
LF883           := $F883
LF89A           := $F89A
LF8C2           := $F8C2
LFC53           := $FC53
LFC63           := $FC63

.segment "BANK07"

        jmp     LA019

        jmp     LA277

        jmp     LA319

        nop
        nop
        nop
        jmp     LA51B

        nop
        nop
        nop
        jmp     LA6B2

        jmp     LA972

        rts

LA019:  lda     $0300,x
        and     #$0F
        tay
        lda     LA02C,y
        sta     L0000
        lda     LA031,y
        sta     $01
        jmp     (L0000)

LA02C:  .byte   $36,$52,$B2,$66,$CF
LA031:  .byte   $A0,$A0,$A0,$A1,$A1
        lda     #$00
        sta     $0500,x
        sta     $0520,x
        sta     $0540,x
        lda     #$3D
        sta     $0560,x
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        inc     $0300,x
        rts

        lda     $04A0,x
        pha
        jsr     LF869
        pla
        cmp     $04A0,x
        beq     LA067
        lda     $0580,x
        eor     #$40
        sta     $0580,x
LA067:  lda     $0500,x
        bne     LA079
        lda     $05A0,x
        cmp     #$05
        bne     LA08B
        jsr     LA22E
        inc     $0500,x
LA079:  lda     $0520,x
        bne     LA08B
        lda     $05A0,x
        cmp     #$08
        bne     LA08B
        jsr     LA22E
        inc     $0520,x
LA08B:  lda     $05A0,x
        cmp     #$0B
        bne     LA0B1
        lda     #$2F
        jsr     LF835
        lda     #$00
        sta     $0500,x
        jsr     LA1FF
        lda     #$68
        sta     $0440,x
        lda     #$08
        sta     $0460,x
        inc     $0300,x
        lda     #$1E
        sta     $0520,x
LA0B1:  rts

        lda     $0520,x
        beq     LA0C5
        dec     $0520,x
        lda     #$00
        sta     $05E0,x
        lda     #$01
        sta     $05A0,x
        rts

LA0C5:  lda     $0500,x
        bne     LA128
        lda     $04A0,x
        and     #$01
        beq     LA0D9
        ldy     #$22
        jsr     LF580
        jmp     LA0DE

LA0D9:  ldy     #$23
        jsr     LF5C4
LA0DE:  ldy     #$26
        jsr     LF67C
        bcc     LA106
        lda     #$30
        jsr     LF835
        lda     #$04
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        inc     $0300,x
        lda     $30
        cmp     #$0E
        beq     LA165
        cmp     #$0F
        beq     LA165
        lda     #$0F
        sta     $30
        rts

LA106:  lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        jsr     LF8C2
        cmp     #$08
        bcs     LA127
        lda     #$30
        jsr     LF835
        inc     $0500,x
        lda     #$00
        sta     $0440,x
        lda     #$05
        sta     $0460,x
LA127:  rts

LA128:  lda     $05A0,x
        cmp     #$03
        bne     LA127
        lda     #$03
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        lda     #$30
        jsr     LF89A
        ldy     #$26
        jsr     LF606
        bcc     LA127
        lda     #$30
        jsr     LF835
        lda     #$04
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        inc     $0300,x
        lda     $30
        cmp     #$0E
        beq     LA165
        cmp     #$0F
        beq     LA165
        lda     #$0F
        sta     $30
LA165:  rts

        lda     $0560,x
        beq     LA1A0
        dec     $0560,x
        beq     LA1A0
        lda     #$CA
        sta     $0480,x
        lda     #$04
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        lda     #$9E
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        lda     $0560,x
        and     #$01
        bne     LA198
        lda     $FA
        clc
        adc     #$02
        sta     $FA
        rts

LA198:  lda     $FA
        sec
        sbc     #$02
        sta     $FA
        rts

LA1A0:  ldy     #$26
        jsr     LF67C
        bcc     LA1BA
        lda     $30
        cmp     #$0E
        beq     LA1B1
        lda     #$01
        sta     $30
LA1B1:  inc     $0300,x
        lda     #$10
        sta     $0540,x
        rts

LA1BA:  lda     #$D0
        sta     $0480,x
        lda     $05A0,x
        cmp     #$04
        bne     LA1CE
        lda     #$00
        sta     $05E0,x
        inc     $05A0,x
LA1CE:  rts

        dec     $0540,x
        bne     LA1EF
        lda     #$C1
        sta     $0300,x
        lda     #$00
        sta     $0500,x
        sta     $0520,x
        sta     $0540,x
        lda     #$3D
        sta     $0560,x
        lda     #$2C
        jsr     LF835
        rts

LA1EF:  lda     #$2F
        jsr     LF835
        lda     #$00
        sta     $05E0,x
        lda     #$01
        sta     $05A0,x
        rts

LA1FF:  jsr     LF8C2
        ldy     #$06
LA204:  cmp     LA219,y
        bcc     LA20C
        dey
        bne     LA204
LA20C:  lda     LA220,y
        sta     $0400,x
        lda     LA227,y
        sta     $0420,x
        rts

LA219:  .byte   $79,$6A,$5B,$4C,$3D,$2E,$1F
LA220:  .byte   $80,$00,$80,$00,$80,$00,$80
LA227:  .byte   $03,$03,$02,$02,$01,$01,$00
LA22E:  jsr     LFC53
        bcs     LA272
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LA273,y
        pha
        lda     $0380,x
        adc     LA274,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        clc
        adc     #$06
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        lda     #$2D
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        lda     #$D1
        sta     $0320,y
LA272:  rts

LA273:  .byte   $04
LA274:  .byte   $00,$FC,$FF
LA277:  lda     $0300,x
        and     #$0F
        bne     LA28F
        sta     $0500,x
        sta     $0520,x
        lda     #$0C
        sta     $0540,x
        jsr     LA308
        inc     $0300,x
LA28F:  lda     $0300,x
        and     #$02
        bne     LA2EE
        lda     $04A0,x
        and     #$08
        beq     LA2A8
        jsr     LF779
        lda     $03E0,x
        bne     LA302
        jmp     LA2B0

LA2A8:  jsr     LF759
        lda     $03E0,x
        bne     LA302
LA2B0:  jsr     LF883
        lda     $04A0,x
        and     #$01
        beq     LA2C0
        jsr     LF71D
        jmp     LA2C3

LA2C0:  jsr     LF73B
LA2C3:  lda     $0500,x
        bne     LA2D3
        jsr     LF8C2
        cmp     #$0C
        bcs     LA2D2
        inc     $0500,x
LA2D2:  rts

LA2D3:  lda     $0520,x
        bne     LA2D2
        dec     $0540,x
        bne     LA2D2
        lda     #$0C
        sta     $0540,x
        inc     $0520,x
        lda     #$2E
        jsr     LF835
        inc     $0300,x
        rts

LA2EE:  lda     $05E0,x
        ora     $05A0,x
        bne     LA301
        lda     #$2D
        jsr     LF835
        jsr     LA308
        dec     $0300,x
LA301:  rts

LA302:  lda     #$00
        sta     $0300,x
        rts

LA308:  lda     #$4C
        sta     $02
        lda     #$03
        sta     $03
        jsr     LFC63
        lda     $0C
        sta     $04A0,x
        rts

LA319:  lda     $0300,x
        and     #$0F
        beq     LA323
        jmp     LA3E7

LA323:  lda     $0500,x
        beq     LA339
        dec     $0500,x
        bne     LA330
        dec     $05A0,x
LA330:  jsr     LA35D
LA333:  lda     #$00
        sta     $05E0,x
        rts

LA339:  lda     $05A0,x
        beq     LA36C
        lda     $05E0,x
        cmp     #$08
        bne     LA301
        dec     $0540,x
        beq     LA355
LA34A:  lda     #$88
        sta     $0440,x
        lda     #$06
        sta     $0460,x
        rts

LA355:  lda     #$38
        jsr     LF835
        inc     $0300,x
LA35D:  lda     $04A0,x
        pha
        jsr     LF869
        jsr     LF883
        pla
        sta     $04A0,x
        rts

LA36C:  lda     $0540,x
        bne     LA38A
        lda     #$37
        jsr     LF835
        jsr     LF883
        jsr     LA34A
        lda     $E4
        adc     $E6
        sta     $E7
        and     #$03
        clc
        adc     #$01
        sta     $0540,x
LA38A:  ldy     #$1E
        jsr     LF67C
        bcs     LA3B7
        jsr     LA333
        lda     $0520,x
        and     #$03
        tay
        lda     LA4E7,y
        sta     $0400,x
        lda     LA4EB,y
        sta     $0420,x
        lda     $04A0,x
        and     #$02
        beq     LA3B2
        ldy     #$21
        jmp     LF5C4

LA3B2:  ldy     #$20
        jmp     LF580

LA3B7:  lda     $0520,x
        tay
        lda     LA4DF,y
        sta     $0360,x
        inc     $05A0,x
        jsr     LA333
        inc     $0520,x
        lda     $0520,x
        and     #$07
        sta     $0520,x
        and     #$03
        bne     LA3E6
LA3D6:  lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     $0580,x
        eor     #$40
        sta     $0580,x
LA3E6:  rts

LA3E7:  jsr     LA35D
        lda     $05A0,x
        ora     $05E0,x
        bne     LA40D
        lda     #$39
        cmp     $05C0,x
        beq     LA3FC
        jmp     LF835

LA3FC:  lda     #$37
        jsr     LF835
        inc     $05A0,x
        lda     #$64
        sta     $0500,x
        dec     $0300,x
        rts

LA40D:  lda     $05E0,x
        bne     LA3E6
        lda     #$39
        cmp     $05C0,x
        beq     LA423
        lda     $05A0,x
        cmp     #$03
        bne     LA3E6
        jmp     LA42D

LA423:  lda     $05A0,x
        cmp     #$0A
        bne     LA3E6
        jmp     LA485

LA42D:  stx     L0000
        lda     #$07
        sta     $01
LA433:  jsr     LFC53
        bcs     LA482
        ldx     $01
        lda     LA4F3,x
        sta     $0400,y
        lda     LA4FB,x
        sta     $0420,y
        lda     LA503,x
        sta     $0440,y
        lda     LA50B,x
        sta     $0460,y
        lda     LA513,x
        sta     $04A0,y
        ldx     L0000
        lda     #$3A
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        lda     #$43
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sec
        sbc     #$08
        sta     $03C0,y
        dec     $01
        bpl     LA433
LA482:  ldx     L0000
        rts

LA485:  stx     $0E
        jsr     LFC53
        bcs     LA4DC
        lda     $03C0,x
        clc
        adc     #$05
        sta     $03C0,y
        lda     $0380,x
        sta     $0380,y
        lda     $0360,x
        sta     $0360,y
        lda     #$3C
        jsr     LF846
        lda     #$8A
        sta     $0480,y
        lda     #$B8
        sta     $0320,y
        sty     $0F
        lda     #$00
        sta     $02
        lda     #$02
        sta     $03
        tya
        tax
        jsr     LFC63
        ldy     $0F
        lda     $0C
        sta     $04A0,y
        and     #$02
        tax
        lda     $0360,y
        clc
        adc     LA4EF,x
        sta     $0360,y
        lda     $0380,y
        adc     LA4F0,x
        sta     $0380,y
LA4DC:  ldx     $0E
        rts

LA4DF:  .byte   $A8,$80,$58,$20,$58,$80,$A8,$E0
LA4E7:  .byte   $6D,$05,$05,$6D
LA4EB:  .byte   $01,$01,$01,$01
LA4EF:  .byte   $20
LA4F0:  .byte   $00,$E0,$FF
LA4F3:  .byte   $00,$6A,$00,$6A,$00,$6A,$00,$6A
LA4FB:  .byte   $00,$01,$02,$01,$00,$01,$02,$01
LA503:  .byte   $00,$96,$00,$6A,$00,$6A,$00,$96
LA50B:  .byte   $FE,$FE,$00,$01,$02,$01,$00,$FE
LA513:  .byte   $02,$02,$02,$02,$01,$01,$01,$01
LA51B:  lda     $0300,x
        and     #$0F
        beq     LA525
        jmp     LA5D8

LA525:  ldy     #$00
        jsr     LF67C
        bcs     LA535
LA52C:  lda     #$23
        jsr     LF835
        inc     $05A0,x
        rts

LA535:  lda     #$23
        cmp     $05C0,x
        bne     LA561
        lda     $05A0,x
        cmp     #$01
        bne     LA54B
        lda     #$02
        sta     $05A0,x
        jmp     LA612

LA54B:  lda     $0540,x
        beq     LA559
        dec     $0540,x
        lda     #$00
        sta     $05E0,x
        rts

LA559:  lda     $05E0,x
        cmp     #$04
        beq     LA561
        rts

LA561:  jsr     LF883
        lda     #$25
        cmp     $05C0,x
        beq     LA56E
        jsr     LF835
LA56E:  jsr     LA745
        bcc     LA577
        ldy     #$00
        beq     LA5C2
LA577:  lda     $0500,x
        tay
        lda     $04A0,x
        and     #$02
        beq     LA58B
        lda     LA6A4,y
        cmp     $0360,x
        bcs     LA594
        rts

LA58B:  lda     LA6A4,y
        cmp     $0360,x
        bcc     LA594
        rts

LA594:  inc     $0500,x
        lda     $0500,x
        and     #$03
        sta     $0500,x
        and     #$01
        bne     LA5A6
        jsr     LA3D6
LA5A6:  ldy     #$0F
LA5A8:  lda     $0310,y
        bpl     LA5B8
        lda     $0330,y
        cmp     #$BA
        bne     LA5B8
        ldy     #$00
        beq     LA5C2
LA5B8:  dey
        bne     LA5A8
        lda     $E4
        adc     $E6
        and     #$01
        tay
LA5C2:  lda     LA6A8,y
        jsr     LF835
        lda     LA6AA,y
        sta     $0440,x
        lda     LA6AC,y
        sta     $0460,x
        inc     $0300,x
        rts

LA5D8:  ldy     #$00
        jsr     LF67C
        bcs     LA5F8
        lda     $05C0,x
        cmp     #$24
        beq     LA5E9
        jsr     LA745
LA5E9:  lda     $0460,x
        bpl     LA612
        lda     #$24
        cmp     $05C0,x
        beq     LA618
        jmp     LA52C

LA5F8:  lda     $05C0,x
        cmp     #$24
        bne     LA604
        lda     #$1A
        sta     $0540,x
LA604:  lda     #$00
        sta     $0520,x
        dec     $0300,x
        jsr     LA52C
        inc     $05A0,x
LA612:  lda     #$00
        sta     $05E0,x
LA617:  rts

LA618:  lda     $0520,x
        beq     LA633
        dec     $0520,x
        lda     $05A0,x
        cmp     #$02
        bne     LA612
        lda     $05E0,x
        cmp     #$08
        bne     LA617
        dec     $05A0,x
        bne     LA612
LA633:  lda     #$02
        sta     $05A0,x
        jsr     LA612
        jsr     LA35D
        lda     #$14
        sta     $0520,x
        stx     L0000
        jsr     LFC53
        bcs     LA6A1
        lda     #$52
        jsr     LF846
        lda     #$CB
        sta     $0480,y
        lda     #$BA
        sta     $0320,y
        lda     #$44
        sta     $0440,y
        lda     #$03
        sta     $0460,y
        lda     $03C0,x
        sec
        sbc     #$04
        sta     $03C0,y
        lda     $0380,x
        sta     $0380,y
        lda     $0360,x
        sta     $0360,y
        lda     $04A0,x
        pha
        jsr     LF869
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tax
        lda     $0360,y
        clc
        adc     LA6AE,x
        sta     $0360,y
        lda     $0380,y
        adc     LA6AF,x
        sta     $0380,y
        ldx     L0000
        pla
        sta     $04A0,x
LA6A1:  ldx     L0000
        rts

LA6A4:  .byte   $80,$28,$80,$D8
LA6A8:  .byte   $23,$24
LA6AA:  .byte   $A8,$00
LA6AC:  .byte   $05,$08
LA6AE:  .byte   $1E
LA6AF:  .byte   $00,$E2,$FF
LA6B2:  lda     $0300,x
        and     #$0F
        cmp     #$02
        bne     LA6BE
        jmp     LA7F1

LA6BE:  cmp     #$01
        beq     LA6E1
        lda     #$3D
        sta     $0440,x
        lda     #$09
        sta     $0460,x
        inc     $0300,x
        lda     #$33
        jsr     LF835
        lda     $0500,x
        bne     LA6E1
        jsr     LA880
        lda     #$01
        sta     $0500,x
LA6E1:  lda     $0500,x
        bmi     LA715
        ldy     $0520,x
        lda     $0300,y
        bpl     LA715
        jsr     L8003
        bcs     LA715
        ldy     $0520,x
        lda     $04E0,x
        and     #$1F
        sta     $04E0,y
        cmp     #$0F
        bcs     LA715
        lda     $0480,y
        ora     #$40
        sta     $0480,y
        lda     #$00
        sta     $0300,x
        lda     #$80
        sta     $0500,y
        rts

LA715:  lda     $05C0,x
        cmp     #$35
        beq     LA795
        cmp     #$34
        bne     LA723
        jmp     LA7C7

LA723:  lda     $0500,x
        bmi     LA739
        lda     $0520,x
        tay
        lda     $05C0,y
        cmp     #$34
        bne     LA739
        lda     #$00
        sta     $05E0,x
LA738:  rts

LA739:  ldy     #$00
        jsr     LF67C
        bcs     LA756
        lda     #$00
        sta     $05E0,x
LA745:  lda     $04A0,x
        and     #$02
        beq     LA751
        ldy     #$01
        jmp     LF5C4

LA751:  ldy     #$00
        jmp     LF580

LA756:  lda     $05A0,x
        beq     LA738
        lda     $05E0,x
        cmp     #$04
        bne     LA738
        lda     #$35
        jsr     LF835
        jsr     LA3D6
        lda     #$28
        sta     $0360,x
        lda     $0500,x
        bpl     LA795
LA774:  lda     #$4C
        sta     $0400,x
        lda     #$01
        sta     $0420,x
        inc     $0300,x
        jsr     LF869
        jsr     LF883
LA787:  lda     $E6
        adc     $E7
        and     #$01
        tay
        lda     LA96C,y
        sta     $0500,x
        rts

LA795:  lda     $14
        and     #$40
        beq     LA7A6
        jsr     LF869
        jsr     LF883
        lda     #$34
        jmp     LF835

LA7A6:  jsr     LA745
        lda     #$D8
        cmp     $0360,x
        bcs     LA7C6
        sta     $0360,x
        lda     $0500,x
        bpl     LA7BB
        jmp     LA774

LA7BB:  dec     $0300,x
        jsr     LA3D6
        lda     #$33
        jmp     LF835

LA7C6:  rts

LA7C7:  lda     $05E0,x
        bne     LA7D6
        lda     $05A0,x
        cmp     #$01
        bne     LA7D6
        jmp     LA8D3

LA7D6:  lda     $05E0,x
        cmp     #$04
        bne     LA7C6
        lda     $05A0,x
        cmp     #$02
        bne     LA7C6
        lda     #$01
        sta     $04A0,x
        jsr     LF883
        lda     #$35
        jmp     LF835

LA7F1:  ldy     #$00
        jsr     LF67C
        bcs     LA800
        lda     #$00
        sta     $05E0,x
        jmp     LA745

LA800:  lda     $05C0,x
        cmp     #$34
        beq     LA854
        lda     $0500,x
        bne     LA81C
        lda     $0310
        bmi     LA81F
        jsr     LF869
        jsr     LF883
        lda     #$34
        jmp     LF835

LA81C:  dec     $0500,x
LA81F:  lda     $05C0,x
        cmp     #$35
        beq     LA837
        lda     $05A0,x
        beq     LA87F
        lda     $05E0,x
        cmp     #$04
        bne     LA87F
        lda     #$35
        jsr     LF835
LA837:  jsr     LA745
        bcc     LA83F
        jsr     LA3D6
LA83F:  lda     $14
        and     #$40
        beq     LA87F
        lda     #$AB
        sta     $0440,x
        lda     #$05
        sta     $0460,x
        lda     #$33
        jmp     LF835

LA854:  lda     $05E0,x
        bne     LA863
        lda     $05A0,x
        cmp     #$01
        bne     LA863
        jmp     LA92A

LA863:  lda     $05E0,x
        cmp     #$04
        bne     LA87F
        lda     $05A0,x
        cmp     #$02
        bne     LA87F
        jsr     LF869
        jsr     LF883
        jsr     LA787
        lda     #$35
        jmp     LF835

LA87F:  rts

LA880:  stx     L0000
        jsr     LFC53
        bcs     LA8D0
        tya
        sta     $0520,x
        txa
        sta     $0520,y
        lda     #$00
        sta     $0500,y
        lda     #$33
        jsr     LF846
        lda     #$01
        sta     $05A0,y
        lda     #$1C
        sta     $04E0,y
        lda     #$C0
        sta     $0300,y
        lda     #$8A
        sta     $0480,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $03E0,x
        sta     $03E0,y
        lda     #$D7
        sta     $0320,y
        lda     $04A0,x
        sta     $04A0,y
LA8D0:  ldx     L0000
        rts

LA8D3:  stx     L0000
        jsr     LFC53
        bcs     LA927
        lda     #$40
        sta     $0320,y
        lda     #$00
        sta     $0400,y
        lda     #$04
        sta     $0420,y
        lda     #$50
        jsr     LF846
LA8EE:  lda     #$8B
        sta     $0480,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $03E0,x
        sta     $03E0,y
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tax
        lda     $0360,y
        clc
        adc     LA96E,x
        sta     $0360,y
        lda     $0380,y
        adc     LA96F,x
        sta     $0380,y
LA927:  ldx     L0000
        rts

LA92A:  stx     L0000
        ldy     #$10
LA92E:  lda     $0300,y
        bmi     LA969
        lda     #$B9
        sta     $0320,y
        lda     #$00
        sta     $0400,y
        sta     $0440,y
        lda     #$03
        sta     $0420,y
        sta     $0460,y
        lda     #$B4
        sta     $0500,y
        lda     #$96
        sta     $0520,y
        ldx     L0000
        lda     #$4A
        jsr     LF846
        jsr     LA8EE
        lda     $0360,y
        and     #$FC
        sta     $0360,y
        iny
        cpy     #$13
        bcc     LA92E
LA969:  ldx     L0000
        rts

LA96C:  .byte   $B4,$FF
LA96E:  .byte   $0D
LA96F:  .byte   $00,$F3,$FF
LA972:  lda     $0300,x
        and     #$0F
        bne     LA99C
        lda     $0500,x
        bne     LA98F
        lda     $0520,x
        tay
        lda     $0360,y
        cmp     #$30
        bcc     LA98F
        lda     #$00
        sta     $05E0,x
        rts

LA98F:  lda     #$2D
        sta     $0400,x
        lda     #$03
        sta     $0420,x
        sta     $0500,x
LA99C:  jmp     LA6B2

        .byte   $2E,$2A,$AA,$EA,$5F,$A0,$11,$2A
        .byte   $88,$A0,$1F,$AA,$F7,$A2,$57,$A6
        .byte   $DA,$8A,$EB,$AA,$7B,$88,$D3,$2A
        .byte   $EF,$AC,$7C,$2A,$60,$AA,$3F,$2A
        .byte   $ED,$A0,$76,$20,$E8,$28,$B7,$AA
        .byte   $9A,$AA,$F6,$AA,$36,$20,$F3,$A8
        .byte   $DB,$0C,$7F,$A2,$B7,$AA,$37,$EA
        .byte   $FF,$AA,$DB,$A8,$D7,$A8,$E6,$2A
        .byte   $2B,$A8,$A6,$22,$B7,$2A,$F5,$A0
        .byte   $FF,$AA,$F9,$28,$57,$20,$DB,$08
        .byte   $3F,$00,$FF,$AA,$DA,$AA,$6D,$AA
        .byte   $E8,$2A,$05,$A0,$FF,$AA,$F8,$AA
        .byte   $F2,$00,$01,$02,$03,$04,$05,$06
        .byte   $07,$08,$09,$0A,$0B,$0C,$0D,$0E
        .byte   $0F,$10,$11,$12,$13,$14,$15,$16
        .byte   $BD,$AA,$F7,$AA,$F9,$20,$9A,$2A
        .byte   $DB,$28,$45,$A6,$FA,$A2,$8D,$1A
        .byte   $6F,$0A,$D7,$A0,$DE,$AA,$BF,$A8
        .byte   $AB,$14,$0C,$0C,$EF,$02,$BC,$A8
        .byte   $F2,$A2,$D7,$A8,$E9,$28,$5F,$08
        .byte   $E1,$40,$40,$40,$61,$40,$66,$67
        .byte   $20,$20,$00,$A0,$7F,$88,$EC,$A2
        .byte   $8E,$8A,$BA,$A0,$DE,$A8,$A7,$28
        .byte   $5C,$A2,$BE,$20,$7D,$AA,$FD,$8A
        .byte   $EB,$06,$00,$06,$00,$06,$00,$2D
        .byte   $00,$0E,$00,$08,$00,$30,$00,$00
        .byte   $00,$29,$00,$00,$7E,$28,$EB,$2A
        .byte   $FA,$2A,$EC,$8A,$B7,$2A,$FE,$0A
        .byte   $B5,$5C,$5E,$0F,$27,$26,$06,$0F
        .byte   $26,$16,$06,$0F,$0C,$09,$01,$0F
        .byte   $30,$28,$07,$8A,$00,$00,$00,$2A
        .byte   $7E,$A0,$B3,$20,$FF,$A0,$F7,$AA
        .byte   $F7,$20,$FE,$88,$60,$88,$DB,$22
        .byte   $A6,$0A,$F6,$2A,$EC,$A2,$FF,$82
        .byte   $3D,$A0,$8D,$AA,$2C,$AA,$DD,$02
        .byte   $EF,$88,$FF,$88,$EE,$A2,$E3,$AA
        .byte   $65,$2A,$8A,$2B,$94,$A2,$F3,$A8
        .byte   $7B,$AA,$75,$22,$C3,$88,$FE,$E8
        .byte   $D6,$A8,$F9,$AA,$6E,$A1,$F7,$A8
        .byte   $A4,$28,$DF,$02,$FF,$20,$F3,$A2
        .byte   $FD,$2A,$8E,$A8,$B7,$22,$97,$A8
        .byte   $7D,$0A,$EF,$AC,$A7,$2A,$7E,$A8
        .byte   $FE,$06,$07,$22,$9D,$22,$31,$A2
        .byte   $FF,$0D,$15,$FF,$9C,$AA,$77,$88
        .byte   $F9,$01,$03,$03,$04,$05,$06,$07
        .byte   $07,$07,$07,$08,$08,$08,$08,$09
        .byte   $09,$09,$0A,$0A,$0A,$0B,$0B,$0B
        .byte   $0C,$0C,$0C,$0D,$0D,$0D,$0D,$0D
        .byte   $0E,$0E,$0F,$0F,$10,$10,$11,$11
        .byte   $11,$12,$12,$12,$13,$14,$16,$FF
        .byte   $A0,$88,$FF,$88,$BE,$AA,$7A,$2A
        .byte   $F7,$A8,$D7,$A2,$FB,$20,$18,$28
        .byte   $F0,$A8,$AF,$2A,$D5,$2A,$B9,$0A
        .byte   $FD,$82,$B7,$28,$2F,$28,$E7,$AB
        .byte   $B3,$AA,$67,$22,$EF,$8A,$A7,$AA
        .byte   $BA,$AA,$DF,$AA,$5D,$AA,$F7,$AA
        .byte   $E3,$AA,$E9,$A2,$6E,$08,$EE,$A8
        .byte   $AC,$A0,$0F,$22,$CF,$82,$50,$EA
        .byte   $BE,$80,$CB,$28,$FB,$80,$47,$AA
        .byte   $F1,$2A,$76,$A0,$FF,$A8,$7F,$A8
        .byte   $2F,$22,$ED,$A0,$25,$2A,$5A,$A2
        .byte   $3F,$0A,$F7,$A0,$EE,$A8,$B1,$00
        .byte   $FF,$8A,$ED,$62,$ED,$AA,$5F,$A8
        .byte   $A7,$92,$F3,$A2,$FD,$AA,$6F,$A0
        .byte   $F7,$2A,$EA,$A2,$E1,$88,$10,$28
        .byte   $FA,$AA,$B7,$80,$75,$8A,$CF,$2A
        .byte   $89,$80,$37,$8A,$A2,$88,$7B,$AA
        .byte   $E2,$0A,$D7,$A8,$E6,$A2,$A9,$8A
        .byte   $5F,$1A,$2D,$88,$2E,$A8,$D9,$22
        .byte   $32,$8A,$5F,$2A,$97,$88,$7E,$AB
        .byte   $FE,$8A,$DD,$28,$77,$80,$4F,$AA
        .byte   $B0,$AA,$EF,$00,$3D,$2A,$EE,$BA
        .byte   $FF,$2A,$D6,$08,$44,$00,$3D,$A8
        .byte   $6E,$AA,$DD,$82,$06,$AA,$FB,$AA
        .byte   $E7,$28,$FD,$A2,$F1,$A0,$9D,$28
        .byte   $29,$AA,$38,$AA,$EF,$AA,$71,$A2
        .byte   $61,$80,$94,$F4,$A8,$30,$D0,$10
        .byte   $90,$B0,$D0,$10,$28,$A8,$D0,$48
        .byte   $68,$F0,$68,$88,$A8,$70,$C8,$F0
        .byte   $10,$B0,$C8,$50,$70,$90,$B0,$D0
        .byte   $78,$A8,$28,$E8,$B8,$E8,$48,$88
        .byte   $E8,$38,$88,$F8,$E8,$88,$C0,$FF
        .byte   $10,$00,$14,$0C,$00,$40,$40,$00
        .byte   $44,$8A,$04,$44,$30,$81,$53,$88
        .byte   $40,$08,$44,$00,$10,$84,$01,$00
        .byte   $00,$04,$04,$20,$00,$85,$11,$91
        .byte   $10,$34,$00,$04,$04,$50,$00,$E0
        .byte   $20,$52,$40,$80,$00,$CA,$41,$10
        .byte   $00,$84,$40,$09,$42,$1A,$14,$00
        .byte   $00,$11,$14,$85,$24,$01,$01,$22
        .byte   $40,$91,$44,$01,$50,$24,$10,$AC
        .byte   $00,$0C,$02,$00,$44,$06,$13,$32
        .byte   $40,$83,$00,$44,$44,$10,$00,$04
        .byte   $00,$20,$40,$41,$00,$24,$01,$C5
        .byte   $05,$04,$04,$20,$10,$94,$40,$02
        .byte   $00,$09,$11,$45,$10,$28,$00,$99
        .byte   $14,$29,$40,$00,$70,$C1,$04,$BC
        .byte   $04,$C1,$15,$20,$40,$06,$00,$21
        .byte   $41,$1B,$44,$32,$00,$41,$94,$80
        .byte   $34,$12,$55,$20,$54,$60,$02,$02
        .byte   $41,$41,$04,$A0,$04,$C0,$05,$13
        .byte   $00,$00,$00,$20,$10,$88,$40,$24
        .byte   $40,$94,$42,$08,$01,$01,$00,$80
        .byte   $40,$84,$04,$00,$10,$02,$10,$83
        .byte   $00,$14,$64,$D0,$51,$4A,$15,$66
        .byte   $55,$22,$51,$18,$05,$8A,$01,$51
        .byte   $44,$00,$10,$98,$44,$A6,$91,$8B
        .byte   $01,$89,$50,$CB,$40,$00,$10,$D0
        .byte   $4C,$84,$98,$B8,$84,$00,$78,$78
        .byte   $B8,$98,$68,$68,$20,$74,$78,$20
        .byte   $74,$58,$54,$20,$74,$78,$20,$74
        .byte   $58,$78,$20,$5C,$5C,$5C,$5C,$B8
        .byte   $B8,$98,$78,$58,$04,$04,$04,$04
        .byte   $04,$04,$04,$98,$18,$18,$00,$FF
        .byte   $40,$82,$00,$0A,$40,$A0,$10,$01
        .byte   $44,$21,$08,$41,$51,$86,$00,$40
        .byte   $74,$52,$50,$0A,$00,$A0,$00,$88
        .byte   $01,$00,$01,$02,$00,$72,$00,$00
        .byte   $05,$21,$00,$10,$11,$00,$50,$2A
        .byte   $04,$01,$10,$C2,$00,$21,$01,$80
        .byte   $41,$81,$41,$04,$00,$80,$04,$65
        .byte   $10,$A6,$04,$AC,$01,$1C,$00,$33
        .byte   $40,$F0,$05,$30,$44,$81,$02,$89
        .byte   $01,$A4,$00,$DC,$04,$BB,$04,$24
        .byte   $01,$88,$10,$01,$00,$40,$04,$00
        .byte   $00,$00,$00,$40,$00,$C8,$00,$23
        .byte   $01,$01,$10,$1B,$00,$60,$00,$80
        .byte   $35,$29,$00,$C0,$00,$02,$04,$C0
        .byte   $04,$18,$10,$51,$01,$04,$41,$F5
        .byte   $00,$90,$54,$A0,$00,$02,$01,$9C
        .byte   $00,$84,$00,$66,$10,$62,$44,$00
        .byte   $10,$8C,$11,$09,$00,$41,$50,$88
        .byte   $05,$08,$00,$5A,$04,$0A,$00,$80
        .byte   $10,$02,$00,$10,$00,$80,$00,$38
        .byte   $00,$21,$12,$14,$00,$68,$04,$21
        .byte   $00,$32,$00,$E9,$01,$12,$11,$54
        .byte   $04,$1F,$00,$2E,$11,$4B,$00,$C0
        .byte   $41,$2B,$00,$85,$11,$05,$1F,$28
        .byte   $00,$6D,$10,$B3,$10,$56,$14,$C2
        .byte   $45,$8E,$40,$31,$14,$23,$01,$4E
        .byte   $09,$02,$26,$26,$12,$3E,$36,$36
        .byte   $36,$36,$36,$36,$3A,$37,$36,$3A
        .byte   $37,$36,$37,$3A,$37,$36,$3A,$37
        .byte   $36,$36,$3A,$53,$53,$53,$53,$26
        .byte   $26,$26,$26,$26,$38,$38,$38,$38
        .byte   $38,$38,$38,$26,$08,$08,$4E,$FF
        .byte   $04,$08,$00,$91,$14,$31,$50,$2B
        .byte   $00,$2A,$41,$70,$10,$10,$00,$48
        .byte   $11,$28,$40,$20,$40,$D3,$00,$18
        .byte   $00,$00,$10,$00,$40,$21,$01,$44
        .byte   $00,$00,$00,$62,$10,$10,$04,$21
        .byte   $00,$30,$10,$A1,$02,$90,$00,$68
        .byte   $50,$98,$00,$8F,$45,$0A,$14,$22
        .byte   $40,$C6,$C0,$03,$51,$D8,$40,$E4
        .byte   $70,$42,$10,$28,$04,$54,$10,$83
        .byte   $00,$C2,$94,$84,$45,$BA,$80,$16
        .byte   $C0,$20,$01,$40,$00,$80,$00,$02
        .byte   $00,$64,$40,$18,$51,$2A,$00,$62
        .byte   $11,$35,$00,$80,$04,$08,$04,$40
        .byte   $04,$00,$41,$65,$10,$02,$00,$01
        .byte   $D9,$84,$10,$00,$00,$59,$10,$22
        .byte   $64,$82,$10,$C0,$00,$02,$01,$08
        .byte   $40,$C2,$00,$0A,$00,$0E,$45,$98
        .byte   $10,$1C,$00,$10,$20,$35,$01,$CA
        .byte   $10,$00,$00,$00,$40,$10,$00,$62
        .byte   $00,$E0,$10,$20,$01,$8A,$05,$40
        .byte   $00,$42,$90,$01,$18,$00,$04,$29
        .byte   $05,$50,$01,$00,$00,$30,$50,$54
        .byte   $48,$C5,$14,$26,$10,$ED,$45,$9A
        .byte   $00,$88,$85,$EA,$01,$98,$34,$52
        .byte   $15,$06,$14,$34,$45,$50,$14,$32
        .byte   $45,$00,$44,$73,$40,$38,$44,$89
        .byte   $40,$00,$01,$02,$03,$01,$02,$03
        .byte   $00,$04,$05,$05,$05,$05,$05,$05
        .byte   $04,$06,$07,$07,$07,$07,$07,$07
        .byte   $06,$08,$09,$09,$09,$09,$09,$09
        .byte   $08,$0A,$09,$09,$09,$09,$09,$09
        .byte   $0A,$00,$09,$09,$0B,$0B,$09,$09
        .byte   $00,$04,$09,$09,$0C,$0C,$09,$09
        .byte   $04,$06,$09,$09,$0D,$0D,$09,$09
        .byte   $06,$08,$0E,$0E,$0D,$0D,$0E,$0E
        .byte   $08,$0A,$0E,$0E,$0F,$0F,$0E,$0E
        .byte   $0A,$00,$0E,$0E,$10,$10,$0E,$0E
        .byte   $00,$04,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $04,$06,$0E,$0E,$11,$12,$0E,$0E
        .byte   $06,$13,$14,$14,$15,$16,$14,$14
        .byte   $13,$17,$18,$19,$0E,$0E,$18,$19
        .byte   $17,$1A,$1B,$1C,$0E,$0E,$1B,$1C
        .byte   $1A,$04,$1D,$1E,$09,$09,$1D,$1E
        .byte   $04,$06,$1F,$20,$09,$09,$1F,$20
        .byte   $06,$13,$01,$03,$09,$09,$01,$03
        .byte   $13,$17,$21,$21,$09,$09,$18,$19
        .byte   $17,$22,$23,$23,$0B,$0B,$1B,$1C
        .byte   $22,$24,$0E,$0E,$18,$19,$25,$26
        .byte   $24,$08,$0E,$0E,$1B,$1C,$1F,$20
        .byte   $08,$0A,$0E,$0E,$27,$28,$27,$28
        .byte   $0A,$17,$09,$09,$1F,$20,$25,$26
        .byte   $1A,$22,$09,$09,$01,$03,$01,$03
        .byte   $1A,$24,$09,$09,$29,$2A,$2B,$2C
        .byte   $2D,$08,$09,$09,$2E,$2F,$30,$31
        .byte   $32,$0A,$09,$09,$33,$34,$35,$36
        .byte   $37,$00,$09,$09,$38,$0C,$39,$3A
        .byte   $3B,$04,$3C,$3C,$3D,$3E,$18,$3F
        .byte   $19,$06,$40,$40,$41,$41,$42,$43
        .byte   $44,$45,$46,$47,$20,$25,$48,$49
        .byte   $17,$25,$47,$46,$49,$45,$4A,$4B
        .byte   $22,$01,$02,$02,$03,$01,$02,$03
        .byte   $24,$4C,$4D,$4E,$32,$4C,$4D,$32
        .byte   $08,$4F,$35,$50,$37,$4F,$35,$51
        .byte   $0A,$0C,$0C,$0C,$0C,$0C,$0C,$52
        .byte   $00,$24,$24,$24,$24,$24,$24,$3B
        .byte   $04,$08,$08,$0D,$0A,$0A,$0A,$3B
        .byte   $06,$24,$53,$0F,$54,$54,$54,$3B
        .byte   $08,$08,$0A,$32,$32,$4D,$21,$3B
        .byte   $0A,$0A,$54,$55,$34,$56,$3B,$3B
        .byte   $00,$00,$21,$57,$58,$59,$3A,$3B
        .byte   $04,$04,$3B,$5A,$5B,$5C,$5D,$3B
        .byte   $06,$06,$3B,$5A,$5E,$5F,$5D,$3B
        .byte   $13,$13,$60,$61,$18,$3F,$3F,$19
        .byte   $17,$17,$3B,$62,$42,$43,$43,$44
        .byte   $22,$00,$3B,$0A,$63,$64,$65,$66
        .byte   $67,$04,$3B,$54,$01,$02,$02,$03
        .byte   $0F,$06,$3B,$21,$21,$68,$69,$4D
        .byte   $21,$13,$3B,$6A,$50,$6B,$34,$35
        .byte   $50,$17,$18,$3F,$6C,$6D,$6E,$3F
        .byte   $19,$22,$1B,$6F,$6F,$70,$43,$43
        .byte   $44,$24,$25,$47,$46,$70,$47,$46
        .byte   $20,$08,$01,$02,$71,$72,$73,$02
        .byte   $03,$67,$09,$09,$09,$09,$74,$34
        .byte   $35,$0F,$09,$09,$09,$09,$2E,$31
        .byte   $39,$75,$09,$09,$09,$09,$76,$50
        .byte   $77,$78,$09,$09,$09,$09,$79,$7A
        .byte   $7B,$0C,$09,$09,$09,$09,$2E,$67
        .byte   $7C,$08,$09,$09,$09,$09,$7D,$7E
        .byte   $1D,$06,$3C,$3C,$3C,$3C,$7F,$80
        .byte   $81,$13,$40,$40,$40,$40,$41,$41
        .byte   $1F,$82,$09,$09,$83,$84,$34,$35
        .byte   $50,$82,$09,$09,$83,$30,$31,$39
        .byte   $3A,$82,$09,$09,$85,$35,$36,$77
        .byte   $4F,$86,$09,$09,$83,$39,$3A,$87
        .byte   $31,$88,$09,$09,$83,$89,$18,$19
        .byte   $3B,$1E,$09,$09,$18,$19,$1B,$1C
        .byte   $0C,$49,$3C,$3C,$1B,$1C,$1F,$49
        .byte   $08,$20,$40,$40,$1F,$49,$81,$26
        .byte   $06,$6B,$34,$35,$50,$78,$09,$09
        .byte   $09,$30,$31,$39,$3A,$82,$09,$09
        .byte   $09,$35,$36,$77,$4F,$8A,$09,$09
        .byte   $09,$39,$3A,$87,$31,$8B,$09,$09
        .byte   $0C,$89,$5D,$87,$0C,$8B,$09,$0C
        .byte   $08,$89,$4F,$0C,$08,$18,$19,$08
        .byte   $06,$18,$19,$08,$06,$1B,$1C,$06
        .byte   $13,$1B,$1C,$06,$13,$1F,$49,$13
        .byte   $17,$83,$84,$34,$35,$50,$6B,$34
        .byte   $35,$83,$30,$31,$39,$3A,$30,$31
        .byte   $39,$85,$35,$36,$77,$4F,$35,$8C
        .byte   $77,$83,$39,$3A,$0C,$31,$39,$67
        .byte   $87,$83,$89,$0C,$08,$18,$19,$53
        .byte   $87,$0C,$89,$08,$06,$1B,$1C,$0A
        .byte   $0C,$08,$8D,$06,$13,$1F,$20,$8E
        .byte   $8F,$0D,$6F,$13,$17,$25,$49,$7F
        .byte   $7F,$50,$82,$09,$09,$09,$09,$83
        .byte   $6B,$3A,$82,$09,$09,$09,$09,$83
        .byte   $30,$4F,$8A,$09,$09,$09,$09,$85
        .byte   $35,$3B,$8B,$09,$09,$09,$09,$83
        .byte   $39,$31,$8B,$09,$18,$19,$09,$83
        .byte   $0C,$3B,$8B,$0C,$1B,$1C,$18,$19
        .byte   $08,$60,$90,$08,$1F,$20,$1B,$1C
        .byte   $06,$6F,$6F,$06,$25,$49,$1F,$20
        .byte   $13,$34,$35,$50,$6B,$91,$92,$31
        .byte   $13,$31,$39,$3A,$30,$31,$93,$3B
        .byte   $17,$36,$77,$4F,$35,$36,$94,$3B
        .byte   $22,$0C,$87,$31,$39,$3A,$95,$3B
        .byte   $24,$08,$0C,$96,$89,$18,$19,$3B
        .byte   $08,$06,$08,$18,$19,$1B,$1C,$3B
        .byte   $0A,$13,$06,$1B,$1C,$25,$49,$3B
        .byte   $00,$17,$13,$27,$97,$27,$97,$3B
        .byte   $04,$00,$54,$01,$03,$01,$03,$3B
        .byte   $24,$67,$32,$98,$99,$21,$9A,$3B
        .byte   $08,$24,$9B,$9B,$39,$50,$9C,$3B
        .byte   $0A,$08,$9B,$9D,$9E,$9E,$9E,$9E
        .byte   $54,$0A,$55,$2C,$99,$21,$68,$9F
        .byte   $99,$00,$3B,$3B,$39,$3A,$87,$31
        .byte   $39,$67,$A0,$A0,$A1,$18,$3F,$3F
        .byte   $19,$24,$6F,$6F,$0D,$1B,$6F,$6F
        .byte   $44,$18,$19,$24,$25,$A2,$49,$08
        .byte   $1F,$1B,$1C,$08,$1F,$A2,$26,$0A
        .byte   $01,$1F,$26,$0A,$01,$02,$03,$54
        .byte   $4D,$01,$03,$54,$4D,$21,$A3,$9F
        .byte   $35,$21,$68,$9F,$35,$50,$77,$3B
        .byte   $39,$3A,$87,$31,$39,$38,$18,$3F
        .byte   $19,$18,$3F,$3F,$19,$67,$1B,$6F
        .byte   $1C,$1B,$6F,$6F,$1C,$24,$01,$02
        .byte   $03,$A2,$26,$0A,$01,$02,$03,$54
        .byte   $3B,$02,$03,$54,$4D,$21,$A3,$4C
        .byte   $3B,$21,$A3,$9F,$35,$50,$77,$4F
        .byte   $A4,$50,$77,$31,$39,$38,$18,$19
        .byte   $38,$38,$18,$3F,$19,$67,$1B,$1C
        .byte   $67,$67,$1B,$6F,$1C,$24,$01,$03
        .byte   $24,$24,$01,$02,$03,$08,$18,$19
        .byte   $08,$A5,$18,$3F,$19,$0A,$1B,$1C
        .byte   $0A,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$A7,$A6,$A7,$A6,$09,$3B
        .byte   $A8,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$3B,$A6,$3B,$A6,$09,$3B
        .byte   $A6,$09,$A9,$AA,$A9,$AA,$09,$A9
        .byte   $AA,$AB,$AC,$AD,$AE,$AF,$AB,$AC
        .byte   $AD,$B0,$B1,$B0,$B1,$B0,$B0,$B1
        .byte   $B0,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$3B,$A8,$09,$A7,$A6,$A7,$A6
        .byte   $09,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$3B,$A6,$09,$3B,$A6,$3B,$A6
        .byte   $09,$A9,$AA,$09,$A9,$AA,$A9,$AA
        .byte   $09,$AE,$AF,$AB,$AC,$B2,$AE,$AF
        .byte   $AB,$B1,$B0,$B0,$B1,$64,$B1,$B0
        .byte   $B0,$3B,$A6,$3B,$A6,$09,$5D,$84
        .byte   $34,$3B,$A6,$3B,$A6,$09,$5D,$30
        .byte   $31,$3B,$A8,$3B,$A8,$09,$4F,$35
        .byte   $36,$3B,$A6,$3B,$A6,$09,$3B,$39
        .byte   $3A,$3B,$A6,$3B,$A6,$09,$3B,$89
        .byte   $5D,$A9,$AA,$A9,$AA,$09,$3B,$89
        .byte   $38,$AC,$B2,$AE,$AF,$AB,$18,$19
        .byte   $67,$B1,$B0,$B1,$B0,$B0,$1B,$1C
        .byte   $24,$B3,$3B,$84,$34,$B3,$3B,$84
        .byte   $34,$39,$3A,$30,$31,$39,$3A,$30
        .byte   $31,$77,$4F,$35,$36,$77,$4F,$35
        .byte   $36,$87,$3B,$39,$3A,$87,$3B,$39
        .byte   $3A,$87,$3B,$89,$18,$3F,$19,$18
        .byte   $3F,$18,$3F,$19,$1B,$6F,$1C,$1B
        .byte   $6F,$1B,$6F,$1C,$01,$02,$03,$01
        .byte   $02,$25,$B4,$26,$18,$3F,$19,$18
        .byte   $3F,$B3,$3B,$84,$34,$B3,$3B,$3B
        .byte   $54,$39,$3A,$30,$31,$39,$3A,$3B
        .byte   $B5,$77,$4F,$35,$36,$77,$5D,$3B
        .byte   $B6,$87,$3B,$39,$3A,$38,$18,$3F
        .byte   $19,$19,$18,$3F,$19,$67,$1B,$6F
        .byte   $1C,$1C,$1B,$6F,$1C,$24,$01,$02
        .byte   $03,$03,$01,$02,$03,$08,$18,$3F
        .byte   $19,$19,$18,$3F,$19,$0A,$1B,$6F
        .byte   $1C,$01,$02,$02,$03,$01,$02,$71
        .byte   $B7,$B8,$21,$21,$21,$21,$21,$21
        .byte   $B5,$B9,$A7,$3B,$3B,$3B,$A7,$3B
        .byte   $B6,$18,$6C,$6D,$6E,$19,$18,$6C
        .byte   $BA,$1B,$6F,$70,$6F,$1C,$1B,$6F
        .byte   $BB,$01,$71,$72,$73,$03,$01,$71
        .byte   $BC,$18,$6C,$6D,$6E,$19,$18,$6C
        .byte   $BD,$1B,$6F,$70,$6F,$1C,$1B,$6F
        .byte   $BE,$BF,$73,$02,$03,$01,$02,$71
        .byte   $C0,$C1,$C2,$32,$C3,$C4,$C2,$21
        .byte   $C5,$C6,$C7,$C8,$C9,$96,$C7,$CA
        .byte   $CB,$CC,$CD,$CE,$CF,$5D,$CD,$CE
        .byte   $D0,$D1,$C9,$96,$87,$D2,$C9,$D3
        .byte   $D4,$D5,$89,$5D,$87,$D6,$89,$95
        .byte   $D7,$D8,$6E,$3F,$19,$18,$3F,$6C
        .byte   $BD,$D9,$6F,$6F,$1C,$1B,$6F,$6F
        .byte   $BE,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A
        .byte   $4A,$1D,$1E,$25,$26,$7C,$7D,$1B
        .byte   $05,$6E,$6E,$05,$05,$7E,$7F,$05
        .byte   $1C,$09,$0A,$8E,$8F,$34,$34,$61
        .byte   $61,$86,$87,$11,$12,$69,$69,$14
        .byte   $15,$11,$12,$8E,$8F,$16,$17,$17
        .byte   $16,$19,$1A,$25,$26,$16,$17,$24
        .byte   $23,$01,$02,$09,$0A,$11,$12,$11
        .byte   $12,$17,$16,$16,$17,$19,$1A,$2A
        .byte   $2B,$36,$35,$16,$17,$17,$23,$16
        .byte   $70,$24,$16,$71,$17,$8E,$8F,$19
        .byte   $1A,$17,$16,$23,$24,$17,$35,$16
        .byte   $17,$36,$16,$16,$17,$25,$26,$1D
        .byte   $1E,$0C,$04,$6C,$65,$04,$0D,$66
        .byte   $6F,$25,$26,$25,$26,$74,$07,$74
        .byte   $0F,$06,$77,$0E,$77,$74,$0F,$74
        .byte   $00,$0E,$77,$10,$77,$74,$00,$74
        .byte   $08,$00,$77,$00,$77,$34,$34,$33
        .byte   $33,$25,$26,$09,$0A,$61,$61,$15
        .byte   $14,$8E,$8F,$86,$87,$74,$00,$74
        .byte   $00,$10,$77,$00,$77,$74,$00,$7C
        .byte   $7D,$10,$77,$7E,$7F,$38,$34,$2F
        .byte   $33,$55,$34,$43,$33,$34,$57,$4C
        .byte   $50,$34,$56,$47,$40,$25,$26,$2A
        .byte   $2B,$2F,$33,$2F,$33,$54,$52,$32
        .byte   $48,$43,$4F,$43,$4F,$33,$33,$32
        .byte   $33,$34,$56,$33,$48,$2F,$33,$2F
        .byte   $4C,$33,$48,$47,$40,$44,$45,$41
        .byte   $42,$46,$46,$33,$33,$46,$53,$33
        .byte   $33,$01,$02,$1D,$1E,$49,$4A,$4E
        .byte   $4F,$4B,$52,$48,$48,$33,$33,$33
        .byte   $33,$04,$04,$3B,$3C,$80,$81,$8C
        .byte   $8D,$82,$83,$8C,$8D,$04,$04,$6D
        .byte   $6D,$30,$31,$0E,$0F,$8A,$8B,$8A
        .byte   $8B,$74,$06,$74,$0E,$07,$06,$0F
        .byte   $0E,$07,$77,$0F,$77,$74,$08,$74
        .byte   $10,$00,$08,$00,$00,$00,$08,$00
        .byte   $08,$10,$00,$00,$08,$08,$77,$10
        .byte   $77,$00,$00,$00,$00,$08,$77,$00
        .byte   $77,$56,$56,$48,$48,$55,$57,$43
        .byte   $4F,$34,$34,$32,$33,$48,$48,$58
        .byte   $40,$46,$46,$33,$32,$46,$53,$33
        .byte   $32,$4B,$51,$58,$47,$8E,$8F,$11
        .byte   $12,$1D,$1E,$2A,$2B,$33,$48,$33
        .byte   $58,$43,$4F,$41,$42,$33,$33,$33
        .byte   $59,$33,$33,$46,$46,$49,$4A,$4D
        .byte   $4F,$33,$43,$33,$43,$59,$46,$43
        .byte   $33,$53,$4F,$33,$4F,$48,$48,$48
        .byte   $48,$43,$33,$43,$33,$33,$4F,$33
        .byte   $4F,$04,$04,$34,$34,$0B,$02,$1D
        .byte   $1E,$09,$0A,$19,$1A,$74,$0E,$74
        .byte   $00,$0F,$0E,$00,$08,$0F,$0E,$00
        .byte   $00,$0F,$77,$10,$77,$09,$0A,$11
        .byte   $12,$56,$57,$48,$4F,$34,$56,$32
        .byte   $48,$33,$59,$33,$43,$53,$4F,$4C
        .byte   $50,$04,$04,$6D,$66,$0D,$0C,$6F
        .byte   $6C,$04,$04,$65,$6D,$06,$07,$0E
        .byte   $0F,$77,$74,$77,$74,$6E,$7E,$05
        .byte   $05,$7F,$7C,$1C,$1B,$7D,$6E,$05
        .byte   $05,$2F,$48,$2F,$58,$56,$38,$48
        .byte   $2F,$2F,$59,$2F,$43,$4D,$4F,$48
        .byte   $4F,$53,$2F,$33,$2F,$2F,$43,$2F
        .byte   $54,$33,$33,$01,$02,$48,$4F,$0C
        .byte   $04,$6C,$65,$74,$07,$01,$02,$80
        .byte   $81,$8E,$8F,$82,$83,$2D,$2E,$8A
        .byte   $8B,$8A,$8B,$8C,$8D,$74,$08,$74
        .byte   $00,$33,$2F,$33,$2F,$2F,$48,$2F
        .byte   $48,$33,$4F,$4C,$50,$2F,$48,$2F
        .byte   $5C,$33,$2F,$04,$0D,$48,$4F,$48
        .byte   $4F,$66,$6F,$06,$77,$4E,$4F,$4E
        .byte   $4F,$33,$2F,$5A,$2F,$5B,$2F,$5B
        .byte   $2F,$46,$46,$01,$02,$04,$04,$5E
        .byte   $57,$1D,$1E,$80,$81,$11,$12,$82
        .byte   $83,$04,$04,$34,$38,$33,$33,$47
        .byte   $47,$4E,$33,$5F,$33,$4E,$33,$4E
        .byte   $33,$4D,$33,$48,$33,$48,$33,$48
        .byte   $33,$33,$33,$4B,$52,$08,$77,$7E
        .byte   $7F,$34,$56,$33,$5C,$56,$57,$41
        .byte   $42,$56,$34,$48,$33,$33,$48,$33
        .byte   $48,$53,$33,$33,$33,$0C,$04,$1B
        .byte   $05,$04,$04,$05,$05,$56,$56,$58
        .byte   $40,$04,$04,$00,$00,$0B,$02,$09
        .byte   $0A,$08,$00,$10,$00,$5E,$57,$4E
        .byte   $4F,$33,$33,$5A,$33,$11,$12,$19
        .byte   $1A,$33,$17,$33,$16,$32,$32,$33
        .byte   $33,$32,$17,$33,$16,$03,$03,$00
        .byte   $00,$03,$17,$00,$16,$23,$24,$30
        .byte   $31,$70,$71,$06,$07,$3D,$24,$06
        .byte   $31,$3E,$70,$06,$07,$71,$24,$06
        .byte   $31,$0E,$0F,$08,$00,$0F,$0F,$00
        .byte   $00,$3D,$24,$06,$30,$4E,$4F,$41
        .byte   $42,$10,$00,$00,$00,$34,$27,$33
        .byte   $27,$33,$27,$33,$27,$7F,$1D,$1C
        .byte   $2A,$72,$34,$7A,$33,$7A,$33,$7A
        .byte   $33,$0D,$01,$6F,$1D,$77,$09,$77
        .byte   $11,$7F,$8E,$1C,$86,$0D,$11,$6F
        .byte   $8E,$77,$19,$77,$25,$1E,$7C,$2B
        .byte   $1B,$7F,$86,$1C,$86,$72,$56,$7A
        .byte   $48,$34,$57,$33,$4F,$55,$57,$44
        .byte   $45,$34,$56,$46,$53,$34,$11,$33
        .byte   $8E,$7A,$48,$7A,$48,$4C,$50,$43
        .byte   $4F,$47,$40,$33,$33,$41,$42,$49
        .byte   $4A,$47,$47,$33,$33,$47,$19,$33
        .byte   $25,$02,$48,$1E,$48,$43,$4F,$44
        .byte   $45,$32,$33,$46,$46,$4E,$4F,$4D
        .byte   $4F,$33,$1D,$46,$25,$0A,$40,$12
        .byte   $33,$58,$40,$33,$33,$33,$33,$4B
        .byte   $51,$33,$09,$51,$11,$8F,$33,$87
        .byte   $33,$32,$33,$33,$33,$33,$8E,$33
        .byte   $86,$12,$0C,$8F,$6C,$1A,$74,$26
        .byte   $74,$08,$00,$00,$00,$44,$45,$01
        .byte   $02,$11,$12,$86,$87,$8E,$8F,$00
        .byte   $00,$1B,$05,$00,$00,$05,$05,$00
        .byte   $00,$1C,$1B,$00,$00,$05,$1C,$00
        .byte   $00,$19,$1A,$00,$00,$0E,$0F,$00
        .byte   $00,$8A,$8B,$00,$00,$74,$08,$00
        .byte   $00,$00,$77,$00,$00,$10,$77,$00
        .byte   $00,$74,$00,$00,$00,$77,$74,$00
        .byte   $00,$10,$08,$00,$00,$11,$12,$00
        .byte   $00,$3D,$24,$30,$31,$3E,$70,$30
        .byte   $07,$3D,$24,$31,$30,$3E,$70,$31
        .byte   $07,$77,$19,$77,$25,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$8C,$8E,$6B,$05,$06,$08
        .byte   $0A,$00,$A8,$AA,$23,$AC,$05,$28
        .byte   $2A,$00,$34,$47,$8E,$0C,$0E,$2C
        .byte   $2E,$00,$A4,$A6,$AE,$06,$9C,$7C
        .byte   $00,$34,$47,$3A,$2C,$2E,$9C,$9E
        .byte   $4E,$B4,$B6,$9C,$9E,$00,$99,$9E
        .byte   $45,$4A,$4C,$6E,$6B,$00,$01,$03
        .byte   $5E,$00,$36,$38,$01,$03,$00,$00
        .byte   $5E,$6B,$30,$32,$6B,$6B,$5B,$55
        .byte   $6B,$6B,$50,$52,$49,$6B,$55,$6B
        .byte   $5B,$5B,$5D,$5D,$55,$6B,$00,$00
        .byte   $00,$6B,$6B,$6B,$5A,$6B,$69,$00
        .byte   $6B,$6B,$79,$7A,$00,$00,$74,$76
        .byte   $00,$6B,$89,$9A,$00,$72,$76,$00
        .byte   $70,$C0,$05,$00,$74,$82,$00,$00
        .byte   $26,$C2,$06,$6B,$00,$82,$00,$00
        .byte   $80,$C4,$C6,$C8,$CA,$00,$00,$99
        .byte   $47,$00,$00,$99,$9E,$99,$9E,$99
        .byte   $DB,$70,$72,$74,$76,$78,$00,$00
        .byte   $00,$70,$72,$74,$76,$78,$00,$00
        .byte   $00,$98,$9A,$9C,$9C,$9E,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$E8,$EA,$EC,$EC,$EE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$8D,$8F,$6B,$05,$06,$09
        .byte   $0B,$10,$A9,$AB,$8D,$05,$AD,$29
        .byte   $2B,$10,$35,$48,$24,$0D,$0F,$2D
        .byte   $2F,$00,$A5,$A7,$06,$AF,$7B,$9F
        .byte   $00,$35,$48,$3B,$2D,$2F,$9D,$9F
        .byte   $4F,$B5,$B7,$9D,$9F,$00,$9D,$CB
        .byte   $46,$4B,$4D,$6F,$6B,$00,$02,$04
        .byte   $5F,$00,$37,$39,$02,$04,$00,$00
        .byte   $5F,$54,$31,$33,$5C,$56,$55,$55
        .byte   $6B,$54,$51,$53,$59,$6B,$58,$5A
        .byte   $6B,$6B,$5D,$65,$66,$67,$00,$00
        .byte   $00,$54,$64,$6B,$6B,$54,$6B,$00
        .byte   $5A,$78,$79,$6B,$00,$00,$75,$77
        .byte   $00,$98,$89,$6B,$00,$73,$76,$00
        .byte   $71,$05,$C1,$00,$22,$27,$00,$00
        .byte   $81,$06,$C3,$6B,$00,$83,$00,$00
        .byte   $81,$C5,$C7,$C9,$CB,$00,$00,$35
        .byte   $48,$00,$00,$9D,$CB,$9D,$CB,$DA
        .byte   $48,$71,$73,$75,$77,$79,$00,$00
        .byte   $00,$71,$73,$75,$77,$79,$00,$00
        .byte   $00,$99,$9B,$9D,$9D,$9F,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$E9,$EB,$ED,$ED,$EF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$9C,$9E,$44,$15,$16,$18
        .byte   $1A,$20,$B8,$BA,$9C,$BC,$15,$00
        .byte   $00,$00,$34,$47,$9E,$1C,$1E,$1E
        .byte   $1C,$00,$B4,$B6,$BE,$16,$9C,$9E
        .byte   $00,$A4,$A6,$3A,$3C,$3E,$9C,$9E
        .byte   $4E,$9C,$9E,$11,$13,$00,$99,$7C
        .byte   $45,$18,$1A,$7E,$6B,$6B,$1E,$1C
        .byte   $5E,$45,$36,$38,$3C,$3E,$00,$6D
        .byte   $5E,$55,$40,$42,$6B,$6B,$5B,$6B
        .byte   $55,$6B,$60,$62,$6B,$6B,$6B,$6B
        .byte   $5B,$5B,$6B,$6B,$6B,$6B,$6B,$6B
        .byte   $5B,$6B,$6B,$69,$5A,$6B,$5A,$6B
        .byte   $55,$6B,$89,$8A,$00,$00,$84,$00
        .byte   $00,$6B,$89,$6B,$00,$82,$00,$85
        .byte   $80,$D0,$15,$6B,$84,$82,$00,$00
        .byte   $80,$D2,$16,$6B,$86,$92,$86,$85
        .byte   $90,$D4,$D6,$D8,$9E,$00,$00,$B0
        .byte   $B2,$00,$00,$99,$9E,$A0,$A2,$99
        .byte   $47,$80,$82,$84,$86,$88,$00,$00
        .byte   $00,$80,$82,$84,$86,$88,$00,$00
        .byte   $00,$A8,$AA,$AC,$AC,$AE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$F8,$FA,$FC,$FC,$FE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$9D,$9F,$44,$15,$16,$19
        .byte   $1B,$00,$B9,$BB,$9D,$15,$BD,$21
        .byte   $00,$20,$35,$48,$9F,$1D,$1F,$1F
        .byte   $1D,$00,$B5,$B7,$16,$BF,$9D,$9F
        .byte   $00,$A5,$A7,$3B,$3D,$3F,$9D,$9F
        .byte   $4F,$9D,$9F,$12,$14,$00,$7B,$CB
        .byte   $46,$19,$1B,$7F,$6B,$6B,$1F,$1D
        .byte   $5F,$46,$37,$39,$3D,$3F,$6C,$00
        .byte   $5F,$57,$41,$43,$5C,$5A,$6B,$6B
        .byte   $55,$54,$61,$63,$54,$64,$54,$5A
        .byte   $6B,$55,$6B,$54,$6B,$6B,$5C,$54
        .byte   $6B,$68,$5C,$6B,$6B,$6A,$6B,$5A
        .byte   $5A,$88,$89,$6B,$00,$00,$00,$00
        .byte   $00,$6B,$89,$6B,$00,$83,$00,$85
        .byte   $81,$15,$D1,$6B,$00,$83,$00,$00
        .byte   $81,$16,$D3,$6B,$87,$93,$85,$87
        .byte   $91,$D5,$D7,$D9,$CB,$00,$00,$B1
        .byte   $B3,$00,$00,$9D,$CB,$A1,$A3,$35
        .byte   $48,$81,$83,$85,$87,$89,$00,$00
        .byte   $00,$81,$83,$85,$87,$89,$00,$00
        .byte   $00,$A9,$AB,$AD,$AD,$AF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$F9,$FB,$FD,$FD,$FF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$02,$13,$13,$01
        .byte   $01,$01,$13,$13,$13,$13,$13,$01
        .byte   $01,$01,$13,$13,$13,$01,$01,$01
        .byte   $01,$00,$13,$13,$13,$13,$13,$13
        .byte   $00,$13,$13,$02,$01,$01,$13,$13
        .byte   $13,$13,$13,$13,$13,$00,$13,$13
        .byte   $02,$01,$01,$02,$02,$02,$01,$01
        .byte   $43,$02,$12,$12,$01,$01,$01,$01
        .byte   $23,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$10,$10,$13,$13
        .byte   $10,$02,$02,$02,$10,$13,$13,$13
        .byte   $13,$13,$13,$12,$13,$13,$10,$10
        .byte   $13,$13,$13,$12,$13,$13,$13,$13
        .byte   $13,$13,$13,$13,$13,$00,$00,$13
        .byte   $13,$00,$00,$13,$13,$13,$13,$13
        .byte   $13,$00,$00,$00,$00,$00,$00,$00
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
        .byte   $00
