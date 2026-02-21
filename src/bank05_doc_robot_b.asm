; =============================================================================
; MEGA MAN 3 (U) — BANK $05 — DOC ROBOT AI (BUBBLE/HEAT/QUICK/AIR)
; =============================================================================
; Doc Robot AI routines mimicking Bubble Man, Heat Man, Quick Man, and Air Man.
;
; Annotation: 0% — unannotated da65 output
; =============================================================================

        .setcpu "6502"

L0000           := $0000
L8003           := $8003
LF580           := $F580
LF588           := $F588
LF5C4           := $F5C4
LF5CC           := $F5CC
LF606           := $F606
LF642           := $F642
LF67C           := $F67C
LF71D           := $F71D
LF73B           := $F73B
LF759           := $F759
LF779           := $F779
LF835           := $F835
LF846           := $F846
LF869           := $F869
LF883           := $F883
LF8B3           := $F8B3
LF8C2           := $F8C2
LFC53           := $FC53
LFC63           := $FC63
LFCEB           := $FCEB
LFD11           := $FD11

.segment "BANK05"

        jmp     LA1DD

        jmp     LA022

        jmp     LA393

        jmp     LA561

        jmp     LA1B4

        jmp     LA360

        jmp     LA50E

        jmp     LA68E

        jmp     LA796

        jmp     LA7BE

        jmp     LA87D

        rts

LA022:  lda     $0300,x
        and     #$0F
        tay
        lda     LA035,y
        sta     L0000
        lda     LA037,y
        sta     $01
        jmp     (L0000)

LA035:  .byte   $39,$AF
LA037:  .byte   $A0,$A0
        jsr     LF869
        jsr     LF883
        jsr     L8003
        bcs     LA06E
        lda     $04E0,x
        beq     LA090
        inc     $0300,x
        lda     #$07
        jsr     LF835
        lda     #$AA
        sta     $0480,x
        lda     $E5
        adc     $E4
        sta     $E4
        sta     L0000
        lda     #$03
        sta     $01
        jsr     LFCEB
        ldy     $03
        lda     LA1A6,y
        sta     $0500,x
        rts

LA06E:  lda     $05C0,x
        cmp     #$02
        bne     LA091
        lda     $05E0,x
        bne     LA0AE
        lda     $05A0,x
        cmp     #$01
        bne     LA084
        jsr     LA12C
LA084:  lda     $05A0,x
        cmp     #$03
        bne     LA0AE
        lda     #$01
        jsr     LF835
LA090:  rts

LA091:  lda     #$00
        sta     $05E0,x
        ldy     #$1F
LA098:  lda     $0300,y
        bpl     LA0A4
        lda     $05C0,y
        cmp     #$0A
        beq     LA0AE
LA0A4:  dey
        cpy     #$0F
        bne     LA098
        lda     #$02
        jsr     LF835
LA0AE:  rts

        lda     $0500,x
        beq     LA0C8
        dec     $0500,x
        bne     LA12B
        lda     #$80
        sta     $0480,x
        lda     #$08
        jsr     LF835
        lda     #$01
        sta     $0540,x
LA0C8:  lda     $05C0,x
        cmp     #$09
        beq     LA0FF
        lda     $05E0,x
        cmp     #$06
        bne     LA12B
        lda     $05A0,x
        cmp     $0540,x
        bne     LA12B
        lda     $0540,x
        tay
        lda     LA1A9,y
        jsr     LF835
        lda     $0540,x
        bne     LA0F6
        dec     $0300,x
        lda     #$CA
        sta     $0480,x
        rts

LA0F6:  lda     $0360
        sta     $0520,x
        jsr     LF869
LA0FF:  lda     $04A0,x
        and     #$02
        beq     LA113
        jsr     LF73B
        lda     $0360,x
        cmp     $0520,x
        bcs     LA12B
        bcc     LA11E
LA113:  jsr     LF71D
        lda     $0360,x
        cmp     $0520,x
        bcc     LA12B
LA11E:  lda     #$08
        jsr     LF835
        inc     $05A0,x
        lda     #$00
        sta     $0540,x
LA12B:  rts

LA12C:  jsr     LF8C2
        clc
        adc     #$40
        sta     $0C
        stx     $0E
        lda     #$02
        sta     $0F
LA13A:  jsr     LFC53
        bcs     LA1A3
        ldx     $0F
        lda     LA1AB,x
        sta     $0440,y
        lda     LA1AE,x
        sta     $0460,y
        lda     LA1B1,x
        sta     $03
        lda     #$00
        sta     $02
        sta     L0000
        lda     $0C
        sec
        sbc     #$20
        bcs     LA161
        lda     #$00
LA161:  sta     $01
        sta     $0C
        sty     $0D
        jsr     LFD11
        ldy     $0D
        lda     $04
        sta     $0400,y
        lda     $05
        sta     $0420,y
        ldx     $0E
        lda     #$0A
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     #$B4
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $04A0,x
        sta     $04A0,y
        dec     $0F
        bpl     LA13A
LA1A3:  ldx     $0E
        rts

LA1A6:  .byte   $1E,$3C,$5A
LA1A9:  .byte   $01,$09
LA1AB:  .byte   $00,$88,$54
LA1AE:  .byte   $04,$06,$08
LA1B1:  .byte   $1C,$2A,$34
LA1B4:  ldy     #$12
        jsr     LF67C
        bcs     LA1DC
        lda     $04A0,x
        and     #$02
        beq     LA1CA
        ldy     #$1F
        jsr     LF5C4
        jmp     LA1CF

LA1CA:  ldy     #$1E
        jsr     LF580
LA1CF:  bcs     LA1D7
        lda     #$00
        sta     $05E0,x
        rts

LA1D7:  lda     #$00
        sta     $0300,x
LA1DC:  rts

LA1DD:  lda     $0300,x
        and     #$0F
        bne     LA1F8
        lda     #$01
        cmp     $05C0,x
        beq     LA252
        ldy     $05A0,x
        cpy     #$02
        bne     LA1DC
        jsr     LF835
        jmp     LA252

LA1F8:  jsr     LA252
        lda     $0560,x
        and     #$F0
        bne     LA21B
        lda     $03C0,x
        cmp     #$50
        bcs     LA23C
        lda     #$00
        sta     $0440,x
        lda     #$01
        sta     $0460,x
        lda     $0560,x
        ora     #$10
        sta     $0560,x
LA21B:  ldy     #$1E
        jsr     LF606
        bcc     LA1DC
        dec     $0300,x
        lda     #$00
        sta     $0540,x
        sta     $0560,x
        lda     $05A0,x
        and     #$01
        clc
        adc     #$01
        jsr     LF835
        inc     $05A0,x
        rts

LA23C:  ldy     #$1F
        jsr     LF642
        lda     $0560,x
        and     #$01
        bne     LA24D
        ldy     #$21
        jmp     LF5CC

LA24D:  ldy     #$20
        jmp     LF588

LA252:  jsr     LF869
        jsr     LF883
        lda     $05A0,x
        bne     LA262
        lda     #$00
        sta     $05E0,x
LA262:  lda     $0520,x
        beq     LA26B
        dec     $0520,x
        rts

LA26B:  lda     $0300,x
        and     #$0F
        tay
        lda     $0540,x
        beq     LA2B6
        dec     $0540,x
        beq     LA28D
        lda     LA354,y
        sta     $0520,x
        lda     #$00
        sta     $05E0,x
        lda     #$01
        sta     $05A0,x
        bne     LA2DD
LA28D:  tya
        bne     LA2DC
        inc     $0300,x
        lda     $03C0
        pha
        lda     #$50
        sta     $03C0
        lda     #$60
        sta     $02
        lda     #$01
        sta     $03
        jsr     LFC63
        lda     $04A0,x
        sta     $0560,x
        pla
        sta     $03C0
        lda     #$1C
        jmp     LF835

LA2B6:  jsr     LF8B3
        cmp     LA35A,y
        bcs     LA2DC
        tya
        beq     LA2C5
        lda     #$05
        bne     LA2D9
LA2C5:  lda     $E6
        adc     $E7
        sta     $E6
        sta     L0000
        lda     #$03
        sta     $01
        jsr     LFCEB
        lda     $03
        clc
        adc     #$02
LA2D9:  sta     $0540,x
LA2DC:  rts

LA2DD:  jsr     LFC53
        bcs     LA2DC
        stx     L0000
        lda     $0300,x
        and     #$0F
        tax
        stx     $01
        lda     LA34C,x
        sta     $0440,y
        lda     LA34E,x
        sta     $0460,y
        lda     LA350,x
        sta     $0400,y
        lda     LA352,x
        sta     $0420,y
        lda     LA356,x
        sta     $0320,y
        lda     LA358,x
        ldx     L0000
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $04A0,x
        sta     $04A0,y
        ldx     $01
        beq     LA349
        and     #$02
        tax
        lda     $0360,y
        clc
        adc     LA35C,x
        sta     $0360,y
        lda     $0380,y
        adc     LA35D,x
        sta     $0380,y
LA349:  ldx     L0000
        rts

LA34C:  .byte   $AB,$00
LA34E:  .byte   $05,$00
LA350:  .byte   $A8,$00
LA352:  .byte   $01,$04
LA354:  .byte   $1E,$14
LA356:  .byte   $B5,$41
LA358:  .byte   $11,$58
LA35A:  .byte   $05,$07
LA35C:  .byte   $1A
LA35D:  .byte   $00,$E6,$FF
LA360:  lda     $95
        and     #$01
        bne     LA392
        ldy     #$08
        jsr     LF67C
        bcc     LA377
        lda     #$A8
        sta     $0440,x
        lda     #$05
        sta     $0460,x
LA377:  lda     $04A0,x
        and     #$02
        beq     LA386
        ldy     #$09
        jsr     LF5C4
        jmp     LA38B

LA386:  ldy     #$08
        jsr     LF580
LA38B:  bcc     LA392
        lda     #$00
        sta     $0300,x
LA392:  rts

LA393:  lda     $0300,x
        and     #$0F
        tay
        lda     LA4ED,y
        sta     L0000
        lda     LA4F0,y
        sta     $01
        jmp     (L0000)

        lda     $E4
        adc     $E5
        sta     $E6
        and     #$03
        tay
        lda     LA503,y
        sta     $0500,x
        lda     LA507,y
        sta     $0540,x
        inc     $0300,x
        lda     $0460,x
        sta     $0F
        ldy     #$1E
        jsr     LF67C
        bcc     LA43D
        jsr     LF869
        jsr     LF883
        dec     $0500,x
        bpl     LA3D8
        jmp     LA422

LA3D8:  lda     $E4
        adc     $E6
        sta     $E7
        and     #$03
        tay
        lda     LA4F7,y
        sta     $0440,x
        lda     LA4FB,y
        sta     $0460,x
        lda     LA4FF,y
        sta     $03
        lda     #$00
        sta     L0000
        sta     $02
        jsr     LF8C2
        sta     $01
        clc
        adc     LA4F3,y
        sta     $04
        lda     LA4F3,y
        bpl     LA40E
        bcc     LA414
        lda     $04
        sta     $01
LA40E:  bcs     LA414
        lda     $04
        sta     $01
LA414:  jsr     LFD11
        lda     $04
        sta     $0400,x
        lda     $05
        sta     $0420,x
        rts

LA422:  inc     $0300,x
        lda     #$00
        sta     $0400,x
        lda     #$02
        sta     $0420,x
        lda     #$3C
        sta     $0520,x
        lda     #$01
        jsr     LF835
        inc     $05A0,x
LA43C:  rts

LA43D:  lda     $0500,x
        cmp     $0540,x
        bne     LA451
        lda     $0F
        bmi     LA451
        lda     $0460,x
        bpl     LA451
        jsr     LA498
LA451:  lda     $04A0,x
        and     #$02
        beq     LA45D
        ldy     #$21
        jmp     LF5C4

LA45D:  ldy     #$20
        jmp     LF580

        lda     $0520,x
        beq     LA485
        dec     $0520,x
        lda     $05C0,x
        cmp     #$05
        beq     LA451
        lda     $05A0,x
        bne     LA43C
        lda     $05E0,x
        cmp     #$03
        bcc     LA43C
        lda     #$05
        jsr     LF835
        jmp     LA451

LA485:  jsr     LF869
        jsr     LF883
        lda     $0300,x
        and     #$F0
        sta     $0300,x
        lda     #$03
        jmp     LF835

LA498:  stx     $0E
        lda     #$02
        sta     $0F
LA49E:  jsr     LFC53
        bcs     LA4EA
        ldx     $0E
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$25
        sta     $0520,y
        lda     #$00
        sta     $0500,y
        lda     #$0F
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     #$B6
        sta     $0320,y
        ldx     $0F
        lda     $03C0
        pha
        clc
        adc     LA50B,x
        sta     $03C0
        tya
        tax
        jsr     LA518
        pla
        sta     $03C0
        dec     $0F
        bpl     LA49E
LA4EA:  ldx     $0E
        rts

LA4ED:  .byte   $A6,$BE,$62
LA4F0:  .byte   $A3,$A3,$A4
LA4F3:  .byte   $C0,$00,$40,$00
LA4F7:  .byte   $88,$15,$3D,$15
LA4FB:  .byte   $06,$08,$09,$08
LA4FF:  .byte   $27,$30,$37,$30
LA503:  .byte   $03,$02,$02,$01
LA507:  .byte   $01,$01,$00,$00
LA50B:  .byte   $E8,$00,$18
LA50E:  lda     $0500,x
        beq     LA529
        dec     $0500,x
        bne     LA553
LA518:  lda     #$00
        sta     $02
        lda     #$04
        sta     $03
        jsr     LFC63
        lda     $0C
        sta     $04A0,x
        rts

LA529:  lda     $0520,x
        beq     LA539
        dec     $0520,x
        bne     LA539
        lda     #$1F
        sta     $0500,x
        rts

LA539:  lda     $04A0,x
        and     #$08
        beq     LA546
        jsr     LF779
        jmp     LA549

LA546:  jsr     LF759
LA549:  lda     $0580,x
        bmi     LA554
        lda     #$00
        sta     $0300,x
LA553:  rts

LA554:  lda     $04A0,x
        and     #$02
        beq     LA55E
        jmp     LF73B

LA55E:  jmp     LF71D

LA561:  lda     $0300,x
        and     #$0F
        beq     LA56B
        jmp     LA632

LA56B:  lda     $05A0,x
        beq     LA57C
        lda     $05E0,x
        cmp     #$08
        bcc     LA553
        lda     #$00
        sta     $05A0,x
LA57C:  sta     $05E0,x
        lda     $0540,x
        beq     LA593
        dec     $0540,x
        lda     $0540,x
        cmp     #$1E
        bne     LA553
        lda     #$00
        sta     $36
        rts

LA593:  lda     $0520,x
        beq     LA5A6
        dec     $0520,x
        bne     LA5AB
        lda     #$03
        jsr     LF835
        inc     $0300,x
        rts

LA5A6:  lda     #$03
        sta     $0520,x
LA5AB:  lda     $E4
        adc     $E6
        sta     $E4
        sta     L0000
        lda     #$05
        sta     $01
        jsr     LFCEB
        lda     $03
        asl     a
        sta     L0000
        asl     a
        clc
        adc     L0000
        sta     L0000
        lda     #$96
        sta     $0540,x
        inc     $05A0,x
        lda     #$05
        sta     $01
        stx     $02
LA5D3:  jsr     LFC53
        bcs     LA62F
        ldx     $03
        lda     LA6FB,x
        sta     $0520,y
        ldx     L0000
        lda     LA700,x
        sta     $0440,y
        lda     LA71E,x
        sta     $0460,y
        lda     LA73C,x
        sta     $0400,y
        lda     LA75A,x
        sta     $0420,y
        lda     LA778,x
        sta     $0500,y
        ldx     $02
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $04A0,x
        sta     $04A0,y
        lda     #$10
        jsr     LF846
        lda     #$A0
        sta     $0480,y
        lda     #$B7
        sta     $0320,y
        inc     L0000
        dec     $01
        bpl     LA5D3
LA62F:  ldx     $02
        rts

LA632:  ldy     #$1E
        jsr     LF67C
        bcs     LA63C
        jmp     LA554

LA63C:  lda     $0520,x
        cmp     #$02
        beq     LA660
        tay
        lda     LA6F3,y
        sta     $0440,x
        lda     LA6F5,y
        sta     $0460,x
        lda     LA6F7,y
        sta     $0400,x
        lda     LA6F9,y
        sta     $0420,x
        inc     $0520,x
        rts

LA660:  dec     $0300,x
        lda     #$00
        sta     $0520,x
        lda     $0580,x
        eor     #$40
        sta     $0580,x
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        and     #$02
        beq     LA680
        lda     #$C8
        bne     LA682
LA680:  lda     #$38
LA682:  sta     $0360,x
        lda     #$01
        jsr     LF835
        inc     $05A0,x
        rts

LA68E:  lda     $0500,x
        beq     LA6AF
        dec     $0500,x
        dec     $0520,x
        lda     $03A0,x
        sec
        sbc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        sbc     $0460,x
        sta     $03C0,x
        jmp     LA554

LA6AF:  lda     $0520,x
        beq     LA6C2
        dec     $0520,x
        beq     LA6BA
        rts

LA6BA:  lda     #$00
        sta     $0400,x
        sta     $0420,x
LA6C2:  lda     $0400,x
        clc
        adc     #$10
        sta     $0400,x
        lda     $0420,x
        adc     #$00
        sta     $0420,x
        cmp     #$04
        bcc     LA6E1
        lda     #$04
        sta     $0420,x
        lda     #$00
        sta     $0400,x
LA6E1:  lda     $04A0,x
        sta     $36
        lda     $0400,x
        sta     $37
        lda     $0420,x
        sta     $38
        jmp     LA554

LA6F3:  .byte   $A8,$A4
LA6F5:  .byte   $05,$08
LA6F7:  .byte   $6A,$DA
LA6F9:  .byte   $01,$01
LA6FB:  .byte   $44,$4A,$42,$43,$43
LA700:  .byte   $00,$F0,$50,$3C,$00,$00,$D3,$CD
        .byte   $68,$0F,$1A,$00,$A7,$68,$00,$7F
        .byte   $B1,$A7,$88,$50,$D4,$D0,$D0,$B9
        .byte   $98,$50,$3C,$1A,$7C,$35
LA71E:  .byte   $04,$03,$03,$02,$02,$00,$03,$03
        .byte   $02,$02,$01,$00,$03,$02,$02,$01
        .byte   $00,$FF,$03,$03,$02,$01,$01,$FF
        .byte   $03,$03,$02,$01,$00,$00
LA73C:  .byte   $00,$B1,$3C,$50,$76,$00,$2B,$3C
        .byte   $31,$6B,$DB,$00,$A0,$31,$76,$B5
        .byte   $F0,$FC,$E0,$3C,$D4,$90,$90,$FD
        .byte   $C0,$3C,$50,$DB,$F8,$FE
LA75A:  .byte   $00,$00,$02,$03,$03,$04,$01,$01
        .byte   $03,$03,$03,$04,$01,$03,$03,$03
        .byte   $03,$03,$01,$02,$02,$03,$03,$03
        .byte   $01,$02,$03,$03,$03,$03
LA778:  .byte   $0C,$16,$24,$0E,$24,$18,$1B,$0E
        .byte   $1E,$2A,$1D,$0C,$0D,$0A,$20,$15
        .byte   $22,$18,$21,$15,$05,$0D,$23,$1C
        .byte   $1A,$0E,$1C,$1D,$10,$24
LA796:  lda     $04A0,x
        and     #$08
        beq     LA7A3
        jsr     LF779
        jmp     LA7A6

LA7A3:  jsr     LF759
LA7A6:  lda     $0580,x
        bmi     LA7B1
        lda     #$00
        sta     $0300,x
        rts

LA7B1:  lda     $04A0,x
        and     #$02
        beq     LA7BB
        jmp     LF73B

LA7BB:  jmp     LF71D

LA7BE:  lda     $0500,x
        beq     LA7CF
        dec     $0500,x
        lda     $0500,x
        cmp     LA86A,x
        bcc     LA7CF
        rts

LA7CF:  lda     $04A0,x
        and     #$01
        beq     LA7F1
        lda     $0500,x
        beq     LA7E3
        ldy     #$1E
        jsr     LF580
        jmp     LA80C

LA7E3:  lda     $0580,x
        ora     #$40
        sta     $0580,x
        jsr     LF71D
        jmp     LA833

LA7F1:  lda     $0500,x
        beq     LA7FE
        ldy     #$1F
        jsr     LF5C4
        jmp     LA80C

LA7FE:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        jsr     LF73B
        jmp     LA833

LA80C:  bcc     LA833
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
        bne     LA879
        lda     $04A0,x
        ora     #$08
        sta     $04A0,x
        rts

LA833:  lda     $04A0,x
        and     #$0C
        beq     LA879
        and     #$04
        beq     LA858
        lda     $0500,x
        bne     LA84B
        lda     #$4B
        sta     $05C0,x
        jmp     LF759

LA84B:  ldy     #$12
        jsr     LF606
        lda     #$4B
        sta     $05C0,x
        jmp     LA86F

LA858:  lda     $0500,x
        bne     LA865
        lda     #$4C
        sta     $05C0,x
        jmp     LF779

LA865:  ldy     #$13
        jsr     LF642
LA86A:  lda     #$4C
        sta     $05C0,x
LA86F:  bcc     LA879
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
LA879:  rts

        .byte   $B4,$B2,$B0
LA87D:  lda     $0300,x
        and     #$0F
        bne     LA8A9
        ldy     #$12
        jsr     LF67C
        bcc     LA8F5
        lda     #$00
        sta     $0400,x
        sta     $0440,x
        lda     #$02
        sta     $0420,x
        sta     $0460,x
        jsr     LF869
        lda     $04A0,x
        ora     #$04
        sta     $04A0,x
        inc     $0300,x
LA8A9:  lda     $04A0,x
        and     #$08
        bne     LA8BD
        ldy     #$12
        jsr     LF606
        lda     #$53
        sta     $05C0,x
        jmp     LA8C7

LA8BD:  ldy     #$13
        jsr     LF642
        lda     #$54
        sta     $05C0,x
LA8C7:  bcs     LA8F6
        lda     $04A0,x
        and     #$0C
        tay
        lda     $04A0,x
        pha
        cpy     #$08
        beq     LA8DC
        eor     #$03
        sta     $04A0,x
LA8DC:  lda     $05C0,x
        pha
        jsr     LA908
        pla
        sta     $05C0,x
        pla
        sta     $04A0,x
        bcs     LA92B
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
LA8F5:  rts

LA8F6:  lda     $04A0,x
        and     #$08
        beq     LA908
        lda     #$59
        jsr     LF835
        lda     #$00
        sta     $0320,x
        rts

LA908:  lda     #$52
        sta     $05C0,x
        lda     $04A0,x
        and     #$01
        beq     LA91C
        ldy     #$1E
        jsr     LF580
        jmp     LA921

LA91C:  ldy     #$1F
        jsr     LF5C4
LA921:  bcc     LA92B
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
LA92B:  rts

        .byte   $AA,$6F,$0A,$6F,$80,$DD,$AA,$B0
        .byte   $A2,$5F,$0A,$DC,$A8,$D3,$2A,$EF
        .byte   $AA,$7F,$AA,$E6,$A8,$BF,$2A,$BC
        .byte   $8A,$DF,$2A,$4F,$22,$95,$00,$77
        .byte   $A2,$1D,$AB,$70,$AA,$63,$8A,$ED
        .byte   $82,$CF,$A2,$4F,$2A,$ED,$AA,$EA
        .byte   $A8,$CE,$EA,$F7,$28,$26,$A3,$BC
        .byte   $2A,$5B,$82,$57,$AA,$7F,$AA,$57
        .byte   $8A,$8F,$28,$26,$AB,$D0,$A6,$FF
        .byte   $A2,$FF,$02,$D8,$AA,$FA,$A0,$73
        .byte   $AA,$D2,$AA,$FE,$A8,$B5,$0A,$19
        .byte   $AA,$4F,$A8,$79,$82,$52,$A8,$9E
        .byte   $AA,$EF,$A8,$C1,$8A,$EA,$A0,$7F
        .byte   $A2,$FA,$88,$FF,$22,$FE,$A8,$FB
        .byte   $AA,$FE,$C8,$DF,$28,$D5,$20,$FC
        .byte   $0A,$82,$A2,$E4,$AA,$FF,$80,$B2
        .byte   $AA,$CF,$E8,$82,$82,$9F,$AA,$91
        .byte   $28,$5C,$8A,$BB,$8A,$FE,$28,$D9
        .byte   $AA,$DD,$A0,$7B,$80,$66,$A2,$68
        .byte   $82,$ED,$28,$8E,$A8,$0C,$22,$4F
        .byte   $22,$EF,$A8,$52,$20,$FC,$A2,$47
        .byte   $A8,$63,$A0,$19,$8A,$D7,$A8,$D1
        .byte   $2A,$A8,$AA,$7A,$8A,$F5,$2A,$CC
        .byte   $02,$6E,$2A,$CF,$8A,$F6,$A0,$78
        .byte   $AA,$DF,$88,$EF,$88,$FB,$82,$AE
        .byte   $A8,$AF,$A0,$DF,$AA,$F4,$AA,$FF
        .byte   $AA,$56,$AA,$FF,$00,$01,$02,$03
        .byte   $04,$1A,$06,$07,$08,$09,$0A,$0B
        .byte   $0C,$0D,$1A,$0F,$10,$11,$12,$13
        .byte   $14,$15,$16,$17,$18,$19,$A8,$F9
        .byte   $88,$FF,$AA,$FE,$20,$CF,$2A,$33
        .byte   $AA,$5D,$42,$2E,$20,$62,$02,$77
        .byte   $A6,$F3,$0A,$05,$17,$1E,$1E,$26
        .byte   $A2,$2F,$20,$EA,$AA,$3A,$22,$D1
        .byte   $AA,$DE,$28,$F7,$23,$80,$A0,$21
        .byte   $62,$80,$80,$80,$A0,$22,$80,$A4
        .byte   $20,$20,$00,$C4,$88,$71,$0A,$5D
        .byte   $A2,$F1,$AA,$D5,$20,$7F,$28,$7B
        .byte   $A8,$C7,$80,$CF,$15,$00,$15,$00
        .byte   $2C,$00,$15,$00,$01,$00,$17,$00
        .byte   $17,$00,$17,$00,$2C,$00,$0C,$00
        .byte   $06,$00,$06,$00,$06,$00,$26,$00
        .byte   $00,$6F,$2A,$37,$40,$42,$0F,$39
        .byte   $19,$09,$0F,$37,$10,$00,$0F,$19
        .byte   $09,$21,$0F,$20,$31,$21,$80,$00
        .byte   $00,$00,$88,$DE,$02,$B3,$08,$FD
        .byte   $08,$BA,$22,$5F,$28,$5F,$02,$4C
        .byte   $22,$9D,$20,$2F,$2A,$FD,$AA,$D5
        .byte   $AA,$48,$80,$3C,$AA,$A2,$A2,$C3
        .byte   $AA,$EF,$AA,$4B,$6A,$F0,$A0,$6E
        .byte   $82,$E3,$8A,$A9,$2A,$DF,$A2,$6D
        .byte   $0A,$89,$08,$4C,$28,$16,$2A,$44
        .byte   $AA,$FB,$8A,$6F,$00,$F7,$0A,$9F
        .byte   $22,$4A,$AA,$33,$2A,$C3,$AA,$78
        .byte   $88,$5F,$AA,$1A,$84,$9A,$A2,$5E
        .byte   $82,$59,$20,$F9,$A0,$DD,$22,$4E
        .byte   $A8,$D3,$2A,$77,$09,$0C,$A2,$FE
        .byte   $AA,$7F,$80,$BF,$0F,$18,$FF,$D9
        .byte   $8A,$F8,$AA,$57,$01,$01,$01,$02
        .byte   $02,$02,$03,$04,$04,$04,$05,$05
        .byte   $05,$05,$05,$05,$05,$05,$05,$05
        .byte   $05,$06,$06,$06,$07,$07,$07,$07
        .byte   $07,$09,$09,$09,$0A,$0A,$0A,$0B
        .byte   $0C,$0D,$0D,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0F,$10
        .byte   $11,$11,$11,$12,$14,$14,$14,$15
        .byte   $15,$15,$15,$15,$15,$16,$16,$16
        .byte   $16,$17,$17,$17,$17,$17,$19,$FF
        .byte   $AA,$FA,$A2,$D1,$8A,$42,$A2,$F3
        .byte   $82,$C3,$A0,$B3,$AA,$FF,$CA,$E7
        .byte   $20,$D7,$A8,$BF,$A8,$3C,$22,$4E
        .byte   $08,$F2,$08,$AD,$2A,$43,$A8,$7D
        .byte   $A2,$FF,$82,$EF,$A0,$DF,$02,$7F
        .byte   $AA,$09,$A0,$DD,$8E,$76,$A2,$32
        .byte   $2A,$33,$8A,$FE,$20,$1E,$82,$47
        .byte   $80,$87,$8A,$F6,$2A,$4C,$A6,$77
        .byte   $A0,$F5,$0A,$4B,$AA,$F5,$2A,$7B
        .byte   $8A,$7E,$A8,$C3,$A2,$FE,$82,$7B
        .byte   $A0,$57,$E8,$FF,$08,$47,$82,$67
        .byte   $2A,$25,$88,$8A,$C8,$C7,$8E,$AC
        .byte   $8A,$61,$24,$87,$80,$B6,$2A,$BA
        .byte   $2A,$F9,$22,$CF,$AA,$11,$A2,$E8
        .byte   $82,$94,$8A,$5D,$32,$4A,$EA,$97
        .byte   $2A,$4F,$88,$B3,$AA,$BF,$8D,$B7
        .byte   $AA,$61,$8A,$EF,$AA,$FA,$2A,$3C
        .byte   $E2,$EB,$2A,$5E,$C8,$E5,$AA,$73
        .byte   $0A,$1F,$AA,$CE,$AA,$6E,$22,$36
        .byte   $A8,$EE,$29,$4E,$AA,$7D,$A2,$5F
        .byte   $A8,$B5,$A2,$F8,$AA,$7A,$A8,$FD
        .byte   $A8,$F9,$AA,$F6,$82,$7D,$AA,$79
        .byte   $A8,$1F,$AA,$FF,$28,$48,$E8,$18
        .byte   $A8,$F8,$B8,$58,$78,$98,$68,$78
        .byte   $88,$98,$A8,$B8,$C8,$D8,$E4,$D0
        .byte   $D8,$A8,$C8,$E8,$48,$78,$88,$A8
        .byte   $D8,$90,$B0,$E0,$30,$50,$A0,$A8
        .byte   $6C,$98,$B8,$68,$78,$88,$98,$A8
        .byte   $B8,$C8,$D8,$E4,$D0,$D8,$E8,$38
        .byte   $28,$48,$D8,$78,$18,$78,$D8,$38
        .byte   $58,$98,$B8,$D8,$F8,$18,$58,$88
        .byte   $B8,$38,$58,$78,$98,$C8,$D0,$FF
        .byte   $80,$00,$00,$00,$00,$04,$80,$00
        .byte   $95,$14,$60,$10,$4B,$10,$94,$40
        .byte   $10,$01,$10,$40,$0A,$11,$1D,$01
        .byte   $98,$51,$CA,$40,$A9,$01,$06,$00
        .byte   $18,$55,$A0,$11,$08,$44,$30,$10
        .byte   $06,$44,$30,$30,$11,$00,$2C,$04
        .byte   $40,$45,$A1,$00,$40,$00,$54,$04
        .byte   $00,$00,$00,$00,$20,$08,$00,$00
        .byte   $00,$C1,$82,$10,$0B,$00,$01,$04
        .byte   $00,$44,$00,$40,$81,$00,$00,$15
        .byte   $20,$00,$45,$41,$CA,$00,$58,$11
        .byte   $16,$00,$00,$01,$A2,$41,$C0,$10
        .byte   $94,$40,$92,$54,$00,$00,$04,$91
        .byte   $A1,$40,$66,$41,$0C,$01,$98,$00
        .byte   $04,$11,$1D,$10,$08,$00,$60,$00
        .byte   $28,$00,$19,$11,$0C,$00,$01,$40
        .byte   $8E,$15,$05,$00,$04,$10,$40,$14
        .byte   $24,$00,$01,$40,$4C,$50,$88,$04
        .byte   $46,$14,$23,$80,$EE,$04,$C0,$00
        .byte   $3C,$51,$44,$10,$CE,$15,$83,$50
        .byte   $69,$50,$B8,$44,$40,$40,$41,$54
        .byte   $65,$44,$82,$40,$00,$11,$81,$81
        .byte   $38,$80,$AC,$44,$70,$54,$B0,$94
        .byte   $74,$50,$B0,$54,$74,$94,$90,$98
        .byte   $90,$88,$90,$98,$90,$88,$50,$27
        .byte   $3F,$28,$68,$88,$38,$98,$38,$B4
        .byte   $88,$38,$B4,$48,$68,$B4,$48,$80
        .byte   $60,$B8,$B8,$90,$98,$90,$88,$90
        .byte   $98,$90,$88,$50,$27,$3F,$74,$74
        .byte   $28,$B4,$B4,$50,$B8,$98,$58,$B8
        .byte   $48,$38,$98,$68,$78,$28,$48,$68
        .byte   $B8,$A8,$48,$68,$68,$88,$00,$FF
        .byte   $94,$00,$26,$00,$00,$28,$30,$00
        .byte   $9A,$00,$03,$00,$A0,$00,$39,$00
        .byte   $01,$00,$0E,$00,$68,$10,$25,$04
        .byte   $22,$44,$01,$01,$32,$15,$49,$14
        .byte   $02,$50,$62,$02,$80,$00,$C8,$52
        .byte   $C9,$21,$11,$00,$0D,$00,$86,$44
        .byte   $95,$00,$91,$30,$12,$90,$00,$00
        .byte   $09,$10,$45,$00,$82,$01,$00,$04
        .byte   $1A,$01,$00,$04,$08,$40,$08,$40
        .byte   $A0,$40,$20,$00,$02,$00,$02,$00
        .byte   $15,$15,$7E,$40,$62,$04,$08,$01
        .byte   $E4,$00,$10,$01,$0A,$10,$09,$50
        .byte   $87,$14,$0A,$00,$80,$10,$88,$00
        .byte   $02,$01,$AE,$40,$A0,$00,$6C,$40
        .byte   $00,$15,$56,$10,$01,$00,$84,$00
        .byte   $4D,$00,$02,$50,$08,$05,$0C,$10
        .byte   $00,$00,$A2,$00,$04,$00,$C1,$50
        .byte   $04,$01,$89,$01,$50,$0C,$B0,$00
        .byte   $02,$10,$00,$4A,$00,$00,$E1,$00
        .byte   $8C,$00,$19,$00,$B3,$55,$60,$45
        .byte   $0C,$41,$C0,$50,$6A,$40,$81,$00
        .byte   $17,$04,$2D,$50,$0E,$40,$B0,$08
        .byte   $43,$01,$88,$41,$00,$60,$00,$60
        .byte   $60,$00,$00,$60,$60,$60,$66,$66
        .byte   $66,$66,$66,$66,$66,$66,$67,$70
        .byte   $71,$01,$61,$61,$50,$01,$50,$60
        .byte   $01,$01,$04,$01,$01,$04,$01,$03
        .byte   $03,$56,$56,$66,$66,$66,$66,$66
        .byte   $66,$66,$66,$67,$70,$71,$04,$04
        .byte   $50,$04,$04,$05,$0B,$0B,$0B,$0B
        .byte   $06,$0B,$06,$06,$0B,$06,$06,$06
        .byte   $0B,$06,$06,$06,$06,$06,$4C,$FF
        .byte   $66,$20,$06,$11,$12,$11,$52,$01
        .byte   $D3,$00,$F0,$00,$08,$41,$00,$00
        .byte   $C2,$00,$08,$00,$44,$40,$10,$40
        .byte   $A1,$01,$20,$11,$C8,$10,$48,$C0
        .byte   $18,$01,$20,$01,$0A,$05,$72,$10
        .byte   $CC,$41,$40,$14,$91,$08,$6C,$04
        .byte   $8C,$43,$23,$41,$00,$00,$40,$00
        .byte   $43,$40,$1B,$40,$00,$04,$40,$44
        .byte   $14,$04,$0C,$04,$00,$11,$C5,$01
        .byte   $32,$40,$10,$01,$20,$40,$04,$10
        .byte   $09,$40,$82,$01,$22,$00,$18,$01
        .byte   $02,$00,$D9,$41,$D1,$94,$42,$C1
        .byte   $88,$44,$04,$40,$44,$50,$8C,$52
        .byte   $00,$10,$D8,$40,$03,$01,$20,$50
        .byte   $20,$44,$13,$90,$A9,$50,$0D,$35
        .byte   $01,$00,$58,$50,$00,$44,$00,$04
        .byte   $04,$04,$04,$40,$40,$02,$10,$01
        .byte   $02,$10,$20,$51,$40,$00,$46,$90
        .byte   $49,$00,$68,$10,$00,$40,$42,$15
        .byte   $18,$15,$32,$04,$09,$00,$80,$10
        .byte   $49,$15,$24,$44,$A2,$55,$E8,$05
        .byte   $40,$40,$95,$11,$84,$51,$D3,$05
        .byte   $CA,$40,$D2,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$02,$02,$02
        .byte   $02,$02,$02,$02,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$04,$05
        .byte   $00,$00,$00,$00,$00,$01,$06,$03
        .byte   $00,$00,$00,$00,$07,$03,$08,$03
        .byte   $00,$00,$00,$00,$09,$0A,$00,$0B
        .byte   $02,$02,$05,$01,$0C,$00,$00,$00
        .byte   $00,$00,$03,$03,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$07,$0D,$00,$00,$00,$00
        .byte   $00,$04,$0E,$0F,$10,$00,$00,$00
        .byte   $01,$11,$12,$13,$14,$02,$02,$02
        .byte   $0A,$00,$00,$15,$15,$0D,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$16,$03,$00,$00,$00,$00
        .byte   $00,$00,$16,$03,$00,$00,$00,$00
        .byte   $00,$00,$16,$03,$00,$00,$17,$18
        .byte   $00,$00,$16,$03,$00,$00,$19,$1A
        .byte   $18,$00,$00,$03,$02,$02,$0A,$1B
        .byte   $1C,$00,$00,$03,$1D,$1D,$1D,$1D
        .byte   $03,$1D,$1D,$1E,$00,$00,$00,$00
        .byte   $03,$00,$00,$03,$03,$1F,$0B,$02
        .byte   $02,$02,$02,$05,$03,$0C,$00,$00
        .byte   $00,$00,$00,$03,$1E,$20,$00,$00
        .byte   $00,$00,$00,$03,$03,$21,$22,$00
        .byte   $00,$00,$00,$03,$03,$23,$21,$22
        .byte   $00,$00,$00,$03,$24,$25,$23,$21
        .byte   $22,$00,$00,$03,$0A,$26,$26,$27
        .byte   $28,$1D,$29,$03,$26,$26,$26,$26
        .byte   $26,$26,$2A,$03,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$31,$03,$00,$00,$00
        .byte   $00,$00,$00,$32,$1E,$29,$01,$02
        .byte   $02,$02,$02,$0A,$03,$1F,$03,$00
        .byte   $17,$33,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$00,$00,$00,$00
        .byte   $00,$00,$37,$38,$00,$00,$00,$00
        .byte   $00,$00,$39,$3A,$00,$00,$00,$00
        .byte   $00,$00,$3B,$3C,$00,$00,$01,$05
        .byte   $00,$00,$00,$0C,$01,$02,$0A,$03
        .byte   $00,$00,$00,$00,$3D,$3E,$3F,$03
        .byte   $00,$00,$00,$00,$40,$41,$42,$03
        .byte   $43,$01,$02,$02,$36,$44,$45,$03
        .byte   $46,$03,$00,$07,$00,$00,$00,$00
        .byte   $00,$00,$03,$00,$00,$00,$00,$00
        .byte   $00,$00,$03,$00,$26,$47,$48,$26
        .byte   $49,$00,$0B,$05,$00,$00,$16,$00
        .byte   $00,$00,$00,$03,$00,$00,$4A,$00
        .byte   $00,$00,$00,$03,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$05,$3E,$4B,$3E
        .byte   $25,$4C,$4D,$03,$1E,$4E,$4F,$50
        .byte   $1D,$0D,$16,$03,$24,$27,$23,$51
        .byte   $25,$38,$00,$03,$03,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$00,$00,$01
        .byte   $02,$02,$02,$0A,$03,$00,$01,$03
        .byte   $02,$05,$00,$01,$03,$00,$0B,$0A
        .byte   $00,$0B,$02,$0A,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$0B,$02,$02,$02
        .byte   $05,$52,$53,$54,$00,$00,$07,$1D
        .byte   $03,$55,$15,$1D,$19,$36,$36,$36
        .byte   $36,$56,$1B,$36,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$0A,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$1D,$57,$58,$59
        .byte   $02,$02,$02,$02,$0D,$00,$03,$10
        .byte   $00,$00,$00,$00,$36,$36,$36,$36
        .byte   $56,$5A,$03,$00,$00,$00,$00,$00
        .byte   $00,$5A,$03,$00,$00,$00,$00,$00
        .byte   $00,$5A,$0B,$05,$00,$00,$00,$00
        .byte   $00,$5B,$25,$24,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$02,$02,$02,$02
        .byte   $05,$01,$02,$0A,$00,$00,$00,$00
        .byte   $03,$03,$00,$00,$5C,$3D,$1D,$5D
        .byte   $5E,$1D,$5F,$03,$60,$61,$25,$62
        .byte   $25,$25,$63,$03,$03,$00,$00,$00
        .byte   $00,$00,$16,$03,$03,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$00,$25,$25
        .byte   $25,$4B,$00,$03,$03,$00,$00,$00
        .byte   $00,$10,$00,$03,$0B,$02,$02,$02
        .byte   $05,$64,$01,$0A,$02,$02,$02,$02
        .byte   $0A,$5A,$03,$00,$24,$63,$0B,$02
        .byte   $02,$0A,$5B,$3F,$03,$16,$00,$00
        .byte   $00,$00,$65,$0C,$03,$00,$00,$00
        .byte   $00,$00,$00,$51,$03,$00,$00,$25
        .byte   $25,$00,$00,$01,$0A,$00,$00,$00
        .byte   $00,$00,$00,$03,$02,$05,$00,$00
        .byte   $00,$00,$66,$03,$26,$1E,$67,$1D
        .byte   $68,$26,$2A,$03,$00,$03,$15,$1D
        .byte   $08,$00,$1F,$03,$03,$1F,$0B,$05
        .byte   $00,$00,$00,$00,$03,$1F,$01,$03
        .byte   $02,$02,$02,$05,$03,$1F,$03,$03
        .byte   $00,$00,$00,$03,$0A,$1F,$03,$03
        .byte   $00,$00,$00,$03,$00,$1F,$03,$03
        .byte   $00,$00,$00,$03,$05,$1F,$03,$03
        .byte   $00,$00,$00,$0B,$03,$1F,$03,$0B
        .byte   $02,$05,$69,$1D,$03,$1F,$03,$00
        .byte   $00,$03,$5A,$00,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$31,$03,$00,$00,$00
        .byte   $00,$00,$00,$32,$1E,$29,$01,$02
        .byte   $02,$02,$02,$0A,$03,$1F,$03,$00
        .byte   $17,$33,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$02,$02,$02
        .byte   $02,$02,$02,$02,$03,$33,$18,$00
        .byte   $00,$01,$02,$05,$6A,$35,$1C,$18
        .byte   $01,$0A,$00,$03,$6B,$36,$03,$1C
        .byte   $03,$00,$00,$03,$00,$00,$6C,$6D
        .byte   $6D,$6D,$6D,$6D,$00,$00,$6C,$6E
        .byte   $6F,$70,$6D,$6E,$00,$00,$6C,$71
        .byte   $72,$72,$73,$71,$00,$00,$74,$75
        .byte   $75,$75,$75,$75,$02,$02,$05,$00
        .byte   $00,$00,$00,$00,$00,$17,$3D,$00
        .byte   $00,$00,$00,$00,$00,$19,$76,$43
        .byte   $43,$01,$02,$02,$00,$03,$46,$46
        .byte   $46,$03,$46,$46,$6D,$77,$78,$79
        .byte   $7A,$7B,$7C,$6D,$7D,$7E,$7F,$79
        .byte   $7A,$7B,$7C,$80,$7B,$81,$72,$79
        .byte   $82,$7B,$83,$84,$7B,$85,$75,$79
        .byte   $86,$7B,$87,$72,$7B,$88,$00,$79
        .byte   $89,$7B,$8A,$72,$7B,$88,$00,$79
        .byte   $89,$7B,$8A,$72,$02,$02,$02,$02
        .byte   $02,$02,$05,$72,$79,$8B,$46,$79
        .byte   $8B,$79,$03,$72,$6D,$8C,$84,$7B
        .byte   $81,$72,$72,$72,$8D,$72,$72,$7B
        .byte   $81,$72,$72,$72,$72,$72,$72,$7B
        .byte   $81,$72,$72,$72,$72,$72,$72,$7B
        .byte   $81,$72,$72,$72,$72,$72,$72,$7B
        .byte   $81,$8E,$8F,$90,$72,$72,$72,$7B
        .byte   $91,$7B,$92,$6D,$93,$93,$94,$95
        .byte   $96,$7B,$7C,$6D,$6D,$6D,$6D,$79
        .byte   $7A,$7B,$7C,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$97,$6D,$6D,$6D,$8E
        .byte   $98,$6D,$99,$80,$6D,$6D,$6D,$7B
        .byte   $7C,$70,$9A,$71,$6D,$6D,$6D,$7B
        .byte   $9B,$72,$72,$72,$6D,$6D,$8D,$7B
        .byte   $81,$72,$72,$72,$6D,$6D,$80,$9C
        .byte   $84,$72,$72,$72,$6D,$6E,$71,$72
        .byte   $72,$72,$72,$72,$8C,$84,$72,$72
        .byte   $72,$72,$72,$72,$72,$72,$72,$72
        .byte   $72,$72,$9D,$9E,$9F,$72,$72,$72
        .byte   $72,$72,$A0,$A1,$72,$72,$72,$9D
        .byte   $9E,$72,$A0,$A1,$9D,$9E,$72,$A0
        .byte   $A1,$72,$A0,$A1,$A0,$A1,$72,$A0
        .byte   $A1,$72,$A0,$A1,$72,$72,$72,$72
        .byte   $72,$72,$72,$A2,$72,$72,$72,$72
        .byte   $72,$72,$72,$72,$72,$72,$72,$72
        .byte   $9D,$9E,$72,$72,$72,$72,$72,$72
        .byte   $A0,$A1,$72,$72,$72,$72,$72,$72
        .byte   $A0,$A1,$72,$9D,$72,$72,$72,$72
        .byte   $A0,$A1,$72,$A0,$72,$9D,$9E,$72
        .byte   $A0,$A1,$72,$A0,$72,$A0,$A1,$72
        .byte   $A0,$A1,$72,$A0,$78,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$6D,$7F,$A3,$A4,$6D
        .byte   $6D,$80,$8D,$A5,$72,$A6,$A7,$A8
        .byte   $A9,$84,$72,$72,$72,$72,$72,$72
        .byte   $72,$72,$72,$72,$9E,$72,$72,$72
        .byte   $72,$72,$72,$AA,$A1,$72,$72,$A6
        .byte   $72,$72,$72,$AB,$A1,$72,$72,$72
        .byte   $AC,$9D,$AD,$6D,$A1,$72,$72,$AB
        .byte   $AE,$A0,$AF,$6D,$6D,$6E,$B0,$72
        .byte   $72,$72,$72,$03,$A8,$84,$72,$72
        .byte   $72,$72,$72,$03,$72,$72,$72,$72
        .byte   $72,$72,$72,$03,$72,$72,$AB,$B1
        .byte   $72,$72,$72,$0B,$72,$B2,$B3,$6D
        .byte   $B4,$B5,$72,$B6,$B7,$6D,$6D,$6D
        .byte   $6D,$B8,$B5,$B6,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$B9,$01,$6D,$6D,$6D,$6D
        .byte   $6D,$6D,$6D,$03,$BA,$BB,$33,$18
        .byte   $00,$03,$10,$BC,$1B,$BD,$BE,$1C
        .byte   $5E,$1E,$BF,$C0,$00,$00,$00,$0B
        .byte   $02,$0A,$00,$C0,$02,$02,$02,$02
        .byte   $02,$02,$02,$C1,$C2,$00,$00,$00
        .byte   $00,$00,$00,$C3,$C2,$00,$00,$00
        .byte   $00,$00,$00,$C3,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$C4,$0A,$0B,$02
        .byte   $02,$02,$02,$0E,$C5,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C5,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C6,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C2,$00,$00,$00
        .byte   $00,$00,$00,$C0,$C2,$00,$00,$C7
        .byte   $C8,$00,$00,$C9,$02,$0E,$CA,$CB
        .byte   $CC,$CD,$CE,$CE,$00,$C0,$CF,$D0
        .byte   $D1,$D2,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$00
        .byte   $D4,$00,$D4,$D5,$1E,$29,$01,$D6
        .byte   $D7,$D6,$D7,$0A,$03,$1F,$03,$D8
        .byte   $17,$D9,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$DA
        .byte   $00,$DA,$00,$D5,$1E,$29,$01,$DB
        .byte   $DC,$DB,$DC,$0A,$03,$1F,$03,$00
        .byte   $DD,$33,$DE,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$D4
        .byte   $00,$D4,$00,$D5,$1E,$29,$01,$D7
        .byte   $D6,$D7,$D6,$0A,$03,$1F,$03,$00
        .byte   $DF,$33,$D9,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$2B,$2C,$03,$00,$00,$00
        .byte   $00,$2D,$2E,$2F,$03,$00,$00,$00
        .byte   $00,$00,$30,$D3,$03,$00,$00,$00
        .byte   $DA,$00,$DA,$D5,$1E,$29,$01,$DC
        .byte   $DB,$DC,$DB,$0A,$03,$1F,$03,$E0
        .byte   $17,$DE,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$1E,$29,$01,$02
        .byte   $02,$02,$02,$0A,$03,$1F,$03,$00
        .byte   $17,$33,$33,$33,$03,$1F,$03,$00
        .byte   $34,$35,$35,$35,$03,$1F,$03,$00
        .byte   $1B,$36,$36,$36,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$46,$46,$46,$46
        .byte   $46,$46,$46,$46,$58,$58,$58,$58
        .byte   $20,$21,$28,$29,$32,$32,$3A,$3A
        .byte   $30,$31,$30,$31,$3C,$21,$3C,$29
        .byte   $22,$23,$2A,$2B,$3C,$27,$3C,$2F
        .byte   $34,$3D,$3C,$58,$37,$58,$58,$58
        .byte   $3C,$32,$3C,$3A,$26,$27,$2E,$2F
        .byte   $24,$25,$2C,$2D,$3C,$58,$3C,$58
        .byte   $3D,$35,$58,$3C,$32,$22,$3A,$2A
        .byte   $23,$3C,$2B,$3C,$58,$3C,$58,$3C
        .byte   $36,$3D,$2E,$2F,$37,$24,$58,$2C
        .byte   $25,$3C,$2D,$3C,$32,$3C,$3A,$3C
        .byte   $58,$36,$58,$58,$58,$13,$58,$13
        .byte   $58,$58,$20,$21,$58,$58,$22,$23
        .byte   $28,$29,$30,$31,$2A,$2B,$24,$25
        .byte   $2C,$2D,$58,$58,$2A,$2B,$30,$31
        .byte   $3D,$3D,$58,$58,$3D,$3D,$30,$31
        .byte   $3C,$13,$3C,$13,$3C,$35,$3C,$3C
        .byte   $3C,$36,$3C,$58,$35,$3C,$3C,$3C
        .byte   $3C,$58,$37,$58,$30,$31,$3D,$3D
        .byte   $58,$58,$3D,$3D,$3D,$3D,$3D,$3D
        .byte   $37,$58,$3D,$3D,$3C,$36,$37,$58
        .byte   $35,$12,$3C,$13,$37,$13,$35,$13
        .byte   $58,$58,$41,$42,$58,$58,$43,$58
        .byte   $58,$48,$58,$50,$49,$4A,$51,$52
        .byte   $4B,$4C,$53,$54,$58,$5A,$58,$58
        .byte   $5B,$5C,$73,$74,$73,$74,$73,$74
        .byte   $58,$58,$32,$32,$28,$29,$24,$25
        .byte   $3A,$3A,$32,$32,$3A,$3A,$58,$58
        .byte   $3C,$58,$3C,$34,$58,$3C,$3D,$37
        .byte   $3C,$3C,$3C,$3C,$34,$35,$3C,$3C
        .byte   $36,$3C,$34,$37,$3D,$3C,$3C,$36
        .byte   $30,$31,$26,$27,$58,$58,$34,$3D
        .byte   $58,$58,$35,$58,$2E,$2F,$32,$32
        .byte   $3C,$58,$3C,$22,$36,$35,$23,$3C
        .byte   $58,$58,$10,$11,$3C,$2A,$3C,$30
        .byte   $2B,$3C,$31,$3C,$00,$00,$00,$00
        .byte   $35,$34,$36,$3D,$3D,$12,$3D,$13
        .byte   $3D,$35,$3D,$37,$58,$13,$58,$58
        .byte   $58,$58,$3D,$35,$36,$35,$3D,$37
        .byte   $58,$12,$58,$13,$3C,$3D,$3C,$58
        .byte   $35,$36,$3C,$58,$3D,$3D,$3C,$58
        .byte   $3C,$58,$36,$3D,$34,$3D,$3C,$34
        .byte   $3D,$35,$3D,$3D,$34,$3D,$37,$58
        .byte   $3C,$37,$3C,$58,$2E,$2F,$58,$58
        .byte   $3D,$35,$58,$36,$20,$21,$3D,$3D
        .byte   $32,$32,$3D,$35,$13,$3C,$13,$3C
        .byte   $13,$3C,$13,$36,$3D,$3D,$20,$21
        .byte   $3D,$35,$34,$3C,$58,$34,$3D,$37
        .byte   $37,$13,$3C,$13,$28,$29,$3D,$3D
        .byte   $2E,$2F,$3D,$3D,$3C,$3C,$37,$36
        .byte   $3C,$13,$37,$13,$12,$3C,$13,$3C
        .byte   $13,$58,$13,$58,$34,$12,$3C,$13
        .byte   $35,$34,$37,$3C,$3D,$3D,$34,$3D
        .byte   $12,$34,$13,$3C,$30,$31,$32,$32
        .byte   $3A,$3A,$30,$31,$58,$59,$58,$59
        .byte   $60,$60,$60,$60,$60,$60,$60,$75
        .byte   $60,$60,$79,$7A,$60,$60,$7B,$7A
        .byte   $75,$01,$01,$01,$01,$01,$01,$01
        .byte   $79,$7A,$01,$01,$58,$44,$58,$58
        .byte   $4D,$4D,$58,$58,$2E,$2F,$11,$10
        .byte   $60,$60,$75,$79,$60,$60,$7A,$60
        .byte   $15,$02,$15,$02,$1F,$60,$1F,$60
        .byte   $15,$16,$15,$16,$17,$60,$17,$60
        .byte   $7B,$7C,$0D,$0E,$01,$01,$0F,$01
        .byte   $01,$70,$01,$01,$60,$60,$60,$7B
        .byte   $17,$01,$17,$01,$1F,$7A,$1F,$01
        .byte   $17,$7B,$17,$01,$7C,$01,$01,$01
        .byte   $17,$4D,$17,$58,$1F,$4D,$1F,$58
        .byte   $17,$45,$17,$59,$17,$58,$17,$58
        .byte   $1F,$58,$1F,$58,$17,$59,$17,$59
        .byte   $1F,$00,$1F,$00,$60,$7B,$75,$01
        .byte   $60,$75,$7C,$01,$0D,$0E,$15,$16
        .byte   $0F,$01,$17,$01,$01,$01,$61,$64
        .byte   $17,$01,$17,$64,$17,$62,$17,$60
        .byte   $01,$01,$64,$61,$01,$61,$62,$60
        .byte   $15,$0B,$15,$02,$0C,$60,$1F,$60
        .byte   $7B,$7C,$63,$64,$0F,$60,$17,$60
        .byte   $60,$60,$08,$60,$60,$60,$7B,$7C
        .byte   $17,$75,$17,$01,$60,$7B,$7C,$01
        .byte   $18,$19,$1B,$1C,$1A,$01,$1D,$01
        .byte   $6D,$01,$7C,$01,$1B,$1C,$1B,$1C
        .byte   $1D,$01,$1D,$01,$70,$60,$01,$79
        .byte   $60,$60,$79,$7C,$60,$60,$70,$60
        .byte   $70,$7B,$01,$01,$01,$08,$01,$01
        .byte   $01,$79,$01,$01,$7A,$7B,$01,$01
        .byte   $7C,$79,$01,$01,$01,$01,$01,$08
        .byte   $01,$61,$68,$60,$01,$01,$01,$61
        .byte   $1A,$62,$1D,$60,$62,$60,$60,$60
        .byte   $1D,$60,$1D,$60,$7B,$7C,$01,$01
        .byte   $64,$01,$60,$6D,$01,$68,$08,$60
        .byte   $60,$60,$60,$08,$63,$64,$08,$60
        .byte   $01,$01,$6D,$01,$5D,$07,$5D,$07
        .byte   $64,$68,$60,$60,$60,$6D,$60,$60
        .byte   $08,$08,$60,$60,$30,$31,$24,$25
        .byte   $36,$35,$32,$3C,$58,$20,$58,$28
        .byte   $3A,$3C,$58,$36,$3A,$3A,$3D,$3D
        .byte   $3D,$37,$58,$58,$58,$30,$58,$30
        .byte   $32,$26,$3A,$2E,$05,$58,$05,$58
        .byte   $58,$07,$58,$07,$21,$32,$29,$3A
        .byte   $31,$58,$31,$58,$27,$58,$2F,$58
        .byte   $58,$34,$58,$3C,$35,$58,$3C,$58
        .byte   $58,$24,$58,$2C,$23,$58,$2B,$20
        .byte   $58,$3C,$21,$3C,$3C,$58,$32,$32
        .byte   $58,$34,$32,$3C,$3D,$3D,$32,$32
        .byte   $31,$28,$31,$30,$29,$3C,$31,$3C
        .byte   $3A,$3A,$3C,$58,$3A,$3C,$58,$3C
        .byte   $5B,$5C,$76,$77,$58,$58,$58,$38
        .byte   $76,$77,$76,$77,$7E,$38,$7F,$39
        .byte   $7E,$39,$7F,$3B,$58,$3B,$58,$58
        .byte   $58,$3B,$32,$32,$58,$58,$38,$58
        .byte   $39,$7E,$3B,$7F,$38,$7E,$39,$7F
        .byte   $3B,$58,$20,$21,$3B,$58,$32,$32
        .byte   $58,$3B,$20,$21,$3B,$58,$58,$58
        .byte   $55,$4D,$5D,$58,$5D,$58,$5D,$58
        .byte   $2E,$2F,$58,$59,$60,$60,$3F,$60
        .byte   $01,$3F,$01,$01,$01,$01,$01,$3F
        .byte   $01,$68,$3F,$60,$60,$60,$60,$3F
        .byte   $63,$64,$3F,$60,$3F,$6D,$60,$60
        .byte   $23,$58,$2B,$3D,$58,$20,$35,$28
        .byte   $25,$32,$2D,$3A,$3C,$30,$3C,$30
        .byte   $5B,$5C,$30,$31,$3C,$58,$32,$22
        .byte   $58,$58,$23,$58,$3A,$2A,$3C,$30
        .byte   $2B,$58,$31,$58,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$3F,$00,$02,$C4,$12,$12,$28
        .byte   $19,$0A,$0C,$0E,$3F,$3F,$3C,$3C
        .byte   $3A,$2A,$2C,$2E,$8D,$8E,$8E,$04
        .byte   $06,$08,$3A,$3E,$40,$42,$AE,$46
        .byte   $C0,$4A,$4C,$C2,$60,$62,$64,$66
        .byte   $68,$6A,$CE,$6E,$C0,$C2,$AE,$12
        .byte   $20,$22,$24,$26,$3F,$BE,$CE,$DE
        .byte   $C6,$CA,$12,$CE,$12,$3F,$3F,$3F
        .byte   $3F,$14,$E8,$EA,$3F,$91,$93,$95
        .byte   $97,$14,$EC,$00,$3F,$B1,$B3,$B5
        .byte   $B7,$07,$E0,$E2,$3F,$3F,$3F,$99
        .byte   $9B,$07,$E4,$E6,$01,$11,$54,$38
        .byte   $11,$12,$C8,$12,$11,$00,$00,$00
        .byte   $00,$38,$00,$00,$70,$00,$00,$C0
        .byte   $C2,$01,$C0,$C2,$00,$71,$01,$01
        .byte   $77,$00,$AE,$CE,$4F,$AE,$CE,$4F
        .byte   $BE,$DE,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$4F,$00,$03,$C5,$12,$12,$29
        .byte   $2F,$0B,$0D,$0F,$4F,$4F,$3D,$3D
        .byte   $3B,$10,$2D,$2F,$8E,$8E,$8F,$05
        .byte   $06,$09,$3B,$2F,$41,$AF,$45,$47
        .byte   $C1,$4B,$4D,$C3,$61,$63,$65,$67
        .byte   $69,$CF,$6D,$6F,$C1,$C3,$AF,$12
        .byte   $21,$23,$25,$27,$4F,$BF,$CF,$DF
        .byte   $C7,$CA,$12,$CF,$12,$4F,$4F,$4F
        .byte   $07,$07,$E9,$EB,$4F,$92,$94,$96
        .byte   $4F,$13,$00,$EF,$B0,$B2,$B4,$B6
        .byte   $B8,$13,$E1,$E3,$4F,$07,$98,$9A
        .byte   $9C,$4F,$E5,$E7,$01,$11,$44,$48
        .byte   $11,$12,$C9,$C9,$44,$00,$00,$00
        .byte   $00,$11,$00,$00,$01,$00,$00,$C1
        .byte   $C3,$77,$C1,$C3,$00,$70,$01,$01
        .byte   $76,$00,$AF,$CF,$3F,$AF,$CF,$3F
        .byte   $BF,$DF,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$4F,$3C,$02,$D4,$12,$12,$10
        .byte   $3E,$1A,$1C,$1E,$4F,$00,$3C,$3C
        .byte   $3A,$2A,$2C,$2E,$9D,$9E,$9E,$04
        .byte   $16,$18,$3A,$3E,$50,$52,$BE,$56
        .byte   $58,$5A,$5C,$5E,$D0,$72,$74,$D2
        .byte   $78,$7A,$DE,$7E,$D0,$D2,$BE,$12
        .byte   $30,$32,$34,$36,$AE,$CE,$DE,$4F
        .byte   $C6,$DA,$12,$DE,$12,$81,$83,$85
        .byte   $4F,$4F,$F8,$00,$4F,$A1,$A3,$A5
        .byte   $A7,$4F,$FC,$FE,$4F,$4F,$4F,$89
        .byte   $8B,$17,$F0,$F2,$4F,$4F,$4F,$A9
        .byte   $AB,$17,$F4,$F6,$01,$54,$01,$01
        .byte   $49,$12,$D8,$D8,$53,$00,$00,$00
        .byte   $00,$01,$00,$00,$11,$00,$00,$D0
        .byte   $D2,$39,$D0,$D2,$00,$11,$71,$39
        .byte   $11,$00,$BE,$DE,$3F,$BE,$DE,$AE
        .byte   $CE,$3F,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$11,$10,$11
        .byte   $12,$3F,$3D,$03,$D5,$12,$12,$10
        .byte   $2F,$1B,$1D,$1F,$87,$87,$3D,$3D
        .byte   $3B,$10,$2D,$2F,$9E,$9E,$9F,$15
        .byte   $16,$09,$3B,$2F,$51,$BF,$55,$57
        .byte   $59,$5B,$5D,$5F,$D1,$73,$75,$D3
        .byte   $79,$DF,$7D,$7F,$D1,$D3,$BF,$12
        .byte   $31,$33,$35,$37,$AF,$CF,$DF,$3F
        .byte   $C7,$DA,$12,$DF,$12,$82,$84,$86
        .byte   $3F,$17,$F9,$FB,$A0,$A2,$A4,$A6
        .byte   $3F,$3F,$FD,$FF,$3F,$3F,$00,$8A
        .byte   $8C,$3F,$F1,$F3,$3F,$17,$3F,$AA
        .byte   $AC,$3F,$F5,$F7,$01,$53,$01,$01
        .byte   $48,$12,$D9,$D9,$01,$00,$00,$00
        .byte   $00,$49,$00,$00,$43,$00,$00,$D1
        .byte   $D3,$11,$D1,$D3,$00,$11,$43,$76
        .byte   $11,$00,$BF,$DF,$4F,$BF,$DF,$AF
        .byte   $CF,$4F,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$03,$01,$00
        .byte   $00,$10,$00,$10,$10,$00,$00,$01
        .byte   $01,$11,$41,$11,$00,$00,$43,$23
        .byte   $43,$01,$21,$01,$11,$11,$11,$11
        .byte   $11,$11,$23,$01,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$00,$00,$10,$00
        .byte   $10,$10,$60,$10,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$00,$00,$00,$00
        .byte   $00,$00,$11,$11,$00,$00,$00,$00
        .byte   $00,$00,$11,$11,$03,$03,$03,$03
        .byte   $03,$00,$13,$10,$03,$00,$00,$00
        .byte   $00,$03,$00,$00,$03,$00,$00,$00
        .byte   $00,$03,$00,$00,$00,$03,$03,$03
        .byte   $03,$00,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00
