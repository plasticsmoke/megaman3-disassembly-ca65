; da65 V2.18 - Ubuntu 2.19-1
; Created:    2026-02-21 05:24:06
; Input file: /home/kn/megamanforever/megaman3-disassembly-ca65/tools/../build/bank12.bin
; Page:       1


        .setcpu "6502"

L0000           := $0000
L8003           := $8003
L8006           := $8006
L8009           := $8009
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
LF835           := $F835
LF846           := $F846
LF869           := $F869
LF898           := $F898
LF89A           := $F89A
LF8B3           := $F8B3
LF8C2           := $F8C2
LFAE2           := $FAE2
LFB7B           := $FB7B
LFC53           := $FC53
LFC63           := $FC63
LFD11           := $FD11
LFD8C           := $FD8C
LFDA6           := $FDA6
LFF3C           := $FF3C

.segment "BANK12"

        jmp     LA058

        jmp     LA364

        jmp     LA424

        jmp     LA7BA

        jmp     LAC97

        jmp     LABD6

        jmp     LAD25

        jmp     LADE0

        jmp     LAE95

        jmp     LB03B

        jmp     LB17B

        jmp     LB1DE

        jmp     LB210

        nop
        nop
        rts

        jmp     LB245

        jmp     LB27A

        jmp     LB664

        jmp     LB7D8

        jmp     LBA2F

        jmp     LBADB

        jmp     LBC0A

        jmp     LBC7D

        jmp     LA057

        jmp     LA057

        jmp     LA057

        jmp     LB290

        jmp     LB38A

        jmp     LB3A7

        jmp     LB474

LA057:  rts

LA058:  lda     $0300,x
        and     #$0F
        tay
        lda     LA35A,y
        sta     L0000
        lda     LA35F,y
        sta     $01
        jmp     (L0000)

        lda     #$09
        cmp     $30
        beq     LA082
        sta     $30
        lda     #$80
        sta     $B0
        sta     $5A
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
LA082:  lda     $B0
        cmp     #$9C
        bne     LA0A1
        inc     $0300,x
        lda     #$02
        sta     $04A0,x
        lda     #$FF
        sta     $0500,x
        lda     #$01
        sta     $10
LA099:  lda     $0500,x
        beq     LA0A2
        dec     $0500,x
LA0A1:  rts

LA0A2:  jsr     LFC53
        stx     L0000
        lda     $0520,x
        sta     $0500,y
        tax
        lda     LA59D,x
        sta     $03C0,y
        lda     $10
        and     #$01
        bne     LA0C6
        lda     LA5E5,x
        sta     $0360,y
        lda     #$71
        sta     $01
        bne     LA0CF
LA0C6:  lda     #$04
        sta     $0360,y
        lda     #$7E
        sta     $01
LA0CF:  lda     #$80
        sta     $0300,y
        sta     $0480,y
        lda     #$90
        sta     $0580,y
        lda     $01
        sta     $05C0,y
        lda     $10
        sta     $04A0,y
        lda     #$00
        sta     $03E0,y
        sta     $05E0,y
        sta     $05A0,y
        sta     $0400,y
        lda     #$04
        sta     $0420,y
        lda     $F9
        sta     $0380,y
        lda     #$E1
        sta     $0320,y
        lda     $10
        and     #$01
        bne     LA10E
        txa
        clc
        adc     #$18
        tax
LA10E:  lda     LA601,x
        ldx     L0000
        sta     $0500,x
        inc     $0520,x
        lda     $0500,x
        bne     LA13D
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$82
        sta     $0300,x
        lda     #$78
        sta     $0500,x
        lda     #$03
        sta     $0520,x
        lda     #$00
        sta     $05E0,x
        sta     $05A0,x
LA13D:  rts

        lda     $0480,x
        pha
        lda     #$18
        sta     $0480,x
        lda     $03C0,x
        pha
        clc
        adc     #$20
        sta     $03C0,x
        lda     $0580,x
        pha
        and     #$FB
        sta     $0580,x
        jsr     L8009
        pla
        sta     $0580,x
        pla
        sta     $03C0,x
        pla
        sta     $0480,x
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        lda     $0500,x
        bne     LA1EA
        lda     $05A0,x
        cmp     #$02
        bne     LA1F5
        jsr     LFC53
        bcs     LA1F5
        lda     #$58
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
        lda     #$15
        sta     $0500,x
        dec     $0520,x
        bne     LA1EA
        lda     $04A0,x
        and     #$02
        tay
        lda     LA5FE,y
        sta     $0300,x
        lda     #$1E
        sta     $0500,x
        lda     #$00
        sta     $0520,x
        sta     $05E0,x
        sta     $05A0,x
        rts

LA1EA:  dec     $0500,x
        lda     $05A0,x
        bne     LA1F5
        sta     $05E0,x
LA1F5:  jsr     L8003
        lda     $04E0,x
        bne     LA20C
        lda     #$0F
        ldy     #$03
LA201:  sta     $0608,y
        sta     $0628,y
        dey
        bpl     LA201
        sty     $18
LA20C:  rts

        lda     $0500,x
        bne     LA25C
        lda     $0580,x
        ora     #$04
        sta     $0580,x
        lda     #$38
        sta     $0360,x
        lda     $19
        beq     LA228
        inc     $0500,x
        bne     LA25C
LA228:  ldy     $0520,x
        lda     LA6D9,y
        sta     $0780
        sta     $0785
        lda     LA6C1,y
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        lda     #$00
        sta     $0783
        sta     $0784
        sta     $0788
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     $19
LA25C:  lda     $05A0,x
        bne     LA264
        sta     $05E0,x
LA264:  lda     #$02
        sta     $10
        jmp     LA099

        lda     $0500,x
        beq     LA27C
        dec     $0500,x
        lda     $05A0,x
        bne     LA27B
        sta     $05E0,x
LA27B:  rts

LA27C:  lda     $0580,x
        ora     #$04
        sta     $0580,x
        stx     L0000
        ldy     $0520,x
        lda     LA6F1,y
        sta     $0780
        sta     $078D
        lda     LA6F6,y
        sta     $0781
        ora     #$01
        sta     $078E
        ldx     #$09
        stx     $0782
        stx     $078F
        lda     #$00
LA2A7:  sta     $0783,x
        sta     $0790,x
        dex
        bpl     LA2A7
        stx     $079A
        stx     $1A
        lda     #$98
        sta     $01
        lda     LA6FB,y
        sta     $02
        lda     #$00
        sta     $03
        sty     $04
LA2C4:  jsr     LFC53
        bcs     LA324
        lda     #$71
        sta     $05C0,y
        lda     #$00
        sta     $05E0,y
        sta     $05A0,y
        sta     $03E0,y
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        lda     #$80
        sta     $0300,y
        sta     $0480,y
        lda     #$90
        sta     $0580,y
        lda     #$E2
        sta     $0320,y
        lda     $03
        sta     $0500,y
        lda     $F9
        sta     $0380,y
        lda     $02
        sta     $0360,y
        lda     $01
        sta     $03C0,y
        sec
        sbc     #$10
        sta     $01
        cmp     #$88
        bne     LA31E
        ldx     $04
        lda     LA700,x
        sta     $0540,y
        lda     LA705,x
        sta     $0520,y
LA31E:  inc     $03
        cmp     #$48
        bne     LA2C4
LA324:  ldx     L0000
        lda     #$14
        sta     $0500,x
        inc     $0520,x
        lda     $0520,x
        cmp     #$05
        bne     LA359
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$82
        sta     $0300,x
        lda     #$C8
        sta     $0360,x
        lda     #$F0
        sta     $0500,x
        lda     #$03
        sta     $0520,x
        lda     #$00
        sta     $05E0,x
        sta     $05A0,x
LA359:  rts

LA35A:  .byte   $6B,$95,$3E,$0D,$6B
LA35F:  .byte   $A0,$A0,$A1,$A2,$A2
LA364:  lda     $05C0,x
        cmp     #$71
        bne     LA378
        lda     $05A0,x
        cmp     #$02
        bne     LA377
        lda     #$7E
        jsr     LF835
LA377:  rts

LA378:  lda     $04A0,x
        and     #$02
        tay
        lda     LA5FD,y
        clc
        adc     $0500,x
        tay
        lda     $0300,x
        and     #$0F
        bne     LA3AD
        lda     $04A0,x
        and     #$01
        beq     LA39A
        jsr     LF71D
        jmp     LA39D

LA39A:  jsr     LF73B
LA39D:  lda     LA5B5,y
        cmp     $0360,x
        bne     LA423
        inc     $0300,x
        lda     #$7F
        jsr     LF835
LA3AD:  lda     $05A0,x
        cmp     #$03
        bne     LA423
        lda     $19
        bne     LA41E
        lda     #$00
        sta     $0300,x
        lda     LA691,y
        sta     $0780
        sta     $0785
        lda     LA661,y
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        lda     LA631,y
        asl     a
        asl     a
        tay
        lda     LA75A,y
        sta     $0783
        lda     LA75B,y
        sta     $0784
        lda     LA75C,y
        sta     $0788
        lda     LA75D,y
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     $19
        lda     $0500,x
        cmp     #$17
        bne     LA41E
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $051F
        lda     $04BF
        and     #$01
        tay
        lda     LA59B,y
        sta     $05DF
LA41E:  lda     #$00
        sta     $05E0,x
LA423:  rts

LA424:  lda     $05C0,x
        cmp     #$7E
        beq     LA43B
        cmp     #$7F
        beq     LA43E
        lda     $05A0,x
        cmp     #$02
        bne     LA423
        lda     #$7E
        jmp     LF835

LA43B:  jmp     LA4BB

LA43E:  lda     #$3C
        sta     $051F
        lda     $05A0,x
        cmp     #$03
        bne     LA423
        lda     $19
        bne     LA4B5
        ldy     $0500,x
        lda     LA70F,y
        bmi     LA4AF
        pha
        lda     LA728,y
        sta     $0780
        sta     $0785
        lda     LA741,y
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        pla
        asl     a
        asl     a
        tay
        lda     LA75A,y
        sta     $0783
        lda     LA75B,y
        sta     $0784
        lda     LA75C,y
        sta     $0788
        lda     LA75D,y
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     $19
        lda     $0500,x
        cmp     #$14
        bne     LA4AF
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $051F
        lda     #$70
        sta     $05DF
LA4AF:  lda     #$00
        sta     $0300,x
        rts

LA4B5:  lda     #$00
        sta     $05E0,x
        rts

LA4BB:  lda     $0500,x
        beq     LA4D0
        jsr     LF759
        lda     $03C0,x
        cmp     #$98
        bne     LA4CF
        lda     #$00
        sta     $0300,x
LA4CF:  rts

LA4D0:  lda     $0520,x
        beq     LA50E
        dec     $0520,x
        bne     LA4CF
        lda     #$02
        sta     $0560,x
LA4DF:  dec     $0560,x
        bmi     LA4F9
        lda     #$A3
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        lda     #$BD
        sta     $0400,x
        lda     #$01
        sta     $0420,x
        rts

LA4F9:  lda     #$87
        sta     $0440,x
        lda     #$06
        sta     $0460,x
        lda     #$72
        sta     $0400,x
        lda     #$02
        sta     $0420,x
        rts

LA50E:  lda     $0300,x
        and     #$0F
        bne     LA551
        jsr     LF797
        lda     #$98
        cmp     $03C0,x
        bcs     LA524
        sta     $03C0,x
        bcc     LA4DF
LA524:  jsr     LF71D
        lda     $0540,x
        cmp     $0360,x
        bcs     LA4CF
        sta     $0360,x
        sbc     #$9F
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        tay
        lda     LA70A,y
        sta     $0560,x
        lda     #$58
        sta     $03C0,x
        lda     #$00
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        inc     $0300,x
LA551:  lda     $03C0,x
        and     #$0F
        cmp     #$08
        bne     LA598
        jsr     LFC53
        bcs     LA597
        lda     $0560,x
        sta     $0500,y
        inc     $0560,x
        lda     #$7F
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $0480,x
        sta     $0480,y
        lda     $0320,x
        sta     $0320,y
        lda     $03C0,x
        cmp     #$98
        bne     LA598
        lda     #$00
        sta     $0300,x
LA597:  rts

LA598:  jmp     LF759

LA59B:  .byte   $6F,$70
LA59D:  .byte   $98,$58,$88,$98,$68,$78,$88,$58
        .byte   $68,$88,$78,$68,$78,$98,$58,$78
        .byte   $68,$58,$98,$78,$88,$88,$68,$58
LA5B5:  .byte   $E8,$E8,$E8,$D8,$E8,$E8,$D8,$D8
        .byte   $D8,$C8,$D8,$C8,$C8,$B8,$C8,$B8
        .byte   $B8,$B8,$A8,$A8,$B8,$A8,$A8,$A8
        .byte   $18,$18,$18,$28,$18,$18,$28,$28
        .byte   $28,$38,$28,$38,$38,$48,$38,$48
        .byte   $48,$48,$58,$58,$48,$58,$58,$58
LA5E5:  .byte   $A8,$A8,$A8,$B8,$A8,$A8,$B8,$B8
        .byte   $B8,$C8,$B8,$C8,$C8,$D8,$C8,$D8
        .byte   $D8,$D8,$E8,$E8,$D8,$E8,$E8,$E8
LA5FD:  .byte   $00
LA5FE:  .byte   $83,$18,$84
LA601:  .byte   $26,$2C,$26,$28,$28,$2C,$28,$26
        .byte   $28,$2C,$26,$2C,$26,$2C,$2C,$26
        .byte   $26,$26,$2C,$24,$26,$28,$26,$00
        .byte   $26,$2C,$22,$2C,$28,$28,$28,$26
        .byte   $24,$30,$22,$2C,$22,$30,$28,$26
        .byte   $26,$22,$2C,$30,$26,$28,$26,$00
LA631:  .byte   $17,$04,$13,$16,$09,$0E,$12,$03
        .byte   $08,$11,$0D,$07,$0C,$15,$02,$0B
        .byte   $06,$01,$14,$0A,$10,$0F,$05,$00
        .byte   $14,$00,$0F,$15,$05,$0A,$10,$01
        .byte   $06,$11,$0B,$07,$0C,$16,$02,$0D
        .byte   $08,$03,$17,$0E,$12,$13,$09,$04
LA661:  .byte   $5C,$5C,$1C,$5A,$9C,$DC,$1A,$5A
        .byte   $9A,$18,$DA,$98,$D8,$56,$58,$D6
        .byte   $96,$56,$54,$D4,$16,$14,$94,$54
        .byte   $42,$42,$02,$44,$82,$C2,$04,$44
        .byte   $84,$06,$C4,$86,$C6,$48,$46,$C8
        .byte   $88,$48,$4A,$CA,$08,$0A,$8A,$4A
LA691:  .byte   $22,$21,$22,$22,$21,$21,$22,$21
        .byte   $21,$22,$21,$21,$21,$22,$21,$21
        .byte   $21,$21,$22,$21,$22,$22,$21,$21
        .byte   $22,$21,$22,$22,$21,$21,$22,$21
        .byte   $21,$22,$21,$21,$21,$22,$21,$21
        .byte   $21,$21,$22,$21,$22,$22,$21,$21
LA6C1:  .byte   $54,$54,$14,$56,$94,$D4,$16,$56
        .byte   $96,$18,$D6,$98,$D8,$5A,$58,$DA
        .byte   $9A,$5A,$5C,$DC,$1A,$1C,$9C,$5C
LA6D9:  .byte   $22,$21,$22,$22,$21,$21,$22,$21
        .byte   $21,$22,$21,$21,$21,$22,$21,$21
        .byte   $21,$21,$22,$21,$22,$22,$21,$21
LA6F1:  .byte   $21,$21,$21,$21,$21
LA6F6:  .byte   $4A,$48,$46,$44,$42
LA6FB:  .byte   $58,$48,$38,$28,$18
LA700:  .byte   $E8,$D8,$C8,$B8,$A8
LA705:  .byte   $10,$1A,$10,$24,$1A
LA70A:  .byte   $14,$0F,$0A,$05,$00
LA70F:  .byte   $04,$09,$0E,$13,$17,$03,$08,$0D
        .byte   $12,$16,$02,$07,$0C,$11,$FF,$01
        .byte   $06,$0B,$10,$15,$00,$05,$0A,$0F
        .byte   $14
LA728:  .byte   $21,$21,$21,$22,$22,$21,$21,$21
        .byte   $22,$22,$21,$21,$21,$22,$22,$21
        .byte   $21,$21,$22,$22,$21,$21,$21,$22
        .byte   $22
LA741:  .byte   $5C,$9C,$DC,$1C,$5C,$5A,$9A,$DA
        .byte   $1A,$5A,$58,$98,$D8,$18,$58,$56
        .byte   $96,$D6,$16,$56,$54,$94,$D4,$14
        .byte   $54
LA75A:  .byte   $80
LA75B:  .byte   $81
LA75C:  .byte   $80
LA75D:  .byte   $89,$82,$83,$8A,$8B,$84,$85,$8C
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
LA7BA:  lda     #$AB
        pha
        lda     #$03
        pha
        lda     $0300,x
        and     #$0F
        tay
        lda     LAD35,y
        sta     L0000
        lda     LAD37,y
        sta     $01
        jmp     (L0000)

        lda     #$09
        cmp     $30
        beq     LA7FD
        sta     $30
        lda     #$80
        sta     $B0
        sta     $5A
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
        lda     #$00
        sta     $69
        sta     $6B
        sta     $6A
        ldy     #$08
LA7F4:  lda     $0377,y
        sta     $0577,y
        dey
        bpl     LA7F4
LA7FD:  lda     $B0
        cmp     #$9C
        bne     LA81C
        inc     $0300,x
        lda     #$0D
        sta     $F8
        lda     #$3A
        sta     $5E
        lda     #$40
        sta     $0520,x
        lda     #$32
        sta     $0540,x
        lda     #$40
        sta     $6A
LA81C:  rts

        ldy     $0500,x
        lda     LAD39,y
        sta     L0000
        lda     LAD43,y
        sta     $01
        jmp     (L0000)

        lda     $5E
        cmp     #$5A
        beq     LA861
        lda     $5E
        clc
        adc     #$02
        sta     $5E
        cmp     #$5A
        bne     LA843
        lda     #$1E
        sta     $0520,x
LA843:  lda     $03DA
        pha
        lda     $03DB
        pha
        ldy     #$08
LA84D:  lda     $03D7,y
        clc
        adc     #$02
        sta     $03D7,y
        dey
        bpl     LA84D
        pla
        sta     $03DB
        pla
        sta     $03DA
LA861:  dec     $0520,x
        bne     LA869
        inc     $0500,x
LA869:  jmp     LAA0A

        lda     $03DA
        pha
        lda     $03DB
        pha
        ldy     #$08
LA876:  lda     $03D7,y
        sec
        sbc     #$02
        sta     $03D7,y
        dey
        bpl     LA876
        pla
        sta     $03DB
        pla
        sta     $03DA
        lda     $5E
        sec
        sbc     #$02
        sta     $5E
        cmp     #$3A
        bne     LA8A0
        lda     #$40
        sta     $0520,x
        lda     $053E
        sta     $0500,x
LA8A0:  jmp     LAA0A

        lda     $0520,x
        and     #$01
        bne     LA8B3
        dec     $057D
        dec     $0579
        dec     $057B
LA8B3:  dec     $03DB
        lda     $0520,x
        cmp     #$20
        bne     LA8C3
        inc     $05BD
        inc     $05B9
LA8C3:  jmp     LA9B5

        lda     $0520,x
        and     #$01
        bne     LA8EC
        lda     $6A
        clc
        adc     #$01
        sta     $6A
        lda     $6B
        adc     #$00
        sta     $6B
        inc     $057C
        inc     $0578
        inc     $057A
        inc     $057D
        inc     $0579
        inc     $057B
LA8EC:  inc     $03DB
        lda     $0520,x
        cmp     #$20
        bne     LA902
        dec     $05BD
        dec     $05B9
        inc     $05BC
        inc     $05B8
LA902:  jmp     LA9B5

        lda     $0520,x
        and     #$01
        bne     LA915
        dec     $057C
        dec     $0578
        dec     $057A
LA915:  dec     $03DA
        lda     $0520,x
        cmp     #$20
        bne     LA925
        dec     $05BC
        dec     $05B8
LA925:  jmp     LA9B5

        inc     $03DA
        jmp     LA9B5

        lda     $0520,x
        and     #$01
        bne     LA93E
        inc     $057C
        inc     $0578
        inc     $057A
LA93E:  dec     $03DA
        lda     $0520,x
        cmp     #$20
        bne     LA9B5
        inc     $05BC
        inc     $05B8
        jmp     LA9B5

        lda     $0520,x
        and     #$01
        bne     LA977
        lda     $6A
        sec
        sbc     #$01
        sta     $6A
        lda     $6B
        sbc     #$00
        sta     $6B
        dec     $057D
        dec     $0579
        dec     $057B
        dec     $057C
        dec     $0578
        dec     $057A
LA977:  inc     $03DA
        lda     $0520,x
        cmp     #$20
        bne     LA9B5
        dec     $05BC
        dec     $05B8
        inc     $05BD
        inc     $05B9
        bne     LA9B5
        lda     $0520,x
        and     #$01
        bne     LA99F
        inc     $057D
        inc     $0579
        inc     $057B
LA99F:  dec     $03DB
        lda     $0520,x
        cmp     #$20
        bne     LA9B5
        dec     $05BD
        dec     $05B9
        jmp     LA9B5

        inc     $03DB
LA9B5:  dec     $0520,x
        bne     LAA0A
        lda     #$40
        sta     $0520,x
        inc     $0500,x
        lda     $0500,x
        and     #$07
        sta     $0500,x
        beq     LA9E6
        cmp     #$04
        bne     LAA0A
        lda     $6A
        sec
        sbc     #$40
        sta     L0000
        lda     $6B
        sbc     #$01
        ora     L0000
        beq     LA9FA
        lda     #$00
        sta     $0500,x
        beq     LA9FA
LA9E6:  lda     $6A
        sec
        sbc     #$C0
        sta     L0000
        lda     $6B
        sbc     #$00
        ora     L0000
        beq     LA9FA
        lda     #$04
        sta     $0500,x
LA9FA:  lda     $031E
        bmi     LAA0A
        lda     $0500,x
        sta     $053E
        lda     #$08
        sta     $0500,x
LAA0A:  lda     $031E
        bpl     LAA7F
        lda     $059E
        and     #$04
        bne     LAA7E
        lda     $0540,x
        bne     LAA7B
        jsr     LF8C2
        cmp     #$48
        bcc     LAA7E
        jsr     LFC53
        bcs     LAA7E
        lda     #$B4
        sta     $0540,x
        lda     #$61
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $F9
        sta     $0380,y
        lda     #$A8
        sta     $03C0,y
        lda     #$90
        sta     $0580,y
        lda     #$E4
        sta     $0320,y
        lda     #$80
        sta     $0480,y
        lda     #$00
        sta     $0500,y
        sta     $0400,y
        sta     $02
        lda     #$01
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
        lda     #$02
        sta     $0520,y
        sta     $0540,y
LAA7B:  dec     $0540,x
LAA7E:  rts

LAA7F:  lda     $0540,x
        bne     LAA7B
        ldy     #$16
LAA86:  lda     $0300,y
        bpl     LAA91
        dey
        cpy     #$0F
        bne     LAA86
        rts

LAA91:  lda     #$58
        jsr     LF846
        lda     #$00
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        lda     #$E6
        sta     $0320,y
        lda     #$80
        sta     $0480,y
        lda     $03C0,x
        clc
        adc     #$0C
        sta     $03C0,y
        lda     $F9
        sta     $0380,y
        sty     $0F
        ldy     $051E
        lda     LADCE,y
        sta     $0540,x
        lda     LADD4,y
        clc
        adc     $0360,x
        ldy     $0F
        sta     $0360,y
        ldx     $0F
        jsr     LF869
        jsr     LF8C2
        sta     $01
        lda     #$00
        sta     L0000
        sta     $02
        lda     #$24
        sta     $03
        jsr     LFD11
        ldy     $0F
        lda     $04
        sta     $0400,x
        lda     $05
        sta     $0420,x
        ldx     #$1F
        inc     $051E
        lda     $051E
        cmp     #$06
        bne     LAB03
        lda     #$00
        sta     $051E
LAB03:  rts

        ldy     #$08
LAB06:  lda     $0597,y
        ora     #$04
        sta     $0597,y
        lda     $0577,y
        sec
        sbc     $6A
        sta     L0000
        lda     #$01
        sbc     $6B
        bne     LAB29
        lda     L0000
        sta     $0377,y
        lda     $0597,y
        and     #$FB
        sta     $0597,y
LAB29:  dey
        bpl     LAB06
        lda     $95
        and     #$01
        tay
        lda     $059D
        and     #$04
        bne     LAB44
        lda     LAD8A,y
        sta     $059D
        lda     LAD8C,y
        sta     $0599
LAB44:  lda     $059C
        and     #$04
        bne     LAB5B
        lda     LAD8A,y
        ora     #$40
        sta     $059C
        lda     LAD8C,y
        ora     #$40
        sta     $0598
LAB5B:  lda     $031E
        bpl     LAB68
        lda     $059F
        ora     #$04
        sta     $059F
LAB68:  lda     #$00
        sta     $05FD
        sta     $05FC
        sta     $05F9
        sta     $05F8
        lda     $057F
        sec
        sbc     $6A
        lda     #$01
        sbc     $6B
        bne     LABA5
        lda     $03DF
        pha
        clc
        adc     #$18
        sta     $03DF
        lda     #$18
        sta     $049F
        lda     $059F
        pha
        and     #$F0
        sta     $059F
        jsr     L8009
        pla
        sta     $059F
        pla
        sta     $03DF
LABA5:  lda     $059F
        and     #$04
        bne     LABD5
        lda     #$02
        sta     $049F
        jsr     L8003
        lda     $04E0,x
        bne     LABD5
        lda     #$6D
        sta     $05D0
        lda     #$00
        sta     $05B0
        sta     $05F0
        ldy     #$0B
        lda     #$0F
LABCA:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     LABCA
        sty     $18
LABD5:  rts

LABD6:  lda     $B3
        bpl     LABD5
        lda     $30
        cmp     #$09
        beq     LABD5
        lda     $04E0,x
        beq     LAC19
        jsr     L8003
        lda     $04E0,x
        ora     #$80
        sta     $B0
        and     #$1F
        bne     LAC4E
        lda     #$E5
        sta     $0320,x
        lda     #$94
        sta     $0580,x
        lda     #$00
        sta     $0500,x
        sta     $05C0,x
        ldy     #$00
LAC07:  lda     LAD4D,y
        sta     $0780,y
        cmp     #$FF
        beq     LAC14
        iny
        bne     LAC07
LAC14:  sta     $19
        jmp     LAC4F

LAC19:  lda     $0500,x
        and     #$03
        bne     LAC4B
        lda     #$1C
        jsr     LF89A
        inc     $B0
        lda     $B0
        cmp     #$9C
        bne     LAC4B
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $031E
        sta     $051E
        sta     $053E
        sta     $055E
        lda     $031F
        ora     #$40
        sta     $031F
LAC4B:  inc     $0500,x
LAC4E:  rts

LAC4F:  lda     #$02
        sta     $01
        lda     $0360,x
        sta     $02
        lda     $03C0,x
        sta     $03
LAC5D:  jsr     LFC53
        bcs     LAC96
        lda     #$59
        jsr     LF846
        lda     #$00
        sta     $0500,y
        sta     $0480,y
        lda     #$19
        sta     $0320,y
        lda     $0380,x
        sta     $0380,y
        stx     $0F
        ldx     $01
        lda     $02
        clc
        adc     LADDA,x
        sta     $0360,y
        lda     $03
        clc
        adc     LADDD,x
        sta     $03C0,y
        ldx     $0F
        dec     $01
        bpl     LAC5D
LAC96:  rts

LAC97:  lda     $04A0,x
        and     #$01
        beq     LACA4
        jsr     LF71D
        jmp     LACA7

LACA4:  jsr     LF73B
LACA7:  lda     $04A0,x
        and     #$08
        beq     LACB4
        jsr     LF779
        jmp     LACB7

LACB4:  jsr     LF759
LACB7:  ldy     $0500,x
        lda     #$00
        sta     L0000
        sta     $01
        lda     LAD9E,y
        bpl     LACC7
        dec     L0000
LACC7:  lda     $03A0,x
        clc
        adc     LAD8E,y
        sta     $03A0,x
        lda     $03C0,x
        adc     LAD9E,y
        sta     $03C0,x
        lda     $03E0,x
        adc     L0000
        beq     LACE7
        lda     #$00
        sta     $0300,x
        rts

LACE7:  lda     LADBE,y
        bpl     LACEE
        dec     $01
LACEE:  lda     $0340,x
        clc
        adc     LADAE,y
        sta     $0340,x
        lda     $0360,x
        adc     LADBE,y
        sta     $0360,x
        lda     $0380,x
        adc     $01
        sta     $0380,x
        dec     $0520,x
        bne     LAD24
        inc     $0500,x
        lda     $0500,x
        and     #$0F
        sta     $0500,x
        bne     LAD1E
        inc     $0540,x
LAD1E:  lda     $0540,x
        sta     $0520,x
LAD24:  rts

LAD25:  jsr     LF797
        lda     $04A0,x
        and     #$01
        beq     LAD32
        jmp     LF71D

LAD32:  jmp     LF73B

LAD35:  .byte   $D3,$1D
LAD37:  .byte   $A7,$A8
LAD39:  .byte   $A3,$C6,$05,$28,$2E,$51,$8F,$B2
        .byte   $2D,$6C
LAD43:  .byte   $A8,$A8,$A9,$A9,$A9,$A9,$A9,$A9
        .byte   $A8,$A8
LAD4D:  .byte   $25,$0A,$0B,$80,$80,$80,$80,$E6
        .byte   $E7,$E8,$E9,$80,$80,$80,$80,$25
        .byte   $2A,$0B,$80,$80,$80,$EA,$EB,$80
        .byte   $80,$80,$EC,$80,$80,$80,$25,$4A
        .byte   $0B,$80,$80,$80,$ED,$EE,$80,$80
        .byte   $EF,$F0,$80,$80,$80,$25,$6A,$0B
        .byte   $80,$F1,$F2,$9A,$F3,$F4,$F5,$F6
        .byte   $9F,$F7,$F8,$80,$FF
LAD8A:  .byte   $90,$94
LAD8C:  .byte   $94,$90
LAD8E:  .byte   $00,$3B,$E1,$DB,$00,$25,$1F,$C5
        .byte   $00,$C5,$1F,$25,$00,$DB,$E1,$3B
LAD9E:  .byte   $FD,$FD,$FD,$FE,$00,$01,$02,$02
        .byte   $03,$02,$02,$01,$00,$FE,$FD,$FD
LADAE:  .byte   $00,$25,$1F,$C5,$00,$C5,$1F,$25
        .byte   $00,$DB,$E1,$3B,$00,$3B,$E1,$DB
LADBE:  .byte   $00,$01,$02,$02,$03,$02,$02,$01
        .byte   $00,$FE,$FD,$FD,$FD,$FD,$FD,$FE
LADCE:  .byte   $32,$3C,$32,$3C,$32,$5A
LADD4:  .byte   $E0,$20,$E0,$20,$E0,$20
LADDA:  .byte   $00,$F0,$10
LADDD:  .byte   $C0,$D0,$D0
LADE0:  lda     $0300,x
        and     #$0F
        bne     LAE17
        sta     $95
        inc     $0300,x
        lda     #$09
        sta     $30
        lda     #$80
        sta     $B0
        sta     $5A
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
        lda     #$30
        sta     $0500,x
        lda     #$6C
        sta     $E8
        lda     #$6E
        sta     $E9
        jsr     LFF3C
        lda     #$00
        sta     $69
        sta     $6A
        sta     $6B
LAE17:  lda     $0500,x
        bmi     LAE5F
        lda     #$00
        lda     $95
        and     #$0F
        bne     LAE5B
        ldy     #$0B
LAE26:  lda     LAE81,y
        sec
        sbc     $0500,x
        bcs     LAE31
        lda     #$0F
LAE31:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     LAE26
        ldy     #$07
LAE3C:  lda     LAE8D,y
        sec
        sbc     $0500,x
        bcs     LAE47
        lda     #$0F
LAE47:  sta     $0618,y
        sta     $0638,y
        dey
        bpl     LAE3C
        sty     $18
        lda     $0500,x
        sec
        sbc     #$10
        sta     $0500,x
LAE5B:  lda     #$80
        sta     $B0
LAE5F:  lda     $B0
        cmp     #$9C
        bne     LAE7B
        lda     #$00
        sta     $30
        sta     $0500,x
        lda     #$C0
        sta     $0300,x
        lda     #$E8
        sta     $0320,x
        lda     #$1C
        sta     $04E0,x
LAE7B:  lda     #$00
        sta     $05E0,x
        rts

LAE81:  .byte   $0F,$30,$16,$04,$0F,$30,$11,$01
        .byte   $0F,$30,$36,$26
LAE8D:  .byte   $0F,$01,$30,$11,$0F,$0F,$30,$10
LAE95:  lda     $0500,x
        bne     LAEAC
        jsr     LF8C2
        cmp     #$50
        bcs     LAEAF
        jsr     LF869
        jsr     LAFD2
        lda     #$1F
        sta     $0500,x
LAEAC:  dec     $0500,x
LAEAF:  lda     $0540,x
        bne     LAEBB
        jsr     LF8B3
        cmp     #$30
        bcs     LAEE3
LAEBB:  lda     $0520,x
        bne     LAEE0
        lda     #$02
        sta     $01
        jsr     LAF76
        lda     #$1F
        sta     $0520,x
        inc     $0540,x
        lda     $0540,x
        cmp     #$03
        bcc     LAEE0
        lda     #$79
        sta     $0520,x
        lda     #$00
        sta     $0540,x
LAEE0:  dec     $0520,x
LAEE3:  lda     $04E0,x
        cmp     #$0F
        bcc     LAEEB
        rts

LAEEB:  ldy     #$17
LAEED:  cpy     #$10
        bcs     LAEF5
        lda     #$7A
        bne     LAEF7
LAEF5:  lda     #$5B
LAEF7:  jsr     LF846
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
        lda     $D7E9,y
        sta     $0400,y
        lda     $D7F9,y
        sta     $0420,y
        lda     $D809,y
        sta     $0440,y
        lda     $D819,y
        sta     $0460,y
        dey
        cpy     #$07
        bne     LAEED
        lda     #$10
        sta     $30
        lda     #$C0
        sta     $0300,x
        lda     #$00
        sta     $69
        sta     $6A
        sta     $0400,x
        sta     $0420,x
        lda     #$B4
        sta     $0500,x
        lda     #$F0
        sta     $0520,x
        lda     #$02
        sta     $0540,x
        lda     #$E9
        sta     $0320,x
        lda     $0580,x
        ora     #$04
        sta     $0580,x
        lda     $0480,x
        and     #$BF
        sta     $0480,x
        lda     #$6B
        jmp     LF835

LAF76:  jsr     LFC53
        bcs     LAFCD
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $0360,x
        clc
        adc     LAFCE,y
        pha
        lda     $0380,x
        adc     LAFCF,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$80
        sta     $0400,y
        lda     #$01
        sta     $0420,y
        lda     #$58
        jsr     LF846
        lda     #$51
        sta     $0320,y
        lda     #$8B
        sta     $0480,y
        lda     #$00
        sta     $04E0,y
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        dec     $01
        bne     LAF76
LAFCD:  rts

LAFCE:  .byte   $0F
LAFCF:  .byte   $00,$F1,$FF
LAFD2:  jsr     LFC53
        bcs     LB02E
        lda     #$00
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        lda     #$58
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
LB018:  cmp     LB02F,x
        bcc     LB020
        dex
        bne     LB018
LB020:  lda     LB033,x
        sta     $0400,y
        lda     LB037,x
        sta     $0420,y
        ldx     L0000
LB02E:  rts

LB02F:  .byte   $4C,$3D,$2E,$1F
LB033:  .byte   $00,$80,$00,$80
LB037:  .byte   $02,$01,$01,$00
LB03B:  lda     $0500,x
        bne     LB08E
        jsr     LFC53
        bcs     LB091
        lda     #$77
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$38
        sta     $03C0,y
        lda     #$80
        sta     $0480,y
        lda     #$50
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
        lda     #$B5
        sta     $0500,x
LB08E:  dec     $0500,x
LB091:  lda     $0520,x
        bne     LB108
        lda     $0540,x
        and     #$01
        bne     LB0E1
        lda     $69
        clc
        adc     $0400,x
        sta     $69
        lda     $6A
        adc     $0420,x
        sta     $6A
        cmp     #$A0
        bcs     LB0CC
        lda     $0400,x
        clc
        adc     #$10
        sta     $0400,x
        lda     $0420,x
        adc     #$00
        sta     $0420,x
        cmp     #$03
        bne     LB10B
        lda     #$00
        sta     $0400,x
        beq     LB10B
LB0CC:  lda     #$A0
        sta     $6A
        lda     #$80
        sta     $0400,x
        lda     #$01
        sta     $0420,x
        lda     #$01
        sta     $0540,x
        bne     LB10B
LB0E1:  lda     $69
        sec
        sbc     $0400,x
        sta     $69
        lda     $6A
        sbc     $0420,x
        sta     $6A
        bcs     LB10B
        lda     #$00
        sta     $69
        sta     $6A
        sta     $0400,x
        sta     $0420,x
        lda     #$F1
        sta     $0520,x
        lda     #$02
        sta     $0540,x
LB108:  dec     $0520,x
LB10B:  jsr     L8003
        lda     $04E0,x
        bne     LB166
        sta     $6A
        sta     $6B
        ldy     #$0B
        lda     #$0F
LB11B:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     LB11B
        sty     $18
        ldy     #$00
LB128:  lda     LB167,y
        sta     $0780,y
        cmp     #$FF
        beq     LB135
        iny
        bne     LB128
LB135:  sta     $19
        lda     #$80
        sta     $0310
        lda     #$90
        sta     $0590
        lda     #$6D
        sta     $05D0
        lda     #$00
        sta     $05F0
        sta     $05B0
        sta     $0490
        lda     #$EF
        sta     $0330
        lda     #$A3
        sta     $0450
        lda     #$04
        sta     $0470
        lda     #$00
        sta     $6A
        sta     $6B
LB166:  rts

LB167:  .byte   $23,$C0,$0F,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$FF
LB17B:  lda     $0580,x
        and     #$04
        bne     LB1BF
        lda     $30
        cmp     #$06
        beq     LB1BF
        cmp     #$0E
        beq     LB1BF
        lda     $03C0
        pha
        inc     $03C0
        jsr     LFAE2
        bcs     LB1A7
        lda     $041F
        sta     $37
        lda     $043F
        sta     $38
        lda     $055F
        sta     $36
LB1A7:  pla
        sta     $03C0
        lda     $04C0,x
        cmp     #$0D
        beq     LB1BF
        lda     $39
        bne     LB1BF
        jsr     LFAE2
        bcs     LB1BF
        lda     #$0E
        sta     $30
LB1BF:  lda     $0580,x
        .byte   $09
LB1C3:  .byte   $04
        sta     $0580,x
        lda     LB1C3,x
        sec
        sbc     $6A
        bcs     LB1DA
        sta     $0360,x
        lda     $0580,x
        and     #$FB
        sta     $0580,x
LB1DA:  rts

        .byte   $80,$40,$10
LB1DE:  jsr     LFAE2
        bcs     LB20F
        jsr     LF8C2
        cmp     #$02
        bcs     LB20F
        lda     $0360,x
        sta     $0360
        lda     $04C0,x
        sbc     #$0E
        cmp     #$01
        beq     LB20F
        sta     $6C
        lda     #$11
        sta     $30
        lda     #$13
        sta     $05C0
        lda     #$00
        sta     $05E0
        sta     $05A0
        sta     $0300,x
LB20F:  rts

LB210:  lda     $03C0,x
        cmp     #$68
        beq     LB21F
        inc     $0360,x
        lda     #$01
        jmp     LB224

LB21F:  dec     $0360,x
        lda     #$02
LB224:  sta     $04A0,x
        lda     $03C0,x
        pha
        dec     $03C0,x
        jsr     LFAE2
        pla
        sta     $03C0,x
        bcs     LB244
        lda     $04A0,x
        sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
LB244:  rts

LB245:  lda     $03C0,x
        cmp     #$A8
        beq     LB265
        clc
        adc     #$04
        sta     $03C0,x
        cmp     #$A8
        bne     LB279
        lda     #$6C
        cmp     $05C0,x
        beq     LB265
        jsr     LF835
        lda     #$10
        sta     $0500,x
LB265:  lda     $05C0,x
        cmp     #$6E
        bne     LB279
        lda     $0500,x
        beq     LB279
        dec     $0500,x
        lda     #$00
        sta     $05E0,x
LB279:  rts

LB27A:  ldy     #$08
        jsr     LF67C
        bcs     LB285
        inc     $0360,x
        rts

LB285:  lda     #$6C
        cmp     $05C0,x
        beq     LB28F
        jsr     LF835
LB28F:  rts

LB290:  jsr     LF797
        lda     #$B0
        cmp     $03C0,x
        bcs     LB28F
        sta     $03C0,x
        lda     $0500,x
        cmp     #$02
        beq     LB301
        bcs     LB307
        lda     #$00
        sta     $0300,x
LB2AB:  lda     #$03
        sta     L0000
LB2AF:  jsr     LFC53
        bcs     LB28F
        lda     #$78
        jsr     LF846
        lda     #$FA
        sta     $0320,y
        lda     #$00
        sta     $0480,y
        lda     $0380,x
        sta     $0380,y
        sta     $0400,y
        lda     #$44
        sta     $0440,y
        lda     #$03
        sta     $0460,y
        stx     $01
        lda     $0360,x
        sta     $02
        ldx     L0000
        lda     LB37A,x
        sta     $03C0,y
        lda     $02
        clc
        adc     LB37E,x
        sta     $0360,y
        lda     LB382,x
        sta     $04A0,y
        lda     LB386,x
        sta     $0420,y
        ldx     $01
        dec     L0000
        bpl     LB2AF
        rts

LB301:  lda     #$00
        sta     $0310
        rts

LB307:  lda     $0580
        ora     #$04
        sta     $0580
        lda     $0520,x
        cmp     #$3C
        bne     LB32B
        lda     #$79
        jsr     LF835
        stx     $0560
        lda     #$00
        sta     $0320,x
        lda     #$B4
        sta     $03C0,x
        jmp     LB2AB

LB32B:  inc     $0520,x
        bne     LB379
        jsr     LFC53
        bcs     LB379
        lda     #$7D
        jsr     LF846
        lda     #$00
        sta     $03C0,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        sta     $0500,y
        lda     #$00
        sta     $0480,y
        sta     $0440,y
        sta     $0460,y
        sta     $0400,y
        sta     $0420,y
        lda     #$FB
        sta     $0320,y
        lda     #$01
        sta     $04A0,y
        ldy     #$07
LB36B:  lda     LB46C,y
        sta     $0610,y
        sta     $0630,y
        dey
        bpl     LB36B
        sty     $18
LB379:  rts

LB37A:  .byte   $A8,$A8,$B8,$B8
LB37E:  .byte   $F8,$08,$F8,$08
LB382:  .byte   $02,$01,$02,$01
LB386:  .byte   $02,$02,$01,$01
LB38A:  jsr     LF797
        lda     $03C0,x
        cmp     #$B8
        bcc     LB39A
        lda     #$00
        sta     $0300,x
        rts

LB39A:  lda     $04A0,x
        and     #$01
        beq     LB3A4
        jmp     LF71D

LB3A4:  jmp     LF73B

LB3A7:  jsr     LF797
        lda     $04A0,x
        and     #$02
        beq     LB3DA
        lda     #$B4
        cmp     $03C0,x
        bcs     LB41D
        lda     #$00
        sta     $0300,x
        lda     #$81
        sta     $0300
        lda     #$00
        ldy     $0560
        sta     $0300,y
        sta     $0500
        lda     #$0D
        sta     $30
        lda     $0580
        and     #$FB
        sta     $0580
        rts

LB3DA:  lda     #$94
        cmp     $03C0,x
        bcs     LB418
        sta     $03C0,x
        lda     $05C0,x
        cmp     #$7A
        beq     LB42A
        cmp     #$7B
        bne     LB3F8
        lda     #$7A
        jsr     LF835
        lda     #$00
        sta     $B8
LB3F8:  lda     $05A0,x
        cmp     #$04
        bne     LB46B
        lda     #$7B
        jsr     LF835
        lda     #$A3
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        lda     #$9B
        sta     $0400,x
        lda     #$02
        sta     $0420,x
LB418:  lda     #$00
        sta     $05E0,x
LB41D:  lda     $04A0,x
        and     #$01
        beq     LB427
        jmp     LF71D

LB427:  jmp     LF73B

LB42A:  inc     $0500,x
        lda     $0500,x
        cmp     #$3C
        bne     LB46B
        lda     $B8
        bne     LB44A
        dec     $0500,x
        lda     #$11
        sta     $F8
        lda     #$D0
        sta     $5E
        lda     #$0A
        sta     $B8
        jmp     LFD8C

LB44A:  jsr     LFDA6
        lda     $B8
        cmp     #$FF
        beq     LB457
        dec     $0500,x
        rts

LB457:  lda     #$A3
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        lda     #$7B
        jsr     LF835
        lda     #$02
        sta     $04A0,x
LB46B:  rts

LB46C:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
LB474:  lda     $0480,x
        pha
        lda     #$00
        sta     $0480,x
        jsr     L8003
        pla
        sta     $0480,x
        bcs     LB4E5
        lda     $04E0,x
        bne     LB4E5
        sta     $F8
        sta     $0480,x
        lda     #$02
        sta     $01
        lda     $0360,x
        sta     $02
        lda     $03C0,x
        sta     $03
LB49E:  jsr     LFC53
        bcs     LB4E4
        lda     #$71
        jsr     LF846
        lda     #$00
        sta     $0500,y
        sta     $0480,y
        lda     #$19
        sta     $0320,y
        lda     $0380,x
        sta     $0380,y
        stx     $0F
        ldx     $01
        lda     $02
        clc
        adc     LB64E,x
        sta     $0360,y
        lda     $03
        clc
        adc     LB651,x
        sta     $03C0,y
        ldx     $0F
        dec     $01
        bpl     LB49E
        ldy     #$0F
LB4D9:  lda     LB654,y
        sta     $0600,y
        dey
        bpl     LB4D9
        sty     $18
LB4E4:  rts

LB4E5:  lda     $F8
        cmp     #$03
        bcc     LB4E4
        lda     $0520,x
        beq     LB50A
        dec     $0520,x
        bne     LB507
        lda     $0540,x
        beq     LB507
        dec     $0540,x
        beq     LB507
        lda     #$1E
        sta     $0520,x
        jmp     LB5FB

LB507:  jmp     LB595

LB50A:  lda     $0300,x
        and     #$0F
        bne     LB526
        inc     $0300,x
        sta     $0460,x
        lda     #$80
        sta     $0440,x
        lda     #$88
        sta     $04A0,x
        lda     #$F0
        sta     $0500,x
LB526:  lda     $0500,x
        bne     LB548
        lda     $04A0,x
        eor     #$0C
        and     #$0C
        sta     $04A0,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     LB63E,y
        sta     $0500,x
        lda     #$00
        sta     $FC
LB548:  lda     $04A0,x
        and     #$04
        bne     LB55E
        jsr     LF779
        lda     #$48
        cmp     $03C0,x
        bcc     LB573
        sta     $03C0,x
        bcs     LB56B
LB55E:  jsr     LF759
        lda     #$80
        cmp     $03C0,x
        bcs     LB573
        sta     $03C0,x
LB56B:  lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
LB573:  dec     $0500,x
        bne     LB595
        lda     #$1E
        sta     $0520,x
        lda     $E4
        adc     $E6
        sta     $E4
        and     #$01
        beq     LB58D
        jsr     LB5A1
        jmp     LB595

LB58D:  lda     #$03
        sta     $0540,x
        jsr     LB5FB
LB595:  lda     $03C0,x
        sec
        sbc     #$D0
        clc
        adc     #$AF
        sta     $5E
        rts

LB5A1:  lda     #$02
        sta     $01
LB5A5:  jsr     LFC53
        bcs     LB5FA
        lda     #$6F
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     #$00
        sta     $04E0,y
        lda     #$0F
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$30
        sta     $03C0,y
        lda     #$02
        sta     $04A0,y
        stx     L0000
        ldx     $01
        lda     LB642,x
        sta     $0400,y
        lda     LB645,x
        sta     $0420,y
        lda     LB648,x
        sta     $0440,y
        lda     LB64B,x
        sta     $0460,y
        ldx     L0000
        dec     $01
        bpl     LB5A5
LB5FA:  rts

LB5FB:  jsr     LFC53
        bcs     LB5FA
        lda     #$1D
        jsr     LF846
        lda     #$C0
        sta     $0480,y
        lda     #$6D
        sta     $0320,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$30
        sta     $03C0,y
        lda     #$AB
        sta     $0440,y
        lda     #$FF
        sta     $0460,y
        lda     #$00
        sta     $0400,y
        lda     #$02
        sta     $0420,y
        lda     #$01
        sta     $04E0,y
        rts

LB63E:  .byte   $1E,$3C,$3C,$5A
LB642:  .byte   $B5,$00,$B5
LB645:  .byte   $00,$01,$00
LB648:  .byte   $4B,$00,$B5
LB64B:  .byte   $FF,$00,$00
LB64E:  .byte   $00,$F0,$10
LB651:  .byte   $00,$10,$10
LB654:  .byte   $0F,$20,$27,$17,$0F,$03,$12,$0F
        .byte   $0F,$2B,$1B,$0B,$0F,$22,$12,$02
LB664:  lda     $0300,x
        and     #$0F
        bne     LB68E
        lda     #$09
        cmp     $30
        beq     LB682
        sta     $30
        lda     #$80
        sta     $B0
        sta     $5A
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
LB682:  lda     $B0
        cmp     #$9C
        bne     LB6EC
        jsr     LB732
        inc     $0300,x
LB68E:  lda     $0300,x
        and     #$02
        bne     LB6EC
        lda     $05C0,x
        cmp     #$4F
        beq     LB6B2
        lda     $0520,x
        bne     LB6ED
        lda     $0540,x
        cmp     #$05
        bcs     LB6E9
        lda     $0500,x
        bne     LB6C7
        lda     #$4F
        jsr     LF835
LB6B2:  lda     $05A0,x
        cmp     #$02
        bne     LB6EC
        jsr     LB756
        inc     $0540,x
        lda     #$31
        jsr     LF835
        inc     $0520,x
LB6C7:  lda     $04A0,x
        and     #$01
        beq     LB6D6
        ldy     #$20
        jsr     LF580
        jmp     LB6DB

LB6D6:  ldy     #$21
        jsr     LF5C4
LB6DB:  bcc     LB6E5
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
LB6E5:  dec     $0500,x
        rts

LB6E9:  inc     $0300,x
LB6EC:  rts

LB6ED:  lda     #$00
        sta     L0000
        lda     #$80
        sta     $01
        ldy     #$1F
LB6F7:  lda     $0300,y
        bmi     LB726
LB6FC:  dey
        cpy     #$0F
        bne     LB6F7
        lda     L0000
        bne     LB71D
        lda     #$00
        sta     $0520,x
        lda     $0540,x
        tay
        lda     $B0
        sec
        sbc     LB750,y
        sta     $B0
        and     #$1F
        bne     LB71D
        jmp     L8006

LB71D:  lda     #$31
        jsr     LF835
        jsr     LB732
        rts

LB726:  lda     $01
        cmp     $04C0,y
        bne     LB6FC
        inc     L0000
        jmp     LB6FC

LB732:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     LB748,y
        sta     $0500,x
        lda     LB74C,y
        sta     $04A0,x
        rts

LB748:  .byte   $40,$A0,$70,$D0
LB74C:  .byte   $01,$02,$01,$02
LB750:  .byte   $01,$02,$03,$05,$08,$0A
LB756:  jsr     LFC53
        bcs     LB7C5
        sty     L0000
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        clc
        adc     #$18
        sta     $03C0,y
        lda     #$C2
        sta     $0480,y
        lda     #$F1
        sta     $0320,y
        lda     #$03
        sta     $04E0,y
        lda     #$80
        sta     $04C0,y
        lda     $0540,x
        sta     $0540,y
        sta     $02
        tay
        lda     LB7C6,y
        ldy     L0000
        sta     $04A0,y
        ldy     $02
        lda     LB7CC,y
        ldy     L0000
        sta     $0400,y
        sta     $0440,y
        ldy     $02
        lda     LB7D2,y
        ldy     L0000
        sta     $0420,y
        sta     $0460,y
        lda     #$5E
        jsr     LF846
        lda     $04A0,y
        and     #$01
        bne     LB7C5
        lda     $0580,y
        and     #$BF
        sta     $0580,y
LB7C5:  rts

LB7C6:  .byte   $06,$05,$05,$06,$05,$06
LB7CC:  .byte   $80,$00,$80,$00,$00,$00
LB7D2:  .byte   $00,$01,$01,$02,$03,$04
LB7D8:  lda     $0300,x
        and     #$0F
        bne     LB7EA
        sta     $0520,x
        lda     #$78
        sta     $0500,x
        inc     $0300,x
LB7EA:  lda     $0300,x
        and     #$02
        beq     LB7F4
        jmp     LB8A3

LB7F4:  jsr     LB94A
        lda     $0500,x
        bne     LB815
        lda     $03C0,x
        cmp     #$68
        bcs     LB80A
        lda     #$78
        sta     $0500,x
        bne     LB819
LB80A:  jsr     LB9EC
        jsr     LB989
        lda     #$FF
        sta     $0500,x
LB815:  cmp     #$FF
        beq     LB81C
LB819:  dec     $0500,x
LB81C:  lda     $04A0,x
        and     #$01
        beq     LB82B
        ldy     #$0C
        jsr     LF580
        jmp     LB830

LB82B:  ldy     #$0D
        jsr     LF5C4
LB830:  bcc     LB847
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        and     #$0C
        bne     LB8A2
        lda     $04A0,x
        ora     #$08
        sta     $04A0,x
        rts

LB847:  lda     $04A0,x
        and     #$0C
        beq     LB8A2
        and     #$04
        beq     LB86A
        lda     $0500,x
        cmp     #$FF
        beq     LB85D
        lda     #$5E
        bne     LB85F
LB85D:  lda     #$62
LB85F:  sta     $05C0,x
        ldy     #$0E
        jsr     LF606
        jmp     LB898

LB86A:  lda     $04A0,x
        and     #$01
        beq     LB87B
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        bne     LB883
LB87B:  lda     $0580,x
        ora     #$40
        sta     $0580,x
LB883:  lda     $0500,x
        cmp     #$FF
        beq     LB88E
        lda     #$60
        bne     LB890
LB88E:  lda     #$64
LB890:  sta     $05C0,x
        ldy     #$0F
        jsr     LF642
LB898:  bcc     LB8A2
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
LB8A2:  rts

LB8A3:  lda     $0560,x
        bne     LB8C7
        lda     $0540,x
        tay
        lda     LB932,y
        sta     $0440,x
        lda     LB938,y
        sta     $0460,x
        lda     LB93E,y
        sta     $0400,x
        lda     LB944,y
        sta     $0420,x
        inc     $0560,x
LB8C7:  ldy     #$0F
        jsr     LF67C
        lda     $10
        and     #$10
        beq     LB8D5
        jmp     LB8FB

LB8D5:  lda     $04A0,x
        and     #$01
        beq     LB8EC
        ldy     #$0C
        jsr     LF580
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        jmp     LB8F9

LB8EC:  ldy     #$0D
        jsr     LF5C4
        lda     $0580,x
        ora     #$40
        sta     $0580,x
LB8F9:  bcc     LB925
LB8FB:  dec     $0300,x
        lda     #$00
        sta     $0520,x
        sta     $0560,x
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
        lda     $0540,x
        tay
        lda     LB7CC,y
        sta     $0440,x
        sta     $0400,x
        lda     LB7D2,y
        sta     $0460,x
        sta     $0420,x
        rts

LB925:  lda     $0460,x
        bpl     LB931
        lda     $03C0,x
        cmp     #$20
        bcs     LB8FB
LB931:  rts

LB932:  .byte   $A2,$4F,$B4,$44,$00,$9E
LB938:  .byte   $01,$02,$02,$03,$04,$04
LB93E:  .byte   $00,$80,$00,$80,$00,$80
LB944:  .byte   $01,$01,$02,$02,$03,$03
LB94A:  lda     $0520,x
        bne     LB973
        lda     $03C0,x
        cmp     #$45
        bcc     LB972
        jsr     LB9B5
        lda     $0540,x
        tay
        lda     LB7CC,y
        sta     $0440,x
        sta     $0400,x
        lda     LB7D2,y
        sta     $0460,x
        sta     $0420,x
        inc     $0520,x
LB972:  rts

LB973:  lda     $03C0,x
        cmp     #$45
        bcs     LB972
        jsr     LB9B5
        lda     #$00
        sta     $0520,x
        sta     $0560,x
        inc     $0300,x
        rts

LB989:  lda     $04A0,x
        and     #$0C
        beq     LB996
        lda     #$62
        sta     $05C0,x
        rts

LB996:  lda     $04A0,x
        and     #$01
        beq     LB9A7
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        bne     LB9AF
LB9A7:  lda     $0580,x
        ora     #$40
        sta     $0580,x
LB9AF:  lda     #$64
        sta     $05C0,x
        rts

LB9B5:  jsr     LFC53
        bcs     LB9E7
        sty     L0000
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     $03C0,x
        sec
        sbc     #$0C
        sta     $03C0,y
        lda     #$00
        sta     $0320,y
        sta     $0480,y
        sta     $04E0,y
        lda     #$67
        jsr     LF846
LB9E7:  rts

        .byte   $00,$00,$00,$00
LB9EC:  jsr     LFC53
        bcs     LBA2E
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$F2
        sta     $0320,y
        lda     #$CA
        sta     $0480,y
        lda     #$01
        sta     $04E0,y
        lda     #$66
        jsr     LF846
        lda     $04A0,y
        and     #$01
        bne     LBA2E
        lda     $0580,y
        and     #$BF
        sta     $0580,y
LBA2E:  rts

LBA2F:  lda     $0300,x
        and     #$0F
        bne     LBA4E
        sta     $0460,x
        sta     $0540,x
        lda     #$80
        sta     $0440,x
        lda     #$32
        sta     $0500,x
        lda     #$F0
        sta     $0520,x
        inc     $0300,x
LBA4E:  jsr     LFB7B
        bcs     LBA62
        lda     #$18
        jsr     LF89A
        ldy     $10
        lda     #$00
        sta     $0300,y
        jmp     LBAA6

LBA62:  lda     $0300,x
        and     #$02
        bne     LBA7A
        jsr     LF779
        dec     $0500,x
        bne     LBA79
        lda     #$02
        sta     $0500,x
        inc     $0300,x
LBA79:  rts

LBA7A:  lda     $04A0,x
        and     #$01
        bne     LBA87
        jsr     LF779
        jmp     LBA8A

LBA87:  jsr     LF759
LBA8A:  dec     $0500,x
        bne     LBA9C
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$04
        sta     $0500,x
LBA9C:  dec     $0520,x
        bne     LBAB6
        lda     #$90
        sta     $0580,x
LBAA6:  lda     #$59
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$19
        sta     $0320,x
        rts

LBAB6:  lda     $0540,x
        bne     LBACB
        lda     $0520,x
        cmp     #$02
        bcs     LBACA
        lda     #$F2
        sta     $0520,x
        inc     $0540,x
LBACA:  rts

LBACB:  lda     $0520,x
        cmp     #$78
        bcs     LBACA
        lda     $0580,x
        eor     #$04
        sta     $0580,x
        rts

LBADB:  lda     $0300,x
        and     #$0F
        bne     LBAED
        jsr     LBB25
        lda     #$3C
        sta     $0560,x
        inc     $0300,x
LBAED:  lda     $0300,x
        and     #$02
        bne     LBAFC
        dec     $0560,x
        bne     LBB24
        inc     $0300,x
LBAFC:  dec     $0500,x
        bne     LBB10
        jsr     LBB91
        jsr     LBB25
        lda     #$94
        sta     $0580,x
        dec     $0520,x
        rts

LBB10:  lda     $0520,x
        bne     LBB24
        lda     $0500,x
        cmp     #$3C
        bcs     LBB24
        lda     #$90
        sta     $0580,x
        inc     $0520,x
LBB24:  rts

LBB25:  lda     #$78
        sta     $0500,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        cmp     #$02
        bcs     LBB53
        tay
        lda     LBB6D,y
        sta     $0360,x
        lda     LBB6F,y
        sta     $04A0,x
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$0F
        tay
        lda     LBB71,y
        sta     $03C0,x
        rts

LBB53:  lda     #$CC
        sta     $03C0,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$0F
        tay
        lda     LBB81,y
        sta     $0360,x
        lda     #$08
        sta     $04A0,x
        rts

LBB6D:  .byte   $14,$EC
LBB6F:  .byte   $01,$02
LBB71:  .byte   $48,$58,$68,$78,$88,$98,$A8,$B8
        .byte   $88,$B8,$A8,$98,$88,$78,$68,$58
LBB81:  .byte   $28,$38,$48,$58,$68,$78,$88,$98
        .byte   $A8,$B8,$C8,$D8,$B8,$A8,$98,$88
LBB91:  jsr     LFC53
        bcs     LBC07
        sty     L0000
        lda     $0380,x
        sta     $0380,y
        lda     #$00
        sta     $04E0,y
        sta     $0400,y
        sta     $0440,y
        lda     #$20
        sta     $0480,y
        lda     $04A0,x
        and     #$08
        bne     LBBD8
        lda     #$5D
        jsr     LF846
        lda     $04A0,x
        sta     $04A0,y
        and     #$01
        tay
        lda     $0360,x
        clc
        adc     LBC08,y
        ldy     L0000
        sta     $0360,y
        lda     $03C0,x
        sta     $03C0,y
        jmp     LBBF2

LBBD8:  lda     $04A0,x
        sta     $04A0,y
        lda     #$5C
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $03C0,x
        sec
        sbc     #$18
        sta     $03C0,y
LBBF2:  lda     #$F4
        sta     $0320,y
        lda     $0580,y
        ora     #$02
        sta     $0580,y
        lda     #$02
        sta     $0420,y
        sta     $0460,y
LBC07:  rts

LBC08:  .byte   $E8,$18
LBC0A:  lda     $04A0,x
        and     #$08
        beq     LBC20
        ldy     #$09
        jsr     LF642
        lda     $03C0,x
        cmp     #$48
        bcs     LBC48
        jmp     LBC36

LBC20:  lda     $04A0,x
        and     #$01
        beq     LBC2F
        ldy     #$08
        jsr     LF580
        jmp     LBC34

LBC2F:  ldy     #$09
        jsr     LF5C4
LBC34:  bcc     LBC48
LBC36:  lda     $04A0,x
        and     #$08
        beq     LBC3D
LBC3D:  lda     #$00
        sta     $0300,x
        lda     #$FF
        sta     $04C0,x
        rts

LBC48:  jsr     LF8C2
        cmp     #$18
        bcs     LBC75
        jsr     LF8B3
        cmp     #$14
        bcs     LBC75
        lda     $04A0,x
        and     #$08
        beq     LBC5E
        rts

LBC5E:  lda     $04A0,x
        and     #$02
        bne     LBC69
        lda     #$01
        bne     LBC6B
LBC69:  lda     #$02
LBC6B:  sta     $36
        lda     #$00
        sta     $37
        lda     #$02
        sta     $38
LBC75:  lda     $04A0,x
        and     #$08
        beq     LBC7C
LBC7C:  rts

LBC7D:  lda     $0300,x
        and     #$0F
        bne     LBCC2
        lda     #$09
        cmp     $30
        beq     LBC99
        sta     $30
        lda     #$80
        sta     $B0
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
LBC99:  lda     $B0
        cmp     #$9C
        bne     LBCFA
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        lda     #$00
        sta     $01
        sta     $02
        jsr     LBDE2
        lda     #$3C
        sta     $0500,x
        lda     #$36
        sta     $0520,x
        lda     #$01
        sta     $04A0,x
        inc     $0300,x
LBCC2:  lda     $0300,x
        and     #$0F
        cmp     #$02
        beq     LBCFB
        cmp     #$03
        bne     LBCD2
        jmp     LBD82

LBCD2:  lda     $0580,x
        and     #$04
        beq     LBCE6
        dec     $0500,x
        bne     LBCFA
        lda     $0580,x
        eor     #$04
        sta     $0580,x
LBCE6:  lda     $05A0,x
        cmp     #$04
        bne     LBCFA
        lda     #$04
        jsr     LF835
        lda     #$3C
        sta     $0500,x
        inc     $0300,x
LBCFA:  rts

LBCFB:  lda     $04A0,x
        and     #$01
        beq     LBD10
        lda     $0580,x
        ora     #$40
        sta     $0580,x
        jsr     LF71D
        jmp     LBD1B

LBD10:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        jsr     LF73B
LBD1B:  dec     $0520,x
        bne     LBD30
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$6C
        sta     $0520,x
        inc     $0540,x
LBD30:  lda     $0540,x
        cmp     #$02
        bcc     LBD50
        lda     $0520,x
        cmp     #$36
        bcs     LBD50
        inc     $0300,x
        lda     #$1E
        sta     $0560,x
        lda     #$A1
        sta     $0480,x
        lda     #$01
        jmp     LF835

LBD50:  dec     $0500,x
        bne     LBD6C
        lda     #$05
        jsr     LF835
        jsr     LBEB8
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$03
        tay
        lda     LBDDE,y
        sta     $0500,x
LBD6C:  lda     $05C0,x
        cmp     #$04
        beq     LBD81
        lda     $05A0,x
        beq     LBD81
        cmp     #$03
        bne     LBD81
        lda     #$04
        jsr     LF835
LBD81:  rts

LBD82:  lda     $05C0,x
        cmp     #$13
        beq     LBD99
        dec     $0560,x
        bne     LBD98
        lda     #$13
        jsr     LF835
        lda     #$3C
        sta     $0560,x
LBD98:  rts

LBD99:  lda     $0580,x
        and     #$04
        bne     LBDB2
        lda     $05A0,x
        cmp     #$04
        bne     LBD98
        jsr     LBE6C
        lda     $0580,x
        eor     #$04
        sta     $0580,x
LBDB2:  dec     $0560,x
        bne     LBD98
        lda     #$06
        sta     $0500,x
        lda     #$36
        sta     $0520,x
        lda     #$00
        sta     $0540,x
        sta     $0560,x
        lda     #$13
        jsr     LF835
        lda     #$94
        sta     $0580,x
        lda     #$80
        sta     $0360,x
        lda     #$C1
        sta     $0300,x
        rts

LBDDE:  .byte   $1E,$3C,$1E,$3C
LBDE2:  jsr     LFC53
        bcs     LBE4B
        sty     L0000
        lda     $0380,x
        sta     $0380,y
        ldy     $01
        lda     LBE5C,y
        ldy     L0000
        sta     $04A0,y
        lda     #$80
        sta     $0360,y
        ldy     $01
        lda     LBE4C,y
        ldy     L0000
        sta     $03C0,y
        lda     #$01
        sta     $0420,y
        lda     #$13
        jsr     LF846
        lda     #$00
        sta     $0540,y
        sta     $0560,y
        sta     $0400,y
        lda     #$F5
        sta     $0320,y
        sta     $04E0,y
        sta     $04C0,y
        lda     #$81
        sta     $0300,y
        lda     #$3C
        sta     $0500,y
        lda     #$36
        sta     $0520,y
        lda     #$94
        sta     $0580,y
        lda     #$81
        sta     $0480,y
        inc     $01
        inc     $02
        lda     $02
        cmp     #$02
        bcc     LBDE2
LBE4B:  rts

LBE4C:  .byte   $74,$C4,$24,$74,$C4,$24,$C4,$24
        .byte   $C4,$74,$24,$74,$24,$C4,$24,$C4
LBE5C:  .byte   $02,$01,$01,$02,$01,$01,$01,$01
        .byte   $01,$02,$01,$02,$01,$01,$01,$01
LBE6C:  lda     $04C0,x
        cmp     #$2C
        bne     LBE9A
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$07
        tay
        lda     LBEA0,y
        sta     $03C0,x
        lda     LBEA8,y
        sta     $04A0,x
        lda     #$C1
        sta     $0480,x
        lda     LBEB0,y
        sta     $01
        lda     #$00
        sta     $02
        jsr     LBDE2
        rts

LBE9A:  lda     #$00
        sta     $0300,x
        rts

LBEA0:  .byte   $24,$C4,$74,$74,$24,$C4,$74,$74
LBEA8:  .byte   $01,$01,$02,$02,$01,$01,$02,$02
LBEB0:  .byte   $00,$02,$04,$06,$08,$0A,$0C,$0E
LBEB8:  jsr     LFC53
        bcs     LBF14
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$01
        tay
        lda     $0360,x
        clc
        adc     LBF15,y
        ldy     L0000
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        lda     #$19
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
        lda     #$73
        jsr     LF846
        lda     #$8F
        sta     $0320,y
        lda     #$8B
        sta     $0480,y
LBF14:  rts

LBF15:  .byte   $EE,$12,$00,$00,$00,$20,$00,$00
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
