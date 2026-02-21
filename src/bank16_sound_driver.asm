; da65 V2.18 - Ubuntu 2.19-1
; Created:    2026-02-21 05:24:06
; Input file: /home/kn/megamanforever/megaman3-disassembly-ca65/tools/../build/bank16.bin
; Page:       1


        .setcpu "6502"

L00C1           := $00C1

.segment "BANK16"

L8000:  .byte   $4C
L8001:  jmp     ($4C80)

        .byte   $FE
        .byte   $80
L8006:  lda     #$00
        sta     $C2
        ldy     #$08
L800C:  asl     $C2
        rol     L00C1
        bcc     L801F
        clc
        lda     $C2
        adc     $C4
        sta     $C2
        lda     L00C1
        adc     #$00
        sta     L00C1
L801F:  dey
        bne     L800C
        rts

L8023:  asl     a
        tay
        iny
        pla
        sta     L00C1
        pla
        sta     $C2
        lda     (L00C1),y
        pha
        iny
        lda     (L00C1),y
        sta     $C2
        pla
        sta     L00C1
        jmp     (L00C1)

L803A:  sty     L00C1
        ldy     #$00
        cmp     #$C0
        bcs     L8047
        sta     $C2
        lda     (L00C1),y
        rts

L8047:  sec
        sbc     #$20
        sta     $C2
        lda     #$07
        sta     L8000
        lda     #$18
        sta     L8001
        lda     (L00C1),y
        pha
        lda     #$07
        sta     L8000
        lda     #$17
        sta     L8001
        lda     #$20
        clc
        adc     $C2
        sta     $C2
        pla
        rts

L806C:  lda     $C0
        lsr     a
        bcs     L80D7
        lda     $D0
        ora     $D1
        beq     L807A
        jsr     L8252
L807A:  clc
        lda     $CA
        adc     $C8
        sta     $C8
        lda     $C9
        adc     #$00
        sta     $C7
        lda     $CF
        pha
        ldx     #$03
L808C:  lsr     $CF
        bcc     L8099
        lda     $CF
        ora     #$80
        sta     $CF
        jsr     L82DE
L8099:  lda     $C0
        and     #$02
        bne     L80A6
        txa
        pha
        jsr     L8393
        pla
        tax
L80A6:  dex
        bpl     L808C
        pla
        sta     $CF
        lsr     $C0
        asl     $C0
        lda     $CC
        and     #$7F
        beq     L80D7
        ldy     #$00
        sty     L00C1
        ldy     #$04
L80BC:  asl     a
        rol     L00C1
        dey
        bne     L80BC
        clc
        adc     $C0
        sta     $C0
        lda     L00C1
        adc     $CD
        bcc     L80D5
        lda     $CC
        and     #$80
        sta     $CC
        lda     #$FF
L80D5:  sta     $CD
L80D7:  rts

L80D8:  txa
        and     #$03
        eor     #$03
        asl     a
        asl     a
        tay
        lda     #$30
        cpy     #$08
        bne     L80E8
        lda     #$00
L80E8:  sta     $4000,y
        rts

L80EC:  pha
        txa
        and     #$03
        eor     #$03
        asl     a
        asl     a
        sty     $C4
        ora     $C4
        tay
        pla
        sta     $4000,y
        rts

L80FE:  inc     $C0
        jsr     L8106
        dec     $C0
        rts

L8106:  cmp     #$F0
        bcc     L810D
        jmp     L81AE

L810D:  cmp     L8A40
        bcc     L8118
        sec
        sbc     L8A40
        bcs     L810D
L8118:  asl     a
        tax
        ldy     L8A44,x
        tya
        ora     L8A43,x
        beq     L816E
        lda     L8A43,x
        jsr     L803A
        tay
        beq     L816F
        ldy     #$00
        inx
        sta     $C4
        and     #$7F
        cmp     $CE
        bcc     L816E
        sta     $CE
        bne     L8145
        lda     $D6
        bpl     L8145
        lda     $C4
        bmi     L8145
        sty     $D7
L8145:  sty     $D6
        asl     $C4
        ror     $D6
        bpl     L814F
        stx     $D7
L814F:  inc     L00C1
        lda     L00C1
        sta     $D0
        bne     L8159
        inc     $C2
L8159:  lda     $C2
        sta     $D1
        tya
        sta     $D2
        sta     $D3
        sta     $D4
        sta     $D5
        ldy     #$27
L8168:  sta     $0700,y
        dey
        bpl     L8168
L816E:  rts

L816F:  ldx     #$01
        stx     $C9
        ldx     #$99
        stx     $CA
        sta     $C8
        sta     $CB
        sta     $CC
        sta     $CD
        ldx     #$53
L8181:  sta     $0728,x
        dex
        bpl     L8181
        ldx     #$03
L8189:  inc     L00C1
        bne     L818F
        inc     $C2
L818F:  ldy     L00C1
        lda     $C2
        jsr     L803A
        sta     $0754,x
        inc     L00C1
        bne     L819F
        inc     $C2
L819F:  ldy     L00C1
        lda     $C2
        jsr     L803A
        sta     $0750,x
        dex
        bpl     L8189
        bmi     L81F1
L81AE:  sty     $C3
        and     #$07
        jsr     L8023
        .byte   $C5,$81,$C8,$81,$E4,$81,$1E,$82
        .byte   $26,$82,$2D,$82,$34,$82,$4A,$82
        jsr     L81E4
L81C8:  lda     #$00
        sta     $CE
        sta     $D0
        sta     $D1
        sta     $D7
        sta     $D8
L81D4:  lda     $CF
        beq     L81E3
        eor     #$0F
        sta     $CF
        jsr     L81F1
        lda     #$00
        sta     $CF
L81E3:  rts

L81E4:  lda     #$00
        ldx     #$03
L81E8:  sta     $0754,x
        sta     $0750,x
        dex
        bpl     L81E8
L81F1:  lda     $CF
        pha
        ldx     #$03
L81F6:  lsr     $CF
        bcs     L820A
        jsr     L80D8
        lda     $0754,x
        ora     $0750,x
        beq     L820A
        lda     #$FF
        sta     $077C,x
L820A:  dex
        bpl     L81F6
        pla
        sta     $CF
        lda     #$08
        sta     $4001
        sta     $4005
        lda     #$0F
        sta     $4015
        rts

        lda     $C0
        ora     #$02
        sta     $C0
        bne     L81F1
        lda     $C0
        and     #$FD
        sta     $C0
        rts

        asl     $C3
        beq     L8234
        sec
        ror     $C3
L8234:  lda     $C0
        and     #$0F
        sta     $C0
        ldy     $C3
        sty     $CC
        beq     L8247
        ldy     #$FF
        cpy     $CD
        bne     L8249
        iny
L8247:  sty     $CD
L8249:  rts

        lda     #$00
        sec
        sbc     $C3
        sta     $D8
        rts

L8252:  lda     $D3
        beq     L825B
        dec     $D3
        dec     $D5
        rts

L825B:  jsr     L8386
        sta     $C4
        asl     a
        bcc     L8273
        sty     $CE
        lda     $D7
        lsr     a
        bcc     L8270
        jsr     L8118
        jmp     L825B

L8270:  jmp     L81C8

L8273:  lsr     $C4
        bcc     L82A6
        jsr     L8386
        asl     a
        beq     L8289
        asl     $D6
        php
        cmp     $D6
        beq     L8296
        plp
        ror     $D6
        inc     $D6
L8289:  jsr     L8386
        tax
        jsr     L8386
        sta     $D0
        stx     $D1
        bne     L825B
L8296:  tya
        plp
        ror     a
        sta     $D6
        clc
        lda     #$02
        adc     $D0
        sta     $D0
        bcc     L82A6
        inc     $D1
L82A6:  lsr     $C4
        bcc     L82AF
        jsr     L8386
        sta     $D4
L82AF:  lsr     $C4
        bcc     L82B8
        jsr     L8386
        sta     $D2
L82B8:  jsr     L8386
        sta     $D3
        sta     L00C1
        lda     $D4
        sta     $C4
        jsr     L8006
        ldy     L00C1
        iny
        sty     $D5
        inc     $C0
        jsr     L8386
        pha
        eor     $CF
        beq     L82DA
        sta     $CF
        jsr     L81D4
L82DA:  pla
        sta     $CF
        rts

L82DE:  ldy     $0700,x
        beq     L82E6
        jsr     L8684
L82E6:  lda     $C0
        lsr     a
        bcs     L830A
        jsr     L86BA
        lda     $D3
        beq     L82FA
        cpx     #$01
        beq     L82FB
        lda     $D5
        beq     L8300
L82FA:  rts

L82FB:  dec     $0710,x
        bne     L82FA
L8300:  lda     $0704,x
        and     #$04
        bne     L82FA
        jmp     L85A3

L830A:  lda     #$00
        sta     $C4
        jsr     L8386
L8311:  lsr     a
        bcc     L8320
        pha
        jsr     L8386
        sta     $C3
        lda     $C4
        jsr     L8326
        pla
L8320:  beq     L8333
        inc     $C4
        bne     L8311
L8326:  jsr     L8023
        .byte   $6F,$86,$AD,$86,$5A,$86,$A7,$86
        .byte   $A1,$86
L8333:  jsr     L8386
        tay
        bne     L8349
        sta     $0710,x
        lda     $0704,x
        and     #$F8
        ora     #$04
        sta     $0704,x
        jmp     L80D8

L8349:  lda     $0704,x
        ora     #$20
        sta     $0704,x
        lda     $0718,x
        asl     a
        lda     #$54
        bcs     L835B
        lda     #$0A
L835B:  sta     $071C,x
        tya
        bpl     L836B
        cpx     #$01
        bne     L8368
        jsr     L85AE
L8368:  jmp     L8644

L836B:  jsr     L85AE
        lda     #$FF
        sta     $077C,x
        dey
        txa
        bne     L837F
        sta     $C3
        tya
        eor     #$0F
        jmp     L8636

L837F:  tya
        clc
        adc     $D2
        jmp     L85DE

L8386:  ldy     $D0
        lda     $D1
        inc     $D0
        bne     L8390
        inc     $D1
L8390:  jmp     L803A

L8393:  txa
        ora     #$28
        tax
        lda     $0728,x
        ora     $072C,x
        beq     L83CC
        lda     $0738,x
        beq     L83CD
        ldy     $0700,x
        beq     L83AF
        jsr     L8684
        jsr     L86BA
L83AF:  lda     $0740,x
        sec
        sbc     $C7
        sta     $0740,x
        beq     L83BC
        bcs     L83BF
L83BC:  jsr     L85A3
L83BF:  lda     $0738,x
        sec
        sbc     $C7
        sta     $0738,x
        beq     L83CD
        bcc     L83CD
L83CC:  rts

L83CD:  jsr     L8592
        cmp     #$20
        bcs     L83DA
        jsr     L8497
        jmp     L83CD

L83DA:  pha
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$07
        tay
        dey
        lda     $0730,x
        asl     a
        asl     a
        bpl     L83EF
        lda     L8915,y
        bne     L8406
L83EF:  asl     a
        asl     a
        lda     L891C,y
        bcc     L8406
        sta     $C3
        lda     $0730,x
        and     #$EF
        sta     $0730,x
        lda     $C3
        lsr     a
        clc
        adc     $C3
L8406:  clc
        adc     $0738,x
        sta     $0738,x
        tay
        pla
        and     #$1F
        bne     L8419
        jsr     L85A3
        jmp     L8491

L8419:  pha
        sty     $C4
        lda     $073C,x
        sta     L00C1
        jsr     L8006
        lda     L00C1
        bne     L842A
        lda     #$01
L842A:  sta     $0740,x
        pla
        tay
        dey
        lda     $0730,x
        bpl     L8440
        lda     $0718,x
        bne     L8454
        jsr     L8644
        jmp     L847E

L8440:  jsr     L85AE
        lda     $CF
        bmi     L8454
        sty     $C3
        txa
        and     #$03
        tay
        lda     #$FF
        sta     $077C,y
        ldy     $C3
L8454:  txa
        and     #$03
        bne     L8466
        sta     $C3
        tya
        and     #$0F
        eor     #$0F
        jsr     L8636
        jmp     L847E

L8466:  sty     $C3
        lda     $0730,x
        and     #$0F
        tay
        lda     L8923,y
        clc
        adc     $C3
        clc
        adc     $CB
        clc
        adc     $0734,x
        jsr     L85DE
L847E:  lda     $0730,x
        tay
        and     #$40
        asl     a
        sta     $C4
        tya
        and     #$7F
        ora     $C4
        sta     $0730,x
        bpl     L8496
L8491:  lda     #$FF
        sta     $0740,x
L8496:  rts

L8497:  cmp     #$04
        bcc     L84A4
        sta     $C4
        jsr     L8592
        sta     $C3
        lda     $C4
L84A4:  jsr     L8023
        .byte   $D9,$84,$DD,$84,$E1,$84,$E8,$84
        .byte   $75,$85,$F1,$84,$FF,$84,$5A,$86
        .byte   $6F,$86,$05,$85,$10,$85,$15,$85
        .byte   $A1,$86,$A7,$86,$1B,$85,$1F,$85
        .byte   $23,$85,$27,$85,$1B,$85,$1F,$85
        .byte   $23,$85,$27,$85,$5A,$85,$80,$85
        .byte   $AD,$86
        lda     #$20
        bne     L84EA
        lda     #$40
        bne     L84EA
        lda     #$10
        ora     $0730,x
        bne     L84ED
        lda     #$08
L84EA:  eor     $0730,x
L84ED:  sta     $0730,x
        rts

        lda     #$00
        sta     $C8
        jsr     L8592
        ldy     $C3
        sta     $CA
        sty     $C9
        rts

        lda     $C3
        sta     $073C,x
        rts

        lda     $0730,x
        and     #$F8
        ora     $C3
        sta     $0730,x
        rts

        lda     $C3
        sta     $CB
        rts

        lda     $C3
        sta     $0734,x
        rts

        lda     #$00
        beq     L8529
        lda     #$04
        bne     L8529
        lda     #$08
        bne     L8529
        lda     #$0C
L8529:  sta     $C2
        txa
        clc
        adc     $C2
        tay
        lda     $C4
        cmp     #$12
        bcs     L8547
        lda     $0744,y
        sec
        sbc     #$01
        bcs     L8540
        lda     $C3
L8540:  sta     $0744,y
        beq     L8566
        bne     L8555
L8547:  lda     $0744,y
        sec
        sbc     #$01
        bne     L8566
        sta     $0744,y
        jsr     L8575
L8555:  jsr     L8592
        sta     $C3
        jsr     L8592
        sta     $0728,x
        lda     $C3
        sta     $072C,x
        rts

L8566:  lda     #$02
        clc
        adc     $0728,x
        sta     $0728,x
        bcc     L8574
        inc     $072C,x
L8574:  rts

L8575:  lda     $0730,x
        and     #$97
        ora     $C3
        sta     $0730,x
        rts

        pla
        pla
        lda     #$00
        sta     $0728,x
        sta     $072C,x
        lda     $CF
        bmi     L8591
        jmp     L80D8

L8591:  rts

L8592:  ldy     $0728,x
        lda     $072C,x
        inc     $0728,x
        bne     L85A0
        inc     $072C,x
L85A0:  jmp     L803A

L85A3:  lda     $0704,x
        and     #$F8
        ora     #$03
        sta     $0704,x
        rts

L85AE:  tya
        pha
        ldy     #$00
        lda     $0704,x
        and     #$F8
        sta     $0704,x
        cpx     #$29
        beq     L85D0
        cpx     #$01
        bne     L85D7
        lda     $D3
        sta     L00C1
        lda     $070C,x
        sta     $C4
        jsr     L8006
        ldy     L00C1
L85D0:  iny
        inc     $0704,x
        inc     $0704,x
L85D7:  tya
        sta     $0710,x
        pla
        tay
        rts

L85DE:  cmp     #$60
        bcc     L85E4
        lda     #$5F
L85E4:  sta     $C3
        inc     $C3
        cpx     #$28
        bcc     L862A
        lda     $071C,x
        beq     L861D
        cmp     $C3
        bne     L85FC
        lda     $0730,x
        bpl     L861D
        bmi     L8644
L85FC:  lda     $0718,x
        beq     L861D
        bcs     L8607
        ora     #$80
        bne     L8609
L8607:  and     #$7F
L8609:  sta     $0718,x
        lda     $0704,x
        ora     #$20
        sta     $0704,x
        lda     $C3
        ldy     $071C,x
        sty     $C3
        bne     L8627
L861D:  lda     $0704,x
        and     #$DF
        sta     $0704,x
        lda     $C3
L8627:  sta     $071C,x
L862A:  asl     $C3
        ldy     $C3
        lda     L8959,y
        sta     $C3
        lda     L895A,y
L8636:  sta     $0724,x
        lda     $C3
        sta     $0720,x
        ldy     #$04
        lda     ($C5),y
        bmi     L864C
L8644:  lda     $0704,x
        and     #$08
        bne     L864C
        rts

L864C:  lda     #$00
        sta     $0708,x
        lda     $0704,x
        and     #$37
        sta     $0704,x
        rts

        cpx     #$01
        bne     L8662
        lda     $C3
        bne     L866B
L8662:  lda     $070C,x
        and     #$C0
        ora     $C3
        ora     #$30
L866B:  sta     $070C,x
        rts

        inc     $C3
        lda     $C3
        cmp     $0700,x
        beq     L86A0
        sta     $0700,x
        tay
        lda     $0704,x
        ora     #$08
        sta     $0704,x
L8684:  dey
        lda     #$00
        sta     $C3
        tya
        asl     a
        rol     $C3
        asl     a
        rol     $C3
        asl     a
        rol     $C3
        clc
        adc     L8A42
        sta     $C5
        lda     $C3
        adc     L8A41
        sta     $C6
L86A0:  rts

        lda     $C3
        sta     $0714,x
        rts

        lda     $C3
        sta     $0718,x
        rts

        lda     $070C,x
        and     #$0F
        ora     $C3
        ora     #$30
        sta     $070C,x
        rts

L86BA:  lda     $0710,x
        sta     $C4
        lda     $0704,x
        and     #$07
        jsr     L8023
        .byte   $D1,$86,$E6,$86,$20,$87,$02,$87
        .byte   $14,$89
        ldy     #$00
        lda     ($C5),y
        tay
        lda     $C4
        clc
        adc     L8933,y
        bcs     L86E2
        cmp     #$F0
        bcc     L871D
L86E2:  lda     #$F0
        bne     L871A
        ldy     #$01
        lda     ($C5),y
        beq     L86FB
        tay
        lda     $C4
        sec
        sbc     L8933,y
        bcc     L86FB
        ldy     #$02
        cmp     ($C5),y
        bcs     L871D
L86FB:  ldy     #$02
        lda     ($C5),y
        jmp     L871A

        txa
        and     #$03
        cmp     #$01
        beq     L8718
        ldy     #$03
        lda     ($C5),y
        beq     L8720
        tay
        lda     $C4
        sec
        sbc     L8933,y
        bcs     L871D
L8718:  lda     #$00
L871A:  inc     $0704,x
L871D:  sta     $0710,x
L8720:  cpx     #$28
        bcc     L8737
        lda     $CF
        bpl     L872B
        jmp     L88A0

L872B:  lda     $CD
        ldy     $CC
        bmi     L8733
        eor     #$FF
L8733:  cmp     #$FF
        bne     L8740
L8737:  txa
        and     #$03
        cmp     #$01
        bne     L8760
        beq     L8752
L8740:  cpx     #$29
        bne     L875B
        sta     $C4
        lda     $0740,x
        sta     L00C1
        jsr     L8006
        lda     L00C1
        beq     L87AA
L8752:  lda     $0710,x
        beq     L87AA
        lda     #$FF
        bne     L87AA
L875B:  cmp     $0710,x
        bcc     L8763
L8760:  lda     $0710,x
L8763:  lsr     a
        lsr     a
        lsr     a
        lsr     a
        eor     #$0F
        sta     $C3
        ldy     #$06
        lda     ($C5),y
        cmp     #$05
        bcc     L8797
        sta     $C4
        ldy     $0708,x
        lda     $0704,x
        asl     a
        asl     a
        tya
        bcc     L8782
        eor     #$FF
L8782:  beq     L8797
        sta     L00C1
        jsr     L8006
        lda     L00C1
        lsr     a
        lsr     a
        cmp     #$10
        bcs     L87A5
        cmp     $C3
        bcc     L8797
        sta     $C3
L8797:  lda     #$10
        sta     $C4
        lda     $070C,x
        sec
        sbc     $C3
        bit     $C4
        bne     L87AA
L87A5:  lda     $070C,x
        and     #$F0
L87AA:  ldy     #$00
        jsr     L80EC
        txa
        and     #$03
        tay
        lda     $077C,y
        bmi     L880C
        ldy     #$05
        lda     ($C5),y
        beq     L880C
        sta     $C4
        ldy     $0708,x
        lda     $0704,x
        asl     a
        asl     a
        tya
        bcc     L87CD
        eor     #$FF
L87CD:  beq     L880C
        sta     L00C1
        jsr     L8006
        lda     L00C1
        lsr     a
        ror     $C2
        lsr     a
        ror     $C2
        lsr     a
        ror     $C2
        lsr     a
        ror     $C2
        tay
        ora     $C2
        beq     L880C
        lda     $0704,x
        bmi     L87FA
        clc
        lda     $C2
        adc     $0720,x
        sta     $C2
        tya
        adc     $0724,x
        bne     L8809
L87FA:  sec
        lda     $0720,x
        sbc     $C2
        sta     $C2
        lda     $0724,x
        sty     L00C1
        sbc     L00C1
L8809:  tay
        bne     L8814
L880C:  lda     $0720,x
        sta     $C2
        ldy     $0724,x
L8814:  cpx     #$28
        bcs     L8835
        lda     $D6
        bpl     L8835
        lda     $D8
        beq     L8835
        sta     $C4
        sty     L00C1
        lda     $C2
        pha
        jsr     L8006
        pla
        clc
        adc     $C2
        sta     $C2
        lda     #$00
        adc     L00C1
        tay
L8835:  txa
        and     #$03
        bne     L8849
        tya
        and     #$0F
        ldy     #$07
        ora     ($C5),y
        sta     $C2
        lda     #$00
        sta     L00C1
        beq     L8884
L8849:  tya
        ldy     #$08
L884C:  dey
        cmp     L8953,y
        bcc     L884C
        sta     L00C1
        tya
        clc
        adc     L00C1
        tay
        and     #$07
        clc
        adc     #$07
        sta     L00C1
        tya
        and     #$38
        eor     #$38
        beq     L8870
L8867:  lsr     L00C1
        ror     $C2
        sec
        sbc     #$08
        bne     L8867
L8870:  ldy     #$00
        lda     $0714,x
        beq     L8884
        bpl     L887A
        dey
L887A:  clc
        adc     $C2
        sta     $C2
        tya
        adc     L00C1
        sta     L00C1
L8884:  ldy     #$02
        lda     $C2
        jsr     L80EC
        txa
        and     #$03
        tay
        lda     L00C1
        cmp     $077C,y
        beq     L88A0
        sta     $077C,y
        ora     #$08
        ldy     #$03
        jsr     L80EC
L88A0:  lda     $0704,x
        and     #$20
        beq     L88FA
        lda     $0718,x
        beq     L88F2
        ldy     #$00
        asl     a
        php
        bcc     L88B8
        eor     #$FF
        clc
        adc     #$01
        dey
L88B8:  clc
        adc     $0720,x
        sta     $0720,x
        tya
        adc     $0724,x
        sta     $0724,x
        lda     $071C,x
        asl     a
        tay
        sec
        lda     $0720,x
        sbc     L8959,y
        lda     $0724,x
        and     #$3F
        sbc     L895A,y
        lda     #$FF
        adc     #$00
        plp
        adc     #$00
        bne     L88FA
        txa
        beq     L88FA
        lda     L8959,y
        sta     $0720,x
        lda     L895A,y
        sta     $0724,x
L88F2:  lda     $0704,x
        and     #$DF
        sta     $0704,x
L88FA:  ldy     #$04
        lda     ($C5),y
        and     #$7F
        beq     L8914
        clc
        adc     $0708,x
        sta     $0708,x
        bcc     L8914
        lda     $0704,x
        clc
        adc     #$40
        sta     $0704,x
L8914:  rts

L8915:  .byte   $02,$04,$08,$10,$20,$40,$80
L891C:  .byte   $03,$06,$0C,$18,$30,$60,$C0
L8923:  .byte   $00,$0C,$18,$24,$30,$3C,$48,$54
        .byte   $18,$24,$30,$3C,$48,$54,$60,$6C
L8933:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0E,$0F,$10
        .byte   $12,$13,$14,$16,$18,$1B,$1E,$23
        .byte   $28,$30,$3C,$50,$7E,$7F,$FE,$FF
L8953:  .byte   $00,$07,$0E,$15,$1C,$23
L8959:  .byte   $2A
L895A:  .byte   $31,$5C,$37,$9C,$36,$E7,$35,$3C
        .byte   $35,$9B,$34,$02,$34,$72,$33,$EA
        .byte   $32,$6A,$32,$F1,$31,$80,$31,$14
        .byte   $31,$5C,$30,$9C,$2F,$E7,$2E,$3C
        .byte   $2E,$9B,$2D,$02,$2D,$72,$2C,$EA
        .byte   $2B,$6A,$2B,$F1,$2A,$80,$2A,$14
        .byte   $2A,$5C,$29,$9C,$28,$E7,$27,$3C
        .byte   $27,$9B,$26,$02,$26,$72,$25,$EA
        .byte   $24,$6A,$24,$F1,$23,$80,$23,$14
        .byte   $23,$5C,$22,$9C,$21,$E7,$20,$3C
        .byte   $20,$9B,$1F,$02,$1F,$72,$1E,$EA
        .byte   $1D,$6A,$1D,$F1,$1C,$80,$1C,$14
        .byte   $1C,$5C,$1B,$9C,$1A,$E7,$19,$3C
        .byte   $19,$9B,$18,$02,$18,$72,$17,$EA
        .byte   $16,$6A,$16,$F1,$15,$80,$15,$14
        .byte   $15,$5C,$14,$9C,$13,$E7,$12,$3C
        .byte   $12,$9B,$11,$02,$11,$72,$10,$EA
        .byte   $0F,$6A,$0F,$F1,$0E,$80,$0E,$14
        .byte   $0E,$5C,$0D,$9C,$0C,$E7,$0B,$3C
        .byte   $0B,$9B,$0A,$02,$0A,$72,$09,$EA
        .byte   $08,$6A,$08,$F1,$07,$80,$07,$14
        .byte   $07,$5C,$06,$9C,$05,$E7,$04,$3C
        .byte   $04,$9B,$03,$02,$03,$72,$02,$EA
        .byte   $01,$6A,$01,$F1,$00,$80,$00,$14
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00
L8A40:  .byte   $39
L8A41:  .byte   $8A
L8A42:  .byte   $B5
L8A43:  .byte   $8C
L8A44:  .byte   $9D,$90,$DF,$95,$9A,$9A,$06,$9C
        .byte   $D7,$A0,$BE,$A4,$14,$A7,$C1,$AA
        .byte   $C2,$AD,$E5,$B1,$23,$B2,$CB,$B4
        .byte   $16,$B5,$F6,$B8,$2B,$B9,$1E,$BE
        .byte   $97,$BF,$CF,$C0,$1D,$C5,$33,$C5
        .byte   $4E,$C5,$B4,$C5,$C0,$C5,$DE,$C6
        .byte   $22,$C6,$2D,$C6,$3D,$C6,$5A,$C6
        .byte   $6C,$C6,$82,$C6,$A9,$C6,$C6,$C6
        .byte   $D8,$C6,$F7,$C7,$0F,$C7,$33,$C7
        .byte   $49,$C7,$57,$C7,$68,$C7,$76,$C7
        .byte   $88,$C7,$A1,$C7,$ED,$C8,$03,$C8
        .byte   $14,$C8,$25,$C8,$3C,$C8,$4C,$C8
        .byte   $65,$C8,$87,$C8,$9A,$C8,$A6,$C9
        .byte   $B3,$C9,$CA,$CB,$67,$CD,$1E,$CD
        .byte   $BF,$1F,$01,$F0,$18,$80,$00,$00
        .byte   $00,$1F,$01,$F0,$18,$E4,$06,$00
        .byte   $00,$1E,$00,$F0,$10,$80,$00,$00
        .byte   $80,$1F,$13,$90,$01,$D6,$07,$00
        .byte   $00,$1F,$11,$50,$01,$E2,$00,$47
        .byte   $00,$1F,$14,$F0,$04,$EE,$04,$00
        .byte   $00,$1F,$19,$A0,$08,$00,$00,$00
        .byte   $00,$1F,$00,$B0,$09,$E3,$02,$00
        .byte   $00,$1F,$0D,$30,$01,$E3,$06,$00
        .byte   $00,$1F,$1B,$A0,$05,$CB,$02,$3C
        .byte   $00,$1F,$03,$50,$12,$00,$00,$00
        .byte   $00,$1F,$0A,$A0,$0E,$00,$00,$00
        .byte   $80,$1F,$19,$50,$10,$9E,$20,$26
        .byte   $00,$1D,$09,$90,$03,$CB,$01,$37
        .byte   $00,$1F,$1F,$E0,$1B,$9E,$58,$7C
        .byte   $00,$1F,$1B,$E0,$10,$B8,$00,$1E
        .byte   $00,$15,$14,$F0,$04,$00,$00,$00
        .byte   $00,$1A,$14,$F0,$04,$00,$00,$00
        .byte   $00,$1F,$17,$A0,$04,$80,$00,$00
        .byte   $00,$19,$01,$D0,$02,$00,$00,$00
        .byte   $00,$1F,$1D,$B0,$0C,$E4,$05,$00
        .byte   $00,$1A,$18,$90,$07,$00,$00,$00
        .byte   $00,$1F,$02,$C0,$13,$80,$00,$00
        .byte   $00,$1F,$02,$C0,$15,$5D,$06,$06
        .byte   $00,$1F,$1E,$D0,$0A,$FF,$02,$3C
        .byte   $00,$1E,$18,$90,$01,$C6,$19,$32
        .byte   $00,$1F,$17,$80,$06,$E4,$02,$28
        .byte   $00,$1F,$17,$70,$01,$5A,$03,$00
        .byte   $00,$1F,$1C,$B0,$06,$00,$00,$00
        .byte   $00,$1F,$1D,$B0,$0C,$00,$00,$00
        .byte   $80,$1F,$1A,$A0,$05,$FF,$01,$33
        .byte   $00,$1E,$1A,$A0,$05,$E4,$04,$00
        .byte   $00,$1F,$19,$A0,$02,$00,$00,$00
        .byte   $00,$1F,$04,$80,$02,$80,$00,$00
        .byte   $00,$1F,$1E,$A0,$01,$C6,$00,$32
        .byte   $00,$1F,$1F,$90,$09,$FF,$00,$2D
        .byte   $00,$1F,$1C,$80,$01,$00,$00,$00
        .byte   $00,$1F,$1E,$A0,$02,$EF,$00,$46
        .byte   $00,$17,$02,$E0,$0F,$80,$00,$00
        .byte   $00,$1F,$1B,$A0,$01,$B0,$07,$62
        .byte   $00,$1F,$1F,$F0,$1F,$00,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$02,$00
        .byte   $00,$1F,$1F,$F0,$1F,$92,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$E3,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$4C,$00
        .byte   $00,$1F,$1F,$F0,$1F,$99,$7F,$00
        .byte   $00,$1D,$1F,$F0,$1F,$80,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$B7,$27,$00
        .byte   $00,$1F,$1F,$F0,$04,$80,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$A6,$7F,$00
        .byte   $80,$1F,$1F,$F0,$1F,$80,$00,$00
        .byte   $80,$1F,$1C,$E0,$1F,$DA,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$0A,$00
        .byte   $00,$1C,$13,$10,$1F,$FF,$7F,$00
        .byte   $00,$1F,$01,$00,$0F,$E3,$7F,$00
        .byte   $00,$1F,$1F,$F0,$08,$00,$00,$00
        .byte   $00,$1F,$15,$A0,$14,$00,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$A5,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$D0,$16,$00
        .byte   $00,$1F,$1A,$A0,$07,$CF,$36,$00
        .byte   $80,$00,$8C,$A6,$8E,$28,$8F,$27
        .byte   $90,$8F,$0A,$01,$05,$01,$11,$06
        .byte   $C8,$07,$0D,$18,$40,$08,$20,$09
        .byte   $01,$03,$8D,$80,$60,$66,$69,$6B
        .byte   $2B,$8D,$80,$02,$60,$20,$90,$06
        .byte   $BE,$8F,$80,$60,$6D,$6B,$8E,$80
        .byte   $02,$80,$6D,$6B,$2B,$02,$4D,$69
        .byte   $66,$60,$64,$66,$60,$69,$60,$66
        .byte   $60,$69,$60,$6B,$60,$2B,$02,$4C
        .byte   $6B,$69,$66,$89,$80,$37,$02,$58
        .byte   $77,$75,$72,$75,$08,$00,$75,$02
        .byte   $97,$08,$20,$60,$66,$68,$69,$6B
        .byte   $6D,$70,$72,$60,$6D,$60,$70,$60
        .byte   $72,$60,$36,$02,$01,$57,$01,$57
        .byte   $40,$60,$75,$60,$72,$70,$60,$31
        .byte   $02,$52,$60,$71,$8D,$80,$71,$72
        .byte   $60,$09,$02,$2C,$02,$4D,$69,$66
        .byte   $61,$03,$72,$74,$75,$01,$57,$01
        .byte   $77,$06,$64,$01,$59,$01,$79,$01
        .byte   $5C,$01,$5C,$01,$5E,$01,$7E,$01
        .byte   $03,$49,$01,$69,$01,$4B,$01,$4B
        .byte   $06,$78,$00,$4B,$01,$4D,$01,$6D
        .byte   $8B,$89,$86,$89,$88,$81,$85,$88
        .byte   $8B,$8D,$91,$04,$08,$18,$40,$08
        .byte   $16,$09,$01,$05,$02,$16,$6D,$6D
        .byte   $6B,$6D,$60,$6B,$60,$6D,$60,$6B
        .byte   $60,$6D,$6D,$60,$6B,$60,$0E,$01
        .byte   $8D,$57,$69,$66,$68,$69,$6B,$68
        .byte   $69,$6B,$6D,$69,$6B,$6D,$70,$6E
        .byte   $6D,$6B,$04,$08,$74,$72,$6D,$0E
        .byte   $03,$8D,$86,$74,$72,$71,$6D,$04
        .byte   $00,$04,$00,$18,$40,$08,$1C,$09
        .byte   $02,$05,$02,$2E,$92,$97,$99,$92
        .byte   $9C,$92,$9B,$92,$97,$9C,$92,$9B
        .byte   $92,$99,$12,$00,$8D,$B8,$92,$97
        .byte   $0E,$01,$8D,$95,$9C,$01,$03,$88
        .byte   $01,$A8,$86,$84,$80,$81,$84,$01
        .byte   $88,$01,$A8,$86,$84,$80,$83,$80
        .byte   $01,$84,$01,$84,$83,$81,$80,$81
        .byte   $80,$03,$97,$80,$97,$99,$80,$01
        .byte   $D9,$01,$99,$04,$00,$99,$9C,$9E
        .byte   $99,$03,$8B,$81,$89,$81,$88,$81
        .byte   $86,$81,$84,$12,$08,$8E,$10,$86
        .byte   $80,$01,$88,$01,$88,$84,$81,$03
        .byte   $97,$99,$9C,$80,$03,$88,$84,$81
        .byte   $03,$97,$99,$80,$99,$80,$01,$99
        .byte   $0E,$01,$8D,$DF,$01,$86,$01,$A6
        .byte   $88,$88,$86,$88,$80,$88,$86,$88
        .byte   $8D,$8D,$8B,$8D,$80,$8B,$8D,$80
        .byte   $16,$8D,$93,$17,$06,$C8,$07,$0A
        .byte   $18,$40,$08,$20,$09,$01,$03,$89
        .byte   $80,$60,$66,$6D,$66,$29,$02,$01
        .byte   $48,$01,$68,$80,$00,$89,$86,$84
        .byte   $00,$A3,$63,$64,$66,$A9,$60,$A8
        .byte   $03,$92,$99,$95,$97,$92,$99,$95
        .byte   $97,$02,$CE,$08,$00,$03,$6E,$02
        .byte   $8E,$07,$08,$08,$13,$C6,$C9,$CD
        .byte   $A4,$A5,$C6,$C9,$ED,$04,$00,$18
        .byte   $80,$08,$16,$09,$01,$04,$00,$7E
        .byte   $7E,$7E,$7E,$60,$7E,$60,$7E,$60
        .byte   $7E,$60,$7E,$7E,$60,$7E,$60,$0E
        .byte   $01,$8E,$71,$7E,$7A,$7C,$7E,$03
        .byte   $68,$64,$66,$68,$69,$66,$68,$69
        .byte   $6D,$6B,$69,$68,$07,$0C,$08,$11
        .byte   $A1,$A5,$A8,$AD,$04,$00,$02,$60
        .byte   $04,$00,$18,$40,$08,$1C,$07,$0A
        .byte   $09,$02,$05,$02,$2E,$92,$97,$99
        .byte   $92,$9C,$92,$9B,$92,$97,$9C,$92
        .byte   $9B,$92,$99,$12,$00,$8E,$C9,$92
        .byte   $97,$0E,$01,$8E,$A4,$9C,$40,$18
        .byte   $80,$07,$0E,$08,$09,$09,$01,$04
        .byte   $00,$9C,$95,$99,$95,$0E,$03,$8E
        .byte   $D3,$9E,$99,$9C,$99,$9E,$99,$9C
        .byte   $99,$9E,$99,$03,$88,$81,$86,$81
        .byte   $84,$81,$07,$0A,$08,$13,$01,$CD
        .byte   $08,$05,$01,$CD,$08,$13,$01,$CB
        .byte   $08,$05,$01,$CB,$08,$13,$01,$D0
        .byte   $08,$05,$01,$D0,$08,$13,$CF,$CB
        .byte   $01,$C8,$08,$05,$01,$C8,$08,$13
        .byte   $01,$C6,$08,$05,$01,$C6,$08,$13
        .byte   $01,$C1,$08,$05,$C1,$01,$E1,$16
        .byte   $8E,$A0,$17,$06,$FF,$08,$02,$09
        .byte   $02,$D2,$02,$B0,$70,$72,$01,$AB
        .byte   $01,$6B,$6D,$60,$CE,$71,$D2,$D0
        .byte   $02,$CE,$6E,$02,$90,$D2,$02,$B0
        .byte   $70,$72,$02,$AB,$60,$6C,$CD,$02
        .byte   $B2,$72,$71,$02,$B0,$70,$6F,$CE
        .byte   $D9,$09,$03,$06,$C8,$62,$62,$62
        .byte   $62,$60,$62,$60,$62,$60,$62,$60
        .byte   $62,$62,$60,$62,$60,$64,$64,$64
        .byte   $64,$60,$64,$60,$64,$60,$64,$60
        .byte   $64,$64,$60,$64,$60,$A6,$A8,$A9
        .byte   $AB,$06,$E6,$08,$0E,$94,$60,$94
        .byte   $60,$94,$60,$94,$60,$76,$76,$96
        .byte   $04,$00,$04,$00,$06,$C8,$08,$02
        .byte   $66,$60,$66,$66,$06,$F0,$08,$0E
        .byte   $92,$06,$C8,$08,$02,$66,$66,$0E
        .byte   $07,$8F,$8E,$04,$00,$69,$60,$69
        .byte   $69,$06,$F0,$08,$0E,$92,$08,$02
        .byte   $06,$C8,$69,$69,$0E,$01,$8F,$A7
        .byte   $04,$00,$6B,$60,$6B,$6B,$08,$0E
        .byte   $06,$F0,$92,$08,$02,$06,$C8,$6B
        .byte   $6B,$0E,$01,$8F,$BC,$04,$00,$61
        .byte   $60,$61,$61,$08,$0E,$06,$F0,$92
        .byte   $08,$02,$06,$C8,$61,$61,$0E,$02
        .byte   $8F,$D1,$08,$0E,$06,$E6,$76,$76
        .byte   $91,$76,$76,$91,$04,$00,$08,$02
        .byte   $06,$C8,$69,$60,$69,$69,$08,$0E
        .byte   $06,$F0,$92,$08,$02,$06,$C8,$69
        .byte   $69,$0E,$01,$8F,$F0,$04,$00,$6B
        .byte   $60,$6B,$6B,$08,$0E,$06,$F0,$92
        .byte   $08,$02,$06,$C8,$6B,$6B,$0E,$01
        .byte   $90,$09,$04,$00,$6D,$60,$6D,$6D
        .byte   $08,$0E,$06,$F0,$92,$08,$02,$06
        .byte   $C8,$6D,$6D,$0E,$02,$90,$1E,$6B
        .byte   $60,$6B,$6B,$08,$0E,$06,$F0,$92
        .byte   $08,$02,$06,$C8,$6B,$6B,$04,$00
        .byte   $69,$60,$69,$69,$08,$0E,$06,$F0
        .byte   $92,$08,$02,$06,$F0,$69,$69,$0E
        .byte   $01,$90,$42,$04,$00,$6B,$60,$6B
        .byte   $6B,$08,$0E,$06,$F0,$92,$08,$02
        .byte   $06,$C8,$6B,$6B,$0E,$01,$90,$57
        .byte   $04,$00,$61,$60,$61,$61,$08,$0E
        .byte   $06,$F0,$92,$08,$02,$06,$C8,$61
        .byte   $61,$0E,$02,$90,$6C,$08,$0E,$06
        .byte   $F0,$60,$96,$60,$96,$76,$76,$16
        .byte   $8F,$8C,$17,$06,$C8,$08,$0C,$07
        .byte   $0C,$04,$00,$E0,$0E,$07,$90,$95
        .byte   $04,$00,$68,$68,$68,$68,$60,$68
        .byte   $60,$68,$60,$68,$60,$68,$68,$60
        .byte   $68,$60,$0E,$01,$90,$9C,$AB,$AB
        .byte   $AB,$AB,$04,$00,$6C,$68,$65,$0E
        .byte   $03,$90,$B6,$6C,$68,$65,$68,$04
        .byte   $00,$04,$00,$63,$60,$6F,$6F,$68
        .byte   $60,$6F,$6F,$0E,$1E,$90,$C5,$63
        .byte   $68,$6C,$6A,$68,$6C,$6A,$68,$16
        .byte   $90,$C3,$17,$00,$90,$E8,$92,$31
        .byte   $93,$91,$94,$B1,$0A,$03,$05,$01
        .byte   $EB,$06,$96,$07,$0B,$18,$C0,$08
        .byte   $1C,$04,$00,$60,$74,$97,$99,$97
        .byte   $99,$77,$99,$80,$74,$60,$74,$97
        .byte   $99,$97,$99,$77,$99,$7A,$79,$77
        .byte   $0E,$04,$90,$F5,$07,$09,$18,$80
        .byte   $09,$02,$60,$74,$76,$77,$79,$77
        .byte   $76,$77,$7B,$79,$7B,$7E,$03,$68
        .byte   $6B,$6D,$08,$11,$06,$FF,$01,$6F
        .byte   $01,$EF,$04,$00,$18,$40,$04,$00
        .byte   $07,$0C,$09,$01,$08,$1C,$06,$64
        .byte   $60,$02,$03,$88,$86,$83,$02,$86
        .byte   $88,$02,$80,$60,$02,$88,$86,$88
        .byte   $8B,$8A,$88,$86,$01,$C3,$12,$48
        .byte   $91,$89,$08,$05,$C3,$02,$01,$A3
        .byte   $08,$1C,$03,$8F,$90,$92,$94,$96
        .byte   $02,$B9,$9B,$01,$D7,$08,$05,$02
        .byte   $01,$B7,$08,$1C,$97,$99,$97,$96
        .byte   $97,$01,$DB,$08,$05,$01,$DB,$08
        .byte   $1C,$01,$DE,$08,$05,$01,$DE,$18
        .byte   $80,$0E,$01,$91,$32,$08,$1C,$01
        .byte   $83,$63,$64,$86,$81,$06,$96,$C3
        .byte   $03,$B7,$B9,$01,$DB,$01,$9B,$B9
        .byte   $97,$D9,$B7,$B9,$DB,$DF,$03,$CA
        .byte   $01,$AD,$01,$8D,$08,$1C,$09,$01
        .byte   $18,$C0,$07,$0A,$01,$84,$04,$00
        .byte   $9C,$9B,$9C,$03,$88,$02,$8B,$02
        .byte   $8F,$84,$80,$83,$84,$88,$8B,$AF
        .byte   $12,$08,$91,$D9,$8D,$80,$8A,$02
        .byte   $01,$C6,$02,$C6,$01,$66,$60,$01
        .byte   $84,$0E,$01,$91,$B2,$92,$80,$8D
        .byte   $02,$01,$CF,$06,$F0,$01,$EF,$18
        .byte   $40,$07,$08,$60,$02,$8F,$8D,$8B
        .byte   $8D,$8B,$02,$8A,$68,$09,$02,$08
        .byte   $16,$60,$02,$88,$86,$83,$86,$83
        .byte   $02,$81,$01,$63,$01,$E3,$60,$02
        .byte   $88,$86,$88,$8B,$8A,$88,$66,$01
        .byte   $68,$08,$00,$01,$E8,$60,$02,$8B
        .byte   $8A,$88,$8A,$88,$A6,$E3,$09,$00
        .byte   $06,$96,$63,$02,$80,$63,$02,$80
        .byte   $63,$02,$80,$09,$01,$63,$60,$66
        .byte   $60,$16,$91,$2E,$17,$06,$FF,$07
        .byte   $0B,$08,$00,$09,$02,$E0,$E0,$E0
        .byte   $E0,$04,$00,$09,$03,$18,$80,$08
        .byte   $1C,$74,$60,$6F,$60,$72,$60,$73
        .byte   $74,$60,$74,$6F,$60,$72,$60,$73
        .byte   $60,$0E,$05,$92,$3D,$0C,$00,$07
        .byte   $09,$60,$74,$72,$6F,$72,$6F,$6D
        .byte   $6B,$6D,$6B,$68,$66,$68,$63,$66
        .byte   $01,$6F,$01,$CF,$00,$6F,$72,$74
        .byte   $77,$7B,$7E,$03,$68,$6B,$6F,$72
        .byte   $74,$77,$04,$08,$04,$08,$04,$08
        .byte   $07,$0A,$09,$01,$18,$80,$08,$04
        .byte   $04,$08,$74,$60,$6B,$60,$6F,$60
        .byte   $6B,$74,$60,$74,$6B,$60,$6F,$60
        .byte   $6B,$60,$0E,$01,$92,$8C,$04,$08
        .byte   $6F,$60,$68,$60,$6B,$60,$68,$6F
        .byte   $60,$6F,$68,$60,$6B,$60,$68,$60
        .byte   $0E,$01,$92,$A2,$6F,$60,$6B,$60
        .byte   $6D,$60,$6B,$6F,$60,$6F,$6B,$60
        .byte   $6D,$60,$6B,$60,$6F,$60,$68,$60
        .byte   $6B,$60,$68,$6F,$60,$6F,$68,$60
        .byte   $6B,$60,$68,$60,$73,$60,$6F,$60
        .byte   $70,$60,$6F,$73,$60,$73,$6F,$60
        .byte   $70,$60,$6F,$60,$14,$08,$93,$05
        .byte   $07,$0B,$08,$00,$AF,$AF,$AF,$00
        .byte   $52,$51,$4F,$4D,$4C,$4A,$48,$46
        .byte   $45,$43,$41,$03,$58,$10,$01,$92
        .byte   $82,$07,$0A,$08,$16,$A3,$A3,$A3
        .byte   $86,$18,$C0,$06,$FF,$08,$1C,$09
        .byte   $01,$0C,$02,$01,$84,$01,$84,$83
        .byte   $84,$88,$02,$8B,$02,$8F,$84,$80
        .byte   $83,$84,$88,$8B,$AF,$8D,$80,$8A
        .byte   $02,$01,$C6,$02,$C6,$01,$66,$60
        .byte   $01,$84,$01,$84,$83,$84,$88,$02
        .byte   $8B,$02,$8F,$84,$80,$83,$84,$88
        .byte   $8B,$AF,$92,$80,$8D,$02,$01,$CF
        .byte   $06,$C8,$01,$EF,$06,$F0,$08,$16
        .byte   $09,$01,$04,$00,$07,$08,$60,$02
        .byte   $03,$8F,$8D,$8B,$8D,$8B,$02,$8A
        .byte   $12,$08,$93,$74,$0C,$00,$01,$68
        .byte   $01,$C8,$A8,$A7,$0E,$02,$93,$56
        .byte   $68,$60,$02,$88,$86,$83,$86,$83
        .byte   $A1,$08,$00,$E3,$60,$02,$8F,$60
        .byte   $02,$8F,$60,$02,$8F,$6F,$60,$72
        .byte   $60,$16,$92,$7E,$17,$04,$00,$06
        .byte   $96,$08,$02,$09,$02,$04,$00,$60
        .byte   $74,$97,$99,$97,$99,$77,$99,$80
        .byte   $74,$60,$74,$97,$99,$97,$99,$77
        .byte   $99,$7A,$79,$77,$0E,$01,$93,$99
        .byte   $04,$48,$08,$02,$09,$02,$06,$FF
        .byte   $CA,$08,$1B,$AA,$01,$8A,$08,$02
        .byte   $88,$01,$CB,$08,$1B,$01,$CB,$0E
        .byte   $02,$93,$B4,$06,$96,$09,$02,$08
        .byte   $02,$60,$68,$66,$63,$66,$63,$61
        .byte   $03,$77,$79,$77,$74,$72,$74,$72
        .byte   $70,$01,$74,$01,$D4,$94,$74,$03
        .byte   $68,$03,$74,$6F,$72,$73,$04,$00
        .byte   $04,$00,$04,$00,$94,$8F,$92,$73
        .byte   $94,$74,$8F,$92,$93,$0E,$02,$93
        .byte   $F6,$8F,$8F,$94,$6F,$8F,$75,$90
        .byte   $92,$94,$90,$8B,$8D,$6F,$90,$70
        .byte   $8B,$8D,$8F,$95,$90,$91,$73,$95
        .byte   $75,$90,$91,$93,$8F,$92,$94,$97
        .byte   $99,$9B,$9E,$9F,$13,$00,$94,$36
        .byte   $AF,$AF,$AF,$02,$92,$74,$0F,$01
        .byte   $93,$F4,$AF,$AF,$AF,$02,$92,$01
        .byte   $70,$02,$01,$90,$70,$90,$8D,$8F
        .byte   $9B,$90,$01,$90,$01,$70,$70,$90
        .byte   $90,$8D,$8F,$9B,$90,$8F,$02,$80
        .byte   $6F,$8F,$8D,$8F,$9B,$8F,$01,$95
        .byte   $01,$75,$75,$95,$94,$94,$92,$92
        .byte   $91,$90,$02,$80,$70,$90,$8D,$8F
        .byte   $9B,$90,$01,$90,$01,$70,$70,$90
        .byte   $90,$8D,$8F,$9B,$90,$8F,$80,$8F
        .byte   $92,$8F,$93,$8F,$96,$8F,$BB,$BB
        .byte   $BB,$BB,$04,$00,$60,$74,$94,$92
        .byte   $73,$02,$94,$02,$AF,$90,$9C,$90
        .byte   $9C,$92,$9E,$8F,$9B,$0E,$01,$94
        .byte   $86,$B4,$B4,$B3,$B3,$B2,$B2,$B0
        .byte   $B0,$EF,$60,$BB,$BB,$02,$9B,$9B
        .byte   $9B,$16,$93,$F2,$17,$06,$C8,$07
        .byte   $0A,$08,$0C,$04,$00,$A3,$0E,$06
        .byte   $94,$B7,$63,$68,$68,$68,$A3,$A3
        .byte   $A3,$A3,$63,$08,$03,$02,$89,$08
        .byte   $0C,$87,$87,$85,$65,$65,$63,$68
        .byte   $68,$68,$04,$00,$04,$00,$A3,$0E
        .byte   $06,$94,$D8,$63,$68,$68,$68,$0F
        .byte   $02,$94,$D6,$63,$04,$00,$07,$0C
        .byte   $66,$0E,$0A,$94,$E8,$68,$68,$68
        .byte   $6A,$A6,$63,$60,$63,$63,$6A,$60
        .byte   $60,$60,$63,$6C,$6C,$6C,$04,$00
        .byte   $04,$00,$63,$60,$6F,$6F,$63,$6C
        .byte   $60,$6F,$63,$6F,$6D,$60,$63,$6F
        .byte   $6F,$6F,$0E,$06,$95,$04,$63,$60
        .byte   $6C,$6C,$63,$60,$6C,$6C,$66,$66
        .byte   $60,$66,$63,$6C,$6C,$60,$04,$00
        .byte   $66,$60,$6C,$6C,$63,$6C,$60,$6C
        .byte   $66,$6C,$6D,$60,$63,$6C,$6C,$60
        .byte   $0E,$05,$95,$2A,$66,$60,$6C,$6C
        .byte   $63,$60,$60,$60,$66,$6C,$6D,$60
        .byte   $63,$6C,$6C,$6C,$66,$60,$6C,$6C
        .byte   $63,$6C,$60,$6C,$66,$6C,$6D,$60
        .byte   $63,$60,$60,$63,$04,$00,$66,$60
        .byte   $63,$6C,$63,$6C,$6D,$66,$6C,$60
        .byte   $65,$6C,$63,$60,$6C,$63,$0E,$0D
        .byte   $95,$60,$66,$60,$6C,$6C,$63,$6C
        .byte   $60,$66,$63,$6C,$6D,$60,$63,$6D
        .byte   $60,$63,$63,$6D,$60,$60,$63,$6D
        .byte   $60,$60,$63,$6D,$60,$60,$63,$60
        .byte   $6A,$60,$16,$95,$02,$17,$00,$95
        .byte   $A3,$96,$EB,$98,$47,$99,$93,$0A
        .byte   $04,$18,$40,$05,$01,$EB,$08,$1D
        .byte   $07,$0A,$06,$E6,$03,$96,$80,$96
        .byte   $80,$09,$01,$8D,$80,$8D,$8F,$04
        .byte   $00,$18,$C0,$80,$79,$78,$79,$7B
        .byte   $80,$79,$78,$79,$7B,$A0,$0E,$01
        .byte   $95,$BB,$02,$9F,$02,$03,$88,$01
        .byte   $8A,$CA,$01,$8A,$6F,$80,$6F,$80
        .byte   $6F,$80,$6F,$80,$6F,$60,$04,$00
        .byte   $07,$08,$06,$FF,$09,$02,$08,$16
        .byte   $B3,$02,$9A,$01,$BB,$08,$17,$02
        .byte   $01,$9B,$08,$16,$73,$60,$97,$76
        .byte   $60,$76,$74,$60,$01,$B4,$08,$17
        .byte   $94,$02,$01,$94,$08,$16,$BD,$02
        .byte   $9B,$01,$BA,$08,$17,$02,$01,$9A
        .byte   $08,$16,$76,$74,$73,$60,$73,$60
        .byte   $73,$74,$60,$01,$B6,$08,$17,$96
        .byte   $02,$01,$96,$06,$E6,$08,$16,$01
        .byte   $97,$08,$17,$01,$B7,$08,$16,$77
        .byte   $79,$02,$9B,$02,$99,$97,$02,$96
        .byte   $02,$93,$01,$AF,$08,$17,$AF,$01
        .byte   $8F,$08,$16,$02,$94,$02,$96,$B8
        .byte   $9B,$9A,$98,$02,$9A,$02,$9B,$01
        .byte   $BD,$08,$17,$BD,$01,$9D,$07,$0A
        .byte   $06,$96,$08,$0B,$80,$9B,$9B,$9B
        .byte   $06,$E6,$02,$9B,$02,$9A,$96,$80
        .byte   $06,$96,$99,$99,$99,$06,$E6,$02
        .byte   $99,$02,$98,$94,$80,$7D,$60,$9D
        .byte   $9B,$02,$9F,$02,$98,$01,$9B,$01
        .byte   $9B,$03,$6A,$60,$68,$60,$67,$60
        .byte   $02,$85,$02,$83,$83,$80,$68,$60
        .byte   $88,$87,$02,$85,$02,$83,$83,$80
        .byte   $67,$60,$87,$88,$02,$88,$02,$03
        .byte   $98,$98,$80,$03,$6A,$60,$8A,$88
        .byte   $02,$87,$02,$85,$83,$85,$83,$85
        .byte   $02,$A7,$85,$83,$04,$08,$06,$C8
        .byte   $6A,$60,$66,$60,$63,$66,$60,$68
        .byte   $60,$6A,$60,$02,$88,$12,$08,$96
        .byte   $D3,$66,$68,$0E,$01,$96,$B8,$09
        .byte   $01,$6A,$6D,$6F,$60,$6D,$60,$6A
        .byte   $6D,$60,$6F,$60,$6F,$6F,$60,$6F
        .byte   $60,$6F,$60,$16,$95,$E2,$17,$18
        .byte   $40,$08,$1D,$06,$E6,$07,$0A,$03
        .byte   $94,$80,$94,$80,$94,$80,$94,$96
        .byte   $04,$00,$09,$01,$08,$1D,$80,$03
        .byte   $68,$67,$68,$6A,$80,$68,$67,$68
        .byte   $6A,$A0,$0E,$01,$96,$FC,$08,$1C
        .byte   $80,$6F,$60,$74,$60,$6F,$60,$02
        .byte   $8D,$6F,$80,$6F,$60,$72,$60,$6F
        .byte   $60,$6D,$6F,$60,$02,$01,$8F,$08
        .byte   $17,$02,$01,$AF,$04,$00,$04,$00
        .byte   $07,$0C,$09,$00,$06,$E6,$18,$00
        .byte   $08,$0F,$80,$03,$6F,$60,$6F,$6A
        .byte   $80,$6F,$60,$6F,$6A,$A0,$12,$08
        .byte   $97,$68,$80,$6F,$60,$6F,$68,$80
        .byte   $6F,$60,$6F,$68,$A0,$80,$6F,$60
        .byte   $6F,$71,$80,$6F,$60,$6F,$71,$A0
        .byte   $0E,$01,$97,$32,$09,$00,$07,$08
        .byte   $18,$80,$08,$13,$CF,$D1,$D3,$D4
        .byte   $18,$40,$07,$0C,$08,$1C,$80,$73
        .byte   $74,$76,$8F,$60,$73,$74,$76,$8F
        .byte   $02,$80,$80,$71,$73,$74,$76,$60
        .byte   $76,$60,$76,$60,$74,$93,$91,$07
        .byte   $0B,$09,$01,$08,$09,$18,$C0,$04
        .byte   $00,$04,$00,$7B,$7B,$03,$6A,$6A
        .byte   $6F,$6F,$76,$76,$0E,$01,$97,$9D
        .byte   $13,$08,$97,$E6,$04,$00,$74,$74
        .byte   $7B,$7B,$03,$68,$68,$6F,$6F,$0E
        .byte   $01,$97,$B0,$04,$00,$78,$78,$7D
        .byte   $7D,$03,$6C,$6C,$71,$71,$0E,$01
        .byte   $97,$BF,$03,$74,$74,$7B,$7B,$03
        .byte   $68,$68,$6F,$6F,$03,$76,$76,$7B
        .byte   $7B,$03,$6A,$6A,$6F,$6F,$0F,$01
        .byte   $97,$9B,$04,$00,$74,$74,$7B,$7B
        .byte   $03,$68,$68,$6F,$6F,$0E,$01,$97
        .byte   $E6,$04,$00,$76,$76,$7B,$7B,$03
        .byte   $6A,$6A,$6F,$6F,$0E,$01,$97,$F5
        .byte   $04,$00,$78,$78,$7D,$7D,$03,$6C
        .byte   $6C,$71,$71,$0E,$01,$98,$04,$09
        .byte   $00,$06,$64,$18,$C0,$08,$0B,$04
        .byte   $08,$6F,$6F,$6D,$60,$6F,$60,$6D
        .byte   $6F,$60,$72,$60,$71,$60,$6F,$60
        .byte   $6D,$0E,$01,$98,$1B,$6F,$6F,$6D
        .byte   $60,$6F,$6D,$60,$6F,$60,$09,$01
        .byte   $66,$66,$60,$66,$60,$66,$60,$16
        .byte   $97,$30,$17,$08,$02,$07,$0C,$06
        .byte   $C8,$09,$02,$8F,$80,$8F,$80,$92
        .byte   $80,$92,$94,$80,$09,$03,$83,$87
        .byte   $83,$88,$87,$85,$83,$80,$83,$8A
        .byte   $83,$88,$81,$82,$83,$80,$83,$87
        .byte   $83,$88,$87,$85,$83,$80,$83,$8A
        .byte   $83,$87,$81,$82,$83,$04,$00,$09
        .byte   $03,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$67,$60,$67,$67,$67,$60,$67
        .byte   $67,$67,$60,$67,$67,$67,$60,$67
        .byte   $67,$65,$60,$65,$65,$65,$60,$65
        .byte   $65,$65,$60,$65,$65,$65,$60,$65
        .byte   $65,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$83,$60,$83,$60,$01,$63,$01
        .byte   $63,$83,$82,$09,$02,$8A,$8E,$94
        .byte   $60,$94,$60,$01,$74,$01,$74,$94
        .byte   $93,$8F,$93,$8C,$60,$8C,$60,$01
        .byte   $6C,$01,$6C,$8C,$8F,$91,$8C,$94
        .byte   $60,$94,$60,$01,$74,$01,$74,$96
        .byte   $94,$93,$91,$8F,$60,$8F,$60,$01
        .byte   $6F,$01,$6F,$8F,$8E,$8A,$8E,$94
        .byte   $60,$94,$60,$01,$74,$01,$74,$94
        .byte   $93,$8F,$93,$8A,$60,$8A,$60,$01
        .byte   $6A,$01,$6A,$8A,$91,$94,$96,$8C
        .byte   $60,$8C,$60,$01,$6C,$01,$6C,$8F
        .byte   $91,$8F,$8C,$72,$60,$72,$60,$72
        .byte   $72,$60,$72,$60,$72,$60,$72,$72
        .byte   $60,$72,$60,$74,$60,$74,$60,$74
        .byte   $74,$60,$74,$60,$74,$60,$74,$74
        .byte   $60,$74,$60,$6F,$60,$6F,$60,$6F
        .byte   $6F,$60,$6F,$60,$6F,$6F,$60,$6F
        .byte   $60,$6F,$60,$16,$98,$79,$17,$08
        .byte   $0C,$07,$0A,$06,$96,$A9,$A9,$A9
        .byte   $88,$88,$04,$00,$06,$C8,$63,$60
        .byte   $6C,$6C,$68,$6C,$60,$6C,$63,$60
        .byte   $6C,$6C,$68,$6C,$6C,$60,$0E,$03
        .byte   $99,$9E,$04,$00,$04,$00,$06,$C8
        .byte   $63,$60,$6C,$6C,$68,$6C,$60,$6C
        .byte   $63,$60,$6C,$6C,$68,$6C,$6C,$60
        .byte   $0E,$07,$99,$B8,$04,$00,$06,$F0
        .byte   $63,$6F,$6D,$6F,$68,$6F,$6D,$6F
        .byte   $0E,$0F,$99,$D0,$04,$00,$63,$6F
        .byte   $63,$6F,$68,$63,$60,$63,$12,$00
        .byte   $99,$FA,$63,$68,$6F,$6F,$68,$6F
        .byte   $6D,$6F,$0E,$02,$99,$E0,$60,$68
        .byte   $68,$60,$68,$60,$68,$60,$16,$99
        .byte   $B6,$17,$00,$9A,$0F,$9A,$EA,$9B
        .byte   $DA,$9C,$5B,$0A,$05,$05,$03,$00
        .byte   $07,$0A,$04,$08,$04,$08,$06,$AA
        .byte   $08,$0B,$18,$80,$09,$01,$A8,$8B
        .byte   $AF,$AF,$88,$A7,$8A,$B0,$B0,$87
        .byte   $0E,$01,$9A,$18,$04,$00,$07,$0C
        .byte   $18,$40,$08,$1A,$0D,$00,$80,$03
        .byte   $88,$8B,$8D,$80,$8D,$80,$8D,$80
        .byte   $6F,$6D,$8B,$80,$AD,$8B,$6D,$60
        .byte   $12,$08,$9A,$63,$80,$02,$AB,$4C
        .byte   $AD,$8D,$02,$01,$6B,$0D,$0C,$02
        .byte   $CB,$01,$A8,$0E,$03,$9A,$30,$80
        .byte   $02,$AB,$4F,$B0,$90,$02,$01,$6F
        .byte   $01,$EF,$04,$08,$07,$06,$18,$C0
        .byte   $08,$11,$06,$FF,$E8,$EB,$01,$ED
        .byte   $08,$01,$02,$01,$CD,$08,$11,$8B
        .byte   $8D,$EB,$E8,$01,$E3,$08,$01,$01
        .byte   $E3,$0E,$01,$9A,$6E,$18,$80,$08
        .byte   $11,$ED,$01,$F0,$F0,$08,$05,$02
        .byte   $01,$D0,$90,$92,$08,$11,$01,$F4
        .byte   $08,$05,$01,$D4,$08,$11,$D7,$F6
        .byte   $F2,$01,$F4,$08,$01,$F4,$01,$F4
        .byte   $02,$C0,$07,$0A,$08,$0B,$06,$DC
        .byte   $18,$C0,$92,$90,$8F,$80,$88,$8D
        .byte   $80,$8D,$80,$88,$8B,$6D,$6B,$88
        .byte   $8D,$80,$8D,$80,$88,$09,$02,$18
        .byte   $40,$88,$80,$8B,$8D,$80,$8D,$80
        .byte   $88,$8B,$6D,$6B,$88,$8D,$80,$8D
        .byte   $80,$88,$16,$9A,$16,$17,$04,$48
        .byte   $06,$DC,$07,$09,$18,$80,$08,$07
        .byte   $09,$01,$04,$48,$EF,$0D,$14,$01
        .byte   $EE,$0E,$01,$9A,$F6,$04,$00,$07
        .byte   $0C,$08,$1A,$0D,$00,$80,$9B,$03
        .byte   $88,$88,$80,$88,$80,$88,$80,$68
        .byte   $67,$86,$80,$A8,$86,$88,$12,$08
        .byte   $9B,$2F,$80,$02,$A6,$A8,$88,$01
        .byte   $86,$0D,$0C,$02,$C6,$01,$A3,$0E
        .byte   $03,$9B,$01,$0D,$00,$80,$02,$A6
        .byte   $A8,$88,$01,$87,$01,$E7,$04,$40
        .byte   $07,$08,$18,$40,$08,$07,$06,$FF
        .byte   $FB,$08,$05,$01,$FB,$08,$11,$01
        .byte   $03,$E9,$08,$01,$01,$E9,$08,$00
        .byte   $18,$C0,$06,$DC,$80,$83,$84,$87
        .byte   $88,$8A,$80,$01,$8B,$01,$AB,$00
        .byte   $8A,$8B,$8A,$00,$A8,$87,$01,$88
        .byte   $E8,$08,$01,$01,$E8,$0E,$01,$9B
        .byte   $3A,$08,$11,$06,$FF,$E8,$ED,$01
        .byte   $EE,$08,$05,$01,$EE,$08,$12,$06
        .byte   $DC,$09,$00,$07,$0C,$80,$8F,$90
        .byte   $92,$B4,$92,$01,$90,$02,$01,$D0
        .byte   $01,$8F,$90,$01,$EF,$CB,$C8,$A0
        .byte   $AD,$AF,$B0,$92,$94,$80,$95,$80
        .byte   $97,$80,$09,$01,$01,$8D,$01,$8D
        .byte   $8B,$AD,$8B,$AD,$8B,$08,$17,$02
        .byte   $CD,$08,$12,$8B,$89,$07,$08,$06
        .byte   $FF,$01,$E8,$01,$E8,$08,$00,$03
        .byte   $94,$80,$97,$99,$80,$99,$80,$94
        .byte   $97,$79,$77,$74,$60,$99,$80,$99
        .byte   $80,$94,$16,$9A,$EA,$17,$06,$64
        .byte   $07,$0E,$08,$02,$09,$02,$04,$00
        .byte   $04,$00,$A8,$8B,$AF,$AF,$88,$A7
        .byte   $8A,$B0,$B0,$87,$0E,$08,$9B,$E4
        .byte   $80,$02,$A7,$A9,$89,$01,$8A,$06
        .byte   $FF,$01,$EA,$04,$00,$06,$64,$A8
        .byte   $8B,$AF,$AF,$88,$A7,$8A,$B0,$B0
        .byte   $87,$0E,$07,$9B,$FF,$AD,$90,$B4
        .byte   $B4,$8D,$AC,$8F,$B4,$B4,$8C,$04
        .byte   $00,$A9,$8D,$B2,$B2,$89,$0E,$01
        .byte   $9C,$1B,$04,$00,$AD,$90,$B4,$B4
        .byte   $8D,$0E,$01,$9C,$26,$04,$00,$AB
        .byte   $90,$B4,$B4,$8B,$0E,$01,$9C,$31
        .byte   $04,$00,$A9,$8D,$B0,$B0,$89,$0E
        .byte   $03,$9C,$3C,$04,$00,$A8,$8B,$AF
        .byte   $AF,$88,$A7,$8A,$B0,$B0,$87,$0E
        .byte   $01,$9C,$47,$16,$9B,$E2,$17,$07
        .byte   $0B,$08,$0C,$04,$00,$04,$00,$04
        .byte   $00,$06,$C8,$83,$8F,$88,$83,$8F
        .byte   $83,$06,$FF,$8C,$06,$C8,$83,$8F
        .byte   $83,$06,$FF,$8C,$06,$C8,$83,$83
        .byte   $83,$88,$8F,$0E,$08,$9C,$63,$80
        .byte   $88,$81,$88,$88,$81,$88,$88,$80
        .byte   $68,$68,$68,$60,$68,$60,$88,$88
        .byte   $88,$88,$04,$08,$8D,$03,$8F,$88
        .byte   $83,$8F,$83,$06,$FF,$8C,$06,$C8
        .byte   $83,$8F,$83,$06,$FF,$8C,$06,$C8
        .byte   $83,$83,$83,$88,$8F,$0E,$0D,$9C
        .byte   $96,$04,$00,$88,$83,$8F,$88,$80
        .byte   $8F,$88,$83,$0E,$01,$9C,$B5,$83
        .byte   $83,$88,$83,$83,$88,$8D,$83,$83
        .byte   $83,$88,$83,$88,$88,$88,$88,$16
        .byte   $9C,$5F,$17,$00,$9C,$E0,$9D,$F3
        .byte   $9F,$57,$A0,$4E,$0A,$02,$04,$08
        .byte   $05,$01,$EB,$06,$78,$07,$0A,$18
        .byte   $40,$08,$07,$09,$01,$04,$08,$04
        .byte   $08,$88,$88,$6B,$88,$8D,$8F,$06
        .byte   $FF,$02,$8D,$06,$78,$6B,$6A,$0E
        .byte   $01,$9C,$F3,$13,$08,$9D,$22,$88
        .byte   $8B,$6A,$86,$88,$8B,$6A,$8A,$86
        .byte   $84,$86,$68,$8F,$02,$01,$AA,$02
        .byte   $01,$8A,$0F,$01,$9C,$F1,$88,$8B
        .byte   $6A,$8D,$8B,$8B,$6F,$8D,$90,$8F
        .byte   $92,$77,$96,$02,$01,$B4,$02,$01
        .byte   $94,$04,$08,$07,$0B,$18,$C0,$08
        .byte   $12,$88,$86,$68,$86,$88,$02,$80
        .byte   $68,$6A,$6B,$6F,$AD,$02,$8A,$02
        .byte   $86,$80,$A0,$8D,$8D,$6B,$8A,$8D
        .byte   $02,$80,$6F,$6D,$6B,$6A,$88,$88
        .byte   $68,$86,$02,$88,$80,$A0,$0E,$01
        .byte   $9D,$35,$04,$08,$8A,$8B,$8A,$88
        .byte   $02,$8A,$01,$A6,$01,$66,$0E,$01
        .byte   $9D,$66,$18,$80,$06,$C8,$02,$86
        .byte   $02,$84,$86,$02,$88,$02,$86,$88
        .byte   $02,$8A,$02,$88,$8A,$02,$8B,$02
        .byte   $8A,$8B,$04,$08,$08,$06,$18,$C0
        .byte   $6D,$60,$6D,$60,$70,$74,$60,$02
        .byte   $92,$90,$8F,$8D,$70,$60,$6F,$60
        .byte   $6D,$6B,$60,$02,$01,$AD,$02,$01
        .byte   $8D,$12,$08,$9D,$CD,$6D,$60,$6D
        .byte   $60,$70,$72,$60,$02,$94,$92,$94
        .byte   $95,$77,$60,$75,$60,$74,$92,$02
        .byte   $01,$B0,$02,$01,$90,$0E,$01,$9D
        .byte   $8E,$09,$02,$6D,$60,$6B,$60,$69
        .byte   $68,$60,$02,$86,$88,$8B,$8D,$70
        .byte   $60,$6F,$60,$6D,$8B,$8D,$07,$0C
        .byte   $08,$00,$09,$00,$6D,$6D,$60,$6D
        .byte   $60,$6D,$6D,$16,$9C,$E2,$17,$04
        .byte   $08,$06,$78,$07,$09,$18,$C0,$08
        .byte   $07,$09,$01,$04,$08,$04,$08,$8B
        .byte   $8B,$64,$84,$86,$86,$01,$A6,$01
        .byte   $66,$0E,$01,$9E,$01,$13,$08,$9E
        .byte   $31,$0C,$FD,$88,$8B,$6A,$86,$88
        .byte   $8B,$6A,$8A,$86,$0C,$00,$03,$94
        .byte   $96,$77,$97,$02,$01,$B9,$02,$01
        .byte   $99,$0F,$01,$9D,$FF,$0C,$FD,$88
        .byte   $8B,$6A,$8D,$8B,$8B,$6F,$8D,$90
        .byte   $0C,$FE,$6F,$72,$74,$77,$09,$01
        .byte   $6D,$6F,$72,$02,$01,$B4,$02,$01
        .byte   $94,$04,$00,$04,$00,$07,$0B,$0C
        .byte   $00,$06,$C8,$0C,$00,$18,$00,$08
        .byte   $0F,$80,$74,$80,$74,$80,$74,$72
        .byte   $60,$74,$A0,$0E,$01,$9E,$4F,$80
        .byte   $72,$80,$72,$80,$72,$6F,$60,$72
        .byte   $A0,$80,$74,$80,$74,$80,$74,$72
        .byte   $60,$74,$A0,$0F,$01,$9E,$4D,$07
        .byte   $07,$18,$80,$08,$13,$09,$01,$CD
        .byte   $D2,$D6,$01,$B9,$00,$01,$59,$08
        .byte   $00,$18,$00,$07,$0C,$03,$54,$52
        .byte   $51,$4F,$4D,$4C,$4A,$48,$46,$45
        .byte   $43,$09,$02,$18,$C0,$07,$0A,$06
        .byte   $C8,$08,$06,$04,$00,$76,$79,$7C
        .byte   $03,$68,$66,$64,$0E,$03,$9E,$AF
        .byte   $03,$76,$79,$7C,$7E,$03,$6A,$6D
        .byte   $70,$74,$04,$08,$09,$01,$08,$06
        .byte   $0C,$FE,$18,$C0,$6D,$60,$6D,$60
        .byte   $70,$74,$60,$02,$92,$90,$8F,$8D
        .byte   $70,$60,$6F,$60,$6D,$6B,$60,$12
        .byte   $08,$9F,$21,$8D,$0C,$00,$18,$00
        .byte   $06,$C8,$08,$00,$02,$94,$72,$60
        .byte   $6D,$60,$08,$0B,$18,$C0,$0C,$FE
        .byte   $6D,$60,$6D,$60,$70,$72,$60,$02
        .byte   $94,$92,$94,$95,$77,$60,$75,$60
        .byte   $74,$72,$60,$90,$0C,$00,$18,$00
        .byte   $08,$00,$74,$72,$70,$72,$74,$6B
        .byte   $6D,$0E,$01,$9E,$C6,$0C,$00,$6D
        .byte   $18,$00,$08,$00,$6D,$6D,$6B,$6D
        .byte   $70,$6F,$6D,$6B,$09,$02,$06,$64
        .byte   $08,$0F,$03,$97,$97,$77,$94,$97
        .byte   $97,$74,$97,$94,$97,$97,$77,$94
        .byte   $99,$09,$01,$08,$00,$07,$0C,$03
        .byte   $77,$77,$60,$77,$60,$77,$77,$16
        .byte   $9D,$F3,$17,$04,$00,$06,$C8,$07
        .byte   $0C,$08,$02,$09,$03,$04,$00,$04
        .byte   $00,$68,$02,$80,$66,$80,$65,$60
        .byte   $02,$85,$64,$60,$63,$60,$0E,$01
        .byte   $9F,$63,$13,$00,$9F,$98,$68,$02
        .byte   $80,$66,$80,$65,$60,$02,$85,$64
        .byte   $60,$64,$60,$61,$60,$61,$60,$64
        .byte   $64,$60,$02,$01,$A6,$02,$01,$86
        .byte   $0F,$01,$9F,$61,$68,$02,$80,$66
        .byte   $80,$65,$60,$02,$85,$64,$60,$64
        .byte   $60,$63,$60,$63,$60,$63,$63,$60
        .byte   $02,$01,$A8,$02,$01,$88,$04,$00
        .byte   $68,$68,$A0,$68,$68,$02,$A0,$68
        .byte   $67,$66,$66,$A0,$66,$66,$02,$A0
        .byte   $66,$65,$63,$63,$A0,$63,$63,$60
        .byte   $6D,$63,$60,$6D,$63,$6D,$6F,$64
        .byte   $64,$A0,$64,$64,$60,$6D,$64,$60
        .byte   $6D,$64,$6D,$70,$0E,$01,$9F,$B2
        .byte   $04,$08,$08,$0E,$09,$01,$6A,$60
        .byte   $6A,$60,$71,$6F,$6D,$6A,$60,$6A
        .byte   $60,$6A,$71,$6F,$6D,$6C,$0E,$01
        .byte   $9F,$E4,$04,$00
