; da65 V2.18 - Ubuntu 2.19-1
; Created:    2026-02-21 05:24:06
; Input file: /home/kn/megamanforever/megaman3-disassembly-ca65/tools/../build/bank1E_1F.bin
; Page:       1


        .setcpu "6502"

L0000           := $0000
L000C           := $000C
L0093           := $0093
L009C           := $009C
L8000           := $8000
L8003           := $8003
L8006           := $8006
L8009           := $8009
L800C           := $800C
L800F           := $800F
L8012           := $8012
L9000           := $9000
L9003           := $9003
L9006           := $9006
L9009           := $9009
L9C00           := $9C00
LA000           := $A000
LA003           := $A003
LA006           := $A006

.segment "FIXED"

LC000:  php
LC001:  pha
        txa
        pha
        tya
        pha
        lda     $2002
        lda     $FF
        and     #$7F
        sta     $2000
        lda     #$00
        sta     $2001
        lda     $EE
        ora     $9A
        bne     LC088
        lda     $FC
        sta     $79
        lda     $FD
        sta     $7A
        lda     $F8
        sta     $78
        lda     $50
        bne     LC02F
        lda     $5E
        sta     $7B
LC02F:  lda     #$00
        sta     $2003
        lda     #$02
        sta     $4014
        lda     $19
        beq     LC040
        jsr     LC4F8
LC040:  lda     $1A
        beq     LC05B
        lda     $FF
        and     #$7F
        ora     #$04
        sta     $2000
        ldx     #$00
        stx     $1A
        jsr     LC4FC
        lda     $FF
        and     #$7F
        sta     $2000
LC05B:  lda     $18
        beq     LC088
        ldx     #$00
        stx     $18
        lda     $2002
        lda     #$3F
        sta     $2006
        stx     $2006
        ldy     #$20
LC070:  lda     $0600,x
        sta     $2007
        inx
        dey
        bne     LC070
        lda     #$3F
        sta     $2006
        sty     $2006
        sty     $2006
        sty     $2006
LC088:  lda     $78
        cmp     #$02
        bne     LC09D
        lda     $2002
        lda     $5F
        sta     $2005
        lda     #$00
        sta     $2005
        beq     LC0AA
LC09D:  lda     $2002
        lda     $79
        sta     $2005
        lda     $FA
        sta     $2005
LC0AA:  lda     $FE
        sta     $2001
        lda     $7A
        and     #$03
        ora     $FF
        sta     $2000
        jsr     LFF41
        lda     $F0
        sta     L8000
        lda     $7B
        sta     LC000
        sta     LC001
        ldx     $9B
        sta     LE000,x
        beq     LC0E7
        ldx     $78
        lda     $50
        beq     LC0DD
        lda     $7B
        cmp     $51
        bcc     LC0DD
        ldx     #$01
LC0DD:  lda     LC4C8,x
        sta     L009C
        lda     LC4DA,x
        sta     $9D
LC0E7:  inc     $92
        ldx     #$FF
        stx     $90
        inx
        ldy     #$04
LC0F0:  lda     $80,x
        cmp     #$01
        bne     LC0FE
        dec     $81,x
        bne     LC0FE
        lda     #$04
        sta     $80,x
LC0FE:  inx
        inx
        inx
        inx
        dey
        bne     LC0F0
        tsx
        lda     $0107,x
        sta     $7D
        lda     $0106,x
        sta     $7C
        lda     #$C1
        sta     $0107,x
        lda     #$21
        sta     $0106,x
        pla
        tay
        pla
        tax
        pla
        plp
        rti

        php
        php
        php
        pha
        txa
        pha
        tya
        pha
        tsx
        sec
        lda     $7C
        sbc     #$01
        sta     $0105,x
        lda     $7D
        sbc     #$00
        sta     $0106,x
        jsr     LFF90
        pla
        tay
        pla
        tax
        pla
        plp
        rts

LC143:  php
        pha
        txa
        pha
        tya
        pha
        sta     LE000
        sta     LE001
        jmp     (L009C)

LC152:  lda     $78
        cmp     #$0B
        beq     LC17B
        lda     $2002
        lda     $52
        sta     $2006
        lda     #$C0
        sta     $2006
        lda     $52
        lsr     a
        lsr     a
        and     #$03
        ora     #$98
        sta     $2000
        lda     #$00
        sta     $2005
        sta     $2005
        jmp     LC4BD

LC17B:  lda     $2002
        lda     #$20
        sta     $2006
        lda     #$00
        sta     $2006
        lda     #$98
        sta     $2000
        lda     #$00
        sta     $2005
        sta     $2005
        jmp     LC4BD

        lda     $2002
        lda     $79
        sta     $2005
        lda     #$00
        sta     $2005
        lda     $50
        beq     LC1BE
        lda     $51
        sec
        sbc     #$9F
        sta     LC000
        lda     LC4C9
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     LC4BD

LC1BE:  jmp     LC4BA

        lda     $2002
        lda     #$28
        sta     $2006
        lda     #$00
        sta     $2006
        lda     $FF
        ora     #$02
        sta     $2000
        lda     #$00
        sta     $2005
        sta     $2005
        lda     #$B0
        sec
        sbc     $7B
        sta     LC000
        ldx     $78
        lda     $50
        beq     LC1F3
        lda     $51
        cmp     #$B0
        bne     LC1F3
        ldx     #$00
LC1F3:  lda     LC4C9,x
        sta     L009C
        lda     LC4DB,x
        sta     $9D
        jmp     LC4BD

        lda     $2002
        lda     #$22
        sta     $2006
        lda     #$C0
        sta     $2006
        lda     $FF
        sta     $2000
        lda     #$00
        sta     $2005
        lda     #$B0
        sta     $2005
        lda     $50
        beq     LC1BE
        lda     $51
        sec
        sbc     #$B0
        sta     LC000
        lda     LC4C9
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     LC4BD

        lda     $2002
        lda     #$20
        sta     $2006
        lda     $79
        lsr     a
        lsr     a
        lsr     a
        and     #$1F
        ora     #$00
        sta     $2006
        lda     $FF
        and     #$FC
        sta     $2000
        lda     $79
        sta     $2005
        lda     #$00
        sta     $2005
        lda     #$C0
        sec
        sbc     $7B
        sta     LC000
        lda     LC4CE
        sta     L009C
        lda     LC4E0
        sta     $9D
        jmp     LC4BD

        lda     $2002
        lda     #$23
        sta     $2006
        lda     $79
        lsr     a
        lsr     a
        lsr     a
        and     #$1F
        ora     #$00
        sta     $2006
        lda     $FF
        and     #$FC
        sta     $2000
        lda     $79
        sta     $2005
        lda     #$C0
        sta     $2005
        jmp     LC4BA

        lda     $2002
        lda     $79
        eor     #$FF
        clc
        adc     #$01
        sta     L009C
        lda     $7A
        eor     #$FF
        adc     #$00
        and     #$01
        sta     $9D
        lda     $FF
        and     #$FC
        ora     $9D
        sta     $2000
        lda     L009C
        sta     $2005
        lda     #$58
        sta     $2005
        lda     #$40
        sta     LC000
        lda     LC4D0
        sta     L009C
        lda     LC4E2
        sta     $9D
        jmp     LC4BD

        lda     $2002
        lda     $7A
        and     #$01
        asl     a
        asl     a
        ora     #$22
        sta     $2006
        lda     $79
        lsr     a
        lsr     a
        lsr     a
        and     #$1F
        ora     #$60
        sta     $2006
        lda     $7A
        and     #$03
        ora     $FF
        sta     $2000
        lda     $79
        sta     $2005
        lda     #$98
        sta     $2005
        jmp     LC4BA

        lda     $2002
        ldy     $73
        lda     $69
        eor     LC4EC,y
        clc
        adc     LC4EF,y
        sta     $2005
        lda     LC4F2,y
        sta     $2005
        lda     #$0E
        sta     LC000
        lda     LC4D2
        sta     L009C
        lda     LC4E4
        sta     $9D
        jmp     LC4BD

        lda     $2002
        ldy     $73
        lda     #$00
        sta     $2005
        lda     LC4F5,y
        sta     $2005
        inc     $73
        lda     $73
        cmp     #$03
        beq     LC355
        lda     #$20
        sta     LC000
        lda     LC4D1
        sta     L009C
        lda     LC4E3
        sta     $9D
        jmp     LC4BD

LC355:  lda     #$00
        sta     $73
        lda     $50
        beq     LC372
        lda     $51
        sec
        sbc     #$A0
        sta     LC000
        lda     LC4C9
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     LC4BD

LC372:  jmp     LC4BA

        lda     $2002
        lda     #$21
        sta     $2006
        lda     #$40
        sta     $2006
        lda     $FF
        and     #$FC
        sta     $2000
        lda     #$00
        sta     $2005
        sta     $2005
        lda     #$4C
        sta     LC000
        lda     LC4D4
        sta     L009C
        lda     LC4E6
        sta     $9D
        jmp     LC4BD

        lda     $2002
        lda     $6A
        sta     $2005
        lda     #$00
        sta     $2005
        lda     $50
        beq     LC3C9
        lda     $51
        sec
        sbc     #$A0
        sta     LC000
        lda     LC4C9
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     LC4BD

LC3C9:  jmp     LC4BA

        lda     $2002
        lda     $6B
        asl     a
        asl     a
        ora     #$20
        sta     $2006
        lda     $6A
        lsr     a
        lsr     a
        lsr     a
        ora     #$E0
        sta     $2006
        lda     $6A
        sta     $2005
        lda     $7B
        sta     $2005
        lda     $FF
        ora     $6B
        sta     $2000
        lda     #$AE
        sec
        sbc     $7B
        sta     LC000
        lda     LC4D6
        sta     L009C
        lda     LC4E8
        sta     $9D
        jmp     LC4BD

        lda     $50
        beq     LC417
        lda     $51
        sec
        sbc     #$B0
        tax
        bne     LC417
        jmp     LC152

LC417:  lda     $2002
        lda     #$22
        sta     $2006
        lda     #$C0
        sta     $2006
        lda     $FF
        and     #$FC
        sta     $2000
        lda     #$00
        sta     $2005
        sta     $2005
        lda     $50
        beq     LC447
        stx     LC000
        lda     LC4C9
        sta     L009C
        lda     LC4DB
        sta     $9D
        jmp     LC4BD

LC447:  jmp     LC4BA

        lda     $2002
        lda     $69
        sta     $2005
        lda     #$00
        sta     $2005
        lda     #$30
        sta     LC000
        lda     LC4D8
        sta     L009C
        lda     LC4EA
        sta     $9D
        jmp     LC4BD

        lda     $2002
        lda     $6A
        sta     $2005
        lda     #$00
        sta     $2005
        lda     $FF
        and     #$FC
        ora     $6B
        sta     $2000
        lda     #$66
        sta     $E8
        lda     #$72
        sta     $E9
        jsr     LFF45
        lda     $F0
        sta     L8000
        lda     #$78
        sta     $E8
        lda     #$7A
        sta     $E9
        inc     $1B
        jmp     LC4BA

        lda     $E8
        pha
        lda     $E9
        pha
        lda     #$66
        sta     $E8
        lda     #$72
        sta     $E9
        jsr     LFF45
        lda     $F0
        sta     L8000
        pla
        sta     $E9
        pla
        sta     $E8
        inc     $1B
LC4BA:  sta     LE000
LC4BD:  pla
        tay
        pla
        tax
        pla
        plp
        rti

        .byte   $00,$00,$00,$00
LC4C8:  .byte   $BA
LC4C9:  .byte   $52,$98,$C1,$00,$35
LC4CE:  .byte   $6F,$97
LC4D0:  .byte   $D2
LC4D1:  .byte   $02
LC4D2:  .byte   $2B,$75
LC4D4:  .byte   $A3,$CC
LC4D6:  .byte   $08,$4A
LC4D8:  .byte   $69,$9C
LC4DA:  .byte   $C4
LC4DB:  .byte   $C1,$C1,$C1,$C2,$C2
LC4E0:  .byte   $C2,$C2
LC4E2:  .byte   $C2
LC4E3:  .byte   $C3
LC4E4:  .byte   $C3,$C3
LC4E6:  .byte   $C3,$C3
LC4E8:  .byte   $C4,$C4
LC4EA:  .byte   $C4,$C4
LC4EC:  .byte   $FF,$00,$FF
LC4EF:  .byte   $01,$00,$01
LC4F2:  .byte   $30,$60,$90
LC4F5:  .byte   $40,$70,$A0
LC4F8:  ldx     #$00
        stx     $19
LC4FC:  lda     $0780,x
        bmi     LC51C
        sta     $2006
        lda     $0781,x
        sta     $2006
        ldy     $0782,x
LC50D:  lda     $0783,x
        sta     $2007
        inx
        dey
        bpl     LC50D
        inx
        inx
        inx
        bne     LC4FC
LC51C:  rts

        lda     $FF
        and     #$11
        sta     $FF
        sta     $2000
        rts

        lda     $FF
        ora     #$80
        sta     $FF
        sta     $2000
        rts

        inc     $EE
        lda     #$00
        sta     $FE
        sta     $2001
        rts

        dec     $EE
        lda     #$18
        sta     $FE
        sta     $2001
        rts

LC545:  ldx     #$01
        stx     $4016
        dex
        stx     $4016
        ldx     #$08
LC550:  lda     $4016
        lsr     a
        rol     $14
        lsr     a
        rol     L0000
        lda     $4017
        lsr     a
        rol     $15
        lsr     a
        rol     $01
        dex
        bne     LC550
        lda     L0000
        ora     $14
        sta     $14
        lda     $01
        ora     $15
        sta     $15
        ldx     #$01
LC573:  lda     $14,x
        tay
        eor     $16,x
        and     $14,x
        sta     $14,x
        sty     $16,x
        dex
        bpl     LC573
        ldx     #$03
LC583:  lda     $14,x
        and     #$0C
        cmp     #$0C
        beq     LC593
        lda     $14,x
        and     #$03
        cmp     #$03
        bne     LC599
LC593:  lda     $14,x
        and     #$F0
        sta     $14,x
LC599:  dex
        bpl     LC583
        rts

LC59D:  sta     L0000
        stx     $01
        sty     $02
        lda     $2002
        lda     $FF
        and     #$FE
        sta     $2000
        lda     L0000
        sta     $2006
        ldy     #$00
        sty     $2006
        ldx     #$04
        cmp     #$20
        bcs     LC5BF
        ldx     $02
LC5BF:  ldy     #$00
        lda     $01
LC5C3:  sta     $2007
        dey
        bne     LC5C3
        dex
        bne     LC5C3
        ldy     $02
        lda     L0000
        cmp     #$20
        bcc     LC5E6
        adc     #$02
        sta     $2006
        lda     #$C0
        sta     $2006
        ldx     #$40
LC5E0:  sty     $2007
        dex
        bne     LC5E0
LC5E6:  ldx     $01
        rts

LC5E9:  lda     $30
        cmp     #$07
        bne     LC5F5
        ldx     #$6C
        stx     $97
        bne     LC60A
LC5F5:  ldx     $97
        cpx     #$04
        bne     LC600
        lda     #$F8
        sta     $0200
LC600:  lda     $50
        bne     LC60A
        lda     $72
        beq     LC60A
        ldx     #$30
LC60A:  lda     #$F8
LC60C:  sta     $0200,x
        inx
        inx
        inx
        inx
        bne     LC60C
        lda     $50
        bne     LC627
        lda     $71
        bne     LC63E
        lda     $72
        bne     LC669
        lda     $F8
        cmp     #$02
        beq     LC696
LC627:  rts

LC628:  ldx     #$1F
LC62A:  lda     #$00
        sta     $0300,x
        lda     #$FF
        sta     $04C0,x
        dex
        bne     LC62A
        lda     #$00
        sta     $71
        sta     $72
        rts

LC63E:  lda     #$00
        sta     $71
        ldy     #$2C
LC644:  lda     LC6D8,y
        sta     $0200,y
        lda     LC6D9,y
        sta     $0201,y
        lda     LC6DA,y
        sta     $0202,y
        lda     LC6DB,y
        sta     $0203,y
        dey
        dey
        dey
        dey
        bpl     LC644
        sty     $72
        lda     #$30
        sta     $97
        rts

LC669:  ldy     #$14
LC66B:  lda     $0203,y
        sec
        sbc     #$01
        sta     $0203,y
        dey
        dey
        dey
        dey
        bpl     LC66B
        lda     $95
        and     #$01
        bne     LC691
        ldy     #$14
LC682:  lda     $021B,y
        sec
        sbc     #$01
        sta     $021B,y
        dey
        dey
        dey
        dey
        bpl     LC682
LC691:  lda     #$30
        sta     $97
        rts

LC696:  lda     $FC
        sta     L0000
        lda     $F9
        lsr     a
        ror     L0000
        lsr     a
        ror     L0000
        lda     $95
        and     #$01
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        tay
        ldx     #$1C
LC6AE:  lda     LC708,y
        sta     $0200,x
        lda     LC709,y
        sta     $0201,x
        lda     LC70A,y
        sta     $0202,x
        lda     LC70B,y
        sec
        sbc     L0000
        sta     $0203,x
        iny
        iny
        iny
        iny
        dex
        dex
        dex
        dex
        bpl     LC6AE
        lda     #$20
        sta     $97
        rts

LC6D8:  .byte   $58
LC6D9:  .byte   $F1
LC6DA:  .byte   $02
LC6DB:  .byte   $28,$E0,$F1,$02,$28,$B8,$F1,$02
        .byte   $70,$20,$F1,$02,$A0,$68,$F1,$02
        .byte   $D0,$D8,$F1,$02,$D0,$90,$F2,$02
        .byte   $10,$40,$F2,$02,$58,$D0,$F2,$02
        .byte   $58,$78,$F2,$02,$80,$28,$F2,$02
        .byte   $D8,$A8,$F2,$02,$D8
LC708:  .byte   $90
LC709:  .byte   $E4
LC70A:  .byte   $03
LC70B:  .byte   $18,$28,$E4,$03,$20,$68,$E4,$03
        .byte   $30,$58,$E4,$03,$60,$80,$E4,$03
        .byte   $70,$10,$E4,$03,$98,$58,$E4,$03
        .byte   $C0,$80,$E4,$03,$D0,$18,$E4,$03
        .byte   $10,$A0,$E4,$03,$48,$28,$E4,$03
        .byte   $58,$40,$E4,$03,$90,$98,$E4,$03
        .byte   $A0,$78,$E4,$03,$D8,$30,$E4,$03
        .byte   $E0,$A0,$E4,$03,$E8,$00,$00,$00
        .byte   $00
LC74C:  lda     #$30
        ldx     #$F0
        bne     LC755
LC752:  lda     #$10
        tax
LC755:  sta     $0F
        stx     $0D
        ldy     #$04
        sty     $0E
LC75D:  ldy     #$1F
LC75F:  lda     $0620,y
        sta     $0600,y
        dey
        bpl     LC75F
        ldy     #$1F
LC76A:  lda     $0600,y
        sec
        sbc     $0F
        bpl     LC774
        lda     #$0F
LC774:  sta     $0600,y
        dey
        bpl     LC76A
        sty     $18
        lda     $0E
LC77E:  pha
        jsr     LFF21
        pla
        sec
        sbc     #$01
        bne     LC77E
        lda     $0F
        clc
        adc     $0D
        sta     $0F
        cmp     #$50
        beq     LC797
        lda     $0F
        bpl     LC75D
LC797:  rts

LC798:  lda     $1C
        beq     LC7DC
        lda     $95
        and     #$03
        bne     LC7DC
        ldy     #$1F
LC7A4:  lda     $0620,y
        sta     $0600,y
        dey
        bpl     LC7A4
        ldy     #$0F
LC7AF:  lda     $0600,y
        sec
        sbc     $1D
        bpl     LC7B9
        lda     #$0F
LC7B9:  sta     $0600,y
        dey
        bpl     LC7AF
        sty     $18
        lda     $1D
        clc
        adc     $1E
        sta     $1D
        cmp     #$F0
        beq     LC7D4
        cmp     #$50
        bne     LC7DC
        inc     $71
        bne     LC7D8
LC7D4:  lda     #$00
        sta     $72
LC7D8:  lda     #$00
        sta     $1C
LC7DC:  rts

        stx     $0E
        sty     $0F
        asl     a
        tay
        iny
        pla
        sta     L000C
        pla
        sta     $0D
        lda     (L000C),y
        tax
        iny
        lda     (L000C),y
        sta     $0D
        stx     L000C
        ldx     $0E
        ldy     $0F
        jmp     (L000C)

LC7FB:  ldx     #$00
        ldy     #$04
        lda     $E4,x
        and     #$02
        sta     L0000
        lda     $E5,x
        and     #$02
        eor     L0000
        clc
        beq     LC80F
        sec
LC80F:  ror     $E4,x
        inx
        dey
        bne     LC80F
        rts

LC816:  lda     $22
        sta     $F5
        jsr     LFF6B
        lda     $AA80
        sta     $E8
        lda     $AA81
        sta     $E9
        ldx     #$07
LC829:  lda     LC898,x
        sta     $0610,x
        sta     $0630,x
        dex
        bpl     LC829
        lda     #$00
        sta     $EA
        lda     #$01
        sta     $EB
LC83D:  lda     $2B
        asl     a
        tax
        lda     $AA60,x
        pha
        lda     $AA61,x
        asl     a
        asl     a
        sta     L0000
        asl     a
        asl     a
        adc     L0000
        tay
        ldx     #$00
LC853:  lda     $AA82,y
        sta     $0600,x
        sta     $0620,x
        iny
        inx
        cpx     #$10
        bne     LC853
        ldx     #$00
LC864:  lda     $AA82,y
        pha
        and     #$80
        sta     $0100,x
        pla
        and     #$7F
        sta     $0108,x
        lda     #$00
        sta     $0104,x
        sta     $010C,x
        iny
        inx
        cpx     #$04
        bne     LC864
        lda     $0600
        sta     $0610
        sta     $0630
        lda     #$01
        sta     $F5
        jsr     LFF6B
        pla
        jsr     LA000
        jmp     LFF3C

LC898:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
LC8A0:  txa
        pha
        tya
        pha
        lda     $F5
        cmp     #$13
        beq     LC8B4
        ldy     $22
        lda     LC8B9,y
        sta     $F5
        jsr     LFF6B
LC8B4:  pla
        tay
        pla
        tax
        rts

LC8B9:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0D,$0D,$0F
        .byte   $0D,$11,$12,$13,$10,$1E,$0E
        ldx     #$BF
        txs
        lda     $FF
        sta     $2000
        jsr     LFF21
        lda     #$88
        sta     $E4
        cli
        lda     #$01
        sta     $9B
        lda     #$00
        jsr     LF898
        lda     #$40
        sta     $99
        lda     #$18
        sta     $F4
        jsr     LFF6B
        jsr     L9009
        lda     #$9C
        sta     $A9
        lda     #$02
        sta     $AE
LC8FF:  lda     #$00
        ldy     #$1F
LC903:  sta     $0150,y
        dey
        bpl     LC903
LC909:  lda     #$00
        sta     $EE
        jsr     LC752
        lda     #$04
        sta     $97
        jsr     LC5E9
        jsr     LFF21
        jsr     LC628
        lda     #$01
        sta     LA000
        lda     #$00
        sta     $F8
        sta     $0300
        sta     $FD
        sta     $71
        sta     $72
        sta     $FC
        sta     $FA
        sta     $F9
        sta     $25
        sta     $0380
        sta     $03E0
        sta     $B1
        sta     $B2
        sta     $B3
        sta     $9E
        sta     $9F
        sta     $5A
        sta     $A0
        sta     $B4
        sta     $A1
        sta     $6F
        ldy     $22
        lda     LCD0C,y
        jsr     LF898
        lda     #$01
        sta     $31
        sta     $23
        sta     $2E
        lda     #$FF
        sta     $29
        lda     #$1F
        sta     $24
LC969:  lda     #$01
        sta     $10
        jsr     LE4F1
        jsr     LFF21
        lda     $29
        beq     LC969
        lda     $22
        sta     $F5
        jsr     LFF6B
        lda     $AA40
        pha
        and     #$E0
        sta     $2A
        ldx     #$01
        ldy     #$2A
        and     #$C0
        beq     LC991
        dex
        ldy     #$26
LC991:  stx     LA000
        sty     $52
        pla
        and     #$1F
        sta     $2C
        lda     #$00
        sta     $2B
        sta     $2D
        jsr     LC816
        lda     #$00
        sta     $18
        jsr     LFF21
        jsr     LC74C
        lda     #$80
        sta     $0360
LC9B3:  lda     #$9C
        sta     $A2
        lda     #$E8
        sta     $51
        sta     $5E
        lda     $F9
        bne     LC9D3
        lda     $22
        cmp     #$02
        beq     LC9CB
        cmp     #$09
        bne     LC9D3
LC9CB:  lda     #$9F
        sta     $5E
        lda     #$02
        sta     $F8
LC9D3:  lda     #$74
        sta     $EA
        lda     #$75
        sta     $EB
        jsr     LFF3C
        lda     #$30
        sta     $0611
        inc     $18
        lda     #$3C
LC9E7:  pha
        lda     $95
        and     #$10
        beq     LCA10
        ldx     #$10
LC9F0:  lda     LCCF8,x
        sta     $0200,x
        lda     LCCF9,x
        sta     $0201,x
        lda     LCCFA,x
        sta     $0202,x
        lda     LCCFB,x
        sta     $0203,x
        dex
        dex
        dex
        dex
        bpl     LC9F0
        lda     #$14
LCA10:  sta     $97
        tax
        lda     #$F8
LCA15:  sta     $0200,x
        inx
        inx
        inx
        inx
        bne     LCA15
        lda     #$00
        sta     $EE
        jsr     LFF21
        inc     $EE
        inc     $95
        pla
        sec
        sbc     #$01
        bne     LC9E7
        lda     #$0F
        sta     $0611
        inc     $18
        lda     #$00
        sta     $EA
        lda     #$01
        sta     $EB
        jsr     LFF3C
        lda     #$80
        sta     $0300
        lda     #$00
        sta     $03C0
        lda     #$D0
        sta     $0580
        lda     #$4C
        sta     $0400
        lda     #$01
        sta     $0420
        lda     #$00
        sta     $0440
        lda     #$F9
        sta     $0460
        ldx     #$00
        lda     #$13
        jsr     LF835
        lda     #$04
        sta     $30
        lda     #$80
        sta     $B2
LCA73:  lda     $14
        and     #$10
        beq     LCAB5
        lda     $30
        cmp     #$04
        beq     LCAB5
        cmp     #$07
        beq     LCAB5
        cmp     #$09
        bcs     LCAB5
        lda     $A0
        sec
        sbc     #$06
        bcc     LCA99
        and     #$01
        beq     LCA99
        lda     #$00
        sta     $0301
        beq     LCAA4
LCA99:  lda     $0301
        ora     $0302
        ora     $0303
        bmi     LCAB5
LCAA4:  ldy     $30
        lda     LCD1E,y
        bmi     LCAB5
        lda     #$02
        sta     $F5
        jsr     LFF6B
        jsr     LA003
LCAB5:  lda     $22
        sta     $F5
        jsr     LFF6B
        jsr     LCD34
        lda     $3D
        beq     LCAD7
        sta     $30
        cmp     #$0E
        bne     LCAD3
        lda     #$F2
        jsr     LF89A
        lda     #$17
        jsr     LF89A
LCAD3:  lda     #$00
        sta     $3D
LCAD7:  jsr     LE16A
        lda     $FC
        sta     $25
        sta     L0000
        lda     $F8
        cmp     #$02
        bne     LCAF2
        lda     $F9
        lsr     a
        ror     L0000
        lsr     a
        ror     L0000
        lda     L0000
        sta     $5F
LCAF2:  lda     $0380
        cmp     $6F
        bcc     LCAFB
        sta     $6F
LCAFB:  lda     $0360
        sta     $27
        ldx     #$1C
        stx     $F4
        inx
        stx     $F5
        jsr     LFF6B
        jsr     L8000
        lda     #$1A
        sta     $F4
        lda     $22
        sta     $F5
        jsr     LFF6B
        jsr     L9C00
        lda     #$09
        sta     $F4
        jsr     LFF6B
        jsr     L8003
        jsr     L8006
        jsr     L800F
        jsr     L8009
        jsr     L800C
        jsr     L8000
        jsr     L8012
        jsr     LC798
        jsr     LFF57
        lda     $98
        and     #$02
        beq     LCB49
        lda     #$01
        ora     $17
        sta     $17
LCB49:  jsr     LC7FB
        lda     $59
        bne     LCB5B
        lda     $3C
        bne     LCB78
        lda     $74
        bne     LCBCE
        jmp     LCA73

LCB5B:  lda     #$00
        sta     $EE
        sta     $71
        sta     $72
        sta     $F8
        sta     $5A
        lda     #$18
        sta     $F4
        lda     #$10
        sta     $F5
        jsr     LFF6B
        jsr     L9000
        jmp     LC8FF

LCB78:  lda     #$00
        sta     $3C
        sta     $5A
        lda     $AE
        sec
        sbc     #$01
        bcc     LCBB1
        sta     $AE
        and     #$0F
        cmp     #$0F
        bne     LCB94
        lda     $AE
        sec
        sbc     #$06
        sta     $AE
LCB94:  lda     #$00
        sta     $EE
        sta     $71
        sta     $72
        sta     $F8
        lda     $60
        cmp     #$12
        beq     LCBA7
        jmp     LCC18

LCBA7:  lda     #$18
        sta     $F4
        jsr     LFF6B
        jmp     L9006

LCBB1:  lda     #$00
        sta     $EE
        sta     $5A
        sta     $71
        sta     $72
        sta     $F8
        lda     #$18
        sta     $F4
        lda     #$13
        sta     $F5
        jsr     LFF6B
        jsr     L9003
        jmp     LC8FF

LCBCE:  pha
        lda     #$00
        sta     $5A
        sta     $74
        sta     $B1
        sta     $B2
        sta     $B3
        sta     $5A
        sta     $F9
        sta     $F8
        lda     #$0B
        sta     $F4
        lda     #$0E
        sta     $F5
        jsr     LFF6B
        pla
        and     #$7F
        bne     LCBF7
        jsr     L8000
        jmp     LC8FF

LCBF7:  lda     $75
        cmp     #$06
        beq     LCC03
        jsr     L8003
        jmp     LC8FF

LCC03:  lda     #$0C
        sta     $F4
        lda     #$0E
        sta     $F5
        jsr     LFF6B
        lda     #$11
        sta     $F8
        jsr     L8000
        jmp     LC8FF

LCC18:  lda     $22
        sta     $F5
        jsr     LFF6B
        ldy     #$00
LCC21:  lda     $6F
        cmp     $AAF8,y
        bcc     LCC2B
        iny
        bne     LCC21
LCC2B:  dey
        bpl     LCC31
        jmp     LC909

LCC31:  tya
        pha
        jsr     LC752
        lda     #$04
        sta     $97
        jsr     LC5E9
        jsr     LFF21
        jsr     LC628
        lda     #$00
        sta     $F8
        sta     $FD
        sta     $71
        sta     $72
        sta     $FC
        sta     $FA
        sta     $25
        sta     $03E0
        sta     $B1
        sta     $B2
        sta     $B3
        sta     $5A
        sta     $A0
        sta     $B4
        sta     $A1
        pla
        tay
        lda     $AAF8,y
        sta     $29
        sta     $F9
        sta     $0380
        lda     $ABC0,y
        sta     $9E
        sta     $9F
        ldx     $AAF0,y
        stx     $2B
        lda     $AA40,x
        pha
        sta     L0000
        and     #$20
        sta     $2A
        bne     LCC8E
        lda     L0000
        and     #$C0
        sta     $2A
LCC8E:  ldx     #$01
        ldy     #$2A
        and     #$20
        bne     LCC99
        dex
        ldy     #$26
LCC99:  stx     LA000
        sty     $52
        pla
        and     #$1F
        sta     $2C
        lda     #$00
        sta     $2D
        ldy     $22
        lda     LCD0C,y
        jsr     LF898
        lda     #$01
        sta     $31
        sta     $23
        sta     $2E
        dec     $29
        lda     #$1F
        sta     $24
        lda     #$21
LCCBF:  pha
        lda     #$01
        sta     $10
        jsr     LE4F1
        jsr     LFF21
        pla
        sec
        sbc     #$01
        bne     LCCBF
        jsr     LC816
        lda     #$00
        sta     $18
        jsr     LFF21
        jsr     LC74C
        lda     #$80
        sta     $0360
        jmp     LC9B3

        lda     $98
        and     #$03
        cmp     #$01
        bne     LCCF7
        ldy     #$0B
LCCEF:  lda     #$9C
LCCF1:  .byte   $99
LCCF2:  ldx     #$00
        dey
        bpl     LCCF1
LCCF7:  .byte   $60
LCCF8:  .byte   $80
LCCF9:  .byte   $40
LCCFA:  .byte   $00
LCCFB:  .byte   $6C,$80,$41,$00,$74,$80,$42,$00
        .byte   $7C,$80,$43,$00,$84,$80,$44,$00
        .byte   $8C
LCD0C:  .byte   $01,$02,$03,$04,$05,$06,$07,$08
        .byte   $01,$03,$07,$08,$09,$09,$0A,$0A
        .byte   $0B,$0B
LCD1E:  .byte   $00,$00,$FF,$00,$FF,$00,$FF,$FF
        .byte   $00,$FF,$00,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF
        .byte   $FF
LCD34:  lda     $0420
        cmp     #$02
        bne     LCD41
        lda     $30
        cmp     #$02
        beq     LCD4B
LCD41:  lda     #$4C
        sta     $0400
        lda     #$01
        sta     $0420
LCD4B:  lda     #$40
        sta     $99
        ldx     #$00
        stx     $5D
        lda     $36
        beq     LCD5A
        jsr     LCDEC
LCD5A:  lda     $39
        beq     LCD6A
        dec     $39
        bne     LCD6A
        lda     $05E0
        and     #$7F
        sta     $05E0
LCD6A:  lda     $16
        and     #$40
        bne     LCD76
        lda     #$E0
        sta     $1F
        bne     LCD8B
LCD76:  lda     $1F
        clc
        adc     #$20
        sta     $1F
        bne     LCD8B
        lda     $A0
        cmp     #$02
        bne     LCD8B
        lda     $14
        ora     #$40
        sta     $14
LCD8B:  ldy     $30
        lda     LCD9A,y
        sta     L0000
        lda     LCDB0,y
        sta     $01
        jmp     (L0000)

LCD9A:  .byte   $36,$07,$FD,$EB,$BA,$13,$AB,$31
        .byte   $58,$29,$91,$BE,$D3,$E1,$79,$CC
        .byte   $14,$AA,$52,$33,$8A,$8C
LCDB0:  .byte   $CE,$D0,$D3,$D4,$D5,$D6,$D6,$D8
        .byte   $D8,$D9,$D9,$D9,$D9,$DB,$D7,$CD
        .byte   $DD,$DD,$DE,$DF,$DF,$E0,$66,$61
        .byte   $E0,$CF,$CE
        .byte   $CF
        lda     $5A
        bmi     LCDE6
        ldy     #$00
        jsr     LF67C
        bcc     LCDE5
        lda     #$01
        cmp     $05C0
        beq     LCDE5
        jsr     LF835
        lda     #$00
        sta     $32
LCDE5:  rts

LCDE6:  lda     #$00
        sta     $05E0
        rts

LCDEC:  lda     $0580
        pha
        lda     $0400
        pha
        lda     $0420
        pha
        lda     $37
        sta     $0400
        lda     $38
        sta     $0420
        ldy     $30
        cpy     #$02
        beq     LCE0A
        ldy     #$00
LCE0A:  lda     $36
        and     #$01
        beq     LCE16
        jsr     LF580
        jmp     LCE1A

LCE16:  iny
        jsr     LF5C4
LCE1A:  pla
        sta     $0420
        pla
        sta     $0400
        pla
        and     #$40
        sta     L0000
        lda     $0580
        and     #$BF
        ora     L0000
        sta     $0580
        lda     #$00
        sta     $36
LCE35:  rts

        jsr     LD007
        bcc     LCE35
        ldy     $5D
        cpy     #$01
        bne     LCE88
        lda     $05C0
        cmp     #$11
        bne     LCE4D
        lda     #$01
        jsr     LF835
LCE4D:  ldy     $05C1
        cpy     #$D7
        bcc     LCE88
        lda     LCCEF,y
        sta     L0000
        lda     LCCF2,y
        sta     $01
        jmp     (L0000)

        lda     $05A1
        bne     LCE88
        inc     $3A
        lda     #$EE
        sta     $0440
        lda     #$06
        sta     $0460
        inc     $05A1
        lda     #$3C
        sta     $0501
        lda     $0581
        and     #$FE
        sta     $0581
        jsr     LDED8
        jmp     LD007

LCE88:  lda     $05C0
        cmp     #$10
        bne     LCE9E
        lda     $14
        and     #$80
        beq     LCE35
        lda     $16
        and     #$04
        beq     LCE35
        jmp     LD38E

LCE9E:  lda     $14
        and     #$80
        beq     LCECD
        lda     $16
        and     #$04
        beq     LCEAD
        jmp     LD38E

LCEAD:  lda     $17
        and     #$01
        bne     LCEC0
        lda     #$E5
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        jmp     LD007

LCEC0:  lda     #$00
        sta     $0440
        lda     #$08
        sta     $0460
        jmp     LD007

LCECD:  lda     $05C0
        cmp     #$04
        beq     LCEEB
        cmp     #$05
        beq     LCEEB
        cmp     #$0D
        beq     LCEE4
        cmp     #$0E
        beq     LCEE4
        cmp     #$0F
        bne     LCF0A
LCEE4:  lda     $05A0
        bne     LCF0A
        beq     LCF59
LCEEB:  lda     $16
        and     $31
        beq     LCEF9
        pha
        jsr     LCF59
        pla
        jmp     LCF3D

LCEF9:  lda     #$0D
        jsr     LF835
        lda     $32
        beq     LCF05
        jsr     LD370
LCF05:  lda     $31
        jmp     LCF3D

LCF0A:  lda     $16
        and     #$03
        beq     LCF4B
        sta     $31
        jsr     LCF59
        lda     $05C0
        cmp     #$0D
        beq     LCF24
        cmp     #$0E
        beq     LCF24
        cmp     #$0F
        bne     LCEF9
LCF24:  lda     #$04
        jsr     LF835
        lda     $31
        ldy     $32
        beq     LCF3D
        jsr     LD370
        ldy     $A0
        cmp     #$04
        bne     LCF3D
        ldy     #$AA
        sta     $05C0
LCF3D:  and     #$01
        beq     LCF46
        ldy     #$00
        jmp     LF580

LCF46:  ldy     #$01
        jmp     LF5C4

LCF4B:  lda     $32
        bne     LCF59
        lda     #$01
        cmp     $05C0
        beq     LCF59
        jsr     LF835
LCF59:  jsr     LD355
        lda     $14
        and     #$40
        beq     LCF65
        jsr     LD0DA
LCF65:  rts

        jsr     LDED8
        lda     #$82
        cmp     $0321
        beq     LCF7B
        sta     $0321
        lda     $05E1
        and     #$7F
        sta     $05E1
LCF7B:  lda     $14
        and     #$80
        beq     LCF84
        jmp     LCEAD

LCF84:  lda     $16
        and     #$03
        beq     LCF9B
        sta     $31
        jsr     LCF3D
        lda     #$01
        jsr     LF835
        lda     $32
        beq     LCF9B
        jsr     LD370
LCF9B:  lda     $16
        and     #$0C
        beq     LCFD3
        pha
        lda     #$80
        sta     $0440,x
        lda     #$01
        sta     $0460,x
        pla
        and     #$08
        beq     LCFC2
        ldy     #$01
        jsr     LF642
        lda     #$0C
        cmp     $03C0
        bcc     LCFC7
        sta     $03C0
        bcs     LCFC7
LCFC2:  ldy     #$07
        jsr     LF606
LCFC7:  lda     $03C0
        clc
        adc     #$0E
        sta     $03C1
        jsr     LF81B
LCFD3:  jsr     LD355
        lda     $14
        and     #$40
        beq     LCFDF
        jsr     LD0DA
LCFDF:  rts

        lda     $0361
        sta     $0360
        lda     $0381
        sta     $0380
        lda     $03C1
        sta     $03C0
        lda     #$00
        sta     $0301
        lda     #$05
        sta     $EB
        jsr     LFF3C
        lda     #$08
        sta     $30
        lda     #$DA
        jmp     LF835

LD007:  lda     $0460
        bmi     LD019
        lda     $3A
        bne     LD01D
        lda     $16
        and     #$80
        bne     LD019
        jsr     LF81B
LD019:  lda     #$00
        sta     $3A
LD01D:  ldy     #$06
        jsr     LE8D6
        lda     $10
        sta     $0F
        ldy     #$00
        jsr     LF67C
        php
        lda     $0F
        cmp     #$80
        bne     LD07E
        lda     $B9
        bne     LD084
        lda     #$03
        sta     $B9
        lda     $BA
        bne     LD084
        inc     $BA
        lda     #$1F
        jsr     LF89A
        lda     #$80
        sta     $0304
        lda     $0580
        sta     $0584
        ldx     #$04
        lda     #$68
        jsr     LF835
        lda     $0360
        sta     $0364
        lda     $0380
        sta     $0384
        lda     $03C0
        and     #$F0
        sec
        sbc     #$08
        sta     $03C4
        lda     $03E0
        sta     $03E4
        ldx     #$00
        stx     $0484
        stx     $0324
        beq     LD084
LD07E:  lda     #$00
        sta     $BA
        sta     $B9
LD084:  plp
        jsr     LD47E
        bcc     LD0A7
        lda     $30
        beq     LD0A6
        lda     #$13
        jsr     LF89A
        lda     #$00
        sta     $30
        lda     #$0D
        jsr     LF835
        inc     $05A0
        lda     $32
        beq     LD0A6
        jsr     LD370
LD0A6:  rts

LD0A7:  lda     $30
        cmp     #$03
        beq     LD0D8
        cmp     #$01
        beq     LD0C1
        lda     #$07
        jsr     LF835
        lda     #$01
        sta     $30
        lda     $32
        beq     LD0C1
        jsr     LD370
LD0C1:  lda     $16
        and     #$03
        beq     LD0CC
        sta     $31
        jsr     LCF3D
LD0CC:  jsr     LD355
        lda     $14
        and     #$40
        beq     LD0D8
        jsr     LD0DA
LD0D8:  clc
        rts

LD0DA:  ldy     $A0
        lda     $A2,y
        and     #$1F
        beq     LD134
        lda     LD33D,y
        tay
LD0E7:  lda     $0300,y
        bpl     LD0F1
        dey
        bne     LD0E7
        beq     LD134
LD0F1:  ldy     $A0
        jsr     LDECE
        lda     LD303,y
        sta     L0000
        lda     LD30F,y
        sta     $01
        jmp     (L0000)

LD103:  lda     $32
        bne     LD10A
        jsr     LD370
LD10A:  lda     $05C0
        cmp     #$05
        beq     LD121
        cmp     #$0E
        beq     LD121
        cmp     #$0F
        beq     LD121
        lda     #$00
        sta     $05A0
        sta     $05E0
LD121:  lda     #$10
        sta     $32
        ldy     $A0
        lda     LD33D,y
        tay
LD12B:  lda     $0300,y
        bpl     LD135
        dey
        bne     LD12B
        clc
LD134:  rts

LD135:  ldx     $A0
        lda     LD349,x
        jsr     LF89A
        ldx     #$00
        lda     #$80
        sta     $0300,y
        lda     $31
        ror     a
        ror     a
        ror     a
        and     #$40
        ora     #$90
        sta     $0580,y
        lda     #$00
        sta     $0400,y
        lda     #$04
        sta     $0420,y
        lda     $31
        sta     $04A0,y
        and     #$02
        tax
        lda     $0360
        clc
        adc     LD31B,x
        sta     $0360,y
        lda     $0380
        adc     LD31C,x
        sta     $0380,y
        lda     $03C0
        sta     $03C0,y
        lda     $03E0
        sta     $03E0,y
        lda     #$00
        sta     $0480,y
        sta     $05A0,y
        sta     $05E0,y
        sta     $0340,y
        sta     $0500,y
        ldx     $A0
        lda     LD323,x
        sta     $05C0,y
        lda     LD32F,x
        sta     $0320,y
        inc     $3B
        ldx     #$00
        sec
        rts

        lda     $05C0
        cmp     #$D7
        bcs     LD1B7
        lda     $0580
        bpl     LD1FA
        lda     $0301
        bpl     LD1BA
LD1B7:  jmp     LD103

LD1BA:  ldy     #$01
        lda     #$13
        jsr     LF846
        lda     #$00
        sta     $03C1
        sta     $03E1
        lda     #$11
        sta     $0481
        lda     $31
        sta     $04A1
        and     #$02
        tax
        lda     $0360
        clc
        adc     LD31F,x
        sta     $0361
        lda     $0380
        adc     LD320,x
        sta     $0381
        lda     #$80
        sta     $0320,y
        lda     #$AB
        sta     $0441
        lda     #$FF
        sta     $0461
        ldx     #$00
LD1FA:  rts

        jsr     LD103
        bcc     LD211
        lda     $3B
        and     #$01
        tax
        lda     $03C0,y
        clc
        adc     LD33B,x
        sta     $03C0,y
        ldx     #$00
LD211:  rts

        jsr     LD103
        bcc     LD24C
        iny
        jsr     LD135
        iny
        jsr     LD135
        lda     #$B4
        sta     $0501
        sta     $0502
        sta     $0503
        lda     #$00
        sta     $0441
        sta     $0442
        sta     $0443
        sta     $0461
        sta     $0462
        sta     $0463
        lda     $0361
        and     #$FC
        sta     $0361
        sta     $0362
        sta     $0363
LD24C:  rts

        lda     $0301
        bmi     LD292
        ldy     $30
        cpy     #$04
        bcs     LD292
        lda     LD293,y
        beq     LD292
        jsr     LD103
        ldy     $30
        lda     LD293,y
        jsr     LF835
        cpy     #$03
        bne     LD271
        lda     #$AE
        sta     $05C1
LD271:  lda     #$00
        sta     $0401
        sta     $0421
        sta     $0461
        lda     #$80
        sta     $0441
        inc     $05E1
        lda     $30
        sta     $0500
        lda     #$10
        sta     $0520
        lda     #$0B
        sta     $30
LD292:  .byte   $60
LD293:  .byte   $AA,$AB,$00,$AD
LD297:  .byte   $01,$07,$00,$0A,$FC,$FF
        .byte   $04
        brk
        lda     $30
        cmp     #$01
        bne     LD2D2
        lda     #$A3
        cmp     $05C0
        beq     LD2D2
        jsr     LF835
        lda     #$2C
        jmp     LF89A

        jsr     LD103
        bcc     LD2D2
        lda     #$44
        sta     $0440,y
        lda     #$03
        sta     $0460,y
        lda     #$00
        sta     $0400,y
        lda     #$01
        sta     $0420,y
        lda     #$13
        sta     $0500,y
LD2D2:  rts

        jsr     LD103
        bcc     LD302
        lda     $16
        and     #$0B
        beq     LD2E1
        sta     $04A0,y
LD2E1:  lda     #$00
        sta     $0440,y
        lda     #$04
        sta     $0460,y
        lda     #$14
        sta     $0500,y
        lda     $0360,x
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
LD302:  .byte   $60
LD303:  .byte   $03,$12,$FB,$4D,$03,$9F,$B4,$A6
        .byte   $03,$A6,$D3,$A6
LD30F:  .byte   $D1,$D2,$D1,$D2,$D1,$D2,$D2,$D1
        .byte   $D1,$D1,$D2,$D1
LD31B:  .byte   $0F
LD31C:  .byte   $00,$F0,$FF
LD31F:  .byte   $17
LD320:  .byte   $00,$E8,$FF
LD323:  .byte   $18,$9F,$A2,$AC,$97,$18,$A5,$18
        .byte   $9C,$18,$9E,$18
LD32F:  .byte   $01,$84,$01,$85,$83,$01,$86,$01
        .byte   $87,$01,$88,$01
LD33B:  .byte   $FE,$02
LD33D:  .byte   $03,$01,$03,$01,$02,$00,$03,$03
        .byte   $02,$03,$01,$03
LD349:  .byte   $15,$2B,$15,$15,$2A,$2C,$15,$15
        .byte   $2D,$15
        .byte   $2E
        .byte   $15
LD355:  lda     $32
        beq     LD36F
        dec     $32
        bne     LD36F
        jsr     LD37F
        ldy     $05C0
        cpy     #$04
        beq     LD36F
        lda     #$00
        sta     $05E0
        sta     $05A0
LD36F:  rts

LD370:  pha
        inc     $05C0
        lda     $A0
        cmp     #$0A
        bne     LD37D
        inc     $05C0
LD37D:  pla
        rts

LD37F:  pha
        dec     $05C0
        lda     $A0
        cmp     #$0A
        bne     LD38C
        dec     $05C0
LD38C:  pla
        rts

LD38E:  lda     $16
        and     #$03
        beq     LD396
        sta     $31
LD396:  ldy     #$04
        lda     $31
        and     #$01
        bne     LD39F
        iny
LD39F:  jsr     LE8D6
        lda     $10
        and     #$10
        beq     LD3A9
        rts

LD3A9:  lda     #$02
        sta     $30
        lda     #$14
        sta     $33
        lda     #$10
        jsr     LF835
        lda     $03C0
        clc
        adc     #$02
        sta     $03C0
        lda     #$80
        sta     $0400
        lda     #$02
        sta     $0420
        lda     #$80
        sta     $0304
        lda     $0580
        sta     $0584
        ldx     #$04
        lda     #$17
        jsr     LF835
        lda     $0360
        sta     $0364
        lda     $0380
        sta     $0384
        lda     $03C0
        sta     $03C4
        lda     $03E0
        sta     $03E4
        ldx     #$00
        stx     $0484
        stx     $0324
        beq     LD412
        ldy     #$02
        jsr     LF67C
        bcc     LD455
        lda     $16
        and     #$03
        beq     LD412
        cmp     $31
        beq     LD412
        sta     $31
        bne     LD43E
LD412:  lda     $31
        and     #$01
        beq     LD420
        ldy     #$02
        jsr     LF580
        jmp     LD425

LD420:  ldy     #$03
        jsr     LF5C4
LD425:  lda     $10
        and     #$10
        bne     LD43E
        lda     $33
        beq     LD43E
        dec     $33
        lda     $33
        cmp     #$0C
        bcs     LD43D
        lda     $14
        and     #$80
        bne     LD43E
LD43D:  rts

LD43E:  ldy     #$01
        jsr     LE8D6
        lda     $10
        and     #$10
        bne     LD470
        sta     $33
        sta     $30
        lda     $14
        and     #$80
        bne     LD471
        beq     LD45D
LD455:  lda     #$00
        sta     $33
        lda     #$01
        bne     LD45F
LD45D:  lda     #$04
LD45F:  jsr     LF835
        lda     #$4C
        sta     $0400
        lda     #$01
        sta     $0420
        lda     #$00
        sta     $30
LD470:  rts

LD471:  lda     #$4C
        sta     $0400
        lda     #$01
        sta     $0420
        jmp     LCEAD

LD47E:  lda     $03E0
        bne     LD4C7
        lda     $16
        and     #$08
        beq     LD4C8
        php
LD48A:  ldy     #$04
        jsr     LE9E3
        lda     $44
        cmp     #$20
        beq     LD4A3
        cmp     #$40
        beq     LD4A3
        lda     $43
        cmp     #$20
        beq     LD4A3
        cmp     #$40
        bne     LD4C6
LD4A3:  plp
        lda     $12
        and     #$F0
        ora     #$08
        sta     $0360
        lda     #$0A
LD4AF:  jsr     LF835
        lda     #$03
        sta     $30
        lda     #$4C
        sta     $0440
        lda     #$01
        sta     $0460
        lda     #$00
        sta     $32
        clc
        rts

LD4C6:  plp
LD4C7:  rts

LD4C8:  lda     $16
        and     #$04
        beq     LD4C7
        php
        lda     $43
        cmp     #$40
        bne     LD48A
        plp
        lda     $0360
        and     #$F0
        ora     #$08
        sta     $0360
        lda     $11
        and     #$F0
        sta     $03C0
        lda     #$14
        bne     LD4AF
        jsr     LD355
        lda     $14
        and     #$40
        beq     LD50B
        lda     $16
        and     #$03
        beq     LD508
        cmp     $31
        beq     LD508
        sta     $31
        lda     $0580
        eor     #$40
        sta     $0580
LD508:  jsr     LD0DA
LD50B:  lda     $32
        bne     LD4C7
        lda     $16
        and     #$0C
        bne     LD521
        lda     $14
        and     #$80
        bne     LD51E
        jmp     LD5B4

LD51E:  jmp     LD5AD

LD521:  pha
        lda     $0360
        and     #$F0
        ora     #$08
        sta     $0360
        pla
        and     #$04
        beq     LD568
        lda     #$0A
        cmp     $05C0
        beq     LD53B
        jsr     LF835
LD53B:  ldy     #$00
        jsr     LF606
        bcs     LD5AD
        lda     $03E0
        beq     LD54D
        lda     #$00
        sta     $03C0
        rts

LD54D:  ldy     #$04
        jsr     LE9E3
        lda     $44
        cmp     #$20
        beq     LD5B9
        cmp     #$40
        beq     LD5B9
        lda     $43
        cmp     #$20
        beq     LD5B9
        cmp     #$40
        beq     LD5B9
        bne     LD5AD
LD568:  ldy     #$01
        jsr     LF642
        bcs     LD5B4
        lda     $03E0
        beq     LD57A
        lda     #$EF
        sta     $03C0
        rts

LD57A:  lda     $03C0
        cmp     #$10
        bcc     LD5B9
        ldy     #$04
        jsr     LE9E3
        lda     $44
        cmp     #$20
        beq     LD5B9
        cmp     #$40
        beq     LD5B9
        lda     $43
        cmp     #$20
        beq     LD5B9
        cmp     #$40
        beq     LD5B9
        lda     #$14
        cmp     $05C0
        beq     LD5A4
        jsr     LF835
LD5A4:  lda     $03C0
        and     #$0F
        cmp     #$0C
        bcs     LD5B9
LD5AD:  lda     #$00
        sta     $30
        jmp     LF81B

LD5B4:  lda     #$00
        sta     $05E0
LD5B9:  rts

        lda     $05A0
        bne     LD5EB
        lda     $22
        cmp     #$07
        beq     LD5D2
        lda     $03C0
        cmp     #$68
        bcs     LD5DF
        jsr     LF797
        jmp     LD5E6

LD5D2:  lda     $03C0
        cmp     #$30
        bcs     LD5DF
        jsr     LF797
        jmp     LD5E6

LD5DF:  ldy     #$00
        jsr     LF67C
        bcs     LD5EB
LD5E6:  lda     #$00
        sta     $05E0
LD5EB:  lda     $05E0
        bne     LD5FC
        lda     $05A0
        cmp     #$01
        bne     LD5FC
        lda     #$34
        jsr     LF89A
LD5FC:  lda     $05A0
        cmp     #$04
        bne     LD60B
        lda     #$00
        sta     $30
        sta     $32
        sta     $39
LD60B:  rts

LD60C:  lda     #$00
        sta     $30
        jmp     LF81B

        ldy     $34
        lda     $0300,y
        bpl     LD60C
        lda     $05C0,y
        cmp     #$62
        bne     LD60C
        lda     $16
        and     #$0C
        beq     LD656
        sta     L0000
        lda     $0440
        pha
        lda     $0460
        pha
        lda     #$40
        sta     $0440
        lda     #$00
        sta     $0460
        lda     L0000
        and     #$08
        beq     LD649
        ldy     #$01
        jsr     LF642
        jmp     LD64E

LD649:  ldy     #$00
        jsr     LF606
LD64E:  pla
        sta     $0460
        pla
        sta     $0440
LD656:  lda     $0440
        ora     $0460
        bne     LD67E
        lda     $14
        and     #$80
        beq     LD667
        jmp     LCEAD

LD667:  lda     #$00
        sta     $0400
        lda     #$01
        sta     $0420
        lda     $0580
        pha
        lda     $35
        jsr     LCF3D
        pla
        sta     $0580
LD67E:  ldy     #$01
        jsr     LF642
        lda     $0440
        clc
        adc     #$40
        sta     $0440
        lda     $0460
        adc     #$00
        sta     $0460
        cmp     #$07
        bcc     LD69D
        lda     #$00
        sta     $0440
LD69D:  lda     #$4C
        sta     $0400
        lda     #$01
        sta     $0420
        jsr     LD0C1
        rts

        lda     $05C0
        cmp     #$11
        beq     LD701
        cmp     #$B1
        beq     LD701
        lda     $05C0
        sta     $3E
        cmp     #$D9
        bcs     LD6C3
        lda     #$11
        bne     LD6C5
LD6C3:  lda     #$B1
LD6C5:  jsr     LF835
        lda     #$00
        sta     $32
        jsr     LF81B
        lda     #$80
        sta     $0304
        lda     $0580
        sta     $0584
        ldx     #$04
        lda     #$12
        jsr     LF835
        lda     $0360
        sta     $0364
        lda     $0380
        sta     $0384
        lda     $03C0
        sta     $03C4
        lda     $03E0
        sta     $03E4
        ldx     #$00
        stx     $0484
        stx     $0324
LD701:  lda     $3E
        cmp     #$10
        beq     LD742
        cmp     #$79
        bcs     LD742
        ldy     #$00
        jsr     LF67C
        lda     #$80
        sta     $0400
        lda     #$00
        sta     $0420
        lda     $0580
        pha
        lda     $0580
        and     #$40
        bne     LD72D
        ldy     #$00
        jsr     LF580
        jmp     LD732

LD72D:  ldy     #$01
        jsr     LF5C4
LD732:  lda     $0360
        sta     $0364
        lda     $0380
        sta     $0384
        pla
        sta     $0580
LD742:  lda     $05A0
        cmp     #$09
        bne     LD778
        lda     $3E
        pha
        cmp     #$10
        beq     LD754
        cmp     #$D9
        bcc     LD757
LD754:  jsr     LF835
LD757:  pla
        cmp     #$10
        beq     LD764
        cmp     #$D9
        bcc     LD768
        lda     #$08
        bne     LD76A
LD764:  lda     #$02
        bne     LD76A
LD768:  lda     #$00
LD76A:  sta     $30
        lda     #$3C
        sta     $39
        lda     $05E0
        ora     #$80
        sta     $05E0
LD778:  rts

        lda     #$00
        sta     $3D
        lda     $05C0
        cmp     #$7A
        beq     LD7E7
        lda     $03E0
        bne     LD7EA
        ldy     #$0F
LD78B:  lda     #$7A
        sta     $05C0,y
        lda     #$00
        sta     $05E0,y
        sta     $05A0,y
        sta     $0480,y
        lda     $0360
        sta     $0360,y
        lda     $0380
        sta     $0380,y
        lda     $03C0
        sta     $03C0,y
        lda     $03E0
        sta     $03E0,y
        lda     #$10
        sta     $0320,y
        lda     #$80
        sta     $0300,y
        lda     #$90
        sta     $0580,y
        lda     LD7F1,y
        sta     $0400,y
        lda     LD801,y
        sta     $0420,y
        lda     LD811,y
        sta     $0440,y
        lda     LD821,y
        sta     $0460,y
        dey
        bpl     LD78B
        lda     #$00
        sta     $0440
        lda     #$03
        sta     $0460
LD7E7:  jsr     LF779
LD7EA:  inc     $3F
        bne     LD7F0
        inc     $3C
LD7F0:  .byte   $60
LD7F1:  .byte   $00,$1F,$00,$1F,$00,$E1,$00,$E1
        .byte   $00,$0F,$80,$0F,$00,$F1,$80,$F1
LD801:  .byte   $00,$02,$03,$02,$00,$FD,$FD,$FD
        .byte   $00,$01,$01,$01,$00,$FE,$FE,$FE
LD811:  .byte   $00,$E1,$00,$1F,$00,$1F,$00,$E1
        .byte   $80,$F1,$00,$0F,$80,$0F,$00,$F1
LD821:  .byte   $FD,$FD,$00,$02,$03,$02,$00,$FD
        .byte   $FE,$FE,$00,$01,$01,$01
        brk
        inc     a:$A9,x
        sta     $05E0
        dec     $0500
        bne     LD857
        lda     #$1E
        sta     $0500
        ldy     #$68
LD842:  lda     $0201,y
        clc
        adc     #$01
        cmp     #$FA
        bne     LD84E
        lda     #$F7
LD84E:  sta     $0201,y
        dey
        dey
        dey
        dey
        bpl     LD842
LD857:  rts

        jsr     LDED8
        lda     $05C0
        cmp     #$DA
        bne     LD86E
        lda     $05A0
        cmp     #$03
        bne     LD857
        lda     #$DB
        jsr     LF835
LD86E:  ldy     #$06
        jsr     LE8D6
        lda     $10
        cmp     #$80
        beq     LD886
        lda     #$DB
        cmp     $05C0
        beq     LD893
        jsr     LF835
        jmp     LD893

LD886:  lda     #$DC
        cmp     $05C0
        beq     LD8BD
        jsr     LF835
        jmp     LD8BD

LD893:  ldy     #$00
        jsr     LF67C
        bcc     LD8AF
        lda     $14
        and     #$80
        beq     LD910
LD8A0:  lda     #$E5
        sta     $0440
        lda     #$04
        sta     $0460
        ldy     #$00
        jsr     LF67C
LD8AF:  lda     $16
        and     #$03
        beq     LD910
        sta     $31
        jsr     LCF3D
        jmp     LD910

LD8BD:  lda     $14
        and     #$80
        beq     LD8CE
        ldy     #$01
        jsr     LE8D6
        lda     $10
        cmp     #$80
        bne     LD8A0
LD8CE:  lda     $16
        and     #$03
        beq     LD8D9
        sta     $31
        jsr     LCF3D
LD8D9:  lda     #$80
        sta     $0440
        lda     #$01
        sta     $0460
        lda     $16
        and     #$0C
        beq     LD910
        and     #$04
        bne     LD90B
        ldy     #$01
        jsr     LF642
        ldy     #$06
        jsr     LE8D6
        lda     $10
        cmp     #$80
        beq     LD908
        lda     $03C0
        and     #$F0
        clc
        adc     #$10
        sta     $03C0
LD908:  jmp     LD910

LD90B:  ldy     #$00
        jmp     LF606

LD910:  lda     $05C0
        pha
        jsr     LD355
        lda     $14
        and     #$40
        beq     LD920
        jsr     LD0DA
LD920:  lda     #$00
        sta     $32
        pla
        sta     $05C0
        rts

        ldy     #$00
        jsr     LF67C
        bcc     LD990
        lda     #$01
        cmp     $05C0
        beq     LD942
        sta     $05C0
        lda     #$00
        sta     $05E0
        sta     $05A0
LD942:  lda     $22
        cmp     #$10
        bne     LD973
        ldy     #$26
        cpy     $52
        beq     LD95B
        sty     $52
        ldy     #$00
        sty     LA000
        sty     $70
        sty     $28
        beq     LD95F
LD95B:  ldy     $70
        beq     LD973
LD95F:  sta     $F5
        jsr     LFF6B
        lda     #$1A
        jsr     LE8B4
        lda     #$04
        sta     $10
        jsr     LEF8C
        ldx     #$00
        rts

LD973:  lda     $B0
        cmp     #$9C
        beq     LD98C
        lda     $95
        and     #$03
        bne     LD990
        inc     $B0
        lda     $B0
        cmp     #$81
        beq     LD990
        lda     #$1C
        jmp     LF89A

LD98C:  lda     #$00
        sta     $30
LD990:  rts

        ldy     #$00
        jsr     LF67C
        bcs     LD9B6
        lda     $0580
        pha
        and     #$40
        bne     LD9A8
        ldy     #$00
        jsr     LF580
        jmp     LD9AD

LD9A8:  ldy     #$01
        jsr     LF5C4
LD9AD:  pla
        sta     $0580
        dec     $0500
        bne     LD9BD
LD9B6:  lda     #$00
        sta     $0500
        sta     $30
LD9BD:  rts

        dec     $0520
        bne     LD9D2
        ldy     $0500
        sty     $30
        lda     LD297,y
        jsr     LF835
        lda     #$00
        sta     $32
LD9D2:  rts

        lda     $0500
        and     #$0F
        beq     LDA31
        lda     $0460
        bpl     LDA31
        lda     #$68
        cmp     $03C0
        beq     LD9EB
        bcs     LDA31
        sta     $03C0
LD9EB:  ldy     #$0F
LD9ED:  lda     $0310,y
        bmi     LDA02
        dey
        bpl     LD9ED
        lda     $0500
        cmp     #$04
        beq     LDA03
        jsr     LDB3A
        inc     $0500
LDA02:  rts

LDA03:  lda     #$31
        jsr     LF89A
        ldy     $22
        lda     LDD04,y
        sta     $A1
        lda     LDD0C,y
        sta     $B4
        clc
        adc     $A1
        tay
        lda     #$80
        sta     $A2,y
        lda     #$02
        sta     $F5
        jsr     LFF6B
        jsr     LA000
        lda     #$0D
        sta     $30
        lda     #$80
        sta     $0300
        rts

LDA31:  lda     $22
        cmp     #$10
        bcc     LDA55
        lda     $95
        and     #$07
        bne     LDA55
        lda     $0520
        beq     LDA49
        lda     $0610
        cmp     #$0F
        beq     LDA55
LDA49:  lda     $0610
        eor     #$2F
        sta     $0610
        lda     #$FF
        sta     $18
LDA55:  ldy     #$00
        jsr     LF67C
        bcc     LDA6C
        lda     $0520
        beq     LDA6C
        lda     #$01
        cmp     $05C0
        beq     LDA6C
        jsr     LF835
        sec
LDA6C:  rol     $0F
        ldy     #$0F
LDA70:  lda     $22
        cmp     #$11
        bne     LDA7A
        cpy     #$00
        beq     LDA82
LDA7A:  lda     $0310,y
        bmi     LDA02
        dey
        bpl     LDA70
LDA82:  lda     $22
        cmp     #$08
        bcc     LDA92
        cmp     #$0C
        bcs     LDA92
        lda     $F9
        cmp     #$18
        bcc     LDAC8
LDA92:  lda     $22
        cmp     #$11
        bne     LDAA6
        lda     #$37
        cmp     $D9
        beq     LDAB4
        lda     #$00
        sta     $FD
        lda     #$37
        bne     LDAAC
LDAA6:  lda     #$38
        cmp     $D9
        beq     LDAB4
LDAAC:  jsr     LF898
        lda     #$FF
        sta     $0520
LDAB4:  lda     $0520
        beq     LDAC8
        dec     $0520
        bne     LDB2B
        lda     $22
        cmp     #$10
        bcc     LDAC8
        lda     #$00
        sta     $FD
LDAC8:  lda     $22
        cmp     #$08
        bcc     LDAD1
        jmp     LDB89

LDAD1:  sty     L0000
        lda     $0360
        cmp     #$80
        beq     LDB00
        bcs     LDAEF
        ldy     #$00
        jsr     LF580
        rol     L0000
        lda     #$80
        cmp     $0360
        bcs     LDB00
        sta     $0360
        bcc     LDB00
LDAEF:  ldy     #$01
        jsr     LF5C4
        rol     L0000
        lda     #$80
        cmp     $0360
        bcc     LDB00
        sta     $0360
LDB00:  ldy     #$00
        lsr     $0F
        bcs     LDB07
        iny
LDB07:  lda     LDC7F,y
        cmp     $05C0
        beq     LDB12
        jsr     LF835
LDB12:  cpy     #$01
        beq     LDB88
        lsr     L0000
        bcc     LDB88
        lda     $0360
        cmp     #$80
        beq     LDB2C
        lda     #$E5
        sta     $0440
        lda     #$04
        sta     $0460
LDB2B:  rts

LDB2C:  lda     #$00
        sta     $0440
        lda     #$08
        sta     $0460
        inc     $0500
        rts

LDB3A:  ldy     $0500
        ldx     LDD00,y
        ldy     #$1F
LDB42:  lda     #$5B
        jsr     LF846
        lda     #$00
        sta     $0480,y
        lda     #$11
        sta     $0320,y
        lda     LDCE1,y
        sta     $0360,y
        lda     $0380
        sta     $0380,y
        lda     LDCD1,y
        sta     $03C0,y
        lda     LDC71,x
        sta     $0400,y
        lda     LDC89,x
        sta     $0420,y
        lda     LDCA1,x
        sta     $0440,y
        lda     LDCB9,x
        sta     $0460,y
        dex
        dey
        cpy     #$0F
        bne     LDB42
        lda     #$32
        jsr     LF89A
        ldx     #$00
LDB88:  rts

LDB89:  lda     $22
        cmp     #$10
        bcs     LDBB5
        cmp     #$0C
        bcs     LDBA6
        lda     $F9
        cmp     #$18
        bcs     LDBA6
        lda     #$00
        sta     $30
        ldy     $22
        lda     LCD0C,y
        jsr     LF898
        rts

LDBA6:  lda     #$81
        sta     $0300
        lda     #$00
        sta     $0500
        lda     #$0D
        sta     $30
        rts

LDBB5:  lda     #$0F
        sta     $0610
        sta     $18
        lda     #$00
        sta     $F8
        sta     $6A
        sta     $6B
        sta     $FD
        lda     $22
        clc
        adc     #$04
        sta     $30
        lda     #$80
        sta     $0300
        lda     #$00
        sta     $0500
        sta     $0520
        sta     $0540
        sta     $0560
        rts

        lda     $0300
        and     #$0F
        bne     LDC0F
        ldy     #$00
        jsr     LF67C
        bcs     LDBFB
        lda     $05A0
        cmp     #$04
        bne     LDC0E
        lda     #$07
        jmp     LF835

LDBFB:  inc     $0300
        sta     $0440
        sta     $0460
        lda     #$3C
        sta     $0500
        lda     #$01
        jsr     LF835
LDC0E:  rts

LDC0F:  lda     $0500
        beq     LDC18
        dec     $0500
        rts

LDC18:  lda     #$13
        cmp     $05C0
        beq     LDC2C
        jsr     LF835
        lda     #$34
        jsr     LF89A
        lda     #$04
        sta     $05A0
LDC2C:  lda     $05A0
        cmp     #$02
        bne     LDC73
        lda     #$00
        sta     $05E0
        lda     $0440
        clc
        adc     $99
        sta     $0440
        lda     $0460
        adc     #$00
        sta     $0460
        jsr     LF779
        lda     $03E0
        beq     LDC73
        ldy     $22
        cpy     #$0C
        bcs     LDC74
        lda     $61
        ora     LDEC2,y
        sta     $61
        inc     $59
        lda     $22
        cmp     #$00
        beq     LDC6F
        cmp     #$07
        bne     LDC73
        lda     #$9C
        sta     $AB
        rts

LDC6F:  lda     #$9C
LDC71:  sta     $AD
LDC73:  rts

LDC74:  lda     #$FF
        sta     $74
        inc     $75
        lda     #$9C
        .byte   $85,$A2,$60
LDC7F:  .byte   $04,$07,$00,$C2,$00,$C2,$00,$3E
        .byte   $00,$3E
LDC89:  .byte   $00,$E1,$00,$E1,$00,$1F,$00,$1F
        .byte   $00,$F1,$80,$F1,$00,$0F,$80,$0F
        .byte   $00,$FB,$FA,$FB,$00,$04,$06,$04
LDCA1:  .byte   $00,$FD,$FD,$FD,$00,$02,$03,$02
        .byte   $00,$FE,$FE,$FE,$00,$01,$01,$01
        .byte   $00,$3E,$00,$C2,$00,$C2,$00,$3E
LDCB9:  .byte   $00,$1F,$00,$E1,$00,$E1,$00,$1F
        .byte   $80,$0F,$00,$F1,$80,$F1,$00,$0F
        .byte   $06,$04,$00,$FB,$FA,$FB,$00,$04
LDCD1:  .byte   $03,$02,$00,$FD,$FD,$FD,$00,$02
        .byte   $01,$01,$00,$FE,$FE,$FE,$00,$01
LDCE1:  .byte   $00,$23,$78,$C0,$F0,$CC,$78,$23
        .byte   $00,$23,$78,$C0,$F0,$CC,$78,$23
        .byte   $80,$D4,$F8,$D4,$80,$2B,$08,$2B
        .byte   $80,$D4,$F8,$D4,$80,$2B,$08
LDD00:  .byte   $2B,$1F,$1F,$27
LDD04:  .byte   $02,$04,$01,$03,$05,$00,$02,$04
LDD0C:  .byte   $00,$00,$00,$00,$00
        asl     $06
        asl     $A0
        brk
        jsr     LF67C
        bcc     LDD25
        lda     #$01
        cmp     $05C0
        beq     LDD25
        jsr     LF835
LDD25:  ldy     #$26
        cpy     $52
        beq     LDD38
        sty     $52
        ldy     #$00
        sty     LA000
        sty     $70
        sty     $28
        beq     LDD3C
LDD38:  ldy     $70
        beq     LDD52
LDD3C:  lda     #$11
        sta     $F5
        jsr     LFF6B
        lda     #$04
        jsr     LE8B4
        lda     #$04
        sta     $10
        jsr     LEF8C
        ldx     #$00
        rts

LDD52:  ldy     #$0F
LDD54:  lda     $0308,y
        bmi     LDDA5
        dey
        bpl     LDD54
        lda     #$0B
        cmp     $F8
        beq     LDD72
        sta     $F8
        lda     $FD
        ora     #$01
        sta     $FD
        lda     #$50
        sta     $FA
        lda     #$52
        sta     $5E
LDD72:  lda     $FA
        sec
        sbc     #$03
        sta     $FA
        bcs     LDD81
        lda     #$00
        sta     $FA
        sta     $30
LDD81:  ldy     #$03
LDD83:  lda     LDDA6,y
        sec
        sbc     $FA
        bcc     LDD96
        sta     $03DC,y
        lda     $059C,y
        and     #$FB
        sta     $059C,y
LDD96:  dey
        bpl     LDD83
        lda     $30
        beq     LDDA5
        lda     $059E
        ora     #$04
        sta     $059E
LDDA5:  .byte   $60
LDDA6:  .byte   $48
        .byte   $3F
        .byte   $3F
        .byte   $3F
        lda     $05A0
        cmp     #$04
        bne     LDDA5
        ldy     $6C
        lda     LDE5E,y
        bpl     LDDC1
        sta     $74
        inc     $75
        lda     #$9C
        sta     $A2
        rts

LDDC1:  lda     #$00
        sta     $EE
        jsr     LC752
        lda     #$04
        sta     $97
        jsr     LC5E9
        jsr     LFF21
        jsr     LC628
        lda     #$01
        sta     $F5
        jsr     LFF6B
        ldy     $6C
        bne     LDDE2
        sty     $6E
LDDE2:  lda     LDE5E,y
        sta     $F9
        sta     $0380
        sta     $2B
        sta     $29
        lda     #$20
        sta     $2A
        lda     LDE72,y
        sta     $0360
        lda     LDE86,y
        sta     $03C0
        lda     LDE9A,y
        sta     $9E
        sta     $9F
        lda     LDEAE,y
        jsr     LA000
        jsr     LFF3C
        lda     #$01
        sta     $31
        sta     $23
        sta     $2E
        dec     $29
        lda     #$1F
        sta     $24
        lda     #$21
LDE1E:  pha
        lda     #$01
        sta     $10
        jsr     LE4F1
        jsr     LFF21
        pla
        sec
        sbc     #$01
        bne     LDE1E
        sta     $2C
        sta     $2D
        sta     $5A
        lda     $F9
        cmp     #$08
        bne     LDE40
        lda     #$0A
        jsr     LF898
LDE40:  jsr     LFF21
        jsr     LC74C
        ldx     #$00
        lda     #$13
        jsr     LF835
        lda     #$12
        sta     $30
        rts

        lda     $05A0
        cmp     #$04
        bne     LDE5D
        lda     #$00
        .byte   $85,$30
LDE5D:  .byte   $60
LDE5E:  .byte   $07,$00,$FF,$0A,$0C,$0E,$10,$12
        .byte   $14,$16,$18,$00,$08,$08,$08,$08
        .byte   $08,$08,$08,$08
LDE72:  .byte   $30,$00,$00,$20,$20,$20,$20,$20
        .byte   $20,$20,$20,$00,$30,$30,$30,$70
        .byte   $90,$D0,$D0,$D0
LDE86:  .byte   $B0,$00,$00,$C0,$B0,$B0,$B0,$B0
        .byte   $B0,$A0,$B0,$00,$2C,$6C,$AC,$AC
        .byte   $AC,$2C,$6C,$AC
LDE9A:  .byte   $10,$10,$00,$1B,$1C,$1D,$1E,$1F
        .byte   $20,$21,$22,$00,$11,$11,$11,$11
        .byte   $11,$11,$11,$11
LDEAE:  .byte   $02,$02,$00,$25,$23,$27,$24,$2A
        .byte   $26,$28,$29,$00,$02,$02,$02,$02
        .byte   $02
LDEBF:  .byte   $02,$02,$02
LDEC2:  .byte   $01,$02,$04,$08,$10,$20,$40,$80
        .byte   $01
        .byte   $04
        rti

        .byte   $80
LDECE:  lda     $A0
        cmp     #$06
        bcc     LDED8
        and     #$01
        bne     LDF1A
LDED8:  ldy     $A0
        inc     $B5
        lda     $B5
        cmp     LDF27,y
        bne     LDF1A
        lda     #$00
        sta     $B5
        lda     $A2,y
        and     #$1F
        sec
        sbc     LDF1B,y
        bcs     LDEF4
        lda     #$00
LDEF4:  ora     #$80
        sta     $A2,y
        cmp     #$80
        bne     LDF1A
        cpy     #$0B
        beq     LDF15
        cpy     #$09
        bne     LDF1A
        lda     #$02
        sta     $EB
        jsr     LFF3C
        lda     #$01
        jsr     LF835
        lda     #$00
        sta     $30
LDF15:  lda     #$00
        .byte   $8D,$01,$03
LDF1A:  .byte   $60
LDF1B:  .byte   $00,$02,$01,$02,$02,$00,$01,$03
        .byte   $01,$01,$01,$01
LDF27:  .byte   $00,$01,$04,$01,$01,$00,$02
        ora     ($01,x)
        asl     $1E02,x
        lda     $05C0
        cmp     #$13
        beq     LDF51
        ldy     #$00
        jsr     LF67C
        bcc     LDF89
        lda     #$13
        jsr     LF835
        inc     $05A0
        lda     #$00
        sta     $0440
        sta     $0460
LDF51:  lda     $05A0
        bne     LDF89
        sta     $05E0
        jsr     LF779
        lda     $03E0
        bne     LDF73
        lda     $0440
        clc
        adc     $99
        sta     $0440
        lda     $0460
        adc     #$00
        sta     $0460
        rts

LDF73:  lda     #$00
        sta     $30
        lda     #$80
        sta     $74
        lda     #$FF
        sta     $60
        ldy     #$0B
        lda     #$9C
LDF83:  sta     $A2,y
        dey
        bpl     LDF83
LDF89:  rts

        ldy     #$00
        jsr     LF67C
        php
        ror     $0F
        plp
        bcs     LDFA2
        lda     #$07
        cmp     $05C0
        beq     LDFAC
        jsr     LF835
        jmp     LDFAC

LDFA2:  lda     #$04
        cmp     $05C0
        beq     LDFAC
        jsr     LF835
LDFAC:  lda     $0300
        and     #$0F
        bne     LE015
        lda     $0360
        cmp     #$50
        beq     LE015
        bcs     LDFC2
        inc     $0360
        jmp     LDFC5

LDFC2:  dec     $0360
LDFC5:  lda     $0360
        cmp     #$50
        bne     LE022
        lda     $031F
        bmi     LE015
        lda     #$80
        sta     $031F
        lda     #$90
        sta     $059F
        lda     #$6D
        sta     $05DF
        lda     #$00
        sta     $05FF
        sta     $05BF
        sta     $03FF
        sta     $03DF
        sta     $0520
        lda     $F9
        sta     $039F
        lda     #$C0
        sta     $037F
        lda     #$EE
        sta     $033F
LE000:  .byte   $A9
LE001:  .byte   $5A
        sta     $0500
        lda     $0580
        ora     #$40
        sta     $0580
        lda     #$78
        sta     $0500
        inc     $0300
LE015:  lda     $0500
        beq     LE023
        dec     $0500
        lda     #$01
        jsr     LF835
LE022:  rts

LE023:  lda     $0300
        and     #$0F
        cmp     #$02
        beq     LE068
        lda     $0360
        cmp     #$A0
        beq     LE05A
        lda     #$04
        cmp     $05C0
        beq     LE03D
        jsr     LF835
LE03D:  inc     $0360
        lda     $0360
        cmp     #$A0
        bne     LE08B
        lda     #$6E
        sta     $05DF
        lda     #$00
        sta     $05FF
        sta     $05BF
        lda     #$3C
        sta     $0500
        rts

LE05A:  lda     #$E5
        sta     $0440
        lda     #$04
        sta     $0460
        inc     $0300
        rts

LE068:  lda     $0F
        bpl     LE079
        lda     $0520
        bne     LE07D
        inc     $0520
        lda     #$78
        sta     $0500
LE079:  dec     $0360
        rts

LE07D:  lda     #$81
        sta     $0300
        lda     #$00
        sta     $0500
        lda     #$0D
        sta     $30
LE08B:  rts

        ldy     #$00
        jsr     LF67C
        bcs     LE097
        lda     #$07
        bne     LE099
LE097:  lda     #$04
LE099:  cmp     $05C0
        beq     LE0A1
        jsr     LF835
LE0A1:  lda     $0360
        cmp     #$68
        beq     LE0B2
        bcs     LE0AE
        inc     $0360
        rts

LE0AE:  dec     $0360
        rts

LE0B2:  lda     #$01
        cmp     $05C0
        beq     LE0BC
        jsr     LF835
LE0BC:  inc     $0500
        bne     LE119
        lda     #$C0
        sta     $0500
        lda     $0520
        cmp     #$04
        beq     LE119
        jsr     LFC53
        bcs     LE119
        lda     #$80
        sta     $0300,y
        lda     #$90
        sta     $0580,y
        lda     #$00
        sta     $03C0,y
        sta     $03E0,y
        sta     $05E0,y
        sta     $05A0,y
        sta     $0480,y
        sta     $0440,y
        sta     $0460,y
        lda     #$7C
        sta     $05C0,y
        lda     #$F9
        sta     $0320,y
        lda     #$C4
        sta     $0520,y
        lda     $0520
        sta     $0500,y
        tax
        inc     $0520
        lda     LE166,x
        sta     $0360,y
        lda     $F9
        sta     $0380,y
        ldx     #$00
LE119:  rts

        lda     #$80
        sta     $030E
        lda     #$90
        sta     $058E
        lda     #$EB
        sta     $032E
        lda     #$67
        sta     $05CE
        lda     #$00
        sta     $05EE
        sta     $05AE
        sta     $03EE
        sta     $B3
        lda     $F9
        sta     $038E
        ldy     $6C
        lda     LDE72,y
        sta     $036E
        lda     LDE86,y
        and     #$F0
        sta     $03CE
        lda     $6C
        clc
        adc     #$18
        sta     $04CE
        lda     LDEBF,y
        ora     $6E
        sta     $6E
        lda     #$0C
        sta     $EC
        jmp     LFF3C

LE166:  .byte   $E0,$30,$B0,$68
LE16A:  lda     $2A
        and     #$20
        bne     LE173
        jmp     LE228

LE173:  lda     #$01
        sta     $10
        sta     $2E
        lda     $0360
        sec
        sbc     $FC
        sta     L0000
        sec
        sbc     #$80
        bcs     LE18A
        eor     #$FF
        adc     #$01
LE18A:  sta     $01
        lda     $0360
        sec
        sbc     $27
        sta     $02
        bpl     LE1E1
        eor     #$FF
        clc
        adc     #$01
        sta     $02
        lda     $01
        cmp     #$09
        bcc     LE1A7
        lda     $02
        sta     $01
LE1A7:  lda     #$08
        cmp     $01
        bcs     LE1AF
        sta     $01
LE1AF:  lda     #$02
        sta     $10
        sta     $2E
        lda     L0000
        cmp     #$80
        bcs     LE228
        lda     $FC
        sec
        sbc     $01
        sta     $FC
        bcs     LE224
        lda     $2D
        dec     $2D
        bpl     LE1DC
        sta     $2D
        lda     #$00
        sta     $FC
        lda     #$10
        cmp     $0360
        bcc     LE228
        sta     $0360
        bcs     LE228
LE1DC:  dec     $F9
        jmp     LE224

LE1E1:  lda     L0000
        cmp     #$81
        bcc     LE228
        lda     $2D
        cmp     $2C
        beq     LE228
        lda     $01
        cmp     #$09
        bcc     LE1F7
        lda     $02
        sta     $01
LE1F7:  lda     #$08
        cmp     $01
        bcs     LE1FF
        sta     $01
LE1FF:  lda     $01
        beq     LE228
        lda     $FC
        clc
        adc     $01
        sta     $FC
        bcc     LE224
        inc     $2D
        lda     $2D
        cmp     $2C
        bne     LE222
        lda     #$00
        sta     $FC
        lda     #$F0
        cmp     $0360
        bcs     LE222
        sta     $0360
LE222:  inc     $F9
LE224:  jmp     LE467

LE227:  rts

LE228:  lda     $FC
        bne     LE227
        lda     $0360
        cmp     #$10
        bcs     LE23D
        lda     #$10
        sta     $0360
        beq     LE23D
LE23A:  jmp     LE33C

LE23D:  cmp     #$E5
        bcc     LE23A
        cmp     #$F0
        bcc     LE24A
        lda     #$F0
        sta     $0360
LE24A:  ldy     $2B
        lda     $AA40,y
        and     #$20
        beq     LE23A
        lda     $AA41,y
        and     #$C0
        bne     LE23A
        lda     $AA41,y
        and     #$20
        beq     LE23A
        sta     L0000
        lda     $22
        cmp     #$08
        bne     LE27A
        lda     $F9
        cmp     #$15
        beq     LE273
        cmp     #$1A
        bne     LE27A
LE273:  lda     $033F
        cmp     #$FC
        beq     LE23A
LE27A:  lda     $F9
        sec
        sbc     $AA30
        cmp     #$02
        bne     LE28F
        lda     $031F
        bmi     LE23A
        lda     $30
        cmp     #$0C
        bcs     LE23A
LE28F:  lda     $22
        cmp     #$0F
        bne     LE2A5
        lda     $F9
        cmp     #$08
        bne     LE2A5
        ldx     #$0F
LE29D:  lda     $0310,x
        bmi     LE23A
        dex
        bpl     LE29D
LE2A5:  lda     L0000
        sta     $2A
        lda     $AA41,y
        and     #$1F
        sta     $2C
        lda     #$00
        sta     $2D
        inc     $2B
        ldy     $F8
        lda     #$00
        sta     $F8
        sta     $76
        sta     $B3
        sta     $5A
        lda     #$E8
        sta     $5E
        cpy     #$02
        bne     LE2D5
        lda     #$42
        sta     $E9
        lda     #$09
        sta     $29
        jsr     LC83D
LE2D5:  lda     $F9
        sec
        sbc     $AA30
        bcc     LE2F7
        cmp     #$03
        bcs     LE2E8
        tax
        ldy     $AA31,x
        jmp     LE2F4

LE2E8:  lda     $F9
        sec
        sbc     $AA38
        bcc     LE2F7
        tax
        ldy     $AA39,x
LE2F4:  jsr     LEE31
LE2F7:  lda     $22
        bne     LE308
        ldx     #$03
LE2FD:  lda     $AAA2,x
        sta     $060C,x
        dex
        bpl     LE2FD
        stx     $18
LE308:  lda     #$E4
        sta     $0360
        jsr     LE5D1
        lda     $22
        sta     $F5
        jsr     LFF6B
        lda     $F9
        sec
        sbc     $AA30
        bcc     LE33B
        cmp     #$05
        bcs     LE32A
        tax
        ldy     $AA30,x
        jmp     LE338

LE32A:  lda     $F9
        sec
        sbc     $AA38
        bcc     LE33B
        beq     LE33B
        tax
        ldy     $AA38,x
LE338:  jsr     LEE44
LE33B:  rts

LE33C:  lda     $03C0
        cmp     #$E8
        bcs     LE35E
        cmp     #$09
        bcs     LE33B
        lda     $30
        cmp     #$03
        bne     LE33B
        lda     #$80
        sta     $10
        jsr     LE3C7
        bcc     LE33B
        lda     #$80
        sta     $10
        lda     #$08
        bne     LE372
LE35E:  lda     $03E0
        bmi     LE3C6
        lda     #$40
        sta     $10
        jsr     LE3C7
        bcc     LE3C6
        lda     #$40
        sta     $10
        lda     #$04
LE372:  sta     $23
        lda     #$00
        sta     $03E0
        ldx     #$01
        ldy     $2B
        lda     $AA40,y
        and     #$C0
        cmp     $10
        beq     LE388
        ldx     #$FF
LE388:  stx     $12
        lda     #$00
        sta     $03A0
        sta     $F8
        lda     #$E8
        sta     $5E
        jsr     LEB6D
        jsr     LE614
        lda     $22
        sta     $F5
        jsr     LFF6B
        ldy     $2B
        lda     $AA40,y
        and     #$20
        beq     LE3AD
        sta     $2A
LE3AD:  lda     #$01
        sta     LA000
        lda     #$2A
        sta     $52
        jsr     LC83D
        ldx     #$00
        ldy     #$04
        jsr     LE9E3
        lda     $10
        bne     LE3C6
        sta     $30
LE3C6:  rts

LE3C7:  lda     $2D
        beq     LE3D1
        cmp     $2C
        beq     LE3D1
        clc
        rts

LE3D1:  lda     $F9
        sta     L0000
        ldy     $2B
        lda     $2A
        and     #$C0
        beq     LE3E5
        lda     $2A
        cmp     $10
        beq     LE403
        bne     LE411
LE3E5:  lda     $2C
        bne     LE3FF
        lda     $AA41,y
        and     #$C0
        cmp     $10
        beq     LE403
        lda     $AA3F,y
        and     #$C0
        eor     #$C0
        cmp     $10
        beq     LE411
        bne     LE465
LE3FF:  lda     $2D
        beq     LE411
LE403:  iny
        lda     $AA40,y
        and     #$C0
        cmp     $10
        bne     LE465
        inc     L0000
        bne     LE430
LE411:  lda     $AA40,y
        and     #$C0
        beq     LE465
        dey
        bmi     LE465
        lda     $AA40,y
        and     #$C0
        bne     LE425
        lda     $AA41,y
LE425:  eor     #$C0
        cmp     $10
        beq     LE425
        dec     L0000
        lda     $AA40,y
LE430:  sta     $2A
        lda     #$01
        sta     $2E
        cpy     $2B
        sty     $2B
        bcs     LE440
        lda     #$02
        sta     $2E
LE440:  lda     $AA40,y
        and     #$1F
        sta     $2C
        ldx     L0000
        cpx     $F9
        bcc     LE44F
        lda     #$00
LE44F:  sta     $2D
        lda     L0000
        sta     $29
        sta     $F9
        sta     $0380
        lda     #$00
        sta     LA000
        lda     #$26
        sta     $52
        sec
        rts

LE465:  clc
        rts

LE467:  lda     $F8
        cmp     #$02
        bne     LE4AF
        jsr     LE4AF
        bcs     LE4EF
        lda     $0780
        ora     #$22
        sta     $0780
        lda     $0781
        ora     #$80
        sta     $0781
        lda     #$09
        sta     $0782
        ldy     #$00
LE489:  lda     $0797,y
        sta     $0783,y
        iny
        cpy     #$0A
        bne     LE489
        lda     $07A1
        bpl     LE49F
        sta     $078D
        sta     $1A
        rts

LE49F:  ldy     #$00
LE4A1:  lda     $07B5,y
        sta     $078D,y
        iny
        cpy     #$0D
        bne     LE4A1
        sta     $1A
        rts

LE4AF:  lda     $FC
        sec
        sbc     $25
        bpl     LE4BB
        eor     #$FF
        clc
        adc     #$01
LE4BB:  sta     $03
        beq     LE4EF
        lda     $23
        and     #$0C
        beq     LE4D6
        lda     $10
        sta     $23
        and     #$01
        tax
        lda     LE5CD,x
        sta     $25
        lda     LE5CF,x
        sta     $24
LE4D6:  lda     $10
        and     #$01
        beq     LE4E1
        lda     $25
        jmp     LE4E5

LE4E1:  lda     $25
        eor     #$FF
LE4E5:  and     #$07
        clc
        adc     $03
        lsr     a
        lsr     a
        lsr     a
        bne     LE4F1
LE4EF:  sec
        rts

LE4F1:  lda     $10
        pha
        and     #$01
        tax
        pla
        cmp     $23
        sta     $23
        beq     LE501
        jmp     LE50F

LE501:  lda     $24
        clc
        adc     LE5C3,x
        cmp     #$20
        and     #$1F
        sta     $24
        bcc     LE517
LE50F:  lda     $29
        clc
        adc     LE5C3,x
        sta     $29
LE517:  lda     $22
        sta     $F5
        jsr     LFF6B
        lda     $24
        lsr     a
        lsr     a
        sta     $28
        ldy     $29
        jsr     LE8B1
        lda     #$00
        sta     $03
LE52D:  jsr     LE7F1
        ldy     $28
        lda     $0640,y
        sta     $11
        lda     $24
        and     #$03
        tay
        ldx     $03
        lda     $06C0,y
        sta     $0783,x
        lda     $06C4,y
        sta     $0784,x
        lda     $28
        cmp     #$38
        bcs     LE55C
        lda     $06C8,y
        sta     $0785,x
        lda     $06CC,y
        sta     $0786,x
LE55C:  lda     $24
        and     #$01
        beq     LE588
        lda     $10
        and     LE5C5,y
        sta     $10
        lda     $11
        and     LE5C9,y
        ora     $10
        sta     $07A4,x
        ldy     $28
        sta     $0640,y
        lda     #$23
        sta     $07A1,x
        tya
        ora     #$C0
        sta     $07A2,x
        lda     #$00
        sta     $07A3,x
LE588:  inc     $03
        inc     $03
        inc     $03
        inc     $03
        lda     $28
        clc
        adc     #$08
        sta     $28
        cmp     #$40
        bcc     LE52D
        lda     #$20
        sta     $0780
        lda     $24
        sta     $0781
        lda     #$1D
        sta     $0782
        ldy     #$00
        lda     $24
        and     #$01
        beq     LE5B4
        ldy     #$20
LE5B4:  lda     #$FF
        sta     $07A1,y
        ldy     $F8
        cpy     #$02
        beq     LE5C1
        sta     $1A
LE5C1:  clc
        rts

LE5C3:  .byte   $FF,$01
LE5C5:  .byte   $33,$33,$CC,$CC
LE5C9:  .byte   $CC,$CC,$33,$33
LE5CD:  .byte   $00,$FF
LE5CF:  .byte   $01,$1F
LE5D1:  jsr     LC628
LE5D4:  lda     $FC
        clc
        adc     #$04
        sta     $FC
        bcc     LE5DF
        inc     $F9
LE5DF:  lda     #$01
        sta     $10
        jsr     LE467
        lda     $FC
        sta     $25
        lda     $0340
        clc
        adc     #$D0
        sta     $0340
        lda     $0360
        adc     #$00
        sta     $0360
        lda     $0380
        adc     #$00
        sta     $0380
        jsr     LFF57
        lda     $FC
        bne     LE5D4
        lda     $22
        sta     $F5
        jsr     LFF6B
        jmp     LC83D

LE614:  jsr     LC628
        lda     $23
        and     #$04
        lsr     a
        lsr     a
        tax
        lda     LE7EB,x
        sta     $24
LE623:  lda     $23
        and     #$04
        beq     LE653
        lda     $FA
        clc
        adc     #$03
        sta     $FA
        cmp     #$F0
        bcc     LE638
        adc     #$0F
        sta     $FA
LE638:  lda     $03A0
        sec
        sbc     #$C0
        sta     $03A0
        lda     $03C0
        sbc     #$02
        sta     $03C0
        bcs     LE67A
        sbc     #$0F
        sta     $03C0
        jmp     LE67A

LE653:  lda     $FA
        sec
        sbc     #$03
        sta     $FA
        bcs     LE660
        sbc     #$0F
        sta     $FA
LE660:  lda     $03A0
        clc
        adc     #$C0
        sta     $03A0
        lda     $03C0
        adc     #$02
        sta     $03C0
        cmp     #$F0
        bcc     LE67A
        adc     #$0F
        sta     $03C0
LE67A:  jsr     LE698
        lda     $FA
        sta     $26
        lda     $12
        pha
        jsr     LFF57
        pla
        sta     $12
        lda     $FA
        beq     LE691
        jmp     LE623

LE691:  lda     $22
        sta     $F5
        jmp     LFF6B

LE698:  lda     $FA
        sec
        sbc     $26
        bpl     LE6A4
        eor     #$FF
        clc
        adc     #$01
LE6A4:  sta     $03
        beq     LE6C1
        lda     $23
        and     #$04
        beq     LE6B3
        lda     $26
        jmp     LE6B7

LE6B3:  lda     $26
        eor     #$FF
LE6B7:  and     #$07
        clc
        adc     $03
        lsr     a
        lsr     a
        lsr     a
        bne     LE6C2
LE6C1:  rts

LE6C2:  lda     $23
        and     #$04
        lsr     a
        lsr     a
        tay
        lda     $24
        clc
        adc     LE7E9,y
        sta     $24
        cmp     #$1E
        bcc     LE6DA
        lda     LE7E7,y
        sta     $24
LE6DA:  lda     $24
        and     #$01
        cmp     LE7E5,y
        beq     LE6E6
        jmp     LE79F

LE6E6:  lda     $22
        sta     $F5
        jsr     LFF6B
        ldy     $29
        jsr     LE8B1
        lda     $24
        and     #$1C
        asl     a
        sta     $28
        ora     #$C0
        sta     $07A4
        lda     #$23
        sta     $07A3
        lda     #$07
        sta     $07A5
        lda     #$00
        sta     $03
LE70C:  ldy     $28
        lda     $0640,y
        sta     $11
        jsr     LE7F1
        ldy     $03
        lda     $24
        and     #$03
        tax
        lda     LE7D9,x
        sta     $04
        lda     LE7DD,x
        sta     $05
        lda     #$03
        sta     $06
LE72B:  ldx     $04
        lda     $06C0,x
        sta     $0783,y
        ldx     $05
        lda     $06C0,x
        sta     $07AF,y
        inc     $04
        inc     $05
        iny
        dec     $06
        bpl     LE72B
        sty     $03
        lda     $24
        and     #$03
        tax
        lda     $10
        and     LE7D1,x
        sta     $10
        lda     $11
        and     LE7D5,x
        ora     $10
        sta     $10
        ldx     $28
        sta     $0640,x
        txa
        and     #$07
        tax
        lda     $10
        sta     $07A6,x
        inc     $28
        cpx     #$07
        bne     LE70C
        lda     $24
        pha
        and     #$03
        tay
        pla
        lsr     a
        lsr     a
        tax
        lda     LE8A9,x
        sta     $0780
        lda     LE8A1,x
        ora     LE7E1,y
        sta     $0781
        lda     #$1F
        sta     $0782
        lda     $23
        and     #$04
        lsr     a
        lsr     a
        tay
        ldx     LE7ED,y
        lda     #$FF
        sta     $0780,x
        sta     $19
        rts

LE79F:  ldy     #$1F
LE7A1:  lda     $07AF,y
        sta     $0783,y
        dey
        bpl     LE7A1
        lda     $24
        and     #$03
        tax
        lda     $0781
        and     #$80
        ora     LE7E1,x
        sta     $0781
        lda     #$23
        sta     $07A3
        lda     $23
        and     #$04
        lsr     a
        lsr     a
        tay
        ldx     LE7EF,y
        lda     #$FF
        sta     $0780,x
        sta     $19
        rts

LE7D1:  .byte   $0F,$0F,$F0,$F0
LE7D5:  .byte   $F0,$F0,$0F,$0F
LE7D9:  .byte   $00,$04,$08,$0C
LE7DD:  .byte   $04,$00,$0C,$08
LE7E1:  .byte   $00,$20,$40,$60
LE7E5:  .byte   $01,$00
LE7E7:  .byte   $1D,$00
LE7E9:  .byte   $FF,$01
LE7EB:  .byte   $00,$1D
LE7ED:  .byte   $23,$2E
LE7EF:  .byte   $2E,$23
LE7F1:  jsr     LE882
LE7F4:  ldy     #$03
        sty     $02
        lda     #$00
        sta     $10
LE7FC:  ldy     $02
        ldx     LE89D,y
        lda     (L0000),y
        tay
        lda     $BB00,y
        sta     $06C0,x
        lda     $BC00,y
        sta     $06C1,x
        lda     $BD00,y
        sta     $06C4,x
        lda     $BE00,y
        sta     $06C5,x
        jsr     LE834
        lda     $BF00,y
        and     #$03
        ora     $10
        sta     $10
        dec     $02
        bmi     LE833
        asl     $10
        asl     $10
        jmp     LE7FC

LE833:  rts

LE834:  lda     $22
        cmp     #$02
        beq     LE83E
        cmp     #$09
        bne     LE833
LE83E:  lda     $BF00,y
        and     #$F0
        cmp     #$70
        bne     LE833
        sty     $0F
        stx     $0E
        lda     $29
        and     #$01
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        sta     $0D
        lda     $28
        pha
        lsr     a
        ora     $0D
        tay
        pla
        asl     a
        asl     a
        and     #$04
        ora     $02
        tax
        lda     $0110,y
        and     LEB82,x
        beq     LE87D
        ldx     $0E
        lda     #$00
        sta     $06C0,x
        sta     $06C1,x
        sta     $06C4,x
        sta     $06C5,x
LE87D:  ldy     $0F
        ldx     $0E
        rts

LE882:  jsr     LC8A0
        lda     #$00
        sta     $01
        ldy     $28
        lda     ($20),y
LE88D:  asl     a
        rol     $01
        asl     a
        rol     $01
        sta     L0000
        lda     $01
        clc
        adc     #$B7
        sta     $01
        rts

LE89D:  .byte   $00,$02,$08,$0A
LE8A1:  .byte   $00,$80,$00,$80,$00,$80,$00,$80
LE8A9:  .byte   $20,$20,$21,$21,$22,$22,$23,$23
LE8B1:  lda     $AA00,y
LE8B4:  pha
        lda     #$00
        sta     L0000
        pla
        asl     a
        rol     L0000
        asl     a
        rol     L0000
        asl     a
        rol     L0000
        asl     a
        rol     L0000
        asl     a
        rol     L0000
        asl     a
        rol     L0000
        sta     $20
        lda     L0000
        clc
        adc     #$AF
        sta     $21
        rts

LE8D6:  lda     LEBE2,y
        sta     $40
        jsr     LEB0C
        tay
        lda     LEC10,y
        sta     $02
        lda     $03E0,x
        sta     $03
        lda     LEC11,y
        pha
        clc
        adc     $03C0,x
        sta     $11
        pla
        bmi     LE900
        bcs     LE902
        lda     $11
        cmp     #$F0
        bcs     LE902
        bcc     LE90F
LE900:  bcs     LE90F
LE902:  lda     #$00
        ldy     $02
LE906:  sta     $42,y
        dey
        bpl     LE906
        jmp     LEB24

LE90F:  lda     $03
        bne     LE902
        lda     $11
        lsr     a
        lsr     a
        pha
        and     #$38
        sta     $28
        pla
        lsr     a
        and     #$02
        sta     $03
        lda     #$00
        sta     $04
        lda     LEC12,y
        bpl     LE92D
        dec     $04
LE92D:  clc
        adc     $0360,x
        sta     $12
        lda     $0380,x
        adc     $04
        sta     $13
        lda     $12
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        pha
        and     #$01
        ora     $03
        sta     $03
        pla
        lsr     a
        ora     $28
        sta     $28
LE94D:  stx     $04
        lda     $22
        sta     $F5
        jsr     LFF6B
        ldx     $04
        ldy     $13
        jsr     LE8B1
LE95D:  jsr     LE882
LE960:  ldy     $03
        lda     (L0000),y
        tay
        lda     $BF00,y
        and     #$F0
        jsr     LEB30
        jsr     LEB8A
        ldy     $02
        sta     $42,y
        cmp     $41
        bcc     LE97B
        sta     $41
LE97B:  ora     $10
        sta     $10
        dec     $02
        bmi     LE9BA
        inc     $40
        ldy     $40
        lda     $12
        pha
        and     #$10
        sta     $04
        pla
        clc
        adc     LEC12,y
        sta     $12
        and     #$10
        cmp     $04
        beq     LE960
        lda     $03
        eor     #$01
        sta     $03
        and     #$01
        bne     LE960
        inc     $28
        lda     $28
        and     #$07
        bne     LE95D
        inc     $13
        dec     $28
        lda     $28
        and     #$38
        sta     $28
        jmp     LE94D

LE9BA:  cpx     #$00
        bne     LE9E0
        lda     $39
        bne     LE9E0
        lda     $3D
        bne     LE9E0
        lda     $30
        cmp     #$06
        beq     LE9E0
        cmp     #$0E
        beq     LE9E0
        ldy     #$06
        lda     $41
        cmp     #$30
        beq     LE9DE
        ldy     #$0E
        cmp     #$50
        bne     LE9E0
LE9DE:  sty     $3D
LE9E0:  jmp     LEB24

LE9E3:  lda     LECE1,y
        sta     $40
        jsr     LEB0C
        tay
        lda     LED07,y
        sta     $02
        lda     #$00
        sta     $04
        lda     LED08,y
        bpl     LE9FC
        dec     $04
LE9FC:  clc
        adc     $0360,x
        sta     $12
        lda     $0380,x
        adc     $04
        sta     $13
        lda     $12
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        pha
        and     #$01
        sta     $03
        pla
        lsr     a
        sta     $28
        lda     $03E0,x
        bmi     LEA41
        bne     LEA3B
        lda     $03C0,x
        clc
        adc     LED09,y
        sta     $11
        lda     LED09,y
        bpl     LEA31
        bcc     LEA41
        bcs     LEA33
LEA31:  bcs     LEA3B
LEA33:  lda     $11
        cmp     #$F0
        bcc     LEA6C
        bcs     LEA31
LEA3B:  lda     #$EF
        sta     $11
        bne     LEA6C
LEA41:  lda     #$00
        sta     $11
        beq     LEA6C
LEA47:  lda     $11
        clc
        adc     LED09,y
        sta     $11
        cmp     #$F0
        bcc     LEA5B
        adc     #$10
        sta     $11
        inc     $04
        beq     LEA6C
LEA5B:  iny
        sty     $40
        ldy     $02
        lda     #$00
        sta     $42,y
        dec     $02
        bpl     LEA47
        jmp     LEB24

LEA6C:  lda     $11
        lsr     a
        lsr     a
        pha
        and     #$38
        ora     $28
        sta     $28
        pla
        lsr     a
        and     #$02
        ora     $03
        sta     $03
        stx     $04
        lda     $22
        sta     $F5
        jsr     LFF6B
        ldx     $04
        ldy     $13
        jsr     LE8B1
LEA8F:  jsr     LE882
LEA92:  ldy     $03
        lda     (L0000),y
        tay
        lda     $BF00,y
        and     #$F0
        jsr     LEB30
        jsr     LEB8A
        ldy     $02
        sta     $42,y
        cmp     $41
        bcc     LEAAD
        sta     $41
LEAAD:  ora     $10
        sta     $10
        dec     $02
        bmi     LEAE9
        inc     $40
        ldy     $40
        lda     $11
        pha
        and     #$10
        sta     $04
        pla
        clc
        adc     LED09,y
        sta     $11
        and     #$10
        cmp     $04
        beq     LEA92
        lda     $03
        eor     #$02
        sta     $03
        and     #$02
        bne     LEA92
        lda     $28
        pha
        clc
        adc     #$08
        sta     $28
        cmp     #$40
        pla
        bcc     LEA8F
        sta     $28
        jmp     LEA92

LEAE9:  cpx     #$00
        bne     LEB09
        lda     $39
        bne     LEB09
        lda     $3D
        bne     LEB09
        lda     $30
        cmp     #$06
        beq     LEB09
        cmp     #$0E
        beq     LEB09
        lda     $41
        cmp     #$50
        bne     LEB09
        lda     #$0E
        sta     $3D
LEB09:  jmp     LEB24

LEB0C:  pha
        txa
        pha
        lda     #$00
        sta     $10
        sta     $41
        lda     $F5
        sta     $2F
        lda     $22
        sta     $F5
        jsr     LFF6B
        pla
        tax
        pla
        rts

LEB24:  txa
        pha
        lda     $2F
        sta     $F5
        jsr     LFF6B
        pla
        tax
        rts

LEB30:  sta     $06
        stx     $05
        cmp     #$70
        bne     LEB68
        lda     $22
        cmp     #$02
        beq     LEB42
        cmp     #$09
        bne     LEB68
LEB42:  lda     $13
        and     #$01
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        sta     $07
        lda     $28
        pha
        lsr     a
        ora     $07
        tay
        pla
        asl     a
        asl     a
        and     #$04
        ora     $03
        tax
        lda     $0110,y
        and     LEB82,x
        beq     LEB68
        lda     #$00
        sta     $06
LEB68:  lda     $06
        ldx     $05
        rts

LEB6D:  lda     $22
        cmp     #$02
        beq     LEB77
        cmp     #$09
        bne     LEB81
LEB77:  lda     #$00
        ldy     #$3F
        sta     $0110,y
        .byte   $88,$10,$FA
LEB81:  .byte   $60
LEB82:  .byte   $80,$40,$20,$10
        php
        .byte   $04
        .byte   $02
        .byte   $01
LEB8A:  sta     $05
        stx     $06
        sty     $07
        lda     $68
        beq     LEBB9
        ldy     $22
        ldx     LEBC6,y
        bmi     LEBC0
        lda     LEBCE,x
        cmp     $F9
        bne     LEBC0
        lda     LEBCF,x
        sta     $08
        lda     $28
LEBA9:  cmp     LEBD0,x
        beq     LEBB5
        inx
        dec     $08
        bpl     LEBA9
        bmi     LEBB9
LEBB5:  lda     #$00
        beq     LEBBB
LEBB9:  lda     $05
LEBBB:  ldx     $06
        ldy     $07
        rts

LEBC0:  lda     #$00
        .byte   $85,$68,$F0,$F3
LEBC6:  .byte   $FF,$00,$11,$04,$FF,$FF,$FF,$0E
LEBCE:  .byte   $05
LEBCF:  .byte   $01
LEBD0:  .byte   $31,$39,$13,$07,$23,$24,$2B,$2C
        .byte   $33,$34,$3B,$3C,$05,$00,$31,$09
        .byte   $00,$00
LEBE2:  .byte   $00,$05,$09,$0E,$12,$16,$1A,$1F
        .byte   $24,$28,$2C,$31,$36,$3B,$40,$45
        .byte   $49,$4E,$53,$57,$5B,$5F,$63,$68
        .byte   $6C,$70,$75,$79,$7D,$81,$85,$8A
        .byte   $8F,$94,$99,$9E,$A3,$A8,$AD,$B2
        .byte   $B7,$BC,$C1,$C6,$CB,$CE
LEC10:  .byte   $02
LEC11:  .byte   $0C
LEC12:  .byte   $F9,$07,$07,$01,$F4,$F9,$0E,$02
        .byte   $0A,$F1,$0F,$0F,$01,$00,$F9,$0E
        .byte   $01,$00,$08,$0E,$01,$00,$EA,$0E
        .byte   $00,$00,$00,$00,$00,$02,$16,$F9
        .byte   $07,$07,$01,$08,$F9,$0E,$01,$F8
        .byte   $F9,$0E,$02,$10,$F5,$0B,$0B,$02
        .byte   $F0,$F5,$0B,$0B,$02,$08,$FC,$04
        .byte   $04,$00,$00,$00,$00,$00,$02,$0C
        .byte   $F5,$0B,$0B,$01,$0C,$F5,$16,$02
        .byte   $18,$F1,$0F,$0F,$02,$E8,$F1,$0F
        .byte   $0F,$01,$04,$FD,$06,$01,$FC,$FD
        .byte   $06,$01,$1C,$F9,$0E,$01,$10,$F9
        .byte   $07,$02,$E8,$F9,$07,$07,$01,$06
        .byte   $F9,$0E,$01,$08,$F5,$16,$02,$F8
        .byte   $F5,$0B,$0B,$01,$0C,$F9,$0E,$01
        .byte   $F4,$F9,$0E,$01,$04,$F9,$0E,$01
        .byte   $FC,$F9,$0E,$02,$10,$F1,$0F,$0F
        .byte   $02,$F0,$F1,$0F,$0F,$02,$0D,$F5
        .byte   $0B,$0B,$02,$F3,$F5,$0B,$0B,$02
        .byte   $10,$F1,$0F,$0F,$02,$E6,$F1,$0F
        .byte   $0F,$02,$14,$F1,$0F,$0F,$02,$00
        .byte   $00,$00,$00,$02,$14,$F0,$10,$10
        .byte   $02,$EC,$F0,$10,$10,$02,$18,$F1
        .byte   $0F,$0F,$02,$E8,$F1,$0F,$0F,$02
        .byte   $1C,$F5,$0B,$0B,$02,$DC,$F5,$0B
        .byte   $0B,$00,$04,$00,$00,$08,$00
LECE1:  .byte   $00,$05,$0A,$0E,$12,$17,$1C,$21
        .byte   $26,$2A,$2E,$33,$38,$3D,$42,$49
        .byte   $50,$57,$5E,$65,$6C,$71,$76,$7B
        .byte   $80,$85,$8A,$8F,$94,$99,$9E,$A2
        .byte   $A6,$AB,$B0,$B5,$BA,$BF
LED07:  .byte   $02
LED08:  .byte   $08
LED09:  .byte   $F5,$0B,$0B,$02,$F8,$F5,$0B,$0B
        .byte   $01,$10,$FB,$0C,$01,$F0,$FB,$0C
        .byte   $02,$00,$F5,$0B,$0B,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$08,$F9,$0E
        .byte   $01,$F8,$F9,$0E,$01,$0C,$F5,$0B
        .byte   $0B,$02,$F4,$F5,$0B,$0B,$02,$0C
        .byte   $F5,$0B,$0B,$02,$F4,$F5,$0B,$0B
        .byte   $04,$10,$E9,$10,$07,$10,$07,$04
        .byte   $F0,$E9,$10,$07,$10,$07,$04,$0C
        .byte   $E5,$10,$0B,$10,$0B,$04,$F4,$E5
        .byte   $10,$0B,$10,$0B,$04,$08,$E5,$10
        .byte   $0B,$10,$0B,$01,$F8,$E5,$10,$0B
        .byte   $10,$0B,$02,$10,$F1,$0F,$0F,$02
        .byte   $F0,$F1,$0F,$0F,$02,$14,$F5,$0B
        .byte   $0B,$02,$EC,$F5,$0B,$0B,$02,$0C
        .byte   $F9,$07,$07,$02,$F4,$F9,$07,$07
        .byte   $02,$0C,$F9,$07,$07,$02,$F4,$F9
        .byte   $07,$07,$02,$0C,$F9,$07,$07,$02
        .byte   $F4,$F9,$07,$07,$01,$04,$FD,$06
        .byte   $01,$FC,$FD,$06,$02,$10,$F1,$0F
LEDB1:  .byte   $0F,$02,$F0,$F1,$0F,$0F,$02,$14
        .byte   $EC,$14,$14,$02,$EC,$EC,$14,$14
        .byte   $02,$10,$E8,$18,$18,$02
        beq     LEDB1
        clc
        clc
LEDCB:  lda     $12
        and     #$0F
        sta     $12
        lda     $0360,x
        sec
        sbc     $12
        sta     $0360,x
        lda     $0380,x
        sbc     #$00
        sta     $0380,x
        rts

LEDE3:  lda     $12
        and     #$0F
        eor     #$0F
        sec
        adc     $0360,x
        sta     $0360,x
        lda     $0380,x
        adc     #$00
        sta     $0380,x
        rts

LEDF9:  lda     $11
        and     #$0F
        eor     #$0F
        sec
        adc     $03C0,x
        sta     $03C0,x
        cmp     #$F0
        bcc     LEE12
        adc     #$0F
        sta     $03C0,x
        inc     $03E0,x
LEE12:  rts

LEE13:  lda     $11
        pha
        and     #$0F
        sta     $11
        lda     $03C0,x
        sec
        sbc     $11
        sta     $03C0,x
        bcs     LEE2D
        sbc     #$0F
        sta     $03C0,x
        dec     $03E0,x
LEE2D:  pla
        sta     $11
        rts

LEE31:  lda     $F4
        pha
        lda     #$10
        sta     $F4
        jsr     LFF6B
        jsr     L8000
        pla
        sta     $F4
        jmp     LFF6B

LEE44:  lda     $F4
        pha
        lda     #$10
        sta     $F4
        jsr     LFF6B
        jsr     L8003
        pla
        sta     $F4
        jmp     LFF6B

        sec
        lda     $1A
        ora     $19
        bne     LEEAA
        lda     #$08
        sta     $0780
        lda     $03C0,x
        and     #$F0
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        sta     $0781
        lda     $0360,x
        and     #$F0
        lsr     a
        lsr     a
        lsr     a
        ora     $0781
        sta     $0781
        ora     #$20
        sta     $0786
        lda     #$01
        sta     $0782
        sta     $0787
        lda     $0780
        sta     $0785
        lda     #$00
        sta     $0783
        sta     $0784
        sta     $0788
        sta     $0789
        lda     #$FF
        sta     $078A
        sta     $19
        clc
LEEAA:  rts

        lda     $28
        pha
        and     #$07
        asl     a
        asl     a
        sta     $0781
        lda     #$02
        sta     $0780
        pla
        and     #$F8
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        ora     $0781
        sta     $0781
        clc
        adc     #$20
        sta     $0788
        adc     #$20
        sta     $078F
        adc     #$20
        sta     $0796
        lda     $28
        ora     #$C0
        sta     $079D
        lda     $0780
        ora     $10
        sta     $0780
        sta     $0787
        sta     $078E
        sta     $0795
        ora     #$03
        sta     $079C
        lda     #$03
        sta     $0782
        sta     $0789
        sta     $0790
        sta     $0797
        lda     #$00
        sta     $079E
        lda     $68
        beq     LEF26
        lda     #$00
        sta     $01
        lda     $11
        jsr     LE88D
        jsr     LE7F4
        jmp     LEF2D

LEF26:  tya
        jsr     LE8B4
        jsr     LE7F1
LEF2D:  ldx     #$03
LEF2F:  lda     $06C0,x
        sta     $0783,x
        lda     $06C4,x
        sta     $078A,x
        lda     $06C8,x
        sta     $0791,x
        lda     $06CC,x
        sta     $0798,x
        dex
        bpl     LEF2F
        lda     $10
        sta     $079F
        stx     $07A0
        lda     $28
        and     #$3F
        cmp     #$38
        bcc     LEF65
        ldx     #$04
LEF5C:  lda     $079C,x
        sta     $078E,x
        dex
        bpl     LEF5C
LEF65:  stx     $19
        rts

LEF68:  lda     #$00
        sta     $70
        lda     #$23
        ora     $10
        sta     $0780
        lda     #$C0
        sta     $0781
        ldy     #$3F
        sty     $0782
LEF7D:  lda     $0640,y
        sta     $0783,y
        dey
        bpl     LEF7D
        sty     $07C3
        sty     $19
        rts

LEF8C:  lda     $70
        cmp     #$40
        beq     LEF68
        sta     $28
        pha
        and     #$07
        asl     a
        asl     a
        sta     $0781
        lda     #$02
        sta     $0780
        pla
        and     #$F8
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        asl     a
        rol     $0780
        ora     $0781
        sta     $0781
        clc
        adc     #$20
        sta     $0794
        adc     #$20
        sta     $07A7
        adc     #$20
        sta     $07BA
        lda     $0780
        ora     $10
        sta     $0780
        sta     $0793
        sta     $07A6
        sta     $07B9
        lda     #$0F
        sta     $0782
        sta     $0795
        sta     $07A8
        sta     $07BB
LEFE9:  jsr     LE7F1
        lda     $28
        and     #$03
        tax
        ldy     LF030,x
        ldx     #$00
LEFF6:  lda     $06C0,x
        sta     $0780,y
        iny
        inx
        txa
        and     #$03
        bne     LEFF6
        tya
        clc
        adc     #$0F
        pha
        ldy     $28
        lda     $10
        sta     $0640,y
        pla
        tay
        cpy     #$4C
        bcc     LEFF6
        inc     $70
        inc     $28
        lda     $28
        and     #$03
        bne     LEFE9
        lda     #$FF
        sta     $07CC
        ldy     $28
        cpy     #$39
        bcc     LF02D
        sta     $07A6
LF02D:  .byte   $85,$19,$60
LF030:  .byte   $03
        .byte   $07
        .byte   $0B
        .byte   $0F
LF034:  inc     $95
        inc     $4F
        lda     $95
        lsr     a
        bcs     LF055
        jsr     LF298
        ldx     #$00
        stx     $96
LF044:  lda     $0300,x
        bpl     LF04C
        jsr     LF06B
LF04C:  inc     $96
        ldx     $96
        cpx     #$20
        bne     LF044
        rts

LF055:  ldx     #$1F
        stx     $96
LF059:  lda     $0300,x
        bpl     LF061
        jsr     LF06B
LF061:  dec     $96
        ldx     $96
        bpl     LF059
        jsr     LF298
        rts

LF06B:  lda     $0580,x
        and     #$10
        beq     LF0F1
        lda     $0360,x
        sec
        sbc     $FC
        sta     $13
        lda     $0380,x
        sbc     $F9
        bne     LF091
        lda     $03E0,x
        beq     LF0F6
        cpx     #$00
        beq     LF0AF
        lda     $03E0,x
        bmi     LF0AF
        bpl     LF0A4
LF091:  lda     $0580,x
        and     #$08
        beq     LF0A4
        lda     $13
        bcs     LF0A0
        eor     #$FF
        adc     #$01
LF0A0:  cmp     #$48
        bcc     LF0AF
LF0A4:  lda     #$00
        sta     $0300,x
        lda     #$FF
        sta     $04C0,x
        rts

LF0AF:  lda     $0580,x
        and     #$7F
        sta     $0580,x
        cpx     #$00
        bne     LF0EE
        lda     $17
        and     #$01
        bne     LF0D9
        lda     $03E0
        bmi     LF0EE
        lda     #$0E
        cmp     $30
        beq     LF0D8
        sta     $30
        lda     #$F2
        jsr     LF89A
        lda     #$17
        jsr     LF89A
LF0D8:  rts

LF0D9:  lda     $03E0
        cmp     #$01
        bne     LF0EE
        lda     #$01
        sta     $30
        lda     #$10
        sta     $0440
        lda     #$0D
        sta     $0460
LF0EE:  jmp     LF108

LF0F1:  lda     $0360,x
        sta     $13
LF0F6:  lda     $03C0,x
        sta     $12
        lda     $0580,x
        ora     #$80
        sta     $0580,x
        and     #$04
        beq     LF108
LF107:  rts

LF108:  ldy     #$00
        lda     $0580,x
        and     #$40
        sta     $10
        beq     LF114
        iny
LF114:  sty     $11
        cpx     #$10
        bcc     LF126
        lda     $5A
        beq     LF126
        ldy     #$15
        cpy     $F4
        beq     LF13B
        bne     LF132
LF126:  ldy     #$1A
        lda     $05C0,x
        bpl     LF12E
        iny
LF12E:  cpy     $F4
        beq     LF13B
LF132:  sty     $F4
        stx     L0000
        jsr     LFF6B
        ldx     L0000
LF13B:  lda     $05C0,x
        beq     LF107
        and     #$7F
        tay
        lda     L8000,y
        sta     L0000
        lda     $8080,y
        sta     $01
        lda     $17
        and     #$08
        beq     LF15F
        lda     $95
        and     #$07
        bne     LF18B
        lda     $17
        and     #$80
        bne     LF18B
LF15F:  lda     $58
        bne     LF18B
        lda     $05E0,x
        and     #$7F
        inc     $05E0,x
        ldy     #$01
        cmp     (L0000),y
        bne     LF18B
        lda     $05E0,x
        and     #$80
        sta     $05E0,x
        lda     $05A0,x
        and     #$7F
        inc     $05A0,x
        dey
        cmp     (L0000),y
        bne     LF18B
        lda     #$00
        sta     $05A0,x
LF18B:  lda     $0580,x
        bpl     LF1E3
        lda     $05E0,x
        bpl     LF19B
        lda     $92
        and     #$04
        bne     LF1E3
LF19B:  cpx     #$10
        bcc     LF1CE
        lda     $04E0,x
        and     #$E0
        beq     LF1CE
        lda     $0300,x
        and     #$40
        beq     LF1B3
        lda     $4F
        and     #$03
        bne     LF1BC
LF1B3:  lda     $04E0,x
        sec
        sbc     #$20
        sta     $04E0,x
LF1BC:  lda     $0300,x
        and     #$40
        beq     LF1E3
        lda     $04E0,x
        and     #$20
        beq     LF1CE
        lda     #$AF
        bne     LF1E4
LF1CE:  lda     $05A0,x
        and     #$7F
        clc
        adc     #$02
        tay
        lda     (L0000),y
        bne     LF1E4
        sta     $0300,x
        lda     #$FF
        sta     $04C0,x
LF1E3:  rts

LF1E4:  tay
        lda     $8100,y
        sta     $02
        lda     $8200,y
        sta     $03
        ldy     #$00
        lda     ($02),y
        pha
        ldy     #$19
        pla
        bpl     LF1FD
        and     #$7F
        ldy     #$14
LF1FD:  sta     $04
        cpy     $F5
        beq     LF20C
        sty     $F5
        stx     $05
        jsr     LFF6B
        ldx     $05
LF20C:  ldy     #$01
        lda     ($02),y
        clc
        adc     $11
        pha
        lda     $0580,x
        and     #$20
        sta     $11
        pla
        tax
        lda     $BE00,x
        sec
        sbc     #$02
        sta     $05
        lda     $BF00,x
        sbc     #$00
        sta     $06
        ldx     $97
        beq     LF28F
LF230:  lda     #$F0
        sta     L0000
        lda     $22
        cmp     #$08
        bne     LF248
        lda     $F9
        cmp     #$15
        beq     LF244
        cmp     #$1A
        bne     LF248
LF244:  lda     #$B0
        sta     L0000
LF248:  iny
        lda     ($02),y
        sta     $0201,x
        lda     $12
        clc
        adc     ($05),y
        sta     $0200,x
        lda     ($05),y
        bmi     LF25E
        bcc     LF260
        bcs     LF290
LF25E:  bcc     LF290
LF260:  lda     $0200,x
        cmp     L0000
        bcs     LF290
        iny
        lda     ($02),y
        eor     $10
        ora     $11
        sta     $0202,x
        lda     $13
        clc
        adc     ($05),y
        sta     $0203,x
        lda     ($05),y
        bmi     LF281
        bcc     LF283
        bcs     LF291
LF281:  bcc     LF291
LF283:  inx
        inx
        inx
        inx
        stx     $97
        beq     LF28F
LF28B:  dec     $04
        bpl     LF230
LF28F:  rts

LF290:  iny
LF291:  lda     #$F8
        sta     $0200,x
        bne     LF28B
LF298:  lda     $95
        lsr     a
        bcs     LF2AD
        ldx     #$00
        stx     $10
LF2A1:  jsr     LF2BB
        inc     $10
        ldx     $10
        cpx     #$03
        bne     LF2A1
        rts

LF2AD:  ldx     #$02
        stx     $10
LF2B1:  jsr     LF2BB
        dec     $10
        ldx     $10
        bpl     LF2B1
LF2BA:  rts

LF2BB:  lda     $B1,x
        bpl     LF2BA
        and     #$7F
        tay
        lda     $A2,y
        and     #$7F
        sta     L0000
        lda     LF318,x
        sta     $01
        lda     LF31B,x
        sta     $02
        ldx     $97
        beq     LF310
        lda     #$48
        sta     $03
        lda     $01
        sta     $0202,x
        lda     $02
        sta     $0203,x
        lda     $03
        sta     $0200,x
        ldy     #$04
        lda     L0000
        sec
        sbc     #$04
        bcs     LF2F7
        ldy     L0000
        lda     #$00
LF2F7:  sta     L0000
        lda     LF313,y
        sta     $0201,x
        inx
        inx
        inx
        inx
        beq     LF310
        lda     $03
        sec
        sbc     #$08
        sta     $03
        cmp     #$10
        .byte   $D0,$CB
LF310:  .byte   $86,$97,$60
LF313:  .byte   $6B,$6A,$69,$68,$67
LF318:  .byte   $00,$01,$02
LF31B:  .byte   $10,$18,$28,$A0,$FB,$2A,$EC,$88
        .byte   $DF,$B8,$FE,$08,$50,$0A,$EB,$0A
        .byte   $6D,$8A,$6F,$2A,$D0,$A0,$B7,$8E
        .byte   $CD,$0A,$EC,$28,$C8,$28,$13,$A0
        .byte   $F7,$68,$83,$80,$CC,$A8,$BE,$AA
        .byte   $FB,$AA,$ED,$2A,$F9,$08,$F4,$A8
        .byte   $F7,$AA,$FB,$20,$CF,$2A,$9F,$28
        .byte   $BF,$CA,$FC,$E0,$78,$CA,$FF,$A2
        .byte   $BF,$20,$FB,$6A,$FF,$28,$FE,$AA
        .byte   $66,$A8,$B7,$8A,$59,$A2,$B2,$2B
        .byte   $AD,$0A,$96,$22,$3A,$82,$CA,$2A
        .byte   $DB,$A8,$92,$9A,$27,$88,$7E,$2A
        .byte   $E7,$46,$EE,$A2,$E9,$26,$C6,$C8
        .byte   $8C,$02,$DE,$B6,$AF,$E2,$5F,$EA
        .byte   $7D,$8A,$BB,$BA,$EC,$02,$FE,$90
        .byte   $E7,$88,$F7,$AE,$FE,$EA,$EE,$2A
        .byte   $B9,$2A,$BB,$A8,$77,$40,$E3,$AA
        .byte   $3D,$0A,$B2,$B8,$4F,$8A,$B8,$CC
        .byte   $FB,$A2,$FB,$81,$6E,$B2,$AD,$82
        .byte   $EF,$93,$76,$AC,$5F,$2E,$CF,$04
        .byte   $E1,$B0,$FF,$82,$9C,$2A,$FF,$6A
        .byte   $83,$A8,$D8,$AA,$4F,$2B,$F3,$28
        .byte   $5C,$2E,$D4,$8A,$52,$82,$67,$0A
        .byte   $D3,$EA,$5E,$0A,$9C,$8A,$36,$82
        .byte   $AB,$2A,$7E,$AD,$F8,$A0,$CE,$2A
        .byte   $2D,$A0,$ED,$88,$9F,$88,$D7,$2A
        .byte   $BF,$AA,$78,$A0,$ED,$A8,$DC,$0E
        .byte   $9B,$28,$FE,$EA,$75,$A2,$57,$A8
        .byte   $99,$A2,$7F,$A2,$B9,$7A,$04,$FC
        .byte   $11,$DC,$05,$3B,$05,$9B,$40,$4B
        .byte   $55,$3C,$51,$8A,$00,$67,$54,$72
        .byte   $15,$1A,$11,$59,$15,$AA,$45,$16
        .byte   $41,$47,$05,$A0,$51,$F1,$45,$76
        .byte   $10,$84,$11,$FF,$14,$6A,$54,$B8
        .byte   $54,$DC,$55,$FD,$40,$92,$51,$1D
        .byte   $14,$32,$40,$C3,$44,$40,$11,$84
        .byte   $45,$96,$44,$D9,$11,$0F,$01,$DB
        .byte   $40,$96,$01,$24,$11,$62,$19,$83
        .byte   $55,$4F,$15,$E7,$14,$3F,$04,$A0
        .byte   $45,$EE,$70,$8A,$54,$59,$44,$7D
        .byte   $11,$39,$44,$6F,$41,$1B,$15,$42
        .byte   $05,$71,$50,$E8,$11,$FD,$44,$2E
        .byte   $00,$BC,$45,$F1,$14,$7B,$51,$E1
        .byte   $45,$49,$10,$2C,$44,$46,$45,$F3
        .byte   $45,$46,$04,$E7,$40,$41,$00,$E8
        .byte   $41,$7A,$01,$E7,$11,$92,$11,$9F
        .byte   $00,$E7,$45,$4E,$51,$D0,$11,$C1
        .byte   $45,$26,$41,$48,$40,$0C,$50,$CD
        .byte   $51,$31,$54,$4D,$50,$6B,$00,$98
        .byte   $51,$78,$14,$CF,$55,$4C,$05,$8E
        .byte   $14,$26,$15,$EB,$04,$CB,$14,$68
        .byte   $50,$06,$00,$F9,$15,$9B,$20,$0C
        .byte   $15,$DE,$01,$C1,$55,$B5,$51,$20
        .byte   $11,$6A,$40,$C1,$00,$19,$54,$58
        .byte   $05,$2C,$41,$0B,$40,$11,$00,$E6
        .byte   $50,$AF,$14,$94,$00,$03,$51,$E6
        .byte   $04,$8F,$15,$EE,$51,$6E,$11,$D7
        .byte   $55,$F6,$05,$6E,$55,$C5,$D5,$CB
        .byte   $51,$FF,$40,$7A,$41,$FA,$50,$57
        .byte   $55,$59,$10,$40,$10,$29,$C4,$01
        .byte   $00,$20,$54,$2D,$45,$60,$04,$89
        .byte   $14,$2F,$65,$84,$51,$A1,$41,$BA
        .byte   $1C,$07,$01,$6C,$41,$79,$54,$88
        .byte   $51,$21,$40,$F9,$14,$3C,$11,$24
        .byte   $15,$A0,$54,$86,$1A,$78,$10,$41
        .byte   $05,$94,$54,$6D,$11,$85,$40,$A1
        .byte   $45,$25,$45,$23,$45,$53,$14,$E3
        .byte   $51,$67,$4C,$F7,$01,$8A,$14,$62
        .byte   $44,$C6,$45,$CA,$44,$E0,$45,$E7
        .byte   $01,$05,$14,$04,$41,$F6,$05,$C3
        .byte   $10,$03,$40,$AA,$40,$CA,$01,$66
        .byte   $05,$DB,$05,$0C,$14,$3D,$44,$80
        .byte   $50,$0A,$51,$AC,$71,$EE,$40,$53
        .byte   $D4,$F5,$05,$56,$00,$EC,$14,$9A
        .byte   $10,$B0,$50,$BE,$55,$F6,$14,$7C
        .byte   $01,$E0,$54,$2A,$45,$4A,$51,$6D
        ora     $38,x
        .byte   $14
        .byte   $E2
        .byte   $50
LF580:  lda     $0580,x
        ora     #$40
        sta     $0580,x
        cpx     #$00
        bne     LF596
        lda     $0360
        sta     $02
        lda     $0380
        sta     $03
LF596:  jsr     LF71D
        cpx     #$00
        bne     LF5B2
        jsr     LFA00
        bcc     LF5B2
        jsr     LFA7D
        jsr     LF5AA
        sec
        rts

LF5AA:  beq     LF5B2
        bcc     LF5B2
        iny
        jmp     LF5F4

LF5B2:  jsr     LE9E3
        jsr     LF700
        clc
        lda     $10
        and     #$10
        beq     LF5C3
        jsr     LEDCB
        sec
LF5C3:  rts

LF5C4:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        cpx     #$00
        bne     LF5DA
        lda     $0360
        sta     $02
        lda     $0380
        sta     $03
LF5DA:  jsr     LF73B
        cpx     #$00
        bne     LF5F4
        jsr     LFA00
        bcc     LF5F4
        jsr     LFA91
        jsr     LF5EE
        sec
        rts

LF5EE:  bcs     LF5F4
        dey
        jmp     LF5B2

LF5F4:  jsr     LE9E3
        jsr     LF700
        clc
        lda     $10
        and     #$10
        beq     LF605
        jsr     LEDE3
        sec
LF605:  rts

LF606:  cpx     #$00
        bne     LF614
        lda     $03C0
        sta     $02
        lda     $0380
        sta     $03
LF614:  jsr     LF759
        cpx     #$00
        bne     LF630
        jsr     LF97E
        bcc     LF630
        jsr     LFAAE
        jsr     LF628
        sec
        rts

LF628:  beq     LF630
        bcc     LF630
        iny
        jmp     LF66A

LF630:  jsr     LE8D6
        jsr     LF700
        clc
        lda     $10
        and     #$10
        beq     LF641
        jsr     LEE13
        sec
LF641:  rts

LF642:  cpx     #$00
        bne     LF650
        lda     $03C0
        sta     $02
        lda     $0380
        sta     $03
LF650:  jsr     LF779
        cpx     #$00
        bne     LF66A
        jsr     LF97E
        bcc     LF66A
        jsr     LFABF
        jsr     LF664
        sec
        rts

LF664:  bcs     LF66A
        dey
        jmp     LF630

LF66A:  jsr     LE8D6
        jsr     LF700
        clc
        lda     $10
        and     #$10
        beq     LF67B
        jsr     LEDF9
        sec
LF67B:  rts

LF67C:  cpx     #$00
        bne     LF68A
        lda     $03C0
        sta     $02
        lda     $0380
        sta     $03
LF68A:  lda     $0460,x
        bpl     LF6CD
        jsr     LF7A8
        cpx     #$00
        bne     LF6AC
        jsr     LF97E
        bcc     LF6AC
        jsr     LFAAE
        jsr     LF6A4
        jmp     LF6C8

LF6A4:  beq     LF6AC
        bcc     LF6AC
        iny
        jmp     LF66A

LF6AC:  jsr     LF7E6
        jsr     LE8D6
        jsr     LF700
        cpx     #$00
        bne     LF6BF
        lda     $41
        cmp     #$40
        beq     LF6C5
LF6BF:  lda     $10
        and     #$10
        beq     LF6FE
LF6C5:  jsr     LEE13
LF6C8:  jsr     LF81B
        sec
        rts

LF6CD:  iny
        jsr     LF7C8
        cpx     #$00
        bne     LF6E9
        jsr     LF97E
        bcc     LF6E9
        jsr     LFABF
        jsr     LF6E3
        jmp     LF6FB

LF6E3:  bcs     LF6E9
        dey
        jmp     LF630

LF6E9:  jsr     LF7E6
        jsr     LE8D6
        jsr     LF700
        lda     $10
        and     #$10
        beq     LF6FE
        jsr     LEDF9
LF6FB:  jsr     LF81B
LF6FE:  clc
        rts

LF700:  lda     $10
        and     #$10
        bne     LF71C
        lda     $0580,x
        and     #$DF
        sta     $0580,x
        lda     $41
        cmp     #$60
        bne     LF71C
        lda     $0580,x
        ora     #$20
        sta     $0580,x
LF71C:  rts

LF71D:  lda     $0340,x
        clc
        adc     $0400,x
        sta     $0340,x
        lda     $0360,x
        adc     $0420,x
        sta     $0360,x
        bcc     LF73A
        lda     $0380,x
        adc     #$00
        sta     $0380,x
LF73A:  rts

LF73B:  lda     $0340,x
        sec
        sbc     $0400,x
        sta     $0340,x
        lda     $0360,x
        sbc     $0420,x
        sta     $0360,x
        bcs     LF758
        lda     $0380,x
        sbc     #$00
        sta     $0380,x
LF758:  rts

LF759:  lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        cmp     #$F0
        bcc     LF778
        adc     #$0F
        sta     $03C0,x
        inc     $03E0,x
LF778:  rts

LF779:  lda     $03A0,x
        sec
        sbc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        sbc     $0460,x
        sta     $03C0,x
        bcs     LF796
        sbc     #$0F
        sta     $03C0,x
        dec     $03E0,x
LF796:  rts

LF797:  lda     $0460,x
        bpl     LF7A2
        jsr     LF7A8
        jmp     LF7E6

LF7A2:  jsr     LF7C8
        jmp     LF7E6

LF7A8:  lda     $03A0,x
        sec
        sbc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        sbc     $0460,x
        sta     $03C0,x
        cmp     #$F0
        bcc     LF7C7
        adc     #$0F
        sta     $03C0,x
        inc     $03E0,x
LF7C7:  rts

LF7C8:  lda     $03A0,x
        sec
        sbc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        sbc     $0460,x
        sta     $03C0,x
        bcs     LF7E5
        sbc     #$0F
        sta     $03C0,x
        dec     $03E0,x
LF7E5:  rts

LF7E6:  cpx     #$00
        bne     LF7F2
        lda     $B9
        beq     LF7F2
        dec     $B9
        bne     LF81A
LF7F2:  lda     $0440,x
        sec
        sbc     $99
        sta     $0440,x
        lda     $0460,x
        sbc     #$00
        sta     $0460,x
        bpl     LF81A
        cmp     #$F9
        bcs     LF81A
        lda     $05C0,x
        cmp     #$13
        beq     LF81A
        lda     #$F9
        sta     $0460,x
        lda     #$00
        sta     $0440,x
LF81A:  rts

LF81B:  cpx     #$00
        beq     LF82A
        lda     #$AB
        sta     $0440,x
        lda     #$FF
        sta     $0460,x
        rts

LF82A:  lda     #$C0
        sta     $0440,x
        lda     #$FF
        sta     $0460,x
        rts

LF835:  sta     $05C0,x
        lda     #$00
        sta     $05A0,x
        lda     $05E0,x
        and     #$80
        sta     $05E0,x
        rts

LF846:  sta     $05C0,y
        lda     #$00
        sta     $05A0,y
        sta     $03E0,y
        lda     $0580,x
        and     #$40
        ora     #$90
        sta     $0580,y
        lda     #$80
        sta     $0300,y
        lda     $05E0,y
        and     #$80
        sta     $05E0,y
        rts

        lda     #$01
        sta     $04A0,x
        lda     $0360,x
        sec
        sbc     $0360
        lda     $0380,x
        sbc     $0380
        bcc     LF882
        lda     #$02
        sta     $04A0,x
LF882:  rts

        lda     $04A0,x
        ror     a
        ror     a
        ror     a
        and     #$40
        sta     L0000
        lda     $0580,x
        and     #$BF
        ora     L0000
        sta     $0580,x
        rts

LF898:  sta     $D9
LF89A:  stx     L0000
        ldx     $DA
        sta     $01
        lda     $DC,x
        cmp     #$88
        bne     LF8B0
        lda     $01
        sta     $DC,x
        inx
        txa
        and     #$07
        sta     $DA
LF8B0:  ldx     L0000
        rts

LF8B3:  lda     $03C0
        sec
        sbc     $03C0,x
        bcs     LF8C1
        eor     #$FF
        adc     #$01
        clc
LF8C1:  rts

LF8C2:  lda     $0360
        sec
        sbc     $0360,x
        pha
        lda     $0380
        sbc     $0380,x
        pla
        bcs     LF8D8
        eor     #$FF
        adc     #$01
        clc
LF8D8:  rts

        ldy     #$00
        lda     $03C0
        sec
        sbc     $03C0,x
        ldy     #$00
        bcs     LF8EC
        eor     #$FF
        adc     #$01
        ldy     #$04
LF8EC:  sta     L0000
        lda     $0360
        sec
        sbc     $0360,x
        pha
        lda     $0380
        sbc     $0380,x
        pla
        bcs     LF905
        eor     #$FF
        adc     #$01
        iny
        iny
LF905:  sta     $01
        cmp     L0000
        bcs     LF914
        pha
        lda     L0000
        sta     $01
        pla
        sta     L0000
        iny
LF914:  lda     #$00
        sta     $02
        lda     $01
        lsr     a
        lsr     a
        cmp     L0000
        bcs     LF929
        inc     $02
        asl     a
        cmp     L0000
        bcs     LF929
        inc     $02
LF929:  tya
        asl     a
        asl     a
        clc
        adc     $02
        .byte   $A8,$B9,$34,$F9,$60,$04,$05,$06
        .byte   $04,$08,$07,$06,$04,$0C,$0B,$0A
        .byte   $04,$08,$09,$0A,$04,$04,$03,$02
        .byte   $04,$00,$01,$02,$04,$0C,$0D,$0E
        .byte   $04
        brk
        .byte   $0F
        asl     $2004
        cmp     $85F8,y
        brk
        lda     $04A0,x
        clc
        adc     #$08
        and     #$0F
        sec
        sbc     L0000
        and     #$0F
        sec
        sbc     #$08
        beq     LF97D
        bcs     LF972
        inc     $04A0,x
        bne     LF975
LF972:  dec     $04A0,x
LF975:  lda     $04A0,x
        and     #$0F
        sta     $04A0,x
LF97D:  rts

LF97E:  lda     $0580,x
        bpl     LF9FE
        sty     L0000
        ldy     #$1F
        sty     $01
LF989:  lda     $0300,y
        bpl     LF9F6
        lda     $0580,y
        bpl     LF9F6
        lda     $0580,y
        and     #$04
        bne     LF9F6
        lda     $0580,y
        and     #$03
        beq     LF9F6
        and     #$01
        beq     LF9AA
        lda     $0460,x
        bpl     LF9F6
LF9AA:  jsr     LFA53
        jsr     LFA6C
        bcc     LF9B9
        lda     $0580,y
        and     #$01
        bne     LF9F6
LF9B9:  lda     $0480,y
        and     #$1F
        tay
        lda     $10
        cmp     LFB5B,y
        bcs     LF9F6
        sec
        lda     LFB3B,y
        sbc     $11
        bcc     LF9F6
        sta     $11
        cmp     #$08
        bcc     LF9D8
        lda     #$08
        sta     $11
LF9D8:  ldy     $01
        lda     $0320,y
        cmp     #$14
        bne     LF9F0
        lda     $04A0,y
        sta     $36
        lda     $0400,y
        sta     $37
        lda     $0420,y
        sta     $38
LF9F0:  sty     $5D
        ldy     L0000
        sec
        rts

LF9F6:  dec     $01
        ldy     $01
        bne     LF989
        ldy     L0000
LF9FE:  clc
        rts

LFA00:  lda     $0580,x
        bpl     LFA51
        sty     L0000
        ldy     #$1F
        sty     $01
LFA0B:  lda     $0300,y
        bpl     LFA49
        lda     $0580,y
        and     #$04
        bne     LFA49
        lda     $0580,y
        and     #$02
        beq     LFA49
        jsr     LFA53
        jsr     LFA6C
        lda     $0480,y
        and     #$1F
        tay
        lda     $11
        cmp     LFB3B,y
        bcs     LFA49
        sec
        lda     LFB5B,y
        sbc     $10
        bcc     LFA49
        sta     $10
        cmp     #$08
        bcc     LFA43
        lda     #$08
        sta     $10
LFA43:  sty     $5D
        ldy     L0000
        sec
        rts

LFA49:  dec     $01
        ldy     $01
        bne     LFA0B
        ldy     L0000
LFA51:  clc
        rts

LFA53:  lda     $0360
        sec
        sbc     $0360,y
        pha
        lda     $0380
        sbc     $0380,y
        pla
        bcs     LFA69
        eor     #$FF
        adc     #$01
        clc
LFA69:  sta     $10
        rts

LFA6C:  lda     $03C0
        sec
        sbc     $03C0,y
        bcs     LFA7A
        eor     #$FF
        adc     #$01
        clc
LFA7A:  sta     $11
        rts

LFA7D:  sec
        lda     $0360,x
        sbc     $10
        sta     $0360,x
        lda     $0380,x
        sbc     #$00
        sta     $0380,x
        jmp     LFAA2

LFA91:  clc
        lda     $0360,x
        adc     $10
        sta     $0360,x
        lda     $0380,x
        adc     #$00
        sta     $0380,x
LFAA2:  sec
        lda     $02
        sbc     $0360,x
        lda     $03
        sbc     $0380,x
        rts

LFAAE:  sec
        lda     $03C0,x
        sbc     $11
        sta     $03C0,x
        bcs     LFAD6
        dec     $03E0,x
        jmp     LFAD6

LFABF:  clc
        lda     $03C0,x
        adc     $11
        sta     $03C0,x
        bcs     LFAD3
        cmp     #$F0
        bcc     LFAD6
        adc     #$0F
        sta     $03C0,x
LFAD3:  inc     $03E0,x
LFAD6:  sec
        lda     $02
        sbc     $03C0,x
        lda     $03
        sbc     $03E0,x
        rts

        lda     $30
        cmp     #$0E
        beq     LFB3A
        cmp     #$04
        beq     LFB3A
        sec
        lda     $0580,x
        bpl     LFB3A
        and     #$04
        bne     LFB3A
        lda     $0480,x
        and     #$1F
        tay
        lda     LFB3B,y
        sta     L0000
        lda     $05C0
        cmp     #$10
        bne     LFB0F
        lda     L0000
        sec
        sbc     #$08
        sta     L0000
LFB0F:  lda     $0360
        sec
        sbc     $0360,x
        pha
        lda     $0380
        sbc     $0380,x
        pla
        bcs     LFB24
        eor     #$FF
        adc     #$01
LFB24:  cmp     LFB5B,y
        bcs     LFB3A
        lda     $03C0
        sec
        sbc     $03C0,x
        bcs     LFB36
        eor     #$FF
        adc     #$01
LFB36:  .byte   $C5,$00,$90,$00
LFB3A:  .byte   $60
LFB3B:  .byte   $13,$1C,$18,$14,$1C,$28,$16,$1C
        .byte   $18,$18,$1C,$10,$24,$24,$34,$14
        .byte   $20,$0E,$1C,$1C,$3C,$1C,$2C,$14
        .byte   $2C,$2C,$14,$34,$0C,$0C,$0C,$0C
LFB5B:  .byte   $0F,$14,$14,$14,$10,$20,$18,$14
        .byte   $10,$18,$18,$0C,$14,$20,$10,$18
        .byte   $1C,$14,$40,$0C,$0C,$0F,$0C,$10
        .byte   $28,$18,$28
        bit     $0808
        php
        php
        lda     $30
        cmp     #$0E
        beq     LFBD0
        cmp     #$04
        beq     LFBD0
        lda     #$03
        sta     $10
LFB89:  ldy     $10
        lda     $0300,y
        bpl     LFBCC
        lda     $0580,y
        bpl     LFBCC
        lda     $0320,y
        cmp     #$0F
        beq     LFBCC
        lda     $05C0,y
        cmp     #$13
        beq     LFBCC
        cmp     #$D7
        beq     LFBCC
        cmp     #$D8
        beq     LFBCC
        cmp     #$D9
        beq     LFBCC
        lda     $0580,x
        bpl     LFBCC
        and     #$04
        bne     LFBCC
        lda     $0360,y
        sta     L0000
        lda     $0380,y
        sta     $01
        lda     $03C0,y
        sta     $02
        jsr     LFBD2
        bcc     LFBD1
LFBCC:  dec     $10
        bne     LFB89
LFBD0:  sec
LFBD1:  rts

LFBD2:  sec
        lda     $0480,x
        and     #$1F
        tay
        lda     L0000
        sec
        sbc     $0360,x
        pha
        lda     $01
        sbc     $0380,x
        pla
        bcs     LFBEC
        eor     #$FF
        adc     #$01
LFBEC:  cmp     LFC23,y
        bcs     LFC02
        lda     $02
        sec
        sbc     $03C0,x
        bcs     LFBFD
        eor     #$FF
        adc     #$01
LFBFD:  cmp     LFC03,y
        .byte   $90,$00
LFC02:  .byte   $60
LFC03:  .byte   $0A,$12,$0E,$0A,$12,$1E,$0C,$16
        .byte   $0E,$0E,$12,$04,$1A,$1A,$2A,$0A
        .byte   $16,$04,$12,$2E,$32,$12,$22,$0A
        .byte   $22,$06,$02,$2A,$02,$02,$02,$02
LFC23:  .byte   $0B,$0F,$0D,$0F,$0B,$13,$13,$13
        .byte   $0B,$13,$13,$05,$0F,$1B,$0B,$13
        .byte   $17,$0F,$13,$07,$07,$0A,$07,$0B
        .byte   $23,$0F,$03
        .byte   $27
        .byte   $03
        .byte   $03
        .byte   $03
        .byte   $03
        ldx     #$1F
LFC45:  lda     $0300,x
        bpl     LFC51
        dex
        cpx     #$0F
        bne     LFC45
        sec
        rts

LFC51:  clc
        rts

LFC53:  ldy     #$1F
LFC55:  lda     $0300,y
        bpl     LFC61
        dey
        cpy     #$0F
        bne     LFC55
        sec
        rts

LFC61:  clc
        rts

        jsr     LF8C2
        sta     $0A
        lda     #$01
        bcs     LFC6E
        lda     #$02
LFC6E:  sta     L000C
        jsr     LF8B3
        sta     $0B
        lda     #$04
        bcs     LFC7B
        lda     #$08
LFC7B:  ora     L000C
        sta     L000C
        lda     $0B
        cmp     $0A
        bcs     LFCB8
        lda     $02
        sta     $0400,x
        lda     $03
        sta     $0420,x
        lda     $0A
        sta     $01
        lda     #$00
        sta     L0000
        jsr     LFD11
        lda     $04
        sta     $02
        lda     $05
        sta     $03
        lda     $0B
        sta     $01
        lda     #$00
        sta     L0000
        jsr     LFD11
        lda     $04
        sta     $0440,x
        lda     $05
        sta     $0460,x
        rts

LFCB8:  lda     $02
        sta     $0440,x
        lda     $03
        sta     $0460,x
        lda     $0B
        sta     $01
        lda     #$00
        sta     L0000
        jsr     LFD11
        lda     $04
        sta     $02
        lda     $05
        sta     $03
        lda     $0A
        sta     $01
        lda     #$00
        sta     L0000
        jsr     LFD11
        lda     $04
        sta     $0400,x
        lda     $05
        sta     $0420,x
        rts

        lda     #$00
        sta     $02
        sta     $03
        lda     L0000
        ora     $01
        bne     LFCFA
        sta     $02
        rts

LFCFA:  ldy     #$08
LFCFC:  asl     $02
        rol     L0000
        rol     $03
        sec
        lda     $03
        sbc     $01
        bcc     LFD0D
        sta     $03
        inc     $02
LFD0D:  dey
        bne     LFCFC
        rts

LFD11:  lda     #$00
        sta     $06
        sta     $07
        lda     L0000
        ora     $01
        ora     $02
        ora     $03
        bne     LFD26
        sta     $04
        sta     $05
        rts

LFD26:  stx     $09
        ldy     #$10
LFD2A:  asl     $06
        rol     L0000
        rol     $01
        rol     $07
        sec
        lda     $01
        sbc     $02
        tax
        lda     $07
        sbc     $03
        bcc     LFD44
        stx     $01
        sta     $07
        inc     $06
LFD44:  dey
        bne     LFD2A
        lda     $06
        sta     $04
        lda     L0000
        sta     $05
        ldx     $09
        rts

        lda     #$80
        sta     $5A
        lda     $F5
        pha
        lda     #$0C
        sta     $97
        jsr     LFF5B
        lda     #$00
        sta     $5A
        lda     #$18
        sta     $F4
        pla
        sta     $F5
        jmp     LFF6B

        lda     $F4
        pha
        lda     $F5
        pha
        jsr     LFF57
LFD77:  pla
        sta     $F5
        pla
        sta     $F4
        jmp     LFF6B

        lda     $F4
        pha
        lda     $F5
        pha
        jsr     LFF5B
        jmp     LFD77

        stx     $0F
        lda     $F5
        pha
        lda     #$0E
        sta     $F5
        jsr     LFF6B
        ldx     $B8
        jsr     LA006
        pla
        sta     $F5
        jsr     LFF6B
        ldx     $0F
        rts

        stx     $0F
        lda     $F5
        pha
        lda     #$0E
        sta     $F5
        jsr     LFF6B
        jsr     LA003
        .byte   $4C,$9D,$FD,$8A,$40,$A3,$00,$0F
        .byte   $04,$4B,$50,$80,$04,$18,$10,$E0
        .byte   $00,$64,$04,$C5,$45,$67,$50,$CA
        .byte   $11,$1B,$51,$BA,$00,$44,$00,$3E
        .byte   $00,$A2,$04,$99,$00,$DD,$04,$81
        .byte   $10,$2B,$11,$80,$01,$4A,$40,$9C
        .byte   $01,$47,$15,$F7,$11,$47,$44,$17
        .byte   $40,$C2,$40,$28,$11,$CB,$44,$6E
        .byte   $50,$8A,$54,$AE,$10,$2B
        brk
        .byte   $53
        ora     $5D
        ora     $78,x
        cld
        lda     #$08
        sta     $2000
        lda     #$40
        sta     $4017
        ldx     #$00
        stx     $2001
        stx     $4010
        stx     $4015
        dex
        txs
        ldx     #$04
LFE1B:  lda     $2002
        bpl     LFE1B
LFE20:  lda     $2002
        bmi     LFE20
        dex
        bne     LFE1B
        lda     $2002
        lda     #$10
        tay
LFE2E:  sta     $2006
        sta     $2006
        eor     #$10
        dey
        bne     LFE2E
        tya
LFE3A:  sta     L0000,y
        dey
        bne     LFE3A
LFE40:  inc     $01
LFE42:  sta     (L0000),y
        iny
        bne     LFE42
        ldx     $01
        cpx     #$07
        bne     LFE40
        ldy     #$07
        lda     #$88
LFE51:  sta     $DC,x
        dex
        bpl     LFE51
        lda     #$18
        sta     $FE
        lda     #$00
        sta     LA000
        ldx     #$1C
        stx     $F4
        inx
        stx     $F5
        jsr     LFF6B
        lda     #$40
        sta     $E8
        lda     #$42
        sta     $E9
        lda     #$00
        sta     $EA
        lda     #$01
        sta     $EB
        lda     #$0A
        sta     $EC
        lda     #$0B
        sta     $ED
        jsr     LFF3C
        jsr     LC5E9
        lda     #$20
        ldx     #$00
        ldy     #$00
        jsr     LC59D
        lda     #$24
        ldx     #$00
        ldy     #$00
        jsr     LC59D
        lda     #$C8
        sta     $94
        lda     #$D0
        sta     L0093
        lda     #$00
        jsr     LFEF2
        lda     #$88
        sta     $FF
LFEAA:  ldx     #$FF
        txs
LFEAD:  ldx     #$00
        stx     $90
        ldy     #$04
LFEB3:  lda     $80,x
        cmp     #$04
        bcs     LFEC3
        inx
        inx
        inx
        inx
        dey
        bne     LFEB3
        jmp     LFEAD

LFEC3:  lda     $90
        bne     LFEAD
        dey
        tya
        eor     #$03
        sta     $91
        ldy     $80,x
        lda     #$02
        sta     $80,x
        cpy     #$08
        bne     LFEE2
        lda     $82,x
        sta     L0093
        lda     $83,x
        sta     $94
        jmp     (L0093)

LFEE2:  lda     $82,x
        tax
        txs
        lda     $91
        bne     LFEED
        jsr     LC545
LFEED:  pla
        tay
        pla
        tax
        rts

LFEF2:  jsr     LFF16
        lda     L0093
        sta     $82,x
        lda     $94
        sta     $83,x
        lda     #$08
        sta     $80,x
        rts

        jsr     LFF16
        lda     #$00
        sta     $80,x
        rts

        jsr     LFF14
        lda     #$00
        sta     $80,x
        jmp     LFEAA

LFF14:  lda     $91
LFF16:  asl     a
        asl     a
        tax
        rts

LFF1A:  jsr     LFF21
        dex
        bne     LFF1A
        rts

LFF21:  lda     #$01
        sta     L0093
        txa
        pha
        tya
        pha
        jsr     LFF14
        lda     L0093
        sta     $81,x
        lda     #$01
        sta     $80,x
        txa
        tay
        tsx
        stx     $82,y
        jmp     LFEAA

LFF3C:  lda     #$FF
        sta     $1B
        rts

LFF41:  lda     $1B
        beq     LFF56
LFF45:  ldx     #$00
        stx     $1B
LFF49:  stx     L8000
        lda     $E8,x
        sta     $8001
        inx
        cpx     #$06
        bne     LFF49
LFF56:  rts

LFF57:  lda     #$04
        sta     $97
LFF5B:  jsr     LC5E9
        jsr     LF034
        lda     #$00
        sta     $EE
        jsr     LFF21
        inc     $EE
        rts

LFF6B:  inc     $F6
        lda     #$06
        sta     $F0
        sta     L8000
        lda     $F4
        sta     $F2
        sta     $8001
        lda     #$07
        sta     $F0
        sta     L8000
        lda     $F5
        sta     $F3
        sta     $8001
        dec     $F6
        lda     $F7
        bne     LFF90
        rts

LFF90:  lda     $F6
        bne     LFFCC
        lda     #$06
        sta     L8000
        lda     #$16
        sta     $8001
        lda     #$07
        sta     L8000
        lda     #$17
        sta     $8001
LFFA8:  ldx     $DB
        lda     $DC,x
        cmp     #$88
        beq     LFFC2
        pha
        lda     #$88
        sta     $DC,x
        inx
        txa
        and     #$07
        sta     $DB
        pla
        jsr     L8003
        jmp     LFFA8

LFFC2:  jsr     L8000
        lda     #$00
        sta     $F7
        .byte   $4C,$6B,$FF
LFFCC:  .byte   $E6,$F7,$60,$05,$10,$11,$04,$41
        .byte   $33,$5C,$D4,$45,$82,$00,$EF,$51
        .byte   $68,$50,$67,$10,$1C,$00,$07,$04
        .byte   $CD,$50,$00,$50,$04,$15,$96,$00
        .byte   $71,$14,$94,$15,$DD,$0E,$97,$C3
        .byte   $43,$04,$00,$00,$08,$57
        brk
        cpy     #$00
        inc     LC143,x
