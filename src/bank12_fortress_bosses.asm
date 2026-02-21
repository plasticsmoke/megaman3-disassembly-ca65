main_yellow_devil:
; =============================================================================
; MEGA MAN 3 (U) — BANK $12 — FORTRESS BOSSES + SPECIAL ENTITIES
; =============================================================================
; AI routines for Wily fortress bosses (Yellow Devil, Clone Mega Man, Wily
; Machine, Gamma) and special entities (breakable blocks, Wily capsule).
;
; Annotation: ~78% — 309 labels named, 52 inline comments
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $12 — FORTRESS BOSSES + SPECIAL ENTITIES
; =============================================================================
; Mapped to $A000-$BFFF. Contains AI routines for Wily fortress bosses
; and special entities. Dispatched from bank1C_1D for routine indices $E0-$FF.
; Known bosses: main_yellow_devil, main_wily_machine_A, main_wily_machine_B,
; main_gamma_B, main_gamma_F.
; Also serves as stage data for stage $12 (special/ending) via stage_to_bank.
;
; Annotation: partial — all 11 boss/entity entry points named, 309 auto labels remain
; =============================================================================

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

        jmp     code_A058

        jmp     code_A364

        jmp     code_A424
main_wily_machine_A:

        jmp     code_A7BA

        jmp     code_AC97
main_wily_machine_B:

        jmp     code_ABD6

        jmp     code_AD25
main_gamma_B:

        jmp     code_ADE0

        jmp     code_AE95

        jmp     code_B03B
main_gamma_F:

        jmp     code_B17B
main_teleporter:

        jmp     code_B1DE

        jmp     code_B210
main_wily_machine_C:

        nop
        nop
        rts

        jmp     code_B245

        jmp     code_B27A
main_kamegoro_maker:

        jmp     code_B664

        jmp     code_B7D8

        jmp     code_BA2F
main_kamegoro_current:

        jmp     code_BADB

        jmp     code_BC0A
main_holograph:

        jmp     code_BC7D

        jmp     code_A057

        jmp     code_A057

        jmp     code_A057

        jmp     code_B290

        jmp     code_B38A

        jmp     code_B3A7
main_giant_met:

        jmp     code_B474

code_A057:  rts

code_A058:  lda     $0300,x
        and     #$0F
        tay
        lda     LA35A,y
        sta     L0000
        lda     LA35F,y
        sta     $01
        jmp     (L0000)

; Yellow Devil init — freeze player for boss intro

        lda     #$09                    ; state → $09 (boss_wait)
        cmp     $30                     ; already in boss_wait?
        beq     LA082                   ; skip to HP fill
        sta     $30                     ; freeze player
        lda     #$80                    ; init boss HP display
        sta     $B0
        sta     $5A                     ; boss active flag
        lda     #$8E                    ; HP fill target (28 HP)
        sta     $B3
        lda     #$0D                    ; SFX $0D = boss intro music
        jsr     LF898
LA082:  lda     $B0                     ; has HP bar filled to $9C?
        cmp     #$9C
        bne     code_A0A1
        inc     $0300,x
        lda     #$02
        sta     $04A0,x
        lda     #$FF
        sta     $0500,x
        lda     #$01
        sta     $10
code_A099:  lda     $0500,x
        beq     code_A0A2
        dec     $0500,x
code_A0A1:  rts

code_A0A2:  jsr     LFC53
        stx     L0000
        lda     $0520,x
        sta     $0500,y
        tax
        lda     LA59D,x
        sta     $03C0,y
        lda     $10
        and     #$01
        bne     code_A0C6
        lda     LA5E5,x
        sta     $0360,y
        lda     #$71
        sta     $01
        bne     code_A0CF
code_A0C6:  lda     #$04
        sta     $0360,y
        lda     #$7E
        sta     $01
code_A0CF:  lda     #$80
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
        bne     code_A10E
        txa
        clc
        adc     #$18
        tax
code_A10E:  lda     LA601,x
        ldx     L0000
        sta     $0500,x
        inc     $0520,x
        lda     $0500,x
        bne     code_A13D
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
code_A13D:  rts

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
        bne     code_A1EA
        lda     $05A0,x
        cmp     #$02
        bne     code_A1F5
        jsr     LFC53
        bcs     code_A1F5
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
        bne     code_A1EA
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

code_A1EA:  dec     $0500,x
        lda     $05A0,x
        bne     code_A1F5
        sta     $05E0,x
code_A1F5:  jsr     L8003
        lda     $04E0,x
        bne     code_A20C
        lda     #$0F
        ldy     #$03
code_A201:  sta     $0608,y
        sta     $0628,y
        dey
        bpl     code_A201
        sty     $18
code_A20C:  rts

        lda     $0500,x
        bne     code_A25C
        lda     $0580,x
        ora     #$04
        sta     $0580,x
        lda     #$38
        sta     $0360,x
        lda     $19
        beq     code_A228
        inc     $0500,x
        bne     code_A25C
code_A228:  ldy     $0520,x
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
code_A25C:  lda     $05A0,x
        bne     code_A264
        sta     $05E0,x
code_A264:  lda     #$02
        sta     $10
        jmp     code_A099

        lda     $0500,x
        beq     code_A27C
        dec     $0500,x
        lda     $05A0,x
        bne     code_A27B
        sta     $05E0,x
code_A27B:  rts

code_A27C:  lda     $0580,x
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
code_A2A7:  sta     $0783,x
        sta     $0790,x
        dex
        bpl     code_A2A7
        stx     $079A
        stx     $1A
        lda     #$98
        sta     $01
        lda     LA6FB,y
        sta     $02
        lda     #$00
        sta     $03
        sty     $04
code_A2C4:  jsr     LFC53
        bcs     code_A324
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
        bne     code_A31E
        ldx     $04
        lda     LA700,x
        sta     $0540,y
        lda     LA705,x
        sta     $0520,y
code_A31E:  inc     $03
        cmp     #$48
        bne     code_A2C4
code_A324:  ldx     L0000
        lda     #$14
        sta     $0500,x
        inc     $0520,x
        lda     $0520,x
        cmp     #$05
        bne     code_A359
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
code_A359:  rts

LA35A:  .byte   $6B,$95,$3E,$0D,$6B
LA35F:  .byte   $A0,$A0,$A1,$A2,$A2
code_A364:  lda     $05C0,x
        cmp     #$71
        bne     code_A378
        lda     $05A0,x
        cmp     #$02
        bne     code_A377
        lda     #$7E
        jsr     LF835
code_A377:  rts

code_A378:  lda     $04A0,x
        and     #$02
        tay
        lda     LA5FD,y
        clc
        adc     $0500,x
        tay
        lda     $0300,x
        and     #$0F
        bne     code_A3AD
        lda     $04A0,x
        and     #$01
        beq     code_A39A
        jsr     LF71D
        jmp     code_A39D

code_A39A:  jsr     LF73B
code_A39D:  lda     LA5B5,y
        cmp     $0360,x
        bne     code_A423
        inc     $0300,x
        lda     #$7F
        jsr     LF835
code_A3AD:  lda     $05A0,x
        cmp     #$03
        bne     code_A423
        lda     $19
        bne     code_A41E
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
        bne     code_A41E
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
code_A41E:  lda     #$00
        sta     $05E0,x
code_A423:  rts

code_A424:  lda     $05C0,x
        cmp     #$7E
        beq     code_A43B
        cmp     #$7F
        beq     code_A43E
        lda     $05A0,x
        cmp     #$02
        bne     code_A423
        lda     #$7E
        jmp     LF835

code_A43B:  jmp     code_A4BB

code_A43E:  lda     #$3C
        sta     $051F
        lda     $05A0,x
        cmp     #$03
        bne     code_A423
        lda     $19
        bne     code_A4B5
        ldy     $0500,x
        lda     LA70F,y
        bmi     code_A4AF
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
        bne     code_A4AF
        lda     $059F
        and     #$FB
        sta     $059F
        lda     #$00
        sta     $051F
        lda     #$70
        sta     $05DF
code_A4AF:  lda     #$00
        sta     $0300,x
        rts

code_A4B5:  lda     #$00
        sta     $05E0,x
        rts

code_A4BB:  lda     $0500,x
        beq     code_A4D0
        jsr     LF759
        lda     $03C0,x
        cmp     #$98
        bne     code_A4CF
        lda     #$00
        sta     $0300,x
code_A4CF:  rts

code_A4D0:  lda     $0520,x
        beq     code_A50E
        dec     $0520,x
        bne     code_A4CF
        lda     #$02
        sta     $0560,x
code_A4DF:  dec     $0560,x
        bmi     code_A4F9
        lda     #$A3
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        lda     #$BD
        sta     $0400,x
        lda     #$01
        sta     $0420,x
        rts

code_A4F9:  lda     #$87
        sta     $0440,x
        lda     #$06
        sta     $0460,x
        lda     #$72
        sta     $0400,x
        lda     #$02
        sta     $0420,x
        rts

code_A50E:  lda     $0300,x
        and     #$0F
        bne     code_A551
        jsr     LF797
        lda     #$98
        cmp     $03C0,x
        bcs     code_A524
        sta     $03C0,x
        bcc     code_A4DF
code_A524:  jsr     LF71D
        lda     $0540,x
        cmp     $0360,x
        bcs     code_A4CF
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
code_A551:  lda     $03C0,x
        and     #$0F
        cmp     #$08
        bne     code_A598
        jsr     LFC53
        bcs     code_A597
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
        bne     code_A598
        lda     #$00
        sta     $0300,x
code_A597:  rts

code_A598:  jmp     LF759

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
code_A7BA:  lda     #$AB
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
        beq     code_A7FD
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
code_A7F4:  lda     $0377,y
        sta     $0577,y
        dey
        bpl     code_A7F4
code_A7FD:  lda     $B0
        cmp     #$9C
        bne     code_A81C
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
code_A81C:  rts

        ldy     $0500,x
        lda     LAD39,y
        sta     L0000
        lda     LAD43,y
        sta     $01
        jmp     (L0000)

        lda     $5E
        cmp     #$5A
        beq     code_A861
        lda     $5E
        clc
        adc     #$02
        sta     $5E
        cmp     #$5A
        bne     code_A843
        lda     #$1E
        sta     $0520,x
code_A843:  lda     $03DA
        pha
        lda     $03DB
        pha
        ldy     #$08
code_A84D:  lda     $03D7,y
        clc
        adc     #$02
        sta     $03D7,y
        dey
        bpl     code_A84D
        pla
        sta     $03DB
        pla
        sta     $03DA
code_A861:  dec     $0520,x
        bne     code_A869
        inc     $0500,x
code_A869:  jmp     code_AA0A

        lda     $03DA
        pha
        lda     $03DB
        pha
        ldy     #$08
code_A876:  lda     $03D7,y
        sec
        sbc     #$02
        sta     $03D7,y
        dey
        bpl     code_A876
        pla
        sta     $03DB
        pla
        sta     $03DA
        lda     $5E
        sec
        sbc     #$02
        sta     $5E
        cmp     #$3A
        bne     code_A8A0
        lda     #$40
        sta     $0520,x
        lda     $053E
        sta     $0500,x
code_A8A0:  jmp     code_AA0A

        lda     $0520,x
        and     #$01
        bne     code_A8B3
        dec     $057D
        dec     $0579
        dec     $057B
code_A8B3:  dec     $03DB
        lda     $0520,x
        cmp     #$20
        bne     code_A8C3
        inc     $05BD
        inc     $05B9
code_A8C3:  jmp     code_A9B5

        lda     $0520,x
        and     #$01
        bne     code_A8EC
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
code_A8EC:  inc     $03DB
        lda     $0520,x
        cmp     #$20
        bne     code_A902
        dec     $05BD
        dec     $05B9
        inc     $05BC
        inc     $05B8
code_A902:  jmp     code_A9B5

        lda     $0520,x
        and     #$01
        bne     code_A915
        dec     $057C
        dec     $0578
        dec     $057A
code_A915:  dec     $03DA
        lda     $0520,x
        cmp     #$20
        bne     code_A925
        dec     $05BC
        dec     $05B8
code_A925:  jmp     code_A9B5

        inc     $03DA
        jmp     code_A9B5

        lda     $0520,x
        and     #$01
        bne     code_A93E
        inc     $057C
        inc     $0578
        inc     $057A
code_A93E:  dec     $03DA
        lda     $0520,x
        cmp     #$20
        bne     code_A9B5
        inc     $05BC
        inc     $05B8
        jmp     code_A9B5

        lda     $0520,x
        and     #$01
        bne     code_A977
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
code_A977:  inc     $03DA
        lda     $0520,x
        cmp     #$20
        bne     code_A9B5
        dec     $05BC
        dec     $05B8
        inc     $05BD
        inc     $05B9
        bne     code_A9B5
        lda     $0520,x
        and     #$01
        bne     code_A99F
        inc     $057D
        inc     $0579
        inc     $057B
code_A99F:  dec     $03DB
        lda     $0520,x
        cmp     #$20
        bne     code_A9B5
        dec     $05BD
        dec     $05B9
        jmp     code_A9B5

        inc     $03DB
code_A9B5:  dec     $0520,x
        bne     code_AA0A
        lda     #$40
        sta     $0520,x
        inc     $0500,x
        lda     $0500,x
        and     #$07
        sta     $0500,x
        beq     code_A9E6
        cmp     #$04
        bne     code_AA0A
        lda     $6A
        sec
        sbc     #$40
        sta     L0000
        lda     $6B
        sbc     #$01
        ora     L0000
        beq     code_A9FA
        lda     #$00
        sta     $0500,x
        beq     code_A9FA
code_A9E6:  lda     $6A
        sec
        sbc     #$C0
        sta     L0000
        lda     $6B
        sbc     #$00
        ora     L0000
        beq     code_A9FA
        lda     #$04
        sta     $0500,x
code_A9FA:  lda     $031E
        bmi     code_AA0A
        lda     $0500,x
        sta     $053E
        lda     #$08
        sta     $0500,x
code_AA0A:  lda     $031E
        bpl     code_AA7F
        lda     $059E
        and     #$04
        bne     code_AA7E
        lda     $0540,x
        bne     code_AA7B
        jsr     LF8C2
        cmp     #$48
        bcc     code_AA7E
        jsr     LFC53
        bcs     code_AA7E
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
code_AA7B:  dec     $0540,x
code_AA7E:  rts

code_AA7F:  lda     $0540,x
        bne     code_AA7B
        ldy     #$16
code_AA86:  lda     $0300,y
        bpl     code_AA91
        dey
        cpy     #$0F
        bne     code_AA86
        rts

code_AA91:  lda     #$58
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
        bne     code_AB03
        lda     #$00
        sta     $051E
code_AB03:  rts

        ldy     #$08
code_AB06:  lda     $0597,y
        ora     #$04
        sta     $0597,y
        lda     $0577,y
        sec
        sbc     $6A
        sta     L0000
        lda     #$01
        sbc     $6B
        bne     code_AB29
        lda     L0000
        sta     $0377,y
        lda     $0597,y
        and     #$FB
        sta     $0597,y
code_AB29:  dey
        bpl     code_AB06
        lda     $95
        and     #$01
        tay
        lda     $059D
        and     #$04
        bne     code_AB44
        lda     LAD8A,y
        sta     $059D
        lda     LAD8C,y
        sta     $0599
code_AB44:  lda     $059C
        and     #$04
        bne     code_AB5B
        lda     LAD8A,y
        ora     #$40
        sta     $059C
        lda     LAD8C,y
        ora     #$40
        sta     $0598
code_AB5B:  lda     $031E
        bpl     code_AB68
        lda     $059F
        ora     #$04
        sta     $059F
code_AB68:  lda     #$00
        sta     $05FD
        sta     $05FC
        sta     $05F9
        sta     $05F8
        lda     $057F
        sec
        sbc     $6A
        lda     #$01
        sbc     $6B
        bne     code_ABA5
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
code_ABA5:  lda     $059F
        and     #$04
        bne     code_ABD5
        lda     #$02
        sta     $049F
        jsr     L8003
        lda     $04E0,x
        bne     code_ABD5
        lda     #$6D
        sta     $05D0
        lda     #$00
        sta     $05B0
        sta     $05F0
        ldy     #$0B
        lda     #$0F
code_ABCA:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     code_ABCA
        sty     $18
code_ABD5:  rts

code_ABD6:  lda     $B3
        bpl     code_ABD5
        lda     $30
        cmp     #$09
        beq     code_ABD5
        lda     $04E0,x
        beq     code_AC19
        jsr     L8003
        lda     $04E0,x
        ora     #$80
        sta     $B0
        and     #$1F
        bne     code_AC4E
        lda     #$E5
        sta     $0320,x
        lda     #$94
        sta     $0580,x
        lda     #$00
        sta     $0500,x
        sta     $05C0,x
        ldy     #$00
code_AC07:  lda     LAD4D,y
        sta     $0780,y
        cmp     #$FF
        beq     code_AC14
        iny
        bne     code_AC07
code_AC14:  sta     $19
        jmp     code_AC4F

code_AC19:  lda     $0500,x
        and     #$03
        bne     code_AC4B
        lda     #$1C
        jsr     LF89A
        inc     $B0
        lda     $B0
        cmp     #$9C
        bne     code_AC4B
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
code_AC4B:  inc     $0500,x
code_AC4E:  rts

code_AC4F:  lda     #$02
        sta     $01
        lda     $0360,x
        sta     $02
        lda     $03C0,x
        sta     $03
code_AC5D:  jsr     LFC53
        bcs     code_AC96
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
        bpl     code_AC5D
code_AC96:  rts

code_AC97:  lda     $04A0,x
        and     #$01
        beq     code_ACA4
        jsr     LF71D
        jmp     code_ACA7

code_ACA4:  jsr     LF73B
code_ACA7:  lda     $04A0,x
        and     #$08
        beq     code_ACB4
        jsr     LF779
        jmp     code_ACB7

code_ACB4:  jsr     LF759
code_ACB7:  ldy     $0500,x
        lda     #$00
        sta     L0000
        sta     $01
        lda     LAD9E,y
        bpl     code_ACC7
        dec     L0000
code_ACC7:  lda     $03A0,x
        clc
        adc     LAD8E,y
        sta     $03A0,x
        lda     $03C0,x
        adc     LAD9E,y
        sta     $03C0,x
        lda     $03E0,x
        adc     L0000
        beq     code_ACE7
        lda     #$00
        sta     $0300,x
        rts

code_ACE7:  lda     LADBE,y
        bpl     code_ACEE
        dec     $01
code_ACEE:  lda     $0340,x
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
        bne     code_AD24
        inc     $0500,x
        lda     $0500,x
        and     #$0F
        sta     $0500,x
        bne     code_AD1E
        inc     $0540,x
code_AD1E:  lda     $0540,x
        sta     $0520,x
code_AD24:  rts

code_AD25:  jsr     LF797
        lda     $04A0,x
        and     #$01
        beq     code_AD32
        jmp     LF71D

code_AD32:  jmp     LF73B

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

; Gamma init phase — same boss_wait pattern as all other bosses
code_ADE0:  lda     $0300,x             ; AI phase
        and     #$0F
        bne     code_AE17               ; skip init if not phase 0
        sta     $95
        inc     $0300,x                 ; advance to phase 1
        lda     #$09                    ; state → $09 (boss_wait)
        sta     $30                     ; freeze player for HP fill
        lda     #$80                    ; init boss HP display
        sta     $B0
        sta     $5A                     ; boss active flag
        lda     #$8E                    ; HP fill target (28 HP)
        sta     $B3
        lda     #$0D                    ; SFX $0D = boss intro music
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
code_AE17:  lda     $0500,x
        bmi     code_AE5F
        lda     #$00
        lda     $95
        and     #$0F
        bne     code_AE5B
        ldy     #$0B
code_AE26:  lda     LAE81,y
        sec
        sbc     $0500,x
        bcs     code_AE31
        lda     #$0F
code_AE31:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     code_AE26
        ldy     #$07
code_AE3C:  lda     LAE8D,y
        sec
        sbc     $0500,x
        bcs     code_AE47
        lda     #$0F
code_AE47:  sta     $0618,y
        sta     $0638,y
        dey
        bpl     code_AE3C
        sty     $18
        lda     $0500,x
        sec
        sbc     #$10
        sta     $0500,x
code_AE5B:  lda     #$80
        sta     $B0

; Wait for boss HP bar to fill, then release player
code_AE5F:  lda     $B0                 ; HP bar position
        cmp     #$9C                    ; filled to max?
        bne     code_AE7B               ; no → keep filling
        lda     #$00                    ; state → $00 (on_ground)
        sta     $30                     ; release player, fight begins
        sta     $0500,x
        lda     #$C0
        sta     $0300,x
        lda     #$E8
        sta     $0320,x
        lda     #$1C
        sta     $04E0,x
code_AE7B:  lda     #$00
        sta     $05E0,x
        rts

LAE81:  .byte   $0F,$30,$16,$04,$0F,$30,$11,$01
        .byte   $0F,$30,$36,$26
LAE8D:  .byte   $0F,$01,$30,$11,$0F,$0F,$30,$10
code_AE95:  lda     $0500,x
        bne     code_AEAC
        jsr     LF8C2
        cmp     #$50
        bcs     code_AEAF
        jsr     LF869
        jsr     code_AFD2
        lda     #$1F
        sta     $0500,x
code_AEAC:  dec     $0500,x
code_AEAF:  lda     $0540,x
        bne     code_AEBB
        jsr     LF8B3
        cmp     #$30
        bcs     code_AEE3
code_AEBB:  lda     $0520,x
        bne     code_AEE0
        lda     #$02
        sta     $01
        jsr     code_AF76
        lda     #$1F
        sta     $0520,x
        inc     $0540,x
        lda     $0540,x
        cmp     #$03
        bcc     code_AEE0
        lda     #$79
        sta     $0520,x
        lda     #$00
        sta     $0540,x
code_AEE0:  dec     $0520,x
code_AEE3:  lda     $04E0,x
        cmp     #$0F
        bcc     code_AEEB
        rts

code_AEEB:  ldy     #$17
code_AEED:  cpy     #$10
        bcs     code_AEF5
        lda     #$7A
        bne     code_AEF7
code_AEF5:  lda     #$5B
code_AEF7:  jsr     LF846
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
        bne     code_AEED

; Gamma phase transition — scroll screen vertically
        lda     #$10                    ; state → $10 (screen_scroll)
        sta     $30                     ; player frozen during scroll
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

code_AF76:  jsr     LFC53
        bcs     code_AFCD
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
        bne     code_AF76
code_AFCD:  rts

LAFCE:  .byte   $0F
LAFCF:  .byte   $00,$F1,$FF
code_AFD2:  jsr     LFC53
        bcs     code_B02E
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
code_B018:  cmp     LB02F,x
        bcc     code_B020
        dex
        bne     code_B018
code_B020:  lda     LB033,x
        sta     $0400,y
        lda     LB037,x
        sta     $0420,y
        ldx     L0000
code_B02E:  rts

LB02F:  .byte   $4C,$3D,$2E,$1F
LB033:  .byte   $00,$80,$00,$80
LB037:  .byte   $02,$01,$01,$00
code_B03B:  lda     $0500,x
        bne     code_B08E
        jsr     LFC53
        bcs     code_B091
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
code_B08E:  dec     $0500,x
code_B091:  lda     $0520,x
        bne     code_B108
        lda     $0540,x
        and     #$01
        bne     code_B0E1
        lda     $69
        clc
        adc     $0400,x
        sta     $69
        lda     $6A
        adc     $0420,x
        sta     $6A
        cmp     #$A0
        bcs     code_B0CC
        lda     $0400,x
        clc
        adc     #$10
        sta     $0400,x
        lda     $0420,x
        adc     #$00
        sta     $0420,x
        cmp     #$03
        bne     code_B10B
        lda     #$00
        sta     $0400,x
        beq     code_B10B
code_B0CC:  lda     #$A0
        sta     $6A
        lda     #$80
        sta     $0400,x
        lda     #$01
        sta     $0420,x
        lda     #$01
        sta     $0540,x
        bne     code_B10B
code_B0E1:  lda     $69
        sec
        sbc     $0400,x
        sta     $69
        lda     $6A
        sbc     $0420,x
        sta     $6A
        bcs     code_B10B
        lda     #$00
        sta     $69
        sta     $6A
        sta     $0400,x
        sta     $0420,x
        lda     #$F1
        sta     $0520,x
        lda     #$02
        sta     $0540,x
code_B108:  dec     $0520,x
code_B10B:  jsr     L8003
        lda     $04E0,x
        bne     code_B166
        sta     $6A
        sta     $6B
        ldy     #$0B
        lda     #$0F
code_B11B:  sta     $0604,y
        sta     $0624,y
        dey
        bpl     code_B11B
        sty     $18
        ldy     #$00
code_B128:  lda     LB167,y
        sta     $0780,y
        cmp     #$FF
        beq     code_B135
        iny
        bne     code_B128
code_B135:  sta     $19
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
code_B166:  rts

LB167:  .byte   $23,$C0,$0F,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$55,$55,$55,$55,$55
        .byte   $55,$55,$55,$FF
code_B17B:  lda     $0580,x
        and     #$04
        bne     code_B1BF
        lda     $30
        cmp     #$06
        beq     code_B1BF
        cmp     #$0E
        beq     code_B1BF
        lda     $03C0
        pha
        inc     $03C0
        jsr     LFAE2
        bcs     code_B1A7
        lda     $041F
        sta     $37
        lda     $043F
        sta     $38
        lda     $055F
        sta     $36
code_B1A7:  pla
        sta     $03C0

; Gamma/fortress hazard — instant kill on contact
        lda     $04C0,x                 ; entity sub-type
        cmp     #$0D                    ; $0D = ??? skip
        beq     code_B1BF
        lda     $39                     ; i-frames active?
        bne     code_B1BF               ; skip
        jsr     LFAE2                   ; AABB collision test
        bcs     code_B1BF               ; no collision → skip
        lda     #$0E                    ; state → $0E (death)
        sta     $30                     ; instant kill, no damage calc
code_B1BF:  lda     $0580,x
        .byte   $09
LB1C3:  .byte   $04
        sta     $0580,x
        lda     LB1C3,x
        sec
        sbc     $6A
        bcs     code_B1DA
        sta     $0360,x
        lda     $0580,x
        and     #$FB
        sta     $0580,x
code_B1DA:  rts

        .byte   $80,$40,$10

; ---------------------------------------------------------------------------
; Teleporter tube — player steps in, warps to boss refight room
; Sets state $11 (warp_init), which transitions to $12 (warp_anim),
; then back to $00 (on_ground) in the destination room.
; $6C = destination index (from entity sub-type).
; ---------------------------------------------------------------------------
code_B1DE:  jsr     LFAE2               ; is player touching teleporter?
        bcs     code_B20F               ; no → return
        jsr     LF8C2                   ; detailed alignment check
        cmp     #$02                    ; close enough to center?
        bcs     code_B20F               ; no → return
        lda     $0360,x                 ; snap player X to teleporter X
        sta     $0360
        lda     $04C0,x                 ; entity sub-type - $0E =
        sbc     #$0E                    ; destination index
        cmp     #$01
        beq     code_B20F               ; destination 1 = invalid? skip
        sta     $6C                     ; $6C = warp destination
        lda     #$11                    ; state → $11 (warp_init)
        sta     $30                     ; begin teleporter sequence
        lda     #$13                    ; player OAM $13 = teleport beam
        sta     $05C0
        lda     #$00
        sta     $05E0                   ; reset animation counter
        sta     $05A0                   ; reset animation frame
        sta     $0300,x                 ; despawn teleporter entity
code_B20F:  rts

code_B210:  lda     $03C0,x
        cmp     #$68
        beq     code_B21F
        inc     $0360,x
        lda     #$01
        jmp     code_B224

code_B21F:  dec     $0360,x
        lda     #$02
code_B224:  sta     $04A0,x
        lda     $03C0,x
        pha
        dec     $03C0,x
        jsr     LFAE2
        pla
        sta     $03C0,x
        bcs     code_B244
        lda     $04A0,x
        sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
code_B244:  rts

code_B245:  lda     $03C0,x
        cmp     #$A8
        beq     code_B265
        clc
        adc     #$04
        sta     $03C0,x
        cmp     #$A8
        bne     code_B279
        lda     #$6C
        cmp     $05C0,x
        beq     code_B265
        jsr     LF835
        lda     #$10
        sta     $0500,x
code_B265:  lda     $05C0,x
        cmp     #$6E
        bne     code_B279
        lda     $0500,x
        beq     code_B279
        dec     $0500,x
        lda     #$00
        sta     $05E0,x
code_B279:  rts

code_B27A:  ldy     #$08
        jsr     LF67C
        bcs     code_B285
        inc     $0360,x
        rts

code_B285:  lda     #$6C
        cmp     $05C0,x
        beq     code_B28F
        jsr     LF835
code_B28F:  rts

code_B290:  jsr     LF797
        lda     #$B0
        cmp     $03C0,x
        bcs     code_B28F
        sta     $03C0,x
        lda     $0500,x
        cmp     #$02
        beq     code_B301
        bcs     code_B307
        lda     #$00
        sta     $0300,x
code_B2AB:  lda     #$03
        sta     L0000
code_B2AF:  jsr     LFC53
        bcs     code_B28F
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
        bpl     code_B2AF
        rts

code_B301:  lda     #$00
        sta     $0310
        rts

code_B307:  lda     $0580
        ora     #$04
        sta     $0580
        lda     $0520,x
        cmp     #$3C
        bne     code_B32B
        lda     #$79
        jsr     LF835
        stx     $0560
        lda     #$00
        sta     $0320,x
        lda     #$B4
        sta     $03C0,x
        jmp     code_B2AB

code_B32B:  inc     $0520,x
        bne     code_B379
        jsr     LFC53
        bcs     code_B379
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
code_B36B:  lda     LB46C,y
        sta     $0610,y
        sta     $0630,y
        dey
        bpl     code_B36B
        sty     $18
code_B379:  rts

LB37A:  .byte   $A8,$A8,$B8,$B8
LB37E:  .byte   $F8,$08,$F8,$08
LB382:  .byte   $02,$01,$02,$01
LB386:  .byte   $02,$02,$01,$01
code_B38A:  jsr     LF797
        lda     $03C0,x
        cmp     #$B8
        bcc     code_B39A
        lda     #$00
        sta     $0300,x
        rts

code_B39A:  lda     $04A0,x
        and     #$01
        beq     code_B3A4
        jmp     LF71D

code_B3A4:  jmp     LF73B

code_B3A7:  jsr     LF797
        lda     $04A0,x
        and     #$02
        beq     code_B3DA
        lda     #$B4
        cmp     $03C0,x
        bcs     code_B41D
        lda     #$00
        sta     $0300,x
        lda     #$81
        sta     $0300
        lda     #$00
        ldy     $0560
        sta     $0300,y
        sta     $0500                   ; clear player timer
        lda     #$0D                    ; state → $0D (teleport)
        sta     $30                     ; player teleports away (end stage)
        lda     $0580
        and     #$FB
        sta     $0580
        rts

code_B3DA:  lda     #$94
        cmp     $03C0,x
        bcs     code_B418
        sta     $03C0,x
        lda     $05C0,x
        cmp     #$7A
        beq     code_B42A
        cmp     #$7B
        bne     code_B3F8
        lda     #$7A
        jsr     LF835
        lda     #$00
        sta     $B8
code_B3F8:  lda     $05A0,x
        cmp     #$04
        bne     code_B46B
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
code_B418:  lda     #$00
        sta     $05E0,x
code_B41D:  lda     $04A0,x
        and     #$01
        beq     code_B427
        jmp     LF71D

code_B427:  jmp     LF73B

code_B42A:  inc     $0500,x
        lda     $0500,x
        cmp     #$3C
        bne     code_B46B
        lda     $B8
        bne     code_B44A
        dec     $0500,x
        lda     #$11
        sta     $F8
        lda     #$D0
        sta     $5E
        lda     #$0A
        sta     $B8
        jmp     LFD8C

code_B44A:  jsr     LFDA6
        lda     $B8
        cmp     #$FF
        beq     code_B457
        dec     $0500,x
        rts

code_B457:  lda     #$A3
        sta     $0440,x
        lda     #$04
        sta     $0460,x
        lda     #$7B
        jsr     LF835
        lda     #$02
        sta     $04A0,x
code_B46B:  rts

LB46C:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
code_B474:  lda     $0480,x
        pha
        lda     #$00
        sta     $0480,x
        jsr     L8003
        pla
        sta     $0480,x
        bcs     code_B4E5
        lda     $04E0,x
        bne     code_B4E5
        sta     $F8
        sta     $0480,x
        lda     #$02
        sta     $01
        lda     $0360,x
        sta     $02
        lda     $03C0,x
        sta     $03
code_B49E:  jsr     LFC53
        bcs     code_B4E4
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
        bpl     code_B49E
        ldy     #$0F
code_B4D9:  lda     LB654,y
        sta     $0600,y
        dey
        bpl     code_B4D9
        sty     $18
code_B4E4:  rts

code_B4E5:  lda     $F8
        cmp     #$03
        bcc     code_B4E4
        lda     $0520,x
        beq     code_B50A
        dec     $0520,x
        bne     code_B507
        lda     $0540,x
        beq     code_B507
        dec     $0540,x
        beq     code_B507
        lda     #$1E
        sta     $0520,x
        jmp     code_B5FB

code_B507:  jmp     code_B595

code_B50A:  lda     $0300,x
        and     #$0F
        bne     code_B526
        inc     $0300,x
        sta     $0460,x
        lda     #$80
        sta     $0440,x
        lda     #$88
        sta     $04A0,x
        lda     #$F0
        sta     $0500,x
code_B526:  lda     $0500,x
        bne     code_B548
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
code_B548:  lda     $04A0,x
        and     #$04
        bne     code_B55E
        jsr     LF779
        lda     #$48
        cmp     $03C0,x
        bcc     code_B573
        sta     $03C0,x
        bcs     code_B56B
code_B55E:  jsr     LF759
        lda     #$80
        cmp     $03C0,x
        bcs     code_B573
        sta     $03C0,x
code_B56B:  lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
code_B573:  dec     $0500,x
        bne     code_B595
        lda     #$1E
        sta     $0520,x
        lda     $E4
        adc     $E6
        sta     $E4
        and     #$01
        beq     code_B58D
        jsr     code_B5A1
        jmp     code_B595

code_B58D:  lda     #$03
        sta     $0540,x
        jsr     code_B5FB
code_B595:  lda     $03C0,x
        sec
        sbc     #$D0
        clc
        adc     #$AF
        sta     $5E
        rts

code_B5A1:  lda     #$02
        sta     $01
code_B5A5:  jsr     LFC53
        bcs     code_B5FA
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
        bpl     code_B5A5
code_B5FA:  rts

code_B5FB:  jsr     LFC53
        bcs     code_B5FA
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
code_B664:  lda     $0300,x
        and     #$0F
        bne     code_B68E
        lda     #$09
        cmp     $30
        beq     code_B682
        sta     $30
        lda     #$80
        sta     $B0
        sta     $5A
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
code_B682:  lda     $B0
        cmp     #$9C
        bne     code_B6EC
        jsr     code_B732
        inc     $0300,x
code_B68E:  lda     $0300,x
        and     #$02
        bne     code_B6EC
        lda     $05C0,x
        cmp     #$4F
        beq     code_B6B2
        lda     $0520,x
        bne     code_B6ED
        lda     $0540,x
        cmp     #$05
        bcs     code_B6E9
        lda     $0500,x
        bne     code_B6C7
        lda     #$4F
        jsr     LF835
code_B6B2:  lda     $05A0,x
        cmp     #$02
        bne     code_B6EC
        jsr     code_B756
        inc     $0540,x
        lda     #$31
        jsr     LF835
        inc     $0520,x
code_B6C7:  lda     $04A0,x
        and     #$01
        beq     code_B6D6
        ldy     #$20
        jsr     LF580
        jmp     code_B6DB

code_B6D6:  ldy     #$21
        jsr     LF5C4
code_B6DB:  bcc     code_B6E5
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
code_B6E5:  dec     $0500,x
        rts

code_B6E9:  inc     $0300,x
code_B6EC:  rts

code_B6ED:  lda     #$00
        sta     L0000
        lda     #$80
        sta     $01
        ldy     #$1F
code_B6F7:  lda     $0300,y
        bmi     code_B726
code_B6FC:  dey
        cpy     #$0F
        bne     code_B6F7
        lda     L0000
        bne     code_B71D
        lda     #$00
        sta     $0520,x
        lda     $0540,x
        tay
        lda     $B0
        sec
        sbc     LB750,y
        sta     $B0
        and     #$1F
        bne     code_B71D
        jmp     L8006

code_B71D:  lda     #$31
        jsr     LF835
        jsr     code_B732
        rts

code_B726:  lda     $01
        cmp     $04C0,y
        bne     code_B6FC
        inc     L0000
        jmp     code_B6FC

code_B732:  lda     $E4
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
code_B756:  jsr     LFC53
        bcs     code_B7C5
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
        bne     code_B7C5
        lda     $0580,y
        and     #$BF
        sta     $0580,y
code_B7C5:  rts

LB7C6:  .byte   $06,$05,$05,$06,$05,$06
LB7CC:  .byte   $80,$00,$80,$00,$00,$00
LB7D2:  .byte   $00,$01,$01,$02,$03,$04
code_B7D8:  lda     $0300,x
        and     #$0F
        bne     code_B7EA
        sta     $0520,x
        lda     #$78
        sta     $0500,x
        inc     $0300,x
code_B7EA:  lda     $0300,x
        and     #$02
        beq     code_B7F4
        jmp     code_B8A3

code_B7F4:  jsr     code_B94A
        lda     $0500,x
        bne     code_B815
        lda     $03C0,x
        cmp     #$68
        bcs     code_B80A
        lda     #$78
        sta     $0500,x
        bne     code_B819
code_B80A:  jsr     code_B9EC
        jsr     code_B989
        lda     #$FF
        sta     $0500,x
code_B815:  cmp     #$FF
        beq     code_B81C
code_B819:  dec     $0500,x
code_B81C:  lda     $04A0,x
        and     #$01
        beq     code_B82B
        ldy     #$0C
        jsr     LF580
        jmp     code_B830

code_B82B:  ldy     #$0D
        jsr     LF5C4
code_B830:  bcc     code_B847
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        and     #$0C
        bne     code_B8A2
        lda     $04A0,x
        ora     #$08
        sta     $04A0,x
        rts

code_B847:  lda     $04A0,x
        and     #$0C
        beq     code_B8A2
        and     #$04
        beq     code_B86A
        lda     $0500,x
        cmp     #$FF
        beq     code_B85D
        lda     #$5E
        bne     code_B85F
code_B85D:  lda     #$62
code_B85F:  sta     $05C0,x
        ldy     #$0E
        jsr     LF606
        jmp     code_B898

code_B86A:  lda     $04A0,x
        and     #$01
        beq     code_B87B
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        bne     code_B883
code_B87B:  lda     $0580,x
        ora     #$40
        sta     $0580,x
code_B883:  lda     $0500,x
        cmp     #$FF
        beq     code_B88E
        lda     #$60
        bne     code_B890
code_B88E:  lda     #$64
code_B890:  sta     $05C0,x
        ldy     #$0F
        jsr     LF642
code_B898:  bcc     code_B8A2
        lda     $04A0,x
        eor     #$0C
        sta     $04A0,x
code_B8A2:  rts

code_B8A3:  lda     $0560,x
        bne     code_B8C7
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
code_B8C7:  ldy     #$0F
        jsr     LF67C
        lda     $10
        and     #$10
        beq     code_B8D5
        jmp     code_B8FB

code_B8D5:  lda     $04A0,x
        and     #$01
        beq     code_B8EC
        ldy     #$0C
        jsr     LF580
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        jmp     code_B8F9

code_B8EC:  ldy     #$0D
        jsr     LF5C4
        lda     $0580,x
        ora     #$40
        sta     $0580,x
code_B8F9:  bcc     code_B925
code_B8FB:  dec     $0300,x
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

code_B925:  lda     $0460,x
        bpl     code_B931
        lda     $03C0,x
        cmp     #$20
        bcs     code_B8FB
code_B931:  rts

LB932:  .byte   $A2,$4F,$B4,$44,$00,$9E
LB938:  .byte   $01,$02,$02,$03,$04,$04
LB93E:  .byte   $00,$80,$00,$80,$00,$80
LB944:  .byte   $01,$01,$02,$02,$03,$03
code_B94A:  lda     $0520,x
        bne     code_B973
        lda     $03C0,x
        cmp     #$45
        bcc     code_B972
        jsr     code_B9B5
        lda     $0540,x
        tay
        lda     LB7CC,y
        sta     $0440,x
        sta     $0400,x
        lda     LB7D2,y
        sta     $0460,x
        sta     $0420,x
        inc     $0520,x
code_B972:  rts

code_B973:  lda     $03C0,x
        cmp     #$45
        bcs     code_B972
        jsr     code_B9B5
        lda     #$00
        sta     $0520,x
        sta     $0560,x
        inc     $0300,x
        rts

code_B989:  lda     $04A0,x
        and     #$0C
        beq     code_B996
        lda     #$62
        sta     $05C0,x
        rts

code_B996:  lda     $04A0,x
        and     #$01
        beq     code_B9A7
        lda     $0580,x
        and     #$BF
        sta     $0580,x
        bne     code_B9AF
code_B9A7:  lda     $0580,x
        ora     #$40
        sta     $0580,x
code_B9AF:  lda     #$64
        sta     $05C0,x
        rts

code_B9B5:  jsr     LFC53
        bcs     code_B9E7
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
code_B9E7:  rts

        .byte   $00,$00,$00,$00
code_B9EC:  jsr     LFC53
        bcs     code_BA2E
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
        bne     code_BA2E
        lda     $0580,y
        and     #$BF
        sta     $0580,y
code_BA2E:  rts

code_BA2F:  lda     $0300,x
        and     #$0F
        bne     code_BA4E
        sta     $0460,x
        sta     $0540,x
        lda     #$80
        sta     $0440,x
        lda     #$32
        sta     $0500,x
        lda     #$F0
        sta     $0520,x
        inc     $0300,x
code_BA4E:  jsr     LFB7B
        bcs     code_BA62
        lda     #$18
        jsr     LF89A
        ldy     $10
        lda     #$00
        sta     $0300,y
        jmp     code_BAA6

code_BA62:  lda     $0300,x
        and     #$02
        bne     code_BA7A
        jsr     LF779
        dec     $0500,x
        bne     code_BA79
        lda     #$02
        sta     $0500,x
        inc     $0300,x
code_BA79:  rts

code_BA7A:  lda     $04A0,x
        and     #$01
        bne     code_BA87
        jsr     LF779
        jmp     code_BA8A

code_BA87:  jsr     LF759
code_BA8A:  dec     $0500,x
        bne     code_BA9C
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$04
        sta     $0500,x
code_BA9C:  dec     $0520,x
        bne     code_BAB6
        lda     #$90
        sta     $0580,x
code_BAA6:  lda     #$59
        jsr     LF835
        lda     #$00
        sta     $0500,x
        lda     #$19
        sta     $0320,x
        rts

code_BAB6:  lda     $0540,x
        bne     code_BACB
        lda     $0520,x
        cmp     #$02
        bcs     code_BACA
        lda     #$F2
        sta     $0520,x
        inc     $0540,x
code_BACA:  rts

code_BACB:  lda     $0520,x
        cmp     #$78
        bcs     code_BACA
        lda     $0580,x
        eor     #$04
        sta     $0580,x
        rts

code_BADB:  lda     $0300,x
        and     #$0F
        bne     code_BAED
        jsr     code_BB25
        lda     #$3C
        sta     $0560,x
        inc     $0300,x
code_BAED:  lda     $0300,x
        and     #$02
        bne     code_BAFC
        dec     $0560,x
        bne     code_BB24
        inc     $0300,x
code_BAFC:  dec     $0500,x
        bne     code_BB10
        jsr     code_BB91
        jsr     code_BB25
        lda     #$94
        sta     $0580,x
        dec     $0520,x
        rts

code_BB10:  lda     $0520,x
        bne     code_BB24
        lda     $0500,x
        cmp     #$3C
        bcs     code_BB24
        lda     #$90
        sta     $0580,x
        inc     $0520,x
code_BB24:  rts

code_BB25:  lda     #$78
        sta     $0500,x
        lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        cmp     #$02
        bcs     code_BB53
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

code_BB53:  lda     #$CC
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
code_BB91:  jsr     LFC53
        bcs     code_BC07
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
        bne     code_BBD8
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
        jmp     code_BBF2

code_BBD8:  lda     $04A0,x
        sta     $04A0,y
        lda     #$5C
        jsr     LF846
        lda     $0360,x
        sta     $0360,y
        lda     $03C0,x
        sec
        sbc     #$18
        sta     $03C0,y
code_BBF2:  lda     #$F4
        sta     $0320,y
        lda     $0580,y
        ora     #$02
        sta     $0580,y
        lda     #$02
        sta     $0420,y
        sta     $0460,y
code_BC07:  rts

LBC08:  .byte   $E8,$18
code_BC0A:  lda     $04A0,x
        and     #$08
        beq     code_BC20
        ldy     #$09
        jsr     LF642
        lda     $03C0,x
        cmp     #$48
        bcs     code_BC48
        jmp     code_BC36

code_BC20:  lda     $04A0,x
        and     #$01
        beq     code_BC2F
        ldy     #$08
        jsr     LF580
        jmp     code_BC34

code_BC2F:  ldy     #$09
        jsr     LF5C4
code_BC34:  bcc     code_BC48
code_BC36:  lda     $04A0,x
        and     #$08
        beq     code_BC3D
code_BC3D:  lda     #$00
        sta     $0300,x
        lda     #$FF
        sta     $04C0,x
        rts

code_BC48:  jsr     LF8C2
        cmp     #$18
        bcs     code_BC75
        jsr     LF8B3
        cmp     #$14
        bcs     code_BC75
        lda     $04A0,x
        and     #$08
        beq     code_BC5E
        rts

code_BC5E:  lda     $04A0,x
        and     #$02
        bne     code_BC69
        lda     #$01
        bne     code_BC6B
code_BC69:  lda     #$02
code_BC6B:  sta     $36
        lda     #$00
        sta     $37
        lda     #$02
        sta     $38
code_BC75:  lda     $04A0,x
        and     #$08
        beq     code_BC7C
code_BC7C:  rts

code_BC7D:  lda     $0300,x
        and     #$0F
        bne     code_BCC2
        lda     #$09
        cmp     $30
        beq     code_BC99
        sta     $30
        lda     #$80
        sta     $B0
        lda     #$8E
        sta     $B3
        lda     #$0D
        jsr     LF898
code_BC99:  lda     $B0
        cmp     #$9C
        bne     code_BCFA
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        lda     #$00
        sta     $01
        sta     $02
        jsr     code_BDE2
        lda     #$3C
        sta     $0500,x
        lda     #$36
        sta     $0520,x
        lda     #$01
        sta     $04A0,x
        inc     $0300,x
code_BCC2:  lda     $0300,x
        and     #$0F
        cmp     #$02
        beq     code_BCFB
        cmp     #$03
        bne     code_BCD2
        jmp     code_BD82

code_BCD2:  lda     $0580,x
        and     #$04
        beq     code_BCE6
        dec     $0500,x
        bne     code_BCFA
        lda     $0580,x
        eor     #$04
        sta     $0580,x
code_BCE6:  lda     $05A0,x
        cmp     #$04
        bne     code_BCFA
        lda     #$04
        jsr     LF835
        lda     #$3C
        sta     $0500,x
        inc     $0300,x
code_BCFA:  rts

code_BCFB:  lda     $04A0,x
        and     #$01
        beq     code_BD10
        lda     $0580,x
        ora     #$40
        sta     $0580,x
        jsr     LF71D
        jmp     code_BD1B

code_BD10:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        jsr     LF73B
code_BD1B:  dec     $0520,x
        bne     code_BD30
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     #$6C
        sta     $0520,x
        inc     $0540,x
code_BD30:  lda     $0540,x
        cmp     #$02
        bcc     code_BD50
        lda     $0520,x
        cmp     #$36
        bcs     code_BD50
        inc     $0300,x
        lda     #$1E
        sta     $0560,x
        lda     #$A1
        sta     $0480,x
        lda     #$01
        jmp     LF835

code_BD50:  dec     $0500,x
        bne     code_BD6C
        lda     #$05
        jsr     LF835
        jsr     code_BEB8
        lda     $E4
        adc     $E5
        sta     $E4
        and     #$03
        tay
        lda     LBDDE,y
        sta     $0500,x
code_BD6C:  lda     $05C0,x
        cmp     #$04
        beq     code_BD81
        lda     $05A0,x
        beq     code_BD81
        cmp     #$03
        bne     code_BD81
        lda     #$04
        jsr     LF835
code_BD81:  rts

code_BD82:  lda     $05C0,x
        cmp     #$13
        beq     code_BD99
        dec     $0560,x
        bne     code_BD98
        lda     #$13
        jsr     LF835
        lda     #$3C
        sta     $0560,x
code_BD98:  rts

code_BD99:  lda     $0580,x
        and     #$04
        bne     code_BDB2
        lda     $05A0,x
        cmp     #$04
        bne     code_BD98
        jsr     code_BE6C
        lda     $0580,x
        eor     #$04
        sta     $0580,x
code_BDB2:  dec     $0560,x
        bne     code_BD98
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
code_BDE2:  jsr     LFC53
        bcs     code_BE4B
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
        bcc     code_BDE2
code_BE4B:  rts

LBE4C:  .byte   $74,$C4,$24,$74,$C4,$24,$C4,$24
        .byte   $C4,$74,$24,$74,$24,$C4,$24,$C4
LBE5C:  .byte   $02,$01,$01,$02,$01,$01,$01,$01
        .byte   $01,$02,$01,$02,$01,$01,$01,$01
code_BE6C:  lda     $04C0,x
        cmp     #$2C
        bne     code_BE9A
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
        jsr     code_BDE2
        rts

code_BE9A:  lda     #$00
        sta     $0300,x
        rts

LBEA0:  .byte   $24,$C4,$74,$74,$24,$C4,$74,$74
LBEA8:  .byte   $01,$01,$02,$02,$01,$01,$02,$02
LBEB0:  .byte   $00,$02,$04,$06,$08,$0A,$0C,$0E
code_BEB8:  jsr     LFC53
        bcs     code_BF14
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
code_BF14:  rts

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
