; =============================================================================
; MEGA MAN 3 (U) — BANK $02 — GEMINI MAN STAGE DATA + TITLE SCREEN
; =============================================================================
; Gemini Man stage layout data and title screen display logic.
;
; Annotation: 0% — unannotated da65 output
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $02 — GEMINI MAN STAGE DATA + TITLE SCREEN
; =============================================================================
; Mapped to $A000-$BFFF. Dual-purpose:
; - Stage data for Gemini Man ($22=$02): layout, enemies, metatiles
; - Title screen logic: input handling, sound, cutscene sequencing
;
; Annotation: light — all labels auto-generated, title screen logic bare
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

LC5E9           := $C5E9
LF835           := $F835
LF89A           := $F89A
LFF21           := $FF21
LFF3C           := $FF3C

.segment "BANK02"

        jmp     code_A15B

        lda     $EA
        pha
        lda     $EB
        pha
        lda     $EC
        pha
        lda     $ED
        pha
        ldx     #$03
code_A011:  lda     $0620,x
        sta     $0600,x
        dex
        bpl     code_A011
code_A01A:  jsr     code_A18E
code_A01D:  lda     joy1_press
        and     #$D0
        bne     code_A02C
code_A023:  jsr     code_A398
        jsr     code_A2EA
        jmp     code_A01D

code_A02C:  and     #$40
        bne     code_A038
        lda     weapon_cursor
        bpl     code_A0A0
        cmp     #$FF
        beq     code_A05C
code_A038:  lda     $51
        cmp     #$E8
        beq     code_A04B
        lda     $51
        clc
        adc     #$04
        sta     $51
        jsr     code_A2EA
        jmp     code_A038

code_A04B:  lda     $B4
        eor     #$06
        sta     $B4
        lda     #$00
        sta     scroll_lock
        lda     #$80
        sta     weapon_cursor
        jmp     code_A01A

code_A05C:  lda     $B4
        bne     code_A09D
        sta     $95
        lda     etanks
        beq     code_A09D
        sec
        sbc     #$01
        sta     etanks
        and     #$0F
        cmp     #$0F
        bne     code_A078
        lda     etanks
        sec
        sbc     #$06
        sta     etanks
code_A078:  lda     #$00
        sta     weapon_cursor
        lda     player_hp
        cmp     #$9C
        beq     code_A09D
        lda     $95
        and     #$03
        bne     code_A097
        inc     player_hp
        lda     weapon_cursor
        pha
        ldx     #$00
        stx     weapon_cursor
        jsr     code_A50D
        pla
        sta     weapon_cursor
code_A097:  jsr     code_A2EA
        jmp     code_A078

code_A09D:  jmp     code_A023

code_A0A0:  lda     weapon_cursor
        clc
        adc     $B4
        cmp     current_weapon
        beq     code_A0AD
        ldy     #$00
        sty     $B5
code_A0AD:  sta     current_weapon
        lda     current_weapon
        bne     code_A0B7
        sta     $B1
        beq     code_A0E6
code_A0B7:  ora     #$80
        sta     $B1
        lda     $B4
        beq     code_A0E6
        lda     weapon_cursor
        and     #$01
        beq     code_A0E6
        lda     weapon_cursor
        lsr     a
        clc
        adc     #$06
        sta     weapon_cursor
        lda     #$00
        sta     $95
code_A0D1:  jsr     code_A2EA
        lda     $95
        cmp     #$0F
        bne     code_A0D1
        lda     #$1E
code_A0DC:  pha
        jsr     LFF21
        pla
        sec
        sbc     #$01
        bne     code_A0DC
code_A0E6:  lda     $51
        cmp     #$E8
        beq     code_A0FB
        lda     $51
        clc
        adc     #$04
        sta     $51
        jsr     code_A2EA
        dec     $95
        jmp     code_A0E6

code_A0FB:  lda     weapon_cursor
        cmp     #$06
        bcc     code_A108
        sbc     #$06
        asl     a
        ora     #$01
        sta     weapon_cursor
code_A108:  pla
        sta     $ED
        pla
        sta     $EC
        pla
        sta     $EB
        pla
        sta     $EA
        ldy     current_weapon
        lda     LA634,y
        sta     $EB
        lda     #$00
        sta     scroll_lock
        ldy     game_mode
        cpy     #$01
        bne     code_A127
        sta     game_mode
code_A127:  ldx     #$0F
code_A129:  lda     $0630,x
        sta     $0610,x
        dex
        bne     code_A129
        lda     current_weapon
        asl     a
        asl     a
        tay
        ldx     #$00
code_A139:  lda     LA641,y
        sta     $0611,x
        sta     $0631,x
        iny
        inx
        cpx     #$03
        bne     code_A139
        stx     palette_dirty
        lda     #PSTATE_REAPPEAR
        sta     player_state
        ldx     #$00
        lda     #$13
        jsr     LF835
        inc     ent_anim_state
        jmp     LFF3C

code_A15B:  lda     $EA
        pha
        lda     $EB
        pha
        lda     $EC
        pha
        lda     $ED
        pha
        jsr     code_A18E
        lda     #$00
        sta     $95
code_A16E:  lda     $95
        and     #$03
        bne     code_A185
        lda     weapon_cursor
        clc
        adc     $B4
        tax
        lda     player_hp,x
        cmp     #$9C
        beq     code_A18B
        inc     player_hp,x
        jsr     code_A50D
code_A185:  jsr     code_A2EA
        jmp     code_A16E

code_A18B:  jmp     code_A0E6

code_A18E:  lda     #$1A
        jsr     LF89A
        lda     scroll_lock
        pha
        inc     scroll_lock
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        pla
        sta     scroll_lock
        lda     game_mode
        bne     code_A1AA
        lda     #$01
        sta     game_mode
code_A1AA:  ldy     scroll_lock
        cpy     #$08
        beq     code_A217
        ldx     LA575,y
        ldy     #$00
code_A1B5:  lda     $52
        and     #$0C
        ora     LA57D,x
        sta     $0780,y
        bmi     code_A1F7
        lda     LA57E,x
        sta     $0781,y
        lda     LA57F,x
        sta     $0782,y
        sta     $00
code_A1CF:  lda     #$00
        sta     $01
        lda     LA580,x
        bpl     code_A1E0
        and     #$7F
        sta     $01
        inx
        lda     LA580,x
code_A1E0:  sta     $0783,y
        iny
        dec     $00
        dec     $01
        bpl     code_A1E0
        inx
        lda     $00
        bpl     code_A1CF
        inx
        inx
        inx
        iny
        iny
        iny
        bne     code_A1B5
code_A1F7:  jsr     code_A251
        lda     game_mode
        cmp     #$0B
        bne     code_A213
        lda     #$20
        sta     $0780
        lda     $0781
        sec
        sbc     #$C0
        sta     $0781
        lda     #$FF
        sta     $07A3
code_A213:  inc     nametable_dirty
        inc     scroll_lock
code_A217:  lda     #$74
        cmp     $EA
        beq     code_A23D
        sta     $EA
        lda     #$0B
        sta     $EB
        lda     #$12
        sta     $EC
        lda     #$1C
        sta     $ED
        ldx     #$0F
code_A22D:  lda     LA624,x
        sta     $0610,x
        dex
        bne     code_A22D
        lda     #$FF
        sta     palette_dirty
        jsr     LFF3C
code_A23D:  jsr     code_A2EA
        lda     $51
        cmp     #$B0
        beq     code_A250
        lda     $51
        sec
        sbc     #$04
        sta     $51
        jmp     code_A1AA

code_A250:  rts

code_A251:  lda     scroll_lock
        cmp     #$06
        bcs     code_A2A9
        and     #$01
        beq     code_A2A9
        lda     scroll_lock
        clc
        adc     $B4
        and     #$FE
        sta     $00
        ldx     #$08
code_A266:  lda     #$07
        sta     $01
        ldy     $00
        lda     player_hp,y
        bpl     code_A29D
        and     #$7F
        sta     $02
        lda     LA5D8,y
        sta     $0780,x
        lda     LA5E4,y
        sta     $0781,x
        inx
        inx
code_A283:  ldy     #$04
        lda     $02
        sec
        sbc     #$04
        bcs     code_A290
        ldy     $02
        lda     #$00
code_A290:  sta     $02
        lda     LA5F0,y
        sta     $0780,x
        inx
        dec     $01
        bne     code_A283
code_A29D:  lda     $00
        and     #$01
        bne     code_A2A9
        ldx     #$13
        inc     $00
        bne     code_A266
code_A2A9:  lda     scroll_lock
        cmp     #$02
        bne     code_A2C3
        lda     $B4
        bne     code_A2DE
        lda     lives
        and     #$0F
        sta     $07A1
        lda     lives
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        sta     $07A0
code_A2C3:  lda     scroll_lock
        cmp     #$05
        bne     code_A2E9
        lda     $B4
        bne     code_A2DE
        lda     etanks
        and     #$0F
        sta     $07A1
        lda     etanks
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        sta     $07A0
        rts

code_A2DE:  lda     #$25
        sta     $079F
        sta     $07A0
        sta     $07A1
code_A2E9:  rts

code_A2EA:  lda     game_mode
        cmp     #$01
        beq     code_A2F6
        lda     $5E
        cmp     $51
        bcc     code_A2F8
code_A2F6:  lda     $51
code_A2F8:  sta     irq_scanline
        jsr     LC5E9
        jsr     code_A30C
        lda     #$00
        sta     nmi_skip
        jsr     LFF21
        inc     nmi_skip
        inc     $95
        rts

code_A30C:  lda     $B4
        bne     code_A347
        ldx     #$1C
        lda     weapon_cursor
        cmp     #$FF
        bne     code_A320
        lda     $95
        and     #$08
        beq     code_A320
        ldx     #$0C
code_A320:  lda     LA670,x
        clc
        adc     $51
        bcs     code_A341
        cmp     #$F0
        bcs     code_A341
        sta     $0200,x
        lda     LA671,x
        sta     $0201,x
        lda     LA672,x
        sta     $0202,x
        lda     LA673,x
        sta     $0203,x
code_A341:  dex
        dex
        dex
        dex
        bpl     code_A320
code_A347:  lda     weapon_cursor
        bpl     code_A34C
code_A34B:  rts

code_A34C:  clc
        adc     $B4
        beq     code_A34B
        asl     a
        tax
        lda     $95
        and     #$08
        beq     code_A35A
        inx
code_A35A:  lda     LA690,x
        sta     $00
        lda     LA6AE,x
        sta     $01
        ldx     #$20
        ldy     #$00
code_A368:  lda     ($00),y
        bmi     code_A397
        clc
        adc     $51
        bcs     code_A391
        cmp     #$F0
        bcs     code_A391
        sta     $0200,x
        iny
        lda     ($00),y
        sta     $0201,x
        iny
        lda     ($00),y
        sta     $0202,x
        iny
        lda     ($00),y
        sta     $0203,x
        iny
code_A38B:  inx
        inx
        inx
        inx
        bne     code_A368
code_A391:  iny
        iny
        iny
        iny
        bne     code_A38B
code_A397:  rts

code_A398:  lda     weapon_cursor
        pha
        lda     joy1_held
        and     #$0F
        bne     code_A3A8
        lda     #$F0
        sta     $1F
code_A3A5:  jmp     code_A477

code_A3A8:  lda     $1F
        clc
        adc     #$10
        sta     $1F
        bne     code_A3A5
        lda     joy1_held
        and     #BTN_RIGHT
        beq     code_A3EC
code_A3B7:  lda     weapon_cursor
        bpl     code_A3C5
        cmp     #$FF
        beq     code_A3A5
        lda     #$00
        sta     weapon_cursor
        beq     code_A3DE
code_A3C5:  lda     $B4
        beq     code_A3CF
        lda     weapon_cursor
        and     #$01
        bne     code_A3A5
code_A3CF:  inc     weapon_cursor
        lda     weapon_cursor
        and     #$01
        bne     code_A3DE
        lda     #$FF
        sta     weapon_cursor
        jmp     code_A477

code_A3DE:  lda     weapon_cursor
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A3B7
        jmp     code_A477

code_A3EC:  lda     joy1_held
        and     #BTN_LEFT
        beq     code_A41D
code_A3F2:  lda     weapon_cursor
        bpl     code_A400
        cmp     #$80
        beq     code_A3A5
        lda     #$01
        sta     weapon_cursor
        bne     code_A40F
code_A400:  dec     weapon_cursor
        lda     weapon_cursor
        and     #$01
        beq     code_A40F
        lda     #$80
        sta     weapon_cursor
        jmp     code_A3A5

code_A40F:  lda     weapon_cursor
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A3F2
        jmp     code_A477

code_A41D:  lda     weapon_cursor
        bmi     code_A477
        lda     joy1_held
        and     #BTN_UP
        beq     code_A44C
code_A427:  lda     weapon_cursor
        bne     code_A431
        lda     #$05
        sta     weapon_cursor
        bne     code_A46C
code_A431:  cmp     #$01
        bne     code_A43B
        lda     #$04
        sta     weapon_cursor
        bne     code_A43F
code_A43B:  dec     weapon_cursor
        dec     weapon_cursor
code_A43F:  lda     weapon_cursor
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A427
        bmi     code_A477
code_A44C:  lda     joy1_held
        and     #BTN_DOWN
        beq     code_A477
code_A452:  lda     weapon_cursor
        cmp     #$04
        bne     code_A45E
        lda     #$01
        sta     weapon_cursor
        bne     code_A46C
code_A45E:  cmp     #$05
        bne     code_A468
        lda     #$00
        sta     weapon_cursor
        beq     code_A46C
code_A468:  inc     weapon_cursor
        inc     weapon_cursor
code_A46C:  lda     weapon_cursor
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A452
code_A477:  pla
        cmp     weapon_cursor
        beq     code_A481
        lda     #$1B
        jsr     LF89A
code_A481:  ldx     #$00
code_A483:  lda     $52
        and     #$2C
        ora     LA5F5,x
        sta     $0780,x
        cmp     #$FF
        beq     code_A4A0
        inx
        ldy     #$04
code_A494:  lda     LA5F5,x
        sta     $0780,x
        inx
        dey
        bne     code_A494
        beq     code_A483
code_A4A0:  lda     #$05
        sta     $00
        ldx     $B4
        ldy     #$03
code_A4A8:  lda     player_hp,x
        bpl     code_A4B8
        lda     LA5D8,x
        sta     $0780,y
        lda     LA5E4,x
        sta     $0781,y
code_A4B8:  inx
        tya
        clc
        adc     #$05
        tay
        dec     $00
        bpl     code_A4A8
        lda     $95
        and     #$08
        beq     code_A4EB
        ldx     weapon_cursor
        bpl     code_A4E0
        cpx     #$FF
        beq     code_A4EB
        lda     #$25
        sta     $07A1
        sta     $07A2
        sta     $07A6
        sta     $07A7
        bne     code_A4EB
code_A4E0:  ldy     LA61E,x
        lda     #$25
        sta     $0780,y
        sta     $0781,y
code_A4EB:  lda     game_mode
        cmp     #$0B
        bne     code_A50A
        ldy     #$00
code_A4F3:  lda     #$20
        sta     $0780,y
        lda     $0781,y
        sec
        sbc     #$C0
        sta     $0781,y
        iny
        iny
        iny
        iny
        iny
        cpy     #$28
        bne     code_A4F3
code_A50A:  inc     nametable_dirty
        rts

code_A50D:  lda     #$1C
        jsr     LF89A
        lda     weapon_cursor
        asl     a
        tay
        lda     $52
        and     #$0C
        ora     LA569,y
        sta     $0780
        lda     LA56A,y
        sta     $0781
        lda     #$06
        sta     $0782
        lda     player_hp,x
        and     #$1F
        sta     $00
        ldy     #$00
code_A533:  ldx     #$04
        lda     $00
        sec
        sbc     #$04
        bcs     code_A540
        ldx     $00
        lda     #$00
code_A540:  sta     $00
        lda     LA5F0,x
        sta     $0783,y
        iny
        cpy     #$07
        bne     code_A533
        lda     game_mode
        cmp     #$0B
        bne     code_A561
        lda     #$20
        sta     $0780
        lda     $0781
        sec
        sbc     #$C0
        sta     $0781
code_A561:  lda     #$FF
        sta     $078A
        .byte   $85,$19,$60
LA569:  .byte   $22
LA56A:  .byte   $E7,$22,$F2,$23,$27,$23,$32,$23
        .byte   $67,$23,$72
LA575:  .byte   $00,$0D,$15,$25,$2D,$38,$46,$53
LA57D:  .byte   $22
LA57E:  .byte   $C0
LA57F:  .byte   $1F
LA580:  .byte   $30,$9D,$31,$32,$23,$E8,$07,$87
        .byte   $00,$FF,$22,$E0,$1F,$33,$9D,$25
        .byte   $34,$FF,$23,$00,$1F,$33,$9A,$25
        .byte   $2B,$00,$00,$34,$23,$F0,$07,$87
        .byte   $00,$FF,$23,$20,$1F,$33,$9D,$25
        .byte   $34,$FF,$23,$40,$1F,$33,$25,$38
        .byte   $39,$9A,$25,$34,$FF,$23,$60,$1F
        .byte   $33,$25,$3A,$3B,$97,$25,$2B,$00
        .byte   $00,$34,$FF,$23,$80,$1F,$33,$9D
        .byte   $25,$34,$23,$F8,$07,$87,$00,$FF
        .byte   $23,$A0,$1F,$35,$9D,$36,$37,$FF
LA5D8:  .byte   $19,$10,$17,$11,$16,$1D,$1C,$1B
        .byte   $1C,$1B,$1C,$1B
LA5E4:  .byte   $25,$0E,$0E,$0A,$0A,$18,$17,$0C
        .byte   $19,$16,$11,$13
LA5F0:  .byte   $24,$2F,$2E,$2D,$2C
LA5F5:  .byte   $22,$E5,$01,$25,$25,$22,$F0,$01
        .byte   $25,$25,$23,$25,$01,$25,$25,$23
        .byte   $30,$01,$25,$25,$23,$65,$01,$25
        .byte   $25,$23,$70,$01,$25,$25,$23,$42
        .byte   $01,$38,$39,$23,$62,$01,$3A,$3B
        .byte   $FF
LA61E:  .byte   $03,$08,$0D,$12,$17,$1C
LA624:  .byte   $0F,$0F,$30,$15,$0F,$0F,$30,$37
        .byte   $0F,$0F,$3C,$11,$0F,$0F,$30,$19
LA634:  .byte   $01,$02,$07,$03,$01,$07,$01,$04
        .byte   $06,$02,$06,$03,$0F
LA641:  .byte   $0F,$2C,$11,$0F,$0F,$30,$21,$0F
        .byte   $0F,$30,$17,$0F,$0F,$10,$01,$0F
        .byte   $0F,$10,$16,$0F,$0F,$36,$00,$0F
        .byte   $0F,$30,$19,$0F,$0F,$30,$15,$0F
        .byte   $0F,$30,$26,$0F,$0F,$30,$15,$0F
        .byte   $0F,$34,$14,$0F,$0F,$30,$15
LA670:  .byte   $08
LA671:  .byte   $F3
LA672:  .byte   $02
LA673:  .byte   $D0,$08,$F3,$42,$D8,$10,$F4,$01
        .byte   $D0,$10,$F4,$41,$D8,$20,$F1,$02
        .byte   $D0,$20,$F2,$02,$D8,$28,$F1,$82
        .byte   $D0,$28,$F2,$82,$D8
LA690:  .byte   $CC,$CC,$CC,$CC,$DD,$DD,$EE,$EE
        .byte   $FF,$FF,$10,$10,$21,$21,$56,$83
        .byte   $5F,$5F,$56,$83,$BA,$BA,$56,$83
        .byte   $56,$32,$2D,$CB,$2D,$70
LA6AE:  .byte   $A6,$A6,$A6,$A6,$A6,$A6,$A6,$A6
        .byte   $A6,$A6,$A7,$A7,$A7,$A7,$A8,$A8
        .byte   $A7,$A7,$A8,$A8,$A7,$A7,$A8,$A8
        .byte   $A8,$A7,$A8,$A7,$A8,$A7,$08,$4F
        .byte   $03,$10,$08,$5F,$C3,$18,$10,$5F
        .byte   $03,$10,$10,$4F,$C3,$18,$FF,$08
        .byte   $F8,$01,$10,$08,$F9,$01,$18,$10
        .byte   $FA,$01,$10,$10,$FB,$01,$18,$FF
        .byte   $08,$B8,$00,$10,$08,$B9,$00,$18
        .byte   $10,$BA,$00,$10,$10,$BB,$00,$18
        .byte   $FF,$08,$5E,$00,$10,$08,$6F,$00
        .byte   $18,$10,$5E,$80,$10,$10,$6F,$80
        .byte   $18,$FF,$08,$BD,$00,$10,$08,$BD
        .byte   $40,$18,$10,$BE,$00,$10,$10,$BF
        .byte   $00,$18,$FF,$08,$FC,$03,$10,$08
        .byte   $FD,$03,$18,$10,$FE,$03,$10,$10
        .byte   $FF,$03,$18,$FF,$0A,$DA,$41,$1C
        .byte   $09,$DB,$41,$14,$04,$D6,$40,$19
        .byte   $04,$E0,$40,$11,$04,$E1,$40,$09
        .byte   $0C,$DC,$40,$19,$0C,$E5,$40,$11
        .byte   $0C,$E6,$40,$09,$14,$E2,$40,$19
        .byte   $14,$E3,$40,$11,$14,$E4,$40,$09
        .byte   $FF,$08,$BC,$01,$10,$08,$BC,$41
        .byte   $18,$10,$BC,$81,$10,$10,$BC,$C1
        .byte   $18,$FF,$13,$D5,$41,$1D,$0C,$CC
        .byte   $40,$20,$0C,$CD,$40,$18,$0C,$CE
        .byte   $40,$10,$0C,$CF,$40,$08,$14,$D0
        .byte   $40,$20,$14,$D1,$40,$18,$14,$D2
        .byte   $40,$10,$14,$D3,$41,$08,$FF,$0F
        .byte   $D5,$41,$1D,$08,$CC,$40,$20,$08
        .byte   $CD,$40,$18,$08,$CE,$40,$10,$08
        .byte   $CF,$40,$08,$10,$D0,$40,$20,$10
        .byte   $D1,$40,$18,$10,$D2,$40,$10,$10
        .byte   $D4,$41,$08,$FF,$08,$B7,$81,$10
        .byte   $08,$B7,$C1,$18,$10,$B7,$01,$10
        .byte   $10,$B7,$41,$18,$FF,$14,$F5,$41
        .byte   $1D,$04,$CB,$40,$08,$0C,$C0,$40
        .byte   $20,$0C,$C1,$40,$18,$0C,$C2,$40
        .byte   $10,$0C,$C3,$40,$08,$0C,$C8,$41
        .byte   $00,$14,$C4,$40,$20,$14,$C5,$40
        .byte   $18,$14,$C6,$40,$10,$14,$C7,$40
        .byte   $08,$14,$C9,$41,$00,$1C,$CA,$41
        .byte   $00,$FF,$10,$F5,$41,$1D,$00,$CB
        .byte   $40,$08,$08,$C0,$40,$20,$08,$C1
        .byte   $40,$18,$08,$C2,$40,$10,$08,$C3
        .byte   $40,$08,$10,$C4,$40,$20,$10,$C5
        .byte   $40,$18,$10,$C6,$40,$10,$10,$F6
        .byte   $40,$08,$10,$F7,$41,$00,$FF,$0E
        .byte   $E7,$41,$1C,$0E,$E8,$41,$14,$04
        .byte   $E9,$40,$19,$04,$EA,$40,$11,$0C
        .byte   $EB,$40,$19,$0C,$EC,$40,$11,$0C
        .byte   $ED,$40,$09,$14,$EE,$40,$19,$14
        .byte   $EF,$40,$11,$14,$F0,$40,$09,$FF
        .byte   $0A,$DA,$41,$1C,$09,$DB,$41,$14
        .byte   $04,$D6,$40,$19,$04,$D7,$40,$11
        .byte   $04,$D8,$40,$09,$0C,$DC,$40,$19
        .byte   $0C,$DD,$40,$11,$0C,$DE,$40,$09
        .byte   $14,$E2,$40,$19,$14,$E3,$40,$11
        .byte   $14,$E4,$40,$09,$FF,$0A,$DA,$41
        .byte   $1C,$09,$DB,$41,$14,$04,$D6,$40
        .byte   $19,$04,$D7,$40,$11,$04,$D9,$40
        .byte   $09,$0C,$DC,$40,$19,$0C,$DD,$40
        .byte   $11,$0C,$DF,$40,$09,$14,$E2,$40
        .byte   $19,$14,$E3,$40,$11,$14,$E4,$40
        .byte   $09,$FF,$08,$00,$00,$0D,$00,$0B
        .byte   $80,$44,$00,$00,$20,$9E,$00,$00
        .byte   $80,$20,$00,$60,$08,$02,$80,$00
        .byte   $00,$00,$22,$10,$00,$00,$00,$42
        .byte   $20,$80,$02,$02,$80,$00,$20,$02
        .byte   $00,$3A,$08,$00,$00,$10,$A0,$04
        .byte   $88,$C0,$00,$02,$00,$11,$28,$B0
        .byte   $0A,$0A,$08,$40,$02,$08,$20,$01
        .byte   $08,$60,$0A,$00,$00,$48,$00,$01
        .byte   $00,$80,$02,$30,$00,$00,$02,$AC
        .byte   $80,$11,$00,$00,$82,$00,$20,$9C
        .byte   $08,$00,$20,$00,$80,$32,$00,$20
        .byte   $02,$01,$00,$02,$80,$80,$00,$22
        .byte   $80,$44,$00,$21,$00,$29,$20,$08
        .byte   $00,$00,$00,$01,$08,$00,$00,$20
        .byte   $80,$10,$28,$00,$02,$98,$02,$08
        .byte   $00,$02,$80,$2C,$20,$50,$22,$06
        .byte   $20,$10,$08,$14,$28,$04,$00,$01
        .byte   $02,$00,$00,$A0,$00,$00,$80,$01
        .byte   $00,$00,$00,$00,$00,$20,$00,$1C
        .byte   $00,$04,$00,$10,$00,$04,$02,$02
        .byte   $02,$00,$00,$20,$20,$00,$00,$11
        .byte   $22,$04,$00,$08,$20,$08,$00,$29
        .byte   $80,$61,$00,$00,$00,$48,$00,$21
        .byte   $08,$40,$08,$40,$18,$32,$82,$00
        .byte   $20,$01,$20,$20,$00,$31,$08,$05
        .byte   $A0,$18,$00,$04,$02,$00,$00,$40
        .byte   $22,$41,$20,$20,$00,$04,$00,$60
        .byte   $A8,$08,$80,$00,$A2,$80,$20,$80
        .byte   $00,$00,$2A,$20,$00,$00,$80,$04
        .byte   $22,$14,$00,$20,$00,$05,$88,$00
        .byte   $20,$22,$0A,$00,$02,$11,$82,$11
        .byte   $00,$02,$00,$00,$20,$00,$20,$62
        .byte   $00,$00,$02,$00,$00,$88,$00,$40
        .byte   $08,$00,$22,$04,$00,$01,$02,$B0
        .byte   $00,$0D,$80,$B0,$28,$81,$20,$48
        .byte   $80,$80,$00,$41,$00,$80,$02,$2D
        .byte   $02,$A0,$02,$10,$08,$22,$20,$01
        .byte   $28,$00,$20,$04,$00,$0C,$22,$F0
        .byte   $30,$10,$08,$04,$00,$49,$00,$80
        .byte   $08,$00,$00,$2A,$0A,$56,$80,$09
        .byte   $20,$5C,$00,$A8,$C2,$59,$02,$60
        .byte   $00,$15,$18,$19,$1A,$1B,$00,$01
        .byte   $02,$03,$04,$1D,$05,$06,$07,$08
        .byte   $17,$09,$0A,$0B,$0C,$1C,$0D,$0E
        .byte   $0F,$10,$11,$12,$13,$14,$15,$16
        .byte   $80,$04,$00,$00,$80,$2C,$20,$10
        .byte   $02,$00,$00,$04,$02,$00,$02,$00
        .byte   $20,$21,$1B,$12,$12,$25,$20,$00
        .byte   $20,$83,$08,$08,$00,$90,$90,$02
        .byte   $20,$30,$28,$20,$62,$80,$A4,$40
        .byte   $65,$80,$A0,$20,$20,$00,$20,$A0
        .byte   $08,$10,$0A,$14,$00,$01,$08,$00
        .byte   $2A,$80,$00,$02,$00,$09,$80,$40
        .byte   $00,$02,$04,$00,$16,$00,$03,$01
        .byte   $03,$01,$33,$01,$09,$01,$33,$01
        .byte   $02,$01,$0B,$01,$00,$02,$27,$02
        .byte   $00,$40,$20,$01,$22,$10,$00,$20
        .byte   $02,$02,$4C,$4E,$0F,$20,$10,$00
        .byte   $0F,$10,$00,$07,$0F,$36,$27,$07
        .byte   $0F,$31,$21,$11,$82,$83,$00,$00
        .byte   $0F,$31,$11,$00,$0F,$31,$15,$17
        .byte   $0F,$22,$12,$12,$0F,$20,$10,$17
        .byte   $84,$85,$86,$00,$0F,$31,$11,$00
        .byte   $0F,$31,$15,$17,$0F,$22,$12,$12
        .byte   $0F,$2C,$0B,$09,$84,$85,$86,$00
        .byte   $0F,$31,$11,$00,$0F,$31,$15,$17
        .byte   $0F,$22,$12,$12,$0F,$31,$21,$11
        .byte   $84,$85,$86,$00,$80,$0B,$00,$20
        .byte   $08,$09,$08,$00,$00,$01,$20,$40
        .byte   $20,$28,$00,$10,$00,$24,$80,$48
        .byte   $02,$40,$08,$08,$80,$40,$20,$12
        .byte   $08,$01,$06,$09,$00,$23,$A0,$11
        .byte   $8A,$00,$14,$1C,$FF,$69,$A0,$66
        .byte   $20,$23,$01,$02,$02,$02,$03,$03
        .byte   $03,$04,$04,$04,$05,$05,$06,$06
        .byte   $06,$07,$07,$07,$08,$08,$08,$09
        .byte   $09,$0A,$0A,$0B,$0E,$0F,$10,$10
        .byte   $10,$12,$12,$12,$13,$15,$15,$16
        .byte   $16,$16,$16,$17,$17,$17,$17,$17
        .byte   $18,$18,$18,$18,$18,$19,$19,$19
        .byte   $19,$19,$19,$1A,$1A,$1B,$1D,$FF
        .byte   $00,$48,$00,$00,$80,$20,$02,$00
        .byte   $00,$2A,$28,$2A,$00,$02,$00,$00
        .byte   $00,$82,$00,$02,$80,$86,$02,$14
        .byte   $00,$00,$00,$40,$80,$06,$02,$22
        .byte   $8A,$60,$00,$04,$22,$10,$20,$C0
        .byte   $08,$84,$00,$21,$04,$00,$02,$00
        .byte   $88,$C5,$00,$02,$A2,$11,$00,$01
        .byte   $08,$64,$00,$10,$00,$90,$00,$20
        .byte   $02,$10,$00,$0A,$00,$01,$00,$0E
        .byte   $20,$70,$00,$02,$00,$10,$00,$02
        .byte   $20,$40,$08,$15,$02,$50,$00,$30
        .byte   $00,$80,$20,$1C,$A0,$00,$08,$00
        .byte   $82,$00,$00,$08,$80,$0C,$00,$04
        .byte   $00,$80,$20,$00,$00,$00,$08,$00
        .byte   $02,$08,$08,$0A,$00,$00,$08,$4A
        .byte   $80,$00,$00,$00,$80,$51,$00,$04
        .byte   $00,$60,$23,$3C,$00,$00,$0A,$40
        .byte   $A2,$10,$20,$00,$00,$80,$00,$80
        .byte   $00,$E5,$08,$13,$20,$12,$20,$51
        .byte   $00,$41,$28,$00,$08,$10,$82,$00
        .byte   $02,$11,$00,$00,$0A,$41,$02,$C0
        .byte   $80,$16,$00,$80,$20,$02,$00,$20
        .byte   $82,$17,$00,$0E,$00,$34,$20,$40
        .byte   $02,$44,$82,$B2,$08,$E0,$08,$20
        .byte   $00,$30,$58,$18,$34,$B8,$28,$78
        .byte   $88,$28,$48,$A8,$48,$A8,$68,$88
        .byte   $98,$18,$68,$C8,$38,$58,$88,$80
        .byte   $80,$48,$B8,$90,$30,$E8,$68,$C0
        .byte   $D8,$18,$20,$D8,$40,$F0,$F8,$98
        .byte   $A8,$F0,$F8,$08,$78,$88,$C8,$F8
        .byte   $68,$88,$C8,$D8,$F8,$48,$78,$A8
        .byte   $C8,$E7,$E8,$2C,$4C,$A8,$D8,$FF
        .byte   $01,$00,$08,$00,$00,$00,$00,$00
        .byte   $41,$00,$03,$01,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$04,$10
        .byte   $00,$00,$08,$00,$18,$00,$40,$00
        .byte   $08,$10,$20,$40,$1A,$00,$00,$00
        .byte   $0C,$00,$44,$00,$20,$00,$20,$00
        .byte   $00,$41,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$41
        .byte   $00,$04,$00,$00,$00,$01,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$40,$40,$30,$00
        .byte   $00,$10,$01,$00,$80,$00,$00,$00
        .byte   $48,$00,$00,$00,$00,$04,$00,$00
        .byte   $02,$00,$08,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$10,$10,$00
        .byte   $20,$00,$00,$00,$28,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$00,$44,$00
        .byte   $80,$00,$84,$00,$00,$10,$08,$00
        .byte   $00,$00,$00,$10,$49,$04,$02,$40
        .byte   $14,$00,$00,$11,$40,$00,$00,$00
        .byte   $01,$00,$38,$58,$B4,$48,$58,$A4
        .byte   $38,$68,$A8,$94,$98,$68,$88,$A8
        .byte   $B4,$58,$78,$98,$68,$48,$94,$00
        .byte   $B0,$38,$58,$47,$48,$18,$18,$9C
        .byte   $18,$18,$9C,$18,$48,$D8,$C8,$C8
        .byte   $18,$D8,$C8,$88,$C8,$18,$18,$C8
        .byte   $C8,$18,$C8,$88,$18,$18,$C8,$C8
        .byte   $18,$C8,$38,$58,$78,$A8,$00,$FF
        .byte   $00,$00,$04,$04,$00,$10,$00,$00
        .byte   $00,$00,$80,$00,$0C,$10,$00,$00
        .byte   $00,$00,$80,$00,$50,$00,$00,$00
        .byte   $04,$00,$00,$04,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$44,$40,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$01,$00,$00,$44
        .byte   $03,$04,$40,$00,$00,$00,$02,$00
        .byte   $00,$41,$00,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$01,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$04,$00,$10
        .byte   $4C,$00,$04,$00,$40,$00,$00,$40
        .byte   $01,$00,$00,$00,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$08,$10,$10,$00,$20,$00
        .byte   $00,$00,$04,$00,$80,$00,$40,$00
        .byte   $00,$00,$00,$01,$00,$00,$00,$10
        .byte   $00,$00,$50,$00,$80,$00,$00,$00
        .byte   $00,$00,$18,$18,$3B,$18,$18,$3B
        .byte   $18,$18,$18,$3B,$18,$18,$18,$18
        .byte   $3B,$18,$18,$18,$18,$18,$3B,$78
        .byte   $74,$55,$50,$56,$52,$08,$08,$1C
        .byte   $08,$08,$1C,$08,$50,$55,$1A,$1A
        .byte   $08,$54,$1A,$52,$1A,$08,$08,$1A
        .byte   $1A,$08,$1A,$52,$08,$08,$1A,$1A
        .byte   $08,$1A,$54,$26,$26,$13,$49,$FF
        .byte   $08,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$88,$10,$20,$00,$08,$04
        .byte   $04,$00,$00,$00,$00,$00,$20,$00
        .byte   $40,$00,$20,$00,$90,$00,$00,$40
        .byte   $00,$00,$00,$00,$00,$00,$40,$00
        .byte   $00,$00,$00,$01,$00,$00,$08,$01
        .byte   $18,$00,$00,$00,$00,$00,$80,$00
        .byte   $20,$10,$00,$00,$00,$00,$80,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $03,$00,$00,$00,$00,$40,$00,$40
        .byte   $00,$00,$00,$00,$00,$00,$08,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$01,$00,$00,$A0,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $01,$00,$01,$00,$10,$01,$00,$00
        .byte   $20,$00,$00,$10,$00,$00,$00,$00
        .byte   $10,$00,$28,$00,$09,$00,$00,$00
        .byte   $01,$41,$20,$04,$15,$00,$00,$10
        .byte   $83,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$01,$00,$00,$00,$02
        .byte   $00,$03,$04,$05,$02,$06,$07,$08
        .byte   $09,$0A,$0B,$0C,$0C,$0D,$0B,$0D
        .byte   $0E,$0F,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$01,$03,$00
        .byte   $10,$00,$00,$02,$11,$12,$13,$02
        .byte   $0C,$00,$00,$0C,$0D,$0B,$0B,$14
        .byte   $15,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$03,$00,$00,$00,$00
        .byte   $01,$10,$0C,$16,$09,$02,$01,$00
        .byte   $17,$18,$0C,$19,$0E,$15,$0A,$00
        .byte   $0C,$1A,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$00,$00,$01
        .byte   $07,$00,$00,$17,$13,$02,$01,$17
        .byte   $0B,$00,$00,$0C,$0B,$14,$0F,$0C
        .byte   $0B,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$1B,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$1C,$01,$01,$00
        .byte   $00,$00,$10,$1D,$1E,$04,$11,$07
        .byte   $10,$02,$0C,$0D,$0B,$0B,$0D,$0B
        .byte   $14,$15,$1F,$20,$21,$22,$22,$23
        .byte   $1F,$20,$24,$25,$22,$22,$22,$26
        .byte   $24,$25,$1F,$20,$21,$27,$22,$22
        .byte   $28,$20,$24,$25,$26,$00,$27,$26
        .byte   $24,$25,$1F,$29,$00,$00,$2A,$22
        .byte   $2B,$2C,$24,$25,$26,$00,$2D,$22
        .byte   $22,$22,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$22,$2F
        .byte   $00,$30,$2E,$31,$22,$32,$33,$34
        .byte   $30,$35,$26,$24,$36,$37,$38,$38
        .byte   $39,$38,$22,$22,$3A,$3B,$38,$38
        .byte   $38,$3C,$22,$22,$2B,$37,$24,$25
        .byte   $26,$24,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$20,$2E,$1F,$20,$2E,$1F
        .byte   $3D,$23,$25,$26,$24,$25,$26,$00
        .byte   $3E,$26,$3F,$2E,$1F,$29,$40,$00
        .byte   $3E,$23,$38,$38,$24,$38,$41,$00
        .byte   $2C,$26,$00,$38,$38,$38,$00,$42
        .byte   $22,$23,$25,$26,$24,$25,$26,$24
        .byte   $25,$26,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$26,$24,$25,$26,$24
        .byte   $25,$26,$29,$43,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$44,$39,$22,$2B,$2F
        .byte   $22,$26,$29,$45,$39,$46,$22,$2B
        .byte   $2D,$23,$25,$44,$47,$2A,$22,$22
        .byte   $22,$26,$29,$38,$47,$00,$22,$22
        .byte   $48,$23,$25,$38,$38,$47,$46,$49
        .byte   $00,$26,$20,$2E,$1F,$20,$2E,$1F
        .byte   $4A,$23,$4B,$4C,$4D,$4B,$4C,$4D
        .byte   $4E,$4C,$1F,$29,$2F,$22,$36,$00
        .byte   $00,$2D,$49,$25,$00,$27,$22,$2B
        .byte   $2C,$22,$00,$4F,$00,$2A,$22,$22
        .byte   $22,$22,$24,$25,$00,$2D,$22,$22
        .byte   $22,$22,$1F,$29,$2D,$22,$49,$22
        .byte   $22,$36,$24,$25,$22,$49,$46,$22
        .byte   $22,$00,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$22,$22,$49,$00,$00,$00
        .byte   $27,$22,$22,$48,$00,$00,$00,$00
        .byte   $00,$2A,$36,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$50
        .byte   $51,$00,$00,$00,$00,$00,$00,$52
        .byte   $53,$00,$00,$00,$00,$00,$00,$54
        .byte   $55,$00,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$22,$23,$1F,$29,$22,$22
        .byte   $22,$48,$27,$56,$24,$22,$22,$49
        .byte   $00,$00,$2A,$57,$22,$22,$36,$00
        .byte   $00,$00,$00,$26,$58,$22,$00,$00
        .byte   $00,$00,$59,$23,$31,$48,$00,$00
        .byte   $00,$00,$25,$26,$24,$00,$00,$00
        .byte   $00,$00,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$26,$24,$25,$26,$24
        .byte   $25,$26,$00,$2F,$22,$3A,$2D,$22
        .byte   $22,$3F,$00,$00,$46,$22,$22,$49
        .byte   $27,$25,$00,$00,$00,$2A,$22,$2B
        .byte   $3A,$3F,$50,$51,$00,$00,$27,$22
        .byte   $22,$25,$52,$53,$00,$00,$2C,$22
        .byte   $48,$3F,$54,$55,$00,$2C,$2D,$26
        .byte   $00,$25,$1F,$20,$2E,$1F,$20,$21
        .byte   $00,$3F,$4D,$4B,$4C,$4D,$4B,$4C
        .byte   $00,$4C,$21,$00,$23,$1F,$20,$2E
        .byte   $5A,$49,$26,$00,$2F,$22,$22,$22
        .byte   $49,$00,$21,$00,$00,$00,$2F,$00
        .byte   $00,$00,$26,$2B,$00,$00,$00,$00
        .byte   $00,$00,$21,$22,$36,$00,$00,$00
        .byte   $00,$00,$26,$24,$25,$26,$5B,$5B
        .byte   $5B,$5B,$2E,$1F,$20,$21,$5C,$5D
        .byte   $5E,$5C,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$00,$00,$27,$22,$2B,$2C
        .byte   $22,$49,$00,$00,$2A,$22,$22,$49
        .byte   $48,$00,$00,$00,$00,$2A,$48,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $5F,$00,$00,$60,$4F,$00,$61,$00
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$25,$22,$22,$22,$22,$22,$49
        .byte   $49,$2F,$00,$27,$22,$27,$49,$48
        .byte   $00,$00,$63,$00,$48,$00,$00,$00
        .byte   $00,$00,$00,$00,$59,$00,$64,$00
        .byte   $65,$00,$66,$00,$00,$00,$00,$00
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$26,$22,$22,$67,$1F,$20,$2E
        .byte   $1F,$20,$00,$2F,$68,$25,$22,$49
        .byte   $49,$00,$69,$00,$5F,$00,$2A,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$6A
        .byte   $00,$6B,$5F,$00,$65,$00,$6C,$00
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$00,$00,$2F,$22,$22,$25
        .byte   $22,$22,$00,$00,$00,$00,$27,$22
        .byte   $22,$22,$00,$4F,$6C,$00,$00,$2F
        .byte   $48,$48,$00,$00,$00,$6D,$00,$66
        .byte   $63,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$20,$2E,$1F,$20,$2E,$31
        .byte   $6E,$2E,$22,$22,$22,$49,$00,$00
        .byte   $6F,$00,$22,$27,$48,$00,$00,$00
        .byte   $00,$6C,$00,$00,$00,$00,$00,$00
        .byte   $70,$00,$71,$65,$00,$63,$00,$71
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$29,$43,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$00,$27,$22,$25,$24
        .byte   $25,$26,$29,$00,$00,$27,$22,$28
        .byte   $20,$2E,$25,$26,$42,$22,$22,$22
        .byte   $25,$26,$20,$2E,$31,$22,$49,$2F
        .byte   $22,$23,$25,$26,$24,$25,$00,$00
        .byte   $00,$26,$20,$2E,$1F,$20,$2E,$31
        .byte   $72,$2E,$4B,$4C,$4D,$4B,$4C,$4D
        .byte   $73,$4C,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$22,$2B,$3A,$27,$3A
        .byte   $25,$26,$29,$00,$27,$22,$22,$22
        .byte   $3F,$2E,$25,$74,$24,$22,$22,$49
        .byte   $25,$26,$29,$43,$31,$27,$22,$2B
        .byte   $00,$75,$25,$76,$24,$00,$27,$22
        .byte   $2B,$75,$29,$43,$1F,$20,$2E,$1F
        .byte   $20,$2E,$4B,$76,$4D,$4B,$4C,$4D
        .byte   $4B,$4C,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$77,$2D,$22,$27,$22,$36
        .byte   $78,$75,$79,$22,$48,$2A,$22,$22
        .byte   $48,$75,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$7A,$7B,$7B,$7B,$7B,$7B
        .byte   $7B,$7C,$7D,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$7F,$80,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$81,$82,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$7F,$83,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$81,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$2A,$22,$2B,$37,$38
        .byte   $47,$00,$29,$6B,$22,$49,$30,$38
        .byte   $38,$40,$25,$22,$22,$00,$35,$38
        .byte   $25,$26,$29,$2A,$22,$36,$84,$28
        .byte   $20,$2E,$25,$00,$46,$22,$26,$24
        .byte   $25,$26,$29,$85,$1F,$20,$2E,$1F
        .byte   $20,$2E,$4B,$76,$4D,$4B,$4C,$4D
        .byte   $4B,$4C,$00,$86,$00,$00,$87,$86
        .byte   $00,$00,$00,$88,$00,$89,$8A,$00
        .byte   $87,$00,$8A,$00,$8B,$00,$00,$00
        .byte   $00,$00,$00,$00,$88,$8A,$00,$89
        .byte   $86,$00,$8C,$00,$00,$00,$8D,$00
        .byte   $00,$00,$10,$01,$10,$01,$10,$10
        .byte   $03,$00,$0C,$0F,$14,$04,$0C,$15
        .byte   $05,$02,$0C,$8E,$19,$0B,$0C,$0B
        .byte   $0C,$0C,$8F,$00,$00,$00,$00,$00
        .byte   $8F,$00,$00,$90,$89,$91,$92,$93
        .byte   $00,$00,$8A,$00,$00,$94,$95,$96
        .byte   $86,$8A,$00,$8B,$97,$98,$99,$9A
        .byte   $00,$00,$89,$00,$8A,$00,$00,$8A
        .byte   $89,$00,$00,$00,$1C,$01,$01,$00
        .byte   $03,$00,$01,$10,$0C,$0F,$05,$01
        .byte   $9B,$02,$14,$15,$0C,$9C,$0C,$0A
        .byte   $0B,$0C,$00,$00,$00,$00,$88,$00
        .byte   $00,$00,$8B,$86,$8B,$00,$00,$8A
        .byte   $88,$00,$8A,$00,$00,$86,$90,$00
        .byte   $00,$89,$00,$88,$86,$00,$00,$88
        .byte   $9D,$00,$00,$00,$00,$8A,$00,$00
        .byte   $00,$00,$00,$00,$1C,$03,$03,$00
        .byte   $00,$03,$06,$9E,$0C,$16,$9B,$10
        .byte   $02,$9F,$0D,$0B,$0C,$0D,$0B,$0C
        .byte   $0C,$0E,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$01,$10,$03,$00,$1C
        .byte   $03,$02,$15,$14,$15,$9F,$00,$0C
        .byte   $11,$1E,$0B,$0D,$0B,$0E,$00,$0C
        .byte   $0D,$0B,$1F,$20,$2E,$1F,$20,$21
        .byte   $00,$3F,$24,$49,$27,$22,$27,$49
        .byte   $00,$25,$31,$A0,$A1,$2B,$00,$00
        .byte   $00,$3F,$24,$27,$22,$22,$48,$00
        .byte   $24,$25,$31,$2A,$49,$00,$00,$23
        .byte   $1F,$20,$24,$00,$00,$00,$25,$26
        .byte   $24,$25,$31,$00,$23,$1F,$20,$2E
        .byte   $1F,$20,$4D,$00,$4C,$4D,$4B,$4C
        .byte   $4D,$4C,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$A2,$A3,$00
        .byte   $00,$00,$01,$09,$02,$A4,$A5,$06
        .byte   $07,$02,$0A,$0E,$0C,$A4,$A5,$0D
        .byte   $0B,$0C,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$70,$71
        .byte   $78,$79,$70,$72,$78,$7A,$00,$00
        .byte   $70,$71,$71,$79,$79,$7B,$78,$7B
        .byte   $78,$7A,$70,$71,$78,$70,$70,$72
        .byte   $71,$7A,$78,$7A,$78,$70,$70,$71
        .byte   $71,$79,$78,$79,$78,$79,$79,$7A
        .byte   $79,$7A,$78,$7A,$78,$7A,$78,$78
        .byte   $78,$78,$79,$79,$79,$79,$71,$79
        .byte   $79,$79,$70,$71,$78,$7B,$78,$7B
        .byte   $78,$70,$78,$79,$71,$7B,$71,$7B
        .byte   $79,$7A,$78,$70,$78,$78,$71,$7A
        .byte   $79,$7A,$78,$79,$78,$70,$78,$79
        .byte   $78,$7B,$72,$7A,$7A,$7A,$71,$78
        .byte   $79,$78,$7A,$7A,$7A,$7A,$60,$60
        .byte   $00,$00,$00,$00,$70,$72,$70,$72
        .byte   $78,$70,$78,$7A,$71,$7A,$11,$12
        .byte   $19,$1A,$13,$14,$1B,$1C,$15,$16
        .byte   $1D,$16,$20,$21,$28,$29,$1F,$10
        .byte   $1F,$18,$14,$15,$1C,$1D,$10,$11
        .byte   $18,$19,$12,$13,$1A,$1B,$20,$21
        .byte   $00,$29,$16,$12,$16,$1A,$13,$1F
        .byte   $1B,$1F,$00,$21,$00,$00,$20,$00
        .byte   $28,$29,$00,$00,$00,$29,$00,$21
        .byte   $28,$29,$15,$10,$1D,$18,$20,$21
        .byte   $00,$00,$00,$00,$00,$09,$11,$17
        .byte   $19,$17,$1F,$16,$1F,$16,$00,$00
        .byte   $16,$17,$09,$00,$09,$00,$00,$09
        .byte   $09,$09,$20,$00,$28,$00,$00,$09
        .byte   $00,$00,$09,$09,$09,$09,$09,$09
        .byte   $09,$00,$00,$00,$28,$00,$09,$09
        .byte   $00,$09,$09,$09,$16,$17,$13,$07
        .byte   $1B,$07,$00,$07,$00,$07,$17,$14
        .byte   $17,$1C,$00,$00,$09,$00,$09,$00
        .byte   $00,$00,$00,$00,$28,$29,$07,$10
        .byte   $07,$18,$07,$09,$07,$09,$07,$09
        .byte   $07,$00,$00,$21,$00,$29,$09,$00
        .byte   $09,$09,$20,$00,$00,$00,$20,$21
        .byte   $28,$00,$13,$0F,$1B,$07,$16,$16
        .byte   $16,$16,$17,$17,$17,$17,$1F,$1F
        .byte   $1F,$1F,$16,$07,$16,$07,$17,$1F
        .byte   $00,$00,$00,$00,$00,$3A,$00,$00
        .byte   $3B,$00,$24,$25,$2C,$2D,$26,$27
        .byte   $2E,$2F,$34,$35,$3C,$3D,$36,$37
        .byte   $3E,$3F,$20,$17,$28,$17,$20,$16
        .byte   $00,$29,$20,$21,$1F,$1F,$00,$00
        .byte   $17,$1F,$11,$21,$19,$29,$00,$00
        .byte   $22,$23,$2A,$2B,$16,$17,$2A,$2B
        .byte   $17,$1F,$2A,$2B,$1F,$16,$00,$00
        .byte   $17,$00,$1F,$16,$00,$00,$00,$16
        .byte   $00,$00,$2A,$2B,$2A,$2B,$00,$00
        .byte   $00,$16,$16,$17,$00,$00,$00,$1F
        .byte   $00,$00,$1F,$00,$00,$00,$20,$10
        .byte   $28,$18,$20,$1F,$28,$1F,$00,$00
        .byte   $16,$00,$00,$00,$00,$17,$00,$00
        .byte   $1F,$16,$16,$00,$00,$00,$00,$17
        .byte   $00,$00,$07,$14,$07,$1C,$07,$00
        .byte   $00,$00,$00,$00,$00,$1F,$17,$00
        .byte   $00,$00,$0F,$14,$07,$1C,$07,$16
        .byte   $07,$16,$0F,$17,$07,$17,$00,$06
        .byte   $00,$06,$07,$17,$07,$17,$05,$00
        .byte   $05,$00,$00,$21,$28,$00,$05,$21
        .byte   $05,$29,$17,$31,$17,$30,$31,$31
        .byte   $30,$30,$31,$1F,$30,$1F,$15,$30
        .byte   $1D,$30,$30,$30,$30,$30,$30,$12
        .byte   $30,$1A,$17,$30,$17,$30,$30,$1F
        .byte   $30,$1F,$04,$30,$03,$30,$03,$30
        .byte   $03,$30,$09,$09,$00,$00,$0F,$10
        .byte   $07,$18,$00,$00,$00,$43,$00,$00
        .byte   $00,$41,$00,$00,$42,$00,$00,$42
        .byte   $00,$00,$43,$00,$00,$00,$41,$00
        .byte   $00,$00,$00,$43,$00,$00,$00,$41
        .byte   $00,$00,$79,$70,$79,$78,$00,$00
        .byte   $00,$42,$43,$00,$00,$43,$00,$00
        .byte   $00,$4A,$00,$44,$4B,$4C,$45,$46
        .byte   $4D,$4E,$51,$52,$59,$5A,$53,$54
        .byte   $5B,$5C,$55,$56,$5D,$00,$00,$00
        .byte   $00,$68,$61,$62,$69,$6A,$63,$64
        .byte   $6B,$00,$65,$00,$00,$00,$78,$7B
        .byte   $71,$7A,$79,$7B,$79,$7A,$41,$00
        .byte   $00,$43,$70,$71,$71,$7B,$78,$79
        .byte   $71,$79,$00,$00,$28,$16,$20,$21
        .byte   $17,$29,$00,$00,$74,$75,$00,$00
        .byte   $76,$77,$7C,$7D,$7C,$7D,$7E,$7F
        .byte   $7E,$7F,$20,$06,$00,$06,$16,$17
        .byte   $28,$29,$1F,$00,$1F,$00,$11,$00
        .byte   $19,$00,$00,$14,$00,$1C,$00,$00
        .byte   $10,$11,$00,$00,$12,$13,$00,$00
        .byte   $14,$15,$18,$19,$17,$14,$1A,$1B
        .byte   $15,$10,$1C,$1D,$11,$17,$17,$1C
        .byte   $10,$11,$1D,$18,$12,$13,$19,$17
        .byte   $14,$15,$00,$00,$09,$09,$00,$16
        .byte   $00,$16,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$13,$02,$00
        .byte   $14,$04,$22,$24,$01,$01,$01,$01
        .byte   $01,$04,$08,$0A,$08,$0A,$0C,$0E
        .byte   $06,$06,$28,$2A,$28,$2A,$2C,$2E
        .byte   $00,$26,$4C,$4E,$40,$42,$00,$8A
        .byte   $8C,$00,$6C,$6E,$60,$62,$00,$AA
        .byte   $AC,$AE,$13,$02,$00,$00,$C8,$CA
        .byte   $CC,$CE,$00,$00,$00,$8F,$E8,$EA
        .byte   $EC,$EE,$44,$77,$76,$00,$00,$68
        .byte   $6A,$4B,$44,$67,$00,$82,$84,$00
        .byte   $74,$01,$00,$00,$A0,$A2,$94,$A6
        .byte   $EB,$01,$00,$00,$C0,$C1,$B5,$C6
        .byte   $01,$01,$00,$B0,$E0,$E2,$E4,$E6
        .byte   $01,$00,$00,$D2,$D5,$E1,$01,$01
        .byte   $01,$00,$48,$4A,$4A,$01,$E0,$E2
        .byte   $E4,$E6,$58,$5A,$5A,$5A,$F8,$FA
        .byte   $FC,$FE,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$12,$03,$00
        .byte   $15,$05,$23,$25,$01,$65,$01,$01
        .byte   $01,$05,$09,$0B,$09,$0B,$0D,$0F
        .byte   $07,$07,$29,$2B,$29,$2B,$2D,$2F
        .byte   $00,$27,$4D,$4F,$41,$43,$89,$8B
        .byte   $8D,$00,$6D,$6F,$61,$63,$A9,$AB
        .byte   $AD,$00,$12,$03,$00,$00,$C9,$CB
        .byte   $CD,$CF,$00,$00,$88,$00,$E9,$00
        .byte   $ED,$EF,$44,$00,$00,$00,$00,$69
        .byte   $6B,$00,$66,$44,$81,$83,$85,$00
        .byte   $75,$00,$01,$80,$A1,$A3,$95,$A7
        .byte   $00,$00,$01,$87,$C1,$B4,$C5,$C7
        .byte   $01,$00,$00,$B1,$D0,$E3,$E5,$00
        .byte   $00,$00,$C4,$D3,$D6,$00,$01,$01
        .byte   $00,$00,$49,$4B,$4B,$01,$E1,$E3
        .byte   $E5,$E7,$59,$5B,$57,$47,$F9,$FB
        .byte   $FD,$FF,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$12,$03,$00
        .byte   $14,$04,$32,$34,$01,$01,$01,$01
        .byte   $01,$04,$18,$1A,$18,$1A,$1C,$1E
        .byte   $16,$16,$38,$3A,$38,$3A,$3C,$3E
        .byte   $00,$36,$5C,$5E,$70,$72,$00,$9A
        .byte   $9C,$9E,$7C,$7E,$70,$72,$B8,$BA
        .byte   $BC,$BE,$12,$03,$00,$00,$D8,$DA
        .byte   $DC,$DE,$00,$00,$00,$9F,$F8,$FA
        .byte   $FC,$FE,$44,$00,$00,$00,$00,$78
        .byte   $7A,$5B,$54,$01,$90,$92,$94,$96
        .byte   $AF,$01,$00,$00,$A0,$B2,$B4,$B6
        .byte   $00,$01,$00,$00,$D0,$D1,$D4,$E5
        .byte   $01,$00,$00,$C2,$00,$F2,$F4,$00
        .byte   $01,$00,$00,$F0,$F7,$00,$01,$00
        .byte   $00,$00,$58,$5A,$5A,$00,$F0,$F2
        .byte   $F4,$F6,$58,$5A,$5A,$5A,$F8,$FA
        .byte   $FC,$FE,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$13,$02,$00
        .byte   $15,$05,$33,$35,$01,$01,$01,$01
        .byte   $01,$05,$19,$1B,$19,$1B,$1D,$1F
        .byte   $17,$17,$39,$3B,$39,$3B,$3D,$3F
        .byte   $00,$37,$5D,$5F,$71,$73,$99,$9B
        .byte   $9D,$00,$7D,$7F,$71,$73,$B9,$BB
        .byte   $BD,$BF,$13,$02,$00,$00,$D9,$DB
        .byte   $DD,$DF,$00,$00,$98,$00,$F9,$FB
        .byte   $FD,$FF,$44,$76,$00,$77,$A8,$79
        .byte   $7B,$00,$01,$01,$91,$93,$95,$97
        .byte   $00,$00,$01,$86,$A1,$B3,$B5,$B7
        .byte   $00,$00,$01,$A5,$D1,$B5,$E4,$D7
        .byte   $00,$00,$00,$C3,$F1,$F3,$F5,$00
        .byte   $00,$00,$E7,$F6,$A4,$00,$01,$00
        .byte   $00,$00,$59,$5B,$47,$00,$F1,$F3
        .byte   $F5,$F7,$59,$5B,$57,$57,$F9,$FB
        .byte   $FD,$FF,$00,$00,$00,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$13,$13,$10
        .byte   $10,$23,$53,$73,$10,$10,$00,$00
        .byte   $00,$43,$10,$10,$11,$11,$11,$11
        .byte   $10,$11,$10,$10,$11,$11,$11,$11
        .byte   $00,$11,$62,$62,$82,$82,$03,$03
        .byte   $03,$03,$62,$62,$82,$82,$03,$03
        .byte   $03,$03,$03,$03,$00,$00,$03,$03
        .byte   $03,$03,$00,$00,$03,$03,$03,$03
        .byte   $03,$03,$00,$00,$00,$00,$01,$01
        .byte   $01,$01,$00,$00,$02,$02,$02,$01
        .byte   $01,$00,$00,$02,$02,$02,$02,$01
        .byte   $00,$00,$00,$01,$02,$02,$01,$01
        .byte   $00,$00,$10,$01,$01,$01,$01,$01
        .byte   $00,$00,$01,$01,$01,$01,$00,$00
        .byte   $00,$00,$13,$13,$13,$00,$00,$00
        .byte   $00,$00,$13,$13,$13,$13,$00,$00
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
        .byte   $00,$00,$00,$00,$00
        brk
        brk
        brk
        brk
        brk
