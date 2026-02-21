; =============================================================================
; MEGA MAN 3 (U) — BANK $0C — GAME OVER / RESULTS SCREEN + WILY 1 STAGE
; =============================================================================
; Game over screen, stage results display, and Wily Castle stage 1 data.
;
; Annotation: 0% — unannotated da65 output
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $0C — GAME OVER / RESULTS SCREEN + WILY 1 STAGE
; =============================================================================
; Mapped to $8000-$9FFF. Contains game over screen rendering, palette setup,
; OAM/nametable filling, and results display logic.
; Also serves as Wily Fortress 1 stage data ($22=$0C) at $A000-$BFFF.
;
; Annotation: light — all labels auto-generated, game over logic bare
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

LA000           := $A000
LA003           := $A003
LA006           := $A006
LC531           := $C531
LC53B           := $C53B
LC59D           := $C59D
LC5E9           := $C5E9
LC628           := $C628
LC74C           := $C74C
LC752           := $C752
LE8B4           := $E8B4
LEEAB           := $EEAB
LEF8C           := $EF8C
LF835           := $F835
LF898           := $F898
LFD6E           := $FD6E
LFD80           := $FD80
LFF1A           := $FF1A
LFF21           := $FF21
LFF3C           := $FF3C
LFF6B           := $FF6B

.segment "BANK0C"

        lda     #$00
        sta     nmi_skip
        ldx     #$B4
        jsr     LFF1A
        jsr     LC752
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LC628
        jsr     LFF21
        lda     #$F0
        jsr     LF898
        lda     #$00
        sta     $B1
        sta     $B2
        sta     $B3
        sta     $70
        sta     camera_x_hi
        sta     camera_x_lo
        sta     game_mode
        jsr     LC531
        lda     #$20
        ldx     #$00
        ldy     #$00
        jsr     LC59D
        jsr     LC53B
        ldy     #$1F
code_803F:  lda     L863E,y
        sta     $0620,y
        dey
        bpl     code_803F
        ldy     #$05
code_804A:  lda     L8626,y
        sta     $E8,y
        dey
        bpl     code_804A
        lda     #$66
        sta     $E8
        jsr     LFF3C
        jsr     LFF21
        lda     #$0D
        sta     prg_bank
        jsr     LFF6B
        ldx     #$12
        lda     #$00
        sta     $10
        jsr     code_85F3
        jsr     LFF21
        jsr     LC74C
        ldx     #$F0
        jsr     LFF1A
        jsr     LC752
        lda     #$16
        sta     stage_id
        lda     #$02
        jsr     LE8B4
code_8084:  lda     #$00
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_8084
        lda     #$78
        sta     $E8
        jsr     LFF3C
        ldy     #$01
code_809B:  lda     #$80
        sta     ent_status,y
        lda     #$90
        sta     ent_flags,y
        lda     L86D0,y
        sta     ent_anim_id,y
        lda     L86D2,y
        sta     ent_x_px,y
        lda     L86D4,y
        sta     ent_y_px,y
        lda     #$00
        sta     ent_x_scr,y
        sta     ent_anim_frame,y
        sta     ent_anim_state,y
        sta     ent_y_scr,y
        sta     ent_yvel_sub,y
        sta     ent_yvel,y
        dey
        bpl     code_809B
        ldy     #$07
code_80D0:  lda     L869E,y
        sta     $0200,y
        dey
        bpl     code_80D0
        lda     #$11
        sta     game_mode
        lda     #$C0
        sta     $5E
        jsr     LFF21
        jsr     LC74C
        lda     #$00
        sta     $0104
        sta     ent_var1
        sta     $B8
        lda     #$0B
        sta     ent_timer
code_80F6:  lda     ent_x_px
        cmp     #$D0
        bne     code_813F
        lda     #$13
        cmp     ent_anim_id
        beq     code_810C
        ldx     #$00
        jsr     LF835
        inc     ent_anim_state
code_810C:  lda     ent_anim_state
        bne     code_813C
        sta     ent_anim_frame
        lda     ent_y_sub
        sec
        sbc     ent_yvel_sub
        sta     ent_y_sub
        lda     ent_y_px
        sbc     ent_yvel
        sta     ent_y_px
        bcs     code_812C
        jmp     code_81E1

code_812C:  lda     ent_yvel_sub
        adc     #$3F
        sta     ent_yvel_sub
        lda     ent_yvel
        adc     #$00
        sta     ent_yvel
code_813C:  jmp     code_81AA

code_813F:  lda     ent_var1
        bne     code_817F
        lda     #$0E
        cmp     ent_timer
        beq     code_8193
        sta     prg_bank
        jsr     LFF6B
        lda     $95
        and     #$03
        bne     code_81AA
        lda     $B8
        bne     code_816C
        ldx     ent_timer
        cpx     #$0C
        bne     code_8166
        lda     #$12
        jsr     LF898
code_8166:  jsr     LA006
        jmp     code_81AA

code_816C:  jsr     LA003
        lda     $B8
        cmp     #$FF
        bne     code_81AA
        inc     ent_timer
        lda     #$B4
        sta     ent_var1
        bne     code_81AA
code_817F:  lda     #$00
        sta     $05E1
        dec     ent_var1
        bne     code_81AA
        sta     $B8
        sta     nmi_skip
        jsr     LA000
        jmp     code_81AA

code_8193:  inc     ent_x_px
        lda     #$04
        cmp     ent_anim_id
        beq     code_81AA
        ldx     #$00
        jsr     LF835
        lda     ent_flags
        ora     #$40
        sta     ent_flags
code_81AA:  lda     $95
        and     #$03
        bne     code_81D7
        lda     $0104
        asl     a
        adc     $0104
        tay
        ldx     #$05
code_81BA:  lda     L86D6,y
        sta     $0600,x
        iny
        inx
        cpx     #$08
        bne     code_81BA
        stx     palette_dirty
        inc     $0104
        lda     $0104
        cmp     #$06
        bne     code_81D7
        lda     #$00
        sta     $0104
code_81D7:  lda     #$08
        sta     oam_ptr
        jsr     LFD80
        jmp     code_80F6

code_81E1:  lda     #$00
        sta     nmi_skip
        jsr     LC752
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LC628
        jsr     LFF21
        lda     #$00
        sta     LA000
        ldy     #$0F
code_81FC:  lda     L865E,y
        sta     $0620,y
        dey
        bpl     code_81FC
        lda     #$22
        sta     $0630
        lda     #$14
        sta     stage_id
        lda     #$00
        jsr     LE8B4
code_8213:  lda     #$04
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_8213
        lda     #$01
        jsr     LE8B4
code_8226:  lda     #$00
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_8226
        ldy     #$05
code_8236:  lda     L862C,y
        sta     $E8,y
        dey
        bpl     code_8236
        jsr     LFF3C
        lda     #$34
        sta     ent_y_px
        lda     #$F8
        sta     ent_x_px
        lda     #$80
        sta     ent_flags
        ldx     #$00
        lda     #$04
        jsr     LF835
        lda     #$05
        sta     camera_x_hi
        lda     #$01
        sta     $29
        sta     $6B
        lda     #$1F
        sta     $28
        lda     #$0D
        sta     prg_bank
        jsr     LFF6B
        lda     #$04
        sta     $10
        ldx     #$00
        jsr     code_85F3
        jsr     LFF21
        ldx     #$01
        jsr     code_85F3
        jsr     LFF21
        ldx     #$10
        jsr     code_85F3
        jsr     LFF21
        ldx     #$11
        jsr     code_85F3
        jsr     LFF21
        ldx     #$06
        jsr     code_85F3
        lda     #$0F
        sta     game_mode
        lda     #$40
        sta     $5E
        lda     #$00
        sta     $69
        sta     $6A
        sta     ent_timer
        sta     ent_var1
        sta     ent_var2
        sta     ent_var3
        jsr     LFF21
        jsr     LC74C
code_82B6:  lda     ent_x_px
        cmp     #$80
        beq     code_82C9
        lda     $95
        and     #$01
        bne     code_82C6
        dec     ent_x_px
code_82C6:  jmp     code_8354

code_82C9:  lda     camera_x_lo
        ora     camera_x_hi
        bne     code_82D9
        ldx     #$00
        lda     #$64
        jsr     LF835
        jmp     code_836D

code_82D9:  dec     $69
        lda     $95
        and     #$01
        bne     code_82F9
        lda     camera_x_lo
        sec
        sbc     #$01
        sta     camera_x_lo
        pha
        lda     camera_x_hi
        sbc     #$00
        sta     camera_x_hi
        pla
        and     #$03
        cmp     #$03
        bne     code_82F9
        jsr     code_85BD
code_82F9:  lda     ent_timer
        bne     code_8335
        lda     $6A
        sec
        sbc     #$04
        sta     $6A
        lda     $6B
        sbc     #$00
        and     #$01
        sta     $6B
        beq     code_8354
        lda     $6A
        bne     code_833B
        lda     #$A1
        sta     ent_timer
        lda     #$0D
        sta     prg_bank
        jsr     LFF6B
        jsr     LA000
        lda     #$00
        sta     ent_var1
        lda     #$04
        sta     $10
        lda     ent_var2
        clc
        adc     #$06
        tax
        jsr     code_85F3
code_8335:  dec     ent_timer
        jmp     code_8354

code_833B:  lda     nametable_dirty
        bne     code_8354
        lda     ent_var1
        cmp     #$04
        beq     code_8354
        inc     ent_var1
        clc
        adc     #$02
        tax
        lda     #$04
        sta     $10
        jsr     code_85F3
code_8354:  lda     ent_timer
        bne     code_8362
        lda     #$04
        sta     oam_ptr
        sta     ent_var3
        bne     code_8367
code_8362:  lda     ent_var3
        sta     oam_ptr
code_8367:  jsr     LFD80
        jmp     code_82B6

code_836D:  lda     ent_var3
        sta     oam_ptr
        jsr     LFD80
        lda     ent_anim_state
        beq     code_836D
        lda     #$00
        sta     ent_anim_frame
        ldx     #$78
        jsr     LFF1A
        lda     #$10
        sta     game_mode
        lda     #$88
        sta     $5E
code_838C:  inc     $5E
        lda     $5E
        cmp     #$E8
        beq     code_839F
        jsr     LFD6E
        lda     #$00
        sta     ent_anim_frame
        jmp     code_838C

code_839F:  lda     #$01
        sta     LA000
        lda     #$00
        sta     nmi_skip
        sta     game_mode
        lda     #$06
        jsr     LE8B4
code_83AF:  lda     #$08
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_83AF
        lda     #$02
        sta     camera_x_hi
        lda     #$EF
        sta     scroll_y
code_83C5:  inc     ent_y_px
        dec     scroll_y
        lda     scroll_y
        cmp     #$70
        beq     code_83DB
        lda     #$00
        sta     ent_anim_frame
        jsr     LFD6E
        jmp     code_83C5

code_83DB:  lda     #$65
        sta     ent_anim_id
        lda     #$00
        sta     ent_anim_frame
        sta     ent_anim_state
        ldy     #$01
code_83EA:  lda     #$80
        sta     $0301,y
        sta     $0581,y
        lda     L86A6,y
        sta     $05C1,y
        lda     #$00
        sta     $05E1,y
        sta     $05A1,y
        sta     $03E1,y
        sta     $0381,y
        sta     $0501,y
        sta     $0521,y
        lda     L86A8,y
        sta     $03C1,y
        lda     L86AA,y
        sta     $0361,y
        dey
        bpl     code_83EA
        lda     #$08
        sta     $0522
        ldy     #$07
code_8422:  lda     L86B0,y
        sta     $0618,y
        sta     $0638,y
        dey
        bpl     code_8422
        sty     palette_dirty
code_8430:  lda     $95
        and     #$01
        bne     code_8460
        dec     $0362
        lda     $0362
        cmp     #$B8
        beq     code_8466
        lda     $0502
        and     #$03
        tay
        lda     L86AC,y
        bne     code_8450
        dec     $03C2
        bne     code_8453
code_8450:  inc     $03C2
code_8453:  dec     $0522
        bne     code_8460
        lda     #$08
        sta     $0522
        inc     $0502
code_8460:  jsr     LFD6E
        jmp     code_8430

code_8466:  ldx     #$F0
        jsr     LFF1A
        lda     #$00
        sta     nmi_skip
        jsr     LC752
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        jsr     LC628
        jsr     LFF21
        lda     #$00
        sta     LA000
        sta     ent_status
        lda     #$0F
        jsr     LF898
        lda     #$13
        sta     prg_bank
        sta     stage_id
        jsr     LFF6B
        lda     #$03
        jsr     LE8B4
        lda     #$00
        sta     $70
code_849E:  lda     #$00
        sta     $10
        jsr     LEF8C
        jsr     LFF21
        lda     $70
        bne     code_849E
        jsr     LC531
        lda     #$24
        ldx     #$24
        ldy     #$00
        jsr     LC59D
        jsr     LC53B
        jsr     LFF21
        ldy     #$05
code_84C0:  lda     L8632,y
        sta     $E8,y
        dey
        bpl     code_84C0
        jsr     LFF3C
        ldy     #$0F
code_84CE:  lda     L866E,y
        sta     $0620,y
        dey
        bpl     code_84CE
        lda     #$0F
        sta     $0630
        lda     #$00
        sta     camera_x_lo
        sta     camera_x_hi
        sta     scroll_y
        sta     $10
        ldx     #$13
        jsr     code_85F3
        jsr     LFF21
        jsr     LC74C
        ldx     #$B4
        jsr     LFF1A
        lda     #$00
        sta     ent_timer
        lda     #$80
        sta     $0310
        sta     $0590
        sta     boss_active
        lda     #$70
        sta     $03D0
        lda     #$58
        sta     $0370
code_850F:  lda     #$00
        sta     $10
        sta     $0310
        ldx     #$14
        jsr     code_85F3
        jsr     LFF21
        lda     #$01
        sta     prg_bank
        jsr     LFF6B
        ldx     ent_timer
        lda     L86B8,x
        sta     $05D0
        lda     #$00
        sta     $05F0
        sta     $05B0
        lda     L86C0,x
        jsr     LA000
        jsr     LFF3C
        lda     #$80
        sta     $0310
code_8544:  jsr     LFD6E
        ldx     ent_timer
        lda     L86C8,x
        cmp     $05B0
        bne     code_8544
        lda     #$00
        sta     nmi_skip
        lda     #$0E
        sta     prg_bank
        jsr     LFF6B
        ldx     ent_timer
        jsr     LA006
        jsr     LFF21
code_8566:  jsr     LA003
        lda     $B8
        cmp     #$FF
        beq     code_8577
        ldx     #$04
        jsr     LFF1A
        jmp     code_8566

code_8577:  ldx     #$B4
        jsr     LFF1A
        inc     ent_timer
        lda     ent_timer
        cmp     #$08
        beq     code_8589
        jmp     code_850F

code_8589:  ldy     #$1F
code_858B:  lda     L867E,y
        sta     $0600,y
        dey
        bpl     code_858B
        sty     palette_dirty
        ldy     #$05
code_8598:  lda     L8638,y
        sta     $E8,y
        dey
        bpl     code_8598
        jsr     LFF3C
        lda     #$04
        sta     oam_ptr
        jsr     LC5E9
        lda     #$00
        sta     $B8
        lda     #$01
        sta     camera_x_hi
        lda     #$0F
        sta     prg_bank
        jsr     LFF6B
        jmp     LA000

code_85BD:  lda     $28
        pha
        lsr     a
        lsr     a
        lsr     a
        and     #$07
        sta     $00
        lda     $28
        asl     a
        asl     a
        asl     a
        and     #$38
        ora     $00
        sta     $28
        cmp     #$10
        bcs     code_85E5
        lda     $29
        eor     #$01
        asl     a
        asl     a
        and     #$04
        sta     $10
        ldy     $29
        jsr     LEEAB
code_85E5:  pla
        sta     $28
        dec     $28
        bpl     code_85F2
        lda     #$3F
        sta     $28
        inc     $29
code_85F2:  rts

code_85F3:  lda     L8903,x
        sta     $02
        lda     L8918,x
        sta     $03
        ldy     #$00
code_85FF:  lda     ($02),y
        ora     $10
        sta     $0780,y
        bmi     code_8623
        iny
        lda     ($02),y
        sta     $0780,y
        iny
        lda     ($02),y
        sta     $0780,y
        sta     $00
        iny
code_8617:  lda     ($02),y
        sta     $0780,y
        iny
        dec     $00
        bpl     code_8617
        bmi     code_85FF
code_8623:  .byte   $85,$19,$60
L8626:  .byte   $78,$7A,$00,$01,$1B,$3B
L862C:  .byte   $78,$7A,$00,$79,$3E,$3F
L8632:  .byte   $7C,$7E,$00,$79,$3E,$3F
L8638:  .byte   $7C,$7E,$00,$03,$15,$17
L863E:  .byte   $0F,$20,$2C,$1C,$0F,$1C,$27,$16
        .byte   $0F,$3B,$2B,$1B,$0F,$32,$22,$12
        .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
        .byte   $0F,$35,$25,$15,$0F,$0F,$30,$11
L865E:  .byte   $0F,$0F,$1C,$21,$0F,$20,$21,$1C
        .byte   $0F,$2A,$1A,$0A,$0F,$0F,$08,$0A
L866E:  .byte   $0F,$20,$21,$11,$0F,$27,$17,$06
        .byte   $0F,$27,$17,$21,$0F,$20,$10,$21
L867E:  .byte   $0F,$20,$0F,$0F,$0F,$20,$0F,$0F
        .byte   $0F,$20,$0F,$0F,$0F,$20,$0F,$0F
        .byte   $0F,$0F,$30,$15,$0F,$0F,$30,$37
        .byte   $0F,$0F,$30,$19,$0F,$0F,$30,$16
L869E:  .byte   $68,$BE,$02,$18,$68,$BF,$02,$20
L86A6:  .byte   $66,$63
L86A8:  .byte   $38,$10
L86AA:  .byte   $30,$F8
L86AC:  .byte   $00,$FF,$FF,$00
L86B0:  .byte   $0F,$2C,$2C,$2C,$0F,$3C,$2C,$1C
L86B8:  .byte   $26,$1F,$32,$2B,$45,$22,$36,$3F
L86C0:  .byte   $25,$23,$27,$24,$2A,$26,$28,$29
L86C8:  .byte   $04,$03,$05,$06,$02,$02,$08,$03
L86D0:  .byte   $01,$60
L86D2:  .byte   $98,$58
L86D4:  .byte   $A4,$A4
L86D6:  .byte   $1C,$27,$16,$0F,$1C,$1A,$16,$0F
        .byte   $0F,$0F,$1A,$16,$17,$0F,$0F,$1A
        .byte   $16,$0F,$26,$46,$13,$6C,$6D,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$6E
        .byte   $6F,$26,$66,$13,$7C,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$7F
        .byte   $26,$86,$13,$EE,$EE,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$EE,$EE,$FF
        .byte   $26,$A5,$14,$7D,$7E,$EE,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$EE,$EE
        .byte   $26,$C6,$13,$EE,$EE,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$EE,$EE,$26
        .byte   $E6,$13,$EE,$EE,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$EE,$EE,$FF,$27
        .byte   $05,$14,$7D,$7E,$EE,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$EE,$EE,$FF
        .byte   $27,$26,$13,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$27
        .byte   $46,$13,$EC,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EF,$27,$66
        .byte   $13,$FC,$FD,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE
        .byte   $EE,$EE,$EE,$FE,$FF,$FF,$26,$46
        .byte   $13,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$26,$66,$13
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$26,$86,$13,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$FF,$26,$A5,$14,$7D
        .byte   $ED,$00,$7B,$7B,$7B,$7B,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$26,$C6,$13,$00
        .byte   $00,$7B,$7B,$7B,$7B,$00,$79,$79
        .byte   $79,$79,$79,$79,$79,$79,$79,$79
        .byte   $79,$79,$00,$26,$E6,$13,$00,$00
        .byte   $7B,$7B,$7B,$7B,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$FF,$27,$05,$14,$7D,$ED
        .byte   $00,$7B,$7B,$7B,$7B,$00,$79,$79
        .byte   $79,$79,$79,$79,$79,$79,$79,$79
        .byte   $79,$79,$00,$27,$26,$13,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$27,$46,$13,$00,$00,$00
        .byte   $00,$00,$00,$00,$79,$79,$79,$79
        .byte   $79,$79,$79,$79,$79,$79,$79,$79
        .byte   $00,$FF,$27,$66,$13,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$FF,$21,$EE,$04,$1C,$1D,$0A
        .byte   $0F,$0F,$FF,$21,$8C,$07,$25,$25
        .byte   $25,$25,$25,$25,$25,$25,$21,$AC
        .byte   $0C,$25,$25,$25,$25,$25,$25,$25
        .byte   $25,$25,$25,$25,$25,$25,$21,$EC
        .byte   $0B,$25,$25,$25,$25,$25,$25,$25
        .byte   $25,$25,$25,$25,$25,$22,$0C,$0B
        .byte   $25,$25,$25,$25,$25,$25,$25,$25
        .byte   $25,$25,$25,$25,$FF
L8903:  .byte   $E8,$8E,$D4,$1A,$61,$A8,$B4,$E5
        .byte   $16,$43,$73,$AA,$DF,$0C,$32,$67
        .byte   $2E,$75,$A8,$C0,$C9
L8918:  .byte   $86,$87,$87,$88,$88,$88,$A3,$A3
        .byte   $A4,$A4,$A4,$A4,$A4,$A5,$A5,$A5
        .byte   $87,$87,$A3,$88,$88,$8A,$00,$00
        .byte   $00,$10,$80,$41,$00,$00,$00,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $2A,$D7,$28,$68,$22,$03,$22,$87
        .byte   $86,$80,$02,$20,$00,$10,$08,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$80,$20,$00,$00,$00
        .byte   $88,$8D,$80,$A6,$82,$E9,$22,$7E
        .byte   $A2,$57,$AA,$C5,$02,$2D,$A8,$7E
        .byte   $02,$AC,$AA,$36,$00,$04,$21,$02
        .byte   $00,$42,$00,$80,$00,$00,$00,$00
        .byte   $AB,$1A,$0A,$F0,$A0,$0B,$20,$A5
        .byte   $00,$00,$02,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $22,$2A,$A2,$43,$2A,$C5,$C8,$30
        .byte   $2A,$60,$82,$82,$2A,$2C,$8A,$19
        .byte   $8A,$06,$02,$08,$00,$02,$00,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $02,$A7,$08,$82,$A8,$DA,$A0,$29
        .byte   $08,$18,$80,$02,$0A,$81,$00,$00
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$00
        .byte   $02,$76,$22,$EF,$32,$B7,$02,$76
        .byte   $8C,$BA,$A8,$FC,$8A,$D8,$A8,$5F
        .byte   $00,$2A,$AA,$06,$01,$DB,$A2,$46
        .byte   $09,$A0,$00,$00,$00,$45,$00,$02
        .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte   $10,$FF,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $84,$F2,$2A,$D0,$02,$C8,$20,$FE
        .byte   $0A,$D9,$02,$13,$20,$95,$0A,$29
        .byte   $0E,$6C,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$02
        .byte   $23,$63,$80,$80,$A2,$80,$A0,$20
        .byte   $40,$FF,$00,$40,$02,$0D,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $31,$00,$03,$00,$19,$00,$19,$00
        .byte   $17,$00,$09,$00,$00,$00,$00,$00
        .byte   $07,$00,$FF,$9C,$A2,$62,$02,$04
        .byte   $00,$18,$08,$00,$20,$00,$00,$00
        .byte   $60,$62,$0F,$20,$10,$00,$0F,$2C
        .byte   $0C,$11,$0F,$27,$17,$08,$0F,$10
        .byte   $00,$09,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$99,$AA,$50,$A2,$71,$00,$57
        .byte   $22,$FD,$8A,$6E,$0A,$0E,$0A,$10
        .byte   $02,$04,$00,$C0,$00,$10,$00,$48
        .byte   $00,$40,$00,$00,$00,$00,$08,$04
        .byte   $0A,$F4,$00,$E6,$28,$46,$28,$7A
        .byte   $00,$8C,$0A,$61,$80,$1C,$08,$02
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $A0,$56,$0A,$D6,$80,$54,$A2,$65
        .byte   $AB,$DD,$8A,$A7,$BA,$E5,$2A,$BB
        .byte   $02,$06,$AA,$49,$08,$F7,$80,$05
        .byte   $08,$0E,$FF,$00,$20,$00,$02,$04
        .byte   $00,$01,$02,$03,$05,$06,$06,$06
        .byte   $06,$06,$06,$07,$07,$07,$08,$08
        .byte   $08,$09,$09,$09,$09,$09,$09,$09
        .byte   $0A,$0B,$0B,$0C,$0D,$0D,$0E,$0E
        .byte   $10,$10,$FF,$85,$0A,$DC,$00,$C2
        .byte   $22,$44,$02,$F6,$28,$06,$00,$20
        .byte   $08,$00,$00,$81,$00,$00,$00,$01
        .byte   $0A,$00,$00,$00,$00,$00,$00,$00
        .byte   $08,$CB,$8A,$79,$00,$20,$00,$61
        .byte   $00,$06,$8A,$82,$00,$85,$00,$01
        .byte   $00,$03,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $0A,$0A,$A2,$FB,$00,$B4,$A8,$E3
        .byte   $02,$BD,$AA,$6F,$8A,$FE,$28,$0E
        .byte   $2C,$6B,$8A,$E3,$12,$E4,$0A,$44
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $82,$F4,$0A,$B7,$00,$0A,$08,$08
        .byte   $0A,$2C,$82,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $20,$F0,$20,$A9,$A2,$B7,$02,$7C
        .byte   $A0,$E7,$02,$9A,$08,$81,$0A,$E8
        .byte   $08,$C6,$00,$DD,$80,$20,$00,$00
        .byte   $00,$00,$02,$22,$00,$00,$00,$00
        .byte   $0E,$1E,$AA,$D9,$80,$48,$8A,$A1
        .byte   $02,$3F,$82,$A5,$A8,$04,$00,$96
        .byte   $08,$08,$02,$04,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $2A,$40,$A0,$A4,$BE,$8F,$88,$E3
        .byte   $A0,$81,$AA,$E5,$A8,$6A,$A8,$4D
        .byte   $84,$26,$82,$E6,$22,$4C,$28,$CA
        .byte   $A2,$09,$00,$04,$00,$00,$00,$08
        .byte   $28,$50,$70,$D8,$E8,$20,$48,$68
        .byte   $C8,$D8,$F8,$28,$38,$78,$70,$98
        .byte   $B8,$68,$70,$78,$88,$88,$88,$D8
        .byte   $F0,$90,$F0,$B0,$58,$68,$C8,$D8
        .byte   $50,$80,$FF,$10,$22,$41,$10,$00
        .byte   $00,$00,$00,$00,$20,$10,$00,$00
        .byte   $10,$00,$00,$10,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$00,$00,$04,$00,$00,$12,$00
        .byte   $02,$00,$00,$00,$80,$00,$20,$40
        .byte   $00,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$10
        .byte   $10,$45,$00,$10,$60,$04,$00,$04
        .byte   $4C,$04,$0F,$01,$09,$00,$01,$00
        .byte   $00,$05,$04,$10,$00,$00,$80,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $11,$00,$2C,$00,$00,$00,$00,$00
        .byte   $00,$10,$80,$00,$00,$00,$00,$10
        .byte   $00,$00,$00,$01,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $50,$01,$B9,$01,$00,$10,$21,$00
        .byte   $09,$00,$2C,$10,$08,$00,$00,$00
        .byte   $20,$40,$C0,$40,$00,$04,$41,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$00
        .byte   $0E,$00,$00,$14,$02,$40,$40,$40
        .byte   $01,$04,$24,$01,$00,$00,$40,$40
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $01,$10,$04,$14,$44,$11,$40,$05
        .byte   $18,$00,$A0,$00,$34,$00,$08,$10
        .byte   $03,$00,$06,$11,$46,$00,$04,$01
        .byte   $04,$00,$00,$00,$00,$01,$80,$40
        .byte   $A8,$50,$50,$B8,$58,$98,$68,$58
        .byte   $58,$68,$78,$58,$88,$68,$98,$90
        .byte   $90,$78,$38,$78,$30,$70,$B0,$B8
        .byte   $70,$B0,$90,$50,$48,$48,$58,$58
        .byte   $2C,$E8,$FF,$00,$88,$10,$00,$40
        .byte   $40,$44,$01,$00,$20,$01,$02,$04
        .byte   $00,$00,$01,$00,$80,$00,$04,$00
        .byte   $00,$00,$00,$00,$02,$00,$00,$00
        .byte   $00,$10,$02,$00,$00,$00,$00,$00
        .byte   $00,$01,$00,$04,$00,$00,$00,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$41,$08,$00,$21,$51,$00,$00
        .byte   $12,$01,$00,$00,$22,$01,$15,$00
        .byte   $28,$00,$10,$00,$00,$00,$00,$00
        .byte   $00,$00,$80,$00,$00,$00,$04,$00
        .byte   $12,$01,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$02,$10,$00,$01,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $26,$00,$A0,$00,$44,$01,$04,$40
        .byte   $48,$11,$04,$40,$30,$00,$01,$00
        .byte   $10,$00,$15,$00,$00,$00,$40,$00
        .byte   $00,$00,$00,$00,$00,$00,$08,$10
        .byte   $60,$10,$08,$00,$30,$00,$02,$00
        .byte   $00,$00,$D0,$00,$20,$01,$00,$00
        .byte   $00,$40,$00,$00,$00,$00,$08,$00
        .byte   $20,$00,$10,$00,$00,$00,$00,$00
        .byte   $01,$00,$42,$04,$15,$00,$11,$10
        .byte   $0C,$10,$02,$01,$42,$40,$00,$00
        .byte   $09,$10,$C4,$04,$44,$00,$70,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$40
        .byte   $55,$62,$62,$54,$2C,$52,$2C,$2C
        .byte   $2C,$2C,$2C,$2C,$2C,$2C,$55,$5D
        .byte   $5D,$50,$54,$52,$5D,$5D,$5D,$50
        .byte   $03,$03,$03,$03,$50,$50,$52,$52
        .byte   $5E,$5F,$FF,$00,$48,$41,$00,$00
        .byte   $40,$00,$42,$00,$00,$01,$04,$05
        .byte   $00,$00,$00,$00,$02,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $1A,$00,$48,$01,$04,$00,$40,$04
        .byte   $01,$10,$04,$00,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$01,$09,$00,$02,$00,$49,$40
        .byte   $00,$05,$88,$14,$89,$05,$60,$10
        .byte   $02,$00,$A1,$14,$00,$04,$00,$00
        .byte   $00,$01,$00,$00,$00,$00,$00,$00
        .byte   $01,$00,$04,$00,$04,$00,$80,$04
        .byte   $40,$00,$08,$00,$00,$04,$00,$00
        .byte   $00,$00,$10,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$05,$42,$00,$01,$01,$92,$00
        .byte   $2C,$10,$10,$00,$84,$10,$00,$00
        .byte   $00,$40,$08,$50,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$00,$20,$00
        .byte   $00,$04,$58,$00,$00,$00,$08,$11
        .byte   $40,$10,$48,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $24,$00,$10,$11,$08,$04,$C0,$10
        .byte   $15,$00,$10,$04,$87,$55,$44,$01
        .byte   $00,$45,$A1,$01,$50,$01,$25,$00
        .byte   $80,$50,$00,$41,$40,$14,$00,$00
        .byte   $00,$00,$01,$02,$03,$04,$00,$00
        .byte   $00,$05,$06,$07,$07,$08,$09,$00
        .byte   $0A,$0B,$07,$07,$07,$07,$07,$0C
        .byte   $07,$07,$0D,$0E,$07,$07,$07,$07
        .byte   $0F,$10,$11,$12,$0F,$10,$0F,$10
        .byte   $13,$14,$11,$15,$16,$15,$13,$14
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $18,$18,$18,$18,$18,$18,$18,$18
        .byte   $00,$00,$00,$05,$19,$00,$00,$00
        .byte   $1A,$1B,$1C,$0B,$07,$1D,$1E,$02
        .byte   $07,$07,$07,$07,$07,$07,$07,$07
        .byte   $07,$07,$0D,$0E,$07,$07,$07,$07
        .byte   $0F,$1F,$20,$21,$22,$10,$0F,$10
        .byte   $0D,$23,$24,$25,$0D,$15,$13,$14
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $18,$18,$18,$18,$18,$18,$18,$18
        .byte   $26,$00,$01,$27,$28,$07,$29,$00
        .byte   $07,$2A,$06,$07,$07,$07,$07,$2A
        .byte   $07,$07,$07,$07,$07,$07,$07,$07
        .byte   $07,$07,$07,$0D,$0E,$07,$07,$07
        .byte   $0F,$10,$1F,$20,$21,$2B,$0F,$10
        .byte   $16,$0D,$23,$24,$25,$0D,$16,$15
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $18,$18,$18,$18,$18,$18,$18,$18
        .byte   $00,$00,$00,$00,$00,$00,$00,$2C
        .byte   $2D,$04,$01,$2E,$02,$2A,$04,$2C
        .byte   $07,$08,$0B,$07,$07,$07,$03,$2C
        .byte   $07,$07,$07,$07,$07,$07,$07,$2C
        .byte   $0F,$10,$0F,$2F,$07,$07,$07,$2C
        .byte   $16,$0D,$16,$30,$07,$07,$07,$2C
        .byte   $31,$32,$32,$21,$33,$34,$35,$36
        .byte   $2C,$37,$37,$25,$38,$39,$3A,$3B
        .byte   $3C,$3C,$3C,$3D,$39,$39,$3E,$3F
        .byte   $3C,$3C,$3C,$3D,$39,$39,$39,$39
        .byte   $40,$40,$40,$41,$42,$42,$42,$42
        .byte   $40,$40,$40,$41,$43,$43,$43,$43
        .byte   $44,$44,$44,$45,$43,$43,$43,$43
        .byte   $40,$40,$40,$40,$40,$41,$46,$46
        .byte   $40,$40,$40,$40,$40,$40,$40,$40
        .byte   $40,$40,$40,$40,$40,$40,$40,$40
        .byte   $3F,$47,$3F,$3F,$48,$3F,$47,$3F
        .byte   $39,$49,$4A,$4A,$4B,$39,$49,$4A
        .byte   $42,$4C,$42,$42,$4D,$4E,$4F,$42
        .byte   $43,$50,$43,$43,$51,$43,$50,$43
        .byte   $43,$50,$52,$52,$51,$53,$50,$52
        .byte   $54,$54,$46,$46,$55,$40,$41,$46
        .byte   $56,$40,$40,$40,$40,$40,$40,$40
        .byte   $57,$40,$40,$40,$40,$40,$40,$40
        .byte   $3F,$48,$3F,$3F,$3F,$3F,$3F,$47
        .byte   $4A,$4B,$39,$39,$39,$39,$39,$49
        .byte   $42,$4D,$4E,$4E,$4E,$4E,$4E,$4F
        .byte   $43,$51,$43,$43,$43,$43,$43,$50
        .byte   $52,$51,$58,$59,$5A,$58,$59,$50
        .byte   $54,$54,$46,$46,$46,$46,$46,$55
        .byte   $56,$40,$40,$40,$40,$40,$40,$40
        .byte   $57,$40,$40,$40,$40,$40,$40,$40
        .byte   $3F,$3F,$48,$3F,$3F,$3F,$38,$5B
        .byte   $4A,$4A,$4B,$39,$5C,$5D,$5D,$3C
        .byte   $42,$42,$4D,$4E,$55,$40,$40,$40
        .byte   $43,$43,$51,$43,$55,$40,$40,$40
        .byte   $52,$52,$54,$43,$5E,$44,$44,$44
        .byte   $40,$40,$56,$40,$40,$40,$40,$40
        .byte   $40,$40,$57,$40,$40,$40,$40,$40
        .byte   $40,$40,$57,$40,$40,$40,$40,$40
        .byte   $3D,$5F,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$5F,$3D,$60,$61,$60,$60,$5B
        .byte   $3D,$62,$3F,$63,$64,$65,$39,$5B
        .byte   $3D,$66,$67,$3F,$68,$3D,$69,$5B
        .byte   $3D,$4A,$3F,$4A,$6A,$60,$6B,$5B
        .byte   $3D,$39,$3C,$3C,$3C,$3D,$38,$5B
        .byte   $3C,$3C,$3C,$3C,$3C,$3D,$38,$5B
        .byte   $3C,$3C,$3C,$3C,$3C,$3D,$38,$5B
        .byte   $3D,$66,$5B,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$66,$67,$6C,$6C,$6D,$6E,$6F
        .byte   $3D,$66,$5B,$3C,$3D,$70,$4A,$5B
        .byte   $3D,$66,$3F,$6C,$6C,$49,$39,$5B
        .byte   $3D,$66,$5B,$3C,$3D,$70,$4A,$5B
        .byte   $3D,$66,$6C,$6C,$6C,$49,$71,$6F
        .byte   $3D,$5F,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$5F,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$72,$73,$72,$73,$3C,$3C
        .byte   $3D,$6C,$74,$6C,$75,$6C,$6C,$76
        .byte   $3D,$39,$77,$39,$78,$39,$39,$79
        .byte   $3D,$39,$6C,$39,$77,$39,$39,$4B
        .byte   $3D,$7A,$39,$39,$6C,$7B,$3C,$3C
        .byte   $3D,$66,$7C,$7C,$7C,$7D,$5B,$3C
        .byte   $3D,$66,$5B,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$66,$5B,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $6C,$6D,$6C,$6C,$76,$6C,$6D,$6C
        .byte   $34,$7E,$39,$39,$79,$34,$7E,$39
        .byte   $39,$49,$7F,$7F,$4B,$39,$49,$7F
        .byte   $3D,$70,$39,$39,$80,$4A,$70,$39
        .byte   $3D,$49,$71,$71,$4B,$5B,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$81,$5B
        .byte   $6C,$76,$6C,$6D,$6C,$6C,$82,$5B
        .byte   $39,$79,$34,$7E,$39,$39,$39,$5B
        .byte   $7F,$4B,$39,$49,$7F,$5B,$3C,$3C
        .byte   $39,$80,$4A,$70,$39,$5B,$3C,$3C
        .byte   $3D,$83,$84,$64,$39,$6F,$85,$85
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$66,$5B,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$39,$86,$87,$6D,$88,$89,$6F
        .byte   $3D,$39,$8A,$77,$49,$39,$39,$5B
        .byte   $3D,$4A,$60,$60,$70,$4A,$4A,$5B
        .byte   $8B,$39,$39,$39,$49,$39,$39,$5B
        .byte   $3D,$7C,$7C,$7C,$8C,$7C,$69,$5B
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$81,$5B
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$81,$5B
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $8B,$6C,$6D,$6C,$6C,$6C,$76,$5B
        .byte   $3D,$39,$49,$4A,$4A,$4A,$4B,$5B
        .byte   $3D,$4A,$70,$39,$39,$39,$5B,$3C
        .byte   $3D,$39,$49,$8D,$8D,$8D,$76,$8E
        .byte   $3D,$39,$49,$39,$39,$39,$4B,$8F
        .byte   $3D,$7A,$5B,$3C,$3C,$3C,$3C,$3C
        .byte   $3D,$66,$5B,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C
        .byte   $90,$6C,$6C,$88,$89,$6F,$85,$85
        .byte   $91,$39,$39,$39,$39,$5B,$3C,$3C
        .byte   $3C,$3C,$3D,$39,$39,$5B,$3C,$3C
        .byte   $3C,$3C,$3D,$39,$39,$5B,$3C,$3C
        .byte   $3D,$3F,$47,$39,$39,$48,$3F,$5B
        .byte   $3D,$34,$92,$34,$34,$93,$34,$5B
        .byte   $94,$42,$4C,$42,$42,$95,$42,$96
        .byte   $94,$52,$97,$43,$43,$98,$52,$96
        .byte   $94,$43,$50,$99,$99,$51,$43,$96
        .byte   $94,$58,$9A,$43,$43,$9B,$59,$96
        .byte   $41,$9C,$9C,$9C,$9C,$9C,$9C,$55
        .byte   $41,$9D,$9D,$9D,$9D,$9D,$9D,$55
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $9E,$9E,$9E,$9E,$9E,$9E,$9E,$9E
        .byte   $4D,$4D,$4D,$4D,$4D,$4D,$4D,$5D
        .byte   $4D,$55,$5C,$03,$03,$52,$03,$03
        .byte   $4D,$4D,$52,$4D,$4D,$4D,$4D,$55
        .byte   $55,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$5B,$03,$03,$5A,$4D,$03,$52
        .byte   $55,$5B,$03,$03,$5C,$03,$03,$03
        .byte   $5B,$5C,$03,$03,$70,$73,$78,$7B
        .byte   $74,$03,$7C,$03,$03,$03,$64,$64
        .byte   $03,$03,$65,$64,$78,$7B,$78,$7B
        .byte   $7C,$03,$65,$64,$6C,$6C,$70,$71
        .byte   $6D,$6C,$72,$73,$6D,$6C,$6D,$6C
        .byte   $6C,$6C,$6C,$6C,$62,$63,$68,$69
        .byte   $60,$61,$68,$69,$4D,$4D,$5B,$5A
        .byte   $4D,$5D,$55,$03,$5C,$5B,$03,$03
        .byte   $5A,$5D,$03,$03,$52,$4D,$03,$5B
        .byte   $4D,$4D,$5A,$5D,$70,$71,$78,$6B
        .byte   $75,$75,$76,$77,$72,$73,$7A,$7B
        .byte   $74,$03,$64,$64,$78,$6B,$78,$79
        .byte   $7E,$7F,$7D,$6A,$7A,$7B,$7A,$7B
        .byte   $4D,$4D,$55,$52,$4D,$4D,$5A,$55
        .byte   $5D,$5C,$03,$03,$52,$4D,$03,$52
        .byte   $5B,$5A,$03,$03,$74,$03,$65,$64
        .byte   $78,$79,$78,$79,$5D,$5A,$03,$03
        .byte   $4D,$4D,$5C,$5B,$03,$03,$65,$03
        .byte   $6D,$03,$6D,$03,$70,$71,$78,$79
        .byte   $75,$75,$7D,$7D,$49,$07,$14,$0F
        .byte   $49,$49,$14,$14,$49,$70,$14,$78
        .byte   $71,$75,$79,$7D,$7D,$7D,$7D,$7D
        .byte   $14,$0F,$14,$0F,$14,$14,$14,$14
        .byte   $14,$78,$14,$78,$79,$7D,$79,$7D
        .byte   $10,$11,$11,$10,$10,$11,$11,$12
        .byte   $14,$12,$14,$13,$10,$11,$13,$13
        .byte   $18,$19,$19,$18,$18,$19,$19,$1A
        .byte   $1D,$1D,$1C,$1C,$1C,$1C,$1C,$1C
        .byte   $37,$37,$3F,$3F,$37,$27,$3F,$2F
        .byte   $1C,$1C,$06,$06,$10,$11,$13,$40
        .byte   $10,$11,$40,$13,$14,$48,$14,$48
        .byte   $14,$14,$49,$49,$48,$14,$48,$14
        .byte   $1D,$4C,$1C,$4A,$4C,$1D,$4A,$43
        .byte   $1D,$1D,$43,$43,$1D,$4C,$43,$4A
        .byte   $1C,$4A,$1C,$4A,$4A,$1C,$4A,$1C
        .byte   $4B,$4B,$1C,$1C,$18,$19,$1A,$1A
        .byte   $32,$33,$3A,$3B,$18,$19,$1A,$18
        .byte   $3C,$1A,$3D,$18,$3D,$1A,$3D,$18
        .byte   $1C,$30,$1C,$38,$31,$1C,$39,$1C
        .byte   $18,$19,$1B,$1B,$10,$11,$12,$10
        .byte   $14,$14,$12,$10,$14,$14,$11,$10
        .byte   $36,$37,$3E,$3F,$0F,$12,$0F,$10
        .byte   $13,$13,$49,$49,$13,$40,$49,$48
        .byte   $0F,$12,$0F,$13,$14,$20,$14,$28
        .byte   $21,$48,$29,$48,$22,$23,$2A,$2B
        .byte   $0F,$14,$0F,$14,$10,$11,$12,$12
        .byte   $10,$11,$13,$10,$14,$07,$14,$0F
        .byte   $14,$40,$49,$48,$14,$0F,$49,$0F
        .byte   $13,$13,$14,$14,$13,$40,$14,$48
        .byte   $50,$51,$58,$59,$56,$57,$5E,$5F
        .byte   $14,$48,$49,$48,$20,$21,$28,$29
        .byte   $10,$22,$11,$2A,$23,$11,$2B,$10
        .byte   $13,$2C,$14,$2E,$13,$2C,$14,$2D
        .byte   $40,$13,$48,$14,$14,$13,$10,$11
        .byte   $14,$2D,$14,$2E,$48,$49,$48,$14
        .byte   $07,$14,$0F,$14,$14,$14,$14,$10
        .byte   $14,$14,$05,$05,$14,$13,$05,$05
        .byte   $49,$48,$14,$48,$41,$41,$14,$14
        .byte   $48,$14,$48,$49,$12,$0F,$11,$0F
        .byte   $13,$0F,$14,$0F,$48,$20,$48,$28
        .byte   $21,$20,$29,$28,$57,$57,$5F,$5F
        .byte   $12,$13,$12,$14,$13,$12,$14,$12
        .byte   $13,$50,$14,$58,$51,$13,$59,$14
        .byte   $13,$14,$10,$11,$57,$47,$5F,$4F
        .byte   $14,$48,$05,$05,$14,$14,$41,$41
        .byte   $13,$0E,$14,$0E,$14,$0E,$14,$0E
        .byte   $0B,$13,$0C,$14,$0C,$14,$0C,$14
        .byte   $49,$44,$14,$48,$44,$49,$48,$14
        .byte   $67,$6F,$67,$6F,$4C,$1D,$4A,$1C
        .byte   $66,$67,$66,$67,$4B,$4A,$1C,$4A
        .byte   $4A,$4B,$4A,$1C,$1C,$1C,$43,$43
        .byte   $31,$4A,$39,$4A,$4A,$30,$4A,$38
        .byte   $46,$46,$4E,$4E,$4E,$4E,$4E,$4E
        .byte   $00,$00,$00,$00,$51,$40,$59,$48
        .byte   $40,$50,$48,$58,$13,$13,$41,$41
        .byte   $62,$73,$68,$7B,$60,$7B,$68,$7B
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
        .byte   $00,$00,$10,$11,$00,$0C,$0C,$0E
        .byte   $10,$00,$00,$00,$24,$4D,$1E,$0E
        .byte   $86,$A6,$86,$00,$24,$00,$00,$A4
        .byte   $86,$A6,$86,$00,$24,$14,$00,$00
        .byte   $2C,$2E,$40,$42,$00,$00,$00,$6D
        .byte   $4C,$3E,$60,$62,$64,$74,$74,$6F
        .byte   $2C,$2E,$40,$42,$00,$00,$6C,$6D
        .byte   $4C,$3E,$60,$62,$64,$74,$6E,$6F
        .byte   $00,$24,$00,$24,$0A,$00,$76,$6D
        .byte   $1A,$0A,$1A,$0A,$2A,$01,$78,$6F
        .byte   $00,$00,$20,$11,$32,$01,$6C,$6D
        .byte   $4C,$3E,$01,$21,$32,$01,$6E,$6F
        .byte   $06,$08,$02,$04,$12,$12,$44,$6B
        .byte   $26,$28,$67,$58,$30,$30,$00,$6A
        .byte   $46,$48,$4A,$48,$11,$4A,$CC,$CE
        .byte   $56,$58,$10,$58,$5A,$10,$EC,$EE
        .byte   $98,$9A,$C8,$CA,$88,$8A,$E2,$95
        .byte   $B8,$BA,$E8,$EA,$F8,$94,$C2,$C3
        .byte   $E5,$E1,$E0,$E2,$C3,$C0,$F0,$C0
        .byte   $C1,$C0,$D4,$C0,$F0,$C2,$E2,$C2
        .byte   $C2,$95,$E2,$D4,$E0,$00,$00,$00
        .byte   $00,$A4,$C6,$E6,$00,$00,$00,$00
        .byte   $CC,$CE,$00,$00,$00,$00,$00,$00
        .byte   $EC,$EE,$00,$00,$00,$00,$00,$00
        .byte   $8E,$8E,$00,$00,$00,$00,$00,$00
        .byte   $81,$90,$90,$00,$00,$00,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$AA,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$11,$00,$0D,$0D,$0F
        .byte   $10,$00,$00,$00,$25,$4D,$1F,$0F
        .byte   $87,$A7,$A7,$00,$25,$00,$00,$A5
        .byte   $87,$A7,$A7,$00,$25,$15,$00,$00
        .byte   $2D,$2F,$41,$43,$00,$00,$00,$6C
        .byte   $3D,$4F,$61,$63,$65,$75,$75,$6E
        .byte   $2D,$2F,$41,$43,$00,$00,$6D,$6D
        .byte   $3D,$4F,$61,$63,$65,$75,$6F,$6F
        .byte   $00,$25,$00,$25,$0A,$00,$77,$6C
        .byte   $1B,$0A,$1B,$0A,$2B,$01,$79,$6E
        .byte   $00,$00,$01,$31,$11,$23,$6D,$6D
        .byte   $3D,$4F,$01,$31,$22,$01,$6F,$6F
        .byte   $07,$09,$03,$05,$12,$13,$6A,$6B
        .byte   $27,$29,$67,$66,$30,$33,$00,$45
        .byte   $47,$4A,$47,$49,$11,$4A,$CD,$CF
        .byte   $57,$10,$57,$59,$11,$10,$ED,$EF
        .byte   $99,$9B,$C9,$CB,$89,$8B,$95,$E1
        .byte   $B9,$BB,$E9,$EB,$84,$FB,$C3,$C1
        .byte   $E5,$E2,$E1,$E3,$C0,$C3,$C0,$F3
        .byte   $C2,$C0,$C0,$D5,$C1,$F3,$E1,$C1
        .byte   $D5,$E1,$95,$C1,$E3,$00,$00,$00
        .byte   $00,$A5,$C7,$E7,$00,$00,$00,$00
        .byte   $CD,$CF,$00,$00,$00,$00,$00,$00
        .byte   $ED,$EF,$00,$00,$00,$00,$00,$00
        .byte   $8E,$8F,$00,$00,$00,$00,$00,$00
        .byte   $90,$90,$82,$00,$00,$00,$00,$00
        .byte   $00,$8B,$80,$00,$00,$00,$00,$00
        .byte   $A9,$AB,$80,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$11,$00,$1C,$1C,$0E
        .byte   $10,$00,$00,$34,$34,$4E,$1E,$0E
        .byte   $96,$B6,$96,$34,$34,$00,$B4,$B4
        .byte   $96,$B6,$96,$34,$34,$34,$00,$00
        .byte   $3C,$3E,$50,$52,$00,$00,$00,$7D
        .byte   $5C,$5E,$70,$72,$74,$74,$3A,$7F
        .byte   $3C,$3E,$50,$52,$00,$00,$7C,$7D
        .byte   $5C,$5E,$70,$72,$74,$74,$7E,$7F
        .byte   $1A,$0A,$1A,$0A,$1A,$00,$68,$7D
        .byte   $1A,$34,$1A,$34,$1A,$01,$78,$7F
        .byte   $3C,$3E,$11,$11,$11,$32,$7C,$7D
        .byte   $5C,$5E,$21,$11,$11,$32,$7E,$7F
        .byte   $16,$18,$16,$18,$30,$30,$54,$7B
        .byte   $36,$38,$10,$58,$30,$30,$00,$7A
        .byte   $56,$58,$10,$58,$5A,$10,$DC,$DE
        .byte   $56,$58,$10,$58,$5A,$10,$FC,$FE
        .byte   $A8,$AA,$D8,$DA,$A8,$AA,$F2,$E4
        .byte   $C8,$CA,$F8,$FA,$F8,$94,$D2,$85
        .byte   $F5,$F1,$F0,$F2,$C4,$C0,$F0,$C0
        .byte   $D1,$C0,$E4,$C0,$D0,$D2,$F2,$D2
        .byte   $D2,$D2,$D2,$D2,$D0,$00,$00,$00
        .byte   $B4,$B4,$D6,$F6,$00,$00,$00,$00
        .byte   $DC,$DE,$00,$00,$00,$00,$00,$00
        .byte   $FC,$FE,$00,$00,$00,$00,$00,$00
        .byte   $9E,$9E,$00,$00,$00,$00,$00,$00
        .byte   $80,$90,$90,$00,$00,$00,$00,$00
        .byte   $80,$9A,$00,$00,$00,$00,$00,$00
        .byte   $91,$90,$90,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$11,$00,$1D,$1D,$0F
        .byte   $10,$00,$00,$35,$35,$4E,$1F,$0F
        .byte   $97,$B7,$B7,$35,$35,$00,$B5,$B5
        .byte   $97,$B7,$B7,$35,$35,$35,$00,$00
        .byte   $3D,$3F,$51,$53,$00,$00,$00,$7C
        .byte   $5D,$5F,$71,$73,$75,$75,$3B,$7E
        .byte   $3D,$3F,$51,$53,$00,$00,$7D,$7D
        .byte   $5D,$5F,$71,$73,$75,$75,$7F,$7F
        .byte   $1B,$0A,$1B,$0A,$1B,$00,$69,$7C
        .byte   $1B,$35,$1B,$35,$1B,$01,$79,$7E
        .byte   $3D,$3F,$31,$11,$11,$11,$7D,$7D
        .byte   $5D,$5F,$31,$11,$11,$22,$7F,$7F
        .byte   $17,$19,$17,$19,$30,$33,$7A,$7B
        .byte   $37,$39,$10,$66,$30,$33,$00,$55
        .byte   $57,$10,$57,$59,$11,$10,$DD,$DF
        .byte   $57,$10,$57,$59,$11,$10,$FD,$FF
        .byte   $A9,$AB,$D9,$DB,$A9,$AB,$E4,$F1
        .byte   $C9,$CB,$F9,$FB,$84,$FB,$85,$D1
        .byte   $F5,$F2,$F1,$F3,$C0,$C5,$C0,$F3
        .byte   $D2,$C0,$C0,$E4,$D1,$D3,$F1,$D1
        .byte   $D1,$D1,$D1,$D1,$D3,$00,$00,$00
        .byte   $B5,$B5,$D7,$F7,$00,$00,$00,$00
        .byte   $DD,$DF,$00,$00,$00,$00,$00,$00
        .byte   $FD,$FF,$00,$00,$00,$00,$00,$00
        .byte   $9E,$9F,$00,$00,$00,$00,$00,$00
        .byte   $90,$90,$80,$00,$00,$00,$00,$00
        .byte   $99,$9B,$80,$00,$00,$00,$00,$00
        .byte   $90,$90,$92,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$01,$00,$53,$51,$43
        .byte   $03,$00,$00,$13,$13,$10,$10,$23
        .byte   $13,$13,$13,$03,$03,$00,$13,$13
        .byte   $11,$11,$11,$81,$81,$81,$00,$00
        .byte   $03,$03,$13,$13,$10,$10,$03,$11
        .byte   $03,$03,$13,$13,$13,$13,$13,$11
        .byte   $81,$81,$11,$11,$10,$10,$11,$11
        .byte   $81,$81,$11,$11,$11,$11,$11,$11
        .byte   $03,$03,$81,$81,$03,$00,$11,$13
        .byte   $03,$03,$81,$81,$81,$01,$11,$13
        .byte   $03,$03,$01,$01,$01,$01,$13,$13
        .byte   $03,$03,$01,$01,$01,$01,$13,$13
        .byte   $12,$12,$12,$12,$01,$01,$11,$11
        .byte   $12,$12,$10,$10,$01,$01,$00,$11
        .byte   $10,$10,$10,$10,$01,$10,$10,$10
        .byte   $10,$10,$10,$10,$01,$10,$10,$10
        .byte   $12,$12,$60,$60,$12,$12,$10,$10
        .byte   $60,$60,$12,$12,$12,$12,$10,$10
        .byte   $13,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$00,$00,$00
        .byte   $03,$03,$03,$03,$00,$00,$00,$00
        .byte   $03,$03,$00,$00,$00,$00,$00,$00
        .byte   $03,$03,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00
        brk
        brk
        brk
        brk
