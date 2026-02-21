; ===========================================================================
; stage_transition_entry — horizontal scroll + boss name reveal
; ===========================================================================
; Entry point after stage select confirmation (JMP from bank18).
; Y = adjusted grid index (used to index all parameter tables below).
;
; This routine:
;   1. Plays stage intro music ($33)
;   2. Loads stage parameters from lookup tables
;   3. Horizontal scroll: 4px/frame for 64 frames (scrolls to new nametable)
;      The MMC3 scanline IRQ creates a 3-strip effect:
;        - Top strip (y=0-87): scrolls left, sliding portraits away
;        - Middle band (y=88-151): stays fixed (blue boss intro band)
;        - Bottom strip (y=152-239): scrolls left
;   4. Waits 60 frames
;   5. Waits for boss animation sync
;   6. Writes boss name to nametable, 1 tile per 4 frames
;   7. Returns to caller (bank18 robot_master_intro)
; ---------------------------------------------------------------------------
stage_transition_entry:
; =============================================================================
; MEGA MAN 3 (U) — BANK $03 — STAGE TRANSITION + HARD MAN STAGE DATA
; =============================================================================
; Stage transition effects and Hard Man stage layout data.
;
; Annotation: 0% — unannotated da65 output
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $03 — STAGE TRANSITION + HARD MAN STAGE DATA
; =============================================================================
; Mapped to $A000-$BFFF. Contains stage transition animation (boss name
; reveal, horizontal scroll entry). Also doubles as Hard Man stage data ($22=$03).
;
; Annotation: partial — main flow well documented, 66 auto labels in branches
; =============================================================================

        .setcpu "6502"                  ; play stage intro music

L9212           := $9212                ; play stage intro music
L938B           := $938B                ; play stage intro music
L939E           := $939E                ; play stage intro music
LCBCE           := $CBCE                ; play stage intro music
LF797           := $F797                ; play stage intro music
LF898           := $F898                ; play stage intro music
LFD52           := $FD52                ; play stage intro music
LFF1A           := $FF1A                ; play stage intro music
LFF21           := $FF21                ; play stage intro music
LFF3C           := $FF3C                ; play stage intro music
LFF6B           := $FF6B                ; play stage intro music

.segment "BANK03"                       ; play stage intro music

        lda     #$33                    ; play stage intro music
        jsr     LF898
        lda     #$80                    ; mark entity slot $10 active
        sta     $0310                   ; (used for scroll entity)
        sta     $0590

; Load stage parameters from lookup tables (indexed by Y = grid index).
; Each table has 18 entries: 9 Robot Master + 9 Doc Robot stages.
; Grid: 0=Spark, 1=Snake, 2=Needle, 3=Hard, 4=Center, 5=Top,
;       6=Gemini, 7=Magnet, 8=Shadow
        lda     LA139,y                 ; → $05D0 (stage config)
        sta     $05D0
        lda     LA14B,y                 ; → $0410 (tileset / scroll speed sub)
        sta     $0410
        lda     LA15D,y                 ; → $0430 (enemy/level data)
        sta     $0430
        lda     LA16F,y                 ; → $0450 (sprite data)
        sta     $0450
        lda     LA181,y                 ; → $0470 (scroll direction flag)
        sta     $0470
        lda     LA193,y                 ; → $03D0 (scroll limit / position)
        sta     $03D0
        lda     LA1A5,y                 ; → $0370 (BG scroll position)
        sta     $0370

; Load intro sprite palettes (8 bytes → SP 0 and SP 1, both active + working copy).
        ldx     #$07
LA039:  lda     LA1D9,x                 ; intro sprite palette data
        sta     $0610,x                 ; → active sprite palette buffer
        sta     $0630,x                 ; → working copy (for flash restore)
        dex
        bpl     LA039
        lda     LA1B7,y                 ; select CHR bank for this stage
        jsr     L938B
        jsr     LFF3C
        lda     #$00
        sta     $05F0                   ; clear entity slot $10 flags
        sta     $05B0                   ; clear entity slot $10 anim phase
        sta     $0300                   ; clear entity slot 0 type
        lda     #$40                    ; $99 = deceleration value for
        sta     $99                     ; scroll physics (gravity)

; --- Horizontal scroll loop ---
; Scrolls X position by 4px/frame. $FC = X scroll low byte (0-255).
; $FD bit 0 = nametable select (toggles when $FC overflows).
; This scrolls the display from the old nametable (stage select)
; to the new nametable (boss intro band). Takes 64 frames (256/4).
; Simultaneously applies "gravity" to entity $10 (Y movement).
LA05D:  lda     $FC                     ; $FC += 4
        clc
        adc     #$04
        sta     $FC
        lda     $FD                     ; $FD = carry into nametable select
        adc     #$00
        and     #$01                    ; (wrap to 0-1)
        sta     $FD
        ldx     #$10                    ; apply Y movement to entity $10
        jsr     LF797                   ; (scroll entity — creates vertical effect)
        lda     $0470                   ; scroll direction check
        bpl     LA082
        lda     #$70                    ; clamp $03D0 to max $70
        cmp     $03D0
        bcs     LA082
        sta     $03D0
        bne     LA095
LA082:  lda     $0350                   ; advance sub-pixel scroll
        clc                             ; $0350 += $0410
        adc     $0410
        sta     $0350
        lda     $0370                   ; advance scroll position
        adc     $0430                   ; $0370 += $0430 + carry
        sta     $0370
LA095:  jsr     LFD52                   ; process entities + wait for NMI
        lda     #$00
        sta     $05F0                   ; clear entity $10 flags
        lda     $FC                     ; loop until $FC wraps to 0
        bne     LA05D                   ; (64 frames)

; --- Post-scroll wait ---
        lda     #$7E                    ; update CHR bank
        sta     $E9
        jsr     LFF3C
        lda     #$3C                    ; A = $3C (60 frames)
LA0AA:  pha
        jsr     LFD52                   ; process entities + wait for NMI
        lda     #$00
        sta     $05F0
        pla
        sec
        sbc     #$01                    ; countdown
        bne     LA0AA

; --- Wait for boss animation sync ---
; $A1C9,y = expected animation phase value for this stage.
; Wait until entity $10's anim phase ($05B0) matches.
LA0B9:  jsr     LFD52                   ; process entities + wait for NMI
        ldy     $22                     ; Y = stage number
        lda     LA1C9,y                 ; expected anim phase
        cmp     $05B0                   ; current anim phase
        bne     LA0B9
        lda     #$03                    ; re-select bank 03
        sta     $F5                     ; (may have been swapped during
        jsr     LFF6B                   ; entity processing)
        jmp     LA0D0

; ===========================================================================
; Write boss name to nametable — 1 tile per 4 frames
; ===========================================================================
; PPU address: row 17, column 11 = $222B (or $262B for nametable 1).
; Row 17 × 8 = pixel Y=136 — this places the name text inside the
; blue band, below the boss sprite (verified: text at y=136-143).
;
; Boss name table at $A1E1 (10 chars per stage, indexed by stage*10):
;   Stage $00 (Needle): "NEEDLE MAN"
;   Stage $01 (Magnet): "MAGNET MAN"
;   Stage $02 (Gemini): "GEMINI MAN"
;   Stage $03 (Hard):   "HARD   MAN"
;   Stage $04 (Top):    "TOP    MAN"
;   Stage $05 (Snake):  "SNAKE  MAN"
;   Stage $06 (Spark):  "SPARK  MAN"
;   Stage $07 (Shadow): "SHADOW MAN"
; Tile encoding: $0A=A, $0B=B, ... $23=Z, $25=space
; ---------------------------------------------------------------------------

LA0D0:  lda     $60                     ; if not Robot Master ($60 != 0),
        bne     LA122                   ; skip name writing

; Set up PPU write queue for 1-tile-at-a-time writes.
; High byte: $22 or $26 depending on current nametable.
        lda     $FD                     ; nametable select → PPU high byte
        and     #$01
        asl     a                       ; 0→$22 (NT0), 1→$26 (NT1)
        asl     a
        ora     #$22
        sta     $0780
        lda     #$2B                    ; PPU low byte = $2B (column 11)
        sta     $0781
        lda     #$00                    ; $0782 = 0 (single tile write mode)
        sta     $0782
        sta     $95                     ; $95 = frame counter for 4-frame pacing
        lda     #$FF                    ; $0784 = terminator
        sta     $0784

; Compute name table offset: stage * 10 = stage * 2 + stage * 8
        lda     $22                     ; $00 = stage * 2
        asl     a
        sta     $00
        asl     a                       ; A = stage * 8
        asl     a
        adc     $00                     ; A = stage * 10
        sta     $10                     ; $10 = offset into boss name table

; Write one tile per 4 frames, advancing across the nametable row.
; 10 columns ($2B to $34) × 4 frames = 40 frames total.
LA0FB:  ldy     $10                     ; Y = current name table offset
        lda     transition_sprite_palette,y ; load tile ID (character)
        sta     $0783                   ; → PPU write queue tile data
        inc     $19                     ; flag PPU write pending
        lda     #$00                    ; allow NMI
        sta     $EE
        jsr     LFF21                   ; wait for NMI (tile gets uploaded)
        inc     $EE                     ; skip next NMI (pacing)
        inc     $95                     ; frame counter
        lda     $95
        and     #$03                    ; every 4 frames:
        bne     LA0FB                   ; re-write same tile (visual hold)
        inc     $10                     ; advance to next character
        inc     $0781                   ; advance PPU column
        lda     $0781
        cmp     #$35                    ; stop at column $35 (10 tiles done)
        bne     LA0FB

; --- Final wait and return ---
LA122:  lda     #$00                    ; A = 0
LA124:  pha
        lda     #$00
        sta     $EE                     ; allow NMI
        jsr     LFF21                   ; wait 1 frame
        inc     $EE
        pla
        sec
        sbc     #$01                    ; countdown (wraps: 0→$FF→254 loops)
        bne     LA124
        lda     #$00
        sta     $EE
        rts                             ; → returns to bank18 robot_master_intro

LA139:  .byte   $36,$22,$26,$2B,$00,$45,$32,$1F
        .byte   $3F,$65,$00,$65,$00,$00,$00,$65
        .byte   $00,$65
LA14B:  .byte   $89,$00,$77,$3C,$00,$C4,$00,$00
        .byte   $00,$89,$00,$77,$3C,$00,$C4,$00
        .byte   $00,$00
LA15D:  .byte   $03,$00,$FC,$02,$00,$FD,$03,$00
        .byte   $FD,$03,$00,$FC,$02,$00,$FD,$03
        .byte   $00,$FD
LA16F:  .byte   $C0,$D4,$C0,$79,$00,$79,$A8,$54
        .byte   $A8,$C0,$D4,$C0,$79,$00,$79,$A8
        .byte   $54,$A8
LA181:  .byte   $FF,$02,$FF,$04,$00,$04,$05,$06
        .byte   $05,$FF,$02,$FF,$04,$00,$04,$05
        .byte   $06,$05
LA193:  .byte   $30,$30,$30,$70,$70,$70,$B0,$B0
        .byte   $B0,$30,$30,$30,$70,$70,$70,$B0
        .byte   $B0,$B0
LA1A5:  .byte   $30,$80,$D0,$30,$80,$D0,$30,$80
        .byte   $D0,$30,$80,$D0,$30,$80,$D0,$30
        .byte   $80,$D0
LA1B7:  .byte   $28,$26,$25,$24,$00,$2A,$27,$23
        .byte   $29,$1E,$00,$1E,$00,$00,$00,$1E
        .byte   $00,$1E
LA1C9:  .byte   $04,$03,$05,$06,$02,$02,$08,$03
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
LA1D9:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37

; Intro sprite palette data (8 bytes → SP 0 + SP 1)
transition_sprite_palette:  .byte   $17,$0E,$0E,$0D,$15,$0E,$25,$16
        .byte   $0A,$17,$16,$0A,$10,$17,$0E,$1D
        .byte   $25,$16,$0A,$17,$10,$0E,$16,$12
        .byte   $17,$12,$25,$16,$0A,$17,$11,$0A
        .byte   $1B,$0D,$25,$25,$25,$16,$0A,$17
        .byte   $1D,$18,$19,$25,$25,$25,$25,$16
        .byte   $0A,$17,$1C,$17,$0A,$14,$0E,$25
        .byte   $25,$16,$0A,$17,$1C,$19,$0A,$1B
        .byte   $14,$25,$25,$16,$0A,$17,$1C,$11
        .byte   $0A,$0D,$18,$20,$25,$16,$0A,$17
        .byte   $AE,$C0,$02,$C8,$AE,$C1,$02,$D0
        .byte   $B6,$C2,$02,$C8,$B6,$C3,$02,$D0
        .byte   $2F,$C4,$02,$C7,$2F,$C5,$02,$D0
        .byte   $AE,$DC,$02,$29,$AE,$DD,$02,$31
        .byte   $B6,$DE,$02,$29,$B6,$DF,$02,$31
        .byte   $67,$C6,$02,$28,$6F,$C7,$02,$20
        .byte   $6F,$C8,$02,$28,$6F,$C9,$02,$30
        .byte   $77,$CA,$02,$27,$77,$CB,$02,$2F
        .byte   $6D,$D8,$02,$C8,$6D,$D9,$02,$D0
        .byte   $75,$DA,$02,$C8,$75,$DB,$02,$D0
        .byte   $7D,$EC,$02,$CC,$2B,$E0,$02,$78
        .byte   $2A,$E2,$02,$80,$33,$E1,$02,$78
        .byte   $32,$E3,$02,$80,$9E,$D4,$01,$79
        .byte   $9E,$D4,$01,$85,$A6,$D5,$01,$79
        .byte   $AE,$D6,$01,$79,$AE,$D7,$01,$81
        .byte   $1C,$CC,$01,$25,$1C,$CD,$01,$2D
        .byte   $24,$CE,$01,$24,$24,$CF,$01,$2D
        .byte   $24,$D0,$01,$35,$34,$D1,$02,$25
        .byte   $34,$D2,$02,$2D,$34,$D3,$02,$35
        .byte   $67,$E6,$03,$74,$67,$6F,$03,$7C
        .byte   $67,$E6,$43,$84,$6F,$E7,$03,$74
        .byte   $6F,$E8,$03,$7C,$6F,$E7,$43,$84
        .byte   $77,$E9,$03,$74,$77,$EA,$03,$7C
        .byte   $77,$EB,$03,$84,$58,$E4,$00,$67
        .byte   $58,$E4,$00,$91,$7E,$E4,$00,$67
        .byte   $7E,$E4,$00,$91,$22,$EA,$0F,$1C
        .byte   $1D,$0A,$10,$0E,$25,$25,$1C,$0E
        .byte   $15,$0E,$0C,$1D,$25,$25,$25,$23
        .byte   $2A,$0A,$19,$0A,$1C,$1C,$25,$25
        .byte   $25,$20,$18,$1B,$0D,$21,$D7,$02
        .byte   $34,$17,$0D,$FF,$23,$0A,$0A,$10
        .byte   $0A,$16,$0E,$25,$25,$25,$18,$1F
        .byte   $0E,$1B,$21,$D7,$02,$34,$17,$0D
        .byte   $FF,$22,$EA,$0C,$1C,$1D,$0A,$10
        .byte   $0E,$25,$25,$1C,$0E,$15,$0E,$0C
        .byte   $1D,$23,$0A,$0A,$25,$25,$25,$25
        .byte   $25,$25,$25,$25,$25,$25,$25,$23
        .byte   $2A,$07,$0C,$18,$17,$1D,$12,$17
        .byte   $1E,$0E,$21,$D7,$02,$34,$17,$0D
        .byte   $FF,$20,$4A,$0B,$19,$1E,$1C,$11
        .byte   $25,$25,$25,$1C,$1D,$0A,$1B,$1D
        .byte   $21,$23,$05,$1C,$19,$0A,$1B,$14
        .byte   $25,$21,$2D,$05,$1C,$17,$0A,$14
        .byte   $0E,$25,$21,$37,$05,$17,$0E,$0E
        .byte   $0D,$15,$0E,$21,$43,$05,$25,$25
        .byte   $25,$16,$0A,$17,$21,$4D,$05,$25
        .byte   $25,$25,$16,$0A,$17,$21,$57,$05
        .byte   $25,$25,$25,$16,$0A,$17,$FF,$22
        .byte   $23,$05,$11,$0A,$1B,$0D,$25,$25
        .byte   $22,$37,$05,$1D,$18,$19,$25,$25
        .byte   $25,$22,$43,$05,$25,$25,$25,$16
        .byte   $0A,$17,$22,$57,$05,$25,$25,$25
        .byte   $16,$0A,$17,$23,$23,$05,$10,$0E
        .byte   $16,$12,$17,$12,$23,$2D,$05,$16
        .byte   $0A,$10,$17,$0E,$1D,$23,$37,$05
        .byte   $1C,$11,$0A,$0D,$18,$20,$23,$43
        .byte   $05,$25,$25,$25,$16,$0A,$17,$23
        .byte   $4D,$05,$25,$25,$25,$16,$0A,$17
        .byte   $23,$57,$05,$25,$25,$25,$16,$0A
        .byte   $17,$FF,$21,$88,$06,$22,$18,$1E
        .byte   $25,$10,$18,$1D,$FF,$21,$E5,$0C
        .byte   $17,$0E,$0E,$0D,$15,$0E,$25,$0C
        .byte   $0A,$17,$17,$18,$17,$22,$4A,$02
        .byte   $0A,$17,$0D,$22,$88,$08,$1B,$1E
        .byte   $1C,$11,$25,$25,$13,$0E,$1D,$FF
        .byte   $21,$E5,$0D,$16,$0A,$10,$17,$0E
        .byte   $1D,$25,$16,$12,$1C,$1C,$12,$15
        .byte   $0E,$FF,$21,$E5,$0B,$10,$0E,$16
        .byte   $12,$17,$12,$25,$15,$0A,$1C,$0E
        .byte   $1B,$FF,$21,$E7,$0B,$11,$0A,$1B
        .byte   $0D,$25,$14,$17,$1E,$0C,$14,$15
        .byte   $0E,$FF,$21,$E7,$07,$1D,$18,$19
        .byte   $25,$1C,$19,$12,$17,$FF,$21,$E7
        .byte   $0B,$1C,$0E,$0A,$1B,$0C,$11,$25
        .byte   $1C,$17,$0A,$14,$0E,$FF,$21,$E6
        .byte   $0A,$1C,$19,$0A,$1B,$14,$25,$1C
        .byte   $11,$18,$0C,$14,$FF,$21,$E5,$0B
        .byte   $1C,$11,$0A,$0D,$18,$20,$25,$0B
        .byte   $15,$0A,$0D,$0E,$22,$4A,$02,$0A
        .byte   $17,$0D,$22,$88,$0A,$1B,$1E,$1C
        .byte   $11,$25,$16,$0A,$1B,$12,$17,$0E
        .byte   $FF,$22,$2D,$05,$0B,$1B,$0E,$0A
        .byte   $14,$25,$22,$4D,$05,$25,$25,$25
        .byte   $16,$0A,$17,$FF,$22,$EA,$0F,$19
        .byte   $0A,$1C,$1C,$25,$20,$18,$1B,$0D
        .byte   $25,$0E,$1B,$1B,$18,$1B,$29,$23
        .byte   $2A,$0A,$25,$25,$25,$25,$25,$25
        .byte   $25,$25,$25,$25,$25,$FF,$21,$67
        .byte   $12,$3C,$01,$09,$09,$00,$25,$0C
        .byte   $0A,$19,$0C,$18,$16,$25,$0C,$18
        .byte   $26,$15,$1D,$0D,$21,$A1,$1E,$1D
        .byte   $16,$25,$0A,$17,$0D,$25,$3C,$01
        .byte   $09,$09,$00,$25,$0C,$0A,$19,$0C
        .byte   $18,$16,$25,$1E,$26,$1C,$26,$0A
        .byte   $26,$27,$12,$17,$0C,$26,$FF,$21
        .byte   $EB,$0A,$15,$12,$0C,$0E,$17,$1C
        .byte   $0E,$0D,$25,$0B,$22,$22,$24,$18
        .byte   $17,$12,$17,$1D,$0E,$17,$0D,$18
        .byte   $25,$18,$0F,$25,$0A,$16,$0E,$1B
        .byte   $12,$0C,$0A,$26,$25,$12,$17,$0C
        .byte   $26,$FF,$22,$EA,$0F,$1C,$1D,$0A
        .byte   $10,$0E,$25,$25,$1C,$0E,$15,$0E
        .byte   $0C,$1D,$25,$25,$25,$21,$D7,$02
        .byte   $34,$17,$0D,$FF,$FD,$25,$3A,$6A
        .byte   $B0,$0B,$16,$39,$4B,$5B,$6B,$77
        .byte   $87,$96,$BA,$CD,$EF,$28,$53,$A2
        .byte   $A3,$A3,$A3,$A3,$A4,$A4,$A4,$A4
        .byte   $A4,$A4,$A4,$A4,$A4,$A4,$A4,$A4
        .byte   $A5,$A5
code_A593:  lda     #$00
        sta     $11
        ldy     #$2F
code_A599:  sta     $0150,y
        dey
        bpl     code_A599
        lda     #$24
        sta     $10
        ldy     #$14
code_A5A5:  lda     LA6FF,y
        sta     $0204,y
        lda     LA700,y
        sta     $0205,y
        lda     LA701,y
        sta     $0206,y
        lda     LA702,y
        sta     $0207,y
        dey
        dey
        dey
        dey
        bpl     code_A5A5
code_A5C3:  lda     $14
        and     #$80
        bne     code_A603
        lda     $14
        and     #$03
        beq     code_A5E2
        lda     $10
        cmp     #$26
        beq     code_A5F2
        lda     #$24
        cmp     $10
        bne     code_A5DD
        lda     #$25
code_A5DD:  sta     $10
        jmp     code_A5F2

code_A5E2:  lda     $14
        and     #$0C
        beq     code_A5F2
        lda     #$26
        cmp     $10
        bne     code_A5F0
        lda     #$24
code_A5F0:  sta     $10
code_A5F2:  jsr     code_A681
        lda     #$00
        sta     $EE
        jsr     LFF21
        inc     $EE
        inc     $95
        jmp     code_A5C3

code_A603:  lda     $10
        cmp     #$26
        bne     code_A60C
        jmp     stage_select_progression

code_A60C:  lda     $10
        and     #$01
        sta     $13
        lda     #$00
        sta     $10
        lda     #$00
        sta     $11
        beq     code_A66D
code_A61C:  lda     $14
        and     #$40
        beq     code_A62D
        lda     #$24
        sta     $10
        lda     #$00
        sta     $11
        jmp     code_A5C3

code_A62D:  lda     $14
        and     #$80
        beq     code_A66D
        lda     $10
        clc
        adc     $11
        tay
        asl     a
        asl     a
        tax
        lda     $0150,y
        bpl     code_A64D
        lda     #$F8
        sta     $021C,x
        lda     #$00
        sta     $0150,y
        beq     code_A66D
code_A64D:  lda     #$F0
        ora     $13
        sta     $0150,y
        lda     LA73E,y
        sta     $021C,x
        lda     #$E4
        clc
        adc     $13
        sta     $021D,x
        lda     #$00
        sta     $021E,x
        lda     LA717,y
        sta     $021F,x
code_A66D:  jsr     code_A6AF
        jsr     code_A681
        lda     #$00
        sta     $EE
        jsr     LFF21
        inc     $EE
        inc     $95
        jmp     code_A61C

code_A681:  lda     $10
        clc
        adc     $11
        tay
        lda     LA717,y
        sta     $00
        lda     LA73E,y
        sta     $01
        ldx     #$0C
        ldy     #$03
code_A695:  lda     $00
        clc
        adc     LA6FB,y
        sta     $0207,x
        lda     $01
        clc
        adc     LA6F7,y
        sta     $0204,x
        dex
        dex
        dex
        dex
        dey
        bpl     code_A695
        rts

code_A6AF:  lda     $14
        and     #$03
        beq     code_A6CF
        and     #$01
        beq     code_A6C7
        inc     $10
        lda     $10
        cmp     #$06
        bne     code_A6CF
        lda     #$00
        sta     $10
        beq     code_A6CF
code_A6C7:  dec     $10
        bpl     code_A6CF
        lda     #$05
        sta     $10
code_A6CF:  lda     $14
        and     #$0C
        beq     code_A6F6
        and     #$04
        bne     code_A6E7
        lda     $11
        sec
        sbc     #$06
        sta     $11
        bcs     code_A6E6
        lda     #$1E
        sta     $11
code_A6E6:  rts

code_A6E7:  lda     $11
        clc
        adc     #$06
        sta     $11
        cmp     #$1F
        bcc     code_A6F6
        lda     #$00
        sta     $11
code_A6F6:  rts

LA6F7:  .byte   $FC,$FC,$04,$04
LA6FB:  .byte   $FC,$04,$FC,$04
LA6FF:  .byte   $3B
LA700:  .byte   $EE
LA701:  .byte   $00
LA702:  .byte   $B4,$3B,$EE,$40,$BC,$43,$EE,$80
        .byte   $B4,$43,$EE,$C0,$BC,$3F,$E4,$00
        .byte   $B8,$3F,$E5,$00,$C8
LA717:  .byte   $38,$48,$58,$68,$78,$88,$38,$48
        .byte   $58,$68,$78,$88,$38,$48,$58,$68
        .byte   $78,$88,$38,$48,$58,$68,$78,$88
        .byte   $38,$48,$58,$68,$78,$88,$38,$48
        .byte   $58,$68,$78,$88,$B8,$C8,$C0
LA73E:  .byte   $27,$27,$27,$27,$27,$27,$37,$37
        .byte   $37,$37,$37,$37,$47,$47,$47,$47
        .byte   $47,$47,$5F,$5F,$5F,$5F,$5F,$5F
        .byte   $6F,$6F,$6F,$6F,$6F,$6F,$7F,$7F
        .byte   $7F,$7F,$7F,$7F,$3F,$3F,$70

; -----------------------------------------------
; stage_select_progression: rebuilds $60/$61 from save data
; scans $0150,x completion flags to determine which tier
; the player is on and which bosses are defeated.
;   $60 = stage select tier (0=Robot Master, $09=Doc Robot, $12=Wily)
;   $61 = boss-defeated bitmask ($FF = all beaten in current tier)
; -----------------------------------------------
stage_select_progression:  lda     #$00 ; reset tier and defeat
        sta     $60                     ; bitmask to start fresh
        sta     $61
        sta     $EE
        ldy     #$0C
code_A76F:  ldx     LA9DF,y
        lda     $0150,x
        bne     code_A77C
        dey
        bpl     code_A76F
        bmi     code_A7A6
code_A77C:  lda     $FD
        asl     a
        asl     a
        and     #$04
        sta     $10
        ldx     #$0F
        jsr     L939E
        ldx     #$B4
        jsr     LFF1A
        ldx     #$00
        jsr     L939E
        ldy     #$04
        lda     #$F8
code_A797:  sta     $0200,y
        iny
        iny
        iny
        iny
        bne     code_A797
        jsr     LFF21
        jmp     code_A593

code_A7A6:  ldy     #$00
code_A7A8:  ldx     LA9B1,y
        lda     $0150,x
        beq     code_A804
        and     #$01
        beq     code_A7C4
        ldx     LA9B7,y
        lda     $0150,x
        bne     code_A77C
        lda     LA9C8,y
        ora     LA9CE,y
        bne     code_A7CF
code_A7C4:  ldx     LA9B7,y
        lda     $0150,x
        bne     code_A77C
        lda     LA9C8,y
code_A7CF:  ora     $61                 ; accumulate defeated bit
        sta     $61                     ; into boss-defeated bitmask
code_A7D3:  iny
        cpy     #$04
        bcc     code_A7A8
        cpy     #$06
        beq     check_doc_robot_complete
        lda     $60                     ; if already in Doc Robot tier,
        bne     code_A7A8               ; skip Robot Master check
        lda     $61                     ; all 8 Robot Masters beaten?
        cmp     #$FF                    ; ($FF = bits 0-7 all set)
        bne     code_A7F0
        lda     #$09                    ; advance to Doc Robot tier
        sta     $60                     ; $60 = $09 (stage select offset)
        lda     #$3A                    ; $61 = $3A (pre-set defeated bits
        sta     $61                     ; for stages without Doc Robots)
        bne     code_A7A8
code_A7F0:  lda     $0157
        ora     $0150
        ora     $015B
        ora     $0153
        ora     $0168
        beq     code_A833
        jmp     code_A77C

code_A804:  ldx     LA9B7,y             ; check Doc Robot completion
        lda     $0150,x                 ; for this stage pair
        beq     code_A7D3
        lda     $61                     ; mark Doc Robot stage defeated
        ora     LA9CE,y                 ; using Doc Robot bitmask table
        sta     $61
        jmp     code_A7D3

check_doc_robot_complete:  lda     $61  ; all 4 Doc Robot stages beaten?
        cmp     #$FF                    ; ($FF = all bits set)
        bne     code_A82B
        lda     #$12                    ; advance to Wily tier
        sta     $60                     ; $60 = $12
        lda     $0168                   ; if Break Man defeated too,
        beq     code_A833               ; done
        lda     #$FF                    ; $60 = $FF marks all stages
        sta     $60                     ; complete (Wily fortress)
        bne     code_A833
code_A82B:  lda     $0168
        beq     code_A833
        jmp     code_A77C

code_A833:  ldy     #$09
        lda     #$01
        sta     $00
code_A839:  ldx     LA9BE,y
        lda     $0150,x
        beq     code_A843
        dec     $00
code_A843:  dey
        bpl     code_A839
        lda     $00
        beq     code_A84D
        jmp     code_A77C

code_A84D:  ldy     #$09
code_A84F:  ldx     LA9BE,y
        lda     $0150,x
        bne     code_A85D
        dey
        bpl     code_A84F
        jmp     code_A77C

code_A85D:  jsr     code_A88E
        lda     LA9D5,y
        sta     $AF
        ldy     #$04
        lda     #$F8
code_A869:  sta     $0200,y
        dey
        dey
        dey
        dey
        bne     code_A869
        lda     $60
        bmi     code_A879
        jmp     L9212

code_A879:  pla
        pla
        lda     #$80
        sta     $74
        lda     #$00
        sta     $75
        ldy     #$1F
code_A885:  sta     $0150,y
        dey
        bpl     code_A885
        jmp     LCBCE

code_A88E:  sty     $00
        ldy     #$00
code_A892:  ldx     LA9B1,y
        lda     $0150,x
        beq     code_A8A9
        pha
        ldx     LA9EC,y
        lda     #$9C
        sta     $A2,x
        pla
        and     #$01
        beq     code_A8B8
        bne     code_A8B1
code_A8A9:  ldx     LA9B7,y
        lda     $0150,x
        beq     code_A8B8
code_A8B1:  ldx     LA9F2,y
        lda     #$9C
        sta     $A2,x
code_A8B8:  iny
        cpy     #$04
        bne     code_A892
        lda     #$9C
        sta     $A9
        ldy     $0164
        beq     code_A8C8
        sta     $AD
code_A8C8:  ldy     $0167
        bne     code_A8D4
        ldy     $0171
        cpy     #$F1
        bne     code_A8D6
code_A8D4:  sta     $AB
code_A8D6:  lda     #$02
        sta     $AE
        ldy     $00
        rts

        lda     $61
        sta     $10
        lda     $60
        beq     code_A8E9
        lda     #$FF
        sta     $10
code_A8E9:  ldy     #$00
code_A8EB:  lda     #$00
        sta     $13
        lda     $10
        and     #$03
        beq     code_A90B
        cmp     #$03
        beq     code_A906
        and     #$01
        bne     code_A908
        lda     LA9B7,y
        jsr     code_A988
        jmp     code_A90B

code_A906:  inc     $13
code_A908:  jsr     code_A985
code_A90B:  lsr     $10
        lsr     $10
        iny
        cpy     #$04
        bne     code_A8EB
        lda     $61
        sta     $10
        lda     $60
        beq     code_A975
        cmp     #$12
        bcc     code_A924
        lda     #$FF
        sta     $10
code_A924:  lda     #$00
        sta     $13
        ldy     #$04
        lda     $10
        and     #$05
        beq     code_A946
        cmp     #$05
        beq     code_A941
        and     #$01
        bne     code_A943
        lda     LA9B7,y
        jsr     code_A988
        jmp     code_A946

code_A941:  inc     $13
code_A943:  jsr     code_A985
code_A946:  lda     #$00
        sta     $13
        ldy     #$05
        lda     $10
        and     #$C0
        beq     code_A968
        cmp     #$C0
        beq     code_A963
        and     #$40
        bne     code_A965
        lda     LA9B7,y
        jsr     code_A988
        jmp     code_A968

code_A963:  inc     $13
code_A965:  jsr     code_A985
code_A968:  lda     $60
        bpl     code_A975
        lda     #$00
        sta     $13
        ldy     #$0C
        jsr     code_A985
code_A975:  lda     #$00
        sta     $13
        lda     $AF
        cmp     #$09
        bcc     code_A981
        lda     #$09
code_A981:  clc
        adc     #$0D
        tay
code_A985:  lda     LA9B1,y
code_A988:  sty     $00
        tay
        asl     a
        asl     a
        tax
        lda     #$F0
        ora     $13
        sta     $0150,y
        lda     LA73E,y
        sta     $021C,x
        lda     #$E4
        clc
        adc     $13
        sta     $021D,x
        lda     #$00
        sta     $021E,x
        lda     LA717,y
        sta     $021F,x
        ldy     $00
        rts

; stage_select_progression lookup tables:
; $A9B1: Robot Master completion slot indices (y=0..5)
; $A9B7: Doc Robot completion slot indices (y=0..5)
; $A9BE: E-tank/item slot indices (y=0..9)
; $A9C8: Robot Master defeat bitmask per pair (y=0..5)
; $A9CE: Doc Robot defeat bitmask per pair (y=0..5)
; $A9D4: combined bitmask (both tiers)
; $A9DF: initial scan slot indices (y=0..12)

LA9B1:  .byte   $14,$0A,$02,$21,$07,$00
LA9B7:  .byte   $22,$0F,$23,$17,$0B,$03,$18
LA9BE:  .byte   $10,$1D,$1B,$09,$04,$0C,$13,$0E
        .byte   $1F,$05
LA9C8:  .byte   $01,$04,$10,$40,$3B,$7A
LA9CE:  .byte   $02,$08,$20,$80,$3E,$BA,$FF
LA9D5:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09
LA9DF:  .byte   $01,$06,$08,$0D,$11,$12,$15,$16
        .byte   $19,$1A,$1C,$1E,$20
LA9EC:  .byte   $02,$01,$05,$08,$00,$00
LA9F2:  .byte   $04,$03,$06,$0A,$00,$24,$80,$58
        .byte   $80,$D0,$82,$91,$80,$00,$00,$01
        .byte   $02,$03,$04,$05,$06,$07,$08,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
        .byte   $12,$13,$14,$15,$16,$17,$18,$42
        .byte   $00,$00,$20,$00,$00,$C0,$00,$20
        .byte   $80,$20,$80,$00,$00,$04,$03,$10
        .byte   $00,$44,$00,$21,$02,$12,$15,$06
        .byte   $06,$28,$08,$52,$20,$44,$22,$38
        .byte   $22,$41,$08,$40,$02,$00,$25,$80
        .byte   $A2,$80,$80,$A3,$80,$A1,$40,$40
        .byte   $60,$20,$20,$80,$00,$20,$00,$00
        .byte   $00,$00,$02,$10,$00,$03,$00,$41
        .byte   $02,$04,$00,$04,$20,$00,$2F,$00
        .byte   $17,$00,$05,$00,$17,$00,$05,$00
        .byte   $2D,$00,$00,$00,$2F,$00,$0E,$00
        .byte   $0E,$00,$0B,$00,$0B,$00,$24,$00
        .byte   $0D,$00,$00,$21,$02,$25,$50,$52
        .byte   $0F,$37,$27,$06,$0F,$07,$08,$09
        .byte   $0F,$39,$29,$19,$0F,$21,$08,$30
        .byte   $00,$00,$00,$00,$20,$0C,$20,$90
        .byte   $20,$A0,$20,$C4,$80,$40,$00,$00
        .byte   $02,$02,$00,$08,$22,$01,$02,$04
        .byte   $00,$04,$20,$01,$80,$44,$00,$90
        .byte   $00,$00,$00,$40,$00,$00,$00,$A0
        .byte   $88,$08,$AA,$19,$00,$4C,$20,$10
        .byte   $00,$00,$08,$28,$22,$40,$28,$10
        .byte   $00,$20,$00,$01,$20,$02,$02,$00
        .byte   $20,$20,$88,$10,$20,$01,$00,$02
        .byte   $00,$06,$20,$80,$00,$04,$80,$49
        .byte   $8A,$00,$08,$25,$A8,$21,$80,$00
        .byte   $00,$02,$00,$04,$AA,$22,$05,$0B
        .byte   $00,$0D,$0A,$40,$28,$45,$0C,$16
        .byte   $FF,$A3,$80,$13,$80,$60,$01,$02
        .byte   $03,$04,$04,$04,$05,$05,$05,$05
        .byte   $06,$08,$08,$0A,$0A,$0B,$0D,$0E
        .byte   $0F,$0F,$10,$10,$10,$10,$10,$11
        .byte   $11,$11,$11,$11,$11,$11,$12,$12
        .byte   $12,$12,$13,$14,$15,$17,$FF,$00
        .byte   $28,$02,$88,$08,$08,$40,$00,$08
        .byte   $22,$30,$00,$04,$02,$00,$02,$40
        .byte   $00,$20,$A2,$28,$02,$4B,$00,$08
        .byte   $00,$00,$00,$02,$20,$00,$00,$E1
        .byte   $08,$00,$00,$03,$02,$01,$00,$81
        .byte   $00,$90,$00,$00,$20,$40,$00,$73
        .byte   $00,$E7,$00,$00,$08,$22,$00,$91
        .byte   $80,$00,$22,$84,$28,$80,$00,$01
        .byte   $00,$0A,$00,$20,$00,$03,$20,$28
        .byte   $A0,$40,$00,$A0,$80,$32,$20,$00
        .byte   $20,$C0,$00,$1C,$00,$01,$00,$0A
        .byte   $00,$50,$08,$08,$00,$05,$02,$20
        .byte   $80,$30,$00,$10,$00,$48,$00,$20
        .byte   $20,$00,$00,$01,$88,$00,$00,$0D
        .byte   $00,$20,$82,$68,$02,$44,$00,$00
        .byte   $20,$00,$00,$00,$00,$11,$02,$00
        .byte   $00,$04,$08,$21,$00,$00,$00,$10
        .byte   $20,$10,$AA,$02,$02,$08,$00,$00
        .byte   $00,$00,$80,$00,$20,$84,$10,$27
        .byte   $80,$44,$30,$22,$00,$62,$00,$20
        .byte   $08,$04,$00,$01,$80,$00,$80,$04
        .byte   $80,$00,$00,$00,$20,$00,$02,$2C
        .byte   $08,$01,$02,$88,$00,$00,$20,$40
        .byte   $02,$80,$22,$80,$0A,$80,$A8,$14
        .byte   $80,$46,$00,$A0,$80,$00,$08,$90
        .byte   $28,$34,$02,$04,$00,$31,$80,$49
        .byte   $82,$20,$82,$00,$A0,$1A,$78,$88
        .byte   $88,$70,$90,$B0,$30,$50,$70,$90
        .byte   $78,$28,$C8,$28,$68,$68,$28,$28
        .byte   $48,$88,$38,$48,$58,$68,$78,$50
        .byte   $70,$90,$A8,$B0,$D0,$F0,$10,$30
        .byte   $50,$70,$30,$D8,$C8,$C8,$FF,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$40,$00,$00,$00,$00
        .byte   $00,$01,$06,$00,$40,$00,$00,$00
        .byte   $00,$00,$04,$10,$08,$00,$00,$00
        .byte   $45,$00,$00,$00,$00,$00,$01,$00
        .byte   $01,$00,$42,$00,$00,$00,$11,$00
        .byte   $90,$00,$10,$00,$00,$04,$90,$00
        .byte   $00,$00,$00,$00,$40,$04,$00,$00
        .byte   $02,$40,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$06,$00,$80,$10
        .byte   $23,$01,$00,$00,$01,$00,$00,$00
        .byte   $00,$00,$00,$00,$04,$00,$82,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$08,$00,$20,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$10,$00,$00,$00
        .byte   $81,$00,$00,$00,$09,$04,$80,$10
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$40
        .byte   $10,$00,$20,$00,$00,$00,$00,$00
        .byte   $01,$00,$00,$01,$00,$10,$00,$00
        .byte   $44,$14,$04,$00,$00,$50,$01,$00
        .byte   $60,$04,$00,$00,$00,$05,$02,$01
        .byte   $00,$00,$00,$00,$00,$00,$03,$14
        .byte   $00,$10,$00,$01,$18,$01,$28,$28
        .byte   $28,$B8,$B8,$B8,$58,$68,$68,$68
        .byte   $50,$B0,$A0,$98,$90,$B0,$84,$A4
        .byte   $44,$58,$A4,$3C,$3C,$3C,$34,$B8
        .byte   $B8,$B8,$38,$B8,$B8,$D8,$B8,$B8
        .byte   $C8,$B8,$04,$B8,$A8,$00,$FF,$40
        .byte   $00,$00,$02,$00,$20,$00,$00,$00
        .byte   $08,$04,$00,$00,$00,$00,$00,$00
        .byte   $0A,$04,$00,$00,$00,$00,$00,$00
        .byte   $08,$00,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$80,$10,$00,$00,$00,$00
        .byte   $04,$00,$02,$00,$00,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$04,$10
        .byte   $08,$00,$02,$00,$00,$00,$90,$00
        .byte   $40,$00,$00,$00,$00,$00,$02,$00
        .byte   $80,$00,$08,$00,$09,$10,$00,$00
        .byte   $02,$11,$80,$10,$00,$00,$00,$00
        .byte   $20,$00,$00,$01,$01,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$04,$00
        .byte   $00,$00,$00,$40,$02,$00,$20,$00
        .byte   $00,$04,$00,$11,$00,$00,$00,$00
        .byte   $40,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$00,$00,$00,$08,$00,$01,$00
        .byte   $00,$01,$04,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$04,$00
        .byte   $81,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$10
        .byte   $00,$00,$00,$00,$10,$00,$44,$00
        .byte   $02,$00,$81,$00,$00,$00,$00,$40
        .byte   $00,$14,$08,$01,$14,$00,$00,$00
        .byte   $00,$40,$00,$45,$00,$00,$44,$00
        .byte   $40,$00,$29,$00,$24,$04,$1F,$1F
        .byte   $1F,$21,$21,$21,$50,$21,$21,$21
        .byte   $03,$1D,$1D,$50,$03,$1D,$12,$12
        .byte   $12,$54,$09,$51,$51,$51,$09,$21
        .byte   $21,$21,$1F,$21,$21,$21,$21,$21
        .byte   $21,$21,$3E,$50,$13,$4A,$FF,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$04,$00,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$01,$00,$04,$50,$10,$01,$00
        .byte   $00,$40,$00,$00,$82,$00,$00,$00
        .byte   $20,$40,$00,$00,$00,$01,$20,$00
        .byte   $00,$00,$00,$00,$00,$10,$04,$00
        .byte   $22,$04,$06,$04,$10,$04,$00,$00
        .byte   $00,$00,$04,$40,$00,$00,$00,$00
        .byte   $00,$00,$80,$10,$00,$00,$00,$00
        .byte   $86,$01,$01,$10,$00,$00,$08,$10
        .byte   $00,$10,$00,$00,$04,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $40,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $22,$00,$00,$04,$00,$00,$00,$40
        .byte   $00,$10,$00,$00,$80,$40,$00,$00
        .byte   $20,$00,$00,$04,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$40,$10,$00,$00
        .byte   $00,$04,$00,$04,$00,$00,$00,$00
        .byte   $90,$00,$00,$10,$00,$14,$00,$00
        .byte   $08,$04,$08,$10,$00,$00,$20,$10
        .byte   $40,$00,$00,$40,$40,$44,$80,$04
        .byte   $00,$10,$00,$01,$01,$00,$1A,$00
        .byte   $00,$00,$08,$00,$00,$00,$00,$00
        .byte   $51,$04,$02,$05,$00,$00,$00,$01
        .byte   $02,$03,$04,$04,$04,$05,$04,$06
        .byte   $07,$04,$04,$04,$08,$09,$0A,$0B
        .byte   $0C,$0D,$0E,$04,$0A,$0B,$0F,$10
        .byte   $11,$12,$13,$14,$0F,$10,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$16,$16
        .byte   $16,$16,$16,$16,$16,$16,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$00,$02
        .byte   $18,$19,$04,$04,$04,$04,$04,$04
        .byte   $04,$04,$0E,$04,$06,$07,$0C,$0D
        .byte   $04,$04,$0A,$0B,$0C,$0D,$11,$12
        .byte   $13,$14,$0F,$10,$11,$12,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$16,$16
        .byte   $16,$16,$16,$16,$16,$16,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$04,$1A
        .byte   $1B,$00,$01,$02,$03,$0E,$04,$0E
        .byte   $04,$04,$1C,$04,$04,$04,$04,$04
        .byte   $0A,$0B,$0C,$0D,$06,$07,$13,$14
        .byte   $0F,$10,$11,$12,$13,$14,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$1D,$1E
        .byte   $1F,$15,$15,$15,$15,$15,$17,$17
        .byte   $20,$16,$16,$16,$16,$16,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$04,$04
        .byte   $04,$05,$00,$02,$18,$19,$04,$04
        .byte   $0E,$04,$06,$07,$04,$04,$0A,$0B
        .byte   $0C,$0D,$04,$04,$0A,$0B,$0F,$10
        .byte   $11,$12,$13,$14,$0F,$10,$21,$22
        .byte   $23,$16,$24,$15,$15,$15,$25,$17
        .byte   $17,$17,$17,$16,$26,$15,$17,$17
        .byte   $17,$17,$17,$17,$17,$16,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$04,$0E
        .byte   $04,$04,$0A,$0B,$27,$17,$04,$0A
        .byte   $0B,$0C,$0F,$10,$11,$28,$0C,$0F
        .byte   $10,$11,$15,$15,$15,$29,$11,$15
        .byte   $15,$15,$15,$15,$15,$15,$2A,$2B
        .byte   $15,$15,$15,$15,$15,$15,$2C,$17
        .byte   $24,$2D,$2D,$2D,$1D,$24,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$2E
        .byte   $2E,$2E,$2F,$30,$2E,$17,$31,$32
        .byte   $33,$33,$34,$35,$36,$2C,$31,$37
        .byte   $15,$15,$15,$38,$39,$2C,$30,$1E
        .byte   $3A,$3A,$3A,$3B,$3C,$2C,$15,$30
        .byte   $17,$17,$17,$2E,$2F,$2C,$15,$15
        .byte   $3D,$3E,$3F,$15,$38,$2C,$16,$16
        .byte   $16,$16,$16,$16,$16,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$40,$41
        .byte   $42,$43,$43,$43,$44,$45,$40,$41
        .byte   $36,$36,$36,$36,$36,$46,$40,$41
        .byte   $47,$48,$48,$48,$49,$4A,$40,$41
        .byte   $48,$4B,$4C,$4D,$48,$4E,$40,$4F
        .byte   $49,$50,$51,$52,$47,$4A,$40,$53
        .byte   $48,$50,$54,$36,$47,$4E,$40,$55
        .byte   $4C,$40,$56,$4C,$4C,$57,$40,$40
        .byte   $40,$40,$41,$50,$40,$40,$17,$58
        .byte   $2E,$2E,$2E,$2E,$59,$17,$17,$5A
        .byte   $36,$36,$36,$36,$5B,$2E,$17,$5C
        .byte   $5D,$5E,$47,$48,$5F,$36,$17,$60
        .byte   $2C,$17,$61,$48,$48,$47,$17,$60
        .byte   $2C,$17,$17,$61,$47,$48,$17,$60
        .byte   $2C,$17,$17,$17,$61,$47,$17,$60
        .byte   $2C,$17,$17,$17,$17,$16,$17,$60
        .byte   $2C,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$2E,$17
        .byte   $2E,$62,$63,$64,$2E,$2E,$36,$65
        .byte   $36,$36,$47,$36,$36,$36,$48,$47
        .byte   $48,$49,$48,$49,$48,$47,$49,$48
        .byte   $48,$48,$48,$47,$48,$48,$48,$48
        .byte   $49,$48,$47,$48,$66,$67,$16,$16
        .byte   $16,$16,$16,$16,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$31,$68,$2C,$2E,$17
        .byte   $17,$62,$63,$69,$6A,$2C,$36,$6B
        .byte   $69,$36,$48,$36,$68,$2C,$48,$48
        .byte   $36,$48,$47,$48,$48,$2C,$48,$6C
        .byte   $48,$47,$48,$47,$48,$2C,$16,$16
        .byte   $16,$16,$16,$16,$16,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$40,$6D
        .byte   $40,$40,$6D,$6E,$6F,$50,$40,$6D
        .byte   $40,$40,$6D,$6E,$70,$50,$40,$71
        .byte   $43,$43,$71,$52,$6F,$50,$6E,$36
        .byte   $36,$36,$36,$36,$70,$50,$6E,$47
        .byte   $47,$48,$49,$48,$70,$50,$40,$4C
        .byte   $4C,$4C,$4C,$4D,$6F,$50,$40,$40
        .byte   $40,$40,$40,$6E,$6F,$50,$40,$40
        .byte   $40,$40,$40,$6E,$70,$50,$31,$72
        .byte   $17,$17,$17,$17,$17,$17,$31,$73
        .byte   $2E,$2E,$2E,$2E,$2E,$17,$31,$74
        .byte   $75,$36,$36,$76,$36,$2C,$31,$6A
        .byte   $77,$47,$48,$78,$47,$2C,$31,$79
        .byte   $7A,$7B,$7B,$7C,$7D,$2C,$31,$48
        .byte   $7E,$48,$47,$7F,$6C,$2C,$17,$80
        .byte   $16,$16,$80,$24,$81,$2C,$17,$82
        .byte   $17,$17,$82,$31,$68,$2C,$83,$43
        .byte   $43,$43,$84,$85,$86,$86,$6E,$87
        .byte   $87,$87,$88,$89,$8A,$86,$6E,$8B
        .byte   $86,$8A,$8C,$89,$86,$86,$6E,$8D
        .byte   $8E,$86,$8C,$89,$8B,$86,$6E,$8F
        .byte   $6E,$8B,$90,$89,$86,$8A,$6E,$8F
        .byte   $6E,$86,$91,$92,$93,$4C,$6E,$8F
        .byte   $40,$4C,$4C,$4C,$57,$40,$6E,$8F
        .byte   $40,$40,$40,$40,$40,$40,$8B,$94
        .byte   $45,$40,$40,$40,$40,$40,$8B,$95
        .byte   $96,$97,$40,$40,$40,$40,$86,$98
        .byte   $99,$97,$40,$40,$40,$40,$8A,$9A
        .byte   $8A,$94,$43,$43,$43,$43,$8A,$9B
        .byte   $99,$9C,$9D,$9E,$9C,$9D,$4C,$4C
        .byte   $9F,$86,$A0,$86,$8A,$A0,$40,$40
        .byte   $40,$4C,$4C,$4C,$4C,$4C,$40,$40
        .byte   $40,$40,$40,$40,$40,$40,$40,$40
        .byte   $40,$43,$43,$43,$43,$43,$40,$40
        .byte   $6E,$A1,$87,$A2,$A3,$87,$40,$40
        .byte   $6E,$98,$A4,$99,$A5,$A6,$43,$43
        .byte   $52,$9A,$86,$8B,$4B,$4C,$9E,$9C
        .byte   $A7,$9B,$A8,$A9,$40,$40,$8A,$86
        .byte   $A6,$9A,$AA,$AB,$AC,$AB,$4C,$4C
        .byte   $4C,$4C,$4C,$4C,$4C,$4C,$40,$40
        .byte   $40,$40,$40,$40,$40,$40,$43,$43
        .byte   $43,$43,$AD,$AE,$41,$50,$87,$87
        .byte   $87,$87,$AF,$AE,$41,$50,$86,$86
        .byte   $8A,$8B,$86,$AE,$41,$50,$4C,$4C
        .byte   $4C,$4C,$4C,$B0,$AD,$50,$40,$40
        .byte   $40,$40,$40,$B0,$AF,$50,$AB,$AB
        .byte   $AB,$AB,$AB,$B1,$8A,$50,$4C,$4C
        .byte   $4C,$4C,$4C,$4C,$4C,$40,$40,$40
        .byte   $40,$40,$40,$40,$40,$40,$31,$72
        .byte   $17,$2E,$2E,$2E,$2E,$17,$31,$B2
        .byte   $B3,$36,$36,$36,$36,$2C,$31,$6A
        .byte   $B4,$16,$16,$B5,$3C,$2C,$31,$6A
        .byte   $30,$2E,$2E,$B6,$60,$2C,$31,$48
        .byte   $36,$36,$36,$B7,$60,$2C,$B8,$B9
        .byte   $B9,$BA,$BB,$B7,$60,$2C,$17,$17
        .byte   $17,$17,$60,$B7,$60,$2C,$17,$17
        .byte   $17,$17,$60,$B7,$60,$2C,$6E,$04
        .byte   $1A,$1B,$00,$01,$02,$03,$6E,$07
        .byte   $04,$04,$04,$06,$07,$04,$6E,$0A
        .byte   $0B,$0C,$0D,$0E,$04,$0A,$6E,$0F
        .byte   $10,$11,$12,$13,$14,$0F,$6E,$15
        .byte   $15,$15,$15,$15,$15,$15,$6E,$15
        .byte   $2D,$2D,$2D,$2D,$2D,$15,$6E,$8D
        .byte   $4C,$4C,$4C,$4C,$4D,$2D,$6E,$8F
        .byte   $40,$40,$40,$40,$40,$4C,$04,$04
        .byte   $05,$00,$01,$02,$18,$50,$04,$06
        .byte   $07,$04,$1C,$04,$04,$50,$0B,$0C
        .byte   $0D,$0E,$04,$0A,$0B,$50,$10,$11
        .byte   $12,$13,$14,$0F,$10,$50,$15,$15
        .byte   $15,$15,$15,$15,$15,$50,$2D,$2D
        .byte   $15,$2D,$15,$15,$15,$50,$4B,$BC
        .byte   $BD,$A9,$4C,$4C,$BE,$50,$40,$BF
        .byte   $BF,$BF,$BF,$BF,$C0,$50,$31,$A2
        .byte   $95,$87,$87,$A3,$AF,$2C,$31,$C1
        .byte   $98,$99,$A4,$C2,$C3,$2C,$31,$86
        .byte   $C4,$86,$8B,$A0,$8A,$2C,$31,$C1
        .byte   $98,$99,$99,$A5,$C3,$2C,$31,$8A
        .byte   $9A,$86,$8A,$A0,$8A,$2C,$31,$86
        .byte   $C5,$3B,$1E,$C6,$8A,$2C,$17,$16
        .byte   $17,$17,$17,$17,$16,$17,$17,$17
        .byte   $17,$C7,$C7,$17,$17,$17,$40,$40
        .byte   $6E,$48,$48,$50,$40,$40,$40,$40
        .byte   $6E,$48,$48,$50,$40,$40,$40,$40
        .byte   $6E,$48,$49,$50,$40,$40,$40,$C8
        .byte   $C9,$48,$48,$CA,$CB,$40,$6E,$CC
        .byte   $CD,$48,$48,$CE,$CF,$D0,$6E,$49
        .byte   $48,$48,$48,$48,$D1,$2C,$6E,$D2
        .byte   $D3,$4C,$4C,$4C,$4C,$40,$6E,$6A
        .byte   $6D,$40,$40,$40,$40,$40,$31,$6F
        .byte   $82,$2E,$2E,$2E,$2E,$17,$31,$70
        .byte   $D4,$87,$87,$87,$87,$2C,$31,$6F
        .byte   $87,$86,$86,$8B,$86,$2C,$31,$86
        .byte   $86,$8B,$86,$8A,$86,$30,$31,$86
        .byte   $8B,$86,$86,$86,$86,$D5,$31,$86
        .byte   $86,$86,$8B,$86,$8A,$D6,$17,$16
        .byte   $16,$16,$16,$16,$16,$16,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$2E,$2E
        .byte   $2E,$2E,$2E,$2E,$2E,$2E,$D7,$87
        .byte   $87,$87,$87,$A2,$87,$D5,$D8,$8A
        .byte   $86,$8B,$86,$86,$8A,$D6,$16,$16
        .byte   $16,$16,$16,$16,$16,$16,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$D9,$C7
        .byte   $C7,$C7,$C7,$C7,$C7,$DA,$DB,$86
        .byte   $86,$8A,$86,$8B,$86,$DC,$DB,$8A
        .byte   $86,$86,$86,$86,$86,$DC,$DD,$86
        .byte   $86,$8B,$86,$86,$8A,$DC,$DE,$86
        .byte   $86,$86,$86,$86,$86,$DC,$D8,$8B
        .byte   $86,$86,$86,$86,$8B,$DC,$16,$16
        .byte   $16,$16,$16,$16,$16,$DF,$17,$17
        .byte   $17,$17,$17,$17,$17,$17,$E0,$04
        .byte   $1A,$1B,$00,$01,$02,$E1,$E0,$08
        .byte   $09,$04,$04,$06,$07,$E2,$E0,$0A
        .byte   $0B,$0C,$0D,$0E,$04,$E2,$E3,$0F
        .byte   $10,$11,$12,$13,$14,$E4,$E5,$15
        .byte   $15,$15,$15,$15,$15,$E6,$E5,$15
        .byte   $4B,$4C,$4C,$4D,$15,$E6,$4C,$4C
        .byte   $40,$40,$40,$40,$4C,$4C,$40,$40
        .byte   $40,$40,$40,$40,$40,$40,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$31,$72
        .byte   $17,$2E,$2E,$2E,$2E,$17,$31,$B2
        .byte   $B3,$36,$36,$36,$36,$2C,$31,$6A
        .byte   $B4,$16,$16,$B5,$3C,$2C,$31,$6A
        .byte   $30,$2E,$2E,$B6,$60,$2C,$31,$48
        .byte   $36,$36,$36,$B7,$60,$2C,$B8,$B9
        .byte   $B9,$BA,$BB,$B7,$60,$2C,$17,$17
        .byte   $17,$17,$60,$B7,$60,$2C,$17,$17
        .byte   $17,$17,$60,$B7,$60,$2C,$31,$A2
        .byte   $95,$87,$87,$A3,$AF,$2C,$31,$C1
        .byte   $98,$99,$A4,$C2,$C3,$2C,$31,$86
        .byte   $C4,$86,$8B,$A0,$8A,$2C,$31,$C1
        .byte   $98,$99,$99,$A5,$C3,$2C,$31,$8A
        .byte   $9A,$86,$8A,$A0,$8A,$2C,$31,$86
        .byte   $C5,$3B,$1E,$C6,$8A,$2C,$17,$16
        .byte   $17,$17,$17,$17,$16,$17,$17,$17
        .byte   $17,$C7,$C7,$17,$17,$17,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$15,$15
        .byte   $15,$15,$15,$15,$15,$15,$50,$51
        .byte   $58,$59,$51,$51,$5A,$51,$51,$51
        .byte   $5B,$5C,$57,$47,$5D,$08,$08,$08
        .byte   $08,$08,$58,$59,$08,$08,$5E,$60
        .byte   $08,$08,$61,$65,$08,$08,$08,$08
        .byte   $5E,$60,$08,$08,$61,$65,$08,$08
        .byte   $08,$4D,$08,$08,$4E,$4F,$08,$08
        .byte   $4A,$4B,$08,$08,$4C,$08,$08,$08
        .byte   $5E,$5F,$4D,$55,$55,$00,$56,$45
        .byte   $00,$00,$45,$53,$00,$00,$54,$4D
        .byte   $00,$55,$4E,$4D,$56,$55,$4F,$4A
        .byte   $45,$52,$00,$00,$00,$00,$01,$02
        .byte   $0E,$0F,$06,$07,$0E,$0F,$51,$5B
        .byte   $5D,$08,$5C,$5D,$08,$08,$08,$58
        .byte   $08,$08,$59,$50,$08,$08,$08,$38
        .byte   $08,$08,$00,$16,$22,$0F,$01,$15
        .byte   $0E,$0F,$00,$00,$21,$00,$06,$02
        .byte   $0E,$0F,$00,$00,$00,$20,$00,$00
        .byte   $03,$04,$00,$16,$03,$0F,$01,$15
        .byte   $0E,$19,$14,$07,$18,$0F,$17,$00
        .byte   $0E,$23,$08,$12,$4A,$2A,$10,$07
        .byte   $1C,$0F,$00,$12,$00,$2A,$00,$00
        .byte   $22,$04,$14,$15,$0E,$19,$10,$07
        .byte   $18,$0F,$00,$00,$3A,$3B,$06,$07
        .byte   $09,$0A,$13,$0D,$1F,$0D,$10,$07
        .byte   $1C,$0A,$06,$11,$0E,$19,$2E,$46
        .byte   $3E,$6C,$6D,$6E,$00,$00,$6D,$0D
        .byte   $00,$00,$2E,$46,$6F,$24,$2E,$46
        .byte   $3E,$24,$6C,$00,$00,$00,$00,$6F
        .byte   $00,$00,$24,$3E,$6F,$24,$3A,$3B
        .byte   $03,$04,$14,$02,$0E,$0F,$17,$05
        .byte   $1B,$0D,$2A,$0C,$00,$00,$0B,$0C
        .byte   $00,$00,$0B,$2B,$00,$00,$0E,$0F
        .byte   $06,$07,$1B,$0D,$13,$0D,$18,$0F
        .byte   $0B,$0C,$0E,$0F,$0B,$0C,$0E,$0F
        .byte   $0B,$63,$0E,$0F,$0B,$07,$2E,$12
        .byte   $3E,$1A,$24,$3E,$25,$24,$24,$3E
        .byte   $3E,$24,$24,$25,$25,$24,$24,$12
        .byte   $3E,$1A,$22,$04,$10,$07,$03,$04
        .byte   $06,$07,$03,$23,$06,$11,$24,$12
        .byte   $25,$1A,$1B,$3E,$13,$24,$18,$0F
        .byte   $10,$07,$0E,$0F,$06,$0C,$0E,$19
        .byte   $0B,$2B,$1B,$24,$13,$3E,$21,$46
        .byte   $13,$24,$0E,$04,$06,$07,$1B,$05
        .byte   $13,$0D,$03,$0F,$06,$07,$06,$07
        .byte   $0E,$0A,$06,$07,$09,$0F,$13,$46
        .byte   $1B,$24,$46,$12,$3E,$1E,$13,$24
        .byte   $1B,$05,$24,$25,$22,$04,$24,$3E
        .byte   $03,$23,$24,$46,$3E,$24,$13,$0D
        .byte   $1B,$0D,$17,$3E,$0E,$23,$06,$07
        .byte   $09,$1D,$0B,$0C,$46,$2E,$06,$07
        .byte   $1C,$0A,$1E,$1F,$46,$2E,$24,$3E
        .byte   $22,$04,$3E,$16,$03,$0F,$0D,$25
        .byte   $0D,$24,$06,$11,$1C,$1D,$0D,$3E
        .byte   $0D,$24,$1E,$0C,$46,$2E,$24,$25
        .byte   $3E,$24,$30,$0F,$30,$07,$0E,$19
        .byte   $06,$11,$0D,$24,$0D,$25,$0D,$24
        .byte   $0D,$3E,$30,$0F,$31,$0C,$0D,$12
        .byte   $0D,$1A,$0D,$12,$0D,$1E,$0D,$46
        .byte   $0D,$24,$32,$46,$32,$24,$2E,$32
        .byte   $3E,$32,$32,$3E,$33,$24,$24,$32
        .byte   $3E,$33,$35,$36,$3E,$24,$3F,$37
        .byte   $34,$24,$35,$35,$3E,$24,$36,$3F
        .byte   $3E,$34,$37,$35,$25,$24,$32,$3E
        .byte   $32,$24,$24,$32,$3E,$32,$2C,$02
        .byte   $30,$0F,$05,$25,$0D,$24,$30,$07
        .byte   $30,$0F,$0E,$07,$06,$0F,$0E,$30
        .byte   $0B,$30,$30,$24,$30,$25,$3E,$24
        .byte   $24,$3E,$46,$2E,$24,$3E,$46,$30
        .byte   $24,$30,$30,$24,$30,$3E,$3E,$24
        .byte   $24,$25,$25,$24,$24,$25,$3E,$30
        .byte   $24,$30,$05,$20,$0D,$12,$03,$15
        .byte   $06,$11,$0D,$1A,$0D,$12,$25,$30
        .byte   $24,$30,$3E,$31,$24,$46,$31,$24
        .byte   $2E,$25,$3E,$20,$24,$12,$18,$0F
        .byte   $1E,$0C,$26,$2E,$32,$25,$46,$2A
        .byte   $24,$46,$0E,$0F,$10,$07,$33,$24
        .byte   $3F,$37,$3E,$24,$35,$35,$34,$24
        .byte   $32,$3E,$32,$24,$3F,$37,$46,$2E
        .byte   $35,$35,$46,$39,$36,$3F,$46,$2E
        .byte   $37,$35,$21,$24,$06,$15,$3E,$34
        .byte   $24,$32,$26,$2E,$32,$3E,$46,$2E
        .byte   $24,$25,$46,$26,$24,$32,$25,$24
        .byte   $35,$35,$3E,$33,$36,$3F,$25,$24
        .byte   $24,$3E,$46,$2E,$35,$36,$3E,$24
        .byte   $14,$02,$22,$04,$06,$07,$1C,$0A
        .byte   $2E,$46,$09,$0A,$2E,$46,$09,$0A
        .byte   $2E,$26,$1B,$0D,$1D,$0D,$18,$30
        .byte   $10,$30,$46,$24,$24,$3E,$0E,$30
        .byte   $06,$30,$09,$31,$2E,$46,$0D,$28
        .byte   $0D,$2E,$0B,$2B,$46,$2E,$14,$02
        .byte   $18,$0F,$01,$2C,$0E,$30,$06,$30
        .byte   $0E,$30,$10,$30,$18,$30,$06,$11
        .byte   $0E,$0F,$24,$3E,$03,$04,$24,$25
        .byte   $03,$04,$24,$3E,$21,$05,$03,$23
        .byte   $06,$07,$3A,$3B,$01,$02,$21,$05
        .byte   $13,$0D,$09,$0A,$46,$2E,$2B,$0D
        .byte   $46,$24,$3E,$24,$35,$36,$24,$33
        .byte   $36,$3F,$3E,$24,$37,$35,$34,$24
        .byte   $32,$25,$32,$24,$22,$04,$25,$32
        .byte   $03,$23,$0B,$0C,$2E,$46,$0E,$0F
        .byte   $0A,$07,$0E,$1D,$29,$2E,$18,$0A
        .byte   $2A,$2E,$09,$0F,$46,$07,$2E,$62
        .byte   $3E,$2E,$2E,$3E,$3E,$24,$2E,$25
        .byte   $3E,$24,$24,$1E,$3E,$2E,$06,$07
        .byte   $18,$0F,$24,$24,$3E,$24,$05,$3E
        .byte   $0D,$24,$2C,$20,$30,$07,$30,$11
        .byte   $31,$62,$46,$2F,$24,$2F,$3E,$2F
        .byte   $24,$2F,$68,$2E,$69,$25,$6A,$24
        .byte   $6B,$3E,$06,$0C,$1B,$46,$0B,$07
        .byte   $2E,$1A,$13,$24,$1B,$3E,$3E,$12
        .byte   $24,$1A,$13,$24,$1F,$3E,$68,$24
        .byte   $69,$3E,$01,$07,$0E,$0F,$30,$08
        .byte   $30,$08,$57,$30,$5D,$30,$08,$30
        .byte   $08,$30,$30,$4A,$30,$52,$4D,$30
        .byte   $55,$30,$30,$00,$30,$00,$00,$30
        .byte   $00,$30,$30,$30,$30,$30,$31,$31
        .byte   $2E,$46,$08,$08,$48,$49,$30,$08
        .byte   $30,$49,$08,$08,$48,$40,$08,$08
        .byte   $41,$49,$08,$30,$48,$30,$32,$24
        .byte   $32,$25,$32,$24,$16,$17,$25,$32
        .byte   $16,$17,$30,$11,$30,$62,$30,$2E
        .byte   $30,$3E,$30,$24,$30,$2C,$06,$07
        .byte   $0E,$0F,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $03,$05,$07,$3A,$10,$12,$3C,$30
        .byte   $32,$10,$12,$3A,$30,$32,$24,$12
        .byte   $26,$10,$0D,$03,$1A,$01,$44,$32
        .byte   $46,$30,$44,$32,$46,$30,$1E,$05
        .byte   $29,$07,$50,$52,$68,$BA,$26,$10
        .byte   $24,$12,$2E,$12,$64,$6A,$38,$38
        .byte   $4A,$4A,$5A,$4C,$4C,$4D,$91,$68
        .byte   $AC,$AE,$30,$32,$48,$4E,$3C,$74
        .byte   $00,$00,$3C,$A9,$6C,$93,$3C,$3C
        .byte   $3C,$3C,$3C,$7C,$7E,$9B,$70,$70
        .byte   $86,$88,$8A,$9C,$9E,$70,$3C,$75
        .byte   $70,$70,$70,$93,$3C,$A1,$96,$70
        .byte   $B0,$12,$44,$94,$93,$00,$64,$48
        .byte   $50,$52,$50,$64,$6C,$2D,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$02
        .byte   $04,$06,$08,$3B,$11,$13,$3C,$31
        .byte   $33,$11,$13,$3B,$31,$33,$11,$25
        .byte   $13,$27,$02,$0E,$04,$18,$31,$45
        .byte   $33,$47,$31,$45,$33,$47,$08,$1D
        .byte   $06,$2B,$51,$53,$69,$BB,$13,$27
        .byte   $11,$25,$2F,$13,$65,$6B,$39,$39
        .byte   $4B,$4B,$5B,$4C,$4D,$4C,$91,$69
        .byte   $AD,$AF,$31,$33,$49,$4F,$71,$3C
        .byte   $00,$00,$3C,$AA,$6D,$3C,$3C,$3C
        .byte   $3C,$3C,$3C,$7D,$7F,$3C,$70,$70
        .byte   $87,$89,$8B,$9D,$9F,$92,$90,$70
        .byte   $70,$70,$92,$91,$97,$94,$70,$95
        .byte   $27,$13,$13,$3C,$3C,$00,$65,$49
        .byte   $51,$53,$A7,$65,$6D,$49,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$20
        .byte   $22,$40,$42,$3A,$20,$22,$3C,$09
        .byte   $0B,$14,$16,$3A,$40,$42,$34,$22
        .byte   $36,$20,$34,$22,$36,$20,$54,$42
        .byte   $56,$40,$0F,$0B,$19,$09,$56,$40
        .byte   $54,$42,$60,$62,$4A,$5C,$1C,$14
        .byte   $2A,$16,$3E,$22,$60,$6A,$3E,$6E
        .byte   $4A,$5A,$4A,$5C,$5C,$5D,$3C,$5A
        .byte   $BC,$BE,$40,$67,$58,$5E,$80,$84
        .byte   $00,$00,$99,$00,$62,$94,$72,$82
        .byte   $76,$78,$7A,$8C,$8E,$AB,$75,$70
        .byte   $A8,$00,$98,$A8,$A8,$70,$3C,$3C
        .byte   $75,$70,$93,$3C,$3C,$3C,$91,$91
        .byte   $B1,$B2,$54,$3C,$3C,$00,$60,$58
        .byte   $60,$62,$A3,$B6,$B8,$00,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$21
        .byte   $23,$41,$43,$3B,$21,$23,$3C,$0A
        .byte   $0C,$15,$17,$3B,$41,$43,$21,$35
        .byte   $23,$37,$21,$35,$23,$37,$41,$55
        .byte   $43,$57,$0A,$1F,$0C,$28,$43,$57
        .byte   $41,$55,$61,$63,$4B,$5C,$17,$1B
        .byte   $15,$2C,$3F,$A0,$61,$6B,$3F,$6F
        .byte   $4B,$5B,$4B,$5C,$5D,$5C,$3C,$5B
        .byte   $BD,$BF,$66,$43,$59,$5F,$81,$85
        .byte   $00,$00,$9A,$98,$63,$3C,$73,$83
        .byte   $77,$79,$7B,$8D,$8F,$3C,$70,$70
        .byte   $00,$00,$00,$98,$87,$95,$3C,$90
        .byte   $70,$92,$3C,$3C,$3C,$3C,$91,$91
        .byte   $B3,$A0,$A0,$3C,$3C,$00,$61,$59
        .byte   $61,$63,$A6,$B7,$B9,$3D,$00,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$10
        .byte   $10,$10,$10,$40,$10,$10,$03,$10
        .byte   $10,$10,$10,$20,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$01,$01,$01,$01,$10,$10
        .byte   $10,$10,$12,$10,$01,$10,$12,$12
        .byte   $01,$01,$01,$01,$01,$01,$03,$01
        .byte   $12,$12,$10,$10,$01,$01,$03,$03
        .byte   $10,$10,$03,$03,$01,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $10,$10,$10,$03,$03,$10,$11,$11
        .byte   $11,$11,$01,$01,$01,$01,$00,$00
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
        .byte   $00,$C0,$00,$FE,$43,$C1
