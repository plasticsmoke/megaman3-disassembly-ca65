; =============================================================================
; MEGA MAN 3 (U) — BANK $0F — WILY FORTRESS 4 CREDITS + STAGE DATA
; =============================================================================
; Mapped to $A000-$BFFF via MMC3.
;
; This bank is dual-purpose:
;   1. Wily Fortress 4 ending credits sequence ($A000-$A629)
;      - Initialization + main loop code ($A000-$A201)
;      - OAM star sprite data, character walk-on tables ($A202-$A24D)
;      - Credits text pointers + encoded staff roll data ($A24E-$A629)
;   2. Wily Fortress 4 stage data ($A62A-$BFFF)
;      - Compressed nametable tile maps, palette data
;      - Enemy/object spawn lists (screen#, X, Y, entity ID)
;      - Metatile definitions (columns, CHR, attributes, collision)
;
; Despite the file name referencing entity spawning, this bank contains
; no entity spawn dispatch code. The stage_id mapping $22 -> bank $0F
; causes this bank to be loaded for Wily 4 stage data.
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"

; --- fixed bank subroutines ---
apply_y_speed           := $F797        ; apply_y_speed — apply Y velocity to entity
process_frame_yield           := $FD80  ; process_frame_yield — render frame + yield to NMI

; =============================================================================
; WILY 4 CREDITS — INITIALIZATION
; =============================================================================
; Entry point for the Wily Fortress 4 ending credits sequence.
; Clears frame counter ($95), NMI flag, entity slots 0-1, and timers.
;
; NOTE: da65 mis-disassembled the STA $A006 as separate bytes because $A006
; is an in-bank address. The original code is a chain of STA instructions
; zeroing out variables. The bytes assemble correctly regardless.
; =============================================================================
.segment "BANK0F"

        lda     #$00                    ; clear all credits state
        sta     $95
        sta     $A006
        sta     nmi_skip
        sta     ent_timer
        sta     ent_var1
        sta     ent_var2
        sta     ent_var3
        sta     ent_status
        sta     $0310
; ===========================================================================
; CREDITS MAIN LOOP — NAMETABLE TEXT SCROLL
; ===========================================================================
; Scrolls the credits text upward one pixel at a time. Every 8 pixels,
; writes a new row of nametable tiles from the credits text data.
; Uses $B8 as the text data index, scroll_y as the Y scroll position.
; The text data pointers are stored in credits_text_pointer_low_bytes (low) / credits_text_pointer_high_bytes (high).
; ===========================================================================
code_A01B:  lda     ent_status          ; check if credits entity active
        bmi     code_A077               ; if active (bit 7 set), skip scroll
        lda     $95                     ; frame counter
        and     #$01                    ; only process on even frames
        bne     code_A077
        lda     scroll_y                ; get Y scroll position
        and     #$07                    ; check if on 8-pixel boundary
        bne     code_A077               ; not aligned, skip nametable update
        lda     scroll_y                ; convert scroll to tile row
        lsr     a                       ; divide by 8
        lsr     a                       ; (continued shift)
        lsr     a                       ; (continued shift)
        sta     $02                     ; $02 = current tile row
        ldx     $B8                     ; text data index
        lda     credits_text_pointer_low_bytes,x ; load text data pointer (low byte)
        sta     $00                     ; store pointer low byte
        lda     credits_text_pointer_high_bytes,x ; load text data pointer (high byte)
        sta     $01                     ; store pointer high byte
        ldy     #$00                    ; Y = 0 for indirect read
        sty     $03                     ; $03/$04 = PPU nametable address
        lda     scroll_y                ; compute PPU row address
        and     #$F8                    ; align to 8-pixel row
        asl     a                       ; multiply by 4 to get nametable offset
        rol     $03                     ; (each row = 32 tiles)
        asl     a                       ; continue multiply by 4
        rol     $03                     ; carry into high byte
        sta     $04                     ; store PPU addr low byte
        cpx     #$3B                    ; past end of text data?
        bcs     code_A059               ; yes: fill with blank tiles
        lda     ($00),y                 ; read expected row from text data
        cmp     $02                     ; matches current scroll row?
        beq     code_A07A               ; yes: copy text row to nametable buffer
; --- fill nametable row with blank tiles ($24 = space) ---
code_A059:  lda     $03                 ; load PPU addr high byte
        ora     #$24                    ; set nametable base ($24xx)
        sta     $0780                   ; PPU address high byte
        lda     $04                     ; load PPU addr low byte
        sta     $0781                   ; PPU address low byte
        ldy     #$1F                    ; 32 tiles per row
        sty     $0782                   ; tile count
        lda     #$24                    ; blank tile
code_A06C:  sta     $0783,y             ; fill row buffer with blanks
        dey                             ; next tile slot
        bpl     code_A06C
        sty     $07A3                   ; terminator
        sty     nametable_dirty         ; signal NMI to upload
code_A077:  jmp     code_A0C0           ; jump to scroll check

; --- copy text row from credits data to nametable buffer ---
; Format: [row#] [left_pad] [text_len] [tile, tile, ...] (padded with blanks)
code_A07A:  iny                         ; skip row number byte
        lda     ($00),y                 ; read left padding (spaces before text)
        sta     $05                     ; store left padding count
        ldx     #$00                    ; X = buffer write index
        lda     #$24                    ; blank tile
code_A083:  sta     $0783,x             ; fill left padding
        inx                             ; advance buffer index
        dec     $05                     ; decrement pad counter
        bpl     code_A083               ; loop until padding done
        lda     $04                     ; load PPU addr low
        sta     $0781                   ; PPU address low byte
        lda     $03                     ; load PPU addr high
        ora     #$24                    ; nametable base
        sta     $0780                   ; PPU address high byte
        iny                             ; advance to text length
        lda     #$1F                    ; 32 tiles total
        sta     $0782                   ; tile count
        lda     ($00),y                 ; read text length
        sta     $02                     ; store text length
        iny                             ; advance to first tile
code_A0A2:  lda     ($00),y             ; copy text tiles to buffer
        sta     $0783,x                 ; write tile to buffer
        iny                             ; next source byte
        inx                             ; next buffer position
        dec     $02                     ; decrement text counter
        bpl     code_A0A2               ; loop until text copied
        lda     #$24                    ; pad remainder with blanks
code_A0AF:  sta     $0783,x             ; fill remaining with blanks
        inx                             ; next buffer position
        cpx     #$20                    ; filled all 32 tiles?
        bne     code_A0AF
        lda     #$FF                    ; terminator value
        sta     $07A3                   ; terminator
        sta     nametable_dirty         ; signal NMI to upload
        inc     $B8                     ; advance to next text data entry
; --- check if credits text finished scrolling ---
code_A0C0:  lda     scroll_y            ; check scroll position
        bne     code_A0D4               ; still scrolling
        lda     $B8                     ; check text data index
        cmp     #$3B                    ; all 59 text entries displayed?
        bne     code_A0D4               ; not all shown yet
        lda     ent_var2                ; screen-wrap counter
        cmp     #$02                    ; wrapped twice (full scroll)?
        bne     code_A0D4               ; not done wrapping
        jmp     code_A137               ; start "PRESENTED BY CAPCOM" animation

; --- advance Y scroll by 1 pixel (every other frame) ---
code_A0D4:  lda     $95
        and     #$01                    ; even frames only
        bne     code_A0EF               ; skip on odd frames
        inc     scroll_y                ; scroll Y down 1 pixel
        lda     scroll_y                ; read new scroll value
        cmp     #$F0                    ; past 240 pixels? (nametable wrap)
        bne     code_A0EF               ; no wrap needed
        lda     #$00                    ; reset scroll to top
        sta     scroll_y                ; wrap back to 0
        lda     $B8                     ; check text data index
        cmp     #$3B                    ; past end of text data?
        bne     code_A0EF               ; not finished yet
        inc     ent_var2                ; count nametable wraps
; --- render star sprites for credits background ---
; Copies 12 OAM entries from credits_star_sprites_oam_y (6 per entity slot), with Y offset from ent_timer.
; Two sets of 6 sprites: first set uses slot 0 timer, second uses slot $20 (1).
code_A0EF:  ldy     #$00                ; OAM buffer index
        ldx     #$00                    ; entity slot offset
code_A0F3:  lda     credits_star_sprites_oam_y,y ; sprite Y position (base)
        clc                             ; prepare for add
        adc     ent_timer,x             ; add vertical scroll offset
        sta     $0200,y                 ; write to OAM Y
        lda     credits_star_sprites_oam_tile,y ; tile ID
        sta     $0201,y                 ; write to OAM tile
        lda     credits_star_sprites_oam_attr,y ; sprite attributes
        sta     $0202                   ; write to OAM attr
        lda     credits_star_sprites_oam_x,y ; sprite X position
        sta     $0203,y                 ; write to OAM X
        iny                             ; skip Y byte
        iny                             ; skip tile byte
        iny                             ; skip attr byte
        iny                             ; advance to next OAM entry (4 bytes each)
        cpy     #$18                    ; first 6 sprites done?
        bcc     code_A0F3               ; loop for first 6 sprites
        ldx     #$20                    ; switch to entity slot 1 offset
        cpy     #$30                    ; all 12 sprites done?
        bcc     code_A0F3               ; loop for next 6 sprites
        sty     oam_ptr                 ; mark end of OAM data
        lda     ent_timer               ; advance slot 0 Y offset
        clc                             ; prepare for add
        adc     #$02                    ; move stars downward 2px/frame
        sta     ent_timer
        lda     ent_var1                ; advance slot 1 Y offset
        clc                             ; prepare for add
        adc     #$03                    ; move stars downward 3px/frame
        sta     ent_var1
        jsr     process_frame_yield     ; yield frame (process_frame_yield)
        jmp     code_A01B               ; loop back to credits main

; ===========================================================================
; "PRESENTED BY CAPCOM" — MEGA MAN WALK-ON ANIMATION
; ===========================================================================
; After credits text finishes scrolling, spawns Mega Man (and Proto Man?)
; entities that walk across the screen. Two nametable rows are cleared
; to make space, then characters scroll in from the right.
; ===========================================================================
code_A137:  lda     ent_status          ; check slot 0 status
        bmi     code_A181               ; already initialized, skip setup
        ldx     #$01                    ; set up entity slots 0 and 1
code_A13E:  lda     #$80                ; active flag value
        sta     ent_status,x            ; mark entity active
        sta     ent_flags,x             ; set active flag
        lda     credits_character_anim_ids,x ; animation ID from table
        sta     ent_anim_id,x
        lda     credits_character_y_positions,x ; initial Y position from table
        sta     ent_y_px,x
        lda     #$F8                    ; start off-screen right
        sta     ent_x_px,x
        lda     #$00                    ; zero value
        sta     ent_anim_frame,x        ; reset animation
        sta     ent_anim_state,x
        dex                             ; next slot
        bpl     code_A13E               ; loop for both slots
        lda     #$25                    ; set up two nametable row clears
        sta     $0780                   ; PPU addr high (row 1)
        lda     #$D6                    ; row 1 PPU addr low = $D6
        sta     $0781                   ; PPU addr low (row 1)
        lda     #$26                    ; row 2 PPU addr high = $26
        sta     $0784                   ; PPU addr high (row 2)
        lda     #$16                    ; row 2 PPU addr low = $16
        sta     $0785                   ; PPU addr low (row 2)
        lda     #$00                    ; zero for clear
        sta     $0782                   ; tile count = 0 (clear)
        sta     $0786
        sta     ent_timer               ; reset animation timer
; --- character walk + text reveal animation ---
code_A181:  dec     $0361               ; decrement slot 1 X timer
        bne     code_A18B               ; skip if timer not zero
        lda     #$00                    ; zero = inactive
        sta     $0301                   ; deactivate slot 1
code_A18B:  lda     ent_anim_id         ; slot 0 animation state
        cmp     #$07                    ; falling animation?
        beq     code_A1E8               ; yes: handle falling
        dec     ent_x_px                ; move character left 1px
        lda     ent_x_px                ; read current X position
        and     #$07                    ; on 8-pixel boundary?
        bne     code_A1CA               ; no: skip text reveal
        lda     ent_x_px                ; check X position range
        cmp     #$B1                    ; past right boundary?
        bcs     code_A1CA               ; yes: skip text reveal
        cmp     #$50                    ; past left boundary?
        bcc     code_A1CA               ; yes: skip text reveal
; --- reveal "CAPCOM" letters one at a time ---
        dec     $0781                   ; shift nametable column left
        dec     $0785                   ; shift bottom row column left
        ldx     ent_var3                ; current letter index
        cpx     #$0C                    ; all 12 chars placed?
        beq     code_A1CA               ; yes: skip letter placement
        lda     credits_presented_by_capcom_top,x ; top row tile for this letter
        sta     $0783                   ; write to top row buffer
        lda     credits_presented_by_capcom_bottom,x ; bottom row tile for this letter
        sta     $0787                   ; write to bottom row buffer
        lda     #$FF                    ; terminator / dirty flag
        sta     $0788                   ; terminator
        sta     nametable_dirty         ; trigger NMI upload
        inc     ent_var3                ; advance to next letter
; --- trigger fall when character reaches X=$20 ---
code_A1CA:  lda     ent_x_px            ; check current X position
        cmp     #$20                    ; reached left target position?
        bne     code_A1FF               ; not at target, skip fall
        lda     #$44                    ; set Y velocity sub-pixel
        sta     ent_yvel_sub
        lda     #$03                    ; set Y velocity (falling speed)
        sta     ent_yvel
        lda     #$07                    ; switch to falling animation
        sta     ent_anim_id
        lda     #$00                    ; zero for reset
        sta     ent_anim_state          ; reset animation state
        sta     ent_anim_frame          ; reset animation frame
; --- handle falling animation ---
code_A1E8:  lda     #$7C                ; ground level Y position
        cmp     ent_y_px                ; reached ground?
        bcs     code_A1F7               ; not yet: keep falling
        sta     ent_y_px                ; clamp to ground
        inc     ent_y_px                ; nudge below ground (trigger landing)
        bcc     code_A1FF               ; continue to sprite render
code_A1F7:  ldx     #$00                ; entity slot 0
        jsr     apply_y_speed           ; apply_y_speed (gravity)
        inc     ent_x_px                ; drift right slightly while falling
code_A1FF:  jmp     code_A0EF           ; back to star sprite render loop

; ===========================================================================
; CREDITS STAR SPRITE DATA
; ===========================================================================
; OAM entries for 12 star sprites (two groups of 6).
; Format: Y-pos, tile, attr, X-pos (4 bytes per sprite).
; ===========================================================================
credits_star_sprites_oam_y:  .byte   $28
credits_star_sprites_oam_tile:  .byte   $F1
credits_star_sprites_oam_attr:  .byte   $02
credits_star_sprites_oam_x:  .byte   $28,$90,$F1,$02,$50,$D0,$F1,$02
        .byte   $68,$60,$F1,$02,$90,$B0,$F1,$02
        .byte   $C0,$70,$F1,$02,$E0,$68,$F2,$02
        .byte   $18,$E0,$F2,$02,$40,$40,$F2,$02
        .byte   $68,$80,$F2,$02,$A0,$20,$F2,$02
        .byte   $D0,$D0,$F2,$02,$F0
; --- entity animation IDs for credits characters ---
credits_character_anim_ids:  .byte   $01,$D7 ; anim IDs for slots 0, 1
; --- initial Y positions for credits characters ---
credits_character_y_positions:  .byte   $74,$82 ; Y positions for slots 0, 1
; --- "PRESENTED BY CAPCOM" letter tiles (top row) ---
credits_presented_by_capcom_top:  .byte   $22,$0B,$24,$0D,$0E,$1D,$17,$0E
        .byte   $1C,$0E,$1B,$19
; --- "PRESENTED BY CAPCOM" letter tiles (bottom row) ---
credits_presented_by_capcom_bottom:  .byte   $24,$24,$24,$16,$18,$0C,$19,$0A
        .byte   $0C,$24,$24,$24
; ===========================================================================
; CREDITS TEXT DATA POINTERS
; ===========================================================================
; 59 entries ($3B) indexing into the credits text data.
; credits_text_pointer_low_bytes = low bytes, credits_text_pointer_high_bytes = high bytes of text data pointers.
; Each entry points to a row: [expected_row, left_pad, text_len, tiles...]
; ===========================================================================
credits_text_pointer_low_bytes:  .byte   $C4,$D9,$E4,$F0,$FC,$03,$0D,$1D
        .byte   $26,$33,$39,$3F,$4B,$55,$60,$6F
        .byte   $7D,$86,$91,$A2,$B5,$C7,$D5,$E7
        .byte   $F9,$0E,$21,$32,$47,$5A,$6B,$7D
        .byte   $8E,$A1,$B5,$C9,$D9,$E9,$F8,$08
        .byte   $17,$29,$37,$44,$54,$68,$78,$89
        .byte   $A0,$B5,$C7,$D6,$E3,$EC,$F7,$02
        .byte   $0D,$16,$22
; --- text data pointer high bytes ---
credits_text_pointer_high_bytes:  .byte   $A2,$A2,$A2,$A2,$A2,$A3,$A3,$A3
        .byte   $A3,$A3,$A3,$A3,$A3,$A3,$A3,$A3
        .byte   $A3,$A3,$A3,$A3,$A3,$A3,$A3,$A3
        .byte   $A3,$A4,$A4,$A4,$A4,$A4,$A4,$A4
        .byte   $A4,$A4,$A4,$A4,$A4,$A4,$A4,$A5
        .byte   $A5,$A5,$A5,$A5,$A5,$A5,$A5,$A5
        .byte   $A5,$A5,$A5,$A5,$A5,$A5,$A5,$A6
; ===========================================================================
; CREDITS TEXT DATA — STAFF ROLL
; ===========================================================================
; Encoded staff names and role titles for the credits scroll.
; Tile values map to the credits CHR font: $00=A, $01=B, ..., $24=space.
; Each row entry: [tile_row, left_padding, text_length, tile, tile, ...]
;
; Staff listed: Character Designer, Inafking, Object Designer, Tokimichi,
; Sound Composer, Programmer, Planner, and many more Capcom staff.
; ===========================================================================
        .byte   $A6,$A6,$A6,$00,$06,$11,$0C,$11
        .byte   $0A,$1B,$0A,$0C,$1D,$0E,$1B,$24
        .byte   $0D,$0E,$1C,$12,$10,$17,$0E,$1B
        .byte   $03,$0B,$07,$12,$17,$0A,$0F,$14
        .byte   $12,$17,$10,$05,$0B,$08,$22,$0A
        .byte   $1C,$1E,$14,$12,$0C,$11,$12,$07
        .byte   $0B,$08,$1D,$18,$14,$12,$16,$12
        .byte   $02,$09,$03,$09,$0D,$03,$23,$12
        .byte   $23,$12,$0B,$0C,$06,$0D,$18,$17
        .byte   $0C,$11,$0A,$17,$0E,$0A,$0C,$1C
        .byte   $18,$1E,$17,$0D,$24,$0C,$18,$16
        .byte   $19,$18,$1C,$0E,$11,$0C,$05,$0B
        .byte   $1E,$17,$0B,$1E,$17,$14,$0A,$09
        .byte   $19,$1B,$18,$10,$1B,$0A,$16,$16
        .byte   $0E,$1B,$17,$0D,$02,$1D,$26,$14
        .byte   $19,$0D,$02,$0A,$26,$16,$1B,$0A
        .byte   $08,$14,$0E,$1B,$18,$24,$14,$0E
        .byte   $1B,$18,$00,$0C,$06,$19,$15,$0A
        .byte   $17,$17,$0E,$1B,$03,$0B,$07,$19
        .byte   $0A,$1D,$0A,$1B,$12,$1B,$18,$06
        .byte   $09,$0B,$1C,$1E,$0B,$24,$19,$15
        .byte   $0A,$17,$17,$12,$17,$10,$09,$0A
        .byte   $0A,$11,$0A,$1D,$1E,$14,$18,$18
        .byte   $0C,$11,$0A,$17,$0B,$0C,$05,$0B
        .byte   $0A,$16,$0B,$18,$18,$0D,$0B,$07
        .byte   $12,$17,$0A,$0F,$14,$12,$17,$10
        .byte   $10,$08,$0D,$1C,$19,$0E,$0C,$12
        .byte   $0A,$15,$24,$1D,$11,$0A,$17,$14
        .byte   $1C,$13,$09,$0F,$16,$1E,$1D,$1C
        .byte   $1E,$18,$24,$1C,$11,$12,$16,$18
        .byte   $16,$1E,$1B,$0A,$15,$08,$0E,$1D
        .byte   $0A,$14,$0A,$1C,$11,$12,$24,$0F
        .byte   $1E,$13,$12,$18,$14,$0A,$17,$0B
        .byte   $0A,$0A,$14,$12,$18,$24,$22,$0A
        .byte   $0B,$1E,$14,$12,$19,$07,$0E,$1D
        .byte   $0A,$14,$0E,$11,$12,$1B,$18,$24
        .byte   $1C,$1E,$23,$1E,$14,$12,$1B,$07
        .byte   $0E,$1D,$18,$16,$18,$11,$12,$1B
        .byte   $18,$24,$14,$18,$16,$12,$17,$0E
        .byte   $1D,$06,$11,$22,$18,$1C,$11,$12
        .byte   $1D,$18,$16,$18,$24,$12,$16,$0A
        .byte   $12,$23,$1E,$16,$12,$01,$06,$0F
        .byte   $16,$0A,$1C,$0A,$22,$18,$1C,$11
        .byte   $12,$24,$0A,$17,$23,$0A,$14,$12
        .byte   $03,$08,$0D,$13,$1E,$17,$13,$12
        .byte   $1B,$18,$24,$14,$12,$16,$1E,$1B
        .byte   $0A,$05,$06,$11,$14,$0A,$1D,$1C
        .byte   $1E,$11,$12,$1B,$18,$24,$17,$0A
        .byte   $14,$0A,$16,$1E,$1B,$0A,$07,$08
        .byte   $0F,$0A,$1D,$1C,$1E,$1C,$11,$12
        .byte   $24,$20,$0A,$1D,$0A,$17,$0A,$0B
        .byte   $0E,$09,$08,$0D,$22,$1E,$11,$1C
        .byte   $1E,$14,$0E,$24,$16,$1E,$1B,$0A
        .byte   $1D,$0A,$0B,$08,$0E,$1D,$0A,$14
        .byte   $0A,$1C,$11,$12,$24,$1E,$16,$0E
        .byte   $23,$0A,$20,$0A,$0D,$07,$0D,$1D
        .byte   $18,$16,$18,$11,$12,$1B,$18,$24
        .byte   $1D,$0A,$14,$0E,$12,$0F,$07,$0F
        .byte   $1D,$18,$16,$18,$11,$12,$0D,$0E
        .byte   $24,$0A,$1C,$0A,$14,$1E,$1B,$0A
        .byte   $11,$08,$10,$1D,$0A,$14,$0A,$1C
        .byte   $11,$12,$24,$22,$18,$1C,$11,$12
        .byte   $16,$1E,$1B,$0A,$13,$06,$10,$22
        .byte   $18,$1C,$11,$12,$1D,$0A,$17,$0E
        .byte   $24,$18,$14,$12,$16,$18,$1D,$18
        .byte   $15,$09,$0C,$22,$0A,$1D,$0A,$14
        .byte   $0A,$24,$1C,$1E,$23,$1E,$14,$12
        .byte   $17,$08,$0C,$14,$0E,$12,$1C,$1E
        .byte   $14,$0E,$24,$11,$18,$14,$14,$1E
        .byte   $19,$0B,$0B,$1D,$0A,$14,$1E,$24
        .byte   $11,$12,$14,$12,$0C,$11,$12,$1B
        .byte   $08,$0B,$1D,$1C,$1E,$1D,$18,$16
        .byte   $1E,$24,$14,$18,$17,$0D,$18,$1D
        .byte   $09,$0B,$11,$12,$1B,$18,$14,$12
        .byte   $24,$18,$14,$0A,$0B,$0E,$01,$07
        .byte   $0E,$11,$12,$1C,$0A,$1D,$18,$16
        .byte   $18,$24,$1D,$0A,$17,$0A,$14,$0A
        .byte   $03,$0A,$0A,$0A,$14,$12,$1B,$0A
        .byte   $24,$18,$0B,$0A,$1D,$0A,$05,$0A
        .byte   $09,$0B,$1E,$17,$10,$18,$24,$12
        .byte   $20,$0A,$12,$07,$08,$0C,$1C,$11
        .byte   $12,$17,$19,$0E,$12,$24,$13,$18
        .byte   $11,$17,$18,$09,$07,$10,$14,$0A
        .byte   $23,$1E,$1D,$0A,$14,$0A,$24,$11
        .byte   $18,$1B,$12,$16,$18,$1D,$18,$0B
        .byte   $0A,$0C,$1D,$0E,$1D,$1C,$1E,$24
        .byte   $0A,$14,$12,$22,$0A,$16,$0A,$0D
        .byte   $08,$0D,$16,$0A,$1C,$0A,$1C,$11
        .byte   $12,$24,$11,$0A,$1B,$1E,$14,$12
        .byte   $0F,$05,$13,$1C,$11,$18,$1E,$12
        .byte   $0C,$11,$12,$1B,$18,$24,$22,$0A
        .byte   $16,$0A,$10,$1E,$0C,$11,$12,$11
        .byte   $08,$11,$14,$0E,$12,$1C,$1E,$14
        .byte   $0E,$24,$14,$0A,$16,$12,$18,$18
        .byte   $1C,$0A,$14,$18,$13,$08,$0E,$0F
        .byte   $1E,$16,$12,$1D,$18,$16,$18,$24
        .byte   $14,$0A,$10,$0A,$16,$12,$15,$09
        .byte   $0B,$16,$0A,$14,$18,$1D,$18,$24
        .byte   $12,$17,$18,$1E,$0E,$17,$0C,$09
        .byte   $16,$1B,$1C,$26,$1D,$0A,$1B,$1E
        .byte   $16,$12,$19,$0C,$05,$13,$22,$0A
        .byte   $10,$1E,$0A,$1B,$0B,$07,$1D,$0A
        .byte   $14,$0E,$19,$18,$17,$10,$1D,$0B
        .byte   $07,$0A,$1C,$11,$0E,$17,$0D,$0E
        .byte   $17,$01,$0B,$07,$14,$18,$0B,$0A
        .byte   $24,$20,$12,$17,$03,$0C,$05,$0A
        .byte   $1B,$12,$0B,$18,$17,$05,$0B,$08
        .byte   $22,$0A,$0C,$0C,$11,$0A,$17,$24
        .byte   $23,$07,$0D,$04,$12,$1B,$12,$14
; =============================================================================
; WILY FORTRESS 4 — STAGE DATA ($A62A-$BFFF)
; =============================================================================
; Standard-format stage data for Wily Fortress 4 (stage_id $22 -> bank $0F).
; Loaded by the fixed bank level layout engine.
;
; Data layout:
;   $A62A:  Compressed nametable tile maps (2-bit RLE)
;   $AA00:  Screen index table (14 screens, terminated by $FF)
;   $AA1B:  Room scroll/transition config data
;   $AA5A:  Room scroll direction / transition table
;   $AA60:  Room CHR/palette config
;   $AA82:  BG palette data (4 palettes x 4 bytes)
;   $AAA6:  Additional compressed nametable data + attribute tables
;   $AC00:  Enemy placement data — X pixel positions
;   $AD00:  Enemy placement data — Y pixel positions
;   $AE00:  Enemy placement data — global enemy IDs
;   $AF00:  Metatile column definitions (8 bytes per column)
;   $B700:  Metatile CHR definitions (4 bytes per metatile: 2x2 tile IDs)
;   $BB00:  Metatile CHR attribute / flip plane data
;   $BF00:  Collision attribute table (upper nybble = collision type)
; =============================================================================
; --- Compressed nametable tile maps ($A62A) ---
; 2-bit RLE encoded screen tile data, decompressed by the fixed bank
; nametable loader. Each screen is 960 bytes when decompressed (30x32 tiles).
        .byte   $18,$22,$15,$2F,$10,$4E,$75,$57
        .byte   $55,$AD,$55,$FD,$15,$FF,$51,$FF
        .byte   $F5,$FB,$55,$FF,$5D,$7F,$55,$70
        .byte   $50,$31,$40,$3E,$51,$03,$11,$7F
        .byte   $15,$0D,$54,$A6,$54,$FE,$55,$FF
        .byte   $5D,$FD,$F5,$F9,$75,$FF,$D7,$9B
        .byte   $7D,$FF,$DD,$FF,$57,$FF,$1F,$0C
        .byte   $00,$78,$51,$02,$00,$91,$11,$A1
        .byte   $40,$B6,$10,$84,$00,$2C,$00,$81
        .byte   $45,$32,$44,$5B,$54,$CA,$45,$BD
        .byte   $53,$BB,$45,$FF,$75,$BF,$5D,$27
        .byte   $41,$59,$04,$9B,$54,$F9,$51,$66
        .byte   $55,$DB,$55,$7F,$15,$7D,$71,$FD
        .byte   $D5,$FF,$73,$FF,$55,$FF,$1D,$BF
        .byte   $55,$FF,$FD,$FF,$F5,$FF,$F5,$68
        .byte   $41,$90,$44,$15,$04,$30,$14,$CD
        .byte   $00,$AB,$14,$19,$54,$12,$14,$AF
        .byte   $54,$EF,$41,$54,$5D,$65,$55,$DF
        .byte   $51,$FF,$13,$FF,$77,$FF,$5D,$99
        .byte   $10,$12,$95,$CE,$50,$C9,$44,$E1
        .byte   $45,$BB,$04,$72,$75,$83,$17,$DF
        .byte   $55,$7B,$14,$FF,$7D,$DD,$57,$FD
        .byte   $DD,$FF,$5F,$FF,$75,$FF,$15,$60
        .byte   $41,$17,$00,$E7,$44,$D0,$41,$01
        .byte   $54,$A0,$01,$E0,$00,$0E,$51,$02
        .byte   $01,$11,$40,$42,$41,$B4,$80,$B1
        .byte   $14,$90,$91,$29,$54,$F7,$53,$93
        .byte   $15,$FD,$4D,$DB,$16,$FE,$1C,$FA
        .byte   $45,$EF,$55,$EF,$5D,$F6,$55,$FF
        .byte   $77,$FF,$77,$FF,$F7,$FF,$E7,$FF
        .byte   $55,$FF,$FD,$FF,$D5,$FF,$FF,$08
        .byte   $01,$4C,$51,$5D,$11,$B4,$50,$4E
        .byte   $55,$4E,$51,$AE,$05,$4F,$11,$69
        .byte   $50,$FE,$14,$77,$FF,$FB,$5D,$FB
        .byte   $44,$FF,$D5,$FF,$57,$FF,$7D,$40
        .byte   $10,$09,$50,$51,$44,$07,$51,$38
        .byte   $51,$DB,$74,$6F,$41,$BD,$75,$FE
        .byte   $75,$FF,$DF,$FF,$3F,$7F,$54,$FF
        .byte   $77,$FF,$5D,$F7,$57,$FD,$75,$62
        .byte   $01,$49,$01,$9C,$14,$D0,$55,$28
        .byte   $11,$12,$14,$20,$41,$5A,$04,$95
        .byte   $05,$D5,$55,$CA,$15,$7C,$45,$EF
        .byte   $1D,$7F,$55,$BF,$75,$77,$1D,$A8
        .byte   $51,$61,$01,$B1,$11,$F5,$90,$BC
        .byte   $44,$EF,$44,$76,$74,$FF,$D5,$FF
        .byte   $7D,$FF,$F5,$FF,$77,$FD,$55,$BF
        .byte   $D5,$FF,$55,$FF,$F5,$FD,$F5,$1B
        .byte   $04,$68,$50,$98,$51,$21,$01,$82
        .byte   $11,$3B,$11,$03,$40,$D7,$35,$90
        .byte   $54,$22,$15,$BD,$D0,$DF,$55,$7F
        .byte   $D5,$FE,$7D,$FF,$7D,$FF,$57,$5A
        .byte   $01,$63,$50,$1F,$10,$06,$44,$7F
        .byte   $15,$3F,$45,$FB,$54,$FE,$51,$FB
        .byte   $44,$DF,$5D,$FF,$D6,$FF,$DD,$FF
        .byte   $57,$FF,$75,$FF,$55,$FF,$75,$D0
        .byte   $00,$00,$45,$04,$00,$02,$05,$54
        .byte   $00,$86,$01,$C6,$00,$40,$45,$23
        .byte   $00,$6E,$11,$18,$04,$06,$16,$8D
        .byte   $00,$CA,$51,$FE,$55,$FD,$55,$28
        .byte   $2C,$0A,$85,$20,$03,$20,$12,$00
        .byte   $08,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$00,$00,$00,$00,$5C
        .byte   $3A,$10,$99,$28,$4F,$08,$A8,$82
        .byte   $C6,$02,$A8,$80,$09,$00,$00,$00
        .byte   $05,$00,$04,$00,$00,$00,$40,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$82
        .byte   $8C,$8A,$17,$62,$09,$08,$E3,$40
        .byte   $01,$02,$69,$00,$47,$00,$01,$00
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$88
        .byte   $EF,$8A,$AA,$62,$CD,$B0,$BD,$22
        .byte   $B7,$A2,$DC,$02,$C2,$22,$A7,$80
        .byte   $CE,$20,$8C,$AA,$AB,$08,$14,$20
        .byte   $40,$08,$10,$00,$24,$00,$00,$2A
        .byte   $E6,$B0,$8C,$08,$6A,$00,$B0,$0A
        .byte   $08,$00,$00,$00,$10,$02,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$00,$00,$01,$00
        .byte   $A6,$A9,$90,$2A,$6A,$22,$8E,$20
        .byte   $A4,$AA,$20,$80,$2F,$88,$3E,$00
        .byte   $48,$20,$44,$00,$80,$0A,$10,$00
        .byte   $10,$00,$00,$00,$00,$08,$80,$8A
        .byte   $D6,$A8,$2A,$80,$78,$08,$A0,$CA
        .byte   $40,$80,$43,$00,$80,$00,$24,$20
        .byte   $20,$00,$00,$80,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$A8
        .byte   $56,$00,$AE,$A0,$FE,$02,$EC,$8A
        .byte   $7E,$AA,$5B,$80,$D7,$22,$3D,$AA
        .byte   $F1,$AA,$6F,$22,$CA,$A0,$2D,$82
        .byte   $03,$20,$40,$80,$02,$08,$00,$01
        .byte   $92,$82,$B1,$00,$8A,$00,$08,$00
        .byte   $04,$00,$00,$A0,$00,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $8A,$28,$7F,$20,$2A,$AA,$AA,$A2
        .byte   $83,$AA,$57,$A2,$00,$82,$64,$00
        .byte   $01,$00,$04,$80,$88,$00,$80,$00
        .byte   $00,$00,$01,$00,$00,$00,$00,$A8
        .byte   $DE,$00,$03,$20,$56,$02,$87,$02
        .byte   $81,$88,$81,$00,$E8,$20,$10,$20
        .byte   $00,$00,$00,$80,$00,$00,$00,$00
        .byte   $01,$00,$00,$00,$00,$00,$00,$A2
        .byte   $D0,$82,$62,$CA,$85,$A2,$CA,$AA
        .byte   $6B,$AA,$65,$A8,$50,$20,$70,$28
        .byte   $3A,$88,$C2,$2A,$58,$2A,$12,$00
        .byte   $24,$00,$21,$00,$00,$00,$02,$28
        .byte   $C2,$00,$1A,$01,$C3,$88,$0B,$20
        .byte   $01,$00,$24,$22,$00,$00,$00,$00
        .byte   $00,$00,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$A2
        .byte   $39,$2A,$DE,$2A,$F4,$08,$10,$9A
        .byte   $13,$20,$69,$88,$B6,$2A,$B3,$22
        .byte   $2A,$02,$00,$0A,$A4,$02,$80,$0A
        .byte   $95,$00,$00,$00,$00,$00,$00,$BA
        .byte   $0C,$AA,$C7,$A2,$DF,$2A,$10,$80
        .byte   $1C,$00,$E3,$00,$02,$02,$50,$20
        .byte   $00,$08,$00,$00,$08,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$28
        .byte   $15,$A0,$F9,$08,$D3,$80,$9A,$72
        .byte   $2E,$2A,$9A,$AA,$DF,$1A,$B1,$A9
        .byte   $75,$88,$F7,$0A,$59,$AA,$95,$A0
        .byte   $D5,$28,$93,$02,$71,$08,$10,$00
; --- Screen index table ($AA00, terminated by $FF) ---
; Lists screen IDs for the stage layout. 14 screens total.
; Screen numbers $01-$0D; entries $0A repeat for multi-screen rooms.
        .byte   $01,$02,$03,$04,$05,$06,$07,$08
        .byte   $09,$0B,$0B,$0A,$0A,$0A,$0A,$0A
        .byte   $0A,$0A,$0A,$0C,$0C,$0D,$0D,$0A
; --- Room scroll/transition config ($AA19+) ---
; Per-screen scrolling parameters, room transition settings, and
; scroll direction data. Two sub-tables, each terminated by $FF.
        .byte   $0A,$FF,$00,$00,$00,$00,$00,$08
        .byte   $4E,$9A,$D1,$28,$BE,$A8,$19,$2A
        .byte   $3D,$00,$31,$20,$CA,$80,$02,$05
        .byte   $7E,$7E,$7E,$7E,$A0,$00,$08,$08
        .byte   $7E,$02,$00,$00,$00,$00,$00,$40
        .byte   $40,$40,$40,$40,$60,$20,$20,$60
        .byte   $20,$40,$40,$20,$40,$20,$40,$20
        .byte   $40,$20,$40,$20,$40,$20,$40,$20
; --- Room CHR/palette config + scroll direction ($AA58+) ---
        .byte   $40,$FF,$00,$00,$00,$00,$00,$00
        .byte   $00,$32,$00,$19,$00,$32,$00,$32
        .byte   $00,$00,$00,$02,$00,$02,$01,$02
        .byte   $01,$02,$01,$00,$01,$00,$01,$00
        .byte   $01,$00,$01,$00,$01,$00,$01,$60
; --- BG palette data ($AA82) ---
; 8 background palettes (4 bytes each: $0F + 3 NES color values).
; First set = normal BG palettes, second set = alternate/variant.
        .byte   $62,$0F,$20,$22,$00,$0F,$20,$10
        .byte   $08,$0F,$27,$17,$07,$0F,$22,$00
        .byte   $03,$00,$00,$00,$00,$0F,$20,$22
        .byte   $00,$0F,$14,$03,$03,$0F,$27,$17
        .byte   $07,$0F,$22,$00,$03,$8D,$00,$00
        .byte   $00,$88,$BC,$0A,$33,$88,$7B,$AA
        .byte   $BE,$08,$00,$00,$02,$00,$00,$02
        .byte   $02,$00,$00,$00,$04,$00,$00,$A8
        .byte   $4A,$22,$D5,$0A,$7F,$02,$82,$A8
        .byte   $81,$00,$A0,$08,$00,$80,$00,$00
        .byte   $08,$00,$08,$00,$20,$00,$00,$00
        .byte   $08,$00,$00,$00,$00,$00,$00,$A0
        .byte   $3A,$02,$B8,$40,$74,$BA,$E2,$AA
        .byte   $DF,$A2,$FB,$29,$69,$AA,$D9,$08
        .byte   $1A,$A2,$9B,$82,$DA,$22,$FF,$08
; --- Enemy placement: screen numbers ($AB00, terminated by $FF) ---
; Each byte is the screen number where the corresponding enemy spawns.
; Entries are sorted by screen number. Terminated by $FF sentinel.
        .byte   $FF,$FF,$0C,$00,$05,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$02,$02,$02
        .byte   $02,$02,$02,$03,$04,$04,$06,$07
        .byte   $09,$09,$09,$09,$09,$09,$09,$09
        .byte   $09,$09,$0A,$0C,$0E,$10,$12,$14
        .byte   $16,$18,$FF,$01,$8E,$88,$9F,$88
        .byte   $F8,$88,$00,$02,$0F,$00,$41,$28
        .byte   $8A,$00,$80,$00,$00,$00,$00,$00
        .byte   $02,$00,$80,$00,$00,$00,$00,$22
        .byte   $A2,$82,$80,$22,$0E,$02,$04,$82
        .byte   $10,$08,$A0,$80,$C0,$00,$00,$02
        .byte   $20,$00,$90,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$00,$00,$AB
        .byte   $22,$2A,$8A,$42,$F5,$AA,$35,$20
        .byte   $59,$82,$04,$28,$4D,$A2,$6F,$AA
        .byte   $50,$2A,$C0,$2A,$85,$28,$39,$08
        .byte   $0D,$00,$14,$00,$41,$20,$00,$0A
        .byte   $42,$02,$1D,$AA,$00,$08,$80,$00
        .byte   $04,$00,$00,$80,$00,$00,$40,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$68
        .byte   $B6,$82,$26,$A8,$CE,$3A,$08,$28
        .byte   $2A,$32,$FF,$A0,$0C,$00,$86,$00
        .byte   $80,$00,$24,$02,$88,$00,$00,$00
        .byte   $00,$00,$00,$00,$02,$00,$00,$11
        .byte   $23,$28,$A3,$21,$84,$AA,$38,$22
        .byte   $25,$20,$40,$0A,$02,$20,$2B,$20
        .byte   $20,$00,$08,$00,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$2A
        .byte   $77,$E6,$80,$AA,$F0,$A2,$8A,$B2
        .byte   $1F,$2A,$76,$A8,$BB,$EA,$9A,$8E
        .byte   $EA,$D8,$F7,$02,$6F,$20,$92,$28
        .byte   $93,$08,$B3,$82,$00,$00,$E1,$28
; --- Enemy placement: X pixel positions ($AC00, terminated by $FF) ---
        .byte   $38,$48,$58,$C0,$50,$64,$68,$74
        .byte   $80,$84,$94,$50,$70,$D0,$B0,$30
        .byte   $30,$48,$58,$68,$78,$68,$78,$88
        .byte   $98,$D0,$C0,$C8,$D8,$C8,$C0,$D0
        .byte   $D8,$C0,$FF,$59,$01,$24,$01,$00
        .byte   $04,$01,$15,$02,$10,$44,$50,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$11
        .byte   $00,$00,$00,$00,$10,$C8,$01,$01
        .byte   $14,$00,$14,$08,$01,$02,$00,$00
        .byte   $00,$01,$00,$00,$10,$01,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$0D
        .byte   $00,$00,$00,$61,$00,$70,$00,$22
        .byte   $01,$21,$04,$11,$00,$00,$00,$A1
        .byte   $00,$00,$00,$00,$14,$14,$10,$00
        .byte   $00,$00,$00,$00,$04,$01,$00,$80
        .byte   $14,$01,$00,$10,$40,$40,$00,$08
        .byte   $00,$08,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$27,$04,$42,$00,$02,$10,$80
        .byte   $40,$09,$00,$80,$00,$00,$00,$04
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$14
        .byte   $01,$60,$00,$01,$10,$C4,$41,$40
        .byte   $40,$84,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $01,$00,$00,$00,$00,$00,$00,$02
        .byte   $14,$18,$00,$C0,$00,$03,$00,$A0
        .byte   $01,$02,$11,$02,$44,$FE,$05,$31
        .byte   $11,$00,$00,$44,$01,$02,$00,$80
        .byte   $04,$00,$00,$23,$00,$00,$10,$3C
; --- Enemy placement: Y pixel positions ($AD00, terminated by $FF) ---
        .byte   $3C,$3C,$3C,$38,$48,$5C,$B8,$5C
        .byte   $88,$5C,$5C,$48,$48,$28,$B0,$00
        .byte   $50,$BC,$BC,$BC,$BC,$5C,$5C,$5C
        .byte   $5C,$88,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$FF,$02,$40,$20,$00,$12
        .byte   $01,$80,$00,$80,$00,$80,$00,$01
        .byte   $01,$00,$00,$00,$10,$00,$00,$01
        .byte   $00,$00,$00,$20,$00,$00,$40,$00
        .byte   $00,$00,$40,$85,$00,$00,$14,$20
        .byte   $00,$22,$00,$04,$00,$00,$01,$20
        .byte   $00,$02,$00,$00,$00,$00,$04,$00
        .byte   $00,$00,$00,$80,$00,$00,$00,$4B
        .byte   $05,$80,$40,$10,$10,$06,$00,$02
        .byte   $04,$94,$01,$42,$00,$35,$00,$04
        .byte   $40,$0C,$00,$02,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$00,$00,$44
        .byte   $00,$00,$00,$80,$04,$02,$45,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$30
        .byte   $04,$02,$00,$04,$41,$54,$54,$01
        .byte   $00,$20,$04,$04,$04,$00,$00,$01
        .byte   $01,$00,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$02,$04,$01
        .byte   $41,$02,$00,$1B,$00,$81,$00,$00
        .byte   $00,$28,$40,$80,$00,$00,$40,$00
        .byte   $00,$00,$00,$10,$00,$10,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $04,$50,$04,$03,$05,$C2,$04,$D2
        .byte   $41,$50,$01,$04,$01,$60,$40,$C4
        .byte   $11,$29,$10,$91,$00,$1D,$01,$04
        .byte   $04,$20,$04,$00,$00,$00,$00,$53
; --- Enemy placement: global enemy IDs ($AE00, terminated by $FF) ---
; Entity IDs for each spawned enemy. $53=Kamegoro Maker, $52=Returning
; Sniper Joe, $51=Bikky, $56=Walking Bomb, $11=weapon energy, $8A=Peterchy,
; $55=Hologran, and various item drops ($47-$4E).
        .byte   $53,$53,$53,$52,$11,$51,$52,$51
        .byte   $56,$51,$51,$11,$11,$11,$8A,$8A
        .byte   $8A,$53,$53,$53,$53,$53,$53,$53
        .byte   $53,$55,$47,$48,$49,$4A,$4B,$4C
        .byte   $4D,$4E,$FF,$00,$01,$00,$00,$E4
        .byte   $01,$46,$00,$06,$00,$00,$00,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$00,$11
        .byte   $00,$00,$00,$A0,$00,$08,$04,$80
        .byte   $00,$01,$00,$00,$00,$00,$00,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$50,$00,$00,$04,$92,$00,$41
        .byte   $20,$0A,$00,$04,$00,$00,$00,$20
        .byte   $01,$84,$05,$04,$05,$21,$40,$31
        .byte   $00,$00,$00,$00,$00,$00,$01,$0C
        .byte   $00,$28,$00,$20,$00,$00,$14,$02
        .byte   $00,$08,$00,$00,$04,$00,$00,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$00,$00,$00,$01,$3C
        .byte   $40,$40,$01,$28,$14,$80,$01,$00
        .byte   $40,$00,$04,$80,$01,$40,$10,$01
        .byte   $00,$02,$00,$00,$00,$00,$10,$10
        .byte   $00,$00,$00,$00,$00,$00,$00,$24
        .byte   $40,$40,$40,$20,$00,$80,$50,$00
        .byte   $00,$00,$00,$20,$00,$40,$00,$08
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$04,$01
        .byte   $04,$A0,$00,$98,$01,$1C,$40,$32
        .byte   $40,$81,$04,$0C,$54,$00,$00,$20
        .byte   $40,$10,$84,$00,$04,$04,$00,$61
        .byte   $40,$00,$00,$01,$01,$00,$00,$00
; ===========================================================================
; Metatile column definitions ($AF00+)
; ===========================================================================
; Each 8-byte group defines one column of metatiles (8 rows top-to-bottom).
; Metatile indices reference the CHR/attribute definitions at $B700/$BB00.
; The level is built from columns of 8-metatile-tall "strips".
; ---------------------------------------------------------------------------
        .byte   $01,$02,$03,$04,$05,$06,$07,$00
        .byte   $08,$09,$09,$0A,$0B,$0C,$07,$00
        .byte   $0D,$0D,$0E,$0F,$0D,$0D,$07,$00
        .byte   $10,$11,$12,$13,$14,$15,$07,$00
        .byte   $16,$17,$18,$19,$1A,$1B,$07,$1C
        .byte   $1C,$1C,$1C,$00,$1D,$07,$1C,$1C
        .byte   $1C,$1C,$1C,$00,$0F,$07,$1C,$1C
        .byte   $1C,$1C,$1C,$00,$1E,$07,$1C,$1F
        .byte   $1F,$1F,$1F,$20,$21,$22,$1F,$20
        .byte   $23,$24,$25,$26,$27,$28,$22,$20
        .byte   $29,$2A,$2B,$2C,$2D,$04,$22,$20
        .byte   $2E,$2F,$30,$31,$32,$33,$22,$20
        .byte   $34,$1E,$0E,$35,$0F,$36,$22,$20
        .byte   $37,$38,$1D,$39,$14,$3A,$22,$20
        .byte   $3B,$1F,$1F,$1F,$1F,$1F,$1F,$20
        .byte   $3C,$1F,$1F,$1F,$1F,$1F,$1F,$00
        .byte   $3D,$1C,$1C,$1C,$1C,$1C,$1C,$00
        .byte   $3E,$3F,$40,$41,$42,$43,$07,$1C
        .byte   $00,$0F,$44,$45,$46,$47,$07,$00
        .byte   $48,$49,$0D,$0D,$44,$34,$07,$00
        .byte   $1D,$0E,$4A,$4B,$14,$35,$07,$00
        .byte   $35,$1E,$1E,$1E,$37,$1E,$07,$00
        .byte   $4C,$4C,$07,$1C,$4D,$4E,$07,$1C
        .byte   $1C,$1C,$1C,$1C,$1C,$4F,$07,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$50,$22,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$51,$22,$20
        .byte   $01,$52,$53,$04,$05,$06,$22,$20
        .byte   $54,$0A,$55,$09,$56,$57,$22,$20
        .byte   $0F,$58,$59,$5A,$38,$15,$22,$20
        .byte   $35,$5B,$17,$18,$5C,$1B,$22,$20
        .byte   $3B,$1F,$1F,$1F,$1F,$1F,$1F,$20
        .byte   $3C,$1F,$1F,$1F,$1F,$1F,$1F,$00
        .byte   $5D,$1C,$1C,$1C,$1C,$1C,$1C,$00
        .byte   $5E,$1C,$1C,$00,$5F,$60,$07,$00
        .byte   $61,$62,$04,$05,$06,$63,$07,$00
        .byte   $64,$34,$34,$56,$65,$66,$07,$00
        .byte   $67,$68,$36,$0F,$09,$1E,$07,$00
        .byte   $17,$18,$69,$38,$1D,$6A,$07,$1C
        .byte   $1C,$1C,$1C,$1C,$1C,$4F,$07,$1C
        .byte   $1C,$1C,$1C,$1C,$1C,$4F,$07,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$50,$22,$20
        .byte   $6B,$6C,$6D,$1E,$6E,$6F,$22,$20
        .byte   $70,$71,$5B,$72,$22,$1F,$1F,$20
        .byte   $73,$22,$1F,$1F,$1F,$1F,$1F,$20
        .byte   $74,$75,$6E,$1E,$6D,$76,$77,$20
        .byte   $78,$79,$5B,$7A,$5B,$7B,$77,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$7C,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$20,$7D,$22,$1F,$7E
        .byte   $6D,$1E,$44,$1E,$7F,$22,$1F,$80
        .byte   $1D,$39,$14,$76,$81,$22,$1F,$1F
        .byte   $1F,$1F,$1F,$20,$82,$22,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$1F,$1F,$1F,$1F,$1F,$20
        .byte   $83,$22,$1F,$1F,$1F,$1F,$1F,$1F
        .byte   $84,$1F,$1F,$1F,$1F,$1F,$1F,$20
        .byte   $81,$68,$1E,$85,$1F,$1F,$1F,$20
        .byte   $82,$22,$50,$85,$1F,$1F,$1F,$1F
        .byte   $1F,$1F,$50,$85,$1F,$1F,$1F,$86
        .byte   $87,$88,$89,$8A,$88,$8B,$8C,$8D
        .byte   $81,$8E,$8F,$90,$91,$81,$92,$8D
        .byte   $93,$94,$8F,$90,$95,$93,$92,$8D
        .byte   $81,$8E,$96,$97,$91,$81,$98,$8D
        .byte   $93,$8E,$99,$9A,$91,$93,$77,$8D
        .byte   $81,$1E,$81,$81,$1E,$81,$77,$9B
        .byte   $9C,$9D,$9C,$9C,$9D,$9C,$9E,$9F
        .byte   $9F,$9F,$9F,$9F,$9F,$9F,$9F,$A0
        .byte   $A1,$A1,$A1,$A1,$A1,$A1,$A2,$8D
        .byte   $7F,$A3,$A4,$A4,$A4,$A4,$92,$8D
        .byte   $81,$A5,$A6,$A6,$A6,$A6,$92,$A7
        .byte   $A8,$A9,$AA,$AB,$A6,$A6,$92,$AC
        .byte   $A4,$A4,$A4,$A4,$A6,$AD,$AE,$AF
        .byte   $A6,$A6,$A6,$A6,$A6,$92,$9F,$B0
        .byte   $B0,$B0,$B0,$B0,$B1,$B2,$9F,$9F
        .byte   $9F,$9F,$9F,$9F,$9F,$9F,$9F,$B3
        .byte   $B4,$B4,$B4,$B4,$B4,$B4,$B5,$B6
        .byte   $A4,$A4,$A4,$A4,$A4,$A4,$B7,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$BA
        .byte   $BB,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$BC
        .byte   $BD,$B0,$B0,$B0,$B0,$B0,$BE,$BF
        .byte   $C0,$9F,$9F,$9F,$9F,$9F,$9F,$B3
        .byte   $B4,$B4,$B4,$B4,$B4,$B4,$B5,$B6
        .byte   $A4,$A4,$A4,$A4,$A4,$A4,$B7,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$C1
        .byte   $C2,$A6,$A6,$A6,$A6,$A6,$B9,$C3
        .byte   $C4,$C5,$C5,$C5,$C5,$C5,$C6,$BF
        .byte   $C0,$9F,$9F,$9F,$9F,$9F,$9F,$B3
        .byte   $B4,$B4,$B4,$B4,$B4,$B4,$B5,$B6
        .byte   $A4,$A4,$A4,$A4,$A4,$A4,$B7,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$BA
        .byte   $BB,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$C7,$C8,$A6,$A6,$B9,$BC
        .byte   $BD,$C9,$CA,$CB,$CC,$B0,$BE,$BF
        .byte   $C0,$9F,$9F,$9F,$9F,$9F,$9F,$B3
        .byte   $B4,$B4,$B4,$B4,$B4,$B4,$B5,$B6
        .byte   $A4,$A4,$A4,$A4,$A4,$A4,$B7,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$B8
        .byte   $A6,$A6,$A6,$A6,$A6,$A6,$B9,$C1
        .byte   $C2,$A6,$C7,$C8,$A6,$A6,$B9,$C3
        .byte   $C4,$CD,$BE,$CE,$C8,$A6,$B9,$BF
        .byte   $C0,$9F,$9F,$9F,$CE,$B0,$BE,$BF
        .byte   $C0,$9F,$9F,$9F,$9F,$9F,$9F,$1E
; --- Unused metatile column slots (filled with $1E) ---
; Wily Fortress 4 is a short stage, so most column slots are empty.
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$CF
        .byte   $D0,$88,$89,$8A,$88,$D1,$D2,$D3
        .byte   $D4,$D5,$D6,$0E,$D7,$D8,$D9,$DA
        .byte   $DB,$DC,$DD,$DE,$14,$DF,$E0,$D3
        .byte   $D4,$E1,$E2,$D5,$E3,$D8,$E4,$DA
        .byte   $E5,$E6,$E7,$E7,$E8,$E9,$EA,$D3
        .byte   $D4,$EB,$81,$81,$EC,$D8,$ED,$EE
        .byte   $EF,$F0,$9C,$9C,$9E,$F1,$EF,$9F
        .byte   $9F,$9F,$9F,$9F,$9F,$9F,$9F,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
; ===========================================================================
; Metatile CHR definitions ($B700+)
; ===========================================================================
; 4 bytes per metatile: 2x2 tile IDs defining the visual appearance.
; Each metatile is a 16x16 pixel block composed of four 8x8 CHR tiles.
; Byte order: top-left, top-right, bottom-left, bottom-right.
; Entries $00-$1D are unused ($1E fill); active metatiles start later.
; ---------------------------------------------------------------------------
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
        .byte   $1E,$1E,$1E,$1E,$1E,$1E,$1E,$10
        .byte   $11,$11,$12,$C0,$C6,$C2,$E2,$C7
        .byte   $C0,$FC,$E5,$C0,$C1,$F7,$DB,$C6
        .byte   $C0,$E0,$C2,$C0,$C1,$CA,$CB,$C7
        .byte   $C0,$CF,$CB,$10,$11,$12,$10,$F8
        .byte   $00,$00,$BE,$00,$00,$00,$BD,$00
        .byte   $F8,$00,$00,$F0,$DB,$00,$F8,$FC
        .byte   $E5,$F8,$F8,$90,$90,$00,$00,$B7
        .byte   $B6,$00,$00,$B5,$B6,$BE,$BD,$CD
        .byte   $CD,$CB,$EB,$CE,$00,$D4,$F8,$B6
        .byte   $B7,$00,$90,$B5,$B7,$90,$BD,$B5
        .byte   $B7,$BD,$00,$B7,$00,$00,$F8,$DB
        .byte   $E0,$CB,$CB,$D7,$C2,$EC,$EE,$CA
        .byte   $EB,$D3,$D3,$00,$B6,$00,$00,$B5
        .byte   $00,$BE,$00,$EA,$C8,$D3,$D3,$10
        .byte   $11,$11,$10,$B5,$B6,$BD,$BD,$00
        .byte   $00,$00,$00,$11,$10,$10,$11,$11
        .byte   $12,$10,$11,$F8,$F8,$F8,$F8,$12
        .byte   $10,$10,$11,$C0,$C1,$E8,$FD,$C0
        .byte   $C7,$E5,$F7,$C0,$C6,$D8,$D2,$C0
        .byte   $C6,$CB,$D0,$C0,$C1,$C2,$C2,$C0
        .byte   $C0,$DA,$DB,$C0,$C7,$D2,$CF,$C1
        .byte   $C0,$C8,$C2,$C6,$C0,$D2,$EB,$C0
        .byte   $C1,$F8,$F8,$C0,$C0,$E8,$DB,$CD
        .byte   $DE,$CB,$CB,$D3,$F8,$F3,$00,$F8
        .byte   $F2,$00,$00,$CB,$CB,$00,$00,$F3
        .byte   $00,$00,$00,$00,$F8,$BD,$00,$F8
        .byte   $F8,$00,$00,$B6,$B6,$BE,$00,$B7
        .byte   $B6,$00,$BD,$B7,$B5,$00,$00,$B6
        .byte   $B6,$BE,$BE,$B7,$B6,$00,$BE,$B5
        .byte   $B6,$BE,$BE,$07,$10,$0F,$12,$0F
        .byte   $10,$0F,$12,$00,$12,$00,$10,$00
        .byte   $12,$00,$00,$10,$11,$00,$00,$10
        .byte   $11,$00,$F8,$10,$11,$D3,$D3,$10
        .byte   $11,$F8,$D4,$10,$11,$F8,$D3,$00
        .byte   $00,$BD,$00,$F0,$E0,$F8,$00,$DA
        .byte   $FC,$F0,$DB,$E5,$E5,$DB,$E0,$00
        .byte   $00,$00,$BE,$00,$B6,$BD,$00,$B5
        .byte   $B6,$90,$90,$B6,$B6,$90,$90,$00
        .byte   $00,$BF,$BF,$00,$00,$11,$10,$00
        .byte   $00,$11,$07,$12,$0F,$11,$0F,$11
        .byte   $00,$12,$00,$11,$F8,$12,$F8,$C7
        .byte   $C1,$FC,$E5,$C0,$C0,$F7,$DB,$F8
        .byte   $00,$00,$BD,$F8,$00,$00,$00,$F0
        .byte   $DB,$00,$00,$FC,$E5,$00,$F8,$00
        .byte   $00,$00,$CC,$00,$00,$CE,$00,$B7
        .byte   $B7,$00,$00,$12,$12,$10,$11,$00
        .byte   $00,$F8,$F8,$F8,$12,$F8,$10,$C0
        .byte   $12,$DB,$10,$C0,$C6,$CE,$E0,$C7
        .byte   $C0,$D7,$C2,$C0,$C1,$DB,$FD,$C7
        .byte   $C0,$F7,$DB,$C0,$C0,$CF,$CB,$CD
        .byte   $DE,$F8,$F8,$FC,$E5,$00,$00,$F7
        .byte   $DB,$F8,$F8,$CE,$F8,$D4,$F8,$F8
        .byte   $00,$F8,$00,$00,$B7,$F8,$00,$12
        .byte   $07,$12,$0F,$CB,$CF,$CD,$DE,$F3
        .byte   $00,$F8,$00,$00,$00,$BE,$BD,$00
        .byte   $00,$00,$BC,$00,$00,$BE,$BC,$F8
        .byte   $F8,$F8,$E8,$CC,$CD,$DF,$DB,$CD
        .byte   $CD,$DB,$DB,$F8,$F2,$F8,$F8,$F8
        .byte   $F8,$CD,$CD,$F8,$F8,$CE,$F8,$00
        .byte   $F8,$00,$F8,$00,$0E,$00,$0E,$C9
        .byte   $CA,$F8,$F2,$CF,$CB,$EC,$CD,$CB
        .byte   $CB,$CD,$CD,$CB,$EB,$CE,$D3,$11
        .byte   $10,$B0,$B1,$B8,$B9,$10,$11,$15
        .byte   $00,$15,$00,$00,$00,$84,$85,$15
        .byte   $B6,$15,$00,$88,$89,$82,$83,$8A
        .byte   $8B,$10,$11,$B0,$B1,$B8,$B9,$11
        .byte   $10,$80,$81,$00,$10,$00,$12,$99
        .byte   $95,$99,$97,$8F,$9F,$84,$85,$9F
        .byte   $9F,$00,$00,$9D,$00,$00,$00,$00
        .byte   $9C,$00,$00,$9F,$8E,$84,$85,$94
        .byte   $99,$96,$99,$99,$97,$99,$97,$A7
        .byte   $BB,$A7,$BB,$AF,$A5,$AF,$A5,$A6
        .byte   $AD,$A6,$AD,$BA,$B2,$BA,$B2,$96
        .byte   $99,$96,$99,$8A,$8B,$80,$81,$A4
        .byte   $BB,$A7,$BB,$BA,$A4,$BA,$B2,$AF
        .byte   $A4,$AF,$A5,$A4,$AD,$A6,$AD,$96
        .byte   $99,$9C,$9F,$AF,$A5,$84,$85,$A6
        .byte   $AD,$84,$85,$99,$97,$99,$9B,$8A
        .byte   $8B,$91,$91,$92,$93,$9A,$9B,$92
        .byte   $9E,$9A,$99,$99,$99,$99,$99,$99
        .byte   $99,$99,$95,$99,$99,$98,$98,$99
        .byte   $99,$94,$99,$00,$A8,$00,$A9,$A8
        .byte   $A8,$A9,$A9,$00,$A9,$00,$A9,$A9
        .byte   $A9,$A9,$A9,$99,$97,$9F,$A0,$8A
        .byte   $8B,$A1,$A2,$92,$9E,$A3,$9F,$9E
        .byte   $9E,$9F,$9F,$9E,$93,$9F,$9D,$16
        .byte   $A8,$17,$A9,$A9,$A9,$92,$86,$96
        .byte   $99,$9A,$99,$17,$A9,$17,$A9,$9E
        .byte   $9E,$99,$99,$9E,$86,$99,$99,$9A
        .byte   $99,$99,$99,$99,$99,$95,$8F,$99
        .byte   $99,$9F,$9F,$99,$99,$8E,$94,$97
        .byte   $A8,$97,$A9,$A8,$96,$A9,$96,$97
        .byte   $A9,$97,$A9,$A9,$96,$A9,$96,$97
        .byte   $A9,$97,$AA,$A9,$A9,$AB,$A9,$97
        .byte   $8A,$97,$8C,$8B,$92,$8D,$96,$86
        .byte   $9A,$99,$99,$97,$8C,$97,$8C,$8D
        .byte   $96,$8D,$96,$97,$AA,$97,$A9,$AB
        .byte   $A9,$A9,$A9,$97,$A9,$97,$8A,$A9
        .byte   $A9,$8B,$92,$A9,$A9,$9E,$9E,$A9
        .byte   $96,$86,$9A,$A9,$92,$A9,$96,$93
        .byte   $A9,$97,$A9,$93,$A9,$9B,$91,$A9
        .byte   $96,$91,$9A,$97,$A9,$9B,$91,$A9
        .byte   $92,$91,$9A,$A9,$92,$86,$9A,$9B
        .byte   $87,$99,$99,$95,$8F,$97,$84,$9F
        .byte   $9F,$85,$00,$9F,$9F,$00,$84,$8E
        .byte   $94,$85,$96,$97,$88,$97,$82,$89
        .byte   $00,$83,$00,$B6,$B7,$00,$00,$B7
        .byte   $B7,$BD,$00,$B7,$B7,$00,$BD,$00
        .byte   $88,$00,$82,$89,$96,$83,$96,$97
        .byte   $8A,$97,$80,$8B,$B7,$81,$00,$B5
        .byte   $B7,$00,$00,$B6,$B7,$BC,$00,$B6
        .byte   $B7,$BD,$00,$B7,$8A,$00,$80,$8B
        .byte   $96,$81,$96,$B7,$A4,$BD,$00,$B7
        .byte   $B5,$00,$BD,$A4,$B7,$00,$BD,$89
        .byte   $96,$83,$9C,$8B,$B7,$81,$A4,$B6
        .byte   $B5,$BC,$00,$B7,$B7,$84,$85,$B7
        .byte   $B7,$BD,$BE,$B7,$8A,$A4,$80,$8B
        .byte   $0E,$81,$0E,$B7,$00,$BD,$00,$00
        .byte   $B5,$00,$BC,$89,$0E,$83,$0E,$97
        .byte   $8A,$9B,$91,$8B,$92,$91,$9A,$9E
        .byte   $93,$99,$9B,$93,$8A,$9B,$91,$8B
; ===========================================================================
; Metatile attribute / flip plane data ($BB00+)
; ===========================================================================
; 4 bytes per metatile: palette assignment and horizontal/vertical flip
; flags for each of the four 8x8 tiles in the 2x2 metatile.
; Bit layout per byte: VH--PP-- (V=vflip, H=hflip, PP=palette 0-3).
; ---------------------------------------------------------------------------
        .byte   $00,$81,$00,$00,$8A,$00,$80,$00
        .byte   $A4,$00,$00,$A4,$00,$00,$00,$8B
        .byte   $00,$81,$A4,$00,$8A,$A4,$80,$A8
        .byte   $A8,$84,$85,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$11,$00,$0C,$0C,$0E,$10
        .byte   $00,$00,$00,$24,$4D,$1E,$0E,$86
        .byte   $A6,$86,$00,$24,$00,$00,$A4,$86
        .byte   $A6,$86,$00,$24,$14,$00,$00,$2C
        .byte   $2E,$40,$42,$00,$00,$00,$6D,$4C
        .byte   $3E,$60,$62,$64,$74,$74,$6F,$2C
        .byte   $2E,$40,$42,$00,$00,$6C,$6D,$4C
        .byte   $3E,$60,$62,$64,$74,$6E,$6F,$00
        .byte   $24,$00,$24,$0A,$00,$76,$6D,$1A
        .byte   $0A,$1A,$0A,$2A,$01,$78,$6F,$00
        .byte   $00,$20,$11,$32,$01,$6C,$6D,$4C
        .byte   $3E,$01,$21,$32,$01,$6E,$6F,$06
        .byte   $08,$02,$04,$12,$12,$44,$6B,$26
        .byte   $28,$67,$58,$30,$30,$00,$6A,$46
        .byte   $48,$4A,$48,$11,$4A,$CC,$CE,$56
        .byte   $58,$10,$58,$5A,$10,$EC,$EE,$98
        .byte   $9A,$C8,$CA,$88,$8A,$E2,$95,$B8
        .byte   $BA,$E8,$EA,$F8,$94,$C2,$C3,$E5
        .byte   $E1,$E0,$E2,$C3,$C0,$F0,$C0,$C1
        .byte   $C0,$D4,$C0,$F0,$C2,$E2,$C2,$C2
        .byte   $95,$E2,$D4,$E0,$93,$BC,$00,$00
        .byte   $A4,$C6,$E6,$A4,$AD,$9D,$AE,$CC
        .byte   $CE,$BF,$BC,$AF,$8C,$8C,$8C,$EC
        .byte   $EE,$AF,$BF,$9C,$AC,$AC,$4B,$8E
        .byte   $8E,$90,$90,$90,$00,$80,$8E,$81
        .byte   $90,$82,$00,$00,$00,$00,$00,$91
        .byte   $90,$92,$80,$00,$00,$00,$90,$90
        .byte   $90,$90,$90,$00,$00,$00,$90,$80
        .byte   $90,$80,$00,$00,$90,$00,$00,$81
        .byte   $82,$00,$00,$00,$00,$00,$00,$91
        .byte   $92,$80,$80,$90,$90,$90,$90,$00
        .byte   $00,$00,$00,$90,$90,$00,$00,$00
        .byte   $00,$10,$11,$00,$0D,$0D,$0F,$10
        .byte   $00,$00,$00,$25,$4D,$1F,$0F,$87
        .byte   $A7,$A7,$00,$25,$00,$00,$A5,$87
        .byte   $A7,$A7,$00,$25,$15,$00,$00,$2D
        .byte   $2F,$41,$43,$00,$00,$00,$6C,$3D
        .byte   $4F,$61,$63,$65,$75,$75,$6E,$2D
        .byte   $2F,$41,$43,$00,$00,$6D,$6D,$3D
        .byte   $4F,$61,$63,$65,$75,$6F,$6F,$00
        .byte   $25,$00,$25,$0A,$00,$77,$6C,$1B
        .byte   $0A,$1B,$0A,$2B,$01,$79,$6E,$00
        .byte   $00,$01,$31,$11,$23,$6D,$6D,$3D
        .byte   $4F,$01,$31,$22,$01,$6F,$6F,$07
        .byte   $09,$03,$05,$12,$13,$6A,$6B,$27
        .byte   $29,$67,$66,$30,$33,$00,$45,$47
        .byte   $4A,$47,$49,$11,$4A,$CD,$CF,$57
        .byte   $10,$57,$59,$11,$10,$ED,$EF,$99
        .byte   $9B,$C9,$CB,$89,$8B,$95,$E1,$B9
        .byte   $BB,$E9,$EB,$84,$FB,$C3,$C1,$E5
        .byte   $E2,$E1,$E3,$C0,$C3,$C0,$F3,$C2
        .byte   $C0,$C0,$D5,$C1,$F3,$E1,$C1,$D5
        .byte   $E1,$95,$C1,$E3,$BC,$9D,$BD,$00
        .byte   $A5,$C7,$E7,$A3,$AE,$AD,$AF,$CD
        .byte   $CF,$00,$BC,$93,$8D,$8D,$8D,$ED
        .byte   $EF,$BD,$AD,$AC,$00,$00,$0B,$8E
        .byte   $8F,$90,$83,$83,$83,$8E,$A0,$90
        .byte   $90,$00,$00,$00,$00,$00,$A0,$90
        .byte   $90,$00,$00,$A0,$00,$00,$A0,$90
        .byte   $90,$90,$90,$A0,$00,$A0,$A0,$90
        .byte   $90,$90,$00,$00,$90,$00,$00,$90
        .byte   $00,$00,$00,$A0,$00,$00,$A0,$90
        .byte   $00,$00,$00,$A0,$90,$90,$A0,$00
        .byte   $00,$00,$00,$A0,$90,$00,$00,$00
        .byte   $00,$10,$11,$00,$1C,$1C,$0E,$10
        .byte   $00,$00,$34,$34,$4E,$1E,$0E,$96
        .byte   $B6,$96,$34,$34,$00,$B4,$B4,$96
        .byte   $B6,$96,$34,$34,$34,$00,$00,$3C
        .byte   $3E,$50,$52,$00,$00,$00,$7D,$5C
        .byte   $5E,$70,$72,$74,$74,$3A,$7F,$3C
        .byte   $3E,$50,$52,$00,$00,$7C,$7D,$5C
        .byte   $5E,$70,$72,$74,$74,$7E,$7F,$1A
        .byte   $0A,$1A,$0A,$1A,$00,$68,$7D,$1A
        .byte   $34,$1A,$34,$1A,$01,$78,$7F,$3C
        .byte   $3E,$11,$11,$11,$32,$7C,$7D,$5C
        .byte   $5E,$21,$11,$11,$32,$7E,$7F,$16
        .byte   $18,$16,$18,$30,$30,$54,$7B,$36
        .byte   $38,$10,$58,$30,$30,$00,$7A,$56
        .byte   $58,$10,$58,$5A,$10,$DC,$DE,$56
        .byte   $58,$10,$58,$5A,$10,$FC,$FE,$A8
        .byte   $AA,$D8,$DA,$A8,$AA,$F2,$E4,$C8
        .byte   $CA,$F8,$FA,$F8,$94,$D2,$85,$F5
        .byte   $F1,$F0,$F2,$C4,$C0,$F0,$C0,$D1
        .byte   $C0,$E4,$C0,$D0,$D2,$F2,$D2,$D2
        .byte   $D2,$D2,$D2,$D0,$00,$00,$00,$B4
        .byte   $B4,$D6,$F6,$B4,$00,$00,$00,$DC
        .byte   $DE,$00,$00,$00,$9C,$9C,$00,$FC
        .byte   $FE,$00,$00,$AC,$9C,$9C,$5B,$9E
        .byte   $9E,$90,$90,$00,$90,$80,$9E,$80
        .byte   $90,$80,$90,$00,$B0,$B0,$90,$90
        .byte   $90,$90,$80,$00,$00,$00,$90,$81
        .byte   $90,$82,$00,$00,$00,$B0,$00,$91
        .byte   $90,$92,$00,$00,$B0,$00,$00,$80
        .byte   $80,$81,$82,$90,$90,$B0,$B0,$00
        .byte   $00,$91,$92,$90,$90,$B0,$B0,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$11,$00,$1D,$1D,$0F,$10
        .byte   $00,$00,$35,$35,$4E,$1F,$0F,$97
        .byte   $B7,$B7,$35,$35,$00,$B5,$B5,$97
        .byte   $B7,$B7,$35,$35,$35,$00,$00,$3D
        .byte   $3F,$51,$53,$00,$00,$00,$7C,$5D
        .byte   $5F,$71,$73,$75,$75,$3B,$7E,$3D
        .byte   $3F,$51,$53,$00,$00,$7D,$7D,$5D
        .byte   $5F,$71,$73,$75,$75,$7F,$7F,$1B
        .byte   $0A,$1B,$0A,$1B,$00,$69,$7C,$1B
        .byte   $35,$1B,$35,$1B,$01,$79,$7E,$3D
        .byte   $3F,$31,$11,$11,$11,$7D,$7D,$5D
        .byte   $5F,$31,$11,$11,$22,$7F,$7F,$17
        .byte   $19,$17,$19,$30,$33,$7A,$7B,$37
        .byte   $39,$10,$66,$30,$33,$00,$55,$57
        .byte   $10,$57,$59,$11,$10,$DD,$DF,$57
        .byte   $10,$57,$59,$11,$10,$FD,$FF,$A9
        .byte   $AB,$D9,$DB,$A9,$AB,$E4,$F1,$C9
        .byte   $CB,$F9,$FB,$84,$FB,$85,$D1,$F5
        .byte   $F2,$F1,$F3,$C0,$C5,$C0,$F3,$D2
        .byte   $C0,$C0,$E4,$D1,$D3,$F1,$D1,$D1
        .byte   $D1,$D1,$D1,$D3,$00,$00,$00,$B5
        .byte   $B5,$D7,$F7,$B3,$00,$00,$00,$DD
        .byte   $DF,$00,$00,$00,$9C,$00,$00,$FD
        .byte   $FF,$00,$00,$AC,$AC,$00,$1D,$9E
        .byte   $9F,$90,$83,$83,$83,$9E,$A0,$90
        .byte   $90,$90,$90,$A1,$B0,$A2,$A0,$90
        .byte   $90,$90,$00,$A0,$00,$00,$A0,$90
        .byte   $90,$00,$00,$B1,$00,$B2,$A0,$90
        .byte   $90,$00,$00,$00,$B0,$00,$00,$00
        .byte   $00,$90,$00,$B1,$A1,$A2,$B2,$00
        .byte   $00,$90,$00,$B1,$A1,$A2,$B2,$00
        .byte   $00,$00,$00,$B1,$A1,$00,$00,$00
; ===========================================================================
; Collision attribute table ($BF00+)
; ===========================================================================
; One byte per metatile. Defines collision behavior:
;   $00 = empty/passable          $10 = solid
;   $01 = breakable               $03 = solid (variant)
;   $11 = solid (variant)         $12 = (verify in Mesen)
;   $13 = semi-solid platform     $23 = (verify in Mesen)
;   $50 = ladder                  $60 = spike (instant kill)
;   $61 = death pit               $81 = solid (special)
; Used by the tile collision system in the fixed bank.
; ---------------------------------------------------------------------------
        .byte   $00,$02,$01,$00,$53,$51,$43,$03
        .byte   $00,$00,$13,$13,$10,$10,$23,$13
        .byte   $13,$13,$03,$03,$10,$13,$13,$11
        .byte   $11,$11,$81,$81,$81,$00,$00,$03
        .byte   $03,$13,$13,$10,$10,$03,$11,$03
        .byte   $03,$13,$13,$13,$13,$13,$11,$81
        .byte   $81,$11,$11,$10,$10,$11,$11,$81
        .byte   $81,$11,$11,$11,$11,$11,$11,$03
        .byte   $03,$81,$81,$03,$00,$11,$13,$03
        .byte   $03,$81,$81,$81,$01,$11,$13,$03
        .byte   $03,$01,$01,$01,$01,$13,$13,$03
        .byte   $03,$01,$01,$01,$01,$13,$13,$12
        .byte   $12,$12,$12,$01,$01,$11,$11,$12
        .byte   $12,$10,$10,$01,$01,$00,$11,$10
        .byte   $10,$10,$10,$01,$10,$10,$10,$10
        .byte   $10,$10,$10,$01,$10,$10,$10,$12
        .byte   $12,$60,$60,$12,$12,$10,$10,$60
        .byte   $60,$12,$12,$12,$12,$10,$10,$13
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$10,$10,$10,$10
        .byte   $10,$10,$10,$10,$01,$01,$01,$03
        .byte   $03,$03,$03,$03,$01,$01,$01,$03
        .byte   $03,$01,$01,$01,$03,$03,$03,$03
        .byte   $03,$01,$01,$03,$03,$03,$50,$61
; --- Unused collision slots filled with $61 (death pit) to end of bank ---
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $61,$61,$61,$61,$61,$61,$61
