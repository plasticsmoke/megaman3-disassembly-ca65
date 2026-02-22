; =============================================================================
; MEGA MAN 3 (U) — BANK $0E — NAMETABLE STREAMING + WILY 6 STAGE DATA
; =============================================================================
; Mapped to $A000-$BFFF via MMC3 bank swap.
;
; This bank serves dual purposes: it contains nametable streaming code used
; by the stage select and password screens, AND it doubles as the stage data
; bank for Wily 6 (stage $16) via the stage_to_bank mapping table.
;
; ---- Nametable Streaming Code ($A000-$A09C) ----
;
;   $A000:  JMP nametable_init_columns — clear 4 column pairs on nametable
;   $A003:  JMP nametable_advance_row  — increment PPU low address by 1
;   $A006:  nametable_stream_by_index  — stream text string X to PPU buffer
;
;   Streams data into the $0780 PPU update buffer, which the NMI handler
;   (drain_ppu_buffer) writes to VRAM each frame.
;   Called via trampoline at fixed bank $FF21 (task_yield for NMI sync).
;
; ---- Nametable Address Tables ($A09D-$A0C0) ----
;
;   $A09D:  PPU address low/high bytes for column init (4 bytes each)
;   $A0A5:  String pointer table — low bytes  (14 entries)
;   $A0B3:  String pointer table — high bytes (14 entries)
;
; ---- Nametable Text Data ($A0C1-$A32C) ----
;
;   Strings 0-7:   Robot master name + designer credit (stage select screen)
;   Strings 8-13:  Password screen text
;   Format: $FE xx yy = set PPU address, $FF = end-of-string
;
; ---- Wily 6 Stage Data ($A32D-$BFFF) ----
;
;   $A32D-$A9FC:  2-bit encoded screen nametable column data
;   $AA00-$AA5C:  Screen layout table (20 bytes/screen: 16 column IDs + 4 conn)
;   $AA5D-$AA7F:  Room config / screen pointer table
;   $AA80-$AA9F:  BG palette data (4 palettes x 4 NES colors)
;   $AB00-$AB2C:  Enemy placement — screen numbers ($FF-terminated)
;   $AB2D-$ABFF:  Enemy placement — continued / padding
;   $AC00-$AC31:  Enemy placement — X pixel positions ($FF-terminated)
;   $AD00-$AD24:  Enemy placement — Y pixel positions ($FF-terminated)
;   $AE00-$AE22:  Enemy placement — global enemy type IDs ($FF-terminated)
;   $AF05-$B6FF:  Metatile column definitions (8 metatile IDs per column)
;   $B700-$B7B4:  Metatile CHR tile definitions (4 bytes/metatile: TL,TR,BL,BR)
;   $B7B5-$B839:  Metatile attribute table (palette per metatile)
;   $B83A-$B8C4:  Metatile collision/type table
;   $B8C5-$BAFF:  Padding ($00)
;   $BB00-$BBFF:  Screen data block A (room layout + palette + enemy config)
;   $BC00-$BCFF:  Screen data block B
;   $BD00-$BDFF:  Screen data block C
;   $BE00-$BEFF:  Screen data block D
;   $BF00-$BFFF:  Stage attribute assignment table + padding
;
; PPU buffer format ($0780):
;   $0780 = PPU address high byte
;   $0781 = PPU address low byte
;   $0782 = byte count
;   $0783+ = tile data bytes
;   $07A3-$07C6 = second buffer for paired column/attribute writes
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

task_yield           := $FF21

.segment "BANK0E"

; ===========================================================================
; Entry point table — three JMP vectors at the start of the bank
; ===========================================================================
; $A000: JMP to nametable_init_columns — full-screen column initialization
; $A003: JMP to nametable_advance_row — increment PPU low address by 1 row
; $A006: fall-through to nametable_stream_by_index
; ===========================================================================

        jmp     code_A05F               ; $A000: init nametable columns

        jmp     code_A026               ; $A003: advance PPU addr (next row)

; ===========================================================================
; nametable_stream_by_index — stream nametable data for string index X
; ===========================================================================
; Input:  X = string index (0-13), selects pointer from string_pointer_table_low/string_pointer_table_high table
; Output: one tile written to PPU buffer, or PPU address changed, or stream ended
;
; Uses $B6/$B7 as pointer to the string data, $B8 as stream byte offset.
; On first call ($B8=0), loads PPU address from first 2 bytes of string.
; Subsequent calls read tile data byte-by-byte into the PPU buffer.
;
; String data format:
;   Byte $FE = set new PPU address (next 2 bytes = high, low)
;   Byte $FF = end of string (marks stream complete)
;   Other    = tile ID to write to nametable
; ===========================================================================

        lda     string_pointer_table_low,x ; load string pointer low byte
        sta     $B6                     ; store string ptr low byte
        lda     string_pointer_table_high,x ; load string pointer high byte
        sta     $B7                     ; store string ptr high byte
        lda     #$00                    ; A = 0
        sta     $B8                     ; reset stream offset to 0
        ldy     #$00                    ; Y = 0 (initial read offset)
        lda     ($B6),y                 ; first byte = PPU address high
        sta     $0780                   ; set PPU addr high in buffer
        iny                             ; advance to next byte
        lda     ($B6),y                 ; second byte = PPU address low
        sta     $0781                   ; set PPU addr low in buffer
        iny                             ; advance past header bytes
        sty     $B8                     ; offset now at 2 (first tile data)
        bne     code_A029               ; always taken (Y=2)
; --- advance row: increment PPU address low byte by 1 ---
code_A026:  inc     $0781               ; next nametable row
; --- resume streaming from current offset ---
code_A029:  ldy     $B8                 ; load current stream offset
        cpy     #$FF                    ; stream already finished?
        beq     code_A05E               ; yes — return immediately
        lda     ($B6),y                 ; read next byte from string data
        cmp     #$FF                    ; $FF = end-of-string marker
        beq     code_A05A               ; yes: mark stream finished
        cmp     #$FE                    ; $FE = set-new-PPU-address command
        bne     code_A046               ; not $FE: normal tile byte
        iny                             ; skip past $FE command byte
        lda     ($B6),y                 ; new PPU address high byte
        sta     $0780                   ; update PPU addr high
        iny                             ; advance to low byte
        lda     ($B6),y                 ; new PPU address low byte
        sta     $0781                   ; update PPU addr low
        iny                             ; advance to tile data
; --- write one tile to the PPU buffer ---
code_A046:  lda     ($B6),y             ; tile ID byte
        sta     $0783                   ; store in PPU buffer data area
        iny                             ; advance stream offset
        sty     $B8                     ; save updated offset
        ldy     #$00                    ; Y = 0
        sty     $0782                   ; byte count = 0 (single tile write)
        dey                             ; Y = $FF
        sty     $0784                   ; terminator byte in buffer
        sty     nametable_dirty         ; signal NMI to flush PPU buffer
        rts                             ; return to caller

; --- end of string: mark stream as finished ---
code_A05A:  lda     #$FF                ; end-of-string sentinel
        sta     $B8                     ; $FF = "stream complete" sentinel
code_A05E:  rts                         ; return (stream complete)

; ===========================================================================
; nametable_init_columns — clear 4 pairs of nametable columns
; ===========================================================================
; Writes 4 pairs of 32-byte blank columns to the nametable via the PPU
; buffer. Each pair uses both buffers ($0780 and $07A3) to write two
; columns simultaneously. Calls task_yield ($FF21) between each pair so
; the NMI handler can drain the PPU buffer.
;
; Uses ppu_nametable_addr_low_col0_buf1/ppu_nametable_addr_low_col0plus_buf2 for PPU address low bytes (column offsets $20/$40/$60/$80)
; and ppu_nametable_addr_high_col0_buf1/ppu_nametable_addr_high_col0plus_buf2 for PPU address high bytes ($23 for all = nametable $2300).
; ===========================================================================
code_A05F:  ldx     #$00                ; column pair index (0, 2)
code_A061:  lda     ppu_nametable_addr_high_col0_buf1,x ; PPU addr high for buffer 1
        sta     $0780                   ; set buffer 1 PPU addr high
        lda     ppu_nametable_addr_high_col0plus_buf2,x ; PPU addr high for buffer 2
        sta     $07A3                   ; set buffer 2 PPU addr high
        lda     ppu_nametable_addr_low_col0_buf1,x ; PPU addr low for buffer 1
        sta     $0781                   ; set buffer 1 PPU addr low
        lda     ppu_nametable_addr_low_col0plus_buf2,x ; PPU addr low for buffer 2
        sta     $07A4                   ; set buffer 2 PPU addr low
        ldy     #$1F                    ; 32 bytes per column
        sty     $0782                   ; buffer 1 byte count
        sty     $07A5                   ; buffer 2 byte count
        lda     #$00                    ; fill with blank tiles ($00)
code_A083:  sta     $0783,y             ; clear buffer 1 tile data
        sta     $07A6,y                 ; clear buffer 2 tile data
        dey                             ; next byte index
        bpl     code_A083               ; loop until all 32 done
        lda     #$FF                    ; terminator value
        sta     $07C6                   ; terminator after buffer 2 data
        sta     nametable_dirty         ; signal NMI to flush PPU buffer
        jsr     task_yield              ; task_yield — wait for NMI drain
        inx                             ; X += 2 (next pair)
        inx                             ; advance to next column pair
        cpx     #$04                    ; done all 4 pairs? (2 pairs x 2 cols)
        bne     code_A061               ; loop if pairs remain
        rts                             ; all columns cleared

; ===========================================================================
; Nametable address tables for column initialization
; ===========================================================================
; PPU address low bytes for column pairs (nametable column offsets)
ppu_nametable_addr_low_col0_buf1:  .byte   $20 ; col pair 0, buffer 1: $2320
ppu_nametable_addr_low_col0plus_buf2:  .byte   $40,$60,$80 ; col pair 0 buf2, pair 1 buf1/buf2
; PPU address high bytes for column pairs (all $23 = attribute area)
ppu_nametable_addr_high_col0_buf1:  .byte   $23 ; buffer 1 high byte
ppu_nametable_addr_high_col0plus_buf2:  .byte   $23,$23,$23 ; buffer 2 / pairs 1-2

; ===========================================================================
; String pointer table — low bytes and high bytes for 14 text strings
; ===========================================================================
; Index X selects which string to stream. Used for robot master names
; on the stage select screen and password screen text.
; Each string begins with a 2-byte PPU address, then tile IDs,
; with $FE for address changes and $FF for end-of-string.
; --- pointer low bytes (14 entries) ---
string_pointer_table_low:  .byte   $C1,$EC,$11,$3C,$62,$88,$B1,$D9
        .byte   $03,$3C,$5A,$85,$DC,$01
; --- pointer high bytes (14 entries) ---
string_pointer_table_high:  .byte   $A0,$A0,$A1,$A1,$A1,$A1,$A1,$A1
        .byte   $A2,$A2,$A2,$A2,$A2,$A3

; =============================================================================
; NAMETABLE TEXT DATA — ROBOT MASTER NAMES + PASSWORD SCREEN STRINGS
; =============================================================================
; Each entry is a variable-length string of PPU nametable tile IDs.
;   $FE xx yy = change PPU write address to $xxyy
;   $FF       = end of string
;
; String  0 ($A0C1): "NO. 107 NEEDLE MAN  NOBUEHIKO TAKATSUKA"
; String  1 ($A0EC): "NO. 108 MAGNET MAN  NAGASHIKI KEE"
; String  2 ($A111): "NO. 109 GEMINI MAN  YOSHIHIDE HATTORI"
; String  3 ($A13C): "NO. 200 HARD MAN    KAWAHIKO OGURO"
; String  4 ($A162): "NO. 201 TOP MAN     YASUSHIKI KONFEKI"  (Konkeki)
; String  5 ($A188): "NO. 202 SNAKE MAN   YUHFERO ESHETANE"   (Yuhfero Eshetane)
; String  6 ($A1B1): "NO. 203 SPARK MAN   MEKEHERO SUZUKI"    (Mekehero Suzuki)
; String  7 ($A1D9): "NO. 204 SHADOW MAN  TAKUMENI YOSHEDA"   (Takumeni Yosheda)
; String  8 ($A203): password screen decorative text / scores
; String  9 ($A23C): password screen text 2
; String 10 ($A25A): password screen text 3
; String 11 ($A285): password screen text 4
; String 12 ($A2DC): password screen text 5
; String 13 ($A301): password screen text 6
; =============================================================================
        .byte   $21,$8F
        .byte   $17,$18,$26,$01,$07,$FE,$21,$AF
        .byte   $17,$0E,$0E,$0D,$15,$0E,$25,$16
        .byte   $0A,$17,$FE,$21,$EF,$17,$18,$0B
        .byte   $1E,$11,$12,$14,$18,$FE,$22,$0F
        .byte   $0A,$14,$0A,$1D,$1C,$1E,$14,$0A
        .byte   $FF,$21,$8F,$17,$18,$26,$01,$08
        .byte   $FE,$21,$AF,$16,$0A,$10,$17,$0E
        .byte   $1D,$25,$16,$0A,$17,$FE,$21,$EF
        .byte   $17,$0A,$10,$0A,$1C,$11,$12,$FE
        .byte   $22,$0F,$14,$12,$12,$FF,$21,$8F
        .byte   $17,$18,$26,$01,$09,$FE,$21,$AF
        .byte   $10,$0E,$16,$12,$17,$12,$25,$16
        .byte   $0A,$17,$FE,$21,$EF,$22,$18,$1C
        .byte   $11,$12,$11,$12,$1D,$18,$FE,$22
        .byte   $0F,$11,$0A,$1D,$1D,$18,$1B,$12
        .byte   $FF,$21,$8F,$17,$18,$26,$02,$00
        .byte   $FE,$21,$AF,$11,$0A,$1B,$0D,$25
        .byte   $16,$0A,$17,$FE,$21,$EF,$14,$0A
        .byte   $23,$1E,$11,$12,$14,$18,$FE,$22
        .byte   $0F,$18,$10,$1E,$1B,$18,$FF,$21
        .byte   $8F,$17,$18,$26,$02,$01,$FE,$21
        .byte   $AF,$1D,$18,$19,$25,$16,$0A,$17
        .byte   $FE,$21,$EF,$22,$0A,$1C,$1E,$1C
        .byte   $11,$12,$FE,$22,$0F,$14,$18,$17
        .byte   $13,$12,$14,$12,$FF,$21,$8F,$17
        .byte   $18,$26,$02,$02,$FE,$21,$AF,$1C
        .byte   $17,$0A,$14,$0E,$25,$16,$0A,$17
        .byte   $FE,$21,$EF,$22,$1E,$11,$13,$12
        .byte   $1B,$18,$FE,$22,$0F,$12,$1C,$11
        .byte   $12,$1D,$0A,$17,$12,$FF,$21,$8F
        .byte   $17,$18,$26,$02,$03,$FE,$21,$AF
        .byte   $1C,$19,$0A,$1B,$14,$25,$16,$0A
        .byte   $17,$FE,$21,$EF,$16,$12,$14,$12
        .byte   $11,$12,$1B,$18,$FE,$22,$0F,$1C
        .byte   $1E,$23,$1E,$14,$12,$FF,$21,$8F
        .byte   $17,$18,$26,$02,$04,$FE,$21,$AF
        .byte   $1C,$11,$0A,$0D,$18,$20,$25,$16
        .byte   $0A,$17,$FE,$21,$EF,$1D,$0A,$14
        .byte   $1E,$16,$12,$17,$0E,$FE,$22,$0F
        .byte   $22,$18,$1C,$11,$12,$0D,$0A,$FF
        .byte   $23,$26,$19,$12,$00,$18,$19,$28
        .byte   $00,$1C,$13,$11,$12,$1E,$00,$0B
        .byte   $10,$1E,$0F,$1C,$00,$21,$0F,$FE
        .byte   $23,$46,$1C,$0F,$0D,$0F,$13,$20
        .byte   $0F,$0E,$00,$1E,$12,$0F,$00,$16
        .byte   $0B,$1D,$1E,$FE,$23,$66,$0F,$16
        .byte   $0F,$17,$0F,$18,$1E,$26,$26,$26
        .byte   $FF,$23,$26,$21,$13,$16,$0F,$23
        .byte   $00,$1C,$0B,$18,$00,$19,$10,$10
        .byte   $00,$21,$13,$1E,$12,$FE,$23,$46
        .byte   $11,$0B,$17,$17,$0B,$28,$FF,$23
        .byte   $66,$21,$12,$0F,$1C,$0F,$27,$1D
        .byte   $00,$0E,$25,$26,$00,$21,$13,$16
        .byte   $0F,$23,$29,$26,$26,$26,$FE,$23
        .byte   $86,$19,$12,$00,$18,$19,$2B,$00
        .byte   $1E,$19,$19,$00,$16,$0B,$1E,$0F
        .byte   $26,$FF,$23,$26,$17,$0F,$11,$0B
        .byte   $17,$0B,$18,$2B,$00,$23,$19,$1F
        .byte   $27,$20,$0F,$00,$1C,$0F,$11,$0B
        .byte   $13,$18,$0F,$0E,$FE,$23,$46,$0D
        .byte   $19,$18,$1D,$0D,$13,$19,$1F,$1D
        .byte   $18,$0F,$1D,$1D,$26,$00,$13,$00
        .byte   $10,$19,$1F,$18,$0E,$FE,$23,$66
        .byte   $23,$19,$1F,$00,$16,$23,$13,$18
        .byte   $11,$00,$12,$0F,$1C,$0F,$00,$21
        .byte   $12,$0F,$18,$00,$13,$FE,$23,$86
        .byte   $0B,$1C,$1C,$13,$20,$0F,$0E,$26
        .byte   $FF,$23,$26,$13,$00,$21,$19,$18
        .byte   $0E,$0F,$1C,$00,$21,$12,$19,$00
        .byte   $0C,$1C,$19,$1F,$11,$12,$1E,$00
        .byte   $23,$19,$1F,$FE,$23,$46,$12,$0F
        .byte   $1C,$0F,$26,$26,$26,$FF,$23,$26
        .byte   $1E,$12,$13,$1D,$00,$21,$12,$13
        .byte   $1D,$1E,$16,$0F,$26,$26,$26,$00
        .byte   $13,$1E,$00,$17,$1F,$1D,$1E,$FE
        .byte   $23,$46,$12,$0B,$20,$0F,$00,$0C
        .byte   $0F,$0F,$18,$00,$1A,$1C,$19,$1E
        .byte   $19,$00,$17,$0B,$18,$28,$FF ; end of last text string

; =============================================================================
; WILY 6 (STAGE $16) — STAGE DATA ($A32D-$BFFF)
; =============================================================================
; Complete level data for Wily fortress stage 6 (the final Wily stage).
; Loaded when stage_id = $16 via the stage_to_bank mapping table.
;
; Standard stage data layout (see bank08.asm header for format details):
;   $A32D-$A9FC:  2-bit encoded screen nametable column data
;   $AA00-$AA5C:  Screen layout table (20 bytes/screen)
;   $AA5D-$AA7F:  Room config / screen pointer table
;   $AA80-$AA9F:  BG palette data (4 palettes x 4 NES colors)
;   $AB00+:       Enemy placement — screen numbers ($FF-terminated)
;   $AC00+:       Enemy placement — X positions ($FF-terminated)
;   $AD00+:       Enemy placement — Y positions ($FF-terminated)
;   $AE00+:       Enemy placement — global enemy type IDs ($FF-terminated)
;   $AF05+:       Metatile column definitions (8 metatile IDs per column)
;   $B700+:       Metatile CHR tile definitions (4 bytes/metatile: TL,TR,BL,BR)
;   $B700+$195:   Metatile attribute table (palette assignment per metatile)
;   $B700+$1A6:   Metatile collision/type table
;   $BB00-$BEFF:  Screen data blocks (4 x $100 bytes: A, B, C, D)
;   $BF00-$BFFF:  Stage attribute assignment table + padding
;
; Large zero-filled regions represent unused screen slots or padding to
; align data tables to $100-byte boundaries expected by the engine.
; =============================================================================

; ===========================================================================
; 2-bit encoded screen nametable column data ($A32D-$A9FC)
; ===========================================================================
; Compressed nametable tile patterns for each screen column. Each pair
; of bytes encodes 4 tiles via 2-bit indices into a metatile set.
; Decompressed by the stage loading routine in the fixed bank.
; ===========================================================================
        .byte   $B2
        .byte   $FF,$AA,$FF,$AF,$FF,$AB,$FF,$EF
        .byte   $BF,$EF,$FF,$FE,$FF,$AA,$DE,$A2
        .byte   $FB,$AE,$7E,$8A,$FF,$AB,$FE,$AB
        .byte   $FF,$EE,$FF,$E3,$FF,$AB,$FF,$BA
        .byte   $6F,$AE,$FF,$EB,$EF,$FB,$FF,$BA
        .byte   $FF,$FB,$FF,$BA,$FF,$92,$FC,$AA
        .byte   $DF,$82,$DF,$2A,$32,$CA,$E7,$A2
        .byte   $FF,$AA,$72,$4A,$7F,$B8,$EF,$AE
        .byte   $FF,$8A,$FF,$AA,$BE,$AB,$FF,$CE
        .byte   $BF,$AA,$FF,$EE,$FF,$EB,$DF,$AA
        .byte   $F5,$AA,$EF,$AA,$FF,$AA,$FF,$9A
        .byte   $FE,$A9,$FF,$EA,$FE,$AA,$3F,$BB
        .byte   $FF,$EE,$FF,$BF,$FF,$FE,$FF,$EF
        .byte   $FF,$BB,$FF,$AE,$FF,$A2,$9C,$B6
        .byte   $FF,$82,$DD,$AC,$DD,$AA,$FB,$B8
        .byte   $7B,$24,$F7,$BA,$DF,$AB,$FF,$AA
        .byte   $F7,$BA,$FF,$EA,$FD,$AB,$FF,$EE
        .byte   $DF,$AF,$FF,$FA,$FF,$AA,$BB,$2A
        .byte   $EF,$0A,$FF,$EE,$FF,$3C,$D7,$EA
        .byte   $FE,$AC,$7F,$EA,$FF,$EA,$EF,$BE
        .byte   $FF,$AF,$FF,$EF,$FF,$B9,$FF,$FF
        .byte   $FB,$BB,$FF,$BA,$FF,$AA,$FF,$AA
        .byte   $26,$B6,$FB,$EA,$BE,$AA,$FB,$AA
        .byte   $F9,$84,$F3,$A6,$6F,$2A,$37,$82
        .byte   $AF,$B3,$7B,$AE,$FF,$82,$5F,$8C
        .byte   $5D,$EA,$F9,$AA,$BF,$34,$50,$18
        .byte   $54,$BB,$55,$B2,$71,$97,$05,$FF
        .byte   $55,$F9,$5D,$7F,$57,$FF,$75,$FF
        .byte   $D5,$FF,$5D,$FF,$57,$FE,$7F,$FF
        .byte   $57,$FF,$FF,$FF,$55,$86,$55,$60
        .byte   $51,$5F,$10,$D5,$14,$CC,$00,$8B
        .byte   $51,$54,$54,$F5,$18,$FE,$44,$CF
        .byte   $74,$5C,$15,$FA,$55,$DF,$55,$FF
        .byte   $F5,$FF,$C5,$EF,$7D,$13,$01,$A5
        .byte   $00,$BB,$50,$37,$05,$FD,$45,$EF
        .byte   $15,$FA,$D4,$FD,$55,$7F,$55,$FD
        .byte   $1D,$FF,$D5,$F5,$FF,$FF,$F5,$FF
        .byte   $5F,$FF,$FD,$FF,$DD,$44,$10,$44
        .byte   $14,$C0,$40,$05,$45,$20,$00,$0C
        .byte   $10,$8A,$50,$AB,$11,$76,$10,$F1
        .byte   $14,$1E,$08,$E4,$45,$FF,$31,$FF
        .byte   $57,$7F,$55,$FF,$D5,$94,$00,$6E
        .byte   $11,$EA,$10,$93,$45,$F9,$48,$FF
        .byte   $5C,$EB,$75,$FF,$75,$D7,$55,$7F
        .byte   $D5,$EF,$55,$FF,$DF,$FD,$DD,$EF
        .byte   $D5,$F7,$D1,$FE,$F5,$0C,$40,$A9
        .byte   $50,$11,$41,$24,$41,$01,$04,$05
        .byte   $51,$E8,$45,$94,$54,$DF,$45,$10
        .byte   $45,$FF,$55,$BF,$55,$FE,$43,$BF
        .byte   $7D,$FF,$55,$FF,$55,$24,$04,$3E
        .byte   $11,$3A,$11,$16,$10,$C4,$41,$94
        .byte   $55,$FE,$54,$FC,$54,$EF,$55,$EF
        .byte   $56,$FF,$55,$9F,$71,$FF,$5D,$F7
        .byte   $FD,$FF,$F5,$FF,$DF,$37,$04,$61
        .byte   $45,$30,$11,$3D,$15,$61,$00,$3E
        .byte   $05,$50,$00,$80,$04,$20,$04,$82
        .byte   $10,$38,$51,$B5,$00,$33,$55,$BF
        .byte   $55,$CA,$35,$F8,$5D,$68,$57,$B5
        .byte   $50,$DD,$49,$7F,$51,$5D,$55,$FF
        .byte   $D5,$FD,$D5,$7F,$55,$FF,$FD,$FF
        .byte   $57,$FF,$DD,$FF,$DD,$FF,$EF,$FF
        .byte   $77,$FF,$FB,$FF,$FF,$2D,$41,$45
        .byte   $50,$89,$00,$E6,$00,$69,$00,$62
        .byte   $45,$99,$00,$DA,$05,$DE,$14,$FF
        .byte   $51,$FF,$3C,$FF,$55,$FF,$57,$FF
        .byte   $65,$FF,$D5,$FF,$57,$77,$14,$5F
        .byte   $50,$F0,$14,$D6,$41,$B6,$55,$CF
        .byte   $45,$FF,$45,$FB,$54,$7F,$5D,$FF
        .byte   $57,$FB,$55,$FF,$DD,$F7,$F5,$FF
        .byte   $57,$FF,$95,$FF,$77,$BA,$40,$0F
        .byte   $00,$A2,$40,$13,$00,$30,$00,$75
        .byte   $14,$E2,$15,$09,$40,$A8,$14,$B8
        .byte   $05,$F4,$00,$B4,$54,$FF,$55,$BF
        .byte   $91,$FF,$F7,$FF,$55,$50,$10,$83
        .byte   $45,$49,$54,$E1,$40,$71,$51,$FB
        .byte   $44,$FB,$54,$FB,$5D,$DF,$D5,$FF
        .byte   $05,$FF,$D5,$FD,$FD,$FF,$F7,$FF
        .byte   $75,$FF,$F7,$FB,$57,$C4,$01,$60
        .byte   $05,$EC,$05,$A0,$54,$62,$11,$D0
        .byte   $14,$E0,$54,$87,$40,$F7,$55,$B7
        .byte   $54,$FB,$BD,$FF,$55,$FF,$5D,$7F
        .byte   $5D,$FF,$55,$FF,$FD,$84,$54,$AB
        .byte   $44,$3F,$41,$CF,$04,$7E,$D5,$BD
        .byte   $05,$5B,$55,$CF,$45,$DF,$55,$EB
        .byte   $57,$FF,$55,$FF,$5D,$FF,$75,$FD
        .byte   $D7,$FD,$D7,$FF,$D5,$AB,$14,$29
        .byte   $04,$0E,$10,$A7,$44,$6A,$14,$41
        .byte   $00,$90,$40,$C1,$10,$48,$14,$31
        .byte   $50,$7C,$45,$A9,$00,$C1,$41,$F6
        .byte   $14,$7B,$50,$FF,$C5,$A8,$04,$72
        .byte   $11,$EE,$4D,$D5,$45,$D8,$7C,$DB
        .byte   $55,$EF,$54,$FF,$F5,$EF,$75,$DF
        .byte   $F5,$FF,$F7,$EF,$D7,$FB,$F5,$FF
        .byte   $5D,$FF,$F5,$FF,$FF,$8B,$10,$91
        .byte   $50,$C9,$10,$2F,$40,$4D,$05,$96
        .byte   $14,$2E,$55,$9C,$51,$EA,$44,$EC
        .byte   $05,$FE,$15,$57,$7D,$FF,$55,$FF
        .byte   $7D,$FB,$F7,$FF,$F5,$7C,$50,$5C
        .byte   $00,$D2,$14,$FC,$41,$C3,$40,$F7
        .byte   $D1,$DF,$54,$BF,$15,$F3,$D6,$E6
        .byte   $7F,$FF,$5F,$FF,$FF,$EF,$7F,$FF
        .byte   $F7,$7F,$F5,$FD,$D9,$20,$45,$13
        .byte   $50,$04,$01,$00,$44,$02,$54,$24
        .byte   $05,$30,$05,$F9,$04,$E9,$54,$B3
        .byte   $40,$FF,$44,$6F,$49,$74,$55,$EF
        .byte   $55,$FF,$55,$FE,$F5,$D5,$01,$10
        .byte   $11,$5D,$45,$CD,$90,$BE,$D5,$E0
        .byte   $55,$DF,$51,$F9,$55,$FF,$D7,$7F
        .byte   $55,$FF,$75,$FF,$FF,$FF,$F5,$FF
        .byte   $57,$FF,$D5,$FF,$FF,$42,$41,$2F
        .byte   $15,$83,$50,$CA,$40,$7D,$00,$0E
        .byte   $10,$3C,$15,$E3,$45,$6F,$15,$C9
        .byte   $45,$FB,$54,$BD,$DD,$BE,$57,$FF
        .byte   $D3,$FF,$5F,$7F,$7D,$6A,$00,$10
        .byte   $14,$6B,$05,$C2,$51,$31,$15,$91
        .byte   $55,$7E,$11,$FF,$15,$D3,$55,$FF
        .byte   $55,$B3,$D7,$FD,$5D,$EF,$2D,$FF
        .byte   $5D,$FF,$57,$FF,$75,$3D,$00,$2B
        .byte   $01,$14,$00,$DC,$00,$09,$00,$06
        .byte   $10,$11,$10,$20,$50,$97,$10,$02
        .byte   $10,$34,$55,$32,$01,$4A,$40,$4A
        .byte   $55,$FD,$50,$BF,$D7,$C9,$04,$97
        .byte   $40,$E5,$05,$96,$50,$FE,$55,$BF
        .byte   $55,$DD,$C5,$FF,$55,$FA,$D7,$FB
        .byte   $D5,$FF,$F5,$FB,$55,$FF,$FD,$EF
        .byte   $DD,$FF,$55,$FF,$F5,$EA,$11,$04
        .byte   $51,$82,$14,$81,$04,$9E,$00,$72
        .byte   $51,$03,$04,$DF,$11,$FF,$51,$FD
        .byte   $54,$FF,$55,$FF,$55,$FF,$44,$F7
        .byte   $F5,$7F,$65,$FF,$D5,$7C,$44,$49
        .byte   $15,$A3,$04,$A4,$04,$FD,$44,$FB
        .byte   $15,$FF,$10,$DF,$55,$FE,$64,$F1
        .byte   $7E,$FF,$57,$FF,$F7,$FF,$D5,$FF
        .byte   $77,$FF,$7F,$FF,$D4,$9F,$45,$20
        .byte   $11,$A2,$14,$D4,$10,$41,$40,$DB
        .byte   $04,$C8,$40,$00,$10,$6D,$51,$61
        .byte   $10,$89,$58,$77,$54,$7F,$15,$BF
        .byte   $55,$FF,$54,$DF,$37,$88,$54,$3E
        .byte   $40,$3C,$14,$70,$04,$DF,$10,$F7
        .byte   $55,$FB,$55,$FE,$15,$FD,$F5,$FB
        .byte   $79,$FF,$55,$FF,$74,$FF,$77,$FF
        .byte   $59,$FF,$DD,$FF,$FD,$CE,$50,$21
        .byte   $01,$89,$01,$03,$01,$11,$00,$CB
        .byte   $14,$06,$41,$6E,$11,$C3,$54,$F7
        .byte   $40,$EB,$C9,$58,$55,$FF,$55,$FF
        .byte   $51,$FF,$35,$FF,$55,$6E,$15,$25
        .byte   $14,$A7,$41,$F5,$54,$16,$10,$4B
        .byte   $11,$DF,$55,$FE,$45,$FD,$55,$FE
        .byte   $51,$7E,$74,$DB,$75,$FF,$D5,$FB
        .byte   $55,$F7,$DF,$FF,$F5,$56,$44,$80
        .byte   $04,$43,$00,$64,$10,$A1,$54,$4A
        .byte   $04,$05,$00,$25,$00,$E5,$00,$28
        .byte   $C0,$08,$40,$65,$44,$15,$11,$5E
        .byte   $46,$BF,$55,$FF,$76,$20,$B5,$8A
        .byte   $61,$0A,$67,$00,$9A,$00,$20,$80
        .byte   $80,$00,$00,$00,$04,$00,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$A6,$65,$2A
        .byte   $0F,$20,$80,$A0,$36,$20,$EC,$8A
        .byte   $82,$02,$2B,$92,$4A,$20,$48,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $40,$00,$00,$00,$00,$22,$DA,$26
        .byte   $A6,$28,$24,$00,$91,$0A,$10,$00
        .byte   $20,$20,$00,$00,$08,$00,$82,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$AA,$0C,$20
        .byte   $C6,$82,$25,$2A,$9E,$8A,$9A,$AA
        .byte   $5A,$AA,$1C,$20,$A6,$A0,$7E,$0A
        .byte   $33,$06,$01,$20,$00,$0A,$C0,$00
        .byte   $40,$00,$00,$00,$00,$0A,$35,$80
        .byte   $09,$88,$E8,$80,$20,$02,$21,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$02,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$A9,$08
        .byte   $6F,$22,$E8,$A8,$A1,$2B,$3C,$88
        .byte   $E1,$82,$AB,$A0,$E8,$A8,$55,$00
        .byte   $A0,$00,$82,$20,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$00,$AA,$64,$A8
        .byte   $2E,$8A,$52,$82,$7B,$A8,$43,$08
        .byte   $A4,$A2,$43,$02,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$AA,$A5,$AA
        .byte   $FE,$AA,$DF,$08,$B0,$2A,$EF,$8A
        .byte   $58,$82,$EF,$A0,$BF,$08,$6F,$92
        .byte   $AF,$08,$25,$80,$98,$AA,$96,$00
        .byte   $25,$22,$0E,$00,$11,$A0,$16,$28
        .byte   $24,$20,$6E,$00,$08,$20,$8D,$00
        .byte   $00,$02,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$82,$E4,$A8
        .byte   $A7,$80,$87,$A2,$20,$A2,$3A,$8A
        .byte   $8C,$A8,$94,$82,$05,$00,$02,$00
        .byte   $80,$08,$00,$00,$00,$00,$00,$00
        .byte   $01,$00,$00,$00,$00,$22,$08,$AA
        .byte   $04,$08,$86,$20,$80,$00,$32,$20
        .byte   $59,$00,$04,$02,$00,$08,$00,$00
        .byte   $00,$00,$21,$00,$01,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$A0,$51,$02
        .byte   $EC,$B2,$36,$2A,$88,$0A,$FB,$22
        .byte   $B7,$80,$EB,$28,$BA,$A0,$F7,$20
        .byte   $ED,$08,$08,$AA,$04,$02,$20,$00
        .byte   $02,$00,$01,$02,$00,$23,$9F,$AA
        .byte   $A1,$28,$0D,$80,$80,$08,$08,$00
        .byte   $01,$00,$0A,$02,$00,$00,$08,$00
        .byte   $01,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$82,$95,$A0
        .byte   $46,$82,$03,$A0,$F7,$A8,$3B,$40
        .byte   $AE,$AA,$4B,$22,$12,$02,$25,$28
        .byte   $09,$02,$BE,$00,$08,$00,$00,$00
        .byte   $80,$80,$01,$00,$00,$A8,$33,$02
        .byte   $9C,$2A,$4A,$20,$CC,$08,$85,$20
        .byte   $85,$A0,$A5,$00,$02,$00,$20,$02
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $10,$00,$40,$00,$00,$28,$2A,$20
        .byte   $B2,$32,$6E,$0A,$7A,$A0,$16,$A2
        .byte   $E7,$88,$F0,$AA,$8E,$38,$10,$2A
        .byte   $BE,$AA,$BF,$00,$20,$2A,$00,$20
; ===========================================================================
; Screen layout table + room config ($AA00-$AA7F)
; ===========================================================================
; $AA00: 20 bytes per screen (16 metatile column IDs + 4 connection bytes).
; $AA5D: Room config / screen pointer table entries.
; Terminated by $FF. Followed by BG palette data at $AA80.
; ===========================================================================
        .byte   $20,$20,$04,$00,$00,$0B,$0C,$0D
        .byte   $0E,$0F,$10,$11,$12,$13,$14,$15
        .byte   $16,$17,$FF,$00,$40,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$80,$00,$00,$A8,$F2,$2A
        .byte   $CD,$02,$4D,$A8,$2E,$08,$06,$80
        .byte   $8E,$80,$40,$00,$20,$0B,$78,$02
        .byte   $01,$88,$00,$00,$09,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$80,$A3
        .byte   $40,$61,$80,$80,$A0,$20,$FF,$00
        .byte   $14,$00,$51,$00,$08,$20,$08,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$06,$00,$06
        .byte   $00,$08,$00,$17,$00,$0B,$00,$00
        .byte   $00,$00,$00,$19,$00,$00,$00,$FF
; --- BG palette data ($AA80): 4 palettes x 4 NES colors = 16 bytes ---
        .byte   $87,$00,$0B,$08,$BB,$00,$06,$02
        .byte   $08,$00,$10,$00,$00,$64,$62,$0F ; $AA7D...$AA8B
        .byte   $37,$27,$17,$0F,$0A,$09,$01,$0F ; BG palette 0-1
        .byte   $14,$04,$01,$0F,$30,$00,$07,$00 ; BG palette 2-3
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$BF,$28
        .byte   $A7,$00,$CD,$0A,$13,$8A,$99,$08
        .byte   $53,$20,$D4,$22,$DA,$80,$B4,$02
        .byte   $08,$80,$80,$00,$01,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$20,$DA,$02
        .byte   $B3,$20,$A3,$88,$56,$02,$5A,$A0
        .byte   $80,$00,$D0,$0A,$00,$A8,$01,$00
        .byte   $05,$20,$08,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$02,$DC,$CC
        .byte   $DE,$98,$91,$9A,$B9,$8E,$BF,$A8
        .byte   $DD,$02,$97,$A2,$E8,$04,$09,$A8
        .byte   $E7,$0A,$DD,$88,$91,$07,$FF,$FF
; ===========================================================================
; Enemy placement tables ($AB00-$AEFF)
; ===========================================================================
; Four parallel arrays, each $FF-terminated, indexed together:
;   $AB00: screen number for each enemy spawn
;   $AC00: X pixel position within the screen
;   $AD00: Y pixel position within the screen
;   $AE00: global enemy type ID (indexes entity AI table)
; ===========================================================================
; --- enemy screen numbers ($AB00) ---
        .byte   $02,$20,$00,$00,$20,$00,$01,$01
        .byte   $01,$01,$01,$02,$03,$03,$03,$03
        .byte   $04,$04,$04,$05,$05,$05,$05,$05
        .byte   $05,$05,$05,$06,$07,$08,$09,$09
        .byte   $09,$09,$09,$09,$09,$0A,$0A,$0A
        .byte   $0A,$0A,$0A,$0A,$0A,$0A,$0B,$0B
        .byte   $0B,$0C,$FF,$28,$12,$20,$05,$A8
        .byte   $88,$02,$20,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$80,$00,$0A,$DA,$0A
        .byte   $56,$02,$41,$A8,$09,$00,$D1,$88
        .byte   $90,$20,$02,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $24,$00,$00,$00,$00,$2A,$C0,$12
        .byte   $EB,$20,$F5,$8B,$FE,$6A,$8F,$82
        .byte   $78,$08,$29,$80,$C3,$22,$F8,$00
        .byte   $8C,$02,$06,$88,$C8,$00,$01,$00
        .byte   $30,$00,$04,$00,$00,$12,$B7,$A0
        .byte   $AC,$02,$54,$A2,$8A,$00,$80,$00
        .byte   $00,$22,$01,$00,$00,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$8A,$81,$00
        .byte   $46,$8A,$CE,$AA,$B3,$04,$EF,$AA
        .byte   $92,$A8,$3E,$22,$23,$00,$01,$08
        .byte   $5E,$20,$A0,$00,$90,$20,$00,$00
        .byte   $00,$00,$00,$00,$00,$17,$2D,$82
        .byte   $C8,$28,$B8,$08,$14,$2A,$01,$0A
        .byte   $18,$02,$01,$00,$00,$00,$0A,$00
        .byte   $00,$00,$00,$00,$04,$00,$89,$00
        .byte   $41,$00,$00,$00,$00,$BA,$6C,$08
        .byte   $8B,$0C,$94,$68,$F8,$0A,$57,$AA
        .byte   $81,$0A,$6F,$28,$0F,$82,$0A,$02
        .byte   $46,$08,$FB,$28,$D4,$0A,$68,$20
        .byte   $51,$20,$00,$00,$80,$6C,$30,$A8
; --- enemy X positions ($AC00) ---
        .byte   $A8,$B8,$B8,$B0,$70,$90,$D0,$F8
        .byte   $68,$70,$A8,$18,$28,$38,$48,$58
        .byte   $68,$70,$88,$48,$C8,$B8,$60,$E0
        .byte   $20,$A0,$30,$70,$B0,$60,$E0,$48
        .byte   $90,$D8,$28,$50,$A0,$F0,$90,$B0
        .byte   $D0,$80,$FF,$10,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$00,$00,$60,$00,$10
        .byte   $40,$00,$04,$00,$40,$00,$00,$02
        .byte   $00,$00,$40,$00,$00,$00,$00,$01
        .byte   $01,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$0A,$00,$61
        .byte   $50,$91,$04,$00,$00,$04,$00,$08
        .byte   $04,$48,$00,$12,$00,$33,$41,$01
        .byte   $01,$88,$04,$00,$50,$40,$00,$12
        .byte   $00,$00,$00,$00,$00,$00,$00,$21
        .byte   $04,$00,$00,$01,$00,$00,$00,$02
        .byte   $00,$00,$00,$00,$01,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$01,$00,$00
        .byte   $00,$00,$40,$00,$00,$68,$10,$C8
        .byte   $00,$04,$00,$40,$00,$20,$40,$00
        .byte   $15,$04,$01,$40,$50,$60,$40,$01
        .byte   $00,$40,$00,$00,$01,$00,$00,$00
        .byte   $01,$00,$00,$00,$01,$00,$10,$80
        .byte   $41,$04,$40,$14,$00,$00,$01,$00
        .byte   $00,$28,$00,$00,$05,$00,$00,$01
        .byte   $00,$04,$00,$00,$01,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$04,$10,$A0
        .byte   $11,$1A,$01,$D1,$40,$E0,$10,$28
        .byte   $01,$97,$11,$80,$00,$D1,$00,$10
        .byte   $00,$04,$40,$01,$00,$00,$40,$06
        .byte   $01,$0A,$00,$00,$00,$54,$64,$58
; --- enemy Y positions ($AD00) ---
        .byte   $88,$50,$80,$B8,$B0,$74,$94,$48
        .byte   $B4,$30,$94,$54,$CC,$CC,$CC,$CC
        .byte   $A8,$70,$34,$50,$A8,$A8,$98,$98
        .byte   $68,$68,$38,$38,$38,$98,$98,$68
        .byte   $68,$68,$38,$38,$38,$38,$30,$58
        .byte   $58,$24,$FF,$10,$00,$01,$00,$02
        .byte   $00,$22,$00,$00,$00,$00,$00,$00
        .byte   $01,$00,$00,$00,$00,$09,$00,$00
        .byte   $10,$C0,$00,$41,$10,$02,$00,$00
        .byte   $04,$40,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$00,$0E
        .byte   $10,$20,$04,$62,$40,$71,$44,$30
        .byte   $50,$48,$40,$81,$01,$02,$00,$10
        .byte   $00,$20,$00,$80,$00,$00,$00,$00
        .byte   $10,$08,$00,$08,$00,$00,$00,$C8
        .byte   $00,$02,$00,$00,$00,$00,$10,$21
        .byte   $10,$00,$00,$08,$00,$00,$00,$80
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$00,$00,$00,$00,$01
        .byte   $00,$4A,$00,$04,$11,$92,$04,$1D
        .byte   $00,$00,$00,$42,$04,$28,$01,$04
        .byte   $00,$20,$00,$08,$10,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$40,$10,$40
        .byte   $04,$13,$00,$04,$00,$02,$00,$00
        .byte   $00,$00,$00,$04,$00,$00,$01,$08
        .byte   $00,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$01,$00
        .byte   $00,$A8,$40,$82,$04,$09,$40,$51
        .byte   $00,$10,$41,$03,$00,$44,$01,$21
        .byte   $01,$88,$40,$E0,$04,$02,$11,$00
        .byte   $00,$00,$00,$10,$00,$02,$02,$52
; --- enemy global type IDs ($AE00) ---
        .byte   $52,$5D,$5D,$54,$39,$37,$37,$55
        .byte   $37,$39,$37,$37,$51,$51,$51,$51
        .byte   $52,$39,$37,$03,$13,$13,$8B,$8B
        .byte   $8B,$8B,$8C,$8C,$8C,$8B,$8B,$8D
        .byte   $8C,$8D,$8D,$8C,$8B,$8C,$5D,$56
        .byte   $56,$33,$FF,$10,$00,$00,$00,$01
        .byte   $00,$04,$00,$00,$00,$48,$00,$00
        .byte   $00,$00,$00,$00,$00,$01,$00,$08
        .byte   $10,$00,$00,$00,$10,$00,$04,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $01,$CA,$54,$00,$00,$14,$00,$05
        .byte   $00,$40,$00,$81,$41,$0C,$10,$61
        .byte   $00,$00,$01,$00,$00,$42,$15,$00
        .byte   $10,$00,$00,$01,$00,$10,$00,$03
        .byte   $00,$20,$00,$00,$00,$00,$04,$08
        .byte   $00,$00,$11,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$9A,$00,$88
        .byte   $40,$10,$01,$00,$00,$40,$04,$08
        .byte   $50,$00,$01,$41,$40,$01,$00,$08
        .byte   $40,$00,$00,$04,$44,$00,$40,$00
        .byte   $00,$00,$00,$00,$00,$10,$00,$0A
        .byte   $01,$01,$00,$08,$14,$0D,$00,$01
        .byte   $00,$16,$00,$02,$00,$20,$00,$00
        .byte   $00,$00,$00,$00,$40,$00,$00,$00
        .byte   $00,$02,$00,$40,$00,$44,$00,$08
        .byte   $11,$02,$00,$01,$01,$03,$00,$A6
        .byte   $40,$00,$44,$25,$00,$48,$00,$15
        .byte   $04,$00,$01,$00,$00,$08,$00,$00
        .byte   $10,$00,$00,$40,$00,$00,$01,$00
; ===========================================================================
; Metatile column definitions ($AF00-$B6FF)
; ===========================================================================
; Each column is 8 bytes: one metatile index per row (8 rows per screen).
; The metatile IDs reference the metatile CHR definition table at $B700.
; Column IDs are referenced by the screen layout table at $AA00.
; ===========================================================================
        .byte   $00,$00,$02,$00,$00,$03,$00,$04
        .byte   $05,$06,$07,$01,$08,$09,$0A,$0B
        .byte   $0C,$0D,$0E,$0F,$10,$11,$01,$12
        .byte   $13,$14,$15,$16,$17,$18,$19,$1A
        .byte   $1B,$1C,$1D,$1E,$00,$00,$1F,$20
        .byte   $21,$22,$23,$24,$25,$26,$27,$28
        .byte   $29,$2A,$2B,$2C,$2D,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$03,$00,$2E
        .byte   $00,$2F,$00,$30,$00,$31,$32,$33
        .byte   $00,$34,$35,$36,$37,$38,$39,$3A
        .byte   $3B,$3C,$3D,$3E,$00,$3F,$2E,$40
        .byte   $32,$00,$41,$42,$00,$43,$44,$45
        .byte   $46,$47,$00,$00,$00,$02,$0F,$48
        .byte   $33,$00,$03,$00,$49,$4A,$00,$00
        .byte   $4B,$00,$4C,$4D,$00,$4E,$00,$00
        .byte   $00,$4F,$3B,$00,$00,$50,$51,$52
        .byte   $50,$51,$50,$53,$54,$55,$56,$52
        .byte   $55,$56,$55,$57,$58,$59,$5A,$5B
        .byte   $5C,$59,$59,$59,$5D,$5E,$5F,$60
        .byte   $61,$51,$62,$51,$63,$64,$65,$66
        .byte   $61,$67,$51,$51,$68,$69,$6A,$6B
        .byte   $6C,$6D,$6E,$6F,$70,$00,$00,$00
; --- zero padding: unused metatile column slots ($AFB3-$B6FF) ---
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
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; ===========================================================================
; Metatile CHR tile definitions ($B700)
; ===========================================================================
; 4 bytes per metatile: top-left, top-right, bottom-left, bottom-right
; CHR tile IDs. Each metatile is a 16x16 pixel block composed of four
; 8x8 pattern table tiles.
; ===========================================================================
        .byte   $00,$00,$00,$00,$A4,$A0,$00,$A0
        .byte   $00,$00,$A0,$A1,$00,$A3,$00,$00
        .byte   $A1,$A4,$00,$00,$09,$02,$03,$0A
        .byte   $0B,$A4,$00,$00,$A1,$A2,$00,$00
        .byte   $A3,$00,$00,$A3,$A2,$A4,$00,$00
        .byte   $A6,$00,$00,$00,$01,$00,$10,$00
        .byte   $18,$11,$12,$19,$1A,$20,$A4,$28
        .byte   $9F,$00,$A3,$A4,$AB,$A5,$AB,$00
        .byte   $A1,$A9,$A3,$00,$00,$00,$05,$0C
        .byte   $0D,$06,$07,$0E,$0F,$40,$41,$48
        .byte   $49,$42,$00,$4A,$4B,$43,$00,$21
        .byte   $A4,$00,$00,$A2,$A0,$00,$AB,$A4
        .byte   $00,$00,$13,$00,$1B,$14,$15,$1C
        .byte   $1D,$16,$17,$1E,$1F,$50,$51,$58
        .byte   $59,$52,$53,$5A,$4B,$29,$00,$5B
        .byte   $00,$A4,$23,$2A,$2B,$24,$25,$2C
        .byte   $2D,$26,$27,$2E,$2F,$60,$61,$68
        .byte   $69,$44,$45,$4C,$4D,$46,$47,$4E
        .byte   $4F,$00,$00,$04,$00,$00,$31,$38
        .byte   $39,$32,$33,$3A,$3B,$34,$35,$3C
        .byte   $3D,$36,$37,$3E,$3F,$70,$71,$78
        .byte   $79,$54,$55,$5C,$5D,$56,$57,$5E
        .byte   $5F,$08,$00,$22,$30,$00,$00,$00
; --- metatile attribute table: palette assignment per metatile ($B7B5) ---
        .byte   $A7,$A0,$00,$A4,$A1,$00,$00,$A2
        .byte   $00,$00,$00,$A7,$A7,$A6,$A4,$00
        .byte   $A6,$00,$00,$A7,$00,$00,$A0,$00
        .byte   $A7,$00,$A2,$00,$A7,$00,$A7,$A3
        .byte   $AB,$A1,$00,$00,$00,$00,$A3,$00
        .byte   $00,$00,$A2,$00,$00,$00,$A0,$A7
        .byte   $00,$00,$A7,$00,$00,$A4,$00,$00
        .byte   $00,$A7,$A4,$00,$00,$A9,$A1,$00
        .byte   $00,$00,$A7,$00,$A7,$AA,$00,$A5
        .byte   $A2,$00,$00,$AB,$A9,$A1,$A5,$00
        .byte   $00,$00,$A2,$A2,$A7,$00,$00,$A9
        .byte   $A2,$00,$00,$00,$A3,$00,$A5,$A8
        .byte   $00,$A4,$A1,$00,$00,$A5,$A3,$A2
        .byte   $00,$A7,$00,$00,$A7,$A4,$A1,$00
        .byte   $A0,$00,$00,$00,$A1,$00,$00,$A5
        .byte   $A2,$A6,$A7,$00,$A6,$00,$A4,$00
        .byte   $9F,$00,$A4,$00,$00
; --- metatile collision/type table ($B83A) ---
; Upper nybble encodes collision behavior (solid, ladder, spike, etc.).
        .byte   $6E,$6E,$65
        .byte   $6C,$6E,$6E,$6E,$6E,$6A,$6E,$6A
        .byte   $6E,$6E,$9B,$6E,$9B,$9A,$9A,$9A
        .byte   $9A,$65,$6C,$6E,$6E,$6B,$6E,$6E
        .byte   $6E,$6E,$8D,$6E,$67,$8E,$6D,$6F
        .byte   $6F,$66,$66,$6F,$6F,$66,$80,$80
        .byte   $82,$82,$83,$8A,$82,$66,$66,$83
        .byte   $6F,$66,$66,$6F,$97,$83,$6E,$8B
        .byte   $72,$88,$84,$73,$86,$87,$8C,$85
        .byte   $85,$8B,$6E,$8B,$6E,$6B,$6B,$6E
        .byte   $6E,$93,$96,$94,$92,$74,$7A,$8F
        .byte   $8F,$7B,$7C,$75,$76,$84,$8C,$77
        .byte   $87,$6E,$6E,$90,$91,$9C,$9A,$9D
        .byte   $92,$7D,$95,$62,$63,$7D,$7E,$64
        .byte   $63,$7F,$89,$62,$63,$8B,$6E,$64
        .byte   $63,$98,$99,$62,$63,$6E,$6E,$64
        .byte   $63,$6E,$6E,$62,$63,$9B,$9A,$64
        .byte   $63,$00,$00,$A4,$00,$00,$00,$38
; --- zero padding ($B8C5-$BAFF) ---
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
; ===========================================================================
; Screen data blocks ($BB00-$BEFF) — 4 blocks x $100 bytes each
; ===========================================================================
; Each block contains per-screen configuration: metatile layout overrides,
; palette/attribute settings, and enemy spawn parameters for one screen.
; ===========================================================================
; --- screen data block A ($BB00): layout + attributes + palette + enemies ---
        .byte   $00,$00,$00,$00,$00,$00,$40,$00
        .byte   $34,$5E,$60,$C1,$A1,$6E,$82,$84
        .byte   $85,$00,$03,$C1,$A9,$00,$A4,$C9
        .byte   $00,$27,$13,$00,$07,$A8,$AA,$AD
        .byte   $00,$03,$23,$25,$01,$A7,$12,$63
        .byte   $00,$13,$C1,$45,$47,$AF,$12,$00
        .byte   $00,$13,$00,$65,$67,$63,$00,$81
        .byte   $C6,$79,$7B,$7D,$42,$72,$73,$62
        .byte   $97,$99,$9B,$9D,$9F,$A3,$A4,$A6
        .byte   $0E,$4D,$10,$00,$5F,$AB,$AC,$AE
        .byte   $C1,$6D,$05,$00,$6F,$09,$0B,$0D
        .byte   $00,$79,$7B,$7D,$6F,$29,$2B,$2D
        .byte   $12,$99,$9B,$9D,$73,$49,$4B,$02
        .byte   $03,$02,$AE,$04,$3C,$69,$01,$36
        .byte   $06,$BE,$10,$01,$00,$05,$C7,$0C
        .byte   $0E,$98,$82,$83,$84,$9F,$97,$2C
        .byte   $2D,$2E,$01,$87,$88,$08,$09,$09
        .byte   $09,$00,$1B,$2B,$0B,$18,$38,$09
        .byte   $10,$00,$22,$24,$01,$06,$2A,$19
        .byte   $08,$80,$06,$09,$08,$16,$01,$19
        .byte   $18,$85,$18,$00,$00,$20,$00,$00
        .byte   $00,$00,$00,$00,$00,$CC,$00,$00
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
; --- screen data block B ($BC00): layout + attributes + palette + enemies ---
        .byte   $00,$00,$00,$00,$00,$00,$41,$33
        .byte   $00,$5F,$61,$C1,$A2,$C0,$83,$84
        .byte   $86,$32,$04,$A8,$AA,$A1,$A4,$A6
        .byte   $38,$22,$14,$06,$08,$A9,$AC,$AE
        .byte   $02,$04,$24,$26,$28,$00,$CB,$62
        .byte   $C3,$CB,$44,$46,$01,$00,$CB,$00
        .byte   $12,$14,$64,$66,$68,$72,$80,$00
        .byte   $78,$7A,$7C,$7E,$43,$63,$63,$63
        .byte   $98,$9A,$9C,$9E,$9F,$B3,$A5,$A7
        .byte   $0F,$4E,$11,$5E,$00,$AB,$AD,$AF
        .byte   $C1,$05,$05,$6E,$00,$0A,$0C,$00
        .byte   $00,$7A,$7C,$2F,$3F,$2A,$2C,$2E
        .byte   $14,$9A,$9C,$62,$73,$4A,$4C,$03
        .byte   $03,$03,$BE,$05,$00,$01,$6C,$37
        .byte   $07,$3B,$10,$01,$00,$05,$78,$0D
        .byte   $0F,$98,$83,$83,$00,$9F,$98,$98
        .byte   $2E,$2F,$01,$88,$89,$09,$09,$09
        .byte   $0A,$0B,$2B,$00,$00,$10,$38,$09
        .byte   $1A,$2B,$23,$10,$01,$29,$07,$19
        .byte   $21,$81,$07,$09,$09,$01,$28,$19
        .byte   $31,$86,$31,$00,$00,$21,$21,$00
        .byte   $31,$21,$00,$00,$00,$00,$20,$00
        .byte   $CD,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- screen data block C ($BD00): layout + attributes + palette + enemies ---
        .byte   $00,$00,$00,$00,$00,$00,$50,$00
        .byte   $6B,$6E,$70,$00,$B1,$1F,$92,$94
        .byte   $95,$00,$CA,$00,$B9,$B0,$B2,$B5
        .byte   $00,$00,$13,$15,$17,$B8,$BA,$BD
        .byte   $00,$CA,$00,$35,$37,$B7,$12,$73
        .byte   $00,$13,$00,$55,$57,$BF,$22,$71
        .byte   $00,$13,$74,$75,$77,$73,$A0,$91
        .byte   $87,$89,$8B,$8D,$52,$72,$72,$72
        .byte   $72,$72,$72,$00,$00,$B2,$B4,$B6
        .byte   $12,$5D,$13,$00,$6F,$BB,$BC,$BE
        .byte   $00,$75,$75,$00,$C0,$19,$1B,$1D
        .byte   $C2,$89,$8B,$8D,$6F,$39,$3B,$3D
        .byte   $1E,$73,$73,$73,$73,$59,$5B,$12
        .byte   $11,$12,$AE,$14,$01,$77,$C5,$36
        .byte   $26,$BE,$10,$01,$01,$00,$87,$1C
        .byte   $1E,$01,$01,$92,$94,$00,$73,$01
        .byte   $3D,$3E,$98,$97,$10,$18,$10,$19
        .byte   $10,$0B,$2B,$2B,$00,$18,$19,$20
        .byte   $10,$1B,$32,$34,$01,$16,$3A,$38
        .byte   $18,$90,$16,$10,$18,$16,$01,$10
        .byte   $95,$95,$18,$00,$00,$00,$31,$00
        .byte   $20,$00,$21,$20,$21,$00,$CD,$00
        .byte   $30,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- screen data block D ($BE00): layout + attributes + palette + enemies ---
        .byte   $00,$00,$00,$00,$00,$00,$51,$6A
        .byte   $00,$6F,$61,$B0,$B2,$6F,$93,$94
        .byte   $96,$C3,$14,$B8,$BA,$B1,$B4,$B6
        .byte   $48,$12,$14,$16,$18,$B9,$BC,$BE
        .byte   $12,$14,$00,$36,$01,$00,$14,$72
        .byte   $C3,$14,$54,$56,$58,$00,$14,$C4
        .byte   $12,$24,$75,$76,$77,$72,$90,$00
        .byte   $88,$8A,$8C,$8E,$53,$73,$73,$73
        .byte   $73,$73,$72,$00,$00,$B2,$B5,$B7
        .byte   $14,$4E,$14,$6E,$C8,$BB,$BD,$BF
        .byte   $00,$75,$75,$1F,$00,$1A,$1C,$C2
        .byte   $C2,$8A,$8C,$2F,$4F,$3A,$3C,$3E
        .byte   $24,$72,$72,$72,$72,$5A,$5C,$13
        .byte   $11,$11,$BE,$15,$01,$8F,$75,$37
        .byte   $27,$3B,$10,$01,$01,$00,$88,$1D
        .byte   $1F,$01,$82,$93,$84,$72,$72,$01
        .byte   $3E,$3F,$98,$10,$99,$10,$10,$19
        .byte   $1A,$1B,$0B,$1B,$2B,$10,$19,$30
        .byte   $1A,$00,$33,$35,$01,$39,$17,$38
        .byte   $31,$91,$17,$10,$10,$01,$17,$10
        .byte   $96,$96,$31,$00,$00,$00,$00,$30
        .byte   $00,$00,$20,$00,$00,$21,$00,$CC
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
; --- stage attribute assignment table ($BF00-$BFFF) ---
; NES nametable attribute data: 2-bit palette indices for each 32x32
; pixel area. Padded with zeros to fill to end of bank.
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $01,$03,$00,$00,$00,$03,$02,$02
        .byte   $02,$02,$02,$00,$00,$03,$03,$03
        .byte   $02,$02,$02,$00,$00,$03,$03,$03
        .byte   $01,$01,$02,$00,$00,$03,$02,$03
        .byte   $01,$01,$00,$00,$00,$03,$02,$02
        .byte   $01,$01,$00,$00,$00,$03,$02,$02
        .byte   $00,$00,$00,$00,$02,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $02,$00,$01,$03,$03,$00,$00,$00
        .byte   $03,$00,$00,$03,$03,$00,$00,$00
        .byte   $00,$00,$00,$00,$03,$00,$00,$00
        .byte   $02,$00,$00,$00,$03,$00,$00,$10
        .byte   $10,$10,$00,$00,$00,$00,$00,$03
        .byte   $00,$00,$02,$00,$00,$02,$00,$02
        .byte   $02,$02,$02,$02,$02,$00,$00,$02
        .byte   $02,$02,$02,$02,$02,$03,$03,$03
        .byte   $03,$01,$01,$01,$01,$03,$03,$03
        .byte   $03,$01,$02,$02,$02,$00,$00,$02
        .byte   $02,$02,$02,$02,$02,$00,$00,$02
        .byte   $02,$02,$02,$00,$00,$01,$03,$02
        .byte   $00,$02,$01,$00,$00,$02,$01,$00
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
        .byte   $00,$00,$00,$00,$00
