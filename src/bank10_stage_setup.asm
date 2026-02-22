; =============================================================================
; MEGA MAN 3 (U) — BANK $10 — BOSS DOOR ANIMATIONS + STAGE $14 DATA
; =============================================================================
; Mapped to $8000-$9FFF. Called via trampolines in the fixed bank
; ($1FEE31 = close door, $1FEE44 = open door).
;
; This bank serves a dual purpose:
;   1. Boss door close/open animation routines ($8000-$82A9)
;      - Entry at $8000: close boss door (tiles written top-to-bottom)
;      - Entry at $8003: open boss door (tiles written bottom-to-top)
;      Both routines write 4 tile columns to the PPU nametable buffer
;      ($0780) and update the attribute table RAM ($0640), animating
;      the boss room shutter one column at a time with a 4-frame delay
;      between columns. A sound effect ($1D) plays during the animation.
;
;   2. Stage data for stage $14 ($83D0-$9FFF)
;      Also referenced by stage_to_bank in the fixed bank.
;      Standard stage data layout (shifted from $A000 base to $8000):
;        $83D0: Collision bitmask table (also used as attr masks by door code)
;        $8800: Tile property / solid flag map
;        $8A00: Screen index / room header data
;        $8A60: Room CHR/palette config
;        $8A80: BG palette data (4 palettes x 4 colors x 2 phases)
;        $8B00: Enemy placement — screen numbers ($FF-terminated)
;        $8C00: Enemy placement — X pixel positions ($FF-terminated)
;        $8D00: Enemy placement — Y pixel positions ($FF-terminated)
;        $8E00: Enemy placement — global enemy IDs ($FF-terminated)
;        $8F00: Nametable screen map (metatile column indices per screen)
;        $9700: Metatile column definitions (8 rows per column)
;        $98A4: Zero padding
;        $9B00: Metatile CHR tiles — top-left quadrant
;        $9C00: Metatile CHR tiles — top-right quadrant
;        $9D00: Metatile CHR tiles — bottom-left quadrant
;        $9E00: Metatile CHR tiles — bottom-right quadrant
;        $9F00: Collision / palette attribute table
;        $9F80: Zero padding to bank end
;
; PPU buffer format at $0780 (consumed by NMI's drain_ppu_buffer):
;   $0780/$0784: PPU address high byte (nametable 0 / nametable 1)
;   $0781/$0785: PPU address low byte (| $20 for second copy)
;   $0782/$0786: byte count (1 = single tile per entry)
;   $0783/$0787: tile data byte
;   $0788-$078C: third entry (attribute table write at $23xx)
;   $078D:       attribute byte value
;   $078E:       $FF terminator
;
; Key RAM regions:
;   $0640,x — nametable attribute table mirror (2-bit palette selectors)
;   $BB00-$BE00 — metatile CHR tile tables (TL, TR, BL, BR) in stage bank
;   $95 — frame counter (animation timing, writes every 4 frames)
;   $02 — column counter (4 columns per door animation)
;   $03 — attribute table offset within $0640
;   $04 — sub-index into attribute mask table
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"

ensure_stage_bank           := $C8A0    ; ensure_stage_bank
submit_sound_ID           := $F89A      ; submit_sound_ID
task_yield           := $FF21           ; task_yield (wait for NMI)
select_PRG_banks           := $FF6B     ; select_PRG_banks

.segment "BANK10"

; =============================================================================
; ENTRY POINT JUMP TABLE
; =============================================================================
        jmp     close_boss_door               ; $8000: close boss door
                                    ;   (called before boss fight)
        jmp     open_boss_door               ; $8003: open boss door
                                    ;   (called after boss defeat)

; =============================================================================
; CLOSE BOSS DOOR — animate shutter tiles top-to-bottom
; =============================================================================
; Writes 4 columns of shutter tiles into the nametable, working from
; the bottom row upward in PPU address space (high addresses first,
; subtracting $40 per column = 2 tile rows). Each column is written
; to both nametable 0 and nametable 1 via the PPU buffer, with
; attribute table bits updated in the $0640 mirror.
; ---------------------------------------------------------------------------
close_boss_door:  jsr     ensure_stage_bank   ; ensure stage PRG bank is selected
        lda     #$00                    ; A = 0
        sta     $95                     ; reset frame counter
        ; --- Set up PPU buffer entry 1 (nametable 0 tile) ---
        lda     boss_door_close_ppu_addr_high,y ; PPU address high byte from table
        sta     $0780                   ; PPU buf entry 1 high byte
        sta     $0785                   ; same high byte for entry 2 (NT1)
        lda     boss_door_close_ppu_addr_low,y ; PPU address low byte from table
        sta     $0781                   ; PPU buf entry 1 low byte
        ora     #$20                    ; offset $20 for nametable 1 copy
        sta     $0786                   ; NT1 low byte
        lda     #$01                    ; 1 byte per PPU write
        sta     $0782                   ; PPU buf entry 1 count
        sta     $0787                   ; PPU buf entry 2 count
        ; --- Set up PPU buffer entry 3 (attribute table) ---
        lda     #$23                    ; $23xx = attribute table region
        sta     $078A                   ; PPU buf attr high byte
        lda     boss_door_close_attr_offset,y ; attribute offset within $0640
        sta     $03                     ; store attr offset
        ora     #$C0                    ; $23C0+ = PPU attribute table addr
        sta     $078B                   ; PPU buf attr low byte
        lda     boss_door_close_attr_subindex,y ; attribute sub-index for mask table
        sta     $04                     ; store attr sub-index
        lda     #$00                    ; A = 0
        sta     $078C                   ; 0 extra bytes for attr write
        ; --- Play door close sound effect ---
        lda     #SFX_DOOR               ; sound ID $1D = door/shutter SFX
        jsr     submit_sound_ID         ; play door sound effect
        lda     #$04                    ; 4 columns to animate
        sta     $02                     ; store column counter
; --- Write one column of door tiles ---
close_door_write_column:  ldx     boss_door_close_metatile_indices,y ; metatile sub-index for this column
        lda     $BB00,x                 ; top-left CHR tile
        sta     $0783                   ; → PPU buffer entry 1 data (NT0)
        lda     $BC00,x                 ; top-right CHR tile
        sta     $0784                   ; → PPU buffer entry 2 data (NT0 second)
        lda     $BD00,x                 ; bottom-left CHR tile
        sta     $0788                   ; → PPU buffer entry 3 (NT1 tile 1)
        lda     $BE00,x                 ; bottom-right CHR tile
        sta     $0789                   ; → PPU buffer entry 4 (NT1 tile 2)
        ; --- Update attribute table ---
        ldx     $04                     ; attribute sub-index
        lda     stage_collision_bitmask_table,x ; attribute bitmask (clear bits)
        sta     $05                     ; store bitmask for AND
        ldx     $03                     ; attribute offset in $0640 mirror
        lda     $0640,x                 ; read current attribute byte
        and     $05                     ; mask off old palette bits
        ora     boss_door_close_attr_palette_bits,y ; OR in new palette bits from table
        sta     $078D                   ; write to PPU buffer
        sta     $0640,x                 ; update RAM mirror
        lda     #$FF                    ; $FF = PPU buffer terminator
        sta     $078E                   ; write terminator to PPU buf
        sta     nametable_dirty         ; signal NMI to flush PPU buffer
; --- Wait 4 frames between columns (animation delay) ---
close_door_wait_frames:  lda     #$00                ; A = 0
        sta     nmi_skip                ; allow NMI processing
        jsr     task_yield              ; task_yield — wait for next frame
        inc     nmi_skip                ; re-lock NMI
        inc     $95                     ; increment frame counter
        lda     $95                     ; read frame counter
        and     #$03                    ; wait until counter is multiple of 4
        bne     close_door_wait_frames               ; loop until multiple of 4
        iny                             ; advance Y to next table entry
        dec     $02                     ; decrement column counter
        beq     close_door_done               ; all 4 columns done → exit
        ; --- Move PPU address up by 2 tile rows ($40 bytes) ---
        lda     $0781                   ; read PPU addr low byte
        sec                             ; set carry for subtract
        sbc     #$40                    ; move up 2 rows in nametable
        sta     $0781                   ; update NT0 PPU addr low
        ora     #$20                    ; set bit 5 for NT1
        sta     $0786                   ; NT1 copy offset by $20
        lda     $0780                   ; read PPU addr high byte
        sbc     #$00                    ; propagate borrow to high byte
        sta     $0780                   ; update NT0 PPU addr high
        sta     $0785                   ; update NT1 PPU addr high
        ; --- Toggle attribute sub-index (alternates 0/2 or 1/3) ---
        lda     $04                     ; read attr sub-index
        eor     #$02                    ; flip between upper/lower attr half
        sta     $04                     ; store toggled sub-index
        cmp     #$03                    ; check if crossed boundary
        bne     close_door_write_column               ; if not wrapped, continue
        ; --- Crossed attribute boundary: move attr offset back ---
        lda     $03                     ; read attr offset
        sec                             ; set carry for subtract
        sbc     #$08                    ; previous attribute row
        sta     $03                     ; store new attr offset
        ora     #$C0                    ; rebuild PPU attribute address
        sta     $078B                   ; update PPU attr address
        jmp     close_door_write_column               ; continue next column

; --- Door close complete: restore stage bank and return ---
close_door_done:  lda     stage_id            ; get current stage ID
        sta     prg_bank                ; set as PRG bank number
        jmp     select_PRG_banks        ; select_PRG_banks and return

; ===========================================================================
; DOOR CLOSE DATA TABLES
; ===========================================================================
; Indexed by Y (set by ensure_stage_bank based on stage_id * 6).
; Each stage has 4 groups of 6 bytes for the 4 door columns.
;
; boss_door_close_ppu_addr_high/boss_door_close_ppu_addr_low: starting PPU nametable address (high/low)
; boss_door_close_metatile_indices: metatile sub-indices into $BB00-$BE00 CHR tile tables
;        (4 entries per column + 2 bytes for next PPU address)
; boss_door_close_attr_offset/boss_door_close_attr_subindex: starting attribute table offset / sub-index
; boss_door_close_attr_palette_bits: attribute palette bits to OR in per column
; ===========================================================================
boss_door_close_ppu_addr_high:  .byte   $21 ; PPU address high byte
boss_door_close_ppu_addr_low:  .byte   $5E ; PPU address low byte
boss_door_close_metatile_indices:  .byte   $6D,$6D,$6D,$6C,$22,$DE,$6B,$6A
        .byte   $69,$68,$21,$5E,$7A,$7A,$7A,$72
        .byte   $22,$DE,$00,$00,$00,$00,$21,$DE
        .byte   $04,$04,$04,$04,$22,$DE,$58,$58
        .byte   $58,$58,$22,$DE,$6C,$6C,$6C,$74
        .byte   $22,$5E,$08,$08,$08,$08,$22,$9E
        .byte   $6C,$6C,$64,$74,$22,$5E,$80,$80
        .byte   $80,$85,$22,$DE,$41,$49,$41,$73
        .byte   $22,$DE,$42,$4A,$42,$74,$22,$DE
        .byte   $00,$00,$00,$00,$22,$DE,$6C,$6C
        .byte   $6C,$74,$22,$5E,$6C,$64,$6C,$74
        .byte   $22,$DE,$9B,$9B,$9B,$93,$21,$DE
        .byte   $00,$00,$00,$00,$22,$5E,$6C,$6C
        .byte   $6C,$74,$22,$DE,$14,$14,$14,$13
        .byte   $22,$5E,$1F,$1F,$1F,$27,$23,$1E
        .byte   $15,$1D,$15,$2D,$22,$DE,$00,$00
        .byte   $00,$00,$22,$DE,$00,$00,$00,$00
        .byte   $22,$DE,$00,$00,$00,$00
boss_door_close_attr_offset:  .byte   $17 ; attribute offset in $0640
boss_door_close_attr_subindex:  .byte   $03 ; attribute sub-index for mask table
boss_door_close_attr_palette_bits:  .byte   $40,$04,$40,$04,$2F,$03,$40,$04
        .byte   $40,$04,$17,$03,$80,$08,$80,$08
        .byte   $2F,$03,$00,$00,$00,$00,$1F,$03
        .byte   $00,$00,$00,$00,$2F,$03,$00,$00
        .byte   $00,$00,$2F,$03,$00,$00,$00,$00
        .byte   $27,$03,$40,$04,$40,$04,$2F,$01
        .byte   $00,$00,$00,$00,$27,$03,$C0,$0C
        .byte   $C0,$0C,$2F,$03,$C0,$0C,$C0,$0C
        .byte   $2F,$03,$C0,$0C,$C0,$0C,$2F,$03
        .byte   $00,$00,$00,$00,$2F,$03,$00,$00
        .byte   $00,$00,$27,$03,$00,$00,$00,$00
        .byte   $2F,$03,$80,$08,$80,$08,$1F,$03
        .byte   $00,$00,$00,$00,$27,$03,$00,$00
        .byte   $00,$00,$2F,$03,$C0,$0C,$C0,$0C
        .byte   $27,$03,$40,$04,$40,$04,$37,$01
        .byte   $04,$40,$04,$40,$2F,$03,$00,$00
        .byte   $00,$00,$2F,$03,$00,$00,$00,$00
        .byte   $2F,$03,$00,$00,$00,$00
; =============================================================================
; OPEN BOSS DOOR — animate shutter tiles bottom-to-top
; =============================================================================
; Mirror of the close routine but works in reverse: starts from the
; top of the door and adds $40 per column (moving downward in PPU
; address space), revealing the passage behind the shutter.
; Uses its own set of data tables (boss_door_open_ppu_addr_high-boss_door_open_attr_palette_bits).
; ---------------------------------------------------------------------------
open_boss_door:  jsr     ensure_stage_bank   ; ensure stage PRG bank is selected
        lda     boss_door_open_ppu_addr_high,y ; PPU address high byte from table
        sta     $0780                   ; PPU buf entry 1 high (NT0)
        sta     $0784                   ; PPU buf entry 2 high (NT1)
        lda     boss_door_open_ppu_addr_low,y ; PPU address low byte from table
        sta     $0781                   ; PPU buf entry 1 low byte
        ora     #$20                    ; offset for second nametable
        sta     $0785                   ; NT1 low byte
        lda     #$00                    ; A = 0
        sta     $0782                   ; 0 extra bytes per PPU entry
        sta     $0786                   ; PPU buf entry 2 count
        sta     $95                     ; reset frame counter
        ; --- Attribute table entry setup ---
        lda     #$23                    ; $23xx = attribute table region
        sta     $0788                   ; PPU buf attr high byte
        lda     boss_door_open_attr_offset,y ; attribute offset within $0640
        sta     $03                     ; store attr offset
        ora     #$C0                    ; $23C0+ = PPU attribute table addr
        sta     $0789                   ; PPU buf attr low byte
        lda     boss_door_open_attr_subindex,y ; attribute sub-index for mask table
        sta     $04                     ; store attr sub-index
        lda     #$00                    ; A = 0
        sta     $078A                   ; 0 extra bytes for attr write
        ; --- Play door open sound effect ---
        lda     #SFX_DOOR               ; sound ID $1D = door/shutter SFX
        jsr     submit_sound_ID         ; play door sound effect
        lda     #$04                    ; 4 columns to animate
        sta     $02                     ; store column counter
; --- Write one column of open-door tiles ---
open_door_write_column:  ldx     boss_door_open_metatile_indices,y ; metatile sub-index for this column
        lda     $BC00,x                 ; top-right CHR tile
        sta     $0783                   ; → PPU buffer tile data (NT0)
        lda     $BE00,x                 ; bottom-right CHR tile
        sta     $0787                   ; → PPU buffer tile data (NT1)
        ; --- Update attribute table ---
        ldx     $04                     ; attribute sub-index
        lda     stage_collision_bitmask_table,x ; attribute bitmask (clear bits)
        sta     $05                     ; store bitmask for AND
        ldx     $03                     ; attribute offset in $0640 mirror
        lda     $0640,x                 ; read current attribute byte
        and     $05                     ; mask off old palette bits
        ora     boss_door_open_attr_palette_bits,y ; OR in new palette bits from table
        sta     $078B                   ; write to PPU buffer
        sta     $0640,x                 ; update RAM mirror
        lda     #$FF                    ; $FF = PPU buffer terminator
        sta     $078C                   ; write terminator to PPU buf
        sta     nametable_dirty         ; signal NMI to flush PPU buffer
; --- Wait 4 frames between columns (animation delay) ---
open_door_wait_frames:  lda     #$00                ; A = 0
        sta     nmi_skip                ; allow NMI processing
        jsr     task_yield              ; task_yield — wait for next frame
        inc     nmi_skip                ; re-lock NMI
        inc     $95                     ; increment frame counter
        lda     $95                     ; read frame counter
        and     #$03                    ; wait until counter is multiple of 4
        bne     open_door_wait_frames               ; loop until multiple of 4
        iny                             ; advance Y to next table entry
        dec     $02                     ; decrement column counter
        beq     open_door_done               ; all 4 columns done → exit
        ; --- Move PPU address down by 2 tile rows ($40 bytes) ---
        lda     $0781                   ; read PPU addr low byte
        clc                             ; clear carry for add
        adc     #$40                    ; move down 2 rows in nametable
        sta     $0781                   ; update NT0 PPU addr low
        ora     #$20                    ; set bit 5 for NT1
        sta     $0785                   ; NT1 copy offset by $20
        lda     $0780                   ; read PPU addr high byte
        adc     #$00                    ; propagate carry to high byte
        sta     $0780                   ; update NT0 PPU addr high
        sta     $0784                   ; update NT1 PPU addr high
        ; --- Toggle attribute sub-index ---
        lda     $04                     ; read attr sub-index
        eor     #$02                    ; flip between upper/lower attr half
        sta     $04                     ; store toggled sub-index
        bne     open_door_write_column               ; if nonzero, continue
        ; --- Crossed attribute boundary: advance attr offset ---
        lda     $03                     ; read attr offset
        clc                             ; clear carry for add
        adc     #$08                    ; next attribute row
        sta     $03                     ; store new attr offset
        ora     #$C0                    ; rebuild PPU attribute address
        sta     $0789                   ; update PPU attr address
        jmp     open_door_write_column               ; continue next column

; --- Door open complete: restore stage bank and return ---
open_door_done:  lda     stage_id            ; get current stage ID
        sta     prg_bank                ; set as PRG bank number
        jmp     select_PRG_banks        ; select_PRG_banks and return

; ===========================================================================
; DOOR OPEN DATA TABLES
; ===========================================================================
; Same structure as door close tables but with reversed PPU direction.
; boss_door_open_ppu_addr_high/boss_door_open_ppu_addr_low: starting PPU nametable address (high/low)
; boss_door_open_metatile_indices: metatile sub-indices (4 per column + 2 bytes PPU address)
; boss_door_open_attr_offset/boss_door_open_attr_subindex: attribute table offset / sub-index
; boss_door_open_attr_palette_bits: attribute palette bits to OR in per column
; ===========================================================================
boss_door_open_ppu_addr_high:  .byte   $20 ; PPU address high byte
boss_door_open_ppu_addr_low:  .byte   $81 ; PPU address low byte
boss_door_open_metatile_indices:  .byte   $16,$16,$16,$16,$22,$01,$2F,$2F
        .byte   $2F,$2F,$20,$81,$27,$27,$27,$27
        .byte   $22,$01,$06,$06,$06,$06,$21,$01
        .byte   $0F,$0F,$0F,$0F,$22,$01,$07,$07
        .byte   $07,$07,$22,$01,$0F,$0F,$0F,$0F
        .byte   $21,$81,$5D,$5D,$5D,$5D,$21,$C1
        .byte   $0F,$0F,$0F,$0F,$21,$81,$5D,$5D
        .byte   $5D,$5D,$22,$01,$5D,$5D,$5D,$5D
        .byte   $22,$01,$5D,$5D,$5D,$5D,$22,$01
        .byte   $06,$06,$06,$06,$22,$01,$0F,$0F
        .byte   $0F,$0F,$21,$81,$0F,$0F,$0F,$0F
        .byte   $22,$01,$27,$27,$27,$27,$21,$01
        .byte   $06,$06,$06,$06,$21,$81,$0F,$0F
        .byte   $0F,$0F,$22,$01,$0E,$0E,$0E,$0E
        .byte   $21,$81,$06,$06,$06,$06,$22,$41
        .byte   $06,$06,$06,$06,$22,$01,$0E,$0E
        .byte   $0E,$0E,$22,$01,$0B,$0B,$0B,$0B
        .byte   $22,$01,$00,$00,$00,$00
boss_door_open_attr_offset:  .byte   $08 ; attribute offset in $0640
boss_door_open_attr_subindex:  .byte   $00 ; attribute sub-index for mask table
boss_door_open_attr_palette_bits:  .byte   $03,$30,$03,$30,$20,$00,$00,$00
        .byte   $00,$00,$08,$00,$03,$30,$03,$30
        .byte   $20,$00,$00,$00,$00,$00,$10,$00
        .byte   $02,$20,$02,$20,$20,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $18,$00,$00,$00,$00,$00,$18,$02
        .byte   $00,$00,$00,$00,$18,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$18,$00,$00,$00,$00,$00
        .byte   $20,$00,$03,$30,$03,$30,$10,$00
        .byte   $00,$00,$00,$00,$18,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $18,$00,$03,$30,$03,$30,$20,$02
        .byte   $30,$03,$30,$03,$20,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$00
; =============================================================================
; STAGE $14 DATA — Wily Stage 2 (Doc Robot: Gemini + Spark)
; =============================================================================
; From $83D0 onward this bank serves as stage data for stage $14.
; Referenced by stage_to_bank in the fixed bank. This is a Doc Robot
; remixed stage combining Gemini Man and Spark Man environments.
;
; The collision bitmask table at stage_collision_bitmask_table doubles as the attribute mask
; lookup used by the door animation routines above (first 4 bytes
; $FC/$F3/$CF/$3F mask the four 2-bit fields of a PPU attribute byte).
;
; All data follows the standard stage bank layout, shifted from the
; normal $A000 base to $8000 (offsets are identical, base differs).
; =============================================================================

; ===========================================================================
; Collision bitmask table ($83D0, equiv $A3D0)
; ===========================================================================
; Each byte encodes passability for 8 horizontal pixels of a metatile row.
; Bit = 1 means passable, bit = 0 means solid. Indexed by metatile ID.
; The first 4 bytes ($FC,$F3,$CF,$3F) also serve as attribute masks for
; the door animation routines above.
; ---------------------------------------------------------------------------
stage_collision_bitmask_table:  .byte   $FC,$F3,$CF,$3F,$A7,$FF,$FF,$FF
        .byte   $FD,$FF,$FF,$FF,$BE,$FF,$FF,$FF
        .byte   $BF,$FF,$AD,$FF,$FB,$FF,$BB,$DF
        .byte   $FE,$FB,$FE,$FF,$FF,$FD,$FF,$7D
        .byte   $AF,$DF,$FE,$FF,$FB,$FF,$EA,$FF
        .byte   $EB,$FF,$36,$DF,$BF,$FF,$BB,$FF
        .byte   $3F,$AA,$FF,$DD,$FF,$D5,$FC,$D7
        .byte   $FF,$DF,$FB,$55,$FF,$57,$FB,$5D
        .byte   $FF,$5D,$BF,$F9,$FB,$DF,$AB,$5F
        .byte   $FF,$D5,$FF,$FF,$CF,$77,$EF,$54
        .byte   $FF,$77,$FF,$7E,$FF,$D7,$6F,$67
        .byte   $FF,$FD,$FF,$7F,$FF,$FD,$FF,$7F
        .byte   $F7,$D5,$FF,$B7,$7F,$5D,$FF,$F7
        .byte   $F7,$F7,$F7,$57,$FF,$FF,$FF,$7F
        .byte   $FD,$F7,$FD,$5F,$7F,$7D,$F7,$7B
        .byte   $FF,$7F,$FF,$75,$FF,$F7,$FF,$55
        .byte   $FF,$5D,$DE,$F5,$F7,$DF,$FE,$FF
        .byte   $BB,$5F,$FF,$5F,$F7,$F7,$F7,$5D
        .byte   $FD,$F5,$FF,$D5,$FF,$5D,$F7,$FF
        .byte   $FB,$DF,$7B,$7F,$FE,$D7,$7F,$5D
        .byte   $FF,$DF,$FF,$FF,$FF,$7D,$FF,$DB
        .byte   $FF,$7D,$FF,$FF,$FF,$D7,$DF,$FD
        .byte   $FF,$73,$7F,$7F,$B7,$75,$BF,$74
        .byte   $FF,$FD,$FF,$75,$7F,$F7,$FE,$FD
        .byte   $FE,$FD,$FF,$77,$FF,$7F,$FB,$5D
        .byte   $F7,$FD,$FF,$55,$FF,$7D,$FF,$7F
        .byte   $FF,$7D,$FF,$56,$FF,$7F,$FE,$5F
        .byte   $FF,$B7,$FF,$5F,$FF,$7F,$FD,$FD
        .byte   $7B,$DF,$FF,$FD,$BF,$7F,$FF,$D7
        .byte   $FF,$7D,$FF,$9F,$FF,$F7,$FE,$75
        .byte   $EF,$77,$FF,$75,$DB,$FD,$FF,$FD
        .byte   $F7,$C7,$FF,$F7,$BF,$6D,$7B,$7F
        .byte   $FF,$DF,$F7,$FF,$FF,$4F,$FD,$5D
        .byte   $FF,$7D,$6F,$DD,$FF,$DF,$FF,$57
        .byte   $FF,$57,$FF,$55,$FF,$FF,$FF,$FD
        .byte   $FF,$FD,$FF,$DF,$BF,$77,$FF,$7D
        .byte   $FF,$7D,$FF,$7F,$FF,$7F,$FF,$FD
        .byte   $BF,$5F,$FF,$74,$FF,$75,$FD,$DF
        .byte   $FF,$75,$EF,$95,$FF,$51,$7F,$55
        .byte   $B7,$C4,$7F,$DD,$FE,$3D,$FF,$46
        .byte   $EF,$1D,$77,$5D,$7F,$57,$77,$C5
        .byte   $BB,$7F,$FF,$7F,$FF,$53,$FF,$67
        .byte   $FF,$D7,$FF,$D5,$FF,$D5,$FF,$DF
        .byte   $FF,$D5,$F7,$F5,$FF,$B7,$FF,$DF
        .byte   $FF,$79,$FF,$FD,$FF,$7F,$FF,$75
        .byte   $FF,$DF,$FB,$DF,$FF,$75,$FF,$55
        .byte   $FF,$DF,$FF,$F3,$EF,$FF,$FF,$D5
        .byte   $76,$F9,$FF,$5B,$FF,$5D,$FF,$CD
        .byte   $FF,$45,$FF,$F5,$FF,$57,$FF,$FF
        .byte   $FF,$FF,$FF,$4E,$FF,$5F,$FD,$76
        .byte   $FF,$F3,$FF,$7D,$FF,$DF,$FF,$7D
        .byte   $FF,$5F,$FF,$5F,$FF,$5D,$FF,$57
        .byte   $FF,$D7,$FF,$F7,$FF,$56,$FF,$F5
        .byte   $FF,$F5,$FF,$F5,$BF,$7F,$FF,$FB
        .byte   $FF,$75,$DE,$FF,$FF,$75,$FD,$DF
        .byte   $FF,$FD,$77,$DF,$DF,$BD,$FF,$D7
        .byte   $FD,$D7,$FF,$53,$76,$DE,$FF,$DF
        .byte   $FF,$5D,$FF,$57,$FF,$D5,$7F,$7D
        .byte   $BF,$7F,$FF,$77,$EF,$5D,$FF,$F7
        .byte   $FF,$DD,$FF,$75,$FF,$FD,$7F,$FD
        .byte   $FF,$5D,$F7,$6F,$FB,$F7,$FF,$6D
        .byte   $FF,$DD,$FE,$F5,$FF,$FF,$FF,$55
        .byte   $FF,$DD,$7F,$5D,$DB,$F7,$FF,$27
        .byte   $FF,$F5,$FE,$F7,$FF,$F5,$FF,$F5
        .byte   $FE,$7F,$FF,$DF,$FF,$7D,$FF,$FF
        .byte   $FF,$77,$FF,$7E,$FF,$7F,$FD,$17
        .byte   $FF,$DD,$3F,$77,$FF,$FF,$FF,$DF
        .byte   $EB,$F7,$FF,$7D,$FF,$DF,$FF,$5F
        .byte   $FF,$77,$FD,$C5,$FF,$D7,$FB,$FF
        .byte   $FF,$F7,$FF,$D6,$FF,$F4,$FF,$7F
        .byte   $CF,$5D,$FF,$DF,$FE,$FF,$DF,$57
        .byte   $FB,$5F,$FF,$7D,$F7,$3D,$B7,$D6
        .byte   $FF,$D7,$FB,$5D,$6E,$54,$FF,$5D
        .byte   $BF,$55,$BF,$F5,$7E,$DD,$FF,$75
        .byte   $FF,$DE,$FF,$FF,$DF,$D7,$EF,$DF
        .byte   $FF,$CC,$FF,$FF,$FF,$BE,$FF,$DD
        .byte   $BF,$CD,$FF,$75,$FF,$FD,$BF,$FD
        .byte   $BF,$F7,$FF,$57,$FF,$7D,$FF,$77
        .byte   $FF,$7D,$FF,$FF,$FF,$7D,$FF,$5D
        .byte   $FF,$CD,$FA,$7F,$7F,$D7,$FF,$D7
        .byte   $FE,$3F,$DF,$FF,$FF,$5D,$FB,$7D
        .byte   $FF,$56,$FF,$F7,$FF,$FC,$FF,$7D
        .byte   $BF,$F3,$FF,$5D,$FF,$FD,$FF,$FF
        .byte   $FF,$FD,$FF,$D7,$FF,$FF,$FF,$F7
        .byte   $FD,$75,$FF,$DD,$FF,$FE,$DF,$FF
        .byte   $FF,$FF,$FF,$F7,$F7,$77,$FF,$77
        .byte   $FF,$6B,$FF,$5F,$EF,$D7,$FF,$D5
        .byte   $FF,$F4,$7F,$DF,$FF,$F5,$9F,$FF
        .byte   $FF,$DD,$E7,$FD,$FF,$75,$FF,$55
        .byte   $FF,$D7,$FF,$DF,$DF,$75,$FF,$DF
        .byte   $FE,$F5,$FE,$57,$EF,$6D,$FF,$F5
        .byte   $FF,$7D,$FF,$7F,$DF,$77,$FB,$F5
        .byte   $FF,$7D,$FD,$57,$FF,$FF,$FF,$F4
        .byte   $FF,$F9,$DF,$DF,$FF,$DD,$FE,$DD
        .byte   $FB,$57,$FF,$7F,$FF,$7D,$FF,$74
        .byte   $BD,$F5,$FF,$FD,$FF,$5D,$FB,$77
        .byte   $1F,$75,$FF,$DD,$F3,$5F,$FF,$DF
        .byte   $FF,$FF,$FF,$DD,$FF,$75,$FF,$75
        .byte   $F7,$F5,$FB,$E3,$FE,$75,$FD,$7F
        .byte   $FF,$D7,$FF,$FF,$EF,$FD,$FF,$7F
        .byte   $FB,$F7,$FF,$7D,$BF,$DF,$FF,$D7
        .byte   $FF,$7D,$EF,$FD,$EF,$F7,$FF,$7F
        .byte   $FF,$77,$FF,$DD,$FB,$DF,$DF,$57
        .byte   $FE,$DD,$FF,$F1,$FD,$77,$FF,$57
        .byte   $FF,$35,$F7,$7D,$FF,$FD,$FF,$DD
        .byte   $FF,$57,$FF,$7D,$FF,$57,$7F,$53
        .byte   $DF,$FF,$FF,$37,$FF,$9F,$F7,$EF
        .byte   $FF,$57,$FF,$FF,$FF,$D6,$FF,$FB
        .byte   $FF,$DF,$FF,$7D,$FD,$5D,$FF,$DF
        .byte   $FF,$D7,$FF,$FD,$EF,$3F,$FF,$DF
        .byte   $FF,$DD,$BE,$1D,$FF,$B7,$FF,$DF
        .byte   $FF,$EF,$FF,$FD,$DE,$75,$FF,$F9
        .byte   $FF,$CB,$FF,$75,$EF,$57,$FF,$C5
        .byte   $FF,$FF,$E7,$F7,$EF,$D5,$FF,$55
        .byte   $FF,$76,$FF,$5F,$FF,$FF,$FF,$77
        .byte   $7F,$DD,$FF,$DD,$FF,$5D,$F7,$D7
        .byte   $FF,$FD,$EF,$DD,$FF,$7D,$FF,$EB
        .byte   $FF,$FD,$FB,$FD,$FF,$5C,$FF,$7F
        .byte   $FF,$5D,$FF,$75,$FE,$DD,$F7,$55
        .byte   $DF,$69,$FF,$5F,$FF,$D6,$FF,$EE
        .byte   $FF,$7F,$FF,$DF,$FB,$E1,$FF,$CD
        .byte   $FF,$7F,$FF,$F5,$FD,$67,$FF,$77
        .byte   $BE,$5D,$FF,$7F,$FF,$5F,$FF,$DF
        .byte   $FF,$DD,$FF,$77,$FF,$5D,$FF,$F7
        .byte   $FF,$DF,$FF,$FF,$FB,$FF,$FF,$5F
        .byte   $FF,$FD,$FF,$FF,$FF,$57,$FF,$7D
        .byte   $FD,$F5,$FF,$3D,$FF,$E7,$EF,$37
        .byte   $FF,$DF,$3F,$55,$F7,$D7,$FF,$7D
        .byte   $FF,$55,$FF,$5D,$FF,$FD,$FF,$FF
        .byte   $FF,$7D,$FF,$77,$FE,$D7,$EF,$5D
        .byte   $FF,$F5,$F7,$55,$FB,$DC,$DD,$FD
        .byte   $FF,$D7,$3F,$D5,$FF,$F7,$FF,$5D
        .byte   $FF,$5F,$FF,$D7,$FF,$FF,$FF,$DD
        .byte   $F7,$7D,$FF,$5D,$FF,$BF,$FF,$FF
; ===========================================================================
; Tile property / solid flag map ($8800, equiv $A800)
; ===========================================================================
; One byte per metatile. Encodes tile-level collision properties such as
; solid, ladder, spike, or water. Mostly zero (passable background).
; ---------------------------------------------------------------------------
        .byte   $00,$00,$80,$00,$00,$00,$00,$10
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$08,$80,$08,$84,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$00,$20
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$80,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $00,$04,$00,$00,$00,$00,$00,$20
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$81,$00,$10,$00,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$04,$00,$02
        .byte   $00,$00,$80,$00,$02,$40,$00,$00
        .byte   $00,$04,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$02,$00,$00,$08,$20,$00
        .byte   $00,$00,$00,$20,$00,$00,$00,$00
        .byte   $00,$20,$00,$20,$00,$00,$00,$10
        .byte   $08,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$02,$00,$00,$00,$02
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $80,$01,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$07
        .byte   $00,$00,$00,$00,$00,$00,$08,$20
        .byte   $00,$00,$00,$40,$00,$40,$00,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $02,$00,$00,$C0,$00,$00,$00,$00
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$08,$10,$08,$08
        .byte   $00,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$04,$20,$00,$00,$00,$00,$00
        .byte   $00,$10,$00,$10,$00,$20,$00,$00
        .byte   $00,$08,$80,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$10,$00,$40
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$08,$00,$00,$00,$00,$00,$40
        .byte   $08,$00,$00,$00,$00,$00,$00,$02
        .byte   $00,$40,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$20,$10,$08,$00,$00,$10
        .byte   $00,$00,$02,$00,$00,$00,$00,$0A
        .byte   $00,$00,$00,$00,$00,$02,$00,$00
        .byte   $00,$00,$80,$00,$02,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$00,$20,$00,$00,$80
        .byte   $00,$00,$00,$00,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$20
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$02,$00,$00
        .byte   $00,$01,$00,$00,$08,$00,$00,$00
        .byte   $00,$00,$00,$01,$00,$04,$00,$00
        .byte   $00,$00,$00,$00,$08,$00,$00,$20
; ===========================================================================
; Screen index / room header data ($8A00, equiv $AA00)
; ===========================================================================
; Screen index table listing the order of screens in the stage.
; Terminated by $FF. Followed by room scroll config / direction data.
; ---------------------------------------------------------------------------
        .byte   $18,$19,$FF,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$08,$00,$00,$00
        .byte   $00,$A0,$00,$00,$00,$00,$00,$42
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$18,$00,$10
        .byte   $10,$2A,$36,$00,$00,$00,$08,$00
        .byte   $08,$18,$00,$00,$00,$10,$00,$00
        .byte   $40,$40,$FF,$00,$00,$00,$20,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$00
        .byte   $00,$01,$00,$00,$00,$00,$00,$02
; --- Room CHR/palette config ($8A60, equiv $AA60) ---
        .byte   $00,$00,$00,$A0,$00,$18,$00,$00
        .byte   $19,$00,$34,$01,$FF,$00,$00,$00
        .byte   $00,$02,$08,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$02
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
; --- BG palette data ($8A80, equiv $AA80) ---
        .byte   $64,$6A,$0F,$30,$22,$0F,$0F,$0A
        .byte   $09,$01,$0F,$30,$25,$15,$0F,$37
        .byte   $17,$05,$00,$00,$00,$00,$0F,$30
        .byte   $22,$0F,$0F,$30,$27,$17,$0F,$30
        .byte   $25,$15,$0F,$30,$27,$15,$00,$00
        .byte   $00,$00,$00,$00,$00,$28,$00,$00
        .byte   $00,$04,$00,$10,$00,$00,$00,$20
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$82,$00,$00,$90
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$00
        .byte   $00,$00,$80,$00,$40,$00,$00,$00
        .byte   $02,$02,$00,$00,$00,$00,$00,$04
        .byte   $FF,$FF,$FF,$00,$00,$04,$00,$00
; --- Enemy placement: screen numbers ($8B00, equiv $AB00, $FF-terminated) ---
        .byte   $00,$00,$00,$00,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$FF,$00,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$01
        .byte   $00,$20,$20,$00,$00,$00,$20,$10
        .byte   $00,$00,$02,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$80
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$04,$00,$00
        .byte   $00,$80,$00,$10,$00,$01,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$02
        .byte   $00,$00,$00,$08,$00,$00,$00,$02
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$80,$00
        .byte   $00,$08,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$08,$00,$00,$02
        .byte   $20,$40,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$80,$08,$00,$40,$00,$80
        .byte   $00,$00,$00,$00,$00,$00,$00,$40
        .byte   $00,$00,$00,$20,$00,$00,$00,$00
        .byte   $0D,$0D,$00,$00,$00,$04,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$00,$20,$00,$00,$00,$00,$20
        .byte   $00,$00,$80,$80,$00,$00,$00,$A0
        .byte   $00,$00,$00,$21,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$00,$02
; --- Enemy placement: X pixel positions ($8C00, equiv $AC00, $FF-terminated) ---
        .byte   $28,$58,$A8,$D8,$80,$80,$48,$B8
        .byte   $48,$B8,$48,$B8,$80,$FF,$00,$00
        .byte   $00,$11,$00,$00,$01,$00,$00,$00
        .byte   $40,$00,$01,$01,$00,$00,$00,$00
        .byte   $00,$00,$28,$40,$00,$00,$02,$04
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$20,$00,$00,$00,$00,$00
        .byte   $02,$00,$00,$40,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$10,$01,$00
        .byte   $00,$00,$08,$00,$80,$10,$22,$00
        .byte   $00,$00,$00,$04,$00,$00,$40,$04
        .byte   $40,$00,$00,$00,$00,$00,$20,$00
        .byte   $80,$00,$A4,$00,$04,$00,$00,$00
        .byte   $00,$10,$C0,$00,$44,$04,$00,$00
        .byte   $04,$00,$02,$00,$00,$00,$00,$04
        .byte   $00,$04,$02,$00,$05,$00,$02,$00
        .byte   $00,$00,$04,$00,$00,$00,$01,$00
        .byte   $00,$00,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$08,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$00
        .byte   $01,$00,$82,$00,$80,$00,$12,$04
        .byte   $46,$01,$20,$01,$04,$40,$00,$04
        .byte   $08,$00,$0A,$04,$00,$00,$50,$10
        .byte   $00,$40,$00,$40,$81,$00,$00,$01
        .byte   $02,$00,$00,$04,$00,$00,$00,$00
        .byte   $92,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$81,$00
        .byte   $00,$00,$00,$00,$00,$00,$40,$00
        .byte   $04,$00,$14,$01,$4C,$80,$48,$00
        .byte   $00,$00,$02,$10,$10,$00,$02,$00
        .byte   $02,$00,$02,$10,$00,$02,$00,$00
        .byte   $08,$00,$40,$14,$00,$00,$00,$00
; --- Enemy placement: Y pixel positions ($8D00, equiv $AD00, $FF-terminated) ---
        .byte   $58,$78,$78,$58,$4F,$98,$7B,$7B
        .byte   $A0,$A0,$7B,$7B,$4F,$FF,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $40,$00,$01,$00,$02,$00,$20,$00
        .byte   $48,$00,$10,$00,$48,$10,$84,$50
        .byte   $00,$00,$80,$00,$00,$01,$09,$00
        .byte   $25,$00,$12,$00,$00,$40,$00,$00
        .byte   $00,$00,$08,$00,$20,$00,$80,$00
        .byte   $01,$01,$00,$00,$00,$00,$20,$40
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $00,$00,$94,$00,$80,$00,$44,$11
        .byte   $20,$01,$82,$00,$20,$00,$00,$00
        .byte   $03,$40,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$01,$00,$00,$00,$04,$00
        .byte   $00,$04,$00,$00,$08,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$04,$00
        .byte   $00,$40,$14,$00,$14,$00,$02,$10
        .byte   $8A,$00,$00,$00,$20,$01,$80,$00
        .byte   $00,$01,$00,$00,$02,$00,$18,$00
        .byte   $01,$00,$10,$00,$00,$00,$00,$00
        .byte   $82,$00,$20,$00,$04,$00,$40,$01
        .byte   $00,$01,$08,$01,$00,$00,$00,$00
        .byte   $00,$00,$80,$00,$01,$10,$20,$00
        .byte   $08,$00,$00,$00,$00,$00,$02,$00
        .byte   $C4,$00,$88,$54,$89,$01,$02,$01
        .byte   $C2,$00,$A0,$01,$22,$04,$80,$00
        .byte   $0E,$00,$20,$01,$70,$00,$08,$01
        .byte   $00,$00,$20,$40,$40,$00,$00,$00
; --- Enemy placement: global enemy IDs ($8E00, equiv $AE00, $FF-terminated) ---
        .byte   $56,$52,$52,$56,$82,$83,$88,$89
        .byte   $84,$84,$85,$86,$87,$FF,$00,$00
        .byte   $00,$00,$00,$00,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$04,$00,$00
        .byte   $40,$00,$00,$40,$00,$00,$08,$10
        .byte   $00,$00,$00,$00,$50,$00,$00,$00
        .byte   $81,$00,$00,$00,$00,$00,$00,$00
        .byte   $48,$00,$40,$00,$08,$00,$00,$00
        .byte   $80,$40,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $20,$00,$01,$00,$40,$00,$01,$00
        .byte   $02,$10,$04,$00,$00,$00,$00,$00
        .byte   $02,$00,$40,$00,$04,$10,$01,$00
        .byte   $20,$00,$08,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$60,$40
        .byte   $00,$00,$00,$00,$02,$00,$44,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$80,$10,$00,$00
        .byte   $20,$00,$09,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$10,$00
        .byte   $00,$04,$20,$00,$00,$01,$00,$01
        .byte   $00,$10,$08,$00,$08,$01,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$10,$00
        .byte   $0A,$01,$20,$40,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$40,$01
        .byte   $00,$00,$80,$00,$20,$00,$10,$00
        .byte   $00,$01,$04,$00,$00,$00,$03,$00
        .byte   $20,$00,$01,$00,$20,$00,$82,$00
        .byte   $01,$00,$00,$05,$10,$04,$00,$01
        .byte   $20,$10,$00,$00,$00,$00,$09,$00
        .byte   $90,$00,$00,$40,$00,$00,$10,$00
        .byte   $00,$00,$00,$41,$80,$10,$00,$04
; ===========================================================================
; Nametable screen map ($8F00, equiv $AF00)
; ===========================================================================
; Each screen is 8 columns x 8 rows of metatile column indices.
; Columns index into the metatile column definition table at $9700.
; Screens appear in the order defined by the screen index table.
; Repeated $69 entries are empty fill (unused screen slots).
; ---------------------------------------------------------------------------
        .byte   $00,$01,$01,$02,$03,$04,$01,$05
        .byte   $06,$07,$08,$09,$0A,$0B,$0C,$06
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $01,$01,$15,$16,$17,$18,$01,$01
        .byte   $01,$01,$01,$01,$01,$19,$1A,$1B
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $01,$01,$1C,$1D,$01,$1E,$1F,$1D
        .byte   $01,$01,$20,$21,$01,$22,$23,$21
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $24,$01,$01,$01,$25,$1F,$1D,$01
        .byte   $26,$27,$01,$01,$28,$29,$21,$01
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $2A,$2B,$2C,$2D,$2E,$2F,$30,$31
        .byte   $32,$33,$34,$35,$36,$37,$38,$39
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $3A,$3B,$01,$3C,$2A,$2B,$3D,$24
        .byte   $3E,$3F,$40,$09,$41,$42,$43,$44
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$45,$46,$47,$01,$01,$48,$46
        .byte   $49,$4A,$4B,$4C,$4D,$4E,$4F,$50
        .byte   $01,$01,$01,$51,$52,$53,$54,$55
        .byte   $01,$01,$01,$56,$57,$58,$59,$5A
        .byte   $5B,$15,$01,$5C,$5D,$5E,$5F,$60
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $63,$63,$63,$63,$63,$63,$63,$63
        .byte   $64,$64,$64,$64,$64,$64,$64,$64
        .byte   $65,$65,$65,$65,$65,$65,$65,$65
        .byte   $66,$66,$66,$66,$66,$66,$66,$66
        .byte   $67,$67,$67,$67,$67,$67,$67,$67
        .byte   $68,$68,$68,$68,$68,$68,$68,$68
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$45,$46,$47,$01,$01,$48,$46
        .byte   $49,$4A,$4B,$4C,$4D,$4E,$4F,$50
        .byte   $01,$01,$01,$51,$52,$53,$54,$55
        .byte   $01,$01,$01,$56,$57,$58,$59,$5A
        .byte   $5B,$15,$01,$5C,$5D,$5E,$5F,$60
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $3A,$3B,$01,$3C,$2A,$2B,$3D,$24
        .byte   $3E,$3F,$40,$09,$41,$42,$43,$44
        .byte   $0D,$0E,$0F,$10,$0E,$0E,$0F,$10
        .byte   $11,$11,$12,$13,$11,$11,$12,$13
        .byte   $14,$14,$14,$14,$14,$14,$14,$14
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
; ===========================================================================
; Metatile column definitions ($9700, equiv $B700)
; ===========================================================================
; Each column is 8 bytes defining 8 rows of metatile IDs (top to bottom).
; These are the building blocks for screen layouts, indexed by the
; nametable screen map above. Metatile IDs reference the CHR tile
; tables at $9B00-$9EFF and the collision/palette table at $9F00.
; ---------------------------------------------------------------------------
        .byte   $02,$02,$60,$06,$02,$02,$02,$02
        .byte   $02,$02,$0B,$02,$03,$04,$02,$02
        .byte   $60,$06,$02,$02,$02,$02,$02,$03
        .byte   $0F,$0C,$2F,$1B,$0F,$02,$07,$07
        .byte   $0C,$15,$1C,$1C,$16,$17,$07,$07
        .byte   $0F,$0C,$1C,$1B,$0D,$0E,$1B,$1C
        .byte   $0F,$0C,$1C,$07,$08,$09,$10,$11
        .byte   $0A,$09,$12,$11,$0A,$08,$12,$10
        .byte   $09,$0A,$11,$12,$18,$19,$70,$70
        .byte   $18,$18,$70,$70,$19,$18,$70,$70
        .byte   $70,$70,$70,$70,$02,$02,$60,$02
        .byte   $58,$59,$02,$42,$49,$49,$48,$40
        .byte   $49,$61,$41,$02,$02,$02,$02,$45
        .byte   $02,$02,$46,$47,$3D,$0E,$07,$1C
        .byte   $02,$02,$02,$51,$02,$02,$4B,$02
        .byte   $02,$58,$02,$51,$59,$4B,$49,$53
        .byte   $69,$6A,$6B,$54,$4C,$02,$55,$4D
        .byte   $02,$63,$51,$49,$49,$49,$68,$54
        .byte   $39,$02,$02,$02,$02,$58,$02,$4A
        .byte   $02,$02,$3F,$43,$02,$02,$44,$02
        .byte   $02,$62,$51,$49,$49,$49,$49,$54
        .byte   $32,$38,$02,$02,$2B,$27,$33,$01
        .byte   $01,$39,$34,$02,$02,$28,$02,$32
        .byte   $2C,$2B,$33,$5E,$2C,$2E,$27,$2C
        .byte   $2C,$2B,$01,$5E,$2C,$3C,$2D,$34
        .byte   $0D,$0E,$07,$07,$46,$1D,$20,$1E
        .byte   $3F,$43,$21,$1C,$44,$45,$1B,$1C
        .byte   $46,$1D,$20,$1F,$4E,$4F,$56,$57
        .byte   $5A,$5C,$5B,$5D,$5A,$44,$5F,$21
        .byte   $02,$05,$02,$02,$60,$02,$02,$02
        .byte   $02,$06,$0B,$02,$01,$3C,$34,$35
        .byte   $3F,$43,$1A,$1B,$44,$45,$1C,$13
        .byte   $46,$47,$2F,$14,$0F,$0C,$07,$2F
        .byte   $0F,$1D,$20,$1E,$45,$46,$21,$1C
        .byte   $44,$45,$13,$07,$02,$02,$58,$59
        .byte   $02,$02,$49,$49,$02,$02,$49,$61
        .byte   $02,$02,$42,$48,$02,$02,$04,$02
        .byte   $02,$42,$02,$02,$48,$40,$02,$02
        .byte   $41,$02,$02,$02,$02,$02,$02,$28
        .byte   $36,$37,$2D,$24,$3A,$02,$3B,$3A
        .byte   $42,$48,$02,$02,$02,$02,$02,$30
        .byte   $28,$2B,$01,$27,$22,$2B,$22,$23
        .byte   $2D,$3C,$2E,$2B,$31,$03,$29,$02
        .byte   $02,$30,$02,$28,$2D,$22,$3C,$27
        .byte   $2E,$23,$2C,$22,$24,$25,$27,$2B
        .byte   $2B,$31,$2B,$31,$02,$02,$02,$05
        .byte   $02,$38,$02,$06,$2D,$2C,$32,$38
        .byte   $22,$2B,$2B,$27,$2D,$01,$01,$3C
        .byte   $3B,$02,$39,$02,$66,$66,$6E,$6E
        .byte   $64,$64,$6C,$6C,$67,$67,$67,$67
        .byte   $71,$71,$72,$72,$72,$72,$73,$73
        .byte   $73,$73,$65,$65,$6D,$6D,$6F,$6F
        .byte   $66,$66,$66,$66,$00,$00,$00,$00
; --- Zero padding ($98A4-$9AFF) ---
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
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
; Metatile CHR tile tables ($9B00-$9EFF, equiv $BB00-$BEFF)
; ===========================================================================
; Four 256-byte tables defining the 2x2 tile pattern for each metatile.
; Each metatile ID indexes into all four tables to get its CHR tile IDs:
;   $9B00 = top-left tile      $9C00 = top-right tile
;   $9D00 = bottom-left tile   $9E00 = bottom-right tile
; These are the PPU pattern table tile numbers rendered for each metatile.
; Values $00-$FF index CHR bank patterns loaded for this stage.
; ---------------------------------------------------------------------------
; --- Top-left CHR tile ($9B00, equiv $BB00) ---
        .byte   $00,$E8,$10,$A2,$A4,$C2,$10,$10
        .byte   $A0,$B1,$B0,$10,$10,$10,$A8,$10
        .byte   $C0,$B0,$B1,$10,$AC,$10,$B7,$A9
        .byte   $E0,$D0,$AB,$AB,$10,$C6,$B2,$B2
        .byte   $10,$CE,$E6,$E6,$E6,$E6,$E6,$F8
        .byte   $01,$FD,$E9,$E8,$E6,$F8,$E9,$10
        .byte   $01,$EE,$FB,$8C,$EC,$8C,$01,$DA
        .byte   $EA,$EC,$01,$EC,$E8,$10,$A8,$B9
        .byte   $E8,$8E,$8A,$D8,$10,$10,$10,$CC
        .byte   $E8,$E8,$10,$E5,$E8,$10,$E8,$CF
        .byte   $E4,$10,$E8,$C4,$E8,$C5,$CF,$EF
        .byte   $10,$E3,$E8,$BF,$E8,$DF,$E8,$BF
        .byte   $10,$D2,$F3,$10,$44,$24,$54,$24
        .byte   $F4,$10,$E3,$10,$35,$44,$44,$54
        .byte   $E8,$24,$24,$24,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- Top-right CHR tile ($9C00, equiv $BC00) ---
        .byte   $00,$E8,$10,$A3,$A5,$C3,$10,$10
        .byte   $A1,$B0,$B1,$10,$10,$A7,$10,$10
        .byte   $C1,$B1,$B0,$AB,$AD,$A6,$B8,$10
        .byte   $E1,$D1,$AB,$10,$10,$E8,$B5,$E8
        .byte   $CD,$10,$E7,$E9,$E9,$E7,$E7,$F9
        .byte   $DA,$01,$E8,$F8,$E9,$E9,$F8,$10
        .byte   $FA,$01,$8C,$EB,$FC,$FC,$01,$FD
        .byte   $EB,$FC,$01,$FC,$DC,$A7,$10,$CA
        .byte   $E8,$8F,$8B,$C7,$10,$10,$C8,$B6
        .byte   $E8,$E8,$E3,$10,$D2,$10,$AF,$D3
        .byte   $E8,$E3,$F4,$C5,$C4,$E5,$E8,$AF
        .byte   $10,$E8,$E8,$E8,$E8,$AF,$E8,$E8
        .byte   $10,$10,$E8,$F3,$44,$24,$54,$24
        .byte   $E8,$10,$E8,$F3,$35,$44,$44,$54
        .byte   $E8,$24,$24,$24,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- Bottom-left CHR tile ($9D00, equiv $BD00) ---
        .byte   $00,$E8,$10,$9A,$9C,$10,$C2,$10
        .byte   $B0,$A0,$C0,$A7,$CB,$B6,$B8,$CA
        .byte   $D0,$C0,$D0,$BA,$BC,$B6,$01,$01
        .byte   $F0,$E0,$BB,$BB,$C9,$B2,$B3,$B3
        .byte   $10,$DE,$F6,$F8,$F6,$F8,$F6,$E8
        .byte   $EA,$DC,$F9,$F8,$F8,$E8,$F6,$BA
        .byte   $01,$FE,$01,$01,$FE,$01,$FA,$DB
        .byte   $01,$ED,$9F,$DC,$E8,$A6,$B8,$01
        .byte   $9C,$9E,$10,$01,$D6,$10,$D9,$01
        .byte   $9A,$E8,$D4,$F4,$E8,$F5,$E8,$DF
        .byte   $E8,$F2,$C4,$F4,$E8,$E8,$DF,$FF
        .byte   $10,$E8,$AF,$E8,$CF,$FF,$E8,$E8
        .byte   $8D,$E2,$E3,$A2,$44,$36,$54,$24
        .byte   $E8,$10,$E8,$10,$24,$44,$54,$44
        .byte   $E8,$24,$24,$24,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- Bottom-right CHR tile ($9E00, equiv $BE00) ---
        .byte   $00,$E8,$10,$9B,$8F,$10,$C3,$10
        .byte   $B1,$A1,$C1,$A8,$CC,$B7,$B9,$CB
        .byte   $D1,$C1,$D1,$BB,$BD,$01,$01,$B9
        .byte   $F1,$E1,$BB,$BA,$AA,$B5,$B4,$E8
        .byte   $DD,$10,$F7,$F7,$F9,$F7,$F9,$E8
        .byte   $DB,$EE,$E8,$E9,$F9,$E8,$E9,$AA
        .byte   $FB,$01,$01,$FB,$01,$01,$9F,$DC
        .byte   $FB,$01,$EE,$9F,$E8,$B7,$A9,$01
        .byte   $9D,$10,$10,$01,$D7,$D8,$01,$01
        .byte   $9B,$E8,$D5,$F5,$E2,$10,$BF,$E8
        .byte   $E8,$E8,$C5,$E8,$E8,$F4,$E8,$BF
        .byte   $F2,$E8,$E8,$CF,$E8,$BF,$D3,$E8
        .byte   $10,$10,$E8,$A3,$44,$36,$54,$24
        .byte   $E8,$E3,$E8,$E3,$24,$44,$54,$44
        .byte   $E8,$24,$24,$24,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
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
; Collision / palette attribute table ($9F00, equiv $BF00)
; ===========================================================================
; One byte per metatile. Upper nybble encodes collision type (solid,
; ladder, spike, water, etc.), lower nybble selects the BG palette
; (0-3) for the metatile's attribute table entry.
; Values: $00 = passable/pal0, $01 = solid/pal0, $03 = solid/pal1,
;         $12 = ladder/pal0 (verify in Mesen)
; ---------------------------------------------------------------------------
        .byte   $00,$00,$01,$01,$01,$01,$01,$00
        .byte   $12,$12,$12,$01,$01,$01,$01,$01
        .byte   $12,$12,$12,$00,$00,$01,$01,$01
        .byte   $12,$12,$00,$00,$00,$03,$03,$03
        .byte   $00,$00,$03,$03,$03,$03,$03,$03
        .byte   $00,$00,$03,$03,$03,$03,$03,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$00,$00
        .byte   $01,$01,$01,$01,$01,$01,$00,$00
        .byte   $01,$01,$00,$00,$00,$00,$00,$00
        .byte   $01,$01,$01,$01,$00,$00,$00,$00
        .byte   $01,$01,$01,$01,$00,$00,$00,$00
        .byte   $00,$01,$02,$03,$00,$00,$00,$00
; --- Zero padding ($9F78-$9FFF) ---
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
