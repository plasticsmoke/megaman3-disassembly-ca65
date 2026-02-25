; ===========================================================================
; IRQ — MMC3 scanline IRQ entry point
; ===========================================================================
; Triggered by the MMC3 mapper when the scanline counter ($7B) reaches zero.
; Saves registers, acknowledges the IRQ, re-enables it, then dispatches
; to the handler whose address is stored at $9C/$9D (set by NMI each frame
; from the irq_vector_table indexed by game mode $78).
;
; The IRQ handlers implement mid-frame scroll splits. For example, during
; stage transitions ($78=$07), a two-IRQ chain creates a 3-strip effect:
;   1. NMI sets top strip scroll + first counter ($7B = scanline 88)
;   2. First IRQ ($C297) sets middle strip scroll, chains to second handler
;   3. Second IRQ ($C2D2) restores bottom strip scroll
; ---------------------------------------------------------------------------

IRQ:  php                               ; push processor status (IRQ entry)
        pha                             ; save registers
        txa                             ; preserve X
        pha                             ; push X
        tya                             ; preserve Y
        pha                             ; push Y
        sta     auto_walk_spawn_done    ; acknowledge MMC3 IRQ (disable)
        sta     weapon_hurt_timer_done  ; re-enable MMC3 IRQ
        jmp     (irq_handler_ptr)       ; dispatch to current handler

; ===========================================================================
; irq_gameplay_status_bar — split after HUD for gameplay area ($C152)
; ===========================================================================
; Fires after the status bar scanlines. Sets PPU scroll/nametable for the
; gameplay area below. $52 encodes the PPU coarse Y and nametable for the
; gameplay viewport. When mode $0B is active, falls through to a simpler
; reset-to-origin variant.
; ---------------------------------------------------------------------------

irq_gameplay_status_bar:  lda     screen_mode ; check game mode
        cmp     #$0B                    ; mode $0B (title)?
        beq     irq_gameplay_status_bar_branch ; → simplified scroll
        lda     PPUSTATUS               ; reset PPU latch
        lda     $52                     ; $2006 = $52:$C0
        sta     PPUADDR                 ; (sets coarse Y, nametable, coarse X=0)
        lda     #$C0                    ; PPU addr low = $C0
        sta     PPUADDR                 ; write PPU addr low byte
        lda     $52                     ; PPUCTRL: nametable from ($52 >> 2) & 3
        lsr     a                       ; merged with base $98 (NMI on, BG $1000)
        lsr     a                       ; shift right for nametable bits
        and     #$03                    ; isolate nametable bits
        ora     #$98                    ; merge with NMI-on + BG pattern $1000
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; fine X = 0, fine Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        jmp     irq_exit                ; done, exit IRQ handler

irq_gameplay_status_bar_branch:  lda     PPUSTATUS ; reset PPU latch
        lda     #$20                    ; $2006 = $20:$00
        sta     PPUADDR                 ; (nametable 0, origin)
        lda     #$00                    ; PPU addr low = $00
        sta     PPUADDR                 ; write PPU addr low byte
        lda     #$98                    ; PPUCTRL = $98 (NT 0, NMI, BG $1000)
        sta     PPUCTRL                 ; write PPUCTRL (NT 0, NMI, BG)
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_gameplay_hscroll — horizontal scroll for gameplay area ($C198)
; ===========================================================================
; Sets X scroll to $79 (the gameplay horizontal scroll position).
; If secondary split is enabled ($50 != 0), chains to status bar handler
; for an additional split at scanline $51.
; ---------------------------------------------------------------------------
irq_gameplay_hscroll:

        lda     PPUSTATUS               ; reset PPU latch
        lda     scroll_x_fine           ; X scroll = $79
        sta     PPUSCROLL               ; write gameplay X scroll
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     scroll_lock             ; secondary split enabled?
        beq     irq_jmp_exit_disable    ; no → last split
        lda     $51                     ; counter = $51 - $9F
        sec                             ; (scanlines until secondary split)
        sbc     #$9F                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for next split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_jmp_exit_disable:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_gameplay_ntswap — nametable swap after HUD ($C1C1)
; ===========================================================================
; Switches PPU to nametable 2 ($2800) at scroll origin (0,0).
; Used for vertical level layouts where the gameplay area uses a different
; nametable than the HUD. Chains to the next handler indexed by $78,
; unless secondary split overrides to mode $00 (no-op).
; ---------------------------------------------------------------------------
irq_gameplay_ntswap:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$28                    ; $2006 = $28:$00
        sta     PPUADDR                 ; (nametable 2 origin)
        lda     #$00                    ; PPU addr low = $00
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: set nametable bit 1
        ora     #$02                    ; → nametable 2
        sta     PPUCTRL                 ; write PPUCTRL with NT 2
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$B0                    ; counter = $B0 - $7B
        sec                             ; (scanlines to next split)
        sbc     irq_scanline            ; subtract base scanline count
        sta     NMI                     ; set MMC3 IRQ counter for next split
        ldx     screen_mode             ; chain index = current game mode
        lda     scroll_lock             ; secondary split?
        beq     irq_gameplay_ntswap_chain ; no → use mode index
        lda     $51                     ; if $51 == $B0, override to mode $00
        cmp     #$B0                    ; (disable further splits)
        bne     irq_gameplay_ntswap_chain ; not $B0, use mode index
        ldx     #$00                    ; X = 0 → irq_exit_disable handler
irq_gameplay_ntswap_chain:  lda     irq_vector_lo_gameplay_status,x ; chain to handler for mode X
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status,x ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_gameplay_vscroll — vertical scroll for gameplay area ($C200)
; ===========================================================================
; Sets PPU to $22C0 (nametable 0, bottom portion) with Y scroll = $B0 (176).
; Used for stages with vertical scrolling — positions the viewport to show
; the bottom part of the nametable. Optionally chains to status bar handler.
; ---------------------------------------------------------------------------
irq_gameplay_vscroll:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$22                    ; $2006 = $22:$C0
        sta     PPUADDR                 ; (nametable 0, coarse Y ≈ 22)
        lda     #$C0                    ; PPU addr low = $C0
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL from base value
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; X scroll = 0
        sta     PPUSCROLL               ; write X scroll = 0
        lda     #$B0                    ; Y scroll = $B0 (176)
        sta     PPUSCROLL               ; write Y scroll = $B0
        lda     scroll_lock             ; secondary split?
        beq     irq_jmp_exit_disable    ; no → exit disabled
        lda     $51                     ; counter = $51 - $B0
        sec                             ; set carry for subtraction
        sbc     #$B0                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for next split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_stagesel_first — stage select X scroll (mode $05)
; ===========================================================================
; Sets scroll for the upper portion of the stage select screen using the
; standard NES mid-frame $2006/$2005 trick. X scroll comes from $79.
; Chains to irq_stagesel_second after $C0 - $7B scanlines.
; ---------------------------------------------------------------------------
irq_stagesel_first:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$20                    ; $2006 = $20:coarseX
        sta     PPUADDR                 ; (nametable 0, coarse Y=0)
        lda     scroll_x_fine           ; load X scroll position
        lsr     a                       ; coarse X = $79 >> 3
        lsr     a                       ; shift right for coarse X
        lsr     a                       ; continue shift
        and     #$1F                    ; mask to 0-31
        ora     #$00                    ; coarse Y=0 (top)
        sta     PPUADDR                 ; write PPU address low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; clear nametable bits
        sta     PPUCTRL                 ; write PPUCTRL
        lda     scroll_x_fine           ; fine X scroll = $79
        sta     PPUSCROLL               ; write fine X scroll
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$C0                    ; counter = $C0 - $7B
        sec                             ; (scanlines to bottom split)
        sbc     irq_scanline            ; subtract base scanline count
        sta     NMI                     ; set MMC3 IRQ counter for next split
        lda     irq_vector_lo_stagesel_second ; chain to irq_stagesel_second ($C26F)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_stagesel_second ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_stagesel_second — stage select bottom scroll (mode $06)
; ===========================================================================
; Sets scroll for the bottom portion of the stage select screen.
; Uses nametable 0 at high coarse Y ($23xx). Last split — disables IRQ.
; ---------------------------------------------------------------------------
irq_stagesel_second:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$23                    ; $2006 = $23:coarseX
        sta     PPUADDR                 ; (nametable 0, high coarse Y)
        lda     scroll_x_fine           ; load X scroll position
        lsr     a                       ; coarse X = $79 >> 3
        lsr     a                       ; continue shift
        lsr     a                       ; continue shift
        and     #$1F                    ; mask to 0-31 tile columns
        ora     #$00                    ; coarse Y = 0 (no-op)
        sta     PPUADDR                 ; write PPU address low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; (select nametable 0)
        sta     PPUCTRL                 ; write PPUCTRL
        lda     scroll_x_fine           ; fine X scroll = $79
        sta     PPUSCROLL               ; write fine X scroll
        lda     #$C0                    ; Y scroll = $C0 (192)
        sta     PPUSCROLL               ; write Y scroll = $C0
        jmp     irq_exit_disable        ; last split

; ===========================================================================
; irq_transition_first_split — middle strip scroll (mode $07, scanline 88)
; ===========================================================================
; First IRQ handler during stage transitions. Fires at scanline $7B (88).
; Sets scroll for the MIDDLE strip (blue boss intro band, y=88-151).
;
; The top strip scrolls left via $FC/$FD (main scroll). The middle strip
; must appear FIXED, so this handler NEGATES the scroll values ($79/$7A),
; canceling the horizontal movement for the middle band.
;
; After setting the middle strip scroll, it programs the next IRQ to fire
; 64 scanlines later (at scanline 152) and chains to the second split
; handler (irq_transition_second_split at $C2D2).
; ---------------------------------------------------------------------------
irq_transition_first_split:

        lda     PPUSTATUS               ; reset PPU address latch
        lda     scroll_x_fine           ; negate scroll_x_fine/$7A (two's complement)
        eor     #$FF                    ; inverted_X = -$79
        clc                             ; clear carry for addition
        adc     #$01                    ; complete two's complement
        sta     irq_handler_ptr         ; $9C = inverted X scroll (temp)
        lda     nt_select               ; negate high byte with carry
        eor     #$FF                    ; negate high byte
        adc     #$00                    ; carry propagates the negation
        and     #$01                    ; keep only nametable bit
        sta     $9D                     ; $9D = inverted nametable select
        lda     ppu_ctrl_shadow         ; PPUCTRL: clear NT bits, set inverted NT
        and     #$FC                    ; clear nametable bits from base
        ora     $9D                     ; merge inverted nametable select
        sta     PPUCTRL                 ; → middle strip uses opposite nametable
        lda     irq_handler_ptr         ; set X scroll = negated value
        sta     PPUSCROLL               ; (cancels horizontal movement)
        lda     #$58                    ; set Y scroll = $58 (88)
        sta     PPUSCROLL               ; (top of middle band)
        lda     #$40                    ; set next IRQ at 64 scanlines later
        sta     NMI                     ; (scanline 88+64 = 152)
        lda     irq_vector_lo_transition_second ; chain to second split handler
        sta     irq_handler_ptr         ; $9C/$9D → $C2D2
        lda     irq_vector_hi_transition_second ; (irq_transition_second_split)
        sta     $9D                     ; store chain handler high byte
        jmp     irq_exit                ; exit without disabling IRQ

; ===========================================================================
; irq_transition_second_split — bottom strip scroll (mode $07, scanline 152)
; ===========================================================================
; Second IRQ handler during stage transitions. Fires at scanline 152.
; Restores scroll for the BOTTOM strip (y=152-239), which scrolls the same
; direction as the top strip (the stage select background scrolling away).
;
; Uses the $2006/$2006/$2000/$2005/$2005 sequence — the standard NES
; mid-frame scroll trick that sets both coarse and fine scroll via PPU
; address register manipulation.
; ---------------------------------------------------------------------------
irq_transition_second_split:

        lda     PPUSTATUS               ; reset PPU address latch
        lda     nt_select               ; compute PPU $2006 high byte:
        and     #$01                    ; nametable bit → bits 2-3
        asl     a                       ; ($7A & 1) << 2 | $22
        asl     a                       ; → $22 (NT 0) or $26 (NT 1)
        ora     #$22                    ; merge with nametable 0 base
        sta     PPUADDR                 ; write PPU address high byte
        lda     scroll_x_fine           ; compute PPU $2006 low byte:
        lsr     a                       ; ($79 >> 3) = coarse X scroll
        lsr     a                       ; $60 = coarse Y=12 (scanline 96)
        lsr     a                       ; continue shift for coarse X
        and     #$1F                    ; mask to 5-bit coarse X
        ora     #$60                    ; merge with coarse Y = 12
        sta     PPUADDR                 ; write PPU address low byte
        lda     nt_select               ; PPUCTRL: set nametable bits from nt_select
        and     #$03                    ; isolate nametable bits from $7A
        ora     ppu_ctrl_shadow         ; merge with base PPUCTRL shadow
        sta     PPUCTRL                 ; write PPUCTRL
        lda     scroll_x_fine           ; fine X scroll = $79 (low 3 bits used)
        sta     PPUSCROLL               ; write PPUSCROLL X
        lda     #$98                    ; Y scroll = $98 (152) — bottom strip
        sta     PPUSCROLL               ; write PPUSCROLL Y = 152
        jmp     irq_exit_disable        ; last split — disable IRQ

; ===========================================================================
; irq_wave_set_strip — water wave scroll strip (mode $09)
; ===========================================================================
; Creates a water wave effect (Gemini Man stage) by alternating X scroll
; direction across strips. $73 indexes the current strip (0-2).
;   Strip 0: X = -$69 + 1,  Y = $30 (48)
;   Strip 1: X =  $69,      Y = $60 (96)
;   Strip 2: X = -$69 + 1,  Y = $90 (144)
; Chains to irq_wave_advance after 14 scanlines.
; ---------------------------------------------------------------------------
irq_wave_set_strip:

        lda     PPUSTATUS               ; reset PPU latch
        ldy     $73                     ; Y = strip index (0-2)
        lda     $69                     ; X scroll = $69 EOR mask + offset
        eor     wave_eor_masks,y        ; masks: $FF/$00/$FF (negate strips 0,2)
        clc                             ; offsets: $01/$00/$01
        adc     wave_adc_offsets,y      ; → alternating wave scroll
        sta     PPUSCROLL               ; write wave X scroll
        lda     wave_y_scroll_set,y     ; Y scroll from table: $30/$60/$90
        sta     PPUSCROLL               ; write wave Y scroll
        lda     #$0E                    ; next IRQ in 14 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (14 scanlines)
        lda     irq_vector_lo_wave_advance ; chain to irq_wave_advance ($C32B)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_wave_advance ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_wave_advance — advance wave strip counter (mode $0A)
; ===========================================================================
; Resets X scroll to 0 between wave strips, advances $73 counter.
; If all 3 strips done, optionally chains to secondary split.
; Otherwise loops back to irq_wave_set_strip after 32 scanlines.
; ---------------------------------------------------------------------------
irq_wave_advance:

        lda     PPUSTATUS               ; reset PPU latch
        ldy     $73                     ; Y = strip index
        lda     #$00                    ; X scroll = 0 (reset between strips)
        sta     PPUSCROLL               ; write X scroll = 0
        lda     wave_y_scroll_advance,y ; Y scroll from table: $40/$70/$A0
        sta     PPUSCROLL               ; write advance Y scroll
        inc     $73                     ; advance to next strip
        lda     $73                     ; check strip count
        cmp     #$03                    ; all 3 strips done?
        beq     irq_wave_all_strips_done ; yes → finish
        lda     #$20                    ; next IRQ in 32 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (32 scanlines)
        lda     irq_vector_lo_wave_set_strip ; chain back to irq_wave_set_strip ($C302)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_wave_set_strip ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_wave_all_strips_done:  lda     #$00 ; reset strip counter
        sta     $73                     ; reset strip index to 0
        lda     scroll_lock             ; secondary split?
        beq     irq_wave_last_split     ; no → last split
        lda     $51                     ; counter = $51 - $A0
        sec                             ; set carry for subtraction
        sbc     #$A0                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for secondary split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_wave_last_split:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_title_first — title/password screen first split (mode $0B)
; ===========================================================================
; Sets scroll to $2140 (nametable 0, tile row 10) with X=0, Y=0.
; This positions the main content area of the title/password screen.
; Chains to irq_title_second after $4C (76) scanlines.
; ---------------------------------------------------------------------------
irq_title_first:

        lda     PPUSTATUS               ; reset PPU latch
        lda     #$21                    ; $2006 = $21:$40
        sta     PPUADDR                 ; (nametable 0, tile row 10)
        lda     #$40                    ; PPU addr low = $40
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; (select nametable 0)
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$4C                    ; next IRQ in 76 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (76 scanlines)
        lda     irq_vector_lo_title_cutscene ; chain to irq_title_second ($C3A3)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_title_cutscene ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_title_second — title/password screen X scroll (mode $0C)
; ===========================================================================
; Sets X scroll from $6A for the bottom portion of the title screen.
; Optionally chains to secondary split for HUD overlay.
; ---------------------------------------------------------------------------
irq_title_second:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $6A                     ; X scroll = $6A
        sta     PPUSCROLL               ; write X scroll from $6A
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     scroll_lock             ; secondary split?
        beq     irq_title_second_last_split ; no → last split
        lda     $51                     ; counter = $51 - $A0
        sec                             ; set carry for subtraction
        sbc     #$A0                    ; subtract scanline offset
        sta     NMI                     ; set MMC3 IRQ counter for secondary split
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_title_second_last_split:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_cutscene_scroll — cutscene full scroll setup (mode $0D)
; ===========================================================================
; Full $2006/$2005 mid-frame scroll using $6A (X), $6B (nametable), $7B (Y).
; Used for cutscene/intro sequences with arbitrary scroll positioning.
; Chains to irq_cutscene_secondary after $AE - $7B scanlines.
; ---------------------------------------------------------------------------
irq_cutscene_scroll:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $6B                     ; $2006 high = ($6B << 2) | $20
        asl     a                       ; → $20 (NT 0) or $24 (NT 1)
        asl     a                       ; shift left for nametable bits
        ora     #$20                    ; merge with nametable 0 base
        sta     PPUADDR                 ; write PPU address high byte
        lda     $6A                     ; $2006 low = ($6A >> 3) | $E0
        lsr     a                       ; coarse X from $6A, high coarse Y
        lsr     a                       ; shift right for coarse X
        lsr     a                       ; continue shift
        ora     #$E0                    ; merge with high coarse Y
        sta     PPUADDR                 ; write PPU address low byte
        lda     $6A                     ; fine X scroll = $6A
        sta     PPUSCROLL               ; write PPUSCROLL X
        lda     irq_scanline            ; fine Y scroll = $7B
        sta     PPUSCROLL               ; write PPUSCROLL Y
        lda     ppu_ctrl_shadow         ; PPUCTRL: base | nametable bits from $6B
        ora     $6B                     ; merge nametable from $6B
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$AE                    ; counter = $AE - $7B
        sec                             ; (scanlines to secondary split)
        sbc     irq_scanline            ; subtract base scanline count
        sta     NMI                     ; set MMC3 IRQ counter
        lda     irq_vector_lo_chr_handlers ; chain to irq_cutscene_secondary ($C408)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_chr_handlers ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_cutscene_secondary — cutscene secondary split (mode $0E)
; ===========================================================================
; Handles the bottom portion of cutscene screens. If $50 != 0 and
; $51 - $B0 == 0, chains directly to status bar. Otherwise resets scroll
; to $22C0 (nametable 0 bottom) and optionally chains for another split.
; ---------------------------------------------------------------------------
irq_cutscene_secondary:

        lda     scroll_lock             ; secondary split enabled?
        beq     irq_cutscene_reset_scroll ; no → reset to origin
        lda     $51                     ; X = $51 - $B0
        sec                             ; (remaining scanlines)
        sbc     #$B0                    ; subtract scanline offset
        tax                             ; save remainder in X for IRQ counter
        bne     irq_cutscene_reset_scroll ; non-zero → need scroll reset
        jmp     irq_gameplay_status_bar ; zero → chain directly to status bar

irq_cutscene_reset_scroll:  lda     PPUSTATUS ; reset PPU latch
        lda     #$22                    ; $2006 = $22:$C0
        sta     PPUADDR                 ; (nametable 0, bottom portion)
        lda     #$C0                    ; PPU addr low = $C0
        sta     PPUADDR                 ; write PPU addr low byte
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable 0
        and     #$FC                    ; clear nametable bits (select NT 0)
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$00                    ; X = 0, Y = 0
        sta     PPUSCROLL               ; write X scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     scroll_lock             ; secondary split?
        beq     irq_cutscene_last_split ; no → last split
        stx     NMI                     ; counter = X (from $51 - $B0 above)
        lda     irq_vector_lo_gameplay_status ; chain to irq_gameplay_status_bar
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_gameplay_status ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

irq_cutscene_last_split:  jmp     irq_exit_disable ; tail call to irq_exit_disable

; ===========================================================================
; irq_chr_split_first — scroll + chain to CHR swap (mode $0F)
; ===========================================================================
; Sets X scroll from $69, then chains to irq_chr_split_swap after 48
; scanlines. First half of a two-part mid-frame CHR bank swap effect.
; ---------------------------------------------------------------------------
irq_chr_split_first:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $69                     ; X scroll = $69
        sta     PPUSCROLL               ; write X scroll from $69
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     #$30                    ; next IRQ in 48 scanlines
        sta     NMI                     ; set MMC3 IRQ counter (48 scanlines)
        lda     irq_vector_lo_chr_swap  ; chain to irq_chr_split_swap ($C469)
        sta     irq_handler_ptr         ; store handler low byte
        lda     irq_vector_hi_chr_swap  ; chain handler high byte
        sta     $9D                     ; store handler high byte → $9C/$9D
        jmp     irq_exit                ; done, exit IRQ handler

; ===========================================================================
; irq_chr_split_swap — scroll + mid-frame CHR bank swap (mode $10)
; ===========================================================================
; Sets X scroll from $6A with nametable from $6B, then performs a mid-frame
; CHR bank swap: swaps BG CHR to banks $66/$72, then sets up $78/$7A for
; the main loop to restore during NMI. $1B flag signals the swap occurred.
; ---------------------------------------------------------------------------
irq_chr_split_swap:

        lda     PPUSTATUS               ; reset PPU latch
        lda     $6A                     ; X scroll = $6A
        sta     PPUSCROLL               ; write X scroll from $6A
        lda     #$00                    ; Y scroll = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        lda     ppu_ctrl_shadow         ; PPUCTRL: nametable from $6B
        and     #$FC                    ; clear nametable bits
        ora     $6B                     ; merge nametable from $6B
        sta     PPUCTRL                 ; write PPUCTRL
        lda     #$66                    ; swap BG CHR bank 0 → $66
        sta     $E8                     ; store to CHR bank 0 shadow
        lda     #$72                    ; swap BG CHR bank 1 → $72
        sta     $E9                     ; store to CHR bank 1 shadow
        jsr     task_yield_clear_flag   ; apply CHR bank swap via MMC3
        lda     mmc3_shadow             ; trigger MMC3 bank latch
        sta     MMC3_BANK_SELECT        ; write MMC3 bank select register
        lda     #$78                    ; set up next banks for NMI restore
        sta     $E8                     ; BG bank 0 → $78
        lda     #$7A                    ; swap BG CHR bank 1 to $7A
        sta     $E9                     ; BG bank 1 → $7A
        inc     $1B                     ; signal main loop: CHR swap occurred
        jmp     irq_exit_disable        ; last split

; ===========================================================================
; irq_chr_swap_only — CHR bank swap without scroll change (mode $11)
; ===========================================================================
; Performs a mid-frame CHR bank swap to $66/$72 without changing scroll.
; Saves and restores $E8/$E9 so the main loop's bank setup is preserved.
; Falls through to irq_exit_disable.
; ---------------------------------------------------------------------------
irq_chr_swap_only:

        lda     $E8                     ; save current CHR bank addresses
        pha                             ; save CHR bank 0 to stack
        lda     $E9                     ; load CHR bank 1 shadow
        pha                             ; save CHR bank 1 to stack
        lda     #$66                    ; temporarily swap BG CHR bank 0 → $66
        sta     $E8                     ; store to CHR bank 0 shadow
        lda     #$72                    ; temporarily swap BG CHR bank 1 → $72
        sta     $E9                     ; store to CHR bank 1 shadow
        jsr     task_yield_clear_flag   ; apply CHR bank swap via MMC3
        lda     mmc3_shadow             ; trigger MMC3 bank latch
        sta     MMC3_BANK_SELECT        ; write MMC3 bank select register
        pla                             ; restore $E9
        sta     $E9                     ; restore CHR bank 1 shadow
        pla                             ; restore $E8
        sta     $E8                     ; restore CHR bank 0 shadow
        inc     $1B                     ; signal main loop: CHR swap occurred

; --- IRQ exit routines ---
; Handlers jump here when done. Two entry points:
;   irq_exit_disable ($C4BA): disables IRQ (last split of frame)
;   irq_exit ($C4BD): keeps IRQ enabled (more splits coming)
irq_exit_disable:  sta     auto_walk_spawn_done ; disable MMC3 IRQ (no more splits)
irq_exit:  pla                          ; restore Y
        tay                             ; restore Y
        pla                             ; restore registers
        tax                             ; restore X
        pla                             ; restore A
        plp                             ; restore processor flags
        rti                             ; return from interrupt

; ===========================================================================
; irq_vector_table — handler addresses indexed by game mode ($78)
; ===========================================================================
; NMI loads $9C/$9D from these tables: low bytes at $C4C8, high at $C4DA.
; The IRQ entry point dispatches via JMP ($009C).
;
; Index | Handler  | Purpose
; ------+----------+---------------------------------------------------
;  $00  | $C4BA    | irq_exit_disable (no-op, no splits needed)
;  $01  | $C152    | irq_gameplay_status_bar — HUD/gameplay split
;  $02  | $C198    | irq_gameplay_hscroll — horizontal scroll after HUD
;  $03  | $C1C1    | irq_gameplay_ntswap — nametable swap after HUD
;  $04  | $C200    | irq_gameplay_vscroll — vertical scroll after HUD
;  $05  | $C235    | irq_stagesel_first — stage select X scroll
;  $06  | $C26F    | irq_stagesel_second — stage select bottom (chain)
;  $07  | $C297    | irq_transition_first_split — 3-strip middle band
;  $08  | $C2D2    | irq_transition_second_split — 3-strip bottom (chain)
;  $09  | $C302    | irq_wave_set_strip — water wave strip (Gemini Man)
;  $0A  | $C32B    | irq_wave_advance — wave strip loop (chain)
;  $0B  | $C375    | irq_title_first — title/password first split
;  $0C  | $C3A3    | irq_title_second — title/password X scroll (chain)
;  $0D  | $C3CC    | irq_cutscene_scroll — full $2006/$2005 scroll
;  $0E  | $C408    | irq_cutscene_secondary — secondary split (chain)
;  $0F  | $C44A    | irq_chr_split_first — scroll + chain to CHR swap
;  $10  | $C469    | irq_chr_split_swap — scroll + mid-frame CHR swap
;  $11  | $C49C    | irq_chr_swap_only — CHR bank swap (no scroll)
; ---------------------------------------------------------------------------
; 4 unused bytes (padding/alignment)

        .byte   $00,$00,$00,$00

; low bytes of handler addresses ($C4C8, 18 entries)
irq_vector_lo:  .byte   $BA             ; modes $00-$03
irq_vector_lo_gameplay_status:  .byte   $52,$98,$C1,$00,$35
irq_vector_lo_stagesel_second:  .byte   $6F,$97
irq_vector_lo_transition_second:  .byte   $D2
irq_vector_lo_wave_set_strip:  .byte   $02
irq_vector_lo_wave_advance:  .byte   $2B,$75
irq_vector_lo_title_cutscene:  .byte   $A3,$CC ; modes $0C-$11
irq_vector_lo_chr_handlers:  .byte   $08,$4A ; modes $0C-$11
irq_vector_lo_chr_swap:  .byte   $69,$9C

; high bytes of handler addresses ($C4DA, 18 entries)
irq_vector_hi:  .byte   $C4             ; modes $00-$01
irq_vector_hi_gameplay_status:  .byte   $C1,$C1,$C1,$C2,$C2 ; modes $00-$01
irq_vector_hi_stagesel_second:  .byte   $C2,$C2
irq_vector_hi_transition_second:  .byte   $C2
irq_vector_hi_wave_set_strip:  .byte   $C3
irq_vector_hi_wave_advance:  .byte   $C3,$C3 ; modes $0A-$11
irq_vector_hi_title_cutscene:  .byte   $C3,$C3
irq_vector_hi_chr_handlers:  .byte   $C4,$C4 ; modes $0A-$11
irq_vector_hi_chr_swap:  .byte   $C4,$C4

; --- water wave scroll tables (used by irq_wave_set_strip/advance) ---
; $73 indexes strip 0-2. Strips 0,2 invert X scroll; strip 1 keeps it.
wave_eor_masks:  .byte   $FF,$00,$FF    ; EOR: negate/keep/negate X scroll
wave_adc_offsets:  .byte   $01,$00,$01  ; ADC: +1/0/+1 (two's complement fixup)
wave_y_scroll_set:  .byte   $30,$60,$90 ; Y scroll for set_strip: 48/96/144
wave_y_scroll_advance:  .byte   $40,$70,$A0 ; Y scroll for advance: 64/112/160

