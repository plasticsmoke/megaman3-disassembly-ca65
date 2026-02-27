; ===========================================================================
; NMI — VBlank interrupt handler ($C000)
; ===========================================================================
; Entry point for the vertical blanking interrupt. Performs per-frame PPU
; updates: OAM DMA, palette copy, nametable buffer drain, scroll setup,
; IRQ vector selection, and frame counter tick.
; ===========================================================================

NMI:  php                               ; push processor status (NMI entry = $C000)
nmi_preserve_regs:  pha                 ; push A (this addr = $C001 = MMC3 IRQ reload)
        txa                             ; preserve X, Y, and
        pha                             ; processor flags
        tya                             ; preserve Y
        pha                             ; push Y

; --- NMI: disable rendering for safe PPU access during VBlank ---
        lda     PPUSTATUS               ; reset PPU address latch
        lda     ppu_ctrl_shadow         ; PPUCTRL: clear NMI enable (bit 7)
        and     #$7F                    ; to prevent re-entrant NMI
        sta     PPUCTRL                 ; write PPUCTRL with NMI disabled
        lda     #$00                    ; PPUMASK = 0: rendering off
        sta     PPUMASK                 ; (safe to write PPU during VBlank)

; --- check if PPU updates are suppressed ---
        lda     nmi_skip                ; rendering disabled?
        ora     $9A                     ; $9A = NMI lock flag
        bne     nmi_scroll_setup        ; either set → skip PPU writes, go to scroll

; --- latch scroll/mode/scanline values for this frame ---
        lda     camera_x_lo             ; latch scroll X fine
        sta     scroll_x_fine           ; latch to scroll_x_fine
        lda     camera_x_hi             ; latch nametable select
        sta     nt_select               ; latch nametable select
        lda     game_mode               ; latch game mode
        sta     screen_mode             ; latch screen mode
        lda     scroll_lock             ; if secondary split active,
        bne     nmi_oam_dma_setup       ; keep current irq_scanline (set by split code)
        lda     $5E                     ; else use default scanline count from $5E
        sta     irq_scanline            ; store default scanline count

; --- OAM DMA: transfer sprite data from $0200-$02FF to PPU ---
nmi_oam_dma_setup:  lda     #$00        ; OAMADDR = $00 (start of OAM)
        sta     OAMADDR                 ; write OAMADDR = 0
        lda     #$02                    ; OAMDMA: copy page $02 ($0200-$02FF)
        sta     OAMDMA                  ; to PPU OAM (256 bytes, 64 sprites)

; --- drain primary PPU buffer ($19 flag) ---
        lda     nametable_dirty         ; nametable_dirty = PPU buffer pending flag
        beq     nmi_drain_secondary_buffer ; no data → skip
        jsr     drain_ppu_buffer        ; write buffered tile data to PPU

; --- drain secondary PPU buffer with VRAM increment ($1A flag) ---
nmi_drain_secondary_buffer:  lda     nt_column_dirty ; nt_column_dirty = secondary buffer flag (vertical writes)
        beq     nmi_palette_update      ; no data → skip
        lda     ppu_ctrl_shadow         ; PPUCTRL: set bit 2 (VRAM addr +32 per write)
        and     #$7F                    ; for vertical column writes to nametable
        ora     #$04                    ; set VRAM +32 increment bit
        sta     PPUCTRL                 ; write PPUCTRL with +32 VRAM increment
        ldx     #$00                    ; clear $1A flag
        stx     nt_column_dirty         ; clear secondary buffer flag
        jsr     drain_ppu_buffer_continue ; write column data to PPU
        lda     ppu_ctrl_shadow         ; PPUCTRL: restore normal increment (+1)
        and     #$7F                    ; clear NMI enable bit
        sta     PPUCTRL                 ; write PPUCTRL with +1 VRAM increment

; --- write palette data ($18 flag) ---
nmi_palette_update:  lda     palette_dirty ; palette update pending?
        beq     nmi_scroll_setup        ; no update → skip to scroll
        ldx     #$00                    ; clear $18 flag
        stx     palette_dirty           ; clear palette dirty flag
        lda     PPUSTATUS               ; reset PPU latch
        lda     #$3F                    ; PPU addr = $3F00 (palette RAM start)
        sta     PPUADDR                 ; set PPU addr high = $3F
        stx     PPUADDR                 ; set PPU addr low = $00
        ldy     #$20                    ; 32 bytes = all 8 palettes (4 BG + 4 sprite)
nmi_palette_copy_loop:  lda     $0600,x ; copy palette from $0600-$061F to PPU
        sta     PPUDATA                 ; write palette byte to PPU
        inx                             ; next byte
        dey                             ; decrement count
        bne     nmi_palette_copy_loop   ; loop all 32 palette bytes
        lda     #$3F                    ; reset PPU address to $3F00 then $0000
        sta     PPUADDR                 ; (two dummy writes to clear PPU latch
        sty     PPUADDR                 ; and prevent palette corruption during
        sty     PPUADDR                 ; scroll register writes below)
        sty     PPUADDR                 ; second dummy write

; --- set scroll position for this frame ---
nmi_scroll_setup:  lda     screen_mode  ; game mode $02 = auto-scroll (Gemini)
        cmp     #$02                    ; (uses horizontal-only scroll from $5F)
        bne     nmi_scroll_mode_check   ; not auto-scroll → use gameplay scroll
        lda     PPUSTATUS               ; reset PPU latch
        lda     $5F                     ; PPUSCROLL X = $5F (auto-scroll X position)
        sta     PPUSCROLL               ; write X scroll for auto-scroll mode
        lda     #$00                    ; PPUSCROLL Y = 0
        sta     PPUSCROLL               ; write Y scroll = 0
        beq     nmi_restore_rendering   ; → restore rendering
nmi_scroll_mode_check:  lda     PPUSTATUS ; reset PPU latch
        lda     scroll_x_fine           ; PPUSCROLL X = $79 (gameplay scroll X)
        sta     PPUSCROLL               ; write gameplay X scroll
        lda     scroll_y                ; PPUSCROLL Y = $FA (vertical scroll offset)
        sta     PPUSCROLL               ; write gameplay Y scroll

; --- restore rendering and CHR banks ---
nmi_restore_rendering:  lda     ppu_mask_shadow ; PPUMASK = $FE (re-enable rendering)
        sta     PPUMASK                 ; re-enable rendering
        lda     nt_select               ; PPUCTRL = $FF | (nt_select & $03)
        and     #$03                    ; bits 0-1 from $7A = nametable select
        ora     ppu_ctrl_shadow         ; rest from ppu_ctrl_shadow (NMI enable, sprite table, etc.)
        sta     PPUCTRL                 ; write PPUCTRL with nametable
        jsr     select_CHR_banks        ; set MMC3 CHR bank registers
        lda     mmc3_shadow             ; $8000 = MMC3 bank select register
        sta     MMC3_BANK_SELECT        ; (restore R6/R7 select state from $F0)

; --- NMI: set up MMC3 scanline IRQ for this frame ---
; irq_scanline = scanline count for first IRQ. irq_enable = flag (0=off, 1=on).
; screen_mode = game mode, used to index irq_vector_table for handler address.
; scroll_lock/$51 = secondary split: if scroll_lock != 0, use gameplay handler.
        lda     irq_scanline            ; scanline count for first split
        sta     NMI                     ; set MMC3 IRQ counter value
        sta     nmi_preserve_regs       ; latch counter (reload)
        ldx     irq_enable              ; IRQ enable flag
        sta     auto_walk_spawn_done,x  ; $9B=0 → $E000 (disable), $9B=1 → $E001 (enable)
        beq     nmi_frame_counter_tick  ; if disabled, skip vector setup
        ldx     screen_mode             ; X = game mode (index into vector table)
        lda     scroll_lock             ; secondary split flag
        beq     nmi_irq_vector_setup    ; if no secondary split, use mode index
        lda     irq_scanline            ; if irq_scanline < $51, use mode index
        cmp     $51                     ; (first split happens before secondary)
        bcc     nmi_irq_vector_setup    ; first split before secondary
        ldx     #$01                    ; else override: use index 1 (gameplay)
nmi_irq_vector_setup:  lda     irq_vector_lo,x ; IRQ vector low byte from table
        sta     irq_handler_ptr         ; store handler address low byte
        lda     irq_vector_hi,x         ; IRQ vector high byte from table
        sta     $9D                     ; → $9C/$9D = handler address for JMP ($009C)

; --- NMI: frame counter and sound envelope timers ---
nmi_frame_counter_tick:  inc     frame_counter ; frame_counter = frame counter (increments every NMI)
        ldx     #$FF                    ; $90 = $FF (signal: NMI occurred this frame)
        stx     nmi_occurred            ; signal NMI occurred
        inx                             ; X = 0
        ldy     #$04                    ; 4 sound channels ($80-$8F, 4 bytes each)
nmi_sound_channel_loop:  lda     $80,x  ; channel state: $01 = envelope counting down
        cmp     #$01                    ; check if envelope is counting down
        bne     nmi_channel_next        ; not counting down, skip
        dec     $81,x                   ; decrement envelope timer
        bne     nmi_channel_next        ; not zero → still counting
        lda     #$04                    ; timer expired → set state to $04 (release)
        sta     $80,x                   ; set channel state to $04 (release)
nmi_channel_next:  inx                  ; advance to next channel (+4 bytes)
        inx                             ; advance channel index +1
        inx                             ; advance channel index +2
        inx                             ; advance channel index +3
        dey                             ; loop 4 channels
        bne     nmi_sound_channel_loop  ; loop all 4 channels
        tsx                             ; get current stack pointer into X
        lda     $0107,x                 ; read return address high byte from stack
        sta     $7D                     ; preserve original address
        lda     $0106,x                 ; read return address low byte from stack
        sta     $7C                     ; save to $7C ($7C/$7D = interrupted PC)
        lda     #$C1                    ; load $C1 (high byte of fake return)
        sta     $0107,x                 ; overwrite stack return addr high byte
        lda     #$21                    ; load $21 (low byte → RTI resumes at $C121)
        sta     $0106,x                 ; overwrite stack return addr low byte
        pla                             ; begin register restore
        tay                             ; restore Y
        pla                             ; restore X, Y, and P flags
        tax                             ; and clear interrupt flag
        pla                             ; restore A
        plp                             ; restore processor flags
        rti                             ; FAKE RETURN

; does not actually return, stack is hardcoded to go right here
; another preserve & restore just to call play_sounds
; this is done in case NMI happened in the middle of selecting PRG banks
; because play_sounds also selects PRG banks - possible race condition
; is handled in play_sounds routine

        php                             ; leave a stack slot for word sized
        php                             ; return address for the RTS
        php                             ; 3rd push for return address frame
        pha                             ; push A
        txa                             ; once again, preserve X, Y & flags
        pha                             ; push X
        tya                             ; preserve Y
        pha                             ; push Y
        tsx                             ; patch return address on stack:
        sec                             ; $7C/$7D = original PC saved by NMI.
        lda     $7C                     ; Subtract 1 because RTS adds 1.
        sbc     #$01                    ; Overwrites the stacked return address
        sta     $0105,x                 ; at SP+5/6 (behind P,A,X,Y,P).
        lda     $7D                     ; This makes the upcoming RTS
        sbc     #$00                    ; resume at the interrupted location.
        sta     $0106,x                 ; write return addr high byte
        jsr     play_sounds             ; call sound engine (safe from bank conflicts)
        pla                             ; begin register restore
        tay                             ; restore Y
        pla                             ; once again, restore X, Y & flags
        tax                             ; restore X
        pla                             ; restore A
        plp                             ; restore processor flags
        rts                             ; this is the "true" RTI

