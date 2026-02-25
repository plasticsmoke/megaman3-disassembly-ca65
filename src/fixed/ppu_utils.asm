; ===========================================================================
; PPU UTILITY ROUTINES — $C4F8-$C815
; ===========================================================================
; Contains:
;   drain_ppu_buffer      — flush PPU write buffer at $0780 to nametable
;   disable_nmi / enable_nmi — PPUCTRL NMI flag management
;   rendering_off / rendering_on — PPUMASK rendering control + nmi_skip
;   read_controllers      — read both joypads with DPCM-glitch mitigation
;   fill_nametable        — fill a PPU nametable with a single tile
;   prepare_oam_buffer    — clear unused OAM + draw overlay/scroll sprites
;   clear_entity_table    — deactivate all non-player entity slots
;   load_overlay_sprites  — copy ROM sprite data for stage transition overlays
;   scroll_overlay_sprites — slide overlay sprites leftward each frame
;   draw_scroll_sprites   — draw 8 camera-tracking sprites
;   fade_palette_out/in   — blocking palette fade effects
;   palette_fade_tick     — per-frame incremental palette fade
;   indirect_dispatch     — jump through word table using A as index
;   shift_register_tick   — 32-bit LFSR on $E4-$E7
; ===========================================================================

; ===========================================================================
; drain_ppu_buffer — write queued PPU updates from $0780 buffer
; ===========================================================================
; Called by NMI to flush pending nametable/attribute writes.
; Buffer format: [addr_hi][addr_lo][count][tile × (count+1)]...[FF=end]
; $19 is cleared (scroll dirty flag reset after PPU writes).
; ---------------------------------------------------------------------------
drain_ppu_buffer:  ldx     #$00         ; start at buffer offset 0
        stx     nametable_dirty         ; clear scroll-dirty flag
drain_ppu_buffer_continue:  lda     $0780,x ; read PPU addr high byte
        bmi     drain_ppu_exit          ; $FF terminator → exit
        sta     PPUADDR                 ; PPUADDR high
        lda     $0781,x                 ; read PPU addr low byte
        sta     PPUADDR                 ; PPUADDR low
        ldy     $0782,x                 ; Y = byte count
drain_ppu_write_tile_loop:  lda     $0783,x ; write tile data
        sta     PPUDATA                 ; to PPUDATA
        inx                             ; advance buffer index
        dey                             ; decrement byte count
        bpl     drain_ppu_write_tile_loop ; loop count+1 times
        inx                             ; skip past addr_hi
        inx                             ; addr_lo
        inx                             ; count fields
        bne     drain_ppu_buffer_continue ; next buffer entry
drain_ppu_exit:  rts

; ---------------------------------------------------------------------------
; disable_nmi — clear NMI enable + sprite bits in PPUCTRL
; ---------------------------------------------------------------------------
; Masks $FF (PPUCTRL shadow) to $11 (keep sprite table select + increment),
; clears NMI enable (bit 7) and sprite size (bit 5).
; Referenced from bank16 dispatch table at $168A68.
; ---------------------------------------------------------------------------
disable_nmi:

        lda     ppu_ctrl_shadow         ; load PPUCTRL shadow
        and     #$11                    ; keep bits 4,0 only
        sta     ppu_ctrl_shadow         ; update shadow
        sta     PPUCTRL                 ; write PPUCTRL
        rts                             ; return

; ---------------------------------------------------------------------------
; enable_nmi — set NMI enable bit in PPUCTRL
; ---------------------------------------------------------------------------
enable_nmi:

        lda     ppu_ctrl_shadow         ; load PPUCTRL shadow
        ora     #$80                    ; set bit 7 (NMI enable)
        sta     ppu_ctrl_shadow         ; update shadow
        sta     PPUCTRL                 ; write PPUCTRL
        rts                             ; return

; ---------------------------------------------------------------------------
; rendering_off — blank screen and increment frame-lock counter
; ---------------------------------------------------------------------------
; Sets PPUMASK ($2001) to $00 (all rendering off).
; $EE++ prevents task_yield from resuming entity processing.
; ppu_mask_shadow = PPUMASK shadow.
; ---------------------------------------------------------------------------
rendering_off:

        inc     nmi_skip                ; frame-lock counter++
        lda     #$00                    ; rendering disabled
        sta     ppu_mask_shadow         ; PPUMASK shadow = $00
        sta     PPUMASK                 ; all rendering off
        rts                             ; return

; ---------------------------------------------------------------------------
; rendering_on — restore screen rendering and decrement frame-lock
; ---------------------------------------------------------------------------
; Sets PPUMASK ($2001) to $18 (show background + show sprites).
; $EE-- re-enables entity processing in task_yield.
; ---------------------------------------------------------------------------
rendering_on:

        dec     nmi_skip                ; frame-lock counter--
        lda     #$18                    ; BG + sprites enabled
        sta     ppu_mask_shadow         ; PPUMASK shadow = $18
        sta     PPUMASK                 ; BG + sprites on
        rts                             ; return

; ===========================================================================
; read_controllers — read both joypads with DPCM-glitch mitigation
; ===========================================================================
; Standard NES controller read using the double-read technique: reads each
; controller via both bit 0 and bit 1 of $4016/$4017, then ORs results to
; compensate for DPCM channel interference on bit 0.
;
; After return:
;   joy1_press ($14) = player 1 new presses (edges only, not held)
;   joy1_press_alt ($15) = player 2 new presses
;   joy1_held ($16) = player 1 held (raw state this frame)
;   $17 = player 2 held
;
; Simultaneous Up+Down or Left+Right are cancelled (D-pad bits cleared).
; Called by scheduler on task slot 0 resume (main game task gets input).
; ---------------------------------------------------------------------------

read_controllers:  ldx     #$01         ; strobe controllers
        stx     JOY1                    ; (write 1 then 0 to latch)
        dex                             ; X = 0
        stx     JOY1                    ; (write 1 then 0 to latch)
        ldx     #$08                    ; 8 bits per controller
read_ctrl_bit_loop:  lda     JOY1       ; player 1: bit 0 → $14
        lsr     a                       ; bit 1 → $00 (DPCM-safe)
        rol     joy1_press              ; shift bit into joy1_press
        lsr     a                       ; shift out bit 1
        rol     temp_00                 ; shift bit into DPCM verify byte
        lda     JOY2                    ; player 2: bit 0 → $15
        lsr     a                       ; shift out bit 0
        rol     joy1_press_alt          ; shift bit into joy1_press_alt
        lsr     a                       ; shift out bit 1
        rol     $01                     ; shift bit into DPCM verify byte
        dex                             ; next controller bit
        bne     read_ctrl_bit_loop      ; loop all 8 bits
        lda     temp_00                 ; OR both reads together
        ora     joy1_press              ; to compensate for DPCM
        sta     joy1_press              ; bit-0 corruption
        lda     $01                     ; to compensate for DPCM
        ora     joy1_press_alt          ; bit-0 corruption
        sta     joy1_press_alt          ; store merged player 2 buttons

; --- edge detection: new presses = (current XOR previous) AND current ---
        ldx     #$01                    ; X=1 (P2), then X=0 (P1)
read_ctrl_edge_detect_loop:  lda     joy1_press,x ; load raw buttons this frame
        tay                             ; save raw state in Y
        eor     joy1_held,x             ; bits that changed from last frame
        and     joy1_press,x            ; AND current = newly pressed only
        sta     joy1_press,x            ; joy1_press/$15 = new presses
        sty     joy1_held,x             ; joy1_held/$17 = held (raw) for next frame
        dex                             ; next player (1 then 0)
        bpl     read_ctrl_edge_detect_loop ; loop for player 1

; --- cancel simultaneous opposite directions ---
        ldx     #$03                    ; check $14,$15,$16,$17
read_ctrl_cancel_opposite_loop:  lda     joy1_press,x ; load button state
        and     #$0C                    ; isolate Up+Down bits
        cmp     #$0C                    ; Up+Down both pressed? ($0C)
        beq     read_ctrl_clear_dpad    ; clear D-pad if both pressed
        lda     joy1_press,x            ; load button state again
        and     #$03                    ; isolate Left+Right bits
        cmp     #$03                    ; Left+Right both pressed? ($03)
        bne     read_ctrl_next_direction ; not both pressed, skip clear
read_ctrl_clear_dpad:  lda     joy1_press,x ; load button state
        and     #$F0                    ; keep A/B/Select/Start
        sta     joy1_press,x            ; store cleaned button state
read_ctrl_next_direction:  dex
        bpl     read_ctrl_cancel_opposite_loop ; loop all 4 button registers
        rts                             ; return

; ===========================================================================
; fill_nametable — fill a PPU nametable (or CHR range) with a single byte
; ===========================================================================
; Fills PPU memory starting at address A:$00 with value X.
; If A >= $20 (nametable range): writes 1024 bytes (full nametable), then
; fills 64-byte attribute table at (A+3):$C0 with value Y.
; If A < $20 (pattern table): writes Y × 256 bytes (Y pages).
;
; Input:  A = PPU address high byte ($20/$24 for nametables, $00-$1F for CHR)
;         X = fill byte (tile index for nametable, pattern data for CHR)
;         Y = attribute fill byte (nametable mode) or page count (CHR mode)
; Called from: RESET (clear both nametables), level transitions
; ---------------------------------------------------------------------------

fill_nametable:  sta     temp_00        ; save parameters
        stx     $01                     ; $00=addr_hi, $01=fill, $02=attr
        sty     $02                     ; $02 = attribute/page count
        lda     PPUSTATUS               ; reset PPU latch
        lda     ppu_ctrl_shadow         ; load PPUCTRL shadow
        and     #$FE                    ; (horizontal increment mode)
        sta     PPUCTRL                 ; write to PPUCTRL
        lda     temp_00                 ; PPUADDR = addr_hi : $00
        sta     PPUADDR                 ; set PPUADDR high byte
        ldy     #$00                    ; Y = 0 for PPUADDR low byte
        sty     PPUADDR                 ; set PPUADDR low byte
        ldx     #$04                    ; nametable? 4 pages (1024 bytes)
        cmp     #$20                    ; addr >= $20 means nametable
        bcs     fill_nametable_page_setup ; branch if nametable range
        ldx     $02                     ; CHR? Y pages (from parameter)
fill_nametable_page_setup:  ldy     #$00 ; 256 iterations per page
        lda     $01                     ; A = fill byte
fill_nametable_write_loop:  sta     PPUDATA ; write fill byte
        dey                             ; inner: 256 bytes
        bne     fill_nametable_write_loop ; loop 256 bytes per page
        dex                             ; outer: X pages
        bne     fill_nametable_write_loop ; loop X pages
        ldy     $02                     ; Y = attribute byte
        lda     temp_00                 ; if addr < $20, skip attributes
        cmp     #$20                    ; addr < $20 means pattern table
        bcc     fill_nametable_restore_x ; if addr < $20, skip attributes
        adc     #$02                    ; PPUADDR = (addr_hi+3):$C0
        sta     PPUADDR                 ; ($20→$23C0, $24→$27C0)
        lda     #$C0                    ; (carry set from CMP above)
        sta     PPUADDR                 ; set PPUADDR low byte
        ldx     #$40                    ; 64 attribute bytes
fill_nametable_attr_loop:  sty     PPUDATA ; write attribute byte
        dex                             ; next attribute byte
        bne     fill_nametable_attr_loop ; write attribute byte
fill_nametable_restore_x:  ldx     $01  ; restore X = fill byte
        rts                             ; return

; ===========================================================================
; prepare_oam_buffer — clear unused OAM sprites and draw overlays
; ===========================================================================
; Called each frame before update_entity_sprites. Hides all OAM entries from
; the current write position ($97) to end of buffer by setting Y=$F8
; (off-screen). Then dispatches to overlay sprite routines based on flags:
;   $71 != 0 → load_overlay_sprites: copy sprite data from ROM, start scroll
;   $72 != 0 → scroll_overlay_sprites: slide overlay sprites leftward
;   game_mode == 2 → draw_scroll_sprites: draw camera-tracking sprites
;
; OAM layout:
;   $0200-$022F (sprites 0-11) — reserved for overlays when $72 is active
;   $0230+ (sprites 12+) — entity sprites, starting at oam_ptr
;
; oam_ptr = OAM write index: $04 = with player, $0C = skip player, $30 = skip overlays
; scroll_lock = pause flag (nonzero = skip overlay dispatch)
; $71 = overlay init trigger (set by palette_fade_tick when fade-in completes)
; $72 = overlay scroll active (nonzero = overlays visible, preserve OAM 0-11)
; 22 callers across banks $02, $0B, $0C, $18, $1E, $1F.
; ---------------------------------------------------------------------------

prepare_oam_buffer:  lda     player_state ; state $07 = special_death:
        cmp     #PSTATE_SPECIAL_DEATH   ; force $97=$6C (keep 27 sprites,
        bne     prepare_oam_hide_sprite0 ; branch if not special death
        ldx     #$6C                    ; force $97=$6C (keep 27 sprites,
        stx     oam_ptr                 ; clear rest)
        bne     prepare_oam_start_hide  ; always branches (X nonzero)
prepare_oam_hide_sprite0:  ldx     oam_ptr ; if oam_ptr==$04 (player slot only):
        cpx     #$04                    ; hide sprite 0 (NES sprite 0 hit
        bne     prepare_oam_check_player_slot ; branch if oam_ptr != $04
        lda     #$F8                    ; hide sprite 0 (NES sprite 0 hit
        sta     $0200                   ; hide sprite 0 Y position
prepare_oam_check_player_slot:  lda     scroll_lock ; if paused (scroll_lock): clear from $97
        bne     prepare_oam_start_hide  ; skip overlay check if paused
        lda     $72                     ; if overlay active ($72): start
        beq     prepare_oam_start_hide  ; branch if no overlay active
        ldx     #$30                    ; start clearing at OAM $30
prepare_oam_start_hide:  lda     #$F8   ; Y=$F8 = off-screen (hide sprite)
prepare_oam_hide_loop:  sta     $0200,x ; write $F8 to Y byte of each
        inx                             ; advance OAM index +1
        inx                             ; advance OAM index +2
        inx                             ; advance OAM index +3
        inx                             ; advance OAM index +4
        bne     prepare_oam_hide_loop   ; loop until all 64 sprites done
        lda     scroll_lock             ; if paused: skip overlay dispatch
        bne     prepare_oam_exit        ; skip overlay dispatch if paused
        lda     $71                     ; $71: load overlay sprites from ROM
        bne     load_overlay_sprites    ; init overlay sprites
        lda     $72                     ; $72: scroll overlay sprites
        bne     scroll_overlay_sprites  ; scroll active overlays
        lda     game_mode               ; game_mode==2: draw camera-tracking sprites
        cmp     #$02                    ; game mode 2 = scroll mode
        beq     draw_scroll_sprites     ; $F8==2: draw camera-tracking sprites
prepare_oam_exit:  rts

; ===========================================================================
; clear_entity_table — reset all non-player entity slots
; ===========================================================================
; Clears entity active/type byte (ent_status,x) and palette-anim timer (ent_spawn_id,x)
; for slots 1-31 (skips slot 0 = player). Also clears $71/$72 (overlay
; sprite flags used by prepare_oam_buffer).
; Called during stage transitions, scene loads, and init sequences.
; 14 callers across banks $0B, $0C, $18, $1E.
; ---------------------------------------------------------------------------

clear_entity_table:  ldx     #$1F       ; start at slot 31
clear_entity_loop:  lda     #$00        ; clear entity type (deactivate slot)
        sta     ent_status,x            ; deactivate entity slot
        lda     #$FF                    ; $FF = palette-anim inactive
        sta     ent_spawn_id,x          ; reset spawn ID
        dex                             ; loop slots 31 down to 1
        bne     clear_entity_loop       ; (BNE: skips slot 0 = player)
        lda     #$00                    ; clear overlay sprite flags
        sta     $71                     ; $71 = load overlay trigger
        sta     $72                     ; $72 = overlay scroll active
        rts                             ; return

; --- load_overlay_sprites ---
; Copies 12 OAM entries from overlay_sprite_data ($C6D8) to OAM $0200-$022F.
; Tiles $F1/$F2, palette 2. Clears $71, sets $72 (activates scroll), $97=$30.

load_overlay_sprites:  lda     #$00     ; clear trigger flag (one-shot)
        sta     $71                     ; disable trigger flag
        ldy     #$2C                    ; Y = $2C: 12 entries (0-11)
load_overlay_copy_loop:  lda     overlay_sprite_data,y ; copy 4 OAM bytes per sprite:
        sta     $0200,y                 ; Y position
        lda     overlay_sprite_tile_table,y ; tile index
        sta     $0201,y                 ; store tile index
        lda     overlay_sprite_attr_table,y ; load attribute byte
        sta     $0202,y                 ; attribute
        lda     overlay_sprite_pos_table,y ; X position
        sta     $0203,y                 ; store X position
        dey                             ; next entry (Y -= 4)
        dey                             ; next sprite entry -1
        dey                             ; next entry (Y -= 4)
        dey                             ; next sprite entry -3
        bpl     load_overlay_copy_loop  ; loop while Y >= 0
        sty     $72                     ; Y=$FC (nonzero) → overlay scroll active
        lda     #$30                    ; $97=$30: entity sprites start at $0230
        sta     oam_ptr                 ; (12 overlay sprites reserved)
        rts                             ; return

; --- scroll_overlay_sprites ---
; Slides overlay sprites leftward (X -= 1 per frame). Sprites 0-5 scroll
; every frame; sprites 6-11 scroll every other frame ($95 bit 0 = frame parity).
; This creates a parallax-like spread effect as the sprites fly across screen.

scroll_overlay_sprites:  ldy     #$14   ; sprites 0-5: X byte offsets $03-$17
scroll_overlay_fast_loop:  lda     $0203,y ; X position -= 1
        sec                             ; set carry for subtraction
        sbc     #$01                    ; X position -= 1
        sta     $0203,y                 ; store updated X position
        dey                             ; next sprite (Y -= 4)
        dey                             ; next sprite entry -1
        dey                             ; next sprite (Y -= 4)
        dey                             ; next sprite entry -3
        bpl     scroll_overlay_fast_loop ; loop sprites 5 → 0
        lda     $95                     ; load global frame counter
        and     #$01                    ; ($95 = global frame counter)
        bne     scroll_overlay_set_oam_ptr ; skip slow set on odd frames
        ldy     #$14                    ; sprites 6-11: X byte offsets $1B-$2F
scroll_overlay_slow_loop:  lda     $021B,y ; X position -= 1 (half speed)
        sec                             ; set carry for subtraction
        sbc     #$01                    ; X position -= 1 (half speed)
        sta     $021B,y                 ; store updated X position
        dey                             ; next sprite (Y -= 4)
        dey                             ; (part of Y -= 4)
        dey                             ; next sprite (Y -= 4)
        dey                             ; (part of Y -= 4)
        bpl     scroll_overlay_slow_loop ; loop sprites 11 → 6
scroll_overlay_set_oam_ptr:  lda     #$30 ; $97=$30: entity sprites at $0230
        sta     oam_ptr                 ; set entity OAM start
        rts                             ; return to caller

; --- draw_scroll_sprites ---
; Draws 8 camera-tracking sprites when $F8==2 (screen scroll mode).
; X positions are adjusted by camera scroll offset ($F9:$FC >> 2).
; Alternates between two 8-sprite sets based on frame parity ($95 bit 0):
;   even frames: scroll_sprite_data+$00 (Y offset 0)
;   odd frames:  scroll_sprite_data+$20 (Y offset 32)
; Tile $E4 (solid fill), palette 3. Sprites written to OAM $0200-$021F.

draw_scroll_sprites:  lda     camera_x_lo ; compute camera X offset >> 2:
        sta     temp_00                 ; $00 = ($F9:$FC) >> 2
        lda     camera_screen           ; load camera screen page
        lsr     a                       ; $00 = ($F9:$FC) >> 2
        ror     temp_00                 ; rotate into result
        lsr     a                       ; shift right again
        ror     temp_00                 ; final rotate into result
        lda     $95                     ; Y = ($95 AND 1) << 5
        and     #$01                    ; even frames: Y=0, odd: Y=$20
        asl     a                       ; multiply by 32 (5 shifts)
        asl     a                       ; (part of multiply by 32)
        asl     a                       ; (part of multiply by 32)
        asl     a                       ; (part of multiply by 32)
        asl     a                       ; (part of multiply by 32)
        tay                             ; Y = sprite set offset (0 or $20)
        ldx     #$1C                    ; 8 sprites (OAM $00-$1C, 4 bytes each)
draw_scroll_sprites_loop:  lda     scroll_sprite_data,y ; Y position (from ROM table)
        sta     $0200,x                 ; store Y position in OAM
        lda     scroll_sprite_tile_table,y ; tile index
        sta     $0201,x                 ; store tile index in OAM
        lda     scroll_sprite_attr_table,y ; attribute byte
        sta     $0202,x                 ; store attribute in OAM
        lda     scroll_sprite_pos_table,y ; X position = ROM value - scroll offset
        sec                             ; prepare for subtraction
        sbc     temp_00                 ; X position = ROM value - scroll offset
        sta     $0203,x                 ; store X position in OAM
        iny                             ; advance source (Y += 4)
        iny                             ; (part of Y += 4)
        iny                             ; advance source (Y += 4)
        iny                             ; (part of Y += 4)
        dex                             ; advance dest (X -= 4)
        dex                             ; (part of X -= 4)
        dex                             ; advance dest (X -= 4)
        dex                             ; (part of X -= 4)
        bpl     draw_scroll_sprites_loop ; loop 8 sprites
        lda     #$20                    ; $97=$20: entity sprites at $0220
        sta     oam_ptr                 ; (8 scroll sprites reserved)
        rts                             ; return to caller

; overlay_sprite_data: 12 OAM entries for load_overlay_sprites
; Format: Y, tile, attr, X (4 bytes per sprite)
; Sprites 0-5: tiles $F1, palette 2 — scroll fast (every frame)
; Sprites 6-11: tiles $F2, palette 2 — scroll slow (every other frame)

overlay_sprite_data:  .byte   $58       ; sprites 0-1
overlay_sprite_tile_table:  .byte   $F1
overlay_sprite_attr_table:  .byte   $02 ; sprites 0-1
overlay_sprite_pos_table:  .byte   $28,$E0,$F1,$02,$28,$B8,$F1,$02
        .byte   $70,$20,$F1,$02,$A0,$68,$F1,$02
        .byte   $D0,$D8,$F1,$02,$D0,$90,$F2,$02
        .byte   $10,$40,$F2,$02,$58,$D0,$F2,$02
        .byte   $58,$78,$F2,$02,$80,$28,$F2,$02
        .byte   $D8,$A8,$F2,$02,$D8

; scroll_sprite_data: 16 OAM entries for draw_scroll_sprites (two 8-sprite sets)
; Format: Y, tile, attr, X (4 bytes per sprite), tile $E4, palette 3
; Set 0 (even frames): entries 0-7, set 1 (odd frames): entries 8-15
scroll_sprite_data:  .byte   $90        ; set 0, sprites 0-1
scroll_sprite_tile_table:  .byte   $E4
scroll_sprite_attr_table:  .byte   $03
scroll_sprite_pos_table:  .byte   $18,$28,$E4,$03,$20,$68,$E4,$03
        .byte   $30,$58,$E4,$03,$60,$80,$E4,$03
        .byte   $70,$10,$E4,$03,$98,$58,$E4,$03
        .byte   $C0,$80,$E4,$03,$D0,$18,$E4,$03
        .byte   $10,$A0,$E4,$03,$48,$28,$E4,$03
        .byte   $58,$40,$E4,$03,$90,$98,$E4,$03 ; set 0, sprites 0-1
        .byte   $A0,$78,$E4,$03,$D8,$30,$E4,$03
        .byte   $E0,$A0,$E4,$03,$E8,$00,$00,$00
        .byte   $00

; ===========================================================================
; fade_palette_out / fade_palette_in — blocking palette fade
; ===========================================================================
; fade_palette_out: starts at subtract=$30, step=$F0 (decreasing by $10/pass)
; fade_palette_in:  starts at subtract=$10, step=$10 (increasing by $10/pass)
; Both copy $0620 (target palette) → $0600 (active palette), subtract $0F
; per color channel, clamp to $0F (NES black). Yields 4 frames per step.
; Runs until subtract reaches $50 or wraps negative. $18 = palette-dirty flag.
; ---------------------------------------------------------------------------
fade_palette_out:  lda     #$30         ; start dark (subtract $30)
        ldx     #$F0                    ; step = -$10 (brighten each pass)
        bne     fade_start              ; always taken (X != 0)
fade_palette_in:  lda     #$10          ; start bright (subtract $10)
        tax                             ; step = +$10 (darken each pass)
fade_start:  sta     $0F                ; $0F = current subtract amount
        stx     $0D                     ; $0D = step delta per pass
        ldy     #$04                    ; 4 frames per fade step
        sty     $0E                     ; $0E = frames to yield per step
fade_copy_palette_loop:  ldy     #$1F   ; Y = 31 (all palette entries)
fade_copy_target_loop:  lda     $0620,y ; copy target → active palette
        sta     $0600,y                 ; store to active palette
        dey                             ; loop through all 32 palette entries
        bpl     fade_copy_target_loop   ; loop all 32 entries
        ldy     #$1F                    ; reset Y for subtract pass
fade_subtract_loop:  lda     $0600,y    ; subtract from each color
        sec                             ; prepare for subtraction
        sbc     $0F                     ; subtract fade amount from color
        bpl     fade_subtract_clamp     ; branch if result >= 0
        lda     #$0F                    ; clamp to $0F (NES black)
fade_subtract_clamp:  sta     $0600,y   ; store clamped color back
        dey                             ; next palette entry
        bpl     fade_subtract_loop      ; loop through all 32 entries
        sty     palette_dirty           ; Y=$FF, mark palette dirty
        lda     $0E                     ; frames to wait this step
fade_wait_frame_loop:  pha              ; wait $0E frames between fade steps
        jsr     task_yield              ; (lets NMI upload palette to PPU)
        pla                             ; restore frame counter
        sec                             ; prepare for subtraction
        sbc     #$01                    ; decrement frame counter
        bne     fade_wait_frame_loop    ; loop until all frames elapsed
        lda     $0F                     ; current subtract amount
        clc                             ; prepare for addition
        adc     $0D                     ; add step delta
        sta     $0F                     ; store new subtract amount
        cmp     #$50                    ; done when subtract reaches $50
        beq     fade_exit               ; fade complete, exit
        lda     $0F                     ; reload subtract amount
        bpl     fade_copy_palette_loop  ; continue if not wrapped negative
fade_exit:  rts                         ; return to caller

; ---------------------------------------------------------------------------
; palette_fade_tick — per-frame incremental palette fade
; ---------------------------------------------------------------------------
; Called each frame from the game loop. When $1C (fade-active flag) is set,
; copies $0620 → $0600 and subtracts $1D from BG palette entries ($0600-$060F)
; every 4th frame. $1E = step delta added to $1D each tick.
; When $1D reaches $F0: sets $72=0 (fade-out complete, rendering halted).
; When $1D reaches $50: sets $71++ (fade-in complete, gameplay resumes).
; $1C cleared when done.
; ---------------------------------------------------------------------------

palette_fade_tick:  lda     $1C         ; fade active?
        beq     fade_tick_exit          ; no → skip
        lda     $95                     ; frame counter
        and     #$03                    ; every 4th frame
        bne     fade_tick_exit          ; not 4th frame, skip
        ldy     #$1F                    ; Y = 31 (all palette entries)
fade_tick_copy_loop:  lda     $0620,y   ; copy target → active
        sta     $0600,y                 ; store to active palette
        dey                             ; loop through all 32 palette entries
        bpl     fade_tick_copy_loop     ; loop all 32 entries
        ldy     #$0F                    ; BG palette only ($0600-$060F)
fade_tick_bg_subtract_loop:  lda     $0600,y ; load current color
        sec                             ; prepare for subtraction
        sbc     $1D                     ; subtract current fade amount
        bpl     fade_tick_clamp         ; branch if result >= 0
        lda     #$0F                    ; clamp to $0F
fade_tick_clamp:  sta     $0600,y       ; store clamped color back
        dey                             ; next palette entry
        bpl     fade_tick_bg_subtract_loop ; loop through BG palette entries
        sty     palette_dirty           ; palette dirty
        lda     $1D                     ; advance fade amount
        clc                             ; prepare for addition
        adc     $1E                     ; $1E = step delta
        sta     $1D                     ; store new fade amount
        cmp     #$F0                    ; check for fully black
        beq     fade_tick_fully_black   ; $F0 = fully black
        cmp     #$50                    ; $50 = fully restored
        bne     fade_tick_exit          ; not done, continue fading
        inc     $71                     ; signal fade-in complete
        bne     fade_tick_restore_complete ; always taken (inc from nonzero)
fade_tick_fully_black:  lda     #$00    ; A = 0 for clear
        sta     $72                     ; signal rendering halted
fade_tick_restore_complete:  lda     #$00 ; A = 0 for clear
        sta     $1C                     ; clear fade-active flag
fade_tick_exit:  rts                    ; return to caller

; ---------------------------------------------------------------------------
; indirect_dispatch — jump through word table using A as index
; ---------------------------------------------------------------------------
; A = dispatch index. Reads return address from stack to find the table
; base (word table immediately follows the JSR to this routine).
; Computes table[A*2] and JMPs to that address. Preserves X, Y.
; ---------------------------------------------------------------------------
indirect_dispatch:

        stx     $0E                     ; save X, Y
        sty     $0F                     ; save Y register
        asl     a                       ; A * 2 (word index)
        tay                             ; skip past low byte
        iny                             ; +1 (return addr is 1 before table)
        pla                             ; pop return address low
        sta     temp_0C                 ; store return addr low ($0C)
        pla                             ; pop return address high
        sta     $0D                     ; store return addr high ($0D)
        lda     (temp_0C),y             ; read target addr low
        tax                             ; X = target addr low
        iny                             ; advance to high byte
        lda     (temp_0C),y             ; read target addr high
        sta     $0D                     ; store target addr high
        stx     temp_0C                 ; store target addr low ($0C)
        ldx     $0E                     ; restore X, Y
        ldy     $0F                     ; restore Y
        jmp     (temp_0C)               ; jump to target

; --- shift_register_tick ---
; 32-bit LFSR (linear feedback shift register) on $E4-$E7.
; Feedback: XOR of bit 1 of $E4 and bit 1 of $E5 → carry → ROR through
; all 4 bytes ($E4→$E5→$E6→$E7). Called once per frame from game loop.
; $E4 initialized to $88 in main_game_entry. Used for animation cycling.

shift_register_tick:  ldx     #$00      ; X = byte index (0)
        ldy     #$04                    ; 4 bytes to rotate
        lda     $E4,x                   ; feedback = ($E4 bit 1) XOR ($E5 bit 1)
        and     #$02                    ; isolate bit 1 of $E4
        sta     temp_00                 ; store in temp
        lda     $E5,x                   ; load $E5
        and     #$02                    ; isolate bit 1 of $E5
        eor     temp_00                 ; XOR → nonzero if bits differ
        clc                             ; default carry = 0 (bits match)
        beq     shift_register_rotate_loop ; skip sec if bits match
        sec                             ; carry = 1 (bits differ)
shift_register_rotate_loop:  ror     $E4,x ; rotate carry into high bit
        inx                             ; next byte
        dey                             ; loop through 4 bytes $E4-$E7
        bne     shift_register_rotate_loop ; loop until 4 bytes done
        rts                             ; return to caller

; -----------------------------------------------
; ===========================================================================
; load_stage — load room data from stage bank, set CHR banks and palettes
; ===========================================================================
; Reads room layout, metatile columns, screen connections, then calls
; bank $01's $A000 init routine to set sprite CHR banks and palettes.
;
; STAGE BANK DATA LAYOUT ($A000-$BFFF per stage):
;   $AA40,y:     room config table (1 byte/room):
;                  bits 7-6 = vertical connection ($40=down, $80=up)
;                  bit 5    = horizontal scrolling enabled
;                  bits 4-0 = screen count for this room
;   $AA60,y*2:   room pointer table (2 bytes/room):
;                  byte 0 = CHR/palette param (indexes bank $01 $A200/$A030)
;                  byte 1 = layout index (into $AA82, *20 for offset)
;   $AA80-$AA81: BG CHR bank indices ($E8/$E9)
;   $AA82+:      screen layout data (20 bytes/entry: 16 column IDs + 4 connection)
;   $AB00,y:     enemy screen number table
;   $AC00,y:     enemy X pixel position
;   $AD00,y:     enemy Y pixel position
;   $AE00,y:     enemy global entity ID
;   $AF00+:      metatile column definitions (64 bytes per column ID)
;   $B700+:      metatile CHR tile definitions (4 bytes per metatile: 2x2 CHR tiles)
;   $BF00-$BFFF: tile collision attribute table (256 bytes, 1 per metatile)
;
; CHR/PALETTE PARAM MECHANISM:
;   Each room has a param byte ($AA60 byte 0) that indexes two tables
;   in bank $01:
;     $A200 + param*2:     sprite CHR banks ($EC, $ED) for tiles $80-$FF
;     $A030 + param*8:     sprite palettes SP2-SP3 (8 bytes)
;   Params $00-$11 match stage defaults, params $12+ are alternate configs
;   used within stages (different rooms load different enemy tilesets).
;   This allows each room to have its own set of enemy graphics and palettes.
; ---------------------------------------------------------------------------

load_stage:  lda     stage_id           ; switch to stage's PRG bank
        sta     prg_bank                ; ($A000-$BFFF = stage data)
        jsr     select_PRG_banks        ; switch to stage data bank
        lda     $AA80                   ; load palette indices
        sta     $E8                     ; from stage bank
        lda     $AA81                   ; load BG CHR bank index 2
        sta     $E9                     ; store to CHR bank slot $E9
        ldx     #$07                    ; copy default palette
load_stage_copy_palette_loop:  lda     load_stage_default_palette_table,x ; to sprite palette RAM
        sta     $0610,x                 ; store to sprite palette RAM
        sta     $0630,x                 ; store to target palette mirror
        dex                             ; next palette byte
        bpl     load_stage_copy_palette_loop ; loop all 8 bytes
        lda     #$00                    ; $EA = page 0 (sprite tiles $00-$3F)
        sta     $EA                     ; shared across all stages
        lda     #$01                    ; $EB = page 1 (sprite tiles $40-$7F)
        sta     $EB                     ; shared across all stages

; --- load_room: read room data from $AA60[$2B] ---
; Called at stage start and on room transitions.
; $2B = current room/section index.
load_room:  lda     $2B                 ; X = room index * 2
        asl     a                       ; (2 bytes per room in $AA60)
        tax                             ; X = room index * 2
        lda     $AA60,x                 ; CHR/palette param from room table
        pha                             ; (saved for bank $01 $A000 call below)
        lda     $AA61,x                 ; layout index → offset into $AA82
        asl     a                       ; multiply by 20:
        asl     a                       ; *4 → $00
        sta     temp_00                 ; *16
        asl     a                       ; *16 + *4 = *20
        asl     a                       ; (20 bytes per layout entry:
        adc     temp_00                 ; 16 column IDs + 4 connection)
        tay                             ; Y = layout data offset
        ldx     #$00                    ; X = 0 (column copy index)
load_room_copy_column_ids:  lda     $AA82,y ; copy 16 metatile column IDs
        sta     $0600,x                 ; to $0600-$060F (current screen)
        sta     $0620,x                 ; and $0620-$062F (mirror)
        iny                             ; next byte in layout data
        inx                             ; next column ID
        cpx     #$10                    ; 16 column IDs per screen
        bne     load_room_copy_column_ids ; loop until all 16 copied
        ldx     #$00                    ; reset X for connection parsing
load_room_parse_connections:  lda     $AA82,y ; parse 4 screen connection bytes
        pha                             ; (up/down/left/right exits):
        and     #$80                    ; bit 7 → $0100,x (scroll direction)
        sta     $0100,x                 ; ($80=scroll, $00=warp)
        pla                             ; restore full connection byte
        and     #$7F                    ; bits 0-6 → $0108,x (target screen#)
        sta     $0108,x                 ; store target screen number
        lda     #$00                    ; clear $0104,x and $010C,x
        sta     $0104,x                 ; (unused padding/high bytes)
        sta     $010C,x                 ; clear unused high byte
        iny                             ; next connection byte
        inx                             ; next connection entry
        cpx     #$04                    ; 4 connections (U/D/L/R)
        bne     load_room_parse_connections ; loop all 4 connections
        lda     $0600                   ; first column ID from room layout
        sta     $0610                   ; placeholder for sprite palette slot
        sta     $0630                   ; placeholder for sprite palette mirror
        lda     #$01                    ; switch to bank $01 (CHR/palette tables)
        sta     prg_bank                ; bank $01 has CHR/palette lookup tables
        jsr     select_PRG_banks        ; switch to bank $01
        pla                             ; A = CHR/palette param from $AA60
        jsr     banked_A000             ; → sets $EC/$ED from $A200[param*2]
        jmp     update_CHR_banks        ; → and SP2-SP3 from $A030[param*8]

; default_sprite_palette: sprite palette 0-1 defaults (8 bytes)
; SP0: $0F(black), $0F(black), $2C(sky blue), $11(blue)
; SP1: $0F(black), $0F(black), $30(white), $37(orange)

load_stage_default_palette_table:  .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37

; ---------------------------------------------------------------------------
; ensure_stage_bank — switch $F5 to current stage's PRG bank if needed
; ---------------------------------------------------------------------------
; Looks up stage_to_bank[$22] and switches $F5. No-op if $F5 is already $13
; (bank $13 = fixed/shared). Preserves X and Y.
; ---------------------------------------------------------------------------
ensure_stage_bank:  txa                 ; save X
        pha                             ; on stack
        tya                             ; save Y
        pha                             ; push Y on stack
        lda     prg_bank                ; already on bank $13?
        cmp     #$13                    ; compare with bank $13
        beq     ensure_stage_bank_skip  ; yes → skip
        ldy     stage_id                ; stage index
        lda     ensure_stage_bank_table,y ; look up stage → bank
        sta     prg_bank                ; store looked-up bank
        jsr     select_PRG_banks        ; switch bank
ensure_stage_bank_skip:  pla            ; restore Y from stack
        tay                             ; transfer to Y
        pla                             ; restore X from stack
        tax                             ; transfer to X
        rts                             ; return to caller

; stage_to_bank: maps stage index ($22) → PRG bank ($F5)
; $00=Needle $01=Magnet $02=Gemini $03=Hard $04=Top $05=Snake $06=Spark $07=Shadow
; $08-$0B=Doc Robot (Needle/Gemini/Spark/Shadow stages)
; $0C-$0F=Wily fortress, $10+=special/ending

ensure_stage_bank_table:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0D,$0D,$0F
        .byte   $0D,$11,$12,$13,$10,$1E,$0E

