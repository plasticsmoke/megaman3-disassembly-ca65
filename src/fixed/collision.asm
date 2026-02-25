; -----------------------------------------------
; check_tile_horiz — horizontal-scan tile collision
; -----------------------------------------------
; Checks multiple X positions at a fixed Y offset for collisions.
; Used for floor/ceiling detection (scanning across the entity's
; width at a specific vertical position).
;
; parameters:
;   Y = hitbox config index into $EBE2 offset table
;   X = entity slot
; hitbox offset tables:
;   $EBE2,y = starting offset index (into $EC12)
;   $EC10,y = number of check points - 1
;   $EC11,y = Y pixel offset (signed, relative to entity Y)
;   $EC12,y = X pixel offsets (signed, relative to entity X)
; results:
;   $42-$44 = tile type at each check point
;   tile_at_feet_max = max (highest priority) tile type encountered
;   $10     = OR of all tile types (AND #$10 tests solid)
; -----------------------------------------------

check_tile_horiz:  lda     metatile_gemini_scr1_starts,y ; $40 = starting offset index
        sta     $40                     ; store starting offset
        jsr     tile_check_init         ; clear accumulators, switch to stage bank
        tay                             ; Y = hitbox config index
        lda     metatile_gemini_scr2_count,y ; $02 = number of check points - 1
        sta     $02                     ; store check point count
        lda     ent_y_scr,x             ; $03 = entity Y screen
        sta     $03                     ; store entity Y screen
        lda     metatile_gemini_scr2_offset,y ; $11 = entity_Y + Y_offset
        pha                             ; (compute check Y position)
        clc                             ; add Y offset to entity Y pixel
        adc     ent_y_px,x              ; compute check Y position
        sta     $11                     ; store check Y position
        pla                             ; A = original Y offset (for sign check)
        bmi     metatile_negative_offset ; negative offset?
        bcs     metatile_clear_results  ; positive + carry = past screen bottom
        lda     $11                     ; check if Y wrapped past $F0
        cmp     #$F0                    ; (screen height)
        bcs     metatile_clear_results  ; wrapped past screen → out of bounds
        bcc     metatile_y_screen_check ; in bounds → continue check
metatile_negative_offset:  bcs     metatile_y_screen_check ; neg + carry = no underflow → ok
metatile_clear_results:  lda     #$00   ; Y out of bounds: zero all results
        ldy     $02                     ; Y = check point count
metatile_clear_loop:  sta     $42,y     ; clear $42..$42+count
        dey                             ; next result slot
        bpl     metatile_clear_loop     ; loop until all cleared
        jmp     tile_check_cleanup      ; restore bank and return

metatile_y_screen_check:  lda     $03   ; Y screen != 0? treat as offscreen
        bne     metatile_clear_results  ; (only check screen 0)
        lda     $11                     ; Y >> 2 = 4-pixel rows
        lsr     a                       ; divide Y by 4
        lsr     a                       ; to get 4-pixel row index
        pha                             ; $28 = metatile row (bits 5-3 of Y>>2)
        and     #$38                    ; = column offset in metatile grid
        sta     $28                     ; store metatile row
        pla                             ; $03 bit1 = sub-tile Y (bit 2 of Y>>2)
        lsr     a                       ; selects top/bottom half of metatile
        and     #$02                    ; isolate sub-tile Y bit
        sta     $03                     ; store sub-tile Y select
        lda     #$00                    ; $04 = sign extension for X offset
        sta     $04                     ; clear sign extension
        lda     metatile_gemini_scr2_widths,y ; first X offset (signed)
        bpl     metatile_compute_x_pos  ; positive → skip sign extension
        dec     $04                     ; negative: $04 = $FF
metatile_compute_x_pos:  clc            ; $12/$13 = entity_X + X_offset
        adc     ent_x_px,x              ; $12 = X pixel
        sta     $12                     ; store X pixel position
        lda     ent_x_scr,x             ; entity X screen
        adc     $04                     ; add sign extension
        sta     $13                     ; store X screen
        lda     $12                     ; X >> 4 = tile column
        lsr     a                       ; divide X by 16
        lsr     a                       ; to get tile column index
        lsr     a                       ; (16 pixels per tile)
        lsr     a                       ; A = X / 16
        pha                             ; $03 bit0 = sub-tile X (bit 0 of X>>4)
        and     #$01                    ; selects left/right half of metatile
        ora     $03                     ; (combined with Y sub-tile in bits 0-1)
        sta     $03                     ; store combined sub-tile index
        pla                             ; $28 |= metatile column (X>>5)
        lsr     a                       ; merged with row from Y
        ora     $28                     ; combine column with row
        sta     $28                     ; store metatile grid index
metatile_save_slot:  stx     $04        ; save entity slot
        lda     stage_id                ; switch to stage PRG bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $04                     ; restore entity slot
        ldy     $13                     ; get metatile column pointer for X screen
        jsr     metatile_column_ptr     ; set $20/$21 to column data
metatile_get_chr_ptr:  jsr     metatile_chr_ptr ; get CHR data pointer for column+row ($28)
metatile_read_index:  ldy     $03       ; read metatile index at sub-tile ($03)
        lda     (temp_00),y             ; $03 = {0,1,2,3} for TL/TR/BL/BR
        tay                             ; Y = sub-index for attribute lookup
        lda     $BF00,y                 ; get collision attribute for this tile
        and     #$F0                    ; upper nibble = collision type
        jsr     breakable_block_collision ; override if destroyed breakable block
        jsr     proto_man_wall_override ; Proto Man wall override
        ldy     $02                     ; store result in $42+count
        sta     $42,y                   ; store tile type for this check point
        cmp     tile_at_feet_max        ; update max tile type
        bcc     metatile_accumulate_tiles ; less than max → skip update
        sta     tile_at_feet_max        ; update max tile type
metatile_accumulate_tiles:  ora     $10 ; accumulate all tile types
        sta     $10                     ; store combined tile type flags
        dec     $02                     ; all check points done?
        bmi     metatile_player_damage_check ; all done → check for hazard damage
        inc     $40                     ; next offset index
        ldy     $40                     ; Y = next offset table index
        lda     $12                     ; save old bit4 of X (16px tile boundary)
        pha                             ; save old X position
        and     #$10                    ; isolate bit4 (16px tile boundary)
        sta     $04                     ; save as comparison reference
        pla                             ; $12 += next X offset
        clc                             ; add next X offset from table
        adc     metatile_gemini_scr2_widths,y ; advance X to next check point
        sta     $12                     ; update X position
        and     #$10                    ; did bit4 change? (crossed 16px tile?)
        cmp     $04                     ; compare with previous bit4
        beq     metatile_read_index     ; same tile → re-read collision
        lda     $03                     ; toggle sub-tile X (bit 0)
        eor     #$01                    ; flip left/right within metatile
        sta     $03                     ; store updated sub-tile index
        and     #$01                    ; if now odd → just toggled into right half
        bne     metatile_read_index     ; same metatile, re-read sub-tile
        inc     $28                     ; advance metatile column index
        lda     $28                     ; check if crossed column group boundary
        and     #$07                    ; crossed column group? (8 columns)
        bne     metatile_get_chr_ptr    ; no → same screen, new column
        inc     $13                     ; next X screen
        dec     $28                     ; wrap column index back
        lda     $28                     ; keep row bits, clear column
        and     #$38                    ; preserve metatile row
        sta     $28                     ; store row-only grid index
        jmp     metatile_save_slot      ; re-load metatile data for new screen

metatile_player_damage_check:  cpx     #$00 ; only check damage for player (slot 0)
        bne     metatile_cleanup_return ; not player → skip hazard check
        lda     invincibility_timer     ; skip if invincibility active
        bne     metatile_cleanup_return ; immune → skip hazard check
        lda     hazard_pending          ; skip if damage already pending
        bne     metatile_cleanup_return ; damage pending → skip
        lda     player_state            ; skip if player in damage state ($06)
        cmp     #PSTATE_DAMAGE          ; already in damage state?
        beq     metatile_cleanup_return ; yes → skip
        cmp     #PSTATE_DEATH           ; skip if player in death state ($0E)
        beq     metatile_cleanup_return ; yes → skip
        ldy     #$06                    ; Y = damage state
        lda     tile_at_feet_max        ; $30 = damage tile?
        cmp     #TILE_DAMAGE            ; check for damage tile type
        beq     metatile_set_pending_damage ; damage tile → set hazard pending
        ldy     #$0E                    ; Y = death state
        cmp     #TILE_SPIKES            ; $50 = spike tile?
        bne     metatile_cleanup_return ; not spikes → no hazard
metatile_set_pending_damage:  sty     hazard_pending ; set pending damage/death transition
metatile_cleanup_return:  jmp     tile_check_cleanup ; restore bank and return

; -----------------------------------------------
; check_tile_collision — vertical-scan tile collision
; -----------------------------------------------
; Checks multiple Y positions at a fixed X offset for collisions.
; Used for wall detection (scanning down the entity's height at a
; specific horizontal position).
;
; parameters:
;   Y = hitbox config index into $ECE1 offset table
;   X = entity slot
; hitbox offset tables:
;   $ECE1,y = starting offset index (into $ED09)
;   $ED07,y = number of check points - 1
;   $ED08,y = X pixel offset (signed, relative to entity X)
;   $ED09,y = Y pixel offsets (signed, relative to entity Y)
; results:
;   $42-$44 = tile type at each check point
;   tile_at_feet_max = max (highest priority) tile type encountered
;   $10     = OR of all tile types (AND #$10 tests solid)
; -----------------------------------------------

check_tile_collision:  lda     metatile_gemini_scr3_starts,y ; $40 = starting offset index
        sta     $40                     ; store starting offset
        jsr     tile_check_init         ; clear accumulators, switch to stage bank
        tay                             ; Y = hitbox config index
        lda     metatile_gemini_scr4_count,y ; $02 = number of check points - 1
        sta     $02                     ; store check point count
        lda     #$00                    ; $04 = sign extension for X offset
        sta     $04                     ; $04 = 0 (sign extension)
        lda     metatile_gemini_scr4_offset,y ; X offset (signed)
        bpl     metatile_offset_compute ; positive → skip sign extension
        dec     $04                     ; negative: $04 = $FF
metatile_offset_compute:  clc           ; $12/$13 = entity_X + X_offset
        adc     ent_x_px,x              ; $12 = X pixel
        sta     $12                     ; $13 = X screen
        lda     ent_x_scr,x             ; entity X screen
        adc     $04                     ; add sign extension
        sta     $13                     ; store X screen
        lda     $12                     ; X >> 4 = tile column
        lsr     a                       ; divide X by 16
        lsr     a                       ; (continued)
        lsr     a                       ; (continued)
        lsr     a                       ; A = X / 16
        pha                             ; $03 bit0 = sub-tile X (left/right)
        and     #$01                    ; isolate sub-tile X bit
        sta     $03                     ; store sub-tile X select
        pla                             ; $28 = metatile column (X>>5)
        lsr     a                       ; divide by 2 for column
        sta     $28                     ; store metatile column
        lda     ent_y_scr,x             ; Y screen
        bmi     metatile_clamp_top_bound ; negative Y screen → clamp to 0
        bne     metatile_clamp_bottom_bound ; Y screen > 0 → clamp to $EF
        lda     ent_y_px,x              ; $11 = entity_Y + first Y offset
        clc                             ; prepare for addition
        adc     metatile_gemini_scr4_widths,y ; add first Y offset
        sta     $11                     ; store check Y position
        lda     metatile_gemini_scr4_widths,y ; check offset sign
        bpl     metatile_offset_overflow ; positive offset → check overflow
        bcc     metatile_clamp_top_bound ; negative + no carry = underflow
        bcs     metatile_screen_bounds  ; negative + carry = ok, check bounds
metatile_offset_overflow:  bcs     metatile_clamp_bottom_bound ; positive + carry = overflow
metatile_screen_bounds:  lda     $11    ; within screen ($00-$EF)?
        cmp     #$F0                    ; check screen boundary
        bcc     metatile_row_shift      ; < $F0 → within screen bounds
        bcs     metatile_offset_overflow ; wrapped past $F0 → below screen
metatile_clamp_bottom_bound:  lda     #$EF ; clamp to bottom of screen
        sta     $11                     ; store clamped Y
        bne     metatile_row_shift      ; always branches (nonzero)
metatile_clamp_top_bound:  lda     #$00 ; clamp to top of screen
        sta     $11                     ; store clamped Y
        beq     metatile_row_shift      ; always branches (zero)
metatile_advance_y_offset:  lda     $11 ; $11 += next Y offset
        clc                             ; add next Y offset from table
        adc     metatile_gemini_scr4_widths,y ; add next Y offset from table
        sta     $11                     ; store updated Y position
        cmp     #$F0                    ; crossed screen boundary?
        bcc     metatile_offset_index_adv ; within screen → continue check
        adc     #$10                    ; wrap into next screen
        sta     $11                     ; store wrapped Y pixel
        inc     $04                     ; Y screen++
        beq     metatile_row_shift      ; reached screen 0 → start checking
metatile_offset_index_adv:  iny         ; advance offset index
        sty     $40                     ; save offset index
        ldy     $02                     ; zero this check point's result
        lda     #$00                    ; clear check point result
        sta     $42,y                   ; zero this check point
        dec     $02                     ; more points? continue skipping
        bpl     metatile_advance_y_offset ; loop through offscreen points
        jmp     tile_check_cleanup      ; all offscreen → done

metatile_row_shift:  lda     $11        ; Y >> 2 = 4-pixel rows
        lsr     a                       ; divide Y by 4
        lsr     a                       ; (continued)
        pha                             ; $28 |= metatile row (bits 5-3)
        and     #$38                    ; merged with column from X
        ora     $28                     ; combine row and column
        sta     $28                     ; store metatile grid index
        pla                             ; $03 bit1 = sub-tile Y (top/bottom)
        lsr     a                       ; isolate sub-tile Y
        and     #$02                    ; isolate sub-tile Y bit
        ora     $03                     ; combine sub-tile X and Y
        sta     $03                     ; store combined sub-tile index
        stx     $04                     ; switch to stage PRG bank
        lda     stage_id                ; switch to stage PRG bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $04                     ; restore entity slot
        ldy     $13                     ; get metatile column pointer for X screen
        jsr     metatile_column_ptr     ; set column data pointer
metatile_get_row_chr_ptr:  jsr     metatile_chr_ptr ; get CHR data pointer for column+row ($28)
metatile_read_row_index:  ldy     $03   ; read metatile index at sub-tile ($03)
        lda     (temp_00),y             ; read metatile sub-index
        tay                             ; Y = sub-index for attribute lookup
        lda     $BF00,y                 ; get collision attribute
        and     #$F0                    ; upper nibble = collision type
        jsr     breakable_block_collision ; override if destroyed breakable block
        jsr     proto_man_wall_override ; Proto Man wall override
        ldy     $02                     ; store result in $42+count
        sta     $42,y                   ; store tile type for point
        cmp     tile_at_feet_max        ; update max tile type
        bcc     metatile_accum_row_tiles ; less than max → skip update
        sta     tile_at_feet_max        ; $03 bit0 = sub-tile X (left/right)
metatile_accum_row_tiles:  ora     $10  ; accumulate all tile types
        sta     $10                     ; store combined tile flags
        dec     $02                     ; all check points done?
        bmi     metatile_player_dmg_row ; all done → check hazard damage
        inc     $40                     ; next offset index
        ldy     $40                     ; Y = next offset index
        lda     $11                     ; save old bit4 of Y (16px tile boundary)
        pha                             ; save old Y position
        and     #$10                    ; isolate bit4 (16px tile boundary)
        sta     $04                     ; save as comparison reference
        pla                             ; $11 += next Y offset
        clc                             ; prepare for addition
        adc     metatile_gemini_scr4_widths,y ; check offset sign
        sta     $11                     ; store updated Y position
        and     #$10                    ; did bit4 change? (crossed 16px tile?)
        cmp     $04                     ; same 16px tile?
        beq     metatile_read_row_index ; yes → same tile, re-read collision
        lda     $03                     ; toggle sub-tile Y (bit 1)
        eor     #$02                    ; flip top/bottom within metatile
        sta     $03                     ; store updated sub-tile index
        and     #$02                    ; if now set → toggled into bottom half
        bne     metatile_read_row_index ; same metatile, re-read sub-tile
        lda     $28                     ; advance metatile row ($28 += $08)
        pha                             ; save old metatile row
        clc                             ; advance to next metatile row
        adc     #$08                    ; next metatile row (+8)
        sta     $28                     ; store advanced row
        cmp     #$40                    ; past screen bottom? ($40 = 8 rows)
        pla                             ; restore old row
        bcc     metatile_get_row_chr_ptr ; within screen → get new CHR pointer
        sta     $28                     ; restore row, check last sub-tile
        jmp     metatile_read_row_index ; (clamp — don't scroll to next screen)

metatile_player_dmg_row:  cpx     #$00  ; only check damage for player (slot 0)
        bne     metatile_row_cleanup_return ; not player → skip hazard check
        lda     invincibility_timer     ; skip if invincibility active
        bne     metatile_row_cleanup_return ; immune → skip hazard check
        lda     hazard_pending          ; skip if damage already pending
        bne     metatile_row_cleanup_return ; damage pending → skip
        lda     player_state            ; skip if player in damage state ($06)
        cmp     #PSTATE_DAMAGE          ; already in damage state?
        beq     metatile_row_cleanup_return ; yes → skip
        cmp     #PSTATE_DEATH           ; skip if player in death state ($0E)
        beq     metatile_row_cleanup_return ; already in death state → skip
        lda     tile_at_feet_max        ; $50 = spike tile → instant kill
        cmp     #TILE_SPIKES            ; check for spike tile
        bne     metatile_row_cleanup_return ; not spikes → no hazard
        lda     #PSTATE_DEATH           ; set pending death transition
        sta     hazard_pending          ; store pending death transition
metatile_row_cleanup_return:  jmp     tile_check_cleanup ; restore bank and return

; -----------------------------------------------
; tile_check_init — initialize tile collision check
; -----------------------------------------------
; Clears result accumulators ($10, $41), saves the current PRG bank
; to $2F, and switches to the stage PRG bank ($22) so the tile
; collision code can read metatile/attribute data from $A000-$BFFF.
; Preserves A and X across the bank switch.
; -----------------------------------------------

tile_check_init:  pha                   ; save A (table index)
        txa                             ; save X (entity slot)
        pha                             ; save X (entity slot)
        lda     #$00                    ; clear collision accumulators
        sta     $10                     ; $10 = OR of all tile types
        sta     tile_at_feet_max        ; max tile type = 0
        lda     prg_bank                ; save current PRG bank to $2F
        sta     $2F                     ; save current PRG bank
        lda     stage_id                ; switch to stage PRG bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        pla                             ; restore X and A
        tax                             ; restore X
        pla                             ; restore A
        rts                             ; return (A = table index)

; -----------------------------------------------
; tile_check_cleanup — restore PRG bank after tile collision
; -----------------------------------------------
; Restores the PRG bank saved in $2F by tile_check_init.
; Preserves X.
; -----------------------------------------------

tile_check_cleanup:  txa                ; save X
        pha                             ; save X on stack
        lda     $2F                     ; restore saved PRG bank
        sta     prg_bank                ; write back saved bank
        jsr     select_PRG_banks        ; apply bank switch
        pla                             ; restore X
        tax                             ; restore X
        rts                             ; return

; ===========================================================================
; breakable_block_collision — override tile type for destroyed blocks
; ===========================================================================
; Called during collision detection. If tile type is $70 (breakable block)
; and we're on Gemini Man stages ($22 = $02 or $09), checks the $0110
; destroyed-block bitfield. If the block is destroyed, overrides tile type
; to $00 (passthrough) so the player can walk through it.
;
; A = tile type on entry, X = caller context (preserved)
; Returns: A = tile type ($70 if intact, $00 if destroyed), X preserved
; ---------------------------------------------------------------------------

breakable_block_collision:  sta     $06 ; save tile type
        stx     $05                     ; save X
        cmp     #$70                    ; only handle $70 (breakable block)
        bne     metatile_return_tile_type ; not breakable → return as-is
        lda     stage_id                ; only on Gemini Man stages
        cmp     #STAGE_GEMINI           ; ($02 = Robot Master,
        beq     metatile_compute_array_index ; yes → check destroyed state
        cmp     #STAGE_DOC_GEMINI       ; Doc Robot Gemini stage?
        bne     metatile_return_tile_type ; not Gemini → return as-is
metatile_compute_array_index:  lda     $13 ; compute array index (same as
        and     #$01                    ; breakable_block_override):
        asl     a                       ; Y = ($13 & 1) << 5 | ($28 >> 1)
        asl     a                       ; ($13 = screen page for collision,
        asl     a                       ; $28 = metatile column position)
        asl     a                       ; shift left 5 total
        asl     a                       ; A = ($13 & 1) << 5
        sta     $07                     ; store partial index
        lda     $28                     ; load metatile column
        pha                             ; save $28
        lsr     a                       ; Y = byte index into $0110 array
        ora     $07                     ; merge screen page and column bits
        tay                             ; Y = byte index in destroyed array
        pla                             ; X = bit index within byte:
        asl     a                       ; ($28 << 2) & $04 | $03 (quadrant)
        asl     a                       ; shift column left 2
        and     #$04                    ; isolate column parity bit
        ora     $03                     ; merge with sub-tile quadrant
        tax                             ; X = bit select index
        lda     $0110,y                 ; test destroyed bit
        and     bitmask_table,x         ; mask with bitmask for this block
        beq     metatile_return_tile_type ; not destroyed → keep original type
        lda     #$00                    ; destroyed → override to passthrough
        sta     $06                     ; store $00 as tile type
metatile_return_tile_type:  lda     $06 ; return tile type ($00 if block destroyed)
        ldx     $05                     ; restore X
        rts                             ; return tile type in A

; ---------------------------------------------------------------------------
; clear_destroyed_blocks — reset the $0110 destroyed-block bitfield
; ---------------------------------------------------------------------------
; Called on stage init. Zeros all 64 bytes ($0110-$014F) on Gemini Man
; stages ($22 = $02 or $09), making all breakable blocks solid again.
; ---------------------------------------------------------------------------

clear_destroyed_blocks:  lda     stage_id ; only on Gemini Man stages
        cmp     #STAGE_GEMINI           ; Gemini Man stage?
        beq     metatile_zero_destroyed ; yes → clear bitfield
        cmp     #STAGE_DOC_GEMINI       ; Doc Robot Gemini stage?
        bne     metatile_zero_done      ; no → skip
metatile_zero_destroyed:  lda     #$00  ; zero $0110..$014F (64 bytes)
        ldy     #$3F                    ; = 64 block slots
metatile_zero_loop:  sta     $0110,y   ; clear destroyed-block byte
        dey                             ; decrement slot index
        bpl     metatile_zero_loop      ; loop until all 64 slots cleared
metatile_zero_done:  rts
bitmask_table:  .byte   $80,$40,$20,$10,$08,$04,$02,$01

; ---------------------------------------------------------------------------
; proto_man_wall_override — open breakable walls after Proto Man cutscene
; ---------------------------------------------------------------------------
; When $68 (cutscene-complete flag) is set, checks if current tile position
; matches a Proto Man breakable wall for this stage. If so, returns A=$00
; (passthrough) instead of the original tile type, opening the path.
; Table at $EBC6: per-stage index into wall position data at $EBCE+.
;   $EBC6,y = data offset for stage $22 ($FF = no wall on this stage)
;   $EBCE,x = scroll position ($F9) to match
;   $EBCF,x = number of wall tile entries
;   $EBD0+  = tile column positions to match against $28
; ---------------------------------------------------------------------------
proto_man_wall_override:  sta     $05   ; save original tile type
        stx     $06                     ; save X
        sty     $07                     ; save Y
        lda     proto_man_flag          ; cutscene-complete flag
        beq     metatile_gemini_original ; no cutscene → return original
        ldy     stage_id                ; stage index
        ldx     metatile_gemini_table_1,y ; look up wall data offset
        bmi     metatile_gemini_wrong_stage ; $FF = no wall → clear $68
        lda     metatile_gemini_table_2,x ; expected scroll position
        cmp     camera_screen           ; match current scroll?
        bne     metatile_gemini_wrong_stage ; no → clear $68 (wrong screen)
        lda     metatile_gemini_col_offsets,x ; number of wall tile entries
        sta     $08                     ; store wall entry count
        lda     $28                     ; current tile column
metatile_gemini_wall_match:  cmp     metatile_gemini_wall_cols,x ; match wall column?
        beq     metatile_gemini_passthrough ; yes → return $00 (passthrough)
        inx                             ; next wall entry
        dec     $08                     ; more entries to check?
        bpl     metatile_gemini_wall_match ; loop through all wall entries
        bmi     metatile_gemini_original ; no match → return original tile type
metatile_gemini_passthrough:  lda     #$00 ; A = $00 (air/passthrough)
        beq     metatile_gemini_restore_xy ; always branches
metatile_gemini_original:  lda     $05  ; A = original tile type
metatile_gemini_restore_xy:  ldx     $06 ; restore X, Y
        ldy     $07                     ; restore Y
        rts                             ; return tile type in A

metatile_gemini_wrong_stage:  lda     #$00 ; wrong stage/screen
        sta     proto_man_flag          ; clear cutscene-complete flag
        beq     metatile_gemini_original ; always branches (A=0)
metatile_gemini_table_1:  .byte   $FF,$00,$11,$04,$FF,$FF,$FF,$0E ; ($28 << 2) & $04 | $03 (quadrant)
metatile_gemini_table_2:  .byte   $05
metatile_gemini_col_offsets:  .byte   $01
metatile_gemini_wall_cols:  .byte   $31,$39,$13,$07,$23,$24,$2B,$2C
        .byte   $33,$34,$3B,$3C,$05,$00,$31,$09
        .byte   $00,$00
metatile_gemini_scr1_starts:  .byte   $00,$05,$09,$0E,$12,$16,$1A,$1F
        .byte   $24,$28,$2C,$31,$36,$3B,$40,$45
        .byte   $49,$4E,$53,$57,$5B,$5F,$63,$68
        .byte   $6C,$70,$75,$79,$7D,$81,$85,$8A
        .byte   $8F,$94,$99,$9E,$A3,$A8,$AD,$B2
        .byte   $B7,$BC,$C1,$C6,$CB,$CE
metatile_gemini_scr2_count:  .byte   $02
metatile_gemini_scr2_offset:  .byte   $0C
metatile_gemini_scr2_widths:  .byte   $F9,$07,$07,$01,$F4,$F9,$0E,$02
        .byte   $0A,$F1,$0F,$0F,$01,$00,$F9,$0E
        .byte   $01,$00,$08,$0E,$01,$00,$EA,$0E
        .byte   $00,$00,$00,$00,$00,$02,$16,$F9
        .byte   $07,$07,$01,$08,$F9,$0E,$01,$F8
        .byte   $F9,$0E,$02,$10,$F5,$0B,$0B,$02
        .byte   $F0,$F5,$0B,$0B,$02,$08,$FC,$04
        .byte   $04,$00,$00,$00,$00,$00,$02,$0C
        .byte   $F5,$0B,$0B,$01,$0C,$F5,$16,$02
        .byte   $18,$F1,$0F,$0F,$02,$E8,$F1,$0F
        .byte   $0F,$01,$04,$FD,$06,$01,$FC,$FD
        .byte   $06,$01,$1C,$F9,$0E,$01,$10,$F9
        .byte   $07,$02,$E8,$F9,$07,$07,$01,$06
        .byte   $F9,$0E,$01,$08,$F5,$16,$02,$F8
        .byte   $F5,$0B,$0B,$01,$0C,$F9,$0E,$01
        .byte   $F4,$F9,$0E,$01,$04,$F9,$0E,$01
        .byte   $FC,$F9,$0E,$02,$10,$F1,$0F,$0F
        .byte   $02,$F0,$F1,$0F,$0F,$02,$0D,$F5
        .byte   $0B,$0B,$02,$F3,$F5,$0B,$0B,$02
        .byte   $10,$F1,$0F,$0F,$02,$E6,$F1,$0F
        .byte   $0F,$02,$14,$F1,$0F,$0F,$02,$00
        .byte   $00,$00,$00,$02,$14,$F0,$10,$10
        .byte   $02,$EC,$F0,$10,$10,$02,$18,$F1
        .byte   $0F,$0F,$02,$E8,$F1,$0F,$0F,$02
        .byte   $1C,$F5,$0B,$0B,$02,$DC,$F5,$0B
        .byte   $0B,$00,$04,$00,$00,$08,$00
metatile_gemini_scr3_starts:  .byte   $00,$05,$0A,$0E,$12,$17,$1C,$21
        .byte   $26,$2A,$2E,$33,$38,$3D,$42,$49
        .byte   $50,$57,$5E,$65,$6C,$71,$76,$7B
        .byte   $80,$85,$8A,$8F,$94,$99,$9E,$A2
        .byte   $A6,$AB,$B0,$B5,$BA,$BF
metatile_gemini_scr4_count:  .byte   $02
metatile_gemini_scr4_offset:  .byte   $08
metatile_gemini_scr4_widths:  .byte   $F5,$0B,$0B,$02,$F8,$F5,$0B,$0B
        .byte   $01,$10,$FB,$0C,$01,$F0,$FB,$0C
        .byte   $02,$00,$F5,$0B,$0B,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$08,$F9,$0E
        .byte   $01,$F8,$F9,$0E,$01,$0C,$F5,$0B
        .byte   $0B,$02,$F4,$F5,$0B,$0B,$02,$0C
        .byte   $F5,$0B,$0B,$02,$F4,$F5,$0B,$0B
        .byte   $04,$10,$E9,$10,$07,$10,$07,$04
        .byte   $F0,$E9,$10,$07,$10,$07,$04,$0C
        .byte   $E5,$10,$0B,$10,$0B,$04,$F4,$E5
        .byte   $10,$0B,$10,$0B,$04,$08,$E5,$10
        .byte   $0B,$10,$0B,$01,$F8,$E5,$10,$0B
        .byte   $10,$0B,$02,$10,$F1,$0F,$0F,$02
        .byte   $F0,$F1,$0F,$0F,$02,$14,$F5,$0B
        .byte   $0B,$02,$EC,$F5,$0B,$0B,$02,$0C
        .byte   $F9,$07,$07,$02,$F4,$F9,$07,$07
        .byte   $02,$0C,$F9,$07,$07,$02,$F4,$F9
        .byte   $07,$07,$02,$0C,$F9,$07,$07,$02
        .byte   $F4,$F9,$07,$07,$01,$04,$FD,$06
        .byte   $01,$FC,$FD,$06,$02,$10,$F1,$0F
metatile_gemini_scr4_hi_widths:  .byte   $0F,$02,$F0,$F1,$0F,$0F,$02,$14
        .byte   $EC,$14,$14,$02,$EC,$EC,$14,$14
        .byte   $02,$10,$E8,$18,$18,$02
        beq     metatile_gemini_scr4_hi_widths ; data (hitbox table continuation)
        clc                             ; data (hitbox table continuation)
        clc                             ; data (hitbox table continuation)

; -----------------------------------------------
; snap_x_to_wall_right — push entity left after hitting a wall on its right
; -----------------------------------------------
; After a rightward collision, aligns entity X to the left edge of the
; solid tile. $12 = collision check X; lower 4 bits = how far into the
; 16-pixel tile. Subtracts that offset from entity X position.
; -----------------------------------------------
snap_x_to_wall_right:  lda     $12      ; offset = $12 & $0F
        and     #$0F                    ; (sub-tile penetration depth)
        sta     $12                     ; store sub-tile penetration
        lda     ent_x_px,x              ; X pixel -= offset
        sec                             ; (push entity left)
        sbc     $12                     ; subtract penetration depth
        sta     ent_x_px,x              ; store corrected X pixel
        lda     ent_x_scr,x             ; X screen -= borrow
        sbc     #$00                    ; propagate borrow to screen
        sta     ent_x_scr,x             ; store corrected X screen
        rts                             ; return

; -----------------------------------------------
; snap_x_to_wall_left — push entity right after hitting a wall on its left
; -----------------------------------------------
; After a leftward collision, aligns entity X to the right edge of the
; solid tile + 1. $12 = collision check X; computes (16 - offset) and
; adds to entity X. EOR #$0F + SEC/ADC = add (16 - low_nibble).
; -----------------------------------------------

snap_x_to_wall_left:  lda     $12       ; offset = ($12 & $0F) ^ $0F
        and     #$0F                    ; = 15 - sub-tile position
        eor     #$0F                    ; invert to get complement
        sec                             ; X pixel += (16 - sub_tile_pos)
        adc     ent_x_px,x              ; SEC + ADC = add offset + 1
        sta     ent_x_px,x              ; (push entity right)
        lda     ent_x_scr,x             ; X screen += carry
        adc     #$00                    ; propagate carry to screen
        sta     ent_x_scr,x             ; store corrected X screen
        rts                             ; return

; -----------------------------------------------
; snap_y_to_ceil — push entity down after hitting a ceiling above
; -----------------------------------------------
; After an upward collision, aligns entity Y to the bottom of the
; solid tile + 1. Same math as snap_x_to_wall_left but for Y axis.
; Handles downward screen wrap at Y=$F0.
; -----------------------------------------------

snap_y_to_ceil:  lda     $11            ; offset = ($11 & $0F) ^ $0F
        and     #$0F                    ; isolate sub-tile position
        eor     #$0F                    ; invert to get complement
        sec                             ; Y pixel += (16 - sub_tile_pos)
        adc     ent_y_px,x              ; (push entity down)
        sta     ent_y_px,x              ; store corrected Y pixel
        cmp     #$F0                    ; screen height = $F0
        bcc     metatile_gemini_done    ; within screen? done
        adc     #$0F                    ; wrap to next screen down
        sta     ent_y_px,x              ; store wrapped Y pixel
        inc     ent_y_scr,x             ; Y screen++
metatile_gemini_done:  rts              ; return

; -----------------------------------------------
; snap_y_to_floor — push entity up after landing on a floor below
; -----------------------------------------------
; After a downward collision, aligns entity Y to the top of the
; solid tile. $11 = collision check Y; subtracts low nibble from
; entity Y. Preserves $11 (restored from stack after adjustment).
; Handles upward screen wrap (underflow).
; -----------------------------------------------

snap_y_to_floor:  lda     $11           ; save original $11
        pha                             ; save original $11
        and     #$0F                    ; offset = $11 & $0F
        sta     $11                     ; (sub-tile penetration depth)
        lda     ent_y_px,x              ; Y pixel -= offset
        sec                             ; (push entity up)
        sbc     $11                     ; subtract penetration depth
        sta     ent_y_px,x              ; store corrected Y pixel
        bcs     metatile_restore_result ; no underflow? done
        sbc     #$0F                    ; wrap to previous screen
        sta     ent_y_px,x              ; store wrapped Y pixel
        dec     ent_y_scr,x             ; Y screen--
metatile_restore_result:  pla           ; restore original $11
        sta     $11                     ; restore original $11
        rts                             ; return

; ===========================================================================
; call_bank10_8000 / call_bank10_8003 — bank $10 trampolines
; ===========================================================================
; Save current $8000-$9FFF bank, switch to bank $10, call entry point
; ($8000 or $8003), then restore original bank. Called from player cutscene
; state machine (code_1FE2F4, code_1FE338). Bank $10 contains cutscene/
; scripted sequence routines.
; ---------------------------------------------------------------------------

call_bank10_8000:  lda     mmc3_select  ; save current $8000 bank
        pha                             ; push current $8000 bank
        lda     #$10                    ; switch $8000-$9FFF to bank $10
        sta     mmc3_select             ; set $8000 bank to $10
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_8000             ; call bank $10 entry point 0
        pla                             ; restore original $8000 bank
        sta     mmc3_select             ; restore $8000 bank
        jmp     select_PRG_banks        ; apply bank switch and return

call_bank10_8003:  lda     mmc3_select  ; save current $8000 bank
        pha                             ; push current $8000 bank
        lda     #$10                    ; switch $8000-$9FFF to bank $10
        sta     mmc3_select             ; set $8000 bank to $10
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_8003             ; call bank $10 entry point 1
        pla                             ; restore original $8000 bank
        sta     mmc3_select             ; restore $8000 bank
        jmp     select_PRG_banks        ; apply bank switch and return

; ===========================================================================
; queue_metatile_clear — queue a 2×2 blank metatile write to PPU
; ===========================================================================
; Converts entity X's screen position (ent_x_px,x / ent_y_px,x) to a PPU nametable
; address and fills the NMI update buffer ($0780+) with two 2-tile rows of
; blank tiles ($00). NMI's drain_ppu_buffer drains this buffer during VBlank.
;
; Buffer format: [addr_hi][addr_lo][count][tile × (count+1)]...[FF=end]
; This builds two entries (top row + bottom row of metatile) + terminator.
;
; Input:  X = entity slot index
; Output: C=0 on success (buffer queued), C=1 if PPU update already pending
; Clobbers: $0780-$078A, $19
; Called from: bank1C_1D (tile destruction / block breaking)
; ---------------------------------------------------------------------------
queue_metatile_clear:

        sec                             ; preset carry = fail
        lda     nt_column_dirty         ; if nametable column or row
        ora     nametable_dirty         ; update already pending,
        bne     metatile_final_return   ; return C=1 (busy)
        lda     #$08                    ; seed high byte = $08
        sta     $0780                   ; (becomes $20-$23 after shifts)
        lda     ent_y_px,x              ; entity Y, upper nibble = metatile row
        and     #$F0                    ; ASL×2 with ROL into $0780:
        asl     a                       ; row * 64 → PPU row offset
        rol     $0780                   ; (each metatile = 2 tile rows × 32)
        asl     a                       ; row * 2 (continued)
        rol     $0780                   ; shift into high byte
        sta     $0781                   ; low byte = row component
        lda     ent_x_px,x              ; entity X, upper nibble
        and     #$F0                    ; LSR×3 = metatile column × 2
        lsr     a                       ; (each metatile = 2 tiles wide)
        lsr     a                       ; divide by 2 (continued)
        lsr     a                       ; divide by 2 (continued)
        ora     $0781                   ; merge column into low byte
        sta     $0781                   ; combine row + column
        ora     #$20                    ; addr + 32 = next tile row
        sta     $0786                   ; (second entry low byte)
        lda     #$01                    ; count = 1 → write 2 tiles per row
        sta     $0782                   ; (push entity left)
        sta     $0787                   ; count = 1 for second row too
        lda     $0780                   ; copy high byte for second entry
        sta     $0785                   ; X screen -= borrow
        lda     #$00                    ; tile data = $00 (blank) for all 4
        sta     $0783                   ; blank tile for row 0 left
        sta     $0784                   ; blank tile for row 0 right
        sta     $0788                   ; blank tile for row 1 left
        sta     $0789                   ; blank tile for row 1 right
        lda     #$FF                    ; terminator
        sta     $078A                   ; end-of-buffer marker
        sta     nametable_dirty         ; flag NMI to process buffer
        clc                             ; success
metatile_final_return:  rts             ; return

; ===========================================================================
; queue_metatile_update — build PPU update buffer for a 4×4 tile metatile
; ===========================================================================
; Converts metatile position ($28) to PPU nametable addresses and builds
; a 5-entry NMI update buffer at $0780: four 4-tile rows + one attribute
; byte. Tile data is sourced from $06C0-$06CF (filled by metatile_to_chr_tiles).
;
; $28 = metatile index (low 3 bits = column, upper bits = row)
; $10 = nametable select (bit 2: 0=$2000, 4=$2400)
; proto_man_flag = cutscene-complete flag (selects alternate tile lookup path)
; Y  = metatile ID (when $68=0, passed to metatile_column_ptr_by_id)
;
; Buffer layout: 4 row entries (7 bytes each) + 1 attribute entry + $FF end
;   Entry: [addr_hi][addr_lo][count=3][tile0][tile1][tile2][tile3]
;   Attr:  [attr_addr_hi][attr_addr_lo][count=0][attr_byte][$FF]
;
; Edge case: if metatile row >= $38 (bottom of nametable), the attribute
; entry overwrites the 3rd row entry to avoid writing past nametable bounds.
;
; Called from: bank09 (enemy spawning), bank0C (level loading), bank18
; ---------------------------------------------------------------------------
queue_metatile_update:

        lda     $28                     ; save metatile index
        pha                             ; push for later row extraction
        and     #$07                    ; column bits × 4 = PPU column offset
        asl     a                       ; (4 tiles wide per metatile)
        asl     a                       ; column × 4 tiles
        sta     $0781                   ; PPU addr low byte (column part)
        lda     #$02                    ; seed high byte = $02
        sta     $0780                   ; (becomes $20+ after shifts)
        pla                             ; row bits (upper 5 bits of $28)
        and     #$F8                    ; ASL×4 with ROL = row × 128
        asl     a                       ; (each metatile row = 4 tile rows
        rol     $0780                   ; × 32 bytes = 128)
        asl     a                       ; shift row bits left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row bits left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row bits left (continued)
        rol     $0780                   ; rotate into high byte
        ora     $0781                   ; merge row and column
        sta     $0781                   ; row 0 addr low byte
        clc                             ; prepare for addition
        adc     #$20                    ; row 1 addr low = row 0 + 32
        sta     $0788                   ; row 1 addr low byte
        adc     #$20                    ; row 2 addr low = row 1 + 32
        sta     $078F                   ; row 2 addr low byte
        adc     #$20                    ; row 3 addr low = row 2 + 32
        sta     $0796                   ; row 3 addr low byte
        lda     $28                     ; attribute addr low = $28 OR $C0
        ora     #$C0                    ; (attribute table offset)
        sta     $079D                   ; attribute table addr low byte
        lda     $0780                   ; merge nametable select bit
        ora     $10                     ; into all row high bytes
        sta     $0780                   ; (rows 0-3 share same high byte)
        sta     $0787                   ; row 1 high byte
        sta     $078E                   ; row 2 high byte
        sta     $0795                   ; row 3 high byte
        ora     #$03                    ; attribute high byte = $23 or $27
        sta     $079C                   ; attribute high = $23 or $27
        lda     #$03                    ; count = 3 → 4 tiles per row
        sta     $0782                   ; row 0 tile count
        sta     $0789                   ; row 1 tile count
        sta     $0790                   ; row 2 tile count
        sta     $0797                   ; row 3 tile count
        lda     #$00                    ; attribute entry count = 0 (1 byte)
        sta     $079E                   ; attribute entry count byte
        lda     proto_man_flag          ; cutscene-complete flag?
        beq     metatile_oam_id_load    ; no → normal metatile lookup
        lda     #$00                    ; clear high byte
        sta     $01                     ; clear high byte
        lda     $11                     ; load scroll position
        jsr     calc_chr_offset         ; calculate CHR tile offset
        jsr     metatile_to_chr_tiles_continue ; convert metatile to CHR tiles
        jmp     metatile_oam_copy_loop  ; skip normal lookup path

metatile_oam_id_load:  tya              ; Y = metatile ID
        jsr     metatile_column_ptr_by_id ; look up metatile definition
        jsr     metatile_to_chr_tiles   ; convert to CHR tile indices
metatile_oam_copy_loop:  ldx     #$03   ; copy 4×4 tile data from $06C0-$06CF
metatile_oam_row_copy:  lda     $06C0,x ; row 0: $06C0-$06C3 → $0783-$0786
        sta     $0783,x                 ; row 0 tiles → PPU entry 0
        lda     $06C4,x                 ; row 1: $06C4-$06C7 → $078A-$078D
        sta     $078A,x                 ; row 1 tiles
        lda     $06C8,x                 ; row 2: $06C8-$06CB → $0791-$0794
        sta     $0791,x                 ; row 2 tiles
        lda     $06CC,x                 ; row 3: $06CC-$06CF → $0798-$079B
        sta     $0798,x                 ; row 3 tiles
        dex                             ; next tile index
        bpl     metatile_oam_row_copy   ; loop all 4 tiles
        lda     $10                     ; attribute byte = nametable select
        sta     $079F                   ; store attribute byte
        stx     $07A0                   ; terminator ($FF from DEX past 0)
        lda     $28                     ; if metatile row >= $38 (bottom edge),
        and     #$3F                    ; row 3+4 would overflow nametable.
        cmp     #$38                    ; Move attribute entry up to replace
        bcc     metatile_oam_dirty_flag ; row 3 entry to avoid overflow.
        ldx     #$04                    ; copy attribute entry ($079C-$07A0)
metatile_oam_attr_copy:  lda     $079C,x ; source: attribute entry at $079C
        sta     $078E,x                 ; dest: overwrite row 2 entry at $078E
        dex                             ; next byte
        bpl     metatile_oam_attr_copy  ; loop all 5 bytes
metatile_oam_dirty_flag:  stx     nametable_dirty ; flag NMI to process buffer
        rts                             ; return

; ===========================================================================
; write_attribute_table — queue attribute table write to PPU
; ===========================================================================
; Called after fill_nametable_progressive finishes all 64 metatile columns.
; Copies the 64-byte attribute buffer ($0640-$067F) to the PPU write queue,
; targeting PPU $23C0 (attribute table of nametable 0 or 1 based on $10).
; Resets $70 to 0 so the nametable fill can restart if needed.
; ---------------------------------------------------------------------------

write_attribute_table:  lda     #$00    ; reset progress counter
        sta     $70                     ; progress counter = 0
        lda     #$23                    ; PPU address high byte: $23 or $27
        ora     $10                     ; ($10 bit 2 = nametable select)
        sta     $0780                   ; PPU addr high byte
        lda     #$C0                    ; PPU address low byte: $C0
        sta     $0781                   ; PPU addr low = $C0 (attr table start)
        ldy     #$3F                    ; count = 64 bytes ($3F+1)
        sty     $0782                   ; byte count = 64
metatile_attr_buffer_copy:  lda     $0640,y ; copy attribute buffer to PPU queue
        sta     $0783,y                 ; $0783+ = attribute data for NMI
        dey                             ; previous byte
        bpl     metatile_attr_buffer_copy ; loop all 64 bytes
        sty     $07C3                   ; $FF terminator after data
        sty     nametable_dirty         ; signal NMI to process queue
        rts                             ; return

; ===========================================================================
; fill_nametable_progressive — fill nametable 4 metatile columns at a time
; ===========================================================================
; Called once per frame during level loading. Progress counter $70 tracks
; the current metatile column (0-63). Each call processes 4 columns,
; converting metatiles to CHR tiles and queuing them for PPU writes.
;
; Each metatile column = 2 CHR tiles wide. 4 columns = 8 tiles = one row
; group. The PPU write queue gets 4 entries (one per tile row), each
; containing 16 tiles ($0F + 1). After all 64 columns ($70=$40), branches
; to write_attribute_table.
;
; PPU address layout: $70 maps to nametable position:
;   - Low 3 bits × 4 = tile column offset within row
;   - High 5 bits × 16 = tile row offset (each row = 32 bytes)
;
; $10 = nametable select (bit 2: 0=NT0, 4=NT1).
; $28 = working column index within the 4-column group.
; $0640 = attribute buffer (one byte per metatile column).
; ---------------------------------------------------------------------------

fill_nametable_progressive:  lda     $70 ; if $70 == $40 (64), all done
        cmp     #$40                    ; → write attribute table and reset
        beq     write_attribute_table   ; all 64 columns done → attributes
        sta     $28                     ; $28 = working column index

; --- Compute PPU base address from $70 ---
; Column ($70 & $07) × 4 → low byte offset.
; Row ($70 & $F8) << 4 → high byte bits.
        pha                             ; save $70 for row extraction
        and     #$07                    ; column within row group × 4
        asl     a                       ; column × 2
        asl     a                       ; column × 4
        sta     $0781                   ; → PPU addr low byte (partial)
        lda     #$02                    ; seed high byte = $02
        sta     $0780                   ; (nametable $2000 base >> 8)
        pla                             ; restore $70
        and     #$F8                    ; row index × 128 via shift+rotate:
        asl     a                       ; ($70 & $F8) << 4 into high/low
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row left (continued)
        rol     $0780                   ; rotate into high byte
        asl     a                       ; shift row left (continued)
        rol     $0780                   ; rotate into high byte
        ora     $0781                   ; merge low byte parts
        sta     $0781                   ; store merged low byte

; --- Set up 4 PPU write entries (4 tile rows) ---
; Each entry is 19 bytes apart ($13). Low bytes advance by $20 (one row).
        clc                             ; prepare for addition
        adc     #$20                    ; row 1 low byte
        sta     $0794                   ; row 1 PPU addr low byte
        adc     #$20                    ; row 2 low byte
        sta     $07A7                   ; row 2 PPU addr low byte
        adc     #$20                    ; row 3 low byte
        sta     $07BA                   ; row 3 PPU addr low byte
        lda     $0780                   ; high byte + nametable select ($10)
        ora     $10                     ; same for all 4 entries
        sta     $0780                   ; row 0 high byte
        sta     $0793                   ; row 1 high byte
        sta     $07A6                   ; row 2 high byte
        sta     $07B9                   ; row 3 high byte
        lda     #$0F                    ; tile count = 16 ($0F+1) per entry
        sta     $0782                   ; count = 16 tiles per entry ($0F+1)
        sta     $0795                   ; row 1 tile count
        sta     $07A8                   ; row 2 tile count
        sta     $07BB                   ; row 3 tile count

; --- Process 4 metatile columns ---
metatile_column_convert:  jsr     metatile_to_chr_tiles ; convert metatile → 16 tiles in $06C0
        lda     $28                     ; which column within group? (0-3)
        and     #$03                    ; determines position within entries
        tax                             ; X = column within group
        ldy     ppu_column_offsets,x    ; Y = starting offset {$03,$07,$0B,$0F}
        ldx     #$00                    ; X = source offset into $06C0
metatile_column_tile_copy:  lda     $06C0,x ; copy 4 tiles from $06C0
        sta     $0780,y                 ; to current position in PPU entry
        iny                             ; advance dest offset
        inx                             ; advance source offset
        txa                             ; check if 4 tiles copied
        and     #$03                    ; 4 tiles per metatile row?
        bne     metatile_column_tile_copy ; more tiles → continue copy
        tya                             ; advance Y past remaining entry bytes
        clc                             ; prepare for addition
        adc     #$0F                    ; skip to next entry start
        pha                             ; save dest offset
        ldy     $28                     ; load current column index
        lda     $10                     ; attribute byte from metatile lookup
        sta     $0640,y                 ; store in attribute buffer
        pla                             ; restore dest offset
        tay                             ; restore Y dest offset
        cpy     #$4C                    ; all 4 tile rows filled?
        bcc     metatile_column_tile_copy ; no → copy next row
        inc     $70                     ; advance progress counter
        inc     $28                     ; advance working column
        lda     $28                     ; repeat for 4 columns total
        and     #$03                    ; check if 4 columns done
        bne     metatile_column_convert ; more columns → continue
        lda     #$FF                    ; $FF terminator after 4th entry
        sta     $07CC                   ; terminator after PPU entries
        ldy     $28                     ; check if past bottom of nametable
        cpy     #$39                    ; column >= $39?
        bcc     metatile_column_signal_nmi ; no → signal NMI
        sta     $07A6                   ; $FF = terminate 3rd entry early
metatile_column_signal_nmi:  sta     nametable_dirty ; signal NMI to process PPU queue
        rts                             ; return

; PPU entry column offsets — maps column-within-group (0-3) to starting
; byte offset within each PPU write entry's data area.
ppu_column_offsets:  .byte   $03
        .byte   $07
        .byte   $0B
        .byte   $0F

