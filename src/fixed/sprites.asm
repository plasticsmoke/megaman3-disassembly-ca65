; ===========================================================================
; SPRITE ANIMATION ENGINE — $F034-$F57F
; ===========================================================================
; Per-frame entity rendering system:
;   update_entity_sprites  — main loop, iterates 32 entity slots
;   process_entity_display — screen culling, death/damage handling
;   setup_sprite_render    — bank selection, animation state machine
;   write_entity_oam       — assembles OAM entries from sprite definitions
;   draw_energy_bars       — draws up to 3 HUD energy meters
;
; Entity slots 0-31 are processed in alternating order each frame
; (forward on even frames, reverse on odd) for OAM priority fairness.
;
; Animation sequence format at ($00):
;   byte 0 = total frames in sequence
;   byte 1 = ticks per frame (duration)
;   byte 2+ = sprite definition IDs per frame (0 = deactivate entity)
;
; Sprite definition format:
;   byte 0 = sprite count (bit 7: use CHR bank $14 instead of $19)
;   byte 1 = position offset table index (for Y/X offsets)
;   byte 2+ = pairs of (CHR tile, OAM attribute) per sprite
; ===========================================================================

; --- update_entity_sprites ---
; main per-frame entity rendering loop
; iterates all 32 entity slots (0-31), alternating direction each frame
; on even frames: draws HUD first, then slots 0→31
; on odd frames: slots 31→0, then draws HUD
; $95 = global frame counter, $96 = current slot index
; oam_ptr = OAM buffer write index (used by sprite assembly routines)
update_entity_sprites:  inc     $95     ; advance global frame counter
        inc     $4F                     ; advance secondary counter
        lda     $95                     ; load frame counter
        lsr     a                       ; even frame? forward iteration
        bcs     sprite_backward_loop    ; odd frame? backward iteration
        jsr     draw_energy_bars        ; draw HUD bars first on even frames
        ldx     #$00                    ; X = first entity slot
        stx     $96                     ; start at slot 0
sprite_entity_active_check:  lda     ent_status,x ; bit 7 = entity active
        bpl     sprite_entity_count_inc ; skip inactive
        jsr     process_entity_display  ; render this entity
sprite_entity_count_inc:  inc     $96     ; advance slot index
        ldx     $96                     ; next slot
        cpx     #$20                    ; 32 slots total
        bne     sprite_entity_active_check ; loop until all 32 done
        rts                             ; return after forward pass

sprite_backward_loop:  ldx     #$1F     ; start at slot 31
        stx     $96                     ; start at slot 31
sprite_entity_check:  lda     ent_status,x ; bit 7 = entity active
        bpl     sprite_count_dec        ; skip inactive
        jsr     process_entity_display  ; render this entity
sprite_count_dec:  dec     $96             ; decrement slot index
        ldx     $96                     ; previous slot
        bpl     sprite_entity_check     ; loop until slot 0 done
        jsr     draw_energy_bars        ; draw HUD bars last on odd frames
        rts                             ; return after backward pass

; --- process_entity_display ---
; per-entity display handler: screen culling, death/damage, OAM assembly
; X = entity slot index (0-31)
; $FC/$F9 = camera scroll X pixel/screen
; $12/$13 = screen Y/X position for OAM drawing

process_entity_display:  lda     ent_flags,x ; load entity flags
        and     #$10                    ; bit 4 = uses world coordinates
        beq     sprite_screen_fixed_x   ; 0 = screen-fixed position
        lda     ent_x_px,x              ; load entity X pixel position
        sec                             ; screen X = entity X - camera X
        sbc     camera_x_lo             ; subtract camera X low byte
        sta     $13                     ; store screen X position
        lda     ent_x_scr,x             ; entity X screen - camera screen
        sbc     camera_screen           ; if different screen, offscreen
        bne     process_entity_offscreen ; X screens differ → offscreen
        lda     ent_y_scr,x             ; Y screen = 0? → on same screen
        beq     sprite_screen_fixed_y   ; on-screen → proceed to render
        cpx     #$00                    ; player? keep visible
        beq     sprite_deactivate_offscreen ; slot 0 = player, special handling
        lda     ent_y_scr,x             ; Y screen negative? keep visible
        bmi     sprite_deactivate_offscreen ; Y screen < 0? keep visible
        bpl     process_entity_deactivate ; Y screen > 0? offscreen, deactivate
process_entity_offscreen:  lda     ent_flags,x ; check wide offscreen margin flag
        and     #$08                    ; (keep alive within 72px of edge)
        beq     process_entity_deactivate ; no margin flag → deactivate
        lda     $13                     ; take absolute value of screen X
        bcs     process_entity_margin   ; carry set = positive
        eor     #$FF                    ; negate if negative
        adc     #$01                    ; complete two's complement
process_entity_margin:  cmp     #$48    ; within 72px margin?
        bcc     sprite_deactivate_offscreen ; within margin → keep alive, skip draw
process_entity_deactivate:  lda     #$00 ; deactivate entity
        sta     ent_status,x            ; clear entity status
        lda     #$FF                    ; mark as despawned
        sta     ent_spawn_id,x          ; prevent respawn
        rts                             ; return after deactivation

sprite_deactivate_offscreen:  lda     ent_flags,x ; load entity flags
        and     #$7F                    ; clear bit 7 (drawn flag)
        sta     ent_flags,x             ; update flags
        cpx     #$00                    ; not player? skip to sprite draw
        bne     sprite_landed_setup     ; not player → skip death check
; --- DEBUG (shipped in retail) — P2 Right = pit death immunity ---
        lda     $17                     ; P2 held buttons
        and     #$01                    ; bit 0 = Right held?
        bne     sprite_landed_check     ; yes → skip death trigger
        lda     ent_y_scr               ; player Y screen negative? draw
        bmi     sprite_landed_setup     ; Y screen < 0 → above screen, draw
        lda     #$0E                    ; set player state = $0E (death)
        cmp     player_state            ; if already dead, skip
        beq     sprite_display_return   ; already dead? skip
        sta     player_state            ; store death state
        lda     #SNDCMD_STOP            ; stop current music
        jsr     submit_sound_ID         ; queue stop command
        lda     #SFX_DEATH              ; play death music
        jsr     submit_sound_ID         ; queue death sound
sprite_display_return:  rts             ; return after triggering death

sprite_landed_check:  lda     ent_y_scr ; Y screen = 1? (landed from above)
        cmp     #$01                    ; Y screen = 1? (one screen below)
        bne     sprite_landed_setup     ; no → skip landing override
        lda     #PSTATE_AIRBORNE        ; set player state = $01 (airborne)
        sta     player_state            ; player state = airborne
        lda     #$10                    ; set Y subpixel speed = $10
        sta     ent_yvel_sub            ; set Y sub velocity
        lda     #$0D                    ; Y vel = $0D (fast fall)
        sta     ent_yvel                ; set Y velocity = fast fall
sprite_landed_setup:  jmp     setup_sprite_render ; proceed to sprite rendering

sprite_screen_fixed_x:  lda     ent_x_px,x ; for screen-fixed entities,
        sta     $13                     ; X pos is already screen-relative
sprite_screen_fixed_y:  lda     ent_y_px,x ; Y pixel → draw Y position
        sta     $12                     ; store screen Y position
        lda     ent_flags,x             ; load entity flags
        ora     #$80                    ; set bit 7 (mark as drawn)
        sta     ent_flags,x             ; store updated flags
        and     #$04                    ; bit 2 = invisible (skip OAM)
        beq     setup_sprite_render     ; visible → render sprite
sprite_render_ret:  rts                 ; invisible → skip rendering

; --- setup_sprite_render ---
; prepares sprite bank selection and animation state for OAM assembly
; X = entity slot, $10 = h-flip flag, $11 = flip offset
; $00/$01 = pointer to animation sequence data

setup_sprite_render:  ldy     #$00      ; Y = 0 (no flip)
        lda     ent_flags,x             ; bit 6 = horizontal flip
        and     #ENT_FLAG_HFLIP         ; isolate H-flip bit
        sta     $10                     ; $10 = flip flag for OAM attr
        beq     sprite_flip_offset_setup ; not flipped → Y stays 0
        iny                             ; Y=1 if flipped
sprite_flip_offset_setup:  sty     $11  ; $11 = flip table offset
        cpx     #$10                    ; slot < $10? enemy/boss slot
        bcc     sprite_oam_bank_select  ; enemy slot → normal bank select
        lda     boss_active             ; boss active? override sprite bank
        beq     sprite_oam_bank_select  ; 0 = no override
        ldy     #$15                    ; weapon sprites in PRG bank $15
        cpy     mmc3_select             ; already selected?
        beq     sprite_oam_id_check     ; already bank $15 → skip switch
        bne     sprite_bank_select_write ; (always branches)
sprite_oam_bank_select:  ldy     #$1A   ; OAM ID bit 7 selects bank:
        lda     ent_anim_id,x           ; $00-$7F → bank $1A
        bpl     sprite_bank_select_check ; bit 7 clear → bank $1A
        iny                             ; Y=$1B → bank $1B for bit 7 set
sprite_bank_select_check:  cpy     mmc3_select ; bank select already correct?
        beq     sprite_oam_id_check     ; already correct → skip switch
sprite_bank_select_write:  sty     mmc3_select ; set $8000 bank
        stx     temp_00                 ; save entity slot index
        jsr     select_PRG_banks        ; apply bank switch
        ldx     temp_00                 ; restore entity slot index
sprite_oam_id_check:  lda     ent_anim_id,x ; OAM ID = 0? no sprite
        beq     sprite_render_ret       ; return
        and     #$7F                    ; strip bank select bit
        tay                             ; Y = OAM ID (7-bit index)
        lda     banked_8000,y           ; low byte of anim sequence ptr
        sta     temp_00                 ; store ptr low
        lda     $8080,y                 ; high byte of anim sequence ptr
        sta     $01                     ; store ptr high

; --- animation tick / frame advancement ---
; Animation sequence format at ($00):
;   byte 0 = total frames in sequence
;   byte 1 = ticks per frame (duration)
;   byte 2+ = sprite definition IDs per frame (0 = deactivate entity)
;
; --- DEBUG (shipped in retail) — P2 Up = slow-mo, P2 A = freeze ---
; Holding Up on controller 2 reduces animation speed to 1/8th.
; Holding A on controller 2 freezes all animation entirely.
; If both are held, A (freeze) takes priority over Up (slow-mo).
        lda     $17                     ; P2 held buttons
        and     #$08                    ; bit 3 = Up held? (slow-mo)
        beq     sprite_anim_freeze_check ; no → check global freeze
        lda     $95                     ; slow-mo: tick every 8th frame
        and     #$07                    ; frame counter mod 8
        bne     sprite_drawn_flag_check ; not 8th frame → skip tick
        lda     $17                     ; also check P2 held buttons
        and     #$80                    ; bit 7 = A held? (full freeze)
        bne     sprite_drawn_flag_check ; yes → skip tick entirely
sprite_anim_freeze_check:  lda     $58  ; $58 = global animation freeze
        bne     sprite_drawn_flag_check ; nonzero = skip ticking
        lda     ent_anim_frame,x        ; ent_anim_frame = frame tick counter
        and     #$7F                    ; (bit 7 = damage flash flag)
        inc     ent_anim_frame,x        ; increment tick
        ldy     #$01                    ; Y=1 → offset to duration byte
        cmp     (temp_00),y             ; compare tick vs duration at seq[1]
        bne     sprite_drawn_flag_check ; not reached? keep waiting
        lda     ent_anim_frame,x        ; tick reached duration:
        and     #$80                    ; reset tick to 0, preserve bit 7
        sta     ent_anim_frame,x        ; (damage flash flag)
        lda     ent_anim_state,x        ; ent_anim_state = current frame index
        and     #$7F                    ; (bit 7 preserved)
        inc     ent_anim_state,x        ; advance to next frame
        dey                             ; Y=0
        cmp     (temp_00),y             ; compare frame vs total at seq[0]
        bne     sprite_drawn_flag_check ; more frames? continue
        lda     #$00                    ; reached end: loop back to frame 0
        sta     ent_anim_state,x        ; reset to frame 0

; --- damage flash and sprite definition lookup ---
sprite_drawn_flag_check:  lda     ent_flags,x ; bit 7 = drawn flag (set by process_entity_display)
        bpl     sprite_render_skip_draw ; not marked for drawing? skip
        lda     ent_anim_frame,x        ; bit 7 of tick counter = damage flash active
        bpl     sprite_weapon_slot_check ; not flashing? draw normally
        lda     frame_counter           ; frame counter for blink timing
        and     #$04                    ; blink every 4 frames
        bne     sprite_render_skip_draw ; odd phase = skip draw (invisible)
sprite_weapon_slot_check:  cpx     #$10 ; slot < 16? not a weapon/projectile
        bcc     sprite_anim_frame_strip ; not a weapon? skip lifetime check
        lda     ent_hp,x                ; ent_hp = projectile lifetime/timer
        and     #$E0                    ; upper 3 bits = active countdown
        beq     sprite_anim_frame_strip ; not counting down? normal
        lda     ent_status,x            ; ent_status bit 6 = slow decay flag
        and     #$40                    ; isolate slow-decay bit
        beq     sprite_hp_timer_dec     ; not set? always decay
        lda     $4F                     ; slow decay: only every 4th frame
        and     #$03                    ; every 4th frame only
        bne     sprite_damage_flash_check ; skip decay this frame
sprite_hp_timer_dec:  lda     ent_hp,x  ; subtract $20 from timer
        sec                             ; (decrement upper 3-bit counter)
        sbc     #$20                    ; decrement upper 3-bit counter
        sta     ent_hp,x                ; store decremented timer
sprite_damage_flash_check:  lda     ent_status,x ; bit 6 = explode when timer expires?
        and     #$40                    ; isolate explode-on-expire bit
        beq     sprite_render_skip_draw ; no explode flag? skip draw
        lda     ent_hp,x                ; check if timer reached $20 threshold
        and     #$20                    ; check if timer at $20 threshold
        beq     sprite_anim_frame_strip ; not yet? keep going
        lda     #$AF                    ; use explosion sprite def $AF
        bne     write_entity_oam        ; (always branches)
sprite_anim_frame_strip:  lda     ent_anim_state,x ; current frame index (strip bit 7)
        and     #$7F                    ; strip bit 7
        clc                             ; +2 to skip header bytes (count, duration)
        adc     #$02                    ; skip count + duration bytes
        tay                             ; Y = frame index + 2
        lda     (temp_00),y             ; load sprite def ID from sequence
        bne     write_entity_oam        ; nonzero? go draw it
        sta     ent_status,x            ; def ID = 0: deactivate entity
        lda     #$FF                    ; mark as despawned
        sta     ent_spawn_id,x          ; mark spawn slot as used
sprite_render_skip_draw:  rts            ; return without drawing

; -----------------------------------------------
; write_entity_oam — assembles OAM entries from sprite definition
; -----------------------------------------------
; Entry: A = sprite definition ID
;   $10 = H-flip mask (ENT_FLAG_HFLIP or $00)
;   $11 = flip table offset (0=normal, 1=flipped)
;   $12 = entity screen Y position
;   $13 = entity screen X position
;   oam_ptr = OAM buffer write pointer
; Sprite definition format:
;   byte 0 = sprite count (bit 7: use CHR bank $14 instead of $19)
;   byte 1 = position offset table index (for Y/X offsets)
;   byte 2+ = pairs of (CHR tile, OAM attribute) per sprite
; Position offset table at ($05/$06): Y offset, X offset per sprite

write_entity_oam:  tay                  ; sprite def ID → index
        lda     $8100,y                 ; $02/$03 = pointer to sprite definition
        sta     $02                     ; (low bytes at $8100+ID,
        lda     $8200,y                 ; high bytes at $8200+ID)
        sta     $03                     ; store ptr high byte
        ldy     #$00                    ; start at byte 0 of definition
        lda     ($02),y                 ; byte 0 = sprite count + bank flag
        pha                             ; save sprite count + bank flag
        ldy     #$19                    ; default CHR bank = $19
        pla                             ; restore count + bank flag
        bpl     write_oam_set_chr_bank  ; bit 7 clear → use bank $19
        and     #$7F                    ; strip bank flag from count
        ldy     #$14                    ; use CHR bank $14 instead
write_oam_set_chr_bank:  sta     $04    ; $04 = sprite count (0-based loop counter)
        cpy     prg_bank                ; current PRG bank already correct?
        beq     write_oam_position_offsets ; yes → skip bank switch
        sty     prg_bank                ; set new PRG bank
        stx     $05                     ; save entity slot
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $05                     ; restore entity slot
write_oam_position_offsets:  ldy     #$01 ; byte 1 = position offset table index
        lda     ($02),y                 ; add flip offset ($11=0 or 1)
        clc                             ; to select normal/flipped offsets
        adc     $11                     ; add flip offset for mirrored positions
        pha                             ; save offset table index
        lda     ent_flags,x             ; bit 5 = on-ladder flag
        and     #$20                    ; stored to $11 for OAM attr overlay
        sta     $11                     ; (behind-background priority)
        pla                             ; offset table index → X
        tax                             ; X = offset table index
        lda     $BE00,x                 ; $BE00/$BF00 = pointer table for
        sec                             ; sprite position offsets
        sbc     #$02                    ; subtract 2 to align with def bytes
        sta     $05                     ; $05/$06 = offset data pointer
        lda     $BF00,x                 ; offset table ptr high
        sbc     #$00                    ; borrow from high byte
        sta     $06                     ; store offset ptr high byte
        ldx     oam_ptr                 ; X = OAM write position
        beq     write_oam_buffer_full   ; 0 = buffer wrapped, full
write_oam_sprite_loop:  lda     #$F0    ; default Y clip boundary = $F0
        sta     temp_00                 ; $00 = Y clip boundary
        lda     stage_id                ; check stage ID
        cmp     #STAGE_DOC_NEEDLE       ; Doc Robot Needle stage?
        bne     write_oam_read_tile     ; no → use default clip
        lda     camera_screen           ; camera screen position
        cmp     #$15                    ; screen $15? underwater area
        beq     write_oam_y_clip        ; yes → reduce Y clip
        cmp     #$1A                    ; screen $1A? underwater area
        bne     write_oam_read_tile     ; not underwater → default clip
write_oam_y_clip:  lda     #$B0         ; Y clip = $B0 for these screens
        sta     temp_00                 ; store reduced Y clip boundary
write_oam_read_tile:  iny               ; read CHR tile from def
        lda     ($02),y                 ; read CHR tile number
        sta     $0201,x                 ; OAM byte 1 = tile index
        lda     $12                     ; screen Y + offset Y
        clc                             ; add Y position offset
        adc     ($05),y                 ; Y pos = screen Y + offset
        sta     $0200,x                 ; OAM byte 0 = Y position
        lda     ($05),y                 ; overflow detection:
        bmi     write_oam_y_underflow   ; offset negative?
        bcc     write_oam_y_range       ; no underflow → check Y range
        bcs     write_oam_skip_attr     ; positive offset + carry = overflow
write_oam_y_underflow:  bcc     write_oam_skip_attr ; neg offset + no borrow = underflow
write_oam_y_range:  lda     $0200,x     ; check Y against clip boundary
        cmp     temp_00                 ; compare Y to clip boundary
        bcs     write_oam_skip_attr     ; Y >= clip? hide sprite
        iny                             ; read OAM attribute from def
        lda     ($02),y                 ; EOR with flip flag ($10)
        eor     $10                     ; ORA with ladder priority ($11)
        ora     $11                     ; apply behind-background priority
        sta     $0202,x                 ; OAM byte 2 = attribute
        lda     $13                     ; screen X + offset X
        clc                             ; add X position offset
        adc     ($05),y                 ; add X position offset
        sta     $0203,x                 ; OAM byte 3 = X position
        lda     ($05),y                 ; overflow detection for X
        bmi     write_oam_x_overflow    ; X offset negative?
        bcc     energy_bar_draw_loop    ; no overflow → write next sprite
        bcs     write_oam_hide_sprite   ; positive offset + carry = offscreen
write_oam_x_overflow:  bcc     write_oam_hide_sprite ; neg offset + no borrow = offscreen
energy_bar_draw_loop:  inx              ; advance OAM pointer by 4
        inx                             ;  (4 bytes per OAM entry:
        inx                             ;   Y, tile, attr, X)
        inx                             ;  X now points to next entry
        stx     oam_ptr                 ; update write position
        beq     write_oam_buffer_full   ; wrapped to 0? buffer full
write_oam_sprite_next:  dec     $04     ; decrement sprite count
        bpl     write_oam_sprite_loop   ; more sprites? continue
write_oam_buffer_full:  rts             ; OAM buffer full or all sprites done

write_oam_skip_attr:  iny               ; skip attribute byte
write_oam_hide_sprite:  lda     #$F8    ; hide sprite (Y=$F8 = below screen)
        sta     $0200,x                 ; Y = $F8 hides sprite offscreen
        bne     write_oam_sprite_next   ; always taken → next sprite

; -----------------------------------------------
; draw_energy_bars — draws up to 3 HUD energy meters
; -----------------------------------------------
; $B1-$B3 = bar slot IDs (bit 7 = active, bits 0-6 = energy index)
; $A2+Y = energy value ($80=empty, HEALTH_FULL=full, 28 units)
; Alternates iteration direction each frame for OAM priority fairness.
; Each bar draws 7 sprites vertically: tile selected from $F313 table,
; position from $F318 (OAM attr) and $F31B (X position).
draw_energy_bars:  lda     $95          ; alternate direction each frame
        lsr     a                       ; even/odd frame check
        bcs     energy_bar_reverse      ; odd = reverse
        ldx     #$00                    ; forward: bar 0, 1, 2
        stx     $10                     ; $10 = bar index
energy_bar_forward_loop:  jsr     draw_one_energy_bar ; draw bar[$10]
        inc     $10                     ; next bar index
        ldx     $10                     ; load next bar index
        cpx     #$03                    ; all 3 bars drawn?
        bne     energy_bar_forward_loop ; loop 3 bars
        rts                             ; return

energy_bar_reverse:  ldx     #$02       ; reverse: bar 2, 1, 0
        stx     $10                     ; start at bar 2
energy_bar_reverse_loop:  jsr     draw_one_energy_bar ; +2 to skip header bytes (count, duration)
        dec     $10                     ; next bar (decrement)
        ldx     $10                     ; load bar index
        bpl     energy_bar_reverse_loop ; bar index >= 0? continue
energy_bar_exit:  rts                   ; return

; draw_one_bar — draws a single energy meter
; X = bar index (0-2), $B1+X = slot ID

draw_one_energy_bar:  lda     $B1,x     ; bit 7 = bar active?
        bpl     energy_bar_exit         ; not active? skip
        and     #$7F                    ; energy index (Y into $A2 table)
        tay                             ; Y = energy index
        lda     player_hp,y             ; read energy value
        and     #$7F                    ; strip bit 7 (display flag)
        sta     temp_00                 ; $00 = energy remaining (0-28)
        lda     bar_attributes,x        ; $01 = OAM attribute (palette)
        sta     $01                     ; bar 0=$00, 1=$01, 2=$02
        lda     bar_x_positions,x       ; $02 = X position
        sta     $02                     ; bar 0=$10, 1=$18, 2=$28
        ldx     oam_ptr                 ; X = OAM write pointer
        beq     energy_bar_buffer_full  ; 0 = full, skip
        lda     #$48                    ; $03 = starting Y position ($48)
        sta     $03                     ; draws upward 7 segments
energy_bar_segment_loop:  lda     $01  ; write OAM attribute
        sta     $0202,x                 ; OAM byte 2 = attribute/palette
        lda     $02                     ; write X position
        sta     $0203,x                 ; OAM byte 3 = X position
        lda     $03                     ; write Y position
        sta     $0200,x                 ; OAM byte 0 = Y position
        ldy     #$04                    ; 4 energy units per segment
        lda     temp_00                 ; load remaining energy
        sec                             ; subtract 4 from remaining
        sbc     #$04                    ; subtract 4 units
        bcs     energy_bar_fill_segment ; still >= 0? full segment tile
        ldy     temp_00                 ; partial: Y = remaining 0-3
        lda     #$00                    ; energy = 0
energy_bar_fill_segment:  sta     temp_00 ; update remaining energy
        lda     bar_fill_tiles,y        ; tile from fill table: 4=$6B 3=$6A 2=$69 1=$68 0=$67
        sta     $0201,x                 ; OAM tile
        inx                             ; advance OAM by 4
        inx                             ; advance OAM pointer
        inx                             ; (4 bytes per entry)
        inx                             ; (continued)
        beq     energy_bar_buffer_full  ; OAM buffer full?
        lda     $03                     ; Y -= 8 (move up one tile)
        sec                             ; prepare for subtraction
        sbc     #$08                    ; move up 8 pixels (one tile height)
        sta     $03                     ; store updated Y position
        cmp     #$10                    ; stop at Y=$10 (7 segments: $48→$10)
        bne     energy_bar_segment_loop ; loop if Y > $10
energy_bar_buffer_full:  stx     oam_ptr ; update OAM pointer
        rts                             ; return

; energy bar tile table: index=fill level (0=empty, 4=full)
bar_fill_tiles:  .byte   $6B,$6A,$69,$68,$67

; energy bar OAM attributes (palette): bar 0, 1, 2
bar_attributes:  .byte   $00,$01,$02

; energy bar X positions: bar 0=$10, 1=$18, 2=$28
bar_x_positions:  .byte   $10,$18,$28,$A0,$FB,$2A,$EC,$88
        .byte   $DF,$B8,$FE,$08,$50,$0A,$EB,$0A
        .byte   $6D,$8A,$6F,$2A,$D0,$A0,$B7,$8E
        .byte   $CD,$0A,$EC,$28,$C8,$28,$13,$A0
        .byte   $F7,$68,$83,$80,$CC,$A8,$BE,$AA
        .byte   $FB,$AA,$ED,$2A,$F9,$08,$F4,$A8
        .byte   $F7,$AA,$FB,$20,$CF,$2A,$9F,$28
        .byte   $BF,$CA,$FC,$E0,$78,$CA,$FF,$A2
        .byte   $BF,$20,$FB,$6A,$FF,$28,$FE,$AA
        .byte   $66,$A8,$B7,$8A,$59,$A2,$B2,$2B
        .byte   $AD,$0A,$96,$22,$3A,$82,$CA,$2A
        .byte   $DB,$A8,$92,$9A,$27,$88,$7E,$2A
        .byte   $E7,$46,$EE,$A2,$E9,$26,$C6,$C8
        .byte   $8C,$02,$DE,$B6,$AF,$E2,$5F,$EA
        .byte   $7D,$8A,$BB,$BA,$EC,$02,$FE,$90
        .byte   $E7,$88,$F7,$AE,$FE,$EA,$EE,$2A
        .byte   $B9,$2A,$BB,$A8,$77,$40,$E3,$AA
        .byte   $3D,$0A,$B2,$B8,$4F,$8A,$B8,$CC
        .byte   $FB,$A2,$FB,$81,$6E,$B2,$AD,$82
        .byte   $EF,$93,$76,$AC,$5F,$2E,$CF,$04
        .byte   $E1,$B0,$FF,$82,$9C,$2A,$FF,$6A
        .byte   $83,$A8,$D8,$AA,$4F,$2B,$F3,$28
        .byte   $5C,$2E,$D4,$8A,$52,$82,$67,$0A
        .byte   $D3,$EA,$5E,$0A,$9C,$8A,$36,$82
        .byte   $AB,$2A,$7E,$AD,$F8,$A0,$CE,$2A
        .byte   $2D,$A0,$ED,$88,$9F,$88,$D7,$2A
        .byte   $BF,$AA,$78,$A0,$ED,$A8,$DC,$0E
        .byte   $9B,$28,$FE,$EA,$75,$A2,$57,$A8
        .byte   $99,$A2,$7F,$A2,$B9,$7A,$04,$FC
        .byte   $11,$DC,$05,$3B,$05,$9B,$40,$4B
        .byte   $55,$3C,$51,$8A,$00,$67,$54,$72
        .byte   $15,$1A,$11,$59,$15,$AA,$45,$16
        .byte   $41,$47,$05,$A0,$51,$F1,$45,$76
        .byte   $10,$84,$11,$FF,$14,$6A,$54,$B8
        .byte   $54,$DC,$55,$FD,$40,$92,$51,$1D
        .byte   $14,$32,$40,$C3,$44,$40,$11,$84
        .byte   $45,$96,$44,$D9,$11,$0F,$01,$DB
        .byte   $40,$96,$01,$24,$11,$62,$19,$83
        .byte   $55,$4F,$15,$E7,$14,$3F,$04,$A0
        .byte   $45,$EE,$70,$8A,$54,$59,$44,$7D
        .byte   $11,$39,$44,$6F,$41,$1B,$15,$42
        .byte   $05,$71,$50,$E8,$11,$FD,$44,$2E
        .byte   $00,$BC,$45,$F1,$14,$7B,$51,$E1
        .byte   $45,$49,$10,$2C,$44,$46,$45,$F3
        .byte   $45,$46,$04,$E7,$40,$41,$00,$E8
        .byte   $41,$7A,$01,$E7,$11,$92,$11,$9F
        .byte   $00,$E7,$45,$4E,$51,$D0,$11,$C1
        .byte   $45,$26,$41,$48,$40,$0C,$50,$CD
        .byte   $51,$31,$54,$4D,$50,$6B,$00,$98
        .byte   $51,$78,$14,$CF,$55,$4C,$05,$8E
        .byte   $14,$26,$15,$EB,$04,$CB,$14,$68
        .byte   $50,$06,$00,$F9,$15,$9B,$20,$0C
        .byte   $15,$DE,$01,$C1,$55,$B5,$51,$20
        .byte   $11,$6A,$40,$C1,$00,$19,$54,$58
        .byte   $05,$2C,$41,$0B,$40,$11,$00,$E6
        .byte   $50,$AF,$14,$94,$00,$03,$51,$E6
        .byte   $04,$8F,$15,$EE,$51,$6E,$11,$D7
        .byte   $55,$F6,$05,$6E,$55,$C5,$D5,$CB
        .byte   $51,$FF,$40,$7A,$41,$FA,$50,$57
        .byte   $55,$59,$10,$40,$10,$29,$C4,$01
        .byte   $00,$20,$54,$2D,$45,$60,$04,$89
        .byte   $14,$2F,$65,$84,$51,$A1,$41,$BA
        .byte   $1C,$07,$01,$6C,$41,$79,$54,$88
        .byte   $51,$21,$40,$F9,$14,$3C,$11,$24
        .byte   $15,$A0,$54,$86,$1A,$78,$10,$41
        .byte   $05,$94,$54,$6D,$11,$85,$40,$A1
        .byte   $45,$25,$45,$23,$45,$53,$14,$E3
        .byte   $51,$67,$4C,$F7,$01,$8A,$14,$62
        .byte   $44,$C6,$45,$CA,$44,$E0,$45,$E7
        .byte   $01,$05,$14,$04,$41,$F6,$05,$C3
        .byte   $10,$03,$40,$AA,$40,$CA,$01,$66
        .byte   $05,$DB,$05,$0C,$14,$3D,$44,$80
        .byte   $50,$0A,$51,$AC,$71,$EE,$40,$53
        .byte   $D4,$F5,$05,$56,$00,$EC,$14,$9A
        .byte   $10,$B0,$50,$BE,$55,$F6,$14,$7C
        .byte   $01,$E0,$54,$2A,$45,$4A,$51,$6D
        ora     $38,x                   ; data (disassembler artifact)
        .byte   $14                     ; password table (continued)
        .byte   $E2
        .byte   $50

