; ===========================================================================
; update_camera — horizontal scroll engine + room transition detection
; ===========================================================================
; Called every frame from gameplay_frame_loop (step 3). Tracks the player's
; X position and smoothly scrolls the camera to keep the player centered.
;
; Scroll variables (all Mesen-verified on Snake Man stage):
;   $2A = scroll flags (from upper 3 bits of $AA40,y per room)
;         bit 5 = horizontal scrolling enabled
;         bits 6-7 = vertical connection direction ($40=down, $80=up)
;   $2B = room/section index (changes at transitions: ladders, doors)
;   $2C = screen count for current room (lower 5 bits of $AA40,y)
;   $2D = scroll progress within room (counts 0→$2C as camera advances)
;   $2E = scroll direction this frame (1=right, 2=left)
;   camera_screen ($F9) = screen position (increments each 256-pixel boundary)
;   camera_x_lo ($FC) = fine X (sub-screen pixel offset, 0-255)
;   $25 = previous camera_x_lo (for column rendering delta)
;   $27 = previous player X (for direction detection)
;   ent_x_px = player X pixel position (slot 0)
;
; Camera tracking:
;   $00 = player screen X (= ent_x_px - camera_x_lo)
;   $01 = scroll speed (= |player screen X - $80|, clamped to max 8)
;   $02 = player movement delta (= |ent_x_px - $27|)

;   When player is right of center ($00 > $80): scroll right
;   When player is left of center ($00 < $80): scroll left
;   Scroll speed adapts — faster when player is far from center,
;   uses movement delta as alternative speed when it's smaller
; ---------------------------------------------------------------------------
update_camera:  lda     $2A             ; check scroll flags
        and     #$20                    ; bit 5 = horizontal scroll enabled
        bne     camera_scroll_dir_right ; enabled → compute scroll
        jmp     camera_mid_scroll_check ; disabled → boundary clamp only

; --- horizontal scroll: compute player screen position and scroll speed ---

camera_scroll_dir_right:  lda     #$01  ; default: scroll direction = right
        sta     $10                     ; $10 = nametable select (1=right)
        sta     $2E                     ; $2E = scroll direction flag
        lda     ent_x_px                ; $00 = player screen X
        sec                             ; = player pixel X - camera fine X
        sbc     camera_x_lo             ; subtract camera fine X
        sta     temp_00                 ; store player screen X in $00
        sec                             ; prepare subtraction
        sbc     #$80                    ; = distance from screen center
        bcs     camera_compute_distance ; positive → player right of center
        eor     #$FF                    ; negate if negative
        adc     #$01                    ; two's complement negate
camera_compute_distance:  sta     $01   ; $01 = abs distance from center
        lda     ent_x_px                ; $02 = player X - previous X ($27)
        sec                             ; = movement delta this frame
        sbc     $27                     ; subtract previous player X ($27)
        sta     $02                     ; store movement delta
        bpl     camera_scroll_right_center ; positive → player moved right

; --- player moving left: negate delta, compute leftward scroll speed ---
        eor     #$FF                    ; negate to get abs delta
        clc                             ; prepare addition
        adc     #$01                    ; two's complement negate
        sta     $02                     ; store abs movement delta
        lda     $01                     ; if center distance >= 9,
        cmp     #$09                    ; use movement delta instead
        bcc     camera_scroll_left_clamp ; (prevents jerky scroll when
        lda     $02                     ; player is far off-center but
        sta     $01                     ; moving slowly)
camera_scroll_left_clamp:  lda     #$08 ; clamp scroll speed to max 8
        cmp     $01                     ; max 8 >= speed?
        bcs     camera_scroll_left_dir  ; yes → no clamp needed
        sta     $01                     ; $01 = min(8, $01)

; --- scroll camera left ---
camera_scroll_left_dir:  lda     #$02   ; direction = left
        sta     $10                     ; $10 = 2 (nametable select)
        sta     $2E                     ; $2E = 2 (scroll direction)
        lda     temp_00                 ; if player screen X >= $80 (right of center)
        cmp     #$80                    ; don't scroll left — go to boundary check
        bcs     camera_mid_scroll_check  ; right of center → don't scroll left
        lda     camera_x_lo             ; camera_x_lo -= scroll speed
        sec                             ; (move camera left)
        sbc     $01                     ; subtract scroll speed
        sta     camera_x_lo             ; store updated fine scroll
        bcs     camera_scroll_render_column ; no underflow → just render
        lda     $2D                     ; $FC underflowed: crossed screen boundary
        dec     $2D                     ; decrement room scroll progress
        bpl     camera_screen_decrement ; still in room? advance screen
        sta     $2D                     ; $2D went negative: at left boundary
        lda     #$00                    ; clamp $FC = 0 (can't scroll further)
        sta     camera_x_lo             ; clamp fine scroll to 0
        lda     #$10                    ; clamp player X to minimum $10
        cmp     ent_x_px                ; (16 pixels from left edge)
        bcc     camera_mid_scroll_check ; player X > $10 → ok
        sta     ent_x_px                ; enforce left boundary
        bcs     camera_mid_scroll_check ; always branch
camera_screen_decrement:  dec     camera_screen ; camera screen position--
        jmp     camera_scroll_render_column ; render new column

; --- scroll camera right ---

camera_scroll_right_center:  lda     temp_00 ; if player screen X < $81 (at/left of center)
        cmp     #$81                    ; don't scroll right
        bcc     camera_mid_scroll_check  ; left of center → don't scroll right
        lda     $2D                     ; if scroll progress == screen count
        cmp     $2C                     ; already at right boundary
        beq     camera_mid_scroll_check ; → don't scroll
        lda     $01                     ; if center distance >= 9,
        cmp     #$09                    ; use movement delta as speed
        bcc     camera_scroll_right_clamp ; distance < 9 → skip delta swap
        lda     $02                     ; use movement delta as speed
        sta     $01                     ; use delta as speed
camera_scroll_right_clamp:  lda     #$08 ; clamp scroll speed to max 8
        cmp     $01                     ; max 8 >= speed?
        bcs     camera_scroll_speed_zero ; yes → no clamp needed
        sta     $01                     ; $01 = min(8, $01)
camera_scroll_speed_zero:  lda     $01  ; speed = 0? nothing to do
        beq     camera_mid_scroll_check ; nothing to scroll
        lda     camera_x_lo             ; camera_x_lo += scroll speed
        clc                             ; (move camera right)
        adc     $01                     ; add scroll speed
        sta     camera_x_lo             ; store updated fine scroll
        bcc     camera_scroll_render_column ; no overflow → just render
        inc     $2D                     ; $FC overflowed: crossed screen boundary
        lda     $2D                     ; increment room scroll progress
        cmp     $2C                     ; reached room right boundary?
        bne     camera_screen_increment ; not at boundary? advance screen
        lda     #$00                    ; at right boundary: clamp $FC = 0
        sta     camera_x_lo             ; clamp fine scroll to 0
        lda     #$F0                    ; clamp player X to maximum $F0
        cmp     ent_x_px                ; (240 pixels from left edge)
        bcs     camera_screen_increment ; player X <= $F0 → ok
        sta     ent_x_px                ; enforce right boundary
camera_screen_increment:  inc     camera_screen ; camera screen position++
camera_scroll_render_column:  jmp     render_scroll_column ; → column rendering

camera_h_scroll_return:  rts

; ===========================================================================
; No-scroll: boundary clamping + room transition detection
; ===========================================================================
; When horizontal scrolling is disabled or camera is between screens,
; clamp player X to screen bounds and check if player has walked to
; the edge of the screen (triggering a room transition via ladder/door).
; ---------------------------------------------------------------------------

camera_mid_scroll_check:  lda     camera_x_lo ; if camera is mid-scroll (camera_x_lo != 0)
        bne     camera_h_scroll_return  ; nothing more to do
        lda     ent_x_px                ; clamp player X >= $10 (left edge)
        cmp     #$10                    ; at left edge?
        bcs     camera_right_edge_check ; X >= $10 → check right edge
        lda     #$10                    ; clamp player X to $10
        sta     ent_x_px                ; enforce left boundary
        beq     camera_right_edge_check ; (always branches, A=$10)
camera_check_vert_transition:  jmp     check_vertical_transition ; check vertical transition

camera_right_edge_check:  cmp     #$E5  ; player X < $E5? not at right edge
        bcc     camera_check_vert_transition ; → check vertical instead
        cmp     #$F0                    ; clamp player X <= $F0
        bcc     camera_room_transition_right ; in range → check transition
        lda     #$F0                    ; clamp player X to $F0
        sta     ent_x_px                ; enforce right boundary

; --- room transition: player walked to right edge ($E5+) of non-scrolling screen ---
camera_room_transition_right:  ldy     $2B ; current room entry
        lda     $AA40,y                 ; check if current room allows
        and     #$20                    ; horizontal scroll (bit 5)
        beq     camera_check_vert_transition ; no scroll attr → vertical check
        lda     $AA41,y                 ; next room entry: check vertical
        and     #$C0                    ; connection bits (6-7)
        bne     camera_check_vert_transition ; has vertical link → not a horiz transition
        lda     $AA41,y                 ; next room: must also have horiz scroll
        and     #$20                    ; (bit 5 set) to allow transition
        beq     camera_check_vert_transition ; no scroll → vertical check
        sta     temp_00                 ; store next room scroll flags
        lda     stage_id                ; stage-specific transition blocks:
        cmp     #STAGE_DOC_NEEDLE       ; stage $08 (Doc Robot Needle)
        bne     camera_screen_offset_check ; skip if not stage $08
        lda     camera_screen           ; Rush Marine water boundary screens
        cmp     #$15                    ; $15 and $1A have special gate
        beq     camera_entity_slot_check ; screen $15 → check gate
        cmp     #$1A                    ; screen $1A → check gate
        bne     camera_screen_offset_check ; other screen → skip gate check
camera_entity_slot_check:  lda     $033F ; entity slot $1F type == $FC?
        cmp     #$FC                    ; (gate/shutter entity present
        beq     camera_check_vert_transition ; → block transition until opened)
camera_screen_offset_check:  lda     camera_screen ; load camera screen position
        sec                             ; subtract room base screen
        sbc     $AA30                   ; compute screen offset
        cmp     #$02                    ; if exactly 2 screens in:
        bne     camera_wily5_stage_check ; not 2 screens → skip boss shutter check
        lda     $031F                   ; check entity slot $1F status
        bmi     camera_check_vert_transition ; active → block room transition
        lda     player_state            ; player state >= $0C (victory)?
        cmp     #PSTATE_VICTORY         ; block if in cutscene state
        bcs     camera_check_vert_transition ; cutscene → block transition
camera_wily5_stage_check:  lda     stage_id ; stage $0F (Wily Fortress 4)
        cmp     #STAGE_WILY4            ; special check
        bne     camera_next_room_scroll ; not Wily 4 → advance to next room
        lda     camera_screen           ; only on screen $08
        cmp     #$08                    ; boss refight screen?
        bne     camera_next_room_scroll ; no → advance normally
        ldx     #$0F                    ; scan enemy slots $1F-$10
camera_entity_status_check:  lda     $0310,x ; ent_status[$10+x]
        bmi     camera_check_vert_transition ; bit 7 = active → block transition
        dex                             ; next enemy slot
        bpl     camera_entity_status_check ; check all 16 slots

; --- advance to next room: set up new room variables ---
camera_next_room_scroll:  lda     temp_00 ; next room's scroll flags
        sta     $2A                     ; store scroll flags
        lda     $AA41,y                 ; $2C = next room's screen count
        and     #$1F                    ; (lower 5 bits)
        sta     $2C                     ; store screen count
        lda     #$00                    ; $2D = 0 (start of new room)
        sta     $2D                     ; reset scroll progress
        inc     $2B                     ; room index++
        ldy     game_mode               ; save game_mode before clearing
        lda     #$00                    ; then clear transition state:
        sta     game_mode               ; game_mode = 0 (normal rendering)
        sta     $76                     ; $76 = 0 (enemy spawn flag)
        sta     $B3                     ; $B3 = 0 (?)
        sta     boss_active             ; clear boss active flag
        lda     #$E8                    ; $5E = $E8 (despawn boundary Y)
        sta     $5E                     ; set despawn boundary Y
        cpy     #$02                    ; was $F8 == 2? (camera scroll mode)
        bne     camera_nt_ptr_setup     ; no → skip
        lda     #$42                    ; special scroll init:
        sta     $E9                     ; $E9 = $42 (CHR bank?)
        lda     #$09                    ; $29 = $09 (metatile column)
        sta     $29                     ; set metatile column base
        jsr     load_room               ; load room layout + CHR/palette
camera_nt_ptr_setup:  lda     camera_screen ; compute screen offset from room base
        sec                             ; prepare subtraction
        sbc     $AA30                   ; subtract room base screen
        bcc     camera_stage_specific_check ; negative → no event trigger
        cmp     #$03                    ; < 3 screens: use table 1 ($AA31)
        bcs     camera_screen_count_3   ; >= 3 → use second table
        tax                             ; X = screen offset
        ldy     $AA31,x                 ; Y = event ID from table
        jmp     camera_bank10_event_call ; call event handler

camera_screen_count_3:  lda     camera_screen ; >= 3 screens: use second table
        sec                             ; compute offset from second base
        sbc     $AA38                   ; subtract second base screen
        bcc     camera_stage_specific_check ; negative → no event
        tax                             ; X = screen offset
        ldy     $AA39,x                 ; Y = event ID from table
camera_bank10_event_call:  jsr     call_bank10_8000 ; call bank $10 event handler
camera_stage_specific_check:  lda     stage_id ; stage-specific: Needle Man ($00)
        bne     camera_enter_from_left  ; skip if not Needle Man stage
        ldx     #$03                    ; copy 4 bytes from $AAA2 → $060C
camera_stage_spawn_table:  lda     $AAA2,x ; copy stage spawn data
        sta     $060C,x                 ; to $060C buffer
        dex                             ; next byte index
        bpl     camera_stage_spawn_table ; loop all 4 bytes
        stx     palette_dirty           ; palette_dirty = $FF (force update)
camera_enter_from_left:  lda     #$E4   ; player X = $E4 (entering from left)
        sta     ent_x_px                ; set player X pixel
        jsr     fast_scroll_right       ; clear entities + fast-scroll camera
        lda     stage_id                ; re-select stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        lda     camera_screen           ; second event dispatch (post-scroll)
        sec                             ; prepare subtraction
        sbc     $AA30                   ; subtract room base screen
        bcc     scroll_engine_rts       ; negative → done
        cmp     #$05                    ; < 5: use $AA30 table
        bcs     camera_screen_count_5   ; >= 5 → use second table
        tax                             ; X = screen offset
        ldy     $AA30,x                 ; Y = event ID from table
        jmp     camera_bank10_post_scroll ; call post-scroll handler

camera_screen_count_5:  lda     camera_screen ; >= 5: use $AA38 table
        sec                             ; prepare subtraction
        sbc     $AA38                   ; subtract second base screen
        bcc     scroll_engine_rts       ; negative → done
        beq     scroll_engine_rts       ; zero → done
        tax                             ; X = screen offset
        ldy     $AA38,x                 ; Y = event ID from table
camera_bank10_post_scroll:  jsr     call_bank10_8003 ; call bank $10 post-scroll handler
scroll_engine_rts:  rts

; ===========================================================================
; check_vertical_transition — detect player at top/bottom of screen
; ===========================================================================
; When horizontal scrolling is not active ($FC==0, not scrolling), checks
; if the player has reached the top or bottom of the visible screen.
;   ent_y_px = player Y pixel, ent_y_scr = player Y screen
;   $10 = direction flag: $80=up, $40=down
;   $23 = vertical scroll direction: $08=up, $04=down
; ---------------------------------------------------------------------------

check_vertical_transition:  lda     ent_y_px ; player Y pixel
        cmp     #$E8                    ; >= $E8? → fell off bottom
        bcs     camera_vert_check_y_screen ; yes → check Y screen page
        cmp     #$09                    ; < $09? → at top of screen
        bcs     scroll_engine_rts       ; $09-$E7 = normal range, RTS
        lda     player_state            ; must be climbing (state $03)
        cmp     #PSTATE_LADDER          ; to transition upward
        bne     scroll_engine_rts       ; not on ladder → skip
        lda     #$80                    ; $10 = $80 (upward direction)
        sta     $10                     ; set direction = up
        jsr     check_room_link         ; validate room connection
        bcc     scroll_engine_rts       ; C=0 → no valid link
        lda     #$80                    ; confirmed: transition up
        sta     $10                     ; set direction = up
        lda     #$08                    ; $23 = $08 (vertical scroll up)
        bne     camera_scroll_dir_set   ; always branch (set scroll dir)
camera_vert_check_y_screen:  lda     ent_y_scr ; player Y screen: bit 7 set?
        bmi     camera_check_vert_end   ; negative = death pit → RTS
        lda     #$40                    ; $10 = $40 (downward direction)
        sta     $10                     ; set direction = down
        jsr     check_room_link         ; validate room connection
        bcc     camera_check_vert_end   ; C=0 → no valid link (death)
        lda     #$40                    ; confirmed: transition down
        sta     $10                     ; set direction = down
        lda     #$04                    ; $23 = $04 (vertical scroll down)

; --- begin vertical room transition ---
camera_scroll_dir_set:  sta     $23     ; $23 = scroll direction ($04/$08)
        lda     #$00                    ; reset Y screen to 0
        sta     ent_y_scr               ; clear Y screen
        ldx     #$01                    ; $12 = direction-dependent flag
        ldy     $2B                     ; check if current room's vertical
        lda     $AA40,y                 ; connection matches $10
        and     #$C0                    ; isolate vertical link bits (6-7)
        cmp     $10                     ; compare with requested direction
        beq     camera_stx_load         ; match → use default $12
        ldx     #$FF                    ; mismatch → $12 = $FF (reverse)
camera_stx_load:  stx     $12
        lda     #$00                    ; clear Y sub-pixel
        sta     ent_y_sub               ; clear Y sub-pixel
        sta     game_mode               ; game_mode = 0 (normal render mode)
        lda     #$E8                    ; $5E = $E8 (despawn boundary Y)
        sta     $5E                     ; $5E = $E8 (despawn boundary)
        jsr     clear_destroyed_blocks  ; reset breakable blocks
        jsr     vertical_scroll_animate ; animate vertical scroll
        lda     stage_id                ; re-select stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldy     $2B                     ; update $2A from new room's scroll flags
        lda     $AA40,y                 ; load new room flags
        and     #$20                    ; (bit 5 = horizontal scroll)
        beq     camera_mmc3_protect_enable ; no h-scroll → skip
        sta     $2A                     ; store scroll flags
camera_mmc3_protect_enable:  lda     #$01 ; set H-mirroring
        sta     MMC3_MIRRORING          ; apply H-mirroring
        lda     #$2A                    ; $52 = $2A (viewport height, h-mirror)
        sta     $52                     ; set viewport height
        jsr     load_room               ; load room layout + CHR/palette
        ldx     #$00                    ; check player tile collision at (0,4)
        ldy     #$04                    ; to snap player into new room
        jsr     check_tile_collision    ; check tile at player feet
        lda     $10                     ; if tile collision result = 0 (air)
        bne     camera_check_vert_end   ; nonzero → solid tile, skip state change
        sta     player_state            ; (landing after vertical transition)
camera_check_vert_end:  rts

; ===========================================================================
; check_room_link — validate that a vertical room connection exists
; ===========================================================================
; Checks if the player can transition to an adjacent room vertically.
; Only allows transitions when at the start ($2D==0) or end ($2D==$2C)
; of the current room's scroll range.
;
; Input:  $10 = direction ($80=up, $40=down)
; Output: C=1 if valid link found, C=0 if no connection
;         Sets up $2B, $2A, $2C, $2D, $29, $F9, ent_x_scr on success
; ---------------------------------------------------------------------------

check_room_link:  lda     $2D           ; scroll progress at room boundary?
        beq     camera_vert_target_screen ; $2D == 0 (at room start) → check link
        cmp     $2C                     ; $2D == $2C (end) → check link
        beq     camera_vert_target_screen ; $2D == $2C (at room end) → check link
        clc                             ; midway through room → no transition
        rts                             ; C=0 → no link

; --- navigate $AA40 room table to find adjacent room in direction $10 ---

camera_vert_target_screen:  lda     camera_screen ; $00 = target screen position
        sta     temp_00                 ; $00 = current screen position
        ldy     $2B                     ; Y = current room index
        lda     $2A                     ; check current room's vertical bits
        and     #$C0                    ; bits 6-7 of $2A
        beq     camera_multi_screen_check ; no vertical → check neighbors
        lda     $2A                     ; current room has vertical link:
        cmp     $10                     ; does direction match?
        beq     room_link_next_entry    ; same direction → scan forward
        bne     room_link_prev_entry    ; opposite → scan backward
camera_multi_screen_check:  lda     $2C ; multi-screen room ($2C > 0)?
        bne     camera_vert_backward_check ; yes → check scroll position
        lda     $AA41,y                 ; next entry's vertical bits
        and     #$C0                    ; match direction?
        cmp     $10                     ; match direction?
        beq     room_link_next_entry    ; yes → look forward
        lda     $AA3F,y                 ; previous entry's vertical bits
        and     #$C0                    ; (inverted: $C0 XOR = flip up/down)
        eor     #$C0                    ; flip up/down bits
        cmp     $10                     ; match requested direction?
        beq     room_link_prev_entry    ; yes → scan backward
        bne     room_link_no_valid      ; no match → fail
camera_vert_backward_check:  lda     $2D ; $2D == 0 (at start) → look backward
        beq     room_link_prev_entry    ; else (at end) → look forward
room_link_next_entry:  iny              ; check next room entry
        lda     $AA40,y                 ; vertical bits must match $10
        and     #$C0                    ; isolate vertical bits
        cmp     $10                     ; match requested direction?
        bne     room_link_no_valid      ; no match → fail
        inc     temp_00                 ; target screen = $F9 + 1
        bne     room_link_setup_vars    ; always branch (screen > 0)
room_link_prev_entry:  lda     $AA40,y  ; current entry must have vert bits
        and     #$C0                    ; isolate vertical link bits
        beq     room_link_no_valid      ; no vertical → fail
        dey                             ; move to previous room entry
        bmi     room_link_no_valid      ; Y < 0 → no previous room
        lda     $AA40,y                 ; check prev room's vertical bits
        and     #$C0                    ; isolate vertical link bits
        bne     room_link_vert_check    ; nonzero → has vertical link
        lda     $AA41,y                 ; no vert bits → check next entry
room_link_vert_check:  eor     #$C0     ; invert and compare with direction
        cmp     $10                     ; (loop safety: BEQ loops if match,
        beq     room_link_vert_check    ; but this shouldn't infinite-loop)
        dec     temp_00                 ; target screen = $F9 - 1
        lda     $AA40,y                 ; load new room's flags

; --- link found: set up new room variables ---
room_link_setup_vars:  sta     $2A      ; $2A = new room's scroll flags
        lda     #$01                    ; $2E = 1 (forward direction)
        sta     $2E                     ; scroll forward
        cpy     $2B                     ; if new room index < current
        sty     $2B                     ; (going backward in table)
        bcs     room_link_set_screen_count ; forward → $2D stays 0
        lda     #$02                    ; $2E = 2 (backward direction)
        sta     $2E                     ; scroll backward
room_link_set_screen_count:  lda     $AA40,y ; $2C = new room's screen count
        and     #$1F                    ; mask to screen count
        sta     $2C                     ; store screen count
        ldx     temp_00                 ; if target screen >= current $F9:
        cpx     camera_screen           ; target screen >= camera screen?
        bcc     room_link_set_position  ; target < camera → set $2D = $2C (end)
        lda     #$00                    ; $2D = 0 (room start)
room_link_set_position:  sta     $2D    ; set scroll progress
        lda     temp_00                 ; update camera and player X screen
        sta     $29                     ; $29 = metatile column base
        sta     camera_screen           ; set camera screen
        sta     ent_x_scr               ; set player X screen
        lda     #$00                    ; set V-mirroring
        sta     MMC3_MIRRORING          ; apply V-mirroring
        lda     #$26                    ; $52 = $26 (viewport height for V-mirror)
        sta     $52                     ; set viewport height
        sec                             ; C=1 → link found
        rts                             ; return with carry set

room_link_no_valid:  clc                ; C=0 → no valid link
        rts                             ; return with carry clear

; ===========================================================================
; render_scroll_column — queue nametable column updates during scrolling
; ===========================================================================
; Called after horizontal scroll speed is applied to $FC. Determines how
; many columns have scrolled into view and queues PPU nametable updates
; via the $0780 NMI buffer.
;
; When $F8 == 2 (dual-nametable mode): renders columns for BOTH nametables
; (first call renders primary, then mirrors to secondary nametable offset).
; Otherwise: renders single column only.
;
; $25 = previous camera_x_lo, camera_x_lo = current fine scroll position
; nt_column_dirty = flag to trigger NMI buffer drain
; ---------------------------------------------------------------------------

render_scroll_column:  lda     game_mode ; if game_mode == 2: dual nametable mode
        cmp     #$02                    ; dual nametable mode?
        bne     scroll_column_single    ; no → single column render
        jsr     scroll_column_single    ; render first nametable column
        bcs     scroll_no_column        ; C=1 → no column needed, skip mirror
        lda     $0780                   ; mirror to second nametable:
        ora     #$22                    ; set bit 1 of high byte ($20→$22)
        sta     $0780                   ; store mirrored PPU addr high byte
        lda     $0781                   ; mirror column to second nametable
        ora     #$80                    ; set bit 7 of low byte
        sta     $0781                   ; store mirrored PPU addr low byte
        lda     #$09                    ; count = 9 (copy 10 tile rows)
        sta     $0782                   ; 10 tile rows to copy
        ldy     #$00                    ; copy attribute data to second
camera_attr_copy_loop:  lda     $0797,y ; read tile data from primary buffer
        sta     $0783,y                 ; copy to secondary buffer
        iny                             ; next byte
        cpy     #$0A                    ; 10 bytes total?
        bne     camera_attr_copy_loop   ; loop until done
        lda     $07A1                   ; check if attribute entry exists
        bpl     camera_attr_finalize    ; bit 7 clear → has attribute data
        sta     $078D                   ; no second attr: terminate here
        sta     nt_column_dirty         ; flag NMI to drain buffer
        rts                             ; return to caller

camera_attr_finalize:  ldy     #$00     ; copy second attribute section
scroll_column_attr_copy:  lda     $07B5,y ; copy secondary attribute section
        sta     $078D,y                 ; to mirror buffer
        iny                             ; next byte
        cpy     #$0D                    ; 13 bytes total?
        bne     scroll_column_attr_copy ; loop until done
        sta     nt_column_dirty         ; flag NMI to drain buffer
        rts                             ; return to caller

; --- single_column: compute if a column crossed an 8-pixel boundary ---
; $03 = |camera_x_lo - $25| = absolute scroll delta since last frame
; If delta + fine position crosses an 8-pixel tile boundary, we need
; to render a new column. $24 = nametable column pointer, $29 = metatile
; column. Direction tables at $E5C3/$E5CD/$E5CF configure left vs right.

scroll_column_single:  lda     camera_x_lo ; $03 = |camera_x_lo - $25
        sec                             ; absolute scroll delta
        sbc     $25                     ; delta = $FC - $25 (previous fine pos)
        bpl     scroll_compute_delta    ; positive → no negate needed
        eor     #$FF                    ; negate if negative
        clc                             ; prepare addition
        adc     #$01                    ; two's complement negate
scroll_compute_delta:  sta     $03      ; $03 = scroll delta (pixels)
        beq     scroll_no_column        ; zero delta → no column to render
        lda     $23                     ; if $23 has vertical scroll bits
        and     #$0C                    ; ($04 or $08), this is first frame
        beq     scroll_direction_setup  ; no vert bits → normal direction check
        lda     $10                     ; overwrite $23 with current direction
        sta     $23                     ; store current direction to $23
        and     #$01                    ; X = direction index (0=left, 1=right)
        tax                             ; X = direction index
        lda     scroll_init_fine_pos_table,x ; init $25 from direction table
        sta     $25                     ; store initial fine scroll position
        lda     scroll_init_column_table,x ; init $24 from direction table
        sta     $24                     ; store initial nametable column
scroll_direction_setup:  lda     $10    ; get fine position for boundary check:
        and     #$01                    ; direction 1 (right): use $25 as-is
        beq     scroll_left_direction   ; direction 0 (left) → invert fine pos
        lda     $25                     ; rightward: use fine pos directly
        jmp     scroll_boundary_check   ; skip inversion

scroll_left_direction:  lda     $25     ; invert for leftward scroll
        eor     #$FF                    ; bitwise complement for left direction
scroll_boundary_check:  and     #$07    ; (fine pos & 7) + delta
        clc                             ; if result / 8 > 0: crossed boundary
        adc     $03                     ; → need to render new column
        lsr     a                       ; divide by 8:
        lsr     a                       ;   result > 0 means crossed tile boundary
        lsr     a                       ;   (8 pixels per tile column)
        bne     do_render_column        ; nonzero → render new column
scroll_no_column:  sec                  ; C=1 → no column to render
        rts                             ; return with carry set

; --- render column: advance nametable pointer and build PPU update ---

do_render_column:  lda     $10          ; X = direction (0 or 1)
        pha                             ; save direction on stack
        and     #$01                    ; isolate direction bit
        tax                             ; X = direction index (0 or 1)
        pla                             ; if direction changed since last frame
        cmp     $23                     ; ($10 != $23): skip column advance,
        sta     $23                     ; just update metatile base
        beq     scroll_column_done      ; same direction → advance column
        jmp     scroll_advance_metatile ; direction changed → skip column advance

scroll_column_done:  lda     $24        ; advance nametable column pointer
        clc                             ; $24 += direction step ($E5C3,x)
        adc     scroll_direction_step_table,x ; add +1 or -1 per direction
        cmp     #$20                    ; wrap at 32 columns (NES nametable)
        and     #$1F                    ; mask to 0-31 range
        sta     $24                     ; store updated column pointer
        bcc     scroll_build_column     ; no column wrap → skip metatile advance
scroll_advance_metatile:  lda     $29   ; $29 += direction step
        clc                             ; (metatile column in level data)
        adc     scroll_direction_step_table,x ; add +1 or -1 per direction
        sta     $29                     ; store updated metatile column

; --- build nametable column from metatile data ---
scroll_build_column:  lda     stage_id  ; select stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        lda     $24                     ; $28 = attribute table row
        lsr     a                       ; = nametable column / 4
        lsr     a                       ; divide column by 4
        sta     $28                     ; store attribute row index
        ldy     $29                     ; set up metatile data pointer
        jsr     metatile_column_ptr     ; for column $29
        lda     #$00                    ; $03 = buffer write offset
        sta     $03                     ; (increments by 4 per row)
scroll_metatile_loop:  jsr     metatile_to_chr_tiles ; decode metatile → $06C0 tile buffer
        ldy     $28                     ; $11 = current attribute byte
        lda     $0640,y                 ; from attribute cache
        sta     $11                     ; save attribute byte
        lda     $24                     ; Y = column within metatile (0-3)
        and     #$03                    ; mask to sub-column (0-3)
        tay                             ; Y = column sub-position
        ldx     $03                     ; write 2 or 4 CHR tiles to buffer
        lda     $06C0,y                 ; top-left tile
        sta     $0783,x                 ; write to PPU column buffer
        lda     $06C4,y                 ; bottom-left tile
        sta     $0784,x                 ; store to PPU column buffer
        lda     $28                     ; if row >= $38 (bottom 2 rows):
        cmp     #$38                    ; skip second pair (status bar area)
        bcs     scroll_attribute_check  ; yes → skip remaining tiles
        lda     $06C8,y                 ; top-right tile
        sta     $0785,x                 ; store to PPU column buffer
        lda     $06CC,y                 ; bottom-right tile
        sta     $0786,x                 ; store to PPU column buffer
scroll_attribute_check:  lda     $24    ; if column is odd: update attribute byte
        and     #$01                    ; (attributes cover 2×2 metatile groups)
        beq     scroll_advance_buffer   ; even column → skip attribute update
        lda     $10                     ; merge new attribute bits:
        and     scroll_attr_mask_left_table,y ; mask with direction table
        sta     $10                     ; save masked new attribute
        lda     $11                     ; combine with old attribute
        and     scroll_attr_mask_right_table,y ; mask old attribute bits
        ora     $10                     ; merge old and new attributes
        sta     $07A4,x                 ; store in attribute buffer
        ldy     $28                     ; update attribute cache
        sta     $0640,y                 ; update attribute cache
        lda     #$23                    ; attribute table PPU address:
        sta     $07A1,x                 ; $23C0 + row offset
        tya                             ; A = attribute row index
        ora     #$C0                    ; form low byte ($C0 + row)
        sta     $07A2,x                 ; store attr PPU addr low byte
        lda     #$00                    ; count = 0 (single byte)
        sta     $07A3,x                 ; 1 attribute byte per entry
scroll_advance_buffer:  inc     $03     ; advance buffer offset by 4
        inc     $03                     ; (4 bytes per metatile row entry)
        inc     $03                     ; (2 tiles + 2 spare)
        inc     $03                     ; total +4
        lda     $28                     ; advance attribute row by 8
        clc                             ; (each metatile = 8 pixel rows)
        adc     #$08                    ; next metatile row block
        sta     $28                     ; store updated row index
        cmp     #$40                    ; < $40 (8 rows)? loop
        bcc     scroll_metatile_loop    ; loop until all 8 metatile rows done
        lda     #$20                    ; finalize PPU buffer header:
        sta     $0780                   ; $0780 = $20 (nametable $2000 base)
        lda     $24                     ; $0781 = nametable column
        sta     $0781                   ; store nametable column
        lda     #$1D                    ; $0782 = $1D (30 tiles = full column)
        sta     $0782                   ; 30 tiles per column
        ldy     #$00                    ; select attribute buffer terminator pos:
        lda     $24                     ; odd column → Y=$20 (secondary buffer)
        and     #$01                    ; even column → Y=$00 (primary buffer)
        beq     scroll_write_end_marker ; even → use primary attr buffer
        ldy     #$20                    ; Y = $20 (secondary attr buffer)
scroll_write_end_marker:  lda     #$FF  ; write $FF end marker
        sta     $07A1,y                 ; write end marker to attr buffer
        ldy     game_mode               ; if game_mode == 2 (dual nametable):
        cpy     #$02                    ; don't flag NMI yet (caller handles it)
        beq     scroll_success_exit     ; dual mode → caller flags NMI
        sta     nt_column_dirty         ; else: flag NMI to drain buffer
scroll_success_exit:  clc               ; C=0 → column was rendered
        rts                             ; return with carry clear

; Direction/column rendering tables:
; $E5C3: direction step (+1/-1 for right/left)
; $E5C5: attribute mask tables (4 bytes each)
; $E5CD: initial $25 values per direction
; $E5CF: initial $24 values per direction

scroll_direction_step_table:  .byte   $FF,$01 ; left = -1, right = +1
scroll_attr_mask_left_table:  .byte   $33,$33,$CC,$CC
scroll_attr_mask_right_table:  .byte   $CC,$CC,$33,$33
scroll_init_fine_pos_table:  .byte   $00,$FF
scroll_init_column_table:  .byte   $01,$1F

; ===========================================================================
; fast_scroll_right — rapid camera scroll during room transition
; ===========================================================================
; Called during horizontal room advance. Scrolls camera right at 4 pixels
; per frame while simultaneously advancing player X by ~$D0/$100 per frame
; (net rightward movement). Renders columns each frame and yields.
; Loops until $FC wraps back to 0 (full screen scrolled).
; ---------------------------------------------------------------------------
fast_scroll_right:  jsr     clear_entity_table ; clear all enemies
fast_scroll_right_loop:  lda     camera_x_lo ; camera_x_lo += 4 (scroll 4 pixels/frame)
        clc                             ; prepare addition
        adc     #$04                    ; scroll 4 pixels per frame
        sta     camera_x_lo             ; store updated camera X
        bcc     fast_scroll_set_direction ; no carry → same screen
        inc     camera_screen           ; crossed screen boundary
fast_scroll_set_direction:  lda     #$01 ; $10 = 1 (scroll right)
        sta     $10                     ; direction = right
        jsr     render_scroll_column    ; render column for new position
        lda     camera_x_lo             ; update $25 = previous fine scroll
        sta     $25                     ; save as previous fine scroll
        lda     ent_x_sub               ; player X sub += $D0
        clc                             ; (24-bit add: sub + pixel + screen)
        adc     #$D0                    ; net effect: player slides right
        sta     ent_x_sub               ; update player X sub-pixel
        lda     ent_x_px                ; player X pixel += carry
        adc     #$00                    ; propagate carry
        sta     ent_x_px                ; store updated player X pixel
        lda     ent_x_scr               ; player X screen += carry
        adc     #$00                    ; propagate carry
        sta     ent_x_scr               ; store updated player X screen
        jsr     process_frame_yield_with_player ; render frame + yield to NMI
        lda     camera_x_lo             ; loop until camera_x_lo wraps to 0
        bne     fast_scroll_right_loop  ; loop until full screen scrolled
        lda     stage_id                ; re-select stage bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        jmp     load_room               ; load room layout + CHR/palette

; ===========================================================================
; vertical_scroll_animate — animate vertical room transition
; ===========================================================================
; Scrolls the screen vertically at 3 pixels per frame while moving the
; player Y position at ~2.75 pixels per frame. Uses $FA as vertical
; fine scroll position (0-$EF, wraps at $F0 like ent_y_px).
; $23 bit 2 selects direction: set=down ($04), clear=up ($08).
; Loops until $FA reaches 0 (full screen scrolled).
; ---------------------------------------------------------------------------

vertical_scroll_animate:  jsr     clear_entity_table ; clear all enemies
        lda     $23                     ; X = direction index:
        and     #$04                    ; $04 → X=1 (scrolling down)
        lsr     a                       ; $08 → X=0 (scrolling up)
        lsr     a                       ; convert to 0 or 1
        tax                             ; X = direction index
        lda     metatile_row_render_table,x ; $24 = initial row from table
        sta     $24                     ; set initial row pointer
vert_scroll_dir_check:  lda     $23     ; bit 2 set = scrolling down
        and     #$04                    ; zero → scrolling up
        beq     vert_scroll_up_player_sub ; branch to scroll-up handler

; --- scroll down: camera moves down, player moves up ---
        lda     scroll_y                ; scroll_y += 3 (fine Y scroll advances)
        clc                             ; prepare addition
        adc     #$03                    ; add 3 pixels
        sta     scroll_y                ; store updated fine Y
        cmp     #$F0                    ; wrap at $F0 (NES screen height)
        bcc     vert_scroll_down_player_sub ; no wrap → move player
        adc     #$0F                    ; skip $F0-$FF range (wrap)
        sta     scroll_y                ; store wrapped fine Y
vert_scroll_down_player_sub:  lda     ent_y_sub ; player Y sub -= $C0
        sec                             ; (player moves up ~2.75 px/frame)
        sbc     #$C0                    ; subtract $C0 sub-pixels
        sta     ent_y_sub               ; store updated Y sub-pixel
        lda     ent_y_px                ; player Y pixel -= 2 + borrow
        sbc     #$02                    ; subtract 2 pixels
        sta     ent_y_px                ; store updated Y pixel
        bcs     vert_scroll_render_row  ; no underflow → render
        sbc     #$0F                    ; wrap at screen boundary
        sta     ent_y_px                ; store wrapped Y pixel
        jmp     vert_scroll_render_row  ; render row

; --- scroll up: camera moves up, player moves down ---

vert_scroll_up_player_sub:  lda     scroll_y ; scroll_y -= 3 (fine Y scroll retreats)
        sec                             ; subtract 3 pixels
        sbc     #$03                    ; subtract 3 pixels
        sta     scroll_y                ; store updated fine Y
        bcs     vert_scroll_up_player_add ; no underflow → move player
        sbc     #$0F                    ; wrap below $00
        sta     scroll_y                ; store wrapped fine Y
vert_scroll_up_player_add:  lda     ent_y_sub ; player Y sub += $C0
        clc                             ; (player moves down ~2.75 px/frame)
        adc     #$C0                    ; add $C0 sub-pixels
        sta     ent_y_sub               ; update player Y sub-pixel
        lda     ent_y_px                ; player Y pixel += 2 + carry
        adc     #$02                    ; add 2 pixels + carry
        sta     ent_y_px                ; update player Y pixel
        cmp     #$F0                    ; wrap at $F0
        bcc     vert_scroll_render_row  ; within screen → render row
        adc     #$0F                    ; skip $F0-$FF range (NES wrap)
        sta     ent_y_px                ; store wrapped Y pixel
vert_scroll_render_row:  jsr     render_vert_row ; render row for new vertical position
        lda     scroll_y                ; $26 = previous scroll_y (for delta)
        sta     $26                     ; save previous scroll Y
        lda     $12                     ; preserve $12 across frame yield
        pha                             ; push $12 to stack
        jsr     process_frame_yield_with_player ; render frame + yield to NMI
        pla                             ; restore $12
        sta     $12                     ; direction-dependent flag
        lda     scroll_y                ; loop until scroll_y == 0 (full screen)
        beq     vert_scroll_reselect_bank ; done → restore bank
        jmp     vert_scroll_dir_check   ; continue scrolling

vert_scroll_reselect_bank:  lda     stage_id ; re-select stage bank
        sta     prg_bank                ; set PRG bank to stage
        jmp     select_PRG_banks        ; apply bank switch

; ===========================================================================
; render_vert_row — queue nametable row update during vertical scroll
; ===========================================================================
; Vertical equivalent of render_scroll_column. Computes whether an 8-pixel
; row boundary was crossed (|$FA - $26| + fine position), and if so,
; builds the PPU update buffer for the new row of metatiles.
;
; scroll_y = current vertical fine scroll, $26 = previous scroll_y
; $23 bit 2: direction (set=down, clear=up)
; $24 = nametable row pointer
; ---------------------------------------------------------------------------

render_vert_row:  lda     scroll_y      ; $03 = |scroll_y - $26
        sec                             ; = absolute vertical scroll delta
        sbc     $26                     ; subtract previous fine scroll
        bpl     vert_row_compute_delta  ; positive → no negate needed
        eor     #$FF                    ; negate if negative
        clc                             ; two's complement:
        adc     #$01                    ; A = |$FA - $26|
vert_row_compute_delta:  sta     $03    ; $03 = scroll delta (pixels)
        beq     vert_row_no_row_exit    ; zero → nothing to render
        lda     $23                     ; get fine position for boundary check
        and     #$04                    ; down: use $26 as-is
        beq     vert_row_up_direction   ; up: invert $26
        lda     $26                     ; use $26 for downward scroll
        jmp     vert_row_boundary_check ; skip inversion

vert_row_up_direction:  lda     $26     ; invert for upward scroll
        eor     #$FF                    ; bitwise complement for up direction
vert_row_boundary_check:  and     #$07  ; (fine pos & 7) + delta
        clc                             ; if result / 8 > 0: crossed boundary
        adc     $03                     ; → need to render new row
        lsr     a                       ; divide by 8:
        lsr     a                       ;   result > 0 means crossed tile boundary
        lsr     a                       ;   (8 pixels per tile row)
        bne     vert_row_advance_ptr    ; nonzero → render new row
vert_row_no_row_exit:  rts

; --- render row: advance row pointer and build PPU update buffer ---
; $24 = nametable row index (0-$1D, wraps), $23 bit 2 = direction (1=down, 0=up)
; PPU buffer: $0780 = header, $0783 = top half tiles, $07AF = bottom half tiles
; $07A3-$07A5 = attribute table PPU address + byte count
; $07A6-$07AD = 8 attribute bytes for this row
; $0640 = attribute cache (8 bytes per row block, updated incrementally)

vert_row_advance_ptr:  lda     $23      ; Y = direction flag (0=up, 1=down)
        and     #$04                    ; bit 2 → shift to bit 0
        lsr     a                       ; shift right twice to get 0 or 1
        lsr     a                       ; A = direction index
        tay                             ; Y = direction index
        lda     $24                     ; advance row pointer:
        clc                             ; down: $24 += $01 (from vert_row_advance_tbl)
        adc     vert_row_advance_tbl,y  ; up: $24 += $FF (i.e. $24 -= 1)
        sta     $24                     ; store advanced row pointer
        cmp     #$1E                    ; row < 30? (NES nametable = 30 rows)
        bcc     vert_row_nt_check       ; yes → skip wrap
        lda     vert_row_wrap_tbl,y     ; wrap: down wraps to $00, up wraps to $1D
        sta     $24                     ; store wrapped row pointer
vert_row_nt_check:  lda     $24         ; check if row is on correct nametable half
        and     #$01                    ; (even/odd parity vs direction)
        cmp     vert_row_parity_tbl,y   ; down expects $01, up expects $00
        beq     vert_row_build_fresh    ; match → need to render fresh row
        jmp     vert_row_shift_existing ; mismatch → just shift existing buffer

vert_row_build_fresh:  lda     stage_id ; switch to stage data bank
        sta     prg_bank                ; set PRG bank to stage data
        jsr     select_PRG_banks        ; apply bank switch
        ldy     $29                     ; set up metatile column pointer for screen $29
        jsr     metatile_column_ptr     ; set metatile column pointer
        lda     $24                     ; $28 = row-within-block offset × 2
        and     #$1C                    ; (rows come in groups of 4 within metatile)
        asl     a                       ; multiply by 2 for column stride
        sta     $28                     ; store as attribute cache index
        ora     #$C0                    ; PPU addr for attribute table row = $23C0 + offset
        sta     $07A4                   ; store low byte of attr PPU addr
        lda     #$23                    ; high byte = $23 (nametable 0 attr table)
        sta     $07A3                   ; store high byte of attr PPU addr
        lda     #$07                    ; 8 attribute bytes to write
        sta     $07A5                   ; store attribute byte count
        lda     #$00                    ; $03 = PPU buffer write offset (starts at 0)
        sta     $03                     ; initialize buffer offset
vert_row_tile_copy_loop:  ldy     $28   ; $11 = previous attribute byte from cache
        lda     $0640,y                 ; read cached attribute for this column
        sta     $11                     ; save previous attribute byte
        jsr     metatile_to_chr_tiles   ; decode metatile → CHR tiles + attr in $10
        ldy     $03                     ; Y = buffer write offset
        lda     $24                     ; X = row sub-position (0-3 within metatile)
        and     #$03                    ; mask to sub-row (0-3)
        tax                             ; X = sub-row index
        lda     vert_chr_top_offsets,x  ; $04 = CHR buffer offset for top row
        sta     $04                     ; store top-half CHR offset
        lda     vert_chr_bot_offsets,x  ; $05 = CHR buffer offset for bottom row
        sta     $05                     ; store bottom-half CHR offset
        lda     #$03                    ; $06 = 4 tiles per metatile column (count-1)
        sta     $06                     ; initialize tile copy counter
vert_row_chr_buffer_copy:  ldx     $04  ; top half: $06C0[top offset] → $0783 buffer
        lda     $06C0,x                 ; read top-half CHR tile
        sta     $0783,y                 ; store to top-half PPU buffer
        ldx     $05                     ; bottom half: $06C0[bot offset] → $07AF buffer
        lda     $06C0,x                 ; read bottom-half CHR tile
        sta     $07AF,y                 ; store to bottom-half PPU buffer
        inc     $04                     ; advance offsets
        inc     $05                     ; advance bottom offset
        iny                             ; advance buffer write position
        dec     $06                     ; 4 tiles done?
        bpl     vert_row_chr_buffer_copy ; loop until 4 tiles copied
        sty     $03                     ; save buffer position
        lda     $24                     ; X = row sub-position (0-3)
        and     #$03                    ; mask to sub-row (0-3)
        tax                             ; X = sub-row index
        lda     $10                     ; keep new attr bits (mask from vert_attr_keep_tbl)
        and     vert_attr_keep_tbl,x    ; mask new attribute per sub-row
        sta     $10                     ; store masked new attribute
        lda     $11                     ; merge old attr bits (mask from vert_attr_old_tbl)
        and     vert_attr_old_tbl,x     ; mask old attribute per sub-row
        ora     $10                     ; combine old and new attribute bits
        sta     $10                     ; $10 = merged attribute byte
        ldx     $28                     ; update attribute cache
        sta     $0640,x                 ; write merged attr to cache
        txa                             ; X = column index (0-7)
        and     #$07                    ; isolate column within attr row
        tax                             ; X = attribute byte index
        lda     $10                     ; store attribute to PPU buffer
        sta     $07A6,x                 ; write to attr PPU buffer
        inc     $28                     ; next column
        cpx     #$07                    ; done all 8?
        bne     vert_row_tile_copy_loop ; loop until all 8 columns done
        lda     $24                     ; compute nametable PPU address for this row
        pha                             ; save $24 for sub-row calc
        and     #$03                    ; Y = sub-row (0-3)
        tay                             ; Y = sub-row (0-3)
        pla                             ; restore $24
        lsr     a                       ; X = row/4 (metatile row index)
        lsr     a                       ; divide row by 4
        tax                             ; X = metatile row index
        lda     metatile_nt_offset_table,x ; $0780 = PPU addr high byte (from row table)
        sta     $0780                   ; set PPU addr high byte
        lda     metatile_burst_table,x  ; $0781 = PPU addr low byte | nametable offset
        ora     vert_row_nt_offsets,y   ; (sub-row adds $00/$20/$40/$60)
        sta     $0781                   ; store PPU addr low byte
        lda     #$1F                    ; $0782 = tile count ($1F = 32 tiles per row)
        sta     $0782                   ; store tile count
        lda     $23                     ; Y = direction (0=up, 1=down)
        and     #$04                    ; convert direction to index
        lsr     a                       ; shift bit 2 to bit 0
        lsr     a                       ; A = 0 (up) or 1 (down)
        tay                             ; Y = direction index
        ldx     vert_term_offset_tbl,y  ; terminator offset from vert_term_offset_tbl
        lda     #$FF                    ; $FF = end-of-buffer marker
        sta     $0780,x                 ; place at appropriate end
        sta     nametable_dirty         ; flag PPU update pending
        rts                             ; return to caller

vert_row_shift_existing:  ldy     #$1F  ; copy 32 bytes: $07AF → $0783
vert_row_copy_bottom:  lda     $07AF,y ; read bottom-half tile
        sta     $0783,y                 ; copy bottom-half → top-half buffer
        dey                             ; next byte (descending)
        bpl     vert_row_copy_bottom    ; loop all 32 bytes
        lda     $24                     ; update PPU addr low byte
        and     #$03                    ; with new sub-row nametable offset
        tax                             ; X = sub-row index
        lda     $0781                   ; keep high bit (nametable select)
        and     #$80                    ; preserve nametable select bit
        ora     vert_row_nt_offsets,x   ; merge sub-row offset ($00/$20/$40/$60)
        sta     $0781                   ; store updated PPU addr low byte
        lda     #$23                    ; attribute table high byte
        sta     $07A3                   ; store attr PPU addr high byte
        lda     $23                     ; set terminator based on direction
        and     #$04                    ; isolate direction bit
        lsr     a                       ; shift bit 2 to bit 0
        lsr     a                       ; A = 0 (up) or 1 (down)
        tay                             ; Y = direction index
        ldx     vert_term_shift_tbl,y   ; terminator offset (shift variant)
        lda     #$FF                    ; $FF = end-of-buffer marker
        sta     $0780,x                 ; write end marker
        sta     nametable_dirty         ; PPU update pending
        rts                             ; return to caller

; --- vertical row rendering lookup tables ---
; attribute mask tables: which bits to keep from new vs old attribute

vert_attr_keep_tbl:  .byte   $0F,$0F,$F0,$F0 ; new attr mask per sub-row (0-3)
vert_attr_old_tbl:  .byte   $F0,$F0,$0F,$0F ; old attr mask per sub-row (0-3)

; CHR buffer offsets: top-half and bottom-half starting positions
vert_chr_top_offsets:  .byte   $00,$04,$08,$0C ; top-half CHR offset per sub-row
vert_chr_bot_offsets:  .byte   $04,$00,$0C,$08 ; bottom-half CHR offset per sub-row

; nametable row offsets: $00, $20, $40, $60 per sub-row
vert_row_nt_offsets:  .byte   $00,$20,$40,$60

; row parity check: down expects $01, up expects $00
vert_row_parity_tbl:  .byte   $01,$00

; row wrap values: down wraps to $00, up wraps to $1D
vert_row_wrap_tbl:  .byte   $1D,$00

; row advance: down +1 ($01), up -1 ($FF)
vert_row_advance_tbl:  .byte   $FF,$01

; unknown small tables (used by boundary check / buffer termination)
metatile_row_render_table:  .byte   $00,$1D
vert_term_offset_tbl:  .byte   $23,$2E  ; terminator offset: up=$23, down=$2E
vert_term_shift_tbl:  .byte   $2E,$23

; ===========================================================================
; metatile_to_chr_tiles — convert 4 metatile quadrants to CHR tile IDs
; ===========================================================================
; Reads the current metatile definition via pointer at $00/$01 (set by
; metatile_chr_ptr). Each metatile has 4 quadrants (indexed 0-3).
; For each quadrant:
;   - Reads metatile sub-index from ($00),y
;   - Looks up 4 CHR tile IDs from tables $BB00-$BE00
;   - Stores them to $06C0 buffer in 2×2 layout
;   - Reads palette/attribute bits from $BF00
;   - Accumulates attribute byte in $10 (2 bits per quadrant)
;
; $E89D offset table: {$00,$02,$08,$0A} maps quadrants to $06C0 offsets.
; Output: $06C0 = 4×4 = 16 CHR tiles, $10 = attribute byte.
; ---------------------------------------------------------------------------
metatile_to_chr_tiles:  jsr     metatile_chr_ptr ; set $00/$01 pointer to metatile CHR data
metatile_to_chr_tiles_continue:  ldy     #$03 ; start with quadrant 3
        sty     $02                     ; $02 = quadrant counter (3→0)
        lda     #$00                    ; clear attribute accumulator
        sta     $10                     ; $10 = accumulated attribute bits
metatile_quadrant_check:  ldy     $02   ; Y = current quadrant
        ldx     metatile_attr_offset_table,y ; X = buffer offset for this quadrant
        lda     (temp_00),y             ; read metatile sub-index
        tay                             ; Y = sub-index for CHR lookup
        lda     $BB00,y                 ; top-left tile
        sta     $06C0,x                 ; store to tile buffer
        lda     $BC00,y                 ; top-right tile
        sta     $06C1,x                 ; store to tile buffer
        lda     $BD00,y                 ; bottom-left tile
        sta     $06C4,x                 ; store to buffer (+4 = next row)
        lda     $BE00,y                 ; bottom-right tile
        sta     $06C5,x                 ; store to buffer (+5)
        jsr     breakable_block_override ; Gemini Man: zero destroyed blocks
        lda     $BF00,y                 ; attribute: low 2 bits = palette
        and     #$03                    ; merge into $10
        ora     $10                     ; combine with existing attribute bits
        sta     $10                     ; accumulate attribute bits
        dec     $02                     ; next quadrant
        bmi     metatile_chr_exit       ; if all 4 done, return
        asl     $10                     ; shift attribute bits left 2
        asl     $10                     ; (make room for next quadrant)
        jmp     metatile_quadrant_check ; process next quadrant

metatile_chr_exit:  rts                 ; return with tiles in $06C0, attr in $10

; ===========================================================================
; breakable_block_override — zero CHR tiles for destroyed blocks (Gemini Man)
; ===========================================================================
; Called from metatile_to_chr_tiles for Gemini Man stages only ($22 = $02 or
; $09). Checks if the current metatile is a breakable block type (attribute
; upper nibble = $7x). If so, looks up the destroyed-block bitfield at
; $0110 — if the bit is set, the block has been destroyed and its 4 CHR
; tiles are zeroed out (invisible).
;
; Y = metatile sub-index, X = $06C0 buffer offset (preserved on exit)
; Uses bitmask_table ($EB82) for bit testing: $80,$40,$20,...,$01
; ---------------------------------------------------------------------------

breakable_block_override:  lda     stage_id ; current stage number
        cmp     #STAGE_GEMINI           ; Gemini Man?
        beq     metatile_attr_upper_nibble ; yes → check
        cmp     #STAGE_DOC_GEMINI       ; Doc Robot (Gemini Man stage)?
        bne     metatile_chr_exit       ; no → skip (shared RTS above)
metatile_attr_upper_nibble:  lda     $BF00,y ; attribute byte upper nibble
        and     #$F0                    ; isolate upper nibble
        cmp     #$70                    ; breakable block type ($7x)?
        bne     metatile_chr_exit       ; no → skip
        sty     $0F                     ; save Y (metatile sub-index)
        stx     $0E                     ; save X (buffer offset)
        lda     $29                     ; compute destroyed-block array index:
        and     #$01                    ; Y = ($29 & 1) << 5 | ($28 >> 1)
        asl     a                       ; ($29 bit 0 = screen page,
        asl     a                       ; $28 = metatile column position)
        asl     a                       ; shift left 5 total
        asl     a                       ; to form high bits of byte index
        asl     a                       ; A = ($29 & 1) << 5
        sta     $0D                     ; store partial byte index
        lda     $28                     ; load metatile column
        pha                             ; save $28
        lsr     a                       ; Y = byte index into $0110 array
        ora     $0D                     ; merge screen page and column bits
        tay                             ; Y = byte index in destroyed array
        pla                             ; X = bit index within byte:
        asl     a                       ; ($28 << 2) & $04 | $02 (quadrant)
        asl     a                       ; combines column parity with quadrant
        and     #$04                    ; isolate column parity bit
        ora     $02                     ; merge with quadrant counter
        tax                             ; X = bit select index
        lda     $0110,y                 ; test destroyed bit
        and     bitmask_table,x         ; via bitmask_table
        beq     metatile_restore_y_reg  ; not destroyed → skip
        ldx     $0E                     ; restore buffer offset
        lda     #$00                    ; zero all 4 CHR tiles (invisible)
        sta     $06C0,x                 ; clear top-left tile
        sta     $06C1,x                 ; clear top-right tile
        sta     $06C4,x                 ; clear bottom-left tile
        sta     $06C5,x                 ; clear bottom-right tile
metatile_restore_y_reg:  ldy     $0F    ; restore Y
        ldx     $0E                     ; restore X
        rts                             ; return to metatile_to_chr_tiles

; metatile_chr_ptr: sets $00/$01 pointer to 4-byte metatile CHR definition
; reads metatile index from column data at ($20),y
; each metatile = 4 CHR tile indices (2x2 pattern: TL, TR, BL, BR)
; metatile definitions at $B700 + (metatile_index * 4) in stage bank

metatile_chr_ptr:  jsr     ensure_stage_bank ; ensure stage bank selected
        lda     #$00                    ; initialize pointer high byte
        sta     $01                     ; $01 = 0 (high byte of pointer)
        ldy     $28                     ; Y = metatile row
        lda     ($20),y                 ; metatile index from column data
calc_chr_offset:  asl     a             ; multiply by 4
        rol     $01                     ; (4 CHR tiles per metatile)
        asl     a                       ; shift 2
        rol     $01                     ; rotate high byte
        sta     temp_00                 ; $00/$01 = $B700 + (index * 4)
        lda     $01                     ; pointer to CHR tile definition
        clc                             ; add base address $B700
        adc     #$B7                    ; high byte of CHR definition table
        sta     $01                     ; store pointer high byte
        rts                             ; $00/$01 → metatile CHR definition

metatile_attr_offset_table:  .byte   $00,$02,$08,$0A
metatile_burst_table:  .byte   $00,$80,$00,$80,$00,$80,$00,$80
metatile_nt_offset_table:  .byte   $20,$20,$21,$21,$22,$22,$23,$23

; metatile_column_ptr: sets $20/$21 pointer to metatile column data
; Y = screen page → reads column ID from $AA00,y in stage bank
; column data is at $AF00 + (column_ID * 64) — 64 bytes per column
metatile_column_ptr:  lda     $AA00,y   ; column ID from screen data
metatile_column_ptr_by_id:  pha         ; multiply column ID by 64:
        lda     #$00                    ; A << 6 → $00:A (16-bit result)
        sta     temp_00                 ; 6 shifts with 16-bit rotate
        pla                             ; restore column ID
        asl     a                       ; shift 1
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 2
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 3
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 4
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 5
        rol     temp_00                 ; 16-bit rotate
        asl     a                       ; shift 6
        rol     temp_00                 ; 16-bit rotate
        sta     $20                     ; $20/$21 = $AF00 + (column_ID × 64)
        lda     temp_00                 ; pointer to metatile column data
        clc                             ; in stage bank at $AF00+
        adc     #$AF                    ; add base address high byte
        sta     $21                     ; store pointer high byte
        rts                             ; $20/$21 → metatile column data

