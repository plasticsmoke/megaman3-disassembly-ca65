; =============================================================================
; MEGA MAN 3 (U) — BANK $02 — GEMINI MAN STAGE DATA + PAUSE MENU
; =============================================================================
; Mapped to $A000-$BFFF (Mapper 4 / MMC3). Dual-purpose bank:
;
;   CODE ($A000-$A567):
;     - Pause menu / weapon-select screen logic
;     - Weapon cursor navigation (D-pad input handling)
;     - E-Tank use and HP refill animation
;     - Weapon switch with palette swap and reappear
;     - Nametable upload for menu bars, lives/etank display
;     - OAM sprite placement for cursor and weapon icons
;     - IRQ scanline split for menu overlay
;
;   DATA ($A568-$BFFF):
;     - Pause menu lookup tables (nametable addrs, bar tiles, icon ptrs)
;     - Weapon palette table (LA641, 3 colors x 11 weapons)
;     - CHR bank assignment per weapon (LA634)
;     - OAM sprite data for Mega Man + weapon icons
;     - Gemini Man stage layout: screen map, metatiles, enemy spawns
;
;   Key variables:
;     $51       — menu scroll Y position (slides in/out)
;     $95       — general-purpose frame counter
;     $B4       — page offset (0 = page 1, 6 = page 2)
;     weapon_cursor ($A1) — selected slot (0-5, $80=E-Tank, $FF=none)
;     scroll_lock ($50)   — menu build phase counter (0-8)
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

; --- Fixed bank subroutine imports ---
LC5E9           := $C5E9    ; prepare_oam_buffer
LF835           := $F835    ; reset_sprite_anim
LF89A           := $F89A    ; submit_sound_ID
LFF21           := $FF21    ; task_yield (wait for NMI)
LFF3C           := $FF3C    ; update_CHR_banks

; =============================================================================
; PAUSE MENU CODE
; =============================================================================

.segment "BANK02"

; --- Entry point table ---
; $A000: JMP to refill-weapon entry (called after selecting weapon with ammo)
; $A003: Weapon-select entry (called when pausing during gameplay)

        jmp     code_A15B               ; entry: refill selected weapon's ammo

; ===========================================================================
; Weapon-select menu — main entry ($A003)
; ===========================================================================
; Called when the player pauses the game. Saves coroutine state ($EA-$ED),
; copies palette backup, then enters the menu loop.

        lda     $EA                     ; save coroutine state
        pha
        lda     $EB
        pha
        lda     $EC
        pha
        lda     $ED
        pha
        ldx     #$03
code_A011:  lda     $0620,x             ; copy saved palette → active palette
        sta     $0600,x
        dex
        bpl     code_A011
code_A01A:  jsr     code_A18E           ; build menu screen (slide in)
; --- Menu input loop ---
code_A01D:  lda     joy1_press
        and     #$D0                    ; check Start, A, B buttons
        bne     code_A02C               ; button pressed → process selection
code_A023:  jsr     code_A398           ; handle D-pad cursor movement
        jsr     code_A2EA               ; wait one frame
        jmp     code_A01D               ; loop

; --- Process button press ---
code_A02C:  and     #$40                ; B button?
        bne     code_A038               ; yes → close menu (no selection)
        lda     weapon_cursor
        bpl     code_A0A0               ; cursor on weapon slot → select weapon
        cmp     #$FF                    ; cursor on E-Tank marker?
        beq     code_A05C               ; yes → use E-Tank
; --- Close menu: slide out ---
code_A038:  lda     $51
        cmp     #$E8                    ; fully scrolled off-screen?
        beq     code_A04B
        lda     $51
        clc
        adc     #$04                    ; scroll menu up 4px per frame
        sta     $51
        jsr     code_A2EA
        jmp     code_A038

; --- Toggle page and re-enter ---
code_A04B:  lda     $B4
        eor     #$06                    ; flip between page 0 and page 6
        sta     $B4
        lda     #$00
        sta     scroll_lock
        lda     #$80                    ; set cursor to E-Tank position
        sta     weapon_cursor
        jmp     code_A01A              ; rebuild menu on other page

; ===========================================================================
; Use E-Tank — refill player HP
; ===========================================================================
; Only works on page 1 ($B4=0). Decrements E-Tank count (BCD), then
; fills player HP one unit every 4 frames until full ($9C = 28 bars).

code_A05C:  lda     $B4
        bne     code_A09D               ; page 2 → skip (no E-Tanks there)
        sta     $95                     ; reset frame counter
        lda     etanks
        beq     code_A09D               ; no E-Tanks → skip
        sec
        sbc     #$01                    ; decrement E-Tank count (BCD)
        sta     etanks
        and     #$0F
        cmp     #$0F                    ; BCD borrow? (e.g. $10 → $0F)
        bne     code_A078
        lda     etanks
        sec
        sbc     #$06                    ; fix BCD: $x9 after borrow
        sta     etanks
; --- HP refill loop ---
code_A078:  lda     #$00
        sta     weapon_cursor           ; temporarily set cursor to slot 0
        lda     player_hp
        cmp     #$9C                    ; HP full? (28 bars)
        beq     code_A09D               ; yes → done
        lda     $95
        and     #$03                    ; every 4th frame:
        bne     code_A097
        inc     player_hp               ;   add 1 HP
        lda     weapon_cursor
        pha
        ldx     #$00
        stx     weapon_cursor
        jsr     code_A50D               ;   update HP bar display
        pla
        sta     weapon_cursor
code_A097:  jsr     code_A2EA           ; wait one frame
        jmp     code_A078               ; loop until full

code_A09D:  jmp     code_A023           ; return to menu input loop

; ===========================================================================
; Select weapon — apply weapon change and exit menu
; ===========================================================================
; Computes absolute weapon index from cursor + page offset, loads the
; weapon's palette, triggers the "reappear" player state, and returns
; to gameplay.

code_A0A0:  lda     weapon_cursor
        clc
        adc     $B4                     ; absolute weapon index = cursor + page
        cmp     current_weapon
        beq     code_A0AD               ; same weapon → skip charge reset
        ldy     #$00
        sty     $B5                     ; reset charge level
code_A0AD:  sta     current_weapon
        lda     current_weapon
        bne     code_A0B7
        sta     $B1                     ; weapon 0 (P) → clear weapon flag
        beq     code_A0E6
code_A0B7:  ora     #$80
        sta     $B1                     ; set "has weapon" flag
        lda     $B4
        beq     code_A0E6               ; page 1 → skip Rush animation
        lda     weapon_cursor
        and     #$01                    ; odd slot on page 2?
        beq     code_A0E6
        ; --- Rush adapter selected: play transformation ---
        lda     weapon_cursor
        lsr     a
        clc
        adc     #$06                    ; remap cursor for Rush slot
        sta     weapon_cursor
        lda     #$00
        sta     $95
code_A0D1:  jsr     code_A2EA           ; wait frames for animation
        lda     $95
        cmp     #$0F                    ; 15 frames
        bne     code_A0D1
        lda     #$1E                    ; then wait 30 more frames
code_A0DC:  pha
        jsr     LFF21                   ; yield one frame
        pla
        sec
        sbc     #$01
        bne     code_A0DC
; --- Slide menu off-screen ---
code_A0E6:  lda     $51
        cmp     #$E8                    ; fully off?
        beq     code_A0FB
        lda     $51
        clc
        adc     #$04                    ; scroll up 4px/frame
        sta     $51
        jsr     code_A2EA
        dec     $95
        jmp     code_A0E6

; --- Finalize weapon selection and return to gameplay ---
code_A0FB:  lda     weapon_cursor
        cmp     #$06                    ; page 2 slot?
        bcc     code_A108
        sbc     #$06                    ; convert back to 0-based
        asl     a
        ora     #$01                    ; mark as odd (Rush item)
        sta     weapon_cursor
code_A108:  pla                         ; restore coroutine state
        sta     $ED
        pla
        sta     $EC
        pla
        sta     $EB
        pla
        sta     $EA
        ldy     current_weapon
        lda     LA634,y                 ; CHR bank for this weapon
        sta     $EB
        lda     #$00
        sta     scroll_lock
        ldy     game_mode
        cpy     #$01                    ; in-game pause mode?
        bne     code_A127
        sta     game_mode               ; clear game mode
code_A127:  ldx     #$0F
code_A129:  lda     $0630,x             ; restore saved palette
        sta     $0610,x
        dex
        bne     code_A129
        lda     current_weapon          ; load weapon palette
        asl     a
        asl     a                       ; weapon * 4 = palette offset
        tay
        ldx     #$00
code_A139:  lda     LA641,y             ; copy 3 palette colors
        sta     $0611,x                 ; to active sprite palette
        sta     $0631,x                 ; and saved sprite palette
        iny
        inx
        cpx     #$03
        bne     code_A139
        stx     palette_dirty           ; flag palette for upload
        lda     #PSTATE_REAPPEAR        ; trigger player reappear animation
        sta     player_state
        ldx     #$00
        lda     #$13                    ; Mega Man sprite object
        jsr     LF835                   ; reset sprite animation
        inc     ent_anim_state
        jmp     LFF3C                   ; update CHR banks and return

; ===========================================================================
; Refill weapon ammo entry ($A000 → JMP here)
; ===========================================================================
; Called after collecting a weapon energy refill pickup. Builds the menu
; overlay, then fills the selected weapon's ammo bar one unit every 4
; frames until full ($9C = 28 bars). Then slides menu out.

code_A15B:  lda     $EA                 ; save coroutine state
        pha
        lda     $EB
        pha
        lda     $EC
        pha
        lda     $ED
        pha
        jsr     code_A18E               ; build menu screen
        lda     #$00
        sta     $95                     ; reset frame counter
; --- Ammo refill loop ---
code_A16E:  lda     $95
        and     #$03                    ; every 4th frame:
        bne     code_A185
        lda     weapon_cursor
        clc
        adc     $B4                     ; absolute weapon index
        tax
        lda     player_hp,x             ; weapon ammo (player_hp+N)
        cmp     #$9C                    ; full?
        beq     code_A18B               ; yes → done
        inc     player_hp,x             ; add 1 ammo unit
        jsr     code_A50D               ; update ammo bar display
code_A185:  jsr     code_A2EA           ; wait one frame
        jmp     code_A16E               ; loop

code_A18B:  jmp     code_A0E6           ; slide menu out

; ===========================================================================
; Build menu screen (code_A18E)
; ===========================================================================
; Plays the pause SFX, sets up OAM, then iterates through 8 scroll_lock
; phases to upload nametable rows for the weapon menu. Each phase writes
; one row of PPU nametable data to the $0780 transfer buffer.
; After all rows are built, loads the menu palette and slides the
; overlay into view by decreasing $51 from $E8 to $B0.

code_A18E:  lda     #$1A                ; pause menu open SFX
        jsr     LF89A
        lda     scroll_lock
        pha
        inc     scroll_lock             ; advance build phase
        lda     #$04
        sta     oam_ptr                 ; reset OAM write pointer
        jsr     LC5E9                   ; prepare OAM buffer
        pla
        sta     scroll_lock             ; restore phase counter
        lda     game_mode
        bne     code_A1AA
        lda     #$01
        sta     game_mode               ; set game mode to "paused"
; --- Upload nametable rows phase by phase ---
code_A1AA:  ldy     scroll_lock
        cpy     #$08                    ; all 8 phases done?
        beq     code_A217               ; yes → load palette
        ldx     LA575,y                 ; get data offset for this phase
        ldy     #$00
; --- Decode compressed nametable row data ---
code_A1B5:  lda     $52
        and     #$0C                    ; base nametable bits
        ora     LA57D,x                 ; merge PPU control byte
        sta     $0780,y                 ; PPU address high byte
        bmi     code_A1F7               ; $FF terminator → done
        lda     LA57E,x
        sta     $0781,y                 ; PPU address low byte
        lda     LA57F,x
        sta     $0782,y                 ; tile count
        sta     $00
code_A1CF:  lda     #$00
        sta     $01                     ; RLE repeat counter
        lda     LA580,x
        bpl     code_A1E0               ; not RLE → single tile
        and     #$7F                    ; extract repeat count
        sta     $01
        inx
        lda     LA580,x                 ; tile to repeat
code_A1E0:  sta     $0783,y             ; store tile
        iny
        dec     $00
        dec     $01
        bpl     code_A1E0              ; repeat if RLE count > 0
        inx
        lda     $00
        bpl     code_A1CF              ; more tiles in this row
        inx                             ; skip 3 bytes (next row header)
        inx
        inx
        iny
        iny
        iny
        bne     code_A1B5              ; next nametable row
; --- Post-processing for this phase ---
code_A1F7:  jsr     code_A251           ; draw weapon bars / lives / etanks
        lda     game_mode
        cmp     #$0B                    ; password screen mode?
        bne     code_A213
        lda     #$20                    ; adjust PPU base for password screen
        sta     $0780
        lda     $0781
        sec
        sbc     #$C0
        sta     $0781
        lda     #$FF
        sta     $07A3                   ; terminate buffer early
code_A213:  inc     nametable_dirty     ; mark for NMI upload
        inc     scroll_lock             ; advance to next phase
; --- Load menu palette and CHR banks ---
code_A217:  lda     #$74
        cmp     $EA                     ; already set up?
        beq     code_A23D
        sta     $EA                     ; set CHR bank config
        lda     #$0B
        sta     $EB
        lda     #$12
        sta     $EC
        lda     #$1C
        sta     $ED
        ldx     #$0F
code_A22D:  lda     LA624,x             ; load menu palette
        sta     $0610,x
        dex
        bne     code_A22D
        lda     #$FF
        sta     palette_dirty           ; flag for NMI upload
        jsr     LFF3C                   ; update CHR banks
; --- Slide menu into view ---
code_A23D:  jsr     code_A2EA           ; wait one frame
        lda     $51
        cmp     #$B0                    ; target Y position reached?
        beq     code_A250
        lda     $51
        sec
        sbc     #$04                    ; scroll down 4px/frame
        sta     $51
        jmp     code_A1AA              ; continue building + sliding

code_A250:  rts

; ===========================================================================
; Draw weapon energy bars, lives, and E-Tank counts (code_A251)
; ===========================================================================
; Appends weapon energy bar tile data to the $0780 nametable buffer.
; Called during odd-numbered scroll_lock phases (1, 3, 5) to fill in
; the energy bar columns for two weapons at a time.
; Also writes lives count (phase 2) and E-Tank count (phase 5).

code_A251:  lda     scroll_lock
        cmp     #$06
        bcs     code_A2A9              ; phases 6-7 → skip bars
        and     #$01
        beq     code_A2A9              ; even phases → skip (row data only)
        lda     scroll_lock
        clc
        adc     $B4                     ; offset by current page
        and     #$FE                    ; round to even = weapon pair index
        sta     $00
        ldx     #$08                    ; buffer write offset
; --- Draw one weapon's energy bar ---
code_A266:  lda     #$07                ; 7 tile segments per bar
        sta     $01
        ldy     $00                     ; weapon index
        lda     player_hp,y            ; weapon HP/ammo (bit 7 = owned)
        bpl     code_A29D              ; not owned → skip
        and     #$7F                    ; extract ammo value (0-28)
        sta     $02
        lda     LA5D8,y                ; PPU address high for this bar
        sta     $0780,x
        lda     LA5E4,y                ; PPU address low for this bar
        sta     $0781,x
        inx
        inx
; --- Fill bar segments (each segment = 4 ammo units) ---
code_A283:  ldy     #$04                ; full segment
        lda     $02
        sec
        sbc     #$04
        bcs     code_A290
        ldy     $02                     ; partial segment
        lda     #$00
code_A290:  sta     $02
        lda     LA5F0,y                ; bar tile (empty/1/2/3/full)
        sta     $0780,x
        inx
        dec     $01
        bne     code_A283
; --- Second weapon in pair ---
code_A29D:  lda     $00
        and     #$01
        bne     code_A2A9              ; already did second → done
        ldx     #$13                    ; second bar buffer offset
        inc     $00                     ; next weapon index
        bne     code_A266
; --- Display lives count (phase 2) or E-Tank count (phase 5) ---
code_A2A9:  lda     scroll_lock
        cmp     #$02                    ; phase 2 → lives count
        bne     code_A2C3
        lda     $B4
        bne     code_A2DE              ; page 2 → blank the digits
        lda     lives                   ; BCD lives count
        and     #$0F                    ; ones digit
        sta     $07A1
        lda     lives
        lsr     a                       ; tens digit
        lsr     a
        lsr     a
        lsr     a
        sta     $07A0
code_A2C3:  lda     scroll_lock
        cmp     #$05                    ; phase 5 → E-Tank count
        bne     code_A2E9
        lda     $B4
        bne     code_A2DE              ; page 2 → blank
        lda     etanks                  ; BCD E-Tank count
        and     #$0F                    ; ones digit
        sta     $07A1
        lda     etanks
        lsr     a                       ; tens digit
        lsr     a
        lsr     a
        lsr     a
        sta     $07A0
        rts

; --- Blank the counter digits (page 2 has no lives/etank display) ---
code_A2DE:  lda     #$25                ; blank tile
        sta     $079F
        sta     $07A0
        sta     $07A1
code_A2E9:  rts

; ===========================================================================
; Wait one frame and update display (code_A2EA)
; ===========================================================================
; Sets the IRQ scanline for the menu/gameplay split, prepares OAM,
; draws sprites, then yields to NMI and increments the frame counter.

code_A2EA:  lda     game_mode
        cmp     #$01                    ; in-game pause?
        beq     code_A2F6
        lda     $5E                     ; gameplay scanline split
        cmp     $51                     ; compare with menu Y
        bcc     code_A2F8               ; use whichever is lower
code_A2F6:  lda     $51                 ; menu scroll Y position
code_A2F8:  sta     irq_scanline        ; set IRQ scanline
        jsr     LC5E9                   ; prepare OAM buffer
        jsr     code_A30C               ; draw menu sprites
        lda     #$00
        sta     nmi_skip                ; allow NMI processing
        jsr     LFF21                   ; yield (wait for NMI)
        inc     nmi_skip                ; block NMI again
        inc     $95                     ; increment frame counter
        rts

; ===========================================================================
; Draw menu OAM sprites (code_A30C)
; ===========================================================================
; Places Mega Man portrait sprites (page 1 only) and the selected weapon
; icon sprites into the OAM buffer at $0200.
; The E-Tank cursor ($FF) blinks by toggling between full and partial
; sprite sets every 8 frames.

code_A30C:  lda     $B4
        bne     code_A347               ; page 2 → skip portrait
        ; --- Draw Mega Man portrait (8 sprites) ---
        ldx     #$1C                    ; start at OAM slot 7 (descending)
        lda     weapon_cursor
        cmp     #$FF                    ; E-Tank cursor?
        bne     code_A320
        lda     $95
        and     #$08                    ; blink every 8 frames
        beq     code_A320
        ldx     #$0C                    ; show fewer sprites when blinking
code_A320:  lda     LA670,x             ; Y offset (relative to menu)
        clc
        adc     $51                     ; add menu scroll position
        bcs     code_A341               ; off-screen → skip
        cmp     #$F0
        bcs     code_A341               ; below visible area → skip
        sta     $0200,x                 ; OAM Y
        lda     LA671,x
        sta     $0201,x                 ; OAM tile
        lda     LA672,x
        sta     $0202,x                 ; OAM attributes
        lda     LA673,x
        sta     $0203,x                 ; OAM X
code_A341:  dex
        dex
        dex
        dex
        bpl     code_A320
; --- Draw selected weapon icon ---
code_A347:  lda     weapon_cursor
        bpl     code_A34C               ; weapon slot selected?
code_A34B:  rts                         ; no (E-Tank or unselected) → done

code_A34C:  clc
        adc     $B4                     ; absolute weapon index
        beq     code_A34B               ; weapon 0 (P) → no icon
        asl     a
        tax
        lda     $95
        and     #$08                    ; animation frame toggle
        beq     code_A35A
        inx                             ; alternate sprite frame
code_A35A:  lda     LA690,x             ; sprite data pointer low
        sta     $00
        lda     LA6AE,x                ; sprite data pointer high
        sta     $01
        ldx     #$20                    ; OAM slot 8 onwards
        ldy     #$00
; --- Copy weapon icon sprite data to OAM ---
code_A368:  lda     ($00),y             ; Y position
        bmi     code_A397               ; $80+ terminator → done
        clc
        adc     $51                     ; offset by menu position
        bcs     code_A391               ; off-screen → skip sprite
        cmp     #$F0
        bcs     code_A391
        sta     $0200,x                 ; OAM Y
        iny
        lda     ($00),y
        sta     $0201,x                 ; OAM tile
        iny
        lda     ($00),y
        sta     $0202,x                 ; OAM attributes
        iny
        lda     ($00),y
        sta     $0203,x                 ; OAM X
        iny
code_A38B:  inx                         ; next OAM slot (+4)
        inx
        inx
        inx
        bne     code_A368
code_A391:  iny                         ; skip this sprite's remaining bytes
        iny
        iny
        iny
        bne     code_A38B
code_A397:  rts

; ===========================================================================
; D-pad cursor navigation (code_A398)
; ===========================================================================
; Handles weapon cursor movement on the pause menu. The menu is laid out
; as a 2x3 grid (6 slots per page). Cursor wraps at edges.
; Auto-repeat: $1F counts up from $F0; movement triggers when it wraps.
;
;   Slot layout (page 1):      Slot layout (page 2):
;     0  1                        6  7
;     2  3                        8  9
;     4  5                       10 11

code_A398:  lda     weapon_cursor
        pha                             ; save old cursor for SFX compare
        lda     joy1_held
        and     #$0F                    ; any D-pad held?
        bne     code_A3A8
        lda     #$F0                    ; no → reset auto-repeat timer
        sta     $1F
code_A3A5:  jmp     code_A477           ; skip to finalize

; --- Auto-repeat delay ---
code_A3A8:  lda     $1F
        clc
        adc     #$10                    ; increment repeat counter
        sta     $1F
        bne     code_A3A5              ; not ready yet → skip
; --- Check Right ---
        lda     joy1_held
        and     #BTN_RIGHT
        beq     code_A3EC
; --- Move Right ---
code_A3B7:  lda     weapon_cursor
        bpl     code_A3C5
        cmp     #$FF                    ; at E-Tank? can't go right
        beq     code_A3A5
        lda     #$00                    ; from $80 (E-Tank) → slot 0
        sta     weapon_cursor
        beq     code_A3DE
code_A3C5:  lda     $B4
        beq     code_A3CF               ; page 1 → normal increment
        lda     weapon_cursor
        and     #$01                    ; page 2: odd slots are rightmost
        bne     code_A3A5              ; already at right edge → stop
code_A3CF:  inc     weapon_cursor
        lda     weapon_cursor
        and     #$01
        bne     code_A3DE              ; now on odd slot → validate
        lda     #$FF                    ; wrapped past right → E-Tank marker
        sta     weapon_cursor
        jmp     code_A477

code_A3DE:  lda     weapon_cursor       ; validate: weapon must be owned
        clc
        adc     $B4
        tay
        lda     player_hp,y            ; bit 7 set = owned
        bpl     code_A3B7              ; not owned → try next
        jmp     code_A477

; --- Check Left ---
code_A3EC:  lda     joy1_held
        and     #BTN_LEFT
        beq     code_A41D
; --- Move Left ---
code_A3F2:  lda     weapon_cursor
        bpl     code_A400
        cmp     #$80                    ; at $80 (E-Tank left)? can't go left
        beq     code_A3A5
        lda     #$01                    ; from $FF → slot 1
        sta     weapon_cursor
        bne     code_A40F
code_A400:  dec     weapon_cursor
        lda     weapon_cursor
        and     #$01                    ; went to even slot → valid
        beq     code_A40F
        lda     #$80                    ; wrapped past left → E-Tank marker
        sta     weapon_cursor
        jmp     code_A3A5

code_A40F:  lda     weapon_cursor       ; validate: weapon must be owned
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A3F2              ; not owned → try next
        jmp     code_A477

; --- Check Up/Down (only when cursor is on a weapon slot) ---
code_A41D:  lda     weapon_cursor
        bmi     code_A477               ; E-Tank cursor → no up/down
        lda     joy1_held
        and     #BTN_UP
        beq     code_A44C
; --- Move Up (subtract 2 slots, wrap at edges) ---
code_A427:  lda     weapon_cursor
        bne     code_A431
        lda     #$05                    ; slot 0 → wrap to slot 5
        sta     weapon_cursor
        bne     code_A46C
code_A431:  cmp     #$01
        bne     code_A43B
        lda     #$04                    ; slot 1 → wrap to slot 4
        sta     weapon_cursor
        bne     code_A43F
code_A43B:  dec     weapon_cursor       ; move up 2 slots
        dec     weapon_cursor
code_A43F:  lda     weapon_cursor       ; validate
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A427              ; not owned → try next
        bmi     code_A477
; --- Check Down ---
code_A44C:  lda     joy1_held
        and     #BTN_DOWN
        beq     code_A477
; --- Move Down (add 2 slots, wrap at edges) ---
code_A452:  lda     weapon_cursor
        cmp     #$04
        bne     code_A45E
        lda     #$01                    ; slot 4 → wrap to slot 1
        sta     weapon_cursor
        bne     code_A46C
code_A45E:  cmp     #$05
        bne     code_A468
        lda     #$00                    ; slot 5 → wrap to slot 0
        sta     weapon_cursor
        beq     code_A46C
code_A468:  inc     weapon_cursor       ; move down 2 slots
        inc     weapon_cursor
code_A46C:  lda     weapon_cursor       ; validate
        clc
        adc     $B4
        tay
        lda     player_hp,y
        bpl     code_A452              ; not owned → try next
; --- Finalize: play cursor move SFX if cursor changed ---
code_A477:  pla                         ; old cursor position
        cmp     weapon_cursor
        beq     code_A481              ; unchanged → no SFX
        lda     #$1B                    ; cursor move SFX
        jsr     LF89A
; ===========================================================================
; Build weapon name/icon nametable buffer (code_A481)
; ===========================================================================
; Writes the weapon name row data to the $0780 PPU transfer buffer.
; Each weapon slot gets a 5-byte entry (PPU addr high, low, 2 name tiles,
; terminator). Also highlights the selected slot by blanking its name
; tiles every 8 frames (blinking cursor effect).

code_A481:  ldx     #$00
; --- Copy weapon name entries from table ---
code_A483:  lda     $52
        and     #$2C                    ; nametable base bits
        ora     LA5F5,x                ; merge PPU address high
        sta     $0780,x
        cmp     #$FF                    ; end of table?
        beq     code_A4A0
        inx
        ldy     #$04                    ; copy 4 more bytes per entry
code_A494:  lda     LA5F5,x
        sta     $0780,x
        inx
        dey
        bne     code_A494
        beq     code_A483
; --- Overlay weapon abbreviation labels on each slot ---
code_A4A0:  lda     #$05                ; 6 slots (0-5)
        sta     $00
        ldx     $B4                     ; start at page offset
        ldy     #$03                    ; buffer offset for first slot
code_A4A8:  lda     player_hp,x         ; weapon owned? (bit 7)
        bpl     code_A4B8              ; not owned → skip
        lda     LA5D8,x                ; weapon name tile 1
        sta     $0780,y
        lda     LA5E4,x                ; weapon name tile 2
        sta     $0781,y
code_A4B8:  inx
        tya
        clc
        adc     #$05                    ; next slot (+5 bytes in buffer)
        tay
        dec     $00
        bpl     code_A4A8
; --- Blink selected cursor (blank name tiles every 8 frames) ---
        lda     $95
        and     #$08
        beq     code_A4EB              ; not in blink phase → skip
        ldx     weapon_cursor
        bpl     code_A4E0              ; on weapon slot → blank that slot
        cpx     #$FF                    ; on E-Tank ($FF)?
        beq     code_A4EB              ; yes → don't blink (different display)
        lda     #$25                    ; $80 cursor: blank E-Tank area tiles
        sta     $07A1
        sta     $07A2
        sta     $07A6
        sta     $07A7
        bne     code_A4EB
code_A4E0:  ldy     LA61E,x             ; get buffer offset for this slot
        lda     #$25                    ; blank tile
        sta     $0780,y                ; blank the name tiles (blink)
        sta     $0781,y
; --- Password screen adjustment ---
code_A4EB:  lda     game_mode
        cmp     #$0B                    ; password screen?
        bne     code_A50A
        ldy     #$00
code_A4F3:  lda     #$20                ; remap to nametable $2000
        sta     $0780,y
        lda     $0781,y
        sec
        sbc     #$C0                    ; adjust PPU address
        sta     $0781,y
        iny
        iny
        iny
        iny
        iny
        cpy     #$28                    ; 8 entries * 5 bytes
        bne     code_A4F3
code_A50A:  inc     nametable_dirty     ; flag for NMI upload
        rts

; ===========================================================================
; Update single weapon energy bar (code_A50D)
; ===========================================================================
; Called during HP/ammo refill animations to update one weapon's energy
; bar in the nametable buffer. Plays the refill tick SFX.

code_A50D:  lda     #$1C                ; HP refill tick SFX
        jsr     LF89A
        lda     weapon_cursor
        asl     a                       ; cursor * 2 = index into addr table
        tay
        lda     $52
        and     #$0C                    ; nametable base
        ora     LA569,y                ; PPU address high for this bar
        sta     $0780
        lda     LA56A,y                ; PPU address low
        sta     $0781
        lda     #$06                    ; 6 data bytes (header says 7 segments)
        sta     $0782
        lda     player_hp,x            ; current ammo value
        and     #$1F                    ; mask to 0-28
        sta     $00
        ldy     #$00
; --- Build 7 bar segments ---
code_A533:  ldx     #$04                ; assume full segment
        lda     $00
        sec
        sbc     #$04                    ; subtract 4 per segment
        bcs     code_A540
        ldx     $00                     ; partial segment
        lda     #$00
code_A540:  sta     $00
        lda     LA5F0,x                ; bar fill tile
        sta     $0783,y
        iny
        cpy     #$07                    ; 7 segments
        bne     code_A533
        lda     game_mode
        cmp     #$0B                    ; password screen?
        bne     code_A561
        lda     #$20                    ; remap to nametable $2000
        sta     $0780
        lda     $0781
        sec
        sbc     #$C0
        sta     $0781
code_A561:  lda     #$FF                ; terminate buffer
        sta     $078A
        .byte   $85,$19,$60             ; STA nametable_dirty / RTS (hand-assembled)
; =============================================================================
; PAUSE MENU DATA TABLES
; =============================================================================

; --- PPU addresses for each weapon's energy bar column ---
; 6 entries (2 bytes each): high, low. Used by code_A50D.
LA569:  .byte   $22
LA56A:  .byte   $E7,$22,$F2,$23,$27,$23,$32,$23
        .byte   $67,$23,$72

; --- Nametable data offset table (one per build phase 0-7) ---
LA575:  .byte   $00,$0D,$15,$25,$2D,$38,$46,$53

; --- Compressed nametable row data for menu layout ---
; Format: PPU_high, PPU_low, tile_count, tiles...
; Tiles with bit 7 set = RLE: (count | $80), tile_value
LA57D:  .byte   $22
LA57E:  .byte   $C0
LA57F:  .byte   $1F
LA580:  .byte   $30,$9D,$31,$32,$23,$E8,$07,$87
        .byte   $00,$FF,$22,$E0,$1F,$33,$9D,$25
        .byte   $34,$FF,$23,$00,$1F,$33,$9A,$25
        .byte   $2B,$00,$00,$34,$23,$F0,$07,$87
        .byte   $00,$FF,$23,$20,$1F,$33,$9D,$25
        .byte   $34,$FF,$23,$40,$1F,$33,$25,$38
        .byte   $39,$9A,$25,$34,$FF,$23,$60,$1F
        .byte   $33,$25,$3A,$3B,$97,$25,$2B,$00
        .byte   $00,$34,$FF,$23,$80,$1F,$33,$9D
        .byte   $25,$34,$23,$F8,$07,$87,$00,$FF
        .byte   $23,$A0,$1F,$35,$9D,$36,$37,$FF
; --- Weapon name tile 1 (PPU addr high) per weapon index ---
LA5D8:  .byte   $19,$10,$17,$11,$16,$1D,$1C,$1B
        .byte   $1C,$1B,$1C,$1B
; --- Weapon name tile 2 (PPU addr low) per weapon index ---
LA5E4:  .byte   $25,$0E,$0E,$0A,$0A,$18,$17,$0C
        .byte   $19,$16,$11,$13
; --- Energy bar fill tiles (0=empty, 1-3=partial, 4=full) ---
LA5F0:  .byte   $24,$2F,$2E,$2D,$2C
; --- Weapon slot nametable row entries (5 bytes each, $FF terminated) ---
; Each: PPU_high, PPU_low, count, tile1, tile2
LA5F5:  .byte   $22,$E5,$01,$25,$25,$22,$F0,$01
        .byte   $25,$25,$23,$25,$01,$25,$25,$23
        .byte   $30,$01,$25,$25,$23,$65,$01,$25
        .byte   $25,$23,$70,$01,$25,$25,$23,$42
        .byte   $01,$38,$39,$23,$62,$01,$3A,$3B
        .byte   $FF
; --- Buffer offset for each cursor slot (used for blink blanking) ---
LA61E:  .byte   $03,$08,$0D,$12,$17,$1C
; --- Menu palette data (4 sub-palettes, 4 bytes each) ---
LA624:  .byte   $0F,$0F,$30,$15,$0F,$0F,$30,$37
        .byte   $0F,$0F,$3C,$11,$0F,$0F,$30,$19
; --- CHR bank assignment per weapon (indexed by current_weapon) ---
LA634:  .byte   $01,$02,$07,$03,$01,$07,$01,$04
        .byte   $06,$02,$06,$03,$0F
; --- Weapon palette table (4 bytes per weapon, 3 used + 1 padding) ---
; Indexed by current_weapon * 4. Colors applied to sprite palette 1.
LA641:  .byte   $0F,$2C,$11,$0F,$0F,$30,$21,$0F
        .byte   $0F,$30,$17,$0F,$0F,$10,$01,$0F
        .byte   $0F,$10,$16,$0F,$0F,$36,$00,$0F
        .byte   $0F,$30,$19,$0F,$0F,$30,$15,$0F
        .byte   $0F,$30,$26,$0F,$0F,$30,$15,$0F
        .byte   $0F,$34,$14,$0F,$0F,$30,$15
; --- Mega Man portrait OAM data (8 sprites, Y/tile/attr/X interleaved) ---
LA670:  .byte   $08                     ; sprite 0 Y offset
LA671:  .byte   $F3                     ; sprite 0 tile
LA672:  .byte   $02                     ; sprite 0 attributes
LA673:  .byte   $D0,$08,$F3,$42,$D8,$10,$F4,$01
        .byte   $D0,$10,$F4,$41,$D8,$20,$F1,$02
        .byte   $D0,$20,$F2,$02,$D8,$28,$F1,$82
        .byte   $D0,$28,$F2,$82,$D8
; --- Weapon icon sprite data pointer table (low bytes) ---
; Two entries per weapon (frame 0, frame 1). Indexed by weapon*2.
LA690:  .byte   $CC,$CC,$CC,$CC,$DD,$DD,$EE,$EE
        .byte   $FF,$FF,$10,$10,$21,$21,$56,$83
        .byte   $5F,$5F,$56,$83,$BA,$BA,$56,$83
        .byte   $56,$32,$2D,$CB,$2D,$70
; --- Weapon icon sprite data pointer table (high bytes) ---
LA6AE:  .byte   $A6,$A6,$A6,$A6,$A6,$A6,$A6,$A6
        .byte   $A6,$A6,$A7,$A7,$A7,$A7,$A8,$A8
        .byte   $A7,$A7,$A8,$A8,$A7,$A7,$A8,$A8
        .byte   $A8,$A7,$A8,$A7,$A8,$A7
; --- Weapon icon OAM sprite definitions ---
; Each weapon has two animation frames (4 bytes per sprite: Y, tile, attr, X).
; Terminated by $FF. Referenced via LA690/LA6AE pointer tables.
        .byte   $08,$4F
        .byte   $03,$10,$08,$5F,$C3,$18,$10,$5F
        .byte   $03,$10,$10,$4F,$C3,$18,$FF,$08
        .byte   $F8,$01,$10,$08,$F9,$01,$18,$10
        .byte   $FA,$01,$10,$10,$FB,$01,$18,$FF
        .byte   $08,$B8,$00,$10,$08,$B9,$00,$18
        .byte   $10,$BA,$00,$10,$10,$BB,$00,$18
        .byte   $FF,$08,$5E,$00,$10,$08,$6F,$00
        .byte   $18,$10,$5E,$80,$10,$10,$6F,$80
        .byte   $18,$FF,$08,$BD,$00,$10,$08,$BD
        .byte   $40,$18,$10,$BE,$00,$10,$10,$BF
        .byte   $00,$18,$FF,$08,$FC,$03,$10,$08
        .byte   $FD,$03,$18,$10,$FE,$03,$10,$10
        .byte   $FF,$03,$18,$FF,$0A,$DA,$41,$1C
        .byte   $09,$DB,$41,$14,$04,$D6,$40,$19
        .byte   $04,$E0,$40,$11,$04,$E1,$40,$09
        .byte   $0C,$DC,$40,$19,$0C,$E5,$40,$11
        .byte   $0C,$E6,$40,$09,$14,$E2,$40,$19
        .byte   $14,$E3,$40,$11,$14,$E4,$40,$09
        .byte   $FF,$08,$BC,$01,$10,$08,$BC,$41
        .byte   $18,$10,$BC,$81,$10,$10,$BC,$C1
        .byte   $18,$FF,$13,$D5,$41,$1D,$0C,$CC
        .byte   $40,$20,$0C,$CD,$40,$18,$0C,$CE
        .byte   $40,$10,$0C,$CF,$40,$08,$14,$D0
        .byte   $40,$20,$14,$D1,$40,$18,$14,$D2
        .byte   $40,$10,$14,$D3,$41,$08,$FF,$0F
        .byte   $D5,$41,$1D,$08,$CC,$40,$20,$08
        .byte   $CD,$40,$18,$08,$CE,$40,$10,$08
        .byte   $CF,$40,$08,$10,$D0,$40,$20,$10
        .byte   $D1,$40,$18,$10,$D2,$40,$10,$10
        .byte   $D4,$41,$08,$FF,$08,$B7,$81,$10
        .byte   $08,$B7,$C1,$18,$10,$B7,$01,$10
        .byte   $10,$B7,$41,$18,$FF,$14,$F5,$41
        .byte   $1D,$04,$CB,$40,$08,$0C,$C0,$40
        .byte   $20,$0C,$C1,$40,$18,$0C,$C2,$40
        .byte   $10,$0C,$C3,$40,$08,$0C,$C8,$41
        .byte   $00,$14,$C4,$40,$20,$14,$C5,$40
        .byte   $18,$14,$C6,$40,$10,$14,$C7,$40
        .byte   $08,$14,$C9,$41,$00,$1C,$CA,$41
        .byte   $00,$FF,$10,$F5,$41,$1D,$00,$CB
        .byte   $40,$08,$08,$C0,$40,$20,$08,$C1
        .byte   $40,$18,$08,$C2,$40,$10,$08,$C3
        .byte   $40,$08,$10,$C4,$40,$20,$10,$C5
        .byte   $40,$18,$10,$C6,$40,$10,$10,$F6
        .byte   $40,$08,$10,$F7,$41,$00,$FF,$0E
        .byte   $E7,$41,$1C,$0E,$E8,$41,$14,$04
        .byte   $E9,$40,$19,$04,$EA,$40,$11,$0C
        .byte   $EB,$40,$19,$0C,$EC,$40,$11,$0C
        .byte   $ED,$40,$09,$14,$EE,$40,$19,$14
        .byte   $EF,$40,$11,$14,$F0,$40,$09,$FF
        .byte   $0A,$DA,$41,$1C,$09,$DB,$41,$14
        .byte   $04,$D6,$40,$19,$04,$D7,$40,$11
        .byte   $04,$D8,$40,$09,$0C,$DC,$40,$19
        .byte   $0C,$DD,$40,$11,$0C,$DE,$40,$09
        .byte   $14,$E2,$40,$19,$14,$E3,$40,$11
        .byte   $14,$E4,$40,$09,$FF,$0A,$DA,$41
        .byte   $1C,$09,$DB,$41,$14,$04,$D6,$40
        .byte   $19,$04,$D7,$40,$11,$04,$D9,$40
        .byte   $09,$0C,$DC,$40,$19,$0C,$DD,$40
        .byte   $11,$0C,$DF,$40,$09,$14,$E2,$40
        .byte   $19,$14,$E3,$40,$11,$14,$E4,$40
        .byte   $09,$FF

; =============================================================================
; GEMINI MAN STAGE DATA
; =============================================================================
; Compressed stage layout data for Gemini Man (stage ID $02, internal $22).
; Contains:
;   - Bit-packed screen layout data (which metatile in each 2x2 block)
;   - Enemy spawn lists with X/Y positions per screen
;   - Screen transition / scroll direction data
;   - Metatile definitions (4 tiles per 16x16 metatile)
;   - Palette/attribute data per metatile
;
; The engine decompresses this at runtime into the nametable and
; attribute table as the player scrolls through the stage.
; =============================================================================

        .byte   $08,$00,$00,$0D,$00,$0B
        .byte   $80,$44,$00,$00,$20,$9E,$00,$00
        .byte   $80,$20,$00,$60,$08,$02,$80,$00
        .byte   $00,$00,$22,$10,$00,$00,$00,$42
        .byte   $20,$80,$02,$02,$80,$00,$20,$02
        .byte   $00,$3A,$08,$00,$00,$10,$A0,$04
        .byte   $88,$C0,$00,$02,$00,$11,$28,$B0
        .byte   $0A,$0A,$08,$40,$02,$08,$20,$01
        .byte   $08,$60,$0A,$00,$00,$48,$00,$01
        .byte   $00,$80,$02,$30,$00,$00,$02,$AC
        .byte   $80,$11,$00,$00,$82,$00,$20,$9C
        .byte   $08,$00,$20,$00,$80,$32,$00,$20
        .byte   $02,$01,$00,$02,$80,$80,$00,$22
        .byte   $80,$44,$00,$21,$00,$29,$20,$08
        .byte   $00,$00,$00,$01,$08,$00,$00,$20
        .byte   $80,$10,$28,$00,$02,$98,$02,$08
        .byte   $00,$02,$80,$2C,$20,$50,$22,$06
        .byte   $20,$10,$08,$14,$28,$04,$00,$01
        .byte   $02,$00,$00,$A0,$00,$00,$80,$01
        .byte   $00,$00,$00,$00,$00,$20,$00,$1C
        .byte   $00,$04,$00,$10,$00,$04,$02,$02
        .byte   $02,$00,$00,$20,$20,$00,$00,$11
        .byte   $22,$04,$00,$08,$20,$08,$00,$29
        .byte   $80,$61,$00,$00,$00,$48,$00,$21
        .byte   $08,$40,$08,$40,$18,$32,$82,$00
        .byte   $20,$01,$20,$20,$00,$31,$08,$05
        .byte   $A0,$18,$00,$04,$02,$00,$00,$40
        .byte   $22,$41,$20,$20,$00,$04,$00,$60
        .byte   $A8,$08,$80,$00,$A2,$80,$20,$80
        .byte   $00,$00,$2A,$20,$00,$00,$80,$04
        .byte   $22,$14,$00,$20,$00,$05,$88,$00
        .byte   $20,$22,$0A,$00,$02,$11,$82,$11
        .byte   $00,$02,$00,$00,$20,$00,$20,$62
        .byte   $00,$00,$02,$00,$00,$88,$00,$40
        .byte   $08,$00,$22,$04,$00,$01,$02,$B0
        .byte   $00,$0D,$80,$B0,$28,$81,$20,$48
        .byte   $80,$80,$00,$41,$00,$80,$02,$2D
        .byte   $02,$A0,$02,$10,$08,$22,$20,$01
        .byte   $28,$00,$20,$04,$00,$0C,$22,$F0
        .byte   $30,$10,$08,$04,$00,$49,$00,$80
        .byte   $08,$00,$00,$2A,$0A,$56,$80,$09
        .byte   $20,$5C,$00,$A8,$C2,$59,$02,$60
        .byte   $00,$15,$18,$19,$1A,$1B,$00,$01
        .byte   $02,$03,$04,$1D,$05,$06,$07,$08
        .byte   $17,$09,$0A,$0B,$0C,$1C,$0D,$0E
        .byte   $0F,$10,$11,$12,$13,$14,$15,$16
        .byte   $80,$04,$00,$00,$80,$2C,$20,$10
        .byte   $02,$00,$00,$04,$02,$00,$02,$00
        .byte   $20,$21,$1B,$12,$12,$25,$20,$00
        .byte   $20,$83,$08,$08,$00,$90,$90,$02
        .byte   $20,$30,$28,$20,$62,$80,$A4,$40
        .byte   $65,$80,$A0,$20,$20,$00,$20,$A0
        .byte   $08,$10,$0A,$14,$00,$01,$08,$00
        .byte   $2A,$80,$00,$02,$00,$09,$80,$40
        .byte   $00,$02,$04,$00,$16,$00,$03,$01
        .byte   $03,$01,$33,$01,$09,$01,$33,$01
        .byte   $02,$01,$0B,$01,$00,$02,$27,$02
        .byte   $00,$40,$20,$01,$22,$10,$00,$20
        .byte   $02,$02,$4C,$4E,$0F,$20,$10,$00
        .byte   $0F,$10,$00,$07,$0F,$36,$27,$07
        .byte   $0F,$31,$21,$11,$82,$83,$00,$00
        .byte   $0F,$31,$11,$00,$0F,$31,$15,$17
        .byte   $0F,$22,$12,$12,$0F,$20,$10,$17
        .byte   $84,$85,$86,$00,$0F,$31,$11,$00
        .byte   $0F,$31,$15,$17,$0F,$22,$12,$12
        .byte   $0F,$2C,$0B,$09,$84,$85,$86,$00
        .byte   $0F,$31,$11,$00,$0F,$31,$15,$17
        .byte   $0F,$22,$12,$12,$0F,$31,$21,$11
        .byte   $84,$85,$86,$00,$80,$0B,$00,$20
        .byte   $08,$09,$08,$00,$00,$01,$20,$40
        .byte   $20,$28,$00,$10,$00,$24,$80,$48
        .byte   $02,$40,$08,$08,$80,$40,$20,$12
        .byte   $08,$01,$06,$09,$00,$23,$A0,$11
        .byte   $8A,$00,$14,$1C,$FF,$69,$A0,$66
        .byte   $20,$23,$01,$02,$02,$02,$03,$03
        .byte   $03,$04,$04,$04,$05,$05,$06,$06
        .byte   $06,$07,$07,$07,$08,$08,$08,$09
        .byte   $09,$0A,$0A,$0B,$0E,$0F,$10,$10
        .byte   $10,$12,$12,$12,$13,$15,$15,$16
        .byte   $16,$16,$16,$17,$17,$17,$17,$17
        .byte   $18,$18,$18,$18,$18,$19,$19,$19
        .byte   $19,$19,$19,$1A,$1A,$1B,$1D,$FF
        .byte   $00,$48,$00,$00,$80,$20,$02,$00
        .byte   $00,$2A,$28,$2A,$00,$02,$00,$00
        .byte   $00,$82,$00,$02,$80,$86,$02,$14
        .byte   $00,$00,$00,$40,$80,$06,$02,$22
        .byte   $8A,$60,$00,$04,$22,$10,$20,$C0
        .byte   $08,$84,$00,$21,$04,$00,$02,$00
        .byte   $88,$C5,$00,$02,$A2,$11,$00,$01
        .byte   $08,$64,$00,$10,$00,$90,$00,$20
        .byte   $02,$10,$00,$0A,$00,$01,$00,$0E
        .byte   $20,$70,$00,$02,$00,$10,$00,$02
        .byte   $20,$40,$08,$15,$02,$50,$00,$30
        .byte   $00,$80,$20,$1C,$A0,$00,$08,$00
        .byte   $82,$00,$00,$08,$80,$0C,$00,$04
        .byte   $00,$80,$20,$00,$00,$00,$08,$00
        .byte   $02,$08,$08,$0A,$00,$00,$08,$4A
        .byte   $80,$00,$00,$00,$80,$51,$00,$04
        .byte   $00,$60,$23,$3C,$00,$00,$0A,$40
        .byte   $A2,$10,$20,$00,$00,$80,$00,$80
        .byte   $00,$E5,$08,$13,$20,$12,$20,$51
        .byte   $00,$41,$28,$00,$08,$10,$82,$00
        .byte   $02,$11,$00,$00,$0A,$41,$02,$C0
        .byte   $80,$16,$00,$80,$20,$02,$00,$20
        .byte   $82,$17,$00,$0E,$00,$34,$20,$40
        .byte   $02,$44,$82,$B2,$08,$E0,$08,$20
        .byte   $00,$30,$58,$18,$34,$B8,$28,$78
        .byte   $88,$28,$48,$A8,$48,$A8,$68,$88
        .byte   $98,$18,$68,$C8,$38,$58,$88,$80
        .byte   $80,$48,$B8,$90,$30,$E8,$68,$C0
        .byte   $D8,$18,$20,$D8,$40,$F0,$F8,$98
        .byte   $A8,$F0,$F8,$08,$78,$88,$C8,$F8
        .byte   $68,$88,$C8,$D8,$F8,$48,$78,$A8
        .byte   $C8,$E7,$E8,$2C,$4C,$A8,$D8,$FF
        .byte   $01,$00,$08,$00,$00,$00,$00,$00
        .byte   $41,$00,$03,$01,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$04,$10
        .byte   $00,$00,$08,$00,$18,$00,$40,$00
        .byte   $08,$10,$20,$40,$1A,$00,$00,$00
        .byte   $0C,$00,$44,$00,$20,$00,$20,$00
        .byte   $00,$41,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$41
        .byte   $00,$04,$00,$00,$00,$01,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$40,$40,$30,$00
        .byte   $00,$10,$01,$00,$80,$00,$00,$00
        .byte   $48,$00,$00,$00,$00,$04,$00,$00
        .byte   $02,$00,$08,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$10,$10,$00
        .byte   $20,$00,$00,$00,$28,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$00,$44,$00
        .byte   $80,$00,$84,$00,$00,$10,$08,$00
        .byte   $00,$00,$00,$10,$49,$04,$02,$40
        .byte   $14,$00,$00,$11,$40,$00,$00,$00
        .byte   $01,$00,$38,$58,$B4,$48,$58,$A4
        .byte   $38,$68,$A8,$94,$98,$68,$88,$A8
        .byte   $B4,$58,$78,$98,$68,$48,$94,$00
        .byte   $B0,$38,$58,$47,$48,$18,$18,$9C
        .byte   $18,$18,$9C,$18,$48,$D8,$C8,$C8
        .byte   $18,$D8,$C8,$88,$C8,$18,$18,$C8
        .byte   $C8,$18,$C8,$88,$18,$18,$C8,$C8
        .byte   $18,$C8,$38,$58,$78,$A8,$00,$FF
        .byte   $00,$00,$04,$04,$00,$10,$00,$00
        .byte   $00,$00,$80,$00,$0C,$10,$00,$00
        .byte   $00,$00,$80,$00,$50,$00,$00,$00
        .byte   $04,$00,$00,$04,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$44,$40,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$01,$00,$00,$44
        .byte   $03,$04,$40,$00,$00,$00,$02,$00
        .byte   $00,$41,$00,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$01
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$01,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$04,$00,$10
        .byte   $4C,$00,$04,$00,$40,$00,$00,$40
        .byte   $01,$00,$00,$00,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$08,$10,$10,$00,$20,$00
        .byte   $00,$00,$04,$00,$80,$00,$40,$00
        .byte   $00,$00,$00,$01,$00,$00,$00,$10
        .byte   $00,$00,$50,$00,$80,$00,$00,$00
        .byte   $00,$00,$18,$18,$3B,$18,$18,$3B
        .byte   $18,$18,$18,$3B,$18,$18,$18,$18
        .byte   $3B,$18,$18,$18,$18,$18,$3B,$78
        .byte   $74,$55,$50,$56,$52,$08,$08,$1C
        .byte   $08,$08,$1C,$08,$50,$55,$1A,$1A
        .byte   $08,$54,$1A,$52,$1A,$08,$08,$1A
        .byte   $1A,$08,$1A,$52,$08,$08,$1A,$1A
        .byte   $08,$1A,$54,$26,$26,$13,$49,$FF
        .byte   $08,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$88,$10,$20,$00,$08,$04
        .byte   $04,$00,$00,$00,$00,$00,$20,$00
        .byte   $40,$00,$20,$00,$90,$00,$00,$40
        .byte   $00,$00,$00,$00,$00,$00,$40,$00
        .byte   $00,$00,$00,$01,$00,$00,$08,$01
        .byte   $18,$00,$00,$00,$00,$00,$80,$00
        .byte   $20,$10,$00,$00,$00,$00,$80,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $03,$00,$00,$00,$00,$40,$00,$40
        .byte   $00,$00,$00,$00,$00,$00,$08,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$00
        .byte   $00,$00,$00,$01,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$01,$00,$00,$A0,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $01,$00,$01,$00,$10,$01,$00,$00
        .byte   $20,$00,$00,$10,$00,$00,$00,$00
        .byte   $10,$00,$28,$00,$09,$00,$00,$00
        .byte   $01,$41,$20,$04,$15,$00,$00,$10
        .byte   $83,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$01,$00,$00,$00,$02
        .byte   $00,$03,$04,$05,$02,$06,$07,$08
        .byte   $09,$0A,$0B,$0C,$0C,$0D,$0B,$0D
        .byte   $0E,$0F,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$01,$03,$00
        .byte   $10,$00,$00,$02,$11,$12,$13,$02
        .byte   $0C,$00,$00,$0C,$0D,$0B,$0B,$14
        .byte   $15,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$03,$00,$00,$00,$00
        .byte   $01,$10,$0C,$16,$09,$02,$01,$00
        .byte   $17,$18,$0C,$19,$0E,$15,$0A,$00
        .byte   $0C,$1A,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$00,$00,$01
        .byte   $07,$00,$00,$17,$13,$02,$01,$17
        .byte   $0B,$00,$00,$0C,$0B,$14,$0F,$0C
        .byte   $0B,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$1B,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$1C,$01,$01,$00
        .byte   $00,$00,$10,$1D,$1E,$04,$11,$07
        .byte   $10,$02,$0C,$0D,$0B,$0B,$0D,$0B
        .byte   $14,$15,$1F,$20,$21,$22,$22,$23
        .byte   $1F,$20,$24,$25,$22,$22,$22,$26
        .byte   $24,$25,$1F,$20,$21,$27,$22,$22
        .byte   $28,$20,$24,$25,$26,$00,$27,$26
        .byte   $24,$25,$1F,$29,$00,$00,$2A,$22
        .byte   $2B,$2C,$24,$25,$26,$00,$2D,$22
        .byte   $22,$22,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$22,$2F
        .byte   $00,$30,$2E,$31,$22,$32,$33,$34
        .byte   $30,$35,$26,$24,$36,$37,$38,$38
        .byte   $39,$38,$22,$22,$3A,$3B,$38,$38
        .byte   $38,$3C,$22,$22,$2B,$37,$24,$25
        .byte   $26,$24,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$20,$2E,$1F,$20,$2E,$1F
        .byte   $3D,$23,$25,$26,$24,$25,$26,$00
        .byte   $3E,$26,$3F,$2E,$1F,$29,$40,$00
        .byte   $3E,$23,$38,$38,$24,$38,$41,$00
        .byte   $2C,$26,$00,$38,$38,$38,$00,$42
        .byte   $22,$23,$25,$26,$24,$25,$26,$24
        .byte   $25,$26,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$26,$24,$25,$26,$24
        .byte   $25,$26,$29,$43,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$44,$39,$22,$2B,$2F
        .byte   $22,$26,$29,$45,$39,$46,$22,$2B
        .byte   $2D,$23,$25,$44,$47,$2A,$22,$22
        .byte   $22,$26,$29,$38,$47,$00,$22,$22
        .byte   $48,$23,$25,$38,$38,$47,$46,$49
        .byte   $00,$26,$20,$2E,$1F,$20,$2E,$1F
        .byte   $4A,$23,$4B,$4C,$4D,$4B,$4C,$4D
        .byte   $4E,$4C,$1F,$29,$2F,$22,$36,$00
        .byte   $00,$2D,$49,$25,$00,$27,$22,$2B
        .byte   $2C,$22,$00,$4F,$00,$2A,$22,$22
        .byte   $22,$22,$24,$25,$00,$2D,$22,$22
        .byte   $22,$22,$1F,$29,$2D,$22,$49,$22
        .byte   $22,$36,$24,$25,$22,$49,$46,$22
        .byte   $22,$00,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$22,$22,$49,$00,$00,$00
        .byte   $27,$22,$22,$48,$00,$00,$00,$00
        .byte   $00,$2A,$36,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$50
        .byte   $51,$00,$00,$00,$00,$00,$00,$52
        .byte   $53,$00,$00,$00,$00,$00,$00,$54
        .byte   $55,$00,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$22,$23,$1F,$29,$22,$22
        .byte   $22,$48,$27,$56,$24,$22,$22,$49
        .byte   $00,$00,$2A,$57,$22,$22,$36,$00
        .byte   $00,$00,$00,$26,$58,$22,$00,$00
        .byte   $00,$00,$59,$23,$31,$48,$00,$00
        .byte   $00,$00,$25,$26,$24,$00,$00,$00
        .byte   $00,$00,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$26,$24,$25,$26,$24
        .byte   $25,$26,$00,$2F,$22,$3A,$2D,$22
        .byte   $22,$3F,$00,$00,$46,$22,$22,$49
        .byte   $27,$25,$00,$00,$00,$2A,$22,$2B
        .byte   $3A,$3F,$50,$51,$00,$00,$27,$22
        .byte   $22,$25,$52,$53,$00,$00,$2C,$22
        .byte   $48,$3F,$54,$55,$00,$2C,$2D,$26
        .byte   $00,$25,$1F,$20,$2E,$1F,$20,$21
        .byte   $00,$3F,$4D,$4B,$4C,$4D,$4B,$4C
        .byte   $00,$4C,$21,$00,$23,$1F,$20,$2E
        .byte   $5A,$49,$26,$00,$2F,$22,$22,$22
        .byte   $49,$00,$21,$00,$00,$00,$2F,$00
        .byte   $00,$00,$26,$2B,$00,$00,$00,$00
        .byte   $00,$00,$21,$22,$36,$00,$00,$00
        .byte   $00,$00,$26,$24,$25,$26,$5B,$5B
        .byte   $5B,$5B,$2E,$1F,$20,$21,$5C,$5D
        .byte   $5E,$5C,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$00,$00,$27,$22,$2B,$2C
        .byte   $22,$49,$00,$00,$2A,$22,$22,$49
        .byte   $48,$00,$00,$00,$00,$2A,$48,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $5F,$00,$00,$60,$4F,$00,$61,$00
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$25,$22,$22,$22,$22,$22,$49
        .byte   $49,$2F,$00,$27,$22,$27,$49,$48
        .byte   $00,$00,$63,$00,$48,$00,$00,$00
        .byte   $00,$00,$00,$00,$59,$00,$64,$00
        .byte   $65,$00,$66,$00,$00,$00,$00,$00
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$26,$22,$22,$67,$1F,$20,$2E
        .byte   $1F,$20,$00,$2F,$68,$25,$22,$49
        .byte   $49,$00,$69,$00,$5F,$00,$2A,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$6A
        .byte   $00,$6B,$5F,$00,$65,$00,$6C,$00
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$00,$00,$2F,$22,$22,$25
        .byte   $22,$22,$00,$00,$00,$00,$27,$22
        .byte   $22,$22,$00,$4F,$6C,$00,$00,$2F
        .byte   $48,$48,$00,$00,$00,$6D,$00,$66
        .byte   $63,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$20,$2E,$1F,$20,$2E,$31
        .byte   $6E,$2E,$22,$22,$22,$49,$00,$00
        .byte   $6F,$00,$22,$27,$48,$00,$00,$00
        .byte   $00,$6C,$00,$00,$00,$00,$00,$00
        .byte   $70,$00,$71,$65,$00,$63,$00,$71
        .byte   $00,$00,$5B,$5B,$5B,$5B,$5B,$5B
        .byte   $5B,$5B,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$62,$62,$62,$62,$62,$62
        .byte   $62,$62,$29,$43,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$00,$27,$22,$25,$24
        .byte   $25,$26,$29,$00,$00,$27,$22,$28
        .byte   $20,$2E,$25,$26,$42,$22,$22,$22
        .byte   $25,$26,$20,$2E,$31,$22,$49,$2F
        .byte   $22,$23,$25,$26,$24,$25,$00,$00
        .byte   $00,$26,$20,$2E,$1F,$20,$2E,$31
        .byte   $72,$2E,$4B,$4C,$4D,$4B,$4C,$4D
        .byte   $73,$4C,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$22,$2B,$3A,$27,$3A
        .byte   $25,$26,$29,$00,$27,$22,$22,$22
        .byte   $3F,$2E,$25,$74,$24,$22,$22,$49
        .byte   $25,$26,$29,$43,$31,$27,$22,$2B
        .byte   $00,$75,$25,$76,$24,$00,$27,$22
        .byte   $2B,$75,$29,$43,$1F,$20,$2E,$1F
        .byte   $20,$2E,$4B,$76,$4D,$4B,$4C,$4D
        .byte   $4B,$4C,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$77,$2D,$22,$27,$22,$36
        .byte   $78,$75,$79,$22,$48,$2A,$22,$22
        .byte   $48,$75,$1F,$20,$2E,$1F,$20,$2E
        .byte   $1F,$20,$24,$25,$26,$24,$25,$26
        .byte   $24,$25,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$7A,$7B,$7B,$7B,$7B,$7B
        .byte   $7B,$7C,$7D,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$7F,$80,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$81,$82,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$7F,$83,$7E,$7E,$7E,$7E,$7E
        .byte   $7E,$81,$2E,$1F,$20,$2E,$1F,$20
        .byte   $2E,$1F,$26,$24,$25,$26,$24,$25
        .byte   $26,$24,$20,$2E,$1F,$20,$2E,$1F
        .byte   $20,$2E,$25,$2A,$22,$2B,$37,$38
        .byte   $47,$00,$29,$6B,$22,$49,$30,$38
        .byte   $38,$40,$25,$22,$22,$00,$35,$38
        .byte   $25,$26,$29,$2A,$22,$36,$84,$28
        .byte   $20,$2E,$25,$00,$46,$22,$26,$24
        .byte   $25,$26,$29,$85,$1F,$20,$2E,$1F
        .byte   $20,$2E,$4B,$76,$4D,$4B,$4C,$4D
        .byte   $4B,$4C,$00,$86,$00,$00,$87,$86
        .byte   $00,$00,$00,$88,$00,$89,$8A,$00
        .byte   $87,$00,$8A,$00,$8B,$00,$00,$00
        .byte   $00,$00,$00,$00,$88,$8A,$00,$89
        .byte   $86,$00,$8C,$00,$00,$00,$8D,$00
        .byte   $00,$00,$10,$01,$10,$01,$10,$10
        .byte   $03,$00,$0C,$0F,$14,$04,$0C,$15
        .byte   $05,$02,$0C,$8E,$19,$0B,$0C,$0B
        .byte   $0C,$0C,$8F,$00,$00,$00,$00,$00
        .byte   $8F,$00,$00,$90,$89,$91,$92,$93
        .byte   $00,$00,$8A,$00,$00,$94,$95,$96
        .byte   $86,$8A,$00,$8B,$97,$98,$99,$9A
        .byte   $00,$00,$89,$00,$8A,$00,$00,$8A
        .byte   $89,$00,$00,$00,$1C,$01,$01,$00
        .byte   $03,$00,$01,$10,$0C,$0F,$05,$01
        .byte   $9B,$02,$14,$15,$0C,$9C,$0C,$0A
        .byte   $0B,$0C,$00,$00,$00,$00,$88,$00
        .byte   $00,$00,$8B,$86,$8B,$00,$00,$8A
        .byte   $88,$00,$8A,$00,$00,$86,$90,$00
        .byte   $00,$89,$00,$88,$86,$00,$00,$88
        .byte   $9D,$00,$00,$00,$00,$8A,$00,$00
        .byte   $00,$00,$00,$00,$1C,$03,$03,$00
        .byte   $00,$03,$06,$9E,$0C,$16,$9B,$10
        .byte   $02,$9F,$0D,$0B,$0C,$0D,$0B,$0C
        .byte   $0C,$0E,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$01,$10,$03,$00,$1C
        .byte   $03,$02,$15,$14,$15,$9F,$00,$0C
        .byte   $11,$1E,$0B,$0D,$0B,$0E,$00,$0C
        .byte   $0D,$0B,$1F,$20,$2E,$1F,$20,$21
        .byte   $00,$3F,$24,$49,$27,$22,$27,$49
        .byte   $00,$25,$31,$A0,$A1,$2B,$00,$00
        .byte   $00,$3F,$24,$27,$22,$22,$48,$00
        .byte   $24,$25,$31,$2A,$49,$00,$00,$23
        .byte   $1F,$20,$24,$00,$00,$00,$25,$26
        .byte   $24,$25,$31,$00,$23,$1F,$20,$2E
        .byte   $1F,$20,$4D,$00,$4C,$4D,$4B,$4C
        .byte   $4D,$4C,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$A2,$A3,$00
        .byte   $00,$00,$01,$09,$02,$A4,$A5,$06
        .byte   $07,$02,$0A,$0E,$0C,$A4,$A5,$0D
        .byte   $0B,$0C,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$70,$71
        .byte   $78,$79,$70,$72,$78,$7A,$00,$00
        .byte   $70,$71,$71,$79,$79,$7B,$78,$7B
        .byte   $78,$7A,$70,$71,$78,$70,$70,$72
        .byte   $71,$7A,$78,$7A,$78,$70,$70,$71
        .byte   $71,$79,$78,$79,$78,$79,$79,$7A
        .byte   $79,$7A,$78,$7A,$78,$7A,$78,$78
        .byte   $78,$78,$79,$79,$79,$79,$71,$79
        .byte   $79,$79,$70,$71,$78,$7B,$78,$7B
        .byte   $78,$70,$78,$79,$71,$7B,$71,$7B
        .byte   $79,$7A,$78,$70,$78,$78,$71,$7A
        .byte   $79,$7A,$78,$79,$78,$70,$78,$79
        .byte   $78,$7B,$72,$7A,$7A,$7A,$71,$78
        .byte   $79,$78,$7A,$7A,$7A,$7A,$60,$60
        .byte   $00,$00,$00,$00,$70,$72,$70,$72
        .byte   $78,$70,$78,$7A,$71,$7A,$11,$12
        .byte   $19,$1A,$13,$14,$1B,$1C,$15,$16
        .byte   $1D,$16,$20,$21,$28,$29,$1F,$10
        .byte   $1F,$18,$14,$15,$1C,$1D,$10,$11
        .byte   $18,$19,$12,$13,$1A,$1B,$20,$21
        .byte   $00,$29,$16,$12,$16,$1A,$13,$1F
        .byte   $1B,$1F,$00,$21,$00,$00,$20,$00
        .byte   $28,$29,$00,$00,$00,$29,$00,$21
        .byte   $28,$29,$15,$10,$1D,$18,$20,$21
        .byte   $00,$00,$00,$00,$00,$09,$11,$17
        .byte   $19,$17,$1F,$16,$1F,$16,$00,$00
        .byte   $16,$17,$09,$00,$09,$00,$00,$09
        .byte   $09,$09,$20,$00,$28,$00,$00,$09
        .byte   $00,$00,$09,$09,$09,$09,$09,$09
        .byte   $09,$00,$00,$00,$28,$00,$09,$09
        .byte   $00,$09,$09,$09,$16,$17,$13,$07
        .byte   $1B,$07,$00,$07,$00,$07,$17,$14
        .byte   $17,$1C,$00,$00,$09,$00,$09,$00
        .byte   $00,$00,$00,$00,$28,$29,$07,$10
        .byte   $07,$18,$07,$09,$07,$09,$07,$09
        .byte   $07,$00,$00,$21,$00,$29,$09,$00
        .byte   $09,$09,$20,$00,$00,$00,$20,$21
        .byte   $28,$00,$13,$0F,$1B,$07,$16,$16
        .byte   $16,$16,$17,$17,$17,$17,$1F,$1F
        .byte   $1F,$1F,$16,$07,$16,$07,$17,$1F
        .byte   $00,$00,$00,$00,$00,$3A,$00,$00
        .byte   $3B,$00,$24,$25,$2C,$2D,$26,$27
        .byte   $2E,$2F,$34,$35,$3C,$3D,$36,$37
        .byte   $3E,$3F,$20,$17,$28,$17,$20,$16
        .byte   $00,$29,$20,$21,$1F,$1F,$00,$00
        .byte   $17,$1F,$11,$21,$19,$29,$00,$00
        .byte   $22,$23,$2A,$2B,$16,$17,$2A,$2B
        .byte   $17,$1F,$2A,$2B,$1F,$16,$00,$00
        .byte   $17,$00,$1F,$16,$00,$00,$00,$16
        .byte   $00,$00,$2A,$2B,$2A,$2B,$00,$00
        .byte   $00,$16,$16,$17,$00,$00,$00,$1F
        .byte   $00,$00,$1F,$00,$00,$00,$20,$10
        .byte   $28,$18,$20,$1F,$28,$1F,$00,$00
        .byte   $16,$00,$00,$00,$00,$17,$00,$00
        .byte   $1F,$16,$16,$00,$00,$00,$00,$17
        .byte   $00,$00,$07,$14,$07,$1C,$07,$00
        .byte   $00,$00,$00,$00,$00,$1F,$17,$00
        .byte   $00,$00,$0F,$14,$07,$1C,$07,$16
        .byte   $07,$16,$0F,$17,$07,$17,$00,$06
        .byte   $00,$06,$07,$17,$07,$17,$05,$00
        .byte   $05,$00,$00,$21,$28,$00,$05,$21
        .byte   $05,$29,$17,$31,$17,$30,$31,$31
        .byte   $30,$30,$31,$1F,$30,$1F,$15,$30
        .byte   $1D,$30,$30,$30,$30,$30,$30,$12
        .byte   $30,$1A,$17,$30,$17,$30,$30,$1F
        .byte   $30,$1F,$04,$30,$03,$30,$03,$30
        .byte   $03,$30,$09,$09,$00,$00,$0F,$10
        .byte   $07,$18,$00,$00,$00,$43,$00,$00
        .byte   $00,$41,$00,$00,$42,$00,$00,$42
        .byte   $00,$00,$43,$00,$00,$00,$41,$00
        .byte   $00,$00,$00,$43,$00,$00,$00,$41
        .byte   $00,$00,$79,$70,$79,$78,$00,$00
        .byte   $00,$42,$43,$00,$00,$43,$00,$00
        .byte   $00,$4A,$00,$44,$4B,$4C,$45,$46
        .byte   $4D,$4E,$51,$52,$59,$5A,$53,$54
        .byte   $5B,$5C,$55,$56,$5D,$00,$00,$00
        .byte   $00,$68,$61,$62,$69,$6A,$63,$64
        .byte   $6B,$00,$65,$00,$00,$00,$78,$7B
        .byte   $71,$7A,$79,$7B,$79,$7A,$41,$00
        .byte   $00,$43,$70,$71,$71,$7B,$78,$79
        .byte   $71,$79,$00,$00,$28,$16,$20,$21
        .byte   $17,$29,$00,$00,$74,$75,$00,$00
        .byte   $76,$77,$7C,$7D,$7C,$7D,$7E,$7F
        .byte   $7E,$7F,$20,$06,$00,$06,$16,$17
        .byte   $28,$29,$1F,$00,$1F,$00,$11,$00
        .byte   $19,$00,$00,$14,$00,$1C,$00,$00
        .byte   $10,$11,$00,$00,$12,$13,$00,$00
        .byte   $14,$15,$18,$19,$17,$14,$1A,$1B
        .byte   $15,$10,$1C,$1D,$11,$17,$17,$1C
        .byte   $10,$11,$1D,$18,$12,$13,$19,$17
        .byte   $14,$15,$00,$00,$09,$09,$00,$16
        .byte   $00,$16,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- Metatile definitions (16x16 blocks, 4 CHR tiles each) ---
; Two sets (BG tiles left/right pages). Each metatile = TL, TR, BL, BR.
        .byte   $00,$00,$00,$00,$00,$13,$02,$00
        .byte   $14,$04,$22,$24,$01,$01,$01,$01
        .byte   $01,$04,$08,$0A,$08,$0A,$0C,$0E
        .byte   $06,$06,$28,$2A,$28,$2A,$2C,$2E
        .byte   $00,$26,$4C,$4E,$40,$42,$00,$8A
        .byte   $8C,$00,$6C,$6E,$60,$62,$00,$AA
        .byte   $AC,$AE,$13,$02,$00,$00,$C8,$CA
        .byte   $CC,$CE,$00,$00,$00,$8F,$E8,$EA
        .byte   $EC,$EE,$44,$77,$76,$00,$00,$68
        .byte   $6A,$4B,$44,$67,$00,$82,$84,$00
        .byte   $74,$01,$00,$00,$A0,$A2,$94,$A6
        .byte   $EB,$01,$00,$00,$C0,$C1,$B5,$C6
        .byte   $01,$01,$00,$B0,$E0,$E2,$E4,$E6
        .byte   $01,$00,$00,$D2,$D5,$E1,$01,$01
        .byte   $01,$00,$48,$4A,$4A,$01,$E0,$E2
        .byte   $E4,$E6,$58,$5A,$5A,$5A,$F8,$FA
        .byte   $FC,$FE,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$12,$03,$00
        .byte   $15,$05,$23,$25,$01,$65,$01,$01
        .byte   $01,$05,$09,$0B,$09,$0B,$0D,$0F
        .byte   $07,$07,$29,$2B,$29,$2B,$2D,$2F
        .byte   $00,$27,$4D,$4F,$41,$43,$89,$8B
        .byte   $8D,$00,$6D,$6F,$61,$63,$A9,$AB
        .byte   $AD,$00,$12,$03,$00,$00,$C9,$CB
        .byte   $CD,$CF,$00,$00,$88,$00,$E9,$00
        .byte   $ED,$EF,$44,$00,$00,$00,$00,$69
        .byte   $6B,$00,$66,$44,$81,$83,$85,$00
        .byte   $75,$00,$01,$80,$A1,$A3,$95,$A7
        .byte   $00,$00,$01,$87,$C1,$B4,$C5,$C7
        .byte   $01,$00,$00,$B1,$D0,$E3,$E5,$00
        .byte   $00,$00,$C4,$D3,$D6,$00,$01,$01
        .byte   $00,$00,$49,$4B,$4B,$01,$E1,$E3
        .byte   $E5,$E7,$59,$5B,$57,$47,$F9,$FB
        .byte   $FD,$FF,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$12,$03,$00
        .byte   $14,$04,$32,$34,$01,$01,$01,$01
        .byte   $01,$04,$18,$1A,$18,$1A,$1C,$1E
        .byte   $16,$16,$38,$3A,$38,$3A,$3C,$3E
        .byte   $00,$36,$5C,$5E,$70,$72,$00,$9A
        .byte   $9C,$9E,$7C,$7E,$70,$72,$B8,$BA
        .byte   $BC,$BE,$12,$03,$00,$00,$D8,$DA
        .byte   $DC,$DE,$00,$00,$00,$9F,$F8,$FA
        .byte   $FC,$FE,$44,$00,$00,$00,$00,$78
        .byte   $7A,$5B,$54,$01,$90,$92,$94,$96
        .byte   $AF,$01,$00,$00,$A0,$B2,$B4,$B6
        .byte   $00,$01,$00,$00,$D0,$D1,$D4,$E5
        .byte   $01,$00,$00,$C2,$00,$F2,$F4,$00
        .byte   $01,$00,$00,$F0,$F7,$00,$01,$00
        .byte   $00,$00,$58,$5A,$5A,$00,$F0,$F2
        .byte   $F4,$F6,$58,$5A,$5A,$5A,$F8,$FA
        .byte   $FC,$FE,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$13,$02,$00
        .byte   $15,$05,$33,$35,$01,$01,$01,$01
        .byte   $01,$05,$19,$1B,$19,$1B,$1D,$1F
        .byte   $17,$17,$39,$3B,$39,$3B,$3D,$3F
        .byte   $00,$37,$5D,$5F,$71,$73,$99,$9B
        .byte   $9D,$00,$7D,$7F,$71,$73,$B9,$BB
        .byte   $BD,$BF,$13,$02,$00,$00,$D9,$DB
        .byte   $DD,$DF,$00,$00,$98,$00,$F9,$FB
        .byte   $FD,$FF,$44,$76,$00,$77,$A8,$79
        .byte   $7B,$00,$01,$01,$91,$93,$95,$97
        .byte   $00,$00,$01,$86,$A1,$B3,$B5,$B7
        .byte   $00,$00,$01,$A5,$D1,$B5,$E4,$D7
        .byte   $00,$00,$00,$C3,$F1,$F3,$F5,$00
        .byte   $00,$00,$E7,$F6,$A4,$00,$01,$00
        .byte   $00,$00,$59,$5B,$47,$00,$F1,$F3
        .byte   $F5,$F7,$59,$5B,$57,$57,$F9,$FB
        .byte   $FD,$FF,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- Palette attribute data per metatile ---
; Each byte = palette index (0-3) for a metatile, packed into attribute bytes
; by the engine during nametable construction.
        .byte   $00,$00,$00,$00,$00,$13,$13,$10
        .byte   $10,$23,$53,$73,$10,$10,$00,$00
        .byte   $00,$43,$10,$10,$11,$11,$11,$11
        .byte   $10,$11,$10,$10,$11,$11,$11,$11
        .byte   $00,$11,$62,$62,$82,$82,$03,$03
        .byte   $03,$03,$62,$62,$82,$82,$03,$03
        .byte   $03,$03,$03,$03,$00,$00,$03,$03
        .byte   $03,$03,$00,$00,$03,$03,$03,$03
        .byte   $03,$03,$00,$00,$00,$00,$01,$01
        .byte   $01,$01,$00,$00,$02,$02,$02,$01
        .byte   $01,$00,$00,$02,$02,$02,$02,$01
        .byte   $00,$00,$00,$01,$02,$02,$01,$01
        .byte   $00,$00,$10,$01,$01,$01,$01,$01
        .byte   $00,$00,$01,$01,$01,$01,$00,$00
        .byte   $00,$00,$13,$13,$13,$00,$00,$00
        .byte   $00,$00,$13,$13,$13,$13,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- End of bank padding ---
        .byte   $00,$00,$00,$00,$00
        brk
        brk
        brk
        brk
        brk
