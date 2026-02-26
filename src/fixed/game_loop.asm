; ===========================================================================
; main_game_entry — top-level game loop (registered as task 0 at $C8D0)
; ===========================================================================
; Called once from RESET via coroutine scheduler. Initializes system state,
; runs title/stage select (bank $18 $9009), then enters the stage init path.
; On game over or stage completion, control returns here for the next cycle.
; Registered at $1FFE99: $93/$94 = $C8D0 (coroutine entry address).
; ---------------------------------------------------------------------------
main_game_entry:
        ldx     #$BF                    ; reset stack pointer to $01BF
        txs                             ; set stack pointer
        lda     ppu_ctrl_shadow         ; sync PPUCTRL from shadow
        sta     PPUCTRL                 ; write to PPU control register
        jsr     task_yield              ; yield 1 frame (let NMI run)
        lda     #$88                    ; CHR bank config byte
        sta     $E4                     ; init shift register seed
        cli                             ; enable IRQ
        lda     #$01                    ; $9B = 1 (game active flag)
        sta     irq_enable              ; enable IRQ processing
        lda     #MUSIC_TITLE            ; play title screen music
        jsr     submit_sound_ID_D9      ; start title music
        lda     #$40                    ; $99 = $40 (initial $99,
        sta     gravity                 ; set to $55 once gameplay starts)
        lda     #$18                    ; map bank $18 to $8000-$9FFF
        sta     mmc3_select             ; set $8000 bank select
        jsr     select_PRG_banks        ; apply bank switch
        jsr     stage_select_title ; title screen / stage select (bank $18)
        lda     #HEALTH_FULL            ; full Rush Coil ammo
        sta     $A9                     ; store to ammo slot
        lda     #$02                    ; $AE = 2 $AE (display as 3)
        sta     lives                   ; set initial lives

; --- stage_reinit: re-entry point after death/boss dispatch ---
; Clears $0150-$016F (entity scratch buffer), then falls through to stage_init.
game_entry_stage_reinit:  lda     #$00  ; clear $0150-$016F (32 bytes)
        ldy     #$1F                    ; Y = $1F (32 bytes to clear)
game_entry_clear_scratch_buffer:  sta     $0150,y ; clear scratch buffer byte
        dey                             ; next byte
        bpl     game_entry_clear_scratch_buffer ; loop until all cleared

; --- stage_init: set up a new stage ---
stage_init:  lda     #$00               ; $EE = 0: allow NMI rendering
        sta     nmi_skip                ; allow NMI rendering during setup

; --- initialize display and entity state ---
        jsr     fade_palette_in         ; fade screen to black
        lda     #$04                    ; $97 = 4 (OAM scan start offset)
        sta     oam_ptr                 ; OAM scan start offset = 4
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; zero all 32 entity slots
        lda     #$01                    ; $A000 = 1 (H-mirroring)
        sta     MMC3_MIRRORING          ; set horizontal mirroring

; --- zero-init game state variables ---
        lda     #$00                    ; A = 0 for bulk clear
        sta     game_mode               ; scroll mode = 0 (normal)
        sta     ent_status              ; player entity inactive
        sta     camera_x_hi             ; nametable select = 0
        sta     $71                     ; overlay init trigger = 0
        sta     $72                     ; overlay scroll active = 0
        sta     camera_x_lo             ; camera X low = 0
        sta     scroll_y                ; scroll Y offset = 0
        sta     camera_screen           ; starting screen = 0
        sta     $25                     ; camera X coarse = 0
        sta     ent_x_scr               ; player X screen = 0
        sta     ent_y_scr               ; player Y screen = 0
        sta     $B1                     ; HP bar state = 0
        sta     $B2                     ; boss bar state = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     $9E                     ; scroll X fine = 0
        sta     $9F                     ; scroll X fine copy = 0
        sta     boss_active             ; boss active flag = 0
        sta     current_weapon          ; weapon = $00 (Mega Buster)
        sta     $B4                     ; ammo slot index = 0
        sta     weapon_cursor           ; weapon menu cursor = 0
        sta     $6F                     ; current screen = 0

; --- start stage music ---
        ldy     stage_id                ; Y = stage number
        lda     frame_loop_stage_music_table,y ; load stage music ID
        jsr     submit_sound_ID_D9      ; play music

; --- set initial scroll/render state ---
        lda     #FACING_RIGHT           ; initial facing = right
        sta     player_facing           ; player facing = right (1=R, 2=L)
        sta     $23                     ; scroll column state = 1
        sta     $2E                     ; scroll direction = right
        lda     #$FF                    ; $29 = $FF (render-done sentinel;
        sta     $29                     ; BEQ loops while 0, skips on $FF)
        lda     #$1F                    ; $24 = 31 (column counter for
        sta     $24                     ; initial nametable fill)

; --- fill nametable: render columns until $29 becomes nonzero ---
game_entry_render_columns_loop:  lda     #$01 ; $10 = 1 (render direction: right)
        sta     $10                     ; render direction = right
        jsr     do_render_column        ; render one BG column
        jsr     task_yield              ; yield to NMI (display frame)
        lda     $29                     ; check if rendering complete
        beq     game_entry_render_columns_loop ; still zero — keep rendering

; --- parse room 0 config from $AA40 ---
        lda     stage_id                ; switch to stage data bank
        sta     prg_bank                ; store stage bank number
        jsr     select_PRG_banks        ; apply bank switch
        lda     $AA40                   ; room 0 config byte
        pha                             ; save for screen count
        and     #$E0                    ; bits 7-5: scroll/connection flags
        sta     $2A                     ; $2A = room scroll/connection flags
        ldx     #$01                    ; default: X=1 (H-mirror), Y=$2A
        ldy     #$2A                    ; default viewport height (h-mirror)
        and     #$C0                    ; bits 7-6: vertical connection
        beq     game_entry_set_mirroring ; if no vertical connection: H-mirror
        dex                             ; else: X=0 (V-mirror), Y=$26
        ldy     #$26                    ; reduced viewport height (v-mirror)
game_entry_set_mirroring:  stx     MMC3_MIRRORING ; set mirroring mode
        sty     $52                     ; $52 = nametable height ($2A or $26)
        pla                             ; bits 4-0 of room config
        and     #$1F                    ; = screen count
        sta     $2C                     ; $2C = screen count for room 0
        lda     #$00                    ; A = 0 for room/screen init
        sta     $2B                     ; $2B = current room = 0
        sta     $2D                     ; $2D = room base screen = 0

; --- load stage data (palettes, CHR, nametable attributes) ---
        jsr     load_stage              ; load stage palettes + CHR banks
        lda     #$00                    ; $18 = 0 (rendering disabled during setup)
        sta     palette_dirty           ; suppress palette upload during setup
        jsr     task_yield              ; yield to NMI
        jsr     fade_palette_out        ; fade from black → stage palette
        lda     #$80                    ; player X = $80 (128, center of screen)
        sta     ent_x_px                ; player X = 128 (center of screen)

; --- set HP and scroll mode for stage type ---
game_entry_set_hp_scroll:  lda     #HEALTH_FULL ; (full HP: 28 bars)
        sta     player_hp               ; full HP = 28 bars
        lda     #$E8                    ; default: $51/$5E = $E8 (normal scroll)
        sta     $51                     ; scroll end marker (normal)
        sta     $5E                     ; scroll end marker copy
        lda     camera_screen           ; if not starting at screen 0: skip
        bne     game_entry_set_intro_chr ; not screen 0, skip Gemini
        lda     stage_id                ; stages $02 (Gemini) and $09 (DR-Gemini)
        cmp     #STAGE_GEMINI           ; use horizontal scroll mode
        beq     game_entry_gemini_scroll ; (start scrolling right from screen 0)
        cmp     #STAGE_DOC_GEMINI       ; check Doc Robot Gemini
        bne     game_entry_set_intro_chr ; not Gemini → skip scroll override
game_entry_gemini_scroll:  lda     #$9F ; $5E = $9F (Gemini scroll end marker)
        sta     $5E                     ; store Gemini scroll end
        lda     #$02                    ; $F8 = 2 (screen scroll mode)
        sta     game_mode               ; game mode = auto-scroll

; --- set intro CHR banks and begin fade-in ---
game_entry_set_intro_chr:  lda     #$74 ; $EA/$EB = $74/$75 (intro/ready CHR pages)
        sta     $EA                     ; store intro CHR page $EA
        lda     #$75                    ; intro CHR page $75
        sta     $EB                     ; store intro CHR page $EB
        jsr     update_CHR_banks        ; apply CHR bank settings
        lda     #$30                    ; $0611 = $30 (BG palette color for intro)
        sta     $0611                   ; store BG palette entry
        inc     palette_dirty           ; enable rendering (palette_dirty = 1)
        lda     #$3C                    ; A = 60 (fade-in frame count)

; --- fade-in loop: 60 frames with flashing "READY" overlay ---
game_entry_fadein_loop:  pha            ; save frame countdown
        lda     $95                     ; frame counter bit 4:
        and     #$10                    ; toggles every 16 frames
        beq     game_entry_hide_overlay ; if clear → hide overlay

; show "READY" overlay: copy 5 OAM entries from $CCF8 table
        ldx     #$10                    ; X = $10 (5 sprites × 4 bytes - 4)
game_entry_show_ready_overlay:  lda     frame_loop_ready_overlay_data,x ; copy OAM entry: Y, tile, attr, X
        sta     $0200,x                 ; store OAM Y position
        lda     rush_coil_dispatch_table,x ; OAM tile index
        sta     $0201,x                 ; store OAM tile index
        lda     rush_marine_dispatch_table,x ; load OAM attribute byte
        sta     $0202,x                 ; store OAM attribute byte
        lda     rush_jet_dispatch_table,x ; load OAM X position
        sta     $0203,x                 ; OAM X position
        dex                             ; prev OAM entry (X -= 4)
        dex                             ; (part of X -= 4)
        dex                             ; (part of X -= 4)
        dex                             ; next OAM entry
        bpl     game_entry_show_ready_overlay ; loop all 5 sprites
        lda     #$14                    ; A = 20 (start hiding at sprite 5)

; hide remaining OAM entries: set Y = $F8 (off-screen) from offset A onward
game_entry_hide_overlay:  sta     oam_ptr ; oam_ptr = OAM scan start offset
        tax                             ; X = OAM offset
        lda     #$F8                    ; Y = $F8 = off-screen
game_entry_hide_oam_loop:  sta     $0200,x ; hide sprite: Y = $F8
        inx                             ; advance to next OAM entry
        inx                             ; (part of X += 4)
        inx                             ; (part of X += 4)
        inx                             ; next OAM entry (4 bytes)
        bne     game_entry_hide_oam_loop ; until X wraps to 0
        lda     #$00                    ; $EE = 0 (allow NMI rendering)
        sta     nmi_skip                ; allow NMI rendering
        jsr     task_yield              ; yield to NMI (display frame)
        inc     nmi_skip                ; nmi_skip = 1 (suppress next NMI rendering)
        inc     $95                     ; advance frame counter
        pla                             ; decrement fade-in countdown
        sec                             ; prepare for subtraction
        sbc     #$01                    ; decrement countdown
        bne     game_entry_fadein_loop  ; loop until 60 frames complete

; --- fade-in complete: set up gameplay CHR and spawn player ---
        lda     #$0F                    ; $0611 = $0F (BG color → black)
        sta     $0611                   ; store BG palette entry
        inc     palette_dirty           ; mark palette dirty for upload
        lda     #$00                    ; $EA/$EB = pages 0/1 (shared sprite CHR)
        sta     $EA                     ; R2: tiles $00-$3F at $1000
        lda     #$01                    ; R3: tiles $40-$7F at $1400
        sta     $EB                     ; store CHR page $EB
        jsr     update_CHR_banks        ; switch to gameplay sprite CHR

; --- initialize player entity (slot 0) for teleport-in ---
        lda     #$80                    ; ent_status = $80 (player entity active)
        sta     ent_status              ; player entity = active
        lda     #$00                    ; ent_y_px = 0 (player Y = top of screen)
        sta     ent_y_px                ; start at top of screen
        lda     #$D0                    ; ent_flags = $D0 (bit7=drawn, bit6=face right, bit4=set)
        sta     ent_flags               ; store player flags
        lda     #$4C                    ; player X speed = $01.4C (walk speed, pre-loaded
        sta     ent_xvel_sub            ; for when reappear transitions to on_ground)
        lda     #$01                    ; X velocity whole = $01
        sta     ent_xvel                ; store X velocity whole
        lda     #$00                    ; player Y speed = $F9.00 (terminal fall velocity)
        sta     ent_yvel_sub            ; player drops from top during reappear state
        lda     #$F9                    ; Y velocity = $F9 (fast downward)
        sta     ent_yvel                ; terminal fall speed for teleport-in
        ldx     #$00                    ; slot 0 (player): set OAM $13 = teleport beam
        lda     #$13                    ; OAM ID $13 = teleport beam sprite
        jsr     reset_sprite_anim       ; set teleport beam animation
        lda     #PSTATE_REAPPEAR        ; $30 = $04 (player state = reappear)
        sta     player_state            ; set to reappear state
        lda     #$80                    ; $B2 = $80 (stage initialization complete)
        sta     $B2                     ; mark stage init complete

; ===========================================================================
; gameplay_frame_loop — main per-frame game loop
; ===========================================================================
; Runs once per frame during active gameplay. Each iteration:
;   1. Check Start button for pause/weapon menu
;   2. Run player state machine (dispatch by $30)
;   3. Update camera scroll
;   4. Process all entity AI (banks $1C/$1D)
;   5. Spawn new enemies (bank $1A + stage bank)
;   6. Run per-frame subsystems (bank $09: HUD, scroll, sound, etc.)
;   7. Fade palette, build OAM, yield to NMI
;   8. Check exit conditions ($59=boss done, $3C=death, $74=stage clear)
; ---------------------------------------------------------------------------
gameplay_frame_loop:  lda     joy1_press ; Start button pressed?
        and     #BTN_START              ; ($14 = new button presses this frame)
        beq     gameplay_no_pause       ; no Start press → skip pause logic
        lda     player_state            ; skip pause if player state is:
        cmp     #PSTATE_REAPPEAR        ; $04 = reappear
        beq     gameplay_no_pause       ; reappearing → can't pause
        cmp     #PSTATE_SPECIAL_DEATH   ; $09+ = boss_wait and above
        beq     gameplay_no_pause       ; special death, skip pause
        cmp     #PSTATE_BOSS_WAIT       ; check boss wait state
        bcs     gameplay_no_pause       ; state >= boss_wait, skip

; --- pause check: despawn Rush (slot 1) when switching from Rush weapon ---
        lda     current_weapon          ; weapon ID - 6: Rush weapons are $06-$0B
        sec                             ; carry clear = weapon < $06 (not Rush)
        sbc     #$06                    ; subtract 6 (Rush range)
        bcc     frame_loop_check_active_weapons ; weapon < 6, not Rush
        and     #$01                    ; odd result = Rush entity active ($07,$09,$0B)
        beq     frame_loop_check_active_weapons ; even = Rush item select, not active
        lda     #$00                    ; despawn slot 1 (Rush entity)
        sta     $0301                   ; before opening pause menu
        beq     game_entry_check_pause  ; always branches (A=0)
frame_loop_check_active_weapons:  lda     $0301 ; if ANY weapon slot (1-3) is active,
        ora     $0302                   ; don't allow pause
        ora     $0303                   ; (prevents pausing mid-shot)
        bmi     gameplay_no_pause       ; active projectile → can't pause
game_entry_check_pause:  ldy     player_state ; check per-state pause permission table
        lda     frame_loop_pause_permission,y ; bit 7 set = pause not allowed
        bmi     gameplay_no_pause       ; pause not allowed, skip
        lda     #$02                    ; switch to bank $02/$03 (pause menu code)
        sta     prg_bank                ; store bank number
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_A003             ; call pause menu handler
gameplay_no_pause:  lda     stage_id    ; switch to stage bank
        sta     prg_bank                ; stage bank for state dispatch
        jsr     select_PRG_banks        ; apply bank switch
        jsr     prelude_check_slide_speed ; player state dispatch + physics

; --- apply pending hazard state transition (set by tile collision) ---
        lda     hazard_pending          ; hazard_pending = pending state from hazard
        beq     frame_loop_update_camera ; 0 = no pending state
        sta     player_state            ; apply: set player state
        cmp     #$0E                    ; state $0E = spike/pit death?
        bne     frame_loop_clear_hazard_pending ; don't allow pause
        lda     #SNDCMD_STOP            ; play death jingle ($F2 = stop music)
        jsr     submit_sound_ID         ; stop current music
        lda     #SFX_DEATH              ; play death sound $17
        jsr     submit_sound_ID         ; play death sound effect
frame_loop_clear_hazard_pending:  lda     #$00 ; clear pending state
        sta     hazard_pending          ; hazard consumed
frame_loop_update_camera:  jsr     update_camera ; scroll/camera update
        lda     camera_x_lo             ; $25 = camera X (coarse)
        sta     $25                     ; sync coarse scroll from camera
        sta     temp_00                 ; also store in temp $00
        lda     game_mode               ; if scroll mode (game_mode==2):
        cmp     #$02                    ; compute camera offset for
        bne     frame_loop_compute_scroll_offset ; not auto-scroll mode → skip
        lda     camera_screen           ; load camera screen page
        lsr     a                       ; shift right (divide by 2)
        ror     temp_00                 ; rotate screen:scroll into $00
        lsr     a                       ; shift right again
        ror     temp_00                 ; shift right again
        lda     temp_00                 ; result = camera offset / 4
        sta     $5F                     ; $5F = camera offset / 4
frame_loop_compute_scroll_offset:  lda     ent_x_scr ; track max screen progress
        cmp     $6F                     ; ($6F = farthest screen reached)
        bcc     frame_loop_track_screen_progress ; haven't passed $6F yet → skip store
        sta     $6F                     ; update farthest screen
frame_loop_track_screen_progress:  lda     ent_x_px ; player X pixel position
        sta     $27                     ; store to $27 copy
        ldx     #$1C                    ; select banks $1C/$1D
        stx     mmc3_select             ; (entity processing code)
        inx                             ; X = $1D (bank pair high)
        stx     prg_bank                ; store PRG bank number
        jsr     select_PRG_banks        ; switch to entity AI banks
        jsr     banked_8000             ; process all entity AI
        lda     #$1A                    ; bank $1A = enemy spawner
        sta     mmc3_select             ; set $8000 bank to $1A
        lda     stage_id                ; load stage bank number
        sta     prg_bank                ; store stage PRG bank
        jsr     select_PRG_banks        ; apply bank switch
        jsr     check_new_enemies       ; spawn enemies for current screen
        lda     #$09                    ; select bank $09
        sta     mmc3_select             ; (per-frame subsystems)
        jsr     select_PRG_banks        ; switch to per-frame subsystem bank
        jsr     banked_8003             ; bank $09 subsystems:
        jsr     banked_8006             ; screen scroll, HUD update,
        jsr     banked_800F             ; sound processing,
        jsr     banked_8009             ; background animation,
        jsr     banked_800C             ; item pickup,
        jsr     banked_8000             ; checkpoint tracking,
        jsr     banked_8012             ; etc.
        jsr     palette_fade_tick       ; update palette fade animation
        jsr     process_frame_yield_with_player ; build OAM + yield 1 frame
; --- DEBUG (shipped in retail) — controller 2 left-press latches right-held ---
; Pressing Left on controller 2 ($98 bit 1) permanently ORs the Right flag
; ($17 bit 0) into the P2 held state. This is necessary because the NES d-pad
; physically cannot report Left+Right simultaneously. Once latched, the debug
; effects (super jump, pit death immunity) persist for the rest of the stage.
        lda     $98                     ; P2 newly-pressed buttons
        and     #$02                    ; bit 1 = Left pressed?
        beq     frame_loop_debug_flag_check ; no → skip
        lda     #$01                    ; latch Right-held flag
        ora     $17                     ; into P2 held state
        sta     $17                     ; store updated P2 state
frame_loop_debug_flag_check:  jsr     shift_register_tick ; LFSR animation tick
        lda     $59                     ; $59 != 0: boss defeated
        bne     frame_loop_boss_defeated ; boss defeated → handle post-boss
        lda     $3C                     ; $3C != 0: player died
        bne     death_handler           ; player died, handle death
        lda     $74                     ; $74 != 0: stage clear
        bne     stage_clear_handler     ; stage clear → handle completion
        jmp     gameplay_frame_loop     ; loop back for next frame

; --- boss_defeated_handler ($59 != 0) ---

frame_loop_boss_defeated:  lda     #$00 ; clear rendering/overlay/scroll state
        sta     nmi_skip                ; allow NMI rendering
        sta     $71                     ; clear overlay init trigger
        sta     $72                     ; clear overlay scroll
        sta     game_mode               ; reset game mode
        sta     boss_active             ; clear boss-active flag
        lda     #$18                    ; select banks $18/$10
        sta     mmc3_select             ; bank $18 for post-defeat dispatch
        lda     #$10                    ; bank $10 for post-defeat
        sta     prg_bank                ; bank $10 = boss post-defeat code
        jsr     select_PRG_banks        ; switch banks
        jsr     stage_select_rm_intro ; boss post-defeat (bank $10)
        jmp     game_entry_stage_reinit ; → stage_reinit

; --- death_handler ($3C != 0) ---

death_handler:  lda     #$00            ; clear death flag + boss state
        sta     $3C                     ; clear death flag
        sta     boss_active             ; clear boss active flag
        lda     lives                   ; decrement lives (BCD format)
        sec                             ; subtract 1 life (BCD)
        sbc     #$01                    ; subtract one life
        bcc     game_over               ; underflow → game over
        sta     lives                   ; store decremented lives
        and     #$0F                    ; wraps to $0F, subtract 6
        cmp     #$0F                    ; ($10 - $01 = $0F → $09)
        bne     frame_loop_bcd_lives_correction ; low nibble valid → no BCD fix needed
        lda     lives                   ; reload lives for BCD correction
        sec                             ; prepare for BCD fix
        sbc     #$06                    ; subtract 6 for BCD adjust
        sta     lives                   ; store corrected lives
frame_loop_bcd_lives_correction:  lda     #$00 ; clear rendering/overlay state
        sta     nmi_skip                ; allow NMI rendering
        sta     $71                     ; clear overlay trigger
        sta     $72                     ; clear overlay scroll
        sta     game_mode               ; clear game mode
        lda     stage_select_page       ; if stage_select_page == $12 (Wily stage):
        cmp     #$12                    ; special respawn path
        beq     frame_loop_wily_respawn ; Wily stage, special respawn
        jmp     handle_checkpoint       ; normal respawn at checkpoint

; Wily stage death → bank $18 $9006 (Wily-specific respawn)

frame_loop_wily_respawn:  lda     #$18  ; select bank $18
        sta     mmc3_select             ; bank $18 for Wily respawn
        jsr     select_PRG_banks        ; switch banks
        jmp     stage_select_wily_gate ; Wily respawn (bank $18)

; --- game_over ($AE underflowed) ---

game_over:  lda     #$00                ; clear all state
        sta     nmi_skip                ; clear NMI skip flag
        sta     boss_active             ; clear boss active flag
        sta     $71                     ; clear overlay trigger
        sta     $72                     ; clear overlay scroll
        sta     game_mode               ; clear game mode
        lda     #$18                    ; select banks $18/$13
        sta     mmc3_select             ; (game over screen)
        lda     #$13                    ; bank $13 for game over
        sta     prg_bank                ; bank $13 = game over handler
        jsr     select_PRG_banks        ; apply bank switch
        jsr     stage_select_password ; game over sequence (bank $18)
        jmp     game_entry_stage_reinit ; → stage_reinit (continue/retry)

; --- stage_clear_handler ($74 != 0: stage completion triggered) ---
; $74 bit 7 distinguishes: 0 = normal stage clear, 1 = special/Wily clear.

stage_clear_handler:  pha               ; save $74 (stage clear type)
        lda     #$00                    ; clear game state for transition
        sta     boss_active             ; boss active = 0
        sta     $74                     ; stage clear trigger = 0
        sta     $B1                     ; HP bar = 0
        sta     $B2                     ; boss bar = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     boss_active             ; boss active = 0 (redundant)
        sta     camera_screen           ; starting screen = 0
        sta     game_mode               ; scroll mode = 0
        lda     #$0B                    ; switch to banks $0B/$0E
        sta     mmc3_select             ; (bank $0B → $8000)
        lda     #$0E                    ; bank $0E for clear handler
        sta     prg_bank                ; store PRG bank number
        jsr     select_PRG_banks        ; switch banks
        pla                             ; $74 AND $7F: 0 = normal stage clear
        and     #$7F                    ; mask off high bit
        bne     frame_loop_special_clear_check ; nonzero = special clear (Wily/Doc Robot)
        jsr     banked_8000             ; normal stage clear sequence (bank $0E)
        jmp     game_entry_stage_reinit ; → stage_reinit

; --- special stage clear (Wily/Doc Robot stages) ---

frame_loop_special_clear_check:  lda     $75 ; $75 = stage clear sub-type
        cmp     #$06                    ; $06 = final boss / ending
        beq     frame_loop_ending_sequence ; $06 = ending → special path
        jsr     banked_8003             ; Wily/Doc Robot clear (bank $0E)
        jmp     game_entry_stage_reinit ; → stage_reinit

; --- game ending sequence ---

frame_loop_ending_sequence:  lda     #$0C ; switch to banks $0C/$0E
        sta     mmc3_select             ; (bank $0C → $8000)
        lda     #$0E                    ; bank $0E for ending
        sta     prg_bank                ; store PRG bank number
        jsr     select_PRG_banks        ; switch banks
        lda     #$11                    ; $F8 = $11 (ending game mode)
        sta     game_mode               ; set ending game mode
        jsr     banked_8000             ; run ending sequence
        jmp     game_entry_stage_reinit ; → stage_reinit (title screen)

handle_checkpoint:  lda     stage_id    ; store current level
        sta     prg_bank                ; as $A000-$BFFF bank
        jsr     select_PRG_banks        ; and swap banks to it
        ldy     #$00                    ; loop through checkpoint data
frame_loop_find_checkpoint:  lda     $6F ; if current checkpoint >
        cmp     $AAF8,y                 ; current screen ID
        bcc     frame_loop_checkpoint_boundary ; we're done & 1 too far
        iny                             ; next checkpoint entry
        bne     frame_loop_find_checkpoint ; next checkpoint
frame_loop_checkpoint_boundary:  dey    ; we're one checkpoint too far so
        bpl     frame_loop_checkpoint_restore ; go back one, if it's negative
        jmp     stage_init              ; we're just at the beginning

; --- checkpoint_restore: reset game state and reload from checkpoint ---

frame_loop_checkpoint_restore:  tya     ; save checkpoint index
        pha                             ; save checkpoint index
        jsr     fade_palette_in         ; fade screen to black
        lda     #$04                    ; $97 = 4 (OAM scan start)
        sta     oam_ptr                 ; store OAM scan start
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; zero all entity slots

; --- zero game state (same as stage_init, minus music/facing) ---
        lda     #$00                    ; A = 0 for bulk clear
        sta     game_mode               ; scroll mode = 0
        sta     camera_x_hi             ; nametable select = 0
        sta     $71                     ; overlay init trigger = 0
        sta     $72                     ; overlay scroll active = 0
        sta     camera_x_lo             ; camera X low = 0
        sta     scroll_y                ; scroll Y offset = 0
        sta     $25                     ; camera X coarse = 0
        sta     ent_y_scr               ; player Y screen = 0
        sta     $B1                     ; HP bar state = 0
        sta     $B2                     ; boss bar state = 0
        sta     $B3                     ; weapon orb bar = 0
        sta     boss_active             ; boss active flag = 0
        sta     current_weapon          ; weapon = $00 (Mega Buster)
        sta     $B4                     ; ammo slot index = 0
        sta     weapon_cursor           ; weapon menu cursor = 0

; --- restore checkpoint position data ---
        pla                             ; restore checkpoint index
        tay                             ; Y = checkpoint index
        lda     $AAF8,y                 ; set screen from checkpoint table
        sta     $29                     ; $29 = render sentinel/screen
        sta     camera_screen           ; camera_screen = starting screen
        sta     ent_x_scr               ; player X screen
        lda     $ABC0,y                 ; set scroll fine position from checkpoint
        sta     $9E                     ; scroll fine X position
        sta     $9F                     ; scroll fine X copy
        ldx     $AAF0,y                 ; $2B = checkpoint room number
        stx     $2B                     ; store room number

; --- parse room config bits (same logic as stage_init) ---
        lda     $AA40,x                 ; load room config byte
        pha                             ; save for screen count later
        sta     temp_00                 ; save config to temp
        and     #$20                    ; bit 5 = horizontal scroll?
        sta     $2A                     ; if set, $2A = $20 (h-scroll flags)
        bne     frame_loop_checkpoint_mirroring ; h-scroll set, skip v-scroll
        lda     temp_00                 ; else $2A = bits 7-6 (vertical connection)
        and     #$C0                    ; bits 7-6 = vertical connection
        sta     $2A                     ; store v-scroll connection flags
frame_loop_checkpoint_mirroring:  ldx     #$01 ; default: H-mirror, viewport height $2A
        ldy     #$2A                    ; default viewport height (h-mirror)
        and     #$20                    ; if h-scroll (bit 5 set): keep defaults
        bne     frame_loop_checkpoint_set_height ; h-scroll → keep H-mirror defaults
        dex                             ; else: V-mirror (X=0), viewport $26
        ldy     #$26                    ; V-mirror viewport height $26
frame_loop_checkpoint_set_height:  stx     MMC3_MIRRORING ; set nametable mirroring (0=V, 1=H)
        sty     $52                     ; $52 = viewport height ($2A or $26)
        pla                             ; $2C = screen count (lower 5 bits)
        and     #$1F                    ; mask to 5-bit count
        sta     $2C                     ; store screen count
        lda     #$00                    ; $2D = 0 (scroll progress within room)
        sta     $2D                     ; $2D = 0 (room base screen)
        ldy     stage_id                ; load stage music ID from $CD0C table
        lda     frame_loop_stage_music_table,y ; and start playing it
        jsr     submit_sound_ID_D9      ; start stage music
        lda     #$01                    ; A = 1 for facing/flags
        sta     player_facing           ; player_facing = face right
        sta     $23                     ; $23 = column render flag
        sta     $2E                     ; $2E = render dirty flag
        dec     $29                     ; $29-- (back up screen sentinel for render)
        lda     #$1F                    ; $24 = $1F (screen attribute pointer)
        sta     $24                     ; 31 columns per nametable fill

; --- render 33 columns of room tiles (one per frame, yielding to NMI) ---
        lda     #$21                    ; 33 columns ($21)
frame_loop_render_columns_loop:  pha    ; save column counter
        lda     #$01                    ; render direction = right
        sta     $10                     ; $10 = 1 (render one column)
        jsr     do_render_column        ; render current column
        jsr     task_yield              ; yield (NMI uploads to PPU)
        pla                             ; restore column counter
        sec                             ; decrement counter
        sbc     #$01                    ; decrement column count
        bne     frame_loop_render_columns_loop ; loop until all 33 done
        jsr     load_stage              ; apply CHR/palette for this room
        lda     #$00                    ; suppress palette upload this frame
        sta     palette_dirty           ; suppress palette upload this frame
        jsr     task_yield              ; yield to let NMI run
        jsr     fade_palette_out        ; fade from black → stage palette
        lda     #$80                    ; spawn player at Y=$80 (mid-screen)
        sta     ent_x_px                ; player X = 128 (center of screen)
        jmp     game_entry_set_hp_scroll ; → continue stage_init (Gemini scroll, fade-in)

; --- DEAD CODE: unreachable block at $1ECCE5 ---
; No references to this address exist in any bank — unreachable dead code.
; Would have checked $98 mode for refill state and filled all 12 weapon ammo
; slots to HEALTH_FULL. The bytes at $CCF1-$CCF3 also serve as the overlap
; trick operand for the Rush dispatch table below, so they must be preserved.

dead_code_refill_all_ammo:
        lda     $98                     ; load P2 button state
        and     #$03                    ; check if mode == 1 (refill)
        cmp     #$01                    ; mode 1 = refill
        bne     frame_loop_ammo_refill_exit ; if not, skip
        ldy     #$0B                    ; fill $A2-$AD (12 ammo slots)

; --- END DEAD CODE ---
; (Bytes below serve as overlap data for Rush dispatch tables and are
; referenced from player_ground.asm via frame_loop_ready_overlay_oam.)

frame_loop_ready_overlay_oam:  lda     #HEALTH_FULL ; all to full (28 units)
; --- overlap trick: $99,$A2,$00 = sta $00A2,y (sta ammo_array,y) ---
; Fall-through fills ammo slots; entry at ready_sprite_table loads X=0.
frame_loop_ammo_refill_loop:  .byte   $99   ; opcode: sta abs,y
frame_loop_ready_sprite_table:  ldx     #$00 ; code: ldx #$00 | addr: $00A2
        dey                             ; next ammo slot
        bpl     frame_loop_ammo_refill_loop ; loop until all 12 filled
frame_loop_ammo_refill_exit:  rts
frame_loop_ready_overlay_data:  .byte   $80
rush_coil_dispatch_table:  .byte   $40
rush_marine_dispatch_table:  .byte   $00
rush_jet_dispatch_table:  .byte   $6C,$80,$41,$00,$74,$80,$42,$00
        .byte   $7C,$80,$43,$00,$84,$80,$44,$00
        .byte   $8C
frame_loop_stage_music_table:           ; stage music IDs (indexed by stage_id)
        .byte   MUSIC_NEEDLE,MUSIC_MAGNET,MUSIC_GEMINI,MUSIC_HARD
        .byte   MUSIC_TOP,MUSIC_SNAKE,MUSIC_SPARK,MUSIC_SHADOW
        .byte   MUSIC_NEEDLE,MUSIC_GEMINI,MUSIC_SPARK,MUSIC_SHADOW
        .byte   MUSIC_WILY_1,MUSIC_WILY_1,MUSIC_WILY_2,MUSIC_WILY_2
        .byte   MUSIC_WILY_3,MUSIC_WILY_3
frame_loop_pause_permission:  .byte   $00,$00,$FF,$00,$FF,$00,$FF,$FF ; per-state pause permission (bit 7 = no pause)
        .byte   $00,$FF,$00,$FF,$FF,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF
        .byte   $FF

; ===========================================================================
; player_state_dispatch_prelude — pre-process before running state handler
; ===========================================================================
; Resets walk speed (unless sliding), handles platform push, invincibility
; timer, and charge shot (Needle auto-fire).
prelude_check_slide_speed:  lda     ent_xvel ; if X speed whole == $02 (slide speed):
        cmp     #$02                    ; slide speed?
        bne     prelude_reset_walk_speed ; no → reset to walk speed
        lda     player_state            ; load current player state
        cmp     #PSTATE_SLIDE           ; check if sliding
        beq     prelude_setup_per_frame ; sliding, keep slide speed
prelude_reset_walk_speed:  lda     #$4C ; reset walk speed to $01.4C
        sta     ent_xvel_sub            ; store walk sub-pixel speed
        lda     #$01                    ; walk speed whole = $01
        sta     ent_xvel                ; store walk whole speed

; --- per-frame setup ---
prelude_setup_per_frame:  lda     #$40  ; gravity = $40 (stage select value)
        sta     gravity                 ; store gravity value
        ldx     #$00                    ; X = 0 (player slot), clear collision entity
        stx     $5D                     ; clear collision entity slot
        lda     $36                     ; if platform pushing: apply push velocity
        beq     prelude_invincibility_timer ; no push pending, skip
        jsr     prelude_platform_push_apply ; (platform_push handler)

; --- invincibility timer ---
prelude_invincibility_timer:  lda     invincibility_timer ; load invincibility timer
        beq     prelude_charge_shot_check ; zero → skip
        dec     invincibility_timer     ; decrement timer
        bne     prelude_charge_shot_check ; not expired → skip
        lda     ent_anim_frame          ; clear animation lock (bit 7)
        and     #$7F                    ; when invincibility expires
        sta     ent_anim_frame          ; store cleared anim frame

; --- charge shot: Needle Cannon auto-fire ($1F counter) ---
prelude_charge_shot_check:  lda     joy1_held ; B button held?
        and     #BTN_B                  ; B button mask
        bne     prelude_charge_increment ; yes → increment charge
        lda     #$E0                    ; not held: reset charge to $E0
        sta     $1F                     ; (high value = "not charging")
        bne     prelude_dispatch_state  ; → dispatch state
prelude_charge_increment:  lda     $1F  ; increment charge counter by $20
        clc                             ; wraps to 0 after 8 increments
        adc     #$20                    ; add $20 to charge counter
        sta     $1F                     ; store updated charge counter
        bne     prelude_dispatch_state  ; not wrapped → dispatch state
        lda     current_weapon          ; if weapon == $02 (Needle Cannon):
        cmp     #WPN_NEEDLE             ; auto-fire when charge wraps
        bne     prelude_dispatch_state  ; not Needle → skip auto-fire
        lda     joy1_press              ; set B-button-pressed flag
        ora     #BTN_B                  ; (triggers weapon_fire in state handler)
        sta     joy1_press              ; force B press for auto-fire
prelude_dispatch_state:  ldy     player_state ; load player state index
        lda     player_state_ptr_lo,y   ; state handler ptr low byte
        sta     temp_00                 ; store at $00
        lda     player_state_ptr_hi,y   ; state handler ptr high byte
        sta     $01                     ; store at $01
        jmp     (temp_00)               ; jump to state handler

player_state_ptr_lo:  .byte   $36,$07,$FD,$EB,$BA,$13,$AB,$31
        .byte   $58,$29,$91,$BE,$D3,$E1,$79,$CC
        .byte   $14,$AA,$52,$33,$8A,$8C ; $12 player_warp_anim
player_state_ptr_hi:  .byte   $CE,$D0,$D3,$D4,$D5,$D6,$D6,$D8 ; $02
        .byte   $D8,$D9,$D9,$D9,$D9,$DB,$D7,$CD ; $0A
        .byte   $DD,$DD,$DE,$DF,$DF,$E0,$66,$61 ; $12
        .byte   $E0,$CF,$CE
        .byte   $CF

; player state $0F: frozen by external force (slam/grab/cutscene) [confirmed]
; Player cannot move. Gravity still applies (falls if airborne).
; Boss fight ($5A bit 7) freezes animation counter instead.
player_stunned:
        lda     boss_active             ; if boss fight active: freeze animation
        bmi     prelude_stunned_freeze_anim ; boss active, freeze anim
        ldy     #$00                    ; Y = 0 (standing hitbox offset)
        jsr     move_vertical_gravity   ; apply gravity + vertical collision
        bcc     prelude_stunned_done    ; not landed → done
        lda     #$01                    ; anim ID $01 = idle
        cmp     ent_anim_id             ; already idle?
        beq     prelude_stunned_done    ; yes → done
        jsr     reset_sprite_anim       ; reset to idle animation
        lda     #$00                    ; clear shooting flag
        sta     walk_flag               ; clear walk/shoot flag
prelude_stunned_done:  rts              ; return to caller

prelude_stunned_freeze_anim:  lda     #$00 ; freeze anim counter (boss cutscene)
        sta     ent_anim_frame          ; clear anim frame counter
        rts                             ; return to caller

; --- platform_push: apply external velocity from moving platform ---
; $36 = push direction (bit 0: 1=right, 0=left)
; $37/$38 = push speed (sub/whole), applied as player X speed temporarily.

prelude_platform_push_apply:  lda     ent_flags ; save player sprite flags
        pha                             ; push flags on stack
        lda     ent_xvel_sub            ; save player X speed (sub + whole)
        pha                             ; push X speed sub on stack
        lda     ent_xvel                ; load X velocity whole
        pha                             ; push X speed whole on stack
        lda     $37                     ; apply platform push speed
        sta     ent_xvel_sub            ; as player X speed temporarily
        lda     $38                     ; load push speed whole byte
        sta     ent_xvel                ; store as X velocity whole
        ldy     player_state            ; Y = collision offset:
        cpy     #PSTATE_SLIDE           ; if sliding (state $02), use slide hitbox
        beq     prelude_platform_push_dir ; sliding → skip Y reset
        ldy     #$00                    ; Y = 0 (standing hitbox)
prelude_platform_push_dir:  lda     $36 ; push direction: bit 0 = right
        and     #$01                    ; test bit 0 (direction)
        beq     prelude_platform_push_left ; bit 0 clear → push left
        jsr     move_right_collide      ; push right with collision
        jmp     prelude_platform_push_restore ; skip left push handler

prelude_platform_push_left:  iny        ; push left with collision
        jsr     move_left_collide       ; apply left push with collision

; --- restore player state after platform push ---
prelude_platform_push_restore:  pla     ; restore original X speed
        sta     ent_xvel                ; restore X velocity whole
        pla                             ; pull sub-pixel speed
        sta     ent_xvel_sub            ; restore X velocity sub
        pla                             ; restore original facing (bit 6 only)
        and     #ENT_FLAG_HFLIP         ; from saved sprite flags
        sta     temp_00                 ; store facing bit in temp
        lda     ent_flags               ; clear bit 6, then OR with saved
        and     #$BF                    ; mask off facing bit
        ora     temp_00                 ; merge saved facing bit
        sta     ent_flags               ; store merged flags
        lda     #$00                    ; clear platform push flag
        sta     $36                     ; clear push flag
prelude_exit:  rts                      ; return to caller

