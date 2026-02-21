; =============================================================================
; MEGA MAN 3 (U) — BANK $0B — INTRO SEQUENCE + DOC ROBOT SHADOW STAGE
; =============================================================================
; Mapped to $8000-$9FFF. Contains the game intro/opening cutscene and
; the Doc Robot Shadow Man stage data (stage_id $16, bank pair $0B).
;
; The intro sequence has three phases:
;   Phase 1 ($8006): "Mountain top" — Mega Man falls from sky, Rush slides in,
;       title music plays via bank $0E music driver, palette fade cycling.
;   Phase 2 ($81B0): "Flying on Rush" — Proto Man scene: Mega Man rides Rush
;       upward through star field, wind SFX ($28), palette flashing.
;   Phase 3 ($8291): "Proto Man confrontation" — Mega Man descends, Proto Man
;       appears with whistle ($5D), they meet, Proto Man departs.
;
; After all three intro phases, jumps to Doc Robot stage entry ($8439)
; which loads the Shadow Man stage nametable and runs the Doc Robot
; cutscene showing boss selection sprites.
;
; Stage data (metatile columns, enemy/object layouts, tile attribute maps)
; fills the $A000-$BFFF half of this bank pair.
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"

; --- External references (fixed bank + swappable bank $0E) ---
LA000           := $A000                ; init music driver (bank $0E)
LA003           := $A003                ; continue music playback (bank $0E)
LA006           := $A006                ; start music track X (bank $0E)
prepare_oam_buffer           := $C5E9                ; prepare OAM buffer
clear_entity_table           := $C628                ; clear entity table
fade_palette_out           := $C74C                ; fade palette out (reveal)
fade_palette_in           := $C752                ; fade palette in (to black)
metatile_column_ptr_by_id           := $E8B4                ; init metatile column pointers
fill_nametable_progressive           := $EF8C                ; fill one nametable column
apply_y_speed           := $F797                ; apply Y speed (gravity)
reset_sprite_anim           := $F835                ; reset sprite animation (A=anim, X=entity)
submit_sound_ID_D9           := $F898                ; submit sound ID (with $D9 prefix)
submit_sound_ID           := $F89A                ; submit sound ID (direct)
process_frame_yield_full           := $FD6E                ; process frame + yield (full entity update)
process_frame_yield           := $FD80                ; process frame + yield (sprites only)
task_yield           := $FF21                ; task yield (wait for NMI)
update_CHR_banks           := $FF3C                ; update CHR banks via MMC3
select_PRG_banks           := $FF6B                ; select PRG banks

.segment "BANK0B"

; ===========================================================================
; Entry point jump table
; ===========================================================================
; $8000: intro sequence (called from game mode handler)
; $8003: Doc Robot Shadow Man stage entry
; ===========================================================================
        jmp     L8006                   ; entry: intro sequence

        jmp     code_8439               ; entry: Doc Robot stage

; ===========================================================================
; Phase 1 Init: "Mountain Top" — Mega Man falls, Rush slides in
; ===========================================================================
; Fade in, load stage $16 nametable, set up Mega Man (entity 0) and
; Rush (entity 1) sprites, start title music, then enter main loop.
; ===========================================================================
L8006:  lda     #$00
        sta     nmi_skip                ; disable NMI processing
        jsr     fade_palette_in                   ; fade palette to black
        lda     #$04
        sta     oam_ptr                 ; OAM write position
        jsr     prepare_oam_buffer                   ; prepare OAM buffer
        jsr     clear_entity_table                   ; clear entity table
        jsr     task_yield                   ; wait for NMI (task yield)
        lda     #SNDCMD_INIT
        jsr     submit_sound_ID_D9                   ; submit sound $F0 (silence/init)
        lda     #$00
        sta     $B1                     ; clear music state vars
        sta     $B2
        sta     $B3
        sta     $70                     ; nametable column progress counter
        sta     camera_x_hi
        sta     camera_x_lo
        sta     $B8                     ; music continuation flag
        lda     #$16
        sta     stage_id                ; stage $16 = intro/Shadow Man stage
        lda     #$02
        jsr     metatile_column_ptr_by_id                   ; init metatile column pointers
; --- fill nametable progressively until complete ---
code_8038:  lda     #$00
        sta     $10                     ; column direction = rightward
        jsr     fill_nametable_progressive                   ; draw one nametable column
        jsr     task_yield                   ; wait for NMI
        lda     $70
        bne     code_8038               ; loop until nametable fully drawn
; --- load palette and CHR bank settings ---
        ldy     #$1F
code_8048:  lda     L8655,y             ; phase 1 palette (mountain top)
        sta     $0620,y                 ; write to palette buffer
        dey
        bpl     code_8048
        ldy     #$05
code_8053:  lda     L86B5,y             ; phase 1 CHR bank assignments
        sta     $E8,y
        dey
        bpl     code_8053
        jsr     update_CHR_banks                   ; update CHR banks via MMC3
; --- set up entities 0 (Mega Man) and 1 (Rush) ---
        ldy     #$01
code_8061:  lda     #$80
        sta     ent_status,y            ; entity active
        lda     #$90
        sta     ent_flags,y             ; facing left, palette 0
        lda     L86E3,y
        sta     ent_anim_id,y           ; anim: $13=Mega Man fall, $60=Rush
        lda     L86E5,y
        sta     ent_x_px,y              ; x: $D8=MM, $58=Rush
        lda     L86E7,y
        sta     ent_y_px,y              ; y: $00=MM (top), $A4=Rush (ground)
        lda     #$00
        sta     ent_x_scr,y
        sta     ent_anim_frame,y
        sta     ent_anim_state,y
        sta     ent_y_scr,y
        sta     ent_timer
        sta     ent_var1
        sta     ent_var2
        sta     ent_var3
        lda     #$C0
        sta     ent_yvel_sub,y          ; initial upward velocity (sub)
        lda     #$FF
        sta     ent_yvel,y              ; initial upward velocity = -1 (rises first frame)
        dey
        bpl     code_8061
; --- load initial OAM sprite data (mountain scenery) ---
        ldy     #$07
code_80A6:  lda     L86E9,y
        sta     $0200,y                 ; OAM buffer: mountain decoration sprites
        dey
        bpl     code_80A6
        lda     #$11
        sta     game_mode               ; set game mode = intro cinematic
        lda     #$C0
        sta     $5E                     ; scroll limit
        jsr     task_yield                   ; wait for NMI
        jsr     fade_palette_out                   ; fade palette out (reveal scene)
; --- init phase 1 state variables ---
        lda     #$08
        sta     ent_timer               ; music track index
        lda     #$00
        sta     $0104                   ; palette cycle index
        sta     $95                     ; frame counter
        sta     camera_screen           ; camera screen position
; ===========================================================================
; Phase 1 Main Loop: Mega Man falling + Rush sliding in
; ===========================================================================
; Mega Man (entity 0) falls with gravity until Y >= $A4 (ground level),
; then plays landing anim. Rush (entity 1, anim $04) slides left until
; X < $98, then changes to standing anim. Meanwhile, palette cycles
; through sunset colors and title music plays track-by-track.
; ===========================================================================
code_80CB:  ldx     #$00
        lda     ent_anim_id             ; check Mega Man's current animation
        cmp     #$04
        beq     code_80F9               ; branch if Rush sliding anim (== $04)
        cmp     #$01
        beq     code_811A               ; branch if standing idle (== $01)
; --- Mega Man is still falling ---
        jsr     apply_y_speed                   ; apply Y speed (gravity)
        lda     #$A4
        cmp     ent_y_px                ; has MM reached ground (Y=$A4)?
        bcs     code_80F1               ; not yet, skip
        sta     ent_y_px                ; clamp Y to ground level
        lda     #$04
        cmp     ent_anim_state          ; check if landing anim complete
        bne     code_811A
        lda     #$04                    ; set anim $04 (landed/standing)
        jsr     reset_sprite_anim                   ; reset sprite animation
code_80F1:  lda     #$00
        sta     ent_anim_frame          ; reset animation frame
        jmp     code_811A
; --- Rush is sliding leftward ---
code_80F9:  lda     ent_anim_id
        cmp     #$01
        beq     code_811A               ; already standing, skip
        lda     ent_x_sub
        sec
        sbc     #$80                    ; subtract 1.5 px per frame (sub)
        sta     ent_x_sub
        lda     ent_x_px
        sbc     #$01                    ; subtract 1 px (whole)
        sta     ent_x_px
        cmp     #$98
        bcs     code_811A               ; Rush hasn't reached target X yet
        lda     #$01                    ; set anim $01 (standing)
        jsr     reset_sprite_anim                   ; reset sprite animation
; --- palette cycling for sunset sky effect ---
code_811A:  lda     $B8                 ; music continuation flag
        bne     code_8121
        sta     $05E1                   ; clear entity 1 anim frame if music idle
code_8121:  lda     $95
        and     #$03                    ; every 4 frames...
        bne     code_814E
        lda     $0104                   ; palette cycle index
        asl     a
        adc     $0104                   ; index * 3 = offset into palette table
        tay
        ldx     #$05
code_8131:  lda     L86C7,y             ; load 3 bytes of cycling palette
        sta     $0600,x                 ; write to palette BG color slots 5-7
        iny
        inx
        cpx     #$08
        bne     code_8131
        stx     palette_dirty           ; mark palette for update
        inc     $0104
        lda     $0104
        cmp     #$06                    ; 6 palette steps in cycle
        bne     code_814E
        lda     #$00
        sta     $0104                   ; reset cycle to beginning
; --- process frame and run music driver ---
code_814E:  lda     #$08
        sta     oam_ptr                 ; OAM write position past scenery
        jsr     process_frame_yield                   ; process frame + yield (sprites + NMI)
        lda     ent_anim_id
        cmp     #$01                    ; Mega Man standing?
        bne     code_81AD               ; no — loop back
; --- title music playback state machine ---
        lda     ent_var1                ; inter-track delay timer
        bne     code_8190               ; counting down between tracks
        lda     $95
        and     #$03
        bne     code_81AD               ; only update music every 4 frames
        lda     #$0E
        sta     prg_bank                ; bank $0E = music/sound driver
        jsr     select_PRG_banks                   ; select PRG banks
        lda     $B8                     ; music continuation flag
        bne     code_817B               ; track playing — continue it
        ldx     ent_timer               ; track index (starts at $08)
        jsr     LA006                   ; start new music track X
        jmp     code_81AD
; --- continue current music track ---
code_817B:  jsr     LA003               ; continue music playback
        lda     $B8
        cmp     #$FF                    ; track finished?
        bne     code_81AD               ; no — keep playing
        inc     ent_timer               ; advance to next track
        lda     #$00
        sta     $B8                     ; clear music flag
        lda     #$B5
        sta     ent_var1                ; set inter-track delay ($B5 frames)
code_8190:  dec     ent_var1            ; count down delay
        bne     code_81AD
        lda     ent_timer
        cmp     #$0A                    ; played all tracks (up to $0A)?
        beq     code_81B0               ; yes — transition to phase 2
        lda     #$0E
        sta     prg_bank
        jsr     select_PRG_banks                   ; select music bank
        lda     #$00
        sta     nmi_skip
        jsr     LA000                   ; init music driver for next track
        jsr     task_yield                   ; wait for NMI
code_81AD:  jmp     code_80CB           ; loop phase 1

; ===========================================================================
; Phase 2 Init: "Flying on Rush" — Mega Man rides Rush upward
; ===========================================================================
; Fade to black, load a new nametable (stage $08 via bank $13),
; set up Mega Man riding Rush (anim $5F) moving right and upward.
; Star field scrolls vertically to simulate flight.
; ===========================================================================
code_81B0:  lda     #$00
        sta     nmi_skip                ; disable NMI
        jsr     fade_palette_in                   ; fade palette to black
        lda     #$04
        sta     oam_ptr
        jsr     prepare_oam_buffer                   ; prepare OAM buffer
        jsr     clear_entity_table                   ; clear entity table
        jsr     task_yield                   ; wait for NMI
        lda     #$00
        sta     $70                     ; nametable column counter
        sta     game_mode               ; game mode = 0 (reset for phase 2)
        lda     #$13
        sta     prg_bank                ; bank $13 for phase 2 stage tiles
        jsr     select_PRG_banks                   ; select PRG banks
        lda     #$08
        jsr     metatile_column_ptr_by_id                   ; init metatile columns for stage $08
; --- fill nametable progressively ---
code_81D6:  lda     #$00
        sta     $10
        jsr     fill_nametable_progressive                   ; draw one nametable column
        jsr     task_yield                   ; wait for NMI
        lda     $70
        bne     code_81D6               ; loop until complete
; --- load phase 2 palette and CHR banks ---
        ldy     #$1F
code_81E6:  lda     L8675,y             ; phase 2 palette (night sky)
        sta     $0620,y                 ; write to palette buffer
        dey
        bpl     code_81E6
        ldy     #$05
code_81F1:  lda     L86BB,y             ; phase 2 CHR bank assignments
        sta     $E8,y
        dey
        bpl     code_81F1
        jsr     update_CHR_banks                   ; update CHR banks via MMC3
; --- set up Mega Man on Rush (entity 0) ---
        lda     #$80
        sta     ent_status              ; entity active
        lda     #$90
        sta     ent_flags               ; facing left
        lda     #$5F
        sta     ent_anim_id             ; anim $5F = riding Rush Jet
        lda     #$80
        sta     ent_x_px                ; center-ish X
        lda     #$E8
        sta     ent_y_px                ; near bottom of screen
        lda     #$00
        sta     ent_x_scr
        sta     ent_anim_frame
        sta     ent_anim_state
        sta     ent_y_scr
        sta     $0104                   ; palette flash toggle
        sta     ent_var1                ; movement phase
        sta     ent_xvel_sub
        lda     #$10
        sta     ent_timer               ; movement phase timer
        lda     #$04
        sta     ent_xvel                ; horizontal speed = 4 px/frame
        jsr     task_yield                   ; wait for NMI
        jsr     fade_palette_out                   ; fade palette out (reveal scene)
; ===========================================================================
; Phase 2 Main Loop: Flying upward on Rush Jet
; ===========================================================================
; Mega Man rises upward (code_8603 decrements Y) while moving rightward
; with sinusoidal motion (code_861B). Star field sprites scroll.
; Palette flashes between two night-sky color sets. Wind SFX ($28)
; plays every 16 frames. Loop exits when ent_y_scr != 0 (scrolled off).
; ===========================================================================
code_823D:  lda     ent_y_scr
        bne     code_8291               ; scrolled past screen — phase 2 done
        jsr     code_8603               ; move Rush upward + apply X velocity
; --- palette flash effect (every 16 frames) ---
        lda     $95
        and     #$0F
        bne     code_8269               ; not time to flash
        lda     $0104                   ; flash toggle (0 or 1)
        asl     a
        adc     $0104                   ; * 3 for table offset
        tay
        ldx     #$05
code_8255:  lda     L86D9,y             ; night sky flash palette
        sta     $0600,x                 ; BG palette bytes 5-7
        iny
        inx
        cpx     #$08
        bne     code_8255
        lda     $0104
        eor     #$01                    ; toggle between two palette states
        sta     $0104
; --- star twinkle effect (every 8 frames) ---
code_8269:  lda     $95
        and     #$07
        bne     code_827E
        lda     $0105                   ; star animation counter
        inc     $0105
        and     #$03                    ; cycle through 4 star brightness levels
        tay
        lda     L86DF,y                 ; star color: $04, $14, $0F, $0F
        sta     $060D                   ; sprite palette slot for stars
; --- wind sound effect (every 16 frames) ---
code_827E:  lda     $95
        and     #$0F
        bne     code_8289
        lda     #SFX_WIND
        jsr     submit_sound_ID                   ; submit wind SFX $28
code_8289:  inc     palette_dirty       ; mark palette for NMI upload
        jsr     process_frame_yield_full                   ; process frame + yield (full)
        jmp     code_823D               ; loop phase 2

; ===========================================================================
; Phase 3 Init: "Proto Man Confrontation"
; ===========================================================================
; Fade to black, reset entity 0 (Mega Man on Rush) for descent.
; Copy sprite palette to BG palette, set BG palette to all black.
; Re-use same nametable from phase 2.
; ===========================================================================
code_8291:  lda     #$00
        sta     nmi_skip                ; disable NMI
        jsr     fade_palette_in                   ; fade palette to black
        lda     #$00
        sta     ent_y_scr              ; reset screen position
        sta     ent_x_scr
        sta     ent_var1                ; movement phase
        sta     ent_var2                ; star scroll offset
        sta     ent_var3                ; frame counter for wind SFX
        sta     ent_yvel_sub
        sta     ent_yvel                ; no vertical velocity initially
        sta     ent_xvel_sub
        lda     #$04
        sta     ent_xvel                ; horizontal speed = 4 px/frame
        lda     #$10
        sta     ent_timer               ; movement phase duration
        lda     #$E8
        sta     ent_y_px                ; start near bottom
        lda     #$80
        sta     ent_x_px                ; center X
        lda     #$80
        sta     ent_status              ; entity active
        lda     #$98
        sta     ent_flags               ; facing left, palette 2
; --- set BG palette to black, copy sprite palette down ---
        ldy     #$0F
code_82D2:  lda     #$0F
        sta     $0620,y                 ; BG palette = all black ($0F)
        lda     $0630,y                 ; sprite palette
        sta     $0610,y                 ; copy to BG sub-palette area
        dey
        bpl     code_82D2
        sty     palette_dirty           ; mark palette dirty (Y=$FF)
; ===========================================================================
; Phase 3 Main Loop: Proto Man confrontation sequence
; ===========================================================================
; State machine controlled by ent_status low nibble and ent_x_scr:
;   ent_x_scr != 0 → done, jump to Doc Robot stage entry
;   status & $0F == 0 → Mega Man descending to Y=$60
;   status & $0F != 0 → Proto Man sequence (appear, whistle, depart)
; ===========================================================================
code_82E2:  lda     ent_x_scr
        beq     code_82EA
        jmp     code_8439               ; phase 3 complete → Doc Robot stage

code_82EA:  lda     ent_status
        and     #$0F                    ; check sub-state
        beq     code_82F4               ; 0 = still descending
        jmp     code_83A8               ; nonzero = Proto Man sequence
; --- Mega Man descending to Y=$60 ---
code_82F4:  lda     ent_y_px
        cmp     #$60                    ; reached target Y?
        beq     code_8301               ; yes — transition
        dec     ent_y_px                ; move up 1 pixel per frame
        jmp     code_83E7               ; continue with movement
; --- at Y=$60: change animation and spawn Proto Man ---
code_8301:  lda     #$5E
        cmp     ent_anim_id             ; already changed anim?
        bcs     code_8344               ; yes — skip spawn
; --- change MM to standing anim, spawn Proto Man (entity 1) ---
        lda     #$5D
        ldx     #$00
        jsr     reset_sprite_anim                   ; MM anim $5D (standing on Rush)
        lda     ent_status              ; copy MM's entity properties to entity 1
        sta     $0301                   ; ent_status[1]
        lda     ent_flags
        sta     $0581                   ; ent_flags[1]
        lda     ent_x_scr
        sta     $0381                   ; ent_x_scr[1]
        lda     ent_y_scr
        sta     $03E1                   ; ent_y_scr[1]
        lda     ent_x_px
        sta     $0361                   ; ent_x_px[1] = same X as MM
        lda     ent_y_px
        sta     $03C1                   ; ent_y_px[1] = same Y as MM
        ldx     #$01
        lda     #$5C                    ; Proto Man walking anim
        jsr     reset_sprite_anim                   ; reset entity 1 sprite animation
        lda     #$5C
        sta     $0501                   ; ent_timer[1] = $5C (countdown)
        lda     #$B4
        sta     $0521                   ; ent_var1[1] = $B4 (approach timer)
; --- Proto Man approach / whistle / depart state machine ---
; ent_timer[1] ($0501) counts down: $5C..$22 = walking toward MM,
; $21..$11 = standing (also moving Y), $10..$01 = whistle pause,
; $00 = whistle done, Proto Man rises, then departs.
code_8344:  lda     $0501               ; Proto Man timer
        cmp     #$21
        bcs     code_8352               ; >= $21: still approaching
        cmp     #$11
        bcc     code_835C               ; < $11: whistle / departure phase
        dec     $03C1                   ; $11-$20: Proto Man rises (dec Y)
code_8352:  lda     #$00
        sta     $95                     ; reset frame counter
        dec     $0501                   ; decrement timer
        jmp     code_83EA
; --- whistle / departure phase ---
code_835C:  lda     $0521               ; ent_var1[1] = whistle delay
        beq     code_8375               ; delay expired
        dec     $0521
        lda     $0521
        cmp     #$78                    ; at $78: change to whistle anim
        bne     code_8372
        ldx     #$00
        lda     #$5E                    ; anim $5E = Proto Man whistling
        jsr     reset_sprite_anim
code_8372:  jmp     code_83EA
; --- Proto Man departure: prepare MM for flight ---
code_8375:  lda     #$00
        sta     $95
        inc     $03C1                   ; move Proto Man down 1 px
        dec     $0501                   ; continue counting
        bne     code_83EA
        ldx     #$00
        stx     $0301                   ; deactivate Proto Man (entity 1)
        lda     #$5F
        jsr     reset_sprite_anim                   ; MM anim $5F = riding Rush Jet
        inc     ent_status              ; advance sub-state (start flight)
        lda     #$F0
        sta     $0521                   ; flight countdown timer
        lda     #$10
        sta     ent_timer               ; movement phase duration
        lda     #$00
        sta     ent_xvel_sub
        sta     ent_var1                ; reset movement phase
        lda     #$04
        sta     ent_xvel                ; horizontal speed = 4
        jmp     code_83EA

; --- flight departure: accelerate upward then fly off-screen ---
code_83A8:  lda     $0521               ; flight countdown
        beq     code_83CC               ; countdown done
        dec     $0521
        lda     ent_yvel_sub            ; accelerate upward
        clc
        adc     #$10
        sta     ent_yvel_sub
        lda     ent_yvel
        adc     #$00
        sta     ent_yvel
        cmp     #$04                    ; cap vertical speed at 4
        bne     code_83EA
        lda     #$00
        sta     ent_yvel_sub            ; clamp sub-pixel
        beq     code_83EA
; --- after countdown: fly left off screen ---
code_83CC:  lda     ent_var1
        cmp     #$01                    ; phase 1 = fly left
        bne     code_83E7
        lda     ent_x_px
        sec
        sbc     #$04                    ; move left 4 px/frame
        sta     ent_x_px
        lda     ent_x_scr
        sbc     #$00                    ; borrow into screen position
        sta     ent_x_scr              ; when this wraps, phase 3 ends
        jmp     code_83EA

code_83E7:  jsr     code_861B           ; apply sinusoidal X movement
; --- star field rendering and frame processing ---
; Scrolls 10 star sprites vertically based on ent_yvel (vertical speed).
; Stars flicker by alternating OAM start position each frame.
code_83EA:  lda     ent_var2            ; star scroll accumulator
        clc
        adc     ent_yvel                ; add vertical speed
        sta     ent_var2
        ldx     #$00
code_83F6:  lda     L8735,x             ; star Y position (base)
        sta     $0200,x
        lda     L8736,x                 ; star tile
        sta     $0201,x
        lda     L8737,x                 ; star attribute
        sta     $0202,x
        lda     L8738,x                 ; star X position (base)
        clc
        adc     ent_var2                ; add scroll offset
        sta     $0203,x
        inx
        inx
        inx
        inx
        cpx     #$28                    ; 10 star sprites * 4 bytes
        bne     code_83F6
        lda     $95
        and     #$01                    ; alternate OAM start for flicker
        beq     code_8422
        ldx     #$04                    ; offset by one sprite
code_8422:  stx     oam_ptr
        lda     ent_var3
        and     #$0F                    ; every 16 frames
        bne     code_8430
        lda     #SFX_WIND
        jsr     submit_sound_ID                   ; wind SFX $28
code_8430:  jsr     process_frame_yield               ; process frame + yield
        inc     ent_var3                ; increment wind timer
        jmp     code_82E2               ; loop phase 3

; ===========================================================================
; Doc Robot Shadow Man Stage Entry + Cutscene
; ===========================================================================
; Called after intro completes, or directly via $8003 jump.
; Fades to black, plays title music ($36), loads stage $16 nametable
; using bank $0E tile data, then runs the Doc Robot cutscene showing
; which robot master will be fought. Ends by jumping to the selected
; stage's PRG bank.
; ===========================================================================
code_8439:  lda     #$00
        sta     nmi_skip                ; disable NMI
        jsr     fade_palette_in                   ; fade palette to black
        lda     #MUSIC_DOC_ROBOT
        jsr     submit_sound_ID_D9                   ; submit music $36 (Doc Robot theme)
        lda     #$04
        sta     oam_ptr
        jsr     prepare_oam_buffer                   ; prepare OAM buffer
        jsr     clear_entity_table                   ; clear entity table
        jsr     task_yield                   ; wait for NMI
        lda     #$16
        sta     stage_id                ; stage $16 = Doc Robot Shadow Man
        lda     #$00
        sta     $70                     ; nametable column counter
        lda     #$0E
        sta     prg_bank                ; bank $0E for stage tile data
        jsr     select_PRG_banks                   ; select PRG banks
        lda     #$00
        jsr     metatile_column_ptr_by_id                   ; init metatile columns
; --- fill nametable progressively ---
code_8466:  lda     #$00
        sta     $10
        jsr     fill_nametable_progressive                   ; draw one nametable column
        jsr     task_yield                   ; wait for NMI
        lda     $70
        bne     code_8466               ; loop until complete
; --- load palette, CHR banks, and OAM scenery ---
        ldy     #$1F
code_8476:  lda     L8695,y             ; Doc Robot stage palette
        sta     $0620,y
        dey
        bpl     code_8476
        ldy     #$05
code_8481:  lda     L86C1,y             ; Doc Robot CHR bank assignments
        sta     $E8,y
        dey
        bpl     code_8481
        jsr     update_CHR_banks                   ; update CHR banks via MMC3
; --- load background sprite decoration ---
        ldy     #$27
code_848F:  lda     L86F1,y             ; 40 bytes of OAM sprites (scenery)
        sta     $0200,y
        dey
        bpl     code_848F
        jsr     clear_entity_table                   ; clear entity table
        jsr     task_yield                   ; wait for NMI
        jsr     fade_palette_out                   ; fade palette out (reveal scene)
; --- load robot master portrait position sprites ---
        ldy     #$13
code_84A3:  lda     L8719,y             ; 5 robot master icon positions
        sta     $0228,y                 ; OAM: Y, tile, attr, X per icon
        dey
        bpl     code_84A3
; --- set up Doc Robot entity (entity 0) ---
        lda     #$80
        sta     ent_status              ; active
        sta     ent_flags               ; facing right
        lda     #$00
        sta     ent_anim_frame
        sta     ent_anim_state
        sta     ent_x_scr
        sta     ent_y_scr
        lda     #$60
        sta     ent_x_px                ; X = $60
        lda     #$24
        sta     ent_y_px                ; Y = $24 (top of screen)
        lda     #$7B
        sta     ent_anim_id             ; anim $7B = Doc Robot
        lda     #$00
        sta     $95                     ; frame counter
        lda     #$20
        sta     $10                     ; palette flash value
; --- Doc Robot palette flash intro (48 frames) ---
code_84D9:  lda     $95
        and     #$07                    ; every 8 frames
        bne     code_84EC
        lda     $10
        sta     $0610                   ; set BG palette color
        inc     palette_dirty
        lda     $10
        eor     #$2F                    ; toggle between $20 and $0F (flash)
        sta     $10
code_84EC:  jsr     task_yield               ; wait for NMI
        inc     $95
        lda     $95
        cmp     #$30                    ; 48 frames of flashing
        bcc     code_84D9
; --- draw all robot master icons except the selected one ---
        jsr     code_854B               ; place robot master icon sprites
        lda     #$00
        sta     nmi_skip
; --- wait 60 frames (showing all icons) ---
        lda     #$3C                    ; 60 frame delay
code_8500:  pha
        jsr     code_8525               ; animate Doc Robot + process frame
        pla
        sec
        sbc     #$01
        bne     code_8500               ; loop for 60 frames
; --- reveal selected robot master icon one sprite at a time ---
        jsr     code_8591               ; animate reveal of selected boss
; --- wait another 60 frames ---
        lda     #$3C
code_850F:  pha
        jsr     code_8525               ; animate Doc Robot + process frame
        pla
        sec
        sbc     #$01
        bne     code_850F               ; loop for 60 frames
; --- transition to selected stage ---
        lda     $75                     ; Doc Robot boss selection index
        clc
        adc     #$0C                    ; stage bank = $0C + boss index
        sta     stage_id
        sta     prg_bank
        jmp     select_PRG_banks                   ; select PRG banks and run stage

; ===========================================================================
; Subroutine: Process one frame with Doc Robot animation
; ===========================================================================
; Saves registers, sets OAM pointer, animates the selected robot master
; icon blinking (tile alternates between $02 and $03 every 8 frames),
; then processes frame and yields. Skips blink if boss >= 4.
; ===========================================================================
code_8525:  txa
        pha
        tya
        pha
        lda     #$C8
        sta     oam_ptr                 ; OAM start position
        ldy     $75                     ; boss selection index
        cpy     #$04
        bcs     code_8543               ; skip blink for bosses 4-5
        ldx     L875E,y                 ; OAM offset for this boss's icon
        lda     $95
        lsr     a
        lsr     a
        lsr     a
        and     #$01                    ; toggle every 8 frames
        clc
        adc     #$02                    ; tile $02 or $03 (blink)
        sta     $0202,x                 ; update icon tile
code_8543:  jsr     process_frame_yield               ; process frame + yield
        pla
        tay
        pla
        tax
        rts

; ===========================================================================
; Subroutine: Draw all robot master icons (except selected one)
; ===========================================================================
; Iterates through boss indices 0-5, skipping the one matching $75
; (the selected boss). Each boss's icon is a group of OAM sprites
; defined in L8774 (Y, tile, attr, X quads). Sprite data index and
; count come from L8762/L8768 tables.
; ===========================================================================
code_854B:  lda     #$40
        sta     $10                     ; OAM write cursor (starts at $40)
        lda     #$00
        sta     ent_timer               ; boss loop counter
code_8554:  ldy     ent_timer
        cpy     $75                     ; is this the selected boss?
        beq     code_8590               ; yes — skip it (will be revealed later)
        ldx     L8762,y                 ; sprite data offset for this boss
        lda     L8768,y                 ; sprite count for this boss
        sta     $00
        ldy     $10                     ; OAM cursor
code_8565:  lda     L8774,x             ; sprite Y
        sta     $0200,y
        lda     L8775,x                 ; sprite tile
        sta     $0201,y
        lda     L8776,x                 ; sprite attribute
        sta     $0202,y
        lda     L8777,x                 ; sprite X
        sta     $0203,y
        inx
        inx
        inx
        inx                             ; advance sprite data pointer
        iny
        iny
        iny
        iny                             ; advance OAM cursor
        sty     $10
        dec     $00                     ; more sprites for this boss?
        bpl     code_8565
        inc     ent_timer               ; next boss
        bne     code_8554
code_8590:  rts

; ===========================================================================
; Subroutine: Reveal selected robot master icon (animated)
; ===========================================================================
; Draws the selected boss's icon sprites one at a time with a sound
; effect ($1C) and 4-frame delay between each sprite, creating a
; sequential reveal animation. If boss 5 is selected, spawns a second
; Doc Robot entity.
; ===========================================================================
code_8591:  ldy     ent_timer           ; selected boss index
        ldx     L8762,y                 ; sprite data offset
        lda     L8768,y                 ; sprite count
        sta     $0F
        lda     L876E,y                 ; OAM base offset for this boss
        tay
code_85A0:  lda     L8774,x             ; copy one sprite to OAM
        sta     $0240,y                 ; Y pos (at $0240+ for reveal area)
        lda     L8775,x
        sta     $0241,y                 ; tile
        lda     L8776,x
        sta     $0242,y                 ; attribute
        lda     L8777,x
        sta     $0243,y                 ; X pos
        inx
        inx
        inx
        inx
        iny
        iny
        iny
        iny
        sty     $10
        lda     #SFX_HP_FILL
        jsr     submit_sound_ID                   ; reveal SFX $1C
        jsr     code_8525               ; wait 4 frames (with Doc Robot anim)
        jsr     code_8525
        jsr     code_8525
        jsr     code_8525
        dec     $0F                     ; more sprites to reveal?
        bpl     code_85A0
; --- special case: boss 5 spawns second Doc Robot entity ---
        lda     $75
        cmp     #$05
        bne     code_8602               ; not boss 5, skip
        lda     #$80
        sta     $0301                   ; ent_status[1] = active
        sta     $0581                   ; ent_flags[1] = active
        lda     #$00
        sta     $0381                   ; ent_x_scr[1]
        sta     $03E1                   ; ent_y_scr[1]
        sta     $05E1                   ; ent_anim_frame[1]
        sta     $05A1                   ; ent_anim_state[1]
        lda     #$7B
        sta     $05C1                   ; ent_anim_id[1] = Doc Robot
        lda     #$60
        sta     $0361                   ; ent_x_px[1] = $60
        lda     #$4C
        sta     $03C1                   ; ent_y_px[1] = $4C
code_8602:  rts

; ===========================================================================
; Subroutine: Move entity 0 upward and apply sinusoidal X velocity
; ===========================================================================
; code_8603: Decrements Y by 1 per frame. Falls through to code_861B
;            when Y >= $60 (still on screen).
; code_861B: Applies X velocity with sub-pixel precision, then adjusts
;            X velocity using a 4-phase acceleration table (L872D/L8731)
;            to create a sinusoidal left-right weaving motion.
;            Each phase lasts $10 frames, cycling through 4 directions.
; ===========================================================================
code_8603:  lda     ent_y_px
        sec
        sbc     #$01                    ; move up 1 pixel
        sta     ent_y_px
        lda     ent_y_scr
        sbc     #$00                    ; borrow into screen
        sta     ent_y_scr
        lda     ent_y_px
        cmp     #$60                    ; still below Y=$60?
        bcs     code_8654               ; yes — skip X movement
; --- apply X velocity with sub-pixel precision ---
code_861B:  lda     ent_x_sub
        clc
        adc     ent_xvel_sub            ; add velocity sub-pixel
        sta     ent_x_sub
        lda     ent_x_px
        adc     ent_xvel                ; add velocity whole pixel
        sta     ent_x_px
; --- update X velocity (sinusoidal acceleration) ---
        lda     ent_var1                ; movement phase (0-3)
        and     #$03
        tay
        lda     ent_xvel_sub
        clc
        adc     L872D,y                 ; acceleration sub-pixel
        sta     ent_xvel_sub
        lda     ent_xvel
        adc     L8731,y                 ; acceleration whole: FF,FF,00,00
        sta     ent_xvel
        dec     ent_timer               ; phase duration countdown
        bne     code_8654
        inc     ent_var1                ; next phase
        lda     #$10
        sta     ent_timer               ; reset phase timer ($10 frames)
code_8654:  .byte   $60                 ; RTS ($60 opcode)
; ===========================================================================
; Data Tables
; ===========================================================================

; --- Phase 1 palette: mountain top / sunset sky (32 bytes) ---
L8655:  .byte   $0F,$20,$2C,$1C,$0F,$1C,$27,$16
        .byte   $0F,$3B,$2B,$1B,$0F,$32,$22,$12
        .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
        .byte   $0F,$35,$25,$15,$0F,$0F,$30,$11
; --- Phase 2 palette: night sky / flying on Rush (32 bytes) ---
L8675:  .byte   $0F,$20,$1B,$0B,$0F,$1C,$11,$01
        .byte   $0F,$28,$18,$08,$0F,$04,$13,$03
        .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$37
        .byte   $0F,$0F,$2C,$11,$0F,$0F,$30,$27
; --- Doc Robot stage palette (32 bytes) ---
L8695:  .byte   $0F,$20,$27,$17,$0F,$3B,$2A,$1A
        .byte   $0F,$3C,$2C,$1C,$0F,$33,$23,$17
        .byte   $0F,$2A,$27,$17,$0F,$3C,$2C,$1C
        .byte   $0F,$0F,$30,$37,$0F,$0F,$30,$27
; --- CHR bank assignments (6 bytes each: banks for $0000-$17FF) ---
; Phase 1 CHR banks
L86B5:  .byte   $78,$7A,$00,$01,$1B,$3B
; Phase 2 CHR banks
L86BB:  .byte   $50,$52,$39,$25,$36,$17
; Doc Robot stage CHR banks
L86C1:  .byte   $70,$72,$09,$39,$36,$35
; --- Phase 1 sunset palette cycle (6 steps x 3 bytes) ---
L86C7:  .byte   $1C,$27,$16,$0F,$1C,$1A,$16,$0F
        .byte   $0F,$0F,$1A,$16,$17,$0F,$0F,$1A
        .byte   $16,$0F
; --- Phase 2 night sky palette flash (2 steps x 3 bytes) ---
L86D9:  .byte   $1C,$11,$01,$11,$1C,$01
; --- Star twinkle colors (4 brightness levels) ---
L86DF:  .byte   $04,$14,$0F,$0F
; --- Phase 1 entity init: anim ID, X, Y for entities 0-1 ---
L86E3:  .byte   $13,$60                 ; anim: $13=MM falling, $60=Rush run
L86E5:  .byte   $D8,$58                 ; x: $D8=Mega Man, $58=Rush
L86E7:  .byte   $00,$A4                 ; y: $00=top (MM), $A4=ground (Rush)
; --- Phase 1 mountain decoration OAM sprites (2 sprites x 4 bytes) ---
L86E9:  .byte   $68,$BE,$02,$18,$68,$BF,$02,$20
; --- Doc Robot stage scenery OAM sprites (10 sprites x 4 bytes) ---
L86F1:  .byte   $40,$9D,$01,$70,$B8,$9B,$00,$C0
        .byte   $88,$97,$00,$78,$88,$98,$00,$80
        .byte   $90,$99,$00,$78,$90,$9A,$00,$80
        .byte   $48,$9C,$01,$80,$68,$9C,$01,$78
        .byte   $68,$9C,$01,$80,$68,$9C,$01,$88
; --- Doc Robot cutscene: robot master portrait position sprites ---
; 5 icon positions (Y, tile, attr, X) at OAM offset $0228
L8719:  .byte   $C8,$0F,$03,$D8,$A0,$0F,$03,$C8
        .byte   $78,$0F,$03,$C8,$58,$0F,$03,$B0
        .byte   $30,$0F,$03,$88
; --- Sinusoidal X acceleration table (4 phases) ---
; Phase 0,1: decelerate (sub=$C0, whole=$FF = -$40)
; Phase 2,3: accelerate (sub=$40, whole=$00 = +$40)
L872D:  .byte   $C0,$C0,$40,$40         ; sub-pixel acceleration per phase
L8731:  .byte   $FF,$FF,$00,$00         ; whole-pixel acceleration per phase
; --- Star field sprite data (10 stars x 4 bytes: Y, tile, attr, X) ---
; Each star is tile $F1, attribute $00. X values are offset by scroll.
L8735:  .byte   $78                     ; star 0 Y
L8736:  .byte   $F1                     ; star 0 tile
L8737:  .byte   $00                     ; star 0 attr
L8738:  .byte   $10,$18,$F1,$00,$20,$D0,$F1,$00
        .byte   $30,$38,$F1,$00,$48,$A0,$F1,$00
        .byte   $68,$18,$F1,$00,$98,$60,$F1,$00
        .byte   $B8,$B8,$F1,$00,$D0,$28,$F1,$00
        .byte   $E0,$88,$F1,$00,$E8,$28
; --- OAM offsets for boss icon blink animation (bosses 0-3) ---
L875E:  .byte   $2C,$30,$34,$38
; --- Robot master icon sprite data indices and counts ---
L8762:  .byte   $00,$18,$28,$40,$64,$7C ; sprite data offset per boss (into L8774)
L8768:  .byte   $05,$03,$05,$08,$05,$03 ; sprite count per boss (0-indexed)
; --- OAM base offset for reveal animation per boss ---
L876E:  .byte   $00,$18,$28,$40,$64,$7C
; --- Robot master icon sprite data (Y, tile, attr, X quads) ---
; 6 bosses, variable sprite counts. Used by code_854B and code_8591.
L8774:  .byte   $C0
L8775:  .byte   $7F
L8776:  .byte   $03
L8777:  .byte   $D8,$B8,$7F,$03,$D8,$B0,$7F,$03
        .byte   $D8,$A8,$7F,$03,$D8,$A0,$7D,$03
        .byte   $D8,$A0,$7E,$03,$D0,$98,$7F,$03
        .byte   $C8,$90,$7F,$03,$C8,$88,$7F,$03
        .byte   $C8,$80,$7F,$03,$C8,$70,$7F,$03
        .byte   $C8,$68,$7F,$03,$C8,$60,$7F,$03
        .byte   $C8,$58,$7D,$03,$C8,$58,$7E,$03
        .byte   $C0,$58,$7E,$03,$B8,$50,$7F,$03
        .byte   $B0,$48,$7F,$03,$B0,$40,$7F,$03
        .byte   $B0,$38,$7F,$03,$B0,$30,$7D,$03
        .byte   $B0,$30,$7E,$03,$A8,$30,$7E,$03
        .byte   $A0,$30,$7E,$03,$98,$30,$7E,$03
        .byte   $90,$28,$7F,$03,$88,$20,$7D,$03
        .byte   $88,$20,$7E,$03,$80,$20,$7E,$03
        .byte   $78,$20,$7E,$03,$70,$20,$7E,$03
        .byte   $68,$2C,$7F,$03,$5C,$34,$7F,$03
        .byte   $5C,$3C,$7F,$03,$5C,$44,$7F,$03
; =============================================================================
; Doc Robot Shadow Man Stage Data ($8800-$9FFF)
; =============================================================================
; Metatile column data, enemy/object spawn lists, tile attribute maps,
; and stage layout data for the Doc Robot Shadow Man stage.
; This data is accessed when stage_id = $16 (bank pair $0B at $A000).
; The data fills the remainder of this 8KB bank.
; =============================================================================
        .byte   $5C,$80,$24,$00,$60,$08,$46,$00
        .byte   $20,$02,$14,$00,$00,$88,$00,$00
        .byte   $A1,$00,$01,$00,$28,$00,$08,$00
        .byte   $A0,$00,$22,$08,$00,$02,$80,$00
        .byte   $00,$00,$00,$00,$08,$00,$01,$00
        .byte   $00,$00,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$04,$20,$00,$00
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $01,$00,$80,$00,$08,$00,$04,$00
        .byte   $01,$00,$00,$00,$08,$00,$40,$00
        .byte   $01,$00,$00,$28,$40,$00,$00,$20
        .byte   $00,$00,$00,$00,$21,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$80,$00,$00,$00,$80,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$40,$00
        .byte   $02,$00,$00,$00,$00,$00,$40,$02
        .byte   $00,$00,$00,$08,$00,$00,$00,$00
        .byte   $10,$00,$01,$00,$00,$02,$00,$00
        .byte   $00,$00,$00,$00,$08,$00,$00,$00
        .byte   $00,$20,$44,$00,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $00,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$08,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$08,$20
        .byte   $00,$08,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$01,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$80,$00,$00,$00,$80
        .byte   $00,$00,$10,$00,$40,$20,$20,$00
        .byte   $00,$00,$00,$00,$20,$08,$00,$00
        .byte   $00,$20,$04,$00,$02,$00,$00,$80
        .byte   $00,$00,$08,$00,$04,$00,$00,$02
        .byte   $8C,$00,$00,$00,$00,$10,$08,$00
        .byte   $10,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $82,$00,$00,$00,$00,$00,$00,$00
        .byte   $01,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$80,$00,$20,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$08,$10,$00,$00,$00,$00,$00
        .byte   $04,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$A8,$00,$00,$00,$00,$00,$02
        .byte   $20,$00,$00,$00,$00,$00,$41,$80
        .byte   $21,$00,$08,$00,$00,$00,$00,$80
        .byte   $00,$08,$00,$00,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$02,$00,$00
        .byte   $00,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$00,$00,$00,$40,$00,$04,$00
        .byte   $00,$00,$00,$00,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$08,$08,$08,$00
        .byte   $00,$00,$00,$00,$00,$00,$05,$00
        .byte   $02,$00,$00,$00,$08,$00,$00,$00
        .byte   $11,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$02,$00,$00,$00
        .byte   $00,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$48
        .byte   $00,$00,$01,$02,$03,$04,$05,$06
        .byte   $07,$08,$09,$0A,$0B,$0C,$0D,$0E
        .byte   $0F,$10,$11,$12,$13,$14,$15,$16
        .byte   $17,$18,$19,$08,$00,$00,$00,$02
        .byte   $08,$00,$00,$20,$21,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$0D,$5A,$5A,$5A,$00,$00,$00
        .byte   $00,$17,$5A,$5A,$00,$00,$00,$00
        .byte   $00,$40,$40,$40,$61,$40,$66,$60
        .byte   $20,$20,$22,$23,$20,$20,$20,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $00,$00,$00,$00,$20,$02,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$0B,$00,$08,$00,$08,$00,$00
        .byte   $00,$0F,$00,$17,$00,$30,$00,$13
        .byte   $00,$00,$00,$0F,$00,$FF,$00,$00
        .byte   $00,$5C,$5E,$0F,$36,$06,$01,$0F
        .byte   $26,$16,$06,$0F,$0C,$09,$01,$0F
        .byte   $30,$28,$07,$8A,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$00,$02,$00,$40,$00
        .byte   $04,$00,$01,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$02
        .byte   $01,$00,$00,$00,$08,$00,$24,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$08,$00,$80,$00
        .byte   $00,$09,$0C,$00,$02,$02,$00,$00
        .byte   $00,$10,$18,$FF,$02,$00,$00,$00
        .byte   $00,$03,$04,$04,$05,$06,$07,$07
        .byte   $07,$08,$08,$08,$08,$08,$08,$09
        .byte   $09,$09,$0A,$0A,$0A,$0A,$0A,$0B
        .byte   $0B,$0B,$0B,$0B,$0B,$0C,$0C,$0D
        .byte   $0D,$0F,$10,$11,$11,$12,$13,$13
        .byte   $14,$14,$14,$14,$15,$15,$15,$15
        .byte   $15,$16,$16,$16,$16,$17,$19,$FF
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$00,$04,$00,$01,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$00,$10,$00
        .byte   $00,$00,$40,$00,$00,$08,$00,$00
        .byte   $00,$20,$08,$00,$00,$00,$02,$00
        .byte   $30,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$08,$00
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $00,$20,$00,$00,$00,$88,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$12,$80
        .byte   $80,$00,$00,$00,$00,$20,$80,$02
        .byte   $00,$00,$02,$00,$00,$00,$00,$80
        .byte   $00,$00,$00,$08,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$20
        .byte   $01,$00,$00,$00,$00,$00,$00,$00
        .byte   $40,$00,$00,$00,$00,$00,$02,$02
        .byte   $00,$21,$35,$00,$00,$00,$04,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$40,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$90,$20,$80,$58,$E8,$B0,$D0
        .byte   $F0,$30,$70,$78,$98,$C8,$D0,$88
        .byte   $88,$E8,$30,$70,$88,$B0,$F0,$30
        .byte   $30,$70,$70,$88,$B0,$74,$CC,$68
        .byte   $D8,$C0,$98,$18,$F4,$74,$58,$D8
        .byte   $14,$38,$A8,$B4,$18,$68,$8C,$B8
        .byte   $E4,$88,$98,$A8,$B8,$A8,$C0,$FF
        .byte   $00,$00,$01,$00,$01,$00,$00,$00
        .byte   $04,$00,$01,$00,$00,$20,$00,$00
        .byte   $00,$00,$00,$20,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$40,$04,$00
        .byte   $00,$00,$00,$20,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $00,$A0,$10,$40,$00,$00,$40,$41
        .byte   $00,$00,$00,$40,$00,$02,$00,$40
        .byte   $00,$20,$00,$08,$40,$04,$80,$08
        .byte   $00,$00,$00,$01,$00,$00,$41,$00
        .byte   $10,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$20,$00,$00,$00,$80
        .byte   $00,$00,$41,$00,$01,$00,$00,$00
        .byte   $01,$00,$00,$41,$00,$00,$01,$00
        .byte   $40,$00,$10,$40,$04,$31,$01,$00
        .byte   $00,$0A,$00,$00,$00,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$10,$0A
        .byte   $40,$14,$00,$01,$01,$10,$01,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$10
        .byte   $05,$00,$00,$10,$00,$01,$00,$0C
        .byte   $00,$00,$01,$01,$00,$00,$00,$00
        .byte   $00,$44,$00,$10,$00,$01,$00,$00
        .byte   $01,$00,$01,$00,$00,$94,$00,$00
        .byte   $00,$00,$00,$00,$00,$02,$00,$00
        .byte   $01,$00,$00,$20,$40,$80,$00,$00
        .byte   $00,$98,$B8,$38,$A8,$B8,$98,$68
        .byte   $38,$98,$88,$20,$58,$B4,$48,$18
        .byte   $34,$34,$78,$78,$10,$88,$B8,$68
        .byte   $98,$58,$88,$10,$68,$94,$74,$58
        .byte   $B8,$B0,$B8,$B0,$90,$70,$04,$04
        .byte   $B8,$04,$04,$B8,$04,$04,$B8,$04
        .byte   $B8,$BC,$BC,$BC,$BC,$60,$B0,$FF
        .byte   $50,$00,$00,$20,$01,$00,$00,$00
        .byte   $00,$21,$00,$20,$40,$04,$00,$00
        .byte   $00,$12,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$44,$11,$10,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$01,$44,$08,$01,$0C,$00,$08
        .byte   $40,$20,$44,$20,$00,$00,$00,$00
        .byte   $10,$00,$40,$10,$00,$00,$00,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$00,$08,$00,$00,$00,$08
        .byte   $00,$04,$00,$00,$10,$20,$00,$88
        .byte   $00,$00,$01,$00,$00,$00,$00,$00
        .byte   $04,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$30,$00,$40
        .byte   $00,$80,$04,$14,$00,$00,$00,$00
        .byte   $00,$08,$00,$08,$00,$00,$45,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $05,$08,$00,$00,$10,$01,$00,$00
        .byte   $00,$00,$00,$81,$01,$00,$00,$02
        .byte   $00,$00,$00,$40,$00,$40,$00,$00
        .byte   $00,$01,$00,$02,$00,$20,$04,$00
        .byte   $00,$00,$00,$00,$00,$08,$05,$12
        .byte   $00,$36,$36,$36,$13,$36,$36,$36
        .byte   $36,$5A,$5A,$3A,$5A,$37,$5A,$3A
        .byte   $37,$37,$5A,$5A,$3A,$5A,$5A,$5A
        .byte   $5A,$5A,$5A,$3A,$5A,$37,$37,$36
        .byte   $36,$6B,$50,$03,$03,$03,$38,$38
        .byte   $26,$38,$38,$26,$38,$38,$26,$38
        .byte   $26,$53,$53,$53,$53,$5C,$6F,$FF
        .byte   $00,$00,$00,$00,$00,$A0,$04,$08
        .byte   $14,$10,$00,$00,$10,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$81,$00,$02
        .byte   $00,$04,$00,$14,$00,$00,$00,$08
        .byte   $00,$00,$00,$01,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$41,$00,$10,$00
        .byte   $01,$00,$00,$00,$04,$08,$00,$00
        .byte   $00,$80,$00,$20,$10,$08,$00,$00
        .byte   $00,$00,$10,$22,$00,$48,$00,$A1
        .byte   $50,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$00,$08
        .byte   $40,$40,$01,$80,$00,$20,$04,$20
        .byte   $00,$00,$41,$00,$10,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$01,$00,$20,$00,$00,$00,$00
        .byte   $00,$01,$00,$10,$04,$80,$00,$80
        .byte   $00,$00,$00,$20,$40,$60,$00,$00
        .byte   $00,$00,$01,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$13,$10,$00,$00,$00
        .byte   $00,$00,$00,$20,$00,$20,$00,$00
        .byte   $00,$01,$00,$00,$40,$08,$00,$28
        .byte   $00,$00,$40,$04,$00,$00,$00,$02
        .byte   $04,$00,$00,$80,$40,$08,$00,$20
        .byte   $00,$00,$01,$02,$03,$01,$02,$03
        .byte   $00,$04,$05,$05,$05,$05,$05,$05
        .byte   $04,$06,$07,$07,$07,$07,$07,$07
        .byte   $06,$08,$09,$09,$09,$09,$09,$09
        .byte   $08,$0A,$09,$09,$09,$09,$09,$09
        .byte   $0A,$00,$09,$09,$09,$09,$09,$09
        .byte   $00,$04,$09,$0B,$0C,$0C,$0D,$09
        .byte   $04,$06,$09,$0E,$0F,$0F,$10,$09
        .byte   $06,$08,$09,$0E,$0F,$0F,$10,$09
        .byte   $08,$0A,$09,$09,$11,$11,$09,$09
        .byte   $0A,$00,$09,$09,$12,$12,$09,$09
        .byte   $00,$04,$09,$09,$09,$09,$09,$09
        .byte   $04,$06,$09,$09,$09,$09,$09,$09
        .byte   $06,$13,$09,$09,$09,$09,$09,$09
        .byte   $13,$14,$15,$16,$0D,$0B,$15,$16
        .byte   $14,$17,$18,$19,$10,$0E,$18,$19
        .byte   $17,$04,$1A,$1B,$10,$0E,$1A,$1B
        .byte   $04,$06,$1C,$1D,$10,$0E,$1C,$1D
        .byte   $06,$13,$01,$03,$10,$0E,$01,$03
        .byte   $13,$14,$12,$12,$09,$0E,$15,$16
        .byte   $14,$1E,$09,$09,$09,$09,$18,$19
        .byte   $1E,$1F,$09,$0B,$15,$16,$20,$21
        .byte   $1F,$08,$09,$0E,$18,$19,$1C,$1D
        .byte   $08,$0A,$09,$0E,$20,$21,$20,$21
        .byte   $0A,$14,$09,$0E,$01,$03,$01,$03
        .byte   $1F,$1E,$09,$22,$23,$12,$23,$12
        .byte   $08,$1F,$09,$24,$25,$09,$26,$09
        .byte   $0A,$08,$09,$27,$28,$29,$26,$09
        .byte   $2A,$0A,$09,$27,$26,$2B,$25,$09
        .byte   $12,$00,$09,$27,$2C,$0C,$2D,$29
        .byte   $09,$04,$2E,$0C,$2F,$30,$31,$0C
        .byte   $15,$06,$32,$1F,$33,$33,$32,$1F
        .byte   $18,$34,$35,$02,$36,$37,$1F,$2A
        .byte   $14,$38,$12,$12,$12,$39,$08,$12
        .byte   $1E,$3A,$3B,$3C,$3D,$3E,$0A,$09
        .byte   $1F,$3F,$40,$41,$42,$43,$2A,$09
        .byte   $08,$44,$45,$46,$47,$48,$12,$09
        .byte   $0A,$09,$49,$4A,$4B,$09,$09,$09
        .byte   $00,$4C,$4D,$3C,$4D,$3C,$4E,$4F
        .byte   $04,$41,$50,$41,$50,$41,$51,$52
        .byte   $06,$36,$53,$4A,$53,$4A,$54,$55
        .byte   $08,$2C,$12,$56,$56,$57,$12,$58
        .byte   $0A,$1E,$09,$59,$5A,$5B,$09,$09
        .byte   $00,$1F,$09,$5C,$5D,$5E,$5F,$09
        .byte   $04,$08,$09,$60,$61,$62,$63,$09
        .byte   $06,$0A,$09,$60,$27,$64,$63,$09
        .byte   $13,$00,$09,$2C,$15,$65,$65,$16
        .byte   $14,$66,$09,$66,$18,$41,$41,$19
        .byte   $66,$67,$09,$67,$68,$69,$6A,$6B
        .byte   $67,$14,$09,$2A,$01,$02,$02,$03
        .byte   $2A,$1E,$09,$56,$56,$57,$6C,$6D
        .byte   $56,$1F,$09,$22,$22,$6E,$09,$28
        .byte   $6F,$08,$09,$59,$5A,$70,$71,$72
        .byte   $09,$0A,$09,$09,$09,$28,$29,$73
        .byte   $09,$00,$15,$65,$4C,$4D,$74,$65
        .byte   $16,$04,$18,$41,$41,$50,$41,$41
        .byte   $19,$67,$75,$76,$76,$76,$77,$2B
        .byte   $70,$2A,$75,$76,$76,$76,$77,$78
        .byte   $28,$12,$75,$76,$76,$76,$77,$09
        .byte   $0C,$09,$75,$76,$76,$76,$77,$79
        .byte   $1F,$09,$75,$76,$76,$76,$77,$66
        .byte   $08,$09,$75,$76,$76,$76,$7A,$7B
        .byte   $0A,$2C,$15,$65,$65,$16,$7C,$7D
        .byte   $00,$66,$18,$41,$41,$19,$33,$33
        .byte   $04,$09,$75,$76,$77,$7E,$5A,$70
        .byte   $71,$09,$75,$76,$77,$6E,$78,$28
        .byte   $29,$09,$75,$76,$77,$70,$71,$72
        .byte   $2B,$09,$75,$76,$77,$28,$29,$73
        .byte   $78,$09,$75,$76,$77,$26,$7F,$73
        .byte   $09,$09,$75,$76,$77,$26,$7F,$73
        .byte   $09,$31,$15,$65,$65,$65,$65,$16
        .byte   $31,$32,$18,$41,$41,$41,$41,$19
        .byte   $32,$80,$5A,$70,$71,$81,$7E,$5A
        .byte   $70,$6E,$78,$28,$29,$09,$6E,$78
        .byte   $28,$15,$65,$65,$16,$15,$65,$65
        .byte   $16,$18,$41,$41,$19,$18,$41,$41
        .byte   $19,$1C,$32,$46,$21,$1C,$32,$46
        .byte   $21,$01,$02,$02,$03,$01,$02,$02
        .byte   $03,$15,$65,$65,$16,$15,$65,$65
        .byte   $16,$18,$41,$41,$19,$18,$41,$41
        .byte   $19,$71,$80,$5A,$70,$71,$80,$5A
        .byte   $70,$29,$6E,$78,$28,$29,$6E,$78
        .byte   $28,$2B,$70,$71,$72,$2B,$70,$71
        .byte   $72,$78,$2D,$29,$73,$78,$2D,$29
        .byte   $73,$09,$09,$2B,$82,$09,$09,$2B
        .byte   $82,$09,$09,$09,$2D,$09,$09,$09
        .byte   $2D,$31,$31,$31,$31,$31,$31,$31
        .byte   $31,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$6F,$75,$76,$76,$76,$76,$77
        .byte   $24,$09,$75,$76,$76,$76,$76,$77
        .byte   $27,$09,$75,$76,$76,$76,$76,$77
        .byte   $27,$09,$75,$76,$76,$76,$76,$15
        .byte   $16,$09,$75,$76,$76,$76,$76,$18
        .byte   $19,$09,$75,$76,$76,$76,$76,$1C
        .byte   $21,$31,$83,$76,$76,$76,$76,$01
        .byte   $03,$32,$32,$76,$76,$76,$76,$15
        .byte   $16,$70,$81,$6E,$09,$84,$09,$09
        .byte   $13,$28,$29,$6E,$78,$84,$09,$09
        .byte   $14,$26,$2B,$70,$71,$85,$09,$78
        .byte   $1E,$26,$78,$28,$29,$86,$09,$09
        .byte   $1F,$0C,$09,$0C,$7F,$86,$09,$87
        .byte   $08,$08,$88,$08,$0C,$86,$0C,$89
        .byte   $0A,$06,$09,$06,$06,$0C,$08,$89
        .byte   $00,$13,$09,$13,$8A,$8A,$8B,$89
        .byte   $04,$0A,$8C,$11,$2A,$2A,$11,$8D
        .byte   $1F,$00,$12,$8E,$8F,$12,$12,$58
        .byte   $08,$66,$09,$27,$28,$90,$91,$09
        .byte   $0A,$1F,$09,$92,$93,$93,$93,$93
        .byte   $2A,$08,$09,$8E,$8F,$12,$12,$12
        .byte   $94,$0A,$09,$27,$28,$90,$91,$09
        .byte   $95,$00,$96,$96,$97,$15,$65,$65
        .byte   $16,$66,$41,$41,$0F,$18,$41,$41
        .byte   $98,$15,$65,$16,$1F,$15,$65,$4C
        .byte   $99,$18,$41,$19,$08,$18,$41,$41
        .byte   $9A,$1C,$46,$21,$0A,$1C,$32,$46
        .byte   $9B,$01,$02,$03,$2A,$01,$02,$36
        .byte   $54,$9C,$9D,$6C,$8F,$9E,$12,$9D
        .byte   $94,$9F,$73,$78,$28,$29,$09,$73
        .byte   $95,$15,$65,$16,$2C,$15,$65,$4C
        .byte   $4E,$18,$41,$19,$66,$18,$41,$41
        .byte   $A0,$A1,$35,$02,$03,$01,$02,$03
        .byte   $1F,$A2,$12,$12,$12,$12,$12,$12
        .byte   $08,$A3,$09,$09,$09,$09,$09,$09
        .byte   $0A,$55,$A4,$09,$A4,$09,$A4,$09
        .byte   $2A,$A5,$09,$09,$09,$09,$09,$09
        .byte   $A6,$9F,$09,$09,$09,$09,$09,$09
        .byte   $95,$A7,$74,$65,$16,$15,$65,$16
        .byte   $2C,$A8,$41,$41,$19,$18,$41,$19
        .byte   $66,$15,$65,$65,$16,$15,$65,$65
        .byte   $16,$18,$41,$41,$19,$18,$41,$41
        .byte   $19,$1C,$A9,$32,$AA,$1C,$A9,$46
        .byte   $21,$01,$02,$02,$03,$01,$02,$02
        .byte   $03,$9C,$9D,$6C,$8F,$12,$9D,$6C
        .byte   $8F,$9F,$73,$78,$28,$29,$73,$78
        .byte   $28,$15,$65,$65,$16,$15,$65,$65
        .byte   $16,$18,$41,$41,$19,$18,$41,$41
        .byte   $19,$15,$65,$16,$15,$65,$16,$1F
        .byte   $1C,$18,$41,$19,$18,$41,$19,$08
        .byte   $1C,$1C,$32,$21,$1C,$32,$21,$0A
        .byte   $01,$01,$02,$03,$01,$02,$03,$2A
        .byte   $8F,$12,$9D,$6C,$8F,$12,$9D,$12
        .byte   $28,$29,$73,$78,$28,$2C,$15,$65
        .byte   $16,$15,$65,$65,$16,$66,$18,$41
        .byte   $19,$18,$41,$41,$19,$1F,$1C,$46
        .byte   $21,$A9,$21,$08,$1C,$46,$21,$0A
        .byte   $2A,$46,$21,$0A,$01,$02,$03,$2A
        .byte   $AB,$02,$03,$2A,$8F,$12,$9D,$12
        .byte   $75,$12,$9D,$12,$28,$15,$16,$09
        .byte   $75,$2C,$15,$65,$16,$18,$19,$09
        .byte   $75,$66,$18,$41,$19,$01,$03,$09
        .byte   $75,$1F,$01,$02,$03,$15,$16,$15
        .byte   $16,$67,$15,$65,$16,$18,$19,$18
        .byte   $19,$AC,$AC,$AD,$AE,$AF,$AD,$B0
        .byte   $AC,$B1,$B1,$B2,$32,$B3,$B2,$B4
        .byte   $B5,$B6,$B6,$B6,$B7,$B8,$B9,$77
        .byte   $75,$BA,$BB,$BC,$BD,$BE,$BF,$77
        .byte   $75,$BD,$BE,$C0,$B6,$76,$B6,$77
        .byte   $75,$32,$76,$C1,$C2,$76,$C2,$77
        .byte   $75,$15,$16,$C3,$2C,$C4,$C5,$C6
        .byte   $C7,$18,$19,$69,$66,$69,$C8,$1D
        .byte   $69,$C9,$AD,$AD,$B0,$AF,$AC,$CA
        .byte   $AC,$CB,$B2,$B2,$CC,$B3,$CD,$AE
        .byte   $B1,$B7,$B8,$CE,$32,$32,$77,$AB
        .byte   $B7,$BD,$BE,$C0,$B6,$B6,$77,$75
        .byte   $BD,$B6,$76,$CF,$BB,$D0,$77,$75
        .byte   $B6,$C2,$76,$BD,$BE,$BF,$77,$75
        .byte   $C2,$2C,$C4,$C5,$C6,$C3,$2C,$C7
        .byte   $2C,$66,$69,$C8,$1D,$D1,$66,$69
        .byte   $66,$AE,$AE,$CA,$CA,$AE,$AE,$AE
        .byte   $AE,$32,$32,$AE,$AE,$32,$32,$32
        .byte   $32,$B8,$B9,$D2,$AB,$B7,$B8,$CE
        .byte   $32,$BE,$BF,$77,$75,$BD,$BE,$C0
        .byte   $B6,$76,$B6,$77,$75,$B6,$76,$CF
        .byte   $BB,$76,$C2,$77,$75,$C2,$76,$BD
        .byte   $BE,$C4,$C5,$C6,$C7,$2C,$C4,$C5
        .byte   $74,$D3,$C8,$1D,$69,$66,$69,$D4
        .byte   $41,$AE,$AE,$AE,$AE,$AE,$AE,$AE
        .byte   $AE,$D2,$12,$12,$12,$12,$12,$12
        .byte   $12,$77,$A4,$09,$A4,$09,$A4,$09
        .byte   $A4,$77,$09,$09,$09,$09,$09,$09
        .byte   $09,$77,$09,$09,$09,$09,$09,$09
        .byte   $09,$77,$09,$09,$09,$09,$09,$09
        .byte   $09,$65,$4C,$4D,$74,$65,$16,$15
        .byte   $65,$41,$41,$50,$41,$41,$19,$18
        .byte   $41,$09,$09,$09,$09,$09,$09,$09
        .byte   $1F,$09,$09,$09,$09,$09,$09,$09
        .byte   $08,$09,$A4,$09,$A4,$09,$A4,$09
        .byte   $0A,$09,$09,$09,$09,$D5,$D6,$09
        .byte   $2A,$09,$09,$09,$09,$D7,$D8,$09
        .byte   $A6,$09,$09,$D9,$88,$1A,$1B,$09
        .byte   $95,$65,$16,$66,$09,$01,$03,$15
        .byte   $65,$41,$19,$1F,$09,$15,$16,$18
        .byte   $41,$01,$35,$02,$03,$01,$02,$36
        .byte   $DA,$15,$65,$65,$16,$15,$65,$4C
        .byte   $DB,$18,$41,$41,$19,$18,$41,$41
        .byte   $9B,$01,$02,$02,$03,$01,$02,$36
        .byte   $54,$9C,$12,$12,$12,$12,$12,$12
        .byte   $94,$9F,$09,$09,$09,$09,$09,$09
        .byte   $95,$4C,$4D,$74,$4C,$4D,$74,$4C
        .byte   $4E,$41,$50,$41,$41,$50,$41,$41
        .byte   $A0,$A1,$35,$02,$03,$01,$02,$36
        .byte   $DA,$A2,$DC,$DD,$56,$DE,$DF,$DD
        .byte   $E0,$A3,$7F,$E1,$E2,$E3,$E4,$E1
        .byte   $E5,$55,$7F,$E6,$5D,$E7,$7F,$E6
        .byte   $E8,$A5,$E9,$E3,$E4,$73,$E9,$E3
        .byte   $EA,$9F,$78,$26,$7F,$73,$78,$26
        .byte   $EB,$A7,$74,$65,$16,$15,$65,$4C
        .byte   $DB,$A8,$41,$41,$19,$18,$41,$41
        .byte   $9B,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$32,$32,$32,$32,$32,$32,$32
        .byte   $32,$1D,$1E,$25,$26,$7C,$7D,$18
        .byte   $05,$6E,$6E,$05,$05,$7E,$7F,$05
        .byte   $1F,$09,$0A,$8E,$8F,$34,$34,$61
        .byte   $61,$86,$87,$11,$12,$69,$69,$63
        .byte   $63,$11,$12,$8E,$8F,$33,$33,$33
        .byte   $33,$19,$1A,$25,$26,$33,$33,$33
        .byte   $96,$01,$02,$09,$0A,$33,$33,$97
        .byte   $33,$33,$96,$33,$96,$11,$12,$11
        .byte   $12,$97,$33,$97,$33,$19,$1A,$2A
        .byte   $2B,$34,$34,$33,$33,$8E,$8F,$19
        .byte   $1A,$25,$26,$1D,$1E,$0C,$04,$6C
        .byte   $65,$04,$0D,$66,$6F,$25,$26,$25
        .byte   $26,$74,$07,$74,$0F,$06,$77,$0E
        .byte   $77,$74,$0F,$74,$00,$0E,$77,$10
        .byte   $77,$74,$00,$74,$08,$00,$77,$00
        .byte   $77,$25,$26,$09,$0A,$8E,$8F,$86
        .byte   $87,$74,$00,$74,$00,$10,$77,$00
        .byte   $77,$33,$48,$33,$48,$5E,$57,$4E
        .byte   $4F,$33,$48,$4C,$40,$4E,$4F,$41
        .byte   $42,$4E,$4F,$4E,$4F,$43,$33,$43
        .byte   $33,$49,$4A,$4E,$4F,$4B,$52,$48
        .byte   $48,$1D,$1E,$2A,$2B,$48,$48,$58
        .byte   $40,$01,$02,$1D,$1E,$49,$4A,$33
        .byte   $33,$39,$39,$04,$04,$80,$81,$8C
        .byte   $8D,$82,$83,$8C,$8D,$03,$03,$00
        .byte   $00,$00,$00,$00,$00,$8A,$8B,$8A
        .byte   $8B,$37,$7C,$37,$18,$7D,$6E,$05
        .byte   $05,$6E,$7E,$05,$05,$7F,$37,$1F
        .byte   $37,$37,$34,$37,$33,$34,$37,$33
        .byte   $37,$3F,$92,$97,$9A,$04,$0C,$96
        .byte   $6C,$04,$04,$65,$66,$0D,$04,$6F
        .byte   $97,$92,$3F,$9A,$96,$97,$9A,$97
        .byte   $9A,$96,$74,$96,$74,$06,$07,$0E
        .byte   $0F,$77,$97,$77,$97,$9A,$96,$9A
        .byte   $96,$33,$9A,$33,$9A,$33,$74,$33
        .byte   $74,$08,$00,$10,$00,$77,$33,$77
        .byte   $33,$9A,$33,$9A,$33,$33,$7C,$33
        .byte   $18,$7D,$7E,$05,$05,$7F,$33,$1F
        .byte   $33,$04,$04,$6D,$66,$0D,$0C,$6F
        .byte   $6C,$0D,$01,$6F,$1D,$02,$92,$1E
        .byte   $9A,$77,$74,$77,$74,$77,$25,$77
        .byte   $25,$26,$9A,$26,$9A,$7F,$7C,$1F
        .byte   $18,$7F,$1D,$1F,$2A,$1E,$33,$2B
        .byte   $33,$34,$56,$33,$48,$55,$57,$43
        .byte   $4F,$34,$33,$33,$33,$33,$48,$33
        .byte   $58,$33,$48,$47,$40,$43,$4F,$41
        .byte   $42,$33,$33,$33,$59,$33,$33,$46
        .byte   $46,$49,$4A,$4D,$4F,$5D,$33,$5B
        .byte   $33,$33,$43,$33,$43,$59,$46,$43
        .byte   $33,$53,$4F,$33,$4F,$5B,$33,$5B
        .byte   $33,$33,$4F,$33,$4F,$04,$04,$6D
        .byte   $6D,$09,$0A,$11,$12,$11,$12,$19
        .byte   $1A,$74,$0E,$74,$00,$0E,$0F,$08
        .byte   $00,$0F,$0E,$00,$00,$0F,$77,$10
        .byte   $77,$56,$56,$58,$40,$5E,$57,$41
        .byte   $42,$43,$4F,$43,$4F,$46,$53,$33
        .byte   $33,$44,$45,$41,$42,$46,$46,$33
        .byte   $33,$4D,$4F,$48,$4F,$48,$4F,$48
        .byte   $4F,$04,$04,$65,$6D,$33,$2F,$33
        .byte   $2F,$16,$17,$17,$16,$2F,$33,$2F
        .byte   $33,$33,$33,$32,$33,$33,$33,$01
        .byte   $02,$01,$02,$80,$81,$8E,$8F,$82
        .byte   $83,$2D,$2E,$8A,$8B,$8A,$8B,$8C
        .byte   $8D,$33,$4F,$4C,$50,$48,$48,$48
        .byte   $48,$53,$4F,$4C,$50,$53,$33,$33
        .byte   $33,$48,$4F,$41,$42,$03,$22,$00
        .byte   $00,$4E,$33,$4E,$33,$4D,$33,$48
        .byte   $33,$48,$33,$48,$33,$70,$92,$37
        .byte   $9A,$04,$04,$34,$34,$37,$9A,$37
        .byte   $9A,$19,$1A,$1D,$1E,$86,$87,$19
        .byte   $1A,$33,$33,$05,$05,$37,$33,$3F
        .byte   $33,$34,$56,$4C,$40,$56,$57,$41
        .byte   $42,$4B,$51,$48,$33,$51,$52,$33
        .byte   $48,$0C,$04,$18,$05,$04,$04,$05
        .byte   $05,$34,$27,$33,$27,$33,$27,$33
        .byte   $27,$04,$04,$00,$00,$0B,$02,$09
        .byte   $0A,$07,$77,$0F,$77,$0D,$8E,$6F
        .byte   $86,$77,$11,$77,$8E,$77,$19,$77
        .byte   $25,$93,$34,$9B,$33,$56,$57,$48
        .byte   $4F,$34,$34,$33,$32,$9B,$33,$9B
        .byte   $33,$77,$09,$77,$11,$8F,$7C,$87
        .byte   $18,$12,$34,$8F,$33,$1A,$33,$26
        .byte   $33,$32,$32,$33,$33,$93,$33,$9B
        .byte   $33,$93,$27,$33,$27,$02,$0C,$1E
        .byte   $6C,$0A,$74,$12,$74,$00,$00,$08
        .byte   $00,$08,$77,$10,$77,$34,$38,$33
        .byte   $2F,$A2,$A2,$AA,$AA,$A2,$AA,$AA
        .byte   $A3,$AA,$AA,$A3,$A3,$AA,$A2,$A2
        .byte   $AA,$AA,$A2,$A3,$AA,$A3,$A3,$00
        .byte   $00,$A3,$00,$00,$00,$AA,$A3,$A3
        .byte   $00,$38,$A3,$2F,$34,$A3,$A3,$34
        .byte   $38,$00,$00,$A4,$A4,$00,$00,$A4
        .byte   $A5,$00,$00,$B4,$B5,$00,$00,$A6
        .byte   $A4,$AC,$AC,$A4,$A5,$AC,$AC,$B4
        .byte   $B5,$AC,$AC,$A6,$A7,$AC,$AD,$00
        .byte   $00,$14,$15,$17,$16,$AE,$AC,$00
        .byte   $00,$AE,$AF,$00,$B7,$00,$BF,$00
        .byte   $00,$AC,$AC,$00,$00,$3E,$00,$06
        .byte   $07,$23,$24,$30,$31,$3E,$0C,$06
        .byte   $6C,$04,$0D,$73,$6F,$33,$2F,$06
        .byte   $07,$0E,$74,$08,$74,$A2,$AA,$AA
        .byte   $A2,$AA,$AA,$A2,$A2,$A3,$AA,$00
        .byte   $A3,$00,$A3,$00,$00,$A3,$A3,$38
        .byte   $34,$00,$00,$A6,$A7,$00,$BF,$A4
        .byte   $A5,$AC,$AC,$A6,$A4,$0F,$0E,$00
        .byte   $08,$38,$34,$2F,$33,$0F,$0F,$00
        .byte   $00,$0E,$74,$00,$74,$33,$33,$0C
        .byte   $04,$33,$33,$04,$0D,$6C,$65,$74
        .byte   $07,$66,$6F,$06,$77,$01,$13,$1D
        .byte   $1E,$7F,$8E,$1F,$86,$0D,$11,$6F
        .byte   $8E,$56,$56,$48,$48,$34,$57,$33
        .byte   $4F,$55,$57,$44,$45,$34,$56,$46
        .byte   $53,$34,$11,$33,$8E,$4C,$50,$43
        .byte   $4F,$47,$40,$33,$33,$41,$42,$49
        .byte   $4A,$33,$33,$4B,$52,$47,$19,$33
        .byte   $25,$43,$4F,$44,$45,$4E,$4F,$4D
        .byte   $4F,$33,$1D,$46,$25,$58,$40,$33
        .byte   $33,$33,$09,$4B,$11,$48,$8E,$48
        .byte   $86,$00,$00,$AC,$AD,$00,$00,$AE
        .byte   $AF,$B4,$B5,$B4,$B5,$B6,$B7,$B6
        .byte   $B7,$BA,$BA,$B8,$B8,$BA,$BD,$B1
        .byte   $B5,$BE,$BF,$B6,$B7,$BA,$BA,$9C
        .byte   $9D,$BA,$BD,$9F,$B5,$BB,$BD,$B1
        .byte   $B5,$BB,$BD,$9F,$B5,$B9,$B9,$06
        .byte   $07,$B9,$0C,$06,$6C,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$8C,$8E,$6B,$05,$06,$08
        .byte   $0A,$00,$A8,$AA,$23,$AC,$05,$28
        .byte   $2A,$00,$34,$47,$8E,$0C,$0E,$2C
        .byte   $2E,$AE,$A4,$A6,$A0,$A2,$9C,$7C
        .byte   $06,$C0,$05,$45,$2C,$2E,$9C,$9E
        .byte   $4E,$C2,$06,$9C,$9E,$00,$99,$9E
        .byte   $45,$4A,$4C,$6E,$6B,$00,$01,$03
        .byte   $E0,$00,$EE,$00,$01,$03,$00,$00
        .byte   $E0,$6B,$30,$32,$6B,$6B,$5B,$55
        .byte   $6B,$6B,$50,$52,$49,$6B,$55,$6B
        .byte   $5B,$5B,$5D,$5D,$55,$6B,$00,$00
        .byte   $00,$6B,$6B,$6B,$5A,$6B,$69,$00
        .byte   $6B,$6B,$79,$7A,$8B,$00,$74,$76
        .byte   $00,$6B,$89,$9A,$00,$72,$76,$00
        .byte   $70,$E2,$00,$00,$74,$82,$00,$00
        .byte   $26,$00,$00,$00,$00,$82,$00,$00
        .byte   $80,$C4,$C6,$C8,$CA,$00,$00,$99
        .byte   $47,$00,$00,$99,$9E,$99,$9E,$99
        .byte   $DB,$00,$00,$5E,$00,$00,$00,$CC
        .byte   $CE,$00,$00,$5E,$6B,$00,$00,$00
        .byte   $00,$00,$00,$36,$EC,$00,$00,$00
        .byte   $00,$00,$00,$36,$00,$F4,$F4,$F5
        .byte   $F8,$00,$00,$00,$00,$00,$00,$00
        .byte   $EA,$00,$00,$00,$00,$00,$00,$00
        .byte   $F6,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$8D,$8F,$6B,$05,$06,$09
        .byte   $0B,$10,$A9,$AB,$8D,$05,$AD,$29
        .byte   $2B,$10,$35,$48,$24,$0D,$0F,$2D
        .byte   $2F,$06,$A5,$A7,$A1,$A3,$7B,$9F
        .byte   $AF,$05,$C1,$46,$2D,$2F,$9D,$9F
        .byte   $4F,$06,$C3,$9D,$9F,$00,$9D,$CB
        .byte   $46,$4B,$4D,$6F,$6B,$00,$02,$04
        .byte   $E1,$00,$EF,$00,$02,$04,$00,$00
        .byte   $E1,$54,$31,$33,$5C,$56,$55,$55
        .byte   $6B,$54,$51,$53,$59,$6B,$58,$5A
        .byte   $6B,$6B,$5D,$65,$66,$67,$00,$00
        .byte   $00,$54,$64,$6B,$6B,$54,$6B,$00
        .byte   $5A,$78,$79,$6B,$8B,$00,$75,$77
        .byte   $00,$98,$89,$6B,$00,$73,$76,$00
        .byte   $71,$E3,$00,$00,$22,$27,$00,$00
        .byte   $81,$00,$00,$00,$00,$83,$00,$00
        .byte   $81,$C5,$C7,$C9,$CB,$00,$00,$35
        .byte   $48,$00,$00,$9D,$CB,$9D,$CB,$DA
        .byte   $48,$00,$00,$5F,$00,$00,$00,$CD
        .byte   $CF,$00,$00,$5F,$6B,$00,$00,$00
        .byte   $00,$00,$00,$37,$ED,$00,$00,$00
        .byte   $00,$00,$00,$37,$00,$F4,$F5,$F4
        .byte   $F9,$00,$00,$00,$00,$00,$00,$00
        .byte   $EB,$00,$00,$00,$00,$00,$00,$00
        .byte   $F7,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$9C,$9E,$44,$15,$16,$18
        .byte   $1A,$20,$B8,$BA,$9C,$BC,$15,$00
        .byte   $00,$00,$34,$47,$9E,$1C,$1E,$1E
        .byte   $1C,$BE,$B4,$B6,$B0,$B2,$9C,$9E
        .byte   $16,$D0,$15,$44,$3C,$3E,$9C,$9E
        .byte   $4E,$D2,$16,$11,$13,$00,$99,$7C
        .byte   $45,$18,$1A,$7E,$6B,$6B,$1E,$1C
        .byte   $E0,$45,$FE,$00,$3C,$3E,$00,$6D
        .byte   $F0,$55,$40,$42,$6B,$6B,$5B,$6B
        .byte   $55,$6B,$60,$62,$6B,$6B,$6B,$6B
        .byte   $5B,$5B,$6B,$6B,$6B,$6B,$6B,$6B
        .byte   $5B,$6B,$6B,$69,$5A,$6B,$5A,$6B
        .byte   $55,$6B,$89,$8A,$6B,$00,$84,$00
        .byte   $00,$6B,$89,$6B,$00,$82,$00,$85
        .byte   $80,$E0,$00,$00,$84,$82,$00,$00
        .byte   $80,$00,$00,$00,$86,$92,$86,$85
        .byte   $90,$D4,$D6,$D8,$9E,$00,$00,$B0
        .byte   $B2,$00,$00,$99,$9E,$A0,$A2,$99
        .byte   $47,$00,$00,$5E,$6B,$00,$00,$DC
        .byte   $DE,$00,$00,$5E,$6B,$00,$00,$00
        .byte   $00,$00,$00,$36,$FC,$E4,$E4,$E5
        .byte   $E8,$00,$00,$38,$00,$00,$00,$00
        .byte   $EA,$00,$00,$00,$00,$F2,$F3,$00
        .byte   $E6,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$9D,$9F,$44,$15,$16,$19
        .byte   $1B,$00,$B9,$BB,$9D,$15,$BD,$21
        .byte   $00,$20,$35,$48,$9F,$1D,$1F,$1F
        .byte   $1D,$16,$B5,$B7,$B1,$B3,$9D,$9F
        .byte   $BF,$15,$D1,$44,$3D,$3F,$9D,$9F
        .byte   $4F,$16,$D3,$12,$14,$00,$7B,$CB
        .byte   $46,$19,$1B,$7F,$6B,$6B,$1F,$1D
        .byte   $E1,$46,$FF,$00,$3D,$3F,$6C,$00
        .byte   $F1,$57,$41,$43,$5C,$5A,$6B,$6B
        .byte   $55,$54,$61,$63,$54,$64,$54,$5A
        .byte   $6B,$55,$6B,$54,$6B,$6B,$5C,$54
        .byte   $6B,$68,$5C,$6B,$6B,$6A,$6B,$5A
        .byte   $5A,$88,$89,$6B,$6B,$00,$00,$00
        .byte   $00,$6B,$89,$6B,$00,$83,$00,$85
        .byte   $81,$E1,$00,$00,$00,$83,$00,$00
        .byte   $81,$00,$00,$00,$87,$93,$85,$87
        .byte   $91,$D5,$D7,$D9,$CB,$00,$00,$B1
        .byte   $B3,$00,$00,$9D,$CB,$A1,$A3,$35
        .byte   $48,$00,$00,$5F,$6B,$00,$00,$DD
        .byte   $DF,$00,$00,$5F,$6B,$00,$00,$00
        .byte   $00,$00,$00,$37,$FD,$E4,$E5,$E4
        .byte   $E9,$00,$00,$39,$00,$00,$00,$00
        .byte   $EB,$00,$00,$00,$00,$F3,$3B,$00
        .byte   $E7,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$13,$13,$02,$13,$13,$01
        .byte   $01,$01,$13,$13,$13,$13,$13,$01
        .byte   $01,$01,$13,$13,$13,$01,$01,$01
        .byte   $01,$13,$13,$13,$13,$13,$13,$13
        .byte   $13,$13,$13,$02,$01,$01,$13,$13
        .byte   $13,$13,$13,$13,$13,$00,$13,$13
        .byte   $02,$01,$01,$02,$02,$02,$01,$01
        .byte   $13,$02,$50,$00,$01,$01,$01,$01
        .byte   $13,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$12,$10,$13,$13
        .byte   $10,$02,$02,$02,$10,$13,$13,$13
        .byte   $13,$13,$10,$10,$13,$13,$10,$10
        .byte   $13,$00,$00,$00,$13,$13,$13,$13
        .byte   $13,$13,$13,$13,$13,$00,$00,$13
        .byte   $13,$00,$00,$13,$13,$13,$13,$13
        .byte   $13,$00,$00,$43,$12,$00,$00,$50
        .byte   $50,$00,$00,$23,$12,$00,$00,$00
        .byte   $00,$00,$00,$13,$53,$02,$02,$02
        .byte   $02,$00,$00,$13,$00,$02,$02,$02
        .byte   $02,$00,$00,$00,$00,$02,$02,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00
; --- end padding ---
        brk
        brk
        brk
