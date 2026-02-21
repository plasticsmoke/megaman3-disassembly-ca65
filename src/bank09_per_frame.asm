; =============================================================================
; MEGA MAN 3 (U) — BANK $09 — PER-FRAME SUBSYSTEMS
; =============================================================================
; Mapped to $8000-$9FFF. Called 7 times every gameplay frame from the fixed
; bank's gameplay_frame_loop. Contains per-frame subsystems for:
;   1. Stage clear entity spawning (falling debris / transition effects)
;   2. Wily 3 entity spawning (scripted enemy placement)
;   3. Palette animation cycling (water, lava, conveyor colors)
;   4. Gemini Man platform animation (stage $05 only)
;   5. Item pickup/drop entity spawning
;   6. Wily 2 camera Y-axis transition
;   7. Doc Robot Gemini stage nametable preparation
;
; Also serves as Doc Robot Gemini stage data bank ($22=$09) when this bank
; is mapped to $A000-$BFFF. Stage layout data occupies $8660-$9FFF.
;
; Key zero-page vars:
;   $22 = stage ID        $55-$57 = Gemini platform state
;   $64-$67 = stage clear state   $68 = item drop state
;   $F8 = scroll mode     $F9 = current screen
;
; Annotation: annotated — labels named, per-frame subsystem logic documented
; =============================================================================

        .setcpu "6502"

; ---------------------------------------------------------------------------
; Fixed bank subroutine imports
; ---------------------------------------------------------------------------
metatile_column_ptr_by_id   := $E8B4   ; set metatile column pointer by ID
queue_metatile_update       := $EEAB   ; build PPU buffer for 4x4 metatile
fill_nametable_progressive  := $EF8C   ; fill nametable 4 columns per call
reset_sprite_anim           := $F835   ; set entity OAM animation (X = slot)
init_child_entity           := $F846   ; init child entity (Y = slot)
submit_sound_ID             := $F89A   ; submit sound effect to queue
find_enemy_freeslot_x       := $FC43   ; find empty entity slot -> X
find_enemy_freeslot_y       := $FC53   ; find empty entity slot -> Y
update_CHR_banks            := $FF3C   ; update CHR bank selection
select_PRG_banks            := $FF6B   ; MMC3 PRG bank switch

.segment "BANK09"

; ===========================================================================
; JUMP TABLE — entry points called from fixed bank gameplay_frame_loop
; ===========================================================================
; The fixed bank dispatches these 7 per-frame calls in order:
;   $8000 → Wily 2 camera Y transition
;   $8003 → Stage clear / scroll-triggered entity spawning
;   $8006 → Wily 3 scripted entity spawning
;   $8009 → Gemini Man platform animation
;   $800C → Item pickup / drop entity spawning
;   $800F → Palette animation cycling
;   $8012 → Doc Robot Gemini nametable prep
;   $8015/$8018 are aliases for $8000/$8003 (unused alternate entries)
; ===========================================================================
subsys_wily_camera_y:  jmp     wily_camera_y_check     ; entry 0: $8000
subsys_stage_clear:  jmp     stage_clear_spawner       ; entry 1: $8003
        jmp     wily3_entity_spawn                      ; entry 2: $8006
        jmp     gemini_platform_update                  ; entry 3: $8009
        jmp     item_drop_update                        ; entry 4: $800C
        jmp     palette_anim_update                     ; entry 5: $800F
        jmp     docrobot_gemini_prep                    ; entry 6: $8012
        jmp     subsys_wily_camera_y                    ; alias: $8015
        jmp     subsys_stage_clear                      ; alias: $8018

; ===========================================================================
; SUBSYSTEM 4: Gemini Man platform animation (stage $05 only)
; ===========================================================================
; Animates the moving platforms in Gemini Man's stage (screens $05 and $0E).
; Uses a two-phase system:
;   Phase 0 ($55=0): Tile animation — cycles platform tile graphics every
;     8 frames using banked CHR data from $BB00-$BE00. Builds PPU update
;     buffer entries at $0780+ for 6 platform columns, shifting right by
;     8px each cycle.
;   Phase 1 ($55>0): Platform spawning — spawns 8 entity $71 (platform
;     pieces) one per frame, clearing metatiles as they detach.
;
; State: $55=spawn counter, $56=anim frame, $57=anim cycle
; ===========================================================================
gemini_platform_update:  lda     $22
        cmp     #$05            ; Gemini Man stage?
        bne     gemini_platform_clear
        ldy     $F9
        cpy     #$05            ; screen $05?
        beq     gemini_platform_active
        cpy     #$0E            ; screen $0E?
        beq     gemini_platform_active
gemini_platform_clear:  lda     #$00
        sta     $56             ; reset animation frame
        sta     $57             ; reset animation cycle
        sta     $55             ; reset spawn counter
        rts

gemini_platform_active:  sta     $F5
        jsr     select_PRG_banks ; swap in stage data bank
        lda     $55
        beq     gemini_tile_animate ; $55=0: tile animation phase
        jmp     gemini_platform_spawn ; $55>0: spawn platform entities

; --- Tile animation: update platform graphics every 8 frames ---
gemini_tile_animate:  lda     $56
        inc     $56             ; advance frame counter
        and     #$07            ; only act on frames 0 and 1 of each 8
        cmp     #$00
        beq     gemini_tile_init  ; frame 0: init PPU entries + tile data
        cmp     #$01
        bne     gemini_tile_done  ; frame 1: shift columns right
        ldx     #$00
        lda     #$06            ; 6 platform columns
        sta     $00
gemini_shift_cols:  lda     $0781,x ; shift each PPU entry address right by 8
        clc
        adc     #$08
        sta     $0781,x
        txa
        clc
        adc     #$0B
        tax
        dec     $00
        bne     gemini_shift_cols
        lda     #$FF
        sta     $19             ; flag: PPU buffer needs flush
        ldx     #$18            ; attribute table offset
        jsr     gemini_write_attrs
        inc     $57             ; advance cycle counter
gemini_tile_done:  rts

; --- Frame 0: init 6 PPU update entries and load tile data from CHR ROM ---
gemini_tile_init:  ldx     #$00
        ldy     #$00
gemini_setup_ppu:  lda     gemini_nt_addr_hi,y ; set up PPU buffer entries
        sta     $0780,x
        lda     gemini_nt_addr_lo,y
        clc
        adc     $00
        sta     $0781,x
        lda     #$07            ; 7 bytes per PPU column entry
        sta     $0782,x
        txa
        clc
        adc     #$0B
        tax
        iny
        cpy     #$06
        bne     gemini_setup_ppu
        lda     #$00
        sta     $02
; Load 4 groups of 3 tile rows from banked CHR ($BB00-$BE00) into PPU buffer
gemini_group_loop:  lda     $57
        and     #$03            ; cycle 0-3 selects tile frame
        tax
        ldy     $02             ; $02 = group index (0-3)
        lda     gemini_frame_base,x
        clc
        adc     gemini_col_offsets,y
        sta     $00
        ldx     gemini_ppu_offsets,y
        lda     #$03
        sta     $01
gemini_row_loop:  ldy     $00             ; tile index into lookup table
        lda     gemini_tile_indices,y
        tay                     ; Y = CHR source tile ID
        lda     $BB00,y         ; load 2x2 tile from banked CHR
        sta     $0783,x
        lda     $BC00,y
        sta     $0784,x
        lda     $BD00,y
        sta     $078E,x
        lda     $BE00,y
        sta     $078F,x
        inc     $00
        txa
        clc
        adc     #$16
        tax
        dec     $01
        bne     gemini_row_loop
        inc     $02
        lda     $02
        cmp     #$04
        bne     gemini_group_loop
        lda     #$FF
        sta     $07C2
        sta     $19
        ldx     #$1C
        jsr     gemini_write_attrs
        rts

; Write 4 attribute bytes for the current animation cycle
gemini_write_attrs:  lda     #$04
        sta     $00
        lda     $57
        and     #$03            ; cycle 0-3
        asl     a
        asl     a               ; * 4 (4 attr bytes per cycle)
        tay
gemini_attr_loop:  lda     gemini_attr_data,y
        sta     $03C0,x
        iny
        inx
        dec     $00
        bne     gemini_attr_loop
        rts

; --- Gemini platform tile/attribute data tables ---
gemini_tile_indices:  .byte   $83,$84,$85,$80,$81,$82,$83,$84
        .byte   $85,$81,$82,$80,$81,$82,$80,$83
        .byte   $84,$85,$80,$81,$82,$83,$84,$85
        .byte   $83,$84,$85,$81,$82,$80,$83,$84
        .byte   $85,$80,$81,$82,$80,$81,$82,$83
        .byte   $84,$85,$81,$82,$80,$83,$84,$85
gemini_frame_base:  .byte   $00,$0C,$18,$24
gemini_col_offsets:  .byte   $00,$03,$06,$09
gemini_ppu_offsets:  .byte   $00,$02,$04,$06
gemini_nt_addr_hi:  .byte   $21,$22,$22,$22,$22,$22
gemini_nt_addr_lo:  .byte   $EC,$0C,$2C,$4C,$6C,$8C
gemini_attr_data:  .byte   $88,$90,$98,$90,$90,$98,$90,$88
        .byte   $98,$90,$88,$90,$90,$88,$90,$98
; --- Phase 1: spawn platform entities (one per frame, 8 total) ---
gemini_platform_spawn:  lda     $55
        and     #$0F
        cmp     #$08            ; spawned all 8?
        bcs     per_frame_rts
        tax
        lda     gemini_metatile_ids,x
        sta     $28             ; metatile position to clear
        lda     #$00
        sta     $10
        ldy     #$1E
        jsr     queue_metatile_update ; remove platform tile from nametable
        ldx     #$00
        jsr     find_enemy_freeslot_y
        bcs     gemini_spawn_next
        lda     #$71            ; entity type $71 = platform piece
        jsr     init_child_entity
        lda     #$19
        sta     $0320,y         ; entity timer
        lda     #$00
        sta     $0500,y
        sta     $0480,y
        lda     $0380           ; copy player screen
        sta     $0380,y
        lda     $55
        and     #$07
        tax
        lda     gemini_spawn_x,x
        sta     $03C0,y         ; entity X position
        lda     gemini_spawn_y,x
        sta     $0360,y         ; entity Y position
        lda     $55
        and     #$03
        bne     gemini_spawn_next
        lda     #$27            ; sound: platform detach
        jsr     submit_sound_ID
gemini_spawn_next:  inc     $55
per_frame_rts:  rts              ; shared RTS for early returns

; --- Gemini platform spawn data (8 platform pieces) ---
gemini_metatile_ids:  .byte   $05,$06,$07,$0D,$0E,$0F,$17,$1F
gemini_spawn_x:  .byte   $10,$10,$10,$30,$30,$30,$50,$70
gemini_spawn_y:  .byte   $B0,$D0,$F0,$B0,$D0,$F0,$F0,$F0
; ===========================================================================
; SUBSYSTEM 5: Item pickup / drop entity spawning
; ===========================================================================
; When $68 bit 7 is set, spawns one item entity per frame from per-stage
; tables. Each item clears its metatile and spawns entity $71.
; When all items spawned ($FF terminator), clears $68 bit 7.
; Special case: stage $02 (Gemini Man) reloads palette + CHR after items.
;
; State: $68 bit 7 = active, $68 bits 0-3 = current item index
; ===========================================================================
item_drop_update:  lda     $68
        bpl     per_frame_rts   ; bit 7 clear = no items pending
        and     #$0F            ; current item index
        ldy     $22             ; stage ID
        clc
        adc     item_stage_offsets,y ; base offset for this stage
        tay
        lda     item_entity_types,y
        cmp     #$FF            ; $FF = all items done
        beq     item_drop_done
        sta     $11             ; entity type for metatile update
        lda     item_metatile_pos,y
        sta     $28             ; metatile position
        tya
        pha
        lda     #$00
        sta     $10
        jsr     queue_metatile_update ; clear the metatile from nametable
        inc     $68             ; advance to next item
        pla
        tay
        jsr     find_enemy_freeslot_x
        bcs     item_drop_rts
        lda     #$71            ; entity type $71 = item pickup
        jsr     reset_sprite_anim
        lda     #$19
        sta     $0320,x         ; entity timer
        lda     #$80
        sta     $0300,x         ; entity flags (active)
        lda     #$90
        sta     $0580,x
        lda     #$00
        sta     $03E0,x
        sta     $0480,x
        sta     $0500,x
        lda     $F9
        sta     $0380,x         ; entity screen = current screen
        lda     item_spawn_x,y
        sta     $03C0,x         ; entity X position
        lda     item_spawn_y,y
        sta     $0360,x         ; entity Y position
        lda     #$27            ; sound: item spawn
        jmp     submit_sound_ID

; --- All items done: clear active flag, handle stage-specific palette ---
item_drop_done:  lda     $68
        and     #$7F            ; clear bit 7 (no more items)
        sta     $68
        lda     $22
        cmp     #$02            ; Gemini Man stage?
        bne     item_drop_rts
        lda     #$4E            ; Gemini stage: reload CHR banks
        sta     $E9
        jsr     update_CHR_banks
        ldy     #$0F
item_load_palette:  lda     $AABE,y ; load 16 bytes of new palette from bank data
        sta     $0600,y         ; palette buffer 1
        sta     $0620,y         ; palette buffer 2
        dey
        bpl     item_load_palette
        sty     $18             ; $18 = $FF: force palette update
        ldx     #$00
        ldy     #$4C            ; offset into stage bank palette anim data
item_init_pal_anim:  lda     $AA82,y ; load palette animation entry
        pha
        and     #$80            ; bit 7 = active flag
        sta     $0100,x         ; pal anim slot active
        pla
        and     #$7F            ; bits 0-6 = palette ID
        sta     $0108,x         ; pal anim slot palette ID
        lda     #$00
        sta     $0104,x         ; pal anim slot frame = 0
        sta     $010C,x         ; pal anim slot tick = 0
        iny
        inx
        cpx     #$04            ; 4 palette animation slots
        bne     item_init_pal_anim
item_drop_rts:  rts

; --- Item drop data tables (per-stage item definitions) ---
; item_stage_offsets: base index into item tables for each stage ($FF = none)
; item_entity_types: entity type per item ($FF = end of list)
; item_metatile_pos: nametable metatile position to clear
; item_spawn_x/y: entity spawn coordinates
item_stage_offsets:  .byte   $FF,$00,$0F,$03,$FF,$FF,$FF,$0C
item_entity_types:  .byte   $43,$43,$FF,$86,$86,$86,$86,$86
        .byte   $86,$86,$86,$FF,$3B,$38,$FF,$00
        .byte   $00,$00,$00,$00,$00,$FF
item_metatile_pos:  .byte   $31,$39,$FF,$23,$24,$2B,$2C,$33
        .byte   $34,$3B,$3C,$FF,$31,$32,$FF,$2B
        .byte   $2C,$33,$34,$3B,$3C,$FF
item_spawn_x:  .byte   $C0,$D0,$FF,$90,$90,$B0,$B0,$D0
        .byte   $D0,$E0,$E0,$FF,$C0,$D0,$FF,$C0
        .byte   $B0,$D0,$E0,$C0,$B0,$FF
item_spawn_y:  .byte   $18,$28,$FF,$70,$90,$70,$90,$70
        .byte   $90,$70,$90,$FF,$18,$28,$FF,$68
        .byte   $88,$90,$78,$78,$88,$FF
; ===========================================================================
; SUBSYSTEM 6: Wily 2 camera Y-axis transition
; ===========================================================================
; On Wily 2 (stage $0E), screens $09/$0A: initiates vertical camera scroll
; by setting scroll mode $F8=$09. Once active, increments $69 each frame
; (the fixed bank uses $69 to drive the Y scroll offset).
; ===========================================================================
wily_camera_y_check:  lda     $F8
        cmp     #$09            ; Y transition already active?
        beq     wily_camera_y_tick
        lda     $22
        cmp     #$0E            ; Wily 2 stage?
        bne     wily_camera_y_rts
        lda     $F9
        cmp     #$09            ; screen $09?
        beq     wily_camera_y_init
        cmp     #$0A            ; screen $0A?
        bne     wily_camera_y_rts
wily_camera_y_init:  lda     #$00
        sta     $69             ; clear Y transition counter
        sta     $73             ; clear transition state
        lda     #$09
        sta     $F8             ; scroll mode = Y transition
        lda     #$2F
        sta     $5E             ; camera Y target
        rts

wily_camera_y_tick:  inc     $69   ; advance Y transition counter
wily_camera_y_rts:  rts

; ===========================================================================
; SUBSYSTEM 7: Doc Robot Gemini stage nametable preparation
; ===========================================================================
; On Doc Robot stage ($08), screens $15/$1A: when entity slot 31 ($033F)
; reaches position $FC, fills nametable column $1F progressively and sets
; scroll mode to $03 when complete. This prepares the stage layout for the
; Gemini-style section of the Doc Robot stage.
; ===========================================================================
docrobot_gemini_prep:  lda     $22
        cmp     #$08            ; Doc Robot stage?
        bne     docrobot_rts
        lda     $F9
        cmp     #$15            ; screen $15?
        beq     docrobot_check_scroll
        cmp     #$1A            ; screen $1A?
        bne     docrobot_rts
docrobot_check_scroll:  lda     $F8
        cmp     #$03            ; already in scroll mode $03?
        beq     docrobot_rts
        lda     $033F           ; entity[31] position
        cmp     #$FC            ; reached trigger position?
        bne     docrobot_rts
        lda     $22
        sta     $F5
        jsr     select_PRG_banks ; swap in stage data bank
        lda     #$1F            ; metatile column $1F
        jsr     metatile_column_ptr_by_id
        lda     #$08
        sta     $10
        jsr     fill_nametable_progressive ; draw columns progressively
        lda     $70             ; $70 = columns remaining
        bne     docrobot_rts    ; still drawing?
        lda     #$03            ; done: set scroll mode $03
        sta     $F8
docrobot_rts:  rts

; ===========================================================================
; SUBSYSTEM 1: Stage clear / scroll-triggered entity spawning
; ===========================================================================
; Spawns waves of entity $2C (debris/effects) triggered by camera scroll
; position. Active on stage $01 (scroll zones 0-3) and stage $0C screen $0D
; (zone 4). Each zone has a cycle count and spawn data index.
; Every 60 frames ($3C), spawns a group of entities from the data tables.
;
; State: $64=zone, $65=timer, $66=data index, $67=cycle counter
; ===========================================================================
stage_clear_spawner:  lda     $22
        cmp     #$0C            ; Wily 1 / Break Man stage?
        beq     stage_clear_wily1
        cmp     #$01            ; stage $01?
        bne     stage_clear_reset
        lda     $0380           ; player screen (camera hi)
        cmp     #$0C            ; past screen $0C?
        bcc     stage_clear_reset
        ldy     #$00
; --- Check which scroll zone the camera is in ---
stage_clear_zone_check:  lda     $0360
        sec
        sbc     clear_threshold_lo,y ; 16-bit compare: camera vs zone boundary
        lda     $0380
        sbc     clear_threshold_hi,y
        bcc     stage_clear_set_zone  ; camera < threshold → in this zone
        iny
        cpy     #$04
        bne     stage_clear_zone_check
        rts                     ; past all zones

stage_clear_reset:  lda     #$3C  ; reset timer to 60 frames
        sta     $65
        lda     #$00
        sta     $64             ; zone = 0
        sta     $67             ; cycle = 0
        sta     $66             ; data index = 0
        rts

stage_clear_wily1:  lda     $F9
        cmp     #$0D            ; screen $0D only
        bne     stage_clear_reset
        ldy     #$04            ; zone 4 (Wily 1 specific)
; --- Enter/update zone ---
stage_clear_set_zone:  cpy     $64
        beq     stage_clear_tick ; same zone: just tick
        sty     $64             ; new zone
        lda     #$3C
        sta     $65             ; reset timer
        lda     #$00
        sta     $67             ; reset cycle counter
        lda     clear_zone_data_idx,y
        sta     $66             ; set spawn data start index
; --- Timer tick: spawn on zero ---
stage_clear_tick:  dec     $65
        bne     stage_clear_rts ; not time yet
        ldy     $66
        lda     clear_spawn_x,y ; first byte = entity count - 1
        sta     $00
        inc     $66             ; skip count byte
; --- Spawn loop: spawn $00+1 entities ---
stage_clear_spawn_loop:  jsr     find_enemy_freeslot_x
        lda     #$2C            ; entity type $2C = debris effect
        jsr     reset_sprite_anim
        lda     #$80
        sta     $0300,x         ; entity flags (active)
        lda     #$9A
        sta     $0580,x
        lda     #$00
        sta     $0320,x         ; entity timer
        sta     $03E0,x
        lda     #$17
        sta     $0480,x         ; entity behavior type
        ldy     $66
        lda     clear_spawn_x,y
        sta     $0360,x         ; entity Y position
        lda     clear_spawn_screen,y
        sta     $0380,x         ; entity screen
        lda     clear_spawn_attr,y
        sta     $03C0,x         ; entity X / attribute
        inc     $66             ; next data entry
        dec     $00
        bpl     stage_clear_spawn_loop
        lda     #$23            ; sound: debris spawn
        jsr     submit_sound_ID
        lda     #$3C
        sta     $65             ; reset timer for next wave
        ldy     $64
        inc     $67             ; increment cycle
        lda     $67
        cmp     clear_zone_cycles,y ; all cycles done for this zone?
        bne     stage_clear_rts
        lda     #$00            ; reset cycle to loop
        sta     $67
        lda     clear_zone_data_idx,y
        sta     $66             ; reset data index to zone start
stage_clear_rts:  rts

; --- Stage clear spawn data tables ---
; clear_threshold_lo/hi: 16-bit camera position boundaries for 4 zones
; clear_zone_cycles: spawn cycles per zone before reset
; clear_zone_data_idx: starting index into spawn data for each zone
; clear_spawn_x: interleaved (count-1, Y pos, Y pos, ...) per group
; clear_spawn_screen: screen number for each spawned entity
; clear_spawn_attr: X position / attribute for each spawned entity
clear_threshold_lo:  .byte   $E0,$80,$40,$C0
clear_threshold_hi:  .byte   $0C,$0D,$0E,$0F
clear_zone_cycles:  .byte   $06,$05,$04,$04,$07
clear_zone_data_idx:  .byte   $00,$0F,$1A,$22,$2A
clear_spawn_x:  .byte   $01,$48,$A8,$00,$78,$00,$58,$00
        .byte   $98,$01,$38,$C0,$01,$98,$C0,$01
        .byte   $28,$58,$00,$08,$00,$58,$00,$28
        .byte   $00,$68,$00,$E8,$00,$18,$00,$F8
        .byte   $00,$38,$00,$F8,$00,$18,$00,$38
        .byte   $00,$68,$00,$C8,$00,$A8,$00,$88
        .byte   $00,$88,$00,$58,$00,$38,$00,$28
clear_spawn_screen:  .byte   $01,$0C,$0C,$00,$0C,$00,$0C,$00
        .byte   $0C,$01,$0C,$0C,$01,$0C,$0C,$01
        .byte   $0D,$0D,$00,$0D,$00,$0D,$00,$0D
        .byte   $00,$0D,$00,$0D,$00,$0E,$00,$0D
        .byte   $00,$0E,$00,$0E,$00,$0F,$00,$0F
        .byte   $00,$0F,$00,$0D,$00,$0D,$00,$0D
        .byte   $00,$0D,$00,$0D,$00,$0D,$00,$0D
clear_spawn_attr:  .byte   $01,$98,$40,$00,$88,$00,$58,$00
        .byte   $70,$01,$78,$90,$01,$98,$60,$01
        .byte   $98,$98,$00,$78,$00,$78,$00,$48
        .byte   $00,$58,$00,$98,$00,$98,$00,$68
        .byte   $00,$68,$00,$78,$00,$B8,$00,$88
        .byte   $00,$98,$00,$88,$00,$68,$00,$59
        .byte   $00,$98,$00,$98,$00,$88,$00,$68
; ===========================================================================
; SUBSYSTEM 2: Wily 3 scripted entity spawning
; ===========================================================================
; On Wily 3 (stage $0F), screen $08: places 8 entities into slots 24-31
; using a bitmask ($6E) to track which have already been spawned.
; Skipped when entity[31] is active ($031F bit 7) or game state = $11.
; ===========================================================================
wily3_entity_spawn:  lda     $22
        cmp     #$0F            ; Wily 3 stage?
        bne     wily3_rts
        lda     $F9
        cmp     #$08            ; screen $08?
        bne     wily3_rts
        lda     $031F           ; entity[31] flags
        bmi     wily3_rts       ; active → skip
        lda     $30             ; game substate
        cmp     #$11
        beq     wily3_rts       ; state $11 → skip
        ldy     #$07            ; 8 entities (slots 24-31)
        lda     $6E             ; spawn bitmask
        sta     $00
wily3_spawn_loop:  asl     $00   ; shift out next bit
        bcs     wily3_spawn_skip ; bit=1: already spawned, skip
        lda     #$80
        sta     $0318,y         ; entity_flags[24+Y] = active
        lda     #$90
        sta     $0598,y
        lda     #$EB
        sta     $0338,y         ; entity_health[24+Y]
        lda     #$67
        sta     $05D8,y         ; entity_anim[24+Y]
        lda     #$00
        sta     $05F8,y
        sta     $05B8,y
        sta     $03F8,y
        lda     #$0B
        sta     $0498,y         ; entity_behavior[24+Y]
        lda     $F9
        sta     $0398,y         ; entity_screen[24+Y]
        lda     wily3_spawn_x,y
        sta     $0378,y         ; entity_x[24+Y]
        lda     wily3_spawn_y,y
        sta     $03D8,y         ; entity_y[24+Y]
        lda     wily3_spawn_type,y
        sta     $04D8,y         ; entity_subtype[24+Y]
wily3_spawn_skip:  dey
        bpl     wily3_spawn_loop
wily3_rts:  rts

; --- Wily 3 spawn position tables (8 entities) ---
wily3_spawn_x:  .byte   $30,$30,$30,$70,$90,$D0,$D0,$D0
wily3_spawn_y:  .byte   $30,$70,$B0,$B0,$B0,$30,$70,$B0
wily3_spawn_type:  .byte   $12,$13,$14,$15,$16,$17,$18,$19
; ===========================================================================
; SUBSYSTEM 3: Palette animation cycling
; ===========================================================================
; Processes 4 palette animation slots ($0100-$010F). Each slot has:
;   $0100+slot = active flag (bit 7)
;   $0104+slot = current frame index
;   $0108+slot = palette animation ID (indexes pal_anim_offsets)
;   $010C+slot = tick counter
;
; Each animation ID maps to a packed header in the data stream:
;   header+0 = max frame index     header+1 = ticks per frame
;   header+2 = palette buffer dest  header+3.. = frame sequence data
; Each frame indexes pal_color_sets (3 bytes per color set) written to
; $0600/$0620 palette buffers.
;
; Skipped when game is paused ($72) or fading ($1C).
; ===========================================================================
palette_anim_update:  lda     $72
        ora     $1C             ; paused or fading?
        bne     palette_anim_rts
        lda     #$03
        sta     $10             ; loop 4 slots (3 down to 0)
        lda     $18
        and     #$01            ; save palette-update-needed flag
        sta     $11
        lda     #$00
        sta     $18             ; clear palette update flag
palette_slot_loop:  ldx     $10
        lda     $0100,x         ; slot active?
        bpl     palette_slot_next
        jsr     palette_anim_tick ; process this slot
palette_slot_next:  dec     $10
        bpl     palette_slot_loop
        lda     $11             ; restore palette update flag
        sta     $18
palette_anim_rts:  rts

; --- Process one palette animation slot ---
palette_anim_tick:  ldy     $0108,x ; palette animation ID
        lda     pal_anim_offsets,y
        tay                     ; Y = header offset
        lda     $010C,x         ; current tick
        inc     $010C,x
        cmp     pal_tick_rate,y ; time for next frame?
        bne     palette_tick_rts ; not yet
        lda     #$00
        sta     $010C,x         ; reset tick counter
        lda     $0104,x         ; current frame
        inc     $0104,x
        cmp     pal_frame_count,y ; past max frame?
        bne     palette_advance_frame
        lda     #$00            ; wrap to frame 0
        sta     $0104,x
palette_advance_frame:  tya     ; Y = header offset
        clc
        adc     $0104,x         ; + current frame
        tax
        lda     pal_sequence_data,x ; frame data = color set index
        asl     a
        adc     pal_sequence_data,x ; * 3 (3 bytes per color set)
        tax
        lda     pal_dest_offset,y ; destination in palette buffer
        tay
        lda     #$03            ; 3 color bytes to copy
        sta     $00
palette_write_colors:  lda     pal_color_sets,x
        sta     $0600,y         ; write to palette buffer 1
        sta     $0620,y         ; write to palette buffer 2
        iny
        inx
        dec     $00
        bne     palette_write_colors
        inc     $11             ; flag: palette was updated
palette_tick_rts:  rts

; ---------------------------------------------------------------------------
; Palette animation data tables
; ---------------------------------------------------------------------------
; pal_color_sets: 38 color sets, 3 NES palette values each (114 bytes)
; pal_anim_offsets: header offsets for 14 animation definitions
; pal_frame_count..pal_sequence_data: packed animation headers + frame data
; ---------------------------------------------------------------------------
pal_color_sets:  .byte   $20,$31,$21,$20,$21,$21,$00,$32
        .byte   $22,$33,$0F,$18,$0F,$33,$18,$3C
        .byte   $07,$0F,$10,$00,$07,$00,$10,$27
        .byte   $36,$27,$07,$20,$36,$27,$20,$20
        .byte   $36,$31,$11,$00,$31,$15,$00,$31
        .byte   $17,$00,$31,$15,$17,$31,$17,$11
        .byte   $31,$11,$15,$22,$12,$12,$12,$22
        .byte   $12,$12,$12,$22,$10,$00,$36,$20
        .byte   $32,$26,$20,$32,$0F,$1B,$1B,$0F
        .byte   $1B,$0F,$1B,$26,$16,$06,$16,$06
        .byte   $26,$06,$26,$16,$31,$31,$21,$31
        .byte   $36,$21,$31,$39,$21,$0F,$10,$02
        .byte   $0F,$02,$10,$31,$30,$21,$14,$03
        .byte   $03,$04,$14,$03,$03,$04,$14,$03
        .byte   $03,$04
pal_anim_offsets:  .byte   $00,$05,$0A,$11,$18,$1E,$24,$2A
        .byte   $2F,$34,$39,$3F,$46,$4B
pal_frame_count:  .byte   $01
pal_tick_rate:  .byte   $08
pal_dest_offset:  .byte   $0D
pal_sequence_data:  .byte   $00,$01,$01,$02,$0D,$03,$04,$03
        .byte   $08,$05,$06,$07,$14,$07,$03,$08
        .byte   $09,$08,$09,$0A,$09,$02,$10,$01
        .byte   $0B,$0C,$0D,$02,$10,$05,$0E,$0F
        .byte   $10,$02,$10,$09,$11,$12,$13,$01
        .byte   $02,$05,$15,$16,$01,$02,$09,$16
        .byte   $15,$01,$04,$0D,$17,$18,$02,$08
        .byte   $05,$19,$1A,$1B,$03,$04,$0D,$1C
        .byte   $1D,$1E,$21,$01,$02,$05,$1F,$20
        .byte   $03,$02,$05,$22,$23,$24,$25
; ===========================================================================
; DOC ROBOT GEMINI STAGE DATA ($8660-$9FFF)
; ===========================================================================
; When bank $09 is mapped at $A000-$BFFF (as the stage data bank for
; Doc Robot Gemini, $22=$09), this data provides the stage layout:
; screen data, metatile definitions, enemy placement, and palette configs.
; Accessed via addresses $A660-$BFFF from the fixed bank's stage loader.
; ===========================================================================
        .byte   $FF
        .byte   $DF,$FF,$7F,$FF,$FD,$FF,$DF,$FF
        .byte   $FD,$FF,$FD,$FF,$F7,$FF,$7F,$FF
        .byte   $F7,$FF,$D7,$FF,$F7,$FD,$FF,$FF
        .byte   $7F,$FF,$DD,$F7,$DF,$FF,$D7,$FF
        .byte   $7D,$FF,$7F,$FF,$DD,$AF,$FF,$FF
        .byte   $7D,$FF,$DD,$FF,$F7,$FF,$5F,$FF
        .byte   $D7,$FF,$F7,$FF,$F5,$FF,$D7,$FF
        .byte   $FF,$FF,$F5,$7D,$7F,$FF,$F7,$FF
        .byte   $D7,$FF,$5F,$EF,$C7,$FB,$F5,$EF
        .byte   $F7,$FF,$FF,$FF,$FD,$FF,$F7,$FF
        .byte   $BF,$FE,$F7,$FF,$FF,$DF,$FD,$FF
        .byte   $FF,$BF,$FD,$FF,$F7,$FF,$FF,$FF
        .byte   $DF,$FF,$FD,$FF,$DF,$F7,$57,$FF
        .byte   $EF,$FF,$35,$F7,$7D,$FF,$7F,$FF
        .byte   $77,$FF,$FD,$FF,$7F,$FF,$DF,$FF
        .byte   $F7,$FE,$77,$FF,$7D,$FF,$FD,$FF
        .byte   $7F,$FF,$7D,$FF,$DF,$FF,$F7,$FF
        .byte   $FF,$FF,$FD,$FE,$DF,$FF,$DF,$FF
        .byte   $F5,$FF,$F7,$FF,$7F,$FF,$5F,$FF
        .byte   $FF,$FF,$5F,$FF,$FF,$FF,$7D,$FF
        .byte   $3D,$FF,$57,$FF,$5E,$FF,$B7,$FF
        .byte   $D5,$FF,$7D,$FF,$F5,$EF,$DC,$BF
        .byte   $F7,$FF,$55,$FF,$5F,$FF,$57,$FF
        .byte   $BD,$FF,$5F,$D7,$7F,$FF,$77,$FF
        .byte   $DF,$FF,$7F,$FF,$FF,$FF,$7D,$DF
        .byte   $FF,$7F,$FD,$FE,$FF,$FF,$5D,$FF
        .byte   $7F,$FF,$FC,$FF,$5F,$FF,$FD,$FF
        .byte   $FF,$FF,$DF,$FF,$DF,$FF,$F7,$F7
        .byte   $F5,$FF,$F7,$FF,$DF,$FF,$F6,$FF
        .byte   $F7,$F7,$5D,$FF,$DF,$FF,$D7,$FF
        .byte   $75,$FD,$FF,$FF,$77,$FF,$D7,$FF
        .byte   $57,$FF,$F5,$F7,$FD,$FE,$DD,$FF
        .byte   $DF,$FF,$DF,$FF,$FD,$9F,$DF,$FF
        .byte   $F5,$FF,$7F,$FF,$FF,$FF,$E5,$FF
        .byte   $F5,$FF,$F7,$FF,$F7,$FF,$FF,$DF
        .byte   $FF,$FF,$77,$FF,$77,$FF,$F7,$BF
        .byte   $FB,$FD,$FF,$FF,$27,$FF,$DD,$FF
        .byte   $7F,$FF,$F5,$FE,$FF,$FD,$7F,$FE
        .byte   $FC,$FF,$D7,$FD,$7F,$FF,$D7,$FF
        .byte   $7F,$FF,$55,$FF,$FF,$FF,$DF,$FF
        .byte   $DD,$FF,$F7,$F7,$7D,$FF,$FF,$EF
        .byte   $7E,$FF,$F7,$FF,$FD,$FF,$D7,$FF
        .byte   $DF,$FF,$DF,$FF,$FF,$FF,$DF,$FF
        .byte   $F7,$FB,$DD,$FF,$FF,$FF,$FD,$FF
        .byte   $FF,$FB,$75,$FF,$FF,$FF,$FF,$FF
        .byte   $DF,$FF,$71,$FF,$FD,$FF,$7F,$FF
        .byte   $F7,$FF,$F7,$FF,$5F,$FF,$DF,$FF
        .byte   $FF,$FF,$DD,$FF,$FF,$FF,$DF,$FF
        .byte   $FF,$FF,$F7,$EF,$FF,$FF,$F5,$FF
        .byte   $FF,$FF,$7B,$FF,$FF,$7F,$5D,$FF
        .byte   $FF,$FF,$7D,$FF,$F5,$FF,$FF,$FF
        .byte   $57,$FF,$FF,$FF,$F7,$FF,$DF,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$31,$00,$01,$00
        .byte   $02,$00,$00,$00,$00,$00,$02,$00
        .byte   $00,$80,$00,$08,$40,$80,$10,$00
        .byte   $02,$00,$20,$00,$00,$00,$20,$00
        .byte   $00,$00,$01,$00,$08,$00,$00,$00
        .byte   $00,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $61,$00,$00,$02,$12,$08,$00,$00
        .byte   $00,$00,$52,$00,$00,$00,$42,$00
        .byte   $00,$08,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $08,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$82
        .byte   $01,$00,$00,$00,$90,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$08,$00,$00,$00,$00
        .byte   $08,$00,$00,$08,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$02,$00,$00
        .byte   $40,$00,$00,$20,$00,$00,$00,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $08,$00,$21,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$00,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$01,$20
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$00,$00,$20,$00,$00
        .byte   $00,$00,$22,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$80,$00,$00
        .byte   $54,$08,$04,$00,$A0,$20,$08,$00
        .byte   $02,$00,$08,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$D0,$00
        .byte   $00,$00,$00,$00,$00,$00,$04,$00
        .byte   $20,$00,$00,$00,$80,$A0,$00,$20
        .byte   $00,$00,$80,$00,$01,$80,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$02
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$08,$00
        .byte   $08,$00,$00,$A8,$00,$01,$01,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$00,$00,$02
        .byte   $40,$00,$02,$02,$10,$20,$00,$00
        .byte   $18,$00,$00,$08,$00,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $40,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$82,$00,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $01,$02,$03,$04,$05,$06,$07,$08
        .byte   $09,$0A,$0B,$0C,$0D,$0E,$0F,$10
        .byte   $11,$12,$13,$14,$15,$16,$17,$18
        .byte   $19,$1A,$1B,$1C,$1D,$1E,$00,$00
        .byte   $00,$00,$81,$00,$40,$02,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$0F
        .byte   $48,$48,$48,$80,$00,$00,$00,$1C
        .byte   $60,$60,$10,$00,$00,$00,$00,$28
        .byte   $20,$62,$62,$20,$20,$21,$22,$20
        .byte   $A2,$40,$60,$20,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$20
        .byte   $8C,$00,$00,$00,$00,$00,$00,$20
        .byte   $00,$00,$04,$38,$01,$38,$01,$00
        .byte   $02,$0F,$02,$06,$03,$38,$03,$38
        .byte   $03,$06,$03,$06,$03,$03,$03,$03
        .byte   $04,$0F,$04,$00,$00,$00,$00,$4C
        .byte   $4E,$0F,$31,$11,$00,$0F,$31,$15
        .byte   $17,$0F,$22,$12,$12,$0F,$31,$21
        .byte   $11,$82,$83,$00,$00,$0F,$31,$11
        .byte   $00,$0F,$31,$15,$17,$0F,$22,$12
        .byte   $12,$0F,$20,$10,$17,$84,$85,$86
        .byte   $00,$0F,$31,$11,$00,$0F,$31,$15
        .byte   $17,$0F,$22,$12,$12,$0F,$2C,$0B
        .byte   $09,$84,$85,$86,$00,$0F,$31,$11
        .byte   $00,$0F,$31,$15,$17,$0F,$22,$12
        .byte   $12,$0F,$20,$10,$00,$84,$85,$86
        .byte   $00,$0F,$31,$11,$00,$0F,$31,$15
        .byte   $17,$0F,$22,$12,$12,$0F,$31,$21
        .byte   $11,$84,$85,$86,$00,$00,$00,$00
        .byte   $00,$80,$00,$02,$00,$00,$00,$06
        .byte   $0C,$00,$00,$00,$08,$00,$01,$12
        .byte   $1D,$FF,$00,$00,$00,$00,$00,$01
        .byte   $01,$02,$02,$02,$02,$03,$03,$03
        .byte   $05,$05,$05,$05,$05,$06,$06,$06
        .byte   $06,$06,$07,$07,$07,$08,$08,$08
        .byte   $08,$0A,$0A,$0B,$0B,$0B,$0F,$0F
        .byte   $0F,$0F,$11,$13,$14,$15,$15,$15
        .byte   $15,$15,$15,$16,$16,$16,$16,$16
        .byte   $16,$17,$17,$17,$18,$18,$19,$1A
        .byte   $1A,$1A,$1B,$1B,$1E,$FF,$02,$00
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$00,$01,$00,$00,$00
        .byte   $00,$00,$40,$00,$20,$00,$40,$00
        .byte   $00,$00,$00,$00,$00,$00,$80,$00
        .byte   $10,$00,$00,$02,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$02
        .byte   $00,$80,$10,$00,$40,$20,$00,$00
        .byte   $00,$00,$00,$22,$02,$08,$10,$00
        .byte   $00,$00,$00,$00,$40,$00,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$00
        .byte   $00,$00,$08,$02,$00,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$24
        .byte   $3D,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$00,$08,$02,$00
        .byte   $40,$00,$00,$00,$10,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$B8
        .byte   $C8,$48,$98,$A8,$E8,$88,$A8,$C8
        .byte   $28,$58,$88,$C8,$E8,$38,$68,$A8
        .byte   $C8,$F8,$68,$A8,$D8,$28,$58,$68
        .byte   $C8,$58,$E8,$28,$48,$98,$38,$68
        .byte   $98,$C8,$C0,$28,$F8,$10,$28,$88
        .byte   $A8,$C8,$C8,$08,$48,$68,$78,$A8
        .byte   $C8,$48,$88,$C8,$E8,$E8,$88,$48
        .byte   $68,$98,$38,$A8,$C8,$FF,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$06
        .byte   $10,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$20,$00,$00,$40,$00,$10,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$84,$00,$10,$04,$80,$10,$00
        .byte   $10,$40,$00,$00,$00,$00,$00,$10
        .byte   $00,$80,$00,$00,$00,$08,$00,$00
        .byte   $00,$20,$00,$20,$10,$24,$80,$00
        .byte   $00,$00,$00,$00,$01,$00,$00,$00
        .byte   $00,$00,$00,$00,$01,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$08,$00,$00,$00,$04
        .byte   $00,$10,$00,$60,$01,$20,$00,$02
        .byte   $00,$40,$04,$02,$40,$01,$40,$00
        .byte   $00,$00,$00,$00,$00,$02,$00,$00
        .byte   $40,$40,$00,$00,$14,$00,$00,$04
        .byte   $00,$00,$00,$04,$00,$02,$00,$02
        .byte   $00,$01,$00,$00,$00,$01,$00,$80
        .byte   $10,$00,$44,$00,$00,$00,$00,$00
        .byte   $00,$80,$00,$00,$00,$08,$01,$00
        .byte   $10,$00,$04,$00,$00,$10,$10,$00
        .byte   $10,$20,$40,$00,$00,$00,$10,$04
        .byte   $00,$02,$00,$00,$00,$00,$00,$24
        .byte   $00,$02,$01,$41,$14,$14,$44,$98
        .byte   $48,$68,$98,$48,$88,$48,$B8,$88
        .byte   $68,$B8,$68,$48,$68,$98,$88,$68
        .byte   $48,$98,$48,$88,$B8,$68,$98,$48
        .byte   $68,$38,$30,$90,$30,$38,$68,$58
        .byte   $48,$38,$A0,$20,$38,$A8,$28,$A8
        .byte   $28,$28,$A8,$38,$A8,$28,$58,$28
        .byte   $A8,$48,$48,$48,$20,$88,$20,$20
        .byte   $A0,$20,$20,$20,$B0,$FF,$00,$04
        .byte   $00,$20,$00,$60,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$40,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$04,$00,$00,$04,$08
        .byte   $40,$00,$00,$00,$04,$08,$10,$00
        .byte   $10,$20,$00,$84,$00,$03,$00,$80
        .byte   $40,$00,$00,$01,$00,$00,$00,$40
        .byte   $00,$00,$00,$01,$00,$00,$00,$88
        .byte   $11,$08,$00,$00,$00,$00,$00,$00
        .byte   $40,$80,$40,$00,$00,$00,$00,$00
        .byte   $00,$08,$00,$00,$00,$00,$00,$20
        .byte   $00,$20,$00,$00,$00,$00,$04,$88
        .byte   $40,$00,$00,$29,$10,$80,$00,$00
        .byte   $00,$01,$00,$00,$00,$04,$00,$22
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$88,$00,$00
        .byte   $00,$20,$10,$00,$00,$20,$00,$20
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $40,$00,$00,$12,$10,$80,$00,$04
        .byte   $00,$10,$40,$41,$00,$00,$00,$24
        .byte   $04,$88,$00,$00,$00,$00,$00,$20
        .byte   $00,$40,$04,$00,$00,$30,$00,$0C
        .byte   $18,$18,$0C,$18,$18,$18,$0C,$18
        .byte   $18,$0C,$18,$18,$18,$0C,$18,$18
        .byte   $18,$0C,$18,$18,$0C,$18,$0C,$18
        .byte   $18,$56,$01,$01,$01,$56,$01,$01
        .byte   $01,$01,$68,$14,$52,$0E,$01,$0E
        .byte   $01,$01,$0E,$52,$0E,$01,$01,$01
        .byte   $0E,$01,$01,$01,$14,$14,$14,$14
        .byte   $14,$14,$14,$14,$69,$FF,$00,$00
        .byte   $10,$00,$10,$08,$01,$40,$00,$00
        .byte   $00,$00,$10,$08,$00,$00,$00,$00
        .byte   $40,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$10,$00,$00,$00,$01,$00,$08
        .byte   $00,$00,$00,$34,$40,$00,$04,$80
        .byte   $00,$80,$00,$80,$00,$01,$01,$00
        .byte   $00,$10,$40,$00,$00,$10,$00,$00
        .byte   $00,$10,$00,$01,$00,$00,$00,$08
        .byte   $00,$18,$01,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$04,$00,$01,$00,$00,$00,$20
        .byte   $00,$00,$00,$00,$00,$02,$40,$10
        .byte   $00,$80,$00,$08,$00,$00,$00,$00
        .byte   $00,$00,$00,$28,$05,$01,$40,$40
        .byte   $00,$02,$00,$00,$00,$80,$04,$00
        .byte   $40,$00,$00,$00,$00,$00,$00,$00
        .byte   $01,$00,$00,$10,$01,$00,$00,$00
        .byte   $00,$02,$00,$02,$00,$00,$00,$01
        .byte   $00,$0C,$00,$04,$04,$40,$00,$00
        .byte   $04,$00,$00,$00,$00,$00,$00,$80
        .byte   $04,$08,$00,$10,$00,$40,$00,$00
        .byte   $00,$00,$10,$80,$00,$00,$00,$40
        .byte   $00,$00,$04,$10,$40,$04,$00,$00
        .byte   $04,$03,$01,$10,$04,$06,$00,$00
        .byte   $01,$00,$00,$02,$01,$00,$00,$00
        .byte   $03,$00,$04,$05,$00,$02,$00,$05
        .byte   $00,$06,$00,$00,$00,$00,$00,$00
        .byte   $00,$03,$05,$00,$04,$01,$00,$07
        .byte   $00,$00,$00,$08,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$09
        .byte   $0A,$0A,$09,$0B,$0C,$0D,$0A,$0E
        .byte   $0F,$10,$0E,$11,$12,$13,$0F,$14
        .byte   $00,$00,$00,$00,$00,$14,$00,$00
        .byte   $15,$04,$16,$17,$18,$00,$00,$05
        .byte   $00,$00,$19,$1A,$1B,$01,$05,$00
        .byte   $06,$1C,$1D,$1E,$1F,$00,$00,$04
        .byte   $00,$05,$00,$00,$05,$04,$00,$00
        .byte   $00,$20,$0A,$21,$20,$00,$00,$0A
        .byte   $22,$23,$24,$25,$0E,$00,$09,$10
        .byte   $13,$11,$26,$27,$0E,$00,$0E,$00
        .byte   $00,$00,$00,$03,$00,$00,$00,$06
        .byte   $01,$06,$00,$00,$05,$03,$00,$05
        .byte   $00,$00,$01,$15,$00,$00,$04,$00
        .byte   $03,$01,$00,$00,$03,$28,$00,$00
        .byte   $00,$00,$05,$00,$00,$00,$00,$00
        .byte   $0A,$0A,$0C,$0A,$00,$00,$00,$20
        .byte   $29,$2A,$2B,$2C,$0B,$0C,$0A,$0E
        .byte   $11,$12,$26,$0E,$11,$12,$2D,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $2E,$09,$0B,$0C,$20,$22,$20,$00
        .byte   $2F,$0E,$30,$31,$0E,$13,$0E,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $32,$00,$00,$00,$00,$33,$2E,$00
        .byte   $34,$00,$35,$36,$37,$34,$2F,$00
        .byte   $34,$00,$38,$39,$3A,$34,$2F,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$09
        .byte   $0B,$0C,$00,$0A,$0B,$0D,$0B,$0E
        .byte   $30,$12,$00,$3B,$30,$13,$11,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$09
        .byte   $0A,$00,$00,$00,$00,$3C,$3D,$24
        .byte   $10,$00,$3C,$3E,$3F,$30,$40,$26
        .byte   $12,$00,$39,$41,$3B,$42,$3B,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$43
        .byte   $3C,$44,$45,$00,$00,$00,$00,$38
        .byte   $39,$46,$47,$00,$20,$0A,$00,$38
        .byte   $39,$48,$38,$00,$0E,$10,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $20,$0A,$00,$00,$00,$00,$00,$09
        .byte   $23,$49,$20,$0B,$0C,$09,$20,$0E
        .byte   $11,$13,$0E,$11,$31,$0F,$4A,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$22
        .byte   $0C,$20,$00,$00,$0B,$0D,$20,$13
        .byte   $12,$0E,$00,$00,$11,$13,$0E,$4B
        .byte   $4C,$4D,$4E,$00,$4F,$4B,$4C,$50
        .byte   $51,$52,$52,$4E,$53,$4E,$54,$4B
        .byte   $4C,$4D,$55,$52,$52,$52,$52,$50
        .byte   $51,$56,$00,$55,$56,$50,$51,$4B
        .byte   $57,$58,$00,$59,$52,$5A,$53,$50
        .byte   $51,$52,$54,$5B,$52,$52,$52,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$5C
        .byte   $4B,$4C,$5C,$4B,$4C,$5C,$4B,$52
        .byte   $52,$52,$56,$52,$5D,$00,$5E,$52
        .byte   $52,$52,$4F,$4B,$4C,$4D,$5F,$56
        .byte   $50,$58,$60,$00,$61,$56,$61,$52
        .byte   $58,$00,$00,$61,$61,$61,$61,$52
        .byte   $5A,$00,$5F,$61,$61,$61,$61,$5C
        .byte   $4B,$4C,$5C,$4B,$4C,$5C,$62,$56
        .byte   $50,$51,$56,$50,$51,$56,$50,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$61
        .byte   $61,$50,$51,$56,$50,$51,$56,$61
        .byte   $61,$63,$55,$64,$52,$5B,$4F,$61
        .byte   $61,$00,$00,$56,$59,$52,$56,$61
        .byte   $00,$00,$00,$64,$00,$65,$4F,$61
        .byte   $61,$00,$00,$66,$00,$00,$56,$67
        .byte   $4F,$4B,$4C,$5C,$4B,$68,$4F,$69
        .byte   $6A,$6B,$69,$6A,$6B,$6C,$6A,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$6D,$64,$51
        .byte   $52,$5B,$52,$58,$60,$00,$56,$57
        .byte   $52,$60,$60,$00,$00,$00,$64,$51
        .byte   $60,$5F,$61,$61,$61,$61,$56,$57
        .byte   $00,$61,$61,$61,$61,$61,$64,$51
        .byte   $5A,$4E,$61,$61,$61,$00,$61,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$55
        .byte   $52,$52,$58,$00,$61,$6E,$63,$6F
        .byte   $58,$59,$00,$00,$61,$70,$63,$58
        .byte   $00,$00,$00,$61,$63,$5E,$61,$60
        .byte   $00,$00,$00,$61,$61,$61,$61,$61
        .byte   $63,$64,$4E,$61,$61,$61,$00,$61
        .byte   $71,$56,$5A,$61,$00,$61,$00,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$55
        .byte   $52,$52,$52,$52,$52,$52,$72,$59
        .byte   $52,$58,$52,$58,$55,$52,$50,$00
        .byte   $59,$00,$00,$00,$59,$55,$72,$00
        .byte   $00,$00,$00,$00,$00,$00,$50,$00
        .byte   $00,$5B,$5B,$5A,$00,$00,$73,$53
        .byte   $5B,$51,$56,$50,$51,$5A,$73,$5C
        .byte   $4B,$4C,$5C,$4B,$4C,$5C,$4B,$56
        .byte   $50,$51,$56,$50,$51,$56,$50,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$74
        .byte   $5D,$52,$52,$52,$52,$58,$73,$74
        .byte   $00,$59,$52,$52,$52,$4E,$73,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$75
        .byte   $76,$76,$76,$76,$76,$76,$51,$77
        .byte   $78,$78,$78,$78,$78,$78,$79,$7A
        .byte   $78,$78,$78,$78,$78,$78,$51,$7B
        .byte   $78,$78,$7C,$7D,$78,$78,$7E,$7F
        .byte   $80,$81,$50,$51,$82,$83,$84,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$5C
        .byte   $4B,$4C,$5C,$4B,$4C,$5C,$4B,$85
        .byte   $52,$52,$52,$52,$52,$58,$00,$86
        .byte   $52,$58,$60,$55,$52,$52,$5A,$87
        .byte   $55,$58,$00,$53,$58,$53,$52,$74
        .byte   $59,$79,$5C,$4B,$6D,$88,$4B,$74
        .byte   $00,$51,$56,$50,$89,$8A,$50,$5C
        .byte   $4B,$4C,$5C,$4B,$6D,$88,$4B,$56
        .byte   $50,$51,$56,$50,$89,$8A,$50,$4C
        .byte   $8B,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$4E
        .byte   $00,$00,$79,$5C,$4B,$4C,$5C,$52
        .byte   $5A,$50,$51,$56,$50,$51,$56,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$62
        .byte   $00,$00,$00,$00,$00,$00,$8C,$50
        .byte   $8D,$8D,$8D,$8D,$8D,$8D,$8D,$62
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$8E,$50
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$8E,$4B
        .byte   $4C,$4D,$8F,$8F,$8F,$8F,$8F,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$90
        .byte   $00,$00,$00,$00,$00,$00,$91,$8D
        .byte   $8D,$8D,$8D,$8D,$8D,$8D,$8D,$8E
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$8E,$8E
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$8E,$64
        .byte   $8F,$8F,$8F,$8F,$8F,$8F,$72,$56
        .byte   $50,$51,$56,$50,$51,$56,$50,$00
        .byte   $00,$00,$00,$00,$00,$00,$92,$00
        .byte   $00,$00,$00,$00,$00,$00,$92,$8C
        .byte   $00,$00,$00,$00,$00,$00,$92,$8D
        .byte   $8D,$8D,$8D,$8D,$8D,$8D,$92,$8E
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$92,$8E
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$93,$57
        .byte   $8F,$8F,$8F,$8F,$8F,$79,$5C,$51
        .byte   $56,$50,$51,$56,$50,$51,$56,$94
        .byte   $95,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $96,$55,$58,$59,$55,$52,$51,$94
        .byte   $96,$00,$00,$00,$00,$59,$79,$50
        .byte   $97,$97,$97,$97,$97,$97,$51,$94
        .byte   $98,$98,$98,$98,$98,$98,$79,$99
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$51,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$9A,$50
        .byte   $55,$52,$52,$58,$60,$00,$00,$62
        .byte   $6F,$58,$60,$00,$00,$9B,$9C,$50
        .byte   $58,$00,$9D,$9E,$9F,$A0,$A1,$62
        .byte   $A2,$00,$A3,$00,$55,$A4,$A5,$50
        .byte   $00,$00,$00,$53,$5B,$50,$51,$62
        .byte   $A6,$5C,$4B,$4C,$5C,$4B,$4C,$6B
        .byte   $A7,$6A,$6B,$69,$6A,$6B,$69,$5C
        .byte   $4B,$4C,$5C,$A8,$4C,$5C,$4B,$55
        .byte   $50,$51,$58,$55,$52,$56,$50,$00
        .byte   $55,$60,$00,$00,$5D,$52,$5D,$9D
        .byte   $A9,$00,$AA,$9E,$AB,$00,$00,$A3
        .byte   $00,$00,$96,$00,$A3,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$50,$5C
        .byte   $62,$00,$4F,$4B,$4C,$5C,$4B,$56
        .byte   $50,$00,$56,$50,$51,$56,$50,$4C
        .byte   $5C,$A8,$4C,$AC,$4B,$4C,$5C,$51
        .byte   $58,$59,$58,$52,$50,$51,$56,$00
        .byte   $00,$00,$00,$59,$52,$79,$5C,$00
        .byte   $AD,$8C,$AE,$00,$59,$52,$56,$00
        .byte   $96,$00,$AF,$9F,$9D,$65,$4F,$00
        .byte   $00,$00,$B0,$00,$B1,$00,$56,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$68,$4F,$69
        .byte   $6A,$6B,$69,$6A,$6B,$6C,$6A,$4C
        .byte   $AC,$4B,$4C,$5C,$A8,$B2,$4F,$51
        .byte   $58,$60,$55,$5D,$00,$A3,$56,$57
        .byte   $B3,$AE,$00,$B4,$B5,$A3,$4F,$51
        .byte   $60,$B1,$00,$B1,$A3,$53,$56,$57
        .byte   $B4,$90,$B4,$91,$B1,$5B,$4F,$51
        .byte   $00,$00,$00,$00,$B6,$B7,$56,$57
        .byte   $B8,$67,$67,$67,$67,$67,$4F,$69
        .byte   $96,$6B,$69,$6A,$6B,$69,$6A,$57
        .byte   $96,$72,$4C,$5C,$4B,$4C,$5C,$51
        .byte   $B9,$B9,$B9,$B9,$B9,$B9,$56,$57
        .byte   $BA,$BA,$BA,$BA,$BA,$BA,$BB,$51
        .byte   $8E,$8E,$8E,$8E,$8E,$8E,$BC,$57
        .byte   $8E,$8E,$8E,$8E,$8E,$79,$5C,$51
        .byte   $8E,$8E,$8E,$56,$50,$51,$56,$4C
        .byte   $5C,$4B,$4C,$5C,$4B,$4C,$5C,$69
        .byte   $6A,$6B,$69,$6A,$6B,$69,$6A,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$BD
        .byte   $BE,$BE,$BE,$BE,$BE,$BE,$BF,$74
        .byte   $00,$00,$00,$00,$00,$00,$73,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$4B
        .byte   $4C,$5C,$4B,$4C,$5C,$4B,$4C,$50
        .byte   $51,$56,$50,$51,$56,$50,$51,$5C
        .byte   $4B,$4C,$5C,$4B,$4C,$5C,$4B,$C0
        .byte   $C1,$C1,$C1,$C1,$C1,$C1,$C2,$C3
        .byte   $C4,$C4,$C4,$C4,$C4,$C4,$C5,$C6
        .byte   $C4,$C4,$C4,$C4,$C4,$C4,$C7,$C8
        .byte   $C4,$C4,$C4,$C4,$C4,$C4,$C5,$C9
        .byte   $C4,$C4,$C4,$C4,$C4,$C4,$C7,$5C
        .byte   $4B,$4C,$5C,$4B,$4C,$5C,$4B,$56
        .byte   $50,$51,$56,$50,$51,$56,$50,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$43,$00
        .byte   $00,$00,$41,$00,$00,$42,$00,$00
        .byte   $42,$00,$00,$43,$00,$00,$00,$41
        .byte   $00,$00,$00,$00,$43,$00,$00,$00
        .byte   $41,$00,$00,$70,$71,$78,$7B,$70
        .byte   $71,$78,$79,$70,$71,$78,$70,$70
        .byte   $71,$71,$79,$70,$72,$71,$7A,$78
        .byte   $7A,$78,$7A,$78,$70,$78,$78,$71
        .byte   $79,$79,$79,$78,$78,$78,$78,$79
        .byte   $79,$79,$79,$79,$7A,$79,$7A,$00
        .byte   $00,$00,$42,$43,$00,$00,$43,$00
        .byte   $00,$00,$4A,$00,$44,$4B,$4C,$45
        .byte   $46,$4D,$4E,$51,$52,$59,$5A,$53
        .byte   $54,$5B,$5C,$55,$56,$5D,$00,$00
        .byte   $00,$00,$68,$61,$62,$69,$6A,$63
        .byte   $64,$6B,$00,$65,$00,$00,$00,$70
        .byte   $72,$78,$7A,$70,$71,$72,$79,$70
        .byte   $71,$71,$7B,$78,$7A,$78,$70,$78
        .byte   $70,$71,$78,$7A,$7B,$7A,$7A,$79
        .byte   $78,$79,$78,$7A,$7A,$7A,$7A,$41
        .byte   $00,$00,$43,$78,$79,$78,$70,$78
        .byte   $79,$71,$79,$79,$79,$79,$70,$78
        .byte   $7B,$78,$7A,$72,$79,$7A,$79,$00
        .byte   $72,$00,$7A,$00,$7A,$00,$7A,$71
        .byte   $78,$79,$78,$79,$7B,$79,$7A,$00
        .byte   $00,$70,$00,$70,$00,$78,$00,$78
        .byte   $00,$78,$00,$00,$00,$72,$00,$00
        .byte   $00,$00,$70,$00,$00,$71,$00,$7A
        .byte   $00,$7A,$00,$00,$78,$00,$78,$79
        .byte   $00,$79,$00,$78,$79,$78,$79,$00
        .byte   $70,$00,$78,$71,$70,$7B,$78,$71
        .byte   $70,$79,$78,$71,$70,$70,$71,$7A
        .byte   $78,$70,$71,$79,$71,$79,$79,$79
        .byte   $72,$79,$7A,$72,$00,$7A,$00,$71
        .byte   $70,$79,$71,$71,$00,$79,$00,$79
        .byte   $79,$7B,$79,$7B,$00,$7A,$00,$7A
        .byte   $79,$7A,$79,$78,$7B,$71,$7A,$71
        .byte   $7A,$79,$7A,$11,$12,$19,$1A,$13
        .byte   $14,$1B,$1C,$15,$16,$1D,$16,$00
        .byte   $00,$28,$00,$1F,$10,$1F,$18,$14
        .byte   $15,$1C,$1D,$10,$11,$18,$19,$20
        .byte   $21,$28,$29,$00,$00,$00,$29,$00
        .byte   $00,$28,$29,$20,$21,$00,$29,$12
        .byte   $13,$1A,$1B,$13,$1F,$1B,$1F,$20
        .byte   $21,$28,$00,$00,$21,$00,$00,$20
        .byte   $00,$28,$29,$00,$21,$28,$29,$15
        .byte   $10,$1D,$18,$20,$21,$00,$00,$00
        .byte   $09,$00,$09,$00,$09,$09,$09,$20
        .byte   $00,$00,$00,$09,$09,$09,$09,$11
        .byte   $17,$19,$17,$09,$00,$09,$00,$1F
        .byte   $16,$1F,$16,$00,$21,$00,$29,$17
        .byte   $17,$00,$00,$00,$00,$08,$08,$13
        .byte   $0F,$1B,$07,$16,$16,$16,$16,$17
        .byte   $17,$17,$17,$1F,$1F,$1F,$1F,$16
        .byte   $07,$16,$07,$13,$00,$1B,$00,$09
        .byte   $09,$00,$00,$00,$21,$28,$00,$00
        .byte   $00,$09,$09,$00,$00,$09,$00,$16
        .byte   $12,$16,$1A,$00,$06,$00,$06,$05
        .byte   $00,$05,$00,$1F,$31,$1F,$30,$31
        .byte   $31,$30,$30,$11,$30,$19,$30,$30
        .byte   $30,$30,$30,$17,$14,$17,$1C,$1F
        .byte   $30,$1F,$30,$04,$30,$03,$30,$30
        .byte   $30,$30,$17,$30,$30,$17,$30,$31
        .byte   $06,$30,$06,$03,$30,$03,$30,$30
        .byte   $30,$30,$16,$30,$17,$17,$17,$17
        .byte   $30,$17,$17,$30,$30,$1F,$30,$30
        .byte   $06,$30,$06,$17,$21,$17,$29,$15
        .byte   $21,$1D,$29,$17,$00,$17,$00,$00
        .byte   $10,$00,$18,$16,$00,$16,$00,$00
        .byte   $17,$00,$17,$15,$16,$84,$16,$17
        .byte   $1F,$00,$00,$22,$23,$38,$38,$38
        .byte   $38,$38,$38,$38,$38,$08,$08,$16
        .byte   $17,$00,$00,$1F,$16,$00,$00,$0A
        .byte   $0B,$0A,$0B,$0C,$0D,$38,$38,$16
        .byte   $17,$16,$17,$07,$14,$07,$1C,$07
        .byte   $00,$07,$00,$22,$23,$2A,$2B,$32
        .byte   $33,$38,$38,$1F,$1F,$38,$38,$13
        .byte   $14,$83,$1C,$0F,$17,$07,$00,$1F
        .byte   $0F,$00,$07,$00,$00,$16,$0F,$00
        .byte   $00,$1F,$16,$00,$00,$17,$1F,$00
        .byte   $00,$16,$12,$00,$00,$13,$1F,$28
        .byte   $00,$00,$00,$00,$07,$00,$07,$20
        .byte   $1A,$28,$20,$83,$00,$00,$00,$0F
        .byte   $14,$07,$1C,$07,$16,$07,$16,$11
        .byte   $12,$81,$1A,$00,$00,$1F,$00,$00
        .byte   $00,$0F,$17,$00,$00,$17,$0F,$15
        .byte   $10,$1D,$80,$0F,$16,$07,$00,$16
        .byte   $0F,$00,$07,$00,$00,$10,$11,$80
        .byte   $19,$00,$00,$00,$07,$00,$00,$13
        .byte   $07,$1B,$07,$1F,$16,$28,$29,$00
        .byte   $0F,$00,$07,$17,$0F,$00,$07,$1F
        .byte   $1F,$00,$00,$16,$16,$00,$00,$0F
        .byte   $00,$07,$00,$00,$00,$22,$23,$2A
        .byte   $2B,$32,$33,$2A,$06,$32,$06,$38
        .byte   $06,$38,$06,$85,$87,$8D,$8F,$86
        .byte   $87,$8E,$8F,$86,$06,$8E,$06,$17
        .byte   $39,$17,$02,$67,$39,$02,$02,$67
        .byte   $1F,$02,$1F,$04,$01,$03,$01,$01
        .byte   $01,$01,$01,$01,$12,$01,$1A,$03
        .byte   $01,$03,$01,$01,$1F,$01,$1F,$15
        .byte   $01,$1D,$01,$17,$01,$17,$01,$00
        .byte   $00,$1F,$0F,$00,$00,$16,$17,$08
        .byte   $08,$1F,$16,$08,$08,$16,$17,$08
        .byte   $08,$17,$1F,$0F,$16,$07,$10,$08
        .byte   $08,$11,$12,$08,$08,$13,$14,$08
        .byte   $08,$15,$10,$1F,$10,$15,$18,$07
        .byte   $18,$07,$17,$19,$1A,$14,$15,$1B
        .byte   $1C,$10,$11,$1D,$18,$12,$13,$1D
        .byte   $17,$12,$13,$07,$10,$07,$18,$50
        .byte   $2B,$58,$33,$05,$38,$05,$38,$39
        .byte   $39,$02,$02,$39,$1F,$02,$1F,$00
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
        .byte   $13,$02,$13,$02,$00,$14,$04,$22
        .byte   $24,$44,$00,$44,$00,$01,$04,$08
        .byte   $0A,$08,$0A,$0C,$0E,$06,$06,$28
        .byte   $2A,$28,$2A,$2C,$2E,$00,$26,$4C
        .byte   $4E,$40,$42,$00,$8A,$8C,$00,$6C
        .byte   $6E,$60,$62,$00,$AA,$AC,$AE,$13
        .byte   $02,$62,$60,$C8,$CA,$CC,$CE,$00
        .byte   $50,$00,$8F,$E8,$EA,$EC,$EE,$44
        .byte   $77,$76,$00,$00,$68,$6A,$4B,$44
        .byte   $67,$00,$82,$84,$00,$74,$01,$60
        .byte   $00,$A0,$A2,$94,$A6,$EB,$01,$62
        .byte   $00,$C0,$C1,$B5,$C6,$01,$01,$20
        .byte   $B0,$E0,$E2,$E4,$E6,$01,$50,$00
        .byte   $D2,$D5,$E1,$01,$01,$01,$00,$48
        .byte   $4A,$4A,$01,$E0,$E2,$E4,$E6,$58
        .byte   $5A,$5A,$5A,$F8,$FA,$FC,$FE,$20
        .byte   $54,$00,$54,$10,$60,$60,$62,$00
        .byte   $00,$00,$00,$00,$62,$62,$60,$00
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
        .byte   $12,$03,$12,$03,$00,$15,$05,$23
        .byte   $25,$45,$46,$45,$46,$01,$05,$09
        .byte   $0B,$09,$0B,$0D,$0F,$07,$07,$29
        .byte   $2B,$29,$2B,$2D,$2F,$00,$27,$4D
        .byte   $4F,$41,$43,$89,$8B,$8D,$00,$6D
        .byte   $6F,$61,$63,$A9,$AB,$AD,$00,$12
        .byte   $03,$00,$61,$C9,$CB,$CD,$CF,$00
        .byte   $51,$88,$00,$E9,$00,$ED,$EF,$44
        .byte   $00,$00,$00,$00,$69,$6B,$00,$66
        .byte   $44,$81,$83,$85,$00,$75,$00,$61
        .byte   $80,$A1,$A3,$95,$A7,$00,$00,$00
        .byte   $87,$C1,$B4,$C5,$C7,$01,$00,$21
        .byte   $B1,$D0,$E3,$E5,$00,$00,$51,$C4
        .byte   $D3,$D6,$00,$01,$01,$00,$00,$49
        .byte   $4B,$4B,$01,$E1,$E3,$E5,$E7,$59
        .byte   $5B,$57,$47,$F9,$FB,$FD,$FF,$21
        .byte   $55,$00,$55,$11,$61,$61,$63,$00
        .byte   $00,$00,$00,$00,$00,$00,$61,$00
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
        .byte   $12,$12,$12,$03,$00,$14,$04,$32
        .byte   $34,$44,$00,$64,$66,$01,$04,$18
        .byte   $1A,$18,$1A,$1C,$1E,$16,$16,$38
        .byte   $3A,$38,$3A,$3C,$3E,$00,$36,$5C
        .byte   $5E,$70,$72,$00,$9A,$9C,$9E,$7C
        .byte   $7E,$70,$72,$B8,$BA,$BC,$BE,$12
        .byte   $03,$00,$00,$D8,$DA,$DC,$DE,$00
        .byte   $52,$00,$9F,$F8,$FA,$FC,$FE,$44
        .byte   $00,$00,$00,$00,$78,$7A,$5B,$54
        .byte   $01,$90,$92,$94,$96,$AF,$01,$70
        .byte   $00,$A0,$B2,$B4,$B6,$00,$01,$00
        .byte   $00,$D0,$D1,$D4,$E5,$01,$00,$30
        .byte   $C2,$00,$F2,$F4,$00,$01,$52,$00
        .byte   $F0,$F7,$00,$01,$00,$00,$00,$58
        .byte   $5A,$5A,$00,$F0,$F2,$F4,$F6,$58
        .byte   $5A,$5A,$5A,$F8,$FA,$FC,$FE,$00
        .byte   $56,$00,$56,$30,$70,$70,$72,$00
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
        .byte   $13,$13,$13,$02,$00,$15,$05,$33
        .byte   $35,$45,$46,$65,$67,$01,$05,$19
        .byte   $1B,$19,$1B,$1D,$1F,$17,$17,$39
        .byte   $3B,$39,$3B,$3D,$3F,$00,$37,$5D
        .byte   $5F,$71,$73,$99,$9B,$9D,$00,$7D
        .byte   $7F,$71,$73,$B9,$BB,$BD,$BF,$13
        .byte   $02,$00,$00,$D9,$DB,$DD,$DF,$00
        .byte   $53,$98,$00,$F9,$FB,$FD,$FF,$44
        .byte   $76,$00,$77,$A8,$79,$7B,$00,$01
        .byte   $01,$91,$93,$95,$97,$00,$00,$71
        .byte   $86,$A1,$B3,$B5,$B7,$00,$00,$00
        .byte   $A5,$D1,$B5,$E4,$D7,$00,$00,$31
        .byte   $C3,$F1,$F3,$F5,$00,$00,$53,$E7
        .byte   $F6,$A4,$00,$01,$00,$00,$00,$59
        .byte   $5B,$47,$00,$F1,$F3,$F5,$F7,$59
        .byte   $5B,$57,$57,$F9,$FB,$FD,$FF,$31
        .byte   $00,$00,$00,$00,$71,$71,$73,$00
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
        .byte   $83,$83,$13,$13,$10,$10,$23,$53
        .byte   $73,$13,$13,$13,$13,$00,$43,$10
        .byte   $10,$11,$11,$11,$11,$10,$11,$10
        .byte   $10,$11,$11,$11,$11,$00,$11,$62
        .byte   $62,$82,$82,$03,$03,$03,$03,$62
        .byte   $62,$82,$82,$03,$03,$03,$03,$03
        .byte   $03,$82,$82,$03,$03,$03,$03,$82
        .byte   $50,$03,$03,$03,$03,$03,$03,$00
        .byte   $00,$00,$00,$01,$01,$01,$01,$00
        .byte   $00,$02,$02,$02,$01,$01,$00,$12
        .byte   $02,$02,$02,$02,$01,$01,$00,$12
        .byte   $01,$02,$02,$01,$01,$00,$00,$00
        .byte   $01,$01,$01,$01,$01,$00,$51,$01
        .byte   $01,$01,$01,$00,$00,$00,$00,$13
        .byte   $13,$13,$00,$00,$00,$00,$00,$13
        .byte   $13,$13,$13,$00,$00,$00,$00,$60
        .byte   $60,$00,$61,$61,$12,$02,$02,$00
        .byte   $00,$00,$00,$00,$12,$02,$02,$00
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
        .byte   $00,$00,$00,$00,$00,$00,$00
