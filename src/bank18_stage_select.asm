; =============================================================================
; MEGA MAN 3 (U) — BANK $18 — STAGE SELECT + PROTO MAN SCENES
; =============================================================================
; Mapped to $A000-$BFFF. Contains stage select screen logic (grid layout,
; cursor movement, portrait rendering, palette flash), robot master intro
; transitions, Proto Man encounter scenes, and Wily gate sequences.
;
; Major sections:
;   $A000-$AFFF  Compressed nametable/palette data (stage select screen layout)
;   $9000-$9008  Jump table: robot_master_intro, password_screen_entry, wily_gate_entry
;   $9009-$90B3  Title screen → stage select transition (CHR bank load, palette init)
;   $90B4-$90CF  Title screen Start button wait loop
;   $90D0-$92FF  Stage select screen (CHR/palette init, cursor, d-pad, bolt sprites)
;   $9300-$938A  Stage select confirmation → bank03 boss intro handoff
;   $938B-$939D  (dead code: unused bank-call helper)
;   $939E-$93E8  write_ppu_data_from_bank03 helper
;   $93E9-$940F  Password cursor OAM setup
;   $9410-$9580  Robot Master intro (boss drop, Mega Man teleport, palette fade)
;   $9581-$968B  Doc Robot transition / all-bosses-beaten stage select return
;   $968C-$970A  Normal stage return → stage select
;   $970B-$97EB  write_portrait_frames / write_portrait_faces
;   $97EC-$985C  write_center_portrait
;   $985D-$9935  Password screen entry point
;   $9936-$995B  reset_stage_state
;   $995C-$99F9  Defeated portrait blanking / frame restoration
;   $99FA-$9ABB  OAM sprite loading (stage select portrait sprites from bank03)
;   $9ABC-$9BB6  Wily fortress gate entrance sequence
;   $9BB7-$9FFF  Data tables: boss palettes, CHR banks, eye sprites, lookup tables
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"

title_screen_wait_start           := $90B4
stage_select_cursor_init           := $9155
stage_select_proto_man_oam           := $9212
stage_select_input_loop           := $9258
stage_select_confirm           := $9300
write_ppu_data_from_table           := $939E
password_cursor_oam_setup           := $93E9
proto_man_sprite_control           := $93FE
robot_master_intro           := $9410
robot_master_intro_loop           := $94D6
boss_energy_fill_loop           := $954A
stage_loading_entry           := $9581
boss_intro_sprite_loop           := $9681
normal_stage_return           := $968C
boss_anim_state_check           := $96FC
write_portrait_frames           := $970B
write_portrait_faces           := $9762
write_center_portrait           := $97EC
password_screen_entry           := $985D
proto_man_anim_loop           := $98F2
reset_stage_state           := $9936
draw_defeated_portraits           := $995C
write_portrait_frame_data           := $99DC
load_stage_select_oam           := $99FA
portrait_oam_write_loop           := $9A58
load_wily_center_face           := $9A87
wily_gate_entry           := $9ABC
wily_gate_setup           := $9ABE
drain_ppu_buffer           := $C4F8
rendering_off           := $C531
rendering_on           := $C53B
fill_nametable           := $C59D
prepare_oam_buffer           := $C5E9
clear_entity_table           := $C628
fade_palette_out           := $C74C
fade_palette_in           := $C752
game_entry_set_hp_scroll           := $C9B3
do_render_column           := $E4F1
metatile_column_ptr_by_id           := $E8B4
queue_metatile_update           := $EEAB
fill_nametable_progressive           := $EF8C
reset_gravity           := $F81B
reset_sprite_anim           := $F835
submit_sound_ID_D9           := $F898
submit_sound_ID           := $F89A
process_frame_yield_full           := $FD6E
task_yield_x           := $FF1A
task_yield           := $FF21
update_CHR_banks           := $FF3C
select_CHR_banks           := $FF45
select_PRG_banks           := $FF6B

; =============================================================================
; COMPRESSED NAMETABLE / PALETTE DATA ($A000-$AFFF)
; =============================================================================
; Stage select screen layout data (compressed). Contains nametable tile layouts,
; attribute tables, and palette data for the stage select screen background,
; portrait frames, and Proto Man cutscene screens.
; =============================================================================

.segment "BANK18"

data_compressed_nametable_start:  .byte   $28,$30,$50,$70,$01,$90,$0D,$00
        .byte   $AF,$8D,$88,$12,$28,$C0,$16,$CA
        .byte   $8A,$8D,$0E,$01,$BF,$EB,$02,$00
        .byte   $CA,$17,$17,$17,$17,$00,$C0,$26
        .byte   $C2,$AD,$C4,$0A,$C5,$32,$0A,$FE
        .byte   $05,$01,$11,$06,$C8,$07,$0C,$08
        .byte   $26,$18,$80,$09,$03,$00,$03,$8A
        .byte   $8D,$04,$48,$2D,$0D,$28,$2F,$4F
        .byte   $02,$8F,$00,$01,$AF,$8D,$D1,$0D
        .byte   $00,$8A,$8D,$01,$2D,$0D,$28,$30
        .byte   $50,$70,$01,$90,$0D,$00,$AF,$8D
        .byte   $88,$12,$28,$C0,$64,$CA,$8A,$8D
        .byte   $0E,$01,$C0,$39,$05,$00,$CC,$AA
        .byte   $05,$00,$88,$AD,$05,$00,$66,$AF
        .byte   $04,$48,$06,$64,$08,$11,$05,$01
        .byte   $11,$2F,$0D,$28,$31,$51,$02,$01
        .byte   $91,$0D,$00,$6A,$6D,$71,$01,$2D
        .byte   $0D,$28,$2F,$4F,$01,$8F,$0D,$00
        .byte   $8D,$6C,$02,$8D,$12,$08,$C0,$AF
        .byte   $8A,$6A,$8C,$6C,$0D,$28,$01,$2B
        .byte   $2D,$4D,$02,$01,$8D,$0D,$00,$8C
        .byte   $6A,$02,$8C,$0E,$01,$C0,$70,$01
        .byte   $2F,$0D,$28,$31,$51,$01,$71,$0D
        .byte   $00,$71,$8F,$01,$6F,$0D,$28,$01
        .byte   $B1,$0D,$00,$09,$02,$08,$20,$6A
        .byte   $6D,$71,$6F,$6D,$6C,$08,$11,$04
        .byte   $48,$09,$03,$2F,$0D,$28,$31,$51
        .byte   $02,$01,$91,$0D,$00,$6A,$6D,$71
        .byte   $01,$2D,$0D,$28,$2F,$4F,$01,$8F
        .byte   $0D,$00,$8D,$6C,$02,$8D,$12,$08
        .byte   $C1,$09,$8A,$6A,$8C,$6C,$0D,$28
        .byte   $01,$2B,$2D,$4D,$02,$01,$8D,$0D
        .byte   $00,$8C,$6A,$02,$8C,$0E,$01,$C0
        .byte   $CF,$09,$02,$01,$34,$0D,$28,$36
        .byte   $56,$01,$76,$76,$98,$78,$09,$03
        .byte   $01,$CD,$01,$8D,$0D,$00,$09,$02
        .byte   $08,$27,$00,$26,$2A,$2D,$00,$01
        .byte   $51,$02,$01,$D1,$02,$80,$04,$00
        .byte   $08,$11,$18,$80,$09,$03,$05,$01
        .byte   $11,$04,$00,$A0,$60,$76,$79,$7B
        .byte   $00,$5B,$0D,$3C,$00,$9C,$02,$9B
        .byte   $00,$03,$88,$86,$12,$28,$C1,$83
        .byte   $0D,$00,$00,$A5,$60,$00,$86,$88
        .byte   $00,$85,$61,$02,$81,$00,$40,$00
        .byte   $61,$63,$0D,$3C,$A4,$60,$00,$86
        .byte   $88,$0D,$00,$00,$83,$03,$77,$02
        .byte   $97,$00,$40,$00,$74,$77,$B9,$60
        .byte   $79,$7B,$79,$98,$75,$B1,$60,$0E
        .byte   $01,$C1,$39,$00,$A5,$60,$00,$86
        .byte   $88,$0D,$00,$00,$8A,$66,$02,$83
        .byte   $00,$40,$00,$03,$76,$78,$B9,$60
        .byte   $79,$7B,$79,$98,$96,$94,$98,$02
        .byte   $D6,$80,$7D,$7E,$04,$40,$3E,$0D
        .byte   $28,$03,$28,$48,$01,$68,$0D,$00
        .byte   $68,$88,$88,$02,$88,$86,$85,$83
        .byte   $03,$98,$79,$98,$96,$B4,$02,$80
        .byte   $12,$00,$C1,$CA,$7D,$7E,$0E,$01
        .byte   $C1,$A4,$03,$68,$69,$01,$29,$0D
        .byte   $28,$2B,$4B,$01,$6B,$0D,$00,$6B
        .byte   $8B,$8B,$02,$8B,$89,$88,$86,$83
        .byte   $64,$83,$03,$97,$02,$99,$99,$9B
        .byte   $9C,$D7,$80,$01,$03,$29,$0D,$28
        .byte   $2B,$4B,$01,$6B,$0D,$00,$89,$88
        .byte   $A6,$60,$66,$68,$66,$C5,$A0,$60
        .byte   $03,$76,$79,$7B,$00,$5B,$0D,$3C
        .byte   $00,$9C,$02,$9B,$00,$03,$88,$86
        .byte   $0D,$00,$00,$A5,$60,$00,$86,$88
        .byte   $00,$8A,$66,$02,$83,$00,$40,$00
        .byte   $03,$76,$78,$B9,$60,$79,$7B,$79
        .byte   $98,$96,$94,$98,$D6,$09,$01,$08
        .byte   $1C
stage_select_oam_y_table:  .byte   $03
stage_select_oam_tile_table:  .byte   $71
stage_select_oam_attr_table:  .byte   $71
stage_select_oam_x_table:  .byte   $6F,$91,$09,$02,$6A,$72,$71,$04
        .byte   $08,$08,$20,$B2,$60,$08,$1C,$6A
        .byte   $72,$71,$08,$20,$B2,$60,$08,$1C
        .byte   $68,$72,$71,$94,$72,$92,$91,$6F
        .byte   $6F,$12,$08,$C2,$63,$6D,$6F,$91
        .byte   $68,$72,$71,$0E,$01,$C2,$3B,$71
        .byte   $6A,$88,$08,$1C,$66,$66,$65,$08
        .byte   $20,$A5,$60,$08,$1C,$66,$66,$65
        .byte   $08,$20,$A5,$60,$65,$6A,$6C,$AF
        .byte   $8D,$6C,$AD,$02,$80,$6D,$6F,$B1
        .byte   $60,$6D,$6D,$6F,$91,$8F,$91,$92
        .byte   $8F,$05,$01,$00,$8C,$05,$00,$F0
        .byte   $8C,$05,$00,$DF,$88,$05,$00,$CC
        .byte   $88,$05,$00,$BD,$85,$05,$00,$88
        .byte   $86,$05,$00,$44,$88,$16,$C1,$2E
        .byte   $17,$06,$64,$07,$0C,$08,$20,$18
        .byte   $80,$09,$01,$00,$A0,$00,$E0,$E0
        .byte   $E0,$E0,$04,$08,$AA,$60,$71,$6D
        .byte   $A8,$A0,$60,$12,$08,$C2,$D8,$86
        .byte   $66,$88,$68,$02,$AA,$66,$02,$88
        .byte   $0E,$01,$C2,$BE,$8D,$6D,$8C,$6C
        .byte   $01,$CD,$01,$8D,$04,$08,$AA,$60
        .byte   $71,$6D,$A8,$A0,$60,$12,$08,$C2
        .byte   $FA,$86,$66,$88,$68,$02,$AA,$66
        .byte   $02,$88,$0E,$01,$C2,$E0,$8D,$6D
        .byte   $8F,$6F,$01,$D1,$01,$91,$E0,$04
        .byte   $00,$18,$40,$04,$00,$96,$99,$9D
        .byte   $99,$95,$98,$9D,$98,$94,$97,$99
        .byte   $9D,$12,$00,$C3,$31,$92,$96,$99
        .byte   $9D,$92,$95,$99,$9C,$90,$94,$97
        .byte   $9B,$8F,$92,$96,$99,$91,$95,$98
        .byte   $95,$0E,$01,$C3,$07,$93,$96,$99
        .byte   $9D,$92,$96,$99,$9D,$94,$98,$9B
        .byte   $9D,$8A,$8E,$91,$96,$9A,$9D,$03
        .byte   $8A,$8E,$04,$00,$8F,$92,$96,$99
        .byte   $8F,$94,$98,$94,$8D,$91,$94,$91
        .byte   $8D,$91,$94,$91,$0E,$01,$C3,$46
        .byte   $92,$95,$99,$9C,$8B,$8F,$92,$95
        .byte   $90,$94,$97,$9B,$93,$96,$99,$9C
        .byte   $92,$95,$99,$9C,$92,$95,$99,$9C
        .byte   $99,$9E,$03,$88,$86,$81,$85,$88
        .byte   $85,$03,$96,$99,$9D,$99,$95,$98
        .byte   $9D,$98,$94,$97,$99,$9D,$93,$96
        .byte   $99,$9D,$92,$96,$99,$9D,$94,$98
        .byte   $9B,$9D,$8A,$8E,$91,$96,$08,$1C
        .byte   $03,$76,$76,$76,$01,$76,$01,$B6
        .byte   $18,$80,$04,$00,$08,$20,$6F,$72
        .byte   $76,$79,$7B,$7E,$03,$6A,$66,$03
        .byte   $74,$78,$7B,$7E,$03,$68,$6C,$6F
        .byte   $6C,$03,$6D,$71,$74,$78,$79,$7D
        .byte   $03,$68,$6D,$71,$6D,$68,$65,$61
        .byte   $03,$76,$72,$71,$0E,$01,$C3,$A6
        .byte   $04,$00,$71,$75,$78,$7B,$7E,$7B
        .byte   $78,$75,$0E,$01,$C3,$D4,$76,$79
        .byte   $7D,$79,$74,$78,$7B,$78,$72,$76
        .byte   $79,$7D,$7E,$03,$6A,$6D,$71,$18
        .byte   $40,$03,$8F,$92,$96,$99,$8F,$92
        .byte   $96,$99,$94,$98,$9B,$9E,$94,$98
        .byte   $9B,$9E,$16,$C3,$03,$17,$06,$B4
        .byte   $08,$00,$09,$03,$00,$A0,$00,$E0
        .byte   $E0,$E0,$E0,$01,$AA,$02,$01,$8A
        .byte   $01,$C5,$01,$65,$02,$86,$02,$88
        .byte   $02,$AA,$A8,$01,$A6,$02,$01,$86
        .byte   $01,$C5,$01,$65,$02,$86,$02,$88
        .byte   $AA,$8A,$91,$88,$01,$A6,$02,$01
        .byte   $86,$01,$C5,$01,$65,$02,$86,$02
        .byte   $88,$02,$AA,$A8,$01,$A6,$02,$01
        .byte   $86,$01,$C5,$01,$65,$02,$83,$02
        .byte   $85,$01,$C6,$01,$86,$C0,$80,$88
        .byte   $8D,$8C,$04,$40,$AA,$01,$6A,$6A
        .byte   $6D,$6A,$02,$A9,$89,$01,$A8,$01
        .byte   $68,$68,$6D,$68,$02,$A6,$86,$01
        .byte   $A6,$01,$66,$69,$68,$66,$02,$A4
        .byte   $84,$01,$A3,$01,$63,$63,$68,$66
        .byte   $02,$A5,$65,$68,$04,$40,$AA,$01
        .byte   $6A,$6A,$6D,$6A,$02,$A9,$89,$01
        .byte   $A8,$01,$68,$68,$6D,$68,$02,$A7
        .byte   $87,$01,$A6,$01,$66,$66,$6D,$66
        .byte   $A8,$8D,$8C,$01,$CA,$01,$8A,$6E
        .byte   $6F,$71,$70,$71,$68,$13,$00,$C4
        .byte   $F2,$04,$40,$A3,$01,$63,$63,$65
        .byte   $66,$02,$A8,$68,$6A,$01,$A1,$01
        .byte   $61,$81,$61,$C1,$0E,$01,$C4,$B5
        .byte   $01,$A6,$01,$66,$66,$68,$69,$02
        .byte   $AB,$6B,$69,$02,$A4,$64,$66,$02
        .byte   $A7,$6B,$69,$02,$A6,$6B,$6D,$8B
        .byte   $02,$A6,$01,$A8,$01,$68,$68,$6B
        .byte   $6C,$CD,$0F,$01,$C4,$88,$04,$40
        .byte   $A3,$01,$63,$63,$65,$66,$02,$A8
        .byte   $68,$6A,$01,$A1,$01,$61,$81,$61
        .byte   $02,$A1,$61,$63,$0E,$01,$C4,$F2
        .byte   $01,$A5,$01,$65,$65,$66,$68,$01
        .byte   $A5,$01,$65,$65,$6D,$6C,$AA,$A8
        .byte   $02,$A6,$66,$65,$02,$A3,$83,$01
        .byte   $A3,$01,$63,$63,$65,$66,$A8,$AA
        .byte   $AC,$AF,$16,$C4,$5E,$17,$17,$06
        .byte   $02,$6F,$03,$0A,$1F,$35,$40,$0F
        .byte   $90,$08,$41,$1D,$31,$0F,$2A,$7F
        .byte   $10,$02,$82,$02,$0A,$00,$41,$00
        .byte   $10,$FF,$14,$02,$FF,$03,$02,$07
        .byte   $28,$C0,$0F,$3E,$02,$FF,$03,$02
        .byte   $00,$40,$02,$FF,$03,$02,$00,$42
        .byte   $02,$FF,$03,$02,$00,$43,$02,$FF
        .byte   $03
ppu_write_table_low_ptr:  .byte   $02,$00,$45,$02,$FF,$03,$02,$00
        .byte   $47,$02,$FF,$03,$02,$00,$49,$02
        .byte   $FF,$03,$02
ppu_write_table_high_ptr:  .byte   $00,$4A,$02,$FF,$03,$02,$04,$08
        .byte   $3E,$02,$FF,$03,$02,$00,$40,$02
        .byte   $FF,$03,$02
bank03_stage_select_entry:  .byte   $00,$42,$02,$FF,$03,$02,$00,$43
        .byte   $02,$FF,$03,$02,$00,$45,$02,$FF
        .byte   $03,$02,$00,$47,$02,$FF,$03,$02
        .byte   $00,$49,$02,$FF,$03,$02,$00,$4A
        .byte   $FF,$07,$02,$EB,$0A,$02,$0F,$36
        .byte   $40,$0F,$FF,$33,$FF,$10,$02,$6F
        .byte   $03,$0A,$0F,$2C,$C0,$0F,$40,$14
        .byte   $0D,$2D,$0F,$7F,$06,$02,$FF,$0B
        .byte   $0A,$0D,$2F,$08,$7F,$30,$09,$2E
        .byte   $FF,$07,$FF,$15,$02,$FF,$0E,$03
        .byte   $0F,$2A,$80,$0F,$7F,$3E,$0F,$2A
        .byte   $40,$0F,$7F,$3E,$02,$FF,$0E,$03
        .byte   $04,$0C,$3E,$04,$0C,$3E,$02,$FF
        .byte   $0E,$03,$04,$09,$3E,$04,$09,$3E
        .byte   $02,$FF,$0E,$03,$04,$06,$3E,$04
        .byte   $06,$3E,$02,$FF,$0E,$03,$04,$03
        .byte   $3E,$04,$03,$3E,$02,$FF,$0E,$03
        .byte   $04,$01,$3E,$04,$01,$3E,$FF,$09
        .byte   $02,$C8,$1C,$08,$0D,$32,$0F,$FF
        .byte   $0E,$FF,$09,$02,$FF,$04,$06,$07
        .byte   $28,$00,$0F,$52,$0D,$28,$AF,$00
        .byte   $5E,$FF,$15,$02,$FF,$04,$02,$07
        .byte   $28,$80,$0F,$2E,$02,$FF,$04,$02
        .byte   $00,$29,$02,$FF,$04,$02,$00,$26
        .byte   $02,$FF,$04,$02,$00,$3A,$FF,$02
        .byte   $02,$FF,$05,$02,$07,$28,$C0,$0F
        .byte   $39,$02,$FF,$08,$02,$02,$40,$32
        .byte   $FF,$09,$02,$FF,$04,$03,$07,$29
        .byte   $40,$0F,$3D,$07,$29,$40,$0F,$41
        .byte   $01,$02,$C6,$6D,$00,$00,$FF,$08
        .byte   $02,$FF,$02,$0A,$0F,$2C,$80,$0F
        .byte   $6E,$59,$0D,$3C,$0F,$4A,$09,$02
        .byte   $FF,$04,$00,$03,$02,$C6,$83,$FF
        .byte   $02,$0A,$0F,$2C,$80,$0F,$6E,$59
        .byte   $0D,$3C,$0F,$4A,$09,$FF,$08,$02
        .byte   $51,$05,$0E,$0F,$01,$80,$0F,$7F
        .byte   $33,$0D,$31,$FF,$7F,$3D,$0D,$28
        .byte   $0F,$FF,$04,$02,$51,$05,$04,$04
        .byte   $00,$34,$FF,$08,$02,$FF,$04,$08
        .byte   $0D,$28,$0F,$7F,$06,$02,$FF,$16
        .byte   $08,$08,$3C,$10,$FF,$08,$02,$51
        .byte   $08,$0C,$0D,$31,$FF,$7F,$38,$0D
        .byte   $28,$0F,$FF,$04,$02,$EB,$06,$06
        .byte   $0F,$2C,$80,$0F,$7F,$21,$0D,$2C
        .byte   $FF,$FF,$2D,$FF,$08,$02,$FF,$07
        .byte   $04,$05,$29,$9F,$5F,$02,$FF,$07
        .byte   $04,$04,$00,$5F,$02,$FF,$07,$04
        .byte   $04,$00,$5F,$FF,$08,$02,$FF,$07
        .byte   $0E,$0F,$2C,$C0,$0F,$7F,$28,$0D
        .byte   $2B,$8C,$7F,$5F,$0D,$32,$0F,$7F
        .byte   $10,$02,$FF,$03,$0E,$08,$FF,$4E
        .byte   $0C,$00,$FF,$5F,$08,$FF,$10,$FF
        .byte   $08,$02,$FF,$2F,$0E,$0F,$28,$80
        .byte   $0F,$87,$21,$0D,$28,$FF,$87,$2D
        .byte   $0D,$28,$0F,$88,$0C,$FF,$07,$02
        .byte   $FF,$1E,$0C,$05,$34,$FF,$2F,$05
        .byte   $28,$0F,$01,$FF,$08,$02,$FF,$0C
        .byte   $06,$1F,$32,$80,$0F,$FF,$FF,$53
        .byte   $05,$32,$74,$5F,$FF,$09,$02,$FF
        .byte   $06,$0A,$05,$32,$0F,$47,$05,$32
        .byte   $0C,$06,$FF,$11,$02,$96,$05,$08
        .byte   $05,$28,$0F,$03,$02,$28,$28,$08
        .byte   $09,$30,$AA,$03,$FF,$08,$02,$FF
        .byte   $0A,$06,$17,$3B,$00,$0F,$07,$58
        .byte   $05,$3B,$FF,$4C,$02,$FF,$0A,$06
        .byte   $00,$58,$04,$00,$4C,$FF,$11,$02
        .byte   $EB,$08,$08,$05,$2F,$0F,$03,$02
        .byte   $EB,$00,$08,$00,$03,$02,$EB,$08
        .byte   $08,$00,$03,$02,$EB,$08,$08,$00
        .byte   $03,$02,$EB,$08,$08,$00,$03,$02
        .byte   $EB,$08,$08,$00,$03,$02,$EB,$08
        .byte   $08,$00,$03,$02,$EB,$08,$08,$00
        .byte   $03,$02,$EB,$08,$08,$00,$03,$02
        .byte   $EB,$08,$08,$00,$03,$02,$EB,$08
        .byte   $08,$00,$03,$02,$EB,$08,$08,$00
        .byte   $03,$FF,$07,$02,$AB,$03,$0A,$05
        .byte   $33,$0F,$19,$05,$33,$0F,$08,$02
        .byte   $A9,$25,$08,$09,$38,$2C,$0D,$FF
        .byte   $07,$02,$FF,$17,$0A,$17,$29,$00
        .byte   $06,$02,$2D,$0D,$32,$0F,$01,$0D
        .byte   $FF,$07,$02,$BF,$04,$08,$05,$2E
        .byte   $0F,$0A,$03,$02,$C8,$15,$01,$01
        .byte   $00,$FF,$09,$02,$96,$04,$08,$05
        .byte   $32,$0F,$09,$02,$B4,$04,$08,$01
        .byte   $28,$09,$01,$01,$C8,$26,$00,$00
        .byte   $FF,$07,$02,$FF,$1B,$0A,$0D,$32
        .byte   $0F,$88,$40,$0D,$32,$0F,$98,$07
        .byte   $FF,$07,$02,$6E,$04,$0A,$0F,$2C
        .byte   $00,$0F,$A1,$24,$0D,$30,$0F,$93
        .byte   $07,$02,$28,$28,$08,$09,$37,$9C
        .byte   $02,$FF,$08,$02,$FF,$10,$0E,$0D
        .byte   $35,$0F,$63,$44,$0D,$28,$E6,$7F
        .byte   $38,$0D,$32,$0F,$2E,$03
bank03_wily_stage_entry:  .byte   $02,$FF,$0A,$0E,$04,$05,$58,$04
        .byte   $A1,$31,$04,$05,$03,$FF,$08,$02
        .byte   $FF,$03,$02,$0F,$3A,$80,$0F,$FF
        .byte   $4F,$02,$05,$1D,$02,$01,$37,$54
        .byte   $FF,$08,$02,$E1,$32,$02,$0F,$39
        .byte   $80,$0F,$8F,$3E,$FF,$00,$C8,$AF
        .byte   $C8,$EE,$C9,$37,$C9,$7E,$05,$02
        .byte   $00,$06,$C8,$07,$0C,$08,$06,$09
        .byte   $01,$18,$40,$03,$AD,$60,$6D,$6C
        .byte   $6D,$8F,$91,$92,$8F,$B1,$B4,$B6
        .byte   $B3,$09,$01,$08,$00,$07,$0A,$65
        .byte   $65,$60,$68,$68,$60,$6A,$6A,$60
        .byte   $6C,$6C,$60,$6F
bank03_portrait_setup_routine:  .byte   $60,$70,$60,$71,$71,$74,$73,$6F
        .byte   $71,$60,$71,$74,$73,$6F,$02,$71
        .byte   $17,$06,$C8,$07,$0A,$08,$06,$09
        .byte   $02,$18,$40,$76,$71,$6F,$6A,$60
        .byte   $6A,$74,$76,$98,$99,$9B,$98,$79
        .byte   $79,$60,$79,$79,$79,$60,$79,$7B
        .byte   $7B,$60,$7B,$7B,$7B,$60,$7B,$0C
        .byte   $FE,$08,$00,$7D,$7D,$60,$03,$68
        .byte   $68,$60,$6A,$6A,$60,$6C,$6C,$60
        .byte   $6F,$60,$70,$60,$65,$65,$68,$67
        .byte   $63,$65,$60,$65,$68,$67,$63,$02
        .byte   $65,$17,$06,$C8,$08,$00,$09,$03
        .byte   $6A,$6A,$6A,$6A,$A0,$08,$0E,$06
        .byte   $E6,$79,$79,$96,$79,$79,$96,$08
        .byte   $00,$06,$C8,$6D,$60,$6D,$6D,$6D
        .byte   $60,$6D,$6D,$6F,$60,$6F,$6F,$6F
        .byte   $60,$6F,$6F,$65,$65,$60,$68,$68
        .byte   $60,$6A,$6A,$60,$6C,$6C,$60,$6F
        .byte   $60,$70,$60,$71,$71,$74,$73,$6F
        .byte   $71,$60,$71,$74,$73,$6F,$02,$71
        .byte   $17,$06,$C8,$07,$0A,$08,$0C,$6C
        .byte   $6C,$6C,$6C,$02,$C0,$04,$00,$65
        .byte   $6D,$6C,$6D,$6A,$6D,$6C,$6D,$0E
        .byte   $01,$C9,$8A,$04,$00,$6C,$6C,$60
        .byte   $0E,$03,$C9,$98,$6C,$60,$6C,$60
        .byte   $6C,$6C,$6C,$6C,$6C,$6C,$60,$6C
        .byte   $6C,$6C,$6C,$6C,$80,$17,$01,$02
        .byte   $FF,$06,$0A,$0F,$39,$C0,$0F,$FF
        .byte   $40,$0D,$39,$06,$7F,$10,$02,$FF
        .byte   $01,$02,$00,$48,$FF,$00,$C9,$D3
        .byte   $CA,$4B,$CA,$C7,$CB,$2B,$0A,$FD
        .byte   $05,$02,$00,$07,$0C,$18,$C0,$04
        .byte   $08,$04,$08,$09,$01,$08,$1D,$06
        .byte   $C8,$02,$B1,$6F,$6D,$02,$AF,$6D
        .byte   $6C,$02,$AD,$6C,$6A,$8C,$8A,$88
        .byte   $8A,$6A,$60,$71,$6F,$02,$01,$D1
        .byte   $08,$01,$06,$F5,$01,$F1,$0E,$01
        .byte   $C9,$DE,$04,$08,$06,$C8,$08,$1D
        .byte   $02,$AA,$6A,$71,$8F,$8D,$8C,$8D
        .byte   $AA,$02,$8C,$6D,$6C,$60,$02,$AA
        .byte   $12,$08,$CA,$35,$02,$AA,$6A,$71
        .byte   $94,$92,$91,$8F,$B1,$02,$92,$74
        .byte   $72,$60,$02,$B1,$0E,$01,$CA,$07
        .byte   $09,$02,$02,$AA,$6C,$6D,$8C,$8A
        .byte   $88,$85,$AA,$02,$8C,$6D,$6C,$60
        .byte   $02,$AA,$16,$C9,$DC,$17,$06,$C8
        .byte   $07,$0C,$08,$06,$09,$02,$04,$00
        .byte   $04,$00,$04,$00,$96,$99,$98,$99
        .byte   $0E,$05,$CA,$57,$76,$79,$78,$76
        .byte   $7E,$79,$76,$03,$68,$63,$03,$78
        .byte   $03,$6A,$65,$61,$6D,$6C,$68,$0F
        .byte   $01,$CA,$55,$04,$00,$0C,$00,$09
        .byte   $01,$9E,$9B,$9D,$9E,$03,$89,$85
        .byte   $87,$89,$0C,$FE,$AA,$02,$8C,$6D
        .byte   $6C,$60,$02,$AA,$12,$08,$CA,$AD
        .byte   $0C,$00,$86,$83,$85,$86,$8C,$89
        .byte   $8A,$8C,$B1,$0C,$FE,$02,$92,$74
        .byte   $72,$60,$02,$B1,$0E,$01,$CA,$78
        .byte   $0C,$00,$09,$02,$8A,$86,$88,$8A
        .byte   $8C,$88,$8A,$8C,$0C,$FE,$AA,$02
        .byte   $8C,$6D,$6C,$60,$02,$AA,$16,$CA
        .byte   $53,$17,$06,$C8,$08,$00,$09,$03
        .byte   $04,$00,$04,$00,$04,$00,$6A,$60
        .byte   $6A,$6A,$0E,$07,$CA,$D1,$04,$00
        .byte   $66,$60,$66,$66,$0E,$06,$CA,$DB
        .byte   $68,$60,$68,$68,$0F,$01,$CA,$CF
        .byte   $04,$00,$63,$60,$63,$63,$63,$60
        .byte   $63,$63,$65,$60,$65,$65,$65,$60
        .byte   $65,$65,$04,$00,$6A,$60,$6A,$6A
        .byte   $0E,$03,$CA,$FF,$0F,$02,$CA,$ED
        .byte   $66,$60,$66,$66,$66,$60,$66,$66
        .byte   $68,$60,$68,$68,$68,$60,$68,$68
        .byte   $04,$00,$6A,$60,$6A,$6A,$0E,$03
        .byte   $CB,$1D,$16,$CA,$CD,$17,$04,$00
        .byte   $04,$00,$08,$0C,$07,$08,$06,$C8
        .byte   $65,$6C,$08,$03,$06,$F0,$8D,$08
        .byte   $0C,$06,$C8,$6A,$6C,$08,$03,$06
        .byte   $F0,$8D,$0E,$0F,$CB,$2D,$04,$00
        .byte   $08,$0C,$6D,$6D,$6D,$6D,$68,$6D
        .byte   $6D,$6D,$0E,$0E,$CB,$4B,$65,$6C
        .byte   $68,$65,$6C,$68,$65,$6C,$16,$CB
        .byte   $2B,$17,$00,$CB,$70,$CB,$F6,$CC
        .byte   $7B,$CC,$CB,$0A,$03,$05,$02,$00
        .byte   $06,$D2,$07,$0A,$08,$16,$09,$01
        .byte   $18,$40,$83,$86,$89,$8C,$89,$86
        .byte   $8F,$92,$95,$98,$95,$92,$9B,$9E
        .byte   $03,$89,$8C,$89,$86,$8F,$92,$95
        .byte   $98,$95,$92,$04,$08,$06,$78,$07
        .byte   $0A,$6F,$70,$0E,$0B,$CB,$98,$08
        .byte   $06,$06,$96,$07,$0C,$09,$02,$03
        .byte   $96,$76,$79,$96,$03,$88,$86,$85
        .byte   $84,$83,$81,$83,$81,$03,$96,$96
        .byte   $76,$79,$96,$9C,$9B,$99,$9B,$99
        .byte   $96,$02,$B6,$03,$8A,$6A,$68,$8A
        .byte   $8D,$8C,$88,$88,$86,$85,$83,$85
        .byte   $81,$03,$96,$9C,$9B,$99,$98,$94
        .byte   $96,$99,$9D,$08,$22,$01,$03,$AA
        .byte   $07,$0A,$AA,$07,$08,$AA,$07,$06
        .byte   $AA,$07,$04,$AA,$07,$02,$01,$AA
        .byte   $17,$06,$D2,$07,$0A,$08,$16,$09
        .byte   $01,$0C,$01,$18,$40,$83,$86,$89
        .byte   $8C,$89,$86,$8F,$92,$95,$98,$95
        .byte   $92,$9B,$9E,$03,$89,$8C,$89,$86
        .byte   $8F,$92,$95,$98,$95,$92,$09,$02
        .byte   $08,$01,$0C,$00,$07,$08,$06,$FA
        .byte   $02,$03,$B6,$02,$B0,$02,$B1,$02
        .byte   $A9,$08,$06,$06,$96,$04,$00,$07
        .byte   $0C,$8A,$6D,$6F,$90,$0E,$02,$CC
        .byte   $32,$8F,$8D,$8A,$04,$00,$8A,$6D
        .byte   $6C,$8A,$0E,$02,$CC,$41,$8F,$8D
        .byte   $8C,$04,$00,$8A,$6D,$6F,$90,$0E
        .byte   $03,$CC,$4E,$0C,$02,$96,$9C,$9B
        .byte   $99,$98,$94,$96,$99,$9D,$08,$22
        .byte   $09,$01,$01,$03,$AA,$07,$0A,$AA
        .byte   $07,$08,$AA,$07,$06,$AA,$07,$04
        .byte   $AA,$07,$02,$01,$AA,$17,$06,$C8
        .byte   $08,$00,$09,$03,$04,$00,$63,$60
        .byte   $63,$63,$63,$60,$0E,$07,$CC,$81
        .byte   $09,$02,$08,$01,$06,$FA,$02,$B6
        .byte   $02,$B0,$02,$B1,$02,$A9,$09,$03
        .byte   $06,$C8,$08,$00,$8A,$8A,$8A,$8A
        .byte   $8A,$8A,$86,$86,$86,$86,$86,$86
        .byte   $83,$83,$83,$88,$88,$88,$04,$00
        .byte   $8A,$0E,$0B,$CC,$B3,$86,$86,$86
        .byte   $86,$86,$86,$8A,$90,$8F,$8D,$8C
        .byte   $88,$8A,$8D,$91,$96,$17,$06,$FF
        .byte   $07,$0C,$08,$09,$02,$CF,$02,$CC
        .byte   $02,$C8,$02,$C5,$E0,$C0,$04,$00
        .byte   $08,$0C,$06,$C8,$07,$0A,$6C,$60
        .byte   $6C,$6C,$6C,$60,$0E,$06,$CC,$DB
        .byte   $6C,$6C,$6C,$6C,$6C,$6C,$04,$00
        .byte   $6C,$60,$6C,$6C,$6C,$60,$0E,$05
        .byte   $CC,$F3,$6C,$6C,$6C,$6C,$6C,$6C
        .byte   $08,$09,$06,$FF,$07,$0C,$01,$A8
        .byte   $07,$0A,$A8,$07,$08,$A8,$07,$06
        .byte   $A8,$07,$04,$A8,$07,$02,$01,$A8
        .byte   $17,$00,$CD,$27,$CD,$5C,$CD,$90
        .byte   $CD,$BE,$05,$02,$00,$06,$C8,$07
        .byte   $0C,$08,$00,$09,$01,$18,$40,$7B
        .byte   $7F,$03,$6A,$6E,$60,$6C,$60,$6A
        .byte   $6C,$60,$6A,$60,$67,$02,$8A,$66
        .byte   $6A,$6D,$71,$60,$6F,$60,$6D,$6F
        .byte   $60,$6D,$60,$6C,$6D,$6C,$68,$70
        .byte   $70,$60,$72,$72,$60,$F4,$17,$06
        .byte   $C8,$07,$0C,$08,$00,$18,$40,$09
        .byte   $01,$73,$76,$7A,$7D,$60,$7B,$60
        .byte   $7A,$7B,$60,$7A,$60,$76,$02,$9A
        .byte   $76,$79,$7D,$03,$68,$60,$66,$60
        .byte   $65,$66,$60,$65,$60,$63,$65,$63
        .byte   $03,$78,$03,$6D,$6D,$60,$6F,$6F
        .byte   $60,$F1,$17,$06,$C8,$08,$00,$09
        .byte   $02,$7B,$7B,$7B,$7B,$60,$7B,$60
        .byte   $7B,$7B,$60,$7B,$60,$7B,$02,$9B
        .byte   $79,$79,$79,$79,$60,$79,$60,$79
        .byte   $03,$68,$60,$68,$60,$68,$68,$68
        .byte   $68,$69,$69,$60,$6B,$6B,$60,$ED
        .byte   $17,$17,$00,$CD,$C8,$CD,$E9,$CE
        .byte   $05,$CE,$3B,$0A,$06,$05,$02,$2E
        .byte   $06,$7D,$07,$0C,$08,$00,$18,$C0
        .byte   $C0,$03,$70,$70,$6F,$70,$C0,$06
        .byte   $96,$70,$72,$60,$01,$74,$D4,$08
        .byte   $01,$01,$D4,$17,$06,$7D,$07,$0C
        .byte   $08,$00,$18,$40,$C0,$03,$6D,$6D
        .byte   $6C,$6D,$C0,$6D,$6F,$60,$06,$96
        .byte   $01,$71,$D1,$08,$01,$01,$D1,$17
        .byte   $06,$C8,$08,$0E,$09,$02,$00,$78
        .byte   $76,$74,$00,$03,$88,$00,$03,$78
        .byte   $76,$74,$00,$03,$88,$08,$00,$69
        .byte   $69,$68,$69,$08,$0E,$06,$E6,$09
        .byte   $02,$68,$68,$82,$68,$68,$81,$08
        .byte   $00,$69,$6B,$60,$06,$96,$01,$6D
        .byte   $CD,$08,$01,$01,$CD,$17,$17,$00
        .byte   $00,$00,$00,$20,$00,$24,$40,$11
        .byte   $00,$30,$00,$00,$00,$00,$00,$01
        .byte   $00,$84,$00,$00,$40,$00,$00,$80
        .byte   $80,$20,$00,$12,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$04,$00,$22
        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $01,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$80
        .byte   $00,$00,$00,$48,$01,$00,$00,$10
        .byte   $00,$01,$00,$20,$40,$00,$00,$10
        .byte   $00,$02,$00,$00,$00,$00,$04,$00
        .byte   $00,$04,$40,$82,$00,$00,$00,$1A
        .byte   $04,$40,$00,$C2,$01,$08,$00,$00
        .byte   $00,$00,$10,$00,$00,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$00,$00,$14
        .byte   $00,$00,$00,$40,$04,$00,$40,$10
        .byte   $00,$80,$01,$20,$00,$00,$00,$44
        .byte   $00,$00,$00,$00,$00,$40,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$44,$40,$00
        .byte   $00,$08,$10,$50,$00,$02,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $04,$21,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$40,$00,$00,$08
        .byte   $00,$20,$00,$00,$00,$80,$00,$04
        .byte   $44,$50,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$80,$00,$00,$04,$00
        .byte   $00,$34,$00,$01,$10,$21,$00,$80
        .byte   $00,$00,$00,$00,$00,$00,$00,$80
        .byte   $00,$02,$00,$00,$00,$00,$00,$09
        .byte   $00,$80,$00,$12,$00,$08,$00,$40
        .byte   $10,$01,$01,$02,$00,$00,$00,$40
        .byte   $00,$00,$00,$00,$00,$10,$00,$00
        .byte   $00,$00,$00,$20,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$01,$14,$00
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$40,$00
        .byte   $00,$01,$00,$00,$00,$44,$00,$00
        .byte   $00,$02,$00,$50,$00,$00,$00,$00
        .byte   $00,$02,$00,$80,$00,$00,$00,$04
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $04,$00,$00,$00,$40,$01,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$02,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$40,$80
        .byte   $40,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$10

; =============================================================================
; ENTRY JUMP TABLE ($9000-$9008)
; =============================================================================
; Three entry points called from the fixed bank dispatcher:
;   $9000: Robot Master stage intro (after boss selected)
;   $9003: Password screen entry
;   $9006: Wily fortress gate entrance
; =============================================================================
        jmp     robot_master_intro      ; → robot_master_intro

        jmp     password_screen_entry   ; → password_screen_entry

        jmp     wily_gate_setup         ; → wily_gate_entry

; ===========================================================================
; Title screen → stage select transition ($9009)
; ===========================================================================
; Loads CHR banks for the title screen, writes nametable data for
; title/credits screens, waits for palette fade, then loads bank $13
; metatile data to render the stage select background. After all 64
; metatile columns are rendered, enables PPU and waits for Start press.
; ===========================================================================
        jsr     fade_palette_in         ; fade palette to black
        jsr     task_yield              ; wait for fade to complete
        jsr     rendering_off           ; rendering_off
        ldy     #$05                    ; index for 6 CHR bank slots
; --- Load CHR banks from $9BF7 table ---
chr_bank_load_loop:  lda     $9BF7,y    ; 6 CHR bank IDs
        sta     $E8,y                   ; store to CHR bank shadow
        dey                             ; next bank slot
        bpl     chr_bank_load_loop      ; loop for all 6 banks
        jsr     select_CHR_banks        ; apply CHR bank selection
        lda     #$20                    ; nametable $2000
        ldx     #$24                    ; fill tile $24 (blank)
        ldy     #$00                    ; nametable offset 0
        jsr     fill_nametable          ; fill_nametable
        jsr     rendering_on            ; rendering_on
; --- Copy palette working copy from $9C03 ---
        ldy     #$1F                    ; 32 palette bytes
palette_copy_loop:  lda     $9C03,y
        sta     $0620,y                 ; palette working copy
        dey                             ; next byte
        bpl     palette_copy_loop       ; loop all 32 bytes
; --- Write title screen nametable data (PPU tables $10/$11 from bank03) ---
        lda     #$00                    ; clear temp
        sta     $10                     ; set PPU write offset
        ldx     #$10                    ; table index $10
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        jsr     task_yield              ; wait for PPU write
        ldx     #$11                    ; table index $11
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        jsr     task_yield              ; wait for PPU write
        jsr     fade_palette_out        ; show title screen
        ldx     #$B4                    ; wait 180 frames (3 seconds)
        jsr     task_yield_x            ; delay before stage select
; --- Second phase: load stage select background via metatiles ---
        jsr     fade_palette_in         ; fade out
        jsr     task_yield              ; wait for fade to complete
        jsr     rendering_off           ; rendering_off
        ldy     #$05                    ; index for 6 CHR bank slots
chr_bank_reload_loop:  lda     $9BF7,y  ; reload CHR banks
        sta     $E8,y                   ; store to CHR bank shadow
        dey                             ; next bank slot
        bpl     chr_bank_reload_loop    ; loop for all 6 banks
        jsr     select_CHR_banks        ; apply CHR bank selection
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank                ; set PRG bank to $13
        jsr     select_PRG_banks        ; apply PRG bank selection
        lda     #$00                    ; start at column 0
        sta     $28                     ; metatile column counter
; --- Render 64 metatile columns (stage select background) ---
metatile_column_loop:  ldy     #$00     ; clear sub-index
        sty     $10                     ; clear PPU write offset
        jsr     queue_metatile_update   ; queue_metatile_update
        jsr     drain_ppu_buffer        ; drain_ppu_buffer
        jsr     task_yield              ; wait for NMI
        inc     $28                     ; next column
        lda     $28                     ; read column counter
        and     #$3F                    ; 64 columns
        bne     metatile_column_loop    ; loop until 64 columns done
; --- Reload palettes and enable rendering ---
        ldy     #$1F                    ; 32 palette bytes
palette_reload_loop:  lda     $9C03,y   ; load palette byte
        sta     $0620,y                 ; store to working copy
        dey                             ; next palette byte
        bpl     palette_reload_loop     ; loop all 32 bytes
        jsr     task_yield              ; wait for NMI
        jsr     rendering_on            ; rendering_on
; --- Set up initial OAM sprite (password cursor sentinel) ---
        ldx     #$03                    ; 4 OAM bytes (Y/tile/attr/X)
oam_sentinel_load_loop:  lda     $9C69,x ; initial OAM data (4 bytes)
        sta     $0200,x                 ; sprite 0: Y=$97, tile=$ED, attr=$01, X=$28
        dex                             ; next OAM byte
        bpl     oam_sentinel_load_loop  ; loop all 4 bytes
        lda     #$00                    ; clear NMI skip
        sta     nmi_skip                ; allow NMI processing
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank                ; set PRG bank
        jsr     select_PRG_banks        ; apply PRG bank selection
        jsr     fade_palette_out        ; show stage select background

; ===========================================================================
; Title screen Start button wait loop ($90B4)
; ===========================================================================
; Waits for Start button press. D-pad Up/Down moves password cursor
; (OAM sprite 0 Y position: $B7 = top, $C7 = bottom).
; Start press → jump to stage_select_init.
; ===========================================================================
title_wait_start_loop:
        lda     joy1_press              ; read new button presses
        and     #BTN_START              ; check Start button
        bne     stage_select_init       ; Start → enter stage select
        lda     joy1_press              ; re-read button presses
        and     #$0C                    ; Up ($08) / Down ($04)
        beq     title_wait_yield        ; no d-pad press → skip
        lsr     a                       ; shift right 3x: $08→$01
        lsr     a                       ; continue shifting
        lsr     a                       ; $08→$01, $04→$00
        tay                             ; use as table index
        lda     $9BFF,y                 ; Y position lookup: [$00]=$30, [$01]=$40
        sta     $0200                   ; update sprite 0 Y position
title_wait_yield:  jsr     task_yield
        jmp     title_screen_wait_start ; loop back to wait

; --- stage_select_init ($1890D0) ---
; Entry point for the stage select screen. Sets up CHR banks, loads palettes,
; and determines if this is a fresh start or return from a stage.
;
; Palette system:
;   $0600-$060F = BG palette buffer (4 sub-palettes × 4 colors)
;   $0610-$061F = Sprite palette buffer (4 sub-palettes × 4 colors)
;   $0620-$062F = BG palette working copy
;   palette_dirty = update flag (nonzero → NMI uploads $0600-$061F to PPU $3F00-$3F1F)
;
; Fresh start ($0F=0): BG palettes from $9C33, sprite palettes from $9C23
;   BG 0: $0F $20 $21 $11 — frame borders: black, white, lt blue, dk blue
;   BG 1: $0F $20 $37 $29 — portrait face BG: black, white, tan, green
;   BG 2: $0F $20 $26 $16 — portrait face detail: black, white, salmon, dk red
;   BG 3: $0F $20 $10 $21 — text/misc: black, white, grey, lt blue
;   SP 0: $0F $30 $15 $11 — cursor bolts + center portrait: transparent, white, dk magenta, dk blue
;   SP 1: $0F $37 $21 $10 — face sprites 1: black, tan, lt blue, grey
;   SP 2: $0F $37 $26 $15 — face sprites 2: black, tan, salmon, dk magenta
;   SP 3: $0F $37 $26 $0F — face sprites 3: black, tan, salmon, black
;
; Return from stage ($0F=1): BG palettes from $9C43

stage_select_init:  lda     #$36        ; CHR bank $36 for sprite tiles
        sta     $EC                     ; set 4KB sprite CHR bank
        lda     #$34                    ; CHR bank $34 for BG tiles
        sta     $ED                     ; set 4KB BG CHR bank
        jsr     select_CHR_banks        ; apply CHR bank selection

; Load sprite palettes from $9C23 ($9C23)
        ldy     #$0F                    ; counter = 16 bytes
sprite_palette_load_loop:  lda     $9C23,y ; load sprite palette color
        sta     $0610,y                 ; store to sprite palette buffer
        dey                             ; next palette byte
        bpl     sprite_palette_load_loop ; loop Y=$0F down to $00
        sty     palette_dirty           ; Y=$FF, trigger palette update

; Determine fresh start vs return: check if OAM is initialized
        ldy     #$00                    ; assume fresh start (Y=0)
        lda     $0200                   ; check OAM sprite 0 Y position
        cmp     #$97                    ; $97 = sentinel for uninitialized
        beq     stage_select_init_fresh_vs_return ; fresh start → Y stays 0
        iny                             ; returning from stage → Y=1
stage_select_init_fresh_vs_return:  sty     $0F ; $0F = 0 (fresh) or 1 (return)
        lda     $9BFD,y                 ; select music track
        jsr     submit_sound_ID_D9      ; play stage select music
        lda     #$00                    ; clear counters
        sta     $70                     ; clear NMI sync flag
        lda     $9C63,y                 ; load screen scroll/setup param
        jsr     metatile_column_ptr_by_id ; init metatile column pointer
        lda     #$04                    ; OAM offset = 4 (skip sprite 0)
        sta     oam_ptr                 ; set OAM write pointer
        jsr     prepare_oam_buffer      ; clear unused OAM sprites
stage_select_nametable_fill_wait_loop:  lda     #$04 ; 4 rows per fill call
        sta     $10                     ; set fill row count
        jsr     fill_nametable_progressive ; fill nametable progressively
        jsr     task_yield              ; wait for NMI
        lda     $70                     ; check NMI sync flag
        bne     stage_select_nametable_fill_wait_loop ; loop until NMI complete

; Load BG palettes — offset depends on fresh start vs return
        ldy     $0F                     ; Y = 0 (fresh) or 1 (return)
        beq     stage_select_defeated_portrait_restore ; skip portrait restore if fresh
        lda     #$04                    ; PPU write table index 4
        sta     $10                     ; set table offset
        ldx     #$00                    ; first PPU write entry
        jsr     write_ppu_data_from_table ; restore defeated boss portraits
stage_select_defeated_portrait_restore:  lda     $9C65,y ; load scroll params
        sta     $10                     ; store scroll speed low
        lda     $9C67,y                 ; load scroll param high
        sta     $11                     ; store scroll speed high
        jsr     rendering_off           ; disable PPU rendering
        ldx     $9C01,y                 ; X = offset into $9C03
        ldy     #$00                    ; palette dest index = 0
bg_palette_load_loop:  lda     $9C03,x  ; load BG palette color
        sta     $0600,y                 ; store to BG palette buffer
        sta     $0620,y                 ; store to working copy
        inx                             ; next source byte
        iny                             ; next dest byte
        cpy     #$10                    ; 16 bytes = 4 palettes × 4 colors
        bne     bg_palette_load_loop    ; loop until all 16 copied
        sty     palette_dirty           ; Y=$10, trigger palette update
        lda     #$20                    ; nametable $2000
        ldx     #$24                    ; fill tile $24 (blank)
        ldy     #$00                    ; nametable offset 0
        jsr     fill_nametable          ; fill_nametable
        jsr     rendering_on            ; rendering_on

; ===========================================================================
; Horizontal scroll transition ($9155)
; ===========================================================================
; Smoothly scrolls the screen horizontally to reveal the stage select grid.
; $10/$11 = scroll speed (low/high). Scroll increments $FC/$FD until
; $FC wraps to 0, then fills the offscreen nametable and swaps.
; ===========================================================================
scroll_transition_start:
        lda     #$F8                    ; hide sprite 0 (offscreen Y)
        sta     $0200                   ; sprite 0 Y = offscreen
        inc     nmi_skip                ; skip NMI processing
        lda     #$58                    ; scroll split Y position
        sta     $5E                     ; set IRQ split scanline
        lda     #$07                    ; game mode $07 = stage select
        sta     game_mode               ; set stage select mode
; --- Scroll loop: increment camera X until low byte wraps ---
scroll_increment_loop:  lda     camera_x_lo ; load scroll X low byte
        clc                             ; add scroll speed (low)
        adc     $10                     ; add scroll speed low
        sta     camera_x_lo             ; update scroll X low
        lda     camera_x_hi             ; load scroll X high byte
        adc     $11                     ; add scroll speed high
        and     #$01                    ; keep nametable bit
        sta     camera_x_hi             ; update scroll X high
        lda     #$00                    ; allow NMI
        sta     nmi_skip                ; allow NMI
        jsr     task_yield              ; wait for next frame
        inc     nmi_skip                ; block NMI again
        lda     camera_x_lo             ; check if scroll aligned
        bne     scroll_increment_loop   ; loop until aligned
; --- Check if we need to fill the offscreen nametable ---
        ldy     #$10                    ; palette offset for $9C33
        lda     $11                     ; check scroll direction
        bmi     palette_buffer_load     ; negative speed → skip fill
; --- Fill offscreen nametable ---
        lda     camera_x_hi             ; get current nametable
        eor     #$01                    ; opposite nametable
        asl     a                       ; shift to bit 2
        asl     a                       ; 0 or 4
        sta     $10                     ; store NT select (0 or 4)
        lda     #$01                    ; metatile column 1
        jsr     metatile_column_ptr_by_id ; metatile_column_ptr_by_id
        lda     #$00                    ; init fill counter
        sta     $70                     ; reset fill progress
        sta     nmi_skip                ; allow NMI
scroll_fill_offscreen_loop:  lda     $10 ; save NT select on stack
        pha                             ; preserve across JSR
        jsr     fill_nametable_progressive ; fill_nametable_progressive
        pla                             ; restore NT select
        sta     $10                     ; write back to $10
        jsr     task_yield              ; wait for NMI upload
        lda     $70                     ; check fill progress
        bne     scroll_fill_offscreen_loop ; loop until complete
; --- Write defeated portraits and PPU data ---
        jsr     draw_defeated_portraits ; blank faces for beaten bosses
        ldx     #$03                    ; PPU data table $03
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        jsr     task_yield              ; wait for NMI upload
        ldx     #$04                    ; PPU data table $04
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
; --- Swap displayed nametable ---
        lda     camera_x_hi             ; get current nametable
        eor     #$01                    ; toggle nametable bit
        sta     camera_x_hi             ; swap displayed nametable
        ldy     #$00                    ; Y=0 for fresh palettes
        lda     #$7E                    ; CHR bank $7E
        sta     $E9                     ; set BG CHR bank
        jsr     update_CHR_banks        ; apply CHR bank change
; --- Load BG palette from $9C33 table ---
; Y=0 for fresh start palettes, Y=$10 for return palettes.
palette_buffer_load:  ldx     #$00      ; palette buffer index
palette_buffer_copy_loop:  lda     $9C33,y ; BG palette color
        sta     $0600,x                 ; store to palette buffer
        iny                             ; next source byte
        inx                             ; next dest byte
        cpx     #$10                    ; 16 bytes
        bne     palette_buffer_copy_loop ; loop until all 16 copied
        lda     #$FF                    ; set dirty flag
        sta     palette_dirty           ; flag palette upload
; --- Initialize cursor at center position ---
        lda     #$01                    ; cursor column = 1 (center)
        sta     $12                     ; store cursor column
        lda     #$03                    ; cursor row offset = 3 (middle)
        sta     $13                     ; store cursor row offset
; --- Branch based on scroll direction ---
        lda     $11                     ; check scroll direction
        bmi     password_menu_setup     ; negative → password screen mode
        jsr     load_stage_select_oam   ; load_stage_select_oam
        jmp     stage_select_input_loop ; → stage select main loop

; --- Password screen mode (negative scroll speed) ---
; Shows "Game Start / Stage Select" cursor, waits for Start.
password_menu_setup:  jsr     password_cursor_oam_setup
password_menu_loop:  jsr     proto_man_sprite_control
        lda     #$00                    ; allow NMI
        sta     nmi_skip                ; clear NMI skip flag
        jsr     task_yield              ; wait for next frame
        inc     nmi_skip                ; block NMI again
        lda     joy1_press              ; check new button presses
        and     #BTN_START              ; check Start button
        beq     password_menu_loop      ; loop until Start pressed
; --- Process password cursor selection ---
        lda     $0200                   ; cursor Y position
        cmp     #$B7                    ; $B7 = "Game Start" row
        beq     game_start_clear_oam    ; → Game Start (clear OAM, re-enter)
; --- Stage Select chosen: jump to bank03 $A593 ---
        lda     #$03                    ; switch to bank $03
        sta     prg_bank                ; set PRG bank register
        jsr     select_PRG_banks        ; apply bank switch
        jmp     bank03_stage_select_entry ; → bank03 stage select entry

; --- Game Start: clear all OAM sprites except sprite 0 ---
game_start_clear_oam:  ldy     #$04     ; start at sprite 1
        lda     #$F8                    ; Y=$F8 = offscreen
oam_clear_loop:  sta     $0200,y        ; clear OAM Y to offscreen
        iny                             ; skip tile byte
        iny                             ; skip attr byte
        iny                             ; skip X byte
        iny                             ; next sprite (4 bytes)
        bne     oam_clear_loop          ; loop all 63 remaining sprites
; --- Play stage select music and rebuild nametable ---
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank                ; set PRG bank register
        jsr     select_PRG_banks        ; apply bank switch
        lda     #MUSIC_STAGE_SELECT     ; music $10 = stage select theme
        jsr     submit_sound_ID_D9      ; play stage select music
        lda     #$00                    ; clear counters
        sta     $70                     ; nametable fill counter
        sta     $28                     ; clear scroll sub-state
        lda     #$04                    ; metatile column 4
        jsr     metatile_column_ptr_by_id ; metatile_column_ptr_by_id
nametable_fill_sync_loop:  lda     #$00 ; clear temp variables
        sta     nmi_skip                ; allow NMI
        sta     $10                     ; NT select = 0
        jsr     fill_nametable_progressive ; fill_nametable_progressive
        jsr     task_yield              ; wait for NMI upload
        lda     $70                     ; check fill progress
        bne     nametable_fill_sync_loop ; loop until complete
; --- Set scroll speed and CHR, then scroll into stage select ---
        lda     #$04                    ; scroll speed low = 4
        sta     $10                     ; store scroll speed low
        lda     #$00                    ; scroll speed high = 0
        sta     $11                     ; store scroll speed high
        lda     #$7C                    ; update CHR bank
        sta     $E8                     ; set BG CHR slot 0
        jsr     update_CHR_banks        ; apply CHR bank change
        jmp     stage_select_cursor_init ; → horizontal scroll transition
stage_select_input_entry:

        lda     joy1_press              ; check new button presses
        and     #$90                    ; A ($80) or Start ($10)
        beq     horizontal_input_check  ; no confirm → d-pad check
        jmp     stage_select_confirm    ; confirm stage selection

; ==========================================================================
; STAGE SELECT SCREEN
; ==========================================================================
; The stage select screen displays a 3×3 grid of boss portraits with
; Mega Man's face in the center. The player moves a cursor between the
; 9 grid positions using the D-pad.
;
; Grid layout:
;   Spark Man ($06) | Snake Man ($05) | Needle Man ($00)
;   Hard Man ($03)  | Mega Man (ctr)  | Top Man ($04)
;   Gemini Man ($02)| Magnet Man ($01)| Shadow Man ($07)
;
; Visual composition:
;   Background tiles: Blue "MEGA MAN" repeating pattern, portrait frame
;     borders (8×6 tiles per frame), boss name text labels, "PUSH START"
;   Nametable writes: Portrait face tiles (4×4 per boss, CHR bank selects face),
;     center Mega Man face tiles ($CC-$FF)
;   OAM sprites: Cursor bolt connectors (4 per selected frame, tiles $E4/$E5,
;     sprite palette 0: $0F/$30/$15/$11 — attribute byte never written, defaults
;     to $00 from OAM clear. Tile $E4 = magenta fill ($15), tile $E5 = blue fill
;     ($11), both with white ($30) upper-left highlight. Alternates every 8 frames.
;     Mega Man eye sprites (6 per cursor position, attr $03 = sprite palette 3,
;     eyes track toward selected boss), boss portrait detail sprites
;
; Key data tables in this bank:
;   $9C75: megaman_eye_sprites — 9 × 12 bytes (6 sprites × tile+attr)
;   $9CE1: stage_select_lookup — grid index → stage $22 value
;   $9CF3: cursor_pos_y — bolt sprite Y base per grid position
;   $9CFC: cursor_pos_x — bolt sprite X base per grid position
;   $9D05: cursor_bolt_offset_y — Y offsets for 4 bolt corners
;   $9D09: cursor_bolt_offset_x — X offsets for 4 bolt corners
;   $9D0D: cursor_direction_offsets — D-pad bit → position delta
;   $9D4C: portrait_addr_lo — PPU nametable low bytes for frame positions
;   $9D55: portrait_addr_hi — PPU nametable high bytes for frame positions
;   $9D5E: portrait_frame_tiles — frame border tile layout (groups)
;   $9D99: face_tiles — 4×4 boss face tile IDs ($C8-$FB)
;   $9DA9: face_attr_offsets — attribute table offsets per portrait
;   $9DB2: center_portrait_data — Mega Man center face tiles ($CC-$FF)
;   $9DC9: face_addr_hi — PPU high bytes for face tile writes
;   $9DD2: face_addr_lo — PPU low bytes for face tile writes
;
; Portrait pixel positions (frame top-left corners):
;   Col 0: x=16   Col 1: x=96   Col 2: x=176
;   Row 0: y=24   Row 1: y=88   Row 2: y=152
;
; Cursor bolt pixel positions (base + offsets):
;   Base Y: row 0=$18(24), row 1=$58(88), row 2=$98(152)
;   Base X: col 0=$17(23), col 1=$67(103), col 2=$B7(183)
;   Offsets: TL(0,0) TR(42,0) BL(0,38) BR(42,38)
;   Bolt tile $E4 = magenta ($15) fill, $E5 = blue ($11) fill, alternates every 8 frames
; ==========================================================================

; --- Stage select d-pad handling ---
; $12 = cursor column (0-2), $13 = cursor row offset (0/3/6)
; Combined ($12+$13) = grid index 0-8 into stage lookup table at $9CE1

horizontal_input_check:  lda     joy1_press ; new button presses
        and     #$03                    ; $02=Left, $01=Right
        beq     vertical_input_check    ; no horizontal input → skip
        tay                             ; Y = d-pad bits as index
        lda     $12                     ; current column
        clc                             ; prepare for add
        adc     $9D0D,y                 ; add direction offset from table
        cmp     #$03                    ; clamp to 0-2
        bcs     vertical_input_check    ; out of range → ignore
        sta     $12                     ; update column
        lda     #SFX_CURSOR             ; SFX $1B = cursor move
        jsr     submit_sound_ID         ; play cursor move sound
vertical_input_check:  lda     joy1_press ; new button presses
        and     #$0C                    ; $08=Up, $04=Down
        beq     cursor_sprite_update    ; no vertical input → skip
        tay                             ; Y = d-pad bits as index
        lda     $13                     ; current row offset
        clc                             ; prepare for add
        adc     $9D0D,y                 ; add direction offset
        cmp     #$07                    ; clamp to 0-6 (rows 0/3/6)
        bcs     cursor_sprite_update    ; out of range → ignore
        sta     $13                     ; update row
        lda     #SFX_CURSOR             ; SFX $1B = cursor move
        jsr     submit_sound_ID         ; play cursor move sound
cursor_sprite_update:  lda     $12      ; combine column + row
        clc                             ; add row offset
        adc     $13                     ; = grid index (0-8)
        sta     temp_00                 ; save grid index to $00
        asl     a                       ; ×2
        asl     a                       ; ×4
        sta     $01                     ; save ×4 to $01
        asl     a                       ; ×8

; --- Cursor sprite rendering ---
; Writes 6 cursor selector sprites + 4 cursor bolt sprites to OAM.
; Grid index = $12 + $13 = $00 (0-8).
        adc     $01                     ; ×12 = OAM data offset for grid pos
        tax                             ; X = OAM data offset
        ldy     #$00                    ; Y = OAM dest offset
cursor_selector_sprite_load_loop:  lda     $9C75,x ; cursor selector OAM data (12 bytes/pos)
        sta     $0299,y                 ; → OAM sprites at $0298+
        lda     $9C76,x                 ; tile + attr byte
        sta     $029A,y                 ; store tile + attr
        inx                             ; advance source ×2
        inx                             ; source uses 2 bytes per sprite
        iny                             ; advance dest ×4
        iny                             ; skip attr byte (unused)
        iny                             ; skip X byte (unused)
        iny                             ; next OAM entry (4 bytes each)
        cpy     #$18                    ; 6 sprites × 4 bytes = $18
        bne     cursor_selector_sprite_load_loop ; loop all 6 sprites

; --- Cursor bolt sprites (4 corner bolts) ---
; Position from $9CF3/$9CFC tables, offsets from $9D05/$9D09.
; Bolt tile alternates $E4/$E5 every 8 frames for flash effect.
; NOTE: OAM attribute byte is never written → defaults to $00 = sprite palette 0.
; SP 0: $0F(transparent), $30(white), $15(dk_magenta), $11(dk_blue).
; Tile $E4 uses color 2 ($15 magenta fill), tile $E5 uses color 3 ($11 blue fill).
; Both tiles have color 1 ($30 white) as upper-left highlight.
; Solid filled shape (not outline) — same pattern as center portrait bolt sprites.
        ldy     temp_00                 ; grid index
        lda     $9CF3,y                 ; cursor Y base (per grid position)
        sta     temp_00                 ; save Y base for bolt positioning
        lda     $9CFC,y                 ; cursor X base (per grid position)
        sta     $01                     ; save X base for bolt positioning
        lda     $95                     ; frame counter
        lsr     a                       ; divide by 2
        lsr     a                       ; divide by 4
        lsr     a                       ; ÷8
        and     #$01                    ; alternates 0/1
        clc                             ; prepare for add
        adc     #$E4                    ; tile $E4 or $E5 (bolt flash)
        sta     $02                     ; save bolt tile ID
        ldx     #$03                    ; 4 bolts (3→0)
        ldy     #$C8                    ; OAM offset $C8 (sprite 50-53)
cursor_bolt_sprite_load_loop:  lda     temp_00 ; Y base
        clc                             ; prepare for add
        adc     $9D05,x                 ; + Y offset (0 or 38 for top/bottom)
        sta     $0200,y                 ; → OAM Y
        lda     $01                     ; X base
        clc                             ; prepare for add
        adc     $9D09,x                 ; + X offset (0 or 42 for left/right)
        sta     $0203,y                 ; → OAM X
        lda     $02                     ; bolt tile ($E4/$E5)
        sta     $0201,y                 ; → OAM tile
        dey                             ; previous OAM slot
        dey                             ; Y -= 4 total
        dey                             ; (OAM entry = 4 bytes)
        dey                             ; back to previous slot Y pos
        dex                             ; next bolt (3→0)
        bpl     cursor_bolt_sprite_load_loop ; loop all 4 bolts
select_loop_next_frame:  lda     #$00   ; allow NMI
        sta     nmi_skip                ; clear NMI skip flag
        jsr     task_yield              ; wait for next frame
        inc     nmi_skip                ; block NMI again
        inc     $95                     ; advance bolt flash timer
        jmp     stage_select_input_loop ; back to input check

; ===========================================================================
; Stage select confirmation (A or Start pressed)
; ===========================================================================
; Called when the player presses A/Start on the stage select screen.
; Validates the selection, then initiates the stage transition:
;   1. Fills the offscreen nametable with boss intro layout (blue band)
;   2. Loads transition palettes from $9C53
;   3. Jumps to bank03 $A000 for the horizontal scroll + boss name reveal
;
; The boss intro layout is a solid light-blue band (NES $21) spanning
; tile rows 11-18 (y=88-151), bordered by white ($20) lines at y=88-89
; and y=150-151. This band is written into the offscreen nametable by
; fill_nametable_progressive ($1FEF8C) using metatile data from bank $13.
; The NMI/IRQ scroll split then creates a 3-strip effect:
;   - Top strip    (y=0-87):    scrolls horizontally, sliding portraits away
;   - Middle band  (y=88-151):  stays fixed, shows blue intro band + boss sprite
;   - Bottom strip (y=152-239): scrolls horizontally, sliding portraits away
; ---------------------------------------------------------------------------

        jsr     clear_entity_table      ; clear all entity slots
        lda     $12                     ; $12 = cursor column (0-2)
        clc                             ; prepare for add
        adc     $13                     ; $13 = cursor row offset (0/3/6)
        tay                             ; Y = grid index (0-8)
        cpy     #$04                    ; index 4 = center position
        bne     stage_select_check_beaten_boss ; not center → try select
        lda     stage_select_page       ; center only selectable when
        cmp     #$12                    ; $60 == $12 (all Doc Robots beaten)
        bne     stage_select_check_beaten_boss ; not all beaten → normal select
        jmp     wily_gate_entry         ; → Wily fortress entrance

stage_select_check_beaten_boss:  lda     bosses_beaten ; boss-defeated bitmask
        and     $9DED,y                 ; check if this boss already beaten
        bne     select_loop_next_frame  ; already beaten → back to select loop
        lda     stage_select_page       ; game progression page
        cmp     #$0A                    ; >= $0A = invalid state
        bcs     select_loop_next_frame  ; → back to select loop
        tya                             ; Y = grid index (0-8)
        clc                             ; prepare for addition
        adc     stage_select_page       ; + page offset (0=Robot Masters, $0A=Doc Robot)
        tay                             ; Y = adjusted grid index for table lookup

; Stage select lookup table at $9CE1 (9 entries per page):
;   Index: 0     1     2     3     4     5     6     7     8
;   Value: $06   $05   $00   $03   $FF   $04   $02   $01   $07
;   Boss:  Spark Snake Needl Hard  (n/a) Top   Gemin Magnt Shadw
;   Grid:  TL    TM    TR    ML    CTR   MR    BL    BM    BR
        lda     $9CE1,y                 ; look up stage number
        bmi     select_loop_next_frame  ; $FF = center (not selectable)
        sta     stage_id                ; set current stage
        sty     $0F                     ; $0F = save adjusted grid index

; --- Set up PRG/CHR banks for the boss intro layout ---
        lda     #$13                    ; select PRG bank $13
        sta     prg_bank                ; (contains boss intro metatile data
        jsr     select_PRG_banks        ; at $AF00+, referenced by fill routine)
        lda     #$04                    ; set rendering mode
        sta     oam_ptr                 ; OAM offset = 4 (skip sprite 0)
        jsr     prepare_oam_buffer      ; clear unused OAM sprites
        lda     #$76                    ; set CHR bank $76
        sta     $E9                     ; (boss intro screen tileset)
        jsr     update_CHR_banks        ; apply CHR bank change

; --- Determine which nametable to fill (the offscreen one) ---
; $FD bit 0 = currently displayed nametable. Toggle it so the NEW
; nametable becomes visible, then compute $10 = old nametable's
; PPU address bit (0 or 4) to write the intro layout into it first.
        lda     camera_x_hi             ; toggle displayed nametable
        pha                             ; save original NT bit
        eor     #$01                    ; toggle NT select
        sta     camera_x_hi             ; store new displayed nametable
        pla                             ; $10 = OLD nametable select bit
        and     #$01                    ; bit 0 → shifted left 2 = 0 or 4
        asl     a                       ; 0 = nametable $2000, 4 = nametable $2400
        asl     a                       ; = 0 or 4
        sta     $10                     ; nametable select for PPU writes

; --- Set up metatile pointer and fill nametable ---
; metatile_column_ptr_by_id computes: ($20/$21) = $AF00 + (A << 6)
; A=$03 → column 3 of bank $13 level data → pointer $AFC0
        lda     #$03                    ; set metatile column pointer
        jsr     metatile_column_ptr_by_id ; ($20/$21) → $AFC0 in bank $13
        lda     #$00                    ; $70 = nametable fill progress (0-63)
        sta     $70                     ; starts at 0
        sta     nmi_skip                ; 0 = allow NMI

; Fill the offscreen nametable progressively.
; fill_nametable_progressive writes 4 tile rows per call.
; 16 calls × 4 rows = 64 rows → full nametable.
; When $70 reaches $40, it writes the attribute table and resets $70 to 0.
boss_intro_nametable_fill_wait_loop:  lda     $10 ; preserve nametable select
        pha                             ; save across JSR
        jsr     fill_nametable_progressive ; write 4 rows to PPU queue
        jsr     task_yield              ; wait for NMI (PPU uploads queued data)
        pla                             ; restore nametable select
        sta     $10                     ; write back nametable select
        lda     $70                     ; $70 = 0 when fill complete
        bne     boss_intro_nametable_fill_wait_loop ; loop until done

; --- Load transition palette and jump to bank03 ---
; $9C53: 16-byte palette for the boss intro screen (BG palettes only).
; This replaces the stage select palettes with the intro band colors.
        ldy     #$0F                    ; 16 palette bytes
transition_palette_load_loop:  lda     $9C53,y ; copy 16 bytes from $9C53
        sta     $0600,y                 ; to BG palette buffer
        sta     $0620,y                 ; and working copy
        dey                             ; loop 15→0
        bpl     transition_palette_load_loop ; loop all 16 bytes
        sty     palette_dirty           ; Y=$FF → flag palette upload

; Jump to bank03 entry point.
; Y = adjusted grid index (used by bank03 to index stage parameter tables).
; This is a JMP (not JSR) — bank03's RTS returns to whoever called the
; stage select, not back here.
        lda     #$03                    ; select PRG bank $03
        sta     prg_bank                ; set PRG bank register
        jsr     select_PRG_banks        ; switch to bank $03
        ldy     $0F                     ; Y = adjusted grid index
        jmp     data_compressed_nametable_start ; → bank03 stage_transition_entry

; --- call_bank01_set_chr ($938B) ---
; Trampoline: calls bank01 set_room_chr_and_palette, then restores bank03.
; Called from bank03 (stage_transition_entry) with A = CHR param index.
call_bank01_set_chr:
        pha                             ; save A on stack
        lda     #$01                    ; switch to bank $01
        sta     prg_bank                ; set PRG bank register
        jsr     select_PRG_banks        ; apply bank switch
        pla                             ; restore A from stack
        jsr     data_compressed_nametable_start ; call into bank $01
        lda     #$03                    ; switch back to bank $03
        sta     prg_bank                ; set PRG bank register
        jmp     select_PRG_banks        ; apply and return

; ---------------------------------------------------------------------------
; write_ppu_data_from_bank03 — copy a PPU write command list from bank03
; ---------------------------------------------------------------------------
; Parameters: X = table index, $10 = nametable select (0 or 4)
; Loads a pointer from bank03 tables $A56D/$A580 and copies the data
; to the PPU write queue at $0780+. The data format is:
;   byte 0: PPU high address (ORed with $10 for nametable select)
;   byte 1: PPU low address
;   byte 2: tile count (N)
;   bytes 3..3+N: tile data
;   ... (repeats for more rows)
;   terminator: high byte with bit 7 set ($FF)
; Used to write nametable tile data for the boss intro screen.
; ---------------------------------------------------------------------------
write_ppu_data_from_bank03:

        sty     $04                     ; save Y
        stx     $05                     ; save table index
        lda     prg_bank                ; save current PRG bank
        pha                             ; push current bank
        lda     #$03                    ; switch to bank 03
        sta     prg_bank                ; (contains PPU data tables)
        jsr     select_PRG_banks        ; switch to bank $03
        ldx     $05                     ; X = table index
        lda     ppu_write_table_low_ptr,x ; ($02/$03) = pointer to PPU data
        sta     $02                     ; set pointer low byte
        lda     ppu_write_table_high_ptr,x ; high byte from $A580+X
        sta     $03                     ; set pointer high byte
        ldy     #$00                    ; start of PPU data
ppu_write_queue_read_addr:  lda     ($02),y ; PPU high address
        ora     $10                     ; OR nametable select bit
        sta     $0780,y                 ; store to PPU write queue
        bmi     ppu_write_queue_done    ; bit 7 set = terminator ($FF)
        iny                             ; next byte
        lda     ($02),y                 ; PPU low address
        sta     $0780,y                 ; store PPU low addr
        iny                             ; next byte
        lda     ($02),y                 ; tile count
        sta     $0780,y                 ; store tile count to queue
        sta     temp_00                 ; save count for loop
        iny                             ; advance to tile data
ppu_write_queue_tile_loop:  lda     ($02),y ; tile data byte
        sta     $0780,y                 ; copy tile to PPU queue
        iny                             ; next tile byte
        dec     temp_00                 ; decrement remaining count
        bpl     ppu_write_queue_tile_loop ; loop until all tiles copied
        bmi     ppu_write_queue_read_addr ; next PPU entry (always branches)
ppu_write_queue_done:  sta     nametable_dirty ; $FF → flag PPU write pending
        pla                             ; restore original PRG bank
        sta     prg_bank                ; set saved PRG bank
        jsr     select_PRG_banks        ; switch back to original bank
        ldy     $04                     ; restore Y
        rts                             ; return
; ---------------------------------------------------------------------------
; init_password_cursor — place password cursor sprite at default position
; ---------------------------------------------------------------------------
; Sets OAM sprite 0 to the password cursor: Y=$B7, tile=$ED, attr=$00, X=$40.
; Y=$B7 = "GAME START" row, Y=$C7 = "STAGE SELECT" row.
; ---------------------------------------------------------------------------
password_cursor_init:
        lda     #$B7                    ; Y = $B7 (Game Start row)
        sta     $0200                   ; OAM sprite 0 Y pos
        lda     #$ED                    ; tile $ED = cursor arrow
        sta     $0201                   ; OAM sprite 0 tile
        lda     #$00                    ; attr $00 = sprite palette 0
        sta     $0202                   ; OAM sprite 0 attr
        lda     #$40                    ; X = $40 (column position)
        sta     $0203                   ; OAM sprite 0 X pos
        rts                             ; return

; ---------------------------------------------------------------------------
; update_password_cursor — move cursor between Game Start / Stage Select
; ---------------------------------------------------------------------------
; D-pad Up ($08) → Y=$B7 (Game Start), Down ($04) → Y=$C7 (Stage Select).
; ---------------------------------------------------------------------------
password_cursor_update:
        lda     joy1_press              ; read new button presses
        and     #$0C                    ; Up ($08) / Down ($04)
        beq     password_cursor_done    ; no vertical input → skip
        ldy     #$B7                    ; assume Up → Game Start
        and     #$08                    ; test Up bit specifically
        bne     password_cursor_set_y   ; if Up, use $B7
        ldy     #$C7                    ; Down → Stage Select
password_cursor_set_y:  sty     $0200   ; update sprite Y position
password_cursor_done:  rts

; ===========================================================================
; Robot Master stage intro sequence
; ===========================================================================
; Called after bank03 returns from the horizontal scroll + boss name reveal.
; This routine handles:
;   1. Refill HP/weapon energy
;   2. Fill nametable with boss intro layout (blue band screen)
;   3. Write stage-specific nametable data (boss face tiles)
;   4. Boss sprite drops from top of screen to center of blue band
;   5. Boss does intro animation, then switches to idle pose
;   6. Wait, then Mega Man teleports in from below
;   7. Palette fade to black + boss face flash
;   8. Jump to stage loading
;
; Verified via Mesen save states:
;   - Blue band: y=88-151 (tile rows 11-18), white borders at y=88-89/150-151
;   - Boss sprite centered at ~x=124, y=112 (42×30 px bounding box)
;   - Boss name text at y=136-143 (tile row 17, col 11 = PPU $222B)
;   - Boss drops from y=$E8 (232) to y=$74 (116) at 4px/frame = 29 frames
;   - Mega Man rises from y=$80 (128) to y=$C0 (192) at 2px/frame = 32 frames
; ---------------------------------------------------------------------------

        ldy     #$0B                    ; refill weapon energy
weapon_energy_refill_loop:  lda     player_hp,y ; player_hp+Y = weapon ammo
        bpl     weapon_energy_refill_continue ; positive → weapon OK, skip refill
        lda     #HEALTH_FULL            ; full energy (28 units)
        sta     player_hp,y             ; store full energy for weapon Y
weapon_energy_refill_continue:  dey     ; next weapon slot
        bpl     weapon_energy_refill_loop ; loop until all weapons checked
        lda     stage_id                ; stage < $08 = Robot Master
        cmp     #STAGE_DOC_NEEDLE       ; stage >= $08 = Doc Robot/Wily
        bcc     robot_master_intro_entry ; Robot Master → intro sequence
        jmp     stage_loading_entry     ; → skip to stage loading for Doc/Wily

; --- Robot Master intro animation ---

robot_master_intro_entry:  jsr     fade_palette_in ; disable sprites/rendering
        lda     #$04                    ; set rendering mode
        sta     oam_ptr                 ; OAM pointer = 4
        jsr     prepare_oam_buffer      ; configure PPU
        jsr     task_yield              ; wait 1 frame
        lda     #MUSIC_BOSS_INTRO       ; play sound $35
        jsr     submit_sound_ID_D9      ; (boss intro fanfare)
        jsr     reset_stage_state       ; zero out game state

; Fill nametable 0 with boss intro layout from bank $13.
; Temporarily set $22 = $14 so metatile pointer references the correct
; level section in bank $13 (stage $14 = the boss intro screen layout).
; metatile_column_ptr_by_id with A=$07 → metatile column 7 → pointer $B0C0.
        lda     stage_id                ; save real stage number
        pha                             ; push real stage_id to stack
        lda     #$14                    ; $22 = $14 (boss intro layout ID)
        sta     stage_id                ; set boss intro layout ID
        lda     #$07                    ; metatile column 7
        jsr     metatile_column_ptr_by_id ; pointer → $B0C0 in bank $13
robot_master_nametable_fill_wait_loop:  lda     #$00 ; $10 = 0 → write to nametable $2000
        sta     $10                     ; target nametable $2000
        jsr     fill_nametable_progressive ; write 4 tile rows
        jsr     task_yield              ; wait for NMI
        lda     $70                     ; $70 = 0 when complete
        bne     robot_master_nametable_fill_wait_loop ; loop until fill complete
        pla                             ; restore real stage number
        sta     stage_id                ; restore real stage_id

; Write stage-specific nametable data (boss face, decorations).
; Two calls to write_ppu_data_from_bank03:
;   X=$05: common intro data
;   X=$22+$06: per-boss face data
        ldx     #$05                    ; table index $05 = common data
        lda     #$00                    ; $10 = 0 → nametable $2000
        sta     $10                     ; target nametable $2000
        jsr     write_ppu_data_from_table ; write common intro nametable data
        jsr     task_yield              ; wait for NMI
        lda     stage_id                ; X = stage + 6
        clc                             ; (per-boss face data table index)
        adc     #$06                    ; offset by 6 for per-boss entry
        tax                             ; X = table index for this boss
        jsr     write_ppu_data_from_table ; write boss-specific face tiles

; Load CHR bank configuration and palettes for the intro screen.
; $9D46: 6 bytes of CHR bank IDs → $E8-$ED
; $9D16: 32 bytes of palette data (BG + sprite) → $0620 working copy
        ldy     #$05                    ; 6 CHR bank IDs (Y=5..0)
intro_chr_bank_load_loop:  lda     $9D46,y ; load CHR bank ID from table
        sta     $E8,y                   ; store to CHR bank shadow $E8-$ED
        dey                             ; next CHR bank slot
        bpl     intro_chr_bank_load_loop ; loop until all 6 loaded
        jsr     update_CHR_banks        ; apply CHR bank configuration
        ldy     #$1F                    ; 32 palette bytes (Y=$1F..0)
intro_palette_load_loop:  lda     $9D16,y ; 32 bytes: BG ($0620-$062F) + sprite ($0630-$063F)
        sta     $0620,y                 ; store to palette working copy
        dey                             ; next palette byte
        bpl     intro_palette_load_loop ; loop until all 32 loaded

; --- Set up boss entity and begin drop animation ---
; Entity slot 0: the boss sprite (reused as the intro display entity).
; ent_x_px = entity 0 X position (player X during gameplay)
; ent_y_px = entity 0 Y position (player Y during gameplay)
        lda     #$80                    ; entity 0 X = $80 (128 = centered)
        sta     ent_x_px                ; set entity 0 X position
        lda     #$E8                    ; entity 0 Y = $E8 (232 = below screen)
        sta     ent_y_px                ; set entity 0 Y position
        ldx     #$00                    ; set animation to $B0
        lda     #$B0                    ; (boss intro drop animation)
        jsr     reset_sprite_anim       ; init boss drop animation
        lda     ent_flags               ; clear bit 6 = facing left
        and     #$BF                    ; mask off bit 6 (H-flip)
        sta     ent_flags               ; face right
        jsr     task_yield              ; wait 1 frame
        jsr     fade_palette_out        ; enable rendering

; Boss drop loop: decrement Y from $E8 to $74 at 4px/frame.
; ($E8 - $74) / 4 = 29 frames for the boss to slide down.
; When anim phase (ent_anim_state) reaches $02, switch to idle anim $1A.
boss_drop_animation_loop:  lda     ent_y_px ; if Y == $74, skip decrement
        cmp     #$74                    ; (target reached)
        beq     boss_drop_anim_phase_check ; at target Y → check anim phase
        sec                             ; Y -= 4
        sbc     #$04                    ; move boss up 4 pixels
        sta     ent_y_px                ; update entity Y position
        lda     #$00                    ; reset anim frame counter
        sta     ent_anim_frame          ; (keep animation progressing)
boss_drop_anim_phase_check:  lda     ent_anim_state ; check animation phase
        cmp     #$02                    ; phase 2 = intro anim done
        bne     boss_drop_frame_process_loop ; not phase 2 → keep animating
        ldx     #$00                    ; switch to idle animation $1A
        lda     #$1A                    ; idle animation ID
        jsr     reset_sprite_anim       ; switch to idle pose
boss_drop_frame_process_loop:  jsr     process_frame_yield_full ; process sprites + wait for NMI
        lda     ent_anim_id             ; check current OAM ID
        cmp     #$1A                    ; $1A = idle pose active
        bne     boss_drop_animation_loop ; loop until idle
        ldx     #$3C                    ; wait $3C (60) frames
        jsr     task_yield_x            ; (boss stands idle)

; --- Mega Man teleport-in animation ---
; Mega Man rises from Y=$80 to Y=$C0, 2px/frame = 32 frames.
; (Teleporting from below the blue band upward into view.)
        lda     ent_x_px                ; if X == $C0, done
        cmp     #$C0                    ; target X = $C0
        beq     boss_idle_wait_before_fadeout ; done → proceed to fadeout
        clc                             ; X += 2
        adc     #$02                    ; (note: using ent_x_px which is
        sta     ent_x_px                ; the X position for the entity)
        jsr     process_frame_yield_full ; process sprites + wait for NMI
        jmp     robot_master_intro_loop ; loop back for next frame

; --- Palette fade to black ---
; Fade all 3 BG palette groups to black ($0F).
; Each call to fade_palette_to_black subtracts $10 per step
; from palette colors, 4 frames per step, until all reach $0F.

boss_idle_wait_before_fadeout:  ldx     #$3C ; wait $3C (60) frames
        jsr     task_yield_x            ; pause before fade
        lda     #$00                    ; clear NMI skip flag
        sta     nmi_skip                ; allow NMI processing
        ldy     #$03                    ; fade BG palette 0 (bytes $00-$03)
        jsr     boss_energy_fill_loop   ; fade palette group 0 to black
        ldy     #$07                    ; fade BG palette 1 (bytes $04-$07)
        jsr     boss_energy_fill_loop   ; fade palette group 1 to black
        ldy     #$0B                    ; fade BG palette 2 (bytes $08-$0B)
        jsr     boss_energy_fill_loop   ; fade palette group 2 to black
        ldx     #$B4                    ; wait $B4 (180) frames
        jsr     task_yield_x            ; (3 seconds on black screen)
        jmp     stage_loading_entry     ; → stage loading

; ---------------------------------------------------------------------------
; boss_face_palette_flash — alternates boss/default sprite palettes
; ---------------------------------------------------------------------------
; Called after the last palette fade (Y=$07 → code_189509 via BEQ).
; Flashes 17 times ($10 counts $10 down to $00), 2 frames per flash.
; Even frames: load boss-specific face colors from $9BB7 table.
; Odd frames: restore default palette from $0630 working copy.
; ---------------------------------------------------------------------------

boss_face_palette_flash:  lda     #$10  ; $10 = flash counter (17 iterations)
        sta     $10                     ; store flash counter
boss_face_flash_frame_loop:  ldy     #$03 ; process 4 palette colors (Y=3..0)
        lda     $10                     ; even/odd toggle
        and     #$01                    ; test bit 0 for even/odd
        bne     boss_face_default_palette_restore_loop ; odd → restore defaults

; Even frames: load boss face colors from $9BB7 table.
; $9BB7 + stage*8 = 8-byte palette (bright + dark variant).
; Colors 0-3 from $9BB7 → SP 0 ($0610), colors 4-7 from $9BBB → SP 1 ($0618).
        lda     stage_id                ; X = stage * 8 + 3
        asl     a                       ; stage_id * 2
        asl     a                       ; stage_id * 4
        asl     a                       ; stage_id * 8
        ora     #$03                    ; set low 2 bits (offset 3)
        tax                             ; X = stage*8 + 3 (table index)
boss_face_bright_palette_load_loop:  lda     $9BB7,x ; bright variant → SP 0
        sta     $0610,y                 ; write to sprite palette 0
        lda     $9BBB,x                 ; dark variant → SP 1
        sta     $0618,y                 ; write to sprite palette 1
        dex                             ; next table entry
        dey                             ; next palette color
        bne     boss_face_bright_palette_load_loop ; loop until 4 colors copied
        beq     boss_face_flash_complete_check ; (always branches)

; Odd frames: copy default palettes back from $0630/$0638 working copy.
boss_face_default_palette_restore_loop:  lda     $0630,y ; default SP 0
        sta     $0610,y                 ; restore sprite palette 0
        lda     $0638,y                 ; default SP 1
        sta     $0618,y                 ; restore sprite palette 1
        dey                             ; next palette color
        bne     boss_face_default_palette_restore_loop ; loop until 4 colors restored
boss_face_flash_complete_check:  inc     palette_dirty ; flag palette upload
        ldx     #$02                    ; wait 2 frames per flash
        jsr     task_yield_x            ; delay between flashes
        dec     $10                     ; decrement flash counter
        bpl     boss_face_flash_frame_loop ; loop until all flashes done
        rts                             ; return to caller

; ---------------------------------------------------------------------------
; fade_palette_to_black — progressively fade a BG palette group to black
; ---------------------------------------------------------------------------
; Parameters: Y = palette end index (3, 7, or $0B for BG palette 0/1/2)
; Subtracts $10 per step from each color in the palette group.
; Colors that would go below $0F are clamped to $0F (black).
; 4 steps × $10 = $40 total subtraction. 4 frames per step.
; Source: $0600,y (base palette). Destination: $0604,y (working palette).
; After the last step (Y=$07), chains into boss_face_palette_flash.
; ---------------------------------------------------------------------------
fade_palette_to_black:

        lda     #$30                    ; $10 = starting subtract value
        sta     $10                     ; (decreases $30→$20→$10→$00)
        sty     $11                     ; save palette end index
palette_fade_black_color_loop:  ldy     $11 ; restore palette index
        ldx     #$03                    ; 4 colors per palette group
palette_fade_subtract_loop:  lda     $0600,y ; load base palette color
        sec                             ; prepare for subtraction
        sbc     $10                     ; subtract fade amount
        bcs     palette_fade_store_color ; if no underflow, use result
        lda     #$0F                    ; clamp to black ($0F)
palette_fade_store_color:  sta     $0604,y ; store to working palette
        dey                             ; next palette color
        dex                             ; decrement color counter
        bpl     palette_fade_subtract_loop ; loop until 4 colors faded
        sty     palette_dirty           ; flag palette upload
        ldx     #$04                    ; wait 4 frames per step
        jsr     task_yield_x            ; wait 4 frames per fade step
        lda     $10                     ; $10 -= $10 (next darker step)
        sec                             ; prepare for subtraction
        sbc     #$10                    ; reduce fade offset by $10
        sta     $10                     ; store new fade amount
        bcs     palette_fade_black_color_loop ; loop while $10 >= 0
        lda     $11                     ; if this was BG palette 1 (Y=$07),
        cmp     #$07                    ; chain into boss face flash
        beq     boss_face_palette_flash ; yes → chain to palette flash
        ldx     #$1E                    ; wait $1E (30) frames between groups
        jsr     task_yield_x            ; wait between palette groups
        rts                             ; return to caller
; ===========================================================================
; Stage loading dispatcher ($9581)
; ===========================================================================
; Called after robot master intro or Doc Robot stage. Checks if all 8 Robot
; Masters are beaten ($61=$FF) to trigger Doc Robot stage select setup.
; Otherwise falls through to normal stage return at normal_return_setup.
; ===========================================================================
stage_load_dispatch:
        lda     bosses_beaten           ; load boss-defeated bitmask
        cmp     #$FF                    ; all 8 Robot Masters beaten?
        beq     doc_robot_select_setup  ; yes → set up Doc Robot stage select
        jmp     normal_stage_return     ; no → normal return to stage select

; ===========================================================================
; All Robot Masters beaten → Doc Robot stage select setup ($958A)
; ===========================================================================
; Sets up the stage select screen for Doc Robot phase. Builds both
; nametables (one with Robot Master portraits, one with Doc Robot layout),
; loads Doc Robot CHR banks and palettes, then enters stage select loop.
;
; If $60=0 (first time all beaten): sets $60=$09, $61=$3A (Doc Robot mask),
;   loads Doc Robot CHR bank $74, new palettes from $9D36.
; If $60!=0 (returning from Doc Robot): sets $60=$12 (all Doc Robots beaten),
;   writes center Mega Man portrait, loads Wily center face sprites.
; ===========================================================================
doc_robot_select_setup:  jsr     fade_palette_in ; disable rendering
        lda     #$04                    ; set OAM pointer to page 4
        sta     oam_ptr                 ; store OAM page
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     reset_stage_state       ; clear stage variables
        lda     #MUSIC_STAGE_SELECT     ; music $10 = stage select theme
        jsr     submit_sound_ID_D9      ; play stage select music
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank                ; switch to bank $13
        jsr     select_PRG_banks        ; apply PRG bank switch
; --- Fill nametable 0 with stage select layout ---
        lda     #$01                    ; metatile column 1
        jsr     metatile_column_ptr_by_id ; set metatile column pointer
        lda     #$00                    ; clear fill progress counter
        sta     $70                     ; store to $70
doc_nt0_fill_loop:  lda     #$00        ; nametable select = $2000
        sta     $10                     ; nametable $2000
        jsr     fill_nametable_progressive ; fill one column of tiles
        jsr     task_yield              ; wait for NMI
        lda     $70                     ; check fill progress
        bne     doc_nt0_fill_loop       ; loop until complete
; --- Blank defeated boss portraits and write nametable data ---
        lda     #$00                    ; nametable select = $2000
        sta     $10                     ; store to $10
        jsr     draw_defeated_portraits ; blank defeated portraits
        lda     #$00                    ; nametable select = $2000
        sta     $10                     ; store to $10
        ldx     #$03                    ; PPU data table $03
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        jsr     task_yield              ; wait for NMI
        ldx     #$04                    ; PPU data table $04
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        jsr     task_yield              ; wait for NMI
; --- Fill nametable 1 (offscreen) ---
        lda     #$04                    ; metatile column 4
        jsr     metatile_column_ptr_by_id ; set metatile column pointer
        lda     #$00                    ; clear fill progress counter
        sta     $70                     ; store to $70
doc_nt1_fill_loop:  lda     #$04
        sta     $10                     ; nametable $2400
        jsr     fill_nametable_progressive ; fill one column of tiles
        jsr     task_yield              ; wait for NMI
        lda     $70                     ; check fill progress
        bne     doc_nt1_fill_loop       ; loop until complete
; --- Load stage select CHR banks and palettes ---
        lda     #$7C                    ; CHR bank $7C
        sta     $E8                     ; set BG CHR slot 0
        lda     #$7E                    ; CHR bank $7E
        sta     $E9                     ; set BG CHR slot 1
        lda     #$36                    ; sprite CHR bank
        sta     $EC                     ; set sprite CHR bank
        lda     #$34                    ; BG CHR bank
        sta     $ED                     ; set BG CHR bank
        jsr     update_CHR_banks        ; apply CHR bank settings
        ldy     #$0F                    ; copy 16 bytes (4 palettes)
doc_bg_palette_loop:  lda     $9C33,y   ; load BG palettes (fresh start set)
        sta     $0620,y                 ; store to BG palette buffer
        dey                             ; decrement index
        bpl     doc_bg_palette_loop     ; loop until all copied
        ldy     #$0F                    ; copy 16 bytes (4 palettes)
doc_spr_palette_loop:  lda     $9C23,y  ; load sprite palettes
        sta     $0630,y                 ; store to sprite palette buffer
        dey                             ; decrement index
        bpl     doc_spr_palette_loop    ; loop until all copied
        jsr     task_yield              ; wait for NMI
        jsr     load_stage_select_oam   ; load OAM sprites from bank03
; --- Enable rendering and enter stage select ---
        lda     #$00                    ; clear palette dirty flag
        sta     palette_dirty           ; store to palette_dirty
        jsr     task_yield              ; wait for NMI
        lda     #$58                    ; IRQ scanline = $58
        sta     $5E                     ; scroll split position
        lda     #$07                    ; mode $07 = stage select
        sta     game_mode               ; game mode $07 = stage select
        jsr     task_yield              ; wait for NMI
        jsr     fade_palette_out        ; enable rendering
; --- Check if first time or returning from Doc Robot ---
        lda     stage_select_page       ; check stage select page
        beq     doc_first_time_init     ; $60=0 → first time all beaten
; --- Returning from Doc Robot stage: set $60=$12 (all Doc Robots done) ---
        lda     #$12                    ; page $12 = all Doc Robots done
        sta     stage_select_page       ; set stage select page
        ldy     #$00                    ; Y = 0 (start at portrait 0)
        sty     $10                     ; nametable select = 0
        ldx     #$19                    ; frame delay
        jsr     write_portrait_frames   ; write_portrait_frames
        jsr     write_center_portrait   ; write_center_portrait
        jsr     load_wily_center_face   ; load Wily center face OAM sprites
        jmp     boss_intro_sprite_loop  ; → enter stage select loop

; --- First time all Robot Masters beaten → init Doc Robot phase ---
doc_first_time_init:  ldx     #$F0      ; wait 240 frames (4 seconds)
        jsr     task_yield_x            ; wait 240 frames
        lda     #$3A                    ; Doc Robot defeated mask (initial)
        sta     bosses_beaten           ; set Doc Robot beaten mask
        lda     #$09                    ; $60=$09 = Doc Robot page
        sta     stage_select_page       ; set Doc Robot select page
        lda     #$74                    ; Doc Robot CHR bank
        sta     $E9                     ; set BG CHR slot 1
        jsr     update_CHR_banks        ; apply CHR bank settings
; --- Load Doc Robot palettes from $9D36 ---
        ldy     #$0F                    ; copy 16 bytes (4 palettes)
doc_robot_palette_loop:  lda     $9D36,y ; load Doc Robot BG palette
        sta     $0600,y                 ; store to base palette
        sta     $0620,y                 ; store to working palette
        dey                             ; decrement index
        bpl     doc_robot_palette_loop  ; loop until all copied
        sty     palette_dirty           ; flag palette upload
; --- Write Doc Robot portrait frames and faces ---
        lda     #$00                    ; nametable select = 0
        sta     $10                     ; store to $10
        ldy     #$01                    ; start from portrait index 1
        ldx     #$19                    ; frame delay
        jsr     write_portrait_frames   ; write_portrait_frames
        ldx     #$19                    ; frame delay = $19
        jsr     write_portrait_faces    ; write_portrait_faces

; --- Enter stage select loop at center position ---
doc_cursor_init:
        lda     #$01                    ; cursor column = 1 (center)
        sta     $12                     ; store cursor column
        lda     #$03                    ; cursor row offset = 3 (middle row)
        sta     $13                     ; store cursor row
        jmp     stage_select_input_loop ; → stage select main loop
; ===========================================================================
; Normal stage return → stage select ($968C)
; ===========================================================================
; Returns to stage select after completing a normal stage (not all beaten).
; Rebuilds nametable 1 with stage select layout, loads return palettes
; from $9C43, writes PPU data, then enters the stage select loop.
; ===========================================================================
normal_return_setup:
        jsr     fade_palette_in         ; disable rendering
        lda     #$04                    ; set OAM pointer to page 4
        sta     oam_ptr                 ; store OAM page
        jsr     prepare_oam_buffer      ; clear OAM buffer
        jsr     task_yield              ; wait for NMI
        jsr     reset_stage_state       ; clear stage variables
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank                ; switch to bank $13
        jsr     select_PRG_banks        ; apply PRG bank switch
; --- Fill nametable 1 (displayed on return) ---
        lda     #$01                    ; select nametable 1
        sta     camera_x_hi             ; display nametable 1
        lda     #$02                    ; metatile column 2
        jsr     metatile_column_ptr_by_id ; set metatile column pointer
return_nt1_fill_loop:  lda     #$04
        sta     $10                     ; nametable $2400
        jsr     fill_nametable_progressive ; fill one column of tiles
        jsr     task_yield              ; wait for NMI
        lda     $70                     ; check fill progress
        bne     return_nt1_fill_loop    ; loop until complete
; --- Load return palettes and CHR banks ---
        lda     #$7C                    ; CHR bank $7C
        sta     $E8                     ; set BG CHR slot 0
        lda     #$76                    ; CHR bank $76
        sta     $E9                     ; set BG CHR slot 1
        lda     #$36                    ; sprite CHR bank
        sta     $EC                     ; set sprite CHR bank
        lda     #$34                    ; BG CHR bank
        sta     $ED                     ; set BG CHR bank
        jsr     update_CHR_banks        ; apply CHR bank settings
        ldy     #$0F                    ; copy 16 bytes (4 palettes)
return_bg_palette_loop:  lda     $9C43,y ; return-from-stage BG palettes
        sta     $0620,y                 ; store to BG palette buffer
        dey                             ; decrement index
        bpl     return_bg_palette_loop  ; loop until all copied
        ldy     #$0F                    ; copy 16 bytes (4 palettes)
return_spr_palette_loop:  lda     $9C23,y ; sprite palettes
        sta     $0630,y                 ; store to sprite palette buffer
        dey                             ; decrement index
        bpl     return_spr_palette_loop ; loop until all copied
; --- Write PPU data and call bank03 for portrait data ---
        lda     #$04                    ; nametable 1 select
        sta     $10                     ; nametable $2400
        ldx     #$12                    ; PPU data table $12
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        jsr     task_yield              ; wait for NMI
        lda     #$03                    ; bank $03 = portrait routines
        sta     prg_bank                ; switch to bank $03
        jsr     select_PRG_banks        ; apply PRG bank switch
        jsr     bank03_portrait_setup_routine ; call bank03 routine (portrait setup)
        jsr     fade_palette_out        ; enable rendering

; --- Wait for Start/A button to enter stage select ---
return_wait_button:
        lda     joy1_press              ; read new button presses
        and     #$90                    ; Start ($10) or A ($80)
        bne     return_button_pressed   ; button pressed → proceed
        jsr     task_yield              ; wait for NMI
        jmp     boss_anim_state_check   ; loop back to button check

return_button_pressed:  jmp     stage_select_proto_man_oam ; → clear OAM and re-enter stage select

; --- write_portrait_frames ---
; Writes portrait frame border tiles to the nametable for all 8 boss positions.
; Uses PPU update buffer at $0780+. Format per group:
;   $0780: PPU addr high, $0781: PPU addr low, $0782: tile count-1, $0783+: tile IDs
; Base addresses from $9D4C/$9D55 tables, tile layout from $9D5E data.
; Iterates y=0,2,4(skip center),6,8 for all 9 grid positions minus Mega Man center.
;
; Portrait frame pixel positions (decoded from nametable addresses):
;   Col 0: x=16   Col 1: x=96   Col 2: x=176
;   Row 0: y=24   Row 1: y=88   Row 2: y=152
; Frame dimensions: 8×6 tiles (64×48 px)

        stx     $0F                     ; save frame delay to $0F
portrait_frame_addr_load_loop:  lda     $9D4C,y ; PPU addr low byte for portrait y
        sta     temp_00                 ; store PPU addr low
        lda     $9D55,y                 ; PPU addr high byte ($20/$21/$22)
        ora     $10                     ; OR nametable select bit
        sta     $01                     ; store PPU addr high
        ldx     #$00                    ; X = 0 (buffer offset)
portrait_frame_tile_row_process_loop:  lda     $9D5E,x ; row Y-offset (bit 7 = end marker)
        bmi     portrait_frame_row_complete ; bit 7 set = end of row data
        lda     $9D5F,x                 ; row X-offset within nametable
        clc                             ; clear carry for add
        adc     temp_00                 ; + base low byte
        sta     $0781,x                 ; → PPU addr low
        lda     $9D5E,x                 ; row offset (carry into high byte)
        adc     $01                     ; + base high byte
        sta     $0780,x                 ; → PPU addr high
        inx                             ; advance buffer index
        inx                             ; skip to tile count byte
        lda     $9D5E,x                 ; tile count (N → writes N+1 tiles)
        sta     $0780,x                 ; store count to PPU queue
        sta     $02                     ; save as loop counter
        inx                             ; advance to first tile byte
portrait_frame_tile_write_loop:  lda     $9D5E,x ; tile ID
        sta     $0780,x                 ; copy tile to PPU queue
        inx                             ; advance buffer index
        dec     $02                     ; decrement tile counter
        bpl     portrait_frame_tile_write_loop ; more tiles → continue
        bmi     portrait_frame_tile_row_process_loop ; done → next row
portrait_frame_row_complete:  sta     $0780,x ; $FF end marker
        sta     nametable_dirty         ; flag nametable update
        tya                             ; save portrait index Y
        pha                             ; push Y to stack
        ldx     $0F                     ; restore frame delay
        jsr     task_yield_x            ; submit PPU update buffer
        pla                             ; pull Y from stack
        tay                             ; restore portrait index Y
portrait_frame_next_position_loop:  iny ; advance to next portrait
        iny                             ; y += 2
        cpy     #$04                    ; check for center position
        beq     portrait_frame_next_position_loop ; skip index 4 (center = Mega Man)
        cpy     #$09                    ; all 9 positions done?
        bcc     portrait_frame_addr_load_loop ; loop until all 9 done
        rts                             ; return

; --- write_portrait_faces ---
; Writes 4×4 portrait face tiles into each boss portrait frame.
; Face tiles from $9D99-$9DA8 are arranged as 4 rows of 4 tiles each.
; PPU addresses from $9DC9 (high) / $9DD2 (low), rows spaced $20 (one nametable row).
; Also writes attribute table entry at $23C0+ for palette assignment.
;
; Face pixel positions (2 tiles inside the frame border):
;   Col 0: x=32   Col 1: x=112  Col 2: x=192
;   Row 0: y=32   Row 1: y=96   Row 2: y=160
; Face dimensions: 4×4 tiles (32×32 px)
;
; Face tile layout (same tiles for ALL portraits — actual portrait selected by CHR bank):
;   Row 0: $C8 $C9 $CA $CB
;   Row 1: $D8 $D9 $DA $DB
;   Row 2: $E8 $E9 $EA $EB
;   Row 3: $F8 $F9 $FA $FB

        stx     $0F                     ; save frame delay to $0F
        ldx     #$03                    ; X = 3 (copy 4 tiles)
portrait_face_tile_copy_loop:  lda     $9D99,x ; face row 0 tiles ($C8-$CB)
        sta     $0783,x                 ; store to PPU buffer row 0
        lda     $9D9D,x                 ; face row 1 tiles ($D8-$DB)
        sta     $078A,x                 ; store to PPU buffer row 1
        lda     $9DA1,x                 ; face row 2 tiles ($E8-$EB)
        sta     $0791,x                 ; store to PPU buffer row 2
        lda     $9DA5,x                 ; face row 3 tiles ($F8-$FB)
        sta     $0798,x                 ; store to PPU buffer row 3
        dex                             ; decrement tile index
        bpl     portrait_face_tile_copy_loop ; loop until all 4 copied
        ldy     #$00                    ; Y = 0 (first portrait)
portrait_face_ppu_addr_setup_loop:  lda     $9DC9,y ; PPU addr high ($20/$21/$22)
        ora     $10                     ; OR nametable select bit
        sta     $0780                   ; row 0 PPU high
        sta     $0787                   ; row 1 PPU high
        sta     $078E                   ; row 2 PPU high
        sta     $0795                   ; row 3 PPU high
        clc                             ; clear carry for add
        lda     $9DD2,y                 ; PPU addr low (base)
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; + 1 nametable row (32 bytes)
        sta     $0788                   ; row 1 PPU low
        adc     #$20                    ; +32 = row 2
        sta     $078F                   ; row 2 PPU low
        adc     #$20                    ; +32 = row 3
        sta     $0796                   ; row 3 PPU low
        lda     #$03                    ; 4 tiles per row (count-1)
        sta     $0782                   ; row 0 tile count
        sta     $0789                   ; row 1 tile count
        sta     $0790                   ; row 2 tile count
        sta     $0797                   ; row 3 tile count
        lda     #$23                    ; attribute table addr high ($23xx)
        ora     $10                     ; OR nametable select bit
        sta     $079C                   ; store attr PPU addr high
        lda     #$C0                    ; attribute table base ($23C0)
        ora     $9DA9,y                 ; + portrait-specific attr offset
        sta     $079D                   ; store attr PPU addr low
        lda     #$00                    ; 1 attribute byte (count-1=0)
        sta     $079E                   ; store attr byte count
        lda     #$55                    ; attr value $55 = palette 1 for all quadrants
        sta     $079F                   ; store attr palette value
        lda     #$FF                    ; end marker
        sta     $07A0                   ; store end marker
        sta     nametable_dirty         ; flag nametable update
        tya                             ; save portrait index Y
        pha                             ; push Y to stack
        ldx     $0F                     ; restore frame delay
        jsr     task_yield_x            ; submit PPU update buffer
        pla                             ; pull Y from stack
        tay                             ; restore portrait index Y
portrait_face_next_position_loop:  iny  ; advance to next portrait
        iny                             ; y += 2
        cpy     #$04                    ; check for center position
        beq     portrait_face_next_position_loop ; skip center
        cpy     #$09                    ; all 9 positions done?
        bcc     portrait_face_ppu_addr_setup_loop ; no → next portrait
        rts                             ; return
; ---------------------------------------------------------------------------
; write_center_portrait — write Mega Man center face tiles to nametable
; ---------------------------------------------------------------------------
; Writes the 4×4 center portrait tiles (different from boss portraits:
; $CC-$CF, $DC-$DF, $EC-$EF, $FC-$FF) to PPU address from $9DB2/$9DB3
; ($218E = pixel 112,96). Also writes attribute table data from $9DC4
; and calls write_ppu_data_from_bank03 with table $0E for additional data.
; ---------------------------------------------------------------------------
        lda     $9DB2                   ; PPU addr high ($21)
        ora     $10                     ; OR nametable select
        sta     $0780                   ; row 0 PPU high
        sta     $0787                   ; row 1 PPU high
        sta     $078E                   ; row 2 PPU high
        sta     $0795                   ; row 3 PPU high
        clc                             ; clear carry for add
        lda     $9DB3                   ; PPU addr low ($8E)
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; +32 = next nametable row
        sta     $0788                   ; row 1 PPU low
        adc     #$20                    ; +32 = row 2
        sta     $078F                   ; row 2 PPU low
        adc     #$20                    ; +32 = row 3
        sta     $0796                   ; row 3 PPU low
        ldx     #$03                    ; 4 tiles per row
        stx     $0782                   ; row 0 tile count
        stx     $0789                   ; row 1 tile count
        stx     $0790                   ; row 2 tile count
        stx     $0797                   ; row 3 tile count
center_face_tile_loop:  lda     $9DB4,x ; center face row 0: $CC-$CF
        sta     $0783,x                 ; store to PPU buffer row 0
        lda     $9DB8,x                 ; center face row 1: $DC-$DF
        sta     $078A,x                 ; store to PPU buffer row 1
        lda     $9DBC,x                 ; center face row 2: $EC-$EF
        sta     $0791,x                 ; store to PPU buffer row 2
        lda     $9DC0,x                 ; center face row 3: $FC-$FF
        sta     $0798,x                 ; store to PPU buffer row 3
        dex                             ; decrement tile index
        bpl     center_face_tile_loop   ; loop until all 4 copied
; --- Write center attribute table data ---
        ldx     #$04                    ; copy 5 attr bytes
center_attr_copy_loop:  lda     $9DC4,x ; $23DB: attr addr, 2 bytes: $88,$22
        sta     $079C,x                 ; store to attr buffer
        dex                             ; decrement index
        bpl     center_attr_copy_loop   ; loop until all copied
        sta     $079C                   ; store loaded attr high byte
        ora     $10                     ; OR nametable select
        sta     $079C                   ; store adjusted attr addr high
        stx     $07A1                   ; end marker ($FF from X=$FF)
        stx     nametable_dirty         ; X=$FF → flag nametable dirty
        jsr     task_yield              ; wait for NMI
        ldx     #$0E                    ; PPU data table $0E
        jsr     write_ppu_data_from_table ; write_ppu_data_from_bank03
        rts                             ; return
; ===========================================================================
; Password screen entry point ($985D)
; ===========================================================================
; Entered from the title screen (JMP $9003). Builds the password/stage
; select screen nametable, loads palettes, plays music $0E (password theme),
; displays the "Game Start / Stage Select" menu, then waits for Start.
;
; After Start is pressed:
;   Cursor at $C7 (Stage Select) → normal stage select flow
;   Cursor at $B7 (Game Start) → check if stage >= Wily ($0C)
;     Stage < Wily → clear OAM, enter stage select at game_start_clear_oam
;     Stage >= Wily → jump to bank03 $A879 (Wily stage entry)
;   If $60=$12 (all Doc Robots beaten) → Wily fortress gate entrance
; ===========================================================================
password_screen_init:                   ; (3 seconds on black screen)
        jsr     fade_palette_in         ; disable rendering
        lda     #$04                    ; OAM page 4 ($0400)
        sta     oam_ptr                 ; set OAM write pointer
        jsr     prepare_oam_buffer      ; clear OAM sprites
        jsr     task_yield              ; wait for NMI
        lda     #MUSIC_PASSWORD         ; music $0E = password screen theme
        jsr     submit_sound_ID_D9      ; play password music
        jsr     reset_stage_state       ; clear scroll/weapon state
        lda     #$01                    ; select nametable 1
        sta     camera_x_hi             ; display nametable 1
        lda     #$00                    ; clear scroll position
        sta     scroll_y                ; clear vertical scroll
; --- Fill nametable 1 with password screen layout ---
        lda     #$02                    ; metatile column 2
        jsr     metatile_column_ptr_by_id ; set metatile pointer for col 2
password_nt_fill_loop:  lda     #$04    ; nametable $2400 high byte
        sta     $10                     ; nametable $2400
        jsr     fill_nametable_progressive ; write 4 tile rows to PPU queue
        jsr     task_yield              ; wait for NMI
        lda     $70                     ; check fill progress
        bne     password_nt_fill_loop   ; loop until complete
; --- Load CHR banks and palettes ---
        lda     #$7C                    ; BG CHR bank 0
        sta     $E8                     ; store to CHR slot
        lda     #$76                    ; BG CHR bank 1
        sta     $E9                     ; store to CHR slot
        lda     #$36                    ; sprite CHR bank
        sta     $EC                     ; store to sprite CHR slot
        lda     #$34                    ; BG CHR bank
        sta     $ED                     ; store to sprite CHR slot
        jsr     update_CHR_banks        ; apply CHR bank settings
        ldy     #$0F                    ; 16 palette bytes
password_bg_palette_loop:  lda     $9C43,y ; return-from-stage BG palettes
        sta     $0620,y                 ; store to BG palette buffer
        dey                             ; next byte
        bpl     password_bg_palette_loop ; loop 16 bytes
        ldy     #$0F                    ; 16 palette bytes
password_spr_palette_loop:  lda     $9C23,y ; sprite palettes
        sta     $0630,y                 ; store to sprite palette buffer
        dey                             ; next byte
        bpl     password_spr_palette_loop ; loop 16 bytes
        jsr     task_yield              ; wait for NMI
; --- Write PPU data and enable rendering ---
        ldx     #$01                    ; PPU data table $01
        lda     #$04                    ; nametable 1 select
        sta     $10                     ; nametable $2400
        jsr     write_ppu_data_from_table ; write PPU data from bank03
        lda     #$58                    ; IRQ scanline = $58
        sta     $5E                     ; scroll split position
        lda     #$07                    ; mode $07 = stage select
        sta     game_mode               ; game mode $07 = stage select
        lda     #$03                    ; bank $03 = portrait data
        sta     prg_bank                ; bank $03
        jsr     select_PRG_banks        ; switch PRG bank
        jsr     bank03_portrait_setup_routine ; call bank03 portrait routine
        jsr     task_yield              ; wait for NMI
        jsr     fade_palette_out        ; enable rendering
; --- Wait 120 frames ($78) before showing cursor ---
        lda     #$78                    ; 120-frame delay
password_startup_delay_loop:  pha       ; save counter
        jsr     task_yield              ; wait for NMI
        pla                             ; restore counter
        sec                             ; set carry for subtract
        sbc     #$01                    ; decrement counter
        bne     password_startup_delay_loop ; loop until counter = 0
; --- Show "Game Start / Stage Select" cursor ---
        lda     #$04                    ; nametable 1 select
        sta     $10                     ; nametable $2400
        ldx     #$02                    ; PPU data table $02
        jsr     write_ppu_data_from_table ; write menu text to PPU
        jsr     password_cursor_oam_setup ; set up cursor sprite at Y=$B7

; --- Password cursor loop: wait for Start ---
password_input_loop:
        jsr     proto_man_sprite_control ; handle cursor Up/Down input
        lda     joy1_press              ; read new button presses
        and     #BTN_START              ; check Start button
        bne     password_selection_process ; Start pressed → process selection
        jsr     task_yield              ; wait for NMI
        jmp     proto_man_anim_loop     ; loop back to cursor input

; --- Process password selection ---
password_selection_process:  ldx     #$0B ; 12 weapon/HP slots
weapon_slot_check_loop:  lda     player_hp,x ; check weapon/HP slot
        bpl     weapon_slot_next        ; positive = valid, skip
        lda     #HEALTH_FULL            ; set to full HP
        sta     player_hp,x             ; fill this weapon/HP slot
weapon_slot_next:  dex                  ; next slot
        bpl     weapon_slot_check_loop  ; loop all 12 slots
; --- Check cursor position to determine action ---
        lda     $0200                   ; cursor Y position
        cmp     #$C7                    ; $C7 = "Stage Select" row
        beq     password_set_lives      ; → set lives and enter stage select
        lda     stage_id                ; check current stage
        cmp     #STAGE_WILY1            ; stage >= $0C = Wily stages
        bcs     wily_stage_via_bank03   ; → Wily stage entry via bank03
        jsr     stage_select_proto_man_oam ; Game Start: re-enter stage select
; --- Set starting lives and check for Wily gate ---
password_set_lives:  lda     #$02       ; 2 extra lives (3 total)
        sta     lives                   ; set starting lives
        lda     stage_select_page       ; check stage select page
        cmp     #$12                    ; $12 = all Doc Robots beaten
        bne     password_done           ; not all Doc Robots → return
        jmp     wily_gate_entry         ; → Wily fortress gate entrance

password_done:  rts                     ; return to caller

; --- Wily stage entry (stage >= $0C) ---
wily_stage_via_bank03:  lda     #$03    ; bank $03
        sta     prg_bank                ; set PRG bank
        jsr     select_PRG_banks        ; switch PRG bank
        jmp     bank03_wily_stage_entry ; → bank03 Wily stage entry

; ---------------------------------------------------------------------------
; reset_stage_state — zero out game state for stage transition
; ---------------------------------------------------------------------------
; Clears scroll position, weapon state, screen page, and various
; game state variables to prepare for the boss intro or stage load.
; ---------------------------------------------------------------------------

        lda     #$00                    ; zero accumulator
        sta     data_compressed_nametable_start ; clear mapped bank flag
        sta     $59                     ; game sub-state
        sta     camera_screen           ; camera/scroll page
        sta     ent_x_scr               ; entity 0 screen page (Y high)
        sta     ent_y_scr               ; entity 0 screen page (X high)
        sta     $B1                     ; scroll-related
        sta     $B2                     ; scroll-related
        sta     $B3                     ; scroll-related
        sta     camera_x_hi             ; horizontal scroll (nametable select)
        sta     camera_x_lo             ; horizontal scroll (sub-tile)
        sta     weapon_cursor           ; menu cursor
        sta     $B4                     ; scroll-related
        sta     current_weapon          ; weapon ID (0 = Mega Buster)
        sta     $9E                     ; clear weapon sub-state
        sta     $9F                     ; clear weapon sub-state
        sta     $70                     ; nametable fill progress counter
        rts                             ; return
; ---------------------------------------------------------------------------
; blank_defeated_portraits — erase face tiles for beaten bosses ($995C)
; ---------------------------------------------------------------------------
; Iterates through all 9 grid positions. For beaten bosses (bit set in
; $61), writes blank tile $24 over the 4×4 face area. Also handles
; special cases for Doc Robot phase and Wily gate center portrait.
; ---------------------------------------------------------------------------
blank_portraits_start:
        jsr     write_portrait_frame_data ; restore portrait frames if needed
        ldx     #$08                    ; iterate grid positions 8→0
blank_portraits_loop:  lda     $9DED,x  ; boss bitmask for position X
        beq     blank_center_check      ; $00 = center position (special case)
        and     bosses_beaten           ; check if this boss beaten
        beq     blank_portraits_next    ; not beaten → skip
; --- Write blank tiles over defeated boss face ---
blank_face_write:  lda     $9DC9,x      ; PPU addr high for face position
        ora     $10                     ; OR nametable select
        sta     $0780                   ; row 0 PPU high
        sta     $0787                   ; row 1 PPU high
        sta     $078E                   ; row 2 PPU high
        sta     $0795                   ; row 3 PPU high
        clc                             ; clear carry for addition
        lda     $9DD2,x                 ; PPU addr low for face position
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; +32 = next NT row
        sta     $0788                   ; row 1 PPU low
        adc     #$20                    ; +32 = next NT row
        sta     $078F                   ; row 2 PPU low
        adc     #$20                    ; +32 = next NT row
        sta     $0796                   ; row 3 PPU low
        lda     #$03                    ; 4 bytes per row (length - 1)
        sta     $0782                   ; row 0 tile count
        sta     $0789                   ; row 1 tile count
        sta     $0790                   ; row 2 tile count
        sta     $0797                   ; row 3 tile count
        ldy     #$03                    ; loop counter for 4 tiles
        lda     #$24                    ; tile $24 = blank (empty frame interior)
blank_face_tile_loop:  sta     $0783,y  ; fill all 4 rows with blank tiles
        sta     $078A,y                 ; row 1 blank tile
        sta     $0791,y                 ; row 2 blank tile
        sta     $0798,y                 ; row 3 blank tile
        dey                             ; next tile
        bpl     blank_face_tile_loop    ; loop all 4 tiles
        sty     $079C                   ; end marker ($FF)
        sty     nametable_dirty         ; flag PPU write
        jsr     task_yield              ; wait for PPU update
blank_portraits_next:  dex
        bpl     blank_portraits_loop    ; next grid position
; --- If all Doc Robots beaten, write center Mega Man portrait ---
        lda     stage_select_page       ; check game progression
        beq     blank_portraits_done    ; $60=0 → Robot Master phase, skip
        cmp     #$12                    ; $12 = all Doc Robots beaten
        bne     blank_portraits_done    ; not all beaten → skip center
        jsr     write_center_portrait   ; draw center Mega Man face
        jsr     task_yield              ; wait for PPU update
blank_portraits_done:  rts

; --- Center position special case ---
blank_center_check:  lda     stage_select_page
        beq     blank_portraits_next    ; $60=0 → skip center
        cmp     #$12                    ; $12 = all Doc Robots beaten
        beq     blank_face_write        ; → blank center face too
        lda     bosses_beaten           ; check bosses defeated mask
        cmp     #$FF                    ; all bosses beaten?
        bne     blank_portraits_next    ; no → skip
        beq     blank_face_write        ; yes → blank center face

; ---------------------------------------------------------------------------
; restore_portrait_frames_if_needed — redraw frames for Doc Robot phase
; ---------------------------------------------------------------------------
; If $60!=0 (Doc Robot or later), redraws portrait frames and faces
; since they may have been modified. If $60=$12, also redraws from index 0.
; ---------------------------------------------------------------------------
restore_frames_start:
        lda     stage_select_page       ; check stage select page
        beq     restore_frames_done     ; $60=0 → nothing to restore
        ldy     #$01                    ; start from portrait index 1
        ldx     #$01                    ; frame delay = 1
        jsr     write_portrait_frames   ; redraw portrait frames from 1
        lda     stage_select_page       ; re-check stage select page
        cmp     #$12                    ; all Doc Robots beaten?
        bne     restore_faces_call      ; no → just redraw faces
        ldy     #$00                    ; also redraw from index 0
        ldx     #$01                    ; frame delay = 1
        jsr     write_portrait_frames   ; redraw portrait frames from 0
restore_faces_call:  ldx     #$01       ; delay = 1 frame per face
        jsr     write_portrait_faces    ; redraw portrait face tiles
restore_frames_done:  rts
; ---------------------------------------------------------------------------
; load_stage_select_oam — load OAM sprites for stage select screen ($99FA)
; ---------------------------------------------------------------------------
; Copies portrait detail sprites from bank03 ($A231+) into OAM ($0200+).
; If Doc Robot phase ($60!=0), loads from offset $98 (fewer sprites) and
; applies Doc Robot CHR bank $74 and palettes from $9D36.
; After loading, hides OAM sprites for beaten bosses using $9DDB/$9DE4
; tables (Y=$F8 = offscreen).
; ---------------------------------------------------------------------------
load_select_oam_start:
        ldx     #$00                    ; start offset (Robot Master phase)
        lda     stage_select_page       ; check stage select page
        beq     oam_copy_start          ; $60=0 → use full sprite set
; --- Doc Robot phase: load fewer sprites, different CHR/palettes ---
        lda     #$74                    ; Doc Robot CHR bank
        sta     $E9                     ; set BG CHR slot 1
        jsr     update_CHR_banks        ; apply CHR bank settings
        ldy     #$0F                    ; 16 palette bytes
doc_oam_palette_loop:  lda     $9D36,y  ; Doc Robot palettes
        sta     $0600,y                 ; store to BG palette buffer
        sta     $0620,y                 ; store to BG palette working
        dey                             ; next byte
        bpl     doc_oam_palette_loop    ; loop all 16 palette bytes
        sty     palette_dirty           ; flag palette upload
        ldx     #$98                    ; start from OAM offset $98
; --- Copy OAM data from bank03 stage_select_oam_y_table table ---
oam_copy_start:  stx     temp_00        ; save start offset
        lda     prg_bank                ; save current PRG bank
        pha                             ; push to stack
        lda     #$03                    ; switch to bank03
        sta     prg_bank                ; set PRG bank register
        jsr     select_PRG_banks        ; apply bank switch
        ldx     temp_00                 ; restore OAM start offset
oam_copy_loop:  lda     stage_select_oam_y_table,x ; OAM Y position
        sta     $0200,x                 ; store Y to OAM buffer
        lda     stage_select_oam_tile_table,x ; OAM tile ID
        sta     $0201,x                 ; store tile to OAM buffer
        lda     stage_select_oam_attr_table,x ; OAM attribute
        sta     $0202,x                 ; store attr to OAM buffer
        lda     stage_select_oam_x_table,x ; OAM X position
        sta     $0203,x                 ; store X to OAM buffer
        inx                             ; advance to next OAM entry
        inx                             ; X += 4 total
        inx                             ; (OAM entry = 4 bytes)
        inx                             ; next sprite slot
        cpx     #$CC                    ; copy up to offset $CC (51 sprites)
        bne     oam_copy_loop           ; loop until all sprites copied
        pla                             ; restore saved PRG bank
        sta     prg_bank                ; set PRG bank number
        jsr     select_PRG_banks        ; switch PRG bank back
; --- Hide OAM sprites for beaten bosses ---
        ldx     #$08                    ; iterate grid positions 8→0
beaten_boss_hide_loop:  lda     $9DED,x ; boss bitmask for position X
        beq     beaten_center_check     ; $00 = center (special case)
        and     bosses_beaten           ; check if beaten
        beq     beaten_boss_next        ; not beaten → keep sprites visible
; --- Move beaten boss sprites offscreen (Y=$F8) ---
hide_portrait_sprites:  ldy     $9DDB,x ; OAM start offset for this portrait
        lda     $9DE4,x                 ; sprite count for this portrait
        sta     temp_00                 ; store sprite count
        lda     #$F8                    ; Y=$F8 = offscreen
hide_sprite_loop:  sta     $0200,y      ; set Y offscreen
        iny                             ; advance to next OAM entry
        iny                             ; Y += 4 total
        iny                             ; (OAM entry = 4 bytes)
        iny                             ; next OAM entry
        dec     temp_00                 ; decrement sprite count
        bpl     hide_sprite_loop        ; loop until all hidden
beaten_boss_next:  dex                  ; next grid position
        bpl     beaten_boss_hide_loop   ; loop all 9 positions
        rts                             ; return

; --- Center position special case ---
beaten_center_check:  lda     stage_select_page ; check stage select page
        beq     beaten_boss_next        ; $60=0 → skip center hiding
        cmp     #$12                    ; all Doc Robots beaten?
        beq     wily_center_load        ; → load Wily face sprites, then hide
        lda     bosses_beaten           ; check bosses beaten mask
        cmp     #$FF                    ; all bosses beaten?
        bne     beaten_boss_next        ; no → skip
        beq     hide_portrait_sprites   ; yes → hide center sprites
wily_center_load:  jsr     load_wily_center_face ; load Wily center face OAM sprites
        jmp     portrait_oam_write_loop ; hide original center sprites
; ---------------------------------------------------------------------------
; load_wily_center_sprites — load Wily skull face OAM into center ($9A87)
; ---------------------------------------------------------------------------
; When all Doc Robots are beaten ($60=$12), the center portrait shows
; Dr. Wily's skull. This copies 9 OAM sprite entries (36 bytes) from
; $9DF6 to OAM $02DC-$02FC (center portrait area) and loads Wily-specific
; sprite palettes from $9E1A.
; ---------------------------------------------------------------------------
wily_sprite_copy_start:
        sty     temp_00                 ; save Y
        ldy     #$20                    ; copy 9 sprites (offset $20→$00)
wily_sprite_copy_loop:  lda     $9DF6,y ; Wily center sprite Y
        sta     $02DC,y                 ; store Y to center OAM area
        lda     $9DF7,y                 ; Wily center sprite tile
        sta     $02DD,y                 ; store tile to center OAM area
        lda     $9DF8,y                 ; Wily center sprite attr
        sta     $02DE,y                 ; store attr to center OAM area
        lda     $9DF9,y                 ; Wily center sprite X
        sta     $02DF,y                 ; store X to center OAM area
        dey                             ; move to previous OAM entry
        dey                             ; Y -= 4 total
        dey                             ; (OAM entry = 4 bytes)
        dey                             ; back to previous slot Y pos
        bpl     wily_sprite_copy_loop   ; loop until all 9 copied
; --- Load Wily sprite palettes from $9E1A ---
        ldy     #$0F                    ; 16 palette bytes
wily_palette_load_loop:  lda     $9E1A,y ; Wily sprite palette data
        sta     $0610,y                 ; sprite palette buffer
        sta     $0630,y                 ; sprite palette working copy
        dey                             ; next palette byte
        bpl     wily_palette_load_loop  ; loop all 16 bytes
        sty     palette_dirty           ; flag palette upload
        ldy     temp_00                 ; restore Y
        rts                             ; return

; ===========================================================================
; Wily fortress gate entrance ($9ABC)
; ===========================================================================
; Entered when player selects center position with $60=$12 (all Doc Robots
; beaten). Discards return address (PLA×2), then falls through to the
; gate sequence at $9ABE.
;
; The gate sequence loads Hard Man's stage (bank $03) to render the fortress
; approach background, sets up Mega Man entity at X=$30, plays music $0C
; (Wily stage theme), then jumps to the fixed bank gate animation at $C9B3.
; ===========================================================================
wily_gate_discard_ret:                  ; submit PPU update buffer
        pla                             ; discard return address (2 bytes)
        pla                             ; discard return address low
; --- Wily gate entry point (also called from $9006 jump table) ---
wily_gate_init:                         ; y += 2
        jsr     fade_palette_in         ; disable rendering
        jsr     reset_stage_state       ; clear scroll/weapon state
        lda     #$04                    ; OAM page 4 ($0400)
        sta     oam_ptr                 ; set OAM pointer
        jsr     prepare_oam_buffer      ; clear OAM sprites
        jsr     task_yield              ; wait for NMI
        jsr     clear_entity_table      ; clear all entity slots
; --- Set up fortress approach ---
        lda     #$01                    ; set flag = 1
        sta     data_compressed_nametable_start ; enable compressed NT data
        lda     #$00                    ; zero accumulator
        sta     game_mode               ; game mode $00
        sta     $9E                     ; clear unknown state var
        sta     $9F                     ; clear unknown state var
        sta     nmi_skip                ; allow NMI processing
        sta     ent_y_scr               ; entity 0 screen page
        sta     ent_y_px                ; entity 0 Y position
        lda     #$17                    ; level width = $17 screens
        sta     $29                     ; level width
        lda     #$20                    ; scroll boundary = $20
        sta     $2A                     ; scroll boundary
        lda     #$30                    ; X position = $30
        sta     ent_x_px                ; Mega Man X = $30
        lda     #FACING_RIGHT           ; facing right
        sta     player_facing           ; facing right
        sta     $23                     ; set screen counter
        sta     $2E                     ; set scroll sub-state
        lda     #$0D                    ; value $0D
        sta     $2B                     ; set level height param
; --- Load Hard Man's stage for fortress approach background ---
        lda     #STAGE_HARD             ; stage $03 = Hard Man
        sta     stage_id                ; set stage to Hard Man
        sta     prg_bank                ; PRG bank = stage ID ($03)
        jsr     select_PRG_banks        ; switch to Hard Man bank
; --- Render 33 columns of fortress approach nametable ---
        lda     #$1F                    ; start at screen column $1F
        sta     $24                     ; set current screen column
        lda     #$21                    ; 33 columns to render
fortress_column_loop:  pha              ; save loop counter
        lda     #$01                    ; nametable 1
        sta     $10                     ; set nametable select
        jsr     do_render_column        ; do_render_column
        jsr     task_yield              ; wait for NMI
        pla                             ; restore loop counter
        sec                             ; set carry for subtract
        sbc     #$01                    ; decrement counter
        bne     fortress_column_loop    ; loop until all columns done
; --- Set up fortress CHR banks and palettes ---
        sta     $2C                     ; clear scroll sub-state
        sta     $2D                     ; clear scroll sub-state
        lda     #$50                    ; fortress CHR banks
        sta     $E8                     ; store to BG CHR slot 0
        lda     #$52                    ; fortress BG CHR bank 1
        sta     $E9                     ; store to BG CHR slot 1
        lda     #$1D                    ; fortress sprite CHR
        sta     $EC                     ; store to sprite CHR slot 0
        lda     #$1E                    ; fortress sprite CHR bank 1
        sta     $ED                     ; store to sprite CHR slot 1
        jsr     update_CHR_banks        ; apply CHR bank settings
; --- Load fortress palettes from $9E2A ---
        ldy     #$1F                    ; 32 bytes (BG + sprite palettes)
fortress_palette_loop:  lda     $9E2A,y ; fortress palette data (32 bytes)
        sta     $0620,y                 ; store to palette buffer
        dey                             ; next byte
        bpl     fortress_palette_loop   ; loop until Y < 0 (all 32 bytes)
; --- Set up entity slot $1F for fortress gate ---
        lda     #$2A                    ; fortress gate scroll position
        sta     $52                     ; set gate scroll offset
        ldx     #$1F                    ; entity slot $1F = fortress gate
        lda     #$80                    ; $80 = entity active
        sta     $031F                   ; ent_status[$1F] = active
        lda     #$94                    ; $94 = bits 7,4,2
        sta     $059F                   ; ent_flags[$1F]
        lda     #$53                    ; AI routine $53
        sta     $033F                   ; ent_routine[$1F]
        lda     #$18                    ; HP = $18 (24)
        sta     $04FF                   ; ent_hp[$1F]
        lda     #$C2                    ; hitbox shape $C2
        sta     $049F                   ; ent_hitbox[$1F]
        lda     #$01                    ; X velocity = 1 (moving right)
        sta     $043F                   ; ent_xvel[$1F]
; --- Set up Mega Man entity ---
        jsr     reset_gravity           ; clear player gravity state
        lda     #$99                    ; walking animation
        jsr     reset_sprite_anim       ; set walking animation
        lda     #$18                    ; screen page $18
        sta     camera_screen           ; camera at screen $18
        sta     ent_x_scr               ; entity 0 screen page
        sta     $039F                   ; ent_x_scr[$1F] = $18
        lda     #$C0                    ; X pixel = $C0 (192)
        sta     $037F                   ; entity $1F X position
        lda     #FACING_LEFT            ; facing left
        sta     $04BF                   ; ent_facing[$1F] = left
; --- Clear various entity fields ---
        lda     #$00                    ; zero out remaining fields
        sta     $041F                   ; ent_xvel_sub[$1F] = 0
        sta     $03FF                   ; ent_y_scr[$1F] = 0
        sta     $03DF                   ; ent_y_px[$1F] = 0
        sta     $051F                   ; ent_timer[$1F] = 0
        sta     $053F                   ; ent_var1[$1F] = 0
        sta     $055F                   ; ent_var2[$1F] = 0
        sta     $057F                   ; ent_var3[$1F] = 0
        sta     $0100                   ; task stack area
        sta     $0101                   ; clear task stack byte 1
        sta     $0102                   ; clear task stack byte 2
        sta     $0103                   ; clear task stack byte 3
; --- Start fortress approach ---
        lda     #$30                    ; X position = $30 (48)
        sta     ent_x_px                ; Mega Man X = $30
        lda     #MUSIC_WILY_MAP         ; music $0C = Wily stage theme
        jsr     submit_sound_ID_D9      ; play Wily map music
        jsr     task_yield              ; yield to let music start
        jsr     fade_palette_out        ; enable rendering
        jmp     game_entry_set_hp_scroll ; → fixed bank gate animation

; =============================================================================
; DATA TABLES ($9BB7-$9FFF)
; =============================================================================
; Boss palette data, CHR bank configuration, stage select lookup tables,
; cursor position tables, portrait frame/face data, and password screen data.
;
; $9BB7-$9BF6: boss_face_palettes — 8 bosses × 8 bytes (bright + dark SP palettes)
; $9BF7-$9BFC: chr_bank_table — 6 CHR bank IDs for stage select screen
; $9BFD-$9BFE: music_track_table — [0]=fresh start music, [1]=return music
; $9BFF-$9C00: password_cursor_y — [0]=$30 (down), [1]=$40 (up)
; $9C01-$9C02: palette_offset_table — BG palette offsets for fresh/return
; $9C03-$9C22: bg_palette_data — BG palette sets (fresh=$9C33, return=$9C43)
; $9C23-$9C32: sprite_palette_data — 16 bytes sprite palettes
; $9C33-$9C42: bg_palette_fresh — BG palettes for fresh start
; $9C43-$9C52: bg_palette_return — BG palettes for return from stage
; $9C53-$9C62: bg_palette_transition — BG palettes for boss intro transition
; $9C63-$9C68: scroll_params — scroll setup parameters
; $9C69-$9C6C: initial_oam — 4-byte initial OAM sprite (cursor sentinel)
; $9C6D-$9C74: (alignment padding)
; $9C75-$9CE0: megaman_eye_sprites — 9 positions × 12 bytes eye OAM data
; $9CE1-$9CF2: stage_select_lookup — grid index → stage number
; $9CF3-$9CFB: cursor_pos_y — Y pixel base per grid position
; $9CFC-$9D04: cursor_pos_x — X pixel base per grid position
; $9D05-$9D08: cursor_bolt_offset_y — 4 corner Y offsets
; $9D09-$9D0C: cursor_bolt_offset_x — 4 corner X offsets
; $9D0D-$9D15: cursor_direction_offsets — d-pad → position delta
; $9D16-$9D35: intro_palette_data — 32 bytes BG+sprite palettes for intro
; $9D36-$9D45: doc_robot_palette_data — 16 bytes Doc Robot BG palettes
; $9D46-$9D4B: intro_chr_banks — 6 CHR bank IDs for boss intro
; $9D4C-$9D54: portrait_addr_lo — PPU low bytes for frame positions
; $9D55-$9D5D: portrait_addr_hi — PPU high bytes for frame positions
; $9D5E-$9D98: portrait_frame_tiles — frame border tile layout
; $9D99-$9DA8: face_tiles — 4×4 boss face tile IDs
; $9DA9-$9DB1: face_attr_offsets — attribute table offsets per portrait
; $9DB2-$9DC3: center_portrait_data — Mega Man center face tiles + PPU addr
; $9DC4-$9DC8: center_attr_data — center portrait attribute table write
; $9DC9-$9DD1: face_addr_hi — PPU high bytes for face writes
; $9DD2-$9DDA: face_addr_lo — PPU low bytes for face writes
; $9DDB-$9DE3: portrait_oam_offsets — OAM start offset per portrait
; $9DE4-$9DEC: portrait_sprite_counts — sprite count per portrait
; $9DED-$9DF5: boss_bitmask — bit mask for each grid position
; $9DF6-$9E19: wily_center_sprites — 9 OAM entries for Wily skull face
; $9E1A-$9E29: wily_sprite_palettes — 16 bytes Wily sprite palettes
; $9E2A-$9E49: fortress_palettes — 32 bytes fortress approach palettes
; $9E4A-$9FFF: password screen RLE data / unused padding
; =============================================================================

; --- boss_face_palettes ($9BB7) ---
; 8 bosses × 8 bytes: 4 bright + 4 dark palette colors.
; Used by boss_face_palette_flash to alternate during intro fadeout.
; Order: Needle, Magnet, Gemini, Hard, Top, Snake, Spark, Shadow
        .byte   $0F,$30,$30,$17,$0F,$07,$30,$17,$0F,$30,$10,$16,$0F,$06,$10,$16
        .byte   $0F,$30,$30,$21,$0F,$11,$30,$21,$0F,$30,$10,$01,$0F,$0C,$10

palette_data_spark_bright:.byte   $01,$0F,$30,$27,$00,$0F,$00,$28,$00,$0F,$30,$30

palette_data_snake_dark:.byte   $19,$0F,$09,$30,$19,$0F,$30,$30

palette_data_needle_bright:.byte   $26,$0F,$16,$30,$26,$0F,$30,$34,$14,$0F,$04,$34,$14,$7C,$76,$38

palette_data_gemini_dark:.byte   $39,$36,$25,$10,$0E,$A7

palette_data_hard_bright:.byte   $97

palette_data_hard_dark:.byte   $30,$40,$0F,$20,$21,$0F,$0F,$21,$20,$09,$0F,$16

palette_data_top_bright:.byte   $20,$06,$0F,$20,$37

palette_data_top_dark:.byte   $0F,$0F,$30,$3C,$11,$0F,$16,$26,$27,$0F,$01,$2C,$11,$0F,$30,$37
        .byte   $26,$0F,$30,$15

palette_data_magnet_dark:.byte   $11,$0F,$37,$21,$10,$0F,$37,$26,$15,$0F,$37,$26,$0F,$0F,$20,$21
        .byte   $11,$0F,$20,$37,$29,$0F,$20,$26,$16,$0F,$20,$10,$21

palette_data_shadow_dark:.byte   $0F,$20,$21,$11,$0F,$20,$37,$11,$0F,$20,$10,$0F,$0F,$20,$37,$0F
        .byte   $0F,$20,$21,$11,$0F,$27

palette_data_section_end:.byte   $17,$06,$0F,$27,$17,$21,$0F,$20,$10,$21,$04,$02,$04,$FC,$00,$FF
        .byte   $97,$ED,$01,$28,$47,$67,$01,$C0,$47,$68,$01,$C8,$F0,$03,$F1,$03
        .byte   $F2

megaman_eye_pos0_tiles:.byte   $03,$F3,$03,$F4,$03,$F5,$03,$F6,$03,$F7,$03,$F6,$43,$F8,$03,$F9
        .byte   $03,$F8,$43,$F2,$43,$F1,$43,$F0,$43,$F5,$43,$F4,$43,$F3,$43,$E6
        .byte   $03,$6F,$03,$E6,$43,$FA,$03,$FB,$03,$FC,$03,$E6,$03,$6F,$03,$E6
        .byte   $43,$E7,$03,$E8,$03,$E7,$43,$E6,$03,$6F,$03,$E6,$43,$FC,$43,$FB
        .byte   $43,$FA,$43,$E6,$03,$6F,$03,$E6,$43,$FD,$03,$FE,$03,$FF,$03,$E6
        .byte   $03,$6F,$03,$E6,$43,$80,$03,$81,$03,$80,$43,$E6

megaman_eye_pos8_tiles:.byte   $03,$6F,$03,$E6,$43,$FF,$43,$FE,$43,$FD,$43,$06,$05,$00,$03,$FF
        .byte   $04,$02,$01,$07,$0A,$FF,$08,$FF,$FF,$FF,$09,$FF,$0B,$18,$18,$18
        .byte   $58,$58,$58,$98,$98,$98,$17,$67,$B7,$17,$67,$B7,$17,$67,$B7,$00
        .byte   $00,$26,$26,$00,$2A,$00,$2A,$00,$01,$FF,$00,$03,$00,$00,$00,$FD
        .byte   $0F,$20,$26,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
        .byte   $0F,$30,$3C,$11,$0F,$16,$26,$27,$0F,$01,$2C,$11,$0F,$30,$37,$26
        .byte   $0F,$20,$21,$11,$0F,$37,$17,$15,$0F,$20,$15,$05,$0F,$20,$10,$21
        .byte   $7C,$7E,$38,$39,$36,$25,$62,$6C,$76,$62,$6C,$76,$62,$6C,$76,$20
        .byte   $20,$20,$21,$21,$21,$22,$22,$22,$00,$00,$07,$45,$E0,$E1,$E2,$E2
        .byte   $E3,$E4,$4B,$00,$21,$05

center_portrait_border_row1_data:.byte   $D5,$24,$24,$24,$24,$D6,$00,$41,$05,$E5,$24,$24,$24,$24,$E6,$00
        .byte   $61,$05,$E5,$24,$24,$24,$24,$E6,$00,$81,$05,$F5,$24,$24,$24,$24
        .byte   $F6,$00,$A0,$07,$55,$F0,$F1,$F2,$F2,$F3,$F4,$5B,$FF,$C8,$C9,$CA
        .byte   $CB,$D8,$D9,$DA,$DB,$E8,$E9,$EA,$EB,$F8,$F9,$FA,$FB,$09,$00,$0E
        .byte   $00,$00,$00,$29,$00,$2E,$21,$8E,$CC,$CD,$CE,$CF,$DC,$DD,$DE,$DF
        .byte   $EC,$ED,$EE,$EF,$FC,$FD,$FE,$FF,$23,$DB,$01,$88,$22,$20,$20,$20
        .byte   $21,$21,$21,$22,$22,$22,$84,$8E,$98,$84,$8E,$98,$84,$8E,$98,$78
        .byte   $54,$10,$28,$98,$40,$18,$64,$00,$07,$03,$01,$05,$08,$04,$03,$04
        .byte   $03,$40,$20,$01,$08,$00,$10,$04,$02,$80,$63,$83

stage_lookup_table_data:.byte   $02,$7E,$6B,$82,$03,$76,$6B,$84,$02,$7E,$6B,$85,$02,$86,$6B

cursor_y_position_table:.byte   $86,$01,$8E,$6B,$87,$01,$96,$73,$88,$02,$86,$73,$89,$01,$8E,$73
        .byte   $8A,$01,$96,$0F,$30,$15,$11,$0F,$0F,$27,$17,$0F,$10,$27,$17,$0F
        .byte   $37,$3C,$0F,$0F,$37,$27,$06,$0F,$07,$08,$09

cursor_bolt_y_offset_table:.byte   $0F,$39,$29,$19,$0F,$21,$08,$30,$0F,$0F,$2C,$11,$0F,$0F,$30,$37
        .byte   $0F,$0F,$10,$15,$0F,$0F,$00,$00

cursor_bolt_x_offset_table:.byte   $20,$40


cursor_direction_offset_table:.byte   $00,$00,$20,$00,$81,$00,$00,$10,$20,$10,$00

intro_palette_table:.byte   $00,$00,$00,$00

intro_chr_bank_table:.byte   $00,$00,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00

portrait_addr_lo_table:.byte   $00,$00,$00,$00

portrait_addr_hi_table:.byte   $00,$00,$00,$02,$00,$00,$00,$00,$04,$00,$40,$00,$00,$00,$00,$20
        .byte   $04,$80,$00,$00,$00,$00,$00,$04,$00,$42,$01,$00,$00,$40,$00,$02
        .byte   $00,$00,$00,$00,$00,$80,$00,$02,$05,$00,$00,$00,$00,$40,$00,$00

cursor_x_position_table:.byte   $10,$80,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $10,$00

portrait_frame_tile_table:.byte   $00,$00,$00,$00,$00,$40,$10,$01,$48

face_tile_row0_table:.byte   $40,$04,$00,$00,$40,$10,$01,$00

face_tile_row1_table:.byte   $00,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00
        .byte   $00,$10,$00

face_tile_row2_table:.byte   $00,$30,$00

face_tile_row3_table:.byte   $20,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$40,$00
        .byte   $01,$00,$00,$00,$01,$00,$80,$00,$00,$00,$02,$04,$00,$00,$00,$00
        .byte   $08,$00,$01,$00,$00,$01,$04,$00,$0A,$00,$00,$01,$20,$00,$20,$00
        .byte   $00,$00,$00,$00,$01,$00,$00,$10,$00

face_addr_hi_table:.byte   $00,$00,$00,$00,$04,$10,$00

face_addr_lo_table:.byte   $10,$00

face_attr_offset_table:.byte   $00,$00,$00,$00,$00,$00,$00,$00,$10,$40,$81,$00,$10,$40,$00,$00
        .byte   $00,$10,$00,$00,$10,$00,$00,$00,$00,$00,$04,$00,$01,$00,$00,$00
        .byte   $08,$10,$00,$00,$00,$04,$10,$00

center_portrait_tile_row3:.byte   $00,$00,$00,$00,$0C,$00,$20,$00,$00,$00,$02,$10,$00

center_portrait_addr_ppu:.byte   $00,$22,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$00,$01,$00,$00
        .byte   $00,$00,$00,$00,$00

center_attr_ppu_addr:.byte   $10,$00

center_attr_table_data:.byte   $20,$01

portrait_oam_offset_table:.byte   $00,$00,$50,$00

portrait_sprite_count_table:.byte   $01,$00,$80,$00,$00,$04,$08,$00,$08,$10,$00

boss_bitmask_table:.byte   $00,$08,$00,$00,$00,$00,$04,$02,$00,$00,$00,$80,$00,$00,$00,$00
        .byte   $00,$1C,$00,$00,$05,$00,$00,$00,$00,$C4,$00,$00,$00,$00,$00,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$20,$00,$00
        .byte   $04,$05,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$40,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$00

wily_center_sprite_table:.byte   $00,$00,$00,$02,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$00,$20,$00,$00,$00,$49
        .byte   $F1
