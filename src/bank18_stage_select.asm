; =============================================================================
; MEGA MAN 3 (U) — BANK $18 — STAGE SELECT + PROTO MAN SCENES
; =============================================================================
; Mapped to $A000-$BFFF. Contains stage select screen logic (grid layout,
; cursor movement, portrait rendering, palette flash), robot master intro
; transitions, Proto Man encounter scenes, and Wily gate sequences.
;
; Major sections:
;   $A000-$AFFF  Compressed nametable/palette data (stage select screen layout)
;   $9000-$9008  Jump table: robot_master_intro, password_screen, wily_gate_entry
;   $9009-$90B3  Title screen → stage select transition (CHR bank load, palette init)
;   $90B4-$90CF  Title screen Start button wait loop
;   $90D0-$92FF  Stage select screen (CHR/palette init, cursor, d-pad, bolt sprites)
;   $9300-$938A  Stage select confirmation → bank03 boss intro handoff
;   $938B-$93E8  write_ppu_data_from_bank03 helper
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

L0000           := $0000
L0400           := $0400
L0515           := $0515
L0801           := $0801
L0F06           := $0F06
L0F10           := $0F10
L0F21           := $0F21
L1121           := $1121
L1137           := $1137
L1626           := $1626
L2020           := $2020
L2076           := $2076
L2110           := $2110
L2120           := $2120
L2937           := $2937
L6276           := $6276
L8004           := $8004
L8100           := $8100
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
LC9B3           := $C9B3
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
        jmp     robot_master_intro                   ; → robot_master_intro

        jmp     password_screen_entry                   ; → password_screen_entry

        jmp     wily_gate_setup                   ; → wily_gate_entry

; ===========================================================================
; Title screen → stage select transition ($9009)
; ===========================================================================
; Loads CHR banks for the title screen, writes nametable data for
; title/credits screens, waits for palette fade, then loads bank $13
; metatile data to render the stage select background. After all 64
; metatile columns are rendered, enables PPU and waits for Start press.
; ===========================================================================
        jsr     fade_palette_in         ; disable rendering
        jsr     task_yield
        jsr     rendering_off                   ; rendering_off
        ldy     #$05
; --- Load CHR banks from $9BF7 table ---
code_9014:  lda     $9BF7,y                 ; 6 CHR bank IDs
        sta     $E8,y
        dey
        bpl     code_9014
        jsr     select_CHR_banks        ; apply CHR bank selection
        lda     #$20                    ; nametable $2000
        ldx     #$24                    ; fill tile $24 (blank)
        ldy     #$00
        jsr     fill_nametable                   ; fill_nametable
        jsr     rendering_on                   ; rendering_on
; --- Copy palette working copy from $9C03 ---
        ldy     #$1F
code_902E:  lda     $9C03,y
        sta     $0620,y                 ; palette working copy
        dey
        bpl     code_902E
; --- Write title screen nametable data (PPU tables $10/$11 from bank03) ---
        lda     #$00
        sta     $10
        ldx     #$10                    ; table index $10
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     task_yield
        ldx     #$11                    ; table index $11
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     task_yield
        jsr     fade_palette_out        ; show title screen
        ldx     #$B4                    ; wait 180 frames (3 seconds)
        jsr     task_yield_x
; --- Second phase: load stage select background via metatiles ---
        jsr     fade_palette_in         ; fade out
        jsr     task_yield
        jsr     rendering_off                   ; rendering_off
        ldy     #$05
code_905E:  lda     $9BF7,y                 ; reload CHR banks
        sta     $E8,y
        dey
        bpl     code_905E
        jsr     select_CHR_banks
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank
        jsr     select_PRG_banks
        lda     #$00
        sta     $28                     ; metatile column counter
; --- Render 64 metatile columns (stage select background) ---
code_9075:  ldy     #$00
        sty     $10
        jsr     queue_metatile_update                   ; queue_metatile_update
        jsr     drain_ppu_buffer                   ; drain_ppu_buffer
        jsr     task_yield
        inc     $28
        lda     $28
        and     #$3F                    ; 64 columns
        bne     code_9075
; --- Reload palettes and enable rendering ---
        ldy     #$1F
code_908C:  lda     $9C03,y
        sta     $0620,y
        dey
        bpl     code_908C
        jsr     task_yield
        jsr     rendering_on                   ; rendering_on
; --- Set up initial OAM sprite (password cursor sentinel) ---
        ldx     #$03
code_909D:  lda     $9C69,x                 ; initial OAM data (4 bytes)
        sta     $0200,x                 ; sprite 0: Y=$97, tile=$ED, attr=$01, X=$28
        dex
        bpl     code_909D
        lda     #$00
        sta     nmi_skip
        lda     #$13
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     fade_palette_out        ; show stage select background

; ===========================================================================
; Title screen Start button wait loop ($90B4)
; ===========================================================================
; Waits for Start button press. D-pad Up/Down moves password cursor
; (OAM sprite 0 Y position: $B7 = top, $C7 = bottom).
; Start press → jump to stage_select_init.
; ===========================================================================
code_90B4:
        lda     joy1_press
        and     #BTN_START
        bne     stage_select_init       ; Start → enter stage select
        lda     joy1_press
        and     #$0C                    ; Up ($08) / Down ($04)
        beq     code_90CA
        lsr     a
        lsr     a
        lsr     a                       ; $08→$01, $04→$00
        tay
        lda     $9BFF,y                 ; Y position lookup: [$00]=$30, [$01]=$40
        sta     $0200                   ; update sprite 0 Y position
code_90CA:  jsr     task_yield
        jmp     title_screen_wait_start

; --- stage_select_init ($1890D0) ---
; Entry point for the stage select screen. Sets up CHR banks, loads palettes,
; and determines if this is a fresh start or return from a stage.
;
; Palette system:
;   $0600-$060F = BG palette buffer (4 sub-palettes × 4 colors)
;   $0610-$061F = Sprite palette buffer (4 sub-palettes × 4 colors)
;   $0620-$062F = BG palette working copy
;   $18 = palette update flag (nonzero → NMI uploads $0600-$061F to PPU $3F00-$3F1F)
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
        sta     $EC
        lda     #$34                    ; CHR bank $34 for BG tiles
        sta     $ED
        jsr     select_CHR_banks                   ; apply CHR bank selection

; Load sprite palettes from $9C23 ($9C23)
        ldy     #$0F                    ; counter = 16 bytes
sprite_palette_load_loop:  lda     $9C23,y                 ; load sprite palette color
        sta     $0610,y                 ; store to sprite palette buffer
        dey
        bpl     sprite_palette_load_loop                   ; loop Y=$0F down to $00
        sty     palette_dirty                     ; Y=$FF, trigger palette update

; Determine fresh start vs return: check if OAM is initialized
        ldy     #$00                    ; assume fresh start (Y=0)
        lda     $0200                   ; check OAM sprite 0 Y position
        cmp     #$97                    ; $97 = sentinel for uninitialized
        beq     stage_select_init_fresh_vs_return                   ; fresh start → Y stays 0
        iny                             ; returning from stage → Y=1
stage_select_init_fresh_vs_return:  sty     $0F                     ; $0F = 0 (fresh) or 1 (return)
        lda     $9BFD,y                 ; select music track
        jsr     submit_sound_ID_D9                   ; play stage select music
        lda     #$00
        sta     $70                     ; clear NMI sync flag
        lda     $9C63,y                 ; load screen scroll/setup param
        jsr     metatile_column_ptr_by_id
        lda     #$04
        sta     oam_ptr
        jsr     prepare_oam_buffer                   ; clear unused OAM sprites
stage_select_nametable_fill_wait_loop:  lda     #$04
        sta     $10
        jsr     fill_nametable_progressive
        jsr     task_yield                   ; wait for NMI
        lda     $70
        bne     stage_select_nametable_fill_wait_loop                   ; loop until NMI complete

; Load BG palettes — offset depends on fresh start vs return
        ldy     $0F                     ; Y = 0 (fresh) or 1 (return)
        beq     stage_select_defeated_portrait_restore                   ; skip portrait restore if fresh
        lda     #$04
        sta     $10
        ldx     #$00
        jsr     write_ppu_data_from_table                   ; restore defeated boss portraits
stage_select_defeated_portrait_restore:  lda     $9C65,y                 ; load scroll params
        sta     $10
        lda     $9C67,y
        sta     $11
        jsr     rendering_off                   ; disable PPU rendering
        ldx     $9C01,y                 ; X = offset into $9C03
        ldy     #$00
bg_palette_load_loop:  lda     $9C03,x                 ; load BG palette color
        sta     $0600,y                 ; store to BG palette buffer
        sta     $0620,y                 ; store to working copy
        inx
        iny
        cpy     #$10                    ; 16 bytes = 4 palettes × 4 colors
        bne     bg_palette_load_loop
        sty     palette_dirty                     ; Y=$10, trigger palette update
        lda     #$20
        ldx     #$24
        ldy     #$00
        jsr     fill_nametable                   ; fill_nametable
        jsr     rendering_on                   ; rendering_on

; ===========================================================================
; Horizontal scroll transition ($9155)
; ===========================================================================
; Smoothly scrolls the screen horizontally to reveal the stage select grid.
; $10/$11 = scroll speed (low/high). Scroll increments $FC/$FD until
; $FC wraps to 0, then fills the offscreen nametable and swaps.
; ===========================================================================
code_9155:
        lda     #$F8                    ; hide sprite 0 (offscreen Y)
        sta     $0200
        inc     nmi_skip
        lda     #$58                    ; scroll split Y position
        sta     $5E
        lda     #$07                    ; game mode $07 = stage select
        sta     game_mode
; --- Scroll loop: increment camera X until low byte wraps ---
code_9164:  lda     camera_x_lo
        clc
        adc     $10                     ; add scroll speed low
        sta     camera_x_lo
        lda     camera_x_hi
        adc     $11                     ; add scroll speed high
        and     #$01                    ; keep nametable bit
        sta     camera_x_hi
        lda     #$00
        sta     nmi_skip                ; allow NMI
        jsr     task_yield
        inc     nmi_skip
        lda     camera_x_lo
        bne     code_9164               ; loop until aligned
; --- Check if we need to fill the offscreen nametable ---
        ldy     #$10                    ; palette offset for $9C33
        lda     $11
        bmi     code_91C8               ; negative speed → skip fill
; --- Fill offscreen nametable ---
        lda     camera_x_hi
        eor     #$01                    ; opposite nametable
        asl     a
        asl     a                       ; 0 or 4
        sta     $10
        lda     #$01                    ; metatile column 1
        jsr     metatile_column_ptr_by_id                   ; metatile_column_ptr_by_id
        lda     #$00
        sta     $70
        sta     nmi_skip
code_9199:  lda     $10
        pha
        jsr     fill_nametable_progressive                   ; fill_nametable_progressive
        pla
        sta     $10
        jsr     task_yield
        lda     $70
        bne     code_9199               ; loop until complete
; --- Write defeated portraits and PPU data ---
        jsr     draw_defeated_portraits                   ; blank_defeated_portraits
        ldx     #$03                    ; PPU data table $03
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     task_yield
        ldx     #$04                    ; PPU data table $04
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
; --- Swap displayed nametable ---
        lda     camera_x_hi
        eor     #$01
        sta     camera_x_hi
        ldy     #$00
        lda     #$7E
        sta     $E9
        jsr     update_CHR_banks
; --- Load BG palette from $9C33 table ---
; Y=0 for fresh start palettes, Y=$10 for return palettes.
code_91C8:  ldx     #$00
code_91CA:  lda     $9C33,y             ; BG palette color
        sta     $0600,x                 ; store to palette buffer
        iny
        inx
        cpx     #$10                    ; 16 bytes
        bne     code_91CA
        lda     #$FF
        sta     palette_dirty           ; flag palette upload
; --- Initialize cursor at center position ---
        lda     #$01                    ; cursor column = 1 (center)
        sta     $12
        lda     #$03                    ; cursor row offset = 3 (middle)
        sta     $13
; --- Branch based on scroll direction ---
        lda     $11
        bmi     code_91EC               ; negative → password screen mode
        jsr     load_stage_select_oam                   ; load_stage_select_oam
        jmp     stage_select_input_loop                   ; → stage select main loop

; --- Password screen mode (negative scroll speed) ---
; Shows "Game Start / Stage Select" cursor, waits for Start.
code_91EC:  jsr     password_cursor_oam_setup               ; init_password_cursor
code_91EF:  jsr     proto_man_sprite_control               ; update_password_cursor
        lda     #$00
        sta     nmi_skip
        jsr     task_yield
        inc     nmi_skip
        lda     joy1_press
        and     #BTN_START
        beq     code_91EF               ; loop until Start pressed
; --- Process password cursor selection ---
        lda     $0200                   ; cursor Y position
        cmp     #$B7                    ; $B7 = "Game Start" row
        beq     code_9212               ; → Game Start (clear OAM, re-enter)
; --- Stage Select chosen: jump to bank03 $A593 ---
        lda     #$03
        sta     prg_bank
        jsr     select_PRG_banks
        jmp     bank03_stage_select_entry                   ; → bank03 stage select entry

; --- Game Start: clear all OAM sprites except sprite 0 ---
code_9212:  ldy     #$04                ; start at sprite 1
        lda     #$F8                    ; Y=$F8 = offscreen
code_9216:  sta     $0200,y             ; clear OAM Y to offscreen
        iny
        iny
        iny
        iny                             ; next sprite (4 bytes)
        bne     code_9216               ; loop all 63 remaining sprites
; --- Play stage select music and rebuild nametable ---
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank
        jsr     select_PRG_banks
        lda     #MUSIC_STAGE_SELECT                    ; music $10 = stage select theme
        jsr     submit_sound_ID_D9
        lda     #$00
        sta     $70                     ; nametable fill counter
        sta     $28
        lda     #$04                    ; metatile column 4
        jsr     metatile_column_ptr_by_id                   ; metatile_column_ptr_by_id
code_9236:  lda     #$00
        sta     nmi_skip
        sta     $10
        jsr     fill_nametable_progressive                   ; fill_nametable_progressive
        jsr     task_yield
        lda     $70
        bne     code_9236               ; loop until complete
; --- Set scroll speed and CHR, then scroll into stage select ---
        lda     #$04                    ; scroll speed low = 4
        sta     $10
        lda     #$00                    ; scroll speed high = 0
        sta     $11
        lda     #$7C                    ; update CHR bank
        sta     $E8
        jsr     update_CHR_banks
        jmp     stage_select_cursor_init                   ; → horizontal scroll transition
code_9258:

        lda     joy1_press
        and     #$90
        beq     code_9261
        jmp     stage_select_confirm

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

code_9261:  lda     joy1_press                 ; new button presses
        and     #$03                    ; $02=Left, $01=Right
        beq     code_9279               ; no horizontal input → skip
        tay
        lda     $12                     ; current column
        clc
        adc     $9D0D,y                 ; add direction offset from table
        cmp     #$03                    ; clamp to 0-2
        bcs     code_9279               ; out of range → ignore
        sta     $12                     ; update column
        lda     #SFX_CURSOR                    ; SFX $1B = cursor move
        jsr     submit_sound_ID
code_9279:  lda     joy1_press                 ; new button presses
        and     #$0C                    ; $08=Up, $04=Down
        beq     code_9291               ; no vertical input → skip
        tay
        lda     $13                     ; current row offset
        clc
        adc     $9D0D,y                 ; add direction offset
        cmp     #$07                    ; clamp to 0-6 (rows 0/3/6)
        bcs     code_9291               ; out of range → ignore
        sta     $13                     ; update row
        lda     #SFX_CURSOR                    ; SFX $1B = cursor move
        jsr     submit_sound_ID
code_9291:  lda     $12                 ; combine column + row
        clc
        adc     $13
        sta     L0000
        asl     a
        asl     a
        sta     $01
        asl     a

; --- Cursor sprite rendering ---
; Writes 6 cursor selector sprites + 4 cursor bolt sprites to OAM.
; Grid index = $12 + $13 = $00 (0-8).
        adc     $01                     ; ×12 = OAM data offset for grid pos
        tax
        ldy     #$00
cursor_selector_sprite_load_loop:  lda     $9C75,x                 ; cursor selector OAM data (12 bytes/pos)
        sta     $0299,y                 ; → OAM sprites at $0298+
        lda     $9C76,x
        sta     $029A,y
        inx
        inx
        iny
        iny
        iny
        iny                             ; Y += 4 (OAM entry size)
        cpy     #$18                    ; 6 sprites × 4 bytes = $18
        bne     cursor_selector_sprite_load_loop

; --- Cursor bolt sprites (4 corner bolts) ---
; Position from $9CF3/$9CFC tables, offsets from $9D05/$9D09.
; Bolt tile alternates $E4/$E5 every 8 frames for flash effect.
; NOTE: OAM attribute byte is never written → defaults to $00 = sprite palette 0.
; SP 0: $0F(transparent), $30(white), $15(dk_magenta), $11(dk_blue).
; Tile $E4 uses color 2 ($15 magenta fill), tile $E5 uses color 3 ($11 blue fill).
; Both tiles have color 1 ($30 white) as upper-left highlight.
; Solid filled shape (not outline) — same pattern as center portrait bolt sprites.
        ldy     L0000                   ; grid index
        lda     $9CF3,y                 ; cursor Y base (per grid position)
        sta     L0000
        lda     $9CFC,y                 ; cursor X base (per grid position)
        sta     $01
        lda     $95                     ; frame counter
        lsr     a
        lsr     a
        lsr     a                       ; ÷8
        and     #$01                    ; alternates 0/1
        clc
        adc     #$E4                    ; tile $E4 or $E5 (bolt flash)
        sta     $02
        ldx     #$03                    ; 4 bolts (3→0)
        ldy     #$C8                    ; OAM offset $C8 (sprite 50-53)
cursor_bolt_sprite_load_loop:  lda     L0000                   ; Y base
        clc
        adc     $9D05,x                 ; + Y offset (0 or 38 for top/bottom)
        sta     $0200,y                 ; → OAM Y
        lda     $01                     ; X base
        clc
        adc     $9D09,x                 ; + X offset (0 or 42 for left/right)
        sta     $0203,y                 ; → OAM X
        lda     $02                     ; bolt tile ($E4/$E5)
        sta     $0201,y                 ; → OAM tile
        dey
        dey
        dey
        dey                             ; Y -= 4 (next OAM slot, backwards)
        dex
        bpl     cursor_bolt_sprite_load_loop
code_92F2:  lda     #$00
        sta     nmi_skip
        jsr     task_yield
        inc     nmi_skip
        inc     $95
        jmp     stage_select_input_loop

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

        jsr     clear_entity_table                   ; clear all entity slots
        lda     $12                     ; $12 = cursor column (0-2)
        clc
        adc     $13                     ; $13 = cursor row offset (0/3/6)
        tay                             ; Y = grid index (0-8)
        cpy     #$04                    ; index 4 = center position
        bne     stage_select_check_beaten_boss                   ; not center → try select
        lda     stage_select_page                     ; center only selectable when
        cmp     #$12                    ; $60 == $12 (all Doc Robots beaten)
        bne     stage_select_check_beaten_boss
        jmp     wily_gate_entry                   ; → Wily fortress entrance

stage_select_check_beaten_boss:  lda     bosses_beaten                     ; $61 = boss-defeated bitmask
        and     $9DED,y                 ; check if this boss already beaten
        bne     code_92F2               ; already beaten → back to select loop
        lda     stage_select_page                     ; $60 = game progression page
        cmp     #$0A                    ; >= $0A = invalid state
        bcs     code_92F2               ; → back to select loop
        tya                             ; Y = grid index (0-8)
        clc
        adc     stage_select_page                     ; + page offset (0=Robot Masters, $0A=Doc Robot)
        tay                             ; Y = adjusted grid index for table lookup

; Stage select lookup table at $9CE1 (9 entries per page):
;   Index: 0     1     2     3     4     5     6     7     8
;   Value: $06   $05   $00   $03   $FF   $04   $02   $01   $07
;   Boss:  Spark Snake Needl Hard  (n/a) Top   Gemin Magnt Shadw
;   Grid:  TL    TM    TR    ML    CTR   MR    BL    BM    BR
        lda     $9CE1,y                 ; look up stage number
        bmi     code_92F2               ; $FF = center (not selectable)
        sta     stage_id                     ; $22 = current stage number
        sty     $0F                     ; $0F = save adjusted grid index

; --- Set up PRG/CHR banks for the boss intro layout ---
        lda     #$13                    ; select PRG bank $13
        sta     prg_bank                     ; (contains boss intro metatile data
        jsr     select_PRG_banks                   ; at $AF00+, referenced by fill routine)
        lda     #$04                    ; set rendering mode
        sta     oam_ptr
        jsr     prepare_oam_buffer                   ; clear unused OAM sprites
        lda     #$76                    ; set CHR bank $76
        sta     $E9                     ; (boss intro screen tileset)
        jsr     update_CHR_banks

; --- Determine which nametable to fill (the offscreen one) ---
; $FD bit 0 = currently displayed nametable. Toggle it so the NEW
; nametable becomes visible, then compute $10 = old nametable's
; PPU address bit (0 or 4) to write the intro layout into it first.
        lda     camera_x_hi                     ; toggle displayed nametable
        pha
        eor     #$01
        sta     camera_x_hi
        pla                             ; $10 = OLD nametable select bit
        and     #$01                    ; bit 0 → shifted left 2 = 0 or 4
        asl     a                       ; 0 = nametable $2000, 4 = nametable $2400
        asl     a
        sta     $10

; --- Set up metatile pointer and fill nametable ---
; metatile_column_ptr_by_id computes: ($20/$21) = $AF00 + (A << 6)
; A=$03 → column 3 of bank $13 level data → pointer $AFC0
        lda     #$03                    ; set metatile column pointer
        jsr     metatile_column_ptr_by_id                   ; ($20/$21) → $AFC0 in bank $13
        lda     #$00                    ; $70 = nametable fill progress (0-63)
        sta     $70                     ; starts at 0
        sta     nmi_skip                     ; $EE = NMI skip flag (0=allow NMI)

; Fill the offscreen nametable progressively.
; fill_nametable_progressive writes 4 tile rows per call.
; 16 calls × 4 rows = 64 rows → full nametable.
; When $70 reaches $40, it writes the attribute table and resets $70 to 0.
boss_intro_nametable_fill_wait_loop:  lda     $10                     ; preserve nametable select
        pha
        jsr     fill_nametable_progressive                   ; write 4 rows to PPU queue
        jsr     task_yield                   ; wait for NMI (PPU uploads queued data)
        pla                             ; restore nametable select
        sta     $10
        lda     $70                     ; $70 = 0 when fill complete
        bne     boss_intro_nametable_fill_wait_loop                   ; loop until done

; --- Load transition palette and jump to bank03 ---
; $9C53: 16-byte palette for the boss intro screen (BG palettes only).
; This replaces the stage select palettes with the intro band colors.
        ldy     #$0F
transition_palette_load_loop:  lda     $9C53,y                 ; copy 16 bytes from $9C53
        sta     $0600,y                 ; to BG palette buffer
        sta     $0620,y                 ; and working copy
        dey
        bpl     transition_palette_load_loop
        sty     palette_dirty                     ; $18 = $FF → flag palette upload

; Jump to bank03 entry point.
; Y = adjusted grid index (used by bank03 to index stage parameter tables).
; This is a JMP (not JSR) — bank03's RTS returns to whoever called the
; stage select, not back here.
        lda     #$03                    ; select PRG bank $03
        sta     prg_bank
        jsr     select_PRG_banks
        ldy     $0F                     ; Y = adjusted grid index
        jmp     data_compressed_nametable_start                   ; → bank03 stage_transition_entry

; --- Unused bank-call helper ($938B) ---
; Dead code (unreferenced): calls bank01 $A000, then restores bank03.
        pha
        lda     #$01
        sta     prg_bank
        jsr     select_PRG_banks
        pla
        jsr     data_compressed_nametable_start
        lda     #$03
        sta     prg_bank
        jmp     select_PRG_banks

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
write_ppu_data_from_bank03:             ; store to sprite palette buffer

        sty     $04                     ; save Y
        stx     $05                     ; save table index
        lda     prg_bank                     ; save current PRG bank
        pha
        lda     #$03                    ; switch to bank 03
        sta     prg_bank                     ; (contains PPU data tables)
        jsr     select_PRG_banks        ; $97 = sentinel for uninitialized
        ldx     $05                     ; X = table index
        lda     ppu_write_table_low_ptr,x                 ; ($02/$03) = pointer to PPU data
        sta     $02                     ; low byte from $A56D+X
        lda     ppu_write_table_high_ptr,x                 ; high byte from $A580+X
        sta     $03                     ; play stage select music
        ldy     #$00
ppu_write_queue_read_addr:  lda     ($02),y                 ; PPU high address
        ora     $10                     ; OR nametable select bit
        sta     $0780,y                 ; store to PPU write queue
        bmi     ppu_write_queue_done                   ; bit 7 set = terminator ($FF)
        iny
        lda     ($02),y                 ; PPU low address
        sta     $0780,y
        iny
        lda     ($02),y                 ; tile count
        sta     $0780,y                 ; wait for NMI
        sta     L0000                   ; save count for loop
        iny                             ; loop until NMI complete
ppu_write_queue_tile_loop:  lda     ($02),y                 ; tile data byte
        sta     $0780,y
        iny                             ; Y = 0 (fresh) or 1 (return)
        dec     L0000                   ; skip portrait restore if fresh
        bpl     ppu_write_queue_tile_loop
        bmi     ppu_write_queue_read_addr                   ; next PPU entry (always branches)
ppu_write_queue_done:  sta     nametable_dirty                     ; $19 = $FF → flag PPU write pending
        pla                             ; restore original PRG bank
        sta     prg_bank                ; load scroll params
        jsr     select_PRG_banks
        ldy     $04                     ; restore Y
        rts
; ---------------------------------------------------------------------------
; init_password_cursor — place password cursor sprite at default position
; ---------------------------------------------------------------------------
; Sets OAM sprite 0 to the password cursor: Y=$B7, tile=$ED, attr=$00, X=$40.
; Y=$B7 = "GAME START" row, Y=$C7 = "STAGE SELECT" row.
; ---------------------------------------------------------------------------
code_93E9:
        lda     #$B7                    ; Y = $B7 (Game Start row)
        sta     $0200                   ; 16 bytes = 4 palettes × 4 colors
        lda     #$ED                    ; tile $ED = cursor arrow
        sta     $0201                   ; Y=$10, trigger palette update
        lda     #$00                    ; attr $00 = sprite palette 0
        sta     $0202
        lda     #$40                    ; X = $40 (column position)
        sta     $0203
        rts

; ---------------------------------------------------------------------------
; update_password_cursor — move cursor between Game Start / Stage Select
; ---------------------------------------------------------------------------
; D-pad Up ($08) → Y=$B7 (Game Start), Down ($04) → Y=$C7 (Stage Select).
; ---------------------------------------------------------------------------
code_93FE:
        lda     joy1_press
        and     #$0C                    ; Up ($08) / Down ($04)
        beq     code_940F               ; no vertical input → skip
        ldy     #$B7                    ; assume Up → Game Start
        and     #$08
        bne     code_940C               ; if Up, use $B7
        ldy     #$C7                    ; Down → Stage Select
code_940C:  sty     $0200               ; update sprite Y position
code_940F:  rts

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
weapon_energy_refill_loop:  lda     player_hp,y                   ; $A2-$AD = weapon ammo
        bpl     weapon_energy_refill_continue                   ; if negative (depleted), set to full
        lda     #$9C                    ; $9C = full energy (28 units)
        sta     player_hp,y
weapon_energy_refill_continue:  dey
        bpl     weapon_energy_refill_loop
        lda     stage_id                     ; stage < $08 = Robot Master
        cmp     #STAGE_DOC_NEEDLE                    ; stage >= $08 = Doc Robot/Wily
        bcc     robot_master_intro_entry
        jmp     stage_loading_entry                   ; → skip to stage loading for Doc/Wily

; --- Robot Master intro animation ---

robot_master_intro_entry:  jsr     fade_palette_in                   ; disable sprites/rendering
        lda     #$04                    ; set rendering mode
        sta     oam_ptr
        jsr     prepare_oam_buffer                   ; configure PPU
        jsr     task_yield                   ; wait 1 frame
        lda     #MUSIC_BOSS_INTRO                    ; play sound $35
        jsr     submit_sound_ID_D9                   ; (boss intro fanfare)
        jsr     reset_stage_state                   ; zero out game state

; Fill nametable 0 with boss intro layout from bank $13.
; Temporarily set $22 = $14 so metatile pointer references the correct
; level section in bank $13 (stage $14 = the boss intro screen layout).
; metatile_column_ptr_by_id with A=$07 → metatile column 7 → pointer $B0C0.
        lda     stage_id                     ; save real stage number
        pha
        lda     #$14                    ; $22 = $14 (boss intro layout ID)
        sta     stage_id
        lda     #$07                    ; metatile column 7
        jsr     metatile_column_ptr_by_id                   ; pointer → $B0C0 in bank $13
robot_master_nametable_fill_wait_loop:  lda     #$00                    ; $10 = 0 → write to nametable $2000
        sta     $10
        jsr     fill_nametable_progressive                   ; write 4 tile rows
        jsr     task_yield                   ; wait for NMI
        lda     $70                     ; $70 = 0 when complete
        bne     robot_master_nametable_fill_wait_loop
        pla                             ; restore real stage number
        sta     stage_id

; Write stage-specific nametable data (boss face, decorations).
; Two calls to write_ppu_data_from_bank03:
;   X=$05: common intro data
;   X=$22+$06: per-boss face data
        ldx     #$05                    ; table index $05 = common data
        lda     #$00                    ; $10 = 0 → nametable $2000
        sta     $10
        jsr     write_ppu_data_from_table                   ; write common intro nametable data
        jsr     task_yield                   ; wait for NMI
        lda     stage_id                     ; X = stage + 6
        clc                             ; (per-boss face data table index)
        adc     #$06
        tax
        jsr     write_ppu_data_from_table                   ; write boss-specific face tiles

; Load CHR bank configuration and palettes for the intro screen.
; $9D46: 6 bytes of CHR bank IDs → $E8-$ED
; $9D16: 32 bytes of palette data (BG + sprite) → $0620 working copy
        ldy     #$05
intro_chr_bank_load_loop:  lda     $9D46,y
        sta     $E8,y
        dey
        bpl     intro_chr_bank_load_loop
        jsr     update_CHR_banks
        ldy     #$1F
intro_palette_load_loop:  lda     $9D16,y                 ; 32 bytes: BG ($0620-$062F) + sprite ($0630-$063F)
        sta     $0620,y
        dey
        bpl     intro_palette_load_loop

; --- Set up boss entity and begin drop animation ---
; Entity slot 0: the boss sprite (reused as the intro display entity).
; ent_x_px = entity 0 X position (player X during gameplay)
; ent_y_px = entity 0 Y position (player Y during gameplay)
        lda     #$80                    ; entity 0 X = $80 (128 = centered)
        sta     ent_x_px
        lda     #$E8                    ; entity 0 Y = $E8 (232 = below screen)
        sta     ent_y_px
        ldx     #$00                    ; set animation to $B0
        lda     #$B0                    ; (boss intro drop animation)
        jsr     reset_sprite_anim
        lda     ent_flags                   ; clear bit 6 = facing left
        and     #$BF
        sta     ent_flags
        jsr     task_yield                   ; wait 1 frame
        jsr     fade_palette_out                   ; enable rendering

; Boss drop loop: decrement Y from $E8 to $74 at 4px/frame.
; ($E8 - $74) / 4 = 29 frames for the boss to slide down.
; When anim phase (ent_anim_state) reaches $02, switch to idle anim $1A.
boss_drop_animation_loop:  lda     ent_y_px                   ; if Y == $74, skip decrement
        cmp     #$74                    ; (target reached)
        beq     boss_drop_anim_phase_check
        sec                             ; Y -= 4
        sbc     #$04
        sta     ent_y_px
        lda     #$00                    ; reset anim frame counter
        sta     ent_anim_frame                   ; (keep animation progressing)
boss_drop_anim_phase_check:  lda     ent_anim_state                   ; check animation phase
        cmp     #$02                    ; phase 2 = intro anim done
        bne     boss_drop_frame_process_loop
        ldx     #$00                    ; switch to idle animation $1A
        lda     #$1A
        jsr     reset_sprite_anim
boss_drop_frame_process_loop:  jsr     process_frame_yield_full                   ; process sprites + wait for NMI
        lda     ent_anim_id                   ; check current OAM ID
        cmp     #$1A                    ; $1A = idle pose active
        bne     boss_drop_animation_loop                   ; loop until idle
        ldx     #$3C                    ; wait $3C (60) frames
        jsr     task_yield_x                   ; (boss stands idle)

; --- Mega Man teleport-in animation ---
; Mega Man rises from Y=$80 to Y=$C0, 2px/frame = 32 frames.
; (Teleporting from below the blue band upward into view.)
        lda     ent_x_px                   ; if X == $C0, done
        cmp     #$C0
        beq     boss_idle_wait_before_fadeout
        clc                             ; X += 2
        adc     #$02                    ; (note: using ent_x_px which is
        sta     ent_x_px                   ; the X position for the entity)
        jsr     process_frame_yield_full                   ; process sprites + wait for NMI
        jmp     robot_master_intro_loop

; --- Palette fade to black ---
; Fade all 3 BG palette groups to black ($0F).
; Each call to fade_palette_to_black subtracts $10 per step
; from palette colors, 4 frames per step, until all reach $0F.

boss_idle_wait_before_fadeout:  ldx     #$3C                    ; wait $3C (60) frames
        jsr     task_yield_x
        lda     #$00                    ; clear NMI skip flag
        sta     nmi_skip
        ldy     #$03                    ; fade BG palette 0 (bytes $00-$03)
        jsr     boss_energy_fill_loop
        ldy     #$07                    ; fade BG palette 1 (bytes $04-$07)
        jsr     boss_energy_fill_loop
        ldy     #$0B                    ; fade BG palette 2 (bytes $08-$0B)
        jsr     boss_energy_fill_loop
        ldx     #$B4                    ; wait $B4 (180) frames
        jsr     task_yield_x                   ; (3 seconds on black screen)
        jmp     stage_loading_entry                   ; → stage loading

; ---------------------------------------------------------------------------
; boss_face_palette_flash — alternates boss/default sprite palettes
; ---------------------------------------------------------------------------
; Called after the last palette fade (Y=$07 → code_189509 via BEQ).
; Flashes 17 times ($10 counts $10 down to $00), 2 frames per flash.
; Even frames: load boss-specific face colors from $9BB7 table.
; Odd frames: restore default palette from $0630 working copy.
; ---------------------------------------------------------------------------

boss_face_palette_flash:  lda     #$10  ; $10 = flash counter (17 iterations)
        sta     $10
boss_face_flash_frame_loop:  ldy     #$03                    ; process 4 palette colors (Y=3..0)
        lda     $10                     ; even/odd toggle
        and     #$01
        bne     boss_face_default_palette_restore_loop                   ; odd → restore defaults

; Even frames: load boss face colors from $9BB7 table.
; $9BB7 + stage*8 = 8-byte palette (bright + dark variant).
; Colors 0-3 from $9BB7 → SP 0 ($0610), colors 4-7 from $9BBB → SP 1 ($0618).
        lda     stage_id                     ; X = stage * 8 + 3
        asl     a
        asl     a
        asl     a
        ora     #$03
        tax
boss_face_bright_palette_load_loop:  lda     $9BB7,x                 ; bright variant → SP 0
        sta     $0610,y
        lda     $9BBB,x                 ; dark variant → SP 1
        sta     $0618,y                 ; new button presses
        dex                             ; $02=Left, $01=Right
        dey                             ; no horizontal input → skip
        bne     boss_face_bright_palette_load_loop
        beq     boss_face_flash_complete_check                   ; (always branches)

; Odd frames: copy default palettes back from $0630/$0638 working copy.
boss_face_default_palette_restore_loop:  lda     $0630,y                 ; default SP 0
        sta     $0610,y                 ; out of range → ignore
        lda     $0638,y                 ; default SP 1
        sta     $0618,y                 ; SFX $1B = cursor move
        dey
        bne     boss_face_default_palette_restore_loop ; new button presses
boss_face_flash_complete_check:  inc     palette_dirty                     ; flag palette upload
        ldx     #$02                    ; wait 2 frames per flash
        jsr     task_yield_x
        dec     $10                     ; decrement flash counter
        bpl     boss_face_flash_frame_loop
        rts                             ; add direction offset

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
palette_fade_black_color_loop:  ldy     $11                     ; restore palette index
        ldx     #$03                    ; 4 colors per palette group
palette_fade_subtract_loop:  lda     $0600,y                 ; load base palette color
        sec
        sbc     $10                     ; subtract fade amount
        bcs     palette_fade_store_color                   ; if no underflow, use result
        lda     #$0F                    ; clamp to black ($0F)
palette_fade_store_color:  sta     $0604,y                 ; store to working palette
        dey
        dex
        bpl     palette_fade_subtract_loop
        sty     palette_dirty                     ; flag palette upload
        ldx     #$04                    ; wait 4 frames per step
        jsr     task_yield_x            ; Y += 4 (OAM entry size)
        lda     $10                     ; $10 -= $10 (next darker step)
        sec
        sbc     #$10
        sta     $10
        bcs     palette_fade_black_color_loop                   ; loop while $10 >= 0
        lda     $11                     ; if this was BG palette 1 (Y=$07),
        cmp     #$07                    ; chain into boss face flash
        beq     boss_face_palette_flash
        ldx     #$1E                    ; wait $1E (30) frames between groups
        jsr     task_yield_x
        rts
; ===========================================================================
; Stage loading dispatcher ($9581)
; ===========================================================================
; Called after robot master intro or Doc Robot stage. Checks if all 8 Robot
; Masters are beaten ($61=$FF) to trigger Doc Robot stage select setup.
; Otherwise falls through to normal stage return at code_968C.
; ===========================================================================
code_9581:
        lda     bosses_beaten           ; ÷8
        cmp     #$FF                    ; all 8 Robot Masters beaten?
        beq     code_958A               ; yes → set up Doc Robot stage select
        jmp     normal_stage_return                   ; no → normal return to stage select

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
code_958A:  jsr     fade_palette_in     ; disable rendering
        lda     #$04
        sta     oam_ptr
        jsr     prepare_oam_buffer      ; Y -= 4 (next OAM slot, backwards)
        jsr     task_yield
        jsr     reset_stage_state                   ; reset_stage_state
        lda     #MUSIC_STAGE_SELECT                    ; music $10 = stage select theme
        jsr     submit_sound_ID_D9
        lda     #$13                    ; bank $13 = metatile data
        sta     prg_bank
        jsr     select_PRG_banks
; --- Fill nametable 0 with stage select layout ---
        lda     #$01                    ; metatile column 1
        jsr     metatile_column_ptr_by_id                   ; metatile_column_ptr_by_id
        lda     #$00
        sta     $70
code_95AF:  lda     #$00
        sta     $10                     ; nametable $2000
        jsr     fill_nametable_progressive                   ; fill_nametable_progressive
        jsr     task_yield
        lda     $70
        bne     code_95AF               ; loop until complete
; --- Blank defeated boss portraits and write nametable data ---
        lda     #$00
        sta     $10
        jsr     draw_defeated_portraits                   ; blank defeated portraits
        lda     #$00
        sta     $10
        ldx     #$03                    ; PPU data table $03
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     task_yield
        ldx     #$04                    ; PPU data table $04
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     task_yield              ; $12 = cursor column (0-2)
; --- Fill nametable 1 (offscreen) ---
        lda     #$04                    ; metatile column 4
        jsr     metatile_column_ptr_by_id                   ; metatile_column_ptr_by_id
        lda     #$00                    ; index 4 = center position
        sta     $70                     ; not center → try select
code_95E1:  lda     #$04                ; center only selectable when
        sta     $10                     ; nametable $2400
        jsr     fill_nametable_progressive                   ; fill_nametable_progressive
        jsr     task_yield              ; → Wily fortress entrance
        lda     $70
        bne     code_95E1               ; loop until complete
; --- Load stage select CHR banks and palettes ---
        lda     #$7C                    ; already beaten → back to select loop
        sta     $E8                     ; $60 = game progression page
        lda     #$7E                    ; >= $0A = invalid state
        sta     $E9                     ; → back to select loop
        lda     #$36                    ; sprite CHR bank
        sta     $EC
        lda     #$34                    ; BG CHR bank
        sta     $ED                     ; Y = adjusted grid index for table lookup
        jsr     update_CHR_banks
        ldy     #$0F
code_9604:  lda     $9C33,y             ; load BG palettes (fresh start set)
        sta     $0620,y
        dey
        bpl     code_9604
        ldy     #$0F                    ; look up stage number
code_960F:  lda     $9C23,y             ; load sprite palettes
        sta     $0630,y                 ; $22 = current stage number
        dey                             ; $0F = save adjusted grid index
        bpl     code_960F
        jsr     task_yield
        jsr     load_stage_select_oam                   ; load OAM sprites from bank03
; --- Enable rendering and enter stage select ---
        lda     #$00                    ; at $AF00+, referenced by fill routine)
        sta     palette_dirty           ; set rendering mode
        jsr     task_yield
        lda     #$58                    ; clear unused OAM sprites
        sta     $5E                     ; scroll split position
        lda     #$07                    ; (boss intro screen tileset)
        sta     game_mode               ; game mode $07 = stage select
        jsr     task_yield
        jsr     fade_palette_out        ; enable rendering
; --- Check if first time or returning from Doc Robot ---
        lda     stage_select_page
        beq     code_964D               ; $60=0 → first time all beaten
; --- Returning from Doc Robot stage: set $60=$12 (all Doc Robots done) ---
        lda     #$12
        sta     stage_select_page
        ldy     #$00
        sty     $10                     ; $10 = OLD nametable select bit
        ldx     #$19                    ; frame delay
        jsr     write_portrait_frames                   ; write_portrait_frames
        jsr     write_center_portrait                   ; write_center_portrait
        jsr     load_wily_center_face                   ; load Wily center face OAM sprites
        jmp     boss_intro_sprite_loop                   ; → enter stage select loop

; --- First time all Robot Masters beaten → init Doc Robot phase ---
code_964D:  ldx     #$F0                ; wait 240 frames (4 seconds)
        jsr     task_yield_x            ; set metatile column pointer
        lda     #$3A                    ; Doc Robot defeated mask (initial)
        sta     bosses_beaten           ; $70 = nametable fill progress (0-63)
        lda     #$09                    ; $60=$09 = Doc Robot page
        sta     stage_select_page       ; $EE = NMI skip flag (0=allow NMI)
        lda     #$74                    ; Doc Robot CHR bank
        sta     $E9
        jsr     update_CHR_banks
; --- Load Doc Robot palettes from $9D36 ---
        ldy     #$0F
code_9663:  lda     $9D36,y             ; preserve nametable select
        sta     $0600,y
        sta     $0620,y                 ; write 4 rows to PPU queue
        dey                             ; wait for NMI (PPU uploads queued data)
        bpl     code_9663               ; restore nametable select
        sty     palette_dirty           ; flag palette upload
; --- Write Doc Robot portrait frames and faces ---
        lda     #$00                    ; loop until done
        sta     $10
        ldy     #$01                    ; start from portrait index 1
        ldx     #$19                    ; frame delay
        jsr     write_portrait_frames                   ; write_portrait_frames
        ldx     #$19
        jsr     write_portrait_faces                   ; write_portrait_faces

; --- Enter stage select loop at center position ---
code_9681:
        lda     #$01                    ; cursor column = 1 (center)
        sta     $12                     ; $18 = $FF → flag palette upload
        lda     #$03                    ; cursor row offset = 3 (middle row)
        sta     $13
        jmp     stage_select_input_loop                   ; → stage select main loop
; ===========================================================================
; Normal stage return → stage select ($968C)
; ===========================================================================
; Returns to stage select after completing a normal stage (not all beaten).
; Rebuilds nametable 1 with stage select layout, loads return palettes
; from $9C43, writes PPU data, then enters the stage select loop.
; ===========================================================================
code_968C:
        jsr     fade_palette_in         ; disable rendering
        lda     #$04
        sta     oam_ptr
        jsr     prepare_oam_buffer
        jsr     task_yield
        jsr     reset_stage_state                   ; reset_stage_state
        lda     #$13
        sta     prg_bank
        jsr     select_PRG_banks
; --- Fill nametable 1 (displayed on return) ---
        lda     #$01
        sta     camera_x_hi            ; display nametable 1
        lda     #$02                    ; metatile column 2
        jsr     metatile_column_ptr_by_id                   ; metatile_column_ptr_by_id
code_96AC:  lda     #$04
        sta     $10                     ; nametable $2400
        jsr     fill_nametable_progressive                   ; fill_nametable_progressive
        jsr     task_yield
        lda     $70
        bne     code_96AC               ; loop until complete
; --- Load return palettes and CHR banks ---
        lda     #$7C
        sta     $E8
        lda     #$76
        sta     $E9
        lda     #$36                    ; sprite CHR bank
        sta     $EC
        lda     #$34                    ; BG CHR bank
        sta     $ED                     ; save Y
        jsr     update_CHR_banks        ; save table index
        ldy     #$0F                    ; save current PRG bank
code_96CF:  lda     $9C43,y             ; return-from-stage BG palettes
        sta     $0620,y                 ; switch to bank 03
        dey                             ; (contains PPU data tables)
        bpl     code_96CF
        ldy     #$0F                    ; X = table index
code_96DA:  lda     $9C23,y             ; sprite palettes
        sta     $0630,y                 ; low byte from $A56D+X
        dey                             ; high byte from $A580+X
        bpl     code_96DA
; --- Write PPU data and call bank03 for portrait data ---
        lda     #$04                    ; PPU high address
        sta     $10                     ; nametable $2400
        ldx     #$12                    ; PPU data table $12
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     task_yield
        lda     #$03                    ; PPU low address
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     bank03_portrait_setup_routine                   ; call bank03 routine (portrait setup)
        jsr     fade_palette_out        ; enable rendering

; --- Wait for Start/A button to enter stage select ---
code_96FC:                              ; tile data byte
        lda     joy1_press
        and     #$90                    ; Start ($10) or A ($80)
        bne     code_9708
        jsr     task_yield
        jmp     boss_anim_state_check   ; next PPU entry (always branches)

code_9708:  jmp     stage_select_proto_man_oam               ; → clear OAM and re-enter stage select

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

        stx     $0F
portrait_frame_addr_load_loop:  lda     $9D4C,y                 ; PPU addr low byte for portrait y
        sta     L0000
        lda     $9D55,y                 ; PPU addr high byte ($20/$21/$22)
        ora     $10
        sta     $01
        ldx     #$00
portrait_frame_tile_row_process_loop:  lda     $9D5E,x                 ; row Y-offset (bit 7 = end marker)
        bmi     portrait_frame_row_complete
        lda     $9D5F,x                 ; row X-offset within nametable
        clc
        adc     L0000                   ; + base low byte
        sta     $0781,x                 ; → PPU addr low
        lda     $9D5E,x                 ; row offset (carry into high byte)
        adc     $01                     ; + base high byte
        sta     $0780,x                 ; → PPU addr high
        inx
        inx
        lda     $9D5E,x                 ; tile count (N → writes N+1 tiles)
        sta     $0780,x
        sta     $02                     ; loop counter
        inx
portrait_frame_tile_write_loop:  lda     $9D5E,x                 ; tile ID
        sta     $0780,x
        inx
        dec     $02
        bpl     portrait_frame_tile_write_loop
        bmi     portrait_frame_tile_row_process_loop
portrait_frame_row_complete:  sta     $0780,x                 ; $FF end marker
        sta     nametable_dirty
        tya
        pha
        ldx     $0F
        jsr     task_yield_x                   ; submit PPU update buffer
        pla
        tay
portrait_frame_next_position_loop:  iny
        iny                             ; y += 2
        cpy     #$04
        beq     portrait_frame_next_position_loop                   ; skip index 4 (center = Mega Man)
        cpy     #$09
        bcc     portrait_frame_addr_load_loop                   ; loop until all 9 done
        rts

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

        stx     $0F                     ; set rendering mode
        ldx     #$03
portrait_face_tile_copy_loop:  lda     $9D99,x                 ; face row 0 tiles ($C8-$CB)
        sta     $0783,x                 ; wait 1 frame
        lda     $9D9D,x                 ; face row 1 tiles ($D8-$DB)
        sta     $078A,x                 ; (boss intro fanfare)
        lda     $9DA1,x                 ; face row 2 tiles ($E8-$EB)
        sta     $0791,x
        lda     $9DA5,x                 ; face row 3 tiles ($F8-$FB)
        sta     $0798,x
        dex
        bpl     portrait_face_tile_copy_loop
        ldy     #$00                    ; save real stage number
portrait_face_ppu_addr_setup_loop:  lda     $9DC9,y                 ; PPU addr high ($20/$21/$22)
        ora     $10                     ; $22 = $14 (boss intro layout ID)
        sta     $0780                   ; row 0 PPU high
        sta     $0787                   ; row 1 PPU high
        sta     $078E                   ; row 2 PPU high
        sta     $0795                   ; row 3 PPU high
        clc
        lda     $9DD2,y                 ; PPU addr low (base)
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; + 1 nametable row (32 bytes)
        sta     $0788                   ; row 1 PPU low
        adc     #$20                    ; restore real stage number
        sta     $078F                   ; row 2 PPU low
        adc     #$20
        sta     $0796                   ; row 3 PPU low
        lda     #$03                    ; 4 tiles per row (count-1)
        sta     $0782
        sta     $0789
        sta     $0790                   ; table index $05 = common data
        sta     $0797                   ; $10 = 0 → nametable $2000
        lda     #$23                    ; attribute table addr high ($23xx)
        ora     $10                     ; write common intro nametable data
        sta     $079C                   ; wait for NMI
        lda     #$C0                    ; attribute table base ($23C0)
        ora     $9DA9,y                 ; + portrait-specific attr offset
        sta     $079D
        lda     #$00                    ; 1 attribute byte (count-1=0)
        sta     $079E                   ; write boss-specific face tiles
        lda     #$55                    ; attr value $55 = palette 1 for all quadrants
        sta     $079F
        lda     #$FF                    ; end marker
        sta     $07A0
        sta     nametable_dirty
        tya
        pha
        ldx     $0F
        jsr     task_yield_x                   ; submit PPU update buffer
        pla
        tay
portrait_face_next_position_loop:  iny  ; 32 bytes: BG ($0620-$062F) + sprite ($0630-$063F)
        iny                             ; y += 2
        cpy     #$04
        beq     portrait_face_next_position_loop                   ; skip center
        cpy     #$09
        bcc     portrait_face_ppu_addr_setup_loop
        rts
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
        clc
        lda     $9DB3                   ; PPU addr low ($8E)
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; +32 = next nametable row
        sta     $0788                   ; row 1 PPU low
        adc     #$20                    ; (target reached)
        sta     $078F                   ; row 2 PPU low
        adc     #$20                    ; Y -= 4
        sta     $0796                   ; row 3 PPU low
        ldx     #$03                    ; 4 tiles per row
        stx     $0782                   ; reset anim frame counter
        stx     $0789                   ; (keep animation progressing)
        stx     $0790                   ; check animation phase
        stx     $0797                   ; phase 2 = intro anim done
code_9821:  lda     $9DB4,x             ; center face row 0: $CC-$CF
        sta     $0783,x                 ; switch to idle animation $1A
        lda     $9DB8,x                 ; center face row 1: $DC-$DF
        sta     $078A,x
        lda     $9DBC,x                 ; center face row 2: $EC-$EF
        sta     $0791,x                 ; check current OAM ID
        lda     $9DC0,x                 ; center face row 3: $FC-$FF
        sta     $0798,x                 ; loop until idle
        dex                             ; wait $3C (60) frames
        bpl     code_9821               ; (boss stands idle)
; --- Write center attribute table data ---
        ldx     #$04
code_983E:  lda     $9DC4,x             ; $23DB: attr addr, 2 bytes: $88,$22
        sta     $079C,x
        dex                             ; if X == $C0, done
        bpl     code_983E
        sta     $079C                   ; fix attr high byte
        ora     $10                     ; OR nametable select
        sta     $079C                   ; (note: using $0360 which is
        stx     $07A1                   ; end marker ($FF from X=$FF)
        stx     nametable_dirty         ; flag PPU write
        jsr     task_yield
        ldx     #$0E                    ; PPU data table $0E
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        rts
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
;     Stage < Wily → clear OAM, enter stage select at code_9212
;     Stage >= Wily → jump to bank03 $A879 (Wily stage entry)
;   If $60=$12 (all Doc Robots beaten) → Wily fortress gate entrance
; ===========================================================================
code_985D:                              ; (3 seconds on black screen)
        jsr     fade_palette_in         ; disable rendering
        lda     #$04
        sta     oam_ptr
        jsr     prepare_oam_buffer
        jsr     task_yield
        lda     #MUSIC_PASSWORD                    ; music $0E = password screen theme
        jsr     submit_sound_ID_D9
        jsr     reset_stage_state                   ; reset_stage_state
        lda     #$01
        sta     camera_x_hi            ; display nametable 1
        lda     #$00
        sta     scroll_y                ; clear vertical scroll
; --- Fill nametable 1 with password screen layout ---
        lda     #$02                    ; metatile column 2
        jsr     metatile_column_ptr_by_id                   ; metatile_column_ptr_by_id
code_987F:  lda     #$04
        sta     $10                     ; nametable $2400
        jsr     fill_nametable_progressive                   ; fill_nametable_progressive
        jsr     task_yield
        lda     $70
        bne     code_987F               ; loop until complete
; --- Load CHR banks and palettes ---
        lda     #$7C
        sta     $E8
        lda     #$76
        sta     $E9
        lda     #$36                    ; sprite CHR bank
        sta     $EC                     ; bright variant → SP 0
        lda     #$34                    ; BG CHR bank
        sta     $ED                     ; dark variant → SP 1
        jsr     update_CHR_banks
        ldy     #$0F
code_98A2:  lda     $9C43,y             ; return-from-stage BG palettes
        sta     $0620,y
        dey                             ; (always branches)
        bpl     code_98A2
        ldy     #$0F
code_98AD:  lda     $9C23,y             ; sprite palettes
        sta     $0630,y
        dey                             ; default SP 1
        bpl     code_98AD
        jsr     task_yield
; --- Write PPU data and enable rendering ---
        ldx     #$01                    ; PPU data table $01
        lda     #$04                    ; wait 2 frames per flash
        sta     $10                     ; nametable $2400
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        lda     #$58
        sta     $5E                     ; scroll split position
        lda     #$07
        sta     game_mode               ; game mode $07 = stage select
        lda     #$03
        sta     prg_bank
        jsr     select_PRG_banks
        jsr     bank03_portrait_setup_routine                   ; call bank03 portrait routine
        jsr     task_yield
        jsr     fade_palette_out        ; enable rendering
; --- Wait 120 frames ($78) before showing cursor ---
        lda     #$78
code_98DC:  pha
        jsr     task_yield
        pla
        sec                             ; $10 = starting subtract value
        sbc     #$01                    ; (decreases $30→$20→$10→$00)
        bne     code_98DC               ; save palette end index
; --- Show "Game Start / Stage Select" cursor ---
        lda     #$04                    ; 4 colors per palette group
        sta     $10                     ; nametable $2400
        ldx     #$02                    ; PPU data table $02
        jsr     write_ppu_data_from_table                   ; write_ppu_data_from_bank03
        jsr     password_cursor_oam_setup                   ; init_password_cursor (Y=$B7)

; --- Password cursor loop: wait for Start ---
code_98F2:
        jsr     proto_man_sprite_control                   ; update_password_cursor (Up/Down)
        lda     joy1_press
        and     #BTN_START              ; flag palette upload
        bne     code_9901               ; Start pressed → process selection
        jsr     task_yield
        jmp     proto_man_anim_loop     ; $10 -= $10 (next darker step)

; --- Process password selection ---
code_9901:  ldx     #$0B                ; refill weapon energy
code_9903:  lda     player_hp,x         ; loop while $10 >= 0
        bpl     code_990B               ; if this was BG palette 1 (Y=$07),
        lda     #$9C                    ; $9C = full energy
        sta     player_hp,x
code_990B:  dex                         ; wait $1E (30) frames between groups
        bpl     code_9903
; --- Check cursor position to determine action ---
        lda     $0200                   ; cursor Y position
        cmp     #$C7                    ; $C7 = "Stage Select" row
        beq     code_991E               ; → set lives and enter stage select
        lda     stage_id
        cmp     #STAGE_WILY1            ; stage >= $0C = Wily stages
        bcs     code_992C               ; → Wily stage entry via bank03
        jsr     stage_select_proto_man_oam                   ; → clear OAM, re-enter stage select
; --- Set starting lives and check for Wily gate ---
code_991E:  lda     #$02                ; 2 extra lives (3 total)
        sta     lives
        lda     stage_select_page
        cmp     #$12                    ; $12 = all Doc Robots beaten
        bne     code_992B
        jmp     wily_gate_entry                   ; → Wily fortress gate entrance

code_992B:  rts

; --- Wily stage entry (stage >= $0C) ---
code_992C:  lda     #$03
        sta     prg_bank
        jsr     select_PRG_banks
        jmp     bank03_wily_stage_entry                   ; → bank03 Wily stage entry

; ---------------------------------------------------------------------------
; reset_stage_state — zero out game state for stage transition
; ---------------------------------------------------------------------------
; Clears scroll position, weapon state, screen page, and various
; game state variables to prepare for the boss intro or stage load.
; ---------------------------------------------------------------------------

        lda     #$00
        sta     data_compressed_nametable_start                   ; reset something in mapped bank
        sta     $59                     ; game sub-state
        sta     camera_screen                     ; camera/scroll page
        sta     ent_x_scr                   ; entity 0 screen page (Y high)
        sta     ent_y_scr                   ; entity 0 screen page (X high)
        sta     $B1                     ; scroll-related
        sta     $B2
        sta     $B3
        sta     camera_x_hi                     ; horizontal scroll (nametable select)
        sta     camera_x_lo                     ; horizontal scroll (sub-tile)
        sta     weapon_cursor                     ; menu cursor
        sta     $B4
        sta     current_weapon                     ; weapon ID (0 = Mega Buster)
        sta     $9E
        sta     $9F
        sta     $70                     ; nametable fill progress counter
        rts
; ---------------------------------------------------------------------------
; blank_defeated_portraits — erase face tiles for beaten bosses ($995C)
; ---------------------------------------------------------------------------
; Iterates through all 9 grid positions. For beaten bosses (bit set in
; $61), writes blank tile $24 over the 4×4 face area. Also handles
; special cases for Doc Robot phase and Wily gate center portrait.
; ---------------------------------------------------------------------------
code_995C:
        jsr     write_portrait_frame_data                   ; restore_portrait_frames_if_needed
        ldx     #$08                    ; iterate grid positions 8→0
code_9961:  lda     $9DED,x             ; boss bitmask for position X
        beq     code_99CC               ; $00 = center position (special case)
        and     bosses_beaten           ; check if this boss beaten
        beq     code_99BA               ; not beaten → skip
; --- Write blank tiles over defeated boss face ---
code_996A:  lda     $9DC9,x             ; PPU addr high for face position
        ora     $10                     ; OR nametable select
        sta     $0780                   ; row 0 PPU high
        sta     $0787                   ; row 1 PPU high
        sta     $078E                   ; row 2 PPU high
        sta     $0795                   ; row 3 PPU high
        clc
        lda     $9DD2,x                 ; PPU addr low for face position
        sta     $0781                   ; row 0 PPU low
        adc     #$20                    ; +32 = next nametable row
        sta     $0788
        adc     #$20
        sta     $078F
        adc     #$20
        sta     $0796
        lda     #$03                    ; 4 tiles per row
        sta     $0782
        sta     $0789
        sta     $0790
        sta     $0797
        ldy     #$03
        lda     #$24                    ; tile $24 = blank (empty frame interior)
code_99A3:  sta     $0783,y             ; fill all 4 rows with blank tiles
        sta     $078A,y
        sta     $0791,y
        sta     $0798,y
        dey
        bpl     code_99A3
        sty     $079C                   ; end marker ($FF)
        sty     nametable_dirty         ; flag PPU write
        jsr     task_yield
code_99BA:  dex
        bpl     code_9961               ; next grid position
; --- If all Doc Robots beaten, write center Mega Man portrait ---
        lda     stage_select_page
        beq     code_99CB               ; $60=0 → Robot Master phase, skip
        cmp     #$12                    ; $12 = all Doc Robots beaten
        bne     code_99CB
        jsr     write_center_portrait                   ; write_center_portrait
        jsr     task_yield
code_99CB:  rts

; --- Center position special case ---
code_99CC:  lda     stage_select_page
        beq     code_99BA               ; $60=0 → skip center
        cmp     #$12                    ; $12 = all Doc Robots beaten
        beq     code_996A               ; → blank center face too
        lda     bosses_beaten
        cmp     #$FF                    ; all bosses beaten?
        bne     code_99BA               ; no → skip
        beq     code_996A               ; yes → blank center face

; ---------------------------------------------------------------------------
; restore_portrait_frames_if_needed — redraw frames for Doc Robot phase
; ---------------------------------------------------------------------------
; If $60!=0 (Doc Robot or later), redraws portrait frames and faces
; since they may have been modified. If $60=$12, also redraws from index 0.
; ---------------------------------------------------------------------------
code_99DC:
        lda     stage_select_page
        beq     code_99F9               ; $60=0 → nothing to restore
        ldy     #$01                    ; start from portrait index 1
        ldx     #$01                    ; frame delay = 1
        jsr     write_portrait_frames                   ; write_portrait_frames
        lda     stage_select_page
        cmp     #$12                    ; all Doc Robots beaten?
        bne     code_99F4
        ldy     #$00                    ; also redraw from index 0
        ldx     #$01
        jsr     write_portrait_frames                   ; write_portrait_frames
code_99F4:  ldx     #$01
        jsr     write_portrait_faces                   ; write_portrait_faces
code_99F9:  rts
; ---------------------------------------------------------------------------
; load_stage_select_oam — load OAM sprites for stage select screen ($99FA)
; ---------------------------------------------------------------------------
; Copies portrait detail sprites from bank03 ($A231+) into OAM ($0200+).
; If Doc Robot phase ($60!=0), loads from offset $98 (fewer sprites) and
; applies Doc Robot CHR bank $74 and palettes from $9D36.
; After loading, hides OAM sprites for beaten bosses using $9DDB/$9DE4
; tables (Y=$F8 = offscreen).
; ---------------------------------------------------------------------------
code_99FA:
        ldx     #$00                    ; start offset (Robot Master phase)
        lda     stage_select_page
        beq     code_9A19               ; $60=0 → use full sprite set
; --- Doc Robot phase: load fewer sprites, different CHR/palettes ---
        lda     #$74                    ; Doc Robot CHR bank
        sta     $E9
        jsr     update_CHR_banks
        ldy     #$0F
code_9A09:  lda     $9D36,y             ; Doc Robot palettes
        sta     $0600,y
        sta     $0620,y
        dey
        bpl     code_9A09
        sty     palette_dirty           ; flag palette upload
        ldx     #$98                    ; start from OAM offset $98
; --- Copy OAM data from bank03 stage_select_oam_y_table table ---
code_9A19:  stx     L0000               ; save start offset
        lda     prg_bank
        pha
        lda     #$03                    ; switch to bank03
        sta     prg_bank
        jsr     select_PRG_banks
        ldx     L0000
code_9A27:  lda     stage_select_oam_y_table,x             ; OAM Y position
        sta     $0200,x
        lda     stage_select_oam_tile_table,x                 ; OAM tile ID
        sta     $0201,x
        lda     stage_select_oam_attr_table,x                 ; OAM attribute
        sta     $0202,x
        lda     stage_select_oam_x_table,x                 ; OAM X position
        sta     $0203,x
        inx
        inx
        inx
        inx
        cpx     #$CC                    ; copy up to offset $CC (51 sprites)
        bne     code_9A27
        pla
        sta     prg_bank
        jsr     select_PRG_banks
; --- Hide OAM sprites for beaten bosses ---
        ldx     #$08                    ; iterate grid positions 8→0
code_9A4F:  lda     $9DED,x             ; boss bitmask for position X
        beq     code_9A71               ; $00 = center (special case)
        and     bosses_beaten           ; check if beaten
        beq     code_9A6D               ; not beaten → keep sprites visible
; --- Move beaten boss sprites offscreen (Y=$F8) ---
code_9A58:  ldy     $9DDB,x             ; OAM start offset for this portrait
        lda     $9DE4,x                 ; sprite count for this portrait
        sta     L0000
        lda     #$F8                    ; Y=$F8 = offscreen
code_9A62:  sta     $0200,y
        iny
        iny
        iny
        iny                             ; next OAM entry
        dec     L0000
        bpl     code_9A62
code_9A6D:  dex
        bpl     code_9A4F
        rts

; --- Center position special case ---
code_9A71:  lda     stage_select_page
        beq     code_9A6D               ; $60=0 → skip center hiding
        cmp     #$12                    ; all Doc Robots beaten?
        beq     code_9A81               ; → load Wily face sprites, then hide
        lda     bosses_beaten
        cmp     #$FF                    ; all bosses beaten?
        bne     code_9A6D               ; no → skip
        beq     code_9A58               ; yes → hide center sprites
code_9A81:  jsr     load_wily_center_face               ; load Wily center face OAM sprites
        jmp     portrait_oam_write_loop                   ; then hide underlying sprites
; ---------------------------------------------------------------------------
; load_wily_center_sprites — load Wily skull face OAM into center ($9A87)
; ---------------------------------------------------------------------------
; When all Doc Robots are beaten ($60=$12), the center portrait shows
; Dr. Wily's skull. This copies 9 OAM sprite entries (36 bytes) from
; $9DF6 to OAM $02DC-$02FC (center portrait area) and loads Wily-specific
; sprite palettes from $9E1A.
; ---------------------------------------------------------------------------
code_9A87:
        sty     L0000                   ; save Y
        ldy     #$20                    ; 9 sprites × 4 bytes = 36 ($24), start at $20
code_9A8B:  lda     $9DF6,y             ; Wily center sprite Y
        sta     $02DC,y
        lda     $9DF7,y                 ; Wily center sprite tile
        sta     $02DD,y                 ; PPU addr low byte for portrait y
        lda     $9DF8,y                 ; Wily center sprite attr
        sta     $02DE,y                 ; PPU addr high byte ($20/$21/$22)
        lda     $9DF9,y                 ; Wily center sprite X
        sta     $02DF,y
        dey
        dey                             ; row Y-offset (bit 7 = end marker)
        dey
        dey                             ; row X-offset within nametable
        bpl     code_9A8B
; --- Load Wily sprite palettes from $9E1A ---
        ldy     #$0F                    ; → PPU addr low
code_9AAB:  lda     $9E1A,y             ; Wily sprite palette data
        sta     $0610,y                 ; sprite palette buffer
        sta     $0630,y                 ; sprite palette working copy
        dey
        bpl     code_9AAB
        sty     palette_dirty           ; flag palette upload
        ldy     L0000                   ; restore Y
        rts                             ; loop counter

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
code_9ABC:                              ; submit PPU update buffer
        pla                             ; discard return address (2 bytes)
        pla
; --- Wily gate entry point (also called from $9006 jump table) ---
code_9ABE:                              ; y += 2
        jsr     fade_palette_in         ; disable rendering
        jsr     reset_stage_state                   ; reset_stage_state
        lda     #$04
        sta     oam_ptr                 ; loop until all 9 done
        jsr     prepare_oam_buffer
        jsr     task_yield
        jsr     clear_entity_table
; --- Set up fortress approach ---
        lda     #$01
        sta     data_compressed_nametable_start                   ; flag for mapped bank
        lda     #$00
        sta     game_mode               ; game mode $00
        sta     $9E
        sta     $9F
        sta     nmi_skip
        sta     ent_y_scr               ; entity 0 screen page
        sta     ent_y_px                ; entity 0 Y position
        lda     #$17
        sta     $29                     ; level width
        lda     #$20
        sta     $2A                     ; scroll boundary
        lda     #$30
        sta     ent_x_px                ; Mega Man X = $30
        lda     #$01
        sta     player_facing           ; facing right
        sta     $23                     ; face row 0 tiles ($C8-$CB)
        sta     $2E
        lda     #$0D                    ; face row 1 tiles ($D8-$DB)
        sta     $2B
; --- Load Hard Man's stage for fortress approach background ---
        lda     #STAGE_HARD             ; stage $03 = Hard Man
        sta     stage_id                ; face row 3 tiles ($F8-$FB)
        sta     prg_bank
        jsr     select_PRG_banks
; --- Render 33 columns of fortress approach nametable ---
        lda     #$1F                    ; $1F = 31 (column counter)
        sta     $24                     ; PPU addr high ($20/$21/$22)
        lda     #$21                    ; loop counter (33 iterations)
code_9B0E:  pha                         ; row 0 PPU high
        lda     #$01                    ; row 1 PPU high
        sta     $10                     ; row 2 PPU high
        jsr     do_render_column                   ; do_render_column
        jsr     task_yield
        pla                             ; PPU addr low (base)
        sec                             ; row 0 PPU low
        sbc     #$01                    ; + 1 nametable row (32 bytes)
        bne     code_9B0E               ; row 1 PPU low
; --- Set up fortress CHR banks and palettes ---
        sta     $2C                     ; row 2 PPU low
        sta     $2D
        lda     #$50                    ; fortress CHR banks
        sta     $E8                     ; 4 tiles per row (count-1)
        lda     #$52
        sta     $E9
        lda     #$1D                    ; fortress sprite CHR
        sta     $EC
        lda     #$1E                    ; attribute table addr high ($23xx)
        sta     $ED
        jsr     update_CHR_banks
; --- Load fortress palettes from $9E2A ---
        ldy     #$1F                    ; + portrait-specific attr offset
code_9B38:  lda     $9E2A,y             ; fortress palette data (32 bytes)
        sta     $0620,y                 ; 1 attribute byte (count-1=0)
        dey
        bpl     code_9B38               ; attr value $55 = palette 1 for all quadrants
; --- Set up entity slot $1F for fortress gate ---
        lda     #$2A                    ; end marker
        sta     $52
        ldx     #$1F                    ; entity slot $1F = fortress gate
        lda     #$80
        sta     $031F                   ; entity $1F sprite ID
        lda     #$94
        sta     $059F                   ; entity $1F animation state
        lda     #$53
        sta     $033F                   ; entity $1F type
        lda     #$18
        sta     $04FF                   ; entity $1F parameter
        lda     #$C2
        sta     $049F                   ; entity $1F Y velocity
        lda     #$01
        sta     $043F                   ; entity $1F flag
; --- Set up Mega Man entity ---
        jsr     reset_gravity
        lda     #$99                    ; walking animation
        jsr     reset_sprite_anim
        lda     #$18
        sta     camera_screen           ; camera at screen $18
        sta     ent_x_scr              ; entity 0 screen page
        sta     $039F
        lda     #$C0
        sta     $037F                   ; entity $1F X position
        lda     #$02
        sta     $04BF
; --- Clear various entity fields ---
        lda     #$00
        sta     $041F
        sta     $03FF
        sta     $03DF
        sta     $051F
        sta     $053F
        sta     $055F
        sta     $057F
        sta     $0100                   ; task stack area
        sta     $0101
        sta     $0102
        sta     $0103
; --- Start fortress approach ---
        lda     #$30
        sta     ent_x_px                ; Mega Man X = $30
        lda     #MUSIC_WILY_MAP                    ; music $0C = Wily stage theme
        jsr     submit_sound_ID_D9
        jsr     task_yield
        jsr     fade_palette_out        ; enable rendering
        jmp     LC9B3                   ; → fixed bank gate animation

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
        .byte   $0F
        bmi     palette_data_needle_bright
        .byte   $17
        .byte   $0F
        .byte   $07
        bmi     palette_data_spark_bright
        .byte   $0F
        bmi     palette_data_section_start
        asl     $0F,x
        asl     $10
        asl     $0F,x
        bmi     palette_data_gemini_dark
        and     ($0F,x)
        ora     (player_state),y
        and     ($0F,x)
        bmi     palette_data_snake_dark
palette_data_section_start:  ora     ($0F,x)
        .byte   $0C
        .byte   $10
palette_data_spark_bright:  ora     ($0F,x)
        bmi     palette_data_hard_dark
        brk
        .byte   $0F
        brk
        plp
        brk
        .byte   $0F
        bmi     palette_data_top_dark
palette_data_snake_dark:  ora     $090F,y
        bmi     palette_data_hard_bright
        .byte   $0F
        bmi     palette_data_magnet_bright
palette_data_needle_bright:  rol     $0F
        asl     player_state,x
        rol     $0F
        bmi     palette_data_magnet_dark
        .byte   $14
        .byte   $0F
        .byte   $04
        .byte   $34
        .byte   $14
        .byte   $7C
        ror     $38,x
palette_data_gemini_dark:  and     $2536,y
        bpl     palette_data_top_bright
        .byte   $A7
palette_data_hard_bright:  .byte   $97
palette_data_hard_dark:  bmi     palette_data_shadow_dark
        .byte   $0F
        jsr     L0F21
        .byte   $0F
        and     ($20,x)
        ora     #$0F
        .byte   $16
palette_data_top_bright:  jsr     L0F06
        .byte   $20
        .byte   $37
palette_data_top_dark:  .byte   $0F
        .byte   $0F
        bmi     palette_data_section_middle
        ora     ($0F),y
        asl     $26,x
palette_data_magnet_bright:  .byte   $27
        .byte   $0F
        ora     ($2C,x)
        ora     ($0F),y
        bmi     palette_data_section_end
        rol     $0F
        bmi     palette_data_shadow_bright
palette_data_magnet_dark:  ora     ($0F),y
        .byte   $37
        and     ($10,x)
        .byte   $0F
        .byte   $37
        rol     joy1_press_alt
        .byte   $0F
        .byte   $37
        rol     $0F
        .byte   $0F
        jsr     L1121
        .byte   $0F
        jsr     L2937
palette_data_shadow_bright:  .byte   $0F
        jsr     L1626
        .byte   $0F
        jsr     L2110
palette_data_shadow_dark:  .byte   $0F
        jsr     L1121
        .byte   $0F
        jsr     L1137
        .byte   $0F
        jsr     L0F10
        .byte   $0F
        .byte   $20
        .byte   $37
palette_data_section_middle:  .byte   $0F
        .byte   $0F
        jsr     L1121
        .byte   $0F
        .byte   $27
palette_data_section_end:  .byte   $17
        asl     $0F
        .byte   $27
        .byte   $17
        and     ($0F,x)
        jsr     L2110                   ; reset something in mapped bank
        .byte   $04                     ; game sub-state
        .byte   $02                     ; camera/scroll page
        .byte   $04                     ; entity 0 screen page (Y high)
        .byte   $FC                     ; entity 0 screen page (X high)
        brk                             ; scroll-related
        .byte   $FF
        .byte   $97
        sbc     $2801                   ; horizontal scroll (nametable select)
        .byte   $47                     ; horizontal scroll (sub-tile)
        .byte   $67                     ; menu cursor
        ora     ($C0,x)                 ; (preceding data)
        .byte   $47                     ; weapon ID (0 = Mega Buster)
        pla
        ora     ($C8,x)

; --- megaman_eye_sprites ($9C75) ---
; Mega Man center portrait eye/face detail sprites. 6 sprites × 2 bytes (tile, attr)
; per grid position. Attribute $03 = palette 3, $43 = palette 3 + H-flip.
; Updates which way Mega Man looks based on cursor position.
; Tiles $E6-$FF, $6F, $80-$81 are CHR sprite tiles for face details.
;
; Position 0 (Spark Man, top-left): eyes look upper-left
        beq     megaman_eye_pos0_tiles
        sbc     ($03),y
        .byte   $F2
megaman_eye_pos0_tiles:  .byte   $03
        .byte   $F3
        .byte   $03
        .byte   $F4
        .byte   $03
        sbc     $03,x

; Position 1 (Snake Man, top-center): eyes look up
        inc     $03,x
        .byte   $F7
        .byte   $03
        inc     tile_at_feet_lo,x
        sed
        .byte   $03
        sbc     $F803,y
        .byte   $43

; Position 2 (Needle Man, top-right): eyes look upper-right (pos 0 H-flipped)
        .byte   $F2
        .byte   $43
        sbc     (tile_at_feet_lo),y
        beq     megaman_eye_pos8_tiles
        sbc     tile_at_feet_lo,x
        .byte   $F4
        .byte   $43
        .byte   $F3
        .byte   $43

; Position 3 (Hard Man, middle-left): eyes look left
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $FA
        .byte   $03
        .byte   $FB
        .byte   $03
        .byte   $FC
        .byte   $03

; Position 4 (center): eyes look forward (straight ahead)
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $E7
        .byte   $03
        inx
        .byte   $03
        .byte   $E7
        .byte   $43

; Position 5 (Top Man, middle-right): eyes look right (pos 3 H-flipped lower)
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $FC
        .byte   $43
        .byte   $FB
        .byte   $43
        .byte   $FA
        .byte   $43

; Position 6 (Gemini Man, bottom-left): eyes look lower-left
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        sbc     $FE03,x
        .byte   $03
        .byte   $FF
        .byte   $03

; Position 7 (Magnet Man, bottom-center): eyes look down
        inc     $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $80
        .byte   $03
        sta     ($03,x)
        .byte   $80
        .byte   $43

; Position 8 (Shadow Man, bottom-right): eyes look lower-right (pos 6 H-flipped)
        .byte   $E6
megaman_eye_pos8_tiles:  .byte   $03
        .byte   $6F
        .byte   $03
        inc     tile_at_feet_lo
        .byte   $FF                     ; (end: $189CDF = $FD, $43)
        .byte   $43
        inc     $FD43,x
        .byte   $43

; --- stage_select_lookup ($9CE1) ---
; Grid index (col + row*3) → stage $22 value. $FF = not selectable.
; Robot Masters (tier $60=0): indices 0-8
; Doc Robots (tier $60=9):   indices 9-17
;   Index: 0    1    2    3    4    5    6    7    8
;   RM:    $06  $05  $00  $03  $FF  $04  $02  $01  $07
;   Boss:  Sprk Snke Ndl  Hard ctr  Top  Gmni Mgnt Shdw
;   Doc:   $0A  $FF  $08  $FF  $FF  $FF  $09  $FF  $0B
        asl     $05                     ; RM: Spark/Snake/Needle/Hard/ctr/Top
        brk
        .byte   $03
        .byte   $FF
        .byte   $04
        .byte   $02                     ; RM: Gemini/Magnet/Shadow
        ora     ($07,x)
        asl     a                       ; Doc: Spark/$FF/Needle/$FF/$FF
        .byte   $FF
        php
        .byte   $FF
        .byte   $FF
        .byte   $FF                     ; Doc: $FF/Gemini/$FF/Shadow
        ora     #$FF
        .byte   $0B

; --- cursor_pos_y ($9CF3) ---
; Y pixel position for cursor base, per grid index (9 entries).
; Row 0=$18(24), Row 1=$58(88), Row 2=$98(152) — matches portrait frame Y positions.
        clc
        clc
        clc
        cli
        cli
        cli
        tya
        tya
        tya

; --- cursor_pos_x ($9CFC) ---
; X pixel position for cursor base, per grid index (9 entries).
; Col 0=$17(23), Col 1=$67(103), Col 2=$B7(183) — 7px into each frame.
        .byte   $17
        .byte   $67
        .byte   $B7
        .byte   $17
        .byte   $67
        .byte   $B7
        .byte   $17
        .byte   $67
        .byte   $B7

; --- cursor_bolt_offset_y ($9D05) ---
; Y offsets for the 4 corner bolts (indexed 3→0):
;   [0]=$00 top, [1]=$00 top, [2]=$26(38) bottom, [3]=$26(38) bottom
        brk
        brk
        rol     $26

; --- cursor_bolt_offset_x ($9D09) ---
; X offsets for the 4 corner bolts (indexed 3→0):
;   [0]=$00 left, [1]=$2A(42) right, [2]=$00 left, [3]=$2A(42) right
; Bolt corners: TL(0,0) TR(42,0) BL(0,38) BR(42,38)
        brk
        rol     a
        brk
        rol     a

; --- cursor_direction_offsets ($9D0D) ---
; Indexed by button bits: $01=Right, $02=Left, $04=Down, $08=Up
        brk
        ora     (ppu_ctrl_shadow,x)
        brk
        .byte   $03
        brk
        brk
        brk
        sbc     $200F,x
        rol     $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        .byte   $0F
        bmi     center_portrait_border_row0_data
        ora     ($0F),y
        asl     $26,x
        .byte   $27
        .byte   $0F
        ora     ($2C,x)
        ora     ($0F),y
        bmi     center_portrait_border_row1_data
        rol     $0F
        jsr     L1121
        .byte   $0F
        .byte   $37
        .byte   $17
        ora     $0F,x
        jsr     L0515
        .byte   $0F
        jsr     L2110
        .byte   $7C
        ror     $3938,x                 ; CHR bank/tile data
        rol     $25,x

; --- portrait_addr_lo ($9D4C) ---
; PPU nametable address low bytes for portrait frame positions (9 entries).
; Repeats per column: $62(col2)/$6C(col12)/$76(col22).
; Combined with $9D55 high bytes, pixel positions:
;   (16,24) (96,24) (176,24) — row 0
;   (16,88) (96,88) (176,88) — row 1
;   (16,152)(96,152)(176,152)— row 2
        .byte   $62                     ; row 0: Spark/Snake/Needle
        jmp     (L6276)

        jmp     (L6276)

        jmp     (L2076)

        jsr     L2120
        and     ($21,x)
        .byte   $22                     ; row 2 ($22xx = tile row 19)
        .byte   $22
        .byte   $22

; --- portrait_frame_tiles ($9D5E) ---
; Portrait frame tile layout. Groups of: [Y-offset, X-offset, count-1, tile IDs...]
; $FF = end marker. Y/X offsets are relative to portrait base nametable address.
; Frame is 8 tiles wide × 6 tiles tall (64×48 px).
;
; Group format in nametable offset terms:
;   Row 0 (+$00): top border    — 8 tiles: $45 $E0 $E1 $E2 $E2 $E3 $E4 $4B
;   Row 1 (+$21): frame body    — 6 tiles: $D5 $24 $24 $24 $24 $D6
;   Row 2 (+$41): frame body    — 6 tiles: $E5 $24 $24 $24 $24 $E6
;   Row 3 (+$61): frame body    — 6 tiles: $E5 $24 $24 $24 $24 $E6
;   Row 4 (+$81): frame body    — 6 tiles: $F5 $24 $24 $24 $24 $F6
;   Row 5 (+$A0): bottom border — 8 tiles: $55 $F0 $F1 $F2 $F2 $F3 $F4 $5B
;   $24 = empty interior tile (face tiles written separately by write_portrait_faces)
        brk                             ; row 0: top border
        brk
        .byte   $07
        eor     $E0
        sbc     ($E2,x)
center_portrait_border_row0_data:  .byte   $E2
        .byte   $E3
        cpx     $4B
        brk                             ; row 1: L border + interior + R border
        and     ($05,x)
center_portrait_border_row1_data:  cmp     $24,x
        bit     $24
        bit     $D6
        brk                             ; row 2
        eor     ($05,x)
        sbc     $24
        bit     $24
        bit     $E6
        brk                             ; row 3
        adc     ($05,x)
        sbc     $24
        bit     $24
        bit     $E6
        brk                             ; row 4
        sta     ($05,x)
        sbc     $24,x
        bit     $24
        bit     $F6
        brk                             ; row 5: bottom border
        ldy     #$07
        eor     mmc3_shadow,x
        sbc     ($F2),y
        .byte   $F2
        .byte   $F3
        .byte   $F4
        .byte   $5B
        .byte   $FF                     ; end marker

; --- face_tiles_row0 ($9D99) ---
; Portrait face tile IDs — 4×4 grid written by write_portrait_faces.
; Same tile IDs for ALL portraits; CHR bank selects which boss face appears.
        iny                             ; face row 0
        cmp     #$CA
        .byte   $CB
        cld                             ; face row 1
        cmp     $DBDA,y
        inx                             ; face row 2
        sbc     #$EA
        .byte   $EB
        sed                             ; face row 3
        sbc     $FBFA,y

; --- face_attr_offsets ($9DA9) ---
; Attribute table offsets for portrait palette (OR'd with $23C0 base).
        ora     #$00
        asl     a:L0000
        brk
        and     #$00                    ; (attr offsets continued)
        rol     $8E21
        cpy     $CECD                   ; center face row 0
        .byte   $CF
        .byte   $DC                     ; center face row 1
        cmp     $DFDE,x
        cpx     $EEED                   ; center face row 2
        .byte   $EF
        .byte   $FC                     ; center face row 3
        sbc     $FFFE,x

; --- center_attr_data ($9DC4) ---
; Attribute table write for center portrait palette.
        .byte   $23                     ; $23DB: attr addr, 2 bytes: $88,$22
        .byte   $DB
        ora     ($88,x)
        .byte   $22

; --- face_addr_hi ($9DC9) ---
; PPU addr high bytes for face tile writes (9 entries). Same as $9D55.
        jsr     L2020                   ; rows 0/1/2
        and     ($21,x)
        and     (stage_id,x)
        .byte   $22
        .byte   $22

; --- face_addr_lo ($9DD2) ---
; PPU addr low bytes for face positions (9 entries).
; Col 0=$84(tile4), Col 1=$8E(tile14), Col 2=$98(tile24).
; Face pixel positions (inside frame, 2 tiles in from border):
;   (32,32) (112,32) (192,32) — row 0
;   (32,96) (112,96) (192,96) — row 1
;   (32,160)(112,160)(192,160)— row 2
        sty     $8E                     ; row 0: Spark/Snake/Needle
        tya
        sty     $8E                     ; row 1: Hard/center/Top
        tya
        sty     $8E                     ; row 2: Gemini/Magnet/Shadow
        tya

; --- portrait_beat_oam_offsets ($9DDB) ---
        sei
        .byte   $54
        bpl     cursor_y_position_table
        tya
        rti

        clc
        .byte   $64
        brk
        .byte   $07
        .byte   $03
        ora     ($05,x)
        php
        .byte   $04
        .byte   $03
        .byte   $04
        .byte   $03
        rti

        jsr     L0801
        brk
        bpl     stage_lookup_table_data
        .byte   $02
        .byte   $80
        .byte   $63
        .byte   $83
stage_lookup_table_data:  .byte   $02
        ror     $826B,x
        .byte   $03
        ror     $6B,x
        sty     $02
        ror     $856B,x
        .byte   $02
        stx     $6B
cursor_y_position_table:  stx     $01
        stx     $876B
        ora     ($96,x)
        .byte   $73
        dey
        .byte   $02
        stx     $73
        .byte   $89
        ora     ($8E,x)
        .byte   $73
        txa
        ora     ($96,x)
        .byte   $0F
        bmi     cursor_bolt_y_offset_table
        ora     ($0F),y
        .byte   $0F
        .byte   $27
        .byte   $17
        .byte   $0F
        bpl     cursor_direction_offset_table
        .byte   $17
        .byte   $0F
        .byte   $37
        .byte   $3C
        .byte   $0F
        .byte   $0F
cursor_x_position_table:  .byte   $37
        .byte   $27
        asl     $0F
        .byte   $07
        php
        .byte   $09
cursor_bolt_y_offset_table:  .byte   $0F
        and     $1929,y
        .byte   $0F
        and     ($08,x)
        bmi     cursor_bolt_x_offset_table
        .byte   $0F
        bit     $0F11
        .byte   $0F
        bmi     portrait_addr_hi_table
        .byte   $0F
        .byte   $0F
        bpl     intro_chr_bank_table
        .byte   $0F
        .byte   $0F
        brk
        brk
cursor_bolt_x_offset_table:  .byte   $20
        rti

cursor_direction_offset_table:  brk
        brk
        jsr     L8100
        brk
        brk
        bpl     portrait_addr_lo_table
        bpl     intro_palette_table
intro_palette_table:  brk
        brk
        brk
        brk
intro_chr_bank_table:  brk
        brk
        brk
        brk
        brk
        brk
        brk
        rti

        brk
        brk
        brk
        brk
        brk
        brk
        ora     (L0000,x)
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
portrait_addr_lo_table:  brk
        brk
        brk
        brk
portrait_addr_hi_table:  brk
        brk
        brk
        .byte   $02
        brk
        brk
        brk
        brk
        .byte   $04
        brk
        rti

        brk
        brk
        brk
        brk
        jsr     L8004
        brk
        brk
        brk
        brk
        brk
        .byte   $04
        brk
        .byte   $42
        ora     (L0000,x)
        brk
        rti

        brk
        .byte   $02
        brk                             ; (preceding data)
        brk
        brk
        brk
        brk
        .byte   $80
        brk
        .byte   $02
        ora     L0000
        brk
        brk
        brk
        rti

        brk
        brk
        bpl     cursor_x_position_table
        .byte   $04
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        bpl     portrait_frame_tile_table
portrait_frame_tile_table:  brk
        brk
        brk
        brk
        brk
        rti

        bpl     face_tile_row0_table
        pha
face_tile_row0_table:  rti

        .byte   $04
        brk
        brk
        rti

        bpl     face_tile_row1_table
        brk
face_tile_row1_table:  brk
        brk
        .byte   $04
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        ora     (L0000,x)
        brk
        bpl     face_tile_row2_table
face_tile_row2_table:  brk
        bmi     face_tile_row3_table
face_tile_row3_table:  jsr     L0400
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        ora     ($40,x)
        brk
        ora     (L0000,x)
        brk
        brk
        ora     (L0000,x)
        .byte   $80
        brk
        brk
        brk
        .byte   $02                     ; (end: $189CDF = $FD, $43)
        .byte   $04
        brk
        brk
        brk
        brk
        php
        brk
        ora     (L0000,x)
        brk
        ora     ($04,x)
        brk
        asl     a
        brk                             ; RM: Spark/Snake/Needle/Hard/ctr/Top
        brk
        ora     ($20,x)
        brk
        jsr     L0000
        brk                             ; RM: Gemini/Magnet/Shadow
        brk
        brk                             ; Doc: Spark/$FF/Needle/$FF/$FF
        ora     (L0000,x)
        brk
        bpl     face_addr_hi_table
face_addr_hi_table:  brk
        brk                             ; Doc: $FF/Gemini/$FF/Shadow
        brk
        brk
        .byte   $04
        bpl     face_addr_lo_table
face_addr_lo_table:  bpl     face_attr_offset_table
face_attr_offset_table:  brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        bpl     center_attr_ppu_addr
        sta     (L0000,x)
        bpl     portrait_oam_offset_table
        brk
        brk
        brk
        bpl     center_portrait_tile_row0
center_portrait_tile_row0:  brk
        bpl     center_portrait_tile_row1
center_portrait_tile_row1:  brk
        brk
        brk
        brk
        .byte   $04
        brk
        ora     (L0000,x)
        brk
        brk
        php
        bpl     center_portrait_tile_row2
center_portrait_tile_row2:  brk
        brk
        .byte   $04
        bpl     center_portrait_tile_row3
center_portrait_tile_row3:  brk
        brk
        brk
        brk
        .byte   $0C
        brk
        jsr     L0000
        brk
        .byte   $02
        bpl     center_portrait_addr_ppu
center_portrait_addr_ppu:  brk
        .byte   $22
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        php
        brk
        ora     (L0000,x)
        brk
        brk
        brk
        brk
        brk
        brk
center_attr_ppu_addr:  bpl     center_attr_table_data
center_attr_table_data:  .byte   $20
        .byte   $01
portrait_oam_offset_table:  brk
        brk
        bvc     portrait_sprite_count_table
portrait_sprite_count_table:  ora     (L0000,x)
        .byte   $80
        brk
        brk
        .byte   $04
        php
        brk
        php
        bpl     boss_bitmask_table
boss_bitmask_table:  brk
        php
        brk
        brk
        brk
        brk                             ; CHR bank/tile data
        .byte   $04
        .byte   $02
        brk
        brk
        brk
        .byte   $80
        brk
        brk
        brk
        brk                             ; row 0: Spark/Snake/Needle
        brk
        .byte   $1C
        brk
        brk
        ora     L0000
        brk
        brk
        brk
        cpy     L0000                   ; row 2 ($22xx = tile row 19)
        brk
        brk
        brk
        brk
        .byte   $02
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk
        brk                             ; row 0: top border
        bpl     wily_center_sprite_table
        brk
        brk
        .byte   $04
        ora     L0000
        brk
        brk
        rti                             ; row 1: L border + interior + R border

        brk
        brk
        brk
        brk                             ; row 2
        brk
        brk
        rti

        brk                             ; row 3
        ora     (L0000,x)
        brk
        brk
        brk
        brk                             ; row 4
        brk
        brk
        brk
        brk
        brk                             ; row 5: bottom border
        brk
        brk
        jsr     L0000
wily_center_sprite_table:  brk
        brk
        brk
        .byte   $02
        brk                             ; end marker
        .byte   $04
        brk
        brk
        brk
        brk                             ; face row 0
        brk
        brk
        brk                             ; face row 1
        brk
        brk                             ; face row 2
        brk
        brk
        brk                             ; face row 3
        rti

        brk
        brk
        brk
        rti

        brk                             ; (attr offsets continued)
        brk
        brk                             ; center face row 0
        brk
        jsr     L0000                   ; center face row 1
        brk
        eor     #$F1                    ; center face row 2
