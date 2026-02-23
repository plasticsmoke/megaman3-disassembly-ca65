; =============================================================================
; MEGA MAN 3 (U) — BANK $13 — SPECIAL/ENDING STAGE DATA 2
; =============================================================================
; Mapped to $A000-$BFFF. Stage data bank for stage $13 (ending sequence).
; Referenced by stage_to_bank[$13] = $13. Also loaded for game over screen
; (bank18 sets prg_bank=$13). The ensure_stage_bank routine skips bank switch
; when prg_bank is already $13, suggesting this bank remains mapped as a
; default in some contexts.
;
; Standard MM3 stage data layout:
;   $A000-$A9FF: boss AI local data / compressed nametable graphics
;   $AA00-$AA5F: screen metatile column grid + room/screen parameters
;   $AA60-$AA7F: room pointer table (2 bytes/room)
;   $AA80-$AA81: BG CHR bank indices
;   $AA82-$AAFF: screen layout data (20 bytes/entry: 16 columns + 4 connections)
;   $AB00-$ABFF: enemy spawn table — screen number (terminated by $FF)
;   $AC00-$ACFF: enemy spawn table — X pixel position
;   $AD00-$ADFF: enemy spawn table — Y pixel position
;   $AE00-$AEFF: enemy spawn table — global enemy ID (→ bank $00 tables)
;   $AF00-$B6FF: metatile column definitions (64 bytes per column ID)
;   $B700-$BAFF: metatile CHR tile definitions (4 bytes per metatile: TL,TR,BL,BR)
;   $BB00-$BEFF: nametable quadrant data (4 x 256-byte blocks)
;   $BF00-$BFFF: tile collision attribute table (1 byte per metatile index)
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"


.segment "BANK13"

; =============================================================================
; BOSS AI LOCAL DATA / COMPRESSED NAMETABLE DATA ($A000-$A9FF)
; =============================================================================
; First $A00 bytes. For ending/special stages this region typically holds
; compressed nametable graphics or cutscene tile data.
; =============================================================================

        .byte   $BE,$FF,$EB,$FF,$FF,$FF,$FE,$FF
        .byte   $6E,$FF,$BF,$FF,$BF,$FF,$FF,$FF
        .byte   $BE,$FF,$FB,$FF,$EE,$FF,$BE,$FF
        .byte   $FE,$FF,$FD,$FF,$F7,$FF,$FE,$FF
        .byte   $B6,$FF,$FC,$FF,$DB,$FF,$AA,$FF
        .byte   $FA,$FF,$BF,$FF,$FE,$FF,$EF,$FD
        .byte   $FF,$FD,$FC,$DF,$F9,$FF,$BF,$FF
        .byte   $AE,$FF,$BC,$FF,$FF,$FF,$FF,$FF
        .byte   $FE,$FF,$BA,$FF,$FF,$FE,$EE,$FF
        .byte   $FB,$FF,$EF,$FF,$FF,$FF,$F7,$FF
        .byte   $EF,$FF,$FA,$FF,$BF,$FF,$BF,$FF
        .byte   $FF,$FF,$AC,$BF,$FF,$FF,$EB,$FF
        .byte   $EF,$FF,$FA,$FB,$6B,$FF,$BB,$CF
        .byte   $6B,$BF,$EF,$EF,$EF,$FF,$FE,$FF
        .byte   $FF,$FD,$FF,$FF,$FB,$FF,$FE,$FF
        .byte   $FB,$FF,$ED,$FF,$FF,$FF,$AE,$FF
        .byte   $EF,$FF,$FF,$FF,$A2,$FE,$BE,$FF
        .byte   $BE,$EF,$FF,$FF,$AE,$FF,$FF,$FF
        .byte   $FF,$FF,$FF,$FF,$FA,$FF,$FF,$FF
        .byte   $BF,$FF,$BE,$FF,$BF,$FF,$FE,$FF
        .byte   $BE,$FF,$F7,$F7,$AF,$FE,$F9,$BE
        .byte   $EF,$FF,$FB,$FF,$BE,$DF,$AB,$FF
        .byte   $FF,$77,$AB,$FF,$9A,$FF,$8E,$FF
        .byte   $FB,$FF,$FA,$FF,$BB,$FF,$FF,$FF
        .byte   $EA,$FF,$BA,$FF,$FF,$FF,$BE,$FE
        .byte   $FE,$FF,$E2,$FF,$FF,$FF,$BF,$FF
        .byte   $FF,$FF,$FB,$FF,$FB,$FF,$FA,$FF
        .byte   $AB,$FF,$3B,$FD,$EA,$EF,$BF,$FF
        .byte   $2F,$FF,$BA,$EF,$BE,$FD,$AA,$BD
        .byte   $89,$EF,$DA,$FF,$BA,$FD,$BB,$FF
        .byte   $BF,$FA,$BF,$FF,$FA,$FF,$FF,$FF
        .byte   $E9,$FF,$6A,$FF,$FB,$FF,$BB,$FF
        .byte   $E9,$FF,$FF,$FF,$FB,$DF,$EE,$FF
        .byte   $FF,$FF,$FE,$FF,$EF,$FF,$BB,$FF
        .byte   $FF,$77,$EF,$7F,$FE,$FF,$BE,$FF
        .byte   $BF,$FF,$AF,$FF,$EF,$FF,$BE,$FF
        .byte   $BE,$FB,$FE,$FF,$AE,$FF,$BE,$F7
        .byte   $B2,$FD,$FE,$FF,$7E,$7F,$FE,$DF
        .byte   $FA,$FF,$FF,$FF,$BB,$FB,$DF,$FF
        .byte   $FF,$FF,$FF,$FF,$FB,$FF,$FE,$BF
        .byte   $FA,$DF,$FF,$FF,$FF,$FD,$FB,$FF
        .byte   $AD,$EF,$BE,$FF,$FF,$FF,$EF,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$AF,$FF
        .byte   $FF,$FF,$EE,$7F,$FA,$FF,$FE,$FF
        .byte   $2E,$DF,$BB,$FB,$E6,$FF,$7A,$6F
        .byte   $FF,$F7,$EE,$F7,$DF,$F7,$BE,$FF
        .byte   $EE,$EF,$FF,$FF,$EE,$FF,$FF,$FF
        .byte   $FF,$FF,$DE,$FB,$AF,$FF,$FF,$EF
        .byte   $6F,$FF,$FF,$FF,$FE,$FF,$BE,$FF
        .byte   $EF,$BF,$EA,$FF,$FE,$FF,$FE,$FF
        .byte   $FF,$7D,$EB,$FB,$FF,$FF,$EA,$FF
        .byte   $FF,$FF,$EF,$FF,$FF,$DF,$BE,$EF
        .byte   $BE,$FF,$AF,$FF,$BB,$FF,$FC,$F7
        .byte   $FF,$FF,$ED,$EF,$BB,$FF,$FF,$FF
        .byte   $FB,$FE,$FE,$F7,$EE,$FF,$77,$FF
        .byte   $7F,$FF,$FE,$DF,$EB,$BF,$EF,$FF
        .byte   $BD,$7F,$FF,$FF,$FA,$DF,$FF,$7D
        .byte   $FB,$FD,$FF,$AF,$FF,$FF,$FB,$FF
        .byte   $BF,$F7,$FE,$FF,$FF,$FF,$7E,$FF
        .byte   $FE,$FF,$BE,$F3,$FB,$FF,$EE,$FF
        .byte   $A6,$FF,$EF,$9F,$FE,$FF,$FA,$FF
        .byte   $FB,$BF,$F4,$FF,$38,$B7,$FF,$FB
        .byte   $E3,$FF,$FA,$FF,$AF,$F7,$FE,$FF
        .byte   $BF,$FF,$EA,$FD,$EF,$FD,$FE,$EF
        .byte   $2B,$FF,$AE,$FF,$FF,$FF,$FF,$FF
        .byte   $EE,$FB,$BF,$FF,$FF,$FF,$AF,$FF
        .byte   $BF,$FF,$3B,$FF,$AE,$F7,$FF,$FF
        .byte   $FF,$FF,$AE,$FF,$7F,$7F,$FF,$BF
        .byte   $EF,$DF,$AF,$FF,$BE,$FF,$FE,$FF
        .byte   $FA,$FF,$FA,$FF,$BF,$FD,$EB,$FF
        .byte   $FF,$FE,$FB,$FF,$FB,$FF,$EE,$FF
        .byte   $FF,$FF,$EF,$DF,$EE,$EF,$FB,$FF
        .byte   $F9,$FF,$FE,$FF,$EF,$7F,$EB,$FF
        .byte   $EB,$FF,$9F,$FF,$FF,$FF,$A6,$D7
        .byte   $FE,$FF,$FE,$FF,$AF,$FF,$EB,$FF
        .byte   $BF,$FF,$EF,$7F,$BF,$FF,$EC,$FF
        .byte   $FF,$FF,$FA,$FF,$BF,$FF,$AA,$AB
        .byte   $0E,$DF,$FE,$FF,$AF,$FF,$CE,$FF
        .byte   $AF,$7F,$BF,$FF,$F9,$7F,$FF,$FF
        .byte   $FD,$FF,$FF,$FF,$F2,$FF,$FE,$FF
        .byte   $FA,$FF,$FF,$FF,$FE,$FB,$BB,$BF
        .byte   $FF,$FF,$EF,$FF,$FB,$FF,$BB,$FF
        .byte   $FF,$7F,$FF,$FD,$FF,$FF,$FA,$FF
        .byte   $EF,$FF,$EB,$FF,$AB,$FF,$AE,$FF
        .byte   $FE,$FF,$6F,$FF,$EF,$7F,$B9,$FF
        .byte   $FF,$BF,$BA,$7F,$9F,$FF,$EB,$DF
        .byte   $BB,$FF,$FE,$FF,$FB,$FF,$AF,$FF
        .byte   $EE,$FF,$BC,$FF,$BE,$FB,$FE,$FF
        .byte   $EA,$FF,$FB,$FF,$BD,$DF,$EE,$BF
        .byte   $EE,$FF,$FF,$FF,$EF,$7F,$FE,$FF
        .byte   $9F,$FF,$FE,$FF,$FE,$FF,$FE,$FF
        .byte   $F9,$FF,$FF,$FF,$FB,$FF,$BE,$FF
        .byte   $BA,$F7,$AF,$FF,$BC,$FF,$BE,$FF
        .byte   $A2,$E7,$FF,$FB,$E8,$FB,$BF,$FF
        .byte   $BF,$DF,$E3,$FF,$EF,$FE,$2E,$FF
        .byte   $FE,$FF,$FF,$FF,$B8,$FF,$BF,$7F
        .byte   $BE,$FF,$BA,$FF,$FF,$DF,$FF,$FF
        .byte   $FF,$FF,$FF,$FD,$FE,$FF,$FE,$FF
        .byte   $EB,$FD,$FE,$FF,$BE,$FF,$3F,$FF
        .byte   $BE,$FF,$EE,$7F,$EF,$FF,$EF,$FF
        .byte   $EB,$FF,$EE,$59,$BE,$DF,$BF,$FF
        .byte   $BF,$FD,$BE,$DF,$EB,$FF,$FF,$F7
        .byte   $FF,$FF,$FE,$FF,$FA,$FF,$6B,$EF
        .byte   $FF,$FF,$EF,$FF,$FF,$FF,$FE,$BF
        .byte   $EB,$FF,$FE,$FE,$FF,$FF,$FF,$F7
        .byte   $FA,$FF,$7B,$FF,$AF,$FF,$EF,$FF
        .byte   $FB,$FF,$DF,$FF,$AB,$FF,$FE,$DF
        .byte   $BB,$FF,$FF,$FF,$FB,$FF,$FE,$FF
        .byte   $BE,$BF,$BB,$BF,$AB,$79,$FF,$FF
        .byte   $8B,$FB,$EE,$FF,$BF,$7F,$FB,$FD
        .byte   $FF,$FF,$FF,$FF,$F9,$FF,$FA,$FE
        .byte   $BF,$FF,$BE,$F7,$AF,$FF,$FE,$FF
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$DE,$FD
        .byte   $AF,$FF,$EA,$FF,$FE,$FF,$FE,$FF
        .byte   $FE,$FF,$FA,$FF,$7F,$F7,$EB,$FF
        .byte   $FB,$FF,$AA,$FF,$EF,$FF,$FF,$FF
        .byte   $FE,$FE,$EA,$FF,$3F,$FF,$AF,$FE
        .byte   $F7,$7F,$AE,$DF,$FE,$FD,$FE,$F7
        .byte   $BF,$FF,$BF,$FE,$FF,$FF,$FA,$FF
        .byte   $BE,$FF,$F7,$FF,$FA,$FF,$EE,$FF
        .byte   $AE,$FC,$FB,$FF,$BA,$F7,$FE,$FF
        .byte   $FF,$FF,$FB,$FF,$FE,$FB,$BF,$FF
        .byte   $BB,$FD,$FB,$FF,$EF,$FF,$FF,$FF
        .byte   $AF,$FF,$EE,$FF,$EF,$FF,$FA,$FF
        .byte   $FB,$EE,$FF,$F3,$FA,$7F,$1F,$FB
        .byte   $BE,$FD,$7E,$FF,$BE,$FB,$E2,$EF
        .byte   $FA,$FD,$BB,$DF,$AB,$FF,$FE,$F6
        .byte   $FF,$FF,$EF,$7F,$AF,$FF,$FA,$FD
        .byte   $FE,$F7,$FF,$9D,$CF,$13,$EF,$D5
        .byte   $EF,$DD,$EF,$F7,$FF,$5D,$FB,$FD
        .byte   $FE,$77,$FF,$7F,$7F,$74,$FF,$75
        .byte   $FF,$17,$FF,$56,$FF,$75,$FF,$55
        .byte   $ED,$D7,$FF,$7D,$FF,$55,$FF,$77
        .byte   $EF,$45,$EF,$FF,$FF,$79,$EF,$F7
        .byte   $FF,$D5,$FF,$55,$FF,$FF,$BF,$37
        .byte   $FF,$EF,$FE,$F7,$FE,$5D,$FE,$79
        .byte   $FD,$7D,$FF,$F7,$FF,$D7,$FF,$F7
        .byte   $EF,$D5,$EB,$DF,$FF,$75,$FF,$D7
        .byte   $FF,$FF,$FF,$F5,$FF,$97,$FB,$D5
        .byte   $FF,$57,$FF,$4F,$FF,$7F,$FE,$57
        .byte   $FF,$DF,$DF,$55,$FB,$5F,$F7,$F7
        .byte   $FF,$B7,$FB,$FF,$FF,$FF,$FF,$D7
        .byte   $FD,$7D,$FF,$7F,$FF,$FF,$FF,$FF
        .byte   $FF,$75,$FF,$5F,$FF,$7D,$7F,$7F
        .byte   $FF,$F5,$FF,$F6,$FB,$75,$FF,$5F
        .byte   $FB,$FF,$FF,$7D,$FF,$BD,$FF,$FF
        .byte   $FF,$7D,$FD,$D5,$FF,$5F,$FF,$FF
        .byte   $7F,$77,$FF,$75,$FF,$5D,$FF,$DD
        .byte   $FF,$5F,$FF,$DD,$BD,$FD,$EF,$D7
        .byte   $FF,$D7,$FF,$BF,$FF,$BD,$FF,$F7
        .byte   $FF,$55,$FF,$F7,$FF,$F5,$FF,$D5
        .byte   $FF,$EF,$FF,$BF,$FF,$DF,$FF,$DD
        .byte   $FF,$D3,$FF,$FF,$FF,$FD,$FF,$5F
        .byte   $BF,$DF,$DF,$5D,$FF,$F7,$BF,$9D
        .byte   $FF,$B5,$FF,$EF,$FF,$7F,$FF,$F9
        .byte   $FF,$E7,$FF,$FF,$FF,$DF,$FF,$65
        .byte   $FF,$FD,$FF,$FD,$EB,$7D,$FF,$FF
        .byte   $DF,$7B,$FF,$7F,$5F,$FD,$FE,$EF
        .byte   $FF,$D5,$6F,$FF,$7F,$7E,$FF,$DF
        .byte   $F7,$75,$FF,$F5,$FF,$FF,$FE,$57
        .byte   $9F,$75,$FF,$47,$FF,$D7,$FF,$D7
        .byte   $FF,$54,$FE,$79,$FF,$57,$BF,$FB
        .byte   $FF,$7C,$FC,$55,$FB,$F1,$F9,$7F
        .byte   $FF,$55,$FF,$77,$FF,$75,$DF,$75
        .byte   $FB,$FD,$FB,$F7,$FF,$FF,$9F,$DF
        .byte   $FF,$ED,$FF,$77,$FF,$79,$DF,$5F
        .byte   $FB,$7F,$FF,$FD,$FF,$DD,$FF,$D5
        .byte   $FF,$F5,$FF,$FF,$BF,$F7,$BF,$7F
        .byte   $FE,$5D,$EF,$D7,$EF,$7F,$FF,$FF
        .byte   $BF,$DD,$AF,$55,$FF,$5D,$FF,$DD
        .byte   $FE,$FD,$7F,$7D,$FF,$5F,$FF,$39
        .byte   $BF,$4F,$FB,$DF,$FF,$57,$FF,$5D
        .byte   $FF,$F5,$FF,$7D,$FF,$7F,$FE,$7F
        .byte   $F7,$5F,$FF,$5F,$FF,$D7,$FF,$77
        .byte   $FF,$F5,$FF,$77,$FF,$5F,$FF,$7F
        .byte   $FF,$D5,$FF,$DF,$FF,$FF,$FF,$F7
        .byte   $F7,$D5,$FE,$F7,$FF,$75,$FF,$D7
        .byte   $FD,$7D,$FF,$77,$FF,$A9,$FF,$9D
        .byte   $FF,$B5,$FD,$D7,$FB,$5F,$FF,$D5
        .byte   $BF,$7F,$FB,$F7,$FF,$57,$FF,$DD
        .byte   $FF,$77,$FF,$7B,$FD,$9D,$FF,$FB
        .byte   $FF,$F5,$FF,$FF,$FF,$57,$FF,$7F
        .byte   $FF,$FD,$FF,$5D,$FF,$DF,$FF,$F5
        .byte   $7D,$57,$FF,$FD,$FF,$FF,$FF,$FF
        .byte   $7D,$DD,$FF,$75,$D7,$75,$F3,$3F
        .byte   $FF,$57,$BE,$DD,$FF,$FF,$FF,$DF
        .byte   $FF,$75,$FF,$57,$FF,$55,$FF,$F3
        .byte   $FF,$57,$FF,$75,$FF,$FF,$FD,$77
        .byte   $FF,$D7,$FF,$5C,$FF,$FD,$DD,$D7
        .byte   $FF,$F7,$EF,$FD,$FD,$D5,$FF,$75
        .byte   $DF,$7F,$FF,$FD,$FF,$FF,$FF,$DD
        .byte   $FF,$F5,$FF,$7F,$BF,$DF,$FF,$FD
        .byte   $F7,$1F,$FF,$5F,$FF,$ED,$FD,$F5
        .byte   $FF,$D5,$FD,$45,$ED,$55,$9F,$FD
        .byte   $EF,$DD,$FF,$97,$DD,$DF,$FE,$DD
        .byte   $DF,$47,$FE,$54,$FB,$EF,$BF,$7D
        .byte   $FF,$DF,$FF,$FF,$FF,$45,$FF,$59
        .byte   $FF,$FF,$FF,$F7,$FF,$D7,$FF,$5F
        .byte   $FF,$F5,$FF,$F9,$FF,$57,$FF,$75
        .byte   $FF,$77,$FF,$DF,$FF,$7F,$FF,$D7
        .byte   $FF,$FD,$FF,$DF,$FE,$D5,$F7,$75
        .byte   $FF,$75,$FF,$77,$FF,$C5,$FF,$18
        .byte   $FF,$DD,$FF,$1C,$FF,$50,$FF,$FD
        .byte   $AF,$DF,$FD,$FD,$FB,$DC,$FF,$FF
        .byte   $FF,$DE,$FF,$CF,$FF,$77,$BF,$7F
        .byte   $FF,$6D,$FB,$D5,$FF,$5D,$FF,$F5
        .byte   $DF,$D7,$FF,$FD,$FF,$DD,$FF,$FD
        .byte   $FE,$FF,$FF,$F5,$FF,$5D,$FF,$FF
        .byte   $FF,$5D,$BF,$55,$FF,$7D,$FF,$51
        .byte   $FF,$DF,$BF,$65,$FF,$D7,$FF,$DF
        .byte   $FF,$D7,$FF,$97,$FF,$57,$FD,$F7
        .byte   $FF,$7F,$FD,$F5,$FF,$7F,$5D,$D7
        .byte   $FF,$5F,$FF,$7F,$EF,$DD,$FE,$D5
        .byte   $BF,$F4,$BF,$FB,$FF,$FF,$FF,$FF
        .byte   $FF,$5F,$FF,$FF,$FF,$DD,$FF,$F9
        .byte   $FF,$7D,$DF,$D7,$FF,$D7,$FF,$F7
        .byte   $FF,$77,$FF,$DF,$FF,$DC,$FF,$55
        .byte   $FF,$FD,$FF,$55,$FF,$F5,$FF,$5D
        .byte   $DF,$57,$FF,$F6,$FF,$D5,$FF,$D7
        .byte   $FF,$DF,$FF,$D7,$EF,$F7,$3F,$7F
        .byte   $FF,$FF,$BF,$55,$FF,$5B,$FF,$FF
        .byte   $FE,$F7,$EF,$55,$FF,$67,$FF,$7F
        .byte   $FF,$57,$FF,$77,$E7,$FD,$FD,$F5
        .byte   $FF,$75,$EF,$5D,$FD,$F7,$FE,$F5
        .byte   $FF,$77,$FF,$CD,$7E,$7D,$FF,$FF
        .byte   $FF,$7E,$FF,$FF,$FF,$47,$FF,$45
        .byte   $FF,$55,$FE,$5D,$FF,$7C,$FF,$7F
        .byte   $FF,$FC,$FB,$D7,$7F,$D5,$FF,$54
        .byte   $EF,$66,$FF,$D4,$FF,$55,$BF,$17
        .byte   $FF,$FE,$FF,$F7,$FF,$71,$FF,$77
        .byte   $FF,$D7,$FF,$F9,$FF,$DD,$FF,$DF
        .byte   $FF,$DF,$FF,$F5,$FF,$FD,$FF,$F5
        .byte   $FF,$77,$FD,$55,$7F,$1F,$FF,$D1
        .byte   $FF,$55,$FF,$75,$D7,$F9,$FF,$75
        .byte   $FF,$C1,$FF,$59,$FD,$CF,$F5,$7F
        .byte   $FF,$5F,$FF,$F7,$FF,$75,$FF,$75
        .byte   $FF,$C5,$FF,$DD,$FF,$F5,$BF,$77
        .byte   $FF,$DF,$FF,$5E,$FF,$75,$FF,$75
        .byte   $BE,$7F,$FF,$77,$FF,$BF,$FF,$E5
        .byte   $FF,$7F,$FF,$D5,$FF,$27,$FF,$7F
        .byte   $FF,$71,$FF,$1D,$FF,$57,$FF,$F5
        .byte   $FF,$55,$FF,$D7,$FF,$F5,$BF,$DD
        .byte   $FF,$FD,$FF,$77,$FF,$FD,$FF,$BD
        .byte   $FF,$55,$7F,$57,$DF,$57,$FF,$5D
        .byte   $FF,$7D,$DF,$55,$FF,$3D,$EF,$75
        .byte   $FD,$FD,$FF,$7F,$FF,$FF,$FF,$D5
        .byte   $FF,$F7,$FF,$FD,$FF,$ED,$FB,$D1
        .byte   $FE,$57,$FF,$F7,$DF,$F5,$FF,$55
        .byte   $FD,$7D,$FF,$DF,$FF,$DD,$EF,$FD
        .byte   $FE,$F5,$FF,$D5,$FF,$D7,$EF,$FF
        .byte   $FF,$55,$FF,$57,$FF,$D7,$FF,$DD
        .byte   $F7,$77,$FD,$7F,$FF,$5D,$FF,$FF
        .byte   $ED,$75,$FF,$DD,$FF,$DD,$FF,$F5
        .byte   $FF,$FF,$FF,$7F,$FF,$5F,$FF,$F5
        .byte   $FF,$DF,$FF,$DF,$FF,$F5,$FF,$55
        .byte   $FF,$DF,$FF,$B7,$FE,$77,$FF,$3D
        .byte   $00,$08,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$10
        .byte   $00,$01,$08,$00,$08,$04,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$41
        .byte   $00,$80,$00,$00,$00,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$01,$00,$00,$00,$00,$00,$80
        .byte   $00,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $00,$10,$00,$00,$00,$10,$00,$00
        .byte   $02,$00,$00,$20,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$00,$00,$08,$00,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $08,$00,$08,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$20,$00,$00,$00,$44,$80,$02
        .byte   $00,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$42,$00,$10,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$08,$00
        .byte   $08,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$44
        .byte   $00,$00,$02,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$80
        .byte   $08,$08,$00,$00,$00,$00,$00,$31
        .byte   $00,$80,$00,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$20,$00,$10,$00,$20
        .byte   $00,$00,$00,$04,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$A0,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$04,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$40,$80,$00,$00,$00
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$04
        .byte   $02,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$04,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$80,$00,$00,$00,$00,$00
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $00,$80,$00,$44,$00,$08,$00,$00
        .byte   $00,$40,$00,$00,$00,$00,$40,$01
        .byte   $20,$00,$00,$00,$00,$00,$00,$80
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$08
        .byte   $00,$00,$80,$00,$00,$04,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$00,$00
        .byte   $00,$00,$00,$00,$00,$20,$00,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$80,$00,$00,$00,$00
        .byte   $00,$40,$00,$00,$00,$00,$00,$88
        .byte   $00,$21,$00,$00,$02,$00,$00,$81
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$80,$40,$00,$00,$00,$20
        .byte   $00,$00,$00,$00,$00,$60,$00,$00
        .byte   $00,$00,$00,$10,$00,$40,$00,$00

; =============================================================================
; SCREEN METATILE GRID + ROOM PARAMETERS ($AA00-$AA5F)
; =============================================================================
; Screen metatile column grid: each byte is a column ID referencing the
; metatile column definitions at $AF00. $FF = end-of-data marker.
; Room/screen init parameters follow the grid data.
; =============================================================================

        .byte   $00,$00,$00,$00,$00,$00,$00,$10
        .byte   $00,$00,$00,$32,$00,$00,$00,$00
        .byte   $00,$00,$22,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$00,$20,$80,$2A,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$00,$00,$00,$00,$00,$00
        .byte   $20,$00,$00,$00,$02,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$01,$00,$44
        .byte   $00,$00,$00,$00,$00,$00,$00,$02

; --- room pointer table ($AA60): 2 bytes/room, indexes screen layout data ---

        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$00,$00,$04,$00,$84
        .byte   $00,$00,$00,$80,$00,$00,$00,$31
        .byte   $00,$00,$00,$14,$00,$00,$00,$00

; --- BG CHR bank indices + screen layout data ($AA80): ---
; First 2 bytes = CHR bank pair, then screen layout entries
; (20 bytes each: 16 metatile column IDs + 4 connection bytes)

        .byte   $00,$00,$00,$00,$00,$80,$00,$80
        .byte   $00,$00,$00,$40,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$08,$00,$08,$00,$01
        .byte   $00,$48,$80,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$01,$00,$04,$20,$00
        .byte   $00,$00,$00,$00,$02,$40,$08,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$41
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$04,$08,$01,$00,$00,$00,$80
        .byte   $00,$18,$00,$00,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$01,$00,$00

; =============================================================================
; ENEMY SPAWN TABLES ($AB00-$AEFF)
; =============================================================================
; Four parallel 256-byte arrays indexed by stage enemy ID:
;   $AB00,y = screen number where enemy appears (terminated by $FF)
;   $AC00,y = X pixel position on screen
;   $AD00,y = Y pixel position on screen
;   $AE00,y = global enemy ID (indexes bank $00 property tables)
; =============================================================================

; --- enemy spawn table: screen number ($AB00) ---

        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$80,$04,$00,$00
        .byte   $02,$00,$00,$00,$00,$80,$00,$10
        .byte   $80,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$10,$00,$21,$00,$00,$00,$00
        .byte   $00,$00,$00,$08,$00,$24,$00,$00
        .byte   $00,$00,$80,$09,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$08,$00,$00,$20
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$08,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$08
        .byte   $20,$04,$00,$00,$00,$00,$00,$08
        .byte   $00,$00,$00,$00,$00,$22,$00,$00
        .byte   $00,$00,$00,$00,$20,$10,$00,$00
        .byte   $02,$00,$00,$20,$00,$00,$00,$20
        .byte   $00,$00,$00,$08,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$40,$80,$00,$08,$00,$00,$00
        .byte   $00,$00,$00,$00,$08,$04,$00,$02
        .byte   $08,$80,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$04,$00,$80,$00,$00,$00,$10
        .byte   $00,$00,$00,$02,$00,$20,$00,$00
        .byte   $00,$10,$00,$80,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$00,$40,$00,$00
        .byte   $00,$00,$00,$92,$00,$02,$00,$00
        .byte   $00,$00,$00,$40,$00,$00,$00,$00

; --- enemy spawn table: X pixel position ($AC00) ---

        .byte   $00,$00,$80,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$20,$00,$10,$00
        .byte   $40,$00,$00,$00,$00,$01,$10,$00
        .byte   $00,$00,$00,$00,$02,$00,$00,$00
        .byte   $00,$10,$00,$01,$01,$00,$00,$00
        .byte   $00,$00,$20,$00,$80,$00,$02,$00
        .byte   $04,$00,$00,$00,$00,$44,$22,$04
        .byte   $00,$11,$10,$00,$00,$00,$00,$00
        .byte   $80,$04,$00,$00,$00,$00,$20,$00

; --- enemy spawn table: Y pixel position ($AD00) ---

        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$08,$40,$00,$00,$10,$04
        .byte   $00,$10,$00,$00,$00,$00,$00,$00
        .byte   $20,$40,$00,$04,$02,$00,$10,$00
        .byte   $00,$44,$88,$00,$08,$00,$00,$00
        .byte   $40,$00,$00,$40,$40,$00,$20,$00
        .byte   $04,$00,$00,$00,$02,$00,$00,$00
        .byte   $09,$05,$80,$40,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$44,$00,$00
        .byte   $10,$00,$00,$00,$01,$40,$00,$01
        .byte   $00,$00,$04,$00,$22,$00,$40,$00
        .byte   $00,$01,$08,$01,$00,$00,$40,$44
        .byte   $20,$40,$22,$40,$00,$00,$00,$04
        .byte   $00,$00,$01,$00,$11,$00,$00,$00
        .byte   $44,$00,$00,$04,$00,$04,$00,$00
        .byte   $01,$10,$06,$00,$30,$00,$00,$00
        .byte   $80,$01,$08,$40,$00,$40,$02,$00
        .byte   $00,$10,$02,$00,$00,$10,$00,$01
        .byte   $40,$01,$00,$00,$00,$00,$00,$00
        .byte   $A0,$00,$08,$40,$10,$00,$01,$00
        .byte   $00,$00,$00,$14,$04,$00,$40,$40
        .byte   $20,$00,$08,$40,$80,$00,$A0,$00
        .byte   $08,$05,$00,$00,$01,$01,$00,$10

; ===========================================================================
; ENEMY SPAWN TABLE — Y POSITIONS ($AD00-$ADFF)
; ===========================================================================
; Y pixel position for each enemy spawn (parallel to $AB00 table).
; ===========================================================================
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$10,$00,$00,$00,$00,$08,$00
        .byte   $20,$00,$00,$00,$00,$00,$00,$00
        .byte   $40,$00,$00,$01,$00,$00,$80,$10
        .byte   $80,$04,$02,$10,$00,$00,$00,$04
        .byte   $80,$00,$20,$10,$00,$00,$01,$00
        .byte   $40,$01,$88,$40,$00,$04,$00,$00
        .byte   $02,$41,$00,$00,$00,$00,$00,$01
        .byte   $00,$01,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$08,$00,$00,$10
        .byte   $0C,$00,$21,$10,$80,$00,$00,$00
        .byte   $02,$01,$40,$10,$00,$00,$00,$00
        .byte   $EC,$00,$00,$00,$80,$00,$25,$10
        .byte   $80,$01,$10,$00,$04,$00,$84,$00
        .byte   $00,$00,$04,$00,$00,$00,$50,$04
        .byte   $00,$00,$00,$00,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$01,$01,$00
        .byte   $00,$00,$00,$01,$00,$00,$40,$00
        .byte   $00,$00,$18,$00,$00,$44,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$00
        .byte   $20,$00,$80,$10,$10,$40,$00,$01
        .byte   $04,$40,$80,$04,$01,$40,$24,$04
        .byte   $00,$00,$08,$10,$00,$00,$01,$00
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $20,$00,$40,$00,$00,$01,$80,$00
        .byte   $02,$00,$01,$40,$00,$04,$00,$14
        .byte   $00,$00,$A0,$00,$20,$40,$01,$00
        .byte   $04,$10,$01,$40,$10,$00,$01,$00
        .byte   $01,$01,$84,$04,$30,$10,$00,$04
        .byte   $01,$40,$02,$00,$00,$00,$00,$00
        .byte   $21,$00,$A0,$00,$21,$00,$00,$00
        .byte   $08,$01,$00,$00,$28,$00,$40,$00

; --- enemy spawn table: global enemy ID ($AE00) ---

        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; Potton, -, -, -, -, -, -, -
        .byte   $40,$40,$00,$00,$00,$00,$00,$00 ; Doc scr, Doc scr, -, -, -, -, -, -
        .byte   $00,$00,$00,$14,$00,$00,$40,$00 ; -, -, -, Petit Snakey, -, -, Doc scr, -
        .byte   $00,$00,$04,$00,$00,$00,$00,$00 ; -, -, Hammer Joe, -, -, -, -, -
        .byte   $04,$00,$20,$00,$00,$00,$42,$00 ; Hammer Joe, -, Penpen Maker, -, -, -, Doc scr, -
        .byte   $00,$10,$00,$00,$00,$00,$00,$00 ; -, Mag Fly(B), -, -, -, -, -, -
        .byte   $00,$04,$80,$40,$00,$00,$24,$00 ; -, Hammer Joe, Yellow Devil, Doc scr, -, -, Junk Block(B), -
        .byte   $08,$00,$04,$00,$10,$00,$00,$00 ; Bomb Flier, -, Hammer Joe, -, Mag Fly(B), -, -, -
        .byte   $84,$00,$00,$00,$80,$00,$01,$00 ; Break Man, -, -, -, Yellow Devil, -, Potton, -
        .byte   $00,$44,$80,$00,$40,$10,$00,$00 ; -, Doc scr, Yellow Devil, -, Doc scr, Mag Fly(B), -, -
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$04,$00,$00,$00,$40,$00 ; -, -, Hammer Joe, -, -, -, Doc scr, -
        .byte   $A6,$11,$40,$40,$10,$00,$00,$10 ; $A6, Junk Golem, Doc scr, Doc scr, Mag Fly(B), -, -, Mag Fly(B)
        .byte   $98,$05,$60,$00,$00,$00,$00,$00 ; $98, New Shotman, Surprise Box, -, -, -, -, -
        .byte   $10,$00,$00,$00,$08,$00,$18,$00 ; Mag Fly(B), -, -, -, Bomb Flier, -, Pickelman, -
        .byte   $04,$00,$00,$00,$28,$00,$00,$04 ; Hammer Joe, -, -, -, Hard Knuckle, -, -, Hammer Joe
        .byte   $80,$00,$00,$00,$00,$00,$C0,$00 ; Yellow Devil, -, -, -, -, -, $C0, -
        .byte   $00,$00,$00,$00,$00,$01,$08,$00 ; -, -, -, -, -, Potton, Bomb Flier, -
        .byte   $00,$00,$01,$10,$00,$00,$00,$00 ; -, -, Potton, Mag Fly(B), -, -, -, -
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$10,$00,$00,$26,$00,$40,$40 ; Hammer Joe, Mag Fly(B), -, -, Magnet Missile, -, Doc scr, Doc scr
        .byte   $81,$40,$0A,$00,$04,$00,$80,$44 ; Wily Machine, Doc scr, Yambow, -, Hammer Joe, -, Yellow Devil, Doc scr
        .byte   $10,$00,$10,$00,$01,$00,$00,$40 ; Mag Fly(B), -, Mag Fly(B), -, Potton, -, -, Doc scr
        .byte   $02,$00,$00,$00,$02,$00,$00,$00 ; New Shotman, -, -, -, New Shotman, -, -, -
        .byte   $60,$00,$40,$00,$40,$00,$43,$04 ; Surprise Box, -, Doc scr, -, Doc scr, -, Doc scr, Hammer Joe
        .byte   $20,$00,$00,$00,$20,$01,$00,$00 ; Penpen Maker, -, -, -, Penpen Maker, Potton, -, -
        .byte   $20,$00,$08,$01,$20,$41,$08,$00 ; Penpen Maker, -, Bomb Flier, Potton, Penpen Maker, Doc scr, Bomb Flier, -
        .byte   $00,$00,$08,$00,$00,$00,$00,$00 ; -, -, Bomb Flier, -, -, -, -, -
        .byte   $00,$00,$10,$01,$00,$11,$85,$10 ; -, -, Mag Fly(B), Potton, -, Junk Golem, $85, Mag Fly(B)
        .byte   $00,$01,$00,$00,$20,$05,$80,$00 ; -, Potton, -, -, Penpen Maker, New Shotman, Yellow Devil, -
        .byte   $00,$10,$00,$00,$00,$00,$80,$00 ; -, Mag Fly(B), -, -, -, -, Yellow Devil, -
        .byte   $60,$00,$00,$00,$40,$05,$21,$00 ; Surprise Box, -, -, -, Doc scr, New Shotman, Proto Man, -

; =============================================================================
; METATILE COLUMN DEFINITIONS ($AF00-$B6FF)
; =============================================================================
; 64 bytes per column ID. Each column is a vertical strip of metatile indices
; for one 16px-wide column of a screen. Referenced by screen layout data.
; =============================================================================

        .byte   $00,$00,$01,$02,$03,$04,$00,$00 ; Column $00
        .byte   $00,$05,$06,$07,$08,$09,$0A,$00
        .byte   $00,$0B,$0C,$0D,$0E,$0F,$10,$00
        .byte   $00,$00,$00,$11,$12,$00,$00,$00
        .byte   $00,$13,$14,$15,$16,$00,$00,$00
        .byte   $00,$17,$18,$19,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $1A,$1B,$1C,$1D,$1E,$1A,$1B,$1F ; Column $01
        .byte   $20,$21,$22,$23,$24,$20,$25,$22
        .byte   $26,$27,$28,$29,$2A,$26,$27,$28
        .byte   $20,$2B,$22,$2C,$2D,$20,$2E,$22
        .byte   $26,$27,$28,$29,$2A,$26,$27,$28
        .byte   $20,$2F,$22,$30,$31,$20,$32,$22
        .byte   $33,$34,$35,$36,$37,$33,$34,$35
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $38,$3D,$3E,$3F,$40,$38,$39,$3A ; Column $02
        .byte   $38,$41,$42,$42,$43,$44,$45,$3A
        .byte   $38,$46,$47,$47,$48,$49,$4A,$3A
        .byte   $38,$4B,$4C,$4C,$4D,$4E,$4F,$3A
        .byte   $38,$50,$51,$52,$53,$54,$55,$3A
        .byte   $38,$56,$57,$57,$57,$57,$58,$3A
        .byte   $38,$59,$5A,$5A,$5A,$5A,$5B,$3A
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A ; Column $03
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $5C,$5D,$5E,$5F,$60,$5C,$5D,$5E
        .byte   $61,$61,$61,$61,$61,$61,$61,$61
        .byte   $62,$63,$64,$65,$66,$62,$63,$64
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $1A,$1B,$1C,$1D,$1E,$1A,$1B,$1F ; Column $04
        .byte   $20,$00,$22,$67,$68,$20,$00,$22
        .byte   $26,$27,$28,$29,$2A,$26,$27,$28
        .byte   $20,$00,$22,$67,$68,$20,$00,$22
        .byte   $26,$27,$28,$29,$2A,$26,$27,$28
        .byte   $20,$00,$22,$67,$68,$20,$00,$22
        .byte   $33,$34,$35,$36,$37,$33,$34,$35
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $69,$69,$6A,$69,$69,$69,$69,$69 ; Column $05
        .byte   $69,$6B,$69,$69,$6C,$6D,$6A,$69
        .byte   $69,$69,$69,$6D,$6A,$6E,$6A,$6D
        .byte   $6F,$70,$69,$69,$71,$69,$6D,$6C
        .byte   $71,$69,$6F,$69,$69,$69,$69,$69
        .byte   $72,$69,$71,$69,$6F,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$73,$70,$74 ; Column $06
        .byte   $69,$73,$6D,$6C,$75,$6D,$6E,$69
        .byte   $76,$76,$76,$76,$76,$76,$77,$76
        .byte   $78,$79,$7A,$7B,$7C,$7D,$7E,$7F
        .byte   $80,$81,$82,$83,$84,$85,$86,$87
        .byte   $88,$89,$8A,$8B,$8C,$8D,$8E,$8F
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $07
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $08
        .byte   $69,$69,$69,$69,$6F,$69,$69,$69
        .byte   $69,$90,$69,$69,$69,$69,$69,$74
        .byte   $69,$69,$69,$69,$69,$91,$69,$69
        .byte   $69,$6C,$69,$69,$69,$69,$92,$69
        .byte   $93,$93,$93,$93,$93,$93,$93,$93
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $09
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $0A
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $0B
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $0C
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $0D
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $0E
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$94 ; Column $0F
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $69,$69,$69,$69,$69,$69,$69,$94
        .byte   $00,$00,$01,$02,$03,$04,$00,$00 ; Column $10
        .byte   $00,$05,$06,$07,$08,$09,$0A,$00
        .byte   $00,$0B,$0C,$0D,$0E,$0F,$10,$00
        .byte   $00,$00,$00,$11,$12,$00,$00,$00
        .byte   $00,$95,$14,$15,$16,$00,$00,$00
        .byte   $00,$17,$18,$19,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; Column $11
        .byte   $96,$97,$98,$99,$9A,$9B,$9C,$9D
        .byte   $00,$9E,$9F,$A0,$A1,$A2,$A3,$A4
        .byte   $00,$00,$00,$00,$00,$00,$00,$A5
        .byte   $00,$A6,$A7,$A8,$A9,$00,$00,$00
        .byte   $00,$AA,$AB,$AC,$AD,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $12
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $13
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $1A,$AE,$AF,$B0,$B1,$B2,$AE,$1F ; Column $14
        .byte   $20,$00,$22,$67,$68,$20,$00,$22
        .byte   $B3,$B4,$B5,$29,$B6,$B7,$B8,$B9
        .byte   $20,$00,$22,$67,$68,$20,$00,$22
        .byte   $BA,$BB,$BC,$BD,$BE,$BF,$C0,$BC
        .byte   $20,$00,$22,$67,$68,$20,$00,$22
        .byte   $C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8
        .byte   $38,$39,$3A,$3B,$3C,$38,$39,$3A
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $15
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $16
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $17
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $18
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $19
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $1A
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $1B
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $1C
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $1D
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $1E
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69 ; Column $1F
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69
        .byte   $69,$69,$69,$69,$69,$69,$69,$69

; =============================================================================
; METATILE CHR TILE DEFINITIONS ($B700-$BAFF)
; =============================================================================
; 4 bytes per metatile: top-left, top-right, bottom-left, bottom-right CHR
; tile indices. Defines the 2x2 pattern for each 16x16 metatile.
; =============================================================================

        .byte   $02,$02,$02,$02,$02,$02,$02,$81
        .byte   $02,$02,$82,$83,$02,$02,$84,$85
        .byte   $02,$02,$86,$02,$02,$02,$02,$A1
        .byte   $88,$89,$90,$91,$8A,$8B,$92,$93
        .byte   $8C,$8D,$94,$95,$8E,$8F,$96,$97
        .byte   $02,$02,$A2,$02,$A8,$A9,$02,$02
        .byte   $98,$99,$02,$02,$9A,$9B,$02,$A5
        .byte   $9C,$9D,$A6,$02,$9E,$9F,$02,$02
        .byte   $AA,$AB,$02,$02,$AC,$AD,$B4,$B5
        .byte   $AE,$AF,$B6,$B7,$02,$02,$02,$BB
        .byte   $02,$02,$B1,$B2,$02,$02,$B3,$B8
        .byte   $02,$02,$B9,$02,$02,$BA,$02,$02
        .byte   $7D,$D5,$02,$02,$C7,$CF,$02,$02
        .byte   $71,$72,$79,$40,$73,$74,$41,$42
        .byte   $70,$71,$43,$79,$72,$73,$40,$41
        .byte   $74,$70,$42,$43,$01,$71,$43,$79
        .byte   $71,$48,$79,$49,$36,$37,$3E,$3F
        .byte   $4B,$71,$4A,$79,$48,$30,$49,$38
        .byte   $31,$4B,$39,$4A,$24,$25,$2C,$2D
        .byte   $71,$44,$79,$40,$45,$46,$41,$42
        .byte   $47,$71,$43,$79,$44,$45,$40,$41
        .byte   $46,$47,$42,$43,$26,$27,$2E,$2F
        .byte   $48,$76,$49,$7E,$77,$4B,$7F,$4A
        .byte   $34,$35,$3C,$3D,$20,$21,$28,$29
        .byte   $48,$32,$49,$3A,$33,$4B,$3B,$4A
        .byte   $22,$23,$2A,$2B,$71,$44,$79,$7A
        .byte   $45,$46,$7B,$7C,$47,$71,$78,$79
        .byte   $44,$45,$7A,$7B,$46,$47,$7C,$78
        .byte   $71,$72,$79,$7A,$73,$74,$7B,$7C
        .byte   $70,$71,$78,$79,$72,$73,$7A,$7B
        .byte   $74,$70,$7C,$78,$73,$74,$01,$C0
        .byte   $70,$71,$C1,$C2,$72,$73,$C3,$C4
        .byte   $74,$70,$C5,$78,$C8,$D0,$C9,$D8
        .byte   $D1,$D1,$D9,$D9,$D1,$D2,$D9,$DD
        .byte   $71,$72,$DF,$EC,$73,$74,$EC,$E4
        .byte   $CA,$D8,$01,$E0,$D9,$D9,$E1,$E1
        .byte   $D9,$DA,$E1,$E2,$E7,$B0,$EE,$D3
        .byte   $B0,$EA,$D4,$EF,$CB,$F5,$CC,$F5
        .byte   $F6,$F6,$F6,$F6,$F6,$F7,$F6,$E5
        .byte   $71,$DB,$79,$DB,$B0,$DE,$B0,$E6
        .byte   $CD,$FD,$7B,$7C,$FE,$FE,$78,$79
        .byte   $FE,$FE,$7A,$7B,$FE,$FF,$7C,$78
        .byte   $71,$FD,$79,$7A,$E3,$ED,$7B,$7C
        .byte   $D6,$D7,$DB,$B0,$D7,$D7,$B0,$B0
        .byte   $D7,$E8,$B0,$EA,$DB,$B0,$FD,$E3
        .byte   $B0,$B0,$E3,$E3,$B0,$EA,$E3,$E9
        .byte   $71,$72,$F9,$FA,$73,$74,$FB,$FC
        .byte   $70,$71,$F8,$F9,$72,$73,$FA,$FB
        .byte   $74,$70,$FC,$F8,$CE,$CE,$CE,$CE
        .byte   $CE,$CE,$F1,$F2,$CE,$CE,$F3,$F4
        .byte   $CE,$CE,$F0,$F1,$CE,$CE,$F2,$F3
        .byte   $CE,$CE,$F4,$F0,$48,$02,$49,$02
        .byte   $02,$4B,$02,$4A,$00,$00,$00,$00
        .byte   $00,$00,$00,$16,$14,$15,$1C,$1D
        .byte   $00,$1B,$00,$00,$19,$1A,$00,$00
        .byte   $00,$00,$00,$59,$16,$00,$00,$00
        .byte   $00,$00,$1B,$00,$00,$59,$1A,$00
        .byte   $00,$00,$00,$1B,$00,$00,$16,$00
        .byte   $00,$59,$00,$00,$00,$19,$1A,$00
        .byte   $00,$00,$11,$11,$12,$13,$11,$11
        .byte   $00,$00,$1E,$08,$00,$00,$09,$09
        .byte   $00,$53,$0A,$5B,$00,$00,$60,$61
        .byte   $00,$60,$00,$68,$61,$00,$54,$6B
        .byte   $00,$53,$56,$5B,$00,$64,$53,$6C
        .byte   $56,$10,$5E,$03,$10,$10,$03,$1F
        .byte   $0B,$00,$5A,$54,$68,$69,$56,$69
        .byte   $1E,$68,$64,$65,$5D,$57,$65,$66
        .byte   $5F,$00,$5E,$08,$5B,$6D,$09,$09
        .byte   $5F,$03,$65,$65,$03,$1F,$66,$1F
        .byte   $5A,$5C,$5A,$5D,$5F,$58,$50,$6A
        .byte   $6C,$6D,$51,$6A,$6D,$6E,$52,$6E
        .byte   $5F,$10,$5F,$03,$10,$10,$03,$03
        .byte   $00,$00,$18,$00,$1A,$00,$00,$00
        .byte   $00,$00,$12,$13,$11,$11,$00,$00
        .byte   $00,$4D,$00,$4D,$02,$02,$02,$B0
        .byte   $02,$02,$02,$80,$02,$02,$81,$82
        .byte   $02,$02,$83,$84,$02,$02,$85,$86
        .byte   $02,$02,$87,$90,$02,$02,$91,$92
        .byte   $AC,$AD,$93,$94,$AE,$02,$95,$02
        .byte   $89,$8A,$02,$BA,$8B,$8C,$BB,$BC
        .byte   $8D,$8E,$BD,$96,$8F,$98,$97,$BE
        .byte   $99,$9A,$BF,$B7,$9B,$9C,$A3,$A4
        .byte   $9D,$02,$A5,$02,$88,$02,$02,$02
        .byte   $02,$02,$02,$9E,$02,$02,$9F,$A6
        .byte   $02,$02,$A0,$AF,$02,$02,$B8,$02
        .byte   $02,$B9,$A7,$B1,$7D,$D5,$B2,$B3
        .byte   $C7,$CF,$B4,$B5,$02,$02,$B6,$02
        .byte   $73,$74,$75,$76,$70,$71,$7E,$18
        .byte   $72,$73,$19,$1A,$74,$70,$1B,$1C
        .byte   $71,$72,$1D,$7D,$71,$50,$79,$40
        .byte   $51,$52,$41,$5A,$53,$71,$5B,$79
        .byte   $46,$6B,$5A,$5B,$71,$4C,$79,$40
        .byte   $4D,$4E,$41,$5A,$4F,$71,$5B,$79
        .byte   $71,$54,$79,$40,$55,$56,$41,$5A
        .byte   $6B,$71,$5B,$79,$68,$6D,$40,$75
        .byte   $6E,$6B,$76,$43,$71,$5C,$79,$40
        .byte   $5D,$6A,$41,$5A,$71,$60,$79,$7A
        .byte   $61,$62,$6C,$58,$63,$71,$59,$79
        .byte   $64,$65,$7A,$6C,$66,$6B,$58,$59
        .byte   $71,$64,$79,$7A,$57,$5E,$6C,$58
        .byte   $5F,$71,$59,$79,$00,$00,$00,$00
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

; =============================================================================
; NAMETABLE QUADRANT DATA ($BB00-$BEFF)
; =============================================================================
; Four 256-byte blocks: nametable data read by stage setup code at
; $BB00,x / $BC00,x / $BD00,x / $BE00,x for metatile rendering.
; Each quadrant stores tile attribute/nametable bytes for one section of
; the screen layout.
; =============================================================================

        .byte   $00,$25,$24,$C3,$C8,$CA,$CC,$CE
        .byte   $00,$C1,$C1,$D3,$E8,$EA,$EC,$EE
        .byte   $D2,$EB,$00,$E9,$E0,$E2,$00,$00
        .byte   $ED,$00,$00,$FE,$E4,$E6,$00,$C3
        .byte   $80,$82,$84,$86,$88,$8A,$8C,$8E
        .byte   $A0,$A2,$A4,$A6,$A8,$AA,$AC,$AE
        .byte   $C0,$C2,$C4,$C6,$C8,$CA,$CC,$CE
        .byte   $E0,$E2,$E4,$E6,$E8,$EA,$EC,$EE
        .byte   $75,$72,$74,$25,$55,$57,$58,$5A
        .byte   $25,$75,$61,$51,$00,$00,$00,$00
        .byte   $C4,$C5,$C5,$00,$C4,$C5,$C5,$D4
        .byte   $DA,$FD,$EF,$D8,$D5,$D4,$D4,$D6
        .byte   $C9,$CB,$00,$00,$CD,$CE,$CE,$00
        .byte   $D9,$DA,$DC,$DC,$DE,$DF,$70,$00
        .byte   $25,$25,$25,$25,$25,$00,$4C,$4E
        .byte   $25,$73,$75,$72,$74,$24,$6C,$6E
        .byte   $00,$24,$24,$24,$24,$24,$24,$00
        .byte   $24,$A2,$A4,$A6,$A8,$AA,$AC,$AE
        .byte   $C0,$C2,$C4,$C6,$C8,$CA,$CC,$CE
        .byte   $E0,$E2,$E4,$E6,$E8,$EA,$EC,$EE
        .byte   $00,$24,$8D,$00,$00,$24,$24,$00
        .byte   $24,$80,$8E,$8A,$24,$77,$79,$24
        .byte   $24,$24,$24,$24,$24,$7B,$7B,$76
        .byte   $24,$24,$24,$24,$00,$00,$00,$00
        .byte   $25,$25,$25,$25,$25,$25,$3A,$24
        .byte   $25,$25,$25,$25,$25,$25,$25,$24
        .byte   $30,$32,$33,$2D,$2D,$24,$25,$25
        .byte   $40,$42,$43,$37,$39,$43,$39,$73
        .byte   $2C,$2E,$2F,$24,$31,$39,$39,$25
        .byte   $25,$24,$24,$25,$31,$39,$73,$2D
        .byte   $3A,$3A,$3A,$3A,$3A,$37,$38,$39
        .byte   $25,$73,$75,$72,$74,$37,$38,$39
; --- nametable block 1 ($BC00-$BCFF) ---
        .byte   $00,$25,$24,$C3,$C9,$CB,$CD,$CF
        .byte   $C0,$C1,$C2,$FF,$E9,$EB,$ED,$EF
        .byte   $D2,$EC,$E8,$EA,$E1,$E3,$FD,$00
        .byte   $00,$00,$00,$00,$E5,$E7,$F7,$D4
        .byte   $81,$83,$85,$87,$89,$8B,$8D,$8F
        .byte   $A1,$A3,$A5,$A7,$A9,$AB,$AD,$AF
        .byte   $C1,$C3,$C5,$C7,$C9,$CB,$CD,$CF
        .byte   $E1,$E3,$E5,$E7,$E9,$EB,$ED,$EF
        .byte   $25,$73,$75,$72,$56,$58,$59,$5B
        .byte   $50,$60,$72,$25,$00,$00,$00,$00
        .byte   $C5,$C5,$C6,$D7,$C5,$C5,$C6,$D5
        .byte   $DB,$FE,$FF,$D8,$D4,$D6,$00,$00
        .byte   $CA,$CC,$00,$00,$CE,$CE,$CF,$00
        .byte   $DA,$DB,$DC,$C5,$DF,$70,$FF,$00
        .byte   $25,$25,$25,$25,$25,$00,$4D,$4F
        .byte   $72,$74,$25,$73,$75,$24,$6D,$6F
        .byte   $00,$24,$24,$24,$24,$24,$24,$00
        .byte   $A1,$A3,$A5,$A7,$A9,$AB,$AD,$24
        .byte   $C1,$C3,$C5,$C7,$C9,$C4,$CD,$CF
        .byte   $E1,$E3,$E5,$E7,$E9,$EB,$ED,$EF
        .byte   $00,$82,$24,$00,$00,$24,$24,$00
        .byte   $85,$81,$8F,$24,$24,$78,$7A,$24
        .byte   $24,$24,$24,$24,$AF,$7B,$7B,$24
        .byte   $24,$24,$24,$24,$00,$00,$00,$00
        .byte   $25,$25,$25,$25,$25,$25,$3A,$24
        .byte   $25,$25,$25,$0D,$0E,$0F,$25,$24
        .byte   $31,$31,$25,$2D,$2D,$24,$25,$25
        .byte   $41,$41,$25,$24,$25,$72,$25,$30
        .byte   $2D,$2D,$72,$24,$33,$72,$75,$37
        .byte   $25,$39,$39,$30,$31,$25,$2C,$2F
        .byte   $3A,$3A,$3A,$3A,$3A,$24,$24,$25
        .byte   $72,$74,$25,$73,$75,$24,$24,$25
; --- nametable block 2 ($BD00-$BDFF) ---
        .byte   $00,$25,$24,$C3,$D8,$DA,$DC,$DE
        .byte   $D0,$D1,$D1,$FF,$F8,$FA,$FC,$FE
        .byte   $DD,$FB,$00,$F9,$F0,$F2,$ED,$ED
        .byte   $00,$EE,$FE,$00,$F4,$F6,$F7,$C3
        .byte   $90,$92,$94,$96,$98,$9A,$9C,$9E
        .byte   $B0,$B2,$B4,$B6,$B8,$BA,$BC,$BE
        .byte   $D0,$D2,$D4,$D6,$D8,$DA,$DC,$DE
        .byte   $F0,$F2,$F4,$F6,$F8,$FA,$FC,$FE
        .byte   $45,$47,$48,$4A,$65,$62,$64,$25
        .byte   $65,$25,$71,$61,$00,$00,$00,$00
        .byte   $DD,$DD,$DD,$C7,$DD,$DD,$DD,$D6
        .byte   $00,$00,$FF,$00,$D4,$D5,$D5,$D4
        .byte   $D9,$DA,$00,$00,$DD,$DD,$DD,$00
        .byte   $D9,$DA,$DD,$DD,$DE,$DE,$DF,$00
        .byte   $25,$63,$65,$62,$64,$00,$5C,$5E
        .byte   $25,$52,$25,$25,$53,$0A,$7C,$24
        .byte   $00,$92,$94,$96,$98,$9A,$9C,$00
        .byte   $B0,$B2,$B4,$B6,$B8,$BA,$BC,$BE
        .byte   $D0,$D2,$D4,$D6,$D8,$DA,$DC,$DE
        .byte   $F0,$F2,$F4,$F6,$F8,$FA,$FC,$FE
        .byte   $00,$83,$8B,$00,$00,$67,$69,$00
        .byte   $86,$90,$9E,$88,$24,$6B,$3E,$66
        .byte   $24,$0A,$0E,$1C,$24,$24,$24,$24
        .byte   $0A,$1D,$24,$24,$00,$00,$00,$00
        .byte   $25,$25,$25,$25,$25,$25,$3B,$20
        .byte   $25,$25,$25,$25,$25,$25,$25,$1B
        .byte   $37,$38,$39,$30,$31,$1C,$30,$31
        .byte   $37,$38,$39,$37,$39,$39,$39,$52
        .byte   $30,$32,$33,$2D,$24,$43,$39,$63
        .byte   $31,$2D,$24,$63,$24,$2F,$52,$33
        .byte   $25,$52,$25,$25,$53,$40,$42,$43
        .byte   $3B,$3B,$3B,$3B,$3B,$2C,$2E,$2F
; --- nametable block 3 ($BE00-$BEFF) ---
        .byte   $00,$25,$24,$C3,$D9,$DB,$DD,$DF
        .byte   $D1,$D1,$EF,$EF,$F9,$FB,$FD,$FF
        .byte   $DD,$FC,$F8,$FA,$F1,$F3,$00,$00
        .byte   $00,$00,$00,$00,$F5,$00,$00,$D4
        .byte   $91,$93,$95,$97,$99,$9B,$9D,$9F
        .byte   $B1,$B3,$B5,$B7,$B9,$BB,$BD,$BF
        .byte   $D1,$D3,$D5,$D7,$D9,$DB,$DD,$DF
        .byte   $F1,$F3,$F5,$F7,$F9,$FB,$FD,$FF
        .byte   $46,$48,$49,$4B,$25,$63,$65,$62
        .byte   $60,$70,$25,$62,$00,$00,$00,$00
        .byte   $DD,$DD,$EF,$C8,$DD,$DD,$00,$D6
        .byte   $00,$00,$EF,$D8,$D5,$D4,$00,$00
        .byte   $DA,$DB,$00,$00,$DD,$DD,$EF,$00
        .byte   $DA,$DB,$DD,$DD,$DE,$DF,$EF,$00
        .byte   $62,$64,$25,$63,$65,$00,$5D,$5F
        .byte   $25,$53,$25,$52,$25,$1C,$24,$7F
        .byte   $00,$93,$95,$97,$99,$9B,$9D,$00
        .byte   $B1,$B3,$B5,$B7,$B9,$BB,$BD,$BF
        .byte   $D1,$D3,$D5,$D7,$D9,$DB,$DD,$DF
        .byte   $F1,$F3,$F5,$F7,$F9,$FB,$FD,$FF
        .byte   $00,$84,$8C,$00,$00,$68,$6A,$00
        .byte   $87,$91,$9F,$89,$A0,$3D,$3F,$24
        .byte   $24,$16,$24,$1D,$24,$24,$24,$24
        .byte   $1B,$24,$19,$10,$00,$00,$00,$00
        .byte   $01,$02,$03,$04,$05,$06,$3B,$18
        .byte   $0A,$0B,$0C,$25,$25,$25,$25,$0D
        .byte   $24,$24,$62,$31,$31,$24,$31,$31
        .byte   $24,$24,$62,$24,$25,$25,$65,$37
        .byte   $31,$31,$25,$2D,$39,$25,$25,$37
        .byte   $33,$2F,$39,$37,$24,$25,$53,$25
        .byte   $25,$53,$25,$52,$25,$41,$41,$62
        .byte   $3B,$3B,$3B,$3B,$3B,$2D,$2D,$62

; =============================================================================
; TILE COLLISION ATTRIBUTE TABLE ($BF00-$BFFF)
; =============================================================================
; 1 byte per metatile index (256 entries). Upper nibble = collision type:
;   $00 = air (passthrough)      $10 = solid ground
;   $20 = ladder (climbable)     $30 = damage tile (lava/fire)
;   $40 = ladder top             $50 = spikes (instant kill)
; Lower 2 bits = palette index for the metatile.
; =============================================================================

        .byte   $00,$00,$00,$00,$01,$01,$02,$02
        .byte   $00,$00,$00,$00,$01,$01,$02,$02
        .byte   $00,$01,$02,$02,$02,$02,$01,$02
        .byte   $03,$02,$00,$02,$02,$02,$03,$00
        .byte   $03,$03,$00,$00,$00,$00,$00,$00
        .byte   $03,$03,$00,$00,$00,$00,$00,$00
        .byte   $01,$01,$02,$02,$02,$02,$02,$02
        .byte   $01,$01,$02,$02,$02,$02,$02,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$03,$03,$03,$03,$03
        .byte   $00,$03,$00,$03,$03,$03,$03,$03
        .byte   $00,$00,$00,$00,$02,$02,$02,$00
        .byte   $00,$00,$00,$03,$02,$02,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$02,$02,$02,$02,$02,$00
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $01,$01,$01,$01,$01,$01,$01,$01
        .byte   $00,$01,$01,$00,$00,$02,$02,$00
        .byte   $01,$01,$01,$01,$02,$02,$02,$02
        .byte   $03,$00,$00,$00,$02,$02,$02,$02
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$03,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
