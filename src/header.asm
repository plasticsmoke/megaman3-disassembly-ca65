.segment "HEADER"

; iNES header for Mega Man 3 (U)
; Mapper 4 (MMC3), 256KB PRG, 128KB CHR
.byte "NES", $1A                        ; Magic number
.byte $10                               ; 16 x 16KB PRG ROM = 256KB
.byte $10                               ; 16 x 8KB CHR ROM = 128KB
.byte $40                               ; Flags 6: mapper low nibble (4), vertical mirroring
.byte $00                               ; Flags 7: mapper high nibble (0)
.byte $00, $00, $00, $00, $00, $00, $00, $00 ; Padding
