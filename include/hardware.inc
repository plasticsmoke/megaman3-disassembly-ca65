; =============================================================================
; NES Hardware Register Definitions
; =============================================================================

; PPU Registers
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
OAMDATA   = $2004
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007

; OAM DMA
OAMDMA    = $4014

; APU Registers
SQ1_VOL   = $4000
SQ1_SWEEP = $4001
SQ1_LO    = $4002
SQ1_HI    = $4003
SQ2_VOL   = $4004
SQ2_SWEEP = $4005
SQ2_LO    = $4006
SQ2_HI    = $4007
TRI_LINEAR = $4008
TRI_LO    = $400A
TRI_HI    = $400B
NOISE_VOL = $400C
NOISE_LO  = $400E
NOISE_HI  = $400F
DMC_FREQ  = $4010
DMC_RAW   = $4011
DMC_START = $4012
DMC_LEN   = $4013
SND_CHN   = $4015
APU_FRAME = $4017

; Controller Registers
JOY1      = $4016
JOY2      = $4017

; MMC3 (Mapper 4) Registers
MMC3_BANK_SELECT = $8000
MMC3_BANK_DATA   = $8001
MMC3_MIRRORING   = $A000
MMC3_PRG_RAM     = $A001
MMC3_IRQ_LATCH   = $C000
MMC3_IRQ_RELOAD  = $C001
MMC3_IRQ_DISABLE = $E000
MMC3_IRQ_ENABLE  = $E001
