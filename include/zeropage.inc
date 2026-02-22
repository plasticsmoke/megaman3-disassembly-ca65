; =============================================================================
; Zero Page Variable Definitions — Mega Man 3 (U)
; =============================================================================
; Addresses verified against xkas reference (bank1E_1F.asm).
; =============================================================================

; --- General-Purpose Temps ($00-$0F) ---
; Reused freely by any routine — no persistent meaning.
temp_00          = $00
temp_01          = $01
temp_02          = $02
temp_03          = $03
temp_04          = $04
temp_05          = $05
temp_06          = $06
temp_07          = $07
temp_08          = $08
temp_09          = $09
temp_0A          = $0A
temp_0B          = $0B
temp_0C          = $0C
temp_0D          = $0D
temp_0E          = $0E
temp_0F          = $0F

; --- Input ---
joy1_press       = $14   ; controller 1 new presses this frame
joy1_press_alt   = $15   ; controller 1 new presses (alternate read)
joy1_held        = $16   ; controller 1 held buttons

; --- Rendering / PPU ---
palette_dirty    = $18   ; palette update request flag
nametable_dirty  = $19   ; nametable update request flag
nt_column_dirty  = $1A   ; nametable column update flag

; --- Stage / World ---
stage_id         = $22   ; current stage index (see STAGE IDs in constants.inc)

; --- Player State ---
player_state     = $30   ; player state machine index (22 states)
player_facing    = $31   ; player facing direction (1=right, 2=left)
walk_flag        = $32   ; walking flag / sub-state
entity_ride_slot = $34   ; entity slot index being ridden (state $05)
facing_sub       = $35   ; facing sub-state / Mag Fly direction inherit
invincibility_timer = $39 ; i-frames timer (nonzero = immune to damage)
jump_counter     = $3A   ; jump / rush counter
hazard_pending   = $3D   ; pending hazard state ($06=damage, $0E=death)
tile_at_feet_max = $41   ; max tile type at feet (highest priority hazard)
tile_at_feet_lo  = $43   ; tile type at player feet (low)
tile_at_feet_hi  = $44   ; tile type at player feet (high)

; --- Scroll / Screen ---
scroll_lock      = $50   ; scroll lock flag
boss_active      = $5A   ; boss active flag (bit 7 = boss fight)
spark_freeze_a   = $5B   ; spark freeze slot A (entity X index)
spark_freeze_b   = $5C   ; spark freeze slot B (entity X index)
stage_select_page = $60  ; stage select page (0=Robot Master, else Doc Robot/Wily)
bosses_beaten    = $61   ; boss-defeated bitmask (bit N = stage N boss beaten)
proto_man_flag   = $68   ; Proto Man encounter flag
warp_dest        = $6C   ; warp destination index (teleporter target)
screen_mode      = $78   ; screen mode
scroll_x_fine    = $79   ; fine X scroll
nt_select        = $7A   ; nametable select
irq_scanline     = $7B   ; IRQ scanline target

; --- NMI / Timing ---
nmi_occurred     = $90   ; NMI occurred flag
frame_counter    = $92   ; frame counter
oam_ptr          = $97   ; OAM buffer pointer
gravity          = $99   ; gravity sub-pixel increment ($55 gameplay, $40 stage select)
irq_enable       = $9B   ; IRQ enable flag

; --- Weapon / Inventory Block ($A0-$AF) ---
current_weapon   = $A0   ; current weapon ID (see WPN_ constants)
weapon_cursor    = $A1   ; weapon menu cursor position
player_hp        = $A2   ; player HP (AND #$1F for value, 28 max)
weapon_ammo      = $A3   ; weapon ammo base (indexed by weapon ID: $A3+wpn)
lives            = $AE   ; lives remaining
etanks           = $AF   ; E-Tanks

; --- Boss HP ---
boss_hp_display  = $B0   ; boss HP bar display position

; --- Sound Driver Workspace ($C0-$CF) ---
; Used exclusively by the sound driver (bank $16).
snd_flags        = $C0   ; driver flags (bit 0=music active, bit 1=SFX mute)
snd_ptr_lo       = $C1   ; sound data pointer low / multiply result
snd_ptr_hi       = $C2   ; sound data pointer high / multiply result
snd_temp         = $C3   ; sound driver temp / command argument
snd_param        = $C4   ; sound driver param / multiply input
snd_inst_lo      = $C5   ; instrument table pointer low
snd_inst_hi      = $C6   ; instrument table pointer high
snd_tempo_ticks  = $C7   ; tempo tick count (decremented per frame)
snd_tempo_accum  = $C8   ; tempo accumulator (fractional)
snd_tempo_lo     = $C9   ; tempo increment low
snd_tempo_hi     = $CA   ; tempo increment high
snd_pitch_offset = $CB   ; master pitch transpose offset
snd_fade_rate    = $CC   ; volume fade rate (signed, bit 7=direction)
snd_fade_level   = $CD   ; volume fade target level
snd_priority     = $CE   ; current sound priority level
snd_channel_mask = $CF   ; channel enable bitmask

; --- System ---
nmi_skip         = $EE   ; NMI skip flag
sprite_slot      = $EF   ; current sprite slot counter (process_sprites loop var)
mmc3_shadow      = $F0   ; MMC3 bank select register shadow
mmc3_select      = $F4   ; MMC3 bank select value
prg_bank         = $F5   ; current $A000-$BFFF PRG bank number
game_mode        = $F8   ; game mode / screen state
camera_screen    = $F9   ; camera/scroll page
scroll_y         = $FA   ; vertical scroll position
camera_x_lo      = $FC   ; camera X position (low byte)
camera_x_hi      = $FD   ; camera X position (high byte)
ppu_mask_shadow  = $FE   ; PPU mask ($2001) shadow
ppu_ctrl_shadow  = $FF   ; PPU control ($2000) shadow
