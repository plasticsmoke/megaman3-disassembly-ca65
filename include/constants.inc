; =============================================================================
; Game Constants — Mega Man 3 (U)
; =============================================================================
; Addresses and values verified against xkas reference (bank1E_1F.asm).
; =============================================================================

; =============================================================================
; Entity Arrays (indexed by X or Y, stride $20, 32 slots $00-$1F)
; =============================================================================
; Slot $00 = Mega Man (player)
; Slots $01-$0F = weapons / projectiles
; Slots $10-$1F = enemies / items / bosses
; =============================================================================
ent_status      = $0300  ; entity active flag (bit 7 = active)
ent_routine     = $0320  ; AI routine index (dispatched in process_sprites)
ent_x_sub       = $0340  ; X position sub-pixel
ent_x_px        = $0360  ; X position pixel
ent_x_scr       = $0380  ; X position screen/page
ent_y_sub       = $03A0  ; Y position sub-pixel
ent_y_px        = $03C0  ; Y position pixel
ent_y_scr       = $03E0  ; Y position screen/page
ent_xvel_sub    = $0400  ; X velocity sub-pixel
ent_xvel        = $0420  ; X velocity pixel (signed)
ent_yvel_sub    = $0440  ; Y velocity sub-pixel
ent_yvel        = $0460  ; Y velocity pixel (signed)
ent_hitbox      = $0480  ; shape/hitbox (bit 7 = contact damage)
ent_facing      = $04A0  ; facing direction (FACING_RIGHT, FACING_LEFT)
ent_spawn_id    = $04C0  ; stage enemy ID (spawn tracking)
ent_hp          = $04E0  ; health / hit points
ent_timer       = $0500  ; AI timer / general counter
ent_var1        = $0520  ; general purpose field 1
ent_var2        = $0540  ; general purpose field 2
ent_var3        = $0560  ; general purpose field 3
ent_flags       = $0580  ; bit 7=active, bit 6=H-flip, bit 4=world coords
ent_anim_state  = $05A0  ; animation state (0=reset)
ent_anim_id     = $05C0  ; current animation / OAM ID
ent_anim_frame  = $05E0  ; animation frame counter

; =============================================================================
; Stage IDs (stored in stage_id / $22)
; =============================================================================
STAGE_NEEDLE     = $00   ; Needle Man
STAGE_MAGNET     = $01   ; Magnet Man
STAGE_GEMINI     = $02   ; Gemini Man
STAGE_HARD       = $03   ; Hard Man
STAGE_TOP        = $04   ; Top Man
STAGE_SNAKE      = $05   ; Snake Man
STAGE_SPARK      = $06   ; Spark Man
STAGE_SHADOW     = $07   ; Shadow Man
STAGE_DOC_NEEDLE = $08   ; Doc Robot — Needle Man stage
STAGE_DOC_GEMINI = $09   ; Doc Robot — Gemini Man stage
STAGE_DOC_SPARK  = $0A   ; Doc Robot — Spark Man stage
STAGE_DOC_SHADOW = $0B   ; Doc Robot — Shadow Man stage
STAGE_WILY1      = $0C   ; Wily Castle 1
STAGE_WILY2      = $0D   ; Wily Castle 2
STAGE_WILY3      = $0E   ; Wily Castle 3
STAGE_WILY4      = $0F   ; Wily Castle 4
STAGE_WILY5      = $10   ; Wily Castle 5
STAGE_WILY6      = $11   ; Wily Castle 6

; =============================================================================
; Weapon IDs (stored in current_weapon / $A0)
; =============================================================================
WPN_BUSTER       = $00   ; Mega Buster (arm cannon)
WPN_GEMINI       = $01   ; Gemini Laser
WPN_NEEDLE       = $02   ; Needle Cannon
WPN_HARD         = $03   ; Hard Knuckle
WPN_MAGNET       = $04   ; Magnet Missile
WPN_TOP          = $05   ; Top Spin
WPN_SNAKE        = $06   ; Search Snake
WPN_RUSH_COIL    = $07   ; Rush Coil
WPN_SPARK        = $08   ; Spark Shock
WPN_RUSH_MARINE  = $09   ; Rush Marine
WPN_SHADOW       = $0A   ; Shadow Blade
WPN_RUSH_JET     = $0B   ; Rush Jet

; =============================================================================
; Player States (stored in player_state / $30)
; =============================================================================
PSTATE_GROUND       = $00 ; idle/walking on ground
PSTATE_AIRBORNE     = $01 ; jumping/falling
PSTATE_SLIDE        = $02 ; sliding on ground
PSTATE_LADDER       = $03 ; on ladder
PSTATE_REAPPEAR     = $04 ; respawn/reappear
PSTATE_ENTITY_RIDE  = $05 ; riding Mag Fly
PSTATE_DAMAGE       = $06 ; taking damage
PSTATE_SPECIAL_DEATH = $07 ; Doc Flash Time Stopper kill
PSTATE_RUSH_MARINE  = $08 ; riding Rush Marine
PSTATE_BOSS_WAIT    = $09 ; frozen during boss intro
PSTATE_TOP_SPIN     = $0A ; Top Spin recoil bounce
PSTATE_WEAPON_RECOIL = $0B ; Hard Knuckle fire freeze
PSTATE_VICTORY      = $0C ; boss defeated cutscene
PSTATE_TELEPORT     = $0D ; teleport away
PSTATE_DEATH        = $0E ; dead, explosion
PSTATE_STUNNED      = $0F ; frozen by external force
PSTATE_SCREEN_SCROLL = $10 ; vertical scroll transition
PSTATE_WARP_INIT    = $11 ; teleporter initialization
PSTATE_WARP_ANIM    = $12 ; warp animation
PSTATE_TELEPORT_BEAM = $13 ; Proto Man exit beam
PSTATE_AUTO_WALK    = $14 ; scripted walk (post-Wily)
PSTATE_AUTO_WALK2   = $15 ; scripted walk (ending)

; =============================================================================
; Button Masks (for joy1_press / $14, joy1_held / $16)
; =============================================================================
BTN_A            = $80
BTN_B            = $40
BTN_SELECT       = $20
BTN_START        = $10
BTN_UP           = $08
BTN_DOWN         = $04
BTN_LEFT         = $02
BTN_RIGHT        = $01

; =============================================================================
; Tile Collision Types (upper nibble of $BF00,y attribute table)
; =============================================================================
TILE_AIR         = $00   ; passthrough, no collision
TILE_SOLID       = $10   ; solid ground
TILE_LADDER      = $20   ; climbable ladder
TILE_DAMAGE      = $30   ; damage tile (lava/fire)
TILE_LADDER_TOP  = $40   ; ladder top (grab point from above)
TILE_SPIKES      = $50   ; instant kill spikes
TILE_DISAPPEAR   = $70   ; disappearing block (Gemini stages)

; =============================================================================
; Music IDs (via submit_sound_ID_D9 — checks $D9 to avoid restart)
; =============================================================================
; Stage music is loaded from the LCD0C table indexed by stage_id.
; Other music IDs are loaded directly.
;
; Composers (source: VGMPF, Mega Man Wiki):
;   Harumi Fujita ("Mrs. Tarumi") — Needle Man, Gemini Man, Staff Roll (partial)
;   Yasuaki Fujita ("Bun Bun") — all remaining tracks + Staff Roll completion
;   (Despite sharing a surname, the two composers are unrelated.)
; =============================================================================
MUSIC_TITLE         = $00   ; title screen / intro theme  [Y. Fujita]
MUSIC_NEEDLE        = $01   ; Needle Man stage  [H. Fujita]
MUSIC_MAGNET        = $02   ; Magnet Man stage  [Y. Fujita]
MUSIC_GEMINI        = $03   ; Gemini Man stage  [H. Fujita]
MUSIC_HARD          = $04   ; Hard Man stage  [Y. Fujita]
MUSIC_TOP           = $05   ; Top Man stage  [Y. Fujita]
MUSIC_SNAKE         = $06   ; Snake Man stage  [Y. Fujita]
MUSIC_SPARK         = $07   ; Spark Man stage  [Y. Fujita]
MUSIC_SHADOW        = $08   ; Shadow Man stage  [Y. Fujita]
MUSIC_WILY_1        = $09   ; Wily Fortress stages 1-2  [Y. Fujita]
MUSIC_WILY_2        = $0A   ; Wily Fortress stages 3-4  [Y. Fujita]
MUSIC_WILY_3        = $0B   ; Wily Fortress stages 5-6  [Y. Fujita]
MUSIC_WILY_MAP      = $0C   ; Wily fortress gate / map  [Y. Fujita]
MUSIC_BOSS          = $0D   ; Boss battle  [Y. Fujita]
MUSIC_PASSWORD      = $0E   ; Password screen  [Y. Fujita]
MUSIC_CONTINUE      = $0F   ; Continue screen (after game over)  [Y. Fujita]
MUSIC_STAGE_SELECT  = $10   ; Stage select menu  [Y. Fujita]
MUSIC_PROTO_WHISTLE = $11   ; Staff Roll / Proto Man whistle  [H. & Y. Fujita]
MUSIC_GAME_OVER     = $12   ; Ending / game over  [Y. Fujita]
MUSIC_STAGE_START   = $33   ; Stage intro fanfare  [Y. Fujita]
MUSIC_BOSS_INTRO    = $35   ; Boss intro sequence  [Y. Fujita]
MUSIC_DOC_ROBOT     = $36   ; Doc Robot / Proto Man theme  [Y. Fujita]
MUSIC_WILY_VICTORY  = $37   ; Wily defeat victory  [Y. Fujita]
MUSIC_VICTORY       = $38   ; Stage clear  [Y. Fujita]

; =============================================================================
; Sound Effect IDs (via submit_sound_ID — plays immediately)
; =============================================================================
SFX_LAND            = $13   ; Player landing
SFX_1UP             = $14   ; 1-up / E-tank pickup
SFX_BUSTER          = $15   ; Mega Buster fire (default weapon sound)
SFX_PLAYER_HIT      = $16   ; Player takes damage
SFX_DEATH           = $17   ; Player / boss death
SFX_ENEMY_HIT       = $18   ; Weapon hits enemy
SFX_TINK            = $19   ; Weapon bounces off (immune)
SFX_PAUSE           = $1A   ; Pause menu open
SFX_CURSOR          = $1B   ; Menu cursor move
SFX_HP_FILL         = $1C   ; HP bar refill tick
SFX_DOOR            = $1D   ; Stage door / shutter
SFX_SHOT            = $1E   ; Projectile fired
SFX_LAND_ALT        = $1F   ; Landing (alternate)
SFX_STOMP           = $20   ; Heavy footstep
                            ; $21 unused (no SFX references in code)
SFX_CLAMP           = $22   ; Clamp snap (Wanaan)
SFX_DEBRIS          = $23   ; Debris / object break
SFX_ATTACK          = $24   ; Enemy attack trigger
SFX_TURRET_FIRE     = $25   ; Turret projectile fire (Wily Machine)
SFX_APPROACH        = $26   ; Entity approach trigger
SFX_PLATFORM        = $27   ; Platform detach / item spawn
SFX_WIND            = $28   ; Wind effect (intro)
                            ; $29 unused (no SFX references in code)
SFX_MAGNET_FIRE     = $2A   ; Magnet Missile fire
SFX_GEMINI_FIRE     = $2B   ; Gemini Laser fire
SFX_TOP_SPIN        = $2C   ; Top Spin fire
SFX_SPARK_FIRE      = $2D   ; Spark Shock fire
SFX_SHADOW_FIRE     = $2E   ; Shadow Blade fire
                            ; $2F unused (no SFX references in code)
SFX_HARD_STOMP      = $30   ; Hard Man stomp attack
SFX_WEAPON_GET      = $31   ; Weapon acquired jingle
SFX_EXPLOSION       = $32   ; Explosion burst
                            ; $33 = MUSIC_STAGE_START (defined above)
SFX_TELEPORT        = $34   ; Teleport beam

; =============================================================================
; Entity Facing Direction (stored in ent_facing / $04A0,x)
; =============================================================================
FACING_RIGHT     = $01   ; facing right (bit 0 set)
FACING_LEFT      = $02   ; facing left (bit 1 set)

; =============================================================================
; Entity Flags Bitmask (for ent_flags / $0580,x)
; =============================================================================
ENT_FLAG_HFLIP   = $40   ; bit 6 — horizontal flip (mirror sprite)

; =============================================================================
; Health / Energy Bar
; =============================================================================
HEALTH_FULL      = $9C   ; full HP / weapon energy (28 units × 4 sub-units)

; =============================================================================
; Sound Commands
; =============================================================================
SNDCMD_INIT         = $F0   ; Silence / reset all channels
SNDCMD_STOP         = $F2   ; Stop current music
