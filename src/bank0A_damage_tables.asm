; =============================================================================
; MEGA MAN 3 (U) — BANK $0A — WEAPON DAMAGE TABLES + DOC ROBOT SPARK STAGE
; =============================================================================
; Mapped to $A000-$BFFF. Dual-purpose bank:
;
; 1) WEAPON DAMAGE DATA ($A000-$A9FF):
;   Loaded explicitly by check_player_hit and check_weapon_hit in bank1C_1D
;   via hardcoded `LDA #$0A / STA prg_bank`. Contains 10 damage tables,
;   each 256 bytes, indexed by entity main routine ID (ent_routine,x):
;     $A000: contact_damage_table  — base/contact damage (read via LDA $A000,y)
;     $A100: buster_damage_table   — Mega Buster (weapon_id $00)
;     $A200: needle_damage_table   — Needle Cannon (weapon_id $02)
;     $A300: magnet_damage_table   — Magnet Missile (weapon_id $04)
;     $A400: gemini_damage_table   — Gemini Laser (weapon_id $01)
;     $A500: hard_knuckle_damage_table — Hard Knuckle (weapon_id $03)
;     $A600: top_spin_damage_table — Top Spin (weapon_id $05)
;     $A700: snake_damage_table    — Search Snake (weapon_id $06)
;     $A800: spark_damage_table    — Spark Shock (weapon_id $08)
;     $A900: shadow_damage_table   — Shadow Blade (weapon_id $0A)
;   Weapon -> table mapping via weapon_damage_ptr_lo/hi in bank1C_1D.
;   Rush Coil/Marine/Jet use buster_damage_table (no unique damage).
;
;   DAMAGE TABLE INDEX — entity main routine ID (ent_routine,x):
;     Regular enemies ($02-$57):
;       $02=Dada  $03=Potton  $05=New Shotman  $06=Hammer Joe  $07=Peterchy
;       $08=Bubukan  $0A=Bomb Flier/PenPen  $0D=Yambow  $0E=Met
;       $12=Cannon  $14=Cloud Platform  $15/$16=Jamacy  $1A=Mag Fly
;       $1E=Gyoraibo  $1F=Junk Golem  $20=Pickelman Bull  $21=Bikky
;       $23=Magnet Force  $25=Nitron  $2A=Hari Harry  $2B=PenPen Maker
;       $2C=Returning Monking  $2E=Have Su Bee  $2F=Beehive
;       $30=Bolton & Nutton  $32=Wanaan  $33=Needle Press
;       $34=Walking Bomb  $35=Elecn  $37=Mechakkero  $38=Top Man Platform
;       $3B=Chibee  $3E=Spark Falling Platform  $43=Pole  $47=Komasaburo
;       $49=Parasyu  $4A/$4B=Hologran  $4C=Bomber Pepe  $4D=Metall DX
;       $4E=Petit Snakey  $4F=Tama (init)  $52=Proto Man
;     Robot Masters ($A0-$D7, at end of each table):
;       $A0=Doc Flash  $A1=Doc Wood  $A2=Doc Crash  $A3=Doc Metal
;       $B0=Doc Bubble  $B1=Doc Heat  $B2=Doc Quick  $B3=Doc Air
;       $C0=Needle Man  $C1=Magnet Man  $C2=Top Man  $C3=Shadow Man
;       $D0=Hard Man  $D2=Spark Man  $D4=Snake Man  $D6=Gemini Man
;     Fortress bosses ($E0-$F0):
;       $E0=Yellow Devil  $E3=Wily Machine A  $E5=Wily Machine B
;       $E7=Gamma B  $EA=Gamma F  $ED=Wily Machine C  $F0=Kamegoro Maker
;     Index $00=inactive, $01/$09/$0B/$0C=internal/projectile, $00 damage.
;
; 2) STAGE DATA ($AA00-$BFFF):
;   Doc Robot Spark Man stage (stage_id $22 = bank $0A).
;   Features Metal Man & Quick Man Doc Robot encounters.
;   Standard stage data layout:
;     $AA00: screen order table (26 screens + terminator)
;     $AA20: screen attribute/palette flags
;     $AA30: per-screen enemy spawn offset table
;     $AA40: screen connection data
;     $AA80: palette data (8 palettes x 4 colors x 2 sets = 64 bytes)
;     $AAA8: scrolling/screen configuration flags
;     $AB00: enemy spawn screen numbers ($FF-terminated)
;     $AC00: enemy spawn X positions
;     $AD00: enemy spawn Y positions
;     $AE00: enemy global IDs (index into bank $00 enemy data tables)
;     $AF00: metatile column definitions (64 bytes per column ID)
;     $B700: metatile CHR tile definitions (4 bytes per metatile: 2x2 patterns)
;     $BB00: metatile CHR definitions (continued)
;     $BF00: collision attribute table (256 bytes, upper nibble = collision type)
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"


.segment "BANK0A"

; =============================================================================
; CONTACT DAMAGE TABLE ($A000-$A0FF)
; =============================================================================
; Damage dealt to Mega Man on contact with each enemy type.
; Indexed by entity main routine ID (ent_routine,x).
; Value = number of HP bars of damage (each bar = 1 unit).
; $00 = no contact damage (invulnerable enemies, projectiles, platforms, etc.)
; Read by check_player_hit in bank1C_1D via LDA $A000,y.
; ===========================================================================
; --- regular enemies ($00-$57) ---
        .byte   $00,$00,$02,$02,$04,$04,$04,$04 ; $00: inactive, -, Dada, Potton, -, New Shotman, Hammer Joe, Peterchy
        .byte   $04,$02,$02,$04,$02,$03,$04,$02 ; $08: Bubukan, -, Bomb Flier, -, -, Yambow, Met, -
        .byte   $00,$00,$06,$02,$00,$02,$02,$02 ; $10: -, -, Cannon, -, Cloud Plat, Jamacy A, Jamacy B, -
        .byte   $02,$00,$04,$02,$00,$03,$06,$04 ; $18: -, -, Mag Fly, -, -, -, Gyoraibo, Junk Golem
        .byte   $06,$08,$00,$00,$04,$04,$02,$01 ; $20: Pickelman Bull, Bikky, -, Magnet Force, -, Nitron, -, -
        .byte   $06,$02,$06,$08,$03,$02,$06,$02 ; $28: Gyoraibo, -, Hari Harry, PenPen Maker, Ret Monking, -, Have Su Bee, Beehive
        .byte   $02,$00,$04,$04,$04,$04,$02,$03 ; $30: Bolton/Nutton, -, Wanaan, Needle Press, Walking Bomb, Elecn, -, Mechakkero
        .byte   $00,$06,$00,$03,$08,$04,$00,$00 ; $38: Top Man Plat, -, -, Chibee, -, Bomb Flier, Spark Plat, -
        .byte   $03,$02,$02,$02,$00,$02,$04,$06 ; $40: -, -, -, Pole, -, -, -, Komasaburo
        .byte   $00,$04,$03,$03,$06,$04,$04,$00 ; $48: -, Parasyu, Hologran, Hologran, Bomber Pepe, Metall DX, Petit Snakey, Tama
        .byte   $03,$04,$04,$04,$04,$06,$04,$00 ; $50: -, -, Proto Man, -, -, -, -, -
; --- robot master intros ($58-$5F), items/misc ($60-$7F) ---
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58: RM intros (no contact damage)
        .byte   $00,$00,$08,$00,$00,$00,$00,$00 ; $60: -, -, -, -, items...
        .byte   $00,$00,$00,$00,$08,$04,$00,$00 ; $68: items, Surprise Box, -, Junk Block, -, Spinning Wheel, -, Trap Plat
        .byte   $00,$00,$00,$06,$00,$02,$00,$00 ; $70: Proto Man (Gemini), -, Giant Springer, Breakable Wall, -, -, -, -
        .byte   $04,$04,$00,$00,$00,$00,$00,$00 ; $78: Electric Gabyoall x2, -, -, -, -, -, -
; --- weapon entities ($80-$9F) ---
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80: internal weapon entities
        .byte   $00,$08,$08,$00,$00,$03,$02,$02 ; $88: -, Big Snakey, Tama A, -, Tama B, -, -, -
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90: Doc Robot intros
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98: Doc Robot intros (cont)
; --- Doc Robot bosses ($A0-$BF) ---
        .byte   $04,$08,$04,$06,$02,$04,$04,$04 ; $A0: Doc Flash=4, Doc Wood=8, Doc Crash=4, Doc Metal=6
        .byte   $03,$08,$08,$03,$02,$00,$04,$03 ; $A8: (sub-entries)
        .byte   $04,$04,$08,$08,$04,$04,$02,$04 ; $B0: Doc Bubble=4, Doc Heat=4, Doc Quick=8, Doc Air=8
        .byte   $04,$06,$04,$00,$00,$00,$00,$00 ; $B8: (sub-entries)
; --- Robot Masters ($C0-$D7) ---
        .byte   $04,$06,$06,$04,$03,$04,$04,$06 ; $C0: Needle=4, Magnet=6, Top=6, Shadow=4
        .byte   $06,$00,$00,$00,$00,$00,$00,$00 ; $C8: (sub-entries)
        .byte   $06,$03,$04,$00,$06,$00,$06,$06 ; $D0: Hard=6, -, Spark=4, -, Snake=6, -, Gemini=6
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8: (sub-entries)
; --- Fortress bosses ($E0-$FF) ---
        .byte   $08,$04,$04,$08,$04,$04,$04,$00 ; $E0: Yellow Devil=8, -, -, Wily Mach A=8
        .byte   $10,$08,$00,$00,$00,$04,$00,$00 ; $E8: (cont), -, Gamma F, -, -, Wily Mach C
        .byte   $08,$04,$04,$00,$00,$06,$00,$00 ; $F0: Kamegoro Maker=8
        .byte   $00,$00,$00,$00,$08,$00,$00,$00 ; $F8: (unused)
; =============================================================================
; MEGA BUSTER DAMAGE TABLE ($A100-$A1FF) — weapon_id $00
; =============================================================================
; Buster pellet damage per entity. $01 = standard 1 damage, $00 = immune.
; Most regular enemies take 1; bosses take 1-2, some immune ($00).
; ===========================================================================
buster_damage_table:
        .byte   $00,$00,$01,$01,$01,$01,$01,$01 ; $00-$07: most enemies take 1
        .byte   $01,$01,$01,$01,$00,$01,$01,$00 ; $08-$0F
        .byte   $00,$00,$01,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$01,$00,$00,$01,$01,$01 ; $18-$1F
        .byte   $01,$01,$00,$00,$01,$01,$00,$01 ; $20-$27
        .byte   $01,$00,$01,$01,$01,$00,$01,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$01,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$01 ; $40-$47
        .byte   $00,$01,$01,$01,$01,$01,$01,$00 ; $48-$4F
        .byte   $00,$00,$01,$01,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F: RM intros immune
        .byte   $00,$00,$01,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$01,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$01,$01,$00,$00,$01,$01,$00 ; $88-$8F: Big Snakey=1, Tama=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots: 1-2 damage ---
        .byte   $01,$01,$02,$01,$00,$00,$00,$00 ; $A0: Doc Flash=1, Doc Wood=1, Doc Crash=2, Doc Metal=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $01,$01,$01,$02,$00,$00,$00,$00 ; $B0: Doc Bubble=1, Doc Heat=1, Doc Quick=1, Doc Air=2
        .byte   $00,$00,$01,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters: 1-2 damage ---
        .byte   $01,$02,$02,$01,$00,$01,$00,$01 ; $C0: Needle=1, Magnet=2, Top=2, Shadow=1
        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $01,$00,$01,$00,$01,$00,$01,$01 ; $D0: Hard=1, Spark=1, Snake=1, Gemini=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $01,$00,$00,$01,$00,$01,$00,$00 ; $E0: Yellow Devil=1, Wily Mach A=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$01,$01,$00,$00,$01,$00,$00 ; $F0: Kamegoro=1
        .byte   $00,$00,$00,$00,$01,$00,$00,$00 ; $F8
; =============================================================================
; NEEDLE CANNON DAMAGE TABLE ($A200-$A2FF) — weapon_id $02
; =============================================================================
; Same as buster for regular enemies. Effective vs Doc Crash (weakness).
; ===========================================================================
needle_damage_table:
        .byte   $00,$00,$01,$01,$01,$01,$01,$01 ; $00-$07
        .byte   $01,$01,$01,$01,$00,$01,$01,$00 ; $08-$0F
        .byte   $00,$00,$01,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$01,$00,$00,$01,$01,$01 ; $18-$1F
        .byte   $01,$01,$00,$00,$01,$01,$00,$01 ; $20-$27
        .byte   $01,$00,$01,$01,$01,$00,$01,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$01,$00,$01 ; $30-$37
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$01 ; $40-$47
        .byte   $00,$01,$01,$01,$01,$01,$01,$00 ; $48-$4F
        .byte   $00,$00,$01,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$01,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$01,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$01,$01,$00,$00,$01,$01,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $04,$04,$02,$00,$00,$00,$00,$00 ; $A0: Doc Flash=4, Doc Wood=4, Doc Crash=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $02,$02,$01,$01,$00,$00,$00,$00 ; $B0: Doc Bubble=2, Doc Heat=2, Doc Quick=1, Doc Air=1
        .byte   $00,$00,$01,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $04,$01,$02,$01,$00,$01,$00,$04 ; $C0: Needle=4, Magnet=1, Top=2, Shadow=1, Gemini(sub)=4
        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $00,$00,$02,$00,$04,$00,$01,$01 ; $D0: Hard=0, Spark=2, Snake=4, Gemini=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $01,$00,$00,$01,$00,$00,$00,$00 ; $E0: Yellow Devil=1, Wily Mach A=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$01,$01,$00,$00,$01,$00,$00 ; $F0: Kamegoro=1
        .byte   $00,$00,$00,$00,$01,$00,$00,$00 ; $F8
; =============================================================================
; MAGNET MISSILE DAMAGE TABLE ($A300-$A3FF) — weapon_id $04
; =============================================================================
; Homing missile. Strong vs Hard Man (weakness: 4), Doc Metal (4).
; ===========================================================================
magnet_damage_table:
        .byte   $00,$00,$01,$01,$01,$02,$04,$02 ; $00-$07
        .byte   $00,$01,$03,$02,$00,$03,$01,$00 ; $08-$0F
        .byte   $00,$00,$03,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$00,$00,$00,$03,$02,$02 ; $18-$1F
        .byte   $02,$00,$00,$00,$02,$01,$00,$01 ; $20-$27
        .byte   $02,$00,$02,$02,$02,$01,$01,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$00,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$02 ; $40-$47
        .byte   $00,$03,$02,$02,$02,$01,$02,$00 ; $48-$4F
        .byte   $00,$00,$02,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$02,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$02,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$02,$00,$01,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$02,$02,$00,$00,$02,$01,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $01,$00,$01,$04,$00,$00,$00,$00 ; $A0: Doc Flash=1, Doc Wood=0, Doc Crash=1, Doc Metal=4 (weakness)
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $00,$01,$02,$04,$00,$00,$00,$00 ; $B0: Doc Bubble=0, Doc Heat=1, Doc Quick=2, Doc Air=4
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $01,$04,$01,$01,$00,$00,$00,$01 ; $C0: Needle=1, Magnet=4, Top=1, Shadow=1
        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $04,$00,$00,$00,$00,$00,$02,$02 ; $D0: Hard=4 (weakness), Spark=0, Snake=0, Gemini=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $01,$00,$00,$02,$00,$01,$00,$00 ; $E0: Yellow Devil=1, Wily Mach A=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$01,$01,$00,$00,$00,$00,$00 ; $F0: Kamegoro=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $F8
; =============================================================================
; GEMINI LASER DAMAGE TABLE ($A400-$A4FF) — weapon_id $01
; =============================================================================
; Bouncing laser. Strong vs Needle Man (weakness: 7), Doc Quick (4).
; ===========================================================================
gemini_damage_table:
        .byte   $00,$00,$01,$01,$01,$02,$04,$02 ; $00-$07
        .byte   $02,$01,$03,$02,$00,$03,$01,$00 ; $08-$0F
        .byte   $00,$00,$02,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$01,$00,$00,$03,$02,$02 ; $18-$1F
        .byte   $06,$00,$00,$00,$02,$01,$00,$01 ; $20-$27
        .byte   $02,$00,$06,$00,$02,$01,$03,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$01,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$06 ; $40-$47
        .byte   $00,$03,$02,$02,$02,$01,$01,$00 ; $48-$4F
        .byte   $00,$00,$01,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$02,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$02,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$04,$00,$01,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$02,$02,$00,$00,$02,$01,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $04,$01,$00,$01,$00,$00,$00,$00 ; $A0: Doc Flash=4, Doc Wood=1, Doc Crash=0, Doc Metal=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $01,$01,$04,$02,$00,$00,$00,$00 ; $B0: Doc Bubble=1, Doc Heat=1, Doc Quick=4, Doc Air=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $07,$02,$00,$01,$00,$00,$00,$07 ; $C0: Needle=7 (weakness!), Magnet=2, Top=0, Shadow=1
        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $01,$00,$01,$00,$01,$00,$04,$04 ; $D0: Hard=1, Spark=1, Snake=1, Gemini=4
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $01,$00,$00,$01,$00,$00,$00,$00 ; $E0: Yellow Devil=1, Wily Mach A=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$01,$01,$00,$00,$01,$00,$00 ; $F0: Kamegoro=1
        .byte   $00,$00,$00,$00,$02,$00,$00,$00 ; $F8
; =============================================================================
; HARD KNUCKLE DAMAGE TABLE ($A500-$A5FF) — weapon_id $03
; =============================================================================
; Slow fist projectile. Strong vs Top Man (weakness: 7), Doc Crash (7).
; Also effective vs Shadow Man (7), Yellow Devil (5).
; ===========================================================================
hard_knuckle_damage_table:
        .byte   $00,$00,$01,$01,$01,$03,$08,$03 ; $00-$07
        .byte   $00,$01,$03,$02,$00,$03,$01,$00 ; $08-$0F
        .byte   $00,$00,$03,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$01,$00,$00,$03,$02,$02 ; $18-$1F
        .byte   $06,$06,$00,$00,$02,$01,$00,$01 ; $20-$27
        .byte   $02,$00,$02,$04,$08,$01,$03,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$01,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$02 ; $40-$47
        .byte   $00,$03,$03,$03,$06,$01,$02,$00 ; $48-$4F
        .byte   $00,$00,$04,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$04,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$04,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$02,$00,$01,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$05,$03,$00,$00,$02,$01,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $00,$02,$07,$04,$00,$00,$00,$00 ; $A0: Doc Flash=0, Doc Wood=2, Doc Crash=7 (weakness), Doc Metal=4
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $02,$02,$01,$02,$00,$00,$00,$00 ; $B0: Doc Bubble=2, Doc Heat=2, Doc Quick=1, Doc Air=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $00,$02,$07,$02,$00,$00,$00,$00 ; $C0: Needle=0, Magnet=2, Top=7 (weakness), Shadow=2
        .byte   $02,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $04,$00,$02,$00,$02,$00,$02,$02 ; $D0: Hard=4, Spark=2, Snake=2, Gemini=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $05,$00,$00,$07,$00,$07,$00,$00 ; $E0: Yellow Devil=5, Wily Mach A=7, Wily Mach B=7
        .byte   $04,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$06,$01,$00,$00,$04,$00,$00 ; $F0: Kamegoro=1, Gamma F=6
        .byte   $00,$00,$00,$00,$03,$00,$00,$00 ; $F8
; =============================================================================
; TOP SPIN DAMAGE TABLE ($A600-$A6FF) — weapon_id $05
; =============================================================================
; Contact-based spinning attack. Strong vs Shadow Man (weakness: 7),
; Doc Heat (7). Also effective vs Snake Man (7).
; Many entries $00 — Top Spin only works on contact, many enemies immune.
; ===========================================================================
top_spin_damage_table:
        .byte   $00,$00,$01,$01,$01,$03,$00,$03 ; $00-$07
        .byte   $04,$01,$03,$06,$00,$03,$01,$00 ; $08-$0F
        .byte   $00,$00,$00,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$01,$00,$00,$00,$02,$06 ; $18-$1F
        .byte   $00,$00,$00,$00,$06,$00,$00,$00 ; $20-$27
        .byte   $02,$00,$06,$00,$08,$01,$00,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$00,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$00,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$06 ; $40-$47
        .byte   $00,$03,$00,$00,$00,$01,$00,$00 ; $48-$4F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$00,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $70-$77
        .byte   $01,$01,$00,$00,$00,$00,$00,$00 ; $78-$7F: Gabyoall=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $01,$02,$04,$00,$00,$00,$00,$00 ; $A0: Doc Flash=1, Doc Wood=2, Doc Crash=4, Doc Metal=0
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $01,$07,$01,$01,$00,$00,$00,$00 ; $B0: Doc Bubble=1, Doc Heat=7 (weakness), Doc Quick=1, Doc Air=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $01,$01,$04,$07,$00,$00,$00,$01 ; $C0: Needle=1, Magnet=1, Top=4, Shadow=7 (weakness!)
        .byte   $07,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $00,$00,$01,$00,$02,$00,$02,$02 ; $D0: Hard=0, Spark=1, Snake=2, Gemini=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $E0: Yellow Devil=0 (immune)
        .byte   $00,$02,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$06,$01,$00,$00,$07,$00,$00 ; $F0: Kamegoro=1, Gamma F=6, Gamma=7
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $F8
; =============================================================================
; SEARCH SNAKE DAMAGE TABLE ($A700-$A7FF) — weapon_id $06
; =============================================================================
; Ground-crawling snake. Strong vs Gemini Man (weakness: 5), Doc Wood (4),
; Doc Quick (4). Also effective vs Spark Man (4).
; ===========================================================================
snake_damage_table:
        .byte   $00,$00,$01,$01,$01,$01,$00,$01 ; $00-$07
        .byte   $01,$01,$03,$01,$00,$00,$01,$00 ; $08-$0F
        .byte   $00,$00,$03,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$00,$00,$00,$03,$02,$01 ; $18-$1F
        .byte   $00,$00,$00,$00,$01,$00,$00,$01 ; $20-$27
        .byte   $02,$00,$01,$01,$02,$01,$00,$00 ; $28-$2F
        .byte   $00,$00,$00,$00,$01,$01,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$00,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$01 ; $40-$47
        .byte   $00,$00,$03,$03,$02,$01,$02,$00 ; $48-$4F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$00,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$02,$00,$00,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$01,$01,$00,$00,$02,$01,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $01,$04,$01,$00,$00,$00,$00,$00 ; $A0: Doc Flash=1, Doc Wood=4, Doc Crash=1, Doc Metal=0
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $00,$01,$04,$01,$00,$00,$00,$00 ; $B0: Doc Bubble=0, Doc Heat=1, Doc Quick=4, Doc Air=1
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $01,$01,$01,$01,$00,$00,$00,$01 ; $C0: Needle=1, Magnet=1, Top=1, Shadow=1
        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $00,$00,$00,$00,$04,$00,$05,$05 ; $D0: Hard=0, Spark=0, Snake=4, Gemini=5 (weakness)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $00,$00,$00,$04,$00,$04,$00,$00 ; $E0: Yellow Devil=0, Wily Mach A=4, Wily Mach B=4
        .byte   $00,$02,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$06,$01,$00,$00,$07,$00,$00 ; $F0: Kamegoro=1, Gamma F=6, Gamma=7
        .byte   $00,$00,$00,$00,$03,$00,$00,$00 ; $F8

; =============================================================================
; SPARK SHOCK DAMAGE TABLE ($A800-$A8FF) — weapon_id $08
; =============================================================================
; Electric stun weapon. $58 = freeze effect (stuns enemy, no damage).
; For regular enemies: $58 = freezes, $00 = immune to freeze.
; For bosses ($A0+): uses normal damage values instead of $58.
; Strong vs Magnet Man (weakness: 7), Doc Bubble (4), Doc Air (4).
; ===========================================================================
spark_damage_table:
        .byte   $00,$00,$58,$58,$58,$58,$00,$58 ; $00-$07: $58=freeze
        .byte   $58,$58,$58,$58,$00,$58,$00,$00 ; $08-$0F
        .byte   $00,$00,$00,$00,$00,$58,$58,$58 ; $10-$17
        .byte   $58,$00,$00,$00,$00,$00,$00,$58 ; $18-$1F
        .byte   $00,$58,$00,$00,$58,$58,$00,$58 ; $20-$27
        .byte   $00,$00,$58,$00,$58,$58,$58,$00 ; $28-$2F
        .byte   $58,$00,$58,$58,$58,$00,$58,$58 ; $30-$37
        .byte   $00,$00,$00,$58,$00,$00,$00,$00 ; $38-$3F
        .byte   $00,$00,$58,$00,$00,$00,$00,$58 ; $40-$47
        .byte   $00,$58,$00,$00,$00,$00,$00,$00 ; $48-$4F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$00,$00,$58,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots (real damage, not freeze) ---
        .byte   $00,$01,$01,$02,$00,$00,$00,$00 ; $A0: Doc Flash=0, Doc Wood=1, Doc Crash=1, Doc Metal=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $A8
        .byte   $04,$01,$01,$04,$00,$00,$00,$00 ; $B0: Doc Bubble=4, Doc Heat=1, Doc Quick=1, Doc Air=4
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters (real damage, not freeze) ---
        .byte   $00,$07,$01,$01,$00,$00,$00,$00 ; $C0: Needle=0, Magnet=7 (weakness!), Top=1, Shadow=1
        .byte   $01,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $00,$02,$04,$00,$01,$00,$01,$01 ; $D0: Hard=0, Spark=2, Snake=4(?), Gemini=0(verify in Mesen)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $00,$00,$00,$00,$00,$07,$00,$00 ; $E0: Yellow Devil=0, Wily Mach B=7
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $00,$58,$00,$00,$00,$00,$00,$00 ; $F0: Kamegoro=freeze($58)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $F8
; =============================================================================
; SHADOW BLADE DAMAGE TABLE ($A900-$A9FF) — weapon_id $0A
; =============================================================================
; Throwing star. Strong vs Spark Man (weakness: 4), Doc Flash (weakness: 7).
; Good all-around weapon — deals 1-4 damage to most bosses.
; ===========================================================================
shadow_damage_table:
        .byte   $00,$00,$01,$01,$01,$03,$02,$02 ; $00-$07
        .byte   $04,$01,$03,$02,$00,$03,$01,$00 ; $08-$0F
        .byte   $00,$00,$03,$01,$00,$01,$01,$01 ; $10-$17
        .byte   $01,$00,$01,$00,$00,$03,$02,$02 ; $18-$1F
        .byte   $02,$02,$00,$00,$02,$01,$00,$01 ; $20-$27
        .byte   $02,$00,$02,$02,$02,$01,$03,$00 ; $28-$2F
        .byte   $01,$00,$00,$00,$01,$01,$01,$01 ; $30-$37
        .byte   $00,$00,$00,$01,$00,$01,$00,$00 ; $38-$3F
        .byte   $00,$00,$01,$00,$00,$00,$01,$02 ; $40-$47
        .byte   $00,$03,$03,$03,$02,$01,$02,$00 ; $48-$4F
        .byte   $00,$00,$02,$00,$00,$00,$00,$00 ; $50-$57
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $58-$5F
        .byte   $00,$00,$02,$00,$00,$00,$00,$00 ; $60-$67
        .byte   $00,$00,$00,$00,$02,$01,$00,$00 ; $68-$6F
        .byte   $00,$00,$00,$02,$00,$01,$00,$00 ; $70-$77
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $78-$7F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $80-$87
        .byte   $00,$03,$02,$00,$00,$02,$01,$00 ; $88-$8F
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $90-$97
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $98-$9F
; --- Doc Robots ---
        .byte   $02,$02,$01,$02,$00,$00,$00,$00 ; $A0: Doc Flash=2, Doc Wood=2, Doc Crash=1, Doc Metal=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$01 ; $A8
        .byte   $04,$04,$02,$00,$00,$00,$00,$00 ; $B0: Doc Bubble=4, Doc Heat=4, Doc Quick=2, Doc Air=0
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $B8
; --- Robot Masters ---
        .byte   $02,$07,$01,$04,$00,$00,$00,$02 ; $C0: Needle=2, Magnet=7, Top=1, Shadow=4
        .byte   $04,$00,$00,$00,$00,$00,$00,$00 ; $C8
        .byte   $00,$02,$04,$00,$02,$00,$02,$02 ; $D0: Hard=0, Spark=4 (weakness), Snake=2, Gemini=2
        .byte   $00,$00,$00,$00,$00,$00,$00,$00 ; $D8
; --- Fortress bosses ---
        .byte   $03,$00,$00,$02,$00,$02,$00,$00 ; $E0: Yellow Devil=3, Wily Mach A=2, Wily Mach B=2
        .byte   $02,$00,$00,$00,$00,$00,$00,$00 ; $E8
        .byte   $01,$03,$01,$00,$00,$02,$00,$00 ; $F0: Kamegoro=1, Gamma F=3
        .byte   $00,$00,$00,$00,$02,$00,$00,$00 ; $F8

; =============================================================================
; DOC ROBOT SPARK MAN STAGE DATA ($AA00-$BFFF)
; =============================================================================
; Stage layout data for the Doc Robot version of Spark Man's stage.
; stage_id $22 = bank $0A. Features Metal Man & Quick Man Doc Robots.
;
; Standard stage data format:
;   $AA00-$AA19: screen order table (26 entries, index -> screen ID)
;   $AA1A-$AA1F: unused/padding
;   $AA20-$AA2F: screen attribute/palette flags (1 byte per screen)
;   $AA30-$AA3F: enemy spawn base index per screen
;   $AA40-$AA4F: screen connection bytes (scroll direction/linking)
;   $AA50-$AA7F: additional screen configuration data
;   $AA80-$AAAF: palette data (BG + sprite palettes, 2 sets)
;   $AAA8-$AAFF: scrolling/screen configuration flags
; ===========================================================================

; --- screen order table ($AA00) ---
; 26 screens: $00-$19, sequential (typical for linear stages)
        .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte   $10,$11,$12,$13,$14,$15,$16,$17
        .byte   $18,$19,$00,$00,$00,$00,$00,$10
; --- screen configuration data ($AA20-$AA7F) ---
; Per-screen flags: scroll direction, palette set, connection info.
        .byte   $00,$20,$00,$60,$00,$08,$00,$00
        .byte   $00,$85,$00,$00,$00,$00,$00,$00
        .byte   $09,$66,$66,$4E,$00,$00,$80,$00
        .byte   $17,$54,$54,$08,$00,$80,$00,$00
        .byte   $80,$80,$A3,$80,$A2,$20,$20,$20
        .byte   $40,$40,$40,$64,$22,$20,$20,$00
        .byte   $00,$00,$02,$00,$00,$60,$00,$00
        .byte   $80,$00,$00,$00,$00,$08,$00,$00
        .byte   $08,$00,$06,$00,$39,$00,$39,$00
        .byte   $13,$00,$00,$00,$0F,$00,$00,$00
        .byte   $00,$01,$00,$01,$00,$01,$03,$01
        .byte   $32,$01,$00,$00,$0F,$00,$00,$00
; --- palette data ($AA80-$AAAF) ---
; BG palettes (4 sets x 4 colors) + sprite palettes (4 sets x 4 colors).
; Two complete palette sets for the stage.
        .byte   $54,$56,$0F,$20,$32,$02,$0F,$20 ; BG palette set 1
        .byte   $32,$26,$0F,$20,$32,$0F,$0F,$1B
        .byte   $1B,$0F,$87,$88,$89,$00,$0F,$20 ; sprite palettes + BG set 2
        .byte   $32,$02,$0F,$20,$32,$26,$0F,$20
        .byte   $32,$0F,$0F,$20,$10,$16,$87,$88
        .byte   $00,$00,$08,$00,$00,$00,$00,$10
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$02,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$02,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$01,$00,$00
        .byte   $0B,$0D,$00,$01,$00,$00,$00,$00
        .byte   $10,$18,$FF,$00,$00,$00,$00,$00
; ===========================================================================
; --- enemy spawn screen numbers ($AB00, $FF-terminated) ---
; Each byte = screen number where an enemy appears. 37 entries + $FF terminator.
; Indexes correspond 1:1 with $AC00 (X), $AD00 (Y), $AE00 (global ID).
        .byte   $00,$01,$03,$03,$03,$03,$04,$04
        .byte   $04,$04,$05,$05,$05,$05,$06,$06
        .byte   $06,$07,$08,$0B,$0C,$11,$11,$11
        .byte   $12,$12,$12,$12,$13,$13,$14,$14
        .byte   $14,$15,$16,$17,$19,$FF,$20,$00
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$08,$00,$00,$00,$00
        .byte   $00,$40,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$00,$20,$00
        .byte   $02,$00,$00,$00,$00,$80,$00,$00
        .byte   $00,$00,$00,$10,$00,$00,$00,$01
        .byte   $00,$00,$00,$01,$00,$01,$00,$09
        .byte   $20,$01,$00,$08,$00,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$80,$00,$02
        .byte   $00,$00,$00,$00,$80,$00,$00,$00
        .byte   $20,$00,$00,$00,$00,$00,$02,$03
        .byte   $00,$C0,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $15,$24,$20,$05,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$80,$20,$00
        .byte   $00,$00,$80,$08,$00,$00,$00,$00
        .byte   $80,$00,$00,$20,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; --- enemy spawn X positions ($AC00) ---
; X pixel coordinate for each enemy spawn entry.
        .byte   $38,$38,$50,$78,$90,$E8,$10,$58
        .byte   $90,$E8,$10,$28,$68,$A8,$38,$78
        .byte   $A8,$C8,$F8,$C0,$58,$18,$28,$A8
        .byte   $48,$78,$B8,$F8,$88,$B8,$58,$68
        .byte   $E8,$B0,$B0,$30,$C8,$FF,$00,$00
        .byte   $08,$04,$00,$00,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$C0,$00,$80,$00,$08,$00
        .byte   $00,$00,$04,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $80,$00,$00,$00,$00,$00,$40,$00
        .byte   $20,$00,$08,$00,$21,$00,$00,$00
        .byte   $02,$00,$01,$00,$00,$00,$00,$00
        .byte   $00,$00,$10,$40,$00,$00,$00,$04
        .byte   $02,$00,$40,$00,$01,$00,$02,$01
        .byte   $00,$00,$0C,$00,$00,$41,$00,$00
        .byte   $40,$00,$01,$00,$00,$00,$00,$00
        .byte   $12,$00,$00,$00,$04,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $04,$00,$10,$00,$00,$40,$00,$00
        .byte   $00,$00,$10,$00,$00,$01,$03,$00
        .byte   $00,$00,$00,$00,$80,$01,$00,$00
        .byte   $00,$00,$00,$40,$00,$00,$01,$01
        .byte   $10,$14,$00,$00,$00,$00,$00,$50
        .byte   $00,$00,$80,$00,$00,$04,$00,$00
        .byte   $00,$40,$81,$01,$00,$00,$00,$00
        .byte   $00,$00,$01,$00,$80,$00,$00,$00
        .byte   $00,$00,$08,$41,$00,$14,$24,$00
        .byte   $00,$00,$00,$05,$00,$00,$01,$00
        .byte   $01,$44,$00,$00,$00,$00,$90,$00
        .byte   $08,$00,$00,$00,$00,$00,$18,$50
; --- enemy spawn Y positions ($AD00) ---
; Y pixel coordinate for each enemy spawn entry.
        .byte   $58,$70,$48,$98,$2C,$78,$2C,$68
        .byte   $2C,$88,$2C,$98,$78,$88,$88,$78
        .byte   $88,$B0,$90,$B0,$98,$70,$70,$88
        .byte   $80,$78,$70,$78,$80,$78,$78,$70
        .byte   $58,$40,$40,$40,$B0,$FF,$00,$00
        .byte   $03,$00,$81,$00,$00,$40,$00,$00
        .byte   $80,$00,$20,$10,$00,$00,$00,$00
        .byte   $08,$00,$00,$00,$08,$04,$08,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $24,$00,$00,$00,$0C,$00,$01,$04
        .byte   $00,$00,$00,$00,$00,$01,$01,$00
        .byte   $00,$00,$02,$00,$00,$00,$00,$00
        .byte   $40,$00,$00,$00,$00,$00,$03,$10
        .byte   $32,$00,$40,$00,$00,$00,$22,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$A0,$00,$80,$00
        .byte   $02,$00,$00,$41,$08,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$04,$20,$00,$24,$00,$00,$00
        .byte   $01,$00,$40,$00,$00,$00,$00,$04
        .byte   $00,$01,$08,$00,$00,$00,$04,$00
        .byte   $00,$10,$00,$00,$02,$00,$00,$00
        .byte   $08,$00,$00,$00,$00,$00,$01,$00
        .byte   $00,$01,$20,$00,$08,$40,$40,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$04
        .byte   $08,$00,$04,$04,$02,$00,$00,$00
        .byte   $00,$00,$00,$00,$10,$00,$81,$00
        .byte   $00,$00,$10,$04,$04,$04,$00,$00
        .byte   $10,$00,$20,$44,$08,$04,$20,$00
        .byte   $83,$00,$80,$00,$00,$41,$08,$00
        .byte   $10,$00,$01,$00,$20,$80,$0C,$00
; --- enemy global IDs ($AE00) ---
; Global enemy ID for each spawn entry. Indexes into bank $00 tables
; (enemy_flags_g, enemy_main_ID_g, enemy_OAM_ID_g, enemy_HP_g, etc.)
        .byte   $36,$15,$50,$59,$24,$59,$24,$58
        .byte   $24,$59,$24,$59,$58,$59,$59,$58
        .byte   $59,$5C,$5C,$6E,$50,$20,$64,$20
        .byte   $64,$20,$20,$20,$65,$20,$20,$64
        .byte   $20,$63,$63,$63,$6A,$FF,$00,$10
        .byte   $00,$00,$00,$14,$20,$00,$00,$00
        .byte   $40,$00,$00,$00,$00,$01,$00,$00
        .byte   $00,$00,$00,$00,$01,$00,$20,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$50
        .byte   $00,$00,$80,$01,$00,$00,$80,$11
        .byte   $00,$00,$00,$00,$08,$04,$00,$10
        .byte   $00,$40,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$10,$08,$00
        .byte   $00,$00,$08,$40,$02,$00,$04,$20
        .byte   $00,$00,$12,$41,$10,$00,$00,$00
        .byte   $04,$00,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$01,$01,$40,$40,$00,$00
        .byte   $00,$01,$01,$00,$00,$00,$08,$00
        .byte   $00,$00,$00,$00,$00,$40,$40,$00
        .byte   $00,$04,$01,$00,$00,$00,$00,$00
        .byte   $0D,$40,$00,$10,$00,$00,$80,$04
        .byte   $02,$10,$00,$04,$00,$01,$00,$10
        .byte   $00,$00,$00,$00,$08,$00,$08,$00
        .byte   $00,$00,$08,$00,$00,$00,$04,$40
        .byte   $00,$00,$00,$00,$00,$04,$00,$00
        .byte   $00,$00,$C0,$04,$00,$00,$80,$00
        .byte   $00,$00,$00,$00,$20,$00,$00,$00
        .byte   $00,$00,$00,$04,$00,$00,$00,$00
        .byte   $02,$00,$90,$01,$04,$00,$08,$00
        .byte   $00,$10,$20,$00,$00,$40,$00,$00
        .byte   $24,$00,$00,$00,$C0,$00,$00,$00
        .byte   $00,$00,$28,$00,$01,$00,$41,$10
; ===========================================================================
; --- metatile column definitions ($AF00-$B6FF) ---
; 64 bytes per column ID. Each column defines 16 rows of 4 metatiles
; (32 tiles tall x 2 metatiles wide = one screen-width column).
; Column IDs referenced by the screen layout data above.
        .byte   $00,$01,$02,$03,$03,$03,$03,$03
        .byte   $00,$04,$05,$06,$07,$07,$07,$08
        .byte   $00,$09,$0A,$0B,$0C,$0D,$0E,$0F
        .byte   $10,$11,$11,$11,$11,$12,$0E,$0F
        .byte   $00,$13,$14,$0E,$0E,$15,$0E,$0F
        .byte   $00,$16,$0A,$0B,$0C,$0D,$0E,$0F
        .byte   $17,$18,$18,$18,$18,$18,$18,$19
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $1A,$01,$1B,$1C,$1C,$1C,$1C,$1C
        .byte   $1A,$1D,$1E,$1F,$20,$20,$20,$20
        .byte   $1A,$21,$22,$1C,$1C,$1C,$1C,$1C
        .byte   $1A,$23,$24,$1F,$20,$20,$20,$20
        .byte   $1A,$1D,$1E,$1C,$1C,$1C,$1C,$1C
        .byte   $1A,$21,$22,$1F,$20,$20,$20,$20
        .byte   $1A,$23,$24,$1C,$1C,$1C,$1C,$1C
        .byte   $1A,$01,$1B,$1C,$1C,$1C,$1C,$1C
        .byte   $03,$03,$03,$03,$03,$03,$03,$25
        .byte   $03,$03,$03,$03,$03,$03,$03,$00
        .byte   $25,$07,$07,$26,$07,$07,$07,$27
        .byte   $00,$28,$29,$2A,$2B,$2C,$2B,$13
        .byte   $00,$2D,$2E,$2F,$30,$31,$30,$16
        .byte   $00,$0E,$32,$18,$18,$18,$18,$33
        .byte   $00,$34,$02,$03,$03,$03,$03,$17
        .byte   $00,$01,$02,$03,$03,$03,$03,$03
        .byte   $07,$07,$07,$07,$35,$07,$07,$07
        .byte   $0E,$36,$36,$37,$38,$0E,$0E,$36
        .byte   $0E,$39,$3A,$3B,$28,$29,$3C,$3D
        .byte   $0E,$3E,$3E,$3F,$40,$41,$42,$3E
        .byte   $0E,$0E,$15,$43,$44,$0E,$45,$46
        .byte   $47,$0C,$0D,$0E,$0E,$48,$0E,$0E
        .byte   $18,$33,$0E,$49,$18,$4A,$33,$0E
        .byte   $03,$00,$0E,$0F,$03,$03,$00,$0E
        .byte   $35,$07,$07,$07,$35,$07,$07,$07
        .byte   $38,$0E,$37,$0E,$38,$0E,$36,$0E
        .byte   $0E,$4B,$4C,$4D,$0E,$0E,$39,$36
        .byte   $4B,$4E,$4F,$4C,$4D,$0E,$50,$39
        .byte   $51,$52,$53,$54,$49,$33,$0E,$50
        .byte   $49,$55,$56,$57,$0F,$58,$59,$5A
        .byte   $0F,$03,$58,$5B,$5C,$03,$03,$12
        .byte   $0F,$03,$03,$03,$03,$03,$03,$12
        .byte   $35,$07,$07,$07,$07,$5D,$5E,$0F
        .byte   $38,$0E,$36,$0E,$0E,$5F,$01,$0F
        .byte   $0E,$36,$39,$4B,$60,$60,$01,$0F
        .byte   $0E,$39,$4E,$61,$4D,$0E,$01,$0F
        .byte   $0E,$3E,$09,$0A,$54,$62,$0E,$0F
        .byte   $0E,$0E,$0E,$0E,$57,$0E,$0E,$0F
        .byte   $0E,$63,$18,$18,$18,$18,$18,$19
        .byte   $0E,$63,$03,$03,$03,$03,$03,$03
        .byte   $1A,$64,$65,$65,$65,$65,$65,$66
        .byte   $1A,$67,$0E,$4B,$0E,$36,$36,$68
        .byte   $1A,$69,$6A,$6B,$6C,$3D,$3E,$68
        .byte   $1A,$6D,$6E,$6F,$70,$71,$0E,$68
        .byte   $1A,$57,$0E,$43,$0E,$50,$0E,$68
        .byte   $72,$73,$74,$74,$74,$75,$76,$68
        .byte   $1C,$1C,$1C,$1C,$1C,$1C,$77,$68
        .byte   $1C,$1C,$1C,$1C,$1C,$1C,$77,$68
        .byte   $78,$79,$78,$79,$78,$25,$07,$07
        .byte   $25,$07,$7A,$07,$07,$27,$0E,$0E
        .byte   $00,$13,$2B,$13,$2B,$13,$0E,$7B
        .byte   $00,$7C,$7D,$7C,$7D,$7C,$2E,$7E
        .byte   $00,$16,$30,$16,$30,$16,$0E,$15
        .byte   $00,$7C,$7D,$7C,$7D,$7C,$0C,$0D
        .byte   $00,$7F,$18,$18,$18,$18,$18,$18
        .byte   $00,$80,$03,$03,$03,$03,$03,$03
        .byte   $08,$03,$03,$25,$07,$07,$07,$81
        .byte   $0F,$03,$03,$00,$0E,$0E,$0E,$82
        .byte   $0F,$03,$03,$00,$0E,$7B,$4B,$36
        .byte   $0F,$03,$03,$00,$41,$83,$84,$3E
        .byte   $0F,$03,$03,$00,$0E,$85,$86,$0E
        .byte   $87,$88,$88,$89,$0E,$8A,$8B,$49
        .byte   $18,$18,$18,$18,$18,$18,$18,$19
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $07,$08,$03,$03,$03,$03,$03,$03
        .byte   $36,$0F,$03,$03,$03,$03,$8C,$07
        .byte   $3E,$0F,$03,$03,$03,$03,$8D,$8E
        .byte   $0E,$87,$88,$88,$88,$88,$8F,$90
        .byte   $18,$18,$18,$18,$18,$18,$18,$18
        .byte   $03,$03,$03,$79,$91,$91,$91,$78
        .byte   $03,$03,$03,$92,$93,$93,$93,$94
        .byte   $03,$79,$78,$79,$78,$79,$78,$03
        .byte   $03,$95,$96,$95,$96,$95,$96,$03
        .byte   $07,$97,$98,$97,$98,$97,$98,$07
        .byte   $99,$13,$2B,$2B,$2B,$13,$2B,$8E
        .byte   $9A,$7C,$7D,$7D,$7D,$7C,$7D,$90
        .byte   $18,$9B,$9C,$9B,$9C,$9B,$9C,$18
        .byte   $03,$95,$96,$95,$96,$95,$96,$03
        .byte   $03,$92,$94,$92,$94,$92,$94,$03
        .byte   $8C,$07,$07,$07,$07,$07,$07,$08
        .byte   $8D,$2B,$9D,$4C,$43,$43,$43,$0F
        .byte   $9E,$7D,$7C,$9F,$A0,$29,$A1,$0F
        .byte   $A2,$30,$16,$43,$44,$2F,$30,$82
        .byte   $9A,$7D,$7C,$0E,$0E,$A3,$7D,$8E
        .byte   $A4,$7D,$7C,$47,$0C,$A3,$7D,$90
        .byte   $A5,$18,$18,$18,$18,$18,$18,$18
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $8C,$07,$07,$A6,$0E,$A7,$0F,$03
        .byte   $8D,$2B,$2B,$A8,$0E,$A7,$0F,$03
        .byte   $A9,$7D,$7D,$A8,$0E,$A7,$0F,$03
        .byte   $AA,$30,$30,$A8,$30,$AB,$0F,$03
        .byte   $AC,$7D,$7D,$AD,$7D,$AE,$0F,$03
        .byte   $9A,$7D,$49,$33,$7D,$AE,$0F,$03
        .byte   $18,$18,$19,$00,$AF,$B0,$0F,$03
        .byte   $03,$03,$03,$00,$B1,$A7,$0F,$03
        .byte   $1C,$1C,$1C,$B2,$0E,$A7,$B3,$1C
        .byte   $1C,$1C,$1C,$B2,$30,$AB,$B3,$1C
        .byte   $20,$20,$B4,$B2,$B5,$B0,$B3,$1C
        .byte   $1C,$1C,$B6,$B7,$0E,$A7,$B3,$1C
        .byte   $20,$B4,$B2,$B1,$0E,$A7,$B3,$1C
        .byte   $1C,$1C,$B2,$B1,$0E,$B8,$B9,$1C
        .byte   $1C,$1C,$B2,$B1,$0E,$BA,$1C,$1C
        .byte   $1C,$1C,$B2,$B1,$0E,$BA,$1C,$1C
        .byte   $03,$03,$00,$B1,$0E,$BA,$03,$03
        .byte   $91,$78,$00,$B1,$0E,$BA,$BB,$91
        .byte   $93,$94,$00,$B1,$0E,$BA,$92,$93
        .byte   $03,$03,$BC,$B1,$0E,$BA,$03,$03
        .byte   $91,$78,$BD,$BE,$0E,$BA,$BB,$91
        .byte   $93,$94,$BD,$7D,$AE,$BF,$92,$93
        .byte   $03,$03,$BD,$B5,$B0,$0F,$03,$03
        .byte   $03,$03,$BD,$0E,$A7,$0F,$03,$03
        .byte   $1C,$1C,$BD,$0E,$A7,$B3,$1C,$1C
        .byte   $20,$B4,$BD,$0E,$A7,$B3,$C0,$20
        .byte   $1C,$1C,$C1,$0E,$A7,$B3,$1C,$1C
        .byte   $20,$B4,$BD,$0E,$C2,$B3,$C0,$20
        .byte   $1C,$1C,$C1,$0E,$C2,$B3,$1C,$1C
        .byte   $20,$B4,$C3,$0E,$0E,$B3,$C0,$20
        .byte   $1C,$1C,$C1,$0E,$C2,$B3,$1C,$1C
        .byte   $1C,$1C,$C3,$0E,$0E,$B3,$1C,$1C
        .byte   $03,$03,$C4,$C5,$C6,$0F,$03,$79
        .byte   $25,$07,$C7,$C8,$C9,$82,$CA,$97
        .byte   $00,$2B,$CB,$CC,$7D,$2C,$2B,$2B
        .byte   $00,$B5,$7D,$7D,$7D,$CD,$7D,$7D
        .byte   $00,$CE,$CF,$30,$30,$31,$D0,$CF
        .byte   $00,$0E,$D1,$7D,$7D,$CD,$D2,$D3
        .byte   $17,$18,$18,$18,$18,$18,$18,$18
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $78,$79,$78,$03,$03,$03,$79,$91
        .byte   $98,$97,$98,$07,$5D,$07,$97,$D4
        .byte   $2B,$13,$2B,$2B,$5F,$2B,$2B,$2B
        .byte   $7D,$7C,$D5,$D6,$2B,$7D,$D7,$B5
        .byte   $30,$16,$D8,$0E,$D9,$D9,$DA,$0E
        .byte   $DB,$49,$33,$0E,$0E,$0E,$A8,$0E
        .byte   $18,$19,$17,$18,$18,$18,$4A,$18
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $78,$03,$03,$03,$79,$78,$79,$78
        .byte   $98,$07,$5D,$07,$97,$98,$97,$98
        .byte   $13,$0E,$DC,$2A,$2B,$2B,$2B,$13
        .byte   $D7,$0E,$0E,$B5,$CC,$7D,$D5,$D7
        .byte   $0E,$0E,$0E,$CE,$CF,$30,$D8,$0E
        .byte   $0E,$0E,$DA,$0E,$49,$33,$DD,$0E
        .byte   $18,$18,$4A,$18,$19,$17,$18,$18
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$79,$78,$79,$78
        .byte   $DE,$03,$25,$07,$97,$98,$DF,$98
        .byte   $05,$07,$27,$0E,$2A,$2B,$E0,$0E
        .byte   $0E,$0E,$0E,$0E,$A3,$D7,$0E,$0E
        .byte   $0E,$DA,$0E,$0E,$D9,$DD,$0E,$49
        .byte   $0E,$A8,$0E,$E1,$DD,$0E,$49,$E2
        .byte   $18,$4A,$18,$18,$18,$18,$19,$92
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$79
        .byte   $07,$5D,$07,$07,$08,$E3,$07,$97
        .byte   $0E,$5F,$0E,$0E,$82,$E4,$0E,$2A
        .byte   $0E,$0E,$0E,$0E,$0E,$E0,$0E,$2F
        .byte   $33,$0E,$0E,$0E,$49,$18,$18,$18
        .byte   $E5,$18,$18,$18,$19,$79,$91,$78
        .byte   $94,$03,$03,$03,$03,$92,$93,$94
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $91,$78,$03,$03,$03,$35,$79,$91
        .byte   $D4,$98,$07,$07,$07,$35,$97,$D4
        .byte   $2B,$2B,$2C,$2B,$E6,$E7,$0E,$0E
        .byte   $30,$30,$31,$30,$D8,$0E,$0E,$0E
        .byte   $18,$33,$CD,$D7,$0E,$0E,$0E,$0E
        .byte   $03,$E8,$E9,$9C,$33,$EA,$48,$0E
        .byte   $03,$92,$93,$94,$00,$EA,$A8,$0E
        .byte   $03,$03,$03,$03,$00,$EA,$A8,$0E
        .byte   $78,$03,$03,$03,$03,$35,$03,$03
        .byte   $98,$07,$07,$07,$07,$35,$07,$07
        .byte   $0E,$0E,$2A,$2B,$E6,$E7,$2B,$2B
        .byte   $0E,$0E,$B5,$EB,$EC,$0E,$B5,$D5
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$ED
        .byte   $49,$18,$18,$18,$33,$EA,$0E,$48
        .byte   $0F,$03,$03,$03,$00,$EA,$0E,$A8
        .byte   $0F,$03,$03,$03,$00,$EA,$0E,$A8
        .byte   $03,$35,$03,$03,$03,$03,$03,$03
        .byte   $07,$35,$07,$07,$07,$07,$07,$08
        .byte   $2B,$E7,$EE,$2B,$13,$2B,$2B,$82
        .byte   $EF,$0E,$D3,$D6,$D7,$B5,$7D,$8E
        .byte   $0E,$0E,$0E,$0E,$0E,$CE,$CF,$F0
        .byte   $0E,$EA,$0E,$49,$18,$18,$18,$18
        .byte   $0E,$EA,$0E,$0F,$03,$03,$03,$03
        .byte   $0E,$EA,$0E,$0F,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $07,$07,$07,$07,$07,$07,$07,$07
        .byte   $99,$13,$2B,$2B,$2B,$13,$2B,$8E
        .byte   $F1,$16,$CF,$30,$30,$16,$F2,$F0
        .byte   $18,$18,$18,$18,$18,$18,$18,$18
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $8C,$07,$07,$07,$07,$07,$07,$DE
        .byte   $8D,$13,$0E,$0E,$4D,$4D,$2A,$F3
        .byte   $AA,$16,$52,$53,$F4,$F5,$2F,$F6
        .byte   $AC,$7C,$F7,$F8,$54,$F9,$A3,$FA
        .byte   $F1,$16,$0E,$0E,$FB,$57,$2F,$F6
        .byte   $A4,$7C,$47,$0C,$0D,$0E,$A3,$FA
        .byte   $A5,$18,$18,$18,$18,$18,$18,$FC
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
; ===========================================================================
; --- metatile CHR tile definitions ($B700-$BAFF) ---
; 4 bytes per metatile: top-left, top-right, bottom-left, bottom-right
; pattern tile IDs. Each metatile is a 2x2 arrangement of 8x8 CHR tiles.
        .byte   $1F,$2E,$17,$2A,$00,$07,$00,$07
        .byte   $00,$2C,$00,$28,$1F,$1E,$17,$16
        .byte   $00,$07,$A6,$07,$00,$2C,$00,$30
        .byte   $08,$09,$35,$31,$1F,$1E,$35,$31
        .byte   $1F,$1E,$3D,$16,$A2,$A3,$AA,$00
        .byte   $A3,$A4,$00,$AC,$A5,$B6,$00,$BE
        .byte   $B6,$B7,$BF,$BE,$A8,$A9,$00,$00
        .byte   $00,$00,$00,$00,$28,$1E,$2C,$16
        .byte   $1F,$3E,$17,$38,$21,$25,$35,$31
        .byte   $58,$00,$58,$00,$74,$75,$6C,$6D
        .byte   $00,$00,$00,$A7,$00,$00,$A0,$A1
        .byte   $64,$65,$6C,$6D,$1F,$3E,$17,$16
        .byte   $21,$25,$17,$16,$3B,$1E,$17,$16
        .byte   $17,$2A,$1F,$2E,$00,$28,$00,$2C
        .byte   $17,$16,$1F,$1E,$B0,$07,$B8,$07
        .byte   $B0,$28,$B8,$2C,$17,$40,$1F,$48
        .byte   $51,$50,$49,$48,$B7,$07,$B8,$07
        .byte   $B7,$28,$B8,$2C,$B1,$07,$00,$07
        .byte   $B1,$28,$00,$2C,$1F,$1E,$17,$38
        .byte   $0A,$0B,$35,$31,$1F,$2E,$35,$32
        .byte   $00,$A6,$A5,$A2,$00,$00,$A3,$A3
        .byte   $73,$74,$6B,$6C,$74,$74,$6C,$6C
        .byte   $75,$74,$6D,$6C,$A5,$A2,$00,$AA
        .byte   $A3,$A3,$00,$00,$63,$64,$6B,$6C
        .byte   $64,$64,$6C,$6C,$65,$64,$6D,$6C
        .byte   $00,$24,$00,$28,$21,$26,$17,$2A
        .byte   $00,$06,$00,$07,$14,$15,$14,$15
        .byte   $00,$00,$B0,$00,$00,$00,$00,$B0
        .byte   $1A,$1B,$00,$00,$B8,$00,$B8,$00
        .byte   $B8,$00,$25,$21,$00,$B8,$00,$B7
        .byte   $A7,$00,$A4,$B9,$B8,$00,$B7,$BD
        .byte   $B8,$00,$B1,$00,$00,$B7,$00,$B7
        .byte   $A5,$A2,$A5,$A2,$A3,$A3,$A3,$A3
        .byte   $A4,$00,$A4,$00,$00,$B8,$00,$B1
        .byte   $00,$AA,$00,$00,$AC,$00,$00,$00
        .byte   $00,$BC,$00,$BC,$B6,$B6,$BE,$BF
        .byte   $20,$26,$2C,$2A,$20,$25,$2C,$16
        .byte   $3B,$3E,$17,$16,$B0,$00,$B8,$00
        .byte   $00,$B8,$00,$B8,$00,$B0,$00,$B8
        .byte   $B1,$00,$A6,$00,$00,$B1,$00,$A7
        .byte   $B1,$00,$00,$00,$B7,$BA,$B7,$BA
        .byte   $A2,$A3,$A2,$A3,$A3,$A4,$A3,$A4
        .byte   $B9,$B7,$00,$B8,$21,$26,$17,$3A
        .byte   $00,$AC,$25,$22,$00,$B1,$00,$00
        .byte   $1F,$2E,$17,$3A,$66,$6E,$25,$21
        .byte   $00,$00,$58,$00,$6E,$6E,$25,$21
        .byte   $28,$1E,$3F,$16,$1F,$1E,$3D,$38
        .byte   $2A,$07,$36,$07,$28,$2E,$34,$32
        .byte   $00,$00,$25,$21,$B8,$00,$B1,$A7
        .byte   $BD,$00,$00,$00,$00,$59,$00,$59
        .byte   $07,$28,$07,$34,$17,$16,$31,$35
        .byte   $17,$16,$39,$1E,$07,$00,$07,$B0
        .byte   $2C,$16,$28,$1E,$00,$B8,$BC,$B7
        .byte   $00,$00,$BA,$A2,$B1,$00,$A3,$A3
        .byte   $00,$00,$A4,$B9,$BC,$B7,$00,$B8
        .byte   $BA,$A2,$00,$00,$A3,$A3,$00,$B0
        .byte   $A4,$B9,$00,$00,$B7,$BD,$B8,$00
        .byte   $17,$3A,$1F,$1E,$58,$00,$6E,$6E
        .byte   $00,$00,$6E,$6E,$00,$59,$6E,$6E
        .byte   $26,$06,$2A,$07,$2E,$07,$2A,$07
        .byte   $1F,$1E,$41,$16,$1F,$1E,$17,$40
        .byte   $0C,$0B,$35,$31,$00,$00,$A7,$00
        .byte   $6C,$6D,$6C,$6D,$6C,$6C,$6C,$6C
        .byte   $A4,$A5,$AC,$00,$06,$24,$07,$28
        .byte   $07,$2C,$07,$28,$1F,$1E,$39,$16
        .byte   $28,$1E,$34,$31,$A4,$B9,$A4,$B9
        .byte   $B7,$BD,$B7,$BD,$AC,$00,$B6,$B6
        .byte   $B8,$00,$B7,$B6,$30,$35,$00,$00
        .byte   $31,$35,$00,$00,$31,$36,$00,$00
        .byte   $BE,$BF,$BE,$BF,$BF,$BE,$BF,$BE
        .byte   $1F,$1E,$3C,$31,$2A,$74,$2E,$6C
        .byte   $74,$0F,$6C,$0F,$32,$6C,$73,$6C
        .byte   $6C,$0F,$6C,$0F,$1F,$1E,$51,$50
        .byte   $1F,$48,$17,$16,$49,$48,$17,$16
        .byte   $49,$1E,$17,$16,$1F,$48,$17,$40
        .byte   $49,$1E,$41,$16,$1F,$48,$35,$31
        .byte   $49,$1E,$35,$31,$02,$74,$03,$6C
        .byte   $03,$6C,$03,$6C,$21,$25,$17,$40
        .byte   $21,$25,$41,$16,$74,$74,$6C,$6D
        .byte   $2A,$6C,$36,$6C,$00,$B8,$B9,$B7
        .byte   $00,$A6,$BA,$A2,$A7,$00,$A4,$00
        .byte   $02,$64,$03,$6C,$6B,$6C,$6B,$6C
        .byte   $22,$6C,$2E,$6C,$3A,$25,$17,$16
        .byte   $1F,$2E,$3D,$2A,$00,$6E,$00,$66
        .byte   $28,$2E,$2C,$2A,$2A,$6C,$2E,$6C
        .byte   $2A,$64,$36,$6C,$64,$6E,$6C,$66
        .byte   $02,$6C,$03,$6C,$30,$36,$74,$74
        .byte   $6C,$6E,$6C,$66,$66,$6C,$6E,$67
        .byte   $6C,$6E,$67,$66,$66,$00,$6E,$00
        .byte   $17,$2E,$1F,$2A,$28,$16,$2C,$1E
        .byte   $41,$16,$49,$1E,$77,$6C,$00,$77
        .byte   $17,$16,$1F,$38,$17,$2E,$35,$32
        .byte   $66,$24,$6E,$28,$3B,$16,$1F,$1E
        .byte   $66,$2C,$6E,$28,$1F,$1E,$17,$50
        .byte   $1F,$2E,$3C,$32,$2A,$6E,$2E,$66
        .byte   $7E,$00,$6C,$7E,$20,$3F,$2C,$16
        .byte   $17,$50,$1F,$48,$2A,$00,$2E,$66
        .byte   $00,$6E,$00,$00,$2A,$00,$2E,$00
        .byte   $2A,$7E,$2E,$6C,$00,$00,$7E,$00
        .byte   $00,$00,$00,$7F,$2E,$6C,$36,$6C
        .byte   $6C,$7E,$6C,$6C,$7F,$6C,$6C,$6C
        .byte   $08,$0E,$35,$31,$74,$6C,$6C,$6C
        .byte   $76,$77,$7E,$7F,$6D,$6C,$6D,$6C
        .byte   $00,$7D,$00,$00,$64,$64,$77,$6C
        .byte   $64,$64,$6C,$76,$00,$77,$00,$7D
        .byte   $6C,$7C,$76,$00,$00,$77,$00,$00
        .byte   $49,$48,$35,$31,$6C,$6C,$6C,$76
        .byte   $6C,$6C,$76,$77,$6C,$76,$76,$00
        .byte   $64,$7C,$76,$00,$77,$76,$00,$00
        .byte   $00,$00,$24,$22,$76,$77,$00,$00
        .byte   $34,$36,$00,$00,$C0,$00,$00,$00
        .byte   $1F,$1E,$35,$39,$1F,$48,$3D,$38
        .byte   $30,$36,$00,$00,$00,$C7,$00,$00
        .byte   $3B,$1E,$17,$40,$1F,$1E,$17,$3C
        .byte   $1F,$2E,$3D,$2E,$1F,$3E,$41,$16
        .byte   $74,$74,$6C,$76,$1C,$1D,$00,$00
        .byte   $1F,$3E,$17,$40,$21,$25,$51,$50
        .byte   $12,$13,$10,$11,$6C,$6C,$76,$5E
        .byte   $76,$00,$00,$00,$5E,$00,$00,$00
        .byte   $74,$74,$77,$6C,$76,$76,$00,$00
        .byte   $64,$0F,$6C,$0F,$01,$64,$03,$6C
        .byte   $64,$64,$76,$77,$74,$2C,$6C,$28
        .byte   $B9,$B7,$B9,$B7,$BB,$B7,$BB,$B7
        .byte   $64,$2C,$6C,$28,$A2,$A3,$00,$00
        .byte   $A3,$A4,$00,$00,$BB,$B7,$00,$B8
        .byte   $6C,$2C,$6C,$28,$00,$B1,$A0,$A1
        .byte   $21,$3F,$17,$16,$58,$00,$00,$00
        .byte   $28,$2E,$30,$32,$1F,$1E,$17,$16
; ===========================================================================
; --- metatile CHR definitions continued ($BB00-$BEFF) ---
; Additional 2x2 pattern definitions + palette attribute data.
; Three sets of metatile graphics (mirrored/flipped variants).
; Each set covers 256 metatiles with $100 bytes of padding between sets.
        .byte   $00,$23,$00,$33,$04,$0E,$0E,$0E
        .byte   $27,$2A,$27,$29,$27,$27,$2D,$1E
        .byte   $06,$08,$84,$86,$0A,$0C,$40,$42
        .byte   $30,$20,$1A,$1C,$0A,$0C,$60,$62
        .byte   $44,$4D,$46,$44,$44,$4D,$46,$44
        .byte   $48,$00,$4A,$48,$48,$00,$4A,$48
        .byte   $64,$4E,$66,$00,$64,$4E,$66,$00
        .byte   $68,$4E,$4A,$7A,$68,$4E,$4A,$7A
        .byte   $80,$82,$88,$8A,$11,$13,$8C,$8E
        .byte   $A0,$A2,$A8,$AA,$10,$10,$AC,$AE
        .byte   $C1,$82,$06,$16,$08,$09,$00,$00
        .byte   $35,$00,$06,$06,$C5,$00,$9D,$00
        .byte   $44,$4D,$46,$22,$23,$23,$6C,$33
        .byte   $48,$00,$4A,$32,$33,$33,$6C,$08
        .byte   $64,$4E,$66,$00,$00,$00,$33,$9D
        .byte   $68,$4E,$4A,$7A,$8C,$00,$8C,$00
        .byte   $CC,$CE,$CA,$00,$00,$00,$00,$00
        .byte   $EC,$EE,$00,$00,$EB,$00,$00,$00
        .byte   $11,$11,$01,$01,$01,$01,$01,$01
        .byte   $C5,$01,$01,$01,$01,$01,$01,$01
        .byte   $00,$00,$AC,$C8,$C7,$C3,$00,$00
        .byte   $F1,$E3,$00,$01,$F7,$01,$01,$01
        .byte   $E5,$4C,$01,$01,$01,$01,$20,$8E
        .byte   $E0,$C3,$AD,$AD,$00,$AD,$AE,$B4
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$23,$00,$33,$05,$0F,$0F,$0F
        .byte   $28,$2F,$2A,$2F,$2B,$2C,$2E,$1F
        .byte   $07,$09,$85,$87,$0B,$0D,$41,$43
        .byte   $21,$31,$1B,$1D,$0B,$0D,$61,$63
        .byte   $45,$4D,$47,$47,$45,$4D,$47,$47
        .byte   $49,$00,$4B,$4B,$49,$00,$4B,$4B
        .byte   $65,$4F,$67,$00,$65,$4F,$67,$00
        .byte   $4F,$6B,$79,$49,$4F,$6B,$79,$49
        .byte   $81,$83,$89,$8B,$11,$10,$8D,$8F
        .byte   $A1,$A3,$A9,$AB,$10,$14,$AD,$AF
        .byte   $81,$C0,$07,$17,$08,$09,$00,$00
        .byte   $00,$36,$06,$06,$C6,$00,$9D,$00
        .byte   $45,$4D,$47,$23,$23,$24,$6D,$33
        .byte   $49,$00,$4B,$33,$33,$34,$6D,$08
        .byte   $65,$4F,$67,$00,$00,$00,$9D,$33
        .byte   $4F,$6B,$79,$49,$00,$8D,$00,$8D
        .byte   $CD,$CF,$CB,$00,$00,$00,$00,$00
        .byte   $ED,$EF,$00,$EA,$00,$00,$00,$00
        .byte   $12,$12,$01,$01,$01,$01,$01,$01
        .byte   $C6,$01,$01,$01,$01,$01,$01,$01
        .byte   $00,$00,$C7,$C9,$AD,$C3,$00,$00
        .byte   $E2,$E4,$F7,$01,$00,$01,$01,$01
        .byte   $E6,$5F,$01,$01,$01,$01,$21,$8F
        .byte   $C2,$AC,$C3,$AC,$AC,$00,$AF,$B5
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$33,$33,$33,$0E,$25,$0E,$0E
        .byte   $37,$3A,$37,$39,$37,$37,$3D,$1E
        .byte   $16,$18,$94,$96,$0A,$0C,$50,$52
        .byte   $30,$20,$00,$00,$1A,$1C,$70,$72
        .byte   $54,$5C,$56,$54,$54,$5C,$56,$54
        .byte   $48,$00,$5A,$48,$48,$00,$5A,$48
        .byte   $74,$5E,$76,$00,$74,$5E,$76,$00
        .byte   $5A,$6A,$78,$5C,$5A,$6A,$78,$5C
        .byte   $90,$92,$98,$9A,$10,$13,$9C,$9E
        .byte   $B0,$B2,$B8,$BA,$12,$10,$BC,$BE
        .byte   $D1,$92,$06,$16,$18,$19,$00,$00
        .byte   $35,$00,$06,$06,$D5,$00,$00,$9C
        .byte   $01,$01,$01,$32,$33,$33,$7C,$15
        .byte   $01,$00,$01,$32,$33,$33,$7C,$18
        .byte   $01,$01,$01,$32,$33,$33,$8C,$00
        .byte   $01,$01,$01,$01,$8C,$00,$33,$9C
        .byte   $DC,$DE,$DA,$00,$FB,$00,$E8,$F8
        .byte   $FC,$FE,$00,$00,$FB,$EA,$E9,$F9
        .byte   $D5,$13,$01,$01,$01,$01,$01,$01
        .byte   $D5,$01,$01,$01,$01,$01,$01,$01
        .byte   $E1,$D3,$BC,$D8,$D7,$C3,$00,$E7
        .byte   $F0,$F3,$00,$01,$00,$01,$01,$01
        .byte   $58,$F5,$01,$01,$01,$01,$30,$9E
        .byte   $E0,$C3,$BD,$BD,$00,$BD,$BE,$B6
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$33,$33,$33,$0F,$26,$0F,$0F
        .byte   $38,$3F,$3A,$3F,$3B,$3C,$3E,$1F
        .byte   $17,$19,$95,$97,$0B,$0D,$51,$53
        .byte   $21,$31,$00,$00,$1B,$1D,$71,$73
        .byte   $55,$5D,$57,$57,$55,$5D,$57,$57
        .byte   $59,$00,$4B,$4B,$59,$00,$4B,$4B
        .byte   $75,$5E,$77,$00,$75,$5E,$77,$00
        .byte   $69,$59,$5D,$7B,$69,$59,$5D,$7B
        .byte   $91,$93,$99,$9B,$10,$10,$9D,$9F
        .byte   $B1,$B3,$B9,$BB,$12,$14,$BD,$BF
        .byte   $91,$D0,$07,$17,$18,$19,$00,$00
        .byte   $00,$36,$06,$7F,$D6,$00,$00,$9C
        .byte   $55,$5D,$57,$33,$33,$34,$7D,$15
        .byte   $59,$00,$4B,$33,$33,$34,$7D,$18
        .byte   $75,$5E,$77,$33,$33,$34,$00,$8D
        .byte   $69,$59,$5D,$7B,$00,$8D,$9C,$33
        .byte   $DD,$DF,$DB,$FA,$00,$EA,$E9,$F9
        .byte   $FD,$FF,$00,$FA,$00,$E8,$F8,$00
        .byte   $D6,$14,$01,$01,$01,$01,$01,$01
        .byte   $D6,$01,$01,$01,$01,$01,$01,$01
        .byte   $D2,$D4,$D7,$D9,$BD,$C3,$E7,$00
        .byte   $F2,$F4,$00,$01,$00,$01,$01,$01
        .byte   $5B,$F6,$01,$01,$01,$01,$31,$9F
        .byte   $C2,$BC,$C3,$BC,$BC,$00,$BF,$B7
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
; ===========================================================================
; --- collision attribute table ($BF00-$BFFF) ---
; 256 bytes, one per metatile ID. Upper nibble = collision type:
;   $00 = empty/passthrough    $10 = solid
;   $03 = ladder               $11 = solid (variant)
;   $12 = solid (variant)      $13 = solid one-way platform
;   $20 = spike/hazard         $40 = water/current
;   $50 = special conveyor     $51/$52 = left/right conveyor
;   $63 = climbable surface
        .byte   $00,$10,$10,$10,$40,$20,$40,$20
        .byte   $11,$12,$11,$12,$11,$11,$12,$10
        .byte   $13,$13,$13,$13,$10,$10,$10,$10
        .byte   $03,$03,$10,$10,$10,$10,$10,$10
        .byte   $11,$11,$11,$11,$12,$12,$12,$12
        .byte   $11,$11,$11,$11,$12,$12,$12,$12
        .byte   $11,$11,$11,$00,$12,$12,$12,$00
        .byte   $11,$11,$11,$11,$12,$12,$12,$12
        .byte   $11,$12,$50,$50,$10,$10,$00,$00
        .byte   $10,$10,$51,$51,$10,$10,$00,$00
        .byte   $11,$12,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$02,$00,$00,$00
        .byte   $13,$13,$13,$00,$00,$00,$51,$00
        .byte   $13,$13,$13,$00,$00,$00,$52,$00
        .byte   $13,$13,$13,$00,$00,$00,$00,$00
        .byte   $13,$13,$13,$13,$00,$00,$00,$00
        .byte   $63,$63,$63,$63,$63,$63,$63,$63
        .byte   $63,$63,$00,$63,$63,$63,$63,$63
        .byte   $10,$10,$00,$00,$00,$00,$00,$00
        .byte   $10,$00,$00,$00,$00,$00,$00,$00
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$00,$03,$00,$00,$00
        .byte   $03,$03,$00,$00,$00,$00,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
