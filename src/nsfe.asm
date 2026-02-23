; =============================================================================
; NSFe Container — Mega Man 3 Soundtrack
; =============================================================================
; Pure ca65 assembly NSFe builder.  Includes the pre-assembled sound engine
; PRG binary (banks $16/$17/partial $18) and wraps it in NSFe chunk format.
;
; Built entirely from source — no ROM extraction, no external scripts.
;
; NSFe spec: each chunk is [4-byte LE size][4-byte FourCC][data...].
; Required chunks: INFO, DATA, NEND.  Optional: rate, auth, time, fade, tlbl.
; =============================================================================

.segment "NSFE"

; =============================================================================
; NSFe magic
; =============================================================================
.byte   "NSFE"

; =============================================================================
; INFO chunk (10 bytes)
; =============================================================================
; Load:  $8000 (start of bank $16 — sound_frame_update)
; Init:  $8003 (JMP play_sound_odd_frame)
; Play:  $8000 (JMP sound_frame_update)
; =============================================================================
.dword  10
.byte   "INFO"
.word   $8000                   ; load address
.word   $8003                   ; init address
.word   $8000                   ; play address
.byte   $00                     ; NTSC only
.byte   $00                     ; no expansion sound
.byte   57                      ; total songs ($00-$38)
.byte   $00                     ; starting song (0-based)

; =============================================================================
; DATA chunk (20028 bytes)
; =============================================================================
; Banks $16 (sound driver, 8 KB) + $17 (music/SFX data, 8 KB) +
; $18 (first 3644 bytes — sound pointer overflow tables).
;
; Bank $16 assembled with -D NSF_BUILD: the read_ptr bank $18 range check
; is NOPped out because NSF has no mapper — bank $18 data sits linearly
; at $C000+ in the address space.
; =============================================================================
.dword  20028
.byte   "DATA"
.incbin "build/nsfe_prg.bin", 0, 20028

; =============================================================================
; rate chunk (4 bytes) — NTSC play speed
; =============================================================================
.dword  4
.byte   "rate"
.word   16666                   ; NTSC microseconds/frame
.word   0                       ; PAL (unused)

; =============================================================================
; auth chunk — title, artist, copyright, ripper
; =============================================================================
.dword  auth_end - auth_start
.byte   "auth"
auth_start:
.byte   "Mega Man III", 0
.byte   "Yasuaki Fujita, Harumi Fujita", 0
.byte   "1990 Capcom", 0
.byte   "ca65 disassembly rip, 2026", 0
auth_end:

; =============================================================================
; time chunk — per-track duration in milliseconds (signed 32-bit)
; =============================================================================
; Music timings from VGMPF verified soundtrack.
; SFX timings estimated from sound data / in-game behavior.
; =============================================================================
.dword  time_end - time_start
.byte   "time"
time_start:
.dword   87000                  ; $00  Title Screen (1:27)
.dword  136000                  ; $01  Needle Man Stage (2:16)
.dword   81000                  ; $02  Magnet Man Stage (1:21)
.dword  120000                  ; $03  Gemini Man Stage (2:00)
.dword  103000                  ; $04  Hard Man Stage (1:43)
.dword  105000                  ; $05  Top Man Stage (1:45)
.dword   89000                  ; $06  Snake Man Stage (1:29)
.dword   73000                  ; $07  Spark Man Stage (1:13)
.dword  127000                  ; $08  Shadow Man Stage (2:07)
.dword  105000                  ; $09  Wily Fortress 1-2 (1:45)
.dword   88000                  ; $0A  Wily Fortress 3-4 (1:28)
.dword   93000                  ; $0B  Wily Fortress 5-6 (1:33)
.dword   17000                  ; $0C  Wily Fortress Map (0:17)
.dword   68000                  ; $0D  Boss Battle (1:08)
.dword   61000                  ; $0E  Password Screen (1:01)
.dword   61000                  ; $0F  Continue Screen (1:01)
.dword   29000                  ; $10  Stage Select (0:29)
.dword   80000                  ; $11  Proto Man Whistle (1:20)
.dword  223000                  ; $12  Ending (3:43)
.dword     500                  ; $13  SFX: Landing
.dword    3000                  ; $14  SFX: 1-Up
.dword     500                  ; $15  SFX: Mega Buster
.dword    1000                  ; $16  SFX: Player Hit
.dword    3000                  ; $17  SFX: Death
.dword     500                  ; $18  SFX: Enemy Hit
.dword     500                  ; $19  SFX: Weapon Bounce
.dword    1000                  ; $1A  SFX: Pause
.dword     200                  ; $1B  SFX: Cursor
.dword     500                  ; $1C  SFX: HP Refill
.dword    2000                  ; $1D  SFX: Stage Door
.dword     500                  ; $1E  SFX: Projectile
.dword     500                  ; $1F  SFX: Landing (Alt)
.dword    1000                  ; $20  SFX: Heavy Stomp
.dword    1000                  ; $21  [Unused SFX]
.dword    1000                  ; $22  SFX: Wanaan Clamp
.dword    1000                  ; $23  SFX: Debris
.dword     500                  ; $24  SFX: Enemy Attack
.dword     500                  ; $25  SFX: Turret Fire
.dword    1000                  ; $26  SFX: Approach
.dword    1000                  ; $27  SFX: Platform
.dword    3000                  ; $28  SFX: Wind
.dword    1000                  ; $29  [Unused SFX]
.dword    2000                  ; $2A  SFX: Magnet Missile
.dword    2000                  ; $2B  SFX: Gemini Laser
.dword    1000                  ; $2C  SFX: Top Spin
.dword    1000                  ; $2D  SFX: Spark Shock
.dword    1000                  ; $2E  SFX: Shadow Blade
.dword    1000                  ; $2F  [Unused SFX]
.dword    1000                  ; $30  SFX: Hard Stomp
.dword   61000                  ; $31  SFX: Weapon Get (1:01)
.dword    2000                  ; $32  SFX: Explosion
.dword    6000                  ; $33  Stage Start Fanfare (0:06)
.dword    3000                  ; $34  SFX: Teleport
.dword   13000                  ; $35  Boss Intro (0:13)
.dword   48000                  ; $36  Doc Robot / Proto Man (0:48)
.dword    5000                  ; $37  Wily Defeat Victory (0:05)
.dword    3000                  ; $38  Stage Clear (0:03)
time_end:

; =============================================================================
; fade chunk — per-track fade-out in milliseconds (signed 32-bit)
; =============================================================================
; Looping music: 10s fade.  Jingles/SFX: no fade (0).
; =============================================================================
.dword  fade_end - fade_start
.byte   "fade"
fade_start:
.dword  10000                   ; $00  Title Screen
.dword  10000                   ; $01  Needle Man Stage
.dword  10000                   ; $02  Magnet Man Stage
.dword  10000                   ; $03  Gemini Man Stage
.dword  10000                   ; $04  Hard Man Stage
.dword  10000                   ; $05  Top Man Stage
.dword  10000                   ; $06  Snake Man Stage
.dword  10000                   ; $07  Spark Man Stage
.dword  10000                   ; $08  Shadow Man Stage
.dword  10000                   ; $09  Wily Fortress 1-2
.dword  10000                   ; $0A  Wily Fortress 3-4
.dword  10000                   ; $0B  Wily Fortress 5-6
.dword  10000                   ; $0C  Wily Fortress Map
.dword  10000                   ; $0D  Boss Battle
.dword  10000                   ; $0E  Password Screen
.dword  10000                   ; $0F  Continue Screen
.dword  10000                   ; $10  Stage Select
.dword  10000                   ; $11  Proto Man Whistle
.dword      0                   ; $12  Ending (plays once)
.dword      0                   ; $13  SFX: Landing
.dword      0                   ; $14  SFX: 1-Up
.dword      0                   ; $15  SFX: Mega Buster
.dword      0                   ; $16  SFX: Player Hit
.dword      0                   ; $17  SFX: Death
.dword      0                   ; $18  SFX: Enemy Hit
.dword      0                   ; $19  SFX: Weapon Bounce
.dword      0                   ; $1A  SFX: Pause
.dword      0                   ; $1B  SFX: Cursor
.dword      0                   ; $1C  SFX: HP Refill
.dword      0                   ; $1D  SFX: Stage Door
.dword      0                   ; $1E  SFX: Projectile
.dword      0                   ; $1F  SFX: Landing (Alt)
.dword      0                   ; $20  SFX: Heavy Stomp
.dword      0                   ; $21  [Unused SFX]
.dword      0                   ; $22  SFX: Wanaan Clamp
.dword      0                   ; $23  SFX: Debris
.dword      0                   ; $24  SFX: Enemy Attack
.dword      0                   ; $25  SFX: Turret Fire
.dword      0                   ; $26  SFX: Approach
.dword      0                   ; $27  SFX: Platform
.dword      0                   ; $28  SFX: Wind
.dword      0                   ; $29  [Unused SFX]
.dword      0                   ; $2A  SFX: Magnet Missile
.dword      0                   ; $2B  SFX: Gemini Laser
.dword      0                   ; $2C  SFX: Top Spin
.dword      0                   ; $2D  SFX: Spark Shock
.dword      0                   ; $2E  SFX: Shadow Blade
.dword      0                   ; $2F  [Unused SFX]
.dword      0                   ; $30  SFX: Hard Stomp
.dword      0                   ; $31  SFX: Weapon Get
.dword      0                   ; $32  SFX: Explosion
.dword      0                   ; $33  Stage Start Fanfare
.dword      0                   ; $34  SFX: Teleport
.dword      0                   ; $35  Boss Intro
.dword  10000                   ; $36  Doc Robot / Proto Man (loops)
.dword      0                   ; $37  Wily Defeat Victory
.dword      0                   ; $38  Stage Clear
fade_end:

; =============================================================================
; tlbl chunk — per-track labels (null-terminated strings)
; =============================================================================
; Composers (source: VGMPF, Mega Man Wiki):
;   Harumi Fujita ("Mrs. Tarumi") — Needle Man, Gemini Man, Staff Roll (partial)
;   Yasuaki Fujita ("Bun Bun") — all remaining tracks
; =============================================================================
.dword  tlbl_end - tlbl_start
.byte   "tlbl"
tlbl_start:
.byte   "Title Screen", 0                        ; $00
.byte   "Needle Man Stage [H. Fujita]", 0        ; $01
.byte   "Magnet Man Stage", 0                    ; $02
.byte   "Gemini Man Stage [H. Fujita]", 0        ; $03
.byte   "Hard Man Stage", 0                      ; $04
.byte   "Top Man Stage", 0                       ; $05
.byte   "Snake Man Stage", 0                     ; $06
.byte   "Spark Man Stage", 0                     ; $07
.byte   "Shadow Man Stage", 0                    ; $08
.byte   "Wily Fortress 1-2", 0                   ; $09
.byte   "Wily Fortress 3-4", 0                   ; $0A
.byte   "Wily Fortress 5-6", 0                   ; $0B
.byte   "Wily Fortress Map", 0                   ; $0C
.byte   "Boss Battle", 0                         ; $0D
.byte   "Password Screen", 0                     ; $0E
.byte   "Continue Screen", 0                     ; $0F
.byte   "Stage Select", 0                        ; $10
.byte   "Proto Man Whistle [H. & Y. Fujita]", 0  ; $11
.byte   "Ending", 0                              ; $12
.byte   "SFX: Landing", 0                        ; $13
.byte   "SFX: 1-Up", 0                           ; $14
.byte   "SFX: Mega Buster", 0                    ; $15
.byte   "SFX: Player Hit", 0                     ; $16
.byte   "SFX: Death", 0                          ; $17
.byte   "SFX: Enemy Hit", 0                      ; $18
.byte   "SFX: Weapon Bounce", 0                  ; $19
.byte   "SFX: Pause", 0                          ; $1A
.byte   "SFX: Cursor", 0                         ; $1B
.byte   "SFX: HP Refill", 0                      ; $1C
.byte   "SFX: Stage Door", 0                     ; $1D
.byte   "SFX: Projectile", 0                     ; $1E
.byte   "SFX: Landing (Alt)", 0                  ; $1F
.byte   "SFX: Heavy Stomp", 0                    ; $20
.byte   "[Unused SFX $21]", 0                    ; $21
.byte   "SFX: Wanaan Clamp", 0                   ; $22
.byte   "SFX: Debris", 0                         ; $23
.byte   "SFX: Enemy Attack", 0                   ; $24
.byte   "SFX: Turret Fire", 0                    ; $25
.byte   "SFX: Approach", 0                       ; $26
.byte   "SFX: Platform", 0                       ; $27
.byte   "SFX: Wind", 0                           ; $28
.byte   "[Unused SFX $29]", 0                    ; $29
.byte   "SFX: Magnet Missile", 0                 ; $2A
.byte   "SFX: Gemini Laser", 0                   ; $2B
.byte   "SFX: Top Spin", 0                       ; $2C
.byte   "SFX: Spark Shock", 0                    ; $2D
.byte   "SFX: Shadow Blade", 0                   ; $2E
.byte   "[Unused SFX $2F]", 0                    ; $2F
.byte   "SFX: Hard Stomp", 0                     ; $30
.byte   "SFX: Weapon Get", 0                     ; $31
.byte   "SFX: Explosion", 0                      ; $32
.byte   "Stage Start Fanfare", 0                 ; $33
.byte   "SFX: Teleport", 0                       ; $34
.byte   "Boss Intro", 0                          ; $35
.byte   "Doc Robot / Proto Man", 0               ; $36
.byte   "Wily Defeat Victory", 0                 ; $37
.byte   "Stage Clear", 0                         ; $38
tlbl_end:

; =============================================================================
; NEND terminator
; =============================================================================
.dword  0
.byte   "NEND"
