main_needle_man_j:
; =============================================================================
; MEGA MAN 3 (U) — BANK $06 — ROBOT MASTER AI (NEEDLE/MAGNET/TOP/SHADOW)
; =============================================================================
; AI state machines for Needle Man, Magnet Man, Top Man, and Shadow Man.
;
; Annotation: 0% — unannotated da65 output
; =============================================================================


; =============================================================================
; MEGA MAN 3 (U) — BANK $06 — ROBOT MASTER AI (NEEDLE/MAGNET/TOP/SHADOW)
; =============================================================================
; Mapped to $A000-$BFFF. Contains boss AI routines dispatched from bank1C_1D
; for routine indices $C0-$CF. Entry points: main_needle_man, main_magnet_man_j,
; main_top_man_j, main_shadow_man_j. Each boss has a multi-state AI with
; attack patterns, movement, and vulnerability windows.
; Also doubles as Spark Man stage data ($22=$06).
;
; Annotation: partial — AI state labels named for all 4 bosses, 81 auto labels remain
; =============================================================================

        .setcpu "6502"

L0000           := $0000
LF580           := $F580
LF5C4           := $F5C4
LF606           := $F606
LF67C           := $F67C
LF71D           := $F71D
LF73B           := $F73B
LF759           := $F759
LF779           := $F779
LF81B           := $F81B
LF835           := $F835
LF846           := $F846
LF869           := $F869
LF89A           := $F89A
LF8C2           := $F8C2
LFC53           := $FC53
LFC63           := $FC63

.segment "BANK06"

        jmp     main_needle_man
main_magnet_man_j:

        jmp     code_A24E
main_top_man_j:

        jmp     code_A4B3
main_shadow_man_j:

        jmp     code_A698

        jmp     code_A472

        jmp     code_A62B

        jmp     code_A8E0

        jmp     main_needle_man

        jmp     code_A698

        rts

main_needle_man:  lda     $0300,x
        and     #$0F
        tay                             ; load Needle Man's
        lda     needle_man_state_ptr_lo,y ; AI pointer based on state
        sta     L0000                   ; jump to it
        lda     needle_man_state_ptr_hi,y
        sta     $01
        jmp     (L0000)

needle_man_state_ptr_lo:  .byte   $39,$4D,$68,$F4,$50 ; state $00: init
needle_man_state_ptr_hi:  .byte   $A0,$A0,$A0,$A0,$A1 ; state $00: init

; state $00: one-frame state for init
needle_man_init:
        lda     #$78                    ; timer: 120 frames
        sta     $0500,x                 ; to wait before acting
        jsr     needle_man_setup_throw
        lda     $0300,x
        ora     #$40                    ; set boss flag
        sta     $0300,x
        inc     $0300,x                 ; next state
        rts

; state $01: waiting for B press at beginning of battle
needle_man_wait_B:

        lda     #$00
        sta     $05E0,x                 ; clear animation
        sta     $05A0,x
        dec     $0500,x
        bne     LA05E                   ; if timer expires,
        inc     $0300,x                 ; go to next state
        rts

LA05E:  lda     $14
        and     #$40                    ; if player presses B,
        beq     LA067                   ; go to next state
        inc     $0300,x
LA067:  rts

; state $02: jumping & throwing needles
needle_man_throw:

        lda     $05C0,x
        cmp     #$28                    ; if his animation is throwing
        beq     LA0AD                   ; needle, skip some stuff
        ldy     #$1E
        jsr     LF67C                   ; I believe this checks for his
        bcc     LA093                   ; Y value to be back on ground?
        lda     #$29                    ; start animation sequence
        jsr     LF835                   ; for jumping before needle throw
        lda     #$02
        sta     $05A0,x                 ; set up animation frame
        lda     #$00                    ; for ???
        sta     $05E0,x
        lda     #$08                    ; 8 frame timer for a brief pause
        sta     $0540,x
        inc     $0300,x                 ; next state
        jsr     LF869                   ; face toward player
        jmp     needle_man_setup_jump   ; setup jump values

LA093:  lda     $0460,x                 ; if he is moving down
        bmi     LA0A3
        lda     #$01
        sta     $05A0,x                 ; if not, set up animation
        lda     #$00                    ; frame for ???
        sta     $05E0,x
        rts

LA0A3:  lda     $0540,x                 ; if timer hasn't expired
        bne     LA0EE                   ; for moving down
        lda     #$28                    ; if it has, set up animation
        jsr     LF835                   ; for throwing needle
LA0AD:  jsr     test_facing_change      ; handle facing change
        lda     $0500,x                 ; if first needle has
        bne     LA0C2                   ; been thrown, skip
        lda     $05A0,x
        cmp     #$01                    ; if animation frame is not ???
        bne     LA0D4
        jsr     spawn_needle            ; else spawn a needle and set
        inc     $0500,x                 ; "first needle thrown" flag
LA0C2:  lda     $0520,x                 ; if second needle has
        bne     LA0D4                   ; been thrown, skip
        lda     $05A0,x
        cmp     #$03                    ; if animation frame is not ???
        bne     LA0D4
        jsr     spawn_needle            ; else spawn a needle and set
        inc     $0520,x                 ; "second needle thrown" flag
LA0D4:  lda     $05A0,x
        cmp     #$03                    ; if animation frame is not ???
        bne     LA0F3
        lda     #$00
        sta     $0500,x                 ; clear needle thrown flags
        sta     $0520,x
        lda     #$29                    ; start animation sequence
        jsr     LF835                   ; for jumping between needle throws
        lda     #$10                    ; give 16 frames between throws
        sta     $0540,x                 ; timer
        rts

LA0EE:  dec     $0540,x                 ; tick timer down
        bne     LA0F3                   ; useless branch
LA0F3:  rts

; state $03: jump toward player (or pause)
needle_man_jump_player:

        lda     $05A0,x                 ; if animation frame is ???
        cmp     #$02                    ; this is to skip movement
        beq     LA132                   ; if this is just a pause state
        lda     #$01
        sta     $05A0,x                 ; set up animation frame
        lda     #$00                    ; for ???
        sta     $05E0,x
        lda     $04A0,x
        and     #$01                    ; if he's not facing right
        beq     LA114
        ldy     #$20                    ; move right
        jsr     LF580
        jmp     LA119

LA114:  ldy     #$21                    ; move left
        jsr     LF5C4
LA119:  ldy     #$1E
        jsr     LF67C                   ; I believe this checks for his
        bcc     LA14F                   ; Y value to be back on ground?
        lda     #$02
        sta     $05A0,x                 ; set up animation frame
        lda     #$00                    ; for ???
        sta     $05E0,x
        jsr     test_facing_change
        lda     #$08                    ; 8-frame timer
        sta     $0540,x                 ; for extra pause when ground reached
LA132:  dec     $0540,x                 ; tick timer down
        bne     LA14F                   ; check if done
        lda     $0560,x                 ; test if state value from table
        bne     LA145                   ; was $FF
        lda     #$C2                    ; if it was $00 instead
        sta     $0300,x                 ; go to throw needles state
        jsr     needle_man_setup_throw  ; and setup values for it
        rts

LA145:  lda     #$2A                    ; set up headbutt
        jsr     LF835                   ; animation
        lda     #$C4                    ; go to headbutt state
        sta     $0300,x
LA14F:  rts

; state $04: head butt
needle_man_headbutt:

        lda     $05A0,x
        cmp     #$04
        bne     code_A176
        lda     $05E0,x
        and     #$08
        beq     code_A176
        lda     #$C0
        sta     $0320,x
        lda     #$29
        jsr     LF835
        lda     #$C3
        sta     $0300,x
        jsr     needle_man_setup_jump
code_A170:  lda     #$CA
        sta     $0480,x
        rts

code_A176:  lda     $05A0,x
        cmp     #$02
        bne     code_A170
        lda     #$D2
        sta     $0480,x
        lda     #$C7
        sta     $0320,x
        rts

needle_man_setup_throw:  lda     $E4
        adc     $E5                     ; grab RNG value
        sta     $E5                     ; and update it as well
        and     #$01                    ; y = random index from 0 to 1
        tay                             ; 50/50
        lda     needle_man_throw_vel_y_sub,y
        sta     $0440,x                 ; Y velocity = either
        lda     needle_man_throw_vel_y,y ; $093C or $0688
        sta     $0460,x                 ; 50/50 chance
        rts

; values for needle man's throwing of needles
; indexed randomly 0 through 1 (50/50 chance each one)
; contains Y speed (both subpixel and pixel)
; so, jump heights

needle_man_throw_vel_y_sub:  .byte   $88,$3C
needle_man_throw_vel_y:  .byte   $06,$09
needle_man_setup_jump:  lda     #$88
        sta     $0440,x                 ; Y velocity = $0688
        lda     #$06
        sta     $0460,x
        lda     $E4
        adc     $E5                     ; grab RNG value
        sta     $E5                     ; and update it as well
        and     #$07                    ; y = random index from 0 to 7
        tay
        lda     needle_man_jump_vel_x_sub,y
        sta     $0400,x                 ; one of 8 random X velocity
        lda     needle_man_jump_vel_x,y ; values
        sta     $0420,x
        lda     needle_man_jump_states,y ; one of 8 random state values
        sta     $0560,x                 ; to go onto after jump
        rts

; values for needle man's jump toward player
; indexed randomly 0 through 7 (8 possible values)
; contains X speed (both subpixel and pixel) and states
; to move onto after the jump is completed

needle_man_jump_vel_x_sub:  .byte   $00,$80,$80,$00,$00,$80,$80,$80
needle_man_jump_vel_x:  .byte   $00,$01,$02,$00,$00,$01,$03,$02
needle_man_jump_states:  .byte   $FF,$00,$FF,$FF,$00,$FF,$00,$FF
spawn_needle:  jsr     LFC53
        bcs     code_A245
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$02
        tay
        lda     $05A0,x
        cmp     #$01
        bne     code_A211
        lda     $0360,x
        clc
        adc     LA246,y
        pha
        lda     $0380,x
        adc     LA247,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
        jmp     code_A228

code_A211:  lda     $0360,x
        clc
        adc     LA24A,y
        pha
        lda     $0380,x
        adc     LA24B,y
        ldy     L0000
        sta     $0380,y
        pla
        sta     $0360,y
code_A228:  lda     $03C0,x
        clc
        adc     #$07
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        lda     #$27
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        lda     #$AF
        sta     $0320,y
code_A245:  rts

LA246:  .byte   $15
LA247:  .byte   $00,$EB,$FF
LA24A:  .byte   $02
LA24B:  .byte   $00,$FE,$FF
code_A24E:  lda     $0300,x
        and     #$0F
        tay
        lda     LA261,y
        sta     L0000
        lda     LA266,y
        sta     $01
        jmp     (L0000)

LA261:  .byte   $6B,$EB,$0C,$76,$A5
LA266:  .byte   $A2,$A2,$A3,$A3,$A3
        lda     $0520,x
        bne     code_A2E7
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        lda     $0560,x
        and     #$01
        beq     code_A28D
        lda     $0580,x
        ora     #$40
        sta     $0580,x
        jsr     LF71D
        jmp     code_A298

code_A28D:  lda     $0580,x
        and     #$BF
        sta     $0580,x
        jsr     LF73B
code_A298:  ldy     #$22
        jsr     LF67C
        bcc     code_A2E2
        lda     $0500,x
        tay
        lda     LA419,y
        sta     $0440,x
        lda     LA41C,y
        sta     $0460,x
        lda     LA41F,y
        sta     $0400,x
        lda     LA422,y
        sta     $0420,x
        lda     #$1F
        jsr     LF835
        lda     #$04
        sta     $0520,x
        inc     $0500,x
        lda     $0500,x
        cmp     #$03
        bcc     code_A2E1
        inc     $0300,x
        lda     #$00
        sta     $0500,x
        lda     #$3C
        sta     $0440,x
        lda     #$09
        sta     $0460,x
code_A2E1:  rts

code_A2E2:  lda     #$20
        jmp     LF835

code_A2E7:  dec     $0520,x
        rts

        lda     $E4
        adc     $E5
        sta     $E4
        and     #$01
        bne     code_A2F9
        inc     $0300,x
        rts

code_A2F9:  lda     #$C4
        sta     $0300,x
        lda     #$F0
        sta     $0540,x
        lda     #$1E
        jsr     LF835
        jsr     code_A3FC
        rts

        lda     $05C0,x
        cmp     #$21
        beq     code_A333
        lda     #$20
        jsr     LF835
        ldy     #$23
        jsr     LF67C
        lda     $10
        and     #$10
        beq     code_A332
        lda     #$21
        jsr     LF835
        lda     #$00
        sta     $0520,x
        lda     #$06
        sta     $0500,x
code_A332:  rts

code_A333:  jsr     code_A3FC
        dec     $0500,x
        bne     code_A375
        lda     #$06
        sta     $0500,x
        lda     $05A0,x
        cmp     #$01
        bne     code_A375
        jsr     code_A425
        lda     #$2A
        jsr     LF89A
        inc     $0520,x
        lda     $0520,x
        cmp     #$03
        bcc     code_A375
        inc     $0300,x
        lda     #$00
        sta     $0500,x
        lda     #$1E
        sta     $0540,x
        lda     #$20
        jsr     LF835
        lda     #$80
        sta     $0440,x
        lda     #$06
        sta     $0460,x
code_A375:  rts

        lda     $0500,x
        bne     code_A383
        dec     $0540,x
        bne     code_A375
        inc     $0500,x
code_A383:  ldy     #$1E
        jsr     LF606
        bcc     code_A3A4
        lda     #$1F
        jsr     LF835
        jsr     LF81B
        lda     $0560,x
        eor     #$01
        sta     $0560,x
        lda     #$C0
        sta     $0300,x
        lda     #$00
        sta     $0500,x
code_A3A4:  rts

        jsr     LF8C2
        cmp     #$18
        bcc     code_A3E8
        lda     $0580,x
        and     #$40
        bne     code_A3B7
        lda     #$01
        bne     code_A3B9
code_A3B7:  lda     #$02
code_A3B9:  sta     $36
        lda     #$00
        sta     $37
        lda     #$01
        sta     $38
        lda     $05A0,x
        cmp     #$04
        bne     code_A3D2
        lda     $05E0,x
        beq     code_A3D2
        inc     $05A0,x
code_A3D2:  lda     $05A0,x
        cmp     #$06
        bne     code_A3E3
        lda     $05E0,x
        beq     code_A3E3
        lda     #$05
        sta     $05A0,x
code_A3E3:  dec     $0540,x
        bne     code_A3F6
code_A3E8:  lda     #$C3
        sta     $0300,x
        lda     #$CA
        sta     $0480,x
        sta     $0500,x
        rts

code_A3F6:  lda     #$AA
        sta     $0480,x
        rts

code_A3FC:  jsr     LF869
        lda     $04A0,x
        and     #$01
        beq     code_A410
        lda     $0580,x
        ora     #$40
        sta     $0580,x
        bne     code_A418
code_A410:  lda     $0580,x
        and     #$BF
        sta     $0580,x
code_A418:  rts

LA419:  .byte   $9E,$88,$88
LA41C:  .byte   $04,$06,$06
LA41F:  .byte   $B3,$00,$00
LA422:  .byte   $01,$02,$02
code_A425:  jsr     LFC53
        bcs     code_A46F
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$01
        tay
        lda     $0360,x
        clc
        adc     LA470,y
        ldy     L0000
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sta     $03C0,y
        lda     #$00
        sta     $03E0,y
        sta     $0400,y
        sta     $0440,x
        lda     #$04
        sta     $0420,y
        sta     $0460,y
        lda     #$4D
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     #$C4
        sta     $0320,y
code_A46F:  rts

LA470:  .byte   $EC,$14
code_A472:  lda     $05C0,x
        cmp     #$51
        beq     code_A49C
        lda     $0300,x
        and     #$0F
        bne     code_A49C
        lda     $04A0,x
        and     #$01
        beq     code_A48D
        jsr     LF71D
        jmp     code_A490

code_A48D:  jsr     LF73B
code_A490:  jsr     LF8C2
        cmp     #$06
        bcs     code_A4B2
        lda     #$51
        jsr     LF835
code_A49C:  lda     $05C0,x
        cmp     #$59
        beq     code_A4B2
        ldy     #$12
        jsr     LF606
        bcc     code_A4B2
        lda     #$59
        jsr     LF835
        inc     $0300,x
code_A4B2:  rts

code_A4B3:  lda     $0300,x
        and     #$0F
        tay
        lda     LA4C6,y
        sta     L0000
        lda     LA4CA,y
        sta     $01
        jmp     (L0000)

LA4C6:  .byte   $CE,$E2,$3A,$75
LA4CA:  .byte   $A4,$A4,$A5,$A5
        lda     #$3C
        sta     $0500,x
        sta     $0520,x
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        inc     $0300,x
        rts

        lda     #$CA
        sta     $0480,x
        lda     $05A0,x
        cmp     #$02
        bne     code_A4FD
        lda     $0540,x
        bne     code_A4FD
        lda     #$00
        sta     $01
        jsr     code_A5BC
        inc     $0540,x
code_A4FD:  lda     $05A0,x
        cmp     #$03
        bne     code_A525
        lda     $05E0,x
        and     #$08
        beq     code_A525
        ldy     #$1F
        lda     #$10
code_A50F:  cmp     $04C0,y
        beq     code_A51B
        dey
        cpy     #$0F
        bne     code_A50F
        beq     code_A525
code_A51B:  lda     #$C5
        sta     $0320,y
        dey
        lda     #$10
        bne     code_A50F
code_A525:  lda     $05A0,x
        cmp     #$05
        bne     code_A539
        lda     $05E0,x
        beq     code_A539
        inc     $0300,x
        lda     #$44
        jsr     LF835
code_A539:  rts

        lda     $05C0,x
        cmp     #$47
        beq     code_A555
        lda     $0540,x
        beq     code_A564
        dec     $0500,x
        bne     code_A539
        lda     #$47
        jsr     LF835
        lda     #$AA
        sta     $0480,x
code_A555:  lda     $05A0,x
        cmp     #$02
        bne     code_A564
        lda     #$48
        jsr     LF835
        dec     $0540,x
code_A564:  dec     $0520,x
        bne     code_A539
        inc     $0300,x
        lda     #$78
        sta     $0500,x
        sta     $0520,x
        rts

        lda     $0560,x
        and     #$01
        beq     code_A589
        jsr     LF71D
        lda     $0360,x
        cmp     #$D0
        bcs     code_A593
        jmp     code_A5BB

code_A589:  jsr     LF73B
        lda     $0360,x
        cmp     #$30
        bcs     code_A5BB
code_A593:  lda     $0560,x
        eor     #$01
        sta     $0560,x
        lda     $04A0,x
        eor     #$03
        sta     $04A0,x
        lda     $0580,x
        eor     #$40
        sta     $0580,x
        lda     #$49
        jsr     LF835
        dec     $0300,x
        dec     $0300,x
        lda     #$00
        sta     $0540,x
code_A5BB:  rts

code_A5BC:  jsr     LFC53
        bcs     code_A624
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$01
        tay
        lda     $0360,x
        ldy     L0000
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sec
        sbc     #$0E
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        sta     $0320,y
        sta     $0400,y
        sta     $0440,y
        lda     #$04
        sta     $0420,y
        sta     $0460,y
        lda     #$46
        jsr     LF846
        lda     #$8B
        sta     $0480,y
        lda     #$10
        sta     $04C0,y
        ldy     $01
        lda     LA625,y
        ldy     L0000
        sta     $0500,y
        ldy     $01
        lda     LA628,y
        ldy     L0000
        sta     $0520,y
        inc     $01
        lda     $01
        cmp     #$03
        bcc     code_A5BC
code_A624:  rts

LA625:  .byte   $08,$10,$18
LA628:  .byte   $32,$2A,$1E
code_A62B:  lda     $0300,x
        and     #$0F
        bne     code_A653
        lda     $04A0,x
        and     #$01
        beq     code_A63F
        jsr     LF71D
        jmp     code_A642

code_A63F:  jsr     LF73B
code_A642:  jsr     LF779
        dec     $0500,x
        bne     code_A652
        lda     #$00
        sta     $0500,x
        inc     $0300,x
code_A652:  rts

code_A653:  lda     $0300,x
        and     #$02
        bne     code_A663
        dec     $0520,x
        bne     code_A652
        inc     $0300,x
        rts

code_A663:  lda     $0500,x
        bne     code_A67B
        lda     #$33
        sta     $02
        lda     #$05
        sta     $03
        jsr     LFC63
        lda     $0C
        sta     $04A0,x
        inc     $0500,x
code_A67B:  lda     $04A0,x
        and     #$08
        beq     code_A688
        jsr     LF779
        jmp     code_A68B

code_A688:  jsr     LF759
code_A68B:  lda     $04A0,x
        and     #$01
        beq     code_A695
        jmp     LF71D

code_A695:  jmp     LF73B

code_A698:  lda     $0300,x
        and     #$0F
        tay
        lda     LA6AB,y
        sta     L0000
        lda     LA6B0,y
        sta     $01
        jmp     (L0000)

LA6AB:  .byte   $B5,$D6,$30,$93,$EE
LA6B0:  .byte   $A6,$A6,$A7,$A7,$A7
        lda     #$00
        sta     $0500,x
        sta     $0520,x
        sta     $0560,x
        lda     #$80
        sta     $0400,x
        lda     #$00
        sta     $0420,x
        lda     $0300,x
        ora     #$40
        sta     $0300,x
        inc     $0300,x
        rts

        lda     $0520,x
        bne     code_A722
        lda     $04A0,x
        and     #$01
        beq     code_A6EA
        ldy     #$24
        jsr     LF580
        jmp     code_A6EF

code_A6EA:  ldy     #$25
        jsr     LF5C4
code_A6EF:  ldy     #$22
        jsr     LF67C
        bcc     code_A714
        lda     #$40
        jsr     LF835
        lda     #$04
        sta     $0520,x
        jsr     code_A84C
        jsr     test_facing_change
        inc     $0500,x
        lda     $0500,x
        cmp     #$03
        bcc     code_A72F
        inc     $0300,x
        rts

code_A714:  lda     #$40
        jsr     LF835
        lda     #$00
        sta     $05E0,x
        sta     $05A0,x
        rts

code_A722:  lda     #$01
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        dec     $0520,x
code_A72F:  rts

        lda     $0540,x
        bne     code_A743
        inc     $0300,x
        lda     #$41
        jsr     LF835
        lda     #$00
        sta     $0520,x
        rts

code_A743:  inc     $0300,x
        inc     $0300,x
        lda     #$08
        sta     $0500,x
        lda     #$00
        sta     $0520,x
        lda     #$3E
        jsr     LF835
        lda     #$C8
        sta     $0320,x
        lda     $0360,x
        sta     $0370
        lda     $0380,x
        sta     $0390
        lda     $03C0,x
        sta     $03D0
        lda     #$00
        sta     $04F0
        sta     $0490
        sta     $0330
        ldy     #$10
        lda     #$5A
        jsr     LF846
        lda     #$C3
        sta     $0480,x
        lda     #$00
        sta     $0400,x
        lda     #$04
        sta     $0420,x
        jmp     LF869

        lda     $0520,x
        bne     code_A7B4
        lda     $05A0,x
        cmp     #$00
        bne     code_A7ED
        jsr     test_facing_change
        lda     #$00
        sta     $01
        jsr     code_A874
        lda     #$14
        sta     $0500,x
        lda     #$FF
        sta     $0520,x
        rts

code_A7B4:  lda     $05A0,x
        bne     code_A7D7
        dec     $0500,x
        bne     code_A7CE
        lda     #$2E
        jsr     LF89A
        lda     #$01
        sta     $05A0,x
        lda     #$00
        sta     $05E0,x
        rts

code_A7CE:  lda     #$00
        sta     $05A0,x
        sta     $05E0,x
        rts

code_A7D7:  lda     $05E0,x
        and     #$08
        beq     code_A7E1
        inc     $05A0,x
code_A7E1:  lda     $05A0,x
        cmp     #$06
        bne     code_A7ED
        lda     #$C0
        sta     $0300,x
code_A7ED:  rts

        lda     $0520,x
        beq     code_A7F8
        dec     $0500,x
        beq     code_A81E
code_A7F8:  lda     $04A0,x
        and     #$01
        beq     code_A807
        ldy     #$24
        jsr     LF580
        jmp     code_A80C

code_A807:  ldy     #$25
        jsr     LF5C4
code_A80C:  bcs     code_A81E
        lda     $0520,x
        bne     code_A835
        jsr     LF8C2
        cmp     #$08
        bcs     code_A835
        inc     $0520,x
        rts

code_A81E:  lda     #$3D
        jsr     LF835
        lda     #$C0
        sta     $0300,x
        lda     #$CA
        sta     $0480,x
        jsr     test_facing_change
        lda     #$C3
        sta     $0320,x
code_A835:  rts

test_facing_change:  lda     $04A0,x
        pha                             ; faces toward player then
        jsr     LF869                   ; tests if the facing
        pla                             ; has changed (left to right
        cmp     $04A0,x                 ; or right to left)
        beq     LA84B
        lda     $0580,x                 ; if so,
        eor     #$40                    ; flip facing-lock flag
        sta     $0580,x                 ; to simulate proper facing
LA84B:  rts

code_A84C:  lda     $E4
        adc     $E5
        sta     $E5
        and     #$03
        tay
        lda     LA868,y
        sta     $0440,x
        lda     LA86C,y
        sta     $0460,x
        lda     LA870,y
        sta     $0540,x
        rts

LA868:  .byte   $9E,$88,$00,$88
LA86C:  .byte   $04,$06,$08,$06
LA870:  .byte   $00,$01,$00,$01
code_A874:  jsr     LFC53
        bcs     code_A8D7
        sty     L0000
        lda     $04A0,x
        sta     $04A0,y
        and     #$01
        tay
        lda     $0360,x
        ldy     L0000
        sta     $0360,y
        lda     $0380,x
        sta     $0380,y
        lda     $03C0,x
        sec
        sbc     #$18
        sta     $03C0,y
        lda     #$00
        sta     $04E0,y
        lda     #$42
        jsr     LF846
        lda     #$80
        sta     $0480,y
        lda     #$C6
        sta     $0320,y
        stx     $03
        lda     $01
        asl     a
        tax
        lda     LA8D8,x
        sta     $0440,y
        lda     LA8D9,x
        sta     $0460,y
        lda     LA8DC,x
        sta     $0400,y
        lda     LA8DD,x
        sta     $0420,y
        ldx     $03
        inc     $01
        lda     $01
        cmp     #$02
        bcc     code_A874
code_A8D7:  rts

LA8D8:  .byte   $D2
LA8D9:  .byte   $FC,$00,$00
LA8DC:  .byte   $2E
LA8DD:  .byte   $03,$80,$04
code_A8E0:  lda     $05C0,x
        cmp     #$42
        bne     code_A8FC
        lda     $05A0,x
        cmp     #$06
        bne     code_A929
        lda     #$43
        jsr     LF835
        lda     $03C0,x
        clc
        adc     #$0E
        sta     $03C0,x
code_A8FC:  lda     #$00
        sta     $02
        lda     $0460,x
        bpl     code_A907
        dec     $02
code_A907:  lda     $03A0,x
        clc
        adc     $0440,x
        sta     $03A0,x
        lda     $03C0,x
        adc     $0460,x
        sta     $03C0,x
        lda     $03E0,x
        adc     $02
        bne     code_A924
        jmp     code_A68B

code_A924:  lda     #$00
        sta     $0300,x
code_A929:  rts

        .byte   $AA,$81,$0A,$B6,$03,$D7,$A9,$F9
        .byte   $A8,$3C,$E2,$71,$20,$F7,$22,$9B
        .byte   $A0,$ED,$AA,$32,$8A,$BD,$0A,$4F
        .byte   $AA,$55,$EA,$6B,$A0,$B2,$BA,$C7
        .byte   $90,$7F,$A8,$47,$88,$F2,$A0,$C2
        .byte   $8A,$7F,$A2,$E3,$A2,$A7,$22,$BF
        .byte   $8A,$F1,$AA,$EF,$A2,$FA,$80,$E6
        .byte   $AA,$F7,$88,$E7,$2A,$FD,$AE,$C8
        .byte   $22,$71,$80,$F9,$A2,$B6,$88,$99
        .byte   $28,$36,$2A,$98,$A2,$6E,$A2,$76
        .byte   $A9,$FF,$8A,$D3,$22,$3C,$8A,$73
        .byte   $2A,$D6,$28,$15,$09,$B0,$2E,$7F
        .byte   $A2,$E4,$EA,$7D,$0A,$51,$8A,$C5
        .byte   $80,$57,$A8,$C4,$82,$54,$A2,$EF
        .byte   $AA,$C1,$AA,$DE,$2A,$F1,$82,$66
        .byte   $8A,$10,$23,$1F,$AA,$7C,$3A,$7C
        .byte   $8A,$E7,$88,$FA,$20,$0D,$A2,$1D
        .byte   $20,$63,$A8,$FC,$88,$7D,$A8,$4B
        .byte   $80,$FD,$AA,$E5,$22,$E9,$A0,$FC
        .byte   $A2,$39,$80,$1C,$AA,$54,$A8,$AB
        .byte   $8E,$2D,$82,$7F,$8A,$33,$8A,$F7
        .byte   $AA,$DF,$02,$3F,$A0,$FC,$A3,$74
        .byte   $AA,$B7,$AA,$EF,$2A,$FF,$A8,$6A
        .byte   $A0,$BF,$A2,$BF,$08,$DE,$80,$F2
        .byte   $A2,$FF,$28,$73,$82,$38,$18,$FF
        .byte   $88,$DD,$A8,$FF,$8A,$FC,$8A,$F6
        .byte   $AA,$FF,$A2,$F3,$AA,$EC,$00,$01
        .byte   $02,$03,$04,$05,$06,$07,$08,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
        .byte   $12,$13,$14,$15,$16,$17,$20,$EF
        .byte   $28,$BF,$A2,$D5,$AA,$EF,$88,$DF
        .byte   $A0,$EE,$28,$F6,$08,$6F,$AA,$5E
        .byte   $4A,$B8,$B2,$4F,$02,$6C,$15,$24
        .byte   $30,$FE,$A8,$F1,$A8,$BE,$82,$77
        .byte   $A2,$9B,$22,$EB,$0A,$BF,$80,$A3
        .byte   $80,$A2,$80,$A1,$40,$40,$40,$62
        .byte   $63,$20,$20,$00,$88,$63,$88,$EF
        .byte   $82,$FB,$22,$7C,$AA,$E6,$8A,$6D
        .byte   $A8,$08,$A8,$FB,$0A,$FC,$08,$00
        .byte   $1A,$00,$17,$00,$03,$00,$2D,$00
        .byte   $08,$00,$00,$00,$00,$00,$00,$01
        .byte   $32,$01,$02,$00,$00,$00,$28,$00
        .byte   $00,$38,$A0,$BE,$A2,$EF,$54,$56
        .byte   $0F,$20,$32,$02,$0F,$20,$32,$26
        .byte   $0F,$20,$32,$0F,$0F,$1B,$1B,$0F
        .byte   $87,$88,$89,$00,$0F,$20,$32,$02
        .byte   $0F,$20,$32,$26,$0F,$20,$32,$0F
        .byte   $0F,$20,$10,$16,$87,$88,$00,$00
        .byte   $8A,$EE,$0A,$FE,$A8,$D3,$8A,$FD
        .byte   $A8,$B9,$AA,$91,$24,$CD,$83,$7F
        .byte   $AA,$E7,$A0,$EF,$2A,$FE,$0A,$35
        .byte   $28,$F7,$A8,$FF,$AA,$5D,$A6,$FD
        .byte   $AA,$36,$0A,$D6,$28,$7D,$AA,$EA
        .byte   $A2,$36,$8A,$F4,$23,$56,$A8,$EA
        .byte   $E8,$FC,$8A,$EE,$28,$DF,$2A,$18
        .byte   $08,$D9,$0A,$F4,$99,$D7,$A2,$DE
        .byte   $20,$FA,$82,$73,$22,$A9,$09,$0B
        .byte   $AA,$B7,$A8,$ED,$28,$4C,$0F,$16
        .byte   $FF,$4F,$AA,$DF,$AA,$BD,$00,$01
        .byte   $02,$03,$03,$03,$03,$04,$05,$06
        .byte   $07,$07,$07,$08,$08,$09,$09,$09
        .byte   $0A,$0B,$0B,$0B,$10,$10,$11,$12
        .byte   $12,$12,$12,$13,$13,$14,$14,$14
        .byte   $14,$14,$14,$15,$15,$15,$17,$FF
        .byte   $08,$5F,$88,$E1,$2A,$FD,$AA,$7F
        .byte   $02,$D0,$AA,$B7,$22,$5F,$A0,$EF
        .byte   $AA,$E8,$8A,$9F,$A8,$BC,$AA,$4B
        .byte   $2A,$5E,$08,$CC,$AA,$BB,$82,$E9
        .byte   $A0,$CE,$20,$DA,$A2,$BF,$82,$BB
        .byte   $8A,$86,$3A,$5F,$8A,$FA,$AA,$F4
        .byte   $00,$AD,$AA,$BE,$00,$6F,$88,$B9
        .byte   $00,$92,$2A,$C4,$AA,$3C,$42,$FC
        .byte   $2A,$BB,$A8,$AA,$A2,$5D,$0A,$FC
        .byte   $AA,$D7,$A8,$6C,$A8,$CF,$EA,$47
        .byte   $82,$3B,$20,$DB,$A8,$FF,$82,$9B
        .byte   $28,$CE,$2A,$F9,$02,$87,$2A,$E5
        .byte   $2A,$5D,$A8,$AF,$B8,$8E,$2A,$39
        .byte   $A8,$D3,$22,$79,$AA,$34,$AA,$F5
        .byte   $DA,$7E,$AA,$E3,$A2,$FD,$2A,$21
        .byte   $8A,$B8,$80,$DF,$AA,$FE,$28,$FD
        .byte   $AA,$F7,$AA,$5D,$2A,$7D,$80,$AF
        .byte   $0A,$F3,$02,$A9,$83,$BA,$2A,$A5
        .byte   $A8,$F6,$A8,$B0,$2A,$BF,$16,$28
        .byte   $8A,$D1,$2A,$59,$AA,$4E,$AA,$5E
        .byte   $02,$E5,$02,$2D,$88,$EB,$8E,$96
        .byte   $2A,$F7,$22,$92,$2C,$BB,$AA,$95
        .byte   $AA,$DF,$88,$59,$AE,$DF,$A2,$11
        .byte   $AA,$72,$82,$70,$AF,$89,$8A,$9F
        .byte   $8A,$BE,$A0,$95,$AE,$EF,$AA,$8A
        .byte   $A2,$37,$8A,$BE,$AA,$FF,$28,$FC
        .byte   $2A,$C3,$2A,$31,$8A,$CD,$48,$A8
        .byte   $90,$10,$68,$90,$E8,$10,$70,$F8
        .byte   $38,$68,$98,$78,$D8,$38,$68,$98
        .byte   $D0,$10,$48,$76,$50,$F0,$90,$C8
        .byte   $D8,$E8,$F8,$C8,$D8,$08,$18,$68
        .byte   $98,$B8,$F8,$08,$28,$68,$D8,$FF
        .byte   $B2,$10,$10,$00,$12,$04,$24,$40
        .byte   $84,$04,$C5,$00,$14,$00,$8B,$40
        .byte   $18,$10,$30,$11,$40,$11,$62,$10
        .byte   $00,$20,$08,$40,$0C,$01,$0E,$00
        .byte   $16,$80,$00,$40,$C8,$20,$40,$00
        .byte   $C0,$40,$02,$00,$06,$00,$84,$01
        .byte   $65,$00,$20,$00,$80,$50,$20,$40
        .byte   $2C,$10,$F0,$40,$E4,$10,$88,$11
        .byte   $08,$40,$60,$C0,$64,$11,$91,$55
        .byte   $21,$00,$20,$44,$A0,$00,$03,$05
        .byte   $69,$00,$00,$51,$C8,$51,$21,$04
        .byte   $00,$10,$00,$00,$00,$40,$00,$00
        .byte   $20,$00,$40,$00,$14,$00,$20,$05
        .byte   $08,$00,$40,$40,$00,$00,$02,$00
        .byte   $04,$00,$3B,$24,$87,$01,$84,$41
        .byte   $04,$44,$21,$14,$11,$14,$44,$14
        .byte   $00,$00,$0A,$14,$02,$41,$00,$10
        .byte   $39,$40,$45,$20,$C8,$00,$8A,$05
        .byte   $20,$00,$50,$81,$4D,$0C,$12,$00
        .byte   $60,$41,$25,$00,$B0,$00,$5C,$10
        .byte   $40,$51,$00,$10,$04,$20,$84,$00
        .byte   $07,$00,$04,$20,$11,$10,$57,$00
        .byte   $50,$00,$3C,$41,$08,$01,$0A,$41
        .byte   $29,$06,$0C,$54,$08,$04,$E2,$54
        .byte   $80,$00,$48,$00,$4C,$01,$0E,$00
        .byte   $F0,$50,$F0,$44,$87,$51,$89,$50
        .byte   $E1,$41,$C1,$41,$0F,$55,$58,$70
        .byte   $54,$54,$80,$54,$80,$54,$50,$C8
        .byte   $98,$D8,$88,$70,$70,$A4,$58,$58
        .byte   $98,$98,$98,$98,$40,$40,$40,$BC
        .byte   $BC,$BC,$BC,$B8,$48,$90,$38,$58
        .byte   $98,$38,$68,$C8,$48,$68,$00,$FF
        .byte   $A0,$40,$40,$00,$20,$00,$E1,$40
        .byte   $95,$01,$B0,$01,$C0,$40,$89,$44
        .byte   $A1,$04,$04,$04,$FE,$07,$70,$00
        .byte   $30,$10,$A0,$10,$20,$00,$01,$00
        .byte   $20,$00,$06,$01,$A0,$01,$8A,$00
        .byte   $A5,$11,$A0,$04,$80,$10,$C4,$10
        .byte   $89,$00,$C2,$00,$9C,$45,$00,$41
        .byte   $A4,$00,$45,$84,$69,$14,$08,$00
        .byte   $05,$11,$48,$14,$83,$01,$28,$05
        .byte   $60,$15,$E2,$10,$1A,$21,$81,$55
        .byte   $08,$00,$A3,$21,$CD,$44,$09,$10
        .byte   $01,$04,$22,$00,$00,$00,$00,$00
        .byte   $00,$00,$A1,$04,$25,$01,$00,$00
        .byte   $0D,$04,$81,$00,$54,$01,$83,$00
        .byte   $00,$00,$3E,$00,$CA,$40,$50,$14
        .byte   $00,$00,$0C,$15,$88,$04,$21,$40
        .byte   $18,$00,$09,$44,$66,$40,$C0,$10
        .byte   $80,$40,$22,$02,$30,$91,$12,$00
        .byte   $A4,$20,$82,$50,$84,$40,$00,$20
        .byte   $04,$03,$01,$54,$00,$10,$20,$01
        .byte   $20,$00,$28,$10,$05,$00,$10,$00
        .byte   $68,$18,$02,$00,$45,$00,$51,$00
        .byte   $20,$02,$B0,$00,$48,$01,$DA,$10
        .byte   $96,$55,$A9,$55,$4C,$10,$B5,$05
        .byte   $E9,$00,$C5,$15,$32,$14,$D0,$00
        .byte   $59,$40,$9D,$04,$FD,$04,$87,$00
        .byte   $7A,$44,$11,$04,$28,$00,$36,$64
        .byte   $24,$24,$65,$24,$65,$24,$03,$2D
        .byte   $2D,$2D,$2D,$64,$64,$12,$50,$52
        .byte   $36,$36,$36,$36,$63,$63,$63,$53
        .byte   $53,$53,$53,$2D,$20,$2D,$20,$20
        .byte   $2D,$20,$20,$2D,$20,$20,$4D,$FF
        .byte   $40,$04,$01,$41,$48,$04,$0C,$40
        .byte   $00,$40,$43,$04,$11,$44,$A0,$05
        .byte   $00,$04,$01,$04,$69,$40,$C0,$01
        .byte   $04,$00,$10,$41,$20,$11,$08,$00
        .byte   $48,$00,$00,$05,$80,$00,$02,$00
        .byte   $40,$00,$10,$41,$20,$00,$44,$40
        .byte   $11,$00,$18,$00,$C4,$41,$80,$80
        .byte   $22,$15,$08,$04,$2A,$10,$64,$15
        .byte   $00,$00,$60,$00,$88,$60,$14,$00
        .byte   $80,$50,$5D,$00,$A0,$44,$26,$40
        .byte   $13,$01,$4A,$05,$78,$C8,$00,$00
        .byte   $19,$41,$40,$81,$00,$14,$10,$05
        .byte   $E0,$01,$92,$01,$04,$00,$4C,$00
        .byte   $88,$40,$40,$10,$85,$40,$68,$00
        .byte   $82,$00,$12,$50,$0F,$00,$80,$42
        .byte   $01,$00,$55,$50,$08,$01,$80,$50
        .byte   $80,$05,$20,$10,$81,$20,$10,$51
        .byte   $22,$04,$8F,$00,$4D,$50,$A2,$01
        .byte   $04,$40,$80,$80,$65,$80,$C0,$20
        .byte   $00,$00,$82,$00,$00,$04,$11,$00
        .byte   $24,$10,$15,$00,$80,$40,$09,$44
        .byte   $02,$04,$00,$10,$80,$11,$CD,$44
        .byte   $00,$00,$80,$00,$65,$40,$22,$19
        .byte   $70,$41,$61,$01,$E2,$69,$A2,$00
        .byte   $49,$50,$EA,$10,$74,$45,$84,$44
        .byte   $95,$14,$5D,$00,$B1,$40,$0A,$50
        .byte   $20,$05,$A4,$16,$5F,$00,$00,$01
        .byte   $02,$03,$03,$03,$03,$03,$00,$04
        .byte   $05,$06,$07,$07,$07,$08,$00,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
        .byte   $11,$11,$11,$12,$13,$0F,$14,$15
        .byte   $16,$0E,$0E,$17,$18,$0F,$19,$1A
        .byte   $0A,$0B,$0C,$0D,$0E,$0F,$1B,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1C,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$1D,$07
        .byte   $07,$1E,$07,$07,$07,$07,$00,$0E
        .byte   $0E,$1F,$14,$20,$14,$15,$00,$21
        .byte   $22,$23,$24,$25,$24,$26,$00,$27
        .byte   $28,$29,$19,$2A,$19,$1A,$00,$0E
        .byte   $2B,$1B,$1B,$1B,$1B,$2C,$00,$2D
        .byte   $02,$03,$03,$03,$03,$2E,$00,$01
        .byte   $02,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$2F,$03,$03,$03,$07,$07
        .byte   $07,$07,$2F,$07,$07,$07,$0E,$30
        .byte   $30,$30,$31,$32,$32,$32,$0E,$33
        .byte   $33,$34,$35,$36,$37,$38,$0E,$0E
        .byte   $39,$3A,$09,$0A,$3B,$3C,$3D,$0C
        .byte   $0D,$33,$0E,$3E,$38,$0E,$1B,$1B
        .byte   $1B,$1B,$1B,$3F,$2C,$0E,$03,$03
        .byte   $03,$03,$03,$03,$00,$0E,$2F,$03
        .byte   $03,$03,$2F,$03,$03,$03,$2F,$07
        .byte   $07,$07,$2F,$07,$07,$07,$31,$32
        .byte   $32,$32,$31,$32,$32,$32,$0E,$40
        .byte   $36,$37,$0E,$38,$38,$37,$0E,$41
        .byte   $42,$43,$3C,$17,$0E,$44,$0E,$09
        .byte   $0A,$0B,$0C,$0D,$0E,$38,$45,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1B,$0F,$03
        .byte   $03,$03,$03,$03,$03,$03,$2F,$03
        .byte   $03,$03,$03,$03,$46,$0F,$2F,$07
        .byte   $07,$07,$07,$07,$47,$0F,$31,$30
        .byte   $30,$32,$32,$0E,$01,$0F,$0E,$34
        .byte   $48,$2C,$37,$0E,$0E,$0F,$49,$3A
        .byte   $4A,$00,$3B,$4B,$4C,$1C,$0E,$33
        .byte   $0F,$2E,$1B,$1C,$03,$03,$1B,$1B
        .byte   $1C,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$4D,$4E
        .byte   $4F,$4F,$4F,$4F,$4F,$50,$4D,$51
        .byte   $20,$14,$14,$20,$14,$52,$4D,$53
        .byte   $2A,$19,$19,$2A,$19,$52,$4D,$24
        .byte   $25,$54,$24,$25,$24,$52,$4D,$19
        .byte   $55,$56,$57,$2A,$19,$52,$58,$59
        .byte   $5A,$5B,$58,$57,$24,$52,$5B,$5B
        .byte   $5B,$5B,$5B,$58,$5C,$52,$5B,$5B
        .byte   $5B,$5B,$5B,$5B,$5D,$52,$5E,$5F
        .byte   $5E,$5F,$5E,$1D,$07,$07,$60,$61
        .byte   $60,$61,$60,$00,$62,$62,$63,$64
        .byte   $63,$64,$63,$00,$0E,$65,$1D,$07
        .byte   $66,$07,$07,$67,$28,$68,$00,$15
        .byte   $14,$15,$14,$15,$0E,$17,$00,$26
        .byte   $24,$26,$24,$26,$0C,$0D,$00,$69
        .byte   $1B,$1B,$1B,$1B,$2C,$0E,$00,$6A
        .byte   $03,$03,$03,$03,$00,$0E,$07,$07
        .byte   $07,$07,$07,$07,$07,$6B,$62,$62
        .byte   $62,$62,$62,$62,$62,$6C,$30,$0E
        .byte   $32,$6D,$0E,$65,$30,$6E,$6F,$0E
        .byte   $70,$71,$72,$73,$74,$75,$33,$0E
        .byte   $76,$77,$0E,$78,$79,$7A,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$45,$1B,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0F,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0E,$0F,$03,$03,$03
        .byte   $03,$03,$03,$03,$7B,$0F,$07,$07
        .byte   $07,$07,$07,$07,$47,$0F,$6E,$0E
        .byte   $0E,$41,$42,$0E,$01,$0F,$79,$65
        .byte   $0E,$7C,$7D,$0E,$0E,$0F,$28,$7E
        .byte   $3D,$0C,$0D,$0E,$0E,$0F,$2C,$0E
        .byte   $45,$2C,$0E,$45,$1B,$1C,$00,$0E
        .byte   $0F,$00,$0E,$0F,$03,$03,$00,$0E
        .byte   $0F,$00,$0E,$0F,$03,$03,$4D,$7F
        .byte   $80,$4F,$4F,$4F,$4F,$50,$4D,$7F
        .byte   $4D,$15,$14,$14,$1F,$52,$4D,$7F
        .byte   $4D,$1A,$19,$19,$29,$52,$4D,$4E
        .byte   $81,$82,$82,$83,$23,$52,$4D,$84
        .byte   $85,$86,$87,$17,$23,$52,$4D,$1A
        .byte   $0A,$0B,$0C,$0D,$29,$52,$58,$59
        .byte   $59,$59,$59,$59,$5C,$52,$5B,$5B
        .byte   $5B,$5B,$5B,$5B,$5D,$52,$5E,$5F
        .byte   $5E,$5F,$5E,$5F,$5E,$5F,$63,$64
        .byte   $63,$64,$63,$64,$63,$64,$1D,$07
        .byte   $88,$07,$07,$07,$07,$07,$00,$16
        .byte   $32,$32,$89,$32,$32,$0E,$00,$42
        .byte   $43,$8A,$8A,$8B,$76,$0E,$00,$0A
        .byte   $0B,$8C,$0B,$8C,$45,$1B,$00,$69
        .byte   $1B,$1B,$1B,$1B,$1C,$03,$00,$6A
        .byte   $03,$03,$03,$03,$03,$03,$5E,$5F
        .byte   $5E,$5F,$5E,$5F,$5E,$5F,$63,$64
        .byte   $63,$64,$63,$64,$63,$64,$07,$07
        .byte   $07,$07,$07,$08,$03,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0F,$03,$03,$0E,$0E
        .byte   $0E,$0E,$0E,$0F,$03,$03,$1B,$1B
        .byte   $1B,$2C,$0E,$0F,$03,$03,$03,$03
        .byte   $03,$00,$0E,$0F,$03,$03,$03,$03
        .byte   $8D,$8E,$0E,$0F,$03,$03,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$19,$19,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$8F,$8F,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$03,$03
        .byte   $00,$19,$19,$0F,$03,$03,$03,$03
        .byte   $00,$8F,$8F,$0F,$03,$03,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$03,$03
        .byte   $00,$19,$19,$0F,$03,$03,$03,$03
        .byte   $00,$8F,$8F,$0F,$03,$03,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$03,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$19,$19,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$24,$24,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$19,$19,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$8F,$8F,$52,$5B,$5B,$5B,$5B
        .byte   $4D,$0E,$0E,$52,$5B,$5B,$03,$03
        .byte   $00,$0E,$0E,$0F,$03,$5F,$1D,$07
        .byte   $67,$19,$19,$6C,$90,$91,$00,$14
        .byte   $1F,$24,$24,$14,$1F,$14,$00,$92
        .byte   $93,$92,$92,$92,$93,$92,$00,$24
        .byte   $23,$24,$24,$24,$23,$24,$00,$24
        .byte   $23,$24,$24,$24,$23,$24,$2E,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1B,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$5E,$03
        .byte   $2F,$03,$5F,$5E,$03,$2F,$94,$07
        .byte   $2F,$07,$91,$94,$07,$2F,$14,$95
        .byte   $96,$97,$14,$1F,$95,$96,$92,$98
        .byte   $0E,$99,$92,$93,$98,$0E,$24,$9A
        .byte   $0E,$9B,$24,$23,$9A,$0E,$45,$2C
        .byte   $9C,$45,$1B,$1B,$2C,$9C,$1C,$00
        .byte   $9C,$0F,$03,$03,$00,$9D,$03,$00
        .byte   $9C,$0F,$03,$03,$00,$9C,$03,$5F
        .byte   $5E,$03,$2F,$03,$5F,$5E,$07,$91
        .byte   $94,$07,$2F,$07,$91,$9E,$97,$14
        .byte   $1F,$95,$96,$97,$14,$0F,$99,$92
        .byte   $93,$98,$0E,$99,$92,$0F,$9B,$24
        .byte   $23,$9A,$0E,$9B,$24,$0F,$45,$1B
        .byte   $1B,$2C,$9C,$45,$9F,$0F,$0F,$03
        .byte   $03,$00,$9C,$0F,$7B,$0F,$0F,$03
        .byte   $03,$00,$9D,$0F,$A0,$0F,$1D,$07
        .byte   $07,$10,$11,$A1,$A2,$A3,$00,$15
        .byte   $14,$14,$15,$0E,$0E,$0F,$00,$A4
        .byte   $92,$92,$A4,$0E,$45,$1C,$00,$26
        .byte   $24,$45,$1B,$1B,$1C,$03,$00,$A4
        .byte   $92,$6C,$07,$07,$08,$1D,$00,$26
        .byte   $24,$14,$15,$0E,$A5,$8E,$A6,$1B
        .byte   $1B,$1B,$1B,$1B,$1B,$1B,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$8D,$A7,$A7,$A7,$A7,$03,$03
        .byte   $1D,$67,$95,$89,$0E,$0E,$03,$1D
        .byte   $67,$A8,$A9,$AA,$AB,$22,$1D,$67
        .byte   $14,$24,$AC,$3B,$AD,$28,$67,$AE
        .byte   $92,$92,$3E,$38,$0E,$0E,$0E,$9B
        .byte   $24,$45,$AF,$0E,$0E,$0E,$1B,$1B
        .byte   $1B,$1C,$00,$0E,$0E,$0E,$03,$03
        .byte   $03,$03,$00,$0E,$0E,$0E,$A7,$A7
        .byte   $A7,$A7,$A7,$A7,$A7,$A7,$0E,$6E
        .byte   $32,$89,$32,$89,$0E,$79,$B0,$B1
        .byte   $AA,$B2,$76,$B3,$AB,$22,$68,$B4
        .byte   $76,$0E,$0E,$44,$AD,$28,$0E,$B5
        .byte   $0E,$32,$0E,$38,$0E,$0E,$0E,$B6
        .byte   $0E,$B7,$0E,$0E,$B8,$0E,$0E,$B6
        .byte   $0E,$B9,$0E,$0E,$BA,$0E,$0E,$B6
        .byte   $0E,$B9,$0E,$0E,$BA,$0E,$A7,$A7
        .byte   $A7,$A7,$A7,$BB,$03,$03,$79,$6E
        .byte   $97,$14,$14,$6C,$08,$03,$B0,$BC
        .byte   $BD,$19,$19,$BE,$6C,$08,$68,$B4
        .byte   $9B,$24,$24,$26,$14,$6C,$0E,$33
        .byte   $BD,$19,$19,$1A,$19,$BF,$0E,$0E
        .byte   $45,$2C,$24,$26,$24,$C0,$0E,$0E
        .byte   $0F,$2E,$1B,$1B,$1B,$1B,$0E,$0E
        .byte   $0F,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$07,$07
        .byte   $07,$07,$07,$C1,$C2,$C3,$C4,$15
        .byte   $14,$14,$14,$15,$19,$C5,$C6,$26
        .byte   $24,$24,$24,$26,$C7,$C8,$1B,$1B
        .byte   $1B,$1B,$1B,$1B,$1C,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$C9,$07
        .byte   $07,$07,$07,$07,$07,$CA,$CB,$15
        .byte   $0E,$0E,$32,$32,$1F,$CC,$CD,$1A
        .byte   $41,$42,$CE,$CF,$29,$D0,$D1,$26
        .byte   $D2,$D3,$3B,$D4,$23,$D5,$D6,$1A
        .byte   $0E,$2B,$D7,$38,$29,$D0,$D8,$D9
        .byte   $DA,$DB,$DC,$DD,$23,$D5,$03,$03
        .byte   $03,$03,$03,$DC,$1B,$DB,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$0E,$0E
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
        .byte   $0E,$0E,$0E,$0E,$0E,$0E,$1F,$2E
        .byte   $17,$2A,$00,$07,$00,$07,$00,$2C
        .byte   $00,$28,$1F,$1E,$17,$16,$00,$07
        .byte   $A6,$07,$00,$2C,$00,$30,$08,$09
        .byte   $35,$31,$1F,$1E,$35,$31,$1F,$1E
        .byte   $3D,$16,$A2,$A3,$AA,$00,$A3,$A4
        .byte   $00,$AC,$A5,$B6,$00,$BE,$B6,$B7
        .byte   $BF,$BE,$A8,$A9,$00,$00,$00,$00
        .byte   $00,$00,$28,$1E,$2C,$16,$1F,$3E
        .byte   $35,$31,$21,$25,$35,$31,$21,$26
        .byte   $35,$32,$06,$00,$07,$00,$74,$74
        .byte   $6C,$6C,$74,$75,$6C,$6D,$00,$00
        .byte   $00,$A7,$00,$00,$A0,$A1,$07,$00
        .byte   $07,$00,$64,$64,$6C,$6C,$64,$65
        .byte   $6C,$6D,$21,$25,$17,$16,$3B,$1E
        .byte   $17,$16,$1F,$1E,$17,$38,$0A,$0B
        .byte   $35,$31,$73,$74,$6B,$6C,$75,$74
        .byte   $6D,$6C,$00,$A6,$A5,$A2,$00,$00
        .byte   $A3,$A3,$6B,$6C,$6B,$6C,$6C,$6C
        .byte   $6C,$6C,$6D,$6C,$6D,$6C,$6C,$6D
        .byte   $6C,$6D,$A5,$A2,$00,$AA,$A3,$A3
        .byte   $00,$00,$63,$64,$6B,$6C,$65,$64
        .byte   $6D,$6C,$00,$24,$00,$28,$21,$26
        .byte   $17,$2A,$00,$06,$00,$07,$1F,$3E
        .byte   $17,$16,$14,$15,$14,$15,$B0,$00
        .byte   $B8,$00,$1A,$1B,$00,$00,$00,$B0
        .byte   $00,$B8,$B1,$00,$00,$00,$B8,$00
        .byte   $B8,$00,$00,$00,$A6,$00,$00,$B1
        .byte   $00,$A7,$00,$B8,$00,$B8,$00,$B1
        .byte   $00,$00,$00,$BC,$A0,$A1,$B7,$BA
        .byte   $B8,$00,$B9,$B7,$00,$B8,$BD,$00
        .byte   $00,$00,$B6,$B6,$BE,$BF,$20,$26
        .byte   $2C,$2A,$3B,$3E,$17,$16,$00,$B1
        .byte   $A6,$00,$A2,$A3,$A2,$A3,$A3,$A4
        .byte   $A3,$A4,$B9,$B7,$00,$00,$BC,$B7
        .byte   $00,$B8,$20,$25,$2C,$16,$2A,$07
        .byte   $2A,$07,$2A,$07,$36,$07,$B1,$24
        .byte   $00,$28,$BA,$B9,$00,$00,$A5,$2C
        .byte   $24,$3B,$BD,$00,$24,$21,$20,$25
        .byte   $3F,$16,$17,$2A,$1F,$2E,$07,$28
        .byte   $07,$34,$17,$16,$31,$35,$17,$16
        .byte   $39,$1E,$07,$74,$07,$6C,$2C,$16
        .byte   $28,$1E,$07,$64,$07,$6C,$24,$22
        .byte   $28,$2A,$24,$21,$28,$1E,$3F,$3A
        .byte   $1F,$1E,$25,$22,$1F,$2A,$17,$3A
        .byte   $1F,$1E,$25,$21,$1F,$1E,$3F,$16
        .byte   $1F,$1E,$17,$16,$1F,$1E,$26,$06
        .byte   $2A,$07,$2E,$07,$2A,$07,$1F,$1E
        .byte   $41,$16,$1F,$1E,$17,$40,$49,$1E
        .byte   $41,$16,$1F,$48,$17,$40,$66,$6E
        .byte   $00,$00,$49,$1E,$17,$16,$1F,$48
        .byte   $17,$16,$00,$00,$A7,$00,$0C,$0B
        .byte   $35,$31,$1F,$2E,$35,$32,$A4,$B9
        .byte   $AC,$00,$06,$24,$07,$28,$07,$2C
        .byte   $07,$28,$1F,$1E,$39,$16,$28,$1E
        .byte   $34,$31,$00,$00,$00,$A6,$00,$00
        .byte   $B0,$00,$B7,$00,$B8,$00,$BC,$B7
        .byte   $BC,$B7,$BA,$A2,$BA,$A2,$A3,$A3
        .byte   $A3,$A3,$A4,$B9,$A4,$B9,$B7,$BD
        .byte   $B7,$BD,$B8,$00,$B1,$A6,$00,$B8
        .byte   $00,$B1,$00,$AA,$00,$00,$AC,$00
        .byte   $00,$00,$B8,$00,$B1,$00,$00,$A2
        .byte   $00,$AA,$2A,$07,$2E,$07,$AA,$00
        .byte   $00,$00,$00,$AC,$A0,$A1,$A4,$A5
        .byte   $AC,$00,$07,$28,$07,$2C,$17,$16
        .byte   $1F,$3C,$17,$3A,$31,$35,$25,$21
        .byte   $31,$35,$25,$22,$31,$36,$6C,$75
        .byte   $6C,$6D,$00,$A7,$A3,$A4,$B1,$B8
        .byte   $00,$B1,$B1,$B1,$00,$00,$0D,$0B
        .byte   $35,$31,$00,$00,$00,$B0,$BB,$B7
        .byte   $00,$00,$BD,$B8,$00,$B1,$B6,$B7
        .byte   $BF,$BF,$1F,$3C,$17,$2A,$31,$36
        .byte   $00,$00,$6C,$6C,$67,$67,$08,$0E
        .byte   $35,$31,$1F,$48,$35,$31,$6C,$6C
        .byte   $64,$64,$6B,$6C,$63,$64,$49,$1E
        .byte   $35,$31,$75,$00,$6D,$00,$1C,$1D
        .byte   $00,$00,$00,$73,$00,$6B,$6D,$00
        .byte   $65,$00,$00,$6B,$00,$63,$6D,$00
        .byte   $6D,$00,$00,$6B,$00,$6B,$12,$13
        .byte   $10,$11,$12,$12,$10,$11,$49,$1E
        .byte   $3D,$16,$22,$06,$2E,$07,$2E,$07
        .byte   $2E,$07,$3B,$1E,$35,$31,$2A,$00
        .byte   $36,$00,$28,$1E,$28,$16,$6C,$6D
        .byte   $64,$65,$30,$35,$00,$00,$1F,$3A
        .byte   $17,$16,$31,$35,$6E,$66,$74,$74
        .byte   $64,$64,$6D,$A7,$65,$A4,$00,$B8
        .byte   $B9,$B7,$00,$A6,$BA,$A2,$6D,$A4
        .byte   $6D,$AC,$BA,$A2,$00,$AA,$00,$73
        .byte   $00,$63,$3B,$2E,$17,$2A,$A7,$00
        .byte   $A4,$B9,$B8,$00,$B7,$BA,$00,$B8
        .byte   $BD,$B1,$00,$B8,$BC,$B7,$B7,$BD
        .byte   $B8,$00,$B1,$00,$00,$90,$00,$98
        .byte   $00,$98,$90,$B8,$98,$B1,$90,$90
        .byte   $98,$98,$98,$00,$98,$00,$98,$98
        .byte   $98,$98,$39,$1E,$2C,$16,$B8,$00
        .byte   $B7,$BD,$00,$63,$00,$6B,$74,$74
        .byte   $6C,$6D,$74,$0F,$6C,$0F,$6C,$0F
        .byte   $6C,$0F,$1F,$3C,$35,$32,$31,$35
        .byte   $74,$74,$31,$35,$74,$0F,$02,$74
        .byte   $03,$6C,$64,$0F,$6C,$0F,$03,$6C
        .byte   $03,$6C,$6C,$6C,$24,$21,$6C,$0F
        .byte   $25,$21,$1F,$1E,$3C,$31,$1F,$1E
        .byte   $35,$39,$2A,$74,$2E,$6C,$74,$2C
        .byte   $6C,$28,$2A,$64,$2E,$6C,$B9,$B7
        .byte   $B9,$B7,$BB,$B7,$BB,$B7,$64,$2C
        .byte   $6C,$28,$32,$6C,$02,$6C,$A2,$A3
        .byte   $00,$00,$A3,$A4,$00,$00,$BB,$B7
        .byte   $00,$B8,$6C,$2C,$6C,$28,$01,$64
        .byte   $03,$6C,$22,$B1,$2E,$00,$03,$6C
        .byte   $25,$21,$6C,$6D,$25,$21,$00,$24
        .byte   $25,$3B,$21,$3F,$17,$16,$3A,$25
        .byte   $17,$16,$22,$00,$2E,$00,$00,$91
        .byte   $00,$00,$00,$00,$91,$00,$C0,$85
        .byte   $85,$80,$86,$87,$81,$88,$8D,$8E
        .byte   $82,$80,$8F,$C7,$81,$84,$85,$86
        .byte   $80,$81,$82,$88,$88,$82,$89,$82
        .byte   $80,$81,$80,$81,$88,$89,$82,$89
        .byte   $82,$80,$12,$13,$12,$12,$12,$13
        .byte   $13,$12,$12,$12,$13,$12,$90,$00
        .byte   $98,$00,$00,$90,$00,$98,$00,$00
        .byte   $00,$90,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$23
        .byte   $00,$33,$04,$0E,$0E,$0E,$27,$2A
        .byte   $27,$29,$27,$27,$2D,$1E,$06,$08
        .byte   $84,$86,$0A,$0C,$40,$42,$30,$20
        .byte   $1A,$1C,$0A,$0C,$60,$62,$44,$4D
        .byte   $46,$44,$44,$4D,$46,$44,$48,$00
        .byte   $4A,$48,$48,$00,$4A,$48,$64,$4E
        .byte   $66,$00,$64,$4E,$66,$00,$68,$4E
        .byte   $4A,$7A,$68,$4E,$4A,$7A,$80,$82
        .byte   $88,$8A,$11,$13,$8C,$8E,$A0,$A2
        .byte   $A8,$AA,$10,$10,$AC,$AE,$C1,$82
        .byte   $06,$16,$08,$09,$00,$00,$35,$00
        .byte   $06,$06,$C5,$00,$9D,$00,$44,$4D
        .byte   $46,$22,$23,$23,$6C,$33,$48,$00
        .byte   $4A,$32,$33,$33,$6C,$08,$64,$4E
        .byte   $66,$00,$00,$00,$33,$9D,$68,$4E
        .byte   $4A,$7A,$8C,$00,$8C,$00,$CC,$CE
        .byte   $CA,$00,$00,$00,$00,$00,$EC,$EE
        .byte   $00,$00,$EB,$00,$00,$00,$11,$11
        .byte   $01,$01,$01,$01,$01,$01,$C5,$01
        .byte   $01,$01,$01,$01,$01,$01,$00,$00
        .byte   $AC,$C8,$C7,$C3,$00,$00,$F1,$E3
        .byte   $00,$01,$F7,$01,$01,$01,$E5,$4C
        .byte   $01,$01,$01,$01,$20,$8E,$E0,$C3
        .byte   $AD,$AD,$00,$AD,$AE,$B4,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$23
        .byte   $00,$33,$05,$0F,$0F,$0F,$28,$2F
        .byte   $2A,$2F,$2B,$2C,$2E,$1F,$07,$09
        .byte   $85,$87,$0B,$0D,$41,$43,$21,$31
        .byte   $1B,$1D,$0B,$0D,$61,$63,$45,$4D
        .byte   $47,$47,$45,$4D,$47,$47,$49,$00
        .byte   $4B,$4B,$49,$00,$4B,$4B,$65,$4F
        .byte   $67,$00,$65,$4F,$67,$00,$4F,$6B
        .byte   $79,$49,$4F,$6B,$79,$49,$81,$83
        .byte   $89,$8B,$11,$10,$8D,$8F,$A1,$A3
        .byte   $A9,$AB,$10,$14,$AD,$AF,$81,$C0
        .byte   $07,$17,$08,$09,$00,$00,$00,$36
        .byte   $06,$06,$C6,$00,$9D,$00,$45,$4D
        .byte   $47,$23,$23,$24,$6D,$33,$49,$00
        .byte   $4B,$33,$33,$34,$6D,$08,$65,$4F
        .byte   $67,$00,$00,$00,$9D,$33,$4F,$6B
        .byte   $79,$49,$00,$8D,$00,$8D,$CD,$CF
        .byte   $CB,$00,$00,$00,$00,$00,$ED,$EF
        .byte   $00,$EA,$00,$00,$00,$00,$12,$12
        .byte   $01,$01,$01,$01,$01,$01,$C6,$01
        .byte   $01,$01,$01,$01,$01,$01,$00,$00
        .byte   $C7,$C9,$AD,$C3,$00,$00,$E2,$E4
        .byte   $F7,$01,$00,$01,$01,$01,$E6,$5F
        .byte   $01,$01,$01,$01,$21,$8F,$C2,$AC
        .byte   $C3,$AC,$AC,$00,$AF,$B5,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$33
        .byte   $33,$33,$0E,$25,$0E,$0E,$37,$3A
        .byte   $37,$39,$37,$37,$3D,$1E,$16,$18
        .byte   $94,$96,$0A,$0C,$50,$52,$30,$20
        .byte   $00,$00,$1A,$1C,$70,$72,$54,$5C
        .byte   $56,$54,$54,$5C,$56,$54,$48,$00
        .byte   $5A,$48,$48,$00,$5A,$48,$74,$5E
        .byte   $76,$00,$74,$5E,$76,$00,$5A,$6A
        .byte   $78,$5C,$5A,$6A,$78,$5C,$90,$92
        .byte   $98,$9A,$10,$13,$9C,$9E,$B0,$B2
        .byte   $B8,$BA,$12,$10,$BC,$BE,$D1,$92
        .byte   $06,$16,$18,$19,$00,$00,$35,$00
        .byte   $06,$06,$D5,$00,$00,$9C,$01,$01
        .byte   $01,$32,$33,$33,$7C,$15,$01,$00
        .byte   $01,$32,$33,$33,$7C,$18,$01,$01
        .byte   $01,$32,$33,$33,$8C,$00,$01,$01
        .byte   $01,$01,$8C,$00,$33,$9C,$DC,$DE
        .byte   $DA,$00,$FB,$00,$E8,$F8,$FC,$FE
        .byte   $00,$00,$FB,$EA,$E9,$F9,$D5,$13
        .byte   $01,$01,$01,$01,$01,$01,$D5,$01
        .byte   $01,$01,$01,$01,$01,$01,$E1,$D3
        .byte   $BC,$D8,$D7,$C3,$00,$E7,$F0,$F3
        .byte   $00,$01,$00,$01,$01,$01,$58,$F5
        .byte   $01,$01,$01,$01,$30,$9E,$E0,$C3
        .byte   $BD,$BD,$00,$BD,$BE,$B6,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$33
        .byte   $33,$33,$0F,$26,$0F,$0F,$38,$3F
        .byte   $3A,$3F,$3B,$3C,$3E,$1F,$17,$19
        .byte   $95,$97,$0B,$0D,$51,$53,$21,$31
        .byte   $00,$00,$1B,$1D,$71,$73,$55,$5D
        .byte   $57,$57,$55,$5D,$57,$57,$59,$00
        .byte   $4B,$4B,$59,$00,$4B,$4B,$75,$5E
        .byte   $77,$00,$75,$5E,$77,$00,$69,$59
        .byte   $5D,$7B,$69,$59,$5D,$7B,$91,$93
        .byte   $99,$9B,$10,$10,$9D,$9F,$B1,$B3
        .byte   $B9,$BB,$12,$14,$BD,$BF,$91,$D0
        .byte   $07,$17,$18,$19,$00,$00,$00,$36
        .byte   $06,$7F,$D6,$00,$00,$9C,$55,$5D
        .byte   $57,$33,$33,$34,$7D,$15,$59,$00
        .byte   $4B,$33,$33,$34,$7D,$18,$75,$5E
        .byte   $77,$33,$33,$34,$00,$8D,$69,$59
        .byte   $5D,$7B,$00,$8D,$9C,$33,$DD,$DF
        .byte   $DB,$FA,$00,$EA,$E9,$F9,$FD,$FF
        .byte   $00,$FA,$00,$E8,$F8,$00,$D6,$14
        .byte   $01,$01,$01,$01,$01,$01,$D6,$01
        .byte   $01,$01,$01,$01,$01,$01,$D2,$D4
        .byte   $D7,$D9,$BD,$C3,$E7,$00,$F2,$F4
        .byte   $00,$01,$00,$01,$01,$01,$5B,$F6
        .byte   $01,$01,$01,$01,$31,$9F,$C2,$BC
        .byte   $C3,$BC,$BC,$00,$BF,$B7,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$10
        .byte   $10,$10,$40,$20,$40,$20,$11,$12
        .byte   $11,$12,$11,$11,$12,$10,$03,$03
        .byte   $13,$13,$10,$10,$10,$10,$03,$03
        .byte   $10,$10,$10,$10,$10,$10,$11,$11
        .byte   $11,$11,$12,$12,$12,$12,$11,$11
        .byte   $11,$11,$12,$12,$12,$12,$11,$11
        .byte   $11,$00,$12,$12,$12,$00,$11,$11
        .byte   $11,$11,$12,$12,$12,$12,$11,$12
        .byte   $50,$50,$10,$10,$00,$00,$10,$10
        .byte   $51,$51,$10,$10,$00,$00,$11,$12
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$02,$00,$00,$00,$13,$13
        .byte   $13,$00,$00,$00,$51,$00,$13,$13
        .byte   $13,$00,$00,$00,$52,$00,$13,$13
        .byte   $13,$00,$00,$00,$00,$00,$13,$13
        .byte   $13,$13,$00,$00,$00,$00,$63,$63
        .byte   $63,$63,$63,$63,$63,$63,$63,$63
        .byte   $00,$63,$63,$63,$63,$63,$10,$10
        .byte   $00,$00,$00,$00,$00,$00,$10,$00
        .byte   $00,$00,$00,$00,$00,$00,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$00,$03,$00,$00,$00,$03,$03
        .byte   $00,$00,$00,$00,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00
