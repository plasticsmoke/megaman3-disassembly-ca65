; ===========================================================================
; RESET, BANK SWITCHING & COOPERATIVE SCHEDULER — $FD52-$FFFF
; ===========================================================================
; Contains:
;   boss_frame_yield          — boss AI frame yield ($5A=active, skip player)
;   process_frame_yield_full  — saves both banks, $97=$04 (includes player)
;   process_frame_yield       — saves both banks, uses caller's $97
;   call_bank0E_A006          — bank $0E trampoline, calls $A006 with X=$B8
;   call_bank0E_A003          — bank $0E trampoline, calls $A003
;   RESET                     — power-on initialization
;   task_scheduler            — cooperative multitasking (4 task slots)
;   task_register/kill/yield  — coroutine primitives
;   update_CHR_banks          — flag CHR bank refresh for NMI
;   select_CHR_banks          — write CHR bank registers to MMC3
;   process_frame_and_yield   — run one frame of game logic, yield to NMI
;   select_PRG_banks          — MMC3 PRG bank switch with race handling
;   play_sounds               — drain circular sound buffer via bank $16/$17
; ===========================================================================

; ===========================================================================
; Frame-yield trampolines — process one frame, yield to NMI, restore banks
; ===========================================================================
; These wrappers let banked code (boss AI, cutscenes, level transitions)
; run one frame of entity processing + sprite update, then yield to the
; cooperative scheduler until NMI completes. They save/restore PRG bank
; state across the yield so the caller doesn't need to worry about it.
;
; $97 controls OAM start offset for update_entity_sprites:
;   $04 = include player sprites (default, set by process_frame_yield_full)
;   $0C = skip player sprites (set by boss_frame_yield)
;
; Variants:
;   boss_frame_yield      — sets $5A (boss active), $97=$0C, saves $F5
;   process_frame_yield_full — saves both banks, $97=$04 (includes player)
;   process_frame_yield   — saves both banks, uses caller's $97
;   call_bank0E_A006      — bank $0E trampoline, calls $A006 with X=$B8
;   call_bank0E_A003      — bank $0E trampoline, calls $A003
; ---------------------------------------------------------------------------

; --- boss_frame_yield ---
; Called from bank03 (boss intro sequences). Sets boss-active flag, processes
; entities (skipping player sprites), yields, then restores banks.
boss_frame_yield:

        lda     #$80                    ; set boss-active flag
        sta     boss_active             ; (bit 7 = boss fight in progress)
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        lda     #$0C                    ; $97 = $0C: OAM offset past player
        sta     oam_ptr                 ; (skip first 3 sprite entries)
        jsr     process_frame_and_yield ; process entities + yield one frame
        lda     #$00                    ; clear boss-active flag
        sta     boss_active             ; boss fight no longer in progress
        lda     #$18                    ; restore $8000 bank to $18
        sta     mmc3_select             ; (bank1C_1D fixed pair)
        pla                             ; restore $A000 bank
        sta     prg_bank                ; restore $A000 bank from stack
        jmp     select_PRG_banks        ; re-select banks and return

; --- process_frame_yield_full ---
; Saves both PRG banks, sets $97=$04, processes frame, restores banks.
; Called from bank0C/0B (level loading, cutscenes), bank18 (stage select).
process_frame_yield_full:

        lda     mmc3_select             ; save both PRG banks
        pha                             ; push $8000 bank to stack
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        jsr     process_frame_yield_with_player ; $97=$04, process + yield
restore_banks:  pla                     ; restore $A000 bank
        sta     prg_bank                ; write back $A000 bank
        pla                             ; restore $8000 bank
        sta     mmc3_select             ; write back $8000 bank
        jmp     select_PRG_banks        ; re-select and return

; --- process_frame_yield ---
; Same as above but uses caller's $97 value (doesn't set $04).
; Called from bank0B/0F (level transitions with custom OAM offset).
process_frame_yield:

        lda     mmc3_select             ; save both PRG banks
        pha                             ; push $8000 bank to stack
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        jsr     process_frame_and_yield ; process + yield (caller's $97)
        jmp     restore_banks           ; restore banks and return

; --- call_bank0E_A006 ---
; Switches $A000-$BFFF to bank $0E, calls entry point $A006 with X=$B8.
; Called from bank12 (entity AI).
call_bank0E_A006:

        stx     $0F                     ; save X
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        lda     #$0E                    ; switch $A000 to bank $0E
        sta     prg_bank                ; set $A000 bank = $0E
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $B8                     ; X = parameter from $B8
        jsr     banked_A006             ; call bank $0E entry point
restore_A000:
        pla                             ; restore $A000 bank
        sta     prg_bank                ; write back $A000 bank
        jsr     select_PRG_banks        ; apply bank switch
        ldx     $0F                     ; restore X
        rts                             ; return to caller

; --- call_bank0E_A003 ---
; Switches $A000-$BFFF to bank $0E, calls entry point $A003.
; Called from bank12 (entity AI).
call_bank0E_A003:

        stx     $0F                     ; save X
        lda     prg_bank                ; save $A000 bank
        pha                             ; push $A000 bank to stack
        lda     #$0E                    ; switch $A000 to bank $0E
        sta     prg_bank                ; set $A000 bank = $0E
        jsr     select_PRG_banks        ; apply bank switch
        jsr     banked_A003             ; call bank $0E entry point
        jmp     restore_A000            ; restore bank and return

; unused data / padding before RESET vector
        .byte   $8A,$40,$A3,$00,$0F
        .byte   $04,$4B,$50,$80,$04,$18,$10,$E0
        .byte   $00,$64,$04,$C5,$45,$67,$50,$CA
        .byte   $11,$1B,$51,$BA,$00,$44,$00,$3E
        .byte   $00,$A2,$04,$99,$00,$DD,$04,$81
        .byte   $10,$2B,$11,$80,$01,$4A,$40,$9C
        .byte   $01,$47,$15,$F7,$11,$47,$44,$17
        .byte   $40,$C2,$40,$28,$11,$CB,$44,$6E
        .byte   $50,$8A,$54,$AE,$10,$2B
        .byte   $00,$53,$05,$5D,$15
RESET:  sei                             ; disable interrupts
        cld                             ; clear decimal mode
        lda     #$08                    ; PPUCTRL: sprite table = $1000
        sta     PPUCTRL                 ; (NMI not yet enabled)
        lda     #$40                    ; APU frame counter: disable IRQ
        sta     APU_FRAME               ; write $40 to APU frame counter
        ldx     #$00                    ; X = 0 for clearing registers
        stx     PPUMASK                 ; PPUMASK: rendering off
        stx     DMC_FREQ                ; DMC: disable
        stx     SND_CHN                 ; APU status: silence all channels
        dex                             ; stack pointer = $FF
        txs                             ; set stack pointer to $01FF

; --- wait for PPU warm-up (4 VBlank cycles) ---
        ldx     #$04                    ; 4 iterations
ppu_vblank_wait_set:  lda     PPUSTATUS ; wait for VBlank flag set
        bpl     ppu_vblank_wait_set     ; loop until bit 7 set
ppu_vblank_wait_clear:  lda     PPUSTATUS ; wait for VBlank flag clear
        bmi     ppu_vblank_wait_clear   ; loop until bit 7 clear
        dex                             ; repeat 4 times
        bne     ppu_vblank_wait_set     ; next VBlank cycle

; --- exercise PPU address bus ---
        lda     PPUSTATUS               ; reset PPU latch
        lda     #$10                    ; toggle PPUADDR between $1010
        tay                             ; and $0000, 16 times
ppu_address_write:  sta     PPUADDR     ; write high byte
        sta     PPUADDR                 ; write low byte
        eor     #$10                    ; toggle $10 ↔ $00
        dey                             ; decrement iteration counter
        bne     ppu_address_write       ; loop 16 times

; --- clear zero page ($00-$FF) ---
        tya                             ; A = 0 (Y wrapped to 0)
zeropage_clear_loop:  sta     temp_00,y ; clear byte at Y
        dey                             ; Y: 0, $FF, $FE, ..., $01
        bne     zeropage_clear_loop     ; loop all 256 bytes

; --- clear RAM pages $01-$06 ($0100-$06FF) ---
ram_clear_page_advance:  inc     $01    ; advance page pointer ($01→$06)
ram_clear_loop:  sta     (temp_00),y    ; clear byte via ($00/$01)+Y
        iny                             ; next byte in page
        bne     ram_clear_loop          ; loop 256 bytes per page
        ldx     $01                     ; check page counter
        cpx     #$07                    ; stop after page $06
        bne     ram_clear_page_advance  ; more pages → continue

; --- initialize sound buffer ($DC-$E3) to $88 (no sound) ---
        ldy     #$07                    ; 8 sound buffer bytes
        lda     #$88                    ; $88 = "no sound" sentinel
sound_buffer_clear_loop:  sta     $DC,x ; X=7..0 → $E3..$DC
        dex                             ; next buffer byte
        bpl     sound_buffer_clear_loop ; loop all 8 bytes

; --- hardware/bank initialization ---
        lda     #$18                    ; PPUMASK shadow: show sprites + BG
        sta     ppu_mask_shadow         ; store PPU mask shadow
        lda     #$00                    ; MMC3 mirroring: vertical
        sta     MMC3_MIRRORING          ; set vertical mirroring
        ldx     #$1C                    ; $F4/$F5 = initial PRG banks
        stx     mmc3_select             ; bank $1C at $8000-$9FFF
        inx                             ; bank $1D at $A000-$BFFF
        stx     prg_bank                ; bank $1D at $A000-$BFFF
        jsr     select_PRG_banks        ; apply initial bank switch
        lda     #$40                    ; CHR bank setup:
        sta     $E8                     ; $E8=$40 (2KB bank 0)
        lda     #$42                    ; $E9=$42 (2KB bank 1)
        sta     $E9                     ; $EA=$00 (1KB bank 2)
        lda     #$00                    ; $EA=$00 (1KB bank 2)
        sta     $EA                     ; store 1KB CHR bank 2
        lda     #$01                    ; $EB=$01 (1KB bank 3)
        sta     $EB                     ; store 1KB CHR bank 3
        lda     #$0A                    ; $EC=$0A (1KB bank 4)
        sta     $EC                     ; store 1KB CHR bank 4
        lda     #$0B                    ; $ED=$0B (1KB bank 5)
        sta     $ED                     ; store 1KB CHR bank 5
        jsr     update_CHR_banks        ; apply CHR bank configuration
        jsr     prepare_oam_buffer      ; init OAM / sprite state
        lda     #$20                    ; clear nametable 0 ($2000)
        ldx     #$00                    ; A=addr hi, X=fill, Y=attr fill
        ldy     #$00                    ; X = fill byte ($00)
        jsr     fill_nametable          ; clear nametable 0
        lda     #$24                    ; clear nametable 1 ($2400)
        ldx     #$00                    ; X = fill byte ($00)
        ldy     #$00                    ; Y = attribute fill ($00)
        jsr     fill_nametable          ; clear nametable 1

; --- register main game task (slot 0, address $C8D0) ---
        lda     #$C8                    ; $93/$94 = $C8D0 (main game entry)
        sta     $94                     ; (in bank $1C, always-mapped range)
        lda     #$D0                    ; low byte of $C8D0
        sta     task_ptr                ; low byte of entry address
        lda     #$00                    ; A = slot 0
        jsr     task_register           ; register task with address
        lda     #$88                    ; PPUCTRL: NMI enable + sprite $1000
        sta     ppu_ctrl_shadow         ; store in PPUCTRL shadow

; fall through to scheduler
; ===========================================================================
; task_scheduler — cooperative multitasking scheduler
; ===========================================================================
; MM3 uses a simple cooperative scheduler with 4 task slots at $80-$8F.
; Each slot is 4 bytes:
;   byte 0 ($80,x): state — $00=free, $01=sleeping, $02=running,
;                            $04=ready (woken by NMI), $08=fresh (has address)
;   byte 1 ($81,x): sleep countdown (decremented by NMI when state=$01)
;   byte 2 ($82,x): saved stack pointer (state $04) or address low (state $08)
;   byte 3 ($83,x): address high (state $08 only)
;
; The scheduler spins until a task with state >= $04 is found, then either:
;   state $08: JMP to address stored in bytes 2-3 (fresh task launch)
;   state $04: restore stack pointer from byte 2 and RTS (resume coroutine)
;
; NMI decrements sleeping tasks' countdowns and sets state $04 when done.
; Task slot 0 gets controller input read on resume (read_controllers).
;
; nmi_occurred = NMI flag (set $FF by NMI, forces rescan)
; $91 = current task slot index (0-3)
; ---------------------------------------------------------------------------
task_scheduler:  ldx     #$FF           ; reset stack to top
        txs                             ; (discard all coroutine frames)
task_scheduler_state_ready:  ldx     #$00 ; clear NMI flag
        stx     nmi_occurred            ; clear NMI flag
        ldy     #$04                    ; 4 slots to check
task_scheduler_state_check:  lda     $80,x ; state >= $04? (ready or fresh)
        cmp     #$04                    ; state >= $04? (ready/fresh)
        bcs     task_scheduler_nmi_check ; yes → found runnable task
        inx                             ; advance to next slot (+4 bytes)
        inx                             ; advance 4 bytes per slot
        inx                             ; (continued)
        inx                             ; (continued)
        dey                             ; more slots?
        bne     task_scheduler_state_check ; more slots → keep scanning
        jmp     task_scheduler_state_ready ; no runnable tasks, spin until NMI

task_scheduler_nmi_check:  lda     nmi_occurred ; if NMI fired during scan,
        bne     task_scheduler_state_ready ; rescan (states may have changed)
        dey                             ; convert Y countdown → slot index
        tya                             ; Y=3→0, Y=2→1, Y=1→2, Y=0→3
        eor     #$03                    ; invert to get slot index
        sta     $91                     ; $91 = current slot index
        ldy     $80,x                   ; Y = task state
        lda     #$02                    ; mark slot as running
        sta     $80,x                   ; mark task as running
        cpy     #$08                    ; state $08? → fresh task (JMP)
        bne     task_scheduler_restore_sp ; state $08? → fresh task
        lda     $82,x                   ; load address from slot
        sta     task_ptr                ; store address low byte
        lda     $83,x                   ; load address high byte
        sta     $94                     ; store address high byte
        jmp     (task_ptr)              ; launch task at stored address

task_scheduler_restore_sp:  lda     $82,x ; restore saved stack pointer
        tax                             ; stack pointer → X
        txs                             ; restore stack pointer
        lda     $91                     ; slot 0? read controllers first
        bne     task_scheduler_restore_regs ; (only main game task gets input)
        jsr     read_controllers        ; read_controllers
task_scheduler_restore_regs:  pla       ; restore Y and X from stack
        tay                             ; (saved by task_yield)
        pla                             ; restore X from stack
        tax                             ; restore X from stack
        rts                             ; resume coroutine after yield

; ===========================================================================
; Coroutine primitives — task registration, yielding, and destruction
; ===========================================================================
; These routines manage the cooperative scheduler's task slots.
;
; task_register:   register a new task with an address (state $08)
; task_kill_by_id: free a task slot by ID (A = slot index)
; task_kill_self:  free current task and return to scheduler
; slot_offset_self/slot_offset: convert slot index → byte offset (×4)
; task_yield_x:    yield for X frames (calls task_yield in a loop)
; task_yield:      yield for 1 frame (save state, sleep, return to scheduler)
; ---------------------------------------------------------------------------

; --- task_register — A = slot index, $93/$94 = entry address ---

task_register:  jsr     slot_offset     ; X = A × 4
        lda     task_ptr                ; store address in slot bytes 2-3
        sta     $82,x                   ; store address low in slot
        lda     $94                     ; load address high byte
        sta     $83,x                   ; store address high in slot
        lda     #$08                    ; state = $08 (fresh, has address)
        sta     $80,x                   ; state = fresh (has address)
        rts                             ; return

; --- task_kill_by_id — A = slot index to kill ---
task_kill_by_id:

        jsr     slot_offset             ; X = A × 4
        lda     #$00                    ; state = $00 (free)
        sta     $80,x                   ; state = free
        rts                             ; return

; --- task_kill_self — kill current task ($91) and return to scheduler ---
task_kill_self:

        jsr     slot_offset_self        ; X = $91 × 4
        lda     #$00                    ; state = $00 (free)
        sta     $80,x                   ; state = free
        jmp     task_scheduler          ; back to scheduler (never returns)

; --- slot_offset_self — X = $91 × 4 (current task's byte offset) ---

slot_offset_self:  lda     $91          ; load current task index

; --- slot_offset — X = A × 4 (task slot byte offset) ---
slot_offset:  asl     a                 ; A × 4
        asl     a                       ; A × 2
        tax                             ; A × 4 → X
        rts                             ; return

; --- task_yield_x — yield for X frames ---
; Called extensively from cutscene/level-transition code for timed waits.

task_yield_x:  jsr     task_yield       ; yield one frame
        dex                             ; loop X times
        bne     task_yield_x            ; loop X times
        rts                             ; return

; --- task_yield — yield current task for 1 frame ---
; Saves X/Y on stack, stores sleep countdown and stack pointer in the
; task slot, sets state to $01 (sleeping), then jumps to scheduler.
; NMI will decrement the countdown; when it reaches 0, state → $04.
; Scheduler then restores SP and does RTS to resume here.

task_yield:  lda     #$01               ; $93 = sleep frames (1)
        sta     task_ptr                ; sleep for 1 frame
        txa                             ; save X and Y on stack
        pha                             ; (scheduler's .restore_regs will
        tya                             ; PLA these back on resume)
        pha                             ; save Y on stack
        jsr     slot_offset_self        ; X = $91 × 4
        lda     task_ptr                ; store sleep countdown in byte 1
        sta     $81,x                   ; store sleep countdown
        lda     #$01                    ; state = $01 (sleeping)
        sta     $80,x                   ; state = sleeping
        txa                             ; Y = slot byte offset
        tay                             ; slot offset → Y
        tsx                             ; save current stack pointer
        stx     $82,y                   ; in slot byte 2
        jmp     task_scheduler          ; hand off to scheduler

update_CHR_banks:  lda     #$FF         ; turns on the flag for
        sta     $1B                     ; refreshing CHR banks
        rts                             ; during NMI

; selects all 6 swappable CHR banks
; based on what's in $E8~$ED

select_CHR_banks:  lda     $1B          ; test select CHR flag
        beq     task_yield_return       ; return if not on
task_yield_clear_flag:  ldx     #$00    ; reset select CHR flag
        stx     $1B                     ; immediately, one-off usage
task_yield_select_chr:  stx     MMC3_BANK_SELECT ; MMC3 bank select register
        lda     $E8,x                   ; load CHR bank number from $E8+X
        sta     MMC3_BANK_DATA          ; write to MMC3 bank data register
        inx                             ; next CHR bank slot
        cpx     #$06                    ; all 6 CHR banks done?
        bne     task_yield_select_chr   ; loop until done
task_yield_return:  rts                 ; return

; ===========================================================================
; process_frame_and_yield — run one frame of game logic, then yield to NMI
; ===========================================================================
; Core game loop primitive. Processes all entities, builds OAM sprite data,
; clears $EE (signals "ready for NMI to render"), yields one frame, then
; sets $EE (signals "NMI done, safe to modify state").
;
; process_frame_yield_with_player: sets oam_ptr=$04 (include player sprites)
; process_frame_and_yield: uses caller's oam_ptr value
;
; oam_ptr = OAM write index start (controls which sprite slots to fill):
;   $04 = start after sprite 0 (include player), $0C = skip player sprites
; nmi_skip = rendering phase flag (0=NMI pending, nonzero=NMI completed)
; ---------------------------------------------------------------------------

process_frame_yield_with_player:  lda     #$04 ; $97 = $04: start OAM after sprite 0
        sta     oam_ptr                 ; (include player in sprite update)
process_frame_and_yield:  jsr     prepare_oam_buffer ; process entities (sprite state)
        jsr     update_entity_sprites   ; build OAM buffer from entity data
        lda     #$00                    ; $EE = 0: "waiting for NMI"
        sta     nmi_skip                ; signal waiting for NMI
        jsr     task_yield              ; sleep 1 frame (NMI will render)
        inc     nmi_skip                ; = 1: "NMI done, frame complete"
        rts                             ; return

; selects both swappable PRG banks
; based on $F4 and $F5

select_PRG_banks:  inc     $F6          ; flag on "selecting PRG bank"
        lda     #$06                    ; MMC3 cmd $06: $8000 bank
        sta     mmc3_shadow             ; MMC3 cmd $06: select $8000 bank
        sta     MMC3_BANK_SELECT        ; select the bank in $F4
        lda     mmc3_select             ; as $8000-$9FFF
        sta     $F2                     ; also mirror in $F2
        sta     MMC3_BANK_DATA          ; write to MMC3 bank data register
        lda     #$07                    ; MMC3 cmd $07: $A000 bank
        sta     mmc3_shadow             ; MMC3 cmd $07: select $A000 bank
        sta     MMC3_BANK_SELECT        ; select the bank in $F5
        lda     prg_bank                ; as $A000-$BFFF
        sta     $F3                     ; also mirror in $F3
        sta     MMC3_BANK_DATA          ; write to MMC3 bank data register
        dec     $F6                     ; flag selecting back off (done)
        lda     $F7                     ; if NMI and non-NMI race condition
        bne     play_sounds             ; we still need to play sounds
        rts                             ; else just return

; go through circular sound buffer and pop for play
; handle NMI simultaneous bank changes as well

play_sounds:  lda     $F6               ; this means both NMI and non
        bne     process_frame_race_condition ; yes: flag race and return
        lda     #$06                    ; MMC3 cmd $06: select $8000 bank
        sta     MMC3_BANK_SELECT        ; select $8000 bank register
        lda     #$16                    ; select bank 16 for $8000-$9FFF
        sta     MMC3_BANK_DATA          ; and 17 for $A000-$BFFF
        lda     #$07                    ; MMC3 cmd $07: $A000 bank
        sta     MMC3_BANK_SELECT        ; select $A000 bank register
        lda     #$17                    ; bank $17 for sound data
        sta     MMC3_BANK_DATA          ; write $A000 bank
process_frame_sound_check:  ldx     $DB ; is current sound slot in buffer
        lda     $DC,x                   ; == $88? this means
        cmp     #$88                    ; no sound, skip processing
        beq     process_frame_sound_call ; no sound → call driver tick
        pha                             ; push sound ID
        lda     #$88                    ; clear sound ID immediately
        sta     $DC,x                   ; in circular buffer
        inx                             ; advance buffer index
        txa                             ; increment circular buffer index
        and     #$07                    ; with wraparound $07 -> $00
        sta     $DB                     ; store wrapped buffer index
        pla                             ; play sound ID
        jsr     banked_8003             ; bank $16: play_sound_effect
        jmp     process_frame_sound_check ; check next slot

process_frame_sound_call:  jsr     banked_8000 ; bank $16: sound_driver_tick
        lda     #$00                    ; clear race condition flag
        sta     $F7                     ; clear race condition flag
        jmp     select_PRG_banks        ; apply bank switch and return
process_frame_race_condition:  inc     $F7 ; set race condition flag
        rts                             ; return
        .byte   $05,$10,$11,$04,$41
        .byte   $33,$5C,$D4,$45,$82,$00,$EF,$51
        .byte   $68,$50,$67,$10,$1C,$00,$07,$04
        .byte   $CD,$50,$00,$50,$04,$15,$96,$00
        .byte   $71,$14,$94,$15,$DD,$0E,$97,$C3
        .byte   $43,$04,$00,$00,$08,$57

; interrupt vectors
        .word   NMI                     ; $FFFA: NMI handler
        .word   RESET                   ; $FFFC: RESET handler
        .word   IRQ                     ; $FFFE: IRQ handler
