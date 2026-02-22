; =============================================================================
; MEGA MAN 3 (U) — BANK $16 — SOUND DRIVER CODE
; =============================================================================
; Mapped to $8000-$9FFF. NES APU sound/music engine: channel management,
; envelope processing, frequency tables, and music/SFX playback.
; Always loaded as a pair with bank $17 ($A000-$BFFF = music/SFX data).
; Called every NMI frame via play_sounds ($1FFF90):
;   $8000 → $06, $8001 → $16 (code), $8000 → $07, $8001 → $17 (data)
;
; Key routines:
;   multiply_8x8 — 8x8→16-bit multiply (snd_ptr_lo/snd_ptr_hi = result)
;   jump_local_ptr — inline pointer table dispatch (pulls JSR return addr)
;   read_ptr — cross-bank byte read ($16/$17 for $8000-$BFFF, temp-swaps
;              $18 for $C000+ addresses)
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"


.segment "BANK16"

driver_entry_jump:  .byte   $4C         ; JMP opcode byte ($4C)
driver_entry_bank:  jmp     ($4C80)     ; MMC3 bank swap indirect jump

        .byte   $FE
        .byte   $80
multiply_8x8:  lda     #$00                ; clear result high byte
        sta     snd_ptr_hi              ; result hi = 0
        ldy     #$08                    ; 8-bit loop counter
multiply_shift_loop:  asl     snd_ptr_hi ; shift result left
        rol     snd_ptr_lo              ; rotate carry into high byte
        bcc     multiply_next_bit               ; skip add if no carry
        clc                             ; add multiplicand to result
        lda     snd_ptr_hi              ; result lo += $C4
        adc     snd_param               ; add multiplicand
        sta     snd_ptr_hi              ; store result low
        lda     snd_ptr_lo              ; propagate carry to high byte
        adc     #$00                    ; add carry
        sta     snd_ptr_lo              ; store result high
multiply_next_bit:  dey                         ; decrement bit counter
        bne     multiply_shift_loop               ; loop 8 times
        rts

; jumps to one of the pointers stored
; locally next to the JSR call in ROM
; parameters:
; local pointer table just after JSR call here
; A: index into pointer table

jump_local_ptr:  asl     a              ; A = index * 2
        tay                             ; Y = index * 2 + 1
        iny                             ; word align & just after JSR
        pla
        sta     snd_ptr_lo              ; grab return address
        pla                             ; to get local params
        sta     snd_ptr_hi
        lda     (snd_ptr_lo),y
        pha                             ; read from ROM at
        iny                             ; just after JSR + Y index
        lda     (snd_ptr_lo),y          ; to grab local pointer
        sta     snd_ptr_hi              ; update snd_ptr with this address
        pla                             ; and jump to it
        sta     snd_ptr_lo
        jmp     (snd_ptr_lo)

; reads one single byte from a passed in word-sized ROM address
; from either bank $16, $17, or $18 depending on high byte
; returns read in accumulator
; parameters:
; A: high byte of address (if >= $C0, read bank $18, else $16~$17)
; Y: low byte of address
; returns:
; A: read of passed in word address

read_ptr:  sty     snd_ptr_lo           ; store low byte -> $C1
        ldy     #$00                    ; 0 index for indirect read
        cmp     #$C0                    ; if high byte >= $C0
        bcs     read_ptr_bank18_check   ; this is a bank $18 read
        sta     snd_ptr_hi
        lda     (snd_ptr_lo),y          ; else return read of address
        rts                             ; at snd_ptr, bank $16~$17

read_ptr_bank18_check:  sec
        sbc     #$20                    ; high byte -= $20
        sta     snd_ptr_hi              ; (get into $A0~$BF range)
        lda     #$07
        sta     driver_entry_jump       ; set $A000~$BFFF bank
        lda     #$18                    ; to $18
        sta     driver_entry_bank
        lda     (snd_ptr_lo),y          ; push read snd_ptr
        pha                             ; from bank $18
        lda     #$07
        sta     driver_entry_jump       ; set $A000~$BFFF bank
        lda     #$17                    ; back to $17
        sta     driver_entry_bank
        lda     #$20
        clc                             ; and (falsely) go back into
        adc     snd_ptr_hi              ; $C0~$DF range for high byte
        sta     snd_ptr_hi
        pla                             ; pull & return read
        rts

sound_driver_update:  lda     snd_flags ; load driver flags
        lsr     a                       ; check bit 0 (music active)
        bcs     driver_update_done               ; exit if music not playing
        lda     $D0                     ; load sound data ptr lo
        ora     $D1                     ; check if ptr is null
        beq     update_tempo_accumulator               ; skip data parse if no sound
        jsr     parse_music_data               ; parse next sound data byte
update_tempo_accumulator:  clc                         ; update tempo accumulator
        lda     snd_tempo_hi            ; tempo speed fractional
        adc     snd_tempo_accum         ; add to tempo accumulator
        sta     snd_tempo_accum         ; store tempo accum lo
        lda     snd_tempo_lo            ; tempo speed integer part
        adc     #$00                    ; add carry from fractional
        sta     snd_tempo_ticks         ; store tempo tick count
        lda     snd_channel_mask        ; load channel enable mask
        pha                             ; save channel mask
        ldx     #$03                    ; 4 channels (3 downto 0)
channel_loop_body:  lsr     snd_channel_mask ; shift out channel bit
        bcc     check_sfx_mute               ; skip if channel disabled
        lda     snd_channel_mask        ; preserve remaining bits
        ora     #$80                    ; keep high bit set
        sta     snd_channel_mask        ; restore channel mask
        jsr     process_music_channel               ; process music channel X
check_sfx_mute:  lda     snd_flags      ; check SFX mute flag
        and     #$02                    ; bit 1 = SFX override
        bne     channel_loop_next               ; skip SFX if muted
        txa                             ; save channel index
        pha                             ; push X
        jsr     process_sfx_channel               ; process SFX channel X
        pla                             ; restore channel index
        tax                             ; pull X
channel_loop_next:  dex                         ; next channel
        bpl     channel_loop_body               ; loop all 4 channels
        pla                             ; restore original channel mask
        sta     snd_channel_mask        ; restore $CF
        lsr     snd_flags               ; clear bit 0 of driver flags
        asl     snd_flags               ; (LSR+ASL clears low bit)
        lda     snd_fade_rate           ; load fade rate
        and     #$7F                    ; mask off sign bit
        beq     driver_update_done               ; skip fade if rate = 0
        ldy     #$00                    ; clear high byte for shift
        sty     snd_ptr_lo              ; init shift overflow
        ldy     #$04                    ; shift left 4 times
fade_shift_loop:  asl     a                   ; A = fade_rate << 4
        rol     snd_ptr_lo              ; shift high bits into $C1
        dey                             ; loop counter
        bne     fade_shift_loop               ; shift 4 bits total
        clc                             ; add fade delta to flags
        adc     snd_flags               ; add lo to driver flags
        sta     snd_flags               ; store updated flags
        lda     snd_ptr_lo              ; add overflow to fade target
        adc     snd_fade_level          ; fade target += overflow
        bcc     store_fade_target               ; check for saturation
        lda     snd_fade_rate           ; on overflow, keep sign only
        and     #$80                    ; preserve fade direction bit
        sta     snd_fade_rate           ; clear fade rate on overflow
        lda     #$FF                    ; saturate to $FF
store_fade_target:  sta     snd_fade_level ; store fade target
driver_update_done:  rts

silence_channel:  txa                         ; get channel index
        and     #$03                    ; mask to 0-3
        eor     #$03                    ; reverse: 3=pulse1, 0=noise
        asl     a                       ; multiply by 4
        asl     a                       ; Y = APU register offset
        tay                             ; Y = APU base for channel
        lda     #$30                    ; silence: vol=0, const vol
        cpy     #$08                    ; offset $08 = triangle ch
        bne     write_silence_reg               ; use $30 for pulse/noise
        lda     #$00                    ; triangle: linear ctr = 0
write_silence_reg:  sta     SQ1_VOL,y           ; write silence to channel
        rts

write_apu_register:  pha                         ; save value to write
        txa                             ; get channel index
        and     #$03                    ; mask to 0-3
        eor     #$03                    ; reverse channel order
        asl     a                       ; multiply by 4
        asl     a                       ; base APU offset
        sty     snd_param               ; save register sub-offset
        ora     snd_param               ; combine base + sub-offset
        tay                             ; Y = full APU register index
        pla                             ; restore value
        sta     SQ1_VOL,y               ; write to APU register
        rts

play_sound_odd_frame:  inc     snd_flags ; set odd-frame flag
        jsr     play_sound_ID           ; play sound with flag set
        dec     snd_flags               ; restore driver flags
        rts

; plays sound effect
; parameters:
; A: sound ID to play

play_sound_ID:  cmp     #$F0            ; if sound ID < $F0
        bcc     play_sound_id_bounds_check ; no: normal sound ID
        jmp     dispatch_sound_command               ; handle sound command

play_sound_id_bounds_check:  cmp     sound_id_max ; compare to max sound ID
        bcc     play_sound_id_modulo_loop ; in range: proceed
        sec                             ; A = sound ID mod $39
        sbc     sound_id_max            ; subtract max to wrap around
        bcs     play_sound_id_bounds_check ; loop until < max
play_sound_id_modulo_loop:  asl     a   ; A*2 = pointer table offset
        tax                             ; X = A * 2
        ldy     sound_pointer_table,x   ; index into sound pointers
        tya                             ; grab pointer word in Y & A
        ora     sound_data_low,x        ; if it's $0000, return
        beq     play_sound_id_return    ; otherwise, Y = read byte
        lda     sound_data_low,x        ; at pointer
        jsr     read_ptr                ; read first byte of snd data
        tay                             ; Y = first sound data byte
        beq     init_music_channels               ; if value read was $00
        ldy     #$00                    ; Y = 0 for clearing
        inx                             ; advance to high byte ptr
        sta     snd_param               ; first byte of sound data -> $C4
        and     #$7F                    ; strip sign = priority
        cmp     snd_priority            ; compare to current priority
        bcc     play_sound_id_return    ; reject lower priority sound
        sta     snd_priority            ; set new priority level
        bne     play_sound_setup_priority_check ; nonzero: skip extra checks
        lda     $D6                     ; check SFX restart flags
        bpl     play_sound_setup_priority_check ; no restart flag: proceed
        lda     snd_param               ; check sign of first byte
        bmi     play_sound_setup_priority_check ; bit 7 set: proceed
        sty     $D7                     ; clear chained sound ptr
play_sound_setup_priority_check:  sty     $D6
        asl     snd_param               ; shift bit 7 into $D6
        ror     $D6                     ; rotate into SFX flags
        bpl     play_sound_read_offset_increment ; no chain: skip
        stx     $D7                     ; save ptr index for chain
play_sound_read_offset_increment:  inc     snd_ptr_lo ; advance past header byte
        lda     snd_ptr_lo              ; sound data ptr lo
        sta     $D0                     ; store as current ptr lo
        bne     play_sound_read_address_low ; skip high if no page cross
        inc     snd_ptr_hi              ; handle page crossing
play_sound_read_address_low:  lda     snd_ptr_hi ; sound data ptr hi
        sta     $D1                     ; store as current ptr hi
        tya                             ; A = 0 (from Y)
        sta     $D2                     ; clear transpose
        sta     $D3                     ; clear note duration
        sta     $D4                     ; clear duration multiplier
        sta     $D5                     ; clear total duration
        ldy     #$27                    ; clear $0700-$0727
play_sound_clear_loop:  sta     $0700,y
        dey                             ; clear $0700~$0727
        bpl     play_sound_clear_loop
play_sound_id_return:  rts

init_music_channels:  ldx     #$01                ; music init: first byte = 0
        stx     snd_tempo_lo            ; tempo speed hi = 1
        ldx     #$99                    ; tempo speed lo = $99
        stx     snd_tempo_hi            ; set default tempo
        sta     snd_tempo_accum         ; clear tempo accumulator
        sta     snd_pitch_offset        ; clear pitch transpose
        sta     snd_fade_rate           ; clear fade rate
        sta     snd_fade_level          ; clear fade target
        ldx     #$53                    ; clear $0728-$077B
clear_sfx_state_loop:  sta     $0728,x             ; clear SFX channel state
        dex                             ; loop counter
        bpl     clear_sfx_state_loop               ; clear all SFX state
        ldx     #$03                    ; read 4 channel pointers
read_channel_ptr_loop:  inc     snd_ptr_lo ; advance data pointer
        bne     read_channel_ptr_hi               ; skip page inc if no wrap
        inc     snd_ptr_hi              ; handle page crossing
read_channel_ptr_hi:  ldy     snd_ptr_lo ; Y = ptr lo for read_ptr
        lda     snd_ptr_hi              ; A = ptr hi for read_ptr
        jsr     read_ptr                ; read channel ptr high byte
        sta     $0754,x                 ; store ch X ptr high
        inc     snd_ptr_lo              ; advance data pointer
        bne     read_channel_ptr_lo               ; skip page inc if no wrap
        inc     snd_ptr_hi              ; handle page crossing
read_channel_ptr_lo:  ldy     snd_ptr_lo ; Y = ptr lo for read_ptr
        lda     snd_ptr_hi              ; A = ptr hi for read_ptr
        jsr     read_ptr                ; read channel ptr low byte
        sta     $0750,x                 ; store ch X ptr low
        dex                             ; next channel
        bpl     read_channel_ptr_loop               ; loop all 4 channels
        bmi     init_channel_state               ; always taken: init channels
dispatch_sound_command:  sty     snd_temp ; save Y in temp
        and     #$07                    ; command index = low 3 bits
        jsr     jump_local_ptr          ; dispatch sound command

; parameters to jump_local_ptr
        .byte   $C5,$81,$C8,$81,$E4,$81,$1E,$82
        .byte   $26,$82,$2D,$82,$34,$82,$4A,$82
        jsr     clear_channel_ptrs               ; cmd $F0: init + stop
stop_music:  lda     #$00                ; cmd $F2: stop music
        sta     snd_priority            ; clear priority
        sta     $D0                     ; clear data ptr lo
        sta     $D1                     ; clear data ptr hi
        sta     $D7                     ; clear chained sound
        sta     $D8                     ; clear detune
mute_active_channels:  lda     snd_channel_mask ; mute unused channels
        beq     mute_channels_done               ; no channels: done
        eor     #$0F                    ; invert mask (active->mute)
        sta     snd_channel_mask        ; set channels to silence
        jsr     init_channel_state               ; silence those channels
        lda     #$00                    ; clear channel mask
        sta     snd_channel_mask        ; all channels off
mute_channels_done:  rts

clear_channel_ptrs:  lda     #$00                ; clear all channel ptrs
        ldx     #$03                    ; 4 channels
clear_channel_ptrs_loop:  sta     $0754,x             ; clear ch ptr hi
        sta     $0750,x                 ; clear ch ptr lo
        dex                             ; next channel
        bpl     clear_channel_ptrs_loop               ; loop all 4
init_channel_state:  lda     snd_channel_mask ; load channel enable mask
        pha                             ; save for restore later
        ldx     #$03                    ; 4 channels
init_channel_loop:  lsr     snd_channel_mask ; shift out channel bit
        bcs     init_channel_next               ; skip if channel active
        jsr     silence_channel               ; silence this channel
        lda     $0754,x                 ; check ch ptr hi
        ora     $0750,x                 ; OR with ch ptr lo
        beq     init_channel_next               ; skip if ptr is null
        lda     #$FF                    ; mark channel for update
        sta     $077C,x                 ; set update flag
init_channel_next:  dex                         ; next channel
        bpl     init_channel_loop               ; loop all 4
        pla                             ; restore channel mask
        sta     snd_channel_mask        ; restore $CF
        lda     #$08                    ; disable sweep ($08)
        sta     SQ1_SWEEP               ; pulse 1 sweep off
        sta     SQ2_SWEEP               ; pulse 2 sweep off
        lda     #$0F                    ; enable all sound channels
        sta     SND_CHN                 ; APU status: enable all
        rts

        lda     snd_flags               ; cmd: set SFX mute flag
        ora     #$02                    ; set bit 1 (mute SFX)
        sta     snd_flags               ; store updated flags
        bne     init_channel_state               ; reinit channels
        lda     snd_flags               ; cmd: clear SFX mute flag
        and     #$FD                    ; clear bit 1
        sta     snd_flags               ; store updated flags
        rts

        asl     snd_temp                ; cmd: set fade speed
        beq     set_fade_speed               ; zero = no fade
        sec                             ; nonzero: set sign bit
        ror     snd_temp                ; sign-extend fade speed
set_fade_speed:  lda     snd_flags      ; keep low nibble of flags
        and     #$0F                    ; mask off upper bits
        sta     snd_flags               ; store cleaned flags
        ldy     snd_temp                ; load fade speed param
        sty     snd_fade_rate           ; set fade rate
        beq     store_fade_target_cmd               ; zero: reset target
        ldy     #$FF                    ; init fade target to $FF
        cpy     snd_fade_level          ; already at max?
        bne     set_fade_done               ; no: keep $FF target
        iny                             ; yes: reset target to 0
store_fade_target_cmd:  sty     snd_fade_level ; store fade target
set_fade_done:  rts

        lda     #$00                    ; cmd: set detune
        sec                             ; negate param
        sbc     snd_temp                ; A = 0 - param
        sta     $D8                     ; store as detune value
        rts

parse_music_data:  lda     $D3                 ; check note duration counter
        beq     read_next_command               ; zero: read next data byte
        dec     $D3                     ; decrement duration
        dec     $D5                     ; decrement total duration
        rts

read_next_command:  jsr     read_music_byte           ; read next sound data byte
        sta     snd_param               ; save command/flag byte
        asl     a                       ; check bit 7
        bcc     check_ptr_redirect               ; 0 = not end marker
        sty     snd_priority            ; clear priority (end of SFX)
        lda     $D7                     ; check for chained sound
        lsr     a                       ; bit 0 = chain flag
        bcc     jump_stop_music               ; no chain: stop music
        jsr     play_sound_id_modulo_loop ; play chained sound ID
        jmp     read_next_command               ; restart data parsing

jump_stop_music:  jmp     stop_music           ; no chain: stop all music

check_ptr_redirect:  lsr     snd_param  ; check bit 0 of flags
        bcc     check_duration_flag               ; 0 = no new ptr
        jsr     read_music_byte               ; read next data byte
        asl     a                       ; check for subsong change
        beq     read_new_ptr               ; zero: just read new ptr
        asl     $D6                     ; shift SFX flags
        php                             ; save carry for later
        cmp     $D6                     ; compare subsong IDs
        beq     restore_subsong_flags               ; match: skip to ptr update
        plp                             ; restore flags
        ror     $D6                     ; restore $D6 bit
        inc     $D6                     ; increment subsong counter
read_new_ptr:  jsr     read_music_byte           ; read new ptr high byte
        tax                             ; X = ptr hi
        jsr     read_music_byte               ; read new ptr low byte
        sta     $D0                     ; set data ptr lo
        stx     $D1                     ; set data ptr hi
        bne     read_next_command               ; continue parsing
restore_subsong_flags:  tya                         ; restore Y
        plp                             ; restore carry
        ror     a                       ; rotate back
        sta     $D6                     ; restore SFX flags
        clc                             ; skip 2 bytes (ptr)
        lda     #$02                    ; advance ptr by 2
        adc     $D0                     ; skip past pointer bytes
        sta     $D0                     ; store updated ptr lo
        bcc     check_duration_flag               ; no page cross: continue
        inc     $D1                     ; handle page crossing
check_duration_flag:  lsr     snd_param ; check bit 1 of flags
        bcc     check_transpose_flag               ; 0 = no new duration mult
        jsr     read_music_byte               ; read duration multiplier
        sta     $D4                     ; set duration multiplier
check_transpose_flag:  lsr     snd_param ; check bit 2 of flags
        bcc     read_note_duration               ; 0 = no new transpose
        jsr     read_music_byte               ; read transpose value
        sta     $D2                     ; set transpose
read_note_duration:  jsr     read_music_byte           ; read raw note duration
        sta     $D3                     ; set note duration counter
        sta     snd_ptr_lo              ; save for multiply
        lda     $D4                     ; duration * multiplier
        sta     snd_param               ; set multiplicand
        jsr     multiply_8x8               ; multiply duration * scale
        ldy     snd_ptr_lo              ; recover raw duration
        iny                             ; +1 for total ticks
        sty     $D5                     ; set total duration counter
        inc     snd_flags               ; set music-active flag
        jsr     read_music_byte               ; read channel enable byte
        pha                             ; save channel mask
        eor     snd_channel_mask        ; check for mask change
        beq     restore_channel_mask               ; same mask: skip update
        sta     snd_channel_mask        ; set changed channels
        jsr     mute_active_channels               ; mute changed channels
restore_channel_mask:  pla                         ; restore new channel mask
        sta     snd_channel_mask        ; set active channel mask
        rts

process_music_channel:  ldy     $0700,x             ; load envelope position
        beq     check_new_note               ; skip if no envelope
        jsr     compute_instrument_ptr               ; process envelope tick
check_new_note:  lda     snd_flags      ; load driver flags
        lsr     a                       ; check music-active bit
        bcs     parse_note_data               ; bit 0 set: read new note
        jsr     update_volume_envelope               ; update channel output
        lda     $D3                     ; check note duration
        beq     music_channel_done               ; zero: done with this note
        cpx     #$01                    ; channel 1 (pulse 2)?
        beq     dec_note_timer_ch1               ; special handling for ch 1
        lda     $D5                     ; check total duration left
        beq     check_sustain_flag               ; zero: note finished
music_channel_done:  rts

dec_note_timer_ch1:  dec     $0710,x             ; decrement note timer
        bne     music_channel_done               ; not expired: return
check_sustain_flag:  lda     $0704,x             ; load channel flags
        and     #$04                    ; check sustain bit
        bne     music_channel_done               ; sustain set: don't cut
        jmp     set_release_phase               ; note-off: silence channel

parse_note_data:  lda     #$00                ; new note: parse note data
        sta     snd_param               ; clear bit counter
        jsr     read_music_byte               ; read channel command byte
note_flag_loop:  lsr     a                   ; shift out flag bits
        bcc     note_flags_done_check               ; bit clear: skip param
        pha                             ; save remaining flags
        jsr     read_music_byte               ; read parameter byte
        sta     snd_temp                ; param -> $C3
        lda     snd_param               ; bit index for dispatch
        jsr     dispatch_note_command               ; dispatch channel command
        pla                             ; restore remaining flags
note_flags_done_check:  beq     read_note_value           ; all bits done?
        inc     snd_param               ; advance bit counter
        bne     note_flag_loop               ; process next bit
dispatch_note_command:  jsr     jump_local_ptr

; parameters to jump_local_ptr
        .byte   $6F,$86,$AD,$86,$5A,$86,$A7,$86
        .byte   $A1,$86
read_note_value:  jsr     read_music_byte           ; read note value
        tay                             ; Y = note value
        bne     play_note               ; nonzero: play note
        sta     $0710,x                 ; zero: set rest timer
        lda     $0704,x                 ; load channel flags
        and     #$F8                    ; clear envelope phase bits
        ora     #$04                    ; set phase = sustain (4)
        sta     $0704,x                 ; store updated flags
        jmp     silence_channel               ; silence APU register

play_note:  lda     $0704,x             ; load channel flags
        ora     #$20                    ; set portamento active bit
        sta     $0704,x                 ; store updated flags
        lda     $0718,x                 ; load slide rate
        asl     a                       ; check slide direction (carry)
        lda     #$54                    ; default: long slide duration
        bcs     set_slide_target               ; if sliding up, use long
        lda     #$0A                    ; else short slide duration
set_slide_target:  sta     $071C,x             ; set target note index
        tya                             ; A = note byte
        bpl     play_note_with_envelope               ; if note >= $80, skip init
        cpx     #$01                    ; is this channel 1 (music)?
        bne     jump_reset_vibrato               ; other channels skip volume init
        jsr     init_volume_envelope               ; init volume envelope
jump_reset_vibrato:  jmp     check_vibrato_reset           ; reset vibrato & flags

play_note_with_envelope:  jsr     init_volume_envelope           ; init volume envelope
        lda     #$FF                    ; force freq hi reload
        sta     $077C,x                 ; invalidate cached freq hi
        dey                             ; note index = Y - 1
        txa                             ; A = channel index
        bne     apply_transpose_and_freq               ; if not channel 0, branch
        sta     snd_temp                ; channel 0: clear temp
        tya                             ; A = note index
        eor     #$0F                    ; invert low nibble (duty bits)
        jmp     write_freq_registers               ; write freq directly

apply_transpose_and_freq:  tya                         ; A = note index
        clc                             ; add transpose offset
        adc     $D2                     ; apply global transpose ($D2)
        jmp     note_to_frequency               ; convert to freq & write APU

read_music_byte:  ldy     $D0                 ; Y = data ptr low byte
        lda     $D1                     ; A = data ptr high byte
        inc     $D0                     ; advance pointer low
        bne     jump_read_ptr               ; if no carry, skip hi inc
        inc     $D1                     ; advance pointer high
jump_read_ptr:  jmp     read_ptr            ; read byte from data stream

process_sfx_channel:  txa                         ; A = music channel index
        ora     #$28                    ; map to SFX buffer (+$28)
        tax                             ; X = SFX channel offset
        lda     $0728,x                 ; load SFX data ptr low
        ora     $072C,x                 ; OR with data ptr high
        beq     sfx_channel_done               ; if no SFX data, return
        lda     $0738,x                 ; load note duration counter
        beq     sfx_read_command_loop               ; if zero, read next command
        ldy     $0700,x                 ; load instrument index
        beq     sfx_update_envelope_timer               ; if no instrument, skip
        jsr     compute_instrument_ptr               ; compute instrument ptr
        jsr     update_volume_envelope               ; run volume envelope update
sfx_update_envelope_timer:  lda     $0740,x             ; load envelope timer
        sec                             ; subtract tick rate
        sbc     snd_tempo_ticks         ; decrement by speed
        sta     $0740,x                 ; store updated timer
        beq     sfx_trigger_release               ; if exactly zero
        bcs     sfx_update_duration               ; if timer still positive, skip
sfx_trigger_release:  jsr     set_release_phase           ; set envelope to release phase
sfx_update_duration:  lda     $0738,x             ; load note duration
        sec                             ; subtract tick rate
        sbc     snd_tempo_ticks         ; decrement by speed
        sta     $0738,x                 ; store updated duration
        beq     sfx_read_command_loop               ; if zero, note finished
        bcc     sfx_read_command_loop               ; if underflowed, note finished
sfx_channel_done:  rts

sfx_read_command_loop:  jsr     read_sfx_byte           ; read next byte from stream
        cmp     #$20                    ; is it a note ($20+)?
        bcs     sfx_decode_note               ; if note, process it
        jsr     sfx_dispatch_command               ; else it's a command byte
        jmp     sfx_read_command_loop               ; loop until note found

sfx_decode_note:  pha                         ; save note+duration byte
        rol     a                       ; extract duration bits
        rol     a                       ; rotate bits 5-7
        rol     a                       ; into low position
        rol     a                       ; 4 rotates = shift right 5
        and     #$07                    ; mask to 3 bits (0-7)
        tay                             ; Y = duration scale index
        dey                             ; adjust to 0-based
        lda     $0730,x                 ; load channel control flags
        asl     a                       ; check flag bits
        asl     a                       ; test bit 5 (scale mode)
        bpl     check_alt_duration_flag               ; if clear, try alt table
        lda     frequency_scale_factors_table,y ; power-of-2 scale factor
        bne     accumulate_duration               ; always taken (nonzero)
check_alt_duration_flag:  asl     a                   ; test bit 4 (alt flag)
        asl     a                       ; check next bit
        lda     frequency_scale_alt_table,y ; x1.5 scale factor
        bcc     accumulate_duration               ; if bit clear, use as-is
        sta     snd_temp                ; save scale factor
        lda     $0730,x                 ; load control flags
        and     #$EF                    ; clear bit 4
        sta     $0730,x                 ; store updated flags
        lda     snd_temp                ; restore scale factor
        lsr     a                       ; divide by 2
        clc                             ; add half to original
        adc     snd_temp                ; result = factor * 1.5
accumulate_duration:  clc                         ; add to duration counter
        adc     $0738,x                 ; accumulate duration
        sta     $0738,x                 ; store total duration
        tay                             ; Y = duration for multiply
        pla                             ; restore note+duration byte
        and     #$1F                    ; mask note index (0-31)
        bne     sfx_note_setup               ; if note > 0, process it
        jsr     set_release_phase               ; note 0 = rest, set release
        jmp     set_infinite_timer               ; set infinite envelope timer

sfx_note_setup:  pha                         ; save note index
        sty     snd_param               ; duration -> multiply input
        lda     $073C,x                 ; load duration multiplier
        sta     snd_ptr_lo              ; multiplier -> $C1
        jsr     multiply_8x8               ; duration * multiplier
        lda     snd_ptr_lo              ; get multiply result high
        bne     store_envelope_timer               ; if nonzero, use it
        lda     #$01                    ; minimum duration = 1
store_envelope_timer:  sta     $0740,x             ; set envelope timer
        pla                             ; restore note index
        tay                             ; Y = note index
        dey                             ; adjust to 0-based note
        lda     $0730,x                 ; load channel control flags
        bpl     sfx_init_envelope               ; if bit 7 clear, normal note
        lda     $0718,x                 ; load slide rate
        bne     sfx_get_channel_index               ; if sliding, skip retrigger
        jsr     check_vibrato_reset               ; reset vibrato phase
        jmp     sfx_update_control_flags               ; update control flags

sfx_init_envelope:  jsr     init_volume_envelope           ; init volume envelope
        lda     snd_channel_mask        ; load channel enable mask
        bmi     sfx_get_channel_index               ; if SFX active, skip
        sty     snd_temp                ; save note index
        txa                             ; A = SFX channel offset
        and     #$03                    ; mask to music channel 0-3
        tay                             ; Y = music channel index
        lda     #$FF                    ; force freq hi reload
        sta     $077C,y                 ; invalidate cached freq hi
        ldy     snd_temp                ; restore note index
sfx_get_channel_index:  txa                         ; A = SFX channel offset
        and     #$03                    ; mask to channel 0-3
        bne     sfx_lookup_frequency               ; if not channel 0 (noise)
        sta     snd_temp                ; clear temp
        tya                             ; A = note index
        and     #$0F                    ; mask to 4-bit noise period
        eor     #$0F                    ; invert (higher = lower)
        jsr     write_freq_registers               ; write noise freq & period
        jmp     sfx_update_control_flags               ; go to control flag update

sfx_lookup_frequency:  sty     snd_temp ; save note index
        lda     $0730,x                 ; load control flags
        and     #$0F                    ; get octave bits (low nibble)
        tay                             ; Y = octave index
        lda     pitch_offset_table,y    ; base pitch for this octave
        clc                             ; add note offset
        adc     snd_temp                ; add semitone within octave
        clc                             ; add global pitch offset
        adc     snd_pitch_offset        ; add master pitch ($CB)
        clc                             ; add channel pitch bend
        adc     $0734,x                 ; add per-channel detune
        jsr     note_to_frequency               ; look up freq & write APU
sfx_update_control_flags:  lda     $0730,x             ; load control flags
        tay                             ; save copy
        and     #$40                    ; isolate bit 6
        asl     a                       ; shift to bit 7
        sta     snd_param               ; store shifted bit
        tya                             ; restore original flags
        and     #$7F                    ; clear bit 7
        ora     snd_param               ; copy bit 6 into bit 7
        sta     $0730,x                 ; store toggled flags
        bpl     sfx_note_done               ; if bit 7 clear, done
set_infinite_timer:  lda     #$FF                ; set infinite envelope timer
        sta     $0740,x                 ; prevent early release
sfx_note_done:  rts

sfx_dispatch_command:  cmp     #$04                ; is command >= 4?
        bcc     sfx_command_jump               ; if < 4, use directly
        sta     snd_param               ; save command index
        jsr     read_sfx_byte               ; read argument byte
        sta     snd_temp                ; store argument in $C3
        lda     snd_param               ; restore command index
sfx_command_jump:  jsr     jump_local_ptr      ; dispatch via pointer table

; parameters to jump_local_ptr
        .byte   $D9,$84,$DD,$84,$E1,$84,$E8,$84
        .byte   $75,$85,$F1,$84,$FF,$84,$5A,$86
        .byte   $6F,$86,$05,$85,$10,$85,$15,$85
        .byte   $A1,$86,$A7,$86,$1B,$85,$1F,$85
        .byte   $23,$85,$27,$85,$1B,$85,$1F,$85
        .byte   $23,$85,$27,$85,$5A,$85,$80,$85
        .byte   $AD,$86
        lda     #$20                    ; cmd 0: toggle bit 5 (legato)
        bne     toggle_control_flag               ; always branch
        lda     #$40                    ; cmd 1: toggle bit 6
        bne     toggle_control_flag               ; always branch
        lda     #$10                    ; cmd 2: set bit 4 (dotted)
        ora     $0730,x                 ; OR into control flags
        bne     store_control_flags               ; always branch
        lda     #$08                    ; cmd 3: toggle bit 3
toggle_control_flag:  eor     $0730,x             ; XOR toggle flag bits
store_control_flags:  sta     $0730,x             ; store updated control flags
        rts

        lda     #$00                    ; cmd 5: reset tempo fraction
        sta     snd_tempo_accum         ; clear tempo accumulator
        jsr     read_sfx_byte               ; read tempo high byte
        ldy     snd_temp                ; Y = previous arg (tempo lo)
        sta     snd_tempo_hi            ; set tempo increment high
        sty     snd_tempo_lo            ; set tempo increment low
        rts

        lda     snd_temp                ; cmd 6: set duration multiplier
        sta     $073C,x                 ; store in channel buffer
        rts

        lda     $0730,x                 ; cmd 7: load control flags
        and     #$F8                    ; clear octave bits
        ora     snd_temp                ; set new octave from arg
        sta     $0730,x                 ; store updated flags
        rts

        lda     snd_temp                ; cmd 8: set global pitch
        sta     snd_pitch_offset        ; store master pitch offset
        rts

        lda     snd_temp                ; cmd 9: set channel detune
        sta     $0734,x                 ; store per-channel pitch bend
        rts

        lda     #$00                    ; cmd 10: loop group 0
        beq     set_loop_group_offset               ; always branch
        lda     #$04                    ; cmd 11: loop group 1
        bne     set_loop_group_offset               ; always branch
        lda     #$08                    ; cmd 12: loop group 2
        bne     set_loop_group_offset               ; always branch
        lda     #$0C                    ; cmd 13: loop group 3
set_loop_group_offset:  sta     snd_ptr_hi ; set loop group offset
        txa                             ; A = channel offset
        clc                             ; add loop group base
        adc     snd_ptr_hi              ; index into loop counters
        tay                             ; Y = loop counter index
        lda     snd_param               ; load command byte
        cmp     #$12                    ; check if loop-end variant
        bcs     loop_end_check               ; if >= $12, loop-end path
        lda     $0744,y                 ; load loop counter
        sec                             ; decrement counter
        sbc     #$01                    ; subtract 1
        bcs     store_loop_counter               ; if counter exhausted
        lda     snd_temp                ; reload from argument
store_loop_counter:  sta     $0744,y             ; store loop counter
        beq     skip_loop_address               ; if zero, skip loop body
        bne     loop_read_new_address               ; else read new address
loop_end_check:  lda     $0744,y             ; load loop counter
        sec                             ; decrement
        sbc     #$01                    ; subtract 1
        bne     skip_loop_address               ; if not zero, skip ahead
        sta     $0744,y                 ; store zero (loop done)
        jsr     merge_control_flags               ; update control flags
loop_read_new_address:  jsr     read_sfx_byte           ; read new data ptr high
        sta     snd_temp                ; save as $C3
        jsr     read_sfx_byte               ; read new data ptr low
        sta     $0728,x                 ; set data pointer low
        lda     snd_temp                ; get high byte
        sta     $072C,x                 ; set data pointer high
        rts

skip_loop_address:  lda     #$02                ; skip 2 bytes (loop address)
        clc                             ; advance data pointer
        adc     $0728,x                 ; ptr low += 2
        sta     $0728,x                 ; store advanced pointer
        bcc     skip_loop_done               ; if no carry, done
        inc     $072C,x                 ; carry into high byte
skip_loop_done:  rts

merge_control_flags:  lda     $0730,x             ; load control flags
        and     #$97                    ; keep bits 7,4,2,1,0
        ora     snd_temp                ; merge new flag bits
        sta     $0730,x                 ; store updated flags
        rts

        pla                             ; cmd 22: discard return addr
        pla                             ; pop JSR return (end track)
        lda     #$00                    ; clear data pointer
        sta     $0728,x                 ; data ptr low = 0
        sta     $072C,x                 ; data ptr high = 0
        lda     snd_channel_mask        ; load channel enable mask
        bmi     end_track_done               ; if SFX mode, just return
        jmp     silence_channel               ; silence this channel

end_track_done:  rts

read_sfx_byte:  ldy     $0728,x             ; Y = data ptr low
        lda     $072C,x                 ; A = data ptr high
        inc     $0728,x                 ; advance pointer low
        bne     jump_read_sfx_ptr               ; if no overflow, skip
        inc     $072C,x                 ; advance pointer high
jump_read_sfx_ptr:  jmp     read_ptr            ; read byte at Y/A address

set_release_phase:  lda     $0704,x             ; load channel flags
        and     #$F8                    ; clear envelope phase bits
        ora     #$03                    ; set phase = release (3)
        sta     $0704,x                 ; store updated flags
        rts

init_volume_envelope:  tya                         ; save note index
        pha                             ; push Y to stack
        ldy     #$00                    ; initial volume = 0
        lda     $0704,x                 ; load channel flags
        and     #$F8                    ; clear envelope phase bits
        sta     $0704,x                 ; phase = 0 (attack start)
        cpx     #$29                    ; is this noise SFX channel?
        beq     init_envelope_noise               ; noise: skip duty calc
        cpx     #$01                    ; is this music channel 1?
        bne     store_initial_volume               ; other channels skip duty
        lda     $D3                     ; load note length param
        sta     snd_ptr_lo              ; as multiply operand
        lda     $070C,x                 ; load duty/volume register
        sta     snd_param               ; as multiply operand
        jsr     multiply_8x8               ; duty * note length
        ldy     snd_ptr_lo              ; Y = multiply result
init_envelope_noise:  iny                         ; start volume at 1
        inc     $0704,x                 ; advance to phase 1
        inc     $0704,x                 ; advance to phase 2 (attack)
store_initial_volume:  tya                         ; store initial volume
        sta     $0710,x                 ; set current volume level
        pla                             ; restore note index
        tay                             ; Y = note index
        rts

note_to_frequency:  cmp     #$60                ; note index >= 96?
        bcc     store_clamped_note               ; if in range, keep it
        lda     #$5F                    ; clamp to max note 95
store_clamped_note:  sta     snd_temp   ; store clamped note index
        inc     snd_temp                ; note index + 1 (1-based)
        cpx     #$28                    ; is this a SFX channel?
        bcc     lookup_freq_table               ; music channels skip slide
        lda     $071C,x                 ; load current note
        beq     clear_portamento_flag               ; if no current note, direct
        cmp     snd_temp                ; same note as target?
        bne     check_slide_rate               ; if different, check slide
        lda     $0730,x                 ; load control flags
        bpl     clear_portamento_flag               ; if legato off, set direct
        bmi     check_vibrato_reset               ; legato on: reset vibrato
check_slide_rate:  lda     $0718,x             ; load slide rate
        beq     clear_portamento_flag               ; if no slide, set direct
        bcs     slide_up_clear_sign               ; target > current?
        ora     #$80                    ; slide down: set bit 7
        bne     store_slide_direction               ; always taken
slide_up_clear_sign:  and     #$7F                ; slide up: clear bit 7
store_slide_direction:  sta     $0718,x             ; store slide direction+rate
        lda     $0704,x                 ; load channel flags
        ora     #$20                    ; set portamento active bit
        sta     $0704,x                 ; store updated flags
        lda     snd_temp                ; A = new target note
        ldy     $071C,x                 ; save old note in $C3
        sty     snd_temp                ; swap old note to $C3
        bne     store_current_note               ; keep old note as current
clear_portamento_flag:  lda     $0704,x             ; load channel flags
        and     #$DF                    ; clear portamento bit
        sta     $0704,x                 ; store updated flags
        lda     snd_temp                ; use new note directly
store_current_note:  sta     $071C,x             ; update current note index
lookup_freq_table:  asl     snd_temp    ; note * 2 for table index
        ldy     snd_temp                ; Y = table offset
        lda     frequency_high_start,y  ; load freq timer high
        sta     snd_temp                ; store in temp
        lda     frequency_period_table,y ; load freq timer low
write_freq_registers:  sta     $0724,x             ; store freq hi register
        lda     snd_temp                ; get freq high from temp
        sta     $0720,x                 ; store freq lo register
        ldy     #$04                    ; instrument data offset 4
        lda     (snd_inst_lo),y         ; load vibrato speed param
        bmi     reset_vibrato_phase               ; if bit 7 set, reset vibrato
check_vibrato_reset:  lda     $0704,x             ; load channel flags
        and     #$08                    ; check new-note flag
        bne     reset_vibrato_phase               ; if set, reset vibrato
        rts

reset_vibrato_phase:  lda     #$00                ; clear vibrato phase
        sta     $0708,x                 ; reset phase accumulator
        lda     $0704,x                 ; load channel flags
        and     #$37                    ; clear vibrato & slide bits
        sta     $0704,x                 ; store cleaned flags
        rts

        cpx     #$01                    ; is this music channel 1?
        bne     set_duty_vol_default               ; if not, use default path
        lda     snd_temp                ; load argument
        bne     store_duty_vol               ; if nonzero, set directly
set_duty_vol_default:  lda     $070C,x             ; load current duty/vol
        and     #$C0                    ; keep duty cycle bits
        ora     snd_temp                ; merge new volume bits
        ora     #$30                    ; set length counter halt
store_duty_vol:  sta     $070C,x             ; store duty/volume register
        rts

        inc     snd_temp                ; cmd: increment instrument
        lda     snd_temp                ; load new instrument index
        cmp     $0700,x                 ; same as current?
        beq     instrument_ptr_done               ; if same, skip reload
        sta     $0700,x                 ; set new instrument index
        tay                             ; Y = instrument index
        lda     $0704,x                 ; load channel flags
        ora     #$08                    ; set new-note flag
        sta     $0704,x                 ; store updated flags
compute_instrument_ptr:  dey                         ; Y = instrument - 1
        lda     #$00                    ; clear high byte
        sta     snd_temp                ; init pointer high = 0
        tya                             ; A = instrument - 1
        asl     a                       ; index * 2
        rol     snd_temp                ; carry into high byte
        asl     a                       ; index * 4
        rol     snd_temp                ; carry into high byte
        asl     a                       ; index * 8
        rol     snd_temp                ; carry into high byte
        clc                             ; add base address low
        adc     sound_data_base         ; + sound data base
        sta     snd_inst_lo             ; store ptr low in $C5
        lda     snd_temp                ; get high byte
        adc     sound_data_high         ; + sound data base high
        sta     snd_inst_hi             ; store ptr high in $C6
instrument_ptr_done:  rts

        lda     snd_temp                ; cmd: set pitch detune
        sta     $0714,x                 ; store detune offset
        rts

        lda     snd_temp                ; cmd: set slide rate
        sta     $0718,x                 ; store portamento rate
        rts

        lda     $070C,x                 ; cmd: load duty/vol register
        and     #$0F                    ; keep volume bits only
        ora     snd_temp                ; merge new duty bits
        ora     #$30                    ; set length counter halt
        sta     $070C,x                 ; store duty/volume register
        rts

update_volume_envelope:  lda     $0710,x             ; load current volume
        sta     snd_param               ; save as multiply input
        lda     $0704,x                 ; load channel flags
        and     #$07                    ; get envelope phase (0-7)
        jsr     jump_local_ptr          ; dispatch by phase

; parameters to jump_local_ptr
        .byte   $D1,$86,$E6,$86,$20,$87,$02,$87
        .byte   $14,$89
        ldy     #$00                    ; phase 0: attack ramp-up
        lda     (snd_inst_lo),y         ; load attack rate
        tay                             ; Y = attack increment idx
        lda     snd_param               ; load current volume
        clc                             ; add attack increment
        adc     duty_cycle_table,y      ; from duty cycle table
        bcs     clamp_attack_max               ; if overflow, clamp
        cmp     #$F0                    ; reached max ($F0)?
        bcc     store_envelope_volume               ; if not, keep volume
clamp_attack_max:  lda     #$F0                ; clamp to max volume $F0
        bne     advance_envelope_phase               ; advance to next phase
        ldy     #$01                    ; phase 1: decay ramp-down
        lda     (snd_inst_lo),y         ; load decay rate
        beq     load_sustain_level               ; if zero, use sustain level
        tay                             ; Y = decay decrement idx
        lda     snd_param               ; load current volume
        sec                             ; subtract decay amount
        sbc     duty_cycle_table,y      ; from duty cycle table
        bcc     load_sustain_level               ; if underflow, use sustain
        ldy     #$02                    ; offset 2 = sustain level
        cmp     (snd_inst_lo),y         ; above sustain level?
        bcs     store_envelope_volume               ; if yes, keep decaying
load_sustain_level:  ldy     #$02                ; reached sustain level
        lda     (snd_inst_lo),y         ; load sustain level
        jmp     advance_envelope_phase               ; advance to sustain phase

        txa                             ; phase 2: release ramp-down
        and     #$03                    ; mask to channel 0-3
        cmp     #$01                    ; is this channel 1?
        beq     set_volume_zero               ; channel 1: fade to zero
        ldy     #$03                    ; offset 3 = release rate
        lda     (snd_inst_lo),y         ; load release rate
        beq     route_volume_output               ; if zero, hold volume
        tay                             ; Y = release decrement idx
        lda     snd_param               ; load current volume
        sec                             ; subtract release amount
        sbc     duty_cycle_table,y      ; from duty cycle table
        bcs     store_envelope_volume               ; if underflow, set to zero
set_volume_zero:  lda     #$00                ; volume = 0 (silent)
advance_envelope_phase:  inc     $0704,x             ; advance envelope phase
store_envelope_volume:  sta     $0710,x             ; store updated volume
route_volume_output:  cpx     #$28                ; is this a SFX channel?
        bcc     get_music_channel_vol               ; music channels: write APU
        lda     snd_channel_mask        ; load channel enable mask
        bpl     apply_fade_volume               ; if SFX not active, mix
        jmp     update_portamento               ; SFX active: write directly

apply_fade_volume:  lda     snd_fade_level ; load master volume high
        ldy     snd_fade_rate           ; load master volume control
        bmi     check_fade_max               ; pitch bend sign is negative?
        eor     #$FF                    ; negate to get absolute value
check_fade_max:  cmp     #$FF                ; check if pitch bend is max
        bne     check_noise_volume               ; nonzero: apply volume scaling
get_music_channel_vol:  txa                         ; get channel index (0-3)
        and     #$03
        cmp     #$01                    ; channel 1 = pulse 2?
        bne     use_envelope_volume               ; not pulse 2: use envelope vol
        beq     load_envelope_volume               ; pulse 2: use envelope vol only
check_noise_volume:  cpx     #$29                ; is this the noise channel?
        bne     compare_fade_envelope               ; not noise: compare volumes
        sta     snd_param               ; pitch bend as multiplier
        lda     $0740,x                 ; load note duration timer
        sta     snd_ptr_lo              ; store for multiply
        jsr     multiply_8x8               ; multiply bend * duration
        lda     snd_ptr_lo              ; get multiply result high
        beq     write_volume_to_apu               ; zero: use result as volume
load_envelope_volume:  lda     $0710,x             ; load current envelope volume
        beq     write_volume_to_apu               ; zero: use as volume output
        lda     #$FF                    ; set full volume ($FF)
        bne     write_volume_to_apu               ; always branch to write vol
compare_fade_envelope:  cmp     $0710,x             ; compare bend with envelope
        bcc     compute_volume_atten               ; use smaller of the two
use_envelope_volume:  lda     $0710,x             ; load current envelope volume
compute_volume_atten:  lsr     a                   ; shift high nibble to low
        lsr     a
        lsr     a
        lsr     a
        eor     #$0F                    ; invert for volume (0=max)
        sta     snd_temp                ; store as volume attenuation
        ldy     #$06                    ; instrument offset 6
        lda     (snd_inst_lo),y         ; load volume vibrato depth
        cmp     #$05                    ; depth < 5?
        bcc     apply_volume_atten               ; skip vol vibrato if small
        sta     snd_param               ; depth as multiplier
        ldy     $0708,x                 ; load vibrato phase
        lda     $0704,x                 ; load channel flags
        asl     a                       ; shift bit 6 into carry
        asl     a
        tya                             ; use phase as operand
        bcc     skip_vol_vibrato_neg               ; check vibrato direction
        eor     #$FF                    ; negate phase if descending
skip_vol_vibrato_neg:  beq     apply_volume_atten           ; zero phase: skip vibrato
        sta     snd_ptr_lo              ; store for multiply
        jsr     multiply_8x8               ; multiply depth * phase
        lda     snd_ptr_lo              ; get result high byte
        lsr     a                       ; divide result by 4
        lsr     a
        cmp     #$10                    ; clamp to 15 max
        bcs     clamp_volume_zero               ; overflow: use raw duty/vol
        cmp     snd_temp                ; compare with attenuation
        bcc     apply_volume_atten               ; use larger attenuation
        sta     snd_temp                ; update volume attenuation
apply_volume_atten:  lda     #$10                ; bit 4 flag for volume test
        sta     snd_param
        lda     $070C,x                 ; load base duty/volume
        sec
        sbc     snd_temp                ; subtract attenuation
        bit     snd_param               ; test bit 4 of result
        bne     write_volume_to_apu               ; underflow: clamp volume
clamp_volume_zero:  lda     $070C,x             ; load raw duty/volume
        and     #$F0                    ; keep duty, zero volume
write_volume_to_apu:  ldy     #$00                ; APU reg offset 0 (volume)
        jsr     write_apu_register               ; write volume to APU
        txa                             ; get channel index
        and     #$03
        tay                             ; Y = channel 0-3
        lda     $077C,y                 ; load cached freq high
        bmi     use_base_frequency               ; $FF = no prev write, skip
        ldy     #$05                    ; instrument offset 5
        lda     (snd_inst_lo),y         ; load freq vibrato depth
        beq     use_base_frequency               ; zero: no vibrato
        sta     snd_param               ; depth as multiplier
        ldy     $0708,x                 ; load vibrato phase
        lda     $0704,x                 ; load channel flags
        asl     a                       ; shift bit 6 into carry
        asl     a
        tya                             ; use phase as operand
        bcc     skip_freq_vibrato_zero               ; check vibrato direction
        eor     #$FF                    ; negate phase if descending
skip_freq_vibrato_zero:  beq     use_base_frequency           ; zero phase: skip vibrato
        sta     snd_ptr_lo              ; store for multiply
        jsr     multiply_8x8               ; multiply depth * phase
        lda     snd_ptr_lo              ; result high = Y:$C2
        lsr     a                       ; shift 16-bit result >> 4
        ror     snd_ptr_hi
        lsr     a
        ror     snd_ptr_hi
        lsr     a
        ror     snd_ptr_hi
        lsr     a
        ror     snd_ptr_hi
        tay                             ; Y = freq offset high
        ora     snd_ptr_hi              ; check if offset is zero
        beq     use_base_frequency               ; zero offset: use base freq
        lda     $0704,x                 ; check vibrato direction bit
        bmi     vibrato_subtract_freq               ; bit 7 set: subtract freq
        clc
        lda     snd_ptr_hi              ; add vibrato offset lo
        adc     $0720,x                 ; to base frequency lo
        sta     snd_ptr_hi              ; store adjusted freq lo
        tya                             ; add vibrato offset hi
        adc     $0724,x                 ; to base frequency hi
        bne     store_adjusted_freq_hi               ; nonzero: use adjusted freq
vibrato_subtract_freq:  sec
        lda     $0720,x                 ; base freq lo
        sbc     snd_ptr_hi              ; subtract vibrato offset lo
        sta     snd_ptr_hi              ; store adjusted freq lo
        lda     $0724,x                 ; base freq hi
        sty     snd_ptr_lo              ; store offset hi for sub
        sbc     snd_ptr_lo              ; subtract vibrato offset hi
store_adjusted_freq_hi:  tay                         ; Y = adjusted freq hi
        bne     check_sweep_active               ; nonzero: use adjusted freq
use_base_frequency:  lda     $0720,x             ; no vibrato: use base freq lo
        sta     snd_ptr_hi              ; store to $C2
        ldy     $0724,x                 ; Y = base freq hi
check_sweep_active:  cpx     #$28                ; is this a music channel?
        bcs     write_freq_to_apu               ; SFX channel: skip sweep
        lda     $D6                     ; load SFX active flag
        bpl     write_freq_to_apu               ; no SFX: skip sweep
        lda     $D8                     ; load pitch sweep amount
        beq     write_freq_to_apu               ; zero: no sweep active
        sta     snd_param               ; sweep as multiplier
        sty     snd_ptr_lo              ; save freq hi
        lda     snd_ptr_hi              ; load freq lo
        pha                             ; save freq lo on stack
        jsr     multiply_8x8               ; multiply sweep * freq lo
        pla                             ; restore freq lo
        clc
        adc     snd_ptr_hi              ; add sweep offset to freq lo
        sta     snd_ptr_hi              ; store swept freq lo
        lda     #$00                    ; propagate carry
        adc     snd_ptr_lo              ; add to freq hi
        tay                             ; Y = swept freq hi
write_freq_to_apu:  txa                         ; get channel index
        and     #$03
        bne     calc_pulse_freq               ; nonzero: not triangle
        tya                             ; A = freq hi for triangle
        and     #$0F                    ; mask to low nibble
        ldy     #$07                    ; instrument offset 7
        ora     (snd_inst_lo),y         ; OR with linear counter val
        sta     snd_ptr_hi              ; store as $4008 value
        lda     #$00                    ; clear freq hi
        sta     snd_ptr_lo              ; freq hi = 0 for triangle
        beq     write_freq_lo_reg               ; jump to write freq regs
calc_pulse_freq:  tya                         ; A = freq hi
        ldy     #$08                    ; Y = 8 for threshold scan
octave_scan_loop:  dey                         ; scan downward
        cmp     volume_thresholds_table,y ; find matching octave range
        bcc     octave_scan_loop               ; loop until A >= threshold
        sta     snd_ptr_lo              ; save raw freq hi
        tya                             ; Y = octave index
        clc
        adc     snd_ptr_lo              ; combine octave + raw freq
        tay                             ; Y = lookup index
        and     #$07                    ; low 3 bits
        clc
        adc     #$07                    ; add 7 for mantissa base
        sta     snd_ptr_lo              ; store as freq hi mantissa
        tya                             ; get combined index
        and     #$38                    ; extract octave (bits 3-5)
        eor     #$38                    ; invert for shift count
        beq     check_pitch_detune               ; zero shifts needed?
mantissa_shift_loop:  lsr     snd_ptr_lo ; shift mantissa right
        ror     snd_ptr_hi              ; into freq lo
        sec
        sbc     #$08                    ; decrement shift counter
        bne     mantissa_shift_loop               ; loop until done
check_pitch_detune:  ldy     #$00                ; Y = 0 for detune check
        lda     $0714,x                 ; load pitch detune
        beq     write_freq_lo_reg               ; zero: no detune
        bpl     apply_pitch_detune               ; positive detune?
        dey                             ; Y = $FF for sign extend
apply_pitch_detune:  clc                         ; add detune to freq lo
        adc     snd_ptr_hi              ; add detune to freq lo
        sta     snd_ptr_hi              ; store adjusted freq lo
        tya                             ; sign extend to high byte
        adc     snd_ptr_lo              ; add carry to freq hi
        sta     snd_ptr_lo              ; store adjusted freq hi
write_freq_lo_reg:  ldy     #$02                ; APU reg offset 2 (freq lo)
        lda     snd_ptr_hi              ; load freq lo
        jsr     write_apu_register               ; write freq lo to APU
        txa                             ; get channel index
        and     #$03
        tay                             ; Y = channel 0-3
        lda     snd_ptr_lo              ; load freq hi
        cmp     $077C,y                 ; same as cached value?
        beq     update_portamento               ; unchanged: skip write
        sta     $077C,y                 ; update cached freq hi
        ora     #$08                    ; set length counter load bit
        ldy     #$03                    ; APU reg offset 3 (freq hi)
        jsr     write_apu_register               ; write freq hi to APU
update_portamento:  lda     $0704,x             ; load channel flags
        and     #$20                    ; test portamento bit
        beq     update_vibrato_phase               ; no portamento: done
        lda     $0718,x                 ; load portamento rate
        beq     clear_portamento_done               ; zero rate: clear flag
        ldy     #$00                    ; Y = 0 (direction up)
        asl     a                       ; double rate, sign to carry
        php                             ; save sign flag
        bcc     apply_slide_rate               ; positive: slide up
        eor     #$FF                    ; negate for absolute value
        clc
        adc     #$01
        dey                             ; Y = $FF (direction down)
apply_slide_rate:  clc
        adc     $0720,x                 ; add rate to freq lo
        sta     $0720,x                 ; store updated freq lo
        tya                             ; propagate carry/borrow
        adc     $0724,x                 ; add to freq hi
        sta     $0724,x                 ; store updated freq hi
        lda     $071C,x                 ; load target note index
        asl     a                       ; note * 2 for table index
        tay                             ; Y = table offset
        sec
        lda     $0720,x                 ; compare freq lo with target
        sbc     frequency_high_start,y  ; subtract target freq lo
        lda     $0724,x                 ; compare freq hi with target
        and     #$3F
        sbc     frequency_period_table,y ; subtract target freq hi
        lda     #$FF                    ; set borrow test value
        adc     #$00                    ; A = 0 if borrow, 1 if not
        plp                             ; restore slide direction
        adc     #$00                    ; add direction flag
        bne     update_vibrato_phase               ; nonzero: not at target yet
        txa                             ; check if channel 0
        beq     update_vibrato_phase               ; channel 0: skip clamp
        lda     frequency_high_start,y  ; clamp to target freq lo
        sta     $0720,x                 ; store clamped freq lo
        lda     frequency_period_table,y ; clamp to target freq hi
        sta     $0724,x                 ; store clamped freq hi
clear_portamento_done:  lda     $0704,x             ; load channel flags
        and     #$DF                    ; clear portamento bit
        sta     $0704,x                 ; store updated flags
update_vibrato_phase:  ldy     #$04                ; instrument offset 4
        lda     (snd_inst_lo),y         ; load vibrato speed
        and     #$7F                    ; mask off sign bit
        beq     vibrato_update_done               ; zero: no vibrato update
        clc
        adc     $0708,x                 ; advance vibrato phase
        sta     $0708,x                 ; store updated phase
        bcc     vibrato_update_done               ; no overflow: done
        lda     $0704,x                 ; load channel flags
        clc
        adc     #$40                    ; toggle vibrato direction
        sta     $0704,x                 ; store updated flags
vibrato_update_done:  rts

frequency_scale_factors_table:  .byte   $02,$04,$08,$10,$20,$40,$80
frequency_scale_alt_table:  .byte   $03,$06,$0C,$18,$30,$60,$C0
pitch_offset_table:  .byte   $00,$0C,$18,$24,$30,$3C,$48,$54
        .byte   $18,$24,$30,$3C,$48,$54,$60,$6C
duty_cycle_table:  .byte   $00,$01,$02,$03,$04,$05,$06,$07
        .byte   $08,$09,$0A,$0B,$0C,$0E,$0F,$10
        .byte   $12,$13,$14,$16,$18,$1B,$1E,$23
        .byte   $28,$30,$3C,$50,$7E,$7F,$FE,$FF
volume_thresholds_table:  .byte   $00,$07,$0E,$15,$1C,$23
frequency_high_start:  .byte   $2A
frequency_period_table:  .byte   $31,$5C,$37,$9C,$36,$E7,$35,$3C
        .byte   $35,$9B,$34,$02,$34,$72,$33,$EA
        .byte   $32,$6A,$32,$F1,$31,$80,$31,$14
        .byte   $31,$5C,$30,$9C,$2F,$E7,$2E,$3C
        .byte   $2E,$9B,$2D,$02,$2D,$72,$2C,$EA
        .byte   $2B,$6A,$2B,$F1,$2A,$80,$2A,$14
        .byte   $2A,$5C,$29,$9C,$28,$E7,$27,$3C
        .byte   $27,$9B,$26,$02,$26,$72,$25,$EA
        .byte   $24,$6A,$24,$F1,$23,$80,$23,$14
        .byte   $23,$5C,$22,$9C,$21,$E7,$20,$3C
        .byte   $20,$9B,$1F,$02,$1F,$72,$1E,$EA
        .byte   $1D,$6A,$1D,$F1,$1C,$80,$1C,$14
        .byte   $1C,$5C,$1B,$9C,$1A,$E7,$19,$3C
        .byte   $19,$9B,$18,$02,$18,$72,$17,$EA
        .byte   $16,$6A,$16,$F1,$15,$80,$15,$14
        .byte   $15,$5C,$14,$9C,$13,$E7,$12,$3C
        .byte   $12,$9B,$11,$02,$11,$72,$10,$EA
        .byte   $0F,$6A,$0F,$F1,$0E,$80,$0E,$14
        .byte   $0E,$5C,$0D,$9C,$0C,$E7,$0B,$3C
        .byte   $0B,$9B,$0A,$02,$0A,$72,$09,$EA
        .byte   $08,$6A,$08,$F1,$07,$80,$07,$14
        .byte   $07,$5C,$06,$9C,$05,$E7,$04,$3C
        .byte   $04,$9B,$03,$02,$03,$72,$02,$EA
        .byte   $01,$6A,$01,$F1,$00,$80,$00,$14
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00

; sound pointers
sound_id_max:  .byte   $39
sound_data_high:  .byte   $8A
sound_data_base:  .byte   $B5
sound_data_low:  .byte   $8C
sound_pointer_table:  .byte   $9D,$90,$DF,$95,$9A,$9A,$06,$9C
        .byte   $D7,$A0,$BE,$A4,$14,$A7,$C1,$AA
        .byte   $C2,$AD,$E5,$B1,$23,$B2,$CB,$B4
        .byte   $16,$B5,$F6,$B8,$2B,$B9,$1E,$BE
        .byte   $97,$BF,$CF,$C0,$1D,$C5,$33,$C5
        .byte   $4E,$C5,$B4,$C5,$C0,$C5,$DE,$C6
        .byte   $22,$C6,$2D,$C6,$3D,$C6,$5A,$C6
        .byte   $6C,$C6,$82,$C6,$A9,$C6,$C6,$C6
        .byte   $D8,$C6,$F7,$C7,$0F,$C7,$33,$C7
        .byte   $49,$C7,$57,$C7,$68,$C7,$76,$C7
        .byte   $88,$C7,$A1,$C7,$ED,$C8,$03,$C8
        .byte   $14,$C8,$25,$C8,$3C,$C8,$4C,$C8
        .byte   $65,$C8,$87,$C8,$9A,$C8,$A6,$C9
        .byte   $B3,$C9,$CA,$CB,$67,$CD,$1E,$CD
        .byte   $BF,$1F,$01,$F0,$18,$80,$00,$00
        .byte   $00,$1F,$01,$F0,$18,$E4,$06,$00
        .byte   $00,$1E,$00,$F0,$10,$80,$00,$00
        .byte   $80,$1F,$13,$90,$01,$D6,$07,$00
        .byte   $00,$1F,$11,$50,$01,$E2,$00,$47
        .byte   $00,$1F,$14,$F0,$04,$EE,$04,$00
        .byte   $00,$1F,$19,$A0,$08,$00,$00,$00
        .byte   $00,$1F,$00,$B0,$09,$E3,$02,$00
        .byte   $00,$1F,$0D,$30,$01,$E3,$06,$00
        .byte   $00,$1F,$1B,$A0,$05,$CB,$02,$3C
        .byte   $00,$1F,$03,$50,$12,$00,$00,$00
        .byte   $00,$1F,$0A,$A0,$0E,$00,$00,$00
        .byte   $80,$1F,$19,$50,$10,$9E,$20,$26
        .byte   $00,$1D,$09,$90,$03,$CB,$01,$37
        .byte   $00,$1F,$1F,$E0,$1B,$9E,$58,$7C
        .byte   $00,$1F,$1B,$E0,$10,$B8,$00,$1E
        .byte   $00,$15,$14,$F0,$04,$00,$00,$00
        .byte   $00,$1A,$14,$F0,$04,$00,$00,$00
        .byte   $00,$1F,$17,$A0,$04,$80,$00,$00
        .byte   $00,$19,$01,$D0,$02,$00,$00,$00
        .byte   $00,$1F,$1D,$B0,$0C,$E4,$05,$00
        .byte   $00,$1A,$18,$90,$07,$00,$00,$00
        .byte   $00,$1F,$02,$C0,$13,$80,$00,$00
        .byte   $00,$1F,$02,$C0,$15,$5D,$06,$06
        .byte   $00,$1F,$1E,$D0,$0A,$FF,$02,$3C
        .byte   $00,$1E,$18,$90,$01,$C6,$19,$32
        .byte   $00,$1F,$17,$80,$06,$E4,$02,$28
        .byte   $00,$1F,$17,$70,$01,$5A,$03,$00
        .byte   $00,$1F,$1C,$B0,$06,$00,$00,$00
        .byte   $00,$1F,$1D,$B0,$0C,$00,$00,$00
        .byte   $80,$1F,$1A,$A0,$05,$FF,$01,$33
        .byte   $00,$1E,$1A,$A0,$05,$E4,$04,$00
        .byte   $00,$1F,$19,$A0,$02,$00,$00,$00
        .byte   $00,$1F,$04,$80,$02,$80,$00,$00
        .byte   $00,$1F,$1E,$A0,$01,$C6,$00,$32
        .byte   $00,$1F,$1F,$90,$09,$FF,$00,$2D
        .byte   $00,$1F,$1C,$80,$01,$00,$00,$00
        .byte   $00,$1F,$1E,$A0,$02,$EF,$00,$46
        .byte   $00,$17,$02,$E0,$0F,$80,$00,$00
        .byte   $00,$1F,$1B,$A0,$01,$B0,$07,$62
        .byte   $00,$1F,$1F,$F0,$1F,$00,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$02,$00
        .byte   $00,$1F,$1F,$F0,$1F,$92,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$E3,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$4C,$00
        .byte   $00,$1F,$1F,$F0,$1F,$99,$7F,$00
        .byte   $00,$1D,$1F,$F0,$1F,$80,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$B7,$27,$00
        .byte   $00,$1F,$1F,$F0,$04,$80,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$A6,$7F,$00
        .byte   $80,$1F,$1F,$F0,$1F,$80,$00,$00
        .byte   $80,$1F,$1C,$E0,$1F,$DA,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$0A,$00
        .byte   $00,$1C,$13,$10,$1F,$FF,$7F,$00
        .byte   $00,$1F,$01,$00,$0F,$E3,$7F,$00
        .byte   $00,$1F,$1F,$F0,$08,$00,$00,$00
        .byte   $00,$1F,$15,$A0,$14,$00,$00,$00
        .byte   $00,$1F,$1F,$F0,$1F,$FF,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$A5,$7F,$00
        .byte   $00,$1F,$1F,$F0,$1F,$D0,$16,$00
        .byte   $00,$1F,$1A,$A0,$07,$CF,$36,$00
        .byte   $80,$00,$8C,$A6,$8E,$28,$8F,$27
        .byte   $90,$8F,$0A,$01,$05,$01,$11,$06
        .byte   $C8,$07,$0D,$18,$40,$08,$20,$09
        .byte   $01,$03,$8D,$80,$60,$66,$69,$6B
        .byte   $2B,$8D,$80,$02,$60,$20,$90,$06
        .byte   $BE,$8F,$80,$60,$6D,$6B,$8E,$80
        .byte   $02,$80,$6D,$6B,$2B,$02,$4D,$69
        .byte   $66,$60,$64,$66,$60,$69,$60,$66
        .byte   $60,$69,$60,$6B,$60,$2B,$02,$4C
        .byte   $6B,$69,$66,$89,$80,$37,$02,$58
        .byte   $77,$75,$72,$75,$08,$00,$75,$02
        .byte   $97,$08,$20,$60,$66,$68,$69,$6B
        .byte   $6D,$70,$72,$60,$6D,$60,$70,$60
        .byte   $72,$60,$36,$02,$01,$57,$01,$57
        .byte   $40,$60,$75,$60,$72,$70,$60,$31
        .byte   $02,$52,$60,$71,$8D,$80,$71,$72
        .byte   $60,$09,$02,$2C,$02,$4D,$69,$66
        .byte   $61,$03,$72,$74,$75,$01,$57,$01
        .byte   $77,$06,$64,$01,$59,$01,$79,$01
        .byte   $5C,$01,$5C,$01,$5E,$01,$7E,$01
        .byte   $03,$49,$01,$69,$01,$4B,$01,$4B
        .byte   $06,$78,$00,$4B,$01,$4D,$01,$6D
        .byte   $8B,$89,$86,$89,$88,$81,$85,$88
        .byte   $8B,$8D,$91,$04,$08,$18,$40,$08
        .byte   $16,$09,$01,$05,$02,$16,$6D,$6D
        .byte   $6B,$6D,$60,$6B,$60,$6D,$60,$6B
        .byte   $60,$6D,$6D,$60,$6B,$60,$0E,$01
        .byte   $8D,$57,$69,$66,$68,$69,$6B,$68
        .byte   $69,$6B,$6D,$69,$6B,$6D,$70,$6E
        .byte   $6D,$6B,$04,$08,$74,$72,$6D,$0E
        .byte   $03,$8D,$86,$74,$72,$71,$6D,$04
        .byte   $00,$04,$00,$18,$40,$08,$1C,$09
        .byte   $02,$05,$02,$2E,$92,$97,$99,$92
        .byte   $9C,$92,$9B,$92,$97,$9C,$92,$9B
        .byte   $92,$99,$12,$00,$8D,$B8,$92,$97
        .byte   $0E,$01,$8D,$95,$9C,$01,$03,$88
        .byte   $01,$A8,$86,$84,$80,$81,$84,$01
        .byte   $88,$01,$A8,$86,$84,$80,$83,$80
        .byte   $01,$84,$01,$84,$83,$81,$80,$81
        .byte   $80,$03,$97,$80,$97,$99,$80,$01
        .byte   $D9,$01,$99,$04,$00,$99,$9C,$9E
        .byte   $99,$03,$8B,$81,$89,$81,$88,$81
        .byte   $86,$81,$84,$12,$08,$8E,$10,$86
        .byte   $80,$01,$88,$01,$88,$84,$81,$03
        .byte   $97,$99,$9C,$80,$03,$88,$84,$81
        .byte   $03,$97,$99,$80,$99,$80,$01,$99
        .byte   $0E,$01,$8D,$DF,$01,$86,$01,$A6
        .byte   $88,$88,$86,$88,$80,$88,$86,$88
        .byte   $8D,$8D,$8B,$8D,$80,$8B,$8D,$80
        .byte   $16,$8D,$93,$17,$06,$C8,$07,$0A
        .byte   $18,$40,$08,$20,$09,$01,$03,$89
        .byte   $80,$60,$66,$6D,$66,$29,$02,$01
        .byte   $48,$01,$68,$80,$00,$89,$86,$84
        .byte   $00,$A3,$63,$64,$66,$A9,$60,$A8
        .byte   $03,$92,$99,$95,$97,$92,$99,$95
        .byte   $97,$02,$CE,$08,$00,$03,$6E,$02
        .byte   $8E,$07,$08,$08,$13,$C6,$C9,$CD
        .byte   $A4,$A5,$C6,$C9,$ED,$04,$00,$18
        .byte   $80,$08,$16,$09,$01,$04,$00,$7E
        .byte   $7E,$7E,$7E,$60,$7E,$60,$7E,$60
        .byte   $7E,$60,$7E,$7E,$60,$7E,$60,$0E
        .byte   $01,$8E,$71,$7E,$7A,$7C,$7E,$03
        .byte   $68,$64,$66,$68,$69,$66,$68,$69
        .byte   $6D,$6B,$69,$68,$07,$0C,$08,$11
        .byte   $A1,$A5,$A8,$AD,$04,$00,$02,$60
        .byte   $04,$00,$18,$40,$08,$1C,$07,$0A
        .byte   $09,$02,$05,$02,$2E,$92,$97,$99
        .byte   $92,$9C,$92,$9B,$92,$97,$9C,$92
        .byte   $9B,$92,$99,$12,$00,$8E,$C9,$92
        .byte   $97,$0E,$01,$8E,$A4,$9C,$40,$18
        .byte   $80,$07,$0E,$08,$09,$09,$01,$04
        .byte   $00,$9C,$95,$99,$95,$0E,$03,$8E
        .byte   $D3,$9E,$99,$9C,$99,$9E,$99,$9C
        .byte   $99,$9E,$99,$03,$88,$81,$86,$81
        .byte   $84,$81,$07,$0A,$08,$13,$01,$CD
        .byte   $08,$05,$01,$CD,$08,$13,$01,$CB
        .byte   $08,$05,$01,$CB,$08,$13,$01,$D0
        .byte   $08,$05,$01,$D0,$08,$13,$CF,$CB
        .byte   $01,$C8,$08,$05,$01,$C8,$08,$13
        .byte   $01,$C6,$08,$05,$01,$C6,$08,$13
        .byte   $01,$C1,$08,$05,$C1,$01,$E1,$16
        .byte   $8E,$A0,$17,$06,$FF,$08,$02,$09
        .byte   $02,$D2,$02,$B0,$70,$72,$01,$AB
        .byte   $01,$6B,$6D,$60,$CE,$71,$D2,$D0
        .byte   $02,$CE,$6E,$02,$90,$D2,$02,$B0
        .byte   $70,$72,$02,$AB,$60,$6C,$CD,$02
        .byte   $B2,$72,$71,$02,$B0,$70,$6F,$CE
        .byte   $D9,$09,$03,$06,$C8,$62,$62,$62
        .byte   $62,$60,$62,$60,$62,$60,$62,$60
        .byte   $62,$62,$60,$62,$60,$64,$64,$64
        .byte   $64,$60,$64,$60,$64,$60,$64,$60
        .byte   $64,$64,$60,$64,$60,$A6,$A8,$A9
        .byte   $AB,$06,$E6,$08,$0E,$94,$60,$94
        .byte   $60,$94,$60,$94,$60,$76,$76,$96
        .byte   $04,$00,$04,$00,$06,$C8,$08,$02
        .byte   $66,$60,$66,$66,$06,$F0,$08,$0E
        .byte   $92,$06,$C8,$08,$02,$66,$66,$0E
        .byte   $07,$8F,$8E,$04,$00,$69,$60,$69
        .byte   $69,$06,$F0,$08,$0E,$92,$08,$02
        .byte   $06,$C8,$69,$69,$0E,$01,$8F,$A7
        .byte   $04,$00,$6B,$60,$6B,$6B,$08,$0E
        .byte   $06,$F0,$92,$08,$02,$06,$C8,$6B
        .byte   $6B,$0E,$01,$8F,$BC,$04,$00,$61
        .byte   $60,$61,$61,$08,$0E,$06,$F0,$92
        .byte   $08,$02,$06,$C8,$61,$61,$0E,$02
        .byte   $8F,$D1,$08,$0E,$06,$E6,$76,$76
        .byte   $91,$76,$76,$91,$04,$00,$08,$02
        .byte   $06,$C8,$69,$60,$69,$69,$08,$0E
        .byte   $06,$F0,$92,$08,$02,$06,$C8,$69
        .byte   $69,$0E,$01,$8F,$F0,$04,$00,$6B
        .byte   $60,$6B,$6B,$08,$0E,$06,$F0,$92
        .byte   $08,$02,$06,$C8,$6B,$6B,$0E,$01
        .byte   $90,$09,$04,$00,$6D,$60,$6D,$6D
        .byte   $08,$0E,$06,$F0,$92,$08,$02,$06
        .byte   $C8,$6D,$6D,$0E,$02,$90,$1E,$6B
        .byte   $60,$6B,$6B,$08,$0E,$06,$F0,$92
        .byte   $08,$02,$06,$C8,$6B,$6B,$04,$00
        .byte   $69,$60,$69,$69,$08,$0E,$06,$F0
        .byte   $92,$08,$02,$06,$F0,$69,$69,$0E
        .byte   $01,$90,$42,$04,$00,$6B,$60,$6B
        .byte   $6B,$08,$0E,$06,$F0,$92,$08,$02
        .byte   $06,$C8,$6B,$6B,$0E,$01,$90,$57
        .byte   $04,$00,$61,$60,$61,$61,$08,$0E
        .byte   $06,$F0,$92,$08,$02,$06,$C8,$61
        .byte   $61,$0E,$02,$90,$6C,$08,$0E,$06
        .byte   $F0,$60,$96,$60,$96,$76,$76,$16
        .byte   $8F,$8C,$17,$06,$C8,$08,$0C,$07
        .byte   $0C,$04,$00,$E0,$0E,$07,$90,$95
        .byte   $04,$00,$68,$68,$68,$68,$60,$68
        .byte   $60,$68,$60,$68,$60,$68,$68,$60
        .byte   $68,$60,$0E,$01,$90,$9C,$AB,$AB
        .byte   $AB,$AB,$04,$00,$6C,$68,$65,$0E
        .byte   $03,$90,$B6,$6C,$68,$65,$68,$04
        .byte   $00,$04,$00,$63,$60,$6F,$6F,$68
        .byte   $60,$6F,$6F,$0E,$1E,$90,$C5,$63
        .byte   $68,$6C,$6A,$68,$6C,$6A,$68,$16
        .byte   $90,$C3,$17,$00,$90,$E8,$92,$31
        .byte   $93,$91,$94,$B1,$0A,$03,$05,$01
        .byte   $EB,$06,$96,$07,$0B,$18,$C0,$08
        .byte   $1C,$04,$00,$60,$74,$97,$99,$97
        .byte   $99,$77,$99,$80,$74,$60,$74,$97
        .byte   $99,$97,$99,$77,$99,$7A,$79,$77
        .byte   $0E,$04,$90,$F5,$07,$09,$18,$80
        .byte   $09,$02,$60,$74,$76,$77,$79,$77
        .byte   $76,$77,$7B,$79,$7B,$7E,$03,$68
        .byte   $6B,$6D,$08,$11,$06,$FF,$01,$6F
        .byte   $01,$EF,$04,$00,$18,$40,$04,$00
        .byte   $07,$0C,$09,$01,$08,$1C,$06,$64
        .byte   $60,$02,$03,$88,$86,$83,$02,$86
        .byte   $88,$02,$80,$60,$02,$88,$86,$88
        .byte   $8B,$8A,$88,$86,$01,$C3,$12,$48
        .byte   $91,$89,$08,$05,$C3,$02,$01,$A3
        .byte   $08,$1C,$03,$8F,$90,$92,$94,$96
        .byte   $02,$B9,$9B,$01,$D7,$08,$05,$02
        .byte   $01,$B7,$08,$1C,$97,$99,$97,$96
        .byte   $97,$01,$DB,$08,$05,$01,$DB,$08
        .byte   $1C,$01,$DE,$08,$05,$01,$DE,$18
        .byte   $80,$0E,$01,$91,$32,$08,$1C,$01
        .byte   $83,$63,$64,$86,$81,$06,$96,$C3
        .byte   $03,$B7,$B9,$01,$DB,$01,$9B,$B9
        .byte   $97,$D9,$B7,$B9,$DB,$DF,$03,$CA
        .byte   $01,$AD,$01,$8D,$08,$1C,$09,$01
        .byte   $18,$C0,$07,$0A,$01,$84,$04,$00
        .byte   $9C,$9B,$9C,$03,$88,$02,$8B,$02
        .byte   $8F,$84,$80,$83,$84,$88,$8B,$AF
        .byte   $12,$08,$91,$D9,$8D,$80,$8A,$02
        .byte   $01,$C6,$02,$C6,$01,$66,$60,$01
        .byte   $84,$0E,$01,$91,$B2,$92,$80,$8D
        .byte   $02,$01,$CF,$06,$F0,$01,$EF,$18
        .byte   $40,$07,$08,$60,$02,$8F,$8D,$8B
        .byte   $8D,$8B,$02,$8A,$68,$09,$02,$08
        .byte   $16,$60,$02,$88,$86,$83,$86,$83
        .byte   $02,$81,$01,$63,$01,$E3,$60,$02
        .byte   $88,$86,$88,$8B,$8A,$88,$66,$01
        .byte   $68,$08,$00,$01,$E8,$60,$02,$8B
        .byte   $8A,$88,$8A,$88,$A6,$E3,$09,$00
        .byte   $06,$96,$63,$02,$80,$63,$02,$80
        .byte   $63,$02,$80,$09,$01,$63,$60,$66
        .byte   $60,$16,$91,$2E,$17,$06,$FF,$07
        .byte   $0B,$08,$00,$09,$02,$E0,$E0,$E0
        .byte   $E0,$04,$00,$09,$03,$18,$80,$08
        .byte   $1C,$74,$60,$6F,$60,$72,$60,$73
        .byte   $74,$60,$74,$6F,$60,$72,$60,$73
        .byte   $60,$0E,$05,$92,$3D,$0C,$00,$07
        .byte   $09,$60,$74,$72,$6F,$72,$6F,$6D
        .byte   $6B,$6D,$6B,$68,$66,$68,$63,$66
        .byte   $01,$6F,$01,$CF,$00,$6F,$72,$74
        .byte   $77,$7B,$7E,$03,$68,$6B,$6F,$72
        .byte   $74,$77,$04,$08,$04,$08,$04,$08
        .byte   $07,$0A,$09,$01,$18,$80,$08,$04
        .byte   $04,$08,$74,$60,$6B,$60,$6F,$60
        .byte   $6B,$74,$60,$74,$6B,$60,$6F,$60
        .byte   $6B,$60,$0E,$01,$92,$8C,$04,$08
        .byte   $6F,$60,$68,$60,$6B,$60,$68,$6F
        .byte   $60,$6F,$68,$60,$6B,$60,$68,$60
        .byte   $0E,$01,$92,$A2,$6F,$60,$6B,$60
        .byte   $6D,$60,$6B,$6F,$60,$6F,$6B,$60
        .byte   $6D,$60,$6B,$60,$6F,$60,$68,$60
        .byte   $6B,$60,$68,$6F,$60,$6F,$68,$60
        .byte   $6B,$60,$68,$60,$73,$60,$6F,$60
        .byte   $70,$60,$6F,$73,$60,$73,$6F,$60
        .byte   $70,$60,$6F,$60,$14,$08,$93,$05
        .byte   $07,$0B,$08,$00,$AF,$AF,$AF,$00
        .byte   $52,$51,$4F,$4D,$4C,$4A,$48,$46
        .byte   $45,$43,$41,$03,$58,$10,$01,$92
        .byte   $82,$07,$0A,$08,$16,$A3,$A3,$A3
        .byte   $86,$18,$C0,$06,$FF,$08,$1C,$09
        .byte   $01,$0C,$02,$01,$84,$01,$84,$83
        .byte   $84,$88,$02,$8B,$02,$8F,$84,$80
        .byte   $83,$84,$88,$8B,$AF,$8D,$80,$8A
        .byte   $02,$01,$C6,$02,$C6,$01,$66,$60
        .byte   $01,$84,$01,$84,$83,$84,$88,$02
        .byte   $8B,$02,$8F,$84,$80,$83,$84,$88
        .byte   $8B,$AF,$92,$80,$8D,$02,$01,$CF
        .byte   $06,$C8,$01,$EF,$06,$F0,$08,$16
        .byte   $09,$01,$04,$00,$07,$08,$60,$02
        .byte   $03,$8F,$8D,$8B,$8D,$8B,$02,$8A
        .byte   $12,$08,$93,$74,$0C,$00,$01,$68
        .byte   $01,$C8,$A8,$A7,$0E,$02,$93,$56
        .byte   $68,$60,$02,$88,$86,$83,$86,$83
        .byte   $A1,$08,$00,$E3,$60,$02,$8F,$60
        .byte   $02,$8F,$60,$02,$8F,$6F,$60,$72
        .byte   $60,$16,$92,$7E,$17,$04,$00,$06
        .byte   $96,$08,$02,$09,$02,$04,$00,$60
        .byte   $74,$97,$99,$97,$99,$77,$99,$80
        .byte   $74,$60,$74,$97,$99,$97,$99,$77
        .byte   $99,$7A,$79,$77,$0E,$01,$93,$99
        .byte   $04,$48,$08,$02,$09,$02,$06,$FF
        .byte   $CA,$08,$1B,$AA,$01,$8A,$08,$02
        .byte   $88,$01,$CB,$08,$1B,$01,$CB,$0E
        .byte   $02,$93,$B4,$06,$96,$09,$02,$08
        .byte   $02,$60,$68,$66,$63,$66,$63,$61
        .byte   $03,$77,$79,$77,$74,$72,$74,$72
        .byte   $70,$01,$74,$01,$D4,$94,$74,$03
        .byte   $68,$03,$74,$6F,$72,$73,$04,$00
        .byte   $04,$00,$04,$00,$94,$8F,$92,$73
        .byte   $94,$74,$8F,$92,$93,$0E,$02,$93
        .byte   $F6,$8F,$8F,$94,$6F,$8F,$75,$90
        .byte   $92,$94,$90,$8B,$8D,$6F,$90,$70
        .byte   $8B,$8D,$8F,$95,$90,$91,$73,$95
        .byte   $75,$90,$91,$93,$8F,$92,$94,$97
        .byte   $99,$9B,$9E,$9F,$13,$00,$94,$36
        .byte   $AF,$AF,$AF,$02,$92,$74,$0F,$01
        .byte   $93,$F4,$AF,$AF,$AF,$02,$92,$01
        .byte   $70,$02,$01,$90,$70,$90,$8D,$8F
        .byte   $9B,$90,$01,$90,$01,$70,$70,$90
        .byte   $90,$8D,$8F,$9B,$90,$8F,$02,$80
        .byte   $6F,$8F,$8D,$8F,$9B,$8F,$01,$95
        .byte   $01,$75,$75,$95,$94,$94,$92,$92
        .byte   $91,$90,$02,$80,$70,$90,$8D,$8F
        .byte   $9B,$90,$01,$90,$01,$70,$70,$90
        .byte   $90,$8D,$8F,$9B,$90,$8F,$80,$8F
        .byte   $92,$8F,$93,$8F,$96,$8F,$BB,$BB
        .byte   $BB,$BB,$04,$00,$60,$74,$94,$92
        .byte   $73,$02,$94,$02,$AF,$90,$9C,$90
        .byte   $9C,$92,$9E,$8F,$9B,$0E,$01,$94
        .byte   $86,$B4,$B4,$B3,$B3,$B2,$B2,$B0
        .byte   $B0,$EF,$60,$BB,$BB,$02,$9B,$9B
        .byte   $9B,$16,$93,$F2,$17,$06,$C8,$07
        .byte   $0A,$08,$0C,$04,$00,$A3,$0E,$06
        .byte   $94,$B7,$63,$68,$68,$68,$A3,$A3
        .byte   $A3,$A3,$63,$08,$03,$02,$89,$08
        .byte   $0C,$87,$87,$85,$65,$65,$63,$68
        .byte   $68,$68,$04,$00,$04,$00,$A3,$0E
        .byte   $06,$94,$D8,$63,$68,$68,$68,$0F
        .byte   $02,$94,$D6,$63,$04,$00,$07,$0C
        .byte   $66,$0E,$0A,$94,$E8,$68,$68,$68
        .byte   $6A,$A6,$63,$60,$63,$63,$6A,$60
        .byte   $60,$60,$63,$6C,$6C,$6C,$04,$00
        .byte   $04,$00,$63,$60,$6F,$6F,$63,$6C
        .byte   $60,$6F,$63,$6F,$6D,$60,$63,$6F
        .byte   $6F,$6F,$0E,$06,$95,$04,$63,$60
        .byte   $6C,$6C,$63,$60,$6C,$6C,$66,$66
        .byte   $60,$66,$63,$6C,$6C,$60,$04,$00
        .byte   $66,$60,$6C,$6C,$63,$6C,$60,$6C
        .byte   $66,$6C,$6D,$60,$63,$6C,$6C,$60
        .byte   $0E,$05,$95,$2A,$66,$60,$6C,$6C
        .byte   $63,$60,$60,$60,$66,$6C,$6D,$60
        .byte   $63,$6C,$6C,$6C,$66,$60,$6C,$6C
        .byte   $63,$6C,$60,$6C,$66,$6C,$6D,$60
        .byte   $63,$60,$60,$63,$04,$00,$66,$60
        .byte   $63,$6C,$63,$6C,$6D,$66,$6C,$60
        .byte   $65,$6C,$63,$60,$6C,$63,$0E,$0D
        .byte   $95,$60,$66,$60,$6C,$6C,$63,$6C
        .byte   $60,$66,$63,$6C,$6D,$60,$63,$6D
        .byte   $60,$63,$63,$6D,$60,$60,$63,$6D
        .byte   $60,$60,$63,$6D,$60,$60,$63,$60
        .byte   $6A,$60,$16,$95,$02,$17,$00,$95
        .byte   $A3,$96,$EB,$98,$47,$99,$93,$0A
        .byte   $04,$18,$40,$05,$01,$EB,$08,$1D
        .byte   $07,$0A,$06,$E6,$03,$96,$80,$96
        .byte   $80,$09,$01,$8D,$80,$8D,$8F,$04
        .byte   $00,$18,$C0,$80,$79,$78,$79,$7B
        .byte   $80,$79,$78,$79,$7B,$A0,$0E,$01
        .byte   $95,$BB,$02,$9F,$02,$03,$88,$01
        .byte   $8A,$CA,$01,$8A,$6F,$80,$6F,$80
        .byte   $6F,$80,$6F,$80,$6F,$60,$04,$00
        .byte   $07,$08,$06,$FF,$09,$02,$08,$16
        .byte   $B3,$02,$9A,$01,$BB,$08,$17,$02
        .byte   $01,$9B,$08,$16,$73,$60,$97,$76
        .byte   $60,$76,$74,$60,$01,$B4,$08,$17
        .byte   $94,$02,$01,$94,$08,$16,$BD,$02
        .byte   $9B,$01,$BA,$08,$17,$02,$01,$9A
        .byte   $08,$16,$76,$74,$73,$60,$73,$60
        .byte   $73,$74,$60,$01,$B6,$08,$17,$96
        .byte   $02,$01,$96,$06,$E6,$08,$16,$01
        .byte   $97,$08,$17,$01,$B7,$08,$16,$77
        .byte   $79,$02,$9B,$02,$99,$97,$02,$96
        .byte   $02,$93,$01,$AF,$08,$17,$AF,$01
        .byte   $8F,$08,$16,$02,$94,$02,$96,$B8
        .byte   $9B,$9A,$98,$02,$9A,$02,$9B,$01
        .byte   $BD,$08,$17,$BD,$01,$9D,$07,$0A
        .byte   $06,$96,$08,$0B,$80,$9B,$9B,$9B
        .byte   $06,$E6,$02,$9B,$02,$9A,$96,$80
        .byte   $06,$96,$99,$99,$99,$06,$E6,$02
        .byte   $99,$02,$98,$94,$80,$7D,$60,$9D
        .byte   $9B,$02,$9F,$02,$98,$01,$9B,$01
        .byte   $9B,$03,$6A,$60,$68,$60,$67,$60
        .byte   $02,$85,$02,$83,$83,$80,$68,$60
        .byte   $88,$87,$02,$85,$02,$83,$83,$80
        .byte   $67,$60,$87,$88,$02,$88,$02,$03
        .byte   $98,$98,$80,$03,$6A,$60,$8A,$88
        .byte   $02,$87,$02,$85,$83,$85,$83,$85
        .byte   $02,$A7,$85,$83,$04,$08,$06,$C8
        .byte   $6A,$60,$66,$60,$63,$66,$60,$68
        .byte   $60,$6A,$60,$02,$88,$12,$08,$96
        .byte   $D3,$66,$68,$0E,$01,$96,$B8,$09
        .byte   $01,$6A,$6D,$6F,$60,$6D,$60,$6A
        .byte   $6D,$60,$6F,$60,$6F,$6F,$60,$6F
        .byte   $60,$6F,$60,$16,$95,$E2,$17,$18
        .byte   $40,$08,$1D,$06,$E6,$07,$0A,$03
        .byte   $94,$80,$94,$80,$94,$80,$94,$96
        .byte   $04,$00,$09,$01,$08,$1D,$80,$03
        .byte   $68,$67,$68,$6A,$80,$68,$67,$68
        .byte   $6A,$A0,$0E,$01,$96,$FC,$08,$1C
        .byte   $80,$6F,$60,$74,$60,$6F,$60,$02
        .byte   $8D,$6F,$80,$6F,$60,$72,$60,$6F
        .byte   $60,$6D,$6F,$60,$02,$01,$8F,$08
        .byte   $17,$02,$01,$AF,$04,$00,$04,$00
        .byte   $07,$0C,$09,$00,$06,$E6,$18,$00
        .byte   $08,$0F,$80,$03,$6F,$60,$6F,$6A
        .byte   $80,$6F,$60,$6F,$6A,$A0,$12,$08
        .byte   $97,$68,$80,$6F,$60,$6F,$68,$80
        .byte   $6F,$60,$6F,$68,$A0,$80,$6F,$60
        .byte   $6F,$71,$80,$6F,$60,$6F,$71,$A0
        .byte   $0E,$01,$97,$32,$09,$00,$07,$08
        .byte   $18,$80,$08,$13,$CF,$D1,$D3,$D4
        .byte   $18,$40,$07,$0C,$08,$1C,$80,$73
        .byte   $74,$76,$8F,$60,$73,$74,$76,$8F
        .byte   $02,$80,$80,$71,$73,$74,$76,$60
        .byte   $76,$60,$76,$60,$74,$93,$91,$07
        .byte   $0B,$09,$01,$08,$09,$18,$C0,$04
        .byte   $00,$04,$00,$7B,$7B,$03,$6A,$6A
        .byte   $6F,$6F,$76,$76,$0E,$01,$97,$9D
        .byte   $13,$08,$97,$E6,$04,$00,$74,$74
        .byte   $7B,$7B,$03,$68,$68,$6F,$6F,$0E
        .byte   $01,$97,$B0,$04,$00,$78,$78,$7D
        .byte   $7D,$03,$6C,$6C,$71,$71,$0E,$01
        .byte   $97,$BF,$03,$74,$74,$7B,$7B,$03
        .byte   $68,$68,$6F,$6F,$03,$76,$76,$7B
        .byte   $7B,$03,$6A,$6A,$6F,$6F,$0F,$01
        .byte   $97,$9B,$04,$00,$74,$74,$7B,$7B
        .byte   $03,$68,$68,$6F,$6F,$0E,$01,$97
        .byte   $E6,$04,$00,$76,$76,$7B,$7B,$03
        .byte   $6A,$6A,$6F,$6F,$0E,$01,$97,$F5
        .byte   $04,$00,$78,$78,$7D,$7D,$03,$6C
        .byte   $6C,$71,$71,$0E,$01,$98,$04,$09
        .byte   $00,$06,$64,$18,$C0,$08,$0B,$04
        .byte   $08,$6F,$6F,$6D,$60,$6F,$60,$6D
        .byte   $6F,$60,$72,$60,$71,$60,$6F,$60
        .byte   $6D,$0E,$01,$98,$1B,$6F,$6F,$6D
        .byte   $60,$6F,$6D,$60,$6F,$60,$09,$01
        .byte   $66,$66,$60,$66,$60,$66,$60,$16
        .byte   $97,$30,$17,$08,$02,$07,$0C,$06
        .byte   $C8,$09,$02,$8F,$80,$8F,$80,$92
        .byte   $80,$92,$94,$80,$09,$03,$83,$87
        .byte   $83,$88,$87,$85,$83,$80,$83,$8A
        .byte   $83,$88,$81,$82,$83,$80,$83,$87
        .byte   $83,$88,$87,$85,$83,$80,$83,$8A
        .byte   $83,$87,$81,$82,$83,$04,$00,$09
        .byte   $03,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$63,$60,$63,$63,$63,$60,$63
        .byte   $63,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$68,$60,$68,$68,$68,$60,$68
        .byte   $68,$67,$60,$67,$67,$67,$60,$67
        .byte   $67,$67,$60,$67,$67,$67,$60,$67
        .byte   $67,$65,$60,$65,$65,$65,$60,$65
        .byte   $65,$65,$60,$65,$65,$65,$60,$65
        .byte   $65,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$6A,$60,$6A,$6A,$6A,$60,$6A
        .byte   $6A,$83,$60,$83,$60,$01,$63,$01
        .byte   $63,$83,$82,$09,$02,$8A,$8E,$94
        .byte   $60,$94,$60,$01,$74,$01,$74,$94
        .byte   $93,$8F,$93,$8C,$60,$8C,$60,$01
        .byte   $6C,$01,$6C,$8C,$8F,$91,$8C,$94
        .byte   $60,$94,$60,$01,$74,$01,$74,$96
        .byte   $94,$93,$91,$8F,$60,$8F,$60,$01
        .byte   $6F,$01,$6F,$8F,$8E,$8A,$8E,$94
        .byte   $60,$94,$60,$01,$74,$01,$74,$94
        .byte   $93,$8F,$93,$8A,$60,$8A,$60,$01
        .byte   $6A,$01,$6A,$8A,$91,$94,$96,$8C
        .byte   $60,$8C,$60,$01,$6C,$01,$6C,$8F
        .byte   $91,$8F,$8C,$72,$60,$72,$60,$72
        .byte   $72,$60,$72,$60,$72,$60,$72,$72
        .byte   $60,$72,$60,$74,$60,$74,$60,$74
        .byte   $74,$60,$74,$60,$74,$60,$74,$74
        .byte   $60,$74,$60,$6F,$60,$6F,$60,$6F
        .byte   $6F,$60,$6F,$60,$6F,$6F,$60,$6F
        .byte   $60,$6F,$60,$16,$98,$79,$17,$08
        .byte   $0C,$07,$0A,$06,$96,$A9,$A9,$A9
        .byte   $88,$88,$04,$00,$06,$C8,$63,$60
        .byte   $6C,$6C,$68,$6C,$60,$6C,$63,$60
        .byte   $6C,$6C,$68,$6C,$6C,$60,$0E,$03
        .byte   $99,$9E,$04,$00,$04,$00,$06,$C8
        .byte   $63,$60,$6C,$6C,$68,$6C,$60,$6C
        .byte   $63,$60,$6C,$6C,$68,$6C,$6C,$60
        .byte   $0E,$07,$99,$B8,$04,$00,$06,$F0
        .byte   $63,$6F,$6D,$6F,$68,$6F,$6D,$6F
        .byte   $0E,$0F,$99,$D0,$04,$00,$63,$6F
        .byte   $63,$6F,$68,$63,$60,$63,$12,$00
        .byte   $99,$FA,$63,$68,$6F,$6F,$68,$6F
        .byte   $6D,$6F,$0E,$02,$99,$E0,$60,$68
        .byte   $68,$60,$68,$60,$68,$60,$16,$99
        .byte   $B6,$17,$00,$9A,$0F,$9A,$EA,$9B
        .byte   $DA,$9C,$5B,$0A,$05,$05,$03,$00
        .byte   $07,$0A,$04,$08,$04,$08,$06,$AA
        .byte   $08,$0B,$18,$80,$09,$01,$A8,$8B
        .byte   $AF,$AF,$88,$A7,$8A,$B0,$B0,$87
        .byte   $0E,$01,$9A,$18,$04,$00,$07,$0C
        .byte   $18,$40,$08,$1A,$0D,$00,$80,$03
        .byte   $88,$8B,$8D,$80,$8D,$80,$8D,$80
        .byte   $6F,$6D,$8B,$80,$AD,$8B,$6D,$60
        .byte   $12,$08,$9A,$63,$80,$02,$AB,$4C
        .byte   $AD,$8D,$02,$01,$6B,$0D,$0C,$02
        .byte   $CB,$01,$A8,$0E,$03,$9A,$30,$80
        .byte   $02,$AB,$4F,$B0,$90,$02,$01,$6F
        .byte   $01,$EF,$04,$08,$07,$06,$18,$C0
        .byte   $08,$11,$06,$FF,$E8,$EB,$01,$ED
        .byte   $08,$01,$02,$01,$CD,$08,$11,$8B
        .byte   $8D,$EB,$E8,$01,$E3,$08,$01,$01
        .byte   $E3,$0E,$01,$9A,$6E,$18,$80,$08
        .byte   $11,$ED,$01,$F0,$F0,$08,$05,$02
        .byte   $01,$D0,$90,$92,$08,$11,$01,$F4
        .byte   $08,$05,$01,$D4,$08,$11,$D7,$F6
        .byte   $F2,$01,$F4,$08,$01,$F4,$01,$F4
        .byte   $02,$C0,$07,$0A,$08,$0B,$06,$DC
        .byte   $18,$C0,$92,$90,$8F,$80,$88,$8D
        .byte   $80,$8D,$80,$88,$8B,$6D,$6B,$88
        .byte   $8D,$80,$8D,$80,$88,$09,$02,$18
        .byte   $40,$88,$80,$8B,$8D,$80,$8D,$80
        .byte   $88,$8B,$6D,$6B,$88,$8D,$80,$8D
        .byte   $80,$88,$16,$9A,$16,$17,$04,$48
        .byte   $06,$DC,$07,$09,$18,$80,$08,$07
        .byte   $09,$01,$04,$48,$EF,$0D,$14,$01
        .byte   $EE,$0E,$01,$9A,$F6,$04,$00,$07
        .byte   $0C,$08,$1A,$0D,$00,$80,$9B,$03
        .byte   $88,$88,$80,$88,$80,$88,$80,$68
        .byte   $67,$86,$80,$A8,$86,$88,$12,$08
        .byte   $9B,$2F,$80,$02,$A6,$A8,$88,$01
        .byte   $86,$0D,$0C,$02,$C6,$01,$A3,$0E
        .byte   $03,$9B,$01,$0D,$00,$80,$02,$A6
        .byte   $A8,$88,$01,$87,$01,$E7,$04,$40
        .byte   $07,$08,$18,$40,$08,$07,$06,$FF
        .byte   $FB,$08,$05,$01,$FB,$08,$11,$01
        .byte   $03,$E9,$08,$01,$01,$E9,$08,$00
        .byte   $18,$C0,$06,$DC,$80,$83,$84,$87
        .byte   $88,$8A,$80,$01,$8B,$01,$AB,$00
        .byte   $8A,$8B,$8A,$00,$A8,$87,$01,$88
        .byte   $E8,$08,$01,$01,$E8,$0E,$01,$9B
        .byte   $3A,$08,$11,$06,$FF,$E8,$ED,$01
        .byte   $EE,$08,$05,$01,$EE,$08,$12,$06
        .byte   $DC,$09,$00,$07,$0C,$80,$8F,$90
        .byte   $92,$B4,$92,$01,$90,$02,$01,$D0
        .byte   $01,$8F,$90,$01,$EF,$CB,$C8,$A0
        .byte   $AD,$AF,$B0,$92,$94,$80,$95,$80
        .byte   $97,$80,$09,$01,$01,$8D,$01,$8D
        .byte   $8B,$AD,$8B,$AD,$8B,$08,$17,$02
        .byte   $CD,$08,$12,$8B,$89,$07,$08,$06
        .byte   $FF,$01,$E8,$01,$E8,$08,$00,$03
        .byte   $94,$80,$97,$99,$80,$99,$80,$94
        .byte   $97,$79,$77,$74,$60,$99,$80,$99
        .byte   $80,$94,$16,$9A,$EA,$17,$06,$64
        .byte   $07,$0E,$08,$02,$09,$02,$04,$00
        .byte   $04,$00,$A8,$8B,$AF,$AF,$88,$A7
        .byte   $8A,$B0,$B0,$87,$0E,$08,$9B,$E4
        .byte   $80,$02,$A7,$A9,$89,$01,$8A,$06
        .byte   $FF,$01,$EA,$04,$00,$06,$64,$A8
        .byte   $8B,$AF,$AF,$88,$A7,$8A,$B0,$B0
        .byte   $87,$0E,$07,$9B,$FF,$AD,$90,$B4
        .byte   $B4,$8D,$AC,$8F,$B4,$B4,$8C,$04
        .byte   $00,$A9,$8D,$B2,$B2,$89,$0E,$01
        .byte   $9C,$1B,$04,$00,$AD,$90,$B4,$B4
        .byte   $8D,$0E,$01,$9C,$26,$04,$00,$AB
        .byte   $90,$B4,$B4,$8B,$0E,$01,$9C,$31
        .byte   $04,$00,$A9,$8D,$B0,$B0,$89,$0E
        .byte   $03,$9C,$3C,$04,$00,$A8,$8B,$AF
        .byte   $AF,$88,$A7,$8A,$B0,$B0,$87,$0E
        .byte   $01,$9C,$47,$16,$9B,$E2,$17,$07
        .byte   $0B,$08,$0C,$04,$00,$04,$00,$04
        .byte   $00,$06,$C8,$83,$8F,$88,$83,$8F
        .byte   $83,$06,$FF,$8C,$06,$C8,$83,$8F
        .byte   $83,$06,$FF,$8C,$06,$C8,$83,$83
        .byte   $83,$88,$8F,$0E,$08,$9C,$63,$80
        .byte   $88,$81,$88,$88,$81,$88,$88,$80
        .byte   $68,$68,$68,$60,$68,$60,$88,$88
        .byte   $88,$88,$04,$08,$8D,$03,$8F,$88
        .byte   $83,$8F,$83,$06,$FF,$8C,$06,$C8
        .byte   $83,$8F,$83,$06,$FF,$8C,$06,$C8
        .byte   $83,$83,$83,$88,$8F,$0E,$0D,$9C
        .byte   $96,$04,$00,$88,$83,$8F,$88,$80
        .byte   $8F,$88,$83,$0E,$01,$9C,$B5,$83
        .byte   $83,$88,$83,$83,$88,$8D,$83,$83
        .byte   $83,$88,$83,$88,$88,$88,$88,$16
        .byte   $9C,$5F,$17,$00,$9C,$E0,$9D,$F3
        .byte   $9F,$57,$A0,$4E,$0A,$02,$04,$08
        .byte   $05,$01,$EB,$06,$78,$07,$0A,$18
        .byte   $40,$08,$07,$09,$01,$04,$08,$04
        .byte   $08,$88,$88,$6B,$88,$8D,$8F,$06
        .byte   $FF,$02,$8D,$06,$78,$6B,$6A,$0E
        .byte   $01,$9C,$F3,$13,$08,$9D,$22,$88
        .byte   $8B,$6A,$86,$88,$8B,$6A,$8A,$86
        .byte   $84,$86,$68,$8F,$02,$01,$AA,$02
        .byte   $01,$8A,$0F,$01,$9C,$F1,$88,$8B
        .byte   $6A,$8D,$8B,$8B,$6F,$8D,$90,$8F
        .byte   $92,$77,$96,$02,$01,$B4,$02,$01
        .byte   $94,$04,$08,$07,$0B,$18,$C0,$08
        .byte   $12,$88,$86,$68,$86,$88,$02,$80
        .byte   $68,$6A,$6B,$6F,$AD,$02,$8A,$02
        .byte   $86,$80,$A0,$8D,$8D,$6B,$8A,$8D
        .byte   $02,$80,$6F,$6D,$6B,$6A,$88,$88
        .byte   $68,$86,$02,$88,$80,$A0,$0E,$01
        .byte   $9D,$35,$04,$08,$8A,$8B,$8A,$88
        .byte   $02,$8A,$01,$A6,$01,$66,$0E,$01
        .byte   $9D,$66,$18,$80,$06,$C8,$02,$86
        .byte   $02,$84,$86,$02,$88,$02,$86,$88
        .byte   $02,$8A,$02,$88,$8A,$02,$8B,$02
        .byte   $8A,$8B,$04,$08,$08,$06,$18,$C0
        .byte   $6D,$60,$6D,$60,$70,$74,$60,$02
        .byte   $92,$90,$8F,$8D,$70,$60,$6F,$60
        .byte   $6D,$6B,$60,$02,$01,$AD,$02,$01
        .byte   $8D,$12,$08,$9D,$CD,$6D,$60,$6D
        .byte   $60,$70,$72,$60,$02,$94,$92,$94
        .byte   $95,$77,$60,$75,$60,$74,$92,$02
        .byte   $01,$B0,$02,$01,$90,$0E,$01,$9D
        .byte   $8E,$09,$02,$6D,$60,$6B,$60,$69
        .byte   $68,$60,$02,$86,$88,$8B,$8D,$70
        .byte   $60,$6F,$60,$6D,$8B,$8D,$07,$0C
        .byte   $08,$00,$09,$00,$6D,$6D,$60,$6D
        .byte   $60,$6D,$6D,$16,$9C,$E2,$17,$04
        .byte   $08,$06,$78,$07,$09,$18,$C0,$08
        .byte   $07,$09,$01,$04,$08,$04,$08,$8B
        .byte   $8B,$64,$84,$86,$86,$01,$A6,$01
        .byte   $66,$0E,$01,$9E,$01,$13,$08,$9E
        .byte   $31,$0C,$FD,$88,$8B,$6A,$86,$88
        .byte   $8B,$6A,$8A,$86,$0C,$00,$03,$94
        .byte   $96,$77,$97,$02,$01,$B9,$02,$01
        .byte   $99,$0F,$01,$9D,$FF,$0C,$FD,$88
        .byte   $8B,$6A,$8D,$8B,$8B,$6F,$8D,$90
        .byte   $0C,$FE,$6F,$72,$74,$77,$09,$01
        .byte   $6D,$6F,$72,$02,$01,$B4,$02,$01
        .byte   $94,$04,$00,$04,$00,$07,$0B,$0C
        .byte   $00,$06,$C8,$0C,$00,$18,$00,$08
        .byte   $0F,$80,$74,$80,$74,$80,$74,$72
        .byte   $60,$74,$A0,$0E,$01,$9E,$4F,$80
        .byte   $72,$80,$72,$80,$72,$6F,$60,$72
        .byte   $A0,$80,$74,$80,$74,$80,$74,$72
        .byte   $60,$74,$A0,$0F,$01,$9E,$4D,$07
        .byte   $07,$18,$80,$08,$13,$09,$01,$CD
        .byte   $D2,$D6,$01,$B9,$00,$01,$59,$08
        .byte   $00,$18,$00,$07,$0C,$03,$54,$52
        .byte   $51,$4F,$4D,$4C,$4A,$48,$46,$45
        .byte   $43,$09,$02,$18,$C0,$07,$0A,$06
        .byte   $C8,$08,$06,$04,$00,$76,$79,$7C
        .byte   $03,$68,$66,$64,$0E,$03,$9E,$AF
        .byte   $03,$76,$79,$7C,$7E,$03,$6A,$6D
        .byte   $70,$74,$04,$08,$09,$01,$08,$06
        .byte   $0C,$FE,$18,$C0,$6D,$60,$6D,$60
        .byte   $70,$74,$60,$02,$92,$90,$8F,$8D
        .byte   $70,$60,$6F,$60,$6D,$6B,$60,$12
        .byte   $08,$9F,$21,$8D,$0C,$00,$18,$00
        .byte   $06,$C8,$08,$00,$02,$94,$72,$60
        .byte   $6D,$60,$08,$0B,$18,$C0,$0C,$FE
        .byte   $6D,$60,$6D,$60,$70,$72,$60,$02
        .byte   $94,$92,$94,$95,$77,$60,$75,$60
        .byte   $74,$72,$60,$90,$0C,$00,$18,$00
        .byte   $08,$00,$74,$72,$70,$72,$74,$6B
        .byte   $6D,$0E,$01,$9E,$C6,$0C,$00,$6D
        .byte   $18,$00,$08,$00,$6D,$6D,$6B,$6D
        .byte   $70,$6F,$6D,$6B,$09,$02,$06,$64
        .byte   $08,$0F,$03,$97,$97,$77,$94,$97
        .byte   $97,$74,$97,$94,$97,$97,$77,$94
        .byte   $99,$09,$01,$08,$00,$07,$0C,$03
        .byte   $77,$77,$60,$77,$60,$77,$77,$16
        .byte   $9D,$F3,$17,$04,$00,$06,$C8,$07
        .byte   $0C,$08,$02,$09,$03,$04,$00,$04
        .byte   $00,$68,$02,$80,$66,$80,$65,$60
        .byte   $02,$85,$64,$60,$63,$60,$0E,$01
        .byte   $9F,$63,$13,$00,$9F,$98,$68,$02
        .byte   $80,$66,$80,$65,$60,$02,$85,$64
        .byte   $60,$64,$60,$61,$60,$61,$60,$64
        .byte   $64,$60,$02,$01,$A6,$02,$01,$86
        .byte   $0F,$01,$9F,$61,$68,$02,$80,$66
        .byte   $80,$65,$60,$02,$85,$64,$60,$64
        .byte   $60,$63,$60,$63,$60,$63,$63,$60
        .byte   $02,$01,$A8,$02,$01,$88,$04,$00
        .byte   $68,$68,$A0,$68,$68,$02,$A0,$68
        .byte   $67,$66,$66,$A0,$66,$66,$02,$A0
        .byte   $66,$65,$63,$63,$A0,$63,$63,$60
        .byte   $6D,$63,$60,$6D,$63,$6D,$6F,$64
        .byte   $64,$A0,$64,$64,$60,$6D,$64,$60
        .byte   $6D,$64,$6D,$70,$0E,$01,$9F,$B2
        .byte   $04,$08,$08,$0E,$09,$01,$6A,$60
        .byte   $6A,$60,$71,$6F,$6D,$6A,$60,$6A
        .byte   $60,$6A,$71,$6F,$6D,$6C,$0E,$01
        .byte   $9F,$E4,$04,$00
