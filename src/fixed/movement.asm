; ===========================================================================
; ENTITY MOVEMENT ROUTINES — $F580-$F897
; ===========================================================================
; Directional movement with collision, raw position updates,
; gravity/velocity system, animation reset, and facing logic.
; Shared by player (slot 0) and enemies.
;
; Velocity convention:
;   Y velocity: positive = rising, negative = falling
;   Position -= velocity, so negative vel subtracts a negative = adds to Y
;   Terminal velocity = $F9.00 (-7 px/frame)
; ===========================================================================

; -----------------------------------------------
; move_right_collide — move right + set facing + tile collision
; -----------------------------------------------
; Sets H-flip flag (facing right), saves pre-move X for player,
; moves right, checks screen boundary (player only), checks tile collision.
; Returns: C=1 if hit solid tile, C=0 if clear
move_right_collide:  lda     ent_flags,x ; set bit 6 = facing right (H-flip)
        ora     #ENT_FLAG_HFLIP         ; set H-flip bit (facing right)
        sta     ent_flags,x     ; store updated flags
move_right_collide_no_face:             ; 0 = full, skip
        cpx     #$00                    ; player (slot 0)?
        bne     move_right_collide_cont ; non-player → skip save
        lda     ent_x_px                ; save player X before move
        sta     $02                     ; save pre-move X pixel
        lda     ent_x_scr               ; write X position
        sta     $03                     ; save pre-move X screen
move_right_collide_cont:  jsr     move_sprite_right ; apply rightward movement
        cpx     #$00                    ; player?
        bne     mr_tile_check           ; non-player: just check tile
        jsr     check_platform_horizontal ; player overlapping horiz platform?
        bcc     mr_tile_check           ; no → normal tile check
        jsr     platform_push_x_left    ; push player left (away from platform)
        jsr     move_right_platform     ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_right_platform:  beq     mr_tile_check ; equal → normal tile check
        bcc     mr_tile_check           ; carry clear → normal tile check
        iny                             ; crossed screen right → check left tiles
        jmp     ml_tile_check           ; use left-side tile check

mr_tile_check:  jsr     check_tile_collision ; check tiles at entity position
        jsr     update_collision_flags  ; OAM buffer full?
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_right_collision_exit ; not solid → return clear
        jsr     snap_x_to_wall_right    ; snap to tile boundary (right)
        sec                             ; set carry = hit solid
move_right_collision_exit:  rts         ; return collision result in C

; -----------------------------------------------
; move_left_collide — move left + set facing + tile collision
; -----------------------------------------------

move_left_collide:  lda     ent_flags,x ; clear bit 6 = facing left (no H-flip)
        and     #$BF                    ; clear H-flip bit (facing left)
        sta     ent_flags,x             ; store updated flags
move_left_collide_no_face:
        cpx     #$00                    ; player (slot 0)?
        bne     move_left_collide_cont  ; non-player → skip save
        lda     ent_x_px                ; save player X before move
        sta     $02                     ; save pre-move X pixel
        lda     ent_x_scr               ; player X screen
        sta     $03                     ; save pre-move X screen
move_left_collide_cont:  jsr     move_sprite_left ; apply leftward movement
        cpx     #$00                    ; player?
        bne     ml_tile_check           ; non-player: just check tile
        jsr     check_platform_horizontal ; player overlapping horiz platform?
        bcc     ml_tile_check           ; no → normal tile check
        jsr     platform_push_x_right   ; push player right (away from platform)
        jsr     move_left_platform      ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_left_platform:  bcs     ml_tile_check ; carry set → normal tile check
        dey                             ; crossed screen left → check right tiles
        jmp     mr_tile_check           ; use right-side tile check

ml_tile_check:  jsr     check_tile_collision ; check tiles at entity position
        jsr     update_collision_flags  ; update ladder/collision flags
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_left_collision_exit ; not solid → return clear
        jsr     snap_x_to_wall_left     ; snap to tile boundary (left)
        sec                             ; set carry = hit solid
move_left_collision_exit:  rts          ; return collision result in C

; -----------------------------------------------
; move_down_collide — move down + tile collision
; -----------------------------------------------

move_down_collide:  cpx     #$00        ; player?
        bne     move_down_collide_cont  ; non-player → skip save
        lda     ent_y_px                ; save player Y before move
        sta     $02                     ; save pre-move Y pixel
        lda     ent_x_scr               ; save pre-move X screen
        sta     $03                     ; save pre-move X screen
move_down_collide_cont:  jsr     move_sprite_down ; apply downward movement
        cpx     #$00                    ; player?
        bne     md_tile_check           ; non-player: just check tile
        jsr     check_platform_vertical ; player standing on vert platform?
        bcc     md_tile_check           ; no → normal tile check
        jsr     platform_push_y_up      ; push player up (onto platform)
        jsr     move_down_platform      ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_down_platform:  beq     md_tile_check ; equal → normal tile check
        bcc     md_tile_check           ; carry clear → normal tile check
        iny                             ; crossed screen down → check up tiles
        jmp     mu_tile_check           ; use upward tile check

md_tile_check:  jsr     check_tile_horiz ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_down_collision_exit ; not solid → return clear
        jsr     snap_y_to_floor         ; snap to tile boundary (down)
        sec                             ; set carry = hit solid
move_down_collision_exit:  rts          ; return collision result in C

; -----------------------------------------------
; move_up_collide — move up + tile collision
; -----------------------------------------------

move_up_collide:  cpx     #$00          ; player?
        bne     move_up_collide_cont    ; non-player → skip save
        lda     ent_y_px                ; save pre-move Y pixel
        sta     $02                     ; store Y pixel to $02
        lda     ent_x_scr               ; save pre-move X screen
        sta     $03                     ; store X screen to $03
move_up_collide_cont:  jsr     move_sprite_up ; apply upward movement
        cpx     #$00                    ; player?
        bne     mu_tile_check           ; non-player: just check tile
        jsr     check_platform_vertical ; player touching vert platform?
        bcc     mu_tile_check           ; no → normal tile check
        jsr     platform_push_y_down    ; push player down (below platform)
        jsr     move_up_platform        ; check if crossed screen boundary
        sec                             ; return with carry set (blocked)
        rts                             ; return C=1 (blocked by platform)

move_up_platform:  bcs     mu_tile_check ; carry set → normal tile check
        dey                             ; crossed screen up → check down tiles
        jmp     md_tile_check           ; use downward tile check

mu_tile_check:  jsr     check_tile_horiz ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        clc                             ; clear carry (no collision yet)
        lda     $10                     ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     move_up_collision_exit  ; not solid → return clear
        jsr     snap_y_to_ceil          ; snap to tile boundary (up)
        sec                             ; set carry = hit solid
move_up_collision_exit:  rts            ; return collision result in C

; -----------------------------------------------
; move_vertical_gravity — $99-based vertical movement with collision
; -----------------------------------------------
; Combines velocity application, $99, screen boundary handling,
; and tile collision for entities affected by $99.
; Y velocity convention: positive = rising, negative = falling.
; Returns: C=1 if landed on solid ground, C=0 if airborne.

move_vertical_gravity:  cpx     #$00    ; player?
        bne     gravity_save_y          ; non-player → skip save
        lda     ent_y_px                ; save player Y before move
        sta     $02                     ; store Y pixel to $02
        lda     ent_x_scr               ; save pre-move X screen
        sta     $03                     ; store X screen to $03
gravity_save_y:  lda     ent_yvel,x     ; Y velocity whole byte
        bpl     gravity_rising          ; positive = rising
        jsr     apply_y_velocity_fall   ; pos -= vel (moves down)
        cpx     #$00                    ; player?
        bne     gravity_apply_physics   ; non-player → skip platform check
        jsr     check_platform_vertical ; player landing on platform?
        bcc     gravity_apply_physics   ; no → normal tile collision
        jsr     platform_push_y_up      ; push player up (onto platform)
        jsr     gravity_platform_fall   ; check screen boundary crossing
        jmp     gravity_reset_vel       ; landed on platform → reset velocity

gravity_platform_fall:  beq     gravity_apply_physics ; no boundary cross → apply physics
        bcc     gravity_apply_physics   ; carry clear → apply physics
        iny                             ; crossed screen → adjust tile offset
        jmp     mu_tile_check           ; → upward tile check on new screen

gravity_apply_physics:  jsr     apply_gravity ; increase downward velocity
        jsr     check_tile_horiz        ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        cpx     #$00                    ; player?
        bne     gravity_tile_check      ; non-player → skip ladder check
        lda     tile_at_feet_max        ; tile type = ladder top ($40)?
        cmp     #TILE_LADDER_TOP        ; player lands on ladder top
        beq     gravity_snap_floor      ; ladder top → land on it
gravity_tile_check:  lda     $10        ; tile collision result
        and     #$10                    ; isolate solid tile bit
        beq     gravity_airborne_exit   ; no solid → still falling
gravity_snap_floor:  jsr     snap_y_to_floor ; snap to tile boundary (down)
gravity_reset_vel:  jsr     reset_gravity ; reset velocity to rest value
        sec                             ; C=1: landed
        rts                             ; return C=1 (on ground)

gravity_rising:  iny                    ; Y=1: upward collision check offset
        jsr     apply_y_velocity_rise   ; pos -= vel (moves up)
        cpx     #$00                    ; player?
        bne     gravity_apply_rise      ; non-player → skip platform check
        jsr     check_platform_vertical ; player hitting platform from below?
        bcc     gravity_apply_rise      ; no collision → normal rise
        jsr     platform_push_y_down    ; push player down (below platform)
        jsr     gravity_platform_rise   ; check screen boundary crossing
        jmp     gravity_snap_ceil       ; hit platform ceiling → reset velocity

gravity_platform_rise:  bcs     gravity_apply_rise ; no boundary cross → normal rise
        dey                             ; may need opposite tile check
        jmp     md_tile_check           ; → downward tile check on new screen

gravity_apply_rise:  jsr     apply_gravity ; apply gravity (slows rise)
        jsr     check_tile_horiz        ; vertical tile collision check
        jsr     update_collision_flags  ; update ladder/collision flags
        lda     $10                     ; bit 4 = solid tile?
        and     #$10                    ; isolate solid tile bit
        beq     gravity_airborne_exit   ; no solid → continue rising
        jsr     snap_y_to_ceil          ; snap to tile boundary (up)
gravity_snap_ceil:  jsr     reset_gravity ; reset velocity
gravity_airborne_exit:  clc             ; C=0: airborne
        rts                             ; return C=0 (in air)

; -----------------------------------------------
; update_collision_flags — update entity collision flags from tile check
; -----------------------------------------------
; $10 = tile collision result from check_tile_collision
; tile_at_feet_max = tile type from last collision (upper nibble of $BF00)
; Updates ent_flags bit 5 (on-ladder) based on tile type $60 (ladder).

update_collision_flags:  lda     $10    ; bit 4 = solid tile hit?
        and     #$10                    ; isolate solid tile bit
        bne     collision_flags_done    ; solid tile? keep flags unchanged
        lda     ent_flags,x             ; clear on-ladder flag (bit 5)
        and     #$DF                    ; mask off bit 5 (on-ladder)
        sta     ent_flags,x             ; store updated flags
        lda     tile_at_feet_max        ; check tile type at feet
        cmp     #$60                    ; $60 = ladder body tile type
        bne     collision_flags_done    ; not ladder → done
        lda     ent_flags,x             ; set on-ladder flag (bit 5)
        ora     #$20                    ; (used by OAM: behind-background priority)
        sta     ent_flags,x             ; store on-ladder flag
collision_flags_done:  rts              ; return

; moves sprite right by its X speeds
; parameters:
; X: sprite slot

move_sprite_right:  lda     ent_x_sub,x ; load X subpixel position
        clc                             ; X subpixel += X subpixel speed
        adc     ent_xvel_sub,x          ; add X subpixel velocity
        sta     ent_x_sub,x             ; store new X subpixel
        lda     ent_x_px,x              ; load X pixel position
        adc     ent_xvel,x              ; add X pixel velocity + carry
        sta     ent_x_px,x              ; store new X pixel
        bcc     move_sprite_right_no_wrap ; no page cross? done
        lda     ent_x_scr,x             ; load X screen
        adc     #$00                    ; increment X screen on overflow
        sta     ent_x_scr,x             ; store new X screen
move_sprite_right_no_wrap:  rts         ; return

; moves sprite left by its X speeds
; parameters:
; X: sprite slot

move_sprite_left:  lda     ent_x_sub,x ; load X subpixel position
        sec                             ; X subpixel -= X subpixel speed
        sbc     ent_xvel_sub,x          ; subtract X subpixel velocity
        sta     ent_x_sub,x             ; store new X subpixel
        lda     ent_x_px,x              ; load X pixel position
        sbc     ent_xvel,x              ; subtract X pixel velocity + borrow
        sta     ent_x_px,x              ; store new X pixel
        bcs     move_sprite_left_no_wrap ; no page cross? done
        lda     ent_x_scr,x             ; load X screen
        sbc     #$00                    ; decrement X screen on underflow
        sta     ent_x_scr,x             ; store new X screen
move_sprite_left_no_wrap:  rts          ; return

; moves sprite down by its Y speeds
; parameters:
; X: sprite slot

move_sprite_down:  lda     ent_y_sub,x ; load Y subpixel position
        clc                             ; Y subpixel += Y subpixel speed
        adc     ent_yvel_sub,x          ; add Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; load Y pixel position
        adc     ent_yvel,x              ; add Y pixel velocity + carry
        sta     ent_y_px,x              ; store new Y pixel
        cmp     #$F0                    ; past screen bottom ($F0)?
        bcc     move_sprite_down_screen ; Y < $F0? no screen wrap
        adc     #$0F                    ; wrap: add $10 to adjust Y
        sta     ent_y_px,x              ; store wrapped Y pixel
        inc     ent_y_scr,x             ; increment Y screen
move_sprite_down_screen:  rts           ; return

; moves sprite up by its Y speeds
; parameters:
; X: sprite slot

move_sprite_up:  lda     ent_y_sub,x ; load Y subpixel position
        sec                             ; Y subpixel += Y subpixel speed
        sbc     ent_yvel_sub,x          ; subtract Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; load Y pixel position
        sbc     ent_yvel,x              ; subtract Y pixel velocity + borrow
        sta     ent_y_px,x              ; store new Y pixel
        bcs     move_sprite_up_screen   ; no underflow? done
        sbc     #$0F                    ; wrap: subtract $10 to adjust Y
        sta     ent_y_px,x              ; store wrapped Y pixel
        dec     ent_y_scr,x             ; decrement Y screen
move_sprite_up_screen:  rts             ; return

; -----------------------------------------------
; apply_y_speed — apply Y velocity + $99 (no collision)
; -----------------------------------------------
; Dispatches to fall/rise velocity application based on sign,
; then applies $99. Used by entities without tile collision.

apply_y_speed:  lda     ent_yvel,x      ; Y velocity sign check
        bpl     apply_y_speed_rising    ; positive = rising
        jsr     apply_y_velocity_fall   ; falling: apply downward velocity
        jmp     apply_gravity           ; then apply gravity

apply_y_speed_rising:  jsr     apply_y_velocity_rise ; rising: apply upward velocity
        jmp     apply_gravity           ; then apply gravity

; -----------------------------------------------
; apply_y_velocity_fall — apply Y velocity when falling
; -----------------------------------------------
; Position -= velocity. When velocity is negative (falling),
; subtracting negative = adding to Y = moving down on screen.
; Handles downward screen wrap at Y=$F0.

apply_y_velocity_fall:  lda     ent_y_sub,x ; Y subpixel -= Y sub velocity
        sec                             ; prepare for subtraction
        sbc     ent_yvel_sub,x          ; subtract Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; Y pixel -= Y velocity
        sbc     ent_yvel,x              ; negative vel → subtracting neg = add
        sta     ent_y_px,x              ; store new Y pixel
        cmp     #$F0                    ; screen height = $F0
        bcc     apply_y_vel_fall_done   ; within screen? done
        adc     #$0F                    ; wrap: add $10 to next screen
        sta     ent_y_px,x              ; store wrapped Y pixel
        inc     ent_y_scr,x             ; increment Y screen
apply_y_vel_fall_done:  rts             ; return

; -----------------------------------------------
; apply_y_velocity_rise — apply Y velocity when rising
; -----------------------------------------------
; Position -= velocity. When velocity is positive (rising),
; subtracting positive = decreasing Y = moving up on screen.
; Handles upward screen wrap (underflow).

apply_y_velocity_rise:  lda     ent_y_sub,x ; Y subpixel -= Y sub velocity
        sec                             ; prepare for subtraction
        sbc     ent_yvel_sub,x          ; subtract Y subpixel velocity
        sta     ent_y_sub,x             ; store new Y subpixel
        lda     ent_y_px,x              ; Y pixel -= Y velocity
        sbc     ent_yvel,x              ; positive vel → Y decreases (up)
        sta     ent_y_px,x              ; store new Y pixel
        bcs     apply_y_vel_rise_done   ; no underflow? done
        sbc     #$0F                    ; wrap: subtract $10 to prev screen
        sta     ent_y_px,x              ; store wrapped Y pixel
        dec     ent_y_scr,x             ; decrement Y screen
apply_y_vel_rise_done:  rts             ; return

; -----------------------------------------------
; apply_gravity — subtract $99 from Y velocity
; -----------------------------------------------
; Gravity ($99) subtracted from velocity each frame, making it
; more negative over time (falling faster). Clamps at terminal
; velocity $F9.00 (-7 px/frame). Player float timer ($B9) delays
; $99 while active.

apply_gravity:  cpx     #$00            ; player?
        bne     apply_gravity_player_check ; non-player → skip float check
        lda     $B9                     ; $B9 = player float timer
        beq     apply_gravity_player_check ; 0 = not floating, apply gravity
        dec     $B9                     ; decrement float timer
        bne     apply_gravity_exit      ; still floating? skip gravity
apply_gravity_player_check:  lda     ent_yvel_sub,x ; Y sub velocity -= gravity
        sec                             ; prepare for subtraction
        sbc     gravity                 ; subtract gravity value
        sta     ent_yvel_sub,x          ; store new Y sub velocity
        lda     ent_yvel,x              ; Y velocity -= borrow
        sbc     #$00                    ; propagate borrow to whole byte
        sta     ent_yvel,x              ; store new Y velocity
        bpl     apply_gravity_exit      ; still positive (rising)? done
        cmp     #$F9                    ; check against terminal velocity $F9
        bcs     apply_gravity_exit      ; within limit (-7 to -1)? done
        lda     ent_anim_id,x           ; check entity animation ID
        cmp     #$13                    ; $13 = exempt from terminal vel clamp
        beq     apply_gravity_exit      ; special entity → no clamp
        lda     #$F9                    ; clamp to terminal velocity
        sta     ent_yvel,x              ; clamp Y velocity to $F9.00
        lda     #$00                    ; clear sub velocity
        sta     ent_yvel_sub,x          ; clear Y sub velocity
apply_gravity_exit:  rts                ; return

; resets a sprite's $99/downward Y velocity
; parameters:
; X: sprite slot

reset_gravity:  cpx     #$00            ; $00 means player
        beq     reset_gravity_player    ; player → different rest velocity
        lda     #$AB                    ; enemy rest vel sub = $AB
        sta     ent_yvel_sub,x          ; store Y sub velocity
        lda     #$FF                    ; enemy rest vel = $FF ($FFAB = -0.33)
        sta     ent_yvel,x              ; store Y velocity whole byte
        rts                             ; entity rest vel = $FFAB

reset_gravity_player:  lda     #$C0     ; player rest vel sub = $C0
        sta     ent_yvel_sub,x          ; store Y sub velocity
        lda     #$FF                    ; player rest vel = $FF ($FFC0 = -0.25)
        sta     ent_yvel,x              ; store Y velocity whole byte
        rts                             ; player rest vel = $FFC0

; resets sprite's animation & sets ID
; parameters:
; A: OAM ID
; X: sprite slot

reset_sprite_anim:  sta     ent_anim_id,x ; store parameter -> OAM ID
        lda     #$00                    ; clear animation state
        sta     ent_anim_state,x        ; store cleared anim state
        lda     ent_anim_frame,x        ; load current anim frame
        and     #$80                    ; preserve damage flash bit (bit 7)
        sta     ent_anim_frame,x        ; reset tick counter, keep flash flag
        rts                             ; return

; -----------------------------------------------
; init_child_entity — initialize a new child entity in slot Y
; -----------------------------------------------
; Sets up a child entity spawned by parent entity in slot X.
; Inherits the parent's horizontal flip for facing direction.
;
; parameters:
;   X = parent entity slot
;   Y = child entity slot (from find_enemy_freeslot_y)
;   A = child entity shape/type value (stored to ent_anim_id,y)
; modifies:
;   ent_status,y = $80 (active)
;   ent_flags,y = parent's hflip | $90 (active + flags)
;   ent_y_scr,y = 0 (Y screen)
;   ent_anim_state,y = 0 (timer/counter cleared)
;   ent_anim_frame,y = bit 7 only preserved
; -----------------------------------------------

init_child_entity:  sta     ent_anim_id,y ; child shape/type
        lda     #$00                    ; clear anim state
        sta     ent_anim_state,y        ; clear child anim state
        sta     ent_y_scr,y             ; clear child Y screen
        lda     ent_flags,x             ; read parent flags
        and     #ENT_FLAG_HFLIP         ; isolate H-flip (bit 6)
        ora     #$90                    ; combine with active + child flags
        sta     ent_flags,y             ; store child flags
        lda     #$80                    ; status $80 = active
        sta     ent_status,y            ; activate child entity
        lda     ent_anim_frame,y        ; preserve only bit 7 of ent_anim_frame
        and     #$80                    ; isolate damage flash bit
        sta     ent_anim_frame,y        ; store cleared anim frame
        rts                             ; return

; faces a sprite toward the player
; parameters:
; X: sprite slot
face_player:

        lda     #FACING_RIGHT           ; start facing right
        sta     ent_facing,x            ; default = facing right
        lda     ent_x_px,x              ; entity X pixel
        sec                             ; subtract player X pixel
        sbc     ent_x_px                ; of player
        lda     ent_x_scr,x             ; entity X screen
        sbc     ent_x_scr               ; subtract player X screen
        bcc     face_player_exit        ; entity left of player → keep facing right
        lda     #FACING_LEFT            ; entity right of player → face left
        sta     ent_facing,x            ; store facing left
face_player_exit:  rts                  ; return

; -----------------------------------------------
; set_sprite_hflip — set horizontal flip from facing direction
; -----------------------------------------------
; Converts ent_facing,x bit 0 (facing right) into ent_flags,x bit 6
; (NES horizontal flip). Three ROR shifts move bit 0 → bit 6.
; Sprites face left by default; bit 6 set = flip to face right.
;
; parameters:
;   X = entity slot
; modifies:
;   ent_flags,x bit 6 = set if facing right (ent_facing,x bit 0)
; -----------------------------------------------
set_sprite_hflip:

        lda     ent_facing,x            ; load facing direction
        ror     a                       ; 3x ROR: bit 0 → bit 6
        ror     a                       ; (continued)
        ror     a                       ; (continued)
        and     #ENT_FLAG_HFLIP         ; isolate bit 6 (was bit 0)
        sta     temp_00                 ; save new hflip bit
        lda     ent_flags,x             ; clear old hflip, set new
        and     #$BF                    ; clear bit 6 (old hflip)
        ora     temp_00                 ; merge new hflip bit
        sta     ent_flags,x             ; store updated flags
        rts                             ; return

; submit a sound ID to global buffer for playing
; if full, do nothing
; parameters:
; A: sound ID

submit_sound_ID_D9:  sta     $D9        ; also store ID in $D9

; this version doesn't store in $D9
submit_sound_ID:  stx     temp_00       ; preserve X
        ldx     $DA                     ; X = current circular buffer index
        sta     $01                     ; sound ID -> $01 temp
        lda     $DC,x                   ; if current slot != $88
        cmp     #$88                    ; buffer FULL, return
        bne     submit_sound_id_exit    ; slot in use → buffer full
        lda     $01                     ; add sound ID to current
        sta     $DC,x                   ; buffer slot
        inx                             ; advance buffer index
        txa                             ; increment circular buffer index
        and     #$07                    ; wrap index at 8 slots
        sta     $DA                     ; store updated buffer index
submit_sound_id_exit:  ldx     temp_00  ; restore X register
        rts                             ; return

; -----------------------------------------------
; entity_y_dist_to_player — |player_Y - entity_Y|
; -----------------------------------------------
; parameters:
;   X = entity slot
; results:
;   A = abs(player_Y - entity[X]_Y) (pixel portion, single screen)
;   carry = set if player is below entity, clear if above
; -----------------------------------------------

entity_y_dist_to_player:  lda     ent_y_px ; player_Y - entity_Y
        sec                             ; prepare for subtraction
        sbc     ent_y_px,x              ; subtract entity Y pixel
        bcs     entity_distance_calc_return ; positive → player is below
        eor     #$FF                    ; negate: player is above entity
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is above
entity_distance_calc_return:  rts       ; A = abs(Y distance)

; -----------------------------------------------
; entity_x_dist_to_player — |player_X - entity_X|
; -----------------------------------------------
; 16-bit subtraction (pixel + screen), returns pixel portion only.
; parameters:
;   X = entity slot
; results:
;   A = abs(player_X - entity[X]_X) (pixel portion)
;   carry = set if player is to the right, clear if left
; -----------------------------------------------

entity_x_dist_to_player:  lda     ent_x_px ; player_X_pixel - entity_X_pixel
        sec                             ; prepare for subtraction
        sbc     ent_x_px,x              ; subtract entity X pixel
        pha                             ; save low byte
        lda     ent_x_scr               ; player_X_screen - entity_X_screen
        sbc     ent_x_scr,x             ; (borrow propagates from pixel sub)
        pla                             ; recover pixel difference
        bcs     entity_distance_x_setup ; positive → player is to the right
        eor     #$FF                    ; negate: player is to the left
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is left
entity_distance_x_setup:  rts           ; A = abs(X distance)

; -----------------------------------------------
; calc_direction_to_player — 16-direction angle from entity to player
; -----------------------------------------------
; Computes the direction from entity X to the player as a 16-step
; compass index (0-15). Used by entity AI for homing/tracking.
;
; Direction values (clockwise from north):
;   $00=N  $01=NNE $02=NE  $03=ENE $04=E  $05=ESE $06=SE  $07=SSE
;   $08=S  $09=SSW $0A=SW  $0B=WSW $0C=W  $0D=WNW $0E=NW  $0F=NNW
;
; Algorithm:
;   1. Compute abs Y and X distances, track quadrant in Y register:
;      bit 2 = player above entity, bit 1 = player left of entity
;   2. Sort distances: $01 = larger, $00 = smaller (bit 0 = swapped)
;   3. Compute sub-angle from slope ratio:
;      larger/4 >= smaller → 0 (nearly axial)
;      larger/2 >= smaller → 1 (moderate)
;      else               → 2 (nearly diagonal)
;   4. Look up final direction from table: [quadrant*4 + sub-angle]
;
; parameters:
;   X = entity slot
; results:
;   A = direction index (0-15)
; -----------------------------------------------
calc_direction_to_player:

        ldy     #$00                    ; Y = quadrant bits
        lda     ent_y_px                ; player_Y - entity_Y
        sec                             ; prepare for subtraction
        sbc     ent_y_px,x              ; subtract entity Y pixel
        ldy     #$00                    ; (clear Y again — assembler artifact)
        bcs     entity_distance_abs_y   ; player below or same → skip
        eor     #$FF                    ; negate: player is above
        adc     #$01                    ; two's complement = abs value
        ldy     #$04                    ; Y bit 2 = player above
entity_distance_abs_y:  sta     temp_00 ; $00 = abs(Y distance)
        lda     ent_x_px                ; player_X - entity_X (16-bit)
        sec                             ; prepare for subtraction
        sbc     ent_x_px,x              ; subtract entity X pixel
        pha                             ; save pixel difference
        lda     ent_x_scr               ; screen portion
        sbc     ent_x_scr,x             ; subtract entity X screen
        pla                             ; recover pixel difference
        bcs     entity_distance_abs_x   ; player right or same → skip
        eor     #$FF                    ; negate: player is left
        adc     #$01                    ; two's complement = abs value
        iny                             ; Y bit 1 = player left
        iny                             ; (INY x2 = add 2 to quadrant)
entity_distance_abs_x:  sta     $01     ; $01 = abs(X distance)
        cmp     temp_00                 ; if X_dist >= Y_dist, no swap needed
        bcs     entity_distance_sub_angle ; X dominant → no swap
        pha                             ; swap so $01=larger, $00=smaller
        lda     temp_00                 ; load Y distance (smaller)
        sta     $01                     ; $01 = Y distance (now larger)
        pla                             ; recover X distance
        sta     temp_00                 ; $00 = X distance (now smaller)
        iny                             ; Y bit 0 = axes swapped (Y dominant)
entity_distance_sub_angle:  lda     #$00 ; $02 = sub-angle (0-2)
        sta     $02                     ; init sub-angle = 0
        lda     $01                     ; larger_dist / 4
        lsr     a                       ; divide by 2
        lsr     a                       ; divide by 2 again (total /4)
        cmp     temp_00                 ; if larger/4 >= smaller → nearly axial
        bcs     entity_distance_table_index ; nearly axial → sub-angle 0
        inc     $02                     ; sub-angle = 1 (moderate)
        asl     a                       ; larger/4 * 2 = larger/2
        cmp     temp_00                 ; if larger/2 >= smaller → moderate
        bcs     entity_distance_table_index ; moderate → sub-angle 1
        inc     $02                     ; sub-angle = 2 (nearly diagonal)
entity_distance_table_index:  tya       ; table index = quadrant * 4 + sub-angle
        asl     a                       ; quadrant * 2
        asl     a                       ; quadrant * 4
        clc                             ; prepare for addition
        adc     $02                     ; + sub-angle
        tay                             ; index → Y
        lda     direction_to_player_table,y ; look up 16-dir facing value
        rts                             ; return direction in A
; ---------------------------------------------------------------------------
; direction_to_player_table — 16-direction lookup by quadrant and sub-angle
; ---------------------------------------------------------------------------
; Index = quadrant * 4 + sub_angle. 8 quadrants × 4 entries (sub_angle 0-2
; used, entry 3 unused). Returns direction 0-15 (0=right, 4=up, 8=left, 12=down).
direction_to_player_table:
        .byte   $04,$05,$06,$04         ; quadrant 0
        .byte   $08,$07,$06,$04         ; quadrant 1
        .byte   $0C,$0B,$0A,$04         ; quadrant 2
        .byte   $08,$09,$0A,$04         ; quadrant 3
        .byte   $04,$03,$02,$04         ; quadrant 4
        .byte   $00,$01,$02,$04         ; quadrant 5
        .byte   $0C,$0D,$0E,$04         ; quadrant 6
        .byte   $00,$0F,$0E,$04         ; quadrant 7
; ---------------------------------------------------------------------------
; track_direction_to_player — rotate ent_facing 1 step toward player
; ---------------------------------------------------------------------------
; Called by entity AI (e.g. Chibee). Gets the 16-direction target toward the
; player, then adjusts ent_facing,x by +1 or -1 each call.
; Output: ent_facing,x updated, temp_00 = target direction.
track_direction_to_player:
        jsr     calc_direction_to_player ; get 16-dir target → A
        sta     temp_00                 ; save target direction
        lda     ent_facing,x            ; signed circular difference:
        clc                             ; (current + 8 - target) & $0F - 8
        adc     #$08                    ; offset by 8 to center range
        and     #$0F                    ; wrap to 0-15
        sec                             ; prepare for subtraction
        sbc     temp_00                 ; subtract target
        and     #$0F                    ; wrap to 0-15
        sec                             ; prepare for subtraction
        sbc     #$08                    ; remove offset → signed diff (-7..+8)
        beq     entity_direction_done   ; diff=0: already facing target
        bcs     entity_direction_dec    ; diff>0: target is CCW → turn CW (DEC)
        inc     ent_facing,x            ; diff<0: target is CW → turn CCW (INC)
        bne     entity_direction_wrap   ; (always branches)
entity_direction_dec:  dec     ent_facing,x ; turn CW (decrement)
entity_direction_wrap:  lda     ent_facing,x ; wrap direction to 0-15
        and     #$0F                    ; mask to 0-15 range
        sta     ent_facing,x            ; store wrapped facing
entity_direction_done:  rts             ; return

; -----------------------------------------------
; check_platform_vertical — check if player stands on a platform entity
; -----------------------------------------------
; Scans all entity slots ($1F→$01) for platform entities that
; the player (slot 0) is standing on. Uses hitbox tables at
; $FB3B (half-heights) and $FB5B (half-widths) indexed by
; the entity's shape (ent_hitbox & $1F).
;
; ent_flags flag bits for platform entities:
;   bit 7 = active/collidable
;   bit 2 = disabled (skip if set)
;   bit 1 = horizontal platform flag
;   bit 0 = vertical platform flag (can stand on)
;
; parameters:
;   X = entity slot (must be player slot 0)
; results:
;   carry = set if standing on platform, clear if not
;   $11 = vertical overlap (clamped to max 8), used as push distance
;   $5D = platform entity slot
;   $36/$37/$38 = platform velocity data (if entity type $14)
; -----------------------------------------------

check_platform_vertical:  lda     ent_flags,x ; entity not active? bail
        bpl     entity_plat_not_found   ; bit 7 clear = inactive, bail
        sty     temp_00                 ; save caller's Y
        ldy     #$1F                    ; start scanning from slot $1F
        sty     $01                     ; store current scan slot
entity_plat_empty_check:  lda     ent_status,y ; entity type empty? skip
        bpl     entity_plat_dec         ; bit 7 clear = inactive, skip
        lda     ent_flags,y             ; entity not active? skip
        bpl     entity_plat_dec         ; bit 7 clear = inactive, skip
        lda     ent_flags,y             ; bit 2 set = disabled? skip
        and     #$04                    ; check disabled bit
        bne     entity_plat_dec         ; disabled → skip
        lda     ent_flags,y             ; bits 0-1 = platform flags
        and     #$03                    ; neither set? skip
        beq     entity_plat_dec         ; neither set? skip
        and     #$01                    ; bit 0 = vertical platform?
        beq     entity_plat_distance    ; not set → check as horiz-only
        lda     ent_yvel,x              ; player Y velocity (falling = negative)
        bpl     entity_plat_dec         ; positive = rising, can't land → skip
entity_plat_distance:  jsr     player_x_distance ; $10 = |player_X - entity_X
        jsr     player_y_distance       ; $11 = |player_Y - entity_Y
        bcc     entity_plat_hitbox      ; player above entity? check hitbox
        lda     ent_flags,y             ; player below + bit0 (vert platform)?
        and     #$01                    ; can't land from below
        bne     entity_plat_dec         ; vert platform → skip (below)
entity_plat_hitbox:  lda     ent_hitbox,y ; shape index = ent_hitbox & $1F
        and     #$1F                    ; mask to hitbox ID
        tay                             ; Y = hitbox table index
        lda     $10                     ; X distance >= half-width? no overlap
        cmp     hitbox_mega_man_widths,y ; compare against hitbox width
        bcs     entity_plat_dec         ; outside width → skip
        sec                             ; Y overlap = half-height - Y distance
        lda     hitbox_mega_man_heights,y ; load hitbox half-height
        sbc     $11                     ; subtract Y distance
        bcc     entity_plat_dec         ; negative? no Y overlap
        sta     $11                     ; $11 = overlap amount
        cmp     #$08                    ; clamp to max 8 pixels
        bcc     entity_plat_slot_save   ; overlap < 8? no clamp needed
        lda     #$08                    ; clamp overlap to max 8
        sta     $11                     ; store clamped overlap
entity_plat_slot_save:  ldy     $01     ; Y = platform entity slot
        lda     ent_routine,y           ; entity routine == $14 (moving platform)?
        cmp     #$14                    ; check for moving platform type
        bne     entity_plat_slot_store  ; not $14? skip velocity copy
        lda     ent_facing,y            ; copy platform velocity to player vars
        sta     $36                     ; $36 = platform direction/speed
        lda     ent_xvel_sub,y          ; $37 = platform X velocity
        sta     $37                     ; store platform X sub velocity
        lda     ent_xvel,y              ; platform X velocity whole byte
        sta     $38                     ; $38 = platform X velocity
entity_plat_slot_store:  sty     $5D    ; $5D = platform entity slot
        ldy     temp_00                 ; restore caller's Y
        sec                             ; C=1: standing on platform
        rts                             ; return C=1

entity_plat_dec:  dec     $01           ; next slot (decrement $1F→$01)
        ldy     $01                     ; load next slot index
        bne     entity_plat_empty_check ; slot 0 = player, stop there
        ldy     temp_00                 ; restore caller's Y
entity_plat_not_found:  clc             ; C=0: not on any platform
        rts                             ; return C=0

; -----------------------------------------------
; check_platform_horizontal — check if player is pushed by a platform entity
; -----------------------------------------------
; Scans all entity slots ($1F→$01) for horizontal platform entities
; that overlap the player. Uses hitbox tables at $FB3B (half-heights)
; and $FB5B (half-widths). Unlike vertical check, this only requires
; bit 1 (horizontal flag) and does not check Y velocity direction.
;
; parameters:
;   X = entity slot (must be player slot 0)
; results:
;   carry = set if overlapping platform, clear if not
;   $10 = horizontal overlap (clamped to max 8), used as push distance
;   $5D = platform entity slot (shape index, not original slot)
; -----------------------------------------------

check_platform_horizontal:  lda     ent_flags,x ; entity not active? bail
        bpl     entity_plat_search_2_not_found ; bit 7 clear = not active, bail
        sty     temp_00                 ; save caller's Y
        ldy     #$1F                    ; start scanning from slot $1F
        sty     $01                     ; save current scan slot
entity_plat_search_2_check:  lda     ent_status,y ; entity type empty? skip
        bpl     entity_plat_search_2_dec ; bit 7 clear = inactive, skip
        lda     ent_flags,y             ; bit 2 set = disabled? skip
        and     #$04                    ; check bit 2 (disabled)
        bne     entity_plat_search_2_dec ; bit 2 set = disabled, skip
        lda     ent_flags,y             ; bit 1 = horizontal platform?
        and     #$02                    ; not set? skip
        beq     entity_plat_search_2_dec ; not horizontal platform → skip
        jsr     player_x_distance       ; $10 = |player_X - entity_X|
        jsr     player_y_distance       ; $11 = |player_Y - entity_Y|
        lda     ent_hitbox,y            ; shape index = ent_hitbox & $1F
        and     #$1F                    ; mask to hitbox ID
        tay                             ; Y = hitbox table index
        lda     $11                     ; Y distance >= half-height? no overlap
        cmp     hitbox_mega_man_heights,y ; compare against hitbox height
        bcs     entity_plat_search_2_dec ; outside height → skip
        sec                             ; X overlap = half-width - X distance
        lda     hitbox_mega_man_widths,y ; load hitbox half-width
        sbc     $10                     ; subtract X distance
        bcc     entity_plat_search_2_dec ; negative? no X overlap
        sta     $10                     ; $10 = overlap amount
        cmp     #$08                    ; clamp to max 8 pixels
        bcc     entity_plat_shape_save  ; overlap < 8? no clamp needed
        lda     #$08                    ; clamp overlap to max 8
        sta     $10                     ; store clamped overlap
entity_plat_shape_save:  sty     $5D    ; $5D = platform shape index
        ldy     temp_00                 ; restore caller's Y
        sec                             ; C=1: overlapping platform
        rts                             ; return C=1

entity_plat_search_2_dec:  dec     $01  ; next slot (decrement $1F→$01)
        ldy     $01                     ; load next slot index
        bne     entity_plat_search_2_check ; continue scanning
        ldy     temp_00                 ; restore caller's Y
entity_plat_search_2_not_found:  clc    ; C=0: not overlapping any platform
        rts                             ; return C=0

; -----------------------------------------------
; player_x_distance — |player_X - entity_X|
; -----------------------------------------------
; Computes absolute X distance between player (slot 0) and entity Y.
; parameters:
;   Y = entity slot
; results:
;   $10 = abs(player_X - entity[Y]_X) (pixel portion)
;   carry = set if player is to the right, clear if left
; -----------------------------------------------

player_x_distance:  lda     ent_x_px    ; player_X_pixel - entity_X_pixel
        sec                             ; prepare for subtraction
        sbc     ent_x_px,y              ; subtract entity X pixel
        pha                             ; save pixel difference
        lda     ent_x_scr               ; player_X_screen - entity_X_screen
        sbc     ent_x_scr,y             ; borrow from pixel subtract
        pla                             ; recover pixel difference
        bcs     entity_dist_x_setup_2   ; player >= entity? already positive
        eor     #$FF                    ; negate: entity is right of player
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is to the left
entity_dist_x_setup_2:  sta     $10     ; $10 = abs X distance
        rts                             ; return

; -----------------------------------------------
; player_y_distance — |player_Y - entity_Y|
; -----------------------------------------------
; Computes absolute Y distance between player (slot 0) and entity Y.
; parameters:
;   Y = entity slot
; results:
;   $11 = abs(player_Y - entity[Y]_Y) (pixel portion)
;   carry = set if player is below entity, clear if above
; -----------------------------------------------

player_y_distance:  lda     ent_y_px    ; player_Y - entity_Y
        sec                             ; prepare for subtraction
        sbc     ent_y_px,y              ; subtract entity Y pixel
        bcs     entity_dist_y_setup_2   ; player >= entity? positive
        eor     #$FF                    ; negate: player is above entity
        adc     #$01                    ; two's complement = abs value
        clc                             ; C=0 means player is above
entity_dist_y_setup_2:  sta     $11     ; $11 = abs Y distance
        rts                             ; return

; -----------------------------------------------
; platform_push_x_left — push entity X left by $10
; -----------------------------------------------
; Subtracts $10 from entity[X] X position, then compares
; against reference point $02/$03 to determine side.
; results: carry = set if entity is left of $02/$03
; -----------------------------------------------

platform_push_x_left:  sec              ; entity_X -= $10 (push left)
        lda     ent_x_px,x              ; load entity X pixel
        sbc     $10                     ; subtract push distance
        sta     ent_x_px,x              ; store updated X pixel
        lda     ent_x_scr,x             ; (16-bit: X screen)
        sbc     #$00                    ; propagate borrow to screen byte
        sta     ent_x_scr,x             ; store updated X screen
        jmp     compare_x               ; compare against reference point

; -----------------------------------------------
; platform_push_x_right — push entity X right by $10
; -----------------------------------------------

platform_push_x_right:  clc             ; entity_X += $10 (push right)
        lda     ent_x_px,x              ; load entity X pixel
        adc     $10                     ; add push distance
        sta     ent_x_px,x              ; store updated X pixel
        lda     ent_x_scr,x             ; (16-bit: X screen)
        adc     #$00                    ; propagate carry to screen byte
        sta     ent_x_scr,x             ; store updated X screen
compare_x:  sec                         ; compare: $02/$03 - entity_X
        lda     $02                     ; load pre-move X pixel
        sbc     ent_x_px,x              ; C=1: entity left of ref point
        lda     $03                     ; load pre-move X screen
        sbc     ent_x_scr,x             ; propagate borrow for 16-bit compare
        rts                             ; return with C = comparison result

; -----------------------------------------------
; platform_push_y_up — push entity Y up by $11
; -----------------------------------------------
; Subtracts $11 from entity[X] Y position, with screen
; underflow handling. Then compares against $02/$03.
; results: carry = set if entity is above $02/$03
; -----------------------------------------------

platform_push_y_up:  sec                ; entity_Y -= $11 (push up)
        lda     ent_y_px,x              ; load entity Y pixel
        sbc     $11                     ; subtract push distance
        sta     ent_y_px,x              ; store updated Y pixel
        bcs     compare_y               ; no underflow? skip
        dec     ent_y_scr,x             ; underflow: decrement Y screen
        jmp     compare_y               ; compare against reference point

; -----------------------------------------------
; platform_push_y_down — push entity Y down by $11
; -----------------------------------------------
; Adds $11 to entity[X] Y position, with $F0 screen
; overflow handling. Then compares against $02/$03.
; results: carry = set if entity is above $02/$03
; -----------------------------------------------

platform_push_y_down:  clc              ; entity_Y += $11 (push down)
        lda     ent_y_px,x              ; load entity Y pixel
        adc     $11                     ; add push distance
        sta     ent_y_px,x              ; store updated Y pixel
        bcs     entity_y_screen_inc     ; carry? wrapped past $FF
        cmp     #$F0                    ; below screen bottom ($F0)?
        bcc     compare_y               ; no → done
        adc     #$0F                    ; wrap Y: $F0+$0F+C = next screen
        sta     ent_y_px,x              ; store wrapped Y pixel
entity_y_screen_inc:  inc     ent_y_scr,x ; increment Y screen
compare_y:  sec                         ; compare: $02/$03 - entity_Y
        lda     $02                     ; load pre-move Y pixel
        sbc     ent_y_px,x              ; C=1: entity above ref point
        lda     $03                     ; load pre-move screen
        sbc     ent_y_scr,x             ; propagate borrow for 16-bit compare
        rts                             ; return with C = comparison result

; tests if a sprite is colliding
; with the player (Mega Man)
; parameters:
; X: sprite slot to check player collision for
; returns:
; Carry flag off = collision, on = no collision
check_player_collision:

        lda     player_state            ; load current player state
        cmp     #PSTATE_DEATH           ; is player dead
        beq     entity_hitbox_done      ; dead? skip collision
        cmp     #PSTATE_REAPPEAR        ; or reappearing?
        beq     entity_hitbox_done      ; reappearing? skip collision
        sec                             ; set carry (will return C=1 if no collision)
        lda     ent_flags,x             ; load entity flags
        bpl     entity_hitbox_done      ; if not active (bit 7 clear)
        and     #$04                    ; or disabled (bit 2 set)
        bne     entity_hitbox_done      ; disabled? skip collision
check_player_collision_hitbox:
        lda     ent_hitbox,x            ; load entity hitbox ID
        and     #$1F                    ; y = hitbox ID
        tay                             ; Y = hitbox table index
        lda     hitbox_mega_man_heights,y ; Mega Man hitbox height
        sta     temp_00                 ; -> $00
        lda     ent_anim_id             ; player anim ID
        cmp     #$10                    ; $10 = sliding?
        bne     entity_hitbox_x_load    ; not sliding? skip height adjust
        lda     temp_00                 ; adjust hitbox height
        sec                             ; by subtracting 8
        sbc     #$08                    ; reduce hitbox by 8 for slide
        sta     temp_00                 ; store reduced height
entity_hitbox_x_load:  lda     ent_x_px ; player X pixel
        sec                             ; player X - entity X
        sbc     ent_x_px,x              ; pixel difference
        pha                             ; save pixel result
        lda     ent_x_scr               ; taking screen into account
        sbc     ent_x_scr,x             ; via carry
        pla                             ; then take absolute value
        bcs     entity_hitbox_width_cmp ; positive → player is right
        eor     #$FF                    ; negate if player is left
        adc     #$01                    ; two's complement
entity_hitbox_width_cmp:  cmp     hitbox_mega_man_widths,y ; if abs(X delta) > hitbox X delta
        bcs     entity_hitbox_done      ; outside width? no collision
        lda     ent_y_px                ; player Y pixel
        sec                             ; get Y delta between
        sbc     ent_y_px,x              ; the two sprites
        bcs     entity_hitbox_return    ; player below? already positive
        eor     #$FF                    ; negate if player is above
        adc     #$01                    ; two's complement
entity_hitbox_return:  cmp     $00     ; compare Y dist vs hitbox height
        bcc     entity_hitbox_done      ; A < height: carry clear → no hit
entity_hitbox_done:  rts                ; return (caller checks carry)

; sprite hitbox heights for Mega Man collision
; the actual height is double this, cause it compares delta
hitbox_mega_man_heights:  .byte   $13,$1C,$18,$14,$1C,$28,$16,$1C
        .byte   $18,$18,$1C,$10,$24,$24,$34,$14
        .byte   $20,$0E,$1C,$1C,$3C,$1C,$2C,$14
        .byte   $2C,$2C,$14,$34,$0C,$0C,$0C,$0C

; sprite hitbox widths for Mega Man collision
; the actual width is double this, cause it compares delta
hitbox_mega_man_widths:  .byte   $0F,$14,$14,$14,$10,$20,$18,$14
        .byte   $10,$18,$18,$0C,$14,$20,$10,$18
        .byte   $1C,$14,$40,$0C,$0C,$0F,$0C,$10
        .byte   $28,$18,$28
        bit     $0808                   ; data (hitbox table padding)
        php                             ; data (hitbox table padding)
        php                             ; data (hitbox table padding)

; loops through all 3 weapon slots
; to check collision against each one
; for a passed in sprite slot
; parameters:
; X: sprite slot to check weapon collision for
; returns:
; Carry flag off = sprite is colliding with player's weapons, on = not
; $10: sprite slot of weapon collided with (if carry off)
check_sprite_weapon_collision:
        lda     player_state            ; check player state
        cmp     #PSTATE_DEATH           ; is player dead
        beq     entity_anim_collision_detected ; dead? skip collision
        cmp     #PSTATE_REAPPEAR        ; or reappearing?
        beq     entity_anim_collision_detected ; reappearing? skip collision
        lda     #$03                    ; start loop through
        sta     $10                     ; all 3 weapon slots
entity_anim_advance_loop:  ldy     $10  ; Y = current weapon slot
        lda     ent_status,y            ; weapon inactive?
        bpl     entity_anim_loop_counter ; not active (bit 7 clear)? skip
        lda     ent_flags,y             ; check weapon flags
        bpl     entity_anim_loop_counter ; not active? skip
        lda     ent_routine,y           ; check weapon routine
        cmp     #$0F                    ; or routine $0F (item pickup dying)
        beq     entity_anim_loop_counter ; dying item? skip
        lda     ent_anim_id,y           ; load weapon animation ID
        cmp     #$13                    ; or OAM ID $13
        beq     entity_anim_loop_counter ; excluded OAM ID? skip
        cmp     #$D7                    ; or $D7
        beq     entity_anim_loop_counter ; excluded OAM ID $D7? skip
        cmp     #$D8                    ; or $D8
        beq     entity_anim_loop_counter ; excluded OAM ID $D8? skip
        cmp     #$D9                    ; or $D9
        beq     entity_anim_loop_counter ; excluded OAM ID? skip
        lda     ent_flags,x             ; check target entity flags
        bpl     entity_anim_loop_counter ; not active? skip
        and     #$04                    ; then don't check collision
        bne     entity_anim_loop_counter ; disabled? skip collision
        lda     ent_x_px,y              ; weapon X pixel
        sta     temp_00                 ; $00 = weapon X pixel
        lda     ent_x_scr,y             ; weapon X screen
        sta     $01                     ; $01 = weapon X screen
        lda     ent_y_px,y              ; weapon Y pixel
        sta     $02                     ; $02 = weapon Y pixel
        jsr     check_weapon_collision  ; carry cleared == collision
        bcc     entity_anim_collision_return ; collision detected? return
entity_anim_loop_counter:  dec     $10  ; continue loop,
        bne     entity_anim_advance_loop ; stop at $00 (Mega Man)
entity_anim_collision_detected:  sec    ; no collision: set carry
entity_anim_collision_return:  rts      ; return

; parameters:
; X: sprite slot to check collision for
; $00: comparison sprite's X position
; $01: comparison sprite's X screen
; $02: comparison sprite's Y position
; returns:
; Carry flag off = collision, on = no collision

check_weapon_collision:  sec            ; set carry (no collision default)
        lda     ent_hitbox,x            ; get hitbox shape index
        and     #$1F                    ; mask to 5 bits
        tay                             ; Y = hitbox table index
        lda     temp_00                 ; weapon X pixel
        sec                             ; weapon X - entity X
        sbc     ent_x_px,x              ; pixel difference
        pha                             ; save result
        lda     $01                     ; weapon X screen
        sbc     ent_x_scr,x             ; subtract entity X screen
        pla                             ; recover pixel difference
        bcs     entity_anim_weapon_width ; positive? already absolute
        eor     #$FF                    ; negate for absolute value
        adc     #$01                    ; two's complement
entity_anim_weapon_width:  cmp     hitbox_weapon_widths,y ; if abs(X delta) > hitbox X delta
        bcs     entity_anim_collision_done ; outside width? no collision
        lda     $02                     ; weapon Y pixel
        sec                             ; weapon Y - entity Y
        sbc     ent_y_px,x              ; pixel difference
        bcs     entity_anim_weapon_height ; positive? already absolute
        eor     #$FF                    ; negate for absolute value
        adc     #$01                    ; two's complement
entity_anim_weapon_height:  cmp     hitbox_weapon_heights,y ; compare abs(Y) vs hitbox height
        .byte   $90,$00                 ; bcc = collision (carry clear)
entity_anim_collision_done:  rts

; sprite hitbox heights for weapon collision
; the actual height is double this, cause it compares delta
hitbox_weapon_heights:  .byte   $0A,$12,$0E,$0A,$12,$1E,$0C,$16
        .byte   $0E,$0E,$12,$04,$1A,$1A,$2A,$0A
        .byte   $16,$04,$12,$2E,$32,$12,$22,$0A
        .byte   $22,$06,$02,$2A,$02,$02,$02,$02

; sprite hitbox widths for weapon collision
; the actual width is double this, cause it compares delta
hitbox_weapon_widths:  .byte   $0B,$0F,$0D,$0F,$0B,$13,$13,$13
        .byte   $0B,$13,$13,$05,$0F,$1B,$0B,$13
        .byte   $17,$0F,$13,$07,$07,$0A,$07,$0B
        .byte   $23,$0F,$03
        .byte   $27
        .byte   $03
        .byte   $03
        .byte   $03
        .byte   $03

; find free sprite slot routine, return in X register
; searches sprite state table ent_status (enemy slots)
; for free slots (inactive)
; returns:
; Carry flag off = slot found, on = not found
; X: next free slot # (if carry off)
find_enemy_freeslot_x:
        ldx     #$1F                    ; start looping from slot $1F
entity_freeslot_x_check:  lda     ent_status,x ; check sprite state sign bit
        bpl     entity_freeslot_x_found ; off means inactive, return
        dex                             ; next slot (down)
        cpx     #$0F                    ; $00-$0F slots not for enemies
        bne     entity_freeslot_x_check ; so stop there
        sec                             ; return C=1
        rts                             ; for no slots found

entity_freeslot_x_found:  clc           ; return C=0 slot found
        rts                             ; return C=0 (slot found)

; find free sprite slot routine, return in Y register
; searches sprite state table ent_status (enemy slots)
; for free slots (inactive)
; returns:
; Carry flag off = slot found, on = not found
; Y: next free slot # (if carry off)

find_enemy_freeslot_y:  ldy     #$1F    ; start looping from slot $1F
entity_freeslot_y_check:  lda     ent_status,y ; check sprite state sign bit
        bpl     entity_freeslot_y_found ; off means inactive, return
        dey                             ; next slot (down)
        cpy     #$0F                    ; $00-$0F slots not for enemies
        bne     entity_freeslot_y_check ; so stop there
        sec                             ; return C=1
        rts                             ; for no slots found

entity_freeslot_y_found:  clc           ; return C=0 slot found
        rts                             ; return C=0 (slot found)

; -----------------------------------------------
; calc_homing_velocity — proportional X/Y velocity toward player
; -----------------------------------------------
; Computes entity velocity that points at the player with the
; given speed magnitude. The dominant axis (larger distance) gets
; full speed; the minor axis gets proportional speed:
;   minor_vel = speed * minor_dist / major_dist
;
; Uses two successive divide_16bit calls to compute this ratio.
;
; parameters:
;   X = entity slot
;   $03.$02 = speed magnitude (8.8 fixed-point, whole.sub)
; results:
;   ent_xvel,x.ent_xvel_sub,x = X velocity (8.8 fixed-point)
;   ent_yvel,x.ent_yvel_sub,x = Y velocity (8.8 fixed-point)
;   $0C = direction bitmask:
;     bit 0 (FACING_RIGHT) = player is right
;     bit 1 (FACING_LEFT)  = player is left
;     bit 2 ($04) = player is below
;     bit 3 ($08) = player is above
;   Callers typically store $0C into ent_facing,x and use it
;   to select move_sprite_right/left/up/down.
; -----------------------------------------------
calc_homing_velocity:

        jsr     entity_x_dist_to_player ; A = |x_dist|, C = player right
        sta     $0A                     ; $0A = x_distance
        lda     #$01                    ; $01 = right
        bcs     entity_dir_x_store      ; player is right? use $01
        lda     #$02                    ; $02 = left
entity_dir_x_store:  sta     temp_0C    ; $0C = X direction bit
        jsr     entity_y_dist_to_player ; A = |y_dist|, C = player below
        sta     $0B                     ; $0B = y_distance
        lda     #$04                    ; $04 = below
        bcs     entity_dir_combine      ; player is below? use $04
        lda     #$08                    ; $08 = above
entity_dir_combine:  ora     temp_0C    ; combine X and Y direction bits
        sta     temp_0C                 ; store combined direction bits
        lda     $0B                     ; compare y_dist vs x_dist
        cmp     $0A                     ; compare Y dist vs X dist
        bcs     entity_divide_y_vel     ; y_dist >= x_dist? Y dominant
        lda     $02                     ; X gets full speed
        sta     ent_xvel_sub,x          ; X vel sub = speed sub
        lda     $03                     ; X vel whole = speed whole
        sta     ent_xvel,x              ; X vel whole = speed whole
        lda     $0A                     ; divisor = x_distance
        sta     $01                     ; $01 = x_distance
        lda     #$00                    ; clear low byte
        sta     temp_00                 ; clear low byte of dividend
        jsr     divide_16bit            ; scale = (x_dist << 16) / speed
        lda     $04                     ; use scale factor as new divisor
        sta     $02                     ; use quotient as new divisor lo
        lda     $05                     ; load quotient high
        sta     $03                     ; use quotient as new divisor hi
        lda     $0B                     ; dividend = y_distance
        sta     $01                     ; $01 = y_distance
        lda     #$00                    ; clear low byte
        sta     temp_00                 ; clear low byte of dividend
        jsr     divide_16bit            ; result = speed * y_dist / x_dist
        lda     $04                     ; Y gets proportional speed
        sta     ent_yvel_sub,x          ; Y vel sub
        lda     $05                     ; load quotient high byte
        sta     ent_yvel,x              ; Y vel whole
        rts                             ; return

entity_divide_y_vel:  lda     $02       ; Y gets full speed
        sta     ent_yvel_sub,x          ; Y vel sub = speed sub
        lda     $03                     ; Y vel whole = speed whole
        sta     ent_yvel,x              ; Y vel whole = speed whole
        lda     $0B                     ; divisor = y_distance
        sta     $01                     ; $01 = y_distance
        lda     #$00                    ; clear low byte of dividend
        sta     temp_00                 ; store cleared low byte
        jsr     divide_16bit            ; compute scale factor
        lda     $04                     ; use scale factor as new divisor
        sta     $02                     ; use quotient as new divisor lo
        lda     $05                     ; load quotient high
        sta     $03                     ; use quotient as new divisor hi
        lda     $0A                     ; dividend = x_distance
        sta     $01                     ; $01 = x_distance
        lda     #$00                    ; clear low byte of dividend
        sta     temp_00                 ; store cleared low byte
        jsr     divide_16bit            ; compute proportional velocity
        lda     $04                     ; X gets proportional speed
        sta     ent_xvel_sub,x          ; X vel sub
        lda     $05                     ; load quotient high byte
        sta     ent_xvel,x              ; X vel whole
        rts                             ; return

; -----------------------------------------------
; divide_8bit — 8-bit restoring division
; -----------------------------------------------
; Computes ($00 * 256) / $01 using shift-and-subtract.
; Effectively returns the ratio $00/$01 as a 0.8 fixed-point
; fraction, useful when $00 < $01.
;
; parameters:
;   $00 = dividend (8-bit)
;   $01 = divisor (8-bit)
; results:
;   $02 = quotient (= $00 * 256 / $01, 8-bit)
;   $03 = remainder
; -----------------------------------------------
divide_8bit:

        lda     #$00                    ; clear quotient
        sta     $02                     ; and remainder
        sta     $03                     ; clear remainder
        lda     temp_00                 ; if both inputs zero,
        ora     $01                     ; return 0
        bne     entity_divide_loop_setup ; nonzero? proceed to division
        sta     $02                     ; store zero quotient
        rts                             ; return (all zero)

entity_divide_loop_setup:  ldy     #$08 ; 8 iterations (8-bit quotient)
entity_divide_shift_left:  asl     $02  ; shift $03:$00:$02 left
        rol     temp_00                 ; shift dividend left into remainder
        rol     $03                     ; shift remainder left
        sec                             ; try remainder - divisor
        lda     $03                     ; load remainder
        sbc     $01                     ; try subtract divisor
        bcc     entity_divide_loop_counter ; divisor doesn't fit, skip
        sta     $03                     ; update remainder
        inc     $02                     ; set quotient bit
entity_divide_loop_counter:  dey        ; next iteration
        bne     entity_divide_shift_left ; loop 8 times
        rts                             ; return

; -----------------------------------------------
; divide_16bit — 16-bit restoring division
; -----------------------------------------------
; Computes ($01:$00 << 16) / ($03:$02) using shift-and-subtract.
; The dividend is placed in byte position 2 of the 32-bit shift
; chain ($07:$01:$00:$06), so the result is effectively:
;   ($01 * 65536) / ($03:$02)  [when $00 = 0, as typical]
;
; Used by calc_homing_velocity: two successive calls compute
; speed * minor_dist / major_dist for proportional velocity.
;
; parameters:
;   $01:$00 = dividend (16-bit, usually $01=value, $00=0)
;   $03:$02 = divisor (16-bit, 8.8 fixed-point speed)
; results:
;   $05:$04 = quotient (16-bit), $05=high, $04=low
; preserves:
;   X register (saved/restored via $09)
; -----------------------------------------------

divide_16bit:  lda     #$00             ; clear quotient accumulator
        sta     $06                     ; and remainder high byte
        sta     $07                     ; clear remainder high
        lda     temp_00                 ; if all inputs zero,
        ora     $01                     ; return 0
        ora     $02                     ; check if divisor nonzero
        ora     $03                     ; (continued)
        bne     entity_divide_temp_save ; nonzero? proceed to division
        sta     $04                     ; quotient low = 0
        sta     $05                     ; quotient high = 0
        rts                             ; return (all zero)

entity_divide_temp_save:  stx     $09   ; save X (used as temp)
        ldy     #$10                    ; 16 iterations (16-bit quotient)
entity_divide_shift_chain:  asl     $06 ; shift 32-bit chain left:
        rol     temp_00                 ; $07:$01:$00:$06
        rol     $01                     ; (remainder ← dividend ← quotient)
        rol     $07                     ; shift remainder high byte
        sec                             ; try $07:$01 - $03:$02
        lda     $01                     ; (remainder - divisor)
        sbc     $02                     ; low byte - divisor low
        tax                             ; X = low byte of difference
        lda     $07                     ; high byte - divisor high
        sbc     $03                     ; high byte - divisor high
        bcc     entity_divide_loop_2    ; divisor doesn't fit, skip
        stx     $01                     ; update remainder low
        sta     $07                     ; update remainder high
        inc     $06                     ; set quotient bit
entity_divide_loop_2:  dey              ; next iteration
        bne     entity_divide_shift_chain ; loop 16 times
        lda     $06                     ; $04 = quotient low byte
        sta     $04                     ; store quotient low byte
        lda     temp_00                 ; $05 = quotient high byte
        sta     $05                     ; store quotient high byte
        ldx     $09                     ; restore X
        rts                             ; return

