# =============================================================================
# Mega Man 3 (U) — ca65 Disassembly Build System
# =============================================================================

CA65    = ca65 -I .
LD65    = ld65
CFG     = cfg/nes.cfg
ROM_OUT = build/mm3_built.nes
ROM_REF = mm3.nes

# Source files
HEADER_SRC = src/header.asm
CHR_SRC    = src/chr.asm

# Swappable bank sources ($00-$19)
BANK_SRCS = \
	src/bank00_enemy_data.asm \
	src/bank01_stage_magnet.asm \
	src/bank02_stage_gemini.asm \
	src/bank03_stage_hard.asm \
	src/bank04_doc_robot_a.asm \
	src/bank05_doc_robot_b.asm \
	src/bank06_robot_masters_a.asm \
	src/bank07_robot_masters_b.asm \
	src/bank08_stage_doc_needle.asm \
	src/bank09_per_frame.asm \
	src/bank0A_damage_tables.asm \
	src/bank0B_intro.asm \
	src/bank0C_game_over.asm \
	src/bank0D_oam_sprites.asm \
	src/bank0E_anim_frames.asm \
	src/bank0F_entity_spawn.asm \
	src/bank10_stage_setup.asm \
	src/bank11_ending_data.asm \
	src/bank12_fortress_bosses.asm \
	src/bank13_ending_data2.asm \
	src/bank14_sprite_offsets_alt.asm \
	src/bank15_weapon_anim.asm \
	src/bank16_sound_driver.asm \
	src/bank17_sound_data.asm \
	src/bank18_stage_select.asm \
	src/bank19_sprite_offsets.asm

# Bank pair sources
PAIR_SRCS = \
	src/bank1A_1B_oam_sequences.asm \
	src/bank1C_1D_entity_ai.asm

# Fixed bank source ($1E/$1F, $C000-$FFFF)
FIXED_SRCS = \
	src/fixed/fixed_bank.asm

ALL_SRCS = $(HEADER_SRC) $(BANK_SRCS) $(PAIR_SRCS) $(FIXED_SRCS) $(CHR_SRC)
ALL_OBJS = $(ALL_SRCS:%.asm=build/%.o)

.PHONY: all verify nsfe clean

all: verify

nsfe: build/mm3.nsfe

$(ROM_OUT): $(ALL_OBJS) $(CFG)
	@mkdir -p $(dir $@)
	$(LD65) -C $(CFG) -o $@ $(ALL_OBJS)

# Pattern rule: assemble .asm -> .o
build/%.o: %.asm
	@mkdir -p $(dir $@)
	$(CA65) -o $@ $<

verify: $(ROM_OUT)
	@cmp $(ROM_OUT) $(ROM_REF) && echo "BUILD VERIFIED: byte-perfect match!" || (echo "BUILD FAILED: ROM mismatch"; exit 1)

build/mm3.nsfe: $(ROM_OUT) tools/nsf_to_nsfe.py
	python3 tools/nsf_to_nsfe.py

clean:
	rm -rf build/src build/mm3_built.nes build/mm3.nsfe
