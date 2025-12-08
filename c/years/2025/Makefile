CC := gcc
CFLAGS := -Wall -Wextra -std=c11 -Iutils

SRCS := $(wildcard [0-9][0-9]/part*.c)
UTILS_SRCS := $(wildcard utils/*.c)

BINS := $(foreach f,$(SRCS),$(dir $f)bin/$(basename $(notdir $f)))

.PHONY: all clean

all: $(BINS)

define BUILD_template
$1: $2 $(UTILS_SRCS)
	mkdir -p $$(@D)
	$(CC) $(CFLAGS) -o $$@ $$^
endef

$(foreach s,$(SRCS), \
  $(eval $(call BUILD_template, \
    $(dir $s)bin/$(basename $(notdir $s)),$s)))

cdb:
	@rm -f compile_commands.json
	@compiledb --output compile_commands.json make clean all
	@echo "✓ compile_commands.json regenerated"

clean:
	rm -f $(BINS)

