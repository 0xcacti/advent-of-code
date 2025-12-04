CC := gcc
CFLAGS := -Wall -Wextra -std=c11

SRCS := $(wildcard [0-9][0-9]/part*.c)
BINS := $(foreach f,$(SRCS),$(dir $f)bin/$(basename $(notdir $f)))

.PHONY: all clean

all: $(BINS)

define BUILD_template
$1: $2
	mkdir -p $$(@D)
	$(CC) $(CFLAGS) -o $$@ $$<
endef

$(foreach s,$(SRCS), \
  $(eval $(call BUILD_template, \
    $(dir $s)bin/$(basename $(notdir $s)),$s)))

clean:
	rm -f $(BINS)

