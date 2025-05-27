CC := gcc
CFLAGS := -Wall -Wextra -std=c11

DIRS   := $(wildcard [0-9][0-9])
SRCS   := $(foreach d,$(DIRS),$(wildcard $(d)/part*.c))
EXECS := $(SRCS:.c=)

.PHONY: all clean

all: $(EXECS)

%: %.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(EXECS)


