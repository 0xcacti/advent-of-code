#ifndef UTILS_H
#define UTILS_H

#include <stddef.h>

char **read_lines(char *path, size_t *line_count);
void free_lines(char **lines, size_t line_count);

#endif

