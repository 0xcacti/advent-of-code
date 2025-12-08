#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char **read_lines(char *path, size_t *line_count) {
  if (path == NULL || line_count == NULL) return NULL;
  FILE *f = fopen(path, "r");

  size_t capacity = 16;
  size_t count = 0;
  char **lines = malloc(capacity * sizeof(char *));
  if (lines == NULL) {
    exit(EXIT_FAILURE);
  }

  char *buf = NULL;
  size_t buf_size = 0;

  for (;;) {
    ssize_t len = getline(&buf, &buf_size, f);
    if (len == -1) break;
    if (count == capacity) {
      size_t new_cap = capacity * 2;
      char **new_lines = realloc(lines, new_cap * sizeof(char *));
      if (new_lines == NULL) {
        exit(EXIT_FAILURE);
      }
      lines = new_lines;
      capacity = new_cap;
    }
    char *line_copy = malloc((size_t)len + 1);
    if (line_copy == NULL) {
      exit(EXIT_FAILURE);
    }
    memcpy(line_copy, buf, (size_t)len + 1);
    lines[count++] = line_copy;
  }
  free(buf);
  *line_count = count;
  fclose(f);
  return lines;
}

void free_lines(char **lines, size_t line_count) {
  if (lines == NULL) return;
  for (size_t i = 0; i < line_count; i++) {
    free(lines[i]);
  }
  free(lines);
}
