#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char **read_lines(FILE *f, size_t *line_count) {
  char **lines = NULL;
  char buf[4096];
  *line_count = 0;

  while (fgets(buf, sizeof(buf), f) != NULL) {
    buf[strcspn(buf, "\n")] = 0; // Remove newline character
    lines = realloc(lines, sizeof(char *) * (*line_count + 1));
    lines[*line_count] = malloc(sizeof(char) * (strlen(buf) + 1));
    strcpy(lines[*line_count], buf);
    (*line_count)++;
  }

  return lines;
}

int **split_lines(char **lines, size_t line_count, size_t *array_count) {
  int **arrays = malloc(sizeof(int *) * line_count);
  *array_count = line_count;

  for (size_t i = 0; i < line_count; i++) {
    int len = 0;
    int count = 8;

    char *tok = strtok(lines[i], " ");
    size_t num_count = 0;
  }

  return arrays;
}
int main() {
  bool is_test = true;
  FILE *file = NULL;
  if (is_test) {
    file = fopen("06/test-input.txt", "r");
  } else {
    file = fopen("06/input.txt", "r");
  }

  size_t line_count = 0;
  char **all_lines = read_lines(file, &line_count);

  for (size_t i = 0; i < line_count; i++) {
    printf("Line %zu: %s\n", i + 1, all_lines[i]);
  }

  fclose(file);

  return 0;
}
