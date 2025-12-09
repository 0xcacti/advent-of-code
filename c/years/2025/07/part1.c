#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void start_coordinate(char **rows, size_t rc, size_t *r, size_t *c) {
  for (size_t i = 0; i < rc; i++) {
    for (size_t j = 0; j < strlen(rows[i]); j++) {
      if (rows[i][j] == 'S') {
        *r = i;
        *c = j;
        return;
      }
    }
  }
}

int main() {
  bool is_test = false;
  char *path = NULL;
  if (is_test) {
    path = "07/test-input.txt";
  } else {
    path = "07/input.txt";
  }

  size_t lc = 0;
  char **all_lines = read_lines(path, &lc);
  bool **visited = malloc(lc * sizeof(bool *));
  if (!visited) exit(EXIT_FAILURE);
  for (size_t i = 0; i < lc; i++) {
    size_t line_length = strlen(all_lines[i]);
    visited[i] = malloc(line_length * sizeof(bool));
    if (!visited[i]) exit(EXIT_FAILURE);
    for (size_t j = 0; j < line_length; j++) {
      visited[i][j] = false;
    }
  }

  size_t r = 0, c = 0;
  start_coordinate(all_lines, lc, &r, &c);

  visited[r + 1][c] = true;

  size_t split_count = 0;

  for (size_t i = r + 1; i < lc - 1; i++) {
    for (size_t j = 0; j < strlen(all_lines[i]); j++) {
      bool visited_current = visited[i][j];
      if (!visited_current) continue;
      char below = all_lines[i + 1][j];
      if (below == '.') visited[i + 1][j] = true;
      if (below == '^') {
        visited[i + 1][j - 1] = true;
        visited[i + 1][j + 1] = true;
        split_count++;
      }
    }
  }

  printf("Split count: %zu\n", split_count);
  free(visited);
  free_lines(all_lines, lc);

  return 0;
}
