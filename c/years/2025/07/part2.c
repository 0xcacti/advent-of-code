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
  long long **ways = malloc(lc * sizeof(long long *));
  if (!ways) exit(EXIT_FAILURE);
  for (size_t i = 0; i < lc; i++) {
    size_t line_length = strlen(all_lines[i]);
    ways[i] = malloc(line_length * sizeof(long long));
    if (!ways[i]) exit(EXIT_FAILURE);
    for (size_t j = 0; j < line_length; j++) {
      ways[i][j] = 0;
    }
  }

  size_t r = 0, c = 0;
  start_coordinate(all_lines, lc, &r, &c);

  ways[r + 1][c] = 1;

  for (size_t i = r + 1; i < lc - 1; i++) {
    for (size_t j = 0; j < strlen(all_lines[i]); j++) {
      long long count = ways[i][j];
      if (count == 0) continue;
      char below = all_lines[i + 1][j];
      if (below == '.') {
        ways[i + 1][j] += count;
      }
      if (below == '^') {
        ways[i + 1][j - 1] += count;
        ways[i + 1][j + 1] += count;
      }
    }
  }

  long long way_count = 0;
  for (size_t j = 0; j < strlen(all_lines[lc - 1]); j++) {
    way_count += ways[lc - 1][j];
  }

  printf("Way count: %lld\n", way_count);
  free(ways);
  free_lines(all_lines, lc);

  return 0;
}
