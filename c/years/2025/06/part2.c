#include "utils.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char ***split_lines(char **lines, size_t line_count, size_t *cols) {
  if (line_count == 0) {
    exit(EXIT_FAILURE);
  }

  size_t columns = strlen(lines[0]);

  char ***grid = malloc(line_count * sizeof(char **));
  if (grid == NULL) exit(EXIT_FAILURE);
  for (size_t i = 0; i < line_count; i++) {
    char **row = malloc(columns * sizeof(char *));
    if (row == NULL) exit(EXIT_FAILURE);

    char *line = lines[i];
    for (size_t j = 0; j < columns; j++) {
      char *ch = malloc(2 * sizeof(char));
      if (ch == NULL) exit(EXIT_FAILURE);
      ch[0] = line[j];
      ch[1] = '\0';
      row[j] = ch;
    }

    grid[i] = row;
  }

  *cols = columns;

  return grid;
}

typedef enum { MULT, ADD } Operation;

Operation get_operation(char *op_str) {
  if (strcmp(op_str, "*") == 0) {
    return MULT;
  } else if (strcmp(op_str, "+") == 0) {
    return ADD;
  } else {
    fprintf(stderr, "Unknown operation: %s\n", op_str);
    exit(EXIT_FAILURE);
  }
}

int main() {
  bool is_test = false;
  char *path = NULL;
  if (is_test) {
    path = "06/test-input.txt";
  } else {
    path = "06/input.txt";
  }

  size_t rows = 0;
  char **all_lines = read_lines(path, &rows);

  size_t cols = 0;
  char ***grid = split_lines(all_lines, rows, &cols);

  long long components[5000] = {0};
  size_t count = 0;
  long long sub_components[5000] = {0};
  size_t sub_count = 0;
  size_t last_break = 0;
  long long sum = 0;

  for (ssize_t c = (ssize_t)cols - 1; c >= 0; c--) {
    char *part = calloc(rows, sizeof(char));
    bool has_op = false;
    Operation op = ADD;

    for (size_t r = 0; r < rows; r++) {
      char *cell = grid[r][c];
      if (isdigit(cell[0])) {
        strcat(part, cell);
      } else if (strcmp(cell, "+") == 0) {
        has_op = true;
        op = ADD;
      } else if (strcmp(cell, "*") == 0) {
        has_op = true;
        op = MULT;
      }
    }

    if (part[0] != '\0') {
      components[count++] = atoll(part);
      printf("Part at col %zu: %s\n", c + 1, part);
    }

    if (has_op) {
      long long acc = (op == ADD) ? 0 : 1;
      for (size_t i = last_break; i < count; i++) {
        if (op == ADD) {
          acc += components[i];
        } else if (op == MULT && components[i] != 0) {
          acc *= components[i];
        }
      }
      sub_components[sub_count++] = acc;
      printf("Op %c over components[%zu..%zu) = %lld\n", op == ADD ? '+' : '*', last_break, count,
             acc);

      last_break = count;
    }
    free(part);
  }

  for (size_t i = 0; i < sub_count; i++) {
    sum += sub_components[i];
  }

  printf("Sum: %lld\n", sum);

  return 0;
}
