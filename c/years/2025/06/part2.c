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

  char ***grid = malloc(line_count * sizeof(char *));
  if (grid == NULL) exit(EXIT_FAILURE);
  for (size_t i = 0; i < line_count; i++) {
    char **row = malloc(columns * sizeof(char *));
    if (row == NULL) exit(EXIT_FAILURE);

    char *line = lines[i];
    // char *tok = strtok(line, " \t\r\n");
    // int j = 0;

    // while (tok != NULL && j < elem_count) {
    //   row[j] = tok;
    //   j++;
    //   tok = strtok(NULL, " \t\r\n");
    // }
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

  long long sum = 0;

  long long components[500] = {0};
  size_t count = 0;
  long long sub_components[500] = {0};
  size_t sub_count = 0;

  for (size_t c = cols - 1; c >= 0; c--) {

    long long part = 0;
    for (size_t r = 0; r < rows; r++) {
      char *cell = grid[r][c];
      if (isdigit(cell[0])) {

      } else if (strcmp(cell, "+") == 0) {
      } else if (strcmp(cell, "*") == 0) {
      }
    }
  }

  printf("Sum: %lld\n", sum);

  return 0;
}
