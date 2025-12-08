#include "utils.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int get_word_count(char *line) {
  int count = 0;
  int in_word = 0;
  for (int i = 0; line[i] != '\0'; i++) {
    if (isspace((unsigned char)line[i])) {
      in_word = 0;
    } else if (in_word == 0) {
      in_word = 1;
      count++;
    }
  }

  return count;
}

char ***split_lines(char **lines, size_t line_count, size_t *cols) {
  if (line_count == 0) {
    exit(EXIT_FAILURE);
  }

  char ***arrays = malloc(sizeof(char *) * line_count);
  if (arrays == NULL) {
    exit(EXIT_FAILURE);
  }

  int elem_count = get_word_count(lines[0]);
  printf("Element count: %d\n", elem_count);

  char ***grid = malloc(line_count * sizeof(char *));
  if (grid == NULL) exit(EXIT_FAILURE);
  for (size_t i = 0; i < line_count; i++) {
    char **row = malloc(elem_count * sizeof(char *));
    if (row == NULL) exit(EXIT_FAILURE);

    char *line = lines[i];
    char *tok = strtok(line, " \t\r\n");
    int j = 0;

    while (tok != NULL && j < elem_count) {
      row[j] = tok;
      j++;
      tok = strtok(NULL, " \t\r\n");
    }

    grid[i] = row;
  }

  *cols = elem_count;

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
  for (size_t c = 0; c < cols; c++) {

    long long part = 0;
    Operation op = get_operation(grid[rows - 1][c]);
    if (op == MULT) part = 1;
    for (size_t r = 0; r < rows - 1; r++) {
      switch (op) {
      case MULT:
        part *= atoll(grid[r][c]);
        break;
      case ADD:
        part += atoll(grid[r][c]);
        break;
      }
    }
    printf("part: %lld\n", part);
    printf("Operation: %s\n", grid[rows - 1][0]);

    sum += part;
    printf("\n");
  }

  printf("Sum: %lld\n", sum);

  return 0;
}
