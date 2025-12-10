#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  double x;
  double y;
  double z;
} Box;

Box *parse_boxes(char **lines, size_t line_count) {
  Box *boxes = malloc(line_count * sizeof(Box));
  if (!boxes) exit(EXIT_FAILURE);
  size_t count = 0;
  for (size_t i = 0; i < line_count; i++) {
    double x, y, z;
    if (sscanf(lines[i], "%lf,%lf,%lf", &x, &y, &z) == 3) {
      boxes[count].x = x;
      boxes[count].y = y;
      boxes[count].z = z;
      count++;
    }
  }
  return boxes;
}

int main() {
  bool is_test = true;
  char *path = NULL;
  int num_connections = 0;

  if (is_test) {
    path = "08/test-input.txt";
    num_connections = 10;
  } else {
    path = "08/input.txt";
    num_connections = 1000;
  }

  size_t lc = 0;
  char **all_lines = read_lines(path, &lc);

  Box *boxes = parse_boxes(all_lines, lc);
  for (size_t i = 0; i < lc; i++) {
    printf("Box %zu: (%.2f, %.2f, %.2f)\n", i + 1, boxes[i].x, boxes[i].y, boxes[i].z);
  }

  free_lines(all_lines, lc);
  return 0;
}
