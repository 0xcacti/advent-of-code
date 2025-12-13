#include "utils.h"

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  double x;
  double y;
  double z;
  bool connected;
  int id;
} Box;

double distance(Box a, Box b) {
  return sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2) + pow(b.z - a.z, 2));
}

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
      boxes[count].connected = false;
      count++;
    }
  }
  return boxes;
}

// TODO: how to handle already connected, is that a step?
void find_shortest_distance(Box *boxes, size_t box_count, int *i, int *j) {
  double min_distance = INFINITY;
  for (size_t m = 0; m < box_count; m++) {
    for (size_t n = m + 1; n < box_count; n++) {
      if (boxes[m].connected && boxes[n].connected) continue;
      double dist = distance(boxes[m], boxes[n]);
      if (dist < min_distance) {
        min_distance = dist;
        *i = m;
        *j = n;
      }
    }
  }
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

  for (int conn = 0; conn < num_connections; conn++) {
    int i, j;
    find_shortest_distance(boxes, lc, &i, &j);
    boxes[i].connected = true;
    boxes[j].connected = true;
    if (boxes[i].id != 0) {
      boxes[j].id = boxes[i].id;
    } else if (boxes[j].id != 0) {
      boxes[i].id = boxes[j].id;
    } else {
      boxes[i].id = conn + 1;
      boxes[j].id = conn + 1;
    }
    printf("Connection %d: Box(%lf, %lf, %lf) <-> Box(%lf, %lf, %lf)\n\n", conn + 1, boxes[i].x,
           boxes[i].y, boxes[i].z, boxes[j].x, boxes[j].y, boxes[j].z);
  }

  free_lines(all_lines, lc);
  return 0;
}
