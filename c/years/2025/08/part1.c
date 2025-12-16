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

typedef struct {
  int box1;
  int box2;
} Connection;

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

void find_shortest_distance(Box *boxes, size_t box_count, int *i, int *j, Connection *attempted,
                            int attempt_count) {
  double min_distance = INFINITY;
  for (size_t m = 0; m < box_count; m++) {
    for (size_t n = m + 1; n < box_count; n++) {
      // Check if this pair was already attempted
      bool already_attempted = false;
      for (int a = 0; a < attempt_count; a++) {
        if ((attempted[a].box1 == (int)m && attempted[a].box2 == (int)n) ||
            (attempted[a].box1 == (int)n && attempted[a].box2 == (int)m)) {
          already_attempted = true;
          break;
        }
      }
      if (already_attempted) continue;

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
  bool is_test = false;
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

  Connection *attempted = malloc(num_connections * sizeof(Connection));
  if (!attempted) exit(EXIT_FAILURE);

  for (int conn = 0; conn < num_connections; conn++) {
    int i, j;
    find_shortest_distance(boxes, lc, &i, &j, attempted, conn);

    // Record this attempt
    attempted[conn].box1 = i;
    attempted[conn].box2 = j;

    if (boxes[i].id != 0 && boxes[i].id == boxes[j].id) {
      printf("Connection %d: Box(%lf, %lf, %lf) <-> Box(%lf, %lf, %lf) [already connected]\n\n",
             conn + 1, boxes[i].x, boxes[i].y, boxes[i].z, boxes[j].x, boxes[j].y, boxes[j].z);
    } else {
      boxes[i].connected = true;
      boxes[j].connected = true;
      int id_i = boxes[i].id;
      int id_j = boxes[j].id;
      if (id_i != 0 && id_j != 0 && id_i != id_j) {
        for (size_t k = 0; k < lc; k++) {
          if (boxes[k].id == id_j) {
            boxes[k].id = id_i;
          }
        }
      } else if (id_i != 0) {
        boxes[j].id = id_i;
      } else if (id_j != 0) {
        boxes[i].id = id_j;
      } else {
        boxes[i].id = conn + 1;
        boxes[j].id = conn + 1;
      }
      printf("Connection %d: Box(%lf, %lf, %lf) <-> Box(%lf, %lf, %lf)\n\n", conn + 1, boxes[i].x,
             boxes[i].y, boxes[i].z, boxes[j].x, boxes[j].y, boxes[j].z);
    }
  }

  // Find max ID used
  int max_id = 0;
  for (size_t i = 0; i < lc; i++) {
    if (boxes[i].id > max_id) {
      max_id = boxes[i].id;
    }
  }

  int *circuit_sizes = calloc(max_id + 1, sizeof(int));
  if (!circuit_sizes) exit(EXIT_FAILURE);
  for (size_t i = 0; i < lc; i++) {
    if (boxes[i].id != 0) {
      circuit_sizes[boxes[i].id]++;
    }
  }

  int max1 = 0, max2 = 0, max3 = 0;
  for (int i = 1; i <= max_id; i++) {
    if (circuit_sizes[i] > max1) {
      max3 = max2;
      max2 = max1;
      max1 = circuit_sizes[i];
    } else if (circuit_sizes[i] > max2) {
      max3 = max2;
      max2 = circuit_sizes[i];
    } else if (circuit_sizes[i] > max3) {
      max3 = circuit_sizes[i];
    }
  }
  printf("Top 3 largest circuits: %d, %d, %d\n", max1, max2, max3);
  printf("Product of their sizes: %d\n", max1 * max2 * max3);

  free_lines(all_lines, lc);
  return 0;
}
