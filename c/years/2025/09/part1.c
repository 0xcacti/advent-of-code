#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  long long first;
  long long second;
} pair_t;

int main() {
  bool is_test = true;
  char *path = NULL;

  if (is_test) {
    path = "09/test-input.txt";
  } else {
    path = "09/input.txt";
  }

  size_t lc = 0;
  char **lines = read_lines(path, &lc);

  pair_t *ps = malloc(sizeof(pair_t) * lc);
  for (size_t i = 0; i < lc; i++) {
    sscanf(lines[i], "%lld,%lld", &ps[i].first, &ps[i].second);
  }

  long long max_area = 0;
  for (size_t i = 0; i < lc; i++) {
    long long area = 0;
    for (size_t j = i + 1; j < lc; j++) {
      int x1 = ps[i].first;
      int y1 = ps[i].second;
      int x2 = ps[j].first;
      int y2 = ps[j].second;
      if (x1 == x2 || y1 == y2) {
        continue;
      }
      area = (llabs(x2 - x1) + 1) * (llabs(y2 - y1) + 1);
      if (area > max_area) {
        max_area = area;
      }
    }
  }
  printf("Max area: %lld\n", max_area);

  free_lines(lines, lc);
}
