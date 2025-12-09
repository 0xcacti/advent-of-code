#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

  free_lines(all_lines, lc);
  return 0;
}
