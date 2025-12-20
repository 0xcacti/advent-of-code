#include "utils.h"
#include <stdbool.h>
#include <stdio.h>

int main() {
  bool is_test = true;
  char *path = NULL;
  if (is_test) {
    path = "12/test-input.txt";
  } else {
    path = "12/input.txt";
  }
  size_t lc = 0;
  char **lines = read_lines(path, &lc);

  for (size_t i = 0; i < lc; i++) {
    printf("%s\n", lines[i]);
  }
}
