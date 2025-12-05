#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int lines_max(char **lines, int line_count) {
  int sum = 0;
  for (int i = 0; i < line_count; i++) {
    // if (i == 2) break;
    int max = 0;
    char *line = lines[i];
    for (int j = 0; j < (int)strlen(line); j++) {
      for (int k = j + 1; k < (int)strlen(line); k++) {
        int start_num = line[j] - '0';
        int end_num = line[k] - '0';
        int num = start_num * 10 + end_num;
        if (num > max) {
          max = num;
        }
      }
    }
    printf("Line %d max: %d\n", i + 1, max);
    sum += max;
  }

  return sum;
}

int main(void) {

  bool is_test = false;
  FILE *input = NULL;
  int len = 0;
  if (is_test) {
    input = fopen("03/test-input.txt", "r");
    len = 4;
  } else {
    input = fopen("03/input.txt", "r");
    len = 400;
  }

  char **lines = malloc(len * sizeof(char *));

  char buf[512];

  int line_count = 0;
  while ((fgets(buf, sizeof(buf), input) != NULL)) {
    lines[line_count] = strdup(buf);
    line_count++;
  }

  for (int i = 0; i < line_count; i++) {
    printf("%s", lines[i]);
  }

  int result = lines_max(lines, line_count);
  printf("Result: %d\n", result);

  return 0;
}
