#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

long long best_12_for_line(const char *line) {
  int L = 12;
  int n = (int)strlen(line);

  int start = 0;
  char picked[13] = {0};
  picked[L] = '\0';

  for (int pos = 0; pos < L; pos++) {
    int remaining = L - pos;
    int last_start = n - remaining;
    char best_digit = '0';
    int best_idx = start;
    for (int i = start; i <= last_start; i++) {
      if (line[i] > best_digit) {
        best_digit = line[i];
        best_idx = i;
      }
    }
    picked[pos] = best_digit;
    start = best_idx + 1;
  }
  long long result = 0;
  for (int i = 0; i < L; i++) {
    result = result * 10 + (picked[i] - '0');
  }
  return result;
}

long long lines_max(char **lines, int line_count) {
  long long sum = 0;
  for (int i = 0; i < line_count; i++) {
    char *line = lines[i];
    sum += best_12_for_line(line);
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

    size_t L = strlen(lines[line_count]);
    if (L > 0 && (lines[line_count][L - 1] == '\n' || lines[line_count][L - 1] == '\r')) {
      lines[line_count][L - 1] = '\0';
      L--;
    }
    if (L > 0 && (lines[line_count][L - 1] == '\r')) {
      lines[line_count][L - 1] = '\0';
    }

    line_count++;
  }

  for (int i = 0; i < line_count; i++) {
    printf("%s\n", lines[i]);
  }

  long long result = lines_max(lines, line_count);
  printf("Result: %lld\n", result);

  return 0;
}
