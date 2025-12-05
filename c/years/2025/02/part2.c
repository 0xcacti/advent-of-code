#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

typedef struct {
  long long start;
  long long end;
  char *sstart;
  char *send;
} range_t;

bool is_invalid_number(long long num) {
  char str[64];
  int len = sprintf(str, "%lld", num);

  int start_idx = 0;
  if (str[0] == '-') {
    start_idx = 1;
  }

  int digit_len = len - start_idx;
  if (digit_len <= 0) return false;

  char *p = str + start_idx;
  for (int b = 1; b <= digit_len / 2; b++) {
    if (digit_len % b != 0) continue;

    int invalid = true;
    for (int i = 0; i < digit_len - b; i++) {
      if (p[i] != p[i + b]) {
        invalid = false;
        break;
      }
    }

    if (invalid) {
      return true;
    }
  }

  return false;
}

long long find_invalid_ranges(range_t *ranges, size_t count) {
  long long invalid_sum = 0;
  for (size_t i = 0; i < count; i++) {
    for (long long j = ranges[i].start; j <= ranges[i].end; j++) {
      if (is_invalid_number(j)) {
        invalid_sum += j;
      }
    }
  }
  return invalid_sum;
}

int main(void) {
  FILE *file = fopen("02/input.txt", "r");
  if (file == NULL) {
    perror("Failed to open file");
    return 1;
  }

  struct stat st;
  if (stat("02/input.txt", &st) != 0) {
    perror("Failed to stat file");
    fclose(file);
    return 1;
  }
  size_t size = (size_t)st.st_size;

  char *string = malloc(size + 1);
  if (string == NULL) {
    perror("Failed to allocate buffer");
    fclose(file);
    return 1;
  }

  size_t nread = fread(string, 1, size, file);
  if (nread != size) {
    perror("Failed to read file");
    free(string);
    fclose(file);
    return 1;
  }
  string[size] = '\0';

  if (fclose(file) != 0) {
    perror("Failed to close file");
    free(string);
    return 1;
  }

  size_t count = 0;
  for (size_t i = 0; string[i] != '\0'; i++) {
    if (string[i] == ',') {
      count++;
    }
  }

  range_t *parts = malloc((count + 1) * sizeof(range_t));
  if (parts == NULL) {
    perror("Failed to allocate parts");
    free(string);
    return 1;
  }

  size_t index = 0;
  char *saveptr_outer = NULL;
  char *tok = strtok_r(string, ",", &saveptr_outer);
  while (tok != NULL) {
    char *dash = strchr(tok, '-');
    if (dash == NULL) {
      fprintf(stderr, "Invalid range format: %s\n", tok);
      free(parts);
      free(string);
      return 1;
    }

    *dash = '\0';
    char *tok1 = tok;
    char *tok2 = dash + 1;

    long long start = strtoll(tok1, NULL, 10);
    long long end = strtoll(tok2, NULL, 10);

    range_t r = {
        .start = start,
        .end = end,
        .sstart = tok1,
        .send = tok2,
    };
    parts[index++] = r;
    tok = strtok_r(NULL, ",", &saveptr_outer);
  }

  for (size_t i = 0; i < index; i++) {
    printf("Range %zu: %lld-%lld\n", i, parts[i].start, parts[i].end);
  }

  long long sum = find_invalid_ranges(parts, index);
  printf("Sum of invalid ranges: %lld\n", sum);

  free(parts);
  free(string);
  return 0;
}
