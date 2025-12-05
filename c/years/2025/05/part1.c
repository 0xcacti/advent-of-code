#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  long long start;
  long long end;
} range_t;

int find_fresh_count(range_t *ranges, int range_count, long long *points, int point_count) {
  for (int i = 0; i < range_count; i++) {
    printf("Range %d: %lld - %lld\n", i, ranges[i].start, ranges[i].end);
  }

  printf("\n");
  // for (int i = 0; i < point_count; i++) {
  //   printf("Point %d: %lld\n", i, points[i]);
  // }

  int fresh_count = 0;
  for (int i = 0; i < point_count; i++) {
    long long point = points[i];
    bool is_fresh = false;
    for (int j = 0; j < range_count; j++) {
      range_t range = ranges[j];
      if (point >= range.start && point <= range.end) {
        is_fresh = true;
        break;
      }
    }
    if (is_fresh) {
      fresh_count++;
    }
  }

  return fresh_count;
}

int main() {
  bool is_test = false;

  FILE *file = NULL;
  int sec_one_count = 0;
  int sec_two_count = 0;
  if (is_test) {
    file = fopen("05/test-input.txt", "r");
    sec_one_count = 4;
    sec_two_count = 6;
  } else {
    file = fopen("05/input.txt", "r");
    sec_one_count = 182;
    sec_two_count = 1000;
  }

  char buf[512];

  range_t ranges[sec_one_count];
  long long points[sec_two_count];

  bool newline_found = false;
  int range_index = 0;
  int point_index = 0;
  while ((fgets(buf, sizeof(buf), file)) != NULL) {
    if (buf[0] == '\n') {
      newline_found = true;
      continue;
    }
    int buf_len = strlen(buf);
    buf[buf_len - 1] = '\0';

    if (!newline_found) {
      char *tok = strtok(buf, "-");
      range_t range;
      range.start = atoll(tok);
      tok = strtok(NULL, "-");
      range.end = atoll(tok);
      ranges[range_index++] = range;
    } else {
      long long point = atoll(buf);
      points[point_index++] = point;
    }
  }

  int count = find_fresh_count(ranges, sec_one_count, points, sec_two_count);
  printf("Sum of fresh points: %d\n", count);

  fclose(file);

  return 0;
}
