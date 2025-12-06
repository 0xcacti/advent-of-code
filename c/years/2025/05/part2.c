#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  long long start;
  long long end;
} range_t;

int comp(const void *elem1, const void *elem2) {
  range_t e1 = *((range_t *)elem1);
  range_t e2 = *((range_t *)elem2);
  if (e1.start < e2.start) {
    return -1;
  } else if (e1.start > e2.start) {
    return 1;
  } else {
    return 0;
  }
}

long long find_fresh_count(range_t *ranges, int range_count) {
  for (int i = 0; i < range_count; i++) {
    printf("Range %d: %lld - %lld\n", i, ranges[i].start, ranges[i].end);
  }

  qsort(ranges, range_count, sizeof(range_t), comp);
  for (int i = 0; i < range_count; i++) {
    printf("Range %d: %lld - %lld\n", i, ranges[i].start, ranges[i].end);
  }

  range_t merged_ranges[range_count];
  merged_ranges[0] = ranges[0];
  int correct_ranges = 0; // Last idx of correct_range

  for (int i = 1; i < range_count; i++) {
    range_t current_range = ranges[i];
    range_t last_merged_range = merged_ranges[correct_ranges];

    if (current_range.start <= last_merged_range.end) {
      if (current_range.end > last_merged_range.end) {
        merged_ranges[correct_ranges].end = current_range.end;
      } else {
        merged_ranges[correct_ranges] = last_merged_range;
      }
    } else {
      correct_ranges++;
      merged_ranges[correct_ranges] = current_range;
    }
  }

  long long fresh_count = 0;
  for (int i = 0; i <= correct_ranges; i++) {
    printf("Merged Range %d: %lld - %lld\n", i, merged_ranges[i].start, merged_ranges[i].end);
    fresh_count += (merged_ranges[i].end - merged_ranges[i].start + 1);
  }

  return fresh_count;
}

int main() {
  bool is_test = false;

  FILE *file = NULL;
  int sec_one_count = 0;
  if (is_test) {
    file = fopen("05/test-input.txt", "r");
    sec_one_count = 4;
  } else {
    file = fopen("05/input.txt", "r");
    sec_one_count = 182;
  }

  char buf[512];

  range_t ranges[sec_one_count];

  bool newline_found = false;
  int range_index = 0;
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
      break;
    }
  }

  long long count = find_fresh_count(ranges, sec_one_count);
  printf("Sum of fresh points: %lld\n", count);

  fclose(file);

  return 0;
}

// 9,223,372,036,854,775,807
