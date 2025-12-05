#include <stdbool.h>
#include <stdio.h>

void print_grid(char *start, int row_count, int col_count) {
  for (int r = 0; r < row_count; r++) {
    for (int c = 0; c < col_count; c++) {
      printf("%c", *(start + r * col_count + c));
    }
    printf("\n");
  }
}

int main(void) {
  bool is_test = false;

  FILE *file = NULL;
  int row_count = 0;
  int col_count = 0;
  if (is_test) {
    file = fopen("04/test-input.txt", "r");
    row_count = 10;
    col_count = 10;
  } else {
    file = fopen("04/input.txt", "r");
    col_count = 138;
    row_count = 138;
  }

  if (file == NULL) {
    printf("Error opening file\n");
    return 1;
  }

  char grid_start[row_count][col_count];
  char grid_end[row_count][col_count];

  char buf[512];
  int row = 0;
  while ((fgets(buf, sizeof(buf), file)) != NULL) {
    for (int col = 0; col < col_count; col++) {
      grid_start[row][col] = buf[col];
      grid_end[row][col] = buf[col];
    }
    row++;
  }

  fclose(file);

  printf("Initial grid:\n");
  print_grid(&grid_start[0][0], row_count, col_count);

  for (int r = 0; r < row_count; r++) {
    for (int c = 0; c < col_count; c++) {
      if (grid_start[r][c] == '@') {
        int adj_occupancy_count = 0;
        for (int dr = -1; dr <= 1; dr++) {
          for (int dc = -1; dc <= 1; dc++) {
            if (dr == 0 && dc == 0) {
              continue;
            }
            int nr = r + dr;
            int nc = c + dc;
            if (nr >= 0 && nr < row_count && nc >= 0 && nc < col_count) {
              if (grid_start[nr][nc] == '@') {
                adj_occupancy_count++;
              }
            }
          }
        }
        if (adj_occupancy_count < 4) {
          grid_end[r][c] = 'x';
        }
      }
    }
  }

  int count = 0;
  for (int r = 0; r < row_count; r++) {
    for (int c = 0; c < col_count; c++) {
      if (grid_end[r][c] == 'x') {
        count++;
      }
    }
  }

  printf("\nTotal 'x' count: %d\n", count);

  return 0;
}
