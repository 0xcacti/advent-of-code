#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int wrap(int x, int n) {
  int r = x % n;
  if (r < 0) r += n;
  return r;
}

int main(void) {
  printf("Beginning Solving Part 1\n");
  FILE *input = fopen("01/input.txt", "r");
  if (input == NULL) {
    perror("Failed to open input file");
    return 1;
  }

  char buf[1024] = {0};

  int pos = 50;
  int z_count = 0;

  printf("Pos: %d\n\n", pos);
  while (fgets(buf, sizeof(buf), input) != NULL) {
    switch (buf[0]) {
    case 'L': {
      int steps = atoi(&buf[1]);
      printf("Turn Left: %d\n", steps);
      pos = wrap(pos - steps, 100);
      printf("New Pos: %d\n", pos);
      break;
    }
    case 'R': {
      int steps = atoi(&buf[1]);
      printf("Turn Right %d\n", steps);
      pos = wrap(pos + steps, 100);
      break;
    }
    }
    if (pos == 0) z_count++;
    printf("\n");
  }

  printf("z count: %d\n", z_count);

  int s = fclose(input);
  if (s != 0) {
    perror("Failed to close input file");
    return 1;
  }

  return 0;
}
