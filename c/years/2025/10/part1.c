#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  long long first;
  long long second;
} pair_t;

typedef struct {
  int start_state;
  int target_state;
  size_t indicator_len;
  int *buttons;
  int button_count;
} machine_t;

void strip_newlines(char **lines, size_t line_count) {
  for (size_t i = 0; i < line_count; i++) {
    char *line = lines[i];
    size_t len = 0;
    while (line[len] != '\0') {
      len++;
    }
    if (len > 0 && line[len - 1] == '\n') {
      line[len - 1] = '\0';
    }
  }
}

int parse_button(char *button_str, int num_lights) {
  int button = 0;
  int current = 0;
  int len = strlen(button_str);
  bool in_number = false;
  for (size_t i = 0; i < (size_t)len; i++) {
    switch (button_str[i]) {
    case '(':
    case ' ':
      continue;
    case ',':
    case ')':
      if (in_number) {
        if (0 <= current && current < num_lights) button |= (1 << current);
        current = 0;
        in_number = false;
      }
      break;
    default: {
      in_number = true;
      int digit = button_str[i] - '0';
      current = current * 10 + digit;
    }
    }
  }
  return button;
}

machine_t *parse_machines(char **lines, size_t line_count) {
  machine_t *machines = malloc(sizeof(machine_t) * line_count);
  // parse diagram
  for (size_t i = 0; i < line_count; i++) {
    machine_t machine = {0};
    char *line_copy = strdup(lines[i]);
    char *token = strtok(line_copy, " ");
    machine.indicator_len = strlen(token);
    int n = 0;
    for (size_t j = 0; j < strlen(token); j++) {
      switch (token[j]) {
      case '#':
        machine.target_state |= (1 << n);
        n++;
        break;
      case '.':
        n++;
        break;
      default:
        continue;
      }
    }
    free(line_copy);
    machines[i] = machine;
  }

  // parse buttons
  for (size_t i = 0; i < line_count; i++) {
    char *line_copy = strdup(lines[i]);
    char *line_copy_ptr = line_copy + machines[i].indicator_len + 1;
    printf("%s\n", line_copy);
    char *token = strtok(line_copy_ptr, " ");
    int button_count = 0;
    int button_capacity = 8;
    int button = parse_button(token, machines[i].indicator_len - 2);
    machines[i].buttons = malloc(sizeof(int) * 8);
    while (token != NULL) {
      if (button_count >= button_capacity) {
        button_capacity *= 2;
        machines[i].buttons = realloc(machines[i].buttons, sizeof(int) * button_capacity);
      }
      if (token[0] == '(') {
        button = parse_button(token, machines[i].indicator_len - 2);
        machines[i].buttons[button_count] = button;
        button_count++;
      }
      token = strtok(NULL, " ");
    }
    free(line_copy);
    machines[i].button_count = button_count;
  }

  return machines;
}

int min_presses(machine_t *m) {
  int nlights = (int)m->indicator_len - 2;
  int max_states = 1 << nlights;
  int *dist = malloc(sizeof(int) * max_states);
  for (int i = 0; i < max_states; i++) dist[i] = -1;
  int *queue = malloc(sizeof(int) * max_states);
  int head = 0;
  int tail = 0;
  int start = 0;
  int target = m->target_state;

  dist[start] = 0;
  queue[tail++] = start;
  while (head < tail) {
    int s = queue[head++];
    int d = dist[s];
    for (int i = 0; i < m->button_count; i++) {
      int ns = s ^ m->buttons[i];
      if (dist[ns] != -1) continue;
      dist[ns] = d + 1;
      if (ns == target) {
        int ans = dist[ns];
        free(dist);
        free(queue);
        return ans;
      }
      queue[tail++] = ns;
    }
  }

  free(dist);
  free(queue);
  return -1;
}

int main() {
  bool is_test = false;
  char *path = NULL;

  if (is_test) {
    path = "10/test-input.txt";
  } else {
    path = "10/input.txt";
  }

  size_t lc = 0;
  char **lines = read_lines(path, &lc);
  strip_newlines(lines, lc);
  machine_t *machines = parse_machines(lines, lc);

  int total = 0;
  for (size_t i = 0; i < lc; i++) {
    printf("machine %zu\n", i);
    printf("\tindicator: %s\n", lines[i]);
    printf("\tbuttons: ");
    for (size_t j = 0; j < (size_t)machines[i].button_count; j++) {
      printf("(%d) ", machines[i].buttons[j]);
    }
    printf("\n");
    int presses = min_presses(&machines[i]);
    total += presses;
  }

  printf("total presses: %d\n", total);

  for (size_t i = 0; i < lc; i++) {
    free(machines[i].buttons);
    free(lines[i]);
  }
  free(lines);
}
