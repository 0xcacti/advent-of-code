#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  char *source;
  char **destinations;
  int count;
} conn_t;

int find_conn(conn_t *conns, size_t lc, const char *name) {
  for (size_t i = 0; i < lc; i++) {
    if (strcmp(conns[i].source, name) == 0) return i;
  }
  return -1;
}

bool in_path(char **path, int depth, const char *name) {
  for (int i = 0; i < depth; i++) {
    if (strcmp(path[i], name) == 0) return true;
  }
  return false;
}

conn_t *parse_connections(char **lines, size_t lc) {
  conn_t *connections = malloc(sizeof(conn_t) * lc);
  for (size_t i = 0; i < lc; i++) {
    char *line = lines[i];
    conn_t conn = {0};

    int j = 0;
    char source[3] = {0};
    for (j = 0; j < 3; j++) {
      source[j] = line[j];
    }
    conn.source = malloc(3 + 1);
    snprintf(conn.source, 4, "%s", source);
    j += 2;
    while (line[j] != '\n' && line[j] != '\0') {
      char dest[3] = {0};
      for (int k = 0; k < 3; k++) {
        dest[k] = line[j + k];
      }
      conn.destinations = realloc(conn.destinations, sizeof(char *) * (conn.count + 1));
      conn.destinations[conn.count] = malloc(3 + 1);
      snprintf(conn.destinations[conn.count], 4, "%s", dest);
      conn.count += 1;
      j += 4;
    }
    connections[i] = conn;
  }

  return connections;
}

int dfs_paths(conn_t *conns, size_t lc, char *cur, char *target, char **path, int depth) {
  path[depth] = cur;

  if (strcmp(cur, target) == 0) {
    return 1;
  }

  int idx = find_conn(conns, lc, cur);
  if (idx < 0) return 0;

  int total = 0;
  conn_t *c = &conns[idx];
  for (int i = 0; i < c->count; i++) {
    char *nxt = c->destinations[i];
    if (in_path(path, depth, nxt)) continue;
    total += dfs_paths(conns, lc, nxt, target, path, depth + 1);
  }
  return total;
}

int main() {
  bool is_test = true;
  char *path = NULL;

  if (is_test) {
    path = "11/test-input.txt";
  } else {
    path = "11/input.txt";
  }

  size_t lc = 0;
  char **lines = read_lines(path, &lc);
  conn_t *conns = parse_connections(lines, lc);
  char **pathbuf = malloc(sizeof(char *) * lc);
  int count = dfs_paths(conns, lc, "you", "out", pathbuf, 0);
  printf("Total paths: %d\n", count);
  free(pathbuf);

  for (size_t i = 0; i < lc; i++) {
    free(conns[i].source);
    for (int j = 0; j < conns[i].count; j++) free(conns[i].destinations[j]);
    free(conns[i].destinations);
  }
  free(conns);
  free_lines(lines, lc);
}
