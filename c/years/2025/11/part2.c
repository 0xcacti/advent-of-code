#include "utils.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int *to;
  int count;
  int cap;
} adj_t;

typedef struct {
  char name[4];
  adj_t adj;
} node_t;

typedef struct {
  char key[4];
  int value;
  bool used;
} map_entry_t;

typedef struct {
  map_entry_t *entries;
  int cap;
  int len;
} map_t;

static uint32_t hash3(const char s[4]) {
  uint32_t h = 2166136261u;
  for (int i = 0; i < 3; i++) {
    h ^= (unsigned char)s[i];
    h *= 16777619u;
  }
  return h;
}

static void map_init(map_t *m, int cap) {
  m->cap = cap;
  m->len = 0;
  m->entries = calloc((size_t)cap, sizeof(map_entry_t));
}

static void map_free(map_t *m) {
  free(m->entries);
}

static void map_rehash(map_t *m, int new_cap) {
  map_entry_t *old = m->entries;
  int old_cap = m->cap;

  m->entries = calloc((size_t)new_cap, sizeof(map_entry_t));
  m->cap = new_cap;
  m->len = 0;

  for (int i = 0; i < old_cap; i++) {
    if (!old[i].used) continue;
    uint32_t h = hash3(old[i].key);
    int idx = (int)(h % (uint32_t)m->cap);
    while (m->entries[idx].used) idx = (idx + 1) % m->cap;
    m->entries[idx] = old[i];
    m->len++;
  }

  free(old);
}

static int map_get(map_t *m, const char key[4]) {
  uint32_t h = hash3(key);
  int idx = (int)(h % (uint32_t)m->cap);
  for (int probes = 0; probes < m->cap; probes++) {
    map_entry_t *e = &m->entries[idx];
    if (!e->used) return -1;
    if (memcmp(e->key, key, 4) == 0) return e->value;
    idx = (idx + 1) % m->cap;
  }
  return -1;
}

static void map_put(map_t *m, const char key[4], int value) {
  if ((m->len + 1) * 10 >= m->cap * 7) {
    map_rehash(m, m->cap * 2);
  }
  uint32_t h = hash3(key);
  int idx = (int)(h % (uint32_t)m->cap);
  while (m->entries[idx].used) {
    if (memcmp(m->entries[idx].key, key, 4) == 0) {
      m->entries[idx].value = value;
      return;
    }
    idx = (idx + 1) % m->cap;
  }
  m->entries[idx].used = true;
  memcpy(m->entries[idx].key, key, 4);
  m->entries[idx].value = value;
  m->len++;
}

static void adj_push(adj_t *a, int v) {
  if (a->count == a->cap) {
    int new_cap = a->cap == 0 ? 4 : a->cap * 2;
    a->to = realloc(a->to, sizeof(int) * (size_t)new_cap);
    a->cap = new_cap;
  }
  a->to[a->count++] = v;
}

static bool read_name3(const char *p, char out[4]) {
  if (p[0] == '\0' || p[1] == '\0' || p[2] == '\0') return false;
  if (p[0] == '\n' || p[1] == '\n' || p[2] == '\n') return false;
  if (p[0] == '\r' || p[1] == '\r' || p[2] == '\r') return false;
  out[0] = p[0];
  out[1] = p[1];
  out[2] = p[2];
  out[3] = '\0';
  return true;
}

static int ensure_node(node_t **nodes, int *n, int *cap, map_t *map, const char name[4]) {
  int idx = map_get(map, name);
  if (idx >= 0) return idx;

  if (*n == *cap) {
    int new_cap = (*cap == 0) ? 32 : (*cap * 2);
    *nodes = realloc(*nodes, sizeof(node_t) * (size_t)new_cap);
    for (int i = *cap; i < new_cap; i++) {
      (*nodes)[i].name[0] = '\0';
      (*nodes)[i].adj.to = NULL;
      (*nodes)[i].adj.count = 0;
      (*nodes)[i].adj.cap = 0;
    }
    *cap = new_cap;
  }

  idx = *n;
  memcpy((*nodes)[idx].name, name, 4);
  map_put(map, name, idx);
  (*n)++;
  return idx;
}

static void parse_graph(char **lines, size_t lc, node_t **out_nodes, int *out_n, map_t *out_map) {
  node_t *nodes = NULL;
  int n = 0;
  int cap = 0;

  map_t map;
  map_init(&map, 256);

  for (size_t i = 0; i < lc; i++) {
    char *line = lines[i];
    if (line[0] == '\0' || line[0] == '\n' || line[0] == '\r') continue;

    char src[4] = {0};
    if (!read_name3(line, src)) continue;

    int sidx = ensure_node(&nodes, &n, &cap, &map, src);

    char *colon = strchr(line, ':');
    if (colon == NULL) continue;

    char *p = colon + 1;
    while (*p == ' ' || *p == '\t') p++;

    while (*p != '\0' && *p != '\n' && *p != '\r') {
      while (*p == ' ' || *p == '\t') p++;
      if (*p == '\0' || *p == '\n' || *p == '\r') break;

      char dst[4] = {0};
      if (!read_name3(p, dst)) break;

      int didx = ensure_node(&nodes, &n, &cap, &map, dst);
      adj_push(&nodes[sidx].adj, didx);

      p += 3;
    }
  }

  *out_nodes = nodes;
  *out_n = n;
  *out_map = map;
}

static inline int mask_add(int mask, const char name[4]) {
  if (memcmp(name, "dac", 4) == 0) mask |= 1;
  if (memcmp(name, "fft", 4) == 0) mask |= 2;
  return mask;
}

static uint64_t *memo = NULL;
static bool *memo_set = NULL;
static bool *in_stack = NULL;
static node_t *G = NULL;
static int GN = 0;
static int out_idx = -1;

static uint64_t dfs_count(int v, int mask) {
  mask = mask_add(mask, G[v].name);

  if (v == out_idx) {
    return (mask == 3) ? 1ULL : 0ULL;
  }

  int key = v * 4 + mask;
  if (memo_set[key]) return memo[key];

  if (in_stack[key]) {
    fprintf(stderr, "Cycle detected in state (node=%s, mask=%d). DP assumes DAG.\n", G[v].name,
            mask);
    exit(1);
  }

  in_stack[key] = true;

  uint64_t total = 0;
  for (int i = 0; i < G[v].adj.count; i++) {
    int nxt = G[v].adj.to[i];
    total += dfs_count(nxt, mask);
  }

  in_stack[key] = false;

  memo_set[key] = true;
  memo[key] = total;
  return total;
}

int main() {
  bool is_test = false;
  char *path = is_test ? "11/test-input.txt" : "11/input.txt";

  size_t lc = 0;
  char **lines = read_lines(path, &lc);

  node_t *nodes = NULL;
  int n = 0;
  map_t map;
  parse_graph(lines, lc, &nodes, &n, &map);

  int start_idx = map_get(&map, "svr");
  out_idx = map_get(&map, "out");

  if (start_idx < 0) {
    fprintf(stderr, "Start node 'svr' not found in %s\n", path);
    return 1;
  }
  if (out_idx < 0) {
    fprintf(stderr, "Target node 'out' not found in %s\n", path);
    return 1;
  }

  G = nodes;
  GN = n;

  memo = calloc((size_t)GN * 4, sizeof(uint64_t));
  memo_set = calloc((size_t)GN * 4, sizeof(bool));
  in_stack = calloc((size_t)GN * 4, sizeof(bool));

  uint64_t ans = dfs_count(start_idx, 0);
  printf("Total paths: %llu\n", (unsigned long long)ans);

  free(memo);
  free(memo_set);
  free(in_stack);

  for (int i = 0; i < GN; i++) free(nodes[i].adj.to);
  free(nodes);
  map_free(&map);
  free_lines(lines, lc);
}
