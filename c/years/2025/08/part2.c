#include "utils.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  long long x;
  long long y;
  long long z;
} Box;

typedef struct {
  int a;
  int b;
  unsigned long long d2;
} Edge;

typedef struct {
  int *parent;
  int *sz;
} DSU;

static int dsu_find(DSU *d, int x) {
  while (d->parent[x] != x) {
    d->parent[x] = d->parent[d->parent[x]];
    x = d->parent[x];
  }
  return x;
}

static bool dsu_union(DSU *d, int a, int b) {
  int ra = dsu_find(d, a);
  int rb = dsu_find(d, b);
  if (ra == rb) return false;
  if (d->sz[ra] < d->sz[rb]) {
    int t = ra;
    ra = rb;
    rb = t;
  }
  d->parent[rb] = ra;
  d->sz[ra] += d->sz[rb];
  return true;
}

static int edge_cmp(const void *p, const void *q) {
  const Edge *e1 = (const Edge *)p;
  const Edge *e2 = (const Edge *)q;
  if (e1->d2 < e2->d2) return -1;
  if (e1->d2 > e2->d2) return 1;
  if (e1->a != e2->a) return (e1->a < e2->a) ? -1 : 1;
  if (e1->b != e2->b) return (e1->b < e2->b) ? -1 : 1;
  return 0;
}

static Box *parse_boxes(char **lines, size_t line_count, size_t *out_n) {
  Box *boxes = malloc(line_count * sizeof(Box));
  if (boxes == NULL) exit(EXIT_FAILURE);

  size_t count = 0;
  for (size_t i = 0; i < line_count; i++) {
    long long x, y, z;
    if (sscanf(lines[i], "%lld,%lld,%lld", &x, &y, &z) == 3) {
      boxes[count].x = x;
      boxes[count].y = y;
      boxes[count].z = z;
      count++;
    }
  }

  *out_n = count;
  return boxes;
}

static Edge *build_edges(const Box *boxes, size_t n, size_t *out_m) {
  size_t m = (n * (n - 1)) / 2;
  Edge *edges = malloc(m * sizeof(Edge));
  if (edges == NULL) exit(EXIT_FAILURE);

  size_t idx = 0;
  for (size_t i = 0; i < n; i++) {
    for (size_t j = i + 1; j < n; j++) {
      long long dx = boxes[j].x - boxes[i].x;
      long long dy = boxes[j].y - boxes[i].y;
      long long dz = boxes[j].z - boxes[i].z;
      unsigned long long d2 = (unsigned long long)(dx * dx) + (unsigned long long)(dy * dy) +
                              (unsigned long long)(dz * dz);
      edges[idx].a = (int)i;
      edges[idx].b = (int)j;
      edges[idx].d2 = d2;
      idx++;
    }
  }

  *out_m = m;
  return edges;
}

static void print_int128(__int128 v) {
  char buf[80];
  int i = 79;
  buf[i--] = '\0';

  bool neg = false;
  if (v < 0) {
    neg = true;
    v = -v;
  }

  if (v == 0) {
    puts("0");
    return;
  }

  while (v > 0 && i >= 0) {
    int digit = (int)(v % 10);
    buf[i--] = (char)('0' + digit);
    v /= 10;
  }

  if (neg && i >= 0) buf[i--] = '-';
  puts(&buf[i + 1]);
}

int main() {
  bool is_test = false;
  char *path = is_test ? "08/test-input.txt" : "08/input.txt";

  size_t lc = 0;
  char **all_lines = read_lines(path, &lc);

  size_t n = 0;
  Box *boxes = parse_boxes(all_lines, lc, &n);

  size_t m = 0;
  Edge *edges = build_edges(boxes, n, &m);
  qsort(edges, m, sizeof(Edge), edge_cmp);

  DSU d2;
  d2.parent = malloc(n * sizeof(int));
  d2.sz = malloc(n * sizeof(int));
  if (d2.parent == NULL || d2.sz == NULL) exit(EXIT_FAILURE);
  for (size_t i = 0; i < n; i++) {
    d2.parent[i] = (int)i;
    d2.sz[i] = 1;
  }

  size_t components = n;
  int last_a = -1, last_b = -1;
  for (size_t i = 0; i < m; i++) {
    if (dsu_union(&d2, edges[i].a, edges[i].b)) {
      components--;
      last_a = edges[i].a;
      last_b = edges[i].b;
      if (components == 1) break;
    }
  }

  __int128 ans = (__int128)boxes[last_a].x * (__int128)boxes[last_b].x;
  printf("Part 2: ");
  print_int128(ans);

  free(d2.parent);
  free(d2.sz);
  free(edges);
  free(boxes);
  free_lines(all_lines, lc);
  return 0;
}
