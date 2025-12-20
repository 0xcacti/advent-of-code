#include "utils.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct {
  int x;
  int y;
} pt_t;

typedef struct {
  int w;
  int h;
  int n;
  pt_t pts[9];
} orient_t;

typedef struct {
  int n_orients;
  orient_t *orients;
  int area;
} shape_t;

typedef struct {
  size_t count;
  uint64_t *masks;
} mask_list_t;

typedef struct {
  int w;
  int h;
  int n_shapes;
  int *need;
} region_t;

int pt_cmp(const void *a, const void *b) {
  const pt_t *pa = (const pt_t *)a;
  const pt_t *pb = (const pt_t *)b;
  if (pa->y != pb->y) return pa->y - pb->y;
  return pa->x - pb->x;
}

void sort_pts(pt_t *pts, int n) {
  qsort(pts, (size_t)n, sizeof(pt_t), pt_cmp);
}

uint64_t hash_orient(const pt_t *pts, int n, int w, int h) {
  uint64_t x = 1469598103934665603ULL;
  x ^= (uint64_t)w;
  x *= 1099511628211ULL;
  x ^= (uint64_t)h;
  x *= 1099511628211ULL;
  for (int i = 0; i < n; i++) {
    uint64_t v = (uint64_t)(pts[i].x + 16 * pts[i].y);
    x ^= v;
    x *= 1099511628211ULL;
  }
  return x;
}

orient_t make_orient_from_cells(const int cells[3][3], int rot, int flip) {
  pt_t tmp[9];
  int n = 0;
  for (int y = 0; y < 3; y++) {
    for (int x = 0; x < 3; x++) {
      if (cells[y][x] == 0) continue;
      int xx = x;
      int yy = y;
      if (flip != 0) xx = 2 - xx;
      int rx = xx, ry = yy;
      if (rot == 1) {
        rx = 2 - yy;
        ry = xx;
      } else if (rot == 2) {
        rx = 2 - xx;
        ry = 2 - yy;
      } else if (rot == 3) {
        rx = yy;
        ry = 2 - xx;
      }
      tmp[n++] = (pt_t){.x = rx, .y = ry};
    }
  }

  int minx = 999, miny = 999, maxx = -999, maxy = -999;
  for (int i = 0; i < n; i++) {
    if (tmp[i].x < minx) minx = tmp[i].x;
    if (tmp[i].y < miny) miny = tmp[i].y;
    if (tmp[i].x > maxx) maxx = tmp[i].x;
    if (tmp[i].y > maxy) maxy = tmp[i].y;
  }
  for (int i = 0; i < n; i++) {
    tmp[i].x -= minx;
    tmp[i].y -= miny;
  }
  sort_pts(tmp, n);

  orient_t o;
  o.w = maxx - minx + 1;
  o.h = maxy - miny + 1;
  o.n = n;
  for (int i = 0; i < n; i++) o.pts[i] = tmp[i];
  return o;
}

int orient_equal(const orient_t *a, const orient_t *b) {
  if (a->w != b->w || a->h != b->h || a->n != b->n) return 0;
  for (int i = 0; i < a->n; i++) {
    if (a->pts[i].x != b->pts[i].x || a->pts[i].y != b->pts[i].y) return 0;
  }
  return 1;
}

shape_t build_shape_from_lines(char **shape_lines, int n_lines) {
  int cells[3][3] = {0};
  int area = 0;
  for (int y = 0; y < 3 && y < n_lines; y++) {
    for (int x = 0; x < 3 && shape_lines[y][x] != 0; x++) {
      if (shape_lines[y][x] == '#') {
        cells[y][x] = 1;
        area++;
      }
    }
  }

  orient_t tmp[8];
  int n = 0;
  uint64_t seen[8];
  int seen_n = 0;

  for (int flip = 0; flip < 2; flip++) {
    for (int rot = 0; rot < 4; rot++) {
      orient_t o = make_orient_from_cells(cells, rot, flip);
      uint64_t h = hash_orient(o.pts, o.n, o.w, o.h);
      int dup = 0;
      for (int i = 0; i < seen_n; i++) {
        if (seen[i] == h) {
          dup = 1;
          break;
        }
      }
      if (dup == 0) {
        seen[seen_n++] = h;
        tmp[n++] = o;
      }
    }
  }

  orient_t *orients = (orient_t *)malloc((size_t)n * sizeof(orient_t));
  int out_n = 0;
  for (int i = 0; i < n; i++) {
    int dup = 0;
    for (int j = 0; j < out_n; j++) {
      if (orient_equal(&tmp[i], &orients[j]) != 0) {
        dup = 1;
        break;
      }
    }
    if (dup == 0) orients[out_n++] = tmp[i];
  }

  shape_t s;
  s.n_orients = out_n;
  s.orients = (orient_t *)realloc(orients, (size_t)out_n * sizeof(orient_t));
  s.area = area;
  return s;
}

void free_shapes(shape_t *shapes, int n_shapes) {
  for (int i = 0; i < n_shapes; i++) free(shapes[i].orients);
  free(shapes);
}

void free_regions(region_t *regions, size_t n_regions) {
  for (size_t i = 0; i < n_regions; i++) free(regions[i].need);
  free(regions);
}

int is_blank(const char *s) {
  return s[0] == 0;
}

int is_shape_row(const char *s) {
  for (const char *p = s; *p; p++) {
    if (*p == '#' || *p == '.') return 1;
  }
  return 0;
}

int parse_int(const char *s) {
  return (int)strtol(s, NULL, 10);
}

void parse_input(char **lines, size_t lc, shape_t **out_shapes, int *out_n_shapes,
                 region_t **out_regions, size_t *out_n_regions) {
  int shapes_cap = 16;
  int n_shapes = 0;
  shape_t *shapes = (shape_t *)malloc((size_t)shapes_cap * sizeof(shape_t));

  size_t regions_cap = 32;
  size_t n_regions = 0;
  region_t *regions = (region_t *)malloc(regions_cap * sizeof(region_t));

  int in_shapes = 1;
  int cur_idx = -1;
  char *cur_shape_lines[3] = {0};
  int cur_shape_line_n = 0;

  for (size_t i = 0; i < lc; i++) {
    char *line = lines[i];
    if (is_blank(line)) continue;

    if (in_shapes != 0) {
      size_t len = strlen(line);
      if (len >= 2 && line[len - 1] == ':' && is_shape_row(line) == 0) {
        if (cur_idx >= 0) {
          shape_t s = build_shape_from_lines(cur_shape_lines, cur_shape_line_n);
          if (n_shapes == shapes_cap) {
            shapes_cap *= 2;
            shapes = (shape_t *)realloc(shapes, (size_t)shapes_cap * sizeof(shape_t));
          }
          shapes[n_shapes++] = s;
          cur_shape_line_n = 0;
        }
        cur_idx = parse_int(line);
        continue;
      }

      if (is_shape_row(line) != 0) {
        if (cur_shape_line_n < 3) cur_shape_lines[cur_shape_line_n++] = line;
        continue;
      }

      if (cur_idx >= 0) {
        shape_t s = build_shape_from_lines(cur_shape_lines, cur_shape_line_n);
        if (n_shapes == shapes_cap) {
          shapes_cap *= 2;
          shapes = (shape_t *)realloc(shapes, (size_t)shapes_cap * sizeof(shape_t));
        }
        shapes[n_shapes++] = s;
        cur_idx = -1;
        cur_shape_line_n = 0;
      }
      in_shapes = 0;
    }

    char *colon = strchr(line, ':');
    if (colon == NULL) continue;

    *colon = 0;
    char *spec = line;
    char *counts = colon + 1;

    char *xpos = strchr(spec, 'x');
    if (xpos == NULL) {
      *colon = ':';
      continue;
    }
    *xpos = 0;
    int w = parse_int(spec);
    int h = parse_int(xpos + 1);
    *xpos = 'x';

    int *need = (int *)calloc((size_t)n_shapes, sizeof(int));

    int si = 0;
    char *p = counts;
    while (*p == ' ') p++;
    while (*p) {
      if (si >= n_shapes) break;
      char *endp = NULL;
      long v = strtol(p, &endp, 10);
      if (endp == p) break;
      need[si++] = (int)v;
      p = endp;
      while (*p == ' ') p++;
    }

    if (n_regions == regions_cap) {
      regions_cap *= 2;
      regions = (region_t *)realloc(regions, regions_cap * sizeof(region_t));
    }
    regions[n_regions++] = (region_t){.w = w, .h = h, .n_shapes = n_shapes, .need = need};

    *colon = ':';
  }

  if (in_shapes != 0 && cur_idx >= 0) {
    shape_t s = build_shape_from_lines(cur_shape_lines, cur_shape_line_n);
    if (n_shapes == shapes_cap) {
      shapes_cap *= 2;
      shapes = (shape_t *)realloc(shapes, (size_t)shapes_cap * sizeof(shape_t));
    }
    shapes[n_shapes++] = s;
  }

  *out_shapes = (shape_t *)realloc(shapes, (size_t)n_shapes * sizeof(shape_t));
  *out_n_shapes = n_shapes;
  *out_regions = (region_t *)realloc(regions, n_regions * sizeof(region_t));
  *out_n_regions = n_regions;
}

mask_list_t build_masks_for_shape_in_region(const shape_t *shape, int W, int H) {
  size_t words = ((size_t)W * (size_t)H + 63) / 64;
  size_t cap = 64;
  size_t count = 0;
  uint64_t *masks = (uint64_t *)malloc(cap * words * sizeof(uint64_t));

  for (int oi = 0; oi < shape->n_orients; oi++) {
    const orient_t *o = &shape->orients[oi];
    if (o->w > W || o->h > H) continue;

    for (int y0 = 0; y0 <= H - o->h; y0++) {
      for (int x0 = 0; x0 <= W - o->w; x0++) {
        if (count == cap) {
          cap *= 2;
          masks = (uint64_t *)realloc(masks, cap * words * sizeof(uint64_t));
        }
        uint64_t *m = &masks[count * words];
        for (size_t k = 0; k < words; k++) m[k] = 0;

        for (int pi = 0; pi < o->n; pi++) {
          int x = x0 + o->pts[pi].x;
          int y = y0 + o->pts[pi].y;
          size_t idx = (size_t)y * (size_t)W + (size_t)x;
          m[idx / 64] |= (1ULL << (idx % 64));
        }
        count++;
      }
    }
  }

  mask_list_t ml;
  ml.count = count;
  ml.masks = (uint64_t *)realloc(masks, count * words * sizeof(uint64_t));
  return ml;
}

int can_place(const uint64_t *occ, const uint64_t *mask, size_t words) {
  for (size_t i = 0; i < words; i++) {
    if ((occ[i] & mask[i]) != 0) return 0;
  }
  return 1;
}

void apply_place(uint64_t *dst, const uint64_t *occ, const uint64_t *mask, size_t words) {
  for (size_t i = 0; i < words; i++) dst[i] = occ[i] | mask[i];
}

int dfs_solve(const shape_t *shapes, int n_shapes, const mask_list_t *mlists, int *need,
              uint64_t *occ, size_t words, int remaining_area, int total_cells) {
  if (remaining_area == 0) return 1;

  int free_cells = total_cells;
  for (size_t i = 0; i < words; i++) free_cells -= (int)__builtin_popcountll(occ[i]);
  if (remaining_area > free_cells) return 0;

  int best_shape = -1;
  size_t best_opts = (size_t)-1;

  for (int si = 0; si < n_shapes; si++) {
    if (need[si] <= 0) continue;
    const mask_list_t *ml = &mlists[si];
    size_t opts = 0;
    for (size_t pi = 0; pi < ml->count; pi++) {
      const uint64_t *mask = &ml->masks[pi * words];
      if (can_place(occ, mask, words) != 0) opts++;
      if (opts >= best_opts) break;
    }
    if (opts == 0) return 0;
    if (opts < best_opts) {
      best_opts = opts;
      best_shape = si;
      if (best_opts == 1) break;
    }
  }

  if (best_shape < 0) return 0;

  const mask_list_t *ml = &mlists[best_shape];
  for (size_t pi = 0; pi < ml->count; pi++) {
    const uint64_t *mask = &ml->masks[pi * words];
    if (can_place(occ, mask, words) == 0) continue;

    uint64_t next_occ[words];
    apply_place(next_occ, occ, mask, words);

    need[best_shape]--;
    int ok = dfs_solve(shapes, n_shapes, mlists, need, next_occ, words,
                       remaining_area - shapes[best_shape].area, total_cells);
    need[best_shape]++;

    if (ok != 0) return 1;
  }

  return 0;
}

int region_satisfiable(const shape_t *shapes, int n_shapes, const region_t *r) {
  int W = r->w;
  int H = r->h;
  int total_cells = W * H;
  size_t words = ((size_t)total_cells + 63) / 64;

  int remaining_area = 0;
  for (int i = 0; i < n_shapes; i++) remaining_area += r->need[i] * shapes[i].area;
  if (remaining_area > total_cells) return 0;

  mask_list_t *mlists = (mask_list_t *)malloc((size_t)n_shapes * sizeof(mask_list_t));
  for (int i = 0; i < n_shapes; i++) mlists[i] = build_masks_for_shape_in_region(&shapes[i], W, H);

  int *need = (int *)malloc((size_t)n_shapes * sizeof(int));
  memcpy(need, r->need, (size_t)n_shapes * sizeof(int));

  uint64_t occ[words];
  for (size_t i = 0; i < words; i++) occ[i] = 0;

  int ok = dfs_solve(shapes, n_shapes, mlists, need, occ, words, remaining_area, total_cells);

  for (int i = 0; i < n_shapes; i++) free(mlists[i].masks);
  free(mlists);
  free(need);

  return ok;
}

int main() {
  bool is_test = false;
  char *path = NULL;
  if (is_test) {
    path = "12/test-input.txt";
  } else {
    path = "12/input.txt";
  }
  size_t lc = 0;
  char **lines = read_lines(path, &lc);
  strip_newlines(lines, lc);

  shape_t *shapes = NULL;
  int n_shapes = 0;
  region_t *regions = NULL;
  size_t n_regions = 0;

  parse_input(lines, lc, &shapes, &n_shapes, &regions, &n_regions);

  int answer = 0;
  for (size_t i = 0; i < n_regions; i++) {
    if (region_satisfiable(shapes, n_shapes, &regions[i]) != 0) answer++;
  }

  printf("%d\n", answer);

  free_shapes(shapes, n_shapes);
  free_regions(regions, n_regions);

  return 0;
}
