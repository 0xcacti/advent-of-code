#include "utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  long long x;
  long long y;
} point_t;

typedef struct {
  long long x1;
  long long y1;
  long long x2;
  long long y2;
  bool vertical;
} segment_t;

bool point_in_poly(double px, double py, segment_t *segs, size_t m) {
  int crosses = 0;
  for (size_t i = 0; i < m; i++) {
    if (!segs[i].vertical) continue;

    double x = (double)segs[i].x1;
    double y1 = segs[i].y1 < segs[i].y2 ? (double)segs[i].y1 : (double)segs[i].y2;
    double y2 = segs[i].y1 > segs[i].y2 ? (double)segs[i].y1 : (double)segs[i].y2;

    if (py >= y1 && py < y2 && px < x) {
      crosses++;
    }
  }
  return (crosses & 1) != 0;
}

bool boundary_crosses_rect_interior(segment_t *segs, size_t m, long long xL, long long xR,
                                    long long yB, long long yT) {
  for (size_t i = 0; i < m; i++) {
    segment_t s = segs[i];

    if (s.vertical) {
      long long x = s.x1;
      if (!(xL < x && x < xR)) continue;

      long long sy1 = s.y1 < s.y2 ? s.y1 : s.y2;
      long long sy2 = s.y1 > s.y2 ? s.y1 : s.y2;

      long long lo = sy1 > yB ? sy1 : yB;
      long long hi = sy2 < yT ? sy2 : yT;
      if (lo < hi) return true;
    } else {
      long long y = s.y1;
      if (!(yB < y && y < yT)) continue;

      long long sx1 = s.x1 < s.x2 ? s.x1 : s.x2;
      long long sx2 = s.x1 > s.x2 ? s.x1 : s.x2;

      long long lo = sx1 > xL ? sx1 : xL;
      long long hi = sx2 < xR ? sx2 : xR;
      if (lo < hi) return true;
    }
  }
  return false;
}

bool rect_ok(segment_t *segs, size_t n, long long xL, long long yB, long long xR, long long yT) {
  double sx = ((double)xL + (double)xR) / 2.0;
  double sy = ((double)yB + (double)yT) / 2.0;

  if (sx <= (double)xL) sx = (double)xL + 0.1;
  if (sx >= (double)xR) sx = (double)xR - 0.1;
  if (sy <= (double)yB) sy = (double)yB + 0.1;
  if (sy >= (double)yT) sy = (double)yT - 0.1;

  if (!point_in_poly(sx, sy, segs, n)) return false;
  if (boundary_crosses_rect_interior(segs, n, xL, xR, yB, yT)) return false;
  return true;
}

int main() {
  bool is_test = false;
  char *path = is_test ? "09/test-input.txt" : "09/input.txt";
  size_t lc = 0;
  char **lines = read_lines(path, &lc);

  point_t *pts = malloc(sizeof(point_t) * lc);
  if (pts == NULL) exit(EXIT_FAILURE);

  size_t n = 0;
  for (size_t i = 0; i < lc; i++) {
    sscanf(lines[i], "%lld,%lld", &pts[n].x, &pts[n].y);
    n++;
  }

  segment_t *segs = malloc(sizeof(segment_t) * n);
  if (segs == NULL) exit(EXIT_FAILURE);

  for (size_t i = 0; i < n; i++) {
    point_t a = pts[i];
    point_t b = pts[(i + 1) % n];
    segs[i].x1 = a.x;
    segs[i].y1 = a.y;
    segs[i].x2 = b.x;
    segs[i].y2 = b.y;
    segs[i].vertical = (a.x == b.x);
  }

  long long best = 0;
  for (size_t i = 0; i < n; i++) {
    for (size_t j = i + 1; j < n; j++) {
      long long x1 = pts[i].x;
      long long y1 = pts[i].y;
      long long x2 = pts[j].x;
      long long y2 = pts[j].y;

      if (x1 == x2 || y1 == y2) continue;

      long long xL = x1 < x2 ? x1 : x2;
      long long xR = x1 > x2 ? x1 : x2;
      long long yB = y1 < y2 ? y1 : y2;
      long long yT = y1 > y2 ? y1 : y2;

      long long area = (xR - xL + 1) * (yT - yB + 1);
      if (area <= best) continue;

      if (rect_ok(segs, n, xL, yB, xR, yT)) {
        best = area;
      }
    }
  }

  printf("Max area: %lld\n", best);

  free(segs);
  free(pts);
  free_lines(lines, lc);
  return 0;
}
