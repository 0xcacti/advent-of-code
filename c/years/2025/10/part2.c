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
  uint64_t *buttons;
  int button_count;
  int *joltages;
  int joltage_count;
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

uint64_t parse_button(char *button_str, int num_lights) {
  uint64_t button = 0;
  int current = 0;
  int len = (int)strlen(button_str);
  bool in_number = false;

  for (int i = 0; i < len; i++) {
    char c = button_str[i];
    if (c >= '0' && c <= '9') {
      in_number = true;
      current = current * 10 + (c - '0');
      continue;
    }
    if ((c == ',' || c == ')') && in_number) {
      if (0 <= current && current < num_lights) button |= (1ULL << current);
      current = 0;
      in_number = false;
    }
  }

  return button;
}

int *parse_joltages(const char *tok, int *out_count) {
  int cap = 8;
  int cnt = 0;
  int *vals = malloc(sizeof(int) * (size_t)cap);

  int current = 0;
  bool in_number = false;

  for (int i = 0; tok[i] != '\0'; i++) {
    char c = tok[i];
    if (c >= '0' && c <= '9') {
      in_number = true;
      current = current * 10 + (c - '0');
      continue;
    }
    if ((c == ',' || c == '}') && in_number) {
      if (cnt >= cap) {
        cap *= 2;
        vals = realloc(vals, sizeof(int) * (size_t)cap);
      }
      vals[cnt++] = current;
      current = 0;
      in_number = false;
    }
  }

  *out_count = cnt;
  return vals;
}

machine_t *parse_machines(char **lines, size_t line_count) {
  machine_t *machines = malloc(sizeof(machine_t) * line_count);

  for (size_t i = 0; i < line_count; i++) {
    machine_t m = {0};

    char *copy = strdup(lines[i]);
    char *tok = strtok(copy, " ");
    if (tok == NULL) {
      free(copy);
      machines[i] = m;
      continue;
    }

    int button_cap = 8;
    m.buttons = malloc(sizeof(uint64_t) * (size_t)button_cap);
    m.button_count = 0;

    tok = strtok(NULL, " ");
    while (tok != NULL) {
      if (tok[0] == '(') {
        if (m.button_count >= button_cap) {
          button_cap *= 2;
          m.buttons = realloc(m.buttons, sizeof(uint64_t) * (size_t)button_cap);
        }
        m.buttons[m.button_count++] = parse_button(tok, 64);
      } else if (tok[0] == '{') {
        m.joltages = parse_joltages(tok, &m.joltage_count);
        break;
      }
      tok = strtok(NULL, " ");
    }

    free(copy);
    machines[i] = m;
  }

  return machines;
}

#include <stdint.h>

typedef struct {
  uint32_t *data;
  size_t len;
  size_t cap;
} u32_arena_t;

static void u32_arena_init(u32_arena_t *a) {
  a->data = NULL;
  a->len = 0;
  a->cap = 0;
}

static uint32_t *u32_arena_alloc(u32_arena_t *a, int k) {
  size_t need = a->len + (size_t)k;
  if (need > a->cap) {
    size_t nc = (a->cap == 0) ? 4096 : a->cap;
    while (nc < need) nc *= 2;
    a->data = realloc(a->data, sizeof(uint32_t) * nc);
    a->cap = nc;
  }
  uint32_t *p = a->data + a->len;
  a->len = need;
  return p;
}

static uint64_t fnv1a_u32(const uint32_t *v, int k) {
  uint64_t h = 1469598103934665603ULL;
  const unsigned char *p = (const unsigned char *)v;
  size_t n = (size_t)k * sizeof(uint32_t);
  for (size_t i = 0; i < n; i++) {
    h ^= (uint64_t)p[i];
    h *= 1099511628211ULL;
  }
  return h;
}

typedef struct {
  uint64_t h;
  uint32_t *key;
  int val;
  bool used;
} memo_ent_t;

typedef struct {
  memo_ent_t *tab;
  size_t cap;
  size_t used;
  int k;
} memo_t;

static size_t pow2_ge(size_t x) {
  size_t p = 1;
  while (p < x) p <<= 1;
  return p;
}

static void memo_init(memo_t *m, int k, size_t cap_hint) {
  m->k = k;
  m->cap = pow2_ge(cap_hint < 1024 ? 1024 : cap_hint);
  m->used = 0;
  m->tab = calloc(m->cap, sizeof(memo_ent_t));
}

static void memo_free(memo_t *m) {
  free(m->tab);
  m->tab = NULL;
  m->cap = 0;
  m->used = 0;
}

static void memo_rehash(memo_t *m) {
  size_t oc = m->cap;
  memo_ent_t *old = m->tab;

  m->cap *= 2;
  m->used = 0;
  m->tab = calloc(m->cap, sizeof(memo_ent_t));

  for (size_t i = 0; i < oc; i++) {
    if (!old[i].used) continue;
    uint64_t h = old[i].h;
    size_t mask = m->cap - 1;
    size_t idx = (size_t)h & mask;
    for (;;) {
      if (!m->tab[idx].used) {
        m->tab[idx] = old[i];
        m->tab[idx].used = true;
        m->used++;
        break;
      }
      idx = (idx + 1) & mask;
    }
  }

  free(old);
}

static bool key_eq_u32(const uint32_t *a, const uint32_t *b, int k) {
  return memcmp(a, b, (size_t)k * sizeof(uint32_t)) == 0;
}

static bool memo_get(memo_t *m, uint64_t h, const uint32_t *key, int *out) {
  size_t mask = m->cap - 1;
  size_t idx = (size_t)h & mask;
  for (;;) {
    if (!m->tab[idx].used) return false;
    if (m->tab[idx].h == h && key_eq_u32(m->tab[idx].key, key, m->k)) {
      *out = m->tab[idx].val;
      return true;
    }
    idx = (idx + 1) & mask;
  }
}

static void memo_set(memo_t *m, uint64_t h, uint32_t *key, int val) {
  if ((m->used + 1) * 10 >= m->cap * 7) memo_rehash(m);

  size_t mask = m->cap - 1;
  size_t idx = (size_t)h & mask;
  for (;;) {
    if (!m->tab[idx].used) {
      m->tab[idx].used = true;
      m->tab[idx].h = h;
      m->tab[idx].key = key;
      m->tab[idx].val = val;
      m->used++;
      return;
    }
    if (m->tab[idx].h == h && key_eq_u32(m->tab[idx].key, key, m->k)) {
      m->tab[idx].key = key;
      m->tab[idx].val = val;
      return;
    }
    idx = (idx + 1) & mask;
  }
}

typedef struct {
  uint64_t parity;
  uint64_t subset;
  uint8_t sz;
} subset_ent_t;

static int cmp_subset_ent(const void *a, const void *b) {
  const subset_ent_t *x = (const subset_ent_t *)a;
  const subset_ent_t *y = (const subset_ent_t *)b;
  if (x->parity < y->parity) return -1;
  if (x->parity > y->parity) return 1;
  if (x->subset < y->subset) return -1;
  if (x->subset > y->subset) return 1;
  return 0;
}

static void find_range(const subset_ent_t *arr, int n, uint64_t key, int *lo, int *hi) {
  int l = 0, r = n;
  while (l < r) {
    int mid = l + (r - l) / 2;
    if (arr[mid].parity < key)
      l = mid + 1;
    else
      r = mid;
  }
  int start = l;
  l = start;
  r = n;
  while (l < r) {
    int mid = l + (r - l) / 2;
    if (arr[mid].parity <= key)
      l = mid + 1;
    else
      r = mid;
  }
  *lo = start;
  *hi = l;
}

static int solve_rec(const uint32_t *J, int k, const uint16_t *target, const uint64_t *touchMask,
                     const subset_ent_t *subs, int subs_n, memo_t *memo, u32_arena_t *arena) {
  bool all0 = true;
  for (int i = 0; i < k; i++)
    if (J[i] != 0) {
      all0 = false;
      break;
    }
  if (all0) return 0;

  uint64_t h = fnv1a_u32(J, k);
  int cached = 0;
  if (memo_get(memo, h, J, &cached)) return cached;

  uint64_t par = 0;
  for (int i = 0; i < k; i++) {
    if ((J[i] & 1U) != 0U) par |= (1ULL << i);
  }

  int lo = 0, hi = 0;
  find_range(subs, subs_n, par, &lo, &hi);
  if (lo == hi) {
    uint32_t *keyc = u32_arena_alloc(arena, k);
    memcpy(keyc, J, (size_t)k * sizeof(uint32_t));
    memo_set(memo, h, keyc, 0x3fffffff);
    return 0x3fffffff;
  }

  int best = 0x3fffffff;
  uint32_t *next = alloca((size_t)k * sizeof(uint32_t));

  for (int idx = lo; idx < hi; idx++) {
    uint64_t sm = subs[idx].subset;

    bool ok = true;
    for (int i = 0; i < k; i++) {
      int cnt = __builtin_popcountll(sm & touchMask[i]);
      uint32_t ji = J[i];
      if ((uint32_t)cnt > ji) {
        ok = false;
        break;
      }
      uint32_t diff = ji - (uint32_t)cnt;
      if ((diff & 1U) != 0U) {
        ok = false;
        break;
      }
      next[i] = diff / 2U;
    }
    if (!ok) continue;

    int subcost = solve_rec(next, k, target, touchMask, subs, subs_n, memo, arena);
    if (subcost == 0x3fffffff) continue;

    int cand = (int)subs[idx].sz + 2 * subcost;
    if (cand < best) best = cand;
  }

  uint32_t *keyc = u32_arena_alloc(arena, k);
  memcpy(keyc, J, (size_t)k * sizeof(uint32_t));
  memo_set(memo, h, keyc, best);
  return best;
}

int min_presses_joltage_halving(const machine_t *m) {
  int k = m->joltage_count;
  int bcount = m->button_count;
  if (k <= 0) return -1;
  if (k > 63) return -1;
  if (bcount <= 0) {
    for (int i = 0; i < k; i++)
      if (m->joltages[i] != 0) return -1;
    return 0;
  }
  if (bcount > 24) return -1;

  uint16_t *target = malloc(sizeof(uint16_t) * (size_t)k);
  for (int i = 0; i < k; i++) {
    if (m->joltages[i] < 0) {
      free(target);
      return -1;
    }
    if (m->joltages[i] > 65535) {
      free(target);
      return -1;
    }
    target[i] = (uint16_t)m->joltages[i];
  }

  uint64_t *touchMask = calloc((size_t)k, sizeof(uint64_t));
  for (int bi = 0; bi < bcount; bi++) {
    uint64_t cm = m->buttons[bi];
    for (int i = 0; i < k; i++) {
      if ((cm & (1ULL << i)) != 0) touchMask[i] |= (1ULL << bi);
    }
  }

  int subs_n = 1 << bcount;
  subset_ent_t *subs = malloc(sizeof(subset_ent_t) * (size_t)subs_n);

  for (int sm = 0; sm < subs_n; sm++) {
    uint64_t s = (uint64_t)sm;

    uint64_t par = 0;
    for (int i = 0; i < k; i++) {
      int cnt = __builtin_popcountll(s & touchMask[i]);
      if ((cnt & 1) != 0) par |= (1ULL << i);
    }

    subs[sm].parity = par;
    subs[sm].subset = s;
    subs[sm].sz = (uint8_t)__builtin_popcountll(s);
  }

  qsort(subs, (size_t)subs_n, sizeof(subset_ent_t), cmp_subset_ent);

  u32_arena_t arena;
  u32_arena_init(&arena);

  memo_t memo;
  memo_init(&memo, k, 1u << 18);

  uint32_t *J0 = alloca((size_t)k * sizeof(uint32_t));
  for (int i = 0; i < k; i++) J0[i] = (uint32_t)target[i];

  int ans = solve_rec(J0, k, target, touchMask, subs, subs_n, &memo, &arena);
  if (ans == 0x3fffffff) ans = -1;

  memo_free(&memo);
  free(arena.data);
  free(subs);
  free(touchMask);
  free(target);
  return ans;
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
    int presses = min_presses_joltage_halving(&machines[i]);
    printf("machine %zu presses: %d\n", i, presses);

    total += presses;
  }
  printf("total presses: %d\n", total);

  for (size_t i = 0; i < lc; i++) {
    free(machines[i].buttons);
    free(machines[i].joltages);
    free(lines[i]);
  }
  free(lines);
}
