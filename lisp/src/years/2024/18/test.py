from collections import deque

# Grid size for full data (0 to 70 inclusive)
s = 70  
grid = [[0] * (s + 1) for _ in range(s + 1)]

# Read coordinates from input file
with open('input.txt', 'r') as f:
    coords = [list(map(int, line.strip().split(","))) for line in f]

# Mark corrupted locations in the grid
for x, y in coords[:1024]:  # Only simulate first 1024 bytes
    grid[y][x] = 1

# Use BFS to find shortest path
q = deque([(0, 0, 0)])  # (row, col, distance)
seen = {(0, 0)}

while q:
    r, c, d = q.popleft()
    for nr, nc in [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]:
        if nr < 0 or nc < 0 or nr > s or nc > s:
            continue
        if grid[nr][nc] == 1:  # Skip corrupted locations
            continue
        if (nr, nc) in seen:
            continue
        if nr == s and nc == s:
            print(f"Found path with {d + 1} steps!")
            exit(0)
        seen.add((nr, nc))
        q.append((nr, nc, d + 1))

print("No solution found!")

# Optional: Uncomment to visualize a small portion of the grid (e.g., first 10x10)
#print("\nFirst 10x10 section of the grid:")
#for row in grid[:10]:
#    print("".join('#' if cell else '.' for cell in row[:10]))
