# Use the same directory as the Python file
import os

# Get the path to the test-input.txt file
file_path = os.path.join(os.path.dirname(__file__), "test-input.txt")

# Read the content of the file
with open(file_path, "r") as f:
    top, bottom = f.read().split("\n\n")

expansion = {"#": "##", "O": "[]", ".": "..", "@": "@."}

grid = [list("".join(expansion[char] for char in line)) for line in top.splitlines()]
moves = bottom.replace("\n", "")


rows = len(grid)
cols = len(grid[0])

def print_grid(grid):
    for r in range(rows):
        for c in range(cols):
            print(grid[r][c], end="")
        print()

print(f"rows: {rows}, cols: {cols}")

for r in range(rows):
    for c in range(cols):
        if grid[r][c] == "@":
            break
    else:
        continue
    break

print(f"r: {r}, c: {c}")

i = 0 
for move in moves:
    if i == 1000: 
        break
    dr = {"^": -1, "v": 1}.get(move, 0)
    dc = {"<": -1, ">": 1}.get(move, 0)
    targets = [(r, c)]
    go = True
    for cr, cc in targets:
        nr = cr + dr
        nc = cc + dc
        if (nr, nc) in targets: continue
        char = grid[nr][nc]
        if char == "#":
            go = False
            break
        if char == "[":
            targets.append((nr, nc))
            targets.append((nr, nc + 1))
        if char == "]":
            targets.append((nr, nc))
            targets.append((nr, nc - 1))
    if not go: continue
    copy = [list(row) for row in grid]
    grid[r][c] = "."
    grid[r + dr][c + dc] = "@"
    for br, bc in targets[1:]:
        grid[br][bc] = "."
    for br, bc in targets[1:]:
        grid[br + dr][bc + dc] = copy[br][bc]
    r += dr
    c += dc
    i += 1
    print_grid(grid)
# 
# print(sum(100 * r + c for r in range(rows) for c in range(cols) if grid[r][c] == "["))
