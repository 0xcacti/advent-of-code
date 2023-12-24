function openFile()
    local file = io.open("input.txt", "r")
    local content = file:read("*all")
    file:close()
    return content
end

grid = {}
for line in string.gmatch(openFile(), "[^\r\n]+") do
    table.insert(grid, line)
end

local sr, sc
for r = 1, #grid do
    for c = 1, #grid[r] do
        if string.sub(grid[r], c, c) == "S" then
            sr, sc = r, c
            break
        end
    end
    if sr then break end
end

assert(#grid == #grid[1])

local size = #grid
local steps = 26501365

assert(sr == sc and sr == math.floor(size / 2) + 1)
assert(steps % size == math.floor(size / 2))

function fill(sr, sc, ss)
    local ans = {}
    local seen = { [sr .. "," .. sc] = true }
    local q = { { sr, sc, ss } }

    while #q > 0 do
        local r, c, s = table.unpack(table.remove(q, 1))

        if s % 2 == 0 then
            ans[r .. "," .. c] = true
        end
        if s == 0 then
            goto continue
        end

        for _, offset in pairs({ { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } }) do
            local nr, nc = r + offset[1], c + offset[2]
            local key = nr .. "," .. nc
            if nr < 1 or nr > #grid or nc < 1 or nc > #grid[1] or string.sub(grid[nr], nc, nc) == "#" or seen[key] then
                goto continue_loop
            end
            seen[key] = true
            table.insert(q, { nr, nc, s - 1 })
            ::continue_loop::
        end
        ::continue::
    end

    return tablelength(ans)
end

function tablelength(T)
    local count = 0
    for _ in pairs(T) do count = count + 1 end
    return count
end

local grid_width = math.floor(steps / size) - 1

local odd = (math.floor(grid_width / 2) * 2 + 1) ^ 2
local even = (math.floor((grid_width + 1) / 2) * 2) ^ 2

local odd_points = fill(sr, sc, size * 2 + 1)
local even_points = fill(sr, sc, size * 2)

local corner_t = fill(size, sc, size)
local corner_r = fill(sr, 1, size)
local corner_b = fill(1, sc, size)
local corner_l = fill(sr, size, size)

local small_tr = fill(size, 1, math.floor(size / 2))
local small_tl = fill(size, size, math.floor(size / 2))
local small_br = fill(1, 1, math.floor(size / 2))
local small_bl = fill(1, size, math.floor(size / 2))

local large_tr = fill(size, 1, math.floor(size * 3 / 2))
local large_tl = fill(size, size, math.floor(size * 3 / 2))
local large_br = fill(1, 1, math.floor(size * 3 / 2))
local large_bl = fill(1, size, math.floor(size * 3 / 2))

local s = odd * odd_points +
    even * even_points +
    corner_t + corner_r + corner_b + corner_l +
    (grid_width + 1) * (small_tr + small_tl + small_br + small_bl) +
    grid_width * (large_tr + large_tl + large_br + large_bl)

print(tostring(s))
print(string.format("%.14f", s))
