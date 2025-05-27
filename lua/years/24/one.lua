require("hailstone")


function mysplit(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

function getInput(path)
    io.input(path)
    local inp = {}
    for line in io.lines() do
        line = string.gsub(line, "@", ",")
        local parts = mysplit(line, ",")
        local numericParts = {}
        for _, part in ipairs(parts) do
            table.insert(numericParts, tonumber(part))
        end
        hail = Hailstone.new(table.unpack(numericParts))
        if hail ~= nil then
            table.insert(inp, hail)
        end
    end
    return inp
end

function printInput(input)
    for i, hailstone in ipairs(input) do
        print(hailstone)
    end
end

function contains(t, e)
    for _, v in ipairs(t) do
        if v == e then
            return true
        end
    end
    return false
end

local function intersect(h1, h2)
    local m1 = h1.vy / h1.vx
    local m2 = h2.vy / h2.vx
    local x = (m1 * h1.sx - h1.sy - m2 * h2.sx + h2.sy) / (m1 - m2)
    if x == x and x ~= math.huge and x ~= -1 * math.huge then
        return x, m1 * (x - h1.sx) + h1.sy
    end
end

local function same_dir(v1, v2)
    return v1[1] * v2[1] >= 0 and v1[2] * v2[2] >= 0
end
function solve(path)
    local hailstones = getInput(path)
    local total = 0
    local min, max = 200000000000000, 400000000000000
    for i = 1, #hailstones do
        for j = 1, i - 1 do
            local hail = hailstones[i]
            local stone = hailstones[j]
            local x, y = intersect(hail, stone)

            if x then
                local dx, dy = x - hail.sx, y - hail.sy
                local dx2, dy2 = x - stone.sx, y - stone.sy
                if
                    same_dir({ hail.vx, hail.vy }, { dx, dy })
                    and same_dir({ stone.vx, stone.vy }, { dx2, dy2 })
                    and x >= min
                    and x <= max
                    and y >= min
                    and y <= max
                then
                    total = total + 1
                end
            end
        end
    end
    return total
end

s = solve("input.txt")
print(s)
