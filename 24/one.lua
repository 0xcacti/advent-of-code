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

function solve(path)
    local hailstones = getInput(path)
    printInput(hailstones)
    local total = 0
    for i = 1, #hailstones do
        for j = 1, i - 1 do
            local hail = hailstones[i]
            local stone = hailstones[j]
            local a1, b1, c1 = hail.a, hail.b, hail.c
            local a2, b2, c2 = stone.a, stone.b, stone.c
            if a1 * b2 == b1 * a2 then
                goto continue
            end
            local x = (c1 * b2 - c2 * b1) / (a1 * b2 - a2 * b1)
            local y = (c2 * a1 - c1 * a2) / (a1 * b2 - a2 * b1)

            if 200000000000000 <= x and x <= 400000000000000 and 200000000000000 <= y and y <= 400000000000000 then
                if (x - hail.sx) * hail.vx >= 0 and (y - hail.sy) * hail.vy >= 0 and (x - stone.sx) * stone.vx >= 0 and (y - stone.sy) * stone.vy >= 0 then
                    total = total + 1
                end
            end
            ::continue::
        end
    end
    return total
end

s = solve("input.txt")
print(s)
