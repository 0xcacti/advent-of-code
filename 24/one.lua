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
        parts = mysplit(line, ",")
        numericParts = {}
        for _, part in ipairs(parts) do
            if part == nil then
                print("fuck")
                break
            end

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
    print(#hailstones)
    for i = 1, #hailstones do
        for j = i + 1, #hailstones do
            hail = hailstones[i]
            stone = hailstones[j]
            a1, b1, c1 = hail.a, hail.b, hail.c
            a2, b2, c2 = stone.a, stone.b, stone.c
            if a1 * b2 == b1 * a2 then
                goto continue
            end

            ::continue::
        end
    end
end

s = solve("test_input.txt")
print(s)
