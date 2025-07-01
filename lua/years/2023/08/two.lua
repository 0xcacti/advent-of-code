local function mysplit(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

io.input("input.txt")
local instructions = io.read()
io.read()

function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

function lcm(a, b)
    return (a * b) / gcd(a, b)
end

local map = {}

for line in io.lines() do
    parts = mysplit(line, " ")
    map[parts[1]] = { string.sub(parts[3], 2, 4), string.sub(parts[4], 1, 3) }
end

function allZ(t)
    for k, v in pairs(t) do
        local e = string.sub(v, 3, 3)
        if e ~= "Z" then
            return false
        end
    end
    return true
end

locs = {}
for i, entry in pairs(map) do
    if string.sub(i, 3, 3) == "A" then
        table.insert(locs, i)
    end
end


local counts = {}
for i, loc in pairs(locs) do
    local count = 0
    while not allZ({ loc }) do
        for j = 1, #instructions do
            if allZ({ loc }) then
                goto loop
            end
            local chr = string.sub(instructions, j, j)
            if chr == "L" then
                loc = map[loc][1]
            end
            if chr == "R" then
                loc = map[loc][2]
            end
            count = count + 1
        end
    end
    table.insert(counts, count)
    ::loop::
end

local result = counts[1]
for i = 2, #counts do
    result = lcm(result, counts[i])
end
print(result)
