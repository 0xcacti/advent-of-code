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


local map = {}

for line in io.lines() do
    parts = mysplit(line, " ")

    map[parts[1]] = { string.sub(parts[3], 2, 4), string.sub(parts[4], 1, 3) }
end

local count = 0
loc = "AAA"
while loc ~= "ZZZ" do
    for i = 1, #instructions do
        chr = string.sub(instructions, i, i)
        if chr == "L" then
            loc = map[loc][1]
        end
        if chr == "R" then
            loc = map[loc][2]
        end
        count = count + 1
        if loc == "ZZZ" then
            goto done
        end
    end
end

::done::
print(count)
