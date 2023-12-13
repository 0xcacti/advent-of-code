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

function parseInput()
    io.input("input.txt")

    lines = {}
    for line in io.lines() do
        split = mysplit(line, " ")
        chunks = mysplit(split[2], ",")
        table.insert(lines, { split[1], chunks })
    end
    return lines
end

function getNewGroup(group)
    newGroup = {}
    for i = 2, #group do
        table.insert(newGroup, group[i])
    end
    return newGroup
end

function recurse(line, groups, size)
    size = size or 0
    if line == "" then
        return ((groups == nil or #groups == 0) and (size == 0)) and 1 or 0
    end

    local lineSolutions = 0
    local symbols = (line:sub(1, 1) == '?') and { '.', '#' } or { line:sub(1, 1) }

    for _, sym in ipairs(symbols) do
        if sym == '#' then
            lineSolutions = lineSolutions + recurse(line:sub(2), groups, size + 1)
        else
            if size > 0 then
                if groups and #groups > 0 and groups[1] == size then
                    lineSolutions = lineSolutions + recurse(line:sub(2), { table.unpack(groups, 2) })
                end
            else
                lineSolutions = lineSolutions + recurse(line:sub(2), groups)
            end
        end
    end
    print(lineSolutions)

    return lineSolutions
end

function solve()
    print("Solving...")
    local lines = parseInput()
    local count = 0
    for _, line in ipairs(lines) do
        count = count + recurse(line[1], line[2], 0)
    end
    return count
end

print(recurse("???.###", { 1, 1, 3 }, 0))
