local M = {}


-- Read the first lines
function M.mysplit(inputstr, sep)
    if sep == nil then
        sep = "\n\n" -- Default to splitting on empty lines
    end
    local t = {}
    local start = 1
    local sepStart, sepEnd = string.find(inputstr, sep, start)
    while sepStart do
        table.insert(t, string.sub(inputstr, start, sepStart - 1))
        start = sepEnd + 1
        sepStart, sepEnd = string.find(inputstr, sep, start)
    end
    table.insert(t, string.sub(inputstr, start)) -- Add the last segment
    return t
end

function M.split(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

M.insertIntoMap = function(map, ranges)
    for i = 1, #ranges do
        table.insert(map, ranges[i])
    end
    return map
end


M.solve = function()
    -- get input
    io.input("test_input.txt")

    -- read seed line and get seeds
    local line = io.read()
    line = M.mysplit(line, ":")[2]
    line = string.sub(line, 2)
    local seed_ranges = M.mysplit(line, " ")

    -- skip blank line
    _ = io.read()

    -- read map lines and get all maps
    local file = io.read("*all")

    -- extract and normalize maps
    local maps = M.mysplit(file, "\n\n")
    for i = 1, #maps do
        maps[i] = M.split(maps[i], "\n")
        local fixed_map = {}
        for j = 2, #maps[i] do
            fixed_map[j - 1] = maps[i][j]
        end
        maps[i] = fixed_map
    end



    local seeds = {}
    for i = 1, #seed_ranges, 2 do
        table.insert(seeds, seed_ranges[i])
        table.insert(seeds, seed_ranges[i] + seed_ranges[i + 1])
    end

    -- loop over each seed range
    for i = 1, #maps do
        local new = {}
        while #seeds > 0 do
            local matched = false
            local e = tonumber(table.remove(seeds))
            local s = tonumber(table.remove(seeds))
            current_map = maps[i]
            for k = 1, #current_map do
                local entry = current_map[k]
                local entry_parts = M.split(entry, " ")
                local a, b, c = entry_parts[1], entry_parts[2], entry_parts[3]
                a = tonumber(a)
                b = tonumber(b)
                c = tonumber(c)

                local os = tonumber(math.max(s, b))
                local oe = tonumber(math.min(e, b + c))
                if os < oe then
                    table.insert(new, os - b + a)
                    table.insert(new, oe - b + a)
                    if os > s then
                        table.insert(seeds, s)
                        table.insert(seeds, os)
                    end
                    if e > oe then
                        table.insert(seeds, oe)
                        table.insert(seeds, e)
                    end
                    matched = true
                    break
                else
                end
            end
            if not matched then
                table.insert(new, s)
                table.insert(new, e)
            end
        end
        seeds = new
    end

    pairs = {}
    for i = 1, #seeds, 2 do
        table.insert(pairs, { seeds[i], seeds[i + 1] })
    end

    table.sort(pairs, function(a, b)
        return a[1] < b[1]
    end)
    print(pairs[1][1])
end

print(M.solve())
