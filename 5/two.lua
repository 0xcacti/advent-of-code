local M = {}

-- Read the first lines
function mysplit(inputstr, sep)
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

function split(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

M.map_range = function(seed_start, seed_end, dest_low, source_low, interval)
    seed_start = tonumber(seed_start)
    seed_end = tonumber(seed_end)
    interval = tonumber(interval)
    local dest_start = tonumber(dest_low)
    local dest_end = dest_start + interval
    local source_start = tonumber(source_low)
    local source_end = source_low + interval - 1

    print("seed_start: " .. seed_start)
    print("seed_end: " .. seed_end)
    print("source_start: " .. source_start)
    print("source_end: " .. source_end)

    if not (seed_start and seed_end and interval and dest_start and dest_end and source_start and source_end) then
        print("not all numbers")
        return nil, nil
    end


    local overlap_start = math.max(seed_start, source_start)
    local overlap_end = math.min(seed_end, source_end)
    print("overlap_start: " .. overlap_start)
    print("overlap_end: " .. overlap_end)

    if overlap_start > overlap_end then
        print("overlap_start > overlap_end")
        return nil, nil
    end

    local mapped_start = dest_start + (overlap_start - source_start)
    local mapped_end = dest_start + (overlap_end - source_start)
    local mapped_interval = mapped_end - mapped_start + 1

    local mapped = { mapped_start, mapped_interval }
    -- check if it's a subset

    if seed_start == overlap_start and seed_end == overlap_end then
        print("subset")
        return mapped, nil
    end

    local unmapped = {}
    -- check if there is lower exclusion

    if seed_start < overlap_start then
        print("lower exclusion")
        local lower_unmapped = seed_start
        local lower_interval = (overlap_start - seed_start)
        table.insert(unmapped, lower_unmapped)
        table.insert(unmapped, lower_interval)
    end

    if overlap_end < seed_end then
        print("upper exclusion")
        local upper_unmapped = overlap_end + 1
        local upper_interval = (seed_end - upper_unmapped) + 1
        table.insert(unmapped, upper_unmapped)
        table.insert(unmapped, upper_interval)
    end

    return mapped, unmapped
end

M.solve = function()
    io.input("test_input.txt")
    local line = io.read()
    line = mysplit(line, ":")[2]
    line = string.sub(line, 2)
    local seed_ranges = mysplit(line, " ")
    _ = io.read()

    file = io.read("*all")
    local maps = mysplit(file, "\n\n")

    -- if current_seed_value < current_lowest then
    --     current_lowest = current_seed_value
    -- end
    -- print(current_lowest)

    for i = 2, #maps do
        local new_seed_ranges = {}
        local j = 1

        -- loop until all seed ranges are mapped
        while j <= #seed_ranges do
            -- loop over each seed range until total is nothing
            local start = seed_ranges[i]
            local stop = seed_ranges[i] + seed_ranges[i + 1]
            local total = stop - start + 1
            local round_unmapped = {}
            local map_entries = split(maps[i], "\n")
            while total > 0 do
                print("total: " .. total)
                for k = 2, #map_entries do
                    if k == #map_entries then
                        for l = 1, #round_unmapped do
                            table.insert(new_seed_ranges, round_unmapped[l])
                        end
                    end
                    local entry = map_entries[k]
                    local entry_parts = split(entry, " ")
                    local mapped, unmapped = map_range(start, stop, entry_parts[1], entry_parts[2],
                        entry_parts[3])
                    if mapped then
                        table.insert(new_seed_ranges, mapped[1])
                        table.insert(new_seed_ranges, mapped[2])
                        total = total - (mapped[1] + mapped[2] + 1)
                    end
                    if unmapped then
                        table.insert(round_unmapped, unmapped[1])
                        table.insert(round_unmapped, unmapped[2])
                    end
                end
            end
        end
        seed_ranges = new_seed_ranges
    end

    for i = 1, #seed_ranges do
        print(seed_ranges[i])
    end
end

-- print(solve())


return M


--
