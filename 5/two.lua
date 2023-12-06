-- io.input("input.txt")
io.input("test_input.txt")

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

function map_range(seed_start, seed_end, dest_low, source_low, interval)
    local seed_start = tonumber(seed_start)
    local seed_end = tonumber(seed_end)
    local interval = tonumber(interval)
    local dest_start = tonumber(dest_low)
    local dest_end = dest_low + interval
    local source_start = tonumber(source_low)
    local source_end = source_low + interval

    if not (seed_start and seed_end and interval and dest_start and dest_end and source_start and source_end) then
        return false, nil, nil
    end

    if source_start > seed_start then
        return false, nil, nil
    end

    local overlap_start = math.max(seed_start, source_start)
    local overlap_end = math.min(seed_end, source_end)
    local mapped_start = dest_start + (overlap_start - source_start)
    local mapped_end = dest_start + (overlap_end - source_start)
    local mapped_interval = mapped_end - mapped_start

    local mapped = { mapped_start, mapped_interval }
    -- check if it's a subset
    if overlap_start >= source_start and overlap_end <= source_end then
        return true, mapped, nil
    end

    local unmapped = {}
    -- check if there is lower exclusion
    if overlap_start > seed_start then
        local lower_unmapped = seed_start
        local lower_interval = (overlap_start - seed_start)
        table.insert(unmapped, lower_unmapped)
        table.insert(unmapped, lower_interval)
    end

    if overlap_end < seed_end then
        local upper_unmapped = overlap_end + 1
        local upper_interval = (seed_end - overlap_end)
        table.insert(unmapped, upper_unmapped)
        table.insert(unmapped, upper_interval)
    end

    return true, mapped, unmapped
end

local line = io.read()
line = mysplit(line, ":")[2]
line = string.sub(line, 2)
local seeds = mysplit(line, " ")
_ = io.read()

file = io.read("*all")
local parts = mysplit(file, "\n\n")

local current_lowest = math.huge

for i = 1, #seeds, 2 do
    print("Calculating for seed range " .. seeds[i] .. " to " .. seeds[i] + seeds[i + 1])
    local start = seeds[i]
    local stop = seeds[i] + seeds[i + 1]
    local total = stop - start + 1

    for _, part in ipairs(parts) do
        local map_entries = split(part, "\n")

        while total > 0 do
            for i = 2, #map_entries do
                local entry = map_entries[i]
                local entry_parts = split(entry, " ")
                local did_mapping, mapped, unmapped = map_range(start, stop, entry_parts[1], entry_parts[2],
                    entry_parts[3])
                if did_mapping then 


                --         if in_range then
                --             -- io.write(current_seed_value .. " -> ")
                --             current_seed_value = value
                --             -- io.write(current_seed_value .. "\n")
                --             break
                --         end
            end
        end
    end
    -- if current_seed_value < current_lowest then
    --     current_lowest = current_seed_value
    -- end
end
-- print(current_lowest)
