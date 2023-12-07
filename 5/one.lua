io.input("input.txt")
    it("computes the correct mapping", function() 
        
        seeds = { 79, 14, 55, 13}
        map = "50 98 2" .. "\n" .. "52 50 48"

    })
-- io.input("test_input.txt")

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

function check_in_range(value, dest_low, source_low, interval)
    dest_low = tonumber(dest_low)
    source_low = tonumber(source_low)
    interval = tonumber(interval)
    value = tonumber(value)

    if value < source_low then
        return false, -1
    end
    if value > source_low + interval then
        return false, -1
    end

    return true, dest_low + (value - source_low)
end

local line = io.read()
line = mysplit(line, ":")[2]
line = string.sub(line, 2)
local seeds = mysplit(line, " ")
_ = io.read()

file = io.read("*all")
local parts = mysplit(file, "\n\n")





local current_lowest = math.huge

for _, seed in ipairs(seeds) do
    print("Seed: " .. seed)
    local current_seed_value = seed
    for _, part in ipairs(parts) do
        local map_entries = split(part, "\n")

        for i = 2, #map_entries do
            local entry = map_entries[i]
            local entry_parts = split(entry, " ")
            local in_range, value = check_in_range(current_seed_value, entry_parts[1], entry_parts[2], entry_parts[3])
            if in_range then
                io.write(current_seed_value .. " -> ")
                current_seed_value = value
                io.write(current_seed_value .. "\n")
                break
            end
        end
    end
    if current_seed_value < current_lowest then
        current_lowest = current_seed_value
    end
end
print(current_lowest)
