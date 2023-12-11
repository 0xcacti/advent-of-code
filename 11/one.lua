io.input("test_input.txt")

local preMap = {}
for line in io.lines() do
    for c in line:gmatch(".") do
        table.insert(preMap, c)
    end
end

local unchanged_cols = {}
local unchanged_rows = {}
for i = 1, #preMap do
    for j = 1, #preMap[i] do
        if preMap[i][j] ~= "#" then
            goto next
        end
        table.insert(unchanged_cols, i)
        table.insert(unchanged_rows, j)
    end
    ::next::
end
