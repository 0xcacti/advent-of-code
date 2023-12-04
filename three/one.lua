io.input("input.txt")

function print_grid(grid)
    for _, row in ipairs(grid) do
        for _, c in ipairs(row) do
            io.write(c)
        end
        io.write("\n")
    end
end

local function is_symbol(c)
    return c ~= "." and c ~= "0" and c ~= "1" and c ~= "2" and c ~= "3" and c ~= "4" and c ~= "5" and c ~= "6" and
        c ~= "7" and
        c ~= "8" and c ~= "9"
end


local function check_is_part(grid, x, y)
    -- (x,y)
    -- (x-1,y) (x+1,y)
    -- (x, y-1) (x, y+1)
    -- (x-1,y-1) (x-1,y+1) (x+1,y-1) (x+1,y+1)
    if x > 1 and is_symbol(grid[x - 1][y]) then
        return true
    end

    if x < #grid and is_symbol(grid[x + 1][y]) then
        return true
    end

    if y > 1 and is_symbol(grid[x][y - 1]) then
        return true
    end

    if y < #grid[x] and is_symbol(grid[x][y + 1]) then
        return true
    end

    if x > 1 and y > 1 and is_symbol(grid[x - 1][y - 1]) then
        return true
    end
    if x > 1 and y < #grid[x] and is_symbol(grid[x - 1][y + 1]) then
        return true
    end
    if x < #grid and y > 1 and is_symbol(grid[x + 1][y - 1]) then
        return true
    end
    if x < #grid and y < #grid[x] and is_symbol(grid[x + 1][y + 1]) then
        return true
    end
    return false
end

local function is_number(c)
    return c == "0" or c == "1" or c == "2" or c == "3" or c == "4" or c == "5" or c == "6" or c == "7" or c == "8" or
        c == "9"
end

--
-- local function get_surrounding_number(grid, i, j)
--     -- get start index
--     local start_j = j
--     local end_j = j
--
--
--     while start_j > 1 and is_number(grid[i][start_j]) do
--         start_j = start_j - 1
--     end
--     start_j = start_j + 1
--
--     while end_j <= #grid[i] and is_number(grid[i][end_j]) do
--         end_j = end_j + 1
--     end
--     end_j = end_j - 1
--
--     local number = ""
--     for k = start_j, end_j do
--         number = number .. grid[i][k]
--     end
--     return number, start_j, end_j
--
--     -- get end index
-- end


local function get_surrounding_number(grid, i, j)
    local start_j = j
    while start_j > 1 and is_number(grid[i][start_j - 1]) do
        start_j = start_j - 1
    end

    local end_j = j
    while end_j < #grid[i] and is_number(grid[i][end_j + 1]) do
        end_j = end_j + 1
    end

    local number = ""
    for k = start_j, end_j do
        number = number .. grid[i][k]
    end
    return number, start_j, end_j
end
local grid = {}

for line in io.lines() do
    local row = {}
    for i = 1, #line do
        local c = line:sub(i, i)
        table.insert(row, c)
    end
    table.insert(grid, row)
end

local part_numbers = {}
local i = 1
while i <= #grid do
    local j = 1
    while j <= #grid[i] do
        if is_number(grid[i][j]) and check_is_part(grid, i, j) then
            local num, _, end_j = get_surrounding_number(grid, i, j)
            if num == "" then
                print("Empty num:", "i:", i, "j:", j)
            end
            table.insert(part_numbers, num)
            j = end_j + 1
        else
            j = j + 1
        end
        print("Next j:", j)
    end
    i = i + 1
end


local sum = 0
for _, num in ipairs(part_numbers) do
    if num ~= "" then
        sum = sum + tonumber(num)
    end
    -- sum = sum + tonumber(num)
end
print(sum)
