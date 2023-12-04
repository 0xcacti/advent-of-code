io.input("input.txt")

local function is_number(c)
    return c == "0" or c == "1" or c == "2" or c == "3" or c == "4" or c == "5" or c == "6" or c == "7" or c == "8" or
        c == "9"
end


local function is_symbol(c)
    return c ~= "." and c ~= "0" and c ~= "1" and c ~= "2" and c ~= "3" and c ~= "4" and c ~= "5" and c ~= "6" and
        c ~= "7" and
        c ~= "8" and c ~= "9"
end

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

local function check_is_gear(grid, x, y)
    -- (x,y)
    -- (x-1,y) (x+1,y)
    -- (x, y-1) (x, y+1)
    -- (x-1,y-1) (x-1,y+1) (x+1,y-1) (x+1,y+1)
    -- I need to check each of the adjacent squares to see if they are numbers
    -- I also need a methodto account for the locations that I have checked
    -- I also need to verify that the are only 2 numbers that in touches, no more, no less

    -- I need to develop a system to see which spaces are occupied
    -- I need to check
    -- check top row

    -- X X X
    -- X * X
    -- X X X

    local touched_numbers = {}
    local top_row_finished = false
    local bottom_row_finished = false

    if x > 1 and y > 1 and is_number(grid[x - 1][y - 1]) then
        local num, _, e = get_surrounding_number(grid, x - 1, y - 1)
        if y + 1 <= e then
            table.insert(touched_numbers, num)
            top_row_finished = true
        else
            if y <= e then
                table.insert(touched_numbers, num)
                top_row_finished = true
            else
                table.insert(touched_numbers, num)
            end
        end
    end

    if not top_row_finished then
        if x > 1 and is_number(grid[x - 1][y]) then
            local num, _, e = get_surrounding_number(grid, x - 1, y)
            if y + 1 <= e then
                table.insert(touched_numbers, num)
                top_row_finished = true
            else
                table.insert(touched_numbers, num)
            end
        end
    end

    if not top_row_finished then
        if x > 1 and y < #grid[x] and is_number(grid[x - 1][y + 1]) then
            local num, _, _ = get_surrounding_number(grid, x - 1, y + 1)
            table.insert(touched_numbers, num)
            top_row_finished = true
        end
    end
    --------

    if y > 1 and is_number(grid[x][y - 1]) then
        local num, _, _ = get_surrounding_number(grid, x, y - 1)
        table.insert(touched_numbers, num)
    end

    if y < #grid[x] and is_number(grid[x][y + 1]) then
        local num, _, _ = get_surrounding_number(grid, x, y + 1)
        table.insert(touched_numbers, num)
    end

    --------
    if x < #grid and y > 1 and is_number(grid[x + 1][y - 1]) then
        local num, _, e = get_surrounding_number(grid, x + 1, y - 1)
        if y + 1 <= e then
            table.insert(touched_numbers, num)
            bottom_row_finished = true
        else
            if y <= e then
                table.insert(touched_numbers, num)
                bottom_row_finished = true
            else
                table.insert(touched_numbers, num)
            end
        end
    end

    if not bottom_row_finished then
        if x < #grid and is_number(grid[x + 1][y]) then
            local num, _, e = get_surrounding_number(grid, x + 1, y)
            if y + 1 <= e then
                table.insert(touched_numbers, num)
                bottom_row_finished = true
            else
                table.insert(touched_numbers, num)
            end
        end
    end


    if not bottom_row_finished then
        if x < #grid and y < #grid[x] and is_number(grid[x + 1][y + 1]) then
            local num, _, _ = get_surrounding_number(grid, x + 1, y + 1)
            table.insert(touched_numbers, num)
            bottom_row_finished = true
        end
    end

    if #touched_numbers == 2 then
        local gear_ratio = 1
        for _, num in ipairs(touched_numbers) do
            gear_ratio = gear_ratio * tonumber(num)
        end
        return true, gear_ratio
    else
        return false, 0
    end
end

local function is_asterisk(c)
    return c == "*"
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

local gear_ratios = {}
local i = 1
while i <= #grid do
    local j = 1
    while j <= #grid[i] do
        print(i, j)
        if is_asterisk(grid[i][j]) then
            local is_gear, ratio = check_is_gear(grid, i, j)
            if is_gear then
                table.insert(gear_ratios, ratio)
            end
        end
        j = j + 1
    end
    i = i + 1
end


local sum = 0
for _, num in ipairs(gear_ratios) do
    if num ~= "" then
        sum = sum + tonumber(num)
    end
    -- sum = sum + tonumber(num)
end
print(sum)
