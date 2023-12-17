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

function getInput(path)
    io.input(path)
    local inp = {}
    for line in io.lines() do
        local row = {}
        for c in line:gmatch(".") do
            table.insert(row, c)
        end
        table.insert(inp, row)
    end
    return inp
end

function printInput(inp)
    for _, row in ipairs(inp) do
        for _, c in ipairs(row) do
            io.write(c)
        end
        io.write("\n")
    end
end

function trackLight(grid, x, y, dx, dy, visited, count, path)
    print("x: " .. x .. " y: " .. y .. " dx: " .. dx .. " dy: " .. dy)
    if x < 1 or x > #grid[1] or y < 1 or y > #grid then
        return count
    end
    key = x .. "," .. y
    if path[key] then
        -- Detected a cycle, so terminate this path
        return count
    else
        path[key] = true
    end

    if not visited[key] then
        visited[key] = true
        count[1] = count[1] + 1
    end



    next = grid[y][x]

    if next == "." then
        trackLight(grid, x + dx, y + dy, dx, dy, visited, count)
    elseif next == "-" then
        if dy == 1 then
            -- split to go left and right
            count = trackLight(grid, x + 1, y, 1, 0, visited, count)
            return trackLight(grid, x - 1, y, -1, 0, visited, count)
        elseif dy == -1 then
            count = trackLight(grid, x + 1, y, 1, 0, visited, count)
            return trackLight(grid, x - 1, y, -1, 0, visited, count)
        else
            trackLight(grid, x + dx, y + dy, dx, dy, visited, count)
        end
    elseif next == "|" then
        if dx == 1 then
            -- split to go up and down
            count = trackLight(grid, x, y + 1, 0, 1, visited, count)
            return trackLight(grid, x, y - 1, 0, -1, visited, count)
        elseif dx == -1 then
            count = trackLight(grid, x, y + 1, 0, 1, visited, count)
            return trackLight(grid, x, y - 1, 0, -1, visited, count)
        else
            trackLight(grid, x + dx, y + dy, dx, dy, visited, count)
        end
    elseif next == "/" then
        if dx == 1 then
            trackLight(grid, x, y - 1, 0, -1, visited, count)
        elseif dx == -1 then
            trackLight(grid, x, y + 1, 0, 1, visited, count)
        elseif dy == 1 then
            trackLight(grid, x - 1, y, -1, 0, visited, count)
        elseif dy == -1 then
            trackLight(grid, x + 1, y, 1, 0, visited, count)
        end
    elseif next == "\\" then
        if dx == 1 then
            trackLight(grid, x, y + 1, 0, 1, visited, count)
        elseif dx == -1 then
            trackLight(grid, x, y - 1, 0, -1, visited, count)
        elseif dy == 1 then
            trackLight(grid, x + 1, y, 1, 0, visited, count)
        elseif dy == -1 then
            trackLight(grid, x - 1, y, -1, 0, visited, count)
        end
    end
    return count
end

function solve(path)
    local inputs = getInput(path)
    printInput(inputs)
    local visited = {}
    local count = { 0 }
    print(inputs[9][1])
    print()
    count = trackLight(inputs, 1, 1, 1, 0, visited, count)
    return count[1]
end

print(solve("test_input.txt"))
