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



    local pathKey = x .. "," .. y .. "," .. dx .. "," .. dy
    local visitKey = x .. "," .. y

    if path[pathKey] then
        return count
    else
        path[pathKey] = true
    end

    if not visited[visitKey] then
        visited[visitKey] = true
        count[1] = count[1] + 1
    end


    next = grid[y][x]

    if next == "." then
        trackLight(grid, x + dx, y + dy, dx, dy, visited, count, path)
    elseif next == "-" then
        if dy ~= 0 then
            count = trackLight(grid, x + 1, y, 1, 0, visited, count, path)
            count = trackLight(grid, x - 1, y, -1, 0, visited, count, path)
        else
            trackLight(grid, x + dx, y + dy, dx, dy, visited, count, path)
        end
    elseif next == "|" then
        if dx ~= 0 then
            -- split to go up and down
            count = trackLight(grid, x, y + 1, 0, 1, visited, count, path)
            count = trackLight(grid, x, y - 1, 0, -1, visited, count, path)
        else
            trackLight(grid, x + dx, y + dy, dx, dy, visited, count, path)
        end
    elseif next == "/" then
        if dx == 1 then
            trackLight(grid, x, y - 1, 0, -1, visited, count, path)
        elseif dx == -1 then
            trackLight(grid, x, y + 1, 0, 1, visited, count, path)
        elseif dy == 1 then
            trackLight(grid, x - 1, y, -1, 0, visited, count, path)
        elseif dy == -1 then
            trackLight(grid, x + 1, y, 1, 0, visited, count, path)
        end
    elseif next == "\\" then
        if dx == 1 then
            trackLight(grid, x, y + 1, 0, 1, visited, count, path)
        elseif dx == -1 then
            trackLight(grid, x, y - 1, 0, -1, visited, count, path)
        elseif dy == 1 then
            trackLight(grid, x + 1, y, 1, 0, visited, count, path)
        elseif dy == -1 then
            trackLight(grid, x - 1, y, -1, 0, visited, count, path)
        end
    end
    return count
end

function solve(map)
    local inputs = getInput(map)

    local counts = {}

    -- Loop for top row (y = 1, varying x)
    for x = 1, #inputs[1] do
        local visited = {}
        local path = {}
        local count = { 0 }
        count = trackLight(inputs, x, 1, 0, 1, visited, count, path)
        table.insert(counts, count[1])
    end

    -- Loop for bottom row (y = #inputs, varying x)
    for x = 1, #inputs[1] do
        local visited = {}
        local path = {}
        local count = { 0 }
        count = trackLight(inputs, x, #inputs, 0, -1, visited, count, path)
        table.insert(counts, count[1])
    end

    -- Loop for left column (x = 1, varying y)
    for y = 1, #inputs do
        local visited = {}
        local path = {}
        local count = { 0 }
        count = trackLight(inputs, 1, y, 1, 0, visited, count, path)
        table.insert(counts, count[1])
    end

    -- Loop for right column (x = #inputs[1], varying y)
    for y = 1, #inputs do
        local visited = {}
        local path = {}
        local count = { 0 }
        count = trackLight(inputs, #inputs[1], y, -1, 0, visited, count, path)
        table.insert(counts, count[1])
    end


    local max = 0
    for _, v in ipairs(counts) do
        if v > max then
            max = v
        end
    end
    return max
end

print(solve("input.txt"))
