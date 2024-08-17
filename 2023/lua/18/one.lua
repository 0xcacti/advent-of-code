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
        parts = mysplit(line, " ")
        for i, part in ipairs(parts) do
            if i == 3 then
                part = string.sub(part, 3, 8)
            end
            table.insert(row, part)
        end
        table.insert(inp, row)
    end
    return inp
end

function printInput(input)
    for i, row in ipairs(input) do
        for j, col in ipairs(row) do
            io.write(col .. " ")
        end
        io.write("\n")
    end
end

function printGrid(input)
    for i, row in ipairs(input) do
        for j, col in ipairs(row) do
            io.write(col .. " ")
        end
        io.write("\n")
    end
end

function digOutline(input, grid, maxX, maxY)
    local currX, currY = maxX, maxY
    path = {}
    grid[currX][currY] = "#"
    table.insert(path, { currX, currY, input[1][3] })

    for _, row in ipairs(input) do
        local direction, distance, color = row[1], row[2], row[3]
        print(direction, distance, color)

        if direction == "R" then
            for i = 1, distance do
                currX = currX + 1
                grid[currY][currX] = "#"
                table.insert(path, { currX, currY, color })
            end
        elseif direction == "L" then
            for i = 1, distance do
                currX = currX - 1
                grid[currY][currX] = "#"
                table.insert(path, { currX, currY, color })
            end
        elseif direction == "U" then
            for i = 1, distance do
                currY = currY - 1
                grid[currY][currX] = "#"
                table.insert(path, { currX, currY, color })
            end
        elseif direction == "D" then
            for i = 1, distance do
                currY = currY + 1
                grid[currY][currX] = "#"
                table.insert(path, { currX, currY, color })
            end
        end
    end
    return grid, path
end

function getGrid(maxX, maxY)
    local grid = {}
    for i = 1, maxY do
        local row = {}
        for j = 1, maxX do
            table.insert(row, ".")
        end
        table.insert(grid, row)
    end
    return grid
end

function getDimensions(input)
    local maxX, maxY = 1, 1
    local currX, currY = 1, 1
    for i, row in ipairs(input) do
        local direction, distance, color = row[1], row[2], row[3]
        if direction == "R" then
            currX = currX + distance
            if currX > maxX then
                maxX = currX
            end
        elseif direction == "L" then
            currX = currX - distance
            if currX < 1 then
                maxX = maxX + math.abs(currX) + 1
                currX = 1
            end
        elseif direction == "U" then
            currY = currY - distance
            if currY < 1 then
                maxY = maxY + math.abs(currY) + 1
                currY = 1
            end
        elseif direction == "D" then
            currY = currY + distance
            if currY > maxY then
                maxY = currY
            end
        end
    end
    return maxX, maxY
end

function isInside(node, path)
    local intersectionCount = 0

    for _, edge in ipairs(path) do
        -- Assuming each 'edge' is a table {x1, y1, x2, y2} representing two points on the edge
        if (edge[2] > node[2]) ~= (edge[4] > node[2]) then
            -- Check if the edge crosses the y-coordinate of the node
            local xCross = edge[1] + (node[2] - edge[2]) * (edge[3] - edge[1]) / (edge[4] - edge[2])
            if xCross > node[1] then
                -- Only count if the intersection is to the right of the node
                intersectionCount = intersectionCount + 1
            end
        end
    end

    return intersectionCount % 2 == 1
end

function convertPath(path)
    local edges = {}
    for i = 1, #path do
        local nextIndex = (i % #path) + 1 -- This will loop back to the first point after the last one
        local currentNode = path[i]
        local nextNode = path[nextIndex]
        table.insert(edges, { currentNode[1], currentNode[2], nextNode[1], nextNode[2] })
    end
    return edges
end

function digInised(path, grid)
    for i, row in ipairs(grid) do
        for j, col in ipairs(row) do
            if isInside({ j, i }, path) then
                grid[i][j] = "#"
            end
        end
    end
    return grid
end

function getCount(grid)
    local count = 0
    for i, row in ipairs(grid) do
        for j, col in ipairs(row) do
            if col == "#" then
                count = count + 1
            end
        end
    end
    return count
end

function solve(path)
    local inputs = getInput(path)
    printInput(inputs)
    print()
    local maxX, maxY = getDimensions(inputs)
    print(maxX, maxY)
    print()
    local grid = getGrid(1000, 1000)
    grid, path = digOutline(inputs, grid, maxX, maxY)
    edges = convertPath(path)
    grid = digInised(edges, grid)
    printGrid(grid)
    return getCount(grid)
end

print(solve("input.txt"))
