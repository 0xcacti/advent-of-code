function loadInput()
    io.input("input.txt")

    local preMap = {}
    for line in io.lines() do
        local row = {}
        for c in line:gmatch(".") do
            table.insert(row, c)
        end
        table.insert(preMap, row)
    end
    return preMap
end

function getUnchangedRows(preMap)
    local unchanged_rows = {}
    for i = 1, #preMap do
        local allDots = true
        for j = 1, #preMap[i] do
            if preMap[i][j] == "#" then
                allDots = false
                break
            end
        end
        if allDots then
            table.insert(unchanged_rows, i)
        end
    end
    return unchanged_rows
end

function getUnchangedCols(preMap)
    local unchanged_cols = {}
    for i = 1, #preMap[1] do
        local allDots = true
        for j = 1, #preMap do
            if preMap[j][i] == "#" then
                allDots = false
                break
            end
        end
        if allDots then
            table.insert(unchanged_cols, i)
        end
    end
    return unchanged_cols
end

function getEmptyRow(len)
    local row = {}
    for i = 1, len do
        table.insert(row, ".")
    end
    return row
end

function printMap(map)
    for i, row in ipairs(map) do
        for j, col in ipairs(row) do
            io.write(col)
        end
        io.write("\n")
    end
end

function updateMap(map)
    local unchanged_rows = getUnchangedRows(map)
    local unchanged_cols = getUnchangedCols(map)
    -- Debug print before modification
    print("Map before modification:")
    printMap(map)

    -- Update columns in-place
    for _, row in ipairs(map) do
        for i = #unchanged_cols, 1, -1 do
            local col = unchanged_cols[i]
            for k = 1, 9 do
                table.insert(row, col, ".")
            end
            -- table.insert(row, col, ".")
        end
    end

    -- Debug print after column modification
    print("Map after column modification:")
    printMap(map)

    for i = #unchanged_rows, 1, -1 do
        local row = unchanged_rows[i]
        local newRow = {}
        for _, v in ipairs(map[row]) do
            table.insert(newRow, v)
        end
        for k = 1, 9 do
            -- table.insert(map, row, getEmptyRow(#newRow))
            table.insert(map, row, newRow)
        end
    end
    print("Map after row modification:")
    printMap(map)
    return map
end

function getGalaxies(map)
    local galaxies = {}
    for i, row in ipairs(map) do
        for j, col in ipairs(row) do
            if col == "#" then
                table.insert(galaxies, { i, j })
            end
        end
    end
    return galaxies
end

function printGalaxies(galaxies)
    for i = 1, #galaxies do
        pair = "(" .. galaxies[i][1] .. "," .. galaxies[i][2] .. ")"
        print(pair)
    end
end

function manhattanDistance(x1, y1, x2, y2, unchanged_rows, unchanged_columns, scale)
    -- calculate the row distance
    local row_distance = 0
    if x1 < x2 then
        for _, rowIndex in ipairs(unchanged_rows) do
            if x1 < rowIndex and rowIndex < x2 then
                row_distance = row_distance + scale - 1
            end
        end
    else
        for _, rowIndex in ipairs(unchanged_rows) do
            if x2 < rowIndex and rowIndex < x1 then
                row_distance = row_distance + scale - 1
            end
        end
    end

    -- calculate the column distance
    local col_distance = 0
    if y1 < y2 then
        for _, colIndex in ipairs(unchanged_columns) do
            if y1 < colIndex and colIndex < y2 then
                col_distance = col_distance + scale - 1
            end
        end
    else
        for _, colIndex in ipairs(unchanged_columns) do
            if y2 < colIndex and colIndex < y1 then
                col_distance = col_distance + scale - 1
            end
        end
    end


    return math.abs(x2 - x1) + row_distance + math.abs(y2 - y1) + col_distance
end

function solve()
    local map = loadInput()

    local unchanged_rows = getUnchangedRows(map)
    local unchanged_cols = getUnchangedCols(map)
    local galaxies = getGalaxies(map)
    local distance = 0
    for i = 1, #galaxies do
        for j = i + 1, # galaxies do
            distance = distance +
                manhattanDistance(galaxies[i][1], galaxies[i][2], galaxies[j][1], galaxies[j][2], unchanged_rows,
                    unchanged_cols, 1000000)
        end
    end
    return distance
end

print(solve())
