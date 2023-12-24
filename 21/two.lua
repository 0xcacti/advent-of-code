require("queue")
require("set")

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
    local rowIndex = 0 -- Start from 0 for rows
    for line in io.lines() do
        local row = {}
        local columnIndex = 0 -- Start from 0 for columns
        for c in line:gmatch(".") do
            row[columnIndex] = c
            columnIndex = columnIndex + 1
        end
        inp[rowIndex] = row
        rowIndex = rowIndex + 1
    end
    return inp
end

function tableLength(table)
    local count = 0
    for _ in pairs(table) do
        count = count + 1
    end
    return count
end

function printInput(input)
    for i, row in ipairs(input) do
        for j, col in ipairs(row) do
            io.write(col .. " ")
        end
        io.write("\n")
    end
end

function findStart(input)
    for i = 0, tableLength(input) - 1 do
        for j = 0, tableLength(input[i]) - 1 do
            if input[i][j] == "S" then
                return { i, j }
            end
        end
    end
end

function contains(list, element)
    for _, value in ipairs(list) do
        if value == element then
            return true
        end
    end
    return false
end

function fill(inputs, startRow, startCol, steps)
    local visited = Set.new()
    visited:add(startRow .. ";" .. startCol)

    local queue = Queue.new()
    queue:pushBack({ startRow, startCol, steps })

    local answer = Set.new()

    while not queue:empty() do
        local entry = queue:popFront()
        local row, col, steps = entry[1], entry[2], entry[3]

        if steps % 2 == 0 then
            answer:add(row .. ";" .. col)
        end

        if steps == 0 then
            goto qtinue
        end

        local nextSteps = { { row + 1, col }, { row - 1, col }, { row, col + 1 }, { row, col - 1 } }
        for _, step in ipairs(nextSteps) do
            local nextRow, nextCol = step[1], step[2]
            local key = nextRow .. ";" .. nextCol
            if nextRow < 0 or nextRow >= tableLength(inputs) or nextCol < 0 or nextCol >= tableLength(inputs[nextRow]) or inputs[nextRow][nextCol] == "#" or visited:contains(key) then
                goto continue
            end
            visited:add(key)
            queue:pushBack({ nextRow, nextCol, steps - 1 })
            ::continue::
        end
        ::qtinue::
    end

    return answer:size()
end

function solve(path)
    input = getInput(path)
    s = findStart(input)
    steps = 26501365
    gridSize = tableLength(input)
    gridWidth = math.floor(steps / gridSize) - 1
    print(gridWidth)

    odd = math.pow(math.floor(gridWidth / 2) * 2 + 1, 2)
    even = math.pow(math.floor((gridWidth + 1) / 2) * 2, 2)
    print(odd .. " " .. even)

    -- Assuming fill is a function that needs to be defined or imported
    local oddPoints = fill(input, s[1], s[2], gridSize * 2 + 1)
    local evenPoints = fill(input, s[1], s[2], gridSize * 2)
    print(gridSize)
    print(oddPoints .. " " .. evenPoints)


    local cornerT = fill(input, gridSize - 1, s[2], gridSize - 1)
    local cornerR = fill(input, s[1], 0, gridSize - 1)
    local cornerB = fill(input, 0, s[2], gridSize - 1)
    local cornerL = fill(input, s[1], gridSize - 1, gridSize - 1)

    local halfSize = math.floor(gridSize / 2)
    local smallTR = fill(input, gridSize - 1, 0, halfSize - 1)
    local smallTL = fill(input, gridSize - 1, gridSize - 1, halfSize - 1)
    local smallBR = fill(input, 0, 0, halfSize - 1)
    local smallBL = fill(input, 0, gridSize - 1, halfSize - 1)
    print(smallTR .. " " .. smallTL .. " " .. smallBR .. " " .. smallBL)

    local oneAndHalfSize = math.floor(gridSize * 3 / 2)
    local largeTR = fill(input, gridSize - 1, 0, oneAndHalfSize - 1)
    local largeTL = fill(input, gridSize - 1, gridSize - 1, oneAndHalfSize - 1)
    local largeBR = fill(input, 0, 0, oneAndHalfSize - 1)
    local largeBL = fill(input, 0, gridSize - 1, oneAndHalfSize - 1)
    print(largeTR .. " " .. largeTL .. " " .. largeBR .. " " .. largeBL)

    return odd * oddPoints + even * evenPoints + cornerT + cornerR + cornerB + cornerL +
        (gridWidth + 1) * (smallTR + smallTL + smallBR + smallBL) + gridWidth * (largeTR + largeTL + largeBR + largeBL)
end

s = solve("input.txt")
print(tostring(s))
print(string.format("%.14f", s))
