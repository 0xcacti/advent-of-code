function loadPuzzle()
    io.input("input.txt")
    local puzzle = {}
    for line in io.lines() do
        local row = {}
        for c in line:gmatch(".") do
            table.insert(row, c)
        end
        table.insert(puzzle, row)
    end
    return puzzle
end

function translateColumn(puzzle, column)
    local translation = {}
    for i = #puzzle, 1, -1 do
        table.insert(translation, puzzle[i][column])
    end
    return translation
end

function getSum(column)
    -- print(table.concat(column))
    local sum = 0
    local lastValid = #column
    for i = #column, 1, -1 do
        if column[i] == "O" then
            print("circ")
            if i <= lastValid then
                print(lastValid)
                sum = sum + lastValid
                lastValid = lastValid - 1
            end
        end
        if column[i] == "#" then
            lastValid = i - 1
        end
    end
    return sum
end

function solve(puzzle)
    local sum = 0
    for i = 1, #puzzle[1] do
        local column = translateColumn(puzzle, i)
        local columnSum = getSum(column)
        sum = sum + columnSum
    end
    return sum
end

-- print(getSum({ "O", ".", ".", ".", ".", "#", ".", ".", ".", "." }))

print(solve(loadPuzzle()))
-- print(getSum({ "#", "#", ".", ".", "O", ".", "O", ".", "O", "O" }))
