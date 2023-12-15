function loadPuzzle()
    io.input("test_input.txt")
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

function rotate(puzzle)
    local t = {}
    local n = #puzzle
    local m = #puzzle[1]

    for i = 1, m do
        t[i] = {}
        for j = 1, n do
            t[i][j] = puzzle[n - j + 1][i]
        end
    end
    return t
end

function tiltNorth(puzzle)
    for i = 1, #puzzle[1] do -- Iterating over each column
        local column = translateColumn(puzzle, i)
        local lastValid = #column

        for j = #column, 1, -1 do
            if column[j] == "O" then
                -- Move "O" upwards in the column
                column[j] = "."
                column[lastValid] = "O"
                lastValid = lastValid - 1
            elseif column[j] == "#" then
                lastValid = j - 1
            end
        end


        -- Apply the updated column back to the puzzle grid
        for j = 1, #column do
            puzzle[#puzzle - j + 1][i] = column[j]
        end
    end
    return puzzle
end

function inTable(t, e)
    for i = 1, #t do
        if t[i] == e then
            return true
        end
    end
    return false
end

function puzzleToString(puzzle)
    local s = ""
    for i = 1, #puzzle do
        for j = 1, #puzzle[i] do
            s = s .. puzzle[i][j]
        end
    end
    return s
end

function stringToPuzzle(s, len)
    local puzzle = {}
    local row = {}
    for i = 1, #s do
        if i % length == 0 then
            table.insert(puzzle, row)
            row = {}
        end
        table.insert(row, s:sub(i, i))
    end
    return puzzle
end

function printPuzzle(puzzle)
    for i = 1, #puzzle do
        for j = 1, #puzzle[i] do
            io.write(puzzle[i][j])
        end
        io.write("\n")
    end
end

function getSum(column)
    -- print(table.concat(column))
    local sum = 0
    for i = #column, 1, -1 do
        if column[i] == "O" then
            sum = sum + i
        end
    end
    return sum
end

function getLoopStartIndex(cache)
    for i = 1, #cache do
        for j = 2, #cache do
            if cache[i] == cache[j] then
                return i
            end
        end
    end
    return -1
end

function solve(puzzle)
    local cache = {}
    local remainder = 0
    for i = 1, 1000000000 do
        for j = 1, 4 do
            puzStr = puzzleToString(puzzle)
            if inTable(cache, puzStr) then
                print("Found a loop at iteration " .. i)
                loopStartIndex = getLoopStartIndex(cache)
                loopLength = i - loopStartIndex
                remainingCycles = 1000000000 - loopStartIndex
                indexInLoop = (remainingCycles - 1) % (loopLength)
                remainder = loopStartIndex + indexInLoop
                goto done
            else
                table.insert(cache, puzStr)
            end
            puzzle = tiltNorth(puzzle)
            puzzle = rotate(puzzle)
        end
    end
    ::done::
    -- for i = 1, remainder do
    --     puzzle = tiltNorth(puzzle)
    --     rotate(puzzle)
    -- end
    length = #puzzle[1]
    puzzle = stringToPuzzle(cache[remainder], length)
    printPuzzle(puzzle)
    local sum = 0
    for i = 1, #puzzle[1] do
        local column = translateColumn(puzzle, i)
        sum = sum + getSum(column)
    end
    print(sum)
end

solve(loadPuzzle())
