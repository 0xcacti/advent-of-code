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
    for line in io.lines() do
        row = {}
        for c in line:gmatch(".") do
            table.insert(row, c)
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

function findStart(input)
    for i, row in ipairs(input) do
        for j, col in ipairs(row) do
            if col == "S" then
                return { j, i }
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
    -- create list of visited
    visited = {}
    visited[startRow .. "," .. startCol] = true


    -- create queue and push start
    queue = Queue.new()
    queue:pushBack({ startRow, startCol, steps })

    answer = Set.new()


    while not queue:empty() do
        local entry = queue:popFront()
        row, col, steps = entry[1], entry[2], entry[3]

        if steps % 2 == 0 then
            answer:add(row .. "," .. col)
        end

        if steps == 0 then
            goto qtinue
        end


        nextSteps = { { row + 1, col }, { row - 1, col }, { row, col + 1 }, { row, col - 1 } }
        for _, step in ipairs(nextSteps) do
            nextRow, nextCol = step[1], step[2]

            key = nextRow .. "," .. nextCol
            if nextRow < 1 or nextRow > #inputs or nextCol < 1 or nextCol > #inputs[1] or visited[key] or inputs[nextRow][nextCol] == "#" then
                goto continue
            end
            visited[key] = true

            queue:pushBack({ nextRow, nextCol, steps - 1 })
            ::continue::
        end
        ::qtinue::
    end

    count = 0
    for k, v in pairs(answer) do
        count = count + 1
    end

    return count
end

function solve(path)
    input = getInput(path)
    s = findStart(input)
    return fill(input, s[1], s[2], 64)
end

print(solve("input.txt"))
