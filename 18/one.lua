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

function solve(path)
    local inputs = getInput(path)
    printInput(inputs)
end

print(solve("test_input.txt"))
