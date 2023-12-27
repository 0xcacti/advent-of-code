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

function contains(t, e)
    for _, v in ipairs(t) do
        if v == e then
            return true
        end
    end
    return false
end

function solve(path)
    local inputs = getInput(path)
end

s = solve("test_input.txt")
print(s)
