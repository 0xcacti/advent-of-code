function hash(input)
    value = 0
    for i = 1, string.len(input) do
        char = string.sub(input, i, i)
        b = string.byte(char)
        value = value + b
        value = value * 17
        value = value % 256
    end
    return value
end

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
    local inp = io.read("*all")
    inputs = mysplit(inp, ",")
    return inputs
end

function clean(input)
    local cleanedInput = input:gsub("\n", "")
    return cleanedInput
end

function solve()
    local inputs = getInput("test_input.txt")

    local box = {}
    for i, inp in ipairs(inputs) do
        label = inp:sub(1, 2)
        operator = inp:sub(3, 3)
        value = inp:sub(4, 4)
        if operator == "=" then
            box[hash(label)] = value
        end
    end
end

print(solve())
