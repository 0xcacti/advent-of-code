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
    local inputs = getInput("input.txt")
    local sum = 0
    for i, inp in ipairs(inputs) do
        h = hash(clean(inp))
        sum = sum + h
    end
    return sum
end

print(solve())
