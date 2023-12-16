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

function contains(table, val)
    for i = 1, #table do
        if mysplit(table[i], " ")[1] == val then
            return true, i
        end
    end
    return false, -1
end

function printBoxes(boxes)
    for k, v in pairs(boxes) do
        print("Box " .. k .. " has:")
        for i, j in ipairs(v) do
            io.write(j .. " ")
        end
    end
end

function solve()
    local inputs = getInput("input.txt")

    local box = {}
    for i = 0, 255 do
        box[i] = {}
    end

    for i, inp in ipairs(inputs) do
        inp = clean(inp)
        e = string.sub(inp, string.len(inp), string.len(inp))

        if e == "-" then
            label = string.sub(inp, 1, string.len(inp) - 1)
            for k, v in ipairs(box[hash(label)]) do
                if mysplit(v, " ")[1] == label then
                    table.remove(box[hash(label)], k)
                end
            end
        else
            local label, value = table.unpack(mysplit(inp, "="))

            entry = label .. " " .. value
            existing = box[hash(label)]

            contained, index = contains(existing, label)
            if contained then
                box[hash(label)][index] = entry
            else
                table.insert(box[hash(label)], entry)
            end
        end
    end

    local ans = 0


    for k, v in pairs(box) do
        for i, j in ipairs(v) do
            value = mysplit(j, " ")[2]
            ans = ans + ((k + 1) * value * i)
        end
    end
    return ans
end

print(solve())
