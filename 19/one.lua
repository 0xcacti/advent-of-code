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

function getName(str)
    for i = 1, string.len(str) do
        if string.sub(str, i, i) == "{" then
            return string.sub(str, 1, i - 1), i
        end
    end
    return nil, -1
end

function getInput(path)
    io.input(path)
    local source = io.read("*all")
    lines = mysplit(source, "\n")

    local inputs = {}
    local rules = {}

    for _, line in ipairs(lines) do
        firstChar = line:sub(1, 1)
        if firstChar == "{" then
            line = string.sub(line, 2, string.len(line) - 1)
            parts = mysplit(line, ",")
            local entry = {}
            for _, part in ipairs(parts) do
                pieces = mysplit(part, "=")
                entry[pieces[1]] = pieces[2]
            end
            table.insert(inputs, entry)
        else
            local name, index = getName(line)
            if name == nil then
                print("Error: no name found")
                goto continue
            end
            local parts = mysplit(string.sub(line, index + 1, string.len(line) - 1), ",")
            rules[name] = parts
        end
        ::continue::
    end
    return inputs, rules
end

function printRules(rules)
    print(rules)
    for name, val in pairs(rules) do
        print("Rule: " .. name)
        for _, parts in ipairs(val) do
            io.write(parts)
            io.write(" ")
        end
        io.write("\n")
    end
end

function printInputs(inputs)
    for _, inp in ipairs(inputs) do
        print("Input:")
        for key, value in pairs(inp) do
            print(key .. " = " .. value)
        end
    end
end

function filterInput(input, rules)
    start = "in"
    print("working out input")
    while start ~= "A" and start ~= "R" do
        print(start)
        rule = rules[start]


        for _, part in ipairs(rule) do
            print(part)
            local sections = mysplit(part, ":")
            if #sections == 1 then
                start = sections[1]
            else
                local law = sections[1]
                local value = sections[2]
                local compKey = string.sub(law, 1, 1)
                local compOp = string.sub(law, 2, 2)
                local compVal = tonumber(string.sub(law, 3, string.len(law)))
                if compOp == "<" then
                    inputValue = tonumber(input[compKey])
                    print(inputValue .. " < " .. compVal)
                    if inputValue < compVal then
                        start = value
                        goto continue
                    end
                elseif compOp == ">" then
                    inputValue = tonumber(input[compKey])
                    if inputValue > compVal then
                        start = value
                        goto continue
                    end
                elseif compOp == "=" then
                    inputValue = tonumber(input[compKey])
                    if inputValue == compVal then
                        start = value
                        goto continue
                    end
                else
                    print("Error: invalid comparison operator")
                end
                -- print("Error: no rule found")
            end
        end
        ::continue::
    end
    return start
end

function sort(inputs, rules)
    accepted = 0
    rejected = 0
    for _, input in ipairs(inputs) do
        if filterInput(input, rules) == "A" then
            for key, value in pairs(input) do
                accepted = accepted + tonumber(value)
            end
        else
            rejected = rejected + 1
        end
    end
    return accepted, rejected
end

function solve(path)
    local inputs, rules = getInput(path)

    -- return filterInput(inputs[3], rules)

    accepted, rejected = sort(inputs, rules)
    return accepted, rejected
end

print(solve("input.txt"))
