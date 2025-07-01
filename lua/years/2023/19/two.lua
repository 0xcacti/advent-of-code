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
            end
            local subRules = {}
            local alternative = {}
            local parts = mysplit(string.sub(line, index + 1, string.len(line) - 1), ",")
            for _, part in ipairs(parts) do
                local sections = mysplit(part, ":")

                if #sections == 1 then
                    table.insert(alternative, sections[1])
                else
                    local law = sections[1]
                    local value = sections[2]

                    local compKey = string.sub(law, 1, 1)
                    local compOp = string.sub(law, 2, 2)
                    local compVal = tonumber(string.sub(law, 3, string.len(law)))
                    table.insert(subRules, { compKey, compOp, compVal, value })
                end
            end
            rules[name] = { subRules, alternative }
        end
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

function deepCopy(original)
    local copy
    if type(original) == 'table' then
        copy = {}
        for key, value in next, original, nil do
            copy[deepCopy(key)] = deepCopy(value)
        end
    else -- for numbers, strings, booleans, etc.
        copy = original
    end
    return copy
end

function findCombinations(ranges, workflows, name)
    if name == "R" then
        return 0
    end
    if name == "A" then
        product = 1
        for key, val in pairs(ranges) do
            local lo, hi = val[1], val[2]
            print(lo, hi)
            product = product * (hi - lo + 1)
        end
        return product
    end

    local rules, alternative = workflows[name][1], workflows[name][2]

    total = 0


    complete = true
    for _, rule in ipairs(rules) do
        local key, cmp, n, target = rule[1], rule[2], rule[3], rule[4]
        print("Rule: " .. key .. " " .. cmp .. " " .. n .. " " .. target)
        local lo, hi = tonumber(ranges[key][1]), tonumber(ranges[key][2])
        local accepted, rejected
        if cmp == "<" then
            accepted = { lo, math.min(n - 1, hi) }
            rejected = { math.max(lo, n), hi }
        else
            accepted = { math.max(n + 1, lo), hi }
            rejected = { lo, math.min(n, hi) }
        end
        if accepted[1] <= accepted[2] then
            copy = deepCopy(ranges)
            copy[key] = accepted
            total = total + findCombinations(copy, workflows, target)
        end
        if rejected[1] <= rejected[2] then
            ranges = deepCopy(ranges)
            ranges[key] = rejected
        else
            complete = false
            break
        end
    end
    if complete then
        for _, alt in ipairs(alternative) do
            total = total + findCombinations(ranges, workflows, alt)
        end
    end


    return total
end

function solve(path)
    local inputs, rules = getInput(path)
    local ranges = {}
    ranges["x"] = { 1, 4000 }
    ranges["m"] = { 1, 4000 }
    ranges["a"] = { 1, 4000 }
    ranges["s"] = { 1, 4000 }

    return findCombinations(ranges, rules, "in")
end

print(solve("input.txt"))
