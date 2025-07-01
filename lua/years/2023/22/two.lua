require("set")
require("deque")
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
        line = string.gsub(line, "~", ",")
        parts = mysplit(line, ",")
        numericParts = {}
        for _, part in ipairs(parts) do
            table.insert(numericParts, tonumber(part))
        end
        table.insert(inp, numericParts)
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

function contains(list, element)
    for _, value in ipairs(list) do
        if value == element then
            return true
        end
    end
    return false
end

function overlap(a, b)
    -- check x and y intersect
    return math.max(a[1], b[1]) <= math.min(a[4], b[4]) and math.max(a[2], b[2]) <= math.min(a[5], b[5])
end

function drop(bricks)
    for i = 1, #bricks do
        minZ = 1
        brick = bricks[i]
        for j = 1, i - 1 do
            nextBrick = bricks[j]
            if overlap(brick, nextBrick) then
                minZ = math.max(nextBrick[6] + 1, minZ)
            end
        end
        brick[6] = brick[6] - (brick[3] - minZ)
        brick[3] = minZ
    end
    return bricks
end

function getSupports(bricks)
    ksv = {}
    vsk = {}
    for i = 1, #bricks do
        ksv[i] = Set:new()
        vsk[i] = Set:new()
    end

    for j = 1, #bricks do
        upper = bricks[j]
        for i = 1, j - 1 do
            lower = bricks[i]
            if overlap(upper, lower) and upper[3] == lower[6] + 1 then
                ksv[i]:add(j)
                vsk[j]:add(i)
            end
        end
    end
    return ksv, vsk
end

function printSupport(list)
    for k, set in pairs(list) do
        io.write(k .. ": ")
        for _, value in ipairs(set:elements()) do
            io.write(value .. " ")
        end
        io.write("\n")
    end
end

function setSubtraction(elements, subtractSet)
    local result = {}
    for _, element in ipairs(elements) do
        if not subtractSet:contains(element) then
            table.insert(result, element)
        end
    end
    return result
end

function isSubset(setA, setB)
    for element in pairs(setA._elements) do
        if not setB:contains(element) then
            return false
        end
    end
    return true
end

function solve(path)
    -- parse input
    local inputs = getInput(path)
    table.sort(inputs, function(a, b)
        return a[3] < b[3]
    end)
    dropped = drop(inputs)

    table.sort(dropped, function(a, b)
        return a[3] < b[3]
    end)



    -- printInput(inputs)
    ksv, vsk = getSupports(dropped)
    local total = 0

    for i = 1, #dropped do
        q = Deque:new()
        for _, j in ipairs(ksv[i]:elements()) do
            if vsk[j]:size() == 1 then
                q:pushBack(j)
            end
        end
        falling = Set.new()
        local index = q._first
        while index <= q._last do
            falling:add(q[index])
            index = index + 1
        end

        falling:add(i)

        while not q:empty() do
            j = q:popFront()
            elements = setSubtraction(ksv[j]:elements(), falling)
            for _, elem in ipairs(elements) do
                if isSubset(vsk[elem], falling) then
                    q:pushBack(elem)
                    falling:add(elem)
                end
            end
        end
        total = total + falling:size() - 1
    end


    print(total)
end

s = solve("input.txt")
