io.input("input.txt")

local sum = 0
forward_patterns = {
    ["%d+"] = function(s) return tonumber(string.sub(s, 1, 1)) end,
    ["one"] = function() return 1 end,
    ["two"] = function() return 2 end,
    ["three"] = function() return 3 end,
    ["four"] = function() return 4 end,
    ["five"] = function() return 5 end,
    ["six"] = function() return 6 end,
    ["seven"] = function() return 7 end,
    ["eight"] = function() return 8 end,
    ["nine"] = function() return 9 end
}

backward_patterns = {
    ["%d+"] = function(s) return tonumber(string.sub(s, 1, 1)) end,
    ["eno"] = function() return 1 end,
    ["owt"] = function() return 2 end,
    ["eerht"] = function() return 3 end,
    ["ruof"] = function() return 4 end,
    ["evif"] = function() return 5 end,
    ["xis"] = function() return 6 end,
    ["neves"] = function() return 7 end,
    ["thgie"] = function() return 8 end,
    ["enin"] = function() return 9 end
}

function getFirstDigit(number)
    local lowestIndex = math.huge
    local lowestPattern = nil
    local value = nil

    for pattern, value_func in pairs(forward_patterns) do
        local index = string.find(number, pattern)
        if index and index < lowestIndex then
            lowestIndex = index
            lowestPattern = pattern
            value = value_func
        end
    end

    if lowestPattern then
        return value(string.match(number, lowestPattern))
    else
        return nil -- or some default value if no pattern is matched
    end
    -- return value(string.match(number, lowestPattern))
end

function getLastDigit(number)
    local lowestIndex = math.huge
    local lowestPattern = nil
    local value = nil

    local str = string.reverse(number)
    for pattern, value_func in pairs(backward_patterns) do
        local index = string.find(str, pattern)
        if index and index < lowestIndex then
            lowestIndex = index
            lowestPattern = pattern
            value = value_func
        end
    end
    return value(str.match(str, lowestPattern))
end

for line in io.lines() do
    -- local x = string.sub(string.match(line, "%d+"), 1, 1)
    -- local y = string.sub(string.match(string.reverse(line), "%d+"), 1, 1)
    local x = getFirstDigit(line)
    local y = getLastDigit(line)
    print(x, y)
    local pair = x .. y
    sum = sum + tonumber(pair)
end

print(sum)
