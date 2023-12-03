io.input("input.txt")


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

function update_counts(round, min_red, min_green, min_blue)
    local colors = mysplit(round, ",")

    for i, color in ipairs(colors) do
        if string.match(color, "red") then
            local red_value = tonumber(string.match(color, "%d+"))
            if red_value then
                if red_value > min_red then
                    min_red = red_value
                end
            end
        end
        if string.match(color, "green") then
            local green_value = tonumber(string.match(color, "%d+"))
            if green_value then
                if green_value > min_green then
                    min_green = green_value
                end
            end
        end
        if string.match(color, "blue") then
            local blue_value = tonumber(string.match(color, "%d+"))
            if blue_value then
                if blue_value > min_blue then
                    min_blue = blue_value
                end
            end
        end
    end
    return min_red, min_green, min_blue
end

local game_id = 1
local powers = {}
for game in io.lines() do
    local start = string.find(game, ":")
    game = string.sub(game, tonumber(start) + 2)
    local rounds = mysplit(game, ";")


    local min_red = 0
    local min_green = 0
    local min_blue = 0

    for i, round in ipairs(rounds) do
        min_red, min_green, min_blue = update_counts(round, min_red, min_green, min_blue)
    end

    local power = min_red * min_green * min_blue
    table.insert(powers, power)
    game_id = game_id + 1
end


local sum = 0
for i, power in ipairs(powers) do
    sum = sum + power
end

print(sum)
