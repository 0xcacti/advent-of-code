io.input("input.txt")

total_red = 12
total_green = 13
total_blue = 14


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

function print_round(round)
    for i, r in ipairs(round) do
        print(round[i])
    end
end

function get_count(round)
    local red = 0.0
    local green = 0.0
    local blue = 0.0

    local colors = mysplit(round, ",")
    for i, color in ipairs(colors) do
        if string.match(color, "red") then
            local red_value = tonumber(string.match(color, "%d+"))
            if red_value then
                red = red_value
            end
        end
        if string.match(color, "green") then
            local green_value = tonumber(string.match(color, "%d+"))
            if green_value then
                green = green_value
            end
        end
        if string.match(color, "blue") then
            local blue_value = tonumber(string.match(color, "%d+"))
            if blue_value then
                blue = blue_value
            end
        end
    end
    return red, green, blue
end

function is_possible(red, green, blue)
    if red == 0 and greed == 0 and blue == 0 then
        return true
    end
    if red > total_red or green > total_green or blue > total_blue then
        return false
    end
    return true
end

possible_ids = {}
game_id = 1
for game in io.lines() do
    local start = string.find(game, ":")
    game = string.sub(game, tonumber(start) + 2)
    rounds = mysplit(game, ";")

    local possible = true
    for i, round in ipairs(rounds) do
        local red, green, blue = get_count(round)
        possible = possible and is_possible(red, green, blue)
    end
    if possible then
        table.insert(possible_ids, game_id)
    end
    game_id = game_id + 1
end


sum = 0
for i, id in ipairs(possible_ids) do
    sum = sum + id
end
print(sum)
