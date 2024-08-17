io.input("input.txt")
-- io.input("input_test.txt")

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

function contains(table, element)
    for _, value in pairs(table) do
        if value == element then
            return true
        end
    end
    return false
end

function get_match_count(line)
    local parts = mysplit(line, "|")
    local winners = mysplit(parts[1], " ")
    local my_numbers = mysplit(parts[2], " ")

    local match_count = 0
    for _, number in pairs(my_numbers) do
        if contains(winners, number) then
            match_count = match_count + 1
        end
    end
    return match_count
end

function contains(table, element)
    for _, value in pairs(table) do
        if value == element then
            return true
        end
    end
    return false
end

local og_cards = {}
for line in io.lines() do
    table.insert(og_cards, line)
end


master_cards = {}
table.insert(master_cards, og_cards)
local i = 1
while i <= #master_cards do
    local card = master_cards[i]
    for j = 1, #card do
        local local_card = {}
        local line = card[j]
        local card_index = string.match(line, "%d+")
        local col_loc = string.find(line, ":")
        line = string.sub(line, col_loc + 1)
        local match_count = get_match_count(line)
        for k = 1, match_count do
            table.insert(local_card, og_cards[card_index + k])
        end
        if #local_card ~= 0 then
            table.insert(master_cards, local_card)
        end
    end
    i = i + 1
end


local card_count = 0
for i, card in ipairs(master_cards) do
    for j, _ in ipairs(card) do
        card_count = card_count + 1
    end
end
print(card_count)
