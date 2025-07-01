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

function contains(table, element)
    for _, value in pairs(table) do
        if value == element then
            return true
        end
    end
    return false
end

local sum = 0
for line in io.lines() do
    local col_loc = string.find(line, ":")
    line = string.sub(line, col_loc + 1)
    parts = mysplit(line, "|")
    winners = mysplit(parts[1], " ")
    my_numbers = mysplit(parts[2], " ")


    local is_first = true
    local card_score = 0
    for _, number in pairs(my_numbers) do
        if contains(winners, number) then
            if is_first then
                is_first = false
                card_score = card_score + 1
            else
                card_score = card_score * 2
            end
        end
    end
    sum = sum + card_score
end
print(sum)
