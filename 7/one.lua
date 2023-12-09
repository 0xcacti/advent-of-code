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

local handsAndBids = {}

for line in io.lines() do
    local hand, bid = table.unpack(mysplit(line, " "))
    table.insert(handsAndBids, { hand, bid })
end


function isFourOfAKind(hand)
    local counts = {}
    for i = 1, #hand do
        local char = hand:sub(i, i)
        counts[char] = (counts[char] or 0) + 1
        if counts[char] == 4 then
            return true
        end
    end
    return false
end

function isFullHouse(hand)
    local hasThreeOfKind = false
    local hasPair = false
    local counts = {}
    for i = 1, #hand do
        local c = hand:sub(i, i)
        if counts[c] == nil then
            counts[c] = 1
        else
            counts[c] = counts[c] + 1
        end
    end
    for _, count in pairs(counts) do
        if count == 3 then
            hasThreeOfKind = true
        elseif count == 2 then
            hasPair = true
        end
    end
    return hasThreeOfKind and hasPair
end

function isThreeOfAKind(hand)
    local counts = {}
    for i = 1, #hand do
        local char = hand:sub(i, i)
        counts[char] = (counts[char] or 0) + 1
        if counts[char] == 3 then
            return true
        end
    end
    return false
end

function isTwoPair(hand)
    local counts = {}
    local pairCount = 0
    for i = 1, #hand do
        local char = hand:sub(i, i)
        counts[char] = (counts[char] or 0) + 1
    end
    for _, v in pairs(counts) do
        if v == 2 then
            pairCount = pairCount + 1
        end
    end
    return pairCount == 2
end

function isOnePair(hand)
    local counts = {}
    for i = 1, #hand do
        local char = hand:sub(i, i)
        counts[char] = (counts[char] or 0) + 1
        if counts[char] == 2 then
            return true
        end
    end
    return false
end

function getHandRank(hand)
    if string.find(hand, "(.)%1%1%1%1") then
        return 7
    end
    if isFourOfAKind(hand) then
        return 6
    end
    if isFullHouse(hand) then
        return 5
    end
    if isThreeOfAKind(hand) then
        return 4
    end
    if isTwoPair(hand) then
        return 3
    end
    if isOnePair(hand) then
        return 2
    end

    return 1
end

function cardWeight(card)
    if card == "A" then
        return 14
    elseif card == "K" then
        return 13
    elseif card == "Q" then
        return 12
    elseif card == "J" then
        return 1
    elseif card == "T" then
        return 10
    else
        return tonumber(card)
    end
end

function sorter(a, b)
    local rankA = getHandRank(a[1])
    local rankB = getHandRank(b[1])
    if rankA == rankB then
        for i = 1, 5 do
            local cardA = a[1]:sub(i, i)
            local cardB = b[1]:sub(i, i)
            if cardWeight(cardA) > cardWeight(cardB) then
                return false
            elseif cardWeight(cardA) < cardWeight(cardB) then
                return true
            end
        end
    end
    return rankA < rankB
end

table.sort(handsAndBids, sorter)
local winning = 0
for i = 1, #handsAndBids do
    print(handsAndBids[i][1], handsAndBids[i][2])
    local handWin = i * handsAndBids[i][2]
    winning = winning + handWin
end
print(winning)
