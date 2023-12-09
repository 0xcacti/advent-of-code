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


function isFiveOfAKind(hand)
    local counts = {}
    local wildcards = 0

    for i = 1, #hand do
        local char = hand:sub(i, i)
        if char ~= "J" then
            counts[char] = (counts[char] or 0) + 1
        else
            wildcards = wildcards + 1
        end
    end
    for char, count in pairs(counts) do
        if count + wildcards == 5 then
            return true
        end
    end
    if wildcards == 5 then
        return true
    end
    return false
end

function isFourOfAKind(hand)
    local counts = {}
    local wildcards = 0

    for i = 1, #hand do
        local char = hand:sub(i, i)
        if char ~= "J" then
            counts[char] = (counts[char] or 0) + 1
        else
            wildcards = wildcards + 1
        end
    end
    for char, count in pairs(counts) do
        if count + wildcards == 4 then
            return true
        end
    end

    if wildcards == 4 then
        return true
    end
    return false
end

function isFullHouse(hand)
    local counts = {}
    local hasPair = false
    local hasThree = false
    local wildcards = 0
    local pairChar = ""
    local threeChar = ""

    -- J2296

    for i = 1, #hand do
        local char = hand:sub(i, i)
        if char ~= "J" then
            counts[char] = (counts[char] or 0) + 1
        else
            wildcards = wildcards + 1
        end
        local char = hand:sub(i, i)
    end

    for char, v in pairs(counts) do
        if v == 3 then
            threeChar = char
            hasThree = true
        elseif v == 2 then
            pairChar = char
            hasPair = true
        end
    end
    if hasThree and not hasPair then
        for char, v in pairs(counts) do
            if v < 2 and v + wildcards == 2 then
                return true
            end
        end
    elseif hasPair and not hasThree then
        pairCount = 0
        for char, v in pairs(counts) do
            if v == 2 then
                pairCount = pairCount + 1
            end
        end
        if pairCount == 2 and wildcards == 1 then
            return true
        end
    else
        if not hasPair and not hasThree then
            for char, v in pairs(counts) do
                if v < 2 and v + wildcards == 2 then
                    hasPair = true
                    wildcards = wildcards - (2 - v)
                end
            end
            for char, v in pairs(counts) do
                if v < 3 and v + wildcards == 3 then
                    hasThree = true
                    wildcards = wildcards - (3 - v)
                end
            end
        end
    end
    return hasPair and hasThree
end

function isThreeOfAKind(hand)
    local counts = {}
    local wildcards = 0

    for i = 1, #hand do
        local char = hand:sub(i, i)
        if char ~= "J" then
            counts[char] = (counts[char] or 0) + 1
        else
            wildcards = wildcards + 1
        end
    end
    for char, count in pairs(counts) do
        if count + wildcards == 3 then
            return true
        end
    end

    if wildcards == 3 then
        return true
    end
    return false
end

function isTwoPair(hand)
    local counts = {}
    local pairCount = 0
    local wildcards = 0

    for i = 1, #hand do
        local char = hand:sub(i, i)
        if char ~= "J" then
            counts[char] = (counts[char] or 0) + 1
        else
            wildcards = wildcards + 1
        end
        local char = hand:sub(i, i)
    end

    for char, v in pairs(counts) do
        if v == 2 then
            pairCount = pairCount + 1
        end
    end
    if pairCount < 2 then
        for char, v in pairs(counts) do
            if v < 2 and v + wildcards == 2 then
                pairCount = pairCount + 1
                wildcards = wildcards - (2 - v)
            end
        end
    end
    return pairCount == 2
end

function isOnePair(hand)
    local counts = {}
    local wildcards = 0

    for i = 1, #hand do
        local char = hand:sub(i, i)
        if char ~= "J" then
            counts[char] = (counts[char] or 0) + 1
        else
            wildcards = wildcards + 1
        end
    end
    for char, count in pairs(counts) do
        if count + wildcards == 2 then
            return true
        end
    end

    if wildcards == 2 then
        return true
    end
    return false
end

function getHandRank(hand)
    if isFiveOfAKind(hand) then
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
    elseif card == "T" then
        return 11
    elseif card == "J" then
        return 1
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

function mapHand(rank)
    if rank == 7 then
        return "Five of a Kind"
    elseif rank == 6 then
        return "Four of a Kind"
    elseif rank == 5 then
        return "Full House"
    elseif rank == 4 then
        return "Three of a Kind"
    elseif rank == 3 then
        return "Two Pair"
    elseif rank == 2 then
        return "One Pair"
    else
        return "Nothing"
    end
end

table.sort(handsAndBids, sorter)
local winning = 0
for i = 1, #handsAndBids do
    -- print(handsAndBids[i][1], handsAndBids[i][2])
    local handWin = i * handsAndBids[i][2]
    winning = winning + handWin
    print(handsAndBids[i][1] .. " " .. mapHand(getHandRank(handsAndBids[i][1])))
end
print(winning)
-- print(isFullHouse("J2296"))
