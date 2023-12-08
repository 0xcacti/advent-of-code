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

function checkForWin(acclTime, totalTime, distance)
    local traveled = acclTime * (totalTime - acclTime)
    return traveled >= distance
end

io.input("input.txt")
local miliseconds = io.read()
local distances = io.read()
miliseconds = mysplit(miliseconds, " ")
distances = mysplit(distances, " ")

local races = {}
for i = 2, #miliseconds do
    table.insert(races, { tonumber(miliseconds[i]), tonumber(distances[i]) })
end

local waysToWin = {}
for i, race in ipairs(races) do
    local length, distance = table.unpack(race)
    local winCount = 0
    for acclTime = 1, length do
        if checkForWin(acclTime, length, distance) then
            winCount = winCount + 1
        end
    end
    table.insert(waysToWin, winCount)
    winCount = 0
end

local product = 1
for i, winCount in ipairs(waysToWin) do
    product = product * winCount
end
print(product)
