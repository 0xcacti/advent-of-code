io.input("input.txt")

function mysplit(inputstr, sep)
    if sep == nil then
        sep = "\n\n" -- Default to splitting on empty lines
    end
    local t = {}
    local start = 1
    local sepStart, sepEnd = string.find(inputstr, sep, start)
    while sepStart do
        table.insert(t, string.sub(inputstr, start, sepStart - 1))
        start = sepEnd + 1
        sepStart, sepEnd = string.find(inputstr, sep, start)
    end
    table.insert(t, string.sub(inputstr, start)) -- Add the last segment
    return t
end

function equalLines(line1, line2)
    for i = 1, #line1 do
        if line1[i] ~= line2[i] then
            return false
        end
    end
    return true
end

function equalColumns(note, col1, col2)
    for i = 1, #note do
        if note[i][col1] ~= note[i][col2] then
            return false
        end
    end
    return true
end

function findVerticalMatch(note)
    local lineCount = 0
    -- for each column in note
    for i = 2, #note[1] do
        if equalColumns(note, i - 1, i) then
            symLineCount = checkHorizontalSymmetry(note, i - 1, i)
            if lineCount == 0 then
                lineCount = symLineCount
            end
        end
    end
    return lineCount
end

function findHorizontalMatch(note)
    recentLine = {}
    local lineCount = 0
    for i = 2, #note do
        if equalLines(note[i], note[i - 1]) then
            symLineCount = checkVerticalSymmetry(note, i - 1, i)
            if lineCount == 0 then
                lineCount = symLineCount
            end
        end
    end
    return lineCount
end

function checkVerticalSymmetry(note, line1, line2)
    local bound = math.min(line1 - 1, (#note - line2))
    for i = 1, bound do
        if not equalLines(note[line1 - i], note[line2 + i]) then
            return 0
        end
    end
    return line1
end

function checkHorizontalSymmetry(note, line1, line2)
    -- find loop bound
    local lineCount = 0
    print("checkHorizontalSymmetry")
    print("line1: " .. line1)
    print("line2: " .. line2)
    local bound = math.min(line1 - 1, (#note[1] - line2))
    print("bound: " .. bound)
    for i = 1, bound do
        if not equalColumns(note, line1 - i, line2 + i) then
            print("not equal at indicies" .. (line1 - i) .. " " .. (line2 + i))

            return 0
        end
    end
    return line1
end

local input = io.read("*all")
local groups = mysplit(input)

local notebook = {}
for i, group in ipairs(groups) do
    local note = {}
    local group = mysplit(group, "\n")
    for j, entry in ipairs(group) do
        local line = {}
        for k = 1, #entry do
            table.insert(line, string.sub(entry, k, k))
        end
        if #line ~= 0 then
            table.insert(note, line)
        end
    end
    if #note ~= 0 then
        table.insert(notebook, note)
    end
end



local totalVertical = 0
local totalHorizontal = 0
-- ######.......
-- ######.......
-- ......#######
-- #####.##.#..#
-- ####..#..#.##
-- ##..##..#####
-- ##..#####.###
-- #..#...##.#..
-- .###.##..#.##
--
function printTable(note)
    for i, line in ipairs(note) do
        for j, char in ipairs(line) do
            io.write(char)
        end
        io.write("\n")
    end
    io.write("\n")
end

for i, note in ipairs(notebook) do
    printTable(note)
    v = findVerticalMatch(note)
    h = findHorizontalMatch(note)
    if v ~= 0 then
        print(v)
    end
    if h ~= 0 then
        print(h * 100)
    end
    totalVertical = totalVertical + findVerticalMatch(note)
    totalHorizontal = totalHorizontal + findHorizontalMatch(note)
end

print(100 * totalHorizontal + totalVertical)
