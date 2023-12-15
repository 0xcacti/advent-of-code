--
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

function linesDifferByAtMostOne(line1, line2)
    local diffCount = 0
    for i = 1, #line1 do
        if line1[i] ~= line2[i] then
            diffCount = diffCount + 1
            if diffCount > 1 then
                return false, diffCount
            end
        end
    end
    return true, diffCount
end

function columnsDifferByAtMostOne(note, col1, col2)
    local diffCount = 0
    for i = 1, #note do
        if note[i][col1] ~= note[i][col2] then
            diffCount = diffCount + 1
            if diffCount > 1 then
                return false, diffCount
            end
        end
    end
    return true, diffCount
end

function checkVerticalSymmetry(note, line1, line2)
    local bound = math.min(line1 - 1, (#note - line2))
    local diffCount = 0

    print("Checking horizontal symmetry")
    print("Line1: " .. line1)
    print("Line2: " .. line2)
    print("Bound: " .. bound)
    isSymBefore, differenceBefore = linesDifferByAtMostOne(note[line1], note[line2]) -- am I doing this too many times
    diffCount = diffCount + differenceBefore


    for i = 1, bound do
        -- print("do we enter here?")
        isSymAfter, difference = linesDifferByAtMostOne(note[line1 - i], note[line2 + i])
        if not isSymAfter or not isSymBefore then
            return 0
        else
            diffCount = diffCount + difference
            -- for j = 1, #note[i] do
            --     io.write(note[line1][j] .. " ")
            -- end
            -- print()
            -- for j = 1, #note[i] do
            --     io.write(note[line2][j] .. " ")
            -- end
            -- print(diffCount)
        end
    end

    if diffCount == 1 then
        return line1
    end
    return 0
end

function checkHorizontalSymmetry(note, line1, line2)
    -- print("Checking horizontal symmetry")
    -- print("Line1: " .. line1)
    -- print("Line2: " .. line2)
    local bound = math.min(line1 - 1, (#note[1] - line2))
    -- print("Bound: " .. bound)
    local diffCount = 0
    isSymBefore, differenceBefore = columnsDifferByAtMostOne(note, line1, line2)
    diffCount = diffCount + differenceBefore

    for i = 1, bound do
        isSym, difference = columnsDifferByAtMostOne(note, line1 - i, line2 + i)
        if not isSym or not isSymBefore then
            return 0
        else
            diffCount = diffCount + difference
        end
    end
    if diffCount == 1 then
        return line1
    end
    return 0
end

function findHorizontalSmudge(note)
    recentLine = {}
    local lineCount = 0
    for i = 2, #note do
        symLineCount = checkVerticalSymmetry(note, i - 1, i)
        if lineCount == 0 then
            lineCount = symLineCount
        end
    end
    return lineCount
end

function findVerticalSmudge(note)
    local lineCount = 0
    -- for each column in note
    for i = 2, #note[1] do
        symLineCount = checkHorizontalSymmetry(note, i - 1, i)
        if lineCount == 0 then
            lineCount = symLineCount
        end
    end
    return lineCount
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
    -- printTable(note)
    -- printTable(note)
    -- if i == 44 then
    --printTable(note)
    verticalSmudge = findVerticalSmudge(note)
    horizontalSmudge = findHorizontalSmudge(note)
    if verticalSmudge ~= 0 then
        print(verticalSmudge)
    end
    if horizontalSmudge ~= 0 then
        print(100 * horizontalSmudge)
    end

    totalVertical = totalVertical + findVerticalSmudge(note)
    totalHorizontal = totalHorizontal + findHorizontalSmudge(note)
    -- end
end

print(100 * totalHorizontal + totalVertical)
