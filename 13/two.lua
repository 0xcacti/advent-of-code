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

function columnsDifferByOne(note, col1, col2)
    local diffCount = 0
    for i = 1, #note do
        if note[i][col1] ~= note[i][col2] then
            diffCount = diffCount + 1
        end
    end
    return diffCount == 1
end

function fixColumnSmudge(note, col1, col2)
    for i = 1, #note do
        if note[i][col1] ~= note[i][col2] then
            note[i][col1] = note[i][col2]
            return
        end
    end
end

function findVerticalSmudge(note)
    -- for each column in note
    for i = 2, #note[1] do
        if columnsDifferByOne(note, i - 1, i) then
            fixColumnSmudge(note, i - 1, i)
        end
    end
end

function linesDifferByOne(note, line1, line2)
    local diffCount = 0
    for i = 1, #note[line1] do
        if note[line1][i] ~= note[line2][i] then
            diffCount = diffCount + 1
        end
    end
    return diffCount == 1
end

function fixLineSmudge(note, line1, line2)
    for i = 1, #note[line1] do
        if note[line1][i] ~= note[line2][i] then
            note[line1][i] = note[line2][i]
            return
        end
    end
end

function findHorizontalSmudge(note)
    -- for each column in note
    for i = 2, #note do
        if linesDifferByOne(note, i - 1, i) then
            fixLineSmudge(note, i - 1, i)
        end
    end
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
    totalVertical = totalVertical + findVerticalMatch(note)
    totalHorizontal = totalHorizontal + findHorizontalMatch(note)
end

print(100 * totalHorizontal + totalVertical)
