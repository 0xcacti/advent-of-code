io.input("input.txt")
local function mysplit(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

sequences = {}
for line in io.lines() do
    table.insert(sequences, mysplit(line, " "))
end
function isZero(sequence)
    for i, number in ipairs(sequence) do
        if number ~= 0 then
            return false
        end
    end
    return true
end

subsequences = {}
sequence_map = {}
for i, sequence in ipairs(sequences) do
    current_sequence = sequence
    subsequences = {}
    while not isZero(current_sequence) do
        new_sequence = {}
        for j = 2, #current_sequence do
            table.insert(new_sequence, current_sequence[j] - current_sequence[j - 1])
        end
        table.insert(subsequences, new_sequence)
        current_sequence = new_sequence
    end
    sequence_map[sequence] = subsequences
end


local last_vals = {}
for sequence, subsequences in pairs(sequence_map) do
    local last_value = 0
    for i = #subsequences, 1, -1 do
        last_value = last_value + subsequences[i][#subsequences[i]]
    end
    last_value = last_value + sequence[#sequence]
    table.insert(last_vals, last_value)
end

sum = 0
for i, val in ipairs(last_vals) do
    sum = sum + val
end
print(sum)
