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
for i, v in ipairs(sequences) do
    for j, w in ipairs(v) do
        io.write(w .. " ")
    end
    io.write("\n")
end
