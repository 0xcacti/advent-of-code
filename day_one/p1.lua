io.input("input.txt")

local sum = 0
for line in io.lines() do
    local x = string.sub(string.match(line, "%d+"), 1, 1)
    local y = string.sub(string.match(string.reverse(line), "%d+"), 1, 1)
    local pair = x .. y
    sum = sum + tonumber(pair)
end

print(sum)
