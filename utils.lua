lineDelimiter = "[^\n]+"
commaDelimiter = "[^,]+"
spaceDelimiter = "[^%s]+"
semiColonDelimiter = "[^;]+"
colonDelimiter = "[^:]+"
matchNumber = "%d+"

function readFile(file)
    local file = io.open(file, "r")
    local input = file:read("*a")
    file:close()
    return input
end

function splitString(str, del)
    local t = {}
    for value in str:gmatch(del) do
        table.insert(t, value)
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
