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

function area(verticies)
    local sum = 0
    for i = 1, #verticies do
        local nextIndex = (i % #verticies) + 1
        local prevIndex = (i == 1) and #verticies or (i - 1)

        sum = sum + verticies[i][1] * (verticies[nextIndex][2] - verticies[prevIndex][2])
    end

    return math.floor(math.abs(sum) / 2)
end

function getInput(path)
    io.input(path)
    local inp = {}
    for line in io.lines() do
        local row = {}
        parts = mysplit(line, " ")
        for i, part in ipairs(parts) do
            if i == 3 then
                part = string.sub(part, 3, 8)
            end
            table.insert(row, part)
        end
        table.insert(inp, row)
    end
    return inp
end

function getDirections()
    directions = {}
    directions["U"] = { 0, -1 }
    directions["D"] = { 0, 1 }
    directions["R"] = { 1, 0 }
    directions["L"] = { -1, 0 }
    return directions
end

function getPoints(input, directions)
    local start = { 1, 1 }
    local points = { start }
    local totalBoundryPoints = 0

    for _, row in ipairs(input) do
        local direction, distance, color = row[1], row[2], row[3]
        totalBoundryPoints = totalBoundryPoints + tonumber(distance)
        print(direction)
        if directions[direction] == nil then
            break
        end

        dr, dc = directions[direction][1], directions[direction][2]
        distance = tonumber(distance)
        currentR, currentC = points[#points][1], points[#points][2]
        table.insert(points, { currentR + dr * distance, currentC + dc * distance })
    end
    table.remove(points, #points)
    return points, totalBoundryPoints
end

function solve(path)
    local input = getInput(path)
    local directions = getDirections()
    local points, boundryPoints = getPoints(input, directions)
    local area = area(points)
    internal = area - math.floor(boundryPoints / 2) + 1
    return (internal + boundryPoints)
end

print(solve("test_input.txt"))
