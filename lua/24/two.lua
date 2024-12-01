require("hailstone")

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

function getInput(path)
    io.input(path)
    local inp = {}
    for line in io.lines() do
        line = string.gsub(line, "@", ",")
        local parts = mysplit(line, ",")
        local numericParts = {}
        for _, part in ipairs(parts) do
            table.insert(numericParts, tonumber(part))
        end
        hail = Hailstone.new(table.unpack(numericParts))
        if hail ~= nil then
            table.insert(inp, hail)
        end
    end
    return inp
end

local function gaussian_elimination(matrix)
    local rows = #matrix
    local cols = #matrix[1] - 1 -- Last column is for the constants

    -- Forward elimination
    for k = 1, rows do
        for i = k + 1, rows do
            local factor = matrix[i][k] / matrix[k][k]
            for j = k, cols + 1 do
                matrix[i][j] = matrix[i][j] - factor * matrix[k][j]
            end
        end
    end

    -- Back substitution
    local solution = {}
    for i = rows, 1, -1 do
        local sum = 0
        for j = i + 1, cols do
            sum = sum + matrix[i][j] * (solution[j] or 0)
        end
        solution[i] = (matrix[i][cols + 1] - sum) / matrix[i][i]
    end

    return solution
end

function constructXYMatrix(hailstones)
    local stones_start, stones_len = 100, 4

    local matrix_xy = {}

    for i = stones_start, stones_start + stones_len do
        local s1, s2 = hailstones[i - 1], hailstones[i]
        -- (dy'-dy) X + (dx-dx') Y + (y-y') DX + (x'-x) DY = x' dy' - y' dx' - x dy + y dx
        local row = {
            (s2.vy - s1.vy),
            (s1.vx - s2.vx),
            (s1.sy - s2.sy),
            (s2.sx - s1.sx),
            (s2.sx * s2.vy)
            - (s2.sy * s2.vx)
            - (s1.sx * s1.vy)
            + (s1.sy * s1.vx),
        }
        table.insert(matrix_xy, row)
    end
    return matrix_xy
end

function printMatrix(matrix)
    for i, row in ipairs(matrix) do
        for j, value in ipairs(row) do
            -- Format the value to a fixed number of decimal places, if needed
            -- For example, string.format("%10.2f", value) formats the number to have 2 decimal places with a fixed width
            io.write(string.format("%10.2f", value), " ")
        end
        io.write("\n") -- New line after each row
    end
end

function constructZMatrix(hailstones)
    local matrix_z = {}
    local stones_start, stones_len = 100, 4

    for i = stones_start, stones_start + stones_len do
        local s1, s2 = hailstones[i - 1], hailstones[i]
        -- (dy'-dy) Z + (dz-dz') Y + (y-y') DZ + (z'-z) DY = z' dy' - y' dz' - z dy + y dz
        local row = {
            (s2.vy - s1.vy),
            (s1.vz - s2.vz),
            (s1.sy - s2.sy),
            (s2.sz - s1.sz),
            (s2.sz * s2.vy)
            - (s2.sy * s2.vz)
            - (s1.sz * s1.vy)
            + (s1.sy * s1.vz),
        }
        table.insert(matrix_z, row)
    end
    return matrix_z
end

function solve(path)
    local hailstones = getInput(path)
    xy = constructXYMatrix(hailstones)
    printMatrix(xy)
    zz = constructZMatrix(hailstones)

    local x, y = table.unpack(gaussian_elimination(xy))
    local z = table.unpack(gaussian_elimination(zz))
    ans = string.format("%18.0f", x + y + z)
    return ans
end

s = solve("input.txt")
print(s)
