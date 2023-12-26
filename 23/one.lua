--
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
        row = {}
        for c in line:gmatch(".") do
            table.insert(row, c)
        end
        table.insert(inp, row)
    end
    return inp
end

function printInput(input)
    for i, row in ipairs(input) do
        for j, col in ipairs(row) do
            io.write(col .. " ")
        end
        io.write("\n")
    end
end

function getStart(input)
    for i = 1, #input[1] do
        if input[1][i] == "." then
            return { 1, i }
        end
    end
end

function getEnd(input)
    for i = 1, #input[1] do
        if input[#input][i] == "." then
            return { #input, i }
        end
    end
end

function parsePair(pair)
    return mysplit(pair, ",")
end

function contains(t, e)
    for _, v in ipairs(t) do
        if v == e then
            return true
        end
    end
    return false
end

function dfs(graph, seen, pt, stop)
    if pt == stop then
        return 0
    end
    m = -math.huge
    table.insert(seen, pt)
    for _, nx in ipairs(graph[pt]) do
        if not contains(seen, nx) then
            m = math.max(m, dfs(graph, seen, nx, stop) + graph[pt][nx])
        end
    end
    return m
end

function solve(path)
    local inputs = getInput(path)
    local start = getStart(inputs)
    local stop = getEnd(inputs)

    points = {}
    table.insert(points, start[1] .. "," .. start[2])
    table.insert(points, stop[1] .. "," .. stop[2])

    for i = 1, #inputs do
        for j = 1, #inputs[1] do
            char = inputs[i][j]

            if char == "#" then
                goto continue
            end

            neighbors = 0
            next = { { i - 1, j }, { i + 1, j }, { i, j - 1 }, { i, j + 1 } }
            for _, n in ipairs(next) do
                ni, nj = n[1], n[2]
                if 1 <= ni and ni <= #inputs and 1 <= nj and nj <= #inputs[1] and inputs[ni][nj] ~= "#" then
                    neighbors = neighbors + 1
                end
            end
            if neighbors >= 3 then
                table.insert(points, i .. "," .. j)
            end
            ::continue::
        end
    end

    dirs = {}
    dirs["^"] = { "-1,0" }
    dirs["v"] = { "1,0" }
    dirs["<"] = { "0,-1" }
    dirs[">"] = { "0,1" }
    dirs["."] = { "-1,0", "1,0", "0,-1", "0,1" }

    graph = {}
    for _, p in ipairs(points) do
        graph[p] = {}
    end
    for _, p in ipairs(points) do
        pair = parsePair(p)
        sr, sc = tonumber(pair[1]), tonumber(pair[2])
        stackStart = { 0, sr, sc }
        stack = { stackStart }
        seen = { p }

        while #stack > 0 do
            entry = table.remove(stack)
            n, r, c = tonumber(entry[1]), tonumber(entry[2]), tonumber(entry[3])
            if n ~= 0 and contains(points, r .. "," .. c) then
                graph[sr .. "," .. sc][r .. "," .. c] = n
                goto continue
            end
            for _, entry in ipairs(dirs[inputs[r][c]]) do
                parsed = parsePair(entry)
                dr, dc = tonumber(parsed[1]), tonumber(parsed[2])
                nr = r + dr
                nc = c + dc
                if 1 <= nr and nr <= #inputs and 1 <= nc and nc <= #inputs[1] and inputs[nr][nc] ~= "#" and not contains(seen, nr .. "," .. "nc") then
                    table.insert(stack, { n + 1, nr, nc })
                    table.insert(seen, nr .. "," .. nc)
                end
            end
            ::continue::
        end
    end
    return dfs(graph, {}, start[1] .. "," .. start[2], stop[1] .. "," .. stop[2])
end

s = solve("input.txt")
print(s)
