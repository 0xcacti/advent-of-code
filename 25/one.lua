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

function generateGraph(path, cuts)
    io.input(path)
    local graph = {}
    local nodes = {}
    local f = assert(io.open("graph.dot", "w"))
    f:write "graph {\n"
    f:write [[	graph [pad="0.5", nodesep="1", ranksep="2"];
    ]]

    for line in io.lines() do
        parts = mysplit(line, ":")
        start = parts[1]
        remainder = mysplit(parts[2], " ")
        graph[start] = graph[start] or {}
        nodes[start] = true

        for i, v in ipairs(remainder) do
            local pair = { start, v }
            table.sort(pair, function(a, b)
                return a < b
            end)

            if not cuts[start .. ":" .. v] and not cuts[v .. ":" .. start] then
                print(start .. ":" .. v)
                f:write(string.format("\t%s -- %s;\n", start, v))
                graph[v] = graph[v] or {}
                graph[start][v] = true
                graph[v][start] = true
            else
                print(start .. ":" .. v)
                print(v .. ":" .. start)
                print("cutting ")
            end
            nodes[v] = true
        end
    end
    f:write "}\n"

    return graph, nodes
end

function printInput(input)
    for i, row in ipairs(input) do
        for j, col in ipairs(row) do
            io.write(col .. " ")
        end
        io.write("\n")
    end
end

local function dfs(graph, n, visited)
    if not visited[n] then
        visited[n] = true
        local v = 1
        for child in pairs(graph[n]) do
            v = v + (dfs(graph, child, visited) or 0)
        end
        return v
    end
end

function solve(path)
    local cuts = {
        ["pgl:mtl"] = true,
        ["lkf:scf"] = true,
        ["zxb:zkv"] = true,
    }

    local graph, nodes = generateGraph(path, cuts)
    local queue = {}
    for k, _ in pairs(nodes) do
        table.insert(queue, k)
    end
    table.sort(queue, function(a, b)
        return a < b
    end)

    local visited = {}
    local subgraphs = {}

    while #queue > 0 do
        local current = table.remove(queue, 1)
        if visited[current] then
            goto continue
        end
        table.insert(subgraphs, dfs(graph, current, visited))
        visited[current] = true
        ::continue::
    end
    if #subgraphs == 1 then
        return 0
    end
    return subgraphs[1] * subgraphs[2]
end

s = solve("input.txt")
print(s)
