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

function solve(path)
    local cuts = {
        ["bvb:cmg"] = true,
        ["hfx:pzl"] = true,
        ["jqt:nvd"] = true,
    }

    local graph, nodes = generateGraph(path, cuts)
end

s = solve("test_input.txt")
print(s)
