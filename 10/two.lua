io.input("two_test_input.txt")
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

map = {}
for line in io.lines() do
    row = {}
    for c in line:gmatch(".") do
        table.insert(row, c)
    end
    table.insert(map, row)
end


function checkInBounds(loc)
    return loc[1] >= 1 and loc[1] <= #map and loc[2] >= 1 and loc[2] <= #map[1]
end

function nodeKey(node)
    return "(" .. tostring(node[1]) .. "," .. tostring(node[2]) .. ")"
end

function addNode(graph, node)
    if not graph[node] then
        graph[node] = {}
    end
end

function addEdge(graph, node1, node2)
    if not graph[node1] then
        addNode(graph, node1)
    end
    if not graph[node2] then
        addNode(graph, node2)
    end
    table.insert(graph[node1], node2)
end

graph = {}
sLoc = ""
for j, row in ipairs(map) do
    for i, c in ipairs(row) do
        if c == "S" then
            sLoc = nodeKey({ i, j })
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i + 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j + 1 }))
            goto continue
        elseif c == "." then
            goto continue
        elseif c == "|" then
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j + 1 }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j - 1 }))
        elseif c == "-" then
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i + 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i - 1, j }))
        elseif c == "L" then
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i + 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j - 1 }))
        elseif c == "J" then
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i - 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j - 1 }))
        elseif c == "7" then
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i - 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j + 1 }))
        elseif c == "F" then
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i + 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j + 1 }))
        end
        ::continue::
    end
end


function DFS(node, visited, parent, path)
    if visited[node] then
        table.insert(path, node) -- Add the node to the path
        return true
    end

    visited[node] = true
    table.insert(path, node)

    if graph[node] ~= nil then
        for _, adjacent in pairs(graph[node]) do
            if adjacent ~= parent and DFS(adjacent, visited, node, path) then
                return true
            end
        end
    end
    table.remove(path)

    return false
end

function detectCycle(start_node, path)
    local visited = {}
    return DFS(start_node, visited, nil, path)
end

function isInsideLoop(node, path)
    for _, n in pairs(path) do
        if n == node then
            return true
        end
    end
    return false
end

path = {}
print(sLoc)
if detectCycle(sLoc, path) then
    print("Cycle detected")
else
    print("No cycle detected")
end


function calculateArea(path)
    local area = 0
    for i = 1, #path - 1 do
        local x1, y1 = table.unpack(path[i])
        local x2, y2 = table.unpack(path[i + 1])
        area = area + (x1 * y2 - y1 * x2)
    end
    -- Close the polygon
    local x1, y1 = table.unpack(path[#path])
    local x2, y2 = table.unpack(path[1])
    area = area + (x1 * y2 - y1 * x2)

    return math.abs(area) / 2
end

print(#path - 1)

-- Convert your path to a list of coordinates
-- Example: path = {{1, 2}, {3, 4}, {5, 6}, ...}
--
updated_path = {}
for _, node in pairs(path) do
    local p = mysplit(string.sub(node, 2, #node - 1), ",")
    table.insert(updated_path, { tonumber(p[1]), tonumber(p[2]) })
end


local area = calculateArea(updated_path)
print("Area: " .. area)
