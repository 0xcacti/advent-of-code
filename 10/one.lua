io.input("input.txt")


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
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i - 1, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j - 1 }))
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

path = {}
print(sLoc)
if detectCycle(sLoc, path) then
    print("Cycle detected")
else
    print("No cycle detected")
end

for _, node in pairs(path) do
    print(node)
end
print(#path - 1)
