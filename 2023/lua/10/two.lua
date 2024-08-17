io.input("input.txt")
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
            -- addNode(graph, nodeKey({ i, j }))
            -- addEdge(graph, nodeKey({ i, j }), nodeKey({ i - 1, j }))
            -- addEdge(graph, nodeKey({ i, j }), nodeKey({ i, j - 1 }))
            addNode(graph, nodeKey({ i, j }))
            addEdge(graph, nodeKey({ i, j }), nodeKey({ i - 1, j }))
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

function isInPath(node, path)
    for _, p in pairs(path) do
        if p[1] == node[1] and p[2] == node[2] then
            return true
        end
    end
    return false
end

function isInsideLoopHorizontal(node, path, maxX)
    local intersectionCount = 0
    while node[1] <= maxX do
        for _, p in pairs(path) do
            if p[1] == node[1] and p[2] == node[2] then
                intersectionCount = intersectionCount + 1
            end
        end
        node[1] = node[1] + 1
    end
    if intersectionCount % 2 == 0 then
        return false
    else
        return true
    end
end

function isInside(node, path)
    local intersectionCount = 0

    for _, edge in ipairs(path) do
        -- Assuming each 'edge' is a table {x1, y1, x2, y2} representing two points on the edge
        if (edge[2] > node[2]) ~= (edge[4] > node[2]) then
            -- Check if the edge crosses the y-coordinate of the node
            local xCross = edge[1] + (node[2] - edge[2]) * (edge[3] - edge[1]) / (edge[4] - edge[2])
            if xCross > node[1] then
                -- Only count if the intersection is to the right of the node
                intersectionCount = intersectionCount + 1
            end
        end
    end

    return intersectionCount % 2 == 1
end

-- function isInsideLoopVertical(node, path, maxY)
--     local intersectionCount = 0
--     while node[2] <= maxY do
--         for _, p in pairs(path) do
--             if p[1] == node[1] and p[2] == node[2] then
--                 intersectionCount = intersectionCount + 1
--             end
--         end
--         node[2] = node[2] + 1
--     end
--     if intersectionCount % 2 == 0 then
--         return false
--     else
--         return true
--     end
-- end

function isOnEdge(node, path)
    for _, p in pairs(path) do
        if p[1] == node[1] and p[2] == node[2] then
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


-- Convert your path to a list of coordinates
-- Example: path = {{1, 2}, {3, 4}, {5, 6}, ...}
--
updated_path = {}
for _, node in pairs(path) do
    local p = mysplit(string.sub(node, 2, #node - 1), ",")
    table.insert(updated_path, { tonumber(p[1]), tonumber(p[2]) })
end
edges = {}
for i = 1, #updated_path do
    local nextIndex = (i % #updated_path) + 1 -- This will loop back to the first point after the last one
    local currentNode = updated_path[i]
    local nextNode = updated_path[nextIndex]
    table.insert(edges, { currentNode[1], currentNode[2], nextNode[1], nextNode[2] })
end

count = 0
for j = 1, #map do
    for i = 1, #map[j] do
        if isInPath({ i, j }, updated_path) then
            goto next
        end
        -- if isInsideLoopHorizontal({ i, j }, updated_path, #map[j], edges) then
        --     print("Inside loop: (" .. i .. "," .. j .. ")")
        --     count = count + 1
        -- end
        if isInside({ i, j }, edges) then
            print("Inside loop: (" .. i .. "," .. j .. ")")
            count = count + 1
        end
        ::next::
    end
end
print("Count: " .. count)
print(count)



local testPoint = { 3, 7 }
-- print(isInsideLoop(testPoint, updated_path, #map[3]))
