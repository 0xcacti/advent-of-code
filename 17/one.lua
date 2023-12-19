require("queue")

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
        local row = {}
        for c in line:gmatch(".") do
            table.insert(row, c)
        end
        table.insert(inp, row)
    end
    return inp
end

function printInput(inp)
    for _, row in ipairs(inp) do
        for _, c in ipairs(row) do
            io.write(c)
        end
        io.write("\n")
    end
end

function createVisit(r, c, dr, dc, count)
    return r .. "," .. c .. "," .. dr .. "," .. dc .. "," .. count
end

function dijkstras(grid, start, destination)
    local q = PriorityQueue.new()
    q:push(start)
    local seen = {}
    while not q:empty() do
        local item = q:pop()
        local hl, r, c, dc, dr, count = item.hl, item.row, item.col, item.dc, item.dr, item.count

        if r == destination.row and c == destination.col then
            return hl
        end

        if seen[createVisit(r, c, dr, dc, count)] then
            goto continue
        end

        seen[createVisit(r, c, dr, dc, count)] = true

        -- process two steps
        if count < 3 and (dr ~= 0 or dc ~= 0) then
            local nr = r + dr
            local nc = c + dc

            if 1 <= nr and nr <= #grid and 1 <= nc and nc <= #grid[1] then
                q:push({ row = nr, col = nc, dc = dc, dr = dr, count = count + 1, hl = hl + grid[nr][nc] })
            end
        end

        for _, next in ipairs({ { 0, 1 }, { 0, -1 }, { 1, 0 }, { -1, 0 } }) do
            local ndr, ndc = next[1], next[2]
            if not (ndr == dr and ndc == dc) and not (ndr == -dr and ndc == -dc) then
                local nr = r + ndr
                local nc = c + ndc

                if 1 <= nr and nr <= #grid and 1 <= nc and nc <= #grid[1] then
                    q:push({ row = nr, col = nc, dc = ndc, dr = ndr, count = 1, hl = hl + grid[nr][nc] })
                end
            end
        end


        ::continue::
    end
end

function solve(path)
    local inputs = getInput(path)
    -- printInput(inputs)
    local start = { row = 1, col = 1, dc = 0, dr = 0, count = 0, hl = 0 }
    local destination = { row = #inputs, col = #inputs[1] }
    local hl = dijkstras(inputs, start, destination)
    print(hl)
end

print(solve("input.txt"))
