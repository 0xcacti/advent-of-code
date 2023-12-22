Set = {}
Set.__index = Set

function Set.new()
    return setmetatable({}, Set)
end

function Set:add(element)
    self[element] = true
end

function Set:remove(element)
    self[element] = nil
end

function Set:contains(element)
    return self[element] ~= nil
end

function Set:size()
    local count = 0
    for _ in pairs(self) do
        count = count + 1
    end
    return count
end

function Set:elements()
    local elems = {}
    for k in pairs(self) do
        table.insert(elems, k)
    end
    return elems
end
