Set = {}
Set.__index = Set

function Set.new()
    local s = setmetatable({ _elements = {}, _size = 0 }, Set)
    return s
end

function Set:add(element)
    if not self._elements[element] then
        self._elements[element] = true
        self._size = self._size + 1
    end
end

function Set:remove(element)
    if self._elements[element] then
        self._elements[element] = nil
        self._size = self._size - 1
    end
end

function Set:contains(element)
    return self._elements[element] ~= nil
end

function Set:size()
    return self._size
end

function Set:elements()
    local elems = {}
    for k in pairs(self._elements) do
        table.insert(elems, k)
    end
    return elems
end
