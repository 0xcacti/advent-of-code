Deque = {}
Deque.__index = Deque

function Deque.new()
    local deque = {}
    setmetatable(deque, Deque)
    deque._first = 0
    deque._last = -1
    return deque
end

function Deque:pushFront(value)
    self._first = self._first - 1
    self[self._first] = value
end

function Deque:pushBack(value)
    self._last = self._last + 1
    self[self._last] = value
end

function Deque:popFront()
    if self._first > self._last then
        return nil
    end
    local value = self[self._first]
    self[self._first] = nil
    self._first = self._first + 1
    return value
end

function Deque:popBack()
    if self._first > self._last then
        return nil
    end
    local value = self[self._last]
    self[self._last] = nil
    self._last = self._last - 1
    return value
end

function Deque:empty()
    return self._first > self._last
end
