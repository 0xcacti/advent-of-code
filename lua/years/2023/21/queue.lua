Queue = {}
Queue.__index = Queue

function Queue.new()
    local deque = {}
    setmetatable(deque, Queue)
    deque._first = 0
    deque._last = -1
    return deque
end

function Queue:pushFront(value)
    self._first = self._first - 1
    self[self._first] = value
end

function Queue:pushBack(value)
    self._last = self._last + 1
    self[self._last] = value
end

function Queue:popFront()
    if self._first > self._last then
        return nil
    end
    local value = self[self._first]
    self[self._first] = nil
    self._first = self._first + 1
    return value
end

function Queue:popBack()
    if self._first > self._last then
        return nil
    end
    local value = self[self._last]
    self[self._last] = nil
    self._last = self._last - 1
    return value
end

function Queue:empty()
    return self._first > self._last
end
