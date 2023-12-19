PriorityQueue = {}
PriorityQueue.__index = PriorityQueue

function PriorityQueue.new()
    local self = setmetatable({}, PriorityQueue)
    self.items = {}
    return self
end

function PriorityQueue:push(item)
    table.insert(self.items, item)
    self:_heapifyUp(#self.items)
end

function PriorityQueue:pop()
    if self:empty() then return nil end

    local root = self.items[1]
    self.items[1] = self.items[#self.items]
    table.remove(self.items)
    self:_heapifyDown(1)

    return root
end

function PriorityQueue:empty()
    return #self.items == 0
end

function PriorityQueue:_heapifyUp(index)
    while index > 1 do
        local parentIdx = math.floor(index / 2)
        if self.items[parentIdx].hl > self.items[index].hl then
            self.items[parentIdx], self.items[index] = self.items[index], self.items[parentIdx]
            index = parentIdx
        else
            break
        end
    end
end

function PriorityQueue:_heapifyDown(index)
    local size = #self.items
    while true do
        local leftIdx, rightIdx, smallestIdx
        leftIdx = 2 * index
        rightIdx = 2 * index + 1
        smallestIdx = index

        if leftIdx <= size and self.items[leftIdx].hl < self.items[smallestIdx].hl then
            smallestIdx = leftIdx
        end

        if rightIdx <= size and self.items[rightIdx].hl < self.items[smallestIdx].hl then
            smallestIdx = rightIdx
        end

        if smallestIdx ~= index then
            self.items[index], self.items[smallestIdx] = self.items[smallestIdx], self.items[index]
            index = smallestIdx
        else
            break
        end
    end
end

