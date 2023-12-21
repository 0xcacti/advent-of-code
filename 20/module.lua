--
Module = {}
Module.__index = Module

function Module.new(name, type, outputs)
    local self = setmetatable({}, Module)
    self.name = name
    self.type = type
    self.outputs = outputs

    if type == "%" then
        self.memory = "off"
    else
        self.memory = {}
    end

    return self
end

function Module:__tostring()
    local outputs_str = table.concat(self.outputs, ",")
    local memory_str = type(self.memory) == "table" and "table" or self.memory
    return self.name .. "{type=" .. self.type .. ",outputs=" .. outputs_str .. ",memory=" .. memory_str .. "}"
end
