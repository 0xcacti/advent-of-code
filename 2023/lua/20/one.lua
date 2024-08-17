require("module")
require("deque")

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

function strip(str)
    str = string.gsub(str, " ", "")
    return string.gsub(str, "\n", "")
end

function getInput(path)
    io.input(path)
    modules = {}
    broadcastTargets = {}
    for line in io.lines() do
        left, right = table.unpack(mysplit(line, "->"))
        left = strip(left)
        right = strip(right)
        if left == "broadcaster" then
            broadcastTargets = mysplit(right, ",")
        else
            t = string.sub(left, 1, 1)
            name = string.sub(left, 2, #left)
            modules[name] = Module.new(name, t, mysplit(right, ","))
        end
    end
    return modules, broadcastTargets
end

function solve(path)
    local modules, broadcastTargets = getInput(path)
    for name, module in pairs(modules) do
        for _, output in ipairs(module.outputs) do
            if modules[output] ~= nil and modules[output].type == "&" then
                modules[output].memory[name] = "low"
            end
        end
    end
    local low, high = 0, 0

    for i = 1, 1000 do
        low = low + 1
        queue = Deque.new()
        for _, target in ipairs(broadcastTargets) do
            queue:pushBack({ "broadcaster", target, "low" })
        end
        while not queue:empty() do
            origin, target, pulse = table.unpack(queue:popFront())
            if pulse == "low" then
                low = low + 1
            else
                high = high + 1
            end
            if modules[target] == nil then
                goto continue
            end
            module = modules[target]

            if module.type == "%" then
                if pulse == "low" then
                    outgoing = ""
                    if module.memory == "off" then
                        module.memory = "on"
                        outgoing = "high"
                    else
                        module.memory = "off"
                        outgoing = "low"
                    end
                    for _, output in ipairs(module.outputs) do
                        queue:pushBack({ module.name, output, outgoing })
                    end
                end
            elseif module.type == "&" then
                module.memory[origin] = pulse
                outgoing = "low"
                for _, memory in pairs(module.memory) do
                    if memory == "low" then
                        outgoing = "high"
                        break
                    end
                end
                for _, output in ipairs(module.outputs) do
                    queue:pushBack({ module.name, output, outgoing })
                end
            end

            ::continue::
        end
    end
    return low * high
end

print(solve("input.txt"))
