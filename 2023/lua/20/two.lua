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

function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

function lcm(a, b)
    return (a * b) / gcd(a, b)
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
    feed = ""
    for name, module in pairs(modules) do
        for _, output in ipairs(module.outputs) do
            if output == "rx" then
                feed = name
                break
            end
        end
    end

    cycleLength = {}
    seen = {}
    for name, module in pairs(modules) do
        for _, output in ipairs(module.outputs) do
            if feed == output then
                seen[name] = 0
            end
        end
    end

    for name, val in pairs(cycleLength) do
        print(name, val)
    end

    presses = 0
    while true do
        presses = presses + 1
        queue = Deque.new()
        for _, target in ipairs(broadcastTargets) do
            queue:pushBack({ "broadcaster", target, "low" })
        end


        while not queue:empty() do
            origin, target, pulse = table.unpack(queue:popFront())
            if modules[target] == nil then
                goto continue
            end

            module = modules[target]


            if module.name == feed and pulse == "high" then
                seen[origin] = seen[origin] + 1
                if cycleLength[origin] == nil then
                    cycleLength[origin] = presses
                else
                    assert(presses == seen[origin] * cycleLength[origin])
                end
                seenAll = true
                for name, val in pairs(seen) do
                    if val == 0 then
                        seenAll = false
                    end
                end
                if seenAll then
                    lcmVal = 1
                    for name, val in pairs(cycleLength) do
                        lcmVal = lcm(lcmVal, val)
                        print(name, val)
                    end


                    print(lcmVal)
                    print(string.format("%.17e", lcmVal))

                    os.exit(0)
                end
            end

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
end

print(solve("input.txt"))
