require("module")

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
            print(type)
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
    for _, module in pairs(modules) do
        print(tostring(module))
    end
end

print(solve("test_input.txt"))
