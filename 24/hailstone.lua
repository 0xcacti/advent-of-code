Hailstone = {}
Hailstone.__index = Hailstone

function Hailstone.new(sx, sy, sz, vx, vy, vz)
    self = setmetatable({}, Hailstone)
    self.sx = sx
    self.sy = sy
    self.sz = sz
    self.vx = vx
    self.vy = vy
    self.vz = vz
    self.a = vy
    self.b = -vx
    self.c = vy * sx - vx * sy
    return self
end

Hailstone.__tostring = function(self)
    return string.format("Hailstone(a=%d, b=%d, c=%d)", self.a, self.b, self.c)
end
