local find_mapping = function(lhs)
    local maps = vim.api.nvim_get_keymap("n")
    for _, value in ipairs(maps) do
        if value.lhs == lhs then
            return value
        end
    end
end

describe("testing solution two", function()
    it("calculates subset ranges correctly", function()
        inc, exc = require("two").map_range(5, 9, 1, 1, 10)
        assert.not_nil(inc)
        assert.is_nil(exc)
        assert.are.same({ 5, 5 }, inc)
    end)

    it("calculates underlap ranges correctly", function()
        -- 1, 2, 3, 4, 5, 6, 7, 8, 9
        -- 5 -> 15
        inc, exc = require("two").map_range(1, 9, 5, 5, 10)
        assert.not_nil(inc)
        assert.not_nil(exc)
        assert.are.same({ 5, 5 }, inc)
        assert.are.same({ 1, 4 }, exc)
    end)

    it("calculates overlap ranges correctly", function()
        --             5, 6, 7, 8, 9, 10
        -- 1, 2, 3, 4, 5, 6, 7, 8
        inc, exc = require("two").map_range(5, 10, 1, 1, 8)
        assert.not_nil(inc)
        assert.not_nil(exc)
        assert.are.same({ 5, 4 }, inc)
        assert.are.same({ 9, 2 }, exc)
    end)


    it("calculates overlap and underlap correctly", function()
        --             5, 6, 7,
        -- 1, 2, 3, 4, 5, 6, 7, 8, 9
        inc, exc = require("two").map_range(1, 9, 5, 5, 3)
        assert.not_nil(inc)
        assert.not_nil(exc)
        assert.are.same({ 5, 3 }, inc)
        assert.are.same(exc[1], 1)
        assert.are.same(exc[2], 4)
        assert.are.same(exc[3], 8)
        assert.are.same(exc[4], 2)
    end)
end)
