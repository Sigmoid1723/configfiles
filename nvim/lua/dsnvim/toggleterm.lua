local status_ok, toggleterm = pcall(require, 'toggleterm')
if not status_ok then
    return
end

toggleterm.setup({
    size = 7,
    open_mapping = [[<leader>\]],
    direction = "horizontal",
})

local Terminal = require("toggleterm.terminal").Terminal
