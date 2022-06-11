-- Initialize the components table
local components = {
    active = {},
    inactive = {}
}

-- Insert two sections (left, and right) for the active statusline
table.insert(components.active, {})
table.insert(components.active, {})

-- Insert two sections (left and right) for the inactive statusline
table.insert(components.inactive, {})
table.insert(components.inactive, {})

-- Component that shows Vi mode with highlight
components.active[1][1] = {
    provider = 'vi_mode',
    hl = function()
        return {
            name = require('feline.providers.vi_mode').get_mode_highlight_name(),
            fg = require('feline.providers.vi_mode').get_mode_color(),
            style = 'bold'
        }
    end,
    -- Disable icons.
    icon = '',
    left_sep = ' ',
    right_sep = ' '
}

--
components.active[1][2] = {
    provider = 'file_info',
    icon = '',
    right_sep = ' '
}

--
components.active[2][2] = {
  provider = 'line_percentage',
  right_sep = ' '
}

--
components.active[2][1] = {
  provider = 'position',
  right_sep = ' '
}

--
require('feline').setup({
    components = components
})
