local vi_mode_utils = require('feline.providers.vi_mode')

-- Initialize the components table.
local components = {
  active = {},
  inactive = {}
}

-- Insert two sections (left, and right) for the active statusline.
table.insert(components.active, {})
table.insert(components.active, {})

-- Insert two sections (left and right) for the inactive statusline.
table.insert(components.inactive, {})
table.insert(components.inactive, {})

-- Define default colors.
local colors = {
  --
  bg = '#1d2021',
  fg = '#ebdbb2',
  --
  gray_dark = '#928374',
  gray_light = '#a89984',
  --
  black = '#1d2021',
  red = '#cc241d',
  green = '#98971a',
  yellow = '#d79921',
  blue = '#458588',
  magenta = '#b16286',
  cyan = '#689d6a',
  white = '#a89984',
}

-- Define vi mode colors.
local vi_mode_colors = {
    NORMAL = colors.gray_dark,
    ['OP'] = 'green',
    INSERT = colors.blue,
    VISUAL = colors.orange,
    ['LINES'] = 'skyblue',
    ['BLOCK'] = 'skyblue',
    ['REPLACE'] = 'violet',
    ['V-REPLACE'] = 'violet',
    ['ENTER'] = 'cyan',
    ['MORE'] = 'cyan',
    ['SELECT'] = 'orange',
    ['COMMAND'] = 'green',
    ['SHELL'] = 'green',
    ['TERM'] = 'green',
    ['NONE'] = 'yellow',
}

-- Display active vi mode.
components.active[1][1] = {
  provider = 'vi_mode',
  hl = function()
    return {
      name = vi_mode_utils.get_mode_highlight_name(),
      fg = vi_mode_utils.get_mode_color(),
      style = 'bold'
    }
  end,
  icon = '',
  left_sep = {
    str = '[ ',
  },
  right_sep = '] '
}

-- Display filename.
components.active[1][2] = {
  provider = { 
    name = 'file_info',
    opts = {
      type = 'unique',
      file_modified_icon = 'M',
      file_readonly_icon = 'RO'
    }    
  },
  icon = ''
}

-- Initialize feline with provided configuration.
require('feline').setup({
  colors = colors,
  components = components,
  vi_mode_colors = vi_mode_colors,
})
