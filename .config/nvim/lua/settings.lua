-- General Settings.
vim.opt.number = true                   -- Print the line number in front of each line.
vim.opt.showmode = false                -- If in Insert, Replace or Visual mode put a message on the last line.
vim.opt.termguicolors = true            -- Enables 24-bit RGB color in the TUI. 
vim.opt.mouse = 'a'                     -- Enable the use of the mouse.

-- Indentation Configuartion.
vim.opt.tabstop = 2                     -- Number of spaces that a <Tab> in the file counts for.
vim.opt.shiftwidth = 2                  -- Number of spaces to use for each step of (auto)indent.
vim.opt.expandtab = true                -- In Insert mode: Use the appropriate number of spaces to insert a <Tab>.
vim.opt.smartindent = true              -- Do smart autoindenting when starting a new line.

-- Keybindings.
vim.keymap.set('n', '<leader>t', ':NERDTreeToggle<cr>')
--Go to tab by number
vim.keymap.set('n', '<leader>1', '1gt')
vim.keymap.set('n', '<leader>2', '2gt')
vim.keymap.set('n', '<leader>3', '3gt')
vim.keymap.set('n', '<leader>4', '4gt')
vim.keymap.set('n', '<leader>5', '5gt')
vim.keymap.set('n', '<leader>6', '6gt')
vim.keymap.set('n', '<leader>7', '7gt')
vim.keymap.set('n', '<leader>8', '8gt')
vim.keymap.set('n', '<leader>9', '9gt')
vim.keymap.set('n', '<leader>0', ':tablast<cr>')
-- Find files using Telescope command-line sugar.
vim.keymap.set('n', '<leader>ff', '<cmd>Telescope find_files hidden=true<cr>')
vim.keymap.set('n', '<leader>fg', '<cmd>Telescope live_grep<cr>')
vim.keymap.set('n', '<leader>fb', '<cmd>Telescope buffers<cr>')
vim.keymap.set('n', '<leader>fh', '<cmd>Telescope help_tags<cr>')

-- Airline Configuration.
vim.g.airline_powerline_fonts = 1
vim.g['airline#extensions#tabline#enabled'] = 1

-- NerdTree Configuration.
vim.g.NERDTreeDirArrowExpandable = '->'
vim.g.NERDTreeDirArrowCollapsible = '<-'
-- Start NERDTree and put the cursor back in the other window.
vim.api.nvim_create_autocmd({"VimEnter"}, {
  command = "NERDTree | wincmd p"
})
-- Open the existing NERDTree on each new tab.
vim.api.nvim_create_autocmd({"BufWinEnter"}, {
  command = "if getcmdwintype() == '' | silent NERDTreeMirror | endif",
})
-- Close the tab if NERDTree is the only window remaining in it.
vim.api.nvim_create_autocmd({"BufEnter"}, {
  command = "if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif",
})
-- Exit Vim if NERDTree is the only window remaining in the only tab.
vim.api.nvim_create_autocmd({"BufEnter"}, {
  command = "if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif",
})

-- Gruvbox Configuration.
vim.opt.background = 'dark'             -- Enable "dark" or "light" mode.
vim.g.gruvbox_bold = 1                  -- Enables bold text.
vim.g.gruvbox_italic = 1                -- Enables italic text.
vim.g.gruvbox_transparent_bg = 0        -- Enables transparent background.
vim.g.gruvbox_underline = 1             -- Enables underlined text.
vim.g.gruvbox_undercurl = 1             -- Enables undercurled text.
vim.g.gruvbox_termcolors = 256          -- Uses 256-color palette.
vim.g.gruvbox_contrast_dark = 'hard'    -- Changes dark mode contrast. Possible values are soft, medium and hard.
vim.g.gruvbox_contrast_light = 'hard'   -- Changes light mode contrast. Possible values are soft, medium and hard.
vim.g.gruvbox_hls_cursor = 'orange'     -- Changes cursor background while search is highlighted. Possible values are any of gruvbox palette.
vim.g.gruvbox_number_column = 'bg0'     -- Changes number column background color. Possible values are any of gruvbox palette.
vim.g.gruvbox_sign_column = 'bg1'       -- Changes sign column background color. Possible values are any of gruvbox palette.
vim.g.gruvbox_color_column = 'bg1'      -- Changes color column background color. Possible values are any of gruvbox palette.
vim.g.gruvbox_vert_split = 'bg0'        -- Changes vertical split background color. Possible values are any of gruvbox palette.
vim.g.gruvbox_italicize_comments = 1    -- Enables italic for comments.
vim.g.gruvbox_italicize_strings = 0     -- Enables italic for strings.
vim.g.gruvbox_invert_selection = 1      -- Inverts selected text.
vim.g.gruvbox_invert_signs = 0          -- Inverts GitGutter and Syntastic signs. Useful to rapidly focus on.
vim.g.gruvbox_invert_indent_guides = 0  -- Inverts indent guides.
vim.g.gruvbox_invert_tabline = 0        -- Inverts tabline highlights, providing distinguishable tabline-fill.
