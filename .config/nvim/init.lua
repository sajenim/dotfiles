require('settings')
require('plugins')
require('statusline')
require('nvim-tree').setup {}

-- Enable gruvbox colorscheme.
vim.cmd([[colorscheme gruvbox]])
-- Automatically run :PackerCompile whenever plugins.lua is updated.
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])
