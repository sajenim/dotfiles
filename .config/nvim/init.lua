-- Imports.
require('plugins')
require('settings')

-- Setup Plugins.

-- Set the colorscheme.
vim.cmd("colorscheme gruvbox")

-- Start NERDTree and put the cursor back in the other window.
vim.cmd([[autocmd VimEnter * NERDTree | wincmd p]])
-- Open the existing NERDTree on each new tab.
vim.cmd([[autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif]])
-- Close the tab if NERDTree is the only window remaining in it.
vim.cmd([[autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif]])

-- Automatically run :PackerCompile whenever plugins.lua is updated.
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])
