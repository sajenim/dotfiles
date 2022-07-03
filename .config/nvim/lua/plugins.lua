-- This file can be loaded by calling `lua require('plugins')` from your init.vim
return require('packer').startup(function(use)
  -- Packer can manage itself.
  use 'wbthomason/packer.nvim'
  
  -- Port of gruvbox community theme to lua.
  use 'gruvbox-community/gruvbox'

  -- Start screen.
  use 'mhinz/vim-startify'
end)
