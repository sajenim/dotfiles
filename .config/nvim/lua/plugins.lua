-- This file can be loaded by calling `lua require('plugins')` from your init.vim
return require('packer').startup(function(use)
  -- Packer can manage itself.
  use 'wbthomason/packer.nvim'
  
  -- A port of gruvbox community theme to lua with treesitter support!
  use 'ellisonleao/gruvbox.nvim'

  -- A minimal, stylish and customizable statusline / winbar for Neovim written in Lua
  use {
    'feline-nvim/feline.nvim',
    requires = {'kyazdani42/nvim-web-devicons'}
  }

  -- A File Explorer For Neovim Written In Lua.
  use {
    'kyazdani42/nvim-tree.lua',
    requires = {'kyazdani42/nvim-web-devicons'},
  }

  -- Start screen for Vim and Neovim.
  use 'mhinz/vim-startify'

  -- Smart and Powerful commenting plugin for neovim.
  use {
    'numToStr/Comment.nvim',
    config = function() require('Comment').setup() end
  }
end)
