-- This file can be loaded by calling `lua require('plugins')` from your init.vim
return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'
  
  -- Designed as a bright theme with pastel 'retro groove' colors and light/dark mode switching.
  -- The main focus when developing gruvbox is to keep colors easily distinguishable, contrast enough and still pleasant for the eyes.
  use 'morhetz/gruvbox'
  
  -- Lean & mean status/tabline for vim that's light as air.
  use {
    'vim-airline/vim-airline',
    requires = 'ryanoasis/vim-devicons'
  }

  -- The NERDTree is a file system explorer for the Vim editor.
  use {
    'preservim/nerdtree',
    requires = 'ryanoasis/vim-devicons'
  }

  -- Start screen for Vim and Neovim.
  use 'mhinz/vim-startify'

  -- Highly extendable fuzzy finder over lists.
  use {
  'nvim-telescope/telescope.nvim',
  requires = 'nvim-lua/plenary.nvim'
  }

  -- Smart and Powerful commenting plugin for neovim.
  use {
    'numToStr/Comment.nvim',
    config = function()
        require('Comment').setup()
    end
  }
end)
