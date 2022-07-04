-- This file can be loaded by calling `lua require('plugins')` from your init.vim
return require('packer').startup(function(use)
  -- Packer can manage itself.
  use 'wbthomason/packer.nvim'
  
  -- Port of gruvbox community theme to lua.
  use 'gruvbox-community/gruvbox'

  -- Start screen.
  use 'mhinz/vim-startify'

  -- File system explorer.
  use {
    'preservim/nerdtree',
    requires = 'ryanoasis/vim-devicons'
  }
 
  -- Lean & mean status/tabline.
  use 'vim-airline/vim-airline'

  -- Git Integrations.
  -- Fugitive is the premier Vim plugin for Git.
  use 'tpope/vim-fugitive'
  -- Shows a git diff in the sign column.
  use 'airblade/vim-gitgutter'
  -- Integrates yadm with vim-fugitive and vim-gitgutter.
  use 'seanbreckenridge/yadm-git.vim'
end)
