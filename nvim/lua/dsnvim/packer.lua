vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

use {
  'nvim-telescope/telescope.nvim', tag = '0.1.1',
-- or                            , branch = '0.1.x',
  requires = { {'nvim-lua/plenary.nvim'} }

  }
use ('mbbill/undotree')
use ('tpope/vim-fugitive')
use ('nvim-tree/nvim-tree.lua')
use('sainnhe/gruvbox-material')
use {"akinsho/toggleterm.nvim", tag = '*', config = function()
  require("toggleterm").setup()
end}

use {
  'nvim-lualine/lualine.nvim',
  requires = { 'nvim-tree/nvim-web-devicons', opt = true }
}
use ('nvim-tree/nvim-web-devicons')
use ('williamboman/nvim-lsp-installer')
use {'romgrk/barbar.nvim'}
use {'blazkowolf/gruber-darker.nvim'}
end)

