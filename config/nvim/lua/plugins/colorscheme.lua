return {
  {
    "dracula/vim",
    priority = 1000,
    lazy = false,
  },
  {
    "miikanissi/modus-themes.nvim",
    priority = 1000,
    lazy = false,
  },
  {
    "zenbones-theme/zenbones.nvim",
    dependencies = "rktjmp/lush.nvim",
    lazy = false,
    priority = 1000,
    -- You can set set configuration options here.
    -- config = function()
    --     vim.g.zenbones_darken_comments = 45
    --     vim.cmd.colorscheme('zenbones')
    -- end
  },
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "default",
      background = light,
    },
  },
}
