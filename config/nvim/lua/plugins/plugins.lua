-- Additional plugins and plugin configurations

return {
  {
    "echasnovski/mini.indentscope",
    opts = {
      draw = { animation = require("mini.indentscope").gen_animation.none() },
    },
  },
  {
    "benknoble/vim-racket",
  },
  {
    "Olical/conjure",
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        racket_langserver = {
          cmd = { "racket", "--lib", "racket-langserver" },
          filetypes = { "racket", "scheme" },
        },
      },
    },
  },
}
