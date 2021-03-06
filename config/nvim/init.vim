" Neovim configuration

" Options {{{
set splitbelow splitright
set clipboard+=unnamedplus

" Tab settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent

" Turn on autowrite.
set autowrite

" Ignore case when searching unless a capital letter is present.
set ignorecase smartcase

" File and script encoding settings.
set fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,latin1
scriptencoding utf-8

" Ignore certain files when globbing.
set wildignore+=*.o,*.obj
set wildignore+=*/.git/*,*/.svn/*,*/__pycache__/,*/build/**
set wildignore+=*.pyc
set wildignore+=.DS_Store
set wildignore+=*.aux,*.bbl,*.blg,*.brf,*.fls

" Searching 
set path+=**
set ignorecase          " ignore case when searching
set incsearch           " search as characters are entered
set hlsearch            " highlight all matches

" Folding
" set foldlevel=0
set foldmethod=indent   " fold based on indent level
set foldnestmax=10      " max 10 depth
set foldenable          " don't fold files by default on open
set foldlevelstart=10   " start with fold level of 1

"Size of command history
set history=500

" Use list mode and customized listchars
if has('multi_byte') && &encoding ==# 'utf-8'
    let &listchars='tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
    let &listchars='tab:> ,extends:>,precedes:<,nbsp:.'
endif

" Backups
set backup
set backupdir=~/.cache/nvim/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.cache/nvim/tmp
set writebackup

filetype plugin indent on
syntax on

" If using the fish shell, spawn things with bash instead.
if &shell =~# 'fish$'
    if has('mac')
        set shell=/opt/local/bin/bash
    else
        set shell=/usr/bin/bash
    endif
endif
" }}}

" Plugin configuration and key bindings {{{

" Key bindings for fzf
nmap ; :Buffers<CR>
nmap <Leader>t :Tags<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>a :Ag<CR>

" NerdTree configuration
map <F2> :NERDTreeToggle<CR>
let g:NERDTreeIgnore=['\~$', '__pycache__', '.git']

function! StartUp()
    if !argc() && !exists("s:std_in")
        NERDTree
    end
    if argc() && isdirectory(argv()[0]) && !exists("s:std_in")
        exe 'NERDTree' argv()[0]
        wincmd p
        ene
    end
endfunction

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * call StartUp()

" VimWiki
let g:vimwiki_list = [{'path': '~/Documents/Wiki/'}]

" vim-commentary
autocmd FileType c,cpp,cs,java setlocal commentstring=//\ %s
autocmd FileType rkt,scm setlocal commentstring=;\ %s

" nvim-lspconfig
lua << EOF
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys 
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
      -- signs = false,
      virtual_text = false,
  }
)
  
-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { "bashls", "clangd", "gopls", "racket_langserver" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end
EOF

" lspsaga
" lua << EOF
" local saga = require 'lspsaga'
" saga.init_lsp_saga()

" require 'lspsaga.diagnostic'.show_line_diagnostics()
" require 'lspsaga.diagnostic'.show_cursor_diagnostics()
" EOF

" }}}

" Functions{{{
function! StatuslineMode()
  let l:mode=mode()
  if l:mode==#"n"
    return "NORMAL"
  elseif l:mode==?"v"
    return "VISUAL"
  elseif l:mode==#"i"
    return "INSERT"
  elseif l:mode==#"R"
    return "REPLACE"
  elseif l:mode==?"s"
    return "SELECT"
  elseif l:mode==#"t"
    return "TERMINAL"
  elseif l:mode==#"c"
    return "COMMAND"
  elseif l:mode==#"!"
    return "SHELL"
  endif
endfunction

" function! LinterStatus() abort
"     let l:counts = ale#statusline#Count(bufnr(''))
"     let l:all_errors = l:counts.error + l:counts.style_error
"     let l:all_non_errors = l:counts.total - l:all_errors
"     return l:counts.total == 0 ? 'OK' : printf('%dW %dE', all_non_errors, all_errors)
" endfunction
" }}}

" Autocommands {{{ 
augroup dynamic_smartcase
    autocmd!
    autocmd CmdLineEnter : set nosmartcase
    autocmd CmdLineLeave : set smartcase
augroup END

augroup cursorline_active_window
    autocmd!
    autocmd WinEnter * set cursorline
    autocmd WinLeave * set nocursorline
augroup END

" Set up Python buffers
augroup python_buffer 
    autocmd!
    autocmd FileType python setlocal signcolumn=yes
    autocmd FileType python nnoremap <Leader>b V:s/[,)]/&\r/g <cr>='<
    autocmd BufWritePre *.py %s/\s\+$//e
augroup END
"}}}

" Key bindings {{{
" Turn word under cursor to upper case.
inoremap <silent> <C-u> <Esc>viwUea

" Turn current word into title case.
inoremap <silent> <C-t> <Esc>b~lea

" Make window navigation quicker
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Location and quickfix list navigation
nnoremap [l :lprevious<CR>zv
nnoremap ]l :lnext<CR>zv
nnoremap [L :lfirst<CR>zv
nnoremap ]L :llast<CR>zv
nnoremap [q :cprevious<CR>zv
nnoremap ]q :cnext<CR>zv
nnoremap [Q :cfirst<CR>zv
nnoremap ]Q :clast<CR>zv

" Close location or quickfix list if present.
nnoremap <silent> <Leader>x :windo lclose <bar> cclose<CR>

" Close a buffer and switch to other buffer.
nnoremap <silent> <Leader>d :bprevious <bar> bdelete #<CR>

" Toggle search highlight.
nnoremap <silent><expr> <Leader>hl (&hls && v:hlsearch ? ':nohls' : ':set hls')."\n"

" Insert a space after current character.
nnoremap <silent> <Space><Space> a<Space><Esc>h

" Decrease indent level in insert mode.
inoremap <S-Tab> <Esc><<i
"}}}

" UI {{{

" Color scheme
set termguicolors
colorscheme dracula 

" Modeline
set modeline
set modelines=1

set number
set showcmd
set wildmenu
set lazyredraw
set showmatch
set foldmethod=marker 

" Show marker at text width
set textwidth=80
set colorcolumn=+1

" Status line configuration
set statusline=\ %n\ 
set statusline+=\ %f\ %m\ %r\ 
set statusline+=\ %{FugitiveStatusline()}\  
set statusline+=\ "%{StatuslineMode()}
set statusline+=%=
set statusline+=%#warningmsg#
set statusline+=%*
set statusline+=%y\ %{&fileencoding?&fileencoding:&encoding}\ [%{&fileformat}]\ 
set statusline+=\ L\ %l/%L\ C\ %c\ (%P)
set laststatus=2
"}}}

" vim: foldmethod=marker:foldlevel=0
