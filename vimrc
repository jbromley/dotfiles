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
set backupdir=~/.cache/nvim/tmp,/var/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.cache/nvim/tmp,/var/tmp
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

" Plugins {{{
call plug#begin()

" Themes
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'sonph/onehalf'
Plug 'bluz71/vim-nightfly-guicolors'
Plug 'mhartington/oceanic-next'
Plug 'barlog-m/oceanic-primal-vim', {'branch': 'main'}

" FZF 
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" NERDTree - file tree navigation
Plug 'preservim/nerdtree'

" Git plugin
Plug 'tpope/vim-fugitive'

" Database interaction
Plug 'tpope/vim-dadbod'

" VimWiki
Plug 'vimwiki/vimwiki'

" Unicode
Plug 'chrisbra/unicode.vim'

" Commentary
Plug 'tpope/vim-commentary'

" Elixir
Plug 'elixir-editors/vim-elixir'

" YouCompleteMe
Plug 'ycm-core/YouCompleteMe'

call plug#end()
"}}}

" Plugin configuration and key bindings {{{

" Key bindings for fzf
nmap ; :Buffers<CR>
nmap <Leader>t :Tags<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>a :Ag<CR>

" NerdTree configuration
map <F2> :NERDTreeToggle<CR>
let g:NERDTreeIgnore=['\~$', '__pycache__', '.git']

" VimWiki
let g:vimwiki_list = [{'path': '~/Documents/VimWiki/'}]

" vim-commentary
autocmd FileType c,cpp,cs,java setlocal commentstring=//\ %s

" YouCompleteMe
autocmd CompleteDone * pclose
if has('mac')
    let g:ycm_clangd_binary_path = "/opt/clang+llvm-10.0.0-x86_64-apple-darwin/bin/clangd"
else
    let g:ycm_clangd_binary_path = "/usr/bin/clangd"
endif
let g:ycm_language_server = 
  \  [
  \    {
  \       'name': 'elixir',
  \       'cmdline': [ '/opt/elixir-ls/language_server.sh' ],
  \       'filetypes': [ 'elixir' ],
  \       'project_root_files': [ 'mix.exs' ]
  \    }
  \  ]
let g:ycm_auto_hover = ''
let g:ycm_always_populate_location_list = 1
let g:ycm_error_symbol = '!!'
let g:ycm_warning_symbol = '??'
map <Leader>i :YcmCompleter GoTo<CR>
map <Leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
map <Leader>r :YcmCompleter GoToReferences<CR>
map <Leader>s :YcmCompleter GoToSymbol<CR>
map <Leader>k :YcmCompleter GetDoc<CR>
map <Leader>y :YcmCompleter GetType<CR>
map <Leader>x :YcmCompleter FixIt<CR>
map <Leader>D <Plug>(YCMHover)
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

if (has('nvim'))
    " colorscheme nightfly
    " colorscheme solarized
    " colorscheme oceanic-primal
    colorscheme OceanicNext
endif

" Modeline
set modeline
set modelines=1

set number
set showcmd
set wildmenu
set lazyredraw
set showmatch

" Show marker at text width
set textwidth=80
set colorcolumn=+1

" Status line configuration
set statusline=\ %n\ 
set statusline+=%1*\ %f\ %m\ %r\ 
set statusline+=%2*\ %{FugitiveStatusline()}\  
set statusline+=%9*\ "%{StatuslineMode()}
set statusline+=%=
set statusline+=%#warningmsg#
set statusline+=%*
set statusline+=%y\ %{&fileencoding?&fileencoding:&encoding}\ [%{&fileformat}]\ 
set statusline+=%1*\ L\ %l/%L\ C\ %c\ (%P)
set laststatus=2
hi User1 ctermbg=darkblue ctermfg=white guibg=darkblue guifg=white
hi User2 ctermbg=magenta ctermfg=white guibg=magenta guifg=white
hi User9 ctermbg=black ctermfg=white guibg=black guifg=white
"}}}


" vim: foldmethod=marker:foldlevel=0