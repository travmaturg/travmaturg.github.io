" wrong defaults
set nocompatible
set backspace=indent,eol,start
set ruler

" indentation
set autoindent
set expandtab
set shiftwidth=2
set tabstop=2

" text color
syntax enable
set background=dark
colorscheme koehler
set hlsearch

" gui
set guioptions-=T
set guifont=Monospace\ 10

set foldcolumn=3
set linebreak

" delete trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

