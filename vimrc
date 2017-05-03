set hidden "Permite poner en background sin guardar
set nocompatible "Funcionalidades no compatibles con vi
set history=100
set background=light
" set background=dark
set incsearch " Busqueda incremental
set hlsearch " dejar iluminada la busqueda
set showmatch " que ilumine el parentesis que coincide
" set is
" set si
" :au Filetype html,xml,xsl source ~/vim/closetag.vim 
filetype indent on
filetype plugin on
" set grepprg=grep\ -nH\ $*

"helptags ~/.vim/doc
"autocmd FileType python set omnifunc=pythoncomplete#Complete
"source ~/.vim/plugin/vimballPlugin.vim "plugin vimball

" Navegacion entre tabs
"map <silent><A-Right> :tabnext<CR>
"map <silent>&ltA-Left> :tabprevious<CR>

"runtime macros/matchit.vim "Match expandido
set wildmode=list:longest "Para el completion tipo bash
set scrolloff=3


"let Tlist_File_Fold_Auto_Close = 1

"map T :TaskList<CR>
"map <buffer> <S-e> :w<CR>:!/usr/bin/env python % <CR>
"map P :TlistToggle<CR>

"set invlist

set expandtab
"set textwidth=79
set tabstop=8
set softtabstop=4
set shiftwidth=4
set autoindent
set syntax=on
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1


set statusline=%t       "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file

set laststatus=2

