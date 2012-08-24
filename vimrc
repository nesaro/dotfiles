set hidden "Permite poner en background sin guardar
set nocompatible "Funcionalidades no compatibles con vi
set history=100
" set number "Anteponer un numero
set background=light
" set background=dark
set incsearch " Busqueda incremental
set hlsearch " dejar iluminada la busqueda
set showmatch " que ilumine el parentesis que coincide
" set is
" set ic " ignorar mayusculas
" set si
" source /usr/share/vim/vimfiles/ftplugin/python/python.vim
" source /home/bourbaki/vim/vimspell.vim
" source /home/bourbaki/vim/ada.vim
" source $VIMRUNTIME/indent/cpp.vim
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
set ruler


"let Tlist_File_Fold_Auto_Close = 1

"map T :TaskList<CR>
"map <buffer> <S-e> :w<CR>:!/usr/bin/env python % <CR>
"map P :TlistToggle<CR>

"set invlist

set expandtab
set textwidth=79
set tabstop=8
set softtabstop=4
set shiftwidth=4
set autoindent
:syntax on
map T :TaskList<CR>
map P :TlistToggle<CR>
