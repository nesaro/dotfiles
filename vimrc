call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'ag.vim/ag.vim'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
call plug#end()

set hidden "Permite poner en background sin guardar
set nocompatible "Funcionalidades no compatibles con vi
set history=100
set incsearch " Busqueda incremental
set hlsearch " dejar iluminada la busqueda
set showmatch
set background=dark
"
"
"if has( "gui_running" )
"    "colo PaperColor
"    colo solarized
"elseif  $TERM =~ '256'
"    colo solarized
"elseif  $TERM == 'screen'
"    set t_Co=256
"    "colo PaperColor
"    colo solarized
"elseif  $COLORTERM == 'gnome-terminal'
"    set t_Co=256
"    "colo PaperColor
"    colo solarized
"else
"    colo desert
"endif


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
syntax on

set laststatus=2

let g:airline#extensions#obsession#enabled = 1
"let g:airline#extensions#tabline#enabled = 1
let g:airline_section_c = "%<%<%{bufnr('%')} %{airline#extensions#fugitiveline#bufname()}%m %#__accent_red#%{airline#util#wrap(airline#parts#readonly(),0)}%#__restore__#"

"autocmd BufEnter *.md :setlocal filetype=markdown
:set wildignore+=venv/**
:set wildignore+=venv3/**

cabbrev lvim
      \ lvim /\<lt><C-R><C-W>\>/gj
      \ *<C-R>=(expand("%:e")=="" ? "" : ".".expand("%:e"))<CR>
      \ <Bar> lw
      \ <C-Left><C-Left><C-Left>


command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
  echo a:cmdline
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
     if part[0] =~ '\v[%#<]'
        let expanded_part = fnameescape(expand(part))
        let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
     endif
  endfor
  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, 'You entered:    ' . a:cmdline)
  call setline(2, 'Expanded Form:  ' .expanded_cmdline)
  call setline(3,substitute(getline(2),'.','=','g'))
  execute '$read !'. expanded_cmdline
  setlocal nomodifiable
  1
endfunction

nmap <F6> :.w !bash <CR>
nmap <F7> :execute getline(".") <CR>
set nomodeline
nnoremap gb :ls<CR>:b<Space>
