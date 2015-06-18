"
" vim configuration
"
" author: Volodymyr Prokopyuk
"
" Statusline
set laststatus=2
set statusline=[%n,%{v:register}]\ %<%F\ %m\ %r\ %y\ [%{&fenc}]\ [%{&ff}]\ (%l,%v)\ %L\ %p%%
" current directory
set autochdir

" line numbers
set number
" highlight current line
set cursorline
" 80 columns margin
set colorcolumn=81
" line wrapping
" :vip
" :gq
set textwidth=80

" font
" :set guifont # check current font
"set guifont=Ubuntu\ Mono\ 12
"set guifont=PT\ Mono\ 12
set guifont=Source\ Code\ Pro\ Light\ 12
" baselineskip
set linespace=3

" color
filetype on
syntax on
" Solarized color scheme
" git clone https://github.com/altercation/vim-colors-solarized.git
" cp ./solarized.vim ~/.vim/colors
"let g:solarized_termcolors=256
"set background=light
"set background=dark
"colorscheme solarized
" Zenburn color scheme
" git clone https://github.com/jnurmine/Zenburn.git
" cp Zenburn/colors/zenburn.vim ~/.vim/colors
colors zenburn

" show tabs and trailing spaces
set listchars=tab:>-,trail:-
set list
" remove trailing white spaces on save
autocmd BufWritePre * :%s/\s\+$//e
" Tab substitution
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

" auto indentation
filetype indent on
set autoindent " shiftwidth
set smartindent
" show matching brace
set showmatch
" show mode (INSERT, VISUAL)
set showmode
" show typed command
set showcmd

" incremental search
set incsearch
set hlsearch
set ignorecase
set smartcase

" GUI configuration
if has("gui_running")
  " remove Toolbar
  set guioptions-=T
  " remove Menubar
  set guioptions-=m
  " remove Scrollbar
  set guioptions+=LlRrb
  set guioptions-=LlRrb

  " spell checker
  " http://ftp.vim.org/vim/runtime/spell/
  " ftp://ftp.vim.org/pub/vim/runtime/spell
  " /usr/share/vim/vim74/spell
  set spell
  set spelllang=en,es
  set spellsuggest=14
  " :set syntax=txt
endif

"""""""""""""""""""""""""""""""""""""""""
" set file encoding
" :set bomb
" :set fileencoding=utf-8
" :w
" set file format
" :set ff=unix
" :w
