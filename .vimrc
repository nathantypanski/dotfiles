" Sane vim plugin management.
call pathogen#infect()
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" Automatically cd into file directory.
set autochdir

" Remove any trailing whitespace
autocmd BufRead,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif

" Restore cursor position to where it was before
augroup JumpCursorOnEdit
   au!
   autocmd BufReadPost *
            \ if expand("<afile>:p:h") !=? $TEMP |
            \   if line("'\"") > 1 && line("'\"") <= line("$") |
            \     let JumpCursorOnEdit_foo = line("'\"") |
            \     let b:doopenfold = 1 |
            \     if (foldlevel(JumpCursorOnEdit_foo) > foldlevel(JumpCursorOnEdit_foo - 1)) |
            \        let JumpCursorOnEdit_foo = JumpCursorOnEdit_foo - 1 |
            \        let b:doopenfold = 2 |
            \     endif |
            \     exe JumpCursorOnEdit_foo |
            \   endif |
            \ endif
   " Need to postpone using "zv" until after reading the modelines.
   autocmd BufWinEnter *
            \ if exists("b:doopenfold") |
            \   exe "normal zv" |
            \   if(b:doopenfold > 1) |
            \       exe  "+".1 |
            \   endif |
            \   unlet b:doopenfold |
            \ endif
augroup END

" Necesary  for lots of cool vim things
set nocompatible

" Modelines are neat, but they're a security hole.
set nomodeline

set undolevels=50		" 50 undos - saved in memory
set updatecount=250		" switch every 250 chars, save swap

" When included, as much as possible of the last line
" in a window will be displayed.  When not included, a
" last line that doesn't fit is replaced with "@" lines.
set display+=lastline

set ttyfast				" we have a fast terminal
set scrolljump=5	  " when scrolling up down, show at least 5 lines
set ttyscroll=999	  " make vim redraw screen instead of scrolling when there are more than 3 lines to be scrolled

" This shows what you are typing as a command.  I love this!
set showcmd

" Needed for Syntax Highlighting and stuff
filetype on
filetype plugin on

" Autoindent
filetype indent on

" Who doesn't like autoindent?
set autoindent

" Spaces as tabs
set expandtab
set smarttab

" Tab length
set shiftwidth=4
set softtabstop=4

" Use english for spellchecking, but don't spellcheck by default
if version >= 700
   set spl=en spell
   set nospell
endif

" Cool tab completion stuff
set wildmenu
set wildmode=list:longest,full

" Enable mouse support in console
set mouse=a

" Got backspace?
set backspace=2

" Show line numbers
set number

" Ignoring case is a fun trick
set ignorecase

" And so is Artificial Intellegence!
set smartcase

" Changes behaviour so that it jumps to the next row in the editor (much more natural):
nnoremap j gj
nnoremap k gk

" Incremental searching is sexy
set incsearch

" Highlight things that we find with the search
set hlsearch

" Since I use linux, I want this
let g:clipbrdDefaultReg = '+'

" When I close a tab, remove the buffer
set nohidden

" Set off the other paren
highlight MatchParen ctermbg=4

" map latex-box compilation to <f5> ... change this later to only work in .tex
" docs
map! <F3> :Latexmk
" }}}

" Solarized settings
:set t_co=256
let g:solarized_contrast="high"
let g:solarized_visibility="normal"
let g:solarized_hitrail=0
syntax enable
set background=dark
colorscheme solarized
let g:solarized_termtrans=0
let g:solarized_degrade=0
let g:solarized_bold=1
let g:solarized_underline=1
let g:solarized_italic=1
let g:solarized_termcolors=16
let g:solarized_diffmode="normal"
let g:solarized_menu=1

"Status line gnarliness
set laststatus=2
set statusline=%F%m%r%h%w\ (%{&ff}){%Y}\ [%l,%v][%p%%]

" Navigate windows with C-hjkl
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Tired of clearing highlighted searches by searching for dsfhjkhgakjks
" Use this:
nmap <silent> ,/ :nohlsearch<CR>

" autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p
function RunJTest()
    let cla = matchstr(expand("%:p"), '^.*[/\\]src[/\\]\(test\|java\)[/\\]\zs.*')
    let class = "java -cp \"/usr/share/java/junit.jar;./\" org.junit.runner.JUnitCore fullpackagename.MyClassTest"
    if match(class, "Test") == -1
        let class = class . "Test"
    endif
endfunction
map <F6> <Esc>:echo RunJTest()<CR>

let windowid=v:windowid

"" 80 columns
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/
"

" Current line highlighting
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END

"UTF-8
set enc=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc

" ; as : always
map ; :
noremap ;; ;
