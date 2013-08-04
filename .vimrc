" ~/.vimrc
" Settings for vim.

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

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

" Use my own ftplugins
":autocmd BufEnter * let b:did_ftplugin = 1
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

" Sane vim plugin management.
call pathogen#infect()
call pathogen#incubate()
call pathogen#helptags()

" color settings
syntax enable
let g:hybrid_use_Xresources = 1
colorscheme hybrid

" Navigate windows with C-hjkl
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Tired of clearing highlighted searches by searching for dsfhjkhgakjks
" Use this:
nmap <silent> ,/ :nohlsearch<CR>

" autocmd VimEnter * wincmd p
" function RunJTest()
"     let cla = matchstr(expand("%:p"), '^.*[/\\]src[/\\]\(test\|java\)[/\\]\zs.*')
"     let class = "java -cp \"/usr/share/java/junit.jar;./\" org.junit.runner.JUnitCore fullpackagename.MyClassTest"
"     if match(class, "Test") == -1
"         let class = class . "Test"
"     endif
" endfunction
" map <F6> <Esc>:echo RunJTest()<CR>

let windowid=v:windowid

" " 80 columns
" highlight OverLength ctermbg=red ctermfg=white guibg=#592929
" match OverLength /\%81v.\+/

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

" " ; as : always
" Bad practice. Servers without my .vimrc make vim hard to use.
" map ; :
" noremap ;; ;

" USE GUNDO
map <leader>g :GundoToggle<CR>

" pep8
let g:pep8_map='<leader>8'

" Omnifunc tab completion
"au FileType python set omnifunc=pythoncomplete#Complete
"let g:SuperTabDefaultCompletionType = "context"
"set completeopt=menuone,longest,preview

"Ropevim is also a great tool that will allow you to navigate around your code.
"It supports automatically inserting import statements, goto definition, refactoring, and code completion.
"You'll really want to read up on everything it does, but the two big things I use it for is to jump to function or class definitions quickly and to rename things (including all their references).
"
"For instance, if you are using django and you place your cursor over the class models.
"Model you reference and then called :RopeGotoDefintion, it would jump you straight to the django library to that class definition.
"We already have it installed in our bundles, so we bind it to a key to use it:
map <leader>j :RopeGotoDefinition<CR>
map <leader>r :RopeRename<CR>

" Spellbad background color is UGLY!
hi clear SpellBad
"hi SpellBad cterm=underline
"highlight SpellBad term=reverse ctermbg=1
"
" Disable comment continuing (hopefully)
au FileType c,cpp setlocal comments-=:// comments+=f://

" Hide the default --insert-- etc. text, I've got powerline.
set laststatus=2 " Always display the statusline in all windows
set noshowmode

" Disable automatic commenting
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" 80 column highlight
" highlight OverLength cterm=bold
execute "set colorcolumn=" . join(range(81,335), ',')
" match OverLength /\%81v.\+/

" Automatically run NERDTree
autocmd VimEnter * NERDTree
" Latex-suite stuff
" -----------------
"
" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'
let g:Tex_DefaultTargetFormat='pdf'
