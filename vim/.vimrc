" ~/.vimrc
" Settings for vim.

" color!
syntax on
set colorcolumn=80

" Turn backup off
set nobackup
set nowb
set noswapfile

" Automatically cd into file directory.
set autochdir

" Limit syntax highlighting (for speed)
set synmaxcol=120

set foldlevelstart=999

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

" Necesary for lots of cool vim things
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

" make vim redraw screen instead of scrolling when there are more than 3 lines 
" to be scrolled
set ttyscroll=999	  

" do not redraw while running macros
set lazyredraw

" Offers a bit of context when scrolling
set scrolloff=1
set sidescrolloff=5

" show current typing
set showcmd

" Makefiles -> Tab characters
autocmd FileType make setlocal noexpandtab shiftwidth=8 softtabstop=0

au BufNewFile,BufRead *.rs set filetype=rust

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

" round indent to multiple of 'shiftwidth'
set shiftround

" show matching parentheses for 1/10 of a second
set matchtime=1

" Update Vim based on filetype.
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on

" Navigate windows with C-hjkl
map<C-h> <C-w>h
map<C-j> <C-w>j
map<C-k> <C-w>k
map<C-l> <C-w>l

" Tired of clearing highlighted searches by searching for dsfhjkhgakjks
" Use this:
nmap <silent> ,/ :nohlsearch<CR>

let windowid=v:windowid

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

" ; as :
map; :
map;; ;

" Spellbad background color is UGLY!
hi clear SpellBad
"
" Disable comment continuing (hopefully)
au FileType c,cpp setlocal comments-=:// comments+=f://

" Hide the default --insert-- etc. text, I've got powerline.
set laststatus=2 " Always display the statusline in all windows
set noshowmode

" Disable automatic commenting
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

set grepprg=grep\ -nH\ $*
let g:tex_conceal = 1
let g:tex_flavor='tex'

" Comma is my <leader>.
let mapleader=","

nnoremap ,cs :let @*=expand("%")<CR>
nnoremap ,cl :let @*=expand("%:p")<CR>

nnoremap <space> za
inoremap <c-d> <esc>ddi
nnoremap <c-d> dd
inoremap <c-u> <esc>lbvwUi
nnoremap <c-u> bvwU
nnoremap - dd
map <f9> :make<CR>

augroup indent_settings
    au!
    au BufEnter *.c setl autoindent smartindent tabstop=8 expandtab shiftwidth=8
    au BufEnter *.h setl autoindent smartindent tabstop=8 expandtab shiftwidth=8
    au BufEnter *.rs setl autoindent smartindent tabstop=4 expandtab shiftwidth=4
    au BufEnter *.rb setl autoindent smartindent tabstop=2 expandtab shiftwidth=2
    au BufEnter *.erb setl autoindent smartindent tabstop=2 expandtab shiftwidth=2

map<leader>w :w<CR>
map<leader>W :wq<CR>
map<leader>q :q<CR>
nnoremap <leader><leader> <C-w><C-w>
nnoremap <leader>\ :vsplit<CR>
nnoremap <leader>- :split<CR>

" Add Vundle to the path.
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'L9'
Plugin 'gmarik/Vundle.vim'

Plugin 'sjl/gundo.vim'
map<leader>g :GundoToggle<CR>

Plugin 'jcrocholl/pep8'

Plugin 'scrooloose/syntastic'

Plugin 'fs111/pydoc.vim'

Plugin 'zenburn'

Plugin 'scrooloose/nerdtree'
nnoremap <leader>h :NERDTree<CR>

Plugin 'tpope/vim-fugitive'
map <leader>c :Gcommit<CR>
map <leader>s :Gstatus<CR>

Plugin 'vim-pandoc/vim-pandoc'
let g:pandoc_auto_format = 0
let g:pandoc_no_empty_implicits = 1
let g:pandoc_no_spans = 1
let g:pandoc_use_hard_wraps = 1

Plugin 'vim-scripts/mail.vim'

Plugin 'FuzzyFinder'
map<leader>f :FufFileWithCurrentBufferDir **/<C-M> 
map<leader>b :FufBuffer<C-M>

Plugin 'jalvesaq/VimCom'

Plugin 'bling/vim-airline'

Plugin 'Yggdroot/indentLine'

Plugin 'LaTeX-Box-Team/LaTeX-Box'
let g:LatexBox_output_type="pdf"
let g:LatexBox_viewer="zathura"
let g:LatexBox_latexmk_async=0
let g:LatexBox_quickfix=0 "don't open the quickfix window automatically
let g:LatexBox_build_dir=""

Plugin 'kien/ctrlp.vim'
let g:ctrlp_working_path_mode = 'ra'

Plugin 'majutsushi/tagbar'

Plugin 'jceb/vim-orgmode'

Plugin 'chrisbra/csv.vim'

Plugin 'wting/rust.vim'

Plugin 'haya14busa/incsearch.vim'
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

Plugin 'tpope/vim-surround'
autocmd BufNewFile,BufRead *.json set ft=javascript

call vundle#end()

colorscheme zenburn
