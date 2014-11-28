" ~/.vimrc
" Settings for vim.

" Turn backup off
set nobackup
set nowb
set noswapfile

" Comma is my <leader>.
let mapleader=","

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

" Use my own ftplugins
":autocmd BufEnter * let b:did_ftplugin = 1
" Needed for Syntax Highlighting and stuff
filetype on

" Autoindent
filetype indent on

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

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle
" required! 
Plugin 'gmarik/Vundle.vim'
" }}}

Plugin 'sjl/gundo.vim'
Plugin 'jcrocholl/pep8'
Plugin 'scrooloose/syntastic'
Plugin 'ervandew/supertab'
Plugin 'altercation/vim-colors-solarized'
Plugin 'fs111/pydoc.vim'
"Plugin 'davidhalter/jedi-vim'
Plugin 'zenburn'
Plugin 'w0ng/vim-hybrid'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-scripts/mail.vim'
Plugin 'FuzzyFinder'
Plugin 'jalvesaq/VimCom'
Plugin 'bling/vim-airline'
Plugin 'Yggdroot/indentLine'
Plugin 'troydm/easybuffer.vim'
Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'vim-scripts/taglist.vim'
Plugin 'DirDiff.vim'
Plugin 'nanotech/jellybeans.vim'
Plugin 'chriskempson/base16-vim'
Plugin 'jceb/vim-orgmode'
Plugin 'chrisbra/csv.vim'
Plugin 'wting/rust.vim'
Plugin 'tpope/vim-surround'
"Bundle 'Vim-R-plugin'
"Bundle 'gerw/vim-latex-suite'
call vundle#end()
filetype plugin on
filetype plugin indent on

let g:ctrlp_working_path_mode = 'ra'

" Don't make symbols in latex documents
let g:tex_conceal = 1

" color settings
syntax on
set background=dark
let g:hybrid_use_Xresources = 1
colorscheme zenburn

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

" " ; as : always
" Bad practice. Servers without my .vimrc make vim hard to use.
map; :
map;; ;

" NERDTree toggle
map<C-n> :NERDTreeToggle<CR>

" USE GUNDO
map<leader>g :GundoToggle<CR>

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
map<leader>j :RopeGotoDefinition<CR>
map<leader>r :RopeRename<CR>

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

" Automatically run NERDTree
"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p
" Latex-suite stuff
" -----------------
"
"" CTAGS FOR LATEX
let tags='./tags'
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'
let g:tagbar_type_tex = {
    \ 'ctagstype' : 'latex',
    \ 'kinds'     : [
        \ 's:sections',
        \ 'g:graphics:0:0',
        \ 'l:labels',
        \ 'r:refs:1:0',
        \ 'p:pagerefs:1:0'
    \ ],
    \ 'sort'    : 0
\ }

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='tex'
let g:Tex_DefaultTargetFormat='pdf'

" latex-search for synctex
map<silent> <Leader>ls :silent
                \ !/Applications/Skim.app/Contents/SharedSupport/displayline
                \ <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>"
                \ "%:p" <CR>

let g:vimwiki_list = [{'path': '~/docs/vimwiki'}]
" wrap wiki to 72 col
au BufEnter *.wiki setlocal textwidth=72
au BufEnter *.wiki setlocal wrapmargin=2

let g:pandoc_auto_format = 0
let g:pandoc_no_empty_implicits = 1
let g:pandoc_no_spans = 1
let g:pandoc_use_hard_wraps = 1
let g:LatexBox_latexmk_async = 1

" FuzzyFinder
map<leader>f :FufFileWithCurrentBufferDir **/<C-M> 
map<leader>b :FufBuffer<C-M>
" indents
augroup indent_settings
    au!
    au BufEnter *.c setl autoindent smartindent tabstop=8 expandtab shiftwidth=8
    au BufEnter *.h setl autoindent smartindent tabstop=8 expandtab shiftwidth=8
    au BufEnter *.rs setl autoindent smartindent tabstop=4 expandtab shiftwidth=4

set colorcolumn=80

"filename copying
nnoremap ,cs :let @*=expand("%")<CR>
nnoremap ,cl :let @*=expand("%:p")<CR>

" latex box
let g:LatexBox_output_type="pdf"
let g:LatexBox_viewer="zathura"
let g:LatexBox_latexmk_async=0
let g:LatexBox_quickfix=0 "don't open the quickfix window automatically
let g:LatexBox_build_dir=""

" make space more useful
nnoremap <space> za

" round indent to multiple of 'shiftwidth'
set shiftround

" show matching parentheses for 1/10 of a second
set matchtime=1

inoremap <c-d> <esc>ddi
nnoremap <c-d> dd
inoremap <c-u> <esc>lbvwUi
nnoremap <c-u> bvwU
nnoremap - dd
map <leader>c :Gcommit<CR>
map <leader>s :Gstatus<CR>
" save
map<leader>w :w<CR>
" save as ...
map<leader>W :wq<CR>
" quit
map<leader>q :q<CR>
"unmap <leader>ww

map <f9> :make<CR>
