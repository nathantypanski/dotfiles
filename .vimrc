"{{{Auto Commands

" Automatically cd into the directory that the file is in
autocmd BufEnter * execute "chdir ".escape(expand("%:p:h"), ' ')

" Remove any trailing whitespace that is in the file
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

"}}}

"{{{Misc Settings

" Necesary  for lots of cool vim things
set nocompatible
set modeline

set undolevels=10		" 50 undos - saved in memory
set updatecount=250		" switch every 250 chars, save swap

" When included, as much as possible of the last line
" in a window will be displayed.  When not included, a
" last line that doesn't fit is replaced with "@" lines.
set display+=lastline

set title titlelen=150 titlestring=%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}

set ttyfast				" we have a fast terminal
set scrolljump=5	  " when scrolling up down, show at least 5 lines
set ttyscroll=999	  " make vim redraw screen instead of scrolling when there are more than 3 lines to be scrolled

"set tags=tags;/			" search recursively up for tags

" This shows what you are typing as a command.  I love this!
set showcmd

" Folding Stuffs
set foldmethod=marker

" Needed for Syntax Highlighting and stuff
filetype on
filetype plugin on

" Autoindent
filetype indent on

" I don't know.
set grepprg=grep\ -nH\ $*

" Who doesn't like autoindent?
set autoindent

" Spaces are better than a tab character
set expandtab
set smarttab

" Who wants an 8 character tab?  Not me!
set shiftwidth=2
set softtabstop=2

" Use english for spellchecking, but don't spellcheck by default
if version >= 700
   set spl=en spell
   set nospell
endif

" Real men use gcc
"compiler gcc

" Cool tab completion stuff
set wildmenu
set wildmode=list:longest,full

" Enable mouse support in console
set mouse=a

" Got backspace?
set backspace=2

" Line Numbers PWN!
set number

" Ignoring case is a fun trick
set ignorecase

" And so is Artificial Intellegence!
set smartcase

" This is totally awesome - remap jj to escape in insert mode.  You'll never type jj anyway, so it's great!
inoremap jj <Esc>

nnoremap JJJJ <Nop>

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

"{{{Look and Feel

" Favorite Color Scheme
set background=dark
if has("gui_running")
   colorscheme solarized
   " Remove Toolbar
   set guioptions-=T
   "Terminus is AWESOME
   set guifont=Terminus\ 10
else
   colorscheme solarized
endif

"Status line gnarliness
set laststatus=2
set statusline=%F%m%r%h%w\ (%{&ff}){%Y}\ [%l,%v][%p%%]

" }}}

"{{{ Functions

"{{{ Open URL in browser

function! Browser ()
   let line = getline (".")
   let line = matchstr (line, "http[^   ]*")
   exec "!konqueror ".line
endfunction

"}}}

"{{{Theme Rotating
let themeindex=0
function! RotateColorTheme()
   let y = -1
   while y == -1
      let colorstring = "inkpot#ron#blue#elflord#evening#koehler#murphy#pablo#desert#torte#"
      let x = match( colorstring, "#", g:themeindex )
      let y = match( colorstring, "#", x + 1 )
      let g:themeindex = x + 1
      if y == -1
         let g:themeindex = 0
      else
         let themestring = strpart(colorstring, x + 1, y - x - 1)
         return ":colorscheme ".themestring
      endif
   endwhile
endfunction
" }}}

"{{{ Paste Toggle
let paste_mode = 0 " 0 = normal, 1 = paste

func! Paste_on_off()
   if g:paste_mode == 0
      set paste
      let g:paste_mode = 1
   else
      set nopaste
      let g:paste_mode = 0
   endif
   return
endfunc
"}}}

"{{{ Todo List Mode

function! TodoListMode()
   e ~/.todo.otl
   Calendar
   wincmd l
   set foldlevel=1
   tabnew ~/.notes.txt
   tabfirst
   " or 'norm! zMzr'
endfunction

"}}}

"}}}

"{{{ Mappings

" Open Url on this line with the browser \w
map <Leader>w :call Browser ()<CR>

" Open the Project Plugin <F2>
nnoremap <silent> <F2> :Project<CR>

" Open the Project Plugin
nnoremap <silent> <Leader>pal  :Project .vimproject<CR>

" TODO Mode
nnoremap <silent> <Leader>todo :execute TodoListMode()<CR>

" Open the TagList Plugin <F3>
nnoremap <silent> <F3> :Tlist<CR>

" Next Tab
nnoremap <silent> <C-Right> :tabnext<CR>

" Previous Tab
nnoremap <silent> <C-Left> :tabprevious<CR>

" New Tab
nnoremap <silent> <C-t> :tabnew<CR>

" Rotate Color Scheme <F8>
nnoremap <silent> <F8> :execute RotateColorTheme()<CR>

" DOS is for fools.
nnoremap <silent> <F9> :%s/$//g<CR>:%s// /g<CR>

" Paste Mode!  Dang! <F10>
nnoremap <silent> <F10> :call Paste_on_off()<CR>
set pastetoggle=<F10>

" Edit vimrc \ev
nnoremap <silent> <Leader>ev :tabnew<CR>:e ~/.vimrc<CR>

" Edit gvimrc \gv
nnoremap <silent> <Leader>gv :tabnew<CR>:e ~/.gvimrc<CR>

" Up and down are more logical with g..
nnoremap <silent> k gk
nnoremap <silent> j gj
inoremap <silent> <Up> <Esc>gka
inoremap <silent> <Down> <Esc>gja

" Good call Benjie (r for i)
nnoremap <silent> <Home> i <Esc>r
nnoremap <silent> <End> a <Esc>r

" Create Blank Newlines and stay in Normal mode
nnoremap <silent> zj o<Esc>
nnoremap <silent> zk O<Esc>

" Space will toggle folds!
nnoremap <space> za

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz

" Testing
set completeopt=longest,menuone,preview

inoremap <expr> <cr> pumvisible() ? "\<c-y>" : "\<c-g>u\<cr>"
inoremap <expr> <c-n> pumvisible() ? "\<lt>c-n>" : "\<lt>c-n>\<lt>c-r>=pumvisible() ? \"\\<lt>down>\" : \"\"\<lt>cr>"
inoremap <expr> <m-;> pumvisible() ? "\<lt>c-n>" : "\<lt>c-x>\<lt>c-o>\<lt>c-n>\<lt>c-p>\<lt>c-r>=pumvisible() ? \"\\<lt>down>\" : \"\"\<lt>cr>"

" " Swap ; and :  Convenient.
nnoremap ; :
nnoremap : ;

" Fix email paragraphs
nnoremap <leader>par :%s/^>$//<CR>

"ly$O#{{{ "lpjjj_%A#}}}jjzajj

"}}}

"{{{Taglist configuration
let Tlist_Use_Right_Window = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Exit_OnlyWindow = 1
let Tlist_Use_SingleClick = 1
let Tlist_Inc_Winwidth = 0
"}}}

let rct_completion_use_fri = 1

" Strip the newline from the end of a string
function! Chomp(str)
  return substitute(a:str, '\n$', '', '')
endfunction

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
  let fname = Chomp(system("find | dmenu -i -l 20 -p " . a:cmd))
  if empty(fname)
    return
  endif
  execute a:cmd . " " . fname
endfunction

map <c-t> :call DmenuOpen("tabe")<cr>
map <c-f> :call DmenuOpen("e")<cr>

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
autocmd VimEnter * execute '!xseticon -id ' . v:windowid . ' /usr/share/icons/Faenza/apps/32/vim.png'
autocmd VimLeave * execute '!xseticon -id ' . v:windowid . ' /usr/share/icons/Faenza/apps/32/terminal.png'

" " http://writequit.org/blog/?p=279
" " Supertab settings
" " supertab + eclim == java win
" let g:SuperTabDefaultCompletionTypeDiscovery = [
" \ "&completefunc:<c-x><c-u>",
" \ "&omnifunc:<c-x><c-o>",
" \ ]
" let g:SuperTabLongestHighlight = 1
" let g:SuperTabDefaultCompletionType = "<c-x><c-u>"

" Tab view of menus.
set wildmenu

call pathogen#infect()
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

"" 80 columns
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/
"
" Turn syntax highlighting on by default, try to keep this near the end of
" this file.
syntax on

" Current line highlighting
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END
