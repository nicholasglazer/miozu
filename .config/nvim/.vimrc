""" nicholasglazer.com
""" My main editor is emacs+vim mode but I'm editing files in a terminal with vim as well.
""" Before this file I had a monter config of neovim and things got out of control, but now i decided to be minimalistic.
""" Especially config files. Or I might create a file with vim instead of touch sometimes.
""" So i have this simple vimrc file to use vim occasionally
""" TODO take advantage of miozu theme



set encoding=utf-8        "" encoding
set noswapfile            "" swap is unnecessary
set autowrite             "" write before : commands
set ruler                 "" show cursor constantly
set number                "" lines numbering
set numberwidth=4         "" offset starting from the left
set wildmenu              ""mcommand autocomplete on
set showmatch             "" match brackets
set lazyredraw            "" redraw only tree change
set incsearch             "" search while typing
set ignorecase            "" ignore case on search
set tabstop=2             "" tabs has 4 spaces
set softtabstop=2
set expandtab             "" tabs are made of spaces
set nobackup              "" backups off
set showcmd               "" visual selection
set ttyfast               "" fast keyboard response
set timeout timeoutlen=1000 ttimeoutlen=50

set cmdheight=2           "" bigger input cmd bar

set undolevels=1000       "" undoing like a god


"colorscheme              "" Pick a colorscheme TODO theme
syntax enable             "" Enable syntax highlighting


"To make theme transparent
hi! Normal ctermbg=NONE guibg=NONE
hi! NonText ctermbg=NONE guibg=NONE
