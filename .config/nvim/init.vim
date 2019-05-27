let vim_plug_just_installed = 0
let vim_plug_path = expand('~/.config/nvim/autoload/plug.vim')
if !filereadable(vim_plug_path)
    echo "Installing Vim-plug..."
    echo ""
    silent !mkdir -p ~/.config/nvim/autoload
    silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    let vim_plug_just_installed = 1
endif

" manually load vim-plug the first time
if vim_plug_just_installed
    :execute 'source '.fnameescape(vim_plug_path)
endif


call plug#begin('~/.config/nvim/plugged')




" assuming your using vim-plug: https://github.com/junegunn/vim-plug


Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}

" Automatically close parenthesis, etc
Plug 'Townk/vim-autoclose'

" Surround
Plug 'tpope/vim-surround'


call plug#end()

" If we installed plug here, also install packages.
if vim_plug_just_installed
    echo "Installing Bundles, please ignore key map error messages"
    :PlugInstall
endif

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4


" Language server stuff.
" Required for operations modifying multiple buffers like rename.
set hidden
let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'javascript': ['/usr/local/bin/javascript-typescript-stdio'],
    \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
    \ 'python': ['/usr/local/bin/pyls'],
    \ }




