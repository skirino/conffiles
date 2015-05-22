syntax on

set fileencodings=iso-2022-jp,cp932,sjis,euc-jp,utf-8

" Disable C-@ (and C-S-Space) since it is used to disable Japanese input method
autocmd VimEnter * imap <C-@> <nop>
