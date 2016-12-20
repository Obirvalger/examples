let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 /home/ogneslav/prog/examples/perl/Polynomial/lib/Polynomial.pm
badd +41 /home/ogneslav/prog/examples/perl/Polynomial/polynomial.pl
badd +1 /home/ogneslav/prog/examples/perl/Polynomial/t/01_general.t
badd +1 /home/ogneslav/prog/examples/perl/Polynomial/t/02_polarization.t
argglobal
silent! argdel *
argadd /home/ogneslav/prog/examples/perl/Polynomial/lib/Polynomial.pm
argadd /home/ogneslav/prog/examples/perl/Polynomial/t/02_polarization.t
argadd /home/ogneslav/prog/examples/perl/Polynomial/t/01_general.t
argadd /home/ogneslav/prog/examples/perl/Polynomial/polynomial.pl
edit /home/ogneslav/prog/examples/perl/Polynomial/lib/Polynomial.pm
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
let s:l = 1 - ((0 * winheight(0) + 14) / 29)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOc
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
