#!/bin/csh

umask 022

setenv SHELL /bin/csh
setenv XFILESEARCHPATH ":/usr/lib/X11/%T/%N%S:/iraf/iraf/local/bin/%T/%N%S"
setenv PATH ".:/usr/local/bin:/bin:/usr/bin:/iraf/iraf/local/bin:/usr/X11/bin:/sbin:/usr/games:"
setenv PAGER "less -C -E -M"
setenv IPAGER "less -C -M +G"
unsetenv LESSPIPE

set history = 200
set prompt = "%n@%m% "

alias cls	'clear;ls'
alias clw	'clear;w'
alias pg	'less -Cqm -b256'
alias his	'history | tail -15'
alias sp	'clear; tail -f spool'
alias man	'setenv PAGER "less -Cqm"; /usr/bin/man'

if ($?iraf == 0) then
    setenv iraf /iraf/iraf/
endif
if ($?MACH == 0) then
    source $iraf/unix/hlib/irafuser.csh
endif
