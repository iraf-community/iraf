setenv TERMCAP ~/.termcap
setenv TERM vt100-npt
stty new dec cr0 ff0 crt -tabs susp ^X eof ^Z

set	history	= 100
set	path = (. /usr/convex /usr/ucb /bin /etc /usr/local/bin /usr/bin\
	/bin/time /usr/lib /usr/games /etc /usr/local/adm /local/bin ~/bin )
set	time	= 30
set	mail	= (/usr/spool/mail/$user)
set	notify

setenv	EXINIT	'set optimize redraw shell=/bin/csh ai sw=4'
setenv	home	/iraf/$user/
setenv	iraf	/iraf/iraf/
source	$iraf/unix/hlib/irafuser.csh
# unalias	cl
set	cdpath	=\
	($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot ~)

alias	cls	'clear;ls'
alias	clw	'clear;w'
alias	hi	'history | tail -15'
alias	notes	'vi + $iraf/local/notes.convex'
alias	pg	'less -cqm'
alias	sp	'clear;tail -23f spool'

# Set floating point architecture for CONVEX/IRAF.
alias   setarch 'setenv IRAFARCH \!*'
setarch ieee

users
