# IRAF cshell login file.  [SITE DEPENDENT]

setenv	TAPE	/dev/nrmt0h

set	history	= 100
set	path	= (. /usr/local/bin /usr/ucb /bin /usr/bin /etc /etc/sec /usr/etc /usr/etc/sec /usr/bin/X11 /usr/new /usr/hosts)
set	time	= 30
set	mail	= (/usr/spool/mail/$user)
set     prompt = "`whoami`@`hostname | sed 's+\..*++'`% "
set	notify

setenv	iraf	/usr/iraf/
if (! -d $iraf) then
    setenv iraf	`(cd ..;pwd)`/
endif
source	$iraf/unix/hlib/irafuser.csh

set	cdpath	=\
	($iraf $iraf/noao $iraf/pkg $iraf/sys $iraf/unix $iraf/unix/boot\
	 $iraf/local $iraf/noao/imred $iraf/noao/twodspec ~)

alias	cls	'clear;ls'
alias	clu	'clear;users'
alias	clw	'clear;w'
alias	his	'history | tail -15'
alias	notes	'vi + $iraf/local/notes.dsux'
alias	pg	'less -Cqm -bp512'
alias	sp	'clear;tail -f spool'
alias	setarch	'setenv IRAFARCH \!*'

setarch	ddec
