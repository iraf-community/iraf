# IRAF cshell login file.  [SITE DEPENDENT]

umask 22

# Set the DISPLAY variable automatically on rlogins to different machines
setenv HOST    `hostname`
setenv DISPLAY unix:0.0
set me = `who -M`
if ($#me == 6) then
    setenv DISPLAY `echo $me[6] | sed 's/(\(.*\))/\1:0/'`
    if ($DISPLAY == "unix:0") then
	setenv DISPLAY "${HOST}:0"
    endif
endif
unset me

setenv	uparm	$home/uparm/
setenv	PAGER	"less -C -E -M"
setenv	IPAGER	"less -C -M +G"
#setenv	LD_LIBRARY_PATH /usr/lib/X11
#setenv	XFILESEARCHPATH ":/usr/lib/X11/%T/%N%S:/u3/x11apps/lib/%T/%N%S"

set	history	= 500
set	time	= 30
set     prompt = "`whoami`@`hostname | sed 's+\..*++'`% "
set	notify

if ($?iraf == 0) then
    setenv iraf /iraf/iraf/
endif

set file = $iraf/unix/hlib/irafuser.csh
if (-e $file) then
    source $file
endif

set	cdpath	=\
($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot ~)

#setenv	wcsdir	$uparm
#setenv	qmfiles	$iraf/sys/qpoe/QPDEFS

alias	cls	'clear;ls'
alias	clw	'clear;w'
alias	clu	'clear;rwho'
alias	sp	'clear;tail -23f spool'
alias	his	'history | tail -15'
alias	notes	'vi + $iraf/local/notes.osf1'
alias	pg	'less -Cqm'
alias	man	'(setenv PAGER "less -C -M"; /usr/bin/man \!*)'
alias	new	'find . \! -type d -mtime -7 -print'
alias	newt	'find . \! -type d -mtime -7 -print | grep -v '\''\.[aoe]$'\'

# Set the IRAF architecture.
alias	setarch	'setenv IRAFARCH \!*'
setarch `uname -m`

users
