# .cshrc - csh resource script, read at beginning 
#	   of execution by each shell

umask 022
setenv iraf /iraf/iraf/
setenv IRAFARCH  `$iraf/unix/hlib/irafarch.csh -actual`
source $iraf/unix/hlib/irafuser.csh

#setenv IRAFARCH macintel
#setenv XC_F77    "g95"
#setenv XC_LFLAGS "-lg95"
#setenv XC_XFLAGS "-/mfpmath=sse -/free-vectorize -/msse"
#setenv XC_FFLAGS "-mfpmath=sse -ftree-vectorize -msse"
#setenv XC_CFLAGS "-mfpmath=sse -ftree-vectorize -msse"

setenv  LC_COLLATE	POSIX


switch (`uname`)
case Linux:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin \
	/usr/local/bin /usr/local/sbin /usr/X11R6/bin)
    breaksw
case Darwin:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin \
	/usr/local/bin /opt/local/bin /usr/local/sbin /usr/X11R6/bin)
    breaksw
default:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin \
	/usr/local/bin /usr/local/sbin /usr/X11R6/bin)
    breaksw
endsw

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set filec
	set history = 100
	set savehist = 100
	set mail = (/var/mail/$USER)

	set prompt = "`hostname -s`> "

	# make mail(1) happy:
	setenv	crt	24
endif

setenv XFILESEARCHPATH ":/usr/lib/X11/%T/%N%S:/usr/x11R6/lib/X11/%T/%N%S:"


alias   del	'/bin/rm -f'
#alias   ls	'ls -FCs'
alias   m	'less'
alias 	po	'popd'
alias 	pu	'pushd'

alias	new	'find . \! -type d -mtime -7 -print'
alias	newt	'find . \! -type d -mtime -7 -print | grep -v '\''\.[aoe]$'\'
alias	newt2	'find . \! -type d -mtime -14 -print | grep -v '\''\.[aoe]$'\'
alias	newt3	'find . \! -type d -mtime -21 -print | grep -v '\''\.[aoe]$'\'
alias	newt4	'find . \! -type d -mtime -31 -print | grep -v '\''\.[aoe]$'\'
