# .cshrc - csh resource script, read at beginning 
#	   of execution by each shell

umask 022
setenv iraf /iraf/iraf/
#setenv iraf /iraf/iraf-g77/iraf/
#source $iraf/unix/hlib/irafuser.csh
#alias  xc $iraf/unix/bin.redhat/xc.e

switch (`uname`)
case FreeBSD:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin /usr/games \
	/usr/local/bin /usr/local/sbin \
	/usr/X11R6/bin)
    breaksw
case Linux:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin /usr/games \
	/usr/local/bin /usr/local/sbin \
	/usr/X11R6/bin)
    breaksw
case Darwin:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin /usr/games \
	/usr/local/bin /usr/local/sbin \
	/usr/X11R6/bin)
    breaksw
case SunOS:
    set path = (. $HOME/bin /usr/local/bin \
	/usr/openwin/bin /usr/bin /bin /sbin /usr/sbin \
	/usr/ccs/bin /usr/dt/bin /usr/ucb \
	/opt/SUNWjws/JWS/intel-S2/bin \
	/opt/Summertime_98.i386/bin \
	/opt/Summertime_98.i386/sbin \
	/opt/Summertime_98.i386/TeX/bin \
	/opt/Summertime_98.i386/Perl/bin \
	/opt/Summertime_98.i386/Python/bin \
	/opt/Summertime_97.i386/netpbm/bin)
    breaksw
default:
    set path = (. $HOME/bin /sbin /bin /usr/sbin /usr/bin /usr/games \
	/usr/local/bin /usr/local/sbin \
	/usr/X11R6/bin)
    breaksw
endsw

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set filec
	set history = 100
	set savehist = 100
	set mail = (/var/mail/$USER)

	if (`uname` == SunOS) then
	    if ($?PS1) then
		set prompt = "root@`hostname`% "
	    else
		set prompt = "$user@`hostname`% "
	    endif
	else
	    if ($HOME == "/root") then
		set prompt = "root@`hostname`% "
	    else
		set prompt = "$user@`hostname`% "
	    endif
	endif

	# make mail(1) happy:
	setenv	crt	24
endif

alias	new	'find . \! -type d -mtime -7 -print'
alias	newt	'find . \! -type d -mtime -7 -print | grep -v '\''\.[aoe]$'\'
alias	newt2	'find . \! -type d -mtime -14 -print | grep -v '\''\.[aoe]$'\'
alias	newt3	'find . \! -type d -mtime -21 -print | grep -v '\''\.[aoe]$'\'
alias	newt4	'find . \! -type d -mtime -31 -print | grep -v '\''\.[aoe]$'\'
