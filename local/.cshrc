# .cshrc - csh resource script, read at beginning 
#	   of execution by each shell

umask 022

set path = (. /sbin /bin /usr/sbin /usr/bin /usr/games \
	/usr/local/bin /usr/local/sbin \
	/usr/X11R6/bin $HOME/bin)

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set filec
	set history = 100
	set savehist = 100
	set mail = (/var/mail/$USER)

	set prompt = "$user@`hostname`% "

	# make mail(1) happy:
	setenv	crt	24
endif

alias	new	'find . \! -type d -mtime -7 -print'
alias	newt	'find . \! -type d -mtime -7 -print | grep -v '\''\.[aoe]$'\'
alias	newt2	'find . \! -type d -mtime -14 -print | grep -v '\''\.[aoe]$'\'
alias	newt3	'find . \! -type d -mtime -21 -print | grep -v '\''\.[aoe]$'\'
alias	newt4	'find . \! -type d -mtime -31 -print | grep -v '\''\.[aoe]$'\'
