# .CSHRC for iraf account, for apollo (need all this stuff in here
# on the apollo because .login is only accessed at actual login time;
# little if anything is propagated by "su iraf" or to subshells, etc.).

# Domain/OS: even if default environment is BSD at login time, without an
# explicit "umask 022" here, any files we create have weird or no permission
# flags.
umask 022

setenv	EXINIT	'set optimize redraw shell=/bin/csh ai sw=4'

set	history	= 100

# Domain/OS:  added /usr/bin/X11 & /usr/apollo/bin.
set	path	= (. /usr/local/bin /usr/ucb /bin /usr/bin /usr/bin/X11 /usr/apollo/bin)
set	time	= 30
set	mail	= (/usr/spool/mail/$user)
set	notify
set	prompt	= "`hostname`_`whoami`% "

setenv	iraf	/usr/iraf/
setenv	home	$iraf
set	cdpath	=\
	($iraf $iraf/noao $iraf/pkg $iraf/sys $iraf/unix $iraf/unix/boot\
	 $iraf/local $iraf/noao/imred $iraf/noao/twodspec ~)

alias	bye	logout
alias	cls	'clear;ls'
alias	clu	'clear;users'
alias	clw	'clear;w'
alias	his	history
alias	!	history
alias	notes	'vi + $iraf/local/notes.apollo.v29'
alias	pg	'less -Cqm'

# Set the desired architecture.  This is necessary only for software
# development or maintanence.

# Domain/OS:  no 'mach' command.  Unfortunately ISP not set for network logins.
# And worse, any *reference* to $ISP generates on stderr "ISP: Undefined 
# variable", whenever $hlib/irafuser.csh is "sourced" during a network
# login, creating havoc for network tar etc., hence the set ISP below.
#if ("$ISP" == "") then
    setenv ISP m68k
    set    ISP=m68k
    setenv MACH m68k
    setenv EXINIT 'set optimize redraw shell=/bin/csh ai sw=4'
#else
#    setenv MACH `echo $ISP`
#endif

switch ($MACH)
case m68k:
    set fpstat =\
	`/etc/nodestat -c | fgrep 'Floating Point Accelerator Unit present'`
    if ("$fpstat" != "" && -e ${iraf}bin.m68k_fpa/cl.e) then
	setenv IRAFARCH "m68k_fpa"
    else
	setenv IRAFARCH "m68k_f68"
    endif
    breaksw
case a88k:
    setenv IRAFARCH	a88k
    breaksw
default:
    echo "WARNING: No MACH or IRAFARCH definable in .cshrc"
endsw

source	$iraf/unix/hlib/irafuser.csh
