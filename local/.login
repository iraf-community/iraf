# IRAF cshell login file.

setenv	TERM 	sun
setenv	EXINIT	'set optimize redraw shell=/bin/csh ai sw=4'

set	history	= 100
set	path	= (. /usr/local/bin /usr/ucb /bin /usr/bin)
set	time	= 30
set	mail	= (/usr/spool/mail/$user)
set	notify

#setenv	iraf	/files/vol/iraf/
setenv	iraf	/local/iraf/
setenv	home	$iraf
source	$iraf/unix/hlib/irafuser.csh
set	cdpath	=\
	($iraf $iraf/noao $iraf/pkg $iraf/sys $iraf/unix $iraf/unix/boot\
	 $iraf/local $iraf/noao/imred $iraf/noao/twodspec ~)

alias	bye	logout
alias	cls	'clear;ls'
alias	clu	'clear;users'
alias	clw	'clear;w'
alias	his	history
alias	notes	'vi + $iraf/local/notes.v*'
alias	pg	'less -Cqm'
#alias	setarch	'setenv IRAFARCH \!*'				# Sun-4, 386i
alias	setarch	'setenv IRAFARCH \!*; setenv FLOAT_OPTION \!*'	# Sun-3
#alias	sun	'suntools -pattern gray'
alias	sun	'suntools -color 107 142 35'

# Set the desired architecture.  This is necessary only for software
# development or maintanence.

switch (`mach`)
case sparc:
    setenv IRAFARCH	sparc
    breaksw
case i386:
    setenv IRAFARCH	i386
    breaksw
default:
    # The following sets IRAFARCH to match the current IRAF architecture.
    # This works so long as the IRAF architecture is not set to "generic".
    # setarch `ls -l $iraf/bin | sed -e 's+^.*bin\.++'`

    # Run f68881 architecture on Sun-3 by default; won't work if only the
    # ffpa binaries are installed!.
    # setarch f68881
endsw

stty	new dec cr0 ff0 crt tabs susp ^X eof ^Z
