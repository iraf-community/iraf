# LOGIN.CL -- User login file for the IRAF command language.

set	home		= "pkg$cl/"
set	imdir		= "uparm$"
set	uparm		= "home$uparm/"
set	userid		= "IRAF"

stty	vt100		# identify the terminal to be used

# Uncomment and edit to change the defaults.

#set	editor		= "vi"			# Environment stuff
#set	printer		= "imagen"
#set	stdgraph	= "vt640"
#set	stdimage	= "iism70"
#set	stdplot		= "vup"
#set	clobber		= "no"
#set	filewait	= "yes"

#ehinit   = "nostandout eol noverify"		# CL parameters
#epinit   = "standout showall"
#showtype = no

# Default USER package; extend or modify as you wish.  Note that this can
# be used to call FORTRAN programs from IRAF.

package user
task	$mail $lpq $diff $od $find $touch $w $ls	= "$foreign"
task	$adb $rsh $rlogin $rwho $telnet $ruptime	= "$foreign"
task	$xc $mkpkg $generic $rtar $wtar			= "$foreign"
task	$sps $top $mon $bgrep $su			= "$foreign"
task	$fc = "$xc -h $* -limfort -lsys -lvops -los"
keep;	clpackage

prcache directory
cache	directory page type help

# Print the message of the day.
clear;	type hlib$motd

# Delete any old MTIO lock files (do NOT delete this).
delete uparm$mt?.lok verify-

keep
