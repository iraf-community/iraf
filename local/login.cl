# LOGIN.CL -- User login file for the IRAF command language.

set	home		= "/local/iraf/local/"
set	imdir		= "/tmp4/iraf/iraf/"
set	uparm		= "home$uparm/"
set	userid		= "iraf"

#reset	bin = envget("IRAFBIN")			# SUN floating point option
print	"setting terminal type to gterm..."	# warn user
stty	gterm					# set terminal protocol

# Uncomment and edit to change the defaults.

#set	editor		= vi			# environment stuff
#set	printer		= lw
#set	stdimage	= imt800
#set	stdimcur	= stdimage
#set	stdplot		= lw
#set	clobber		= no
#set	filewait	= yes
#set	cmbuflen	= 256000
#set	min_lenuserarea	= 20000

# IMTOOL/XIMAGE stuff.
#set	node		= ""			# set to name of your WS
#set	wcsdir		= "node!" // osfn("uparm$")

#ehinit   = "nostandout eol noverify"		# CL parameters
#epinit   = "standout showall"
#showtype = no

# Default USER package; extend or modify as you wish.  Note that this can
# be used to call FORTRAN programs from IRAF.

package user
if (access ("home$loginuser.cl")) cl < "home$loginuser.cl"
task	$mail $man $lpq $diff $od $find $touch $w $ls	= "$foreign"
task	$adb $rsh $rlogin $rwho $telnet $ruptime	= "$foreign"
task	$xc $mkpkg $generic $rtar $wtar $buglog		= "$foreign"
task	$sps $top $mon $bgrep $su			= "$foreign"
#task	$fc = "$xc -h $* -limfort -lsys -lvops -los"
task	$fc = ("$" // envget("iraf") // "unix/hlib/fc.csh" //
	    " -h $* -limfort -lsys -lvops -los")
task	$nbugs = ("$(setenv EDITOR 'buglog -e';" //
	    "less -Cqm +G " // envget ("iraf") // "local/bugs.*)")
keep;	clpackage

prcache directory
cache	directory page type help

# Print the message of the day.
clear;	type hlib$motd

# Delete any old MTIO lock files or display WCS (do NOT delete this).
delete uparm$mt?.lok,uparm$*.wcs verify-

# List any packages you want loaded at login time, ONE PER LINE.
noao		# (uncomment to load package `noao')
proto		# (uncomment to load package `noao.proto' - also `images.tv')

keep
