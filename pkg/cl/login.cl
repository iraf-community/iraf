# LOGIN.CL -- User login file for the IRAF command language.

logver	= "IRAF V2.10BETA February 1992"

set	home		= "cl$"
set	imdir		= "uparm$"
set	uparm		= "home$uparm/"
set	userid		= "CLTEST"

# Set the terminal type.
if (envget("TERM") == "sun") {
    if (!access (".hushiraf"))
	print "setting terminal type to gterm..."
    stty gterm
} else if (envget("TERM") == "xterm") {
    if (!access (".hushiraf"))
	print "setting terminal type to xterm..."
    stty xterm nl=54
} else {
    if (!access (".hushiraf"))
	print "setting terminal type to vt100 nl=50..."
    stty vt100 nl=50
}

# Uncomment and edit to change the defaults.
#set	editor		= vi
#set	printer		= lw
#set	stdimage	= imt800
#set	stdimcur	= stdimage
#set	stdplot		= lw
#set	clobber		= no
#set	filewait	= yes
#set	cmbuflen	= 256000
#set	min_lenuserarea	= 20000

# IMTOOL/XIMAGE stuff.  Set node to the name of your workstation to
# enable remote image display.
#set	node		= ""

# CL parameters you mighth want to change.
#ehinit   = "nostandout eol noverify"
#epinit   = "standout showall"
showtype = yes

# Default USER package; extend or modify as you wish.  Note that this can
# be used to call FORTRAN programs from IRAF.

package user
if (access ("home$loginuser.cl")) cl < "home$loginuser.cl"
;

task	$adb $bc $cal $cat $comm $cp $csh $date $df $diff	= "$foreign"
task	$du $find $finger $ftp $grep $lpq $ls $mail $make	= "$foreign"
task	$man $mon $mv $nm $od $ps $rcp $rlogin $rsh $ruptime	= "$foreign"
task	$rwho $sh $spell $sps $strings $su $telnet $top		= "$foreign"
task	$touch $vi $w $wc $less $rusers $sync			= "$foreign"

task	$xc $mkpkg $generic $rtar $wtar $buglog			= "$foreign"
#task	$fc = "$xc -h $* -limfort -lsys -lvops -los"
task	$fc = ("$" // envget("iraf") // "unix/hlib/fc.csh" //
	    " -h $* -limfort -lsys -lvops -los")
task	$nbugs = ("$(setenv EDITOR 'buglog -e';" //
	    "less -Cqm +G " // envget ("iraf") // "local/bugs.*)")
task	$cls = "$clear;ls"
keep;	clpackage

prcache directory
cache	directory page type help

# Print the message of the day.
if (access (".hushiraf"))
    menus = no
else {
    #clear; type hlib$motd
}

# Delete any old MTIO lock files or display WCS (do NOT delete this).
delete uparm$mt*.lok,uparm$*.wcs verify-

# List any packages you want loaded at login time, ONE PER LINE.
dataio		# data conversions, import export
images		# general image operators
lists		# list processing
plot		# graphics tasks
proto		# prototype or ad hoc tasks
tv		# image display
utilities	# miscellaneous utilities
#noao		# optical astronomy packages

keep
