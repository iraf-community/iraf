# LOGIN.CL -- User login file for the IRAF command language.

# Identify login.cl version (checked in images.cl).
if (defpar ("logver"))
    logver = "IRAF V2.16.1 Oct 2013"

set	home		= "U_HOME"
set	imdir		= "U_IMDIR"
set	cache		= "U_CACHEDIR"
set	uparm		= "home$uparm/"
set	userid		= "U_USER"

# Set the terminal type.  We assume the user has defined this correctly 
# when issuing the MKIRAF and no longer key off the unix TERM to set a
# default.
if (access (".hushiraf") == no)
    print "setting terminal type to 'U_TERM' ..."
stty U_TERM


#============================================================================
# Uncomment and edit to change the defaults.
#set	editor		= vi
#set	printer		= lp
#set	pspage		= "letter"
#set	stdimage	= imt800
#set	stdimcur	= stdimage
#set	stdplot		= lw
#set	clobber		= no
#set	imclobber	= no
#set	filewait	= yes
#set	cmbuflen	= 512000
#set	min_lenuserarea	= 64000
#set	imtype		= "imh"
set	imextn		= "oif:imh fxf:fits,fit fxb:fxb plf:pl qpf:qp stf:hhh,??h"


# XIMTOOL/DISPLAY stuff.  Set node to the name of your workstation to
# enable remote image display.  The trailing "!" is required.
#set	node		= "my_workstation!"

# CL parameters you mighth want to change.
#ehinit   = "nostandout eol noverify"
#epinit   = "standout showall"
showtype = yes


#============================================================================
# Default USER package; extend or modify as you wish.  Note that this can
# be used to call FORTRAN programs from IRAF.

package user

task	$adb $bc $cal $cat $comm $cp $csh $date $dbx $df $diff	  = "$foreign"
task	$du $find $finger $ftp $grep $lpq $lprm $ls $mail $make	  = "$foreign"
task	$man $mon $mv $nm $od $ps $rcp $rlogin $rsh $ruptime	  = "$foreign"
task	$rwho $sh $spell $sps $strings $su $telnet $tip $top	  = "$foreign"
task	$awk $sed $vi $emacs $w $wc $less $rusers $sync $pwd $gdb = "$foreign"

task	$xc $mkpkg $generic $rtar $wtar $buglog			  = "$foreign"
#task	$fc = "$xc -h $* -limfort -lsys -lvops -los"
task	$fc = ("$" // envget("iraf") // "unix/hlib/fc.csh" //
	    " -h $* -limfort -lsys -lvops -los")
task	$nbugs = ("$(setenv EDITOR 'buglog -e';" //
	    "less -Cqm +G " // envget ("iraf") // "local/bugs.*)")
task	$cls = "$clear;ls"
task	$clw = "$clear;w"
task	$pg = ("$(less -Cqm $*)")


#============================================================================
# Load private home$loginuser.cl definitions.  The global login means that
# a user can create a loginuser.cl in the HOME$.iraf/ directory that will 
# apply to all logins.  In a case where MKIRAF created a local login.cl then
# this will load any loginuser.cl in the current directory.

if (access ("home$loginuser.cl"))
    cl < "home$loginuser.cl"
;
keep


# Allow a local loginuser.cl to override global definitions.  In a global 
# login this allows a 'loginuser.cl' file in a current project directory to
# override definitions set in the global login.cl/loginuser.cl file.  In a
# case where MKIRAF create a local login this simple re-loads the loginuser.cl

if (access ("./loginuser.cl"))
    cl < "./loginuser.cl"
;
keep


# Allow for a local uparm directory.  In a global login this allows us to 
# create a 'uparm' directory in a specific project dir that will override
# the params in the global login.  In a case where MKIRAF created a local
# uparm this simply redefines the 'uparm' as the absolute path.

path (osfn(".")) | scan (s1)
if (access (s1 // "uparm/")) {          
    s1 = substr (s1, strldx("!",s1)+1, strlen(s1))	# strip 'node!' prefix
    printf ("reset uparm = \"" // s1 // "uparm/\"; keep\n") | cl()
    s1 = ""
}
;
keep

#============================================================================
# Load the default CL package.  Doing so here allows us to override package
# paths and load personalized packages from our loginuser.cl. 
clpackage


# List any packages you want loaded at login time, ONE PER LINE.
images          # general image operators
plot            # graphics tasks
dataio          # data conversions, import export
lists           # list processing

# The if(deftask...) is needed for V2.9 compatibility.
if (deftask ("proto"))
    proto       # prototype or ad hoc tasks

tv              # image display
utilities       # miscellaneous utilities
if (deftask ("noao"))
    noao        # optical astronomy packages
;
if (deftask ("vo"))
    vo          # Virtual Observatory tools
;
prcache directory
cache   directory page type help

# Print the message of the day.
if (access (".hushiraf"))
    menus = no
else {
    clear; type hlib$motd
}


#============================================================================
# Check for updates to the system
chkupdate

# Notify the user if we're using the global login.
path (".") | scan (s1)
if ( osfn("home$") != substr (s1, strldx("!",s1)+1, strlen(s1)) ) {
    printf ("  ***  Using global login file:  %slogin.cl\n", osfn("home$"))
}
;


#============================================================================
# Uncomment to initialize the SAMP interface on startup.
if (deftask ("samp") == yes) {
  printf ("  ***  Initializing SAMP .... ")
  if (sampHubAccess() == yes) {
     # Enable SAMP messaaging.  Set default handlers that don't require 
     # VO capabilities.
     samp quiet
     samp ("on",                                                >& "dev$null")
#    samp ("handler", "table.load.votable", "tinfo $url",       >& "dev$null")
#    samp ("handler", "image.load.fits", "imstat $url",         >& "dev$null")
     samp noquiet
     print ("on")
  } else 
     print ("No Hub Available\n")
}


#============================================================================
# Delete any old MTIO lock (magtape position) files.
if (deftask ("mtclean"))
    mtclean
else
    delete uparm$mt?.lok,uparm$*.wcs verify-


#============================================================================
print ("  The following commands or packages are currently defined:\n")

keep

