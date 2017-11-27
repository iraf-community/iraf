#{ CLPACKAGE.CL -- Package definitions file for the "clpackage" package.
# When this script is run, the current package is "clpackage", the default
# startup package.  NOTE -- See hlib$zzsetenv.def for additional environment
# definitions.

# Uncomment and edit the following to change the default values of these
# CL parameters for your site.

#ehinit		= "nostandout eol noverify"
#epinit		= "standout showall"
#keeplog	= no
#logfile	= "home$logfile"
#logmode	= "commands nobackground noerrors notrace"

szprcache	= 4

# IRAF standard system package script task declarations.

task	dbms.pkg	= "dbms$dbms.cl"
task	dataio.pkg	= "dataio$dataio.cl"
task	images.pkg	= "images$images.cl"
task	language.pkg	= "language$language.cl"
task	lists.pkg	= "lists$lists.cl"
task	obsolete.pkg	= "obsolete$obsolete.cl"
task	plot.pkg	= "plot$plot.cl"
task	proto.pkg	= "proto$proto.cl"
task	softools.pkg	= "softools$softools.cl"
task	system.pkg	= "system$system.cl"
task	utilities.pkg	= "utilities$utilities.cl"

# Handy task to call the user's logout.cl file.
task	$_logout	= "home$logout.cl"

# Define the external (user-configurable) packages.
cl < hlib$extern.pkg


# Load dynamically-defined external packages.
if (access ("hlib$extpkg.cl") == yes)
    cl < hlib$extpkg.cl
;


# Load packages needed by dynamic external packages.  These are reloaded
# in the login.cl.
images
proto
utilities
if (deftask ("noao"))
   noao
;

# Load the SYSTEM package.  Avoid printing menu, but do not change the
# default value of the menus switch.

if (menus) {
    menus = no;  system;  menus = yes
} else
    system

keep
