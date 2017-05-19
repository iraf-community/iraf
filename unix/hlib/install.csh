#!/bin/csh
#
#  INSTALL -- Install IRAF on a UNIX/IRAF host.  May also be run after the
#  initial installation as a consistency check to verify that all the necessary
#  links and file permissions are still in place (e.g., after updating UNIX
#  itself).
#
#  Installation:
#	This file should be installed in the $iraf directory, if it is being 
#  installed as a patch to a distributed IRAF system the existing iraf$install
#  script should be backed up and then this file used to replace it.  Once 
#  in place the script may be made executable with the command
# 
#		% chmod 755 install
#
#  Please consult the IRAF web pages for the latest information on bug fixes
#  to this script.
#
#
#  Usage:     install [-n] [-r rootdir] [-i imdir] [-b localbindir] \
#		    [-R oldroot] [-I oldimdir] [-u username (e.g., 'iraf')]
#		    [-l locallibdir]  [-m mach (e.g., 'sparc', 'ssun') ]
#		    [-noedit]
#
#  Example:
#		% su
#		% cd $hlib
#		% ./install -n
#		% ./install
#
#  If run with no arguments, INSTALL will make an informed guess and prompt
#  with this value; type <return> to accept the value, or enter a new value.
#
#  Use "install -n" to do a dry run to see what the would be done, without
#  actually modifying the host system and IRAF configuration files.  To do the
#  actual installation one must be superuser, but anyone can run "install -n"
#  to see what it would do.
#
# ----------------------------------------------------------------------------


unset	noclobber
onintr	cleanup_
unalias cd cp cmp echo ln mv rm sed set grep ls chmod chown pwd touch sort which

setenv	path  "(/sbin /usr/sbin /bin /usr/bin /usr/5bin /usr/ucb /etc /usr/etc $path /usr/local/bin /opt/local/bin /local/bin /home/local/bin /usr/openwin/bin /usr/X11R6/bin /usr/X11/bin)"

# set echo



##############################################################################
# START OF MACHDEP DEFINITIONS.
##############################################################################

# MACHDEP definitions which may be reset below.
set LS			= "/bin/ls"				# [MACHDEP]
set LSDF        	= "-lLtgs"				# [MACHDEP]

set VERSION		= "V2.16"
set V 			= `echo $VERSION | cut -c2-5`
set hmach 		= "INDEF"
set shlib 		= "no"
set pciraf 		= 0
set suniraf 		= 0
set do_tapecaps 	= 0
set do_tapes 		= 1
set do_pipes 		= 0
set has_pipes 		= 1
set hilite    		= 1
set no_edit		= 0


# Utility aliases.
#alias PUT      "mv -f \!*; chown $user \!$ "			# [MACHDEP]
alias PUT      "cp -p \!*; chown $user \!$ "			# [MACHDEP]
alias BOLD_ON  "(if ($hilite) tput bold)"
alias BOLD_OFF "(if ($hilite) tput sgr0)"
alias SO_ON    "(if ($hilite) tput smso)"
alias SO_OFF   "(if ($hilite) tput sgr0)"

alias DO_OK    "(echo -n '[ '; BOLD_ON; echo -n ' OK '; BOLD_OFF; echo ' ]')"
alias DO_WARN  "(echo -n '[ '; BOLD_ON; echo -n 'WARN'; BOLD_OFF; echo ' ]')"
alias DO_FAIL  "(echo -n '[ ';   SO_ON; echo -n 'FAIL';   SO_OFF; echo ' ]')"

alias MSG      "(echo -n '   ';BOLD_ON;echo -n '***  ';BOLD_OFF; echo \!*)"
alias MSGB     "(echo -n '   ';BOLD_ON;echo -n '***  ';echo \!*; BOLD_OFF)"
alias MSGN     "(echo -n '   ';BOLD_ON;echo -n '***  ';BOLD_OFF; echo -n \!*)"
alias MSGBN    "(echo -n '   ';BOLD_ON;echo -n '***  ';echo -n \!*; BOLD_OFF)"
alias ERRMSG   "(echo -n '   ';BOLD_ON;echo -n 'ERROR: '  ;BOLD_OFF; echo \!*)"
alias WARNING  "(echo -n '   ';BOLD_ON;echo -n 'WARNING: ';BOLD_OFF; echo \!*)"
alias NEWLINE  "(echo '')"

alias PROMPT   "(BOLD_ON; echo -n \!*; BOLD_OFF; echo -n ' (yes): ')"
alias PROMPT_N "(BOLD_ON; echo -n \!*; BOLD_OFF; echo -n ' (no): ')"

alias RM	"rm -rf"
alias LN	"ln -s"


#----------------------------------
# Determine platform architecture.
#----------------------------------

set UNAME=""
if (-e /usr/bin/uname) then
    set uname_cmd = /usr/bin/uname
    set UNAME=`/usr/bin/uname | tr '[A-Z]' '[a-z]'`
else if (-e /bin/uname) then
    set uname_cmd = /bin/uname
    set UNAME=`/bin/uname | tr '[A-Z]' '[a-z]'`
else
    WARNING  "No 'uname' command found to determine architecture."
    exit 1
endif

set WHOAMI=`whoami`
if (-e /usr/bin/whoami) then
    set WHOAMI=`/usr/bin/whoami`
else if (-e /usr/ucb/whoami) then
    set WHOAMI=`/usr/ucb/whoami`
else
    WARNING  "No 'whoami' command found for this architecture."
    exit 1
endif




switch ($UNAME) 
    case sunos:
	set do_tapecaps 	= 1
        if (`$uname_cmd -m | cut -c2-` != "86pc") then
	    set suniraf		= 1
            setenv OSVERSION `$uname_cmd -r | cut -c1`
            if ($OSVERSION == 5) then			    # Sparc Solaris
                set mach  	= "ssun"
                set hmach 	= "ssol"
		set TAPES	= "/dev/*st[0-7]*"
		set shlib	= "no"
		set LIBFILES    = ""
                set LS          = "/usr/ucb/ls"
            else			   		    # Sparc SunOS 4.x
                set mach  	= "sparc"
                set hmach 	= "sparc"
		set TAPES	= "/dev/*st[0-7]*"
		set shlib	= "no"
		set LIBFILES    = ""
            endif
        else
	    set pciraf		= 1
            set mach 		= "sunos"	    	    # Intel Solaris
            set hmach 		= "sunos"
	    set TAPES		= "/dev/*st[0-7]*"
	    set shlib		= "no"
	    set LIBFILES    	= ""
	    set LSDF        	= "-lLts"
        endif
        breaksw
    case linux:
	set pciraf		= 1
	set do_tapecaps 	= 1
	if ($?IRAFARCH == 1) then
	    # Let IRAFARCH override the default for the machine.
            set mach 		= "$IRAFARCH"
            set hmach 		= "$IRAFARCH"
        else if (`$uname_cmd -m` == "x86_64") then	    # Linux x86_64
            set mach 		= "linux64"
            set hmach 		= "linux64"
        else if (`$uname_cmd -m` == "ppc") then		    # LinuxPPC
            if (-f /etc/redhat-release) then
                set mach 	= "linuxppc"
                set hmach 	= "linuxppc"
            endif
        else
            set mach 		= "linux"
            set hmach 		= "linux"
        endif
	set TAPES		= "/dev/*st[0-7]"
	set shlib		= "no"
	set LIBFILES    	= ""
        breaksw
    case freebsd: 					    # FreeBSD 4.0
	set do_tapecaps 	= 1
        setenv OSVERSION `$uname_cmd -r | cut -c1`
        if ($OSVERSION == 5) then			    # Sparc Solaris
	    set has_pipes 	= 0
	endif
        set mach 		= "freebsd"
        set hmach 		= "freebsd"
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "no"
	set LIBFILES    	= ""
	set pciraf		= 1
        breaksw
    case darwin: 					    # Mac OS X
    case macosx:
    case macintel:
    case ipad:
	# Mac OS X doesn't appear to have tape support 
	# at this point.
	set do_tapecaps 	= 0
	set do_tapes 		= 0
	set has_pipes 		= 0
	if ($?IRAFARCH == 1) then
	    # Let IRAFARCH override the default for the machine.
            set mach 		= "$IRAFARCH"
            set hmach 		= "$IRAFARCH"
        else if (`$uname_cmd -m` == "x86_64") then		    # 64-bit OSX
            set mach 		= "macintel"
            set hmach 		= "macintel"
        else if (`$uname_cmd -m |cut -c1-4` == "iPad") then # iPad/iPod OSX
            set mach 		= "ipad"
            set hmach 		= "ipad"
        else						    # 32-bit OSX
            set mach 		= "macosx"
            set hmach 		= "macosx"
        endif
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "no"
	set LIBFILES    	= ""
	set pciraf		= 1
        breaksw
    case hp-ux:						    # HP/UX 10.20
        set mach 		= "hp700"
        set hmach 		= "hp700"
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "no"
	set LIBFILES    	= ""
        set LSDF        	= "-lLts"

	# reset for HP/UX
	alias PUT 'mv -f \!*; chown $user \!$ '
        breaksw
    case irix:    					    # IRIX 6.5
    case irix64:  
        set mach 		= "irix"
        set hmach 		= "irix"
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "no"
	set LIBFILES    	= ""
        set LSDF        	= "-lLts"
        breaksw
    case aix:     					    # AIX V4
        set mach 		= "rs6000"
        set hmach 		= "rs6000"
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "no"
	set LIBFILES    	= ""
        set LSDF        	= "-lLts"
        breaksw
    case osf1:    					    # Alpha OSF/1
        set mach 		= "alpha"
        set hmach 		= "alpha"
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "yes"
	set LIBFILES    	= "libiraf.so"
        breaksw
    case ultrix:    					    # DEC Ultrix
        set mach 		= "ultrix"
        set hmach 		= "ultrix"
	set TAPES		= "/dev/*st[0-7]*"
	set shlib		= "no"
	set LIBFILES    	= ""
        breaksw

    default:      
	# We don't want to be limited by the CYGWIN version numbering so
	# look for a truncated match here before punting.
	set os_mach = `echo $UNAME | cut -c1-6`
	if ("$os_mach" == "cygwin") then
            set mach 		= "cygwin"
            set hmach 		= "cygwin"
	    set shlib		= "no"
	    set LIBFILES    	= ""
	    set TAPES		= ""		# no tape support
	    set do_tapecaps 	= 0
	    set do_tapes 	= 0
	    set has_pipes 	= 0
            breaksw

	else
	    ERRMSG  "Unable to determine platform architecture."
	    exit 1
	endif
endsw

##############################################################################
# END OF MACHDEP DEFINITIONS.
##############################################################################


#=============================================================================
# Declarations and initializations.
#=============================================================================

set W		= '\([ 	"]\)'			# match a blank, tab, or quote
set TEMP	= "/tmp/iraf_install.$$"
set exec	= yes
set user	= iraf
set port	= 0

set imdir	= ""				# Initialize paths
set lbin	= ""
set llib	= ""
set cache	= ""
set o_iraf 	= ""
set o_imdir 	= ""
set o_cache 	= ""



#=============================================================================
# Initialize the path variables.
#=============================================================================

set valid_iraf = 1
if ($?iraf == 1) then
  if (! (-d $iraf) || ! (-r $iraf) || \
     ("`$LS -lLd $iraf |& grep '.rw[xs]r.[xs]r.[xt]'`" == "") || \
     ("`$LS -lLd $iraf/.. |& grep '.rw[xs]r.[xs]r.[xt]'`" == "")) then
         set valid_iraf = 0
  endif
  if ($valid_iraf == 0) then
     NEWLINE
     BOLD_ON
     echo -n '    ERROR: invalid $iraf value '
     echo "($iraf)"
     BOLD_OFF
     NEWLINE
     MSG  '    The iraf directory tree set by your environment variable, $iraf'
     MSG  "    is not generally readable.  This will prevent most users from"
     MSG  "    being able to read iraf files, especially the binaries! "
     NEWLINE
     MSG  "    Please reset the permissions on the tree and try again..."
     NEWLINE
     exit 1
  endif
  if (! -d $iraf) then
     NEWLINE
     WARNING  "Env definition of iraf root is incorrect, resetting."
     NEWLINE
     setenv iraf ""
  endif

else
    setenv iraf	""
endif


#=============================================================================
# Process any command line arguments.
#=============================================================================
while ("$1" != "")
    switch ("$1")
    case -n:				# no execute
	alias PUT "diff \!$ \!^; rm -f $TEMP"
	set exec = no
	breaksw
    case -noedit:			# no-edit installations
	set no_edit = 1
	breaksw
    case -port:				# do a "port" install
	set port = 1
	breaksw
    case -hl:				# disable highlighting
	set hilite = 0
	alias BOLD_ON   "(if ($hilite) tput bold)"
	alias BOLD_OFF  "(if ($hilite) tput sgr0)"
	alias SO_ON     "(if ($hilite) tput smso)"
	alias SO_OFF    "(if ($hilite) tput rmso)"
	breaksw
    case +hl:				# enable highlighting
	set hilite = 1
	alias BOLD_ON   "(if ($hilite) tput bold)"
	alias BOLD_OFF  "(if ($hilite) tput sgr0)"
	alias SO_ON     "(if ($hilite) tput smso)"
	alias SO_OFF    "(if ($hilite) tput rmso)"
	breaksw
    case -f:				# create fifo pipes
	set do_pipes = 1
	breaksw
    case -h:				# print help summary
	goto Usage
    case -help:				# print help summary
	goto Usage
    case --help:			# print help summary
	goto Usage
    case -b:				# set local bin directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-b <localbindir>' switch"
	    exit 1
	endif
	set lbin = "$1"
	breaksw
    case -l:                            # set local lib directory
	if ("$shlib" == "yes") then
            if ("$2" != "") then
                shift
            else
                ERRMSG  "missing argument to '-l <locallibdir>' switch"
                exit 1
            endif
            set llib = "$1"
	else
            set llib = ""
            shift
	endif
        breaksw
    case -c:				# set cache directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-c <cache>' switch"
	    exit 1
	endif
	set cache = "$1"
	breaksw
    case -i:				# set imdir directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-i <imdir>' switch"
	    exit 1
	endif
	set imdir = "$1"
	breaksw
    case -m:				# set machine type
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-m <mach>' switch"
	    exit 1
	endif
	set mach = "$1"
	if ("$1" == "ssun") then
	    set hmach = "ssol"
	else
	    set hmach = "$1"
	endif
	setenv IRAFARCH  $mach
	breaksw
    case -r:				# set root directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-r <irafdir>' switch"
	    exit 1
	endif
	setenv iraf "$1"
	breaksw
    case -C:				# set old cache directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-C <o_cache>' switch"
	    exit 1
	endif
	set o_cache = "$1"
	breaksw
    case -I:				# set old imdir directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-I <o_imdir>' switch"
	    exit 1
	endif
	set o_imdir = "$1"
	breaksw
    case -R:				# set old root directory
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-R <o_iraf>' switch"
	    exit 1
	endif
	set o_iraf = "$1"
	breaksw
    case -u:				# set user name for iraf, e.g, 'iraf'
	if ("$2" != "") then
	    shift
	else
	    ERRMSG  "missing argument to '-u <irafowner>' switch"
	    exit 1
	endif
	set user = "$1"
	breaksw
    default:
	ERRMSG  "install: unknown argument $1"
	goto Usage
	breaksw
    endsw

    if ("$2" == "") then
	break
    else
	shift
    endif
end



#=============================================================================
# See whether we're gonna work ...
#=============================================================================
if ($exec == yes && $WHOAMI != "root" && $mach != "cygwin") then
  clear
  NEWLINE
  BOLD_ON
  echo "		      IRAF "$VERSION" System Installation"
  echo "		      ================================"
  BOLD_OFF
  NEWLINE ; NEWLINE
  BOLD_ON
  echo "======================================================================"
  echo -n "WARNING"
  BOLD_OFF
  echo ": This script must be run as root for changes to take effect."
  echo "         If you decide to proceed,  the 'no-op' flag will be enabled"
  echo "         by default.   No changes will be made to the system  files,"
  echo "         however you will be able to see what the script does."
  BOLD_ON
  echo "======================================================================"
  BOLD_OFF
  NEWLINE
no_op_proc_:
  PROMPT "Proceed with a no-op installation anyway? "
  setenv ans "$<"
  if ("$ans" == "n" || "$ans" == "N" || "$ans" == "no") then
      exit 0
  endif
  if ("$ans" != "" && "$ans" != "y" && "$ans" != "Y" && "$ans" != "yes") then
      echo "Huh?"
      goto no_op_proc_
  endif
  alias PUT "diff \!$ \!^; rm -f $TEMP"
  set exec = no
endif


#=============================================================================
# Initialize the screen output.
#=============================================================================
clear
NEWLINE
BOLD_ON
echo "		      IRAF "$VERSION" System Installation"
echo "		      ================================"
BOLD_OFF
NEWLINE

echo "       Welcome to the IRAF installation script.  This script will first"
echo "  prompt you for several needed path names.  The system will be verified"
echo "  for proper structure before the actual install begins, all error must"
echo "  must be corrected before you will be allowed to continue.  Recommend-"
echo "  ations for fixing problems will be made but no corrective action will"
echo "  be taken directly.  Once properly installed, you will be allowed to"
echo "  do some minimal configuration."

# Print a quick usage summary.
NEWLINE
echo -n "  For each prompt: hit "
BOLD_ON ; echo -n "<CR>"; BOLD_OFF;
echo -n " to accept the default value, "
BOLD_ON ; echo -n "'q'" ; BOLD_OFF
echo ' to quit,'

echo -n "  or "
BOLD_ON ; echo -n "'help'"; BOLD_OFF
echo -n " or ";
BOLD_ON ; echo -n "'?'"; BOLD_OFF
echo -n " to print an explanation of the prompt."
NEWLINE
NEWLINE


#=============================================================================
# Prompt the user for needed paths.
#=============================================================================

NEWLINE
BOLD_ON
echo "========================================================================"
echo "=====================  Query for System Settings  ======================"
echo "========================================================================"
BOLD_OFF
NEWLINE


#=============================================================================
# Set $iraf, the new root directory for iraf.  The system must already have
# been read in at this directory (e.g., /iraf/iraf), but we assume that no
# files have yet been modified.
#=============================================================================

if ("$iraf" == "") then

    if (-e "IRAF.NET" && -e "IS.PORT.GEN") then
        # Use the current directory.
        set d_iraf = `pwd`

    else
        # Make a guess at what the new root directory is.
        set d_iraf = ""
        if (-d /iraf/iraf) then
	    set d_iraf = /iraf/iraf
        else if (-d /iraf) then
	    set d_iraf = /iraf
        else if (-d /usr/local/iraf) then
	    set d_iraf = /usr/local/iraf
        else if (-d /usr/iraf) then
	    set d_iraf = /usr/iraf
        else
	    # Search for an iraf directory.
	    set IDIRS = "/u* /local /home /opt /iraf* /"
	    foreach i ($IDIRS)
	        if (-d $i/iraf) then
		    set d_iraf = "$i/iraf"
		    break
	        endif
	    end
        endif

        if ("$d_iraf" == "") then
	    set d_iraf = /iraf/iraf
        endif
    endif
else
    set d_iraf = $iraf
endif

# If the given directory doesn't exist, compute the root directory relative
# to $iraf/unix/hlib (our current directory, presumably).

if (! -d $d_iraf) then
    set d_iraf = `(cd ../..;pwd)`
endif

iraf_prompt:
    set d_iraf = `echo $d_iraf | sed -e 's+/\(["]*\)$+\1+'`

    BOLD_ON ; echo -n "New iraf root directory " ; BOLD_OFF
    echo -n "($d_iraf): "
    setenv iraf "$<"
    if ("$iraf" == "") then
        setenv iraf "$d_iraf"
    else if ("$iraf" == "quit" || "$iraf" == "q") then
        exit 0
    else if ("$iraf" == "help" || "$iraf" == "h" || "$iraf" == "?") then
        NEWLINE
            MSG "The iraf root directory is the place where the AS distribution"
            MSG "file was unpacked; it contains subdirectories such as 'dev',"
            MSG "'local', 'noao', 'pkg', and the file IS.PORT.GEN."

	set di = $d_iraf
  	if (((-d $di/dev) && (-d $di/pkg) && (-d $di/noao))) then
	    MSG  ""
            MSG  "The default path '$d_iraf' appears to be correct ..."
  	else 
	    MSG  ""
            MSG  "The default path '$d_iraf' appears to be incorrect ..."
  	endif
        NEWLINE

	setenv iraf $d_iraf
        goto iraf_prompt
    endif


# See whether this looks like a reasonable $iraf directory
if (! (-e $iraf)) then
  NEWLINE
      MSG  "The '$iraf' directory doesn't exist."
      MSG  "Please try again..."
  NEWLINE
  goto iraf_prompt

else if (! (-r $iraf) || \
	("`$LS -lLd $iraf |& grep '.rw[xs]r.[xs]r.[xt]'`" == "") || \
	("`$LS -lLd $iraf/.. |& grep '.rw[xs]r.[xs]r.[xt]'`" == "")) then
  NEWLINE
      MSG  "The iraf directory tree is not generally readable."
      MSG  "This will prevent most users from being able to read iraf"
      MSG  "files, especially the binaries\\! "
      MSG  "Please reset the permissions and try again..."
  NEWLINE
  exit 1

else if (! ((-d $iraf/dev) && (-d $iraf/pkg) && (-d $iraf/noao))) then
  NEWLINE
      MSG  "The definition of '$iraf' looks incorrect."
      MSG  ""
      MSG  "The iraf root directory is the place where the AS distribution"
      MSG  "file was unpacked; it contains subdirectories such as 'dev',"
      MSG  "'local', 'noao', 'pkg', and the file IS.PORT.GEN."
      MSG  ""
  if (((-d $iraf/iraf/dev) && (-d $iraf/iraf/pkg) && (-d $iraf/iraf/noao))) then
      MSG  "The path '$iraf/iraf' appears to be correct ..."
      set iraf = $iraf/iraf
  else if (((-d $iraf/../dev) && (-d $iraf/../pkg) && (-d $iraf/../noao))) then
      if (-d $iraf/..) then
	  pushd $iraf/.. >& /dev/null
	  setenv ip `echo $cwd`
          MSG  "The path '$ip' appears to be correct ..."
	  set iraf = $ip
	  popd >& /dev/null
      endif
  endif
      MSG  ""
      MSG  "Please verify your path and try again ..."
  NEWLINE
  goto iraf_prompt
endif


#=============================================================================
# Get the values of o_iraf and o_imdir from the current mkiraf.csh file, if
# not already otherwise defined.
#=============================================================================

cd $iraf/unix/hlib
set WS = '[ 	]'
if ("$o_iraf" == "") then
    set o_iraf =\
    `grep "^set$WS*iraf" mkiraf.csh | sed -e "s+^.*=$WS*++" | sed -e 's+"++g'`
endif
if ("$o_imdir" == "") then
    set o_imdir =\
    `grep "^set$WS*imdir" mkiraf.csh | sed -e "s+^.*=$WS*++" | sed -e 's+"++g'`
endif
if ("$o_cache" == "") then
    set o_cache =\
    `grep "^set$WS*cachedir" mkiraf.csh | sed -e "s+^.*=$WS*++" | sed -e 's+"++g'`
endif

# Strip any trailing / in the pathname to be matched, so that the trailing /,
# if present, will be LEFT in the occurrence of the path in the file.

set o_iraf  = `echo $o_iraf  | sed -e 's+/\(["]*\)$+\1+'`
set o_imdir = `echo $o_imdir | sed -e 's+/\(["]*\)$+\1+'`
set o_cache = `echo $o_cache | sed -e 's+/\(["]*\)$+\1+'`


#=============================================================================
# Get the iraf parent directory to be used below.
#=============================================================================
pushd $iraf/.. >& /dev/null ; set iraf_p = `echo $cwd` ; popd >& /dev/null


#=============================================================================
# Set $imdir, the default user image storage root directory.  Each user imdir
# will be a subdirectory of this directory by default, when MKIRAF is run.
# Since bulk image data can consume hundreds of megabytes of disk space, IRAF
# likes to keep such data on a public scratch device, which is probably not
# backed up, which has a short file expiration interval, and possibly which
# has been configured (newfs/mkfs) with a large block size for fast seq. i/o.
#=============================================================================

if ("$imdir" == "") then

    if (-d $o_imdir) then
	set d_imdir = $o_imdir
    else if (-d /iraf) then
	set d_imdir = /iraf/imdirs
    else if (-d /home/iraf) then
	set d_imdir = /home/iraf/imdirs
    else if (-d $iraf_p) then
	set d_imdir = $iraf_p/imdirs
    else if (-d /usr/local/iraf) then
	set d_imdir = /usr/local/iraf/imdirs
    else
	set d_imdir = /tmp
    endif

imdir_prompt:
    BOLD_ON ; echo -n "Default root image storage directory " ; BOLD_OFF
    echo -n "($d_imdir): "
    set imdir = "$<"
    if ("$imdir" == "") then
	set imdir = "$d_imdir"
    else if ("$imdir" == "quit" || "$imdir" == "q") then
        exit 0
    else if ("$imdir" == "help" || "$imdir" == "h" || "$imdir" == "?") then
        NEWLINE
            MSG "The root imdir directory is the default image storage dir"
	    MSG 'for OIF images (i.e. the ".imh" format) used by all users on'
	    MSG "this system.   Individual user dirs will be created as needed."
	    MSG "It should be some large data disk on the machine which has a"
	    MSG "regular backup, scratch or tmp disks should be avoided or data"
	    MSG "may be lost."
            MSG ""
            MSG 'The "HDR$" syntax should not be used at this stage, please'
            MSG 'edit the hlib$mkiraf.csh script after installation if you wish'
	    MSG "to make this the default."
        NEWLINE
	setenv imdir $d_imdir
        goto imdir_prompt
    endif

    # Cannot have iraf and imdir the same.
    if ("$imdir" == "$iraf") then
  	NEWLINE
            MSG  "The definition of imdir cannot be the same as the iraf"
	    MSG  "root, please choose a different directory.  Ideally this"
	    MSG  "should be some large data area on your system or a user"
	    MSG  "data area such as /home, /users, /u1, etc."
  	NEWLINE
  	NEWLINE
  	goto imdir_prompt
    endif
endif


#=============================================================================
# Set $cache, the default user file cache root directory.
#=============================================================================

if ("$cache" == "") then

    if (-d /iraf) then
	set d_cache = /iraf/cache
    else if (-d /home/iraf) then
	set d_cache = /home/iraf/cache
    else if (-d $iraf_p) then
	set d_cache = $iraf_p/cache
    else if (-d /usr/local/iraf) then
	set d_cache = /usr/local/iraf/cache
    else
	set d_cache = /tmp
    endif

cache_prompt:
    BOLD_ON ; echo -n "Default root cache directory " ; BOLD_OFF
    echo -n "($d_cache): "
    set cache = "$<"
    if ("$cache" == "") then
	set cache = "$d_cache"
    else if ("$cache" == "quit" || "$cache" == "q") then
        exit 0
    else if ("$cache" == "help" || "$cache" == "h" || "$cache" == "?") then
        NEWLINE
            MSG "The root cache directory is the default storage directory for"
	    MSG "URL-referenced files. Individual user dirs will be created as"
	    MSG "needed. It should be some large data disk on the machine "
	    MSG "which has a regular backup, scratch or tmp disks should be"
	    MSG "avoided or data may be lost."
            MSG ""
        NEWLINE
	setenv cache $d_cache
        goto cache_prompt
    endif

    # Cannot have iraf and cache the same.
    if ("$cache" == "$iraf") then
  	NEWLINE
            MSG  "The definition of cache cannot be the same as the iraf"
	    MSG  "root, please choose a different directory.  Ideally this"
	    MSG  "should be some large data area on your system or a user"
	    MSG  "data area such as /home, /users, /u1, etc."
  	NEWLINE
  	NEWLINE
  	goto cache_prompt
    endif
endif


#=============================================================================
# Get UNIX directory where HSI commands (links) are to be installed, if not
# set on command line.  IRAF will only install a very few new commands in this
# directory.  Ideally it should be a directory on the standard user $path,
# so that users do not have to customize their . files just to run IRAF.
#=============================================================================

if ("$lbin" == "") then
    # Look around and come up with a likely candidate directory.
    if (-d /usr/local/bin) then
	set d_lbin = /usr/local/bin
    else if (-d /opt/local/bin) then
	set d_lbin = /opt/local/bin
    else if (-d /local/bin) then
	set d_lbin = /local/bin
    else
	set d_lbin = /usr/bin
    endif

lbin_prompt:
    BOLD_ON ; echo -n "Local unix commands directory " ; BOLD_OFF
    echo -n "($d_lbin): "
    set lbin = "$<"
    if ("$lbin" == "") then
	set lbin = "$d_lbin"
    else if ("$lbin" == "quit" || "$lbin" == "q") then
        exit 0
    else if ("$lbin" == "help" || "$lbin" == "h" || "$lbin" == "?") then
        NEWLINE
            MSG "The local bin directory is the system directory into which the"
            MSG "iraf commands (e.g. cl, mkiraf, mkpkg, etc) will be installed"
	    MSG "as symlinks to files in the iraf tree. This should be a common"
	    MSG "dir such as /usr/local/bin which will likely be found in every"
	    MSG "user's path."
        NEWLINE
	setenv lbin $d_lbin
        goto lbin_prompt
    endif

    # Create the local bin directory if it doesn't exist?
    if (! (-e $lbin)) then
  	PROMPT "    Sorry, but $lbin does not exist, create it? "
        set ans = "$<"
	if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
  	    echo "  Creating directory $lbin..."
	    if ($exec == yes) then
		mkdir $lbin
	    endif
    	    if (! (-e $lbin) && $exec == yes) then
  		ERRMSG  "Cannot create $lbin, please retry..."
		setenv lbin $d_lbin
        	goto lbin_prompt
    	    endif
        else
            goto lbin_prompt
	endif
  	NEWLINE
    endif
endif



#=============================================================================
# Get UNIX directory where IRAF shared library should be installed.
#=============================================================================

if ("$shlib" == "yes") then
    if ("$llib" == "") then
        # Look around and come up with a likely candidate directory.
        if (-d /usr/local/lib) then
            set d_llib = /usr/local/lib
        else if (-d /local/lib) then
            set d_llib = /local/lib
        else if (-d /usr/lib) then
            set d_llib = /usr/lib
        else if (-d /var/shlib) then
            set d_llib = /var/shlib
        else
            set d_llib = /usr/shlib
        endif

llib_prompt:
        BOLD_ON ; echo -n "Directory for local shared libraries " ; BOLD_OFF
 	echo -n "($d_llib): "
        set llib = "$<"
        if ("$llib" == "") then
            set llib = "$d_llib"
        else if ("$llib" == "quit" || "$llib" == "q") then
            exit 0
        else if ("$llib" == "help" || "$llib" == "h" || "llib" == "?") then
            NEWLINE
                MSG "The local lib directory is the system directory into which"
                MSG "the IRAF shared library will be installed as a symlink to"
                MSG "the library in the iraf tree.  The default prompt dir is"
                MSG "one which will be used on this system w/out special user"
                MSG "setup, choose another one with care."
            NEWLINE
	    setenv llib $d_llib
            goto llib_prompt
        endif

        # Create the local lib directory if it doesn't exist?
        if (! (-e $llib)) then
  	    PROMPT "    Sorry, but $llib does not exist, create it? "
            set ans = "$<"
	    if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
  	        echo "  Creating directory $llib..."
	        if ($exec == yes) then
		    mkdir $llib
		endif
    	        if (! (-e $llib) && $exec == yes) then
  		    ERRMSG  "Cannot create $lbin, please retry..."
		    setenv llib $d_llib
        	    goto llib_prompt
    	        endif
            else
                goto llib_prompt
	    endif
  	    NEWLINE
        endif
    endif
endif



##############################################################################
#
#  Step 1: VERIFICATION
#
#  Run some simple checks to be sure the system was unpacked correctly 
#  and the settings used are correct.  Verification tests include:
#
#       o  Verify the machine type and document settings.
#       o  Check iraf root directory looks correct.
#       o  Check iraf root and imdir aren't the same
#       o  Check iraf user exists in passwd file/NIS.
#       o  Check iraf user login path in passwd file is iraf$local.
#       o  Check iraf tree for proper structure.
#       o  Check binary dirs are both populated correctly.
#       o  Check that the local bin directory exists.
#       o  Check that the local lib directory exists.
#
#  An error at this stage will cause the script to exit so we can reset
#  and try again.  
#
##############################################################################

set err_stat	= 0				# initialize error stats
set err_count 	= 0
set warn_stat	= 0				# initialize warning stats
set warn_count 	= 0

NEWLINE
BOLD_ON
echo "========================================================================"
echo "=====================  Verifying System Settings  ======================"
echo "========================================================================"
BOLD_OFF


# Verify the machine type and document the old root pathnames.
NEWLINE
BOLD_ON; echo -n "Hostname      = "; \
    BOLD_OFF; echo `hostname` | awk '{printf("%-20.20s  ", $1)}'
BOLD_ON; echo -n "OS version    = "; \
    BOLD_OFF; echo `$uname_cmd`" "`$uname_cmd -r`
BOLD_ON; echo -n "Architecture  = "; \
    BOLD_OFF; echo $mach | awk '{printf("%-20s  ", $1)}'
BOLD_ON; echo -n "HSI arch      = "; \
    BOLD_OFF; echo $hmach | awk '{printf("%-20s\n", $1)}'
BOLD_ON; echo -n "New iraf root = "; \
    BOLD_OFF; echo $iraf | awk '{printf("%-20s  ", $1)}'
BOLD_ON; echo -n "Old iraf root = "; \
    BOLD_OFF; echo $o_iraf | awk '{printf("%-20s\n", $1)}'
BOLD_ON; echo -n "New imdir     = "; \
    BOLD_OFF; echo $imdir | awk '{printf("%-20s  ", $1)}'
BOLD_ON; echo -n "Old imdir     = "; \
    BOLD_OFF; echo $o_imdir | awk '{printf("%-20s\n", $1)}'
BOLD_ON; echo -n "New cache     = "; \
    BOLD_OFF; echo $cache | awk '{printf("%-20s  ", $1)}'
BOLD_ON; echo -n "Old cache     = "; \
    BOLD_OFF; echo $o_cache | awk '{printf("%-20s\n", $1)}'
BOLD_ON; echo -n "Local bin dir = "; \
    BOLD_OFF; echo $lbin | awk '{printf("%-20s\n", $1)}'
if ("$shlib" == "yes") then
    BOLD_ON; echo -n "Local lib dir = "; \
        BOLD_OFF; echo $llib | awk '{printf("%-20s\n", $1)}'
endif
NEWLINE ; NEWLINE


# Check iraf root directory looks correct.
echo -n "Checking definition of iraf root directory ...			"
if (! ((-d $iraf/dev) && (-d $iraf/pkg) && (-d $iraf/noao))) then
  DO_FAIL ; set err_stat = 1 ; set iraf_root_ok = 0
  NEWLINE
      MSG  "The definition of '$iraf' looks incorrect.  The iraf root"
      MSG  "directory is the place where the AS distribution was unpacked,"
      MSG  "it contains subdirectories such as 'dev', 'local', 'noao', and"
      MSG  "'pkg' and the binary directory links."
      MSG  ""
  if (((-d $iraf/iraf/dev) && (-d $iraf/iraf/pkg) && (-d $iraf/iraf/noao))) then
      MSG  "The path '$iraf/iraf' appears to be correct ..."
      set iraf = $iraf/iraf
  else if (((-d $iraf/../dev) && (-d $iraf/../pkg) && (-d $iraf/../noao))) then
      if (-d $iraf/..) then
          pushd $iraf/.. >& /dev/null
          setenv ip `echo $cwd`
          MSG  "The path '$ip' appears to be correct ..."
          set iraf = $ip
          popd >& /dev/null
      endif
  endif
      MSG  "Please verify your path and try again ..."
  NEWLINE
  set err_count = `expr $err_count + 1`
else
  DO_OK ; set iraf_root_ok = 1
endif


# Cannot have iraf and imdir the same.
echo -n "Checking iraf root and imdir directory ...			"
if ($iraf == $imdir) then
  DO_FAIL ; set err_stat = 1
  NEWLINE
      MSG  "The 'imdir' pixel storage directory cannot be the same as the"
      MSG  "iraf root directory.  Please choose another directory."
  NEWLINE
  set err_count = `expr $err_count + 1`
else
  DO_OK
endif

# Cannot have iraf and cache the same.
echo -n "Checking iraf root and cache directory ...			"
if ($iraf == $cache) then
  DO_FAIL ; set err_stat = 1
  NEWLINE
      MSG  "The 'cache' storage directory cannot be the same as the"
      MSG  "iraf root directory.  Please choose another directory."
  NEWLINE
  set err_count = `expr $err_count + 1`
else
  DO_OK
endif



if ($no_edit == 0) then

# Check for write permission on iraf directory...
echo -n "Checking iraf directory write permissions ...			"

  set TEST	= "_inst.check"
  if ($exec == "yes") then
    set back 	= $cwd/
    chdir $iraf_p/iraf
    touch $TEST 	>& /dev/null
    if (-e $TEST) then
        rm $TEST
        DO_OK
    else
        DO_WARN ; set warn_stat = 1
        NEWLINE
            MSG  "You do not have write permission on the iraf directory."
            MSG  "This will prevent the install script from modifying files"
            MSG  "in the tree needed to complete the setup.  If this is an"
            MSG  "NFS disk there can be several explanations aside from just"
	    MSG  "the usual permissions problems:"
	    MSG  ""
	    MSG  "  1) The NFS disk is mounted read-only"
	    MSG  "  2) The NFS disk is mounted with the 'nosuid' option"
	    MSG  "  3) The NFS disk is exported with the 'all_squash' option"
	    MSG  "     which remaps your userid to 'anonymous'"
	    MSG  ""
	    MSG  "The script can continue by installing the command links on"
	    MSG  "the local machine, but will not edit the iraf path into the"
	    MSG  "files.  If you choose not to do this the verification stage"
	    MSG  "will be completed but the installation will then abort."
	    MSG  ""
	    MSG  "In either case, you should verify the server iraf path is"
	    MSG  "correct for this machine and rerun the install script or"
	    MSG  "make the appropriate links as needed."
	    MSG  ""
	    MSG  ""

	    echo -n '   '
	    BOLD_ON
	    echo -n '***  Would you like to continue with a no-edit install? '
	    BOLD_OFF
	    echo -n "(yes) "

            set ans = "$<"
            if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
		set no_edit = 1
	    else
		set err_stat = 1
	    endif
        NEWLINE
        set warn_count = `expr $warn_count + 1`
    endif
    chdir $back
  else
    DO_OK
  endif
endif



# Check iraf user.

goto _no_user_check

#  Check for iraf user disabled for all platforms with v2.15
if ($mach == "macosx" || $mach == "macintel" || $mach == "cygwin") then
    goto _no_user_check
endif

echo -n "Checking for iraf user account ...				"
set pass = ""
if (($pciraf && ($mach == "macosx" || $mach == "macintel" || $mach == "cygwin")) || (!(-r /etc/passwd)) ) then
  # Special-case user info check for OS X and systems where the /etc/passwd
  # file may not contain the user info or isn't readable.

  set id_found = 0
  foreach d ($path)
      if (-e $d/id) then
          set id_found = 1
          break
      endif
  end
  if ($id_found == 1) then
    if (`id iraf |& grep -i 'no such user'` != "") then
      DO_WARN ; set warn_stat = 1
      NEWLINE
          MSG  "No 'iraf' user was found on the system.  The iraf user should"
          MSG  "be created before installing the system to ensure all files"
          MSG  "are owned by the iraf user, and the have the proper environment"
          MSG  "defined for installation and maintanence."
      NEWLINE

    else
      DO_OK

      # Check iraf user login path and shell
      echo -n "Checking iraf user login directory ... 				" 

      set v = `finger iraf |& egrep '^Directory'`
      set ihome = `echo $v[2] | sed -e 's+/\(["]*\)$+\1+'`
      set shel  = `echo $v[4] | sed -e 's+/\(["]*\)$+\1+' | grep csh`
      if ("$ihome" != "$iraf/local" && $shel == "") then
        DO_FAIL ; set errstat = 1
        NEWLINE   
            MSG  "The iraf user login info appears to be incorrect.  For the"
            MSG  "given iraf root this path should be '$iraf/local',"
            MSG  "please run the 'chpass' command to change this.  The iraf"
            MSG  "user account should also be defined to use a C-shell."
        if ("$iraf_root_ok" == 0) then
            MSG  "(This error may be related to the incorrect definition of"
            MSG  "the iraf root directory seen above.)"
        endif
        NEWLINE
      else
        DO_OK
      endif

    else

    endif
  endif

else
  set pass1 = "`grep ^iraf: /etc/passwd`"
  set pass2 = "`ypcat passwd |& grep ^iraf:`"

  if ("$pass1" == "" && "$pass2" == "") then
    DO_WARN ; set warn_stat = 1
    NEWLINE
        MSG  "No 'iraf' user was found in the /etc/passwd file.  The iraf"
        MSG  "user should be created before installing the system to ensure"
        MSG  "all files are owned by the iraf user, and the have the proper"
        MSG  "environment defined for installation and maintanence."
    NEWLINE
  else
    DO_OK

    # Check iraf user login path in passwd file is iraf$local.
    echo -n "    Checking iraf user login directory ...			"
    if ("$pass1" != "") then
        set pass = `grep ^iraf: /etc/passwd |sed -e 's/[ \*]/_/g' |sed -e 's/:/ /g'`
    else
        set pass = `ypcat passwd | grep ^iraf: |sed -e 's/[ \*]/_/g' |sed -e 's/:/ /g'`
    endif

    set c = `echo $pass | wc -w`
    set indx = `expr $c - 1`

    set ihome = `echo $pass[$indx] | sed -e 's+/\(["]*\)$+\1+'`
    if ("$ihome" != "$iraf/local") then
      DO_FAIL ; set errstat = 1
      NEWLINE
          MSG  "The iraf user login directory appears to be incorrect."
          MSG  "For the given iraf root this path should be '$iraf/local',"
          MSG  "please edit the /etc/passwd file to change this."
      if ("$iraf_root_ok" == 0) then
          MSG  "(This error may be related to the incorrect definition of"
          MSG  "the iraf root directory seen above.)"
      endif
      NEWLINE
    else
      DO_OK
    endif

    echo -n "    Checking iraf user account shell ...			"
    set shel =  `echo $pass[$c] | sed -e 's+/\(["]*\)$+\1+' | grep csh`
    if ($shel == "") then
      DO_FAIL ; set errstat = 1
      NEWLINE
          MSG  "The iraf user login directory appears to be incorrect."
          MSG  "The account should be configured to use a C-shell in order"
          MSG  "to take advantage of a preconfigured environment which will"
          MSG  "make maintainence easier.  Please edit the passwd file or"
          MSG  "use the 'chsh' command (if available) to change this."
      NEWLINE
    else
      DO_OK
    endif
  endif
endif

_no_user_check:


# Skip the ownership check on some platforms.
if ($V != "2.15" && $V != "2.16") then
 if ($mach != "cygwin" && $mach != "macosx" && $mach != "macintel") then

  # Check file ownership.
  if ($mach == "hp700" || $mach == "rs6000") then
    set downr = `$LS -lLd $iraf_p/iraf | awk '{print ($4)}'`
    set fownr = `$LS -lLd $iraf_p/iraf/mkpkg | awk '{print ($4)}'`
  else
    set downr = `$LS -lLd $iraf_p/iraf | awk '{print ($3)}'`
    set fownr = `$LS -lLd $iraf_p/iraf/mkpkg | awk '{print ($3)}'`
  endif
  set dperm = `$LS -lLd $iraf_p/iraf | awk '{print ($1)}'`
  set fperm = `$LS -lLd $iraf_p/iraf/mkpkg | awk '{print ($1)}'`


  echo -n "Checking file ownerships ...					"
  if ("$downr" == "iraf" && "$fownr" == "iraf") then
    DO_OK
  else if ("$downr" == "tody" && "$fownr" == "tody") then
    # Special exemption for NOAO installations.
    DO_OK
  else 
    DO_WARN ; set warn_stat = 1
    NEWLINE
        MSG  "(root dir owned by $downr, iraf files owned by $fownr)"
        MSG  "The iraf tree should be owned by the iraf user so it can"
        MSG  "be updated and maintained properly."
        MSG  ""
        MSG  'To fix this, login as root, set the iraf environment, and'
        MSG  'issue the commands:'
        MSG  ""
        MSG  "	cd " `echo $iraf_p`
        MSG  '	chown -R iraf .		# change dir owner'
        MSG  '	cd $hbin		# go to HSI bin dir'
        MSG  '	chown 0 alloc.e		# fix alloc.e ownership'
        MSG  '	chmod 4755 alloc.e	# fix permissions'
    NEWLINE
    set warn_count = `expr $warn_count + 1`
  endif

 endif

# Check file/directory permissions.
echo -n "Checking file permissions ...					"

set err_seen 	= 0
foreach dir ($iraf_p $iraf_p/iraf $iraf_p/iraf/unix $iraf_p/iraf/unix/hlib)
    if ("`$LS -lLd $dir | grep '.rw[xs]r.[xs]r.[xt]'`" == "") then
 	set err_seen = 1
	break
    endif
end

if ("$err_seen" == 0) then
    DO_OK
else
    DO_FAIL ; set err_stat = 1
    NEWLINE
        MSG  "The permissions on the iraf directory tree appear to be too"
        MSG  "restrictive to allow group or world user to access the file"
        MSG  "This may prevent users other than $downr from starting the system"
    NEWLINE
    set err_count = `expr $err_count + 1`
endif

endif


# Check iraf tree for proper structure.
set check_iraf_tree = 0			# for v214 and earlier

set iraf_r  = $iraf			# iraf root directory
set iraf_p  = $iraf_r:h			# iraf parent directory
set iraf_b  = $iraf_p/irafbin		# irafbin directory
set iraf_tree_ok = 1

set iraf_ib = $iraf/bin.$mach		# irafbin IB directory
set iraf_nb = $iraf/noao/bin.$mach	# irafbin NB directory


if ($check_iraf_tree == 1) then

set iraf_ib = $iraf_b/bin.$mach		# irafbin IB directory
set iraf_nb = $iraf_b/noao.bin.$mach	# irafbin NB directory


echo "Checking proper iraf tree structure in $iraf_p ..."

echo -n "    Checking for 'iraf' subdir ...				"
if (-d "$iraf_p/iraf") then
    DO_OK
else
    DO_FAIL ; set err_stat = 1 ; set iraf_tree_ok = 0
    set err_count = `expr $err_count + 1`
endif

echo -n "    Checking for 'irafbin' subdir ...				"
if (-d "$iraf_p/irafbin") then
    DO_OK
else
    set temp    = $iraf_tree_ok
    #echo -n "[ "; BOLD_ON; echo -n "Not Found" ; BOLD_OFF; echo " ]" ; 
    echo ""
    set iraf_tree_ok = 0

    # Look for a fallback to recover ...
    echo -n "	Checking for fallback tree structure ...	"
    set iraf_p = $iraf/../
    set iraf_b  = $iraf_p/irafbin		# irafbin directory
    set iraf_ib = $iraf_b/bin.$mach		# irafbin IB directory
    set iraf_nb = $iraf_b/noao.bin.$mach	# irafbin NB directory
    if (-d "$iraf_p/irafbin") then
        echo -n "	"; DO_OK ; set iraf_tree_ok = $temp
    else
        echo -n "	"; DO_FAIL; set err_stat = 1 ; set iraf_tree_ok = 0
        set err_count = `expr $err_count + 1`
    endif
endif


echo "'irafbin/bin.$mach'" | \
	awk '{ printf ("    Checking for %s subdir ...   \t", $1) }'
if (-d "$iraf_p/irafbin/bin.$mach") then
    echo -n "	"; DO_OK
else
    echo -n  "	"; DO_FAIL; set err_stat = 1 ; set iraf_tree_ok = 0
    set err_count = `expr $err_count + 1`
endif

echo "'irafbin/noao.bin.$mach'" | \
	awk '{ printf ("    Checking for %s subdir ...\t\t", $1) }'
if (-d "$iraf_p/irafbin/noao.bin.$mach") then
    DO_OK
else
    DO_FAIL ; set err_stat = 1 ; set iraf_tree_ok = 0
    set err_count = `expr $err_count + 1`
endif

if ("$iraf_tree_ok" == 0) then
  NEWLINE
      MSG  "An error was detected in the structure of the iraf tree."
      MSG  "Your directory tree should look something like:"
      MSG  ""
      MSG  "                    	$iraf_p"
      MSG  "                    	  /  \"
      MSG  "          	(AS) /iraf  /irafbin"
      MSG  "                              /  \"
      MSG  "            	(IB) bin.$mach  noao.bin.$mach (NB)"
      MSG  ""
      MSG  "The AS, IB, and NB distribution files are shown where they"
      MSG  "should be unpacked.  Please verify the structure and the"
      MSG  "system architecture."
  NEWLINE
endif

endif	# if (check_iraf_tree)




# Check binary dirs are both populated correctly.
echo -n "Checking Core system binary directory ...			"
if (! (-e "$iraf_ib/cl.e" && -e "$iraf_ib/x_system.e")) then
  if ($port == 1) then
      DO_WARN ; set warn_stat = 1
  else
      DO_FAIL ; set err_stat = 1
  endif
  NEWLINE
      MSG  "The core system binary directory, $iraf_ib, does"
      MSG  "not appear to contain the proper binaries."
  NEWLINE
  set err_count = `expr $err_count + 1`
else
  DO_OK
endif

echo -n "Checking NOAO package binary directory ...			"
if (! (-e "$iraf_nb/x_apphot.e" && -e "$iraf_nb/x_rv.e")) then
  if ($port == 1) then
      DO_WARN ; set warn_stat = 1
  else
      DO_FAIL ; set err_stat = 1
  endif
  if (-e "$iraf_ib/x_apphot.e" && -e "$iraf_ib/x_rv.e") then
      NEWLINE
          MSG  "The NOAO package binary directory, $iraf_nb, is"
          MSG  "empty but the binaries appear to have been unpacked in the"
          MSG  "core system directory, $iraf_ib.  These will need to be moved,"
          MSG  "please delete the binaries and start again, be sure to unpack"
          MSG  "the NB distribution files in the $iraf_nb directory,"
          MSG  "and the core system file in the $iraf_ib directory."
      NEWLINE
  else
      NEWLINE
          MSG  "The NOAO package binary directory, $iraf_nb, does"
          MSG  "not appear to contain the proper files."
      NEWLINE
  endif
  set err_count = `expr $err_count + 1`
else
  DO_OK
endif


# Check that the specified local bin directory exists.
echo -n "Checking that local bin directory exists ...			"
if (-d "$lbin") then
    DO_OK
else
    DO_FAIL ; set err_stat = 1
    NEWLINE
        MSG  "The specified local bin directory does not exist.  This"
        MSG  "directory should be a common local bin directory which "
        MSG  "is found in all user's paths, e.g. /usr/local/bin."
        MSG  "Please create the directory or else reset and try again."
    NEWLINE
    set err_count = `expr $err_count + 1`
endif


# Check that the specified local lib directory exists.
if ("$shlib" == "yes") then
  echo -n "Checking that local lib directory exists ...			"
  if (-d "$llib") then
    DO_OK
  else
    DO_FAIL ; set err_stat = 1
    NEWLINE
        MSG  "The specified local lib directory does not exist.  This"
        MSG  "directory should be a common local lib directory which "
        MSG  "is found in all user's paths, e.g. /usr/local/lib."
        MSG  "This directory is required for the iraf shared library."
        MSG  "Please create the directory or else reset and try again."
    NEWLINE
    set err_count = `expr $err_count + 1`
  endif
endif


# See if we're good to go ...
if ("$err_stat" != "0") then
  NEWLINE ; NEWLINE
  BOLD_ON
  echo "======================================================================"
  echo "|   An error occured during verification.  Please take corrective    |"
  echo "|          action before rerunning the install script.               |"
  echo "======================================================================"
  BOLD_OFF
  NEWLINE
  exit $err_stat
endif



# ============================================
# The following is partially system dependent.
# ============================================

# Set the BINDIRS pathnames - directories where the HSI executables go.
set host	= "$iraf/unix"
set hbin	= "$iraf/unix/bin.$hmach"
set hlib	= "$iraf/unix/hlib"
set fbin	= "$iraf/bin"

# Replace any // by /.
set host	= `echo $host | sed -e "s+//+/+g"`
set hbin	= `echo $hbin | sed -e "s+//+/+g"`
set fbin	= `echo $fbin | sed -e "s+//+/+g"`
set hlib	= `echo $hlib | sed -e "s+//+/+g"`

# Strip any trailing /.
set host	= `echo $host | sed -e 's+/\(["]*\)$+\1+'`
set hbin	= `echo $hbin | sed -e 's+/\(["]*\)$+\1+'`
set fbin	= `echo $fbin | sed -e 's+/\(["]*\)$+\1+'`
set hlib	= `echo $hlib | sed -e 's+/\(["]*\)$+\1+'`
set BINDIRS	= "$hbin $hlib $fbin $host"

# The following file lists are partially system dependent.
set PATHFILES	= "mkiraf.csh libc/iraf.h cl.csh"
set MODEFILES	= "cl.csh fc.csh mkiraf.csh mkfloat.csh mkmlist.csh $host/reboot generic.e mkpkg.e rmbin.e rmfiles.e rpp.e rtar.e wtar.e xc.e xpp.e xyacc.e sgidispatch.e $hbin/sgi2*.e irafarch.csh"
set LINKFILES	= "ecl.e cl.e mkiraf.csh mkmlist.csh generic.e mkpkg.e rmbin.e rmfiles.e rtar.e sgidispatch.e wtar.e rpp.e xpp.e xyacc.e xc.e"
set CMDLINKS	= "ecl cl mkiraf mkmlist generic mkpkg rmbin rmfiles rtar sgidispatch wtar rpp xpp xyacc xc irafarch"


#=============================================================================
# See whether there is an existing commands dir we need to delete.
#=============================================================================

echo -n "Checking for existing commands directory...			"
set cl_found = 0
set clpath   = ""
foreach d ($path)
    if (-e $d/cl) then
	set cl_found = 1
	set clpath = $d/cl
	break
    endif
end
if ($cl_found == 1) then
    set o_lbin = $clpath:h

    if ("$o_lbin" != "$lbin") then
        DO_WARN
	NEWLINE
 	    MSG  "IRAF commands were found in the directory:"
 	    MSG  ""
 	    MSG  "	$o_lbin"
	    MSG  ""
 	    MSG  "    These commands may conflict with the commands now being"
  	    MSG  "installed in:  '$lbin'"
	    MSG  ""
del_cmd_:
	PROMPT "Do you want to delete commands in the old directory? "
        set ans = "$<"

	if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
	    NEWLINE
	    foreach i ($CMDLINKS)               # remove the iraf commands
	        set file = $o_lbin/$i
	        if (-e $file) then
	            MSG  "Deleting command $file ..."
	            if ($exec == yes) then
	                RM $file
	            endif
	        endif
	    end
	else if ("$ans" == "quit" || "$ans" == "q") then
	    exit 1
	else if ("$ans" == "no" || "$ans" == "n") then
	    ;	# fall through
	else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
	    NEWLINE
	        MSG "Multiple commands such as 'cl' or 'mkiraf' on a machine"
	        MSG "may cause errors (such as 'command not found' due to an"
		MSG "invalid link), or confusions as to which version of iraf"
		MSG "is being run if the old link is still valid.  This is"
		MSG "because the command being used depends on the order in"
		MSG 'which the directories occur in the users "$path" environ-'
		MSG "ment variable (which may vary by user)."
		MSG ""
		MSG "It is recommended there be only one iraf command directory"
		MSG "on a given system, other methods can be used to start a"
		MSG "different IRAF installation.  This script will not auto-"
		MSG "matically remove those links, and will only correct the"
		MSG "path is the local bin directory is the same as before."
		MSG ""
		MSG "Type 'q' to quit and rerun the install script to specify"
		MSG "a different local bin directory, 'yes' to remove the old"
		MSG "links, and 'no' to leave the old commands around."
		MSG ""
	    NEWLINE
	    goto del_cmd_
        endif
	NEWLINE
    else
        DO_OK
    endif

else
    DO_OK
endif


#=============================================================================
# Prompt for the go-ahead ...
#=============================================================================
NEWLINE
proceed_:
PROMPT "Proceed with installation? "
set ans = "$<"
if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
    NEWLINE
else if ("$ans" == "quit" || "$ans" == "q") then
    exit 0
else if ("$ans" == "no" || "$ans" == "n") then
    exit 0
else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
    NEWLINE
        MSG  "If you proceed, the system will be installed on this machine."
        MSG  "This means that command links will be placed in the local bin"
        MSG  "directory, needed system files will be created, and the iraf"
	MSG  "root path will be edited into key files.  Stopping at this stage"
	MSG  "will have no side effects on your system or the iraf files."
	MSG  "Type <cr> to continue, or 'q' to quit to exit the installation."
    NEWLINE
    goto proceed_
else 
    echo "Huh?"
    goto proceed_
endif


##############################################################################
#
#  Step 2: INSTALLATION
#
#  Do the actual installation.  This involves:
#	
#	1) Editing the $iraf path into system files
#	2) Creating the <iraf.h> link
#	3) Creating the system command links
#	4) Creating the image directory (imdir)
#	5) Creating the cache directory (cache)
#	6) Tape setup (modes on alloc.e and /dev tape devices)
#	7) Graphics/Display file installation/setup
#
##############################################################################

# Begin installation.
# ------------------

set err_seen = 0


NEWLINE
BOLD_ON
echo "========================================================================"
echo "=========================  Begin Installation  ========================="
echo "========================================================================"
BOLD_OFF

# Skip ahead if we're not editing the files or touching the disk.
if ($no_edit == 1) then
    goto end_no_edit
endif


NEWLINE
BOLD_ON
echo "                             Editing Paths"
echo "                             -------------"
BOLD_OFF

# Edit the $iraf pathname in the .login file for user 'iraf'.
echo -n "Editing the iraf user .login/.cshrc paths ...			"
cd $iraf/local

foreach file (.cshrc .login)
    if (-e $file) then
        RM $TEMP >& /dev/null
        sed -e "s+$W$o_iraf+\1$iraf+" $file > $TEMP
        cmp -s $file $TEMP
        if ($status) then
	    PUT $TEMP $file
        endif
        RM $TEMP >& /dev/null
    else
        if ("$err_seen" == 0) then
            DO_FAIL
            set err_seen = 1
            set err_count = `expr $err_count + 1`
        endif
        MSG  "Cannot find the iraf $file file"
        RM $TEMP >& /dev/null
    endif
end
if ("$err_seen" == 0) then
    DO_OK
endif


# If we're on a Mac OS X or Cygwin system the iraf login directory isn't easy 
# to change and as of OSX 10.1 the default is to create this in /Users/iraf.  
# If this is how things are set up then link the .login/.cshrc files in this 
# directory so the account has the proper environment.

set err_seen = 0

if ($V != "2.15" && $V != "2.16") then

 if ($pciraf && ($mach == "macosx" || $mach == "macintel" || $mach == "cygwin")) then
 echo -n "Creating iraf user .login/.cshrc links ...			"

    set v = `finger iraf |& egrep '^Directory'`
    set ihome = `echo $v[2] | sed -e 's+/\(["]*\)$+\1+'`

    if ("$ihome" != "$iraf/local") then
      cd $ihome				# go to account login dir

      if (! (-e bugs.log)) then		# make sure it's not iraf$local
          foreach file (.cshrc .login)
              if ($exec == yes) then
		if (-e $file) then
                    RM $file >& /dev/null	# remove old file
		endif
		ln -s $iraf/local/$file $file	# make local link

		if (! (-e $file) && $exec == yes) then
		    if ("$err_seen" == 0) then
		        DO_FAIL
                        set err_seen = 1
                        set err_count = `expr $err_count + 1`
		    endif
        	    MSG  "Cannot find the iraf $file file in $ihome"
        	    RM $TEMP >& /dev/null
		endif

	      endif
          end
      endif
      cd $iraf/local

    else
	DO_OK
    endif

    if ("$err_seen" == 0) then
        DO_OK
    endif
 endif

endif


# Edit the $iraf and $imdir paths in mkiraf.csh, cl.csh, and libc/iraf.h files.

echo -n "Editing iraf/imdir paths into system files ...			"
cd $iraf/unix/hlib

set err_seen = 0
foreach i ($PATHFILES)
    if (-e $i) then
	RM $TEMP >& /dev/null
	sed -e "s+$W$o_iraf+\1$iraf+" $i |\
	    sed -e "s+$W$o_cache+\1$cache+" | \
	    sed -e "s+$W$o_imdir+\1$imdir+" > $TEMP
	cmp -s $i $TEMP
	if ($status) then
	    if ($exec == yes) then
	        PUT $TEMP $i
		chmod 755 $i
	    endif
	endif
        RM $TEMP >& /dev/null
    else
	if ("$err_seen" == 0) then
	    DO_FAIL
	    set err_seen = 1
            set err_count = `expr $err_count + 1`
	endif
	MSG  "File $i not found."
        RM $TEMP >& /dev/null
    endif
end
if ("$err_seen" == 0) then
    DO_OK
endif



NEWLINE
BOLD_ON
echo "                       Checking File Permissions"
echo "                       -------------------------"
BOLD_OFF

# Set default file permissions for the executable files in the BINDIRS,
# in case the file mode has somehow been changed, e.g., in a file restore
# or copy.

echo -n "Checking iraf file permissions ...				"

set err_seen = 0
foreach i ($MODEFILES)
    set file = $i
    if (! -e $file) then
	foreach j ($BINDIRS)
	    if (-e $j/$i) then
		set file = $j/$i
		break
	    endif
	end
    endif
	
    if (-e $file) then
	if ("`$LS -l $file | grep '^.rw[xs]r.[xs]r.[xt]'`" == "") then
	    if ("$err_seen" == 0) then
	        DO_WARN
		set err_seen = 1
                set err_count = `expr $err_count + 1`
	    endif
	    MSG  "Setting $file:t to mode 0755."
	    if ($exec == yes) then
		chmod 755 $file
	    endif
	endif
    else
	if ("$err_seen" == 0) then
	    DO_FAIL
	    set err_seen = 1
            set err_count = `expr $err_count + 1`
	endif
	MSG  "File $file:t not found."
    endif
end
if ("$err_seen" == 0) then
    DO_OK
endif


# Create the root imdir as a public scratch directory, if not already created.
set err_seen = 0
if (-d $imdir) then
    if ("`$LS -ld $imdir | grep '^.rw[xs]r.[xs]r.[xt]'`" != "") then
	echo -n 'Checking imdir permissions ...'
	echo -n '					'
    else
	echo -n 'Setting mode for $imdir to 0777			'
	if ($exec == yes) then
	    chmod 777 $imdir
	endif
    endif
else
    echo -n "Creating root imdir at $imdir ...				"
    if ($exec == yes) then
	mkdir $imdir; chmod 777 $imdir
    endif
endif
if ("$err_seen" == 0) then
    DO_OK
endif


# Create the root cache as a public scratch directory, if not already created.
set err_seen = 0
if (-d $cache) then
    if ("`$LS -ld $cache | grep '^.rw[xs]r.[xs]r.[xt]'`" != "") then
	echo -n 'Checking cache permissions ...'
	echo -n '					'
    else
	echo -n 'Setting mode for $cache to 0777			'
	if ($exec == yes) then
	    chmod 777 $cache
	endif
    endif
else
    echo -n "Creating root cache at $cache ...				"
    if ($exec == yes) then
	mkdir $cache; chmod 777 $cache
    endif
endif
if ("$err_seen" == 0) then
    DO_OK
endif



# Initialize permissions of tape devices.
if ($do_tapes) then
echo -n "Setting tape device permissions ...				"
    if ($exec == yes) then
        chmod 666 $TAPES >& /dev/null
	chown root $TAPES >& /dev/null
    endif
    DO_OK
endif



# Set owner=root for the device allocation task, alloc.e.

echo -n "Checking alloc.e permissions ...				"

set err_seen = 0
foreach i ($BINDIRS)
    if (-e $i/alloc.e) then
	if ("`$LS -l $i/alloc.e | grep 'rwsr-.*root'`" == "") then
	    if ($exec == yes) then
		chown 0 $i/alloc.e
		chmod u+s $i/alloc.e
	    endif
	endif
	break
    else
	if ("$err_seen" == 0) then
	    DO_FAIL
	    set err_seen = 1
            set err_count = `expr $err_count + 1`
	endif
	MSG  "Cannot find alloc.e executable."
    endif
end
if ("$err_seen" == 0) then
    DO_OK
endif


# Target for no-edit install.
end_no_edit:


NEWLINE
BOLD_ON
echo "                         Creating File Links"
echo "                         -------------------"
BOLD_OFF


# Create a /iraf symlink on the system to establish a /iraf/iraf root
# path regardless of the actual root dir.  We only do this if there is
# no /iraf on the system already.

echo -n "Checking for /iraf symlink ...					"
if (! -e /iraf) then
    if ($exec == yes) then
	ln -s $iraf_p /iraf
    endif
    if ($exec == no || -e /iraf/iraf ) then
	DO_OK
    else
	DO_FAIL
        set err_count = `expr $err_count + 1`
    endif
else
    DO_OK
endif


# Link $hlib/libc/iraf.h to <iraf.h>.  This is needed not only to compile C
# source files in iraf, but also to define $iraf, $host, etc. for iraf tasks.

# Verify we have a /usr/include directory (some MacOSX systems won't)
echo -n "Checking /usr/include directory ...				"
if (! -e /usr/include) then
    if ($exec == yes) then
        mkdir /usr/include
        if (-d /usr/include) then
            DO_OK
        else
            DO_FAIL
            set err_count = `expr $err_count + 1`
        endif
    else
        DO_OK
    endif
else
    DO_OK
endif


echo -n "Creating <iraf.h> symlink ...					"
set file1 = /usr/include/iraf.h
set file2 = $iraf/unix/hlib/libc/iraf.h

set err_seen = 0
if (-e $file1) then
    if ("`$LS -l $file1 | grep $file2`" == "") then
	if ($exec == yes) then
	    RM $file1
	endif
	if ($exec == yes) then
	    ln -s $file2 $file1
	endif
    endif
else
    if ($exec == yes) then
	ln -s $file2 $file1
    endif
endif
if (("$err_seen" == 0 && -e $file1) || $exec == "no") then
    DO_OK
else
    DO_FAIL
    set err_count = `expr $err_count + 1`
endif


# Establish the remaining symbolic links to HSI tasks.
echo -n "Creating iraf command links in local bin dir ...		"
cd $lbin

set err_seen = 0
foreach i ($LINKFILES)
    # Locate the file to be linked to.
    set file1 = $i:r
    foreach j ($BINDIRS)
	set file2 = $j/$file1.csh
	if (-e $file2) then
	    break
	endif
	set file2 = $j/$i
	if (-e $file2) then
	    break
	endif
    end

    # Verify or set the link.
    if (-e $file1) then
	if ("`$LS -l $file1 | grep $file2`" == "") then
	    if ($exec == yes) then
	        RM $file1
	    endif
	    if ($exec == yes) then
		ln -s $file2 $file1
	    endif
	endif
    else
	if ($exec == yes) then
	    RM $file1
	    ln -s $file2 $file1
	endif
    endif

    if (! (-e $file1) && "$exec" == "yes") then
        if ("$err_seen" == 0) then
            DO_FAIL
        endif
        MSG  "Could not make link $file1 -> $file2"
	set err_seen = 1
        set err_count = `expr $err_count + 1`
    endif
end
if ("$err_seen" == 0) then
    DO_OK
endif


# Establish the remaining symbolic links to shared libraries.
if ("$shlib" == "yes") then
    echo -n "Creating iraf library links in local lib dir ...		"
    cd $llib

    set err_seen = 0
    foreach i ($LIBFILES)
        # Locate the file to be linked to.
        set file1 = $i
        foreach j ($iraf/bin.$mach $iraf/bin.$hmach)
	    set file2 = $j/$i
	    if (-e $file2) then
	        break
	    endif
        end

        # Verify or set the link.
        if (-e $file1) then
	    if ("`$LS -l $file1 | grep $file2`" == "") then
	        if ($exec == yes) then
	            RM $file1
	        endif
	        if ($exec == yes) then
		    ln -s $file2 $file1
	        endif
	    endif
        else
	    if ($exec == yes) then
	        RM $file1
	        ln -s $file2 $file1
	    endif
        endif

        if (! (-e $file1) && "$exec" == "yes") then
            if ("$err_seen" == 0) then
                DO_FAIL
            endif
            MSG  "Could not make link $file1 -> $file2"
	    set err_seen = 1
            set err_count = `expr $err_count + 1`
        endif
    end
    if ("$err_seen" == 0) then
        DO_OK
    endif
endif


# Mark the system update time.
if ($no_edit == 0) then
    echo -n 'Marking system update time hlib$utime ...			'
    if ($exec == yes) then
        touch $hlib/utime
    endif
    DO_OK
endif


#=============================================================================
# Install the VOClient Daemon code.
#=============================================================================

NEWLINE
BOLD_ON
echo "                    Installing VOClient Code"
echo "                    ------------------------"
BOLD_OFF


echo -n "Creating 'voclientd' symlink ...				"
set file1 = $lbin/voclientd
set file2 = $iraf/vo/java/voclientd

set err_seen = 0
if (-e $file1) then
    if ("`$LS -l $file1 | grep $file2`" == "") then
	if ($exec == yes) then
	    RM $file1
	endif
	if ($exec == yes) then
	    ln -s $file2 $file1
	endif
    endif
else
    if ($exec == yes) then
	ln -s $file2 $file1
    endif
endif
if (("$err_seen" == 0 && -e $file1) || $exec == "no") then
    DO_OK
else
    DO_FAIL
    set err_count = `expr $err_count + 1`
endif


echo -n "Creating 'voclient.jar' symlink ...				"
set file1 = $lbin/voclient.jar
set file2 = $iraf/vo/java/voclient.jar

set err_seen = 0
if (-e $file1) then
    if ("`$LS -l $file1 | grep $file2`" == "") then
	if ($exec == yes) then
	    RM $file1
	endif
	if ($exec == yes) then
	    ln -s $file2 $file1
	endif
    endif
else
    if ($exec == yes) then
	ln -s $file2 $file1
    endif
endif
if (("$err_seen" == 0 && -e $file1) || $exec == "no") then
    DO_OK
else
    DO_FAIL
    set err_count = `expr $err_count + 1`
endif



#=============================================================================
# Common code for XGTERM/XIMTOOL installation.
#=============================================================================

NEWLINE
BOLD_ON
echo "                    Creating Graphics Device Files"
echo "                    ------------------------------"
BOLD_OFF

if ($do_pipes == 0 || $has_pipes == 0) then
    goto _no_pipes
endif

# Make sure special device entries for the display servers exist in /dev.
echo -n "Creating /dev/imt1 fifo pipes for image display ...		"

set err_seen = 0
foreach i (/dev/imt1i /dev/imt1o)
    if (-e $i) then
	# Check the file permissions.
	if ("`$LS -l $i | grep '^.rwxrwxrwx'`" == "") then
	    if ($exec == yes) then
	        chmod 777 $i
	    endif
	endif
    else
	if ($exec == yes) then
	    set mkfifo_found = 0
	    foreach d ($path)
	        if (-e $d/mkfifo) then
	            set mkfifo_found = 1
	            break
	        endif
	    end
	    if ($mkfifo_found == 1) then
		mkfifo $i
	    else
		mknod $i p
	    endif
	    chmod 777 $i
	endif
    endif

    if (! (-e $i) && $exec == "yes") then
        if ("$err_seen" == 0) then
            DO_FAIL
        endif
        MSG  "Could not create fifo $i"
	set err_seen = 1
        set err_count = `expr $err_count + 1`
    endif
end
if ("$err_seen" == 0) then
    DO_OK
endif


# The old /dev/imt1 entry is now just a link to /dev/imt1o.
echo -n "Creating /dev/imt fifo pipes link ...				"
set file = /dev/imt1

set err_seen = 0
if (-e $file) then
    if ("`$LS -l $file | grep imt1o`" == "") then
	if ($exec == yes) then
	    RM $file
	    ln -s /dev/imt1o $file
	endif
    endif
else
    if ($exec == yes) then
	ln -s /dev/imt1o $file
    endif
endif
if (("$err_seen" == 0 && -e $file) || $exec == "no") then
    DO_OK
else
    DO_FAIL
    set err_count = `expr $err_count + 1`
endif

_no_pipes:

if ($mach == "cygwin") then
 echo -n "Creating special graphcap file ...				"
 cp $iraf/dev/graphcap.inet $iraf/dev/graphcap
 DO_OK
endif

#=============================================================================
# Install the default IMTOOLRC frame buffer configuration file.  The path
# /usr/local/lib path hardwired in to imtool and cannot easily be changed, but
# if installation of the default imtoolrc in this directory is not possible,
# the file can be installed in each imtool user's login directory as .imtoolrc,
# or the environment variable IMTOOLRC can be defined in each imtool user's
# .login or .cshrc to define the path to the file.
#=============================================================================

# Verify imtoolrc link.
echo -n "Checking /usr/local/lib directory ...				"
if (! -e /usr/local/lib) then
    if ($exec == yes) then
	if (! -e /usr/local) then
	    mkdir /usr/local
	endif
	mkdir /usr/local/lib
	if (-d /usr/local/lib) then
	    DO_OK
	else
	    DO_FAIL
    	    set err_count = `expr $err_count + 1`
	endif
    else
	DO_OK
    endif
else
    DO_OK
endif


# Verify or set the IMTOOLRC link.
if ($exec == yes) then
    cd /usr/local/lib
endif
set file1 = imtoolrc
set file2 = $iraf/dev/imtoolrc

echo -n "Creating /usr/local/lib/imtoolrc link ...			"

set err_seen = 0
if (-e $file1) then
    if ("`$LS $file1`" == "$file1") then
        if ("`$LS -l $file1 | grep $file2`" == "") then
	    if ($exec == yes) then
	        RM $file1
	        ln -s $file2 $file1
	    endif
        endif
    endif
else
    if ($exec == yes) then
	ln -s $file2 $file1
    endif
endif
if (("$err_seen" == 0 && -e $file1) || $exec == "no") then
    DO_OK
else
    DO_FAIL
    set err_count = `expr $err_count + 1`
endif


#=============================================================================
# Make sure there are entries in the the termcap and graphcap files for xgterm
# and imtool.  This is almost a no-op however we check in case there's an
# ancient local version which may have been replaced repeatedly rather than
# diff/merged over time.
#=============================================================================

echo -n "Checking if termcap file contains an XGterm entry ...		"
set temp = `grep -l xgterm $iraf/dev/termcap | grep -v '^#'`
if ("$temp" == "") then
    DO_FAIL
    set err_count = `expr $err_count + 1`
else
    DO_OK
endif

echo -n "Checking graphcap file for XGterm/imtool entries ...		"
set gcok = yes
foreach i (xgterm imtool)
    set temp = `grep -l $i $iraf/dev/graphcap | grep -v '^#'`
    if ("$temp" == "" && "$gcok" == "yes") then
        DO_FAIL
        set err_count = `expr $err_count + 1`
        set gcok = no
    endif
end
if ($gcok == yes) then
    DO_OK
endif

if ("$err_count" > 0) then
    goto fini_
endif



##############################################################################
#
#  Step 3: POST-INSTALL CONFIGURATION
#
#  Do some of the post-installation config required of all systems.
#  This involves:
#	
#	1) Creating a dev$tapecap for this system
#	2) Adding machine to dev$hosts file
#	3) Check for display servers
#	4) Check for graphics terminals 
#	5) Delete unused HSI binaries
#	6) Strip system sources
#
##############################################################################

# Begin configuration.
# --------------------

set err_seen = 0

NEWLINE ; NEWLINE
BOLD_ON
echo "========================================================================"
echo "=====================  Post-Install Configuration  ====================="
echo "========================================================================"
BOLD_OFF
NEWLINE


echo "    The system should be fully functional at this point however some"
echo "post-install configuration may be required to make use of all the"
echo "features such as networking or tape access.  Additional software such"
echo "as external packages or display servers will need to be installed"
echo "separately.  Some minimal configuration can be accomplished now but"
echo -n "you should consult the "
BOLD_ON
echo -n "IRAF Site Manager's Guide"
BOLD_OFF
echo " for a more complete"
echo "discussion of IRAF system management, configuration of printers, etc."

NEWLINE
post_install_:
PROMPT "Proceed to post-install configuration stage? "
set ans = "$<"
if ("$ans" == "" || "$ans" == "y") then
 NEWLINE
 BOLD_ON
 echo "------------------------------------------------------------------------"
 BOLD_OFF
else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
  NEWLINE
      MSG  "    If you continue, some minimal configuration of the IRAF net-"
      MSG  "working and tapecap file will be performed.  Additional checks"
      MSG  "on the availability of display servers and graphics terminals will"
      MSG  "also be done."
  NEWLINE
  goto post_install_
else if ("$ans" == "no" || "$ans" == "n") then
  NEWLINE; NEWLINE
  goto fini_
else if ("$ans" == "q" || "$ans" == "quit") then
  exit 0
else
  echo "Huh?"
  goto post_install_
endif


#==============================================================================
#  Set up the dev$hosts file to enable iraf networking on this system.
#==============================================================================


# Get the networking values.
set hname       = "`hostname`"
set lhost_abbr  = `hostname | awk '{printf ("%16.16s\n", $1 ) }'`
set ihosts      = $iraf/dev/hosts
set tmp_host    = /tmp/_host$$

    
if (`echo $hname | grep "\."` != "") then
    # When using FQDN lnode may not be set....
    set is_fqdn = yes
    set domain  = "`hostname | sed -e 's/^[a-zA-Z0-9_\-]*\.//g'`"
    set lhost = "`hostname | sed -e 's/\.[a-zA-Z0-9]*//g'`"
else
    set is_fqdn = no
    set domain  = "<unknown>"
    set lhost = $hname
endif
set nnode = "`hostname`"

# Compute the recommended hosts entry.
set irafks = $iraf/bin.$mach/irafks.e
set bang = '\\!'
if ("$is_fqdn" == "no") then
    set rec = `echo $lhost "	: " ${hname}${bang}${irafks}`
else
    set rec = `echo $lhost $lhost_abbr " : " ${nnode}${bang}${irafks}`
endif



NEWLINE
BOLD_ON
echo "                        IRAF Networking Config"
echo "                        ----------------------"
BOLD_OFF
NEWLINE

echo "    IRAF Networking can be used to access a remote image, tape device,"
echo "display server, or other network service. It's configuration is not"
echo "a requirement for normal IRAF operations and it can be updated at any"
echo 'time by editing the IRAF dev$hosts file with new entries.'
NEWLINE
echo "    In this stage we will add an entry for the current platform to the"
echo "hosts file.  In a local network installation this script should be run"
echo "on each system to add a networking entry as well as to install other"
echo "system files needed by IRAF."

net_conf_:
NEWLINE
PROMPT "Configure IRAF Networking on this machine? "
set ans = "$<"
if ("$ans" == "" || "$ans" == "y") then

    # Check to see if the current machine is already in the hosts file.
    set overwrite = 0
    set have_entry = 0
    grep $lhost $ihosts >& /dev/null
    if ($status == 0) then
	# Compare the recommended entry with the one already in the file.
	echo $rec 		>& /tmp/_net_rec.$$
	grep $lhost $ihosts 	>& /tmp/_net_exists.$$
	diff -bitw /tmp/_net_rec.$$ /tmp/_net_exists.$$ >& /dev/null
	if ($status == 0) then
	    RM  /tmp/_net_*.$$
    	    set have_entry = 1
	    NEWLINE
	    echo "Host '$lhost' exists with recommended entry in $ihosts."
	    NEWLINE
	    goto net_proc_

	else
	    RM  /tmp/_net_*.$$
	    NEWLINE
	    echo "Host '$lhost' exists with entry in $ihosts other than what"
	    echo "would be computed by this script:"
	    NEWLINE
    	    echo '  Recommended dev$hosts file entry used for this machine: '
    	    NEWLINE
            if ("$is_fqdn" == "no") then
                echo "    "$lhost "       : "$hname"\!"$irafks
            else
                echo "    "$lhost $lhost_abbr " : " $nnode"\!"$irafks
            endif
    	    NEWLINE
    	    echo '  Existing entry found in the dev$hosts file: '
    	    NEWLINE
	    echo -n "    "; grep $lhost $ihosts
	    NEWLINE

net_overwrite_:
	    NEWLINE
	    PROMPT_N "Overwrite the existing entry with recommended one?"
	    set ans = "$<"
    	    if ("$ans" == "" || "$ans" == "no" || "$ans" == "n") then
		set overwrite = 0
	        goto net_check_
    	    else if ("$ans" == "y" || "$ans" == "yes") then
		set overwrite = 1
		# fall through
    	    else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
		NEWLINE
		    MSG "Answering 'yes' will replace the entry in the file"
		    MSG "with the one created by this script, answering 'no'"
		    MSG "will leave the current entry (which may be correct)"
		    MSG "intact."
		NEWLINE
		goto net_overwrite_
            else if ("$ans" == "q" || "$ans" == "quit") then
	    	exit 0
            else 
	        echo "Huh? "
	        goto net_overwrite_
            endif

	endif

    else
	# This host isn't currently in the file.
net_rec_:
        NEWLINE
        echo 'Recommended dev$hosts file entry used for this machine: '
        NEWLINE
        if ("$is_fqdn" == "no") then
            echo "    "$lhost "       : "$hname"\!"$irafks
        else
            echo "    "$lhost $lhost_abbr " : " $nnode"\!"$irafks
        endif
        NEWLINE

net_proc_:
        PROMPT "Proceed with this entry? "
        set ans = "$<"
        if ("$ans" == "" || "$ans" == "y") then
	    ;
        else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
          NEWLINE
              MSG  'If you continue the recommended entry will be added to the'
	      MSG  "IRAF dev$hosts file as is, otherwise you will be asked if"
              MSG  "you wish to edit the file manually."
          NEWLINE
          goto net_proc_

        else if ("$ans" == "q" || "$ans" == "quit") then
	    exit 0
        else if ("$ans" == "no" || "$ans" == "n") then
net_edit_:
          PROMPT 'Do you wish to edit the dev$hosts file manually? '
          set ans = "$<"

          if ("$ans" == "" || "$ans" == "y") then
	      if ($?EDITOR) then
	          $EDITOR $ihosts
	      else
	          vi $ihosts
	      endif
	      goto net_check_
          else if ("$ans" == "no" || "$ans" == "n") then
	      MSG "Skipping network configuration."
	      goto net_check_
          else if ("$ans" == "q" || "$ans" == "quit") then
	      exit 0
          else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
	      NEWLINE
	      MSG  'If you say yes you will be allowed to edit the dev$hosts'
	      MSG  "file manually.  You should use the existing entries as a"
	      MSG  "template.  Be sure the host name and iraf path are correct"
	      MSG  "for this machine."
	      NEWLINE
	      goto net_edit_
          else 
	      echo "Huh? "
	      goto net_edit_
          endif
        else
          echo "Huh?"
          goto net_proc_
        endif

    endif

    # If we have an entry already in the file skip ahead to the check.
    if ($have_entry == 1) then
	goto net_check_
    endif

    # When exported to other sites the default dev$hosts file is the one
    # configured for the NOAO network.  Check to see whether we really *are*
    # running at NOAO so we don't wipe this out assuming it's a fresh install
    # and we can treat the file as an existing installation (i.e. append to
    # the hosts file).
    set is_noao = 0
    ifconfig -a |& grep 140.252 >& /dev/null
    if ($status == 0) then
	set is_noao = 1
    endif

    grep kpno.noao.edu $ihosts >& /dev/null
    if ($status == 0 && $is_noao == 0) then
	# If we're here then the hosts file is the NOAO default, but we are
	# not running on an NOAO network machine.  So, we want to initialize
	# the hosts file by getting rid of the NOAO hosts, adding a standard
	# "header" and adding the current platform.	

	NEWLINE
	echo 'Creating backup of default dev$hosts file...'
	if ($exec == yes) then
	    cp $iraf/dev/hosts $iraf/dev/hosts.ORIG
	endif

	echo 'Initializing dev$hosts file ...'


	echo "# HOSTS -- IRAF local network host table."	>  $tmp_host
	echo ""							>> $tmp_host
	echo -n "# Logical nodes (lpnode = line printer"	>> $tmp_host
	echo "output, plnode = plotter output)."		>> $tmp_host
	echo "#lpnode                 : @ursa"			>> $tmp_host
	echo "#plnode                 : @ursa"			>> $tmp_host
	echo ""							>> $tmp_host
	echo "# Host table."					>> $tmp_host
	echo ""							>> $tmp_host

    	if ("$is_fqdn" == "no") then
      	  echo $lhost $hname $iraf/bin.$mach/irafks.e | \
	    awk '{printf("%-16s\t: %s\!%s\n",$1,$2,$3)}'	>> $tmp_host
    	else
      	  echo $lhost $lhost_abbr $hname $iraf/bin.$mach/irafks.e | \
	    awk '{printf("%-16s %s\t: %s\!%s\n",$1,$2,$3,$4)}'	>> $tmp_host
    	endif

	# Copy the header to the file
	if ($exec == yes) then
	    cp $tmp_host $ihosts
	else 
	    cat $tmp_host
	endif

	RM  $tmp_host
        echo "Host '$hname' has been added to the network configuration file..."

    else
	# If we get to this point we're appending a hosts file that is
	# correct for the current network, NOAO or not.  


	# Pull off the file "header" and "data" to separate segments.
	head -7 $ihosts				>& /tmp/_hhdr$$
	if ($overwrite == 1) then
	    more +8 $ihosts | grep -v $lhost	>& /tmp/_hdat$$
	else
	    more +8 $ihosts			>& /tmp/_hdat$$
	endif

    	if ("$is_fqdn" == "no") then
      	  echo $lhost $hname $iraf/bin.$mach/irafks.e | \
	    awk '{printf("%-16s\t: %s\!%s\n",$1,$2,$3)}'	>> /tmp/_hdat$$
    	else
      	  echo $lhost $lhost_abbr $hname $iraf/bin.$mach/irafks.e | \
	    awk '{printf("%-16s %s\t: %s\!%s\n",$1,$2,$3,$4)}'	>> /tmp/_hdat$$
    	endif

net_sort_:
	# See if we want to sort the file.
	PROMPT "Do you want to sort the hosts file by node name? "
	set ans = "$<"
    	if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
	    sort /tmp/_hdat$$ >& /tmp/_hsdat$$
    	else if ("$ans" == "no" || "$ans" == "n") then
	    cp /tmp/_hdat$$ /tmp/_hsdat$$
    	else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
	    NEWLINE
	        MSG "Answering 'yes' will sort the hosts file."
	    NEWLINE
	    goto net_sort_
    	else if ("$ans" == "quit" || "$ans" == "q") then
	    exit 0
        else 
	    echo "Huh? "
	    goto net_sort_
	endif

	# Finally, put together the hosts file.
	cat /tmp/_hhdr$$ /tmp/_hsdat$$ 	>& $tmp_host

	# Copy the header to the file
	if ($exec == yes) then
	    cp $tmp_host $ihosts
	else 
	    cat $tmp_host
	endif

	RM  $tmp_host /tmp/_hhdr$$ /tmp/_hdat$$ /tmp/_hsdat$$
    endif


    # Host should be added to the dev$hosts file, let's check to see that
    # it actually works...

    # See what NETSTATUS says about this setup.
net_check_:
    NEWLINE
    echo -n "Checking that iraf networking is properly enabled ...   "
    echo -n "	"
    set system = $iraf/bin.$mach/x_system.e
    setenv iraf $iraf/
    set errstat = 0
    if (-e $system) then
        set net = `$system netstatus | grep -i "interface disabled"`
        if ("$net" == "") then
            DO_OK
        else
            DO_FAIL ; set errstat = 1
            NEWLINE
                MSG "The NETSTATUS task claims that networking is disabled."
                MSG "Please contact http://iraf.net with questions or check"
	        MSG "the Site Manager's Guide for details on how to properly"
	        MSG "configure networking."
            NEWLINE
        endif
    else
        DO_FAIL ; set errstat = 1
        NEWLINE
            MSG  "The NETSTATUS task binary could not be executed or does not"
            MSG  "exist.  Please contact http://iraf.net with questions or"
	    MSG  "check the Site Manager's Guide for details on how to properly"
	    MSG  "configure networking."
        NEWLINE
    endif

net_restart_:
    if ($errstat == 1) then
        PROMPT "Would you like to return to networking setup? "
        set ans = "$<"
	if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
	    goto net_conf_
	else if ("$ans" == "n" || "$ans" == "no") then
	    ;  # fall through
	else if ("$ans" == "h" || "$ans" == "help" || "$ans" == "?") then
	  NEWLINE
	      MSG "Answering 'yes' will return you to the top of the networking"
	      MSG "configuration where you can make changes.  Answering 'no'"
	      MSG "will leave the current (unworking) configuration in place."
	  NEWLINE
	    goto net_restart_
	else if ("$ans" == "q" || "$ans" == "quit") then
	    exit 1
	else 
	    echo "Huh? "
	    goto net_restart_
	endif
    endif

else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
      NEWLINE
          MSG  'If you continue a recommended entry can be added to the'
	  MSG  'IRAF dev$hosts file, otherwise you will be asked if'
          MSG  "you wish to edit the file manually."
      NEWLINE
      goto net_conf_
else if ("$ans" == "q" || "$ans" == "quit") then
      exit 0
else if ("$ans" == "no" || "$ans" == "n") then
      NEWLINE
      goto end_net_
else
      echo "Huh?"
      goto net_conf_
endif

end_net_:
BOLD_ON
echo "------------------------------------------------------------------------"
BOLD_OFF

NEWLINE


#==============================================================================
#  Set up the default tapecap file for the machine.
#==============================================================================

# Skip the configuration if the system already has a default dev$tapecap
# file.  We only need to do this for e.g. PC-IRAF systems where we need
# to create a default based on the OS version.

if (! $do_tapecaps ) then
    goto skip_tape_
endif


NEWLINE
BOLD_ON
echo "                      Tapecap Device File Config"
echo "                      --------------------------"
BOLD_OFF
NEWLINE

echo '    By default IRAF will search for a dev$tapecap.<node> file (where'
echo "<node> is the system name) when looking for a tape configuration file."
echo "Platforms such as PC-IRAF and Sun/IRAF support multiple OS versions"
echo "and so the proper template file must be used.  This configuration will"
echo "allow you to setup a default tapecap for this system, it may be skipped"
echo "if this machine has no tape drive attached."
NEWLINE

tape_conf_:
PROMPT "Create a default tapecap file? "
set ans = "$<"
if ("$ans" == "" || "$ans" == "y") then

    # Figure out what the template should be.
    if ($pciraf == 1) then
	if ("$UNAME" == "linux") then
	    set tapefile = "tapecap.linux"
	else if ("$UNAME" == "sunos") then
	    set tapefile = "tapecap.solaris"
	else if ("$UNAME" == "freebsd") then
	    set tapefile = "tapecap.freebsd"
	endif
    else if ($suniraf == 1) then
        setenv OSVERSION `$uname_cmd -r | cut -c1`
	if ($OSVERSION == 5) then
	    set tapefile = "tapecap.solaris"
	else
	    set tapefile = "tapecap.sunos"
	endif
    else
	set tapefile = tapecap.$UNAME
    endif

    # Get the (possibly abbreviated) local host name.
    set lhost = `hostname | awk '{printf ("%16.16s\n", $1 ) }'`

    set file1 = $iraf/dev/tapecap.$lhost
    set file2 = $tapefile
    if (! -e $file1) then
tape_link1_:
        echo "Creating default file 'tapecap.$lhost' from $tapefile..."
        if ($exec == yes) then
	    chdir $iraf/dev
	    if (-e $file1) then
	        # Remove the link in case it exists but points to a file 
		# which doesn't.  
	        RM tapecap.$lhost >& /dev/null
	    endif
	    ln -s $tapefile tapecap.$lhost
        endif
    else
        if ("`$LS -l $file1 | grep $file2`" != "") then
            echo "Tapecap symlink 'tapecap.$lhost' exists and is ok."
        else
            echo "Tapecap symlink 'tapecap.$lhost' exists but is invalid...."
	    echo "Deleting invalid link...."
	    if ($exec == yes) then
		RM $file1 >& /dev/null
	    endif
	    goto tape_link1_
	endif
    endif


    # See if we want to make this the dev$tapecap default.
    NEWLINE ; NEWLINE
    echo '    In the event a dev$tapecap.<node> file is not found on this'
    echo 'system IRAF will fallback to use just dev$tapecap.  In cases where'
    echo 'the node name changes, this installation is shared with another'
    echo 'machine or in a local network, or any case where a tapecap.<node>'
    echo 'is not found, the dev$tapecap file will be the default tapecap used'
    echo "for all IRAF systems."
    echo ""


tape_def_:
    PROMPT 'Do you wish to create a default dev$tapecap link? '
    set ans = "$<"
    if ("$ans" == "" || "$ans" == "y") then

        set file1 = $iraf/dev/tapecap
	if (! -e $file1) then
tape_link2_:
            echo -n 'Creating default dev$tapecap link to dev$'
	    echo "$tapefile..."
	    if ($exec == yes) then
	        chdir $iraf/dev
		if (-e $file1) then
	            # Remove the link in case it exists but points to a file 
		    # which doesn't.  
		    RM  tapecap >& /dev/null
		endif
	        ln -s $tapefile tapecap
	    endif
	else
            if ("`$LS -l $file1 | grep $tapefile`" != "") then
                echo "Tapecap symlink 'tapecap' exists and is ok."
            else
                echo "Tapecap symlink 'tapecap' exists but is invalid...."
	        echo "Deleting invalid link...."
	        if ($exec == yes) then
		    RM $file1 >& /dev/null
	        endif
		goto tape_link2_
	    endif
	endif

    else if ("$ans" == "no" || "$ans" == "n") then
	;
    else if ("$ans" == "q" || "$ans" == "quit") then
	exit 0
    else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
	NEWLINE
	    MSG 'A dev$tapecap file is the fallback file used if there is no'
	    MSG "tapecap.<node> file found. If you continue a link will be made"
	    MSG "to the template file appropriate for this machine."
	NEWLINE
        goto tape_def_
    else
        echo "Huh?"
        goto tape_def_
    endif

else if ("$ans" == "q" || "$ans" == "quit") then
    exit 0
else if ("$ans" == "no" || "$ans" == "n") then
    NEWLINE
    goto end_tape_
else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
    NEWLINE
        MSG  "If you continue a default tapecap appropriate for this system"
	MSG  'will be created in the $iraf/dev directory'
    NEWLINE
    goto tape_conf_
else
    echo "Huh?"
    goto tape_conf_
endif

end_tape_:
BOLD_ON
echo "------------------------------------------------------------------------"
BOLD_OFF

NEWLINE
skip_tape_:



#==============================================================================
#  Delete the unused HSI binaries in the system to recover disk space.
#==============================================================================

if ($pciraf == 0 && $suniraf == 0) then
    goto skip_hsi_del_
endif


# Get the set of currently installed system binaries.
set archs = ""
foreach i ($iraf/bin.*)
  set dir = $i:t
  if ($dir:r == "bin") then
    set sz = `(chdir $i ; du -s | awk '{printf ("%d", $1)}')`
    if (`$LS -lL $i | wc -l` > 1) then
      if (-e "$i/cl.e" && -e "$i/x_system.e") then
        # Save the list of installed binaries, allow for changes between
        # the binary arch and HSI arch here (e.g. ssun->ssol).
        if ("$dir:e" == "ssun") then
            set archs = `echo $archs ssol`
        else
            set archs = `echo $archs $dir:e`
        endif
      endif
    endif
  endif
end



# Check for HSI bin directories.
set  delete_bin = ""
set  empty_bin  = ""
foreach i ($iraf_p/iraf/unix/bin.*)
  set dir = $i:t
  if ($dir:r == "bin") then
    set sz = `(chdir $i ; du -s | awk '{printf ("%d", $1)}')`
    if (`$LS -lL $i | wc -l` > 1) then
      set d = `$LS $LSDF $i/* | head -2 | tail -1`
      if ("`echo $archs | grep $dir:e`" == "") then
        set delete_bin = `echo $delete_bin $dir`
      endif
    else
        if ($sz > 8 && "`echo $archs | grep $dir:e`" == "") then
            set delete_bin = `echo $delete_bin $dir`
        endif
    endif
  else
    continue
  endif
end

# If there were no unused bin directories found then skip this section.
if ("$delete_bin" == "") then
    goto end_hsi_del_
endif



NEWLINE
BOLD_ON
echo "                     Delete Unneeded HSI Binaries"
echo "                     ----------------------------"
BOLD_OFF
NEWLINE

echo '    The following bin directories in the iraf$unix directories were'
echo "found to be unused on this machine:"
NEWLINE
set tot = 0
foreach i ($delete_bin)
    set p = `echo $iraf/unix/$i | sed -e 's://:/:g'`
    set sz = `du -s $p`
    set tot = `expr $tot + $sz[1]`
    echo $sz[1] $p | awk '{printf ("\t    (%5d Kb)\t%s\n", $1, $2)}'
end
set tot = `expr $tot / 1000`
NEWLINE
echo "The contents of these directories may be safely deleted to reclaim"
echo "about $tot Mb of disk space without affecting the IRAF runtime system."
NEWLINE


hsi_del_:
PROMPT "Do you wish to delete these unused HSI binaries? "
set ans = "$<"
if ("$ans" == "" || "$ans" == "y") then
    NEWLINE
    chdir $iraf/unix
    foreach i ($delete_bin)
        echo -n "Delete HSI binaries in $i ...				"
        if ($exec == yes) then
	    if (-e $i) then
	        rm -rf $i/*
		DO_OK
	    else
		DO_FAIL
		NEWLINE
		    MSG  "Hmm, I can't find '$i' in '$cwd', skipping..."
		NEWLINE
	    endif
	else
	    # No-op okay
	    DO_OK
        endif
    end
else if ("$ans" == "q" || "$ans" == "quit") then
    exit 0
else if ("$ans" == "no" || "$ans" == "n") then
    NEWLINE
    goto end_hsi_del_
else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
    NEWLINE
        MSG  "This system contains binaries needed for multi-architecture"
        MSG  "support, however you do not appear to need all of the binaries"
        MSG  "supplied.  Removing uneeded binaries can recover some disk"
        MSG  "space that could be used for data."
    NEWLINE
    goto hsi_del_
else
    echo "Huh?"
    goto hsi_del_
endif

end_hsi_del_:
BOLD_ON
echo "------------------------------------------------------------------------"
BOLD_OFF

NEWLINE
skip_hsi_del_:


#==============================================================================
#  Strip the system sources.
#==============================================================================

NEWLINE
BOLD_ON
echo "                       Strip IRAF System Sources"
echo "                       -------------------------"
BOLD_OFF
NEWLINE

echo "    Source code for all IRAF tasks and interfaces is included with this"
echo "installation, but is strictly only required if you plan to develop this"
echo "code.  The sources may be deleted from the system without affecting the"
echo "runtime environment (including help pages, compilation of external pack-"
echo "ages or local task development) allowing you to reclaim 50-60Mb of disk"
echo "space for the system.  Stripping sources is recommended for systems very"
echo "short on space, leaving it on the system will allow IRAF site support to"
echo "send code fixes and compilation instructions as needed to fix problems"
echo "which have no other workaround."
NEWLINE

strip_:
PROMPT_N "Do you wish to strip the system of sources? "
set ans = "$<"
if ("$ans" == "y" || "$ans" == "yes") then
    NEWLINE
    echo -n "Stripping core system sources ...				"
    if ($exec == yes) then
	cd $iraf
	$lbin/mkpkg strip
    endif
    DO_OK
    echo -n "Stripping NOAO package sources ...				"
    if ($exec == yes) then
	cd $iraf/noao
	$lbin/mkpkg -p noao strip
    endif
    DO_OK
else if ("$ans" == "q" || "$ans" == "quit") then
    exit 0
else if ("$ans" == "" || "$ans" == "no" || "$ans" == "n") then
    NEWLINE
    goto end_strip_
else if ("$ans" == "help" || "$ans" == "h" || "$ans" == "?") then
    NEWLINE
        MSG  "Stripping system sources can recover considerable amounts of"
        MSG  "disk space while leaving a full runtime system.  Sources should"
        MSG  "be stripp if there is insufficient room and you do not plan to"
        MSG  "do any development of the Core system or NOAO package."
    NEWLINE
    goto strip_
else
    echo "Huh?"
    goto strip_
endif

#BOLD_ON
#echo "------------------------------------------------------------------------"
#BOLD_OFF

end_strip_:
NEWLINE



#==============================================================================
#  Post-Install Verification
#==============================================================================

NEWLINE
BOLD_ON
echo "========================================================================"
echo "=====================  Post-Install Verification  ======================"
echo "========================================================================"
BOLD_OFF
NEWLINE


#==============================================================================
#  Check for available display servers on this system.
#==============================================================================

NEWLINE
BOLD_ON
echo "                     Display Server Availability"
echo "                     ---------------------------"
BOLD_OFF
NEWLINE

set  DISPLAY_SERVERS    = "ximtool ximtool-alt saoimage ds9 saotng"

echo "Display Servers Found on This Machine:"
NEWLINE

set found = 0
foreach i ($DISPLAY_SERVERS)
    set p = `which $i |& grep -i "^\/"`
    if ($status == 0) then
        set d = `$LS $LSDF $p | head -2 | tail -1`
        echo $d | awk '{printf ("  ( Date: %3s %2s %-5s )    ", $7, $8, $9) }'
        echo "    "`which $i`
        set found = 1
    endif
end
if ($found == 0) then
    BOLD_ON
    echo "          No Display Servers Found"
    BOLD_OFF
    NEWLINE
	MSG  "No display servers were found on this machine or in the user"
	MSG  "path.  A display server such as XImtool/SAOimage/SAOtng/DS9"
	MSG  "is required to be running on the local machine before an iraf"
	MSG  "display command (e.g. DISPLAY/TVMARK/IMEXAMINE) will work."
	MSG  ""
	MSG  "Remote displays (i.e. the server on one machine and IRAF on"
	MSG  "another) require either iraf networking be enabled or the X"
	MSG  "'DISPLAY' variable be set so the server appears on the remote"
	MSG  "machine."
	MSG  ""
	MSG  "    XImtool can be downloaded as part of X11IRAF from:"
	MSG  ""
	MSG  "        http://iraf.net/ftp/iraf/x11iraf"
	MSG  ""
	MSG  "or it's mirror sites."
	MSG  "    For information on DS9 please see"
	MSG  ""
	MSG  "         http://hea-www.harvard.edu/RD/ds9/"
	MSG  ""
	MSG  "Please contact http://iraf.net with questions."
endif
NEWLINE

BOLD_ON
echo "------------------------------------------------------------------------"
BOLD_OFF

end_servers_:



#==============================================================================
#  Check for available graphics terminals on this system.
#==============================================================================

NEWLINE
BOLD_ON
echo "                     Graphics Terminal Availability"
echo "                     ------------------------------"
BOLD_OFF
NEWLINE

set  GRAPHICS_TERMS     = "xgterm xterm"

echo "Graphics Terminals Found on This Machine:"
NEWLINE

set found = 0
foreach i ($GRAPHICS_TERMS)
    set p = `which $i |& grep "^\/"`
    if ($status == 0) then
        set d = `$LS $LSDF $p | head -2 | tail -1`
        echo $d | awk '{printf ("  ( Date: %3s %2s %-5s )    ", $7, $8, $9) }'
        echo "    "`which $i`
        set found = 1
    endif
end
if ($found == 0) then
    BOLD_ON
    echo "          No Suitable Graphics Terminals Found"
    BOLD_OFF
    NEWLINE
        MSG  "No 'xterm' or 'xgterm' binary was found on this systen or"
        MSG  "in the user path.  IRAF graphics require some form of "
        MSG  "graphics-enabled terminal window to be running or else"
        MSG  "garbarge characters will appear on the screen.  Windows"
        MSG  "such as 'cmdtool', 'rxvt', 'aixterm', 'hpterm', 'decterm'"
        MSG  "do not support graphics and should not be used for IRAF."
        MSG  ""
        MSG  "The default terminal type is set in the login.cl when"
        MSG  "a user runs MKIRAF, this is the type of window they should"
        MSG  "be running when starting IRAF.  Users can use the 'show"
        MSG  "terminal' command to see the current setting, or 'stty"
        MSG  "xterm' or 'stty xgterm' command (or rerun MKIRAF and reset"
        MSG  "the default terminal type) to change the default iraf terminal."
        MSG  ""
        MSG  "XGterm can be downloaded as part of X11IRAF from"
        MSG  ""
	MSG  "        http://iraf.net/ftp/iraf/x11iraf"
        MSG  ""
        MSG  "or it's mirror sites.  Xterm will normally be a part of the"
 	MSG  "system, if not found please check your path."
        MSG  ""
endif
NEWLINE

#BOLD_ON
#echo "------------------------------------------------------------------------"
#BOLD_OFF

end_terms_:



#=============================================================================
#  Finish up and set the exit status.
#=============================================================================
fini_:

NEWLINE ; NEWLINE
if ("$err_count" > 0) then
 BOLD_ON
 echo "========================================================================"
 echo "=================  Installation Completed With Errors  ================="
 echo "========================================================================"
 BOLD_OFF
 NEWLINE
 exit 1
else
 BOLD_ON
 echo "========================================================================"
 echo -n "Congratulations!  "
 BOLD_OFF
 echo "IRAF has been successfully installed on this system."
 BOLD_ON
 echo "========================================================================"
 BOLD_OFF
 NEWLINE
 echo "    To begin using the system simply log in as any user and from the"
 echo "directory you wish to use as your iraf login directory type:"
 echo ""
 echo -n '	    % ';
 BOLD_ON; echo -n 'mkiraf'; BOLD_OFF
 echo '	    # create a login.cl file'
 echo -n '	    % ';
 BOLD_ON; echo -n 'cl'; BOLD_OFF
 echo '	    # start IRAF'
 echo ""
 echo "The 'iraf' user is already configured with a login.cl file so a simple"
 echo "'cl' command is sufficient to start the system."
 NEWLINE
 echo "Additional information can be found at the IRAF.NET web site:"
 NEWLINE
 BOLD_ON ; echo "		    http://iraf.net" ; BOLD_OFF
 NEWLINE
 echo "Please contact http://iraf.net with any questions or problems."
 NEWLINE
 NEWLINE

 BOLD_ON
 echo "========================================================================"
 echo "================  Installation Completed With No Errors  ==============="
 echo "========================================================================"
 BOLD_OFF
 NEWLINE
 exit 0
endif

cleanup_:
  exit 0



# Print usage information.  We will not get here unless the "-help" flag
# was issued.

Usage:
    echo "Usage:    install [-n] [-h] [-hl] [-f] [-r rootdir] [-i imdir]"
    echo "              [-b localbindir] [-R oldroot] [-I oldimdir]"
    echo "              [-u username ] [-l locallibdir]  [-m mach ]"
    echo "              [-noedit]"
    echo ""
    echo "    where -n          # no execute"
    echo "          -h          # print this help summary"
    echo "          +hl         # enable highlighted text"
    echo "          -hl         # disable highlighted text"
    echo "          -f          # create fifo pipes (if supported)"
    echo "          -r          # set iraf root directory"
    echo "          -c          # set cache directory"
    echo "          -i          # set imdir directory"
    echo "          -b          # set local bin directory"
    echo "          -l          # set local lib directory"
    echo "          -R          # set old iraf root directory"
    echo "          -I          # set old imdir directory"
    echo "          -u          # set username to own files"
    echo "          -m          # set machine type (ssun, linux, etc)"
    echo "          -noedit     # install but don't edit pathnames"
    exit 0
