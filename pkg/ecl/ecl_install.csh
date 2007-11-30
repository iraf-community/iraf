#!/bin/csh -f
#
#  ECL_INSTALL -- Install the ECL onto an existing IRAF system or for private
#  use when you don't have write permission on the iraf tree.
#
#  Usage:     ecl_install [-n] 
#
#  If run with no arguments, ECL_INSTALL will make an informed guess and prompt
#  with this value; type <return> to accept the value, or enter a new value.
#
#  Use "ecl_install -n" to do a dry run to see what the would be done, without
#  actually modifying the host system and IRAF configuration files.
#
#============================================================================

unset   noclobber
onintr  ecl_cleanup_
unalias cd cp cmp echo ln mv rm sed set grep ls chmod chown pwd touch sort

set     path  = (/sbin /usr/sbin /bin /usr/bin /usr/ucb /etc /usr/etc $path /usr /local/bin /opt/local/bin /local/bin /home/local/bin )


#============================================================================
# Global Variables.
#============================================================================

set hilite            	= 1
set SKIP              	= `cat $0 | grep -n ^START_OF_DATA | cut -c1-3`
set SKIP		= `expr $SKIP + 1`

# Identify the installation type.
set INS_TYPE		= DTYPE
set INS_VERSION		= VERSION_DATE


#============================================================================
# Utility aliases.
#============================================================================
alias PUT      "mv -f \!*; chown $user \!$ "           # [MACHDEP]
alias BOLD_ON  "(if ($hilite) tput bold)"
alias BOLD_OFF "(if ($hilite) tput sgr0)"
alias SO_ON    "(if ($hilite) tput smso)"
alias SO_OFF   "(if ($hilite) tput rmso)"

alias DO_OK    "(echo -n '[ '; BOLD_ON; echo -n ' OK '; BOLD_OFF; echo ' ]')"
alias DO_WARN  "(echo -n '[ '; BOLD_ON; echo -n 'WARN'; BOLD_OFF; echo ' ]')"
alias DO_FAIL  "(echo -n '[ ';   SO_ON; echo -n 'FAIL';   SO_OFF; echo ' ]')"

alias MSG      "(echo -n '   ';BOLD_ON;echo -n '***  ';BOLD_OFF; echo \!*)"
alias MSGN     "(echo -n '   ';BOLD_ON;echo -n '***  ';BOLD_OFF; echo -n \!*)"
alias MSGB     "(echo -n '   ';BOLD_ON;echo -n '***  ';echo \!*; BOLD_OFF)"
alias MSGBN    "(echo -n '   ';BOLD_ON;echo -n '***  ';echo -n \!*;BOLD_OFF)"
alias ERRMSG   "(echo -n '   ';BOLD_ON;echo -n 'ERROR: ';BOLD_OFF; echo \!*)"
alias WARNING  "(echo -n '   ';BOLD_ON;echo -n 'WARNING: ';BOLD_OFF; echo \!*)"
alias NEWLINE  "(echo '')"

alias PROMPT   "(BOLD_ON; echo -n \!*; BOLD_OFF; echo -n ' (yes): ')"
alias PROMPT_N "(BOLD_ON; echo -n \!*; BOLD_OFF; echo -n ' (no): ')"


#============================================================================
# Get the current platform architecture.
#============================================================================

set UNAME=""
set UNCOMPRESS="gunzip"

if (-e /usr/bin/uname) then
    set uname_cmd = /usr/bin/uname
    set UNAME=`/usr/bin/uname | tr '[A-Z]' '[a-z]'`
else if (-e /bin/uname) then
    set uname_cmd = /bin/uname
    set UNAME=`/bin/uname | tr '[A-Z]' '[a-z]'`
else
    set UNAME = "INDEF"
endif

switch ($UNAME) 
    case sunos:
        if (`$uname_cmd -m | cut -c2-` == "86pc") then
            set arch = "sunos"
        else
            setenv OSVERSION `uname -r | cut -c1`
            if ($OSVERSION == 5) then
                set arch = "ssun"
		set UNCOMPRESS="uncompress"
            else
                set arch = "sparc"
		set UNCOMPRESS="uncompress"
            endif
        endif
	breaksw
    case linux:
        if (`$uname_cmd -m` == "ppc") then
            if (-f /etc/redhat-release) then
	        set arch = "linuxppc"
	    else
                set arch = "mklinux"
	    endif
	else
            if (-f /etc/redhat-release) then
                set arch = "redhat"
            else if (-f /etc/SuSE-release) then
                set arch = "suse"
            else
                set arch = "linux"
            endif
        endif
	breaksw
    case darwin: 
	set arch = "macosx"
	breaksw
    case freebsd: 
	set arch = "freebsd"
	breaksw
    case hp-ux:   
	set arch = "hp700"
	set UNCOMPRESS="uncompress"
	breaksw
    case irix:    
	set arch = "irix"
	set UNCOMPRESS="uncompress"
	breaksw
    case irix64:  
	set arch = "irix"
	set UNCOMPRESS="uncompress"
	breaksw
    case aix:  	  
	set arch = "rs6000"
	set UNCOMPRESS="uncompress"
	breaksw
    case osf1:    
	set arch = "alpha"
	set UNCOMPRESS="uncompress"
	breaksw
	breaksw
    default:	  
        echo "ERROR: No 'uname' command found to determine architecture."
bad_arch:
	NEWLINE
	echo -n "Enter architecture name: "
	set arch = "$<"
	if (! -e bin.$arch) then
	    echo -n "Invalid architecture, try again..."
	    goto bad_arch
	endif
	breaksw
endsw



#############################################################################
# Process command line options.
#############################################################################
set  exec	= yes
set personal 	= no

while ("$1" != "")
    switch ("$1")
    case -n:                            # no execute
        set exec = no
        breaksw
    case -p:                            # no execute
        set personal = yes
        breaksw
    case -hl:                           # disable highlighting
        set hilite = 0
        alias BOLD_ON   "(if ($hilite) tput bold)"
        alias BOLD_OFF  "(if ($hilite) tput sgr0)"
        alias SO_ON     "(if ($hilite) tput smso)"
        alias SO_OFF    "(if ($hilite) tput rmso)"
        breaksw
    case +hl:                           # enable highlighting
        set hilite = 1
        alias BOLD_ON   "(if ($hilite) tput bold)"
        alias BOLD_OFF  "(if ($hilite) tput sgr0)"
        alias SO_ON     "(if ($hilite) tput smso)"
        alias SO_OFF    "(if ($hilite) tput rmso)"
        breaksw
    case -h:                            # print help summary
        goto Usage
    default:
        echo "install: unknown argument $1"
        breaksw
    endsw

    if ("$2" == "") then
        break
    else
        shift
    endif
end

    
#############################################################################
# Print the banner message.
#############################################################################
clear
NEWLINE
BOLD_ON
echo "			   ECL V0.9 Installation"
echo "			   ====================="
echo ""
echo "		 Build Date: " $INS_VERSION
echo ""
BOLD_OFF
NEWLINE

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



#############################################################################
# Check that we're running as 'root' so we can write to system bin dirs.
#############################################################################
if (-e /usr/bin/whoami) then
    set WHOAMI = `/usr/bin/whoami`
else if (-e /usr/ucb/whoami) then
    set WHOAMI = `/usr/ucb/whoami`
else if (-e /bin/whoami) then
    set WHOAMI = `/bin/whoami`
endif

if ($WHOAMI != "root" && $exec == "yes") then
  NEWLINE ; NEWLINE
  BOLD_ON
  echo "======================================================================"
  echo -n "WARNING"
  BOLD_OFF
  echo ": This script must be run as root for changes to take effect."
  echo "         If you decide to proceed,  the 'no-op' flag will be enabled"
  echo "         by default.   No changes will be made to the system  files,"
  echo "         however you will be able to see what the script does."
  echo ""
  echo "	 If you would like to do a personal installation, use the"
  echo "	 '-p' flag."
  BOLD_ON
  echo "======================================================================"
  BOLD_OFF
  NEWLINE
no_op_proc_:
  PROMPT "Proceed with a no-op installation anyway? "
  set ans = "$<"
  if ("$ans" != "") then
        if ($ans == 'n' || $ans == 'N' || $ans == 'no' || $ans == 'NO') then
	    exit 0
      endif
  endif
  NEWLINE
        
  set exec = no
endif



#############################################################################
# Locate the current 'cl' command directory.
#############################################################################

set cldir	= ""

set p = `which cl |& grep -i "^\/"`
if ($status == 0) then
    set cldir = $p:h
endif


clbin_prompt:
    BOLD_ON ; echo -n "Local unix commands directory " ; BOLD_OFF
    echo -n "($cldir): "
    set clbin = "$<"
    if ("$clbin" == "") then
        set clbin = "$cldir"
    else if ("$clbin" == "quit" || "$clbin" == "q") then
        exit 0
    else if ("$clbin" == "help" || "$clbin" == "h" || "$clbin" == "?") then
        NEWLINE
            MSG "The local bin directory is the system directory into which the"
            MSG "iraf commands (e.g. cl, mkiraf, mkpkg, etc) will be installed"
            MSG "as symlinks to files in the iraf tree. This should be a common"
            MSG "dir such as /usr/local/bin which will likely be found in every"
            MSG "user's path."
        NEWLINE
        setenv clbin $cldir
        goto clbin_prompt
    endif

    # Create the local bin directory if it doesn't exist?
    if (! (-e $clbin)) then
        PROMPT "    Sorry, but $clbin does not exist, create it? "
        set ans = "$<"
        if ("$ans" == "" || "$ans" == "y" || "$ans" == "yes") then
            echo "  Creating directory $clbin..."
            if ($exec == yes) then
                mkdir $clbin
            endif
            if (! (-e $clbin) && $exec == yes) then
                ERRMSG  "Cannot create $clbin, please retry..."
                setenv clbin $cldir
                goto clbin_prompt
            endif
        else
            goto clbin_prompt
        endif
        NEWLINE
    endif


#############################################################################
# Locate the hlib$ directory.
#############################################################################

set hlib_dir	= ""

if ($?iraf) then
    set hlib_dir = $iraf/unix/hlib/
else if (-e /iraf/iraf/unix/hlib) then
    set hlib_dir = /iraf/iraf/unix/hlib/
else if (-e /usr/include/iraf.h) then
    set WS = '[ 	]'
    set ip = `grep "define$WS*IRAF" /usr/include/iraf.h | sed -e 's/"//g'`
    set hlib_dir = $ip[3]/unix/hlib/

else
hlib_prompt:
    BOLD_ON ; echo -n "IRAF hlib$ directory " ; BOLD_OFF
    echo -n "($hlib_dir): "
    set hlib_dir = "$<"
    if ("$hlib_dir" == "quit" || "$hlib_dir" == "q") then
        exit 0
    else if ("$hlib_dir"=="help" || "$hlib_dir"=="h" || "$hlib_dir"=="?") then
        NEWLINE
            MSG "The install script could not determine the hlib$ iraf logical"
            MSG "directory.  This is the $iraf/unix/hlib subdirectory."
        NEWLINE
        setenv hlib ""
        goto hlib_prompt
    endif

endif

set iraf = $hlib/../../


#############################################################################
# Dump the tarball of files from the end of this script file.
#############################################################################

set bck = $cwd				# goto a temp directory to work
mkdir /tmp/_ecl.$$
tail -n +$SKIP $0 > /tmp/_ecl.$$/_ecl	# unpack the files
chdir /tmp/_ecl.$$
uudecode _ecl
cat ecl_tar.gz | $UNCOMPRESS | tar -xf -
/bin/rm -f _ecl ecl_tar.gz		# clean up temp files created so far


if ($exec == "yes") then
  if ($WHOAMI == "root" && $exec == "yes") then
    echo "Updating cl.csh script in hlib directory ...."
	cp -p $hlib/cl.csh $hlib/cl.csh.ORIG
	mv cl.csh $hlib/

    echo "Installing 'ecl' unix command ...."
	if (! -e $cldir/ecl) then
	    ln -s $hlib/cl.csh $cldir/ecl
	endif

    echo "Installing ECL binary ...."
	chown iraf ecl.e
	chmod 755 ecl.e
	mv ecl.e $iraf/bin.$arch/

    echo "Installing ECL in IRAF source tree ...."
	if (! -e $iraf/pkg/ecl) then
	    mkdir $iraf/pkg/ecl
	endif
	cp -rp * $iraf/pkg/ecl
	chown -R iraf $iraf/pkg/ecl
  endif
else
    echo "Installing ECL binary ...."
    echo "Installing ECL in IRAF source tree ...."
    echo "Installing 'ecl' unix command ...."
    echo "Updating cl.csh script in hlib directory ...."
endif


# Go back from whence we came and 
chdir $bck				

# Clean up.
/bin/rm -rf /tmp/_ecl.$$


######################################################################
exit 0
######################################################################
START_OF_DATA

