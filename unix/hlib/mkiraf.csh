#! /bin/csh
# MKIRAF -- Setup the IRAF environment for a user.  Should be called from the
# directory from which the user will thereafter type "cl" to start a session.

# The following definitions are site dependent. [SITEDEP]

set	iraf	= "/iraf/iraf"
set	imdir	= "/d0/iraf"
set	ttymsg  =\
"Terminal types: xgterm,xterm,gterm,vt640,vt100,etc."

# ------------- (end of site dependent definitions) ------------------------

unalias rm mkdir pwd echo mkdir sed whoami pushd popd

# The following kludge is for Solaris, which doesn't have whoami.
if (! $?USER) then
    setenv USER `whoami`
endif
alias whoami "(echo $USER)"

# Protect against running mkiraf in an iraf system directory.
pushd $iraf >& /dev/null;  set irafdir = `pwd`;  popd >& /dev/null
if ("`pwd | grep $irafdir`" != "") then
    if ("`pwd | grep iraf/local`" == "") then
	echo "Error: current directory is not an iraf user login directory"
	exit 1
    endif
endif

# Make an empty "uparm" (user parameter) directory.
if (! -e uparm) then
    echo '-- creating a new uparm directory'
    mkdir uparm
else
    echo -n 'Initialize uparm? (y|n): '
    set yesno = $<
    if ($yesno == 'y' || $yesno == 'yes') then
	echo '-- initializing uparm'
	rm -rf uparm; mkdir uparm
    endif
endif

if (-e login.cl) then
    mv -f login.cl login.cl.OLD
endif


# Edit the login.cl file, setting the user's home directory, default image
# directory, and terminal.

echo $ttymsg
echo -n 'Enter terminal type: '

echo $<	| sed -e "s;.*;s+U_TERM+&+;"		>  _sed
pwd	| sed -e "s;.*;s+U_HOME+&/+;"		>> _sed
pwd	| sed -e "s;.*;s+U_UPARM+&/uparm/+;"	>> _sed
whoami	| sed -e "s;.*;s+U_IMDIR+$imdir/&/+;"	>> _sed
whoami	| sed -e "s;.*;s+U_USER+&+;"		>> _sed

sed -f _sed < $iraf/unix/hlib/login.cl > login.cl; rm _sed
whoami	| sed -e "s;.*;mkdir $imdir/& 2> /dev/null;" | sh

echo 'A new LOGIN.CL file has been created in the current directory.'
echo 'You may wish to review and edit this file to change the defaults.'
