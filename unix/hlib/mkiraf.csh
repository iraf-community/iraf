#! /bin/csh
# MKIRAF -- Setup the IRAF environment for a user.  Should be called from the
# directory from which the user will thereafter type "cl" to start a session.

# The following definitions are site dependent. [SITEDEP]

set	iraf	= "/local/iraf"
set	imdir	= "/tmp4/iraf"
set	ttymsg  =\
"Terminal types: gterm=ttysw+graphics,vt640=(vt100+retrographics),etc."

# ------------- (end of site dependent definitions) ------------------------
# Make an empty "uparm" (user parameter) directory.

unalias rm mkdir pwd echo mkdir sed whoami

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
