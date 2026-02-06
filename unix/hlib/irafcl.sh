#!/bin/sh
#
# CL.SH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.  This script can be used to invoke a number of CL flavors 
# depending on how it is called.  The installation will create a 'cl'
# and 'ecl' command link to this script with the intent that a different
# binary would be started for each command.  

# Determine IRAF root directory (value set in install script).
d_iraf=/iraf/iraf/
if [ -n "$iraf" -a  ! -e "$iraf" ]; then
    echo "Warning: iraf=$iraf does not exist (check .bashrc or .zshrc)"
    echo "Session will default to iraf=$d_iraf"
    unset iraf ; sleep 3
fi
if [ -z "$iraf" ]; then
    if [ -r ${HOME}/.iraf/irafroot ] ; then
	export iraf=$(cat /etc/iraf/irafroot)
    elif [ -r /etc/iraf/irafroot ] ; then
	export iraf=$(cat /etc/iraf/irafroot)
    else
	export iraf="$d_iraf"
    fi
fi

if [ "${IRAFARCH}" -a  ! -e "${iraf}bin.${IRAFARCH}" ] ; then
    echo "Warning: ${iraf}bin.${IRAFARCH} does not exist (check .bashrc or .zshrc)"
    echo "IRAFARCH will be reset"
    unset IRAFARCH arch ; sleep 3
fi
if [ "${IRAFARCH}" ] ; then
    export arch=".${IRAFARCH}"
    bin="${iraf}bin${arch}/"
else
    bin="${iraf}bin/"
fi

# Determine CL binary to run based on how we were called.
nm=${0##*/}

cl_binary="${bin}ecl.e"

iraf_version=$(grep version\\s  ${iraf}unix/hlib/zzsetenv.def | \
		   cut -d\" -f2 | cut -d\  -f-3)

cmdline=""

while getopts "h?Voxf:c:" opt; do
    case "$opt" in
    h|\?)
	echo 'IRAF Command Language Interpreter'
	echo ''
	echo 'Usage:'
	echo '    irafcl [-V|-o] [-x] [-f file] | [-c cmd]'
	echo ''
	echo 'Arguments:'
	echo '    -V        print version and exit'
	echo '    -o        force classic cl'
	echo '    -x        start in new xgterm (xterm if xgterm is not installed)'
	echo '    -f file   start with file'
	echo '    -c cmd    execute command (may be repeated) and exit'
	echo ''
	exit 0
        ;;
    V)
        echo "${iraf_version}"
	exit 0
        ;;
    o)
	cl_binary=${irafbin}cl.e
	;;
    x)
	start_x="yes"
	;;
    f)
	script=$OPTARG
	;;
    c)
	if [ -z "$cmdline" ] ; then
	    cmdline="$OPTARG"
	else
	    cmdline="${cmdline}
$OPTARG"
	fi
	;;
    esac
done

if [ "${cmdline}" ] ; then
    cmdline="${cmdline}
logout"
    script=$(mktemp -t irafcl.XXXXXX) || exit 1
    trap 'rm -f "$script"' EXIT
    cat << EOF > "$script"
$cmdline
EOF
fi

# Workaround for autoconf scripts attempting to use this command as a
# valid compiler option.  On some systems (mostly Debian) a valid CC
# command can't be found and eventually the 'cl' (lisp) compiler is
# tried.  It will always apparently have the conftest.c test file, so
# simply exit with a code to tell autoconf it won't work.
if [ "${1##*.}" = "c" ]; then
    exit 1
fi

# Determine the temporary dir, using standard variable from The Open Group
# Base Specifications. Default to FHS.
if [ -z $tmp ]; then
    if [ -n "$TMPDIR" ]; then
        export tmp=$(echo "$TMPDIR" | sed s';/$;;')/
    else
        export tmp="/tmp/"
    fi
fi

# Initialize the IRAF user dirs
for d in imdir cache uparm ; do
    if [ ! -e "${HOME}/.iraf/${d}" ] ; then
        mkdir -p "${HOME}/.iraf/${d}"
    fi
done


# Check whether we should start in an x(g)term window
if [ "${start_x}" = "yes" ] ; then
    if which xgterm > /dev/null ; then
        cl_binary="xgterm -e ${cl_binary}"
    elif which xterm > /dev/null ; then
        cl_binary="xterm -e ${cl_binary}"
    fi
fi

if [ -z "${script}" ] ; then
    exec ${cl_binary}
elif [ -z "${cmdline}" ] ; then
    exec ${cl_binary} -f "${script}"
else
    # If a command line was specified, we can't do exec because
    # we need to remove the script with the trap afterwards
    ${cl_binary} -f "${script}"
fi
