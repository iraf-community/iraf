#	$Id: dot.login,v 1.7.2.1 1997/02/23 20:57:42 joerg Exp $
#
# .login - csh login script, read by login shell, 
#	   after `.cshrc' at login.
#
# see also csh(1), environ(7).
#

set notify

#set autologout = 1440

if ($TERM != "linux" && $TERM != "cons25") then
    stty erase '^?'
endif
if ($TERM == "xterm") then
    setenv TERM xterm-color
endif


setenv EDITOR 	     vi
setenv BLOCKSIZE     K
setenv PAGER 	     "less -C -e -M"
setenv IPAGER 	     "less -C -M +G"
setenv MANPATH 	     "/usr/man:/usr/X11R6/man:/usr/local/man:/usr/share/man"
setenv MOZILLA_HOME  /usr/local/bin/netscape
#setenv NNTPSERVER    noao.edu
setenv TAPE 	     /dev/nrsa0
setenv SHELL 	     /bin/csh

# Uncomment for IRAF admin/prog definitions.
if (-f /etc/redhat-release) then
    if ("`uname -m`" == "ppc") then
	setenv IRAFARCH linuxppc
    else
	setenv IRAFARCH redhat
    endif
else if (-f /etc/SuSE-release) then
    if ("`uname -m`" == "ppc") then
	setenv IRAFARCH linuxppc
    else
	setenv IRAFARCH suse
    endif
else if (-f /etc/yellowdog-release || "`uname -m`" == "ppc") then
    setenv IRAFARCH linuxppc
else
    setenv IRAFARCH `uname -s | tr '[A-Z]' '[a-z]'`
    if ($IRAFARCH == "darwin") then
        setenv IRAFARCH macosx
    endif
endif


# Setup the iraf environment.
setenv iraf /iraf/iraf/
set file = $iraf/unix/hlib/irafuser.csh
if (-e $file) then
    source $file
endif
unset file


# Pick up C-shell definitions.
#source ~iraf/.cshrc


set cdpath  =\
($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot /u1/x11apps/src/x11iraf /u1/x11apps/src /u3/x11iraf /local/src ~)

unalias ls cd pwd rm

alias cls	'clear;ls'
alias clw	'clear;w'
alias pg	'less -Cqme'
alias his	'history 33'
if (-e /etc/X11/XF86Config-4) then
    alias win	'startx -- -depth 24'
    alias win8	'startx -- -depth 8'
else
    alias win	'startx -- -bpp 24'
    alias win8	'startx -- -bpp 8'
endif
alias rsync	'rsync -avz'
alias sp	'clear; tail -33f spool'
