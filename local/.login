#	$Id: dot.login,v 1.7.2.1 1997/02/23 20:57:42 joerg Exp $
#
# .login - csh login script, read by login shell, 
#	   after `.cshrc' at login.
#
# see also csh(1), environ(7).
#

source ~iraf/.cshrc
set notify

#set autologout = 1440

if ($TERM != "linux" && $TERM != "cons25") then
    stty erase '^?'
endif

setenv EDITOR vi
setenv BLOCKSIZE K
setenv PAGER "less -C -e -M"
setenv IPAGER "less -C -M +G"
setenv MANPATH "/usr/man:/usr/X11R6/man:/usr/local/man"
setenv MOZILLA_HOME /usr/local/bin/netscape
setenv NNTPSERVER noao.edu
setenv TAPE /dev/nrsa0
setenv SHELL /bin/csh

# Uncomment for IRAF admin/prog definitions.
if (-f /etc/redhat-release) then
    setenv IRAFARCH redhat
else
    setenv IRAFARCH `uname -s | tr '[A-Z]' '[a-z]'`
endif
setenv iraf /iraf/iraf/
set file = $iraf/unix/hlib/irafuser.csh
if (-e $file) then
    source $file
endif
unset file

set cdpath  =\
($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot /u1/x11apps/src/x11iraf /u1/x11apps/src /u3/x11iraf /local/src ~)

unalias ls cd pwd rm

alias cls	'clear;ls'
alias clw	'clear;w'
alias pg	'less -Cqme'
alias his	'history 33'
alias win	'startx -- -bpp 24'
alias win8	'startx -- -bpp 8'
alias rsync	'rsync -avz'
alias sp	'clear; tail -33f spool'
