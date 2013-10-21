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
setenv TAPE 	     /dev/nrsa0
setenv SHELL 	     /bin/csh

# Uncomment for IRAF admin/prog definitions.
#setenv IRAFARCH	    `/iraf/iraf/unix/hlib/irafarch.csh -actual`

# Setup the iraf environment.
setenv iraf /iraf/build/iraf/

foreach f ($iraf/unix/hlib/irafuser.csh ~/.alias)
  if (-e $f) then
    source $f
  endif
  unset f
end

set    prompt  = "iraf> "

# Pick up C-shell definitions.
#source ~iraf/.cshrc

set cdpath  =\
($iraf $iraf/pkg $iraf/noao $iraf/sys $iraf/unix $iraf/unix/boot /u1/x11apps/src/x11iraf /u1/x11apps/src /u3/x11iraf /local/src ~)

unalias ls cd pwd rm

alias c		'clear'
alias cls	'clear;ls'
alias ls	'ls -FCs'
alias clw	'clear;w'
alias pg	'less -Cqme'
alias m		'less -Cqme'
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


# Setup for NVOSS
#setenv NVOSS_HOME	/usr/local/nvoss/
#setenv JAVA_HOME	/Library/java/Home//
#source $NVOSS_HOME/bin/setup.csh
