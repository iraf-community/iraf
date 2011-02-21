/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/*
** CTTY -- Terminal control.  The TTY package is an interface to the TERMCAP
** database, originally developed for Berkeley UNIX by Bill Joy.  The termcap
** entry for a particular terminal presents in a condensed form the
** characteristics of the device, e.g., the number of line and columns on the
** screen, and how to clear the screen or move the cursor.  The TTY routines
** are used to retrieve such capabilities from the database as well as to
** send the appropriate characters to a file (terminal) to perform such control
** functions.
**
** 	     tty = c_ttyodes (ttyname)
** 		   c_ttycdes (tty)
** 		   c_ttyseti (tty, parameter, value)
** 	    int = c_ttystati (tty, parameter)
** 
** 	    bool = c_ttygetb (tty, cap)
** 	     int = c_ttygeti (tty, cap)
** 	   float = c_ttygetr (tty, cap)
** 	  nchars = c_ttygets (tty, cap, &outstr, maxch)
** 		   c_ttyctrl (fd, tty, cap, afflncnt)
** 		   c_ttyputs (fd, tty, ctrlstr, afflncnt)
** 
** 		  c_ttyclear (fd, tty)
** 		c_ttyclearln (fd, tty)
** 		   c_ttygoto (fd, tty, col, line)
** 		   c_ttyinit (fd, tty)
** 		c_ttyputline (fd, tty, text, map_cc)
** 		     c_ttyso (fd, tty, onflag)
** 
** 
** Complete descriptions of TTY and termcap are given elsewhere.  Briefly, the
** device descriptor for a particular terminal is opened with ttyodes, which
** returns a IRAF pointer (C integer) to the binary TTY descriptor.  The
** terminal name may be given as "terminal", in which case ttyodes will look up
** the name of the default terminal in the environment and search the termcap
** database for the entry for the named device.
** 
** The ttyget functions are used to read the capabilities.  Capabilities are
** specified by two character mnemonics (character strings), shown as the cap
** arguments in the calling sequences above.  Control sequences may be output
** with ttyctrl or with ttyputs, depending on whether you are willing to do a
** binary search for a particular capability at run time.  The remaining high
** level functions make it easy to perform the more common control functions.
** 
** Raw mode output to a terminal device is provided by the system interface
** (the newline and tab characters are exceptions).  Raw mode input is provided
** as an fseti option in FIO.  To set raw mode on STDIN:
** 
** 	c_fseti (STDIN, F_RAW, YES);
** 
** While raw mode is in effect input characters are read as they are typed,
** few or no control characters are recognized, and no echoing is performed.
** Raw mode is cleared whenever the newline character is sent to the terminal,
** but will be reset if by the next read if F_RAW remains set.
*/


/* C_TTYODES -- Open the TTY descriptor for a particular terminal device.
** An SPP pointer to the TTY descriptor is returned as the function value.
** If the device name is given as "terminal" or "printer", the actual device
** name is taken to be the value of the environment variable of the same name.
** If the device name is the filename of a termcap format file, the entry
** for the first device in the file is loaded (this gives the user a simple
** means to supply special termcap entries).  The name of the default
** termcap file is given by the environment variable "termcap".  TTY maintains
** a cache of preloaded termcap device entries for frequently referenced
** devices.
*/
XINT
c_ttyodes (
  char	*ttyname		/* termcap name of device	*/
)
{
	XINT	tty;

	iferr (tty = (XINT) TTYODES (c_sppstr (ttyname)))
	    return ((XINT) ERR);
	else
	    return (tty);
}
