include <error.h>
include	<gio.h>
include	<gset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"

.help debugging
.nf ___________________________________________________________________________
RV_DEBUG - Debugging facility.

	A debugging facility has been set up in the RV  package that is des-
igned to be hidden from ordinary users.  Several layers of debugging are pro-
vided, to write info on various operations of the package.  These layers are 
defined as follows:

		Level:		Action:
		------		-------
		  0		Simple procedure trace
		  1		Data cache and flag output
		  2		CCF and line fitting output
		  3		Velocity computation and correction output
		  4		FFT information

These layers are set up such that (e.g.) a level 3 debug flag will include all
information up to and including level 3 output.

.endhelp _____________________________________________________________________


# RV_DEBUG - Debugging utility commands.

procedure rv_debug (rv, cmdstr)

pointer	rv					#I RV struct pointer
char	cmdstr[SZ_LINE]				#I Command line

pointer	sp, cmd, buf
int	strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.
	switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, DEBUG_KEYWORDS)) {
	case DEBUG_DEBUG:
	    # Toggle the debugging flag
	    call cmd_dbg_debug (rv, NULL)

	case DEBUG_D_ON:
	    # Toggle the debugging flag
	    call cmd_dbg_debug (rv, YES)

	case DEBUG_D_OFF:
	    # Toggle the debugging flag
	    call cmd_dbg_debug (rv, NO)

	case DEBUG_FILE:
	    # Set the debug file name for output
	    call cmd_dbg_file (rv)

	case DEBUG_LEVEL:
	    # Set the debug level for output
	    call cmd_dbg_level (rv)

	case DEBUG_OTHER:
	    # Compare other algorithims?
	    call cmd_dbg_other (rv)

	default:
	    call rv_errmsg ("rv_debug: invalid case label.")
	}

	call sfree (sp)
end


procedure cmd_dbg_file (rv)

pointer	rv						#I RV struct pointer

pointer	sp, buf, open()
errchk 	open

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call gargstr (Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS) {
	    call realloc (DBG_FNAME(rv), SZ_FNAME, TY_CHAR)
	    call strcpy (Memc[buf+1], DEBUG_FNAME(rv), SZ_FNAME)
	    if (DBG_FD(rv) != NULL && DBG_FD(rv) != STDOUT)
		call close (DBG_FD(rv))
	    iferr (DBG_FD(rv) = open (DEBUG_FNAME(rv), APPEND, TEXT_FILE))
		call error (0, "Error opening debug file.")
	    call dbg_init (rv, DBG_FD(rv))
	    
	} else {
	    call printf ("Debugging filename = '%s'\n")
	 	if (DBG_FNAME(rv) == NULL)
	            call pargstr ("")
	  	else
	            call pargstr (DEBUG_FNAME(rv))
	}

	call sfree (sp)
end


procedure cmd_dbg_debug (rv, flag)

pointer	rv						#I RV struct pointer
int	flag						#I Debugging state

bool	bval, itob()
int	nscan(), btoi()

begin
	switch (flag) {
	case YES:
	    DBG_DEBUG(rv) = YES
	    if (DBG_FD(rv) == NULL)
		DBG_FD(rv) = STDOUT
	case NO:
	    DBG_DEBUG(rv) = NO
	default:
	    call gargb (bval)
	    if (nscan() == 2) {
	        DBG_DEBUG(rv) = btoi (bval)
	    } else {
	        call printf ("Debugging flag = %b\n")
		    call pargb (itob(DBG_DEBUG(rv)))
	    }
	}
end


procedure cmd_dbg_level (rv)

pointer	rv						#I RV struct pointer

int	nscan()
int	ival

begin
	call gargi (ival)
	if (nscan() == 2) {
	    DBG_LEVEL(rv) = ival
	} else {
	    call printf ("Debugging level = %d\n")
		call pargi (DBG_LEVEL(rv))
	}
end


procedure cmd_dbg_other (rv)

pointer	rv						#I RV struct pointer

int	nscan()
bool	bval
int	btoi()
bool	itob()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    DBG_OTHER(rv) = btoi (bval)
	} else {
	    call printf ("Debugging other = %b\n")
		call pargb (itob(DBG_OTHER(rv)))
	}
end


procedure d_printf (fd, str)

pointer	fd					# Debug file descriptor
char	str[SZ_LINE]				# Format string

begin
	if (fd == NULL)
	    return
	else if (fd == STDOUT)
	    call printf (str)
	else if (fd == STDERR)
	    call eprintf (str)
	else
	    call fprintf (fd, str)

	call flush (fd)
end


procedure d_flush (fd)

pointer	fd

begin
	if (fd != NULL)
	    call flush (fd)
end


procedure dbg_init (rv, fd)

pointer	rv, fd

pointer sp, system_id

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)

	call sysid (Memc[system_id], SZ_LINE)
	call d_printf (fd, "\n\t-->%s<--\n\n")
	    call pargstr (Memc[system_id])

	call d_printf (fd, "Task:  `fxcor'\n\n")
	call d_printf (fd, "\tObject = '%s'/%4d\t\tTemplate = '%s'/%4d\n")
	    call pargstr (IMAGE(rv));	call pargi (RV_NPTS(rv))
	    call pargstr (RIMAGE(rv));	call pargi (RV_RNPTS(rv))
	call d_printf (fd, "%12t=> '%s'%42t=> '%s'\n\n")
	    call pargstr (OBJNAME(rv));	call pargstr (TEMPNAME(rv))

	call flush (fd)
	call sfree (sp)
end


# OP_DEBUG - Read the _RVDEBUG parameters.

procedure op_debug (rv)

pointer	rv					#I RV struct pointer

pointer	db, sp, bp, ks
pointer	clopset(), open()
bool	clgpsetb(), streq()
int	clgpseti(), btoi()
errchk  clopset, open

begin
	if (DBG_FD(rv) != NULL && DBG_FD(rv) != STDOUT && DBG_FD(rv) != STDERR)
	    return

	call smark (sp)
	call salloc (bp, SZ_FNAME, TY_CHAR)
	call salloc (ks, SZ_FNAME, TY_CHAR)

	db = clopset ("rvdebug")

	DBG_DEBUG(rv) = btoi (clgpsetb(db, "debug"))
	DBG_LEVEL(rv) = clgpseti (db, "level")
	DBG_OTHER(rv) = btoi (clgpsetb(db, "other"))
	DBG_QUICK(rv) = btoi (clgpsetb(db, "quickdraw"))

	call clgpset (db, "file", Memc[bp], SZ_LINE)
	call clgpset (db, "keystroke", Memc[ks], SZ_LINE)
	DBG_KEYSTROKE(rv) = Memc[ks]
	if (!streq(Memc[bp],"") && !streq(Memc[bp]," ")) {
	    call realloc (DBG_FNAME(rv), SZ_FNAME, TY_CHAR)
	    call strcpy (Memc[bp], DEBUG_FNAME(rv), SZ_FNAME)
	    iferr (DBG_FD(rv) = open (DEBUG_FNAME(rv), APPEND, TEXT_FILE))
		call error (0, "Error opening debug file.")
	    if (DBG_DEBUG(rv) == YES)
		call dbg_init (rv, DBG_FD(rv))
	} else if (streq("STDOUT", Memc[bp]))
	    DBG_FD(rv) = STDOUT
	else
	    DBG_FD(rv) = NULL

	call clcpset (db)
	call sfree (sp)
end


procedure zz_msg(str)
char	str[SZ_LINE]

begin
	call eprintf ("%s\n")
	     call pargstr(str)
	call flush(STDERR)
end


procedure zz_pause(str)
char 	str[ARB]

real	x, y
int	wcs, key, stat
char	command[SZ_FNAME]
int	clgcur()

begin
	call eprintf("%s")
	     call pargstr(str)
	call flush (STDERR)
	stat = clgcur("cursor", x, y, wcs, key, command, SZ_FNAME)
	call eprintf("                                          \n")

	if ('x' == key || 'q' == key || 'I' == key)
	    call error (0,"Quitting")
	
	return
end


procedure zz_dump1r(fname, data, npts)
char	fname[SZ_FNAME]
real	data[ARB]
int	npts

pointer	fd
int	i, open()
errchk 	open

begin
	fd = open(fname, WRITE_ONLY, TEXT_FILE)

	do i = 1, npts {
	     call fprintf(fd, "%f %f\n")
		call pargr(real(i))
		call pargr(data[i])
	}
	call flush(fd)
	call close(fd)
	return
end


procedure zz_dump1rx (fname, data, npts, x1, xinc)
char	fname[SZ_FNAME]
real	data[ARB]
int	npts
real	x1, xinc

pointer	fd
int	i, open()
errchk 	open

begin
	fd = open(fname, WRITE_ONLY, TEXT_FILE)

	do i = 1, npts {
	     call fprintf(fd, "%f %f\n")
		call pargr(x1+(i-1)*xinc)
		call pargr(data[i])
	}
	call flush(fd)
	call close(fd)
	return
end


procedure zz_dump1i(fname, data, npts)
char	fname[SZ_FNAME]
int	data[ARB]
int	npts

pointer	fd
int	i, open()
errchk 	open

begin
	fd = open(fname, WRITE_ONLY, TEXT_FILE)

	do i = 1, npts {
	     call fprintf(fd, "%d %d\n")
		call pargi(i)
		call pargi(data[i])
	}
	call flush(fd)
	call close(fd)
	return
end


procedure zz_pvec (gp, y, npts, lo, hi, title)

pointer	gp
real	y[npts]
int	npts
real	lo, hi
char	title[SZ_LINE]

real	x1, x2, y1, y2

begin
	if (gp == NULL)
	    return

	call gclear (gp)
	call gascale (gp, y, npts, 2)
	call ggwind (gp, x1, x2, y1, y2)
	call gswind (gp, lo-(hi*.1), hi+(hi*.1), y1-(y2*.1), y2+(y2*.1))
	call glabax (gp, title, "pixel", "intensity")
	call gvline (gp, y, npts, lo, hi)

	call zz_pause("")
end


procedure zz_pvecm (gp, y, npts, lo, hi, title)

pointer	gp
real	y[npts]
int	npts
real	lo, hi
char	title[SZ_LINE]

real	x1, x2, y1, y2

begin
	if (gp == NULL)
	    return

	call gclear (gp)
	call gascale (gp, y, npts, 2)
	call ggwind (gp, x1, x2, y1, y2)
	call gswind (gp, lo-(hi*.1), hi+(hi*.1), y1-(y2*.1), y2+(y2*.1))
	call glabax (gp, title, "pixel", "intensity")
	call gvline (gp, y, npts, lo, hi)
end
