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
int	ival

begin
	call gargi (ival)
	if (nscan() == 2) {
	    DBG_OTHER(rv) = ival
	} else {
	    call printf ("Debugging other = %b\n")
		call pargi (DBG_OTHER(rv))
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
	DBG_OTHER(rv) = clgpseti(db, "other")
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


procedure zz_print_struct (rv)

pointer	rv

begin
	call zz_prstruct1 (rv)
	call zz_prstruct2 (rv)
end


procedure zz_prstruct1 (rv)

pointer	rv

begin
	call eprintf ("RV_OPIXX = %d\n") ; call pargi(RV_OPIXX(rv))
	call eprintf ("RV_OPIXY = %d\n") ; call pargi(RV_OPIXY(rv))
	call eprintf ("RV_RPIXX = %d\n") ; call pargi(RV_RPIXX(rv))
	call eprintf ("RV_RPIXY = %d\n") ; call pargi(RV_RPIXY(rv))
	call eprintf ("RV_WKPIXX = %d\n") ; call pargi(RV_WKPIXX(rv))
	call eprintf ("RV_WKPIXY = %d\n") ; call pargi(RV_WKPIXY(rv))
	call eprintf ("RV_APODIZE = %g\n") ; call pargr(RV_APODIZE(rv))
	call eprintf ("RV_AUTOWRITE = %d\n") ; call pargi(RV_AUTOWRITE(rv))
	call eprintf ("RV_AUTODRAW = %d\n") ; call pargi(RV_AUTODRAW(rv))
	call eprintf ("RV_CONTINUUM = %d\n") ; call pargi(RV_CONTINUUM(rv))
	call eprintf ("RV_FILTER = %d\n") ; call pargi(RV_FILTER(rv))
	call eprintf ("RV_INTERACTIVE = %d\n") ; call pargi(RV_INTERACTIVE(rv))
	call eprintf ("RV_PIXCORR = %d\n") ; call pargi(RV_PIXCORR(rv))
	call eprintf ("RV_INTERP = %d\n") ; call pargi(RV_INTERP(rv))
	call eprintf ("RV_BACKGROUND = %g\n") ; call pargr(RV_BACKGROUND(rv))
	call eprintf ("RV_FITDONE = %d\n") ; call pargi(RV_FITDONE(rv))
	call eprintf ("RV_FITFUNC = %d\n") ; call pargi(RV_FITFUNC(rv))
	call eprintf ("RV_FITHGHT = %g\n") ; call pargr(RV_FITHGHT(rv))
	call eprintf ("RV_FITWIDTH = %g\n") ; call pargr(RV_FITWIDTH(rv))
	call eprintf ("RV_ISHIFT = %d\n") ; call pargi(RV_ISHIFT(rv))
	call eprintf ("RV_ISTART = %d\n") ; call pargi(RV_ISTART(rv))
	call eprintf ("RV_IEND = %d\n") ; call pargi(RV_IEND(rv))
	call eprintf ("RV_MINWIDTH = %g\n") ; call pargr(RV_MINWIDTH(rv))
	call eprintf ("RV_MAXWIDTH = %g\n") ; call pargr(RV_MAXWIDTH(rv))
	call eprintf ("RV_MAXITERS = %d\n") ; call pargi(RV_MAXITERS(rv))
	call eprintf ("RV_PEAK = %d\n") ; call pargi(RV_PEAK(rv))
	call eprintf ("RV_TOLERANCE = %g\n") ; call pargr(RV_TOLERANCE(rv))
	call eprintf ("RV_WEIGHTS = %g\n") ; call pargr(RV_WEIGHTS(rv))
	call eprintf ("RV_WINPAR = %g\n") ; call pargr(RV_WINPAR(rv))
	call eprintf ("RV_WINCENPAR = %g\n") ; call pargr(RV_WINCENPAR(rv))
	call eprintf ("RV_WINDOW = %d\n") ; call pargi(RV_WINDOW(rv))
	call eprintf ("RV_WINCENTER = %d\n") ; call pargi(RV_WINCENTER(rv))
	call eprintf ("RV_WINL = %d\n") ; call pargi(RV_WINL(rv))
	call eprintf ("RV_WINR = %d\n") ; call pargi(RV_WINR(rv))
	call eprintf ("RV_APNUM = %d\n") ; call pargi(RV_APNUM(rv))
	call eprintf ("RV_CCFNPTS = %d\n") ; call pargi(RV_CCFNPTS(rv))
	call eprintf ("RV_CURAPNUM = %d\n") ; call pargi(RV_CURAPNUM(rv))
	call eprintf ("RV_DI1 = %d\n") ; call pargi(RV_DI1(rv))
	call eprintf ("RV_DSCALE = %g\n") ; call pargr(RV_DSCALE(rv))
	call eprintf ("RV_DSLOPE = %g\n") ; call pargr(RV_DSLOPE(rv))
	call eprintf ("RV_DX1 = %g\n") ; call pargr(RV_DX1(rv))
	call eprintf ("RV_DY1 = %g\n") ; call pargr(RV_DY1(rv))
	call eprintf ("RV_DX2 = %g\n") ; call pargr(RV_DX2(rv))
	call eprintf ("RV_DY2 = %g\n") ; call pargr(RV_DY2(rv))
	call eprintf ("RV_FILL = %d\n") ; call pargi(RV_FILL(rv))
	call eprintf ("RV_FFTNPTS = %d\n") ; call pargi(RV_FFTNPTS(rv))
	call eprintf ("RV_IMNUM = %d\n") ; call pargi(RV_IMNUM(rv))
	call eprintf ("RV_IMUPDATE = %d\n") ; call pargi(RV_IMUPDATE(rv))
	call eprintf ("RV_IS_DOUBLE = %d\n") ; call pargi(RV_IS_DOUBLE(rv))
	call eprintf ("RV_MODES = %d\n") ; call pargi(RV_MODES(rv))
	call eprintf ("RV_NOBJS = %d\n") ; call pargi(RV_NOBJS(rv))
	call eprintf ("RV_NTEMPS = %d\n") ; call pargi(RV_NTEMPS(rv))
	call eprintf ("RV_NFITP = %d\n") ; call pargi(RV_NFITP(rv))
	call eprintf ("RV_NPTS = %d\n") ; call pargi(RV_NPTS(rv))
	call eprintf ("RV_NSHIFTS = %d\n") ; call pargi(RV_NSHIFTS(rv))
	call eprintf ("RV_NUMAPS = %d\n") ; call pargi(RV_NUMAPS(rv))
	call eprintf ("RV_OAPNUM = %d\n") ; call pargi(RV_OAPNUM(rv))
	call eprintf ("RV_REBIN = %d\n") ; call pargi(RV_REBIN(rv))
	call eprintf ("RV_RNPTS = %d\n") ; call pargi(RV_RNPTS(rv))
	call eprintf ("RV_RAPNUM = %d\n") ; call pargi(RV_RAPNUM(rv))
	call eprintf ("RV_TEMPNUM = %d\n") ; call pargi(RV_TEMPNUM(rv))
	call eprintf ("RV_UPDATE = %d\n") ; call pargi(RV_UPDATE(rv))
	call eprintf ("RV_VERBOSE = %d\n") ; call pargi(RV_VERBOSE(rv))
	call eprintf ("RV_ZTHRESH = %g\n") ; call pargr(RV_ZTHRESH(rv))
	call eprintf ("RV_OBSPTR = %d\n") ; call pargi(RV_OBSPTR(rv))
	call eprintf ("RV_ALTITUDE = %g\n") ; call pargr(RV_ALTITUDE(rv))
	call eprintf ("RV_LATITUDE = %g\n") ; call pargr(RV_LATITUDE(rv))
	call eprintf ("RV_LONGITUDE = %g\n") ; call pargr(RV_LONGITUDE(rv))
	call eprintf ("RV_NEWGRAPH = %d\n") ; call pargi(RV_NEWGRAPH(rv))
	call eprintf ("RV_RECORD = %d\n") ; call pargi(RV_RECORD(rv))
	call eprintf ("RV_TXFD = %d\n") ; call pargi(RV_TXFD(rv))
	call eprintf ("RV_GRFD = %d\n") ; call pargi(RV_GRFD(rv))
	call eprintf ("RV_VBFD = %d\n") ; call pargi(RV_VBFD(rv))
	call eprintf ("RV_CCFFILE = %d\n") ; call pargi(RV_CCFFILE(rv))
	call eprintf ("RV_CCFTYPE = %d\n") ; call pargi(RV_CCFTYPE(rv))
	call eprintf ("RV_STATLINE = %d\n") ; call pargi(RV_STATLINE(rv))
	call eprintf ("RV_TEMPCODE = %d\n") ; call pargi(RV_TEMPCODE(rv))
	call eprintf ("RV_TCODE = %d\n") ; call pargi(RV_TCODE(rv))
	call eprintf ("RV_PRINTZ = %d\n") ; call pargi(RV_PRINTZ(rv))
	call eprintf ("RV_DTYPE = %d\n") ; call pargi(RV_DTYPE(rv))
	call eprintf ("RV_GTYPE = %d\n") ; call pargi(RV_GTYPE(rv))
	call eprintf ("RV_RESDONE = %d\n") ; call pargi(RV_RESDONE(rv))
	call eprintf ("RV_SPMKEY = %d\n") ; call pargi(RV_SPMKEY(rv))
	call eprintf ("RV_SPMPLOT = %d\n") ; call pargi(RV_SPMPLOT(rv))
	call eprintf ("RV_WHERE = %d\n") ; call pargi(RV_WHERE(rv))
	call eprintf ("RV_X1 = %g\n") ; call pargr(RV_X1(rv))
	call eprintf ("RV_X2 = %g\n") ; call pargr(RV_X2(rv))
	call eprintf ("RV_Y1 = %g\n") ; call pargr(RV_Y1(rv))
	call eprintf ("RV_Y2 = %g\n") ; call pargr(RV_Y2(rv))
end


procedure zz_prstruct2 (rv)

pointer	rv

begin
	call eprintf ("RV_APPARAM = %d\n") ; call pargi(RV_APPARAM(rv))
	call eprintf ("RV_APLIST = %d\n") ; call pargi(RV_APLIST(rv))
	call eprintf ("RV_CMD = %d\n") ; call pargi(RV_CMD(rv))
	call eprintf ("RV_DCBIAS = %g\n") ; call pargr(RV_DCBIAS(rv))
	call eprintf ("RV_DCFLAG = %d\n") ; call pargi(RV_DCFLAG(rv))
	call eprintf ("RV_DELTAV = %g\n") ; call pargr(RV_DELTAV(rv))
	call eprintf ("RV_DO_CORRECT = %d\n") ; call pargi(RV_DO_CORRECT(rv))
	call eprintf ("RV_OFORMAT = %d\n") ; call pargi(RV_OFORMAT(rv))
	call eprintf ("RV_RFORMAT = %d\n") ; call pargi(RV_RFORMAT(rv))
	call eprintf ("RV_FWHM_Y = %g\n") ; call pargr(RV_FWHM_Y(rv))
	call eprintf ("RV_GLOB_W1 = %g\n") ; call pargr(RV_GLOB_W1(rv))
	call eprintf ("RV_GLOB_W2 = %g\n") ; call pargr(RV_GLOB_W2(rv))
	call eprintf ("RV_NEWXCOR = %d\n") ; call pargi(RV_NEWXCOR(rv))
	call eprintf ("RV_OW0 = %g\n") ; call pargr(RV_OW0(rv))
	call eprintf ("RV_OW2 = %g\n") ; call pargr(RV_OW2(rv))
	call eprintf ("RV_OWPC = %g\n") ; call pargr(RV_OWPC(rv))
	call eprintf ("RV_RW0 = %g\n") ; call pargr(RV_RW0(rv))
	call eprintf ("RV_RW2 = %g\n") ; call pargr(RV_RW2(rv))
	call eprintf ("RV_RWPC = %g\n") ; call pargr(RV_RWPC(rv))
	call eprintf ("RV_DO_REBIN = %d\n") ; call pargi(RV_DO_REBIN(rv))
	call eprintf ("RV_VOBS = %g\n") ; call pargd(RV_VOBS(rv))
	call eprintf ("RV_VCOR = %g\n") ; call pargd(RV_VCOR(rv))
	call eprintf ("RV_ERROR = %g\n") ; call pargd(RV_ERROR(rv))
	call eprintf ("RV_HJD = %g\n") ; call pargd(RV_HJD(rv))
	call eprintf ("RV_MJD_OBS = %g\n") ; call pargd(RV_MJD_OBS(rv))
	call eprintf ("RV_VREL = %g\n") ; call pargr(RV_VREL(rv))
	call eprintf ("RV_R = %g\n") ; call pargr(RV_R(rv))
	call eprintf ("RV_SHIFT = %g\n") ; call pargr(RV_SHIFT(rv))
	call eprintf ("RV_SIGMA = %g\n") ; call pargr(RV_SIGMA(rv))
	call eprintf ("RV_FWHM = %g\n") ; call pargr(RV_FWHM(rv))
	call eprintf ("RV_HEIGHT = %g\n") ; call pargr(RV_HEIGHT(rv))
end
