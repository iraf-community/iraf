# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<gset.h>
include	<gki.h>
include	<gio.h>

.help gkiprint
.nf __________________________________________________________________________
GKIPRINT -- Graphics kernel for decoding metacode.  This graphics kernel
formats metacode instructions into readable form and prints them on the output
file.  The gkiprint kernel is useful for examining metafiles and for
debugging kernels which drive specific devices.  The driver consists of the
following procedures:

	     gkp_openws (devname, n, mode)
	    gkp_closews (devname, n)
       gkp_deactivatews (flags)
       gkp_reactivatews (flags)
	    gkp_mftitle (title, n)			**
	      gkp_clear (dummy)
	     gkp_cancel (dummy)
	      gkp_flush (dummy)
	   gkp_polyline (p, npts)
	 gkp_polymarker (p, npts)
	       gkp_text (x, y, text, n)
	   gkp_fillarea (p, npts)
       gkp_getcellarray (m, nx, ny, x1,y1, x2,y2)
       gkp_putcellarray (m, nx, ny, x1,y1, x2,y2)
	  gkp_setcursor (x, y, cursor)
	      gkp_plset (gki)
	      gkp_pmset (gki)
	      gkp_txset (gki)
	      gkp_faset (gki)
	  gkp_getcursor (cursor)
	     gkp_escape (fn, instruction, nwords)	**
	     gkp_setwcs (wcs, nwords)			**
	     gkp_getwcs (wcs, nwords)			**
	    gkp_unknown (gki)				**

A GKI driven device driver may implement any subset of these procedures.
The starred procedures should be omitted by most drivers.  In particular,
the SETWCS and GETWCS instructions are internal instructions which should
be ignored by ordinary device drivers.  The procedure names may be anything,
but the arguments lists must be as shown.  All coordinates are in GKI units,
0 to 32767.  Character strings are passed in ASCII, one character per metacode
word.  Whenever a GKI character string appears as an array argument in the
argument list of a procedure, the count N of the number of characters in the
string follows as the next argument.  GKI character strings are not EOS
delimited.  Polyline, polymarker, and fillarea data is passed as an array
of (x,y) points P, in GKI coordinates, defining the polyline or polygon to
be plotted.

One additional procedure, GKP_INSTALL, is called by the main program of the
graphics kernel task to install the debugging driver, i.e., to fill the DD
array with the entry point addresses of the driver procedures.  For a normal
driver this function is performed by a user supplied procedure named
GKOPEN (graphics kernel open).  The user supplied kernel procedures will
be called to execute each instruction as the instructions are decoded by the
main routine.  The user supplied procedure GKCLOSE will be called when
interpretation ends and the task is about to exit.

		 gkopen (dd)
		gkclose ()

Do not confuse GKOPEN and GKCLOSE, which open and close the graphics kernel,
with GKI_OPENWS and GKI_CLOSEWS, the metacode instructions used to direct
an opened kernel to open and close workstations.
.endhelp ___________________________________________________________________


# GKP_INSTALL -- Install the GKI debugging kernel as a graphics kernel
# device driver.  The device table DD consists of an array of the entry
# point addresses for the driver procedures.  If a driver does not implement
# a particular instruction the table entry for that procedure may be set
# to zero, causing the interpreter to ignore the instruction.

procedure gkp_install (dd, out_fd, verbose_output, use_gkiunits)

int	dd[ARB]			# device table to be initialized
int	out_fd			# output file
int	verbose_output		# verbose output desired
int	use_gkiunits		# print coords in GKI units rather than NDC

int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

extern	gkp_openws(), gkp_closews(), gkp_mftitle(), gkp_clear(), gkp_cancel()
extern	gkp_flush(), gkp_polyline(), gkp_polymarker(), gkp_text()
extern	gkp_fillarea(), gkp_putcellarray(), gkp_setcursor(), gkp_plset()
extern	gkp_pmset(), gkp_txset(), gkp_faset(), gkp_getcursor()
extern	gkp_getcellarray(), gkp_escape(), gkp_setwcs(), gkp_getwcs()
extern	gkp_unknown(), gkp_reactivatews(), gkp_deactivatews()

begin
	# Set the GDC internal parameters.
	fd = out_fd
	stream = NULL
	gkiunits = use_gkiunits
	verbose = verbose_output

	# Install the device driver.
	call zlocpr (gkp_openws,	dd[GKI_OPENWS])
	call zlocpr (gkp_closews,	dd[GKI_CLOSEWS])
	call zlocpr (gkp_reactivatews,	dd[GKI_REACTIVATEWS])
	call zlocpr (gkp_deactivatews,	dd[GKI_DEACTIVATEWS])
	call zlocpr (gkp_mftitle,	dd[GKI_MFTITLE])
	call zlocpr (gkp_clear,		dd[GKI_CLEAR])
	call zlocpr (gkp_cancel,	dd[GKI_CANCEL])
	call zlocpr (gkp_flush,		dd[GKI_FLUSH])
	call zlocpr (gkp_polyline,	dd[GKI_POLYLINE])
	call zlocpr (gkp_polymarker,	dd[GKI_POLYMARKER])
	call zlocpr (gkp_text,		dd[GKI_TEXT])
	call zlocpr (gkp_fillarea,	dd[GKI_FILLAREA])
	call zlocpr (gkp_putcellarray,	dd[GKI_PUTCELLARRAY])
	call zlocpr (gkp_setcursor,	dd[GKI_SETCURSOR])
	call zlocpr (gkp_plset,		dd[GKI_PLSET])
	call zlocpr (gkp_pmset,		dd[GKI_PMSET])
	call zlocpr (gkp_txset,		dd[GKI_TXSET])
	call zlocpr (gkp_faset,		dd[GKI_FASET])
	call zlocpr (gkp_getcursor,	dd[GKI_GETCURSOR])
	call zlocpr (gkp_getcellarray,	dd[GKI_GETCELLARRAY])
	call zlocpr (gkp_escape,	dd[GKI_ESCAPE])
	call zlocpr (gkp_setwcs,	dd[GKI_SETWCS])
	call zlocpr (gkp_getwcs,	dd[GKI_GETWCS])
	call zlocpr (gkp_unknown,	dd[GKI_UNKNOWN])
end


# GKP_CLOSE -- Close the GKP kernel.

procedure gkp_close()
begin
end


# GKP_GRSTREAM -- Set the FD of the graphics stream, from which we shall read
# metacode instructions and to which we shall return cell arrays and cursor
# values.

procedure gkp_grstream (graphics_stream)

int	graphics_stream		# FD of the new graphics stream
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	stream = graphics_stream
end


# GKP_OPENWS -- Open the named workstation.

procedure gkp_openws (devname, n, mode)

short	devname[ARB]		# device name
int	n			# length of device name
int	mode			# access mode

int	junk
pointer	sp, buf
int	itoc()
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call smark (sp)
	call salloc (buf, max (SZ_FNAME, n), TY_CHAR)

	call achtsc (devname, Memc[buf], n)
	Memc[buf+n] = EOS

	call fprintf (fd, "open_workstation '%s', mode=%s\n")
	    call pargstr (Memc[buf])
	    switch (mode) {
	    case NEW_FILE:
		call pargstr ("new_file")
	    case APPEND:
		call pargstr ("append")
	    default:
		junk = itoc (mode, Memc[buf], SZ_FNAME)
	    }
	
	call sfree (sp)
end


# GKP_CLOSEWS -- Close the named workstation.

procedure gkp_closews (devname, n)

short	devname[ARB]		# device name
int	n			# length of device name
pointer	sp, buf
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call smark (sp)
	call salloc (buf, n, TY_CHAR)

	call achtsc (devname, Memc[buf], n)
	Memc[buf+n] = EOS

	call fprintf (fd, "close_workstation '%s'\n")
	    call pargstr (Memc[buf])
	call flush (fd)
	
	call sfree (sp)
end


# GKP_REACTIVATEWS -- Reactivate the workstation (enable graphics).

procedure gkp_reactivatews (flags)

int	flags			# action flags
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "reactivatews %d\n")
	    call pargi (flags)
end


# GKP_DEACTIVATEWS -- Deactivate the workstation (disable graphics).

procedure gkp_deactivatews (flags)

int	flags			# action flags
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "deactivatews %d\n")
	    call pargi (flags)
	call flush (fd)
end


# GKP_MFTITLE -- Metafile title or comment.  A nonfunctional instruction used
# to document a metafile.

procedure gkp_mftitle (title, n)

short	title[ARB]		# title string
int	n			# length of title string
pointer	sp, buf
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call smark (sp)
	call salloc (buf, n, TY_CHAR)

	call achtsc (title, Memc[buf], n)
	Memc[buf+n] = EOS

	call fprintf (fd, "title '%s'\n")
	    call pargstr (Memc[buf])
	
	call sfree (sp)
end


# GKP_CLEAR -- Clear the workstation screen.

procedure gkp_clear (dummy)

int	dummy			# not used at present
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "clear\n")
end


# GKP_CANCEL -- Cancel output.

procedure gkp_cancel (dummy)

int	dummy			# not used at present
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "cancel\n")
	call flush (fd)
end


# GKP_FLUSH -- Flush output.

procedure gkp_flush (dummy)

int	dummy			# not used at present
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "flush\n")
	call flush (fd)
end


# GKP_POLYLINE -- Draw a polyline.

procedure gkp_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	# Print statistics on polyline.
	call gkp_pstat (fd, p, npts, "polyline", verbose, gkiunits)
end


# GKP_POLYMARKER -- Draw a polymarker.

procedure gkp_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	# Print statistics on polymarker.
	call gkp_pstat (fd, p, npts, "polymarker", verbose, gkiunits)
end


# GKP_FILLAREA -- Fill a closed area.

procedure gkp_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	# Print statistics on the fillarea polygon.
	call gkp_pstat (fd, p, npts, "fillarea", verbose, gkiunits)
end


# GKP_TEXT -- Draw a text string.

procedure gkp_text (x, y, text, n)

int	x, y			# where to draw text string
short	text[ARB]		# text string
int	n			# number of characters

pointer	sp, buf
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call smark (sp)
	call salloc (buf, n, TY_CHAR)

	call achtsc (text, Memc[buf], n)
	Memc[buf+n] = EOS

	if (gkiunits == YES) {
	    call fprintf (fd, "text %5d, %5d, '%s'\n")
		call pargi (x)
		call pargi (y)
		call pargstr (Memc[buf])
	} else {
	    call fprintf (fd, "text %4.2f, %4.2f, '%s'\n")
		call pargr (real(x) / GKI_MAXNDC)
		call pargr (real(y) / GKI_MAXNDC)
		call pargstr (Memc[buf])
	}
	
	call sfree (sp)
end


# GKP_PUTCELLARRAY -- Draw a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure gkp_putcellarray (m, nx, ny, x1,y1, x2,y2)

int	nx, ny			# number of pixels in X and Y
short	m[nx,ny]		# cell array
int	x1, y1			# lower left corner of output window
int	x2, y2			# lower left corner of output window

int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "put_cellarray nx=%d, ny=%d, ")
	    call pargi (nx)
	    call pargi (ny)

	if (gkiunits == YES) {
	    call fprintf (fd, "x1=%5d, y1=%5d, x2=%5d, y2=%5d\n")
		call pargi (x1)
		call pargi (y1)
		call pargi (x2)
		call pargi (y2)
	} else {
	    call fprintf (fd, "x1=%4.2f, y1=%4.2f, x2=%4.2f, y2=%4.2f\n")
		call pargr (real(x1) / GKI_MAXNDC)
		call pargr (real(y1) / GKI_MAXNDC)
		call pargr (real(x2) / GKI_MAXNDC)
		call pargr (real(y2) / GKI_MAXNDC)
	}

	if (verbose == YES)
	    call gkp_dump (fd, m, (nx * ny))
end


# GKP_GETCELLARRAY -- Input a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure gkp_getcellarray (nx, ny, x1,y1, x2,y2)

int	nx, ny			# number of pixels in X and Y
int	x1, y1			# lower left corner of input window
int	x2, y2			# lower left corner of input window

pointer	sp, buf
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "get_cellarray nx=%d, ny=%d, ")
	    call pargi (nx)
	    call pargi (ny)

	if (gkiunits == YES) {
	    call fprintf (fd, "x1=%5d, y1=%5d, x2=%5d, y2=%5d\n")
		call pargi (x1)
		call pargi (y1)
		call pargi (x2)
		call pargi (y2)
	} else {
	    call fprintf (fd, "x1=%4.2f, y1=%4.2f, x2=%4.2f, y2=%4.2f\n")
		call pargr (real(x1) / GKI_MAXNDC)
		call pargr (real(y1) / GKI_MAXNDC)
		call pargr (real(x2) / GKI_MAXNDC)
		call pargr (real(y2) / GKI_MAXNDC)
	}

	if (stream == NULL)
	    return

	call smark (sp)
	call salloc (buf, nx * ny, TY_SHORT)
	call amovks (short(-1), Mems[buf], nx * ny)

	call flush (fd)
	iferr {
	    call gki_retcellarray (stream, Mems[buf], nx * ny) 
	    call flush (stream)
	} then
	    ;
	
	call sfree (sp)
end


# GKP_SETCURSOR -- Set the position of a cursor.

procedure gkp_setcursor (x, y, cursor)

int	x, y			# new position of cursor
int	cursor			# cursor to be set
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	if (gkiunits == YES) {
	    call fprintf (fd, "set_cursor %5d, %5d, cursor=%d\n")
		call pargi (x)
		call pargi (y)
		call pargi (cursor)
	} else {
	    call fprintf (fd, "set_cursor %4.2f, %4.2f, cursor=%d\n")
		call pargr (real(x) / GKI_MAXNDC)
		call pargr (real(y) / GKI_MAXNDC)
		call pargi (cursor)
	}
end


# GKP_GETCURSOR -- Get the position of a cursor.

procedure gkp_getcursor (cursor)

int	cursor
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "get_cursor cursor=%d\n")
	    call pargi (cursor)
	call flush (fd)

	if (stream != NULL)
	    iferr {
		# gki_retcursorvalue (stream, cn, key, sx, sy, rn, rx, ry)
		call gki_retcursorvalue (stream, 0, EOF, 0, 0, 0, 0, 0)
		call flush (stream)
	    } then
		;
end


# GKP_PLSET -- Set the polyline attributes.

procedure gkp_plset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "set_polyline ltype=%d, lwidth=%0.2f, color=%d\n")
	    call pargs (gki[GKI_PLSET_LT])
	    call pargr (GKI_UNPACKREAL (gki[GKI_PLSET_LW]))
	    call pargs (gki[GKI_PLSET_CI])
end


# GKP_PMSET -- Set the polymarker attributes.

procedure gkp_pmset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "set_polymarker mtype=%d, mwidth=%0.2f, color=%d\n")
	    call pargs (gki[GKI_PMSET_MT])
	    call pargr (GKI_UNPACKREAL (gki[GKI_PMSET_MW]))
	    call pargs (gki[GKI_PMSET_CI])
end


# GKP_FASET -- Set the fillarea attributes.

procedure gkp_faset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "set_fillarea style=%d, color=%d\n")
	    call pargs (gki[GKI_FASET_FS])
	    call pargs (gki[GKI_FASET_CI])
end


# GKP_TXSET -- Set the text drawing attributes.

procedure gkp_txset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "set_text up=%d, path=%d, hjustify=%s, ")
	    call pargs (gki[GKI_TXSET_UP])
	    call gkp_txparg (gki[GKI_TXSET_P])
	    call gkp_txparg (gki[GKI_TXSET_HJ])
	call fprintf (fd, "vjustify=%s, font=%s,\n")
	    call gkp_txparg (gki[GKI_TXSET_VJ])
	    call gkp_txparg (gki[GKI_TXSET_F])

	call fprintf (fd, "\tsize=%0.2f, spacing=%0.2f, color=%d, quality=%s\n")
	    call pargr (GKI_UNPACKREAL (gki[GKI_TXSET_SZ]))
	    call pargr (GKI_UNPACKREAL (gki[GKI_TXSET_SP]))
	    call pargs (gki[GKI_TXSET_CI])
	    call gkp_txparg (gki[GKI_TXSET_Q])
end


# GKP_ESCAPE -- Device dependent instruction.

procedure gkp_escape (fn, instruction, nwords)

int	fn			# function code
short	instruction[ARB]	# instruction data words
int	nwords			# length of instruction
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "escape %d, nwords=%d\n")
	    call pargi (fn)
	    call pargi (nwords)

	# Dump the instruction.
	if (verbose == YES)
	    call gkp_dump (fd, instruction, nwords)
end


# GKP_SETWCS -- Set the world coordinate systems.  Internal GIO instruction.

procedure gkp_setwcs (wcs, nwords)

short	wcs[ARB]		# WCS data
int	nwords			# number of words of data

int	i, nwcs
pointer	sp, wcs_temp, w
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call smark (sp)
	call salloc (wcs_temp, LEN_WCSARRAY, TY_STRUCT)

	call fprintf (fd, "set_wcs nwords=%d\n")
	    call pargi (nwords)

	nwcs = nwords * SZ_SHORT / SZ_STRUCT / LEN_WCS
	if (verbose == YES && nwcs > 1) {
	    call amovi (wcs, Memi[wcs_temp], nwcs * LEN_WCS)

	    do i = 1, nwcs {
		w = ((i - 1) * LEN_WCS) + wcs_temp
		if ((WCS_WX1(w) > EPSILON) ||
		    (abs(1.0 - WCS_WX2(w)) > EPSILON) ||
		    (WCS_WY1(w) > EPSILON) ||
		    (abs(1.0 - WCS_WY2(w)) > EPSILON)) {

		    call fprintf (fd, "\t%2d %g %g %g %g  ")
			call pargi (i)
			call pargr (WCS_WX1(w))
			call pargr (WCS_WX2(w))
			call pargr (WCS_WY1(w))
			call pargr (WCS_WY2(w))

		    call fprintf (fd, "%4.2f %4.2f %4.2f %4.2f  ")
			call pargr (WCS_SX1(w))
			call pargr (WCS_SX2(w))
			call pargr (WCS_SY1(w))
			call pargr (WCS_SY2(w))

		    call fprintf (fd, "%d %d %d\n")
			call pargi (WCS_XTRAN(w))
			call pargi (WCS_YTRAN(w))
			call pargi (WCS_CLIP(w))
		}
	    }
	}

	call sfree (sp)
end


# GKP_GETWCS -- Get the world coordinate systems.  Internal GIO instruction.

procedure gkp_getwcs (wcs, nwords)

short	wcs[ARB]		# WCS data
int	nwords			# number of words of data
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "get_wcs nwords=%d\n")
	    call pargi (nwords)
end


# GKP_UNKNOWN -- The unknown instruction.  Called by the interpreter whenever
# an unrecognized opcode is encountered.  Should never be called.

procedure gkp_unknown (gki)

short	gki[ARB]		# the GKI instruction
int	fd, stream, verbose, gkiunits
common	/gkpcom/ fd, stream, verbose, gkiunits

begin
	call fprintf (fd, "unknown\n")
end


# GKP_PSTAT -- Compute and print on the standard error output a statistical
# summary of a sequence of (x,y) points.  If verbose mode is enabled, follow
# this by the values of the points themselves.

procedure gkp_pstat (fd, p, npts, label, verbose, gkiunits)

int	fd			# output file
short	p[npts]			# array of points, i.e., (x,y) pairs
int	npts			# number of points
char	label[ARB]		# type of instruction
int	verbose			# verbose output desired
int	gkiunits		# print coords in GKI rather than NDC units

int	i
real	x, y, xsum, xmin, xmax, ysum, ymin, ymax, scale

begin
	if (gkiunits == YES)
	    scale = 1.0
	else
	    scale = 1.0 / GKI_MAXNDC

	xsum = 0
	xmin = 1.0
	xmax = 0
	ysum = 0
	ymin = 1.0
	ymax = 0

	# Compute mean, minimum, and maximum values.
	do i = 1, npts * 2, 2 {
	    x = real (p[i]) * scale
	    xsum = xsum + x
	    if (x < xmin)
		xmin = x
	    if (x > xmax)
		xmax = x

	    y = real (p[i+1]) * scale
	    ysum = ysum + y
	    if (y < ymin)
		ymin = y
	    if (y > ymax)
		ymax = y
	}

	# Print summary of statistics.
	call fprintf (fd, "%s np=%d, ")
	    call pargstr (label)
	    call pargi (npts)

	if (gkiunits == YES)
	    call fprintf (fd, "xmin=%d,xmax=%d,xavg=%d, ")
	else
	    call fprintf (fd, "xmin=%4.2f,xmax=%4.2f,xavg=%4.2f, ")
	    if (npts == 0) {
		do i = 1, 3
		    call pargr (INDEF)
	    } else {
		call pargr (xmin)
		call pargr (xmax)
		call pargr (xsum / npts)
	    }

	if (gkiunits == YES)
	    call fprintf (fd, "ymin=%d,ymax=%d,yavg=%d\n")
	else
	    call fprintf (fd, "ymin=%4.2f,ymax=%4.2f,yavg=%4.2f\n")
	    if (npts == 0) {
		do i = 1, 3
		    call pargr (INDEF)
	    } else {
		call pargr (ymin)
		call pargr (ymax)
		call pargr (ysum / npts)
	    }

	# Dump the points if verbose output is enabled.
	if (verbose == NO && npts > 0)
	    return

	call fprintf (fd, "\t")
	for (i=1;  i <= npts * 2;  i=i+2) {
	    if (i > 1 && mod (i-1, 10) == 0)
		call fprintf (fd, "\n\t")
	    if (gkiunits == YES)
		call fprintf (fd, "%5d %5d  ")
	    else
		call fprintf (fd, "%5.3f %5.3f  ")
		call pargr (real(p[i])   * scale)
		call pargr (real(p[i+1]) * scale)
	}
	call fprintf (fd, "\n")
end


# GKP_DUMP -- Print a sequence of metacode words as a table, formatted eight
# words per line, in decimal.

procedure gkp_dump (fd, data, nwords)

int	fd			# output file
short	data[ARB]		# metacode data
int	nwords			# number of words of data
int	i

begin
	if (nwords <= 0)
	    return

	call fprintf (fd, "\t")

	for (i=1;  i <= nwords;  i=i+1) {
	    if (i > 1 && mod (i-1, 8) == 0)
		call fprintf (fd, "\n\t")
	    call fprintf (fd, "%7d")
		call pargs (data[i])
	}

	call fprintf (fd, "\n")
end
