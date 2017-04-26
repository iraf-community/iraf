include <gset.h>
include "pexamine.h"

# PT_COLON -- Execute PEXAMINE colon commands.

procedure pt_colon (px, gd, cmdstr, newdata, newxy, newhist, newcoords,
	newplot, plottype, undelete)

pointer	px		# pointer to the pexamine structure
pointer	gd		# pointer to the graphics stream
char	cmdstr[ARB]	# the colon command
int	newdata		# read new columns from the input file
int	newxy		# load new columns into the x and y arrays
int	newhist		# load new column into the histogram array
int	newcoords	# load new data into the coords array
int	newplot		# replot the current plot
int	plottype	# the plot type
int	undelete	# delete or undelete points

bool	bval
int	type, ncmd, nrcols, ncols, ival, marktype
pointer	sp, cmd, str, rcols, cols
real	rval
bool	clgetb()
int	strdic(), nscan(), clgeti()
real	clgetr()

errchk	clgstr(), clgetb(), clgeti(), clgetr()
errchk	clpstr(), clputb(), clputi(), clputr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (rcols, PX_SZCOLNAME * (PX_MAXNCOLS + 1), TY_CHAR)
	call salloc (cols, PX_SZCOLNAME * (PX_MAXNCOLS + 1), TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Define the pset type.
	call salloc (str, SZ_LINE, TY_CHAR)
	switch (plottype) {
	case PX_XYPLOT:
	    call strcpy ("xyplot.", Memc[str], SZ_LINE)
	case PX_HISTPLOT:
	    call strcpy ("histplot.", Memc[str], SZ_LINE)
	case PX_RADPLOT:
	    call strcpy ("radplot.", Memc[str], SZ_LINE)
	case PX_SURFPLOT:
	    call strcpy ("surfplot.", Memc[str], SZ_LINE)
	case PX_CNTRPLOT:
	    call strcpy ("cntrplot.", Memc[str], SZ_LINE)
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PX_PCMDS)
	switch (ncmd) {

	case PX_PCMD_PHOTCOLUMNS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call pt_gphotcols (px, Memc[rcols], nrcols, Memc[cols],
		    ncols) 
		if (gd != NULL)
		    call gdeactivate (gd, AW_CLEAR)
		call pt_lcols ("PHOTOMETRY COLUMNS", Memc[rcols], nrcols,
		    Memc[cols], ncols)
		if (gd != NULL)
		    call greactivate (gd, AW_PAUSE)
	    } else {
		call pt_gusercols (px, Memc[rcols], nrcols, Memc[cols], ncols) 
		call pt_setnames (px, Memc[cmd], Memc[rcols])
		newdata = YES
		newxy = YES
		newhist = YES
		newcoords = YES
		newplot = YES
	    }

	case PX_PCMD_USERCOLUMNS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call pt_gusercols (px, Memc[rcols], nrcols, Memc[cols], ncols) 
		if (gd != NULL)
		    call gdeactivate (gd, AW_CLEAR)
		call pt_lcols ("USER COLUMNS", Memc[rcols], nrcols, Memc[cols],
		    ncols)
		if (gd != NULL)
		    call greactivate (gd, AW_PAUSE)
	    } else {
		call pt_gphotcols (px, Memc[rcols], nrcols, Memc[cols], ncols) 
		call pt_setnames (px, Memc[rcols], Memc[cmd])
		newdata = YES
		newxy = YES
		newhist = YES
		newcoords = YES
		newplot = YES
	    }

	case PX_PCMD_XCOLUMN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call printf ("X column requested: %s  stored: %s\n")
		    call pargstr (PX_RXCOLNAME(px))
		    call pargstr (PX_XCOLNAME(px))
	    } else {
		call strupr (Memc[cmd])
		call strcpy (Memc[cmd], PX_RXCOLNAME(px), PX_SZCOLNAME)
		if (strdic (Memc[cmd], Memc[cmd], PX_SZCOLNAME,
		    Memc[PX_COLNAMES(px)]) > 0) {
		    call strcpy (Memc[cmd], PX_XCOLNAME(px), PX_SZCOLNAME)
		    newxy = YES
		    newplot = YES
		    call clputr ("xyplot.x1", INDEFR)
		    call clputr ("xyplot.x2", INDEFR)
		} else {
		    call printf ("Column %s not found\n")
			call pargstr (PX_RXCOLNAME(px))
		}
	    }

	case PX_PCMD_YCOLUMN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call printf ("Y column requested: %s  stored: %s\n")
		    call pargstr (PX_RYCOLNAME(px))
		    call pargstr (PX_YCOLNAME(px))
	    } else {
		call strupr (Memc[cmd])
		call strcpy (Memc[cmd], PX_RYCOLNAME(px), PX_SZCOLNAME)
		if (strdic (Memc[cmd], Memc[cmd], PX_SZCOLNAME,
		    Memc[PX_COLNAMES(px)]) > 0) {
		    call strcpy (Memc[cmd], PX_YCOLNAME(px), PX_SZCOLNAME)
		    newxy = YES
		    newplot = YES
		    call clputr ("xyplot.y1", INDEFR)
		    call clputr ("xyplot.y2", INDEFR)
		} else {
		    call printf ("Column %s not found\n")
			call pargstr (PX_RYCOLNAME(px))
		}
	    }

	case PX_PCMD_HCOLUMN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call printf ("Histogram column requested: %s  stored: %s\n")
		    call pargstr (PX_RHCOLNAME(px))
		    call pargstr (PX_HCOLNAME(px))
	    } else {
		call strupr (Memc[cmd])
		call strcpy (Memc[cmd], PX_RHCOLNAME(px), PX_SZCOLNAME)
		if (strdic (Memc[cmd], Memc[cmd], PX_SZCOLNAME,
		    Memc[PX_COLNAMES(px)]) > 0) {
		    call strcpy (Memc[cmd], PX_HCOLNAME(px), PX_SZCOLNAME)
		    newhist = YES
		    newplot = YES
		    call clputr ("histplot.z1", INDEFR)
		    call clputr ("histplot.z2", INDEFR)
		    call clputr ("histplot.x1", INDEFR)
		    call clputr ("histplot.x2", INDEFR)
		    call clputr ("histplot.y1", INDEFR)
		    call clputr ("histplot.y2", INDEFR)
		} else {
		    call printf ("Column %s not found\n")
			call pargstr (PX_RHCOLNAME(px))
		}
	    }

	case PX_PCMD_XPOSCOLUMN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call printf ("X coord column requested: %s  stored: %s\n")
		    call pargstr (PX_RXPOSNAME(px))
		    call pargstr (PX_XPOSNAME(px))
	    } else {
		call strupr (Memc[cmd])
		call strcpy (Memc[cmd], PX_RXPOSNAME(px), PX_SZCOLNAME)
		if (strdic (Memc[cmd], Memc[cmd], PX_SZCOLNAME,
		    Memc[PX_COLNAMES(px)]) > 0) {
		    call strcpy (Memc[cmd], PX_XPOSNAME(px), PX_SZCOLNAME)
		    newcoords = YES
		} else {
		    call printf ("Column %s not found\n")
			call pargstr (PX_RXPOSNAME(px))
		}
	    }

	case PX_PCMD_YPOSCOLUMN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call printf ("X coord column requested: %s  stored: %s\n")
		    call pargstr (PX_RYPOSNAME(px))
		    call pargstr (PX_YPOSNAME(px))
	    } else {
		call strupr (Memc[cmd])
		call strcpy (Memc[cmd], PX_RYPOSNAME(px), PX_SZCOLNAME)
		if (strdic (Memc[cmd], Memc[cmd], PX_SZCOLNAME,
		    Memc[PX_COLNAMES(px)]) > 0) {
		    call strcpy (Memc[cmd], PX_YPOSNAME(px), PX_SZCOLNAME)
		    newcoords = YES
		} else {
		    call printf ("Column %s not found\n")
			call pargstr (PX_RXPOSNAME(px))
		}
	    }

	case PX_PCMD_EDIT:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS)
		call printf ("The parameter set is undefined\n")
	    else {
		type = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PX_PLOTTYPES)
		if (gd != NULL)
		    call gdeactivate (gd, 0)
		switch (type) {
		case PX_XYPLOT:
		    call clcmdw ("eparam xyplot")
		case PX_HISTPLOT:
		    call clcmdw ("eparam histplot")
		case PX_RADPLOT:
		    call clcmdw ("eparam radplot")
		case PX_SURFPLOT:
		    call clcmdw ("eparam surfplot")
		case PX_CNTRPLOT:
		    call clcmdw ("eparam cntrplot")
		default:
		    call printf (
		        "The parameter set %s undefined\n")
			call pargstr (Memc[cmd])
		}
		newplot = YES
		if (gd != NULL)
		    call greactivate (gd, 0)
	    }

	case PX_PCMD_UNLEARN:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS)
		call printf ("The parameter set is undefined\n")
	    else {
		type = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PX_PLOTTYPES)
		switch (type) {
		case PX_XYPLOT:
		    call clcmdw ("unlearn xyplot")
		case PX_HISTPLOT:
		    call clcmdw ("unlearn histplot")
		case PX_RADPLOT:
		    call clcmdw ("unlearn radplot")
		case PX_SURFPLOT:
		    call clcmdw ("unlearn surfplot")
		case PX_CNTRPLOT:
		    call clcmdw ("unlearn cntrplot")
		default:
		    call printf ("The parameter set %s  is undefined\n")
			call pargstr (Memc[cmd])
		}
		newplot = YES
	    }

	case PX_PCMD_NBINS:
	    call strcpy ("histplot.nbins", Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("nbins = %d\n")
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_Z1:
	    call strcpy ("histplot.z1", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("z1 = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_Z2:
	    call strcpy ("histplot.z2", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("z2 = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_TOP_CLOSED:
	    call strcpy ("histplot.top_closed", Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("top_closed = %b\n")
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_X1:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_X2:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_Y1:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_Y2:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_MARKER:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call clgstr (Memc[str], Memc[cmd], SZ_LINE)
		call printf ("marker = %s\n")
		    call pargstr (Memc[cmd])
	    } else {
		call pt_marker (Memc[cmd], SZ_LINE, marktype)
		call clpstr (Memc[str], Memc[cmd])
	    }

	case PX_PCMD_SZMARKER:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (clgetr (Memc[str]))
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_GRID:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargb (bval)
		    call pargstr (Memc[cmd])
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_LOGX:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargstr (Memc[cmd])
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_LOGY:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargstr (Memc[cmd])
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_BOX:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargstr (Memc[cmd])
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_TICKLABELS:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargstr (Memc[cmd])
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_FILL:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargstr (Memc[cmd])
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_ROUND:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("%s = %b\n")
		    call pargstr (Memc[cmd])
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_MAJRX:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("%s = %d\n")
		    call pargstr (Memc[cmd])
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_MINRX:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("%s = %d\n")
		    call pargstr (Memc[cmd])
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_MAJRY:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("%s = %d\n")
		    call pargstr (Memc[cmd])
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_MINRY:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("%s = %d\n")
		    call pargstr (Memc[cmd])
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_DELETE:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("delete = %b\n")
		if (undelete == YES)
		    call pargb (false)
		else
		    call pargb (true)
	    } else {
		if (bval)
		    undelete = NO
		else
		    undelete = YES
	    }

	case PX_PCMD_RIN:
	    call strcpy ("radplot.rinner", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("rinner = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_ROUT:
	    call strcpy ("radplot.router", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("router = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_NCOLUMNS:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("%s = %d\n")
		    call pargstr (Memc[cmd])
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_NLINES:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("%s = %d\n")
		    call pargstr (Memc[cmd])
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_AXES:
	    call strcpy ("surfplot.axes", Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("axes = %b\n")
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	case PX_PCMD_ANGH:
	    call strcpy ("surfplot.angh", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("angh = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_ANGV:
	    call strcpy ("surfplot.angv", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("angv = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_FLOOR:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_CEILING:
	    call strcat (Memc[cmd], Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("%s = %g\n")
		    call pargstr (Memc[cmd])
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_ZERO:
	    call strcpy ("cntrplot.zero", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("zero = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_NCONTOURS:
	    call strcpy ("cntrplot.ncontours", Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("ncontours = %d\n")
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_INTERVAL:
	    call strcpy ("cntrplot.interval", Memc[str], SZ_LINE)
	    call gargr (rval)
	    if (nscan() == 1) {
		rval = clgetr (Memc[str])
		call printf ("interval = %g\n")
		    call pargr (rval)
	    } else
		call clputr (Memc[str], rval)

	case PX_PCMD_NHI:
	    call strcpy ("cntrplot.nhi", Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("nhi = %d\n")
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_DASHPAT:
	    call strcpy ("cntrplot.dashpat", Memc[str], SZ_LINE)
	    call gargi (ival)
	    if (nscan() == 1) {
		ival = clgeti (Memc[str])
		call printf ("dashpat = %d\n")
		    call pargi (ival)
	    } else
		call clputi (Memc[str], ival)

	case PX_PCMD_LABEL:
	    call strcpy ("cntrplot.label", Memc[str], SZ_LINE)
	    call gargb (bval)
	    if (nscan() == 1) {
		bval = clgetb (Memc[str])
		call printf ("label = %b\n")
		    call pargb (bval)
	    } else
		call clputb (Memc[str], bval)

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
