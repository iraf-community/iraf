include <error.h>
include <mach.h>
include	<ctype.h>
include	<gset.h>
include	<fset.h>
include <tbset.h>
include "../../lib/ptkeysdef.h"
include "pexamine.h"

define	GHELPFILE	"ptools$pexamine/pexamine.key"
define	FRACTION	0.05

int procedure pt_plot (gd, px, apd, apkey, im, deleted, npix, max_npix,
	first_star, match_radius, use_display)

pointer	gd			# pointer to the graphics stream
pointer	px			# pointer to the pexamine structure
pointer	apd			# the input catalog descriptor
pointer	apkey			# pointer to the text file structure
pointer	im			# pointer to the input image
int	deleted[ARB]		# array of deleted values
int	npix			# number of points
int	max_npix		# maximum number of points
int	first_star		# first object read in
real	match_radius		# tolerance in pixels for cursor positioning
int	use_display		# use the image display

int	newdata, newxy, xyinvalid, newhist, hinvalid, plottype, newplot
int	firstplot, newcoo, cooinvalid, undelete, status, key, starno, curtype
pointer	sp, title, xlabel, ylabel, cmd, x, y, h, xpos, ypos
real	wx, wy, twx, twy

bool	fp_equalr()
int	pt_rxydata(), pt_rhdata(), pt_rcoodata(), pt_getphot(), pt_fstarg()
int	pt_gcur(), pt_gldata()

define	replot_		91

begin
	# Allocate working stack space.
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (xlabel, SZ_LINE, TY_CHAR)
	call salloc (ylabel, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize some parameters.
	twx = INDEFR
	twy = INDEFR
	newdata = NO
	newxy = YES
	newhist = YES
	newcoo = YES
	newplot = YES
	firstplot = YES
	curtype = 'g'
	plottype = PX_XYPLOT
	call amovki (PX_GOOD, deleted, npix)

	undelete = NO

replot_
	if (newdata == YES) {
	    if (apkey != NULL) {
		call pt_kyfree (apkey)
		call pt_kyinit (apkey)
	    }
	    npix = pt_getphot (px, apd, apkey, npix, first_star)
	}

	if (newxy == YES)
	    xyinvalid = pt_rxydata (px, x, y)
	if (newhist == YES)
	    hinvalid = pt_rhdata (px, h)
	if (newcoo == YES)
	    cooinvalid = pt_rcoodata (px, xpos, ypos)

	switch (plottype) {
	case PX_XYPLOT:
	    call pt_xyinfo (px, apd, apkey, Memc[title], SZ_LINE,
	        Memc[xlabel], Memc[ylabel], SZ_LINE)
	    if (xyinvalid == NO) {
		if (newplot == YES) {
	            call pt_xyplot (gd, Memr[x], Memr[y], deleted, npix,
		        Memc[title], Memc[xlabel], Memc[ylabel])
		}
	    } else {
	        call printf ("Cannot plot X: %s versus Y: %s\n")
		    call pargstr (Memc[xlabel])
		    call pargstr (Memc[ylabel])
	    }

	case PX_HISTPLOT:
	    call pt_hinfo (px, apd, apkey, Memc[title], SZ_LINE,
		Memc[xlabel], Memc[ylabel], SZ_LINE)
	    if (hinvalid == NO) {
		if (newplot == YES) {
	            call pt_hplot (gd, Memr[h], deleted, npix, Memc[title],
		        Memc[xlabel], Memc[ylabel])
		}
	    } else {
	        call printf ("Cannot plot histogram of %s\n")
		    call pargstr (Memc[xlabel])
	    }

	case PX_RADPLOT:
	    if (im != NULL) {
		if (newplot == YES)
		    call pt_rplot (gd, im, twx, twy)
	    } else
		call printf ("The input image is undefined\n")

	case PX_SURFPLOT:
	    if (im != NULL) {
		if (newplot == YES)
		    call pt_splot (gd, im, twx, twy)
	    } else
		call printf ("The input image is undefined\n")

	case PX_CNTRPLOT:
	    if (im != NULL) {
		if (newplot == YES)
		    call pt_cplot (gd, im, twx, twy)
	    } else
		call printf ("The input image is undefined\n")

	default:
	    call printf ("Invalid plot type\n")
	}

	if ((firstplot == YES || newdata == YES) && (xyinvalid == NO)) {
	    call printf ("nstars read: %d  first_star: %d  max_nstars: %d\n")
	        call pargi (npix)
	        call pargi (first_star)
	        call pargi (max_npix)
	}

	newdata = NO
	newxy = NO
	newcoo = NO
	newhist = NO
	newplot = NO
	firstplot = NO

	while (pt_gcur (curtype, wx, wy, key, Memc[cmd], SZ_LINE) != EOF) {

	    switch (key) {

	    # Quit and do not make changes.
	    case 'q':
		call printf ("Type 'q' to confirm quit without saving edits\n")
		if (pt_gcur (curtype, wx, wy, key, Memc[cmd], SZ_LINE) == EOF)
		    next
		if (key != 'q')
		    next
		status = PX_QUIT
		break

	    # Exit and do the changes.
	    case 'e':
		status = PX_EXIT
		break

	    # Print the help page.
	    case '?':
		call gpagefile (gd, GHELPFILE, "")

	    # Activate the graphics cursor.
	    case 'g':
		curtype = 'g'
		
	    # Activate the image cursor.
	    case 'i':
		if (use_display == YES)
		    curtype = 'i'
		else
		    call printf ("The image display is not available\n")

	    # Plot the current y column versus the current x column.
	    case 'x':
		if (plottype != PX_XYPLOT) {
		    newplot = YES
		    plottype = PX_XYPLOT
		}
		if (newplot == YES)
		    goto replot_

	    # Plot the current histogram.
	    case 'h':
		if (plottype != PX_HISTPLOT) {
		    newplot = YES
		    plottype = PX_HISTPLOT
		}
		if (newplot == YES)
		    goto replot_

	    # Print a matrix of points around the cursor.
	    case 'm':
		if (im == NULL) {
		    call printf ("The input image is undefined\n")
		    next
		} else if (cooinvalid == YES) {
		    call printf ("The x or y coordinate data is undefined\n")
		    next
		} else if (curtype == 'i') {
		    starno = pt_fstarg (gd, wx, wy, Memr[xpos], Memr[ypos],
		        npix, match_radius)
		} else if (xyinvalid == YES) {
		    call printf ("The x or y column data is undefined\n")
		    next
		} else if (plottype != PX_XYPLOT) {
		    call printf ("Points must be marked on an X-Y plot\n")
		    next
		} else {
		    starno = pt_fstarg (gd, wx, wy, Memr[x], Memr[y], npix,
		        INDEFR)
		}

		if (starno > 0)
		    call pt_print (gd, im, Memr[xpos+starno-1],
		        Memr[ypos+starno-1])

	    # Plot the the radial profile of the point nearest the graphics
	    # cursor.
	    case 'r', 's', 'c':

		if (im == NULL) {
		    call printf ("The input image is undefined\n")
		    next
		}
		if (cooinvalid == YES) {
		    call printf ("The x or y coordinate data is undefined\n")
		    next
		}

		if (curtype == 'i') {
		    starno = pt_fstarg (gd, wx, wy, Memr[xpos], Memr[ypos],
		        npix, match_radius)
		} else if (xyinvalid == YES) {
		    call printf ("The x or y column data is undefined\n")
		    next
		} else if (plottype != PX_XYPLOT) {
		    call printf ("Points must be marked on the X-Y plot\n")
		    next
		} else {
		    starno = pt_fstarg (gd, wx, wy, Memr[x], Memr[y], npix,
		        INDEFR)
		}

		if (starno > 0) {
		    switch (key) {
		    case 'r':
			if (plottype != PX_RADPLOT)
			    newplot = YES
		        plottype = PX_RADPLOT
		    case 'c':
			if (plottype != PX_CNTRPLOT)
			    newplot = YES
		        plottype = PX_CNTRPLOT
		    case 's':
			if (plottype != PX_SURFPLOT)
			    newplot = YES
		        plottype = PX_SURFPLOT
		    }
		    wx = Memr[xpos+starno-1]
		    wy = Memr[ypos+starno-1]
		    if (! fp_equalr (wx, twx) && ! fp_equalr (wy, twy)) {
		        newplot = YES
			twx = wx
			twy = wy
		    }
		} else
		    call printf ("Cannot find selected star in the catalog\n")

		if (newplot == YES)
		    goto replot_

	    # Replot the current plot.
	    case 'p':
		newplot = YES
		goto replot_

	    # Print out the names, types and units of all defined columns.
	    case 'l':
		call pt_colinfo (gd, apd, apkey)

	    # Print out the names, values and units of the stored columns
	    # for the object nearest the cursor.
	    case 'o':
		if (curtype == 'g') {
		    if (xyinvalid == YES)
			call printf ("The x or y column data is undefined\n")
		    else if (plottype != PX_XYPLOT)
			call printf ("Points must be marked on an x-y plot\n")
		    else  {
		        starno = pt_gldata (gd, px, apd, apkey, wx, wy,
			    Memr[x], Memr[y], npix, INDEFR)
			if (starno > 0)
			    call printf ("Star found\n")
			else
			    call printf ("Star not found\n")
		    }
		} else if (curtype == 'i') {
		    if (cooinvalid == NO) {
		        starno = pt_gldata (gd, px, apd, apkey, wx, wy,
			    Memr[xpos], Memr[ypos], npix, match_radius)
			if ((gd != NULL) && (starno > 0) && (xyinvalid == NO) &&
			    (plottype == PX_XYPLOT)) {
			    call gscur (gd, Memr[x+starno-1], Memr[y+starno-1])
			    call printf ("Star found\n")
			    curtype = 'g'
			} else
			    call printf ("Star not found\n")
		    } else
			call printf (
			    "The x or y coordinate data is undefined\n")
		}

	    # Undelete everything.
	    case 'z':
		call amovki (PX_GOOD, deleted, npix)
		newplot = YES
		goto replot_

	    # Delete points and replot.
	    case 'f':
		call pt_update (deleted, npix)
		newplot = YES
		goto replot_

	    # Mark a point for deletion.
	    case 'd':
		if (curtype == 'g') {
		    if ((xyinvalid == YES) || (plottype != PX_XYPLOT))
			call printf (
			    "Invalid plot type for deleting points\n")
		    else
		        call pt_delpt (gd, wx, wy, Memr[x], Memr[y], Memr[x],
			    Memr[y], deleted, npix, NO, INDEFR)
		} else if (curtype == 'i') {
		    if (cooinvalid == NO) {
		        if ((xyinvalid == NO) && (plottype == PX_XYPLOT))
		            call pt_delpt (gd, wx, wy, Memr[xpos], Memr[ypos],
			        Memr[x], Memr[y], deleted, npix, NO,
				match_radius)
			else
		            call pt_delpt (NULL, wx, wy, Memr[xpos], Memr[ypos],
			        Memr[x], Memr[y], deleted, npix, NO,
				match_radius)
		    } else
		        call printf (
			"The x or y coordinate data is undefined\n")
		}

	    # Undelete a point marked for deletion.
	    case 'u':
		if (curtype == 'g') {
		    if ((xyinvalid == YES) || (plottype != PX_XYPLOT))
			call printf (
			    "Invalid plot type for undeleting points\n")
		    else
		        call pt_delpt (gd, wx, wy, Memr[x], Memr[y], Memr[x],
			    Memr[y], deleted, npix, YES, INDEFR)
		} else if (curtype == 'i') {
		    if (cooinvalid == NO) {
		        if (xyinvalid == NO && plottype == PX_XYPLOT)
		            call pt_delpt (gd, wx, wy, Memr[xpos], Memr[ypos],
			         Memr[x], Memr[y], deleted, npix, YES,
				 match_radius)
			else
		            call pt_delpt (NULL, wx, wy, Memr[xpos], Memr[ypos],
			         Memr[x], Memr[y], deleted, npix, YES,
				 match_radius)
		    } else
		        call printf (
			    "The x and y coordinate data is undefined\n")
		}

	    # Toggle the delete/undelete function
	    case 't':
		if (undelete == NO) {
		    call printf ("Now undeleting points\n")
		    undelete = YES
		} else if (undelete == YES) {
		    call printf ("Now deleting points\n")
		    undelete = NO
		}

	    # Mark for deletion points with X < X (cursor).
	    case '(':
		if (curtype == 'g') {
		    if ((xyinvalid == NO) && (plottype == PX_XYPLOT)) {
		        call pt_dxltg (gd, wx, Memr[x], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    } else if ((hinvalid == NO) && plottype == PX_HISTPLOT) {
		        call pt_dxltg (NULL, wx, Memr[h], Memr[h], Memr[h],
			    deleted, npix, undelete)
			newplot = YES
		    } else
			call printf (
			    "Invalid plot type for (un)deleting points\n")
		} else if (curtype == 'i') {
		    if (cooinvalid == YES) {
		        call printf (
			    "The x or y coordinate data is undefined\n")
		    } else if ((xyinvalid == NO) && (plottype == PX_XYPLOT)) {
		        call pt_dxltg (gd, wx, Memr[xpos], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    } else {
		        call pt_dxltg (NULL, wx, Memr[xpos], Memr[x], Memr[y],
			    deleted, npix, undelete)
			if (plottype == PX_XYPLOT || plottype == PX_HISTPLOT)
			    newplot = YES
			else
			    call printf (
			    "Replot x-y or histogram to show (un)deletions\n")
		    }
		}
		if (newplot == YES)
		    goto replot_

	    # Mark for deletion points with X > X (cursor).
	    case ')':
		if (curtype == 'g') {
		    if ((xyinvalid == NO) && (plottype == PX_XYPLOT)) {
		        call pt_dxgtg (gd, wx, Memr[x],  Memr[x], Memr[y],
			    deleted, npix, undelete)
		    } else if ((hinvalid == NO) && (plottype == PX_HISTPLOT)) {
		        call pt_dxgtg (NULL, wx, Memr[h], Memr[h], Memr[h],
			    deleted, npix, undelete)
		        newplot = YES
		    } else
			call printf (
			    "Invalid plot type for (un)deleting points\n")
		} else if (curtype == 'i') {
		    if (cooinvalid == YES) {
		        call printf (
			"The x and y coordinate data is undefined\n")
		    } else if ((xyinvalid == NO) && (plottype == PX_XYPLOT)) {
		        call pt_dxgtg (gd, wx, Memr[xpos], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    } else {
		        call pt_dxgtg (NULL, wx, Memr[xpos], Memr[x], Memr[y],
			    deleted, npix, undelete)
			if (plottype == PX_XYPLOT || plottype == PX_HISTPLOT)
			    newplot = YES
			else
			    call printf (
			"Replot x-y or histogram to show (un)deletions\n")
		    }
		}
		if (newplot == YES)
		    goto replot_

	    # Mark for deletion points with Y  > Y (cursor).
	    case '^':
		if (curtype == 'g') {
		    if ((xyinvalid == NO) && (plottype == PX_XYPLOT))
		        call pt_dygtg (gd, wy, Memr[y], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    else
			call printf (
			    "Invalid plot type for (un)deleting points\n")
		} else if (curtype == 'i') {
		    if (cooinvalid == YES)
		        call printf (
			"The x and y coordinate data is undefined\n")
		    else if ((xyinvalid == NO) && (plottype == PX_XYPLOT))
		        call pt_dygtg (gd, wy, Memr[ypos], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    else {
		        call pt_dygtg (NULL, wy, Memr[ypos], Memr[x], Memr[y],
			    deleted, npix, undelete)
			if (plottype == PX_XYPLOT || plottype == PX_HISTPLOT)
			    newplot = YES
			else
			    call printf (
			    "Replot x-y or histogram to show (un)deletions\n")
		    }
		}
		if (newplot == YES)
		    goto replot_

	    # Mark for deletion points with Y < Y (cursor).
	    case 'v':
		if (curtype == 'g') {
		    if ((xyinvalid == NO) && (plottype == PX_XYPLOT))
		        call pt_dyltg (gd, wy, Memr[y], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    else
			call printf (
			    "Invalid plot type for (un)deleting points\n")
		} else if (curtype == 'i') {
		    if (cooinvalid == YES)
		        call printf (
			    "The x and y coordinate data is undefined\n")
		    else if ((xyinvalid == NO) && (plottype == PX_XYPLOT))
		        call pt_dyltg (gd, wy, Memr[ypos], Memr[x], Memr[y],
			    deleted, npix, undelete)
		    else {
		        call pt_dyltg (NULL, wy, Memr[ypos], Memr[x], Memr[y],
			    deleted, npix, undelete)
			if (plottype == PX_XYPLOT || plottype == PX_HISTPLOT)
			    newplot = YES
			else
			    call printf (
			"Replot x-y or histogram to show (un)deletions\n")
		    }
		}
		if (newplot == YES)
		    goto replot_

	    # Mark points for deletion inside a box.
	    case 'b':
	        if (curtype == 'g') {
		    if ((xyinvalid == NO) && (plottype == PX_XYPLOT)) {
		        twx = wx; twy = wy
		        call printf ("again:\n")
		        if (pt_gcur (curtype,  wx, wy, key, Memc[cmd],
			    SZ_LINE) == EOF)
		            next
		        call pt_dboxg (gd, Memr[x], Memr[y], Memr[x], Memr[y],
		            deleted, npix, twx, twy, wx, wy, undelete)
		    } else
		        call printf (
			    "Invalid plot type for (un)deleting points\n")
	        } else if (curtype == 'i') {
		    if (cooinvalid == YES)
		        call printf (
			    "The x or y coordinate data is undefined\n")
		    else {
		        twx = wx; twy = wy
		        call printf ("again:\n")
		        if (pt_gcur (curtype,  wx, wy, key, Memc[cmd],
			    SZ_LINE) == EOF)
		            next
			if ((xyinvalid == NO) && (plottype == PX_XYPLOT))
		            call pt_dboxg (gd, Memr[xpos], Memr[ypos], Memr[x],
			        Memr[y], deleted, npix, twx, twy, wx, wy,
				undelete)
			else {
		            call pt_dboxg (NULL, Memr[xpos], Memr[ypos],
			        Memr[x], Memr[y], deleted, npix, twx, twy,
				wx, wy, undelete)
			    if ((plottype == PX_XYPLOT) || 
			        (plottype == PX_HISTPLOT))
			        newplot = YES
			    else
			        call printf (
			    "Replot x-y or histogram to show (un)deletions\n")
			}
		    }
	        }

	    # Colon commands:
	    case ':':
		iferr (call pt_colon (px, gd, Memc[cmd], newdata, newxy,
		    newhist, newcoo, newplot, plottype, undelete))
		    call erract (EA_WARN)
		if (newplot == YES)
		    goto replot_

	    default:
		call printf ("Unknown or ambiguous keystroke command\007\n")
	    }
	}

	call sfree (sp)

	return (status)
end


# PT_PRINT -- Print a 10 by 10 box of pixel values around the star.
 
procedure pt_print (gd, im, x, y)
 
pointer	gd			# pointer to the graphics stream
pointer	im			# pointer to the input image
real	x, y			# center of box
 
int	i, j, x1, x2, y1, y2, nx
pointer	data
pointer	pt_gdata()
 
begin
	if (gd != NULL)
	    call gdeactivate (gd, 0)

	# Check that the image is defined.
	if (im == NULL) {
	    call printf ("The input image is undefined\n")
	    return
	}

	# Check that the center is defined.
	if (IS_INDEFR(x) || IS_INDEFR(y)) {
	    call printf ("The box center is undefined\n")
	    return
	}

	x1 = x - 5 + 0.5
	x2 = x + 5 + 0.5
	y1 = y - 5 + 0.5
	y2 = y + 5 + 0.5
	data = pt_gdata (im, x1, x2, y1, y2) 
	if (data == NULL) {
	    call printf ("The requested image section is off the image\n")
	    return
	}
	nx = x2 - x1 + 1

	call printf ("\n%4w") 
	do i = x1, x2 {
	    call printf (" %4d ")
		call pargi (i)
	}
	call printf ("\n")
 
	do j = y2, y1, -1 {
	    call printf ("%4d")
		call pargi (j)
	    do i = x1, x2 {
		call printf (" %5g")
		    call pargr (Memr[data+(j-y1)*nx+(i-x1)])
	    }
	    call printf ("\n")
	}
	call printf ("\n")

	if (gd != NULL)
	    call greactivate (gd, 0)
end


define	LEN_TYPESTR	9

# PT_COLINFO -- Print the name, type and units of all the columns in the
# input catalog.

procedure pt_colinfo (gd, apd, key)

pointer	gd		# pointer to the graphics stream
pointer	apd		# the file descriptor for the input catalog
int	key		# the key structure for text catalogs

int	len_colname, i, datatype, numelems
pointer	tty, sp, name, units, type, junk, colptr
int	tbpsta(), tbcigi(), pt_gnfn(), pt_kstati()
pointer	ttyodes(), tbcnum(), pt_ofnl()

begin
	if (gd != NULL)
	    call gdeactivate (gd, AW_CLEAR)
	else {
	    tty = ttyodes ("terminal")
	    call ttyclear (STDOUT, tty)
	}

	len_colname = max (SZ_COLNAME, KY_SZPAR)
	call printf ("\n%*.*s  %*.*s  %s\n\n")
	    call pargi (-len_colname)
	    call pargi (len_colname)
	    call pargstr ("COLUMN")
	    call pargi (-LEN_TYPESTR)
	    call pargi (LEN_TYPESTR)
	    call pargstr ("TYPE")
	    call pargstr ("UNITS")

	call smark (sp)
	call salloc (name, len_colname, TY_CHAR)
	call salloc (units, max (SZ_COLUNITS, KY_SZPAR), TY_CHAR)
	call salloc (type, LEN_TYPESTR, TY_CHAR)

	if (key == NULL) {

	    do i = 1, tbpsta (apd, TBL_NCOLS) {

		colptr = tbcnum (apd, i)
		call tbcigt (colptr, TBL_COL_NAME, Memc[name], SZ_COLNAME)
		call tbcigt (colptr, TBL_COL_UNITS, Memc[units], SZ_COLUNITS)
		datatype = tbcigi (colptr, TBL_COL_DATATYPE)
		switch (datatype) {
		case TY_BOOL:
		    call strcpy ("boolean", Memc[type], LEN_TYPESTR)
		case TY_SHORT, TY_INT, TY_LONG:
		    call strcpy ("integer", Memc[type], LEN_TYPESTR)
		case TY_REAL:
		    call strcpy ("real", Memc[type], LEN_TYPESTR)
		case TY_DOUBLE:
		    call strcpy ("double", Memc[type], LEN_TYPESTR)
		default:
		    if (datatype < 0)
		        call strcpy ("character", Memc[type], LEN_TYPESTR)
		    else
		        call strcpy ("undefined", Memc[type], LEN_TYPESTR)
		}

		call printf ("%*.*s  %*.*s  %s\n")
		    call pargi (-len_colname)
		    call pargi (len_colname)
		    call pargstr (Memc[name])
		    call pargi (-LEN_TYPESTR)
		    call pargi (LEN_TYPESTR)
		    call pargstr (Memc[type])
		    call pargstr (Memc[units])
	    }

	} else {

	    call salloc (junk, SZ_LINE, TY_CHAR)
	    colptr = pt_ofnl (key, "*")
	    while (pt_gnfn (colptr, Memc[name], Memc[junk], KY_SZPAR) != EOF) {
		#if (pt_kstati (key, Memc[name], KY_INDEX) <= KY_NOKEYS(key))
		if (pt_kstati (key, Memc[name], KY_INDEX) <= KY_NPKEYS(key))
		    next
		numelems = pt_kstati (key, Memc[name], KY_NUMELEMS)
		call pt_kstats (key, Memc[name], KY_UNITSTR, Memc[units],
		    KY_SZPAR)
		datatype = pt_kstati (key, Memc[name], KY_DATATYPE)
		switch (datatype) {
		case TY_BOOL:
		    call strcpy ("boolean", Memc[type], LEN_TYPESTR)
		case TY_CHAR:
		    call strcpy ("character", Memc[type], LEN_TYPESTR)
		case TY_INT:
		    call strcpy ("integer", Memc[type], LEN_TYPESTR)
		case TY_REAL:
		    call strcpy ("real", Memc[type], LEN_TYPESTR)
		default:
		    call strcpy ("undefined", Memc[type], LEN_TYPESTR)
		}

	        do i = 1, numelems {
		    if (numelems == 1)
			call strcpy (Memc[name], Memc[junk], KY_SZPAR)
		    else {
			call sprintf (Memc[junk], KY_SZPAR, "%s[%d]")
			    call pargstr (Memc[name])
			    call pargi (numelems)
		    }
	            call printf ("%*.*s  %*.*s  %s\n")
		        call pargi (-len_colname)
		        call pargi (len_colname)
		        call pargstr (Memc[junk])
		        call pargi (-LEN_TYPESTR)
		        call pargi (LEN_TYPESTR)
		        call pargstr (Memc[type])
		        call pargstr (Memc[units])
	        }

	    }
	    call pt_cfnl (colptr)

	}
	call printf ("\n")

	call sfree (sp)

	if (gd != NULL)
	    call greactivate (gd, AW_PAUSE)
	else
	    call ttycdes (tty)
end


# PT_GLDATA -- List the values of the loaded columns for this point.

int procedure pt_gldata (gd, px, apd, key, wx, wy, x, y, npix, matchrad)

pointer	gd		# pointer to the graphics stream
pointer	px		# pointer to the pexamine structure	
int	apd		# file descriptor for catalog
pointer	key		# pointer to the key structure for text files
real	wx		# the graphics cursor x coordinate
real	wy		# the graphics cursor y coordinate
real	x[ARB]		# the array of x plotted data
real	y[ARB]		# the array of y plotted data
int	npix		# the number of points
real	matchrad	# matching radius

int	row, ip, ncol
pointer	sp, name, units, colptr
int	pt_getnames(), pt_fstarg()

begin
	if (gd != NULL)
	    call gdeactivate (gd, 0)

	# Find the star.
	row = pt_fstarg (gd, wx, wy, x, y, npix, matchrad)

	# List the values of the loaded columns.
	if (row != 0) {

	    call smark (sp)
	    call salloc (name, PX_SZCOLNAME, TY_CHAR)
	    call salloc (units, max (KY_SZPAR, SZ_COLUNITS), TY_CHAR)

	    call printf ("\n%*.*s  %s  (%s)\n\n")
		call pargi (-PX_SZCOLNAME)
		call pargi (PX_SZCOLNAME)
		call pargstr ("COLUMN")
		call pargstr ("VALUE")
		call pargstr ("UNITS")

	    ip = 1
	    ncol = 0
	    while (pt_getnames (Memc[PX_COLNAMES(px)], ip, Memc[name], 
	        PX_SZCOLNAME) != EOF) {
		if (PX_COLPTRS(px) == NULL)
		    next
		if (key == NULL) {
		    call tbcfnd (apd, Memc[name], colptr, 1)
		    call tbcigt (colptr, TBL_COL_UNITS, Memc[units],
		        SZ_COLUNITS)
		} else
		    call pt_kstats (key, Memc[name], KY_UNITSTR, Memc[units],
		        KY_SZPAR)
		call printf ("%*.*s  %g  (%s)\n")
		    call pargi (-PX_SZCOLNAME)
		    call pargi (PX_SZCOLNAME)
		    call pargstr (Memc[name])
		    call pargr (Memr[Memi[PX_COLPTRS(px)+ncol]+row-1])
		    call pargstr (Memc[units])
		ncol = ncol + 1
	    }
	    call printf ("\n")

	    call sfree (sp)
	}

	if (gd != NULL)
	    call greactivate (gd, 0)

	return (row)
end


# PT_XYPLOT -- Plot the x and y points.

procedure pt_xyplot (gd, x, y, deleted, npix, title, xlabel, ylabel)

pointer	gd		# pointer to the graphics stream
real	x[ARB]		# array of x coordinates
real	y[ARB]		# array of y coordinates
int	deleted[ARB]	# deletions array
int	npix		# number of points
char	title[ARB]	# title of the plot
char	xlabel[ARB]	# x axis label
char	ylabel[ARB]	# y axis label

int	i, marker_type
pointer	sp, marker, longtitle
real	szmarker, x1, x2, y1, y2, dmin, dmax
bool	clgetb()
int	clgeti(), strlen()
real	clgetr()

begin
	# Check for undefined graphis stream.
	if (gd == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Allocate working space.
	call smark (sp)
	call salloc (marker, SZ_FNAME, TY_CHAR)
	call salloc (longtitle, 2 * SZ_LINE, TY_CHAR)

	# Clear the plotting strucuture.
	call gclear (gd)

	# Fetch the window and viewport parameters.
	x1 = clgetr ("xyplot.x1")
	x2 = clgetr ("xyplot.x2")
	y1 = clgetr ("xyplot.y1")
	y2 = clgetr ("xyplot.y2")
	if (IS_INDEFR(x1) || IS_INDEFR(x2)) {
	    call pt_alimr (x, npix, dmin, dmax)
	    if (IS_INDEFR(x1))
		x1 = dmin - FRACTION * (dmax - dmin)
	    if (IS_INDEFR(x2))
		x2 = dmax + FRACTION * (dmax - dmin)
	}
	if (IS_INDEFR(y1) || IS_INDEFR(y2)) {
	    call pt_alimr (y, npix, dmin, dmax)
	    if (IS_INDEFR(y1))
		y1 = dmin - FRACTION * (dmax - dmin)
	    if (IS_INDEFR(y2))
		y2 = dmax + FRACTION * (dmax - dmin)
	}

	# Set the scale of the axes.
	call gswind (gd, x1, x2, y1, y2)
	if (clgetb ("xyplot.logx"))
	    call gseti (gd, G_XTRAN, GW_LOG)
	else
	    call gseti (gd, G_XTRAN, GW_LINEAR)
	if (clgetb ("xyplot.logy"))
	    call gseti (gd, G_YTRAN, GW_LOG)
	else
	    call gseti (gd, G_YTRAN, GW_LINEAR)

	# Get the x and y axes parameters.
	if (! clgetb ("xyplot.fill"))
	    call gseti (gd, G_ASPECT, 1)
	if (clgetb ("xyplot.round"))
	    call gseti (gd, G_ROUND, YES)

	# Get the axis drawing parameters. 
	if (clgetb ("xyplot.box")) {

	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("xyplot.majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("xyplot.minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("xyplot.majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("xyplot.minry"))

	    # Label tick marks on axes.
	    if (clgetb ("xyplot.ticklabels"))
	        call gseti (gd, G_LABELTICKS, YES)
	    else
	        call gseti (gd, G_LABELTICKS, NO)

	    # Draw grid.
	    if (clgetb ("xyplot.grid"))
	        call gseti (gd, G_DRAWGRID, YES)
	    else
	        call gseti (gd, G_DRAWGRID, NO)

	    # Optionally draw a box around the plot.
	    if (clgetb ("xyplot.banner")) {
		call sysid (Memc[longtitle], 2 * SZ_LINE)
		call sprintf (Memc[longtitle+strlen(Memc[longtitle])],
		    2 * SZ_LINE, "\n%s") 
		    call pargstr (title)
	    } else
		call strcpy (title, Memc[longtitle], 2 * SZ_LINE)
	    call glabax (gd, Memc[longtitle], xlabel, ylabel)

	}

	# Get the marker type, the size of the marker and the linewidth.
	call clgstr ("xyplot.marker", Memc[marker], SZ_FNAME)
	call pt_marker (Memc[marker], SZ_FNAME, marker_type)
	if (marker_type != GM_POINT)
	    szmarker = clgetr ("xyplot.szmarker")
	else
	    szmarker  = 0.0
	call gsetr (gd, G_PLWIDTH, 2.0)

	# Draw the points in using the deletions array.
	do i = 1, npix {
	    if (deleted[i] == PX_DELETE)
		next
	    call gmark (gd, x[i], y[i], marker_type, szmarker, szmarker)
	}

	# Overplot crosses for those new points marked for deletion.
	call pt_mdelete (gd, x, y, deleted, npix)

	call gflush (gd)
	call sfree (sp)
end


# PT_HPLOT -- Compute and plot histogram.

procedure pt_hplot (gd, x, deleted, npix, title, xlabel, ylabel)

pointer	gd		# pointer to the graphics stream
real	x[ARB]		# array of points to be made into a histogram
int	deleted[ARB]	# deletions array
int	npix		# number of pixels
char	title[ARB]	# the title of the plot
char	xlabel[ARB]	# the x axis label
char	ylabel[ARB]	# the y axis label

int	i, j, nbins, nbins1
pointer	sp, hgm, xp, yp, longtitle
real	z1, z2, dz, x1, x2, y1, y2, dmin, dmax
bool	clgetb(), fp_equalr()
int	clgeti(), strlen()
real	clgetr()

begin
	# Check for undefined graphis stream.
	if (gd == NULL) {
	    call printf ("The graphics device is undefined\n")
	    return
	}

	# Get default histogram resolution and range.
	nbins = clgeti ("histplot.nbins")
	z1 = clgetr ("histplot.z1")
	z2 = clgetr ("histplot.z2")

	# Use data limits for INDEF limits.
	if (IS_INDEFR(z1) || IS_INDEFR(z2)) {
	    call pt_alimr (x, npix, dmin, dmax)
	    if (IS_INDEFR(z1))
		z1 = dmin
	    if (IS_INDEFR(z2))
		z2 = dmax
	}
	if (z1 > z2) {
	    dz = z1
	    z1 = z2
	    z2 = dz
	}

	# Test for constant valued image, which causes zero divide in ahgm.
	if (fp_equalr (z1, z2)) {
	    call printf ("Warning: Histogram has no data range.\n")
	    return
	}

	# The extra bin counts the pixels that equal z2 and shifts the
	# remaining bins to evenly cover the interval [z1,z2].
	# Note that real numbers could be handled better - perhaps
	# adjust z2 upward by ~ EPSILONR (in ahgm itself).

	nbins1 = nbins + 1

	# Initialize the histogram buffer and image line vector.
	call smark (sp)
	call salloc (hgm, nbins1, TY_INT)
	call aclri  (Memi[hgm], nbins1)

	# Accumulate the histogram. Add the ability to use the deletions
	# array in future.
	call pt_ahgmr (x, deleted, npix, Memi[hgm], nbins1, z1, z2)

	# "Correct" the topmost bin for pixels that equal z2.  Each
	# histogram bin really wants to be half open.

	if (clgetb ("histplot.top_closed"))
	    Memi[hgm+nbins-1] = Memi[hgm+nbins-1] + Memi[hgm+nbins1-1]

	# List or plot the histogram.  In list format, the bin value is the
	# z value of the left side (start) of the bin.

	dz = (z2 - z1) / real (nbins)

	# Plot the histogram in box mode.
	nbins1 = 2 * nbins + 2
	call salloc (xp, nbins1, TY_REAL)
	call salloc (yp, nbins1, TY_REAL)
	Memr[xp] = z1
	Memr[yp] = 0.0
	j = 1
	do i = 0, nbins - 1 {
	    Memr[xp+j] = Memr[xp+j-1]
	    Memr[yp+j] = Memi[hgm+i]
	    j = j + 1
	    Memr[xp+j] = Memr[xp+j-1] + dz
	    Memr[yp+j] = Memr[yp+j-1]
	    j = j + 1
	}
	Memr[xp+j] = Memr[xp+j-1]
	Memr[yp+j] = 0.0

	# Construct the title.
	call salloc (longtitle, 2 * SZ_LINE, TY_CHAR)
	if (clgetb ("histplot.banner")) {
	    call sysid (Memc[longtitle], 2 * SZ_LINE)
	    call sprintf (Memc[longtitle+strlen(Memc[longtitle])], 2 * SZ_LINE, 
	        "\nHistogram from z1=%g to z2=%g, nbins=%d\n%s")
	        call pargr (z1)
	        call pargr (z2)
	        call pargi (nbins)
	        call pargstr (title)
	} else {
	    call sprintf (Memc[longtitle], 2 * SZ_LINE, 
	        "Histogram from z1=%g to z2=%g, nbins=%d\n%s")
	        call pargr (z1)
	        call pargr (z2)
	        call pargi (nbins)
	        call pargstr (title)
	}

	# Clear the screen.
	call gclear (gd)

	# Compute the data window to be plotted.
	x1 = clgetr ("histplot.x1")
	x2 = clgetr ("histplot.x2")
	if (IS_INDEFR(x1) || IS_INDEFR(x2)) {
	    call alimr (Memr[xp], nbins1, dmin, dmax)
	    if (IS_INDEFR(x1))
		x1 = dmin
	    if (IS_INDEFR(x2))
		x2 = dmax
	}
	y1 = clgetr ("histplot.y1")
	y2 = clgetr ("histplot.y2")
	if (IS_INDEFR(y1) || IS_INDEFR(y2)) {
	    call alimr (Memr[yp], nbins1, dmin, dmax)
	    if (IS_INDEFR(y1))
		y1 = dmin
	    if (IS_INDEFR(y2))
		y2 = dmax
	}

	# Set the scale of the axes.
	call gswind (gd, x1, x2, y1, y2)
	call gseti (gd, G_XTRAN, GW_LINEAR)
	if (clgetb ("histplot.logy"))
	    call gseti (gd, G_YTRAN, GW_LOG)
	else
	    call gseti (gd, G_YTRAN, GW_LINEAR)

	if (! clgetb ("xyplot.fill"))
	    call gseti (gd, G_ASPECT, 1)
	if (clgetb ("xyplot.round"))
	    call gseti (gd, G_ROUND, YES)
	call gsetr (gd, G_PLWIDTH, 2.0)

	# Draw a box around the axes.
	if (clgetb ("histplot.box")) {

	    # Label tick marks on axes.
	    if (clgetb ("histplot.ticklabels"))
	        call gseti (gd, G_LABELTICKS, YES)
	    else
	        call gseti (gd, G_LABELTICKS, NO)

	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("histplot.majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("histplot.minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("histplot.majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("histplot.minry"))

	    # Draw the axes.
	    call glabax (gd, Memc[longtitle], xlabel, ylabel)

	}

	# Draw the historgram.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gpline (gd, Memr[xp], Memr[yp], nbins1)
	call gflush (gd)

	call sfree (sp)
end


# PT_XYINFO -- Get the title and axes labels for the X-Y plot.

procedure pt_xyinfo (px, tp, key, title, max_sztitle, xlabel, ylabel,
	max_szlabel)

pointer	px			# pointer to the pexamine strcuture
pointer	tp			# input catalog file descriptor
pointer	key			# pointer to key structure for text files
char	title[ARB]		# title for the plot
int	max_sztitle		# maximum length of the title
char	xlabel[ARB]		# X axis label
char	ylabel[ARB]		# Y axis label
int	max_szlabel		# maximum size of the label

pointer	sp, label, units, cd

begin
	call smark (sp)
	call salloc (label, max_szlabel, TY_CHAR)
	call salloc (units, max_szlabel, TY_CHAR)

	# Get the title.

	# Get the x and y labels for the columns.
	if (key == NULL) {

	    call tbtnam (tp, title, max_sztitle)

	    call tbcfnd (tp, PX_XCOLNAME(px), cd, 1)
	    if (cd != NULL) {
		call strcpy (PX_XCOLNAME(px), Memc[label], max_szlabel)
		call tbcigt (cd, TBL_COL_UNITS, Memc[units], SZ_COLUNITS)
	    } else {
		call strcpy ("UNDEFINED", Memc[label], max_szlabel)
		Memc[units] = EOS
	    }
	    call sprintf (xlabel, max_szlabel, "%s in %s")
		call pargstr (Memc[label])
		call pargstr (Memc[units])

	    call tbcfnd (tp, PX_YCOLNAME(px), cd, 1)
	    if (cd != NULL) {
		call strcpy (PX_YCOLNAME(px), Memc[label], max_szlabel)
		call tbcigt (cd, TBL_COL_UNITS, Memc[units], SZ_COLUNITS)
	    } else {
		call strcpy ("UNDEFINED", Memc[label], max_szlabel)
		Memc[units] = EOS
	    }
	    call sprintf (ylabel, max_szlabel, "%s in %s")
		call pargstr (Memc[label])
		call pargstr (Memc[units])

	} else {

	    call fstats (tp, F_FILENAME, title, max_sztitle)

	    if (PX_XCOLNAME(px) == EOS) {
		call strcpy ("UNDEFINED", Memc[label], max_szlabel)
		Memc[units] = EOS
	    } else {
		call strcpy (PX_XCOLNAME(px), Memc[label], max_szlabel)
		call pt_kstats (key, PX_XCOLNAME(px), KY_UNITSTR, Memc[units],
		    max_szlabel)
	    }
	    call sprintf (xlabel, max_szlabel, "%s %s")
		call pargstr (Memc[label])
		call pargstr (Memc[units])

	    if (PX_YCOLNAME(px) == EOS) {
		call strcpy ("UNDEFINED", Memc[label], max_szlabel)
		Memc[units] = EOS
	    } else {
		call strcpy (PX_YCOLNAME(px), Memc[label], max_szlabel)
		call pt_kstats (key, PX_YCOLNAME(px), KY_UNITSTR, Memc[units],
		    max_szlabel)
	    }
	    call sprintf (ylabel, max_szlabel, "%s %s")
		call pargstr (Memc[label])
		call pargstr (Memc[units])

	}

	call sfree (sp)
end


# PT_HINFO -- Get the title and axes labels for the histogram plot.

procedure pt_hinfo (px, tp, key, title, max_sztitle, xlabel, ylabel,
	max_szlabel)

pointer	px			# pointer to the pexamine strcuture
pointer	tp			# input catalog file descriptor
pointer	key			# pointer to key structure for text files
char	title[ARB]		# title for the plot
int	max_sztitle		# maximum length of the title
char	xlabel[ARB]		# X axis label
char	ylabel[ARB]		# Y axis label
int	max_szlabel		# maximum size of the label

pointer	sp, label, units, cd

begin
	call smark (sp)
	call salloc (label, max_szlabel, TY_CHAR)
	call salloc (units, max_szlabel, TY_CHAR)

	# Get the title.

	# Get the x and y labels for the columns.
	if (key == NULL) {

	    call tbtnam (tp, title, max_sztitle)

	    call tbcfnd (tp, PX_HCOLNAME(px), cd, 1)
	    if (cd != NULL) {
		call strcpy (PX_HCOLNAME(px), Memc[label], max_szlabel)
		call tbcigt (cd, TBL_COL_UNITS, Memc[units], SZ_COLUNITS)
	    } else {
		call strcpy ("UNDEFINED", Memc[label], max_szlabel)
		Memc[units] = EOS
	    }
	    call sprintf (xlabel, max_szlabel, "%s in %s")
		call pargstr (Memc[label])
		call pargstr (Memc[units])

	    call sprintf (ylabel, max_szlabel, "N(%s)")
		call pargstr (Memc[label])

	} else {

	    call fstats (tp, F_FILENAME, title, max_sztitle)

	    if (PX_HCOLNAME(px) == EOS) {
		call strcpy ("UNDEFINED", Memc[label], max_szlabel)
		Memc[units] = EOS
	    } else {
		call strcpy (PX_HCOLNAME(px), Memc[label], max_szlabel)
		call pt_kstats (key, PX_HCOLNAME(px), KY_UNITSTR, Memc[units],
		    max_szlabel)
	    }
	    call sprintf (xlabel, max_szlabel, "%s %s")
		call pargstr (Memc[label])
		call pargstr (Memc[units])

	    call sprintf (ylabel, max_szlabel, "N(%s)")
		call pargstr (Memc[label])

	}

	call sfree (sp)
end

 
# PT_GCUR -- Get PEXAMINE cursor value.
# This is an interface between the standard cursor input and PEXAMINE.
# It reads the appropriate cursor, makes the appropriate default
# coordinate conversions when using graphics cursor input, and gets any
# further cursor reads needed.  Missing coordinates default to the last
# coordinates.
 
int procedure pt_gcur (curtype, x, y, key, strval, maxch)
 
int	curtype			# cursor type
real	x, y			# cursor position
int	key			# keystroke value of cursor event
char	strval[ARB]		# string value, if any
int	maxch			# max chars out
 
char	ch
int	nitems, wcs, ip
int	clgcur(), ctor(), cctoc()
errchk	clgcur
 
begin
	# Initialize.
	strval[1] = EOS

	# Get a cursor values from the desired cursor parameter.
	switch (curtype) {
	case 'i':
	    nitems = clgcur ("icommands", x, y, wcs, key, strval, maxch)
	case 'g':
	    nitems = clgcur ("gcommands", x, y, wcs, key, strval, maxch)
	}

	call flush (STDOUT)

	# Map numeric colon sequences (: x [y] key strval) to make them appear
	# as ordinary "x y key" type cursor reads.  This makes it possible for
	# the user to access any command using typed in rather than positional
	# cursor coordinates.  Special treatment is also given to the syntax
	# ":lN" and ":cN", provided for compatibility with IMPLOT for simple
	# line and column plots.

	if (key == ':') {
	    for (ip=1;  IS_WHITE(strval[ip]);  ip=ip+1)
		;
	    if (IS_DIGIT(strval[ip])) {
		if (ctor (strval, ip, x) <= 0)
		    ;
		if (ctor (strval, ip, y) <= 0)
		    y = x
		for (;  IS_WHITE(strval[ip]);  ip=ip+1)
		    ;
		if (cctoc (strval, ip, ch) > 0)
		    key = ch
		call strcpy (strval[ip], strval, maxch)

	    } 
	}
 
	return (nitems)
end


# PT_FSTARG -- Find the the point data point nearest the input position.

int procedure pt_fstarg (gd, wx, wy, x, y, npix, matchrad)

pointer	gd		# pointer to the graphics descriptor
real	wx		# X cursor position
real	wy		# Y cursor position
real	x[ARB]		# X array of plotted data
real	y[ARB]		# Y array of plotted data
int	npix		# number of pixels
real	matchrad	# the matching radius

int	i, row
real	r2min, r2, mr2, wx0, wy0, x0, y0

begin
	row = 0
	r2min = MAX_REAL
	if (IS_INDEFR(matchrad)) {
	    mr2 = MAX_REAL
	    if (gd != NULL)
		call gctran (gd, wx, wy, wx0, wy0, 1, 0)
	    else {
	        wx0 = wx
	        wy0 = wy
	    }
	} else {
	    mr2 = matchrad ** 2
	    wx0 = wx
	    wy0 = wy
	}

	# Search for the nearest point.
	do i = 1 , npix {
	    if (! IS_INDEFR(x[i]) && ! IS_INDEFR(y[i])) {
		if (IS_INDEFR(matchrad)) {
		    if (gd != NULL)
			call gctran (gd, x[i], y[i], x0, y0, 1, 0)
		    else {
		        x0 = x[i]
		        y0 = y[i]
		    }
		} else {
		    x0 = x[i]
		    y0 = y[i]
		}
	        r2 = (wx0 - x0) ** 2 + (wy0 - y0) ** 2
	    } else
		r2 = MAX_REAL
	    if (r2 >= r2min)
		next
	    r2min = r2
	    row = i
	}

	if ((row != 0) && (r2min <= mr2))
	    return (row)
	else
	    return (0)
end


# PT_MARKER -- Return an integer code for the marker type string.

procedure pt_marker (marker, maxch, imark)

char	marker[ARB]		# string defining the marker type
int	maxch			# maximum length of the marker name
int	imark			# the integer code for the marker

int	i
int	strdic()

begin
	i = strdic (marker, marker, maxch, PX_MARKERS)
	switch (i) {
	case 1:
	    imark = GM_POINT
	case 2:
	    imark = GM_BOX
	case 3:
	    imark = GM_PLUS
	case 4:
	    imark = GM_CROSS
	case 5:
	    imark = GM_CIRCLE
	case 6:
	    imark = GM_HLINE
	case 7:
	    imark = GM_VLINE
	case 8:
	    imark = GM_DIAMOND
	default:
	    imark = GM_BOX
	    call strcpy ("box", marker, maxch)
	}
end
