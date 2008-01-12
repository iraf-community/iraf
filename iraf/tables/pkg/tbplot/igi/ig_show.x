include "igi.h"

#  IG_SHOW -- Format and print the values of the plot parameters.  Write
#  the status to a temporary file and display it with the cl pager. 

#  8/20/91 Removed ^Ls. ZGL
#  9/17/91 Added image name and made other cosmetic changes.  ZGL
## 12 June 1992  Check for redirected STDOUT and don't page temp file.  ZGL
## 7/10/92 Added stuff like color, Z buffer, font set, etc.  ZGL
## 7/27/92 Add Z data range, fill pattern.  ZGL
## 8/10/92 Minor cosmetic changes.  ZGL
## 1/27/93 Fix INDEF tests.
## 7/13/93 Add version and date to listing

procedure ig_show (igs)

pointer	igs			# igi parameters structure

pointer	igps			# igi plot parameters structure
pointer	sp, strcode, tempfile
int	sf
real	vl, vr, vb, vt

int	open()

begin
	igps = PLOT_PARMS(igs)

	call smark  (sp)
	call salloc (strcode, SZ_LINE, TY_CHAR)

	if (STDOUT_REDIR(igs) == YES)
	    sf = STDOUT

	else {
	    call salloc (tempfile, SZ_FNAME, TY_CHAR)
	    call mktemp ("tmp$igi_parms", Memc[tempfile], SZ_FNAME)
	    sf = open (Memc[tempfile], TEMP_FILE, TEXT_FILE)
	}

	call clgstr ("Version", Memc[strcode], SZ_LINE)
	call fprintf (sf, "STSDAS igi version %s\n\n")
	call pargstr (Memc[strcode])

#	if (DEBUG_OUTPUT(igs) == YES) {
	    call fprintf (sf, "Last Command:\n%s\n\n")
		call pargstr (LAST_COMMAND(igs))
#	}

	switch (MG_DATASRC(igps)) {

	case TABLE_DATA:
	    call fprintf (sf, "Input TABLE data file:\t%s\n")
		call pargstr (MG_FILE_NAME(igps))
	    if (MG_COLNAME(igps) != EOS) {
		call fprintf (sf, "\tTable column:\t%s\n")
		call pargstr (MG_COLNAME(igps))
	    }

	case TEXT_DATA:
	    call fprintf (sf, "Input TEXT data file:\t%s\n")
		call pargstr (MG_FILE_NAME(igps))
	    if (!IS_INDEFI (MG_COLNUM(igps)) && MG_COLNUM(igps) > 0) {
		call fprintf (sf, "\tColumn number:\t%d\n")
		call pargi (MG_COLNUM(igps))
	    }

	case IMAGE_DATA:
	    call fprintf (sf, "Input IMAGE file:\t%s\n")
		call pargstr (MG_FILE_NAME(igps))
	    if (MG_IMGWCS(igps) == YES)
		call fprintf (sf, "\tUsing image WCS for X data\n")
		
	default:
	    call fprintf (sf, "No input data file specified\n")
	}

	if (MG_FILE_NAME(igps) != EOS) {
	    if (IS_INDEFI (MG_FROW(igps)) && IS_INDEFI (MG_LROW(igps)))
		call fprintf (sf, "\tAll rows\n")
	    else if (IS_INDEFI (MG_FROW(igps)) && !IS_INDEFI (MG_LROW(igps))) {
		call fprintf (sf, "\tRows 1 to %d\n")
		    call pargi (MG_LROW(igps))
	    } else if (!IS_INDEFI (MG_FROW(igps)) && IS_INDEFI (MG_LROW(igps))) {
		call fprintf (sf, "\tRows %d to End of Data\n")
		    call pargi (MG_FROW(igps))
	    } else {
		call fprintf (sf, "\tRows %d to %d\n")
		    call pargi (MG_FROW(igps))
		    call pargi (MG_LROW(igps))
	    }
	}

#	call fprintf (sf, "\n")

	if (MG_XDATAP(igps) != NULL) {
	    call fprintf (sf, "%d X  ")
	    call pargi (MG_XNPTS(igps))
	}

	if (MG_YDATAP(igps) != NULL) {
	    call fprintf (sf, "%d Y  ")
	    call pargi (MG_YNPTS(igps))
	}

	if (MG_ZDATAP(igps) != NULL) {
	    call fprintf (sf, "%d x %d Z;  Min: %g, Max: %g  ")
	    call pargi (MG_ZNPTSX(igps))
	    call pargi (MG_ZNPTSY(igps))
	    call pargr (MG_ZMIN(igps))
	    call pargr (MG_ZMAX(igps))
	}

	if (MG_EDATAP(igps) != NULL) {
	    call fprintf (sf, "%d Error  ")
	    call pargi (MG_ENPTS(igps))
	}

	if (MG_PDATAP(igps) != NULL) {
	    call fprintf (sf, "%d Point style  ")
	    call pargi (MG_PNPTS(igps))
	}

	call fprintf (sf, "\n")
#	call fprintf (sf, "\nLine, Text, and Point Marker attributes\n")
	call fprintf (sf, "\n")

	call ltypic (MG_LTYPEN(igps), Memc[strcode], SZ_LINE)
	call fprintf (sf, "LTYPE\t\tLine type:\t\t\t%s (%d)\n")
	    call pargstr (Memc[strcode])
	    call pargi (MG_LTYPEN(igps))
	call fprintf (sf, "LWEIGHT\t\tRelative line weight:\t\t%.1f\n")
	    call pargr (MG_LWEIGHT(igps))
	call fprintf (sf, "EXPAND\t\tSize (expand factor):\t\t%.1f\n")
	    call pargr (MG_EXPAND(igps))
	call fprintf (sf, "ANGLE\t\tRotation angle:\t\t\t%.1f (degrees)\n")
	    call pargr (MG_ANGLE(igps))
	call fprintf (sf, "\t\tText size:\t\t\t%.3f (NDC)\n")
	    call pargr (MG_EXPAND(igps) * MG_CHARSIZE(igps))
	call justic (MG_IJUSTC(igps), Memc[strcode], SZ_LINE)
	call fprintf (sf, "JUSTIFY\t\tText justification:\t\t%s (%d)\n")
	    call pargstr (Memc[strcode])
	    call pargi (MG_IJUSTC(igps))

	call fprintf (sf, "FONTSET\t\tFont Set:\t\t\t%d, ")
	    call pargi (MG_FONTSET(igps))
	switch (MG_FONTSET(igps)) {
	case IGI_FONTS:
	    call fprintf (sf, "igi (soft) fonts")
	case GIO_FONTS:
	    call fprintf (sf, "gio (hard) fonts")
	}
	call fprintf (sf, "\n")

	if (DEBUG_OUTPUT(igs) == YES) {
	    call fprintf (sf, "\t\tBase text size:\t\t\t%5.4f (NDC)\n")
		call pargr (MG_CHARSIZE(igps))
	    call fprintf (sf, "\t\tItalic factor:\t\t\t%.3f\n")
		call pargr (MG_SLANT(igps))
	    call fprintf (sf, "\t\tSuperscript factor:\t\t%.3f\n")
		call pargr (MG_SUPFRAC(igps))
	    call fprintf (sf, "\t\tBase marker size:\t\t%5.4f (NDC)\n")
		call pargr (MG_PNTSIZE(igps))
	    call fprintf (sf, "\t\tSolid marker fill factor:\t%.3f\n")
		call pargr (MG_PNTFILL(igps))
	    call fprintf (sf, "\t\tStellar factor:\t\t\t%.3f\n")
		call pargr (MG_STELLAR(igps))
	}

	call ptypic (MG_PTYPS(igps), Memc[strcode], SZ_LINE)
	if (!IS_INDEFI (MG_PTYPN(igps)) && !IS_INDEFI (MG_PTYPS(igps))) {
	    call fprintf (sf, "PTYPE\t\tPoint style:\t\t\t%d vertices;  %s (%d)\n")
		call pargi (MG_PTYPN(igps))
		call pargstr (Memc[strcode])
		call pargi (MG_PTYPS(igps))
	} else
	    call fprintf (sf, "PTYPE\t\tNO point marker type specified\n")

	call fprintf (sf, "\t\tSymbol (point marker) size:\t%.3f (NDC)\n")
	    call pargr (MG_EXPAND(igps) * MG_PNTSIZE(igps))

	call fprintf (sf, "COLOR\t\tColor index (device-dependent)\t%d\n")
	    call pargi (MG_COLOR(igps))

	call fprintf (sf, "FILLPAT\t\tFill pattern (device-dependent)\t%d ")
	    call pargi (MG_FILLPAT(igps))

	if (MG_FILLPAT(igps) >= HATCH_FILL)
	    call fprintf (sf, "(Hatch)\n")

	else
	    switch (MG_FILLPAT(igps)) {
	    case CLEAR_FILL:
		call fprintf (sf, "(Clear)\n")
	    case HOLLOW_FILL:
		call fprintf (sf, "(Hollow)\n")
	    case SOLID_FILL:
		call fprintf (sf, "(Solid)\n")
	}

#	call fprintf (sf, "\nDevice and Plot Scaling\n")

	if (DEBUG_OUTPUT(igs) == YES) {
	    call fprintf (sf, "\n")
	    call show_window (sf, igs)
	}

	call fprintf (sf, "\n")

	if (MG_NXPANE(igps) != 1 || MG_NYPANE(igps) != 1) {
	    call fprintf (sf, "WINDOW\t\tScreen windowed:\t\t%d X %d tiles;  tile %d\n")
		call pargi (MG_NXPANE(igps))
		call pargi (MG_NYPANE(igps))
		call pargi (MG_PANE(igps))
	}

	call fprintf (sf, "VPAGE\t\tVirtual page (device):\t\t%.3f %.3f %.3f %.3f (NDC)\n")
	    call pargr (MG_PAGELEFT(igps))
	    call pargr (MG_PAGERIGHT(igps))
	    call pargr (MG_PAGEBOTTOM(igps))
	    call pargr (MG_PAGETOP(igps))

	call fprintf (sf, "LOCATION\tViewport on virtual page:\t%.3f %.3f %.3f %.3f\n")
	    call pargr (MG_VIEWLEFT(igps))
	    call pargr (MG_VIEWRIGHT(igps))
	    call pargr (MG_VIEWBOTTOM(igps))
	    call pargr (MG_VIEWTOP(igps))

	call fprintf (sf, "LIMITS\t\tData window:\t\t\t%.3f %.3f %.3f %.3f (WC)\n")
	    call pargr (MG_WINDLEFT(igps))
	    call pargr (MG_WINDRIGHT(igps))
	    call pargr (MG_WINDBOTTOM(igps))
	    call pargr (MG_WINDTOP(igps))

	if (DEBUG_OUTPUT(igs) == YES) {
	    call ggview (GIO_GP(igs), vl, vr, vb, vt)
	    call fprintf (sf, "\t\tGIO viewport:\t\t\t%.3f %.3f %.3f %.3f (NDC)\n")
		call pargr (vl)
		call pargr (vr)
		call pargr (vb)
		call pargr (vt)
	}

#	if (MG_XLOG(igps) == NO)
#	    call fprintf (sf, "\tX axis Linear;")
#	else
#	    call fprintf (sf, "\tX axis Logarithmic;")

#	if (MG_YLOG(igps) == NO)
#	    call fprintf (sf, "\tY axis Linear\n")
#	else
#	    call fprintf (sf, "\tY axis Logarithmic\n")

	call fprintf (sf, "\nRELOCATE\tCurrent logical pen position:\t%8g, %8g (WC)\n")
	    call pargr (MG_XPOS(igps))
	    call pargr (MG_YPOS(igps))

#	MG_MINORX(igps) = INDEFR
#	MG_MINORY(igps) = INDEFR
#	MG_MAJORX(igps) = INDEFR
#	MG_MAJORY(igps) = INDEFR
#	MG_XLEXP(igps)  = INDEFR
#	MG_XHEXP(igps)  = INDEFR
#	MG_YLEXP(igps)  = INDEFR
#	MG_YHEXP(igps)  = INDEFR

#	MG_SEXAGX(igps) = NO
#	MG_SEXAGY(igps) = NO
#	MG_SEXAGS(igps) = NO
#	MG_NDECMX(igps) = INDEFI
#	MG_NDECMY(igps) = INDEFI
#	MG_NDECIM(igps) = INDEFI

	call fprintf (sf, "\n")

	if (MG_TITLE(igps) != EOS) {
	    call fprintf (sf, "TITLE\tPlot title\n%s\n")
		call pargstr (MG_TITLE(igps))
	}

	if (MG_XLABEL(igps) != EOS) {
	    call fprintf (sf, "XLABEL\tX axis label\n%s\n")
		call pargstr (MG_XLABEL(igps))
	}

	if (MG_YLABEL(igps) != EOS) {
	    call fprintf (sf, "YLABEL\tY axis label\n%s\n")
		call pargstr (MG_YLABEL(igps))
	}

	if (STDOUT_REDIR(igs) == NO) {
	    call close (sf)
	    call gpagefile (GIO_GP(igs), Memc[tempfile], "Parameter Status")
	    call delete (Memc[tempfile])
	}

	call sfree (sp)

	call lcmdcat (igs, YES)
#	call cmdcat  (igs, NO)
end


#  SHOW_SCALE -- List the viewport scale.

procedure show_scale (outfd, igs)

int	outfd			# Output stream file descriptor
pointer	igs			# igi parameters structure

pointer	igps			# Plot parameters structure
real	gvl, gvr, gvb, gvt	# GIO viewport

begin
	igps = PLOT_PARMS(igs)
	call gdeactivate (GIO_GP(igs), 0)

	if (MG_NXPANE(igps) != 1 || MG_NYPANE(igps) != 1) {
	    call fprintf (outfd, "Screen windowed:\t\t%d X %d tiles;  tile %d\n")
		call pargi (MG_NXPANE(igps))
		call pargi (MG_NYPANE(igps))
		call pargi (MG_PANE(igps))
	}

	call fprintf (outfd, "Device and Plot Scaling\t\tLeft\tRight\tBottom\tTop\n")
	call fprintf (outfd, "Virtual page (device):\t\t%.3f\t%.3f\t%.3f\t%.3f\n")
	    call pargr (MG_PAGELEFT(igps))
	    call pargr (MG_PAGERIGHT(igps))
	    call pargr (MG_PAGEBOTTOM(igps))
	    call pargr (MG_PAGETOP(igps))

	call fprintf (outfd, "Viewport on virtual page:\t%.3f\t%.3f\t%.3f\t%.3f\n")
	    call pargr (MG_VIEWLEFT(igps))
	    call pargr (MG_VIEWRIGHT(igps))
	    call pargr (MG_VIEWBOTTOM(igps))
	    call pargr (MG_VIEWTOP(igps))

	if (DEBUG_OUTPUT(igs) == YES) {
	    call ggview (GIO_GP(igs), gvl, gvr, gvb, gvt)
	    call fprintf (outfd, "GIO Viewport:\t\t\t%.3f\t%.3f\t%.3f\t%.3f (NDC)\n")
		call pargr (gvl)
		call pargr (gvr)
		call pargr (gvb)
		call pargr (gvt)
	}

	call greactivate (GIO_GP(igs), 0)
end


#  SHOW_WINDOW -- List the window

procedure show_window (outfd, igs)

int	outfd			# Output stream file descriptor
pointer	igs			# igi parameters structure

pointer	igps			# igi parameters structure
int	nxp, nyp
int	ixp, iyp
int	pane
real	gvl, gvr, gvb, gvt	# GIO viewport

begin
	igps = PLOT_PARMS(igs)
	call gdeactivate (GIO_GP(igs), 0)

	if (MG_NXPANE(igps) != 1 || MG_NYPANE(igps) != 1) {
	    nxp  = MG_NXPANE(igps)
	    nyp  = MG_NYPANE(igps)
	    pane = MG_PANE(igps)
	    ixp  = mod (pane, nxp)
	    if (ixp == 0)
		ixp = nxp
	    iyp = 1 + (pane - 1) / nxp

	    call fprintf (outfd, "Pane %d (%d,%d) in Window of %d X %d panes\n")
		call pargi (pane)
		call pargi (ixp)
		call pargi (iyp)
		call pargi (nxp)
		call pargi (nyp)
	}

	call fprintf (outfd, "Virtual page (device):\t\t%.3f\t%.3f\t%.3f\t%.3f\n")
	    call pargr (MG_PAGELEFT(igps))
	    call pargr (MG_PAGERIGHT(igps))
	    call pargr (MG_PAGEBOTTOM(igps))
	    call pargr (MG_PAGETOP(igps))

	call fprintf (outfd, "Viewport on virtual page:\t%.3f\t%.3f\t%.3f\t%.3f\n")
	    call pargr (MG_VIEWLEFT(igps))
	    call pargr (MG_VIEWRIGHT(igps))
	    call pargr (MG_VIEWBOTTOM(igps))
	    call pargr (MG_VIEWTOP(igps))

	if (DEBUG_OUTPUT(igs) == YES) {
	    call ggview (GIO_GP(igs), gvl, gvr, gvb, gvt)
	    call fprintf (outfd, "GIO Viewport:\t\t\t%.3f\t%.3f\t%.3f\t%.3f (NDC)\n")
		call pargr (gvl)
		call pargr (gvr)
		call pargr (gvb)
		call pargr (gvt)
	}

	call greactivate (GIO_GP(igs), 0)
end
