include <imhdr.h>
include <fset.h>
include <ctype.h>
include "xregister.h"

define	HELPFILE	"immatch$src/xregister/xregister.key"
define	OHELPFILE	"immatch$src/xregister/oxregister.key"

define	XC_PCONTOUR	1
define	XC_PLINE	2
define	XC_PCOL		3


# RG_XICORR -- Compute the shifts for each image interactively using
# cross-correlation techniques.

int procedure rg_xicorr (imr, im1, im2, db, dformat, reglist, tfd, xc, gd, id)

pointer	imr		#I/O pointer to the reference image
pointer	im1		#I/O pointer to the input image
pointer	im2		#I/O pointer to the output image
pointer	db		#I/O pointer to the shifts database file
int	dformat		#I is the shifts file in database format
pointer	reglist		#I/O the regions list descriptor
int	tfd		#I/O the transform file descriptor
pointer	xc		#I pointer to the cross-corrrelation structure
pointer	gd		#I the graphics stream pointer
pointer	id		#I the display stream pointer

size_t	sz_val
int	newdata, newcross, newcenter, wcs, key, cplottype, newplot, ip
long	ncolr, nliner
pointer	sp, cmd
real	xshift, yshift, wx, wy
int	rg_xstati(), rg_icross(), clgcur(), rg_xgtverify(), rg_xgqverify()
long	rg_xstatl(), lnint()
int	ctol()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (cmd, sz_val, TY_CHAR)

	# Initialize.
	newdata = YES
	newcross = YES
	newcenter = YES
	ncolr = (1 + rg_xstatl (xc, XWINDOW)) / 2
	nliner = (1 + rg_xstatl (xc, YWINDOW)) / 2
	cplottype = XC_PCONTOUR 
	newplot = YES
	xshift = 0.0
	yshift = 0.0

	# Compute the cross-correlation function for the first region
	# and print the results.
	if (rg_xstati (xc, NREGIONS) <= 0) {
	    call gclear (gd)
	    call printf ("The regions list is empty\n")
	} else if (rg_icross (xc, imr, im1, rg_xstati (xc, CREGION)) != ERR) {
	    call rg_xcplot (xc, gd, ncolr, nliner, cplottype)
	    call rg_fit (xc, rg_xstati (xc, CREGION), gd, xshift, yshift)
	    call rg_xpwrec (xc, rg_xstati (xc, CREGION))
	    newdata = NO
	    newcross = NO
	    newcenter = NO
	    newplot = NO
	} else {
	    call gclear (gd)
	    call printf (
	        "Error computing X-correlation function for region %d\n")
		call pargi (rg_xstati (xc, CREGION))
	}


	# Loop over the cursor commands.
	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    switch (key) {

	    # Print the help page.
	    case '?':
	        call gpagefile (gd, HELPFILE, "")

	    # Redraw the current plot.
	    case 'r':
		newplot = YES

	    # Draw a contour plot of the cross-correlation function.
	    case 'c':
		if (cplottype != XC_PCONTOUR)
		    newplot = YES
		ncolr = (rg_xstatl (xc, XWINDOW) + 1) / 2
		nliner = (rg_xstatl (xc, YWINDOW) + 1) / 2
		cplottype = XC_PCONTOUR 

	    # Plot a column of the cross-correlation function.
	    case 'x':
		if (cplottype != XC_PCOL)
		    newplot = YES
		if (cplottype == XC_PCONTOUR) {
		    ncolr = lnint(wx)
		    nliner = lnint(wy)
		} else if (cplottype == XC_PLINE) {
		    ncolr = lnint(wx)
		} 
		cplottype = XC_PCOL 

	    # Plot a line of the cross-correlation function.
	    case 'y':
		if (cplottype != XC_PLINE)
		    newplot = YES
		if (cplottype == XC_PCONTOUR) {
		    ncolr = lnint(wx)
		    nliner = lnint(wy)
		} else if (cplottype == XC_PCOL) {
		    ncolr = lnint(wx)
		} 
		cplottype = XC_PLINE 

	    # Quit the task gracefully.
	    case 'q':
		if (rg_xgqverify ("xregister", db, dformat, xc, key) == YES) {
		    call sfree (sp)
		    return (rg_xgtverify (key))
		}

	    # The Data overlay menu.
	    case 'o':
		#call gdeactivate (gd, 0)
		call rg_xoverlay (gd, xc, rg_xstati (xc, CREGION), imr, im1)
		#call greactivate (gd, 0)
		newplot = YES

	    # Process colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		switch (Memc[cmd+ip-1]) {
		case 'x':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
	                call rg_xcolon (gd, xc, imr, im1, im2, db, dformat,
			    tfd, reglist, Memc[cmd], newdata, newcross,
			    newcenter)
		    } else {
			ip = ip + 1
			if (ctol (Memc[cmd], ip, ncolr) <= 0)
			    ncolr = (1 + rg_xstatl (xc, XWINDOW)) / 2
			cplottype = XC_PCOL
			newplot = YES
		    }
		case 'y':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
	                call rg_xcolon (gd, xc, imr, im1, im2, db, dformat,
			    tfd, reglist, Memc[cmd], newdata, newcross,
			    newcenter)
		    } else {
			ip = ip + 1
			if (ctol (Memc[cmd], ip, nliner) <= 0)
			    nliner = (1 + rg_xstatl (xc, YWINDOW)) / 2
			cplottype = XC_PLINE
			newplot = YES
		    }
		default:
	            call rg_xcolon (gd, xc, imr, im1, im2, db, dformat, tfd,
		        reglist, Memc[cmd], newdata, newcross, newcenter)
		}

	    # Compute an image lag interactively.
	    case 't':
		call gdeactivate (gd, 0)
		call rg_itransform (xc, imr, im1, id)
		newdata = YES; newcross = YES; newcenter = YES
		call greactivate (gd, 0)

	     # Write the parameters to the parameter file.
	     case 'w':
		call rg_pxpars (xc)

	    case 'f':

		if (rg_xstati (xc, NREGIONS) > 0) {

	    	    if (newdata == YES) {
			call rg_xcindefr (xc, rg_xstati(xc,CREGION))
			newdata = NO
	    	    }

	    	    if (newcross == YES) {
			call printf (
			    "Recomputing X-correlation function ...\n")
			if (rg_icross (xc, imr, im1, 
				       rg_xstati (xc, CREGION)) != ERR) {
			    ncolr = (1 + rg_xstatl (xc, XWINDOW)) / 2
			    if (IM_NDIM(imr) == 1)
				nliner = 1
			    else
			        nliner = (1 + rg_xstatl (xc, YWINDOW)) / 2
	    	            call rg_xcplot (xc, gd, ncolr, nliner, cplottype)
	    	            call rg_fit (xc, rg_xstati (xc, CREGION), gd,
				xshift, yshift)
	    	            call rg_xpwrec (xc, rg_xstati (xc, CREGION))
	    	            newcross = NO
	    	            newcenter = NO
	    	            newplot = NO
		        } else {
	    	            call printf (
	               "Error computing X-correlation function for region %d\n")
			        call pargi (rg_xstati (xc, CREGION))
		        }
	            }

	    	    if (newcenter == YES) {
	    		call rg_fit (xc, rg_xstati (xc, CREGION), gd,
			    xshift, yshift)
	    		call rg_xpwrec (xc, rg_xstati (xc, CREGION))
	    		newcenter = NO
	    	    }

		} else
		    call printf ("The regions list is empty\n")



	    # Do nothing gracefully.
	    default:
		call printf ("Unknown or ambiguous keystroke command\n")
	    }

	    # Replot the correlation function.
	    if (newplot == YES) {
	        if (newdata == YES) {
		    call printf (
		        "Warning: X-correlation function should be refit\n")
	        } else if (newcross == YES) {
		    call printf (
		        "Warning: X-correlation function should be refit\n")
	        } else if (newcenter == YES) {
		    call printf (
		        "Warning: X-correlation function should be refit\n")
	        } else if (rg_xstatp (xc, XCOR) != NULL) {
	            call rg_xcplot (xc, gd, ncolr, nliner, cplottype)
	            call rg_xpwrec (xc, rg_xstati (xc, CREGION))
		    newplot = NO
	        } else {
		    call printf (
		        "Warning: X-correlation function is undefined\n")
	        }
	    }
	}

	call sfree (sp)
end


# RG_XOVERLAY -- The image overlay plot menu.

procedure rg_xoverlay (gd, xc, nreg, imr, im1)

pointer	gd		#I graphics stream pointer
pointer	xc		#I pointer to the crosscor structure
int	nreg		#I the current region number
pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image

size_t	sz_val
long	c_0
int	ip, wcs, key
long	ixlag, iylag, ixshift, iyshift
long	nrimcols, nrimlines, nimcols, nimlines, ncolr, ncoli, nliner, nlinei
pointer	sp, cmd
real	wx, wy, rxlag, rylag, xshift, yshift
int	clgcur(), ctol(), rg_xstati()
long	rg_xstatl()
pointer	rg_xstatp()
long	lnint()

begin
	c_0 = 0

	if (gd == NULL)
	    return

	nrimcols = IM_LEN(imr,1)
	if (IM_NDIM(imr) == 1)
	    nrimlines = 1
	else
	    nrimlines = IM_LEN(imr,2)
	nimcols = IM_LEN(im1,1)
	if (IM_NDIM(im1) == 1)
	    nimlines = 1
	else
	    nimlines = IM_LEN(im1,2)
	if (rg_xstati (xc, NREFPTS) > 0) {
	    wx = (1. + nrimcols) / 2.0
	    wy = (1. + nrimlines) / 2.0
	    call rg_etransform (xc, wx, wy, rxlag, rylag)
	    ixlag = rxlag - wx
	    iylag = rylag - wy
	} else {
	    ixlag = rg_xstatl (xc, XLAG) 
	    iylag = rg_xstatl (xc, YLAG) 
	}
	xshift = -Memr[rg_xstatp(xc,XSHIFTS)+nreg-1]
	yshift = -Memr[rg_xstatp(xc,YSHIFTS)+nreg-1]

	call smark (sp)
	sz_val = SZ_LINE
	call salloc (cmd, sz_val, TY_CHAR)

	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	    switch (key) {

	    # Print the help menu.
	    case '?':
		call gdeactivate (gd, 0)
		call pagefile (OHELPFILE, "")
		call greactivate (gd, 0)

	    # Quit.
	    case 'q':
		break

	    # Plot the same line of the reference and input image.
	    case 'l':
		call rg_xpline (gd, imr, im1, lnint(wy), c_0, c_0)

	    # Plot the same column of the reference and input image
	    case 'c':
		call rg_xpcol (gd, imr, im1, lnint(wx), c_0, c_0)

	    case 'y':
		call rg_xpline (gd, imr, im1, lnint(wy), ixlag, iylag)

	    case 'x':
		call rg_xpcol (gd, imr, im1, lnint(wx), ixlag, iylag)

	    case 'h':
		call rg_xpline (gd, imr, im1, lnint(wy), lnint(xshift),
				lnint(yshift))

	    case 'v':
		call rg_xpcol (gd, imr, im1, lnint(wx), lnint(xshift),
			       lnint(yshift))

	    case ':':
		ip = 1
	        call rg_cokeys (Memc[cmd], ip, SZ_LINE, key)
		switch (key) {
	        case 'l':
		    ixshift = 0
		    if (ctol (Memc[cmd], ip, nliner) <= 0)
			nliner = (1 + nrimlines) / 2 
		    nliner = max (1, min (nliner, nrimlines))
		    if (ctol (Memc[cmd], ip, nlinei) <= 0)
			nlinei = nliner
		    iyshift = nlinei - nliner
		    call rg_xpline (gd, imr, im1, nliner, ixshift, iyshift)

		case 'c':
		    if (ctol (Memc[cmd], ip, ncolr) <= 0)
		        ncolr = (1 + nrimcols) / 2
		    ncolr = max (1, min (ncolr, nrimcols))
		    if (ctol (Memc[cmd], ip, ncoli) <= 0)
			ncoli = ncolr
		    ncoli = max (1, min (ncoli, nimcols))
		    ixshift = ncoli - ncolr
		    iyshift = 0
		    call rg_xpcol (gd, imr, im1, ncolr, ixshift, iyshift)

		case 'y':
		    if (ctol (Memc[cmd], ip, nliner) <= 0)
		 	nliner = (1 + nrimlines) / 2
		    nliner = max (1, min (nliner, nrimlines))
		    call rg_xpline (gd, imr, im1, nliner, ixlag, iylag)

		case 'x':
		    if (ctol (Memc[cmd], ip, ncolr) <= 0)
			ncolr = (1 + nrimcols) / 2
		    ncolr = max (1, min (ncolr, nrimcols))
		    call rg_xpcol (gd, imr, im1, ncolr, ixlag, iylag)

		case 'h':
		    if (ctol (Memc[cmd], ip, nliner) <= 0)
		 	nliner = (1 + nrimlines) / 2
		    nliner = max (1, min (nliner, nrimlines))
		    call rg_xpline (gd, imr, im1, nliner, lnint(xshift),
				    lnint(yshift))

		case 'v':
		    if (ctol (Memc[cmd], ip, ncolr) <= 0)
			ncolr = (1 + nrimcols) / 2
		    ncolr = max (1, min (ncolr, nrimcols))
		    call rg_xpcol (gd, imr, im1, ncolr, lnint(xshift),
				   lnint(yshift))
		default:
		    call printf ("Ambiguous or unknown overlay menu command\n")
		}
	    case 'g':
		while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd],
		    SZ_LINE) != EOF) {
		    if (key == 'q')
			break
		}
	    default:
		call printf ("Ambiguous or unknown overlay menu command\n")
	    }

	}

	call sfree (sp)
end


# RG_XCPLOT -- Draw the default plot of the cross-correlation function.

procedure rg_xcplot (xc, gd, col, line, plottype)

pointer	xc		#I pointer to cross-correlation structure
pointer	gd		#I pointer to the graphics stream
long	col		#I column of cross-correlation function to plot
long	line		#I line of cross-correlation function to plot
int	plottype	#I the default plot type

size_t	sz_val
long	c_1
int	nreg
size_t	xwindow, ywindow
pointer	sp, title, str, prc1, prc2, prl1, prl2
int	rg_xstati(), strlen()
long	rg_xstatl()
pointer	rg_xstatp()

begin
	c_1 = 1

	if (gd == NULL)
	    return

	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (title, sz_val, TY_CHAR)
	call salloc (str, sz_val, TY_CHAR)

	# Get the regions.
	nreg = rg_xstati (xc, CREGION)
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)

	# Initialize the window size.
	xwindow = rg_xstatl (xc, XWINDOW)
	if ((Meml[prl2+nreg-1] - Meml[prl1+nreg-1] + 1) == 1)
	    ywindow = 1
	else
	    ywindow = rg_xstatl (xc, YWINDOW)

	# Construct a title.
	call sprintf (Memc[title], SZ_LINE,
	    "Reference: %s  Image: %s  Region: [%d:%d,%d:%d]")
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call pargl (Meml[prc1+nreg-1])
	    call pargl (Meml[prc2+nreg-1])
	    call pargl (Meml[prl1+nreg-1])
	    call pargl (Meml[prl2+nreg-1])

	# Draw the plot.
	if (ywindow == 1) {
	    call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	        "\nX-Correlation Function: line %d")
		call pargi (1)
	    call rg_xcpline (gd, Memc[title], Memr[rg_xstatp(xc,XCOR)],
	        xwindow, ywindow, c_1)
	} else {
	    switch (plottype) {
	    case XC_PCONTOUR:
	        call rg_contour (gd, "X-Correlation Function", Memc[title],
		    Memr[rg_xstatp (xc, XCOR)], xwindow, ywindow)
	    case XC_PLINE:
	        call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	            "\nX-Correlation Function: line %d")
		    call pargl (line)
	        call rg_xcpline (gd, Memc[title], Memr[rg_xstatp(xc,XCOR)],
	            xwindow, ywindow, line)
	    case XC_PCOL:
	        call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	            "\nX-Correlation Function: column %d")
		    call pargl (col)
	        call rg_xcpcol (gd, Memc[title], Memr[rg_xstatp(xc,XCOR)],
	            xwindow, ywindow, col)
	    default:
	        call rg_contour (gd, "X-Correlation Function", Memc[title],
		    Memr[rg_xstatp (xc, XCOR)], xwindow, ywindow)
	    }
	}

	call sfree (sp)
end


# RG_COKEYS -- Fetch the first keystroke of a colon command.

procedure rg_cokeys (cmd, ip, maxch, key)

char	cmd[ARB]		#I the command string
int	ip			#I/O pointer into the command string
int	maxch			#I maximum number of characters
int	key			#O the keystroke

begin
	ip = 1
	while (IS_WHITE(cmd[ip]) && cmd[ip] != EOS && ip <= maxch)
	    ip = ip + 1

	if (cmd[ip] == EOS && ip > maxch)
	    key = EOS
	else {
	    key = cmd[ip]
	    ip = ip + 1
	}
end


define	QUERY "Hit [return=continue, n=next image, q=quit, w=quit and update parameters]: "

# RG_XGQVERIFY -- Print a message on the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure rg_xgqverify (task, db, dformat, rg, ch)

char	task[ARB]	#I the calling task name
pointer	db		#I pointer to the shifts database file
int	dformat		#I is the shifts file in database format
pointer	rg		#I pointer to the task structure
int	ch		#I the input keystroke command

size_t	sz_val
int	wcs, stat
pointer	sp, cmd
real	wx, wy
bool	streq()
int	clgcur()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (cmd, sz_val, TY_CHAR)

	# Print the status line query in reverse video and get the keystroke.
	call printf (QUERY)
	#call flush (STDOUT)
	if (clgcur ("gcommands", wx, wy, wcs, ch, Memc[cmd], SZ_LINE) == EOF)
	    ;

	# Process the command.
	if (ch == 'q') {
	    call rg_xwrec (db, dformat, rg)
	    stat = YES
	} else if (ch == 'w') {
	    call rg_xwrec (db, dformat, rg)
	    if (streq ("xregister", task))
		call rg_pxpars (rg)
	    stat = YES
	} else if (ch == 'n') {
	    call rg_xwrec (db, dformat, rg)
	    stat = YES
	} else {
	    stat = NO
	}

	call sfree (sp)
	return (stat)
end


# RG_XGTVERIFY -- Verify whether or not the user truly wishes to quit the
# task.

int procedure rg_xgtverify (ch)

int	ch		#I the input keystroke command

begin
	if (ch == 'q') {
	    return (YES)
	} else if (ch == 'w') {
	    return (YES)
	} else if (ch == 'n') {
	    return (NO)
	} else {
	    return (NO)
	}
end
