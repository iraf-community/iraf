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
int	reglist		#I/O the regions list descriptor
int	tfd		#I/O the transform file descriptor
pointer	xc		#I pointer to the cross-corrrelation structure
pointer	gd		#I the graphics stream pointer
pointer	id		#I the display stream pointer

int	newdata, newcross, newcenter, wcs, key, cplottype, newplot
int	ip, ncolr, nliner
pointer	sp, cmd
real	xshift, yshift, wx, wy
int	rg_xstati(), rg_icross(), clgcur(), rg_xgtverify(), rg_xgqverify()
int	ctoi()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize.
	newdata = YES
	newcross = YES
	newcenter = YES
	ncolr = (1 + rg_xstati (xc, XWINDOW)) / 2
	nliner = (1 + rg_xstati (xc, YWINDOW)) / 2
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
		ncolr = (rg_xstati (xc, XWINDOW) + 1) / 2
		nliner = (rg_xstati (xc, YWINDOW) + 1) / 2
		cplottype = XC_PCONTOUR 

	    # Plot a column of the cross-correlation function.
	    case 'x':
		if (cplottype != XC_PCOL)
		    newplot = YES
		if (cplottype == XC_PCONTOUR) {
		    ncolr = nint (wx)
		    nliner = nint (wy)
		} else if (cplottype == XC_PLINE) {
		    ncolr = nint (wx)
		} 
		cplottype = XC_PCOL 

	    # Plot a line of the cross-correlation function.
	    case 'y':
		if (cplottype != XC_PLINE)
		    newplot = YES
		if (cplottype == XC_PCONTOUR) {
		    ncolr = nint (wx)
		    nliner = nint (wy)
		} else if (cplottype == XC_PCOL) {
		    ncolr = nint (wx)
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
			if (ctoi (Memc[cmd], ip, ncolr) <= 0)
			    ncolr = (1 + rg_xstati (xc, XWINDOW)) / 2
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
			if (ctoi (Memc[cmd], ip, nliner) <= 0)
			    nliner = (1 + rg_xstati (xc, YWINDOW)) / 2
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
			if (rg_icross (xc, imr, im1, rg_xstati (xc,
			        CREGION)) != ERR) {
			    ncolr = (1 + rg_xstati (xc, XWINDOW)) / 2
			    if (IM_NDIM(imr) == 1)
				nliner = 1
			    else
			        nliner = (1 + rg_xstati (xc, YWINDOW)) / 2
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

int	ip, wcs, key, ixlag, iylag, ixshift, iyshift
int	nrimcols, nrimlines, nimcols, nimlines, ncolr, ncoli, nliner, nlinei
pointer	sp, cmd
real	wx, wy, rxlag, rylag, xshift, yshift
int	clgcur(), ctoi(), rg_xstati()
pointer	rg_xstatp()

begin
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
	    ixlag = rg_xstati (xc, XLAG) 
	    iylag = rg_xstati (xc, YLAG) 
	}
	xshift = -Memr[rg_xstatp(xc,XSHIFTS)+nreg-1]
	yshift = -Memr[rg_xstatp(xc,YSHIFTS)+nreg-1]

	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

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
		call rg_xpline (gd, imr, im1, nint (wy), 0, 0)

	    # Plot the same column of the reference and input image
	    case 'c':
		call rg_xpcol (gd, imr, im1, nint (wx), 0, 0)

	    case 'y':
		call rg_xpline (gd, imr, im1, nint (wy), ixlag, iylag)

	    case 'x':
		call rg_xpcol (gd, imr, im1, nint (wx), ixlag, iylag)

	    case 'h':
		call rg_xpline (gd, imr, im1, nint (wy), nint (xshift),
		    nint (yshift))

	    case 'v':
		call rg_xpcol (gd, imr, im1, nint (wx), nint (xshift),
		    nint (yshift))

	    case ':':
		ip = 1
	        call rg_cokeys (Memc[cmd], ip, SZ_LINE, key)
		switch (key) {
	        case 'l':
		    ixshift = 0
		    if (ctoi (Memc[cmd], ip, nliner) <= 0)
			nliner = (1 + nrimlines) / 2 
		    nliner = max (1, min (nliner, nrimlines))
		    if (ctoi (Memc[cmd], ip, nlinei) <= 0)
			nlinei = nliner
		    iyshift = nlinei - nliner
		    call rg_xpline (gd, imr, im1, nliner, ixshift, iyshift)

		case 'c':
		    if (ctoi (Memc[cmd], ip, ncolr) <= 0)
		        ncolr = (1 + nrimcols) / 2
		    ncolr = max (1, min (ncolr, nrimcols))
		    if (ctoi (Memc[cmd], ip, ncoli) <= 0)
			ncoli = ncolr
		    ncoli = max (1, min (ncoli, nimcols))
		    ixshift = ncoli - ncolr
		    iyshift = 0
		    call rg_xpcol (gd, imr, im1, ncolr, ixshift, iyshift)

		case 'y':
		    if (ctoi (Memc[cmd], ip, nliner) <= 0)
		 	nliner = (1 + nrimlines) / 2
		    nliner = max (1, min (nliner, nrimlines))
		    call rg_xpline (gd, imr, im1, nliner, ixlag, iylag)

		case 'x':
		    if (ctoi (Memc[cmd], ip, ncolr) <= 0)
			ncolr = (1 + nrimcols) / 2
		    ncolr = max (1, min (ncolr, nrimcols))
		    call rg_xpcol (gd, imr, im1, ncolr, ixlag, iylag)

		case 'h':
		    if (ctoi (Memc[cmd], ip, nliner) <= 0)
		 	nliner = (1 + nrimlines) / 2
		    nliner = max (1, min (nliner, nrimlines))
		    call rg_xpline (gd, imr, im1, nliner, nint (xshift),
		        nint (yshift))

		case 'v':
		    if (ctoi (Memc[cmd], ip, ncolr) <= 0)
			ncolr = (1 + nrimcols) / 2
		    ncolr = max (1, min (ncolr, nrimcols))
		    call rg_xpcol (gd, imr, im1, ncolr, nint (xshift),
		        nint (yshift))
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
int	col		#I column of cross-correlation function to plot
int	line		#I line of cross-correlation function to plot
int	plottype	#I the default plot type

int	nreg, xwindow, ywindow
pointer	sp, title, str, prc1, prc2, prl1, prl2
int	rg_xstati(), strlen()
pointer	rg_xstatp()

begin
	if (gd == NULL)
	    return

	# Allocate working space.
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the regions.
	nreg = rg_xstati (xc, CREGION)
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)

	# Initialize the window size.
	xwindow = rg_xstati (xc, XWINDOW)
	if ((Memi[prl2+nreg-1] - Memi[prl1+nreg-1] + 1) == 1)
	    ywindow = 1
	else
	    ywindow = rg_xstati (xc, YWINDOW)

	# Construct a title.
	call sprintf (Memc[title], SZ_LINE,
	    "Reference: %s  Image: %s  Region: [%d:%d,%d:%d]")
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	    call pargi (Memi[prc1+nreg-1])
	    call pargi (Memi[prc2+nreg-1])
	    call pargi (Memi[prl1+nreg-1])
	    call pargi (Memi[prl2+nreg-1])

	# Draw the plot.
	if (ywindow == 1) {
	    call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	        "\nX-Correlation Function: line %d")
		call pargi (1)
	    call rg_xcpline (gd, Memc[title], Memr[rg_xstatp(xc,XCOR)],
	        xwindow, ywindow, 1)
	} else {
	    switch (plottype) {
	    case XC_PCONTOUR:
	        call rg_contour (gd, "X-Correlation Function", Memc[title],
		    Memr[rg_xstatp (xc, XCOR)], xwindow, ywindow)
	    case XC_PLINE:
	        call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	            "\nX-Correlation Function: line %d")
		    call pargi (line)
	        call rg_xcpline (gd, Memc[title], Memr[rg_xstatp(xc,XCOR)],
	            xwindow, ywindow, line)
	    case XC_PCOL:
	        call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	            "\nX-Correlation Function: column %d")
		    call pargi (col)
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

int	wcs, stat
pointer	sp, cmd
real	wx, wy
bool	streq()
int	clgcur()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

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
