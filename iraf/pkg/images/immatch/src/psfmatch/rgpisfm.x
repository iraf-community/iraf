include <imhdr.h>
include <ctype.h>
include <gset.h>
include "psfmatch.h"

define	HELPFILE	"immatch$src/psfmatch/psfmatch.key"

# Define the plot functions

define	PM_PPOWER	1
define	PM_PKERNEL	2

# Define the plot types

define  PM_PCONTOUR     1
define  PM_PLINE        2
define  PM_PCOL         3

# RG_PISFM -- Procedure to compute the shifts interactively.

int procedure rg_pisfm (pm, imr, reglist, impsf, im1, imk, imp, im2, gd, id)

pointer	pm		#I pointer to the psfmatch structure
pointer	imr		#I/O pointer to the reference image/psf
pointer	reglist		#I/O pointer to the regions list
pointer	impsf		#I/O pointer to the input psf
pointer	im1		#I/O pointer to the input image
pointer	imp		#I/O pointer to the fourier spectrum image
pointer	imk		#I/O pointer to the kernel image
pointer	im2		#I/O pointer to the output image
pointer	gd		#I graphics stream pointer
pointer	id		#I display stream pointer

int	newref, newimage, newfourier, newfilter, plotfunc, plottype, wcs, key
int	newplot, ncolr, nliner, ip
pointer	sp, cmd
real	wx, wy
int	rg_pstati(), rg_psfm(), clgcur(), rg_pgqverify(), rg_pgtverify()
int	ctoi(), rg_pregions()
pointer	rg_pstatp()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	newref = YES
	newimage = YES
	newfourier = YES
	newfilter = YES
	ncolr = INDEFI
	nliner = INDEFI
	plotfunc = PM_PKERNEL
	plottype = PM_PCONTOUR

	# Compute the convolution kernel for the current image.
	if (rg_pstati (pm, CONVOLUTION) == PM_CONIMAGE && rg_pstati (pm,
	    NREGIONS) <= 0) {
	    call gclear (gd)
	    call gflush (gd)
	    call printf ("The objects list is empty\n")
	} else {
	    if (rg_psfm (pm, imr, im1, impsf, imk, newref) == OK) {
	        call rg_pplot (gd, pm, ncolr, nliner, plotfunc, plottype)
	        newref = NO
	        newimage = NO
	        newfourier = NO
	        newfilter = NO
	    } else {
	        call gclear (gd)
	        call gflush (gd)
	        call rg_pstats (pm, IMAGE, Memc[cmd], SZ_FNAME)
	        call printf ("Error computing kernel for image %s\n")
		    call pargstr (Memc[cmd])
	    }
	}
	newplot = NO

	# Loop over the cursor commands.
	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    switch (key) {

	    # Print the help page.
	    case '?':
	        call gpagefile (gd, HELPFILE, "")

	    # Quit the task gracefully.
	    case 'q':
		if (rg_pgqverify ("psfmatch", pm, imk, key) == YES) {
		    call sfree (sp)
		    return (rg_pgtverify (key))
		}

	    # Process colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
                    ;
                switch (Memc[cmd+ip-1]) {

                case 'x':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
    		        call rg_pcolon (gd, pm, imr, reglist, impsf, im1, imk,
			    NULL, im2, Memc[cmd], newref, newimage,
			    newfourier, newfilter)
                    } else {
                        ip = ip + 1
                        if (ctoi (Memc[cmd], ip, ncolr) <= 0) {
			    switch (plotfunc) {
			    case PM_PPOWER:
                                ncolr = rg_pstati (pm, NXFFT) / 2 + 1
			    case PM_PKERNEL:
                                ncolr = rg_pstati (pm, KNX) / 2 + 1
			    default:
                                ncolr = rg_pstati (pm, KNX) / 2 + 1
			    }
			}
                        plottype = PM_PCOL
                        newplot = YES
                    }

		case 'y':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
    		        call rg_pcolon (gd, pm, imr, reglist, impsf, im1, imk,
			    NULL, im2, Memc[cmd], newref, newimage,
			    newfourier, newfilter)
                    } else {
			ip = ip + 1
                        if (ctoi (Memc[cmd], ip, nliner) <= 0) {
			    switch (plotfunc) {
			    case PM_PPOWER:
                                nliner = rg_pstati (pm, NYFFT) / 2 + 1
			    case PM_PKERNEL:
                                nliner = rg_pstati (pm, KNY) / 2 + 1
			    default:
                                nliner = rg_pstati (pm, KNY) / 2 + 1
			    }
			}
                        plottype = PM_PLINE
                        newplot = YES
		    }


		default:
    		    call rg_pcolon (gd, pm, imr, reglist, impsf, im1, imk, NULL,
		        im2, Memc[cmd], newref, newimage, newfourier,
			newfilter)
		}

		 # Write the parameters to the parameter file.
		 case 'w':
		    call rg_pppars (pm)

		# Recompute the convolution kernel function.
		 case 'f':

		    if (rg_pstati(pm,CONVOLUTION) == PM_CONIMAGE) {
		        if (newref == YES)
			    if (rg_pregions (reglist, imr, pm, 1, YES) > 0)
			        ;
			else if (newimage == YES)
			    call rg_pindefr (pm)
		    }

		    if (rg_pstati (pm, NREGIONS) > 0 || rg_pstati (pm,
		        CONVOLUTION) != PM_CONIMAGE) {

		        if (newfourier == YES) {
		            call printf (
			        "\nRecomputing convolution kernel ...\n")
			    if (rg_psfm (pm, imr, im1, impsf, imk,
			        newref) == OK) {
			        ncolr = INDEFI
			        nliner = INDEFI
	    		        call rg_pplot (gd, pm, ncolr, nliner, plotfunc,
			            plottype)
			        newref = NO
				newimage = NO
			        newfourier = NO
			        newfilter = NO
			        newplot = NO
			    } else
		    	        call printf (
				    "\nError computing new kernel ...\n")
		        }

		        if (newfilter == YES) {
		            if (Memr[rg_pstatp(pm,FFT)] != NULL) {
			        call rg_pfilter (pm)
			        ncolr = INDEFI
			        nliner = INDEFI
	    		        call rg_pplot (gd, pm, ncolr, nliner, plotfunc,
			            plottype)
				newfilter = NO
				newplot = NO
			    } else
			        call printf (
				"The kernel fourier spectrum is undefined\n")
		        }

		    } else
		        call printf ("The objects list is empty\n")

	    # Draw a contour plot of the kernel.
	    case 'k':
	        if (plotfunc != PM_PKERNEL)
		    newplot = YES
		if (plottype != PM_PCONTOUR)
		    newplot = YES
		plotfunc = PM_PKERNEL
		plottype = PM_PCONTOUR
		ncolr = (1 + rg_pstati (pm, KNX)) / 2
		nliner = (1 + rg_pstati (pm, KNY)) / 2 

	    # Draw a contour plot of the fourier spectrum.
	    case 'p':
		if (plotfunc != PM_PPOWER)
		    newplot = YES
		if (plottype != PM_PCONTOUR)
		    newplot = YES
		plotfunc = PM_PPOWER
		plottype = PM_PCONTOUR
		ncolr = (1 + rg_pstati (pm, NXFFT)) / 2
		nliner = (1 + rg_pstati (pm, NYFFT)) / 2 

	    # Plot a line of the current plot.
	    case 'x':
	        if (plottype != PM_PCOL)
                    newplot = YES
                if (plottype == PM_PCONTOUR) {
                    ncolr = nint (wx)
                    nliner = nint (wy)
                } else if (plottype == PM_PLINE) {
                    ncolr = nint (wx)
                }
                plottype = PM_PCOL

	    # Plot a line of the current plot.
	    case 'y':
		if (plottype != PM_PLINE)
                    newplot = YES
                if (plottype == PM_PCONTOUR) {
                    ncolr = nint (wx)
                    nliner = nint (wy)
                } else if (plottype == PM_PCOL) {
                    ncolr = nint (wx)
                }
                plottype = PM_PLINE

	    # Redraw the current plot.
	    case 'r':
	        newplot = YES

	    # Do nothing gracefully.
	    default:
		;

	    }

	    if (newplot == YES) {
		if (rg_pstati (pm, CONVOLUTION) == PM_CONIMAGE &&
		    rg_pstati (pm, NREGIONS) <= 0) {
	    	    call printf ("Warning: The objects list is empty\n")
		} else if (newref == YES || newimage == YES ||
		    newfourier == YES || newfilter == YES) {
		    call printf (
		        "Warning: Convolution kernel should be refit\n")
		} else if (rg_pstatp (pm, CONV) != NULL) {
	    	    call rg_pplot (gd, pm, ncolr, nliner, plotfunc, plottype)
		    newplot = NO
		} else {
		    call printf (
		        "Warning: The convolution kernel is undefined\n")
		}
	    }

	}

	call sfree (sp)
end


define	QUERY "[Hit return to continue, n next image, q quit, w quit and update parameters]"

# RG_PGQVERIFY -- Print a message in the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure rg_pgqverify (task, pm, imk, ch)

char	task[ARB]	# task name
pointer	pm		# pointer to psfmatch structure
pointer	imk		# pointer to kernel image
int	ch		# character keystroke command

int	wcs, stat 
pointer	sp, cmd
real	wx, wy
bool	streq()
int	clgcur(), rg_pstati()

begin
 	call smark (sp)
        call salloc (cmd, SZ_LINE, TY_CHAR)

        # Print the status line query in reverse video and get the keystroke.
        call printf (QUERY)
        if (clgcur ("gcommands", wx, wy, wcs, ch, Memc[cmd], SZ_LINE) == EOF)
            ;

	# Process the command.
        if (ch == 'q') {
	    if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL)
                call rg_pwrite (pm, imk, NULL)
            stat = YES
        } else if (ch == 'w') {
	    if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL)
                call rg_pwrite (pm, imk, NULL)
            if (streq ("psfmatch", task))
                call rg_pppars (pm)
            stat = YES
        } else if (ch == 'n') {
	    if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL)
                call rg_pwrite (pm, imk, NULL)
            stat = YES
        } else {
            stat = NO
        }

	call sfree (sp)

	return (stat)
end


# RG_PGTVERIFY -- Verify whether or not the user truly wishes to quit the
# task.

int procedure rg_pgtverify (ch)

int     ch              #I the input keystroke command

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


# RG_PPLOT -- Draw the default plot of the kernel fourier spectrum or the
# kernel itself.

procedure rg_pplot (gd, pm, col, line, plotfunc, plottype)

pointer gd              #I pointer to the graphics stream
pointer pm              #I pointer to the psfmatch structure
int     col             #I column of cross-correlation function to plot
int     line            #I line of cross-correlation function to plot
int	plotfunc	#I the default plot function type
int     plottype        #I the default plot type

int	nx, ny
pointer	sp, title, str, data
int	rg_pstati(), strlen()
pointer	rg_pstatp()

begin
        if (gd == NULL)
            return

        # Allocate working space.
        call smark (sp)
        call salloc (title, SZ_LINE, TY_CHAR)
        call salloc (str, SZ_LINE, TY_CHAR)

        # Initialize the plot title and data.
	switch (plotfunc) {
	case PM_PPOWER:
            call sprintf (Memc[title], SZ_LINE,
                "Fourier Spectrum for Reference: %s  Image: %s")
                call rg_pstats (pm, REFIMAGE, Memc[str], SZ_FNAME)
                call pargstr (Memc[str])
                call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
                call pargstr (Memc[str])
	    data = rg_pstatp (pm, ASFFT)
	    nx = rg_pstati (pm, NXFFT)
	    ny = rg_pstati (pm, NYFFT)
	case PM_PKERNEL:
            call sprintf (Memc[title], SZ_LINE,
                "Convolution Kernel for Reference: %s  Image: %s")
                call rg_pstats (pm, REFIMAGE, Memc[str], SZ_FNAME)
                call pargstr (Memc[str])
                call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
                call pargstr (Memc[str])
	    data = rg_pstatp (pm, CONV)
	    nx = rg_pstati (pm, KNX)
	    ny = rg_pstati (pm, KNY)
	default:
            call sprintf (Memc[title], SZ_LINE,
                "Convolution Kernel for Reference: %s  Image: %s")
                call rg_pstats (pm, REFIMAGE, Memc[str], SZ_FNAME)
                call pargstr (Memc[str])
                call rg_pstats (pm, IMAGE, Memc[str], SZ_FNAME)
                call pargstr (Memc[str])
	    data = rg_pstatp (pm, CONV)
	    nx = rg_pstati (pm, KNX)
	    nx = rg_pstati (pm, KNY)
	}
	if (IS_INDEFI(col))
	    col = 1 + nx / 2
	if (IS_INDEFI(line))
	    line = 1 + ny / 2

	# Draw the plot.
        if (ny == 1) {
	    switch (plotfunc) {
	    case PM_PPOWER:
                call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
                    "\nLine %d")
                    call pargi (1)
                call rg_pcpline (gd, Memc[title], Memr[rg_pstatp(pm,ASFFT)],
                    nx, ny, 1)
	    case PM_PKERNEL:
                call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
                    "\nLine %d")
                    call pargi (1)
                call rg_pcpline (gd, Memc[title], Memr[rg_pstatp(pm,CONV)],
                    nx, ny, 1)
	    default:
                call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
                    "\nLine %d")
                    call pargi (1)
                call rg_pcpline (gd, Memc[title], Memr[rg_pstatp(pm,CONV)],
                    nx, ny, 1)
            }
        } else {
            switch (plottype) {
            case PM_PCONTOUR:
                call rg_contour (gd, Memc[title], "", Memr[data], nx, ny)
            case PM_PLINE:
                call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
                    "\nLine %d")
                    call pargi (line)
                call rg_pcpline (gd, Memc[title], Memr[data], nx, ny, line)
            case PM_PCOL:
                call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
                    "\nColumn %d")
                    call pargi (col)
                call rg_pcpcol (gd, Memc[title], Memr[data], nx, ny, col)
            default:
                call rg_contour (gd, Memc[title], "", Memr[data], nx, ny)
            }
        }

        call sfree (sp)
end


# RG_PCPLINE -- Plot a line of a 2D function.

procedure rg_pcpline (gd, title, data, nx, ny, nline)

pointer gd              #I pointer to the graphics stream
char    title[ARB]      #I title for the plot
real    data[nx,ARB]    #I the input data array
int     nx, ny          #I dimensions of the input data array
int     nline           #I the line number

int     i
pointer sp, str, x
real    ymin, ymax

begin
        # Return if no graphics stream.
        if (gd == NULL)
            return

        # Check for valid line number.
        if (nline < 1 || nline > ny)
            return

        # Allocate some working space.
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)
        call salloc (x, nx, TY_REAL)

        # Initialize the data.
        do i = 1, nx
            Memr[x+i-1] = i
        call alimr (data[1,nline], nx, ymin, ymax)

        # Set up the labels and the axes.
        call gclear (gd)
        call gswind (gd, 1.0, real (nx), ymin, ymax)
        call glabax (gd, title, "X Lag", "X-Correlation Function")

        # Plot the line profile.
        call gseti (gd, G_PLTYPE, GL_SOLID)
        call gpline (gd, Memr[x], data[1,nline], nx)
        call gflush (gd)

        call sfree (sp)
end


# RG_PCPCOL -- Plot a column of the cross-correlation function.

procedure rg_pcpcol (gd, title, data, nx, ny, ncol)

pointer gd              #I pointer to the graphics stream
char    title[ARB]      #I title of the column plot
real    data[nx,ARB]    #I the input data array
int     nx, ny          #I the dimensions of the input data array
int     ncol            #I line number

int     i
pointer sp, x, y
real    ymin, ymax

begin
        # Return if no graphics stream.
        if (gd == NULL)
            return

        # Check for valid column number.
        if (ncol < 1 || ncol > nx)
            return

        # Initialize.
        call smark (sp)
        call salloc (x, ny, TY_REAL)
        call salloc (y, ny, TY_REAL)

        # Get the data to be plotted.
        do i = 1, ny {
            Memr[x+i-1] = i
            Memr[y+i-1] = data[ncol,i]
        }
        call alimr (Memr[y], ny, ymin, ymax)

        # Set up the labels and the axes.
        call gclear (gd)
        call gswind (gd, 1.0, real (ny), ymin, ymax)
        call glabax (gd, title, "Y Lag", "X-Correlation Function")

        # Plot the profile.
        call gseti (gd, G_PLTYPE, GL_SOLID)
        call gpline (gd, Memr[x], Memr[y], ny)

        call sfree (sp)
end
