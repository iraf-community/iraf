include <gset.h>
include <imhdr.h>
include <ctype.h>
include "linmatch.h"

# Define the help files.
define	HELPFILE	"immatch$src/linmatch/linmatch.key"

# RG_LISCALE -- Scale the output image interactively.

int procedure rg_liscale (imr, im1, im2, db, dformat, reglist, rpfd, ipfd, sfd,
	ls, gd, id)

pointer	imr		#I/O pointer to the reference image
pointer	im1		#I/O pointer to the input image
pointer	im2		#I/O pointer to the output image
pointer	db		#I/O pointer to the database file
int	dformat		#I is the scale file in database format
pointer	reglist		#I/O the regions list descriptor
int	rpfd		#I/O the reference photometry file descriptor
int	ipfd		#I/O the input photometry file descriptor
int	sfd		#I/O the shifts file descriptor
pointer	ls		#I pointer to the linmatch structure
pointer	gd		#I the graphics stream pointer
pointer	id		#I display stream pointer

int	i, newref, newimage, newfit, newavg, newplot, plottype, wcs, key, reg
int	hplot, lplot, lplot_type
pointer	sp, cmd, udelete, stat
real	bscale, bzero, bserr, bzerr, wx, wy
int	rg_lstati(), rg_lplot(), clgcur(), rg_lgqverify(), rg_lgtverify()
int	rg_ldelete(), rg_lfind(), rg_mmhplot(), rg_rifplot(), rg_rirplot()
int	rg_lregions()
pointer	rg_lstatp()

begin
	call gdeactivate (gd, 0)

	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (udelete, rg_lstati(ls, MAXNREGIONS), TY_INT)

	# Initialize the fitting.
	newref = YES
	newimage = YES
	newfit = YES
	newavg = YES

	# Initialize the plotting.
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN, LS_MEDIAN, LS_MODE:
	    if (rg_lstati (ls, NREGIONS) > 1)
	        plottype = LS_MMFIT
	    else
	        plottype = LS_MMHIST
	case LS_FIT:
	    if (rg_lstati (ls, NREGIONS) > 1)
	        plottype = LS_BSZFIT
	    else
	        plottype = LS_RIFIT
	case LS_PHOTOMETRY:
	    plottype = LS_BSZFIT
	default:
	}
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN, LS_MEDIAN, LS_MODE:
	    if (rg_lstati (ls, NREGIONS) > 1)
	        plottype = LS_MMFIT
	    else
	        plottype = LS_MMHIST
	case LS_FIT:
	    if (rg_lstati (ls, NREGIONS) > 1)
	        plottype = LS_BSZFIT
	    else
	        plottype = LS_RIFIT
	case LS_PHOTOMETRY:
	    plottype = LS_BSZFIT
	default:
	}

	# Do the initial fit.
	if (rg_lstati (ls, NREGIONS) <= 0) {
	    call gclear (gd)
	    call gflush (gd)
	    bscale = 1.0; bzero = 0.0
	    bserr = INDEFR; bzerr = INDEFR
	    call printf ("The regions/photometry list is empty\n")
	} else {
	    call amovki (LS_NO, Memi[rg_lstatp(ls,RDELETE)], rg_lstati(ls,
	        NREGIONS))
	    call rg_scale (imr, im1, ls, bscale, bzero, bserr, bzerr, YES)
	    call amovki (NO, Memi[udelete], rg_lstati(ls,NREGIONS))
	    if (rg_lplot (gd, imr, im1, ls, Memi[udelete], 1, bscale, bzero,
	    	plottype) == OK) {
	        newref = NO
	        newimage = NO
	        newfit = NO
		newavg = NO
		call rg_lpwrec (ls, 0)
	    } else {
	        call gclear (gd)
	        call gflush (gd)
		call rg_lstats (ls, IMAGE, Memc[cmd], SZ_FNAME)
	        call printf ("Error computing scale factors for image %s\n")
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
		    if (rg_lgqverify ("linmatch", db, dformat, ls,
		        key) == YES) {
			call sfree (sp)
			return (rg_lgtverify (key))
		    }

		# Refit the data.
		case 'f':
		    if (newref == YES || newimage == YES || newfit == YES ||
		        newavg == YES) {
			if (rg_lstati(ls, BSALGORITHM) != LS_PHOTOMETRY &&
			    rg_lstati(ls, BZALGORITHM) != LS_PHOTOMETRY) {
			    if (newref == YES) {
				if (rg_lregions (reglist, imr, ls, 1, YES) > 0)
				    ;
			    } else if (newimage == YES) {
				call rg_lindefr (ls)
			    }
			}
			if (newfit == YES)
	    		    call amovki (LS_NO, Memi[rg_lstatp(ls,RDELETE)],
			        rg_lstati(ls,NREGIONS))
			else if (newavg == YES) {
			    do i = 1, rg_lstati(ls,NREGIONS) {
				if (Memi[rg_lstatp(ls,RDELETE)+i-1] ==
				    LS_DELETED || Memi[rg_lstatp(ls,
				    RDELETE)+i-1] == LS_BADSIGMA)
				    Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_NO
			    }
			    
			}
			do i = 1, rg_lstati(ls,NREGIONS) {
			    if (Memi[udelete+i-1] == YES)
				Memi[rg_lstatp(ls,RDELETE)+i-1] = LS_DELETED
			}
			if (newfit == YES)
	    		    call rg_scale (imr, im1, ls, bscale, bzero, bserr,
			        bzerr, YES)
			else if (newavg == YES)
	    		    call rg_scale (imr, im1, ls, bscale, bzero, bserr,
			        bzerr, NO)
			newref = NO
			newimage = NO
			newfit = NO
			newavg = NO
			newplot = YES
		    }

		# Plot the default graph.
		case 'g':
		    switch (rg_lstati(ls, BZALGORITHM)) {
		    case LS_MEAN, LS_MEDIAN, LS_MODE:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_MMFIT)
				newplot = YES
	        	    plottype = LS_MMFIT
	    		} else {
			    if (plottype != LS_MMHIST)
				newplot = YES
	        	    plottype = LS_MMHIST
			}
		    case LS_FIT:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_BSZFIT)
				newplot = YES
	        	    plottype = LS_BSZFIT
	    		} else {
			    if (plottype != LS_RIFIT)
				newplot = YES
	        	    plottype = LS_RIFIT
			}
		    case LS_PHOTOMETRY:
			if (plottype != LS_BSZFIT)
			    newplot = YES
	        	plottype = LS_BSZFIT
		    default:
		    }
		    switch (rg_lstati(ls, BSALGORITHM)) {
		    case LS_MEAN, LS_MEDIAN, LS_MODE:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_MMFIT)
				newplot = YES
	        	    plottype = LS_MMFIT
	    		} else {
			    if (plottype != LS_MMHIST)
				newplot = YES
	        	    plottype = LS_MMHIST
			}
		    case LS_FIT:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_BSZFIT)
	        	        plottype = LS_BSZFIT
	    		} else {
			    if (plottype != LS_RIFIT)
	        	        plottype = LS_RIFIT
			}
		    case LS_PHOTOMETRY:
			if (plottype != LS_BSZFIT)
			    newplot = YES
	        	plottype = LS_BSZFIT
		    default:
		    }

		# Graph the residuals from the current fit.
		case 'i':
		    switch (rg_lstati(ls, BZALGORITHM)) {
		    case LS_MEAN, LS_MEDIAN, LS_MODE:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_MMRESID)
				newplot = YES
	        	    plottype = LS_MMRESID
	    		} else {
			    call printf (
			    "There are too few regions for a residuals plot\n")
			}
		    case LS_FIT:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_BSZRESID)
				newplot = YES
	        	     plottype = LS_BSZRESID
	    		} else {
			    if (plottype != LS_RIRESID)
				newplot = YES
	        	    plottype = LS_RIRESID
			}
		    case LS_PHOTOMETRY:
			if (plottype == LS_BSZFIT) {
			    newplot = YES
			    plottype = LS_BSZRESID
			} else if (plottype == LS_MAGSKYFIT) {
			    newplot = YES
			    plottype = LS_MAGSKYRESID
			}
		    default:
		    }
		    switch (rg_lstati(ls, BSALGORITHM)) {
		    case LS_MEAN, LS_MEDIAN, LS_MODE:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_MMRESID)
				newplot = YES
	        	    plottype = LS_MMRESID
	    		} else {
			    call printf (
			    "There are too few regions for a residuals plot\n")
			}
		    case LS_FIT:
	    		if (rg_lstati (ls, NREGIONS) > 1) {
			    if (plottype != LS_BSZRESID)
				newplot = YES
	        	    plottype = LS_BSZRESID
	    		} else {
			    if (plottype != LS_RIRESID)
				newplot = YES
	        	    plottype = LS_RIRESID
			}
		    case LS_PHOTOMETRY:
			if (plottype == LS_BSZFIT) {
			    newplot = YES
			    plottype = LS_BSZRESID
			} else if (plottype == LS_MAGSKYFIT) {
			    newplot = YES
			    plottype = LS_MAGSKYRESID
			}
		    default:
		    }

		# Plot the histogram and show the statistics of a given region.
		# selected from a plot.
		case 's':
		    if (imr != NULL && im1 != NULL) {
	    	        reg = rg_lfind (gd, ls, wcs, wx, wy, bscale, bzero,
		            plottype)
		        if (reg > 0) {
			    if (rg_mmhplot (gd, imr, im1, ls, Memi[udelete],
			        reg) == OK) {
			        call rg_lpwrec (ls, reg)
			    } else {
	        	        call printf (
			            "Unable to plot statistics for region %d\n")
				    call pargi (reg)
			    }
		        } else
	        	    call printf ("Unable to plot region statistics\n")
		    } else
		        call printf (
			"The reference or input image is undefined\n")

		# Trace the fit of a given region selected from a plot.
		case 't':
		    if (imr != NULL && im1 != NULL && (rg_lstati(ls,
		        BSALGORITHM) == LS_FIT || rg_lstati(ls,BZALGORITHM) ==
		        LS_FIT)) {
	    	        reg = rg_lfind (gd, ls, wcs, wx, wy, bscale, bzero,
			    plottype)
		        if (reg > 0) {
			    if (plottype == LS_BSZFIT)
				stat = rg_rifplot (gd, imr, im1, ls,
				    Memi[udelete], reg)
			    else if (plottype == LS_BSZRESID)
				stat = rg_rirplot (gd, imr, im1, ls,
				    Memi[udelete], reg)
			    else
				stat = ERR
			    if (stat == OK)
				call rg_lpwrec (ls, reg)
			    else {
	        	        call printf (
			            "Unable to plot statistics for region %d\n")
				    call pargi (reg)
			    }
		        } else
	        	    call printf (
			        "Unable to plot region statistics\n")
		    } else
			call printf (
			    "The least squares fit is undefined\n") 

		# Plot the statistics and show the histograms for each
		# region in turn.
		case 'h':
		    if (imr != NULL && im1 != NULL) {
			reg = 1
			if (rg_mmhplot (gd, imr, im1, ls, Memi[udelete],
			    reg) == ERR) {
			    call printf (
			    "Unable to plot statistics for region 1\n")
			    next
			}
			hplot = NO 
			call printf (
			"Hit [spbar=next,-=prev,s=stats,?=help,q=quit]:")
			while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd],
			    SZ_LINE) != EOF) {
			    switch (key) {
			    case '?':
				call printf (
			     "Hit [spbar=next,-=prev,s=stats,?=help,q=quit]:")
			    case 'q':
				call printf ("\n")
				break
			    case ' ':
				if (reg < rg_lstati (ls, NREGIONS)) {
				    reg = reg + 1
				    hplot = YES
				}
			    case '-':
				if (reg > 1) {
				    reg = reg - 1
				    hplot = YES
				}
			    case 's':
			        call rg_lpwrec (ls, reg)
			    }
			    if (hplot == YES) {
				if (rg_mmhplot (gd, imr, im1, ls,
				    Memi[udelete], reg) == ERR)
			    	    ;
				call printf (
			      "Hit [spbar=next,-=prev,s=stats,?=help,q=quit]:")
				hplot = NO
			    }
			}
			newplot = YES
		    } else
		        call printf (
			"The reference or input image is undefined\n")

		# Step through the least sqares fits one at a time.
		case 'l':
		    if (imr != NULL && im1 != NULL && (rg_lstati(ls,
		        BSALGORITHM) == LS_FIT || rg_lstati(ls,BZALGORITHM) ==
		        LS_FIT)) {
			reg = 1
			lplot = NO 
			if (plottype == LS_BSZFIT || plottype == LS_RIFIT)
			    lplot_type = LS_RIFIT
			else if (plottype == LS_BSZRESID || plottype ==
			    LS_RIRESID)
			    lplot_type = LS_RIRESID
			if (lplot_type == LS_RIFIT)
			    stat = rg_rifplot (gd, imr, im1, ls, Memi[udelete],
			        reg)
			else if (lplot_type == LS_RIRESID)
			    stat = rg_rirplot (gd, imr, im1, ls, Memi[udelete],
			        reg)
			else
			    stat = ERR
			if (stat == ERR) {
			    call printf ("Unable to plot fits for region 1\n")
			    next
			}
			call printf (
		"Hit [spbar=next,-=prev,l=fit,i=resid,s=stats,?=help,q=quit]:")
			while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd],
			    SZ_LINE) != EOF) {
			    switch (key) {
			    case '?':
				call printf (
		"Hit [spbar=next,-=prev,l=fit,i=resid,s=stats,?=help,q=quit]:")
			    case 'q':
				call printf ("\n")
				break
			    case ' ':
				if (reg < rg_lstati (ls, NREGIONS)) {
				    reg = reg + 1
				    lplot = YES
				}
			    case '-':
				if (reg > 1) {
				    reg = reg - 1
				    lplot = YES
				}
			    case 'l':
				if (lplot_type == LS_RIRESID)
				    lplot = YES
				lplot_type = LS_RIFIT
			    case 'i':
				if (lplot_type == LS_RIFIT)
				    lplot = YES
				lplot_type = LS_RIRESID
			    case 's':
			        call rg_lpwrec (ls, reg)
			    }
			    if (lplot == YES) {
				if (lplot_type == LS_RIFIT)
			    	    stat = rg_rifplot (gd, imr, im1, ls,
				        Memi[udelete], reg)
				else if (lplot_type == LS_RIRESID)
			    	    stat = rg_rirplot (gd, imr, im1, ls,
				        Memi[udelete], reg)
				call printf (
		"Hit [spbar=next,-=prev,l=fit,i=resid,s=stats,?=help,q=quit]:")
				lplot = NO
			    }
			}
			newplot = YES
		    } else
			call printf (
			    "The least squares fit is undefined\n") 

		# Plot the photometry
		case 'p':
		    if (rg_lstati(ls,BSALGORITHM) == LS_PHOTOMETRY ||
			rg_lstati(ls,BZALGORITHM) == LS_PHOTOMETRY) {
			plottype = LS_MAGSKYFIT
			newplot = YES
		    } else
			call printf ("The input photometry is undefined\n") 

		# Replot the current graph.
		case 'r':
		    newplot = YES

		# Delete or undelete a region.
		case 'd', 'u':
		    if (key == 'd')
	    	        reg = rg_ldelete (gd, ls, Memi[udelete], wcs, wx, wy,
			    bscale, bzero, plottype, YES)
		    else
	    	        reg = rg_ldelete (gd, ls, Memi[udelete], wcs, wx, wy,
			    bscale, bzero, plottype, NO)
		    if (reg > 0)
			newavg = YES


	        # Process colon commands.
	        case ':':
		    call rg_lcolon (gd, ls, imr, im1, im2, db, dformat,
		        reglist, rpfd, ipfd, sfd, Memc[cmd], newref,
			newimage, newfit, newavg)

		 # Write the parameters to the parameter file.
		 case 'w':
		    call rg_plpars (ls)

		# Do nothing gracefully.
		default:
	    }

	    if (newplot == YES) {
		if (rg_lstati(ls,NREGIONS) <= 0) {
	    	    call gclear (gd)
	    	    call gflush (gd)
	    	    bscale = 1.0; bzero = 0.0
	            bserr = INDEFR; bzerr = INDEFR
	    	    call printf ("The regions/photometry list is empty\n")
		} else if (newref == YES || newimage == YES) {
		    call printf ("Bscale and bzero must be recomputed\n")
		} else if (rg_lplot (gd, imr, im1, ls, Memi[udelete], 1,
		    bscale, bzero, plottype) == OK) {
		    if (newfit == YES || newavg == YES)
		        call printf ("Bscale and bzero should be recomputed\n")
		    else
		        call rg_lpwrec (ls, 0)
		    newplot = NO
		} else
	            call printf ("Unable to plot image data for region 1\n")
	    }

	}

	call sfree (sp)
end

define  QUERY "Hit [return=continue, n=next image, q=quit, w=quit and update parameters]: "

# RG_LGQVERIFY -- Print a message on the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure rg_lgqverify (task, db, dformat, rg, ch)

char    task[ARB]       #I the calling task name
pointer db              #I pointer to the shifts database file
int     dformat         #I is the shifts file in database format
pointer rg              #I pointer to the task structure
int     ch              #I the input keystroke command

int     wcs, stat
pointer sp, cmd
real    wx, wy
bool    streq()
int     clgcur()

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
            call rg_lwrec (db, dformat, rg)
            stat = YES
        } else if (ch == 'w') {
            call rg_lwrec (db, dformat, rg)
            if (streq ("linmatch", task))
                call rg_plpars (rg)
            stat = YES
        } else if (ch == 'n') {
            call rg_lwrec (db, dformat, rg)
            stat = YES
        } else {
            stat = NO
        }

        call sfree (sp)
        return (stat)
end


# RG_LGTVERIFY -- Verify whether or not the user truly wishes to quit the
# task.

int procedure rg_lgtverify (ch)

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
