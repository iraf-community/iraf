include	<gset.h>
include <math.h>
include "rvpackage.h"
include "rvflags.h"


# RV_ANPLOT - Write the split-plot of the correlation function and anti-
# symmetric noise component to the metacode file, or screen.

procedure rv_anplot (rv, gp)

pointer	rv				#I RV struct pointer
pointer	gp				#I Graphics pointer

int	i
real	xp, yp, step
real	vx1, vx2, vy1, vy2		# Viewport boundaries on input

begin
	if (gp == NULL)
	    return			# No-op

	# Save the current viewport
	call ggview (gp, vx1, vx2, vy1, vy2)   

	# Clear the screen
	call gclear (gp)

	# Draw the two plots to the screen
	call split_plot (rv, gp, TOP, WRKPIXY(rv,1), RV_CCFNPTS(rv),
	    ANTISYM_PLOT, CORRELATION_PLOT)
	call split_plot (rv, gp, BOTTOM, ANTISYM(rv,1), RV_CCFNPTS(rv), 
	    OBJECT_SPECTRUM, ANTISYM_PLOT)

	# Restore the viewport to the way we found it originally
	call gsview (gp, vx1-0.1, vx2, vy1, vy2)		
	call gflush (gp)

	# Now get the coords to draw the text
	call gswind (gp, 0.0, 1.0, 0.0, 1.0)	# set to NDC space
	call gseti (gp, G_TXCOLOR, RV_TXTCOLOR(rv))
	xp = 0.15
	step = 0.15
	do i = 1, 6 {
	    yp = -0.05
	    call an_text (rv, gp, xp, yp, -i)	# do the titles
	    yp = -0.1
	    call an_text (rv, gp, xp, yp, i)	# do the numbers
	    xp = xp + step
	}
	call gseti (gp, G_TXCOLOR, C_FOREGROUND)
	call gflush (gp)
end


# AN_TEXT - Write the text string to the screen at the specified point.

procedure an_text (rv, gp, xp, yp, lnum)

pointer	rv					#I RV struct pointer
pointer	gp					#I Graphics pointer
real	xp, yp					#I Position
int 	lnum					#I Line to write

pointer	sp, bp
real	sigmaa, eps

begin
	# Allocate working space
	call smark (sp)			
	call salloc (bp, SZ_LINE, TY_CHAR)

	switch (lnum) {
	case -1:
	    call strcpy ("Height", Memc[bp], SZ_LINE)
	case -2:
	    call strcpy ("   R", Memc[bp], SZ_LINE)
	case -3:
	    call strcpy (" Sigma ", Memc[bp], SZ_LINE)
	case -4:
	    call strcpy ("Epsilon", Memc[bp], SZ_LINE)
	case -5:
	    if (RV_DCFLAG(rv) != -1)
	        call strcpy ("  CZ", Memc[bp], SZ_LINE)
	    else
	        call strcpy ("Shift", Memc[bp], SZ_LINE)
	case -6:
	    call strcpy (" +/-", Memc[bp], SZ_LINE)
	case 1:
	    call sprintf (Memc[bp], SZ_LINE, "%-.4f")
		call pargr (RV_HEIGHT(rv))
	case 2:
	    call sprintf (Memc[bp], SZ_LINE, "%-.4f")
		call pargr (RV_R(rv))
	case 3:
	    sigmaa =  RV_HEIGHT(rv) / (RV_R(rv) * SQRTOF2)
	    call sprintf (Memc[bp], SZ_LINE, "%-.5f")
		call pargr (sigmaa)
	case 4:
	    eps =  (TWOPI * RV_FWHM(rv)) / (RV_R(rv)+1.0) / 8.0
	    call sprintf (Memc[bp], SZ_LINE, "%-.5f")
		call pargr (eps)
	case 5:
	    call sprintf (Memc[bp], SZ_LINE, "%-.3f")
		if (RV_DCFLAG(rv) != -1)
		    call pargd (RV_VCOR(rv))
		else
		    call pargr (RV_SHIFT(rv))
	case 6:
	    call sprintf (Memc[bp], SZ_LINE, "%-.3f")
		call pargd (RV_ERROR(rv))
	}

	# Write the text
	call gtext (gp, xp, yp, Memc[bp], "")

	call sfree (sp)
end
