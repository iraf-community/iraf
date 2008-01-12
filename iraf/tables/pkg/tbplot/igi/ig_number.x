include <gset.h>
include "igi.h"

#  IG_NUMBER -- Draw element number at the coordinates in the input column(s)

#  3/3/92 ZGL
## 17 June 1992  Change MG_QUALITY to MG_FONTSET.  ZGL

procedure ig_number (igs)

pointer	igs		# Parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	if (MG_YDATAP(PLOT_PARMS(igs)) == NULL) {
	    call eprintf ("No Y data ")
	    return
	}

	call ii_number (igs)
end


#  IG_NUMBER -- Draw element number at the coordinates in the input column(s)

procedure ii_number (igs)

pointer	igs		# Parameters structure

int	npts
pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	if (MG_YDATAP(igps) == NULL)
	    return

	call gseti (GIO_GP(igs), G_CLIP, YES)
	call setltype (igs, SOLID_LINE)

	# Y data exists
	if (MG_XDATAP(igps) == NULL) {
	    # Y data only;  use pixel numbers for X
	    npts = MG_YNPTS(igps)
	    call mgvnum (igs, Memr[MG_YDATAP(igps)], npts)

	} else {
	    # Both X and Y data
	    npts = min (MG_XNPTS(igps), MG_YNPTS(igps))
	    call mgpnum (igs, Memr[MG_XDATAP(igps)], Memr[MG_YDATAP(igps)], 
		npts)
	}

	MG_NPTS(igps) = npts

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end


procedure mgvnum (igs, ydata, npts)

pointer	igs
real	ydata[ARB]
int	npts

pointer	igps		# Plot parameters structure
int	pix
pointer	sp, numstr
int	nchar

int	itoc()

begin
	call smark (sp)
	call salloc (numstr, SZ_LINE, TY_CHAR)

	igps = PLOT_PARMS(igs)

	do pix = 1, npts {
	    if (IS_INDEF(ydata[pix]))
		next

	    nchar = itoc (pix, Memc[numstr], SZ_LINE)

	    call mgostr (igs, real (pix), ydata[pix], 
		Memc[numstr], MG_EXPAND(igps), MG_ANGLE(igps), 
		MG_IJUSTC(igps), MG_FONTSET(igps))
	}

	call sfree (sp)
end


procedure mgpnum (igs, xdata, ydata, npts)

pointer	igs
real	xdata[ARB]
real	ydata[ARB]
int	npts

pointer	igps		# Plot parameters structure
int	pix
pointer	sp, numstr
int	nchar

int	itoc()

begin
	call smark (sp)
	call salloc (numstr, SZ_LINE, TY_CHAR)

	igps = PLOT_PARMS(igs)

	do pix = 1, npts {
	    if (IS_INDEF(ydata[pix]) || IS_INDEF(xdata[pix]))
		next

	    nchar = itoc (pix, Memc[numstr], SZ_LINE)

	    call mgostr (igs, xdata[pix], ydata[pix], 
		Memc[numstr], MG_EXPAND(igps), MG_ANGLE(igps), 
		MG_IJUSTC(igps), MG_FONTSET(igps))
	}

	call sfree (sp)
end
