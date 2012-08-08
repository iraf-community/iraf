include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"
include "rvplots.h"

.help plotpars
.nf ___________________________________________________________________________
PLOTPARS  - Support routines for the 'plotpars' named external pset.  

	This file include routines for opening/closing the FFT plot structure 
as well as command handling.  Command handling is limited to changing the 
parameter values or resetting them to the default values.  Routines included
here are as follows.

	  	       plot_open (rv)
	              plot_close (rv)
	           plot_get_pars (rv)
		  plot_parupdate (rv)
		    plot_unlearn (rv)
		       plot_show (rv, fd)
		      plot_colon (rv, cmdstr)
                        cmd_plot (rv)
                     cmd_overlay (rv)
                 cmd_split_plotx (rv)
                   cmd_one_image (rv)
                        cmd_when (rv)
                   cmd_log_scale (rv)
                    cmd_fft_zoom (rv)

	The 'cmd_' prefix indicates that the routine is called from a colon 
command to either print the current value or set the new value for that
field.  Other routines should be self-explanatory

.endhelp _____________________________________________________________________

# The default Fourier plot parameters
define	DEF_PLOT	AMPLITUDE_PLOT		# Default plot type
define	DEF_OVERLAY	YES			# Default filter overlay
define	DEF_SPLIT_PLOT	SPLIT_PLOT		# Default plot type
define	DEF_ONE_IMAGE	OBJECT_SPECTRUM		# Deault for one image plot
define	DEF_WHEN	BEFORE			# Default when to plot
define	DEF_LOG_SCALE	YES			# Default Y-axis scaling
define	DEF_FFT_ZOOM	1.0			# Default zoom


# PLOT_OPEN - Open the Process parameters substructure.  This is used to
# reduce the size of the already over-burdened main RV struct.

procedure plot_open (rv)

pointer	rv					#I RV struct pointer

pointer	plot

begin
	iferr (call calloc (plot, SZ_PLOTSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating sub-structure RV_PLOTP.")

	RV_PLOTP(rv) = plot

	# Initlialize the values
	call plot_unlearn (rv)			# Set to defaults
end


# PLOT_CLOSE - Close the process structure.

procedure plot_close (rv)

pointer	rv					#I RV struct pointer

begin
	call mfree (RV_PLOTP(rv), TY_STRUCT)
end


# PLOT_UNLEARN -- Reset all of the plot parameters to their default values.

procedure plot_unlearn (rv)

pointer	rv					#I RV struct pointer

begin
	RVP_PLOT(rv) = DEF_PLOT
	RVP_OVERLAY(rv) = DEF_OVERLAY
	RVP_SPLIT_PLOT(rv) = DEF_SPLIT_PLOT
	RVP_ONE_IMAGE(rv) = DEF_ONE_IMAGE
	RVP_WHEN(rv) = DEF_WHEN
	RVP_LOG_SCALE(rv) = DEF_LOG_SCALE
	RVP_FFT_ZOOM(rv) = DEF_FFT_ZOOM
end


# PLOT_COLON -- Process the PLOTPARS task colon commands.

procedure plot_colon (rv, cmdstr)

pointer	rv				#I pointer to the RV structure
char	cmdstr[SZ_LINE]			#I command string

pointer	sp, cmd, buf
int	strdic()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)

	# Unpack the keyword from the string and look it up in the
	# dictionary.  Switch on command and call the appropriate routines.

	switch (strdic(Memc[cmd], Memc[cmd], SZ_FNAME, PLOT_KEYWORDS)) {
	case PLT_PLOT:
	    call cmd_plot (rv)
	case PLT_OVERLAY:
	    call cmd_overlay (rv)
	case PLT_SPLIT_PLOT:
	    call cmd_split_plotx (rv)
	case PLT_ONE_IMAGE:
	    call cmd_one_image (rv)
	case PLT_WHEN:
	    call cmd_when (rv)
	case PLT_LOG_SCALE:
	    call cmd_log_scale (rv)
	case PLT_FFT_ZOOM:
	    call cmd_fft_zoom (rv)
	default:
	}

	call sfree (sp)
end


# CMD_PLOT - Set/Show the type of plot to draw.

procedure cmd_plot (rv)

pointer	rv					#I RV struct pointer

pointer	sp, bp
int	cod_plotype()

begin
	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[bp], SZ_FNAME)
	if (Memc[bp] != EOS)
	    RVP_PLOT(rv) = cod_plotype (Memc[bp+1])
	else {
	    call nam_plotype (rv, Memc[bp])
	    call printf ("plot = `%s'\n")
		call pargstr (Memc[bp])
	    call flush (STDOUT)
	}

	call sfree (sp)
end


# CMD_OVERLAY - Set/Show the filter overlay flag.

procedure cmd_overlay (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    RVP_OVERLAY(rv) = btoi (bval)
	    RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("overlay = %b\n")
		call pargb (itob(RVP_OVERLAY(rv)))
	}
end


# CMD_SPLIT_PLOTX - Set/Show the split plot toggle flag.

procedure cmd_split_plotx (rv)

pointer	rv					#I RV struct pointer

bool	bval
int	nscan()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    if (bval)
	        RVP_SPLIT_PLOT(rv) = SPLIT_PLOT
	    else
	        RVP_SPLIT_PLOT(rv) = SINGLE_PLOT
	} else {
	    call printf ("split_plot = %b\n")
	    if (RVP_SPLIT_PLOT(rv) == SPLIT_PLOT)
	      	call pargb (true) 
	    else
		call pargb (false)
	}
end


# CMD_ONE_IMAGE - Set/Show the type of image to draw on a single plot.

procedure cmd_one_image (rv)

pointer	rv					#I RV struct pointer

pointer	sp, bp

begin
	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[bp], SZ_FNAME)
	if (Memc[bp] != EOS) {
	    if (Memc[bp+1] == 'o')
	         RVP_ONE_IMAGE(rv) = OBJECT_SPECTRUM
	    else if (Memc[bp+1] == 't' || Memc[bp+1] == 'r')
	         RVP_ONE_IMAGE(rv) = REFER_SPECTRUM
	    else
		 call rv_errmsg ("Choose one of 'object|template'.")
	} else {
	    call printf ("one_image = `%s'\n")
	    if (RVP_ONE_IMAGE(rv) == OBJECT_SPECTRUM)
		call pargstr ("object")
	    else
		call pargstr ("template")
	    call flush (STDOUT)
	}

	call sfree (sp)
end


# CMD_WHEN - Set/Show whether to plot before or after filtering.

procedure cmd_when (rv)

pointer	rv					#I RV struct pointer

pointer	sp, bp

begin
	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)

	call gargstr (Memc[bp], SZ_FNAME)
	if (Memc[bp] != EOS) {
	    if (Memc[bp+1] == 'b') {
	        RVP_WHEN(rv) = BEFORE
	        RV_NEWGRAPH(rv) = YES
	    } else if (Memc[bp+1] == 'a') {
	        RVP_WHEN(rv) = AFTER
	        RV_NEWGRAPH(rv) = YES
	    } else
		 call rv_errmsg ("Choose one of `before|after'.")
	} else {
	    call printf ("when = `%s'\n")
	    if (RVP_WHEN(rv) == BEFORE)
		call pargstr ("before")
	    else 
		call pargstr ("after")
	    call flush (STDOUT)
	}
	call sfree (bp)
end


# CMD_LOG_SCALE - Set/Show whether to plot on a log scale.

procedure cmd_log_scale (rv)

pointer	rv					#I RV struct pointer

bool	bval, itob()
int	nscan(), btoi()

begin
	call gargb (bval)
	if (nscan() == 2) {
	    RVP_LOG_SCALE(rv) = btoi (bval)
	    RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("log_scale = %b\n")
		call pargb (itob(RVP_LOG_SCALE(rv)))
	}
end


# CMD_FFT_ZOOM - Set/Show the FFT zooming factor.

procedure cmd_fft_zoom (rv)

pointer	rv					#I RV struct pointer

real	rval
int	nscan()

begin
	call gargr (rval)
	if (nscan() == 2) {
	    if (rval < 1.)
		call rv_errmsg ("Warning: Zoom must be >= 1.0")
	    else
	        RVP_FFT_ZOOM(rv) = rval
	    RV_NEWGRAPH(rv) = YES
	} else {
	    call printf ("zoom = %f\n")
		call pargr (RVP_FFT_ZOOM(rv))
	}
end
