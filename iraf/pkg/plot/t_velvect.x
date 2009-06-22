# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DUMMY	6

include	<error.h>
include	<gset.h>
include	<config.h>
include	<mach.h>
include	<imhdr.h>
include	<xwhen.h>
include	<fset.h>

# T_VELVECT -- Draw a representation of a two-dimensional velocity field
# by drawing arrows from each data location.  The length of each arrow
# is proportional to the strength of the field at that location, and the
# direction of the arrow indicates the direction of flow at that location.
# This is an interface to the NCAR GKS VELVCT routine.

procedure t_velvect()

char	u_imsect[SZ_FNAME], v_imsect[SZ_FNAME]
char	device[SZ_FNAME], title[SZ_LINE]
pointer	u_im, v_im, u_subras, v_subras
pointer	tcojmp[LEN_JUMPBUF]
long	u_ncols, v_ncols, u_nlines, v_nlines
int	mode, status, wkid
pointer	epa, old_onint
long	c_1
int	i_val0, i_val1

pointer	gp, gopen()

bool	clgetb(), streq()
extern	vl_tco_onint()
pointer	immap(), imgs2r()
common	/tcocom/ tcojmp

include	<nullptr.inc>

begin
	c_1 = 1

	# Get image section strings and output device.
	call clgstr ("u_image", u_imsect, SZ_FNAME)
	call clgstr ("v_image", v_imsect, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)

	# Map image.
	u_im = immap (u_imsect, READ_ONLY, NULLPTR)
	v_im = immap (v_imsect, READ_ONLY, NULLPTR)

	call clgstr ("title", title, SZ_LINE)
	if (streq (title, "imtitle")) {
	    call strcpy (u_imsect, title, SZ_LINE)
	    call strcat (": ", title, SZ_LINE)
	    call strcat (IM_TITLE(u_im), title, SZ_LINE)
	}


	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	# Read in subraster.  Warn the user if the subraster is very large,
	# because the plot will take a long time to generate.

	u_ncols  = IM_LEN(u_im,1)
	u_nlines = IM_LEN(u_im,2)
	v_ncols  = IM_LEN(v_im,1)
	v_nlines = IM_LEN(v_im,2)

	if ((u_ncols != v_ncols) || (u_nlines != v_nlines))
	    call error (0, "U and V subrasters must be same size")

	u_subras = imgs2r (u_im, c_1, u_ncols, c_1, u_nlines)
	v_subras = imgs2r (v_im, c_1, v_ncols, c_1, v_nlines)

	if (u_ncols * u_nlines > 128 ** 2 || v_ncols * v_nlines > 128 ** 2 && 
	    clgetb ("verbose")) {
	    call eprintf("Warning: image is quite large; subsampling with an\n")
	    call eprintf("image section would speed things up considerably\n")
	}


	# Open device and make contour plot.
	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, mode, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)
	call gtext (gp, 0.5, 0.96, title, "f=b;h=c;s=.80")

	# Install interrupt exception handler.
	call zlocpr (vl_tco_onint, epa)
	call xwhen (X_INT, epa, old_onint)

	# Make the contour plot.  If an interrupt occurs ZSVJMP is reeentered
	# with an error status.

	call zsvjmp (tcojmp, status)
	if (status == OK) {
	    if ( u_ncols > MAX_INT ) {	# limited by sys/gio/ncarutil/velvct.f
		call error (0, "T_VELVECT: Too large u_ncols (32-bit limit)")
	    }
	    if ( u_nlines > MAX_INT ) {	# limited by sys/gio/ncarutil/velvct.f
		call error (0, "T_VELVECT: Too large u_nlines (32-bit limit)")
	    }
	    i_val0 = u_ncols
	    i_val1 = u_nlines
	    call ezvec (Memr[u_subras], Memr[v_subras], i_val0, i_val1)
	} else {
	    call gcancel (gp)
	    call fseti (STDOUT, F_CANCEL, OK)
	}

	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()

	call imunmap (u_im)
	call imunmap (v_im)
end


# VL_TCO_ONINT -- Interrupt handler for the task contour.  Branches back to
# ZSVJMP in the main routine to permit shutdown without an error message.

procedure vl_tco_onint (vex, next_handler)

int	vex		# virtual exception
pointer	next_handler	# not used

pointer	tcojmp[LEN_JUMPBUF]
common	/tcocom/ tcojmp

begin
	call xer_reset()
	call zdojmp (tcojmp, vex)
end
