# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

define	LTM	Memd[ltm+(($2)-1)*pdim+($1)-1]

# MW_ROTATE -- Front end to mw_translate, used to perform simple rotations
# of the logical system by specifying the rotation angle in degrees, and the
# center of rotation.  Since only one rotation angle can be specified, this
# routine is useful only for 2-dim rotations (between any two axes).  Note
# that the transformation is performed in double precision even though the
# rotation angle and center are specified in single precision, preserving
# the full internal precision of the Lterm.

procedure mw_rotate (mw, theta, center, axbits)

pointer	mw			#I pointer to MWCS descriptor
real	theta			#I rotation angle, degrees
real	center[ARB]		#I center of rotation
int	axbits			#I bitflags defining axes to be rotated

double	d_theta
pointer	sp, ltm, ltv_1, ltv_2
int	axis[MAX_DIM], naxes, ax1, ax2, axmap, pdim, nelem
errchk	syserr

begin
	# Convert axis bitflags to axis list.
	call mw_gaxlist (mw, axbits, axis, naxes)
	if (naxes != 2)
	    call syserr (SYS_MWROT2AX)

	pdim = MI_NDIM(mw)
	nelem = pdim * pdim
	axmap = MI_USEAXMAP(mw)
	MI_USEAXMAP(mw) = NO
	d_theta = theta
	ax1 = axis[1]
	ax2 = axis[2]

	call smark (sp)
	call salloc (ltm, nelem, TY_DOUBLE)
	call salloc (ltv_1, pdim, TY_DOUBLE)
	call salloc (ltv_2, pdim, TY_DOUBLE)

	# Initialize the translation matrix and vectors.
	call mw_mkidmd (Memd[ltm], pdim)
	call aclrd (Memd[ltv_1], pdim)
	call aclrd (Memd[ltv_2], pdim)

	# Set up a 2-dim rotation between the specified axes.
	LTM(ax1,ax1) =  cos(d_theta)
	LTM(ax2,ax1) =  sin(d_theta)
	LTM(ax1,ax2) = -sin(d_theta)
	LTM(ax2,ax2) =  cos(d_theta)

	# Set the rotation center.
	Memd[ltv_1+ax1-1] = center[1]
	Memd[ltv_1+ax2-1] = center[2]

	# Set the back translation vector.
	Memd[ltv_2+ax1-1] = center[1]
	Memd[ltv_2+ax2-1] = center[2]

	# Perform the translation.
	call mw_translated (mw, Memd[ltv_1], Memd[ltm], Memd[ltv_2], pdim)

	MI_USEAXMAP(mw) = axmap
	call sfree (sp)
end
