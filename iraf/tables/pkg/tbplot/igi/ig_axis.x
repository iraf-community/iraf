include <gset.h>
include	<math.h>
include "igi.h"

#  1/27/93  Fixed INDEF tests

procedure ig_axis (igs)

pointer	igs		# Parameters structure pointer

real	a1, a2		# Range of data values
real	small, big	# Minor, major tick interval
real	ax, ay		# Start of axis in NDC
real	alen		# Size of axis in NDC
int	label		# Labels?
int	clock		# Clockwise ticks?

int	get_int()
real	get_real()

errchk	get_int, get_real

begin
	call lcmdcat (igs, YES)

	# Set the virtual page and viewport on the device
	call vpage (igs)

	iferr (a1 = get_real (igs))
	    return

	if (!IS_INDEFR (a1)) {
	    iferr (a2 = get_real (igs))
		return

	    if (!IS_INDEFR (a2)) {
		iferr (small = get_real (igs))
		    return

		if (!IS_INDEFR (small)) {
		    iferr (big = get_real (igs))
			return

		    if (!IS_INDEFR (big)) {
			iferr (ax = get_real (igs))
			    return

			if (!IS_INDEFR (ax)) {
			    iferr (ay = get_real (igs))
				return

			    if (!IS_INDEFR (ay)) {
				iferr (alen = get_real (igs))
				    return

				if (!IS_INDEFR (alen)) {
				    iferr (label = get_int (igs))
					return

				    if (!IS_INDEFI (label))
					iferr (clock = get_int (igs))
					    return
				}
			    }
			}
		    }
		}
	    }
	}

	call ii_axis (igs, a1, a2, small, big, ax, ay, alen, label, clock)

	call cmdcat (igs, YES)
	call gflush (GIO_GP(igs))
end


procedure ii_axis (igs, a1, a2, small, big, ax1, ay1, alen, label, clock)

pointer	igs			# Parameters structure pointer
real	a1, a2			# Range of data values
real	small, big		# Minor, major tick interval
real	ax1, ay1		# Start of axis in VPC
real	alen			# Size of axis in VPC
int	label			# Labels?
int	clock			# Clockwise ticks?

pointer	igps
real	ax2, ay2		# End of axis in VPC
real	gvx1, gvy1, gvx2, gvy2	# Endpoints in NDC
real	gvlen			# Size in NDC

begin
	call gseti (GIO_GP(igs), G_CLIP, NO)

	if (IS_INDEFR (a1))
	    a1 = 0.0

	if (IS_INDEFR (a2))
	    a2 = 1.0

	if (IS_INDEFR (small))
	    small = 0.0

	if (IS_INDEFR (big))
	    big = 0.0

	if (IS_INDEFR (ax1))
	    ax1 = 0.0

	if (IS_INDEFR (ay1))
	    ay1 = 0.0

	if (IS_INDEFR (alen))
	    alen = 1.0

	if (IS_INDEFI (label))
	    label = 0

	if (IS_INDEFI (clock))
	    clock = 1

	igps = PLOT_PARMS(igs)

	ax2 = ax1 + alen * cos (DEGTORAD(MG_ANGLE(igps)))
	ay2 = ay1 + alen * sin (DEGTORAD(MG_ANGLE(igps)))
	call vpc_ndc (igs, ax1, ay1, gvx1, gvy1)
	call vpc_ndc (igs, ax2, ay2, gvx2, gvy2)
	gvlen = sqrt ((gvx2 - gvx1) ** 2 + (gvy2 - gvy1) ** 2)

	call axis (igs, a1, a2, small, big, gvx1, gvy1, gvlen,
	    label, clock, MG_TICKFMT(igps))

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
end
