include	"qpio.h"

# QPIO_LOADWCS -- Load the WCS, if any, from the QPOE file associated with the
# given QPIO descriptor, into an open MWCS descriptor.  This is equivalent to
# QP_LOADWCS except that the Lterm is updated to reflect the current blocking
# factor and rect (if any) used for rasterization.  In the resultant WCS, the
# logical coordinate system gives the pixel coordinates of the sampled rect.

pointer	procedure qpio_loadwcs (io)

pointer	io				#I QPIO descriptor

pointer	qp, mw
int	ndim, i, j
double	ltv_1[NDIM], ltv_2[NDIM], ltm[NDIM,NDIM]
pointer	qp_loadwcs()
errchk	qp_loadwcs

begin
	qp = IO_QP(io)
	mw = qp_loadwcs (qp)
	ndim = NDIM

	# Formalize the transformation.
	ltv_1[1] = IO_VSDEF(io,1) - 1
	ltv_1[2] = IO_VSDEF(io,2) - 1

	# L(i) :==  LTM=(1 / block) * P(i)  +  Vx
	# At pixel {P(i) :==  (block + 1) / 2}  L(i) is 1.0.
	# Solve for Vx :==  1.0 - (1 / block) * ((block + 1) / 2)
	#	       -->  0.5 - 1 / (2 * block)

	ltv_2[1] = 0.5d0 - 1.0d0 / double (max (1.0, IO_XBLOCK(io))) / 2.0d0
	ltv_2[2] = 0.5d0 - 1.0d0 / double (max (1.0, IO_YBLOCK(io))) / 2.0d0

	do j = 1, ndim
	    do i = 1, ndim
		if (i == j) {
		    if (i == 1)
			ltm[i,j] = 1.0D0 / max (1.0, IO_XBLOCK(io))
		    else
			ltm[i,j] = 1.0D0 / max (1.0, IO_YBLOCK(io))
		} else
		    ltm[i,j] = 0

	# Apply the transformation.
	call mw_translated (mw, ltv_1, ltm, ltv_2, ndim)

	return (mw)
end
