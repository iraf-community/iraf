# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_TRANSLATE -- Translate the logical system, i.e., perform a linear
# transformation of the logical system by modifying the Lterm of the MWCS.
# The transformation is defined in terms of the CURRENT LOGICAL SYSTEM,
# subject to axis mapping, dimensional reduction, etc.  This is unlike
# MW_SLTERM, which defines the Lterm relative to the physical system in
# physical terms (no axis mapping, full dimensionality, etc.).
#
#	p' = ltm * (p - ltv_1) + ltv_2
#	
# For convenience the transformation is specified using separate translation
# vectors for the input and output systems.  If ltv_1 is set to zero a
# "fully reduced" transformation of the form used internally may be entered.

procedure mw_translated (mw, ltv_1, ltm, ltv_2, ndim)

pointer	mw			#I pointer to MWCS descriptor
double	ltv_1[ndim]		#I input translation vector
double	ltm[ndim,ndim]		#I linear transformation matrix
double	ltv_2[ndim]		#I output translation vector
int	ndim			#I dimensionality of transform

double	v
pointer	sp, o_ltm, o_ltv, n_ltm, n_ltv, ltv
int	pdim, nelem, axis[MAX_DIM], i, j
errchk	syserrs
define	err_ 91

begin
	pdim = MI_NDIM(mw)
	nelem = pdim * pdim

	call smark (sp)
	call salloc (ltv, ndim, TY_DOUBLE)
	call salloc (o_ltm, nelem, TY_DOUBLE)
	call salloc (o_ltv, pdim, TY_DOUBLE)
	call salloc (n_ltm, nelem, TY_DOUBLE)
	call salloc (n_ltv, pdim, TY_DOUBLE)

	# Combine the input and output translation vectors.
	do j = 1, ndim {
	    v = ltv_2[j]
	    do i = 1, ndim
		v = v + ltm[i,j] * (-ltv_1[i])
	    Memd[ltv+j-1] = v
	}

	# Get axis map.
	if (MI_USEAXMAP(mw) == NO) {
	    if (ndim > MI_NDIM(mw))
		goto err_
	    do i = 1, ndim
		axis[i] = i
	} else {
	    if (ndim > MI_NLOGDIM(mw))
err_		call syserrs (SYS_MWNDIM, "mw_translate")
	    do i = 1, ndim
		axis[i] = MI_PHYSAX(mw,i)
	}

	# Perform the transformation.  Use a procedure call to dereference
	# the pointers to simplify the notation.

	call mw_axtran (D(mw,MI_LTM(mw)), D(mw,MI_LTV(mw)),
	    Memd[n_ltm], Memd[n_ltv], pdim, ltm, Memd[ltv], axis, ndim)

	# Update the Lterm.
	call amovd (Memd[n_ltm], D(mw,MI_LTM(mw)), nelem)
	call amovd (Memd[n_ltv], D(mw,MI_LTV(mw)), pdim)

	call sfree (sp)
end


# MW_AXTRAN -- Axis mapped linear transformation.  Matrix or vector elements
# not included in the axis map are propagated unchanged.

procedure mw_axtran (o_ltm,o_ltv, n_ltm,n_ltv, pdim, ltm,ltv, ax, ndim)

double	o_ltm[pdim,pdim]	#I matrix to be transformed
double	o_ltv[pdim]		#I vector to be transformed
double	n_ltm[pdim,pdim]	#O transformed matrix
double	n_ltv[pdim]		#O transformed vector
int	pdim			#I dimension of these guys
double	ltm[ndim,ndim]		#I transform matrix
double	ltv[ndim]		#I transform vector
int	ax[ndim]		#I transform axis map: physax=axis[logax]
int	ndim			#I dimension of these guys

double	v
int	i, j, k

begin
	# Transform the matrix.
	call amovd (o_ltm, n_ltm, pdim * pdim)
	do j = 1, ndim
	    do i = 1, ndim {
		v = 0
		do k = 1, ndim
		    # v = v + o_ltm[ax[k],ax[j]] * ltm[i,k]
		    v = v + ltm[k,j] * o_ltm[ax[i],ax[k]]
		n_ltm[ax[i],ax[j]] = v
	    }

	# Transform the vector.	    
	call amovd (o_ltv, n_ltv, pdim)
	do j = 1, ndim {
	    v = ltv[j]
	    do i = 1, ndim
		v = v + ltm[i,j] * o_ltv[ax[i]]
	    n_ltv[ax[j]] = v
	}
end
