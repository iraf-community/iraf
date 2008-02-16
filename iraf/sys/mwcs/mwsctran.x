# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<mach.h>
include	"mwcs.h"

# MW_SCTRAN -- Set up a coordinate transformation (CTRAN) descriptor.
# The general idea is to reduce the coordinate transformation to as simple
# a form as possible for efficient evaluation at runtime.  Most of the
# complexities of the actual coordinate system, e.g., axis mapping, multiple
# WCS, separate linear and world terms, forward and inverse transforms, etc.,
# can be dealt with at CTRAN compile time.  The result is a CTRAN descriptor
# for an N-d coordinate defining an N-d linear transformation and zero or
# more calls to WCS functions for individual axes, where N can be anything
# less than or equal to the dimensionality of the full system.
#
# A transformation may be set up between any two coordinate systems
# SYSTEM1 and SYSTEM2.  The dimensionality of the transformation, and the
# axes to which it applies, is determined by the axis bitflags in AXBITS.
# A pointer to the optimized transformation descriptor is returned as the
# function value.  An arbitrary number of transformation descriptors may be
# simultaneously open (a limit of 16 or so is imposed by the main MWCS
# descriptor).  A CTRAN descriptor reflects the state of the WCS *at the
# time that the descriptor was compiled*, i.e., subsequent changes to
# the MWCS descriptor do not affect any compiled transformation descriptors.
# CTRAN descriptors not subsequently closed by CTFREE are automatically
# closed when the main MWCS descriptor is closed.

pointer procedure mw_sctran (mw, system1, system2, axbits)

pointer mw			#I pointer to MWCS descriptor
char	system1[ARB]		#I input coordinate system
char	system2[ARB]		#I output coordinate system
int	axbits			#I bitmap defining axes to be transformed

bool	newfunc
int	naxes, axis[MAX_DIM], wfno, fn, epa
int	i, j, k , matlen, ndata, ctlen, pdim
pointer	i_ltm, i_ltv, o_ltm, o_ltv, t_ltm, t_ltv, ltm, ltv
pointer	sp, w1, w2, ct, wf, fc, lp, ip, op, ct_r, sv_wcs

pointer	coerce()
errchk	syserr, syserrs, calloc, zcall2, mw_invertd, mw_ssystem
include	"mwcs.com"

begin
	call smark (sp)

	# Get pointers to the input and output systems.
	sv_wcs = MI_WCS(mw)
	iferr {
	    call mw_ssystem (mw, system1)
	    w1 = MI_WCS(mw)
	    call mw_ssystem (mw, system2)
	    w2 = MI_WCS(mw)
	} then {
	    MI_WCS(mw) = sv_wcs
	    call erract (EA_ERROR)
	} else
	    MI_WCS(mw) = sv_wcs

	# Get the physical axis list.  The bitflags in AXBITS define the axes
	# in the logical system; run these through the axis map (if enabled)
	# to get the list of physical axes for which the transformation is to
	# be prepared.

	call mw_gaxlist (mw, axbits, axis, naxes)

	# Allocate the CTRAN descriptor.  First we must figure out how
	# much space is required.  The space required is for the base
	# descriptor, plus additional space for the LTM and LTV, which vary
	# in size depending upon the dimensionality of the transformation.
	# The whole thing is then doubled to provide 2 versions of the
	# descriptor, providing both single and double precision versions
	# of the LTM and LTV.  Any additional storage utilized by the WCS
	# functions is separately allocated by the initialization routines
	# in the function drivers.

	matlen = naxes * naxes
	ndata = matlen + naxes
	ctlen = LEN_CTBASE + ndata * SZ_DOUBLE / SZ_STRUCT
	call calloc (ct, ctlen*2, TY_STRUCT)

	# Save a pointer to the CTRAN descriptor in the main MWCS descriptor,
	# to permit automatic deallocation at close time.

	do i = 1, MAX_CTRAN+1 {
	    if (i > MAX_CTRAN) {
		call mfree (ct, TY_STRUCT)
		call syserr (SYS_MWCTOVFL)
	    }

	    if (MI_CTRAN(mw,i) == NULL) {
		MI_CTRAN(mw,i) = ct
		break
	    }
	}

	CT_MW(ct) = mw
	CT_WCSI(ct) = w1
	CT_WCSO(ct) = w2
	CT_NDIM(ct) = naxes
	CT_R(ct) = ct + ctlen
	call amovi (axis, CT_AXIS(ct,1), naxes)
	CT_LTM(ct) = coerce (ct + LEN_CTBASE, TY_STRUCT, TY_DOUBLE)
	CT_LTV(ct) = CT_LTM(ct) + matlen

	ltm = CT_LTM(ct)
	ltv = CT_LTV(ct)

	# We also need some full-system matrix and vector buffers.
	pdim = min (WCS_NDIM(w1), WCS_NDIM(w2))
	pdim = min (MI_NDIM(mw), pdim)

	i = pdim * pdim
	call salloc (i_ltm, i, TY_DOUBLE)
	call salloc (i_ltv, pdim, TY_DOUBLE)
	call salloc (o_ltm, i, TY_DOUBLE)
	call salloc (o_ltv, pdim, TY_DOUBLE)
	call salloc (t_ltm, i, TY_DOUBLE)
	call salloc (t_ltv, pdim, TY_DOUBLE)

	# Compute the transformation.  A transformation between any two
	# world systems W1 and W2 consists of the transformation W1->P
	# from W1 to the physical system, followed by a transformation
	# P->W2 to the second world system.  The linear portions of these
	# two transformations can be combined to produce a single linear
	# transformation, and if no WCS function calls are involved at
	# either end, the entire transformation reduces to a single linear
	# transformation defined by LTM and LTV.  Note that as far as we
	# are concerned here, the special world systems "logical" and
	# "physical" are just like other world systems, except that both are
	# always linear systems.  The linear term for the logical system is
	# the MWCS Lterm; for the physical system it is the identity matrix.

	# Set up the transformation W1->P.  First we must determine if there
	# are any WCS function calls.  We do this by going ahead and compiling
	# the "in" function calls in the CTRAN descriptor.

	do i = 1, naxes {
	    wfno = WCS_AXCLASS(w1,axis[i])

	    # Skip to next axis if no WCS function is assigned to this axis.
	    if (wfno == 0)
		next

	    # Has function call for this axis already been compiled?
	    newfunc = true
	    do j = 1, CT_NCALLI(ct) {
		fc = CT_FCI(ct,j)
		do k = 1, FC_NAXES(fc)
		    if (FC_AXIS(fc,k) == i)
			newfunc = false
	    }

	    # Compile a function call for the inverse transformation.
	    if (newfunc) {
		CT_NCALLI(ct) = CT_NCALLI(ct) + 1
		if (CT_NCALLI(ct) > MAX_CALL)
		    call syserrs (SYS_MWFCOVFL, system1)

		fc = CT_FCI(ct,CT_NCALLI(ct))
		wf = WCS_FUNC(w1,wfno)
		fn = WF_FN(wf)

		FC_CT(fc)    = ct
		FC_WCS(fc)   = w1
		FC_WF(fc)    = wf
		FC_FCN(fc)   = FN_INV(fn)
		FC_NAXES(fc) = WF_NAXES(wf)

		# Store CTRAN-relative list of axes in function call
		# descriptor.  Verify that all the axes needed for the
		# function call are included in the transformation.
		# This requirement can theoretically be relaxed in
		# some cases but this is not supported in MWCS.

		do j = 1, WF_NAXES(wf) {
		    for (k=1;  k <= naxes;  k=k+1)
			if (axis[k] == WF_AXIS(wf,j)) {
			    FC_AXIS(fc,j) = k
			    break
			}
		    if (k > naxes)
			call syserrs (SYS_MWMISSAX, system1)
		}

		# Call the function driver to perform any driver dependent
		# initialization.

		epa = FN_INIT(fn)
		if (epa != NULL)
		    call zcall2 (epa, fc, INVERSE)
	    }
	}

	# Prepare the linear part of the input transformation W1->P.
	# This is LTM=inv(CD), and for axis I, LTV[i]=(R[i]-inv(CD)*W)
	# if no function call, or LTV[i]=R[i] if there is a function
	# assigned to axis I which already deals with the W[i].  All
	# this is done in the full dimension of the internal system for
	# now; extraction of the portion of the full system affecting
	# the CTRAN axes is done later to permit verification of the
	# legality of the reduction step required.

	# Invert CD matrix.
	if (WCS_CD(w1) == NULL)
	    call mw_mkidmd (Memd[i_ltm], pdim)
	else
	    call mw_invertd (D(mw,WCS_CD(w1)), Memd[i_ltm], pdim)

	# If no function calls for an axis and W is set, LTV=(R-inv(CD)*W).
	if (WCS_W(w1) != NULL) {
	    call amovd (D(mw,WCS_W(w1)), Memd[i_ltv], pdim)
	    do i = 1, CT_NCALLI(ct) {
		fc = CT_FCI(ct,i)
		do j = 1, FC_NAXES(fc) {
		    k = axis[FC_AXIS(fc,j)]
		    Memd[i_ltv+k-1] = 0.0d0
		}
	    }
	    call mw_vmuld (Memd[i_ltm], Memd[i_ltv], Memd[t_ltv], pdim)

	    # Copy R to LTV.
	    if (WCS_R(w1) == NULL)
	        call anegd (Memd[t_ltv], Memd[i_ltv], pdim)
	    else
		call asubd (D(mw,WCS_R(w1)), Memd[t_ltv], Memd[i_ltv], pdim)

	} else {
	    # Copy R to LTV.
	    if (WCS_R(w1) == NULL)
	        call aclrd (Memd[i_ltv], pdim)
	    else
	        call amovd (D(mw,WCS_R(w1)), Memd[i_ltv], pdim)
	}

	# Now prepare the output side of the transformation, from P->W2.
	# Like the input half, this consists of a linear term and a list
	# of zero or more function calls.

	# Compile the "out" function calls in the CTRAN descriptor.
	do i = 1, naxes {
	    wfno = WCS_AXCLASS(w2,axis[i])

	    # Skip to next axis if no WCS function is assigned to this axis.
	    if (wfno == 0)
		next

	    # Has function call for this axis already been compiled?
	    newfunc = true
	    do j = 1, CT_NCALLO(ct) {
		fc = CT_FCO(ct,j)
		do k = 1, FC_NAXES(fc)
		    if (FC_AXIS(fc,k) == i)
			newfunc = false
	    }

	    # Compile a function call for the forward transformation.
	    if (newfunc) {
		CT_NCALLO(ct) = CT_NCALLO(ct) + 1
		if (CT_NCALLO(ct) > MAX_CALL)
		    call syserrs (SYS_MWFCOVFL, system2)

		fc = CT_FCO(ct,CT_NCALLO(ct))
		wf = WCS_FUNC(w2,wfno)
		fn = WF_FN(wf)

		FC_CT(fc)    = ct
		FC_WCS(fc)   = w2
		FC_WF(fc)    = wf
		FC_FCN(fc)   = FN_FWD(fn)
		FC_NAXES(fc) = WF_NAXES(wf)

		# Store CTRAN-relative list of axes in function call
		# descriptor.  Verify that all the axes needed for the
		# function call are included in the transformation.

		do j = 1, WF_NAXES(wf) {
		    for (k=1;  k <= naxes;  k=k+1)
			if (axis[k] == WF_AXIS(wf,j)) {
			    FC_AXIS(fc,j) = k
			    break
			}
		    if (k > naxes)
			call syserrs (SYS_MWMISSAX, system2)
		}

		# Call the function driver to perform any driver dependent
		# initialization.

		epa = FN_INIT(fn)
		if (epa != NULL)
		    call zcall2 (epa, fc, FORWARD)
	    }
	}

	# Prepare the linear part of the input transformation P->W2.
	# This is LTM=CD, and for axis I, LTV[i]=(W-CD*R) if no function
	# call, or LTV[i]=(-CD*R) if there is a function assigned to axis
	# I which already deals with the W[i].

	# Copy CD matrix to LTM.
	if (WCS_CD(w2) == NULL)
	    call mw_mkidmd (Memd[o_ltm], pdim)
	else
	    call amovd (D(mw,WCS_CD(w2)), Memd[o_ltm], pdim*pdim)

	# Copy -R to t_ltv.
	if (WCS_R(w2) == NULL)
	    call aclrd (Memd[t_ltv], pdim)
	else
	    call amulkd (D(mw,WCS_R(w2)), -1.0D0, Memd[t_ltv], pdim)

	# Compute -CD*R in LTV.
	call mw_vmuld (Memd[o_ltm], Memd[t_ltv], Memd[o_ltv], pdim)

	# If no function calls for an axis and W is set, LTV=(W-CD*R).
	if (WCS_W(w2) != NULL) {
	    call amovd (D(mw,WCS_W(w2)), Memd[t_ltv], pdim)
	    call aaddd (Memd[t_ltv], Memd[o_ltv], Memd[o_ltv], pdim)
	    do i = 1, CT_NCALLO(ct) {
		fc = CT_FCO(ct,i)
		do j = 1, FC_NAXES(fc) {
		    k = axis[FC_AXIS(fc,j)]		# undo +W[k]
		    lp = o_ltv + k - 1
		    Memd[lp] = Memd[lp] - Memd[t_ltv+k-1]
		}
	    }
	}

	# Now combine the linear terms of the input and output transformations
	# to produce the linear portion of the full transformation.

	call mw_mmuld (Memd[o_ltm], Memd[i_ltm], Memd[t_ltm], pdim)
	call mw_vmuld (Memd[o_ltm], Memd[i_ltv], Memd[t_ltv], pdim)
	call    aaddd (Memd[o_ltv], Memd[t_ltv], Memd[t_ltv], pdim)

	# Extract the rows of the full linear transformation which are used
	# for the axes involved in the transformation we are compiling.
	# In the process we must examine the off-diagonal elements of the
	# matrix to verify that the system does not include any dependencies
	# upon axes other than those included in the transformation we are
	# compiling.  (This restriction prohibits dimensional reduction via
	# an image section which results in loss of a rotated axis).

	do i = 1, naxes {
	    # Get matrix line pointers for axis[i].
	    ip = t_ltm + (axis[i]-1) * pdim
	    op = ltm + (i-1) * naxes

	    do j = 1, pdim {
		# Is column J used by transform?
		for (k=1;  k <= naxes;  k=k+1)
		    if (axis[k] == j)
			break

		# If column J is not used in the transform but is not zero,
		# then transform I is dependent upon physical axis J and
		# we cannot do the transform.  If column J is used in the
		# transform, copy the value to the final output matrix LTM
		# discarding unused columns as we go.

		if (k > naxes) {
		    # Check for dependency on axis outside transform.
		    if (abs(Memd[ip+j-1]) > EPSILOND*100.0D0)
			call syserr (SYS_MWROTDEP)
		} else {
		    # Add matrix element to final LTM.
		    Memd[op+k-1] = Memd[ip+j-1]
		}
	    }

	    # Copy the LTV vector element.
	    Memd[ltv+i-1] = Memd[t_ltv+axis[i]-1]
	}

	# Determine the transformation type.  This is LNR for a purely
	# linear transformation with no rotational (off-diagonal) terms,
	# LRO for a purely linear transform with rotational terms, and
	# GEN for everything else.

	if (CT_NCALLI(ct) > 0 || CT_NCALLO(ct) > 0)
	    CT_TYPE(ct) = GEN
	else {
	    CT_TYPE(ct) = LNR
	    do j = 1, naxes
		do i = 1, naxes
		    if (i != j) {
			lp = ltm + (j-1)*naxes + i-1
			if (abs(Memd[lp]) > EPSILOND*100.0D0) {
			    CT_TYPE(ct) = LRO
			    break
			}
		    }
	}

	# Prepare the single precision part of the transform.
	call amovi (Memi[CT_D(ct)], Memi[CT_R(ct)], ctlen)

	ct_r = CT_R(ct)
	CT_LTM(ct_r) = coerce (ct_r + LEN_CTBASE, TY_STRUCT, TY_REAL)
	CT_LTV(ct_r) = CT_LTM(ct_r) + matlen
	call achtdr (Memd[CT_LTM(ct)], Memr[CT_LTM(ct_r)], matlen)
	call achtdr (Memd[CT_LTV(ct)], Memr[CT_LTV(ct_r)], naxes)

	call sfree (sp)
	return (ct)
end
