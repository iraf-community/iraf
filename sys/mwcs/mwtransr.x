# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MW_TRANSLATE -- Translate the logical system, i.e., perform a linear
# transformation of the logical system by modifying the Lterm of the MWCS.

procedure mw_translater (mw, ltv_1, ltm, ltv_2, ndim)

pointer	mw			#I pointer to MWCS descriptor
real	ltv_1[ndim]		#I input translation vector
real	ltm[ndim,ndim]		#I linear transformation matrix
real	ltv_2[ndim]		#I output translation vector
int	ndim			#I dimensionality of transform

int	nelem
pointer	sp, d_ltm, d_ltv1, d_ltv2

begin
	call smark (sp)
	nelem = ndim * ndim
	call salloc (d_ltm, nelem, TY_DOUBLE)
	call salloc (d_ltv1, ndim, TY_DOUBLE)
	call salloc (d_ltv2, ndim, TY_DOUBLE)

	call achtrd (ltm, Memd[d_ltm], nelem)
	call achtrd (ltv_1, Memd[d_ltv1], ndim)
	call achtrd (ltv_2, Memd[d_ltv2], ndim)

	call mw_translated (mw, Memd[d_ltv1], Memd[d_ltm], Memd[d_ltv2], ndim)
	call sfree (sp)
end
