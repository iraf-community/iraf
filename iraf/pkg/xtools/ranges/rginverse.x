# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_INVERSE -- Invert a set of ordered and merged ranges.

procedure rg_inverse (rg, rmin, rmax)

pointer	rg			# RANGES pointer
int	rmin			# Minimum value of window
int	rmax			# Maximum value of window

int	i
pointer rgtmp

pointer	rg_window()

begin
	call malloc (rgtmp, LEN_RG + 2 * (RG_NRGS(rg) + 1), TY_STRUCT)
	RG_NRGS(rgtmp) = RG_NRGS(rg) + 1

	RG_X1(rgtmp, 1) = rmin

	do i = 1, RG_NRGS(rg) {
	    RG_X2(rgtmp, i) = RG_X1(rg, i) - 1
	    RG_X1(rgtmp, i+1) = RG_X2(rg, i) + 1
	}

	RG_X2(rgtmp, RG_NRGS(rgtmp)) = rmax

	call rg_free (rg)
	rg = rg_window (rgtmp, rmin, rmax)
	call rg_free (rgtmp)
end
