include	<imhdr.h>
include	<mwset.h>
include	"ccdred.h"

# SET_SECTIONS -- Set the data section, ccd section, trim section and
# bias section.  Also set the WCS.

procedure set_sections (ccd)

pointer	ccd			# CCD structure (returned)

pointer	sp, str, mw, lterm, mw_openim()
int	nc, nl, c1, c2, cs, l1, l2, ls, ndim, mw_stati()
bool	streq()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)

	# The default data section is the entire image.
	c1 = 1
	c2 = nc
	cs = 1
	l1 = 1
	l2 = nl
	ls = 1
	call hdmgstr (IN_IM(ccd), "datasec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)||(cs!=1)||(ls!=1))
	    call error (0, "Error in DATASEC parameter")
	IN_C1(ccd) = c1
	IN_C2(ccd) = c2
	IN_L1(ccd) = l1
	IN_L2(ccd) = l2

	# The default trim section is the data section.
	# Defer limit checking until actually used.
	c1 = IN_C1(ccd)
	c2 = IN_C2(ccd)
	l1 = IN_L1(ccd)
	l2 = IN_L2(ccd)
	call clgstr ("trimsec", Memc[str], SZ_LINE)
	if (streq (Memc[str], "image"))
	    call hdmgstr (IN_IM(ccd), "trimsec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs!=1)||(ls!=1))
	    call error (0, "Error in TRIMSEC parameter")
	TRIM_C1(ccd) = c1
	TRIM_C2(ccd) = c2
	TRIM_L1(ccd) = l1
	TRIM_L2(ccd) = l2

	# The default bias section is the whole image.
	# Defer limit checking until actually used.
	c1 = 1
	c2 = nc
	l1 = 1
	l2 = nl
	call clgstr ("biassec", Memc[str], SZ_LINE)
	if (streq (Memc[str], "image"))
	    call hdmgstr (IN_IM(ccd), "biassec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs!=1)||(ls!=1))
	    call error (0, "Error in BIASSEC parameter")
	BIAS_C1(ccd) = c1
	BIAS_C2(ccd) = c2
	BIAS_L1(ccd) = l1
	BIAS_L2(ccd) = l2

	# The default ccd section is the size of the data section.
	c1 = 1
	c2 = IN_C2(ccd) - IN_C1(ccd) + 1
	l1 = 1
	l2 = IN_L2(ccd) - IN_L1(ccd) + 1
	call hdmgstr (IN_IM(ccd), "ccdsec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs != 1) || (ls != 1))
	    call error (0, "Error in CCDSEC parameter")
	CCD_C1(ccd) = c1
	CCD_C2(ccd) = c2
	CCD_L1(ccd) = l1
	CCD_L2(ccd) = l2
	if ((IN_C2(ccd)-IN_C1(ccd) != CCD_C2(ccd)-CCD_C1(ccd)) ||
	    (IN_L2(ccd)-IN_L1(ccd) != CCD_L2(ccd)-CCD_L1(ccd)))
	    call error (0, "Size of DATASEC and CCDSEC do not agree")

	# The default output data section is the input data section.
	OUT_C1(ccd) = IN_C1(ccd)
	OUT_C2(ccd) = IN_C2(ccd)
	OUT_L1(ccd) = IN_L1(ccd)
	OUT_L2(ccd) = IN_L2(ccd)

	# Set the physical WCS to be CCD coordinates.
	mw = mw_openim (IN_IM(ccd))
	ndim = mw_stati (mw, MW_NPHYSDIM)
	call salloc (lterm, ndim * (1 + ndim), TY_REAL)
	call mw_gltermr (mw, Memr[lterm+ndim], Memr[lterm], ndim)
	Memr[lterm] = IN_C1(ccd) - CCD_C1(ccd)
	Memr[lterm+1] = IN_L1(ccd) - CCD_L1(ccd)
	Memr[lterm+ndim] = 1. / cs
	Memr[lterm+ndim+1] = 0.
	Memr[lterm+ndim+ndim] = 0.
	Memr[lterm+ndim+ndim+1] = 1. / ls
	call mw_sltermr (mw, Memr[lterm+ndim], Memr[lterm], ndim)
	call mw_saveim (mw, IN_IM(ccd))
	call mw_saveim (mw, OUT_IM(ccd))
	call mw_close (mw)

	call sfree (sp)
end
