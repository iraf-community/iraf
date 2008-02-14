# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"stf.h"

# STF_MERGEGPB -- Merge the non-reserved parameters from an existing GPB into
# a new GPB; called to construct a new GPB when an image is opened in new-copy
# mode.  Since the new copy may not be the same size and dimension as the
# original, the reserved parameters must be set up fresh for the new copy
# image, i.e., we cannot simply copy them from the old image.  Likewise, the
# WCS must be transformed if the new copy image does not geometrically overlay
# the original.
#
# NOTE:  no longer called by stf_opix; save this code for future use!
# <dlb--11/4/87>

procedure stf_mergegpb (n_im, o_im)

pointer	n_im		# new copy image
pointer	o_im		# image being copied

bool	match
int	n_i, o_i, n, ip, axis
int	up_psize
pointer	sp, cd_pat, n_stf, o_stf, n_pp, o_pp
int	strncmp(), strlen(), patmake(), patmatch(), ctoi()

begin
	call smark (sp)
	call salloc (cd_pat, SZ_LINE, TY_CHAR)

	# Make a pattern to match the CDa_b parameter names.
	if (patmake ("CD[0-9]_[0-9]", Memc[cd_pat], SZ_LINE) < 0)
	    ; # cannot happen

	n_stf = IM_KDES(n_im)
	o_stf = IM_KDES(o_im)

	# Examine each parameter in the old GPB and make an entry for the new
	# ones in the new GPB.  Note that all we are doing here is defining
	# the structure; the GPB data is not physically written until the new
	# header is updated on disk.  The FITS encoded values for the GPB
	# parameters will already have been copied to the user area of the
	# new image.

	up_psize = 0
	for (o_i=1;  o_i <= STF_PCOUNT(o_stf);  o_i=o_i+1) {
	    o_pp = STF_PDES(o_stf,o_i)
	    n = strlen (P_PTYPE(o_pp))

	    if (P_PTYPE(o_pp) == 'C')
		if (strncmp  (P_PTYPE(o_pp), "CRPIX", 5) == 0 ||
		    strncmp  (P_PTYPE(o_pp), "CRVAL", 5) == 0 ||
		    strncmp  (P_PTYPE(o_pp), "CTYPE", 5) == 0 ||
		    patmatch (P_PTYPE(o_pp), Memc[cd_pat]) > 0) {

		    ip = 6
		    if (ctoi (P_PTYPE(o_pp), ip, axis) <= 0)
			axis = IM_MAXDIM + 1
		    if (axis <= STF_NAXIS(n_stf))
		    	next
		}

	    # Is there a parameter of the same name in the new descriptor?
	    match = false
	    for (n_i=1;  n_i <= STF_PCOUNT(n_stf);  n_i=n_i+1) {
		n_pp = STF_PDES(n_stf,n_i)
		if (strncmp (P_PTYPE(o_pp), P_PTYPE(n_pp), n) == 0) {
		    match = true
		    break
		}
	    }

	    # If there was no match for the parameter, add a definition for
	    # it to the GPB descriptor for the new image.

	    if (!match) {
		n = STF_PCOUNT(n_stf) + 1
		if (n > MAX_PCOUNT)
		    call error (4, "stf_merge: too many group parameters")

		STF_PCOUNT(n_stf) = n
		up_psize = up_psize + P_PSIZE(o_pp)
		n_pp = STF_PDES(n_stf,n)
	
		P_SPPTYPE(n_pp)	= P_SPPTYPE(o_pp)
		P_PSIZE(n_pp)	= P_PSIZE(o_pp)
		P_LEN(n_pp)	= P_LEN(o_pp)

		call strcpy (P_PTYPE(o_pp), P_PTYPE(n_pp), SZ_PTYPE)
		call strcpy (P_PDTYPE(o_pp), P_PDTYPE(n_pp), SZ_PDTYPE)
	    }
	}

	# Moved the PSIZE, SZGROUP calculations here to fix error in the
	# computation of PSIZE--dlb, 11/2/87

	STF_PSIZE(n_stf) = STF_PSIZE(n_stf) + up_psize
	STF_SZGROUP(n_stf) = STF_SZGROUP(n_stf) +
	    up_psize / (SZB_CHAR * NBITS_BYTE)

	call sfree (sp)
end
