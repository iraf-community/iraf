# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<pmset.h>
include	<imhdr.h>
include	<imio.h>

# IM_PMOPEN -- Open an image mask.  If the mask name is given is "BPM" (upper
# case) the bad pixel list for the reference image is opened, if the mask name
# is "EMPTY" an empty mask is opened, otherwise the mask name is taken to be
# the name of the file in which the mask is stored.  If there is no bad pixel
# list for the image an empty mask is opened.  If a more specialized mask is
# needed it should be opened or generated via explicit calls to the PMIO
# package.

pointer procedure im_pmopen (mask, mode, title, maxch, ref_im)

char	mask[ARB]		#I mask file name or "BPM"
int	mode			#I mode and flag bits
char	title[maxch]		#O mask title
int	maxch			#I max chars out
pointer	ref_im			#I reference image

pointer	sp, fname, pl, b_pl
long	axlen[PL_MAXDIM], v[PL_MAXDIM]
int	acmode, flags, naxes, depth

bool	streq()
pointer	pl_open(), pl_create()
errchk	syserr, pl_open, pl_create, pl_loadf, pl_loadim

string	s_empty	"EMPTY"		# the empty mask
string	s_bpl	"BPM"		# the reference image bad pixel list

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	acmode = PL_ACMODE(mode)
	flags  = PL_FLAGS(mode)

	# Get mask name for the BPM for the given reference image.
	if (streq (mask, s_bpl)) {
	    if (ref_im == NULL)
		call syserr (SYS_IMPLNORI)
	    iferr (call imgstr (ref_im, s_bpl, Memc[fname], SZ_FNAME))
		call strcpy (s_empty, Memc[fname], SZ_FNAME)
	} else
	    call strcpy (mask, Memc[fname], SZ_FNAME)

	pl = pl_open (NULL)

	# Open the named mask.
	if (acmode != NEW_IMAGE && acmode != NEW_COPY) {
	    if (streq (Memc[fname], s_empty)) {
		if (ref_im == NULL) {
		    call pl_close (pl)
		    call syserr (SYS_IMPLNORI)
		}
		call pl_ssize (pl, IM_NPHYSDIM(ref_im), IM_SVLEN(ref_im,1), 1)
	    } else {
		iferr (call pl_loadf (pl, Memc[fname], title, maxch)) {
		    call pl_close (pl)
		    pl = pl_open (NULL)
		    iferr (call pl_loadim (pl, Memc[fname], title, maxch)) {
			call pl_close (pl)
		        call erract (EA_ERROR)
		    }
		}
	    }

	    # Modify the mask according to the given flags, if any.
	    if (flags != 0) {
		call pl_gsize (pl, naxes, axlen, depth)
		call amovkl (1, v, PL_MAXDIM)

		if (and (flags, BOOLEAN_MASK) != 0 && depth > 1) {
		    b_pl = pl_create (naxes, axlen, 1)

		    if (and (flags, INVERT_MASK) != 0) {
		        call pl_rop (pl, v, b_pl, v, axlen, PIX_SRC)
			call amovkl (1, v, PL_MAXDIM)
		        call pl_rop (b_pl, v, b_pl, v, axlen, PIX_NOT(PIX_SRC))
		    } else {
		        call pl_rop (pl, v, b_pl, v, axlen, PIX_SRC)
		    }

		    call pl_close (pl)
		    pl = b_pl

		} else if (and (flags, INVERT_MASK) != 0)
		    call pl_rop (pl, v, pl, v, axlen, PIX_NOT(PIX_SRC))
	    }
	}

	call sfree (sp)
	return (pl)
end
