include	"gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types

# GF_GROUPS -- Return YES if image has multiple groups or extensions

int procedure gf_groups (im)

pointer	im		# i: image descriptor
#--
int	groups

bool	imgetb()
int	btoi(), imaccf(), gf_imtype()

begin
	# WARNING: This test does not work reliably on fits files
	# opened in NEW_FILE or NEW_COPY mode because of how the 
	# fits kernel is implemented

	switch (gf_imtype (im)) {
	case GEIS_FMT:
	    groups = btoi (imgetb (im, "GROUPS"))

	case FITS_FMT:
	    if (imaccf (im, "EXTEND") == YES) {
		groups = btoi (imgetb (im, "EXTEND"))
	    } else {
		groups = NO
	    }

	default:
	    groups = NO
	}

	return (groups)
end
