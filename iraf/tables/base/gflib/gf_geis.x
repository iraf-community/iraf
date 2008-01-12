include	"gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types

# GF_GEIS -- Return true if image is in STF format

bool procedure gf_geis (im)

pointer	im		# i: Image descriptor
#--
int	gf_imtype()

begin
	return (gf_imtype (im) == GEIS_FMT)
end
