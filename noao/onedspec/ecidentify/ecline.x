include	<smw.h>
include	"ecidentify.h"

# EC_LINE -- Get line corresponding to aperture.

int procedure ec_line (ec, ap)

pointer	ec		# EC pointer
int	ap		# Aperture

int	i

begin
	if (IS_INDEFI (ap))
	    return (INDEFI)

	do i = 1, EC_NLINES(ec)
	    if (ap == APS(ec,i))
		return (i)

	call error (0, "Image line for aperture number not found")
end
