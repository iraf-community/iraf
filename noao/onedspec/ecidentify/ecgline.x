include	<smw.h>
include	"ecidentify.h"

# EC_GLINE -- Get line of data.

procedure ec_gline (ec, line)

pointer	ec		# EC pointer
int	line		# Image line

begin
	if (IS_INDEFI(line))
	    return

	EC_SH(ec) = SH(ec,line)
	EC_NPTS(ec) = SN(EC_SH(ec))
	EC_IMLINE(ec) = SY(EC_SH(ec))
	EC_PIXLINE(ec) = EC_PIXDATA(ec) + (line - 1) * EC_NCOLS(ec)
	EC_FITLINE(ec) = EC_FITDATA(ec) + (line - 1) * EC_NCOLS(ec)
end
