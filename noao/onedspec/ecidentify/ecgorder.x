include	"ecidentify.h"

# EC_GLINE -- Get line of data.

procedure ec_gline (ec, line)

pointer	ec		# EC pointer
int	line		# Image line

begin
	if (IS_INDEFI(line))
	    return

	EC_IMLINE(ec) = EC_IMDATA(ec) + (line - 1) * EC_NPTS(ec)
	EC_PIXLINE(ec) = EC_PIXDATA(ec) + (line - 1) * EC_NPTS(ec)
	EC_FITLINE(ec) = EC_FITDATA(ec) + (line - 1) * EC_NPTS(ec)
end
