include	"ecidentify.h"

# EC_DELETE -- Delete a feature.

procedure ec_delete (ec, feature)

pointer	ec			# ID pointer
int	feature			# Feature to be deleted

int	i

begin
	if (feature == 0)
	    return

	do i = feature + 1, EC_NFEATURES(ec) {
	    APN(ec,i-1) = APN(ec,i)
	    LINE(ec,i-1) = LINE(ec,i)
	    ORDER(ec,i-1) = ORDER(ec,i)
	    PIX(ec,i-1) = PIX(ec,i)
	    FIT(ec,i-1) = FIT(ec,i)
	    USER(ec,i-1) = USER(ec,i)
	    FWIDTH(ec,i-1) = FWIDTH(ec,i)
	    FTYPE(ec,i-1) = FTYPE(ec,i)
	}
	EC_NFEATURES(ec) = EC_NFEATURES(ec) - 1
	EC_NEWFEATURES(ec) = YES
end
