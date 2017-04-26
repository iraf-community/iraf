include	"ecidentify.h"

# EC_NEXT -- Return the next feature.

int procedure ec_next (ec, feature)

pointer	ec		# ID pointer
int	feature		# Starting feature (input), next feature (returned)

int	i

begin
	for (i=feature+1; i<=EC_NFEATURES(ec); i=i+1)
	    if (APN(ec,i) == EC_AP(ec))
		break

	if (i <= EC_NFEATURES(ec))
	    feature = i
	else
	   i = EOF

	return (i)
end
