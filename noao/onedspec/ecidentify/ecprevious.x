include	"ecidentify.h"

# EC_PREVIOUS -- Return the previous feature.

int procedure ec_previous (ec, feature)

pointer	ec		# ID pointer
int	feature		# Starting feature (input), previous feature (returned)

int	i

begin
	for (i=feature-1; i>0; i=i-1)
	    if (APN(ec,i) == EC_AP(ec))
		break

	if (i > 0)
	    feature = i
	else
	    i = EOF

	return (i)
end
