include	"apertures.h"

define	NRANGES	100


# AP_SELECT -- Select apertures.
# The AP_SELECT field of the aperture structure is set.

procedure ap_select (apertures, aps, naps)

char	apertures[ARB]		#I Aperture selection string
pointer	aps[ARB]		#U Aperture pointers
int	naps			#I Number of apertures

pointer	sp, ranges
int	i, decode_ranges()
bool	is_in_range()

begin
	# Check if apertures are defined.
	if (naps < 1)
	    return

	call smark (sp)
	call salloc (ranges, 3*NRANGES, TY_INT)

	# Decode aperture string.
	if (decode_ranges (apertures, Memi[ranges], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list")

	# Select apertures.
	do i = 1, naps {
	    if (is_in_range (Memi[ranges], AP_ID(aps[i])))
		AP_SELECT(aps[i]) = YES
	    else
		AP_SELECT(aps[i]) = NO
	}

	call sfree (sp)
end
