include	"apertures.h"

# EX_GMODAPS -- Get model apertures.
# If there are no model apertures in the database use the object apertures.
# If there are model apertures match them to the object apertures by ID.
# If there is no match it is an error.

procedure ex_gmodaps (modimage, aps, modaps, naps)

char	modimage[ARB]		# Model image name
pointer	aps[naps]		# Object apertures
pointer	modaps[naps]		# Model apertures (returned)
int	naps			# Number of object apertures

int	i, j, n
pointer	sp, temp
errchk	ap_dbread

begin
	call smark (sp)
	call salloc (temp, AP_MAXAPS, TY_INT)
	call amovki (NULL, Memi[temp], AP_MAXAPS)
	n = 0

        iferr (call ap_dbread (modimage, Memi[temp], n))
	    ;
	if (n == 0) {
	    call eprintf ("Using input image apertures for model image")
            do i = 1, naps
		call ap_copy (aps[i], modaps[i])
	} else {
	    do i = 1, naps {
		modaps[i] = NULL
		do j = 1, n {
		    if (AP_ID(aps[i]) == AP_ID(Memi[temp+j-1])) {
			call ap_copy (Memi[temp+j-1], modaps[i])
			break
		    }
		}
		if (modaps[i] == NULL)
	    	    call error (0,
			"Model aperture matching object aperture not found")
	    }
	}

	do i = 1, n
	    call ap_free (Memi[temp+i-1])
	call sfree (sp)
end
