include <syserr.h>
include	<imhdr.h>
include	<imio.h>

# GF_UPUSER -- Update the user area of an image from a spool file

procedure gf_upuser (im, spool, acmode)

pointer	im		# i: image descriptor
int	spool		# i: spool file with new user area
int	acmode		# i: access mode for user area (WRITE_ONLY or APPEND)
#--
int	buflen, ua_len, out
pointer	ua

int	stropen()

begin
	# Check for existence of spool file

	if (spool == NULL)
	    return

	# Get user area length

	buflen = LEN_IMDES + IM_LENHDRMEM(im)
	ua_len = (buflen - IMU) * SZ_STRUCT - 1
	
	# Open user area as a string file

	ua = IM_USERAREA(im)
	out = stropen (Memc[ua], ua_len, acmode)

	# Copy the keywords into the user area

	call seek (spool, BOF)

	iferr {
	    call fcopyo (spool, out)
	} then {
	    call syserr (SYS_IDBOVFL)
	}

	# Close string file

	call strclose (out)

end
