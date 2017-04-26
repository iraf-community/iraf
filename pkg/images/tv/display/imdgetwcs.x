# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"zdisplay.h"
include	"iis.h"

# IMD_GETWCS -- Get the saved WCS for the given frame of the given display
# device.  (No great attempt at generality here).
# [INTERNAL ROUTINE - RESTRICTED USE].
#
# Example:
#
#	dev$pix - m51  B  600s
#	1. 0. 0. -1. 1. 512. 36. 320.0713 1
#
# The file format is the image title, followed by a line specifying the
# coordinate transformation matrix (6 numbers: a b c d tx ty) and the
# greyscale transformation (z1 z2 zt).
#
# The procedure returns OK if the WCS for the frame is sucessfully accessed,
# or ERR if the WCS cannot be read.  In the latter case the output WCS will
# be the default unitary WCS.

int procedure imd_getwcs (frame, server, image, sz_image, title, sz_title,
	a, b, c, d, tx, ty)

int	frame			#I frame (wcs) number of current device
int	server			#I device is a display server
char	image[ARB]		#O image name
int	sz_image		#I max image name length
char	title[ARB]		#O image title string
int	sz_title		#I max image title length
real	a, d			#O x, y scale factors
real	b, c			#O cross terms (rotations)
real	tx, ty			#O x, y offsets

char	ch
int	fd, chan, status, wcs_status, zt
real	z1, z2
pointer	sp, dir, device, fname, wcstext
int	envfind(), strncmp(), open(), fscan(), nscan(), stropen(), iisflu()

include "iis.com"

begin
	call smark (sp)
	call salloc (dir, SZ_PATHNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (wcstext, SZ_WCSTEXT, TY_CHAR)

	wcs_status = OK

	# Retrieve the WCS text and open a file descriptor on it.

	if (server == YES) {
	    # Retrieve the WCS information from a display server.
	    chan = iisflu(FRTOCHAN(frame))

	    # Cannot use iisio here as the data is byte packed and cannot be
	    # swapped (while the header still has to be swapped).

	    if (iis_version > 0) {
		iis_valid = NO
	        call iishdr (IREAD+PACKED, SZ_WCSTEXT, WCS, 1, 0, chan, 0)
	        call iisio (Memc[wcstext], SZ_WCSTEXT, status)
	        if (status > 0)
		    call strupk (Memc[wcstext], Memc[wcstext], SZ_WCSTEXT)

	        iferr (fd = stropen (Memc[wcstext], SZ_WCSTEXT, READ_ONLY))
		    fd = NULL

	    } else {
	        call iishdr (IREAD+PACKED, SZ_OLD_WCSTEXT, WCS, 0, 0, chan, 0)
	        call iisio (Memc[wcstext], SZ_OLD_WCSTEXT, status)
	        if (status > 0)
		    call strupk (Memc[wcstext], Memc[wcstext], SZ_OLD_WCSTEXT)

	        iferr (fd = stropen (Memc[wcstext], SZ_OLD_WCSTEXT, READ_ONLY))
		    fd = NULL
	    }

	} else {
	    # Construct the WCS filename, "dir$device_frame.wcs".  (Copied from
	    # the make-WCS code in t_display.x).

	    if (envfind ("wcsdir", Memc[dir], SZ_PATHNAME) <= 0)
		if (envfind ("WCSDIR", Memc[dir], SZ_PATHNAME) <= 0)
		    if (envfind ("uparm", Memc[dir], SZ_PATHNAME) <= 0)
			call strcpy ("tmp$", Memc[dir], SZ_PATHNAME)

	    if (envfind ("stdimage", Memc[device], SZ_FNAME) <= 0)
		call strcpy ("display", Memc[device], SZ_FNAME)

	    # Get the WCS file filename.
	    call sprintf (Memc[fname], SZ_PATHNAME, "%s%s_%d.wcs")
		call pargstr (Memc[dir])
		if (strncmp (Memc[device], "imt", 3) == 0)
		    call pargstr ("imtool")
		else
		    call pargstr (Memc[device])
		call pargi (frame)

	    if (sz_image > 0)
		image[1] = EOS
	    if (sz_title > 0)
		title[1] = EOS

	    # Get the saved WCS.
	    iferr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE))
		fd = NULL
	}

	# Decode the WCS from the WCS text.
	if (fd != NULL) {
	    image[1] = EOS
	    title[1] = EOS

	    if (fscan (fd) != EOF) {
		# Decode "image - title".
		if (sz_image > 0)
		    call gargwrd (image, sz_image)
		if (sz_title > 0) {
		    call gargwrd (title, sz_title)
		    repeat {
			call gargc (ch)
		    } until (!IS_WHITE(ch))
		    title[1] = ch
		    call gargstr (title[2], sz_title - 1)
		}

		# Decode the WCS information.
		if (fscan (fd) != EOF) {
		    call gargr (a)
		    call gargr (b)
		    call gargr (c)
		    call gargr (d)
		    call gargr (tx)
		    call gargr (ty)
		    call gargr (z1)
		    call gargr (z2)
		    call gargi (zt)
		    if (nscan() == 9)
			wcs_status = OK

	            if (iis_version > 0) {
			if (fscan (fd) != EOF) {
		            call gargstr (iis_region, SZ_FNAME)
		            call gargr (iis_sx)
		            call gargr (iis_sy)
		            call gargi (iis_snx)
		            call gargi (iis_sny)
		            call gargi (iis_dx)
		            call gargi (iis_dy)
		            call gargi (iis_dnx)
		            call gargi (iis_dny)
			}
		        if (nscan() == 9) {
			    if (fscan (fd) != EOF)
		                call gargstr (iis_objref, SZ_FNAME)
		            if (nscan() == 1)
			        iis_valid = YES
			} else
			    iis_valid = NO
	            } else {
		        if (nscan() != 9) {
			    # Set up the unitary transformation if we
			    # cannot retrieve the real one.
			    a  = 1.0
			    b  = 0.0
			    c  = 0.0
			    d  = 1.0
			    tx = 1.0
			    ty = 1.0
			    wcs_status = ERR
			}
	            }
		}
	    }
	}


	if (fd != NULL)
	    call close (fd)
	call sfree (sp)

	return (wcs_status)
end
