include <error.h>
include <imhdr.h>
include <imio.h>
include <imset.h>
include <fio.h>
include <tbset.h>
include "rfits.h"

int procedure rft_image_ext (im, fits_fd, iraffile, template, fits)
pointer	im			# i,o: Image descriptor
int	fits_fd			# i: FITS file descriptor
char	iraffile[SZ_FNAME]	# i: image name
char	template[SZ_FNAME]	# i: template filename
pointer fits			# i: FITS descriptor

pointer imt
int	fd_usr, nread
char	cluster[SZ_FNAME]
int	rft_read_header(), open()
errchk rft_read_header, rft_opnim, close, rft_read_image

include "rfits.com" 

begin
	# Open spool file to contain the fits header
	fd_usr = open ("fits_hdr", READ_WRITE, SPOOL_FILE)
	if (FITS_XTEN(fits) == IMAGE)
	   EXTEND(fits) = IMAGE  # This is a message for the call below.
  	nread = rft_read_header (fits_fd, fd_usr, fits)

        if (nread != EOF) {
	   call imgcluster (iraffile, cluster, SZ_FNAME)
	   if (FITS_XTEN(fits) == IMAGE) {
	      if (im == NULL) {
		 if (gkey != IMH)
		    gkey = DEF_GPB
	         if (old_name == YES)
		    call change_name (cluster, fits)
		 call print_header (fits, "", cluster)
		 FITS_XTEN(fits) = NO
	      } else
	         EXTEND(fits) = YES  # This is a message for the call below.
	   }
	   call rft_opnim (template, cluster, fd_usr, fits, nread, im, imt)
	   call close (fd_usr)
	   call rft_read_image (fits_fd, fits, im)
	   # Reset the flag in case is the last extension in the FITS file
	   # and we  need to close the output image.
	   FITS_XTEN(fits) = IMAGE
	} else {
	   call printf ("End of file while reading data.\n")
	   call close (fd_usr)
	   call erract(EA_WARN)
	   return
	}

	return (0)
end
