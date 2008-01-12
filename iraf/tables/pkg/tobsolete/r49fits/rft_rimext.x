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
int	fd_usr, nread, open(), rft_read_header()
char	cluster[SZ_FNAME]

include "rfits.com" 
include "tab.com"

begin
	# Open spool file to contain the fits header
	fd_usr = open ("fits_hdr", READ_WRITE, SPOOL_FILE)
	iferr {
	   if (ext_type == IMAGE)
	      EXTEND(fits) = IMAGE  # This is a message for the call below.
  	   nread = rft_read_header (fits_fd, fd_usr, fits)
	} then {
	   call erract (EA_WARN)
	   return
	}
        if (nread != EOF) {
	iferr {
	   call imgcluster (iraffile, cluster, SZ_FNAME)
	   if (ext_type == IMAGE) {
	      if (im == NULL) {
		 if (gkey != IMH)
		    gkey = DEF_GPB
	         if (old_name == YES)
		    call change_name (cluster, fits)
		 call print_header (fits, "", cluster)
		 ext_type = 0
	      } else
	         EXTEND(fits) = YES  # This is a message for the call below.
	   }
	   call rft_opnim (template, cluster, fd_usr, fits, nread, im, imt)
	   call close (fd_usr)
	   call rft_read_image (fits_fd, fits, im)
	   # Reset the flag in case is the last extension in the FITS file
	   # and we  need to close the output image.
	   ext_type = IMAGE
	} then {
	   call erract (EA_WARN)
	   return
	}
	} else {
	   call printf ("End of data\n")
	   call close (fd_usr)
	   return (EOF)
	}

	return (0)
end
