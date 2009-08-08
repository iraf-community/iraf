include <fset.h>
include <mach.h>
include <mii.h>
include "wfits.h"

define BUF_LEN   32760

# WFT_INIT_WRITE_PIXELS -- This procedure calculates the input and
# output buffer sizes and allocates the required space.

procedure wft_init_write_pixels (npix_record, spp_type, obitpix)

size_t	npix_record		# number of data pixels per record
int	spp_type		# pixel data type
int	obitpix			# output bits per pixel

# entry wft_write_pixels, wft_write_last_record

int	fd			# output file descriptor
char	buffer[BUF_LEN]		# input buffer
size_t	npix			# number of pixels in the input buffer
size_t	nrecords		# number of FITS records written

char	blank
int	ty_mii, ty_spp
size_t	npix_rec, nch_rec, len_mii, sz_rec, nchars, n, nrec
pointer	spp, mii, ip, op
short	s_val

int	sizeof()
long	fstatl()
size_t	miipksize()
errchk	malloc, mfree, write, miipak, amovc
data	mii /NULL/, spp/NULL/

include "wfits.com"

begin
	# Change input parameters into local variables
	ty_mii = obitpix
	ty_spp = spp_type
	npix_rec = npix_record
	nch_rec = npix_rec * sizeof (ty_spp)
	blank = ' '

	len_mii = miipksize (npix_rec, ty_mii)
	sz_rec = len_mii

	# Allocate space for the buffers
	if (spp != NULL)
	   call mfree (spp, TY_CHAR)
	call malloc (spp, nch_rec, TY_CHAR)
	
	if (mii != NULL)
	   call mfree (mii, TY_CHAR)
	call malloc (mii, len_mii, TY_CHAR)

	op = 0
	nrec = 0

	return

# WFT_WRITE_PIXELS -- Wrt_pixels gets an image line and places it in the output
# buffer. When the output buffer is full the data are packed by the mii routines
# and written to the specified output.

entry	wft_write_pixels (fd, buffer, npix)

	nchars = npix * sizeof (ty_spp)
	ip = 0
     if (ieee == YES && (ty_spp == TY_LONG || ty_spp == TY_DOUBLE ||
				      ext_type == BINTABLE)) {
	repeat {
	    # Fill output buffer.
	    n = min (nch_rec - op, nchars - ip)
	    if (ext_type == BINTABLE){
	       call bytmov (buffer, ip+1, Memc[spp], op+1, n)
	    }
	    else
	       call amovc (buffer[1 + ip], Memc[spp + op], n)
	    ip = ip + n
	    op = op + n

	    # Write output record.
	    if (op == nch_rec) {
	        if (ext_type == BINTABLE)
		   sz_rec = nch_rec/SZB_CHAR
		else
		   sz_rec = nch_rec
		iferr (call write (fd, Memc[spp], sz_rec)) {
		    call eprintf (" File incomplete: %d logical data")
		         call pargz (nrec)
		    call eprintf (" (%d byte) records written\n")
			 call pargz (sz_rec)
		    call error (19, "WRT_RECORD: Error writing data record.")
		}
		if (fstatl (fd, F_NCHARS) != sz_rec) {
		    call flush (STDOUT)
	    	    call error (17, "WRT_PIXELS: Error writing image record.")
	        }
		nrec = nrec + 1
		op = 0
	    }

	} until (ip == nchars)
     } else {
	repeat {
	    # Fill output buffer.
	    n = min (nch_rec - op, nchars - ip)
	    call amovc (buffer[1 + ip], Memc[spp + op], n)
	    ip = ip + n
	    op = op + n

	    # Write output record.
	    if (op == nch_rec) {
		call miipak (Memc[spp], Memc[mii], npix_rec, ty_spp, ty_mii)
		iferr (call write (fd, Memc[mii], sz_rec)) {
		   call eprintf (" File incomplete: %d logical data")
		       call pargz (nrec)
	           call eprintf (" (%d byte) records written\n")
		       call pargz (sz_rec)
		   call error (19, "WRT_RECORD: Error writing data record.")
	        }
		if (fstatl (fd, F_NCHARS) != sz_rec) {
		    call flush (STDOUT)
	    	    call error (17, "WRT_PIXELS: Error writing image record.")
	        }
		nrec = nrec + 1
		op = 0
	    }

	} until (ip == nchars)
      }
	return

# WFT_WRITE_LAST_RECORD -- Procedure to write the last partially filled record
# to tape. Fill with blanks if header record otherwise fill with zeros.

entry	wft_write_last_record (fd, nrecords)

	if (op != 0) {
	    # Blank or zero fill.
	    n = nch_rec - op
	    if (ieee == YES && ext_type == BINTABLE) {
	        n = (n-1)/SZB_CHAR + 1
		op = (op-1)/SZB_CHAR + 1
		s_val = 0
	        call amovks (s_val, Memc[spp + op], n)
            } else {
	        if (ty_spp == TY_CHAR) {
		   call amovkc (blank, Memc[spp + op], n)
	        } else {
		   s_val = 0
		   call amovks (s_val, Memc[spp + op], n)
		}
	    }
	    # Write last record.

	    
            if (ieee == YES && (ty_spp == TY_LONG || ty_spp == TY_DOUBLE ||
				      ext_type == BINTABLE)) {
	       if (ext_type == BINTABLE)
		  sz_rec = nch_rec/SZB_CHAR
	       else
		  sz_rec = nch_rec
	       iferr (call write (fd, Memc[spp], sz_rec)) {
		   call printf (" File incomplete: %d logical data")
		       call pargz (nrec)
		   call printf (" (%d byte) records written\n")
		       call pargz (sz_rec)
		       call error (19, "WRT_RECORD: Error writing last record.")
	       }
	       if (fstatl (fd, F_NCHARS) != sz_rec) {
		  call flush (STDOUT)
		  call error (19,
			"WRT_LAST_RECORD: Error writing last data record.")
	       }
	    } else {
	       call miipak (Memc[spp], Memc[mii], npix_rec, ty_spp, ty_mii)
	       iferr (call write (fd, Memc[mii], sz_rec)) {
                   call printf (" File incomplete: %d logical data")
		       call pargz (nrec)
		   call printf (" (%d byte) records written\n")
		       call pargz (sz_rec)
		   call error (19, "WRT_RECORD: Error writing last record.")
	       }
	       if (fstatl (fd, F_NCHARS) != sz_rec) {
		   call flush (STDOUT)
	           if (ty_spp == TY_CHAR)
	               call error (18,
			  "WRT_LAST_RECORD: Error writing last header record.")
	           else
		       call error (19,
			  "WRT_LAST_RECORD: Error writing last data record.")
	       }
	    }

	    nrec = nrec + 1
	    op = 0	
	}

	nrecords = nrec
end
