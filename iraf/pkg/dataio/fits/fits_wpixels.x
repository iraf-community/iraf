# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <fset.h>
include "wfits.h"

# WFT_INIT_WRITE_PIXELS -- This procedure calculates the input and
# output buffer sizes based in the spp and mii data types and allocates
# the required space.

procedure wft_init_write_pixels (npix_record, spp_type, bitpix, blkfac)

size_t	npix_record		# number of data pixels per record
int	spp_type		# pixel data type
int	bitpix			# output bits per pixel
size_t	blkfac			# fits blocking factor (0 for disk)

# entry wft_write_pixels, wft_write_last_record

int	fd			# output file descriptor
char	buffer[1]		# input buffer
size_t	npix			# number of pixels in the input buffer
size_t	nrecords		# number of FITS records written

long	l_val
char	blank, zero
int	ty_mii, ty_spp
size_t	npix_rec, nch_rec, len_mii, sz_rec, nchars, n, nrec, bf
long	szblk
pointer	spp, mii, ip, op

int	sizeof()
long	fstatl()
size_t	miipksize()
long	note(), modl()
errchk	malloc, mfree, write, miipak, amovc
data	mii /NULL/, spp/NULL/

begin
	# Change input parameters into local variables.
	ty_mii = bitpix
	ty_spp = spp_type
	npix_rec = npix_record
	nch_rec = npix_rec * sizeof (ty_spp)
	bf = blkfac
	blank = ' '
	zero = 0

	# Compute the size of the mii buffer.
	len_mii = miipksize (npix_rec, ty_mii)
	sz_rec = len_mii

	# Allocate space for the buffers.
	if (spp != NULL)
	    call mfree (spp, TY_CHAR)
	call malloc (spp, nch_rec, TY_CHAR)
	if (mii != NULL)
	    call mfree (mii, TY_CHAR)
	call malloc (mii, len_mii, TY_CHAR)

	op = 0
	nrec = 0

	return

# WFT_WRITE_PIXELS -- Wft_wrt_pixels gets an image line and places it in the
# output buffer. When the output buffer is full the data are packed by the mii
# routines and written to the specified output.

entry	wft_write_pixels (fd, buffer, npix)

	nchars = npix * sizeof (ty_spp)
	ip = 0

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
	            if (ty_spp == TY_CHAR) {
		        call printf (" File incomplete: %d logical header")
			    call pargz (nrec)
			call printf (" (2880 byte) records written\n")
	                call error (18,
			    "WRT_RECORD: Error writing header record.")
	            } else {
		        call printf (" File incomplete: %d logical data")
			    call pargz (nrec)
			call printf (" (2880 byte) records written\n")
		        call error (19,
			    "WRT_RECORD: Error writing data record.")
		    }
		}

		nrec = nrec + 1
		op = 0
	    }

	} until (ip == nchars)

	return


# WFT_WRITE_LAST_RECORD -- Procedure to write the last partially filled record
# to tape. Fill with blanks if header record otherwise fill with zeros.

entry	wft_write_last_record (fd, nrecords)

	if (op != 0) {

	    # Blank or zero fill the last record.
	    n = nch_rec - op
	    if (ty_spp == TY_CHAR)
		call amovkc (blank, Memc[spp + op], n)
	    else
		call amovkc (zero, Memc[spp + op], n)

	    # Write last record.
	    call miipak (Memc[spp], Memc[mii], npix_rec, ty_spp, ty_mii)
	    iferr (call write (fd, Memc[mii], sz_rec)) {
	        if (ty_spp == TY_CHAR) {
		    call printf ("File incomplete: %d logical header")
			call pargz (nrec)
		    call printf (" (2880 byte) records written\n")
	            call error (18,
			"WRT_LAST_RECORD: Error writing last header record.")
	        } else {
		    call printf ("File incomplete: %d logical data")
			call pargz (nrec)
		    call printf (" (2880 byte) records written\n")
		    call error (19,
			"WRT_LAST_RECORD: Error writing last data record.")
		}
	    }


	    nrec = nrec + 1

	    # Pad out the record if the blocking is non-standard.
	    szblk = fstatl (fd, F_BUFSIZE) * SZB_CHAR
	    l_val = FITS_RECORD
	    if ((bf > 0) && modl(szblk, l_val) != 0 &&
	        (ty_spp != TY_CHAR)) {
		szblk = szblk / SZB_CHAR
		n = note (fd) - 1
		l_val = n
		if (modl(l_val, szblk) == 0)
		    n = 0
		else
		    n = szblk - modl(l_val, szblk)
		for (op = 1; op <= n; op = op + nch_rec) {
		    szblk = min (nch_rec, n - op + 1)
		    call amovkc (zero, Memc[spp], szblk)
		    #call write (fd, Memc[spp], szblk)
		}
	    }

	}

	nrecords = nrec
end
