# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <mii.h>
include <mach.h>

# RFT_INIT_READ_PIXELS and READ_PIXELS -- Read pixel data with record buffering
# and data type conversion.  The input data must meet the MII standard
# except for possibly in the case of integers having the least significant
# byte first.
#
# Read data in records of len_record and convert to the specified IRAF
# data type.  Successive calls of rft_read_pixels returns the next npix pixels.
# Read_pixels returns EOF or the number of pixels converted.
# Init_read_pixels must be called before read_pixels.
#
# Error conditions are:
# 1. A short input record
# 2. Error in converting the pixels by miiup.
#
# This routine is based on the MII unpack routine which is machine dependent.
# The bitpix must correspond to an MII type.  If the lsbf (least significant
# byte first) flag is YES then the pixels do not satisfy the MII standard.
# In this case the bytes are first swapped into most significant byte first
# before the MII unpack routine is called.

int procedure rft_init_read_pixels (npix_record, bitpix, lsbf, spp_type)

int	npix_record	# number of pixels per input record
int	bitpix		# bits per pixel (must correspond to an MII type)
int	lsbf		# byte swap?
int	spp_type	# SPP data type to be returned

# entry rft_read_pixels (fd, buffer, npix)

int	rft_read_pixels
int	fd		# input file descriptor
char	buffer[1]	# output buffer
int	npix		# number of pixels to read

int	swap
int	ty_mii, ty_spp, npix_rec, nch_rec, sz_rec, nchars, len_mii, recptr
int	bufsize, i, n, ip, op
pointer	mii, spp

int	rft_getbuf(), sizeof(), miilen()
errchk	miilen, mfree, malloc, rft_getbuf, miiupk
data	mii/NULL/, spp/NULL/

begin
	ty_mii = bitpix
	ty_spp = spp_type
	swap = lsbf
	npix_rec = npix_record
	nch_rec = npix_rec * sizeof (ty_spp)

	len_mii = miilen (npix_rec, ty_mii)
	sz_rec = len_mii * SZ_INT32

	if (mii != NULL)
	    call mfree (mii, TY_INT)
	call malloc (mii, len_mii, TY_INT)

	if (spp != NULL)
	    call mfree (spp, TY_CHAR)
	call malloc (spp, nch_rec, TY_CHAR)

	ip = nch_rec
	return (OK)

entry	rft_read_pixels (fd, buffer, npix, recptr, bufsize)

	nchars = npix * sizeof (ty_spp)
	op = 0

	repeat {

	    # If data is exhausted read the next record
	    if (ip == nch_rec) {

		i = rft_getbuf (fd, Memi[mii], sz_rec, bufsize, recptr)
		if (i == EOF)
		    return (EOF)

		if (swap == YES)
		    switch (ty_mii) {
		    case MII_SHORT:
			call bswap2 (Memi[mii], 1, Memi[mii], 1,
				sz_rec * SZB_CHAR)
		    case MII_LONG:
			call bswap4 (Memi[mii], 1, Memi[mii], 1,
				sz_rec * SZB_CHAR)
		    }

		call miiupk (Memi[mii], Memc[spp], npix_rec, ty_mii, ty_spp)

		ip = 0
		#recptr = recptr + 1
	    }

	    n = min (nch_rec - ip, nchars - op)
	    call amovc (Memc[spp+ip], buffer[1+op], n)
	    ip = ip + n
	    op = op + n

	} until (op == nchars)

	return (npix)
end


# RFT_GETBUF -- Procedure to get the buffer.

int procedure rft_getbuf (fd, buf, sz_rec, bufsize, recptr)

int	fd		# file descriptor
char	buf[ARB]	# buffer to be filled
int	sz_rec		# size in chars of record to be read
int	bufsize		# buffer size in records
int	recptr		# last successful FITS record read

int	i, nchars
int	read(), fstati()
errchk	read

begin
	nchars = 0
	repeat {
	    iferr {
	        i = read (fd, buf[nchars+1], sz_rec - nchars)
	    } then {
	        call printf ("Error reading FITS record %d\n")
	        if (mod (recptr + 1, bufsize) == 0)
		    call pargi ((recptr + 1) / bufsize)
	        else
		    call pargi ((recptr + 1) / bufsize + 1)
	        call fseti (fd, F_VALIDATE, fstati (fd, F_SZBBLK) / SZB_CHAR)
	        i = read (fd, buf[nchars+1], sz_rec - nchars)
	    }

	    if (i == EOF)
		break
	    else
	        nchars = nchars + i

	} until (nchars >= sz_rec)

	if ((i == EOF) && (nchars == 0))
	    return (EOF)
	else {
	    recptr = recptr + 1
	    return (nchars)
	}
end
