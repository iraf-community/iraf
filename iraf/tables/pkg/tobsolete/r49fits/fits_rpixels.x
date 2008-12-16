include <fset.h>
include <mii.h>
include <mach.h>

define  BUF_LEN    32760

# RFT_INIT_READ_PIXELS and READ_PIXELS -- Read pixel data with record buffering
# and data type conversion.  The input data must meet the MII standard
# except for possibly having the least significant byte first.
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

int	npix_record	# Number of pixels per input record
int	bitpix		# Bits per pixel (must correspond to an MII type)
int	lsbf		# byte swap?
int	spp_type	# SPP data type to be returned

# entry rft_read_pixels (fd, buffer, npix)
int	rft_read_pixels
int	rft_ieee_read
int	fd		# Input file descriptor
pointer	buf
char	buffer[BUF_LEN]	# Output buffer
int	npix		# Number of pixels to read

int	swap
int	ty_mii, ty_spp, npix_rec, nch_rec, sz_rec, nchars, len_mii, recptr
int	bufsize, i, n, ip, op, nd
pointer	mii, spp, bufrd

long	read()
int	sizeof(), nint_rec
size_t	miipksize()
errchk	mfree, malloc, read
data	mii/NULL/, spp/NULL/, bufrd/NULL/

include "rfits.com"

begin
	ty_spp = spp_type
	swap = lsbf
	npix_rec = npix_record
	nch_rec = npix_rec * sizeof (ty_spp)

	if (ty_spp == TY_CHAR || ty_spp == TY_LONG) {
	   ty_mii = bitpix
	   len_mii = miipksize (npix_rec, ty_mii)
	   sz_rec = len_mii
	   if (mii != NULL)
	      call mfree (mii, TY_CHAR)
	   call malloc (mii, len_mii, TY_CHAR)
	   ip = nch_rec
	} else { # is REAL or DOUBLE
	   if (bufrd != NULL)
	      call mfree (bufrd, TY_INT)
	   nint_rec = npix_rec * sizeof (ty_spp) / SZ_INT
	   call malloc (bufrd, nint_rec, TY_INT)
	   ip = npix_rec
	}
	if (spp != NULL)
	    call mfree (spp, TY_CHAR)
	call malloc (spp, nch_rec, TY_CHAR)

	return (OK)

entry	rft_read_pixels (fd, buffer, npix, recptr, bufsize)

	nchars = npix * sizeof (ty_spp)
	op = 0

	repeat {
	    # If data is exhausted read the next record
	    if (ip == nch_rec) {
		iferr (i = read (fd, Memc[mii], sz_rec)) {
		       call fseti (fd, F_VALIDATE, bufsize * sz_rec)
		       call printf ("Error reading record %d\n")
		       if (mod (recptr + 1, bufsize) == 0)
			   call pargi ((recptr + 1) / bufsize)
		       else
			   call pargi ((recptr + 1) / bufsize + 1)
		       i = read (fd, Memc[mii], sz_rec)
		}
		if (i == EOF)
		    return (EOF)

		if (swap == YES)
		   switch (ty_mii) {
		   case MII_SHORT:
		      call bswap2 (Memc[mii], 1, Memc[mii], 1,
				sz_rec * SZB_CHAR)
		   case MII_LONG:
		      call bswap4 (Memc[mii], 1, Memc[mii], 1,
				sz_rec * SZB_CHAR)
		   }

		if (byte_input == YES)
		   call amovc (Memc[mii], Memc[spp], npix_rec)
		else
		  call miiupk (Memc[mii], Memc[spp], npix_rec, ty_mii, ty_spp)

		ip = 0
		recptr = recptr + 1
	    }

	    n = min (nch_rec - ip, nchars - op)
	    if (byte_input == YES)
	       call bytmov (Memc[spp], ip+1, buffer, op+1, n)
	    else
	       call amovc (Memc[spp+ip], buffer[1+op], n)
	    ip = ip + n
	    op = op + n

	} until (op == nchars)

	return (npix)

entry	rft_ieee_read(fd, buf, npix, recptr, bufsize)

	op = 0

	repeat {
	    # If data is exhausted read the next record
	    if (ip == npix_rec) {
		iferr (i = read (fd, Meml[bufrd], nch_rec)) {
		       call fseti (fd, F_VALIDATE, bufsize * nch_rec)
		       call printf ("Error reading record %d\n")
		       if (mod (recptr + 1, bufsize) == 0)
			   call pargi ((recptr + 1) / bufsize)
		       else
			   call pargi ((recptr + 1) / bufsize + 1)
		       i = read (fd, Meml[bufrd], nch_rec)
		}
		ip = 0
		nd = 0
		recptr = recptr + 1
	    }

 	    n = min (npix_rec - ip, npix - op)
	    if (ty_spp == TY_REAL) {
#              call sun2vaxr (Meml[bufrd+ip], Memr[buf+op], n)	    
	       call ieevupkr (Meml[bufrd+ip], Memr[buf+op], n)	    
	    } else {
#	       call sun2vaxd (Meml[bufrd+nd], Memd[buf+op], n)
	       call ieevupkd (Meml[bufrd+nd], Memd[buf+op], n)
	       # There are 2 Meml per Memd
	       nd = nd + n * 2
	    }
	       ip = ip + n
	       op = op + n

	} until (op == npix)

	return (npix)
end
