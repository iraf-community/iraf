include <mii.h>
include <mach.h>
include "rpds.h"

# PDS_INIT_READ_SCAN -- A single PDS scan consists of one or more physical
# records. Each scan begins with a 10 byte scan start parameter.
# A "full" record will contain p_npts_per_rec data points, usually
# set equal to 2000. The last record may be short.
# If the number of records per scan is 1, the first record
# may also be shorter than p_npts_per_record.
# If the input tape is 7 track the actual number of points on the tape
# is rounded up to an integral multiple of 5.
# The input parameters of the procedure are an array of parameters of
# type long which are derived from the PDS header. The routine
# calculates the sizes of the records in chars, allocates the space
# required to hold the record buffer, and initializes the line count.

# PDS_READ_SCAN -- Reads an entire PDS scan a record at a time. This procedure
# uses the MII unpack routine which is machine dependent. The bitpix
# must correspond to an MII type. The routine returns the number of
# data points per scan or EOF. READ_SCAN  converts the 12 or 10 bit data
# values to short integers and flips every other scan if the PDS scan
# is a raster scan.

int procedure pds_init_read_scan (parameters)

long	parameters[LEN_PAR_ARRAY]	# array of header parameters

# entry pds_read_scan (fd, scanbuf)

int	pds_read_scan()
int	fd
short	scanbuf[1]

int	maxbufsize, len_mii, linecount, temp
int	npts_first, npts_full, npts_last
int	sz_first, sz_full, sz_last
int	p_npts_per_rec, p_npts_per_scan, p_nrecs_per_scan, p_scantype
int	nrec, op, nchars
pointer	mii

int	miilen(), pds_roundup(), read()

errchk	miilen, malloc, mfree, miiupk, read

data	mii/NULL/

include "rpds.com"

begin
	# Allocate sufficient space for the largest record in a scan
	# Rounding up if the recordsize is not an integral number of chars.

	p_nrecs_per_scan = P_NRECS_PER_SCAN(parameters)
	p_npts_per_scan = P_NPTS_PER_SCAN(parameters)
	p_npts_per_rec = P_NPTS_PER_REC(parameters)
	p_scantype = P_SCANTYPE(parameters)
	temp = p_nrecs_per_scan

	maxbufsize = pds_roundup (SCANSTART + 2*p_npts_per_rec, SZB_CHAR)
	maxbufsize = pds_roundup (maxbufsize, 2) / 2
	len_mii = miilen (maxbufsize, MII_SHORT)
	if (mii != NULL)
	    call mfree (mii, TY_INT)
	call malloc (mii, len_mii, TY_INT)

	# Calculate the number of data points, and the number of chars
	# in a first, full and last record rounding up if the record is
	# not an integral number of chars

	linecount = 1
	npts_first = min (p_npts_per_scan, p_npts_per_rec)
	if (ninetrack == NO) {
	    sz_first = pds_roundup (npts_first, SCANSTART/2)
	    sz_first = pds_roundup (2*sz_first + SCANSTART, SZB_CHAR) / SZB_CHAR
	}  else
	    sz_first = pds_roundup (2*npts_first + SCANSTART,
	        SZB_CHAR) / SZB_CHAR
	npts_full = p_npts_per_rec
	sz_full = pds_roundup (npts_full*2, SZB_CHAR) / SZB_CHAR
	npts_last = mod (p_npts_per_scan, p_npts_per_rec)
	if (ninetrack == NO) {
	    sz_last = pds_roundup (npts_last, SCANSTART/2)
	    sz_last = pds_roundup (2*sz_last, SZB_CHAR) / SZB_CHAR
	} else
	    sz_last = pds_roundup (2*npts_last, SZB_CHAR) / SZB_CHAR

	return (OK)

entry	pds_read_scan (fd, scanbuf)   
	
	op = 1

	# Loop over the number of records in a scan
	do nrec = 1, temp {

	    # Get 1st record, remove the 10 bytes containing the scan start
	    # parameter, and unpack the data.
	    if (nrec == 1) {
		nchars = read (fd, Memi[mii], sz_first)
		if (nchars == EOF)
		    return (EOF)
		else if (nchars < sz_first)
		    call error (6, "Short record encountered.")
		call bytmov (Memi[mii], SCANSTART + 1, Memi[mii], 1,
					npts_first*2)
		call miiupk (Memi[mii], scanbuf[op], npts_first,
					    MII_SHORT, TY_SHORT)
		op = op + npts_first

	    # Get last record which may be short
	    } else if (nrec == p_nrecs_per_scan) {
		nchars = read (fd, Memi[mii], sz_last)
		if (nchars == EOF)
		    return (EOF)
		else if (nchars < sz_last)
		    call error (6, "Short record encountered.")
		call miiupk (Memi[mii], scanbuf[op], npts_last,
					    MII_SHORT, TY_SHORT)
		op = op + npts_last

	    # Get a full record
	    } else {
		nchars = read (fd, Memi[mii], sz_full)
		if (nchars == EOF)
		    return (EOF)
		else if ( nchars < sz_full)
		    call error (6, "Short record encountered.")
		call miiupk (Memi[mii], scanbuf[op], npts_full,
					    MII_SHORT, TY_SHORT)
		op = op + npts_full
	    }
	}

	# Convert PDS 10 or 12 bit values to short SPP values
	call pds_apdp8s (scanbuf, scanbuf, p_npts_per_scan)

	# If the image is a raster scan flip every other line
	if (p_scantype == RASTER && mod (linecount, 2) == 0)
	    call pds_aflips (scanbuf, p_npts_per_scan)

	linecount = linecount + 1
	return (op - 1)
end


# PDS_AFLIPS -- Procedure  to flip a short vector in place

procedure pds_aflips (buf, npix)

short	buf[npix]
int	npix

int	n_total, n_half, i, j

begin
	n_half = npix/2
	n_total = npix + 1
	for (i=1; i <= n_half; i = i + 1) {
	    j = buf[i]
	    buf[i] = buf[n_total - i]
	    buf[n_total - i] = j
	}
end


# PDS_ROUNDUP -- Procedure to round an integer to the next highest number
# divisible by base.

int procedure pds_roundup (number, base)

int	number, base
int	value

begin
	if (mod(number, base) == 0)
	    return (number)
	else {
	    value = (number/base + 1) * base
	    return (value)
	}
end
