# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	"fmio.h"

# FM_LFBINREAD -- Asynchronous blocked binary read from an lfile.  We deal
# only with binary data here; unpacking of text must be done at a higher
# level.  The basic procedure is to convert the indicated lfile segment into
# a range of lfile pages, then use the pagemap for the lfile to map these onto
# physical datafile pages.  I/O is done in chunks of contiguous pages until
# the requested amount of data has been transferred.

procedure fm_lfbinread (lf, buf, maxbytes, offset)

pointer	lf			#I lfile descriptor
char	buf[ARB]		#O output data buffer
int	maxbytes		#I max bytes to read
long	offset			#I lfile offset

pointer	fm, pm
int	status, chan, nleft, szbpage
int	l1,l2, p1,p2, d1,d2, op, nb, np

begin
	fm = LF_FM(lf)
	pm = LF_PAGEMAP(lf)

	# Verify descriptor.
	if (fm == NULL || pm == NULL) {
	    LF_STATUS(lf) = ERR
	    return
	} else
	    LF_STATUS(lf) = 0

	np = LF_NPAGES(lf)
	chan = FM_CHAN(fm)
	    
	# Check that the read is in bounds.
	nleft = min (offset + maxbytes, LF_FSIZE(lf) + 1) - offset
	if (nleft <= 0)
	    return		# read at EOF

	# Map lfile offset,nbytes into a range of lfile pages.
	# I/O transfers are required to be aligned on page boundaries.
	# Note that less than full page may be transferred in a read.

	szbpage = FM_SZBPAGE(fm)
	l1 = (offset - 1) / szbpage + 1
	l2 = l1 + ((nleft + szbpage-1) / szbpage) - 1

	# Read the data from the physical datafile into the user buffer,
	# mapping lfile pages to physical offsets and moving data in chunks
	# of as many contiguous pages as possible.

	op = 1
	for (p1=l1;  nleft > 0 && p1 <= l2;  p1=p2) {
	    # Get a contiguous range of datafile pages.
	    d1 = Memi[pm+p1-1]
	    for (p2=p1+1;  p2 <= l2;  p2=p2+1) {
		d2 = Memi[pm+p2-1]
		if (d2 - d1 != p2 - p1)
		    break
	    }

	    # Read in the file segment.
	    nb = min (nleft, (p2 - p1) * szbpage)
	    call zardbf (chan, buf[op], nb, (d1-1)*szbpage + FM_DATASTART(fm))
	    LF_FLAGS(lf) = or (LF_FLAGS(lf), LFF_IOINPROGRESS)
	    LF_LTSIZE(lf) = nb
	    
	    # Bump the i/o counters.
	    op = op + nb / SZB_CHAR
	    nleft = nleft - nb

	    # If we didn't read all the data, wait until the read completes.
	    if (nleft > 0) {
		call zawtbf (chan, status)
		LF_FLAGS(lf) = and (LF_FLAGS(lf), not(LFF_IOINPROGRESS))
		if (status == ERR) {
		    LF_STATUS(lf) = ERR
		    return
		} else if (status == 0) {
		    break
		} else
		    LF_STATUS(lf) = LF_STATUS(lf) + status
	    }
	}
end
