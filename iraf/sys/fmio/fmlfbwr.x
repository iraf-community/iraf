# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	"fmio.h"

# FM_LFBINWRITE -- Asynchronous blocked binary write to an lfile.  We deal
# only with binary data here; unpacking of text must be done at a higher
# level.  The basic procedure is to convert the indicated lfile segment into
# a range of lfile pages, then use the pagemap for the lfile to map these onto
# physical datafile pages.  I/O is done in chunks of contiguous pages until
# the requested amount of data has been transferred.  When writing at or
# beyond EOF, new pages are automatically allocated upon demand.

procedure fm_lfbinwrite (lf, buf, nbytes, offset)

pointer	lf			#I lfile descriptor
char	buf[ARB]		#I input data buffer
int	nbytes			#I nbytes to write
long	offset			#I lfile offset

pointer	fm, pm
int	status, chan, nleft, szbpage
int	lfile, l1,l2, p1,p2, d1,d2, ip, nb, nt
int	fmio_extend()

begin
	fm = LF_FM(lf)
	pm = LF_PAGEMAP(lf)

	# Verify descriptor.
	if (fm == NULL || pm == NULL) {
	    LF_STATUS(lf) = ERR
	    return
	} else
	    LF_STATUS(lf) = 0

	chan = FM_CHAN(fm)
	szbpage = FM_SZBPAGE(fm)
	lfile = (lf - FM_FTABLE(fm)) / LEN_FTE
	nleft = nbytes

	# Extend the pagemap?
	while (offset + nbytes > LF_NPAGES(lf)*szbpage + 1)
	    if (fmio_extend (fm, lfile, 1) == ERR) {
		LF_STATUS(lf) = ERR
		return
	    } else
		pm = LF_PAGEMAP(lf)

	# Map lfile offset,nbytes into a range of lfile pages.
	# I/O transfers are required to be aligned on page boundaries.

	l1 = (offset - 1) / szbpage + 1
	l2 = l1 + ((nleft + szbpage-1) / szbpage) - 1

	# Write the data from the user buffer to the physical datafile,
	# mapping lfile pages to physical offsets and moving data in chunks
	# of as many contiguous pages as possible.

	ip = 1
	for (p1=l1;  nleft > 0 && p1 <= l2;  p1=p2) {
	    # Get a contiguous range of datafile pages.
	    d1 = Memi[pm+p1-1]
	    for (p2=p1+1;  p2 <= l2;  p2=p2+1) {
		d2 = Memi[pm+p2-1]
		if (d2 - d1 != p2 - p1)
		    break
	    }

	    # Compute the logical transfer size NB, and the amount of data
	    # to be physically written NT.  The latter is always an integral
	    # number of datafile pages in size.  NOTE that this requires that
	    # the user buffer be an integral multiple of the page size, to
	    # prevent referencing off the end of the buffer.

	    nb = min (nleft, (p2 - p1) * szbpage)
	    nt = (nb + szbpage-1) / szbpage * szbpage
	    LF_LTSIZE(lf) = nb

	    # Write the file segment.
	    call zawrbf (chan, buf[ip], nt, (d1-1)*szbpage + FM_DATASTART(fm))
	    LF_FLAGS(lf) = or (LF_FLAGS(lf), LFF_IOINPROGRESS)
	    
	    # Bump the i/o counters.
	    ip = ip + nb / SZB_CHAR
	    nleft = nleft - nb

	    # If we didn't write all the data, wait until the write completes.
	    if (nleft > 0) {
		call zawtbf (chan, status)
		LF_FLAGS(lf) = and (LF_FLAGS(lf), not(LFF_IOINPROGRESS))
		if (status == ERR) {
		    LF_STATUS(lf) = ERR
		    return
		} else if (status == 0) {
		    break
		} else
		    LF_STATUS(lf) = LF_STATUS(lf) + min(LF_LTSIZE(lf),status)
	    }
	}

	# Update the lfile size counter.
	nb = offset + nbytes - 1
	if (nb > LF_FSIZE(lf)) {
	    LF_FSIZE(lf) = nb
	    FM_DHMODIFIED(fm) = YES
	}
end
