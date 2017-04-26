# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <syserr.h>
include <plset.h>
include <imhdr.h>
include <imio.h>
include <mach.h>
include "fxf.h"


# FXF_PLREAD -- Read a PLIO mask stored in a FITS binary table extension
# and load it into an image descriptor.
#
# There is a builtin assumption in this code (also in fxf_plwrite) that
# masks will not be more than 3-dimensional.  This could be generalized
# if necessary, but we have never seen a mask of dimensionality higher
# than 3.  The dimensionality, size, and depth of the mask is preserved.

procedure fxf_plread (im)

pointer	im			#I image descriptor

char	kwname[SZ_KEYWORD]
pointer sp, fk, pl, lp, ip, ix
long	data_offset, data_len, heap_offset, llen, loff
int	naxes, axlen[IM_MAXDIM], depth, maxlen
int	fd, i, j, nelem, nlines, v[PL_MAXDIM], maxoff, nbytes

long	note()
bool	streq()
int	imgeti(), pl_create(), miireadi(), miireads()
errchk	imgeti, pl_create, miireadi, miireads, seek, pl_update, syserrs

begin
	call smark (sp)

	fk = IM_KDES(im)
	fd = IM_HFD(im)

	# The maximum encoded line list length is (normally) passed in via
	# the binary table format keywords, and stored in FIT_PLMAXLEN.

	maxlen = FIT_PLMAXLEN(fk)
	if (maxlen <= 0)
	    maxlen = DEF_PLMAXLEN

	# Scratch buffer for encoded line lists.
	call salloc (lp, maxlen, TY_SHORT)

	# Get the dimensionality and size of the stored mask.
	call amovki (1, axlen, IM_MAXDIM)
	naxes = imgeti (im, "ZNAXIS")
	call fxf_filter_keyw (im, "ZNAXIS")
	do i = 1, naxes {
	    call sprintf (kwname, LEN_CARD, "ZNAXIS%d")
		call pargi(i)
	    axlen[i] = imgeti (im, kwname)
	    call fxf_filter_keyw (im, kwname)
	    call sprintf (kwname, LEN_CARD, "ZTILE%d")
		call pargi(i)
	    call fxf_filter_keyw (im, kwname)
	}

	# Get the mask depth, passed as compression algorithm parameter
	# number 1 for a PLIO-compressed image.

	depth = DEF_PLDEPTH
	ifnoerr (call imgstr (im, "ZNAME1", kwname, SZ_KEYWORD)) {
	    if (streq (kwname, "depth"))
		iferr (depth = imgeti (im, "ZVAL1"))
		    depth = DEF_PLDEPTH
	    call fxf_filter_keyw (im, "ZNAME1")
	    call fxf_filter_keyw (im, "ZVAL1")
	    call fxf_filter_keyw (im, "ZBITPIX")
	    call fxf_filter_keyw (im, "ZIMAGE")
	}

	# Create an initially empty mask of the given size.
	pl = pl_create (naxes, axlen, depth)

	# Create a buffer for the line list index (maxdim 3 assumed).
	nlines = axlen[3] * axlen[2]
	call salloc (ix, nlines * 2, TY_INT)

	# Compute the file offsets of the table data and heap areas.  The
	# file position is assumed to be already positioned at the start
	# of the data area of the file.

	data_offset = note (fd)
	data_len = FIT_LENAXIS(fk,3) * FIT_LENAXIS(fk,2) * FIT_LENAXIS(fk,1)
	heap_offset = data_offset + data_len/SZB_CHAR

	# Read the line list index from the input file.  The index contains
	# one entry for every line in the (possibly multidimensional) image.
	# Each entry consists of two integer values, the length of the
	# stored line list, and the heap offset (in bytes) of the stored list.

	nelem = miireadi (fd, Memi[ix], nlines * 2)
	if (nelem != nlines * 2)
	    call syserrs (SYS_FXFRMASK, IM_NAME(im))
	
	# Find out the maximum offset value to determine if they were
	# written using the 2 byte units rather than the standard (byte unit)

	maxoff = 0
        ip = ix
	do j = 1, axlen[3] {
            do i = 1, axlen[2] {
	       maxoff = max (maxoff, Memi[ip+1]+2*Memi[ip]) 
   	       ip = ip + 2
	    }
	}

	if (maxoff < (FIT_PCOUNT(fk) - maxoff/2)) {
	    nbytes = 1
        } else {	
	    nbytes = 2
	}

	# Read the line list data and insert it into the PLIO mask.  
	# pl_update will be called for each line of the mask even if multiple
	# lines point to the same line list data, but pl_update will sort
	# all this out and restore the multiple references as the mask is
	# built.

	ip = ix
	v[1] = 1

	do j = 1, axlen[3] {
	    v[3] = j
	    do i = 1, axlen[2] {
		v[2] = i

		llen = Memi[ip]

		# This offset on the table data is in byte units, convert
		# to short.

		loff = Memi[ip+1] / nbytes

		call seek (fd, heap_offset + loff)
		nelem = miireads (fd, Mems[lp], llen)
		if (nelem != llen)
		    call syserrs (SYS_FXFRMASK, IM_NAME(im))

		call pl_update (pl, v, Mems[lp])

		ip = ip + 2
	    }
	}

	# Set up IMIO descriptor.
	call amovl (axlen, IM_LEN(im,1), IM_MAXDIM)
	call amovl (axlen, IM_PHYSLEN(im,1), IM_MAXDIM)
        IM_NDIM(im) = naxes
	IM_PIXTYPE(im) = TY_INT
	IM_PL(im) = pl

	call sfree (sp)
end
