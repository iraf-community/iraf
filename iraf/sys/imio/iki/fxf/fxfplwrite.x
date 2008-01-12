# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <mach.h>
include <imio.h>
include <imhdr.h>
include <mii.h>
include <plset.h>
include <pmset.h>
include "fxf.h"


# FXFPLWRITE.X -- Routines to handle masks in FITS extensions.
#
#	 fxf_plwrite     (im, fd)
#	 fxf_plinfo      (im, maxlen, pcount, depth)
#	 fxf_pl_adj_heap (im, hdr_fd, pcount)
#        fxf_copy_adj    (im, in_fd, hdroff, poff, datasize)
#	 fxf_plpf        (im)
#


# FXF_PLWRITE -- Write the data from a PLIO mask into the data area of a
# FITS compressed image (ZIMAGE) binary table extension.  The data is
# written to the file pointed to by file descriptor FD.
#
# The data to be written consists of the data for the ZIMAGE binary table
# records, followed by the heap area of the BINTABLE extension, which
# contains the actual encoded line lists.  For simplicity we assume that
# the table contains only one column, the COMPRESSED_DATA column, which is
# of type variable length integer array.  Each element of this column is a
# BINTABLE variable length array descriptor which physically consists of two
# integer values: an integer giving the length of the stored array (encoded
# line list), followed by an integer (in byte unit) giving the offset of 
# the array data (encoded line list) in the heap area.  Multiple variable 
# length array descriptors may point to the same stored array, and in 
# fact PLIO uses this feature to implement compression in the Y direction 
# (adjacent mask lines will point to the same encoded line list).  
# The code here supports masks of up to 3 dimensions.

procedure fxf_plwrite (im, fd)

pointer	im			#I image descriptor
int	fd			#I output file descriptor

int	i, j, v_in[PL_MAXDIM], lp_len
int	naxes, axlen[PL_MAXDIM], depth
int	heap_offset, ep_off, lp_off, vararray[2]
pointer	pl, lp, op, emptyline, lastline

int	pl_llen()
pointer	pl_access(), pl_emptyline()
errchk	pl_access

begin
	pl = IM_PL(im)
	call pl_gsize (pl, naxes, axlen, depth)

	# Write the COMPRESSED_DATA table column.  This is an index giving
	# the length and heap offset of the encoded PLIO line list for each
	# line of the image.  Multiple image lines (index entries) may point
	# to the same stored line list: this happens if a mask line is empty
	# (the empty line) or if successive lines are all the same.  For the
	# sake of simplicity, only masks of up to 3 dimensions are supported.

	op = 0
	heap_offset = 0
	emptyline = pl_emptyline (pl)
	ep_off = -1
	lastline = NULL
	lp_off = -1
	call amovkl(long(1), v_in, PL_MAXDIM)

	do j = 1, axlen[3] {
	    v_in[3] = j
	    do i = 1, axlen[2] {
		v_in[2] = i
		lp = pl_access (pl, v_in)
		lp_len = pl_llen (Mems[lp])

		if (lp == emptyline && ep_off >= 0)
		    op = ep_off
	        else if (lp == lastline)
		    op = lp_off
		else
		    op = heap_offset

		vararray[1] = lp_len

		# The offsets on the FITS BINTABLE is in byte unit 
		# as establish by the FITS standard.

		vararray[2] = op * 2              # Byte offset

		call miiwritei (fd, vararray, 2)

		lastline = lp
		lp_off = op 
		if (lp == emptyline && ep_off < 0)
		    ep_off = op

		if (op == heap_offset)
		    heap_offset = heap_offset + lp_len
	    }
	}
	# Now write the line list data to the heap area.  The logic here must
	# follow that above or the line offsets won't match.

	ep_off = -1
	lp_off = -1
	lastline = NULL

	do j = 1, axlen[3] {
	    v_in[3] = j
	    do i = 1, axlen[2] {
		v_in[2] = i
		lp = pl_access (pl, v_in)
		lp_len = pl_llen (Mems[lp])

		if (lp == emptyline && ep_off >= 0)
		    next
		else if (lp == lastline)
		    next

		call miiwrites (fd, Mems[lp], lp_len)

		lastline = lp
		if (lp == emptyline && ep_off < 0)
		    ep_off = 0
	    }
	}
end


# FXF_PLINFO -- Examine a PLIO mask and compute the maximum length of an
# encoded line list, and the storage in bytes required to store the mask
# data in the heap area of a FITS binary table.

procedure fxf_plinfo (im, maxlen, pcount, depth)

pointer	im			#I image descriptor
int	maxlen			#O maximum line list length
int	pcount			#O storage required to store mask (bytes)
int	depth			#O mask depth

int	naxes, axlen[PL_MAXDIM]
int	i, j, v_in[PL_MAXDIM], lp_len
int	heap_offset, ep_off, lp_off
pointer	pl, lp, op, emptyline, lastline

int	pl_llen()
pointer	pl_access(), pl_emptyline()
errchk	pl_access

begin
	pl = IM_PL(im)
	call pl_gsize (pl, naxes, axlen, depth)

	op = 0
	maxlen = 0
	heap_offset = 0
	emptyline = pl_emptyline (pl)
	ep_off = -1
	lastline = NULL
	lp_off = -1
	call amovkl(long(1), v_in, PL_MAXDIM)

	# The following must duplicate the logic above for determining what
	# gets written to the heap area.  All we are doing here is computing
	# the amount of heap storage required to store the compressed mask.

	do j = 1, axlen[3] {
	    v_in[3] = j
	    do i = 1, axlen[2] {
		v_in[2] = i
		lp = pl_access (pl, v_in)
		lp_len = pl_llen (Mems[lp])
		maxlen = max (maxlen, lp_len)

		if (lp == emptyline && ep_off >= 0)
		    op = ep_off
		else if (lp == lastline)
		    op = lp_off
		else
		    op = heap_offset

		lastline = lp
		lp_off = op
		if (lp == emptyline && ep_off < 0)
		    ep_off = op

		if (op == heap_offset)
		    heap_offset = heap_offset + lp_len
	    }
	}

	pcount = heap_offset * (SZ_SHORT * SZB_CHAR)
end


# FXF_PL_ADJ_HEAP -- Resize heap when we have a hole bigger than 2880 bytes
# or if we overwrite the next extension.

procedure fxf_pl_adj_heap (im, hdr_fd, pcount)

pointer	im			#I imio descriptor
int	hdr_fd			#U file descriptor
int	pcount			#I new heap size in bytes

pointer fk, hdrp, pixp
int	datasize, hdroff, diff, nb, group, i

begin
	fk = IM_KDES(im)

	# Calculate the size of the TABLE data. (8 bytes per line)
	datasize = FIT_LENAXIS(fk,1)*FIT_LENAXIS(fk,2)*
		FIT_LENAXIS(fk,3)
	datasize = (datasize + pcount)/SZB_CHAR

	call fxf_not_incache(im)
	hdrp = FIT_HDRPTR(fk)
	pixp = FIT_PIXPTR(fk)
	group = FIT_GROUP(fk)

	hdroff = Memi[hdrp+group]

	# Calculate the amount of space left  or grown in the heap
	# as a result of the READ-WRITE operation on the data.

	diff = datasize - (Memi[hdrp+group+1] - Memi[pixp+group])
	
	# See if the new data overwrites the next unit or
	# there is a hole with more than 2880 bytes.

	if ( (diff > 0) || ((-diff / 2880) > 0) ) {

	    # Adjust the header and pixel offset for subsequent groups.
	    # Add header size.
	    datasize = datasize + Memi[pixp+group] - Memi[hdrp+group]
	    call fxf_copy_adj (im, hdr_fd, hdroff, Memi[hdrp+group+1], datasize)

	    if (diff > 0)
		nb = FITS_LEN_CHAR (diff)
	    else
		nb = (diff / 1440) * 1440

	    # Update FK cache offset values
	    do i = group+1, FIT_NUMOFFS(fk) {
	        Memi[hdrp+i] = Memi[hdrp+i] + nb
		if (Memi[pixp+i] > 0) {
		    Memi[pixp+i] = Memi[pixp+i] + nb
		} else
		    break
	    }
	}
end


# FXF_COPY_ADJ -- Make a copy of the input file extending or shrinking
# the heap area.

procedure fxf_copy_adj (im, in_fd, hdroff, poff, datasize)
	
pointer	im		#I Imio descriptor
int	in_fd		#I Input file descriptor
int	hdroff		#I Header offset
int 	poff		#I Pixel offset
int	datasize	#I New FITS unit size

pointer sp, tempfile, outname
int	nchars, junk, inoff, out_fd, size
int	fnldir(), fnroot(), open(), note()
errchk	open, note, seek, close, delete, rename
errchk	fxf_make_adj_copy, fxf_write_blanks

begin
	call smark (sp)
	call salloc (tempfile, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)

	nchars = fnldir (IM_HDRFILE(im), Memc[tempfile], SZ_FNAME)
	junk = fnroot (IM_HDRFILE(im), Memc[tempfile+nchars], SZ_FNAME)
	call mktemp (Memc[tempfile], Memc[outname], SZ_PATHNAME)
	call strcat (".fits", Memc[outname], SZ_PATHNAME)

	inoff = note (in_fd)
	out_fd = open (Memc[outname], NEW_FILE, BINARY_FILE)

	call fxf_make_adj_copy (in_fd, out_fd, hdroff, poff, datasize)

	# Pad to 2880 bytes block
	size =  note (out_fd) - 1
	size = mod(size, FITS_BLOCK_CHARS)
	if (size != 0) {
	    size = FITS_BLOCK_CHARS - size
	    call fxf_write_blanks (out_fd, size)
	}

	size = note (out_fd) - 1
	call close (in_fd)
	call delete (IM_HDRFILE(im))
	call rename (Memc[outname], IM_HDRFILE(im))

	in_fd = out_fd
	call seek (in_fd, inoff)
	call sfree (sp)
end


# FXF_PLPF -- Initialize IMIO dependencies when dealing with a PLIO
# image mask.

procedure fxf_plpf (im)

pointer im				#I IMIO descriptor

int	pfd
pointer	sp, imname, ref_im
int	sv_acmode, sv_update, ndim, i, depth
errchk	iki_opix, open
int	open()

begin
	call smark (sp)
	call salloc (imname, SZ_IMNAME, TY_CHAR)

	# Complete the initialization of a mask image.
	ref_im = IM_PLREFIM(im)

	sv_acmode = IM_ACMODE(im)
	sv_update = IM_UPDATE(im)
	call strcpy (IM_NAME(im), Memc[imname], SZ_IMNAME)

	if (ref_im != NULL) {
	    # Create a mask the same size as the physical size of the
	    # reference image.  Inherit any image section from the
	    # reference image.

	    IM_NDIM(im) = IM_NDIM(ref_im)
	    IM_NPHYSDIM(im) = IM_NPHYSDIM(ref_im)
	    IM_SECTUSED(im) = IM_SECTUSED(ref_im)
	    call amovl (IM_LEN(ref_im,1), IM_LEN(im,1), IM_MAXDIM)
	    call amovl (IM_PHYSLEN(ref_im,1),IM_PHYSLEN(im,1),IM_MAXDIM)
	    call amovl (IM_SVLEN(ref_im,1), IM_SVLEN(im,1), IM_MAXDIM)
	    call amovl (IM_VMAP(ref_im,1), IM_VMAP(im,1), IM_MAXDIM)
	    call amovl (IM_VOFF(ref_im,1), IM_VOFF(im,1), IM_MAXDIM)
	    call amovl (IM_VSTEP(ref_im,1), IM_VSTEP(im,1), IM_MAXDIM)

	    # Tell PMIO to use this image as the reference image.
	    call pm_seti (IM_PL(im), P_REFIM, im)

	} else if (sv_acmode == NEW_IMAGE || sv_acmode == NEW_COPY) {
	    # If ndim was not explicitly set, compute it by counting
	    # the number of nonzero dimensions.

	    ndim = IM_NDIM(im)
	    if (ndim == 0) {
		ndim = 1
		while (IM_LEN(im,ndim) > 0 && ndim <= IM_MAXDIM)
		    ndim = ndim + 1
		ndim = ndim - 1
		IM_NDIM(im) = ndim
	    }

	    # Make sure dimension stuff makes sense.
	    if (ndim < 0 || ndim > IM_MAXDIM)
		call imerr (IM_NAME(im), SYS_IMNDIM)

	    do i = 1, ndim
		if (IM_LEN(im,i) <= 0)
		    call imerr (IM_NAME(im), SYS_IMDIMLEN)

	    # Set the unused higher dimensions to 1.  This makes it
	    # possible to access the image as if it were higher
	    # dimensional, and in a way it truely is.

	    do i = ndim + 1, IM_MAXDIM
		IM_LEN(im,i) = 1

	    IM_NPHYSDIM(im) = ndim
	    call amovl (IM_LEN(im,1), IM_PHYSLEN(im,1), IM_MAXDIM)
	    call amovl (IM_LEN(im,1), IM_SVLEN(im,1), IM_MAXDIM)
	    if (sv_acmode == NEW_IMAGE)
		call amovkl (long(1), IM_VSTEP(im,1), IM_MAXDIM)	

	    depth = PL_MAXDEPTH
	    if (and (IM_PLFLAGS(im), PL_BOOL) != 0)
                 depth = 1
	    call pl_ssize (IM_PL(im), IM_NDIM(im), IM_LEN(im,1), depth)

	}

	call strcpy (Memc[imname], IM_NAME(im), SZ_IMNAME)
	IM_ACMODE(im) = sv_acmode
	IM_UPDATE(im) = sv_update
	IM_PIXOFF(im) = 1	
	IM_HGMOFF(im) = NULL
	IM_BLIST(im) = NULL
	IM_HFD(im) = NULL

	pfd = open ("dev$null", READ_WRITE, BINARY_FILE)
	IM_PFD(im) = pfd

	# Execute this even if pixel file has already been opened.
	call imsetbuf (IM_PFD(im), im)

	# "Fast i/o" in the conventional sense no IMIO buffering)
	# is not permitted for mask images, since IMIO must buffer
	# the pixels, which are generated at run time.

	if (IM_FAST(im) == YES) {
	    IM_PLFLAGS(im) = or (IM_PLFLAGS(im), PL_FAST)
	    IM_FAST(im) = NO
	}

	call sfree (sp)
end
