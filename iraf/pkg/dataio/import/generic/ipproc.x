include <mach.h>
include <imhdr.h>
include <evvexpr.h>
include "../import.h"

define	DEBUG	false


# IP_PRBAND -- Process a band interleaved file.

procedure ip_prband (ip, fd, im, cmap)

pointer	ip					#i task struct pointer
int	fd					#i inpout file descriptor
pointer	im					#i output image pointer
pointer	cmap					#i colormap pointer

long	l_val
size_t	nlines, npix
int	ii, j, optype, nbytes_pix, percent
long	i, cur_offset, band_offset, line_offset

int	ip_ptype()
long	ip_lnote()

begin
	# Rewind the file and skip header pixels.
	l_val = BOF
	call ip_lseek (fd, l_val)
	call ip_lseek (fd, IP_HSKIP(ip)+1)

	# Compute the offset between the same pixel in different bands.  This
	# is the area of the image plus any image padding, computed as a
	# byte offset.
	optype = ip_ptype (IO_TYPE(PTYPE(ip,1)),IO_NBYTES(PTYPE(ip,1)))
	switch (optype) {
	case TY_UBYTE:			nbytes_pix = 1
	case TY_USHORT, TY_SHORT:	nbytes_pix = SZB_CHAR * SZ_SHORT
	case TY_INT:			nbytes_pix = SZB_CHAR * SZ_INT
	case TY_LONG:			nbytes_pix = SZB_CHAR * SZ_LONG
	case TY_REAL:			nbytes_pix = SZB_CHAR * SZ_REAL
	case TY_DOUBLE:			nbytes_pix = SZB_CHAR * SZ_DOUBLE
	}
	band_offset = (IP_AXLEN(ip,1) * (IP_AXLEN(ip,2)-1)) +
		      ((IP_LSKIP(ip) + IP_LPAD(ip)) * (IP_AXLEN(ip,2)-1)) +
		      IP_BSKIP(ip)
	band_offset = (band_offset * nbytes_pix) #+ 1

	if (DEBUG) {
	    call eprintf ("ip_prband: band_offset=%d curpos=%d\n")
	        call pargl(band_offset) ; call pargl(ip_lnote(fd))
	    call zzi_prstruct ("ip_prband", ip) 
	}

	# Patch up the pixtype param if needed.
	call ip_fix_pixtype (ip)

        # See if we need to create any outbands operands if the user didn't.
        if (IP_NBANDS(ip) == ERR)
            call ip_fix_outbands (ip)

	# Loop over the image lines.
	nlines = IP_AXLEN(ip,2)
	npix = IP_AXLEN(ip,1)
	percent = 0
	do i = 1, nlines {
	    # Skip pixels at front of line
	    line_offset = ip_lnote (fd)
	    if (IP_LSKIP(ip) != 0)
	        call ip_lskip (fd, IP_LSKIP(ip))

	    # Read pixels in the line and save as operand.
	    call ip_rdline (ip, fd, 1, npix, cmap)

	    # Skip pixels at end of line.
	    if (IP_LPAD(ip) != 0)
	        call ip_lskip (fd, IP_LPAD(ip))
	    cur_offset = ip_lnote (fd)

	    # Loop over each of the remaining pixtypes.
	    do j = 2, IP_NPIXT(ip) {
		# Seek to offset of next band (i.e. line_offset + band_offset).
	        call ip_lskip (fd, band_offset)
		if (IP_LSKIP(ip) != 0)
	            call ip_lskip (fd, IP_LSKIP(ip))
	        call ip_rdline (ip, fd, j, npix, cmap) # read pixels in the line
		if (IP_LPAD(ip) != 0)
	            call ip_lskip (fd, IP_LPAD(ip))
	    }

	    # Evaluate and write the outbands expressions.
	    call ip_probexpr (ip, im, npix, i)

            # Print percent done if being verbose
            #if (IP_VERBOSE(ip) == YES)
		call ip_pstat (ip, i, percent)

	    # Restore file pointer to cur_offset.
	    call ip_lseek (fd, cur_offset)
	}
	do ii = 1, IP_NBANDS(ip)
	    call mfree (BUFFER(ip,ii), IM_PIXTYPE(im))
end


# IP_PRLINE -- Process a line interleaved file.

procedure ip_prline (ip, fd, im, cmap)

pointer	ip					#i task struct pointer
int	fd					#i inpout file descriptor
pointer	im					#i output image pointer
pointer	cmap					#i colormap pointer

long	l_val
long	i
size_t	nlines, npix
int	ii, j, percent

begin
	# Rewind the file and skip header pixels.
	l_val = BOF
	call ip_lseek (fd, l_val)
	call ip_lseek (fd, IP_HSKIP(ip)+1)

	if (DEBUG) { 
	    call eprintf ("ip_prline:\n")
	    call zzi_prstruct ("ip_prline", ip) 
	}

	# Patch up the pixtype param if needed.
	call ip_fix_pixtype (ip)

        # See if we need to create any outbands operands if the user didn't.
        if (IP_NBANDS(ip) == ERR)
            call ip_fix_outbands (ip)

        # Loop over the image lines.
        nlines = IP_AXLEN(ip,2)
        npix = IP_AXLEN(ip,1)
	percent = 0
        do i = 1, nlines {

            do j = 1, IP_NPIXT(ip) {
                # Skip pixels at front of line
                call ip_lskip (fd, IP_LSKIP(ip))

                # Read pixels in the line and save as operand.
                call ip_rdline (ip, fd, j, npix, cmap)

                # Skip pixels at end of line.
                call ip_lskip (fd, IP_LPAD(ip))
	    }

	    # Evaluate and write the outbands expressions.
	    call ip_probexpr (ip, im, npix, i)

            # Print percent done if being verbose
            #if (IP_VERBOSE(ip) == YES)
		call ip_pstat (ip, i, percent)
	}
	do ii = 1, IP_NBANDS(ip)
	    call mfree (BUFFER(ip,ii), IM_PIXTYPE(im))
end


# IP_PRPIX -- Process a pixel interleaved file.

procedure ip_prpix (ip, fd, im, cmap)

pointer	ip					#i task struct pointer
int	fd					#i inpout file descriptor
pointer	im					#i output image pointer
pointer	cmap					#i colormap pointer

size_t	c_1
long	l_val
long	i
pointer	op, data
size_t	npix, totpix, nlines
int	ii, swap, optype, percent

int	and(), ip_ptype()

begin
	c_1 = 1
	# Rewind the file and skip header pixels.
	l_val = BOF
	call ip_lseek (fd, l_val)
	call ip_lseek (fd, IP_HSKIP(ip)+1)

	if (DEBUG) { call eprintf ("ip_prpix:  ") }

        # See if we need to create any outbands operands if the user didn't.
        if (IP_NBANDS(ip) == ERR)
            call ip_fix_outbands (ip)

	# Allocate the pixtype data pointers.
        npix = IP_AXLEN(ip,1)
        nlines = IP_NPIXT(ip)
	do i = 1, nlines {
	    op = PTYPE(ip,i)
	    optype = ip_ptype (IO_TYPE(op),IO_NBYTES(op))
	    IO_NPIX(op) = npix
	    if (IO_DATA(op) == NULL)
	        if (optype == TY_UBYTE)
		    call malloc (IO_DATA(op), npix, TY_SHORT)
		else
		    call malloc (IO_DATA(op), npix, optype)
	}

        # Loop over the image lines.
        nlines = IP_AXLEN(ip,2)
	totpix = npix * IP_NPIXT(ip)
	swap = IP_SWAP(ip)
	percent = 0
	if (DEBUG) { 
	    call zzi_prstruct ("ip_prpix", ip)
	    call eprintf ("nl=%d np=%d tp=%d:\n") 
		call pargz(nlines) ; call pargz(npix) ; call pargz(totpix)
	}
        do i = 1, nlines {

            # Skip pixels at front of line
            call ip_lskip (fd, IP_LSKIP(ip))

	    # Read pixels in the line.
	    switch (optype) {
	    case TY_UBYTE:
	        call ip_agetb (fd, data, totpix)
	        call ip_lskip (fd, totpix)
		# Apply a colormap to the bytes.  In general a pixel-interleaved
		# file is a 24-bit True Color image, but maybe this is a
		# 3-D color index file?
		if (cmap != NULL && IP_USE_CMAP(ip) == YES) 		
                    call ip_gray_cmap (Memc[data], totpix, cmap)

	    case TY_USHORT:
	        call ip_agetu (fd, data, totpix)
                if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I2) {
                    call bswap2 (Mems[data], c_1, Mems[data], c_1, 
				 totpix*(SZ_SHORT*SZB_CHAR))
	        }
	        call ip_lskip (fd, (totpix * (SZB_CHAR * SZ_SHORT)))


	    case TY_SHORT:
	        call ip_agets (fd, data, totpix)
                if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I2) {
                    call bswap2 (Mems[data], c_1, Mems[data], c_1, 
				 totpix*(SZ_SHORT*SZB_CHAR))
	        }
	        call ip_lskip (fd, (totpix * (SZB_CHAR * SZ_SHORT)))

	    case TY_INT:
	        call ip_ageti (fd, data, totpix)
                if (and(swap, S_ALL) == S_ALL || and(swap, S_I4) == S_I4) {
                    call bswap4 (Memi[data], c_1, Memi[data], c_1, 
				 totpix*(SZ_INT*SZB_CHAR))
	        }
	        call ip_lskip (fd, (totpix * (SZB_CHAR * SZ_INT)))

	    case TY_LONG:
	        call ip_agetl (fd, data, totpix)
		if ( SZ_LONG == 2 ) {
                    if (and(swap, S_ALL) == S_ALL || and(swap, S_I4) == S_I4) {
                        call bswap4 (Meml[data], c_1, Meml[data], c_1, 
				     totpix*(SZ_LONG*SZB_CHAR))
		    }
	        } else {
                    if (and(swap, S_ALL) == S_ALL || and(swap, S_I8) == S_I8) {
                        call bswap8 (Meml[data], c_1, Meml[data], c_1, 
				     totpix*(SZ_LONG*SZB_CHAR))
		    }
		}
	        call ip_lskip (fd, (totpix * (SZB_CHAR * SZ_LONG)))

	    case TY_REAL:
	        call ip_agetr (fd, data, totpix)
                if (and(swap, S_ALL) == S_ALL) {
                    call bswap4 (Memr[data], c_1, Memr[data], c_1, 
				 totpix*(SZ_REAL*SZB_CHAR))
	        }
	        call ip_lskip (fd, (totpix * (SZB_CHAR * SZ_REAL)))

	    case TY_DOUBLE:
	        call ip_agetd (fd, data, totpix)
                if (and(swap, S_ALL) == S_ALL) {
                    call bswap8 (Memd[data], c_1, Memd[data], c_1, 
				 totpix*(SZ_DOUBLE*SZB_CHAR))
	        }
	        call ip_lskip (fd, (totpix * (SZB_CHAR * SZ_DOUBLE)))

	    }

            # Skip pixels at end of line.
            call ip_lskip (fd, IP_LPAD(ip))

	    # Separate pixels into different vectors.
	    call ip_upkpix (ip, data, npix)

	    # Evaluate and write the outbands expressions.
	    call ip_probexpr (ip, im, npix, i)

            # Print percent done if being verbose
            #if (IP_VERBOSE(ip) == YES)
		call ip_pstat (ip, i, percent)
	}

	if (optype == TY_UBYTE)
	    call mfree (data, TY_SHORT)
	else
	    call mfree (data, optype)
	do ii = 1, IP_NBANDS(ip)
	    call mfree (BUFFER(ip,ii), IM_PIXTYPE(im))
end


# IP_PROBEXPR -- Process each of the outbands expressions and write the result
# to the output image.

procedure ip_probexpr (ip, im, npix, line)

pointer	ip					#i task struct pointer
pointer	im					#i output image pointer
size_t	npix					#i number of output pixels
long	line					#i line number

int	i
pointer	out

pointer	ip_evaluate()

begin
        # Loop over outbands expressions.
        do i = 1, IP_NBANDS(ip) {
            # Evaluate outbands expression.
            out = ip_evaluate (ip, O_EXPR(ip,i))

            # Write bands to output image
            if (IP_OUTPUT(ip) != IP_NONE)
                call ip_wrline (ip, im, out, npix, line, i)

	    call evvfree (out)
        }
end


# IP_RDLINE -- Read a line of pixels from the binary file.

procedure ip_rdline (ip, fd, pnum, npix, cmap)

pointer	ip					#i task struct pointer
int	fd					#i input file descriptor
int	pnum					#i pixtype number
size_t	npix					#i number of pixels to read
pointer	cmap					#i colormap pointer

size_t	c_1
pointer	op, data
int	swap, ptype

int	and(), ip_ptype()

begin
	c_1 = 1
	# Read pixels in the line and save as operand.
	op = PTYPE(ip,pnum)
	ptype = ip_ptype (IO_TYPE(op), IO_NBYTES(op))
	data = IO_DATA(op)
	swap = IP_SWAP(ip)
	IO_NPIX(op) = npix

	switch (ptype) {
	case TY_UBYTE:
	    call ip_agetb (fd, data, npix)
	    call ip_lskip (fd, npix)
	    # Apply a colormap to the bytes.  If the colormap is non-null we
	    # assume the bytes are color indices into a colormap.
	    if (cmap != NULL && IP_USE_CMAP(ip) == YES) 		
                call ip_gray_cmap (Memc[data], npix, cmap)

	case TY_USHORT:
	    call ip_agetu (fd, data, npix)
            if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I2) {
                call bswap2 (Mems[data], c_1, Mems[data], c_1, 
			     npix*(SZ_SHORT*SZB_CHAR))
	    }
	    call ip_lskip (fd, (npix * (SZB_CHAR * SZ_SHORT)))

	case TY_SHORT:
	    call ip_agets (fd, data, npix)
            if (and(swap, S_ALL) == S_ALL || and(swap, S_I2) == S_I2) {
                call bswap2 (Mems[data], c_1, Mems[data], c_1, 
			     npix*(SZ_SHORT*SZB_CHAR))
	    }
	    call ip_lskip (fd, npix * (SZB_CHAR * SZ_SHORT))

	case TY_INT:
	    call ip_ageti (fd, data, npix)
            if (and(swap, S_ALL) == S_ALL || and(swap, S_I4) == S_I4) {
                call bswap4 (Memi[data], c_1, Memi[data], c_1, 
			     npix*(SZ_INT*SZB_CHAR))
	    }
	    call ip_lskip (fd, npix * (SZB_CHAR * SZ_INT))

	case TY_LONG:
	    call ip_agetl (fd, data, npix)
	    if ( SZ_LONG == 2 ) {
                if (and(swap, S_ALL) == S_ALL || and(swap, S_I4) == S_I4) {
                    call bswap4 (Meml[data], c_1, Meml[data], c_1, 
		                 npix*(SZ_LONG*SZB_CHAR))
		}
	    } else {
                if (and(swap, S_ALL) == S_ALL || and(swap, S_I8) == S_I8) {
                    call bswap8 (Meml[data], c_1, Meml[data], c_1, 
		                 npix*(SZ_LONG*SZB_CHAR))
		}
	    }
	    call ip_lskip (fd, npix * (SZB_CHAR * SZ_LONG))

	case TY_REAL:
	    call ip_agetr (fd, data, npix)
            if (and(swap, S_ALL) == S_ALL) {
                call bswap4 (Memr[data], c_1, Memr[data], c_1, 
			     npix*(SZ_REAL*SZB_CHAR))
	    }
	    call ip_lskip (fd, npix * (SZB_CHAR * SZ_REAL))

	case TY_DOUBLE:
	    call ip_agetd (fd, data, npix)
            if (and(swap, S_ALL) == S_ALL) {
                call bswap8 (Memd[data], c_1, Memd[data], c_1, 
			     npix*(SZ_DOUBLE*SZB_CHAR))
	    }
	    call ip_lskip (fd, npix * (SZB_CHAR * SZ_DOUBLE))

	}
	IO_DATA(op) = data
end


# IP_WRLINE -- Write a line of pixels to the output image.  We handle image
# flipping here to avoid possibly doing it several times while the outbands
# expression is being evaluated.

procedure ip_wrline (ip, im, out, npix, line, band)

pointer	ip					#i task struct pointer
pointer	im					#i output image pointer
pointer	out					#i output operand pointer
size_t	npix					#i number of pixels to read
long	line					#i image line number
int	band					#i image band number

long	c_1
long	l_val0, l_val1
int	type
long	i, lnum, lband, lnpix
pointer	sp, dptr, data, optr
bool	lastline
long	nldone, blnum

int	and()
long	modl()
pointer	imps3s(), imps3i(), imps3l(), imps3r(), imps3d()
pointer	ip_chtype()

data	blnum 	   /0/
data	nldone 	   /1/
data	lastline   /false/

begin
	c_1 = 1
	lband = band
	lnpix = npix
	
	call smark (sp)

	# The first thing we do is change the datatype of the operand to
	# match the output pixel type.
	if (IP_OUTTYPE(ip) != NULL) {
	    if (IP_OUTTYPE(ip) == O_TYPE(out))
		optr = O_VALP(out)
	    else
		optr = ip_chtype (out, IP_OUTTYPE(ip))
	}
	type = IP_OUTTYPE(ip)

	# See if we're flipping image in Y, and adjust the line number.
	if (and(IP_FLIP(ip),FLIP_Y) == FLIP_Y) {
	    lnum = IP_AXLEN(ip,2) - line + 1
	    if (band == 1) {
		l_val0 = IP_SZBUF(ip)
	        blnum = IP_SZBUF(ip) - modl(line-1, l_val0)
	    }
	    lastline = (lnum == 1)
	} else {
	    lnum = line
	    if (band == 1)
	        blnum = blnum + 1
	    lastline = (lnum == IP_AXLEN(ip,2))
	}

	# See if we're flipping image in x, and reverse the pixels.
	if (and(IP_FLIP(ip),FLIP_X) == FLIP_X) {
	    call salloc (dptr, npix, type)    
	    do i = 1, npix {
        	switch (type) {
        	case TY_UBYTE, TY_USHORT, TY_SHORT:
		    Mems[dptr+i-1] = Mems[optr+(npix-i)]

        	case TY_INT:
		    Memi[dptr+i-1] = Memi[optr+(npix-i)]

        	case TY_LONG:
		    Meml[dptr+i-1] = Meml[optr+(npix-i)]

        	case TY_REAL:
		    Memr[dptr+i-1] = Memr[optr+(npix-i)]

        	case TY_DOUBLE:
		    Memd[dptr+i-1] = Memd[optr+(npix-i)]

        	}
	    }
	} else 
	    dptr = optr

	# Make sure the image pixtype is set.
	if (IM_PIXTYPE(im) == NULL)
	    IM_PIXTYPE(im) = type

	# Allocate the buffer pointer if needed.
	if (BUFFER(ip,band) == NULL)
	    call calloc (BUFFER(ip,band), npix*IP_SZBUF(ip), IP_OUTTYPE(ip))

	if (nldone < IP_SZBUF(ip) && !lastline) {
	    # Copy the image line to the buffer
	    data = BUFFER(ip,band)
            switch (type) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
                call amovs (Mems[dptr], Mems[data+((blnum-1)*npix)], npix)

            case TY_INT:
                call amovi (Memi[dptr], Memi[data+((blnum-1)*npix)], npix)

            case TY_LONG:
                call amovl (Meml[dptr], Meml[data+((blnum-1)*npix)], npix)

            case TY_REAL:
                call amovr (Memr[dptr], Memr[data+((blnum-1)*npix)], npix)

            case TY_DOUBLE:
                call amovd (Memd[dptr], Memd[data+((blnum-1)*npix)], npix)

            }
	    if (band == IP_NBANDS(ip))
	        nldone = nldone + 1

	} else {
	    # Write the buffer to the image as a section.
	    data = BUFFER(ip,band)
            switch (type) {
            case TY_UBYTE, TY_USHORT, TY_SHORT:
                call amovs (Mems[dptr], Mems[data+((blnum-1)*npix)], npix)
		if (and(IP_FLIP(ip),FLIP_Y) == FLIP_Y) {
		    l_val0 = max(1,(lnum-IP_SZBUF(ip)+1)+IP_SZBUF(ip)-1)
		    l_val1 = max(1,lnum+min(nldone,IP_SZBUF(ip))-1)
                    data = imps3s (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovs (Mems[BUFFER(ip,band)+(blnum-1)*npix], 
			        Mems[data], npix*(IP_SZBUF(ip)-blnum+1))
		} else {
		    l_val0 = min(IP_AXLEN(ip,2),(lnum-blnum+1))
		    l_val1 = min(IP_AXLEN(ip,2),lnum)
                    data = imps3s (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovs (Mems[BUFFER(ip,band)], Mems[data], npix*blnum)
		}

            case TY_INT:
                call amovi (Memi[dptr], Memi[data+((blnum-1)*npix)], npix)
		if (and(IP_FLIP(ip),FLIP_Y) == FLIP_Y) {
		    l_val0 = max(1,(lnum-IP_SZBUF(ip)+1)+IP_SZBUF(ip)-1)
		    l_val1 = max(1,lnum+min(nldone,IP_SZBUF(ip))-1)
                    data = imps3i (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovi (Memi[BUFFER(ip,band)+(blnum-1)*npix], 
			        Memi[data], npix*(IP_SZBUF(ip)-blnum+1))
		} else {
		    l_val0 = min(IP_AXLEN(ip,2),(lnum-blnum+1))
		    l_val1 = min(IP_AXLEN(ip,2),lnum)
                    data = imps3i (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovi (Memi[BUFFER(ip,band)], Memi[data], npix*blnum)
		}

            case TY_LONG:
                call amovl (Meml[dptr], Meml[data+((blnum-1)*npix)], npix)
		if (and(IP_FLIP(ip),FLIP_Y) == FLIP_Y) {
		    l_val0 = max(1,(lnum-IP_SZBUF(ip)+1)+IP_SZBUF(ip)-1)
		    l_val1 = max(1,lnum+min(nldone,IP_SZBUF(ip))-1)
                    data = imps3l (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovl (Meml[BUFFER(ip,band)+(blnum-1)*npix], 
			        Meml[data], npix*(IP_SZBUF(ip)-blnum+1))
		} else {
		    l_val0 = min(IP_AXLEN(ip,2),(lnum-blnum+1))
		    l_val1 = min(IP_AXLEN(ip,2),lnum)
                    data = imps3l (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovl (Meml[BUFFER(ip,band)], Meml[data], npix*blnum)
		}

            case TY_REAL:
                call amovr (Memr[dptr], Memr[data+((blnum-1)*npix)], npix)
		if (and(IP_FLIP(ip),FLIP_Y) == FLIP_Y) {
		    l_val0 = max(1,(lnum-IP_SZBUF(ip)+1)+IP_SZBUF(ip)-1)
		    l_val1 = max(1,lnum+min(nldone,IP_SZBUF(ip))-1)
                    data = imps3r (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovr (Memr[BUFFER(ip,band)+(blnum-1)*npix], 
			        Memr[data], npix*(IP_SZBUF(ip)-blnum+1))
		} else {
		    l_val0 = min(IP_AXLEN(ip,2),(lnum-blnum+1))
		    l_val1 = min(IP_AXLEN(ip,2),lnum)
                    data = imps3r (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovr (Memr[BUFFER(ip,band)], Memr[data], npix*blnum)
		}

            case TY_DOUBLE:
                call amovd (Memd[dptr], Memd[data+((blnum-1)*npix)], npix)
		if (and(IP_FLIP(ip),FLIP_Y) == FLIP_Y) {
		    l_val0 = max(1,(lnum-IP_SZBUF(ip)+1)+IP_SZBUF(ip)-1)
		    l_val1 = max(1,lnum+min(nldone,IP_SZBUF(ip))-1)
                    data = imps3d (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovd (Memd[BUFFER(ip,band)+(blnum-1)*npix], 
			        Memd[data], npix*(IP_SZBUF(ip)-blnum+1))
		} else {
		    l_val0 = min(IP_AXLEN(ip,2),(lnum-blnum+1))
		    l_val1 = min(IP_AXLEN(ip,2),lnum)
                    data = imps3d (im, c_1, lnpix, l_val0, l_val1, lband, lband)
                    call amovd (Memd[BUFFER(ip,band)], Memd[data], npix*blnum)
		}

            }
	    if (band == IP_NBANDS(ip)) {
	        nldone = 1
	        blnum = 0
	    }
	}

	if (IP_OUTTYPE(ip) != O_TYPE(out))
	    call mfree (optr, type)
	call sfree (sp)
end


# IP_UPKPIX -- Unpack a line of pixel-interleaved pixels to the separate
# pixtype operand arrays.

procedure ip_upkpix (ip, ptr, npix)

pointer	ip					#i task struct pointer
pointer	ptr					#i pointer to pixels
size_t	npix					#i number of pixels in line

pointer	op[IM_MAXDIM]
int	j, np, optype[IM_MAXDIM]
long	i

int	ip_ptype()

begin
	np = IP_NPIXT(ip)
	do j = 1, np {
	    op[j] = PTYPE(ip,j)
	    optype[j] = ip_ptype (IO_TYPE(op[j]),IO_NBYTES(op[j]))
	}

	do j = 1, np {

	    do i = 0, npix-1 {
		switch (optype[j]) {
        	case TY_UBYTE, TY_USHORT, TY_SHORT:
		    Mems[IO_DATA(op[j])+i] = Mems[ptr+(i*np+j)-1]

        	case TY_INT:
		    Memi[IO_DATA(op[j])+i] = Memi[ptr+(i*np+j)-1]

        	case TY_LONG:
		    Meml[IO_DATA(op[j])+i] = Meml[ptr+(i*np+j)-1]

        	case TY_REAL:
		    Memr[IO_DATA(op[j])+i] = Memr[ptr+(i*np+j)-1]

        	case TY_DOUBLE:
		    Memd[IO_DATA(op[j])+i] = Memd[ptr+(i*np+j)-1]

		}
	    }
	}
end


# IP_FIX_PIXTYPE -- Create the pixtype operands for 3-D band or line-
# interleaved files.  These weren't allocated at first since the pixtype
# parameter or database field was atomic.

procedure ip_fix_pixtype (ip)

pointer	ip					#i task struct pointer

pointer	op, op1
int	i, nnp

begin
	if (DEBUG) {
	    call eprintf ("fix_pixtype: npixt=%d ndim=%d inter=%d\n")
	        call pargi(IP_NPIXT(ip)) ; call pargi(IP_NDIM(ip))
	        call pargi(IP_INTERLEAVE(ip)) ; call flush (STDERR)
	}

	# See if there's anything to be fixed.
        if (IP_NDIM(ip) < 3 || IP_NDIM(ip) < IP_NPIXT(ip))
	    return
	if (BAND_INTERLEAVED(ip) && (IP_NPIXT(ip) == IP_NDIM(ip)))
	    return
	if (LINE_INTERLEAVED(ip) && (IP_NPIXT(ip) == IP_INTERLEAVE(ip)))
	    return

	if (LINE_INTERLEAVED(ip))
	    nnp = IP_INTERLEAVE(ip)
	else
	    #nnp = IP_NDIM(ip)
	    nnp = IP_AXLEN(ip,3)

	# Make the new pixtype operands.
	op1 = PTYPE(ip,1)
	do i = 2, nnp {
	    call ip_alloc_operand (PTYPE(ip,i))
	    op = PTYPE(ip,i)
	    IO_TYPE(op) = IO_TYPE(op1)
	    IO_NBYTES(op) = IO_NBYTES(op1)
            call sprintf (OP_TAG(op), SZ_TAG, "b%d")
                call pargi (i)
	}
	IP_NPIXT(ip) = nnp

	if (DEBUG) { call zzi_prstruct ("fix_pixtype", ip) }
end


# IP_FIX_OUTBANDS -- Create the outbands operands if none were specified in
# the parameter file.

procedure ip_fix_outbands (ip)

pointer ip                                      #i task struct pointer

size_t	sz_val
pointer	sp, buf, im
int	i, nbands

define	SZ_OBSTR	2500

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (buf, sz_val, TY_CHAR)

	if (DEBUG) {
	    call eprintf ("fix_outbands: npixt=%d ndim=%d inter=%d\n")
	        call pargi(IP_NPIXT(ip)) ; call pargi(IP_NDIM(ip))
	        call pargi(IP_INTERLEAVE(ip)) ; call flush (STDERR)
	}

	# Free up the existing outbands operands.
	nbands = IP_NBANDS(ip)
	do i = 1, nbands
	    call ip_free_outbands (OBANDS(ip,i))

	# Create an outbands parameter string according to the tags in the
	# pixtype structure.  This way we preserve any user-defined tags on
	# output.
	nbands = IP_NPIXT(ip)
	sz_val = SZ_FNAME
	call aclrc (Memc[buf], sz_val)
	do i = 1, nbands {
	    call ip_alloc_outbands (OBANDS(ip,i))
	    sz_val = SZ_FNAME
	    call aclrc (Memc[buf], sz_val)
	    call sprintf (Memc[buf], SZ_FNAME, "b%d")
		call pargi (i)
            call strcpy (Memc[buf], O_EXPR(ip,i), SZ_EXPR)

            # Load the operand struct.
            call strcpy (Memc[buf], OP_TAG(O_OP(ip,i)), SZ_EXPR)
	}
	IP_NBANDS(ip) = nbands

	# Fix the output image dimensions.
	im = IP_IM(ip)
        IM_LEN(im,3) = IP_AXLEN(ip,3)
        if (IP_NBANDS(ip) > 1)
            IM_NDIM(im) = 3
        else
            IM_NDIM(im) = IP_NDIM(ip)

	call sfree (sp)

	if (DEBUG) { call zzi_prstruct ("fix_outbands", ip) }
end


# IP_CHTYPE - Change the expression operand vector to the output datatype.
# We allocate and return a pointer to the correct type to the converted
# pixels, this pointer must be freed later on.

pointer procedure ip_chtype (op, type)

pointer	op				#i evvexpr operand pointer
int	type				#i new type of pointer

pointer	out
pointer	coerce()

begin
	# Allocate the pointer and coerce it so the routine works.
	if (type == TY_UBYTE || type == TY_CHAR)
            call calloc (out, O_LEN(op), TY_CHAR)
	else {
            call calloc (out, O_LEN(op), type)
            out = coerce (out, type, TY_CHAR)
	}

	# Change the pixel type.
        switch (O_TYPE(op)) {
        case TY_CHAR:
            call achtc (Memc[O_VALP(op)], Memc[out], O_LEN(op), type)
        case TY_SHORT:
            call achts (Mems[O_VALP(op)], Memc[out], O_LEN(op), type)
        case TY_INT:
            call achti (Memi[O_VALP(op)], Memc[out], O_LEN(op), type)
        case TY_LONG:
            call achtl (Meml[O_VALP(op)], Memc[out], O_LEN(op), type)
        case TY_REAL:
            call achtr (Memr[O_VALP(op)], Memc[out], O_LEN(op), type)
        case TY_DOUBLE:
            call achtd (Memd[O_VALP(op)], Memc[out], O_LEN(op), type)
	default:
	    call error (0, "Invalid output type requested.")
        }

        out = coerce (out, TY_CHAR, type)
	return (out)
end


# IP_PTYPE -- For a given pixtype parameter return the corresponding IRAF
# data type.

define	NTYPES		7
define	NBITPIX		4

int procedure ip_ptype (type, nbytes)

int	type					#i pixel type
int	nbytes					#i number of bytes

int	i, pt, pb, ptype, off
int	tindex[NTYPES], bindex[NBITPIX], ttbl[2*NTYPES*NBITPIX]

data	tindex	/PT_BYTE, PT_UINT, PT_INT, PT_LONG, PT_IEEE, PT_NATIVE, PT_SKIP/
data	bindex	/1, 2, 4, 8/
# for 32-bit long
data	(ttbl(i), i= 1, 4)    /TY_UBYTE,  TY_USHORT,  TY_INT,      0/      # B
data	(ttbl(i), i= 5, 8)    /TY_UBYTE,  TY_USHORT,    0,         0/      # U
data	(ttbl(i), i= 9,12)    /TY_UBYTE,  TY_SHORT,   TY_INT,      0/      # I
data	(ttbl(i), i=13,16)    /TY_UBYTE,  TY_SHORT,   TY_LONG,     0/      # L
data	(ttbl(i), i=17,20)    /   0,         0,       TY_REAL, TY_DOUBLE/  # R
data	(ttbl(i), i=21,24)    /   0,         0,       TY_REAL, TY_DOUBLE/  # N
data	(ttbl(i), i=25,28)    /TY_UBYTE,  TY_USHORT,  TY_REAL, TY_DOUBLE/  # X
# for 64-bit long
data	(ttbl(i), i=29,32)    /TY_UBYTE,  TY_USHORT,  TY_INT,  TY_LONG/    # B
data	(ttbl(i), i=33,36)    /TY_UBYTE,  TY_USHORT,    0,         0/      # U
data	(ttbl(i), i=37,40)    /TY_UBYTE,  TY_SHORT,   TY_INT,  TY_LONG/    # I
data	(ttbl(i), i=41,44)    /TY_UBYTE,  TY_SHORT,   TY_INT,  TY_LONG/    # L
data	(ttbl(i), i=45,48)    /   0,         0,       TY_REAL, TY_DOUBLE/  # R
data	(ttbl(i), i=49,52)    /   0,         0,       TY_REAL, TY_DOUBLE/  # N
data	(ttbl(i), i=53,56)    /TY_UBYTE,  TY_USHORT,  TY_REAL, TY_DOUBLE/  # X

begin
	if (type == 0 || nbytes == 0) 		# uninitialized values
	    return (0)

	pt = NTYPES
	do i = 1, NTYPES {
	    if (tindex[i] == type)
		pt = i
	}
	pb = NBITPIX
	do i = 1, NBITPIX {
	    if (bindex[i] == nbytes)
		pb = i
	}

	if ( SZ_LONG == 2 ) {
	    off = 0	# for 32-bit long
        } else {
	    off = 28	# for 64-bit long
	}

	ptype = ttbl[off + (pt-1)*NBITPIX + pb]
	if (ptype == 0)
	    call error (0, "Invalid pixtype specified.")
	else
	    return (ptype)
end


# IP_PSTAT - Print information about the progress we're making.

procedure ip_pstat (ip, row, percent)

pointer ip                              #i task struct pointer
long	row                             #u current row
int	percent                         #u percent completed

begin
        # Print percent done if being verbose
        if (row * 100 / IP_AXLEN(ip,2) >= percent + 10) {
            percent = percent + 10
            call eprintf ("    Status: %2d%% complete\r")
                call pargi (percent)
            call flush (STDERR)
        }
end
