include <imio.h>
include <imhdr.h>
include <mii.h>
include <fset.h>
include <mach.h>
include <syserr.h>
include "fxf.h"

define MIN_BUFSIZE     2880


# FXF_EXPANDH -- Routine to expand all the headers of a MEF file. The calling
# routine only requires that extension 'group' be expanded but when dealing
# with large MEF files with many extensions this procedure can take a long
# time if the application code wants to expand more than one header.
# fxf_expandh will expand all the headers in the file so they will have at 
# least 'nlines' blank cards.

procedure fxf_expandh (in_fd, out_fd, nlines, group, nbks, hdroff, pixoff)

int	in_fd		#I input file descriptor
int	out_fd		#I output file descriptor
int	nlines		#I minimum number of blank cards
int	group		#I group that initiated the expansion
int	nbks		#I numbers of blocks to expand group 'group'
long	hdroff		#O new offset for beginning of 'group' 
long	pixoff		#0 new offset for beginning of data

size_t	sz_val
pointer hd, ip, op, buf
char	line[80], endl[80]
int	gn, newc, k, hsize
size_t	bufsize, nchars, rem
long	psize, lval, nbk, j
long	hoffset, poffset

int	fxf_xaddl()
int	modi()
long	read(), note(), modl(), fstatl()
errchk	malloc, read, write

begin
	# In case nlines is zero set a minimum > 0.
	nlines = max (nlines, 10)

	# Initialize a blank line.
	sz_val = LEN_CARD
	call amovks (" ", line, sz_val)

	# Initialize END card image.
	sz_val = 3
	call amovc ("END", endl, sz_val)
	sz_val = LEN_CARD-3
	call amovks (" ", endl[4], sz_val)

	call fseti (in_fd, F_ADVICE, SEQUENTIAL)
	call fseti (out_fd, F_ADVICE, SEQUENTIAL)

	bufsize = max (MIN_BUFSIZE, fstatl (in_fd, F_BUFSIZE))
	call malloc (buf, bufsize, TY_CHAR)

	gn = 0
	hd = buf

	repeat { 
	    hd = buf
	    if (group == gn) 
	        hdroff = note(out_fd)

	    # Read and write header information. The last block must 
	    # have the END card and is output from this routine.

	    iferr (call fxf_xhrd (in_fd, out_fd, Memc[buf], bufsize, hoffset, 
		poffset, hsize))
		    break
	    
	    # Determine the number of cards to expand. newc is in blocks
	    # of 36 cards. 0, 36, 72, ...

	    newc = fxf_xaddl (buf, hsize, nlines)

	    # expand the given group at least one block
	    if (newc == 0 && nbks > 0 && group == gn)
		newc = nbks * 36

	    # OP points to the top of the last block read, IP to the bottom.
	    op = buf + hsize - FITS_BLOCK_BYTES
	    ip = buf + hsize

	    if (newc == 0) {
		# Leave space for the END card.
	        ip = ip - 80
	    } else {
	        # Write current buffer before writing blanks.
		sz_val = FITS_BLOCK_BYTES
		call miipak (Memc[op], Memc[op], sz_val, TY_CHAR, MII_BYTE)
		sz_val = FITS_BLOCK_CHARS
		call write (out_fd, Memc[op], sz_val)

		# Use the same buffer space since we are using blanks
		ip = ip - FITS_BLOCK_BYTES
		op = ip
	    }

	    # Write the blank cards.
	    do k = 1, newc-1 {
	        sz_val = LEN_CARD
	        call amovc (line, Memc[ip], sz_val)
	        ip = ip + LEN_CARD
		if (modi (k,36) == 0) {
		    # We have more than one block of blanks.
		    call miipak (Memc[op], Memc[op], nchars, TY_CHAR, MII_BYTE)
		    sz_val = FITS_BLOCK_CHARS
		    call write (out_fd, Memc[op], sz_val)

		    # Notice we used the same buffer space
		    ip = ip - FITS_BLOCK_BYTES
		    op = ip
	        }
	    }
	    
	    # Finally the END card.
	    sz_val = LEN_CARD
	    call amovc (endl, Memc[ip], sz_val)
	    nchars = 2880
	    call miipak (Memc[op], Memc[op], nchars, TY_CHAR, MII_BYTE)
	    call write (out_fd, Memc[op], nchars/2)

	    # Get the number of blocks of pixel data to copy.  We are not
	    # changing anything; it is straight copy.

	    psize = (hoffset - poffset)

	    nbk = psize / bufsize
	    lval = bufsize
	    rem = modl (psize,lval)

            if (group == gn)
		pixoff = note(out_fd)

	    do j = 1, nbk {
		nchars = read (in_fd, Memc[buf], bufsize)
		call write (out_fd, Memc[buf], bufsize)
	    }
	    if (rem > 0) {
		nchars = read (in_fd, Memc[buf], rem)
		call write (out_fd, Memc[buf], rem)
	    }
	    gn = gn + 1
	}
 
	call mfree (buf, TY_CHAR)
end


# FXF_XHRD -- Procedure to read 2880 bytes blocks of header from 'in'
# and copy them to 'out'.  The last block read contains the END card
# and is pass to the calling routine which will write it out to 'out.

procedure fxf_xhrd (in, out, buf, bufsize, hoffset, poffset, hsize)

int	in		#I Input file descriptor
int	out		#I output file descriptor
char	buf[ARB]	#I Working buffer
size_t	bufsize		#I Workign buffer size
long	hoffset		#O Header offset for next group
long	poffset		#O Data offset for current group
int	hsize		#O Number of cards read in header

size_t	sz_val
size_t	sz_block_chars
pointer	sp, hb
int	nblks, i, j, ip
long	totpix, nchars
int	strncmp()
long	read(), note()
bool	end_card, fxf_xn_decode_blk1()

include "fxfcache.com"
errchk  syserr, read, write

begin
	call smark (sp)
	sz_val = 1440
	call salloc (hb, sz_val, TY_CHAR)

	hoffset = note (in)

	# Read first block of header.
	sz_val = FITS_BLOCK_CHARS
	nchars = read (in, Memc[hb], sz_val)
	if (nchars == EOF) {
	    call sfree (sp)
	    call syserr (SYS_FXFRFEOF)
	}

	sz_val = FITS_BLOCK_BYTES
	call miiupk (Memc[hb], buf, sz_val, MII_BYTE, TY_CHAR)
	end_card = fxf_xn_decode_blk1 (buf, totpix)
	if (!end_card) {
	    sz_val = FITS_BLOCK_BYTES
	    call miipak (buf, Memc[hb], sz_val, TY_CHAR, MII_BYTE)
	    sz_val = FITS_BLOCK_CHARS
	    call write (out, Memc[hb], sz_val)
	}
	ip = FITS_BLOCK_BYTES + 1

	nblks = 1
	if (!end_card) {
	    # Continue reading header until the block with END
	    # which is the last before the data block.

	    sz_block_chars = FITS_BLOCK_CHARS
	    while (read (in, Memc[hb], sz_block_chars) != EOF) {
		sz_val = FITS_BLOCK_BYTES
	        call miiupk (Memc[hb], buf[ip], sz_val, MII_BYTE, TY_CHAR)

		# Look for the END card
		do i = 0, 35 {
		   j = ip + i*LEN_CARD
		   if (buf[j] == 'E') {
		       if (strncmp (buf[j], "END     ", 8) == 0)
			   end_card = true
		   }
		}
	        nblks = nblks + 1
		if (end_card)
		    break
		sz_val = FITS_BLOCK_BYTES
	        call miipak (buf[ip], Memc[hb], sz_val, TY_CHAR, MII_BYTE)
		call write (out, Memc[hb], sz_block_chars)
	        ip = ip + FITS_BLOCK_BYTES

		# If the header is really big we can run out of
		# buffer space. Revert back to the beginning.

		if (ip > bufsize) {
		    ip = 1
		    nblks = 1
		}
	    }
	}

	hsize = nblks * 36 * LEN_CARD

	# We are at the beginning of the pixel area.
	poffset = note (in)
	
	# Get the beginnning of the next header.
	hoffset = poffset + totpix

	call sfree (sp)
end


# FXF_XN_DECODE_BLK1 -- Function that return true if the 1st block of a header
# contains the END card. The size of the pixel are is also returned.

bool procedure fxf_xn_decode_blk1 (buf, datalen)

char	buf[ARB]		#I header data buffer
long	datalen			#O length of data area in chars

char	card[LEN_CARD]
long	totpix, nbytes
int	index, k, i, pcount, bitpix, naxis, ip
long	len_axis[7]
int	fxf_ctype(), absi()
bool	end_card
errchk	syserr, syserrs

begin
	# Read successive lines of the 1st header block
	pcount = 0

	end_card = false
	do k = 0, 35 {
	    ip = k*LEN_CARD + 1
	    
	    # Copy into a one line buffer, we need to EOS mark.
	    call strcpy (buf[ip], card, LEN_CARD)
	    switch (fxf_ctype (card, index)) {
	    case KW_END:
		end_card = true
		break
	    case KW_PCOUNT:
		call fxf_geti (card, pcount)
	    case KW_BITPIX:
		call fxf_geti (card, bitpix)
	    case KW_NAXIS:
		if (index == 0) {
		    call fxf_geti (card, naxis)
		    if (naxis < 0 )
			call syserr (SYS_FXFRFBNAXIS)
	  	} else
		    call fxf_getl (card, len_axis[index])
	    default:
	        ;
	    }
	}

	# Calculate the length of the data area of the current extension,
	# measured in SPP chars and rounded up to an integral number of FITS
	# logical blocks.

	if (naxis != 0) {
	    totpix =  len_axis[1]
	    do i = 2, naxis
		totpix = totpix * len_axis[i]

	    # Compute the size of the data area (pixel matrix plus PCOUNT)
	    # in bytes.  Be careful not to overflow a 32 bit integer.

	    nbytes = (totpix + pcount) * (absi(bitpix) / NBITS_BYTE)

	    # Round up to fill the final 2880 byte FITS logical block.
	    nbytes = ((nbytes + 2880-1) / 2880) * 2880

	    datalen = nbytes / SZB_CHAR

	} else
	    datalen = 0

	return (end_card)
end


# FXF_XADDL -- Algorithm to find the number of blank cards stored in the
# input buffer.  This is the number from the end of the buffer up to the
# last non blank card (excluding the END card).  The function returns the
# number of extra header cards (in multiple of 36) that is necessary to 
# add to the current header.

int procedure fxf_xaddl (hd, ncua, nlines)

pointer hd		#U header area pointer
int	ncua		#I number of characters in the user area
int	nlines		#I minimum number of header lines to be added 

size_t	sz_val
pointer	ip
int	nbc, k, ncards, nkeyw
int	strncmp(), modi()

begin
	# Go to the end of buffer and get last line pointer
	ip = hd + ncua - LEN_CARD
	
	# See if line is blank.
	nbc = 0
	while (ip > hd) {
	    # Check for nonblank card
	    do k = 0, LEN_CARD-1 
		if (Memc[ip+k] != ' ')
		    break

	    # Since we are counting from the bottom, the first keyword
	    # (except END) would end counting.

	    if (k != LEN_CARD && k != 0)   # nonblank keyw card reached
		break
	    else if (k == 0) {
		# Just bypass END and continue looking for blank cards
		if (strncmp ("END     ", Memc[ip], 8) == 0) {
		    # Clear this card as it will be written at the 
		    # end of the output header.
		    sz_val = LEN_CARD
		    call amovkc (" ", Memc[ip], sz_val)
	    	    ip = ip - LEN_CARD
		    next
		} else
		    break
	    } else
		nbc = nbc + 1
	    ip = ip - LEN_CARD
	}

	# Calculate the number of keywords right before the last blank
	# card and right after the last non-blank keyword, excluding the
	# END card

	nkeyw = (ip-hd)/80 + 1

	ncards = ncua / LEN_CARD

	# Calculate the complement with respect to 36
	ncards = ((ncards + 35)/36)*36 - ncards
	nbc = nbc + ncards


	if (nbc < nlines) {
	    # Lets add nlines-nbc cards to the header
	    ncards = nlines - nbc

	    # Adjust to a 36 cards boundary.
	    ncards = 36 - modi (ncards, 36) + ncards
	} else
	    ncards = 0

	return (ncards)
end
