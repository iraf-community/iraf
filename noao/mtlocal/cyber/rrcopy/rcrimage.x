include	<mach.h>
include	<imhdr.h>
include	<error.h>
include	"rrcopy.h"

# RC_READ_IMAGE -- reads the rcopy image row by row from the tape and writes
# the output image.  At the completion of READ_IMAGE, the tape is positioned
# to the header record of the next tape raster.

procedure rc_read_image (rd, out_fname, data_type, rp)

int	rd, data_type
char	out_fname[SZ_FNAME]
pointer	rp

pointer	sp, im, cyber_buf, spp_buf
int	nchars_per_row, nbits_skip, nchars_to_skip, i
long	clktime()

int	rc_read_cyber()
pointer	immap(), impl2r()
errchk	rc_skip_chars, immap, rc_ipps_to_iraf

begin
	# Allocate buffer for rcopy image pixels
	call smark (sp)
	nchars_per_row = WRDS_PER_ROW(rp) * NBITS_CYBER_WORD / NBITS_CHAR
	call salloc (cyber_buf, nchars_per_row, TY_CHAR)
	call salloc (spp_buf, nchars_per_row, TY_CHAR)

	# Map new iraf image and set up image header
	im = immap (out_fname, NEW_IMAGE, 0)
	IM_LEN(im, 1) = NCOLS(rp)
	IM_LEN(im, 2) = NROWS(rp)
	call strcpy (IPPS_ID(rp), IM_TITLE(im), SZ_IMTITLE)
	IM_MIN(im) = DATA_MIN(rp)
	IM_MAX(im) = DATA_MAX(rp)

	# Set optimum image pixel type
	if (data_type == NOT_SET) {
	    switch (BITS_PIXEL(rp)) {
		case 12:
		    IM_PIXTYPE(im) = TY_SHORT
		case 20:
		    IM_PIXTYPE(im) = TY_REAL
		case 30:
		    IM_PIXTYPE(im) = TY_REAL
		case 60:
		    IM_PIXTYPE(im) = TY_REAL
		default:
		    call error (3, "IPPS BITS_PIXEL is incorrect")
	    }
	} else
	    IM_PIXTYPE(im) = data_type
	IM_LIMTIME(im) = clktime (long(0))

	# Loop over rows to read, reorder and convert pixels. 
	for (i=1; i <= NROWS(rp); i=i+1) {
	    if (rc_read_cyber (rd, Memc[cyber_buf], nchars_per_row) == EOF)
		call error (4, "Unexpected EOT when reading image")
	    call rc_order_cyber_bits (Memc[cyber_buf], 1, Memc[spp_buf], 
			   WRDS_PER_ROW(rp))
	    call rc_ipps_to_iraf (Memc[spp_buf], Memr[impl2r(im,i)], NCOLS(rp), 
				BITS_PIXEL(rp))
        }
	
	# Skip from present position to end of rcopy raster
	nbits_skip = ((PRU_EOR(rp) - PRU_ROW_ONE(rp)) * LEN_PRU -
	   (WRDS_PER_ROW(rp) * NROWS(rp))) * NBITS_CYBER_WORD + NBITS_EOR_MARK

	nchars_to_skip = nbits_skip / NBITS_CHAR
	call rc_skip_chars (rd, nchars_to_skip)

	call imunmap (im)
	call sfree (sp)
end


# RC_IPPS_TO_IRAF -- performs the conversion from Cyber pixels to IRAF pixels.  
# Each row of the rcopy image is required to occupy an integral of Cyber
# PRU's, so the input buffer contains pixels plus filler.  The entire
# buffer is converted and npix pixels are written to the output image.

procedure rc_ipps_to_iraf (in_buf, iraf_real, npix, nbits_pixel)

char	in_buf[ARB]
real	iraf_real[npix]
pointer	iraf_int, sp
int	nbits_pixel, npix, bit_offset, npix_unpk, npix_cyber_wrd
errchk	rc_up_12, rc_up_20

begin
	# Calculate and allocate (maximum) space needed on the stack. The
	# number of pixels unpacked will always fill an integral number
	# of Cyber words.  A maximum of 4 extraneous pixels will be unpacked.
	call smark (sp)
	call salloc (iraf_int, npix + 4, TY_INT)
	bit_offset = 1

	switch (nbits_pixel) {
	case 12: 
	    npix_cyber_wrd = 5
	    npix_unpk = ((npix + 4) / npix_cyber_wrd) * npix_cyber_wrd
	    call rc_up_12 (in_buf, bit_offset, Memi[iraf_int], npix_unpk)
	    call achtir (Memi[iraf_int], iraf_real, npix)

	case 20: 
	    npix_cyber_wrd = 3
	    npix_unpk = ((npix + 2) / npix_cyber_wrd) * npix_cyber_wrd
	    call rc_up_20 (in_buf, bit_offset, Memi[iraf_int], npix_unpk)
	    call achtir (Memi[iraf_int], iraf_real, npix)
	
	case 30: 
	    call rc_up_30 (in_buf, bit_offset, iraf_real, npix)
	
	case 60: 
	    call rc_up_60r (in_buf, bit_offset, iraf_real, npix)
	
	default:
	    call error (5, "Illegal IPPS #B/P")
	}

	call sfree (sp)
end


# RC_SKIP_IMAGE -- skips over an RCOPY raster once the header has been
# read.  When SKIP_IMAGE returns, the tape is positioned to the first
# record of the next tape image.

procedure rc_skip_image (rd, rp)

int	rd
pointer	rp
int	nchars_to_skip
errchk	rc_skip_chars

begin
        # Calculate number of chars in image
        nchars_to_skip = ((PRU_EOR(rp) - PRU_ROW_ONE(rp)) * 
	  LEN_PRU * NBITS_CYBER_WORD + NBITS_EOR_MARK ) / NBITS_CHAR
        call rc_skip_chars (rd, nchars_to_skip)
end

# RC_SKIP_CHARS -- positions the tape by skipping the requested number of chars.
 
procedure rc_skip_chars (rd, nchars_to_skip)

int	rd, nchars_to_skip
pointer	sp, dummy
int	nblks_read, nchars_remaining, i
int	rc_read_cyber()

begin
        call smark (sp)
        call salloc (dummy, SZ_TAPE_BLK, TY_CHAR)

        # Calculate the number of full blocks to skip
        nblks_read = nchars_to_skip / SZ_TAPE_BLK
        nchars_remaining = mod (nchars_to_skip, SZ_TAPE_BLK)

	# Read from tape, a block at a time
        for (i=1; i <= nblks_read; i=i+1) {
            if (rc_read_cyber (rd, Memc[dummy], SZ_TAPE_BLK) == EOF)
		call error (6, "Unexpected EOT when skipping image")
	}
    
        # Read partial block from tape
        if (rc_read_cyber (rd, Memc[dummy], nchars_remaining) == EOF)
	    call error (7, "Unexpected EOT when skipping image")

        call sfree (sp)
end
