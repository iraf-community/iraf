include	<mach.h>
include	<imhdr.h>
include	<error.h>
include	"cyber.h"

# READ_IPPS_ROWS -- reads the ipps raster image row by row from the tape and 
# writes the output image.  At the completion of READ_IMAGE, the tape is 
# positioned to the header record of the next ipps raster.

procedure read_ipps_rows (rd, out_fname, data_type, dt)

int	rd, data_type
char	out_fname[SZ_FNAME]
pointer	dt

pointer	sp, im, spp_buf
int	npru_skip, i, stat
long	clktime()

int	read_dumpf()
pointer	immap(), impl2r()
errchk	cy_skip_pru, immap, ipps_to_iraf, read_dumpf()

begin
	# Allocate buffer for ipps raster pixels
	call smark (sp)
	call salloc (spp_buf, WRDS_PER_ROW(dt) * NINT_CYBER_WRD, TY_INT)

	# Map new iraf image and set up image header
	im = immap (out_fname, NEW_IMAGE, 0)
	IM_LEN(im, 1) = NCOLS(dt)
	IM_LEN(im, 2) = NROWS(dt)
	call strcpy (IPPS_ID(dt), IM_TITLE(im), SZ_IMTITLE)
	IM_MIN(im) = DATA_MIN(dt)
	IM_MAX(im) = DATA_MAX(dt)

	# Set optimum image pixel type
	if (data_type == NOT_SET) {
	    switch (BITS_PIXEL(dt)) {
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
	for (i=1; i <= NROWS(dt); i=i+1) {
	    iferr {
		stat = read_dumpf (rd, Memi[spp_buf], WRDS_PER_ROW(dt))
	    } then {
		call imunmap (im)
		call sfree (sp)
		call erract (EA_WARN)
		return
	    }
	    if (stat == EOF) {
		call imunmap (im)
		call sfree (sp)
		call eprintf ("Premature EOF in image at row # %d\n")
		    call pargi (i)
		return
	    }

	    call ipps_to_iraf (Memi[spp_buf], Memr[impl2r(im,i)], NCOLS(dt),
		BITS_PIXEL(dt))
        }
	
	# Skip from present position to end of rcopy raster
	npru_skip = PRU_EOR(dt) - PRU_ROW_ONE(dt) - (NPRU_ROW(dt)*NROWS(dt))
	call cy_skip_pru (rd, npru_skip)

	call imunmap (im)
	call sfree (sp)
end


# IPPS_TO_IRAF -- performs the conversion from Cyber pixels to IRAF pixels.  
# Each row of the ipps image is required to occupy an integral number of Cyber
# PRU's, so the input buffer contains pixels plus filler.  The number of 
# pixels unpacked from this buffer will always fill an integral number of
# Cyber words; a maximum of 4 extraneous pixels will be unpacked.

procedure ipps_to_iraf (in_buf, iraf_real, npix, nbits_pixel)

int	in_buf[ARB]
real	iraf_real[npix]
pointer	iraf_int, sp
int	nbits_pixel, npix, offset, npix_unpk, npix_cyber_wrd
errchk	unpk_12, unpk_20

begin
	# Allocate (maximum) space needed on the stack.
	call smark (sp)
	call salloc (iraf_int, npix + 4, TY_INT)
	offset = 1

	switch (nbits_pixel) {
	case 12: 
	    npix_cyber_wrd = 5
	    npix_unpk = ((npix + 4) / npix_cyber_wrd) * npix_cyber_wrd
	    call unpk_12 (in_buf, offset, Memi[iraf_int], npix_unpk)
	    call achtir (Memi[iraf_int], iraf_real, npix)

	case 20: 
	    npix_cyber_wrd = 3
	    npix_unpk = ((npix + 2) / npix_cyber_wrd) * npix_cyber_wrd
	    call unpk_20 (in_buf, offset, Memi[iraf_int], npix_unpk)
	    call achtir (Memi[iraf_int], iraf_real, npix)
	
	case 30: 
	    call unpk_30 (in_buf, offset, iraf_real, npix)
	
	case 60: 
	    call unpk_60r (in_buf, offset, iraf_real, npix)
	
	default:
	    call error (5, "Illegal IPPS #B/P")
	}

	call sfree (sp)
end


# CY_SKIP_IMAGE -- skips over an IPPS raster once the header has been
# read.  When SKIP_IMAGE returns, the tape is positioned to the first
# record of the next tape image.

procedure cy_skip_image (rd, dt)

int	rd
pointer	dt
int	npru_skip
errchk	cy_skip_pru

begin
        # Calculate number of PRU's in image
	npru_skip = PRU_EOR(dt) - PRU_ROW_ONE(dt)
        call cy_skip_pru (rd, npru_skip)
end

# CY_SKIP_PRU -- positions the tape by skipping the requested number of PRU's.
 
procedure cy_skip_pru (rd, npru_skip)

int	rd, npru_skip
pointer	sp, dummy
int	read_dumpf()

begin
        call smark (sp)
        call salloc (dummy, NINT_CYBER_WRD * LEN_PRU * npru_skip, TY_INT)

        if (read_dumpf (rd, Memi[dummy], npru_skip * LEN_PRU) == EOF) 
	    call eprintf ("Unexpected EOF when skipping image\n")
    
        call sfree (sp)
end
