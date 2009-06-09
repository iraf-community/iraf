include <imhdr.h>
include <math/gsurfit.h>
include "psfmatch.h"

# RG_PSFM -- Procedure to match the psf functions of two images.

int procedure rg_psfm (pm, imr, im1, impsf, imk, newref)

pointer	pm		#I pointer to psf matching structure
pointer	imr		#I pointer to reference image
pointer	im1		#I pointer to input image
pointer	impsf		#I pointer to the psf image
pointer	imk		#I pointer to kernel image
int	newref		#I new reference image ?

int	stat
int	rg_pstati(), rg_pfget(), rg_psfget(), rg_kget()
long	rg_pstatl()
pointer	rg_pstatp()
include	<nullptr.inc>

begin
	# Compute the convolution kernel.
	if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL) {

	    # Compute the kernel using raw image data or the psf image.
	    if (rg_pstati (pm,CONVOLUTION) ==  PM_CONIMAGE) {

		# Set the kernel size to the user specified kernel size.
		call rg_psetl (pm, KNX, rg_pstatl (pm, PNX))
		if (IM_NDIM(imr) == 1)
		    call rg_pseti (pm, KNY, 1)
		else
		    call rg_psetl (pm, KNY, rg_pstatl (pm, PNY))

		# Compute the FFTS of the input and reference image.
		stat = rg_pfget (pm, imr, im1, newref)

	    } else {

		# Set the kernel size to the psf image size
		call rg_psetl (pm, KNX, IM_LEN (impsf,1))
		if (IM_NDIM(imr) == 1)
		    call rg_pseti (pm, KNY, 1)
		else
		    call rg_psetl (pm, KNY, IM_LEN(impsf,2))

		# Compute the FFTS of the input and reference psf images.
		stat = rg_psfget (pm, imr, impsf, newref)
	    }

	    # Delete working arrays if an error occurs.
	    if (stat == ERR) {
	        if (rg_pstatp (pm, REFFFT) != NULL)
		    call mfree (rg_pstatp (pm, REFFFT), TY_REAL)
	        call rg_psetp (pm, REFFFT, NULLPTR)
	        if (rg_pstatp (pm, IMFFT) != NULL)
		    call mfree (rg_pstatp (pm, IMFFT), TY_REAL)
	        call rg_psetp (pm, IMFFT, NULLPTR)
	        if (rg_pstatp (pm, FFT) != NULL)
		    call mfree (rg_pstatp (pm, FFT), TY_REAL)
	        call rg_psetp (pm, FFT, NULLPTR)
	        if (rg_pstatp (pm, CONV) != NULL)
		    call mfree (rg_pstatp (pm, CONV), TY_REAL)
	        call rg_psetp (pm, CONV, NULLPTR)
	        if (rg_pstatp (pm, ASFFT) != NULL)
		    call mfree (rg_pstatp (pm, ASFFT), TY_REAL)
	        call rg_psetp (pm, ASFFT, NULLPTR)
	    }

	    # Do the filtering in frequency space.
	    if (rg_pstatp (pm, FFT) != NULL) 
	        call rg_pfilter (pm)

	} else {

	    # Set the kernel size.
	    call rg_psetl (pm, KNX, IM_LEN(imk,1))
	    if (IM_NDIM(im1) == 1)
		call rg_pseti (pm, KNY, 1)
	    else
		call rg_psetl (pm, KNY, IM_LEN(imk,2))

	    # Read in the convolution kernel.
	    stat = rg_kget (pm, imk)

	    # Delete working arrays if an error occurs.
	    if (stat == ERR) {
	        if (rg_pstatp (pm, REFFFT) != NULL)
		    call mfree (rg_pstatp (pm, REFFFT), TY_REAL)
	        call rg_psetp (pm, REFFFT, NULLPTR)
	        if (rg_pstatp (pm, IMFFT) != NULL)
		    call mfree (rg_pstatp (pm, IMFFT), TY_REAL)
	        call rg_psetp (pm, IMFFT, NULLPTR)
	        if (rg_pstatp (pm, FFT) != NULL)
		    call mfree (rg_pstatp (pm, FFT), TY_REAL)
	        call rg_psetp (pm, FFT, NULLPTR)
	        if (rg_pstatp (pm, CONV) != NULL)
		    call mfree (rg_pstatp (pm, CONV), TY_REAL)
	        call rg_psetp (pm, CONV, NULLPTR)
	        if (rg_pstatp (pm, ASFFT) != NULL)
		    call mfree (rg_pstatp (pm, ASFFT), TY_REAL)
	        call rg_psetp (pm, ASFFT, NULLPTR)
	    }
	}

	return (stat)
end


# RG_PFGET -- Compute the psfmatching function using Fourier techniques.

int procedure rg_pfget (pm, imr, im1, newref)

pointer	pm		#I pointer to psfmatch structure
pointer	imr		#I pointer to reference image
pointer	im1		#I pointer to input image
int	newref		#I new reference image ?

size_t	sz_val
long	c_0
int	i, nregions
size_t	nrcols, nrlines, nrpcols, nrplines, nborder
long	nrimcols, nrimlines, rc1, rc2, rl1, rl2
int	stat
long	nxfft, nyfft
pointer	sp, str, coeff, dim, rbuf, ibuf, rsum, isum, border
pointer	prc1, prc2, prl1, prl2, przero, prxslope, pryslope, reffft, imfft, fft
real	rwtsum, iwtsum, rscale, iscale, rnscale, inscale
bool	fp_equalr()
int	rg_pstati(), rg_szfft()
long	rg_border(), rg_pstatl()
pointer	rg_pstatp(), rg_pgdata()
real	rg_pstatr(), rg_pnsum(), rg_pg1norm(), rg_pg2norm()
real	rg_pg10f(), rg_pg20f()

define	nextimage_	11

begin
	c_0 = 0

	# Assemble the PSF data by looping over the regions list.
	nregions = rg_pstati (pm, NREGIONS)
	if (nregions <= 0)
	    return (ERR)

	call smark (sp)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)
	sz_val = max (GS_SAVECOEFF+6, 9)
	call salloc (coeff, sz_val, TY_REAL)
	sz_val = 2
	call salloc (dim, sz_val, TY_LONG)

	# Get the reference region pointers.
	prc1 = rg_pstatp (pm, RC1)
	prc2 = rg_pstatp (pm, RC2)
	prl1 = rg_pstatp (pm, RL1)
	prl2 = rg_pstatp (pm, RL2)
	przero = rg_pstatp (pm, RZERO)
	prxslope = rg_pstatp (pm, RXSLOPE)
	pryslope = rg_pstatp (pm, RYSLOPE)

	# Check to see if the reference / input images are 1D.
	nrimcols = IM_LEN(imr,1)
	nrpcols = rg_pstatl (pm, PNX)
	if (IM_NDIM(imr) == 1) {
	    nrimlines = 1
	    nrplines = 1
	} else {
	    nrimlines = IM_LEN(imr,2)
	    nrplines = rg_pstatl (pm, PNY)
	}

	# Initialize
	rwtsum = 0.0
	iwtsum = 0.0
	rnscale = INDEFR
	inscale = INDEFR
	rbuf = NULL
	ibuf = NULL
	stat = OK
	if (newref == YES) {
	    sz_val = rg_pstatl (pm, DNX) * rg_pstatl (pm, DNY)
	    call calloc  (rsum, sz_val, TY_REAL)
	}
	sz_val = rg_pstatl (pm, DNX) * rg_pstatl (pm, DNY)
	call calloc  (isum, sz_val, TY_REAL)

	do i = 1, nregions {

	    # Get the reference subraster regions.
	    rc1 = max (1, min (nrimcols, Meml[prc1+i-1]))
	    rc2 = min (nrimcols, max (1, Meml[prc2+i-1]))
	    rl1 = max (1, min (nrimlines, Meml[prl1+i-1]))
	    rl2 = min (nrimlines, max (1, Meml[prl2+i-1]))
	    nrcols = rc2 - rc1 + 1
	    nrlines = rl2 - rl1 + 1

	    # Go to next object if reference region is off the image.
	    if (nrcols < rg_pstatl (pm, DNX) || (IM_NDIM(imr) == 2 &&
	        nrlines < rg_pstatl (pm, DNY))) {
	        call rg_pstats (pm, REFIMAGE, Memc[str], SZ_LINE)
	        call eprintf (
		    "Reference object %d: %s[%d:%d,%d:%d] is off image.\n")
		    call pargi (i)
		    call pargstr (Memc[str])
		    call pargl (rc1)
		    call pargl (rc2)
		    call pargl (rl1)
		    call pargl (rl2)
		next
	    }

	    if (newref == YES) {

	        # Get the reference data.
	        rbuf = rg_pgdata (imr, rc1, rc2, rl1, rl2)

                # Do the reference image background subtraction.
                border = NULL
                nborder = rg_border (Memr[rbuf], nrcols, nrlines, nrpcols,
		    nrplines, border)
                call rg_pscale (pm, Memr[border], nborder, nrcols,
                    nrlines, nrpcols, nrplines, rg_pstatr (pm, BVALUER),
		    Memr[coeff])
                if (border != NULL)
                    call mfree (border, TY_REAL)

                # Save the coefficients.
                Memr[przero+i-1] = Memr[coeff]
                Memr[prxslope+i-1] = Memr[coeff+1]
                Memr[pryslope+i-1] = Memr[coeff+2]

		# Subtract the reference background.
                call rg_subtract (Memr[rbuf], nrcols, nrlines,
		    Memr[przero+i-1], Memr[prxslope+i-1], Memr[pryslope+i-1])

	        # Apodize the reference image data.
                if (rg_pstatr (pm, APODIZE) > 0.0)
                    call rg_apodize (Memr[rbuf], nrcols, nrlines,
		        rg_pstatr (pm, APODIZE), YES)

	        # Compute the scale factors and accumulate the weighted sums.
	        rscale = rg_pnsum (Memr[rbuf], nrcols, nrlines, nrpcols,
		    nrplines)
		if (! IS_INDEFR(rscale)) {
		    if (IS_INDEFR(rnscale))
		        rnscale = 1.0 / rscale
		}
		if (IS_INDEFR(rscale))
		    rscale = 1.0
		else
		    rscale = rscale / rnscale

		call amulkr (Memr[rbuf], rscale, Memr[rbuf], nrcols *
		    nrlines) 
		rwtsum = rwtsum + rscale
	        call aaddr (Memr[rsum], Memr[rbuf], Memr[rsum], nrcols *
		    nrlines)

	        call mfree (rbuf, TY_REAL)
	    }

	    # Get the input image data
	    ibuf = rg_pgdata (im1, rc1, rc2, rl1, rl2)

	    # Compute the zero point, and the x and y slopes of input image.
            border = NULL
            nborder = rg_border (Memr[ibuf], nrcols, nrlines, nrpcols,
	        nrplines, border)
            call rg_pscale (pm, Memr[border], nborder, nrcols, nrlines,
                nrpcols, nrplines, rg_pstatr (pm, BVALUE), Memr[coeff])
            if (border != NULL)
                call mfree (border, TY_REAL)

            # Subtract the background from the input image.
            call rg_subtract (Memr[ibuf], nrcols, nrlines, Memr[coeff],
                Memr[coeff+1], Memr[coeff+2])

	    # Apodize the data.
            if (rg_pstatr (pm, APODIZE) > 0.0)
                call rg_apodize (Memr[ibuf], nrcols, nrlines, rg_pstatr (pm,
                    APODIZE), YES)

	    # Compute the scale factors and accumulate the weighted sums for
	    # input image.
	    iscale = rg_pnsum (Memr[ibuf], nrcols, nrlines, nrpcols, nrplines)
	    if (! IS_INDEFR(iscale)) {
		if (IS_INDEFR(inscale))
		    inscale = 1.0 / iscale
	    }
	    if (IS_INDEFR(iscale))
	        iscale = 1.0
	    else
		iscale = iscale / inscale

	    call amulkr (Memr[ibuf], iscale, Memr[ibuf], nrcols * nrlines) 
	    iwtsum = iwtsum + iscale
	    call aaddr (Memr[isum], Memr[ibuf], Memr[isum], nrcols * nrlines)

	    # Free the individual image buffers.
	    call mfree (ibuf, TY_REAL)
	}

	# Check to see if any data was read.
	if (iwtsum <= 0.0) {
	    stat = ERR
	    goto nextimage_
	}

	# Normalize the summed buffers by the weights. 
	if (newref == YES) {
	    if (! fp_equalr (rwtsum, 0.0))
	        call adivkr (Memr[rsum], rwtsum, Memr[rsum], nrcols * nrlines)
	}
	if (! fp_equalr (iwtsum, 0.0))
	    call adivkr (Memr[isum], iwtsum, Memr[isum], nrcols * nrlines)

	# Figure out how big the Fourier transform has to be, given
	# the size of the reference subraster, the window size and
	# the fact that the FFT must be a power of 2.

	nxfft = rg_szfft (nrcols, c_0)
	if (nrlines == 1)
	    nyfft = 1
	else
	    nyfft = rg_szfft (nrlines, c_0)
	call rg_psetl (pm, NXFFT, nxfft)
	call rg_psetl (pm, NYFFT, nyfft)

	imfft = rg_pstatp (pm, IMFFT)
	if (imfft != NULL)
	    call mfree (imfft, TY_REAL)
	sz_val = 2 * nxfft * nyfft
	call calloc (imfft, sz_val, TY_REAL)
	call rg_psetp (pm, IMFFT, imfft)

	# Allocate space for the fft.
	fft = rg_pstatp (pm, FFT)
	if (fft != NULL)
	    call mfree (fft, TY_REAL)
	sz_val = 2 * nxfft * nyfft
	call calloc (fft, sz_val, TY_REAL)
	call rg_psetp (pm, FFT, fft)

	# Allocate space for the reference and input image ffts
	if (newref == YES) {

	    reffft = rg_pstatp (pm, REFFFT)
	    if (reffft != NULL)
	        call mfree (reffft, TY_REAL)
	    sz_val = 2 * nxfft * nyfft
	    call calloc (reffft, sz_val, TY_REAL)
	    call rg_psetp (pm, REFFFT, reffft)

	    # Load the reference image FFT.
	    call rg_rload (Memr[rsum], nrcols, nrlines, Memr[fft], nxfft,
	        nyfft)
	    call mfree (rsum, TY_REAL)
	    rsum = NULL

	    # Load the input image FFT.
	    call rg_iload (Memr[isum], nrcols, nrlines, Memr[fft], nxfft,
	        nyfft)
	    call mfree (isum, TY_REAL)
	    isum = NULL

	    # Shift the data for easy of filtering.
	    call rg_fshift (Memr[fft], Memr[fft], 2 * nxfft, nyfft)

	    # Compute the Fourier Transform of the reference and input image
	    # data.
	    Meml[dim] = nxfft
	    Meml[dim+1] = nyfft
	    if (Meml[dim+1] == 1)
	        call rg_fourn (Memr[fft], Meml[dim], 1, 1)
	    else
	        call rg_fourn (Memr[fft], Meml[dim], 2, 1)

	    # Compute the flux ratio between the two data sets.
	    if (IS_INDEFR(rg_pstatr(pm, UFLUXRATIO))) {
		if (rg_pstati (pm, BACKGRD) == PM_BNONE)
	            call rg_psetr (pm, FLUXRATIO, rg_pg2norm (Memr[fft],
	                2 * nxfft, nyfft))
		else
	            call rg_psetr (pm, FLUXRATIO, rg_pg20f (Memr[fft],
	                2 * nxfft, nyfft))
	    } else
		call rg_psetr (pm, FLUXRATIO, rg_pstatr (pm, UFLUXRATIO))

	    # Separate the two transforms and compute the division.
	    call rg_pdivfft (Memr[fft], Memr[reffft], Memr[imfft], Memr[fft],
	        2 * nxfft, nyfft)

	} else {


	    # Get the reference image FFT.
	    reffft = rg_pstatp (pm, REFFFT)

	    # Load the input image FFT.
	    call rg_rload (Memr[isum], nrcols, nrlines, Memr[imfft], nxfft,
	        nyfft)
	    call mfree (isum, TY_REAL)
	    isum = NULL

	    # Shift the data for easy of filtering.
	    call rg_fshift (Memr[imfft], Memr[imfft], 2 * nxfft, nyfft)

	    # Compute the Fourier Transform of the input image data.
	    Meml[dim] = nxfft
	    Meml[dim+1] = nyfft
	    if (Meml[dim+1] == 1)
	        call rg_fourn (Memr[imfft], Meml[dim], 1, 1)
	    else
	        call rg_fourn (Memr[imfft], Meml[dim], 2, 1)

	    # Compute the flux ratio between the two data sets.
	    if (IS_INDEFR(rg_pstatr(pm, UFLUXRATIO))) {
		if (rg_pstati (pm, BACKGRD) == PM_BNONE)
	            call rg_psetr (pm, FLUXRATIO, rg_pg1norm (Memr[reffft],
	                2 * nxfft, nyfft) / rg_pg1norm (Memr[imfft], 2 * nxfft,
		        nyfft))
		else
	            call rg_psetr (pm, FLUXRATIO, rg_pg10f (Memr[reffft],
	                2 * nxfft, nyfft) / rg_pg10f (Memr[imfft], 2 * nxfft,
		        nyfft))
	    } else
		call rg_psetr (pm, FLUXRATIO, rg_pstatr (pm, UFLUXRATIO))

	    # Divide the two functions.
	    # arg 1,2,3 : incompatible pointer
	    call adivx (Memr[reffft], Memr[imfft], Memr[fft], nxfft * nyfft)
	}

	# Normalize the FFT.
	call rg_pnorm (Memr[fft], nxfft, nyfft, rg_pstatr (pm, FLUXRATIO))


nextimage_

	if (rsum != NULL)
	    call mfree (rsum, TY_REAL)
	if (isum != NULL)
	    call mfree (isum, TY_REAL)
	call sfree (sp)
	if (stat == ERR)
	    return (ERR)
	else
	    return (OK)
end


# RG_PSFGET -- Compute the psfmatching function using Fourier techniques.

int procedure rg_psfget (pm, imr, impsf, newref)

pointer	pm		#I pointer to the psfmatch structure
pointer	imr		#I pointer to the reference psf
pointer	impsf		#I pointer to the input image psf
int	newref		#I new reference image

size_t	sz_val
long	c_1, c_0
long	nrcols, nrlines, nxfft, nyfft
pointer	sp, dim, rbuf, ibuf, imfft, fft, reffft
int	rg_szfft()
pointer	rg_pgdata(), rg_pstatp()
real	rg_pstatr(), rg_pg2norm(), rg_pg1norm()

begin
	c_0 = 0
	c_1 = 1

	call smark (sp)
	sz_val = 2
	call salloc (dim, sz_val, TY_LONG)

	nrcols = IM_LEN(imr,1)
	if (IM_NDIM(imr) == 1)
	    nrlines = 1
	else
	    nrlines = IM_LEN(imr,2)

	# Get the psf data.
	rbuf = NULL
	ibuf = NULL
	if (newref == YES) {
	    call calloc (rbuf, nrcols * nrlines, TY_REAL)
	    rbuf = rg_pgdata (imr, c_1, nrcols, c_1, nrlines)
	}
	call calloc (ibuf, nrcols * nrlines, TY_REAL)
	ibuf = rg_pgdata (impsf, c_1, nrcols, c_1, nrlines)

	# Compute the size for the FFT buffers.
 	nxfft = rg_szfft (nrcols, c_0)
        if (nrlines == 1)
            nyfft = 1
        else
            nyfft = rg_szfft (nrlines, c_0)
        call rg_psetl (pm, NXFFT, nxfft)
        call rg_psetl (pm, NYFFT, nyfft)

	imfft = rg_pstatp (pm, IMFFT)
        if (imfft != NULL)
            call mfree (imfft, TY_REAL)
        call calloc (imfft, 2 * nxfft * nyfft, TY_REAL)
        call rg_psetp (pm, IMFFT, imfft)

        # Allocate space for the fft.
        fft = rg_pstatp (pm, FFT)
        if (fft != NULL)
            call mfree (fft, TY_REAL)
        call calloc (fft, 2 * nxfft * nyfft, TY_REAL)
        call rg_psetp (pm, FFT, fft)

	if (newref == YES) {

 	    reffft = rg_pstatp (pm, REFFFT)
            if (reffft != NULL)
                call mfree (reffft, TY_REAL)
            call calloc (reffft, 2 * nxfft * nyfft, TY_REAL)
            call rg_psetp (pm, REFFFT, reffft)

            # Load the reference image FFT.
            call rg_rload (Memr[rbuf], nrcols, nrlines, Memr[fft], nxfft,
                nyfft)

            # Load the input image FFT.
            call rg_iload (Memr[ibuf], nrcols, nrlines, Memr[fft], nxfft,
                nyfft)

            # Shift the data for easy of filtering.
            call rg_fshift (Memr[fft], Memr[fft], 2 * nxfft, nyfft)

            # Compute the Fourier Transform of the reference and input image
            # data.
            Meml[dim] = nxfft
            Meml[dim+1] = nyfft
            if (Meml[dim+1] == 1)
                call rg_fourn (Memr[fft], Meml[dim], 1, 1)
            else
                call rg_fourn (Memr[fft], Meml[dim], 2, 1)

            # Compute the flux ratio between the two data sets.
	    if (IS_INDEFR(rg_pstatr(pm, UFLUXRATIO)))
                call rg_psetr (pm, FLUXRATIO, rg_pg2norm (Memr[fft],
                    2 * nxfft, nyfft))
	    else
                call rg_psetr (pm, FLUXRATIO, rg_pstatr(pm, UFLUXRATIO))

            # Separate the two transforms and compute the division.
            call rg_pdivfft (Memr[fft], Memr[reffft], Memr[imfft], Memr[fft],
                2 * nxfft, nyfft)

	} else {

            # Get the reference image FFT.
            reffft = rg_pstatp (pm, REFFFT)

            # Load the input image FFT.
            call rg_rload (Memr[ibuf], nrcols, nrlines, Memr[imfft], nxfft,
                nyfft)

            # Shift the data for easy of filtering.
            call rg_fshift (Memr[imfft], Memr[imfft], 2 * nxfft, nyfft)

            # Compute the Fourier Transform of the input image data.
            Meml[dim] = nxfft
            Meml[dim+1] = nyfft
            if (Meml[dim+1] == 1)
                call rg_fourn (Memr[imfft], Meml[dim], 1, 1)
            else
                call rg_fourn (Memr[imfft], Meml[dim], 2, 1)

            # Compute the flux ratio between the two data sets.
	    if (IS_INDEFR(rg_pstatr(pm, UFLUXRATIO)))
                call rg_psetr (pm, FLUXRATIO, rg_pg1norm (Memr[reffft],
                    2 * nxfft, nyfft) / rg_pg1norm (Memr[imfft], 2 * nxfft,
                    nyfft))
	    else
                call rg_psetr (pm, FLUXRATIO, rg_pstatr(pm, UFLUXRATIO))

            # Divide the two functions.
	    # arg 1,2,3 : incompatible pointer
            call adivx (Memr[reffft], Memr[imfft], Memr[fft], nxfft * nyfft)

	}

	# Normalize the FFT.
	call rg_pnorm (Memr[fft], nxfft, nyfft, rg_pstatr (pm, FLUXRATIO))

	# Free the data buffers.
	if (rbuf != NULL)
	    call mfree (rbuf, TY_REAL)
	if (ibuf != NULL)
	    call mfree (ibuf, TY_REAL)

	call sfree (sp)

	return (OK)
end


# RG_KGET -- Read in the convolution kernel.

int procedure rg_kget (pm, imk)

pointer	pm			#I pointer to the psfmatch structure
pointer	imk			#I pointer to the kernel image

long	c_1
long	nrlines
pointer	conv
pointer	rg_pstatp(), rg_pgdata()

begin
	c_1 = 1

	if (IM_NDIM(imk) == 1)
	    nrlines = 1
	else
	    nrlines = IM_LEN(imk,2)
	conv = rg_pstatp (pm, CONV)
	if (conv != NULL)
	    call mfree (conv, TY_REAL)
	conv = rg_pgdata (imk, c_1, IM_LEN(imk,1), c_1, nrlines)
	call rg_psetp (pm, CONV, conv)

	return (OK)
end


# RG_PFILTER -- Procedure to filter the FFT in frequency space.

procedure rg_pfilter (pm)

pointer	pm		#I pointer to the psf matching structure

size_t	sz_val
pointer	sp, dim, psfft, conv
real	nfactor
int	rg_pstati()
long	rg_pstatl()
pointer	rg_pstatp()
real	rg_pstatr(), asumr()

begin
	call smark (sp)
	sz_val = 2
	call salloc (dim, sz_val, TY_LONG)

	# Allocate space for the fourier spectrum.
	if (rg_pstatp (pm, ASFFT) != NULL)
	    call mfree (rg_pstatp (pm, ASFFT), TY_REAL)
	call calloc (psfft, rg_pstatl (pm, NXFFT) * rg_pstatl (pm, NYFFT),
	    TY_REAL)
	call rg_psetp (pm, ASFFT, psfft)

	# Allocate space for the convolution kernel.
	if (rg_pstatp (pm, CONV) != NULL)
	    call mfree (rg_pstatp (pm, CONV), TY_REAL)
	call malloc (conv, 2 * rg_pstatl (pm, NXFFT) * rg_pstatl (pm, NYFFT),
	    TY_REAL)
	call rg_psetp (pm, CONV, conv)
	call amovr (Memr[rg_pstatp(pm,FFT)], Memr[rg_pstatp(pm,CONV)],
	    2 * rg_pstatl (pm, NXFFT) * rg_pstatl (pm, NYFFT))

#	# Compute the zextend parameter.
#	call rg_psetr (pm, THRESHOLD, rg_pstatr (pm, PRATIO) *
#	    rg_gnorm (Memr[rg_pstatp(pm,IMFFT)], rg_pstatl(pm,NXFFT),
#	    rg_pstatl(pm,NYFFT)))

	# Filter the frequency spectrum.
	switch (rg_pstati(pm,FILTER)) {
	case PM_FCOSBELL:
	    call rg_pcosbell (Memr[rg_pstatp(pm,CONV)], rg_pstatl (pm, NXFFT),
	        rg_pstatl (pm, NYFFT), rg_pstatr (pm, SXINNER), rg_pstatr (pm,
		SXOUTER), rg_pstatr (pm, SYINNER), rg_pstatr (pm, SYOUTER),
		rg_pstati (pm, RADSYM))
	case PM_FREPLACE:
	    call rg_preplace (Memr[rg_pstatp(pm,CONV)], Memr[rg_pstatp(pm,
	        IMFFT)], rg_pstatl (pm, NXFFT), rg_pstatl (pm, NYFFT),
		rg_pstatr (pm,THRESHOLD), rg_pstatr (pm,FLUXRATIO))
	case PM_FMODEL:
	    call rg_pgmodel (Memr[rg_pstatp(pm,CONV)], Memr[rg_pstatp(pm,
	        IMFFT)], rg_pstatl (pm, NXFFT), rg_pstatl (pm, NYFFT),
		rg_pstatr (pm, THRESHOLD), rg_pstatr (pm, FLUXRATIO))
	default:
	    ;
	}

	# Filter out any values greater than the normalization. 
	call rg_pnormfilt (Memr[rg_pstatp(pm,CONV)], rg_pstatl(pm,NXFFT),
	    rg_pstatl(pm,NYFFT), rg_pstatr (pm, FLUXRATIO))

	# Compute the fourier spectrum.
	call rg_pfourier (Memr[rg_pstatp(pm,CONV)], Memr[rg_pstatp(pm,ASFFT)],
	    rg_pstatl(pm,NXFFT), rg_pstatl(pm,NYFFT))

	Meml[dim] = rg_pstatl (pm, NXFFT)
	Meml[dim+1] = rg_pstatl (pm, NYFFT)
	call rg_fshift (Memr[rg_pstatp(pm,CONV)], Memr[rg_pstatp(pm,CONV)],
	    2 * rg_pstatl(pm, NXFFT), rg_pstatl(pm, NYFFT))
	call rg_fourn (Memr[rg_pstatp(pm,CONV)], Meml[dim], 2, -1)
	call rg_fshift (Memr[rg_pstatp(pm,CONV)], Memr[rg_pstatp(pm,CONV)],
	    2 * rg_pstatl(pm, NXFFT), rg_pstatl(pm, NYFFT))
	call adivkr (Memr[rg_pstatp(pm,CONV)], real (rg_pstatl(pm,NXFFT) *
	    rg_pstatl(pm,NYFFT)), Memr[rg_pstatp(pm,CONV)], 2 * rg_pstatl(pm,
	    NXFFT) * rg_pstatl(pm,NYFFT))

	# Unpack the convolution kernel.
	call rg_movexr (Memr[rg_pstatp(pm,CONV)], rg_pstatl(pm,NXFFT),
	    rg_pstatl(pm,NYFFT), Memr[rg_pstatp(pm,CONV)], rg_pstatl(pm,KNX),
	    rg_pstatl(pm,KNY))

	# Normalize the kernel.
	if (! IS_INDEFR(rg_pstatr (pm, NORMFACTOR))) {
	    nfactor = rg_pstatr (pm, NORMFACTOR) / asumr (Memr[rg_pstatp(pm,
		CONV)], rg_pstatl (pm, KNX) * rg_pstatl(pm,KNY))
	    call amulkr (Memr[rg_pstatp (pm,CONV)], nfactor,
	        Memr[rg_pstatp(pm, CONV)], rg_pstatl (pm, KNX) *
		rg_pstatl (pm, KNY))
	}

	# Reallocate the convolution kernel array
	#conv = rg_pstatp (pm, CONV)
	#if (conv != NULL) {
	    #call realloc (conv, rg_pstatl(pm, KNX) * rg_pstatl(pm, KNY),
	        #TY_REAL)
	    #call rg_psetp (pm, CONV, conv) 
	#}

	call sfree (sp)
end


# RG_PGDATA -- Fill a buffer from a specified region of the image.

pointer procedure rg_pgdata (im, c1, c2, l1, l2)

pointer im              #I pointer to the iraf image
long	c1, c2          #I column limits in the input image
long	l1, l2          #I line limits in the input image

long	i
size_t	ncols, nlines, npts
pointer ptr, index, buf
pointer imgs1r(), imgs2r()

begin
        ncols = c2 - c1 + 1
        nlines = l2 - l1 + 1
        npts = ncols * nlines
        call malloc (ptr, npts, TY_REAL)

        index = ptr
        do i = l1, l2 {
            if (IM_NDIM(im) == 1)
                buf = imgs1r (im, c1, c2)
            else
                buf = imgs2r (im, c1, c2, i, i)
            call amovr (Memr[buf], Memr[index], ncols)
            index = index + ncols
        }

        return (ptr)
end


# RG_PNSUM -- Compute the total intensity in the subtracted subraster.

real procedure rg_pnsum (data, ncols, nlines, nxdata, nydata)

real	data[ncols,nlines]		#I the input data subraster
size_t	ncols, nlines			#I the size of the input subraster
size_t	nxdata, nydata			#I the size of the data region

long	j, wxborder, wyborder, npts
real	sum
bool	fp_equalr()
real	asumr()

begin
	wxborder = (ncols - nxdata) / 2
	wyborder = (nlines - nydata) / 2
	
	sum = 0.0
	npts = 0
	do j = 1 + wyborder, nlines - wyborder {
	    sum = sum + asumr (data[1+wxborder,j], nxdata)
	    npts = npts + nxdata
	}
	if (npts <= 0 || fp_equalr (sum, 0.0))
	    return (INDEFR)
	else
	    return (sum)
end


# RG_PWRITE -- Save the convolution kernel and the fourier spectrum of the
# convolution kernel in an image.

procedure rg_pwrite (pm, imk, imf)

pointer	pm			#I pointer to psf matching structure
pointer	imk			#I pointer to kernel image
pointer imf			#I pointer to fourier spectrum image

long	c_1
long	nx, ny
pointer	buf
long	rg_pstatl()
pointer	rg_pstatp(), imps2r()

begin
	c_1 = 1
	# Write out the kernel image.
	if (imk != NULL && rg_pstatp(pm, CONV) != NULL) {
	    nx = rg_pstatl (pm, KNX)
	    ny = rg_pstatl (pm, KNY)
	    IM_NDIM(imk) = 2
	    IM_LEN(imk,1) = nx
	    IM_LEN(imk,2) = ny
	    IM_PIXTYPE(imk) = TY_REAL
	    buf = imps2r (imk, c_1, nx, c_1, ny)
	    if (rg_pstatp (pm, CONV) != NULL)
	        call amovr (Memr[rg_pstatp(pm,CONV)], Memr[buf], nx * ny)
	    else
	        call amovkr (0.0, Memr[buf], nx * ny)
	}

	# Write out the fourier spectrum.
	if (imf != NULL && rg_pstatp(pm,ASFFT) != NULL) {
	    nx = rg_pstatl (pm, NXFFT)
	    ny = rg_pstatl (pm, NYFFT)
	    IM_NDIM(imf) = 2
	    IM_LEN(imf,1) = nx
	    IM_LEN(imf,2) = ny
	    IM_PIXTYPE(imf) = TY_REAL
	    buf = imps2r (imf, c_1, nx, c_1, ny)
	    if (rg_pstatp (pm, CONV) != NULL)
	        call amovr (Memr[rg_pstatp(pm,ASFFT)], Memr[buf], nx * ny)
	    else
	        call amovkr (0.0, Memr[buf], nx * ny)
	}
end

