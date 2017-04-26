include	<imhdr.h>
include <imset.h>

define	TMINSW		1.00	# Relative timings for nvecs = 5
define	TMXMNSW		1.46
define	TMED3		0.18
define	TMED5		0.55

# RS_APSUMR -- Sum or average images using input masks with optional high and
# low pixel rejection. This version of the routines takes a list of image and
# mask pointers as input.
# 
# This procedure is a modified version of code used by the imsum task which
# was easy to modify for the present purposes.

procedure rs_apsumr (imptrs, mskptrs, imids, im_out, msk_out, start, finish,
	current, flow, fhigh, skyscale)

pointer	imptrs[ARB]			#I the input image pointers
pointer	mskptrs[ARB]			#I the input mask pointers
int	imids[ARB]			#I the list of image ids
pointer	im_out				#I Output image descriptor
pointer	msk_out				#I Output "mask" descriptor
int	start				#I The starting image for the sum
int	finish				#I The ending image for the sum
int	current				#I The current image to be skipped
real	flow				#I Number of low pixels to reject
real	fhigh				#I Number of high pixels to reject
char	skyscale[ARB]			#I Keyword containing scaling factor

pointer	sp, im, mpim, norm, vout, mvout, vs, ve, vin
pointer	buf_out, buf_msk, buf_in, pbuf
int	i, n, nimages, npix, npts, mval

real	imgetr()
pointer	mio_openo()
int	impnlr(), impnli(), mio_glsegr(), imstati()
errchk	imgetr()

begin
	# Initialize.
	nimages = finish - start
	npix = IM_LEN(im_out, 1)

	# Allocate memory.
	call smark (sp)
	call salloc (im, nimages, TY_INT)
	call salloc (mpim, nimages, TY_INT)
	call salloc (norm, nimages, TY_REAL)
	call salloc (vs, IM_MAXDIM, TY_LONG)
	call salloc (ve, IM_MAXDIM, TY_LONG)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (mvout, IM_MAXDIM, TY_LONG)

	# If there are no pixels to be rejected avoid calls to reject pixels.
	# This case will not actually be used in the rskysub task because it
	# is handled more efficiently in a different module but is included
	# for completeness.

	if ((flow <= 0.0) && (fhigh <= 0.0)) {

	    # Open the images.
	    n = 0
	    do i = 1, finish - start + 1 {
		if (imids[i] == current)
		    next
	        Memi[im+n] = imptrs[i]
		iferr (Memr[norm+n] = imgetr (imptrs[i], skyscale))
		    Memr[norm+n] = 1.0
		Memi[mpim+n] = mio_openo (imstati(mskptrs[i], IM_PLDES),
		        imptrs[i])
		n = n + 1
	    }

	    # Initialize i/o.
	    call amovkl (long(1), Meml[vout], IM_MAXDIM)
	    call amovkl (long(1), Meml[mvout], IM_MAXDIM)
	    call amovkl (long(1), Meml[vs], IM_MAXDIM)
	    call amovkl (long(1), Meml[ve], IM_MAXDIM)
	    Meml[ve] = npix

	    # For each input line compute an output line.
	    while (impnlr (im_out, buf_out, Meml[vout]) != EOF &&
		impnli (msk_out, buf_msk, Meml[mvout]) != EOF) {

		# Clear the output buffer.
		call aclrr (Memr[buf_out], npix)
		call aclri (Memi[buf_msk], npix)

		# Accumulate lines from each input image.
	    	do i = 1, n {
		    call mio_setrange (Memi[mpim+i-1], Meml[vs], Meml[ve],
			IM_NDIM(Memi[im+i-1]))
		    call amovl (Meml[vs], Meml[vin], IM_MAXDIM)
		    while (mio_glsegr (Memi[mpim+i-1], buf_in, mval,
		        Meml[vin], npts) != EOF) {
		        call awsur (Memr[buf_in], Memr[buf_out+Meml[vin]-1],
			    Memr[buf_out+Meml[vin]-1], npts, Memr[norm+i-1],
			    1.0)
		        call aaddki (Memi[buf_msk+Meml[vin]-1], 1,
			    Memi[buf_msk+Meml[vin]-1], npts)
		    }
		}

		# Compute the average.
		do i = 1, npix {
		    if (Memi[buf_msk+i-1] > 1)
			Memr[buf_out+i-1] = Memr[buf_out+i-1] /
			    Memi[buf_msk+i-1]
		}

		# Set the i/o parameters.
		call amovl (Meml[vout], Meml[vs], IM_MAXDIM)
		call amovl (Meml[vout], Meml[ve], IM_MAXDIM)
		Meml[vs] = 1
		Meml[ve] = npix
	    }

	    # Unmap the images.
	    do i = 1, n 
		call mio_close (Memi[mpim+i-1])

	    # Finish up.
	    call sfree (sp)
	    return
	}

	# Pixel rejection is turned on.
	       
	# Collect the images to be combined and open them for masked i/o.
	n = 0
	do i = 1, finish - start + 1 {
	    if (imids[i] == current)
		next
	    Memi[im+n] = imptrs[i]
	    iferr (Memr[norm+n] = imgetr (imptrs[i], skyscale))
	        Memr[norm+n] = 1.0
	    Memi[mpim+n] = mio_openo (imstati(mskptrs[i], IM_PLDES), imptrs[i])
	   n = n + 1
	}

	# Allocate additional buffer space.
	call salloc (pbuf, nimages * npix, TY_REAL)

	# Initialize the i/o.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[mvout], IM_MAXDIM)
	call amovkl (long(1), Meml[vs], IM_MAXDIM)
	call amovkl (long(1), Meml[ve], IM_MAXDIM)
	Meml[ve] = npix

	# Compute output lines for each input line.
	while (impnlr (im_out, buf_out, Meml[vout]) != EOF &&
	    impnli (msk_out, buf_msk, Meml[mvout]) != EOF) {

	    # Initialize the output image.
	    call aclri (Memi[buf_msk], npix)

	    # Read lines from the input images.
	    for (i = 1; i <= n; i = i + 1) {
		call mio_setrange (Memi[mpim+i-1], Meml[vs], Meml[ve],
		    IM_NDIM(Memi[im+i-1]))
		call amovl (Meml[vs], Meml[vin], IM_MAXDIM)
		while (mio_glsegr (Memi[mpim+i-1], buf_in, mval, Meml[vin],
		    npts) != EOF) {
		    call rs_accumr (Memr[buf_in], npts, Meml[vin] - 1,
		        Memr[norm+i-1], Memr[pbuf], Memi[buf_msk], npix)
		}
	    }

	    # Reject pixels.
	    call rs_mmrejr (Memr[pbuf], Memi[buf_msk], Memr[buf_out], npix,
	        flow, fhigh)

	    # If averaging divide the sum by the number of images averaged.
	    do i = 1, npix {
		if (Memi[buf_msk+i-1] > 1)
		    Memr[buf_out+i-1] = Memr[buf_out+i-1] / Memi[buf_msk+i-1]
	    }

	    # Set the i/o parameters.
	    call amovl (Meml[vout], Meml[vs], IM_MAXDIM)
	    call amovl (Meml[vout], Meml[ve], IM_MAXDIM)
	    Meml[vs] = 1
	    Meml[ve] = npix
	}

	# Finish up.
	do i = 1, n
	    call mio_close (Memi[mpim+i-1])

	call sfree (sp)
end


# RS_PSUMR -- Sum or average images using input masks with optional high and
# low pixel rejection. This version of the routines takes a list of images and
# masks as input.
# 
# This procedure is a modified version of code used by the imsum task which
# was easy to modify for the present purposes.

procedure rs_psumr (list, msklist, im_out, msk_out, start, finish, current,
	flow, fhigh, msk_invert, skyscale)

int	list				#I List of input images
int	msklist				#I List of input masks
pointer	im_out				#I Output image descriptor
pointer	msk_out				#I Output "mask" descriptor
int	start				#I The starting image for the sum
int	finish				#I The ending image for the sum
int	current				#I The current image to be skipped
real	flow				#I Number of low pixels to reject
real	fhigh				#I Number of high pixels to reject
bool	msk_invert			#I inver the input mask ?
char	skyscale[ARB]			#I Keyword containing scaling factor

pointer	sp, input, str, im, mkim, mpim, norm, vout, mvout, vs, ve, vin
pointer	buf_out, buf_msk, buf_in, pbuf
int	i, n, nimages, npix, npts, mval

real	imgetr()
pointer	immap(), mp_open(), mio_openo()
int	imtrgetim(), impnlr(), impnli(), mio_glsegr(), imstati()
errchk	imgetr()

begin
	# Initialize.
	nimages = finish - start
	npix = IM_LEN(im_out, 1)

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (im, nimages, TY_INT)
	call salloc (mkim, nimages, TY_INT)
	call salloc (mpim, nimages, TY_INT)
	call salloc (norm, nimages, TY_REAL)
	call salloc (vs, IM_MAXDIM, TY_LONG)
	call salloc (ve, IM_MAXDIM, TY_LONG)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (mvout, IM_MAXDIM, TY_LONG)

	# If there are no pixels to be rejected avoid calls to reject pixels.
	# This case will not actually be used in the rskysub task because it
	# is handled more efficiently in a different module but is included
	# for completeness.

	if (flow <= 0.0 && fhigh <= 0.0) {

	    # Open the images.
	    n = 0
	    do i = start, finish {
		if (i == current)
		    next
	        if (imtrgetim (list, i, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    iferr (Memr[norm+n] = imgetr (Memi[im+n], skyscale))
			Memr[norm+n] = 1.0
                    if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                        if (msk_invert) {
                            Memc[str] = '^'
                            Memi[mkim+n] = mp_open (Memc[str], Memi[im+n],
			        Memc[input], SZ_FNAME)
                        } else
                            Memi[mkim+n] = mp_open (Memc[str+1], Memi[im+n],
				Memc[input], SZ_FNAME)
                    } else if (imtrgetim (msklist, 1, Memc[str],
			SZ_FNAME) != EOF) {
                        Memi[mkim+n] = mp_open (Memc[str], Memi[im+n],
			    Memc[input], SZ_FNAME)
                    } else {
                        Memi[mkim+n] = mp_open ("", Memi[im+n], Memc[input],
			    SZ_FNAME)
                    }
		    Memi[mpim+n] = mio_openo (imstati(Memi[mkim+n], IM_PLDES),
		        Memi[im+n])
		    n = n + 1
		}
	    }

	    # Initialize i/o.
	    call amovkl (long(1), Meml[vout], IM_MAXDIM)
	    call amovkl (long(1), Meml[mvout], IM_MAXDIM)
	    call amovkl (long(1), Meml[vs], IM_MAXDIM)
	    call amovkl (long(1), Meml[ve], IM_MAXDIM)
	    Meml[ve] = npix

	    # For each input line compute an output line.
	    while (impnlr (im_out, buf_out, Meml[vout]) != EOF &&
		impnli (msk_out, buf_msk, Meml[mvout]) != EOF) {

		# Clear the output buffer.
		call aclrr (Memr[buf_out], npix)
		call aclri (Memi[buf_msk], npix)

		# Accumulate lines from each input image.
	    	do i = 1, n {
		    call mio_setrange (Memi[mpim+i-1], Meml[vs], Meml[ve],
			IM_NDIM(Memi[im+i-1]))
		    call amovl (Meml[vs], Meml[vin], IM_MAXDIM)
		    while (mio_glsegr (Memi[mpim+i-1], buf_in, mval,
		        Meml[vin], npts) != EOF) {
		        call awsur (Memr[buf_in], Memr[buf_out+Meml[vin]-1],
			    Memr[buf_out+Meml[vin]-1], npts, Memr[norm+i-1],
			    1.0)
		        call aaddki (Memi[buf_msk+Meml[vin]-1], 1,
			    Memi[buf_msk+Meml[vin]-1], npts)
		    }
		}

		# Compute the average.
		do i = 1, npix {
		    if (Memi[buf_msk+i-1] > 1)
			Memr[buf_out+i-1] = Memr[buf_out+i-1] /
			    Memi[buf_msk+i-1]
		}

		# Set the i/o parameters.
		call amovl (Meml[vout], Meml[vs], IM_MAXDIM)
		call amovl (Meml[vout], Meml[ve], IM_MAXDIM)
		Meml[vs] = 1
		Meml[ve] = npix
	    }

	    # Unmap the images.
	    do i = 1, n {
		call mio_close (Memi[mpim+i-1])
	        call imunmap (Memi[mkim+i-1])
	        call imunmap (Memi[im+i-1])
	    }

	    # Finish up.
	    call sfree (sp)
	    return
	}

	# Pixel rejection is turned on.
	       
	# Collect the images to be combined and open them for masked i/o.
	n = 0
	do i = start, finish {
	    if (i == current)
		next
	    if (imtrgetim (list, i, Memc[input], SZ_FNAME) != EOF) {
	        Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		iferr (Memr[norm+n] = imgetr (Memi[im+n], skyscale))
		    Memr[norm+n] = 1.0
                if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                    if (msk_invert) {
                        Memc[str] = '^'
                        Memi[mkim+n] = mp_open (Memc[str], Memi[im+n],
		            Memc[input], SZ_FNAME)
                    } else
                        Memi[mkim+n] = mp_open (Memc[str+1], Memi[im+n],
			    Memc[input], SZ_FNAME)
                } else if (imtrgetim (msklist, 1, Memc[str], SZ_FNAME) != EOF) {
                    Memi[mkim+n] = mp_open (Memc[str], Memi[im+n],
		        Memc[input], SZ_FNAME)
                } else {
                    Memi[mkim+n] = mp_open ("", Memi[im+n], Memc[input],
		        SZ_FNAME)
                }
		Memi[mpim+n] = mio_openo (imstati(Memi[mkim+n], IM_PLDES),
		    Memi[im+n])
	        n = n + 1
	    }
	}

	# Allocate additional buffer space.
	call salloc (pbuf, nimages * npix, TY_REAL)

	# Initialize the i/o.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[mvout], IM_MAXDIM)
	call amovkl (long(1), Meml[vs], IM_MAXDIM)
	call amovkl (long(1), Meml[ve], IM_MAXDIM)
	Meml[ve] = npix

	# Compute output lines for each input line.
	while (impnlr (im_out, buf_out, Meml[vout]) != EOF &&
	    impnli (msk_out, buf_msk, Meml[mvout]) != EOF) {

	    # Initialize the output image.
	    call aclri (Memi[buf_msk], npix)

	    # Read lines from the input images.
	    for (i = 1; i <= n; i = i + 1) {
		call mio_setrange (Memi[mpim+i-1], Meml[vs], Meml[ve],
		    IM_NDIM(Memi[im+i-1]))
		call amovl (Meml[vs], Meml[vin], IM_MAXDIM)
		while (mio_glsegr (Memi[mpim+i-1], buf_in, mval, Meml[vin],
		    npts) != EOF) {
		    call rs_accumr (Memr[buf_in], npts, Meml[vin] - 1,
		        Memr[norm+i-1], Memr[pbuf], Memi[buf_msk], npix)
		}
	    }

	    # Reject pixels.
	    call rs_mmrejr (Memr[pbuf], Memi[buf_msk], Memr[buf_out], npix,
	        flow, fhigh)

	    # If averaging divide the sum by the number of images averaged.
	    do i = 1, npix {
		if (Memi[buf_msk+i-1] > 1)
		    Memr[buf_out+i-1] = Memr[buf_out+i-1] / Memi[buf_msk+i-1]
	    }

	    # Set the i/o parameters.
	    call amovl (Meml[vout], Meml[vs], IM_MAXDIM)
	    call amovl (Meml[vout], Meml[ve], IM_MAXDIM)
	    Meml[vs] = 1
	    Meml[ve] = npix
	}

	# Finish up.
	do i = 1, n {
	    call mio_close (Memi[mpim+i-1])
	    call imunmap (Memi[mkim+i-1])
	    call imunmap (Memi[im+i-1])
	}
	call sfree (sp)
end


# RS_ASUMR -- Sum or average images with optional high and low pixel rejection.
# This version of the routine takes a list of image pointers as input. Median
# combining is enabled if either of the incoming nlow or nhigh parameters is
# INDEF.
# 
# This procedure is a simplified version of code used by the imsum task which
# was easy to modify for the present purposes.

procedure rs_asumr (imptrs, imids, im_out, start, finish, current, nlow, nhigh,
	skyscale)

pointer	imptrs[ARB]			#I the image pointers
int	imids[ARB]			#I the image ids
pointer	im_out				#I Output image descriptor
int	start				#I The starting image for the sum
int	finish				#I The ending image for the sum
int	current				#I The current image to be skipped
int	nlow				#I Number of low pixels to reject
int	nhigh				#I Number of high pixels to reject
char	skyscale[ARB]			#I Keyword containing scaling factor

real	const
pointer	sp, v1, v2, im, norm, buf_out, buf_in, pbuf, rbuf
int	i, n, nl, nh, nimages, naccept, npix
real	imgetr()
int	impnlr(), imgnlr()
errchk	imgetr()

begin
	# Initialize.
	nimages = finish - start
	if (IS_INDEFI(nlow) || IS_INDEFI(nhigh)) {
	    if (mod (nimages,2) == 0) {
		nl = nimages / 2 - 1
		nh = nimages / 2 - 1
	    } else {
		nl = nimages / 2
		nh = nimages / 2
	    }
	} else {
	    nl = nlow
	    nh = nhigh
	}
	naccept = nimages - nl - nh
	const = naccept
	npix = IM_LEN(im_out, 1)

	# Allocate memory.
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)
	call salloc (norm, nimages, TY_REAL)

	# If there are no pixels to be rejected avoid calls to reject pixels.
	# This case will not actually be used in the rskysub task because it
	# is handled more efficiently in a different module but is included
	# for completeness.

	if ((nl == 0) && (nh == 0)) {

	    # Open the images.
	    n = 0
	    do i = 1, finish - start + 1 {
		if (imids[i] == current)
		    next
	        Memi[im+n] = imptrs[i]
		iferr (Memr[norm+n] = imgetr (Memi[im+n], skyscale))
		    Memr[norm+n] = 1.0
		n = n + 1
	    }

	    # Initialize i/o.
	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	    # For each input line compute an output line.
	    while (impnlr (im_out, buf_out, Meml[v2]) != EOF) {

		# Clear the output buffer.
		call aclrr (Memr[buf_out], npix)

		# Accumulate lines from each input image.
	    	do i = 1, n {
		    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		    if (imgnlr (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	call error (0, "Error reading input image")
		    call awsur (Memr[buf_in], Memr[buf_out], Memr[buf_out],
			npix, Memr[norm+i-1], 1.0)
		    #call amulkr (Memr[buf_in], Memr[norm+i-1], Memr[buf_in],
		        #npix)
		    #call aaddr (Memr[buf_in], Memr[buf_out], Memr[buf_out],
		        #npix)
		}

		# Compute the average.
		call adivkr (Memr[buf_out], const, Memr[buf_out], npix)

		# Set the i/o parameters.
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	    }

	    # Finish up.
	    call sfree (sp)
	    return
	}

	# Pixel rejection is turned on.
	       
	n = 0
	do i = 1, finish - start + 1 {
	    if (imids[i] == current)
		next
	    Memi[im+n] = imptrs[i]
	    iferr (Memr[norm+n] = imgetr (Memi[im+n], skyscale))
		Memr[norm+n] = 1.0
	    n = n + 1
	}

	# Allocate additional buffer space.
	call salloc (pbuf, nimages, TY_INT)
	call salloc (rbuf, nimages * npix, TY_REAL)

	# Initialize the i/o.
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnlr (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the input images.
	    for (i = 1; i <= n; i = i + 1) {
		Memi[pbuf+i-1] = rbuf + (i - 1) * npix
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnlr (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		call amulkr (Memr[buf_in], Memr[norm+i-1], Memr[Memi[pbuf+i-1]],
		    npix)
	    }

	    # Reject pixels. Sum the remaining pixels.
	    call rs_rejr (Memi[pbuf], nimages, Memr[buf_out], npix, nl, nh)

	    # If averaging divide the sum by the number of images averaged.
	    if (naccept > 1) {
		const = naccept
		call adivkr (Memr[buf_out], const, Memr[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	call sfree (sp)
end


# RS_SUMR -- Sum or average images with optional high and low pixel rejection.
# This version of the routines takes a list of images as input. Medianing
# combining is enabled if either of the incoming nlow or nhigh values is
# INDEF
# 
# This procedure is a simplified version of code used by the imsum task which
# was easy to modify for the present purposes.

procedure rs_sumr (list, im_out, start, finish, current, nlow, nhigh, skyscale)

int	list				#I List of input images
pointer	im_out				#I Output image descriptor
int	start				#I The starting image for the sum
int	finish				#I The ending image for the sum
int	current				#I The current image to be skipped
int	nlow				#I Number of low pixels to reject
int	nhigh				#I Number of high pixels to reject
char	skyscale[ARB]			#I Keyword containing scaling factor

real	const
pointer	sp, input, v1, v2, im, norm, buf_out, buf_in, buf
int	i, n, nimages, naccept, npix, nl, nh
real	imgetr()
pointer	immap()
int	imtrgetim(), impnlr(), imgnlr()
errchk	imgetr()

begin
	# Initialize.
	nimages = finish - start
	if (IS_INDEFI(nlow) || IS_INDEFI(nhigh)) {
	    if (mod (nimages,2) == 0) {
		nl = nimages / 2 - 1
		nh = nimages / 2 - 1
	    } else {
		nl = nimages / 2
		nh = nimages / 2
	    }
	} else {
	    nl = nlow
	    nh = nhigh
	}
	naccept = nimages - nl - nh
	const = naccept
	npix = IM_LEN(im_out, 1)

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)
	call salloc (norm, nimages, TY_REAL)

	# If there are no pixels to be rejected avoid calls to reject pixels.
	# This case will not actually be used in the rskysub task because it
	# is handled more efficiently in a different module but is included
	# for completeness.

	if ((nl == 0) && (nh == 0)) {

	    # Open the images.
	    n = 0
	    do i = start, finish {
		if (i == current)
		    next
	        if (imtrgetim (list, i, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    iferr (Memr[norm+n] = imgetr (Memi[im+n], skyscale))
			Memr[norm+n] = 1.0
		    n = n + 1
		}
	    }

	    # Initialize i/o.
	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	    # For each input line compute an output line.
	    while (impnlr (im_out, buf_out, Meml[v2]) != EOF) {

		# Clear the output buffer.
		call aclrr (Memr[buf_out], npix)

		# Accumulate lines from each input image.
	    	do i = 1, n {
		    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		    if (imgnlr (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	call error (0, "Error reading input image")
		    call amulkr (Memr[buf_in], Memr[norm+i-1], Memr[buf_in],
		        npix)
		    call aaddr (Memr[buf_in], Memr[buf_out], Memr[buf_out],
		        npix)
		}

		# Compute the average.
		call adivkr (Memr[buf_out], const, Memr[buf_out], npix)

		# Set the i/o parameters.
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	    }

	    # Unmap the images.
	    do i = 1, n
	        call imunmap (Memi[im+i-1])

	    # Finish up.
	    call sfree (sp)
	    return
	}

	# Pixel rejection is turned on.
	       
	n = 0
	do i = start, finish {
	    if (i == current)
		next
	    if (imtrgetim (list, i, Memc[input], SZ_FNAME) != EOF) {
	        Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		iferr (Memr[norm+n] = imgetr (Memi[im+n], skyscale))
		    Memr[norm+n] = 1.0
	        n = n + 1
	    }
	}

	# Allocate additional buffer space.
	call salloc (buf, nimages, TY_INT)

	# Initialize the i/o.
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnlr (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the input images.
	    for (i = 1; i <= n; i = i + 1) {
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnlr (Memi[im+i-1], Memi[buf+i-1], Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		call amulkr (Memr[Memi[buf+i-1]], Memr[norm+i-1],
		    Memr[Memi[buf+i-1]], npix)
	    }

	    # Reject pixels. Sum the remaining pixels.
	    call rs_rejr (Memi[buf], nimages, Memr[buf_out], npix, nl, nh)

	    # If averaging divide the sum by the number of images averaged.
	    if (naccept > 1) {
		const = naccept
		call adivkr (Memr[buf_out], const, Memr[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	do i = 1, n
	    call imunmap (Memi[im+i-1])
	call sfree (sp)
end


# RS_ACCUMR -- Acumulate the masked data into the input buffer.

procedure rs_accumr (indata, npts, offset, norm, outdata, ndata, npix)

real	indata[npts]			#I the input data 
int	npts				#I the number of input data points
int	offset				#I the offset of the first data point
real	norm				#I the normalization factor
real	outdata[npix,ARB]		#U the output array
int	ndata[npix]			#U the number of good data points
int	npix				#I number of points in a line

int	i

begin
	do i = 1, npts {
	    ndata[i+offset] = ndata[i+offset] + 1
	    outdata[i+offset,ndata[i+offset]] = norm * indata[i]
	}
end


# RS_MMREJR --  Reject a specified number of high and low pixels. This routine
# is a modified version of one in imcombine. It works off a real data
# buffer rather than a set of image i/o buffers. It also sums the points at
# the end

procedure rs_mmrejr (indata, n, out, npts, flo, fhi)

real	indata[npts,ARB]	#U the data buffer of good pixels
int	n[npts]			#U The number of good pixels
real	out[npts]		#O the output sum
int	npts			#I The number of output points per line
real	flo			#I Fraction of low points to reject
real	fhi			#I Fraction of high points to reject


real	d1, d2, dmin, dmax, sum
int	n1, npairs, nlow, nhigh, naccept, np, nlo, nhi, medflag
int	i, j, jmax, jmin


begin
	if (IS_INDEFR(flo) || IS_INDEFR(fhi))
	    medflag = YES
	else
	    medflag = NO

	do i = 1, npts {

	    n1 = n[i]
	    if (medflag == YES) {
		if (mod (n1, 2) == 0) {
		    nlo = n1 / 2 - 1
		    nhi = n1 / 2 - 1
		} else {
		    nlo = n1 / 2
		    nhi = n1 / 2
		}
	    } else {
	        nlo = flo * n1 + 0.001
	        nhi = fhi * n1 + 0.001
	    }
	    naccept = n1 - nlo - nhi

	    # No points are rejected.
	    if (naccept == n1)
		next

	    # All points are rejected.
	    if (naccept <= 0) {
		n[i] = 0
		next
	    }

	    npairs = min (nlo, nhi)
    	    nlow = nlo - npairs
    	    nhigh = nhi - npairs

	    # Reject the npairs low and high points.
	    do np = 1, npairs {
		d1 = indata[i,1]
		dmax = d1; dmin = d1; jmax = 1; jmin = 1
		do j = 2, n1 {
		    d2 = d1
		    d1 = indata[i,j]
		    if (d1 > dmax) {
			dmax = d1; jmax = j
		    } else if (d1 < dmin) {
			dmin = d1; jmin = j
		    }
		}
		j = n1 - 1
		if (jmax < j) {
		    if (jmin != j)
			indata[i,jmax] = d2
		    else
			indata[i,jmax] = d1
		}
		if (jmin < j) {
		    if (jmax != n1)
			indata[i,jmin] = d1
		    else
			indata[i,jmin] = d2
		}
		n1 = n1 - 2
	    }

	    # Reject the excess low points.
	    do np = 1, nlow {
		d1 = indata[i,1]
		dmin = d1; jmin = 1
		do j = 2, n1 {
		     d1 = indata[i,j]
		    if (d1 < dmin) {
			dmin = d1; jmin = j
		    }
		}
		if (jmin < n1)
		    indata[i,jmin] = d1
		n1 = n1 - 1 
	    }

	    # Reject the excess high points.
	    do np = 1, nhigh {
		d1 = indata[i,1]
		dmax = d1; jmax = 1
		do j = 2, n1 {
		    d1 = indata[i,j]
		    if (d1 > dmax) {
			dmax = d1; jmax = j
		    }
		}
		if (jmax < n1)
		    indata[i,jmax] = d1
		n1 = n1 - 1 
	    }

	    n[i] = n1
	}

	# Compute the sum.
	do i = 1, npts {
	    if (n[i] == 0) {
		out[i] = 0.0 
	    } else if (n[i] == 1) {
		out[i] = indata[i,1]
	    } else {
	        sum = indata[i,1]
	        do j = 2, n[i]
		    sum = sum + indata[i,j]
		out[i] = sum
	    }
	}
end


## RS_MMREJR --  Reject a specified number of high and low pixels from a
## buffer by doing min / max comparison, reordering the data buffer, and
## editing the number of good pixels array. This routine is a modified
## version of the one in the imcombine task.
#
#procedure rs_mmrejr (d, n, npts, nlo, nhi)
#
#pointer	d[ARB]			#I The input data pointers
#int	n[npts]			#U The number of good pixels
#int	npts			#I The number of output points per line
#int	nlo			#I Number of low points to reject
#int	nhi			#I Number of high points to reject
#
#real	d1, d2, dmin, dmax
#pointer	k, kmax, kmin
#int	n1, npairs, nlow, nhigh, np
#int	i, i1, j, jmax, jmin
#
#begin
#       npairs = min (nlo, nhi)
#    	nlow = nlo - npairs
#       nhigh = nhi - npairs
#	do i = 1, npts {
#
#	    i1 = i - 1
#	    n1 = n[i]
#	    naccept = n1 - nlo - nhi
#	    if (naccept == n1)
#	        next
#	    if (naccept <= 0) {
#	        n[i] = 0
#	        next
#	    }
#
#
#
#	    # Reject the npairs low and high points.
#	    do np = 1, npairs {
#		k = d[1] + i1
#		d1 = Memr[k]
#		dmax = d1; dmin = d1; jmax = 1; jmin = 1; kmax = k; kmin = k
#		do j = 2, n1 {
#		    d2 = d1
#		    k = d[j] + i1
#		    d1 = Memr[k]
#		    if (d1 > dmax) {
#			dmax = d1; jmax = j; kmax = k
#		    } else if (d1 < dmin) {
#			dmin = d1; jmin = j; kmin = k
#		    }
#		}
#		j = n1 - 1
#		if (jmax < j) {
#		    if (jmin != j)
#			Memr[kmax] = d2
#		    else
#			Memr[kmax] = d1
#		}
#		if (jmin < j) {
#		    if (jmax != n1)
#			Memr[kmin] = d1
#		    else
#			Memr[kmin] = d2
#		}
#		n1 = n1 - 2
#	    }
#
#	    # Reject the excess low points.
#	    do np = 1, nlow {
#		k = d[1] + i1
#		d1 = Memr[k]
#		dmin = d1; jmin = 1; kmin = k
#		do j = 2, n1 {
#		    k = d[j] + i1
#		    d1 = Memr[k]
#		    if (d1 < dmin) {
#			dmin = d1; jmin = j; kmin = k
#		    }
#		}
#		if (jmin < n1)
#		    Memr[kmin] = d1
#		n1 = n1 - 1 
#	    }
#
#	    # Reject the excess high points.
#	    do np = 1, nhigh {
#		k = d[1] + i1
#		d1 = Memr[k]
#		dmax = d1; jmax = 1; kmax = k
#		do j = 2, n1 {
#		    k = d[j] + i1
#		    d1 = Memr[k]
#		    if (d1 > dmax) {
#			dmax = d1; jmax = j; kmax = k
#		    }
#		}
#		if (jmax < n1)
#		    Memr[kmax] = d1
#		n1 = n1 - 1 
#	    }
#
#	    n[i] = n1
#	}
#end


# RS_REJR --  Reject the number of high and low points and sum the rest.

procedure rs_rejr (a, nvecs, b, npts, nlow, nhigh)

pointer	a[nvecs]		# Pointers to set of vectors
int	nvecs			# Number of vectors
real	b[npts]			# Output vector
int	npts			# Number of points in the vectors
int	nlow			# Number of low points to be rejected
int	nhigh			# Number of high points to be rejected

int	i, j
int	naccept, minrej, npairs, nlow1, nhigh1
real	tmedian, time1, time2

begin
	naccept = nvecs - nlow - nhigh

	# If no points are rejected return the sum.

	if (naccept == nvecs) {
	    call amovr (Memr[a[1]], b, npts)
	    for (j = 2; j <= naccept; j = j + 1)
		call aaddr (Memr[a[j]], b, b, npts)
	    return
	}

	minrej = min (nlow, nhigh)
	npairs = minrej
	nlow1 = nlow - npairs
	nhigh1 = nhigh - npairs

	if ((naccept == 1) && (npairs > 0)) {
	    if (npairs == 1) {
	        tmedian = TMED3
		npairs = npairs - 1
	    } else {
		tmedian = TMED5
	        npairs = npairs - 2
	    }
	} else
	    tmedian = 0

	# Compare the time required to reject the minimum number
	# of low or high points and extract the number of points to accept
	# with the time to reject pairs and the excess number of low or
	# high points to either reach a median of 3 or 5 points or isolate
	# the acceptable points.

	time1 = TMINSW * (minrej + naccept)
	time2 = tmedian + TMXMNSW * npairs + TMINSW * (nlow1 + nhigh1)

	i = nvecs
	if (time1 < time2) {

 	    # Sort the nlow and naccept points
	    if (nlow < nhigh) {
	        for (j = 1; j <= nlow + naccept; j = j + 1) {
	            call rs_minswr (a, i, npts)
		    i = i - 1
	        }
	    	call amovr (Memr[a[nhigh+1]], b, npts)
		for (j = nhigh+2; j <= nhigh+naccept; j = j + 1)
		    call aaddr (Memr[a[j]], b, b, npts)

	    # Sort the nhigh and naccept points
	    } else {
	        for (j = 1; j <= nhigh + naccept; j = j + 1) {
	            call rs_maxswr (a, i, npts)
		    i = i - 1
	        }
	    	call amovr (Memr[a[nlow+1]], b, npts)
		for (j = nlow+2; j <= nlow+naccept; j = j + 1)
		    call aaddr (Memr[a[j]], b, b, npts)
	    }

	} else {
	    # Reject the npairs low and high points.
	    for (j = 1; j <= npairs; j = j + 1) {
	        call rs_mxmnswr (a, i, npts)
		i = i - 2
	    }
	    # Reject the excess low points.
	    for (j = 1; j <= nlow1; j = j + 1) {
	        call rs_minswr (a, i, npts)
		i = i - 1
	    }
	    # Reject the excess high points.
	    for (j = 1; j <= nhigh1; j = j + 1) {
	        call rs_maxswr (a, i, npts)
		i = i - 1
	    }

	    # Check if the remaining points constitute a 3 or 5 point median
	    # or the set of desired points.
	    if (tmedian == 0.) {
	        call amovr (Memr[a[1]], b, npts)
	        for (j = 2; j <= naccept; j = j + 1)
		    call aaddr (Memr[a[j]], b, b, npts)
	    } else if (tmedian == TMED3) {
		call amed3r (Memr[a[1]], Memr[a[2]], Memr[a[3]], b, npts)
	    } else {
		call amed5r (Memr[a[1]], Memr[a[2]], Memr[a[3]],
		        Memr[a[4]], Memr[a[5]], b, npts)
	    }
	}
end


# RS_MINSWR -- Given an array of vector pointers for each element in the vectors
# swap the minimum element with that of the last vector.

procedure rs_minswr (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmin
real	temp

begin
	do i = 0, npts - 1 {
	    kmin = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memr[k] < Memr[kmin])
		    kmin = k
	    }
	    if (k != kmin) {
	        temp = Memr[k]
	        Memr[k] = Memr[kmin]
	        Memr[kmin] = temp
	    }
	}
end


# RS_MAXSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector.

procedure rs_maxswr (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax
real	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memr[k] > Memr[kmax])
		    kmax = k
	    }
	    if (k != kmax) {
	        temp = Memr[k]
	        Memr[k] = Memr[kmax]
	        Memr[kmax] = temp
	    }
	}
end


# RS_MXMNSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector and the minimum element
# with that of the next to last vector.  The number of vectors must be greater
# than 1.

procedure rs_mxmnswr (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax, kmin
real	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    kmin = kmax
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memr[k] > Memr[kmax])
		    kmax = k
		else if (Memr[k] < Memr[kmin])
		    kmin = k
	    }
	    temp = Memr[k]
	    Memr[k] = Memr[kmax]
	    Memr[kmax] = temp
	    if (kmin == k) {
	        j = a[nvecs - 1] + i
	        temp = Memr[j]
		Memr[j] = Memr[kmax]
		Memr[kmax] = temp
	    } else {
	        j = a[nvecs - 1] + i
	        temp = Memr[j]
	        Memr[j] = Memr[kmin]
	        Memr[kmin] = temp
	    }
	}
end

