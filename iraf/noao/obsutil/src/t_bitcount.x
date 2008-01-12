include	<error.h>
include	<imhdr.h>
include	<mach.h>

# T_BITCOUNT -- The pixel bits for each image are counted and reported.

procedure t_bitcount ()

pointer	imname, imlist, im, typename, sp
bool	grand, leftzeroes, verbose, clear_counters, first_time, pixtype_err
int	zeroes[NBITS_INT], ones[NBITS_INT], zeroes_tot, ones_tot, ntotal
int	nimages, npixels, maxbit, bit, nim, pixtype

pointer	imtopenp(), immap()
int	imtgetim(), imtlen(), bitcount()
bool	clgetb()

errchk	immap

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (typename, SZ_FNAME, TY_CHAR)

	imlist = imtopenp ("images")
	nimages = imtlen (imlist)
	if (nimages <= 0) {
	    call sfree (sp)
	    call error (0, "no images in list")
	}

	grand = clgetb ("grandtotal")
	leftzeroes = clgetb ("leftzeroes")
	verbose = clgetb ("verbose")

	if (grand && verbose)
	    call printf ("\nGrand totals for:\n\n")

	clear_counters = true
	pixtype_err = false
	first_time = true

	for (nim=1; imtgetim (imlist, Memc[imname], SZ_FNAME) != EOF; nim=nim+1)
	    iferr (im = immap (Memc[imname], READ_ONLY, 0)) {
		call erract (EA_WARN)
	    } else {
		if (first_time) {
		    pixtype = IM_PIXTYPE(im)
		    first_time = false
		} else if (IM_PIXTYPE(im) != pixtype) {
		    pixtype_err = true
		}

		if (clear_counters) {
		    call aclri (zeroes, NBITS_INT)
		    call aclri (ones, NBITS_INT)
		    zeroes_tot = 0
		    ones_tot = 0
		    ntotal = 0
		    clear_counters = ! grand
		}

		npixels = bitcount (im, leftzeroes, zeroes, ones, maxbit)

		ntotal = ntotal + npixels

		if (verbose) {
		    call dtstring (IM_PIXTYPE(im), Memc[typename], SZ_FNAME)

		    call printf ("%s[%d,%d][%s], npix=%d:\n")
			call pargstr (Memc[imname])
			call pargi (IM_LEN(im,1))
			call pargi (IM_LEN(im,2))
			call pargstr (Memc[typename])
			call pargi (npixels)
		}

		if (grand && nim != nimages)
		    next

		if (verbose)
		    call printf ("\n")

		if (grand && pixtype_err) {
		    call eprintf ("Warning: image data types vary!\n\n")
		    call flush (STDOUT)
		    call flush (STDERR)
		}

		if (verbose) {
		    do bit = 1, maxbit {
			zeroes_tot = zeroes_tot + zeroes[bit]
			ones_tot = ones_tot + ones[bit]
		    }

		    call printf ("  bit     0's       1's     %%0's    %%1's\n")
		    call printf ("  ---     ---       ---     ----    ----\n")

		    do bit = 1, maxbit {
			call printf ("  %2d  %8d  %8d   %5.1f   %5.1f")
			    call pargi (bit-1)
			    call pargi (zeroes[bit])
			    call pargi (ones[bit])
			    call pargr (100.*zeroes[bit]/ntotal)
			    call pargr (100.*ones[bit]/ntotal)

			if (bit==maxbit && IM_PIXTYPE(im)!=TY_USHORT)
			    call printf ("  (sign bit)\n")
			else
			    call printf ("\n")
		    }

		    call printf ("       -------   -------    ----    ----\n")
		    call printf ("    %10d%10d   %5.1f   %5.1f\n\n\n")
			call pargi (zeroes_tot)
			call pargi (ones_tot)
			call pargr (100.*zeroes_tot/(ntotal*maxbit))
			call pargr (100.*ones_tot/(ntotal*maxbit))

		} else
		    do bit = 1, maxbit {
			call printf ("%d\t%d\n")
			    call pargi (zeroes[bit])
			    call pargi (ones[bit])
		    }

		call imunmap (im)
		call flush (STDOUT)
	    }

	call imtclose (imlist)
	call sfree (sp)
end


# BITCOUNT -- Accumulate the bit statistics for an image and return
# the number of pixels.  The calling routine is responsible for
# zeroing the arrays.

int procedure bitcount (im, leftzeroes, zeroes, ones, maxbit)

pointer	im			#I image descriptor
bool	leftzeroes		#I are leftmost zeroes significant?
int	zeroes[ARB]		#O array to count zeroes / bit
int	ones[ARB]		#O array to count ones / bit
int	maxbit			#O number of bits/pixel

int	bit, npixels, nrows, ncols, ival, i
long	v[IM_MAXDIM]
pointer	buf

int	imgnli()

errchk	imgnli

begin
	ncols = IM_LEN(im,1)
	nrows = IM_LEN(im,2)
	npixels = nrows * ncols

	# this will break on machines with 64 bit integers
	switch (IM_PIXTYPE(im)) {
	case TY_SHORT, TY_USHORT:
	    maxbit = NBITS_SHORT
	case TY_INT, TY_LONG:
	    maxbit = NBITS_INT
	default:
	    call error (0, "image pixels aren't integers")
	}

	call amovkl (long(1), v, IM_MAXDIM)

	while (imgnli (im, buf, v) != EOF)
	    do i = 1, ncols {
		ival = Memi[buf+i-1]

		# special handling for the high order bit
		if (ival < 0) {
		    # convert to 2's complement and tally the sign bit
		    ival = 2**maxbit - abs(ival)
		    ones[maxbit] = ones[maxbit] + 1
		} else if ((IM_PIXTYPE(im) == TY_USHORT) && (ival > 32767))
		    ones[maxbit] = ones[maxbit] + 1
		else if (leftzeroes)
		    zeroes[maxbit] = zeroes[maxbit] + 1

		for (bit=1; bit<maxbit && ival!=0; bit=bit+1) {
		    if (mod (ival, 2) == 0)
			zeroes[bit] = zeroes[bit] + 1
		    else
			ones[bit] = ones[bit] + 1

		    ival = ival / 2
		}

		if (leftzeroes)
		    do bit = bit, maxbit-1
			zeroes[bit] = zeroes[bit] + 1
	    }

	return (npixels)
end
