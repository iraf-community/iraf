include <imhdr.h>

define	VPTR		Memi[$1+$2-1]	# Array of axis vector pointers


# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension up to 7 (NOT necessarily IM_MAXDIM!).


procedure imjoins (inptr, nimages, out, joindim, outtype)
pointer	inptr[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
int	joindim			# Dimension along which to join images
int	outtype			# Output datatype

pointer	in, inbuf, outbuf, sp, vin
int	stat, image, cum_len, line, band, dim4, dim5, dim6, dim7
long	vout[IM_MAXDIM]

pointer	imgnls()
pointer	impnls()

begin
	call smark (sp)
	call salloc (vin, nimages, TY_INT)

	call amovkl (long(1), vout, IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along specified dimension.  Joins along columns
	# and lines require processing in special order, all others in the
	# same order.  In the first two cases we process all input images
	# in inner loops, so we have to keep all those imdescriptors open.

	switch (joindim) {
	case 1:						# join columns
	    do band = 1, IM_LEN(out,3)
		do line = 1, IM_LEN(out,2) {
		    stat = impnls (out, outbuf, vout)
		    cum_len = 0
		    do image = 1, nimages {
			in = inptr[image]
			stat = imgnls (in, inbuf, Meml[VPTR(vin,image)])
			call amovs (Mems[inbuf], Mems[outbuf+cum_len],
			    IM_LEN(in,1))
			cum_len = cum_len + IM_LEN(in,1)
		    }
		}
	case 2:						# join lines
	    do band = 1, IM_LEN(out,3)
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnls (out, outbuf, vout)
			stat = imgnls (in, inbuf, Meml[VPTR(vin,image)])
			call amovs (Mems[inbuf], Mems[outbuf], IM_LEN(in,1))
		    }
		}
	case 3,4,5,6,7:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		do dim7 = 1, IM_LEN(in,7)
		    do dim6 = 1, IM_LEN(in,6)
			do dim5 = 1, IM_LEN(in,5)
			    do dim4 = 1, IM_LEN(in,4)
				do band = 1, IM_LEN(in,3)
				    do line = 1, IM_LEN(in,2) {
					stat = impnls (out, outbuf, vout)
					stat = imgnls (in, inbuf,
					    Meml[VPTR(vin,image)])
					call amovs (Mems[inbuf],
					    Mems[outbuf], IM_LEN(in,1))
				    }
		# Unmap last image to free resources.
		call imunmap (in)
	    }
	}

	call sfree (sp)
end


procedure imjoini (inptr, nimages, out, joindim, outtype)
pointer	inptr[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
int	joindim			# Dimension along which to join images
int	outtype			# Output datatype

pointer	in, inbuf, outbuf, sp, vin
int	stat, image, cum_len, line, band, dim4, dim5, dim6, dim7
long	vout[IM_MAXDIM]

pointer	imgnli()
pointer	impnli()

begin
	call smark (sp)
	call salloc (vin, nimages, TY_INT)

	call amovkl (long(1), vout, IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along specified dimension.  Joins along columns
	# and lines require processing in special order, all others in the
	# same order.  In the first two cases we process all input images
	# in inner loops, so we have to keep all those imdescriptors open.

	switch (joindim) {
	case 1:						# join columns
	    do band = 1, IM_LEN(out,3)
		do line = 1, IM_LEN(out,2) {
		    stat = impnli (out, outbuf, vout)
		    cum_len = 0
		    do image = 1, nimages {
			in = inptr[image]
			stat = imgnli (in, inbuf, Meml[VPTR(vin,image)])
			call amovi (Memi[inbuf], Memi[outbuf+cum_len],
			    IM_LEN(in,1))
			cum_len = cum_len + IM_LEN(in,1)
		    }
		}
	case 2:						# join lines
	    do band = 1, IM_LEN(out,3)
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnli (out, outbuf, vout)
			stat = imgnli (in, inbuf, Meml[VPTR(vin,image)])
			call amovi (Memi[inbuf], Memi[outbuf], IM_LEN(in,1))
		    }
		}
	case 3,4,5,6,7:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		do dim7 = 1, IM_LEN(in,7)
		    do dim6 = 1, IM_LEN(in,6)
			do dim5 = 1, IM_LEN(in,5)
			    do dim4 = 1, IM_LEN(in,4)
				do band = 1, IM_LEN(in,3)
				    do line = 1, IM_LEN(in,2) {
					stat = impnli (out, outbuf, vout)
					stat = imgnli (in, inbuf,
					    Meml[VPTR(vin,image)])
					call amovi (Memi[inbuf],
					    Memi[outbuf], IM_LEN(in,1))
				    }
		# Unmap last image to free resources.
		call imunmap (in)
	    }
	}

	call sfree (sp)
end


procedure imjoinl (inptr, nimages, out, joindim, outtype)
pointer	inptr[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
int	joindim			# Dimension along which to join images
int	outtype			# Output datatype

pointer	in, inbuf, outbuf, sp, vin
int	stat, image, cum_len, line, band, dim4, dim5, dim6, dim7
long	vout[IM_MAXDIM]

pointer	imgnll()
pointer	impnll()

begin
	call smark (sp)
	call salloc (vin, nimages, TY_INT)

	call amovkl (long(1), vout, IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along specified dimension.  Joins along columns
	# and lines require processing in special order, all others in the
	# same order.  In the first two cases we process all input images
	# in inner loops, so we have to keep all those imdescriptors open.

	switch (joindim) {
	case 1:						# join columns
	    do band = 1, IM_LEN(out,3)
		do line = 1, IM_LEN(out,2) {
		    stat = impnll (out, outbuf, vout)
		    cum_len = 0
		    do image = 1, nimages {
			in = inptr[image]
			stat = imgnll (in, inbuf, Meml[VPTR(vin,image)])
			call amovl (Meml[inbuf], Meml[outbuf+cum_len],
			    IM_LEN(in,1))
			cum_len = cum_len + IM_LEN(in,1)
		    }
		}
	case 2:						# join lines
	    do band = 1, IM_LEN(out,3)
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnll (out, outbuf, vout)
			stat = imgnll (in, inbuf, Meml[VPTR(vin,image)])
			call amovl (Meml[inbuf], Meml[outbuf], IM_LEN(in,1))
		    }
		}
	case 3,4,5,6,7:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		do dim7 = 1, IM_LEN(in,7)
		    do dim6 = 1, IM_LEN(in,6)
			do dim5 = 1, IM_LEN(in,5)
			    do dim4 = 1, IM_LEN(in,4)
				do band = 1, IM_LEN(in,3)
				    do line = 1, IM_LEN(in,2) {
					stat = impnll (out, outbuf, vout)
					stat = imgnll (in, inbuf,
					    Meml[VPTR(vin,image)])
					call amovl (Meml[inbuf],
					    Meml[outbuf], IM_LEN(in,1))
				    }
		# Unmap last image to free resources.
		call imunmap (in)
	    }
	}

	call sfree (sp)
end


procedure imjoinr (inptr, nimages, out, joindim, outtype)
pointer	inptr[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
int	joindim			# Dimension along which to join images
int	outtype			# Output datatype

pointer	in, inbuf, outbuf, sp, vin
int	stat, image, cum_len, line, band, dim4, dim5, dim6, dim7
long	vout[IM_MAXDIM]

pointer	imgnlr()
pointer	impnlr()

begin
	call smark (sp)
	call salloc (vin, nimages, TY_INT)

	call amovkl (long(1), vout, IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along specified dimension.  Joins along columns
	# and lines require processing in special order, all others in the
	# same order.  In the first two cases we process all input images
	# in inner loops, so we have to keep all those imdescriptors open.

	switch (joindim) {
	case 1:						# join columns
	    do band = 1, IM_LEN(out,3)
		do line = 1, IM_LEN(out,2) {
		    stat = impnlr (out, outbuf, vout)
		    cum_len = 0
		    do image = 1, nimages {
			in = inptr[image]
			stat = imgnlr (in, inbuf, Meml[VPTR(vin,image)])
			call amovr (Memr[inbuf], Memr[outbuf+cum_len],
			    IM_LEN(in,1))
			cum_len = cum_len + IM_LEN(in,1)
		    }
		}
	case 2:						# join lines
	    do band = 1, IM_LEN(out,3)
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnlr (out, outbuf, vout)
			stat = imgnlr (in, inbuf, Meml[VPTR(vin,image)])
			call amovr (Memr[inbuf], Memr[outbuf], IM_LEN(in,1))
		    }
		}
	case 3,4,5,6,7:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		do dim7 = 1, IM_LEN(in,7)
		    do dim6 = 1, IM_LEN(in,6)
			do dim5 = 1, IM_LEN(in,5)
			    do dim4 = 1, IM_LEN(in,4)
				do band = 1, IM_LEN(in,3)
				    do line = 1, IM_LEN(in,2) {
					stat = impnlr (out, outbuf, vout)
					stat = imgnlr (in, inbuf,
					    Meml[VPTR(vin,image)])
					call amovr (Memr[inbuf],
					    Memr[outbuf], IM_LEN(in,1))
				    }
		# Unmap last image to free resources.
		call imunmap (in)
	    }
	}

	call sfree (sp)
end


procedure imjoind (inptr, nimages, out, joindim, outtype)
pointer	inptr[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
int	joindim			# Dimension along which to join images
int	outtype			# Output datatype

pointer	in, inbuf, outbuf, sp, vin
int	stat, image, cum_len, line, band, dim4, dim5, dim6, dim7
long	vout[IM_MAXDIM]

pointer	imgnld()
pointer	impnld()

begin
	call smark (sp)
	call salloc (vin, nimages, TY_INT)

	call amovkl (long(1), vout, IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along specified dimension.  Joins along columns
	# and lines require processing in special order, all others in the
	# same order.  In the first two cases we process all input images
	# in inner loops, so we have to keep all those imdescriptors open.

	switch (joindim) {
	case 1:						# join columns
	    do band = 1, IM_LEN(out,3)
		do line = 1, IM_LEN(out,2) {
		    stat = impnld (out, outbuf, vout)
		    cum_len = 0
		    do image = 1, nimages {
			in = inptr[image]
			stat = imgnld (in, inbuf, Meml[VPTR(vin,image)])
			call amovd (Memd[inbuf], Memd[outbuf+cum_len],
			    IM_LEN(in,1))
			cum_len = cum_len + IM_LEN(in,1)
		    }
		}
	case 2:						# join lines
	    do band = 1, IM_LEN(out,3)
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnld (out, outbuf, vout)
			stat = imgnld (in, inbuf, Meml[VPTR(vin,image)])
			call amovd (Memd[inbuf], Memd[outbuf], IM_LEN(in,1))
		    }
		}
	case 3,4,5,6,7:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		do dim7 = 1, IM_LEN(in,7)
		    do dim6 = 1, IM_LEN(in,6)
			do dim5 = 1, IM_LEN(in,5)
			    do dim4 = 1, IM_LEN(in,4)
				do band = 1, IM_LEN(in,3)
				    do line = 1, IM_LEN(in,2) {
					stat = impnld (out, outbuf, vout)
					stat = imgnld (in, inbuf,
					    Meml[VPTR(vin,image)])
					call amovd (Memd[inbuf],
					    Memd[outbuf], IM_LEN(in,1))
				    }
		# Unmap last image to free resources.
		call imunmap (in)
	    }
	}

	call sfree (sp)
end


procedure imjoinx (inptr, nimages, out, joindim, outtype)
pointer	inptr[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
int	joindim			# Dimension along which to join images
int	outtype			# Output datatype

pointer	in, inbuf, outbuf, sp, vin
int	stat, image, cum_len, line, band, dim4, dim5, dim6, dim7
long	vout[IM_MAXDIM]

pointer	imgnlx()
pointer	impnlx()

begin
	call smark (sp)
	call salloc (vin, nimages, TY_INT)

	call amovkl (long(1), vout, IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along specified dimension.  Joins along columns
	# and lines require processing in special order, all others in the
	# same order.  In the first two cases we process all input images
	# in inner loops, so we have to keep all those imdescriptors open.

	switch (joindim) {
	case 1:						# join columns
	    do band = 1, IM_LEN(out,3)
		do line = 1, IM_LEN(out,2) {
		    stat = impnlx (out, outbuf, vout)
		    cum_len = 0
		    do image = 1, nimages {
			in = inptr[image]
			stat = imgnlx (in, inbuf, Meml[VPTR(vin,image)])
			call amovx (Memx[inbuf], Memx[outbuf+cum_len],
			    IM_LEN(in,1))
			cum_len = cum_len + IM_LEN(in,1)
		    }
		}
	case 2:						# join lines
	    do band = 1, IM_LEN(out,3)
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnlx (out, outbuf, vout)
			stat = imgnlx (in, inbuf, Meml[VPTR(vin,image)])
			call amovx (Memx[inbuf], Memx[outbuf], IM_LEN(in,1))
		    }
		}
	case 3,4,5,6,7:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		do dim7 = 1, IM_LEN(in,7)
		    do dim6 = 1, IM_LEN(in,6)
			do dim5 = 1, IM_LEN(in,5)
			    do dim4 = 1, IM_LEN(in,4)
				do band = 1, IM_LEN(in,3)
				    do line = 1, IM_LEN(in,2) {
					stat = impnlx (out, outbuf, vout)
					stat = imgnlx (in, inbuf,
					    Meml[VPTR(vin,image)])
					call amovx (Memx[inbuf],
					    Memx[outbuf], IM_LEN(in,1))
				    }
		# Unmap last image to free resources.
		call imunmap (in)
	    }
	}

	call sfree (sp)
end


