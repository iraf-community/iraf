include <imhdr.h>

define	VPTR		Memi[$1+$2-1]	# Array of axis vector pointers



# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension.

procedure imjoins (inptr, nimages, out, joindim, outtype)

pointer	inptr[nimages]		#I Input IMIO pointers
int	nimages			#I Number of input images
pointer	out			#I Output IMIO pointer
int	joindim			#I Dimension along which to join images
int	outtype			#I Output datatype

int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

pointer	imgnls()
pointer	impnls()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (vin, nimages, TY_INT)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	# Initialize the v vectors.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along the specified dimension. Joins along
	# columns and lines require processing in special order, all others
	# in the same order.  In the first two cases we process all input
	# images in inner loops, so we have to keep all those image
	# descriptors open.

	switch (joindim) {
	case 1:						# join columns
	    nlines = 1
	    do i = 2, IM_NDIM(out)
		nlines = nlines * IM_LEN(out,i)
	    do i = 1, nlines {
		stat = impnls (out, outbuf, Meml[vout])
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
	    nbands = 1
	    do i = 3, IM_NDIM(out)
		nbands = nbands * IM_LEN(out,i)
	    do i = 1, nbands {
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnls (out, outbuf, Meml[vout])
			stat = imgnls (in, inbuf, Meml[VPTR(vin,image)])
			call amovs (Mems[inbuf], Mems[outbuf], IM_LEN(in,1))
		    }
		}
	    }

	default:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		nlines = 1
		do i = 2, IM_NDIM(in)
		    nlines = nlines * IM_LEN(in,i)
		do i = 1, nlines {
		    stat = impnls (out, outbuf, Meml[vout])
		    stat = imgnls (in, inbuf, Meml[VPTR(vin,image)])
		    call amovs (Mems[inbuf], Mems[outbuf], IM_LEN(in,1))
		}
	    }
	}

	call sfree (sp)
end



# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension.

procedure imjoini (inptr, nimages, out, joindim, outtype)

pointer	inptr[nimages]		#I Input IMIO pointers
int	nimages			#I Number of input images
pointer	out			#I Output IMIO pointer
int	joindim			#I Dimension along which to join images
int	outtype			#I Output datatype

int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

pointer	imgnli()
pointer	impnli()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (vin, nimages, TY_INT)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	# Initialize the v vectors.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along the specified dimension. Joins along
	# columns and lines require processing in special order, all others
	# in the same order.  In the first two cases we process all input
	# images in inner loops, so we have to keep all those image
	# descriptors open.

	switch (joindim) {
	case 1:						# join columns
	    nlines = 1
	    do i = 2, IM_NDIM(out)
		nlines = nlines * IM_LEN(out,i)
	    do i = 1, nlines {
		stat = impnli (out, outbuf, Meml[vout])
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
	    nbands = 1
	    do i = 3, IM_NDIM(out)
		nbands = nbands * IM_LEN(out,i)
	    do i = 1, nbands {
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnli (out, outbuf, Meml[vout])
			stat = imgnli (in, inbuf, Meml[VPTR(vin,image)])
			call amovi (Memi[inbuf], Memi[outbuf], IM_LEN(in,1))
		    }
		}
	    }

	default:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		nlines = 1
		do i = 2, IM_NDIM(in)
		    nlines = nlines * IM_LEN(in,i)
		do i = 1, nlines {
		    stat = impnli (out, outbuf, Meml[vout])
		    stat = imgnli (in, inbuf, Meml[VPTR(vin,image)])
		    call amovi (Memi[inbuf], Memi[outbuf], IM_LEN(in,1))
		}
	    }
	}

	call sfree (sp)
end



# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension.

procedure imjoinl (inptr, nimages, out, joindim, outtype)

pointer	inptr[nimages]		#I Input IMIO pointers
int	nimages			#I Number of input images
pointer	out			#I Output IMIO pointer
int	joindim			#I Dimension along which to join images
int	outtype			#I Output datatype

int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

pointer	imgnll()
pointer	impnll()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (vin, nimages, TY_INT)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	# Initialize the v vectors.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along the specified dimension. Joins along
	# columns and lines require processing in special order, all others
	# in the same order.  In the first two cases we process all input
	# images in inner loops, so we have to keep all those image
	# descriptors open.

	switch (joindim) {
	case 1:						# join columns
	    nlines = 1
	    do i = 2, IM_NDIM(out)
		nlines = nlines * IM_LEN(out,i)
	    do i = 1, nlines {
		stat = impnll (out, outbuf, Meml[vout])
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
	    nbands = 1
	    do i = 3, IM_NDIM(out)
		nbands = nbands * IM_LEN(out,i)
	    do i = 1, nbands {
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnll (out, outbuf, Meml[vout])
			stat = imgnll (in, inbuf, Meml[VPTR(vin,image)])
			call amovl (Meml[inbuf], Meml[outbuf], IM_LEN(in,1))
		    }
		}
	    }

	default:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		nlines = 1
		do i = 2, IM_NDIM(in)
		    nlines = nlines * IM_LEN(in,i)
		do i = 1, nlines {
		    stat = impnll (out, outbuf, Meml[vout])
		    stat = imgnll (in, inbuf, Meml[VPTR(vin,image)])
		    call amovl (Meml[inbuf], Meml[outbuf], IM_LEN(in,1))
		}
	    }
	}

	call sfree (sp)
end



# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension.

procedure imjoinr (inptr, nimages, out, joindim, outtype)

pointer	inptr[nimages]		#I Input IMIO pointers
int	nimages			#I Number of input images
pointer	out			#I Output IMIO pointer
int	joindim			#I Dimension along which to join images
int	outtype			#I Output datatype

int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

pointer	imgnlr()
pointer	impnlr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (vin, nimages, TY_INT)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	# Initialize the v vectors.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along the specified dimension. Joins along
	# columns and lines require processing in special order, all others
	# in the same order.  In the first two cases we process all input
	# images in inner loops, so we have to keep all those image
	# descriptors open.

	switch (joindim) {
	case 1:						# join columns
	    nlines = 1
	    do i = 2, IM_NDIM(out)
		nlines = nlines * IM_LEN(out,i)
	    do i = 1, nlines {
		stat = impnlr (out, outbuf, Meml[vout])
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
	    nbands = 1
	    do i = 3, IM_NDIM(out)
		nbands = nbands * IM_LEN(out,i)
	    do i = 1, nbands {
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnlr (out, outbuf, Meml[vout])
			stat = imgnlr (in, inbuf, Meml[VPTR(vin,image)])
			call amovr (Memr[inbuf], Memr[outbuf], IM_LEN(in,1))
		    }
		}
	    }

	default:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		nlines = 1
		do i = 2, IM_NDIM(in)
		    nlines = nlines * IM_LEN(in,i)
		do i = 1, nlines {
		    stat = impnlr (out, outbuf, Meml[vout])
		    stat = imgnlr (in, inbuf, Meml[VPTR(vin,image)])
		    call amovr (Memr[inbuf], Memr[outbuf], IM_LEN(in,1))
		}
	    }
	}

	call sfree (sp)
end



# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension.

procedure imjoind (inptr, nimages, out, joindim, outtype)

pointer	inptr[nimages]		#I Input IMIO pointers
int	nimages			#I Number of input images
pointer	out			#I Output IMIO pointer
int	joindim			#I Dimension along which to join images
int	outtype			#I Output datatype

int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

pointer	imgnld()
pointer	impnld()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (vin, nimages, TY_INT)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	# Initialize the v vectors.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along the specified dimension. Joins along
	# columns and lines require processing in special order, all others
	# in the same order.  In the first two cases we process all input
	# images in inner loops, so we have to keep all those image
	# descriptors open.

	switch (joindim) {
	case 1:						# join columns
	    nlines = 1
	    do i = 2, IM_NDIM(out)
		nlines = nlines * IM_LEN(out,i)
	    do i = 1, nlines {
		stat = impnld (out, outbuf, Meml[vout])
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
	    nbands = 1
	    do i = 3, IM_NDIM(out)
		nbands = nbands * IM_LEN(out,i)
	    do i = 1, nbands {
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnld (out, outbuf, Meml[vout])
			stat = imgnld (in, inbuf, Meml[VPTR(vin,image)])
			call amovd (Memd[inbuf], Memd[outbuf], IM_LEN(in,1))
		    }
		}
	    }

	default:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		nlines = 1
		do i = 2, IM_NDIM(in)
		    nlines = nlines * IM_LEN(in,i)
		do i = 1, nlines {
		    stat = impnld (out, outbuf, Meml[vout])
		    stat = imgnld (in, inbuf, Meml[VPTR(vin,image)])
		    call amovd (Memd[inbuf], Memd[outbuf], IM_LEN(in,1))
		}
	    }
	}

	call sfree (sp)
end



# IMJOIN -- Join the set of input images into an output image along the
# specified axis, any dimension.

procedure imjoinx (inptr, nimages, out, joindim, outtype)

pointer	inptr[nimages]		#I Input IMIO pointers
int	nimages			#I Number of input images
pointer	out			#I Output IMIO pointer
int	joindim			#I Dimension along which to join images
int	outtype			#I Output datatype

int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

pointer	imgnlx()
pointer	impnlx()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (vin, nimages, TY_INT)
	call salloc (vout, IM_MAXDIM, TY_LONG)

	# Initialize the v vectors.
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	do image = 1, nimages {
	    call salloc (VPTR(vin,image), IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[VPTR(vin,image)], IM_MAXDIM)
	}

	# Join input images along the specified dimension. Joins along
	# columns and lines require processing in special order, all others
	# in the same order.  In the first two cases we process all input
	# images in inner loops, so we have to keep all those image
	# descriptors open.

	switch (joindim) {
	case 1:						# join columns
	    nlines = 1
	    do i = 2, IM_NDIM(out)
		nlines = nlines * IM_LEN(out,i)
	    do i = 1, nlines {
		stat = impnlx (out, outbuf, Meml[vout])
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
	    nbands = 1
	    do i = 3, IM_NDIM(out)
		nbands = nbands * IM_LEN(out,i)
	    do i = 1, nbands {
		do image = 1, nimages {
		    in = inptr[image]
		    do line = 1, IM_LEN(in,2) {
			stat = impnlx (out, outbuf, Meml[vout])
			stat = imgnlx (in, inbuf, Meml[VPTR(vin,image)])
			call amovx (Memx[inbuf], Memx[outbuf], IM_LEN(in,1))
		    }
		}
	    }

	default:					# join bands or higher
	    do image = 1, nimages {
		in = inptr[image]
		nlines = 1
		do i = 2, IM_NDIM(in)
		    nlines = nlines * IM_LEN(in,i)
		do i = 1, nlines {
		    stat = impnlx (out, outbuf, Meml[vout])
		    stat = imgnlx (in, inbuf, Meml[VPTR(vin,image)])
		    call amovx (Memx[inbuf], Memx[outbuf], IM_LEN(in,1))
		}
	    }
	}

	call sfree (sp)
end


