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

long	lg_val
size_t	sz_val
int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

int	imgnls()
int	impnls()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = nimages
	call salloc (vin, sz_val, TY_INT)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)

	# Initialize the v vectors.
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, Meml[vout], sz_val)
	do image = 1, nimages {
	    sz_val = IM_MAXDIM
	    call salloc (VPTR(vin,image), sz_val, TY_LONG)
	    lg_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (lg_val, Meml[VPTR(vin,image)], sz_val)
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

long	lg_val
size_t	sz_val
int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

int	imgnli()
int	impnli()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = nimages
	call salloc (vin, sz_val, TY_INT)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)

	# Initialize the v vectors.
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, Meml[vout], sz_val)
	do image = 1, nimages {
	    sz_val = IM_MAXDIM
	    call salloc (VPTR(vin,image), sz_val, TY_LONG)
	    lg_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (lg_val, Meml[VPTR(vin,image)], sz_val)
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

long	lg_val
size_t	sz_val
int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

int	imgnll()
int	impnll()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = nimages
	call salloc (vin, sz_val, TY_INT)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)

	# Initialize the v vectors.
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, Meml[vout], sz_val)
	do image = 1, nimages {
	    sz_val = IM_MAXDIM
	    call salloc (VPTR(vin,image), sz_val, TY_LONG)
	    lg_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (lg_val, Meml[VPTR(vin,image)], sz_val)
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
		    sz_val = IM_LEN(in,1)
		    call amovl (Meml[inbuf], Meml[outbuf+cum_len], sz_val)
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
			sz_val = IM_LEN(in,1)
			call amovl (Meml[inbuf], Meml[outbuf], sz_val)
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
		    sz_val = IM_LEN(in,1)
		    call amovl (Meml[inbuf], Meml[outbuf], sz_val)
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

long	lg_val
size_t	sz_val
int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

int	imgnlr()
int	impnlr()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = nimages
	call salloc (vin, sz_val, TY_INT)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)

	# Initialize the v vectors.
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, Meml[vout], sz_val)
	do image = 1, nimages {
	    sz_val = IM_MAXDIM
	    call salloc (VPTR(vin,image), sz_val, TY_LONG)
	    lg_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (lg_val, Meml[VPTR(vin,image)], sz_val)
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

long	lg_val
size_t	sz_val
int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

int	imgnld()
int	impnld()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = nimages
	call salloc (vin, sz_val, TY_INT)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)

	# Initialize the v vectors.
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, Meml[vout], sz_val)
	do image = 1, nimages {
	    sz_val = IM_MAXDIM
	    call salloc (VPTR(vin,image), sz_val, TY_LONG)
	    lg_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (lg_val, Meml[VPTR(vin,image)], sz_val)
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

long	lg_val
size_t	sz_val
int	i, image, line, nlines, nbands, stat, cum_len
pointer	sp, vin, vout, in, inbuf, outbuf

int	imgnlx()
int	impnlx()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = nimages
	call salloc (vin, sz_val, TY_INT)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)

	# Initialize the v vectors.
	lg_val = 1
	sz_val = IM_MAXDIM
	call amovkl (lg_val, Meml[vout], sz_val)
	do image = 1, nimages {
	    sz_val = IM_MAXDIM
	    call salloc (VPTR(vin,image), sz_val, TY_LONG)
	    lg_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (lg_val, Meml[VPTR(vin,image)], sz_val)
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


