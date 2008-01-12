include	<error.h>
include	<imhdr.h>

# T_CRGROW -- Grow cosmic ray mask identifications.

procedure t_crgrow ()

int	input				# Input masks
int	output				# Output masks
real	radius				# Radius
int	inval				# Input mask value to grow
int	outval				# Output grown mask value

pointer	sp, inmask, outmask, temp1, temp2
pointer	im, ptr

int	imtopenp(), imtlen(), imtgetim()
bool	strne()
int	clgeti()
real	clgetr()
pointer	immap()
errchk	immap, crgrow

begin
	call smark (sp)
	call salloc (inmask, SZ_FNAME, TY_CHAR)
	call salloc (outmask, SZ_FNAME, TY_CHAR)
	call salloc (temp1, SZ_FNAME, TY_CHAR)
	call salloc (temp2, SZ_FNAME, TY_CHAR)

	# Task parameters.
	input = imtopenp ("input")
	output = imtopenp ("output")
	radius = max (0., clgetr ("radius"))
	inval = clgeti ("inval")
	outval = clgeti ("outval")

	if (imtlen (output) != imtlen (input))
	    call error (1, "Input and output lists do not match")

	# Grow the cosmic ray masks.
	while (imtgetim (input, Memc[inmask], SZ_FNAME) != EOF) {
	    call strcpy (Memc[inmask], Memc[outmask], SZ_FNAME)
	    if (imtgetim (output, Memc[outmask], SZ_FNAME) == EOF)
		call error (1, "Output list ended prematurely")
	    if (strne (Memc[inmask], Memc[outmask])) {
		call imgcluster (Memc[inmask], Memc[temp1], SZ_FNAME)
		call imgcluster (Memc[outmask], Memc[temp2], SZ_FNAME)
		iferr (call imcopy (Memc[temp1], Memc[temp2])) {
		    call erract (EA_WARN)
		    next
		}
		im = immap (Memc[inmask], READ_ONLY, TY_CHAR)
		iferr (call imgstr (im, "extname", Memc[temp1], SZ_FNAME))
		    call strcpy ("pl", Memc[temp1], SZ_FNAME)
		call imunmap (im)
	    }
	    call xt_maskname (Memc[outmask], Memc[temp1], 0, Memc[outmask],
		SZ_FNAME)

	    if (radius < 1.)
		next

	    iferr {
		im = NULL
	        ptr = immap (Memc[outmask], READ_WRITE, 0); im = ptr
		call crgrow (im, radius, inval, outval)
	    } then {
		call erract (EA_WARN)
		if (strne (Memc[inmask], Memc[outmask])) {
		    if (im != NULL) {
			call imunmap (im)
			iferr (call imdelete (Memc[outmask]))
			    call erract (EA_WARN)
		    }
		}
	    }

	    if (im != NULL)
		call imunmap (im)
	}

	call imtclose (output)
	call imtclose (input)
	call sfree (sp)
end


# CRGROW -- Grow cosmic rays.

procedure crgrow (im, grow, inval, outval)

pointer	im			# Mask pointer (Read/Write)
real	grow			# Radius (pixels)
int	inval			# Input mask value for pixels to grow
int	outval			# Output mask value for grown pixels

int	i, j, k, l, nc, nl, ngrow, nbufs, val1, val2
long	v1[2], v2[2]
real	grow2, y2
pointer	, buf, buf1, buf2, ptr

int	imgnli(), impnli()
errchk	calloc, imgnli, impnli

begin
	if (grow < 1. || inval == 0)
	    return

	grow2 = grow * grow
	ngrow = int (grow)
	buf = NULL

	iferr {
	    if (IM_NDIM(im) > 2)
		call error (1,
		    "Only one or two dimensional masks are allowed")

	    nc = IM_LEN(im, 1)
	    if (IM_NDIM(im) > 1)
		nl = IM_LEN(im,2)
	    else
		nl = 1

	    # Initialize buffering.
	    nbufs = min (1 + 2 * ngrow, nl)
	    call calloc (buf, nc*nbufs, TY_INT)

	    call amovkl (long(1), v1, IM_NDIM(im))
	    call amovkl (long(1), v2, IM_NDIM(im))
	    while (imgnli (im, buf1, v1) != EOF) {
		j = v1[2] - 1
		buf2 = buf + mod (j, nbufs) * nc
		do i = 1, nc {
		    val1 = Memi[buf1]
		    val2 = Memi[buf2]
		    if ((IS_INDEFI(inval) && val1 != 0) || val1 == inval) {
			do k = max(1,j-ngrow), min (nl,j+ngrow) {
			    ptr = buf + mod (k, nbufs) * nc - 1
			    y2 = (k - j) ** 2
			    do l = max(1,i-ngrow), min (nc,i+ngrow) {
				if ((l-i)**2 + y2 > grow2)
				    next
				Memi[ptr+l] = -val1
			    }
			}
		    } else {
			if (val2 >= 0)
			    Memi[buf2] = val1
		    }
		    buf1 = buf1 + 1
		    buf2 = buf2 + 1
		}

		if (j > ngrow) {
		    while (impnli (im, buf2, v2) != EOF) {
			k = v2[2] - 1
			buf1 = buf + mod (k, nbufs) * nc
			do i = 1, nc {
			    val1 = Memi[buf1]
			    if (val1 < 0) {
				if (IS_INDEFI(outval))
				    Memi[buf2] = -val1
				else 
				    Memi[buf2] = outval
			    } else
				Memi[buf2] = val1
			    Memi[buf1] = 0.
			    buf1 = buf1 + 1
			    buf2 = buf2 + 1
			}
			if (j != nl)
			    break
		    }
		}
	    }
	} then
	    call erract (EA_ERROR)

	if (buf != NULL)
	    call mfree (buf, TY_INT)
end
