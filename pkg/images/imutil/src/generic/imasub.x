# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMA_SUB -- Image arithmetic subtraction.


procedure ima_subs (im_a, im_b, im_c, a, b)

pointer	im_a, im_b, im_c
short	a, b

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nls()

begin
	# Loop through all of the image lines.
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb.  Do a vector/scalar
	# subtraction and then negate the result.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nls (im, buf, v, 2) != EOF) {
		if (a != 0) {
		    call asubks (Mems[buf[2]], a, Mems[buf[1]], len)
		    call anegs (Mems[buf[1]], Mems[buf[1]], len)
		} else
		    call anegs (Mems[buf[2]], Mems[buf[1]], len)
	    }

	# If imageb is constant then read imagea and do a vector/scalar
	# subtraction to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nls (im, buf, v, 2) != EOF) {
		if (b == 0)
		    call amovs (Mems[buf[2]], Mems[buf[1]], len)
		else
		    call asubks (Mems[buf[2]], b, Mems[buf[1]], len)
	    }

	# Read imagea and imageb and do a vector subtraction into imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nls (im, buf, v, 3) != EOF)
		call asubs (Mems[buf[2]], Mems[buf[3]], Mems[buf[1]], len)
	}
end

procedure ima_subi (im_a, im_b, im_c, a, b)

pointer	im_a, im_b, im_c
int	a, b

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nli()

begin
	# Loop through all of the image lines.
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb.  Do a vector/scalar
	# subtraction and then negate the result.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nli (im, buf, v, 2) != EOF) {
		if (a != 0) {
		    call asubki (Memi[buf[2]], a, Memi[buf[1]], len)
		    call anegi (Memi[buf[1]], Memi[buf[1]], len)
		} else
		    call anegi (Memi[buf[2]], Memi[buf[1]], len)
	    }

	# If imageb is constant then read imagea and do a vector/scalar
	# subtraction to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nli (im, buf, v, 2) != EOF) {
		if (b == 0)
		    call amovi (Memi[buf[2]], Memi[buf[1]], len)
		else
		    call asubki (Memi[buf[2]], b, Memi[buf[1]], len)
	    }

	# Read imagea and imageb and do a vector subtraction into imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nli (im, buf, v, 3) != EOF)
		call asubi (Memi[buf[2]], Memi[buf[3]], Memi[buf[1]], len)
	}
end

procedure ima_subl (im_a, im_b, im_c, a, b)

pointer	im_a, im_b, im_c
long	a, b

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nll()

begin
	# Loop through all of the image lines.
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb.  Do a vector/scalar
	# subtraction and then negate the result.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nll (im, buf, v, 2) != EOF) {
		if (a != 0) {
		    call asubkl (Meml[buf[2]], a, Meml[buf[1]], len)
		    call anegl (Meml[buf[1]], Meml[buf[1]], len)
		} else
		    call anegl (Meml[buf[2]], Meml[buf[1]], len)
	    }

	# If imageb is constant then read imagea and do a vector/scalar
	# subtraction to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nll (im, buf, v, 2) != EOF) {
		if (b == 0)
		    call amovl (Meml[buf[2]], Meml[buf[1]], len)
		else
		    call asubkl (Meml[buf[2]], b, Meml[buf[1]], len)
	    }

	# Read imagea and imageb and do a vector subtraction into imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nll (im, buf, v, 3) != EOF)
		call asubl (Meml[buf[2]], Meml[buf[3]], Meml[buf[1]], len)
	}
end

procedure ima_subr (im_a, im_b, im_c, a, b)

pointer	im_a, im_b, im_c
real	a, b

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nlr()

begin
	# Loop through all of the image lines.
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb.  Do a vector/scalar
	# subtraction and then negate the result.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nlr (im, buf, v, 2) != EOF) {
		if (a != 0.0) {
		    call asubkr (Memr[buf[2]], a, Memr[buf[1]], len)
		    call anegr (Memr[buf[1]], Memr[buf[1]], len)
		} else
		    call anegr (Memr[buf[2]], Memr[buf[1]], len)
	    }

	# If imageb is constant then read imagea and do a vector/scalar
	# subtraction to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nlr (im, buf, v, 2) != EOF) {
		if (b == 0.0)
		    call amovr (Memr[buf[2]], Memr[buf[1]], len)
		else
		    call asubkr (Memr[buf[2]], b, Memr[buf[1]], len)
	    }

	# Read imagea and imageb and do a vector subtraction into imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nlr (im, buf, v, 3) != EOF)
		call asubr (Memr[buf[2]], Memr[buf[3]], Memr[buf[1]], len)
	}
end

procedure ima_subd (im_a, im_b, im_c, a, b)

pointer	im_a, im_b, im_c
double	a, b

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nld()

begin
	# Loop through all of the image lines.
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb.  Do a vector/scalar
	# subtraction and then negate the result.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nld (im, buf, v, 2) != EOF) {
		if (a != 0.0D0) {
		    call asubkd (Memd[buf[2]], a, Memd[buf[1]], len)
		    call anegd (Memd[buf[1]], Memd[buf[1]], len)
		} else
		    call anegd (Memd[buf[2]], Memd[buf[1]], len)
	    }

	# If imageb is constant then read imagea and do a vector/scalar
	# subtraction to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nld (im, buf, v, 2) != EOF) {
		if (b == 0.0D0)
		    call amovd (Memd[buf[2]], Memd[buf[1]], len)
		else
		    call asubkd (Memd[buf[2]], b, Memd[buf[1]], len)
	    }

	# Read imagea and imageb and do a vector subtraction into imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nld (im, buf, v, 3) != EOF)
		call asubd (Memd[buf[2]], Memd[buf[3]], Memd[buf[1]], len)
	}
end

