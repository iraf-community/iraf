# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMA_MAX -- Image arithmetic maximum value.


procedure ima_maxs (im_a, im_b, im_c, a, b)

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

	# If imagea is constant then read imageb and do the vector/scalar
	# maximum to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nls (im, buf, v, 2) != EOF)
		call amaxks (Mems[buf[2]], a, Mems[buf[1]], len)

	# If imageb is constant then read imagea and do the vector/scalar
	# maximum to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nls (im, buf, v, 2) != EOF)
		call amaxks (Mems[buf[2]], b, Mems[buf[1]], len)

	# Read imagea and imageb and do a vector-vector maximum
	# operation to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nls (im, buf, v, 3) != EOF)
		call amaxs (Mems[buf[2]], Mems[buf[3]], Mems[buf[1]], len)
	}
end

procedure ima_maxi (im_a, im_b, im_c, a, b)

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

	# If imagea is constant then read imageb and do the vector/scalar
	# maximum to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nli (im, buf, v, 2) != EOF)
		call amaxki (Memi[buf[2]], a, Memi[buf[1]], len)

	# If imageb is constant then read imagea and do the vector/scalar
	# maximum to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nli (im, buf, v, 2) != EOF)
		call amaxki (Memi[buf[2]], b, Memi[buf[1]], len)

	# Read imagea and imageb and do a vector-vector maximum
	# operation to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nli (im, buf, v, 3) != EOF)
		call amaxi (Memi[buf[2]], Memi[buf[3]], Memi[buf[1]], len)
	}
end

procedure ima_maxl (im_a, im_b, im_c, a, b)

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

	# If imagea is constant then read imageb and do the vector/scalar
	# maximum to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nll (im, buf, v, 2) != EOF)
		call amaxkl (Meml[buf[2]], a, Meml[buf[1]], len)

	# If imageb is constant then read imagea and do the vector/scalar
	# maximum to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nll (im, buf, v, 2) != EOF)
		call amaxkl (Meml[buf[2]], b, Meml[buf[1]], len)

	# Read imagea and imageb and do a vector-vector maximum
	# operation to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nll (im, buf, v, 3) != EOF)
		call amaxl (Meml[buf[2]], Meml[buf[3]], Meml[buf[1]], len)
	}
end

procedure ima_maxr (im_a, im_b, im_c, a, b)

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

	# If imagea is constant then read imageb and do the vector/scalar
	# maximum to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nlr (im, buf, v, 2) != EOF)
		call amaxkr (Memr[buf[2]], a, Memr[buf[1]], len)

	# If imageb is constant then read imagea and do the vector/scalar
	# maximum to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nlr (im, buf, v, 2) != EOF)
		call amaxkr (Memr[buf[2]], b, Memr[buf[1]], len)

	# Read imagea and imageb and do a vector-vector maximum
	# operation to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nlr (im, buf, v, 3) != EOF)
		call amaxr (Memr[buf[2]], Memr[buf[3]], Memr[buf[1]], len)
	}
end

procedure ima_maxd (im_a, im_b, im_c, a, b)

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

	# If imagea is constant then read imageb and do the vector/scalar
	# maximum to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nld (im, buf, v, 2) != EOF)
		call amaxkd (Memd[buf[2]], a, Memd[buf[1]], len)

	# If imageb is constant then read imagea and do the vector/scalar
	# maximum to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nld (im, buf, v, 2) != EOF)
		call amaxkd (Memd[buf[2]], b, Memd[buf[1]], len)

	# Read imagea and imageb and do a vector-vector maximum
	# operation to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nld (im, buf, v, 3) != EOF)
		call amaxd (Memd[buf[2]], Memd[buf[3]], Memd[buf[1]], len)
	}
end

