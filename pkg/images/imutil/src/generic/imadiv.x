# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMA_DIV -- Image arithmetic division.


procedure ima_divs (im_a, im_b, im_c, a, b, c)

pointer	im_a, im_b, im_c
short	a, b, c

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nls()
short	ima_efncs()
extern	ima_efncs

short	divzero
common	/imadcoms/ divzero

begin
	# Loop through all of the image lines.
	divzero = c
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb and do a vector
	# reciprical to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nls (im, buf, v, 2) != EOF)
		call arczs (a, Mems[buf[2]], Mems[buf[1]], len,
		    ima_efncs)

	# If imageb is constant then read imagea.  If the constant
	# is 1 do a vector move to imagec otherwise do a vector/scalar
	# divide to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nls (im, buf, v, 2) != EOF) {
		if (b == 0)
		    call amovks (divzero, Mems[buf[1]], len)
		else if (b == 1)
		    call amovs (Mems[buf[2]], Mems[buf[1]], len)
		else
		    call adivks (Mems[buf[2]], b, Mems[buf[1]], len)
	    }

	# Read imagea and imageb and do the vector divide to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nls (im, buf, v, 3) != EOF)
		call advzs (Mems[buf[2]], Mems[buf[3]], Mems[buf[1]],
		    len, ima_efncs)
	}
end


# IMA_EFNC -- Error function for division by zero.

short procedure ima_efncs (a)

short	a
short	divzero
common	/imadcoms/ divzero

begin
	return (divzero)
end

procedure ima_divi (im_a, im_b, im_c, a, b, c)

pointer	im_a, im_b, im_c
int	a, b, c

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nli()
int	ima_efnci()
extern	ima_efnci

int	divzero
common	/imadcomi/ divzero

begin
	# Loop through all of the image lines.
	divzero = c
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb and do a vector
	# reciprical to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nli (im, buf, v, 2) != EOF)
		call arczi (a, Memi[buf[2]], Memi[buf[1]], len,
		    ima_efnci)

	# If imageb is constant then read imagea.  If the constant
	# is 1 do a vector move to imagec otherwise do a vector/scalar
	# divide to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nli (im, buf, v, 2) != EOF) {
		if (b == 0)
		    call amovki (divzero, Memi[buf[1]], len)
		else if (b == 1)
		    call amovi (Memi[buf[2]], Memi[buf[1]], len)
		else
		    call adivki (Memi[buf[2]], b, Memi[buf[1]], len)
	    }

	# Read imagea and imageb and do the vector divide to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nli (im, buf, v, 3) != EOF)
		call advzi (Memi[buf[2]], Memi[buf[3]], Memi[buf[1]],
		    len, ima_efnci)
	}
end


# IMA_EFNC -- Error function for division by zero.

int procedure ima_efnci (a)

int	a
int	divzero
common	/imadcomi/ divzero

begin
	return (divzero)
end

procedure ima_divl (im_a, im_b, im_c, a, b, c)

pointer	im_a, im_b, im_c
long	a, b, c

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nll()
long	ima_efncl()
extern	ima_efncl

long	divzero
common	/imadcoml/ divzero

begin
	# Loop through all of the image lines.
	divzero = c
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb and do a vector
	# reciprical to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nll (im, buf, v, 2) != EOF)
		call arczl (a, Meml[buf[2]], Meml[buf[1]], len,
		    ima_efncl)

	# If imageb is constant then read imagea.  If the constant
	# is 1 do a vector move to imagec otherwise do a vector/scalar
	# divide to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nll (im, buf, v, 2) != EOF) {
		if (b == 0)
		    call amovkl (divzero, Meml[buf[1]], len)
		else if (b == 1)
		    call amovl (Meml[buf[2]], Meml[buf[1]], len)
		else
		    call adivkl (Meml[buf[2]], b, Meml[buf[1]], len)
	    }

	# Read imagea and imageb and do the vector divide to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nll (im, buf, v, 3) != EOF)
		call advzl (Meml[buf[2]], Meml[buf[3]], Meml[buf[1]],
		    len, ima_efncl)
	}
end


# IMA_EFNC -- Error function for division by zero.

long procedure ima_efncl (a)

long	a
long	divzero
common	/imadcoml/ divzero

begin
	return (divzero)
end

procedure ima_divr (im_a, im_b, im_c, a, b, c)

pointer	im_a, im_b, im_c
real	a, b, c

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nlr()
real	ima_efncr()
extern	ima_efncr

real	divzero
common	/imadcomr/ divzero

begin
	# Loop through all of the image lines.
	divzero = c
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb and do a vector
	# reciprical to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nlr (im, buf, v, 2) != EOF)
		call arczr (a, Memr[buf[2]], Memr[buf[1]], len,
		    ima_efncr)

	# If imageb is constant then read imagea.  If the constant
	# is 1 do a vector move to imagec otherwise do a vector/scalar
	# divide to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nlr (im, buf, v, 2) != EOF) {
		if (b == 0.0)
		    call amovkr (divzero, Memr[buf[1]], len)
		else if (b == 1.0)
		    call amovr (Memr[buf[2]], Memr[buf[1]], len)
		else
		    call adivkr (Memr[buf[2]], b, Memr[buf[1]], len)
	    }

	# Read imagea and imageb and do the vector divide to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nlr (im, buf, v, 3) != EOF)
		call advzr (Memr[buf[2]], Memr[buf[3]], Memr[buf[1]],
		    len, ima_efncr)
	}
end


# IMA_EFNC -- Error function for division by zero.

real procedure ima_efncr (a)

real	a
real	divzero
common	/imadcomr/ divzero

begin
	return (divzero)
end

procedure ima_divd (im_a, im_b, im_c, a, b, c)

pointer	im_a, im_b, im_c
double	a, b, c

int	len
pointer	im[3], buf[3]
long	v[IM_MAXDIM, 3]

int	ima_nld()
double	ima_efncd()
extern	ima_efncd

double	divzero
common	/imadcomd/ divzero

begin
	# Loop through all of the image lines.
	divzero = c
	im[1] = im_c
	len = IM_LEN (im[1], 1)
	call amovkl (long(1), v, 3 * IM_MAXDIM)

	# If imagea is constant then read imageb and do a vector
	# reciprical to imagec.
	if (im_a == NULL) {
	    im[2] = im_b
	    while (ima_nld (im, buf, v, 2) != EOF)
		call arczd (a, Memd[buf[2]], Memd[buf[1]], len,
		    ima_efncd)

	# If imageb is constant then read imagea.  If the constant
	# is 1 do a vector move to imagec otherwise do a vector/scalar
	# divide to imagec.
	} else if (im_b == NULL) {
	    im[2] = im_a
	    while (ima_nld (im, buf, v, 2) != EOF) {
		if (b == 0.0D0)
		    call amovkd (divzero, Memd[buf[1]], len)
		else if (b == 1.0D0)
		    call amovd (Memd[buf[2]], Memd[buf[1]], len)
		else
		    call adivkd (Memd[buf[2]], b, Memd[buf[1]], len)
	    }

	# Read imagea and imageb and do the vector divide to imagec.
	} else {
	    im[2] = im_a
	    im[3] = im_b
	    while (ima_nld (im, buf, v, 3) != EOF)
		call advzd (Memd[buf[2]], Memd[buf[3]], Memd[buf[1]],
		    len, ima_efncd)
	}
end


# IMA_EFNC -- Error function for division by zero.

double procedure ima_efncd (a)

double	a
double	divzero
common	/imadcomd/ divzero

begin
	return (divzero)
end

