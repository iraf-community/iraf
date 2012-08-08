# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pmset.h>

task    sum = t_sum

# SUM -- Sum the image pixels lying within the given mask.

procedure t_sum()

char    image[SZ_FNAME]              # input data image
char    mask[SZ_FNAME]               # image mask

int     npix, mval, totpix, m_flags
long    v[PM_MAXDIM]
pointer im, mp, pp
real    sum

bool	clgetb()
real    asumr()
int     mio_glsegr()
pointer immap(), mio_open()

begin
	call clgstr ("image", image, SZ_FNAME)
	call clgstr ("mask", mask, SZ_FNAME)
	m_flags = 0
	if (clgetb ("invert"))
	    m_flags = INVERT_MASK

	im = immap (image, READ_ONLY, 0)
	mp = mio_open (mask, m_flags, im)

	sum = 0;  totpix = 0
	while (mio_glsegr (mp, pp, mval, v, npix) != EOF) {
	    sum = sum + asumr (Memr[pp], npix)
	    totpix = totpix + npix
	}

	call mio_close (mp)
	call imunmap (im)

	call printf ("%d pixels, sum=%g, mean=%g\n")
	    call pargi (totpix)
	    call pargr (sum)
	    if (totpix > 0)
		call pargr (sum / totpix)
	    else
		call pargr (INDEF)
end
