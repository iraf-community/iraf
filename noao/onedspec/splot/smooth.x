include "../idsmtn.h"

# SMOOTH -- Box smooth the array

procedure smooth (gfd, pix, npts, ids)

int	gfd
real	pix[ARB]
int	npts
pointer	ids

int	i, j, boxsize, halfbox, del
int	nsum
real	sum
pointer	sp, smpix

int	clgeti()

begin
	call smark (sp)
	call salloc (smpix, npts, TY_REAL)

	# Get boxsize
	boxsize = clgeti ("boxsize")
	SM_FLAG(ids) = boxsize

	halfbox = boxsize/2

	# This is not efficiently coded, but easy to code
	# A running box mean would be faster
	do i = 1, npts {
	    sum = 0.0
	    nsum = 0

	    if (i > halfbox && i < (npts-halfbox))
		del = halfbox
	    else
	    if (i <= halfbox)
		del = i/2
	    else
		del = (npts - i + 1)/2

	    do j = i-del, i+del {
		nsum = nsum + 1
		sum = sum + pix[j]
	    }

	    Memr[smpix+i-1] = sum / nsum
	}

	# Replace pixels back
	call amovr (Memr[smpix], pix, npts)
	call sfree (sp)
end
