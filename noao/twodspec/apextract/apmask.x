include	<imhdr.h>
include	<pmset.h>
include	"apertures.h"

# AP_MASK -- Create an aperture mask.
# The mask is boolean with pixels within the apertures having value 1 and
# pixels outside the mask having values 0.  An additional  buffer distance 
# may be specified.

procedure ap_mask (image, output,  aps, naps)

char	image[SZ_FNAME]		# Image name
char	output[SZ_FNAME]	# Output mask name
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

real	buffer			# Buffer distance

int	i, j, aaxis, baxis, nc, nl, na, nb, apmin, apmax, low, high
real	aplow, aphigh, shift
long	v[2]
short	val
pointer	im, pm, ap, cv, a1, b1
pointer	sp, name, str, buf, a, b, amin, bmax

real	clgetr(), ap_cveval()
bool	ap_answer()
pointer	ap_immap(), pm_newmask()
errchk	ap_immap, pm_savef

begin
	# Query user.
	call smark (sp)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Create aperture mask for %s?")
	    call pargstr (image)
	if (!ap_answer ("ansmask", Memc[str])) {
	    call sfree (sp)
	    return
	}

	# Get buffer distance.
	buffer = clgetr ("buffer")

	# Make the image and initialize the mask.
	im = ap_immap (image, aaxis, baxis)
	pm = pm_newmask (im, 1)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	na = IM_LEN(im,aaxis)
	nb = IM_LEN(im,baxis)

	# Allocate memory.
	call salloc (buf, nc, TY_SHORT)
	call salloc (a, naps*nb, TY_SHORT)
	call salloc (b, naps*nb, TY_SHORT)
	call salloc (amin, naps, TY_SHORT)
	call salloc (bmax, naps, TY_SHORT)
	val = 1

	# Go through and compute all the limits as well as the maximum
	# range of each aperture.  This information must be computed for
	# an aperture axis of 2 and it is also done for aperture axis
	# of 1 just to keep the code the same.

	do i = 1, naps {
	    ap = aps[i]
	    cv = AP_CV(ap)
	    aplow = AP_CEN(ap,aaxis) + AP_LOW(ap,aaxis) - buffer
	    aphigh = AP_CEN(ap,aaxis) + AP_HIGH(ap,aaxis) + buffer
	    apmin = aplow
	    apmax = aphigh
	    a1 = a + (i - 1) * nb
	    b1 = b + (i - 1) * nb
	    do j = 1, nb {
		shift = ap_cveval (cv, real (j))
		low = nint (aplow + shift)
		high = nint (aphigh + shift)
		Mems[a1+j-1] = low
		Mems[b1+j-1] = high
		apmin = min (low, apmin)
		apmax = max (high, apmax)
	    }
	    Mems[amin+i-1] = apmin
	    Mems[bmax+i-1] = apmax
	}

	# For each line create a pixel array mask.  For aperture axis 1 this
	# is simple while for aperture axis 2 we have to look through each
	# line to see if any apertures intersect the line.

	switch (aaxis) {
	case 1:
	    do j = 1, nl {
	        v[2] = j
		call aclrs (Mems[buf], nc)
		a1 = a + j - 1
		b1 = b + j - 1
	        do i = 1, naps {
		    low = Mems[a1]
		    high = Mems[b1]
		    low = max (1, low)
		    high = min (na, high)
		    if (low <= high)
			call amovks (val, Mems[buf+low-1], high-low+1)
		    a1 = a1 + nb
		    b1 = b1 + nb
	        }
	        call pmplps (pm, v, Mems[buf], 1, nc, PIX_SRC)
	    }
	case 2:
	    do j = 1, nl {
		v[2] = j
		call aclrs (Mems[buf], nc)
		do i = 1, naps {
		    if (j < Mems[amin+i-1] || j > Mems[bmax+i-1])
			next

		    a1 = a + (i - 1) * nb
		    b1 = b + (i - 1) * nb
		    for (low=0; low<nb; low=low+1) {
			if (j < Mems[a1+low] || j > Mems[b1+low])
			    next
			for (high=low+1; high<nb; high=high+1)
			    if (j < Mems[a1+high] || j > Mems[b1+high])
				break
			call amovks (val, Mems[buf+low], high-low)
			low = high - 1
		    }
		}
	        call pmplps (pm, v, Mems[buf], 1, nc, PIX_SRC)
	    }
	}
	    
	# Log the output and finish up.
	if (output[1] == EOS) {
	    call sprintf (Memc[name], SZ_LINE, "%s.pl")
	        call pargstr (image)
	} else
	    call strcpy (output, Memc[name], SZ_LINE)
	call sprintf (Memc[str], SZ_LINE, "Aperture mask for %s")
	    call pargstr (image)

	call pm_savef (pm, Memc[name], Memc[str], 0)
	call pm_close (pm)
	call imunmap (im)

	call sprintf (Memc[str], SZ_LINE, "MASK - Aperture mask for %s --> %s")
       	    call pargstr (image)
	    call pargstr (Memc[name])
	call ap_log (Memc[str], YES, YES, NO)

	call sfree (sp)
end
