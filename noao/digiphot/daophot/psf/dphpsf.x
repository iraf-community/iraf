include	"../lib/psf.h"

# DP_APSF -- Add the current PSF star's id, position and magnitude to PSF
# image header.

procedure dp_apsf (psfim, number, id, x, y, magnitude)

pointer	psfim				# PSF image descriptor
int	number				# number of current PSF star
int	id				# the id of the star
real	x, y				# position of PSF star
real	magnitude			# magnitude of PSF star

pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[str], SZ_FNAME, "IDPSF%d")
	    call pargi (number)
	call imaddi (psfim, Memc[str], id)

	call sprintf (Memc[str], SZ_FNAME, "XPSF%d")
	    call pargi (number)
	call imaddr (psfim, Memc[str], x) 

	call sprintf (Memc[str], SZ_FNAME, "YPSF%d")
	    call pargi (number)
	call imaddr (psfim, Memc[str], y) 

	call sprintf (Memc[str], SZ_FNAME, "MPSF%d")
	    call pargi (number)
	call imaddr (psfim, Memc[str], magnitude) 

	call sfree (sp)
end


# DP_DPSF -- Delete all the PSF star information from the PSF  image header.

procedure dp_dpsf (psfim)

pointer	psfim			# pointer to the psf image

int	number
pointer	sp, str
errchk	imdelf

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	number = 1
	repeat {
	    iferr {
		call sprintf (Memc[str], SZ_FNAME, "IDPSF%d")
	    	    call pargi (number)
		call imdelf (psfim, Memc[str])
		call sprintf (Memc[str], SZ_FNAME, "XPSF%d")
	    	    call pargi (number)
		call imdelf (psfim, Memc[str])
		call sprintf (Memc[str], SZ_FNAME, "YPSF%d")
	    	    call pargi (number)
		call imdelf (psfim, Memc[str])
		call sprintf (Memc[str], SZ_FNAME, "MPSF%d")
	    	    call pargi (number)
		call imdelf (psfim, Memc[str])
	    } then
		break
	    number = number + 1
	}

	call sfree (sp)
end


# DP_ISPSF -- Is the star under consideration already a PSF star ?

bool procedure dp_ispsf (psfim, starid, npsf)

pointer	psfim			# pointer to the psf image
int	starid			# id of star to be checked
int	npsf			# total number of psf stars

bool	psfstar
int	i, id
pointer	sp, str
int	imgeti()
errchk	imgeti()

begin
	psfstar = false

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	do i = 1, npsf {
	    call sprintf (Memc[str], SZ_FNAME, "IDPSF%d")
	    	call pargi (i)
	    iferr (id = imgeti (psfim, Memc[str]))
		next
	    if (id != starid)
		next
	    psfstar = true
	    break
	}

	call sfree (sp)

	return (psfstar)
end
