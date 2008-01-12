include <error.h>
include	<imhdr.h>
include "../lib/daophotdef.h"

# DP_READPSF -- Read in the PSF from the specified image.

procedure dp_readpsf (dao, im)

pointer	dao			# pointer to the DAOPHOT Structure
pointer	im			# image descriptor

int	i, ival, npsfstars
pointer	sp, str, v, psffit, psflut, buf
real	scale, rval
bool	imgetb(), streq()
int	imgeti(), imgnlr(), btoi()
real	imgetr()
errchk	imgetr(), imgeti()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Set up needed daophot pointers.
	psffit = DP_PSFFIT(dao)

	# Read in the function parameters.
	call imgstr (im, "FUNCTION", Memc[str], SZ_FNAME)
	if (streq (Memc[str], "gauss")) {
	    DP_PSFUNCTION(psffit) = FCTN_GAUSS
	    call strcpy ("gauss", DP_FUNCTION(dao), SZ_FNAME)
	} else if (streq (Memc[str], "moffat25")) {
	    DP_PSFUNCTION(psffit) = FCTN_MOFFAT25
	    call strcpy ("moffat25", DP_FUNCTION(dao), SZ_FNAME)
	} else if (streq (Memc[str], "penny1")) {
	    DP_PSFUNCTION(psffit) = FCTN_PENNY1
	    call strcpy ("penny1", DP_FUNCTION(dao), SZ_FNAME)
	} else if (streq (Memc[str], "moffat15")) {
	    DP_PSFUNCTION(psffit) = FCTN_MOFFAT15
	    call strcpy ("moffat15", DP_FUNCTION(dao), SZ_FNAME)
	} else if (streq (Memc[str], "penny2")) {
	    DP_PSFUNCTION(psffit) = FCTN_PENNY2
	    call strcpy ("penny2", DP_FUNCTION(dao), SZ_FNAME)
	} else if (streq (Memc[str], "lorentz")) {
	    DP_PSFUNCTION(psffit) = FCTN_LORENTZ
	    call strcpy ("lorentz", DP_FUNCTION(dao), SZ_FNAME)
	} else
	    call error (0, "Unknown PSF function in PSF image\n")

	# Read in the position and brightness parameters.
	DP_PSFX (psffit) = imgetr (im, "PSFX")
	DP_PSFY (psffit) = imgetr (im, "PSFY")
	DP_PSFHEIGHT(psffit) = imgetr (im, "PSFHEIGHT")
	DP_PSFMAG (psffit) = imgetr (im, "PSFMAG")

	DP_PSFNPARS(psffit) = imgeti (im, "NPARS")
	do i = 1, DP_PSFNPARS(psffit) {
	    call sprintf (Memc[str], SZ_FNAME, "PAR%d")
		call pargi (i)
	    Memr[DP_PSFPARS(psffit)+i-1] = imgetr (im, Memc[str])
	}

	# Get the psfradius with which the psf was made. Make sure the
	# psf radius requested by the user is less than or equal to the
	# stored psf radius. 

	iferr {
	    scale = imgetr (im, "SCALE")
	} then {
	    DP_PSFRAD(dao) = min (DP_RPSFRAD(dao) / DP_SCALE(dao),
	        imgetr (im, "PSFRAD"))
	    DP_SPSFRAD(dao) = DP_SCALE(dao) * DP_PSFRAD(dao)
	} else {
	    DP_PSFRAD(dao) = min (DP_RPSFRAD(dao) / DP_SCALE(dao),
	        imgetr (im, "PSFRAD") / scale)
	    DP_SPSFRAD(dao) = DP_SCALE(dao) * DP_PSFRAD(dao)
	}

	# Get the lookup table(s) parameters.
	DP_VARORDER(dao) = imgeti (im, "VARORDER")
	switch (DP_VARORDER(dao)) {
	case -1:
	    DP_NVLTABLE(psffit) = 0
	case 0:
	    DP_NVLTABLE(psffit) = 1
	case 1:
	    DP_NVLTABLE(psffit) = 3
	case 2:
	    DP_NVLTABLE(psffit) = 6
	}
	DP_FEXPAND(dao) = btoi (imgetb (im, "FEXPAND"))
	if (DP_FEXPAND(dao) == NO)
	    DP_NFEXTABLE(psffit) = 0
	else
	    DP_NFEXTABLE(psffit) = 5

	# Read in the lookup table(s).
	if ((DP_NVLTABLE(psffit) + DP_NFEXTABLE(psffit)) <= 0) {
	    DP_PSFSIZE(psffit) = 0
	} else {
	    DP_PSFSIZE(psffit) = IM_LEN(im,1)
	    call realloc (DP_PSFLUT(psffit), DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit) * IM_LEN(im,3), TY_REAL)
	    psflut = DP_PSFLUT (psffit)
	    call amovkl (long(1), Meml[v], IM_MAXDIM)
	    while (imgnlr (im, buf, Meml[v]) != EOF) {
	        call amovr (Memr[buf], Memr[psflut], DP_PSFSIZE(psffit))
	        psflut = psflut + DP_PSFSIZE(psffit)
	    }
	}

	# Check that the complete header can be read.
	iferr {
	    npsfstars = imgeti (im, "NPSFSTAR")
	    call sprintf (Memc[str], SZ_FNAME, "ID%d")
		call pargi (npsfstars)
	    ival = imgeti (im, Memc[str])
	    call sprintf (Memc[str], SZ_FNAME, "X%d")
		call pargi (npsfstars)
	    rval = imgetr (im, Memc[str])
	    call sprintf (Memc[str], SZ_FNAME, "Y%d")
		call pargi (npsfstars)
	    rval = imgetr (im, Memc[str])
	    call sprintf (Memc[str], SZ_FNAME, "MAG%d")
		call pargi (npsfstars)
	    rval = imgetr (im, Memc[str])
	} then {
	    call eprintf ("PSF image header is too long to be read in.\n")
	    call eprintf (
	        "Reset min_lenusearea environment variable and try again.")
	    call erract (EA_ERROR)
	}


	call sfree (sp)
end
