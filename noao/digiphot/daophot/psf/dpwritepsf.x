include <time.h>
include <imhdr.h>
include	"../lib/daophotdef.h"
include	"../lib/apseldef.h"
include	"../lib/psfdef.h"

# DP_WRITEPSF -- Write out the PSF into an IRAF image. 

procedure dp_writepsf (dao, im, psfim)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
pointer	psfim			# pointer to the output psf image

begin
	# Check that the psfimage is open.
	if (psfim == NULL)
	    return

	# Write out the id and fitting parameters.
	call dp_widpars (dao, psfim)

	# Write out the psf function definition parameters.
	call dp_wfuncpars (dao, psfim) 

	# Write out the list of PSF stars.
	call dp_wstars (dao, im, psfim)

	# Write out the lookup table.
	call dp_wlt (dao, psfim)
end


# DP_WIDPARS -- Add the id and fitting parameters to the PSF image header

procedure dp_widpars (dao, psfim)

pointer	dao			# pointer to the daophot structure
pointer	psfim			# the psf image descriptor

pointer	sp, outstr, date, time
bool	itob()
int	envfind()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Record IRAF version, user, host, date, time, package and task.
	if (envfind ("version", Memc[outstr], SZ_LINE) <= 0)
	    call strcpy ("IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call imastr (psfim, "IRAF", Memc[outstr])
	call gethost (Memc[outstr], SZ_LINE)
	call imastr (psfim, "HOST", Memc[outstr])
	if (envfind ("userid", Memc[outstr], SZ_LINE) <= 0)
	    Memc[outstr] = EOS
	call imastr (psfim, "USER", Memc[outstr])
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call imastr (psfim, "DATE", Memc[date])
	call imastr (psfim, "TIME", Memc[time])

	# Write out the package, task, and input/output file names.
	call imastr (psfim, "PACKAGE", "daophot")
	call imastr (psfim, "TASK", "psf")
	call dp_imroot (DP_INIMAGE(dao), Memc[outstr], SZ_LINE)
	call imastr (psfim, "IMAGE", Memc[outstr])
	call dp_froot (DP_INPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call imastr (psfim, "PHOTFILE", Memc[outstr])
	call dp_froot (DP_COORDS(dao), Memc[outstr], SZ_LINE)
	call imastr (psfim, "PSTFILE", Memc[outstr])
	call dp_imroot (DP_PSFIMAGE(dao), Memc[outstr], SZ_LINE)
	call imastr (psfim, "PSFIMAGE", Memc[outstr])
	call dp_froot (DP_OUTREJFILE(dao), Memc[outstr], SZ_LINE)
	call imastr (psfim, "OPSTFILE", Memc[outstr])
	call dp_froot (DP_OUTPHOTFILE(dao), Memc[outstr], SZ_LINE)
	call imastr (psfim, "GRPSFILE", Memc[outstr])

	# Add information about fitting parameters.
	call imaddr (psfim, "SCALE", DP_SCALE(dao))
	call imaddr (psfim, "PSFRAD", DP_SPSFRAD (dao))
	call imaddr (psfim, "FITRAD", DP_SFITRAD(dao))
	call imaddr (psfim, "DATAMIN", DP_MINGDATA(dao))
	call imaddr (psfim, "DATAMAX", DP_MAXGDATA(dao))
	call imaddi (psfim, "NCLEAN", DP_NCLEAN(dao))
	call imaddb (psfim, "USESAT", itob (DP_SATURATED(dao)))

	# Define the image title.
	call sprintf (IM_TITLE(psfim), SZ_IMTITLE, "PSF for image: %s")
	    call pargstr (DP_INIMAGE(dao))

	call sfree (sp)
end


# DP_WFUNCPARS -- Write out the the parameters of the PSF function
# to the PSF image.

procedure dp_wfuncpars (dao, psfim)

pointer	dao			# pointer to the daophot structure
pointer	psfim			# image descriptor

int	i
pointer	sp, str, psffit
bool	itob()

begin
	psffit = DP_PSFFIT(dao)

	call imastr (psfim, "FUNCTION", DP_FUNCTION(dao))
	call imaddr (psfim, "PSFX", DP_PSFX(psffit))
	call imaddr (psfim, "PSFY", DP_PSFY(psffit))
	call imaddr (psfim, "PSFHEIGHT", DP_PSFHEIGHT(psffit))
	call imaddr (psfim, "PSFMAG", DP_PSFMAG (psffit))

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	switch (DP_PSFUNCTION(psffit)) {
	case FCTN_MOFFAT25:
	    call imaddi (psfim, "NPARS", DP_PSFNPARS(psffit)+1)
	    do i = 1, DP_PSFNPARS(psffit) {
	        call sprintf (Memc[str], SZ_FNAME, "PAR%d")
		    call pargi (i)
	        call imaddr (psfim, Memc[str], Memr[DP_PSFPARS(psffit)+i-1])
	    }
	    call sprintf (Memc[str], SZ_FNAME, "PAR%d")
		call pargi (DP_PSFNPARS(psffit)+1)
	    call imaddr (psfim, Memc[str], 2.5)
	case FCTN_MOFFAT15:
	    call imaddi (psfim, "NPARS", DP_PSFNPARS(psffit)+1)
	    do i = 1, DP_PSFNPARS(psffit) {
	        call sprintf (Memc[str], SZ_FNAME, "PAR%d")
		    call pargi (i)
	        call imaddr (psfim, Memc[str], Memr[DP_PSFPARS(psffit)+i-1])
	    }
	    call sprintf (Memc[str], SZ_FNAME, "PAR%d")
		call pargi (DP_PSFNPARS(psffit)+1)
	    call imaddr (psfim, Memc[str], 1.5)
	default:
	    call imaddi (psfim, "NPARS", DP_PSFNPARS(psffit))
	    do i = 1, DP_PSFNPARS(psffit) {
	        call sprintf (Memc[str], SZ_FNAME, "PAR%d")
		    call pargi (i)
	        call imaddr (psfim, Memc[str], Memr[DP_PSFPARS(psffit)+i-1])
	    }
	}

	call imaddi (psfim, "VARORDER", DP_VARORDER(dao))
	call imaddb (psfim, "FEXPAND", itob (DP_FEXPAND(dao)))

	call sfree (sp)
end


# DP_WSTARS -- Write out the PSF star list to the PSF image.

procedure dp_wstars (dao, im, psfim)

pointer	dao			# pointer to the daophot descriptor
pointer	im			# the input image descriptor
pointer	psfim			# the psfimage descriptor

real	tx, ty
pointer	apsel, psf, sp, str
int	i

begin
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Write out the number of PSF stars.
	call sprintf (Memc[str], SZ_FNAME, "NPSFSTAR")
	call imaddi (psfim, Memc[str], DP_PNUM(psf))

	# Write out the ids of all the PSF stars.
	do i = 1, DP_PNUM(psf) {

	    call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+i-1],
		Memr[DP_APYCEN(apsel)+i-1], tx, ty, 1)
	    call sprintf (Memc[str], SZ_FNAME, "ID%d")
	        call pargi (i)
	    call imaddi (psfim, Memc[str], Memi[DP_APID(apsel)+i-1])

	    call sprintf (Memc[str], SZ_FNAME, "X%d")
	        call pargi (i)
	    call imaddr (psfim, Memc[str], tx)

	    call sprintf (Memc[str], SZ_FNAME, "Y%d")
	        call pargi (i)
	    call imaddr (psfim, Memc[str], ty)

	    call sprintf (Memc[str], SZ_FNAME, "MAG%d")
	        call pargi (i)
	    call imaddr (psfim, Memc[str], Memr[DP_APMAG(apsel)+i-1]) 
	}

	call sfree (sp)
end


# DP_WLT -- Write out the PSF lookup table to the output PSF image.

procedure dp_wlt (dao, psfim)

pointer	dao			# pointer to DAO Structure
pointer	psfim			# image descriptor

int	nexp
pointer	psffit

begin
	psffit = DP_PSFFIT(dao)
	nexp = DP_NVLTABLE(psffit) + DP_NFEXTABLE(psffit)

	IM_PIXTYPE(psfim) = TY_REAL
	if (nexp == 0) {
	    IM_NDIM(psfim) = 0
	} else if (nexp == 1) {
	    IM_NDIM(psfim) = 2
	    IM_LEN(psfim,1) = DP_PSFSIZE(psffit)
	    IM_LEN(psfim,2) = DP_PSFSIZE(psffit)
	} else {
	    IM_NDIM(psfim) = 3
	    IM_LEN(psfim,1) = DP_PSFSIZE(psffit)
	    IM_LEN(psfim,2) = DP_PSFSIZE(psffit)
	    IM_LEN(psfim,3) = nexp
	}

	if (nexp > 0)
	    call dp_wltim (psfim, Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
	        DP_PSFSIZE(psffit), nexp) 
end


# DP_WLTIM -- Write the lookup tables into the image pixels.

procedure dp_wltim (psfim, psflut, nxlut, nylut, nexp)

pointer	psfim			# image descriptor
real	psflut[nexp,nxlut,ARB]	# the psf lookup table
int	nxlut, nylut,nexp	# the dimensions of the psf look-up table

int	i, j, k
pointer	sp, v, buf
int	impnlr()

begin
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[v], IM_MAXDIM)
	do k = 1, nexp {
	    do j = 1, nylut {
		if (impnlr (psfim, buf, Meml[v]) == EOF)
		    ;
		do i = 1, nxlut
		    Memr[buf+i-1] = psflut[k,i,j]
	    }
	}

	call sfree (sp)
end
