include <time.h>
include <fset.h>
include <imset.h>
include <imhdr.h>
include <math.h>
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_WRITEPSF -- Write out the PSF into an IRAF image. 

int procedure	dp_writepsf (dao, im, psfim)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
pointer	psfim			# pointer to the output psfim

pointer	psffit, sp, temp, str
real	dx, dy
int	dp_wrtlut()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR) 

	# Get pointers to other PSF structures.
	psffit = DP_PSFFIT(dao)

	# If the PSF image has already been written into and the PSF
	# changes size delete the old image and remap it.

	# Set the parameters of the output PSF image.
	if (DP_VARPSF(dao) == YES)
	    IM_NDIM(psfim) = 3
	else
	    IM_NDIM(psfim) = 2
	IM_LEN (psfim,1) = DP_PSFSIZE (psffit)
	IM_LEN (psfim,2) = DP_PSFSIZE (psffit)
	if (DP_VARPSF(dao) == YES)
	    IM_LEN(psfim,3) = 3
	IM_PIXTYPE(psfim) = TY_REAL
	call sprintf (IM_TITLE(psfim), SZ_IMTITLE, "PSF for image: %s")
	    call pargstr (IM_HDRFILE(im))

	# Write out the lookup table.
	if (dp_wrtlut (dao, psfim, dx, dy) == ERR) {
	    call sfree (sp)
	    return (ERR)
	}

	# Add information about fitting parameters.
	call imaddr (psfim, "SCALE", DP_SCALE(dao))
	call imaddr (psfim, "PSFRAD", DP_SPSFRAD (dao))
	call imaddr (psfim, "FITRAD", DP_SFITRAD(dao))
	call imaddr (psfim, "DATAMIN", DP_MINGDATA(dao))
	call imaddr (psfim, "DATAMAX", DP_MAXGDATA(dao))
	call imaddr (psfim, "GAIN", DP_PHOT_ADC(dao))
	call imaddr (psfim, "READNOISE", DP_READ_NOISE(dao))

	# Add information about the PSF fitting parameters to the PSF header.
	call imaddr (psfim, "PSFMAG", DP_PSFMAG (psffit))
	call imaddr (psfim, "XPSF", DP_XPSF(psffit))
	call imaddr (psfim, "YPSF", DP_YPSF(psffit))

	# Add the parameters of the Gaussian fit to the PSF image header.
	call imaddr (psfim, "HEIGHT", DP_PSFHEIGHT(psffit))
	call imaddr (psfim, "XOFFSET", DP_PSFDXCEN(psffit))
	call imaddr (psfim, "YOFFSET", DP_PSFDYCEN(psffit))
	call imaddr (psfim, "SIGMAX", DP_PSFSIGX(psffit))
	call imaddr (psfim, "SIGMAY", DP_PSFSIGY(psffit))

	return (OK)
end


# DP_HEADPARS -- Add miscellaneous parameters to the header of the PSF image.

procedure dp_headpars (im, dao)

pointer	im			# image descriptor
pointer	dao			# pointer to the daophot structure

pointer	sp, outstr, date, time
int	envfind()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Write out the IRAF and system id.
	if (envfind ("version", Memc[outstr], SZ_LINE) <= 0)
	    call strcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call dp_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call imastr (im, "IRAF", Memc[outstr])
	if (envfind ("userid", Memc[outstr], SZ_LINE) <= 0)
	    Memc[outstr] = EOS
	call imastr (im, "USER", Memc[outstr])
	call gethost (Memc[outstr], SZ_LINE)
	call imastr (im, "HOST", Memc[outstr])

	# Write out the date.
	call dp_date (Memc[date], Memc[time], SZ_DATE)
	call imastr (im, "DATE", Memc[date])
	call imastr (im, "TIME", Memc[time])

	# Write out the package and task description. Other parameters may
	# be added in the future.
	call imastr (im, "PACKAGE", "daophot")
	call imastr (im, "TASK", "psf")

	# Write out the file names.
	call imastr (im, "IMAGE", DP_IMNAME(dao))
	call imastr (im, "APFILE", DP_APFILE(dao))
	call imastr (im, "PSFIMAGE", DP_PSFIMAGE(dao))
	call imastr (im, "GRPSFILE", DP_GRPSFFILE(dao))

	call sfree(sp)
end


# DP_WRTLUT -- Write out the PSF lookup table to the output PSF image.

int procedure dp_wrtlut (dao, im, dx, dy)

pointer	dao			# pointer to DAO Structure
pointer	im			# image descriptor
real	dx, dy			# psf position offsets

int	ncols
long	v[IM_MAXDIM]
pointer	psf, psffit, psflut, psfmatrix, buf
real	coeff[3]
pointer	impnlr()

begin
	psffit = DP_PSFFIT (dao)
	psf = DP_PSF (dao)
	ncols = DP_PSFSIZE (psffit)
	psfmatrix = DP_PSFMATRIX (psf)
	psflut = DP_PSFLUT(psffit)

	# If the lookup table is undefined return an error.
	if (psfmatrix == NULL || psflut == NULL)
	    return (ERR)

	# Set the offsets.
	call dp_xyoff (Memr[DP_PSFMATRIX(psf)], coeff, SZ_PSFMATRIX,
	    dx,  dy)

	# Normalize and copy the lookup table into the PSF image.
	if (DP_VARPSF(dao) == YES) {
	    call dp_3dlut (dao, Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		DP_PSFSIZE(psffit), Memr[DP_PSFMATRIX(psf)], coeff)
	    call amovkl (long(1), v, IM_MAXDIM)
	    while (impnlr (im, buf, v) != EOF) {
	        call amovr (Memr[psflut], Memr[buf], ncols)
	        psflut = psflut + ncols
	    }
	} else {
	    call amovkl (long(1), v, IM_MAXDIM)
	    while (impnlr (im, buf, v) != EOF) {
	        call amovr (Memr[psflut], Memr[buf], ncols)
	        call adivkr (Memr[buf], Memr[psfmatrix], Memr[buf], ncols)
	        psflut = psflut + ncols
	    }
	}

	return (OK)
end



# DP_3DLUT -- Procedure to compute the 3d look up table.

procedure dp_3dlut (dao, psflut, nxpsf, nypsf, matrix, coeff)

pointer	dao			    # the daophot pointer
real	psflut[nxpsf,nypsf,3]	    # the lookup table
int	nxpsf, nypsf		    # dimensions of the lut
real	matrix[3,3]		    # maxtrix
real	coeff[3]		    # coefficient array

int	i, j, k, l, status
pointer	psffit
real	x, y, sigx, sigy, p, erfx, erfy, z[3]
real	erf()

begin
	psffit = DP_PSFFIT(dao)

	call invers (matrix, SZ_PSFMATRIX, SZ_PSFMATRIX, status)

	do j = 1, nypsf {
	    do i = 1, nxpsf {
		do k = 1, 3 {
		    z[k] = 0.0
		    do l = 1, 3
			z[k] = z[k] + matrix[k,l] * psflut[i,j,l]
		}
		do k = 1, 3
		    psflut[i,j,k] = z[k]
	    }
	}

	x = real (nxpsf + 1) / 2. + DP_PSFDXCEN(psffit)
	y = real (nypsf + 1) / 2. + DP_PSFDYCEN(psffit)
	sigx = 2.0 * DP_PSFSIGX(psffit)
	sigy = 2.0 * DP_PSFSIGY(psffit)

	do k = 2, 3 {
	    p = 0.0
	    do j = 1, nypsf {
		do i = 1, nxpsf
		    p = p + psflut[i,j,k]
	    }
	    p = p / (TWOPI * sigx * sigy)

	    do j = 1, nypsf {
	        erfy = erf (real (j), y, sigy, DP_PSFDHDYC(psffit),
		    DP_PSFDHDSY(psffit))
	        do i = 1, nxpsf {
	            erfx = erf (real (i), x, sigx, DP_PSFDHDXC(psffit),
		        DP_PSFDHDSX(psffit))
		    psflut[i,j,1] = psflut[i,j,1] + coeff[k] * p * erfx *
			erfy
		    psflut[i,j,k] = psflut[i,j,k] - p * erfx * erfy
	        }
	    }
	}
end


# DP_XYOFF -- Compute the x and y offsets.

procedure dp_xyoff (matrix, coeff, n, dx, dy)

real	matrix[n,n]			# matrix
real	coeff[n]			# coefficient array
int	n				# dimensions of matrix
real	dx, dy				# x and y offsets

begin
	coeff[2] = matrix[1,2] / matrix[1,1]
	coeff[3] = matrix[1,3] / matrix[1,1]
	dx = coeff[2]
	dy = coeff[3]
end


# DP_RMPSF -- Delete the psf image and psf group file.

procedure dp_rmpsf (dao, psfim, psfgr)

pointer	dao			# pointer to the daophot structure
pointer	psfim			# pointer to the psf image
pointer	psfgr			# pointer to the psf group image

pointer	sp, temp

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	if (DP_TEXT(dao) == YES) {
	    call fstats (psfgr, F_FILENAME, Memc[temp], SZ_FNAME)
	    call close (psfgr)
	} else {
	    call tbtnam (psfgr, Memc[temp], SZ_FNAME)
	    call tbtclo (psfgr)
	}
	call delete (Memc[temp])
	psfgr = NULL

	call strcpy (IM_HDRFILE(psfim), Memc[temp], SZ_FNAME)
	call imunmap (psfim)
	call imdelete (Memc[temp])
	psfim = NULL

	call sfree (sp)
end
