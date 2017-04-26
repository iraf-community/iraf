include <math.h>

define	SZ_GRID		10
define	SZ_NTERMS	2

# CALCDS -- Procedure to calculate the values of the CD matrix from the
# GSSS plate solution and a grid of 100 tie points.  This routine was
# adapted from one in stsdas$pkg/analysis/gasp/gasplib/. See the routine
# stsdas$copyright.stsdas.

procedure calcds (plt_centre_ra, plt_centre_dec, plt_centre_x, plt_centre_y,
	x_corner, y_corner, x_pixel_size, y_pixel_size, plate_scale, x_size,
	y_size,	im_cen_ra, im_cen_dec, amd_x, amd_y, cd_matrix)

double  plt_centre_ra		#I plate centre RA (radians)
double  plt_centre_dec		#I plate centre DEC (radians)
double	plt_centre_x		#I x center position (microns)
double	plt_centre_y		#I y center position (microns) 
int	x_corner		#I x lower left of the extracted image
int	y_corner		#I y lower left of the extracted image
double	x_pixel_size		#I y scan pixel size (microns)
double	y_pixel_size		#I y scan pixel size (microns)
double	plate_scale		#I plate scale (arcsec / mm)
int	x_size			#I extracted image size x_axis (pixel)
int	y_size			#I extracted image size y_axis (pixel)
double	im_cen_ra		#I extracted image center RA (radians)
double	im_cen_dec		#I extracted image center DEC (radians)
double  amd_x[ARB]  		#I XI plate solution coefficients
double  amd_y[ARB]		#I ETA coefficients (arsec / mm)
double	cd_matrix[ARB]		#O CD1_1, CD1_2, CD2_1, CD2_2 (degrees / pixel)

double	ra, dec, new_plt_centre_x, new_plt_centre_y, xref, yref, mag, color
double	x_coeff[SZ_NTERMS], y_coeff[SZ_NTERMS], xchisqr, ychisqr
double	x_sigma[SZ_NTERMS], y_sigma[SZ_NTERMS], x, y, xc, yc
pointer	sp, xip, etap, x_arr, y_arr, ww, u, v, w, cvm
int	sx, sy, xlim, ylim, nx, ny, nxy
int	i, j, nterms, xi, eta, npts

begin
	# Initialize color and magnitude.
	mag = 0.0d0
	color = 0.0d0

	# Calculate new plate center in microns.
	new_plt_centre_x = (x_size / 2.0d0) * x_pixel_size
	new_plt_centre_y = (y_size / 2.0d0) * y_pixel_size
	
	call smark (sp)
	call salloc (xip, SZ_GRID * SZ_GRID, TY_DOUBLE)
	call salloc (etap, SZ_GRID * SZ_GRID, TY_DOUBLE)
	call salloc (x_arr, SZ_NTERMS * SZ_GRID * SZ_GRID, TY_DOUBLE)
	call salloc (y_arr, SZ_NTERMS * SZ_GRID * SZ_GRID, TY_DOUBLE)
	call salloc (ww, SZ_GRID * SZ_GRID, TY_REAL)

	sx = max (1, x_size / SZ_GRID)
	sy = max (1, y_size / SZ_GRID)
	xlim = x_size - mod (x_size, sx)
	ylim = y_size - mod (y_size, sy)
	nx = xlim / sx
	ny = ylim / sy
	nxy = nx * ny
	xi =  xip
	eta = etap

	# Compute the grid points.
	npts = 0
	do i = sx, xlim, sx {
	   y = i 			# x coord. from lower left
	   do j = sy, ylim, sy {
	      x =j 			# y coord. from lower left
	      xc = x + x_corner
	      yc = y + y_corner

	      # Obtain ra and dec from this grid (w/r to the original lower
	      # left corner) given the original plate center.
	      call ccgseq (plt_centre_ra, plt_centre_dec, plt_centre_x,
	          plt_centre_y, x_pixel_size, y_pixel_size, plate_scale,
		  amd_x, amd_y, xc, yc, mag, color, ra, dec)
	      
	      # Calculate xi and eta given the new plate center.
	      call treqst (im_cen_ra, im_cen_dec, ra, dec, Memd[xi], Memd[eta])
	      xi = xi + 1
	      eta = eta + 1

	      # Pixel to mm from the new plate center, notice x, y are
	      # w/r to the new lower left corner.
#	      xref = (new_plt_centre_x - x * x_pixel_size) / 1000.
	      xref = (x * x_pixel_size - new_plt_centre_x) / 1000.
	      yref = (y * y_pixel_size - new_plt_centre_y) / 1000.

	      # Form normal equations for the model.
	      #   xi = a*xref + b*yref
	      #  eta = c*yref + d*xref
	      #
	      Memd[x_arr+npts]     = xref       # XAR(j,1)
	      Memd[x_arr+npts+nxy] = yref       # XAR(j,2)
	      Memd[y_arr+npts]     = yref	# YAR(i,1)
	      Memd[y_arr+npts+nxy] = xref       # YAR(i,2)
	      Memr[ww+npts] = 1.0
	      npts = npts + 1
	    }
	}
	
	# Calculate the coefficients.
	nterms = SZ_NTERMS
	call salloc (u, npts * nterms, TY_DOUBLE)
	call salloc (v, nterms * nterms, TY_DOUBLE)
	call salloc (w, nterms, TY_DOUBLE)
	call salloc (cvm, nterms * nterms, TY_DOUBLE)
	call fitsvd (Memd[x_arr], Memd[xip], Memr[ww], npts, x_coeff, 
		     nterms, Memd[u], Memd[v], Memd[w], xchisqr)
	call varsvd (Memd[v], nterms, Memd[w], Memd[cvm], nterms) 
	do i =1, nterms
	   x_sigma[i] = sqrt(Memd[cvm+(i-1)+(i-1)*nterms])
	call fitsvd (Memd[y_arr], Memd[etap], Memr[ww], npts, y_coeff, 
		     nterms, Memd[u], Memd[v], Memd[w], ychisqr)
	call varsvd (Memd[v], nterms, Memd[w], Memd[cvm], nterms) 
	do i =1, nterms		
	   y_sigma[i] = sqrt(Memd[cvm+(i-1)+(i-1)*nterms])

	# Degrees/pixel = (arcsec/mm)*(mm/pixel)*(degrees/arcsec)
	cd_matrix[1] = x_coeff[1] * (x_pixel_size / 1000.0d0 / 3600.0d0)
	cd_matrix[2] = x_coeff[2] * (y_pixel_size / 1000.0d0 / 3600.0d0)
	cd_matrix[3] = y_coeff[2] * (y_pixel_size / 1000.0d0 / 3600.0d0)
	cd_matrix[4] = y_coeff[1] * (x_pixel_size / 1000.0d0 / 3600.0d0)

	call sfree (sp)
end
