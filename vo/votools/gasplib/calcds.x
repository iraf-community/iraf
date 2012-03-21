include <math.h>
define	SZ_CDMATRIX	4	# CD1_1, CD1_2, CD2_1, CD2_2

# CALCDS -- Procedure to calculate the values of the CD matrix from 
# 	    information given by a extracted Guide Star file.

procedure calcds (plt_centre_ra,
		  plt_centre_dec,
		  plt_centre_x,
		  plt_centre_y,
		  x_corner,
		  y_corner,
		  x_pixel_size,
		  y_pixel_size,
		  plate_scale,
		  x_size,
		  y_size,	
	          im_cen_ra,
	     	  im_cen_dec,
		  amd_x,
		  amd_y,
		  cd_matrix)

double  plt_centre_ra		# Plate centre RA (radians)
double  plt_centre_dec		# Plate centre DEC
double	plt_centre_x		# X center position (microns)
double	plt_centre_y		# Y center position 
int	x_corner		# x lower left of the extracted subset
int	y_corner		# y
double	x_pixel_size		# X scan pixel size (microns)
double	y_pixel_size		# Y scan pixel size
double	plate_scale		# Plate scale (arcsec/mm)
int	x_size			# Extracted image size x_axis (pixel)
int	y_size			# Extracted image size y_axis (pixel)
double	im_cen_ra		# Extracted image center RA (radians)
double	im_cen_dec		# Extracted image center DEC (radians)
double  amd_x[ARB]  		# Xi plate solution coefficients
double  amd_y[ARB]		# Eta coefficients (arsec/mm)
double	cd_matrix[SZ_CDMATRIX]  # CD1_1, CD1_2, CD2_1, CD2_2 (degrees/pixel)

pointer	sp, xip, etap, x_arr, y_arr, u, v, w, cvm
int	sx, sy, xlim, ylim
int	i, j, k, nterms, xi, eta, npts
double	x_coeff[2], y_coeff[2], xchisqr, ychisqr
double	x_sigma[2], y_sigma[2], x, y, xc, yc
real	ww[100]
double	new_plt_centre_x, new_plt_centre_y, xref, yref, mag, color
double	ra, dec
int	nx,ny,nxy

begin
	mag = 0.0
	color= 0.0
	# calculate new plate center in microns
	new_plt_centre_x = (x_size/2.)*x_pixel_size
	new_plt_centre_y = (y_size/2.)*y_pixel_size
	
	call smark (sp)
	call salloc (xip, 100, TY_DOUBLE)
	call salloc (etap, 100, TY_DOUBLE)
	call salloc (x_arr, 2*100, TY_DOUBLE)
	call salloc (y_arr, 2*100, TY_DOUBLE)

	sx = max (1, x_size/10)
	sy = max (1, y_size/10)

	xlim = x_size - mod(x_size, sx)
	ylim = y_size - mod(y_size, sy)
	nx = xlim/sx
	ny = ylim/sy
	nxy = nx*ny
	xi =  xip
	eta = etap
	k = 0
	do i = sx, xlim, sx {
	   y = i 			# x coord. from lower left
	   do j = sy, ylim, sy {
	      x =j 			# y coord. from lower left
	      xc = x + x_corner
	      yc = y + y_corner
	      # Obtain ra and dec from this grid (w/r to the original lower
	      # left corner) given the original plate center.
	      call ccgseq (plt_centre_ra, plt_centre_dec,
		   plt_centre_x, plt_centre_y,
		   x_pixel_size, y_pixel_size, plate_scale, amd_x, amd_y,
		   xc, yc, mag, color, ra, dec)
	      
	      # Calculate xi and eta given the new plate center
	      call treqst (im_cen_ra, im_cen_dec, ra, dec, Memd[xi], Memd[eta])
	      xi = xi + 1
	      eta = eta + 1

	      # pixel to mm from the new plate center, notice x, y are
	      # w/r to the new lower left corner
#	      xref = (new_plt_centre_x - x * x_pixel_size) / 1000.
	      xref = (x * x_pixel_size - new_plt_centre_x) / 1000.
	      yref = (y * y_pixel_size - new_plt_centre_y) / 1000.

	      # form normal equations for the model
	      #   xi = a*xref + b*yref
	      #  eta = c*yref + d*xref
	      #
	      Memd[x_arr+k]     = xref       # XAR(j,1)
	      Memd[x_arr+k+nxy] = yref       # XAR(j,2)
	      Memd[y_arr+k]     = yref	     # YAR(i,1)
	      Memd[y_arr+k+nxy] = xref       # YAR(i,2)
		
	      k = k + 1
	      ww[k] = 1.0
	    }
	}
	
	# calculate coefficients now

	npts = k
	nterms = 2
	call salloc (u, npts*nterms, TY_DOUBLE)
	call salloc (v, nterms*nterms, TY_DOUBLE)
	call salloc (w, nterms, TY_DOUBLE)
	call salloc (cvm, nterms*nterms, TY_DOUBLE)
	call fitsvd (Memd[x_arr], Memd[xip], ww, npts, x_coeff, 
		     nterms, Memd[u], Memd[v], Memd[w], xchisqr)
	call varsvd (Memd[v], nterms, Memd[w], Memd[cvm], nterms) 
	do i =1, nterms
	   x_sigma[i] = sqrt(Memd[cvm+(i-1)+(i-1)*nterms])
	call fitsvd (Memd[y_arr], Memd[etap], ww, npts, y_coeff, 
		     nterms, Memd[u], Memd[v], Memd[w], ychisqr)
	call varsvd (Memd[v], nterms, Memd[w], Memd[cvm], nterms) 
	do i =1, nterms		
	   y_sigma[i] = sqrt(Memd[cvm+(i-1)+(i-1)*nterms])

	# degrees/pixel = (arcsec/mm)*(mm/pixel)*(degrees/arcsec)
	cd_matrix[1] = x_coeff[1]*(x_pixel_size/1000.)*(1/3600.)
	cd_matrix[2] = x_coeff[2]*(y_pixel_size/1000.)*(1/3600.)
	cd_matrix[3] = y_coeff[2]*(y_pixel_size/1000.)*(1/3600.)
	cd_matrix[4] = y_coeff[1]*(x_pixel_size/1000.)*(1/3600.)

	call sfree (sp)
end
