include <math.h>
define	SZ_CDMTX	4

# EQTOPIX -- procedure to obtain pixel coordinates from the equatorial
# ones, giving the center position and scale of the frame.

procedure eqtopix (plt_ra_cen, plt_dec_cen, plt_x_cen, plt_y_cen,
	   	     cdmtx, ra, dec, x, y)

double	plt_ra_cen		# Plate centre (radians)
double	plt_dec_cen		#      "          "
double	plt_x_cen		# Plate center in x (pixels)
double	plt_y_cen		# Plate center in y (pixels)
double	cdmtx[SZ_CDMTX]		# CD Matrix (cd11, cd12, cd21, cd22) 
double	ra			# Objects RA position (radians)
double	dec			# Objects DEC position (radians)      
double	x			# Position from centre (pixels)
double	y			#      "

double	xi			# Standard coordinate (degrees)
double	eta			#		"
double	det, cd11, cd12, cd21, cd22
double	xi_arcs, eta_arcs	# Standard coord. in arc seconds.

begin

	cd11 = cdmtx[1]
	cd12 = cdmtx[2]
	cd21 = cdmtx[3]
	cd22 = cdmtx[4]

	det = (cd11*cd22 - cd21*cd12)
	   call treqst (plt_ra_cen, plt_dec_cen, ra, dec, 
			xi_arcs, eta_arcs)
	   xi = xi_arcs/3600.0		# To degrees
	   eta = eta_arcs/3600.0	# To degrees
	   # Calculate x[i] and y[i] by solving the linear equations:
	   #     xi = cd11(X-Xo) + cd12(Y-Yo)
	   #    eta = cd21(X-Xo) + cd22(Y-Yo)

	   x = (cd22*xi - cd12*eta)/det  + plt_x_cen
	   y = (cd11*eta - cd21*xi)/det  + plt_y_cen
end
