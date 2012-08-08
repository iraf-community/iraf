include <math.h>
define	SZ_CDMTX	4

# PHYSTOPIX -- procedure to obtain pixel coordinates from the equatorial
# ones, giving the center position and scale of the frame.

procedure pixtoeq (plt_ra_cen, plt_dec_cen, plt_x_cen, plt_y_cen,
	   	     cdmtx, x, y, ra, dec)

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
double	cd11, cd12, cd21, cd22
double	xi_arcs, eta_arcs	# Standard coord. in arc seconds.

begin

	cd11 = cdmtx[1]
	cd12 = cdmtx[2]
	cd21 = cdmtx[3]
	cd22 = cdmtx[4]

	xi  = cd11*(x-plt_x_cen) + cd12*(y-plt_y_cen)
	eta = cd21*(x-plt_x_cen) + cd22*(y-plt_y_cen)
	xi_arcs = xi*3600.0			# to arcs
	eta_arcs = eta*3600.0
	call trsteq (plt_ra_cen, plt_dec_cen, xi_arcs, eta_arcs, ra, dec)
end
