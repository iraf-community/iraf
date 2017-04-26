# CCGSEQ -- Routine for computing RA and Dec for a given X,Y pixel 
#           position on a GSSS plate. 

procedure ccgseq (plate_centre_ra, plate_centre_dec, plate_centre_x,
     		  plate_centre_y, x_pixel_size, y_pixel_size,
     		  plate_scale, amd_x, amd_y, object_x, object_y,
     		  object_mag, object_col, object_ra, object_dec)

double	plate_centre_ra		#Plate Right Ascension (radians)
double	plate_centre_dec	#Plate Declination     (radians)
double	plate_centre_x		#Position used in soln.(microns)
double	plate_centre_y		# "
double	x_pixel_size		#Scan pixel size (microns)
double	y_pixel_size		# "
double	plate_scale		#Plate scale (arcsec/mm)
double	amd_x[20]		#Plate model coefficients
double	amd_y[20]		# "
double	object_x		#Pixel position for object
double	object_y		# "
double	object_mag		#Magnitude 
double	object_col		#Colour
double	object_ra		#Object RA (radians)	
double	object_dec		# "     Dec

double	x			#Position from centre (mm)
double	y			# "
double	xi_object		#Standard coords (arcsec)
double	eta_object		# "
double  p1,p2,p3,p4

begin
	# Convert x,y from pixels to mm measured from the plate centre

	x = (plate_centre_x - object_x * x_pixel_size) / 1000.0d0
	y = (object_y * y_pixel_size - plate_centre_y) / 1000.0d0

	# Compute standard coordinates from x,y and plate model coefficients

	p1 = 	  amd_x(1)	*x		       		+
     		  amd_x(2)	*y		       		+
     		  amd_x(3)			       		+
     		  amd_x(4)	*x**2		       		+
     		  amd_x(5)	*x*y		       		+
     		  amd_x(6)	*y**2		       		

        p2 = 	  amd_x(7)	*(x**2+y**2)	       		+
     		  amd_x(8)	*x**3    	       		+
     		  amd_x(9) 	*x**2*y		       		+
     		  amd_x(10)	*x*y**2		       		+
     		  amd_x(11)  	*y**3		       		

     	p3 = 	  amd_x(12)	*x*(x**2+y**2)	       		+
     		  amd_x(13)	*x*(x**2+y**2)**2  		+
     		  amd_x(14)	*object_mag			+
     		  amd_x(15)	*object_mag**2			+
     		  amd_x(16)	*object_mag**3			

     	p4 =	  amd_x(17)	*object_mag*x			+
     		  amd_x(18)	*object_mag*(x**2+y**2)	        +
     		  amd_x(19)	*object_mag*x*(x**2+y**2)	+
     		  amd_x(20)	*object_col

	xi_object = p1 + p2 + p3 + p4

	p1 =       amd_y(1)	*y		       		+
     		   amd_y(2)	*x		       		+
     		   amd_y(3)			       		+
     		   amd_y(4)	*y**2		       		+
     		   amd_y(5)	*x*y		       		+
     		   amd_y(6)	*x**2				

     	p2 = 	   amd_y(7)	*(x**2+y**2) 	 		+
     		   amd_y(8)	*y**3    	       		+
     		   amd_y(9) 	*y**2*x				+
     		   amd_y(10)	*y*x**2				+
     		   amd_y(11)  	*x**3				

     	p3 = 	   amd_y(12)	*y*(x**2+y**2)			+
     		   amd_y(13)	*y*(x**2+y**2)**2		+
     		   amd_y(14)	*object_mag			+
     		   amd_y(15)	*object_mag**2			+
     		   amd_y(16)	*object_mag**3			

     	p4 = 	   amd_y(17)	*object_mag*y			+
     		   amd_y(18)	*object_mag*(x**2+y**2)		+
     		   amd_y(19)	*object_mag*y*(x**2+y**2)	+
     		   amd_y(20)	*object_col  

	eta_object = p1 + p2 + p3 + p4

	call trsteq (plate_centre_ra, plate_centre_dec,
     	             xi_object, eta_object, object_ra, object_dec)

end
