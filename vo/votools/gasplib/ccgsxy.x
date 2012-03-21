define	MAX_ITERATIONS		20
# CCGSXY --  Routine for computing X,Y pixel position from a given RA and Dec
#            on a GSSS plate.

procedure ccgsxy (plate_centre_ra,
     		  plate_centre_dec,
     		  plate_centre_x,
     		  plate_centre_y,
     		  x_pixel_size,
     		  y_pixel_size,
     		  plate_scale,
     		  amd_x,
     		  amd_y,              
     		  object_x,
     		  object_y,
     		  object_mag,
     		  object_col,
     		  object_ra,
     		  object_dec)


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
double	object_mag		#Object magnitude 
double	object_col		#Colour
double	object_ra		#Object RA (radians)	
double	object_dec		# "     Dec
double	xi_object		#Standard coords (arcsec)
double	eta_object		# "

double	x			#Position from centre (mm)
double	y			# "
double	delta_x			#correction to x
double	delta_y			#correction to y
int	n_iterations		#no.iterations used

double	f			#Function for x model
double	g			#Function for y model
double	fx			#Deriv. x model wrt x
double	gx			#Deriv. y model wrt x
double	fy			#Deriv. x model wrt y
double	gy			#deriv. y model wrt y

int	ierr			#Stats flag

begin

	#
	# Convert RA and Dec to standard coordinates
	#
	call treqst (plate_centre_ra, plate_centre_dec,
     	             object_ra, object_dec, xi_object, eta_object)
	
	# Iterate by Newtons method
	n_iterations = 0
	ierr = 0
	
	# Get initial estimate of x,y
	x = xi_object / plate_scale
	y = eta_object / plate_scale

    while (ierr == 0) {
	n_iterations=n_iterations+1

	#
	# X plate model
	#
	f = amd_x(1)*x 			     	   	+
     	    amd_x(2)*y 		   	   	   	+
     	    amd_x(3)   		       	    	   	+
     	    amd_x(4)*x*x 	       	    	   	+ 
     	    amd_x(5)*x*y	       	      	   	+
     	    amd_x(6)*y*y	       	      	   	+
     	    amd_x(7)*(x*x+y*y)	       		   	+
     	    amd_x(8)*x*x*x	       		   	+
     	    amd_x(9)*x*x*y	       	       	   	+
     	    amd_x(10)*x*y*y	       	       	   	+
     	    amd_x(11)*y*y*y	       	       	   	+
     	    amd_x(12)*x*(x*x+y*y)   		   	+
     	    amd_x(13)*x*(x*x+y*y)**2	       	   	+
     	    amd_x(14)*object_mag		   	+
     	    amd_x(15)*object_mag**2		   	+
     	    amd_x(16)*object_mag**3		   	+
     	    amd_x(17)*object_mag*x		   	+
     	    amd_x(18)*object_mag*(x*x+y*y)		+
      	    amd_x(19)*object_mag*x*(x*x+y*y)	       	+ 
     	    amd_x(20)*object_col			-
     	              xi_object
	#
	# Derivative of X model wrt x
	#
	fx = amd_x(1)	 			       	+
     		amd_x(4)*2.0*x 		    		       	+ 
     		amd_x(5)*y	       			       	+       	
     		amd_x(7)*2.0*x				       	+
     		amd_x(8)*3.0*x*x     			       	+
     		amd_x(9)*2.0*x*y			       	+
     		amd_x(10)*y*y		       		       	+
     		amd_x(12)*(3.0*x*x+y*y)	      		       	+
     		amd_x(13)*(5.0*x**4+6.0*x*x*y*y+y**4)  	       	+
     		amd_x(17)*object_mag		       	       	+
     		amd_x(18)*object_mag*2.0*x	       	       	+
      		amd_x(19)*object_mag*(3.0*x*x+y*y)	
	#
	# Derivative of X model wrt y
	#
	fy = amd_x(2)	 		      	       	+
     		amd_x(5)*x		       		       	+
     		amd_x(6)*2.0*y		       		       	+
     		amd_x(7)*2.0*y		  		       	+
     		amd_x(9)*x*x	   			       	+
     		amd_x(10)*x*2.0*y			       	+
     		amd_x(11)*3.0*y*y  			       	+
     		amd_x(12)*2.0*x*y		      	       	+
     		amd_x(13)*4.0*x*y*(x*x+y*y)		       	+
     		amd_x(18)*object_mag*2.0*y		       	+
      		amd_x(19)*object_mag*2.0*x*y		
	#
	# Y plate model
	#
	g =	amd_y(1)*y  				       	+
     		amd_y(2)*x 		       		       	+
     		amd_y(3)   		   		       	+
     		amd_y(4)*y*y 	     			       	+ 
     		amd_y(5)*y*x	      			       	+
     		amd_y(6)*x*x	      			       	+
     		amd_y(7)*(x*x+y*y)     			       	+
     		amd_y(8)*y*y*y				       	+
     		amd_y(9)*y*y*x		   		       	+
     		amd_y(10)*y*x*x		 		       	+
     		amd_y(11)*x*x*x		  		       	+
     		amd_y(12)*y*(x*x+y*y)	  		       	+
     		amd_y(13)*y*(x*x+y*y)**2	    	       	+
     		amd_y(14)*object_mag			       	+
     		amd_y(15)*object_mag**2			       	+
     		amd_y(16)*object_mag**3		  	       	+
     		amd_y(17)*object_mag*y		  	       	+
     		amd_y(18)*object_mag*(x*x+y*y)	   	       	+
      		amd_y(19)*object_mag*y*(x*x+y*y)	       	+
     		amd_y(20)*object_col		      	       	-
     		          eta_object
	#
	# Derivative of Y model wrt x
	#
	gx = amd_y(2)	 			       	+
     		amd_y(5)*y		 		       	+
     		amd_y(6)*2.0*x	    			       	+
     		amd_y(7)*2.0*x	      			       	+
     		amd_y(9)*y*y				       	+
     		amd_y(10)*y*2.0*x			       	+
     		amd_y(11)*3.0*x*x		   	       	+
     		amd_y(12)*2.0*x*y		  	       	+
     		amd_y(13)*4.0*x*y*(x*x+y*y)  		       	+
     		amd_y(18)*object_mag*2.0*x		       	+
      		amd_y(19)*object_mag*y*2.0*x		
	#
	# Derivative of Y model wrt y
	#
	gy = amd_y(1)				       	+
     		amd_y(4)*2.0*y 		   	       	       	+ 
     		amd_y(5)*x				       	+
     		amd_y(7)*2.0*y		     		       	+
     		amd_y(8)*3.0*y*y	      		       	+
     		amd_y(9)*2.0*y*x		       	       	+
     		amd_y(10)*x*x		     		       	+
     		amd_y(12)*3.0*y*y		       	       	+
     		amd_y(13)*(5.0*y**4+6.0*x*x*y*y+x**4) 	       	+
     		amd_y(17)*object_mag			       	+
     		amd_y(18)*object_mag*2.0*y	  	       	+
      		amd_y(19)*object_mag*(x*x+3.0*y*y)		

	delta_x = (-f * gy + g * fy) / (fx * gy - fy * gx)
	delta_y = (-g * fx + f * gx) / (fx * gy - fy * gx)
	x = x + delta_x
	y = y + delta_y

	if (dmax1 (dabs(delta_x), dabs(delta_y),
     	           dabs(f), dabs(g)) < 1.e-5) 
           ierr=1

	if (n_iterations == MAX_ITERATIONS) 
	   ierr=2

     } 
	 
	# Convert x,y from mm about plate centre to pixels

	object_x = (plate_centre_x - x*1000.0d0) / x_pixel_size
	object_y = (plate_centre_y + y*1000.0d0) / y_pixel_size

end
