# IMINTERP.H -- User definitions for image interpolation package.

define	II_FUNCTIONS	"|nearest|linear|poly3|poly5|spline3|sinc|"
define	II_NEAREST	1	# nearest neighbour
define	II_LINEAR	2	# linear
define	II_POLY3	3	# 3rd order polynomial
define	II_POLY5	4	# 5th order polynomial
define	II_SPLINE3	5	# cubic spline
define	II_SINC		6	# sinc
define	II_NTYPES	6

# Boundary types for arbpix

define	II_BOUNDARYEXT	1	# boundary extension
define	II_NBOUND	1	# number of boundary types

# 2D interpolation definitions.

define	II_BFUNCTIONS	"|nearest|linear|poly3|poly5|spline3|"
define	II_BINEAREST	1		# nearest neighbour
define	II_BILINEAR	2		# bilinear
define	II_BIPOLY3	3		# bicubic polynomial 
define	II_BIPOLY5	4		# biquintic polynomial
define	II_BISPLINE3	5		# bicubic spline
define	II_NTYPES2D	5

# Define types for asiget

define	II_ASITYPE	1	# interpolant type
define	II_ASINSAVE	2	# size of array to be saved

# Define types for msiget

define	II_MSITYPE	1	# interpolant type
define	II_MSINSAVE	2	# size of array to be saved
