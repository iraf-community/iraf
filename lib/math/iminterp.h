# IMINTERP.H -- User definitions for the image interpolation package.

define	II_FUNCTIONS   "|nearest|linear|poly3|poly5|spline3|sinc|lsinc|drizzle|"
define	II_NEAREST	1	# nearest neighbour
define	II_LINEAR	2	# linear
define	II_POLY3	3	# 3rd order polynomial
define	II_POLY5	4	# 5th order polynomial
define	II_SPLINE3	5	# cubic spline
define	II_SINC		6	# sinc
define	II_LSINC	7	# look-up table sinc
define	II_DRIZZLE	8	# drizzle
define	II_NTYPES	8

# 2D interpolation definitions.

define	II_BFUNCTIONS  "|nearest|linear|poly3|poly5|spline3|sinc|lsinc|drizzle|"
define	II_BINEAREST	1		# nearest neighbour
define	II_BILINEAR	2		# bilinear
define	II_BIPOLY3	3		# bicubic polynomial 
define	II_BIPOLY5	4		# biquintic polynomial
define	II_BISPLINE3	5		# bicubic spline
define	II_BISINC	6		# bisinc
define	II_BILSINC	7		# look-up table bisinc
define	II_BIDRIZZLE	8		# drizzle
define	II_NTYPES2D	8

# Define types for asigeti

define	II_ASITYPE	1	# interpolant type
define	II_ASINSAVE	2	# size of array to be saved
define	II_ASINSINC	3	# size of the sinc convolution
define	II_ASIBADVAL	4	# bad pixel value for drizzle

# Define types for msigeti

define	II_MSITYPE	1	# interpolant type
define	II_MSINSAVE	2	# size of array to be saved
define	II_MSINSINC	3	# size of array to be saved
define	II_MSIBADVAL	4	# bad pixel value for drizzle

# Boundary types for arbpix

define	II_BOUNDARYEXT	1	# boundary extension
define	II_NBOUND	1	# number of boundary types
