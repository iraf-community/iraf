# PDS reader common

int	data_type		# Output data type

# Option flags
int	make_image		# Create an IRAF image
int	long_header		# Print a long PDSheader
int	short_header		# Print a short header (Title and size)
int	tenbit			# Specify tenbit data
int	ninetrack		# Specify ninetrack tape

common /pdsreadcom/ data_type, make_image, long_header, short_header, tenbit,
		    ninetrack
