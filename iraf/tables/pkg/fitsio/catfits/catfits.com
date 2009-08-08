
# FITS reader common

size_t	len_record		# Length of FITS records in bytes

# Option flags
int	print_ext		# Print extension if set
int	long_header		# Print a long header (FITS header cards)
int	short_header		# Print a short header (see help file for dtils)
bool    main_header		# TRUE is main FITS header.

common	/catfitscom/ print_ext, len_record, long_header, short_header, main_header
