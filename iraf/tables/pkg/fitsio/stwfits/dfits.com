# FITS reader common
 
# Option flags
bool	form_header
 
# Tables
pointer log_fd			# log file descriptor
int	nkeywords		# number of keywords (and formats) stored
pointer	key_table[MAX_TABLE]	# keywords
pointer fmt_table[MAX_TABLE]	# formats
char	opt_table[MAX_TABLE]	# format options
 
common	/dfitscom/ log_fd, form_header, nkeywords, key_table, fmt_table, opt_table
