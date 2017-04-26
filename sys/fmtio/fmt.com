# Printf common block.

int	fd			# output file
int	ip			# pointer to next char in format string
int	width, decpl		# field width, number of decimal places
int	col			# output column
int	left_justify		# left or right justify output in field
int	radix			# output radix
int	fmt_state		# current state of FPRFMT (gets a format)
int	ofile_type		# type of output file
int	format_char 		# format type character (bcdefghmorstuxz#*)
char	fill_char		# filler char for rt. justification
char	format[SZ_OBUF]		# format string
char	obuf[SZ_OBUF]		# for formatting output

common	/fmtcom/  fd,ip,width,decpl,col,left_justify,radix,fmt_state,
		  ofile_type,format_char,fill_char,format,obuf
