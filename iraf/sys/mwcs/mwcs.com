# MWCS common.  Used for things that are global and don't change, i.e.,
# the WCS function drivers.

int	fn_nfn				# number of defined functions
int	fn_table[LEN_FN,MAX_FN]		# function table
char	fn_names[SZ_FNNAME,MAX_FN]	# function names

common	/mwcscom/ fn_nfn, fn_table, fn_names
