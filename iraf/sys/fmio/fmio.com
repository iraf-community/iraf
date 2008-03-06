
size_t	num_lf
pointer	lf_ptrs

common	/fmiocom/ num_lf, lf_ptrs

