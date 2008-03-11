
size_t	num_lf
pointer	lf_ptrs
pointer	lf_ptrs0

common	/fmiocom/ num_lf, lf_ptrs, lf_ptrs0

