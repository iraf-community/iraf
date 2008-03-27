
size_t	num_fit
pointer	fit_ptrs
pointer	fit_ptrs0

common	/fxfcom/ num_fit, fit_ptrs, fit_ptrs0

