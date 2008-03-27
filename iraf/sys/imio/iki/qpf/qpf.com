
size_t	num_qpf
pointer	qpf_ptrs
pointer	qpf_ptrs0

common	/qpfcom/ num_qpf, qpf_ptrs, qpf_ptrs0

