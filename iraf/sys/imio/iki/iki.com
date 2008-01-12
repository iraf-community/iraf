# IKI.COM -- Image Kernel Interface global common.

int	k_nkernels, k_nextn, k_sbufused, k_defimtype, k_inherit
int	k_kernel[MAX_IMEXTN], k_extn[MAX_IMEXTN], k_pattern[MAX_IMEXTN]
int	k_table[LEN_KERNEL,MAX_KERNELS]
char	k_kname[SZ_KNAME,MAX_KERNELS]
char	k_sbuf[SZ_IKISBUF]

common	/ikicom/ k_nkernels, k_nextn, k_sbufused, k_defimtype, k_inherit,
	k_kernel, k_extn, k_pattern, k_table, k_kname, k_sbuf
