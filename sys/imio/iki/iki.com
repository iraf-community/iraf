# IKI.COM -- Image Kernel Interface global common.

int	k_nkernels
int	k_table[LEN_KERNEL,MAX_KERNELS]

common	/ikicom/ k_nkernels, k_table
