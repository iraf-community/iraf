# IMCMASK -- Common for IMCOMBINE mask interface.

int	mtype		# Mask type
int	mvalue		# Mask value
pointer	bufs		# Pointer to data line buffers
pointer	pms		# Pointer to array of PMIO pointers

common	/imcmask/ mtype, mvalue, bufs, pms
