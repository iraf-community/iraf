# KEYSELECT.COM -- Global variables used by keyselect

#* HISTORY *
#* B.Simon	12-Mar-92	Original

common	/global/  hasgroup, img

bool	hasgroup	# true if image has group parameters
pointer	img		# image descriptor
