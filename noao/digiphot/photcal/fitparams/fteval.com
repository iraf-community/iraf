# Fitting function and derivatives evaluation common

pointer	eqcode		# equation code
pointer	dercode		# equation derivative codes
pointer	xpcode		# x plotting equation code
pointer	ypcode		# y plotting equation code

common	/ftevalcom/ eqcode, dercode, xpcode, ypcode
