pointer	clamp		# pointer to the clamping vector
pointer	normal		# pointer to the normal matrix
pointer	resid		# pointer to the residuals vector
pointer	deriv		# pointer to the derivatives vector
pointer	result		# pointer to the current results vector
pointer	old_result	# pointer to the previous results vector

common /pfitcom/ clamp, normal, resid, deriv, result, old_result
