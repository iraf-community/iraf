pointer	py		# pointer to the reference equation values
pointer	pyfit		# pointer to the fit equation values
pointer	pa		# pointer to the parameters to be fit
pointer	pdelta		# pointer to the parameter increments
pointer	pda		# pointer to the temporary parameter increments
pointer	palpha		# pointer to the accumulated matrix
pointer	pbeta		# pointer to the accumulated right side vector
pointer	pik		# pointer to working index array for matrix inversion
pointer	pjk		# pointer to working index array for matrix inversion

pointer	pyerr		# pointer to the error equation values
pointer	pafit		# pointer to the temporary fit array

common /invertcom / py, pyfit, pa, pdelta, pda, palpha, pbeta, pik, pjk
common /invertcom / pyerr, pafit
