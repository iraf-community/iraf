# FLATTEN -- Flatten a spectrum and normalize to 1.0

procedure flatten (pix, npts)

real	pix[ARB]
int	npts

real	negsig, possig
int	norder, niter
pointer	sp, spec, cont

real 	clgetr()
int	clgeti()

begin
	# Allocate space for continuum fit and current spectrum
	# which gets destroyed by the procedure
	call smark (sp)
	call salloc (cont, npts, TY_REAL)
	call salloc (spec, npts, TY_REAL)
	call amovr (pix, Memr[spec], npts)

	# Get values for the iteration parameters

	negsig = clgetr ("negsig")	# Negative going clip level
	possig = clgetr ("possig")	# Positive going clip level
	niter  = clgeti ("niter")	# Number of iterations
	norder = clgeti ("norder")	# Order of fit

	call fcont (Memr[spec], npts, norder, negsig, possig, niter, Memr[cont])

	# Divide original spectrum by continuum fit
	call adivr (pix, Memr[cont], pix, npts)

	call sfree (sp)
end
