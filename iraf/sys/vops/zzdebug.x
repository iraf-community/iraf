# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	xft

define	MAXPIX		4096

# XFT -- Test complex transform routines.

procedure xft

complex	x[MAXPIX]
int	npix, ntrip
long	seed
int	i, clgeti()
real	urand()

begin
	npix  = max(1, min(MAXPIX, clgeti ("npix")))
	ntrip = clgeti ("ntrip")
	seed = 1

	do i = 1, NPIX
	    x[i] = complex (urand(seed), urand(seed))

	do i = 1, ntrip {
	    call afftx (x, x, NPIX)
	    call aiftx (x, x, NPIX)
	}
end
