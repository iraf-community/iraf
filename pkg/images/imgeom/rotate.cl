# ROTATE -- Rotate an image by calling the GEOTRAN task with the appropriate
# parameters.

procedure rotate (input, output, rotation, xin, yin, xout, yout, ncols, nlines,
	interpolant, boundary, constant, nxblock, nyblock, verbose)

string	input
string	output
real	rotation
real	xin
real	yin
real	xout
real	yout
real	ncols
real	nlines
string	interpolant
string	boundary
real	constant
int	nxblock
int	nyblock
bool	verbose


begin
	# Declare local variables.
	string	tinput, toutput
	real	trotation

	# Get the parameters.
	tinput = input
	toutput = output
	trotation = rotation

	# Call GEOTRAN.
	geotran (input=tinput, output=toutput, database="", xrotation=trotation,
	    yrotation=trotation, xin=xin, yin=yin, xout=xout, yout=yout,
	    xshift=INDEF, yshift=INDEF, xmin=1.0, xmax=ncols, ymin=1.0,
	    ymax=nlines, xscale=1.0, yscale=1.0, ncols=INDEF, nlines=INDEF,
	    xmag=INDEF, ymag=INDEF, interpolant=interpolant,
	    boundary=boundary, constant=constant, xsample=1., ysample=1.,
	    fluxconserve=no, nxblock=nxblock, nyblock= nyblock, verbose=verbose)
end

