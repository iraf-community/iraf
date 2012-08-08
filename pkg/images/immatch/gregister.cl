# GREGISTER -- Register a list of images by calling the GEOTRAN task with the
# appropriate parameters.

procedure gregister (input, output, database, transforms, geometry, xmin, xmax,
	ymin, ymax, xscale, yscale, ncols, nlines, xsample, ysample,
	interpolant, boundary, constant, fluxconserve, nxblock, nyblock,
	verbose)

string	input
string	output
string	database
string	transforms
string	geometry
real	xmin
real	xmax
real	ymin
real	ymax
real	xscale
real	yscale
int	ncols
int	nlines
real	xsample
real	ysample
string	interpolant
string	boundary
real	constant
bool	fluxconserve
int	nxblock
int	nyblock
bool	verbose

begin
	# Declare local variables
	string din, dout, ddata, dtran

	# Get the parameters.
	din = input
	dout = output
	ddata = database
	dtran = transforms

	# Call GEOTRAN.
	geotran (input=din, output=dout, database=ddata, transforms=dtran,
	    geometry=geometry, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
	    xscale=xscale, yscale=yscale, ncols=ncols, nlines=nlines,
	    interpolant=interpolant, boundary=boundary, constant=constant,
	    fluxconserve=fluxconserve, xsample=xsample, ysample=ysample,
	    nxblock=nxblock, nyblock=nyblock, xin=INDEF, yin=INDEF, xout=INDEF,
	    yout=INDEF, xshift=INDEF, yshift=INDEF, xmag=INDEF, ymag=INDEF,
	    xrotation=INDEF, yrotation=INDEF, verbose=verbose)
end
