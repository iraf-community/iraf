#{ CL Script to Correct an Image for Distortion

# Call the GEOTRAN task

{

# List of input files
din = input

# List of output files
dout = output

# Transformation
ddata = database
dtran = transform

# Get output image format parameters

geotran (input=din, output=dout, database=ddata, transform=dtran,
	 geometry=geometry, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
	 xscale=xscale, yscale=yscale, ncols=ncols, nlines=nlines,
	 interpolant=interpolant, boundary=boundary, constant=constant,
	 fluxconserve=fluxconserve, xsample=xsample, ysample=ysample,
	 nxblock=nxblock, nyblock=nyblock, xin=INDEF, yin=INDEF, xout=INDEF,
	 yout=INDEF, xshift=INDEF, yshift=INDEF, xmag=INDEF, ymag=INDEF,
	 xrotation=INDEF, yrotation=INDEF)
}
