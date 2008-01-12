# KPNOFOCUS -- KPNO Focus measuring task.
# This is customized to the header keywords provided by ICE.

procedure kpnofocus (images)

string	images		{prompt="List of focus images"}
int	frame = 1	{prompt="Display frame to use"}
real	level = 0.5	{prompt="Measurement level (fraction or percent)"}
string	size = "FWHM"	{prompt="Size to display",
			 enum="Radius|FWHM|GFWHM|MFWHM"}
real	scale = 1.	{prompt="Pixel scale"}
real	radius = 5.	{prompt="Measurement radius (pixels)"}
real	sbuffer = 5.	{prompt="Sky buffer (pixels)"}
real	swidth = 5.	{prompt="Sky width (pixels)"}
real	saturation = INDEF {prompt="Saturation level"}
bool	ignore_sat = no {prompt="Ignore objects with saturated pixels?"}
int	iterations = 2	{prompt="Number of radius adjustment iterations",
			 min=1}
string	logfile = "logfile" {prompt="Logfile"}

begin
	string	ims

	ims = images

#print ("\nKPNOFOCUS: Estimate best focus from ICE focus images.")
#print ("  The stars to mark are from the first focus exposure which are the")
#print ("  top ones in each sequence unless the display is flipped.")
#print ("  Specifically, they are those with the largest y value in the")
#print ("  sequence and closest to the double step gap.\n")
print ("Mark the top star (in unflipped display).")

	starfocus (ims, focus="FOCSTART", fstep="FOCSTEP",
	    nexposures="FOCNEXPO", step="FOCSHIFT", direction="-line",
	    gap="beginning", coords="markall", wcs="logical", display=yes,
	    frame=frame, imagecur="", graphcur="", level=level, size=size,
	    beta=INDEF, scale=scale, radius=radius, sbuffer=sbuffer,
	    swidth=swidth, saturation=saturation, ignore_sat=ignore_sat,
	    xcenter=INDEF, ycenter=INDEF, logfile=logfile,
	    iterations=iterations)
end
