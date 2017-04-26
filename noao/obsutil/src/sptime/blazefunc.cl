procedure blazefunc (grating, order)

file	grating			{prompt="Grating"}
int	order = INDEF		{prompt="Order"}
real	camcolangle = 45.	{prompt="Camera-grating-collimator angle (deg)"}
string	search = "spectimedb$KPNO/Gratings" {prompt="Directory search list\n"}	

string	title = ""		{prompt="Title"}
real	w1 = 3000.		{prompt="Lower wavelength to plot"}
real	w2 = 12000.		{prompt="Upper wavelength to plot\n"}

real	x1 = 3000.		{prompt="Left graph wavelength"}
real	x2 = 12000.		{prompt="Right graph wavelength"}
real	y1 = -5.		{prompt="Bottom graph efficiency"}
real	y2 = 105.		{prompt="Top graph efficiency"}
string	ltype = "1"		{prompt="Line type"}
string	color = "1"		{prompt="Color"}
bool	append = no		{prompt="Append?"}

struct	*fd

begin
	file	tmp1, tmp2
	real	x, y

	tmp1 = mktemp ("tmp$iraf")
	tmp2 = mktemp ("tmp$iraf")

	# Spectrograph.
	print ("# area = 1", >> tmp1)
	print ("# scale = 1", >> tmp1)
	print ("# fl = 1", >> tmp1)
	print ("# ndisp = 2000", >> tmp1)
	print ("# pixsize = 1", >> tmp1)

	sptime (time=1., maxexp=3600., sn=25., spectrum="blackbody",
	    sky="", sensfunc="none", airmass=1., seeing=1., phase=0.,
	    temperature=6000., index=0., refwave=INDEF, refflux=10.,
	    funits="AB", abjohnson="none", wave=INDEF, order=order,
	    xorder=INDEF, width=1., length=1., diameter=1.,
	    inoutangle=camcolangle, xinoutangle=INDEF, xbin=1, ybin=1,
	    search=search, spectrograph=tmp1, filter="none",
	    filter2="none", disperser=grating, xdisperser="none",
	    fiber="none", telescope=tmp1, adc="none", collimator=tmp1,
	    corrector="none", camera=tmp1, detector=tmp1, aperture=tmp1,
	    extinction="", gain=1., rdnoise=0., dark=0., skysub="none",
	    nskyaps=10, output="disperser", list=tmp2, graphics="",
	    interactive=no, nw=1000, > "dev$null")

	delete (tmp1, verify-)

	fd = tmp2
	while (fscan (fd, x, y) != EOF) {
	    if (nscan() != 2)
		next
	    if (x < w1 || x > w2)
		next
	    print (x, y, >> tmp1)
	}
	fd = ""

	if (title == "")
	    title = grating

	graph (tmp1, wx1=x1, wx2=x2, wy1=y1, wy2=y2, wcs="logical", axis=1,
	    transpose=no, pointmode=no, marker="box", szmarker=0.005,
	    ltypes=ltype, colors=color, logx=no, logy=no, box=yes,
	    ticklabels=yes, xlabel="Wavelength (A)", ylabel="Efficiency (%)",
	    xformat="wcsformat", yformat="", title=title,
	    lintran=no, p1=0., p2=0., q1=0., q2=1., vx1=0., vx2=0., vy1=0.,
	    vy2=0., majrx=5, minrx=5, majry=7, minry=5, overplot=no,
	    append=append, device="stdgraph", round=no, fill=yes)

	delete (tmp1, verify-)
	delete (tmp2, verify-)
end
