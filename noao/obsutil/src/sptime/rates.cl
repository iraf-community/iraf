procedure rates (grating, filter, order)

file	grating				{prompt="Grating"}
file	filter				{prompt="Filter"}
int	order = INDEF			{prompt="Order"}
real	wave = INDEF			{prompt="Central wavelength"}
file	spectrograph = "rcspec"		{prompt="Spectrograph"}
real	width = 10.			{prompt="Slit width"}

string	title = ""			{prompt="Title"}
real	w1 = 3000.			{prompt="Lower wavelength to plot"}
real	w2 = 12000.			{prompt="Upper wavelength to plot\n"}

real	x1 = 3000.			{prompt="Left graph wavelength"}
real	x2 = 12000.			{prompt="Right graph wavelength"}
real	y1 = -5.			{prompt="Bottom graph efficiency"}
real	y2 = 105.			{prompt="Top graph efficiency"}
string	ltype = "1"			{prompt="Line type"}
string	color = "1"			{prompt="Color"}
bool	append = no			{prompt="Append?"}

struct	*fd

begin
	string	search
	file	tmp1, tmp2
	real	x, y

	tmp1 = mktemp ("tmp$iraf")
	tmp2 = mktemp ("tmp$iraf")

	search = "spectimedb$,sptimeKPNO$"
	search = search // ",sptimeKPNO$Spectrographs"
	search = search // ",sptimeKPNO$Gratings"
	search = search // ",sptimeKPNO$Grisms"
	search = search // ",sptimeKPNO$Filters/RCCRYO"
	search = search // ",sptimeKPNO$Filters/GCAM"

	sptime (time=1., maxexp=3600., sn=25., spectrum="fnu_power",
	    sky="none", sensfunc="none", airmass=1., seeing=1.5, phase=0.,
	    temperature=6000., index=0., refwave=INDEF, refflux=10.,
	    funits="AB", abjohnson="none", wave=wave, order=order,
	    xorder=INDEF, width=width, length=INDEF, diameter=INDEF,
	    inoutangle=INDEF, xinoutangle=INDEF, xbin=1, ybin=1,
	    search=search, spectrograph=spectrograph, filter=filter,
	    filter2="none", disperser=grating, xdisperser="none",
	    fiber="none", telescope="", adc="", collimator="",
	    corrector="", camera="", detector="", aperture="",
	    extinction="", gain=INDEF, rdnoise=INDEF, dark=INDEF, skysub="none",
	    nskyaps=10, output="rate", list=tmp2, graphics="",
	    interactive=no, nw=1000, > "dev$null")

	fd = tmp2
	while (fscan (fd, x, y) != EOF) {
	    if (nscan() != 2)
		next
	    if (x < w1 || x > w2)
		next
	    print (x, y, >> tmp1)
	}
	fd = ""

	graph (tmp1, wx1=x1, wx2=x2, wy1=y1, wy2=y2, wcs="logical", axis=1,
	    transpose=no, pointmode=no, marker="box", szmarker=0.005,
	    ltypes=ltype, colors=color, logx=no, logy=no, box=yes,
	    ticklabels=yes, xlabel="Wavelength (A)", ylabel="Rate",
	    xformat="wcsformat", yformat="", title=title,
	    lintran=no, p1=0., p2=0., q1=0., q2=1., vx1=0., vx2=0., vy1=0.,
	    vy2=0., majrx=7, minrx=3, majry=7, minry=3, overplot=no,
	    append=append, device="stdgraph", round=no, fill=yes)

	delete (tmp1, verify-)
	delete (tmp2, verify-)
end
