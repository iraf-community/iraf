# MKFIBERS - Make multifiber examples

procedure mkfibers (image)

file	image				{prompt="Image name"}
string	type="object"			{prompt="Object type",
		 enum="object|objnosky|sky|flat|henear|ehenear|ohenear|mercury"}
file	fibers=""			{prompt="Fiber data file"}
string	title="Multifiber artificial image"	{prompt="Title"}
file	header="artdata$stdhdr.dat"	{prompt="Header keyword file"}
int	ncols=400			{prompt="Number of columns"}
int	nlines=512			{prompt="Number of lines"}
real	wstart=4210.			{prompt="Starting wavelength"}
real	wend=7362.			{prompt="Ending wavelength"}
int	seed=1				{prompt="Noise seed"}

begin
	int	i, ap, beam
	real	ar
	file	out, obj, sky, arc, dat
	string	htype, imtype

	out = image
	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	if (access (out) || access (out//imtype))
	    return

	print ("Creating image ", out, " ...")
	
	obj = mktemp ("art")
	sky = mktemp ("art")
	arc = mktemp ("art")
	dat = mktemp ("art")
	
	list = fibers
	if (type == "object") {		# Object spectrum + sky
	    htype = "object"
	    mk1dspec (obj, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=1000., slope=0.,
		temperature=7000., lines="", nlines=50, peak=-0.5,
		profile="gaussian", gfwhm=24, seed=2, comments=no, header="")
	    mk1dspec (sky, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=1000., slope=0.,
		temperature=5800., lines="", nlines=20, peak=1.,
		profile="gaussian", gfwhm=12, seed=1, comments=no, header="")
	    imarith (obj, "+", sky, obj, verbose=no, noact=no)
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=0.8, slope=0.,
		temperature=0., lines="mkexamples$henear2.dat",
		profile="gaussian", gfwhm=14, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, continuum=20,
		slope=0., temperature=0., lines="", nlines=0,
		comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF) {
		if (beam == 0)
		    print (sky, " ", line, >> dat)
		else if (beam == 1)
		    print (obj, " ", line, >> dat)
		else if (beam == 2)
		    print (arc, " ", line, >> dat)
	    }
	} else if (type == "objnosky") {	# Object spectrum
	    htype = "object"
	    mk1dspec (obj, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=1000., slope=0.,
		temperature=7000., lines="", nlines=50, peak=-0.5,
		profile="gaussian", gfwhm=24, seed=2, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=0.8, slope=0.,
		temperature=0., lines="mkexamples$henear2.dat",
		profile="gaussian", gfwhm=14, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, continuum=20,
		slope=0., temperature=0., lines="", nlines=0,
		comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF) {
		if (beam == 1)
		    print (obj, " ", line, >> dat)
		else if (beam == 2)
		    print (arc, " ", line, >> dat)
	    }
	} else if (type == "sky") {	# Sky only
	    htype = "object"
	    mk1dspec (sky, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=1000., slope=0.,
		temperature=5800., lines="", nlines=20, peak=1.,
		profile="gaussian", gfwhm=12, seed=1, comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF)
		print (sky, " ", line, >> dat)
	} else if (type == "flat") {	# Flat field
	    htype = "flat"
	    mk1dspec (obj, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=10000., slope=0.,
		temperature=8000., lines="", nlines=0, comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF)
		print (obj, " ", line, >> dat)
	} else if (type == "henear") {	# HE-NE-AR
	    htype = "comp"
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=0.8, slope=0.,
		temperature=0., lines="mkexamples$henear2.dat",
		profile="gaussian", gfwhm=14, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, continuum=20,
		slope=0., temperature=0., lines="", nlines=0,
		comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF)
		print (arc, " ", line, >> dat)
	} else if (type == "ehenear") {	# HE-NE-AR Even fibers
	    htype = "comp"
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=0.8, slope=0.,
		temperature=0., lines="mkexamples$henear2.dat",
		profile="gaussian", gfwhm=14, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, continuum=20,
		slope=0., temperature=0., lines="", nlines=0,
		comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF) {
		if (mod (ap, 2) == 0) {
		    print (arc, " ", line, >> dat)
		}
	    }
	} else if (type == "ohenear") {	# HE-NE-AR Odd fibers
	    htype = "comp"
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=0.8, slope=0.,
		temperature=0., lines="mkexamples$henear2.dat",
		profile="gaussian", gfwhm=14, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, continuum=20,
		slope=0., temperature=0., lines="", nlines=0,
		comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF) {
		if (mod (ap, 2) == 1) {
		    print (arc, " ", line, >> dat)
		}
	    }
	} else if (type == "mercury") {	# Emission lines
	    htype = "comp"
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, ncols=nlines, naps=1,
		wstart=wstart, wend=wend, continuum=0., slope=0.,
		temperature=0., lines="", nlines=30, peak=10000.,
		profile="gaussian", gfwhm=7, seed=i, comments=no, header="")
	    mk1dspec (arc, output="", ap=1, rv=0., z=no, continuum=20,
		slope=0., temperature=0., lines="", nlines=0,
		comments=no, header="")
	    while (fscan (list, ap, beam, line) != EOF) {
		print (arc, " ", line, >> dat)
	    }
	}
	list = ""
	
	mk2dspec (out, output="", model=dat, ncols=ncols, nlines=nlines,
	    title=title, header=header, comments=no)
	hedit (out, "imagetyp", htype, update=yes, add=no, delete=no,
	    show=no, verify=no)
	    
	
	mknoise (out, output="", background=0., gain=1., rdnoise=3.,
	    poisson=yes, seed=seed, cosrays="", ncosrays=0, energy=30000.,
	    radius=0.5, ar=1., pa=0., comments=no)
	
	imdelete (obj, verify=no, >& "dev$null")
	imdelete (sky, verify=no, >& "dev$null")
	imdelete (arc, verify=no, >& "dev$null")
	delete (dat, verify=no, >& "dev$null")
end
