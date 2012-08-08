# Create test data if needed.

procedure mkdofoe ()
begin

	artdata
	artdata.nxc = 5
	artdata.nyc = 5
	artdata.nxsub = 10
	artdata.nysub = 10
	artdata.nxgsub = 5
	artdata.nygsub = 5
	artdata.dynrange = 100000.
	artdata.psfrange = 10.
	artdata.ranbuf = 0

	if (!access ("demoobj." // envget ("imtype"))) {
	    print ("Creating example demoobj ...")
	    mkechelle ("demoobj", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=50, yc=50.1, pixsize=0.027,
		profile="gaussian", width=4., scattered=25., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=500., temperature=7700., lines="",
		nrandom=100, peak=-0.2, sigma=0.3, seed=1, >& "dev$null")
	    mkechelle ("demoobj", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=60, yc=51.6, pixsize=0.027,
		profile="gaussian", width=4., scattered=0., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=4.95, temperature=0.,
		lines="mkexamples$ecthorium.dat", nrandom=0, peak=-0.5,
		sigma=0.05, seed=1, >& "dev$null")
	    mknoise ("demoobj", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=1, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}

	if (!access ("demoflat." // envget ("imtype"))) {
	    print ("Creating example demoflat ...")
	    mkechelle ("demoflat", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=50, yc=50.2, pixsize=0.027,
		profile="gaussian", width=4., scattered=25., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=1000., temperature=5700., lines="",
		nrandom=0, peak=-0.2, sigma=0.3, seed=1, >& "dev$null")
	    mkechelle ("demoflat", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=60, yc=51.7, pixsize=0.027,
		profile="gaussian", width=4., scattered=25., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=990., temperature=7700., lines="",
		nrandom=0, peak=-0.2, sigma=0.3, seed=1, >& "dev$null")
	    mknoise ("demoflat", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=2, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}

	if (!access ("demoarc." // envget ("imtype"))) {
	    print ("Creating example demoarc ...")
	    mkechelle ("demoarc", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoarc.dat", list=no, make=yes,
		comments=no, xc=50, yc=50, pixsize=0.027,
		profile="gaussian", width=4., scattered=0., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=10., temperature=0.,
		lines="mkexamples$ecthorium.dat", nrandom=0, peak=-0.5,
		sigma=0.05, seed=1, >& "dev$null")
	    mkechelle ("demoarc", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoarc.dat", list=no, make=yes,
		comments=no, xc=60, yc=51.5, pixsize=0.027,
		profile="gaussian", width=4., scattered=0., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=9.9, temperature=0.,
		lines="mkexamples$ecthorium.dat", nrandom=0, peak=-0.5,
		sigma=0.05, seed=1, >& "dev$null")
	    mknoise ("demoarc", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=3, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}
end
