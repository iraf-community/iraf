# Create test data if needed.

procedure mkdoecslit ()
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

	if (!access ("Bdemoflat." // envget ("imtype"))) {
	    print ("Creating example demoflat ...")
	    mkechelle ("Bdemoflat", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="slit", width=20., scattered=10., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=20000., temperature=5700., lines="",
		nrandom=0, peak=5.0, sigma=0.1, seed=2, >& "dev$null")
	    mknoise ("Bdemoflat", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=no,
		seed=5, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}

	if (!access ("Bdemoobj1." // envget ("imtype"))) {
	    print ("Creating example demoobj1 ...")
	    mkechelle ("Bdemoobj1", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="gaussian", width=4., scattered=25., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=500., temperature=7700., lines="",
		nrandom=100, peak=-0.2, sigma=0.3, seed=1, >& "dev$null")
	    mkechelle ("Bdemoobj1", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="slit", width=20., scattered=10., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=200., temperature=5700., lines="",
		nrandom=20, peak=5.0, sigma=0.1, seed=2, >& "dev$null")
	    mknoise ("Bdemoobj1", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=1, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}

	if (!access ("Bdemoobj2." // envget ("imtype"))) {
	    print ("Creating example demoobj2 ...")
	    mkechelle ("Bdemoobj2", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="gaussian", width=4., scattered=25., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=10., z=no, continuum=500., temperature=7700., lines="",
		nrandom=100, peak=-0.2, sigma=0.3, seed=1, >& "dev$null")
	    mkechelle ("Bdemoobj2", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoobj.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="slit", width=20., scattered=10., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=200., temperature=5700., lines="",
		nrandom=20, peak=5.0, sigma=0.1, seed=2, >& "dev$null")
	    mknoise ("Bdemoobj2", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=4, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}

	if (!access ("Bdemostd." // envget ("imtype"))) {
	    print ("Creating example demostd ...")
	    mkechelle ("Bdemostd", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demostd.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="gaussian", width=4., scattered=25., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=500., temperature=10000., lines="",
		nrandom=0, peak=-0.5, sigma=0.5, seed=3, >& "dev$null")
	    mkechelle ("Bdemostd", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demostd.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="slit", width=20., scattered=10., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=200., temperature=5700., lines="",
		nrandom=20, peak=5.0, sigma=0.1, seed=2, >& "dev$null")
	    mknoise ("Bdemostd", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=2, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}

	if (!access ("Bdemoarc." // envget ("imtype"))) {
	    print ("Creating example demoarc ...")
	    mkechelle ("Bdemoarc", yes, ncols=100, nlines=256, norders=21,
		title="Artificial Echelle Spectrum",
		header="demos$demoarc.dat", list=no, make=yes,
		comments=no, xc=INDEF, yc=INDEF, pixsize=0.027,
		profile="slit", width=20., scattered=10., f=590., gmm=31.6,
		blaze=63., theta=69., order=112, wavelength=5007.49,
		dispersion=2.61, cf=590., cgmm=226., cblaze=4.53,
		ctheta=-11.97, corder=1, cwavelength=6700., cdispersion=70.,
		rv=0., z=no, continuum=20., temperature=0.,
		lines="mkexamples$ecthorium.dat", nrandom=0, peak=-0.5,
		sigma=0.05, seed=1, >& "dev$null")
	    mknoise ("Bdemoarc", output="", ncols=512, nlines=512, title="",
		header="", background=0., gain=1., rdnoise=10., poisson=yes,
		seed=3, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., pa=0., comments=no)
	}
end
