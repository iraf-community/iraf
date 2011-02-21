#{
#  BENCH.CL
#
#  A rough benchmark script for testing IRAF on different systems.
#

procedure bench ()

begin

    string dum1, dum2, time1, time2
    real t0, t1, t2, t3, t4, tf, tsec, tmake, tproc, tcomb, tmed
    

    # Load the needed tasks (if necessary) BEFORE timer starts.
    if (!defpac("noao.artdata")) {
    	artdata
    }

    reset imdir = "HDR$pix/"

    
    print(" ")
    time() | scan (dum1, time1, dum2)
    print("Bench started at ",time1)
    time() | scan (dum1, t0, dum2)
    print(" ")
    
    print ("=====> Making images...")
    print ("         Making zero...")
    mknoise ("zimage",
        output="", title="zero image", ncols=2048, nlines=2048,
        header="artdata$stdheader.dat", background=0., gain=1., rdnoise=2.,
        poisson=no, seed=1, cosrays="", ncosrays=100, energy=30000., radius=0.5,
        pa=0., comments=yes)
    
    print ("         Making flat...")
    mknoise ("fimage",
        output="", title="flat image", ncols=2048, nlines=2048,
        header="artdata$stdheader.dat", background=30000., gain=1., rdnoise=1.,
        poisson=no, seed=1, cosrays="", ncosrays=0, energy=30000., radius=0.5, 
        pa=0., comments=yes)
    
    print ("         Making 3 objs...")
    mknoise ("o1image",
        output="", title="obj image", ncols=2048, nlines=2048,
        header="artdata$stdheader.dat", background=500., gain=1., rdnoise=5.,
        poisson=no, seed=1, cosrays="", ncosrays=100, energy=30000., radius=0.5,
        pa=0., comments=yes)
    
    mknoise ("o2image",
        output="", title="obj image", ncols=2048, nlines=2048,
        header="artdata$stdheader.dat", background=500., gain=1., rdnoise=5.,
        poisson=no, seed=1, cosrays="", ncosrays=100, energy=30000., radius=0.5,
        pa=0., comments=yes)
    
    mknoise ("o3image",
        output="", title="obj image", ncols=2048, nlines=2048,
        header="artdata$stdheader.dat", background=500., gain=1., rdnoise=5.,
        poisson=no, seed=1, cosrays="", ncosrays=100, energy=30000., radius=0.5,
        pa=0., comments=yes)
    
    time() | scan (dum1, t1, dum2)
    

    # PROC SIMULATION SECTION
    #
    # Flat normalization is usually done realtime in memory in ccdproc,
    # but writing the image might be similar to the temp image in ccdproc
    
    print(" ")
    print(" ")
    print ("=====>   Normalizing flat...")
    imarith ("fimage",
        "/", "30000.", "fimage", title="", divzero=1., hparams="", pixtype="",
        calctype="", verbose=yes, noact=no)
    
    print(" ")
    print(" ")
    print ("=====>   Subtracting zero from 3 images...")
    imarith ("o1image,o2image,o3image",
        "-", "zimage", "o1image,o2image,o3image", title="", 
        divzero=0., hparams="", pixtype="", calctype="", verbose=yes, noact=no)
    
    print(" ")
    print(" ")
    print ("=====>   Dividing flat into 3 images...")
    imarith ("o1image,o2image,o3image",
        "/", "fimage", "o1image,o2image,o3image", title="", divzero=0., 
        hparams="", pixtype="", calctype="", verbose=yes, noact=no)
    
    # END of PROC SIMULATION SECTION
    
    time() | scan (dum1, t2, dum2)
    
    print(" ")
    print(" ")
    printf ("=====>   Combining 3 images...")
    imcombine ("o1image,o2image,o3image",
        "ocimage", sigma="", logfile="STDOUT", combine="average",
        reject="crreject", project=no, outtype="real", offsets="none",
        masktype="none", maskvalue=0., blank=0., scale="none", zero="none",
        weight="none", statsec="", expname="", lthreshold=INDEF, 
	hthreshold=INDEF, nlow=1, nhigh=1, nkeep=1, mclip=yes, lsigma=3., 
	hsigma=3., rdnoise="5", gain="1", snoise="0.", sigscale=0.1, 
	pclip=-0.5, grow=0)
    
    time() | scan (dum1, t3, dum2)
    
    print(" ")
    print(" ")
    print ("=====>   Median filtering image...")
    median ("ocimage",
        "omimage", 9, 9, zloreject=INDEF, zhireject=INDEF, boundary="nearest",
        constant=0., verbose=yes)
    
    time() | scan (dum1, t4, dum2)
    
    print(" ")
    print ("=====>   Deleting all images...")
    imdel ("o1image,o2image,o3image,ocimage,omimage,zimage,fimage",
        yes, verify=no, default_acti=yes)
    
    time() | scan (dum1, tf, dum2)
    time() | scan (dum1, time2, dum2)
    print(" ")
    print("Bench started at ",time1)
    print("Bench  ended at  ",time2)
    print(" ")
    
    tsec  = (tf - t0) * 3600.
    tmake = (t1 - t0) * 3600.
    tproc = (t2 - t1) * 3600.
    tcomb = (t3 - t2) * 3600.
    tmed  = (t4 - t3) * 3600.
    

    printf ("Total execution time = %7.1f seconds\n",tsec)
    printf (" Total time    Make 5 imgs    Proc 3 imgs    Combine 3 imgs    Median 1 img\n")
    printf ("  %7.1f        %7.1f       %7.1f         %7.1f           %7.1f\n",
	tsec, tmake, tproc, tcomb, tmed)
    
end
