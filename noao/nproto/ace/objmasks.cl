# OBJMASK -- Make object masks from image data.

procedure objmasks ()

begin
	detect (images, objmasks=objmasks, masks=masks, omtype=omtype,
	    skys=skys, sigmas=sigmas,
	    extnames=extnames, logfiles=logfiles, blkstep=blkstep,
	    blksize=blksize, convolve=convolve, hsigma=hsigma,
	    lsigma=lsigma, hdetect=hdetect, ldetect=ldetect,
	    neighbors=neighbors, minpix=minpix, ngrow=ngrow, agrow=agrow,
	    exps=objmasks1.exps, gains=objmasks1.gains,
	    catalogs=objmasks1.catalogs, catdefs=objmasks1.catdefs,
	    dodetect=objmasks1.dodetect, dosplit=objmasks1.dosplit,
	    dogrow=objmasks1.dogrow, doevaluate=objmasks1.doevaluate,
	    skytype=objmasks1.skytype, fitstep=objmasks1.fitstep,
	    fitblk1d=objmasks1.fitblk1d, fithclip=objmasks1.fithclip,
	    fitlclip=objmasks1.fitlclip, fitxorder=objmasks1.fitxorder,
	    fityorder=objmasks1.fityorder, fitxterms=objmasks1.fitxterms,
	    blknsubblks=objmasks1.blknsubblks,
	    updatesky=objmasks1.updatesky, sigavg=objmasks1.sigavg,
	    sigmax=objmasks1.sigmax, bpval=objmasks1.bpval,
	    splitmax=objmasks1.splitmax, splitstep=objmasks1.splitstep,
	    splitthresh=objmasks1.splitthresh, sminpix=objmasks1.sminpix,
	    ssigavg=objmasks1.ssigavg, ssigmax=objmasks1.ssigmax,
	    magzero=objmasks1.magzero)

end
