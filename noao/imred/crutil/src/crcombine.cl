# CRCOMBINE -- Reject cosmic rays by combining multiple exposures.

procedure crcombine (input, output)

begin
	imcombine (input, output, logfile=logfile, combine=combine,
	    reject=reject, scale=scale, zero=zero, statsec=statsec,
	    lsigma=lsigma, hsigma=hsigma, rdnoise=rdnoise, gain=gain,
	    grow=grow, headers=headers, bpmasks=bpmasks, rejmasks=rejmasks,
	    nrejmasks=nrejmasks, expmasks=expmasks, sigmas=sigmas,
	    project=project, outtype=outtype, outlimits=outlimits,
	    offsets=offsets, masktype=masktype, maskvalue=maskvalue,
	    blank=blank, weight=weight, expname=expname,
	    lthreshold=lthreshold, hthreshold=hthreshold, nlow=nlow,
	    nhigh=nhigh, nkeep=nkeep, mclip=mclip, snoise=snoise,
	    sigscale=sigscale, pclip=pclip)
end
