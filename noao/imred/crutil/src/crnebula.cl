# CRNEBULA -- Cosmic ray cleaning for images with fine nebular structure.

procedure crnebula (input, output)

file    input           {prompt="Input image"}
file	output		{prompt="Output image"}
file    crmask          {prompt="Cosmic ray mask"}
file	residual	{prompt="Residual median image"}
file	rmedresid	{prompt="Residual ring median image"}
real    var0 = 1.       {prompt="Variance coefficient for DN^0 term", min=0.}
real	var1 = 0.	{prompt="Variance coefficient for DN^1 term", min=0.}
real	var2 = 0.	{prompt="Variance coefficient for DN^2 term", min=0.}
real    sigmed = 3      {prompt="Sigma clip factor for residual median"}
real    sigrmed = 3     {prompt="Sigma clip factor for residual ring median"}

int     mbox = 5        {prompt="Median box size"}
real    rin = 1.5       {prompt="Inner radius for ring median"}
real    rout = 6        {prompt="Outer radius for ring median"}
bool    verbose = no    {prompt="Verbose"}

begin
        file    in, out, med, sig, resid, rmed
        struct  expr

        # Query once for query parameters.
        in = input
        out = output

        # Check on output images.
	if (out != "" && imaccess (out))
            error (1, "Output image already exists ("//out//")")
        if (crmask != "" && imaccess (crmask))
            error (1, "Output mask already exists ("//crmask//")")
	if (residual != "" && imaccess (residual))
            error (1, "Output residual image already exists ("//residual//")")
	if (rmedresid != "" && imaccess (rmedresid))
            error (1,
	       "Output ring median difference already exists ("//rmedresid//")")

        # Create median results.
	med = mktemp ("cr")
	sig = mktemp ("cr")
	resid = residual
	if (resid == "")
	    resid = mktemp ("cr")
	if (verbose)
	    printf ("Creating CRMEDIAN results\n")
	crmedian (in, "", crmask="", median=med, sigma=sig, residual=resid,
	    var0=var0, var1=var1, var2=var2, lsigma=100., hsigma=sigmed,
	    ncmed=mbox, nlmed=mbox, ncsig=25, nlsig=25)

        # Create ring median filtered image.
        rmed = mktemp ("cr")
	rmedian (in, rmed, rin, rout, ratio=1., theta=0., zloreject=INDEF,
	    zhireject=INDEF, boundary="wrap", constant=0., verbose=verbose)

	# Create output images.
	if (rmedresid != "") {
	    printf ("(a-b)/c\n") | scan (expr)
	    imexpr (expr, rmedresid, med, rmed, sig, dims="auto",
		intype="auto", outtype="real", refim="auto", bwidth=0,
		btype="nearest", bpixval=0., rangecheck=yes, verbose=no,
		exprdb="none")
	    imdelete (rmed, verify-)
	    imdelete (sig, verify-)
	    if (out != "") {
		if (verbose)
		    printf ("Create output image %s\n", out)
		printf ("((a<%.3g)||(abs(b)>%.3g)) ? c : d\n",
		    sigmed, sigrmed) | scan (expr)
		imexpr (expr, out, resid, rmedresid, in, med, dims="auto",
		    intype="auto", outtype="auto", refim="auto", bwidth=0,
		    btype="nearest", bpixval=0., rangecheck=yes, verbose=no,
		    exprdb="none")
	    }
	    if (crmask != "") {
		if (verbose)
		    printf ("Create cosmic ray mask %s\n", crmask)
		printf ("((a<%.3g)||(abs(b)>%.3g)) ? 0 : 1\n",
		    sigmed, sigrmed) | scan (expr)
		set imtype = "pl"
		imexpr (expr, crmask, resid, rmedresid, dims="auto",
		    intype="auto", outtype="short", refim="auto", bwidth=0,
		    btype="nearest", bpixval=0., rangecheck=yes, verbose=no,
		    exprdb="none")
	    }
	    imdelete (med, verify-)
	} else {
	    if (out != "") {
		if (verbose)
		    printf ("Create output image %s\n", out)
		printf ("((a<%.3g)||(abs((b-c)/d)>%.3g)) ? e : b\n",
		    sigmed, sigrmed) | scan (expr)
		imexpr (expr, out, resid, med, rmed, sig, in, dims="auto",
		    intype="auto", outtype="auto", refim="auto", bwidth=0,
		    btype="nearest", bpixval=0., rangecheck=yes, verbose=no,
		    exprdb="none")
	    }
	    if (crmask != "") {
		if (verbose)
		    printf ("Create cosmic ray mask %s\n", crmask)
		printf ("((a<%.3g)||(abs((b-c)/d)>%.3g)) ? 0 : 1\n",
		    sigmed, sigrmed) | scan (expr)
		set imtype = "pl"
		imexpr (expr, crmask, resid, med, rmed, sig, dims="auto",
		    intype="auto", outtype="short", refim="auto", bwidth=0,
		    btype="nearest", bpixval=0., rangecheck=yes, verbose=no,
		    exprdb="none")
	    }
	    imdelete (med, verify-)
	    imdelete (sig, verify-)
	    imdelete (rmed, verify-)
	}
	if (residual == "")
	    imdelete (resid, verify-)
end
