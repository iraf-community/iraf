# MSRESP1D -- Make a aperture response spectrum using a flat field
# and a throughput file or image.

procedure msresp1d (flat, throughput, apreference, response)

string	flat			{prompt="Flat field spectrum"}
string	throughput		{prompt="Throughput file or image"}
string	apreference		{prompt="Aperture reference spectrum"}
string	response		{prompt="Response spectrum"}

bool	recenter = no		{prompt="Recenter apertures?"}
bool	edit = yes		{prompt="Edit/review apertures?"}
bool	trace = no		{prompt="Trace spectra?"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	fitflat = no		{prompt="Fit and ratio flat field spectrum?"}
bool	interactive = yes	{prompt="Fit flat field interactively?"}
string	function = "spline3"	{prompt="Fitting function",
				 enum="spline3|legendre|chebyshev|spline1"}
int	order = 20		{prompt="Fitting function order", min=1}

begin
	file	flat2d, skyflat2d, apref, resp
	file	temp1, temp2, log1, log2
	string	imtype, mstype
	int	i, n, ap, naxis
	real	respval

	flat2d = flat
	skyflat2d = throughput
	apref = apreference
	resp = response
	temp1 = mktemp ("tmp")
	temp2 = mktemp ("tmp")

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype
	n = strlen (imtype)

	# Check required input and output.
 	if (resp == "" || resp == flat2d || resp == skyflat2d)
 	    error (1, "Bad response image name")
	if (flat2d == "" && skyflat2d == "")
	    error (1, "No flat field or throughput specified")

	if (flat2d != "") {
	    i = strlen (flat2d)
	    if (i > n && substr (flat2d, i-n+1, i) == imtype)
		flat2d = substr (flat2d, 1, i-n)
	    if (!access (flat2d // imtype))
		error (1, "Flat field spectrum not found - " // flat2d)
	}
	if (skyflat2d != "") {
	    i = strlen (skyflat2d)
	    if (i > n && substr (skyflat2d, i-n+1, i) == imtype)
	        skyflat2d = substr (skyflat2d, 1, i-n)
	    if (!access (skyflat2d // imtype)) {
		if (!access (skyflat2d))
		    error (1,
			"Throughput file or image not found - " // skyflat2d)

		if (flat2d == "") {
		    i = strlen (apref)
		    if (i > n && substr (apref, i-n+1, i) == imtype)
			apref = substr (apref, 1, i-n)
		    if (!access (apref // imtype))
			error (1, "Aperture reference image required")
		}
	    }
	}

	# Set logging
	tee.append = yes
	if (logfile == "")
	    log1 = "dev$null"
	else
	    log1 = logfile
	if (verbose)
	    log2 = "STDOUT"
	else
	    log2 = "dev$null"

	# If using a flat field extract it if necessary and possibly fit it
	# and ratio the individual apertures by an overall smooth function

	if (flat2d != "") {
	    if (!access (flat2d // mstype)) {
		print ("Extract flat field ", flat2d) | tee (log1)
		if (flat2d != apref)
		    apall (flat2d, output=resp, references=apref, profiles="",
			interactive=yes, find=yes, recenter=recenter,
			resize=no, edit=edit, trace=trace, fittrace=yes,
			extract=yes, review=no, background="none", clean=clean,
			extras=no)
		else
		    apall (flat2d, output=resp, references=apref, profiles="",
			interactive=no, find=yes, recenter=no, resize=no,
			edit=edit, trace=no, fittrace=yes, extract=yes,
			review=no, background="none", clean=clean, extras=no)
	    } else
		imcopy (flat2d//".ms", resp, verbose=no)

	    if (fitflat) {
		print ("Fit and ratio flat field ", flat2d) | tee (log1)
		blkavg (resp, temp1, option="average", b1=1, b2=10000)
		imcopy (temp1//"[*,1]", temp1, verbose=no)
		fit1d (temp1, temp1, "fit", axis=1, interactive=interactive,
		    sample="*", naverage=1, function=function, order=order,
		    low_reject=0., high_reject=0., niterate=1, grow=0.,
		    graphics="stdgraph")
		sarith (resp, "/", temp1, resp, w1=INDEF, w2=INDEF,
		    apertures="", beams="", apmodulus=0, reverse=no,
		    ignoreaps=yes, format="multispec", renumber=no, offset=0,
		    clobber=yes, merge=no, errval=0, verbose=no)
		imdelete (temp1, verify=no)
	    }
	}

	# If using a throughput image extract it if necessary.
	# Apply it to the flat field if given and otherwise only
	# compute the throughput through each aperture.

	if (skyflat2d != "") {
	    if (access (skyflat2d // imtype)) {
		if (!access (skyflat2d // mstype)) {
		    print ("Extract throughput image ", skyflat2d) | tee (log1)
		    apall (skyflat2d, output=temp1, references=apref,
			profiles="", interactive=yes, find=yes,
			recenter=recenter, resize=no, edit=edit,
			trace=trace, fittrace=yes, extract=yes, review=no,
			background="none", clean=clean, extras=no)
		    temp2 = temp1
		} else
		    temp2 = skyflat2d // ".ms"

		if (flat2d != "") {
		    print ("Correct flat field to throughput image") |
			tee (log1)
		    sarith (temp2, "/", resp, temp1, w1=INDEF, w2=INDEF,
			apertures="", beams="", apmodulus=0, reverse=no,
			ignoreaps=no, format="multispec", renumber=no, offset=0,
			clobber=yes, merge=no, errval=0, verbose=no)
		    fit1d (temp1, temp1, type="fit", axis=1,
			interactive=no, sample="*", naverage=1,
			function="legendre", order=1, niterate=0)
		    sarith (resp, "*", temp1, resp, w1=INDEF, w2=INDEF,
			apertures="", beams="", apmodulus=0, reverse=no,
			ignoreaps=yes, format="multispec", renumber=no,
			offset=0, clobber=yes, merge=no, errval=0, verbose=no)
		    imdelete (temp1, verify=no)
		} else {
		    print ("Compute aperture throughput from image") |
			tee (log1)
		    fit1d (temp2, resp, type="fit", axis=1,
			interactive=no, sample="*", naverage=1,
			function="legendre", order=1, niterate=0)
		    if (temp2 == temp1)
			imdelete (temp2, verify=no)
		}

	    # If a flat field and throughput file are used scale the average
	    # flat field in each aperture to those values

	    } else if (flat2d != "") {
		print ("Correct flat field with throughput file ", skyflat2d) |
		    tee (log1)
		fit1d (resp, resp, type="ratio", axis=1,
		    interactive=no, sample="*", naverage=1,
		    function="legendre", order=1, niterate=0)

		list = skyflat2d
		while (fscan (list, ap, respval) != EOF) {
		    sarith (resp, "*", respval, resp, w1=INDEF, w2=INDEF,
		        apertures=ap, beams="", apmodulus=0, reverse=no,
		        ignoreaps=no, format="multispec", renumber=no,
			offset=0, clobber=yes, merge=yes, errval=0.,
			verbose=no)
		}
		list = ""

	    # If only a throughput file is given create the response from the
	    # aperture reference and set the aperture response to the specified
	    # values.

	    } else {
		print ("Set aperture throughput using ", skyflat2d) | tee (log1)
		if (!access (apref // mstype)) {
		    apall (apref, output=resp, references=apref,
			profiles="", interactive=no, find=yes, recenter=no,
			resize=no, edit=edit, trace=no, fittrace=yes,
			extract=yes, review=no, background="none", clean=no,
			extras=no)
		    sarith (resp, "replace", "0", resp, w1=INDEF, w2=INDEF,
			apertures="", beams="", apmodulus=0, reverse=no,
			ignoreaps=no, format="multispec", renumber=no,
			offset=0, clobber=yes, merge=yes, errval=0., verbose=no)
		} else
		    sarith (apref//".ms", "replace", "0", resp, w1=INDEF,
			w2=INDEF, apertures="", beams="", apmodulus=0,
			reverse=no, ignoreaps=no, format="multispec",
			renumber=no, offset=0, clobber=yes, merge=yes,
			errval=0., verbose=no)

		list = skyflat2d
		while (fscan (list, ap, respval) != EOF) {
		    sarith (resp, "replace", respval, resp, w1=INDEF, w2=INDEF,
		        apertures=ap, beams="", apmodulus=0, reverse=no,
		        ignoreaps=no, format="multispec", renumber=no,
			offset=0, clobber=yes, merge=yes, errval=0.,
			verbose=no)
		}
		list = ""
	    }
	}

	# The final response is normalized to overall unit mean and the
	# average aperture response is printed.

	print ("Create the normalized response ", resp) | tee (log1)
	bscale (resp, resp, bzero="0.", bscale="mean", section="",
	    step=1, upper=INDEF, lower=INDEF, verbose=yes) | tee (log1, >log2)
	blkavg (resp, temp1, option="average", b1=10000, b2=1)
	print ("Average aperture response:") | tee (log1, >log2)
	hselect (temp1, "naxis", yes, > temp2)
	list = temp2; ap = fscan (list, naxis)
	if (naxis == 1)
	    listpixels (temp1) | tee (log1, >log2)
	else
	    listpixels (temp1//"[1,*]") | tee (log1, >log2)
	list = ""; delete (temp2, verify=no)
	imdelete (temp1, verify=no)
end
