# IMALIGN - a procedure to register a set of images.

procedure imalign (images, coords)
begin
	bool	shifts_found = no

	string	l_images, l_coords
	string	tmpfile, outfile, shiftfile, trimsect, tmp, junk
	int	x1, x2, y1, y2
	real	xshift, yshift
	struct	line

	tmpfile = mktemp ("tmp$ia_tmp.")
	outfile = mktemp ("tmp$ia_tmp.")
	shiftfile = mktemp ("tmp$ia_tmp.")

	l_images = images
	l_coords = coords

	# write the output names to `outfile'
	sections (l_images, option="fullname", > tmpfile)
	list = tmpfile
	while (fscan (list, tmp) != EOF)
	    print (prefix//tmp, >> outfile)
	list = ""; delete (tmpfile, ver-, >& "dev$null")

	imcentroid (l_images, l_coords, reference=reference, shifts=shifts,
	    boxsize=boxsize, bigbox=bigbox, negative=negative,
	    background=background, lower=lower, upper=upper,
	    niterate=niterate, tolerance=tolerance, verbose=verbose, >& tmpfile)

	if (verbose)
	    type (tmpfile)

	if (! shiftimages)
	    goto finish

	# read the shifts
	list = tmpfile
	while (fscan (list, line) != EOF)
	    if (stridx ("!", line) != 0) {
		shifts_found = yes
		break
	    }

	if (shifts_found)
	    while (fscan (list, junk, xshift, junk, yshift, junk) == 5)
		print (xshift, " ", yshift, >> shiftfile)
	else
	    error (1, "No shifts were calculated.")

	print ("\n# Shifting images:")
	imshift (l_images, "@"//outfile, shifts_file=shiftfile,
	    interp_type=interp_type, boundary_type=boundary_type,
	    constant=constant)

	if (! trimimages)
	    goto finish

	# read and correct the trim section
	if (fscan (list, x1, x2, y1, y2) == 4) {

	    # correct for boundary extension "contamination"
	    if (interp_type == "poly3")
		{ x1 += 1; x2 -= 1; y1 += 1; y2 -= 1 }
	    else if (interp_type == "poly5" || interp_type == "spline3")
		{ x1 += 2; x2 -= 2; y1 += 2; y2 -= 2 }

	    if (1 <= x1 && x1 <= x2 && 1 <= y1 && y1 <= y2) {
		trimsect = "["// x1 //":"// x2 //","// y1 //":"// y2 //"]"

		list = outfile; delete (tmpfile, ver-, >& "dev$null")
		while (fscan (list, tmp) != EOF)
		    print (tmp//trimsect, >> tmpfile)

		print ("# Trimming images:  corrected section = ", trimsect)
		imcopy ("@"//tmpfile, "@"//outfile, verbose-)

	    } else
		print ("Images not trimmed!  No overlap region.")

	} else
	    print ("Images not trimmed!  Problem with the trim section.")

finish:	list = ""
	delete (tmpfile, ver-, >& "dev$null")
	delete (outfile, ver-, >& "dev$null")
	delete (shiftfile, ver-, >& "dev$null")
end
