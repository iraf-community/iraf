# IMALIGN - Register a set of images using the results of the IMCENTROID,
# and the IMSHIFT, and IMCOPY tasks.

procedure imalign (input, reference, coords, output)

begin
	bool	shifts_found, trim_found
	string	tmpfile, outfile, shiftfile, trimsect, tmp, junk
	string	l_input, l_reference, l_coords, l_output
	int	x1, x2, y1, y2
	real	xshift, yshift
	struct	line

	# Set up some temporary files.
	tmpfile = mktemp ("tmp$ia_tmp.")
	outfile = mktemp ("tmp$ia_tmp.")
	shiftfile = mktemp ("tmp$ia_tmp.")

	# Get the required parameters.
	l_input = input
	l_reference = reference
	l_coords = coords
	l_output = output

	# Write the output names to outfile.
	sections (l_output, option="fullname", > outfile)

	# Compute the centers and relative shifts.
	imcentroid (l_input, l_reference, l_coords, shifts=shifts,
	    boxsize=boxsize, bigbox=bigbox, negative=negative,
	    background=background, lower=lower, upper=upper,
	    niterate=niterate, tolerance=tolerance, maxshift=maxshift,
	    verbose=verbose, >& tmpfile)

	# Print the centering results on the screen?
	if (verbose)
	    type (tmpfile)

	# Shift the images.
	if (shiftimages) {

	    # Read the shifts.
            shifts_found = no
 	    list = tmpfile
	    while (fscan (list, line) != EOF) {
	        tmp = substr (line, 2, 7)
	        if (tmp == "Shifts") {
		    shifts_found = yes
		    break
	        }
	    }

	    # Decode the shifts.
	    if (shifts_found)
	        while (fscan (list, junk, xshift, junk, yshift, junk) == 5)
		    print (xshift, " ", yshift, >> shiftfile)
	    else
	        error (1, "No shifts were calculated.")

	    # Shift the images.
	    print ("\n# Shifting images:\n")
	    imshift (l_input, "@"//outfile, shifts_file=shiftfile,
	             interp_type=interp_type, boundary_type=boundary_type,
	             constant=constant)

	    # Trim the images.
	    if (trimimages) {

	        # Check for vignetting.
	        trim_found = no
	        while (fscanf (list, "%s = [%d:%d,%d:%d]", line, x1, x2,
                      y1, y2) != EOF) {
	            tmp = substr (line, 2, 5)
	            if (tmp == "Vign") {
		        print ("Images not trimmed ! Vignetting is present.")
		        trim_found = no
		        break
	            } else if (tmp == "Trim") {
		        trim_found = yes
		        break
	            }
	        }

		# Trim the images.
	        if (!trim_found) {
	            print ("Images not trimmed ! Trim section is undefined.")
	        } else {

		    # Correct for boundary extension "contamination".
	            if (interp_type == "poly3") {
	                x1 += 1; x2 -= 1; y1 += 1; y2 -= 1
                    } else if (interp_type == "poly5" ||
                               interp_type == "spline3") {
	                x1 += 2; x2 -= 2; y1 += 2; y2 -= 2
                    }

	            if (1 <= x1 && x1 <= x2 && 1 <= y1 && y1 <= y2) {
	                trimsect = "["//x1//":"//x2//","//y1//":"//y2//"]"

	                list = outfile; delete (tmpfile, ver-, >& "dev$null")
	                while (fscan (list, tmp) != EOF)
		            print (tmp//trimsect, >> tmpfile)

	                print ("# Trimming images:  corrected section = ",
                               trimsect)
	                imcopy ("@"//tmpfile, "@"//outfile, verbose-)

	            } else {
	                print ("Images not trimmed !  No overlap region.")
                    }
                }
            }
        }

        list = ""
	delete (tmpfile, ver-, >& "dev$null")
	delete (outfile, ver-, >& "dev$null")
	delete (shiftfile, ver-, >& "dev$null")
end
