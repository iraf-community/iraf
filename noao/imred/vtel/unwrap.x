include <mach.h>
include <imhdr.h>

define	MAXBADLINES	20		# maximum number of bad lines
define	BADTHRESH	1000		# threshold for bad lines
define	FIXWIDTH	20		# Width of average for fixline

# UNWRAP -- Filter an iraf image.  This filter checks for binary wraparound
# in IRAF images.  The algorithm is described in detail in the help page.
# The program accepts templates for both input and output image lists.

procedure t_unwrap()

char	image[SZ_FNAME]			# input image template
char	outimage[SZ_FNAME]		# output image template
int	threshold1			# threshold value for first unwrap
int	threshold2			# threshold value for second unwrap
int	wrapval1			# wrapvalue for first unwrap
int	wrapval2			# wrapvalue for second unwrap
int	cstart				# column to start on
int	step				# number of steps to perform
bool	verbose				# verbose flag

int	i, j 
int	listin, listout
int	length, nlines
int	badlines[MAXBADLINES]
int	diff, nbad
char	tempimage[SZ_FNAME]
pointer	im, imout, lgp, lgp2, lpp, cck, sp

bool	clgetb()
int	imtopenp(), imtlen(), imtgetim(), clgeti()
pointer	immap(), imgl2s(), impl2s()
errchk	immap, imgl2s, impl2s

begin
	# Get parameters from the CL.
	listin = imtopenp ("image")
	listout = imtopenp ("outimage")
	threshold1 = clgeti("threshold1")
	wrapval1 = clgeti("wrapval1")
	threshold2 = clgeti("threshold2")
	wrapval2 = clgeti("wrapval2")
	cstart = clgeti("cstart")
	step = clgeti("step")
	verbose = clgetb("verbose")

	if (verbose) {
	    call printf ("\n\nUNWRAP: ")
	    call printf ("threshold1 = %d\n")
	    call pargi (threshold1)
	    call printf ("\twrapval1   = %d\n")
	    call pargi (wrapval1)
	    call printf ("\tthreshold2 = %d\n")
	    call pargi (threshold2)
	    call printf ("\twrapval2   = %d\n")
	    call pargi (wrapval2)
	    call printf ("\tcstart     = %d\n")
	    call pargi (cstart)
	    call printf ("\tstep       = %d\n\n")
	    call pargi (step)
	    call flush (STDOUT)
	}

	# Check the number of elements.
	if (imtlen (listin) != imtlen (listout)) {
	    call imtclose (listin)
	    call imtclose (listout)
	    call error (1, "Wrong number of elements in the operand lists")
	}

	# Get the next images from the lists.
	while (imtgetim (listin, image, SZ_FNAME) != EOF) {
	    if (imtgetim (listout, outimage, SZ_FNAME) != EOF) {

	        if (verbose) {
		    # Write out about the input file name and output file name.
		    call printf ("\tUnwrapping %s into %s.  ")
		        call pargstr (image)
		        call pargstr (outimage)
		    call flush (STDOUT)
	        }

	        # Open images.
	        iferr {
	            im = immap (image, READ_WRITE, 0)
	        } then {
		    call eprintf ("Cannot open image %s.\n")
		        call pargstr (image)
		    next
	        }

	        call xt_mkimtemp (image, outimage, tempimage, SZ_FNAME)

	        iferr {
	            imout = immap (outimage, NEW_COPY, im)
	        } then {
		    call eprintf ("Cannot open image %s, (already exists?).\n")
		        call pargstr (outimage)
		    next
	        }

	        length = IM_LEN(im,1)
	        nlines = IM_LEN(im,2)

	        # Set up the column check array, then unwrap line by line.
	        call smark (sp)
	        call salloc (cck, nlines, TY_INT)
	        call amovks (0, Memi[cck], nlines)
	        do i = 1, nlines {
	            lgp = imgl2s (im, i)
	            lpp = impl2s (imout, i)
	            call unwrapline (Mems[lgp], Mems[lpp], cck, length,
			threshold1, wrapval1, threshold2, wrapval2, cstart,
			step, i)
	        }

	        # Step 5 is the final step. (fixline)
	        if (step == 5) {
	            # Analyze the column, check for wraps.
	            nbad = 0
	            do i = 2, nlines {
	    	        diff = Memi[cck+i-1] - Memi[cck+i-2]
	    	        if (abs(diff) > BADTHRESH) {
	    	            # Mark this line bad.
	    	            nbad = nbad + 1
	    	            if (nbad > MAXBADLINES)
	    		        break
	    	            badlines[nbad] = i
	    	        }
	            }
	        }

	        # If number bad lines <= than MAXBADLINES, fix em, else, quit.
	        if (nbad <= MAXBADLINES && nbad > 0) {
	            do i = 1, nbad {

	    	        # GET the lines above and below the bad line and PUT the
	    	        # bad line.  Then average the above and below lines and
			# save in the bad line.

	    	        if (badlines[i] != 1 && badlines[i] != nlines) {
	    	            if ((badlines[i+1] - badlines[i]) == 1) {
	                        lgp = imgl2s (imout, badlines[i]-1)
	                        lgp2 = imgl2s (imout, badlines[i]+1)
	                        lpp = impl2s (imout, badlines[i])
	    	                do j = 1, length {
	    		            Mems[lpp+j-1] = int((real(Mems[lgp+j-1]) +
	    		                real(Mems[lgp2+j-1]))/2. + .5)
	    		        }
	    	            } 
	    	        }
	            }
	        }

	        if (verbose) {
	            call printf ("number of bad lines = %d\n")
	                call pargi (nbad)
	            do i = 1, nbad {
	                call printf ("\tbadlines[%d] = %d\n")
	    	        call pargi (i)
	    	        call pargi (badlines[i])
	            }
		    call printf ("\n")
	            call flush (STDOUT)
	        }

	        # Unmap images.
	        call imunmap (im)
	        call imunmap (imout)
	        call xt_delimtemp (outimage, tempimage)
	        call sfree (sp)

	    } # End of if (not EOF)
	}  # End of while loop on input images

	call imtclose (listin)
	call imtclose (listout)
end


# UNWRAPLINE -- Unwrap a line of the image.

procedure unwrapline (line1, line2, cck, numpix, threshold1, wrapval1,
	threshold2, wrapval2, cstart, step, whichline)

short	line1[numpix]			# input line
short	line2[numpix]			# output line
pointer	cck				# pointer to array for column check
int	numpix				# number of pixels per line
int	threshold1			# unwrap threshold for first unwrap
int	wrapval1			# unwrap value for first unwrap
int	threshold2			# unwrap threshold for second unwrap
int	wrapval2			# unwrap value for second unwrap
int	cstart				# column to start on
int	step				# steps to complete
int	whichline			# which line this is we are unwrapping

pointer	tl1, tl2, tl3			# pointers of temporary arrays
pointer	sp				# stack pointer
int	i, diff, sum
short	wrap				# wrap number

begin
	# Mark the stack and allcoate the temporary arrays.
	call smark (sp)
	call salloc (tl1, numpix, TY_SHORT)
	call salloc (tl2, numpix, TY_SHORT)
	call salloc (tl3, numpix, TY_SHORT)

	# Initialize wrap.
	wrap = 0

	# Copy the input line into the output line and the temporary arrays.
	call amovs (line1, line2, numpix)
	call amovs (line1, Mems[tl1], numpix)
	call amovs (line1, Mems[tl2], numpix)
	call amovs (line1, Mems[tl3], numpix)

	# Check the image width, do various things if the image is too small.

	# Too small for anything.
	if (numpix <= 4) {
	    call sfree (sp)
	    return
	}

	# Too small for step 5 (fixline).
	if (numpix <= FIXWIDTH && step == 5)
	    step = 4

	# Unwrap1 (step 1).
	Mems[tl1+cstart-1] = line1[cstart]
	do i = cstart+1, numpix {
	    diff = line1[i] - line1[i-1]
	    if (diff < -threshold1)
		wrap = wrap + 1
	    if (diff > threshold1)
		wrap = wrap - 1

	    Mems[tl1+i-1] = line1[i] + wrap * wrapval1
	}
	if (step == 1) {
	    call amovs (Mems[tl1], line2, numpix)
	    call sfree (sp)
	    return
	}

	# If the user wants it, step 2 (dif).
	do i = cstart, numpix
	    Mems[tl2+i-1] = Mems[tl1+i-1] - Mems[tl1+i-2]

	if (step == 2) {
	    call amovs (Mems[tl2], line2, numpix)
	    call sfree (sp)
	    return
	}

	# If the user wants it, step 3 (unwrap2).
	wrap = 0
	line2[cstart] = Mems[tl2+cstart-1]
	do i = cstart+1, numpix {
	    diff = Mems[tl2+i-1] - Mems[tl2+i-2]
	    if (diff < -threshold2)
	        wrap = wrap + 1
	    if (diff > threshold2)
		wrap = wrap - 1

	    line2[i] = Mems[tl2+i-1] + wrap * wrapval2
	}
	if (step == 3) {
	    call sfree (sp)
	    return
	}

	# If the user wants it, step 4 (reconstruct).
	do i = cstart, numpix
	    line2[i] = line2[i-1] + line2[i]
	
	if (step == 4) {
	    call sfree (sp)
	    return
	}

	# Again, if the user wants it, save data for step 5, (fixline).
	sum = 0
	do i = numpix-FIXWIDTH+1, numpix
	    sum = sum + line2[i]
	Memi[cck+whichline-1] = int(real(sum)/real(FIXWIDTH) + .5)

	call sfree (sp)
end
