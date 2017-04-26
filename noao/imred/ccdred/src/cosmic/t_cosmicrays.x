include	<error.h>
include <imhdr.h>
include <imset.h>
include	<math/gsurfit.h>
include	<gset.h>
include	<pkg/gtools.h>
include	"crlist.h"

# T_COSMICRAYS -- Detect and remove cosmic rays in images.
# A list of images is examined for cosmic rays which are then replaced
# by values from neighboring pixels.  The output image may be the same
# as the input image.  This is the top level procedure which manages
# the input and output image data.  The actual algorithm for detecting
# cosmic rays is in CR_FIND.

procedure t_cosmicrays ()

int	list1			# List of input images to be cleaned
int	list2			# List of output images
int	list3			# List of output bad pixel files
real	threshold		# Detection threshold
real	fluxratio		# Luminosity boundary for stars
int	npasses			# Number of cleaning passes
int	szwin			# Size of detection window
bool	train			# Use training objects?
pointer	savefile		# Save file for training objects
bool	interactive		# Examine cosmic ray parameters?
char	ans			# Answer to interactive query

int	nc, nl, c, c1, c2, l, l1, l2, szhwin, szwin2
int	i, j, k, m, ncr, ncrlast, nreplaced, flag
pointer	sp, input, output, badpix, str, gp, gt, im, in, out
pointer	x, y, z, w, sf1, sf2, cr, data, ptr

bool	clgetb(), ccdflag(), streq(), strne()
char	clgetc()
int	imtopenp(), imtlen(), imtgetim(), clpopnu(), clgfil(), clgeti()
real	clgetr()
pointer	immap(), impl2r(), imgs2r(), gopen(), gt_init()
errchk	immap, impl2r, imgs2r
errchk	cr_find, cr_examine, cr_replace, cr_plot, cr_badpix

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (badpix, SZ_FNAME, TY_CHAR)
	call salloc (savefile, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the task parameters.  Check that the number of output images
	# is either zero, in which case the cosmic rays will be removed
	# in place, or equal to the number of input images.

	list1 = imtopenp ("input")
	list2 = imtopenp ("output")
	i = imtlen (list1)
	j = imtlen (list2)
	if (j > 0 && j != i)
	    call error (0, "Input and output image lists do not match")

	list3 = clpopnu ("badpix")
	threshold = clgetr ("threshold")
	fluxratio = clgetr ("fluxratio")
	npasses = clgeti ("npasses")
	szwin = clgeti ("window")
	train = clgetb ("train")
	call clgstr ("savefile", Memc[savefile], SZ_FNAME)
	interactive = clgetb ("interactive")
	call clpstr ("answer", "yes")
	ans = 'y'

	# Set up the graphics.
	call clgstr ("graphics", Memc[str], SZ_LINE)
	if (interactive) {
	    gp = gopen (Memc[str], NEW_FILE+AW_DEFER, STDGRAPH)
	    gt = gt_init()
	    call gt_sets (gt, GTTYPE, "mark")
	    call gt_sets (gt, GTXTRAN, "log")
	    call gt_setr (gt, GTXMIN, 10.)
	    call gt_setr (gt, GTYMIN, 0.)
	    call gt_sets (gt, GTTITLE, "Parameters of cosmic rays candidates")
	    call gt_sets (gt, GTXLABEL, "Flux")
	    call gt_sets (gt, GTYLABEL, "Flux Ratio")
	}

	# Use image header translation file.
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])

	# Set up surface fitting.  The background points are placed together
	# at the beginning of the arrays.  There are two surface pointers,
	# one for using the fast refit if there are no points excluded and
	# one for doing a full fit with points excluded.

	szhwin = szwin / 2
	szwin2 = szwin * szwin
	call salloc (data, szwin, TY_INT)
	call salloc (x, szwin2, TY_REAL)
	call salloc (y, szwin2, TY_REAL)
	call salloc (z, szwin2, TY_REAL)
	call salloc (w, szwin2, TY_REAL)

	k = 0
	do i = 1, szwin {
	    Memr[x+k] = i
	    Memr[y+k] = 1
	    k = k + 1
	}
	do i = 2, szwin {
	    Memr[x+k] = szwin
	    Memr[y+k] = i
	    k = k + 1
	}
	do i = szwin-1, 1, -1 {
	    Memr[x+k] = i
	    Memr[y+k] = szwin
	    k = k + 1
	}
	do i = szwin-1, 2, -1 {
	    Memr[x+k] = 1
	    Memr[y+k] = i
	    k = k + 1
	}
	do i = 2, szwin-1 {
	    do j = 2, szwin-1 {
	        Memr[x+k] = j
	        Memr[y+k] = i
	        k = k + 1
	    }
	}
	call aclrr (Memr[z], szwin2)
	call amovkr (1., Memr[w], 4*szwin-4)
	call gsinit (sf1, GS_POLYNOMIAL, 2, 2, NO, 1., real(szwin),
	    1., real(szwin))
	call gsinit (sf2, GS_POLYNOMIAL, 2, 2, NO, 1., real(szwin),
	    1., real(szwin))
	call gsfit (sf1, Memr[x], Memr[y], Memr[z], Memr[w], 4*szwin-4,
	    WTS_USER, j)

	# Process each input image.  Either work in place or create a
	# new output image.  If an error mapping the images occurs
	# issue a warning and go on to the next input image.

	while (imtgetim (list1, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (list2, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    if (clgfil (list3, Memc[badpix], SZ_FNAME) == EOF)
		Memc[badpix] = EOS

	    iferr {
		in = NULL
		out = NULL
		cr = NULL

		# Map the input image and check for image type and
		# previous correction flag.  If the output image is
		# the same as the input image work in place.
		# Initialize IMIO to use a scrolling buffer of lines.

		call set_input (Memc[input], im, i)
		if (im == NULL)
		    call error (1, "Skipping input image")

		if (ccdflag (im, "crcor")) {
		    call eprintf ("WARNING: %s previously corrected\n")
			call pargstr (Memc[input])
		    #call imunmap (im)
		    #next
		}

	        if (streq (Memc[input], Memc[output])) {
		    call imunmap (im)
	            im = immap (Memc[input], READ_WRITE, 0)
		}
		in = im

	        nc = IM_LEN(in,1)
	        nl = IM_LEN(in,2)
	        if ((nl < szwin) || (nc < szwin))
	            call error (0, "Image size is too small")
	        call imseti (in, IM_NBUFS, szwin)
	        call imseti (in, IM_TYBNDRY, BT_NEAREST)
	        call imseti (in, IM_NBNDRYPIX, szhwin)

		# Open the output image if needed.
	        if (strne (Memc[input], Memc[output]))
		    im = immap (Memc[output], NEW_COPY, in)
		out = im

		# Open a cosmic ray list structure.
	        call cr_open (cr)
		ncrlast = 0
		nreplaced = 0

		# Now proceed through the image line by line, scrolling
		# the line buffers at each step.  If creating a new image
		# also write out each line as it is read.  A procedure is
		# called to find the cosmic ray candidates in the line
		# and add them to the list maintained by CRLIST.
		# Note that cosmic rays are not replaced at this point
		# in order to allow the user to modify the criteria for
		# a cosmic ray and review the results.

	        c1 = 1-szhwin
	        c2 = nc+szhwin
		do i = 1, szwin-1 
		    Memi[data+i] =
			imgs2r (in, c1, c2, i-szhwin, i-szhwin)

	        do l = 1, nl {
		    do i = 1, szwin-1
			Memi[data+i-1] = Memi[data+i]
		    Memi[data+szwin-1] =
			imgs2r (in, c1, c2, l+szhwin, l+szhwin)
		    if (out != in)
		        call amovr (Memr[Memi[data+szhwin]+szhwin],
			    Memr[impl2r(out,l)], nc)

	            call cr_find (cr, threshold, Memi[data],
		        c2-c1+1, szwin, c1, l,
		        sf1, sf2, Memr[x], Memr[y], Memr[z], Memr[w])
	        }
		if (interactive && train) {
		    call cr_train (cr, gp, gt, in, fluxratio, Memc[savefile])
		    train = false
		}
	        call cr_flags (cr, fluxratio)

		# If desired examine the cosmic ray list interactively.
	        if (interactive && ans != 'N') {
		    if (ans != 'Y') {
		        call eprintf ("%s - ")
		            call pargstr (Memc[input])
		        call flush (STDERR)
		        ans = clgetc ("answer")
		    }
		    if ((ans == 'Y') || (ans == 'y'))
	                call cr_examine (cr, gp, gt, in, fluxratio, 'r')
	        }

		# Now replace the selected cosmic rays in the output image.

		call imflush (out)
		call imseti (out, IM_ADVICE, RANDOM)
	        call cr_replace (cr, ncrlast, out, nreplaced)

		# Do additional passes through the data.  We work in place
		# in the output image.  Note that we only have to look in
		# the vicinity of replaced cosmic rays for secondary
		# events since we've already looked at every pixel once.
		# Instead of scrolling through the image we will extract
		# subrasters around each replaced cosmic ray.  However,
		# we use pointers into the subraster to maintain the same
		# format expected by CR_FIND.

	        if (npasses > 1) {
	            if (out != in)
	                call imunmap (out)
	            call imunmap (in)
		    im = immap (Memc[output], READ_WRITE, 0)
		    in = im
		    out = im
	            call imseti (in, IM_TYBNDRY, BT_NEAREST)
	            call imseti (in, IM_NBNDRYPIX, szhwin)

	            for (i=2; i<=npasses; i=i+1) {
			# Loop through each cosmic ray in the previous pass.
		        ncr = CR_NCR(cr)
		        do j = ncrlast+1, ncr {
			    flag = Memi[CR_FLAG(cr)+j-1]
			    if (flag==NO || flag==ALWAYSNO)
			        next
			    c = Memr[CR_COL(cr)+j-1]
			    l = Memr[CR_LINE(cr)+j-1]
			    c1 = max (1-szhwin, c - (szwin-1))
			    c2 = min (nc+szhwin, c + (szwin-1))
			    k = c2 - c1 + 1
			    l1 = max (1-szhwin, l - (szwin-1))
			    l2 = min (nl+szhwin, l + (szwin-1))

			    # Set the line pointers off an image section
			    # centered on a previously replaced cosmic ray.

			    ptr = imgs2r (in, c1, c2, l1, l2) - k

			    l1 = max (1, l - szhwin)
			    l2 = min (nl, l + szhwin)
			    do l = l1, l2 {
				do m = 1, szwin
				    Memi[data+m-1] = ptr + m * k
				ptr = ptr + k

	                        call cr_find ( cr, threshold, Memi[data],
				    k, szwin, c1, l, sf1, sf2,
				    Memr[x], Memr[y], Memr[z], Memr[w])
			    }
	                }
		        call cr_flags (cr, fluxratio)

			# Replace any new cosmic rays found.
	                call cr_replace (cr, ncr, in, nreplaced)
			ncrlast = ncr
		    }
	        }

		# Output header log, log, plot, and bad pixels.
		call sprintf (Memc[str], SZ_LINE,
		    "Threshold=%5.1f, fluxratio=%6.2f, removed=%d")
		    call pargr (threshold)
		    call pargr (fluxratio)
		    call pargi (nreplaced)
		call timelog (Memc[str], SZ_LINE)
		call ccdlog (out, Memc[str])
		call hdmpstr (out, "crcor", Memc[str])

		call cr_plot (cr, in, fluxratio)
		call cr_badpix (cr, Memc[badpix])

	        call cr_close (cr)
	        if (out != in)
	            call imunmap (out)
	        call imunmap (in)
	    } then {
		# In case of error clean up and go on to the next image.
		if (in != NULL) {
		    if (out != NULL && out != in)
			call imunmap (out)
		    call imunmap (in)
		}
		if (cr != NULL)
		    call cr_close (cr)
		call erract (EA_WARN)
	    }
	}

	if (interactive) {
	    call gt_free (gt)
	    call gclose (gp)
	}
	call imtclose (list1)
	call imtclose (list2)
	call clpcls (list3)
	call hdmclose ()
	call gsfree (sf1)
	call gsfree (sf2)
	call sfree (sp)
end
