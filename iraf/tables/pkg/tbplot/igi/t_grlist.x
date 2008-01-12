include <ctype.h>

procedure t_grlist ()

pointer	sp
pointer	imroot, inimnm, imsect
pointer	templt
int	nclust				# Number of group members (clusters)
pointer	imname
int	im				# Input image descriptor
pointer	clist
int	clust
int	nvals
pointer	list
pointer	inlist
bool	incl
pointer	itmpl
int	nrngs
int	status

int	immap(), imaccf(), imgeti(), imtgetim()
int	decode_ranges(), get_next_number()
pointer	imtopen()
bool	clgetb(), imgetb(), streq()

begin
	call smark (sp)

	call salloc (inlist, SZ_FNAME, TY_CHAR)
	call salloc (inimnm, SZ_FNAME, TY_CHAR)
	call salloc (imsect, SZ_FNAME, TY_CHAR)
	call salloc (imroot, SZ_FNAME, TY_CHAR)
	call salloc (templt, SZ_LINE,  TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	call clgstr ("image", Memc[inlist], SZ_FNAME)
	#call eprintf ("# %s\n")
	#    call pargstr (Memc[inlist])

	call clgstr ("members", Memc[templt], SZ_LINE)
	#call eprintf ("# %s\n")
	#    call pargstr (Memc[templt])

	for (itmpl = templt;
	     IS_WHITE(Memc[itmpl])&&Memc[itmpl]!=EOS;
	     itmpl = itmpl + 1)
	    ;

	if (streq (Memc[itmpl], "*"))
	    Memc[itmpl] = EOS

	incl = clgetb ("inclusive")

	# Open the image "template" list
	list = imtopen (Memc[inlist])

	while (imtgetim (list, Memc[inimnm], SZ_FNAME) > 0) {
	    # For each image in the list
	    #call eprintf ("# %s\n")
	    #	call pargstr (Memc[inimnm])

	    call imgsection (Memc[inimnm], Memc[imsect], SZ_FNAME)
	    call imgimage (Memc[inimnm], Memc[imroot], SZ_FNAME)

	    im = immap (Memc[imroot], READ_ONLY, 0)

	    if (imaccf (im, "GROUPS") == NO ||
		!imgetb (im, "GROUPS") ||
		imaccf (im, "GCOUNT") == NO) {

		call eprintf ("# Image not in group format: %s\n")
		    call pargstr (Memc[inimnm])

		call error (0, "Image not in group format")

		if (incl) {
		    call printf ("%s\n")
			call pargstr (Memc[inimnm])
		}

	    } else {
		# Group format image
		# Find the number of clusters (group members)
		nclust = imgeti (im, "GCOUNT")
		nrngs = nclust + 1

		call imunmap (im)

		call calloc (clist, 3*nrngs, TY_INT)

#		if (decode_ranges (Memc[itmpl], Memi[clist],
#		    nrngs, nvals) == ERR && nvals == 0) {

		status = decode_ranges (Memc[itmpl], Memi[clist],
		    nrngs, nvals)

		if (status == ERR && nvals == 0) {

		    call eprintf ("# %s\n")
			call pargstr (Memc[itmpl])

		    call error (0, "Bad range")
		}

		#call eprintf ("# %s %d\n")
		#    call pargstr (Memc[itmpl])
		#    call pargi (nclust)

		clust = 0

		while (get_next_number (Memi[clist], clust) != EOF) {
		    # For each cluster member

		    if (clust > nclust)
			break

		    call printf ("%s[%d]%s\n")
			call pargstr (Memc[imroot])
			call pargi (clust)
			call pargstr (Memc[imsect])
		}
	
		call mfree (clist, TY_INT)

	    }  # Group format?

	}  # For each image

	call imtclose (list)
	call sfree (sp)
end
