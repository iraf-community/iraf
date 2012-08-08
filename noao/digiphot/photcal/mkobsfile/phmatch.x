include <mach.h>
include "../lib/obsfile.h"

# PH_JOIN -- For each individual field join the data from each image
# line for line.

procedure ph_join (out, label, sym, filters, nfilters, objid, x, y, mag, merr,
	allfilters, wrap, verbose)

int	out			  # the output file descriptor
char	label[ARB]		  # the label string
pointer	sym[ARB]		  # list of pointers to the image set  data
char	filters[ARB]		  # the filter id string
int	nfilters		  # number of filters in an image  set
char	objid[DEF_LENLABEL,ARB]   # array of object ids
real	x[ARB]			  # array of shifted x coordinates
real	y[ARB]			  # array of shifted y coordinates
real	mag[ARB]		  # array of corrected magnitudes
real	merr[ARB]		  # array of magnitude errors
int	allfilters		  # match objects in all filters ?
int	wrap			  # format the output file for easy reading ?
int	verbose			  # print status, warning and error messages ?

int	i, j, nobjs, dptr, len_label, useid
pointer	sp, outfilter, idlabel, newlabel, bptrs, eptrs
bool	streq()
int	ph_nthlabel(), strlen()

begin
	# Find the maximum length of the output. Return if there is no data
	# in any of the frames, or if there is no data in one frame and
	# the match in all filters switch is on.

	if (allfilters == YES) {
	    nobjs = MAX_INT
	    do i = 1, nfilters {
		if (sym[i] == NULL) {
		    nobjs = 0
		    break
		}
		if (IMT_NENTRIES(sym[i]) == 0) {
		    nobjs = 0
		    break
		}
		nobjs = min (nobjs, IMT_NENTRIES(sym[i]))
	    }
	} else {
	    nobjs = 0
	    do i = 1, nfilters {
		if (sym[i] == NULL)
		    next
	        nobjs = max (nobjs, IMT_NENTRIES(sym[i]))
	    }
	}


	# Return if there is no data.
	if (nobjs <= 0) {
	    if (verbose == YES) {
	        call printf (
	        "\tImage set: %s  0 stars written to the observations file\n")
	            call pargstr (label)
	    }
	    return
	}

	# Allocate working space.
	call smark (sp)
	call salloc (idlabel, SZ_FNAME, TY_CHAR)
	call salloc (newlabel, SZ_FNAME, TY_CHAR)
	call salloc (outfilter, SZ_FNAME, TY_CHAR)
	call salloc (bptrs, nfilters, TY_INT)
	call salloc (eptrs, nfilters, TY_INT)

	# Determine whether or not to use the object id as the object label
	# and determine the length of the label. 

	if (objid[1,1] == EOS) {
	    useid = NO
	    if (nobjs == 1)
	        len_label = max (DEF_LENLABEL, strlen (label))
	    else
	        len_label = max (DEF_LENLABEL, strlen (label)+DEF_LENINDEX)
	} else { 
	    useid = YES
	    len_label = DEF_LENLABEL
	}

	# Initialize the arrays which point to the beginning and ending of
	# the data for each image in the set.

	do j = 1, nfilters {
	    if (sym[j] == NULL) {
		Memi[bptrs+j-1] = 0
		Memi[eptrs+j-1] = 0
	    } else {
		Memi[bptrs+j-1] = IMT_OFFSET(sym[j])
		Memi[eptrs+j-1] = IMT_OFFSET(sym[j]) + IMT_NENTRIES(sym[j]) - 1
	    }
	}

	# Write out the output.
	do i = 1, nobjs {

	    # Determine the object id. This is the object id of the first
	    # defined filter.
	    if (useid == YES) {
		Memc[idlabel] = EOS
		do j = 1, nfilters {
		    if (Memi[bptrs+j-1] == 0)
			next
		    call strcpy (objid[1,Memi[bptrs+j-1]], Memc[idlabel],
		        DEF_LENLABEL)
		    break
		}
	    }

	    do j = 1, nfilters {

		# Write the label.
		if (j == 1) {
		    if (useid == YES && Memc[idlabel] != EOS) {
		        call fprintf (out, "%*.*s ")
			    call pargi (-len_label)
			    call pargi (len_label)
			    call pargstr (Memc[idlabel])
		    } else if (nobjs == 1) {
		        call fprintf (out, "%*.*s ")
			    call pargi (-len_label)
			    call pargi (len_label)
			    call pargstr (label)
		    } else {
			call sprintf (Memc[newlabel], SZ_FNAME, "%s-%d")
			    call pargstr (label)
			    call pargi (i)
		        call fprintf (out, "%*.*s ")
			    call pargi (-len_label)
			    call pargi (len_label)
			    call pargstr (Memc[newlabel])
		    }
		} else if (wrap == YES) {
		    call fprintf (out, "%*.*s ")
		        call pargi (-len_label)
		        call pargi (len_label)
		        call pargstr ("*")
		} else {
		    call fprintf (out, "  ")
		}

		# Write the results.
		if (ph_nthlabel (filters, j, Memc[outfilter], SZ_FNAME) != j)
		    Memc[outfilter] = EOS
		dptr = Memi[bptrs+j-1]
		if (dptr == 0) {
		    call fprintf (out,
		        "%-10.10s %11.1h %6.3f %9.3f %9.3f %7.3f %6.3f")
		        call pargstr (Memc[outfilter])
			call pargr (INDEFR)
		        call pargr (INDEFR)
		        call pargr (INDEFR)
		        call pargr (INDEFR)
		        call pargr (INDEFR)
		        call pargr (INDEFR)
		} else {
		    if (IMT_OTIME(sym[j]) >= 0.0 && IMT_OTIME(sym[j]) <= 24.0)
		        call fprintf (out,
		            "%-10.10s %11.1h %6.3f %9.3f %9.3f %7.3f %6.3f")
		    else
		        call fprintf (out,
		            "%-10.10s %11g %6.3f %9.3f %9.3f %7.3f %6.3f")
		    if (streq (IMT_IFILTER(sym[j]), "INDEF"))
			call pargstr (Memc[outfilter])
		    else
		        call pargstr (IMT_IFILTER(sym[j]))
			call pargr (IMT_OTIME(sym[j]))
		        call pargr (IMT_XAIRMASS(sym[j]))
		        call pargr (x[dptr] - IMT_XSHIFT(sym[j]))
		        call pargr (y[dptr] - IMT_YSHIFT(sym[j]))
		        call pargr (mag[dptr])
		        call pargr (merr[dptr])
		}
		if (wrap == YES)
		    call fprintf (out, "\n")
		else if (j == nfilters)
		    call fprintf (out, "\n")

		# Increment the data pointers making sure to check for non-
		# existent data and unequal length object lists.

		if (dptr == 0)
		    next
		if (dptr >= Memi[eptrs+j-1])
		    dptr = 0
		else
		    Memi[bptrs+j-1] = dptr + 1
	    }
	}

	if (verbose == YES) {
	    call printf (
	        "\tImage set: %s  %d stars written to the observations file\n")
	        call pargstr (label)
	        call pargi (nobjs)
	}

	call sfree (sp)
end


# PH_MERGE -- For an individual field join the data from each image (filter) in
# the image (filter) set by matching the x-y positions.

procedure ph_merge (out, label, sym, filters, nfilters, objid, x, y, mag, merr,
	ysort, match, index, ndata, tolerance, allfilters, wrap, verbose)

int	out			  # the output file descriptor
char	label[ARB]		  # label string
pointer	sym[ARB]		  # list of pointers to the image set  data
char	filters[ARB]		  # filter string
int	nfilters		  # number of images an image set
char	objid[DEF_LENLABEL,ARB] # array of object ids
real	x[ARB]			  # array of x coordinates
real	y[ARB]			  # array of y coordinates
real	mag[ARB]		  # array of magnitudes
real	merr[ARB]		  # array of magnitude errors
int	ysort[ARB]		  # the y sort index
int	match[ARB]		  # the matching array
int	index[ndata,ARB]	  # the matching index array
int	ndata		          # the number of data points
real	tolerance		  # tolerance in pixels
int	allfilters		  # match objects in all filters
int	wrap			  # format the output file for easy reading ?
int	verbose			  # print status, warning and error messages

int	i, j, k, l, biptr, eiptr, bjptr, ejptr, len_label
int	ntimes, nframe, nobjs, nmatch, starok, lsort, useid
pointer	sp, idlabel, newlabel, outfilter
real	tol2, dx, dy, maxrsq, distsq
bool	streq()
int	ph_nthlabel(), strlen()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (idlabel, SZ_FNAME, TY_CHAR)
	call salloc (newlabel, SZ_FNAME, TY_CHAR)
	call salloc (outfilter, SZ_FNAME, TY_CHAR)

	# Initialize.
	nframe = 0
	do i = 1, nfilters {
	    if (sym[i] != NULL) {
		call amovki (NO, match[IMT_OFFSET(sym[i])],
		    IMT_NENTRIES(sym[i]))
		if (IMT_NENTRIES(sym[i]) > 0)
		    nframe = nframe + 1
	    }
	}
	tol2 = tolerance ** 2
	nobjs = 0
	if (allfilters == YES)
	    ntimes = 1
	else
	    ntimes = nfilters

	# Loop over the filters.
	do i = 1, ntimes {

	    # Find the data pointers for the first frame.
	    if (sym[i] == NULL)
		next
	    biptr = IMT_OFFSET(sym[i])
	    if (biptr == 0)
		next
	    eiptr = biptr + IMT_NENTRIES(sym[i]) - 1

	    # Initialize.
	    nmatch = 0

	    # Clear the index array.
	    do j = 1, nfilters
	        call aclri (index[1,j], ndata)

	    # Loop over the other frames matching stars.
	    do j = i + 1, nfilters {

		# Find the data pointers for the second frame.
	        if (sym[j] == NULL)
		    next
	        bjptr = IMT_OFFSET(sym[j])
	        if (bjptr == 0)
		    next
	        ejptr = bjptr + IMT_NENTRIES(sym[j]) - 1

	        # Loop over the stars in the first frame.
	        nmatch = 0
	        do k = biptr, eiptr {

		    if (match[ysort[k]] == YES)
			next
		    if (IS_INDEFR(x[ysort[k]]) || IS_INDEFR(y[ysort[k]]))
			next
		    nmatch = nmatch + 1
		    index[ysort[k]-biptr+1,i] = ysort[k]
		    maxrsq = tol2
		    lsort = 0

		    do l = bjptr, ejptr {
			if (match[ysort[l]] == YES)
			    next
		        if (IS_INDEFR(x[ysort[l]]) || IS_INDEFR(y[ysort[l]]))
			    next
			dy = y[ysort[l]] - y[ysort[k]]
			if (dy > tolerance)
			    break
			dx = x[ysort[l]] - x[ysort[k]]
			distsq = dx * dx + dy * dy 
			if (distsq > maxrsq)
			    next
			if (lsort > 0)
			    match[ysort[lsort]] = NO
			match[ysort[l]] = YES
			index[ysort[k]-biptr+1,j] = ysort[l]
			maxrsq = distsq
			lsort = l
		    }
	        }

		# If all the stars have been matched quit the loop.
		if (nmatch == 0)
		    break
	    }

	    # Search for unmatched stars in the last frame.
	    if ((nframe == 1) || (allfilters == NO && i == nfilters)) {
		do k = biptr, eiptr {
		    if (match[ysort[k]] == YES)
			next
		    if (IS_INDEFR(x[ysort[k]]) || IS_INDEFR(y[ysort[k]]))
			next
		    nmatch = nmatch + 1
		    index[ysort[k]-biptr+1,i] = ysort[k]
		}
	    }

	    # Use the individual star id.
	    if (objid[1,1] == EOS)
		useid = NO
	    else
		useid = YES

	    # Write the results.
	    do j = 1, eiptr - biptr + 1 {

		# Break if no stars were matched.
	        if (nmatch <= 0)
		    break
		    #next

		# If the allfilters switch is on only print points with
		# data for all the filters, otherwise print all the objects.

		if (allfilters == YES) {
		    starok = YES
		    do k = 1, nfilters {
			if (index[j,k] > 0)
			    next
			starok = NO
			break
		    }
		} else {
		    starok = NO
		    do k = 1, nfilters {
			if (index[j,k] <= 0)
			    next
			starok = YES
			break
		    }
		}
		if (starok == NO)
		    next

		if (useid == YES)
	    	    len_label = DEF_LENLABEL
	        else if (nobjs == 1)
	    	    len_label = max (DEF_LENLABEL, strlen (label))
		else
	    	    len_label = max (DEF_LENLABEL, strlen (label)+DEF_LENINDEX)

		# Find the label.
		if (useid == YES) {
		    Memc[idlabel] = EOS
		    do k = 1, nfilters {
			if (index[j,k] == 0)
			    next
			call strcpy (objid[1,index[j,k]], Memc[idlabel],
			    DEF_LENLABEL)
			break
		    }
		}

		# Loop over the filters, writing the data for each point.
		do k = 1, nfilters {

		    # Write the label.
		    if (k == 1) {
			if (useid == YES && Memc[idlabel] != EOS) {
		            call fprintf (out, "%*.*s ")
				call pargi (-len_label)
				call pargi (len_label)
			        call pargstr (Memc[idlabel])
		        } else if ((nmatch == 1) && (nobjs == 0)) {
		            call fprintf (out, "%*.*s ")
				call pargi (-len_label)
				call pargi (len_label)
			        call pargstr (label)
		        } else {
			    call sprintf (Memc[newlabel], SZ_FNAME, "%s-%d")
			        call pargstr (label)
			        call pargi (nobjs + 1)
		            call fprintf (out, "%*.*s ")
				call pargi (-len_label)
				call pargi (len_label)
			        call pargstr (Memc[newlabel])
		        }
		    } else if (wrap == YES) {
		        call fprintf (out, "%*.*s ")
			    call pargi (-len_label)
			    call pargi (len_label)
			    call pargstr ("*")
		    } else {
			call fprintf (out, "  ")
		    }

		    # Write the data.
		    if (ph_nthlabel (filters, k, Memc[outfilter],
		        SZ_FNAME) != k)
			Memc[outfilter] = EOS
		    if (index[j,k] == 0) {
		        call fprintf (out,
		            "%-10.10s %11.1h %6.3f %9.3f %9.3f %7.3f %6.3f")
		            call pargstr (Memc[outfilter])
			    call pargr (INDEFR)
		            call pargr (INDEFR)
		            call pargr (INDEFR)
		            call pargr (INDEFR)
		            call pargr (INDEFR)
		            call pargr (INDEFR)
		    } else {
			if (IMT_OTIME(sym[k]) >= 0.0 &&
			    IMT_OTIME(sym[k]) <= 24.0)
		            call fprintf (out,
		            "%-10.10s %11.1h %6.3f %9.3f %9.3f %7.3f %6.3f")
			else
		            call fprintf (out,
		            "%-10.10s %11g %6.3f %9.3f %9.3f %7.3f %6.3f")
			if (streq (IMT_IFILTER(sym[k]), "INDEF"))
		            call pargstr (Memc[outfilter])
			else
		            call pargstr (IMT_IFILTER(sym[k]))
			    call pargr (IMT_OTIME(sym[k]))
		            call pargr (IMT_XAIRMASS(sym[k]))
		            call pargr (x[index[j,k]] - IMT_XSHIFT(sym[k]))
		            call pargr (y[index[j,k]] - IMT_YSHIFT(sym[k]))
		            call pargr (mag[index[j,k]])
		            call pargr (merr[index[j,k]])
		    }
		    if (wrap == YES)
			call fprintf (out, "\n")
		    else if (k == nfilters)
			call fprintf (out, "\n")

		}

		nobjs = nobjs + 1
	    }
	}

	if (verbose == YES) {
	    call printf (
	        "\tImage set: %s  %d stars written to the observations file\n")
	        call pargstr (label)
	        call pargi (nobjs)
	}

	call sfree (sp)
end
