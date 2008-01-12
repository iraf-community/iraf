include	<imhdr.h>
include <ctype.h>

procedure t_grplot ()

#  GRPLOT -- Plot multi-image (group format) files on a single frame.
#  Because there is no IMIO syntax for wildcarding the group element
#  (cluster), we need to build the image name and open each one explicitly.

#  21 June 1993 Fixed bogus scaling for data within range 0:1
#               Removed grlist to separate source

pointer	sp
pointer	imroot, inimnm, imsect
pointer	templt
pointer	device
pointer	gp
int	nclust				# Number of group members (clusters)
pointer	cllist
int	size
pointer	data
pointer	units
pointer	title, xlabel, ylabel
real	border
int	ncrv				# Number of data curves

int	numcls(), rdcldat()
pointer	gopen()
real	clgetr()

begin
	call smark (sp)

	call salloc (inimnm, SZ_FNAME, TY_CHAR)
	call salloc (imsect, SZ_FNAME, TY_CHAR)
	call salloc (imroot, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (templt, SZ_LINE,  TY_CHAR)
	call salloc (title,  SZ_LINE,  TY_CHAR)
	call salloc (xlabel, SZ_LINE,  TY_CHAR)
	call salloc (ylabel, SZ_LINE,  TY_CHAR)
	call salloc (units,  SZ_LINE,  TY_CHAR)

	call clgstr ("image", Memc[inimnm], SZ_FNAME)
	call imgsection (Memc[inimnm], Memc[imsect], SZ_FNAME)
	call imgimage (Memc[inimnm], Memc[imroot], SZ_FNAME)

	call clgstr ("members", Memc[templt], SZ_LINE)

	# Find the image members to read
	nclust = numcls (Memc[inimnm], Memc[templt], cllist, size,
	    Memc[units], SZ_LINE)

	# Allocate pointers to the data buffers
	call malloc (data, size*nclust, TY_REAL)

	call clgstr ("title", Memc[title], SZ_FNAME)
	border = clgetr ("border")
	call clgstr ("device", Memc[device], SZ_FNAME)

	# Open graphics
	gp = gopen (Memc[device], NEW_FILE, STDGRAPH)

	# Read the image data
	ncrv = rdcldat (gp, Memc[imroot], Memc[imsect],
	    Memi[cllist], Memr[data], nclust, size)

	call mfree (cllist, TY_INT)

	# Scale the X axis
	call gswind (gp, 0.0, real (size+1), INDEF, INDEF)

	# Expand the scale slightly for a border
	call expscl (gp, border)

	if (Memc[title] == EOS) {
	    call sprintf (Memc[title], SZ_LINE, "%s[%s]%s")
		call pargstr (Memc[imroot])
		call pargstr (Memc[templt])
		call pargstr (Memc[imsect])
	}

	call strcpy ("Pixel", Memc[xlabel], SZ_LINE)
	call strcpy (Memc[units], Memc[ylabel], SZ_LINE)

	# Draw the axes and labels
	call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])

	# Draw the curves
	call plclust (gp, Memr[data], size, ncrv)

	call gclose (gp)

	call mfree (data, TY_REAL)
	call sfree (sp)
end


int procedure numcls (imroot, cltmpl, cllist, size, units, maxch)

char	imroot[ARB]			# Root image name
char	cltmpl[ARB]			# Cluster template
pointer	cllist				# Pointer to list of clusters
int	size				# Data elements per vector
char	units[ARB]			# Data units
int	maxch

int	im				# Input image descriptor
int	nclust				# Number of group members (clusters)
int	nvals
int	nrngs

int	immap(), imaccf(), imgeti(), decode_ranges()
bool	imgetb()

begin

	im = immap (imroot, READ_ONLY, 0)

	if (imaccf (im, "GROUPS") == NO)
	    call error (0, "Image not in group format")

	if (!imgetb (im, "GROUPS"))
	    call error (0, "Image not in group format")

	if (imaccf (im, "GCOUNT") == NO)
	    call error (0, "Image not in group format")

	# Find the number of clusters (group members)
	nclust = imgeti (im, "GCOUNT")
	size   = IM_LEN(im,1)

	nrngs = nclust + 1

	call malloc (cllist, 3*nrngs, TY_INT)

	if (decode_ranges (cltmpl, Memi[cllist],
	    nrngs, nvals) == ERR && nvals == 0)
	    call error (0, "Bad range")

	if (imaccf (im, "BUNIT") == YES)
	    call imgstr (im, "BUNIT", units, maxch)
	else
	    units[1] = EOS

	call imunmap (im)

	return (nclust)
end


procedure expscl (gp, border)

#  EXPSCL -- Expand the plot scale slightly by a fraction of the range.

pointer	gp				# Graphics descriptor
real	border				# Percent of the scale

real	wl, wr, wb, wt
real	dx, dy

begin
	call ggwind (gp, wl, wr, wb, wt)

	if (IS_INDEFR(border))
	    border = 0.05
	else
	    border = border / 100.0

	dy = border * (wt - wb)
	wb = wb - dy
	wt = wt + dy

	dx = border * (wr - wl)
	wl = wl - dx
	wr = wr + dx

	call gswind (gp, wl, wr, wb, wt)
end


procedure plclust (gp, data, size, nclust)

# Draw the curves

pointer	gp
real	data[size,nclust]
int	size
int	nclust

int	clust

begin
	do clust = 1, nclust
	    # For each image cluster (group member), draw the curve
	    call gvline (gp, data[1,clust], size, 1.0, real (size))
end


int procedure rdcldat (gp, imroot, imsect, cllist, data, nclust, size)

# Read the image data

pointer	gp
char	imroot[ARB]
char	imsect[ARB]
int	cllist[ARB]
real	data[size,nclust]
int	nclust
int	size

int	clust
int	im
pointer	sp, imname
int	ncrv

#real	wl, wr, wb, wt

int	immap(), get_next_number()
pointer	imgl1r()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	clust = 0
	ncrv  = 0

#	call gswind (gp, 0.0, 0.0, 0.0, 0.0)

	while (get_next_number (cllist, clust) != EOF) {
	    # For each image cluster (group member)

	    if (clust > nclust)
		break

	    ncrv = ncrv + 1

	    # Build the cluster-specific image name
	    call sprintf (Memc[imname], SZ_FNAME, "%s[%d]%s")
		call pargstr (imroot)
		call pargi (clust)
		call pargstr (imsect)

	    # Map the image cluster (group) member
	    im = immap (Memc[imname], READ_ONLY, 0)

	    # Read the image data for this cluster
	    call amovr (Memr[imgl1r (im)], data[1,ncrv], size)

#	    call ggwind (gp, wl, wr, wb, wt)
#	    call printf ("%.2g %.2g %.2g %.2g\t")
#		call pargr (wl)
#		call pargr (wr)
#		call pargr (wb)
#		call pargr (wt)

	    if (ncrv == 1)
		# Scale the Y axis for the first curve
		call gascale (gp, data[1,ncrv], size, 2)
	    else
		# Rescale the Y axis for the other curve(s)
		call grscale (gp, data[1,ncrv], size, 2)

#	    call ggwind (gp, wl, wr, wb, wt)
#	    call printf ("%.2g %.2g %.2g %.2g\n")
#		call pargr (wl)
#		call pargr (wr)
#		call pargr (wb)
#		call pargr (wt)

#	    call printf ("%s  %d\n")
#		call pargstr (Memc[imname])
#		call pargi (im)

	    call imunmap (im)
	}

	# Scale the X axis
	call gswind (gp, 0.0, real (size+1), INDEF, INDEF)

#	call ggwind (gp, wl, wr, wb, wt)
#	call printf ("%.2g %.2g %.2g %.2g\n")
#	    call pargr (wl)
#	    call pargr (wr)
#	    call pargr (wb)
#	    call pargr (wt)

	call sfree (sp)

	return (ncrv)
end
