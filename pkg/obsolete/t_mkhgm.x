include	<mach.h>
include	<gset.h>

define	SZ_HISTBUF	512

# HISTOGRAM -- Compute and plot the histogram of an file.

procedure t_mkhistogram()

bool	listout
int	fd, ndata, nbins
pointer sp, file, device, data, hgm, hgmr, gp
real	z1, z2, zmin, zmax, dz, zval

bool	clgetb(), fp_equalr()
int	i, clgeti(), open(), get_histdata()
real	clgetr()
pointer	gopen()

begin
	call smark (sp)
	call salloc (file,   SZ_LINE,  TY_CHAR)
	call salloc (device,  SZ_FNAME, TY_CHAR)

	# Get the file name.
	call clgstr ("file", Memc[file], SZ_LINE)
	fd = open (Memc[file], READ_ONLY, TEXT_FILE)

	# Output can be either a list or a plot.
	listout = clgetb ("listout")
	if (! listout)
	    call clgstr ("device", Memc[device], SZ_FNAME)


	# Get histogram length and allocate buffer.
	nbins = clgeti ("nbins")
	if (nbins < 2) {
	    call eprintf ("Warning: Less than 2 bins in histogram.\n")
	    call sfree (sp)
	    return
	}
	call salloc (hgm,  nbins, TY_INT)
	call salloc (hgmr, nbins, TY_REAL)

	# Fetch the data.
	call malloc (data, SZ_HISTBUF, TY_REAL)
	ndata = get_histdata (fd, data, SZ_HISTBUF)
	if (ndata <= 0) {
	    call eprintf ("Warning: No input data for histogram.\n")
	    call mfree (data, TY_REAL)
	    call sfree (sp)
	    return
	}

	z1 = clgetr ("z1")
	z2 = clgetr ("z2")
	if (IS_INDEFR(z1) || IS_INDEFR(z2)) {
	    call alimr (Memr[data], ndata, zmin, zmax)
	    if (IS_INDEFR(z1))
		z1 = zmin
	    if (IS_INDEFR(z2))
		z2 = zmax
	}
	dz = (z2 - z1) / (nbins - 1)

	# Test for constant valued file, which causes zero divide in ahgm.
	if (fp_equalr (z1, z2)) {
	    call eprintf (
		"Warning: Constant valued file `%s' has no data range.\n")
		call pargstr (Memc[file])
	    call mfree (data, TY_REAL)
	    call sfree (sp)
	    return
	}

	# Initialize histogram and file line vector.
	call aclri (Memi[hgm], nbins)

	# Accumulate the histogram.
	call ahgmr (Memr[data], ndata, Memi[hgm], nbins,  z1,  z2)

	# List or plot the histogram.
	if (listout) {
	    zval = z1
	    do i = 1, nbins {
		call printf ("%4d  %g  %d\n")
		    call pargi (i)
		    call pargr (zval + dz / 2.)
		    call pargi (Memi[hgm+i-1])
		zval = zval + dz
	    }
	} else {
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)
	    #call gseti (gp, G_YTRAN, GW_LOG)
	    call achtir (Memi[hgm], Memr[hgmr], nbins)
	    call gploto (gp, Memr[hgmr], nbins, z1 + dz / 2., z2 + dz / 2.,
	        Memc[file])
	    call gclose (gp)
	}

	# Shutdown.
	call close (fd)
	call sfree (sp)
end


# GET_HISTDATA -- Procedure to get data for the histogram

int procedure get_histdata (fd, data, buf_incr)

int	fd		# file descriptor of histogram data
pointer	data		# pointer to the data array
int	buf_incr	# increment for data buffer size

int	szbuf, ndata
int	fscan(), nscan()

begin
	szbuf = buf_incr
	ndata = 0
	while (fscan (fd) != EOF) {
	    call gargr (Memr[data+ndata])
	    if (nscan() != 1)
		next
	    ndata = ndata + 1
	    if (ndata == szbuf) {
		szbuf = szbuf + buf_incr
		call realloc (data, szbuf, TY_REAL)
	    }
	}

	# Fit the buffer size to the data.
	if (ndata > 0)
	    call realloc (data, ndata, TY_REAL)

	return (ndata)
end
