# AP_GETIM -- Standardize image name so that different ways of specifying
# the images map to the same database and output rootnames.

int procedure ap_getim (list, image, maxchar)

int	list			#I Image list
char	image[maxchar]		#O Image name
int	maxchar			#I Maximum number of chars in image name

char	ksection[SZ_FNAME]	#O Image name

int	i, j, stat, cl_index, cl_size
pointer	im
pointer	sp, cluster, section

int	imtgetim(), strlen(), stridxs(), ctoi()
pointer	immap()

begin
	# Get next image name.
	stat = imtgetim (list, image, maxchar)
	if (stat == EOF)
	    return (stat)

	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	call imparse (image, Memc[cluster], SZ_FNAME, ksection, SZ_FNAME,
	    Memc[section], SZ_FNAME, cl_index, cl_size)

	# Strip the extension.
	call xt_imroot (Memc[cluster], Memc[cluster], SZ_FNAME)

	# Generate standard ksection.  Only map image if index used.
	# Don't worry about cases with both an index and ksection.

	if (cl_index < 0 && ksection[1] == EOS)
	    ;
	else if (cl_index == 0)
	    ksection[1] = EOS
	else {
	    if (cl_index > 0) {
		im = immap (image, READ_ONLY, 0)
		ksection[1] = '['
		call imgstr (im, "extname", ksection[2], SZ_FNAME-1)
		i = strlen (ksection)
		ifnoerr (call imgstr (im, "extver" ,
		    ksection[i+2], SZ_FNAME-i-1)) {
		    ksection[i+1] = ','
		    i = strlen (ksection)
		}
		ksection[i+1] = ']'
		ksection[i+2] = EOS
		call imunmap (im)
	    } else {
		i = stridxs (",", ksection[2]) + 2
		if (i > 2) {
		    j = ctoi (ksection, i, j)
		    ksection[i] = ']'
		    ksection[i+1] = EOS
		}
	    }
	}

	call sprintf (image, maxchar, "%s%s%s")
	    call pargstr (Memc[cluster])
	    call pargstr (ksection)
	    call pargstr (Memc[section])

	call sfree (sp)
	return (stat)
end
