# T_NAMES -- Expand record extension format into a list of images.

procedure t_names ()

pointer	odr			# Input record list
pointer	append			# String to append to name
bool	check			# Check existence of image?

int	odr_getim()
bool	clgetb()
pointer	sp, image, im, immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (append, SZ_LINE, TY_CHAR)

	# Get parameters.
	call odr_open1 ("input", "records", odr)
	call clgstr ("append", Memc[append], SZ_LINE)
	check = clgetb ("check")

	# Loop over all input images - print name on STDOUT
	while (odr_getim (odr, Memc[image], SZ_FNAME) != EOF) {
	    # Open image if check for existence required
	    if (check) {
		ifnoerr (im = immap (Memc[image], READ_ONLY, 0)) {
		    call printf ("%s%s\n")
			call pargstr (Memc[image])
			call pargstr (Memc[append])
		    call imunmap (im)
		}
	    } else {
		call printf ("%s%s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[append])
	    }
	    call flush (STDOUT)
	}

	call odr_close (odr)
	call sfree (sp)
end
