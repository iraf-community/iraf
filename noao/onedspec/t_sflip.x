include	<error.h>
include	<imhdr.h>
include	<smw.h>


# SFLIP -- Flip data and/or coordinate system in spectra.

procedure t_sflip ()

pointer	inlist		# Input list
pointer	outlist		# Output list
bool	coord_flip	# Flip coordinates?
bool	data_flip	# Flip data?

bool	in_place
int	i, j, k, n, axis
pointer	sp, input, output, temp, a, b, c
pointer	in, out, smw, mw, tmp, inbuf, outbuf

bool	clgetb(), streq()
int	imtgetim(), imtlen()
pointer	imtopenp(), immap(), smw_openim(), imgl3r(), impl3r()
errchk	immap, smw_openim

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (a, 3*3, TY_DOUBLE)
	call salloc (b, 3, TY_DOUBLE)
	call salloc (c, 3, TY_DOUBLE)

	# Get task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	coord_flip = clgetb ("coord_flip")
	data_flip = clgetb ("data_flip")

	# Loop over all input images.
	in = NULL
	out = NULL
	smw = NULL
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtlen (outlist) > 0) {
		if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		    break
	    } else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    if (streq (Memc[input], Memc[output])) {
		if (data_flip) {
		    in_place = false
		    call mktemp ("temp", Memc[temp], SZ_FNAME)
		} else
		    in_place = true
	    } else {
		in_place = false
		call strcpy (Memc[output], Memc[temp], SZ_FNAME)
	    }

	    iferr {
		# Map the images and WCS.
		if (in_place) {
		    tmp = immap (Memc[input], READ_WRITE, 0); in = tmp
		    out = in
		} else {
		    tmp = immap (Memc[input], READ_ONLY, 0); in = tmp
		    tmp = immap (Memc[temp], NEW_COPY, in); out = tmp
		}
		tmp = smw_openim (in); smw = tmp

		# Flip coordinates.
		if (coord_flip) {
		    mw = SMW_MW(smw,0)
		    n = SMW_PDIM(smw)
		    axis = SMW_PAXIS(smw,1) - 1
		    call mw_gltermd (mw, Memd[a], Memd[b], n)
		    Memd[a+axis*(n+1)] = -Memd[a+axis*(n+1)]
		    Memd[b+axis] = SMW_LLEN(smw,1) - Memd[b+axis] + 1
		    call mw_sltermd (mw, Memd[a], Memd[b], n)
		    call smw_saveim (smw, out)
		}

		# Flip data.
		if (data_flip) {
		    n = IM_LEN(in,1)
		    do j = 1, IM_LEN(in,3) {
			do i = 1, IM_LEN(in,2) {
			    inbuf = imgl3r (in, i, j)
			    switch (SMW_FORMAT(smw)) {
			    case SMW_ND:
				switch (SMW_LAXIS(smw,1)) {
				case 1:
				    outbuf = impl3r (out, i, j) + n - 1
				    do k = 0, n-1
					Memr[outbuf-k] = Memr[inbuf+k]
				case 2:
				    outbuf = impl3r (out, IM_LEN(in,2)-i+1, j)
				    call amovr (Memr[inbuf], Memr[outbuf], n)
				case 3:
				    outbuf = impl3r (out, i, IM_LEN(in,3)-j+1)
				    call amovr (Memr[inbuf], Memr[outbuf], n)
				}
			    case SMW_ES, SMW_MS:
				outbuf = impl3r (out, i, j) + n - 1
				do k = 0, n-1
				    Memr[outbuf-k] = Memr[inbuf+k]
			    }
			}
		    }
		} else if (!in_place) {
		    n = IM_LEN(in,1)
		    do j = 1, IM_LEN(in,3) {
			do i = 1, IM_LEN(in,2) {
			    inbuf = imgl3r (in, i, j)
			    outbuf = impl3r (out, i, j)
			    call amovr (Memr[inbuf], Memr[outbuf], n)
			}
		    }
		}
	    } then {
		if (!in_place && out != NULL) {
		    call imunmap (out)
		    call imdelete (Memc[temp])
		}
		call erract (EA_WARN)
	    }

	    if (smw != NULL)
		call smw_close (smw)
	    if (!in_place && out != NULL) {
		call imunmap (out)
		call imunmap (in)
		if (streq (Memc[input], Memc[output])) {
		    call imdelete (Memc[input])
		    call imrename (Memc[temp], Memc[output])
		}
	    } else if (in != NULL)
		call imunmap (in)
	}

	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end
