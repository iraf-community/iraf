include	<error.h>
include	<imhdr.h>
 
define	NRANGES		100		# Maximum number of aperture ranges
 
# T_MSSELECT -- Select apertures from multispec or echelle format spectra.
# This task looks at the APNUM keywords and selects and extracts only
# those apertures in the aperture list.
 
procedure t_msselect()
 
int	inlist			# List of input spectra
int	outlist			# List of output spectra
pointer	aps			# Aperture list
bool	verbose			# Verbose?
 
int	i, j, nc, nl, ap, np2
real	w0, wpc
pointer	sp, input, output, temp, key, apnum, in, out
 
int	imtopenp(), imtgetim(), imtlen(), decode_ranges()
bool	clgetb(), is_in_range(), streq()
pointer	immap(), imgl2r(), impl2r()
errchk	imgstr()
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (apnum, SZ_LINE, TY_CHAR)
	call salloc (aps, 3*NRANGES, TY_INT)
 
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("apertures", Memc[input], SZ_FNAME)
	verbose = clgetb ("verbose")
 
	if (imtlen (outlist) != 0 && imtlen (outlist) != imtlen (inlist))
	    call error (1,
		"Input and output image lists are not the same length")
 
	if (decode_ranges (Memc[input], Memi[aps], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list")
 
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
 
	    if (streq (Memc[input], Memc[output]))
		call mktemp ("temp", Memc[temp], SZ_FNAME)
	    else
		call strcpy (Memc[output], Memc[temp], SZ_FNAME)
 
	    iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    iferr (out = immap (Memc[temp], NEW_COPY, in)) {
		call imunmap (in)
		call erract (EA_WARN)
		next
	    }
 
	    nc = IM_LEN(in,1)
	    nl = IM_LEN(in,2)
	    j = 0
	    do i = 1, nl {
		call sprintf (Memc[key], SZ_LINE, "apnum%d")
		    call pargi (i)
		iferr {
		    call imgstr (out, Memc[key], Memc[apnum], SZ_LINE)
		    call imdelf (out, Memc[key])
		    call sscan (Memc[apnum])
		    call gargi (ap)
		} then
		    ap = i
		if (!is_in_range (Memi[aps], ap))
		    next
		j = j + 1
	    }
 
	    if (j == 0) {
		call imunmap (in)
		call imunmap (out)
		call imdelete (Memc[output])
		call eprintf ("WARNING: No apertures to select in %s\n")
		    call pargstr (Memc[input])
		next
	    } else if (j == 1) {
		IM_NDIM(out) = 1
		call imastr (out, "apformat", "onedspec")
	    }
	    IM_LEN(out,2) = j
 
	    j = 0
	    do i = 1, IM_LEN(in,2) {
		call sprintf (Memc[key], SZ_LINE, "apnum%d")
		    call pargi (i)
		iferr {
		    call imgstr (in, Memc[key], Memc[apnum], SZ_LINE)
		    call sscan (Memc[apnum])
		    call gargi (ap)
		} then {
		    Memc[apnum] = EOS
		    ap = i
		}
		if (!is_in_range (Memi[aps], ap))
		    next
		j = j + 1
 
		call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,j)], nc)
		if (Memc[apnum] != EOS) {
		    call sprintf (Memc[key], SZ_LINE, "apnum%d")
		        call pargi (j)
		    call imastr (out, Memc[key], Memc[apnum])
		    if (IM_NDIM(out) == 1) {
			call gargi (np2)
			call gargr (w0)
			call gargr (wpc)
			call gargi (np2)
			call imaddr (out, "CRPIX1", 1.)
			call imaddr (out, "CRVAL1", w0)
			call imaddr (out, "CDELT1", wpc)
			call imaddr (out, "W0", w0)
			call imaddr (out, "WPC", wpc)
			call imaddi (out, "NP1", 0)
			call imaddi (out, "NP2", np2)
		    }
		}
 
		if (verbose) {
		    call printf ("%s[%d] --> %s\n")
			call pargstr (Memc[input])
			call pargi (ap)
			call pargstr (Memc[output])
		}
	    }
 
	    call imunmap (in)
	    call imunmap (out)
	    if (streq (Memc[input], Memc[output])) {
		call imdelete (Memc[input])
		call imrename (Memc[temp], Memc[output])
	    }
	}
 
	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end

