include	<imset.h>


# IC_HDR -- Set output header.

procedure ic_hdr (in, out, nimages)

pointer	in[nimages]		#I Input images
pointer	out[ARB]		#I Output images
int	nimages			#I Number of images

int	i, imgnfn()
pointer	sp, key, str, list, imofnlu()

begin
	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Set new PROCID.
	call xt_procid (out)

	# Set input PROCIDs.
	if (nimages < 100) {
	    list = imofnlu (out, "PROCID[0-9][0-9]")
	    while (imgnfn (list, Memc[key], SZ_LINE) != EOF)
		call imdelf (out, Memc[key])
	    call imcfnl (list)
	    do i = 1, nimages {
		call sprintf (Memc[key], 8, "PROCID%02d")
		    call pargi (i)
		iferr (call imgstr (in[i], "PROCID", Memc[str], SZ_LINE)) {
		    iferr (call imgstr (in[i], "OBSID", Memc[str], SZ_LINE))
			Memc[str] = EOS
		}
		if (Memc[str] != EOS)
		    call imastr (out, Memc[key], Memc[str])
	    }

	    # Set input image names.
	    list = imofnlu (out, "IMCMB[0-9][0-9][0-9]")
	    while (imgnfn (list, Memc[key], SZ_LINE) != EOF)
		call imdelf (out, Memc[key])
	    call imcfnl (list)
	    do i = 1, nimages {
		iferr (call imgstr (in[i], "ICFNAME", Memc[str], SZ_LINE))
		    call imstats (in[i], IM_IMAGENAME, Memc[str], SZ_LINE)
		call sprintf (Memc[key], SZ_LINE, "IMCMB%03d")
		    call pargi (i)
		call imastr (out, Memc[key], Memc[str])
	    }
	}

	call sfree (sp)
end
