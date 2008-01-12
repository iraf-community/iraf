#* HISTORY *
#* B.Simon	30-Sep-98	Original code

# GF_FITSNAME -- Construct a fits file name from its parts

procedure  gf_fitsname (image, gn, mode, fitsname, maxch)

char	image[ARB]	# i: image name
int	gn		# i: group number
int	mode		# i: access mode
char	fitsname[ARB]	# o: fits file name
int	maxch		# i: declared length of fits file name
#--
int	ic
int	gstrcpy(), itoc()

begin
	ic = gstrcpy (image, fitsname, maxch-1) + 1
	fitsname[ic] = '['
	ic = ic + 1

	if (gn >= 0) {
	    ic = ic + itoc (gn, fitsname[ic], maxch-ic-1)

	} else if (ic <= maxch) {
	    fitsname[ic] = '1'
	    ic = ic + 1
	}

	switch (mode) {
	case READ_ONLY:
	    ;
	case READ_WRITE, WRITE_ONLY:
	    ic = ic + gstrcpy (",over+", fitsname[ic], maxch-ic-1)
	case APPEND, NEW_FILE:
	    ic = ic + gstrcpy (",append+", fitsname[ic], maxch-ic-1)
	case NEW_COPY:
	    ic = ic + gstrcpy (",dupname+,append+", fitsname[ic], maxch-ic-1)
	}

	if (ic <= maxch) {
	    fitsname[ic] = ']'
	    ic = ic + 1
	}

	fitsname[ic] = EOS
end
