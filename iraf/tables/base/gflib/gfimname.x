include "gf.h"

#* HISTORY *
#* B.Simon	19-Nov-99	original code
#* B.Simon	20-Nov-00	get default extension with gf_getext

# GF_IMNAME -- Construct an image name from a file name and extension number

procedure gf_imname (filename, extra, acmode, gnum, imname, maxch)

char	filename[ARB]	# i: image file name
char	extra[ARB]	# i: extra switches for fits files
int	acmode		# i: image access mode
int	gnum		# u: group/extension number
char	imname[ARB]	# o: image name, including extension
int	maxch		# i: maximum length of full image name
#--
int	ic, nc, code, gn, gcount
pointer	sp, cluster, ksection, section, ext

int	strlen(), fnextn(), gstrcpy(), itoc()
int	gf_filetype(), gf_get_gnum()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (ksection, SZ_SHORTSTR, TY_CHAR)
	call salloc (section, SZ_SHORTSTR, TY_CHAR)
	call salloc (ext, SZ_SHORTSTR, TY_CHAR)

	# Parse image name into parts

	call imparse (filename, Memc[cluster], SZ_FNAME, Memc[ksection], 
		      SZ_SHORTSTR, Memc[section], SZ_SHORTSTR, gn, gcount)

	if (gnum < 0)
	    gnum = gn

	# Add default extension onto cluster name if not present

	nc = strlen (Memc[cluster])
	if (Memc[cluster+nc-1] != '.' && 
	    fnextn (Memc[cluster], Memc[ext], SZ_SHORTSTR) == 0) {

	    call gf_getext (Memc[cluster], acmode, Memc[ext], SZ_SHORTSTR)

	    Memc[cluster+nc] = '.'
	    call strcpy (Memc[ext], Memc[cluster+nc+1], SZ_FNAME-(nc+1))
	}

	# Determine the file type

	code = gf_filetype (Memc[cluster], acmode)

	if (code == FITS_FMT) {
	    ic = gstrcpy (Memc[cluster], imname, maxch-1) + 1

	    # Add extension number to file name

	    if (Memc[ksection] == EOS) {
		imname[ic] = '['
		ic = ic + 1

		if (gnum >= 0) {
		    ic = ic + itoc (gnum, imname[ic], maxch-ic-1)

		} else if (ic <= maxch) {
		    imname[ic] = '1'
		    ic = ic + 1
		    gnum = 1
		}

	    } else {
		# Add kernel section, modified by new extension number,
		# to file name

		if (gnum == -1) {
		    gnum = gf_get_gnum (Memc[ksection])

		} else if (gnum == 0) {
		    call strcpy ("[0]", Memc[ksection], SZ_SHORTSTR)

		} else {
		    call gf_swap_gnum (gnum, Memc[ksection], SZ_SHORTSTR)
		}

		ic = (ic - 1) + gstrcpy (Memc[ksection], imname[ic], maxch-ic)
	    }

	    # Add appropriate switches for access mode

	    switch (acmode) {
	    case READ_ONLY:
		;
	    case READ_WRITE, WRITE_ONLY:
		ic = ic + gstrcpy (",over+", imname[ic], maxch-ic)
	    case APPEND, NEW_FILE:
		ic = ic + gstrcpy (",append+", imname[ic], maxch-ic)
	    case NEW_COPY:
		ic = ic + gstrcpy (",dupname+,append+", imname[ic], maxch-ic)
	    }

	    if (extra[1] != EOS) {
		if (extra[1] != ',') {
		    imname[ic] = ','
		    ic = ic + 1
		}

		ic = ic + gstrcpy (extra, imname[ic], maxch-ic)
	    }

	    if (ic <= maxch) {
		imname[ic] = ']'
		ic = ic + 1
	    }

	    imname[ic] = EOS

	} else if (code == GEIS_FMT) {
	    # Add appropriate extension according to image mode

	    if (gnum == -1)  {
		call strcpy (Memc[cluster], imname, maxch)
		gnum = 1

	    } else if (gcount == -1) {
		call sprintf (imname, maxch, "%s[%d]")
		call pargstr (Memc[cluster])
		call pargi (gnum)

	    } else if (acmode == NEW_COPY || acmode == NEW_IMAGE) {
		call sprintf (imname, maxch, "%s[%d/%d]")
		call pargstr (Memc[cluster])
		call pargi (gnum)
		call pargi (gcount)

	    } else {
		call sprintf (imname, maxch, "%s[%d]")
		call pargstr (Memc[cluster])
		call pargi (gnum)
	    }

	} else {
	    call strcpy (Memc[cluster], imname, maxch)
	    gnum = 1
	}

	# Add image section back onto image name

	if (Memc[section] != EOS)
	    call strcat (Memc[section], imname, maxch)

	# Free temporary memory

	call sfree (sp)
end

