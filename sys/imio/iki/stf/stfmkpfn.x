# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stf.h"

# STF_MKPIXFNAME -- Given the root and extn fields of the image header filename,
# construct the pixel file name.  The pixel file has the same root name as
# the header and the first two characters of the extension are the same as for
# the header, if a header extension was given.

procedure stf_mkpixfname (hdr_root, hdr_extn, pixfname, maxch)

char	hdr_root[ARB]		# root name of header file
char	hdr_extn[ARB]		# extension of header file
char	pixfname[maxch]		# receives pixel filename
int	maxch

int	i
char	pix_extn[MAX_LENEXTN]

begin
	call strcpy (STF_DEFPIXEXTN, pix_extn, MAX_LENEXTN)
	if (hdr_extn[1] != EOS) {
	    for (i=1;  i < MAX_LENEXTN;  i=i+1)
		pix_extn[i] = hdr_extn[i]
	}

	call iki_mkfname (hdr_root, pix_extn, pixfname, maxch)
end
