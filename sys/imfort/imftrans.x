# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# IMF_TRANS -- Translate a host filename into root (includes directory
# prefix) and extension fields.  FIO escape sequence encoding is used on
# the portion of the filename excluding the directory prefix.  Legal host
# filenames are unaffected by the translation except for case conversions,
# i.e., only constructs which are not legal in host filenames are affected
# by the translation, allowing legal host filenames to be passed through
# without change.

procedure imf_trans (fname, root, extn)

char	fname[ARB]			#I input filename
char	root[SZ_PATHNAME]		#O root portion of filename
char	extn[MAX_LENEXTN]		#O extn portion of filename

int	o_root, o_extn, ip, op
int	gstrcpy()

begin
	# Copy out the directory prefix, if any, unchanged.
	call zfnbrk (fname, o_root, o_extn)
	op = gstrcpy (fname, root, o_root-1) + 1
	ip = o_root

	# Perform escape sequence encoding and parse into root and extn.
	call vfn_encode (fname, ip, root[op], o_root, extn, o_extn)
end
