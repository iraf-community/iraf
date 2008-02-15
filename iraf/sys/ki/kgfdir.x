# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KGFDIR -- Get the next filename from a directory file.  Rather than make a
# call over the net to read each filename, which would be very slow, we read
# filenames in large batches and return filenames out of our internal buffer.

procedure kgfdir (chan, osfn, maxch, status)

int	chan			# channel descriptor
char	osfn[maxch]		# receives packed OS filename
int	maxch			# maxchars out
int	status			# answer; ok or err

pointer	dp, bp, ip, op
pointer	sp, vfn, root, extn
int	server, nchars, len_root, len_extn
int	ki_sendrcv(), gstrcpy()
include	"kichan.com"
include	"kii.com"
define	quit_ 91

begin
	server = k_node[chan]

	if (server == NULL) {
	    call zgfdir (k_oschan[chan], osfn, maxch, status)

	} else {
	    call smark (sp)
	    call salloc (vfn, SZ_PATHNAME, TY_CHAR)
	    call salloc (root, SZ_FNAME, TY_CHAR)
	    call salloc (extn, SZ_FNAME, TY_CHAR)

	    dp = k_bufp[chan]
	    bp = D_DATA(dp)

	    if (D_IP(dp) >=  D_ITOP(dp)) {
		# Refill buffer.

		# If the last block of data was returned with the EOF flag
		# set then the directory has been exhausted.

		if (D_EOFSEEN(dp) == YES) {
		    status = EOF
		    goto quit_
		}

		p_arg[1]  = k_oschan[chan]
		p_arg[2]  = SZ_DIRDATA
		p_sbuflen = 0

		if (ki_sendrcv (server, KI_ZGFDIR, 0) == ERR) {
		    status = ERR
		    goto quit_
		} else if (p_arg[1] == ERR) {
		    status = ERR
		    goto quit_
		} else if (p_arg[1] <= 0) {
		    status = EOF
		    goto quit_
		}

		nchars = p_arg[1]
		if (nchars <= SZ_SBUF) {
		    call amovc (p_sbuf, Memc[bp], nchars)
		    p_sbuflen = nchars
		} else {
		    call ks_aread (server, Memc[bp], nchars)
		    call ks_await (server, status)

		    if (status != nchars) {
			status = ERR
			goto quit_
		    } else
			call chrupk (Memc[bp], 1, Memc[bp], 1, nchars)
		}

		D_IP(dp)      = bp
		D_ITOP(dp)    = bp + nchars
		D_EOFSEEN(dp) = p_arg[2]
	    }

	    # Return the next filename from the buffer.

	    for (ip=D_IP(dp);  ip < D_ITOP(dp) && Memc[ip] != '\n';  ip=ip+1)
		;
	    Memc[ip] = EOS

	    if (ip > D_IP(dp)) {
		# A kernel server always returns unmapped (IRAF) filenames since
		# the unmapping must be performed on the host where the files
		# are.  We must map these into local host filenames since that
		# is what the kernel interface is supposed to return.

		call vfn_encode (Memc, D_IP(dp),
		    Memc[root], len_root, Memc[extn], len_extn)
		if (len_extn > 0)
		    call vfn_map_extension (Memc[extn], Memc[extn], SZ_FNAME)

		op = vfn + gstrcpy (Memc[root], Memc[vfn], SZ_PATHNAME)
		if (len_extn > 0) {
		    Memc[op] = '.'
		    op = op + 1
		    op = op + gstrcpy (Memc[extn], Memc[op], ARB)
		}

		# Return a packed (local) host filename.
		call strpak (Memc[vfn], osfn, maxch)
	    } else
		op = vfn

	    status = op - vfn
	    if (status <= 0)
		status = EOF

	    D_IP(dp) = ip + 1
quit_
	    call sfree (sp)
	}
end
