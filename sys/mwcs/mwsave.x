# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"mwcs.h"

# MW_SAVE -- Save the contents of a MWCS descriptor, i.e., the MWCS object,
# in a machine independent binary array.  This may be stored in a file or
# database, passed through a network interface, etc., and later reopened
# on a descriptor with MW_LOAD or MW_OPEN.

int procedure mw_save (o_mw, bp, buflen)

pointer	o_mw			#I pointer to MWCS descriptor
pointer	bp			#U pointer to save buffer of type char
int	buflen			#U allocated length of save buffer

int	nchars, olen
pointer	mw, sp, sv, op, oo
errchk	coerce, realloc, mw_newcopy
pointer	coerce(), mw_newcopy()
int	pl_p2li()

begin
	call smark (sp)

	# We save a new copy of the MWCS, rather than the MWCS itself,
	# to discard any dead storage and to cause the runtime descriptor
	# pointers to be set to NULL.

	mw = mw_newcopy (o_mw)

	# Clear runtime fields that cannot be meaningfully saved.
	MI_WCS(mw) = NULL
	MI_REFIM(mw) = NULL
	call aclri (MI_AXNO(mw,1), MAX_DIM)
	call aclri (MI_AXVAL(mw,1), MAX_DIM)
	call aclri (MI_PHYSAX(mw,1), MAX_DIM)

	# Compress the main header to save space.
	call salloc (oo, MI_LEN(mw) * 2, TY_INT)
	olen = pl_p2li (Memi[mw], 1, Memi[oo], MI_LEN(mw))

	# Determine how much space will be needed.
	nchars = LEN_SVHDR * SZ_STRUCT + olen * SZ_INT +
	    MI_DBUFUSED(mw) * SZ_DOUBLE +
	    (MI_SBUFUSED(mw) + SZB_CHAR-1) / SZB_CHAR

	# Get the space.
	if (nchars > buflen) {
	    call realloc (bp, nchars, TY_CHAR)
	    buflen = nchars
	}

	# Prepare the save header.
	sv = coerce (bp, TY_CHAR, TY_STRUCT)
	call aclri (Memi[sv], LEN_SVHDR)

	SV_MAGIC(sv)   = MWCS_MAGIC
	SV_CWCSLEN(sv) = olen
	SV_MWCSLEN(sv) = MI_LEN(mw)
	SV_DBUFLEN(sv) = MI_DBUFUSED(mw)
	SV_SBUFLEN(sv) = MI_SBUFUSED(mw)
	SV_MWCSOFF(sv) = LEN_SVHDR * SZ_STRUCT
	SV_DBUFOFF(sv) = SV_MWCSOFF(sv) + olen * SZ_INT
	SV_SBUFOFF(sv) = SV_DBUFOFF(sv) + MI_DBUFUSED(mw) * SZ_DOUBLE

	# Store the three segments of the MWCS, i.e., the main descriptor
	# and the data and string buffers.

	op = coerce (bp + SV_MWCSOFF(sv), TY_CHAR, TY_STRUCT)
	call miipak32 (Memi[oo], Memi[op], olen, TY_INT)
	op = coerce (bp + SV_DBUFOFF(sv), TY_CHAR, TY_DOUBLE)
	call miipakd (D(mw,1), Memd[op], SV_DBUFLEN(sv), TY_DOUBLE)
	op = coerce (bp + SV_SBUFOFF(sv), TY_CHAR, TY_CHAR)
	call miipak8 (S(mw,1), Memc[op], SV_SBUFLEN(sv), TY_CHAR)

	call mw_close (mw)
	return (nchars)
end
