# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<mach.h>
include	"mwcs.h"

# MW_LOAD -- Load a saved MWCS into a descriptor.  The saved MWCS will
# have been created in a previous call to MW_SAVE.  In its saved form,
# the MWCS is a machine independent binary array of arbitrary length.

procedure mw_load (mw, bp)

pointer	mw			#I pointer to MWCS descriptor
pointer	bp			#I pointer to save buffer, type char

int	nelem, n, i
pointer	sp, sv, ct, ip, oo
errchk	syserrs, malloc
pointer	coerce()
int	pl_l2pi()

begin
	# Get the save header.
	sv = coerce (bp, TY_CHAR, TY_STRUCT)
	if (SV_MAGIC(sv) != MWCS_MAGIC)
	    call syserrs (SYS_MWMAGIC, "MWCS save buffer")

	call smark (sp)
	call salloc (oo, SV_CWCSLEN(sv), TY_INT)

	# Free any storage associated with the old descriptor.
	# Start with any still allocated CTRAN descriptors.

	do i = 1, MAX_CTRAN {
	    ct = MI_CTRAN(mw,i)
	    if (ct != NULL)
		iferr (call mw_ctfree (ct))
		    call erract (EA_WARN)
	}

	# Free the old string and data buffers.
	if (MI_SBUF(mw) != NULL)
	    call mfree (MI_SBUF(mw), TY_CHAR)
	if (MI_DBUF(mw) != NULL)
	    call mfree (MI_DBUF(mw), TY_DOUBLE)

	# Load the main descriptor, which is stored in compressed and MII
	# encoded form in the save buffer.

	nelem = SV_MWCSLEN(sv)
	ip = coerce (bp + SV_MWCSOFF(sv), TY_CHAR, TY_STRUCT)
	call miiupk32 (Memi[ip], Memi[oo], SV_CWCSLEN(sv), TY_INT)
	n = pl_l2pi (Memi[oo], 1, Memi[mw], nelem)
	call aclri (Memi[mw+nelem], LEN_MWCS-nelem)

	# Load the data buffer.
	nelem = SV_DBUFLEN(sv)
	if (nelem > 0) {
	    ip = coerce (bp + SV_DBUFOFF(sv), TY_CHAR, TY_DOUBLE)
	    call malloc (MI_DBUF(mw), nelem, TY_DOUBLE)
	    call miiupkd (Memd[ip], D(mw,1), nelem, TY_DOUBLE)
	    MI_DBUFUSED(mw) = nelem
	    MI_DBUFLEN(mw) = nelem
	}

	# Load the string buffer.
	nelem = SV_SBUFLEN(sv)
	if (nelem > 0) {
	    ip = coerce (bp + SV_SBUFOFF(sv), TY_CHAR, TY_CHAR)
	    call malloc (MI_SBUF(mw), nelem, TY_CHAR)
	    call miiupk8 (Memc[ip], S(mw,1), nelem, TY_CHAR)
	    MI_SBUFUSED(mw) = nelem
	    MI_SBUFLEN(mw) = nelem
	}

	# Set the default WCS.
	call mw_sdefwcs (mw)
	call sfree (sp)
end
