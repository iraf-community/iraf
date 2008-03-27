# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<mach.h>
include	"mwcs.h"
include	"mwsv.h"

# MW_LOAD -- Load a saved MWCS into a descriptor.  The saved MWCS will
# have been created in a previous call to MW_SAVE.  In its saved form,
# the MWCS is a machine independent binary array of arbitrary length.

procedure mw_load (mw, bp)

pointer	mw			#I pointer to MWCS descriptor
pointer	bp			#I pointer to save buffer, type char

size_t	sz_val
pointer	sp, sv, ct, ip, ms, wp
#pointer	cw
int	nelem, nwcs, lenwcs, i
#int	n
size_t	sz_nelem, mslen
#size_t	cwlen
errchk	syserrs, malloc
pointer	coerce()
#long	pl_l2pi()

begin
	call smark (sp)
	sz_val = LEN_SVHDR
	call salloc (sv, sz_val, TY_STRUCT)

	# Get the save header.
	ip = coerce (bp, TY_CHAR, TY_POINTER)
#	call miiupk32 (Memi[ip], Memi[sv], LEN_SVHDR, TY_INT)
	sz_val = LEN_SVHDR
	call miiupk64 (Memp[ip], Memp[sv], sz_val, TY_STRUCT)
	if (SV_MAGIC(sv) != MWSV_MAGIC)
	    call syserrs (SYS_MWMAGIC, "MWCS save file")
	if (SV_VERSION(sv) < 2) {
	    call syserrs (SYS_MWMAGIC, "invalid MWSV version")
	}

#	cwlen = SV_CWCSLEN(sv)
	mslen = SV_MWSVLEN(sv)

	# Prior to MWSV version 1 lenwcs and nwcs were not recorded.
	if (SV_VERSION(sv) < 1) {
	    lenwcs = MWSV_LENWCS0
	    nwcs = (mslen - MWSV_BASELEN) / lenwcs
	} else {
	    lenwcs = SV_LENWCS(sv)
	    nwcs = SV_NWCS(sv)
	}

#	call salloc (cw, cwlen, TY_SHORT)
	call salloc (ms, mslen, TY_STRUCT)

	# Unpack the saved MWSV descriptor.  Due to a bug in MWCS prior to
	# V2.10.4 IRAF the packed descriptor was erroneously encoded using
	# miipak32, so if unpacking with miiupk16 doesn't work try using
	# miiupk32.  This should allow old saved MWCS written on a similar
	# architecture to still be read - the data is not portable however
	# unless miipak16 is used, since pl_p2li produces a short array.

	# PLIO does not have 64-bit support :-p

	ip = coerce (bp + SV_MWSVOFF(sv), TY_CHAR, TY_POINTER)
#	call miiupk16 (Memi[ip], Memi[cw], SV_CWCSLEN(sv), TY_SHORT)
#	n = pl_l2pi (Memi[cw], 1, Memi[ms], mslen)
#	if (MI_MAGIC(ms) != MWCS_MAGIC) {
#	    call miiupk32 (Memi[ip], Memi[cw], SV_CWCSLEN(sv), TY_INT)
#	    n = pl_l2pi (Memi[cw], 1, Memi[ms], mslen)
#	}
	call miiupk64 (Memp[ip], Memp[ms], mslen, TY_STRUCT)

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

	# Copy the MWSV descriptor to the active MWCS descriptor.  This
	# assumes that the base descriptor and the WCS sub-descriptor have
	# identical structures, except for the length of each element.

	sz_val = LEN_BASEMWCS
	call amovp (Memp[ms], Memp[mw], sz_val)
	nelem = min (lenwcs, LEN_WCS)
	do i = 1, nwcs {
	    wp = MI_WCSP(mw,i)
	    sz_val = nelem
	    call amovp (Memp[MS_WCSP(ms,i,lenwcs)], Memp[wp], sz_val)
	    if (nelem < LEN_WCS) {
		sz_val = LEN_WCS-nelem
		call aclrp (Memp[wp+nelem], sz_val)
	    }
	}
	do i = nwcs+1, MAX_WCS {
	    sz_val = LEN_WCS
	    call aclrp (Memp[MI_WCSP(mw,i)], sz_val)
	}

	# Initialize the axis map (not preserved over a save/load).
	do i = 1, MI_NDIM(mw) {
	    MI_AXNO(mw,i) = i
	    MI_PHYSAX(mw,i) = i
	}

	# Load the data buffer.
	sz_nelem = SV_DBUFLEN(sv)
	if (sz_nelem > 0) {
	    ip = coerce (bp + SV_DBUFOFF(sv), TY_CHAR, TY_DOUBLE)
	    call malloc (MI_DBUF(mw), sz_nelem, TY_DOUBLE)
	    call miiupkd (Memd[ip], D(mw,1), sz_nelem, TY_DOUBLE)
	    MI_DBUFUSED(mw) = sz_nelem
	    MI_DBUFLEN(mw) = sz_nelem
	}

	# Load the string buffer.
	nelem = SV_SBUFLEN(sv)
	if (nelem > 0) {
	    ip = coerce (bp + SV_SBUFOFF(sv), TY_CHAR, TY_CHAR)
	    sz_val = nelem
	    call malloc (MI_SBUF(mw), sz_val, TY_CHAR)
	    call miiupk8 (Memc[ip], S(mw,1), sz_val, TY_CHAR)
	    MI_SBUFUSED(mw) = nelem
	    MI_SBUFLEN(mw) = nelem
	}

	# Set the default WCS.
	call mw_sdefwcs (mw)
	call sfree (sp)
end
