# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "fmio.h"

# FM_LFOPEN -- Open an lfile.  This routine is designed to be called with a
# pseudo-filename identifying the FMIO datafile, lfile therein, and file type.
# This is necessary to conform to the binary file driver interface standard.

procedure fm_lfopen (pk_lfname, mode, lf_chan)

char	pk_lfname[ARB]		#I encoded lfile specification (packed char)
int	mode			#I file access mode
int	lf_chan			#O i/o channel assigned (descriptor)

size_t	sz_val
int	flags, lfile, type
long	np, i
pointer	sp, lfname, fm, lf, pt, pm
int	kmalloc(), krealloc(), fm_lfparse()

include "fmio.com"

define	err_ 91

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (lfname, sz_val, TY_CHAR)

	flags = 0

	# Parse the file spec.
	sz_val = SZ_FNAME
	call strupk (pk_lfname, Memc[lfname], sz_val)
	if (fm_lfparse (Memc[lfname], fm, lfile, type) == ERR)
	    goto err_
	else if (type == TEXT_FILE)
	    flags = flags + LFF_TEXTFILE

	# Verify input.
	if (FM_MAGIC(fm) != FMIO_MAGIC)
	    goto err_
	else if (lfile < 0 || lfile > FM_NLFILES(fm))
	    goto err_
	else if (lfile == PT_LFILE && mode != READ_ONLY)
	    goto err_		# protect page table!

	lf = FM_FTABLE(fm) + lfile * LEN_FTE

	# Setup lf address table
	do i = 0, num_lf {
	    if ( i == num_lf ) break
	    if ( Memp[lf_ptrs+i] == NULL ) break
	}
	if ( i == num_lf ) {
	    sz_val = num_lf + 1
	    call realloc (lf_ptrs, sz_val, TY_POINTER)
	    num_lf = sz_val
	}
	Memp[lf_ptrs+i] = lf
	lf_chan = i
	#

	# Activate the descriptor?
	if (LF_PAGEMAP(lf) == NULL) {
	    LF_PMLEN(lf) = DEF_PMLEN
	    if (kmalloc (LF_PAGEMAP(lf), LF_PMLEN(lf), TY_LONG) == ERR)
		goto err_

	    pm = LF_PAGEMAP(lf)
	    pt = FM_PTABLE(fm)
	    np = 0

	    # Determine the lfile pages from the global page table.
	    do i = 1, FM_PTNPTE(fm)
		if (Mems[pt+i-1] == lfile) {
		    np = np + 1
		    if (np > LF_PMLEN(lf)) {
			LF_PMLEN(lf) = (np+INC_PMLEN-1) / INC_PMLEN * INC_PMLEN
			if (krealloc (pm, LF_PMLEN(lf), TY_LONG) == ERR)
			    goto err_
			LF_PAGEMAP(lf) = pm
		    }
		    Meml[pm+np-1] = i
		}

	    LF_NPAGES(lf) = np
	}

	# Mode dependent processing.
	if (mode == NEW_FILE || and (LF_FLAGS(lf), LFF_DELETED) != 0) {
	    LF_FSIZE(lf) = 0
	    LF_FLAGS(lf) = flags
	}
	    
	LF_FM(lf) = fm
	LF_STATUS(lf) = 0
	LF_FLAGS(lf) = or (LFF_ALLOCATED,
	    and (LF_FLAGS(lf), not(LFF_IOINPROGRESS)))

	FM_DHMODIFIED(fm) = YES

	call fmio_tick (fm)
	call sfree (sp)
	return
err_
	lf_chan = ERR
	call sfree (sp)
end
