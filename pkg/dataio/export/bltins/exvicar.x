include <mach.h>
include "../export.h"


define	SZ_VICHDR	1024


# EX_VICAR - Write the evaluated expressions as a VICAR2 format file.

procedure ex_vicar (ex)

pointer	ex					#i task struct pointer

pointer	sp, hdr, user, date, arch
int	i, flags
char	space

int	envfind(), strncmp(), strlen()
long	clktime()

begin
	# Check to see that we have the correct number of expressions to
	# write this format.
	flags = EX_OUTFLAGS(ex)
	if (EX_NEXPR(ex) != 1 && !bitset(flags, OF_BAND))
	    call error (7, "Invalid number of expressions for VICAR file.")
	if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
	    call error (7, "Line storage illegal for VICAR file.")

	# Write the header to the file.
	call smark (sp)
	call salloc (hdr, SZ_VICHDR, TY_CHAR)
	call salloc (user, SZ_FNAME, TY_CHAR)
	call salloc (date, SZ_FNAME, TY_CHAR)
	call salloc (arch, SZ_FNAME, TY_CHAR)

	space = ' '
	call amovkc (space, Memc[hdr], SZ_VICHDR)
	call aclrc (Memc[user], SZ_FNAME)
	call aclrc (Memc[date], SZ_FNAME)
	call aclrc (Memc[arch], SZ_FNAME)

	# Header keywords:
	call getuid (Memc[user], SZ_FNAME)
	call cnvtime (clktime(long(0)), Memc[date], SZ_FNAME)
	call sprintf (Memc[hdr], SZ_VICHDR, 
	   "LBLSIZE=%d FORMAT='%s' TYPE='IMAGE' BUFSIZ=20480 DIM=3 EOL=0 RECSIZE=%d ORG='%s' NL=%d NS=%d NB=%d N1=%d N2=%d N3=%d N4=0 NBB=0 NLB=0 INTFMT='%s' REALFMT='%s' TASK='EXPORT' USER='%s' DAT_TIM='%s'                    ")

		call pargi (SZ_VICHDR)			# LBLSIZE
		switch (EX_OUTTYPE(ex)) {		# FORMAT
                case TY_UBYTE:      call pargstr ("BYTE")
                case TY_SHORT:      call pargstr ("HALF")
                case TY_INT:        call pargstr ("FULL")
                case TY_LONG:       call pargstr ("FULL")
                case TY_REAL:       call pargstr ("REAL")
                case TY_DOUBLE:     call pargstr ("DOUB")
		}
		call pargi (EX_OCOLS(ex))		# RECSIZE
		if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
		    call pargstr ("BIL")		# ORG
		else
		    call pargstr ("BSQ")
		call pargi (EX_OROWS(ex))		# NL
		call pargi (EX_OCOLS(ex))		# NS
		call pargi (EX_NEXPR(ex))		# NB
		call pargi (EX_OCOLS(ex))		# N1
		call pargi (EX_OROWS(ex))		# N2
		call pargi (EX_NEXPR(ex))		# N3
		if (BYTE_SWAP2 == NO)
		    call pargstr ("HIGH")		# INTFMT
		else
		    call pargstr ("LOW")
		if (IEEE_USED == YES) {			# REALFMT
		    if (envfind ("arch", Memc[arch], SZ_FNAME) != ERR) {
			# If this is a DECstation we have a different IEEE.
			if (strncmp(Memc[arch], ".d", 2) == 0)
		            call pargstr ("RIEEE")
			else
		            call pargstr ("IEEE")
		    }
		} else {
		    # Assume it's a VAX.
		    call pargstr ("VAX")
		}
		call pargstr (Memc[user])		# USER
		call pargstr (Memc[date])		# DAT_TIM

	i = SZ_VICHDR
	while (Memc[hdr+i-1] != EOS && i > 0)
	    i = i - 1
	Memc[hdr+i-1] = ' '

	call strpak (Memc[hdr], Memc[hdr], SZ_VICHDR)
	call write (EX_FD(ex), Memc[hdr], strlen(Memc[hdr])/SZB_CHAR)
	call sfree (sp)

	# Fix the output pixel type to single bytes.
	switch (EX_OUTTYPE(ex)) {
        case TY_UBYTE:   call ex_do_outtype (ex, "b1")
        case TY_SHORT:   call ex_do_outtype (ex, "i2")
        case TY_INT:     call ex_do_outtype (ex, "i4")
        case TY_LONG:    call ex_do_outtype (ex, "i4")
        case TY_REAL:    call ex_do_outtype (ex, "n4")
        case TY_DOUBLE:  call ex_do_outtype (ex, "n8")
	}
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Finally, evaluate the expressions and write the image.
	if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND))
	    call ex_no_interleave (ex)
end
