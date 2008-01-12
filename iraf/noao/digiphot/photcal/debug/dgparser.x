include	"../lib/parser.h"
include	"../lib/prstruct.h"
include "../lib/preval.h"
include "debug.h"

# DG_PRVDUMP -- Dump the parser variables.

procedure dg_prvdump  (label)

char	label[ARB]	# string label

int	fd

include "../parser/parser.com"

bool	clgetb()
int	open()

begin
	# Debug ?
	if (!clgetb ("debug.parvars"))
	    return

	# Open the log file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Put in the time stamp.
	call dg_ptime (fd, label)

	# Dump the table pointers.
	call fprintf (fd, "(obstable=%d) (cattable=%d) (partable=%d)\n")
	    call pargi (obstable)
	    call pargi (cattable)
	    call pargi (partable)
	call fprintf (fd, "(settable=%d) (exttable=%d) (trntable=%d)\n")
	    call pargi (settable)
	    call pargi (exttable)
	    call pargi (trntable)
	call fprintf (fd, "(trobstable=%d) (trcattable=%d)\n")
	    call pargi (trobstable)
	    call pargi (trcattable)
	call fprintf (fd, "(tfobstable=%d) (tfcattable=%d)\n")
	    call pargi (tfobstable)
	    call pargi (tfcattable)
	call fprintf (fd, "(tpartable=%d)\n")
	    call pargi (tpartable)

	# Dump the counters.
	call fprintf (fd, "(nerrors=%d) (nwarnings=%d)\n")
	    call pargi (nerrors)
	    call pargi (nwarnings)
	call fprintf (fd,
	    "(nobsvars=%d) (ncatvars=%d) (nfitpars=%d) (ntotpars=%d)\n")
	    call pargi (nobsvars)
	    call pargi (ncatvars)
	    call pargi (nfitpars)
	    call pargi (ntotpars)
	call fprintf (fd, "(nseteqs=%d) (nexteqs=%d) (ntrneqs=%d)\n")
	    call pargi (nseteqs)
	    call pargi (nexteqs)
	    call pargi (ntrneqs)

	# Dump the column limits.
	call fprintf (fd, "(mincol=%d)\n")
	    call pargi (mincol)
	call fprintf (fd, "(minobscol=%d) (maxobscol=%d)\n")
	    call pargi (minobscol)
	    call pargi (maxobscol)
	call fprintf (fd, "(mincatcol=%d) (maxcatcol=%d)\n")
	    call pargi (mincatcol)
	    call pargi (maxcatcol)

	# Dump the flags.
	call fprintf (fd, "(flageqsect=%d) (flagerrors=%d)\n")
	    call pargi (flageqsect)
	    call pargi (flagerrors)

	# Close the dump file.
	call close (fd)
end


# DG_PRTDUMP -- Dump the parser symbol table.

procedure dg_prtdump  (label, stp)

char	label[ARB]	# string label
pointer	stp		# symbol table pointer

int	fd
pointer	sym

bool	clgetb()
int	open()
pointer	sthead(), stnext()

begin
	# Debug ?
	if (!clgetb ("debug.partable"))
	    return

	# Open lthe og file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Put in the time stamp.
	call dg_ptime (fd, label)

	# Check for the null pointer.
	if (stp == NULL) {
	    call fprintf (fd, "dg_prdump: Null pointer\n")
	    return
	}	

	# Dump the table.
	sym = sthead (stp)
	while (sym != NULL) {

	    # Dump the symbol information.
	    call dg_symdump (fd, stp, sym)

	    # Dump the substructure information (if any).
	    call dg_subdump (fd, stp, sym)

	    # Advance to next symbol.
	    sym = stnext (stp, sym)
	}

	# Close the dump file.
	call close (fd)
end


# DG_SYMDUMP -- Dump symbol structure.

procedure dg_symdump (fd, stp, sym)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	sym		# symbol pointer


pointer	stname(), strefstab()

begin
	# Check for a null substructure pointer.
	if (sym == NULL) {
	    call fprintf (fd, "-- dg_symdump: Null pointer\n")
	    return
	}

	# Print the main structure.
	call fprintf (fd,
      "-- dg_symdump: (%s) (sym=%d) (offset=%d) (type=%d) (num=%d) (sub=%d)\n")
	    call pargstr (Memc[stname (stp, sym)])
	    call pargi (sym - strefstab (stp, 0))
	    call pargi (sym)
	    call pargi (PSYM_TYPE (sym))
	    call pargi (PSYM_NUM (sym))
	    call pargi (PSYM_SUB (sym))
	call flush (fd)
end

	
# DG_SUBDUMP -- Dump symbol substructure.

procedure dg_subdump (fd, stp, sym)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	sym		# symbol pointer

pointer	ptr

begin
	# Check for a null substructure pointer.
	ptr = PSYM_SUB (sym)
	if (ptr == NULL) {
	    call fprintf (fd, "dg_subdump: Null substructure pointer\n")
	    return
	}

	# Branch according to symbol type.
	switch (PSYM_TYPE (sym)) {
	case PTY_CATVAR, PTY_OBSVAR:
	    call dg_inpdump (fd, stp, ptr)
	case PTY_FITPAR, PTY_CONST:
	    call dg_fitdump (fd, stp, ptr)
	case PTY_SETEQ:
	    call dg_setdump (fd, stp, ptr)
	case PTY_EXTEQ:
	    call dg_extdump (fd, stp, ptr)
	case PTY_TRNEQ:
	    call dg_trndump (fd, stp, ptr)
	default:
	    call fprintf (fd, "dg_subdump: Unknown symbol type\n")
	}
end


# DG_INPDUMP -- Dump the input variable substructure.

procedure dg_inpdump (fd, stp, ptr)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	ptr		# substructure pointer

bool	itob()

begin
	# Print substructure pointer.
	call fprintf (fd, "dg_inpdump: (ptr=%d)\n")
	    call pargi (ptr)

	# Print input, error and weight columns.
	call fprintf (fd, "(input=%d) (error=%d) (weight=%d) (spare=%b)\n")
	    call pargi (PINP_COL (ptr))
	    call pargi (PINP_ERRCOL (ptr))
	    call pargi (PINP_WTSCOL (ptr))
	    call pargb (itob (PINP_SPARE(ptr)))
	call flush (fd)
end


# DG_FITDUMP -- Dump the set equation substructure.

procedure dg_fitdump (fd, stp, ptr)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	ptr		# substructure pointer

begin
	# Print the substructure pointer.
	call fprintf (fd, "dg_fitdump: (ptr=%d)\n")
	    call pargi (ptr)

	# Print the parameter value and delta.
	call fprintf (fd, "(value=%g) (delta=%g)\n")
	    call pargr (PFIT_VALUE (ptr))
	    call pargr (PFIT_DELTA (ptr))
end


# DG_SETDUMP -- Dump the set equation substructure.

procedure dg_setdump (fd, stp, ptr)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	ptr		# substructure pointer

pointer	strefsbuf()

begin
	# Print the substructure pointer.
	call fprintf (fd, "dg_setdump: (ptr=%d)\n")
	    call pargi (ptr)

	# Print the equation string.
	call fprintf (fd, "equation: %d [%s]\n")
	    call pargi (PSEQ_EQ (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_EQ (ptr))])

	# Print the error equation strings.
	call fprintf (fd, "error: %d [%s]\n")
	    call pargi (PSEQ_ERROR (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_ERROR (ptr))])
	call fprintf (fd, "error max: %d [%s]\n")
	    call pargi (PSEQ_ERRMIN (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_ERRMIN (ptr))])
	call fprintf (fd, "error min: %d [%s]\n")
	    call pargi (PSEQ_ERRMAX (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_ERRMAX (ptr))])

	# Print the weight equation strings.
	call fprintf (fd, "weight: %d [%s]\n")
	    call pargi (PSEQ_WEIGHT (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_WEIGHT (ptr))])
	call fprintf (fd, "error max: %d [%s]\n")
	    call pargi (PSEQ_WTSMIN (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_WTSMIN (ptr))])
	call fprintf (fd, "error min: %d [%s]\n")
	    call pargi (PSEQ_WTSMAX (ptr))
	    call pargstr (Memc[strefsbuf (stp, PSEQ_WTSMAX (ptr))])

	# Print the equation code.
	call fprintf (fd, "equation rpn: ")
	call dg_cdump (fd, PSEQ_RPNEQ (ptr))

	# Print the error equation codes.
	call fprintf (fd, "error rpn: ")
	call dg_cdump (fd, PSEQ_RPNERROR (ptr))
	call fprintf (fd, "error min rpn: ")
	call dg_cdump (fd, PSEQ_RPNERRMIN (ptr))
	call fprintf (fd, "error max rpn: ")
	call dg_cdump (fd, PSEQ_RPNERRMAX (ptr))

	# Print the weight equation codes.
	call fprintf (fd, "weight rpn: ")
	call dg_cdump (fd, PSEQ_RPNWEIGHT (ptr))
	call fprintf (fd, "weight min rpn: ")
	call dg_cdump (fd, PSEQ_RPNWTSMIN (ptr))
	call fprintf (fd, "weight max rpn: ")
	call dg_cdump (fd, PSEQ_RPNWTSMAX (ptr))
end


# DG_EXTDUMP -- Dump the extinction equation substructure.

procedure dg_extdump (fd, stp, ptr)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	ptr		# substructure pointer

begin
	# Print the substructure pointer.
	call fprintf (fd, "dg_extdump: (ptr=%d)\n")
	    call pargi (ptr)
end


# DG_TRNDUMP -- Dump the transformation equation substructure.

procedure dg_trndump (fd, stp, ptr)

int	fd		# dump file descriptor
pointer	stp		# table pointer
pointer	ptr		# substructure pointer

int	i

pointer	strefsbuf()

begin
	# Print substructure pointer, number of variables, and parameters
	call fprintf (fd,
	    "dg_trndump: (ptr=%d) (nrcat=%d) (nrobs=%d) (nrvar=%d) (nfcat=%d) ")
	    call pargi (ptr)
	    call pargi (PTEQ_NRCAT (ptr))
	    call pargi (PTEQ_NROBS (ptr))
	    call pargi (PTEQ_NRVAR (ptr))
	    call pargi (PTEQ_NFCAT (ptr))
	call fprintf (fd,
	    "(nfobs=%d) (nfvar=%d) (nvar=%d) (npar=%d) (nfpar=%d)\n")
	    call pargi (PTEQ_NFOBS (ptr))
	    call pargi (PTEQ_NFVAR (ptr))
	    call pargi (PTEQ_NVAR  (ptr))
	    call pargi (PTEQ_NPAR  (ptr))
	    call pargi (PTEQ_NFPAR (ptr))
	call flush (fd)

	# Print reference equation variable offsets.
	call fprintf (fd, "Reference variable offsets:")
	do i = 1, PTEQ_NRVAR (ptr) {
	    call fprintf (fd, " %d")
		call pargi (PTEQ_REFVAR (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print reference equation counters.
	call fprintf (fd, "Reference equation counters:")
	do i = 1, PTEQ_NRVAR (ptr) {
	    call fprintf (fd, " %d")
		call pargi (PTEQ_REFCNT (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print fit equation variable offsets.
	call fprintf (fd, "Fit variable offsets:")
	do i = 1, PTEQ_NFVAR (ptr) {
	    call fprintf (fd, " %d")
		call pargi (PTEQ_FITVAR (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print fit equation counters.
	call fprintf (fd, "Fit equation counters:")
	do i = 1, PTEQ_NFVAR (ptr) {
	    call fprintf (fd, " %d")
		call pargi (PTEQ_FITCNT (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print parameter offsets.
	call fprintf (fd, "Parameter offsets:")
	do i = 1, PTEQ_NPAR (ptr) {
	    call fprintf (fd, " %d")
		call pargi (PTEQ_PAR (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print the parameter values.
	call fprintf (fd, "Parameter values:")
	do i = 1, PTEQ_NPAR (ptr) {
	    call fprintf (fd, " %g")
		call pargr (PTEQ_PARVAL (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print the fitting parameter list.
	call fprintf (fd, "Parameter list:")
	do i = 1, PTEQ_NPAR (ptr) {
	    call fprintf (fd, " %d")
		call pargi (PTEQ_PLIST (ptr, i))
	}
	call fprintf (fd, "\n")
	call flush (fd)

	# Print fit and reference equation strings.
	call fprintf (fd, "Fit: %d [%s]\n")
	    call pargi (PTEQ_FIT (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_FIT (ptr))])
	call fprintf (fd, "Reference: %d [%s]\n")
	    call pargi (PTEQ_REF (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_REF (ptr))])
	call flush (fd)

	# Print error equation strings.
	call fprintf (fd, "Error: %d [%s]\n")
	    call pargi (PTEQ_ERROR (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_ERROR (ptr))])
	call fprintf (fd, "Error max: %d [%s]\n")
	    call pargi (PTEQ_ERRMIN (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_ERRMIN (ptr))])
	call fprintf (fd, "Error min: %d [%s]\n")
	    call pargi (PTEQ_ERRMAX (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_ERRMAX (ptr))])
	call flush (fd)

	# Print weight equation strings.
	call fprintf (fd, "Weight: %d [%s]\n")
	    call pargi (PTEQ_WEIGHT (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_WEIGHT (ptr))])
	call fprintf (fd, "Weight min: %d [%s]\n")
	    call pargi (PTEQ_WTSMIN (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_WTSMIN (ptr))])
	call fprintf (fd, "Weight max: %d [%s]\n")
	    call pargi (PTEQ_WTSMAX (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_WTSMAX (ptr))])
	call flush (fd)

	# Print plot equation strings.
	call fprintf (fd, "Plot x: %d [%s]\n")
	    call pargi (PTEQ_XPLOT (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_XPLOT (ptr))])
	call fprintf (fd, "Plot y: %d [%s]\n")
	    call pargi (PTEQ_YPLOT (ptr))
	    call pargstr (Memc[strefsbuf (stp, PTEQ_YPLOT (ptr))])
	call flush (fd)

	# Print derivative strings.
	call fprintf (fd, "Derivatives:\n")
	do i = 1, PTEQ_NPAR (ptr) {
	    call fprintf (fd, "%d ")
		call pargi (i)
	    if (!IS_INDEFI (PTEQ_DER (ptr, i))) {
		call fprintf (fd, "(%d) [%s]\n")
		    call pargi (PTEQ_DER (ptr, i))
		    call pargstr (Memc[strefsbuf (stp, PTEQ_DER (ptr, i))])
	    } else
		call fprintf (fd, "INDEF\n")
	}
	call flush (fd)

	# Print equation codes.
	call fprintf (fd, "Fit rpn: ")
	call dg_cdump (fd, PTEQ_RPNFIT (ptr))
	call fprintf (fd, "Reference rpn: ")
	call dg_cdump (fd, PTEQ_RPNREF (ptr))

	# Print error equation codes.
	call fprintf (fd, "Error rpn: ")
	call dg_cdump (fd, PTEQ_RPNERROR (ptr))
	call fprintf (fd, "Error min rpn: ")
	call dg_cdump (fd, PTEQ_RPNERRMIN (ptr))
	call fprintf (fd, "Error max rpn: ")
	call dg_cdump (fd, PTEQ_RPNERRMAX (ptr))

	# Print weight equation codes.
	call fprintf (fd, "Weight rpn: ")
	call dg_cdump (fd, PTEQ_RPNWEIGHT (ptr))
	call fprintf (fd, "Weight min rpn: ")
	call dg_cdump (fd, PTEQ_RPNWTSMIN (ptr))
	call fprintf (fd, "Weight max rpn: ")
	call dg_cdump (fd, PTEQ_RPNWTSMAX (ptr))

	# Print plot equation codes.
	call fprintf (fd, "Plot x rpn: ")
	call dg_cdump (fd, PTEQ_RPNXPLOT (ptr))
	call fprintf (fd, "Plot y rpn: ")
	call dg_cdump (fd, PTEQ_RPNYPLOT (ptr))

	# Print derivative codes.
	call fprintf (fd, "Derivative rpn:\n")
	do i = 1, PTEQ_NPAR (ptr) {
	    call fprintf (fd, "%d ")
		call pargi (i)
	    call dg_cdump (fd, PTEQ_RPNDER (ptr, i))
	}

	# Flush output.
	call flush (fd)
end


# DG_CDUMP -- Dump equation code.

procedure dg_cdump (fd, code)

int	fd			# dump file descriptor
pointer	code			# equation code

int	i, n

begin
	# Check the pointer.
	if (code == NULL) {
	    call fprintf (fd, "Null code\n")
	    return
	} else {
	    call fprintf (fd, "(%d) [")
		call pargi (code)
	}
	call flush (fd)

	# Print the equation code.
	i = 1
	n = Memi[code + i - 1]
	while (n != PEV_EOC) {

	    # Print instruction according to type.
	    if (n == PEV_OBSVAR || n == PEV_CATVAR || n == PEV_PARAM  ||
		n == PEV_SETEQ  || n == PEV_EXTEQ  || n == PEV_TRNEQ) {
		call fprintf (fd, "%d,%d ")
		    call pargi (n)
		    i = i + 1
		    call pargi (Memi[code + i - 1])
	    } else if (n == PEV_NUMBER) {
		call fprintf (fd, "%d,%g ")
		    call pargi (n)
		    i = i + 1
		    call pargr (Memr[code + i - 1])
	    } else {
		call fprintf (fd, "%d ")
	    	    call pargi (Memi[code + i - 1])
	    }

	    # Get next instruction.
	    i = i + 1
	    n = Memi[code + i - 1]
	}
	call fprintf (fd, "%d]\n")
	    call pargi (PEV_EOC)

	# Flush output.
	call flush (fd)
end
