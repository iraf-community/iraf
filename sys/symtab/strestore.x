# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STRESTORE -- Restore to memory a symbol table previously saved in a file
# with STSAVE.  The file must be positioned to the correct offset before
# we are called.  STRESTORE is called in place of STOPEN and returns a
# symtab descriptor pointer as the function value.  The symbol table is
# restored to exactly the state it was in when STSAVE was called.  Note
# that since SYMTAB symbol tables use only relative offsets internally,
# the data structures may be relocated anywhere in memory when they are
# read back from the file.  The symbol table is stored externally in a
# machine independent binary file.

pointer procedure strestore (fd)

int	fd			# file from which symbol table is to be read

int	nelem
pointer	stp, stab, sbuf, index
int	miireadc(), miireadi()
errchk	miireadc, miireadi
define	readerr_ 91

begin
	index = NULL
	stab  = NULL
	sbuf  = NULL

	# Read symbol table descriptor.
	call malloc (stp, LEN_SYMTAB, TY_STRUCT)
	if (miireadi (fd, Memi[stp], LEN_SYMTAB) < LEN_SYMTAB)
	    goto readerr_

	if (ST_MAGIC(stp) != MAGIC)
	    call error (1, "strestore: bad magic in save file")

	# Read the hash table index.
	nelem = ST_INDEXLEN(stp)
	call malloc (index, nelem, TY_INT)
	if (miireadi (fd, Memi[index], nelem) < nelem)
	    goto readerr_

	# Read the symbol table data.
	nelem = ST_STABLEN(stp)
	call malloc (stab, nelem, TY_STRUCT)
	if (miireadi (fd, Memi[stab], nelem) < nelem)
	    goto readerr_

	# Read the string buffer.
	nelem = ST_SBUFLEN(stp)
	call malloc (sbuf, nelem, TY_CHAR)
	if (miireadc (fd, Memc[sbuf], nelem) < nelem)
	    goto readerr_

	ST_INDEX(stp) = index
	ST_SBUFP(stp) = sbuf
	ST_STABP(stp) = stab

	return (stp)

readerr_
	call mfree (sbuf,  TY_CHAR)
	call mfree (stab,  TY_STRUCT)
	call mfree (index, TY_INT)
	call mfree (stp,   TY_STRUCT)

	call error (2, "strestore: unexpected EOF")
end
