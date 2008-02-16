# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STOPEN -- Create and initialize a new symbol table.  The size of the table
# and the size of the hash index are user defined.  Any number of symbol
# tables may be open simultaneously.  LEN_SYMBTAB is the initial length of
# the symbol table in struct units.  LEN_INDEX is the number of hash threads;
# a good choice for this value is twice the expected number of symbols in the
# table, but good performance can be expected even if the number of symbols
# is several times the size of the index.  The index consumes SZ_INT chars per
# index element.

pointer	procedure stopen (name, len_index, len_stab, sz_sbuf)

char	name[ARB]		# symbol table name (optional)
int	len_index		# number of hash threads in index
int	len_stab		# initial length of STAB
int	sz_sbuf			# initial size of string buffer

pointer	stp
int	stpstr()

begin
	# Allocate symtab descriptor.
	call calloc (stp, LEN_SYMTAB, TY_STRUCT)
	ST_MAGIC(stp) = MAGIC

	# Allocate index.
	call calloc (ST_INDEX(stp), len_index, TY_INT)
	ST_INDEXLEN(stp) = len_index

	# Allocate string buffer.  The first char of storage, at offset 0,
	# is set to EOS so that offset 0 may be used to reference the null
	# string.

	call malloc (ST_SBUFP(stp), sz_sbuf, TY_CHAR)
	ST_SBUFINC(stp) = max (1, nint (sz_sbuf * INC_START))
	ST_SBUFLEN(stp) = sz_sbuf
	ST_SBUFOP(stp)  = 1
	ST_SBUFNGROW(stp) = 0
	Memc[ST_SBUFP(stp)] = EOS

	# Allocate symbol table.  The initial STABOP (offset into STAB) is set
	# to 1 rather than 0 since 0 as an STAB offset is used to mark the end
	# of a list.

	call malloc (ST_STABP(stp), len_stab, TY_STRUCT)
	ST_STABINC(stp) = max (1, nint (len_stab * INC_START))
	ST_STABLEN(stp) = len_stab
	ST_STABOP(stp)  = 1
	ST_STABNGROW(stp) = 0

	# Save the symbol table name in the string buffer.  This name is
	# for documentation purposes only (it is printed by STINFO).

	ST_NAME(stp) = stpstr (stp, name, 0)

	return (stp)
end
