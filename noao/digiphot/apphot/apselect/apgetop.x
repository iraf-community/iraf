include <evexpr.h>
include "../lib/apkeysdef.h"

define	MAX_NRANGES	50

# AP_GETOP -- Procedure to fetch an operand for evexpr.

procedure ap_getop (operand, o)

char	operand[ARB]	# operand name
pointer	o		# operand output

pointer	apkey
common	/kycommon/ apkey
errchk	ap_getfield()

begin
	call ap_getfield (apkey, operand, o)
end


# AP_GETSET -- Porcedure to pass the apselect structure in a common block.

procedure ap_getset (key)

pointer	key	# apselect strucuture

pointer	apkey
common	/kycommon/ apkey

begin
	apkey = key	
end


# AP_GETFIELD -- Procedure to select an apphot field.

procedure ap_getfield (key, field, o)

pointer	key		# pointer to select strucuture
char	field[ARB]	# field to evaluated
pointer	o		# operand

int	type, maxnelems, nelems
pointer	sp, root, ranges, list
bool	ap_kybool()
int	ap_kytype(), ap_kyinteger(), decode_ranges()
real	ap_kyreal()
errchk	ap_kytype(), ap_kybool(), ap_kyreal(), ap_kystr()

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ranges, SZ_FNAME, TY_CHAR)
	call salloc (list, 3 * MAX_NRANGES + 1, TY_INT)

	# Select the field.
	call strupr (field, field, SZ_FNAME)
	type = ap_kytype (key, field, Memc[root], Memc[ranges], maxnelems)
	if (Memc[ranges] == EOS) {
	    nelems = 1
	    Memi[list] = 1
	} else if (decode_ranges (Memc[ranges], Memi[list], MAX_NRANGES,
	    nelems) == ERR)
	    call error (0, "Cannot decode range string")

	# Decode the value.
	switch (type) {
	case TY_BOOL:
	    if (nelems == 1) {
	        call xev_initop (o, 0, TY_BOOL)
	        O_VALB(o) = ap_kybool (key, Memc[root], Memi[list])
	    } else {
		call eprintf ("Error decoding boolean field array: %s\n")
		    call pargstr (field)
		call error (0, "Boolean arrays not allowed in expressions.")
	    }
	case TY_INT:
	    if (nelems == 1) {
	        call xev_initop (o, 0, TY_INT)
	        O_VALI(o) = ap_kyinteger (key, Memc[root], Memi[list])
	    } else {
		call eprintf ("Error decoding integer field array: %s\n")
		    call pargstr (field)
		call error (0, "Integer arrays not allowed in expressions.")
	    }
	case TY_REAL:
	    if (nelems == 1) {
		call xev_initop (o, 0, TY_REAL)
	        O_VALR(o) = ap_kyreal (key, Memc[root], Memi[list])
	    } else {
		call eprintf ("Error decoding real array field: %s\n")
		    call pargstr (field)
		call error (0, "Real arrays not allowed in expressions.")
	    }
	default:
	    if (nelems == 1) {
	        call xev_initop (o, SZ_LINE, TY_CHAR)
	        call ap_kystr (key, Memc[root], Memi[list], O_VALC(o), SZ_LINE)
	    } else {
		call eprintf ("Error decoding char array field: %s\n")
		    call pargstr (field)
		call error (0, "Character arrays not allowed in expressions.")
	    }
	}

	call sfree (sp)
end
