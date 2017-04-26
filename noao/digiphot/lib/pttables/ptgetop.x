include <evexpr.h>
include "../ptkeysdef.h"

# PT_GETOP -- Procedure to fetch an apphot operand for evexpr.

procedure pt_getop (operand, o)

char	operand[ARB]	# operand name
pointer	o		# operand output

pointer	apkey
common	/kycommon/ apkey
errchk	pt_getfield()

begin
	call pt_getfield (apkey, operand, o)
end


# PT_APSET -- Porcedure to pass the apphot structure in a common block.

procedure pt_apset (key)

pointer	key	# apphot strucuture

pointer	apkey
common	/kycommon/ apkey

begin
	apkey = key	
end


# PT_GETFIELD -- Procedure to select an apphot field.

procedure pt_getfield (key, field, o)

pointer	key		# pointer to select strucuture
char	field[ARB]	# field to evaluated
pointer	o		# operand

int	type, maxnelems, nelems
pointer	sp, root, ranges, list
bool	pt_kybool()
int	pt_kytype(), pt_kyinteger(), decode_ranges()
real	pt_kyreal()
errchk	pt_kytype(), pt_kybool(), pt_kyreal(), pt_kystr()

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ranges, SZ_FNAME, TY_CHAR)
	call salloc (list, 3 * KY_MAXNRANGES + 1, TY_INT)

	# Select the field.
	call strupr (field)
	type = pt_kytype (key, field, Memc[root], Memc[ranges], maxnelems)
	if (Memc[ranges] == EOS) {
	    nelems = 1
	    Memi[list] = 1
	} else if (decode_ranges (Memc[ranges], Memi[list], KY_MAXNRANGES,
	    nelems) == ERR) {
	    call sfree (sp)
	    call error (0, "Cannot decode range string")
	}

	# Decode the value.
	switch (type) {
	case TY_BOOL:
	    if (nelems == 1) {
	        call xev_initop (o, 0, TY_BOOL)
	        O_VALB(o) = pt_kybool (key, Memc[root], Memi[list])
	    } else {
		call sfree (sp)
		call eprintf ("Error decoding boolean field array: %s\n")
		    call pargstr (field)
		call error (0, "Boolean arrays not allowed in expressions.")
	    }
	case TY_INT:
	    if (nelems == 1) {
	        call xev_initop (o, 0, TY_INT)
	        O_VALI(o) = pt_kyinteger (key, Memc[root], Memi[list])
	    } else {
		call sfree (sp)
		call eprintf ("Error decoding integer field array: %s\n")
		    call pargstr (field)
		call error (0, "Integer arrays not allowed in expressions.")
	    }
	case TY_REAL:
	    if (nelems == 1) {
		call xev_initop (o, 0, TY_REAL)
	        O_VALR(o) = pt_kyreal (key, Memc[root], Memi[list])
	    } else {
		call sfree (sp)
		call eprintf ("Error decoding real array field: %s\n")
		    call pargstr (field)
		call error (0, "Real arrays not allowed in expressions.")
	    }
	default:
	    if (nelems == 1) {
	        call xev_initop (o, SZ_LINE, TY_CHAR)
	        call pt_kystr (key, Memc[root], Memi[list], O_VALC(o), SZ_LINE)
	    } else {
		call eprintf ("Error decoding char array field: %s\n")
		call sfree (sp)
		    call pargstr (field)
		call error (0, "Character arrays not allowed in expressions.")
	    }
	}

	call sfree (sp)
end
