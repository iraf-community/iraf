include defs

# POICOD -- Called to process a declaration of type "pointer".

subroutine poicod (declare_variable)

integer	declare_variable
include COMMON_BLOCKS
string	spointer XPOINTER

# Fortran declarations for the MEM common.
string	p1	"logical Memb(1024)"
string	p2	"integer*2 Memc(1024)"
string	p3	"integer*2 Mems(1024)"
string	p4	"integer Memi(1024)"
string	p5	"integer Meml(1024)"
string	p6	"real Memr(1024)"
string	p7	"double precision Memd(1024)"
string	p8	"complex Memx(1024)"
string	p9	"equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)"
string	pa	"common /Mem/ Memd"

	# Output declarations only once per procedure declarations section.
	# The flag memflg is cleared when processing of a procedure begins.

	if (memflg == NO) {
	    call poidec (p1)
	    call poidec (p2)
	    call poidec (p3)
	    call poidec (p4)
	    call poidec (p5)
	    call poidec (p6)
	    call poidec (p7)
	    call poidec (p8)
	    call poidec (p9)
	    call poidec (pa)
	    memflg = YES
	}

	if (declare_variable == YES) {
	    call outtab
	    call outstr (spointer)
	}
end


# POIDEC -- Output a poicod declaration statement.

subroutine poidec (str)

character str

	call outtab
	call outstr (str)
	call outdon
end
