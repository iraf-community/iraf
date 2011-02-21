
include defs

# ENTXKW -- Enter all XPP directives in the symbol table.

subroutine entxkw

include	COMMON_BLOCKS

string	sbool	"x$bool"
string	schar	"x$char"
string	sshort	"x$short"
string	sint	"x$int"
string	slong	"x$long"
string	sreal	"x$real"
string	sdble	"x$dble"
string	scplx	"x$cplx"
string	spntr	"x$pntr"
string	sfchr	"x$fchr"
string	sfunc	"x$func"
string	ssubr	"x$subr"
string	sextn	"x$extn"

string dbool	"logical"
string dchar	"integer*2"
string dshort	"integer*2"
string dint	"integer"
string dlong	"integer"
string dpntr	"integer"
string dreal	"real"
string ddble	"double precision"
string dcplx	"complex"
string dfchr	"character"
string dfunc	"function"
string dsubr	"subroutine"
string dextn	"external"

	call entdef (sbool,  dbool,  xpptbl)
	call entdef (schar,  dchar,  xpptbl)
	call entdef (sshort, dshort, xpptbl)
	call entdef (sint,   dint,   xpptbl)
	call entdef (slong,  dlong,  xpptbl)
	call entdef (spntr,  dpntr,  xpptbl)
	call entdef (sreal,  dreal,  xpptbl)
	call entdef (sdble,  ddble,  xpptbl)
	call entdef (scplx,  dcplx,  xpptbl)
	call entdef (sfchr,  dfchr,  xpptbl)
	call entdef (sfunc,  dfunc,  xpptbl)
	call entdef (ssubr,  dsubr,  xpptbl)
	call entdef (sextn,  dextn,  xpptbl)
end
