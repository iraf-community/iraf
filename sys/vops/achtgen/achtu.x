# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtu (a, b, nelem, ty_b)

short	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtub (a, b, nelem)
	case TY_USHORT:
	    call achtuu (a, b, nelem)
	case TY_CHAR:
	    call achtuc (a, b, nelem)
	case TY_SHORT:
	    call achtus (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtui (a, b, nelem)
	case TY_LONG:
	    call achtul (a, b, nelem)
	case TY_REAL:
	    call achtur (a, b, nelem)
	case TY_DOUBLE:
	    call achtud (a, b, nelem)
	case TY_COMPLEX:
	    call achtux (a, b, nelem)
	}
end
