# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achts (a, b, nelem, ty_b)

short	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtsb (a, b, nelem)
	case TY_USHORT:
	    call achtsu (a, b, nelem)
	case TY_CHAR:
	    call achtsc (a, b, nelem)
	case TY_SHORT:
	    call achtss (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtsi (a, b, nelem)
	case TY_LONG:
	    call achtsl (a, b, nelem)
	case TY_REAL:
	    call achtsr (a, b, nelem)
	case TY_DOUBLE:
	    call achtsd (a, b, nelem)
	case TY_COMPLEX:
	    call achtsx (a, b, nelem)
	}
end
