# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtr (a, b, nelem, ty_b)

real	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtrb (a, b, nelem)
	case TY_USHORT:
	    call achtru (a, b, nelem)
	case TY_CHAR:
	    call achtrc (a, b, nelem)
	case TY_SHORT:
	    call achtrs (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtri (a, b, nelem)
	case TY_LONG:
	    call achtrl (a, b, nelem)
	case TY_REAL:
	    call achtrr (a, b, nelem)
	case TY_DOUBLE:
	    call achtrd (a, b, nelem)
	case TY_COMPLEX:
	    call achtrx (a, b, nelem)
	}
end
