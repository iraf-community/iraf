# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtc (a, b, nelem, ty_b)

char	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtcb (a, b, nelem)
	case TY_USHORT:
	    call achtcu (a, b, nelem)
	case TY_CHAR:
	    call achtcc (a, b, nelem)
	case TY_SHORT:
	    call achtcs (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtci (a, b, nelem)
	case TY_LONG:
	    call achtcl (a, b, nelem)
	case TY_REAL:
	    call achtcr (a, b, nelem)
	case TY_DOUBLE:
	    call achtcd (a, b, nelem)
	case TY_COMPLEX:
	    call achtcx (a, b, nelem)
	}
end
