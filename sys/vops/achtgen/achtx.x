# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtx (a, b, nelem, ty_b)

complex	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtxb (a, b, nelem)
	case TY_USHORT:
	    call achtxu (a, b, nelem)
	case TY_CHAR:
	    call achtxc (a, b, nelem)
	case TY_SHORT:
	    call achtxs (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtxi (a, b, nelem)
	case TY_LONG:
	    call achtxl (a, b, nelem)
	case TY_REAL:
	    call achtxr (a, b, nelem)
	case TY_DOUBLE:
	    call achtxd (a, b, nelem)
	case TY_COMPLEX:
	    call achtxx (a, b, nelem)
	}
end
