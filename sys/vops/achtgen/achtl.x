# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtl (a, b, nelem, ty_b)

long	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtlb (a, b, nelem)
	case TY_USHORT:
	    call achtlu (a, b, nelem)
	case TY_CHAR:
	    call achtlc (a, b, nelem)
	case TY_SHORT:
	    call achtls (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtli (a, b, nelem)
	case TY_LONG:
	    call achtll (a, b, nelem)
	case TY_REAL:
	    call achtlr (a, b, nelem)
	case TY_DOUBLE:
	    call achtld (a, b, nelem)
	case TY_COMPLEX:
	    call achtlx (a, b, nelem)
	}
end
