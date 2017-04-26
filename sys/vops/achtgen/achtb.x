# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtb (a, b, nelem, ty_b)

char	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtbb (a, b, nelem)
	case TY_USHORT:
	    call achtbu (a, b, nelem)
	case TY_CHAR:
	    call achtbc (a, b, nelem)
	case TY_SHORT:
	    call achtbs (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtbi (a, b, nelem)
	case TY_LONG:
	    call achtbl (a, b, nelem)
	case TY_REAL:
	    call achtbr (a, b, nelem)
	case TY_DOUBLE:
	    call achtbd (a, b, nelem)
	case TY_COMPLEX:
	    call achtbx (a, b, nelem)
	}
end
