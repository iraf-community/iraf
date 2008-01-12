# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achtd (a, b, nelem, ty_b)

double	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtdb (a, b, nelem)
	case TY_USHORT:
	    call achtdu (a, b, nelem)
	case TY_CHAR:
	    call achtdc (a, b, nelem)
	case TY_SHORT:
	    call achtds (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtdi (a, b, nelem)
	case TY_LONG:
	    call achtdl (a, b, nelem)
	case TY_REAL:
	    call achtdr (a, b, nelem)
	case TY_DOUBLE:
	    call achtdd (a, b, nelem)
	case TY_COMPLEX:
	    call achtdx (a, b, nelem)
	}
end
