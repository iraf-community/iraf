# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT -- General data type conversion based on the generic routines
# The data types are BUcsilrdx.

procedure acht (a, b, nelem, ty_a, ty_b)

char	a[ARB], b[ARB]
int	ty_a, ty_b, nelem

begin
 	switch (ty_a) {
	case TY_UBYTE:
	    call achtb (a, b, nelem, ty_b)
	case TY_USHORT:
	    call achtu (a, b, nelem, ty_b)
	case TY_CHAR:
	    call achtc (a, b, nelem, ty_b)
	case TY_SHORT:
	    call achts (a, b, nelem, ty_b)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achti (a, b, nelem, ty_b)
	case TY_LONG:
	    call achtl (a, b, nelem, ty_b)
	case TY_REAL:
	    call achtr (a, b, nelem, ty_b)
	case TY_DOUBLE:
	    call achtd (a, b, nelem, ty_b)
	case TY_COMPLEX:
	    call achtx (a, b, nelem, ty_b)
	}
end
