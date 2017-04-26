# AT_DTYPE -- Decode the field data type.

define	NTYPES	6

# AT_DTYPE -- Given a single character data type from the set [csilrd] return
# the appropriate integer type,

int procedure at_dtype (c)

char    c

int     type_codes[NTYPES], i
string  types "csilrd"
int     stridx()
data    type_codes /TY_CHAR, TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE/

begin
        i = stridx (c, types)
        if (i == 0)
            return (TY_CHAR)
        else
            return (type_codes[stridx(c,types)])
end


# AT_ITYPE -- Given an integer code from the set TY_CHAR, TY_SHORT, TY_INT,
# TY_LONG, TY_REAL, and TY_DOUBLE return the appropriate character code
# from the set [csilrd].

char procedure at_itype (itype)

int	itype		#I the integer data type

char	c

begin
	switch (itype) {
	case TY_CHAR:
	    c = 'c'
	case TY_SHORT:
	    c = 's'
	case TY_INT:
	    c = 'i'
	case TY_LONG:
	    c = 'l'
	case TY_REAL:
	    c = 'r'
	case TY_DOUBLE:
	    c = 'd'
	default:
	    c = 'c'
	}

	return (c)
end
