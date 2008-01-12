# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	NTYPES		9

# GETDATATYPE -- Convert a character to an IRAF data type

int procedure getdatatype (ch)

char	ch
int	i, type_code[NTYPES]
int	stridx()

string	types "bcusilrdx"		# Supported data types
data	type_code /TY_UBYTE, TY_CHAR, TY_USHORT, TY_SHORT, TY_INT, TY_LONG,
	TY_REAL, TY_DOUBLE, TY_COMPLEX/

begin
	i = stridx (ch, types)
	if (i == 0)
	   return (ERR)
	else
	    return (type_code[stridx(ch,types)])
end


# DTSTRING -- Convert a datatype to a string

procedure dtstring (datatype, str, maxchar)

int	datatype			# IRAF datatype
char	str[maxchar]			# Output string
int	maxchar				# Maximum characters in string

begin
	switch (datatype) {
	case TY_UBYTE:
	    call strcpy ("unsigned byte", str, maxchar)
	case TY_CHAR:
	    call strcpy ("character", str, maxchar)
	case TY_USHORT:
	    call strcpy ("unsigned short", str, maxchar)
	case TY_SHORT:
	    call strcpy ("short", str, maxchar)
	case TY_INT:
	    call strcpy ("integer", str, maxchar)
	case TY_LONG:
	    call strcpy ("long", str, maxchar)
	case TY_REAL:
	    call strcpy ("real", str, maxchar)
	case TY_DOUBLE:
	    call strcpy ("double", str, maxchar)
	case TY_COMPLEX:
	    call strcpy ("complex", str, maxchar)
	default:
	    call strcpy ("unknown", str, maxchar)
	}
end
