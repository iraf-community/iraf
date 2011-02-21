include <tbset.h>

# B.Simon	16-Apr-99	first code

# SPP_TYPE -- Retrieve the spp type of a table column

int procedure spp_type (cp)

pointer	cp		# i: Column pointer
#--
int	dtype
int	tbcigi()

begin
	if (cp == NULL) {
	    # Null column pointer indicates row number

	    dtype = TY_INT

	} else {
	    # Table data types store strings as negative values

	    dtype = tbcigi (cp, TBL_COL_DATATYPE)
	    if (dtype < 0)
		dtype = TY_CHAR
	}

	return (dtype)
end
