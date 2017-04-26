# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# KI_MAPNAME -- Map a logical node name to a literal node name.  Logical
# node names may be defined in the environment as aliases for "hardcoded"
# hosts-file node names.  To distinguish logical node names from other
# environment variables the value string must end in the node delimiter
# character "!".   For example, "set alpha = foo.bar.edu!" defines logical
# node alpha to be the same as foo.tar.edu.  The "!" suffix also allows
# the logical node name to be used in file references, e.g. "alpha!pathname".
# 
# If the input name is a logical node name the translated value is returned
# in newname and the number of characters output is returned as the function
# value (as for envfind).  If the input name is not a logical node name
# zero is returned as the function value.
#
# It might make sense to allow multiple indirection on name translations,
# but this is not currently supported.

int procedure ki_mapname (name, newname, maxch)

char	name[ARB]		#I input logical node name
char	newname[ARB]		#O output translated name
int	maxch			#I max chars out

int	nchars
int	envfind()

begin
	nchars = envfind (name, newname, maxch)
	if (nchars > 1)
	    if (newname[nchars] == '!') {
		newname[nchars] = EOS
		return (nchars - 1)
	    }

	newname[1] = EOS
	return (0)
end
