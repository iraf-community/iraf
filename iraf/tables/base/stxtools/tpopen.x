include	"template.h"

# TP_OPEN -- Expand a group image template into a list of image names.

# Create an array of image names that contain the group specification. 
# Return a pointer to the list of names and the total number of names.
#
# B.Simon	28-Feb-89	Original
# B.Simon	23-Jun-89	Hint character added
# B.Simon	21-Aug-91	Changed template structure

pointer procedure tp_open (imname, def_count, count)

char	imname[ARB]		# i: image template
int	def_count		# i: default image name count
int	count			# o: number of image names
#--
pointer	ptr

errchk	tp_parse, malloc

begin
	# Allocate data structure

	call malloc (ptr, LEN_TPSTRUCT, TY_STRUCT)
	call malloc (TP_ROOTPTR(ptr), SZ_FNAME, TY_CHAR)
	call malloc (TP_SECTPTR(ptr), SZ_FNAME, TY_CHAR)

	# Parse the template into a root name, starting group number,
	# and group count

	call tp_parse (imname, def_count, TP_ROOT(ptr), TP_SECT(ptr),
		       TP_START(ptr), TP_COUNT(ptr))

	TP_INDEX(ptr) = TP_START(ptr)
	count = TP_COUNT(ptr)
	return(ptr)
end             
