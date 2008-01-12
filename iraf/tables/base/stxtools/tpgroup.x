include	<ctype.h>
include	<imio.h>

# TP_GROUP -- Extract the group index and count from an image name 
#
# B.Simon	02-Jun-89	Original
# B.Simon	10-Jul-98	Revised to ignore what it can't parse
# B.Simon	02-Oct-98	added call to tp_count
# B.Simon	26-Apr-99	set index to ERR if undefined

procedure tp_group (root, gsect, def_count, index, count)

char	root[ARB]	# i: Root section of image name
char	gsect[ARB]	# i: Group section of image name
int	def_count	# i: Default count if not specified
int	index		# o: Starting group index
int	count		# o: Group count
#--
bool	star
int	ic, inum, num[2]

int	tp_count()

begin
	inum = 0
	num[1] = 0
	num[2] = 0
	star = false

	# Extract the numeric fields from the group section
	# Set a flag if a star was found

	for (ic = 1; gsect[ic] != EOS; ic = ic + 1) {
	    switch (gsect[ic]) {
	    case ' ':
		;
	    case '[':
		inum = 1
	    case ']':
		break
	    case '*':
		star = true
		inum = inum - 1
	    case '/':
		inum = inum + 1
	    default:
		if (! star && IS_DIGIT(gsect[ic])) {
		    if (inum > 2) {
			inum = 2
			break
		    }
		    num[inum] = 10 * num[inum] + TO_INTEG(gsect[ic])

		} else {
		    inum = 0
		    star = false
		    break
		}
	    }
	}

	# Set the output variables according to the number of fields found

	switch (inum) {
	case 0:
	    index = ERR
	    count = ERR
	case 1:
	    index = num[1]
	    count = 1
	case 2:
	    index = num[1]
	    count = max (1, num[2])
	}

	# Either use the default count or if the default is zero,
	# Open the image and read the count from it

	if (star) {
	    if (def_count > 0) {
		count = def_count

	    } else {
		count = tp_count (root)
	    }
	}
end
