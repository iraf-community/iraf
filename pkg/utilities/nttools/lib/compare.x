.help compare
.nf___________________________________________________________________________

Comparison routines used to sort table columns. There are two sets of routines,
compasc[bdirt] for sorting in ascending order and compdsc[bdirt] for sorting in
descending order. The last letter indicates the type of data compared in the
sort. All routines return an integer that indicates the results of comparison.
The value of the integer is set according to the following scheme: 

		Ascending				Descending

	if mem[i] < mem[j], order = -1		if mem[i] > mem[j], order = -1
	if mem[i] == mem[j], order = 0		if mem[i] == mem[j], order = 0
	if mem[i] > mem[j], order =  1		if mem[i] < mem[j], order =  1

.endhelp_______________________________________________________________________

# B.Simon	16-Sept-87	First Code

# COMPASCB -- Boolean comparison routine used for sort in ascending order

int procedure compascb (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin
	# false < true

	if (! Memb[dataptr+i-1] && Memb[dataptr+j-1])
	    order = -1
	else if (Memb[dataptr+i-1] && ! Memb[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPASCD -- Double comparison routine used for sort in ascending order

int procedure compascd (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin

	if (Memd[dataptr+i-1] < Memd[dataptr+j-1])
	    order = -1
	else if (Memd[dataptr+i-1] > Memd[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPASCI -- Integer comparison routine used for sort in ascending order

int procedure compasci (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin

	if (Memi[dataptr+i-1] < Memi[dataptr+j-1])
	    order = -1
	else if (Memi[dataptr+i-1] > Memi[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPASCR -- Real comparison routine used for sort in ascending order

int procedure compascr (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin

	if (Memr[dataptr+i-1] < Memr[dataptr+j-1])
	    order = -1
	else if (Memr[dataptr+i-1] > Memr[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPASCT -- Text comparison routine used for sort in ascending order

int procedure compasct (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

bool	strlt(), strgt()

begin

	if (strlt (Memc[dataptr+(i-1)*(lendata+1)],
		   Memc[dataptr+(j-1)*(lendata+1)]) )
	    order = -1
	else if (strgt (Memc[dataptr+(i-1)*(lendata+1)],
	                Memc[dataptr+(j-1)*(lendata+1)]) )
	    order = 1
	else
	    order = 0

	return (order)
end
                     
# COMPDSCB -- Boolean comparison routine used for sort in descending order

int procedure compdscb (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin
	# true > false

	if (Memb[dataptr+i-1] && ! Memb[dataptr+j-1])
	    order = -1
	else if (! Memb[dataptr+i-1] && Memb[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPDSCD -- Double comparison routine used for sort in descending order

int procedure compdscd (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin

	if (Memd[dataptr+i-1] > Memd[dataptr+j-1])
	    order = -1
	else if (Memd[dataptr+i-1] < Memd[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPDSCI -- Integer comparison routine used for sort in descending order

int procedure compdsci (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin

	if (Memi[dataptr+i-1] > Memi[dataptr+j-1])
	    order = -1
	else if (Memi[dataptr+i-1] < Memi[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPDSCR -- Real comparison routine used for sort in descending order

int procedure compdscr (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

begin

	if (Memr[dataptr+i-1] > Memr[dataptr+j-1])
	    order = -1
	else if (Memr[dataptr+i-1] < Memr[dataptr+j-1])
	    order = 1
	else
	    order = 0

	return (order)
end

# COMPDSCT -- Text comparison routine used for sort in descending order

int procedure compdsct (i, j)

int	i	# i: Index to first array element in comparison
int	j	# i: Index to second element in comparison
#--
include	"compare.com"

int	order

bool	strgt(), strlt()

begin

	if (strgt (Memc[dataptr+(i-1)*(lendata+1)],
		   Memc[dataptr+(j-1)*(lendata+1)]) )
	    order = -1
	else if (strlt (Memc[dataptr+(i-1)*(lendata+1)],
	                Memc[dataptr+(j-1)*(lendata+1)]) )
	    order = 1
	else
	    order = 0

	return (order)
end
