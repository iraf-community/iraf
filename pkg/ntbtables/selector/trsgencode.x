include <tbset.h>
include	"trs.h"

#* HISTORY *
#* B.Simon	02-Jan-98	original

# TRSGENCODE -- Generate pseudocode from binary tree

procedure trsgencode (tp, root, pcode)

pointer	tp		# i: table descriptor
int	root		# i: root node of binary tree
pointer	pcode		# u: pseudocode structure
#--
int	nrow

bool	trshasrow()
int	tbpsta()
pointer	trsoptimize(), rst_create()
errchk	trshasrow, trsputcode, trsoptimze

begin
	nrow = tbpsta (tp, TBL_NROWS)

	if (trshasrow (root)) {
	    TRS_ROWS(pcode) = trsoptimize (root, nrow)

	} else {
	    TRS_ROWS(pcode) = rst_create (1, nrow)
	}

	call trsputcode (root, pcode)
	call trsputjump (root, pcode)

end

# TRSHASROW -- Does code contains a row expression that can be optimized?

bool procedure trshasrow (root)

pointer	root		# i: root of binary tree
#--
bool	result, hasrow
pointer	node, child

bool	trs_over_tree()
pointer	trs_first_tree(), trs_next_tree()
errchk	trs_xcg_tree

begin
	# Expressions without row ranges cannot be optimized. Also
	# expressions with YNOT outside of YRANGE cannot be optimized.
	# However, if the YNOT operates on a single range, the order
	# of the YRANGE and YNOT can be flipped

	result = true
	hasrow = false
	node = trs_first_tree (root)

	while (node != NULL) {
	    if (TREE_OPER(node) == YRANGE && TREE_RIGHT(node) == NULL) {
		hasrow = true

	    } else if (TREE_OPER(node) == YNOT) {

		# If a YNOT is found outside a YRANGE controlling a row, 
		# it is not optimizable unless the two can be swapped

		child = TREE_LEFT(node)

		if (TREE_OPER(child) == YRANGE) {
		    # YNOT and YRANGE can be swapped, so do it
		    call trs_xcg_tree (child)

		} else if (trs_over_tree (node)) {
		    # Can't be swapped and over row range, 
		    # so not optimizable
		    result = false
		}
	    }

	    node = trs_next_tree (node)
	}

	# No row range, so not optimizable

	if (! hasrow)
	    result = false

	return (result)
end

# TRSOPTIMIZE -- Optimize an expression by evaluting its row ranges

pointer procedure trsoptimize (root, nrow)

pointer	root		# i: root of binary tree
int	nrow		# i: number of rows in table
#--
int	top, istack, nstack
pointer	sp, eval, node, prev, set

bool	trs_under_tree()
pointer	trs_first_tree(), trs_next_tree()
errchk	trsroweval, trs_snip_tree

begin
	# Allocate arrays used in traversing binary tree

	call smark (sp)
	call salloc (eval, MAXDEPTH, TY_INT)

	# Traverse the binary tree, looking for row expressions
	# when one is found, evaluate it and remove it from the tree

	top = 0
	node = trs_first_tree (root)

	while(node != NULL) {
	    # Evaluate row expressions

	    if (trs_under_tree (node))
		call trsroweval (TREE_OPER(node), -TREE_LEFT(node), 
				 -TREE_RIGHT(node), nrow, Memi[eval], 
				 top)

	    prev = node
	    node = trs_next_tree (node)

	    # After complete evaluation of the row expression
	    # snip it out of the binary tree. If both branches
	    # of a logical have been snipped, also snip it out
	    # of the tree. Don't have to worry about YNOT as it
	    # was already buried beneath YRANGE in trshasrow

	    if (TREE_OPER(prev) == YRANGE && TREE_RIGHT(prev) == NULL) {
		call trs_snip_tree (prev)

	    } else if ((TREE_OPER(prev) == YAND || TREE_OPER(prev) == YOR) &&
		       (TREE_RIGHT(prev) == NULL && TREE_LEFT(prev) == NULL)) {
		call trs_snip_tree (prev)
	    }

	}

	# If there is more than one row expression, they are 
	# combined with ands

	nstack = top - 1
	do istack = 1, nstack
	    call trsroweval (YAND, NULL, NULL, nrow, Memi[eval], top)

	# Return the row set evaluated 

	set = Memi[eval]

	call sfree (sp)
	return (set)

end

# TRSPUTCODE -- Convert binary tree into pseudocode instructions

procedure trsputcode (root, pcode)

pointer	root		# i: root of binary tree
pointer	pcode		# u: pseudocode structure
#--
int	icode, oper
pointer	codebuf, node, col, loval, hival

string	noroom  "Table row selection expression too complex"

pointer	trs_first_tree(), trs_next_tree(), trs_col_tree()

begin
	icode = 0
	codebuf = TRS_CODE(pcode)

	node = trs_first_tree (root)

	while (node != NULL) {
	    oper = TREE_OPER(node)

	    if ((oper == YAND || oper == YOR) && 
		(TREE_LEFT(node) == NULL || 
		 TREE_RIGHT(node) == NULL)) {

		# Skip encoding if one branch of a logical 
		# has been snipped

		TREE_INST(node) = ERR

	    } else {
		# Check for buffer overflow

		if (icode + SZ_INSTR >= SZ_BUFFER)
		    call error (1, noroom)

		# Set instruction field in tree

		TREE_INST(node) = icode

		# Retrieve column value

		if (YLOGICAL(oper))
		    col = NULL
		else 
		    col = trs_col_tree (node)

		# Retrieve field values

		call trsvalue (node, loval, hival)

		# Add instruction to code buffer

		Memi[codebuf+icode+OCODE] = oper
		Memi[codebuf+icode+OCOLUMN] = col
		Memi[codebuf+icode+OTJUMP] = NULL
		Memi[codebuf+icode+OFJUMP] = NULL
		Memi[codebuf+icode+OLOVAL] = loval
		Memi[codebuf+icode+OHIVAL] = hival

		# Increment code buffer index

		icode = icode + SZ_INSTR
	    }

	    node = trs_next_tree (node)
	}

end

# TRSPUTJUMP -- Add jumps to pseudocode

procedure trsputjump (root, pcode)

pointer	root		# i: root of binary tree
pointer	pcode		# u: pseudocode structure
#--
int	icode, inst
pointer	codebuf, node, jump, child

pointer	trs_first_tree(), trs_next_tree()

begin
	codebuf = TRS_CODE(pcode)
	node = trs_first_tree (root)

	while (node != NULL) {
	    if (TREE_INST(node) != ERR) {
		inst = TREE_OPER(node)
		jump = TREE_INST(node)

		child = TREE_LEFT(node)
		if (child > 0) {
		    icode = TREE_INST(child)

		    if (inst == YOR) 
			Memi[codebuf+icode+OTJUMP] = jump

		    if (inst == YAND)
			Memi[codebuf+icode+OFJUMP] = jump
		}
	    }

	    node = trs_next_tree (node)
	}

end

# TRSROWEVAL -- Evaluate an operation in a row expression

procedure trsroweval (code, loval, hival, nrow, eval, top)

int	code		# i: pseudocode instruction
pointer	loval		# i: low end of range
pointer	hival		# i: high end of range
int	nrow		# i: number of rows in table
pointer	eval[MAXDEPTH]	# u: stack of pending results
int	top		# u: index to top of stack
#--
int	narg, iarg, lo, hi

string	ovflow  "trs_roweval: stack overflow"
string	badcode "trs_roweval: bad instruction"

pointer	rst_create(), rst_and(), rst_or(), rst_not()

begin
	if (top == MAXDEPTH)
	    call error (1, ovflow)

	switch (code) {
	case YRANGE: 	# range operation, really a no-op
	    narg = 0

	case YAND:	# logical and
	    narg = 2
	    top = top + 1
	    eval[top] = rst_and (eval[top-1], eval[top-2])

	case YOR:	# logical or
	    narg = 2
	    top = top + 1
	    eval[top] = rst_or (eval[top-1], eval[top-2])

	case YNOT:	# logical not
	    narg = 1
	    top = top + 1
	    eval[top] = rst_not (nrow, eval[top-1])

	case YEQN:	# numerical equality test
	    narg = 0
	    top = top + 1

	    lo = max (1, int(Memd[loval]))
	    eval[top] = rst_create (lo, lo)

	case YLEN:	# numeric less than or equal check
	    narg = 0
	    top = top + 1

	    lo = max (1, int(Memd[loval]))
	    eval[top] = rst_create (1, lo)

	case YINN: 	# numeric inclusion check
	    narg = 0
	    top = top + 1

	    lo = min (Memd[loval], Memd[hival])
	    hi = max (Memd[loval], Memd[hival])

	    lo = max (1, lo)
	    hi = min (nrow, hi)
	    eval[top] = rst_create (lo, hi)


	case YGEN:	# numeric greater than or equal check
	    narg = 0
	    top = top + 1

	    hi = min (nrow, int(Memd[loval]))
	    eval[top] = rst_create (hi, nrow)

	default:
	    call error (1, badcode)
	}

	# Free used stack elements

	if (narg > 0) {
	    do iarg = 1, narg
		call rst_free (eval[top-iarg])

	    eval[top-narg] = eval[top]
	    top = top - narg
	}
end

# TRSVALUE -- Extract field values from a node of a binary tree

procedure trsvalue (node, loval, hival)

pointer	node		# i: binary tree node
pointer	loval		# o: smaller of the two values
pointer	hival		# o: larger of the two values
#--
bool	strgt()

begin

	if (TREE_RIGHT(node) == NULL) {
	    # Duplicate left value if right value is NULL

	    loval = -TREE_LEFT(node)
	    hival = -TREE_LEFT(node)

	} else {
	    # Flip high and low values if out of order

	    if (TREE_OPER(node) == YINN) {
		if (Memd[-TREE_RIGHT(node)] > 
		    Memd[-TREE_LEFT(node)]) {

		    loval = -TREE_LEFT(node)
		    hival = -TREE_RIGHT(node)
		} else {
		    loval = -TREE_RIGHT(node)
		    hival = -TREE_LEFT(node)
		}

	    } else if (TREE_OPER(node) == YINS) {
		if (strgt (Memc[-TREE_RIGHT(node)], 
			   Memc[-TREE_LEFT(node)])) {

		    loval = -TREE_LEFT(node)
		    hival = -TREE_RIGHT(node)
		} else {
		    loval = -TREE_RIGHT(node)
		    hival = -TREE_LEFT(node)
		}
	    }
	}

	# Set values to null if the value is actually a node address

	if (loval < 0)
	    loval = NULL

	if (hival < 0)
	    hival = NULL

end
