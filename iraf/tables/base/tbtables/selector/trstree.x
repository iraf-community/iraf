include "trs.h"

#* HISTORY *
#* B.Simon	02-Jan-98	original

# This tree traversal code assumes the tree has links to the left and right
# subtrees, as well as a link to the parent node. The parent node of the root
# node is NULL. Leaf nodes contain pointers to other information. To 
# distinguish these pointers from tree links, the pointers are negative 
# numbers. All procedures are non-recursive as SPP does not support recursion.
# The tree contains information parsed from the row selection expression.
# It is used as an intermediate data structure for optimization before
# the information is converted to pseudocode instructions.

# TRS_COL_TREE -- Retrieve column pointer from range node above it in the tree

pointer procedure trs_col_tree (current)

pointer current		# i: current node in tree
#--
pointer	col, node

begin
	col = NULL
	node = current

	while (node != NULL) {
	    if (TREE_OPER(node) == YRANGE) {
		col = - TREE_RIGHT(node)
		break
	    }

	    node = TREE_UP(node)
	}

	return (col)
end

# TRS_FIRST_TREE -- Get first (deepest leftmost) node in binary tree

pointer procedure trs_first_tree (root)

pointer root		# i: root of binary tree
#--
pointer	node

begin
	node = root

	repeat {
	    while (TREE_LEFT(node) > 0)
		node = TREE_LEFT(node)

	    if (TREE_RIGHT(node) <= 0)
		break

	    node = TREE_RIGHT(node)
	}

	return (node)
end

# TRS_NEXT_TREE -- Get next node in binary tree in postfix order

# After looking at the left subtree, look at the leftmost node of the right 
# subtree. After looking at the right subtree, look at its immediate parent
# The root node has an UP node of NULL, which signals an end to the traverse

pointer procedure trs_next_tree (current)

pointer	current		# i: currently visited node
#--
pointer	node

begin
	node = TREE_UP(current)

	if (node > 0) {
	    # right nodes only need to back up one
	    # left nodes need to get right subtree

	    if (current == TREE_LEFT(node)) {
		if (TREE_RIGHT(node) > 0) {

		    # Get right node of parent
		    node = TREE_RIGHT(node)

		    # Get deepest leftmost subtree
		    repeat {
			while (TREE_LEFT(node) > 0)
			    node = TREE_LEFT(node)

			if (TREE_RIGHT(node) <= 0)
			    break

			node = TREE_RIGHT(node)
		    }
		}
	    }
	}

	return (node)
end

# TRS_OVER_TREE -- Determine if current node is over a row range node

bool procedure trs_over_tree (current)

pointer	current		# i: node to be checked
#--
bool	over
pointer	node

pointer	trs_first_tree(), trs_next_tree()

begin
	over = false
	node = trs_first_tree (current)

	while (node != TREE_UP(current)) {
	    if (TREE_OPER(node) == YRANGE && TREE_RIGHT(node) == NULL) {
		over = true
		break
	    }

	    node = trs_next_tree (node)
	}

	return (over)
end

# TRS_SNIP_TREE -- Remove node and its children from the binary tree

procedure trs_snip_tree (current)

pointer	current		# i: currently visited node
#--
pointer node

string	badlink  "trs_snip_tree: bad links in binary tree"

begin
	node = TREE_UP(current)
	TREE_UP(current) = NULL

	if (TREE_LEFT(node) == current) {
	    TREE_LEFT(node) = NULL
	} else if (TREE_RIGHT(node) == current) {
	    TREE_RIGHT(node) = NULL
	} else {
	    call error (1, badlink)
	}

end

# TRS_UNDER_TREE -- Determine if current node is under a row range node

bool procedure trs_under_tree (current)

pointer current		# i: node to be checked
#--
bool	under
pointer	node

begin
	under = false
	node = TREE_UP(current)

	while (node != NULL) {
	    if (TREE_OPER(node) == YRANGE && TREE_RIGHT(node) == NULL) {
		under = true
		break
	    }

	    node = TREE_UP(node)
	}

	return (under)
end

# TRS_XCG_TREE -- Exchange position of node with its parent in binary tree

procedure trs_xcg_tree (current)

pointer	current		# i: node to be exchanged
#--
pointer	child, parent, grandparent

string	badlink  "trs_xcg_tree: bad links in binary tree"

begin
	child = TREE_LEFT(current)
	parent = TREE_UP(current)
	grandparent = TREE_UP(parent)

	if (TREE_LEFT(grandparent) == parent) {
	    TREE_LEFT(grandparent) = current
	} else if (TREE_RIGHT(grandparent) == parent) {
	    TREE_RIGHT(grandparent) = current
	} else {
	    call error (1, badlink)
	}

	TREE_LEFT(parent) = TREE_LEFT(current)
	TREE_UP(parent) = current

	TREE_LEFT(current) = parent
	TREE_UP(current) = grandparent

	TREE_UP(child) = parent
end
