include defs

# INDENT -- Indent the output listing.

subroutine indent (nlevels)

integer	nlevels
include COMMON_BLOCKS

	logical_column = logical_column + (nlevels * INDENT)
	col = max(6, min(MAX_INDENT, logical_column))
end
