include  defs

# entfkw - place Fortran keywords in symbol table.
# Place in the following table any long (> 6 characters)
# keyword that is used by your Fortran compiler:


subroutine entfkw

include COMMON_BLOCKS
string sequiv "equivalence"

	call enter (sequiv, 0, fkwtbl)
end
