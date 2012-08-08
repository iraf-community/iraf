.help prconv
Parser symbol conversion

These procedures convert SYMTAB pointers into symbol offsets and viceversa,
and string offsets into character pointers (when applicable).
These procedures are called by the prget and prput procedures in order to
perform the appropiate type conversions.

.nf
Entry points:

int	= pr_offset (sym)	Convert SYMTAB pointer into symbol offset
pointer	= pr_pointer (offset)	Convert symbol offset into SYMTAB pointer
pointer	= pr_charp (offset)	Convert string offset into character pointer
.fi
.endhelp

include	"../lib/parser.h"


# PR_OFFSET - Convert SYMTAB pointer into an offset

int procedure pr_offset (sym)

pointer	sym		# symbol pointer

pointer	strefstab()
pointer	pr_getp()

begin
	# Check pointer
	if (sym == NULL)
	    return (INDEFI)
	else
	    return (sym - strefstab (pr_getp (SYMTABLE), 0))
end


# PR_POINTER - Convert an offset into a SYMTAB pointer

pointer procedure pr_pointer (offset)

int	offset			# symbol offset

pointer	strefstab()
pointer	pr_getp()

begin
	# Check offset
	if (IS_INDEFI (offset))
	    return (NULL)
	else
	    return (strefstab (pr_getp (SYMTABLE), offset))
end


# PR_CHARP - Convert string offset into character pointer

pointer procedure pr_charp (offset)

int	offset			# string offset

pointer	strefsbuf()
pointer	pr_getp()

begin
	# Check offset
	if (IS_INDEFI (offset))
	    return (NULL)
	else
	    return (strefsbuf (pr_getp (SYMTABLE), offset))
end
