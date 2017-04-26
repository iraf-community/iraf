#{ MKMANPAGE -- Make a template for a manual page, ready to be filled in.
# Leaves us in the editor.
#
#	 mkmanpage (module)

{
	fname = module // ".hlp"

	if (clformat)
	    copy (cltemplate, fname)
	else
	    copy (xtemplate,  fname)

	edit (fname)
}
