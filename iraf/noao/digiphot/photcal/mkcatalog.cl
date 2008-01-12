# MKCATALOG -- Create / edit a catalog of photometric standards.

procedure mkcatalog (catalog)

file	catalog		{prompt="The name of the output catalog"}
bool	review		{no, prompt="Verify each existing catalog record"}
bool	verify		{no, prompt="Verify each new catalog record"}
bool	edit		{yes,prompt="Check the output catalog with the editor"}


begin
	# Declare local variables
	file	cat

	# Get the positional parameters
	cat = catalog

	# Make a new catalog or edit an existing catalog.
	catalog (cat, review=review, verify=verify)

	# Edit the catalog with the editor.
	print ("")
	if (access (cat) && edit)
	    edit (cat)
end
