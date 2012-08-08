# REFERENCES -- Search the help database for help on a given subject.
# By default a runtime search is performed of all the package menus in the
# help database, but the user may prepare a "quick-reference" file to speed
# further searches if desired.  This cannot easily be done automatically
# since any set of packages may form the help database.
#
# NOTE -- Uses LISTS.

procedure references (topic)

string	topic			     { prompt = "topic to find help for" }
file	quickref = "uparm$quick.ref" { prompt = "quick-reference file" }
bool	updquick = no		     { prompt = "update quick-reference file" }
bool	usequick = no		     { prompt = "use quick-reference file" }

begin
	string	pattern
	file	fname

	# Make a quick-search file if so requested.
	if (updquick) {
	    fname = quickref
	    print ("generating new quick-reference file " // fname // "...")
	    if (access (fname))
		delete (fname, verify-)
	    help ("[a-z]*.", option="ref", curpack="AsckCL", device="terminal",
		helpdb="helpdb") |& match ("-", metacharacters=yes) |
		sort(ignore+) | unique ( > fname)
	    references.quickref = fname
	    references.usequick = yes

	} else {
	    # Ignore case.
	    pattern = ("{" // topic // "}")

	    # If the user has prepared a quick-search file (e.g., by running
	    # this task with topic=* and saving the output to make the quick
	    # file), search that if it exists, otherwise perform a runtime
	    # search of the package menu files for the entire help database.

	    if (usequick && access (quickref))
		match (pattern, quickref, metacharacters=yes)
	    else {
		print ("searching the help database...")
		help ("[a-z]*.", section="all", option="ref", curpack="AsckCL",
		    device="terminal", helpdb="helpdb") |& sort(ignore+) | 
		    unique | match (pattern, metacharacters=yes)
	    }
	}
end
