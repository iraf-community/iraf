procedure apropos ( topic )

string	topic {prompt = ">Apropos ? ", mode="ql"}
file	index = "aproposdb" {prompt=">index to search"}

begin

string csubject, dblist, fname, pvar
int len, n, iend
bool var_ok

	# Remove case sensitivity
	csubject = "{"//topic//"}"

	# As a special case, if we're given the name of the "aproposdb"
	# environment variable then get the list of database filenames
	# from that:
	if (index == "aproposdb") {
		if (defvar("aproposdb")) {
			dblist = envget("aproposdb")
		} else {
			error(1, "variable aproposdb is not set!")
		}
	} else {
		dblist = index
	}
	
	len = strlen(dblist)

	# Loop over the database filenames in the list:
	while (len > 0) {

		# Parse next name and remove it from the start of the list:
		iend = stridx(",", dblist) - 1
		if (iend < 0) iend = len     # no more commas; use what's left
		fname = substr(dblist, 1, iend) # parse next filename
		if (iend > len-2) {          # no more filenames; empty list
			dblist=""
		} else {                     # still have another file
			dblist = substr(dblist, iend+2, len)
		}
		len = strlen(dblist)

		# Extract any path variable and check that it exists,
		# otherwise IRAF will complain:
		var_ok = yes
		iend = stridx("$", fname) - 1
		if (iend > -1) {
			pvar = substr(fname, 1, iend)
			if (!defvar(pvar))
				var_ok = no
		}

		# Try to match the query if the database file exists:
		if (var_ok)
			if (access(fname)) {

				# Print any matches in this database file:
				match ( csubject, fname, stop=no, meta=yes, \
					print_file_names=no)

			}

	} # while loop over database files

end
