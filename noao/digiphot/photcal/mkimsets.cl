# MKIMSETS -- Create an image set file which describes which images of a given
# region of the sky form a complete observation.
#
# This script requires the IMAGES task HSELECT, the PTOOLS task DUMP, and
# the PROTO task FIELDS. THE IRAF core system tasks UNIQUE (LISTS package),
# FILES, DELETE, RENAME, CONCATENATE, and MATCH (SYSTEM package) and FSCAN,
# ACCESS, MKTEMP and EDIT (LANGUAGE package) are also used.

procedure mkimsets (imlist, idfilters, imsets)

file	imlist	   {prompt="The input image list"}
string	idfilters  {prompt="The list of filter ids"}
file	imsets     {prompt="The output image set file"}
file	imobsparams  {"", prompt="The output image observing parameters file"}

string	input	   {"photfiles", enum="images|photfiles|user",
			prompt="The source of the input image list"}
string	filter	   {prompt="The filter keyword"}
string	fields	   {"", prompt="Additional image list fields"}

string	sort	   {"", prompt="The image list field to be sorted on"}
bool	edit	   {yes, prompt="Edit the input image list before grouping"}
bool	rename	   {yes, prompt="Prompt the user for image set names"}
bool	review	   {yes, prompt="Review the image set file with the editor"}

struct  *list

begin
	# Declare local variables.
	int tcolumn, nfields
	string tlist, tidfilters, timsets, tobsparams, tfields, tfilter, tsort
	string tfile1, tfile2, tstr

	# Get the parameters.
	tlist = imlist
	tidfilters = idfilters
	timsets = imsets
	if (access (timsets)) {
	    error (1,
	    "The image observation set file " // timsets // " already exists")
	}
	tobsparams = imobsparams
	if (access (tobsparams)) {
	    error (1,
	    "The observing parameters file " // tobsparams // " already exists")
	}

	# Create temporary file names to store the intermediate sets of images.

	tfile1 = mktemp ("tmp$")

	# Construct the initial list of images and write them to a file.
	# Sort on the image name and remove duplicate lines. 

	if (input == "photfiles") {

	    tfilter = "IFILTER"
	    tfields = "ITIME,XAIRMASS,OTIME" // "," // fields 
	    tbkeycol (tlist, "IMAGE," // tfilter // "," // tfields)
	    pdump (tlist, "IMAGE," // tfilter // "," // tfields, 
		"yes", headers=no, parameters=yes)                           |
	    system.sort ("STDIN", column=0, ignore_white=no,
		    numeric_sort=no, reverse_sort=no)                 |
	    unique ("STDIN", > tfile1)

	} else if (input == "images") {

	    tfilter = filter
	    tfields = fields
	    hselect (tlist, "$I," // tfilter // "," // tfields, yes)  |
	    system.sort ("STDIN", column=0, ignore_white=no,
	        numeric_sort=no, reverse_sort=no)                     |
	    unique ("STDIN", > tfile1)

	} else if (input == "user") {

	    tfilter = filter
	    tfields = fields
	    fields (tlist, "1," // tfilter // "," // tfields,
	        lines="1-9999", quit_if_missing=no,
		print_file_name=no)                                   |
	    unique ("STDIN", > tfile1)

	}

	# Create temporary file names to store the intermediate sets of images.

	tfile2 = mktemp ("tmp$")

	# Determine the column to be sorted on and store it in the parameter
	# tcolumn. The files routine breaks up the fields parameter into
	# its component parts and removes any imbedded commas.

	tsort = sort
	if (tsort == "") {
	    tcolumn = 0
	} else if (tsort == "image") {
	    tcolumn = 1
	} else if (tsort == tfilter) {
	    tcolumn = 2
	} else {
	    files (tfields, sort=no, > tfile2)
	    tcolumn = 0
	    nfields = 0
	    list = tfile2
	    while (fscan (list, tstr) != EOF) {
		nfields = nfields + 1
		if (tsort != tstr)
		    next
		tcolumn = nfields + 2
		break
	    }
	    list = ""
	    delete (tfile2, verify-, default_action+, allversions+,
		subfiles+, >& "dev$null")
	}

	# Create a temporary file name to store the sorted list.

	tfile2 = mktemp ("tmp$")

	# Sort the image list on the user defined column.

	if (tcolumn > 0) {
	    system.sort (tfile1, column=tcolumn, ignore_white-,
	        numeric_sort+, reverse_sort-, > tfile2)
	    delete (tfile1, verify-, default_action+, allversions+, subfiles+,
	        >& "dev$null")
	    rename (tfile2, tfile1)
	}

	# Insert an explanatory  message at the beginning of the temporary
	# image list file.

	concatenate ("photcal$mkimsets/imlist.key" // "," // tfile1,
	    tfile2, out_type="in_type", append=no)
	delete (tfile1, verify-, default_action+, allversions+, subfiles+,
	    >& "dev$null")

	# Allow the user to edit the temporary image list file.

	if (edit)
	    edit (tfile2)

	# Group the images in the image list file and optionally supply a
	# name for each image set.

	tfile1 = mktemp ("tmp$")
	imgroup (tfile2, timsets, tidfilters, rename=rename,
	    >& tfile1, > "STDOUT")

	# Remove the leading message and write the output obsparams file.

	if (tobsparams != "")
	    match ("^ \#", tfile2, metacharacters+, stop+,
	        print_file_names+, > tobsparams)

	# Delete the temporary file.

	delete (tfile2, verify-, default_action+, allversions+, subfiles+,
	    >& "dev$null")

	# Allow the user to edit the new image sets file. If review is yes
	# prepend the error message file to the image set file and enter
	# the editor. Other wise simply type the error messages to the
	# standard output.

	if (review) {
	    concatenate ("photcal$mkimsets/imsets.key," // tfile1 //
	        "," // timsets, tfile2, out_type="in_type", append=no)
	    edit (tfile2)
	    delete (timsets, verify-, default_action+, allversions+, subfiles+,
	        >& "dev$null")
	    match ("^<", tfile2, metacharacters+, stop+, print_file_names+) |
	    match ("^ \#", "STDIN", metacharacters+, stop+, print_file_names+,
	        > timsets)
	    delete (tfile2, verify-, default_action+, allversions+, subfiles+,
	        >& "dev$null")
	} else
	    type (tfile1)

	# Delete the temporary file.

	delete (tfile1, verify-, default_action+, allversions+, subfiles+,
	    >& "dev$null")
end
