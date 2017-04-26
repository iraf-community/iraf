# GRIPES -- Send gripes to the system.  Gripes may be gripes, complaints, or
# suggestions.

procedure gripes (subject)

string	subject { prompt = "Subject" }
file	gripesfile = "hlib$gripesfile"
struct	gripetext { len = 80 }
file	tempfile
struct	*list
struct	timestring { len = 25 }
bool	quit
bool	verbose = yes

begin
	# Put gripe in tempfile and only append to the system gripefile if
	# we complete normally.  Thus if the user aborts us the gripe is
	# not recorded.

	tempfile = mktemp ("uparm$gripe") // ".txt"
	time (> tempfile)
	list = tempfile
	if (fscan (list, timestring) != EOF)
	    delete (tempfile, verify=no)

	# Print gripe report header.
	print ("\n------------", >> tempfile)
	print ("From: ", envget ("userid"), " ", timestring, >> tempfile)

	# Learn mode is not very useful for the subject string, since new
	# gripemail virtually always deals with a different subject.  Reset
	# the subject string to null so that no prompt will be issued the
	# next time we are called.

	print ("Subject: ", subject, >> tempfile)
	subject = ""

	if (verbose) {
	    print ("Enter your gripe(s).\n")
	    print ("Type <eof> or '.' to quit, '~edit' to go into the editor:")
	}
	print (" ", >> tempfile)		# skip line on output
	print (" ")				# skip line on terminal

	# Copy user text.  Call up editor on temp file if "~edit" escape
	# is entered, or any abbreviation thereof, and append file to
	# gripesfile after exiting from the editor.

	while (scan (gripetext) != EOF)
	    if (substr (gripetext,1,1) == '.') {
		break
	    } else if (substr (gripetext,1,2) == "~e") {
		edit (tempfile)
		clear; type (tempfile)
	    } else
		print (gripetext, >> tempfile)

	# type (tempfile, >> gripesfile)
# UNIX
	print ("!!mail iraf@noao.edu < ", osfn(tempfile)) | cl
# VMS
	# print ("!mail ", osfn(tempfile), " 5355::iraf") | cl

	delete (tempfile, verify=no)
end
