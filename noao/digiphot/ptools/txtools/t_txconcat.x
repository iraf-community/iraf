include	<fset.h>
include <error.h>
include "../../lib/ptkeysdef.h"

# T_TXCONCAT -- Procedure to concatenate standard APPHOT and DAOPHOT text
# output files into a single file. The task checks to see that the list 
# of input files was produced by the same task.

procedure t_txconcat()

int	list		# input file list descriptor
int	tp_in		# input file descriptor
int	tp_out		# output file descriptor

int	len_list, first_file, stat
pointer	sp, infile, outfile, task, task1, task2
int	fstati(), clpopnu(), clplen(), open(), clgfil(), strncmp()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (task, SZ_FNAME, TY_CHAR)
	call salloc (task1, SZ_FNAME, TY_CHAR)
	call salloc (task2, SZ_FNAME, TY_CHAR)

	# Get the task parameters.
	list = clpopnu ("textfiles")
	len_list = clplen (list)
	if (len_list <= 0)
	    call error (0, "Empty input file list.\n")
	else if (len_list == 1)
	    call error (0, "Input file list has only one file.\n")
	call clgstr ("outfile", Memc[outfile], SZ_FNAME)
	call clgstr ("task", Memc[task], SZ_FNAME)

	# Loop over the input files to check the task keyword.
	first_file = YES
	stat = OK
	while (clgfil (list, Memc[infile], SZ_FNAME) != EOF) {
	    tp_in = open (Memc[infile], READ_ONLY, TEXT_FILE)
	    if (first_file == YES) {
		call pt_gtaskname (tp_in, Memc[task], Memc[task1], SZ_FNAME)
		if (Memc[task1] == EOS) {
		    call eprintf (
		    "File: %s is not an APPHOT/DAOPHOT text database file")
			call pargstr (Memc[infile])
		    stat = ERR
		}
	        first_file = NO
	    } else {
		call pt_gtaskname (tp_in, Memc[task], Memc[task2], SZ_FNAME)
		if (Memc[task2] == EOS) {
		    call eprintf (
		    "File: %s is not an APPHOT/DAOPHOT text database file\n")
			call pargstr (Memc[infile])
		    stat = ERR
		}
		if (strncmp (Memc[task1], Memc[task2], SZ_FNAME) != 0) {
		    call eprintf (
		    "TASK keyword is not the same for all input files\n")
		    stat = ERR
		}
	    }
	    call close (tp_in)
	    if (stat == ERR)
		break
	}
	call clprew (list)

	# Loop over the input text files and copy each file to the output
	# file.
	if (stat == OK) {
	    tp_out = open (Memc[outfile], NEW_FILE, TEXT_FILE)
	    while (clgfil (list, Memc[infile], SZ_FNAME) != EOF) {
		tp_in = open (Memc[infile], READ_ONLY, TEXT_FILE)
	        call fcopyo (tp_in, tp_out)
		call close (tp_in)
	    }
	    call close (tp_out)
	}

	call clpcls (list)
	call sfree (sp)
end	


# PT_GTASKNAME -- Fetch a task name from  an APPHOT/DAOPHOT text file.

procedure pt_gtaskname (tp_in, name, outname, maxch)

int	tp_in			# input file descriptor
char	name[ARB]		# task keyword
char	outname[ARB]		# output task name
int	maxch			# maximum number of characters

int	findex, lindex
pointer	sp, line
int	getline(), strncmp(), gstrmatch(), ctowrd()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	outname[1] = EOS
	while (getline (tp_in, Memc[line]) != EOF) {
	    if (Memc[line] !=  KY_CHAR_POUND)
		break
	    if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) != 0)
		next
	    if (gstrmatch (Memc[line], name, findex, lindex) == 0)
		next
	    lindex = lindex + 1
	    if (ctowrd (Memc[line], lindex, outname, maxch) <= 0)
		break
	    lindex = lindex + 1
	    if (ctowrd (Memc[line], lindex, outname, maxch) <= 0)
		outname[1] = EOS
	    break
	}

	call sfree (sp)
end
