# The following strings are not device dependent; if found, and map_cc is
# enabled, they are converted into the standout mode sequences for the
# destination terminal or printer.
# N.B.: The ^ in these strings is a strmatch BOL, not an escape sequence.
# Used in PAGE and LPOPEN.

string	so_on	"^\016"		# SO
string	so_off	"^\017"		# SI
