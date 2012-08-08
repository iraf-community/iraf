.TH RTAR 1 "14 November 1984"
.SH NAME
rtar \- read tape archive format file
.SH SYNOPSIS
.B rtar
[ flags ] [ archive ] [ after ] [ files ]
.SH DESCRIPTION
.PP
.I Rtar
reads multiple files from a UNIX \fItar\fR format file, restoring the files
to disk on the local host machine.  Output filenames are mapped according to
the IRAF filenaming conventions of the local host operating system.
.IR Rtar 's
actions are controlled by the
.I flags
argument. 
.I Flags
consists of an \fB-\fR followed by
a string of characters containing any combination of the function flags
described below.
Other arguments to 
.I rtar
are the name of the archive file to be read,
the name of the file on the archive at which reading is to begin,
and the names of the files or directories to be read or to be excluded
from the read.
In all cases, appearance of a directory name refers to
the files and (recursively) subdirectories of that directory.
All
.I rtar
filename arguments are UNIX pathnames except
.I archive,
which is a host system filename.
.PP
The default action of \fIrtar\fR is to unpack all files from the \fItar\fR
format standard input.  The following flag characters may be used to further
control the function of \fIrtar\fR:
.TP 8
.B x
The named files are extracted from the tape.  If the named file
matches a directory whose contents had been written onto the tape, this
directory is (recursively) extracted.  The owner, modification time, and mode
are restored (if possible).  If no file argument is given, the entire content
of the tape is extracted.  Note that if multiple entries specifying the same
file are on the tape, the last one overwrites all earlier.
.TP 8
.B r
The extracted file replaces any existing file of the same name, i.e.,
.I rtar
performs a delete before creating the extracted file.
.TP 8
.B e
Extract the entire contents of the tape \fIexcluding\fR the files or directories
listed in \fIfiles\fR.
.TP 8
.B a
Advance to the archive file named by the \fIafter\fR argument before
performing the main operation.  The extract or list operation will begin with
the file \fIafter\fR and continue to the end of the archive.
.TP 8
.B t
The names of the specified files are listed each time they occur on
the tape.  If no \fIfiles\fR argument is given, all of the names on the tape
are listed.
.TP 8
.B v
Print more information about the tape entries than just their names.
The verbose file list format gives the file permissions, the link flag
(zero if there were no links to the file), the owner and group identification
numbers of the file on the system that wrote the archive, the file size in
bytes, the date of last modification of the file, and the file name.
.TP 8
.B d
Print detailed information about what \fIrtar\fR is doing.
.TP 8
.B f
.I Rtar
uses the first filename argument as the host filename of the archive
instead of reading from \fIstdin\fR.
.TP 8
.B l
Do not try to resolve file links by a disk to disk file copy.  By default,
if file A appears in the archive as a link to file B,
\fIrtar\fR trys to resolve the link by performing a disk to disk copy of
file B to A.  This is valid providing file B was present in the archive and
has already been extracted.  If the \fBl\fR flag is present linked files
will not be extracted.
.TP 8
.B o
Omit binary files when performing the extraction.  A binary file is any
file containing ASCII values other than 040 through 0176 (the printable
ASCII characters), tab, or newline in the first 512 byte block of the file.
.TP 8
.B b
Output only binary byte stream files.  By default, \fIrtar\fR outputs text
files in the host system textfile format.  The conversion from the byte stream
\fItar\fR format to host textfile format may involve modification of the
file, e.g., conversion from ASCII to EBCDIC.  A binary extraction copies
the file to disk without modification.
.PP
If the input archive file is a tape the blocksize must be a multiple
of 512 bytes, with a maximum blocksize of 10240 bytes.  Each archived file
occupies an integral number of 512 byte blocks in the archive.
.PP
Filenames appearing in the file list are interpreted as prefix strings,
i.e., a match occurs if the given string is a prefix of an actual filename
in the archive.  If the last character in the \fIfiles\fR filename is
a \fB$\fR then an exact match is required (excluding the $ metacharacter).
.SH DIAGNOSTICS
.br
A file read error occurring while reading the archive file is fatal unless
caught and corrected by the host system.
.br
File header checksum errors result in skipping of the archive file
currently being read, with execution continuing with the next archive
file if possible.
.br
File write errors on the output file are reported but do not cause
termination of \fIrtar\fR.  The output file being written will be corrupted.
.SH BUGS
.br
The current limit on file name length is 100 characters (this restriction
is imposed by the standard UNIX \fItar\fR format).
.br
File links are not recreated.
