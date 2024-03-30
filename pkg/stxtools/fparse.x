#---------------------------------------------------------------------------
.help fparse Aug98 xtools
.ih
NAME
fparse -- Parse a file name
.ih
USAGE
call fparse (input, dir, dir_size, root, root_size, ext, ext_size,
             cl_index, cl_size, section, section_size, ksection,
             ksection_size)
.ih
ARGUMENTS
.ls input (I: char[ARB])
The input file name to parse into its components.
.le
.ls dir (O: char[dir_size])
The directory component of the file name.  Includes the directory
separator character.
.le
.ls dir_size (I: int)
The maximum length of the string to place in the dir argument.
.le
.ls root (O: char[root_size])
The root component of the file name.  Includes any wildcard characters
specified for the root.
.le
.ls root_size (I: int)
The maximum length of the string to place in the root argument.
.le
.ls ext (O: char[ext_size])
The extension component of the file name.  DOES NOT INCLUDE the
extension separator, i.e. the ".".
.le
.ls ext_size (I: int)
The maximum length of the string to place in the ext argument.
.le
.ls cl_index (O: int)
The cluster or group index found in the file name.  If none is found,
this returns -1.  Before IRAF v2.11, returned 0.
.le
.ls cl_size (O: int)
The number of clusters or groups found in the file name.  If none is
found, this returns -1.  Before IRAF v2.11, returned 0.
.le
.ls section (O: char[section_size])
The image section specification part of the file name.  Will contain
the standard image section specifications.
.le
.ls section_size (I: int)
The maximum length of the string to place in the section argument.
.le
.ls ksection (O: char[ksection_size])
This is the "catchall".  If the filename cannot be parsed into the
form "dir$root.ext[cl_index/cl_size][section]", ksection will contain
the extra.
.le
.ih
DESCRIPTION
This routine basically performs the individual functions of fnldir,
fnext, and fnroot, and the "illegal" imparse.  The only distinct
advantage to this routine is that wildcard characters, and the
relative directory characters ".", and ".." can be present in the file
name.
.ih
BUGS
This routine calls the illegal routine imparse.  If the image naming
conventions in IMIO ever change, this routine will surely break all to
pieces.
.endhelp
#---------------------------------------------------------------------------
#
# M.D. De La Pena - 11 August 1998: updated internal documentation for
#   cl_index and cl_size to reflect changes made for IRAF v2.11.
#
procedure fparse (input, dir, dir_size, root, root_size, ext, ext_size,
                  cl_index, cl_size, section, section_size, ksection,
                  ksection_size)

char    input[ARB]              # I:  Input pathname
char    dir[dir_size]           # O:  Directory part of pathname.
int     dir_size                # I:  Max size of dir.
char    root[root_size]         # O:  Root part of pathname.
int     root_size               # I:  Max size of root.
char    ext[ext_size]           # O:  Extension part of pathname.
int     ext_size                # I:  Max size of extension.
int     cl_index                # O:  The cluster index.
int     cl_size                 # O:  The cluster size.
char    section[section_size]   # O:  The section part of pathname.
int     section_size            # I:  Max size of section.
char    ksection[ksection_size] # O:  The remainder of the pathname.
int     ksection_size           # I:  Max size of ksection.

# Declarations
int     i                       # Generic.
int     len_dir                 # Length of the directory spec.

pointer cluster                 # Cluster.
pointer last_period             # Pointer to the last period.
pointer new_cluster             # Cluster without the directory spec.
pointer ptr                     # Pointer into strings.
pointer sp                      # Stack pointer.

string  wildcards       "*?"

# Function prototypes.
int     fnldir(), stridxs()
bool    streq()

begin
        
        call smark(sp)
        call salloc (cluster, SZ_LINE, TY_CHAR)
        
        # Parse the name with the (illegal) call imparse.
        call imparse (input, Memc[cluster], SZ_LINE, ksection,
                      ksection_size, section, section_size, cl_index,
                      cl_size)

        # Further parse the the cluster name into directory, root,
        # and extension.
        # Wildcards are a problem.  The above only deals with fully qualified
        # pathnames, not templates.  But, it seems it could be done.  Scan
        # the directory for wildcards and try to parse out a bit more.  The
        # assumption made is that directories cannot be wildcarded.
        root[1] = EOS
        ext[1] = EOS
        len_dir = fnldir (Memc[cluster], dir, dir_size)
        i = stridxs (wildcards, dir)
        if (i > 0) {
            dir[i] = EOS
            len_dir = fnldir (dir, dir, dir_size)
        }

        # Now there is just root and extension.  Check to see if root is just
        # the relative directory names.  If so, append them to the directory
        # specification.
        new_cluster = cluster + len_dir
        if (streq (Memc[new_cluster], ".") || streq (Memc[new_cluster], "..")) {
            call strcat (Memc[new_cluster], dir, dir_size)
            call strcat ("/", dir, dir_size)
        }

        # Else, find the extension.  This is just the last found "." in the
        # specification.
        else {
            last_period = NULL
            ptr = new_cluster
            while (Memc[ptr] != EOS) {
                if ( Memc[ptr] == '.')
                    last_period = ptr
                ptr = ptr + 1
            }
            if (last_period == NULL) {
                call strcpy (Memc[new_cluster], root, root_size)
                ext[1] = EOS
            } else {
                Memc[last_period] = EOS
                call strcpy (Memc[new_cluster], root, root_size )
                Memc[last_period] = '.'
                call strcpy (Memc[last_period], ext, ext_size)
            }
        }

        # That's all folks.
        call sfree(sp)
        
end
#---------------------------------------------------------------------------
# End of fparse
#---------------------------------------------------------------------------
