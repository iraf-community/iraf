C----------------------------------------------------------------------
        subroutine ftgabc(nfield,tform,space, rowlen,tbcol,status)

C       Get ASCII table Beginning Columns
C       determine the byte offset of the beginning of each field of a 
C       ASCII table, and the total width of the table

C       nfield i  number of fields in the binary table
C       tform  c  array of FITS datatype codes of each column.
C                 must be left justified in the string variable
C       space  i  number of blank spaces to insert between each column
C       OUTPUT PARAMETERS:
C       rowlen i  total width of the table, in bytes
C       tbcol  i  beginning position of each column (first column begins at 1)
C       status i  returned error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1992

        integer nfield,space,rowlen,tbcol(*),status
        character*(*) tform(*)
        integer i,j,ival

        if (status .gt. 0)return

        rowlen=0
        do 100 i=1,nfield
                if (tform(i)(2:2) .eq. ' ')then
C                       no explicit width; assume width=1
                        ival=1
                else
C                       find the field width characters
                        j=2
10                      j=j+1
                        if (tform(i)(j:j) .eq. ' ' .or. 
     &                      tform(i)(j:j) .eq. '.')then
C                           read the width
                            call ftc2ii(tform(i)(2:j-1),ival,status)
                        else
C                           keep looking for the end of the width field
                            go to 10
                        end if
                        tbcol(i)=rowlen+1
                        rowlen=rowlen+ival+space
                end if
100     continue

C       don't add space after the last field
        rowlen=rowlen-space
        end
