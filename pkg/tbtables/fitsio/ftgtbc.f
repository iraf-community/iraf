C----------------------------------------------------------------------
        subroutine ftgtbc(tfld,tdtype,trept,tbcol,lenrow,status)

C       Get Table Beginning Columns
C       determine the byte offset of the beginning of each field of a 
C       binary table

C       tfld   i  number of fields in the binary table
C       tdtype i array of numerical datatype codes of each column
C       trept  i array of repetition factors for each column
C       OUTPUT PARAMETERS:
C       tbcol  i array giving the byte offset to the start of each column
C       lenrow i total width of the table, in bytes
C       status i  returned error status
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C       modified 6/17/92 to deal with ASCII column trept values measured
C       in units of characters rather than in terms of number of repeated
C       strings.

        integer tfld,tdtype(*),trept(*),tbcol(*),lenrow
        integer status,i,nbytes
        character ifld*4

        if (status .gt. 0)return

C       the first column always begins at the first byte of the row:
        tbcol(1)=0
        
        do 100 i=1,tfld-1
                if (tdtype(i) .eq. 16)then
C                       ASCII field; each character is 1 byte
                        nbytes=1
                else if (tdtype(i) .gt. 0)then
                        nbytes=tdtype(i)/10
                else if (tdtype(i) .eq. 0)then
C                   error: data type of column not defined! (no TFORM keyword)
                        status=232
                        write(ifld,1000)i
1000                    format(i4)
                        call ftpmsg('Field'//ifld//' of the binary'//
     &                  ' table has no TFORMn keyword')
                        return
                else 
C                       this is a descriptor field: 2J
                        nbytes=8
                end if

                if (nbytes .eq. 0)then
C                       this is a bit array
                        tbcol(i+1)=tbcol(i)+(trept(i)+7)/8
                else
                        tbcol(i+1)=tbcol(i)+trept(i)*nbytes
                end if
100     continue

C       determine the total row width
        if (tdtype(tfld) .eq. 16)then
C               ASCII field; each character is 1 byte
                nbytes=1
        else if (tdtype(tfld) .gt. 0)then
                nbytes=tdtype(tfld)/10
        else if (tdtype(i) .eq. 0)then
C            error: data type of column not defined! (no TFORM keyword)
                status=232
                write(ifld,1000)tfld
                call ftpmsg('Field'//ifld//' of the binary'//
     &                  ' table is missing required TFORMn keyword.')
                return
        else
C               this is a descriptor field: 2J
                nbytes=8
        end if
        if (nbytes .eq. 0)then
C               this is a bit array
                lenrow=tbcol(tfld)+(trept(tfld)+7)/8
        else
                lenrow=tbcol(tfld)+trept(tfld)*nbytes
        end if

        end
