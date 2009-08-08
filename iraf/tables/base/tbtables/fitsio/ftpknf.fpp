C--------------------------------------------------------------------------
        subroutine ftpknf(ounit,keywrd,nstart,nkey,rval,decim,comm,
     &                    status)

C       write an array of real*4 values to header records in F format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       rval    r  array of keyword values
C       decim   i  number of decimal places to display in the value field
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm(*)
        integer nstart,nkey,decim,ounit,status,i,j
        real rval(*)
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                  call ftpkyf(ounit,keynam,rval(i),decim,comm1,status)
                else
                  call ftpkyf(ounit,keynam,rval(i),decim,comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
