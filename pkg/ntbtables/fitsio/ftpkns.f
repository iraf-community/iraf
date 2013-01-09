C--------------------------------------------------------------------------
        subroutine ftpkns(ounit,keywrd,nstart,nkey,strval,comm,
     &                    status)

C       write an array of character string values to header records
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       nstart  i  starting sequence number (usually 1)
C       nkey    i  number of keywords to write
C       strval  c  array of keyword values
C       comm    c  array of keyword comments (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,strval(*),comm(*)
        integer nstart,nkey,ounit,status,i,j
        character keynam*8,comm1*48
        logical repeat

        if (status .gt. 0)return

C       check if the first comment string is to be repeated for all keywords
        call ftcrep(comm(1),comm1,repeat)

        j=nstart
        do 10 i=1,nkey
C               construct keyword name:
                call ftkeyn(keywrd,j,keynam,status)

C               write the keyword record
                if (repeat)then
                   call ftpkys(ounit,keynam,strval(i),comm1,status)
                else
                   call ftpkys(ounit,keynam,strval(i),comm(i),status)
                end if
                if (status .gt. 0)return
                j=j+1
10      continue
        end
