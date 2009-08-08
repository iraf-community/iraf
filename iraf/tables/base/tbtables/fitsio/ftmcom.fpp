C--------------------------------------------------------------------------
        subroutine ftmcom(ounit,keywrd,comm,status)

C       modify a the comment string in a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       comm    c  new keyword comment (max of 72 characters long)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Feb 1992

        character*(*) keywrd,comm
        integer ounit,status,lenval,ncomm
        character value*80,knam*8,cmnt*72
        
        if (status .gt. 0)return

        knam=keywrd

C       find the old keyword + value string
        call ftgcrd(ounit,knam,value,status)
        if (status .eq. 202)then
          call ftpmsg('FTMCOM Could not find the '//knam//' keyword.')
          return
        end if

        call ftprsv(value,lenval,status)
        
        cmnt=comm

C       find amount of space left for comment string (3 spaces needed for ' / ')
        ncomm=77-lenval

C       write the keyword record if there is space
        if (ncomm .gt. 0)then
          call ftmodr(ounit,
     &    value(1:lenval)//' / '//cmnt(1:ncomm),status)
        end if  
        end
