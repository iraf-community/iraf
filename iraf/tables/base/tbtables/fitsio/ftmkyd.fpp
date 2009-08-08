C--------------------------------------------------------------------------
        subroutine ftmkyd(ounit,keywrd,dval,decim,comm,status)

C       modify a double precision value header record in E format
C       If it will fit, the value field will be 20 characters wide;
C       otherwise it will be expanded to up to 35 characters, left
C       justified.
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (max. 47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) keywrd,comm
        double precision dval
        integer ounit,status,decim,vlen
        character value*35,key*8,cmnt*48

C       find the old keyword
        call ftgkey(ounit,keywrd,value,cmnt,status)

        key=keywrd
C       check for special symbol indicating that comment should not be changed
        if (comm .ne. '&')then
              cmnt=comm
        end if

C       convert double precision to E format character string
        call ftd2e(dval,decim,value,vlen,status)

C       write the keyword record
        call ftmodr(ounit,key//'= '//value(1:vlen)//' / '//cmnt,status)
        end
