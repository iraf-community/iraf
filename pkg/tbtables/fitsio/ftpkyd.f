C--------------------------------------------------------------------------
        subroutine ftpkyd(ounit,keywrd,dval,decim,comm,status)

C       write a double precision value to a header record in E format
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

        key=keywrd
        cmnt=comm

C       convert double precision to E format character string
        call ftd2e(dval,decim,value,vlen,status)

C       write the keyword record
        call ftprec(ounit,key//'= '//value(1:vlen)//' / '//cmnt,status)
        end
