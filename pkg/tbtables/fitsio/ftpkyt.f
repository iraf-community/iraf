C--------------------------------------------------------------------------
        subroutine ftpkyt(ounit,keywrd,jval,dval,comm,status)

C       concatinate a integer value with a double precision fraction
C       and write it to the FITS header along with the comment string
C       The value will be displayed in F28.16 format
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       jval    i  integer part of the keyword value
C       dval    d  fractional part of the keyword value
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Sept 1992

        character*(*) keywrd,comm
        double precision dval
        integer ounit,jval,status,dlen
        character dstr*35,jstr*20,key*8,cmnt*48

        if (status .gt. 0)return

        if (dval .ge. 1.0  .or. dval .lt.  0.)then
                status = 402
        end if

        key=keywrd
        cmnt=comm

C       convert integer to C*20 character string
        call fti2c(jval,jstr,status)

C       convert double precision to E23.16 format character string
        call ftd2e(dval,20,dstr,dlen,status)

C       write the concatinated keyword record
        call ftprec(ounit,key//'= '//jstr(10:20)//'.'//
     1   dstr(2:2)//dstr(4:18)//' / '//cmnt,status)
        end
