C--------------------------------------------------------------------------
        subroutine ftikyd(ounit,keywrd,dval,decim,comm,status)

C       insert a double E keyword into the header at the current position
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       dval    d  keyword value 
C       decim   i  number of decimal places to display in value field
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, March 1993

        character*(*) keywrd,comm
        integer ounit,status,decim
        double precision dval

        character value*35,key*8,com*47
        character*80 record
        integer nkeys,keypos,vlen

        if (status .gt. 0)return

C       convert double to F format character string and construct the record
        call ftd2e(dval,decim,value,vlen,status)
        key=keywrd
        com=comm
        record=key//'= '//value(1:vlen)//' / '//com

        call ftghps(ounit,nkeys,keypos,status)
        call ftirec(ounit,keypos,record,status)
        end
