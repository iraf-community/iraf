C--------------------------------------------------------------------------
        subroutine ftgtcl(iunit,colnum,datcod,repeat,width,status)

C       get the datatype of the column, as well as the vector
C       repeat count and (if it is an ASCII character column) the
C       width of a unit string within the column.  This supports the
C       TFORMn = 'rAw' syntax for specifying arrays of substrings.


C       iunit   i  Fortran i/o unit number
C       colnum  i  number of the column (first column = 1)

C       datcod  i  returned datatype code
C       repeat  i  number of elements in the vector column
C       width   i  width of unit string in character columns
C       status  i  returned error status
C
C       written by Wm Pence, HEASARC/GSFC, November 1994

        integer iunit,colnum,datcod,repeat,width,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C       END OF COMMON BLOCK DEFINITIONS------------------------------------

        integer ibuff,dummy
        character keywrd*8,tform*24,comm*20

        if (status .gt. 0)return

C       construct the keyword name
        call ftkeyn('TFORM',colnum,keywrd,status)

C       get the keyword value
        call ftgkys(iunit,keywrd,tform,comm,status)
        if (status .gt. 0)then
            call ftpmsg('Could not read the '//keywrd//' keyword.')
            return
        end if

C       parse the keyword value
        ibuff=bufnum(iunit)
        if (hdutyp(ibuff) .eq. 1)then
C           this is an ASCII table
            repeat=1
            call ftasfm(tform,datcod,width,dummy,status)

        else if (hdutyp(ibuff) .eq. 2)then
C           this is a binary table
            call ftbnfm(tform,datcod,repeat,width,status)

        else
C           error: this HDU is not a table
            status=235
            return
        end if
        end
