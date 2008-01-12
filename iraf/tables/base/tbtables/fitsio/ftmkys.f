C--------------------------------------------------------------------------
        subroutine ftmkys(ounit,keywrd,strval,comm,status)

C       modify a character string value header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C       modifed 7/93 to support string keywords continued over multiple cards
C       modified 9/94 to support the OGIP long string convention

        character*(*) keywrd,strval,comm
        integer ounit,status

        integer clen,i,nvalue,ncomm
        character keynam*8,value*70,cmnt*48,bslash

C-------COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C-------END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        if (status .gt. 0)return

C       check if the new value is too long to fit in a single 'card image'
        clen=len(strval)
        if (clen .le. 68)go to 20

        do 10 i=clen,69,-1
                if (strval(i:i) .ne. ' ')go to 100
10      continue

C       now check that the old keyword is not continued over multiple cards
C       read the (first line) of the existing keyword

20      call ftgkey(ounit,keywrd,value,cmnt,status)
        if (status .gt. 0)go to 900

C       is last character of the value a backslash or & ?
C       have to use 2 \\'s because the SUN compiler treats 1 \ as an escape
        bslash='\\'
        do 30 i=70,1,-1
                if (value(i:i) .ne. ' '.and. value(i:i).ne.'''')then
                    if (value(i:i) .eq. bslash .or. 
     &                  value(i:i) .eq. '&')then
C                     backspace the current header pointer by one record
                      nxthdr(bufnum(ounit))=nxthdr(bufnum(ounit))-80
                      go to 100
                    else
                      go to 40
                    end if
                end if
30      continue

C       OK, we can simply overwrite the old keyword with the new
40      continue

C       overwrite the old comment unless user supplied the magic value
        if (comm .ne. '&')then
                cmnt=comm
        end if
C       convert string to quoted character string (max length = 70 characters)
        call fts2c(strval,value,clen,status)
        if (status .gt. 0)go to 900

C       find amount of space left for comment string
C       (assume 10 char. for 'keyword = ', and 3 between value and comment)
C       which leaves 67 spaces for the value string + comment string
        nvalue=max(20,clen)
        ncomm=67-nvalue

C       write the keyword record
        keynam=keywrd
        if (ncomm .gt. 0)then
C         there is space for a comment
          call ftmodr(ounit,
     &    keynam//'= '//value(1:nvalue)//' / '//cmnt(1:ncomm),status)
        else
C         no room for a comment
          call ftmodr(ounit,
     &    keynam//'= '//value(1:nvalue)//'   ',status)
        end if  
        go to 900

100     continue

C       Either the old or new keyword is continued over multiple
C       header card images, so have to use a less efficient way to modify
C       the keyword by completely deleting the old and inserting the new.

C       read the old comment, if we need to preserve it
        if (comm .eq. '&')then
                call ftgkys(ounit,keywrd,value,cmnt,status)
                if (status .gt. 0)go to 900
C               reset the current header pointer by 2 records to make
C               it faster (usually) to find and delete the keyword
                nxthdr(bufnum(ounit))=nxthdr(bufnum(ounit))-160
        else
                cmnt=comm
        end if

C       delete the old keyword
        call ftdkey(ounit,keywrd,status)
        if (status .gt. 0)go to 900

C       insert the new keyword
        call ftikys(ounit,keywrd,strval,cmnt,status)

900     continue
        end
