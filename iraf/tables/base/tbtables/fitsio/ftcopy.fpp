C----------------------------------------------------------------------
        subroutine ftcopy(iunit,ounit,moreky,status)

C       copies the CHDU from IUNIT to the CHDU of OUNIT.
C       This will also reserve space in the header for MOREKY keywords
C       if MOREKY > 0.

C       iunit   i  fortran unit number of the input file to be copied
C       ounit   i  fortran unit number of the output file to be copied to
C       moreky  i  create space in header for this many more keywords
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Jan, 1992

        integer iunit,ounit,moreky,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer ibuff,obuff,i,nkeys,nadd
        integer bitpix,naxis,naxes(99),pcount,gcount
        character hrec*80
        logical simple,extend

        if (status .gt. 0)return

        if (iunit .eq. ounit)then
                status=101
                return
        end if

        ibuff=bufnum(iunit)
        obuff=bufnum(ounit)

C       find out the number of keywords which exist in the input CHDU
        call ftghsp(iunit,nkeys,nadd,status)

C       copy the keywords one at a time to the output CHDU
        if ( (chdu(ibuff) .eq. 1 .and. chdu(obuff) .ne. 1) .or.
     &     (chdu(ibuff) .ne. 1 .and. chdu(obuff) .eq. 1) )then
C               copy primary array to image extension, or vise versa

C               copy the required keywords:
                simple=.true.
                extend=.true.
                call ftghpr(iunit,99,simple,bitpix,naxis,
     &          naxes,pcount,gcount,extend,status)
                if (status .gt. 0)return
                call ftphpr(ounit,simple,bitpix,naxis,
     &          naxes,pcount,gcount,extend,status)
                if (status .gt. 0)return

C               copy remaining keywords, excluding pcount, gcount and extend
                do 10 i=naxis+4,nkeys
                    call ftgrec(iunit,i,hrec,status)
                    if (hrec(1:8) .ne. 'PCOUNT  ' .and.
     &                  hrec(1:8) .ne. 'GCOUNT  ' .and.
     &                  hrec(1:8) .ne. 'EXTEND  ')then
                           call ftprec(ounit,hrec,status)
                    end if
10              continue
        else 
C               just copy all the keys exactly from the input file to the output
                do 20 i=1,nkeys
                    call ftgrec(iunit,i,hrec,status)
                    call ftprec(ounit,hrec,status)
20              continue
        end if
  
C       reserve space for more keywords (if moreky > 0)
        call fthdef(ounit,moreky,status)

C       now ccopy the data from the input CHDU to the output CHDU
        call ftcpdt(iunit,ounit,status)

        end
