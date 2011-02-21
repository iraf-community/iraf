C----------------------------------------------------------------------
        subroutine ftphpr(ounit,simple,bitpix,naxis,naxes,
     &                    pcount,gcount,extend,status)

C       write required primary header keywords
C
C       ounit   i  fortran output unit number
C       simple  l  does file conform to FITS standard?
C       bitpix  i  number of bits per data value
C       naxis   i  number of axes in the data array
C       naxes   i  array giving the length of each data axis
C       pcount  i  number of group parameters
C       gcount  i  number of random groups
C       extend  l  may extensions be present in the FITS file?
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,bitpix,naxis,naxes(*),pcount,gcount,status,i,ibuff
        character comm*50,caxis*20,clen*3
        logical simple,extend

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

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (chdu(ibuff) .eq. 1)then
            if (simple)then
                comm='file does conform to FITS standard'
            else
                comm='file does not conform to FITS standard'
            end if
            call ftpkyl(ounit,'SIMPLE',simple,comm,status)
        else  
            comm='IMAGE extension'
            call ftpkys(ounit,'XTENSION','IMAGE',comm,status)
        end if

C       test for legal value of bitpix
        call fttbit(bitpix,status)
        comm='number of bits per data pixel'
        call ftpkyj(ounit,'BITPIX',bitpix,comm,status)
        if (status .gt. 0)go to 900

        if (naxis .ge. 0 .and. naxis .le. 999)then
                comm='number of data axes'
                call ftpkyj(ounit,'NAXIS',naxis,comm,status)
        else
C               illegal value of naxis
                status=212
                write(caxis,1001)naxis
1001            format(i20)
                call ftpmsg('NAXIS ='//caxis//' in the call to FTPHPR '
     &          //'is illegal.')
                go to 900
        end if

        comm='length of data axis'
        do 10 i=1,naxis
                if (naxes(i) .ge. 0)then
                        write(comm(21:23),1000)i
1000                    format(i3)      
                        call ftpknj(ounit,'NAXIS',i,1,naxes(i),comm,
     &                              status)
                else
C                       illegal NAXISnnn keyword value
                        status=213
                        write(clen,1000)i
                        write(caxis,1001)naxes(i)
        call ftpmsg('In call to FTPHPR, axis '//clen//
     &  ' has illegal negative size: '//caxis)
                        go to 900
                end if
10      continue

        if (chdu(ibuff) .eq. 1)then
C               only write the EXTEND keyword to primary header if true
                if (extend)then
                        comm='FITS dataset may contain extensions'
                        call ftpkyl(ounit,'EXTEND',extend,comm,status)
                end if

C               write the PCOUNT and GCOUNT values if nonstandard
                if (pcount .gt. 0 .or. gcount .gt. 1)then
                    comm='random group records are present'
                    call ftpkyl(ounit,'GROUPS',.true.,comm,status)
                    comm='number of random group parameters'
                    call ftpkyj(ounit,'PCOUNT',pcount,comm,status)  
                    comm='number of random groups'
                    call ftpkyj(ounit,'GCOUNT',gcount,comm,status)
                end if

                call ftpcom(ounit,'FITS (Flexible Image Transport '//
     & 'System) format defined in Astronomy and',status)
                call ftpcom(ounit,'Astrophysics Supplement Series '//
     & 'v44/p363, v44/p371, v73/p359, v73/p365.',status)
                call ftpcom(ounit,'Contact the NASA Science '//
     & 'Office of Standards and Technology for the',status)
                call ftpcom(ounit,'FITS Definition document '//
     & '#100 and other FITS information.',status)

        else 
                comm='number of random group parameters'
                call ftpkyj(ounit,'PCOUNT',pcount,comm,status)                
                comm='number of random groups'
                call ftpkyj(ounit,'GCOUNT',gcount,comm,status) 
        end if

900     continue
        end
