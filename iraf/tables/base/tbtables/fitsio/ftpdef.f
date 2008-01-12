C--------------------------------------------------------------------------
        subroutine ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,
     &                    status)

C       Primary data DEFinition
C       define the structure of the primary data unit or an IMAGE extension
C
C       ounit   i  Fortran I/O unit number
C       bitpix  i  bits per pixel value
C       naxis   i  number of data axes
C       naxes   i  length of each data axis (array)
C       pcount  i  number of group parameters
C       gcount  i  number of 'random groups'
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,bitpix,naxis,naxes(*),pcount,gcount,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne,nf
        parameter (nb = 20)
        parameter (ne = 200)
        parameter (nf = 3000)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        integer tfield,tstart,tbcol,rowlen,tdtype,trept,tnull,scount
        integer theap,nxheap
        double precision tscale,tzero
        common/ft0002/tfield(nb),tstart(nb),tbcol(nf),rowlen(nb),
     &  tdtype(nf),trept(nf),tscale(nf),tzero(nf),tnull(nf),scount(nb)
     &  ,theap(nb),nxheap(nb)
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------
        
        integer ibuff,ttype,bytlen,npix,i,pcnt,gcnt
        character caxis*20

        if (status .gt. 0)return

        ibuff=bufnum(ounit)

        if (dtstrt(ibuff) .lt. 0)then
C               freeze the header at its current size
                call fthdef(ounit,0,status)
                if (status .gt. 0)return
        end if

C       check for error conditions
        if (naxis .lt. 0)then
                status=212
                write(caxis,1001)naxis
1001            format(i20)
                call ftpmsg('NAXIS ='//caxis//' in the call to FTPDEF '
     &          //'is illegal.')

        else if (pcount .lt. 0)then
                status=214
        else if (gcount .lt. 0)then
                status=215
        else
                go to 5
        end if
        return

C       test that bitpix has a legal value and set the datatype code value
5       if (bitpix .eq. 8)then
                ttype=11
                bytlen=1
        else if (bitpix .eq. 16)then
                ttype=21
                bytlen=2
        else if (bitpix .eq. 32)then
                ttype=41
                bytlen=4
        else if (bitpix .eq. -32)then
                ttype=42
                bytlen=4
        else if (bitpix .eq. -64)then
                ttype=82
                bytlen=8
        else
C               illegal value of bitpix
                status=211
                return
        end if

C       calculate the number of pixels in the array
        if (naxis .eq. 0)then
C               no data
                npix=0
                gcnt=0
                pcnt=0
        else
C               make sure that the gcount is not zero
                gcnt=max(gcount,1)
                pcnt=pcount        
                npix=1
                do 10 i=1,naxis
                        if (naxes(i) .ge. 0)then
C       The convension used by 'random groups' with NAXIS1 = 0 is not
C       directly supported here.  If one wants to write a 'random group'
C       FITS file, then one should call FTPDEF with naxes(1) = 1, but
C       then write the required header keywords (with FTPHPR) with 
C       naxes(1) = 0.
                                npix=npix*naxes(i)
                        else if (naxes(i) .lt. 0)then
                                status=213
                                return
                        end if
10              continue
        end if
C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &          dtstrt(ibuff)+((pcnt+npix)*bytlen*gcnt+2879)/2880*2880

C       the primary array is actually interpreted as a binary table.  There
C       are two columns: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data.  Each group is a separate row in the table.
C       The scaling and null values are set to the default values.

        hdutyp(ibuff)=0
        tfield(ibuff)=2

        if (nxtfld + 2 .gt. nf)then
C               too many columns open at one time; exceeded array dimensions
                status=111
        else
                tstart(ibuff)=nxtfld
                nxtfld=nxtfld+2
                tdtype(1+tstart(ibuff))=ttype
                tdtype(2+tstart(ibuff))=ttype
                trept(1+tstart(ibuff))=pcnt
                trept(2+tstart(ibuff))=npix
C               choose a special value to represent the absence of a blank value
                tnull(1+tstart(ibuff))=123454321
                tnull(2+tstart(ibuff))=123454321
                tscale(1+tstart(ibuff))=1.
                tscale(2+tstart(ibuff))=1.
                tzero(1+tstart(ibuff))=0.
                tzero(2+tstart(ibuff))=0.
                tbcol(1+tstart(ibuff))=0
                tbcol(2+tstart(ibuff))=pcnt*bytlen
                rowlen(ibuff)=(pcnt+npix)*bytlen
        end if

C       initialize the fictitious heap starting address (immediately following
C       the array data) and a zero length heap.  This is used to find the
C       end of the data when checking the fill values in the last block. 
        scount(ibuff)=0
        theap(ibuff)=(pcnt+npix)*bytlen*gcnt
        nxheap(ibuff)=0
        end
