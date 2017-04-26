C--------------------------------------------------------------------------
        subroutine ftpini(iunit,status)

C       initialize the parameters defining the structure of the primary data

C       iunit   i  Fortran I/O unit number
C       OUTPUT PARAMETERS:
C       status  i  returned error status (0=ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer iunit,status

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nf,nb,ne
        parameter (nb = 20)
        parameter (nf = 3000)
        parameter (ne = 200)
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

        integer ibuff,bitpix,naxis,naxes(99),pcnt,gcnt,ttype
        integer blank,bytlen,npix,i,nblank,tstat
        double precision bscale,bzero
        logical simple,extend,groups
        character*8 comm

        if (status .gt. 0)return
        groups=.false.

C       define the number of the buffer used for this file
        ibuff=bufnum(iunit)

C       store the type of HDU (0=primary array or image extension)
        hdutyp(ibuff)=0

C       temporarily set the location of the end of the header to a huge number
        hdend(ibuff)=2000000000
        hdstrt(ibuff,chdu(ibuff)+1)=2000000000

C       get the standard header keywords
        tstat=status
        call ftgphx(iunit,99,simple,bitpix,naxis,naxes,
     &        pcnt,gcnt,extend,bscale,bzero,blank,nblank,status)
        if (status .eq. 251)then
C               ignore 'unknown extension type' error, and go on
                status=tstat
        else if (status .gt. 0)then
                return
        end if

        if (naxis .gt. 99)then
C               the image array has too many dimensions for me to handle
                status=111
        call ftpmsg('This FITS image has too many dimensions (FTPINI)')
                return
        end if

C       the 'END' record is 80 bytes before the current position, ignoring
C       any trailing blank keywords just before the END keyword.
        hdend(ibuff)=nxthdr(ibuff)-80*(nblank+1)

C       the data unit begins at the beginning of the next logical block
        dtstrt(ibuff)=((nxthdr(ibuff)-80)/2880+1)*2880

C       test for the presence of 'random groups' structure
        if (naxis .gt. 0 .and. naxes(1) .eq. 0)then
                tstat=status
                call ftgkyl(iunit,'GROUPS',groups,comm,status)
                if (status .gt. 0)then
                        status=tstat
                        groups=.false.
                end if
        end if

C       test  bitpix and set the datatype code value
        if (bitpix .eq. 8)then
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
        end if
        
C       calculate the size of the primary array
        if (naxis .eq. 0)then
                npix=0
        else
                if (groups)then
C                       NAXIS1 = 0 is a special flag for 'random groups'
                        npix=1
                else
                        npix=naxes(1)
                end if

                do 10 i=2,naxis
                        npix=npix*naxes(i)
10              continue
        end if

C       now we know everything about the array; just fill in the parameters:
C       the next HDU begins in the next logical block after the data
        hdstrt(ibuff,chdu(ibuff)+1)=
     &  dtstrt(ibuff)+((pcnt+npix)*bytlen*gcnt+2879)/2880*2880

C       initialize the fictitious heap starting address (immediately following
C       the array data) and a zero length heap.  This is used to find the
C       end of the data when checking the fill values in the last block. 
        scount(ibuff)=0
        theap(ibuff)=(pcnt+npix)*bytlen*gcnt
        nxheap(ibuff)=0

C       quit if there is no data
        if (naxis .eq. 0)then
                tfield(ibuff)=0
                rowlen(ibuff)=0
                go to 900
        end if

C       the primary array is actually interpreted as a binary table.  There
C       are two columns: the first column contains the 
C       group parameters, if any, and the second column contains the
C       primary array of data.  Each group is in a separate row of the table.

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
                tnull(1+tstart(ibuff))=blank
                tnull(2+tstart(ibuff))=blank
                tscale(1+tstart(ibuff))=1.
                tscale(2+tstart(ibuff))=bscale
                tzero(1+tstart(ibuff))=0.
                tzero(2+tstart(ibuff))=bzero
                tbcol(1+tstart(ibuff))=0
                tbcol(2+tstart(ibuff))=pcnt*bytlen
                rowlen(ibuff)=(pcnt+npix)*bytlen
        end if

900     continue
        end
