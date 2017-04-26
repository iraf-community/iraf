C--------------------------------------------------------------------------
        subroutine ftpdat(ounit,status)

C       write the current date to the DATE keyword in the ounit CHU
C
C       ounit   i  fortran output unit number
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, Jan 1992

        integer ounit,status,dd,mm,yy
        character datstr*8

C       call the system dependent routine to get the current date
        call ftgsdt(dd,mm,yy,status)
        if (status .gt. 0)return

        datstr='  /  /  '
        write(datstr(1:2),1001)dd
        write(datstr(4:5),1001)mm
        write(datstr(7:8),1001)yy
1001    format(i2)

C       replace blank with leading 0 in each field if required
        if (datstr(1:1) .eq. ' ')datstr(1:1)='0'
        if (datstr(4:4) .eq. ' ')datstr(4:4)='0'
        if (datstr(7:7) .eq. ' ')datstr(7:7)='0'
        
C       update the DATE keyword
        call ftukys(ounit,'DATE',datstr,
     &             'FITS file creation date (dd/mm/yy)',status)
        end
