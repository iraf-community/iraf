c
c-----------------------------------------------------------------------
c main program: time-efficient radix-4 fast fourier transform
c author:       l. robert morris
c               department of systems engineering and computing science
c               carleton university, ottawa, canada k1s 5b6
c
c input:        the array "a" contains the data to be transformed
c-----------------------------------------------------------------------
c
c       test program for autogen radix-4 fft
c
        dimension a(2048),b(2048)
        common /aa/a
c
        ioutd=i1mach(2)
c
c       compute dft and idft for n = 64, 256, and 1024 complex points
c
        do 1 mm=3,5
        n=4**mm
        do 2 j=1,n
        a(2*j-1)=uni(0)
        a(2*j  )=uni(0)
        b(2*j-1)=a(2*j-1)
2       b(2*j  )=a(2*j)
c
c       forward dft
c
        call radix4(mm,1,-1)
c
        if(mm.ne.3) go to 5
c
c       list dft input, output for n = 64 only
c
        write(ioutd,98)
        write(ioutd,100)
        do 3 j=1,n
        write(ioutd,96) b(2*j-1),b(2*j),a(2*j-1),a(2*j)
3       continue
c
c       inverse dft
c
5       call radix4(mm,0, 1)
c
c       list dft input, idft output for n = 64 only
c
        if(mm.ne.3) go to 7
c
        write(ioutd,99)
        write(ioutd,100)
        do 6 j=1,n
        write(ioutd,96) b(2*j-1),b(2*j),a(2*j-1),a(2*j)
6       continue
c
c       calculate rms error
c
7       err=0.0
        do 8 j=1,n
8       err=err+(a(2*j-1)-b(2*j-1))**2+(a(2*j)-b(2*j))**2
        err=sqrt(err/float(n))
        write(ioutd,97) mm,err
1       continue
c
96      format(1x,4(f10.6,2x))
97      format(1x,20h   rms error for m =,i2,4h is ,e14.6/)
98      format(1x,43h       dft input                dft  output/)
99      format(1x,43h       dft input                idft output/)
100     format(1x,44h   real        imag       real          imag/)
        stop
        end
