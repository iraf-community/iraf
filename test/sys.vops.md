# Test of sys/vops routines

## FFT842: fast fourier transform for complex input


This program replaces the vector z=x+iy by its finite discrete, complex
fourier transform if `in=0`. The inverse transform is calculated for `in=1`.

File: `test_fft842.x`
```
task test_fft842 = t_fft842

procedure eval_fft842(x, y, ndim)
int ndim
real x[ndim], y[ndim]
int j
int ier
begin
    call printf("FFT input  ")
    do j = 1, ndim {
        call printf(" (%6.3f;%4.3f)")
        call pargr(x[j])
        call pargr(y[j])
    }
    call fft842(0, ndim, x, y, ier)
    call printf("\nFFT output ")
    do j = 1, ndim {
        call printf(" (%6.3f;%4.3f)")
        call pargr(x[j])
        call pargr(y[j])
    }
    call fft842(1, ndim, x, y, ier)
    call printf("\nInverse    ")
    do j = 1, ndim {
        call printf(" (%6.3f;%4.3f)")
        call pargr(x[j])
        call pargr(y[j])
    }
    call printf("\n")
end

procedure t_fft842 ()
real x[8], y[8]
data x /1.0, 2.0, 1.0, -1.0, 1.5, 1.0, 0.5, 1.0/
data y /0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
begin
    call eval_fft842(x, y, 8)
end
```

Test options: `decimals=4`
```
cl> softools
cl> xc -x test_fft842.x
cl> task $test_fft842 = test_fft842.e
cl> test_fft842
FFT input   ( 1.000;0.00) ( 2.000;0.00) ( 1.000;0.00) (-1.000;0.00) ( 1.500;0.00) ( 1.000;0.00) ( 0.500;0.00) ( 1.000;0.00)
FFT output  ( 7.0000;0.0000) ( 1.6210;0.2100) ( 1.0000;-3.0000) (-2.6210;1.2100) ( 1.0000;0.0000) (-2.6210;-1.2000) ( 1.0000;3.0000) ( 1.6210;0.2000)
Inverse     ( 1.000;0.00) ( 2.000;0.00) ( 1.000;0.00) (-1.000;0.00) ( 1.500;0.00) ( 1.000;0.00) ( 0.500;0.00) ( 1.000;0.00)
```
