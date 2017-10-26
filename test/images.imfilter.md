# imfilter - Image filtering package

## boxcar - Boxcar smooth a list of 1 or 2-D images

Smooth an image using a 3 by 3 smoothing box and nearest neighbor
boundary extension.

```
cl> boxcar dev$pix pix.box 3 3
cl> imstat pix.box
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.box    262144     107.9      119.       30.    12240.
```

## convolve - Convolve a list of 1 or 2-D images with a rectangular filter

Convolve an image with a kernel using string entry mode and wrap
around boundary extension.

See https://github.com/iraf/iraf-v216/issues/109

Test options: `xfail`
```
cl> convolve dev$pix pix.cnv "1. 1. 1.; 1. 0. 1.; 1. 1. 1." bound=wrap
cl> imstat pix.cnv
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.cnv    262144     860.5     804.5   -30822.    32055.
```

```
cl> chpixtype dev$pix rpix real
Image: dev$pix (short) -> Image: rpix (real)
cl> convolve rpix rpix.cnv "1. 1. 1.; 1. 0. 1.; 1. 1. 1." bound=wrap
cl> imstat rpix.cnv
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
             rpix.cnv    262144     866.5     942.5      230.    90232.
```


## fmedian - Quantize and box median filter a list of 1D or 2D images

Median filter the test image dev$pix rejecting any pixels < 5 or
greater than 19935 from the medianing process.

```
cl> fmedian dev$pix pix.fme 5 5 hmin=-1 hmax=20000 zmin=-1.0 zmax=20000 zloreject=5 zhireject=20000
5x5 Box median filter dev$pix to pix.fme
    Pixels < 5. or > 20000. excluded from the median filter
cl> imstat pix.fme
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.fme    262144     106.4     93.33       36.     4568.
```

## fmode - Quantize and box modal filter a list of 1D or 2D images

Modal filter the test image dev$pix rejecting any pixels < 5 or
greater than 19935 from the mode computing process.

```
cl> fmode dev$pix pix.fmo 5 5 hmin=-1 hmax=20000 zmin=-1.0 zmax=20000 zloreject=5 zhireject=20000
5x5 Box modal filter dev$pix to pix.fmo
    Pixels < 5. or > 20000. excluded from the modal filter
cl> imstat pix.fmo
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.fmo    262144     102.2     96.15    -6312.     3280.
```

## frmedian - Quantize and ring median filter a list of 1D or 2D images

Median filter a 16 bit CCD image using a circular ring filter with an
inner radius of 4 pixels and a width of 1 pixel.
    
```
cl> frmedian dev$pix pix.frmed 4.0 5.0 hmin=-32768 hmax=32767 zmin=-32768.  zmax=32767.
Ring rin=4.0 rout=5.0 median filter dev$pix to pix.frmed
cl> imstat pix.frmed
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
            pix.frmed    262144     104.2     80.83       36.     1738.
```

## frmode - Quantize and ring modal filter a list of 1D or 2D images

Modal filter a 16 bit CCD image using a circular ring filter with an
inner radius of 4 pixels and a width of 1 pixel.

```
cl> frmode dev$pix pix.frmo 4.0 5.0 hmin=-32768 hmax=32767 zmin=-32768. zmax=32767.
Ring rin=4.0 rout=5.0 modal filter dev$pix to pix.frmo
cl> imstat pix.frmo
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
             pix.frmo    262144     95.38     94.47    -4119.     1743.
```

## gauss - Convolve a list of 1 or 2-D images with an elliptical Gaussian

Convolve an image with an elliptical Gaussian function whose sigma in
the major and minor axis direction is 2.0 and 1.5 respectively, and
whose position angle is 45 degrees, using wrap around boundary
extension. In this case the full 2D kernel is used by default.

```
cl> gauss dev$pix pix.gau 2.0 ratio=.75 theta=45. bound=wrap
cl> imstat pix.gau
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.gau    262144     107.8     105.3       36.     6151.
```

## gradient - Convolve a list of 1 or 2-D images with a gradient operator

Calculate the gradient in the 180 degree direction using nearest
neighbor boundary extension.

```
cl> gradient dev$pix pix.odeg 180
cl> imstat pix.odeg fields="image,npix,stddev,min,max"
#               IMAGE      NPIX    STDDEV       MIN       MAX
             pix.odeg    262144     42.85    -6983.     6529.
``` 

## laplace - Laplacian filter a list of 1 or 2-D images

Convolve an image with the Laplacian filter xyall using nearest
neighbor boundary extension. The result quality is a bit bad, so we
limit the precision to one digit.

Test options: `decimals=1`
```
cl> laplace dev$pix pix.lap laplace=xyall
cl> imstat pix.lap fields="image,npix,stddev,min,max"
#               IMAGE      NPIX    STDDEV       MIN       MAX
              pix.lap    262144     77.22   -20349.     4191.
```

## median - Median box filter a list of 1D or 2D images

Median filter an image using a 5 by 5 window and nearest pixel
boundary extension.
    
```
cl> median dev$pix pix.5by5 5 5
5x5 Box median filter dev$pix to pix.5by5
cl> imstat pix.5by5
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
             pix.5by5    262144     106.4     93.33       36.     4568.
```

## mode - Modal box filter a list of 1D or 2D images

Modal filter an image using a 3 by 3 window and constant boundary
extension.

```
cl> mode dev$pix pix.3by3 3 3 boun=const const=0.
3x3 Box modal filter dev$pix to pix.3by3
cl> imstat pix.3by3
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
             pix.3by3    262144     105.8      101.    -3482.    10975.
```

## rmedian - Ring median filter a list of 1D or 2D images

Median filter the test image dev$pix rejecting any pixels < 5 or
greater than 19935 from the medianing process using a circular filter
of outer radius 5.0.
    
```
cl> rmedian dev$pix pix.rme 0.0 5.0 zloreject=5 zhireject=19935
Ring rin=0.0 rout=5.0 median filter dev$pix to pix.rme
    Pixels < 5. or > 19935. excluded from the median filter
cl> imstat pix.rme
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.rme    262144     105.2     84.41       37.     2110.
```

## rmode - Ring modal filter a list of 1D or 2D images

Modal filter the test image dev$pix rejecting any pixels < 5 or
greater than 19935 from the modal filter using a circular filter of
outer radius 5.0.
    
```
cl> rmode dev$pix pix.rmo 0.0 5.0 zloreject=5 zhireject=19935
Ring rin=0.0 rout=5.0 modal filter dev$pix to pix.rmo
    Pixels < 5. or > 19935. excluded from the modal filter
cl> imstat pix.rmo
#               IMAGE      NPIX      MEAN    STDDEV       MIN       MAX
              pix.rmo    262144     98.58     90.65    -2937.     1144.
```
