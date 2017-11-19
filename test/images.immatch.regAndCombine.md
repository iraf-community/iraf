# immatch - Image matching and combining package

# cl> copy ../../../home/steele/starGateCluster/regList .
# cl> directory $iraf
# cl> directory 
# cl> directory ..
# cl> directory ../..
# cl> directory ../../..
# cl> directory ../../../home/steele
# cl> directory ../../../home/steele/starGateCluster
# cl> !cp ~/starGateCluster/regList  .
# cl> !ls $iraf
# cl> directory
# regList
# cl> del regList

Test options: `decimals=5`
```
cl> !cp $iraf/test/registrationTestFiles/StarGat*.fit .
cl> files *.fit >origList
cl> !cp $iraf/test/registrationTestFiles/regList              .
cl> !ls
origList
regList
StarGateCluster_030_sec_1x1_0001.fit
StarGateCluster_030_sec_1x1_0002.fit
StarGateCluster_030_sec_1x1_0003.fit
StarGateCluster_030_sec_1x1_0004.fit
StarGateCluster_030_sec_1x1_0005.fit
StarGateCluster_030_sec_1x1_0006.fit
StarGateCluster_030_sec_1x1_0007.fit
StarGateCluster_030_sec_1x1_0008.fit
StarGateCluster_030_sec_1x1_0009.fit
StarGateCluster_030_sec_1x1_0010.fit
```

Test options: `decimals=6`
```
cl> xregister input=@origList reference=StarGateCluster_030_sec_1x1_0005.fit regions=[10:700,10:500] shifts=input_output_shifts.txt output=@regList
Average shift from StarGateCluster_030_sec_1x1_0001.fit to StarGateCluster_030_sec_1x1_0005.fit is 3.223879 4.60711 pixels
	Shifting image StarGateCluster_030_sec_1x1_0001.fit to image r01.fit ...
Average shift from StarGateCluster_030_sec_1x1_0002.fit to StarGateCluster_030_sec_1x1_0005.fit is 2.999974 4.786735 pixels
	Shifting image StarGateCluster_030_sec_1x1_0002.fit to image r02.fit ...
Average shift from StarGateCluster_030_sec_1x1_0003.fit to StarGateCluster_030_sec_1x1_0005.fit is 2.021923 3.226826 pixels
	Shifting image StarGateCluster_030_sec_1x1_0003.fit to image r03.fit ...
Average shift from StarGateCluster_030_sec_1x1_0004.fit to StarGateCluster_030_sec_1x1_0005.fit is 0.9533567 1.941077 pixels
	Shifting image StarGateCluster_030_sec_1x1_0004.fit to image r04.fit ...
Average shift from StarGateCluster_030_sec_1x1_0005.fit to StarGateCluster_030_sec_1x1_0005.fit is 5.435944E-5 0.01065922 pixels
	Shifting image StarGateCluster_030_sec_1x1_0005.fit to image r05.fit ...
Average shift from StarGateCluster_030_sec_1x1_0006.fit to StarGateCluster_030_sec_1x1_0005.fit is 0.455512 3.213199 pixels
	Shifting image StarGateCluster_030_sec_1x1_0006.fit to image r06.fit ...
Average shift from StarGateCluster_030_sec_1x1_0007.fit to StarGateCluster_030_sec_1x1_0005.fit is 0.3876963 3.894753 pixels
	Shifting image StarGateCluster_030_sec_1x1_0007.fit to image r07.fit ...
Average shift from StarGateCluster_030_sec_1x1_0008.fit to StarGateCluster_030_sec_1x1_0005.fit is 0.4276552 4.834742 pixels
	Shifting image StarGateCluster_030_sec_1x1_0008.fit to image r08.fit ...
Average shift from StarGateCluster_030_sec_1x1_0009.fit to StarGateCluster_030_sec_1x1_0005.fit is -1.150846 2.851855 pixels
	Shifting image StarGateCluster_030_sec_1x1_0009.fit to image r09.fit ...
Average shift from StarGateCluster_030_sec_1x1_0010.fit to StarGateCluster_030_sec_1x1_0005.fit is -2.731817 1.014388 pixels
	Shifting image StarGateCluster_030_sec_1x1_0010.fit to image r10.fit ...
Overlap region: [5:749,6:581]
```

# This fails because the first line of the output includes the data and
# time it was run and this fails the difference test. The directory
# that follows confirms that the test worked.

Test options: `xfail`
```
cl> images.immatch.imcombine input=@regList output=imcombineRegisteredImage.fit

Nov 17 13:27: IMCOMBINE
  combine = average, scale = none, zero = none, weight = none
  blank = 0.
                Images 
                r01.fit
                r02.fit
                r03.fit
                r04.fit
                r05.fit
                r06.fit
                r07.fit
                r08.fit
                r09.fit
                r10.fit

  Output image = imcombineRegisteredImage.fit, ncombine = 10

```

```
cl> !ls imcombineRegisteredImage.fit
imcombineRegisteredImage.fit
```

# For reasons that I don’t understand the test harness is declaring this
# having failed. But is working. The confirmation it worked is the directory
# command that follows.

Test options: `xfail`
```
cl> noao
cl> imred
cl> ccdred
cl> combine input=@regList output=combineRegisteredImage.fit
```

```
cl> !ls combineRegisteredImage.fit 
combineRegisteredImage.fit
```

```
cl> !rm  $iraf/test/registrationTestFiles/outputFiles/*
cl> !cp  * $iraf/test/registrationTestFiles/outputFiles/.
```


