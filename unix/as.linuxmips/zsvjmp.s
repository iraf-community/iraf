	.file	"zsvjmp.s"

# Copyright (c) 2014 David Kuehling <dvdkhlng AT posteo TOD de>
# Distributable under the same license as IRAF
# This file contains the Linux mipsel version of ZSVJMP for Debian.

	.set mips1
	.abicalls
	.text
	.global	zsvjmp_
	.ent zsvjmp_
	.type	zsvjmp_, %function

zsvjmp_:
	.set noreorder
	.cpload $t9
	.set reorder
	sw  $a1, 0($a0)		# buf[0]=status
	sw  $zero, 0($a1)	# *status=0
	addiu  $a0, $a0, 4	# &buf[1] --> 1st arg for sigsetjmp
	move    $a1, $zero	# 2nd arg is zero

	# this call sequence is required when used inside shared library
	la $t9, __sigsetjmp
	j $t9
	##  note: no delay slot, filled by GAS

	.end 	zsvjmp_
