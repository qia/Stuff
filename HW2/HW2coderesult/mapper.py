#!/usr/bin/env python
import sys

#get input from STDIN
for line in sys.stdin:
	
	coords = line.split()
	#transform string to float number
	coords[0] = float(coords[0])
	coords[1] = float(coords[1])

	#trancate x coordinate into bin precision
	try:
		tmpx = (int(coords[0] / 0.1)) * 0.1
	except ValueError:
		print "%s is not a valid number.", coords[0]
		continue	

	#set up x bin
	if tmpx >= 0:
		x_lo, x_hi = tmpx, tmpx + 0.1
	else:
		x_lo, x_hi = tmpx - 0.1, tmpx
	
	#trancate y coordinate into bin precision
	try:
		tmpy = (int(coords[1] / 0.1)) * 0.1
	except ValueError:
		print "%s is not a valid number.", coords[1]
                continue   
  
	#set up y bin
	if tmpy >=0:
		y_lo, y_hi = tmpy, tmpy + 0.1
	else:
		y_lo, y_hi = tmpy - 0.1, tmpy
	
	print '%.1f,%.1f,%.1f,%.1f,%d' % (x_lo,x_hi,y_lo,y_hi,1)

