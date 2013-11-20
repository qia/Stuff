#!/usr/bin/env python
import sys

#set up box for split
cur_Box = None
cur_Count = 0
box = None

#input boxes for count
for line in sys.stdin:
	tmp = line.split(',')
	
	#convert string to numbers
	try:
	  tmp = map(float, tmp)
	except:
	  raise

	box = tmp[0:4]
	count = tmp[4]

	#print tmp
	#print box
	#print count

	if cur_Box is None:
		cur_Box = box
		cur_Count = count
	else:
		if cur_Box[0] == box[0] and cur_Box[1] == box[1] and cur_Box[2] == box[2] and cur_Box[3] == box[3]:
			
			cur_Count = cur_Count + count
		else:
			print '%.1f,%.1f,%.1f,%.1f,%d'% (cur_Box[0], cur_Box[1]				 ,cur_Box[2],cur_Box[3],cur_Count)
			cur_Box = box
			cur_Count = count


if cur_Box[0] == box[0] and cur_Box[1] == box[1] and cur_Box[2] == box[2] and cur_Box[3] == box[3]:
	print '%.1f,%.1f,%.1f,%.1f,%d'% (cur_Box[0], cur_Box[1],cur_Box[2],cur_Box[3],cur_Count)



