# CSV file
import pandas as pd
csv_file = 'export-13.csv'
# read cvs with pandas read_csv
df = pd.read_csv(csv_file)
print df
all_users = {}

import datetime
from dateutil import parser
import math
import numpy as np
import matplotlib.pyplot as plt



def days_tracker():

	all_days = []
	for index, row in df.iterrows():
		first = pd.to_datetime(row['Joined'], errors='coerce')
		last =  pd.to_datetime(row['Last Visit'], errors='coerce')
		name = row['Email']
		visits = int(row['Visits'])
		
		if first != last and visits < 70 and visits > 0:
		
			diff = last - first
			x=float(diff.days)
			if not math.isnan(x):
				temp_days = diff.days/visits
				if (temp_days) > 1:
					all_days.append(temp_days)

	#print(all_days)
	a = np.array(all_days)
	dev = np.std(a)
	avg = np.average(a)

	print "Zenreach Average Number of Days Between First and Second Visit = " + str(avg)
	print "Zenreach Standard Deviation of Days Between First and Second Visit = " + str(dev)

	newDF = pd.DataFrame() 
	newDF['diff.days'] = all_days
	#newDF['first.visit.day'] = summer


	#print(newDF)

	his = newDF.hist(column='diff.days', bins=max(all_days)-min(all_days))
	#his = newDF.hist(column='first.visit.day', bins=max(summer)-min(summer)+1)

	plt.show()



days_tracker()	



