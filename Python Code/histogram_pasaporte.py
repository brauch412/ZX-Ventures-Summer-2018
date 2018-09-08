# CSV file
import pandas as pd
import matplotlib.pyplot as plt

csv_file = 'transactions.csv'
# read cvs with pandas read_csv
df = pd.read_csv(csv_file)
print df
all_users = {}

import datetime
from dateutil import parser
import numpy as np

def days_tracker():
	for index, row in df.iterrows():
		user = row['DNI de Usuario']
		date = row['Fecha y hora']
		date = parser.parse(date)

		if user not in all_users:
			all_users[user] = [date]

		if date not in all_users[user]:
			all_users[user].append(date)


	

def first_second_calculator():
	summer = []
	all_nums = []
	
	for user in all_users:

		if len(all_users[user]) != 1:
			second_date = all_users[user][1]
			first_date = all_users[user][0]
			diff = second_date - first_date
			
			
			if(diff.days > 1 and diff.days <= 7):
				#summer.append(first_date.weekday())
				summer.append(diff.days)

	print len(summer)



	newDF = pd.DataFrame() 


	#a = np.array(summer)
	#dev = np.std(a)
	#avg = np.average(a)


	#print "Pasaporte Average Number of Days Between First and Second Visit = " + str(avg)
	#print "Pasaporte Standard Deviation of Days Between First and Second Visit = " + str(dev)




	newDF['diff.days'] = summer
	#newDF['first.visit.day'] = summer


	print(newDF)

	his = newDF.hist(column='diff.days', bins=max(summer)-min(summer))
	#his = newDF.hist(column='first.visit.day', bins=max(summer)-min(summer)+1)

	plt.show()




days_tracker()	
first_second_calculator()


