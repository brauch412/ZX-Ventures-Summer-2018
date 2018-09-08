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
			all_users[user] = []
		if date not in all_users[user]:
			all_users[user].append(date)


	#print all_users
	

def avg_days_calculator():
	summer = []
	all_nums = []
	
	for user in all_users:

		if len(all_users[user]) > 1:
			second_date = all_users[user][1]
			first_date = all_users[user][0]
			diff = second_date - first_date
			#print second_date, first_date

			#print diff.days
			if(diff.days > 1 and diff.days < 70):
				summer.append(diff.days)

	for user in all_users:
		if len(all_users[user]) > 1:
			all_users[user]
			spliced = all_users[user][1:]
			#print(all_users[user], (spliced))

			if len(spliced) > 1:
				new_nums = [j-i for i, j in zip(spliced[:-1], spliced[1:])]
				#print(new_nums)
				all_avg = reduce(lambda x, y: x + y, new_nums) / len(new_nums)
				all_nums.append(all_avg.days)



	newDF = pd.DataFrame() 
	print all_nums

	#a = np.array(summer)
	#dev = np.std(a)
	#avg = np.average(a)


	#print "Pasaporte Average Number of Days Between First and Second Visit = " + str(avg)
	#print "Pasaporte Standard Deviation of Days Between First and Second Visit = " + str(dev)




	



	a = np.array(summer)
	dev = np.std(a)
	avg = np.average(a)


	print "Pasaporte Average Number of Days Between First and Second Visit = " + str(avg)
	print "Pasaporte Standard Deviation of Days Between First and Second Visit = " + str(dev)


	b = np.array(all_nums)
	bdev = np.std(b)
	bavg = np.average(b)


	print "Pasaporte Average Number of Days Between Visits After First = " + str(bavg)
	print "Pasaporte Standard Deviation of Days Between Visits After First = " + str(bdev)		


	newDF['diff.days'] = summer
	#newDF['first.visit.day'] = summer


	#print(newDF)

	his = newDF.hist(column='diff.days', bins=max(summer)-min(summer))
	#his = newDF.hist(column='first.visit.day', bins=max(summer)-min(summer)+1)

	#plt.show()


days_tracker()	
avg_days_calculator()


