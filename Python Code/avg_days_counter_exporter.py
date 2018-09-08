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
			spliced = all_users[user]
			#print(all_users[user], (spliced))

			
			new_nums = [j-i for i, j in zip(spliced[:-1], spliced[1:])]
			#print(new_nums)
			all_avg = reduce(lambda x, y: x + y, new_nums) / len(new_nums)
			all_nums.append(all_avg.days)
		else:
			z = 0
			all_nums.append(z)



	#print len(all_users.keys())
	#print len(all_nums)
	newDF = pd.DataFrame() 
	newDF['users'] = all_users.keys()
	newDF['avg.days'] = all_nums
	print newDF
	newDF.to_csv('avg_visits.csv', sep='\t')


days_tracker()	
avg_days_calculator()