# CSV file
import pandas as pd
csv_file = 'transactions.csv'
# read cvs with pandas read_csv
df = pd.read_csv(csv_file)
consecutive = 0
ids = 1
order_id = []
order_line = []
all_users = []
for col in df:
	if col == "DNI de Usuario":
		users = (df[col])
		for user in users:
			all_users.append(user)

for i in range(len(all_users)-1):
	consecutive += 1
	if all_users[i] != all_users[i+1]:
		for j in range(consecutive):
			order_id.append(ids)
			order_line.append(j+1)
		ids+=1
		consecutive = 0

order_id.append(1)
order_line.append(1)

df['order.id'] = order_id

df['order_line'] = order_line

df.to_csv('transactions_updated.csv')


print(len(order_id))

print(len(order_line))

print(len(all_users))
				


