from urllib2 import urlopen
from datetime import datetime, timedelta
from sys import argv
from collections import OrderedDict

year=int(argv[1])

with open(`year` + 'Temp.csv', 'w+') as csv:
	runner = datetime(year,01,01)
	anchor = datetime(year,01,03)
	while (runner < anchor):
		m = OrderedDict()
		print 'downloading data for '+`runner.year`+'/'+`runner.month`+'/'+`runner.day`
		rows = urlopen('http://www.wunderground.com/history/airport/KBOS/'+`runner.year`+'/'+`runner.month`+'/'+`runner.day`+'/DailyHistory.html?format=1').read().split('<br />\n')
		#rows = [','.join([`runner.year`,`runner.month`,`runner.day`] + row.split(',')[0:2]) for row in rows[1:len(rows)-1]]
		#for row in rows:
		#	row.split(':')[0].split(',')
		#print rows
		
		#[m.append([`runner.year`,`runner.month`,`runner.day`] + row.split(',')[:2]) for row in rows[1:len(rows)-1]]
		#for n in m:
		#	n[3] = n[3].split(':')[0]
		#runner += timedelta(days=1)

		rows = rows[1:-1]
		cells = [row.split(',') for row in rows]
		for cell in cells:
			m[ datetime.strptime(runner.strftime("%Y-%m-%d") + ' ' + cell[0], '%Y-%m-%d %I:%M %p').strftime('%Y-%m-%d %H')] = cell[1]
		runner += timedelta(days=1)
	
		for n in m:
			csv.write('\n'.join([n + ',' + m[n]] + ['']))
