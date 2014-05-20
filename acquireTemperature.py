from urllib2 import urlopen
from datetime import datetime, timedelta

with open('acquireTemperature.csv', 'w+') as csv:
	runner = datetime(2011,01,30)
	anchor = datetime(2011,02,02)
	m = []
	while (runner < anchor):
		print 'downloading data for '+`runner.year`+'/'+`runner.month`+'/'+`runner.day`
		rows = urlopen('http://www.wunderground.com/history/airport/KBOS/'+`runner.year`+'/'+`runner.month`+'/'+`runner.day`+'/DailyHistory.html?format=1').read().split('<br />\n')
		#rows = [','.join([`runner.year`,`runner.month`,`runner.day`] + row.split(',')[0:2]) for row in rows[1:len(rows)-1]]
		#for row in rows:
		#	row.split(':')[0].split(',')
		#print rows
		[m.append([`runner.year`,`runner.month`,`runner.day`] + row.split(',')[:2]) for row in rows[1:len(rows)-1]]
		for n in m:
			n[3] = n[3].split(':')[0]
		runner += timedelta(days=1)
	csv.write('\n'.join([','.join(n) for n in m] + ['']))
