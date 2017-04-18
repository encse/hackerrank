# coding=utf-8
import json
import os


#$.get('https://www.hackerrank.com/rest/contests/master/categories/data-structures%7Cdisjoint-set/challenges?offset=0&limit=1000&filters=&track_login=true')
#.then(res=>
#    Promise.all(
#    	res.models
#    		.filter(m => m.solved)
#    		.map(m => $.get("https://www.hackerrank.com/rest/contests/master/challenges/"+m.slug+"/submissions/?offset=0&limit=1")
#					   .then(r => $.get('https://www.hackerrank.com/rest/contests/master/submissions/'+r.models[0].id))
#					   .then(r => ({slug: r.model.slug, code: r.model.code, language: r.model.language}))
#					   )))
#.then(x=>x.then(r => console.log(JSON.stringify(r))))


file = open('solutions.txt', 'r') 
stuff = json.loads(file.read())
for idx in xrange(0, len(stuff)):
	d = str(idx)
	if not os.path.exists(d):
		os.mkdir(d)
	for solution in stuff[idx]:
		ext = '.py'
		if solution['language'] == 'scala':
			ext = '.scala'
		elif solution['language'] == 'java':
			ext = '.java'
		filn = d + '/' + solution['slug'] + ext
		with open(filn, 'w') as f:
			f.write(solution['code'])


