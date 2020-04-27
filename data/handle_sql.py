#! /usr/bin/python3
import sys
import json
import sqlite3

conn = sqlite3.connect('comments.sqlite')
c = conn.cursor()

i = 0
for line in sys.stdin:
  i = i + 1
  json_obj = json.loads(line)
  if json_obj['author'] == '[deleted]':
    continue
  vals = [json_obj['created_utc'], json_obj['author'], json_obj['subreddit'], json_obj['score']]
  c.execute("INSERT INTO comments VALUES (?,?,?,?)", vals)
  if i % 1000000 == 0: conn.commit()

conn.commit()
conn.close()
