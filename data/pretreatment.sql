/* create table if not exists comments ( author char, subreddit char, created int, score int); */
delete from comments where author == "AutoModerator" or subreddit like "u\_%" escape '\';
delete from comments 
  where author in
    (select author from comments 
     group by author
     having count(distinct subreddit) < 3 or count(*) < 5)
  or subreddit in
    (select subreddit from comments
     group by subreddit
     having count(distinct author) < 10);
.mode csv
.output cleaned.csv
select * from comments;
.quit
