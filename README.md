# topten ablums

```shell
$ brew install sbcl

# copy .env.sample -> .env, update and source
# create a local/ranking.txt file, each row formatted like:
# PersonName | No. 10 Album - Artist Name | No. 9 Album - Artist Name | No. 8 Album - Artist Name | No. 7 Album - Artist Name | No. 6 Album - Artist Name | No. 5 Album - Artist Name | No. 4 Album - Artist Name | No. 3 Album - Artist Name | No. 2 Album - Artist Name | No. 1 Album - Artist Name 

$ RANKING_FILE=local/sample.txt make run
./build/bin/topten
 <INFO> [14:16:28] topten (run-main) - ranking from: local/sample.txt
===== RANK: ALBUMS =====
no. 1 album: 10
no. 2 album: 9
no. 3 album: 8
no. 4 album: 7
no. 5 album: 6
no. 6 album: 5
no. 7 album: 4
no. 8 album: 3
no. 9 album: 2
no. 10 album: 1

===== RANK: ARTISTS BY ALBUM SCORE =====
artist name: 55

===== RANK: ARTISTS BY ALBUM COUNT =====
artist name: 10

===== RANK PEOPLE BY ALBUM SCORE =====
PersonName: 55
```

