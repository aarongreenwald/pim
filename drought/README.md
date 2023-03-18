From directory you want to process:

`sh ~/data/code/drought/findfiles.sh | /usr/bin/time -v python3 ~/data/code/drought/files.py 1> ../files.log 2> ../files.err`


.env:

```
ENDPOINT = 'https://something.backblazeb2.com'
KEY_ID=''
APPLICATION_KEY=''
BUCKET_NAME=''
DB_PATH="/some/path/to/pim.db"
```

Useful commands:

```
pip3 install --upgrade b2
b2 authorize-account
b2 list-large-unfinished-files $bucketName
b2 cancel-all-unfinished-large-files $bucketName
```
