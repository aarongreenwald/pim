From directory you want to process:

`sh ~/data/code/drought/findfiles.sh | /usr/bin/time python3 ~/data/code/drought/files.py 2&1 > err.log`


.env:

```
ENDPOINT = 'https://something.backblazeb2.com'
KEY_ID=''
APPLICATION_KEY=''
BUCKET_NAME=''
DB_PATH="/some/path/to/pim.db"
```
