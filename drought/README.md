From directory you want to process:

`sh ~/data/code/drought/findfiles.sh | /usr/bin/time -v python3 ~/data/code/drought/files.py`

Complete logs are in `../files.log`. Consider moving before overwriting. 


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

### TODO

* Connect to the remote PIM db so a copy isn't required for this. Latency will be an issue so consider this carefully.
* Decide how to handle git/svn repos. 
   * Review skipped repos
   * Log/document structure of the zipped repos
* Deduping repos
* Cleaning files
* Build frontend
* mimetypes might be able to be improved. 
* Find a better method to upload large files so that I can validate the hashes. Perhaps the lower level API will allow this?
* Performance:
  * For files that need to be uploaded, obviously the upload dominates and I'm limited by bandwidth, so parallelization isn't worth much
  * For subsequent runs, the db updates are expensive so it's important to avoid unnecessary updates. 
   * After db updates, it seems the primary cost is hashing large files. I could limit to only sha256. But it's a relatively small cost
   * How much is the logging costing in perf? Not much, but measure it. 
   * I'm not sure how db inserts compare to updates, but the reads seem very fast. Still, I could load the entiredb into memory at the start and not 
hit the disk again unless there's a change. 
   * The `find` is trivial. 
* Dry run mode - document required updates and exit.


Notes

* Small files have a sha1 in backblaze, large files don't but they have a large_file_sha1 field in the fileinfo dict
