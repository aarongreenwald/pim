import sys
import hashlib
import os
import mimetypes
import sqlite3
import datetime
import time
from operator import itemgetter
from dotenv import load_dotenv
from b2sdk.v2 import *

DRY_RUN = False
BUF_SIZE = 65536  # 64kb chunks
VERBOSE = True
LOGS_PATH = "../files.log"

added_files = 0
added_filenames = 0
updated_filenames = 0
uploaded_files = 0
uploaded_bytes = 0
empty_files = 0


load_dotenv()
endpoint = os.getenv("ENDPOINT")
key_id = os.getenv("KEY_ID")
application_key = os.getenv("APPLICATION_KEY")
bucket_name = os.getenv("BUCKET_NAME")
db_path = os.getenv("DB_PATH")

con = sqlite3.connect(db_path, isolation_level=None) # isolation_level might not be necessary
# con.execute('pragma journal_mode=wal') #impact is questionable, doesn't seem to affect benchmarks, and it makes transferring the db harder

full_log_file = open(LOGS_PATH, "a")


def log(*args):
    print(datetime.datetime.now(), " ", *args, file=full_log_file, flush=True)
    if (VERBOSE):
        print(*args, file=sys.stdout)

def log_err(*args):
    print(datetime.datetime.now(), " ERROR: ", *args, file=full_log_file, flush=True)
    print(*args, file=sys.stderr)

def get_b2_sdk_client():    
    info = InMemoryAccountInfo()
    b2_api = B2Api(info)
    b2_api.authorize_account("production", key_id, application_key)
    return b2_api

bucket = get_b2_sdk_client().get_bucket_by_name(bucket_name)

def upload_b2(sha256, sha1, file_path, mime_type, size, md5):
    """
    Upload file with the b2 sdk - it allows passing a precomputed sha1 as well as verifying the size afterwards,
    both of which are apparently not possible with boto. 
    """
    global uploaded_files, uploaded_bytes
    try:
        log("Beginning upload of ", sha256, file_path, mime_type, size)
        response = bucket.upload_local_file(
            file_path,
            file_name = sha256,
            content_type = mime_type,
            file_infos = {            
                'sample_filename': file_path
            },
            # this is stored as the sha1 and validated on all uploads except those that are considered "large",
            # apparently over 100mb. For large files, it's stored in file_infos.large_file_sha1, and not validated. 
            sha1_sum=sha1,
            progress_listener=TqdmProgressListener(sha256) 
        )

        if (response.size != size):
            raise Exception("File size expected was " + str(size) + " but upload response contains size=" + str(response.size))

        # for some reason the only hash they give me back is the md5, may as well check it just in case behavior on their side
        # changes and they don't verify the sha1 I send them. Unfortunately it's not available on large files. 
        if (response.content_md5 != md5 and response.size < 100e6):
            log_err(response)
            raise Exception("File md5 was " + str(md5) + " but upload response contains md5=" + str(response.content_md5))
        
        log("Uploaded file successfully")
        cur = con.cursor()
        cur.execute("""update file set storage_account_1 = 1 where sha256 = ?""", [sha256])
        con.commit()
        log("Updated db")
        uploaded_files+=1
        uploaded_bytes+=size
    except Exception as ce:
        log_err('Error: Upload failed', sha256, sha1, file_path, ce)
        raise ce

def hash_file(file):
    md5 = hashlib.md5()
    sha1 = hashlib.sha1()
    sha256 = hashlib.sha256()
    with open(file, 'rb') as f:
        while True:
            data = f.read(BUF_SIZE)
            if not data:
                break
            md5.update(data)
            sha1.update(data)
            sha256.update(data)
        return md5.hexdigest(), sha1.hexdigest(), sha256.hexdigest()


def add_reference(filename, md5, sha1, sha256, created, mimetype, size):
    global added_filenames, updated_filenames, added_files
    # TODO most of the wall time in this function is spent on inserts/updates to sqlite. 
    upload_required = True
    cur = con.cursor()
    cur.execute("select storage_account_1 from file where sha256 = ?", [sha256])
    file = cur.fetchone()
    if (file is None):
        log("Adding file to db")
        cur.execute("""insert into file(sha1, sha256, bytes, created, mimetype, accessed) 
            values (?, ?, ?, ?, ?, ?)""", [sha1, sha256, size, created, mimetype, time.time()])
        con.commit()
        added_files+=1
    else:
        # it's possible the mimetype differs but it's a heuristic anyway
        # TODO it's also possible the mtime is different.
        # Since the "created" should represent the first time a file with the given sha256 existed,
        # we should update the created to the first seen mtime (birth time can't be used, because we can't
        # be sure the hash was the same). In general, newer mtimes usually happen
        # when metadata is lost while copying.
        # In any case, this timestamp is for context only so it's not critical. 
        upload_required = not file[0]
        log("File already in db, upload_required=", upload_required)

    cur.execute("select sha256, created from filename where name = ?", [filename])
    existing_files = cur.fetchall()

    if (not existing_files):
        log("Adding filename to db")
        cur.execute("""insert into filename(name, created, sha256,  accessed) 
            values (?, ?, ?, ?)""", [filename, created, sha256, time.time()])
        con.commit()
        added_filenames+=1
    elif (sha256 not in set(map(lambda x: x[0], existing_files))):
        log_err("Warning: Filename already exists with a different hash, filename=", filename, ", new_hash=", sha256)
        log("Adding filename to db (new version of already existing filename)")
        # Note: the created time is used to decide which of two hashes is the newer version of a path.
        # But sometimes the created is just the time the file was copied from somewhere, and this insertion _could_
        # mask a file that's actually newer. Hard to prevent, though. 
        cur.execute("""insert into filename(name, created, sha256,  created, accessed) 
            values (?, ?, ?, ?, ?)""", [filename, created, sha256, created, time.time()])
        con.commit()
        added_filenames+=1
    else:
        # sha256/filename already exists.
        # when the table is large, this seems to cost more than the inserts and definitely more than the reads
        # to save unnecessary updates, only update the filename reference if one of the dates needs to be updated
        filename_record = next((x for x in existing_files if x[0] == sha256), None)
        if (filename_record is None):
            raise Exception("That isn't supposed to happen")
        existing_created = filename_record[1]
        if (created < existing_created):
            # the same path+hash was seen with an earlier mtime. Set the created time of the file version to the earlier timestamp
            log("Updating existing filename reference with new created timestamp", created, ". Previously: ", existing_created)
            cur.execute("""update filename set created = min(created, ?), accessed = ? where sha256 = ? and name = ?""", [created, time.time(), sha256, filename])
            con.commit()
            updated_filenames+=1
        else:
            log("References are up to date, nothing to do")
        
    return upload_required


def mime_type(filename):
    # todo improve heuristic
    mimetype = mimetypes.guess_type(filename, False)[0]
    return 'text/plain' if mimetype is None else mimetype

counter = 0
for line in sys.stdin:
    try:
        filename = line[:-1]
        stat = os.stat(filename)
        size = stat.st_size
        created = stat.st_mtime # files are considered immutable, so the modified is considered "creation time"
        mimetype = mime_type(filename)

        if (size == 0):
            # empty files are sometimes meaningful in a file structure (eg .stignore, .gitkeep, etc)
            # but also often are corrupted files. It's relatively harmless to include them (since they are represented by a single 0 byte
            # file in storage and a lot of references) and decide what to do with them later.
            log_err("Warning: file is empty. Filename: ", filename)
            empty_files += 1
        
        md5, sha1, sha256 = hash_file(filename)

        log("Processing file: ", filename, sha1, sha256, size, created, mimetype)
        upload_required = add_reference(filename, md5, sha1, sha256, created, mimetype, size)

        if (upload_required and not DRY_RUN):
            upload_b2(sha256, sha1, filename, mimetype, size, md5)
            
    except Exception as ex:
        log_err("Error: Failed to process filename=", line)
        log_err(ex)
        log_err("\n-----------------\n")
    finally:
        counter+=1
        log("\n-----------------\n")


log("Files processed: ", counter)
log("Added files: ", added_files)
log("Uploaded: ", uploaded_files)
log("Uploaded bytes: ", uploaded_bytes)
log("Added filenames: ", added_filenames)
log("Updated filenames: ", updated_filenames)
log("Empty files: ", empty_files)

full_log_file.close()
