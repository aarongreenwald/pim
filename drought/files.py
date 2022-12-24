import sys
import hashlib
import os
import mimetypes
import sqlite3
from operator import itemgetter
import boto3  # REQUIRED! - Details here: https://pypi.org/project/boto3/
from botocore.exceptions import ClientError
from botocore.config import Config
from dotenv import load_dotenv  # Project Must install Python Package:  python-dotenv
import os
import sys

added_files = 0
added_filenames = 0
updated_filenames = 0
uploaded_files = 0

BUF_SIZE = 65536  # 64kb chunks


load_dotenv()

endpoint = os.getenv("ENDPOINT")
key_id = os.getenv("KEY_ID")
application_key = os.getenv("APPLICATION_KEY")
bucket_name = os.getenv("BUCKET_NAME")
db_path = os.getenv("DB_PATH")

con = sqlite3.connect(db_path, isolation_level=None) # isolation_level might not be necessary
con.execute('pragma journal_mode=wal') #impact is questionable, doesn't seem to affect benchmarks

def get_b2_client(endpoint, keyID, applicationKey):
    return boto3.client(service_name='s3', endpoint_url=endpoint, aws_access_key_id=keyID, aws_secret_access_key=applicationKey)

def get_b2_resource(endpoint, keyID, applicationKey):
    return boto3.resource(service_name='s3',
                        endpoint_url=endpoint,
                        aws_access_key_id=keyID,
                        aws_secret_access_key=applicationKey,
                        config = Config(signature_version='s3v4',)
                        )

b2 = get_b2_resource(endpoint, key_id, application_key)
b2_client = get_b2_client(endpoint, key_id, application_key)

def upload_file(sha256, file_path, mime_type):
    #todo upload the file_path in fileinfo, and verify the sha1
    #might need to switch away from s3 API to do this
    global uploaded_files
    try:
        print("Beginning upload of ", sha256, file_path)
        response = b2.Bucket(bucket_name).upload_file(file_path, sha256, ExtraArgs={'ContentType': mime_type, 'Metadata': {'sampleFilePath': file_path}}) #metadata isn't working?
        print("Uploaded file", response)
        cur = con.cursor()
        cur.execute("""update file set storage_account_1 = 1 where sha256 = ?""", [sha256])
        con.commit()
        uploaded_files+=1
    except Exception as ce:
        print('Upload failed', sha256, file_path, ce, file=sys.stderr)
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


def add_reference(filename, md5, sha1, sha256, accesed, modified, created, mimetype, size):
    global added_filenames, updated_filenames, added_files
    # TODO most of the wall time in this function is spent on inserts/updates to sqlite. 
    upload_required = True
    cur = con.cursor()
    cur.execute("select storage_account_1 from file where sha256 = ?", [sha256])
    file = cur.fetchone()
    if (file is None):
        cur.execute("""insert into file(sha1, sha256, bytes, accessed, created, mimetype) 
            values (?, ?, ?, ?, ?, ?)""", [sha1, sha256, size, accessed, created, mimetype])
        con.commit()
        added_files+=1
    else:
        upload_required = not file[0]

    cur.execute("select sha256 from filename where name = ?", [filename])
    existing_files = cur.fetchall()

    if (not existing_files):
        cur.execute("""insert into filename(name, created, sha256,  accessed) 
            values (?, ?, ?, ?)""", [filename, created, sha256, accessed])
        con.commit()
        added_filenames+=1
    elif (sha256 not in set(map(lambda x: x[0], existing_files))):
        print("Filename already exists with a different hash, filename=", filename, ", new_hash=", sha256, file=sys.stderr)
        cur.execute("""insert into filename(name, created, sha256,  accessed) 
            values (?, ?, ?, ?)""", [filename, created, sha256, accessed])
        con.commit()
        added_filenames+=1
    else: #sha256/filename already exists.
        # when the table is large, I think this costs more than the inserts
        cur.execute("""update filename set created = min(created, ?), accessed = max(accessed, ?) where sha256 = ? and name = ?""", [created, accessed, sha256, filename])
        con.commit()
        updated_filenames+=1
        
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
        accessed = stat.st_atime
        modified = stat.st_mtime
        created = stat.st_ctime
        mimetype = mime_type(filename)
        
        if (size == 0):
            continue
        md5, sha1, sha256 = hash_file(filename)

        print("Adding reference", filename, sha1, sha256, size, accessed, created, mimetype)
        upload_required = add_reference(filename, md5, sha1, sha256, accessed, modified, created, mimetype, size)

        if (upload_required):
            #todo make this not block!
            upload_file(sha256, filename, mimetype)

        counter+=1
    except Exception as ex:
        print("Failed to process filename=", line, file=sys.stderr)
        print(ex, file=sys.stderr)        


print("Files: ", counter, file=sys.stderr)
print("Added files: ", added_files, file=sys.stderr)
print("Uploaded: ", uploaded_files, file=sys.stderr)
print("Added filenames: ", added_filenames, file=sys.stderr)
print("Updated filenames: ", updated_filenames, file=sys.stderr)

