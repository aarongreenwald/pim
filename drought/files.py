import sys
import hashlib
import os
import mimetypes
import sqlite3
from operator import itemgetter
import boto3  # https://pypi.org/project/boto3/
from botocore.exceptions import ClientError
from botocore.config import Config
from dotenv import load_dotenv
from b2sdk.v2 import *
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

def get_b2_sdk_client():    
    info = InMemoryAccountInfo()
    b2_api = B2Api(info)
    b2_api.authorize_account("production", key_id, application_key)
    return b2_api


def get_b2_client(endpoint, keyID, applicationKey):
    return boto3.client(
        service_name='s3',
        endpoint_url=endpoint,
        aws_access_key_id=keyID,
        aws_secret_access_key=applicationKey)

def get_b2_resource(endpoint, keyID, applicationKey):
    return boto3.resource(service_name='s3',
                        endpoint_url=endpoint,
                        aws_access_key_id=keyID,
                        aws_secret_access_key=applicationKey,
                        config = Config(signature_version='s3v4',)
                        )

b2 = get_b2_resource(endpoint, key_id, application_key)
b2_client = get_b2_client(endpoint, key_id, application_key)
bucket = get_b2_sdk_client().get_bucket_by_name(bucket_name)

def upload_file(sha256, file_path, mime_type):
    #TODO does sha1 verification happen automatically?
    #Consider uploading more metadata
    global uploaded_files
    try:
        print("Beginning upload of ", sha256, file_path)               
        response = b2.Bucket(bucket_name).upload_file(
            file_path,
            sha256,
            ExtraArgs={
                'ContentType': mime_type,
                'Metadata': {'sample_filename': file_path},
                'ChecksumAlgorithm': 'SHA1'
            }) 
        print("Uploaded file", response)
        cur = con.cursor()
        cur.execute("""update file set storage_account_1 = 1 where sha256 = ?""", [sha256])
        con.commit()
        uploaded_files+=1
    except Exception as ce:
        print('Error: Upload failed', sha256, file_path, ce, file=sys.stderr)
        raise ce

def upload_b2(sha256, sha1, file_path, mime_type, size, md5):
    """
    Upload file with the b2 sdk - it allows passing a precomputed sha1 as well as verifying the size afterwards,
    both of which are apparently not possible with boto. 
    """
    global uploaded_files
    try:
        print("Beginning upload of ", sha256, file_path, mime_type, size)
        response = bucket.upload_local_file(
            file_path,
            file_name = sha256,
            content_type = mime_type,
            file_infos = {            
                'sample_filename': file_path
            },
            sha1_sum=sha1,
            upload_mode = UploadMode.INCREMENTAL
            # progress_listener=
        )

        if (response.size != size):
            raise Exception("File size expected was " + str(size) + " but upload response contains size=" + str(response.size))

        # for some reason the only hash they give me back is the md5, may as well check it,
        # it's redundant if I trust that they are actually verifying the sha1 I send them but why trust if you don't have to?
        if (response.content_md5 != md5):
            raise Exception("File md5 was " + md5 + " but upload response contains md5=" + response.content_md5)
        
        print("Uploaded file successfully")
        cur = con.cursor()
        cur.execute("""update file set storage_account_1 = 1 where sha256 = ?""", [sha256])
        con.commit()
        print("Updated db")
        uploaded_files+=1
    except Exception as ce:
        print('Error: Upload failed', sha256, sha1, file_path, ce, file=sys.stderr)
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
        print("Warning: Filename already exists with a different hash, filename=", filename, ", new_hash=", sha256, file=sys.stderr)
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
            raise Exception("File size is 0")
        md5, sha1, sha256 = hash_file(filename)

        print("Adding reference", filename, sha1, sha256, size, accessed, created, mimetype)
        upload_required = add_reference(filename, md5, sha1, sha256, accessed, modified, created, mimetype, size)

        if (upload_required):
            #todo make this not block!
            # upload_file(sha256, filename, mimetype)
            upload_b2(sha256, sha1, filename, mimetype, size, md5)
            
    except Exception as ex:
        print("Error: Failed to process filename=", line, file=sys.stderr)
        print(ex, file=sys.stderr)
        print("\n-----------------\n", file=sys.stderr)
    finally:
        counter+=1
        print("\n-----------------\n")


print("Files processed: ", counter, file=sys.stdout)
print("Added files: ", added_files, file=sys.stdout)
print("Uploaded: ", uploaded_files, file=sys.stdout)
print("Added filenames: ", added_filenames, file=sys.stdout)
print("Updated filenames: ", updated_filenames, file=sys.stdout)

