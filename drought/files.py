import sys
import hashlib
import os
import mimetypes
import sqlite3

con = sqlite3.connect("/home/aaron/data/pim-data/pim.db")
BUF_SIZE = 65536  # 64kb chunks

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

def upload_required(sha256):
    cur = con.cursor()
    cur.execute("select 1 from file where sha256 = ? and storage_account_1 = 1", [sha256])
    return cur.fetchone() is None
 
def add_reference(filename, md5, sha1, sha256, accesed, modified, created, mimetype, size):
    cur = con.cursor()
    cur.execute("select 1 from file where sha256 = ?", [sha256])
    if (cur.fetchone() is None):
        cur.execute("""insert into file(sha1, sha256, bytes, accessed, created, mimetype) 
        values (?, ?, ?, ?, ?, ?)""", [sha1, sha256, size, accessed, created, mimetype])
        con.commit()
        
    # if the file needs to be created, do it, otherwise just
    # add the reference if it doesn't already exist in the same relative path with the same hash
    # if the same path with a different hash exists, suffix the new one (since I don't know which is newer) and log a warning
    return True

def upload_file():
    # upload file
    # verify hash correct
    # mark file uploaded in db
    # if anything fails here, mark the file not uploaded. 
    return True

def mime_type(filename):
    return mimetypes.guess_type(filename, False)
        
for line in sys.stdin:
    filename = line[:-1]
    stat = os.stat(filename)
    size = stat.st_size
    accessed = stat.st_atime
    modified = stat.st_mtime
    created = stat.st_ctime
    mimetype = mime_type(filename)[0]
  
    if (size == 0):
        continue
    md5, sha1, sha256 = hash_file(filename)

    print("Adding reference", sha1, sha256, size, accessed, created, mimetype)
    add_reference(filename, md5, sha1, sha256, accessed, modified, created, mimetype, size)
    print("Reference added")

    if (upload_required(sha256)):
        upload_file()
    

