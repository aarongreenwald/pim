import sqlite from 'sqlite3';
import {resolvePath} from "../utils/utils";

const DATABASE_PATH = process.env.DATABASE_PATH;
if (!DATABASE_PATH) {
    throw 'Environment variable DATABASE_PATH is not set!'
}

sqlite.verbose();

/**
 * Helper function to promisify Database.all()
 * @param db
 * @param sql
 * @param params
 * @returns {Promise<unknown[]>}
 */
export function all<T>(db: sqlite.Database, sql: string, params = [] as any[]): Promise<T[]> {
    return new Promise(((resolve, reject) =>
            db.all(sql, params, (err, rows) => {
                if (err) reject(err)
                else resolve(rows as unknown as T[])
            })
    ))
}

/**
 * Helper function to promisify Database.run()
 * @param db
 * @param sql
 * @param params
 * @returns {Promise<{lastId, changes}>}
 */
export function run(db: sqlite.Database, sql: string, params: any[] = []): Promise<{lastId: any; changes: any}> {
    return new Promise((resolve, reject) => {
        db.run(sql, params, function (err) {
            if (err) reject(err)
            // @ts-ignore
            resolve({lastId: this.lastID, changes: this.changes})
        })
    })
}

/**
 * Helper function to promisify Database.get()
 * @param db
 * @param sql
 * @param params
 * @returns {Promise<T>}
 */
export function get<T>(db: sqlite.Database, sql: string, params = [] as any): Promise<T> {
    return new Promise((resolve, reject) => {
        db.get(sql, params, function(err, data: T) {
            if (err) reject(err)
            resolve(data)
        })
    })
}

/**
 * Setups a db connection
 * @param readonly = true
 * @returns {Promise<sqlite.Database>}
 */
export const getDb = async (readonly = true) => {
    const db = await new Promise<sqlite.Database>((resolve, reject) => {
        //sqlite3 creates a DB immediately but could throw in error if it fails
        //to find the file, or permission is denied, etc. So best to promisify
        //the whole thing
        const _db: sqlite.Database = new sqlite.Database(resolvePath(DATABASE_PATH),
            readonly ? sqlite.OPEN_READONLY : sqlite.OPEN_READWRITE,
            err => err ? reject(err): resolve(_db) //resolve the object already created 🤔
        );
    })
    if (!readonly) {
        await run(db, 'PRAGMA foreign_keys = ON;')
    }
    return db;
}

export const beginTransaction = (db) =>
    new Promise(resolve => db.exec('begin transaction', resolve));

export const commitTransaction = (db) =>
    new Promise(resolve => db.exec('commit transaction', resolve));

export const rollbackTransaction = (db) =>
    new Promise(resolve => db.exec('rollback transaction', resolve));




