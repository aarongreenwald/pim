import {Express} from "express";
import helmet, {contentSecurityPolicy} from "helmet";
const fs = require('fs');
const {exec} = require('child_process')
const uuid = require('uuid')

fs.mkdir('./temporary_files', err => {});

export const setupFileserverRoutes = (app: Express) => {

    app.get('/notes/path', async (req, res) => {
        const {path} = req.query;
        const fullPath = `${CONTENT_DIRECTORY}/${replaceSpaces(`./${path}`)}`
        const data: any = await stat(fullPath);
        const isRoot = !data
        const isDirectory = isRoot || data.isDirectory();
        const directoryPath = isDirectory ? fullPath : fullPath.split('/').slice(0, -1).join('/');
        const directoryInfo = await getDirectoryInfo(directoryPath)
        const fileContent = isDirectory || isBinary(path as string) ? null : await fs.promises.readFile(fullPath, 'utf8');

        res.send({
            isDirectory,
            path: cleanPath(path),
            breadcrumbs: buildBreadcrumbs(path),
            directoryInfo,
            fileContent,
            isPlainText: !isDirectory && mimeType(path) === 'text/plain'
        })
    })

    app.get('/notes/download', async (req, res) => {
        const {path} = req.query;
        const fullPath = `${CONTENT_DIRECTORY}/${replaceSpaces(`./${path}`)}`
        const data: any = await stat(fullPath);
        const isRoot = !data
        const isDirectory = isRoot || data.isDirectory();

        if (isDirectory) {
            zipAndReturn(res, fullPath, path)
        } else {
            res.setHeader("content-type",'application/file');
            fs.createReadStream(fullPath).pipe(res);
        }
    })

    app.get('/notes/viewfile', async (req, res) => {
        const {path} = req.query;
        const fullPath = `${CONTENT_DIRECTORY}/${replaceSpaces(`./${path}`)}`
        const data: any = await stat(fullPath);
        const isRoot = !data
        const isDirectory = isRoot || data.isDirectory();

        if (isDirectory) {
            res.status(500).send('Path is a directory')
        } else {
            res.set('Content-Security-Policy', "default-src 'self'");
            res.setHeader('Content-Type', mimeType(path));
            res.setHeader('filename', filename(path))
            res.sendFile(fullPath)
        }
    })

    const getDirectoryInfo = async (path: string) => {
        try {
            const contents: any = await readdir(path)
            const promises = contents.map(name => getStats(name, path))
            return await Promise.all(promises);
        } catch (ex) {
            if (ex.code === 'ENOENT') {
                throw `404 Directory ${path} not found.`
            } else {
                throw `500 ${ex}`
            }
        }
    }
}

const isBinary = (path: string) => mimeType(path) !== 'text/plain';

const CONTENT_DIRECTORY = process.env.CONTENT_DIRECTORY || './'

const stat = path => new Promise((resolve, reject) => {
    fs.stat(path, (err, result) => {
        if (err) reject(err)

        resolve(result)
    })
})

const filename = (path) => path.split('/').pop()

const openInBrowser = (path) => mimeType(path) === 'application/pdf';

const mimeType = path => {
    const filename = path.split('/').pop()
    const extension = filename.split('.').pop()

    switch(extension) {
        case 'doc':
        case 'docx':
        case 'xls':
        case 'xlsx':
            return ''
        case 'pdf':
            return 'application/pdf';
        default:
            return 'text/plain';
    }
};

const replaceSpaces = path => path.split('%20').join('\ ');

const cleanPath = path => path.split('/').filter(x => x).join('/');

const zipAndReturn = (res, fullPath, path) => {
    path = cleanPath(path)
    const name = path.split('/').pop()
    const id = uuid()
    //TODO don't include the full path - cd to the directory first
    exec(`tar -cf temporary_files/${id}.tar.gz ${fullPath}`, (err, stdout, stderr) => {
        res.setHeader("Content-Disposition",  `attachment; filename=${name}.tar.gz`)
        fs.createReadStream(`temporary_files/${id}.tar.gz`).pipe(res);
    })
}

const readdir = dir => new Promise((resolve, reject) => {
    fs.readdir(dir, (err, result) => {
        if (err) reject(err)

        resolve(result)
    })
})

const buildBreadcrumbs = path => {
    path = cleanPath(path)

    return path.split('/').reduce((acc, name, i) => {
        acc.push({
            name,
            path: '/' + path.split('/').splice(0, i + 1).filter(x => x).join('/')
        })
        return acc
    }, [])
}

const getStats = (name, dir) => new Promise((resolve, reject) => {
    fs.stat(`${dir}/${name}`, (err, stats) => {
        if (err) reject(err)

        resolve({
            isDirectory: stats && stats.isDirectory(),
            name,
            isPlainText: mimeType(name) === 'text/plain',
            openInBrowser: openInBrowser(name)
        })
    })
})
