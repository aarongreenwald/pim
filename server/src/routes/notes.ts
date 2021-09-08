import {Express} from 'express';
import fs from 'fs';
import {exec} from 'child_process';
import {resolvePath} from '../utils/utils';
import {NotesPathDto, DirectoryItem, Breadcrumb, FileSystemItemType} from "@pim/common";
import path from 'path';
import bodyParser from 'body-parser';
const uuid = require('uuid')
const textParser = bodyParser.text();

const NOTES_PATH = process.env.NOTES_PATH;
if (!NOTES_PATH) {
    throw 'Environment variable NOTES_PATH is not set!'
}


export const setupNotesRoutes = (app: Express) => {

    app.get('/notes/path', async (req, res) => {
        const response = await getPath(req.query.path as string);
        res.send(response)
    })

    app.get('/notes/download', async (req, res) => {
        const {path, fullPath, isDirectory} = await getPathDetails(req.query.path as string);

        if (isDirectory) {
            zipAndReturn(res, fullPath, path)
        } else {
            res.setHeader('content-type','application/file');
            fs.createReadStream(fullPath).pipe(res);
        }
    })

    app.get('/notes/viewfile', async (req, res) => {
        const {path, fullPath, isDirectory} = await getPathDetails(req.query.path as string);

        if (isDirectory) {
            res.status(500).send('Path is a directory')
        } else {
            res.set('Content-Security-Policy', "default-src 'self'");
            res.setHeader('Content-Type', mimeType(path));
            res.setHeader('filename', filename(path))
            res.sendFile(fullPath)
        }
    })

    app.put('/notes/files', textParser, async (req, res) => {
        const fullPath = getFullPath(req.query.path as string);

        try {
            await fs.promises.writeFile(fullPath, req.body);
            const result = await getPath(req.query.path as string)
            res.send(result)
        } catch (ex) {
            res.status(500).send(ex)
        }
    })

    app.post('/notes/path', async (req, res) => {
        const fullPath = getFullPath(req.query.path as string);
        const itemType = req.query.type as FileSystemItemType;
        const name = req.query.name as string;
        try {
            if (itemType === 'F') {
                await fs.promises.writeFile(path.join(fullPath, name), '', {flag: 'wx'})
            } else {
                await fs.promises.mkdir(path.join(fullPath, name), {recursive: true})
            }
            // const result = await getPath(path.join(req.query.path as string, name))
            // res.send(result)
            res.send(200)
        } catch (ex) {
            if (ex.code === 'EEXIST') {
                res.status(500).send(`Path ${req.query.path} already exists`)
            }
            res.status(500).send(ex)
        }

    })

}

const getPath: (relativePath: string) => Promise<NotesPathDto> = async (relativePath) => {
    const {path, fullPath, isDirectory, directoryPath} = await getPathDetails(relativePath);
    const directoryInfo = await getDirectoryInfo(directoryPath)
    const fileContent = isDirectory || isBinary(path as string) ? null : await fs.promises.readFile(fullPath, 'utf8');

    const response: NotesPathDto = {
        isDirectory,
        path: cleanPath(path),
        breadcrumbs: buildBreadcrumbs(path),
        directoryInfo,
        fileContent,
        isPlainText: !isDirectory && mimeType(path) === 'text/plain'
    };
    return response;
}

const getDirectoryInfo: (path: string) => Promise<DirectoryItem[]> = async (path: string) => {
    try {
        const contents = await fs.promises.readdir(path)
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

async function getPathDetails<P, ResBody, ReqBody, ReqQuery>(path: string) {
    const fullPath = `${CONTENT_DIRECTORY}/${replaceSpaces(`./${path}`)}`
    const data = await fs.promises.stat(fullPath);
    const isRoot = !data
    const isDirectory = isRoot || data.isDirectory();
    const directoryPath = isDirectory ? fullPath : fullPath.split('/').slice(0, -1).join('/');
    return {path, fullPath, isDirectory, directoryPath};
}

const getFullPath = (path: string) => `${CONTENT_DIRECTORY}/${replaceSpaces(`./${path}`)}`


const isBinary = (path: string) => mimeType(path) !== 'text/plain';

const CONTENT_DIRECTORY = resolvePath(process.env.NOTES_PATH)

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

const buildBreadcrumbs: (path: string) => Breadcrumb[] = path => {
    path = cleanPath(path)

    return path.split('/').reduce((acc, name, i) => {
        acc.push({
            name,
            path: '/' + path.split('/').splice(0, i + 1).filter(x => x).join('/')
        })
        return acc
    }, [{
        name: 'root',
        path: '.'
    }] as Breadcrumb[])
}

const getStats = async (name, dir) => {
    const stats = await fs.promises.stat(`${dir}/${name}`)
    return {
        isDirectory: stats && stats.isDirectory(),
        name,
        isPlainText: mimeType(name) === 'text/plain',
        openInBrowser: openInBrowser(name)
    }
}

fs.mkdir('./temporary_files', err => {});