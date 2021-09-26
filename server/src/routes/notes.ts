import {Express} from 'express';
import fs from 'fs';
import {exec} from 'child_process';
import {resolvePath} from '../utils/utils';
import {FileSystemItem, DirectoryItem, Breadcrumb, FileSystemItemType, Directory, File, GitStatus} from "@pim/common";
import path from 'path';
import bodyParser from 'body-parser';
const uuid = require('uuid')
const textParser = bodyParser.text();
import simpleGit, {SimpleGit, StatusResult} from 'simple-git';
import util from "util";
const execp = util.promisify(exec);

const NOTES_PATH = process.env.NOTES_PATH;
if (!NOTES_PATH) {
    throw 'Environment variable NOTES_PATH is not set!'
}
const CONTENT_DIRECTORY = resolvePath(process.env.NOTES_PATH)
const git: SimpleGit = simpleGit(CONTENT_DIRECTORY);

export const setupNotesRoutes = (app: Express) => {

    app.get('/notes/path', async (req, res) => {
        try {
            const response = await getPath(req.query.path as string);
            res.send(response)
        } catch (ex) {
            console.error(ex)
            if (ex.code === 'ENOENT') {
                res.status(404).send(ex)
            } else {
                res.status(500).send(ex)
            }
        }

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
            console.error(ex)
            res.status(500).send(ex)
        }
    })

    app.post('/notes/path', async (req, res) => {
        const fullPath = getFullPath(req.query.path as string);
        const itemType = req.query.type as FileSystemItemType;
        const name = req.query.name as string;
        const createdPath = path.join(fullPath, name);
        try {
            if (itemType === 'F') {
                await fs.promises.writeFile(createdPath, '', {flag: 'wx'})
            } else {
                await fs.promises.mkdir(createdPath, {recursive: true})
            }
            // const result = await getPath(path.join(req.query.path as string, name))
            // res.send(result)
            res.send(200)
        } catch (ex) {
            if (ex.code === 'EEXIST') {
                console.warn(ex)
                res.status(500).send(`Path ${createdPath} already exists`)
            }
            console.error(ex)
            res.status(500).send(ex)
        }

    })

    app.put('/notes/pull', async (req, res) => {
        //This shouldn't really be necessary ever, but until the system is stable it's a good stop-gap
        try {
            await gitPullRebase();
            //no need to fetch first, most likely even if the pull failed the fetch part succeeded and the status will reflect the remote
            const status = await git.status()
            res.send(toGitStatus(status))
        } catch (ex) {
            console.error(ex)
            res.status(500).send(ex)
        }
    })

    app.put('/notes/push', async (req, res) => {
        //This shouldn't really be necessary ever, but until the system is stable it's a good stop-gap
        try {
            await gitPullRebase();
            await git.push();
            const status = await git.status()
            res.send(toGitStatus(status))
        } catch (ex) {
            console.error(ex)
            res.status(500).send(ex)
        }
    })

    app.put('/notes/commit', async (req, res) => {
        const fullPath = getFullPath(req.query.path as string);
        const message = req.query.message as string;

        try {
            //add() is necessary in case there are new files. existing files are automatically added by the commit() command
            await git.add(fullPath)
            await git.commit(message || 'Updated via pim webapp', fullPath)
            await git.push()
            const result = await getPath(req.query.path as string)
            res.send(result)
        } catch (ex) {
            console.error(ex)
            res.status(500).send(ex)
        }

    })

    app.get('/notes/status', async(req, res) => {
        //must fetch before getting status so the 'behind' is updated
        const status = await git.fetch().status()
        res.send(toGitStatus(status))
    })

    app.get('/notes/search', async (req, res) => {
        try {
            const {query, excludeHidden} = req.query;

            const results = await Promise.all([
                execp(`cd ${CONTENT_DIRECTORY} && grep ${excludeHidden === 'true' ? '--exclude=\'.[^.]*\'' : ''} ${excludeHidden === 'true' ? '--exclude-dir=\'.[^.]*\'' : '--exclude-dir=.git'} -inr '${query}' .`),
                //TODO I'd like to return directories as well (excluded by -xtype f) but not every file in the directory
                execp(`cd ${CONTENT_DIRECTORY} && find . ${excludeHidden === 'true' ? '-not -path \'*/\\.*\'' : '-not -path \'./.git/*\''} -iname '*${query}*' -xtype f -printf "%h:%f\\n"`)
            ])
            const {stdout: contents, stderr: contentsErr} = results[0];
            const {stdout: names, stderr: namesErr} = results[1];

            if (contentsErr) {
                console.error('Failed while searching contents of files', contentsErr)
                throw contentsErr;
            }
            if (namesErr) {
                console.error('Failed while searching files by name', namesErr)
                throw namesErr;
            }


            res.send({
                names: names.split('\n').filter(Boolean).map(result => {
                    const parts = result.split(':');
                    return {
                        directory: parts[0],
                        fileName: parts[1],
                        path: `${parts[0]}/${parts[1]}`
                    }
                }),
                contents: Object.values(
                    contents.split('\n').filter(Boolean).reduce((acc, result) => {
                        const [path, lineNumber, ...text] = result.split(':');
                        const item = {
                            lineNumber,
                            text: text.join(':')
                        };
                        if (acc[path]) {
                            acc[path].items.push(item)
                        } else {
                            acc[path] = {
                                path,
                                items: [item]
                            }
                        }
                        return acc;
                    }, {}))
            })
        } catch (ex) {
            console.error(ex)
            res.status(500).send(ex)
        }

    });

}

const toGitStatus = (status: StatusResult) => ({
    ...status,
    notAdded: status.not_added,
    isClean: status.isClean(),
} as unknown as GitStatus)

const getGitChanges = async (path: string) => {
    //ideally nothing should ever be anything but modified - delete/create should commit immediately,
    //and there's no reason to  add without committing. But just in case the status gets screwed up, it's worth
    //considering all kinds of changes. Renames are tricky so it's not worth it, handle it separately

    const {modified, not_added, created, staged, deleted} = await git.status([path])
    const changed = [...modified, ...not_added, ...created, ...staged, ...deleted];
    //git.status() uses --porcelain which always returns a status relative to the path root, even if I cwd to the path and call status on '.'
    //attempt to relativize the paths
    const pathRelativeToRepoRoot = getPathRelativeToRepoRoot(path)
    console.log({path, pathRelativeToRepoRoot})
    return changed
}

async function gitPullRebase() {
    const stash = await git.stash()
    await git.pull(['--rebase'])
    if (!stash.includes('No local changes to save')) {
        await git.stash(['pop'])
    }
}

const getPathRelativeToRepoRoot = (path: string) => {
    const root = path.indexOf('./')
    return root !== -1 ? path.substring(root) : path
}


const getPath: (relativePath: string) => Promise<Directory | File> = async (relativePath) => {
    const {path, fullPath, isDirectory, directoryPath} = await getPathDetails(relativePath);
    const directoryInfo = await getDirectoryInfo(directoryPath)
    const fileContent = isDirectory || isBinary(path as string) ? null : await fs.promises.readFile(fullPath, 'utf8');
    const gitChanges = await getGitChanges(fullPath)

    gitChanges.forEach(changedPath => {
        const sanitized = changedPath.includes(' ') ? changedPath.substring(1, changedPath.length - 1) : changedPath;
        const split = sanitized.split('/');
        let index = -1
        let section = 0
        while (index === -1 && section < split.length) {
            index = directoryInfo.findIndex(d => d.name === split[section])
            section++
        }
        if (index !== -1) directoryInfo[index].pendingCommit = true
    })

    const response: Directory | File = {
        type: isDirectory ? 'D' : 'F',
        path: cleanPath(path),
        breadcrumbs: buildBreadcrumbs(path),
        directoryContents: directoryInfo,
        fileContent,
        pendingCommit: !!gitChanges.length,
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