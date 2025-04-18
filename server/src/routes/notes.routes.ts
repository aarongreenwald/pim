import {Express} from 'express';
import fs from 'fs';
import {exec} from 'child_process';
import {resolvePath} from '../utils/utils';
import {
  Breadcrumb,
  Directory,
  DirectoryItem,
  File,
  FileSystemItemType,
  GitStatus,
  NotesSearchResults,
  DiaryInfo
} from "@pim/common";
import path from 'path';
import bodyParser from 'body-parser';
import simpleGit, {SimpleGit, StatusResult} from 'simple-git';
import util from 'util';

const uuid = require('uuid')
const textParser = bodyParser.text();
const execp = util.promisify(exec);

const NOTES_PATH = process.env.NOTES_PATH;
if (!NOTES_PATH) {
  throw 'Environment variable NOTES_PATH is not set!'
}
const CONTENT_DIRECTORY = resolvePath(process.env.NOTES_PATH)
const git: SimpleGit = simpleGit(CONTENT_DIRECTORY);

export const setupNotesRoutes = (app: Express) => {

  app.get('/notes/path', decodeQueryParams(), async (req, res) => {
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

  app.get('/notes/download', decodeQueryParams(), async (req, res) => {
    const {path, fullPath, isDirectory} = await getPathDetails(req.query.path as string);

    if (isDirectory) {
      zipAndReturn(res, fullPath, path)
    } else {
      res.setHeader('content-type','application/file');
      fs.createReadStream(fullPath).pipe(res);
    }
  })

  app.get('/notes/viewfile', decodeQueryParams(), async (req, res) => {
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

  app.put('/notes/files', decodeQueryParams(), textParser, async (req, res) => {
    const fullPath = getFullPath(req.query.path as string);

    try {
      uncommittedFiles.set(fullPath, new Date())
      await fs.promises.writeFile(fullPath, req.body);
      const result = await getPath(req.query.path as string)
      res.send(result)
    } catch (ex) {
      console.error(ex)
      res.status(500).send(ex)
    }
  })

  app.post('/notes/path', decodeQueryParams(), async (req, res) => {
    const fullPath = getFullPath(req.query.path as string);
    const itemType = req.query.type as FileSystemItemType;
    const name = req.query.name as string;
    const createdPath = path.join(fullPath, name);
    try {
      if (itemType === 'F') {
        uncommittedFiles.set(fullPath, new Date())
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

  app.put('/notes/commit', decodeQueryParams(), async (req, res) => {
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

  app.get('/notes/search', decodeQueryParams('query'), async (req, res) => {
    try {
      const {query, excludeHidden} = req.query;

      const results = await Promise.all([
        //pipe to cat at the end so that if no results are found the exit code will be 0 and not 1
        execute(`cd ${CONTENT_DIRECTORY} && grep ${excludeHidden === 'true' ? '--exclude=\'.[^.]*\'' : ''} ${excludeHidden === 'true' ? '--exclude-dir=\'.[^.]*\'' : '--exclude-dir=.git'} -inr '${query}' . | cat`),
        //TODO I'd like to return directories as well (excluded by -xtype f) but not every file in the directory
        execute(`cd ${CONTENT_DIRECTORY} && find . ${excludeHidden === 'true' ? '-not -path \'*/\\.*\'' : '-not -path \'./.git/*\''} -iname '*${query}*' -xtype f -printf "%h:%f\\n"`)
      ])
      const [contents, names] = results;

      const response: NotesSearchResults = {
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
      };
      res.send(response)
    } catch (ex) {
      console.error(ex)
      res.status(500).send(ex)
    }

  });

  app.get('/notes/recent', async (req, res) => {
    try {
      const recentFilesScript = path.resolve(__dirname, '..', 'cli', 'recent-files.sh')
      const files = await execute(`cd ${CONTENT_DIRECTORY} && ${recentFilesScript} 40 10`)
      res.send(files.split('\n').filter(Boolean))
    } catch (ex) {
      console.error(ex)
      res.status(500).send(ex)
    }
  })

  app.get('/notes/diary-path', decodeQueryParams(), async (req, res) => {    
    try {
      const date = req.query.date as string;
      const createIfNotExists = (req.query.createIfNotExists as string) ? 1 : 0;
      
      const diaryPathScript = path.resolve(__dirname, '..', 'cli', 'diary-path.sh')
      const diaryPath = await execute(`cd ${CONTENT_DIRECTORY} && ${diaryPathScript} ${date} ${createIfNotExists}`)
      res.send({diaryPath})
    } catch (ex) {
      console.error(ex)
      res.status(500).send(ex)
    }
  })

  
  app.post('/notes/move', decodeQueryParams(), async (req, res) => {
    try {
      const from = req.query.from as string;
      const to = req.query.to as string;

      await git.add(from) //in case it's a new file it needs to be added before committing
      await git.commit(`Commit changes in ${from} before moving`, from)

      await git.mv(from, to)
      await git.commit(`Renamed ${from} to ${to} via pim webapp`, [from, to])

      await git.push()
      //TODO consider returning the $from directory so the UI can update easily
      //but I might want to call this from other places, like breadcrumbs (in which case I need to return the $from)
      res.send(200)
    } catch (ex) {
      console.error(ex)
      res.status(500).send(ex)
    }
  })
}

const decodeQueryParams = (...paramNames) => {
  return (req, res, next) => {
    if (!paramNames.length) {
      paramNames = Object.keys(req.query)
    }

    paramNames.forEach(name => {
      if (req.query[name]) {
        req.query[name] = decodeURIComponent(req.query[name])
      }
    })
    next()
  }
}

const uncommittedFiles = new Map<string, Date>()
const AUTOCOMMIT_DELAY_MS = 20 * 1000 * 60 //20 minutes
setInterval(async () => {
  const entries = uncommittedFiles.entries()
  for (let [path, lastChanged] of entries) {
    if (lastChanged.getTime() + AUTOCOMMIT_DELAY_MS < new Date().getTime()) {
      try {
        console.log(`Attempting to autocommit ${path}`)
        await git.add(path)
        await git.commit('Autocommitted via pim webapp', path)
        await git.push()
        uncommittedFiles.delete(path)
      } catch (ex) {
        console.error(`Failed to autocommit ${path}`, ex)
        //Try again at the next interval. Consider just deleting it to not clutter the logs.
        uncommittedFiles.set(path, new Date())
      }
    }
  }
}, 1000 * 30)

const execute = async (cmd: string) => {
  const {stdout, stderr} = await execp(cmd)
  if (stderr) {
    console.error(`Failed to execute: ${cmd}`, stderr)
    throw stderr
  }
  return stdout
}

const toGitStatus = (status: StatusResult) => ({
  ...status,
  notAdded: status.not_added,
  isClean: status.isClean(),
} as unknown as GitStatus)

const getGitChanges = async (path: string): Promise<string[]> => {
  //ideally nothing should ever be anything but modified - delete/create should commit immediately,
  //and there's no reason to  add without committing. But just in case the status gets screwed up, it's worth
  //considering all kinds of changes. Renames are tricky so it's not worth it, handle it separately

  const {modified, not_added, created, staged, deleted} = await git.status([path])
  return [...modified, ...not_added, ...created, ...staged, ...deleted]
}

async function gitPullRebase() {
  const stash = await git.stash()
  await git.pull(['--rebase'])
  if (!stash.includes('No local changes to save')) {
    await git.stash(['pop'])
  }
}

//git.status() uses --porcelain which always returns a status relative to the path root, even if I cwd to the path and call status on '.'
//attempt to relativize the paths
// const getPathRelativeToRepoRoot = (path: string) => {
//     const root = path.indexOf('./')
//     return root !== -1 ? path.substring(root) : path
// }

const getPath: (relativePath: string) => Promise<Directory | File> = async (relativePath) => {
  const {path, fullPath, isDirectory, directoryPath} = await getPathDetails(relativePath);
  const directoryInfo = await getDirectoryInfo(directoryPath)
  const diaryInfo = getDiaryInfo(directoryInfo, isDirectory, path)
  const fileContent = isDirectory || isBinary(path as string) ? null : await fs.promises.readFile(fullPath, 'utf8');
  const gitChanges = await getGitChanges(fullPath)

  gitChanges.forEach(changedPath => {
    //paths with spaces are surrounded by quotes. strip the quotes
    const sanitized = changedPath.includes(' ') ? changedPath.substring(1, changedPath.length - 1) : changedPath;
    const split = sanitized.split('/');
    let index = -1
    let section = 0
    //TODO this logic might be wrong because it doesn't compare the whole path, and
    //similar file names in different parts of the tree could conflict. I need unit tests here
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
    diaryInfo,
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

const getDiaryInfo = (directoryInfo: DirectoryItem[], isDirectory: boolean, path: string): DiaryInfo | null => { 
  if (!directoryInfo.find(f => f.name === ".diary")) {
    return null
  }

  // TODO this assumes that the input directoryInfo array is already sorted (at least when it's a diary
  // and that other than the .diary file, which is always first, there are no other non-diary entries in the directory
  
  if (isDirectory) {
    return {
      latest: directoryInfo[directoryInfo.length - 1],
      prev: null,
      next: null
    }
  } else {
    const currIndex = directoryInfo.findIndex(x => x.name === path.split('/').pop())
    return {
      latest: directoryInfo[directoryInfo.length - 1],
      prev: currIndex > 1 ? directoryInfo[currIndex - 1] : null,
      next: currIndex < directoryInfo.length - 1 ? directoryInfo[currIndex + 1] : null
    }
  }
  
  //TODO if there is eventually relevant info in the .diary file, it can be read here as well
}

async function getPathDetails<P, ResBody, ReqBody, ReqQuery>(path: string) {
  const fullPath = getFullPath(path)
  const data = await fs.promises.stat(fullPath);
  const isRoot = !data
  const isDirectory = isRoot || data.isDirectory();
  const directoryPath = isDirectory ? fullPath : fullPath.split('/').slice(0, -1).join('/');
  return {path, fullPath, isDirectory, directoryPath};
}

const getFullPath = (path: string) => {
  console.log(CONTENT_DIRECTORY, path)
  return require('path').join(CONTENT_DIRECTORY, path);
}


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
    path: ''
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

fs.mkdir('temporary_files', err => {});
