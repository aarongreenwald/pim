import {Express} from "express";
const fs = require('fs');
const {exec} = require('child_process')
const uuid = require('uuid')

export const setupFileserverRoutes = (app: Express) => {
    app.get('/notes/*', (req, res, next) => {
        const route = req.path.substring(6);
        const path = replaceSpaces(`./${route}`)
        const fullPath = `${CONTENT_DIRECTORY}/${path}`
        const {view, download} = req.query

        stat(fullPath).then((data: any) => {
            const isRoot = !data
            const isDirectory = isRoot || data.isDirectory();

            if (isDirectory) {
                handleDirectory({res, fullPath, path, download});
            } else {
                res.setHeader("content-type", view ? mimeType(path) : 'application/file');
                fs.createReadStream(fullPath).pipe(res);
            }

        })
            .catch(ex => {
                console.error(ex);
                res.status(404).send(`Not found: ${path}`);
            })
    })
}

const CONTENT_DIRECTORY = process.env.CONTENT_DIRECTORY || './'

fs.mkdir('./temporary_files', err => {});


const stat = path => new Promise((resolve, reject) => {
    fs.stat(path, (err, result) => {
        if (err) reject(err)

        resolve(result)
    })
})

const handleDirectory = ({res, fullPath, path, download}) => {
    if (download) {
        zipAndReturn(res, fullPath, path)
    } else {
        showDirectory(res, fullPath, path)
    }
}

const mimeType = path => {
    const filename = path.split('/').pop()
    const extension = filename.split('.').pop()

    switch(extension) {
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

const showDirectory = (res, fullPath, path) => {
    return readdir(fullPath).then((contents: any) => {
        const promises = contents.map(name => getStats(name, fullPath))
        return Promise.all(promises).then(stats => {
            res.send({
                breadcrumbs: buildBreadcrumbs(path),
                directory: cleanPath(path),
                items: stats
            });
        });
    }).catch(ex => {
        if (ex.code === 'ENOENT') {
            console.error('fff', ex.code)
            res.status(404).send(`Directory ${path} not found.`)
        } else {
            res.status(500).send(ex)
        }

    })

}

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
            name
        })
    })
})
