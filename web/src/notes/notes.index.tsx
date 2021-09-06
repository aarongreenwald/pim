import * as React from 'react';
import {useEffect, useState} from 'react';
import {getNotes} from '../services/server-api';
import {Spinner} from '@fluentui/react';
import {Link} from 'react-router-dom';
import {useLocation} from 'react-router';

export function Notes() {
    const [notes, setNotes] = useState(null)
    const location = useLocation()
    const path = getPath(location)
    useEffect(() => {
        getNotes(path ?? '').then(setNotes)
    }, [path])

    if (!notes) return <Spinner />;

    const {
        breadcrumbs,
        path: directory,
        directoryInfo,
        fileContent,
        isDirectory
    } = notes;
    return (
        <div>
            <Breadcrumbs breadcrumbs={breadcrumbs} />
            {
                isDirectory && <DirectoryContents directory={directory} contents={directoryInfo}/>
            }
            {
                fileContent && <div dangerouslySetInnerHTML={{__html: fileContent.replace(/\n/g, '<br />')}}/>
            }
        </div>
    )
}

const Breadcrumbs = ({breadcrumbs}) => (
    <h3>
        {breadcrumbs.map((breadcrumb) =>
            <span key={breadcrumb.name}><Link to={`/notes/?path=${breadcrumb.path}`}>{breadcrumb.name}</Link> /</span>
        )}
    </h3>
);

const DirectoryContents = ({directory, contents}) => (
    <table>
        <tbody>
        {contents.map((item) =>
            <tr key={item.name}>
                <td>{item.isDirectory ? 'D' : 'F' }</td>
                {
                    item.isPlainText ?
                        <td><Link to={`/notes?path=${directory}/${item.name}`}>{item.name}</Link></td> :
                        <td>{item.name}</td>
                }

                <td><a href={`/api/notes/download?path=${directory}/${item.name}`} download={item.name}>&#8595;</a></td>
                {
                    item.openInBrowser &&
                    <td><a href={`/api/notes/viewfile?path=${directory}/${item.name}`}>??</a></td>
                }

            </tr>
        )}
        </tbody>
    </table>
);


function getPath(loc) {
    try {
        return loc.search.substring(1).split('&').find(x => x.indexOf('path') === 0).split('=')[1];
    } catch {
        return null;
    }
}