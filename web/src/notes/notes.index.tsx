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

    if (typeof notes === 'string') {
        return <div>{notes}</div>
    }

    const {
        breadcrumbs,
        directory,
        items
    } = notes;
    return (
        <div>
            <Breadcrumbs breadcrumbs={breadcrumbs} />
            <DirectoryContents directory={directory} contents={items}/>
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
                <td><Link to={`/notes?path=${directory}/${item.name}&view=true`}>{item.name}</Link></td>
                <td><a href={`/api/notes/${directory}/${item.name}?download=true`}>&#8595;</a></td>
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