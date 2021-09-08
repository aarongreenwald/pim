import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {getNotes, saveFileContent} from '../services/server-api';
import {Spinner} from '@fluentui/react';
import {useLocation} from 'react-router';
import {Directory, File} from '@pim/common';
import {FileContent} from './file';
import {Directory as DirectoryViewer} from './directory';
import {Breadcrumbs} from './breadcrumbs';

export const Notes: React.FC = () => {
    const {fileSystemItem, setFileSystemItem} = useFileSystemItem();

    const onSaveContent = useCallback((content: string) => {
        return saveFileContent(fileSystemItem.path, content).then(setFileSystemItem)
    }, [fileSystemItem?.path]);

    if (!fileSystemItem) return <Spinner />;

    const {
        breadcrumbs,
        path,
        directoryContents,
        fileContent,
        type,
    } = fileSystemItem;

    return (
        <>
            <Breadcrumbs breadcrumbs={breadcrumbs} />
            {
                type === 'D' && <DirectoryViewer path={path} contents={directoryContents} />
            }
            {
                type === 'F' && <FileContent content={fileContent} onSaveContent={onSaveContent}/>
            }
        </>
    )
};

function useFileSystemItem() {
    const [fileSystemItem, setFileSystemItem] = useState<File | Directory>(null)
    const location = useLocation()
    const path = getPath(location)
    useEffect(() => {
        getNotes(path ?? '').then(setFileSystemItem)
    }, [path])
    return {fileSystemItem, setFileSystemItem}
}


function getPath(loc) {
    try {
        return loc.search.substring(1).split('&').find(x => x.indexOf('path') === 0).split('=')[1];
    } catch {
        return null;
    }
}