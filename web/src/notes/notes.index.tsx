import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {getNotes, saveFileContent, commitPath, gitPull, getGitStatus} from '../services/server-api';
import {DefaultButton, Spinner} from '@fluentui/react';
import {useLocation} from 'react-router';
import {Directory, File, GitStatus} from '@pim/common';
import {FileContent} from './file';
import {Directory as DirectoryViewer} from './directory';
import {Breadcrumbs} from './breadcrumbs';

export const Notes: React.FC = () => {
    const {fileSystemItem, setFileSystemItem, refresh} = useFileSystemItem();

    const [gitStatus, setGitStatus] = useState<GitStatus>(null)
    useEffect(() => {
        getGitStatus().then(setGitStatus)
    }, [setGitStatus])

    const pullGit = useCallback(() => {
        gitPull()
            .then(setGitStatus)
            .then(refresh)
    }, [setGitStatus, refresh])

    const onSaveContent = useCallback((content: string) => {
        return saveFileContent(fileSystemItem.path, content).then(setFileSystemItem)
    }, [fileSystemItem?.path, setFileSystemItem]);

    const onCommit = useCallback(() => {
        //todo add support for specifying a commit message
        commitPath(fileSystemItem.path).then(setFileSystemItem)
    }, [fileSystemItem?.path])

    if (!fileSystemItem) return <Spinner />;

    const {
        breadcrumbs,
        path,
        directoryContents,
        fileContent,
        pendingCommit,
        type,
    } = fileSystemItem;

    return (
        <>
            {
                gitStatus?.behind ? <DefaultButton onClick={pullGit}>Pull</DefaultButton> : null
            }

            <Breadcrumbs breadcrumbs={breadcrumbs} />
            {
                type === 'D' && <DirectoryViewer path={path} contents={directoryContents} onCommit={onCommit} />
            }
            {
                type === 'F' && <FileContent content={fileContent} onSaveContent={onSaveContent} pendingCommit={pendingCommit} onCommit={onCommit}/>
            }
        </>
    )
};

function useFileSystemItem() {
    const [fileSystemItem, setFileSystemItem] = useState<File | Directory>(null)
    const location = useLocation()
    const path = getPath(location)

    const refresh = useCallback(() => {
        getNotes(path ?? '').then(setFileSystemItem)
    }, [path, setFileSystemItem])

    useEffect(() => refresh(), [refresh])
    return {fileSystemItem, setFileSystemItem, refresh}
}


function getPath(loc) {
    try {
        return loc.search.substring(1).split('&').find(x => x.indexOf('path') === 0).split('=')[1];
    } catch {
        return null;
    }
}