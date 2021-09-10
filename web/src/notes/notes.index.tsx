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
    const [gitStatus, setGitStatus] = useState<GitStatus>(null)
    useEffect(() => {
        getGitStatus().then(setGitStatus)
    }, [setGitStatus])

    const pullGit = useCallback(() => {
        gitPull().then(setGitStatus)
    }, [setGitStatus])

    const {fileSystemItem, setFileSystemItem} = useFileSystemItem();

    const onSaveContent = useCallback((content: string) => {
        return saveFileContent(fileSystemItem.path, content).then(setFileSystemItem)
    }, [fileSystemItem?.path, setFileSystemItem]);

    const onCommit = useCallback(() => {
        commitPath(fileSystemItem.path) //TODO update the status, support a message
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
                gitStatus?.behind && <DefaultButton onClick={pullGit}>Pull</DefaultButton>
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