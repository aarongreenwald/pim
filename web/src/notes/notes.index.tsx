import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {
    commitPath,
    getGitStatus,
    getNotes,
    gitPull,
    gitPush,
    saveFileContent
} from '../services/server-api';
import {IconButton, Label, Spinner} from '@fluentui/react';
import {useLocation} from 'react-router';
import {Directory, File, GitStatus} from '@pim/common';
import {FileContent} from './file';
import {Directory as DirectoryViewer} from './directory';
import {Breadcrumbs} from './breadcrumbs';
import {Search} from './search';
import {downloadIcon, searchIcon, uploadIcon} from './icons';
import {RecentNotes} from './recent-notes';

export const Notes: React.FC = () => {

    const {fileSystemItem, setFileSystemItem, refresh} = useFileSystemItem();
    const [showSearch, setShowSearch] = useState(false)

    const [gitStatus, setGitStatus] = useState<GitStatus>(null)
    useEffect(() => {
        getGitStatus().then(setGitStatus)
    }, [setGitStatus])

    const pullGit = useCallback(() => {
        gitPull()
            .then(setGitStatus)
            .then(refresh)
    }, [setGitStatus, refresh])

    const pushGit = useCallback(() => {
        gitPush()
            .then(setGitStatus)
            .then(refresh)
    }, [setGitStatus, refresh])

    const onSaveContent = useCallback((content: string) => {
        return saveFileContent(fileSystemItem.path, content).then(setFileSystemItem)
    }, [fileSystemItem?.path, setFileSystemItem]);

    const onCommit = useCallback(() => {
        //todo add support for specifying a commit message
        commitPath(fileSystemItem.path).then(setFileSystemItem)
    }, [fileSystemItem?.path, setFileSystemItem])

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
            <Breadcrumbs breadcrumbs={breadcrumbs} />
            <IconButton iconProps={searchIcon} title="Search" onClick={() => setShowSearch(true)} />
            {
                gitStatus?.behind? <IconButton iconProps={downloadIcon} title="Git Pull" onClick={pullGit}/> : null
            }
            {
                gitStatus?.ahead ? <IconButton iconProps={uploadIcon} title="Git Push" onClick={pushGit}/> : null
            }
            {
                type === 'D' &&
                <>
                    <DirectoryViewer path={path} contents={directoryContents} onCommit={onCommit} pendingCommit={pendingCommit} />
                    <hr />
                    <Label>Recent Files</Label>
                    <RecentNotes />
                </>
            }
            {
                type === 'F' && <FileContent content={fileContent} onSaveContent={onSaveContent} pendingCommit={pendingCommit} onCommit={onCommit} path={path}/>
            }
            <Search show={showSearch} onDismiss={() => setShowSearch(false)}/>
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