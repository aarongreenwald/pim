import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {
    getNotes,
    saveFileContent,
    commitPath,
    gitPull,
    gitPush,
    getGitStatus,
    searchNotes
} from '../services/server-api';
import {IconButton, Panel, SearchBox, Spinner} from '@fluentui/react';
import {Link} from 'react-router-dom';
import {useLocation} from 'react-router';
import {Directory, File, GitStatus} from '@pim/common';
import {FileContent} from './file';
import {Directory as DirectoryViewer} from './directory';
import {Breadcrumbs} from './breadcrumbs';
import {useDebouncedInput} from '../common/debounced-input.hook';
import ReactMarkdown from 'react-markdown';


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
            <Breadcrumbs breadcrumbs={breadcrumbs} />
            <IconButton iconProps={{iconName: 'Search'}} title="Search" onClick={() => setShowSearch(true)} />
            {
                gitStatus?.behind? <IconButton iconProps={{iconName: 'Download'}} title="Git Pull" onClick={pullGit}/> : null
            }
            {
                gitStatus?.ahead ? <IconButton iconProps={{iconName: 'Upload'}} title="Git Push" onClick={pushGit}/> : null
            }
            {
                type === 'D' && <DirectoryViewer path={path} contents={directoryContents} onCommit={onCommit} pendingCommit={pendingCommit} />
            }
            {
                type === 'F' && <FileContent content={fileContent} onSaveContent={onSaveContent} pendingCommit={pendingCommit} onCommit={onCommit}/>
            }
            <Panel isOpen={showSearch} onDismiss={() => setShowSearch(false)}>
                <Search onDismiss={() => setShowSearch(false)}/>
            </Panel>
        </>
    )
};

const Search = ({onDismiss}) => {
    const {inputVal, debouncedValue, updateValue} = useDebouncedInput('')
    const [searchResults, setSearchResults] = useState(null)
    useEffect(() => {
        if (!debouncedValue) return;

        searchNotes(debouncedValue).then(setSearchResults)
    }, [debouncedValue])
    
    return (
        <>
            <SearchBox value={inputVal}
                       onChange={(_, val) => updateValue(val)}
                       onSearch={() => searchNotes(debouncedValue).then(setSearchResults)}/>
            {
                searchResults?.names.map(name =>
                    <Link key={name} to={`/notes/?path=${name}`} onClick={onDismiss}>{name}</Link>
                )
            }
            {
                searchResults?.contents.map((result) =>
                    <div key={`${result.path}_${result.lineNumber}`}>
                        <Link to={`/notes/?path=${result.path}`} onClick={onDismiss}>{result.path}</Link>
                        <ReactMarkdown>{result.text}</ReactMarkdown>
                    </div>
                )
            }
        </>
        )
    
}

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