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
import {DiaryInfo, Directory, File, GitStatus} from '@pim/common';
import {FileContent} from './file';
import {Directory as DirectoryViewer} from './directory';
import {Breadcrumbs} from './breadcrumbs';
import {Search} from './search';
import {downloadIcon, searchIcon, uploadIcon} from './icons';
import {RecentNotes} from './recent-notes';
import {Link} from 'react-router-dom';
import {todayAsISODate} from '../common/date.utils';
import {useLoadTodayDiary} from './todays-diary';

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
    diaryInfo
  } = fileSystemItem;

  const parentDirectory = breadcrumbs.length > 2 ? breadcrumbs[breadcrumbs.length - 2].path : null;

  return (
    <>
      <Breadcrumbs breadcrumbs={breadcrumbs} />
      <div style={{display: 'flex', justifyContent: 'space-between'}}>
        {
          diaryInfo && type === 'F' &&
            <DiaryPrevNext diaryInfo={diaryInfo} directory={parentDirectory} />
        }
      </div>
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

const DiaryPrevNext: React.FC<{diaryInfo: DiaryInfo; directory: string}> = ({diaryInfo, directory}) => {
  const loadTodayDiary = useLoadTodayDiary()
  const today = todayAsISODate() //TODO consider memoizing? If there are #perf issues start here

  return (
    <>
      {
        diaryInfo.prev &&
          <Link to={`/notes?path=${encodeURIComponent(`${directory}/${diaryInfo.prev.name}`)}`}>{`<< ${diaryInfo.prev.name}`}</Link>
      }
      {
        diaryInfo.next &&
          <Link to={`/notes?path=${encodeURIComponent(`${directory}/${diaryInfo.next.name}`)}`}>{`${diaryInfo.next.name} >>`}</Link>
      }
      {
        !diaryInfo.next && today.toString() != diaryInfo.latest.name && // we're at the end and the latest (ie current) isn't already today's diary
          <a href="#" onClick={loadTodayDiary}>{'Today >>'}</a>
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
