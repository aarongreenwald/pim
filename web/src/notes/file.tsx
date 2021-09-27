import * as React from 'react';
import {useState} from 'react';
import {FileEditor} from './file-editor';
import {FileViewer} from './file-viewer';

export const FileContent = ({content, onSaveContent, onCommit, pendingCommit, path}) => {
    const [editMode, setEditMode] = useState(false);

    return editMode ?
        <FileEditor content={content} onSaveContent={onSaveContent} onExitEditor={() => setEditMode(false)} path={path}/> :
        <FileViewer content={content} onEdit={() => setEditMode(true)} onCommit={onCommit} pendingCommit={pendingCommit} path={path}/>;
}