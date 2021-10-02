import * as React from 'react';
import {useState} from 'react';
import {FileEditor} from './file-editor';
import {FileViewer} from './file-viewer';

interface FileContentProps {
    content: string;
    onSaveContent: (content: string) => Promise<void>;
    onCommit: () => void;
    pendingCommit: boolean;
    path: string;
}
export const FileContent: React.FC<FileContentProps> = ({content, onSaveContent, onCommit, pendingCommit, path}) => {
    const [editMode, setEditMode] = useState(false);

    return editMode ?
        <FileEditor content={content} onSaveContent={onSaveContent} onExitEditor={() => setEditMode(false)} path={path}/> :
        <FileViewer content={content} onEdit={() => setEditMode(true)} onCommit={onCommit} pendingCommit={pendingCommit} path={path}/>;
}