import * as React from 'react';
import {useState} from 'react';
import {FileEditor} from './file-editor';
import {FileViewer} from './file-viewer';

export const FileContent = ({content, onSaveContent}) => {
    const [editMode, setEditMode] = useState(false);

    return editMode ?
        <FileEditor content={content} onSaveContent={onSaveContent} onExitEditor={() => setEditMode(false)}/> :
        <FileViewer content={content} onEdit={() => setEditMode(true)}/>;
}