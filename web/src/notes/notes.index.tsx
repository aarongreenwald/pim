import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {createItem, getNotes} from '../services/server-api';
import {ChoiceGroup, IconButton, PrimaryButton, Spinner, Stack, TextField} from '@fluentui/react';
import {Link} from 'react-router-dom';
import {useLocation} from 'react-router';
import Editor from '@monaco-editor/react';
import ReactMarkdown from 'react-markdown';
import {FileSystemItemType, NotesPathDto} from '@pim/common';
import {horizontalChoiceGroup, StyledChoiceGroup} from '../financials/styles';

export function Notes() {
    const [notes, setNotes] = useState<NotesPathDto>(null)
    const location = useLocation()
    const path = getPath(location)
    useEffect(() => {
        getNotes(path ?? '').then(setNotes)
    }, [path])

    if (!notes) return <Spinner />;

    const {
        breadcrumbs,
        path: directory,
        directoryInfo,
        fileContent,
        isDirectory
    } = notes;
    return (
        <>
            <Breadcrumbs breadcrumbs={breadcrumbs} />
            {
                isDirectory && <DirectoryCommandBar currentDirectory={path}/>
            }


            {
                isDirectory && <DirectoryContents directory={directory} contents={directoryInfo}/>
            }
            {
                !isDirectory && <FileContent content={fileContent} />
            }
        </>
    )
}

export const itemTypeRadioOptions = [{key: 'F', text: 'File'}, {key: 'D', text: 'Directory'}]


const DirectoryCommandBar = ({currentDirectory}) => {
    const [showNewItemForm, setShowNewItemForm] = useState(false)
    const [newItemType, setNewItemType] = useState<FileSystemItemType>('F')
    const [name, setName] = useState('')
    const saveFile = () => {
        //TODO after creating navigate to the newly recreated path
        createItem(currentDirectory, name, newItemType).then(() => setShowNewItemForm(false))
    }

    return (
        <>
            <IconButton iconProps={{iconName: 'Add'}} onClick={()=> setShowNewItemForm(true)} />
            {
                showNewItemForm &&
                <Stack horizontal>
                    <TextField placeholder={'Name'} value={name} onChange={(_, val) => setName(val)}/>
                    <StyledChoiceGroup
                        selectedKey={newItemType}
                        styles={horizontalChoiceGroup}
                        onChange={(_, val) => setNewItemType(val.key as FileSystemItemType)}
                        options={itemTypeRadioOptions}/>
                    <PrimaryButton disabled={!name} onClick={saveFile}>Create</PrimaryButton>
                </Stack>
            }
        </>
    )
}

const FileContent = ({content}) => {
    const [editMode, setEditMode] = useState(false);

    return (
        <>
            {
                editMode ?
                    <PrimaryButton onClick={() => setEditMode(false)}>Exit</PrimaryButton> :
                    <PrimaryButton onClick={() => setEditMode(true)}>Edit</PrimaryButton>
            }
            {
                editMode ? <Editor
                        options={{wordWrap: 'on'}}
                        defaultLanguage="markdown"
                        height='80vh'
                        defaultValue={content}
                    /> : <ReactMarkdown>{content}</ReactMarkdown>
            }
        </>
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
                {
                    item.isPlainText ?
                        <td><Link to={`/notes?path=${directory}/${item.name}`}>{item.name}</Link></td> :
                        <td>{item.name}</td>
                }

                <td><a href={`/api/notes/download?path=${directory}/${item.name}`} download={item.name}>&#8595;</a></td>
                {
                    item.openInBrowser &&
                    <td><a href={`/api/notes/viewfile?path=${directory}/${item.name}`}>??</a></td>
                }

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