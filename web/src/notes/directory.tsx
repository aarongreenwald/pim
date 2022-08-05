import * as React from 'react';
import {useMemo, useState} from 'react';
import {FileSystemItemType} from '@pim/common';
import {createItem, renameDirectoryItem} from '../services/server-api';
import {Icon, IconButton, Stack, TextField} from '@fluentui/react';
import {horizontalChoiceGroup, StyledChoiceGroup} from '../financials/styles';
import {Link, useHistory} from 'react-router-dom';
import {
    addIcon,
    cancelIcon,
    checkIcon,
    commitIcon, downloadIconName,
    editIcon, externalIconName,
    fileIconName,
    folderIconName,
    hideIcon,
    saveIcon
} from './icons';

interface DirectoryProps {
    path: string;
    contents: {
        name: string;
    }[];
    onCommit: () => void;
    pendingCommit: boolean;
}

export const Directory: React.FC<DirectoryProps> = ({path, contents, onCommit, pendingCommit}) => {
    const [showHidden, setShowHidden] = useState(false)
    const filteredContents = useMemo(() => { 
        //HACK - this should be optional, but when it matters, more often than not reversed is more useful than the order on disk
        let results = contents.reverse();

        if (!showHidden) {
            return results.filter(i => i.name[0] !== '.')
        }        

        return results;
    }, [contents, showHidden])
    return (
        <>
            <DirectoryCommandBar currentDirectory={path} onCommit={onCommit} pendingCommit={pendingCommit} showHidden={showHidden} setShowHidden={setShowHidden}/>
            <DirectoryContents directory={path} contents={filteredContents}/>
        </>
    )
}

const DirectoryCommandBar = ({currentDirectory, onCommit, pendingCommit, showHidden, setShowHidden}) => {
    const [showNewItemForm, setShowNewItemForm] = useState(false)
    const [newItemType, setNewItemType] = useState<FileSystemItemType>('F')
    const [name, setName] = useState('')
    const history = useHistory();
    const saveFile = () => {
        createItem(currentDirectory, name, newItemType)
            .then(() => setShowNewItemForm(false))
            .then(() => history.push(`/notes?path=${encodeURIComponent(`${currentDirectory}/${name}`)}`))
    }

    return (
        <>
            {
                pendingCommit && <IconButton title="Commit" iconProps={commitIcon} onClick={onCommit}/>
            }
            <IconButton iconProps={hideIcon} toggle checked={showHidden} title="Show hidden folders" onClick={() => setShowHidden(val => !val)}/>
            <IconButton iconProps={addIcon} onClick={() => setShowNewItemForm(true)}/>
            {
                showNewItemForm &&
                <Stack horizontal>
                    <TextField placeholder={'Name'} value={name} onChange={(_, val) => setName(val)}/>
                    <StyledChoiceGroup
                        selectedKey={newItemType}
                        styles={horizontalChoiceGroup}
                        onChange={(_, val) => setNewItemType(val.key as FileSystemItemType)}
                        options={itemTypeRadioOptions}/>
                    <IconButton iconProps={checkIcon} disabled={!name} onClick={saveFile} title="Create"/>
                    <IconButton iconProps={cancelIcon} onClick={() => setShowNewItemForm(false)} title="Cancel"/>
                </Stack>
            }
        </>
    )
}

const DirectoryContents = ({directory, contents}) => (
    <table>
        <tbody>
        {contents.map((item) => <DirectoryItem directory={directory} item={item} key={item.name} />)}
        </tbody>
    </table>
);

const DirectoryItem = ({directory, item}) => {
    const [editMode, setEditMode] = useState(false)
    const [itemNameDraft, setItemNameDraft] = useState(item.name)

    const onSave = () => {
        renameDirectoryItem(`${directory}/${item.name}`, `${directory}/${itemNameDraft}`)
            .then(() => setEditMode(false)) //TODO refresh view
    }

    return (
        <tr>
            <td><Icon iconName={item.isDirectory ? folderIconName : fileIconName}/></td>
            <td>{item.pendingCommit ? '*' : ' '}</td>
            {
                editMode ?
                    <td><TextField placeholder={item.name} value={itemNameDraft} onChange={(_, val) => setItemNameDraft(val)} /></td> :
                    item.isPlainText ?
                        <td><Link to={`/notes?path=${encodeURIComponent(`${directory}/${item.name}`)}`}>{item.name}</Link></td> :
                        <td>{item.name}</td>
            }

            <td>
                <a href={`/api/notes/download?path=${directory}/${item.name}`} download={item.name}>
                    <Icon iconName={downloadIconName} styles={iconStyles}/>
                </a>
            </td>
            <td>
                {
                    item.openInBrowser &&
                    <a href={`/api/notes/viewfile?path=${directory}/${item.name}`}><Icon iconName={externalIconName} styles={iconStyles}/></a>
                }
            </td>
            <td>
                {
                    !editMode ?
                        <IconButton iconProps={editIcon} styles={iconButtonStyles} onClick={() => setEditMode(true)} /> :
                        <>
                            <IconButton iconProps={saveIcon} styles={iconButtonStyles} onClick={onSave} />
                            <IconButton iconProps={cancelIcon} styles={iconButtonStyles} onClick={() => setEditMode(false)} />
                        </>
                }
            </td>

        </tr>
    )
}

const iconStyles = {root: {fontSize: 16}};
const iconButtonStyles = {
    root: {
        height: 16,
        width: 16
    }
};

const itemTypeRadioOptions = [{key: 'F', text: 'File'}, {key: 'D', text: 'Directory'}]