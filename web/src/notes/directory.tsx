import * as React from 'react';
import {useMemo, useState} from 'react';
import {FileSystemItemType} from '@pim/common';
import {createItem} from '../services/server-api';
import {Icon, IconButton, Stack, TextField} from '@fluentui/react';
import {horizontalChoiceGroup, StyledChoiceGroup} from '../financials/styles';
import {Link} from 'react-router-dom';


export const Directory = ({path, contents, onCommit, pendingCommit}) => {
    const [showHidden, setShowHidden] = useState(false)
    const filteredContents = useMemo(() => {
        if (!showHidden) {
            return contents.filter(i => i.name[0] !== '.')
        }
        return contents;
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
    const saveFile = () => {
        //TODO after creating navigate to the newly recreated path
        createItem(currentDirectory, name, newItemType).then(() => setShowNewItemForm(false))
    }

    return (
        <>
            {
                pendingCommit && <IconButton title="Commit" iconProps={{iconName: 'BranchCommit'}} onClick={onCommit}/>
            }
            <IconButton iconProps={{iconName: 'Hide3'}} toggle checked={showHidden} title="Show hidden folders" onClick={() => setShowHidden(val => !val)}/>
            <IconButton iconProps={{iconName: 'Add'}} onClick={() => setShowNewItemForm(true)}/>
            {
                showNewItemForm &&
                <Stack horizontal>
                    <TextField placeholder={'Name'} value={name} onChange={(_, val) => setName(val)}/>
                    <StyledChoiceGroup
                        selectedKey={newItemType}
                        styles={horizontalChoiceGroup}
                        onChange={(_, val) => setNewItemType(val.key as FileSystemItemType)}
                        options={itemTypeRadioOptions}/>
                    <IconButton iconProps={{iconName: 'CheckMark'}} disabled={!name} onClick={saveFile} title="Create"/>
                    <IconButton iconProps={{iconName: 'Cancel'}} onClick={() => setShowNewItemForm(false)} title="Cancel"/>
                </Stack>
            }
        </>
    )
}

const DirectoryContents = ({directory, contents}) => (
    <table>
        <tbody>
        {contents.map((item) =>
            <tr key={item.name}>
                <td><Icon iconName={item.isDirectory ? 'FabricFolder' : 'TextDocument'}/></td>
                <td>{item.pendingCommit ? '*' : ' '}</td>
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

const itemTypeRadioOptions = [{key: 'F', text: 'File'}, {key: 'D', text: 'Directory'}]