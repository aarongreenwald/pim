import * as React from 'react';
import {useState} from 'react';
import {FileSystemItemType} from '@pim/common';
import {createItem} from '../services/server-api';
import {IconButton, PrimaryButton, Stack, TextField} from '@fluentui/react';
import {horizontalChoiceGroup, StyledChoiceGroup} from '../financials/styles';
import {Link} from 'react-router-dom';


export const Directory = ({path, contents}) => {
    return (
        <>
            <DirectoryCommandBar currentDirectory={path}/>
            <DirectoryContents directory={path} contents={contents}/>
        </>
    )
}

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
                    <PrimaryButton disabled={!name} onClick={saveFile}>Create</PrimaryButton>
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
                <td>{item.isDirectory ? 'D' : 'F'}</td>
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