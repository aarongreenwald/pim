import {IconButton} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import * as React from 'react';
import remarkGfm from 'remark-gfm';
import styled from '@emotion/styled';
import {commitIcon, editIcon} from './icons';

interface FileViewerProps {
    content: string;
    onEdit: () => void;
    pendingCommit: boolean;
    onCommit: () => void;
    path: string;
}
export const FileViewer: React.FC<FileViewerProps> = ({content, onEdit, pendingCommit, onCommit, path}) => {
    return (
        <>
            {
               pendingCommit && <IconButton iconProps={commitIcon} onClick={onCommit} title="Commit"/>
            }
            <IconButton iconProps={editIcon} onClick={onEdit} title="Edit"/>

            <StyledReactMarkdown
                remarkPlugins={[remarkGfm]}
                transformLinkUri={customUriHandler(path)}>
                {content}
            </StyledReactMarkdown>
        </>
    )
}

const StyledReactMarkdown = styled(ReactMarkdown)`
  ul.contains-task-list {
    padding-left: 1em;
    
    li {
      list-style: none;  
    }
  }
`

const customUriHandler = (path: string) => (uri: string) => {
    if (uri.indexOf('./') === 0) {
        //this whole thing is pretty hacky. works in the common case of a relative link to a file in the same directory
        //anything else doesn't
        path = `./${path}`;
        const directory = path.substr(0, path.lastIndexOf('/'))
        return `#/notes?path=${directory.substring(2)}/${uri.substring(2)}`
    }
    return uri
}