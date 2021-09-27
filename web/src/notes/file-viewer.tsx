import {IconButton} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import * as React from 'react';
import remarkGfm from 'remark-gfm';
import styled from '@emotion/styled';
import {commitIcon, editIcon} from './icons';

export const FileViewer = ({content, onEdit, pendingCommit, onCommit}) => {
    return (
        <>
            {
               pendingCommit && <IconButton iconProps={commitIcon} onClick={onCommit} title="Commit"/>
            }
            <IconButton iconProps={editIcon} onClick={onEdit} title="Edit"/>

            <StyledReactMarkdown remarkPlugins={[remarkGfm]}>{content}</StyledReactMarkdown>
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