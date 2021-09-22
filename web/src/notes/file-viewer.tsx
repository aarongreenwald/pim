import {DefaultButton, PrimaryButton} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import * as React from 'react';
import remarkGfm from 'remark-gfm';
import styled from '@emotion/styled';

export const FileViewer = ({content, onEdit, pendingCommit, onCommit}) => {
    return (
        <>
            <PrimaryButton onClick={onEdit}>Edit</PrimaryButton>
            {
               pendingCommit && <DefaultButton onClick={onCommit}>Commit</DefaultButton>
            }
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