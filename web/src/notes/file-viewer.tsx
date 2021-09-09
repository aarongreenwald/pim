import {DefaultButton, PrimaryButton} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import * as React from 'react';

export const FileViewer = ({content, onEdit, pendingCommit, onCommit}) => {
    return (
        <>
            <PrimaryButton onClick={onEdit}>Edit</PrimaryButton>
            {
               pendingCommit && <DefaultButton onClick={onCommit}>Commit</DefaultButton>
            }
            <ReactMarkdown>{content}</ReactMarkdown>
        </>
    )
}