import {PrimaryButton} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import * as React from 'react';

export const FileViewer = ({content, onEdit}) => {
    return (
        <>
            <PrimaryButton onClick={onEdit}>Edit</PrimaryButton>
            <ReactMarkdown>{content}</ReactMarkdown>
        </>
    )
}