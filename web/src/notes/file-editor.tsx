import {useCallback, useEffect, useRef, useState} from 'react';
import {PrimaryButton, Spinner} from '@fluentui/react';
import Editor from '@monaco-editor/react';
import * as React from 'react';

export const FileEditor = ({content, onSaveContent, onExitEditor}) => {
    const [saving, setSaving] = useState(false)
    const editorRef = useRef(null);

    const onEditorMount = editor => editorRef.current = editor

    const saveContent = useCallback(() => {
        const draft = editorRef.current.getValue();
        if (content !== draft) {
            setSaving(true)
            onSaveContent(draft).then(() => setSaving(false))
        }
    }, [onSaveContent, setSaving, content])

    const onExit = async () => {
        await saveContent()
        onExitEditor();
    }

    useEffect(() => {
        const interval = setInterval(() => saveContent(), 1000 * 30)
        return () => clearInterval(interval)
    }, [content, saveContent])

    return (
        <>

            <PrimaryButton onClick={onExit}>Exit</PrimaryButton>
            <PrimaryButton onClick={saveContent}>Save</PrimaryButton>
            {
                saving && <Spinner />
            }
            <Editor
                    options={{wordWrap: 'on'}}
                    defaultLanguage="markdown"
                    height='80vh'
                    defaultValue={content}
                    onMount={onEditorMount}
            />
        </>
    )
}
