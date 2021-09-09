import {useCallback, useEffect, useRef, useState} from 'react';
import {PrimaryButton, Spinner, Stack, Toggle} from '@fluentui/react';
import Editor from '@monaco-editor/react';
import * as React from 'react';
import AceEditor from 'react-ace';
import {Prompt} from 'react-router';
require('ace-builds/src-noconflict/mode-markdown');
require('ace-builds/src-noconflict/theme-tomorrow_night_eighties');
//see more themes here: https://ace.c9.io/build/kitchen-sink.html
//more options here: https://ace.c9.io/build/kitchen-sink.html //TODO - search

export const FileEditor = ({content, onSaveContent, onExitEditor}) => {
    const [saving, setSaving] = useState(false)
    const [wordWrap, setWordWrap] = useState(true)
    //TODO automatically use ace on mobile, or configure ace to be as nice as monaco somehow
    const [editor, setEditor] = useState<'monaco' | 'ace'>('monaco')
    const editorRef = useRef(null);
    const [isEditorDirty, setIsEditorDirty] = useState(false)

    const onEditorMount = editor => editorRef.current = editor

    const getEditorValue = useCallback(() => {
        if (editor === 'monaco') {
            return editorRef.current?.getValue()
        } else {
            return editorRef.current?.editor.getValue();
        }
    }, [editor])

    const saveContent = useCallback(() => {
        const draft = getEditorValue();
        if (content !== draft) {
            setSaving(true)
            onSaveContent(draft).then(() => {
                setIsEditorDirty(getEditorValue() === draft)
                setSaving(false);
            })
        }
    }, [onSaveContent, setSaving, content, getEditorValue])

    const onExit = async () => {
        await saveContent()
        onExitEditor();
    }

    useEffect(() => {
        //this hook only controls navigation outside the page. react-router navigation is covered by the Prompt component
        if (!isEditorDirty) {
            return
        }

        window.onbeforeunload = (e) => {
            e.preventDefault();
            e.returnValue = '';
        }
        return () => {
            window.onbeforeunload = null;
        }
    }, [isEditorDirty])

    useEffect(() => {
        const interval = setInterval(() => saveContent(), 1000 * 30)
        return () => clearInterval(interval)
    }, [content, saveContent])

    return (
        <>

            <Stack horizontal>
                <PrimaryButton onClick={onExit}>Exit</PrimaryButton>
                <PrimaryButton onClick={saveContent}>Save</PrimaryButton>
                <Toggle label="Wrap lines" checked={wordWrap} onChange={(_, val) => setWordWrap(val)} />
                <Toggle label="Use Monaco" checked={editor === 'monaco'} onChange={(_, val) => setEditor(val ? 'monaco' : 'ace')} />
            </Stack>
            {
                saving && <Spinner />
            }
            {
                //this is for in-app navigation with react-router
                //for browser navigation the onbeforeunload is setup in a hook
                <Prompt
                    when={isEditorDirty}
                    message='You have unsaved changes, are you sure you want to leave?'
                />
            }
            {
                editor === 'monaco' ?
                    <Editor
                        options={{wordWrap: wordWrap ? 'on' : 'off'}}
                        defaultLanguage="markdown"
                        height='80vh'
                        defaultValue={content}
                        onChange={val => setIsEditorDirty(val !== content)}
                        onMount={onEditorMount}
                    /> :
                    <AceEditor
                        ref={editorRef}
                        width={'100%'}
                        fontSize={14}
                        mode="markdown"
                        scrollMargin={[8]}
                        focus
                        highlightActiveLine
                        wrapEnabled={wordWrap}
                        tabSize={2}
                        theme="tomorrow_night_eighties"
                        showPrintMargin={false}
                        name="file-editor"
                        defaultValue={content}
                        onChange={val => setIsEditorDirty(val !== content)}
                        editorProps={{ $blockScrolling: true }}
                    />
            }
        </>
    )
}
