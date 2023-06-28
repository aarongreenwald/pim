import {useCallback, useEffect, useRef, useState} from 'react';
import {IconButton} from '@fluentui/react';
import Editor from '@monaco-editor/react';
import * as React from 'react';
import AceEditor from 'react-ace';
import {Prompt} from 'react-router';
import {saveAndCloseIcon, saveIcon, toggleEditorIcon, wrapLinesIcon} from './icons';
import 'ace-builds/src-noconflict/mode-markdown';
import 'ace-builds/src-noconflict/theme-tomorrow_night_eighties';
import 'ace-builds/src-min-noconflict/ext-searchbox';

//see more themes here: https://ace.c9.io/build/kitchen-sink.html
//more options here: https://ace.c9.io/build/kitchen-sink.html //TODO - search

/***
 * Represents the content saved on the server per path.
 * TODO - LRU this cache. In can grow to the size of the notes repo, which currently isn't that big so this isn't urgent
 */
const savedContents = new Map<string, string>();

interface FileEditorProps {
    content: string;
    onSaveContent: (content: string) => Promise<void>;
    onExitEditor: () => void;
    path: string;
}

export const FileEditor: React.FC<FileEditorProps> = ({content, onSaveContent, onExitEditor, path}) => {
    const [saving, setSaving] = useState(false)
    const [wordWrap, setWordWrap] = useState(true)
    //TODO automatically use ace on mobile, or configure ace to be as nice as monaco somehow
    const [editor, setEditor] = useState<'monaco' | 'ace'>('ace')
    const editorRef = useRef(null);
    const [isEditorDirty, setIsEditorDirty] = useState(false)

    const onEditorMount = editor => editorRef.current = editor

    //TODO this whole thing is a mess of race conditions. If I introduce a state management system
    //this could be simplified and then the bugs can be eliminated.
    //One risk is that onSaveContent refers to a specific path, and if the editor content is not in sync with the path the onSaveContent
    //will save to, and a save is triggered - data will be lost.

    useEffect(() => {
        //after saving the contents the content updates, after switching paths the path updates
        //regardless, keep track of what content the server has for each path
        savedContents.set(path, content)
    }, [path, content])

    useEffect(() => {
        //if the path changes (user switched files while the editor is open), retrieve the value of the contents so that the editor can be updated
        //from the map set in the previous useEffect. This assumes the previous one will happen first, which is a bad assumption
        //the alternative is to take the content from the props, assuming they both arrive at the same time (I'm not sure about that)
        //but content CANNOT be a dependency of this effect or the saved contents will be inserted into the editor whenever the content is saved,
        //causing the cursor to jump in middle of typing and potentially losing the content entered during the time the save was being executed
        if (editor === 'monaco') {
            return editorRef.current?.getModel().setValue(savedContents.get(path))
        } else {
            return editorRef.current?.editor.getSession().setValue(savedContents.get(path))
        }
    }, [path, editor])

    const getEditorValue = useCallback(() => {
        if (editor === 'monaco') {
            return editorRef.current?.getValue()
        } else {
            return editorRef.current?.editor.getValue();
        }
    }, [editor])

    const saveContent = useCallback(async () => {
        const draft = getEditorValue();
        if (content !== draft) { //perhaps the comparison should be to the saved content map and not the prop?
            setSaving(true)
            await onSaveContent(draft).then(() => {
                //if the user is typing, an onchange will recalculate this anyway. But if not,
                //it's necessary to compare the current value to what was saved. draft is definitely the most reliable value
                setIsEditorDirty(getEditorValue() !== draft)
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
    }, [saveContent])

    return (
        <>
            <IconButton title={'Save and close'}
                        iconProps={saveAndCloseIcon}
                        onClick={onExit}/>
            <IconButton
                disabled={!isEditorDirty || saving}
                title={'Save'}
                iconProps={saveIcon}
                onClick={saveContent}/>
            <IconButton title="Wrap lines"
                        checked={wordWrap}
                        toggle
                        onClick={() => setWordWrap(val => !val)}
                        iconProps={wrapLinesIcon} />
            <IconButton title="Toggle editor type"
                        checked={editor === 'monaco'}
                        toggle
                        onClick={() => setEditor(val => val === 'ace' ? 'monaco' : 'ace')}
                        iconProps={toggleEditorIcon}/>
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
                        onChange={val => setIsEditorDirty(val !== content)} // compare to savedContents? probably doesn't matter
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
                        onChange={val => setIsEditorDirty(val !== content)} // compare to savedContents? probably doesn't matter
                        editorProps={{ $blockScrolling: true }}
                        commands={[
                            {
                                name: 'save',
                                bindKey: {win: 'Ctrl-s', mac: 'Command-s'},
                                exec: saveContent
                            },
                            {
                                name: 'saveandclose',
                                bindKey: {win: 'Ctrl-Enter', mac: 'Command-Enter'},
                                exec: onExit
                            },
                        ]}
                    />
            }
        </>
    )
};
