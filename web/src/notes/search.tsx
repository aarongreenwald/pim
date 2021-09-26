import {useDebouncedInput} from '../common/debounced-input.hook';
import * as React from 'react';
import {useEffect, useState} from 'react';
import {searchNotes} from '../services/server-api';
import {Icon, Stack, Panel, SearchBox, Toggle, FocusZone} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import {Link} from 'react-router-dom'
import styled from '@emotion/styled';

export const Search = ({show, onDismiss}) => {
    const {inputVal, debouncedValue, updateValue} = useDebouncedInput('')
    const [excludeHidden, setExcludeHidden] = useState(true);
    const [searchResults, setSearchResults] = useState(null)
    useEffect(() => {
        if (debouncedValue?.length < 3) {
            setSearchResults(null)
            return;
        }

        searchNotes(debouncedValue, excludeHidden).then(setSearchResults)
    }, [debouncedValue, excludeHidden])

    return (
        <Panel isOpen={show} 
               onDismiss={onDismiss} 
               isHiddenOnDismiss 
               onRenderHeader={() =>
                   (
                       <Stack styles={headerStackStyles}>
                           <SearchBox value={inputVal}
                                      onChange={(_, val) => updateValue(val)}/>
                           <Toggle checked={!excludeHidden}
                                   onChange={(_, val) => setExcludeHidden(!val)}
                                   label={'Include hidden'}/>
                       </Stack>
                   )}>
            <FocusZone>
            {
                searchResults?.names.map(item => <DirectoryItemSearchResult key={item.path} item={item} onDismiss={onDismiss}/>)
            }
            {
                searchResults?.contents.map((result) =>
                    <StyledSearchContentResult key={`${result.path}`}>
                        <Link to={`/notes/?path=${result.path}`} onClick={onDismiss}>{result.path}</Link>
                        {
                            result.items.map((item, i) =>
                                <div key={item.lineNumber}>
                                    <ReactMarkdown>{item.text}</ReactMarkdown>
                                    {
                                        i !== result.items.length - 1 && <hr />
                                    }
                                </div>)
                        }
                    </StyledSearchContentResult>
                )
            }
            </FocusZone>
        </Panel>
    )
}

const headerStackStyles = {
    root: {
        flexGrow: 1,
        marginLeft: 24 //hack - needs to match the panel content padding
    }
};

const DirectoryItemSearchResult = ({item, onDismiss}) => {
    return (
        <StyledSearchResult>
            <Icon iconName={'TextDocument'}/>
            <Link to={`/notes/?path=${item.path}`} onClick={onDismiss}>{item.fileName}</Link>
        </StyledSearchResult>
    )
}

const StyledSearchResult = styled.div`
  border: 1px solid gray;
  margin-bottom: 6px;
  overflow-wrap: anywhere;
  padding: 8px 6px;
  box-shadow: lightgray 1px 1px;
  border-radius: 3px;
`;

const StyledSearchContentResult = styled(StyledSearchResult)`
  padding-bottom: 0;
`