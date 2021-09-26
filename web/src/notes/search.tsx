import {useDebouncedInput} from '../common/debounced-input.hook';
import * as React from 'react';
import {useEffect, useState} from 'react';
import {searchNotes} from '../services/server-api';
import {Icon, Stack, Panel, SearchBox, Toggle, FocusZone} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import {Link} from 'react-router-dom'
import styled from '@emotion/styled';
import { NeutralColors, CommunicationColors } from '@fluentui/theme';

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
                    <StyledSearchContentResult key={`${result.path}`} to={`/notes/?path=${encodeURIComponent(result.path)}`} onClick={onDismiss}>
                        <div className="search-result-card">
                            <StyledSearchCardHeader>{result.path}</StyledSearchCardHeader>
                            <StyledSearchCardBody>
                            {
                                result.items.map((item, i) =>
                                    <div key={item.lineNumber}>
                                        <ReactMarkdown>{item.text}</ReactMarkdown>
                                        {
                                            i !== result.items.length - 1 && <hr />
                                        }
                                    </div>)
                            }
                            </StyledSearchCardBody>
                        </div>
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
        <StyledSearchResult to={`/notes/?path=${encodeURIComponent(item.path)}`} onClick={onDismiss}>
            <div className="search-result-card">
                <Icon iconName={'TextDocument'}/> {item.fileName}
            </div>
        </StyledSearchResult>
    )
}

const CARD_SPACING = '6px';
const hoverBoxShadow = `8px 4px 6px -3px ${NeutralColors.gray120}`;

const StyledSearchResult = styled(Link) `
  text-decoration: none;

  div.search-result-card {
    border: 1px solid ${NeutralColors.gray120};
    margin-bottom: ${CARD_SPACING};
    overflow-wrap: anywhere;
    box-shadow: 4px 2px 6px -3px ${NeutralColors.gray120};
    border-radius: 4px;  
    color: ${NeutralColors.black};
    padding: ${CARD_SPACING};
    
    :hover {
      box-shadow: ${hoverBoxShadow};
      transform: scale(1.02);
    }
  }
  
  :focus > div.search-result-card {
    box-shadow: ${hoverBoxShadow};
    transform: scale(1.02);
  }
`;

const StyledSearchCardHeader = styled.div`
  padding: ${CARD_SPACING};
  background: ${CommunicationColors.tint40};
  border-bottom: 1px solid ${NeutralColors.gray120};
`

const StyledSearchCardBody = styled.div`
  padding: ${CARD_SPACING};
`

const StyledSearchContentResult = styled(StyledSearchResult)`
  padding-bottom: 0;
  
  div.search-result-card {
    padding: 0;
  }
`