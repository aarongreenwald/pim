import styled from '@emotion/styled';
import {ChoiceGroup} from '@fluentui/react';

export const commandBarStyles = {root: {padding: 0}};
export const horizontalChoiceGroup = { flexContainer: { display: 'flex' } };
export const stackTokens = {childrenGap: 8};

export const StyledChoiceGroup = styled(ChoiceGroup)`  
  & label {
    margin-right: 8px;
  }
`