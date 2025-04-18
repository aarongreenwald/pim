import * as React from 'react';
import {DefaultButton, Stack, Panel, Label} from '@fluentui/react';
import {RecentNotes} from '../notes/recent-notes';
import {useBoolean} from '@fluentui/react-hooks';
import {Search} from '../notes/search';
import {searchIcon} from '../notes/icons';
import {useLoadTodayDiary} from '../notes/todays-diary'
import {useHistory} from 'react-router-dom'
import styled from '@emotion/styled';
import {LogFuelForm} from '../financials/fuel-log/log-fuel-form';
import {PaymentForm} from '../financials/payments/payment-form';

export const Home: React.FC = () => {
  const [addPayment, {setTrue: showAddPayment, setFalse: hideAddPayment}] = useBoolean(false)
  const [addFuelLog, {setTrue: showAddFuelLog, setFalse: hideAddFuelLog}] = useBoolean(false)
  const [searchNotes, {setTrue: showSearchNotes, setFalse: hideSearchNotes}] = useBoolean(false)
  const history = useHistory();

  const loadTodayDiary = useLoadTodayDiary()

  return (
    <div>
      <StyledButtons>
        <DefaultButton onClick={showAddPayment}>Add Payment</DefaultButton>
        <DefaultButton onClick={showAddFuelLog}>Add Fuel Log</DefaultButton>
        <DefaultButton onClick={loadTodayDiary}>{'Today\'s Diary'}</DefaultButton>
        <Stack horizontal>
          <DefaultButton onClick={() => history.push('/notes')}>View Notes</DefaultButton>
          <DefaultButton iconProps={searchIcon} onClick={showSearchNotes}/>
        </Stack>
      </StyledButtons>
      <Label>Recent Notes</Label>
      <RecentNotes />

      <Panel
        isOpen={addPayment}
        headerText="Add Payment"
        isBlocking={false}
        onDismiss={hideAddPayment}>
        <PaymentForm onClose={hideAddPayment}/>
      </Panel>

      <Panel
        isOpen={addFuelLog}
        headerText="Log Fuel"
        onDismiss={hideAddFuelLog}>
        <LogFuelForm onClose={hideAddFuelLog}/>
      </Panel>

      <Search show={searchNotes} onDismiss={hideSearchNotes}/>

    </div>
  )
}

const SPACING = 8;
const StyledButtons = styled.div`
  display: flex;
  @media (max-width: 600px) {
    flex-direction: column;
    
    > button, div {
      width: 100%;
      height: 100px;
    }
    
    > button:last-child,div:last-child {
      margin-bottom: ${SPACING}px;
    }
    
    > div > button {
      height: 100%;
    }

    > div > button:first-child {
      flex: 1;
    }
  }
  
  @media (min-width: 600px) {
    flex-direction: row;
    
    > button {
      margin-bottom: ${SPACING}px;
      max-width: 300px;
    }

    > button,div:not(:last-child) {
      margin-right: ${SPACING}px;
    }
  }
  
  > button, div {
    margin-top: ${SPACING}px;  
  }
`
