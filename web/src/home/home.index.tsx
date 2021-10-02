import * as React from 'react';
import {CommandBarButton, DefaultButton, Stack, IconButton, Panel} from '@fluentui/react';
import {RecentNotes} from '../notes/recent-notes';
import {PaymentForm} from '../financials/payment-form';
import {useBoolean} from '@uifabric/react-hooks';
import {LogFuelForm} from '../financials/log-fuel-form';
import {useCallback, useEffect, useState} from 'react';
import {getFuelLog} from '../services/server-api';
import {FuelLogDto} from '@pim/common';
import {Search} from '../notes/search';
import {fileIcon, searchIcon} from '../notes/icons';
import {useHistory} from 'react-router-dom'
import styled from '@emotion/styled';

export const Home: React.FC = () => {
    const [addPayment, {setTrue: showAddPayment, setFalse: hideAddPayment}] = useBoolean(false)
    const [addFuelLog, {setTrue: showAddFuelLog, setFalse: hideAddFuelLog}] = useBoolean(false)
    const [searchNotes, {setTrue: showSearchNotes, setFalse: hideSearchNotes}] = useBoolean(false)
    const [fuelLog, setFuelLog] = useState<FuelLogDto>(null)
    const history = useHistory();

    const reloadFuelLog = useCallback(() => {
        getFuelLog(1).then(setFuelLog)
    }, [setFuelLog])
    useEffect(reloadFuelLog, [])

    return (
        <div>
            <StyledButtons>
                <DefaultButton onClick={showAddPayment}>Add Payment</DefaultButton>
                <DefaultButton onClick={showAddFuelLog}>Add Fuel Log</DefaultButton>
            </StyledButtons>
            <div>
                <Stack horizontal styles={commandBarStackStyles}>
                    <CommandBarButton onClick={() => history.push('/notes')} iconProps={fileIcon}>View Notes</CommandBarButton>
                    <IconButton iconProps={searchIcon} onClick={showSearchNotes} title="Search Notes" />
                </Stack>
                <RecentNotes />
            </div>


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
                <LogFuelForm onClose={hideAddFuelLog} onSave={reloadFuelLog} data={fuelLog?.fuelLog[0]}/>
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
    
    > button {
      width: 100%;
      height: 100px;
    }
    
    > button :last-child {
      margin-bottom: ${SPACING}px;
    }
  }
  
  @media (min-width: 600px) {
    flex-direction: row;
    
    > button {
      margin-bottom: ${SPACING}px;
      max-width: 300px;
    }

    > button:not(:last-child) {
      margin-right: ${SPACING}px;
    }
  }
  
  > button {
    margin-top: ${SPACING}px;  
  }
`

const commandBarStackStyles = {root: {marginLeft: -SPACING, marginRight: -SPACING}};
