import * as React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {FuelLogDto, PaymentId} from '@pim/common';
import {getFuelLog} from '../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel, List as FluentList, Text} from '@fluentui/react';
import {commandBarStyles} from './styles';
import {useBoolean} from '@uifabric/react-hooks';
import {LogFuelForm} from './log-fuel-form';
import {FuelLogCard} from './fuel-log-card';
import {PaymentForm} from './payment-form';

export const FuelHistory: React.FC = () => {
    const [fuelLog, setFuelLog] = useState<FuelLogDto>()

    const reloadData = useCallback(() => {
        getFuelLog().then(setFuelLog)
    }, [setFuelLog])

    useEffect(() => {
        reloadData();
    }, [reloadData])

    const [addFuelLog, {setTrue: showAddFuelLog, setFalse: hideAddFuelLog}] = useBoolean(false)
    const [selectedPayment, setSelectedPayment] = useState<PaymentId>(null)
    const hideEditPayment = () => setSelectedPayment(null);

    const commands = useCommandBarCommands(showAddFuelLog, reloadData);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            {
                fuelLog && <FluentList
                    items={fuelLog.fuelLog}
                    onRenderCell={f =>
                        <Text>
                            <FuelLogCard fuelLog={f}
                                         onViewPayment={() => setSelectedPayment(f.paymentId)}/>
                        </Text>
                    }
                />

            }

            {/*{*/}

            {/*    fuelLog &&*/}
            {/*        <List<FuelLog>*/}
            {/*            data={fuelLog.fuelLog}*/}
            {/*            idField={'id'} />*/}
            {/*}*/}

            <Panel
                isOpen={addFuelLog}
                headerText="Log Fuel"
                onDismiss={hideAddFuelLog}>
                <LogFuelForm onClose={hideAddFuelLog} onSave={reloadData} data={fuelLog?.fuelLog[0]}/>
            </Panel>

            <Panel
                isOpen={!!selectedPayment}
                headerText="Edit Payment"
                onDismiss={hideEditPayment}>
                <PaymentForm onClose={hideEditPayment} id={selectedPayment}/>
            </Panel>
        </>

    )
}


function useCommandBarCommands(onAddFuelLog: () => void,
                               reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
        [
            {
                split: true,
                key: 'addFuelLog',
                text: 'Log Fuel',
                iconProps: {iconName: 'Add'},
                onClick: onAddFuelLog,
            },
            {
                key: 'refresh',
                text: 'Refresh',
                iconProps: {iconName: 'Refresh'},
                onClick: reloadData
            }
        ]), [reloadData, onAddFuelLog])
    return commands;
}
