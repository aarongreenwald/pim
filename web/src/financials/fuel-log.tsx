import * as React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {FuelLog, FuelLogDto} from '@pim/common';
import {getFuelLog} from '../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {List} from './list';
import {commandBarStyles} from './styles';
import {useBoolean} from '@uifabric/react-hooks';
import {LogFuelForm} from './log-fuel-form';

export const FuelHistory: React.FC = () => {
    const [fuelLog, setFuelLog] = useState<FuelLogDto>()

    const reloadData = useCallback(() => {
        getFuelLog().then(setFuelLog)
    }, [setFuelLog])

    useEffect(() => {
        reloadData();
    }, [reloadData])

    const [addFuelLog, {setTrue: showAddFuelLog, setFalse: hideAddFuelLog}] = useBoolean(false)

    const commands = useCommandBarCommands(showAddFuelLog, reloadData);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            {

                fuelLog &&
                    <List<FuelLog>
                        data={fuelLog.fuelLog}
                        idField={'id'} />
            }
            {

                <Panel
                    isOpen={addFuelLog}
                    headerText="Log Fuel"
                    onDismiss={hideAddFuelLog}>
                    <LogFuelForm onClose={hideAddFuelLog} onSave={reloadData} data={fuelLog?.fuelLog[0]}/>
                </Panel>
            }
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
