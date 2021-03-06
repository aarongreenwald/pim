import {useCallback, useEffect, useMemo, useState} from 'react';
import {getCarSummary, getCashAllocations, getUnreportedSpending} from '../services/server-api';
import {List} from './list';
import * as React from 'react';
import {CarSummary, CashAssetAllocation, UnreportedSpending} from '@pim/common';
import {useBoolean} from '@uifabric/react-hooks';
import {CommandBar, ICommandBarItemProps, Panel, Stack} from '@fluentui/react';
import {commandBarStyles} from './styles';
import {AddCashRecord} from './add-cash-record';
import {formatDay} from '../common/date.utils';

export const CashRecordHistory: React.FC = () => {
    const [carSummary, setCarSummary] = useState<CarSummary[]>([]);
    const [cashAllocations, setCashAllocations] = useState<CashAssetAllocation[]>(null)
    const [unreportedSpending, setUnreportedSpending] = useState<UnreportedSpending[]>(null);
    const reloadData = useCallback(() => {
        getCarSummary().then(setCarSummary);
        getCashAllocations().then(({cashAssetsAllocation, unallocatedCashSnapshot}) => {
            setCashAllocations([
                ...cashAssetsAllocation,
                {
                    allocationCode: 'Unallocated',
                    ils: unallocatedCashSnapshot.ils,
                    usd: unallocatedCashSnapshot.usd
                }
            ])
        })
        getUnreportedSpending().then(setUnreportedSpending);
    }, [])
    useEffect(() => {reloadData()}, [reloadData])

    const [addCar, {setTrue: showAddCar, setFalse: hideAddCar}] = useBoolean(false)
    const commands = useCommandBarCommands(showAddCar, reloadData);

    const [selectedItem, setSelectedItem] = useState<string>(null)
    const hideEditCar = () => setSelectedItem(null);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            <Stack>
                {
                    cashAllocations &&
                    <>
                        <List<CashAssetAllocation>
                            data={cashAllocations}
                            idField={'allocationCode'} />
                    </>
                }
                {
                    carSummary &&
                    <List<CarSummary>
                        data={carSummary}
                        onClick={car => setSelectedItem(car.recordDate)}
                        idField={'recordDate'} />
                }
                {
                    unreportedSpending &&
                    <List<UnreportedSpending>
                        data={unreportedSpending}
                        idField={'endDate'} />
                }
            </Stack>

            <Panel
                isOpen={addCar}
                headerText="Add Cash Assets"
                onDismiss={hideAddCar}>
                <AddCashRecord onClose={hideAddCar} />
            </Panel>

            <Panel
                isOpen={!!selectedItem}
                headerText={`Edit ${(formatDay(selectedItem))}`}
                onDismiss={hideEditCar}>
                <AddCashRecord onClose={() => setSelectedItem(null)} id={selectedItem}/>
            </Panel>

        </>
    )
}

function useCommandBarCommands(onAddCar: () => void,
                               reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
        [
            {
                split: true,
                key: 'newCar',
                text: 'Cash Assets',
                iconProps: {iconName: 'Add'},
                onClick: onAddCar,
            },
            {
                key: 'refresh',
                text: 'Refresh',
                iconProps: {iconName: 'Refresh'},
                onClick: reloadData
            }
        ]), [reloadData, onAddCar])
    return commands;
}