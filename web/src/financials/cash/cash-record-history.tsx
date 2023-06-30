import {useCallback, useEffect, useMemo, useState} from 'react';
import {getCarSummary, getCashAllocations, getUnreportedSpending} from '../../services/server-api';
import {List} from '../list';
import * as React from 'react';
import {CarSummary, CashAssetAllocation, UnreportedSpending} from '@pim/common';
import {useBoolean} from '@fluentui/react-hooks';
import {CommandBar, ICommandBarItemProps, Panel, PanelType, Stack} from '@fluentui/react';
import {commandBarStyles} from '../styles';
import {AddCashRecord} from './add-cash-record';
import {formatDay} from '../../common/date.utils';
import {AddCashAssetAllocation} from './add-cash-asset-allocation';
import {CashAllocationHistory} from './cash-allocation-history';

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
    const [addCaa, {setTrue: showAddCaa, setFalse: hideAddCaa}] = useBoolean(false)
    const commands = useCommandBarCommands(showAddCar, showAddCaa, reloadData);

    const [selectedItem, setSelectedItem] = useState<string>(null)
    const hideEditCar = () => setSelectedItem(null);

    const [selectedAllocation, setSelectedAllocation] = useState<string>(null)
    const hideViewAllocationHistory = () => setSelectedAllocation(null);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            <Stack>
                {
                    cashAllocations &&
                    <>
                        <List<CashAssetAllocation>
                            data={cashAllocations}
                            onClick={alloc => setSelectedAllocation(alloc.allocationCode == 'Unallocated' ? null : alloc.allocationCode )}
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
                isOpen={addCaa}
                headerText="Update Cash Allocations"
                onDismiss={hideAddCaa}>
                <AddCashAssetAllocation onClose={hideAddCaa} />
            </Panel>

            <Panel
		type={PanelType.large}
                isOpen={!!selectedAllocation}
                headerText={selectedAllocation}
                onDismiss={hideViewAllocationHistory}>
                <CashAllocationHistory id={selectedAllocation} onClose={hideViewAllocationHistory} />
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
                               onAddCaa: () => void,
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
                split: true,
                key: 'newCaa',
                text: 'Update Allocations',
                iconProps: {iconName: 'Edit'},
                onClick: onAddCaa,
            },
            {
                key: 'refresh',
                text: 'Refresh',
                iconProps: {iconName: 'Refresh'},
                onClick: reloadData
            }
        ]), [reloadData, onAddCar, onAddCaa])
    return commands;
}
