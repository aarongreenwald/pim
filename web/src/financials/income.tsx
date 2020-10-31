import {List} from './table';
import {useCallback, useEffect, useMemo, useState} from 'react';
import * as React from 'react';
import {getAllIncome} from '../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {useBoolean} from '@uifabric/react-hooks';
import {commandBarStyles} from './styles';
import {IncomeForm} from './income-form';
import {IncomeId, Income as IncomeModel} from '@pim/common';

export const Income: React.FC = () => {
    const [income, setIncome] = useState([]);
    const reloadData = useCallback(() => getAllIncome().then(setIncome), [])
    useEffect(() => {reloadData()}, [reloadData])

    const [addIncome, {setTrue: showAddIncome, setFalse: hideAddIncome}] = useBoolean(false)
    const commands = useCommandBarCommands(showAddIncome, reloadData);

    const [selectedItem, setSelectedItem] = useState<IncomeId>(null)
    const hideEditIncome = () => setSelectedItem(null);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            {

                income &&
                <List<IncomeModel>
                    data={income}
                    onClick={income => setSelectedItem(income.id)}
                    idField={'income_id'} />
            }
            {
                <Panel
                    isOpen={addIncome}
                    headerText="Add Income"
                    onDismiss={hideAddIncome}>
                    <IncomeForm onClose={hideAddIncome} />
                </Panel>
            }
            {
                <Panel
                    isOpen={!!selectedItem}
                    headerText="Edit Income"
                    onDismiss={hideEditIncome}>
                    <IncomeForm onClose={hideEditIncome} id={selectedItem}/>
                </Panel>
            }
        </>
    )
}

function useCommandBarCommands(onAddIncome: () => void,
                               reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
        [
            {
                split: true,
                key: 'newIncome',
                text: 'Income',
                iconProps: {iconName: 'Add'},
                onClick: onAddIncome,
            },
            {
                key: 'refresh',
                text: 'Refresh',
                iconProps: {iconName: 'Refresh'},
                onClick: reloadData
            }
        ]), [reloadData, onAddIncome])
    return commands;
}