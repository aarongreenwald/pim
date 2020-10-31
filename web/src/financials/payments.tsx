import * as React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {PaymentId, vPayment} from '@pim/common';
import {getPayments} from '../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {List, SortConfig, SortDirection} from './list';
import {PaymentForm} from './payment-form';
import {commandBarStyles} from './styles';
import {useBoolean} from '@uifabric/react-hooks';

export const Payments: React.FC = () => {
    const {
        payments,
        sortConfig,
        onSortPayments,
        reloadData
    } = useSortablePayments();
    const [addPayment, {setTrue: showAddPayment, setFalse: hideAddPayment}] = useBoolean(false)

    const commands = useCommandBarCommands(showAddPayment, reloadData);

    const [selectedItem, setSelectedItem] = useState<PaymentId>(null)
    const hideEditPayment = () => setSelectedItem(null);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            {

                payments &&
                    <List<vPayment>
                        data={payments}
                        onClick={(item) => setSelectedItem(item.id)}
                        sortConfig={sortConfig}
                        sortData={onSortPayments}
                        idField={'id'} />
            }
            {

                <Panel
                    isOpen={addPayment}
                    headerText="Add Payment"
                    onDismiss={hideAddPayment}>
                    <PaymentForm onClose={hideAddPayment}/>
                </Panel>
            }
            {
                <Panel
                    isOpen={!!selectedItem}
                    headerText="Edit Payment"
                    onDismiss={hideEditPayment}>
                    <PaymentForm onClose={hideEditPayment} id={selectedItem}/>
                </Panel>
            }
        </>

    )
}

/*
    Much of this can be generalized for use outside of payments,
    the sorting logic could be part of the table and the reload
    is only necessary because I don't have a store
 */
function useSortablePayments() {
    const [payments, setPayments] = useState<vPayment[]>()
    const [sortConfig, setSortConfig] = useState<SortConfig>(defaultPaymentsSortConfig)

    const onSortPayments = useCallback((config: SortConfig) => {
        setSortConfig(config)
    }, [])

    const reloadData = useCallback(() => {
        getPayments()
            .then(data => sortPayments(data, sortConfig))
            .then(setPayments)
    }, [sortConfig])

    useEffect(() => {
        if (payments && sortConfig) {
            setPayments(sortPayments(payments, sortConfig))
        }
        //payments is NOT a dependency of this effect or we'll get an infinite loop
        //the only way payments is modified outside of sorting is on load/refresh
        //and the sorting is done there separately
    }, [sortConfig]) //eslint-disable-line react-hooks/exhaustive-deps


    useEffect(() => {
        getPayments()
            .then(data => sortPayments(data, defaultPaymentsSortConfig))
            .then(setPayments)
    }, [])
    return {payments, sortConfig, onSortPayments, reloadData};
}

const sortPayments = (payments: vPayment[], sortConfig: SortConfig) => {
    const newPayments = [...payments];
    const sortFn =
        sortConfig.direction === SortDirection.asc ?
            (a, b) => a[sortConfig.fieldName] > b[sortConfig.fieldName] ? 1 : -1 :
            (a, b) => a[sortConfig.fieldName] < b[sortConfig.fieldName] ? 1 : -1

    newPayments.sort(sortFn)

    return newPayments;
}

function useCommandBarCommands(onAddPayment: () => void,
                               reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
        [
            {
                split: true,
                key: 'newPayment',
                text: 'Payment',
                iconProps: {iconName: 'Add'},
                onClick: onAddPayment,
                // subMenuProps: {
                //     items: [
                //         {
                //             key: 'newIncome',
                //             text: 'Income',
                //             onClick: onAddIncome,
                //             iconProps: { iconName: 'Money' },
                //         },
                //         {
                //             key: 'newCashAssets',
                //             text: 'Cash balances',
                //             onClick: onAddCar,
                //             iconProps: { iconName: 'AddToShoppingList' },
                //         },
                //     ]
                // },
            },
            {
                key: 'refresh',
                text: 'Refresh',
                iconProps: {iconName: 'Refresh'},
                onClick: reloadData
            }
        ]), [reloadData, onAddPayment])
    return commands;
}

const defaultPaymentsSortConfig = {fieldName: 'paidDate', direction: SortDirection.desc};