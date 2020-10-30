import * as React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {PaymentId, vPayment} from '@pim/common';
import {getPayments} from '../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {List, SortConfig, SortDirection} from './table';
import {PaymentForm} from './payment-form';

interface PaymentsProps {
    onAddPayment: () => void;
    onAddIncome: () => void;
    onAddCar: () => void;
}

export const Payments: React.FC<PaymentsProps> = ({onAddPayment, onAddCar, onAddIncome}) => {
    const {
        payments,
        sortConfig,
        onSortPayments,
        reloadData
    } = useSortablePayments();
    const commands = useCommandBarCommands(onAddPayment, onAddIncome, onAddCar, reloadData);

    const [selectedItem, setSelectedItem] = useState<PaymentId>(null)
    const hideEditPayment = () => setSelectedItem(null);

    return (
        <>
            <CommandBar items={commands} styles={commandBarStyles}/>
            {

                payments &&
                    <List
                        data={payments}
                        onClick={(item: vPayment) => setSelectedItem(item.id)}
                        sortConfig={sortConfig}
                        sortData={onSortPayments}
                        idField={'id'} />
            }
            {
                <Panel
                    isOpen={!!selectedItem}
                    headerText="Edit Payment"
                    onDismiss={hideEditPayment}>
                    <PaymentForm onClose={hideEditPayment} paymentId={selectedItem}/>
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
                               onAddIncome: () => void,
                               onAddCar: () => void,
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
        ]), [reloadData, onAddPayment, onAddIncome, onAddCar])
    return commands;
}

const commandBarStyles = {root: {padding: 0}};

const defaultPaymentsSortConfig = {fieldName: 'paidDate', direction: SortDirection.desc};