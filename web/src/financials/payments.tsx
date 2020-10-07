import * as React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {Payment} from '@pim/common';
import {getPayments} from '../services/server-api';
import {CommandBar} from '@fluentui/react';
import {List, SortConfig, SortDirection} from './table';

interface PaymentsProps {
    onAddPayment: () => void;
}

export const Payments: React.FC<PaymentsProps> = ({onAddPayment}) => {

    const [payments, setPayments] = useState<Payment[]>()
    useEffect(() => {
        getPayments().then(setPayments)
    }, [])

    const sortPayments = useCallback((sort: SortConfig) => {
        const newPayments = [...payments];
        const sortFn =
            sort.direction === SortDirection.asc ?
                (a, b) => a[sort.fieldName] > b[sort.fieldName] ? 1 : -1 :
                (a, b) => a[sort.fieldName] < b[sort.fieldName] ? 1 : -1

        newPayments.sort(sortFn)

        setPayments(newPayments)
    }, [payments])

    const reloadData = useCallback(() => {
        getPayments().then(setPayments)
    }, [])

    //maybe use CommandBarButton directly, or a CommandButton
    const commands = useMemo(() => (
        [
            {
                key: 'new',
                text: 'New Payment',
                iconProps: { iconName: 'Add' },
                onClick: onAddPayment,
            },
            {
                key: 'refresh',
                text: 'Refresh',
                iconProps: { iconName: 'Refresh' },
                onClick: reloadData
            }
        ]), [reloadData, onAddPayment])

    return (
        <>
            <CommandBar items={commands} />
            {

                payments &&
                    <List data={payments}
                        sortData={sortPayments}
                        idField={'payment_id'} />
            }
        </>

    )
}
