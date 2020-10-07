import {useCallback, useState} from 'react';
import {AddPayment} from './add-payment';
import * as React from 'react';
import {Payments} from './payments';
import {Panel} from '@fluentui/react';

export const FinancialsHome: React.FC = () => {
    const [showAddPayment, setShowAddPayment] = useState(false)
    const onAddPayment = useCallback(() => setShowAddPayment(!showAddPayment), [showAddPayment]);
    const closePanel = useCallback(() => setShowAddPayment(false), [])

    return (
        <>
            <Payments onAddPayment={onAddPayment} />
            <Panel
                isOpen={showAddPayment}
                headerText="Add Payment"
                onDismiss={closePanel}>
                <AddPayment onClose={closePanel}/>
            </Panel>
        </>
    )
}