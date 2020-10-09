import {AddPayment} from './add-payment';
import * as React from 'react';
import {Payments} from './payments';
import {Panel} from '@fluentui/react';
import {useBoolean} from '@uifabric/react-hooks';
import {AddCashRecord} from './add-cash-record';
import {AddIncome} from './add-income';

export const FinancialsHome: React.FC = () => {
    const [addIncome, {setTrue: showAddIncome, setFalse: hideAddIncome}] = useBoolean(false)
    const [addCar, {setTrue: showAddCar, setFalse: hideAddCar}] = useBoolean(false)
    const [addPayment, {setTrue: showAddPayment, setFalse: hideAddPayment}] = useBoolean(false)

    return (
        <>
            <Payments
                onAddPayment={showAddPayment}
                onAddIncome={showAddIncome}
                onAddCar={showAddCar}/>
            <Panel
                isOpen={addPayment}
                headerText="Add Payment"
                onDismiss={hideAddPayment}>
                <AddPayment onClose={hideAddPayment}/>
            </Panel>
            <Panel
                isOpen={addCar}
                headerText="Add Cash Record"
                onDismiss={hideAddCar}>
                <AddCashRecord onClose={hideAddCar}/>
            </Panel>
            <Panel
                isOpen={addIncome}
                headerText="Add Income"
                onDismiss={hideAddIncome}>
                <AddIncome onClose={hideAddIncome}/>
            </Panel>
        </>
    )
}