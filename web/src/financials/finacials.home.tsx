import {PaymentForm} from './payment-form';
import * as React from 'react';
import {Payments} from './payments';
import { Label, Panel, Pivot, PivotItem} from '@fluentui/react';
import {useBoolean} from '@uifabric/react-hooks';
import {AddCashRecord} from './add-cash-record';
import {AddIncome} from './add-income';
import {useEffect} from 'react';
import {getCarSummary, getIncome} from '../services/server-api';
import {HistoryAnalysis} from './history-analysis';

export const FinancialsHome: React.FC = () => {
    const [addIncome, {setTrue: showAddIncome, setFalse: hideAddIncome}] = useBoolean(false)
    const [addCar, {setTrue: showAddCar, setFalse: hideAddCar}] = useBoolean(false)
    const [addPayment, {setTrue: showAddPayment, setFalse: hideAddPayment}] = useBoolean(false)

    useEffect(() => {getCarSummary().then(console.log)}, [])
    useEffect(() => {getIncome().then(console.log)}, [])
    return (
        <>
            <Pivot>
                <PivotItem headerText="Spending">
                    <Payments
                        onAddPayment={showAddPayment}
                        onAddIncome={showAddIncome}
                        onAddCar={showAddCar}/>
                </PivotItem>
                <PivotItem headerText="Income">
                    <Label>Income: TODO</Label>
                </PivotItem>
                <PivotItem headerText="Cash Record History">
                    <Label>CAR: TODO</Label>
                </PivotItem>
                <PivotItem headerText="Analysis">
                    <HistoryAnalysis />
                </PivotItem>
            </Pivot>

            <Panel
                isOpen={addPayment}
                headerText="Add Payment"
                onDismiss={hideAddPayment}>
                <PaymentForm onClose={hideAddPayment}/>
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