import {Button, Dialog} from '@material-ui/core';
import {useCallback, useState} from 'react';
import {AddPayment} from './add-payment';
import * as React from 'react';
import {Payments} from './payments';

export const FinancialsHome: React.FC = () => {
    const [showAddPayment, setShowAddPayment] = useState(false)
    const onAddPayment = useCallback(() => setShowAddPayment(!showAddPayment), [showAddPayment]);

    return (
        <>
            <Payments />
            <Button
                variant={'contained'}
                color={'primary'}
                onClick={onAddPayment}>
                Add Payment
            </Button>
            {/*<Drawer anchor={'right'} open={showAddPayment} onClose={() => setShowAddPayment(false)}>*/}
            {/*    <AddPayment />*/}
            {/*</Drawer>*/}
            <Dialog fullScreen open={showAddPayment}>
                <AddPayment onClose={() => setShowAddPayment(false)}/>
            </Dialog>
        </>
    )
}