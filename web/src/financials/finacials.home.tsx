import {Button} from '@material-ui/core';
import {useCallback, useState} from 'react';
import {AddPayment} from './add-payment';
import * as React from 'react';
import {Payments} from './payments';
import {Panel, initializeIcons} from '@fluentui/react';

initializeIcons()

export const FinancialsHome: React.FC = () => {
    const [showAddPayment, setShowAddPayment] = useState(false)
    const onAddPayment = useCallback(() => setShowAddPayment(!showAddPayment), [showAddPayment]);
    const closePanel = useCallback(() => setShowAddPayment(false), [])

    return (
        <>
            <Payments />
            <Button
                variant={'contained'}
                color={'primary'}
                onClick={onAddPayment}>
                Add Payment
            </Button>
            {/*<Drawer anchor={'right'} open={showAddPayment} onClose={closePanel}>*/}
            {/*    <AddPayment />*/}
            {/*</Drawer>*/}
            {/*<Dialog fullScreen open={showAddPayment}>*/}
            {/*    <AddPayment onClose={closePanel}/>*/}
            {/*</Dialog>*/}
            <Panel
                isOpen={showAddPayment}
                onDismiss={closePanel}>
                <AddPayment onClose={closePanel}/>
            </Panel>
        </>
    )
}