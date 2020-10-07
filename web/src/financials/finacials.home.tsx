import {useCallback, useState} from 'react';
import {AddPayment} from './add-payment';
import * as React from 'react';
import {Payments} from './payments';
import {Panel} from '@fluentui/react';
// import useTheme from '@material-ui/core/styles/useTheme';
// import useMediaQuery from '@material-ui/core/useMediaQuery';
// import Button from '@material-ui/core/Button';
// import {Fab} from '@material-ui/core';

export const FinancialsHome: React.FC = () => {
    const [showAddPayment, setShowAddPayment] = useState(false)
    const onAddPayment = useCallback(() => setShowAddPayment(!showAddPayment), [showAddPayment]);
    const closePanel = useCallback(() => setShowAddPayment(false), [])
    // const theme = useTheme()
    // const fullScreen = useMediaQuery(theme.breakpoints.down('xs'))

    return (
        <>
            {/*<Fab*/}
            {/*    color={'primary'}*/}
            {/*    onClick={onAddPayment}>*/}
            {/*    Add Payment*/}
            {/*</Fab>*/}
            <Payments onAddPayment={onAddPayment} />
            {/*<Drawer anchor={'right'} open={showAddPayment} onClose={closePanel}>*/}
            {/*    <AddPayment onClose={closePanel}/>*/}
            {/*</Drawer>*/}
            {/*<Dialog maxWidth={'md'} fullWidth fullScreen={fullScreen} open={showAddPayment}>*/}
            {/*    <AppBar position={'relative'}>*/}
            {/*        <Toolbar>*/}
            {/*            <Typography>Add Payment</Typography>*/}
            {/*            <IconButton onClick={closePanel} color={'inherit'}>*/}
            {/*                <Close />*/}
            {/*            </IconButton>*/}
            {/*        </Toolbar>*/}
            {/*    </AppBar>*/}
            {/*    <AddPayment onClose={closePanel}/>*/}
            {/*</Dialog>*/}
            <Panel
                isOpen={showAddPayment}
                headerText="Add Payment"
                onDismiss={closePanel}>
                <AddPayment onClose={closePanel}/>
            </Panel>
        </>
    )
}