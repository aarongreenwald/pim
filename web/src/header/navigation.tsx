import * as React from 'react';
import {Nav} from '@fluentui/react';
import {SwipeableDrawer} from '@material-ui/core';
import {Route, Switch} from 'react-router-dom';
import {CashRecordHistory} from '../financials/cash/cash-record-history';
import {FuelHistory} from '../financials/fuel-log/fuel-log';
import {HistoryAnalysis} from '../financials/history-analysis';
import {Notes} from '../notes/notes.index';
import {Home} from '../home/home.index';
import {Income} from '../financials/income/income';
import {Payments} from '../financials/payments/payments';
import {Stocks} from '../financials/stocks/stocks';
import {StockAccountsCash} from '../financials/stocks/stock-accounts-cash';
import {Fx} from '../financials/fx/fx';

interface NavigationProps {
    hideNav: () => void;
    showNav: () => void;
    shouldShowNav: boolean;
}
export const Navigation: React.FC<NavigationProps> = ({hideNav, showNav, shouldShowNav}) => {
    return (
        <>
            <SwipeableDrawer onClose={hideNav} onOpen={showNav} open={shouldShowNav}>
                <Nav groups={navGroups}
                     onLinkClick={hideNav}
                     styles={navStyles}/>
            </SwipeableDrawer>
            <Switch>
                <Route path="/spending" component={Payments}/>
                <Route path="/income" component={Income}/>
		<Route path="/stocks" component={Stocks}/>
                <Route path="/stock-accounts" component={StockAccountsCash}/>
		<Route path="/fx" component={Fx}/>
                <Route path="/cash" component={CashRecordHistory}/>
                <Route path="/fuel" component={FuelHistory}/>
                <Route path="/analysis" component={HistoryAnalysis}/>
                <Route path="/notes" component={Notes}/>
                <Route path="/" component={Home}/>
            </Switch>
        </>
    )
}

const navGroups = [
    {
        name: '',
        links: [
            {
                key: 'home',
                name: 'Home',
                url: '#/'
            },
            {
                key: 'notes',
                name: 'Notes',
                url: '#/notes'
            }
        ]
    },
    {
        name: 'Financials',
        links: [
            {
                key: 'spending',
                name: 'Spending',
                url: '#/spending'
            },
            {
                key: 'income',
                name: 'Income',
                url: '#/income'
            },
            {
                key: 'stocks',
                name: 'Stocks',
                url: '#/stocks'
            },
            {
		key: 'stock-accounts',
		name: 'Stock Accounts',
		url: '#/stock-accounts'
            },
            {
                key: 'fx',
                name: 'FX Transactions',
                url: '#/fx'
            },
            {
                key: 'cash',
                name: 'Cash',
                url: '#/cash'
            },
            {
                key: 'fuel',
                name: 'Fuel Log',
                url: '#/fuel'
            },
            {
                key: 'analysis',
                name: 'Analysis',
                url: '#/analysis'
            }
        ]
    }
]
const navStyles = {
    root: {
        minWidth: 300
    },
    groupContent: {
        marginBottom: 0
    }
};

