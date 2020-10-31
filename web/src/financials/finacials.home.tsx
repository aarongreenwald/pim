import * as React from 'react';
import {Payments} from './payments';
import { Pivot, PivotItem} from '@fluentui/react';
import {HistoryAnalysis} from './history-analysis';
import {Income} from './income';
import {CashRecordHistory} from './cash-record-history';

export const FinancialsHome: React.FC = () => {

    return (
        <>
            <Pivot> {/* overflowBehavior="menu" */}
                <PivotItem headerText="Spending">
                    <Payments />
                </PivotItem>
                <PivotItem headerText="Income">
                    <Income />
                </PivotItem>
                <PivotItem headerText="Cash Record History">
                    <CashRecordHistory />
                </PivotItem>
                <PivotItem headerText="Analysis">
                    <HistoryAnalysis />
                </PivotItem>
            </Pivot>

        </>
    )
}