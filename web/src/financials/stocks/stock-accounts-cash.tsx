import {List} from '../list';
import {useCallback, useEffect, useMemo, useState} from 'react';
import * as React from 'react';
import {getStockAccountCashBalances, getStockAccountCashFlow} from '../../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {useBoolean} from '@fluentui/react-hooks';
import {commandBarStyles} from '../styles';
import {StocksForm} from './stocks-form';
import {FxForm} from '../fx/fx-form';
import {CashStockAccountFundsTransferForm} from './cash-stock-account-funds-transform-form';
import {StockAccountCashBalance, StockAccountId, StockAccountCashFlow} from '@pim/common';
import {StockAccountCashTransactionForm} from './stock-account-cash-form';

export const StockAccountsCash: React.FC = () => {
  const [stockAccountsCashBalances, setStockAccountsCashBalances] = useState<StockAccountCashBalance[]>([]);
  const [stockAccountCashFlow, setStockAccountCashFlow] = useState<StockAccountCashFlow[]>([]);
  const [stockAccountId, setStockAccountId] = useState<StockAccountId>(null);
  const reloadData = useCallback(() => {
    getStockAccountCashBalances().then(setStockAccountsCashBalances)
  }, [])
  useEffect(() => {reloadData()}, [reloadData])

  useEffect(() => {
    if (stockAccountId) {
      getStockAccountCashFlow(stockAccountId).then(setStockAccountCashFlow)
    } else {
      setStockAccountCashFlow([])
    }
  }, [stockAccountId])

  const [transferFunds, {setTrue: showTransferFunds, setFalse: hideTransferFunds}] = useBoolean(false)
  const commands = useCommandBarCommands(showTransferFunds, reloadData);

  const [selectedItem, setSelectedItem] = useState<{recordId: number; recordType: string}>(null) //TODO type
  const hideEditItem = () => setSelectedItem(null);

  return (
    <>
      <CommandBar items={commands} styles={commandBarStyles}/>
      {
        stockAccountsCashBalances &&
          <List<StockAccountCashBalance>
            onClick={account => setStockAccountId(account.id)}
            data={stockAccountsCashBalances} />
      }
      {
        /* TODO add id field for performance, but it needs to be a compound field.*/
        stockAccountCashFlow &&
          <List<StockAccountCashFlow>
            data={stockAccountCashFlow}
            onClick={({recordType, recordId}) => setSelectedItem({recordId, recordType})} /> 
      }
      {
        <Panel
          isOpen={selectedItem && selectedItem.recordType == 'stock_transaction'}
          headerText="Edit Stock Transaction"
          onDismiss={hideEditItem}>
          <StocksForm onClose={hideEditItem} id={selectedItem && selectedItem.recordId}/>
        </Panel>
      }
      {
        <Panel
          isOpen={selectedItem && selectedItem.recordType == 'fx_transaction'}
          headerText="Edit FX Transaction"
          onDismiss={hideEditItem}>
          <FxForm onClose={hideEditItem} id={selectedItem && selectedItem.recordId}/>
        </Panel>
      }
      {
        <Panel
          isOpen={selectedItem && selectedItem.recordType === 'cash_flow'}
          headerText="Edit Stock Account Cash Transaction"
          onDismiss={hideEditItem}>
          <StockAccountCashTransactionForm onClose={hideEditItem} id={selectedItem && selectedItem.recordId} />
        </Panel>
      }
      {
        /* TODO support editing other record types from here as well */
      }
      {
        <Panel
          isOpen={transferFunds}
          headerText="Transfer Cash"
          onDismiss={hideTransferFunds}>
          <CashStockAccountFundsTransferForm onClose={hideTransferFunds} />
        </Panel>
      }
    </>
  )
}

function useCommandBarCommands(onTransferFunds: () => void,
  reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
      [
        {
          split: true,
          key: 'newCashStockAccountFundsTransfer',
          text: 'Transfer Funds',
          iconProps: {iconName: 'Add'}, //TODO a better icon? 
          onClick: onTransferFunds,
        },
        {
          key: 'refresh',
          text: 'Refresh',
          iconProps: {iconName: 'Refresh'},
          onClick: reloadData
        }
      ]), [reloadData, onTransferFunds])
    return commands;
  }
