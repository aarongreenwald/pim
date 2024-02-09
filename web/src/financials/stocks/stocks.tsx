import {List} from '../list';
import {useCallback, useEffect, useMemo, useState} from 'react';
import * as React from 'react';
import {getAllStocks, getStockHoldingsSummary} from '../../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {useBoolean} from '@fluentui/react-hooks';
import {commandBarStyles} from '../styles';
import {StocksForm} from './stocks-form';
import {StockTransactionId, StockTransactionDto, StockHoldingSummaryDto} from '@pim/common';

export const Stocks: React.FC = () => {
  const [stocks, setStocks] = useState([]);
  const [stockHoldingsSummary, setStockHoldingsSummary] = useState<StockHoldingSummaryDto[]>([])
  const reloadData = useCallback(() => {
    getAllStocks().then(setStocks)
    getStockHoldingsSummary().then(setStockHoldingsSummary)
  }, [])
  useEffect(() => {reloadData()}, [reloadData])

  const [addStockTransaction, {setTrue: showAddStockTransaction, setFalse: hideAddStockTransaction}] = useBoolean(false)
  const commands = useCommandBarCommands(showAddStockTransaction, reloadData);

  const [selectedItem, setSelectedItem] = useState<StockTransactionId>(null)
  const hideEditStockTransaction = () => setSelectedItem(null);

  return (
    <>
      <CommandBar items={commands} styles={commandBarStyles}/>
      {
	stockHoldingsSummary &&
          <List<StockHoldingSummaryDto>
            data={stockHoldingsSummary} />
      }
      {

        stocks &&
          <List<StockTransactionDto>
            data={stocks}
            onClick={transaction => setSelectedItem(transaction.id)}
            idField={'id'} />
      }
      {
        <Panel
          isOpen={addStockTransaction}
          headerText="Add Stock Transaction"
          onDismiss={hideAddStockTransaction}>
          <StocksForm onClose={hideAddStockTransaction} />
        </Panel>
      }
      {
        <Panel
          isOpen={!!selectedItem}
          headerText="Edit Stock Transaction"
          onDismiss={hideEditStockTransaction}>
          <StocksForm onClose={hideEditStockTransaction} id={selectedItem}/>
        </Panel>
      }
    </>
  )
}

function useCommandBarCommands(onAddStockTransaction: () => void,
  reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
      [
        {
          split: true,
          key: 'newStockTransaction',
          text: 'Stock Transaction',
          iconProps: {iconName: 'Add'},
          onClick: onAddStockTransaction,
        },
        {
          key: 'refresh',
          text: 'Refresh',
          iconProps: {iconName: 'Refresh'},
          onClick: reloadData
        }
      ]), [reloadData, onAddStockTransaction])
    return commands;
  }
