import {List} from '../list';
import {useCallback, useEffect, useMemo, useState} from 'react';
import * as React from 'react';
import {getFxHistory} from '../../services/server-api';
import {CommandBar, ICommandBarItemProps, Panel} from '@fluentui/react';
import {useBoolean} from '@fluentui/react-hooks';
import {commandBarStyles} from '../styles';
import {FxForm} from './fx-form';
import {FxTransactionId, vFxHistory} from '@pim/common';

//TODO there's a lot of overlap between this component and Stocks - consider a way to unify for easier reuse.
//Payment and Income can probably share as well - all are views over data with add/edit on records that are not
//strictly represented by the view. 
export const Fx: React.FC = () => {
  const [fxHistory, setFxHistory] = useState([]);
  const reloadData = useCallback(() => getFxHistory().then(setFxHistory), [])
  useEffect(() => {reloadData()}, [reloadData])
    
  const [addFxTransaction, {setTrue: showAddFxTransaction, setFalse: hideAddFxTransaction}] = useBoolean(false);
  const commands = useCommandBarCommands(showAddFxTransaction, reloadData);
    
  const [selectedItem, setSelectedItem] = useState<FxTransactionId>(null);
  const hideEditFxTransaction = () => setSelectedItem(null);

  return (
    <>
      <CommandBar items={commands} styles={commandBarStyles}/>
      {

        fxHistory &&
          <List<vFxHistory>
            data={fxHistory}
            onClick={transaction => setSelectedItem(transaction.id)}
            idField={'id'} />
      }
      {
        <Panel
          isOpen={addFxTransaction}
          headerText="Add FX Transaction"
          onDismiss={hideAddFxTransaction}>
          <FxForm onClose={hideAddFxTransaction} />
        </Panel>
      }
      {
        <Panel
          isOpen={!!selectedItem}
          headerText="Edit FX Transaction"
          onDismiss={hideEditFxTransaction}>
          <FxForm onClose={hideEditFxTransaction} id={selectedItem}/>
        </Panel>
      }
    </>
  )
}

function useCommandBarCommands(onAddFxTransaction: () => void,
  reloadData: () => void): ICommandBarItemProps[] {
    const commands = useMemo(() => (
      [
        {
          split: true,
          key: 'newFxTransaction',
          text: 'FX Transaction',
          iconProps: {iconName: 'Add'},
          onClick: onAddFxTransaction,
        },
        {
          key: 'refresh',
          text: 'Refresh',
          iconProps: {iconName: 'Refresh'},
          onClick: reloadData
        }
      ]), [reloadData, onAddFxTransaction])
    return commands;
  }
