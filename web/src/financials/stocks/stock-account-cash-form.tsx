import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import {StockAccountCashTransaction, StockAccountCashTransactionId, StockAccountId} from '@pim/common';
import {saveStockAccountCashTransaction, getStockAccountCashTransaction} from '../../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../../common/panel.types';
import {CurrencyInput} from '../currency-input';
import {StockAccountDropdown} from './stock-account-dropdown';
import {expandISODate, collapseISODate} from '../../common/date.utils';
import {currencyRadioOptions} from '../currencies';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from '../styles';

export const StockAccountCashTransactionForm: React.FC<PanelProps<StockAccountCashTransactionId>> = ({onClose, id}) => {
  const {transaction, updateTransaction, updateAccount, updateCurrency, submitForm} = useForm(onClose, id);

  if (!transaction) return null;

  return (
    <form>
      <Stack tokens={stackTokens}>
        <TextField
          label={'Date'}
          type="date"
          onChange={updateTransaction}
          value={transaction.transactionDate ? expandISODate(transaction.transactionDate) : undefined}
          name="transactionDate"/>

        <StockAccountDropdown value={transaction.accountId} onChange={updateAccount}/>

        <StyledChoiceGroup
          selectedKey={transaction.currency}
          label="Currency"
          styles={horizontalChoiceGroup}
          onChange={updateCurrency}
          options={currencyRadioOptions}/>

        <CurrencyInput
          amount={transaction.amount}
          currency={transaction.currency}
          name="amount"
          onChange={updateTransaction}
        />

        <TextField
          label="Notes"
          name="note"
          value={transaction.note}
          multiline
          onChange={updateTransaction}/>

        <Stack horizontal tokens={stackTokens}>
          <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
          <DefaultButton onClick={onClose}>Cancel</DefaultButton>
        </Stack>

      </Stack>
    </form>
  )
}

function useForm(onClose: () => void, id: StockAccountCashTransactionId) {

  
  const [transaction, setTransaction] = useState<StockAccountCashTransaction>(null)

  useEffect(() => {
    if (id) {
      getStockAccountCashTransaction(id).then(setTransaction)
    }
    //TODO cleanup - if the panel is closed before the request finishes an error is thrown
  }, [id])

  
  const updateTransaction = useCallback(({target}) => {
    setTransaction({
      ...transaction,
      [target.name]: target.type == 'date' ? collapseISODate(target.value)  : target.value
    })
  }, [transaction])

  const updateAccount = useCallback((accountId: StockAccountId) => {
    setTransaction({
      ...transaction,
      accountId
    })
  }, [transaction]);

  
  const updateCurrency = useCallback((_, {key}) => {
    setTransaction({
      ...transaction,
      currency: key
    })
  }, [transaction])

  
  const submitForm = useCallback(async () => {
    await saveStockAccountCashTransaction(transaction)
    onClose()
  }, [transaction, onClose])
  return {transaction, updateTransaction, updateCurrency, updateAccount, submitForm};
}
