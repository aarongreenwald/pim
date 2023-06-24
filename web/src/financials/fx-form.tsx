import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import {StockAccountId, FxTransactionDto, FxTransactionId} from '@pim/common';
import {getFxTransaction, saveFxTransaction} from '../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../common/panel.types';
import {stackTokens} from './styles';
import {CurrencyInput} from './currency-input';
import {StockAccountDropdown} from './stocks/stock-account-dropdown';
import {formatDay, formatTimeInput} from '../common/date.utils';

export const FxForm: React.FC<PanelProps<FxTransactionId>> = ({onClose, id}) => {
    const {fxTransaction, updateTransaction, updateAccount, submitForm} = useFxTransactionForm(onClose, id);

    if (!fxTransaction) return null; //TODO spinner

    return (
      <form>
        <Stack tokens={stackTokens}>
            {/* datetime-local inputs aren't good here, they want to respect the client's timezone, which is not
            what we want. Fighting it is too hard, it's easier to separate the two fields. */}
            <TextField
               label={'Date'}
               type="date"
               onChange={updateTransaction}
               value={fxTransaction.transactionDate}
               name="transactionDate"/>

            <TextField
                label={'Time'}
                type="time"
                step="0.001"
                onChange={updateTransaction}
                value={fxTransaction.transactionTime}
                name="transactionTime"/>

            <StockAccountDropdown value={fxTransaction.accountId} onChange={updateAccount}/>

            {/*
		TODO: toggle for direction, and validation that both numbers are positive. 
		In the meantime, enter numbers with opposite signs.
	      */}

            <Stack.Item>
                <CurrencyInput
                    amount={fxTransaction.usdAmount}
                    label="USD"
                    currency={'USD'}
                    name="usdAmount"
                    onChange={updateTransaction}
                />
            </Stack.Item>

            <Stack.Item>
		<CurrencyInput
                    amount={fxTransaction.ilsAmount}
                    label="ILS"
                    currency={'ILS'}
                    name="ilsAmount"
                    onChange={updateTransaction}
		/>
            </Stack.Item>

             <Stack.Item>
               <CurrencyInput
                  label="Commission"
                  amount={fxTransaction.usdCommission}
                  currency={'USD'}
                  name="usdCommission"
                  onChange={updateTransaction}
             />
             </Stack.Item>

            <TextField
                label="Notes"
                name="note"
                value={fxTransaction.note}
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

function useFxTransactionForm(onClose: () => void, fxTransactionId?: FxTransactionId) {
    const [fxTransaction, setFxTransaction] = useState<FxTransaction>(fxTransactionId ? null : initializeTransaction())

    useEffect(() => {
        if (fxTransactionId) {
            getFxTransaction(fxTransactionId)
                .then(transaction => ({
                    ...transaction,
                    transactionDate: formatDay(transaction.transactionDate),
                    transactionTime: formatTimeInput(transaction.transactionDate)
                }))
                .then(setFxTransaction)
        }
    }, [fxTransactionId])

    const updateTransaction = useCallback(({target}) => {
        setFxTransaction({
            ...fxTransaction,
            [target.name]: target.value
        })
    }, [fxTransaction])

    const updateAccount = useCallback((accountId: StockAccountId) => {
       setFxTransaction({
           ...fxTransaction,
           accountId
       })
    }, [fxTransaction]);

    const submitForm = useCallback(async () => {
        await saveFxTransaction(convertToTransactionDto(fxTransaction))
        if (fxTransaction.id === -1) {
            setFxTransaction(initializeTransaction())
        } else {
            onClose();
        }
    }, [fxTransaction, onClose])
    return {fxTransaction, updateTransaction, updateAccount, submitForm};
}

function initializeTransaction(): FxTransaction {
    const today = formatDay(new Date())
    return {
      id: -1,
      accountId: null,
      transactionDate: today,
      transactionTime: '',
      ilsAmount: null,
      usdAmount: null,
      usdCommission: null,
      note: ''
    };
}

function convertToTransactionDto(transaction: FxTransaction): FxTransactionDto {
    return {
        ...transaction,
        transactionDate: `${transaction.transactionDate}T${transaction.transactionTime || '00:00:00'}Z`
    }
}

interface FxTransaction extends Omit<FxTransactionDto, 'transactionDate'> {
    transactionDate: string;
    transactionTime: string;
}
