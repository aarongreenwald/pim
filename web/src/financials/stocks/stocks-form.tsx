import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import {StockAccountId, StockTransactionDto, StockTransactionId} from '@pim/common';
import {getStockTransaction, saveStockTransaction} from '../../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField, Label} from '@fluentui/react'
import {PanelProps} from '../../common/panel.types';
import {stackTokens} from '../styles';
import {CurrencyInput} from '../currency-input';
import {StockAccountDropdown} from './stock-account-dropdown';
import {formatDay, formatTimeInput} from '../../common/date.utils';
import { currencySymbols } from '../currencies';

export const StocksForm: React.FC<PanelProps<StockTransactionId>> = ({onClose, id}) => {
    const {stockTransaction, updateTransaction, updateAccount, submitForm} = useStockTransactionForm(onClose, id);

    if (!stockTransaction) return null; //TODO spinner

    return (
      <form>
        <Stack tokens={stackTokens}>
            {/* datetime-local inputs aren't good here, they want to respect the client's timezone, which is not
            what we want. Fighting it is too hard, it's easier to separate the two fields. */}
            <TextField
               label={'Date'}
               type="date"
               onChange={updateTransaction}
               value={stockTransaction.transactionDate}
               name="transactionDate"/>

            <TextField
                label={'Time'}
                type="time"
                step="0.001"
                onChange={updateTransaction}
                value={stockTransaction.transactionTime}
                name="transactionTime"/>

            <StockAccountDropdown value={stockTransaction.accountId} onChange={updateAccount}/>

            <TextField
                label="Ticker"
                name="tickerSymbol"
                value={stockTransaction.tickerSymbol}
                onChange={updateTransaction}
            />

            <Stack horizontal tokens={stackTokens}>
                <Stack.Item grow>
                    <TextField
                        label="Quantity"
                        name="quantity"
                        type="number"
                        value={ stockTransaction.quantity || stockTransaction.quantity === 0 ? stockTransaction.quantity.toString() : ''}
                        onChange={updateTransaction}
                    />
                </Stack.Item>

                <Stack.Item grow={5}>
                    <CurrencyInput
                        amount={stockTransaction.costBasis}
                        currency={'USD'}
                        name="costBasis"
                        onChange={updateTransaction}
                    />
                </Stack.Item>

                <Stack.Item grow>
                    <Label>Total</Label>
                    <TextField
                        borderless
                        underlined
                        readOnly
                        value={currencySymbols.usd + (stockTransaction.costBasis * stockTransaction.quantity).toFixed(2)}
                    />

                </Stack.Item>

            </Stack>

            <Stack horizontal tokens={stackTokens}>
                <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
                <DefaultButton onClick={onClose}>Cancel</DefaultButton>
            </Stack>

        </Stack>
    </form>
  )
}

function useStockTransactionForm(onClose: () => void, stockTransactionId?: StockTransactionId) {
    const [stockTransaction, setStockTransaction] = useState<StockTransaction>(stockTransactionId ? null : initializeTransaction())

    useEffect(() => {
        if (stockTransactionId) {
            getStockTransaction(stockTransactionId)
                .then(transaction => ({
                    ...transaction,
                    transactionDate: formatDay(transaction.transactionDate),
                    transactionTime: formatTimeInput(transaction.transactionDate)
                }))
                .then(setStockTransaction)
        }
    }, [stockTransactionId])

    const updateTransaction = useCallback(({target}) => {
        setStockTransaction({
            ...stockTransaction,
            [target.name]: target.value
        })
    }, [stockTransaction])

    const updateAccount = useCallback((accountId: StockAccountId) => {
       setStockTransaction({
           ...stockTransaction,
           accountId
       })
    }, [stockTransaction]);

    const submitForm = useCallback(async () => {
        await saveStockTransaction(convertToTransactionDto(stockTransaction))
        if (stockTransaction.id === -1) {
            setStockTransaction(initializeTransaction())
        } else {
            onClose();
        }
    }, [stockTransaction, onClose])
    return {stockTransaction, updateTransaction, updateAccount, submitForm};
}

function initializeTransaction(): StockTransaction {
    const today = formatDay(new Date())
    return {
        id: -1,
        accountId: null,
        transactionDate: today,
        transactionTime: '',
        costBasis: null,
        tickerSymbol: '',
        quantity: null
    };
}

function convertToTransactionDto(transaction: StockTransaction): StockTransactionDto {
    return {
        ...transaction,
        transactionDate: `${transaction.transactionDate}T${transaction.transactionTime || '00:00:00'}Z`
    }
}

interface StockTransaction extends Omit<StockTransactionDto, 'transactionDate'> {
    transactionDate: string;
    transactionTime: string;
}