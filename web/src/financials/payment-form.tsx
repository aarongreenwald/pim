import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import {Payment, PaymentId} from '@pim/common';
import {format} from 'date-fns';
import {getPayment, savePayment} from '../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../common/panel.types';
import {CategoryDropdown} from './category-dropdown';
import {currencyRadioOptions, currencySymbols, defaultCurrency} from './currencies';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from './styles';

export const PaymentForm: React.FC<PanelProps<PaymentId>> = ({onClose, id}) => {
    const {payment, updatePayment, updateCurrency, updateCategory, submitForm} = usePaymentForm(onClose, id);

    if (!payment) return null; //TODO show a spinner

    return (
      <form>
        <Stack tokens={stackTokens}>
            <TextField
               label={'Date'}
               type="date"
               onChange={updatePayment}
               value={format(new Date(payment.paidDate), 'yyyy-MM-dd')}
               name="paidDate"/>

            {/* todo begin/end incurred dates */}
            <TextField
                label="Counterparty"
                name="counterparty"
                value={payment.counterparty}
                onChange={updatePayment}
            />

            <StyledChoiceGroup
                selectedKey={payment.currency}
                label="Currency"
                styles={horizontalChoiceGroup}
                onChange={updateCurrency}
                options={currencyRadioOptions}/>

            <TextField
                label="Amount"
                value={payment.amount ? payment.amount.toString() : ''}
                prefix={currencySymbols[payment.currency.toLowerCase()]}
                type="number"
                name="amount"
                onChange={updatePayment}
            />

            <CategoryDropdown
                showLabel
                value={payment.categoryId}
                onChange={updateCategory}>
            </CategoryDropdown>

            <TextField
                label="Notes"
                name="note"
                value={payment.note}
                multiline
                onChange={updatePayment}/>

            <Stack horizontal tokens={stackTokens}>
                <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
                <DefaultButton onClick={onClose}>Cancel</DefaultButton>
            </Stack>

        </Stack>
    </form>
  )
}

function usePaymentForm(onClose: () => void, paymentId?: PaymentId, ) {
    const [payment, setPayment] = useState<Payment>(paymentId ? null : initializePayment())

    useEffect(() => {
        if (paymentId) {
            getPayment(paymentId).then(setPayment)
        }
    }, [paymentId])

    const updateCurrency = useCallback((_, {key}) => {
        setPayment({
            ...payment,
            currency: key
        })
    }, [payment])

    const updateCategory = useCallback((categoryId) => {
        setPayment({
            ...payment,
            categoryId
        });
    }, [payment])

    const updatePayment = useCallback(({target}) => {
        setPayment({
            ...payment,
            [target.name]: target.value
        })
    }, [payment])
    const submitForm = useCallback(async () => {
        await savePayment(payment)
        if (payment.id === -1) {
            setPayment(initializePayment())
        } else {
            onClose();
        }
    }, [payment, onClose])
    return {payment, updatePayment, updateCategory, updateCurrency, submitForm};
}

function initializePayment(): Payment {
  return {
    id: -1,
    paidDate: format(new Date(), 'yyyy-MM-dd'),
    categoryId: -1,
    amount: 0,
    counterparty: '',
    currency: defaultCurrency,
    note: ''
  };
}
