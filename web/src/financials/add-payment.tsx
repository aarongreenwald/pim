import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import styled from '@emotion/styled';
import {Category, Payment} from '@pim/common';
import {format} from 'date-fns';
import {getAllCategories, savePayment} from '../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField, ChoiceGroup} from '@fluentui/react'
import {PanelProps} from '../common/panel.types';

export const AddPayment: React.FC<PanelProps & {data?: Payment}> = ({onClose, data}) => {
  const categories = useCategories();
    const {payment, updatePayment, updateCurrency, submitForm} = usePaymentForm(onClose, data);
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

            <TextField
                label="Amount"
                value={payment.amount ? payment.amount.toString() : ''}
                type="number"
                name="amount"
                onChange={updatePayment}
            />

            <StyledChoiceGroup
                selectedKey={payment.currency}
                label="Currency"
                styles={horizontalChoiceGroup}
                onChange={updateCurrency}
                options={currencyOptions}/>

            <StyledInput>
                <select
                    name="categoryId"
                    value={payment.categoryId}
                    onChange={updatePayment}>
                  <option disabled value={-1}> Select category </option>
                  {
                    categories && categories.map(category =>
                        <option
                            key={category.id}
                            value={category.id}>
                            {category.name}
                        </option>)
                  }
                </select>
            </StyledInput>

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

function useCategories() {
  const [categories, setCategories] = useState<Category[]>()
  useEffect(() => {
    getAllCategories().then(setCategories)
  }, [])
  return categories;
}

function usePaymentForm(onClose: () => void, data?: Payment, ) {
    const [payment, setPayment] = useState<Payment>(data || initializePayment())
    const updateCurrency = useCallback((_, {key}) => {
        setPayment({
            ...payment,
            currency: key
        })
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
    }, [payment])
    return {payment, updatePayment, updateCurrency, submitForm};
}

function initializePayment(): Payment {
  return {
    id: -1,
    paidDate: format(new Date(), 'yyyy-MM-dd'),
    categoryId: -1,
    amount: 0,
    counterparty: '',
    currency: 'ILS',
    note: ''
  };
}

const currencyOptions = [
    {key: 'USD', text: 'USD'},
    {key: 'ILS', text: 'ILS'},
];

const StyledInput = styled.div`
  width: 100%;
  
  select { 
    width: 100%;
  } 
`

const horizontalChoiceGroup = { flexContainer: { display: 'flex' } };

const StyledChoiceGroup = styled(ChoiceGroup)`  
  & label {
    margin-right: 8px;
  }
`

const stackTokens = {childrenGap: 8};