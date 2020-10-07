import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import styled from '@emotion/styled';
import {Category, Payment} from '@pim/common';
import {format} from 'date-fns';
import {getAllCategories, savePayment} from '../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField, ChoiceGroup} from '@fluentui/react'

interface AddPaymentProps {
    onClose: () => void;
}
export const AddPayment: React.FC<AddPaymentProps> = ({onClose}) => {
  const categories = useCategories();

  const [item, setItem] = useState<Payment>(initializePayment())
  const updateItem = useCallback(({target}) => {
    setItem({
      ...item,
      [target.name]: target.value
    })
  }, [item])
  const submitForm = useCallback(async () => {
    await savePayment(item)
    setItem(initializePayment())
  }, [item])

    return (
      <form>
        <Stack tokens={stackTokens}>
            <TextField
                    label={'Date'}
                    type="date"
                   onChange={updateItem}
                   value={item.paidDate}
                   name="paidDate"/>

            {/* todo begin/end incurred dates */}
            <TextField
                label="Counterparty"
                name="counterparty"
                value={item.counterparty}
                onChange={updateItem}
            />

            <TextField
                label="Amount"
                value={item.amount ? item.amount.toString() : ''}
                type="number"
                name="amount"
                onChange={updateItem}
            />

            <StyledChoiceGroup selectedKey={item.currency}
                               label="Currency"
                         styles={horizontalChoiceGroup}
                         onChange={updateItem}
                         options={currencyOptions}/>

            <StyledInput>
                <select name="categoryId" value={item.categoryId} onChange={updateItem}>
                  <option disabled value={-1}> Select category </option>
                  {
                    categories && categories.map(category => <option key={category.id}
                                                                     value={category.id}>{category.name}</option>)
                  }
                </select>
            </StyledInput>

            <TextField label="Notes"
                       multiline
                       name="note"
                       value={item.note}
                       onChange={updateItem}/>

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