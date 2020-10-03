import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import styled from '@emotion/styled';
import {Category, Payment} from '@pim/common';
import {Button} from '@material-ui/core';
import {format} from 'date-fns';
import {getAllCategories, savePayment} from '../services/server-api';

export const AddPayment: React.FC = () => {
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
      <StyledInput>
        <label>Date</label>
        <input type="date"
               onChange={updateItem}
               value={item.paidDate}
               name="paidDate"/>
      </StyledInput>
      {/*<StyledInput>*/}
      {/*  <label>Incurred Start Date</label>*/}
      {/*  <input type="date" onChange={updateItem}/>*/}
      {/*</StyledInput>*/}
      {/*<StyledInput>*/}
      {/*  <label>Incurred End Date</label>*/}
      {/*  <input type="date" onChange={updateItem}/>*/}
      {/*</StyledInput>*/}
      <StyledInput>
        <input type="text"
               name="counterparty"
               placeholder="Counterparty"
               value={item.counterparty}
               onChange={updateItem}/>
      </StyledInput>
      <StyledInput>
        <label>Amount</label>
        <input type="number" name="amount" value={item.amount || ''} onChange={updateItem}/>
      </StyledInput>
      <StyledRadioInput>
        <input type="radio" id="usd" name="currency" value="USD"
               checked={item.currency === 'USD'}
               onChange={updateItem} />
        <label htmlFor="usd">USD</label>
        <input type="radio" id="ils" name="currency" value="ILS"
               checked={item.currency === 'ILS'}
               onChange={updateItem}/>
        <label htmlFor="ils">ILS</label>
      </StyledRadioInput>
      <StyledInput>
        <label>Category</label>
        <select name="categoryId" value={item.categoryId} onChange={updateItem}>
          <option disabled value={-1}> Please select </option>
          {
            categories && categories.map(category => <option key={category.id}
                                                             value={category.id}>{category.name}</option>)
          }
        </select>
      </StyledInput>
      <StyledInput>
        <label>Notes</label>
        <textarea name="note" value={item.note} onChange={updateItem}/>
      </StyledInput>
      <Button variant={'contained'} color={'primary'} onClick={submitForm}>Save</Button>
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

const StyledInput = styled.div`
  padding: 5px;  
  
  & label {
    display: block;
  }
`
const StyledRadioInput = styled(StyledInput)`  
  & label {
    display: inline-block;
  }
`