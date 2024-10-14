import {useCallback, useState} from 'react';
import * as React from 'react';
import {StockAccountId, TransferCashToStockAccountDto} from '@pim/common';
import {saveTransferCashToStockAccount} from '../../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../../common/panel.types';
import {CurrencyInput} from '../currency-input';
import {StockAccountDropdown} from './stock-account-dropdown';
import {expandISODate, todayAsISODate, collapseISODate} from '../../common/date.utils';
import {CategoryDropdown} from '../payments/category-dropdown';
import {currencyRadioOptions, defaultCurrency} from '../currencies';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from '../styles';

export const CashStockAccountFundsTransferForm: React.FC<PanelProps> = ({onClose}) => {
  const {dto, updateDto, updateAccount, updateCategory, updateCurrency, submitForm} = useForm(onClose);

  return (
    <form>
      <Stack tokens={stackTokens}>
        <TextField
          label={'Cash Account Date'}
          type="date"
          onChange={updateDto}
          value={dto.cashAccountDate ? expandISODate(dto.cashAccountDate) : undefined}
          name="cashAccountDate"/>

        <TextField
          label={'Stock Account Date'}
          type="date"
          onChange={updateDto}
          value={dto.stockAccountDate ? expandISODate(dto.stockAccountDate) : undefined}
          name="stockAccountDate"/>
            
        <StockAccountDropdown value={dto.stockAccountId} onChange={updateAccount} label="Stock Account"/>

        <TextField
          label="Cash Allocation"
          name="cashAllocationCode"
          value={dto.cashAllocationCode}
          onChange={updateDto}
        />

        <CategoryDropdown
          showLabel
          label="Cash Category"
          value={dto.categoryId}
          onChange={updateCategory} />
          
        <StyledChoiceGroup
          selectedKey={dto.currency}
          label="Currency"
          styles={horizontalChoiceGroup}
          onChange={updateCurrency}
          options={currencyRadioOptions}/>

        <CurrencyInput
          amount={dto.amount}
          currency={dto.currency}
          name="amount"
          placeholder="Cash --> Stock Acct: amount > 0"
          onChange={updateDto}
        />

        <TextField
          label="Notes"
          name="note"
          value={dto.note}
          multiline
          onChange={updateDto}/>

        <Stack horizontal tokens={stackTokens}>
          <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
          <DefaultButton onClick={onClose}>Cancel</DefaultButton>
        </Stack>

      </Stack>
    </form>
  )
}

function useForm(onClose: () => void) {
  const [dto, setDto] = useState<TransferCashToStockAccountDto>(initializeDto())

  const updateDto = useCallback(({target}) => {
    setDto({
      ...dto,
      [target.name]: target.type == 'date' ? collapseISODate(target.value)  : target.value
    })
  }, [dto])

  const updateAccount = useCallback((stockAccountId: StockAccountId) => {
    setDto({
      ...dto,
      stockAccountId
    })
  }, [dto]);

  
  const updateCurrency = useCallback((_, {key}) => {
    setDto({
      ...dto,
      currency: key
    })
  }, [dto])

  const updateCategory = useCallback((categoryId) => {
    setDto({
      ...dto,
      categoryId
    });
  }, [dto])
  


  const submitForm = useCallback(async () => {
    await saveTransferCashToStockAccount(dto)
    onClose()
  }, [dto, onClose])
  return {dto, updateDto, updateCurrency, updateCategory, updateAccount, submitForm};
}

function initializeDto(): TransferCashToStockAccountDto {
  return {
    stockAccountId: null,
    currency: defaultCurrency,
    categoryId: null,
    cashAllocationCode: '',
    cashAccountDate: todayAsISODate() as number,
    stockAccountDate: todayAsISODate() as number, //TODO default to tomorrow
    amount: null,
    note: '',
  };
}
