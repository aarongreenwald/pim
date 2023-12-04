import {useCallback, useState} from 'react';
import * as React from 'react';
import {CashAssetAllocationRecord, PaymentId} from '@pim/common';
import {saveAllocationRecord} from '../../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../../common/panel.types';
import {currencyRadioOptions, defaultCurrency} from '../currencies';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from '../styles';
import {expandISODate, todayAsISODate, collapseISODate} from '../../common/date.utils';
import {CurrencyInput} from '../currency-input';

export const AddCashAssetAllocation: React.FC<PanelProps<PaymentId>> = ({onClose}) => {
    const {allocation, updateRecord, updateCurrency, submitForm} = useCaaForm();

    if (!updateRecord) return null; //TODO show a spinner

    return (
      <form>
        <Stack tokens={stackTokens}>
            <TextField
               label="Date"
               type="date"
               onChange={updateRecord}
               value={expandISODate(allocation.recordDate)}
               name="recordDate"/>

            <TextField
                label="Allocation Code"
                name="allocationCode"
                value={allocation.allocationCode}
                onChange={updateRecord}
            />

            <CurrencyInput
                amount={allocation.amount}
                currency={allocation.currency}
                name="amount"
                onChange={updateRecord}
            />

            <StyledChoiceGroup
                selectedKey={allocation.currency}
                label="Currency"
                styles={horizontalChoiceGroup}
                onChange={updateCurrency}
                options={currencyRadioOptions}/>

            <TextField
                label="Notes"
                name="note"
                value={allocation.note}
                multiline
                onChange={updateRecord}/>

            <Stack horizontal tokens={stackTokens}>
                <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
                <DefaultButton onClick={onClose}>Cancel</DefaultButton>
            </Stack>

        </Stack>
    </form>
  )
}

function useCaaForm() {
    const [allocation, setAllocation] = useState<CashAssetAllocationRecord>(initializeRecord())

    const updateCurrency = useCallback((_, {key}) => {
        setAllocation({
            ...allocation,
            currency: key
        })
    }, [allocation])

    const updateRecord = useCallback(({target}) => {
        setAllocation({
            ...allocation,
           [target.name]: target.type == 'date' ? collapseISODate(target.value)  : target.value
        })
    }, [allocation])
    const submitForm = useCallback(async () => {
        await saveAllocationRecord(allocation)
        setAllocation(initializeRecord())
    }, [allocation])
    return {allocation, updateRecord, updateCurrency, submitForm};
}

function initializeRecord(): CashAssetAllocationRecord {
  return {
    // id: -1,
    recordDate: todayAsISODate(),
    amount: null,
    allocationCode: '',
    currency: defaultCurrency,
    note: ''
  };
}
