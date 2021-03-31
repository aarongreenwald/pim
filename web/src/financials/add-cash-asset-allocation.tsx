import {useCallback, useState} from 'react';
import * as React from 'react';
import styled from '@emotion/styled';
import {CashAssetAllocationRecord, PaymentId} from '@pim/common';
import {format} from 'date-fns';
import {saveAllocationRecord} from '../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../common/panel.types';
import {currencyRadioOptions, defaultCurrency} from './currencies';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from './styles';
import {formatDay} from '../common/date.utils';

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
               value={formatDay(allocation.recordDate)}
               name="recordDate"/>

            <TextField
                label="Allocation Code"
                name="allocationCode"
                value={allocation.allocationCode}
                onChange={updateRecord}
            />

            <TextField
                label="Amount"
                value={allocation.amount ? allocation.amount.toString() : ''}
                type="number"
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
            [target.name]: target.value
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
    recordDate: format(new Date(), 'yyyy-MM-dd'),
    amount: 0,
    allocationCode: '',
    currency: defaultCurrency,
    note: ''
  };
}