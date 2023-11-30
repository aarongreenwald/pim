import {useCallback, useEffect, useState} from 'react';
import * as React from 'react';
import {Income, IncomeId} from '@pim/common';
import {getIncome, saveIncome} from '../../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField} from '@fluentui/react'
import {PanelProps} from '../../common/panel.types';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from '../styles';
import {currencyRadioOptions, defaultCurrency} from '../currencies';
import {expandISODate, todayAsISODate} from '../../common/date.utils';
import {CurrencyInput} from '../currency-input';

export const IncomeForm: React.FC<PanelProps<IncomeId>> = ({onClose, id}) => {
    const {income, updateIncome, updateCurrency, submitForm} = useIncomeForm(onClose, id);

    if (!income) return null; //TODO spinner

    return (
      <form>
        <Stack tokens={stackTokens}>
            <TextField
               label={'Date'}
               type="date"
               onChange={updateIncome}
               value={expandISODate(income.paidDate)}
               name="paidDate"/>

            <TextField
                label="Source"
                name="source"
                value={income.source}
                onChange={updateIncome}
            />

            <CurrencyInput
                amount={income.amount}
                currency={income.currency}
                name="amount"
                onChange={updateIncome}
            />

            <StyledChoiceGroup
                selectedKey={income.currency}
                label="Currency"
                styles={horizontalChoiceGroup}
                onChange={updateCurrency}
                options={currencyRadioOptions}/>

            <TextField
                label="Notes"
                name="note"
                value={income.note}
                multiline
                onChange={updateIncome}/>

            <Stack horizontal tokens={stackTokens}>
                <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
                <DefaultButton onClick={onClose}>Cancel</DefaultButton>
            </Stack>

        </Stack>
    </form>
  )
}

function useIncomeForm(onClose: () => void, incomeId?: IncomeId) {
    const [income, setIncome] = useState<Income>(incomeId ? null : initializeIncome())

    useEffect(() => {
        if (incomeId) {
            getIncome(incomeId).then(setIncome)
        }
    }, [incomeId])

    const updateCurrency = useCallback((_, {key}) => {
        setIncome({
            ...income,
            currency: key
        })
    }, [income])

    const updateIncome = useCallback(({target}) => {
        setIncome({
            ...income,
            [target.name]: target.value
        })
    }, [income])
    const submitForm = useCallback(async () => {
        await saveIncome(income)
        if (income.id === -1) {
            setIncome(initializeIncome())
        } else {
            onClose();
        }
    }, [income, onClose])
    return {income, updateIncome, updateCurrency, submitForm};
}

function initializeIncome(): Income {
  return {
    id: -1,
    paidDate: todayAsISODate() as number,
    amount: null,
    source: '',
    currency: defaultCurrency,
    note: ''
  };
}
