import {useCallback, useEffect, useRef, useState} from 'react';
import * as React from 'react';
import {Payment, PaymentId} from '@pim/common';
import {getPayment, savePayment} from '../../services/server-api';
import {PrimaryButton, DefaultButton, Stack, TextField, Toggle, Spinner, ITextField} from '@fluentui/react'
import {PanelProps} from '../../common/panel.types';
import {CategoryDropdown} from './category-dropdown';
import {currencyRadioOptions, defaultCurrency} from '../currencies';
import {horizontalChoiceGroup, stackTokens, StyledChoiceGroup} from '../styles';
import {CurrencyInput} from '../currency-input';
import {formatDay} from '../../common/date.utils';

export const PaymentForm: React.FC<PanelProps<PaymentId>> = ({onClose, id}) => {
    const {
        payment,
        updatePayment,
        updateCurrency,
        updateCategory,
        submitForm,
        saveAndClose,
        showIncurredDates,
        setShowIncurredDates,
        formStartRef
    } = usePaymentForm(onClose, id);

    if (!payment) return <Spinner />;

    return (
      <form>
        <Stack tokens={stackTokens}>
            <Stack horizontal tokens={stackTokens}>
                <TextField
                   styles={{root: {flex: 1}}}
                   componentRef={formStartRef}
                   label={'Date'}
                   type="date"
                   onChange={updatePayment}
                   value={formatDay(payment.paidDate)}
                   name="paidDate"/>

                <Toggle
                    label={'Incurred'}
                    checked={showIncurredDates}
                    onChange={(_, val) => setShowIncurredDates(val)}/>
            </Stack>
            {
                showIncurredDates &&
                <>
                    <TextField
                        label={'Incurred Begin'}
                        type="date"
                        onChange={updatePayment}
                        value={payment.incurredBeginDate ? formatDay(payment.incurredBeginDate) : undefined}
                        name="incurredBeginDate"/>

                    <TextField
                        label={'Incurred End'}
                        type="date"
                        onChange={updatePayment}
                        value={payment.incurredEndDate ? formatDay(payment.incurredEndDate) : undefined}
                        name="incurredEndDate"/>
                </>
            }


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

            <CurrencyInput
                amount={payment.amount}
                currency={payment.currency}
                name="amount"
                onChange={updatePayment}
            />

            {
                showIncurredDates &&
                <CurrencyInput
		    label="Incurred Amount"
                    amount={payment.incurredAmount}
                    currency={payment.currency}
                    name="incurredAmount"
                    onChange={updatePayment}
                />
            }

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
                {
                    !id &&
                    <PrimaryButton onClick={saveAndClose}>Save & Close</PrimaryButton>
                }

                <DefaultButton onClick={onClose}>Cancel</DefaultButton>
            </Stack>

        </Stack>
    </form>
  )
}

function usePaymentForm(onClose: () => void, paymentId?: PaymentId, ) {
    const [showIncurredDates, setShowIncurredDates] = useState<boolean>(false);
    const [payment, setPayment] = useState<Payment>(paymentId ? null : initializePayment())
    const formStartRef = useRef<ITextField>();

    useEffect(() => {
        if (paymentId) {
            getPayment(paymentId)
                .then(payment => {
                    if (payment.incurredBeginDate || payment.incurredEndDate || payment.incurredAmount !== null) {
                        setShowIncurredDates(true);
                    }
                    return payment;
                })
                .then(setPayment)
            //TODO cleanup - if the panel is closed before the request finishes an error is thrown
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

    const saveAndClose  = useCallback(async () => {
        await savePayment(preparePayment(payment, showIncurredDates))
        onClose();
    }, [payment, onClose, showIncurredDates])

    /**
     * submit changes its behavior depending on whether this is a new payment (form stays open, another payment is added)
     * or editing an existing payment (close the form).
     */
    const submitForm = useCallback(async () => {
        if (payment.id !== -1) {
            await saveAndClose();
        } else {
            await savePayment(preparePayment(payment, showIncurredDates))
            setShowIncurredDates(false)
            setPayment(initializePayment(payment.paidDate))
            formStartRef.current.focus();
        }
    }, [payment, saveAndClose, showIncurredDates])
    return {
        payment,
        updatePayment,
        updateCategory,
        updateCurrency,
        submitForm,
        saveAndClose,
        showIncurredDates,
        setShowIncurredDates,
        formStartRef
    };
}

const preparePayment = (payment: Payment, useIncurredDates: boolean): Payment => ({
    ...payment,
    incurredBeginDate: useIncurredDates ? payment.incurredBeginDate : null,
    incurredEndDate: useIncurredDates ? payment.incurredEndDate : null,
})

function initializePayment(paidDate = formatDay(new Date())): Payment {
  return {
    id: -1,
    paidDate,
    incurredBeginDate: paidDate,
    incurredEndDate: paidDate,
    categoryId: -1,
    amount: null,
    incurredAmount: null,
    counterparty: '',
    currency: defaultCurrency,
    note: ''
  };
}
