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
import {collapseISODate, expandISODate, todayAsISODate} from '../../common/date.utils';

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
	showSplitFields,
	setShowSplitFields,
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
                  value={payment.paidDate ? expandISODate(payment.paidDate) : undefined}
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
                        value={payment.incurredBeginDate ? expandISODate(payment.incurredBeginDate) : undefined}
                        name="incurredBeginDate"/>

                    <TextField
                        label={'Incurred End'}
                        type="date"
                        onChange={updatePayment}
                        value={payment.incurredEndDate ? expandISODate(payment.incurredEndDate) : undefined}
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

            <Stack horizontal tokens={stackTokens}>
		<CurrencyInput
                    amount={payment.amount}
                    currency={payment.currency}
                    name="amount"
                    onChange={updatePayment}
		/>

                <Toggle
                    label={'Split'}
                    checked={showSplitFields}
                    onChange={(_, val) => setShowSplitFields(val)}/>
            </Stack>

            {
                showSplitFields &&
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
                onChange={updateCategory} />

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
    const [showSplitFields, setShowSplitFields] = useState<boolean>(false);
    const formStartRef = useRef<ITextField>();

    useEffect(() => {
        if (paymentId) {
            getPayment(paymentId)
                .then(payment => {
                    if (payment.incurredBeginDate || payment.incurredEndDate) {
                        setShowIncurredDates(true);
                    }
                    if (payment.incurredAmount !== null) {
                        setShowSplitFields(true);
                    }
                    return payment;
                })
                .then(setPayment)
            //TODO cleanup - if the panel is closed before the request finishes an error is thrown
        }
    }, [paymentId])

    useEffect(() => {
	if (showSplitFields && payment.incurredAmount == null && payment.amount !== null) {
            setPayment(payment => ({
                ...payment,
                incurredAmount: payment.amount / 2
            }))
	} else if (!showSplitFields) {
            setPayment(payment => ({
                ...payment,
                incurredAmount: null
            }))
	}
    }, [showSplitFields]) //eslint-disable-line react-hooks/exhaustive-deps

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
          [target.name]: target.type == 'date' ? collapseISODate(target.value)  : target.value
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
            setShowSplitFields(false)
            setPayment(payment => initializePayment(payment.paidDate))
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
	showSplitFields,
	setShowSplitFields,
        formStartRef
    };
}

const preparePayment = (payment: Payment, useIncurredDates: boolean): Payment => ({
  ...payment,
  paidDate: payment.paidDate,
  incurredBeginDate: useIncurredDates ? payment.incurredBeginDate : null,
  incurredEndDate: useIncurredDates ? payment.incurredEndDate : null,
})

function initializePayment(paidDate = todayAsISODate() as number): Payment { //TODO
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
