import {TextField} from '@fluentui/react';
import {currencySymbols, defaultCurrency} from './currencies';
import * as React from 'react';
import {Currency, Money} from '@pim/common';

export const CurrencyInput: React.FC<CurrencyInputProps> = ({
  amount,
  currency = defaultCurrency,
  onChange,
  name,
  placeholder,
  label= 'Amount'}) => (
    <TextField
      label={label}
      value={ amount || amount === 0 ? amount.toString() : ''}
      prefix={currencySymbols[currency.toLowerCase()]}
      type="number"
      placeholder={placeholder}
      name={name}
      onChange={onChange}
    />
  )

interface CurrencyInputProps {
  amount: Money;
  currency?: Currency;
  label?: string;
  name?: string;
  placeholder?: string;
  onChange: (event: React.FormEvent, newValue?: string) => void;

}
