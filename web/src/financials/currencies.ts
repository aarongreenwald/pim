export const currencies = [
    'USD', 'ILS'
]

export const currencySymbols = {
    ils: '₪',
    usd: '$'
}

export const defaultCurrency = 'ILS'

export const currencyRadioOptions = currencies.map(currency => ({
    key: currency.toUpperCase(),
    text: currency.toUpperCase()
}));

export const currencyFields = currencies.map(c => c.toLowerCase());
