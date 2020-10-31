export const currencies = [
    'USD', 'ILS'
]

export const defaultCurrency = 'ILS'

export const currencyRadioOptions = currencies.map(currency => ({
    key: currency.toUpperCase(),
    text: currency.toUpperCase()
}));
