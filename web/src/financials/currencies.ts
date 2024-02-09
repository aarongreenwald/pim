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

export const currencyFields = new Map();
currencies.forEach(c => currencyFields.set(c.toLocaleLowerCase(), c.toLowerCase()));

const additionalUsdFields = ['marketValue', 'costBasis', 'marketPrice', 'avgCostBasis', 'usdCommission', 'usdBalance']
additionalUsdFields.forEach(field => currencyFields.set(field, 'usd'));

const additionalIlsFields = ['ilsBalance']
additionalIlsFields.forEach(field => currencyFields.set(field, 'ils'));
