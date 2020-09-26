import config from '../config/config';
import {Category, Payment} from '../financials/models';
/* eslint-disable */
export default {
    newEntry: () => fetch(`${config.apiServiceUrl}/entries`, {method: 'POST'})
        .then(res => res.json()) ,
    getEntries: () => fetch(`${config.apiServiceUrl}/entries`)
        .then(res => res.json()),
    getSpending: () => fetch(`${config.apiServiceUrl}/spending`)
        .then(res => res.json())
}
/* eslint-enable */

export const getAllCategories = (): Promise<Category[]> =>
  fetch(`${config.apiServiceUrl}/categories`)
    .then(res => res.json())
    .then(data => data.map(convertCategoryDto))

export const savePayment = (payment: Payment): Promise<any> =>
    fetch(`${config.apiServiceUrl}/spending`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify(payment)
    }).then(res => {
      if (!res.ok) {
          throw 'Request failed'
      }
    })

function convertCategoryDto(category: any): Category {
    return {
        ...category,
        id: category.category_id
    }
}