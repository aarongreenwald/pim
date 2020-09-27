import config from '../config/config';
import {Category, Payment} from '@pim/common';

const handleResponse = (res) => {
    if (res.status === 401) {
        throw 'Auth failure'
    } else if (!res.ok) {
        throw 'Fetch failed'
    } else {
        return res
    }

}
export const logout = () : Promise<boolean> => {
    return fetch(`${config.apiServiceUrl}/logout`)
        .then(handleResponse)
        .then(() => true)
}
export const login = (password: string) : Promise<boolean> => {
    return fetch(`${config.apiServiceUrl}/login`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        credentials: 'include',
        body: JSON.stringify({password})
    })
        .then(handleResponse)
        .then(() => true)
}


export const getSpending: () => Promise<any> = () =>
    fetch(`${config.apiServiceUrl}/spending`, {
            credentials: 'include',
        })
        .then(handleResponse)
        .then(res => res.json());


export const getAllCategories = (): Promise<Category[]> =>
  fetch(`${config.apiServiceUrl}/categories`, {
      credentials: 'include',
  })
    .then(handleResponse)
    .then(res => res.json())
    .then(data => data.map(convertCategoryDto))

export const savePayment = (payment: Payment): Promise<any> =>
    fetch(`${config.apiServiceUrl}/spending`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify(payment)
    }).then(handleResponse)

function convertCategoryDto(category: any): Category {
    return {
        ...category,
        id: category.category_id
    }
}