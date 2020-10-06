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

export const getLoggedIn = (): Promise<boolean> => {
    return fetch(`${config.apiServiceUrl}/login`)
        .then(handleResponse)
        .then(res => res.json())
        .catch(() => false)
}

export const getPayments: () => Promise<Payment[]> = () =>
    fetch(`${config.apiServiceUrl}/payments`, {
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

export const savePayment = (payment: Payment): Promise<Payment[]> =>
    fetch(`${config.apiServiceUrl}/payments`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify(payment)
    }).then(handleResponse)

