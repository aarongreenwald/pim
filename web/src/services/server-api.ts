import config from '../config/config';
import {CarSummary, Category, CategoryId, Income, Payment, PaymentId, SpendingByCategory, vPayment} from '@pim/common';

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
    return fetch(`${config.apiServiceUrl}/login`, {
            credentials: 'include',
        })
        .then(handleResponse)
        .then(res => res.json())
        .catch(() => false)
}

export const getPayment: (paymentId: PaymentId) => Promise<Payment> = (paymentId) =>
    fetch(`${config.apiServiceUrl}/payments/${paymentId}`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json());

export const getPayments: () => Promise<vPayment[]> = () =>
    fetch(`${config.apiServiceUrl}/payments`, {
            credentials: 'include',
        })
        .then(handleResponse)
        .then(res => res.json());

export const getCarSummary: () => Promise<CarSummary[]> = () =>
    fetch(`${config.apiServiceUrl}/car/summary`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json());


export const getIncome: () => Promise<Income[]> = () =>
    fetch(`${config.apiServiceUrl}/income`, {
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
        method: payment.id === -1 ? 'POST' : 'PUT',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify(payment)
    }).then(handleResponse)

export function getSpendingByCategory(rootCategoryId: CategoryId): Promise<SpendingByCategory[]> {
    return fetch(`${config.apiServiceUrl}/analysis/spending-by-category?rootCategoryId=${rootCategoryId}`, {
        credentials: 'include'
    }).then(handleResponse)
        .then(res => res.json())
}