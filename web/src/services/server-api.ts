import config from '../config/config';
import {
    CarSummary,
    CashAccount,
    CashAllocationsDto,
    CashAssetAllocationRecord,
    CashAssetRecord,
    Category,
    CategoryId,
    FuelLog,
    FuelLogSummary,
    NewFuelLogDto,
    Income,
    IncomeId,
    Payment,
    PaymentId,
    SpendingByCategory,
    UnreportedSpending,
    vPayment
} from '@pim/common';

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

export const getActiveCashAccounts: () => Promise<CashAccount[]> = () =>
    fetch(`${config.apiServiceUrl}/car/accounts`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json());

export const saveCashRecords = (recordDate: string | number | Date, accountBalances: CashAssetRecord[]): Promise<void> =>
    fetch(`${config.apiServiceUrl}/car/records`, {
        method: 'PUT',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify({
            recordDate,
            accountBalances
        })
    }).then(handleResponse)

export const getCashRecords = (recordDate: string | number | Date): Promise<CashAssetRecord[]> =>
    fetch(`${config.apiServiceUrl}/car/records?recordDate=${recordDate}`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json())

export const getCashAllocations = (): Promise<CashAllocationsDto> =>
    fetch(`${config.apiServiceUrl}/cash-allocations`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json())

export const getUnreportedSpending = (): Promise<UnreportedSpending[]> =>
    fetch(`${config.apiServiceUrl}/unreported-spending`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json())

export const getAllIncome: () => Promise<Income[]> = () =>
    fetch(`${config.apiServiceUrl}/income`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json());

export const getIncome: (incomeId: IncomeId) => Promise<Income> = (incomeId) =>
    fetch(`${config.apiServiceUrl}/income/${incomeId}`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json());

export const saveIncome: (income: Income) => Promise<Income> = (income) =>
    fetch(`${config.apiServiceUrl}/income`, {
        method: income.id === -1 ? 'POST' : 'PUT',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify(income)
    }).then(handleResponse)

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

export const saveAllocationRecord = (allocation: CashAssetAllocationRecord): Promise<void> =>
    fetch(`${config.apiServiceUrl}/cash-allocations`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify(allocation)
    }).then(handleResponse)

export function getSpendingByCategory(rootCategoryId: CategoryId): Promise<SpendingByCategory[]> {
    return fetch(`${config.apiServiceUrl}/analysis/spending-by-category?rootCategoryId=${rootCategoryId}`, {
        credentials: 'include'
    }).then(handleResponse)
        .then(res => res.json())
}

export const getFuelLog: () => Promise<{ fuelLog: FuelLog[]; summary: FuelLogSummary }> = () =>
    fetch(`${config.apiServiceUrl}/fuel-log`, {
        credentials: 'include',
    })
        .then(handleResponse)
        .then(res => res.json());


export const saveFuelLog = (fuelLog: NewFuelLogDto): Promise<void> =>
    fetch(`${config.apiServiceUrl}/fuel-log`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        credentials: 'include',
        body: JSON.stringify(fuelLog)
    }).then(handleResponse)

export const getNotes = (path: string) => {
    return fetch(`${config.apiServiceUrl}/notes/path?path=${path}`, {credentials: 'include'})
        .then(handleResponse)
        .then(res => res.text())
        .then(text => {
            try {
                return JSON.parse(text)
            } catch {
                return text;
            }
        })
}
// const debugSleep = (ms) => (...args) => new Promise(resolve => setTimeout(resolve, ms, args))