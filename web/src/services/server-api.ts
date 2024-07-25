import config from '../config/config';
import {
  CarSummary,
  CashAccount,
  CashAssetAllocation,
  CashAssetAllocationRecord,
  CashAssetAllocationHistory,
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
  vPayment,
  FileSystemItemType,
  File,
  Directory,
  GitStatus,
  NotesSearchResults,
  StockTransactionDto,
  StockTransactionId,
  StockAccountDto,
  vFxHistory,
  FxTransactionDto,
  FxTransactionId,
  StockHoldingSummaryDto,
  StockAccountCashBalance,
  StockAccountId,
  StockAccountCashFlow,
  BasicISODate,
  TransferCashToStockAccountDto,
  StockAccountCashTransactionId,
  StockAccountCashTransaction
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

export const getLoggedIn = (): Promise<boolean> => get<boolean>('login').catch(() => false)

export const getPayment: (paymentId: PaymentId) => Promise<Payment> = (paymentId) => get(`payments/${paymentId}`);

export const getPayments: () => Promise<vPayment[]> = () => get('payments');

export const getCarSummary: () => Promise<CarSummary[]> = () => get('car/summary');

export const getActiveCashAccounts: () => Promise<CashAccount[]> = () => get('car/accounts');

export const saveCashRecords = (recordDate: string | number | Date, accountBalances: CashAssetRecord[]): Promise<void> =>
  put('car/records', {
    recordDate,
    accountBalances
  })

export const getCashRecords = (recordDate: string | number | Date): Promise<CashAssetRecord[]> => get(`car/records?recordDate=${recordDate}`)

export const getCashAllocations = (): Promise<CashAssetAllocation[]> =>
  get('cash-allocations')

export const getCashAssetAllocationHistory = (allocationCode: string): Promise<CashAssetAllocationHistory[]> =>
  get(`cash-allocations/history/${allocationCode}`)

export const getUnreportedSpending = (): Promise<UnreportedSpending[]> =>
  get('unreported-spending')

export const getAllIncome: () => Promise<Income[]> = () =>
  get('income');

export const getIncome: (incomeId: IncomeId) => Promise<Income> = (incomeId) =>
  get(`income/${incomeId}`);

export const saveIncome: (income: Income) => Promise<Income> = (income) =>
  income.id === -1 ? post('income', income) : put('income', income)

export const getFxHistory: () => Promise<vFxHistory[]> = () =>
  get('fx-history');

export const getFxTransaction: (transactionId: FxTransactionId) => Promise<FxTransactionDto> = (transactionId) =>
  get(`fx-transactions/${transactionId}`);

export const saveFxTransaction: (fxTransaction: FxTransactionDto) => Promise<FxTransactionDto> = (fxTransaction) =>
  fxTransaction.id === -1 ? post('fx-transactions', fxTransaction) : put('fx-transactions', fxTransaction)

export const getAllStocks: () => Promise<StockTransactionDto[]> = () =>
  get('stock-transactions');

export const getStockHoldingsSummary: () => Promise<StockHoldingSummaryDto[]> = () =>
  get('stock-holdings-summary');

export const getStockTransaction: (transactionId: StockTransactionId) => Promise<StockTransactionDto> = (transactionId) =>
  get(`stock-transactions/${transactionId}`);

export const saveStockTransaction: (stockTransaction: StockTransactionDto) => Promise<StockTransactionDto> = (stockTransaction) =>
  stockTransaction.id === -1 ? post('stock-transactions', stockTransaction) : put('stock-transactions', stockTransaction)

export const getStockAccounts: () => Promise<StockAccountDto[]> = () =>
  get('/stock-accounts')

export const getStockAccountCashBalances: () => Promise<StockAccountCashBalance[]> = () =>
  get('stock-accounts/cash-balances');

export const getStockAccountCashFlow: (accountId: StockAccountId) => Promise<StockAccountCashFlow[]>  = (accountId) =>
  get(`stock-accounts/${accountId}/cash-flow`);

export const saveTransferCashToStockAccount: (dto: TransferCashToStockAccountDto) => Promise<void> = (dto) =>
  post('compound-transactions/cash-stock-accounts-fund-transfer', dto);

export const getStockAccountCashTransaction: (id: StockAccountCashTransactionId) => Promise<StockAccountCashTransaction> = (id) =>
  get(`stock-accounts/cash-transactions/${id}`)

export const saveStockAccountCashTransaction: (transaction: StockAccountCashTransaction) => Promise<void> = (transaction) =>
  put(`stock-accounts/cash-transactions/${transaction.id}`, transaction)

export const getAllCategories = (): Promise<Category[]> =>
  get('categories', )

export const savePayment = (payment: Payment): Promise<Payment[]> =>
  payment.id === -1 ? post('payments', payment) : put('payments', payment)

export const saveAllocationRecord = (allocation: CashAssetAllocationRecord): Promise<void> =>
  post('cash-allocations', allocation)

export function getSpendingByCategory(rootCategoryId: CategoryId): Promise<SpendingByCategory[]> {
  return get(`analysis/spending-by-category?rootCategoryId=${rootCategoryId}`)
}

export const getFuelLog: (pageSize?: number) => Promise<{ fuelLog: FuelLog[]; summary: FuelLogSummary }> = (pageSize) =>
  get(`fuel-log${pageSize ? `?pageSize=${pageSize}` : ''}`);


export const saveFuelLog = (fuelLog: NewFuelLogDto): Promise<void> =>
  post('fuel-log', fuelLog)

export const getNotes: (path: string) => Promise<File | Directory> = (path: string) => {
  return get(`notes/path?path=${encodeURIComponent(path)}`);
}

export const createItem = (path: string, name: string, type: FileSystemItemType): Promise<void> =>
  post(`notes/path?type=${type}&path=${encodeURIComponent(path)}&name=${name}`)

export const saveFileContent = (path: string, content: string): Promise<File | Directory> =>
  put(`notes/files?&path=${encodeURIComponent(path)}`, content, false);

export const commitPath = (path: string): Promise<File | Directory> =>
  put(`notes/commit?&path=${encodeURIComponent(path)}`);

export const gitPull: () => Promise<GitStatus> = () =>
  put('notes/pull');


export const gitPush: () => Promise<GitStatus> = () =>
  put('notes/push');

export const getGitStatus = (): Promise<GitStatus> => get('notes/status');

export const searchNotes = (query: string, excludeHidden = true): Promise<NotesSearchResults> =>
  get(`notes/search?query=${encodeURIComponent(query)}&excludeHidden=${excludeHidden}`);

export const getRecentFiles = (): Promise<string[]> => get('notes/recent');

export const renameDirectoryItem = (from: string, to: string): Promise<void> =>
  post(`notes/move?from=${encodeURIComponent(from)}&to=${encodeURIComponent(to)}`)

export const getDiaryPath = (date: BasicISODate, createIfNotExists = false): Promise<string> =>
  get<{diaryPath: string}>(`notes/diary-path?date=${date}&createIfNotExists=${createIfNotExists}`)
    .then(res => res.diaryPath)

// const debugSleep = (ms) => (...args) => new Promise(resolve => setTimeout(resolve, ms, args))

const get = <T>(path: string): Promise<T> =>
  fetch(`${config.apiServiceUrl}/${path}`, {credentials: 'include'})
    .then(handleResponse)
    .then(res => res.json())


const put = <T>(path: string, body?, json = true): Promise<T> => fetch(`${config.apiServiceUrl}/${path}`, {
  method: 'PUT',
  headers: json && body ? {
    'Content-Type': 'application/json'
  } : undefined,
  credentials: 'include',
  body: body && json ? JSON.stringify(body) : body
})
  .then(handleResponse)
  .then(async res => {
    const text = await res.text()
    try {
      return JSON.parse(text)
    } catch {
      return text
    }
  })

const post = <T>(path: string, body?): Promise<T> => fetch(`${config.apiServiceUrl}/${path}`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  credentials: 'include',
  body: body ? JSON.stringify(body) : null
})
  .then(handleResponse)
  .then(async res => {
    const text = await res.text()
    try {
      return JSON.parse(text)
    } catch {
      return text
    }
  })
