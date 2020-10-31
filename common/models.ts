export type Money = number;
export type CategoryId = number;
export type PaymentId = number;
export type IncomeId = number;
export type Currency = 'ILS' | 'USD';

export interface Payment {
  id: PaymentId;
  paidDate: string;
  incurredBeginDate?: string;
  incurredEndDate?: string;
  amount: Money;
  currency: Currency;
  counterparty: string;
  note?: string;
  categoryId: CategoryId
}

export interface vPayment {
  id: PaymentId;
  paidDate: string;
  incurredBeginDate?: string;
  incurredEndDate?: string;
  ils: Money;
  usd: Money;
  counterparty: string;
  note?: string;
  categoryId: CategoryId;
  categoryName: string;
}

export interface Category {
  id: CategoryId;
  name: string;
}

export interface CarSummary {
  recordDate: string;
  ils: Money;
  usd: Money;
}

export interface CashAccount {
  id: number;
  name: string;
  currency: Currency;
}

export interface CashAssetRecord {
  accountId: number;
  amount: Money;
}

export interface Income {
  id: IncomeId;
  paidDate: string;
  source: string;
  amount: Money;
  currency: Currency;
  note: string;
}

export interface SpendingByCategory {
  id: CategoryId;
  categoryName: string;
  ils: Money;
  usd: Money;
}