export type Money = number;
export type CategoryId = number;
export type PaymentId = number;
export type IncomeId = number;
export type StockTransactionId = number;
export type StockAccountId = number;
export type FxTransactionId = number;
export type Currency = 'ILS' | 'USD';
export type TaxCategory = 'Taxable' | 'Exempt' | 'Deferred';

export interface Payment {
  id: PaymentId;
  paidDate: string;
  incurredBeginDate?: string;
  incurredEndDate?: string;
  amount: Money;
  incurredAmount: Money | null;
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
  hierachicalName: string;
  level: number;
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
  active: boolean;
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

export interface CashAssetAllocationRecord {
  // id: ;
  recordDate: string;
  amount: Money;
  currency: Currency;
  allocationCode: string;
  note?: string;
}

export interface CashAssetAllocation {
  allocationCode: string;
  ils: Money;
  usd: Money;
}

export interface CashAssetAllocationHistory {
  recordDate: string;
  ils: Money;
  usd: Money;
  allocationCode: string;
  note?: string;
}

export interface UnallocatedCash {
  ils: Money;
  usd: Money;
}

export interface CashAllocationsDto {
  unallocatedCashSnapshot: UnallocatedCash;
  cashAssetsAllocation: CashAssetAllocation[];
}

export interface UnreportedSpending {
  startDate: string;
  endDate: string;
  ils: Money;
  usd: Money;
}

export interface FuelLog {
  id: number;
  timestamp: Date;
  odometer: number;
  liters: number;
  kilometers: number;
  kilometersPerLiter: number;
  note?: string;
  isFull: boolean;
  paymentId: PaymentId;
  totalCost: Money;
  currency: Currency;
}

export interface FuelLogSummary {
  liters: number;
  kilometers: number;
  kilometersPerLiter: number;
  ils: Money;
}

export interface FuelLogDto {
  fuelLog: FuelLog[];
  summary: FuelLogSummary;
}

export interface NewFuelLogDto {
  id: number;
  timestamp: Date;
  odometer: number;
  liters: number;
  price: Money;
  note?: string;
  isFull: boolean;
}

export interface StockAccountDto {
  id: StockAccountId;
  name: string;
  taxCategory: TaxCategory
}

export interface StockTransactionDto {
  id: StockTransactionId;
  accountId: StockAccountId;
  accountName: string;
  tickerSymbol: string;
  //transfer a string in the form '2022-01-29 21:34:23.000'
  //so that timezone information is stripped out
  transactionDate: string;
  quantity: number;
  unitPrice: Money;
  costBasis: Money;
  commission: Money;
}

export interface StockHoldingSummaryDto {
  accountName?: string;
  taxCategory: TaxCategory;
  tickerSymbol: string;
  quantity: number;
  costBasis: number;
}

export interface vFxHistory {
  id: FxTransactionId;
  accountName: string;
  transactionDate: string; //datetime???
  ils: Money;
  usd: Money;
  usdCommission: Money;
  fxRate: number;
  effectiveRate: number;
  note: string;
}

export interface FxTransactionDto {
  id: FxTransactionId;
  //transfer a string in the form '2022-01-29 21:34:23.000'
  //so that timezone information is stripped out
  transactionDate: string;
  accountId: StockAccountId;
  ilsAmount: Money;
  usdAmount: Money;
  usdCommission: Money;
  note: string;
}
