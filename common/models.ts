export type Money = number;
export type CategoryId = number;
export type PaymentId = number;

export interface Payment {
  id: PaymentId;
  paidDate: string;
  incurredBeginDate?: string;
  incurredEndDate?: string;
  amount: Money;
  currency: 'ILS' | 'USD';
  counterParty: string;
  note?: string;
  categoryId: CategoryId
}

export interface Category {
  id: CategoryId;
  name: string;
}