import {PanelProps} from '../../common/panel.types';
import {Checkbox, DefaultButton, PrimaryButton, Stack, TextField} from '@fluentui/react';
import {stackTokens} from '../styles';
import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {BasicISODate, CashAccount} from '@pim/common';
import {getActiveCashAccounts, getCashRecords, saveCashRecords} from '../../services/server-api';
import {collapseISODate, expandISODate, todayAsISODate} from '../../common/date.utils';
import {CurrencyInput} from '../currency-input';

export const AddCashRecord: React.FC<PanelProps<BasicISODate>> = ({onClose, id}) => {
  const {draft, accounts, updateDate, updateAccount, submitForm} = useCashRecordsForm(onClose, id);
  const [showAllAccounts, setShowAllAccounts] = useState<boolean>(false);

  //if editing an existing CAR, changing the date isn't allowed becase it'll just create a new
  //CAR set without deleting the old. So you must delete the old manually by clearing the inputs
  //and create a new one.
  const allowSelectingDate = !id;
  return (
    <form>
      <Stack tokens={stackTokens}>
        {
          allowSelectingDate &&
            <TextField
              label={'Date'}
              type="date"
              onChange={updateDate}
              value={expandISODate(draft.recordDate)}
              name="recordDate"/>
        }

        <Checkbox
          label="Show all accounts"
          checked={showAllAccounts}
          onChange={(_, checked) => setShowAllAccounts(checked)}
        />

        {
          accounts?.filter(a => showAllAccounts || a.active || typeof getAccountValue(draft, a) !== 'undefined').map(account =>
            <CurrencyInput
              key={account.id}
              label={`${account.name} ${!account.active ? '(Inactive)' : ''}`}
              amount={getAccountValue(draft, account)}
              currency={account.currency}
              name={`account_${account.id}`}
              onChange={updateAccount}
            />

          )
        }

        <Stack horizontal tokens={stackTokens}>
          <PrimaryButton onClick={submitForm}>Save</PrimaryButton>
          <DefaultButton onClick={onClose}>Cancel</DefaultButton>
        </Stack>

      </Stack>
    </form>
  )
}

interface Draft {
  recordDate: BasicISODate;
  accounts: Record<number, number>
}
function getAccountValue(draft: Draft, account: CashAccount) {
  //zeroes should be shown, other falsy values skipped
  return draft.accounts[account.id];
}


function useCashRecordsForm(onClose: () => void, recordDate?: BasicISODate) {
  const [accounts, setAccounts] = useState<CashAccount[]>()
  const [draft, setDraft] = useState<Draft>({
    recordDate: recordDate || todayAsISODate(),
    accounts: {}
  })

  useEffect(() => {
    getActiveCashAccounts().then(setAccounts)
  }, [])

  useEffect(() => {
    if (recordDate) {
      getCashRecords(recordDate).then(records =>
        setDraft({
          recordDate,
          accounts: records.reduce((acc, item) => {
            acc[item.accountId] = item.amount;
            return acc;
          }, {})
        })
      )
    }
  }, [recordDate])

  const updateDate = useCallback(({target}) => {
    setDraft({
      ...draft,
      recordDate: collapseISODate(target.value) as number
    })
  }, [draft])

  const updateAccount = useCallback(({target}) => {
    setDraft({
      ...draft,
      accounts: {
        ...draft.accounts,
        [target.name.split('_')[1]]: target.value
      }
    })
  }, [draft])

  const submitForm = useCallback(async () => {
    const balances = Object.keys(draft.accounts)
      .filter(key => draft.accounts[key] !== '') //only save explicit zeroes as zero, unfilled fields should not be set
      .map(key => ({
        accountId: Number(key),
        amount: Number(draft.accounts[key])
      }))
    await saveCashRecords(draft.recordDate, balances)
    onClose();
  }, [draft, onClose])
    
  return {accounts, draft, updateAccount, updateDate, submitForm};
}
