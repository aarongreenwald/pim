import {PanelProps} from '../common/panel.types';
import {DefaultButton, PrimaryButton, Stack, TextField} from '@fluentui/react';
import {stackTokens} from './styles';
import {format} from 'date-fns';
import * as React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {CashAccount} from '@pim/common';
import {getActiveCashAccounts, getCashRecords, saveCashRecords} from '../services/server-api';

export const AddCashRecord: React.FC<PanelProps<string>> = ({onClose, id}) => {
    const {draft, accounts, updateDate, updateAccount, submitForm} = useCashRecordsForm(onClose, id);

    return (
        <form>
            <Stack tokens={stackTokens}>
                <TextField
                    label={'Date'}
                    type="date"
                    onChange={updateDate}
                    value={format(new Date(draft.recordDate), 'yyyy-MM-dd')}
                    name="recordDate"/>

                {
                    accounts?.map(account =>
                        <TextField
                            key={account.id}
                            label={account.name}
                            value={draft.accounts[account.id] ? draft.accounts[account.id].toString() : ''}
                            type="number"
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


function useCashRecordsForm(onClose: () => void, recordDate?: string) {
    const [accounts, setAccounts] = useState<CashAccount[]>()
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

    const [draft, setDraft] = useState({
        recordDate: recordDate || new Date(),
        accounts: {}
    })

    const updateDate = useCallback(({target}) => {
        setDraft({
            ...draft,
            recordDate: target.value
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
        const balances = Object.keys(draft.accounts).map(key => ({
            accountId: Number(key),
            amount: Number(draft.accounts[key])
        }))
        await saveCashRecords(draft.recordDate, balances)
        onClose();
    }, [draft, onClose])
    
    return {accounts, draft, updateAccount, updateDate, submitForm};
}
