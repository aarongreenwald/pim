import * as React from 'react';
import {StockAccountDto, StockAccountId} from '@pim/common';
import {useEffect, useMemo, useState} from 'react';
import {ComboBox} from '@fluentui/react';
import {getStockAccounts} from '../../services/server-api';

interface StockAccountDropdownProps {
  value: StockAccountId;
  onChange: (id: StockAccountId) => void;
}

export const StockAccountDropdown: React.FC<StockAccountDropdownProps> = ({value, onChange}) => {
  const [accounts, setAccounts] = useState<StockAccountDto[]>()
  useEffect(() => {
    getStockAccounts().then(setAccounts)
  }, [])
  const options = useMemo(() => accounts?.map(account => ({
    key: account.id,
    text: account.name,
    data: account
  })), [accounts]);

  return (
    <ComboBox
      autoComplete="on"
      placeholder="Select account"
      label={'Account'}
      selectedKey={value}
      onChange={(ev, option) => onChange(option.key as unknown as number)}
      onRenderOption={(option) => <>{option.data.name}</>}
      options={options}/>
  )
}
